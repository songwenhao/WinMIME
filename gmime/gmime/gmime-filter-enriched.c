/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  GMime
 *  Copyright (C) 2000-2020 Jeffrey Stedfast
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1
 *  of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gmime-common.h"
#include "gmime-filter-enriched.h"

/* text/enriched is rfc1896 */


/**
 * SECTION: gmime-filter-enriched
 * @title: GMimeFilterEnriched
 * @short_description: Convert text/enriched or text/rtf to HTML
 * @see_also: #GMimeFilter
 *
 * A #GMimeFilter used for converting text/enriched or text/rtf to HTML.
 **/


typedef char * (*EnrichedParamParser) (const char *inptr, size_t inlen);

static char *param_parse_paraindent (const char *inptr, size_t inlen);
static char *param_parse_colour (const char *inptr, size_t inlen);
static char *param_parse_font (const char *inptr, size_t inlen);
static char *param_parse_lang (const char *inptr, size_t inlen);

static struct {
	char *enriched;
	char *html;
	gboolean needs_param;
	EnrichedParamParser parse_param; /* parses *and* validates the input */
} enriched_tags[] = {
	{ "bold",        "<b>",                 FALSE, NULL               },
	{ "/bold",       "</b>",                FALSE, NULL               },
	{ "italic",      "<i>",                 FALSE, NULL               },
	{ "/italic",     "</i>",                FALSE, NULL               },
	{ "fixed",       "<tt>",                FALSE, NULL               },
	{ "/fixed",      "</tt>",               FALSE, NULL               },
	{ "smaller",     "<font size=-1>",      FALSE, NULL               },
	{ "/smaller",    "</font>",             FALSE, NULL               },
	{ "bigger",      "<font size=+1>",      FALSE, NULL               },
	{ "/bigger",     "</font>",             FALSE, NULL               },
	{ "underline",   "<u>",                 FALSE, NULL               },
	{ "/underline",  "</u>",                FALSE, NULL               },
	{ "center",      "<p align=center>",    FALSE, NULL               },
	{ "/center",     "</p>",                FALSE, NULL               },
	{ "flushleft",   "<p align=left>",      FALSE, NULL               },
	{ "/flushleft",  "</p>",                FALSE, NULL               },
	{ "flushright",  "<p align=right>",     FALSE, NULL               },
	{ "/flushright", "</p>",                FALSE, NULL               },
	{ "excerpt",     "<blockquote>",        FALSE, NULL               },
	{ "/excerpt",    "</blockquote>",       FALSE, NULL               },
	{ "paragraph",   "<p>",                 FALSE, NULL               },
	{ "signature",   "<address>",           FALSE, NULL               },
	{ "/signature",  "</address>",          FALSE, NULL               },
	{ "comment",     "<!-- ",               FALSE, NULL               },
	{ "/comment",    " -->",                FALSE, NULL               },
	{ "np",          "<hr>",                FALSE, NULL               },
	{ "fontfamily",  "<font face=\"%s\">",  TRUE,  param_parse_font   },
	{ "/fontfamily", "</font>",             FALSE, NULL               },
	{ "color",       "<font color=\"%s\">", TRUE,  param_parse_colour },
	{ "/color",      "</font>",             FALSE, NULL               },
	{ "lang",        "<span lang=\"%s\">",  TRUE,  param_parse_lang   },
	{ "/lang",       "</span>",             FALSE, NULL               },
	
	/* don't handle this tag yet... */
	{ "paraindent",  "<p style=\"%s\">",    TRUE,  param_parse_paraindent },
	{ "/paraindent", "</p>",                FALSE, NULL              },
	
	/* as soon as we support all the tags that can have a param
	 * tag argument, these should be unnecessary, but we'll keep
	 * them anyway just in case? */
	{ "param",       "<!-- param:",         FALSE, NULL               },
	{ "/param",      " -->",                FALSE, NULL               },
};

#define PARAM_TAG_MIN_LEN  (sizeof ("<param>") + sizeof ("</param>") - 2)

static void g_mime_filter_enriched_class_init (GMimeFilterEnrichedClass *klass);
static void g_mime_filter_enriched_init       (GMimeFilterEnriched *filter, GMimeFilterEnrichedClass *klass);
static void g_mime_filter_enriched_finalize   (GObject *object);

static GMimeFilter *filter_copy (GMimeFilter *filter);
static void filter_filter (GMimeFilter *filter, char *in, size_t len, size_t prespace,
			   char **out, size_t *outlen, size_t *outprespace);
static void filter_complete (GMimeFilter *filter, char *in, size_t len, size_t prespace,
			     char **out, size_t *outlen, size_t *outprespace);
static void filter_reset (GMimeFilter *filter);


static GMimeFilterClass *parent_class = NULL;


GType
g_mime_filter_enriched_get_type (void)
{
	static GType type = 0;
	
	if (!type) {
		static const GTypeInfo info = {
			sizeof (GMimeFilterEnrichedClass),
			NULL, /* base_class_init */
			NULL, /* base_class_finalize */
			(GClassInitFunc) g_mime_filter_enriched_class_init,
			NULL, /* class_finalize */
			NULL, /* class_data */
			sizeof (GMimeFilterEnriched),
			0,    /* n_preallocs */
			(GInstanceInitFunc) g_mime_filter_enriched_init,
		};
		
		type = g_type_register_static (GMIME_TYPE_FILTER, "GMimeFilterEnriched", &info, 0);
	}
	
	return type;
}

static void
g_mime_filter_enriched_class_init (GMimeFilterEnrichedClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GMimeFilterClass *filter_class = GMIME_FILTER_CLASS (klass);
	
	parent_class = g_type_class_ref (GMIME_TYPE_FILTER);
	
	object_class->finalize = g_mime_filter_enriched_finalize;
	
	filter_class->copy = filter_copy;
	filter_class->reset = filter_reset;
	filter_class->filter = filter_filter;
	filter_class->complete = filter_complete;
}

static void
g_mime_filter_enriched_init (GMimeFilterEnriched *filter, GMimeFilterEnrichedClass *klass)
{
	filter->flags = 0;
	filter->nofill = 0;
}

static void
g_mime_filter_enriched_finalize (GObject *object)
{
	G_OBJECT_CLASS (parent_class)->finalize (object);
}


static GMimeFilter *
filter_copy (GMimeFilter *filter)
{
	GMimeFilterEnriched *enriched = (GMimeFilterEnriched *) filter;
	
	return g_mime_filter_enriched_new (enriched->flags);
}

#if 0
static gboolean
enriched_tag_needs_param (const char *tag)
{
	int i;
	
	for (i = 0; i < G_N_ELEMENTS (enriched_tags); i++)
		if (!g_ascii_strcasecmp (tag, enriched_tags[i].enriched))
			return enriched_tags[i].needs_param;
	
	return FALSE;
}
#endif

typedef enum {
	PARAINDENT_NONE  = 0,
	PARAINDENT_LEFT  = (1 << 0),
	PARAINDENT_RIGHT = (1 << 1),
	PARAINDENT_IN    = (1 << 2),
	PARAINDENT_OUT   = (1 << 3)
} paraindent_t;

static const char *valid_indents[] = {
	"left", "right", "in", "out"
};

static char *
param_parse_paraindent (const char *in, size_t inlen)
{
	const char *inend = in + inlen;
	const char *inptr = in;
	paraindent_t indent = 0;
	GString *str;
	guint i;
	
	while (inptr < inend) {
		const char *value = inptr;
		size_t vlen;
		
		while (inptr < inend && *inptr != ',')
			inptr++;
		
		vlen = inptr - value;
		
		for (i = 0; i < G_N_ELEMENTS (valid_indents); i++) {
			size_t n = strlen (valid_indents[i]);
			
			if (vlen == n && !g_ascii_strncasecmp (value, valid_indents[i], n)) {
				indent |= 1 << i;
				break;
			}
		}
		
		inptr++;
	}
	
	str = g_string_new ("");
	
	/* if In and Out are both specified, they cancel each other out? */
	if (indent & PARAINDENT_IN && indent & PARAINDENT_OUT)
		indent = indent & ~(PARAINDENT_IN | PARAINDENT_OUT);
	
	if (indent & PARAINDENT_LEFT)
		g_string_append_printf (str, "%smargin-left:4em", str->len > 0 ? "; " : "");
	if (indent & PARAINDENT_RIGHT)
		g_string_append_printf (str, "%smargin-right:4em", str->len > 0 ? "; " : "");
	if (indent & PARAINDENT_IN)
		g_string_append_printf (str, "%smargin:4em", str->len > 0 ? "; " : "");
	if (indent & PARAINDENT_OUT)
		g_string_append_printf (str, "%smargin:-4em", str->len > 0 ? "; " : "");
	
	return g_string_free (str, FALSE);
}

static const char *valid_colours[] = {
	"red", "green", "blue", "yellow", "cyan", "magenta", "black", "white"
};

static char *
param_parse_colour (const char *in, size_t inlen)
{
	const char *inend = in + inlen;
	const char *inptr = in;
	const char *end;
	guint32 rgb = 0;
	guint v, i;
	
	for (i = 0; i < G_N_ELEMENTS (valid_colours); i++) {
		size_t n = strlen (valid_colours[i]);
		
		if (inlen == n && !g_ascii_strncasecmp (inptr, valid_colours[i], n))
			return g_strdup (valid_colours[i]);
	}
	
	/* check for numeric r/g/b in the format: ####,####,#### */
	for (i = 0; i < 3; i++) {
		v = strtoul (inptr, (char **) &end, 16);
		if (end != inptr + 4 || (i < 2 && *end != ',') || (i == 2 && end != inend))
			return g_strndup (in, inlen);
		
		v >>= 8;
		
		rgb = (rgb << 8) | (v & 0xff);
		
		inptr += 5;
	}
	
	return g_strdup_printf ("#%.6X", rgb);
}

static char *
param_parse_font (const char *fontfamily, size_t inlen)
{
	register const char *inptr = fontfamily;
	const char *inend = inptr + inlen;
	
	/* don't allow any of '"', '<', nor '>' */
	while (inptr < inend && *inptr != '"' && *inptr != '<' && *inptr != '>')
		inptr++;
	
	return g_strndup (fontfamily, (size_t) (inptr - fontfamily));
}

static char *
param_parse_lang (const char *lang, size_t inlen)
{
	register const char *inptr = lang;
	const char *inend = inptr + inlen;
	
	/* don't allow any of '"', '<', nor '>' */
	while (inptr < inend && *inptr != '"' && *inptr != '<' && *inptr != '>')
		inptr++;
	
	return g_strndup (lang, (size_t) (inptr - lang));
}

static char *
param_parse (const char *enriched, const char *inptr, size_t inlen)
{
	guint i;
	
	for (i = 0; i < G_N_ELEMENTS (enriched_tags); i++) {
		if (!g_ascii_strcasecmp (enriched, enriched_tags[i].enriched))
			return enriched_tags[i].parse_param (inptr, inlen);
	}
	
	g_assert_not_reached ();
	
	return NULL;
}

#define IS_RICHTEXT GMIME_FILTER_ENRICHED_IS_RICHTEXT

static void
enriched_to_html (GMimeFilter *filter, char *in, size_t inlen, size_t prespace,
		  char **out, size_t *outlen, size_t *outprespace, gboolean flush)
{
	GMimeFilterEnriched *enriched = (GMimeFilterEnriched *) filter;
	const char *tag, *inend, *outend;
	register const char *inptr;
	register char *outptr;
	
	g_mime_filter_set_size (filter, inlen * 2 + 6, FALSE);
	
	inptr = in;
	inend = in + inlen;
	outptr = filter->outbuf;
	outend = filter->outbuf + filter->outsize;
	
 retry:
	do {
		while (inptr < inend && outptr < outend && !strchr (" <>&\n", *inptr))
			*outptr++ = *inptr++;
		
		if (outptr == outend)
			goto backup;
		
		if ((inptr + 1) >= inend)
			break;
		
		switch (*inptr++) {
		case ' ':
			while (inptr < inend && (outptr + 7) < outend && *inptr == ' ') {
				memcpy (outptr, "&nbsp;", 6);
				outptr += 6;
				inptr++;
			}
			
			if (outptr < outend)
				*outptr++ = ' ';
			
			break;
		case '\n':
			if (!(enriched->flags & IS_RICHTEXT)) {
				/* text/enriched */
				if (enriched->nofill > 0) {
					if ((outptr + 4) < outend) {
						memcpy (outptr, "<br>", 4);
						outptr += 4;
					} else {
						inptr--;
						goto backup;
					}
				} else if (*inptr == '\n') {
					if ((outptr + 4) >= outend) {
						inptr--;
						goto backup;
					}
					
					while (inptr < inend && (outptr + 4) < outend && *inptr == '\n') {
						memcpy (outptr, "<br>", 4);
						outptr += 4;
						inptr++;
					}
				} else {
					*outptr++ = ' ';
				}
			} else {
				/* text/richtext */
				*outptr++ = ' ';
			}
			break;
		case '>':
			if ((outptr + 4) < outend) {
				memcpy (outptr, "&gt;", 4);
				outptr += 4;
			} else {
				inptr--;
				goto backup;
			}
			break;
		case '&':
			if ((outptr + 5) < outend) {
				memcpy (outptr, "&amp;", 5);
				outptr += 5;
			} else {
				inptr--;
				goto backup;
			}
			break;
		case '<':
			if (!(enriched->flags & IS_RICHTEXT)) {
				/* text/enriched */
				if (*inptr == '<') {
					if ((outptr + 4) < outend) {
						memcpy (outptr, "&lt;", 4);
						outptr += 4;
						inptr++;
						break;
					} else {
						inptr--;
						goto backup;
					}
				}
			} else {
				/* text/richtext */
				if ((inend - inptr) >= 3 && (outptr + 4) < outend) {
					if (strncmp (inptr, "lt>", 3) == 0) {
						memcpy (outptr, "&lt;", 4);
						outptr += 4;
						inptr += 3;
						break;
					} else if (strncmp (inptr, "nl>", 3) == 0) {
						memcpy (outptr, "<br>", 4);
						outptr += 4;
						inptr += 3;
						break;
					}
				} else {
					inptr--;
					goto backup;
				}
			}
			
			tag = inptr;
			while (inptr < inend && *inptr != '>')
				inptr++;
			
			if (inptr == inend) {
				inptr = tag - 1;
				goto need_input;
			}
			
			if (!g_ascii_strncasecmp (tag, "nofill>", 7)) {
				if ((outptr + 5) < outend) {
					enriched->nofill++;
				} else {
					inptr = tag - 1;
					goto backup;
				}
			} else if (!g_ascii_strncasecmp (tag, "/nofill>", 8)) {
				if ((outptr + 6) < outend) {
					enriched->nofill--;
				} else {
					inptr = tag - 1;
					goto backup;
				}
			} else {
				const char *fmt, *html_tag = NULL;
				char *enriched_tag;
				size_t len;
				guint i;
				
				len = inptr - tag;
				enriched_tag = g_alloca (len + 1);
				memcpy (enriched_tag, tag, len);
				enriched_tag[len] = '\0';
				
				for (i = 0; i < G_N_ELEMENTS (enriched_tags); i++) {
					if (!g_ascii_strcasecmp (enriched_tag, enriched_tags[i].enriched)) {
						html_tag = enriched_tags[i].html;
						break;
					}
				}
				
				if (html_tag != NULL) {
					if ((fmt = strstr (html_tag, "%s")) != NULL) {
						const char *start;
						char *param;
						
						while (inptr < inend && *inptr != '<')
							inptr++;
						
						if (inptr == inend || (size_t) (inend - inptr) <= PARAM_TAG_MIN_LEN) {
							inptr = tag - 1;
							goto need_input;
						}
						
						if (g_ascii_strncasecmp (inptr, "<param>", 7) != 0) {
							/* ignore the enriched command tag... */
							inptr -= 1;
							goto loop;
						}
						
						inptr += 7;
						start = inptr;
						
						while (inptr < inend && *inptr != '<')
							inptr++;
						
						if (inptr == inend || (inend - inptr) <= 8) {
							inptr = tag - 1;
							goto need_input;
						}
						
						if (g_ascii_strncasecmp (inptr, "</param>", 8) != 0) {
							/* ignore the enriched command tag... */
							inptr += 7;
							goto loop;
						}
						
						len = inptr - start;
						param = param_parse (enriched_tag, start, len);
						len = strlen (param);
						
						inptr += 7;
						
						len += strlen (html_tag);
						
						if ((outptr + len) < outend) {
							size_t n = (size_t) (fmt - html_tag);
							
							memcpy (outptr, html_tag, n);
							outptr += n;
							fmt += 2;
							
							outptr = g_stpcpy (outptr, param);
							outptr = g_stpcpy (outptr, fmt);
							
							g_free (param);
						} else {
							g_free (param);
							inptr = tag - 1;
							goto backup;
						}
					} else {
						len = strlen (html_tag);
						if ((outptr + len) < outend) {
							memcpy (outptr, html_tag, len);
							outptr += len;
						} else {
							inptr = tag - 1;
							goto backup;
						}
					}
				}
			}
			
		loop:
			inptr++;
			break;
		default:
			break;
		}
	} while (inptr < inend);
	
 need_input:
	
	/* the reason we ignore @flush here is because if there isn't
           enough input to parse a tag, then there's nothing we can
           do. */
	
	if (inptr < inend)
		g_mime_filter_backup (filter, inptr, (unsigned) (inend - inptr));
	
	*out = filter->outbuf;
	*outlen = outptr - filter->outbuf;
	*outprespace = filter->outpre;
	
	return;
	
 backup:
	
	if (flush) {
		size_t offset, grow;
		
		grow = (inend - inptr) * 2 + 20;
		offset = outptr - filter->outbuf;
		g_mime_filter_set_size (filter, filter->outsize + grow, TRUE);
		outend = filter->outbuf + filter->outsize;
		outptr = filter->outbuf + offset;
		
		goto retry;
	} else {
		g_mime_filter_backup (filter, inptr, (unsigned) (inend - inptr));
	}
	
	*out = filter->outbuf;
	*outlen = outptr - filter->outbuf;
	*outprespace = filter->outpre;
}

static void
filter_filter (GMimeFilter *filter, char *in, size_t len, size_t prespace,
	       char **out, size_t *outlen, size_t *outprespace)
{
	enriched_to_html (filter, in, len, prespace, out, outlen, outprespace, FALSE);
}

static void 
filter_complete (GMimeFilter *filter, char *in, size_t len, size_t prespace,
		 char **out, size_t *outlen, size_t *outprespace)
{
	enriched_to_html (filter, in, len, prespace, out, outlen, outprespace, TRUE);
}

static void
filter_reset (GMimeFilter *filter)
{
	GMimeFilterEnriched *enriched = (GMimeFilterEnriched *) filter;
	
	enriched->nofill = 0;
}


/**
 * g_mime_filter_enriched_new:
 * @flags: flags
 *
 * Creates a new GMimeFilterEnriched object.
 *
 * Returns: a new GMimeFilter object.
 **/
GMimeFilter *
g_mime_filter_enriched_new (guint32 flags)
{
	GMimeFilterEnriched *enriched;
	
	enriched = g_object_new (GMIME_TYPE_FILTER_ENRICHED, NULL);
	enriched->flags = flags;
	
	return (GMimeFilter *) enriched;
}
