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
#include <string.h>

#include <zlib.h>

#include "gmime-filter-gzip.h"

#ifdef ENABLE_WARNINGS
#define w(x) x
#else
#define w(x)
#endif /* ENABLE_WARNINGS */


/**
 * SECTION: gmime-filter-gzip
 * @title: GMimeFilterGZip
 * @short_description: GNU Zip compression/decompression
 * @see_also: #GMimeFilter
 *
 * A #GMimeFilter used for compressing or decompressing a stream using
 * GNU Zip.
 **/


/* rfc1952 */

enum {
	GZIP_FLAG_FTEXT     = (1 << 0),
	GZIP_FLAG_FHCRC     = (1 << 1),
	GZIP_FLAG_FEXTRA    = (1 << 2),
	GZIP_FLAG_FNAME     = (1 << 3),
	GZIP_FLAG_FCOMMENT  = (1 << 4),
	GZIP_FLAG_RESERVED0 = (1 << 5),
	GZIP_FLAG_RESERVED1 = (1 << 6),
	GZIP_FLAG_RESERVED2 = (1 << 7),
};

enum {
	GZIP_OS_FAT,
	GZIP_OS_AMIGA,
	GZIP_OS_VMS,
	GZIP_OS_UNIX,
	GZIP_OS_VM_CMS,
	GZIP_OS_ATARI_TOS,
	GZIP_OS_HPFS,
	GZIP_OS_MACINTOSH,
	GZIP_OS_ZSYSTEM,
	GZIP_OS_CPM,
	GZIP_OS_TOPS20,
	GZIP_OS_NTFS,
	GZIP_OS_QDOS,
	GZIP_OS_ACORN_RISCOS,
	GZIP_OS_UNKNOWN = 255
};

#define GZIP_FLAG_RESERVED (GZIP_FLAG_RESERVED0 | GZIP_FLAG_RESERVED1 | GZIP_FLAG_RESERVED2)

/* http://www.gzip.org/zlib/rfc-gzip.html */
typedef union {
	unsigned char buf[10];
	struct {
		guint8 id1;
		guint8 id2;
		guint8 cm;
		guint8 flg;
		guint32 mtime;
		guint8 xfl;
		guint8 os;
	} v;
} gzip_hdr_t;

typedef union {
	struct {
		guint16 xlen;
		guint16 xlen_nread;
		guint16 crc16;
		
		guint8 got_hdr:1;
		guint8 is_valid:1;
		guint8 got_xlen:1;
		guint8 got_fname:1;
		guint8 got_fcomment:1;
		guint8 got_crc16:1;
	} unzip;
	struct {
		guint32 wrote_hdr:1;
		guint32 flushed:1;
	} zip;
} gzip_state_t;

struct _GMimeFilterGZipPrivate {
	z_stream *stream;
	
	gzip_state_t state;
	gzip_hdr_t hdr;
	
	char *filename;
	char *comment;
	
	guint32 crc32;
	guint32 isize;
};

static void g_mime_filter_gzip_class_init (GMimeFilterGZipClass *klass);
static void g_mime_filter_gzip_init (GMimeFilterGZip *filter, GMimeFilterGZipClass *klass);
static void g_mime_filter_gzip_finalize (GObject *object);

static GMimeFilter *filter_copy (GMimeFilter *filter);
static void filter_filter (GMimeFilter *filter, char *in, size_t len, size_t prespace,
			   char **out, size_t *outlen, size_t *outprespace);
static void filter_complete (GMimeFilter *filter, char *in, size_t len, size_t prespace,
			     char **out, size_t *outlen, size_t *outprespace);
static void filter_reset (GMimeFilter *filter);


static GMimeFilterClass *parent_class = NULL;


GType
g_mime_filter_gzip_get_type (void)
{
	static GType type = 0;
	
	if (!type) {
		static const GTypeInfo info = {
			sizeof (GMimeFilterGZipClass),
			NULL, /* base_class_init */
			NULL, /* base_class_finalize */
			(GClassInitFunc) g_mime_filter_gzip_class_init,
			NULL, /* class_finalize */
			NULL, /* class_data */
			sizeof (GMimeFilterGZip),
			0,    /* n_preallocs */
			(GInstanceInitFunc) g_mime_filter_gzip_init,
		};
		
		type = g_type_register_static (GMIME_TYPE_FILTER, "GMimeFilterGZip", &info, 0);
	}
	
	return type;
}


static void
g_mime_filter_gzip_class_init (GMimeFilterGZipClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GMimeFilterClass *filter_class = GMIME_FILTER_CLASS (klass);
	
	parent_class = g_type_class_ref (GMIME_TYPE_FILTER);
	
	object_class->finalize = g_mime_filter_gzip_finalize;
	
	filter_class->copy = filter_copy;
	filter_class->filter = filter_filter;
	filter_class->complete = filter_complete;
	filter_class->reset = filter_reset;
}

static void
g_mime_filter_gzip_init (GMimeFilterGZip *filter, GMimeFilterGZipClass *klass)
{
	filter->priv = g_new0 (struct _GMimeFilterGZipPrivate, 1);
	filter->priv->stream = g_new0 (z_stream, 1);
	filter->priv->crc32 = crc32 (0, Z_NULL, 0);
}

static void
g_mime_filter_gzip_finalize (GObject *object)
{
	GMimeFilterGZip *gzip = (GMimeFilterGZip *) object;
	struct _GMimeFilterGZipPrivate *priv = gzip->priv;
	
	if (gzip->mode == GMIME_FILTER_GZIP_MODE_ZIP)
		deflateEnd (priv->stream);
	else
		inflateEnd (priv->stream);
	
	g_free (priv->filename);
	g_free (priv->comment);
	g_free (priv->stream);
	g_free (priv);
	
	G_OBJECT_CLASS (parent_class)->finalize (object);
}


static GMimeFilter *
filter_copy (GMimeFilter *filter)
{
	GMimeFilterGZip *gzip = (GMimeFilterGZip *) filter;
	
	return g_mime_filter_gzip_new (gzip->mode, gzip->level);
}

static inline size_t
next_alloc_size (size_t n)
{
	return (n + 1023) & ~1023;
}

static void
gzip_filter (GMimeFilter *filter, char *in, size_t len, size_t prespace,
	     char **out, size_t *outlen, size_t *outprespace, gboolean flush)
{
	GMimeFilterGZip *gzip = (GMimeFilterGZip *) filter;
	struct _GMimeFilterGZipPrivate *priv = gzip->priv;
	size_t atleast, olen;
	int retval;
	
	if (priv->state.zip.flushed) {
		*outprespace = prespace;
		*outlen = 0;
		*out = in;
		return;
	}
	
	if (!priv->state.zip.wrote_hdr) {
		size_t filenamelen = gzip->priv->filename ? strlen (gzip->priv->filename) + 1 : 0;
		size_t commentlen = gzip->priv->comment ? strlen (gzip->priv->comment) + 1 : 0;
		size_t hdrlen = 10 + filenamelen + commentlen;
		char *outptr;
		
		priv->hdr.v.id1 = 31;
		priv->hdr.v.id2 = 139;
		priv->hdr.v.cm = Z_DEFLATED;
		priv->hdr.v.mtime = 0;
		priv->hdr.v.flg = 0;
		if (gzip->priv->filename)
			priv->hdr.v.flg |= GZIP_FLAG_FNAME;
		if (gzip->priv->comment)
			priv->hdr.v.flg |= GZIP_FLAG_FCOMMENT;
		if (gzip->level == Z_BEST_COMPRESSION)
			priv->hdr.v.xfl = 2;
		else if (gzip->level == Z_BEST_SPEED)
			priv->hdr.v.xfl = 4;
		else
			priv->hdr.v.xfl = 0;
		priv->hdr.v.os = GZIP_OS_UNKNOWN;
		
		atleast = next_alloc_size ((len * 2) + hdrlen + 12);
		g_mime_filter_set_size (filter, atleast, FALSE);
		
		memcpy (filter->outbuf, priv->hdr.buf, 10);
		outptr = filter->outbuf + 10;
		
		if (gzip->priv->filename) {
			memcpy (outptr, gzip->priv->filename, filenamelen);
			outptr += filenamelen;
		}
		
		if (gzip->priv->comment) {
			memcpy (outptr, gzip->priv->comment, commentlen);
			outptr += commentlen;
		}
		
		priv->stream->next_out = (unsigned char *) outptr;
		priv->stream->avail_out = filter->outsize - hdrlen;
		
		priv->state.zip.wrote_hdr = TRUE;
	} else {
		atleast = next_alloc_size ((len * 2) + 12);
		g_mime_filter_set_size (filter, atleast, FALSE);
		
		priv->stream->next_out = (unsigned char *) filter->outbuf;
		priv->stream->avail_out = filter->outsize;
	}
	
	priv->stream->next_in = (unsigned char *) in;
	priv->stream->avail_in = len;
	
	do {
		retval = deflate (priv->stream, Z_NO_FLUSH);
		
		if (retval != Z_OK && !(retval == Z_BUF_ERROR && !priv->stream->avail_in)) {
			w(fprintf (stderr, "gzip: %d: %s\n", retval, priv->stream->msg));
			break;
		}
		
		if (priv->stream->avail_out > 0) {
			if (priv->stream->avail_in > 0) {
				g_mime_filter_backup (filter, (char *) priv->stream->next_in,
						      priv->stream->avail_in);
			}
			break;
		}
		
		if (priv->stream->avail_in == 0)
			break;
		
		olen = filter->outsize - priv->stream->avail_out;
		atleast = next_alloc_size (olen + priv->stream->avail_in * 2);
		g_mime_filter_set_size (filter, atleast, TRUE);
		priv->stream->next_out = (unsigned char *) filter->outbuf + olen;
		priv->stream->avail_out = filter->outsize - olen;
	} while (1);
	
	if (flush) {
		guint32 val;
		
		do {
			retval = deflate (priv->stream, Z_FULL_FLUSH);
			
			if (retval != Z_OK && !(retval == Z_BUF_ERROR && !priv->stream->avail_in)) {
				w(fprintf (stderr, "gzip: %d: %s\n", retval, priv->stream->msg));
				break;
			}
			
			if (priv->stream->avail_out > 0)
				break;
			
			olen = filter->outsize;
			g_mime_filter_set_size (filter, olen + 1024, TRUE);
			priv->stream->next_out = (unsigned char *) filter->outbuf + olen;
			priv->stream->avail_out = filter->outsize - olen;
		} while (1);
		
		if (priv->stream->avail_out < 8) {
			olen = filter->outsize;
			g_mime_filter_set_size (filter, olen + 8, TRUE);
			priv->stream->next_out = (unsigned char *) filter->outbuf + olen;
			priv->stream->avail_out = filter->outsize - olen;
		}
		
		val = GUINT32_TO_LE (priv->crc32);
		memcpy (priv->stream->next_out, &val, 4);
		priv->stream->avail_out -= 4;
		priv->stream->next_out += 4;
		
		val = GUINT32_TO_LE (priv->isize);
		memcpy (priv->stream->next_out, &val, 4);
		priv->stream->avail_out -= 4;
		priv->stream->next_out += 4;
		
		priv->state.zip.flushed = TRUE;
	}
	
	priv->crc32 = crc32 (priv->crc32, (unsigned char *) in, len - priv->stream->avail_in);
	priv->isize += len - priv->stream->avail_in;
	
	*out = filter->outbuf;
	*outlen = filter->outsize - priv->stream->avail_out;
	*outprespace = filter->outpre;
}

static void
gunzip_filter (GMimeFilter *filter, char *in, size_t len, size_t prespace,
	       char **out, size_t *outlen, size_t *outprespace, gboolean flush)
{
	GMimeFilterGZip *gzip = (GMimeFilterGZip *) filter;
	struct _GMimeFilterGZipPrivate *priv = gzip->priv;
	const char *inend = in + len;
	const char *inptr = in;
	guint16 need, val;
	size_t left = len;
	int retval;
	
	*outprespace = prespace;
	*outlen = 0;
	*out = in;
	
	if (!priv->state.unzip.got_hdr) {
		if (left < 10) {
			/* not enough data to decode the header */
			g_mime_filter_backup (filter, inptr, left);
			return;
		}
		
		memcpy (priv->hdr.buf, inptr, 10);
		priv->state.unzip.got_hdr = TRUE;
		inptr += 10;
		left -= 10;
		
		priv->state.unzip.is_valid = (priv->hdr.v.id1 == 31 &&
					      priv->hdr.v.id2 == 139 &&
					      priv->hdr.v.cm == Z_DEFLATED);
	}
	
	if (!priv->state.unzip.is_valid)
		return;
	
	if (priv->hdr.v.flg & GZIP_FLAG_FEXTRA) {
		if (!priv->state.unzip.got_xlen) {
			if (left < 2) {
				/* not enough data to get the xlen field */
				g_mime_filter_backup (filter, inptr, left);
				return;
			}
			
			memcpy (&val, inptr, 2);
			priv->state.unzip.xlen = GUINT16_FROM_LE (val);
			priv->state.unzip.got_xlen = TRUE;
			inptr += 2;
			left -= 2;
		}
		
		if (priv->state.unzip.xlen_nread < priv->state.unzip.xlen) {
			need = priv->state.unzip.xlen - priv->state.unzip.xlen_nread;
			
			if (need >= left) {
				priv->state.unzip.xlen_nread += left;
				return;
			}
			
			priv->state.unzip.xlen_nread += need;
			inptr += need;
			left -= need;
		}
	}
	
	if ((priv->hdr.v.flg & GZIP_FLAG_FNAME) && !priv->state.unzip.got_fname) {
		const char *start = inptr;
		
		while (inptr < inend && *inptr)
			inptr++;
		
		if (inptr == inend) {
			g_mime_filter_backup (filter, start, left);
			return;
		}
		
		g_free (priv->filename);
		priv->filename = g_strndup (start, inptr - start);
		priv->state.unzip.got_fname = TRUE;
		inptr++;
		
		left -= (inptr - start);
	}
	
	if ((priv->hdr.v.flg & GZIP_FLAG_FCOMMENT) && !priv->state.unzip.got_fcomment) {
		const char *start = inptr;
		
		while (inptr < inend && *inptr)
			inptr++;
		
		if (inptr == inend) {
			g_mime_filter_backup (filter, start, left);
			return;
		}

		g_free (priv->comment);
		priv->comment = g_strndup (start, inptr - start);
		priv->state.unzip.got_fcomment = TRUE;
		inptr++;
		
		left -= (inptr - start);
	}
	
	if ((priv->hdr.v.flg & GZIP_FLAG_FHCRC) && !priv->state.unzip.got_crc16) {
		if (left < 2) {
			g_mime_filter_backup (filter, inptr, left);
			return;
		}
		
		memcpy (&val, inptr, 2);
		priv->state.unzip.crc16 = GUINT16_FROM_LE (val);
		priv->state.unzip.got_crc16 = TRUE;
		inptr += 2;
		left -= 2;
	}
	
	if (left == 0 && !flush)
		return;
	
	g_mime_filter_set_size (filter, (left * 2) + 12, FALSE);
	
	priv->stream->next_in = (unsigned char *) inptr;
	priv->stream->avail_in = left;
	
	priv->stream->next_out = (unsigned char *) filter->outbuf;
	priv->stream->avail_out = filter->outsize;
	
	do {
		/* FIXME: handle error cases? */
		/* Note: Z_BUF_ERROR is not really an error unless there is input available */
		retval = inflate (priv->stream, flush ? Z_FULL_FLUSH : Z_NO_FLUSH);
		
		if (retval != Z_OK && !(retval == Z_BUF_ERROR && !priv->stream->avail_in)) {
			w(fprintf (stderr, "gunzip: %d: %s\n", retval, priv->stream->msg));
			break;
		}
		
		if (priv->stream->avail_in == 0)
			break;
		
		if (flush) {
			size_t olen = filter->outsize - priv->stream->avail_out;
			
			g_mime_filter_set_size (filter, olen + (priv->stream->avail_in * 2) + 12, TRUE);
			priv->stream->next_out = (unsigned char *) filter->outbuf + olen;
			priv->stream->avail_out = filter->outsize - olen;
		} else {
			g_mime_filter_backup (filter, (char *) priv->stream->next_in,
					      priv->stream->avail_in);
			break;
		}
	} while (1);
	
	/* FIXME: if we keep this, we could check that the gzip'd
	 * stream is sane, but how would we tell our consumer if it
	 * was/wasn't? */
	/*priv->crc32 = crc32 (priv->crc32, in, len - priv->stream->avail_in - 8);
	  priv->isize += len - priv->stream->avail_in - 8;*/
	
	*out = filter->outbuf;
	*outlen = filter->outsize - priv->stream->avail_out;
	*outprespace = filter->outpre;
}

static void
filter_filter (GMimeFilter *filter, char *in, size_t len, size_t prespace,
	       char **out, size_t *outlen, size_t *outprespace)
{
	GMimeFilterGZip *gzip = (GMimeFilterGZip *) filter;
	
	if (gzip->mode == GMIME_FILTER_GZIP_MODE_ZIP)
		gzip_filter (filter, in, len, prespace, out, outlen, outprespace, FALSE);
	else
		gunzip_filter (filter, in, len, prespace, out, outlen, outprespace, FALSE);
}

static void
filter_complete (GMimeFilter *filter, char *in, size_t len, size_t prespace,
		 char **out, size_t *outlen, size_t *outprespace)
{
	GMimeFilterGZip *gzip = (GMimeFilterGZip *) filter;
	
	if (gzip->mode == GMIME_FILTER_GZIP_MODE_ZIP)
		gzip_filter (filter, in, len, prespace, out, outlen, outprespace, TRUE);
	else
		gunzip_filter (filter, in, len, prespace, out, outlen, outprespace, TRUE);
}

static void
filter_reset (GMimeFilter *filter)
{
	GMimeFilterGZip *gzip = (GMimeFilterGZip *) filter;
	struct _GMimeFilterGZipPrivate *priv = gzip->priv;
	
	memset (&priv->state, 0, sizeof (priv->state));
	
	if (gzip->mode == GMIME_FILTER_GZIP_MODE_ZIP) {
		deflateReset (priv->stream);
	} else {
		inflateReset (priv->stream);
		g_free (priv->filename);
		g_free (priv->comment);
		priv->filename = NULL;
		priv->comment = NULL;
	}
	
	priv->crc32 = crc32 (0, Z_NULL, 0);
	priv->isize = 0;
}


/**
 * g_mime_filter_gzip_new:
 * @mode: zip or unzip
 * @level: compression level
 *
 * Creates a new gzip (or gunzip) filter.
 *
 * Returns: a new gzip (or gunzip) filter.
 **/
GMimeFilter *
g_mime_filter_gzip_new (GMimeFilterGZipMode mode, int level)
{
	GMimeFilterGZip *gzip;
	int retval;
	
	gzip = g_object_new (GMIME_TYPE_FILTER_GZIP, NULL);
	gzip->mode = mode;
	gzip->level = level;
	
	if (mode == GMIME_FILTER_GZIP_MODE_ZIP)
		retval = deflateInit2 (gzip->priv->stream, level, Z_DEFLATED, -MAX_WBITS, MAX_MEM_LEVEL, Z_DEFAULT_STRATEGY);
	else
		retval = inflateInit2 (gzip->priv->stream, -MAX_WBITS);
	
	if (retval != Z_OK) {
		g_object_unref (gzip);
		return NULL;
	}
	
	return (GMimeFilter *) gzip;
}


/**
 * g_mime_filter_gzip_get_filename:
 * @gzip: A #GMimeFilterGZip filter
 *
 * Gets the filename that was either previously set or retrieved when decoding a gzip stream.
 *
 * Returns: a string containing th ename of the file.
 *
 * Since: 3.2
 **/
const char *
g_mime_filter_gzip_get_filename (GMimeFilterGZip *gzip)
{
	g_return_val_if_fail (GMIME_IS_FILTER_GZIP (gzip), NULL);
	
	return gzip->priv->filename;
}


/**
 * g_mime_filter_gzip_set_filename:
 * @gzip: A #GMimeFilterGZip filter
 * @filename: The name of the file
 *
 * Sets the filename that should be used when generating the gzip header.
 *
 * Since: 3.2
 **/
void
g_mime_filter_gzip_set_filename (GMimeFilterGZip *gzip, const char *filename)
{
	char *buf;
	
	g_return_if_fail (GMIME_IS_FILTER_GZIP (gzip));
	
	buf = g_strdup (filename);
	g_free (gzip->priv->filename);
	gzip->priv->filename = buf;
}


/**
 * g_mime_filter_gzip_get_comment:
 * @gzip: A #GMimeFilterGZip filter
 *
 * Gets the comment that was either previously set or retrieved when decoding a gzip stream.
 *
 * Returns: a string containing the comment.
 *
 * Since: 3.2
 **/
const char *
g_mime_filter_gzip_get_comment (GMimeFilterGZip *gzip)
{
	g_return_val_if_fail (GMIME_IS_FILTER_GZIP (gzip), NULL);
	
	return gzip->priv->comment;
}


/**
 * g_mime_filter_gzip_set_comment:
 * @gzip: A #GMimeFilterGZip filter
 * @comment: The comment
 *
 * Sets the comment that should be used when generating the gzip header.
 *
 * Since: 3.2
 **/
void
g_mime_filter_gzip_set_comment (GMimeFilterGZip *gzip, const char *comment)
{
	char *buf;
	
	g_return_if_fail (GMIME_IS_FILTER_GZIP (gzip));
	
	buf = g_strdup (comment);
	g_free (gzip->priv->comment);
	gzip->priv->comment = buf;
}
