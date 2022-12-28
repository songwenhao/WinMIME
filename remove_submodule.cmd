@echo off

set submodule=%1
::echo submodule=%submodule%

set convertedSubmodule=%submodule%
set convertedSubmodule=%convertedSubmodule:/=\%
::echo convertedSubmodule=%convertedSubmodule%

git submodule deinit -f %submodule%

git rm --cached %submodule%

rmdir /s /q %convertedSubmodule%

rmdir /s /q .git\modules\%convertedSubmodule%