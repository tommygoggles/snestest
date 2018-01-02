@echo off 
"wla-dx-9.5-win32-bin-2013-SVN-rev8-WJ/wla-65816" -vo TGSNES.asm TGSNES.obj
"wla-dx-9.5-win32-bin-2013-SVN-rev8-WJ/wlalink" -vr TGSNES.link TGSNES.smc
"snes9x1.51.ep10r2/Snes9X1.51.ep10r2"
