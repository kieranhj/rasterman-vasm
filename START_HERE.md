# rasterman-vasm: start here

RasterMan (Steve Harrison's raster-effects module for the Acorn Archimedes) ported to the vasm
assembler. A port of a library/module.

| | |
|---|---|
| GitHub | https://github.com/kieranhj/rasterman-vasm (public) |
| Upstream | RasterMan by Steve Harrison (original source in `ref\`) |
| Local path | D:\archie\mine\rasterman-vasm (moved from OneDrive\Archie\Repos\rasterman-vasm on 2026-11-22) |
| Branch | main (only branch) |
| Last commit | 2023-01-20: Nits. (2 commits in total) |

## What's in here
- `rasterman.asm`: the module; `fiq_code.asm`: the FIQ handler, assembled separately and included.
- `config.h.asm`: build options; `lib\swis.h.asm`: SWI numbers; `ref\`: the original v0.30 BASIC
  source (`RasterV030,ffb`, `rm_v030_src.txt`) and its readme.
- `make.bat`: the build; `bin\`: bundled vasm and vlink.

## Build and run
- `make`: runs `make.bat` (works, 2026-11-22): deletes `build\*`, assembles `build\fiq_code.bin` and
  `build\rasterman.bin`, copies it to `..\arculator\hostfs\Builds\RMv030,ffa` (D:\archie\mine\arculator\hostfs\Builds).
- Tools: bundled `bin\vasmarm_std_win32.exe`.
- Load it in Arculator as a relocatable module (`RMv030`, filetype &FFA).

## Local changes
- Uncommitted now: `.vscode/` (untracked).
- No stashes; no snapshot branches.

## Status and next steps
- Initial port 2023-01-17, tidy 2023-01-20. No TODO or plan notes found.

## See also
- README.md (one line), D:\archie\mine\qtm-vasm (same approach for QTM), D:\archie\mine\globe and
  D:\archie\mine\proto-arc (RasterMan experiments).
