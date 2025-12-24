# pbp-kit
Basic template for creating a project using PBP.

# Files to Edit
- `.gitignore`
- `main.py`
- `Makefile`
- `package.json`
- `README.md`

## `.gitignore`
extend with project-specific files to be ignored
## `main.py`
import project-specific code and Leaf parts
## `Makefile`
modify `NAME` to match that of the project
## `package.json`
insert project-specific JS dependencies (if any)
## `README.md`
modify to suit project

# Files to Preserve (Don't Touch)
- `pbp/`
- `PBP.xml`

## `pbp/`
The Parts Based Programming Tools, such as `t2t`, `kernel`, `das` and `tas`
`t2t` means "text to text". A tool for transmogrifying input text into output text. Like REGEX, but more powerful.
## `PBP.xml`
Palette of Part symbols for draw.io editor.

# Files to Add
- `.git` via `git init`
- project specific files

# Other Tools Needed
## Draw.io
Editor for creating Container Parts for the project. Download it from https://drawio.com, or use the online version.

# Process
Create a fresh copy of this template directory.

At a minimum, 
- edit `main.py`
  - import an Leaf parts needed for the project
  - initialize each Leaf part after creating the template registry `zd.initialize_from_files (...)` and before starting the system `zd.start (...)`
- edit `Makefile` 
  - the `NAME` variable to match the project name
  - to insert the kick-off argument to the project (if needed)
  
# UTF-8
Some of the Part names contain Unicode. 

You may need to enable Unicode before running `make` using this version of PBP.

In Linux/Mac:
`export PYTHONUTF8=1`

In Windows:
`set PYTHONUTF8=1`

![API for Building and Registering Part Templates](./pbp/api.md)
