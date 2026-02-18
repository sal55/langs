## Language and Compiler Development

(Site being rearranged.)

I develop a number of small-scale language-related projects on my PC.

This github site is used for associated resources such as source backups, essential binaries, and docs. Most projects are written in my M language, but the master sources and development are on my home PC.

### Languages

**M** Lower level systems language. See [Mlanguage](MLanguage)

**Q** Scripting language. See [QLanguage](QLanguage)

**ILs** Intermediate languages including assembly. See [InternalLangs](InternalLangs)

### Binaries

Most things are written in my M language, including its own compiler. So existing binaries for that are critical. So the [Binaries](Binaries) folder keeps two versions in hex format (due to AV implications of EXE files)

### Compilers

See [Compilers](Compilers) for a summary of current tools, which include interpreters and emulators

### Targets

Most products now target x64 running Windows. There have also been experimental versions for Z80 and ARM64. And there are ways of getting some of the tools to work at least partially on Linux, on at least x64 and ARM64.

### Characteristics of my HLLs

* Old-fashioned Algol/Pascal-style syntax
* Case-insensitive
* 1-based/N-based arrays
* Devoid of most modern new features - except for those I consider worth having and easy to understand, use and implement
* A focus on having what I consider to be fundamental features that many now lack, especially in the scripting language

### Characteristics of my Tools

* Single-file, self-contained executables
* Smallish size (0.1 to 0.6MB)
* Usually providing very fast compilation or interpretation
* Effortless and near-instant build process
* Sources for any tool can be rendered as one self-contained amalgamated source file

### mcc.c

This file that sometimes appears is a version of my C-subset compiler, transpiled to OS-agnostic C code, that someone is using. It's how updates are made available.
