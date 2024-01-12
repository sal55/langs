## 'Q' Source Files

* Q is my dynamic, interpreted language
* Sources are written in my 'M' systems language (with parts in my x64 assembly syntax)
* Uses hard tabs that need to be viewed with 4 spaces
* Lead module is qq.m, which contains all modules used (except std library which is implicit)
* The entry point is main() in qq_cli.m
* Modules not starting with 'qq' (m*.m) belong to M's standard library
* Module mwindll.m is the DLL stuff discussed. That function is called from qq_calldll.m
* Module qq_khandlers.m are the main bytecode handlers

Module qq_jhandlers.m has the inline ASM handlers. This is used with a special dispatch mode to provide acceleration: controls aims to stay in a tight loop using threaded-code dispatching, with globals SPTR, PCPTR, FRAMEPTR in registers, unless it has to call into the HLL handlers for stuff not supported here or to deal with errors.
