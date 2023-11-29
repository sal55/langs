Some Q interpreter source files:


* Source code is in my systems language
* View using 4-byte tabs (I haven't converted to spaces)
* Line comments start with !
* Start with `runqprogram()` in `qq_api.m`
* `qq_khandlers.m` shows HLL bytecode handlers
* `qq_jhandlers.m` shows ASM bytecode handlers (laid on top of the
   HLL ones; as much is possible is done here, only calling the HLL version
   when necessary). During ASM execution, essential globals (`sptr`, `pcptr`,
   `frameptr`) stay in machine registers
* `qq_tables.m` The set of enums such as `kadd` are the bytecodes. Here also
   are the types used
* `qq_decls.m` The 16-byte descriptor is `record varrec`, which stores
   value-types. For reference types, there is a separate 32-byte descriptor
   called `record objtype`.

