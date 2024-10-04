importdll $test =
    func "SDL_GetPlatform"                   ()ref i8
    const SDL_FALSE                          = 0
    const SDL_TRUE                           = 1
    const DUMMY_ENUM_VALUE                   = 0
    func "SDL_malloc"                        (u64)ref void
    func "SDL_calloc"                        (u64,u64)ref void
    func "SDL_realloc"                       (ref void,u64)ref void
    proc "SDL_free"                          (ref void)
    proc "SDL_GetOriginalMemoryFunctions"    (ref ref void,ref ref void,ref ref void,ref ref void)
    proc "SDL_GetMemoryFunctions"            (ref ref void,ref ref void,ref ref void,ref ref void)
    func "SDL_SetMemoryFunctions"            (ref void,ref void,ref void,ref void)i32
    func "SDL_GetNumAllocations"             ()i32
    func "SDL_getenv"                        (ref i8)ref i8
    func "SDL_setenv"                        (ref i8,ref i8,i32)i32
    proc "SDL_qsort"                         (ref void,u64,u64,ref void)
    func "SDL_bsearch"                       (ref void,ref void,u64,u64,ref void)ref void
    func "SDL_abs"                           (i32)i32
    func "SDL_isalpha"                       (i32)i32
    func "SDL_isalnum"                       (i32)i32
    func "SDL_isblank"                       (i32)i32
    func "SDL_iscntrl"                       (i32)i32
    func "SDL_isdigit"                       (i32)i32
    func "SDL_isxdigit"                      (i32)i32
    func "SDL_ispunct"                       (i32)i32
    func "SDL_isspace"                       (i32)i32
    func "SDL_isupper"                       (i32)i32
    func "SDL_islower"                       (i32)i32
    func "SDL_isprint"                       (i32)i32
    func "SDL_isgraph"                       (i32)i32
    func "SDL_toupper"                       (i32)i32
    func "SDL_tolower"                       (i32)i32
    func "SDL_crc16"                         (u16,ref void,u64)u16
    func "SDL_crc32"                         (u32,ref void,u64)u32
    func "SDL_memset"                        (ref void,i32,u64)ref void
    proc "SDL_memset4"                       (ref void,u32,u64)
    func "SDL_memcpy"                        (ref void,ref void,u64)ref void
    func "SDL_memmove"                       (ref void,ref void,u64)ref void
    func "SDL_memcmp"                        (ref void,ref void,u64)i32
    func "SDL_wcslen"                        (ref u16)u64
    func "SDL_wcslcpy"                       (ref u16,ref u16,u64)u64
    func "SDL_wcslcat"                       (ref u16,ref u16,u64)u64
    func "SDL_wcsdup"                        (ref u16)ref u16
    func "SDL_wcsstr"                        (ref u16,ref u16)ref u16
    func "SDL_wcscmp"                        (ref u16,ref u16)i32
    func "SDL_wcsncmp"                       (ref u16,ref u16,u64)i32
    func "SDL_wcscasecmp"                    (ref u16,ref u16)i32
    func "SDL_wcsncasecmp"                   (ref u16,ref u16,u64)i32
    func "SDL_strlen"                        (ref i8)u64
    func "SDL_strlcpy"                       (ref i8,ref i8,u64)u64
    func "SDL_utf8strlcpy"                   (ref i8,ref i8,u64)u64
    func "SDL_strlcat"                       (ref i8,ref i8,u64)u64
    func "SDL_strdup"                        (ref i8)ref i8
    func "SDL_strrev"                        (ref i8)ref i8
    func "SDL_strupr"                        (ref i8)ref i8
    func "SDL_strlwr"                        (ref i8)ref i8
    func "SDL_strchr"                        (ref i8,i32)ref i8
    func "SDL_strrchr"                       (ref i8,i32)ref i8
    func "SDL_strstr"                        (ref i8,ref i8)ref i8
    func "SDL_strcasestr"                    (ref i8,ref i8)ref i8
    func "SDL_strtokr"                       (ref i8,ref i8,ref ref i8)ref i8
    func "SDL_utf8strlen"                    (ref i8)u64
    func "SDL_utf8strnlen"                   (ref i8,u64)u64
    func "SDL_itoa"                          (i32,ref i8,i32)ref i8
    func "SDL_uitoa"                         (u32,ref i8,i32)ref i8
    func "SDL_ltoa"                          (i32,ref i8,i32)ref i8
    func "SDL_ultoa"                         (u32,ref i8,i32)ref i8
    func "SDL_lltoa"                         (i64,ref i8,i32)ref i8
    func "SDL_ulltoa"                        (u64,ref i8,i32)ref i8
    func "SDL_atoi"                          (ref i8)i32
    func "SDL_atof"                          (ref i8)r64
    func "SDL_strtol"                        (ref i8,ref ref i8,i32)i32
    func "SDL_strtoul"                       (ref i8,ref ref i8,i32)u32
    func "SDL_strtoll"                       (ref i8,ref ref i8,i32)i64
    func "SDL_strtoull"                      (ref i8,ref ref i8,i32)u64
    func "SDL_strtod"                        (ref i8,ref ref i8)r64
    func "SDL_strcmp"                        (ref i8,ref i8)i32
    func "SDL_strncmp"                       (ref i8,ref i8,u64)i32
    func "SDL_strcasecmp"                    (ref i8,ref i8)i32
    func "SDL_strncasecmp"                   (ref i8,ref i8,u64)i32
    func "SDL_sscanf"                        (ref i8,ref i8,...)i32
    func "SDL_vsscanf"                       (ref i8,ref i8,ref i8)i32
    func "SDL_snprintf"                      (ref i8,u64,ref i8,...)i32
    func "SDL_vsnprintf"                     (ref i8,u64,ref i8,ref i8)i32
    func "SDL_asprintf"                      (ref ref i8,ref i8,...)i32
    func "SDL_vasprintf"                     (ref ref i8,ref i8,ref i8)i32
    func "SDL_acos"                          (r64)r64
    func "SDL_acosf"                         (r32)r32
    func "SDL_asin"                          (r64)r64
    func "SDL_asinf"                         (r32)r32
    func "SDL_atan"                          (r64)r64
    func "SDL_atanf"                         (r32)r32
    func "SDL_atan2"                         (r64,r64)r64
    func "SDL_atan2f"                        (r32,r32)r32
    func "SDL_ceil"                          (r64)r64
    func "SDL_ceilf"                         (r32)r32
    func "SDL_copysign"                      (r64,r64)r64
    func "SDL_copysignf"                     (r32,r32)r32
    func "SDL_cos"                           (r64)r64
    func "SDL_cosf"                          (r32)r32
    func "SDL_exp"                           (r64)r64
    func "SDL_expf"                          (r32)r32
    func "SDL_fabs"                          (r64)r64
    func "SDL_fabsf"                         (r32)r32
    func "SDL_floor"                         (r64)r64
    func "SDL_floorf"                        (r32)r32
    func "SDL_trunc"                         (r64)r64
    func "SDL_truncf"                        (r32)r32
    func "SDL_fmod"                          (r64,r64)r64
    func "SDL_fmodf"                         (r32,r32)r32
    func "SDL_log"                           (r64)r64
    func "SDL_logf"                          (r32)r32
    func "SDL_log10"                         (r64)r64
    func "SDL_log10f"                        (r32)r32
    func "SDL_pow"                           (r64,r64)r64
    func "SDL_powf"                          (r32,r32)r32
    func "SDL_round"                         (r64)r64
    func "SDL_roundf"                        (r32)r32
    func "SDL_lround"                        (r64)i32
    func "SDL_lroundf"                       (r32)i32
    func "SDL_scalbn"                        (r64,i32)r64
    func "SDL_scalbnf"                       (r32,i32)r32
    func "SDL_sin"                           (r64)r64
    func "SDL_sinf"                          (r32)r32
    func "SDL_sqrt"                          (r64)r64
    func "SDL_sqrtf"                         (r32)r32
    func "SDL_tan"                           (r64)r64
    func "SDL_tanf"                          (r32)r32
    type _SDL_iconv_t = struct $caligned
        int dummy    !empty record
    end

    func "SDL_iconv_open"                    (ref i8,ref i8)ref _SDL_iconv_t
    func "SDL_iconv_close"                   (ref _SDL_iconv_t)i32
    func "SDL_iconv"                         (ref _SDL_iconv_t,ref ref i8,ref u64,ref ref i8,ref u64)u64
    func "SDL_iconv_string"                  (ref i8,ref i8,ref i8,u64)ref i8
    func "SDL_memcpy4"                       (ref void,ref void,u64)ref void
    func "SDL_size_mul_overflow"             (u64,u64,ref u64)i32
    func "SDL_size_add_overflow"             (u64,u64,ref u64)i32
    func "SDL_main"                          (i32,ref ref i8)i32
    proc "SDL_SetMainReady"                  ()
    func "SDL_RegisterApp"                   (ref i8,u32,ref void)i32
    proc "SDL_UnregisterApp"                 ()
    const SDL_ASSERTION_RETRY                = 0
    const SDL_ASSERTION_BREAK                = 1
    const SDL_ASSERTION_ABORT                = 2
    const SDL_ASSERTION_IGNORE               = 3
    const SDL_ASSERTION_ALWAYS_IGNORE        = 4
    type SDL_AssertData = struct $caligned
        i32 always_ignore
        u32 trigger_count
        ref i8 condition
        ref i8 filename
        i32 linenum
        ref i8 function$
        ref SDL_AssertData next$
    end

    func "SDL_ReportAssertion"               (ref SDL_AssertData,ref i8,ref i8,i32)i32
    proc "SDL_SetAssertionHandler"           (ref void,ref void)
    func "SDL_GetDefaultAssertionHandler"    ()ref void
    func "SDL_GetAssertionHandler"           (ref ref void)ref void
    func "SDL_GetAssertionReport"            ()ref SDL_AssertData
    proc "SDL_ResetAssertionReport"          ()
    func "SDL_AtomicTryLock"                 (ref i32)i32
    proc "SDL_AtomicLock"                    (ref i32)
    proc "SDL_AtomicUnlock"                  (ref i32)
    proc "SDL_MemoryBarrierReleaseFunction"  ()
    proc "SDL_MemoryBarrierAcquireFunction"  ()
    type $T1 = struct $caligned
        i32 value
    end

    func "SDL_AtomicCAS"                     (ref $T1,i32,i32)i32
    func "SDL_AtomicSet"                     (ref $T1,i32)i32
    func "SDL_AtomicGet"                     (ref $T1)i32
    func "SDL_AtomicAdd"                     (ref $T1,i32)i32
    func "SDL_AtomicCASPtr"                  (ref ref void,ref void,ref void)i32
    func "SDL_AtomicSetPtr"                  (ref ref void,ref void)ref void
    func "SDL_AtomicGetPtr"                  (ref ref void)ref void
    func "SDL_SetError"                      (ref i8,...)i32
    func "SDL_GetError"                      ()ref i8
    func "SDL_GetErrorMsg"                   (ref i8,i32)ref i8
    proc "SDL_ClearError"                    ()
    const SDL_ENOMEM                         = 0
    const SDL_EFREAD                         = 1
    const SDL_EFWRITE                        = 2
    const SDL_EFSEEK                         = 3
    const SDL_UNSUPPORTED                    = 4
    const SDL_LASTERROR                      = 5
    func "SDL_Error"                         (i32)i32
    func "SDL_Swap16"                        (u16)u16
    func "SDL_Swap32"                        (u32)u32
    func "SDL_Swap64"                        (u64)u64
    func "SDL_SwapFloat"                     (r32)r32
    type SDL_mutex = struct $caligned
        int dummy    !empty record
    end

    func "SDL_CreateMutex"                   ()ref SDL_mutex
    func "SDL_LockMutex"                     (ref SDL_mutex)i32
    func "SDL_TryLockMutex"                  (ref SDL_mutex)i32
    func "SDL_UnlockMutex"                   (ref SDL_mutex)i32
    proc "SDL_DestroyMutex"                  (ref SDL_mutex)
    type SDL_semaphore = struct $caligned
        int dummy    !empty record
    end

    func "SDL_CreateSemaphore"               (u32)ref SDL_semaphore
    proc "SDL_DestroySemaphore"              (ref SDL_semaphore)
    func "SDL_SemWait"                       (ref SDL_semaphore)i32
    func "SDL_SemTryWait"                    (ref SDL_semaphore)i32
    func "SDL_SemWaitTimeout"                (ref SDL_semaphore,u32)i32
    func "SDL_SemPost"                       (ref SDL_semaphore)i32
    func "SDL_SemValue"                      (ref SDL_semaphore)u32
    type SDL_cond = struct $caligned
        int dummy    !empty record
    end

    func "SDL_CreateCond"                    ()ref SDL_cond
    proc "SDL_DestroyCond"                   (ref SDL_cond)
    func "SDL_CondSignal"                    (ref SDL_cond)i32
    func "SDL_CondBroadcast"                 (ref SDL_cond)i32
    func "SDL_CondWait"                      (ref SDL_cond,ref SDL_mutex)i32
    func "SDL_CondWaitTimeout"               (ref SDL_cond,ref SDL_mutex,u32)i32
    func "_spawnvp"                          (i32,ref i8,ref ref i8)i32
    proc "endthread"                         ()
    func "_beginthreadex"                    (ref void,u32,ref void,ref void,u32,ref u32)u32
    proc "_endthreadex"                      (u32)
    func "_cwait"                            (ref i32,i32,i32)i32
    func "_execve"                           (ref i8,ref ref i8,ref ref i8)i64
    type SDL_Thread = struct $caligned
        int dummy    !empty record
    end

    const SDL_THREAD_PRIORITY_LOW            = 0
    const SDL_THREAD_PRIORITY_NORMAL         = 1
    const SDL_THREAD_PRIORITY_HIGH           = 2
    const SDL_THREAD_PRIORITY_TIME_CRITICAL  = 3
    func "SDL_CreateThread"                  (ref void,ref i8,ref void,ref void,ref void)ref SDL_Thread
    func "SDL_CreateThreadWithStackSize"     (ref void,ref i8,u64,ref void,ref void,ref void)ref SDL_Thread
    func "SDL_GetThreadName"                 (ref SDL_Thread)ref i8
    func "SDL_ThreadID"                      ()u32
    func "SDL_GetThreadID"                   (ref SDL_Thread)u32
    func "SDL_SetThreadPriority"             (i32)i32
    proc "SDL_WaitThread"                    (ref SDL_Thread,ref i32)
    proc "SDL_DetachThread"                  (ref SDL_Thread)
    func "SDL_TLSCreate"                     ()u32
    func "SDL_TLSGet"                        (u32)ref void
    func "SDL_TLSSet"                        (u32,ref void,ref void)i32
    proc "SDL_TLSCleanup"                    ()
    type SDL_RWops = struct $caligned
        ref void size
        ref void seek
        ref void read$
        ref void write
        ref void close
        u32 type$
        $T3 hidden
    end

    type $T3 = struct $caligned
        $T4 windowsio
        $T6 mem
        $T7 unknown
    end

    type $T4 = struct $caligned
        i32 append
        ref void h
        $T5 buffer
    end

    type $T5 = struct $caligned
        ref void data
        u64 size
        u64 left
    end

    type $T6 = struct $caligned
        ref u8 base
        ref u8 here
        ref u8 stop$
    end

    type $T7 = struct $caligned
        ref void data1
        ref void data2
    end

    func "SDL_RWFromFile"                    (ref i8,ref i8)ref SDL_RWops
    func "SDL_RWFromFP"                      (ref void,i32)ref SDL_RWops
    func "SDL_RWFromMem"                     (ref void,i32)ref SDL_RWops
    func "SDL_RWFromConstMem"                (ref void,i32)ref SDL_RWops
    func "SDL_AllocRW"                       ()ref SDL_RWops
    proc "SDL_FreeRW"                        (ref SDL_RWops)
    func "SDL_RWsize"                        (ref SDL_RWops)i64
    func "SDL_RWseek"                        (ref SDL_RWops,i64,i32)i64
    func "SDL_RWtell"                        (ref SDL_RWops)i64
    func "SDL_RWread"                        (ref SDL_RWops,ref void,u64,u64)u64
    func "SDL_RWwrite"                       (ref SDL_RWops,ref void,u64,u64)u64
    func "SDL_RWclose"                       (ref SDL_RWops)i32
    func "SDL_LoadFile_RW"                   (ref SDL_RWops,ref u64,i32)ref void
    func "SDL_LoadFile"                      (ref i8,ref u64)ref void
    func "SDL_ReadU8"                        (ref SDL_RWops)u8
    func "SDL_ReadLE16"                      (ref SDL_RWops)u16
    func "SDL_ReadBE16"                      (ref SDL_RWops)u16
    func "SDL_ReadLE32"                      (ref SDL_RWops)u32
    func "SDL_ReadBE32"                      (ref SDL_RWops)u32
    func "SDL_ReadLE64"                      (ref SDL_RWops)u64
    func "SDL_ReadBE64"                      (ref SDL_RWops)u64
    func "SDL_WriteU8"                       (ref SDL_RWops,u8)u64
    func "SDL_WriteLE16"                     (ref SDL_RWops,u16)u64
    func "SDL_WriteBE16"                     (ref SDL_RWops,u16)u64
    func "SDL_WriteLE32"                     (ref SDL_RWops,u32)u64
    func "SDL_WriteBE32"                     (ref SDL_RWops,u32)u64
    func "SDL_WriteLE64"                     (ref SDL_RWops,u64)u64
    func "SDL_WriteBE64"                     (ref SDL_RWops,u64)u64
    type SDL_AudioSpec = struct $caligned
        i32 freq
        u16 format
        u8 channels
        u8 silence
        u16 samples
        u16 padding
        u32 size
        ref void callback$
        ref void userdata
    end

    type SDL_AudioCVT = struct $caligned
        i32 needed
        u16 src_format
        u16 dst_format
        r64 rate_incr
        ref u8 buf
        i32 len$
        i32 len_cvt
        i32 len_mult
        r64 len_ratio
        [10]ref void filters
        i32 filter_index
    end

    func "SDL_GetNumAudioDrivers"            ()i32
    func "SDL_GetAudioDriver"                (i32)ref i8
    func "SDL_AudioInit"                     (ref i8)i32
    proc "SDL_AudioQuit"                     ()
    func "SDL_GetCurrentAudioDriver"         ()ref i8
    func "SDL_OpenAudio"                     (ref SDL_AudioSpec,ref SDL_AudioSpec)i32
    func "SDL_GetNumAudioDevices"            (i32)i32
    func "SDL_GetAudioDeviceName"            (i32,i32)ref i8
    func "SDL_GetAudioDeviceSpec"            (i32,i32,ref SDL_AudioSpec)i32
    func "SDL_GetDefaultAudioInfo"           (ref ref i8,ref SDL_AudioSpec,i32)i32
    func "SDL_OpenAudioDevice"               (ref i8,i32,ref SDL_AudioSpec,ref SDL_AudioSpec,i32)u32
    const SDL_AUDIO_STOPPED                  = 0
    const SDL_AUDIO_PLAYING                  = 1
    const SDL_AUDIO_PAUSED                   = 2
    func "SDL_GetAudioStatus"                ()i32
    func "SDL_GetAudioDeviceStatus"          (u32)i32
    proc "SDL_PauseAudio"                    (i32)
    proc "SDL_PauseAudioDevice"              (u32,i32)
    func "SDL_LoadWAV_RW"                    (ref SDL_RWops,i32,ref SDL_AudioSpec,ref ref u8,ref u32)ref SDL_AudioSpec
    proc "SDL_FreeWAV"                       (ref u8)
    func "SDL_BuildAudioCVT"                 (ref SDL_AudioCVT,u16,u8,i32,u16,u8,i32)i32
    func "SDL_ConvertAudio"                  (ref SDL_AudioCVT)i32
    type _SDL_AudioStream = struct $caligned
        int dummy    !empty record
    end

    func "SDL_NewAudioStream"                (u16,u8,i32,u16,u8,i32)ref _SDL_AudioStream
    func "SDL_AudioStreamPut"                (ref _SDL_AudioStream,ref void,i32)i32
    func "SDL_AudioStreamGet"                (ref _SDL_AudioStream,ref void,i32)i32
    func "SDL_AudioStreamAvailable"          (ref _SDL_AudioStream)i32
    func "SDL_AudioStreamFlush"              (ref _SDL_AudioStream)i32
    proc "SDL_AudioStreamClear"              (ref _SDL_AudioStream)
    proc "SDL_FreeAudioStream"               (ref _SDL_AudioStream)
    proc "SDL_MixAudio"                      (ref u8,ref u8,u32,i32)
    proc "SDL_MixAudioFormat"                (ref u8,ref u8,u16,u32,i32)
    func "SDL_QueueAudio"                    (u32,ref void,u32)i32
    func "SDL_DequeueAudio"                  (u32,ref void,u32)u32
    func "SDL_GetQueuedAudioSize"            (u32)u32
    proc "SDL_ClearQueuedAudio"              (u32)
    proc "SDL_LockAudio"                     ()
    proc "SDL_LockAudioDevice"               (u32)
    proc "SDL_UnlockAudio"                   ()
    proc "SDL_UnlockAudioDevice"             (u32)
    proc "SDL_CloseAudio"                    ()
    proc "SDL_CloseAudioDevice"              (u32)
    func "SDL_SetClipboardText"              (ref i8)i32
    func "SDL_GetClipboardText"              ()ref i8
    func "SDL_HasClipboardText"              ()i32
    func "SDL_SetPrimarySelectionText"       (ref i8)i32
    func "SDL_GetPrimarySelectionText"       ()ref i8
    func "SDL_HasPrimarySelectionText"       ()i32
    func "SDL_GetCPUCount"                   ()i32
    func "SDL_GetCPUCacheLineSize"           ()i32
    func "SDL_HasRDTSC"                      ()i32
    func "SDL_HasAltiVec"                    ()i32
    func "SDL_HasMMX"                        ()i32
    func "SDL_Has3DNow"                      ()i32
    func "SDL_HasSSE"                        ()i32
    func "SDL_HasSSE2"                       ()i32
    func "SDL_HasSSE3"                       ()i32
    func "SDL_HasSSE41"                      ()i32
    func "SDL_HasSSE42"                      ()i32
    func "SDL_HasAVX"                        ()i32
    func "SDL_HasAVX2"                       ()i32
    func "SDL_HasAVX512F"                    ()i32
    func "SDL_HasARMSIMD"                    ()i32
    func "SDL_HasNEON"                       ()i32
    func "SDL_HasLSX"                        ()i32
    func "SDL_HasLASX"                       ()i32
    func "SDL_GetSystemRAM"                  ()i32
    func "SDL_SIMDGetAlignment"              ()u64
    func "SDL_SIMDAlloc"                     (u64)ref void
    func "SDL_SIMDRealloc"                   (ref void,u64)ref void
    proc "SDL_SIMDFree"                      (ref void)
    const SDL_PIXELTYPE_UNKNOWN              = 0
    const SDL_PIXELTYPE_INDEX1               = 1
    const SDL_PIXELTYPE_INDEX4               = 2
    const SDL_PIXELTYPE_INDEX8               = 3
    const SDL_PIXELTYPE_PACKED8              = 4
    const SDL_PIXELTYPE_PACKED16             = 5
    const SDL_PIXELTYPE_PACKED32             = 6
    const SDL_PIXELTYPE_ARRAYU8              = 7
    const SDL_PIXELTYPE_ARRAYU16             = 8
    const SDL_PIXELTYPE_ARRAYU32             = 9
    const SDL_PIXELTYPE_ARRAYF16             = 10
    const SDL_PIXELTYPE_ARRAYF32             = 11
    const SDL_BITMAPORDER_NONE               = 0
    const SDL_BITMAPORDER_4321               = 1
    const SDL_BITMAPORDER_1234               = 2
    const SDL_PACKEDORDER_NONE               = 0
    const SDL_PACKEDORDER_XRGB               = 1
    const SDL_PACKEDORDER_RGBX               = 2
    const SDL_PACKEDORDER_ARGB               = 3
    const SDL_PACKEDORDER_RGBA               = 4
    const SDL_PACKEDORDER_XBGR               = 5
    const SDL_PACKEDORDER_BGRX               = 6
    const SDL_PACKEDORDER_ABGR               = 7
    const SDL_PACKEDORDER_BGRA               = 8
    const SDL_ARRAYORDER_NONE                = 0
    const SDL_ARRAYORDER_RGB                 = 1
    const SDL_ARRAYORDER_RGBA                = 2
    const SDL_ARRAYORDER_ARGB                = 3
    const SDL_ARRAYORDER_BGR                 = 4
    const SDL_ARRAYORDER_BGRA                = 5
    const SDL_ARRAYORDER_ABGR                = 6
    const SDL_PACKEDLAYOUT_NONE              = 0
    const SDL_PACKEDLAYOUT_332               = 1
    const SDL_PACKEDLAYOUT_4444              = 2
    const SDL_PACKEDLAYOUT_1555              = 3
    const SDL_PACKEDLAYOUT_5551              = 4
    const SDL_PACKEDLAYOUT_565               = 5
    const SDL_PACKEDLAYOUT_8888              = 6
    const SDL_PACKEDLAYOUT_2101010           = 7
    const SDL_PACKEDLAYOUT_1010102           = 8
    const SDL_PIXELFORMAT_UNKNOWN            = 0
    const SDL_PIXELFORMAT_INDEX1LSB          = 286261504
    const SDL_PIXELFORMAT_INDEX1MSB          = 287310080
    const SDL_PIXELFORMAT_INDEX4LSB          = 303039488
    const SDL_PIXELFORMAT_INDEX4MSB          = 304088064
    const SDL_PIXELFORMAT_INDEX8             = 318769153
    const SDL_PIXELFORMAT_RGB332             = 336660481
    const SDL_PIXELFORMAT_XRGB4444           = 353504258
    const SDL_PIXELFORMAT_RGB444             = 353504258
    const SDL_PIXELFORMAT_XBGR4444           = 357698562
    const SDL_PIXELFORMAT_BGR444             = 357698562
    const SDL_PIXELFORMAT_XRGB1555           = 353570562
    const SDL_PIXELFORMAT_RGB555             = 353570562
    const SDL_PIXELFORMAT_XBGR1555           = 357764866
    const SDL_PIXELFORMAT_BGR555             = 357764866
    const SDL_PIXELFORMAT_ARGB4444           = 355602434
    const SDL_PIXELFORMAT_RGBA4444           = 356651010
    const SDL_PIXELFORMAT_ABGR4444           = 359796738
    const SDL_PIXELFORMAT_BGRA4444           = 360845314
    const SDL_PIXELFORMAT_ARGB1555           = 355667970
    const SDL_PIXELFORMAT_RGBA5551           = 356782082
    const SDL_PIXELFORMAT_ABGR1555           = 359862274
    const SDL_PIXELFORMAT_BGRA5551           = 360976386
    const SDL_PIXELFORMAT_RGB565             = 353701890
    const SDL_PIXELFORMAT_BGR565             = 357896194
    const SDL_PIXELFORMAT_RGB24              = 386930691
    const SDL_PIXELFORMAT_BGR24              = 390076419
    const SDL_PIXELFORMAT_XRGB8888           = 370546692
    const SDL_PIXELFORMAT_RGB888             = 370546692
    const SDL_PIXELFORMAT_RGBX8888           = 371595268
    const SDL_PIXELFORMAT_XBGR8888           = 374740996
    const SDL_PIXELFORMAT_BGR888             = 374740996
    const SDL_PIXELFORMAT_BGRX8888           = 375789572
    const SDL_PIXELFORMAT_ARGB8888           = 372645892
    const SDL_PIXELFORMAT_RGBA8888           = 373694468
    const SDL_PIXELFORMAT_ABGR8888           = 376840196
    const SDL_PIXELFORMAT_BGRA8888           = 377888772
    const SDL_PIXELFORMAT_ARGB2101010        = 372711428
    const SDL_PIXELFORMAT_RGBA32             = 376840196
    const SDL_PIXELFORMAT_ARGB32             = 377888772
    const SDL_PIXELFORMAT_BGRA32             = 372645892
    const SDL_PIXELFORMAT_ABGR32             = 373694468
    const SDL_PIXELFORMAT_YV12               = 842094169
    const SDL_PIXELFORMAT_IYUV               = 1448433993
    const SDL_PIXELFORMAT_YUY2               = 844715353
    const SDL_PIXELFORMAT_UYVY               = 1498831189
    const SDL_PIXELFORMAT_YVYU               = 1431918169
    const SDL_PIXELFORMAT_NV12               = 842094158
    const SDL_PIXELFORMAT_NV21               = 825382478
    const SDL_PIXELFORMAT_EXTERNAL_OES       = 542328143
    type SDL_Color = struct $caligned
        u8 r
        u8 g
        u8 b
        u8 a
    end

    type SDL_Palette = struct $caligned
        i32 ncolors
        ref SDL_Color colors
        u32 version
        i32 refcount
    end

    type SDL_PixelFormat = struct $caligned
        u32 format
        ref SDL_Palette palette
        u8 BitsPerPixel
        u8 BytesPerPixel
        [2]u8 padding
        u32 Rmask
        u32 Gmask
        u32 Bmask
        u32 Amask
        u8 Rloss
        u8 Gloss
        u8 Bloss
        u8 Aloss
        u8 Rshift
        u8 Gshift
        u8 Bshift
        u8 Ashift
        i32 refcount
        ref SDL_PixelFormat next$
    end

    func "SDL_GetPixelFormatName"            (u32)ref i8
    func "SDL_PixelFormatEnumToMasks"        (u32,ref i32,ref u32,ref u32,ref u32,ref u32)i32
    func "SDL_MasksToPixelFormatEnum"        (i32,u32,u32,u32,u32)u32
    func "SDL_AllocFormat"                   (u32)ref SDL_PixelFormat
    proc "SDL_FreeFormat"                    (ref SDL_PixelFormat)
    func "SDL_AllocPalette"                  (i32)ref SDL_Palette
    func "SDL_SetPixelFormatPalette"         (ref SDL_PixelFormat,ref SDL_Palette)i32
    func "SDL_SetPaletteColors"              (ref SDL_Palette,ref SDL_Color,i32,i32)i32
    proc "SDL_FreePalette"                   (ref SDL_Palette)
    func "SDL_MapRGB"                        (ref SDL_PixelFormat,u8,u8,u8)u32
    func "SDL_MapRGBA"                       (ref SDL_PixelFormat,u8,u8,u8,u8)u32
    proc "SDL_GetRGB"                        (u32,ref SDL_PixelFormat,ref u8,ref u8,ref u8)
    proc "SDL_GetRGBA"                       (u32,ref SDL_PixelFormat,ref u8,ref u8,ref u8,ref u8)
    proc "SDL_CalculateGammaRamp"            (r32,ref u16)
    type SDL_Point = struct $caligned
        i32 x
        i32 y
    end

    type SDL_FPoint = struct $caligned
        r32 x
        r32 y
    end

    type SDL_Rect = struct $caligned
        i32 x
        i32 y
        i32 w
        i32 h
    end

    type SDL_FRect = struct $caligned
        r32 x
        r32 y
        r32 w
        r32 h
    end

    func "SDL_PointInRect"                   (ref SDL_Point,ref SDL_Rect)i32
    func "SDL_RectEmpty"                     (ref SDL_Rect)i32
    func "SDL_RectEquals"                    (ref SDL_Rect,ref SDL_Rect)i32
    func "SDL_HasIntersection"               (ref SDL_Rect,ref SDL_Rect)i32
    func "SDL_IntersectRect"                 (ref SDL_Rect,ref SDL_Rect,ref SDL_Rect)i32
    proc "SDL_UnionRect"                     (ref SDL_Rect,ref SDL_Rect,ref SDL_Rect)
    func "SDL_EnclosePoints"                 (ref SDL_Point,i32,ref SDL_Rect,ref SDL_Rect)i32
    func "SDL_IntersectRectAndLine"          (ref SDL_Rect,ref i32,ref i32,ref i32,ref i32)i32
    func "SDL_PointInFRect"                  (ref SDL_FPoint,ref SDL_FRect)i32
    func "SDL_FRectEmpty"                    (ref SDL_FRect)i32
    func "SDL_FRectEqualsEpsilon"            (ref SDL_FRect,ref SDL_FRect,r32)i32
    func "SDL_FRectEquals"                   (ref SDL_FRect,ref SDL_FRect)i32
    func "SDL_HasIntersectionF"              (ref SDL_FRect,ref SDL_FRect)i32
    func "SDL_IntersectFRect"                (ref SDL_FRect,ref SDL_FRect,ref SDL_FRect)i32
    proc "SDL_UnionFRect"                    (ref SDL_FRect,ref SDL_FRect,ref SDL_FRect)
    func "SDL_EncloseFPoints"                (ref SDL_FPoint,i32,ref SDL_FRect,ref SDL_FRect)i32
    func "SDL_IntersectFRectAndLine"         (ref SDL_FRect,ref r32,ref r32,ref r32,ref r32)i32
    const SDL_BLENDMODE_NONE                 = 0
    const SDL_BLENDMODE_BLEND                = 1
    const SDL_BLENDMODE_ADD                  = 2
    const SDL_BLENDMODE_MOD                  = 4
    const SDL_BLENDMODE_MUL                  = 8
    const SDL_BLENDMODE_INVALID              = 2147483647
    const SDL_BLENDOPERATION_ADD             = 1
    const SDL_BLENDOPERATION_SUBTRACT        = 2
    const SDL_BLENDOPERATION_REV_SUBTRACT    = 3
    const SDL_BLENDOPERATION_MINIMUM         = 4
    const SDL_BLENDOPERATION_MAXIMUM         = 5
    const SDL_BLENDFACTOR_ZERO               = 1
    const SDL_BLENDFACTOR_ONE                = 2
    const SDL_BLENDFACTOR_SRC_COLOR          = 3
    const SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR = 4
    const SDL_BLENDFACTOR_SRC_ALPHA          = 5
    const SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA = 6
    const SDL_BLENDFACTOR_DST_COLOR          = 7
    const SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR = 8
    const SDL_BLENDFACTOR_DST_ALPHA          = 9
    const SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA = 10
    func "SDL_ComposeCustomBlendMode"        (i32,i32,i32,i32,i32,i32)i32
    type SDL_BlitMap = struct $caligned
        int dummy    !empty record
    end

    type SDL_Surface = struct $caligned
        u32 flags
        ref SDL_PixelFormat format
        i32 w
        i32 h
        i32 pitch
        ref void pixels
        ref void userdata
        i32 locked
        ref void list_blitmap
        SDL_Rect clip_rect
        ref SDL_BlitMap map
        i32 refcount
    end

    const SDL_YUV_CONVERSION_JPEG            = 0
    const SDL_YUV_CONVERSION_BT601           = 1
    const SDL_YUV_CONVERSION_BT709           = 2
    const SDL_YUV_CONVERSION_AUTOMATIC       = 3
    func "SDL_CreateRGBSurface"              (u32,i32,i32,i32,u32,u32,u32,u32)ref SDL_Surface
    func "SDL_CreateRGBSurfaceWithFormat"    (u32,i32,i32,i32,u32)ref SDL_Surface
    func "SDL_CreateRGBSurfaceFrom"          (ref void,i32,i32,i32,i32,u32,u32,u32,u32)ref SDL_Surface
    func "SDL_CreateRGBSurfaceWithFormatFrom" (ref void,i32,i32,i32,i32,u32)ref SDL_Surface
    proc "SDL_FreeSurface"                   (ref SDL_Surface)
    func "SDL_SetSurfacePalette"             (ref SDL_Surface,ref SDL_Palette)i32
    func "SDL_LockSurface"                   (ref SDL_Surface)i32
    proc "SDL_UnlockSurface"                 (ref SDL_Surface)
    func "SDL_LoadBMP_RW"                    (ref SDL_RWops,i32)ref SDL_Surface
    func "SDL_SaveBMP_RW"                    (ref SDL_Surface,ref SDL_RWops,i32)i32
    func "SDL_SetSurfaceRLE"                 (ref SDL_Surface,i32)i32
    func "SDL_HasSurfaceRLE"                 (ref SDL_Surface)i32
    func "SDL_SetColorKey"                   (ref SDL_Surface,i32,u32)i32
    func "SDL_HasColorKey"                   (ref SDL_Surface)i32
    func "SDL_GetColorKey"                   (ref SDL_Surface,ref u32)i32
    func "SDL_SetSurfaceColorMod"            (ref SDL_Surface,u8,u8,u8)i32
    func "SDL_GetSurfaceColorMod"            (ref SDL_Surface,ref u8,ref u8,ref u8)i32
    func "SDL_SetSurfaceAlphaMod"            (ref SDL_Surface,u8)i32
    func "SDL_GetSurfaceAlphaMod"            (ref SDL_Surface,ref u8)i32
    func "SDL_SetSurfaceBlendMode"           (ref SDL_Surface,i32)i32
    func "SDL_GetSurfaceBlendMode"           (ref SDL_Surface,ref i32)i32
    func "SDL_SetClipRect"                   (ref SDL_Surface,ref SDL_Rect)i32
    proc "SDL_GetClipRect"                   (ref SDL_Surface,ref SDL_Rect)
    func "SDL_DuplicateSurface"              (ref SDL_Surface)ref SDL_Surface
    func "SDL_ConvertSurface"                (ref SDL_Surface,ref SDL_PixelFormat,u32)ref SDL_Surface
    func "SDL_ConvertSurfaceFormat"          (ref SDL_Surface,u32,u32)ref SDL_Surface
    func "SDL_ConvertPixels"                 (i32,i32,u32,ref void,i32,u32,ref void,i32)i32
    func "SDL_PremultiplyAlpha"              (i32,i32,u32,ref void,i32,u32,ref void,i32)i32
    func "SDL_FillRect"                      (ref SDL_Surface,ref SDL_Rect,u32)i32
    func "SDL_FillRects"                     (ref SDL_Surface,ref SDL_Rect,i32,u32)i32
    func "SDL_UpperBlit"                     (ref SDL_Surface,ref SDL_Rect,ref SDL_Surface,ref SDL_Rect)i32
    func "SDL_LowerBlit"                     (ref SDL_Surface,ref SDL_Rect,ref SDL_Surface,ref SDL_Rect)i32
    func "SDL_SoftStretch"                   (ref SDL_Surface,ref SDL_Rect,ref SDL_Surface,ref SDL_Rect)i32
    func "SDL_SoftStretchLinear"             (ref SDL_Surface,ref SDL_Rect,ref SDL_Surface,ref SDL_Rect)i32
    func "SDL_UpperBlitScaled"               (ref SDL_Surface,ref SDL_Rect,ref SDL_Surface,ref SDL_Rect)i32
    func "SDL_LowerBlitScaled"               (ref SDL_Surface,ref SDL_Rect,ref SDL_Surface,ref SDL_Rect)i32
    proc "SDL_SetYUVConversionMode"          (i32)
    func "SDL_GetYUVConversionMode"          ()i32
    func "SDL_GetYUVConversionModeForResolution" (i32,i32)i32
    type $T8 = struct $caligned
        u32 format
        i32 w
        i32 h
        i32 refresh_rate
        ref void driverdata
    end

    type SDL_Window = struct $caligned
        int dummy    !empty record
    end

    const SDL_WINDOW_FULLSCREEN              = 1
    const SDL_WINDOW_OPENGL                  = 2
    const SDL_WINDOW_SHOWN                   = 4
    const SDL_WINDOW_HIDDEN                  = 8
    const SDL_WINDOW_BORDERLESS              = 16
    const SDL_WINDOW_RESIZABLE               = 32
    const SDL_WINDOW_MINIMIZED               = 64
    const SDL_WINDOW_MAXIMIZED               = 128
    const SDL_WINDOW_MOUSE_GRABBED           = 256
    const SDL_WINDOW_INPUT_FOCUS             = 512
    const SDL_WINDOW_MOUSE_FOCUS             = 1024
    const SDL_WINDOW_FULLSCREEN_DESKTOP      = 4097
    const SDL_WINDOW_FOREIGN                 = 2048
    const SDL_WINDOW_ALLOW_HIGHDPI           = 8192
    const SDL_WINDOW_MOUSE_CAPTURE           = 16384
    const SDL_WINDOW_ALWAYS_ON_TOP           = 32768
    const SDL_WINDOW_SKIP_TASKBAR            = 65536
    const SDL_WINDOW_UTILITY                 = 131072
    const SDL_WINDOW_TOOLTIP                 = 262144
    const SDL_WINDOW_POPUP_MENU              = 524288
    const SDL_WINDOW_KEYBOARD_GRABBED        = 1048576
    const SDL_WINDOW_VULKAN                  = 268435456
    const SDL_WINDOW_METAL                   = 536870912
    const SDL_WINDOW_INPUT_GRABBED           = 256
    const SDL_WINDOWEVENT_NONE               = 0
    const SDL_WINDOWEVENT_SHOWN              = 1
    const SDL_WINDOWEVENT_HIDDEN             = 2
    const SDL_WINDOWEVENT_EXPOSED            = 3
    const SDL_WINDOWEVENT_MOVED              = 4
    const SDL_WINDOWEVENT_RESIZED            = 5
    const SDL_WINDOWEVENT_SIZE_CHANGED       = 6
    const SDL_WINDOWEVENT_MINIMIZED          = 7
    const SDL_WINDOWEVENT_MAXIMIZED          = 8
    const SDL_WINDOWEVENT_RESTORED           = 9
    const SDL_WINDOWEVENT_ENTER              = 10
    const SDL_WINDOWEVENT_LEAVE              = 11
    const SDL_WINDOWEVENT_FOCUS_GAINED       = 12
    const SDL_WINDOWEVENT_FOCUS_LOST         = 13
    const SDL_WINDOWEVENT_CLOSE              = 14
    const SDL_WINDOWEVENT_TAKE_FOCUS         = 15
    const SDL_WINDOWEVENT_HIT_TEST           = 16
    const SDL_WINDOWEVENT_ICCPROF_CHANGED    = 17
    const SDL_WINDOWEVENT_DISPLAY_CHANGED    = 18
    const SDL_DISPLAYEVENT_NONE              = 0
    const SDL_DISPLAYEVENT_ORIENTATION       = 1
    const SDL_DISPLAYEVENT_CONNECTED         = 2
    const SDL_DISPLAYEVENT_DISCONNECTED      = 3
    const SDL_ORIENTATION_UNKNOWN            = 0
    const SDL_ORIENTATION_LANDSCAPE          = 1
    const SDL_ORIENTATION_LANDSCAPE_FLIPPED  = 2
    const SDL_ORIENTATION_PORTRAIT           = 3
    const SDL_ORIENTATION_PORTRAIT_FLIPPED   = 4
    const SDL_FLASH_CANCEL                   = 0
    const SDL_FLASH_BRIEFLY                  = 1
    const SDL_FLASH_UNTIL_FOCUSED            = 2
    const SDL_GL_RED_SIZE                    = 0
    const SDL_GL_GREEN_SIZE                  = 1
    const SDL_GL_BLUE_SIZE                   = 2
    const SDL_GL_ALPHA_SIZE                  = 3
    const SDL_GL_BUFFER_SIZE                 = 4
    const SDL_GL_DOUBLEBUFFER                = 5
    const SDL_GL_DEPTH_SIZE                  = 6
    const SDL_GL_STENCIL_SIZE                = 7
    const SDL_GL_ACCUM_RED_SIZE              = 8
    const SDL_GL_ACCUM_GREEN_SIZE            = 9
    const SDL_GL_ACCUM_BLUE_SIZE             = 10
    const SDL_GL_ACCUM_ALPHA_SIZE            = 11
    const SDL_GL_STEREO                      = 12
    const SDL_GL_MULTISAMPLEBUFFERS          = 13
    const SDL_GL_MULTISAMPLESAMPLES          = 14
    const SDL_GL_ACCELERATED_VISUAL          = 15
    const SDL_GL_RETAINED_BACKING            = 16
    const SDL_GL_CONTEXT_MAJOR_VERSION       = 17
    const SDL_GL_CONTEXT_MINOR_VERSION       = 18
    const SDL_GL_CONTEXT_EGL                 = 19
    const SDL_GL_CONTEXT_FLAGS               = 20
    const SDL_GL_CONTEXT_PROFILE_MASK        = 21
    const SDL_GL_SHARE_WITH_CURRENT_CONTEXT  = 22
    const SDL_GL_FRAMEBUFFER_SRGB_CAPABLE    = 23
    const SDL_GL_CONTEXT_RELEASE_BEHAVIOR    = 24
    const SDL_GL_CONTEXT_RESET_NOTIFICATION  = 25
    const SDL_GL_CONTEXT_NO_ERROR            = 26
    const SDL_GL_FLOATBUFFERS                = 27
    const SDL_GL_CONTEXT_PROFILE_CORE        = 1
    const SDL_GL_CONTEXT_PROFILE_COMPATIBILITY = 2
    const SDL_GL_CONTEXT_PROFILE_ES          = 4
    const SDL_GL_CONTEXT_DEBUG_FLAG          = 1
    const SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG = 2
    const SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG  = 4
    const SDL_GL_CONTEXT_RESET_ISOLATION_FLAG = 8
    const SDL_GL_CONTEXT_RELEASE_BEHAVIOR_NONE = 0
    const SDL_GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH = 1
    const SDL_GL_CONTEXT_RESET_NO_NOTIFICATION = 0
    const SDL_GL_CONTEXT_RESET_LOSE_CONTEXT  = 1
    func "SDL_GetNumVideoDrivers"            ()i32
    func "SDL_GetVideoDriver"                (i32)ref i8
    func "SDL_VideoInit"                     (ref i8)i32
    proc "SDL_VideoQuit"                     ()
    func "SDL_GetCurrentVideoDriver"         ()ref i8
    func "SDL_GetNumVideoDisplays"           ()i32
    func "SDL_GetDisplayName"                (i32)ref i8
    func "SDL_GetDisplayBounds"              (i32,ref SDL_Rect)i32
    func "SDL_GetDisplayUsableBounds"        (i32,ref SDL_Rect)i32
    func "SDL_GetDisplayDPI"                 (i32,ref r32,ref r32,ref r32)i32
    func "SDL_GetDisplayOrientation"         (i32)i32
    func "SDL_GetNumDisplayModes"            (i32)i32
    func "SDL_GetDisplayMode"                (i32,i32,ref $T8)i32
    func "SDL_GetDesktopDisplayMode"         (i32,ref $T8)i32
    func "SDL_GetCurrentDisplayMode"         (i32,ref $T8)i32
    func "SDL_GetClosestDisplayMode"         (i32,ref $T8,ref $T8)ref $T8
    func "SDL_GetPointDisplayIndex"          (ref SDL_Point)i32
    func "SDL_GetRectDisplayIndex"           (ref SDL_Rect)i32
    func "SDL_GetWindowDisplayIndex"         (ref SDL_Window)i32
    func "SDL_SetWindowDisplayMode"          (ref SDL_Window,ref $T8)i32
    func "SDL_GetWindowDisplayMode"          (ref SDL_Window,ref $T8)i32
    func "SDL_GetWindowICCProfile"           (ref SDL_Window,ref u64)ref void
    func "SDL_GetWindowPixelFormat"          (ref SDL_Window)u32
    func "SDL_CreateWindow"                  (ref i8,i32,i32,i32,i32,u32)ref SDL_Window
    func "SDL_CreateWindowFrom"              (ref void)ref SDL_Window
    func "SDL_GetWindowID"                   (ref SDL_Window)u32
    func "SDL_GetWindowFromID"               (u32)ref SDL_Window
    func "SDL_GetWindowFlags"                (ref SDL_Window)u32
    proc "SDL_SetWindowTitle"                (ref SDL_Window,ref i8)
    func "SDL_GetWindowTitle"                (ref SDL_Window)ref i8
    proc "SDL_SetWindowIcon"                 (ref SDL_Window,ref SDL_Surface)
    func "SDL_SetWindowData"                 (ref SDL_Window,ref i8,ref void)ref void
    func "SDL_GetWindowData"                 (ref SDL_Window,ref i8)ref void
    proc "SDL_SetWindowPosition"             (ref SDL_Window,i32,i32)
    proc "SDL_GetWindowPosition"             (ref SDL_Window,ref i32,ref i32)
    proc "SDL_SetWindowSize"                 (ref SDL_Window,i32,i32)
    proc "SDL_GetWindowSize"                 (ref SDL_Window,ref i32,ref i32)
    func "SDL_GetWindowBordersSize"          (ref SDL_Window,ref i32,ref i32,ref i32,ref i32)i32
    proc "SDL_GetWindowSizeInPixels"         (ref SDL_Window,ref i32,ref i32)
    proc "SDL_SetWindowMinimumSize"          (ref SDL_Window,i32,i32)
    proc "SDL_GetWindowMinimumSize"          (ref SDL_Window,ref i32,ref i32)
    proc "SDL_SetWindowMaximumSize"          (ref SDL_Window,i32,i32)
    proc "SDL_GetWindowMaximumSize"          (ref SDL_Window,ref i32,ref i32)
    proc "SDL_SetWindowBordered"             (ref SDL_Window,i32)
    proc "SDL_SetWindowResizable"            (ref SDL_Window,i32)
    proc "SDL_SetWindowAlwaysOnTop"          (ref SDL_Window,i32)
    proc "SDL_ShowWindow"                    (ref SDL_Window)
    proc "SDL_HideWindow"                    (ref SDL_Window)
    proc "SDL_RaiseWindow"                   (ref SDL_Window)
    proc "SDL_MaximizeWindow"                (ref SDL_Window)
    proc "SDL_MinimizeWindow"                (ref SDL_Window)
    proc "SDL_RestoreWindow"                 (ref SDL_Window)
    func "SDL_SetWindowFullscreen"           (ref SDL_Window,u32)i32
    func "SDL_GetWindowSurface"              (ref SDL_Window)ref SDL_Surface
    func "SDL_UpdateWindowSurface"           (ref SDL_Window)i32
    func "SDL_UpdateWindowSurfaceRects"      (ref SDL_Window,ref SDL_Rect,i32)i32
    proc "SDL_SetWindowGrab"                 (ref SDL_Window,i32)
    proc "SDL_SetWindowKeyboardGrab"         (ref SDL_Window,i32)
    proc "SDL_SetWindowMouseGrab"            (ref SDL_Window,i32)
    func "SDL_GetWindowGrab"                 (ref SDL_Window)i32
    func "SDL_GetWindowKeyboardGrab"         (ref SDL_Window)i32
    func "SDL_GetWindowMouseGrab"            (ref SDL_Window)i32
    func "SDL_GetGrabbedWindow"              ()ref SDL_Window
    func "SDL_SetWindowMouseRect"            (ref SDL_Window,ref SDL_Rect)i32
    func "SDL_GetWindowMouseRect"            (ref SDL_Window)ref SDL_Rect
    func "SDL_SetWindowBrightness"           (ref SDL_Window,r32)i32
    func "SDL_GetWindowBrightness"           (ref SDL_Window)r32
    func "SDL_SetWindowOpacity"              (ref SDL_Window,r32)i32
    func "SDL_GetWindowOpacity"              (ref SDL_Window,ref r32)i32
    func "SDL_SetWindowModalFor"             (ref SDL_Window,ref SDL_Window)i32
    func "SDL_SetWindowInputFocus"           (ref SDL_Window)i32
    func "SDL_SetWindowGammaRamp"            (ref SDL_Window,ref u16,ref u16,ref u16)i32
    func "SDL_GetWindowGammaRamp"            (ref SDL_Window,ref u16,ref u16,ref u16)i32
    const SDL_HITTEST_NORMAL                 = 0
    const SDL_HITTEST_DRAGGABLE              = 1
    const SDL_HITTEST_RESIZE_TOPLEFT         = 2
    const SDL_HITTEST_RESIZE_TOP             = 3
    const SDL_HITTEST_RESIZE_TOPRIGHT        = 4
    const SDL_HITTEST_RESIZE_RIGHT           = 5
    const SDL_HITTEST_RESIZE_BOTTOMRIGHT     = 6
    const SDL_HITTEST_RESIZE_BOTTOM          = 7
    const SDL_HITTEST_RESIZE_BOTTOMLEFT      = 8
    const SDL_HITTEST_RESIZE_LEFT            = 9
    func "SDL_SetWindowHitTest"              (ref SDL_Window,ref void,ref void)i32
    func "SDL_FlashWindow"                   (ref SDL_Window,i32)i32
    proc "SDL_DestroyWindow"                 (ref SDL_Window)
    func "SDL_IsScreenSaverEnabled"          ()i32
    proc "SDL_EnableScreenSaver"             ()
    proc "SDL_DisableScreenSaver"            ()
    func "SDL_GL_LoadLibrary"                (ref i8)i32
    func "SDL_GL_GetProcAddress"             (ref i8)ref void
    proc "SDL_GL_UnloadLibrary"              ()
    func "SDL_GL_ExtensionSupported"         (ref i8)i32
    proc "SDL_GL_ResetAttributes"            ()
    func "SDL_GL_SetAttribute"               (i32,i32)i32
    func "SDL_GL_GetAttribute"               (i32,ref i32)i32
    func "SDL_GL_CreateContext"              (ref SDL_Window)ref void
    func "SDL_GL_MakeCurrent"                (ref SDL_Window,ref void)i32
    func "SDL_GL_GetCurrentWindow"           ()ref SDL_Window
    func "SDL_GL_GetCurrentContext"          ()ref void
    proc "SDL_GL_GetDrawableSize"            (ref SDL_Window,ref i32,ref i32)
    func "SDL_GL_SetSwapInterval"            (i32)i32
    func "SDL_GL_GetSwapInterval"            ()i32
    proc "SDL_GL_SwapWindow"                 (ref SDL_Window)
    proc "SDL_GL_DeleteContext"              (ref void)
    const SDL_SCANCODE_UNKNOWN               = 0
    const SDL_SCANCODE_A                     = 4
    const SDL_SCANCODE_B                     = 5
    const SDL_SCANCODE_C                     = 6
    const SDL_SCANCODE_D                     = 7
    const SDL_SCANCODE_E                     = 8
    const SDL_SCANCODE_F                     = 9
    const SDL_SCANCODE_G                     = 10
    const SDL_SCANCODE_H                     = 11
    const SDL_SCANCODE_I                     = 12
    const SDL_SCANCODE_J                     = 13
    const SDL_SCANCODE_K                     = 14
    const SDL_SCANCODE_L                     = 15
    const SDL_SCANCODE_M                     = 16
    const SDL_SCANCODE_N                     = 17
    const SDL_SCANCODE_O                     = 18
    const SDL_SCANCODE_P                     = 19
    const SDL_SCANCODE_Q                     = 20
    const SDL_SCANCODE_R                     = 21
    const SDL_SCANCODE_S                     = 22
    const SDL_SCANCODE_T                     = 23
    const SDL_SCANCODE_U                     = 24
    const SDL_SCANCODE_V                     = 25
    const SDL_SCANCODE_W                     = 26
    const SDL_SCANCODE_X                     = 27
    const SDL_SCANCODE_Y                     = 28
    const SDL_SCANCODE_Z                     = 29
    const SDL_SCANCODE_1                     = 30
    const SDL_SCANCODE_2                     = 31
    const SDL_SCANCODE_3                     = 32
    const SDL_SCANCODE_4                     = 33
    const SDL_SCANCODE_5                     = 34
    const SDL_SCANCODE_6                     = 35
    const SDL_SCANCODE_7                     = 36
    const SDL_SCANCODE_8                     = 37
    const SDL_SCANCODE_9                     = 38
    const SDL_SCANCODE_0                     = 39
    const SDL_SCANCODE_RETURN                = 40
    const SDL_SCANCODE_ESCAPE                = 41
    const SDL_SCANCODE_BACKSPACE             = 42
    const SDL_SCANCODE_TAB                   = 43
    const SDL_SCANCODE_SPACE                 = 44
    const SDL_SCANCODE_MINUS                 = 45
    const SDL_SCANCODE_EQUALS                = 46
    const SDL_SCANCODE_LEFTBRACKET           = 47
    const SDL_SCANCODE_RIGHTBRACKET          = 48
    const SDL_SCANCODE_BACKSLASH             = 49
    const SDL_SCANCODE_NONUSHASH             = 50
    const SDL_SCANCODE_SEMICOLON             = 51
    const SDL_SCANCODE_APOSTROPHE            = 52
    const SDL_SCANCODE_GRAVE                 = 53
    const SDL_SCANCODE_COMMA                 = 54
    const SDL_SCANCODE_PERIOD                = 55
    const SDL_SCANCODE_SLASH                 = 56
    const SDL_SCANCODE_CAPSLOCK              = 57
    const SDL_SCANCODE_F1                    = 58
    const SDL_SCANCODE_F2                    = 59
    const SDL_SCANCODE_F3                    = 60
    const SDL_SCANCODE_F4                    = 61
    const SDL_SCANCODE_F5                    = 62
    const SDL_SCANCODE_F6                    = 63
    const SDL_SCANCODE_F7                    = 64
    const SDL_SCANCODE_F8                    = 65
    const SDL_SCANCODE_F9                    = 66
    const SDL_SCANCODE_F10                   = 67
    const SDL_SCANCODE_F11                   = 68
    const SDL_SCANCODE_F12                   = 69
    const SDL_SCANCODE_PRINTSCREEN           = 70
    const SDL_SCANCODE_SCROLLLOCK            = 71
    const SDL_SCANCODE_PAUSE                 = 72
    const SDL_SCANCODE_INSERT                = 73
    const SDL_SCANCODE_HOME                  = 74
    const SDL_SCANCODE_PAGEUP                = 75
    const SDL_SCANCODE_DELETE                = 76
    const SDL_SCANCODE_END                   = 77
    const SDL_SCANCODE_PAGEDOWN              = 78
    const SDL_SCANCODE_RIGHT                 = 79
    const SDL_SCANCODE_LEFT                  = 80
    const SDL_SCANCODE_DOWN                  = 81
    const SDL_SCANCODE_UP                    = 82
    const SDL_SCANCODE_NUMLOCKCLEAR          = 83
    const SDL_SCANCODE_KP_DIVIDE             = 84
    const SDL_SCANCODE_KP_MULTIPLY           = 85
    const SDL_SCANCODE_KP_MINUS              = 86
    const SDL_SCANCODE_KP_PLUS               = 87
    const SDL_SCANCODE_KP_ENTER              = 88
    const SDL_SCANCODE_KP_1                  = 89
    const SDL_SCANCODE_KP_2                  = 90
    const SDL_SCANCODE_KP_3                  = 91
    const SDL_SCANCODE_KP_4                  = 92
    const SDL_SCANCODE_KP_5                  = 93
    const SDL_SCANCODE_KP_6                  = 94
    const SDL_SCANCODE_KP_7                  = 95
    const SDL_SCANCODE_KP_8                  = 96
    const SDL_SCANCODE_KP_9                  = 97
    const SDL_SCANCODE_KP_0                  = 98
    const SDL_SCANCODE_KP_PERIOD             = 99
    const SDL_SCANCODE_NONUSBACKSLASH        = 100
    const SDL_SCANCODE_APPLICATION           = 101
    const SDL_SCANCODE_POWER                 = 102
    const SDL_SCANCODE_KP_EQUALS             = 103
    const SDL_SCANCODE_F13                   = 104
    const SDL_SCANCODE_F14                   = 105
    const SDL_SCANCODE_F15                   = 106
    const SDL_SCANCODE_F16                   = 107
    const SDL_SCANCODE_F17                   = 108
    const SDL_SCANCODE_F18                   = 109
    const SDL_SCANCODE_F19                   = 110
    const SDL_SCANCODE_F20                   = 111
    const SDL_SCANCODE_F21                   = 112
    const SDL_SCANCODE_F22                   = 113
    const SDL_SCANCODE_F23                   = 114
    const SDL_SCANCODE_F24                   = 115
    const SDL_SCANCODE_EXECUTE               = 116
    const SDL_SCANCODE_HELP                  = 117
    const SDL_SCANCODE_MENU                  = 118
    const SDL_SCANCODE_SELECT                = 119
    const SDL_SCANCODE_STOP                  = 120
    const SDL_SCANCODE_AGAIN                 = 121
    const SDL_SCANCODE_UNDO                  = 122
    const SDL_SCANCODE_CUT                   = 123
    const SDL_SCANCODE_COPY                  = 124
    const SDL_SCANCODE_PASTE                 = 125
    const SDL_SCANCODE_FIND                  = 126
    const SDL_SCANCODE_MUTE                  = 127
    const SDL_SCANCODE_VOLUMEUP              = 128
    const SDL_SCANCODE_VOLUMEDOWN            = 129
    const SDL_SCANCODE_KP_COMMA              = 133
    const SDL_SCANCODE_KP_EQUALSAS400        = 134
    const SDL_SCANCODE_INTERNATIONAL1        = 135
    const SDL_SCANCODE_INTERNATIONAL2        = 136
    const SDL_SCANCODE_INTERNATIONAL3        = 137
    const SDL_SCANCODE_INTERNATIONAL4        = 138
    const SDL_SCANCODE_INTERNATIONAL5        = 139
    const SDL_SCANCODE_INTERNATIONAL6        = 140
    const SDL_SCANCODE_INTERNATIONAL7        = 141
    const SDL_SCANCODE_INTERNATIONAL8        = 142
    const SDL_SCANCODE_INTERNATIONAL9        = 143
    const SDL_SCANCODE_LANG1                 = 144
    const SDL_SCANCODE_LANG2                 = 145
    const SDL_SCANCODE_LANG3                 = 146
    const SDL_SCANCODE_LANG4                 = 147
    const SDL_SCANCODE_LANG5                 = 148
    const SDL_SCANCODE_LANG6                 = 149
    const SDL_SCANCODE_LANG7                 = 150
    const SDL_SCANCODE_LANG8                 = 151
    const SDL_SCANCODE_LANG9                 = 152
    const SDL_SCANCODE_ALTERASE              = 153
    const SDL_SCANCODE_SYSREQ                = 154
    const SDL_SCANCODE_CANCEL                = 155
    const SDL_SCANCODE_CLEAR                 = 156
    const SDL_SCANCODE_PRIOR                 = 157
    const SDL_SCANCODE_RETURN2               = 158
    const SDL_SCANCODE_SEPARATOR             = 159
    const SDL_SCANCODE_OUT                   = 160
    const SDL_SCANCODE_OPER                  = 161
    const SDL_SCANCODE_CLEARAGAIN            = 162
    const SDL_SCANCODE_CRSEL                 = 163
    const SDL_SCANCODE_EXSEL                 = 164
    const SDL_SCANCODE_KP_00                 = 176
    const SDL_SCANCODE_KP_000                = 177
    const SDL_SCANCODE_THOUSANDSSEPARATOR    = 178
    const SDL_SCANCODE_DECIMALSEPARATOR      = 179
    const SDL_SCANCODE_CURRENCYUNIT          = 180
    const SDL_SCANCODE_CURRENCYSUBUNIT       = 181
    const SDL_SCANCODE_KP_LEFTPAREN          = 182
    const SDL_SCANCODE_KP_RIGHTPAREN         = 183
    const SDL_SCANCODE_KP_LEFTBRACE          = 184
    const SDL_SCANCODE_KP_RIGHTBRACE         = 185
    const SDL_SCANCODE_KP_TAB                = 186
    const SDL_SCANCODE_KP_BACKSPACE          = 187
    const SDL_SCANCODE_KP_A                  = 188
    const SDL_SCANCODE_KP_B                  = 189
    const SDL_SCANCODE_KP_C                  = 190
    const SDL_SCANCODE_KP_D                  = 191
    const SDL_SCANCODE_KP_E                  = 192
    const SDL_SCANCODE_KP_F                  = 193
    const SDL_SCANCODE_KP_XOR                = 194
    const SDL_SCANCODE_KP_POWER              = 195
    const SDL_SCANCODE_KP_PERCENT            = 196
    const SDL_SCANCODE_KP_LESS               = 197
    const SDL_SCANCODE_KP_GREATER            = 198
    const SDL_SCANCODE_KP_AMPERSAND          = 199
    const SDL_SCANCODE_KP_DBLAMPERSAND       = 200
    const SDL_SCANCODE_KP_VERTICALBAR        = 201
    const SDL_SCANCODE_KP_DBLVERTICALBAR     = 202
    const SDL_SCANCODE_KP_COLON              = 203
    const SDL_SCANCODE_KP_HASH               = 204
    const SDL_SCANCODE_KP_SPACE              = 205
    const SDL_SCANCODE_KP_AT                 = 206
    const SDL_SCANCODE_KP_EXCLAM             = 207
    const SDL_SCANCODE_KP_MEMSTORE           = 208
    const SDL_SCANCODE_KP_MEMRECALL          = 209
    const SDL_SCANCODE_KP_MEMCLEAR           = 210
    const SDL_SCANCODE_KP_MEMADD             = 211
    const SDL_SCANCODE_KP_MEMSUBTRACT        = 212
    const SDL_SCANCODE_KP_MEMMULTIPLY        = 213
    const SDL_SCANCODE_KP_MEMDIVIDE          = 214
    const SDL_SCANCODE_KP_PLUSMINUS          = 215
    const SDL_SCANCODE_KP_CLEAR              = 216
    const SDL_SCANCODE_KP_CLEARENTRY         = 217
    const SDL_SCANCODE_KP_BINARY             = 218
    const SDL_SCANCODE_KP_OCTAL              = 219
    const SDL_SCANCODE_KP_DECIMAL            = 220
    const SDL_SCANCODE_KP_HEXADECIMAL        = 221
    const SDL_SCANCODE_LCTRL                 = 224
    const SDL_SCANCODE_LSHIFT                = 225
    const SDL_SCANCODE_LALT                  = 226
    const SDL_SCANCODE_LGUI                  = 227
    const SDL_SCANCODE_RCTRL                 = 228
    const SDL_SCANCODE_RSHIFT                = 229
    const SDL_SCANCODE_RALT                  = 230
    const SDL_SCANCODE_RGUI                  = 231
    const SDL_SCANCODE_MODE                  = 257
    const SDL_SCANCODE_AUDIONEXT             = 258
    const SDL_SCANCODE_AUDIOPREV             = 259
    const SDL_SCANCODE_AUDIOSTOP             = 260
    const SDL_SCANCODE_AUDIOPLAY             = 261
    const SDL_SCANCODE_AUDIOMUTE             = 262
    const SDL_SCANCODE_MEDIASELECT           = 263
    const SDL_SCANCODE_WWW                   = 264
    const SDL_SCANCODE_MAIL                  = 265
    const SDL_SCANCODE_CALCULATOR            = 266
    const SDL_SCANCODE_COMPUTER              = 267
    const SDL_SCANCODE_AC_SEARCH             = 268
    const SDL_SCANCODE_AC_HOME               = 269
    const SDL_SCANCODE_AC_BACK               = 270
    const SDL_SCANCODE_AC_FORWARD            = 271
    const SDL_SCANCODE_AC_STOP               = 272
    const SDL_SCANCODE_AC_REFRESH            = 273
    const SDL_SCANCODE_AC_BOOKMARKS          = 274
    const SDL_SCANCODE_BRIGHTNESSDOWN        = 275
    const SDL_SCANCODE_BRIGHTNESSUP          = 276
    const SDL_SCANCODE_DISPLAYSWITCH         = 277
    const SDL_SCANCODE_KBDILLUMTOGGLE        = 278
    const SDL_SCANCODE_KBDILLUMDOWN          = 279
    const SDL_SCANCODE_KBDILLUMUP            = 280
    const SDL_SCANCODE_EJECT                 = 281
    const SDL_SCANCODE_SLEEP                 = 282
    const SDL_SCANCODE_APP1                  = 283
    const SDL_SCANCODE_APP2                  = 284
    const SDL_SCANCODE_AUDIOREWIND           = 285
    const SDL_SCANCODE_AUDIOFASTFORWARD      = 286
    const SDL_SCANCODE_SOFTLEFT              = 287
    const SDL_SCANCODE_SOFTRIGHT             = 288
    const SDL_SCANCODE_CALL                  = 289
    const SDL_SCANCODE_ENDCALL               = 290
    const SDL_NUM_SCANCODES                  = 512
    const SDLK_UNKNOWN                       = 0
    const SDLK_RETURN                        = 13
    const SDLK_ESCAPE                        = 27
    const SDLK_BACKSPACE                     = 8
    const SDLK_TAB                           = 9
    const SDLK_SPACE                         = 32
    const SDLK_EXCLAIM                       = 33
    const SDLK_QUOTEDBL                      = 34
    const SDLK_HASH                          = 35
    const SDLK_PERCENT                       = 37
    const SDLK_DOLLAR                        = 36
    const SDLK_AMPERSAND                     = 38
    const SDLK_QUOTE                         = 39
    const SDLK_LEFTPAREN                     = 40
    const SDLK_RIGHTPAREN                    = 41
    const SDLK_ASTERISK                      = 42
    const SDLK_PLUS                          = 43
    const SDLK_COMMA                         = 44
    const SDLK_MINUS                         = 45
    const SDLK_PERIOD                        = 46
    const SDLK_SLASH                         = 47
    const SDLK_0                             = 48
    const SDLK_1                             = 49
    const SDLK_2                             = 50
    const SDLK_3                             = 51
    const SDLK_4                             = 52
    const SDLK_5                             = 53
    const SDLK_6                             = 54
    const SDLK_7                             = 55
    const SDLK_8                             = 56
    const SDLK_9                             = 57
    const SDLK_COLON                         = 58
    const SDLK_SEMICOLON                     = 59
    const SDLK_LESS                          = 60
    const SDLK_EQUALS                        = 61
    const SDLK_GREATER                       = 62
    const SDLK_QUESTION                      = 63
    const SDLK_AT                            = 64
    const SDLK_LEFTBRACKET                   = 91
    const SDLK_BACKSLASH                     = 92
    const SDLK_RIGHTBRACKET                  = 93
    const SDLK_CARET                         = 94
    const SDLK_UNDERSCORE                    = 95
    const SDLK_BACKQUOTE                     = 96
    const SDLK_a                             = 97
    const SDLK_b                             = 98
    const SDLK_c                             = 99
    const SDLK_d                             = 100
    const SDLK_e                             = 101
    const SDLK_f                             = 102
    const SDLK_g                             = 103
    const SDLK_h                             = 104
    const SDLK_i                             = 105
    const SDLK_j                             = 106
    const SDLK_k                             = 107
    const SDLK_l                             = 108
    const SDLK_m                             = 109
    const SDLK_n                             = 110
    const SDLK_o                             = 111
    const SDLK_p                             = 112
    const SDLK_q                             = 113
    const SDLK_r                             = 114
    const SDLK_s                             = 115
    const SDLK_t                             = 116
    const SDLK_u                             = 117
    const SDLK_v                             = 118
    const SDLK_w                             = 119
    const SDLK_x                             = 120
    const SDLK_y                             = 121
    const SDLK_z                             = 122
    const SDLK_CAPSLOCK                      = 1073741881
    const SDLK_F1                            = 1073741882
    const SDLK_F2                            = 1073741883
    const SDLK_F3                            = 1073741884
    const SDLK_F4                            = 1073741885
    const SDLK_F5                            = 1073741886
    const SDLK_F6                            = 1073741887
    const SDLK_F7                            = 1073741888
    const SDLK_F8                            = 1073741889
    const SDLK_F9                            = 1073741890
    const SDLK_F10                           = 1073741891
    const SDLK_F11                           = 1073741892
    const SDLK_F12                           = 1073741893
    const SDLK_PRINTSCREEN                   = 1073741894
    const SDLK_SCROLLLOCK                    = 1073741895
    const SDLK_PAUSE                         = 1073741896
    const SDLK_INSERT                        = 1073741897
    const SDLK_HOME                          = 1073741898
    const SDLK_PAGEUP                        = 1073741899
    const SDLK_DELETE                        = 127
    const SDLK_END                           = 1073741901
    const SDLK_PAGEDOWN                      = 1073741902
    const SDLK_RIGHT                         = 1073741903
    const SDLK_LEFT                          = 1073741904
    const SDLK_DOWN                          = 1073741905
    const SDLK_UP                            = 1073741906
    const SDLK_NUMLOCKCLEAR                  = 1073741907
    const SDLK_KP_DIVIDE                     = 1073741908
    const SDLK_KP_MULTIPLY                   = 1073741909
    const SDLK_KP_MINUS                      = 1073741910
    const SDLK_KP_PLUS                       = 1073741911
    const SDLK_KP_ENTER                      = 1073741912
    const SDLK_KP_1                          = 1073741913
    const SDLK_KP_2                          = 1073741914
    const SDLK_KP_3                          = 1073741915
    const SDLK_KP_4                          = 1073741916
    const SDLK_KP_5                          = 1073741917
    const SDLK_KP_6                          = 1073741918
    const SDLK_KP_7                          = 1073741919
    const SDLK_KP_8                          = 1073741920
    const SDLK_KP_9                          = 1073741921
    const SDLK_KP_0                          = 1073741922
    const SDLK_KP_PERIOD                     = 1073741923
    const SDLK_APPLICATION                   = 1073741925
    const SDLK_POWER                         = 1073741926
    const SDLK_KP_EQUALS                     = 1073741927
    const SDLK_F13                           = 1073741928
    const SDLK_F14                           = 1073741929
    const SDLK_F15                           = 1073741930
    const SDLK_F16                           = 1073741931
    const SDLK_F17                           = 1073741932
    const SDLK_F18                           = 1073741933
    const SDLK_F19                           = 1073741934
    const SDLK_F20                           = 1073741935
    const SDLK_F21                           = 1073741936
    const SDLK_F22                           = 1073741937
    const SDLK_F23                           = 1073741938
    const SDLK_F24                           = 1073741939
    const SDLK_EXECUTE                       = 1073741940
    const SDLK_HELP                          = 1073741941
    const SDLK_MENU                          = 1073741942
    const SDLK_SELECT                        = 1073741943
    const SDLK_STOP                          = 1073741944
    const SDLK_AGAIN                         = 1073741945
    const SDLK_UNDO                          = 1073741946
    const SDLK_CUT                           = 1073741947
    const SDLK_COPY                          = 1073741948
    const SDLK_PASTE                         = 1073741949
    const SDLK_FIND                          = 1073741950
    const SDLK_MUTE                          = 1073741951
    const SDLK_VOLUMEUP                      = 1073741952
    const SDLK_VOLUMEDOWN                    = 1073741953
    const SDLK_KP_COMMA                      = 1073741957
    const SDLK_KP_EQUALSAS400                = 1073741958
    const SDLK_ALTERASE                      = 1073741977
    const SDLK_SYSREQ                        = 1073741978
    const SDLK_CANCEL                        = 1073741979
    const SDLK_CLEAR                         = 1073741980
    const SDLK_PRIOR                         = 1073741981
    const SDLK_RETURN2                       = 1073741982
    const SDLK_SEPARATOR                     = 1073741983
    const SDLK_OUT                           = 1073741984
    const SDLK_OPER                          = 1073741985
    const SDLK_CLEARAGAIN                    = 1073741986
    const SDLK_CRSEL                         = 1073741987
    const SDLK_EXSEL                         = 1073741988
    const SDLK_KP_00                         = 1073742000
    const SDLK_KP_000                        = 1073742001
    const SDLK_THOUSANDSSEPARATOR            = 1073742002
    const SDLK_DECIMALSEPARATOR              = 1073742003
    const SDLK_CURRENCYUNIT                  = 1073742004
    const SDLK_CURRENCYSUBUNIT               = 1073742005
    const SDLK_KP_LEFTPAREN                  = 1073742006
    const SDLK_KP_RIGHTPAREN                 = 1073742007
    const SDLK_KP_LEFTBRACE                  = 1073742008
    const SDLK_KP_RIGHTBRACE                 = 1073742009
    const SDLK_KP_TAB                        = 1073742010
    const SDLK_KP_BACKSPACE                  = 1073742011
    const SDLK_KP_A                          = 1073742012
    const SDLK_KP_B                          = 1073742013
    const SDLK_KP_C                          = 1073742014
    const SDLK_KP_D                          = 1073742015
    const SDLK_KP_E                          = 1073742016
    const SDLK_KP_F                          = 1073742017
    const SDLK_KP_XOR                        = 1073742018
    const SDLK_KP_POWER                      = 1073742019
    const SDLK_KP_PERCENT                    = 1073742020
    const SDLK_KP_LESS                       = 1073742021
    const SDLK_KP_GREATER                    = 1073742022
    const SDLK_KP_AMPERSAND                  = 1073742023
    const SDLK_KP_DBLAMPERSAND               = 1073742024
    const SDLK_KP_VERTICALBAR                = 1073742025
    const SDLK_KP_DBLVERTICALBAR             = 1073742026
    const SDLK_KP_COLON                      = 1073742027
    const SDLK_KP_HASH                       = 1073742028
    const SDLK_KP_SPACE                      = 1073742029
    const SDLK_KP_AT                         = 1073742030
    const SDLK_KP_EXCLAM                     = 1073742031
    const SDLK_KP_MEMSTORE                   = 1073742032
    const SDLK_KP_MEMRECALL                  = 1073742033
    const SDLK_KP_MEMCLEAR                   = 1073742034
    const SDLK_KP_MEMADD                     = 1073742035
    const SDLK_KP_MEMSUBTRACT                = 1073742036
    const SDLK_KP_MEMMULTIPLY                = 1073742037
    const SDLK_KP_MEMDIVIDE                  = 1073742038
    const SDLK_KP_PLUSMINUS                  = 1073742039
    const SDLK_KP_CLEAR                      = 1073742040
    const SDLK_KP_CLEARENTRY                 = 1073742041
    const SDLK_KP_BINARY                     = 1073742042
    const SDLK_KP_OCTAL                      = 1073742043
    const SDLK_KP_DECIMAL                    = 1073742044
    const SDLK_KP_HEXADECIMAL                = 1073742045
    const SDLK_LCTRL                         = 1073742048
    const SDLK_LSHIFT                        = 1073742049
    const SDLK_LALT                          = 1073742050
    const SDLK_LGUI                          = 1073742051
    const SDLK_RCTRL                         = 1073742052
    const SDLK_RSHIFT                        = 1073742053
    const SDLK_RALT                          = 1073742054
    const SDLK_RGUI                          = 1073742055
    const SDLK_MODE                          = 1073742081
    const SDLK_AUDIONEXT                     = 1073742082
    const SDLK_AUDIOPREV                     = 1073742083
    const SDLK_AUDIOSTOP                     = 1073742084
    const SDLK_AUDIOPLAY                     = 1073742085
    const SDLK_AUDIOMUTE                     = 1073742086
    const SDLK_MEDIASELECT                   = 1073742087
    const SDLK_WWW                           = 1073742088
    const SDLK_MAIL                          = 1073742089
    const SDLK_CALCULATOR                    = 1073742090
    const SDLK_COMPUTER                      = 1073742091
    const SDLK_AC_SEARCH                     = 1073742092
    const SDLK_AC_HOME                       = 1073742093
    const SDLK_AC_BACK                       = 1073742094
    const SDLK_AC_FORWARD                    = 1073742095
    const SDLK_AC_STOP                       = 1073742096
    const SDLK_AC_REFRESH                    = 1073742097
    const SDLK_AC_BOOKMARKS                  = 1073742098
    const SDLK_BRIGHTNESSDOWN                = 1073742099
    const SDLK_BRIGHTNESSUP                  = 1073742100
    const SDLK_DISPLAYSWITCH                 = 1073742101
    const SDLK_KBDILLUMTOGGLE                = 1073742102
    const SDLK_KBDILLUMDOWN                  = 1073742103
    const SDLK_KBDILLUMUP                    = 1073742104
    const SDLK_EJECT                         = 1073742105
    const SDLK_SLEEP                         = 1073742106
    const SDLK_APP1                          = 1073742107
    const SDLK_APP2                          = 1073742108
    const SDLK_AUDIOREWIND                   = 1073742109
    const SDLK_AUDIOFASTFORWARD              = 1073742110
    const SDLK_SOFTLEFT                      = 1073742111
    const SDLK_SOFTRIGHT                     = 1073742112
    const SDLK_CALL                          = 1073742113
    const SDLK_ENDCALL                       = 1073742114
    const KMOD_NONE                          = 0
    const KMOD_LSHIFT                        = 1
    const KMOD_RSHIFT                        = 2
    const KMOD_LCTRL                         = 64
    const KMOD_RCTRL                         = 128
    const KMOD_LALT                          = 256
    const KMOD_RALT                          = 512
    const KMOD_LGUI                          = 1024
    const KMOD_RGUI                          = 2048
    const KMOD_NUM                           = 4096
    const KMOD_CAPS                          = 8192
    const KMOD_MODE                          = 16384
    const KMOD_SCROLL                        = 32768
    const KMOD_CTRL                          = 192
    const KMOD_SHIFT                         = 3
    const KMOD_ALT                           = 768
    const KMOD_GUI                           = 3072
    const KMOD_RESERVED                      = 32768
    type SDL_Keysym = struct $caligned
        i32 scancode
        i32 sym
        u16 mod
        u32 unused
    end

    func "SDL_GetKeyboardFocus"              ()ref SDL_Window
    func "SDL_GetKeyboardState"              (ref i32)ref u8
    proc "SDL_ResetKeyboard"                 ()
    func "SDL_GetModState"                   ()i32
    proc "SDL_SetModState"                   (i32)
    func "SDL_GetKeyFromScancode"            (i32)i32
    func "SDL_GetScancodeFromKey"            (i32)i32
    func "SDL_GetScancodeName"               (i32)ref i8
    func "SDL_GetScancodeFromName"           (ref i8)i32
    func "SDL_GetKeyName"                    (i32)ref i8
    func "SDL_GetKeyFromName"                (ref i8)i32
    proc "SDL_StartTextInput"                ()
    func "SDL_IsTextInputActive"             ()i32
    proc "SDL_StopTextInput"                 ()
    proc "SDL_ClearComposition"              ()
    func "SDL_IsTextInputShown"              ()i32
    proc "SDL_SetTextInputRect"              (ref SDL_Rect)
    func "SDL_HasScreenKeyboardSupport"      ()i32
    func "SDL_IsScreenKeyboardShown"         (ref SDL_Window)i32
    type SDL_Cursor = struct $caligned
        int dummy    !empty record
    end

    const SDL_SYSTEM_CURSOR_ARROW            = 0
    const SDL_SYSTEM_CURSOR_IBEAM            = 1
    const SDL_SYSTEM_CURSOR_WAIT             = 2
    const SDL_SYSTEM_CURSOR_CROSSHAIR        = 3
    const SDL_SYSTEM_CURSOR_WAITARROW        = 4
    const SDL_SYSTEM_CURSOR_SIZENWSE         = 5
    const SDL_SYSTEM_CURSOR_SIZENESW         = 6
    const SDL_SYSTEM_CURSOR_SIZEWE           = 7
    const SDL_SYSTEM_CURSOR_SIZENS           = 8
    const SDL_SYSTEM_CURSOR_SIZEALL          = 9
    const SDL_SYSTEM_CURSOR_NO               = 10
    const SDL_SYSTEM_CURSOR_HAND             = 11
    const SDL_NUM_SYSTEM_CURSORS             = 12
    const SDL_MOUSEWHEEL_NORMAL              = 0
    const SDL_MOUSEWHEEL_FLIPPED             = 1
    func "SDL_GetMouseFocus"                 ()ref SDL_Window
    func "SDL_GetMouseState"                 (ref i32,ref i32)u32
    func "SDL_GetGlobalMouseState"           (ref i32,ref i32)u32
    func "SDL_GetRelativeMouseState"         (ref i32,ref i32)u32
    proc "SDL_WarpMouseInWindow"             (ref SDL_Window,i32,i32)
    func "SDL_WarpMouseGlobal"               (i32,i32)i32
    func "SDL_SetRelativeMouseMode"          (i32)i32
    func "SDL_CaptureMouse"                  (i32)i32
    func "SDL_GetRelativeMouseMode"          ()i32
    func "SDL_CreateCursor"                  (ref u8,ref u8,i32,i32,i32,i32)ref SDL_Cursor
    func "SDL_CreateColorCursor"             (ref SDL_Surface,i32,i32)ref SDL_Cursor
    func "SDL_CreateSystemCursor"            (i32)ref SDL_Cursor
    proc "SDL_SetCursor"                     (ref SDL_Cursor)
    func "SDL_GetCursor"                     ()ref SDL_Cursor
    func "SDL_GetDefaultCursor"              ()ref SDL_Cursor
    proc "SDL_FreeCursor"                    (ref SDL_Cursor)
    func "SDL_ShowCursor"                    (i32)i32
    type $T9 = struct $caligned
        [16]u8 data
    end

    proc "SDL_GUIDToString"                  ($T9,ref i8,i32)
    func "SDL_GUIDFromString"                ($T9,ref i8)$T9
    type _SDL_Joystick = struct $caligned
        int dummy    !empty record
    end

    const SDL_JOYSTICK_TYPE_UNKNOWN          = 0
    const SDL_JOYSTICK_TYPE_GAMECONTROLLER   = 1
    const SDL_JOYSTICK_TYPE_WHEEL            = 2
    const SDL_JOYSTICK_TYPE_ARCADE_STICK     = 3
    const SDL_JOYSTICK_TYPE_FLIGHT_STICK     = 4
    const SDL_JOYSTICK_TYPE_DANCE_PAD        = 5
    const SDL_JOYSTICK_TYPE_GUITAR           = 6
    const SDL_JOYSTICK_TYPE_DRUM_KIT         = 7
    const SDL_JOYSTICK_TYPE_ARCADE_PAD       = 8
    const SDL_JOYSTICK_TYPE_THROTTLE         = 9
    const SDL_JOYSTICK_POWER_UNKNOWN         = -1
    const SDL_JOYSTICK_POWER_EMPTY           = 0
    const SDL_JOYSTICK_POWER_LOW             = 1
    const SDL_JOYSTICK_POWER_MEDIUM          = 2
    const SDL_JOYSTICK_POWER_FULL            = 3
    const SDL_JOYSTICK_POWER_WIRED           = 4
    const SDL_JOYSTICK_POWER_MAX             = 5
    proc "SDL_LockJoysticks"                 ()
    proc "SDL_UnlockJoysticks"               ()
    func "SDL_NumJoysticks"                  ()i32
    func "SDL_JoystickNameForIndex"          (i32)ref i8
    func "SDL_JoystickPathForIndex"          (i32)ref i8
    func "SDL_JoystickGetDevicePlayerIndex"  (i32)i32
    func "SDL_JoystickGetDeviceGUID"         ($T9,i32)$T9
    func "SDL_JoystickGetDeviceVendor"       (i32)u16
    func "SDL_JoystickGetDeviceProduct"      (i32)u16
    func "SDL_JoystickGetDeviceProductVersion" (i32)u16
    func "SDL_JoystickGetDeviceType"         (i32)i32
    func "SDL_JoystickGetDeviceInstanceID"   (i32)i32
    func "SDL_JoystickOpen"                  (i32)ref _SDL_Joystick
    func "SDL_JoystickFromInstanceID"        (i32)ref _SDL_Joystick
    func "SDL_JoystickFromPlayerIndex"       (i32)ref _SDL_Joystick
    func "SDL_JoystickAttachVirtual"         (i32,i32,i32,i32)i32
    type SDL_VirtualJoystickDesc = struct $caligned
        u16 version
        u16 type$
        u16 naxes
        u16 nbuttons
        u16 nhats
        u16 vendor_id
        u16 product_id
        u16 padding
        u32 button_mask
        u32 axis_mask
        ref i8 name
        ref void userdata
        ref void Update
        ref void SetPlayerIndex
        ref void Rumble
        ref void RumbleTriggers
        ref void SetLED
        ref void SendEffect
    end

    func "SDL_JoystickAttachVirtualEx"       (ref SDL_VirtualJoystickDesc)i32
    func "SDL_JoystickDetachVirtual"         (i32)i32
    func "SDL_JoystickIsVirtual"             (i32)i32
    func "SDL_JoystickSetVirtualAxis"        (ref _SDL_Joystick,i32,i16)i32
    func "SDL_JoystickSetVirtualButton"      (ref _SDL_Joystick,i32,u8)i32
    func "SDL_JoystickSetVirtualHat"         (ref _SDL_Joystick,i32,u8)i32
    func "SDL_JoystickName"                  (ref _SDL_Joystick)ref i8
    func "SDL_JoystickPath"                  (ref _SDL_Joystick)ref i8
    func "SDL_JoystickGetPlayerIndex"        (ref _SDL_Joystick)i32
    proc "SDL_JoystickSetPlayerIndex"        (ref _SDL_Joystick,i32)
    func "SDL_JoystickGetGUID"               ($T9,ref _SDL_Joystick)$T9
    func "SDL_JoystickGetVendor"             (ref _SDL_Joystick)u16
    func "SDL_JoystickGetProduct"            (ref _SDL_Joystick)u16
    func "SDL_JoystickGetProductVersion"     (ref _SDL_Joystick)u16
    func "SDL_JoystickGetFirmwareVersion"    (ref _SDL_Joystick)u16
    func "SDL_JoystickGetSerial"             (ref _SDL_Joystick)ref i8
    func "SDL_JoystickGetType"               (ref _SDL_Joystick)i32
    proc "SDL_JoystickGetGUIDString"         ($T9,ref i8,i32)
    func "SDL_JoystickGetGUIDFromString"     ($T9,ref i8)$T9
    proc "SDL_GetJoystickGUIDInfo"           ($T9,ref u16,ref u16,ref u16,ref u16)
    func "SDL_JoystickGetAttached"           (ref _SDL_Joystick)i32
    func "SDL_JoystickInstanceID"            (ref _SDL_Joystick)i32
    func "SDL_JoystickNumAxes"               (ref _SDL_Joystick)i32
    func "SDL_JoystickNumBalls"              (ref _SDL_Joystick)i32
    func "SDL_JoystickNumHats"               (ref _SDL_Joystick)i32
    func "SDL_JoystickNumButtons"            (ref _SDL_Joystick)i32
    proc "SDL_JoystickUpdate"                ()
    func "SDL_JoystickEventState"            (i32)i32
    func "SDL_JoystickGetAxis"               (ref _SDL_Joystick,i32)i16
    func "SDL_JoystickGetAxisInitialState"   (ref _SDL_Joystick,i32,ref i16)i32
    func "SDL_JoystickGetHat"                (ref _SDL_Joystick,i32)u8
    func "SDL_JoystickGetBall"               (ref _SDL_Joystick,i32,ref i32,ref i32)i32
    func "SDL_JoystickGetButton"             (ref _SDL_Joystick,i32)u8
    func "SDL_JoystickRumble"                (ref _SDL_Joystick,u16,u16,u32)i32
    func "SDL_JoystickRumbleTriggers"        (ref _SDL_Joystick,u16,u16,u32)i32
    func "SDL_JoystickHasLED"                (ref _SDL_Joystick)i32
    func "SDL_JoystickHasRumble"             (ref _SDL_Joystick)i32
    func "SDL_JoystickHasRumbleTriggers"     (ref _SDL_Joystick)i32
    func "SDL_JoystickSetLED"                (ref _SDL_Joystick,u8,u8,u8)i32
    func "SDL_JoystickSendEffect"            (ref _SDL_Joystick,ref void,i32)i32
    proc "SDL_JoystickClose"                 (ref _SDL_Joystick)
    func "SDL_JoystickCurrentPowerLevel"     (ref _SDL_Joystick)i32
    type _SDL_Sensor = struct $caligned
        int dummy    !empty record
    end

    const SDL_SENSOR_INVALID                 = -1
    const SDL_SENSOR_UNKNOWN                 = 0
    const SDL_SENSOR_ACCEL                   = 1
    const SDL_SENSOR_GYRO                    = 2
    const SDL_SENSOR_ACCEL_L                 = 3
    const SDL_SENSOR_GYRO_L                  = 4
    const SDL_SENSOR_ACCEL_R                 = 5
    const SDL_SENSOR_GYRO_R                  = 6
    proc "SDL_LockSensors"                   ()
    proc "SDL_UnlockSensors"                 ()
    func "SDL_NumSensors"                    ()i32
    func "SDL_SensorGetDeviceName"           (i32)ref i8
    func "SDL_SensorGetDeviceType"           (i32)i32
    func "SDL_SensorGetDeviceNonPortableType" (i32)i32
    func "SDL_SensorGetDeviceInstanceID"     (i32)i32
    func "SDL_SensorOpen"                    (i32)ref _SDL_Sensor
    func "SDL_SensorFromInstanceID"          (i32)ref _SDL_Sensor
    func "SDL_SensorGetName"                 (ref _SDL_Sensor)ref i8
    func "SDL_SensorGetType"                 (ref _SDL_Sensor)i32
    func "SDL_SensorGetNonPortableType"      (ref _SDL_Sensor)i32
    func "SDL_SensorGetInstanceID"           (ref _SDL_Sensor)i32
    func "SDL_SensorGetData"                 (ref _SDL_Sensor,ref r32,i32)i32
    func "SDL_SensorGetDataWithTimestamp"    (ref _SDL_Sensor,ref u64,ref r32,i32)i32
    proc "SDL_SensorClose"                   (ref _SDL_Sensor)
    proc "SDL_SensorUpdate"                  ()
    type _SDL_GameController = struct $caligned
        int dummy    !empty record
    end

    const SDL_CONTROLLER_TYPE_UNKNOWN        = 0
    const SDL_CONTROLLER_TYPE_XBOX360        = 1
    const SDL_CONTROLLER_TYPE_XBOXONE        = 2
    const SDL_CONTROLLER_TYPE_PS3            = 3
    const SDL_CONTROLLER_TYPE_PS4            = 4
    const SDL_CONTROLLER_TYPE_NINTENDO_SWITCH_PRO = 5
    const SDL_CONTROLLER_TYPE_VIRTUAL        = 6
    const SDL_CONTROLLER_TYPE_PS5            = 7
    const SDL_CONTROLLER_TYPE_AMAZON_LUNA    = 8
    const SDL_CONTROLLER_TYPE_GOOGLE_STADIA  = 9
    const SDL_CONTROLLER_TYPE_NVIDIA_SHIELD  = 10
    const SDL_CONTROLLER_TYPE_NINTENDO_SWITCH_JOYCON_LEFT = 11
    const SDL_CONTROLLER_TYPE_NINTENDO_SWITCH_JOYCON_RIGHT = 12
    const SDL_CONTROLLER_TYPE_NINTENDO_SWITCH_JOYCON_PAIR = 13
    const SDL_CONTROLLER_BINDTYPE_NONE       = 0
    const SDL_CONTROLLER_BINDTYPE_BUTTON     = 1
    const SDL_CONTROLLER_BINDTYPE_AXIS       = 2
    const SDL_CONTROLLER_BINDTYPE_HAT        = 3
    type SDL_GameControllerButtonBind = struct $caligned
        i32 bindType
        $T10 value
    end

    type $T10 = struct $caligned
        i32 button
        i32 axis
        $T11 hat
    end

    type $T11 = struct $caligned
        i32 hat
        i32 hat_mask
    end

    func "SDL_GameControllerAddMappingsFromRW" (ref SDL_RWops,i32)i32
    func "SDL_GameControllerAddMapping"      (ref i8)i32
    func "SDL_GameControllerNumMappings"     ()i32
    func "SDL_GameControllerMappingForIndex" (i32)ref i8
    func "SDL_GameControllerMappingForGUID"  ($T9)ref i8
    func "SDL_GameControllerMapping"         (ref _SDL_GameController)ref i8
    func "SDL_IsGameController"              (i32)i32
    func "SDL_GameControllerNameForIndex"    (i32)ref i8
    func "SDL_GameControllerPathForIndex"    (i32)ref i8
    func "SDL_GameControllerTypeForIndex"    (i32)i32
    func "SDL_GameControllerMappingForDeviceIndex" (i32)ref i8
    func "SDL_GameControllerOpen"            (i32)ref _SDL_GameController
    func "SDL_GameControllerFromInstanceID"  (i32)ref _SDL_GameController
    func "SDL_GameControllerFromPlayerIndex" (i32)ref _SDL_GameController
    func "SDL_GameControllerName"            (ref _SDL_GameController)ref i8
    func "SDL_GameControllerPath"            (ref _SDL_GameController)ref i8
    func "SDL_GameControllerGetType"         (ref _SDL_GameController)i32
    func "SDL_GameControllerGetPlayerIndex"  (ref _SDL_GameController)i32
    proc "SDL_GameControllerSetPlayerIndex"  (ref _SDL_GameController,i32)
    func "SDL_GameControllerGetVendor"       (ref _SDL_GameController)u16
    func "SDL_GameControllerGetProduct"      (ref _SDL_GameController)u16
    func "SDL_GameControllerGetProductVersion" (ref _SDL_GameController)u16
    func "SDL_GameControllerGetFirmwareVersion" (ref _SDL_GameController)u16
    func "SDL_GameControllerGetSerial"       (ref _SDL_GameController)ref i8
    func "SDL_GameControllerGetAttached"     (ref _SDL_GameController)i32
    func "SDL_GameControllerGetJoystick"     (ref _SDL_GameController)ref _SDL_Joystick
    func "SDL_GameControllerEventState"      (i32)i32
    proc "SDL_GameControllerUpdate"          ()
    const SDL_CONTROLLER_AXIS_INVALID        = -1
    const SDL_CONTROLLER_AXIS_LEFTX          = 0
    const SDL_CONTROLLER_AXIS_LEFTY          = 1
    const SDL_CONTROLLER_AXIS_RIGHTX         = 2
    const SDL_CONTROLLER_AXIS_RIGHTY         = 3
    const SDL_CONTROLLER_AXIS_TRIGGERLEFT    = 4
    const SDL_CONTROLLER_AXIS_TRIGGERRIGHT   = 5
    const SDL_CONTROLLER_AXIS_MAX            = 6
    func "SDL_GameControllerGetAxisFromString" (ref i8)i32
    func "SDL_GameControllerGetStringForAxis" (i32)ref i8
    func "SDL_GameControllerGetBindForAxis"  (SDL_GameControllerButtonBind,ref _SDL_GameController,i32)SDL_GameControllerButtonBind
    func "SDL_GameControllerHasAxis"         (ref _SDL_GameController,i32)i32
    func "SDL_GameControllerGetAxis"         (ref _SDL_GameController,i32)i16
    const SDL_CONTROLLER_BUTTON_INVALID      = -1
    const SDL_CONTROLLER_BUTTON_A            = 0
    const SDL_CONTROLLER_BUTTON_B            = 1
    const SDL_CONTROLLER_BUTTON_X            = 2
    const SDL_CONTROLLER_BUTTON_Y            = 3
    const SDL_CONTROLLER_BUTTON_BACK         = 4
    const SDL_CONTROLLER_BUTTON_GUIDE        = 5
    const SDL_CONTROLLER_BUTTON_START        = 6
    const SDL_CONTROLLER_BUTTON_LEFTSTICK    = 7
    const SDL_CONTROLLER_BUTTON_RIGHTSTICK   = 8
    const SDL_CONTROLLER_BUTTON_LEFTSHOULDER = 9
    const SDL_CONTROLLER_BUTTON_RIGHTSHOULDER = 10
    const SDL_CONTROLLER_BUTTON_DPAD_UP      = 11
    const SDL_CONTROLLER_BUTTON_DPAD_DOWN    = 12
    const SDL_CONTROLLER_BUTTON_DPAD_LEFT    = 13
    const SDL_CONTROLLER_BUTTON_DPAD_RIGHT   = 14
    const SDL_CONTROLLER_BUTTON_MISC1        = 15
    const SDL_CONTROLLER_BUTTON_PADDLE1      = 16
    const SDL_CONTROLLER_BUTTON_PADDLE2      = 17
    const SDL_CONTROLLER_BUTTON_PADDLE3      = 18
    const SDL_CONTROLLER_BUTTON_PADDLE4      = 19
    const SDL_CONTROLLER_BUTTON_TOUCHPAD     = 20
    const SDL_CONTROLLER_BUTTON_MAX          = 21
    func "SDL_GameControllerGetButtonFromString" (ref i8)i32
    func "SDL_GameControllerGetStringForButton" (i32)ref i8
    func "SDL_GameControllerGetBindForButton" (SDL_GameControllerButtonBind,ref _SDL_GameController,i32)SDL_GameControllerButtonBind
    func "SDL_GameControllerHasButton"       (ref _SDL_GameController,i32)i32
    func "SDL_GameControllerGetButton"       (ref _SDL_GameController,i32)u8
    func "SDL_GameControllerGetNumTouchpads" (ref _SDL_GameController)i32
    func "SDL_GameControllerGetNumTouchpadFingers" (ref _SDL_GameController,i32)i32
    func "SDL_GameControllerGetTouchpadFinger" (ref _SDL_GameController,i32,i32,ref u8,ref r32,ref r32,ref r32)i32
    func "SDL_GameControllerHasSensor"       (ref _SDL_GameController,i32)i32
    func "SDL_GameControllerSetSensorEnabled" (ref _SDL_GameController,i32,i32)i32
    func "SDL_GameControllerIsSensorEnabled" (ref _SDL_GameController,i32)i32
    func "SDL_GameControllerGetSensorDataRate" (ref _SDL_GameController,i32)r32
    func "SDL_GameControllerGetSensorData"   (ref _SDL_GameController,i32,ref r32,i32)i32
    func "SDL_GameControllerGetSensorDataWithTimestamp" (ref _SDL_GameController,i32,ref u64,ref r32,i32)i32
    func "SDL_GameControllerRumble"          (ref _SDL_GameController,u16,u16,u32)i32
    func "SDL_GameControllerRumbleTriggers"  (ref _SDL_GameController,u16,u16,u32)i32
    func "SDL_GameControllerHasLED"          (ref _SDL_GameController)i32
    func "SDL_GameControllerHasRumble"       (ref _SDL_GameController)i32
    func "SDL_GameControllerHasRumbleTriggers" (ref _SDL_GameController)i32
    func "SDL_GameControllerSetLED"          (ref _SDL_GameController,u8,u8,u8)i32
    func "SDL_GameControllerSendEffect"      (ref _SDL_GameController,ref void,i32)i32
    proc "SDL_GameControllerClose"           (ref _SDL_GameController)
    func "SDL_GameControllerGetAppleSFSymbolsNameForButton" (ref _SDL_GameController,i32)ref i8
    func "SDL_GameControllerGetAppleSFSymbolsNameForAxis" (ref _SDL_GameController,i32)ref i8
    const SDL_TOUCH_DEVICE_INVALID           = -1
    const SDL_TOUCH_DEVICE_DIRECT            = 0
    const SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE = 1
    const SDL_TOUCH_DEVICE_INDIRECT_RELATIVE = 2
    type SDL_Finger = struct $caligned
        i64 id
        r32 x
        r32 y
        r32 pressure
    end

    func "SDL_GetNumTouchDevices"            ()i32
    func "SDL_GetTouchDevice"                (i32)i64
    func "SDL_GetTouchName"                  (i32)ref i8
    func "SDL_GetTouchDeviceType"            (i64)i32
    func "SDL_GetNumTouchFingers"            (i64)i32
    func "SDL_GetTouchFinger"                (i64,i32)ref SDL_Finger
    func "SDL_RecordGesture"                 (i64)i32
    func "SDL_SaveAllDollarTemplates"        (ref SDL_RWops)i32
    func "SDL_SaveDollarTemplate"            (i64,ref SDL_RWops)i32
    func "SDL_LoadDollarTemplates"           (i64,ref SDL_RWops)i32
    const SDL_FIRSTEVENT                     = 0
    const SDL_QUIT                           = 256
    const SDL_APP_TERMINATING                = 257
    const SDL_APP_LOWMEMORY                  = 258
    const SDL_APP_WILLENTERBACKGROUND        = 259
    const SDL_APP_DIDENTERBACKGROUND         = 260
    const SDL_APP_WILLENTERFOREGROUND        = 261
    const SDL_APP_DIDENTERFOREGROUND         = 262
    const SDL_LOCALECHANGED                  = 263
    const SDL_DISPLAYEVENT                   = 336
    const SDL_WINDOWEVENT                    = 512
    const SDL_SYSWMEVENT                     = 513
    const SDL_KEYDOWN                        = 768
    const SDL_KEYUP                          = 769
    const SDL_TEXTEDITING                    = 770
    const SDL_TEXTINPUT                      = 771
    const SDL_KEYMAPCHANGED                  = 772
    const SDL_TEXTEDITING_EXT                = 773
    const SDL_MOUSEMOTION                    = 1024
    const SDL_MOUSEBUTTONDOWN                = 1025
    const SDL_MOUSEBUTTONUP                  = 1026
    const SDL_MOUSEWHEEL                     = 1027
    const SDL_JOYAXISMOTION                  = 1536
    const SDL_JOYBALLMOTION                  = 1537
    const SDL_JOYHATMOTION                   = 1538
    const SDL_JOYBUTTONDOWN                  = 1539
    const SDL_JOYBUTTONUP                    = 1540
    const SDL_JOYDEVICEADDED                 = 1541
    const SDL_JOYDEVICEREMOVED               = 1542
    const SDL_JOYBATTERYUPDATED              = 1543
    const SDL_CONTROLLERAXISMOTION           = 1616
    const SDL_CONTROLLERBUTTONDOWN           = 1617
    const SDL_CONTROLLERBUTTONUP             = 1618
    const SDL_CONTROLLERDEVICEADDED          = 1619
    const SDL_CONTROLLERDEVICEREMOVED        = 1620
    const SDL_CONTROLLERDEVICEREMAPPED       = 1621
    const SDL_CONTROLLERTOUCHPADDOWN         = 1622
    const SDL_CONTROLLERTOUCHPADMOTION       = 1623
    const SDL_CONTROLLERTOUCHPADUP           = 1624
    const SDL_CONTROLLERSENSORUPDATE         = 1625
    const SDL_FINGERDOWN                     = 1792
    const SDL_FINGERUP                       = 1793
    const SDL_FINGERMOTION                   = 1794
    const SDL_DOLLARGESTURE                  = 2048
    const SDL_DOLLARRECORD                   = 2049
    const SDL_MULTIGESTURE                   = 2050
    const SDL_CLIPBOARDUPDATE                = 2304
    const SDL_DROPFILE                       = 4096
    const SDL_DROPTEXT                       = 4097
    const SDL_DROPBEGIN                      = 4098
    const SDL_DROPCOMPLETE                   = 4099
    const SDL_AUDIODEVICEADDED               = 4352
    const SDL_AUDIODEVICEREMOVED             = 4353
    const SDL_SENSORUPDATE                   = 4608
    const SDL_RENDER_TARGETS_RESET           = 8192
    const SDL_RENDER_DEVICE_RESET            = 8193
    const SDL_POLLSENTINEL                   = 32512
    const SDL_USEREVENT                      = 32768
    const SDL_LASTEVENT                      = 65535
    type SDL_CommonEvent = struct $caligned
        u32 type$
        u32 timestamp
    end

    type SDL_DisplayEvent = struct $caligned
        u32 type$
        u32 timestamp
        u32 display
        u8 event
        u8 padding1
        u8 padding2
        u8 padding3
        i32 data1
    end

    type SDL_WindowEvent = struct $caligned
        u32 type$
        u32 timestamp
        u32 windowID
        u8 event
        u8 padding1
        u8 padding2
        u8 padding3
        i32 data1
        i32 data2
    end

    type SDL_KeyboardEvent = struct $caligned
        u32 type$
        u32 timestamp
        u32 windowID
        u8 state
        u8 repeat
        u8 padding2
        u8 padding3
        SDL_Keysym keysym
    end

    type SDL_TextEditingEvent = struct $caligned
        u32 type$
        u32 timestamp
        u32 windowID
        [32]i8 text
        i32 start
        i32 length
    end

    type SDL_TextEditingExtEvent = struct $caligned
        u32 type$
        u32 timestamp
        u32 windowID
        ref i8 text
        i32 start
        i32 length
    end

    type SDL_TextInputEvent = struct $caligned
        u32 type$
        u32 timestamp
        u32 windowID
        [32]i8 text
    end

    type SDL_MouseMotionEvent = struct $caligned
        u32 type$
        u32 timestamp
        u32 windowID
        u32 which
        u32 state
        i32 x
        i32 y
        i32 xrel
        i32 yrel
    end

    type SDL_MouseButtonEvent = struct $caligned
        u32 type$
        u32 timestamp
        u32 windowID
        u32 which
        u8 button
        u8 state
        u8 clicks
        u8 padding1
        i32 x
        i32 y
    end

    type SDL_MouseWheelEvent = struct $caligned
        u32 type$
        u32 timestamp
        u32 windowID
        u32 which
        i32 x
        i32 y
        u32 direction
        r32 preciseX
        r32 preciseY
        i32 mouseX
        i32 mouseY
    end

    type SDL_JoyAxisEvent = struct $caligned
        u32 type$
        u32 timestamp
        i32 which
        u8 axis
        u8 padding1
        u8 padding2
        u8 padding3
        i16 value
        u16 padding4
    end

    type SDL_JoyBallEvent = struct $caligned
        u32 type$
        u32 timestamp
        i32 which
        u8 ball
        u8 padding1
        u8 padding2
        u8 padding3
        i16 xrel
        i16 yrel
    end

    type SDL_JoyHatEvent = struct $caligned
        u32 type$
        u32 timestamp
        i32 which
        u8 hat
        u8 value
        u8 padding1
        u8 padding2
    end

    type SDL_JoyButtonEvent = struct $caligned
        u32 type$
        u32 timestamp
        i32 which
        u8 button
        u8 state
        u8 padding1
        u8 padding2
    end

    type SDL_JoyDeviceEvent = struct $caligned
        u32 type$
        u32 timestamp
        i32 which
    end

    type SDL_JoyBatteryEvent = struct $caligned
        u32 type$
        u32 timestamp
        i32 which
        i32 level
    end

    type SDL_ControllerAxisEvent = struct $caligned
        u32 type$
        u32 timestamp
        i32 which
        u8 axis
        u8 padding1
        u8 padding2
        u8 padding3
        i16 value
        u16 padding4
    end

    type SDL_ControllerButtonEvent = struct $caligned
        u32 type$
        u32 timestamp
        i32 which
        u8 button
        u8 state
        u8 padding1
        u8 padding2
    end

    type SDL_ControllerDeviceEvent = struct $caligned
        u32 type$
        u32 timestamp
        i32 which
    end

    type SDL_ControllerTouchpadEvent = struct $caligned
        u32 type$
        u32 timestamp
        i32 which
        i32 touchpad
        i32 finger
        r32 x
        r32 y
        r32 pressure
    end

    type SDL_ControllerSensorEvent = struct $caligned
        u32 type$
        u32 timestamp
        i32 which
        i32 sensor
        [3]r32 data
        u64 timestamp_us
    end

    type SDL_AudioDeviceEvent = struct $caligned
        u32 type$
        u32 timestamp
        u32 which
        u8 iscapture
        u8 padding1
        u8 padding2
        u8 padding3
    end

    type SDL_TouchFingerEvent = struct $caligned
        u32 type$
        u32 timestamp
        i64 touchId
        i64 fingerId
        r32 x
        r32 y
        r32 dx
        r32 dy
        r32 pressure
        u32 windowID
    end

    type SDL_MultiGestureEvent = struct $caligned
        u32 type$
        u32 timestamp
        i64 touchId
        r32 dTheta
        r32 dDist
        r32 x
        r32 y
        u16 numFingers
        u16 padding
    end

    type SDL_DollarGestureEvent = struct $caligned
        u32 type$
        u32 timestamp
        i64 touchId
        i64 gestureId
        u32 numFingers
        r32 error
        r32 x
        r32 y
    end

    type SDL_DropEvent = struct $caligned
        u32 type$
        u32 timestamp
        ref i8 file
        u32 windowID
    end

    type SDL_SensorEvent = struct $caligned
        u32 type$
        u32 timestamp
        i32 which
        [6]r32 data
        u64 timestamp_us
    end

    type SDL_QuitEvent = struct $caligned
        u32 type$
        u32 timestamp
    end

    type SDL_OSEvent = struct $caligned
        u32 type$
        u32 timestamp
    end

    type SDL_UserEvent = struct $caligned
        u32 type$
        u32 timestamp
        u32 windowID
        i32 code
        ref void data1
        ref void data2
    end

    type SDL_SysWMmsg = struct $caligned
        int dummy    !empty record
    end

    type SDL_SysWMEvent = struct $caligned
        u32 type$
        u32 timestamp
        ref SDL_SysWMmsg msg
    end

    type SDL_Event = struct $caligned
        u32 type$
        SDL_CommonEvent common
        SDL_DisplayEvent display
        SDL_WindowEvent window
        SDL_KeyboardEvent key
        SDL_TextEditingEvent edit
        SDL_TextEditingExtEvent editExt
        SDL_TextInputEvent text
        SDL_MouseMotionEvent motion
        SDL_MouseButtonEvent button
        SDL_MouseWheelEvent wheel
        SDL_JoyAxisEvent jaxis
        SDL_JoyBallEvent jball
        SDL_JoyHatEvent jhat
        SDL_JoyButtonEvent jbutton
        SDL_JoyDeviceEvent jdevice
        SDL_JoyBatteryEvent jbattery
        SDL_ControllerAxisEvent caxis
        SDL_ControllerButtonEvent cbutton
        SDL_ControllerDeviceEvent cdevice
        SDL_ControllerTouchpadEvent ctouchpad
        SDL_ControllerSensorEvent csensor
        SDL_AudioDeviceEvent adevice
        SDL_SensorEvent sensor
        SDL_QuitEvent quit
        SDL_UserEvent user
        SDL_SysWMEvent syswm
        SDL_TouchFingerEvent tfinger
        SDL_MultiGestureEvent mgesture
        SDL_DollarGestureEvent dgesture
        SDL_DropEvent drop
        [56]u8 padding
    end

    proc "SDL_PumpEvents"                    ()
    const SDL_ADDEVENT                       = 0
    const SDL_PEEKEVENT                      = 1
    const SDL_GETEVENT                       = 2
    func "SDL_PeepEvents"                    (ref SDL_Event,i32,i32,u32,u32)i32
    func "SDL_HasEvent"                      (u32)i32
    func "SDL_HasEvents"                     (u32,u32)i32
    proc "SDL_FlushEvent"                    (u32)
    proc "SDL_FlushEvents"                   (u32,u32)
    func "SDL_PollEvent"                     (ref SDL_Event)i32
    func "SDL_WaitEvent"                     (ref SDL_Event)i32
    func "SDL_WaitEventTimeout"              (ref SDL_Event,i32)i32
    func "SDL_PushEvent"                     (ref SDL_Event)i32
    proc "SDL_SetEventFilter"                (ref void,ref void)
    func "SDL_GetEventFilter"                (ref ref void,ref ref void)i32
    proc "SDL_AddEventWatch"                 (ref void,ref void)
    proc "SDL_DelEventWatch"                 (ref void,ref void)
    proc "SDL_FilterEvents"                  (ref void,ref void)
    func "SDL_EventState"                    (u32,i32)u8
    func "SDL_RegisterEvents"                (i32)u32
    func "SDL_GetBasePath"                   ()ref i8
    func "SDL_GetPrefPath"                   (ref i8,ref i8)ref i8
    type _SDL_Haptic = struct $caligned
        int dummy    !empty record
    end

    type SDL_HapticDirection = struct $caligned
        u8 type$
        [3]i32 dir
    end

    type SDL_HapticConstant = struct $caligned
        u16 type$
        SDL_HapticDirection direction
        u32 length
        u16 delay
        u16 button
        u16 interval
        i16 level
        u16 attack_length
        u16 attack_level
        u16 fade_length
        u16 fade_level
    end

    type SDL_HapticPeriodic = struct $caligned
        u16 type$
        SDL_HapticDirection direction
        u32 length
        u16 delay
        u16 button
        u16 interval
        u16 period
        i16 magnitude
        i16 offset
        u16 phase
        u16 attack_length
        u16 attack_level
        u16 fade_length
        u16 fade_level
    end

    type SDL_HapticCondition = struct $caligned
        u16 type$
        SDL_HapticDirection direction
        u32 length
        u16 delay
        u16 button
        u16 interval
        [3]u16 right_sat
        [3]u16 left_sat
        [3]i16 right_coeff
        [3]i16 left_coeff
        [3]u16 deadband
        [3]i16 center
    end

    type SDL_HapticRamp = struct $caligned
        u16 type$
        SDL_HapticDirection direction
        u32 length
        u16 delay
        u16 button
        u16 interval
        i16 start
        i16 end
        u16 attack_length
        u16 attack_level
        u16 fade_length
        u16 fade_level
    end

    type SDL_HapticLeftRight = struct $caligned
        u16 type$
        u32 length
        u16 large_magnitude
        u16 small_magnitude
    end

    type SDL_HapticCustom = struct $caligned
        u16 type$
        SDL_HapticDirection direction
        u32 length
        u16 delay
        u16 button
        u16 interval
        u8 channels
        u16 period
        u16 samples
        ref u16 data
        u16 attack_length
        u16 attack_level
        u16 fade_length
        u16 fade_level
    end

    type SDL_HapticEffect = struct $caligned
        u16 type$
        SDL_HapticConstant constant
        SDL_HapticPeriodic periodic
        SDL_HapticCondition condition
        SDL_HapticRamp ramp
        SDL_HapticLeftRight leftright
        SDL_HapticCustom custom
    end

    func "SDL_NumHaptics"                    ()i32
    func "SDL_HapticName"                    (i32)ref i8
    func "SDL_HapticOpen"                    (i32)ref _SDL_Haptic
    func "SDL_HapticOpened"                  (i32)i32
    func "SDL_HapticIndex"                   (ref _SDL_Haptic)i32
    func "SDL_MouseIsHaptic"                 ()i32
    func "SDL_HapticOpenFromMouse"           ()ref _SDL_Haptic
    func "SDL_JoystickIsHaptic"              (ref _SDL_Joystick)i32
    func "SDL_HapticOpenFromJoystick"        (ref _SDL_Joystick)ref _SDL_Haptic
    proc "SDL_HapticClose"                   (ref _SDL_Haptic)
    func "SDL_HapticNumEffects"              (ref _SDL_Haptic)i32
    func "SDL_HapticNumEffectsPlaying"       (ref _SDL_Haptic)i32
    func "SDL_HapticQuery"                   (ref _SDL_Haptic)u32
    func "SDL_HapticNumAxes"                 (ref _SDL_Haptic)i32
    func "SDL_HapticEffectSupported"         (ref _SDL_Haptic,ref SDL_HapticEffect)i32
    func "SDL_HapticNewEffect"               (ref _SDL_Haptic,ref SDL_HapticEffect)i32
    func "SDL_HapticUpdateEffect"            (ref _SDL_Haptic,i32,ref SDL_HapticEffect)i32
    func "SDL_HapticRunEffect"               (ref _SDL_Haptic,i32,u32)i32
    func "SDL_HapticStopEffect"              (ref _SDL_Haptic,i32)i32
    proc "SDL_HapticDestroyEffect"           (ref _SDL_Haptic,i32)
    func "SDL_HapticGetEffectStatus"         (ref _SDL_Haptic,i32)i32
    func "SDL_HapticSetGain"                 (ref _SDL_Haptic,i32)i32
    func "SDL_HapticSetAutocenter"           (ref _SDL_Haptic,i32)i32
    func "SDL_HapticPause"                   (ref _SDL_Haptic)i32
    func "SDL_HapticUnpause"                 (ref _SDL_Haptic)i32
    func "SDL_HapticStopAll"                 (ref _SDL_Haptic)i32
    func "SDL_HapticRumbleSupported"         (ref _SDL_Haptic)i32
    func "SDL_HapticRumbleInit"              (ref _SDL_Haptic)i32
    func "SDL_HapticRumblePlay"              (ref _SDL_Haptic,r32,u32)i32
    func "SDL_HapticRumbleStop"              (ref _SDL_Haptic)i32
    type SDL_hid_device_ = struct $caligned
        int dummy    !empty record
    end

    type SDL_hid_device_info = struct $caligned
        ref i8 path
        u16 vendor_id
        u16 product_id
        ref u16 serial_number
        u16 release_number
        ref u16 manufacturer_string
        ref u16 product_string
        u16 usage_page
        u16 usage
        i32 interface_number
        i32 interface_class
        i32 interface_subclass
        i32 interface_protocol
        ref SDL_hid_device_info next$
    end

    func "SDL_hid_init"                      ()i32
    func "SDL_hid_exit"                      ()i32
    func "SDL_hid_device_change_count"       ()u32
    func "SDL_hid_enumerate"                 (u16,u16)ref SDL_hid_device_info
    proc "SDL_hid_free_enumeration"          (ref SDL_hid_device_info)
    func "SDL_hid_open"                      (u16,u16,ref u16)ref SDL_hid_device_
    func "SDL_hid_open_path"                 (ref i8,i32)ref SDL_hid_device_
    func "SDL_hid_write"                     (ref SDL_hid_device_,ref u8,u64)i32
    func "SDL_hid_read_timeout"              (ref SDL_hid_device_,ref u8,u64,i32)i32
    func "SDL_hid_read"                      (ref SDL_hid_device_,ref u8,u64)i32
    func "SDL_hid_set_nonblocking"           (ref SDL_hid_device_,i32)i32
    func "SDL_hid_send_feature_report"       (ref SDL_hid_device_,ref u8,u64)i32
    func "SDL_hid_get_feature_report"        (ref SDL_hid_device_,ref u8,u64)i32
    proc "SDL_hid_close"                     (ref SDL_hid_device_)
    func "SDL_hid_get_manufacturer_string"   (ref SDL_hid_device_,ref u16,u64)i32
    func "SDL_hid_get_product_string"        (ref SDL_hid_device_,ref u16,u64)i32
    func "SDL_hid_get_serial_number_string"  (ref SDL_hid_device_,ref u16,u64)i32
    func "SDL_hid_get_indexed_string"        (ref SDL_hid_device_,i32,ref u16,u64)i32
    proc "SDL_hid_ble_scan"                  (i32)
    const SDL_HINT_DEFAULT                   = 0
    const SDL_HINT_NORMAL                    = 1
    const SDL_HINT_OVERRIDE                  = 2
    func "SDL_SetHintWithPriority"           (ref i8,ref i8,i32)i32
    func "SDL_SetHint"                       (ref i8,ref i8)i32
    func "SDL_ResetHint"                     (ref i8)i32
    proc "SDL_ResetHints"                    ()
    func "SDL_GetHint"                       (ref i8)ref i8
    func "SDL_GetHintBoolean"                (ref i8,i32)i32
    proc "SDL_AddHintCallback"               (ref i8,ref void,ref void)
    proc "SDL_DelHintCallback"               (ref i8,ref void,ref void)
    proc "SDL_ClearHints"                    ()
    func "SDL_LoadObject"                    (ref i8)ref void
    func "SDL_LoadFunction"                  (ref void,ref i8)ref void
    proc "SDL_UnloadObject"                  (ref void)
    const SDL_LOG_CATEGORY_APPLICATION       = 0
    const SDL_LOG_CATEGORY_ERROR             = 1
    const SDL_LOG_CATEGORY_ASSERT            = 2
    const SDL_LOG_CATEGORY_SYSTEM            = 3
    const SDL_LOG_CATEGORY_AUDIO             = 4
    const SDL_LOG_CATEGORY_VIDEO             = 5
    const SDL_LOG_CATEGORY_RENDER            = 6
    const SDL_LOG_CATEGORY_INPUT             = 7
    const SDL_LOG_CATEGORY_TEST              = 8
    const SDL_LOG_CATEGORY_RESERVED1         = 9
    const SDL_LOG_CATEGORY_RESERVED2         = 10
    const SDL_LOG_CATEGORY_RESERVED3         = 11
    const SDL_LOG_CATEGORY_RESERVED4         = 12
    const SDL_LOG_CATEGORY_RESERVED5         = 13
    const SDL_LOG_CATEGORY_RESERVED6         = 14
    const SDL_LOG_CATEGORY_RESERVED7         = 15
    const SDL_LOG_CATEGORY_RESERVED8         = 16
    const SDL_LOG_CATEGORY_RESERVED9         = 17
    const SDL_LOG_CATEGORY_RESERVED10        = 18
    const SDL_LOG_CATEGORY_CUSTOM            = 19
    const SDL_LOG_PRIORITY_VERBOSE           = 1
    const SDL_LOG_PRIORITY_DEBUG             = 2
    const SDL_LOG_PRIORITY_INFO              = 3
    const SDL_LOG_PRIORITY_WARN              = 4
    const SDL_LOG_PRIORITY_ERROR             = 5
    const SDL_LOG_PRIORITY_CRITICAL          = 6
    const SDL_NUM_LOG_PRIORITIES             = 7
    proc "SDL_LogSetAllPriority"             (i32)
    proc "SDL_LogSetPriority"                (i32,i32)
    func "SDL_LogGetPriority"                (i32)i32
    proc "SDL_LogResetPriorities"            ()
    proc "SDL_Log"                           (ref i8,...)
    proc "SDL_LogVerbose"                    (i32,ref i8,...)
    proc "SDL_LogDebug"                      (i32,ref i8,...)
    proc "SDL_LogInfo"                       (i32,ref i8,...)
    proc "SDL_LogWarn"                       (i32,ref i8,...)
    proc "SDL_LogError"                      (i32,ref i8,...)
    proc "SDL_LogCritical"                   (i32,ref i8,...)
    proc "SDL_LogMessage"                    (i32,i32,ref i8,...)
    proc "SDL_LogMessageV"                   (i32,i32,ref i8,ref i8)
    proc "SDL_LogGetOutputFunction"          (ref ref void,ref ref void)
    proc "SDL_LogSetOutputFunction"          (ref void,ref void)
    const SDL_MESSAGEBOX_ERROR               = 16
    const SDL_MESSAGEBOX_WARNING             = 32
    const SDL_MESSAGEBOX_INFORMATION         = 64
    const SDL_MESSAGEBOX_BUTTONS_LEFT_TO_RIGHT = 128
    const SDL_MESSAGEBOX_BUTTONS_RIGHT_TO_LEFT = 256
    const SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT = 1
    const SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT = 2
    type $T12 = struct $caligned
        u32 flags
        i32 buttonid
        ref i8 text
    end

    type $T13 = struct $caligned
        u8 r
        u8 g
        u8 b
    end

    const SDL_MESSAGEBOX_COLOR_BACKGROUND    = 0
    const SDL_MESSAGEBOX_COLOR_TEXT          = 1
    const SDL_MESSAGEBOX_COLOR_BUTTON_BORDER = 2
    const SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND = 3
    const SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED = 4
    const SDL_MESSAGEBOX_COLOR_MAX           = 5
    type $T14 = struct $caligned
        [5]$T13 colors
    end

    type $T15 = struct $caligned
        u32 flags
        ref SDL_Window window
        ref i8 title
        ref i8 message
        i32 numbuttons
        ref $T12 buttons
        ref $T14 colorScheme
    end

    func "SDL_ShowMessageBox"                (ref $T15,ref i32)i32
    func "SDL_ShowSimpleMessageBox"          (u32,ref i8,ref i8,ref SDL_Window)i32
    func "SDL_Metal_CreateView"              (ref SDL_Window)ref void
    proc "SDL_Metal_DestroyView"             (ref void)
    func "SDL_Metal_GetLayer"                (ref void)ref void
    proc "SDL_Metal_GetDrawableSize"         (ref SDL_Window,ref i32,ref i32)
    const SDL_POWERSTATE_UNKNOWN             = 0
    const SDL_POWERSTATE_ON_BATTERY          = 1
    const SDL_POWERSTATE_NO_BATTERY          = 2
    const SDL_POWERSTATE_CHARGING            = 3
    const SDL_POWERSTATE_CHARGED             = 4
    func "SDL_GetPowerInfo"                  (ref i32,ref i32)i32
    const SDL_RENDERER_SOFTWARE              = 1
    const SDL_RENDERER_ACCELERATED           = 2
    const SDL_RENDERER_PRESENTVSYNC          = 4
    const SDL_RENDERER_TARGETTEXTURE         = 8
    type SDL_RendererInfo = struct $caligned
        ref i8 name
        u32 flags
        u32 num_texture_formats
        [16]u32 texture_formats
        i32 max_texture_width
        i32 max_texture_height
    end

    type SDL_Vertex = struct $caligned
        SDL_FPoint position
        SDL_Color color
        SDL_FPoint tex_coord
    end

    const SDL_ScaleModeNearest               = 0
    const SDL_ScaleModeLinear                = 1
    const SDL_ScaleModeBest                  = 2
    const SDL_TEXTUREACCESS_STATIC           = 0
    const SDL_TEXTUREACCESS_STREAMING        = 1
    const SDL_TEXTUREACCESS_TARGET           = 2
    const SDL_TEXTUREMODULATE_NONE           = 0
    const SDL_TEXTUREMODULATE_COLOR          = 1
    const SDL_TEXTUREMODULATE_ALPHA          = 2
    const SDL_FLIP_NONE                      = 0
    const SDL_FLIP_HORIZONTAL                = 1
    const SDL_FLIP_VERTICAL                  = 2
    type SDL_Renderer = struct $caligned
        int dummy    !empty record
    end

    type SDL_Texture = struct $caligned
        int dummy    !empty record
    end

    func "SDL_GetNumRenderDrivers"           ()i32
    func "SDL_GetRenderDriverInfo"           (i32,ref SDL_RendererInfo)i32
    func "SDL_CreateWindowAndRenderer"       (i32,i32,u32,ref ref SDL_Window,ref ref SDL_Renderer)i32
    func "SDL_CreateRenderer"                (ref SDL_Window,i32,u32)ref SDL_Renderer
    func "SDL_CreateSoftwareRenderer"        (ref SDL_Surface)ref SDL_Renderer
    func "SDL_GetRenderer"                   (ref SDL_Window)ref SDL_Renderer
    func "SDL_RenderGetWindow"               (ref SDL_Renderer)ref SDL_Window
    func "SDL_GetRendererInfo"               (ref SDL_Renderer,ref SDL_RendererInfo)i32
    func "SDL_GetRendererOutputSize"         (ref SDL_Renderer,ref i32,ref i32)i32
    func "SDL_CreateTexture"                 (ref SDL_Renderer,u32,i32,i32,i32)ref SDL_Texture
    func "SDL_CreateTextureFromSurface"      (ref SDL_Renderer,ref SDL_Surface)ref SDL_Texture
    func "SDL_QueryTexture"                  (ref SDL_Texture,ref u32,ref i32,ref i32,ref i32)i32
    func "SDL_SetTextureColorMod"            (ref SDL_Texture,u8,u8,u8)i32
    func "SDL_GetTextureColorMod"            (ref SDL_Texture,ref u8,ref u8,ref u8)i32
    func "SDL_SetTextureAlphaMod"            (ref SDL_Texture,u8)i32
    func "SDL_GetTextureAlphaMod"            (ref SDL_Texture,ref u8)i32
    func "SDL_SetTextureBlendMode"           (ref SDL_Texture,i32)i32
    func "SDL_GetTextureBlendMode"           (ref SDL_Texture,ref i32)i32
    func "SDL_SetTextureScaleMode"           (ref SDL_Texture,i32)i32
    func "SDL_GetTextureScaleMode"           (ref SDL_Texture,ref i32)i32
    func "SDL_SetTextureUserData"            (ref SDL_Texture,ref void)i32
    func "SDL_GetTextureUserData"            (ref SDL_Texture)ref void
    func "SDL_UpdateTexture"                 (ref SDL_Texture,ref SDL_Rect,ref void,i32)i32
    func "SDL_UpdateYUVTexture"              (ref SDL_Texture,ref SDL_Rect,ref u8,i32,ref u8,i32,ref u8,i32)i32
    func "SDL_UpdateNVTexture"               (ref SDL_Texture,ref SDL_Rect,ref u8,i32,ref u8,i32)i32
    func "SDL_LockTexture"                   (ref SDL_Texture,ref SDL_Rect,ref ref void,ref i32)i32
    func "SDL_LockTextureToSurface"          (ref SDL_Texture,ref SDL_Rect,ref ref SDL_Surface)i32
    proc "SDL_UnlockTexture"                 (ref SDL_Texture)
    func "SDL_RenderTargetSupported"         (ref SDL_Renderer)i32
    func "SDL_SetRenderTarget"               (ref SDL_Renderer,ref SDL_Texture)i32
    func "SDL_GetRenderTarget"               (ref SDL_Renderer)ref SDL_Texture
    func "SDL_RenderSetLogicalSize"          (ref SDL_Renderer,i32,i32)i32
    proc "SDL_RenderGetLogicalSize"          (ref SDL_Renderer,ref i32,ref i32)
    func "SDL_RenderSetIntegerScale"         (ref SDL_Renderer,i32)i32
    func "SDL_RenderGetIntegerScale"         (ref SDL_Renderer)i32
    func "SDL_RenderSetViewport"             (ref SDL_Renderer,ref SDL_Rect)i32
    proc "SDL_RenderGetViewport"             (ref SDL_Renderer,ref SDL_Rect)
    func "SDL_RenderSetClipRect"             (ref SDL_Renderer,ref SDL_Rect)i32
    proc "SDL_RenderGetClipRect"             (ref SDL_Renderer,ref SDL_Rect)
    func "SDL_RenderIsClipEnabled"           (ref SDL_Renderer)i32
    func "SDL_RenderSetScale"                (ref SDL_Renderer,r32,r32)i32
    proc "SDL_RenderGetScale"                (ref SDL_Renderer,ref r32,ref r32)
    proc "SDL_RenderWindowToLogical"         (ref SDL_Renderer,i32,i32,ref r32,ref r32)
    proc "SDL_RenderLogicalToWindow"         (ref SDL_Renderer,r32,r32,ref i32,ref i32)
    func "SDL_SetRenderDrawColor"            (ref SDL_Renderer,u8,u8,u8,u8)i32
    func "SDL_GetRenderDrawColor"            (ref SDL_Renderer,ref u8,ref u8,ref u8,ref u8)i32
    func "SDL_SetRenderDrawBlendMode"        (ref SDL_Renderer,i32)i32
    func "SDL_GetRenderDrawBlendMode"        (ref SDL_Renderer,ref i32)i32
    func "SDL_RenderClear"                   (ref SDL_Renderer)i32
    func "SDL_RenderDrawPoint"               (ref SDL_Renderer,i32,i32)i32
    func "SDL_RenderDrawPoints"              (ref SDL_Renderer,ref SDL_Point,i32)i32
    func "SDL_RenderDrawLine"                (ref SDL_Renderer,i32,i32,i32,i32)i32
    func "SDL_RenderDrawLines"               (ref SDL_Renderer,ref SDL_Point,i32)i32
    func "SDL_RenderDrawRect"                (ref SDL_Renderer,ref SDL_Rect)i32
    func "SDL_RenderDrawRects"               (ref SDL_Renderer,ref SDL_Rect,i32)i32
    func "SDL_RenderFillRect"                (ref SDL_Renderer,ref SDL_Rect)i32
    func "SDL_RenderFillRects"               (ref SDL_Renderer,ref SDL_Rect,i32)i32
    func "SDL_RenderCopy"                    (ref SDL_Renderer,ref SDL_Texture,ref SDL_Rect,ref SDL_Rect)i32
    func "SDL_RenderCopyEx"                  (ref SDL_Renderer,ref SDL_Texture,ref SDL_Rect,ref SDL_Rect,r64,ref SDL_Point,i32)i32
    func "SDL_RenderDrawPointF"              (ref SDL_Renderer,r32,r32)i32
    func "SDL_RenderDrawPointsF"             (ref SDL_Renderer,ref SDL_FPoint,i32)i32
    func "SDL_RenderDrawLineF"               (ref SDL_Renderer,r32,r32,r32,r32)i32
    func "SDL_RenderDrawLinesF"              (ref SDL_Renderer,ref SDL_FPoint,i32)i32
    func "SDL_RenderDrawRectF"               (ref SDL_Renderer,ref SDL_FRect)i32
    func "SDL_RenderDrawRectsF"              (ref SDL_Renderer,ref SDL_FRect,i32)i32
    func "SDL_RenderFillRectF"               (ref SDL_Renderer,ref SDL_FRect)i32
    func "SDL_RenderFillRectsF"              (ref SDL_Renderer,ref SDL_FRect,i32)i32
    func "SDL_RenderCopyF"                   (ref SDL_Renderer,ref SDL_Texture,ref SDL_Rect,ref SDL_FRect)i32
    func "SDL_RenderCopyExF"                 (ref SDL_Renderer,ref SDL_Texture,ref SDL_Rect,ref SDL_FRect,r64,ref SDL_FPoint,i32)i32
    func "SDL_RenderGeometry"                (ref SDL_Renderer,ref SDL_Texture,ref SDL_Vertex,i32,ref i32,i32)i32
    func "SDL_RenderGeometryRaw"             (ref SDL_Renderer,ref SDL_Texture,ref r32,i32,ref SDL_Color,i32,ref r32,i32,i32,ref void,i32,i32)i32
    func "SDL_RenderReadPixels"              (ref SDL_Renderer,ref SDL_Rect,u32,ref void,i32)i32
    proc "SDL_RenderPresent"                 (ref SDL_Renderer)
    proc "SDL_DestroyTexture"                (ref SDL_Texture)
    proc "SDL_DestroyRenderer"               (ref SDL_Renderer)
    func "SDL_RenderFlush"                   (ref SDL_Renderer)i32
    func "SDL_GL_BindTexture"                (ref SDL_Texture,ref r32,ref r32)i32
    func "SDL_GL_UnbindTexture"              (ref SDL_Texture)i32
    func "SDL_RenderGetMetalLayer"           (ref SDL_Renderer)ref void
    func "SDL_RenderGetMetalCommandEncoder"  (ref SDL_Renderer)ref void
    func "SDL_RenderSetVSync"                (ref SDL_Renderer,i32)i32
    func "SDL_CreateShapedWindow"            (ref i8,u32,u32,u32,u32,u32)ref SDL_Window
    func "SDL_IsShapedWindow"                (ref SDL_Window)i32
    const ShapeModeDefault                   = 0
    const ShapeModeBinarizeAlpha             = 1
    const ShapeModeReverseBinarizeAlpha      = 2
    const ShapeModeColorKey                  = 3
    type $T16 = struct $caligned
        u8 binarizationCutoff
        SDL_Color colorKey
    end

    type SDL_WindowShapeMode = struct $caligned
        i32 mode
        $T16 parameters
    end

    func "SDL_SetWindowShape"                (ref SDL_Window,ref SDL_Surface,ref SDL_WindowShapeMode)i32
    func "SDL_GetShapedWindowMode"           (ref SDL_Window,ref SDL_WindowShapeMode)i32
    proc "SDL_SetWindowsMessageHook"         (ref void,ref void)
    func "SDL_Direct3D9GetAdapterIndex"      (i32)i32
    type IDirect3DDevice9 = struct $caligned
        int dummy    !empty record
    end

    func "SDL_RenderGetD3D9Device"           (ref SDL_Renderer)ref IDirect3DDevice9
    type ID3D11Device = struct $caligned
        int dummy    !empty record
    end

    func "SDL_RenderGetD3D11Device"          (ref SDL_Renderer)ref ID3D11Device
    type ID3D12Device = struct $caligned
        int dummy    !empty record
    end

    func "SDL_RenderGetD3D12Device"          (ref SDL_Renderer)ref ID3D12Device
    func "SDL_DXGIGetOutputInfo"             (i32,ref i32,ref i32)i32
    func "SDL_IsTablet"                      ()i32
    proc "SDL_OnApplicationWillTerminate"    ()
    proc "SDL_OnApplicationDidReceiveMemoryWarning" ()
    proc "SDL_OnApplicationWillResignActive" ()
    proc "SDL_OnApplicationDidEnterBackground" ()
    proc "SDL_OnApplicationWillEnterForeground" ()
    proc "SDL_OnApplicationDidBecomeActive"  ()
    func "SDL_GetTicks"                      ()u32
    func "SDL_GetTicks64"                    ()u64
    func "SDL_GetPerformanceCounter"         ()u64
    func "SDL_GetPerformanceFrequency"       ()u64
    proc "SDL_Delay"                         (u32)
    func "SDL_AddTimer"                      (u32,ref void,ref void)i32
    func "SDL_RemoveTimer"                   (i32)i32
    type SDL_version = struct $caligned
        u8 major
        u8 minor
        u8 patch
    end

    proc "SDL_GetVersion"                    (ref SDL_version)
    func "SDL_GetRevision"                   ()ref i8
    func "SDL_GetRevisionNumber"             ()i32
    type SDL_Locale = struct $caligned
        ref i8 language
        ref i8 country
    end

    func "SDL_GetPreferredLocales"           ()ref SDL_Locale
    func "SDL_OpenURL"                       (ref i8)i32
    func "SDL_Init"                          (u32)i32
    func "SDL_InitSubSystem"                 (u32)i32
    proc "SDL_QuitSubSystem"                 (u32)
    func "SDL_WasInit"                       (u32)u32
    proc "SDL_Quit"                          ()
end
global macro  SDL_MAX_UINT32 = ((Uint32)0xFFFFFFFFu)
global macro  SDL_HINT_GRAB_KEYBOARD = "SDL_GRAB_KEYBOARD"
global macro  SDL_HINT_JOYSTICK_HIDAPI_COMBINE_JOY_CONS = "SDL_JOYSTICK_HIDAPI_COMBINE_JOY_CONS"
global const SDL_HAPTIC_STEERING_AXIS = 3
global macro  SDL_TEXTEDITINGEVENT_TEXT_SIZE = (32)
global const SDL_BUTTON_LEFT = 1
global macro  SDL_MIN_SINT16 = ((Sint16)(~0x7FFF))
global macro  SDL_HINT_JOYSTICK_HIDAPI_XBOX_ONE = "SDL_JOYSTICK_HIDAPI_XBOX_ONE"
global macro  SDL_HINT_APP_NAME = "SDL_APP_NAME"
global macro  SDL_HINT_LINUX_HAT_DEADZONES = "SDL_LINUX_HAT_DEADZONES"
global const WAIT_CHILD = 0
global const SDL_HAPTIC_XINPUT =  1
global const INT64_MAX =  0x7FFFFFFFFFFFFFFF
global macro  SDL_MIN_SINT32 = ((Sint32)(~0x7FFFFFFF))
global macro  SDL_MAX_UINT64 = ((Uint64)0xFFFFFFFFFFFFFFFFull)
global const CALLBACK = $callback
global macro  SDL_HAPTIC_GAIN = (1u<<12)
global macro  SDL_TOUCH_MOUSEID = ((Uint32)-1)
global macro  SDL_HINT_AUDIO_DEVICE_APP_NAME = "SDL_AUDIO_DEVICE_APP_NAME"
global macro  SDL_HINT_WINDOWS_FORCE_SEMAPHORE_KERNEL = "SDL_WINDOWS_FORCE_SEMAPHORE_KERNEL"
global const HAVE_DINPUT_H = 1
global macro  SDL_HINT_AUDIO_RESAMPLING_MODE = "SDL_AUDIO_RESAMPLING_MODE"
global macro  SDL_MIN_SINT64 = ((Sint64)(~0x7FFFFFFFFFFFFFFFll))
global macro  SDL_Unsupported = SDL_Error(SDL_UNSUPPORTED)
global macro  SDL_arraysize(array) = (sizeof(array)/sizeof(array[0]))
global const AUDIO_S32SYS = AUDIO_S32LSB
global macro  SDL_iconv_wchar_utf8(S) =  SDL_iconv_string("UTF-8","WCHAR_T",(char*)S,(SDL_wcslen(S)+1)*sizeof(wchar_t))
global macro  SDL_ISPIXELFORMAT_FOURCC(format) = ((format)&&(SDL_PIXELFLAG(format)!=1))
global macro  SDL_WINDOWPOS_ISUNDEFINED(X) = (((X)&0xFFFF0000)==SDL_WINDOWPOS_UNDEFINED_MASK)
global macro  SDL_PRIX32 = "X"
global const SDL_HAPTIC_CARTESIAN = 1
global macro  SDL_HAPTIC_SINE = (1u<<1)
global macro  SDL_HINT_JOYSTICK_HIDAPI_VERTICAL_JOY_CONS = "SDL_JOYSTICK_HIDAPI_VERTICAL_JOY_CONS"
global macro  SDL_enabled_assert(condition) = do{while(!(condition)){static struct SDL_AssertData sdl_assert_data={0,0,#condition,0,0,0,0};const SDL_AssertState sdl_assert_state=SDL_ReportAssertion(&sdl_assert_data,SDL_FUNCTION,SDL_FILE,SDL_LINE);if(sdl_assert_state==SDL_ASSERTION_RETRY){continue;}else if(sdl_assert_state==SDL_ASSERTION_BREAK){SDL_TriggerBreakpoint();}break;}}while(SDL_NULL_WHILE_LOOP_CONDITION)
global const INT64_MIN = 0x8000000000000000
global const SDL_BlitScaled =  SDL_UpperBlitScaled
global macro  SDL_HINT_BMP_SAVE_LEGACY_FORMAT = "SDL_BMP_SAVE_LEGACY_FORMAT"
global const UINT8_MAX = 255
global macro  SDL_AUDIO_ALLOW_ANY_CHANGE = (SDL_AUDIO_ALLOW_FREQUENCY_CHANGE|SDL_AUDIO_ALLOW_FORMAT_CHANGE|SDL_AUDIO_ALLOW_CHANNELS_CHANGE|SDL_AUDIO_ALLOW_SAMPLES_CHANGE)
global macro  SDL_HINT_XINPUT_ENABLED = "SDL_XINPUT_ENABLED"
global macro  SDL_HINT_JOYSTICK_HIDAPI_LUNA = "SDL_JOYSTICK_HIDAPI_LUNA"
global macro  SDL_BUTTON_RMASK = SDL_BUTTON(SDL_BUTTON_RIGHT)
global macro  SDL_PRIX64 = "I64X"
global const SDL_ASSERT_LEVEL = 1
global macro  SDL_HINT_ALLOW_ALT_TAB_WHILE_GRABBED = "SDL_ALLOW_ALT_TAB_WHILE_GRABBED"
global macro  SDL_HAPTIC_STATUS = (1u<<14)
global macro  SDL_AUDIO_ISLITTLEENDIAN(x) = (!SDL_AUDIO_ISBIGENDIAN(x))
global macro  SDL_HINT_AUDIO_INCLUDE_MONITORS = "SDL_AUDIO_INCLUDE_MONITORS"
global macro  SDL_HINT_IME_INTERNAL_EDITING = "SDL_IME_INTERNAL_EDITING"
global macro  SDL_SHAPEMODEALPHA(mode) = (mode==ShapeModeDefault||mode==ShapeModeBinarizeAlpha||mode==ShapeModeReverseBinarizeAlpha)
global const SDL_assert_data = SDL_AssertData
global macro  SDL_HINT_AUTO_UPDATE_SENSORS = "SDL_AUTO_UPDATE_SENSORS"
global const SIZE_MAX = 0xFFFFFFFFFFFFFFFF
global macro  SDL_HINT_JOYSTICK_DEVICE = "SDL_JOYSTICK_DEVICE"
global const HAVE_AUDIOCLIENT_H = 1
global macro  SDL_stack_alloc(type,count) = (type*)SDL_malloc(sizeof(type)*(count))
global const SDL_LINE = __LINE__
global macro  SDL_DEFINE_PIXELFOURCC(A,B,C,D) =  SDL_FOURCC(A,B,C,D)
global const main = SDL_main
global macro  SDL_VERSIONNUM(X,Y,Z) = ((X)*1000+(Y)*100+(Z))
global const SDL_DONTFREE = 0x00000004
global const SDL_ENABLE =  1
global macro  SDL_HINT_KMSDRM_REQUIRE_DRM_MASTER = "SDL_KMSDRM_REQUIRE_DRM_MASTER"
global macro  NULL = ((void*)0)
global const SDL_RWOPS_UNKNOWN = 0U
global const RW_SEEK_END =  2
global macro  SDL_HINT_ORIENTATIONS = "SDL_IOS_ORIENTATIONS"
global const AUDIO_U16LSB = 0x0010
global macro  SDL_PRIu32 = "u"
global macro  SDL_PIXELORDER(X) = (((X)>>20)&0x0F)
global macro  SDL_HAPTIC_CONSTANT = (1u<<0)
global macro  SDL_HINT_JOYSTICK_HIDAPI_SHIELD = "SDL_JOYSTICK_HIDAPI_SHIELD"
global macro  SDL_ISPIXELFORMAT_ARRAY(format) = (!SDL_ISPIXELFORMAT_FOURCC(format)&&((SDL_PIXELTYPE(format)==SDL_PIXELTYPE_ARRAYU8)||(SDL_PIXELTYPE(format)==SDL_PIXELTYPE_ARRAYU16)||(SDL_PIXELTYPE(format)==SDL_PIXELTYPE_ARRAYU32)||(SDL_PIXELTYPE(format)==SDL_PIXELTYPE_ARRAYF16)||(SDL_PIXELTYPE(format)==SDL_PIXELTYPE_ARRAYF32)))
global macro  SDL_HINT_JOYSTICK_GAMECUBE_RUMBLE_BRAKE = "SDL_JOYSTICK_GAMECUBE_RUMBLE_BRAKE"
global macro  SDL_CreateThread(fn,name,data) = SDL_CreateThread(fn,name,data,(pfnSDL_CurrentBeginThread)SDL_beginthread,(pfnSDL_CurrentEndThread)SDL_endthread)
global macro  SDL_AUDIO_BITSIZE(x) = (x&SDL_AUDIO_MASK_BITSIZE)
global macro  SDL_HINT_MOUSE_RELATIVE_SPEED_SCALE = "SDL_MOUSE_RELATIVE_SPEED_SCALE"
global const SDL_VIDEO_DRIVER_WINDOWS = 1
global macro  SDL_PRIu64 = "I64u"
global const WINT_MAX = 65535
global macro  SDL_AtomicIncRef(a) =  SDL_AtomicAdd(a,1)
global macro  SDL_HINT_JOYSTICK_HIDAPI_PS4_RUMBLE = "SDL_JOYSTICK_HIDAPI_PS4_RUMBLE"
global const AUDIO_S16 = AUDIO_S16LSB
global macro  SDL_ISPIXELFORMAT_INDEXED(format) = (!SDL_ISPIXELFORMAT_FOURCC(format)&&((SDL_PIXELTYPE(format)==SDL_PIXELTYPE_INDEX1)||(SDL_PIXELTYPE(format)==SDL_PIXELTYPE_INDEX4)||(SDL_PIXELTYPE(format)==SDL_PIXELTYPE_INDEX8)))
global macro  SDL_HINT_WINDOWS_DPI_SCALING = "SDL_WINDOWS_DPI_SCALING"
global const SDL_RWOPS_STDFILE = 2U
global const SDL_RWOPS_MEMORY =  4U
global const AUDIO_S32 =  AUDIO_S32LSB
global const SDL_BYTEORDER =  SDL_LIL_ENDIAN
global const SDL_VIDEO_DRIVER_DUMMY =  1
global macro  SDL_FALLTHROUGH =  do{}while(0)
global const SDL_TIMER_WINDOWS = 1
global macro  SDL_WINDOWPOS_CENTERED_DISPLAY(X) = (SDL_WINDOWPOS_CENTERED_MASK|(X))
global const SDL_SIMD_ALIGNED = 0x00000008
global macro  SDL_TEXTINPUTEVENT_TEXT_SIZE = (32)
global macro  SDL_SwapLE16(X) = (X)
global const UINTPTR_MAX = 0xFFFFFFFFFFFFFFFF
global const HAVE_MMDEVICEAPI_H =  1
global macro  SDL_WINDOWPOS_UNDEFINED_DISPLAY(X) = (SDL_WINDOWPOS_UNDEFINED_MASK|(X))
global macro  SDL_AUDIO_MASK_ENDIAN = (1<<12)
global macro  SDL_HINT_TV_REMOTE_AS_JOYSTICK = "SDL_TV_REMOTE_AS_JOYSTICK"
global const SDL_JOYSTICK_AXIS_MAX = 32767
global macro  SDL_HINT_JOYSTICK_HIDAPI = "SDL_JOYSTICK_HIDAPI"
global const SDL_SWSURFACE = 0
global const SDL_VIDEO_OPENGL_WGL =  1
global macro  SDL_SwapLE32(X) = (X)
global macro  SDL_AUDIO_MASK_BITSIZE = (0xFF)
global macro  SDL_HINT_RENDER_DIRECT3D_THREADSAFE = "SDL_RENDER_DIRECT3D_THREADSAFE"
global macro  SDL_HAPTIC_SPRING = (1u<<7)
global const SDL_ALPHA_TRANSPARENT = 0
global macro  SDL_HINT_EMSCRIPTEN_KEYBOARD_ELEMENT = "SDL_EMSCRIPTEN_KEYBOARD_ELEMENT"
global macro  SDL_assert_release(condition) = SDL_enabled_assert(condition)
global macro  SDL_MOUSE_TOUCHID = ((Sint64)-1)
global const WINT_MIN = 0
global macro  SDL_HINT_JOYSTICK_HIDAPI_XBOX = "SDL_JOYSTICK_HIDAPI_XBOX"
global const HAVE_DXGI_H = 1
global macro  SDL_HINT_WAVE_TRUNCATION = "SDL_WAVE_TRUNCATION"
global macro  SDL_SwapLE64(X) = (X)
global const HAVE_WINAPIFAMILY_H = 0
global macro  SDL_HINT_VIDEO_WIN_D3DCOMPILER = "SDL_VIDEO_WIN_D3DCOMPILER"
global macro  SDL_SwapBE16(X) = SDL_Swap16(X)
global macro  SDL_TABLESIZE(table) = SDL_arraysize(table)
global macro  SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS = "SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS"
global macro  SDL_TICKS_PASSED(A,B) = ((Sint32)((B)-(A))<=0)
global macro  SDL_HINT_VIDEO_WAYLAND_ALLOW_LIBDECOR = "SDL_VIDEO_WAYLAND_ALLOW_LIBDECOR"
global const SDL_VIDEO_RENDER_D3D = 1
global const AUDIO_S32LSB =  0x8020
global macro  SDL_HINT_OPENGL_ES_DRIVER = "SDL_OPENGL_ES_DRIVER"
global macro  SDL_HAT_LEFTDOWN = (SDL_HAT_LEFT|SDL_HAT_DOWN)
global const AUDIO_U16MSB = 0x1010
global macro  SDL_SwapBE32(X) =  SDL_Swap32(X)
global const SDL_INIT_SENSOR = 0x00008000u
global const SDL_INLINE =  inline
global macro  SDL_JOYSTICK_AXIS_MIN = -32768
global macro  SDL_HINT_AUTO_UPDATE_JOYSTICKS = "SDL_AUTO_UPDATE_JOYSTICKS"
global macro  SDL_CreateThreadWithStackSize(fn,name,stacksize,data) = SDL_CreateThreadWithStackSize(fn,name,stacksize,data,(pfnSDL_CurrentBeginThread)SDL_beginthread,(pfnSDL_CurrentEndThread)SDL_endthread)
global const SDL_BUTTON_RIGHT = 3
global macro  SDL_const_cast(type,expression) = ((type)(expression))
global const EXTERN_C = extern
global macro  SDL_InvalidParamError(param) =  SDL_SetError("Parameter \'%s\' is invalid",(param))
global const SDL_MIX_MAXVOLUME = 128
global const SDL_PREALLOC =  0x00000001
global macro  SDL_HINT_RENDER_SCALE_QUALITY = "SDL_RENDER_SCALE_QUALITY"
global macro  SDL_SwapBE64(X) = SDL_Swap64(X)
global macro  SDL_HAT_RIGHTUP = (SDL_HAT_RIGHT|SDL_HAT_UP)
global const SDL_FILESYSTEM_WINDOWS = 1
global macro  SDL_FUNCTION = "???"
global macro  SDL_HINT_MOUSE_DOUBLE_CLICK_TIME = "SDL_MOUSE_DOUBLE_CLICK_TIME"
global macro  SDL_HINT_VIDEO_WAYLAND_EMULATE_MOUSE_WARP = "SDL_VIDEO_WAYLAND_EMULATE_MOUSE_WARP"
global macro  SDL_iconv_utf8_locale(S) = SDL_iconv_string("","UTF-8",S,SDL_strlen(S)+1)
global macro  SDL_HINT_TOUCH_MOUSE_EVENTS = "SDL_TOUCH_MOUSE_EVENTS"
global macro  SDL_PIXELLAYOUT(X) = (((X)>>16)&0x0F)
global const HAVE_STDINT_H = 1
global macro  SDL_HINT_WINDOW_NO_ACTIVATION_WHEN_SHOWN = "SDL_WINDOW_NO_ACTIVATION_WHEN_SHOWN"
global const SDL_assert_state = SDL_AssertState
global macro  SDL_MemoryBarrierRelease =  SDL_CompilerBarrier()
global const SDL_VIDEO_OPENGL_EGL = 1
global macro  SDL_HAT_LEFTUP = (SDL_HAT_LEFT|SDL_HAT_UP)
global const SDL_MINOR_VERSION = 26
global macro  SDL_HINT_GAMECONTROLLER_IGNORE_DEVICES = "SDL_GAMECONTROLLER_IGNORE_DEVICES"
global const SDL_HAPTIC_INFINITY = 4294967295U
global macro  SDL_SaveBMP(surface,file) =  SDL_SaveBMP_RW(surface,SDL_RWFromFile(file,"wb"),1)
global macro  SDL_HINT_ANDROID_TRAP_BACK_BUTTON = "SDL_ANDROID_TRAP_BACK_BUTTON"
global macro  SDL_QuitRequested = (SDL_PumpEvents(),(SDL_PeepEvents(NULL,0,SDL_PEEKEVENT,SDL_QUIT,SDL_QUIT)>0))
global macro  SDL_HINT_GAMECONTROLLER_IGNORE_DEVICES_EXCEPT = "SDL_GAMECONTROLLER_IGNORE_DEVICES_EXCEPT"
global const HAVE_DSOUND_H = 1
global macro  SDL_zeroa(x) =  SDL_memset((x),0,sizeof((x)))
global const SDL_endthread = _endthreadex
global macro  SDL_HINT_MOUSE_RELATIVE_WARP_MOTION = "SDL_MOUSE_RELATIVE_WARP_MOTION"
global macro  SDL_HINT_QUIT_ON_LAST_WINDOW_CLOSE = "SDL_QUIT_ON_LAST_WINDOW_CLOSE"
global macro  SDL_HINT_VIDEO_X11_FORCE_EGL = "SDL_VIDEO_X11_FORCE_EGL"
global macro  SDL_zerop(x) = SDL_memset((x),0,sizeof(*(x)))
global macro  SDL_HINT_JOYSTICK_HIDAPI_NINTENDO_CLASSIC = "SDL_JOYSTICK_HIDAPI_NINTENDO_CLASSIC"
global macro  SDL_HINT_VIDEO_WAYLAND_MODE_EMULATION = "SDL_VIDEO_WAYLAND_MODE_EMULATION"
global const SDL_IPHONE_MAX_GFORCE = 5.0
global const SDL_THREAD_GENERIC_COND_SUFFIX =  1
global macro  SDL_NONSHAPEABLE_WINDOW = -1
global const SDL_AUDIO_ALLOW_CHANNELS_CHANGE =  0x00000004
global macro  SDL_HAPTIC_PAUSE = (1u<<15)
global macro  SDL_HINT_FRAMEBUFFER_ACCELERATION = "SDL_FRAMEBUFFER_ACCELERATION"
global macro  SDL_BUTTON_X2MASK = SDL_BUTTON(SDL_BUTTON_X2)
global macro  SDL_BITSPERPIXEL(X) = (((X)>>8)&0xFF)
global macro  SDL_ISPIXELFORMAT_PACKED(format) = (!SDL_ISPIXELFORMAT_FOURCC(format)&&((SDL_PIXELTYPE(format)==SDL_PIXELTYPE_PACKED8)||(SDL_PIXELTYPE(format)==SDL_PIXELTYPE_PACKED16)||(SDL_PIXELTYPE(format)==SDL_PIXELTYPE_PACKED32)))
global macro  SDL_HINT_JOYSTICK_THREAD = "SDL_JOYSTICK_THREAD"
global const P_DETACH = 4
global macro  SDL_BUTTON_MMASK =  SDL_BUTTON(SDL_BUTTON_MIDDLE)
global macro  SDL_HINT_JOYSTICK_HIDAPI_SWITCH_PLAYER_LED = "SDL_JOYSTICK_HIDAPI_SWITCH_PLAYER_LED"
global macro  SDL_HINT_VIDEODRIVER = "SDL_VIDEODRIVER"
global const AUDIO_S32MSB = 0x9020
global macro  SDL_HINT_EVENT_LOGGING = "SDL_EVENT_LOGGING"
global macro  SDL_HINT_MAC_OPENGL_ASYNC_DISPATCH = "SDL_MAC_OPENGL_ASYNC_DISPATCH"
global macro  SDL_HINT_XINPUT_USE_OLD_JOYSTICK_MAPPING = "SDL_XINPUT_USE_OLD_JOYSTICK_MAPPING"
global const SDL_AUDIO_DRIVER_WINMM = 1
global const SDL_HAT_DOWN =  0x04
global macro  SDL_copyp(dst,src) = {SDL_COMPILE_TIME_ASSERT(SDL_copyp,sizeof(*(dst))==sizeof(*(src)));}SDL_memcpy((dst),(src),sizeof(*(src)))
global macro  SDL_AUDIO_ISFLOAT(x) = (x&SDL_AUDIO_MASK_DATATYPE)
global const __WIN32__ = 1
global macro  SDL_HINT_KMSDRM_DEVICE_INDEX = "SDL_KMSDRM_DEVICE_INDEX"
global const HAVE_TPCSHRD_H = 1
global macro  SDL_HINT_WINRT_HANDLE_BACK_BUTTON = "SDL_WINRT_HANDLE_BACK_BUTTON"
global const SDL_VIDEO_OPENGL_ES2 = 1
global macro  SDL_AUDIO_MASK_DATATYPE = (1<<8)
global macro  SDL_HINT_JOYSTICK_HIDAPI_STEAM = "SDL_JOYSTICK_HIDAPI_STEAM"
global macro  SDL_GetEventState(type) = SDL_EventState(type,SDL_QUERY)
global macro  SDL_HINT_WINDOWS_NO_CLOSE_ON_ALT_F4 = "SDL_WINDOWS_NO_CLOSE_ON_ALT_F4"
global const SDL_MUTEX_TIMEDOUT = 1
global macro  SDL_HINT_WAVE_RIFF_CHUNK_SIZE = "SDL_WAVE_RIFF_CHUNK_SIZE"
global macro  SDL_NULL_WHILE_LOOP_CONDITION = (0)
global macro  SDL_HINT_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR = "SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR"
global macro  SDL_HINT_AUDIO_DEVICE_STREAM_ROLE = "SDL_AUDIO_DEVICE_STREAM_ROLE"
global macro  SDL_FORCE_INLINE = static SDL_INLINE
global macro  SDL_HINT_ANDROID_APK_EXPANSION_MAIN_FILE_VERSION = "SDL_ANDROID_APK_EXPANSION_MAIN_FILE_VERSION"
global const SDL_RLEACCEL = 0x00000002
global const AUDIO_U16 =  AUDIO_U16LSB
global macro  SDL_MAX_UINT8 = ((Uint8)0xFF)
global macro  SDL_BUTTON_X1MASK = SDL_BUTTON(SDL_BUTTON_X1)
global macro  SDL_HINT_WINDOWS_ENABLE_MESSAGELOOP = "SDL_WINDOWS_ENABLE_MESSAGELOOP"
global const SIZEOF_VOIDP = 4
global macro  SDL_ISPIXELFORMAT_ALPHA(format) = ((SDL_ISPIXELFORMAT_PACKED(format)&&((SDL_PIXELORDER(format)==SDL_PACKEDORDER_ARGB)||(SDL_PIXELORDER(format)==SDL_PACKEDORDER_RGBA)||(SDL_PIXELORDER(format)==SDL_PACKEDORDER_ABGR)||(SDL_PIXELORDER(format)==SDL_PACKEDORDER_BGRA)))||(SDL_ISPIXELFORMAT_ARRAY(format)&&((SDL_PIXELORDER(format)==SDL_ARRAYORDER_ARGB)||(SDL_PIXELORDER(format)==SDL_ARRAYORDER_RGBA)||(SDL_PIXELORDER(format)==SDL_ARRAYORDER_ABGR)||(SDL_PIXELORDER(format)==SDL_ARRAYORDER_BGRA))))
global macro  SDL_INVALID_SHAPE_ARGUMENT = -2
global macro  SDL_BUTTON_LMASK =  SDL_BUTTON(SDL_BUTTON_LEFT)
global macro  SDL_disabled_assert(condition) = do{(void)sizeof((condition));}while(SDL_NULL_WHILE_LOOP_CONDITION)
global macro  SDL_MIN_UINT16 = ((Uint16)0x0000)
global const AUDIO_S8 = 0x8008
global const SDL_SIZE_MAX =  SIZE_MAX
global const SDL_STANDARD_GRAVITY =  9.80665f
global const _P_OVERLAY =  P_OVERLAY
global macro  SDL_PIXELTYPE(X) = (((X)>>24)&0x0F)
global macro  SDL_ICONV_EINVAL = (size_t)-4
global macro  SDL_MUSTLOCK(S) = (((S)->flags&SDL_RLEACCEL)!=0)
global macro  SDL_HINT_QTWAYLAND_CONTENT_ORIENTATION = "SDL_QTWAYLAND_CONTENT_ORIENTATION"
global const P_WAIT = 0
global macro  SDL_HINT_VIDEO_X11_WINDOW_VISUALID = "SDL_VIDEO_X11_WINDOW_VISUALID"
global macro  SDL_MIN_UINT32 = ((Uint32)0x00000000)
global const AUDIO_U8 = 0x0008
global const AUDIO_F32SYS =  AUDIO_F32LSB
global const SDL_AUDIO_DRIVER_DUMMY =  1
global macro  SDL_BUTTON(X) = (1<<((X)-1))
global const SDL_AUDIO_ALLOW_FREQUENCY_CHANGE = 0x00000001
global macro  SDL_COMPILE_TIME_ASSERT(name,x) =  typedef int SDL_compile_time_assert_##name[(x)*2-1]
global const spawnvp = _spawnvp
global macro  SDL_max(x,y) = (((x)>(y))?(x):(y))
global macro  UINTMAX_C(a) = (a##ULL)
global macro  SDL_MIN_UINT64 = ((Uint64)(0x0000000000000000ull))
global macro  va_arg(ap,t) = *(t*)((ap+=8)-8)
global macro  SDL_MUTEX_MAXWAIT = (~(Uint32)0)
global macro  SDL_OutOfMemory = SDL_Error(SDL_ENOMEM)
global const _P_WAIT = P_WAIT
global const WCHAR_MAX =  65535
global macro  va_start(ap,v) =  ap=((va_list)&v+8)
global macro  SDL_PRIx32 = "x"
global const UINT32_MAX = 0xFFFFFFFF
global const HAVE_SENSORSAPI_H =  1
global macro  SDL_BYTESPERPIXEL(X) = (SDL_ISPIXELFORMAT_FOURCC(X)?((((X)==SDL_PIXELFORMAT_YUY2)||((X)==SDL_PIXELFORMAT_UYVY)||((X)==SDL_PIXELFORMAT_YVYU))?2:1):(((X)>>0)&0xFF))
global const INT8_MAX = 127
global const HAVE_XINPUT_H =  1
global macro  SDL_HINT_AUDIO_CATEGORY = "SDL_AUDIO_CATEGORY"
global macro  SDL_HINT_MOUSE_RELATIVE_SCALING = "SDL_MOUSE_RELATIVE_SCALING"
global macro  SDL_HINT_RENDER_LINE_METHOD = "SDL_RENDER_LINE_METHOD"
global const SDL_RWOPS_MEMORY_RO = 5U
global macro  SDL_HINT_VIDEO_X11_XVIDMODE = "SDL_VIDEO_X11_XVIDMODE"
global macro  SDL_min(x,y) = (((x)<(y))?(x):(y))
global macro  SDL_PRIx64 = "I64x"
global const SDL_IGNORE = 0
global macro  SDL_LoadBMP(file) =  SDL_LoadBMP_RW(SDL_RWFromFile(file,"rb"),1)
global const SDL_HAT_RIGHT = 0x02
global macro  SDL_HINT_IME_SHOW_UI = "SDL_IME_SHOW_UI"
global macro  SDL_WINDOW_LACKS_SHAPE = -3
global macro  SDL_HINT_LINUX_JOYSTICK_DEADZONES = "SDL_LINUX_JOYSTICK_DEADZONES"
global macro  SDL_MAX_SINT8 = ((Sint8)0x7F)
global macro  SDL_HINT_JOYSTICK_HIDAPI_PS5_RUMBLE = "SDL_JOYSTICK_HIDAPI_PS5_RUMBLE"
global macro  SDL_HINT_IDLE_TIMER_DISABLED = "SDL_IOS_IDLE_TIMER_DISABLED"
global const SDL_INIT_HAPTIC = 0x00001000u
global macro  SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH = "SDL_MOUSE_FOCUS_CLICKTHROUGH"
global macro  SDL_HAPTIC_CUSTOM = (1u<<11)
global macro  SDL_HINT_VITA_TOUCH_MOUSE_DEVICE = "SDL_HINT_VITA_TOUCH_MOUSE_DEVICE"
global const WCHAR_MIN = 0
global macro  SDL_assert_always(condition) =  SDL_enabled_assert(condition)
global const SDL_AUDIO_ALLOW_FORMAT_CHANGE = 0x00000002
global const INT32_MAX =  0x7FFFFFFF
global macro  SDL_HINT_AUDIO_DEVICE_STREAM_NAME = "SDL_AUDIO_DEVICE_STREAM_NAME"
global const SDL_RWOPS_JNIFILE = 3U
global macro  SDL_VERSION_ATLEAST(X,Y,Z) = ((SDL_MAJOR_VERSION>=X)&&(SDL_MAJOR_VERSION>X||SDL_MINOR_VERSION>=Y)&&(SDL_MAJOR_VERSION>X||SDL_MINOR_VERSION>Y||SDL_PATCHLEVEL>=Z))
global const SDL_DISABLE = 0
global const SDL_RWOPS_WINFILE =  1U
global const SDL_FILE =  __FILE__
global macro  SDL_HINT_MAC_BACKGROUND_APP = "SDL_MAC_BACKGROUND_APP"
global macro  INT8_MIN = -128
global macro  SDL_HAPTIC_INERTIA = (1u<<9)
global const SDL_INIT_GAMECONTROLLER = 0x00002000u
global const SDL_BUTTON_X1 =  4
global macro  SDL_HINT_X11_WINDOW_TYPE = "SDL_X11_WINDOW_TYPE"
global const SDL_BUTTON_X2 = 5
global const SDL_INIT_JOYSTICK =  0x00000200u
global const AUDIO_S16SYS =  AUDIO_S16LSB
global macro  SDL_HINT_VIDEO_MAC_FULLSCREEN_SPACES = "SDL_VIDEO_MAC_FULLSCREEN_SPACES"
global const SDL_VIRTUAL_JOYSTICK_DESC_VERSION = 1
global macro  SDL_AUDIO_ISINT(x) = (!SDL_AUDIO_ISFLOAT(x))
global macro  SDL_HINT_PREFERRED_LOCALES = "SDL_PREFERRED_LOCALES"
global macro  SDL_HINT_MOUSE_AUTO_CAPTURE = "SDL_MOUSE_AUTO_CAPTURE"
global macro  SDL_WINDOWPOS_CENTERED = SDL_WINDOWPOS_CENTERED_DISPLAY(0)
global const SDL_VIDEO_RENDER_OGL = 1
global macro  SDL_HINT_X11_FORCE_OVERRIDE_REDIRECT = "SDL_X11_FORCE_OVERRIDE_REDIRECT"
global const INT32_MIN = 0x80000000
global const _P_NOWAIT =  P_NOWAIT
global macro  SDL_HINT_JOYSTICK_HIDAPI_XBOX_ONE_HOME_LED = "SDL_JOYSTICK_HIDAPI_XBOX_ONE_HOME_LED"
global const HAVE_DDRAW_H = 1
global macro  SDL_PIXELFLAG(X) = (((X)>>28)&0x0F)
global macro  SDL_mutexP(m) = SDL_LockMutex(m)
global const RW_SEEK_SET = 0
global macro  SDL_HAT_RIGHTDOWN = (SDL_HAT_RIGHT|SDL_HAT_DOWN)
global macro  SDL_mutexV(m) = SDL_UnlockMutex(m)
global macro  SDL_HINT_VIDEO_ALLOW_SCREENSAVER = "SDL_VIDEO_ALLOW_SCREENSAVER"
global macro  SDL_MAX_SINT16 = ((Sint16)0x7FFF)
global macro  SDL_HINT_ENABLE_STEAM_CONTROLLERS = "SDL_ENABLE_STEAM_CONTROLLERS"
global macro  SDL_HINT_SCREENSAVER_INHIBIT_ACTIVITY_NAME = "SDL_SCREENSAVER_INHIBIT_ACTIVITY_NAME"
global macro  SDL_HAPTIC_AUTOCENTER = (1u<<13)
global macro  SDL_HINT_ACCELEROMETER_AS_JOYSTICK = "SDL_ACCELEROMETER_AS_JOYSTICK"
global macro  SDL_HINT_JOYSTICK_HIDAPI_JOYCON_HOME_LED = "SDL_JOYSTICK_HIDAPI_JOYCON_HOME_LED"
global macro  SDL_HINT_RENDER_VSYNC = "SDL_RENDER_VSYNC"
global macro  SDL_INIT_EVERYTHING = (SDL_INIT_TIMER|SDL_INIT_AUDIO|SDL_INIT_VIDEO|SDL_INIT_EVENTS|SDL_INIT_JOYSTICK|SDL_INIT_HAPTIC|SDL_INIT_GAMECONTROLLER|SDL_INIT_SENSOR)
global macro  SDL_MAX_SINT32 = ((Sint32)0x7FFFFFFF)
global const SDL_MAX_LOG_MESSAGE = 4096
global macro  SDL_HAPTIC_SAWTOOTHUP = (1u<<4)
global macro  SDL_assert_paranoid(condition) = SDL_disabled_assert(condition)
global const SDL_ALPHA_OPAQUE = 255
global const SDL_JOYSTICK_VIRTUAL =  1
global macro  SDL_HINT_THREAD_FORCE_REALTIME_TIME_CRITICAL = "SDL_THREAD_FORCE_REALTIME_TIME_CRITICAL"
global const SDL_HAPTIC_POLAR = 0
global macro  SDL_HINT_WINDOWS_FORCE_MUTEX_CRITICAL_SECTIONS = "SDL_WINDOWS_FORCE_MUTEX_CRITICAL_SECTIONS"
global macro  SDL_HINT_RENDER_BATCHING = "SDL_RENDER_BATCHING"
global const SDL_BIG_ENDIAN = 4321
global const execve =  _execve
global macro  SDL_HINT_JOYSTICK_HIDAPI_WII = "SDL_JOYSTICK_HIDAPI_WII"
global macro  SDL_MAX_SINT64 = ((Sint64)0x7FFFFFFFFFFFFFFFll)
global macro  SDL_HINT_JOYSTICK_HIDAPI_STADIA = "SDL_JOYSTICK_HIDAPI_STADIA"
global macro  SDL_HINT_GAMECONTROLLERCONFIG_FILE = "SDL_GAMECONTROLLERCONFIG_FILE"
global macro  SDL_HAPTIC_RAMP = (1u<<6)
global macro  SDL_HINT_JOYSTICK_RAWINPUT = "SDL_JOYSTICK_RAWINPUT"
global macro  SDL_HINT_VIDEO_WAYLAND_PREFER_LIBDECOR = "SDL_VIDEO_WAYLAND_PREFER_LIBDECOR"
global macro  SDL_HINT_MOUSE_NORMAL_SPEED_SCALE = "SDL_MOUSE_NORMAL_SPEED_SCALE"
global macro  UINT64_C(x) = (x##ull)
global macro  SDL_HINT_JOYSTICK_HIDAPI_XBOX_360_WIRELESS = "SDL_JOYSTICK_HIDAPI_XBOX_360_WIRELESS"
global macro  SDL_HINT_GAMECONTROLLERCONFIG = "SDL_GAMECONTROLLERCONFIG"
global macro  SDL_HINT_VIDEO_EXTERNAL_CONTEXT = "SDL_VIDEO_EXTERNAL_CONTEXT"
global const AUDIO_F32LSB = 0x8120
global const SDL_AUDIO_DRIVER_DSOUND =  1
global const M_PI =  3.14159265358979323846264338327950288
global macro  SDL_AUDIO_MASK_SIGNED = (1<<15)
global macro  SDL_assert(condition) = SDL_disabled_assert(condition)
global macro  SDL_MemoryBarrierAcquire = SDL_CompilerBarrier()
global macro  SDL_LoadWAV(file,spec,audio_buf,audio_len) = SDL_LoadWAV_RW(SDL_RWFromFile(file,"rb"),1,spec,audio_buf,audio_len)
global const SDL_HAPTIC_SPHERICAL = 2
global const SDL_AUDIO_DRIVER_DISK =  1
global macro  SDL_HINT_DISPLAY_USABLE_BOUNDS = "SDL_DISPLAY_USABLE_BOUNDS"
global macro  SDL_AtomicDecRef(a) = (SDL_AtomicAdd(a,-1)==1)
global macro  SDL_HINT_RPI_VIDEO_LAYER = "SDL_RPI_VIDEO_LAYER"
global macro  SDL_VERSION(x) = {(x)->major=SDL_MAJOR_VERSION;(x)->minor=SDL_MINOR_VERSION;(x)->patch=SDL_PATCHLEVEL;}
global macro  SDL_HINT_ALLOW_TOPMOST = "SDL_ALLOW_TOPMOST"
global const SDL_INIT_EVENTS = 0x00004000u
global const P_NOWAIT =  1
global const SDL_INIT_VIDEO =  0x00000020u
global macro  SDL_HINT_LINUX_JOYSTICK_CLASSIC = "SDL_LINUX_JOYSTICK_CLASSIC"
global macro  SDL_HINT_VIDEO_EGL_ALLOW_TRANSPARENCY = "SDL_VIDEO_EGL_ALLOW_TRANSPARENCY"
global macro  SDL_HINT_WAVE_FACT_CHUNK = "SDL_WAVE_FACT_CHUNK"
global macro  SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT = "SDL_VIDEO_WINDOW_SHARE_PIXEL_FORMAT"
global macro  SDL_HAPTIC_TRIANGLE = (1u<<3)
global macro  SDL_AUDIO_ISBIGENDIAN(x) = (x&SDL_AUDIO_MASK_ENDIAN)
global macro  SDL_HINT_MOUSE_DOUBLE_CLICK_RADIUS = "SDL_MOUSE_DOUBLE_CLICK_RADIUS"
global const SDL_PATCHLEVEL = 5
global macro  SDL_CompilerBarrier = {SDL_SpinLock _tmp=0;SDL_AtomicLock(&_tmp);SDL_AtomicUnlock(&_tmp);}
global const SDL_AUDIO_DRIVER_WASAPI = 1
global macro  SDL_HINT_WINDOWS_INTRESOURCE_ICON = "SDL_WINDOWS_INTRESOURCE_ICON"
global const HAVE_STDDEF_H = 1
global macro  SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK = "SDL_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK"
global macro  SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING = "SDL_WINDOWS_DISABLE_THREAD_NAMING"
global macro  SDL_WINDOWPOS_UNDEFINED = SDL_WINDOWPOS_UNDEFINED_DISPLAY(0)
global macro  SDL_HINT_EMSCRIPTEN_ASYNCIFY = "SDL_EMSCRIPTEN_ASYNCIFY"
global macro  SDL_HAPTIC_DAMPER = (1u<<8)
global macro  SDL_HINT_RENDER_OPENGL_SHADERS = "SDL_RENDER_OPENGL_SHADERS"
global macro  SDL_HINT_NO_SIGNAL_HANDLERS = "SDL_NO_SIGNAL_HANDLERS"
global macro  SDL_HINT_RENDER_LOGICAL_SIZE_MODE = "SDL_RENDER_LOGICAL_SIZE_MODE"
global macro  SDL_HINT_GAMECONTROLLERTYPE = "SDL_GAMECONTROLLERTYPE"
global const PTRDIFF_MAX = 0x7FFFFFFFFFFFFFFF
global const HAVE_STDARG_H =  1
global macro  SDL_HINT_VIDEO_X11_XINERAMA = "SDL_VIDEO_X11_XINERAMA"
global macro  SDL_HINT_PS2_DYNAMIC_VSYNC = "SDL_PS2_DYNAMIC_VSYNC"
global const SDL_LOADSO_WINDOWS = 1
global macro  SDL_HINT_RENDER_DRIVER = "SDL_RENDER_DRIVER"
global macro  va_copy(dest,src) = (dest=src)
global const SDL_Colour = SDL_Color
global macro  SDL_HINT_IOS_HIDE_HOME_INDICATOR = "SDL_IOS_HIDE_HOME_INDICATOR"
global macro  SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS = "SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS"
global macro  SDL_HINT_JOYSTICK_HIDAPI_JOY_CONS = "SDL_JOYSTICK_HIDAPI_JOY_CONS"
global macro  SDLK_SCANCODE_MASK = (1<<30)
global const SDL_CACHELINE_SIZE = 128
global const SDL_INIT_TIMER =  0x00000001u
global const SDL_JOYSTICK_DINPUT =  1
global const SDL_THREAD_WINDOWS =  1
global const AUDIO_F32MSB =  0x9120
global macro  SDL_HINT_QTWAYLAND_WINDOW_FLAGS = "SDL_QTWAYLAND_WINDOW_FLAGS"
global macro  SDL_HINT_ANDROID_BLOCK_ON_PAUSE = "SDL_ANDROID_BLOCK_ON_PAUSE"
global const AUDIO_S16LSB = 0x8010
global const SDL_VIDEO_VULKAN =  1
global macro  SDL_HINT_TRACKPAD_IS_TOUCH_ONLY = "SDL_TRACKPAD_IS_TOUCH_ONLY"
global macro  SDL_SwapFloatBE(X) = SDL_SwapFloat(X)
global macro  SDL_HINT_THREAD_PRIORITY_POLICY = "SDL_THREAD_PRIORITY_POLICY"
global const SDL_HAT_CENTERED = 0x00
global macro  SDL_HAPTIC_LEFTRIGHT = (1u<<2)
global const SDL_WINDOWPOS_CENTERED_MASK = 0x2FFF0000u
global macro  SDL_MIN_UINT8 = ((Uint8)0x00)
global const SDL_AUDIOCVT_MAX_FILTERS = 9
global macro  SDL_iconv_utf8_ucs2(S) = (Uint16*)SDL_iconv_string("UCS-2-INTERNAL","UTF-8",S,SDL_strlen(S)+1)
global const SDL_FLT_EPSILON = 1.1920928955078125e-07F
global macro  SDL_iconv_utf8_ucs4(S) = (Uint32*)SDL_iconv_string("UCS-4-INTERNAL","UTF-8",S,SDL_strlen(S)+1)
global const SDL_JOYSTICK_XINPUT = 1
global const PTRDIFF_MIN =  0x8000000000000000
global macro  SDL_HINT_JOYSTICK_HIDAPI_XBOX_360_PLAYER_LED = "SDL_JOYSTICK_HIDAPI_XBOX_360_PLAYER_LED"
global macro  SDL_ICONV_ERROR = (size_t)-1
global macro  SDL_HINT_THREAD_STACK_SIZE = "SDL_THREAD_STACK_SIZE"
global macro  offsetof(a,b) = (size_t)&(((a*)0)->b)
global macro  SDL_HINT_DIRECTINPUT_ENABLED = "SDL_DIRECTINPUT_ENABLED"
global const WINAPI_FAMILY_WINRT = 0
global macro  SDL_HINT_LINUX_DIGITAL_HATS = "SDL_LINUX_DIGITAL_HATS"
global const SDL_VIDEO_OPENGL = 1
global macro  SDL_HINT_AUDIODRIVER = "SDL_AUDIODRIVER"
global macro  SDL_DEFINE_PIXELFORMAT(type,order,layout,bits,bytes) = ((1<<28)|((type)<<24)|((order)<<20)|((layout)<<16)|((bits)<<8)|((bytes)<<0))
global macro  SDL_AUDIO_ISUNSIGNED(x) = (!SDL_AUDIO_ISSIGNED(x))
global const SDL_BUTTON_MIDDLE = 2
global macro  SDL_HINT_HIDAPI_IGNORE_DEVICES = "SDL_HIDAPI_IGNORE_DEVICES"
global macro  SDL_HINT_VIDEO_X11_XRANDR = "SDL_VIDEO_X11_XRANDR"
global macro  SDL_HINT_JOYSTICK_HIDAPI_PS5_PLAYER_LED = "SDL_JOYSTICK_HIDAPI_PS5_PLAYER_LED"
global macro  SDL_HINT_WINRT_PRIVACY_POLICY_LABEL = "SDL_WINRT_PRIVACY_POLICY_LABEL"
global const SDL_LIL_ENDIAN = 1234
global const AUDIO_F32 =  AUDIO_F32LSB
global macro  SDL_HINT_WINDOWS_DPI_AWARENESS = "SDL_WINDOWS_DPI_AWARENESS"
global macro  SDL_zero(x) = SDL_memset(&(x),0,sizeof((x)))
global macro  SDL_SwapFloatLE(X) = (X)
global const cwait = _cwait
global macro  SDL_COMPILEDVERSION =  SDL_VERSIONNUM(SDL_MAJOR_VERSION,SDL_MINOR_VERSION,SDL_PATCHLEVEL)
global macro  SDL_HINT_ANDROID_APK_EXPANSION_PATCH_FILE_VERSION = "SDL_ANDROID_APK_EXPANSION_PATCH_FILE_VERSION"
global macro  SDL_HINT_JOYSTICK_ROG_CHAKRAM = "SDL_JOYSTICK_ROG_CHAKRAM"
global const SDL_PRESSED = 1
global macro  SDL_HINT_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN = "SDL_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN"
global macro  va_end(ap) = (ap=(va_list)0)
global macro  INTMAX_C(a) = (a##LL)
global macro  SDL_HINT_WINDOWS_USE_D3D9EX = "SDL_WINDOWS_USE_D3D9EX"
global const SDL_INIT_NOPARACHUTE = 0x00100000u
global macro  SDL_HINT_JOYSTICK_RAWINPUT_CORRELATE_XINPUT = "SDL_JOYSTICK_RAWINPUT_CORRELATE_XINPUT"
global const SDL_SENSOR_WINDOWS = 1
global macro  SDL_HINT_VIDEO_HIGHDPI_DISABLED = "SDL_VIDEO_HIGHDPI_DISABLED"
global macro  SDL_FOURCC(A,B,C,D) = ((SDL_static_cast(Uint32,SDL_static_cast(Uint8,(A)))<<0)|(SDL_static_cast(Uint32,SDL_static_cast(Uint8,(B)))<<8)|(SDL_static_cast(Uint32,SDL_static_cast(Uint8,(C)))<<16)|(SDL_static_cast(Uint32,SDL_static_cast(Uint8,(D)))<<24))
global const AUDIO_S16MSB = 0x9010
global macro  SDL_HINT_JOYSTICK_HIDAPI_XBOX_360 = "SDL_JOYSTICK_HIDAPI_XBOX_360"
global macro  SDL_HINT_MOUSE_RELATIVE_MODE_WARP = "SDL_MOUSE_RELATIVE_MODE_WARP"
global const __inline__ = inline
global macro  SDL_HINT_POLL_SENTINEL = "SDL_POLL_SENTINEL"
global const SDL_FLOATWORDORDER = SDL_BYTEORDER
global macro  SDL_GameControllerAddMappingsFromFile(file) =  SDL_GameControllerAddMappingsFromRW(SDL_RWFromFile(file,"rb"),1)
global const UINT16_MAX = 65535
global macro  SDL_HINT_MOUSE_RELATIVE_MODE_CENTER = "SDL_MOUSE_RELATIVE_MODE_CENTER"
global const SDL_JOYSTICK_RAWINPUT = 1
global macro  SDL_MIN_SINT8 = ((Sint8)(~0x7F))
global macro  SDL_HINT_VIDEO_FOREIGN_WINDOW_VULKAN = "SDL_VIDEO_FOREIGN_WINDOW_VULKAN"
global macro  SDL_HINT_FORCE_RAISEWINDOW = "SDL_HINT_FORCE_RAISEWINDOW"
global const SDL_AUDIO_ALLOW_SAMPLES_CHANGE = 0x00000008
global const P_OVERLAY =  2
global const SDLCALL =  __cdecl
global macro  SDL_HAPTIC_FRICTION = (1u<<10)
global macro  SDL_AUDIO_ISSIGNED(x) = (x&SDL_AUDIO_MASK_SIGNED)
global macro  SDL_HINT_JOYSTICK_HIDAPI_SWITCH = "SDL_JOYSTICK_HIDAPI_SWITCH"
global macro  SDL_clamp(x,a,b) = (((x)<(a))?(a):(((x)>(b))?(b):(x)))
global macro  SDL_stack_free(data) = SDL_free(data)
global macro  SDL_HINT_APPLE_TV_CONTROLLER_UI_EVENTS = "SDL_APPLE_TV_CONTROLLER_UI_EVENTS"
global const INT16_MAX = 32767
global macro  SDL_HINT_MOUSE_TOUCH_EVENTS = "SDL_MOUSE_TOUCH_EVENTS"
global macro  SDL_HINT_WINDOWS_INTRESOURCE_ICON_SMALL = "SDL_WINDOWS_INTRESOURCE_ICON_SMALL"
global macro  SDL_HINT_APPLE_TV_REMOTE_ALLOW_ROTATION = "SDL_APPLE_TV_REMOTE_ALLOW_ROTATION"
global const SDL_WINDOWPOS_UNDEFINED_MASK = 0x1FFF0000u
global const INTPTR_MAX =  0x7FFFFFFFFFFFFFFF
global macro  SDL_HINT_VIDEO_FOREIGN_WINDOW_OPENGL = "SDL_VIDEO_FOREIGN_WINDOW_OPENGL"
global const SDL_HAT_UP = 0x01
global macro  SDL_HINT_VIDEO_X11_NET_WM_PING = "SDL_VIDEO_X11_NET_WM_PING"
global const SDL_BlitSurface = SDL_UpperBlit
global const SDL_MAJOR_VERSION =  2
global macro  SDL_HAPTIC_SAWTOOTHDOWN = (1u<<5)
global macro  SDL_HINT_VIDEO_DOUBLE_BUFFER = "SDL_VIDEO_DOUBLE_BUFFER"
global const SDL_VIDEO_RENDER_OGL_ES2 = 1
global macro  SDL_HINT_JOYSTICK_HIDAPI_WII_PLAYER_LED = "SDL_JOYSTICK_HIDAPI_WII_PLAYER_LED"
global macro  SDL_HINT_JOYSTICK_HIDAPI_PS3 = "SDL_JOYSTICK_HIDAPI_PS3"
global macro  SDL_HINT_JOYSTICK_HIDAPI_PS4 = "SDL_JOYSTICK_HIDAPI_PS4"
global macro  SDL_HINT_JOYSTICK_HIDAPI_PS5 = "SDL_JOYSTICK_HIDAPI_PS5"
global macro  SDL_SCANCODE_TO_KEYCODE(X) = (X|SDLK_SCANCODE_MASK)
global macro  SDL_ICONV_EILSEQ = (size_t)-3
global macro  SDL_ICONV_E2BIG = (size_t)-2
global macro  SDL_HINT_RENDER_DIRECT3D11_DEBUG = "SDL_RENDER_DIRECT3D11_DEBUG"
global const __WINDOWS__ = 1
global macro  SDL_PRIs32 = "d"
global const AUDIO_U16SYS = AUDIO_U16LSB
global const SDL_JOYSTICK_HIDAPI =  1
global const SDL_RELEASED =  0
global macro  SDL_HINT_MOUSE_RELATIVE_SYSTEM_SCALE = "SDL_MOUSE_RELATIVE_SYSTEM_SCALE"
global macro  SDL_QUERY = -1
global macro  SDL_HINT_RETURN_KEY_HIDES_IME = "SDL_RETURN_KEY_HIDES_IME"
global const SDL_HAT_LEFT = 0x08
global macro  INT16_MIN = -32768
global const SDL_beginthread =  _beginthreadex
global macro  SDL_PRIs64 = "I64d"
global macro  SDL_static_cast(type,expression) = ((type)(expression))
global macro  SDL_reinterpret_cast(type,expression) = ((type)(expression))
global macro  SDL_HINT_ANDROID_BLOCK_ON_PAUSE_PAUSEAUDIO = "SDL_ANDROID_BLOCK_ON_PAUSE_PAUSEAUDIO"
global const INTPTR_MIN = 0x8000000000000000
global const UINT64_MAX =  0xFFFFFFFFFFFFFFFF
global macro  SDL_HINT_WINRT_PRIVACY_POLICY_URL = "SDL_WINRT_PRIVACY_POLICY_URL"
global macro  SDL_HINT_JOYSTICK_HIDAPI_GAMECUBE = "SDL_JOYSTICK_HIDAPI_GAMECUBE"
global const SDL_INIT_AUDIO = 0x00000010u
global macro  SDL_HINT_TIMER_RESOLUTION = "SDL_TIMER_RESOLUTION"
global const _SDL_HAS_BUILTIN(x) = 0
global macro  SDL_MAX_UINT16 = ((Uint16)0xFFFF)
global macro  SDL_WINDOWPOS_ISCENTERED(X) = (((X)&0xFFFF0000)==SDL_WINDOWPOS_CENTERED_MASK)
global const RW_SEEK_CUR = 1
global macro  SDL_HINT_GAMECONTROLLER_USE_BUTTON_LABELS = "SDL_GAMECONTROLLER_USE_BUTTON_LABELS"
global const SDL_HAPTIC_DINPUT = 1
global macro  SDL_HINT_IME_SUPPORT_EXTENDED_TEXT = "SDL_IME_SUPPORT_EXTENDED_TEXT"
global macro  SDL_HINT_JOYSTICK_HIDAPI_SWITCH_HOME_LED = "SDL_JOYSTICK_HIDAPI_SWITCH_HOME_LED"
global const SDL_POWER_WINDOWS = 1
global macro  SDL_STRINGIFY_ARG(arg) = #arg
