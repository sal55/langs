importdll stb_image =
    const STBI_default                       = 0
    const STBI_grey                          = 1
    const STBI_grey_alpha                    = 2
    const STBI_rgb                           = 3
    const STBI_rgb_alpha                     = 4
    record $T3 =
        var ref clang function(ref void,ref i8,i32)i32 read$
        var ref clang proc(ref void,i32) skip
        var ref clang function(ref void)i32 eof
    end

    clang function "stbi_load_from_memory"             (ref byte,i32,ref i32,ref i32,ref i32,i32)ref byte
    clang function "stbi_load_from_callbacks"          (ref $T3,ref void,ref i32,ref i32,ref i32,i32)ref byte
    clang function "stbi_load"                         (ref i8,ref i32,ref i32,ref i32,i32)ref byte
    clang function "stbi_load_from_file"               (ref $T1,ref i32,ref i32,ref i32,i32)ref byte
    clang function "stbi_load_gif_from_memory"         (ref byte,i32,ref ref i32,ref i32,ref i32,ref i32,ref i32,i32)ref byte
    clang function "stbi_load_16_from_memory"          (ref byte,i32,ref i32,ref i32,ref i32,i32)ref u16
    clang function "stbi_load_16_from_callbacks"       (ref $T3,ref void,ref i32,ref i32,ref i32,i32)ref u16
    clang function "stbi_load_16"                      (ref i8,ref i32,ref i32,ref i32,i32)ref u16
    clang function "stbi_load_from_file_16"            (ref $T1,ref i32,ref i32,ref i32,i32)ref u16
    clang function "stbi_loadf_from_memory"            (ref byte,i32,ref i32,ref i32,ref i32,i32)ref r32
    clang function "stbi_loadf_from_callbacks"         (ref $T3,ref void,ref i32,ref i32,ref i32,i32)ref r32
    clang function "stbi_loadf"                        (ref i8,ref i32,ref i32,ref i32,i32)ref r32
    clang function "stbi_loadf_from_file"              (ref $T1,ref i32,ref i32,ref i32,i32)ref r32
    clang proc     "stbi_hdr_to_ldr_gamma"             (r32)
    clang proc     "stbi_hdr_to_ldr_scale"             (r32)
    clang proc     "stbi_ldr_to_hdr_gamma"             (r32)
    clang proc     "stbi_ldr_to_hdr_scale"             (r32)
    clang function "stbi_is_hdr_from_callbacks"        (ref $T3,ref void)i32
    clang function "stbi_is_hdr_from_memory"           (ref byte,i32)i32
    clang function "stbi_is_hdr"                       (ref i8)i32
    clang function "stbi_is_hdr_from_file"             (ref $T1)i32
    clang function "stbi_failure_reason"               ()ref i8
    clang proc     "stbi_image_free"                   (ref void)
    clang function "stbi_info_from_memory"             (ref byte,i32,ref i32,ref i32,ref i32)i32
    clang function "stbi_info_from_callbacks"          (ref $T3,ref void,ref i32,ref i32,ref i32)i32
    clang function "stbi_is_16_bit_from_memory"        (ref byte,i32)i32
    clang function "stbi_is_16_bit_from_callbacks"     (ref $T3,ref void)i32
    clang function "stbi_info"                         (ref i8,ref i32,ref i32,ref i32)i32
    clang function "stbi_info_from_file"               (ref $T1,ref i32,ref i32,ref i32)i32
    clang function "stbi_is_16_bit"                    (ref i8)i32
    clang function "stbi_is_16_bit_from_file"          (ref $T1)i32
    clang proc     "stbi_set_unpremultiply_on_load"    (i32)
    clang proc     "stbi_convert_iphone_png_to_rgb"    (i32)
    clang proc     "stbi_set_flip_vertically_on_load"  (i32)
    clang proc     "stbi_set_unpremultiply_on_load_thread" (i32)
    clang proc     "stbi_convert_iphone_png_to_rgb_thread" (i32)
    clang proc     "stbi_set_flip_vertically_on_load_thread" (i32)
    clang function "stbi_zlib_decode_malloc_guesssize" (ref i8,i32,i32,ref i32)ref i8
    clang function "stbi_zlib_decode_malloc_guesssize_headerflag" (ref i8,i32,i32,ref i32,i32)ref i8
    clang function "stbi_zlib_decode_malloc"           (ref i8,i32,ref i32)ref i8
    clang function "stbi_zlib_decode_buffer"           (ref i8,i32,ref i8,i32)i32
    clang function "stbi_zlib_decode_noheader_malloc"  (ref i8,i32,ref i32)ref i8
    clang function "stbi_zlib_decode_noheader_buffer"  (ref i8,i32,ref i8,i32)i32
    global macro  STBI_VERSION = 1
    global macro  STBIDEF =  extern
end
