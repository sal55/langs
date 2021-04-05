! Result of automatic conversion of stb_image.h header into my M language

importdll stb_image =
    const STBI_default                       = 0
    const STBI_grey                          = 1
    const STBI_grey_alpha                    = 2
    const STBI_rgb                           = 3
    const STBI_rgb_alpha                     = 4
    record $T2 =
        var ref clang function(ref void,ref i8,i32)i32 read$
        var ref clang proc(ref void,i32) skip
        var ref clang function(ref void)i32 eof
    end

    clang function "stbi_load_from_memory"             (ref byte,i32,ref i32,ref i32,ref i32,i32)ref byte
    clang function "stbi_load_from_callbacks"          (ref $T2,ref void,ref i32,ref i32,ref i32,i32)ref byte
    clang function "stbi_load_gif_from_memory"         (ref byte,i32,ref ref i32,ref i32,ref i32,ref i32,ref i32,i32)ref byte
    clang function "stbi_load"                         (ref i8,ref i32,ref i32,ref i32,i32)ref byte
    clang function "stbi_load_from_file"               (ref $T1,ref i32,ref i32,ref i32,i32)ref byte
    clang function "stbi_load_16_from_memory"          (ref byte,i32,ref i32,ref i32,ref i32,i32)ref u16
    clang function "stbi_load_16_from_callbacks"       (ref $T2,ref void,ref i32,ref i32,ref i32,i32)ref u16
    clang function "stbi_load_16"                      (ref i8,ref i32,ref i32,ref i32,i32)ref u16
    clang function "stbi_load_from_file_16"            (ref $T1,ref i32,ref i32,ref i32,i32)ref u16
    clang function "stbi_loadf_from_memory"            (ref byte,i32,ref i32,ref i32,ref i32,i32)ref r32
    clang function "stbi_loadf_from_callbacks"         (ref $T2,ref void,ref i32,ref i32,ref i32,i32)ref r32
    clang function "stbi_loadf"                        (ref i8,ref i32,ref i32,ref i32,i32)ref r32
    clang function "stbi_loadf_from_file"              (ref $T1,ref i32,ref i32,ref i32,i32)ref r32
    clang proc     "stbi_hdr_to_ldr_gamma"             (r32)
    clang proc     "stbi_hdr_to_ldr_scale"             (r32)
    clang proc     "stbi_ldr_to_hdr_gamma"             (r32)
    clang proc     "stbi_ldr_to_hdr_scale"             (r32)
    clang function "stbi_is_hdr_from_callbacks"        (ref $T2,ref void)i32
    clang function "stbi_is_hdr_from_memory"           (ref byte,i32)i32
    clang function "stbi_is_hdr"                       (ref i8)i32
    clang function "stbi_is_hdr_from_file"             (ref $T1)i32
    clang function "stbi_failure_reason"               ()ref i8
    clang proc     "stbi_image_free"                   (ref void)
    clang function "stbi_info_from_memory"             (ref byte,i32,ref i32,ref i32,ref i32)i32
    clang function "stbi_info_from_callbacks"          (ref $T2,ref void,ref i32,ref i32,ref i32)i32
    clang function "stbi_is_16_bit_from_memory"        (ref byte,i32)i32
    clang function "stbi_is_16_bit_from_callbacks"     (ref $T2,ref void)i32
    clang function "stbi_info"                         (ref i8,ref i32,ref i32,ref i32)i32
    clang function "stbi_info_from_file"               (ref $T1,ref i32,ref i32,ref i32)i32
    clang function "stbi_is_16_bit"                    (ref i8)i32
    clang function "stbi_is_16_bit_from_file"          (ref $T1)i32
    clang proc     "stbi_set_unpremultiply_on_load"    (i32)
    clang proc     "stbi_convert_iphone_png_to_rgb"    (i32)
    clang proc     "stbi_set_flip_vertically_on_load"  (i32)
    clang function "stbi_zlib_decode_malloc_guesssize" (ref i8,i32,i32,ref i32)ref i8
    clang function "stbi_zlib_decode_malloc_guesssize_headerflag" (ref i8,i32,i32,ref i32,i32)ref i8
    clang function "stbi_zlib_decode_malloc"           (ref i8,i32,ref i32)ref i8
    clang function "stbi_zlib_decode_buffer"           (ref i8,i32,ref i8,i32)i32
    clang function "stbi_zlib_decode_noheader_malloc"  (ref i8,i32,ref i32)ref i8
    clang function "stbi_zlib_decode_noheader_buffer"  (ref i8,i32,ref i8,i32)i32
    record $T4 =
        var u32 img_x
        var u32 img_y
        var i32 img_n
        var i32 img_out_n
        var $T2 io
        var ref void io_user_data
        var i32 read_from_callbacks
        var i32 buflen
        var [128]byte buffer_start
        var ref byte img_buffer
        var ref byte img_buffer_end
        var ref byte img_buffer_original
        var ref byte img_buffer_original_end
    end

    clang proc     "stbi__refill_buffer"               (ref $T4)
    clang proc     "stbi__start_mem"                   (ref $T4,ref byte,i32)
    clang proc     "stbi__start_callbacks"             (ref $T4,ref $T2,ref void)
    clang function "stbi__stdio_read"                  (ref void,ref i8,i32)i32
    clang proc     "stbi__stdio_skip"                  (ref void,i32)
    clang function "stbi__stdio_eof"                   (ref void)i32
    $T2 stbi__stdio_callbacks ={&stbi__stdio_read,&stbi__stdio_skip,&stbi__stdio_eof}
    clang proc     "stbi__start_file"                  (ref $T4,ref $T1)
    clang proc     "stbi__rewind"                      (ref $T4)
    const STBI_ORDER_RGB                     = 0
    const STBI_ORDER_BGR                     = 1
    record $T5 =
        var i32 bits_per_channel
        var i32 num_channels
        var i32 channel_order
    end

    clang function "stbi__jpeg_test"                   (ref $T4)i32
    clang function "stbi__jpeg_load"                   (ref $T4,ref i32,ref i32,ref i32,i32,ref $T5)ref void
    clang function "stbi__jpeg_info"                   (ref $T4,ref i32,ref i32,ref i32)i32
    clang function "stbi__png_test"                    (ref $T4)i32
    clang function "stbi__png_load"                    (ref $T4,ref i32,ref i32,ref i32,i32,ref $T5)ref void
    clang function "stbi__png_info"                    (ref $T4,ref i32,ref i32,ref i32)i32
    clang function "stbi__png_is16"                    (ref $T4)i32
    clang function "stbi__bmp_test"                    (ref $T4)i32
    clang function "stbi__bmp_load"                    (ref $T4,ref i32,ref i32,ref i32,i32,ref $T5)ref void
    clang function "stbi__bmp_info"                    (ref $T4,ref i32,ref i32,ref i32)i32
    clang function "stbi__tga_test"                    (ref $T4)i32
    clang function "stbi__tga_load"                    (ref $T4,ref i32,ref i32,ref i32,i32,ref $T5)ref void
    clang function "stbi__tga_info"                    (ref $T4,ref i32,ref i32,ref i32)i32
    clang function "stbi__psd_test"                    (ref $T4)i32
    clang function "stbi__psd_load"                    (ref $T4,ref i32,ref i32,ref i32,i32,ref $T5,i32)ref void
    clang function "stbi__psd_info"                    (ref $T4,ref i32,ref i32,ref i32)i32
    clang function "stbi__psd_is16"                    (ref $T4)i32
    clang function "stbi__hdr_test"                    (ref $T4)i32
    clang function "stbi__hdr_load"                    (ref $T4,ref i32,ref i32,ref i32,i32,ref $T5)ref r32
    clang function "stbi__hdr_info"                    (ref $T4,ref i32,ref i32,ref i32)i32
    clang function "stbi__pic_test"                    (ref $T4)i32
    clang function "stbi__pic_load"                    (ref $T4,ref i32,ref i32,ref i32,i32,ref $T5)ref void
    clang function "stbi__pic_info"                    (ref $T4,ref i32,ref i32,ref i32)i32
    clang function "stbi__gif_test"                    (ref $T4)i32
    clang function "stbi__gif_load"                    (ref $T4,ref i32,ref i32,ref i32,i32,ref $T5)ref void
    clang function "stbi__load_gif_main"               (ref $T4,ref ref i32,ref i32,ref i32,ref i32,ref i32,i32)ref void
    clang function "stbi__gif_info"                    (ref $T4,ref i32,ref i32,ref i32)i32
    clang function "stbi__pnm_test"                    (ref $T4)i32
    clang function "stbi__pnm_load"                    (ref $T4,ref i32,ref i32,ref i32,i32,ref $T5)ref void
    clang function "stbi__pnm_info"                    (ref $T4,ref i32,ref i32,ref i32)i32
    ref i8 stbi__g_failure_reason
    clang function "stbi__err"                         (ref i8)i32
    clang function "stbi__malloc"                      (u64)ref void
    clang function "stbi__addsizes_valid"              (i32,i32)i32
    clang function "stbi__mul2sizes_valid"             (i32,i32)i32
    clang function "stbi__mad2sizes_valid"             (i32,i32,i32)i32
    clang function "stbi__mad3sizes_valid"             (i32,i32,i32,i32)i32
    clang function "stbi__mad4sizes_valid"             (i32,i32,i32,i32,i32)i32
    clang function "stbi__malloc_mad2"                 (i32,i32,i32)ref void
    clang function "stbi__malloc_mad3"                 (i32,i32,i32,i32)ref void
    clang function "stbi__malloc_mad4"                 (i32,i32,i32,i32,i32)ref void
    clang function "stbi__ldr_to_hdr"                  (ref byte,i32,i32,i32)ref r32
    clang function "stbi__hdr_to_ldr"                  (ref r32,i32,i32,i32)ref byte
    i32 stbi__vertically_flip_on_load =0
    clang function "stbi__load_main"                   (ref $T4,ref i32,ref i32,ref i32,i32,ref $T5,i32)ref void
    clang function "stbi__convert_16_to_8"             (ref u16,i32,i32,i32)ref byte
    clang function "stbi__convert_8_to_16"             (ref byte,i32,i32,i32)ref u16
    clang proc     "stbi__vertical_flip"               (ref void,i32,i32,i32)
    clang proc     "stbi__vertical_flip_slices"        (ref void,i32,i32,i32,i32)
    clang function "stbi__load_and_postprocess_8bit"   (ref $T4,ref i32,ref i32,ref i32,i32)ref byte
    clang function "stbi__load_and_postprocess_16bit"  (ref $T4,ref i32,ref i32,ref i32,i32)ref u16
    clang proc     "stbi__float_postprocess"           (ref r32,ref i32,ref i32,ref i32,i32)
    clang function "stbi__fopen"                       (ref i8,ref i8)ref $T1
    clang function "stbi__loadf_main"                  (ref $T4,ref i32,ref i32,ref i32,i32)ref r32
    r32 stbi__l2h_gamma =2.200000
    r32 stbi__l2h_scale =1.000000
    r32 stbi__h2l_gamma_i =0.454545
    r32 stbi__h2l_scale_i =1.000000
    const STBI__SCAN_load                    = 0
    const STBI__SCAN_type                    = 1
    const STBI__SCAN_header                  = 2
    clang function "stbi__get8"                        (ref $T4)byte
    clang function "stbi__at_eof"                      (ref $T4)i32
    clang proc     "stbi__skip"                        (ref $T4,i32)
    clang function "stbi__getn"                        (ref $T4,ref byte,i32)i32
    clang function "stbi__get16be"                     (ref $T4)i32
    clang function "stbi__get32be"                     (ref $T4)u32
    clang function "stbi__get16le"                     (ref $T4)i32
    clang function "stbi__get32le"                     (ref $T4)u32
    clang function "stbi__compute_y"                   (i32,i32,i32)byte
    clang function "stbi__convert_format"              (ref byte,i32,i32,u32,u32)ref byte
    clang function "stbi__compute_y_16"                (i32,i32,i32)u16
    clang function "stbi__convert_format16"            (ref u16,i32,i32,u32,u32)ref u16
    record $T6 =
        var [512]byte fast
        var [256]u16 code
        var [256]byte values
        var [257]byte size
        var [18]u32 maxcode
        var [17]i32 delta
    end

    record $T7 =
        var ref $T4 s
        var [4]$T6 huff_dc
        var [4]$T6 huff_ac
        var [4][64]u16 dequant
        var [4][512]i16 fast_ac
        var i32 img_h_max
        var i32 img_v_max
        var i32 img_mcu_x
        var i32 img_mcu_y
        var i32 img_mcu_w
        var i32 img_mcu_h
        var [4]$T8 img_comp
        var u32 code_buffer
        var i32 code_bits
        var byte marker
        var i32 nomore
        var i32 progressive
        var i32 spec_start
        var i32 spec_end
        var i32 succ_high
        var i32 succ_low
        var i32 eob_run
        var i32 jfif
        var i32 app14_color_transform
        var i32 rgb
        var i32 scan_n
        var [4]i32 order
        var i32 restart_interval
        var i32 todo
        var ref clang proc(ref byte,i32,ref i16) idct_block_kernel
        var ref clang proc(ref byte,ref byte,ref byte,ref byte,i32,i32) YCbCr_to_RGB_kernel
        var ref clang function(ref byte,ref byte,ref byte,i32,i32)ref byte resample_row_hv_2_kernel
    end

    record $T8 =
        var i32 id
        var i32 h
        var i32 v
        var i32 tq
        var i32 hd
        var i32 ha
        var i32 dc_pred
        var i32 x
        var i32 y
        var i32 w2
        var i32 h2
        var ref byte data
        var ref void raw_data
        var ref void raw_coeff
        var ref byte linebuf
        var ref i16 coeff
        var i32 coeff_w
        var i32 coeff_h
    end

    clang function "stbi__build_huffman"               (ref $T6,ref i32)i32
    clang proc     "stbi__build_fast_ac"               (ref i16,ref $T6)
    clang proc     "stbi__grow_buffer_unsafe"          (ref $T7)
    [17]u32 stbi__bmask ={0,1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,32767,65535}
    clang function "stbi__jpeg_huff_decode"            (ref $T7,ref $T6)i32
    [16]i32 stbi__jbias ={0,-1,-3,-7,-15,-31,-63,-127,-255,-511,-1023,-2047,-4095,-8191,-16383,-32767}
    clang function "stbi__extend_receive"              (ref $T7,i32)i32
    clang function "stbi__jpeg_get_bits"               (ref $T7,i32)i32
    clang function "stbi__jpeg_get_bit"                (ref $T7)i32
    [79]byte stbi__jpeg_dezigzag ={0,1,8,16,9,2,3,10,17,24,32,25,18,11,4,5,12,19,26,33,40,48,41,34,27,20,13,6,7,14,21,28,35,42,49,56,57,50,43,36,29,22,15,23,30,37,44,51,58,59,52,45,38,31,39,46,53,60,61,54,47,55,62,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63}
    clang function "stbi__jpeg_decode_block"           (ref $T7,ref i16,ref $T6,ref $T6,ref i16,i32,ref u16)i32
    clang function "stbi__jpeg_decode_block_prog_dc"   (ref $T7,ref i16,ref $T6,i32)i32
    clang function "stbi__jpeg_decode_block_prog_ac"   (ref $T7,ref i16,ref $T6,ref i16)i32
    clang function "stbi__clamp"                       (i32)byte
    clang proc     "stbi__idct_block"                  (ref byte,i32,ref i16)
    clang function "stbi__get_marker"                  (ref $T7)byte
    clang proc     "stbi__jpeg_reset"                  (ref $T7)
    clang function "stbi__parse_entropy_coded_data"    (ref $T7)i32
    clang proc     "stbi__jpeg_dequantize"             (ref i16,ref u16)
    clang proc     "stbi__jpeg_finish"                 (ref $T7)
    clang function "stbi__process_marker"              (ref $T7,i32)i32
    clang function "stbi__process_scan_header"         (ref $T7)i32
    clang function "stbi__free_jpeg_components"        (ref $T7,i32,i32)i32
    clang function "stbi__process_frame_header"        (ref $T7,i32)i32
    clang function "stbi__decode_jpeg_header"          (ref $T7,i32)i32
    clang function "stbi__decode_jpeg_image"           (ref $T7)i32
    clang function "resample_row_1"                    (ref byte,ref byte,ref byte,i32,i32)ref byte
    clang function "stbi__resample_row_v_2"            (ref byte,ref byte,ref byte,i32,i32)ref byte
    clang function "stbi__resample_row_h_2"            (ref byte,ref byte,ref byte,i32,i32)ref byte
    clang function "stbi__resample_row_hv_2"           (ref byte,ref byte,ref byte,i32,i32)ref byte
    clang function "stbi__resample_row_generic"        (ref byte,ref byte,ref byte,i32,i32)ref byte
    clang proc     "stbi__YCbCr_to_RGB_row"            (ref byte,ref byte,ref byte,ref byte,i32,i32)
    clang proc     "stbi__setup_jpeg"                  (ref $T7)
    clang proc     "stbi__cleanup_jpeg"                (ref $T7)
    record $T9 =
        var ref clang function(ref byte,ref byte,ref byte,i32,i32)ref byte resample
        var ref byte line0
        var ref byte line1
        var i32 hs
        var i32 vs
        var i32 w_lores
        var i32 ystep
        var i32 ypos
    end

    clang function "stbi__blinn_8x8"                   (byte,byte)byte
    clang function "load_jpeg_image"                   (ref $T7,ref i32,ref i32,ref i32,i32)ref byte
    clang function "stbi__jpeg_info_raw"               (ref $T7,ref i32,ref i32,ref i32)i32
    record $T10 =
        var [512]u16 fast
        var [16]u16 firstcode
        var [17]i32 maxcode
        var [16]u16 firstsymbol
        var [288]byte size
        var [288]u16 value
    end

    clang function "stbi__bitreverse16"                (i32)i32
    clang function "stbi__bit_reverse"                 (i32,i32)i32
    clang function "stbi__zbuild_huffman"              (ref $T10,ref byte,i32)i32
    record $T11 =
        var ref byte zbuffer
        var ref byte zbuffer_end
        var i32 num_bits
        var u32 code_buffer
        var ref i8 zout
        var ref i8 zout_start
        var ref i8 zout_end
        var i32 z_expandable
        var $T10 z_length
        var $T10 z_distance
    end

    clang function "stbi__zget8"                       (ref $T11)byte
    clang proc     "stbi__fill_bits"                   (ref $T11)
    clang function "stbi__zreceive"                    (ref $T11,i32)u32
    clang function "stbi__zhuffman_decode_slowpath"    (ref $T11,ref $T10)i32
    clang function "stbi__zhuffman_decode"             (ref $T11,ref $T10)i32
    clang function "stbi__zexpand"                     (ref $T11,ref i8,i32)i32
    [31]i32 stbi__zlength_base ={3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258,0,0}
    [31]i32 stbi__zlength_extra ={0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0,0,0}
    [32]i32 stbi__zdist_base ={1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577,0,0}
    [32]i32 stbi__zdist_extra ={0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13}
    clang function "stbi__parse_huffman_block"         (ref $T11)i32
    clang function "stbi__compute_huffman_codes"       (ref $T11)i32
    clang function "stbi__parse_uncompressed_block"    (ref $T11)i32
    clang function "stbi__parse_zlib_header"           (ref $T11)i32
    [288]byte stbi__zdefault_length ={8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8}
    [32]byte stbi__zdefault_distance ={5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5}
    clang function "stbi__parse_zlib"                  (ref $T11,i32)i32
    clang function "stbi__do_zlib"                     (ref $T11,ref i8,i32,i32,i32)i32
    record $T12 =
        var u32 length
        var u32 type$
    end

    clang function "stbi__get_chunk_header"            (ref $T4)$T12
    clang function "stbi__check_png_header"            (ref $T4)i32
    record $T13 =
        var ref $T4 s
        var ref byte idata
        var ref byte expanded
        var ref byte out
        var i32 depth
    end

    const STBI__F_none                       = 0
    const STBI__F_sub                        = 1
    const STBI__F_up                         = 2
    const STBI__F_avg                        = 3
    const STBI__F_paeth                      = 4
    const STBI__F_avg_first                  = 5
    const STBI__F_paeth_first                = 6
    [5]byte first_row_filter ={0,1,0,5,6}
    clang function "stbi__paeth"                       (i32,i32,i32)i32
    [9]byte stbi__depth_scale_table ={0,255,85,0,17,0,0,0,1}
    clang function "stbi__create_png_image_raw"        (ref $T13,ref byte,u32,i32,u32,u32,i32,i32)i32
    clang function "stbi__create_png_image"            (ref $T13,ref byte,u32,i32,i32,i32,i32)i32
    clang function "stbi__compute_transparency"        (ref $T13,ref byte,i32)i32
    clang function "stbi__compute_transparency16"      (ref $T13,ref u16,i32)i32
    clang function "stbi__expand_png_palette"          (ref $T13,ref byte,i32,i32)i32
    i32 stbi__unpremultiply_on_load =0
    i32 stbi__de_iphone_flag =0
    clang proc     "stbi__de_iphone"                   (ref $T13)
    clang function "stbi__parse_png_file"              (ref $T13,i32,i32)i32
    clang function "stbi__do_png"                      (ref $T13,ref i32,ref i32,ref i32,i32,ref $T5)ref void
    clang function "stbi__png_info_raw"                (ref $T13,ref i32,ref i32,ref i32)i32
    clang function "stbi__bmp_test_raw"                (ref $T4)i32
    clang function "stbi__high_bit"                    (u32)i32
    clang function "stbi__bitcount"                    (u32)i32
    clang function "stbi__shiftsigned"                 (i32,i32,i32)i32
    record $T14 =
        var i32 bpp
        var i32 offset
        var i32 hsz
        var u32 mr
        var u32 mg
        var u32 mb
        var u32 ma
        var u32 all_a
    end

    clang function "stbi__bmp_parse_header"            (ref $T4,ref $T14)ref void
    clang function "stbi__tga_get_comp"                (i32,i32,ref i32)i32
    clang proc     "stbi__tga_read_rgb16"              (ref $T4,ref byte)
    clang function "stbi__psd_decode_rle"              (ref $T4,ref byte,i32)i32
    clang function "stbi__pic_is4"                     (ref $T4,ref i8)i32
    clang function "stbi__pic_test_core"               (ref $T4)i32
    record $T15 =
        var byte size
        var byte type$
        var byte channel
    end

    clang function "stbi__readval"                     (ref $T4,i32,ref byte)ref byte
    clang proc     "stbi__copyval"                     (i32,ref byte,ref byte)
    clang function "stbi__pic_load_core"               (ref $T4,i32,i32,ref i32,ref byte)ref byte
    record $T16 =
        var i16 prefix
        var byte first
        var byte suffix
    end

    record $T17 =
        var i32 w
        var i32 h
        var ref byte out
        var ref byte background
        var ref byte history
        var i32 flags
        var i32 bgindex
        var i32 ratio
        var i32 transparent
        var i32 eflags
        var [256][4]byte pal
        var [256][4]byte lpal
        var [8192]$T16 codes
        var ref byte color_table
        var i32 parse
        var i32 step
        var i32 lflags
        var i32 start_x
        var i32 start_y
        var i32 max_x
        var i32 max_y
        var i32 cur_x
        var i32 cur_y
        var i32 line_size
        var i32 delay
    end

    clang function "stbi__gif_test_raw"                (ref $T4)i32
    clang proc     "stbi__gif_parse_colortable"        (ref $T4,ref [4]byte,i32,i32)
    clang function "stbi__gif_header"                  (ref $T4,ref $T17,ref i32,i32)i32
    clang function "stbi__gif_info_raw"                (ref $T4,ref i32,ref i32,ref i32)i32
    clang proc     "stbi__out_gif_code"                (ref $T17,u16)
    clang function "stbi__process_gif_raster"          (ref $T4,ref $T17)ref byte
    clang function "stbi__gif_load_next"               (ref $T4,ref $T17,ref i32,i32,ref byte)ref byte
    clang function "stbi__hdr_test_core"               (ref $T4,ref i8)i32
    clang function "stbi__hdr_gettoken"                (ref $T4,ref i8)ref i8
    clang proc     "stbi__hdr_convert"                 (ref r32,ref byte,i32)
    clang function "stbi__pnm_isspace"                 (i8)i32
    clang proc     "stbi__pnm_skip_whitespace"         (ref $T4,ref i8)
    clang function "stbi__pnm_isdigit"                 (i8)i32
    clang function "stbi__pnm_getinteger"              (ref $T4,ref i8)i32
    clang function "stbi__info_main"                   (ref $T4,ref i32,ref i32,ref i32)i32
    clang function "stbi__is_16_main"                  (ref $T4)i32
    clang function "main"                              ()i32
    const stbi__SOF_progressive              = ((x)==0xc2)    ! macro
    const STBI_SIMD_ALIGN                    = type name    ! macro
    const STBI__IDCT_1D                      =  int t0,t1,t2,t3,p1,p2,p3,p4,p5,x0,x1,x2,x3;p2=s2;p3=s6;p1=(p2+p3)*stbi__f2f(0.5411961f);t2=p1+p3*stbi__f2f(-1.847759065f);t3=p1+p2*stbi__f2f(0.765366865f);p2=s0;p3=s4;t0=stbi__fsh(p2+p3);t1=stbi__fsh(p2-p3);x0=t0+t3;x3=t0-t3;x1=t1+t2;x2=t1-t2;t0=s7;t1=s5;t2=s3;t3=s1;p3=t0+t2;p4=t1+t3;p1=t0+t3;p2=t1+t2;p5=(p3+p4)*stbi__f2f(1.175875602f);t0=t0*stbi__f2f(0.298631336f);t1=t1*stbi__f2f(2.053119869f);t2=t2*stbi__f2f(3.072711026f);t3=t3*stbi__f2f(1.501321110f);p1=p5+p1*stbi__f2f(-0.899976223f);p2=p5+p2*stbi__f2f(-2.562915447f);p3=p3*stbi__f2f(-1.961570560f);p4=p4*stbi__f2f(-0.390180644f);t3+=p1+p4;t2+=p2+p3;t1+=p2+p4;t0+=p1+p3;    ! macro
    const STBI_VERSION                       = 1    ! macro
    const STBI_REALLOC_SIZED                 =  STBI_REALLOC(p,newsz)    ! macro
    const stbi__div16                        = ((stbi_uc)((x)>>4))    ! macro
    const STBI__COMBO                        = ((a)*8+(b))    ! macro
    const STBI_ASSERT                        = assert(x)    ! macro
    const STBI__MARKER_none                  = 0xff    ! macro
    const STBI_FREE                          =  free(p)    ! macro
    const stbi__DNL                          = ((x)==0xdc)    ! macro
    const stbi__err                          = stbi__err(x)    ! macro
    const STBI__ZFAST_MASK                   = ((1<<STBI__ZFAST_BITS)-1)    ! macro
    const stbi__errpuc                       = ((unsigned char*)(size_t)(stbi__err(x,y)?NULL:NULL))    ! macro
    const STBI__RESTART                      = ((x)>=0xd0&&(x)<=0xd7)    ! macro
    const stbi__div4                         = ((stbi_uc)((x)>>2))    ! macro
    const stbi__float2fixed                  = (((int)((x)*4096.0f+0.5f))<<8)    ! macro
    const STBI__ZFAST_BITS                   = 9    ! macro
    const stbi__errpf                        = ((float*)(size_t)(stbi__err(x,y)?NULL:NULL))    ! macro
    const stbi__float2int                    = ((int)(x))    ! macro
    const stbi__SOF                          = ((x)==0xc0||(x)==0xc1||(x)==0xc2)    ! macro
    const stbi__SOI                          = ((x)==0xd8)    ! macro
    const STBI__PNG_TYPE                     = (((unsigned)(a)<<24)+((unsigned)(b)<<16)+((unsigned)(c)<<8)+(unsigned)(d))    ! macro
    const stbi__SOS                          = ((x)==0xda)    ! macro
    const stbi__EOI                          = ((x)==0xd9)    ! macro
    const STBI_NOTUSED                       = (void)sizeof(v)    ! macro
    const stbi__fsh                          = ((x)*4096)    ! macro
    const stbi_lrot                          = (((x)<<(y))|((x)>>(32-(y))))    ! macro
    const STBI_MALLOC                        = malloc(sz)    ! macro
    const STBI__HDR_BUFLEN                   = 1024    ! macro
    const STBI__BYTECAST                     = ((stbi_uc)((x)&255))    ! macro
    const FAST_BITS                          = 9    ! macro
    const STBI_REALLOC                       =  realloc(p,newsz)    ! macro
    const stbi__f2f                          = ((int)(((x)*4096+0.5)))    ! macro
    const STBIDEF                            = extern    ! macro
end
