importdll c =
    const false                              = 0
    const true                               = 1
    record Vector2 =
        var r32 x
        var r32 y
    end

    record Vector3 =
        var r32 x
        var r32 y
        var r32 z
    end

    record Vector4 =
        var r32 x
        var r32 y
        var r32 z
        var r32 w
    end

    record Matrix =
        var r32 m0
        var r32 m4
        var r32 m8
        var r32 m12
        var r32 m1
        var r32 m5
        var r32 m9
        var r32 m13
        var r32 m2
        var r32 m6
        var r32 m10
        var r32 m14
        var r32 m3
        var r32 m7
        var r32 m11
        var r32 m15
    end

    record Rectangle =
        var r32 x
        var r32 y
        var r32 width
        var r32 height
    end

    record Image =
        var ref void data
        var i32 width
        var i32 height
        var i32 mipmaps
        var i32 format
    end

    record Texture =
        var u32 id
        var i32 width
        var i32 height
        var i32 mipmaps
        var i32 format
    end

    record RenderTexture =
        var u32 id
        var Texture texture
        var Texture depth
    end

    record NPatchInfo =
        var Rectangle source
        var i32 left
        var i32 top
        var i32 right
        var i32 bottom
        var i32 type$
    end

    record CharInfo =
        var i32 value
        var i32 offsetX
        var i32 offsetY
        var i32 advanceX
        var Image image
    end

    record Font =
        var i32 baseSize
        var i32 charsCount
        var i32 charsPadding
        var Texture texture
        var ref Rectangle recs
        var ref CharInfo chars
    end

    record Camera3D =
        var Vector3 position
        var Vector3 target
        var Vector3 up
        var r32 fovy
        var i32 type$
    end

    record Camera2D =
        var Vector2 offset
        var Vector2 target
        var r32 rotation
        var r32 zoom
    end

    record Mesh =
        var i32 vertexCount
        var i32 triangleCount
        var ref r32 vertices
        var ref r32 texcoords
        var ref r32 texcoords2
        var ref r32 normals
        var ref r32 tangents
        var ref byte colors
        var ref u16 indices
        var ref r32 animVertices
        var ref r32 animNormals
        var ref i32 boneIds
        var ref r32 boneWeights
        var u32 vaoId
        var ref u32 vboId
    end

    record Shader =
        var u32 id
        var ref i32 locs
    end

    record MaterialMap =
        var Texture texture
        var u32 color
        var r32 value
    end

    record Material =
        var Shader shader
        var ref MaterialMap maps
        var ref r32 params
    end

    record Transform =
        var Vector3 translation
        var Vector4 rotation
        var Vector3 scale
    end

    record BoneInfo =
        var [32]i8 name
        var i32 parent
    end

    record Model =
        var Matrix transform
        var i32 meshCount
        var i32 materialCount
        var ref Mesh meshes
        var ref Material materials
        var ref i32 meshMaterial
        var i32 boneCount
        var ref BoneInfo bones
        var ref Transform bindPose
    end

    record ModelAnimation =
        var i32 boneCount
        var i32 frameCount
        var ref BoneInfo bones
        var ref ref Transform framePoses
    end

    record Ray =
        var Vector3 position
        var Vector3 direction
    end

    record RayHitInfo =
        var i32 hit
        var r32 distance
        var Vector3 position
        var Vector3 normal
    end

    record BoundingBox =
        var Vector3 min
        var Vector3 max
    end

    record Wave =
        var u32 sampleCount
        var u32 sampleRate
        var u32 sampleSize
        var u32 channels
        var ref void data
    end

    record rAudioBuffer =
        var int dummy    !empty record
    end

    record AudioStream =
        var ref rAudioBuffer buffer
        var u32 sampleRate
        var u32 sampleSize
        var u32 channels
    end

    record Sound =
        var AudioStream stream
        var u32 sampleCount
    end

    record Music =
        var AudioStream stream
        var u32 sampleCount
        var i32 looping
        var i32 ctxType
        var ref void ctxData
    end

    record VrDeviceInfo =
        var i32 hResolution
        var i32 vResolution
        var r32 hScreenSize
        var r32 vScreenSize
        var r32 vScreenCenter
        var r32 eyeToScreenDistance
        var r32 lensSeparationDistance
        var r32 interpupillaryDistance
        var [4]r32 lensDistortionValues
        var [4]r32 chromaAbCorrection
    end

    const FLAG_VSYNC_HINT                    = 64
    const FLAG_FULLSCREEN_MODE               = 2
    const FLAG_WINDOW_RESIZABLE              = 4
    const FLAG_WINDOW_UNDECORATED            = 8
    const FLAG_WINDOW_HIDDEN                 = 128
    const FLAG_WINDOW_MINIMIZED              = 512
    const FLAG_WINDOW_MAXIMIZED              = 1024
    const FLAG_WINDOW_UNFOCUSED              = 2048
    const FLAG_WINDOW_TOPMOST                = 4096
    const FLAG_WINDOW_ALWAYS_RUN             = 256
    const FLAG_WINDOW_TRANSPARENT            = 16
    const FLAG_WINDOW_HIGHDPI                = 8192
    const FLAG_MSAA_4X_HINT                  = 32
    const FLAG_INTERLACED_HINT               = 65536
    const LOG_ALL                            = 0
    const LOG_TRACE                          = 1
    const LOG_DEBUG                          = 2
    const LOG_INFO                           = 3
    const LOG_WARNING                        = 4
    const LOG_ERROR                          = 5
    const LOG_FATAL                          = 6
    const LOG_NONE                           = 7
    const KEY_APOSTROPHE                     = 39
    const KEY_COMMA                          = 44
    const KEY_MINUS                          = 45
    const KEY_PERIOD                         = 46
    const KEY_SLASH                          = 47
    const KEY_ZERO                           = 48
    const KEY_ONE                            = 49
    const KEY_TWO                            = 50
    const KEY_THREE                          = 51
    const KEY_FOUR                           = 52
    const KEY_FIVE                           = 53
    const KEY_SIX                            = 54
    const KEY_SEVEN                          = 55
    const KEY_EIGHT                          = 56
    const KEY_NINE                           = 57
    const KEY_SEMICOLON                      = 59
    const KEY_EQUAL                          = 61
    const KEY_A                              = 65
    const KEY_B                              = 66
    const KEY_C                              = 67
    const KEY_D                              = 68
    const KEY_E                              = 69
    const KEY_F                              = 70
    const KEY_G                              = 71
    const KEY_H                              = 72
    const KEY_I                              = 73
    const KEY_J                              = 74
    const KEY_K                              = 75
    const KEY_L                              = 76
    const KEY_M                              = 77
    const KEY_N                              = 78
    const KEY_O                              = 79
    const KEY_P                              = 80
    const KEY_Q                              = 81
    const KEY_R                              = 82
    const KEY_S                              = 83
    const KEY_T                              = 84
    const KEY_U                              = 85
    const KEY_V                              = 86
    const KEY_W                              = 87
    const KEY_X                              = 88
    const KEY_Y                              = 89
    const KEY_Z                              = 90
    const KEY_SPACE                          = 32
    const KEY_ESCAPE                         = 256
    const KEY_ENTER                          = 257
    const KEY_TAB                            = 258
    const KEY_BACKSPACE                      = 259
    const KEY_INSERT                         = 260
    const KEY_DELETE                         = 261
    const KEY_RIGHT                          = 262
    const KEY_LEFT                           = 263
    const KEY_DOWN                           = 264
    const KEY_UP                             = 265
    const KEY_PAGE_UP                        = 266
    const KEY_PAGE_DOWN                      = 267
    const KEY_HOME                           = 268
    const KEY_END                            = 269
    const KEY_CAPS_LOCK                      = 280
    const KEY_SCROLL_LOCK                    = 281
    const KEY_NUM_LOCK                       = 282
    const KEY_PRINT_SCREEN                   = 283
    const KEY_PAUSE                          = 284
    const KEY_F1                             = 290
    const KEY_F2                             = 291
    const KEY_F3                             = 292
    const KEY_F4                             = 293
    const KEY_F5                             = 294
    const KEY_F6                             = 295
    const KEY_F7                             = 296
    const KEY_F8                             = 297
    const KEY_F9                             = 298
    const KEY_F10                            = 299
    const KEY_F11                            = 300
    const KEY_F12                            = 301
    const KEY_LEFT_SHIFT                     = 340
    const KEY_LEFT_CONTROL                   = 341
    const KEY_LEFT_ALT                       = 342
    const KEY_LEFT_SUPER                     = 343
    const KEY_RIGHT_SHIFT                    = 344
    const KEY_RIGHT_CONTROL                  = 345
    const KEY_RIGHT_ALT                      = 346
    const KEY_RIGHT_SUPER                    = 347
    const KEY_KB_MENU                        = 348
    const KEY_LEFT_BRACKET                   = 91
    const KEY_BACKSLASH                      = 92
    const KEY_RIGHT_BRACKET                  = 93
    const KEY_GRAVE                          = 96
    const KEY_KP_0                           = 320
    const KEY_KP_1                           = 321
    const KEY_KP_2                           = 322
    const KEY_KP_3                           = 323
    const KEY_KP_4                           = 324
    const KEY_KP_5                           = 325
    const KEY_KP_6                           = 326
    const KEY_KP_7                           = 327
    const KEY_KP_8                           = 328
    const KEY_KP_9                           = 329
    const KEY_KP_DECIMAL                     = 330
    const KEY_KP_DIVIDE                      = 331
    const KEY_KP_MULTIPLY                    = 332
    const KEY_KP_SUBTRACT                    = 333
    const KEY_KP_ADD                         = 334
    const KEY_KP_ENTER                       = 335
    const KEY_KP_EQUAL                       = 336
    const KEY_BACK                           = 4
    const KEY_MENU                           = 82
    const KEY_VOLUME_UP                      = 24
    const KEY_VOLUME_DOWN                    = 25
    const MOUSE_LEFT_BUTTON                  = 0
    const MOUSE_RIGHT_BUTTON                 = 1
    const MOUSE_MIDDLE_BUTTON                = 2
    const MOUSE_CURSOR_DEFAULT               = 0
    const MOUSE_CURSOR_ARROW                 = 1
    const MOUSE_CURSOR_IBEAM                 = 2
    const MOUSE_CURSOR_CROSSHAIR             = 3
    const MOUSE_CURSOR_POINTING_HAND         = 4
    const MOUSE_CURSOR_RESIZE_EW             = 5
    const MOUSE_CURSOR_RESIZE_NS             = 6
    const MOUSE_CURSOR_RESIZE_NWSE           = 7
    const MOUSE_CURSOR_RESIZE_NESW           = 8
    const MOUSE_CURSOR_RESIZE_ALL            = 9
    const MOUSE_CURSOR_NOT_ALLOWED           = 10
    const GAMEPAD_PLAYER1                    = 0
    const GAMEPAD_PLAYER2                    = 1
    const GAMEPAD_PLAYER3                    = 2
    const GAMEPAD_PLAYER4                    = 3
    const GAMEPAD_BUTTON_UNKNOWN             = 0
    const GAMEPAD_BUTTON_LEFT_FACE_UP        = 1
    const GAMEPAD_BUTTON_LEFT_FACE_RIGHT     = 2
    const GAMEPAD_BUTTON_LEFT_FACE_DOWN      = 3
    const GAMEPAD_BUTTON_LEFT_FACE_LEFT      = 4
    const GAMEPAD_BUTTON_RIGHT_FACE_UP       = 5
    const GAMEPAD_BUTTON_RIGHT_FACE_RIGHT    = 6
    const GAMEPAD_BUTTON_RIGHT_FACE_DOWN     = 7
    const GAMEPAD_BUTTON_RIGHT_FACE_LEFT     = 8
    const GAMEPAD_BUTTON_LEFT_TRIGGER_1      = 9
    const GAMEPAD_BUTTON_LEFT_TRIGGER_2      = 10
    const GAMEPAD_BUTTON_RIGHT_TRIGGER_1     = 11
    const GAMEPAD_BUTTON_RIGHT_TRIGGER_2     = 12
    const GAMEPAD_BUTTON_MIDDLE_LEFT         = 13
    const GAMEPAD_BUTTON_MIDDLE              = 14
    const GAMEPAD_BUTTON_MIDDLE_RIGHT        = 15
    const GAMEPAD_BUTTON_LEFT_THUMB          = 16
    const GAMEPAD_BUTTON_RIGHT_THUMB         = 17
    const GAMEPAD_AXIS_LEFT_X                = 0
    const GAMEPAD_AXIS_LEFT_Y                = 1
    const GAMEPAD_AXIS_RIGHT_X               = 2
    const GAMEPAD_AXIS_RIGHT_Y               = 3
    const GAMEPAD_AXIS_LEFT_TRIGGER          = 4
    const GAMEPAD_AXIS_RIGHT_TRIGGER         = 5
    const LOC_VERTEX_POSITION                = 0
    const LOC_VERTEX_TEXCOORD01              = 1
    const LOC_VERTEX_TEXCOORD02              = 2
    const LOC_VERTEX_NORMAL                  = 3
    const LOC_VERTEX_TANGENT                 = 4
    const LOC_VERTEX_COLOR                   = 5
    const LOC_MATRIX_MVP                     = 6
    const LOC_MATRIX_MODEL                   = 7
    const LOC_MATRIX_VIEW                    = 8
    const LOC_MATRIX_PROJECTION              = 9
    const LOC_VECTOR_VIEW                    = 10
    const LOC_COLOR_DIFFUSE                  = 11
    const LOC_COLOR_SPECULAR                 = 12
    const LOC_COLOR_AMBIENT                  = 13
    const LOC_MAP_ALBEDO                     = 14
    const LOC_MAP_METALNESS                  = 15
    const LOC_MAP_NORMAL                     = 16
    const LOC_MAP_ROUGHNESS                  = 17
    const LOC_MAP_OCCLUSION                  = 18
    const LOC_MAP_EMISSION                   = 19
    const LOC_MAP_HEIGHT                     = 20
    const LOC_MAP_CUBEMAP                    = 21
    const LOC_MAP_IRRADIANCE                 = 22
    const LOC_MAP_PREFILTER                  = 23
    const LOC_MAP_BRDF                       = 24
    const UNIFORM_FLOAT                      = 0
    const UNIFORM_VEC2                       = 1
    const UNIFORM_VEC3                       = 2
    const UNIFORM_VEC4                       = 3
    const UNIFORM_INT                        = 4
    const UNIFORM_IVEC2                      = 5
    const UNIFORM_IVEC3                      = 6
    const UNIFORM_IVEC4                      = 7
    const UNIFORM_SAMPLER2D                  = 8
    const MAP_ALBEDO                         = 0
    const MAP_METALNESS                      = 1
    const MAP_NORMAL                         = 2
    const MAP_ROUGHNESS                      = 3
    const MAP_OCCLUSION                      = 4
    const MAP_EMISSION                       = 5
    const MAP_HEIGHT                         = 6
    const MAP_CUBEMAP                        = 7
    const MAP_IRRADIANCE                     = 8
    const MAP_PREFILTER                      = 9
    const MAP_BRDF                           = 10
    const UNCOMPRESSED_GRAYSCALE             = 1
    const UNCOMPRESSED_GRAY_ALPHA            = 2
    const UNCOMPRESSED_R5G6B5                = 3
    const UNCOMPRESSED_R8G8B8                = 4
    const UNCOMPRESSED_R5G5B5A1              = 5
    const UNCOMPRESSED_R4G4B4A4              = 6
    const UNCOMPRESSED_R8G8B8A8              = 7
    const UNCOMPRESSED_R32                   = 8
    const UNCOMPRESSED_R32G32B32             = 9
    const UNCOMPRESSED_R32G32B32A32          = 10
    const COMPRESSED_DXT1_RGB                = 11
    const COMPRESSED_DXT1_RGBA               = 12
    const COMPRESSED_DXT3_RGBA               = 13
    const COMPRESSED_DXT5_RGBA               = 14
    const COMPRESSED_ETC1_RGB                = 15
    const COMPRESSED_ETC2_RGB                = 16
    const COMPRESSED_ETC2_EAC_RGBA           = 17
    const COMPRESSED_PVRT_RGB                = 18
    const COMPRESSED_PVRT_RGBA               = 19
    const COMPRESSED_ASTC_4x4_RGBA           = 20
    const COMPRESSED_ASTC_8x8_RGBA           = 21
    const FILTER_POINT                       = 0
    const FILTER_BILINEAR                    = 1
    const FILTER_TRILINEAR                   = 2
    const FILTER_ANISOTROPIC_4X              = 3
    const FILTER_ANISOTROPIC_8X              = 4
    const FILTER_ANISOTROPIC_16X             = 5
    const WRAP_REPEAT                        = 0
    const WRAP_CLAMP                         = 1
    const WRAP_MIRROR_REPEAT                 = 2
    const WRAP_MIRROR_CLAMP                  = 3
    const CUBEMAP_AUTO_DETECT                = 0
    const CUBEMAP_LINE_VERTICAL              = 1
    const CUBEMAP_LINE_HORIZONTAL            = 2
    const CUBEMAP_CROSS_THREE_BY_FOUR        = 3
    const CUBEMAP_CROSS_FOUR_BY_THREE        = 4
    const CUBEMAP_PANORAMA                   = 5
    const FONT_DEFAULT                       = 0
    const FONT_BITMAP                        = 1
    const FONT_SDF                           = 2
    const BLEND_ALPHA                        = 0
    const BLEND_ADDITIVE                     = 1
    const BLEND_MULTIPLIED                   = 2
    const BLEND_ADD_COLORS                   = 3
    const BLEND_SUBTRACT_COLORS              = 4
    const BLEND_CUSTOM                       = 5
    const GESTURE_NONE                       = 0
    const GESTURE_TAP                        = 1
    const GESTURE_DOUBLETAP                  = 2
    const GESTURE_HOLD                       = 4
    const GESTURE_DRAG                       = 8
    const GESTURE_SWIPE_RIGHT                = 16
    const GESTURE_SWIPE_LEFT                 = 32
    const GESTURE_SWIPE_UP                   = 64
    const GESTURE_SWIPE_DOWN                 = 128
    const GESTURE_PINCH_IN                   = 256
    const GESTURE_PINCH_OUT                  = 512
    const CAMERA_CUSTOM                      = 0
    const CAMERA_FREE                        = 1
    const CAMERA_ORBITAL                     = 2
    const CAMERA_FIRST_PERSON                = 3
    const CAMERA_THIRD_PERSON                = 4
    const CAMERA_PERSPECTIVE                 = 0
    const CAMERA_ORTHOGRAPHIC                = 1
    const NPT_9PATCH                         = 0
    const NPT_3PATCH_VERTICAL                = 1
    const NPT_3PATCH_HORIZONTAL              = 2
    clang proc     "InitWindow"                        (i32,i32,ref i8)
    clang function "WindowShouldClose"                 ()i32
    clang proc     "CloseWindow"                       ()
    clang function "IsWindowReady"                     ()i32
    clang function "IsWindowFullscreen"                ()i32
    clang function "IsWindowHidden"                    ()i32
    clang function "IsWindowMinimized"                 ()i32
    clang function "IsWindowMaximized"                 ()i32
    clang function "IsWindowFocused"                   ()i32
    clang function "IsWindowResized"                   ()i32
    clang function "IsWindowState"                     (u32)i32
    clang proc     "SetWindowState"                    (u32)
    clang proc     "ClearWindowState"                  (u32)
    clang proc     "ToggleFullscreen"                  ()
    clang proc     "MaximizeWindow"                    ()
    clang proc     "MinimizeWindow"                    ()
    clang proc     "RestoreWindow"                     ()
    clang proc     "SetWindowIcon"                     (Image)
    clang proc     "SetWindowTitle"                    (ref i8)
    clang proc     "SetWindowPosition"                 (i32,i32)
    clang proc     "SetWindowMonitor"                  (i32)
    clang proc     "SetWindowMinSize"                  (i32,i32)
    clang proc     "SetWindowSize"                     (i32,i32)
    clang function "GetWindowHandle"                   ()ref void
    clang function "GetScreenWidth"                    ()i32
    clang function "GetScreenHeight"                   ()i32
    clang function "GetMonitorCount"                   ()i32
    clang function "GetMonitorPosition"                (i32)Vector2
    clang function "GetMonitorWidth"                   (i32)i32
    clang function "GetMonitorHeight"                  (i32)i32
    clang function "GetMonitorPhysicalWidth"           (i32)i32
    clang function "GetMonitorPhysicalHeight"          (i32)i32
    clang function "GetMonitorRefreshRate"             (i32)i32
    clang function "GetWindowPosition"                 ()Vector2
    clang function "GetWindowScaleDPI"                 ()Vector2
    clang function "GetMonitorName"                    (i32)ref i8
    clang proc     "SetClipboardText"                  (ref i8)
    clang function "GetClipboardText"                  ()ref i8
    clang proc     "ShowCursor"                        ()
    clang proc     "HideCursor"                        ()
    clang function "IsCursorHidden"                    ()i32
    clang proc     "EnableCursor"                      ()
    clang proc     "DisableCursor"                     ()
    clang function "IsCursorOnScreen"                  ()i32
    clang proc     "ClearBackground"                   (u32)
    clang proc     "BeginDrawing"                      ()
    clang proc     "EndDrawing"                        ()
    clang proc     "BeginMode2D"                       (Camera2D)
    clang proc     "EndMode2D"                         ()
    clang proc     "BeginMode3D"                       (Camera3D)
    clang proc     "EndMode3D"                         ()
    clang proc     "BeginTextureMode"                  (RenderTexture)
    clang proc     "EndTextureMode"                    ()
    clang proc     "BeginScissorMode"                  (i32,i32,i32,i32)
    clang proc     "EndScissorMode"                    ()
    clang function "GetMouseRay"                       (Vector2,Camera3D)Ray
    clang function "GetCameraMatrix"                   (Camera3D)Matrix
    clang function "GetCameraMatrix2D"                 (Camera2D)Matrix
    clang function "GetWorldToScreen"                  (Vector3,Camera3D)Vector2
    clang function "GetWorldToScreenEx"                (Vector3,Camera3D,i32,i32)Vector2
    clang function "GetWorldToScreen2D"                (Vector2,Camera2D)Vector2
    clang function "GetScreenToWorld2D"                (Vector2,Camera2D)Vector2
    clang proc     "SetTargetFPS"                      (i32)
    clang function "GetFPS"                            ()i32
    clang function "GetFrameTime"                      ()r32
    clang function "GetTime"                           ()r64
    clang proc     "SetConfigFlags"                    (u32)
    clang proc     "SetTraceLogLevel"                  (i32)
    clang proc     "SetTraceLogExit"                   (i32)
    clang proc     "SetTraceLogCallback"               (ref clang proc(i32,ref i8,ref i8))
    clang proc     "TraceLog"                          (i32,ref i8,...)
    clang function "MemAlloc"                          (i32)ref void
    clang proc     "MemFree"                           (ref void)
    clang proc     "TakeScreenshot"                    (ref i8)
    clang function "GetRandomValue"                    (i32,i32)i32
    clang function "LoadFileData"                      (ref i8,ref u32)ref byte
    clang proc     "UnloadFileData"                    (ref byte)
    clang function "SaveFileData"                      (ref i8,ref void,u32)i32
    clang function "LoadFileText"                      (ref i8)ref i8
    clang proc     "UnloadFileText"                    (ref byte)
    clang function "SaveFileText"                      (ref i8,ref i8)i32
    clang function "FileExists"                        (ref i8)i32
    clang function "DirectoryExists"                   (ref i8)i32
    clang function "IsFileExtension"                   (ref i8,ref i8)i32
    clang function "GetFileExtension"                  (ref i8)ref i8
    clang function "GetFileName"                       (ref i8)ref i8
    clang function "GetFileNameWithoutExt"             (ref i8)ref i8
    clang function "GetDirectoryPath"                  (ref i8)ref i8
    clang function "GetPrevDirectoryPath"              (ref i8)ref i8
    clang function "GetWorkingDirectory"               ()ref i8
    clang function "GetDirectoryFiles"                 (ref i8,ref i32)ref ref i8
    clang proc     "ClearDirectoryFiles"               ()
    clang function "ChangeDirectory"                   (ref i8)i32
    clang function "IsFileDropped"                     ()i32
    clang function "GetDroppedFiles"                   (ref i32)ref ref i8
    clang proc     "ClearDroppedFiles"                 ()
    clang function "GetFileModTime"                    (ref i8)i32
    clang function "CompressData"                      (ref byte,i32,ref i32)ref byte
    clang function "DecompressData"                    (ref byte,i32,ref i32)ref byte
    clang function "SaveStorageValue"                  (u32,i32)i32
    clang function "LoadStorageValue"                  (u32)i32
    clang proc     "OpenURL"                           (ref i8)
    clang function "IsKeyPressed"                      (i32)i32
    clang function "IsKeyDown"                         (i32)i32
    clang function "IsKeyReleased"                     (i32)i32
    clang function "IsKeyUp"                           (i32)i32
    clang proc     "SetExitKey"                        (i32)
    clang function "GetKeyPressed"                     ()i32
    clang function "GetCharPressed"                    ()i32
    clang function "IsGamepadAvailable"                (i32)i32
    clang function "IsGamepadName"                     (i32,ref i8)i32
    clang function "GetGamepadName"                    (i32)ref i8
    clang function "IsGamepadButtonPressed"            (i32,i32)i32
    clang function "IsGamepadButtonDown"               (i32,i32)i32
    clang function "IsGamepadButtonReleased"           (i32,i32)i32
    clang function "IsGamepadButtonUp"                 (i32,i32)i32
    clang function "GetGamepadButtonPressed"           ()i32
    clang function "GetGamepadAxisCount"               (i32)i32
    clang function "GetGamepadAxisMovement"            (i32,i32)r32
    clang function "IsMouseButtonPressed"              (i32)i32
    clang function "IsMouseButtonDown"                 (i32)i32
    clang function "IsMouseButtonReleased"             (i32)i32
    clang function "IsMouseButtonUp"                   (i32)i32
    clang function "GetMouseX"                         ()i32
    clang function "GetMouseY"                         ()i32
    clang function "GetMousePosition"                  ()Vector2
    clang proc     "SetMousePosition"                  (i32,i32)
    clang proc     "SetMouseOffset"                    (i32,i32)
    clang proc     "SetMouseScale"                     (r32,r32)
    clang function "GetMouseWheelMove"                 ()r32
    clang function "GetMouseCursor"                    ()i32
    clang proc     "SetMouseCursor"                    (i32)
    clang function "GetTouchX"                         ()i32
    clang function "GetTouchY"                         ()i32
    clang function "GetTouchPosition"                  (i32)Vector2
    clang proc     "SetGesturesEnabled"                (u32)
    clang function "IsGestureDetected"                 (i32)i32
    clang function "GetGestureDetected"                ()i32
    clang function "GetTouchPointsCount"               ()i32
    clang function "GetGestureHoldDuration"            ()r32
    clang function "GetGestureDragVector"              ()Vector2
    clang function "GetGestureDragAngle"               ()r32
    clang function "GetGesturePinchVector"             ()Vector2
    clang function "GetGesturePinchAngle"              ()r32
    clang proc     "SetCameraMode"                     (Camera3D,i32)
    clang proc     "UpdateCamera"                      (ref Camera3D)
    clang proc     "SetCameraPanControl"               (i32)
    clang proc     "SetCameraAltControl"               (i32)
    clang proc     "SetCameraSmoothZoomControl"        (i32)
    clang proc     "SetCameraMoveControls"             (i32,i32,i32,i32,i32,i32)
    clang proc     "DrawPixel"                         (i32,i32,u32)
    clang proc     "DrawPixelV"                        (Vector2,u32)
    clang proc     "DrawLine"                          (i32,i32,i32,i32,u32)
    clang proc     "DrawLineV"                         (Vector2,Vector2,u32)
    clang proc     "DrawLineEx"                        (Vector2,Vector2,r32,u32)
    clang proc     "DrawLineBezier"                    (Vector2,Vector2,r32,u32)
    clang proc     "DrawLineStrip"                     (ref Vector2,i32,u32)
    clang proc     "DrawCircle"                        (i32,i32,r32,u32)
    clang proc     "DrawCircleSector"                  (Vector2,r32,i32,i32,i32,u32)
    clang proc     "DrawCircleSectorLines"             (Vector2,r32,i32,i32,i32,u32)
    clang proc     "DrawCircleGradient"                (i32,i32,r32,u32,u32)
    clang proc     "DrawCircleV"                       (Vector2,r32,u32)
    clang proc     "DrawCircleLines"                   (i32,i32,r32,u32)
    clang proc     "DrawEllipse"                       (i32,i32,r32,r32,u32)
    clang proc     "DrawEllipseLines"                  (i32,i32,r32,r32,u32)
    clang proc     "DrawRing"                          (Vector2,r32,r32,i32,i32,i32,u32)
    clang proc     "DrawRingLines"                     (Vector2,r32,r32,i32,i32,i32,u32)
    clang proc     "DrawRectangle"                     (i32,i32,i32,i32,u32)
    clang proc     "DrawRectangleV"                    (Vector2,Vector2,u32)
    clang proc     "DrawRectangleRec"                  (Rectangle,u32)
    clang proc     "DrawRectanglePro"                  (Rectangle,Vector2,r32,u32)
    clang proc     "DrawRectangleGradientV"            (i32,i32,i32,i32,u32,u32)
    clang proc     "DrawRectangleGradientH"            (i32,i32,i32,i32,u32,u32)
    clang proc     "DrawRectangleGradientEx"           (Rectangle,u32,u32,u32,u32)
    clang proc     "DrawRectangleLines"                (i32,i32,i32,i32,u32)
    clang proc     "DrawRectangleLinesEx"              (Rectangle,i32,u32)
    clang proc     "DrawRectangleRounded"              (Rectangle,r32,i32,u32)
    clang proc     "DrawRectangleRoundedLines"         (Rectangle,r32,i32,i32,u32)
    clang proc     "DrawTriangle"                      (Vector2,Vector2,Vector2,u32)
    clang proc     "DrawTriangleLines"                 (Vector2,Vector2,Vector2,u32)
    clang proc     "DrawTriangleFan"                   (ref Vector2,i32,u32)
    clang proc     "DrawTriangleStrip"                 (ref Vector2,i32,u32)
    clang proc     "DrawPoly"                          (Vector2,i32,r32,r32,u32)
    clang proc     "DrawPolyLines"                     (Vector2,i32,r32,r32,u32)
    clang function "CheckCollisionRecs"                (Rectangle,Rectangle)i32
    clang function "CheckCollisionCircles"             (Vector2,r32,Vector2,r32)i32
    clang function "CheckCollisionCircleRec"           (Vector2,r32,Rectangle)i32
    clang function "CheckCollisionPointRec"            (Vector2,Rectangle)i32
    clang function "CheckCollisionPointCircle"         (Vector2,Vector2,r32)i32
    clang function "CheckCollisionPointTriangle"       (Vector2,Vector2,Vector2,Vector2)i32
    clang function "CheckCollisionLines"               (Vector2,Vector2,Vector2,Vector2,ref Vector2)i32
    clang function "GetCollisionRec"                   (Rectangle,Rectangle)Rectangle
    clang function "LoadImage"                         (ref i8)Image
    clang function "LoadImageRaw"                      (ref i8,i32,i32,i32,i32)Image
    clang function "LoadImageAnim"                     (ref i8,ref i32)Image
    clang function "LoadImageFromMemory"               (ref i8,ref byte,i32)Image
    clang proc     "UnloadImage"                       (Image)
    clang function "ExportImage"                       (Image,ref i8)i32
    clang function "ExportImageAsCode"                 (Image,ref i8)i32
    clang function "GenImageColor"                     (i32,i32,u32)Image
    clang function "GenImageGradientV"                 (i32,i32,u32,u32)Image
    clang function "GenImageGradientH"                 (i32,i32,u32,u32)Image
    clang function "GenImageGradientRadial"            (i32,i32,r32,u32,u32)Image
    clang function "GenImageChecked"                   (i32,i32,i32,i32,u32,u32)Image
    clang function "GenImageWhiteNoise"                (i32,i32,r32)Image
    clang function "GenImagePerlinNoise"               (i32,i32,i32,i32,r32)Image
    clang function "GenImageCellular"                  (i32,i32,i32)Image
    clang function "ImageCopy"                         (Image)Image
    clang function "ImageFromImage"                    (Image,Rectangle)Image
    clang function "ImageText"                         (ref i8,i32,u32)Image
    clang function "ImageTextEx"                       (Font,ref i8,r32,r32,u32)Image
    clang proc     "ImageFormat"                       (ref Image,i32)
    clang proc     "ImageToPOT"                        (ref Image,u32)
    clang proc     "ImageCrop"                         (ref Image,Rectangle)
    clang proc     "ImageAlphaCrop"                    (ref Image,r32)
    clang proc     "ImageAlphaClear"                   (ref Image,u32,r32)
    clang proc     "ImageAlphaMask"                    (ref Image,Image)
    clang proc     "ImageAlphaPremultiply"             (ref Image)
    clang proc     "ImageResize"                       (ref Image,i32,i32)
    clang proc     "ImageResizeNN"                     (ref Image,i32,i32)
    clang proc     "ImageResizeCanvas"                 (ref Image,i32,i32,i32,i32,u32)
    clang proc     "ImageMipmaps"                      (ref Image)
    clang proc     "ImageDither"                       (ref Image,i32,i32,i32,i32)
    clang proc     "ImageFlipVertical"                 (ref Image)
    clang proc     "ImageFlipHorizontal"               (ref Image)
    clang proc     "ImageRotateCW"                     (ref Image)
    clang proc     "ImageRotateCCW"                    (ref Image)
    clang proc     "ImageColorTint"                    (ref Image,u32)
    clang proc     "ImageColorInvert"                  (ref Image)
    clang proc     "ImageColorGrayscale"               (ref Image)
    clang proc     "ImageColorContrast"                (ref Image,r32)
    clang proc     "ImageColorBrightness"              (ref Image,i32)
    clang proc     "ImageColorReplace"                 (ref Image,u32,u32)
    clang function "LoadImageColors"                   (Image)ref u32
    clang function "LoadImagePalette"                  (Image,i32,ref i32)ref u32
    clang proc     "UnloadImageColors"                 (ref u32)
    clang proc     "UnloadImagePalette"                (ref u32)
    clang function "GetImageAlphaBorder"               (Image,r32)Rectangle
    clang proc     "ImageClearBackground"              (ref Image,u32)
    clang proc     "ImageDrawPixel"                    (ref Image,i32,i32,u32)
    clang proc     "ImageDrawPixelV"                   (ref Image,Vector2,u32)
    clang proc     "ImageDrawLine"                     (ref Image,i32,i32,i32,i32,u32)
    clang proc     "ImageDrawLineV"                    (ref Image,Vector2,Vector2,u32)
    clang proc     "ImageDrawCircle"                   (ref Image,i32,i32,i32,u32)
    clang proc     "ImageDrawCircleV"                  (ref Image,Vector2,i32,u32)
    clang proc     "ImageDrawRectangle"                (ref Image,i32,i32,i32,i32,u32)
    clang proc     "ImageDrawRectangleV"               (ref Image,Vector2,Vector2,u32)
    clang proc     "ImageDrawRectangleRec"             (ref Image,Rectangle,u32)
    clang proc     "ImageDrawRectangleLines"           (ref Image,Rectangle,i32,u32)
    clang proc     "ImageDraw"                         (ref Image,Image,Rectangle,Rectangle,u32)
    clang proc     "ImageDrawText"                     (ref Image,ref i8,i32,i32,i32,u32)
    clang proc     "ImageDrawTextEx"                   (ref Image,Font,ref i8,Vector2,r32,r32,u32)
    clang function "LoadTexture"                       (ref i8)Texture
    clang function "LoadTextureFromImage"              (Image)Texture
    clang function "LoadTextureCubemap"                (Image,i32)Texture
    clang function "LoadRenderTexture"                 (i32,i32)RenderTexture
    clang proc     "UnloadTexture"                     (Texture)
    clang proc     "UnloadRenderTexture"               (RenderTexture)
    clang proc     "UpdateTexture"                     (Texture,ref void)
    clang proc     "UpdateTextureRec"                  (Texture,Rectangle,ref void)
    clang function "GetTextureData"                    (Texture)Image
    clang function "GetScreenData"                     ()Image
    clang proc     "GenTextureMipmaps"                 (ref Texture)
    clang proc     "SetTextureFilter"                  (Texture,i32)
    clang proc     "SetTextureWrap"                    (Texture,i32)
    clang proc     "DrawTexture"                       (Texture,i32,i32,u32)
    clang proc     "DrawTextureV"                      (Texture,Vector2,u32)
    clang proc     "DrawTextureEx"                     (Texture,Vector2,r32,r32,u32)
    clang proc     "DrawTextureRec"                    (Texture,Rectangle,Vector2,u32)
    clang proc     "DrawTextureQuad"                   (Texture,Vector2,Vector2,Rectangle,u32)
    clang proc     "DrawTextureTiled"                  (Texture,Rectangle,Rectangle,Vector2,r32,r32,u32)
    clang proc     "DrawTexturePro"                    (Texture,Rectangle,Rectangle,Vector2,r32,u32)
    clang proc     "DrawTextureNPatch"                 (Texture,NPatchInfo,Rectangle,Vector2,r32,u32)
    clang function "Fade"                              (u32,r32)u32
    clang function "ColorToInt"                        (u32)i32
    clang function "ColorNormalize"                    (u32)Vector4
    clang function "ColorFromNormalized"               (Vector4)u32
    clang function "ColorToHSV"                        (u32)Vector3
    clang function "ColorFromHSV"                      (r32,r32,r32)u32
    clang function "ColorAlpha"                        (u32,r32)u32
    clang function "ColorAlphaBlend"                   (u32,u32,u32)u32
    clang function "GetColor"                          (i32)u32
    clang function "GetPixelColor"                     (ref void,i32)u32
    clang proc     "SetPixelColor"                     (ref void,u32,i32)
    clang function "GetPixelDataSize"                  (i32,i32,i32)i32
    clang function "GetFontDefault"                    ()Font
    clang function "LoadFont"                          (ref i8)Font
    clang function "LoadFontEx"                        (ref i8,i32,ref i32,i32)Font
    clang function "LoadFontFromImage"                 (Image,u32,i32)Font
    clang function "LoadFontFromMemory"                (ref i8,ref byte,i32,i32,ref i32,i32)Font
    clang function "LoadFontData"                      (ref byte,i32,i32,ref i32,i32,i32)ref CharInfo
    clang function "GenImageFontAtlas"                 (ref CharInfo,ref ref Rectangle,i32,i32,i32,i32)Image
    clang proc     "UnloadFontData"                    (ref CharInfo,i32)
    clang proc     "UnloadFont"                        (Font)
    clang proc     "DrawFPS"                           (i32,i32)
    clang proc     "DrawText"                          (ref i8,i32,i32,i32,u32)
    clang proc     "DrawTextEx"                        (Font,ref i8,Vector2,r32,r32,u32)
    clang proc     "DrawTextRec"                       (Font,ref i8,Rectangle,r32,r32,i32,u32)
    clang proc     "DrawTextRecEx"                     (Font,ref i8,Rectangle,r32,r32,i32,u32,i32,i32,u32,u32)
    clang proc     "DrawTextCodepoint"                 (Font,i32,Vector2,r32,u32)
    clang function "MeasureText"                       (ref i8,i32)i32
    clang function "MeasureTextEx"                     (Font,ref i8,r32,r32)Vector2
    clang function "GetGlyphIndex"                     (Font,i32)i32
    clang function "TextCopy"                          (ref i8,ref i8)i32
    clang function "TextIsEqual"                       (ref i8,ref i8)i32
    clang function "TextLength"                        (ref i8)u32
    clang function "TextFormat"                        (ref i8,...)ref i8
    clang function "TextSubtext"                       (ref i8,i32,i32)ref i8
    clang function "TextReplace"                       (ref i8,ref i8,ref i8)ref i8
    clang function "TextInsert"                        (ref i8,ref i8,i32)ref i8
    clang function "TextJoin"                          (ref ref i8,i32,ref i8)ref i8
    clang function "TextSplit"                         (ref i8,i8,ref i32)ref ref i8
    clang proc     "TextAppend"                        (ref i8,ref i8,ref i32)
    clang function "TextFindIndex"                     (ref i8,ref i8)i32
    clang function "TextToUpper"                       (ref i8)ref i8
    clang function "TextToLower"                       (ref i8)ref i8
    clang function "TextToPascal"                      (ref i8)ref i8
    clang function "TextToInteger"                     (ref i8)i32
    clang function "TextToUtf8"                        (ref i32,i32)ref i8
    clang function "GetCodepoints"                     (ref i8,ref i32)ref i32
    clang function "GetCodepointsCount"                (ref i8)i32
    clang function "GetNextCodepoint"                  (ref i8,ref i32)i32
    clang function "CodepointToUtf8"                   (i32,ref i32)ref i8
    clang proc     "DrawLine3D"                        (Vector3,Vector3,u32)
    clang proc     "DrawPoint3D"                       (Vector3,u32)
    clang proc     "DrawCircle3D"                      (Vector3,r32,Vector3,r32,u32)
    clang proc     "DrawTriangle3D"                    (Vector3,Vector3,Vector3,u32)
    clang proc     "DrawTriangleStrip3D"               (ref Vector3,i32,u32)
    clang proc     "DrawCube"                          (Vector3,r32,r32,r32,u32)
    clang proc     "DrawCubeV"                         (Vector3,Vector3,u32)
    clang proc     "DrawCubeWires"                     (Vector3,r32,r32,r32,u32)
    clang proc     "DrawCubeWiresV"                    (Vector3,Vector3,u32)
    clang proc     "DrawCubeTexture"                   (Texture,Vector3,r32,r32,r32,u32)
    clang proc     "DrawSphere"                        (Vector3,r32,u32)
    clang proc     "DrawSphereEx"                      (Vector3,r32,i32,i32,u32)
    clang proc     "DrawSphereWires"                   (Vector3,r32,i32,i32,u32)
    clang proc     "DrawCylinder"                      (Vector3,r32,r32,r32,i32,u32)
    clang proc     "DrawCylinderWires"                 (Vector3,r32,r32,r32,i32,u32)
    clang proc     "DrawPlane"                         (Vector3,Vector2,u32)
    clang proc     "DrawRay"                           (Ray,u32)
    clang proc     "DrawGrid"                          (i32,r32)
    clang proc     "DrawGizmo"                         (Vector3)
    clang function "LoadModel"                         (ref i8)Model
    clang function "LoadModelFromMesh"                 (Mesh)Model
    clang proc     "UnloadModel"                       (Model)
    clang proc     "UnloadModelKeepMeshes"             (Model)
    clang function "LoadMeshes"                        (ref i8,ref i32)ref Mesh
    clang proc     "UnloadMesh"                        (Mesh)
    clang function "ExportMesh"                        (Mesh,ref i8)i32
    clang function "LoadMaterials"                     (ref i8,ref i32)ref Material
    clang function "LoadMaterialDefault"               ()Material
    clang proc     "UnloadMaterial"                    (Material)
    clang proc     "SetMaterialTexture"                (ref Material,i32,Texture)
    clang proc     "SetModelMeshMaterial"              (ref Model,i32,i32)
    clang function "LoadModelAnimations"               (ref i8,ref i32)ref ModelAnimation
    clang proc     "UpdateModelAnimation"              (Model,ModelAnimation,i32)
    clang proc     "UnloadModelAnimation"              (ModelAnimation)
    clang function "IsModelAnimationValid"             (Model,ModelAnimation)i32
    clang function "GenMeshPoly"                       (i32,r32)Mesh
    clang function "GenMeshPlane"                      (r32,r32,i32,i32)Mesh
    clang function "GenMeshCube"                       (r32,r32,r32)Mesh
    clang function "GenMeshSphere"                     (r32,i32,i32)Mesh
    clang function "GenMeshHemiSphere"                 (r32,i32,i32)Mesh
    clang function "GenMeshCylinder"                   (r32,r32,i32)Mesh
    clang function "GenMeshTorus"                      (r32,r32,i32,i32)Mesh
    clang function "GenMeshKnot"                       (r32,r32,i32,i32)Mesh
    clang function "GenMeshHeightmap"                  (Image,Vector3)Mesh
    clang function "GenMeshCubicmap"                   (Image,Vector3)Mesh
    clang function "MeshBoundingBox"                   (Mesh)BoundingBox
    clang proc     "MeshTangents"                      (ref Mesh)
    clang proc     "MeshBinormals"                     (ref Mesh)
    clang proc     "MeshNormalsSmooth"                 (ref Mesh)
    clang proc     "DrawModel"                         (Model,Vector3,r32,u32)
    clang proc     "DrawModelEx"                       (Model,Vector3,Vector3,r32,Vector3,u32)
    clang proc     "DrawModelWires"                    (Model,Vector3,r32,u32)
    clang proc     "DrawModelWiresEx"                  (Model,Vector3,Vector3,r32,Vector3,u32)
    clang proc     "DrawBoundingBox"                   (BoundingBox,u32)
    clang proc     "DrawBillboard"                     (Camera3D,Texture,Vector3,r32,u32)
    clang proc     "DrawBillboardRec"                  (Camera3D,Texture,Rectangle,Vector3,r32,u32)
    clang function "CheckCollisionSpheres"             (Vector3,r32,Vector3,r32)i32
    clang function "CheckCollisionBoxes"               (BoundingBox,BoundingBox)i32
    clang function "CheckCollisionBoxSphere"           (BoundingBox,Vector3,r32)i32
    clang function "CheckCollisionRaySphere"           (Ray,Vector3,r32)i32
    clang function "CheckCollisionRaySphereEx"         (Ray,Vector3,r32,ref Vector3)i32
    clang function "CheckCollisionRayBox"              (Ray,BoundingBox)i32
    clang function "GetCollisionRayMesh"               (Ray,Mesh,Matrix)RayHitInfo
    clang function "GetCollisionRayModel"              (Ray,Model)RayHitInfo
    clang function "GetCollisionRayTriangle"           (Ray,Vector3,Vector3,Vector3)RayHitInfo
    clang function "GetCollisionRayGround"             (Ray,r32)RayHitInfo
    clang function "LoadShader"                        (ref i8,ref i8)Shader
    clang function "LoadShaderCode"                    (ref i8,ref i8)Shader
    clang proc     "UnloadShader"                      (Shader)
    clang function "GetShaderDefault"                  ()Shader
    clang function "GetTextureDefault"                 ()Texture
    clang function "GetShapesTexture"                  ()Texture
    clang function "GetShapesTextureRec"               ()Rectangle
    clang proc     "SetShapesTexture"                  (Texture,Rectangle)
    clang function "GetShaderLocation"                 (Shader,ref i8)i32
    clang function "GetShaderLocationAttrib"           (Shader,ref i8)i32
    clang proc     "SetShaderValue"                    (Shader,i32,ref void,i32)
    clang proc     "SetShaderValueV"                   (Shader,i32,ref void,i32,i32)
    clang proc     "SetShaderValueMatrix"              (Shader,i32,Matrix)
    clang proc     "SetShaderValueTexture"             (Shader,i32,Texture)
    clang proc     "SetMatrixProjection"               (Matrix)
    clang proc     "SetMatrixModelview"                (Matrix)
    clang function "GetMatrixModelview"                ()Matrix
    clang function "GetMatrixProjection"               ()Matrix
    clang function "GenTextureCubemap"                 (Shader,Texture,i32,i32)Texture
    clang function "GenTextureIrradiance"              (Shader,Texture,i32)Texture
    clang function "GenTexturePrefilter"               (Shader,Texture,i32)Texture
    clang function "GenTextureBRDF"                    (Shader,i32)Texture
    clang proc     "BeginShaderMode"                   (Shader)
    clang proc     "EndShaderMode"                     ()
    clang proc     "BeginBlendMode"                    (i32)
    clang proc     "EndBlendMode"                      ()
    clang proc     "InitVrSimulator"                   ()
    clang proc     "CloseVrSimulator"                  ()
    clang proc     "UpdateVrTracking"                  (ref Camera3D)
    clang proc     "SetVrConfiguration"                (VrDeviceInfo,Shader)
    clang function "IsVrSimulatorReady"                ()i32
    clang proc     "ToggleVrMode"                      ()
    clang proc     "BeginVrDrawing"                    ()
    clang proc     "EndVrDrawing"                      ()
    clang proc     "InitAudioDevice"                   ()
    clang proc     "CloseAudioDevice"                  ()
    clang function "IsAudioDeviceReady"                ()i32
    clang proc     "SetMasterVolume"                   (r32)
    clang function "LoadWave"                          (ref i8)Wave
    clang function "LoadWaveFromMemory"                (ref i8,ref byte,i32)Wave
    clang function "LoadSound"                         (ref i8)Sound
    clang function "LoadSoundFromWave"                 (Wave)Sound
    clang proc     "UpdateSound"                       (Sound,ref void,i32)
    clang proc     "UnloadWave"                        (Wave)
    clang proc     "UnloadSound"                       (Sound)
    clang function "ExportWave"                        (Wave,ref i8)i32
    clang function "ExportWaveAsCode"                  (Wave,ref i8)i32
    clang proc     "PlaySound"                         (Sound)
    clang proc     "StopSound"                         (Sound)
    clang proc     "PauseSound"                        (Sound)
    clang proc     "ResumeSound"                       (Sound)
    clang proc     "PlaySoundMulti"                    (Sound)
    clang proc     "StopSoundMulti"                    ()
    clang function "GetSoundsPlaying"                  ()i32
    clang function "IsSoundPlaying"                    (Sound)i32
    clang proc     "SetSoundVolume"                    (Sound,r32)
    clang proc     "SetSoundPitch"                     (Sound,r32)
    clang proc     "WaveFormat"                        (ref Wave,i32,i32,i32)
    clang function "WaveCopy"                          (Wave)Wave
    clang proc     "WaveCrop"                          (ref Wave,i32,i32)
    clang function "LoadWaveSamples"                   (Wave)ref r32
    clang proc     "UnloadWaveSamples"                 (ref r32)
    clang function "LoadMusicStream"                   (ref i8)Music
    clang proc     "UnloadMusicStream"                 (Music)
    clang proc     "PlayMusicStream"                   (Music)
    clang proc     "UpdateMusicStream"                 (Music)
    clang proc     "StopMusicStream"                   (Music)
    clang proc     "PauseMusicStream"                  (Music)
    clang proc     "ResumeMusicStream"                 (Music)
    clang function "IsMusicPlaying"                    (Music)i32
    clang proc     "SetMusicVolume"                    (Music,r32)
    clang proc     "SetMusicPitch"                     (Music,r32)
    clang function "GetMusicTimeLength"                (Music)r32
    clang function "GetMusicTimePlayed"                (Music)r32
    clang function "InitAudioStream"                   (u32,u32,u32)AudioStream
    clang proc     "UpdateAudioStream"                 (AudioStream,ref void,i32)
    clang proc     "CloseAudioStream"                  (AudioStream)
    clang function "IsAudioStreamProcessed"            (AudioStream)i32
    clang proc     "PlayAudioStream"                   (AudioStream)
    clang proc     "PauseAudioStream"                  (AudioStream)
    clang proc     "ResumeAudioStream"                 (AudioStream)
    clang function "IsAudioStreamPlaying"              (AudioStream)i32
    clang proc     "StopAudioStream"                   (AudioStream)
    clang proc     "SetAudioStreamVolume"              (AudioStream,r32)
    clang proc     "SetAudioStreamPitch"               (AudioStream,r32)
    clang proc     "SetAudioStreamBufferSizeDefault"   (i32)
    global macro  RL_REALLOC(ptr,sz) = realloc(ptr,sz)
    global macro  RL_FREE(ptr) = free(ptr)
    global macro  LOC_MAP_DIFFUSE = LOC_MAP_ALBEDO
    global macro  LIGHTGRAY =  CLITERAL(200,200,200,255)
    global macro  YELLOW = CLITERAL(253,249,0,255)
    global macro  DARKGREEN = CLITERAL(0,117,44,255)
    global macro  GetExtension = GetFileExtension
    global macro  MAGENTA =  CLITERAL(255,0,255,255)
    global macro  RAD2DEG = (180.0f/PI)
    global macro  CLITERAL(r,g,b,a) = (r|(b<<8)|(g<<16)|(a<<24))
    global macro  BLANK = CLITERAL(0,0,0,0)
    global macro  PINK = CLITERAL(255,109,194,255)
    global macro  GOLD = CLITERAL(255,203,0,255)
    global macro  GRAY = CLITERAL(130,130,130,255)
    global macro  LoadText = LoadFileText
    global macro  PURPLE =  CLITERAL(200,122,255,255)
    global macro  BLUE = CLITERAL(0,121,241,255)
    global macro  PI = 3.14159265358979323846f
    global macro  DEG2RAD = (PI/180.0f)
    global macro  DARKGRAY = CLITERAL(80,80,80,255)
    global macro  ORANGE = CLITERAL(255,161,0,255)
    global macro  DARKBLUE = CLITERAL(0,82,172,255)
    global macro  SpriteFont = Font
    global macro  RL_CALLOC(n,sz) =  calloc(n,sz)
    global macro  LOC_MAP_SPECULAR = LOC_MAP_METALNESS
    global macro  MAP_SPECULAR =  MAP_METALNESS
    global macro  VIOLET =  CLITERAL(135,60,190,255)
    global macro  LIME = CLITERAL(0,158,47,255)
    global macro  RL_MALLOC(sz) = malloc(sz)
    global macro  GetImageData = LoadImageColors
    global macro  RAYWHITE =  CLITERAL(245,245,245,255)
    global macro  FormatText = TextFormat
    global macro  BEIGE =  CLITERAL(211,176,131,255)
    global macro  BROWN = CLITERAL(127,106,79,255)
    global macro  WHITE = CLITERAL(255,255,255,255)
    global macro  MAROON = CLITERAL(190,33,55,255)
    global macro  MAP_DIFFUSE = MAP_ALBEDO
    global macro  DARKBROWN =  CLITERAL(76,63,47,255)
    global macro  SKYBLUE = CLITERAL(102,191,255,255)
    global macro  BLACK = CLITERAL(0,0,0,255)
    global macro  RED = CLITERAL(230,41,55,255)
    global macro  GREEN = CLITERAL(0,228,48,255)
    global macro  DARKPURPLE = CLITERAL(112,31,126,255)
end
