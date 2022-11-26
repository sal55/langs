!(case-insensitive syntax)

    const sdl_init_video = 0x20
    const sdl_centered = 0x2FFF0000
    const sdl_renderer_presentvsync = 4
    const sdl_quitx = 0x100

    const w = 1024, h = 768

    importdll sdl2 =
        func "SDL_Init" (u32)i32
        func "SDL_CreateWindow" (stringz, i32, i32, i32, i32, u32)ref void
        func "SDL_CreateRenderer" (ref void, i32, u32)ref void
        func "SDL_PollEvent" (ref void)i32
        func "SDL_SetRenderDrawColor" (ref void,i32,i32,i32,i32)i32
        func "SDL_RenderFillRect"     (ref void,ref void)i32
        proc "SDL_RenderPresent"      (ref void)
        proc "SDL_Quit"
    end

    type sdl_event = struct
        u32 etype
        [64]byte dummy
    end

    type sdl_rect = struct
        i32 x,y,w,h
    end

    fun randz(n) = random(0..n-1)

    proc main=
        if sdl_init(sdl_init_video) then
            abort("SDL error")
        fi

        wnd := sdl_createwindow("Example", sdl_centered, sdl_centered, w, h, 0)

        if not wnd then
            abort("SDL window error")
        fi

        r := sdl_createrenderer(wnd, -1, sdl_renderer_presentvsync)
        if not r then
            abort("SDL Render error")
        fi

        do
            e:=new(sdl_event)
            while sdl_pollevent(&e) do
                if e.etype=sdl_quitx then
                    sdl_quit()
                    stop
                fi
            od

            sdl_setrenderdrawcolor(r, randz(256), randz(256), randz(256), 255)

            rect:=new(sdl_rect)
            rect.x:=randz(w)
            rect.y:=randz(h)
            rect.w:=randz(w-rect.x)
            rect.h:=randz(h-rect.y)

            sdl_renderfillrect(r, &rect)
            sdl_renderpresent(r)
        od

    end
