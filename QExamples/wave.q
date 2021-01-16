!experimenting with WAV formats:
! - reading and writing wav files
! - converting to accessible array format
! - creating (synthesising) wav data
! - playing wav data via SDL

import sys
import clib
import files

type sdl_audiospec = struct
    int32       freq
    word16      format
    byte        channels
    byte        silence
    word16      samples
    word16      padding
    word32      size
    ref byte    callbackfn
    ref byte    userdata
end

record waverec =
    var length          !number of samples
    var freq            !samples/second
    var channels        !1 or 2 channels

    var data            ![]int16
        var ldata@data
    var rdata           !data arrays are () when not used

    var pdata           ![]byte packed representation same as in .wav file
    var samplebytes     !1 or 2 in packed format (3-byte formats reduced to 2)

    method duration(&self)=
        return self.length/self.freq
    end

    method bytelength(&self)=
        return self.length*self.channels*self.samplebytes
    end
end

importdll sdl2 =
    clang function "SDL_Init"           (word32)int32
    clang function "SDL_LoadWAV_RW"     (ref byte,i32,ref sdl_audiospec,
                                         ref byte, ref U32)Ref sdl_audiospec
    clang function "SDL_RWFromFile"     (string,string)ref void
    clang function "SDL_OpenAudio"      (ref sdl_audiospec, ref sdl_audiospec)i32
    clang proc     "SDL_CloseAudio"
    clang function "SDL_QueueAudio"     (u32, ref byte, u32)i32
    clang proc     "SDL_PauseAudio"     (i32)
    clang function "SDL_GetAudioStatus" ()i32
end

var errormess

proc start=
    if ncmdparams>=2 then
        file:=cmdparams[2]
    else
        println "Play WAV file using SDL"
        println "Usage:"
        println "     pc wave filename[.wav]"
        stop
    fi

    file:=addext(file,"wav")

    println "Reading WAV file:",file

    if mess:=playwavfile(file) then
        abort(mess)
    fi
end

proc wavetest=
    w:=makewavesound()
    playwavesound(w)
end

global function playwavfile(file)=

    if not (w:=readwavfile(file)) then
        return file+":"+errormess
    fi

    if not playwavesound(w) then
        return file+":"+errormess
    fi
    return ""
end

function readwavfile(file)=
    mess:=""
    w:=readwavfile2(file,mess)
    if mess then
        if w then free(w) fi
        errormess:=mess
        return nil
    fi
    return w
end

function readwavfile2(file,&mess)=
    p:=readblockfile(file)

    if p=nil then
        mess:="Can't load file"
        return nil
    fi
    if readfilesize<30 then
        mess:="Not WAV format"
        return p
    fi

    chunkid:=peek(p,word32)
    chunksize:=peek(p+4,word32)
    format:=peek(p+8,word32)
    if chunkid<>'RIFF' or format<>'WAVE' then
        mess:="Not WAV format"
        return p
    fi

    q:=p+12

    n:=0
    repeat
        subchunkid:=peek(q,word32)
        subchunksize:=peek(q+4,word32)
        if subchunkid='fmt ' then exit fi
        q:=q+subchunksize+8
    until ++n=20

    audioformat:=peek(q+8,word16)
    channels:=peek(q+10,word16)
    samplerate:=peek(q+12,word32)
    byterate:=peek(q+16,word32)
    blockalign:=peek(q+20,word16)
    bitspersample:=peek(q+22,word16)

    dataoffset:=subchunksize+8

    subchunk2id:=peek(q+dataoffset,word32)
    datasize:=peek(q+dataoffset+4,word32)
    pdata:=q+dataoffset+8

    if subchunk2id<>'data' then
        q:=q+dataoffset+datasize+8
        subchunk2id:=peek(q,word32)
        datasize:=peek(q+4,word32)
        pdata:=q+8
        if subchunk2id<>'data' then
            mess:="data chunk not found"
            return p
        fi
    fi

    if audioformat<>1 then
        mess:="Format not supported "+tostr(audioformat)
        return p
    fi
    if channels not in 1..2 then
        mess:="Too many channels:"+tostr(channels)
        return p
    fi

    w:=new(waverec,0)
    w.freq:=samplerate
    w.channels:=channels
    w.samplebytes:=bitspersample%8

    w.pdata:=new(array,byte,datasize)
    memcpy(&w.pdata,pdata,datasize)
    w.length:=datasize%w.samplebytes%w.channels

    if w.samplebytes=3 then
        pd:=makeref(&w.pdata,byte)
        q:=makeref(pdata,byte)
        to w.length do
            to w.channels do
                ++q
                pd++^:=q++^
                pd++^:=q++^

            od
        od
        w.samplebytes:=2

    fi

    free(p)
    return w
end

function peek(p,t=byte)=
    p:=makeref(p,t)
    return p^
end

function getaudiospec(w)=
!convert my waverec to an (sdl_audiospec,data,length)
    ws:=new(sdl_audiospec)
    ws.freq:=w.freq
    ws.channels:=w.channels
    ws.samples:=4096
    case w.samplebytes
    when 1 then
        ws.format:=8
    when 2 then
        ws.format:=0x8010
    else
        abort("samplebytes?")
    esac

    if w.pdata=nil then
        if w.channels<>1 then
            abort("gas/not mono")
        fi
        if w.samplebytes<>1 then
            abort("gas/not 8-bit")
        fi
        w.pdata:=new(array,byte,w.data.len)
        for i to w.data.len do
            w.pdata[i]:=w.data[i]
        od
        w.length:=w.data.len
    fi

    return (ws, &w.pdata,w.bytelength())
end

function playwavesound(w)=
    (ws,data,length):=getaudiospec(w)

    if sdl_openaudio(ws,0)<0 then
        errormess:="Couldn't open audio"
        return 0
    fi

    sdl_queueaudio(1,data,length)

    sdl_pauseaudio(0)

    waitsec(w.duration())
    sdl_closeaudio()

    return 1
end

function makewavesound=
    w:=new(waverec,0)
    w.freq:=11025
    w.channels:=1
    w.samplebytes:=1

    data:=new(array,byte,10000,0)

    for i to data.len do
        data[i]:=i.[2]*255
    od
    w.data:=data

    return w
end

function newwave=
!create empty wave data
    w:=new(waverec,0)
    w.freq:=12000
    w.channels:=1
    w.samplebytes:=1

    w.data:=new(array,byte,0)
    return w
end

proc addnote(w,freq, length, volume=1.0)=
    samplesperwave:=w.freq/freq

    halfwavesamples:=int(samplesperwave/2)

    nwaves:=int((length/1000)*freq)

    totalsamples:=samplesperwave*nwaves

    to nwaves do
        to halfwavesamples do
            w.data append:=int(255*volume)
        od
        to halfwavesamples do
            w.data append:=0
        od
    od
end
