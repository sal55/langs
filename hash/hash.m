import clib
import mlib
import select

const version = "1.5 (M-port)"

proc start=
    ichar pm, chars
    int length

    banner()

    if nsysparams>1 then
        pm:=sysparams[2]

        if eqstring(pm, "-h") or eqstring(pm, "--help") then
            help()
        elsif eqstring(pm, "-l") then
            list_()
        else
            length:=strlen(pm)

            println "Hash:", pm
            println "Length:", length

            chars:=charset(pm)
            println "Charset:",chars
            println
            sel(length, chars)
            definite(pm, length)
        fi

    else
        fprintln "Usage: # [HASH] or -h for help", sysparams[1]
    fi

end

function hasupper(ichar s)int =
    while s^ do
        if isupper(s++^) then
            return true
        fi
    od
    return false
end

proc banner =
    println "          __"
    println "(\\,------'()'--o  Sniff.."
    println " l_ ) _    /-''    Sniff..."
    println " /_)_) /_)_)\n"
    
    println "Houndsniff - Hash Identification Program - Version", version
    println "By Michael Constantine Dimopoulos Sep 2020"
    println
end

proc list_ =
    println "\nHoundsniff supports:\n"
    println strinclude "list.txt"
end

proc definite(ichar strng, int length)=
    ref[]char str:=cast(strng)

    if str[0]='$' and str[1]='P' and str[2]='$' then
        println "[+] Definite identification: Wordpress hash"
        stop
    elsif str[0]='$' and str[1]='1' and str[2]='$' then
        println "[+] Definite identification: MD5 crypt(3)"
        stop
    elsif str[0]='$' and str[1]='5' and str[2]='$' then
        println "[+] Definite identification: SHA256 crypt(3)"
        stop
    elsif str[0]='$' and str[1]='6' and str[2]='$' then
        println "[+] Definite identification: SHA512 crypt(3)"
        stop
    elsif str[length-1]='=' then
        println "[+] Definite identification: Base64"
        stop
    elsif str[0]='$' and str[1]='a' and str[2]='p' and str[3]='r' and str[4]='1' and str[5]='$' then
        println "[+] Definite identification: APR1"
        stop
    elsif str[0]='$' and str[1]='H' and str[2]='$' then
        println "[+] Definite identification: phpBB"
        stop
    elsif str[0]='s' and str[1]='h' and str[2]='a' and str[3]='1' and str[4]='$' then
        println "[+] Definite identification: SHA1 Django"
        stop
    elsif strlen(cast(str))>31 and str[32]=':' and length=65 then
        println "[+] Definite identification: MD5 Joomla (pass:salt)"
        stop
    fi
end

proc help=
    println strinclude "hashhelp.txt"
end

function charset(ichar str)ichar =
    if strchr(str,'$') then
        return "b"
    elsif strchr(str,'/') then
        return "c"
    elsif str^='=' and (str+1)^='x' and (str+2)^='0' then
        return "d"
    elsif hasupper(str) then
        return "e"
    else
        return "a"
    fi
end
