# Work out the offsets and overall size of a struct that needs C alighnment
# 'd' is a symbol table entry for the struct

....
maxalign:=1
index:=1

scanstruct(1, fieldlist, index, size, 0, ttcaligned[m], maxalign, 2)

if ttcaligned[m] then
    size:=roundoffset(size,maxalign)
    d.maxalign:=maxalign
else
    d.maxalign:=1
fi
....


proc scanstruct(
    int smode,
    []symbol &fields,
    int &index, &isize, offset, calign, &maxalign, countmode)=

#   smode=1/0 for structmode/unionmode
#   index is next field in fieldlist
#   offset = current offset
#   maxalign = current max alignment, which can be updated
#   isize returns the size of this span
#   countmode=2/1/0 to with counting top-level/nested/union fields
#   (& fields are reference parameters which update the caller's variables)

    symbol f
    int newoffset, fieldsize, alignment
    int nfields, structmode, ndepth, size

    size:=0

    while f:=fields[index++] do
        case f.nameid
        when structfieldid then
            converttype(f.mode)
            fieldsize:=ttsize[f.mode]

            if calign then
                alignment:=getalignment(f.mode)
                maxalign max:=alignment
                newoffset:=roundoffset(offset, alignment)
                size+:=newoffset-offset
            else
                newoffset:=offset
            fi
            f.fieldoffset:=newoffset
            offset:=newoffset
            ++nallfields
            if countmode then
                structfields[++ntopfields]:=f

            fi
        when structblockid then
            scanstruct(1, fields, index, fieldsize, offset, calign, maxalign,countmode)

        when unionblockid then
            scanstruct(0, fields, index, fieldsize, offset, calign, maxalign, (countmode|1|0))

        when endblockid then
            isize:=size
            return
        esac

        if smode then
            offset+:=fieldsize
            size+:=fieldsize
        else
            size:=max(size,fieldsize)
            countmode:=0
        fi

    od

    isize:=size             !end of fields; tread as endblock
end

global function getalignment(int m)int=
#   return alignment needed for type m, as 1,2,4,8
    int a

    case ttbasetype[m]
    when tcarray then
        return getalignment(tttarget[m])
    when tstruct then
        return ttnamedef[m].maxalign
    esac

    a:=ttsize[m]
    case a
    when 1,2,4,8 then
        return a
    esac
    pcerror("Getalign not 1248: #: #", ttname[m], a)

    return 0
end

global function roundoffset(int offset, alignment)int=
    int mask

    if alignment=1 then return offset fi
    mask:=alignment-1
    while offset iand mask do ++offset od

    return offset
end
