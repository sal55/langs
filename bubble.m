proc sortexports([]int &sortindex)=
!Sort exporttable by name. This is done by building a set of sorted indices into
!sortindex
    psymbol d,e

!First, store 1..nexports into sortindex
    for i to nexports do
        sortindex[i]:=i
    end

!do bubble sort for now
    repeat
        int swapped:=0
        for i:=1 to nexports-1 do

            d:=exporttable[sortindex[i]].def
            e:=exporttable[sortindex[i+1]].def

            if strcmp(getbasename(d.name), getbasename(e.name))>0 then
                swapped:=1
                swap(sortindex[i], sortindex[i+1])
            end
        end
    until not swapped
end
