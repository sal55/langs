record sqlite = (int dummy)

importdll sqlite3 =
    clang function sqlite3_open(ichar, ref ref sqlite)int32
    clang function sqlite3_exec(ref sqlite, ichar, ref void, ref void, ref ichar)int32
    clang function sqlite3_close(ref sqlite)int32
end

int counter

global proc sel(int length, ichar charset) =
    ref sqlite db
    ichar err_msg := nil
    [1150]char sql
    int32 rc

    rc := sqlite3_open("hashes", &db)

    fprint @sql,
        "SELECT name FROM hashes WHERE length=#"+
        " AND charset='#'"+
        " ORDER BY popularity DESC;", length, charset
    rc := sqlite3_exec(db, &.sql, callbackfn, nil, &err_msg)
    sqlite3_close(db)
    
end

function callbackfn(ref void unused, int n, ref[]ichar args, colnames)int =
    ichar s
    
    ++counter

    for i to n do
        s:=(args[i]|args[i]|"NULL")
        if counter=1 then
            fprint "Possible results:\n\n[#] #", counter, s
        else
            fprint "[#] #", counter, s
        fi
    od
    println
    return 0
end
