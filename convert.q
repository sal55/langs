var charmap =
 "A𝐚B𝐛C𝐜D𝐝E𝐞F𝐟G𝐠H𝐡I𝐢J𝐣K𝐤L𝐥M𝐦N𝐧O𝐨P𝐩Q𝐪R𝐫S𝐬T𝐭U𝐮V𝐯W𝐰X𝐱Y𝐲Z𝐳"
        + "A𝐀B𝐁C𝐂D𝐃E𝐄F𝐅G𝐆H𝐇I𝐈J𝐉K𝐊L𝐋M𝐌N𝐍O𝐎P𝐏Q𝐐R𝐑S𝐒T𝐓U𝐔V𝐕W𝐖X𝐗Y𝐘Z𝐙0𝟎1𝟏2𝟐3𝟑4𝟒5𝟓6𝟔7𝟕8𝟖9𝟗"
        + "a𝑎b𝑏c𝑐d𝑑e𝑒f𝑓g𝑔hℎi𝑖j𝑗k𝑘l𝑙m𝑚n𝑛o𝑜p𝑝q𝑞r𝑟s𝑠t𝑡u𝑢v𝑣w𝑤x𝑥y𝑦z𝑧"
        + "a𝐴b𝐵c𝐶d𝐷e𝐸f𝐹g𝐺h𝐻i𝐼j𝐽k𝐾l𝐿m𝑀n𝑁o𝑂p𝑃q𝑄r𝑅s𝑆t𝑇u𝑈v𝑉w𝑊x𝑋y𝑌z𝑍*×*·%÷#¢";

var mapfrom = ("≤", "≥", "≠", "¬", "∧", "∨", "←");
var mapto = ("<=", ">=", "/=", " NOT ", " AND ", " OR ", ":=");

func next_unicode_char_length(s, pos) =
    if pos > s.len then
        0
    elsif s.[pos]<128 then
        1
    else
        result := 1
        while result + pos <= s.len and s.[result+pos] in 129..191 do
            ++result
        od
        result
    fi
end

proc bold_to_upper_strop(line) =
    inquotes := false
    pos := 1

    while pos <= line.len do
        length := next_unicode_char_length(line, pos)

        chunk := line[pos .. pos+length-1]
    
        if length = 1 or inquotes then
            print chunk
            if chunk = """" then
                inquotes := not inquotes
            fi
        elsif mappos := chunk inx charmap then
            print charmap[mappos-1]
        else
            mappos := mapto.len
            while mappos>0 and mapfrom[mappos] <> chunk do
                --mappos
            od
            print (mappos | mapto[mappos] | chunk)
        fi
        pos +:=length
    od
    println
end

proc main =
    if ncmdparams<1 then
        println "Usage: qq prog filename"
        stop
    fi

    lines := readtextfile(cmdparams[1])

    if lines = 0 then
        abort("File not found: " + cmdparams[1])
    fi

    for line in lines do
        bold_to_upper_strop(line)
    od
end
