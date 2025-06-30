**var** *charmap* =  
 "AðšBð›CðœDðEðžFðŸGð Hð¡Ið¢Jð£Kð¤Lð¥Mð¦Nð§Oð¨Pð©QðªRð«Sð¬Tð­Uð®Vð¯Wð°Xð±Yð²Zð³"  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ "Að€BðCð‚DðƒEð„Fð…Gð†Hð‡IðˆJð‰KðŠLð‹MðŒNðOðŽPðQðRð‘Sð’Tð“Uð”Vð•Wð–Xð—Yð˜Zð™0ðŸŽ1ðŸ2ðŸ3ðŸ‘4ðŸ’5ðŸ“6ðŸ”7ðŸ•8ðŸ–9ðŸ—"  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ "að‘Žbð‘cð‘dð‘‘eð‘’fð‘“gð‘”hâ„Žið‘–jð‘—kð‘˜lð‘™mð‘šnð‘›oð‘œpð‘qð‘žrð‘Ÿsð‘ tð‘¡uð‘¢vð‘£wð‘¤xð‘¥yð‘¦zð‘§"  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;+ "að´bðµcð¶dð·eð¸fð¹gðºhð»ið¼jð½kð¾lð¿mð‘€nð‘oð‘‚pð‘ƒqð‘„rð‘…sð‘†tð‘‡uð‘ˆvð‘‰wð‘Šxð‘‹yð‘Œzð‘*Ã—*Â·%Ã·#Â¢";  
  
**var** *mapfrom* = ("â‰¤", "â‰¥", "â‰ ", "Â¬", "âˆ§", "âˆ¨", "â†");  
**var** *mapto* = ("<=", ">=", "/=", " NOT ", " AND ", " OR ", ":=");  
  
**func** *next_unicode_char_length*(*s*, *pos*) =  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**if** *pos* > *s*.**len** **then**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**elsif** *s*.[*pos*]<128 **then**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;1  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**else**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*result* := 1  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**while** *result* + *pos* <= *s*.**len** **and** *s*.[*result*+*pos*] **in** 129..191 **do**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;++*result*  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**od**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*result*  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**fi**  
**end**  
  
**proc** *bold_to_upper_strop*(*line*) =  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*inquotes* := *false*  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*pos* := 1  
  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**while** *pos* <= *line*.**len** **do**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*length* := *next_unicode_char_length*(*line*, *pos*)  
  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*chunk* := *line*[*pos* .. *pos*+*length*-1]  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**if** *length* = 1 **or** *inquotes* **then**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**print** *chunk*  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**if** *chunk* = """" **then**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*inquotes* := **not** *inquotes*  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**fi**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**elsif** *mappos* := *chunk* **inx** *charmap* **then**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**print** *charmap*[*mappos*-1]  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**else**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*mappos* := *mapto*.**len**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**while** *mappos*>0 **and** *mapfrom*[*mappos*] <> *chunk* **do**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;--*mappos*  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**od**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**print** (*mappos* | *mapto*[*mappos*] | *chunk*)  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**fi**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*pos* +:= *length*  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**od**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**println**  
**end**  
  
**proc** *main* =  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**if** *ncmdparams*<1 **then**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**println** "Usage:", *cmdparams*[1], "filename"  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**stop**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**fi**  
  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*lines* := *readtextfile*(*cmdparams*[1])  
  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**if** *lines* = 0 **then**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*abort*("File not found:"+*cmdparams*[1])  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**fi**  
  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**for** *line* **in** *lines* **do**  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*bold_to_upper_strop*(*line*)  
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**od**  
**end**  
&nbsp;
