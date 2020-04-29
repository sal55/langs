[0..20631]***word*** *Q*\
***word*** *carry*  = 36243678541\
***word*** *xcng*   = 12367890123456\
***word*** *xs*     = 521288629546311\
***word*** *indx*   = *Q*.***len***\
\
***function*** *refill*:***word*** =\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;***word*** *h*,*z*\
\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;***for*** *i* ***in*** *Q* ***do***\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*h* := *carry* ***iand*** 1\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*z* := (*Q*[*i*]<<41)>>1 + (*Q*[*i*]<<39)>>1 + *carry*>>1\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*carry* :=  *Q*[*i*]>>23 + *Q*[*i*]>>25 + *z*>>63\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*Q*[*i*] := ***inot*** (*z*<<1+*h*)\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;***od***\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*indx*:=1\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*Q*[*Q*.***lwb***]\
***end***\
\
***function*** *kiss*:***word*** =\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*supr*()+*cng*()+*xxs*()\
***end***\
\
***function*** *supr*:***word*** *s*=\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;***if*** *indx* <= *Q*.***upb*** ***then***\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*Q*[*indx*++]\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;***else***\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*refill*()\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;***fi***\
***end***\
\
***function*** *xxs*:***word*** =\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*xs* :=*xs* ***ixor*** *xs*<<13\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*xs* :=*xs* ***ixor*** *xs*>>17\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*xs* :=*xs* ***ixor*** *xs*<<43\
***end***\
\
***function*** *cng*:***word*** =\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*xcng*:=***word***(6906969069) \* *xcng* + ***word***(123)\
***end***\
\
***proc*** *start*=\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;***word*** *x*\
\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;***for*** *i* ***in*** *Q* ***do***\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*Q*[*i*] := *cng*() + *xxs*()\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;***od***\
\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;***to*** 10 *million* ***do***\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*x*:=*kiss*()\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;***od***\
\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;***println*** "Does x=4013566000157423768"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;***println*** "     x=",,*x*\
***end***\
&nbsp;
