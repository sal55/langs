**var** *q*=*new*(**list**,0..20631)\
**var** *carry*  = 36243678541*u*\
**var** *xcng*   = 12367890123456*u*\
**var** *xs*     = 521288629546311*u*\
**var** *indx*   = *Q*.len\
\
**function** *refill* =\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**for** *i* in *Q* **do**\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*h* := *carry* iand 1\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*z* := (*Q*[*i*]<<41)>>1 + (*Q*[*i*]<<39)>>1 + *carry*>>1\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*carry* :=  *Q*[*i*]>>23 + *Q*[*i*]>>25 + *z*>>63\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*Q*[*i*] := inot (*z*<<1+*h*)\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**od**\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*indx*:=1\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**return** *Q*[*Q*.lwb]\
**end**\
\
**function** *kiss* =\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**return** *supr*()+*cng*()+*xxs*()\
**end**\
\
**function** *supr*=\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**if** *indx* <= *Q*.upb **then**\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**return** *Q*[*indx*++]\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**else**\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**return** *refill*()\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**fi**\
**end**\
\
**function** *xxs* =\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*xs* :=*xs* ixor *xs*<<13\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*xs* :=*xs* ixor *xs*>>17\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*xs* :=*xs* ixor *xs*<<43\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**return** *xs*\
**end**\
\
**function** *cng* =\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*xcng*:=word(6906969069) \* *xcng* + word(123)\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**return** *xcng*\
**end**\
\
**proc** *start*=\
\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**for** *i* in *Q* **do**\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*Q*[*i*] := *cng*() + *xxs*()\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**od**\
\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**to** 10 *million* **do**\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*x*:=*kiss*()\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**od**\
\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**println** "Does x = 4013566000157423768"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**println** "     x =",*x*\
**end**\
&nbsp;
