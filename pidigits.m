!(Bignumber library for integers and floats)

import clib
import msys
import mlib
import mbignum

proc start=
	int n:=0

	if nsysparams>1 then
		n:=strtoint(sysparams[2])
	fi
	if n<1 then n:=300 fi

	pidigits(n)
end

proc pidigits(int ndecimals)=
	bignum w,n1,n2,d,f10,n10,u,v
	bignum t1,t2,t3
	bignum k2minus1, k2plus1, kminus1,kplus2
	int i,k,k2
	int count

	w:=bn_makeint(0)
	n1:=bn_makeint(4)
	n2:=bn_makeint(3)
	d:=bn_makeint(1)
	f10:=bn_makeint(10)
	n10:=bn_makeint(-10)

	t1:=bn_init()
	t2:=bn_init()
	t3:=bn_init()
	u:=bn_init()
	v:=bn_init()

	k:=1
	i:=0
	count:=0

	do
		++count
		bn_idiv(u,n1,d)
		bn_idiv(v,n2,d)

		if bn_toint(u)=bn_toint(v) then
			print chr(bn_toint(u)+'0')
			++i
			if i=1 then
			else
				if (i rem 10)=0 then println "	:",,I fi
			fi
			if i=ndecimals then exit fi

			bn_mul(u,n10,u)
			bn_mul(u,d,u)
			bn_mul(n1,n1,f10)
			bn_add(n1,n1,u)

			bn_mul(n2,n2,f10)
			bn_add(n2,n2,u)

		else
			k2:=k<<1
			k2minus1:=bn_makeint(k2-1)
			k2plus1:=bn_makeint(k2+1)
			kminus1:=bn_makeint(k-1)
			kplus2:=bn_makeint(k+2)

			bn_mul(u,n1,k2minus1)
			bn_add(v,n2,n2)

			bn_mul(w,n1,kminus1)
			bn_add(n1,u,v)

			bn_mul(u,n2,kplus2)
			bn_add(n2,w,u)

			bn_mul(d,d,k2plus1)

			++k

			bn_free(k2minus1)
			bn_free(k2plus1)
			bn_free(kminus1)
			bn_free(kplus2)

		fi
	od
	println
end
