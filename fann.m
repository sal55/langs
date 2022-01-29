function fyjrsr(int n)int=
	int signx,maxflips,sum
	int i,j,k
	int q1,flips,qq,t,sx,tt
	[100]int p,q,s

	signx:=1
	maxflips:=0
	sum:=0

	for i:=1 to n do
		p[i]:=i
		q[i]:=i
		s[i]:=i
	od

	do

		q1:=p[1]
		if q1<>1 then
			for i:=2 to n do q[i]:=p[i] od
			flips:=1
			do
				qq:=q[q1]
				if qq=1 then
					sum+:=signx*flips
					if flips>maxflips then
							maxflips:=flips
					fi
					exit
				fi
				q[q1]:=q1
				if q1>=4 then
					i:=2; j:=q1-1
					repeat
						swap(q[i],q[j])
						++i
						--j
					until i>=j
				fi
				q1:=qq
				++flips
			od
		fi

		if signx=1 then
			swap(p[2],p[1])
			signx:=-1
		else
			swap(p[2],p[3])
			signx:=1
			for i:=3 to n do
				sx:=s[i]
				if sx<>1 then s[i]:=sx-1; exit fi
				if i=n then
					res2:=maxflips
					return sum
				fi
				s[i]:=i
				tt:=p[1]

				for j:=1 to i do
					p[j]:=p[j+1]
				od

				p[i+1]:=tt
			od
		fi
	od

	return 0
end
