cclib rectd

record point=(real32 x,y)
record rect=(real32 x,y,width,height)
record rect2=(real32 x,y,width,height,a,b)

importdll rectd=
	clang proc "PrintPoint" (point)	
	clang proc "PrintRect" (ref rect)	
	clang proc "PrintRect2" (ref rect2)	
	clang function "NewPoint":point
	clang proc "NewRect"(ref rect)
	clang proc "NewRect2"(ref rect2)
end

proc start=
	point p := (30,40)
	rect r := (3,4,5,6)
	rect2 r2 := (3,4,5,6,7,8)

	p:=newpoint()
	newrect(&r)
	newrect2(&r2)

	printpoint(p)
	printrect(&r)
	printrect2(&r2)

end
