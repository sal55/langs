!Port of raytrace C demo

const ss        = 2
const n         = 1024
const levels    = 5
const delta     = 0.0001

record Vector =
    real x
    real y
    real z
end

record Scenet =
    Vector center
    real radius
    ref[0:]Scenet child
end

Vector neglight

function vec(real x, y, z)Vector r=
    r.x := x
    r.y := y
    r.z := z
    return r
end

function add(Vector a, b)Vector =
    return vec(a.x+b.x,a.y+b.y,a.z+b.z)
end

function subx(Vector a, b)Vector =
    return vec(a.x-b.x,a.y-b.y,a.z-b.z)
end

function scale(real a, Vector b)Vector =
    return vec(a*b.x,a*b.y,a*b.z)
end

function dot(Vector a, b)real =
    return a.x*b.x+a.y*b.y+a.z*b.z
end

function unitise(Vector a)Vector =
    scale(1.000000/sqrt(dot(a,a)), a)
end

function ray_sphere(Vector o, d, c, real r)real =
    Vector v
    real b, disc, t1, t2

    v := subx(c,o)
    b := dot(v,d)
    disc := b*b-dot(v,v)+r*r
    if disc < 0 then
        return Infinity
    fi
    disc := sqrt(disc)
    t2 := b+disc
    if t2 < 0 then
        return Infinity
    fi
    t1 := b-disc
    if t1 > 0 then
        return t1
    fi
    return t2
end

proc intersect(Vector o, d, ref real lambda, ref ref Scenet t, ref Scenet scene) =
    real lambda2
    int i

    lambda2 := ray_sphere(o,d,scene^.center,scene^.radius)
    if lambda2<lambda^ then
        if scene.child then
            for i:=0 to 4 do
                intersect(o,d,lambda,t,&scene^.child[i])
            od
        else
             lambda^ := lambda2
             t^ := scene
        fi
    fi
end

function ray_trace(Vector o, d, Scenet scene)real =
    real lambda
    ref Scenet t
    Vector p, normal
    real g

    lambda := Infinity
    t := nil
    intersect(o,d,&lambda,&t,&scene)
    if lambda=Infinity then
        return 0.000000
    fi
    p := add(o,scale(lambda,d))
    normal := unitise(subx(p,t^.center))
    g := dot(normal,neglight)
    if g<=0.000000 then
        return 0.000000
    fi
    p := add(p,scale(delta,normal))
    lambda := Infinity
    intersect(p,neglight,&lambda,&t,&scene)
    if lambda<Infinity then
        return 0.0
    fi
    return g
end

function create(int level, Vector c, real r)Scenet =
    Scenet scene
    real rn

    scene.center := c
    if level=1 then
        scene.radius := r
        scene.child := nil
    else
        rn := 3*r/sqrt(12)
        scene.radius := 3*r
        scene.child := malloc(5*scenet.bytes)
        scene.child[0] := create(1,c,r)
        scene.child[1] := create(level-1,add(c,scale(rn,vec(-1,1,-1))),r/2)
        scene.child[2] := create(level-1,add(c,scale(rn,vec(1,1,-1))),r/2)
        scene.child[3] := create(level-1,add(c,scale(rn,vec(-1,1,1))),r/2)
        scene.child[4] := create(level-1,add(c,scale(rn,vec(1,1,1))),r/2)
    fi
    return scene
end

proc main=
    Scenet scene
    real g
    Vector d
    filehandle f

    neglight := unitise(vec(1,3,-2))
    scene := create(levels,vec(0,-1,0),1)

    f:=fopen("fred.pgm","wb")
    println @f,"P5"
    println @f,n,n
    println @f,"255"

    for y:=n-1 downto 0 do
        for x:=0 to n-1 do
            g := 0.000000
            for dx:=0 to ss-1 do
                for dy:=0 to ss-1 do
                    d := unitise(vec(x+dx*1.000000/ss-n/2.000000,y+dy*1.000000/ss-n/2.000000,n))
                    g +:= ray_trace(vec(0,0,-4),d,scene)
                od
            od
            outbyte(f,0.500000+255.000000*g/sqr(ss))
        od
    od
    fclose(f)
end
