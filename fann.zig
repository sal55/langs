const print = @import("std").debug.print;

var res2:i32=0;

pub fn fann(n:usize) i32 {
    var p=[20]usize {0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0};
    var q=[20]usize {0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0};
    var s=[20]usize {0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0};

    var sign:i32=1;
    var maxflips:i32=0;
    var sum:i32=0;
    var i:usize=0;
    var j:usize=0;
    var q1:usize=0;
    var flips:i32=0;
    var qq:usize=0;
    var t:usize=0;
    var tt:usize=0;
    var sx:usize=0;

    i=1;
    while (i<=n) {
        p[i]=i;
        q[i]=i;
        s[i]=i;
        i+=1;
    }

    while (true) {
        q1=p[1];
        if (q1!=1) {
            i=2;
            while (i<=n) { q[i]=p[i]; i=i+1;}
            flips=1;
            while (true) {
                qq=q[q1];
                if (qq==1) {
                    sum+=sign*flips;
                    if (flips>maxflips) {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1]=q1;
                if (q1>=4) {
                    i=2; j=q1-1;
                    while (true) {
                        t=q[i]; q[i]=q[j]; q[j]=t;
                        i+=1;
                        j-=1;
                        if (i>=j) {break;}
                    }
                }
                q1=qq;
                flips+=1;
            }
        }

        if (sign==1) {
            t=p[1]; p[1]=p[2]; p[2]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            i=3;
            while (i<=n) {
                sx=s[i];
                if (sx!=1) { s[i]=sx-1; break; }
                if (i==n) {
                    res2=maxflips;
                    return sum;
                }
                s[i]=i;
                tt=p[1];
                j=1;
                while (j<=i) {
                    p[j]=p[j+1]; j+=1;
                }
                p[i+1]=tt;
                i+=1;
            }
        }
    }

    return 0;
}

pub fn main() !void {
    var n: usize = 11;
    var res: i32=0;

    res=fann(n);

    print("{d} ", .{res});
    print("{d}\n", .{res2});
}
