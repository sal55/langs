fn fyjref(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fgqbjy(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fmikaf(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fengeu(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn flhhdo(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn frhunz(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fnitaw(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fakxpk(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fdaguo(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn firota(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fktexl(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fkiaec(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fwuozk(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fflhbg(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fzacum(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fniydb(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fcucea(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fnkgal(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fvnqzz(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fojkni(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fztine(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fwmqko(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fwbnbe(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fryrun(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn frarmx(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn flglyl(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fxjlev(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fuvpsb(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn frkdym(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fmwxxl(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fjayer(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fbcwaw(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fzhafs(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fgrvdv(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fmuvsk(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fwgjjx(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fvwpqc(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fusdpx(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fwmpbl(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn faoupj(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fxkpcm(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fwdecc(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn ffxjim(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fhsfat(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fzmrky(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn ferwlq(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn frrjfx(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fmsdhq(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn faavfy(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fxvlen(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn flwcoa(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fxcaiw(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fpgucp(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fmapho(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fstorc(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fsvkku(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fcefbq(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fgylgf(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fycjnl(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fhydvw(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fkbubh(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fymrro(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fxemfd(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fgbftv(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fiycpj(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fwfnar(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fqesel(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn favhzz(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fgptfw(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fmrmlg(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fvgyro(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fsthjf(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fqfgfz(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fnrfdx(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fcyehr(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fxnxoa(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fnhycz(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fhbwaz(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fcobkp(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fjqsfm(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fzuyyr(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fcqvae(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fvoney(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fzhabh(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fsnclb(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fpzmwu(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fkxwky(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fvjwjs(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fptlzp(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fbdwty(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn faarab(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fntkag(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fawwbk(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fbvlwm(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn flqaok(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fwtnaq(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fdnuzh(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fhjniw(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fziaxu(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}

fn fnekht(n:i32)->(i32,i32) {
    let mut sign:i32=1;
    let mut maxflips:i32=0;
    let mut sum:i32=0;
    let mut i:i32;
    let mut j:i32;
    let mut q1:i32;
    let mut qq:i32;
    let mut t:i32;
    let mut sx:i32;
    let mut flips:i32;
    let mut p:[i32;20]=[0;20];
    let mut q:[i32;20]=[0;20];
    let mut s:[i32;20]=[0;20];

    for i in 1..=n {
        p[i as usize] = i;
        q[i as usize] = i;
        s[i as usize] = i;
    }
    
    loop {
        
        q1=p[1];
        if q1!=1 {
            for i in 2..=n { q[i as usize] = p[i as usize]; }
            flips=1;
            loop {
                qq=q[q1 as usize];
                if qq==1 {
                    sum=sum+sign*flips;
                    if flips>maxflips {
                        maxflips=flips;
                    }
                    break;
                }
                q[q1 as usize]=q1;
                if q1>=4 {
                    i=2; j=q1-1;
                    loop {
                        t=q[i as usize];
                        q[i as usize]=q[j as usize];
                        q[j as usize]=t;
                        i=i+1;
                        j=j-1;
                        if i>=j {break;}
                    }
                }
                q1=qq;
                flips=flips+1;
            }
        }
        
        
        if sign==1 {
            t=p[2]; p[2]=p[1]; p[1]=t;
            sign=-1;
        } else {
            t=p[2]; p[2]=p[3]; p[3]=t;
            sign=1;
            for i in 3..=n {
                sx=s[i as usize];
                if sx!=1 {s[i as usize]=sx-1; break; }
                if i==n {
                    return (sum, maxflips);
                }
                s[i as usize]=i;
                t=p[1];
                for j in 1..=i {
                    p[j as usize] = p[(j+1) as usize];
                }
                p[(i+1) as usize]=t;
            }
        }
    }
    
}


fn main() {
fyjref(5);
fgqbjy(5);
fmikaf(5);
fengeu(5);
flhhdo(5);
frhunz(5);
fnitaw(5);
fakxpk(5);
fdaguo(5);
firota(5);
fktexl(5);
fkiaec(5);
fwuozk(5);
fflhbg(5);
fzacum(5);
fniydb(5);
fcucea(5);
fnkgal(5);
fvnqzz(5);
fojkni(5);
fztine(5);
fwmqko(5);
fwbnbe(5);
fryrun(5);
frarmx(5);
flglyl(5);
fxjlev(5);
fuvpsb(5);
frkdym(5);
fmwxxl(5);
fjayer(5);
fbcwaw(5);
fzhafs(5);
fgrvdv(5);
fmuvsk(5);
fwgjjx(5);
fvwpqc(5);
fusdpx(5);
fwmpbl(5);
faoupj(5);
fxkpcm(5);
fwdecc(5);
ffxjim(5);
fhsfat(5);
fzmrky(5);
ferwlq(5);
frrjfx(5);
fmsdhq(5);
faavfy(5);
fxvlen(5);
flwcoa(5);
fxcaiw(5);
fpgucp(5);
fmapho(5);
fstorc(5);
fsvkku(5);
fcefbq(5);
fgylgf(5);
fycjnl(5);
fhydvw(5);
fkbubh(5);
fymrro(5);
fxemfd(5);
fgbftv(5);
fiycpj(5);
fwfnar(5);
fqesel(5);
favhzz(5);
fgptfw(5);
fmrmlg(5);
fvgyro(5);
fsthjf(5);
fqfgfz(5);
fnrfdx(5);
fcyehr(5);
fxnxoa(5);
fnhycz(5);
fhbwaz(5);
fcobkp(5);
fjqsfm(5);
fzuyyr(5);
fcqvae(5);
fvoney(5);
fzhabh(5);
fsnclb(5);
fpzmwu(5);
fkxwky(5);
fvjwjs(5);
fptlzp(5);
fbdwty(5);
faarab(5);
fntkag(5);
fawwbk(5);
fbvlwm(5);
flqaok(5);
fwtnaq(5);
fdnuzh(5);
fhjniw(5);
fziaxu(5);
fnekht(5);
}
