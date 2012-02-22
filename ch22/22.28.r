lost=0;
for (i in 0:5) {
    val =
    sum(c(rep(3.5,i),103.5)*(1/1.02)^(0:i))*0.55/1.02^(6-i) ;
    print(val);
    lost = lost + val;
}

#unconditional probability of default
#conditional probability q,
#number of years n
prob<-function(q,n) {
    if (n==0) {
        c(q,1-q)
    } else {
        sn=prob(q,n-1);
        d=q*sn[2];
        c(d,-d+sn[2]);
    }
}

targetfunc<-function(q) {
    lost=0;
    for (i in 0:5) {
        prb=prob(q,5-i);
        val =
        prb[1]*sum(c(rep(3.5,i),103.5)*(1/1.02)^(0:i))*0.55/1.02^(6-i) ;
        lost=lost+val;
    }
    lost-108.402+105.508
}
