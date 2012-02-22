#prob function
source('../ch22/22.28.r');

# total payment of CDS
# assuming yearly payment,
# default in the middle of year
# CDS buyer has to pay
#
cdspayment<-function(q,n,r) {
    total=0;
    for (i in 0:n) {
        p=prob(q,i);
        pv=exp(-r*(i+1))*p[2];
        accuralpv=exp(-r*(i+0.5))*p[1]/2;
        total=total+pv+accuralpv;
    }
    total
}

#calculate CDS payoff, CDS saler has to pay
cdspayoff<-function(q,n,r,recovery) {
    total=0;
    for (i in 0:n) {
        p=prob(q,i);
        payoffpv=exp(-r*(i+0.5))*p[1]*(1-recovery);
        #print(payoffpv);
        total=total+payoffpv;
    }
    total
}

targetfun<-function(q,n,r,spread,recovery) {
    #n=4;
    #r=0.05;
    #spread=0.1;
    #recovery=0.4;
    abs(cdspayment(q,n,r)*spread-cdspayoff(q,n,r,recovery));
}

optim(0.2,targetfun,gr=NULL,4,0.05,0.1,0.4)
optim(0.2,targetfun,gr=NULL,4,0.05,0.1,0.2)
