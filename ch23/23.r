########
# contains all functions required in ch23
########

#calculate expected tranche principal at time tau_j conditional
#on the value of factor f
#alphal : attachment
#alphah : detachment
#q : default probability
#total : number of corporate bonds
#recovery : recovery rate
# eqn (23.11)
# TODO : something wrong with this one, result doesn't aggree
# with book
exptranchprinc<-function(alphal,alphah,q,total,recovery) {
    lower=boundaryp(alphal,total,recovery);
    upper=boundaryp(alphah,total,recovery);
    result=rep(0,length(q));
    for (i in 0:(lower-1)) {
        item=(dbinom(i,total,q));
        #print(item);
        result=result+item;
    }
    for (k in lower:(upper-1)) {
        item=(dbinom(k,total,q)*(alphah-k*(1-recovery)/total)/(alphah-alphal));
        #print(item);
        result=result+item;
    }
    result;
}

#used to calculate attachment or detachment points
boundaryp<-function(alpha,total,recovery) {
    ceiling(alpha*total/(1-recovery));
}

#calculate Q(t), default probability on time t
#with average default intensity lambda
#eqn (23.9)
qt<-function(lambda,t){
    1-exp(-lambda*t)
}

#discount factor
discount<-function(r,t){
    exp(-r*t);
}
#Gaussian copula model of time to default Q(t|F)
#given default probability at time t: qt
#copula correlation: rho
#factor level: f
#eqn (23.8)
gcopula<-function(q,rho,f) {
    pnorm((qnorm(q)-sqrt(rho)*f)/sqrt(1-rho))
}

#expected payment
payment<-function(f,T,lambda,rho,alphas,r,total,recovery) {
    a=0;
    for (i in 2:(length(T))) {
        qj=gcopula(qt(lambda,T[i]),rho,f);
        qj_1=gcopula(qt(lambda,T[i-1]),rho,f);
        a=a+(T[i]-T[i-1])*exptranchprinc(alphas[1],alphas[2],qj,total,recovery)*discount(r,T[i]);
    }
    a*exp(-f^2/2)
}

#accural payment
accural<-function(f,T,lambda,rho,alphas,r,total,recovery) {
    b=rep(0,length(f));
    #print(f);
    for (i in 2:(length(T))) {
        #print(paste('lambda=',lambda));
        qj=gcopula(qt(lambda,T[i]),rho,f);
        #print(qj);
        qj_1=gcopula(qt(lambda,T[i-1]),rho,f);
        b=b+0.5*(T[i]-T[i-1])*(exptranchprinc(alphas[1],alphas[2],qj_1,total,recovery)-exptranchprinc(alphas[1],alphas[2],qj,total,recovery))*discount(r,(T[i]+T[i-1])/2);
    }
    b*exp(-f^2/2)
}

#expected payoff
payoff<-function(f,T,lambda,rho,alphas,r,total,recovery) {
    c=0;
    for (i in 2:(length(T))) {
        qj=gcopula(qt(lambda,T[i]),rho,f);
        qj_1=gcopula(qt(lambda,T[i-1]),rho,f);
        c=c+(exptranchprinc(alphas[1],alphas[2],qj_1,total,recovery)-exptranchprinc(alphas[1],alphas[2],qj,total,recovery))*discount(r,(T[i]+T[i-1])/2);
    }
    c*exp(-f^2/2)
}

#calculate trenche spread with given factor, need further
#integration over factors to get real trenche spread 
#eqn(23.15) 
#T payment time, vector
#lambda, default density
#rho copula correlation
#alphas, one trench
#r, risk free rate
#total, number of bonds,
#recovery, recovery rate,
tspreads<-function(T,lambda,rho,alphas,r,total,recovery) {
    spreads=rep(0,length(alphas));
    for (i in 2:length(alphas)) {
        p=integrate(payment,-Inf,Inf,T,lambda,rho,c(alphas[i-1],alphas[i]),r,total,recovery)$value/sqrt(2*pi);
        po=integrate(payoff,-Inf,Inf,T,lambda,rho,c(alphas[i-1],alphas[i]),r,total,recovery)$value/sqrt(2*pi);
        ac=integrate(accural,-Inf,Inf,T,lambda,rho,c(alphas[i-1],alphas[i]),r,total,recovery)$value/sqrt(2*pi);
        print(paste('p=',p,'po=',po,'ac=',ac));
        spreads[i]=po/(p+ac);
    }
    spreads;
}




