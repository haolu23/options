#calculate american option with explicit finite difference
#method
americano<-function(type,underlying,strike,dividend,riskFree,maturity,vol,ntimestep,prices){
    pricesteps=length(prices);
    ds=(prices[pricesteps]-prices[1])/(pricesteps-1);
    print(paste('pricesteps = ',pricesteps,'ds = ',ds));
    dt=maturity/ntimestep;
    #ds=strike/pricesteps*10;
    #initalize the matrix
    if (type=='call'){
        #intrinsic price for different prices
        dif=prices-strike;
        dif[dif<0]<-0;
        f<-matrix(rep(dif,ntimestep),pricesteps,ntimestep);
        #when price->0
        f[1,1:ntimestep]=0;
        #when price->infinity
        f[pricesteps,1:ntimestep]=prices[pricesteps];
    } else {
        dif=strike-(1:pricesteps)*ds;
        dif[dif<0]<-0;
        f<-matrix(rep(dif,ntimestep),pricesteps,ntimestep);
        f[1,1:ntimestep]=strike;
        f[pricesteps,1:ntimestep]=0;
    }
    euro=f;
    print(euro);
    # calculation
    const=1/(1+riskFree*dt);
    for(i in (ntimestep-1):1) {
        for(j in 2:(pricesteps-1)) {
            aj=const*(-0.5*(riskFree-dividend)*j*dt+1/2*vol^2*j^2*dt);
            bj=const*(1-vol^2*j^2*dt);
            cj=const*(0.5*(riskFree-dividend)*j*dt+1/2*vol^2*j^2*dt);
            euro[j,i]=aj*euro[j-1,i+1]+bj*euro[j,i+1]+cj*euro[j+1,i+1];
            #American option
            f[j,i]=max(f[j,i],aj*f[j-1,i+1]+bj*f[j,i+1]+cj*f[j+1,i+1]);
        }
    }
    opt<-list(american=f,european=euro);
    opt
}
