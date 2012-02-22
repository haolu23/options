#calculate exchange option
# eqn (24.3)
#
exchangeoption<-function(sigmau,sigmav,rho,u0,v0,qu,qv,r,t) {
    sigma=sqrt(sigmau^2+sigmav^2-2*rho*sigmau*sigmav);
    d1=(log(v0/u0)+(qu-qv+sigma^2/2)*t)/sigma/sqrt(t);
    d2=d1-sigma*sqrt(t);
    v0*exp(-qv*t)*pnorm(d1)-u0*exp(-qu*t)*pnorm(d2)
}

#cash-or-nothing call binary option
#
cbinaryoption<-function(s0,k,r,q,sigma,t,Q) {
    d1=(log(s0/k)+(r-q+sigma^2/2)*t)/sigma/sqrt(t);
    d2=d1-sigma*sqrt(t);
    Q*exp(-r*t)*pnorm(d2);
}

#down-and-in call
cdioption<-function(underlying, strike, barrier,dividend,
                    riskFree,maturity, volatility) {
    if (barrier <= strike) {
        lambda=(riskFree-dividend+volatility^2/2)/volatility^2;
        y=log(barrier^2/underlying/strike)/volatility/sqrt(maturity)+lambda*volatility*sqrt(maturity);
        underlying*exp(-dividend*maturity)*(barrier/underlying)^(2*lambda)*pnorm(y)-strike*(barrier/underlying)^(2*lambda-2)*pnorm(y-volatility*sqrt(maturity));
    }
    else {
        EuropeanOption('call',underlying,strike,dividend,riskFree,maturity,volatility)$value-cdooption(underlying, strike, barrier,dividend,
                    riskFree,maturity, volatility);
    }

}

#down-and-out call
cdooption<-function(underlying, strike, barrier,dividend,
                    riskFree,maturity, volatility) {
    if (barrier > strike) {
        lambda=(riskFree-dividend+volatility^2/2)/volatility^2;
        x1=log(underlying/barrier)/volatility/sqrt(maturity)+lambda*volatility*sqrt(maturity);
        y1=log(barrier/underlying)/volatility/sqrt(maturity)+lambda*volatility*sqrt(maturity)
        underlying*pnorm(x1)*exp(-dividend*maturity)-strike*exp(-riskFree*maturity)*pnorm(x1-volatility*sqrt(maturity))-underlying*exp(-dividend*maturity)*(barrier/underlying)^(2*lambda)*pnorm(y1)+strike*exp(-riskFree*maturity)*(barrier/underlying)^(2*lambda-2)*pnrom(y1-volatility*sqrt(maturity));
    }
    else {
        EuropeanOption('call',underlying,strike,dividend,riskFree,maturity,volatility)$value-cdioption(underlying, strike, barrier,dividend,
                    riskFree,maturity, volatility);
    }
}

#up-and-out call
cupoutoption<-function(underlying, strike, barrier,dividend,
                    riskFree,maturity, volatility) {
    if (barrier <= strike) {
        0
    }
    else {
        EuropeanOption('call',underlying,strike,dividend,riskFree,maturity,volatility)$value-cupinoption(underlying, strike, barrier,dividend,
                    riskFree,maturity, volatility);
    }
}


#up-and-in call
cupinoption<-function(underlying, strike, barrier,dividend,
                    riskFree,maturity, volatility) {
    if (barrier > strike) {
        lambda=(riskFree-dividend+volatility^2/2)/volatility^2;
        x1=log(underlying/barrier)/volatility/sqrt(maturity)+lambda*volatility*sqrt(maturity);
        y1=log(barrier/underlying)/volatility/sqrt(maturity)+lambda*volatility*sqrt(maturity)
        y=log(barrier^2/underlying/strike)/volatility/sqrt(maturity)+lambda*volatility*sqrt(maturity);
        underlying*pnorm(x1)*exp(-dividend*maturity)-strike*exp(-riskFree*maturity)*pnorm(x1-volatility*sqrt(maturity))-underlying*exp(-dividend*maturity)*(barrier/underlying)^(2*lambda)*(pnorm(-y)-pnorm(-y1))+strike*exp(-riskFree*maturity)*(barrier/underlying)^(2*lambda-2)*(pnorm(-y+volatility*sqrt(maturity))-pnorm(-y1+volatility*sqrt(maturity)));
    }
    else {
        EuropeanOption('call',underlying,strike,dividend,riskFree,maturity,volatility)$value;
    }
}

#floating lookback call
flcoption<-function(underlying, minunderlying,dividend,
                    riskFree,maturity, volatility) {
    a1=(log(underlying/minunderlying)+(riskFree-dividend+volatility^2/2)*maturity)/volatility/sqrt(maturity)
    a2=a1-volatility*sqrt(maturity);
    a3=(log(underlying/minunderlying)+(-riskFree+dividend+volatility^2/2)*maturity)/volatility/sqrt(maturity);
    y1=-2*(riskFree-dividend-volatility^2/2)*log(underlying/minunderlying)/volatility^2
    underlying*exp(-maturity*dividend)*pnorm(a1)-underlying*exp(-dividend*maturity)*volatility^2/2/(riskFree-dividend)*pnorm(-a1)-minunderlying*exp(-riskFree*maturity)*(pnorm(a2)-volatility^2/2/(riskFree-dividend)*exp(y1)*pnorm(-a3))
}

#average price call
apcoption<-function(underlying, strike,dividend,
                    riskFree,maturity, volatility) {
    m1=(exp((riskFree-dividend)*maturity)-1)/(riskFree-dividend)/maturity*underlying
    m2=2*exp((2*riskFree-2*dividend+volatility^2)*maturity)*underlying^2/(riskFree-dividend+volatility^2)/(2*riskFree-2*dividend+volatility^2)/maturity^2+2*underlying^2/(riskFree-dividend)/maturity^2*(1/(2*riskFree-2*dividend+volatility^2)-exp((riskFree-dividend)*maturity)/(riskFree-dividend+volatility^2))
    sigma=sqrt(log(m2/m1^2)/maturity);
    EuropeanOption('call',m1,strike,riskFree,riskFree,maturity,sigma);
}

#variance swap
#underlying, current value of underlying
#strikes, a vector of strike prices
#dividend, dividend rate of underlying
#riskFree, risk free rate
#maturity, maturity of this swap
#variancestrike, strike variance
#impliedvolatilities, impliedvolatilities of corresponding
#strikes
varswap<-function(underlying,strikes,dividend,riskFree,maturity,variancestrike,impliedvolatilities) {
    strikediffs=rep(0,length(strikes));
    strikediffs[1]=strikes[2]-strikes[1];
    strikediffs[length(strikes)]=strikes[length(strikes)]-strikes[length(strikes)-1];
    for (i in 2:(length(strikes)-1)) {
        strikediffs[i]=0.5*(strikes[i+1]-strikes[i-1]);
    }
    #print(strikediffs);
    futureprice=underlying*exp((riskFree-dividend)*maturity);
    underlyingthredshold=strikes[which.max(strikes[strikes<futureprice]-futureprice)];
    expectedAvgVar=log(futureprice/underlyingthredshold)-futureprice/underlyingthredshold+1;
    #print(paste('expectedAvgVar=',expectedAvgVar));
    for (i in 1:length(strikes)) {
        if (strikes[i]<underlyingthredshold) {
            putorcall=EuropeanOption('put',underlying,strikes[i],dividend,riskFree,maturity,impliedvolatilities[i])$value;
        } else if (strikes[i]>underlyingthredshold){
            putorcall=EuropeanOption('call',underlying,strikes[i],dividend,riskFree,maturity,impliedvolatilities[i])$value;
        } else {
            putorcall=(EuropeanOption('call',underlying,strikes[i],dividend,riskFree,maturity,impliedvolatilities[i])$value+EuropeanOption('put',underlying,strikes[i],dividend,riskFree,maturity,impliedvolatilities[i])$value)/2;
        }
        expectedAvgVar=expectedAvgVar+strikediffs[i]/strikes[i]^2*exp(riskFree*maturity)*putorcall;
        #print(paste('putorcall=',putorcall))
        #print(paste('expectedAvgVar=',expectedAvgVar));
    }
    expectedAvgVar=expectedAvgVar*2/maturity;
    (expectedAvgVar-variancestrike)*exp(-riskFree*maturity);
}


#static construction of hedging 
#up-and-out option
#output: matrix elements from top to down are weights of static
#options with increasing maturities
upoutoptionhedge<-function(underlying, strike, barrier,dividend,
                           riskFree,maturity, volatility, nhedges)
{
    maturities=matrix(seq(maturity/nhedges,maturity,length.out=nhedges),nrow=1);
    #print(maturities);
    #calculate option A's values at different boundaries
    va=apply(maturities,c(1,2),function (m,...)
             EuropeanOption(maturity=m,'call',...)$value,barrier,strike,dividend,riskFree,volatility);
    va=matrix(rev(-va),nrow=1,byrow=TRUE);
    #print(va);

    #calculate other options's values at different boundaries
    ma=apply(maturities,c(1,2),function (m,...)
             EuropeanOption(maturity=m,'call',...)$value,barrier,barrier,dividend,riskFree,volatility);
    #print(ma);
    #shift the value matrix to create a nhedges by nhedges
    #matrix
    maa=matrix(rep(0,nhedges*nhedges),nrow=nhedges);
    for (i in 1:nhedges) {
        #print(c(rep(0,i-1),ma[seq(1,nhedges-i+1)]))
        maa[i,]=c(rep(0,i-1),ma[seq(1,nhedges-i+1)])
    }
    #calculate other options' weights
    weight=solve(maa,t(va))
    #calculate other options' delta,gamma,vega
    alls=apply(maturities,c(1,2),function (m,...)
             EuropeanOption(maturity=m,'call',...),underlying,barrier,dividend,riskFree,volatility);
    deltas=apply(alls,c(1,2),function (l) l[[1]]$delta);
    gammas=apply(alls,c(1,2),function (l) l[[1]]$gamma);
    vegas=apply(alls,c(1,2),function (l) l[[1]]$vega);
    values=apply(alls,c(1,2),function (l) l[[1]]$value);
    #print(weight)
    #calculate option A's delta,gamma,vega
    aoption=EuropeanOption('call',underlying,strike,dividend,riskFree,maturity,volatility)
    #print(deltas%*%weight);
    value=values%*%weight+aoption$value
    delta=deltas%*%weight+aoption$delta
    gamma=gammas%*%weight+aoption$gamma
    vega=vegas%*%weight+aoption$vega
    list(value=value,weight=weight,delta=delta,gamma=gamma,vega=vega)
}
