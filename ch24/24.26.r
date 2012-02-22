source('24.r');

#up-and-out call, stock price is a vector
#or we could do
#apply(underlyings,1,cupoutoption(...),strike,barrier,dividend,riskFree,maturity,volatility)
cupoutoptionstocks<-function(underlyings, strike, barrier,dividend,
                    riskFree,maturity, volatility) {
    value = rep(0,length(underlyings));
    delta = rep(0,length(underlyings));
    for (i in 1:length(underlyings)) {
        value[i]=cupoutoption(underlyings[i], strike, barrier,dividend, riskFree,maturity, volatility);
    }
    delta=diff(value)/diff(underlyings);
    value;
    list(value=value,delta=delta);
}


#up-and-out call, maturity is a vector
#or we could do
#apply(maturities,1,function (m,...) cupoutoption(maturity=m,...),underlying, strike, barrier, dividend, riskFree,volatility)
cupoutoptionmaturities<-function(underlying, strike, barrier,dividend,
                                 riskFree,maturities, volatility) {
    value = rep(0,length(maturities));
    for (i in 1:length(maturities)) {
        value[i]=cupoutoption(underlying, strike,
                              barrier,dividend,
                              riskFree,maturities[i], volatility);
    }
    value;
}


#up-and-out call, volatily is a vector
cupoutoptionvols<-function(underlying, strike, barrier,dividend,
                                 riskFree,maturity,
                                 volatilities) {
    value = rep(0,length(volatilities));
    for (i in 1:length(volatilities)) {
        value[i]=cupoutoption(underlying, strike,
                              barrier,dividend,
                              riskFree,maturity,volatilities[i]);
    }
    value;
}
