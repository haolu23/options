targetfun<-function(volatility) {
    underlying=1;
    strike1=1.05;
    strike2=1.10;
    volatility1=0.134;
    volatility2=0.143;
    dividend=riskFree=0.05;
    maturity=0.5;
    value=EuropeanOption('call',underlying,strike1,dividend,riskFree,maturity,volatility1)$value-EuropeanOption('call',underlying,strike2,dividend,riskFree,maturity,volatility2)$value;

    abs(EuropeanOption('call',underlying,strike1,dividend,riskFree,maturity,volatility)$value-EuropeanOption('call',underlying,strike2,dividend,riskFree,maturity,volatility)$value-value)
}

optim(0.138,targetfun)
