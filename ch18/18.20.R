callintrinsic<-function(s,k) {
    d=s-k;
    ifelse(d>0,d,0);
}

putintrinsic<-function(s,k) {
    d=k-s;
    ifelse(d>0,d,0);
}
    
coption<-function(k) {
    p=0.356;
    exp(-0.08/12)*(p*callintrinsic(24,k)+(1-p)*callintrinsic(18,k))
}

poption<-function(k) {
    p=0.356;
    exp(-0.08/12)*(p*putintrinsic(24,k)+(1-p)*putintrinsic(18,k))
}

cimplied<-function(k) {
    value=coption(k);
    r<-0;
    for (i in 1:length(k)) {
        r[i]<-AmericanOptionImpliedVolatility('call', value[[i]], 20, k[[i]], 0, 0.011, 1/12, 0.1)$impliedVol;
    }
    r
}

pimplied<-function(k) {
    value=poption(k);
    r<-0;
    for (i in 1:length(k)) {
        r[i]<-AmericanOptionImpliedVolatility('put', value[[i]], 20, k[[i]], 0, 0.011, 1/12, 0.1)$impliedVol;
    }
    r
}
