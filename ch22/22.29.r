#calculate default loss
#payment: semiannual payment
#rate: risk free rate continous compound
#year: number of years
defaultloss<-function(payment,rate, year) {
    lost=0;
    for (i in 0:(2*year-1)) {
        val=sum(c(rep(payment,i),100+payment)*exp(-rate*(0:i)/2))*exp(-rate/2*(2*year-i));
        lost = lost+val;
    }
    lost
}
