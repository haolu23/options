monte<-function(correlationmatrix) {
    size=sqrt(length(correlationmatrix));
    q=chol(correlationmatrix);
    port=0;
    port0=EuropeanOption('call',50,51,0,0.06,9/12,0.28)$value+EuropeanOption('put',20,19,0,0.06,1,0.25)$value;
    t=10/365;
    for (i in 1:10000) {
        sample=rnorm(size);
        newsample=q%*%sample;
        s1=50*exp((0.06-0.28^2/2)*t+0.28*newsample[1]*sqrt(t));
        s2=20*exp((0.06-0.25^2/2)*t+0.25*newsample[1]*sqrt(t));
        port[i]=EuropeanOption('call',s1,51,0,0.06,9/12,0.28)$value+EuropeanOption('put',s2,19,0,0.06,1,0.25)$value;
    }
    port-port0;
}
c<-matrix(c(1,0.4,0.4,1),ncol=2);
dp=monte(c);
print((sort(dp))[100])
