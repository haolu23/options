source('23.r');
#calculate kth-to-default CDS probability at one factor
#f, factor
#lambda, default density
#t, time
#rho, coupla correlation
#k, k-th to default
#total, total number of bonds
kthdefault<-function(f,t,rho,r,k,total) {
    exp(-f^2/2)*(pbinom(k,total,gcopula(q2330(t[1]),r,f))-pbinom(k,total,gcopula(q2330(t[2]),r,f)));
}

q2330<-function(t) {
    q<-c(0.02,0.0196,0.0192,0.0188,0.0184);
    sum(q[0:t]);
}
