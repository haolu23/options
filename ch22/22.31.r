d1<-function(v0,d,r,sigmav,t) {
    (log(v0/d)+(r+sigmav^2/2)*t)/sigmav/sqrt(t)
}

d2<-function(v0,d,r,sigmav,t){
    d1(v0,d,r,sigmav,t)-sigmav*sqrt(t)
}

#Merton model, return the differences
merton<-function(e0,v0,d,r,sigmav,t,sigmae) {
    d11=d1(v0,d,r,sigmav,t);
    x1=e0-v0*pnorm(d11)+d*exp(-r*t)*pnorm(d2(v0,d,r,sigmav,t))
    x2=sigmae*e0-pnorm(d11)*sigmav*v0
    c(x1,x2);
}

#optim target function, minimize square of merton model
#differences
targetfun<-function(x){
    v0=x[1];
    sigmav=x[2];
    #parameters taken from book
    sum(merton(4,v0,15,0.06,sigmav,2,0.6)^2)
}
