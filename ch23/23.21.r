#Gaussian copula model of time to default
#given default probability at time t: qt
#copula correlation: rho
#factor level: f
gcopula<-function(qt,rho,f) {
    pnorm((qnorm(qt)-sqrt(rho)*f)/sqrt(1-rho))
}
