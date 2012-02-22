#using binominal tree to calculate prices of american option and
#european option
bino<-function(type,underlying,strike,dividend,riskFree,maturity,vol,nsteps) {
    pricetree<-matrix(0,nsteps,nsteps);
    optiontree=pricetree;
    dt=maturity/(nsteps-1);
    u=exp(vol*sqrt(dt));
    d=1/u;
    discount=exp(-riskFree*dt);
    #initalize pricetree 
    #dividend can be a number or a matrix if it is a number,
    #we assume it is a dividend yield. if it is a matrix,
    #we assume dividend is paid cash, and function cashdividend is used
    #to recaculate pricetree. 
    if(length(dividend)==1) {
        a=exp((riskFree-dividend)*dt);
        for(i in 1:nsteps) {
            for(j in 1:i) {
                pricetree[i,j]=underlying*d^(i-j)*u^(j-1);
            }
        }
    } else if (is.matrix(dividend)) {
        a=exp(riskFree*dt);
        pricetree=cashdividend(underlying,dividend,u,riskFree,maturity,nsteps);
    }
    p=(a-d)/(u-d);
    #initalize optiontree with intrinsic values
    if (type == "call") {
        optiontree=callintrinsic(pricetree,strike);
    } else if (type == "put"){
        optiontree=putintrinsic(pricetree,strike);
    } else {
        optiontree=xsquarepayoff(pricetree,strike);
    }

    #print(optiontree);
    amxoption=optiontree;
    #calculate option price
    for(i in (nsteps-1):1){
        for(j in 1:i) {
            #print(paste("i=",i," j=",j));
            optiontree[i,j]=discount*((1-p)*optiontree[i+1,j]+p*optiontree[i+1,j+1]);
            amxoption[i,j]=max(amxoption[i,j],discount*((1-p)*amxoption[i+1,j]+p*amxoption[i+1,j+1]));
        }
    }
    european<-list(option<-optiontree,d<-delta(pricetree,optiontree));
    american<-list(option<-amxoption,d<-delta(pricetree,amxoption));
    obj<-list(price=pricetree,euro=european,us=american);
    obj
}

#calculate price tree given cash dividends payments
#parameter dividends is a matrix with two columns, the first one
#is dividend payment, second is ex-dividend date
cashdividend<-function(underlying,dividends,up,riskfree,maturity,nsteps) {
    pricetree<-matrix(0,nsteps,nsteps);
    dt<-maturity/(nsteps-1);
    #assume vector, first one is dividend amount, second is
    #ex-dividend date, third is current time
    func<-function(v) {
        if(length(v)!=3){
            0
        }
        else{
            v[1]*exp(-riskfree*(v[2]-v[3]))
        }
    };
    sstar=underlying-sum(apply(cbind(dividends,0),1,func));
    for(i in 1:nsteps) {
        for(j in 1:i) {
            div=cbind(matrix(dividends[(i-1)*dt<dividends[,2],],ncol=2),i*dt);
            #print(paste("i=",i," j=",j,sum(div)));
            pricetree[i,j]=sstar*up^(2*j-1-i)+sum(apply(div,1,func));
        }
    }
    pricetree
}

delta<-function(pricetree,optiontree) {
    (optiontree[2,1]-optiontree[2,2])/(pricetree[2,1]-pricetree[2,2]);
}

#calculate option price using different steps of binominal tree
#method
binos<-function(type,underlying,strike,dividend,riskFree,maturity,vol,nsteps) {
    us=0;
    euro=0;
    j=1;
    for (i in nsteps) {
        obj=bino(type,underlying,strike,dividend,riskFree,maturity,vol,i);
        euro[j]=obj$euro[1][[1]][1,1];
        us[j]=obj$us[1][[1]][1,1];
        j=j+1;
    }
    list(us=us,euro=euro);
}

#calculate implied volatility using binominal tree method
impliedvol<-function(type,price,underlying,strike,dividend,riskFree,maturity,vol,nsteps) {
    calcerror=price;
    impliedvol=vol;
    high=10*vol;
    low=0;
    while (calcerror>10^-3) {
        obj=bino(type,underlying,strike,dividend,riskFree,maturity,impliedvol,nsteps);
        usprice=obj$us[1][[1]][1,1];
        europrice=obj$euro[1][[1]][1,1];
        if (usprice>price) {
            high=impliedvol;
            impliedvol=(impliedvol+low)/2;
        } else {
            low=impliedvol;
            impliedvol=(impliedvol+high)/2;
        }
        calcerror=abs(usprice-price);
        #print(paste("vol=",impliedvol," error=",calcerror));
    }
    impliedvol
}

stopcondition<-function(target,result) {
    if (abs((target-result)/result) < 10^-3) {
        return TRUE;
    } else {
        return FALSE;
    }
}

impliedvols<-function(type,price,underlying,strike,dividend,riskFree,maturity,vol,nsteps) {
    #library(doParallel);
    #registerDoParallel(cores=2);
    vols=0;
    j=1;
    for(i in nsteps) #%dopar%{
    {
        vols[j]=impliedvol(type,price,underlying,strike,dividend,riskFree,maturity,vol,i);
        j=j+1;
    }
    vols
}
#payoff functions
callintrinsic<-function(s,k) {
    pmax(s-k,0)
}

putintrinsic<-function(s,k) {
    pmax(k-s,0);
}

xsquarepayoff<-function(s,k) {
    s^2;
}
