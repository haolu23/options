#bitree
#a generic binominal tree program
#datastructure of the node:
#list
#---stockprice
#---nodeparams
#------coordination
#---path (matrix)
bitree<-function(underlying,strike,dividend,riskFree,
                 maturity,volatility,func,nstep=100)
{
    dtime=maturity/(nstep-1);
    up=exp(volatility*sqrt(dtime));
    down=1/up;
    a=exp((riskFree-dividend)*dtime);
    p=(a-down)/(up-down);
    discount=exp(-riskFree*dtime);

    cat("up=",up," down=",down," p=",p," discount=",discount,"\n");

    tree=matrix(vector("list",nstep*nstep),ncol=nstep);
    tree[1,1][[1]]=list(underlying,nodeparams=list(coordination=c(1,1)),path=matrix(c(underlying,0),ncol=2));
    #initalize tree node stock price
    for (i in 2:nstep) {
        for (j in 1:i) {
            stockprice=underlying*up^(j-1)*down^(i-j);
            tree[i,j][[1]]=list(stockprice=stockprice,nodeparams=list(coordination=c(i,j)));
            tree[i,j]=func('genpath',tree[i,j],tree[i-1,j],tree[i-1,j-1]);
        }
    }
    #walk backward of the tree
    for (i in (nstep-1):1) {
        for (j in 1:i) {
            ##have to assign back because R passes by value
            tree[i,j]=func('calcpath',tree[i,j],tree[i+1,j+1],tree[i+1,j],p,discount);
        }
    }
    tree
}


#floating lookback option
#each node is represented by a list of two items
#the first one is the stock price, second is a two-column
#matrix, the first column is path dependent stock prices,
#the second column is the corresponding option price
flookback<-function(americanOrEuro,putOrCall) {
    #disable put
    #putOrCall='put';
    intrinsicpricer=switch(putOrCall,put=function(stock,strike) pmax(strike-stock,0),
                           call=function(stock,strike) pmax(stock-strike,0));
    selector=switch(putOrCall,put=pmax, call=pmin);
    merger=switch(putOrCall,put=function(a,b) a>b,call=function(a,b) a<b );
    nodepricer=switch(americanOrEuro,euro=function(treeprice,intrinsicprice) {treeprice},
                      american=function(treeprice,intrinsicprice) pmax(treeprice,intrinsicprice));
    findoptionprice<-function(stock,path) {
        result=0;
        for (i in 1:length(path[,1])) {
            if(isTRUE(all.equal(stock , path[i,1]))) {
                result=path[i,2];
                break;
            }
        }
        result;
    }
    uniquedouble<-function(arr) {
        arr=sort(arr);
        #cat('Inside unique function',arr,"\n");
        index=2;
        i=1;
        while (i < length(arr) ) {
            if (isTRUE(all.equal(arr[i],arr[index]))) {
                arr[index]=NA;
                index=index+1;
            } else {
                i=index;
                index=i+1;
            }
        }
        #cat('After unique : ',arr,"\n")
        arr[!is.na(arr)]
    }
    function(mode,thisnode,upnode,downnode,p,discount){
        genpath<-function(thisnode,upnode,downnode) {
            prices=c();
            #browser()
            if(length(upnode)>0) {
                prices=c(prices,upnode[[1]]$path[merger(upnode[[1]]$path[,1],thisnode[[1]]$stockprice),1])
            }
            if(length(downnode)>0) {
                prices=c(prices,downnode[[1]]$path[merger(downnode[[1]]$path[,1],thisnode[[1]]$stockprice),1])
            }
            prices=uniquedouble(c(thisnode[[1]]$stockprice,prices));
            #print(prices)
            #print(thisnode)
            #print(upnode)
            #print(downnode)
            #intrinsic=pmax(prices-thisnode[[1]]$stockprice,0);
            intrinsic=intrinsicpricer(thisnode[[1]]$stockprice,prices);
            thisnode[[1]]$path=matrix(c(prices,intrinsic),ncol=2);
            thisnode
        }
        calcpath<-function(thisnode,upnode,downnode,p,discount){
            #print('++++++++++++++++++++++++++++++++++++++');
            pathprices=thisnode[[1]]$path[,1];
            optionprices=thisnode[[1]]$path[,2];
            #given one of the pathprices, find the corresponding
            #option price
            uprice=selector(pathprices,upnode[[1]]$stockprice);
            downprice=selector(pathprices,downnode[[1]]$stockprice);
            up=apply(matrix(uprice,ncol=1),1,findoptionprice,upnode[[1]]$path);
            #cat('uprice=',uprice,'\n','upnode=',upnode[[1]]$path,'\n','up=',up,'\n');
            down=apply(matrix(downprice,ncol=1),1,findoptionprice,downnode[[1]]$path);
            treeprice=(up*p+down*(1-p))*discount;
            thisnode[[1]]$path[,2]=nodepricer(treeprice,thisnode[[1]]$path[,2]);
            
            thisnode;
        }
        switch(mode,genpath=genpath(thisnode,upnode,downnode),calcpath=calcpath(thisnode,upnode,downnode,p,discount));
    }
}


#generalization mode
#this implementation is really not a general mode, but the method described
#in the book.  However, the interface of bitree is general.  
#To calculate a different path function, just implement 
#two functions "genpath" and "calcpath"
avgoption<-function(americanOrEuro) {
    nodepricer=switch(americanOrEuro,euro=function(treeprice,intrinsicprice) treeprice,
                      american=function(treeprice,intrinsicprice) pmax(treeprice,intrinsicprice));
    function(thisnode,upnode,downnode,p,discount) {
        pathfunc<-function(thisprice,upprice,downprice) {
        }
        typicalvals<-function(min,max) {
            seq(min,max,length.out=20);
        }
        pathaverage<-function(thisprice,depth,anotherprice) {
            (thisprice*depth+anotherprice)/(depth+1)
        }
        genpath<-function(thisnode,upnode,downnode) {
            upPathPrices=upnode[[1]]$path[,1];
            downPathPrices=downnode[[1]]$path[,1];

            nodedepth=upnode[[1]]$nodeparams$coordination[1];
            aggregration=pathaverage(c(upPathPrices,downPathPrices),thisnode[[1]]$stockprice,nodedepth);
            minp=min(aggregration);
            maxp=max(aggregration);
            thisPathPrices=typicalvals(minp,maxp);
            intrinsic=pmax(thisnode[[1]]$stockprice-thisPathPrices,0);
            thisnode[[1]]$path=matrix(c(thisPathPrices,intrinsic),ncol=2);
            thisnode
        }
        pricer<-function(prices,nextlevel) {
        }
        calcpath<-function(thisnode,upnode,downnode,p,discount) {
            nodedepth=thisnode[[1]]$nodeparams$coordination[1];
            uprices=pathaverage(thisnode[[1]]$path[,1],upnode[[1]]$stockprice,nodedepth);
            downprices=pathaverage(thisnode[[1]]$path[,1],downnode[[1]]$stockprice,nodedepth);
            upoptionprices=pricer(uprices,upnode[[1]]$path)
        }
        switch(mode,genpath=genpath(thisnode,upnode,downnode),calcpath=calcpath(thisnode,upnode,downnode,p,discount));
    }
}
