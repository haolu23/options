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
