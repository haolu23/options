 t<-read.csv('Exch&IndData.csv',blank.lines.skip=TRUE,header=TRUE);
 aud<-t['AUD'][[1]];
 audoffby1<-aud[2:length(aud)];
 audret=(audoffby1-aud[1:(length(aud)-1)])/aud[1:(length(aud)-1)];
 optresult=optim(c(0.1,1),funcwrapper)
 print(optresult);

 funcwrapper<-function(lambda) {
     func(lambda,audret);
 }

 func<-function (lambda, v) {
     vi<-c(v[1]^2,rep(0,length(v)-26));
     betai<-c(beta25(v[1:25]),rep(0,length(v)-26));

     for (i in 2:(length(v)-24)) {
         betai[i]=beta25(v[i:(i+24)]);
         vi[i]=ewma(vi[i-1],v[i],lambda);
     }
     #print(paste('beta=',betai));
     #print(paste('v=',vi));
     sum((betai-vi)^2)
 }

 ewma<-function(sigman1, un1, lambda) {
     lambda*sigman1^2 + (1-lambda)*un1^2
 }

 beta25<-function(v) {
     var(v);
 }
