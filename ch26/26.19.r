source('26.r');
source('../24.r');
flcoption(400,400,0.04,0.06,9/12,0.2)
#this is VERY slow
a<-bitree(400,30,0.04,0.06,9/12,0.2,flookback('euro','call'),100)
