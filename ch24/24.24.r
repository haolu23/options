source('24.r');
strikes=seq(800,1200,by=50);
vol= seq(0.2,0.24,by=0.005);
varswap(1020,strikes,0.01,0.04,3/12,0.045,vol)
