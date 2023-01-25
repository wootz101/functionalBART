# practicing Functional Bart Model MAtrix



### Clear Console ###
cat("\014")

### Clear Environment ### 
rm(list = ls(all = TRUE))
setwd("~/Desktop/Functional Data")
library(FADPclust)
library(fda)
library(tidyverse)

data(simData1)
#simData1

plot(simData1)

bb = create.bspline.basis(rangeval = c(0,0.2), norder=4)
#eval.basis(seq(by =0.01,from=0,to=0.2), simData1$basis)


#create the intervals
numInts = 8
ints = rexp(numInts,300)
ints = ints/(sum(ints))
ints

ints2 = cumsum(ints)
ints2 = c(0, ints2)

plot(simData1)
discrete_data = NULL

# fUnction that creates exponentially distributed partition of unerlying T space
randomInterval <- function(numInts, theta){
  ints = rexp(numInts,theta)
  ints = ints/(sum(ints))
  ints2 = cumsum(ints)
  ints2 = c(0, ints2)
  
  return(ints2)
}

randomInterval(8,1)

#Gets the AverageValue
scalarExtract <- function(intervals, coeffs, basis ){
  scalar_data = NULL
  for (i in 1:(length(intervals)-1)) {
    xvals = seq(by =0.01,from=intervals[i],to=intervals[i+1])
    yvals = t(coeffs)%*%t(eval.basis(xvals, basis))
    scalar_data = cbind(scalar_data,rowMeans(yvals) )
  }
  return(scalar_data)
}


#gets scalar extraction for M trees, each extraction is different interval
mScalarExtraction <- function(m, numInts, theta=1, coeffs, basis){
  m_scalar_data = array(dim = c(dim(coeffs)[2],numInts,m))
  
  for (j in 1:m) {
    m_scalar_data[,,j]= scalarExtract(randomInterval(numInts,theta),
                                      coeffs, basis)
    
  }
  return(m_scalar_data)
}

s_data_test = mScalarExtraction(4, 6, 1, simData1$coefs, simData1$basis)


s_data_test[1,,1]
s_data_test[1,,2]
s_data_test[1,,3]


s_data_test[1,,1:4]

plot(simData1, ylab = "X(t)", xlab = "t")


for (i in 1:numInts) {
  xvals = seq(by =0.01,from=ints2[i],to=ints2[i+1])
  yvals = t(simData1$coefs)%*%t(eval.basis(xvals, simData1$basis))
  
  points(x= xvals, y=yvals[190,], col="darkgreen", pch=16)
  meany = rep(mean(yvals[190,]), length(xvals))
  discrete_data = cbind(discrete_data,rowMeans(yvals) )
  
  lines(x= xvals, y = meany, col="green", lwd=3 )
}



# See return matrix of 10 different average partitions

X = s_data_test

library(BART)

m = dim(X)[3]

newX = array(dim = c(dim(X)))
for (i in 1:m) {
  
  newX[,,i] = bartModelMatrix(X[,,i])
  
}

newX

########################################################
########################################################





