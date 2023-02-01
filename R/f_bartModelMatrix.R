## BART: Bayesian Additive Regression Trees
## Copyright (C) 2017 Robert McCulloch and Rodney Sparapani

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, a copy is available at
## https://www.R-project.org/Licenses/GPL-2





# X is a 3 dimensional array. n(observations) x p(predictor variables) x m(trees)


f_bartModelMatrix=function(X, numcut=0L, usequants=FALSE, type=7,
                         rm.const=FALSE, cont=FALSE, xinfo=NULL) {


  n = dim(X)[1]
  p = dim(X)[2]
  m = dim(X)[3]

  newX = array(dim = c(n*m, p ))


  for (i in 1:m) {


    print(i)
    temp = bartModelMatrix(X[,,i], numcut, usequants, type,
                           rm.const, cont, xinfo)
    #print(dim(newX))
    #print(dim(temp$X))
    #print(dim(newX[((m-1)*n+1):(m*n),]))
    newX[((m-1)*n+1):(m*n),] = temp$X



    #newX[,,i] = temp
  }

  temp$X = newX


  return(temp)
}
