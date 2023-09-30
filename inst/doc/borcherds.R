## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library("weyl")
set.seed(0)

## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/weyl.png", package = "weyl"))

## ----defineDEF----------------------------------------------------------------
D <- weyl(spray(cbind(matrix(sample(8),4,2),kronecker(diag(2),c(1,1))),1:4))
E <- weyl(spray(cbind(matrix(sample(8),4,2),kronecker(diag(2),c(1,1))),1:4))
F <- weyl(spray(cbind(matrix(sample(8),4,2),kronecker(diag(2),c(1,1))),1:4))
D

## ----sumED--------------------------------------------------------------------
summary(E*D)

## ----giveliebracketofEandD----------------------------------------------------
.[E,D]

## ----showotherprintmethod-----------------------------------------------------
options(polyform = TRUE)
.[E,D]
options(polyform = FALSE) # revert to default

## ----verifyjacobi,cache=TRUE--------------------------------------------------
.[D,.[E,F]] + .[F,.[D,E]] + .[E,.[F,D]]

## ----makeconstantops,cache=TRUE-----------------------------------------------
(D <- as.weyl(spray(cbind(matrix(0,3,3),matrix(c(0,1,0,1,0,0,0,0,1),3,3,byrow=T)),1:3)))
(E <- as.weyl(spray(cbind(matrix(0,3,3),matrix(c(0,1,0,1,0,0,0,0,1),3,3,byrow=T)),5:7)))

## ----label=abelianshow--------------------------------------------------------
.[D,E]

