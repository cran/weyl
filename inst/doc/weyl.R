## ----out.width='20%', out.extra='style="float:right; padding:10px"',echo=FALSE----
knitr::include_graphics(system.file("help/figures/weyl.png", package = "weyl"))

## ----loadlib,echo=TRUE,print=FALSE,results="hide",message=FALSE---------------
library(weyl)

## ----firstexample-------------------------------------------------------------
7*d + 4*x*d^3*x

## ----firstexamplepolyform-----------------------------------------------------
options(polyform=TRUE)
7*d + 4*x*d^3*x

## ----defineandprintd1d2-------------------------------------------------------
(d1 <- d*x + 2*d^3)
(d2 <- 3 + 7*d  -5*x^2*d^2)

## ----showmultiplication-------------------------------------------------------
d1*d2

## ----showinpolyform-----------------------------------------------------------
options(polyform=TRUE)
(d1^2 + d2) * (d2 - 3*d1)

## -----------------------------------------------------------------------------
x <- weyl(cbind(0,1))
D <- weyl(cbind(1,0))
x^2*D*x*D^2

## ----showmultivariateweyl-----------------------------------------------------
options(polyform=FALSE)  # revert to default print method
set.seed(0)
x <- rweyl()
x

## ----associativityisnontrivial,cache=TRUE-------------------------------------
x <- rweyl(n=1,d=2)
y <- rweyl(n=2,d=2)
z <- rweyl(n=3,d=2)
options(polyform=TRUE)
x*(y*z)
(x*y)*z

## ----verifyassociativity------------------------------------------------------
x*(y*z) - (x*y)*z

## -----------------------------------------------------------------------------
(x9 <- rweyl(dim=9))

## -----------------------------------------------------------------------------
options(polyform=TRUE)
x9
options(polyform=FALSE) # revert to default

## ----define_derivation_D------------------------------------------------------
f <- rweyl()
D <- as.der(f)  # D(x) = xf-fx

## ----show_D_is_a_derivation,cache=TRUE----------------------------------------
d1 <- rweyl()
d2 <- rweyl()
D(d1*d2) == d1*D(d2) + D(d1)*d2

## -----------------------------------------------------------------------------
weyl_prod_helper3

## -----------------------------------------------------------------------------
`weyl_e_prod` <- function(a,b,c,d){
    if(c==0){return(spray(cbind(a,b+d)))}
    if(b==0){return(spray(cbind(a+c,d)))}
    return(
    Recall(a,b-1,c,d+1) +
    c*Recall(a,b-1,c,d)  # cf: c*Recall(a,b-1,c-1,d)) for regular Weyl algebra
    )  
}

## -----------------------------------------------------------------------------
options(prodfunc = weyl_e_prod) 
options(weylvars = "e")  # changes print method
d <- weyl(spray(cbind(0,1)))
e <- weyl(spray(cbind(1,0)))
d*d*e
d^2*e

## -----------------------------------------------------------------------------
d^5*e == e*(1+d)^5

## -----------------------------------------------------------------------------
options(polyform = TRUE)
d*e*d^2*e

## -----------------------------------------------------------------------------
o1 <- weyl(spray(cbind(2,1)))
o2 <- weyl(spray(cbind(3,3)))
options(polyform = FALSE)
(1+o1)*(1-5*o2)

## -----------------------------------------------------------------------------
options(polyform = TRUE)
(1+o1)*(1-5*o2)

## ----echo=FALSE---------------------------------------------------------------
options(polyform = NULL) # restore default print method

## ----makearandomweyl----------------------------------------------------------
options(weylvars = NULL)  # revert to default names
(W <- weyl(spray(matrix(c(0,1,1,1,1,2,1,0),2,4),2:3))^2)

## ----showcoeffsinuse----------------------------------------------------------
coeffs(W)

## ----showdisorderror,error=TRUE-----------------------------------------------
try(coeffs(W)[1] , silent=FALSE)

## ----ogreaterthantwo----------------------------------------------------------
o <- coeffs(W)
o[o>6]

## ----showextractionworking----------------------------------------------------
coeffs(W)[coeffs(W)<7] <- coeffs(W)[coeffs(W)<7] + 100
W

