% - http://www.uni-kiel.de/psychologie/rexrepos/posts/regressionOrdinal.html
## Install required packages
## MASS, ordinal, rms, VGAM

wants <- c("MASS", "ordinal", "rms", "VGAM")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

#########################################################

## Ordinal regression (proportional odds model)

## Simulate data

## Dependent variable Yord with k=4 groups, p=2 predictor variables

set.seed(123)
N     <- 100
X1    <- rnorm(N, 175, 7)
X2    <- rnorm(N,  30, 8)
Ycont <- 0.5*X1 - 0.3*X2 + 10 + rnorm(N, 0, 6)
Yord  <- cut(Ycont, breaks=quantile(Ycont), include.lowest=TRUE,
             labels=c("--", "-", "+", "++"), ordered=TRUE)
dfOrd <- data.frame(X1, X2, Yord)

## Using vglm() from package VGAM

library(VGAM)
(vglmFit <- vglm(Yord ~ X1 + X2, family=propodds, data=dfOrd))
# Call:
# vglm(formula = Yord ~ X1 + X2, family = propodds, data = dfOrd)
# Coefficients:
# (Intercept):1 (Intercept):2 (Intercept):3            X1            X2 
#     -15.61123     -17.00112     -18.28507       0.11197      -0.09518 
# 
# Degrees of Freedom: 300 Total; 295 Residual
# Residual deviance: 249.4 
# Log-likelihood: -124.7 

################################

vglm(Yord ~ X1 + X2, family=cumulative(parallel=TRUE, reverse=TRUE), data=dfOrd)

# not shown
# Adjacent category logits lnP(Y=g)P(Y=g−1) with proportional odds assumption

vglm(Yord ~ X1 + X2, family=acat(parallel=TRUE), data=dfOrd)
# not shown
Continuation ratio logits lnP(Y=g)P(Y<g) with proportional odds assumption (discrete version of Cox proportional hazards model for survival data)

vglm(Yord ~ X1 + X2, family=sratio(parallel=TRUE), data=dfOrd)
# not shown

Using lrm() from package rms

#################################
library(rms)
(lrmFit <- lrm(Yord ~ X1 + X2, data=dfOrd))

Logistic Regression Model

lrm(formula = Yord ~ X1 + X2, data = dfOrd)

Frequencies of Responses

--  -  + ++ 
25 25 25 25 

                      Model Likelihood     Discrimination    Rank Discrim.    
                         Ratio Test            Indexes          Indexes       
Obs           100    LR chi2      27.90    R2       0.260    C       0.708    
max |deriv| 5e-08    d.f.             2    g        1.176    Dxy     0.416    
                     Pr(> chi2) <0.0001    gr       3.240    gamma   0.417    
                                           gp       0.244    tau-a   0.315    
                                           Brier    0.202                     

      Coef     S.E.   Wald Z Pr(>|Z|)
y>=-  -15.6111 5.5109 -2.83  0.0046  
y>=+  -17.0010 5.5508 -3.06  0.0022  
y>=++ -18.2849 5.5863 -3.27  0.0011  
X1      0.1120 0.0314  3.56  0.0004  
X2     -0.0952 0.0272 -3.50  0.0005  
##################################################################
Using polr() from package MASS

library(MASS)
(polrFit <- polr(Yord ~ X1 + X2, method="logistic", data=dfOrd))
# not shown
Profile likelihood based confidence intervals

exp(confint(polrFit))
Error: Objekt 'dfOrd' nicht gefunden

Using clm() from package ordinal

library(ordinal)
(clmFit <- clm(Yord ~ X1 + X2, link="logit", data=dfOrd))
# not shown
Predicted category membership
Predicted category probabilities
########################################################################

PhatCateg <- predict(vglmFit, type="response")
head(PhatCateg)
       --      -      +     ++
1 0.22610 0.3137 0.2692 0.1910
2 0.32021 0.3339 0.2182 0.1277
3 0.07321 0.1676 0.2930 0.4662
4 0.19020 0.2951 0.2877 0.2270
5 0.12404 0.2384 0.3100 0.3276
6 0.07534 0.1711 0.2950 0.4585
predict(lrmFit, type="fitted.ind")
predict(clmFit, subset(dfOrd, select=c("X1", "X2"), type="prob"))$fit
predict(polrFit, type="probs")
#####################################################
Predicted categories

categHat <- levels(dfOrd$Yord)[max.col(PhatCateg)]
head(categHat)
[1] "-"  "-"  "++" "-"  "++" "++"
predict(clmFit, type="class")
predict(polrFit, type="class")
# not shown
###################################################
Apply regression model to new data
Simulate new data

Nnew  <- 3
dfNew <- data.frame(X1=rnorm(Nnew, 175, 7),
                    X2=rnorm(Nnew,  30, 8))
Predicted class probabilities

predict(vglmFit, dfNew, type="response")
      --      -      +      ++
1 0.2514 0.3227 0.2554 0.17038
2 0.5306 0.2888 0.1231 0.05753
3 0.5565 0.2779 0.1135 0.05212
predict(lrmFit,  dfNew, type="fitted.ind")
predict(polrFit, dfNew, type="probs")
predict(clmFit,  subset(dfNew, select=c("X1", "X2"), type="prob"))$fit
# not shown
#########################################################################
Assess model fit
Classification table

facHat <- factor(categHat, levels=levels(dfOrd$Yord))
cTab   <- table(dfOrd$Yord, facHat, dnn=c("Yord", "facHat"))
addmargins(cTab)
     facHat
Yord   --   -   +  ++ Sum
  --   17   4   3   1  25
  -     5  11   2   7  25
  +     1  10   4  10  25
  ++    3   9   2  11  25
  Sum  26  34  11  29 100
Correct classification rate

(CCR <- sum(diag(cTab)) / sum(cTab))
[1] 0.43
Deviance, log-likelihood and AIC

deviance(vglmFit)
[1] 249.4
logLik(vglmFit)
[1] -124.7
AIC(vglmFit)
[1] 259.4
McFadden, Cox & Snell and Nagelkerke pseudo R2
\end{frame}
%================================================ %
\begin{frame}[fragile]
\frametitle{Negative Binomial Regression with \texttt{R} }
\Large
Log-likelihoods for full model and 0-model without predictors X1, X2

vglm0 <- vglm(Yord ~ 1, family=propodds, data=dfOrd)
LLf   <- logLik(vglmFit)
LL0   <- logLik(vglm0)
McFadden pseudo-R2
as.vector(1 - (LLf / LL0))
[1] 0.1006
#############################################################
as.vector(1 - exp((2/N) * (LL0 - LLf)))
[1] 0.2435
Nagelkerke

as.vector((1 - exp((2/N) * (LL0 - LLf))) / (1 - exp(LL0)^(2/N)))
[1] 0.2597
Coefficient tests and overall model test
#############################################################
Individual coefficient tests

Estimated standard deviations and z-values for parameters

sumOrd   <- summary(vglmFit)
(coefOrd <- coef(sumOrd))
               Estimate Std. Error z value
(Intercept):1 -15.61123    5.41913  -2.881
(Intercept):2 -17.00112    5.45614  -3.116
(Intercept):3 -18.28507    5.49804  -3.326
X1              0.11197    0.03122   3.586
X2             -0.09518    0.02694  -3.533
############################################################
Approximative Wald-based confidence intervals

zCrit   <- qnorm(c(0.05/2, 1 - 0.05/2))
(ciCoef <- t(apply(coefOrd, 1, function(x) x["Estimate"] - zCrit*x["Std. Error"] )))
                  [,1]      [,2]
(Intercept):1 -4.98994 -26.23252
(Intercept):2 -6.30730 -27.69495
(Intercept):3 -7.50911 -29.06102
X1             0.17317   0.05077
X2            -0.04238  -0.14798
p-values for two-sided paramter tests based on assumption that z-values are asymptotically N(0,1) distributed

2*(1 - pnorm(abs(coefOrd[ , "z value"])))
(Intercept):1 (Intercept):2 (Intercept):3            X1            X2 
    0.0039671     0.0018334     0.0008818     0.0003357     0.0004109 
summary(polrFit)
Error: Objekt 'dfOrd' nicht gefunden
summary(clmFit)
# not shown
Model comparisons - likelihood-ratio tests
######################################################################
# Likelihood-ratio-test for predictor X2
# 
We need to specify VGAM::lrtest() here because after attaching package mlogit above, there is another function present with the same name.

vglmR <- vglm(Yord ~ X1, family=propodds, data=dfOrd)
VGAM::lrtest(vglmFit, vglmR)
Likelihood ratio test

Model 1: Yord ~ X1 + X2
Model 2: Yord ~ X1
  #Df LogLik Df Chisq Pr(>Chisq)    
1 295   -125                        
2 296   -131  1  13.5    0.00024 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
Likelihood-ratio-test for the full model against the 0-model without predictors (just intercept)

VGAM::lrtest(vglmFit, vglm0)
Likelihood ratio test

Model 1: Yord ~ X1 + X2
Model 2: Yord ~ 1
  #Df LogLik Df Chisq Pr(>Chisq)    
1 295   -125                        
2 297   -139  2  27.9    8.7e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
Test assumption of proportional odds (=parallel logits)

#########################################################################
vglmP <- vglm(Yord ~ X1 + X2, family=cumulative(parallel=TRUE,  reverse=TRUE),
              data=dfOrd)

vglmNP <- vglm(Yord ~ X1 + X2, family=cumulative(parallel=FALSE, reverse=TRUE),
                data=dfOrd)

Error: NA/NaN/Inf in externem Funktionsaufruf (arg 1)
VGAM::lrtest(vglmP, vglmNP)
Error: Objekt 'vglmNP' nicht gefunden
Detach (automatically) loaded packages (if possible)
try(detach(package:ordinal))
try(detach(package:ucminf))
try(detach(package:Matrix))
try(detach(package:lattice))
try(detach(package:MASS))
try(detach(package:rms))
try(detach(package:Hmisc))
try(detach(package:survival))
try(detach(package:VGAM))
try(detach(package:splines))
try(detach(package:stats4))
