Parametric Survival model +/- Frailty
================

``` r
leuk <- read.table("C:/Users/Choi Sung Wook/Documents/R-projects/anderson.txt", header=TRUE)
head(leuk)
```

    ##   X35 X0 X1 X1.45 X0.1
    ## 1  34  0  1  1.47    0
    ## 2  32  0  1  2.20    0
    ## 3  32  0  1  2.53    0
    ## 4  25  0  1  1.78    0
    ## 5  23  1  1  2.57    0
    ## 6  22  1  1  2.32    0

``` r
names(leuk) <- c(X35="t", X0="d", X1="sex",X1.45="logWBC",X0.1="Rx")
str(leuk)
```

    ## 'data.frame':    41 obs. of  5 variables:
    ##  $ t     : int  34 32 32 25 23 22 20 19 17 16 ...
    ##  $ d     : int  0 0 0 0 1 1 0 0 0 1 ...
    ##  $ sex   : int  1 1 1 1 1 1 1 0 0 1 ...
    ##  $ logWBC: num  1.47 2.2 2.53 1.78 2.57 2.32 2.01 2.05 2.16 3.6 ...
    ##  $ Rx    : int  0 0 0 0 0 0 0 0 0 0 ...

``` r
library(survival)
fit <- survreg(Surv(t,d)~Rx, dist="exponential", data=leuk)
summary(fit)
```

    ## 
    ## Call:
    ## survreg(formula = Surv(t, d) ~ Rx, data = leuk, dist = "exponential")
    ##              Value Std. Error     z       p
    ## (Intercept)  3.584      0.333 10.75 < 2e-16
    ## Rx          -1.424      0.398 -3.57 0.00035
    ## 
    ## Scale fixed at 1 
    ## 
    ## Exponential distribution
    ## Loglik(model)= -107.6   Loglik(intercept only)= -114.8
    ##  Chisq= 14.32 on 1 degrees of freedom, p= 0.00015 
    ## Number of Newton-Raphson Iterations: 4 
    ## n= 41

``` r
# S(t)= exp(-lambda*t), lambda=exp[-3.584+1.424*Rx]
```

``` r
fit.weibull <- survreg(Surv(t,d)~Rx, dist="weibull", data=leuk)
summary(fit.weibull)
```

    ## 
    ## Call:
    ## survreg(formula = Surv(t, d) ~ Rx, data = leuk, dist = "weibull")
    ##              Value Std. Error     z       p
    ## (Intercept)  3.418      0.246 13.87 < 2e-16
    ## Rx          -1.165      0.304 -3.83 0.00013
    ## Log(scale)  -0.328      0.147 -2.23 0.02547
    ## 
    ## Scale= 0.72 
    ## 
    ## Weibull distribution
    ## Loglik(model)= -105.5   Loglik(intercept only)= -114.1
    ##  Chisq= 17.37 on 1 degrees of freedom, p= 3.1e-05 
    ## Number of Newton-Raphson Iterations: 6 
    ## n= 41

``` r
# S(t) = exp(-lambda*t), lambda=exp(-3.418+1.267*Rx)
# shape parameter p = 1/0.72  --> 1보다 크므로 시간 지날수록 hazard function 값 증가
```

``` r
data(veteran)
head(veteran)
```

    ##   trt celltype time status karno diagtime age prior
    ## 1   1 squamous   72      1    60        7  69     0
    ## 2   1 squamous  411      1    70        5  64    10
    ## 3   1 squamous  228      1    60        3  38     0
    ## 4   1 squamous  126      1    60        9  63    10
    ## 5   1 squamous  118      1    70       11  65    10
    ## 6   1 squamous   10      1    20        5  49     0

``` r
library(eha)
```

    ## Warning: package 'eha' was built under R version 4.0.3

``` r
no.frailty <- phreg(Surv(time,status)~trt+karno+diagtime+age+prior,
                    dist="weibull", data=veteran)
summary(no.frailty)
```

    ## Call:
    ## phreg(formula = Surv(time, status) ~ trt + karno + diagtime + 
    ##     age + prior, data = veteran, dist = "weibull")
    ## 
    ## Covariate          W.mean      Coef Exp(Coef)  se(Coef)    Wald p
    ## trt                 1.523     0.137     1.147     0.181     0.450 
    ## karno              68.419    -0.034     0.966     0.005     0.000 
    ## diagtime            8.139     0.003     1.003     0.009     0.746 
    ## age                57.379    -0.001     0.999     0.009     0.927 
    ## prior               3.471    -0.013     0.988     0.022     0.566 
    ## 
    ## log(scale)                    2.808               0.712     0.000 
    ## log(shape)                   -0.018               0.065     0.786 
    ## 
    ## Events                    128 
    ## Total time at risk         16663 
    ## Max. log. likelihood      -725.62 
    ## LR test statistic         44.95 
    ## Degrees of freedom        5 
    ## Overall p-value           1.48617e-08

``` r
#log(shape)=-0.018  --> shape=exp(-0.018)=0.982
```

``` r
# Frailty model
library(parfm)
```

    ## Warning: package 'parfm' was built under R version 4.0.3

    ## Loading required package: optimx

    ## Warning: package 'optimx' was built under R version 4.0.3

``` r
veteran$id=seq(1,nrow(veteran))
frailty <- parfm(Surv(time,status)~trt+karno+diagtime+age+prior,
                 cluster="id", frailty="gamma", dist="weibull",
                 data=veteran)
frailty
```

    ## 
    ## Frailty distribution: gamma 
    ## Baseline hazard distribution: Weibull 
    ## Loglikelihood: -719.526 
    ## 
    ##          ESTIMATE SE    p-val    
    ## theta     0.861   0.328          
    ## rho       1.545   0.216          
    ## lambda    0.105   0.114          
    ## trt       0.105   0.291 0.719    
    ## karno    -0.061   0.012 <.001 ***
    ## diagtime -0.006   0.015 0.663    
    ## age      -0.013   0.015 0.383    
    ## prior    -0.006   0.035 0.859    
    ## ---
    ## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
data(kidney)
head(kidney)
```

    ##   id time status age sex disease frail
    ## 1  1    8      1  28   1   Other   2.3
    ## 2  1   16      1  28   1   Other   2.3
    ## 3  2   23      1  48   2      GN   1.9
    ## 4  2   13      0  48   2      GN   1.9
    ## 5  3   22      1  32   1   Other   1.2
    ## 6  3   28      1  32   1   Other   1.2

``` r
kfit1 <- coxph(Surv(time,status)~age+sex, data=kidney)
kfit1
```

    ## Call:
    ## coxph(formula = Surv(time, status) ~ age + sex, data = kidney)
    ## 
    ##          coef exp(coef)  se(coef)      z       p
    ## age  0.002032  1.002034  0.009246  0.220 0.82607
    ## sex -0.829314  0.436349  0.298955 -2.774 0.00554
    ## 
    ## Likelihood ratio test=7.12  on 2 df, p=0.02849
    ## n= 76, number of events= 58

``` r
# multiple observation per person 이므로 frailty model 적용
kfit2 <- coxph(Surv(time,status)~age+sex+frailty(id), data=kidney)
```

    ## Warning in coxpenal.fit(X, Y, istrat, offset, init = init, control, weights =
    ## weights, : Inner loop failed to coverge for iterations 3

``` r
kfit2
```

    ## Call:
    ## coxph(formula = Surv(time, status) ~ age + sex + frailty(id), 
    ##     data = kidney)
    ## 
    ##                 coef se(coef)      se2    Chisq DF       p
    ## age          0.00525  0.01189  0.00880  0.19515  1 0.65867
    ## sex         -1.58749  0.46055  0.35200 11.88114  1 0.00057
    ## frailty(id)                            23.12952 13 0.04036
    ## 
    ## Iterations: 7 outer, 65 Newton-Raphson
    ##      Variance of random effect= 0.412   I-likelihood = -181.6 
    ## Degrees of freedom for terms=  0.5  0.6 13.0 
    ## Likelihood ratio test=46.8  on 14.1 df, p=2e-05
    ## n= 76, number of events= 58
