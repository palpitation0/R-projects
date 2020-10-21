Failure time distribution
================

``` r
library(SMPracticals)
```

    ## Warning: package 'SMPracticals' was built under R version 4.0.3

    ## Loading required package: ellipse

    ## 
    ## Attaching package: 'ellipse'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     pairs

``` r
data(motorette)
df <- motorette[(11:40),] # 150'C 데이터는 분석에서 제외
head(df,30)
```

    ##      x cens    y
    ## 11 170    1 1764
    ## 12 170    1 2772
    ## 13 170    1 3444
    ## 14 170    1 3542
    ## 15 170    1 3780
    ## 16 170    1 4860
    ## 17 170    1 5196
    ## 18 170    0 5448
    ## 19 170    0 5448
    ## 20 170    0 5448
    ## 21 190    1  408
    ## 22 190    1  408
    ## 23 190    1 1344
    ## 24 190    1 1344
    ## 25 190    1 1440
    ## 26 190    0 1680
    ## 27 190    0 1680
    ## 28 190    0 1680
    ## 29 190    0 1680
    ## 30 190    0 1680
    ## 31 220    1  408
    ## 32 220    1  408
    ## 33 220    1  504
    ## 34 220    1  504
    ## 35 220    1  504
    ## 36 220    0  528
    ## 37 220    0  528
    ## 38 220    0  528
    ## 39 220    0  528
    ## 40 220    0  528

``` r
df$z <- 1000/(273+df$x) # 온도 단위 변환
```

``` r
library(survival)
```

    ## 
    ## Attaching package: 'survival'

    ## The following objects are masked from 'package:SMPracticals':
    ## 
    ##     aml, pbc

``` r
fit <- survreg(Surv(y,cens)~z, data=df, dist="weibull")
summary(fit)
```

    ## 
    ## Call:
    ## survreg(formula = Surv(y, cens) ~ z, data = df, dist = "weibull")
    ##               Value Std. Error     z       p
    ## (Intercept) -11.883      1.965 -6.05 1.5e-09
    ## z             9.031      0.905  9.98 < 2e-16
    ## Log(scale)   -1.018      0.220 -4.63 3.7e-06
    ## 
    ## Scale= 0.361 
    ## 
    ## Weibull distribution
    ## Loglik(model)= -144.3   Loglik(intercept only)= -155.7
    ##  Chisq= 22.67 on 1 degrees of freedom, p= 1.9e-06 
    ## Number of Newton-Raphson Iterations: 7 
    ## n= 30

``` r
predict(fit, newdata=data.frame(z=1000/(273+130)),
        type="uquantile", p=c(0.15,0.5,0.85))
```

    ## [1]  9.869061 10.393077 10.756830
