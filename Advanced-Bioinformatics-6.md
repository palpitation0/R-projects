Advanced Bioinformatics 6 - Network Analysis
================

``` r
library(glasso)
```

``` r
set.seed(101010)
```

``` r
# 데이터 simulation
x <- matrix(rnorm(100*500), 100, 500)
#100개는 샘플 수, 500개는 유전체 수
```

``` r
#covariance matrix
s <- cov(x)
```

``` r
g <- glassopath(s)
```

    ## rho=
    ## [1] 1.411213
    ## rho=
    ## [1] 1.270091
    ## rho=
    ## [1] 1.12897
    ## rho=
    ## [1] 0.9878488
    ## rho=
    ## [1] 0.8467275
    ## rho=
    ## [1] 0.7056063
    ## rho=
    ## [1] 0.564485
    ## rho=
    ## [1] 0.4233638
    ## rho=
    ## [1] 0.2822425
    ## rho=
    ## [1] 0.1411213

``` r
g$rholist # 람다값
```

    ##  [1] 0.1411213 0.2822425 0.4233638 0.5644850 0.7056063 0.8467275 0.9878488
    ##  [8] 1.1289700 1.2700913 1.4112126

``` r
gt <- glasso(s,rho=g$rholist[2])
```

``` r
# concentration matrix : gt$wi
# 500*500 으로 너무 크니까 0 아닌 것만 보자
sum(gt$wi!=0)
```

    ## [1] 1970

``` r
# 실제 연결의 개수는, 대각성분 1이라 빼주고 2로 나눈것
(1970-500)/2
```

    ## [1] 735

``` r
# g$rholist[3] 써서 돌려보면?
gt <- glasso(s,rho=g$rholist[3])
sum(gt$wi !=0)
```

    ## [1] 510

``` r
(510-500)/2
```

    ## [1] 5

이제, 사이즈 줄여서 visualization 해보자.

``` r
set.seed(101010)
x <- matrix(rnorm(50*50), 50, 50)
s <- cov(x)
gt <- glasso(s, 0.3)
sum(gt$wi !=0)
```

    ## [1] 166

``` r
(166-50)/2
```

    ## [1] 58

``` r
library(igraph)
```

    ## Warning: package 'igraph' was built under R version 3.6.3

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
# 일단 concentration matrix 를 adjacency matrix 로 저장
adjm <- gt$wi
```

``` r
adjm[adjm!=0] <- 1 # 0 아닌 성분은 1로 지정
diag(adjm) <- 0 # 대각성분은 0 으로 지저
```

``` r
grl <- graph.adjacency(adjm, "undirected")
cl <- layout.graphopt(grl) # 어떤 모양으로 만들거냐
```

``` r
tkplot(grl, layout=cl, vertex.size=7, edge.width=1.5,
       edge.color="dark grey")
```

    ## [1] 1
