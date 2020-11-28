Sentiment analysis
================

``` r
library(plyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(text2vec)
```

    ## Warning: package 'text2vec' was built under R version 4.0.3

``` r
library(tidytext)
```

    ## Warning: package 'tidytext' was built under R version 4.0.3

``` r
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
data_train <- read.csv("movie_train.csv")
data_train <- data_train[c(1:250),]
str(data_train)
```

    ## 'data.frame':    250 obs. of  2 variables:
    ##  $ text     : chr  "Now, I won't deny that when I purchased this off eBay, I had high expectations. This was an incredible out-of-p"| __truncated__ "The saddest thing about this \"tribute\" is that almost all the singers (including the otherwise incredibly tal"| __truncated__ "Last night I decided to watch the prequel or shall I say the so called prequel to Carlito's Way - \"Carlito's W"| __truncated__ "I have to admit that i liked the first half of Sleepers. It looked good, the acting was even better, the story "| __truncated__ ...
    ##  $ sentiment: chr  "neg" "neg" "neg" "neg" ...

``` r
tokens <- space_tokenizer(data_train$text)
token_iterator <- itoken(tokens)

vocab <- create_vocabulary(token_iterator)
vocab <- prune_vocabulary(vocab, term_count_min = 5)
vectorizer <- vocab_vectorizer(vocab)
```

``` r
tcm <- create_tcm(token_iterator, vectorizer, skip_grams_window = 5)

library(rsparse)
```

    ## Warning: package 'rsparse' was built under R version 4.0.3

``` r
glove <- GloVe$new(rank = 50, 
                   x_max = 10)
```

``` r
wv_main <- glove$fit_transform(tcm, n_iter = 3, 
                               convergence_tol = 0.01)
```

    ## INFO  [11:30:08.317] epoch 1, loss 0.1774 
    ## INFO  [11:30:08.911] epoch 2, loss 0.0937 
    ## INFO  [11:30:09.409] epoch 3, loss 0.0756

``` r
text <- unlist(data_train$text)
length(text)
```

    ## [1] 250

``` r
text_df <- data.frame(line = 1:250, text = text)

text_df <- text_df %>% unnest_tokens(word, text)
head(text_df)
```

    ##     line  word
    ## 1      1   now
    ## 1.1    1     i
    ## 1.2    1 won't
    ## 1.3    1  deny
    ## 1.4    1  that
    ## 1.5    1  when

``` r
head(wv_main[,1:3])
```

    ##              [,1]         [,2]        [,3]
    ## "I     0.53014323  0.302807724  0.07816242
    ## 'The   0.08464217  0.266331022  0.25086277
    ## ,     -0.21850687 -0.016265694  0.10616240
    ## /     -0.38718534  0.089896621 -0.22896526
    ## />And  0.36407369 -0.220981870 -0.40604721
    ## />My   0.44949339 -0.005960978 -0.36507794

``` r
wv_context <- glove$components
wv <- wv_main + t(wv_context)
wv <- as.data.frame(wv_main)
wv$word <- row.names(wv)
df <-  wv%>% inner_join(text_df)
```

    ## Joining, by = "word"

``` r
df <- df %>% 
        group_by(line) %>% 
        summarize_all(mean) %>% 
        select(1:50) 
```

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

    ## Warning in mean.default(word): argument is not numeric or logical: returning NA

``` r
df$label <- as.factor(data_train$sentiment)
```

``` r
control <- trainControl(method="cv", repeats=3)
```

    ## Warning: `repeats` has no meaning for this resampling method.

``` r
set.seed(7)
 modelRF <- train(
                    label~., 
                    data=df, 
                    method="rf", 
                    trControl=control
                   )

   modelNaiveBayes <- train(
                            label~., 
                            data=df, 
                            method="nb", 
                            trControl=control
                            )
```

``` r
results <- resamples(
  list(
    RF=modelRF,
    NB=modelNaiveBayes
    )
  )

summary(results)
```

    ## 
    ## Call:
    ## summary.resamples(object = results)
    ## 
    ## Models: RF, NB 
    ## Number of resamples: 10 
    ## 
    ## Accuracy 
    ##    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## RF 0.48 0.6000000 0.6201923 0.6122821 0.6562500 0.6923077    0
    ## NB 0.50 0.5738462 0.6400000 0.6559487 0.7180769 0.8400000    0
    ## 
    ## Kappa 
    ##           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## RF -0.06557377 0.1242715 0.1830022 0.1779565 0.2955734 0.3540373    0
    ## NB -0.09090909 0.1083373 0.2637545 0.2884661 0.4223984 0.6666667    0
