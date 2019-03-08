Homework 4: Bags, Forests, Boosts, oh my
================
Brooke Seibert
2/28/2019

Problem 1
---------

Problem 7 from Chapter 8 in the text. To be specific, please use a sequence of `ntree` from 25 to 500 in steps of 25 and `mtry` from 3 to 9 for by 1.

In the lab, we applied random forests to the Boston data using mtry=6 and using ntree=25 and ntree=500. Create a plot displaying the test error resulting from random forests on this data set for a more comprehensive range of values for mtry and ntree. You can model your plot after Figure 8.10. Describe the results obtained.

Answer 1
--------

*There is a negative relationship between the testing MSE and the number of trees. As trees is increased, the testing MSE decreases. Also, having just one tree results in a much higher testing MSE. The testing MSE for all predictors is the highest, while the test MSE for half of the predictors has the lowest testing MSE*

``` r
set.seed(1)
df <- tbl_df(Boston)
inTraining <- createDataPartition(df$medv, p = .75, list = F)
training <- df[inTraining, ]
testing <- df[-inTraining, ]
set.seed(10832)
#...couldn't see the rest of the code that we went over in class and couldn't figure it out with troubleshooting, but you mentioned it will not be graded. Hoping to get the rest of the code next class.
```

Problem 2
---------

Problem 8 from Chapter 8 in the text. Set your seed with 9823 and split into train/test using 50% of your data in each split. In addition to parts (a) - (e), do the following:

1.  Fit a gradient-boosted tree to the training data and report the estimated test MSE.
2.  Fit a multiple regression model to the training data and report the estimated test MSE
3.  Summarize your results.

Problem 8: In the lab, a classiﬁcation tree was applied to the Carseats data set after converting Sales into a qualitative response variable. Now we will seek to predict Sales using regression trees and related approaches, treating the response as a quantitative variable.

1.  Split the data set into a training set and a test set.

``` r
set.seed(9823)
df <- tbl_df(Carseats)
inTraining <- createDataPartition(df$Sales, p = .50, list = F)
training_c <- df[inTraining, ]
testing_c  <- df[-inTraining, ]
```

1.  Fit a regression tree to the training set. Plot the tree, and interpret the results. What test MSE do you obtain? *Testing MSE is 4.48.*

``` r
tree_c <- rpart::rpart(Sales ~ ., data = training_c, control = rpart.control(minsplit = 20))
summary(tree_c)
```

    ## Call:
    ## rpart::rpart(formula = Sales ~ ., data = training_c, control = rpart.control(minsplit = 20))
    ##   n= 201 
    ## 
    ##            CP nsplit rel error    xerror       xstd
    ## 1  0.23155949      0 1.0000000 1.0173468 0.09708974
    ## 2  0.12226232      1 0.7684405 0.8555409 0.07828876
    ## 3  0.09172973      2 0.6461782 0.7218100 0.06677113
    ## 4  0.03533612      3 0.5544484 0.6239798 0.05599835
    ## 5  0.03433475      4 0.5191123 0.7008466 0.06537458
    ## 6  0.03003688      5 0.4847776 0.7115057 0.06797243
    ## 7  0.02823833      6 0.4547407 0.6966850 0.06851981
    ## 8  0.02671147      7 0.4265024 0.7006259 0.06910021
    ## 9  0.02151143      8 0.3997909 0.6807879 0.06912370
    ## 10 0.01799895      9 0.3782795 0.7180768 0.07159743
    ## 11 0.01200941     11 0.3422816 0.7059585 0.06945401
    ## 12 0.01033541     12 0.3302722 0.7122929 0.07028484
    ## 13 0.01000000     13 0.3199367 0.7076406 0.07023507
    ## 
    ## Variable importance
    ##   ShelveLoc       Price   CompPrice         Age  Population   Education 
    ##          41          23          11           7           5           5 
    ## Advertising      Income 
    ##           4           4 
    ## 
    ## Node number 1: 201 observations,    complexity param=0.2315595
    ##   mean=7.465721, MSE=7.428204 
    ##   left son=2 (162 obs) right son=3 (39 obs)
    ##   Primary splits:
    ##       ShelveLoc   splits as  LRL,       improve=0.23155950, (0 missing)
    ##       Price       < 129.5 to the right, improve=0.13414320, (0 missing)
    ##       Advertising < 13.5  to the left,  improve=0.09155010, (0 missing)
    ##       Age         < 61.5  to the right, improve=0.05517630, (0 missing)
    ##       US          splits as  LR,        improve=0.03545178, (0 missing)
    ## 
    ## Node number 2: 162 observations,    complexity param=0.1222623
    ##   mean=6.822222, MSE=5.749911 
    ##   left son=4 (49 obs) right son=5 (113 obs)
    ##   Primary splits:
    ##       ShelveLoc   splits as  L-R,       improve=0.19597310, (0 missing)
    ##       Price       < 105.5 to the right, improve=0.18264610, (0 missing)
    ##       Advertising < 11.5  to the left,  improve=0.07161144, (0 missing)
    ##       Age         < 63.5  to the right, improve=0.05718564, (0 missing)
    ##       Income      < 116.5 to the left,  improve=0.04340843, (0 missing)
    ##   Surrogate splits:
    ##       CompPrice  < 93.5  to the left,  agree=0.704, adj=0.02, (0 split)
    ##       Population < 14.5  to the left,  agree=0.704, adj=0.02, (0 split)
    ## 
    ## Node number 3: 39 observations,    complexity param=0.03533612
    ##   mean=10.13872, MSE=5.534591 
    ##   left son=6 (13 obs) right son=7 (26 obs)
    ##   Primary splits:
    ##       Price       < 127.5 to the right, improve=0.2444267, (0 missing)
    ##       US          splits as  LR,        improve=0.1751606, (0 missing)
    ##       Advertising < 0.5   to the left,  improve=0.1675706, (0 missing)
    ##       Education   < 11.5  to the right, improve=0.1537685, (0 missing)
    ##       Population  < 356   to the left,  improve=0.1463307, (0 missing)
    ##   Surrogate splits:
    ##       Income    < 30.5  to the left,  agree=0.744, adj=0.231, (0 split)
    ##       CompPrice < 149   to the right, agree=0.718, adj=0.154, (0 split)
    ##       Age       < 34    to the left,  agree=0.718, adj=0.154, (0 split)
    ## 
    ## Node number 4: 49 observations,    complexity param=0.03003688
    ##   mean=5.210204, MSE=4.903508 
    ##   left son=8 (35 obs) right son=9 (14 obs)
    ##   Primary splits:
    ##       Price       < 102.5 to the right, improve=0.18665160, (0 missing)
    ##       Income      < 95.5  to the left,  improve=0.18458130, (0 missing)
    ##       Education   < 11.5  to the left,  improve=0.13839380, (0 missing)
    ##       Population  < 185.5 to the left,  improve=0.12645500, (0 missing)
    ##       Advertising < 11.5  to the left,  improve=0.07818734, (0 missing)
    ##   Surrogate splits:
    ##       CompPrice  < 112.5 to the right, agree=0.816, adj=0.357, (0 split)
    ##       Age        < 75.5  to the left,  agree=0.776, adj=0.214, (0 split)
    ##       Income     < 27    to the right, agree=0.755, adj=0.143, (0 split)
    ##       Population < 496.5 to the left,  agree=0.735, adj=0.071, (0 split)
    ## 
    ## Node number 5: 113 observations,    complexity param=0.09172973
    ##   mean=7.521239, MSE=4.501483 
    ##   left son=10 (73 obs) right son=11 (40 obs)
    ##   Primary splits:
    ##       Price       < 105.5 to the right, improve=0.26925010, (0 missing)
    ##       Income      < 57.5  to the left,  improve=0.11786330, (0 missing)
    ##       Age         < 60.5  to the right, improve=0.11445890, (0 missing)
    ##       Advertising < 6.5   to the left,  improve=0.09124711, (0 missing)
    ##       CompPrice   < 115.5 to the left,  improve=0.04636677, (0 missing)
    ##   Surrogate splits:
    ##       CompPrice  < 120.5 to the right, agree=0.699, adj=0.15, (0 split)
    ##       Population < 80    to the right, agree=0.681, adj=0.10, (0 split)
    ##       Income     < 118.5 to the left,  agree=0.664, adj=0.05, (0 split)
    ## 
    ## Node number 6: 13 observations
    ##   mean=8.493846, MSE=3.219254 
    ## 
    ## Node number 7: 26 observations,    complexity param=0.02671147
    ##   mean=10.96115, MSE=4.663056 
    ##   left son=14 (17 obs) right son=15 (9 obs)
    ##   Primary splits:
    ##       Education   < 11.5  to the right, improve=0.3289528, (0 missing)
    ##       CompPrice   < 120.5 to the left,  improve=0.2320853, (0 missing)
    ##       Advertising < 0.5   to the left,  improve=0.1510274, (0 missing)
    ##       Price       < 108.5 to the right, improve=0.1324465, (0 missing)
    ##       Population  < 312.5 to the left,  improve=0.1093973, (0 missing)
    ##   Surrogate splits:
    ##       CompPrice  < 135   to the left,  agree=0.769, adj=0.333, (0 split)
    ##       Price      < 93.5  to the right, agree=0.769, adj=0.333, (0 split)
    ##       Population < 416   to the left,  agree=0.731, adj=0.222, (0 split)
    ##       Income     < 36.5  to the right, agree=0.692, adj=0.111, (0 split)
    ## 
    ## Node number 8: 35 observations,    complexity param=0.02151143
    ##   mean=4.605143, MSE=3.748665 
    ##   left son=16 (27 obs) right son=17 (8 obs)
    ##   Primary splits:
    ##       CompPrice < 137.5 to the left,  improve=0.2447961, (0 missing)
    ##       Price     < 143.5 to the right, improve=0.1786213, (0 missing)
    ##       Education < 11.5  to the left,  improve=0.1692974, (0 missing)
    ##       Age       < 44    to the right, improve=0.1435586, (0 missing)
    ##       Income    < 58.5  to the right, improve=0.1297241, (0 missing)
    ##   Surrogate splits:
    ##       Income    < 41    to the right, agree=0.8, adj=0.125, (0 split)
    ##       Education < 17.5  to the left,  agree=0.8, adj=0.125, (0 split)
    ## 
    ## Node number 9: 14 observations
    ##   mean=6.722857, MSE=4.587249 
    ## 
    ## Node number 10: 73 observations,    complexity param=0.02823833
    ##   mean=6.706301, MSE=3.290895 
    ##   left son=20 (59 obs) right son=21 (14 obs)
    ##   Primary splits:
    ##       Advertising < 13.5  to the left,  improve=0.17550200, (0 missing)
    ##       Income      < 59    to the left,  improve=0.13442920, (0 missing)
    ##       CompPrice   < 121.5 to the left,  improve=0.13058810, (0 missing)
    ##       Age         < 48.5  to the right, improve=0.12723440, (0 missing)
    ##       Price       < 136   to the right, improve=0.04860137, (0 missing)
    ## 
    ## Node number 11: 40 observations,    complexity param=0.03433475
    ##   mean=9.0085, MSE=3.286838 
    ##   left son=22 (27 obs) right son=23 (13 obs)
    ##   Primary splits:
    ##       Age         < 50.5  to the right, improve=0.3899200, (0 missing)
    ##       CompPrice   < 117.5 to the left,  improve=0.3136527, (0 missing)
    ##       Advertising < 5.5   to the left,  improve=0.1404600, (0 missing)
    ##       Income      < 55    to the left,  improve=0.1330976, (0 missing)
    ##       Price       < 96.5  to the right, improve=0.1321742, (0 missing)
    ##   Surrogate splits:
    ##       CompPrice   < 125.5 to the left,  agree=0.775, adj=0.308, (0 split)
    ##       Population  < 32    to the right, agree=0.725, adj=0.154, (0 split)
    ##       Advertising < 17.5  to the left,  agree=0.700, adj=0.077, (0 split)
    ##       Education   < 16.5  to the left,  agree=0.700, adj=0.077, (0 split)
    ## 
    ## Node number 14: 17 observations
    ##   mean=10.06, MSE=3.393306 
    ## 
    ## Node number 15: 9 observations
    ##   mean=12.66333, MSE=2.630133 
    ## 
    ## Node number 16: 27 observations,    complexity param=0.01033541
    ##   mean=4.083704, MSE=2.696394 
    ##   left son=32 (10 obs) right son=33 (17 obs)
    ##   Primary splits:
    ##       Population < 171.5 to the left,  improve=0.21196320, (0 missing)
    ##       Price      < 139.5 to the right, improve=0.18986180, (0 missing)
    ##       Education  < 11.5  to the left,  improve=0.18936010, (0 missing)
    ##       Income     < 64.5  to the right, improve=0.15477380, (0 missing)
    ##       Age        < 42.5  to the right, improve=0.09996678, (0 missing)
    ##   Surrogate splits:
    ##       Advertising < 1.5   to the left,  agree=0.778, adj=0.4, (0 split)
    ##       Price       < 149.5 to the right, agree=0.741, adj=0.3, (0 split)
    ##       Education   < 16.5  to the right, agree=0.741, adj=0.3, (0 split)
    ##       Age         < 68.5  to the right, agree=0.704, adj=0.2, (0 split)
    ## 
    ## Node number 17: 8 observations
    ##   mean=6.365, MSE=3.285325 
    ## 
    ## Node number 20: 59 observations,    complexity param=0.01799895
    ##   mean=6.336102, MSE=2.802651 
    ##   left son=40 (18 obs) right son=41 (41 obs)
    ##   Primary splits:
    ##       CompPrice   < 121.5 to the left,  improve=0.13130470, (0 missing)
    ##       Age         < 27.5  to the right, improve=0.11301050, (0 missing)
    ##       Price       < 131.5 to the right, improve=0.08968463, (0 missing)
    ##       Income      < 59    to the left,  improve=0.08736340, (0 missing)
    ##       Advertising < 6.5   to the left,  improve=0.06259425, (0 missing)
    ##   Surrogate splits:
    ##       Price      < 114.5 to the left,  agree=0.831, adj=0.444, (0 split)
    ##       Income     < 111.5 to the right, agree=0.712, adj=0.056, (0 split)
    ##       Population < 31.5  to the left,  agree=0.712, adj=0.056, (0 split)
    ##       Age        < 32    to the left,  agree=0.712, adj=0.056, (0 split)
    ## 
    ## Node number 21: 14 observations
    ##   mean=8.266429, MSE=2.336937 
    ## 
    ## Node number 22: 27 observations
    ##   mean=8.222963, MSE=2.030821 
    ## 
    ## Node number 23: 13 observations
    ##   mean=10.64, MSE=1.952092 
    ## 
    ## Node number 32: 10 observations
    ##   mean=3.098, MSE=2.886056 
    ## 
    ## Node number 33: 17 observations
    ##   mean=4.663529, MSE=1.677093 
    ## 
    ## Node number 40: 18 observations
    ##   mean=5.420556, MSE=0.8434497 
    ## 
    ## Node number 41: 41 observations,    complexity param=0.01799895
    ##   mean=6.738049, MSE=3.133225 
    ##   left son=82 (16 obs) right son=83 (25 obs)
    ##   Primary splits:
    ##       Price       < 131.5 to the right, improve=0.24937500, (0 missing)
    ##       Age         < 48.5  to the right, improve=0.14544350, (0 missing)
    ##       Income      < 59    to the left,  improve=0.12444810, (0 missing)
    ##       Advertising < 6.5   to the left,  improve=0.07810302, (0 missing)
    ##       CompPrice   < 131.5 to the right, improve=0.05041991, (0 missing)
    ##   Surrogate splits:
    ##       CompPrice  < 140   to the right, agree=0.805, adj=0.500, (0 split)
    ##       Education  < 16.5  to the right, agree=0.683, adj=0.188, (0 split)
    ##       Income     < 114.5 to the right, agree=0.659, adj=0.125, (0 split)
    ##       Population < 437   to the right, agree=0.659, adj=0.125, (0 split)
    ## 
    ## Node number 82: 16 observations
    ##   mean=5.633125, MSE=2.359334 
    ## 
    ## Node number 83: 25 observations,    complexity param=0.01200941
    ##   mean=7.4452, MSE=2.347105 
    ##   left son=166 (7 obs) right son=167 (18 obs)
    ##   Primary splits:
    ##       Age         < 68    to the right, improve=0.30558290, (0 missing)
    ##       Advertising < 6     to the left,  improve=0.28734230, (0 missing)
    ##       Urban       splits as  LR,        improve=0.18450460, (0 missing)
    ##       Income      < 87.5  to the right, improve=0.10393110, (0 missing)
    ##       Price       < 121.5 to the right, improve=0.08411169, (0 missing)
    ##   Surrogate splits:
    ##       Income      < 101   to the right, agree=0.84, adj=0.429, (0 split)
    ##       Advertising < 1     to the left,  agree=0.80, adj=0.286, (0 split)
    ##       Population  < 384.5 to the right, agree=0.76, adj=0.143, (0 split)
    ## 
    ## Node number 166: 7 observations
    ##   mean=6.087143, MSE=0.8713061 
    ## 
    ## Node number 167: 18 observations
    ##   mean=7.973333, MSE=1.924867

``` r
prp(tree_c)
```

![](homework-4_files/figure-markdown_github/8b.%20training%20regression%20tree-1.png)

``` r
plot(as.party(tree_c))
```

![](homework-4_files/figure-markdown_github/8b.%20training%20regression%20tree-2.png)

``` r
predict_c <- predict(tree_c, testing_c)
mean((testing_c$Sales - predict_c)^2)
```

    ## [1] 4.484515

1.  Use cross-validation in order to determine the optimal level of tree complexity. Does pruning the tree improve the test MSE? *Pruning the tree changes testing MSE to 6.17*

``` r
fitted_control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
cv_tree_c <- train(Sales ~ ., data = training_c, method = "rpart", trControl = fitted_control)
```

    ## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
    ## trainInfo, : There were missing values in resampled performance measures.

``` r
plot(cv_tree_c)
```

![](homework-4_files/figure-markdown_github/8c.%20cross%20validation-1.png)

``` r
plot(as.party(cv_tree_c$finalModel))
```

![](homework-4_files/figure-markdown_github/8c.%20cross%20validation-2.png)

``` r
predict_c_2 = predict(cv_tree_c, testing_c)
mean((testing_c$Sales - predict_c_2)^2)
```

    ## [1] 6.170433

1.  Use the bagging approach in order to analyze this data. What test MSE do you obtain? Use the importance() function to determine which variables are most important. *The testing MSE has decreased to 3.07. The most important variables are Price, ShelveLoc, and CompPrice.*

``` r
bag_c <- randomForest(Sales ~ ., data = training_c, mtry = 10)
bag_c
```

    ## 
    ## Call:
    ##  randomForest(formula = Sales ~ ., data = training_c, mtry = 10) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 10
    ## 
    ##           Mean of squared residuals: 2.826847
    ##                     % Var explained: 61.94

``` r
test_predict <- predict(bag_c, newdata = testing_c)
test_c <- testing_c %>%
  mutate(y_hat_bags = test_predict,
         sq_bags = (y_hat_bags - Sales)^2)
mean(test_c$sq_bags)
```

    ## [1] 3.064914

``` r
importance(bag_c)
```

    ##             IncNodePurity
    ## CompPrice       142.12601
    ## Income          121.43883
    ## Advertising     113.78931
    ## Population       49.38153
    ## Price           337.48808
    ## ShelveLoc       510.96163
    ## Age             105.26429
    ## Education        53.80762
    ## Urban             6.32532
    ## US               16.64603

1.  Use random forests to analyze this data. What test MSE do you obtain? Use the importance() function to determine which variables are most important. Describe the eﬀect of m, the number of variables considered at each split, on the error rate obtained. *The testing MSE has reduced again after the random forest. The new value is 2.86.* *The most important varaibles remain to be Price, ShelveLoc, and CompPrice.*

``` r
rf_c <- randomForest(Sales ~ ., data = training_c, mtry = 10)
rf_c
```

    ## 
    ## Call:
    ##  randomForest(formula = Sales ~ ., data = training_c, mtry = 10) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 10
    ## 
    ##           Mean of squared residuals: 2.82273
    ##                     % Var explained: 62

``` r
predict_3 = predict(rf_c, testing_c)
mean((testing_c$Sales - predict_3)^2)
```

    ## [1] 3.076327

``` r
importance(rf_c)
```

    ##             IncNodePurity
    ## CompPrice      142.187808
    ## Income         124.504551
    ## Advertising    118.541537
    ## Population      48.858268
    ## Price          329.836035
    ## ShelveLoc      492.683771
    ## Age            104.867523
    ## Education       49.768182
    ## Urban            7.216004
    ## US              18.873052

Additional parts: 1. Fit a gradient-boosted tree to the training data and report the estimated test MSE. *Thes testing MSE is 1.825.*

``` r
#gradient-boosted training data tree
grid <- expand.grid(interaction.depth = c(1, 3), 
                    n.trees = seq(0, 2000, by = 100),
                    shrinkage = c(.01, 0.001),
                    n.minobsinnode = 10)
trainControl <- trainControl(method = "cv", number = 5)
gbm_c <- train(Sales ~ ., 
                    data = training_c, 
                    distribution = "gaussian", 
                    method = "gbm",
                    trControl = trainControl, 
                    tuneGrid = grid,
                    verbose = FALSE)
```

    ## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
    ## trainInfo, : There were missing values in resampled performance measures.

``` r
gbm_c
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 201 samples
    ##  10 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 160, 161, 161, 161, 161 
    ## Resampling results across tuning parameters:
    ## 
    ##   shrinkage  interaction.depth  n.trees  RMSE      Rsquared   MAE     
    ##   0.001      1                     0     2.707941        NaN  2.198810
    ##   0.001      1                   100     2.658221  0.3072206  2.162933
    ##   0.001      1                   200     2.614692  0.3338248  2.132458
    ##   0.001      1                   300     2.576900  0.3504440  2.104695
    ##   0.001      1                   400     2.541819  0.3675903  2.078234
    ##   0.001      1                   500     2.507354  0.3873201  2.049150
    ##   0.001      1                   600     2.477261  0.3996785  2.024150
    ##   0.001      1                   700     2.448646  0.4089865  2.000409
    ##   0.001      1                   800     2.422030  0.4193878  1.976754
    ##   0.001      1                   900     2.395144  0.4320000  1.953175
    ##   0.001      1                  1000     2.369702  0.4443429  1.930521
    ##   0.001      1                  1100     2.347348  0.4543584  1.910573
    ##   0.001      1                  1200     2.324835  0.4633463  1.891582
    ##   0.001      1                  1300     2.304761  0.4716868  1.874211
    ##   0.001      1                  1400     2.285108  0.4802836  1.857733
    ##   0.001      1                  1500     2.266160  0.4881776  1.841928
    ##   0.001      1                  1600     2.247849  0.4952085  1.826712
    ##   0.001      1                  1700     2.230193  0.5016889  1.810946
    ##   0.001      1                  1800     2.212902  0.5082136  1.796382
    ##   0.001      1                  1900     2.196370  0.5159103  1.782517
    ##   0.001      1                  2000     2.180384  0.5226449  1.769185
    ##   0.001      3                     0     2.707941        NaN  2.198810
    ##   0.001      3                   100     2.618874  0.5270922  2.125306
    ##   0.001      3                   200     2.536355  0.5458457  2.059462
    ##   0.001      3                   300     2.463641  0.5609235  2.001196
    ##   0.001      3                   400     2.399009  0.5768398  1.948741
    ##   0.001      3                   500     2.339175  0.5873023  1.899297
    ##   0.001      3                   600     2.283001  0.6003513  1.852651
    ##   0.001      3                   700     2.230245  0.6120673  1.809097
    ##   0.001      3                   800     2.183836  0.6223029  1.769263
    ##   0.001      3                   900     2.139413  0.6327802  1.731350
    ##   0.001      3                  1000     2.100495  0.6413786  1.697637
    ##   0.001      3                  1100     2.060811  0.6500729  1.664182
    ##   0.001      3                  1200     2.024805  0.6576679  1.634443
    ##   0.001      3                  1300     1.990637  0.6645470  1.606976
    ##   0.001      3                  1400     1.958253  0.6719663  1.581116
    ##   0.001      3                  1500     1.926799  0.6780308  1.555926
    ##   0.001      3                  1600     1.896877  0.6835754  1.531475
    ##   0.001      3                  1700     1.869955  0.6890268  1.508992
    ##   0.001      3                  1800     1.844864  0.6927973  1.488675
    ##   0.001      3                  1900     1.819554  0.6977340  1.467633
    ##   0.001      3                  2000     1.795715  0.7020571  1.448543
    ##   0.010      1                     0     2.707941        NaN  2.198810
    ##   0.010      1                   100     2.373249  0.4334303  1.933180
    ##   0.010      1                   200     2.185158  0.5158183  1.772317
    ##   0.010      1                   300     2.048617  0.5703270  1.657528
    ##   0.010      1                   400     1.932882  0.6211901  1.560355
    ##   0.010      1                   500     1.832529  0.6527950  1.476900
    ##   0.010      1                   600     1.745839  0.6817872  1.408201
    ##   0.010      1                   700     1.671204  0.7049254  1.351421
    ##   0.010      1                   800     1.611004  0.7193537  1.303108
    ##   0.010      1                   900     1.556412  0.7297823  1.264027
    ##   0.010      1                  1000     1.516110  0.7374018  1.232029
    ##   0.010      1                  1100     1.474418  0.7434692  1.198131
    ##   0.010      1                  1200     1.436771  0.7506812  1.167748
    ##   0.010      1                  1300     1.408708  0.7552229  1.144786
    ##   0.010      1                  1400     1.387525  0.7585885  1.126181
    ##   0.010      1                  1500     1.371684  0.7607528  1.111865
    ##   0.010      1                  1600     1.355249  0.7633029  1.098063
    ##   0.010      1                  1700     1.343519  0.7650400  1.085707
    ##   0.010      1                  1800     1.334934  0.7661658  1.076758
    ##   0.010      1                  1900     1.326727  0.7674918  1.068423
    ##   0.010      1                  2000     1.319458  0.7689825  1.063663
    ##   0.010      3                     0     2.707941        NaN  2.198810
    ##   0.010      3                   100     2.107300  0.6458832  1.703433
    ##   0.010      3                   200     1.792486  0.7054886  1.443958
    ##   0.010      3                   300     1.613111  0.7324781  1.306434
    ##   0.010      3                   400     1.495751  0.7509791  1.205636
    ##   0.010      3                   500     1.432185  0.7565919  1.147454
    ##   0.010      3                   600     1.392641  0.7598289  1.118052
    ##   0.010      3                   700     1.364240  0.7634439  1.096448
    ##   0.010      3                   800     1.351776  0.7636292  1.086685
    ##   0.010      3                   900     1.342408  0.7646782  1.078344
    ##   0.010      3                  1000     1.333988  0.7659227  1.072774
    ##   0.010      3                  1100     1.329169  0.7658424  1.068556
    ##   0.010      3                  1200     1.327850  0.7660202  1.067032
    ##   0.010      3                  1300     1.323073  0.7668483  1.063888
    ##   0.010      3                  1400     1.319027  0.7676234  1.062418
    ##   0.010      3                  1500     1.320768  0.7665930  1.063287
    ##   0.010      3                  1600     1.323541  0.7653681  1.064973
    ##   0.010      3                  1700     1.323796  0.7651722  1.065029
    ##   0.010      3                  1800     1.322689  0.7656988  1.063674
    ##   0.010      3                  1900     1.322997  0.7655423  1.063907
    ##   0.010      3                  2000     1.322834  0.7652224  1.064482
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 1400,
    ##  interaction.depth = 3, shrinkage = 0.01 and n.minobsinnode = 10.

``` r
plot(gbm_c)
```

![](homework-4_files/figure-markdown_github/8.1%20gradient-boosted%20tree-1.png)

``` r
#testing MSE
predict_4 = predict(gbm_c, testing_c)
mean((testing_c$Sales - predict_4)^2)
```

    ## [1] 1.781054

1.  Fit a multiple regression model to the training data and report the estimated test MSE *The testing MSE is 1.02.*

``` r
#multiple regression with training data
lm_c <- lm(Sales ~ ., data = training_c)
summary(lm_c)
```

    ## 
    ## Call:
    ## lm(formula = Sales ~ ., data = training_c)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.73001 -0.74370  0.05735  0.70125  2.79990 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      5.3719058  0.8813459   6.095 6.02e-09 ***
    ## CompPrice        0.0961630  0.0061210  15.710  < 2e-16 ***
    ## Income           0.0145521  0.0026568   5.477 1.36e-07 ***
    ## Advertising      0.1108655  0.0157793   7.026 3.74e-11 ***
    ## Population       0.0002349  0.0005417   0.434    0.665    
    ## Price           -0.0963392  0.0040825 -23.598  < 2e-16 ***
    ## ShelveLocGood    4.8395901  0.2283608  21.193  < 2e-16 ***
    ## ShelveLocMedium  1.9517284  0.1838631  10.615  < 2e-16 ***
    ## Age             -0.0450811  0.0048028  -9.386  < 2e-16 ***
    ## Education       -0.0198464  0.0292315  -0.679    0.498    
    ## UrbanYes         0.1770836  0.1634524   1.083    0.280    
    ## USYes           -0.2112815  0.2150752  -0.982    0.327    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.049 on 189 degrees of freedom
    ## Multiple R-squared:  0.8608, Adjusted R-squared:  0.8527 
    ## F-statistic: 106.3 on 11 and 189 DF,  p-value: < 2.2e-16

``` r
#stepwise regression
stepwise_c <- stepAIC(lm_c, direction='backward')
```

    ## Start:  AIC=30.69
    ## Sales ~ CompPrice + Income + Advertising + Population + Price + 
    ##     ShelveLoc + Age + Education + Urban + US
    ## 
    ##               Df Sum of Sq    RSS     AIC
    ## - Population   1      0.21 208.01  28.895
    ## - Education    1      0.51 208.31  29.185
    ## - US           1      1.06 208.87  29.719
    ## - Urban        1      1.29 209.10  29.939
    ## <none>                     207.81  30.695
    ## - Income       1     32.99 240.79  58.309
    ## - Advertising  1     54.28 262.09  75.338
    ## - Age          1     96.87 304.68 105.608
    ## - CompPrice    1    271.38 479.19 196.625
    ## - ShelveLoc    2    496.48 704.29 272.030
    ## - Price        1    612.29 820.10 304.630
    ## 
    ## Step:  AIC=28.89
    ## Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + 
    ##     Age + Education + Urban + US
    ## 
    ##               Df Sum of Sq    RSS     AIC
    ## - Education    1      0.56 208.57  27.433
    ## - Urban        1      1.23 209.25  28.083
    ## - US           1      1.38 209.40  28.227
    ## <none>                     208.01  28.895
    ## - Income       1     33.00 241.01  56.492
    ## - Advertising  1     65.76 273.78  82.110
    ## - Age          1     96.94 304.95 103.785
    ## - CompPrice    1    271.24 479.26 194.655
    ## - ShelveLoc    2    497.35 705.36 270.337
    ## - Price        1    612.51 820.53 302.735
    ## 
    ## Step:  AIC=27.43
    ## Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + 
    ##     Age + Urban + US
    ## 
    ##               Df Sum of Sq    RSS     AIC
    ## - US           1      1.26 209.83  26.640
    ## - Urban        1      1.34 209.91  26.723
    ## <none>                     208.57  27.433
    ## - Income       1     32.59 241.16  54.615
    ## - Advertising  1     65.22 273.80  80.125
    ## - Age          1     96.45 305.02 101.834
    ## - CompPrice    1    271.34 479.92 192.932
    ## - ShelveLoc    2    497.69 706.26 268.592
    ## - Price        1    612.36 820.93 300.835
    ## 
    ## Step:  AIC=26.64
    ## Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + 
    ##     Age + Urban
    ## 
    ##               Df Sum of Sq    RSS     AIC
    ## - Urban        1      1.25 211.08  25.831
    ## <none>                     209.83  26.640
    ## - Income       1     32.40 242.22  53.498
    ## - Advertising  1     93.16 302.99  98.490
    ## - Age          1     98.08 307.91 101.728
    ## - CompPrice    1    270.09 479.92 190.932
    ## - ShelveLoc    2    496.69 706.52 266.666
    ## - Price        1    611.45 821.28 298.919
    ## 
    ## Step:  AIC=25.83
    ## Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + 
    ##     Age
    ## 
    ##               Df Sum of Sq    RSS     AIC
    ## <none>                     211.08  25.831
    ## - Income       1     32.90 243.97  52.944
    ## - Advertising  1     93.57 304.65  97.587
    ## - Age          1     96.90 307.98  99.773
    ## - CompPrice    1    269.12 480.19 189.047
    ## - ShelveLoc    2    495.49 706.57 264.679
    ## - Price        1    618.85 829.92 299.023

``` r
stepwise_c$anova
```

    ## Stepwise Model Path 
    ## Analysis of Deviance Table
    ## 
    ## Initial Model:
    ## Sales ~ CompPrice + Income + Advertising + Population + Price + 
    ##     ShelveLoc + Age + Education + Urban + US
    ## 
    ## Final Model:
    ## Sales ~ CompPrice + Income + Advertising + Price + ShelveLoc + 
    ##     Age
    ## 
    ## 
    ##           Step Df  Deviance Resid. Df Resid. Dev      AIC
    ## 1                                 189   207.8076 30.69490
    ## 2 - Population  1 0.2066936       190   208.0143 28.89472
    ## 3  - Education  1 0.5577104       191   208.5721 27.43291
    ## 4         - US  1 1.2560734       192   209.8281 26.63975
    ## 5      - Urban  1 1.2472010       193   211.0753 25.83094

``` r
predict_5 = predict(stepwise_c, testing_c)
mean((testing_c$Sales - predict_5)^2)
```

    ## [1] 1.018354

1.  Summarize your results. *The best model with the lowest testing MSE of 1.02 is the backwards stepwise regression model.* *Regression Tree MSE = 4.48* *CV Regression Tree MSE = 6.17* *Bagged Random Forest MSE = 3.07* *Random Forest MSE = 2.86* *Gradient Boosted Model MSE = 1.825* *Backwards Stepwise Regression MSE = 1.01*
