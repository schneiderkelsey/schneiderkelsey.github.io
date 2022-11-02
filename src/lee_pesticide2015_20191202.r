pesticide <- read.table("C:\\Users\\hlee05\\Documents\\ehl\\rare\\pesticide201314.csv",sep=",",header=T,fill=T)
pesticide2015 <- read.table("C:\\Users\\hlee05\\Documents\\ehl\\rare\\pesticide2015.csv",sep=",",header=T,fill=T)
pesticide2015new <- read.table("C:\\Users\\hlee05\\Documents\\ehl\\rare\\pesticide2015new.csv",sep=",",header=T,fill=T)
pesticide2015new <- pesticide2015new[,c(1:18)]


# 12-2-19 DS has 1% corn adjacent ------------------------------------------------------------------------------------------- 

test <- pesticide2015new[pesticide2015new$site=="DS",]
test$logclothianidin <- log10(test$clothianidin)
test$logthiamethoxam <- log10(test$thiamethoxam)
test$lognitenpyram <- log10(test$nitenpyram)
test$logimidacloprid <- log10(test$imidacloprid)
test$logthiacloprid <- log10(test$thiacloprid)
test$logdinotefuran <- log10(test$dinotefuran)
test$logacetamiprid <- log10(test$acetamiprid)

n <- dim(test)
test$cornplt <- rep(0,n[1])
test$cornplt[test$julian>121&test$julian<129] <- 1
test$cornplt2 <- rep(0,n[1])
test$cornplt2[test$julian>120&test$julian<129] <- 1
test$cornplt3 <- rep(0,n[1])
test$cornplt3[test$julian>120&test$julian<128] <- 1
test$i122 <- rep(0,n[1])
test$i122[test$julian==122] <- 1
test$i125 <- rep(0,n[1])
test$i125[test$julian==125] <- 1
test$i128 <- rep(0,n[1])
test$i128[test$julian==128] <- 1
test$i121 <- rep(0,n[1])
test$i121[test$julian==121] <- 1
test$i147 <- rep(0,n[1])
test$i147[test$julian==147] <- 1

png(file="C:\\Users\\hlee05\\Documents\\references\\dendrochronology\\pesticide_2015_DS.png",width=12,height=12,units="in",res=700)
par(mfrow=c(1,1),mai=c(2,2,1,1),oma=c(1,1,1,1),cex=1.3,mex=1.4,lwd=4,cex.lab=1.4,cex.axis=1.4)
plot(test$julian,log10(test$clothianidin),type="b",pch=16,ylim=c(-2,1.5),ylab="Pesticide concentration in pollen sample (log ng/g)",xlab="Julian day for 2015")
points(test$julian,log10(test$thiamethoxam),type="b",pch=16,col="red")
points(test$julian,log10(test$thiacloprid),type="b",pch=16,col="orange")
points(test$julian,log10(test$dinotefuran),type="b",pch=16,col="blue")
points(test$julian,log10(test$nitenpyram),type="b",pch=16,col="green")
points(test$julian,log10(test$imidacloprid),type="b",pch=16,col="purple")
points(test$julian,log10(test$acetamiprid),type="b",pch=16,col="cyan")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)
text(125,-1.8,"Corn planting",adj=0.5,cex=1.2)

legend(135,1.5,legend=c("clothianidin","thiamethoxam","thiacloprid","dinotefuran","nitenpyram","imidacloprid","acetamiprid"),pch=rep(16,7),col=c("black","red","orange","blue","green","purple","cyan"),pt.cex=rep(1.2,7),bty="n")

# 12-2-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

ds2015.clothianidin.lm <- lm(logclothianidin ~ i121+i122+i125+i128,data=test,na.action="na.omit")
summary(ds2015.clothianidin.lm)

Call:
lm(formula = logclothianidin ~ i121 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         2         12         22         32         42         52 
 0.000e+00  6.939e-18  8.751e-18  3.716e-18  1.765e-01 -1.089e-01 
        62         72         82 
-1.089e-01 -1.089e-01  1.503e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)  0.08115    0.06685   1.214  0.29152   
i121         0.82134    0.16374   5.016  0.00741 **
i122         0.64239    0.16374   3.923  0.01720 * 
i125         0.76949    0.16374   4.700  0.00931 **
i128         0.91998    0.16374   5.619  0.00493 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1495 on 4 degrees of freedom
Multiple R-squared:  0.9408,    Adjusted R-squared:  0.8817 
F-statistic:  15.9 on 4 and 4 DF,  p-value: 0.01009

ds2015.clothianidin.lm <- lm(logclothianidin ~ cornplt2,data=test,na.action="na.omit")
summary(ds2015.clothianidin.lm)

# 12-2-19 final model mean for corn planting period 2 (julian=121-128)

Call:
lm(formula = logclothianidin ~ cornplt2, data = test, na.action = "na.omit")

Residuals:
    Min      1Q  Median      3Q     Max 
-0.1459 -0.1089 -0.0188  0.1317  0.1765 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.08115    0.06081   1.334    0.224    
cornplt2     0.78830    0.09122   8.642 5.55e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.136 on 7 degrees of freedom
Multiple R-squared:  0.9143,    Adjusted R-squared:  0.9021 
F-statistic: 74.68 on 1 and 7 DF,  p-value: 5.552e-05

ds2015.clothianidin.gls <- gls(logclothianidin ~ cornplt2,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(ds2015.clothianidin.gls)

# 12-2-19 test for 1st order autoregressive model not significant

Generalized least squares fit by maximum likelihood
  Model: logclothianidin ~ cornplt2 
  Data: test 
       AIC       BIC  logLik
  -4.66368 -3.874781 6.33184

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
0.06066025 

Coefficients:
                Value  Std.Error  t-value p-value
(Intercept) 0.0836405 0.06378665 1.311254  0.2311
cornplt2    0.7856222 0.09479716 8.287402  0.0001

 Correlation: 
         (Intr)
cornplt2 -0.662

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.2150720 -0.9291806 -0.1552261  1.0995065  1.4511506 

Residual standard error: 0.1199311 
Degrees of freedom: 9 total; 7 residual

# 12-2-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

ds2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i121+i122+i125+i128,data=test,na.action="na.omit")
summary(ds2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ i121 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         2         12         22         32         42         52 
-2.776e-17 -3.469e-17  1.357e-17 -6.209e-17  2.938e-02 -5.201e-01 
        62         72         82 
 5.101e-01  5.007e-01 -5.201e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.7240     0.2294  -3.156   0.0343 *
i121          1.2616     0.5620   2.245   0.0881 .
i122          1.4308     0.5620   2.546   0.0636 .
i125          1.1965     0.5620   2.129   0.1003  
i128          0.3095     0.5620   0.551   0.6111  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.513 on 4 degrees of freedom
Multiple R-squared:  0.7529,    Adjusted R-squared:  0.5058 
F-statistic: 3.047 on 4 and 4 DF,  p-value: 0.153

# 12-2-19 final model mean for corn planting period 3 (julian=121-125)

ds2015.thiamethoxam.lm <- lm(logthiamethoxam ~ cornplt3,data=test,na.action="na.omit")
summary(ds2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ cornplt3, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.57168 -0.09981 -0.02220  0.25791  0.45849 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)  -0.6724     0.1663  -4.043  0.00491 **
cornplt3      1.2447     0.2881   4.321  0.00348 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4074 on 7 degrees of freedom
Multiple R-squared:  0.7273,    Adjusted R-squared:  0.6883 
F-statistic: 18.67 on 1 and 7 DF,  p-value: 0.003477

ds2015.thiamethoxam.gls <- gls(logthiamethoxam ~ cornplt3,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(ds2015.thiamethoxam.gls)

# 12-2-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logthiamethoxam ~ cornplt3 
  Data: test 
       AIC      BIC    logLik
  14.00478 14.79368 -3.002389

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.3809893 

Coefficients:
                 Value Std.Error   t-value p-value
(Intercept) -0.6579557 0.1171447 -5.616606   8e-04
cornplt3     1.2611897 0.2143044  5.885038   6e-04

 Correlation: 
         (Intr)
cornplt3 -0.59 

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.6184842 -0.3610720 -0.1013136  0.6721019  1.2259287 

Residual standard error: 0.3621719 
Degrees of freedom: 9 total; 7 residual

ds2015.nitenpyram.lm <- lm(lognitenpyram ~ i121+i122+i125+i128,data=test,na.action="na.omit")
summary(ds2015.nitenpyram.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = lognitenpyram ~ i121 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         2         12         22         32         42         52 
 0.000e+00  0.000e+00  4.163e-17 -3.021e-17  4.329e-01  2.593e-01 
        62         72         82 
-4.192e-01  2.844e-02 -3.015e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)  -0.7954     0.1616  -4.923  0.00791 **
i121          0.6600     0.3958   1.667  0.17074   
i122          0.7709     0.3958   1.948  0.12328   
i125          0.4329     0.3958   1.094  0.33546   
i128          0.4329     0.3958   1.094  0.33546   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3613 on 4 degrees of freedom
Multiple R-squared:  0.6105,    Adjusted R-squared:  0.2211 
F-statistic: 1.568 on 4 and 4 DF,  p-value: 0.3369

# 12-2-19 decreasing trend in nitenpyram

ds2015.nitenpyram.lm <- lm(lognitenpyram ~ julian,data=test,na.action="na.omit")
summary(ds2015.nitenpyram.lm)

Call:
lm(formula = lognitenpyram ~ julian, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.40452 -0.04211  0.03180  0.10863  0.20242 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.72485    0.96335   4.905 0.001744 ** 
julian      -0.03982    0.00727  -5.477 0.000929 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1904 on 7 degrees of freedom
Multiple R-squared:  0.8108,    Adjusted R-squared:  0.7838 
F-statistic:    30 on 1 and 7 DF,  p-value: 0.0009286

ds2015.nitenpyram.gls <- gls(lognitenpyram ~ julian,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(ds2015.nitenpyram.gls)

# 12-2-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: lognitenpyram ~ julian 
  Data: test 
         AIC       BIC   logLik
  -0.3772091 0.4116893 4.188605

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
      Phi 
-0.405372 

Coefficients:
                Value Std.Error   t-value p-value
(Intercept)  4.759659 0.6746974  7.054509   2e-04
julian      -0.040081 0.0050976 -7.862650   1e-04

 Correlation: 
       (Intr)
julian -0.998

Standardized residuals:
      Min        Q1       Med        Q3       Max 
-2.449612 -0.275629  0.214704  0.642047  1.245300 

Residual standard error: 0.1645479 
Degrees of freedom: 9 total; 7 residual

ds2015.imidacloprid.lm <- lm(logimidacloprid ~ i121+i122+i125+i128,data=test,na.action="na.omit")
summary(ds2015.imidacloprid.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logimidacloprid ~ i121 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         2         12         22         32         42         52 
-2.776e-17  2.082e-17  3.110e-19 -2.744e-17  4.324e-02 -1.681e-01 
        62         72         82 
-5.809e-02 -3.981e-01  5.810e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   0.1199     0.1627   0.737    0.502
i121         -0.2061     0.3986  -0.517    0.632
i122          0.1895     0.3986   0.475    0.659
i125         -0.2742     0.3986  -0.688    0.529
i128          0.3384     0.3986   0.849    0.444

Residual standard error: 0.3639 on 4 degrees of freedom
Multiple R-squared:  0.3359,    Adjusted R-squared:  -0.3282 
F-statistic: 0.5058 on 4 and 4 DF,  p-value: 0.7373

ds2015.imidacloprid.gls <- gls(logimidacloprid ~ i121,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(ds2015.imidacloprid.gls)

# 12-2-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logimidacloprid ~ i121 
  Data: test 
       AIC      BIC    logLik
  7.652642 8.441541 0.1736789

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.7075622 

Coefficients:
                  Value  Std.Error    t-value p-value
(Intercept)  0.11077373 0.05662718  1.9561937  0.0913
i121        -0.05640725 0.27640505 -0.2040746  0.8441

 Correlation: 
     (Intr)
i121 -0.35 

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.2036119 -0.4918589 -0.1514542  0.6146847  1.8262890 

Residual standard error: 0.3231632 
Degrees of freedom: 9 total; 7 residual

ds2015.thiacloprid.lm <- lm(logthiacloprid ~ i121+i122+i125+i128,data=test,na.action="na.omit")
summary(ds2015.thiacloprid.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logthiacloprid ~ i121 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         2         12         22         32         42         52 
 2.776e-17  6.939e-18 -4.857e-17  4.722e-17 -1.663e-01 -3.190e-01 
        62         72         82 
-1.694e-01 -1.357e-02  6.683e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.3865     0.1739  -2.223   0.0903 .
i121         -0.8211     0.4260  -1.927   0.1262  
i122          0.1567     0.4260   0.368   0.7317  
i125         -0.8281     0.4260  -1.944   0.1238  
i128          0.2074     0.4260   0.487   0.6518  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3889 on 4 degrees of freedom
Multiple R-squared:  0.6728,    Adjusted R-squared:  0.3457 
F-statistic: 2.057 on 4 and 4 DF,  p-value: 0.2511

ds2015.dinotefuran.lm <- lm(logdinotefuran ~ i121+i122+i125+i128+i147,data=test,na.action="na.omit")
summary(ds2015.dinotefuran.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting except julian 147 is significantly higher

Call:
lm(formula = logdinotefuran ~ i121 + i122 + i125 + i128 + i147, 
    data = test, na.action = "na.omit")

Residuals:
         2         12         22         32         42         52 
 6.906e-18  2.975e-19  1.353e-17  3.220e-17 -6.775e-03 -1.464e-01 
        62         72         82 
 2.996e-01 -1.464e-01  6.427e-17 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.391165   0.105168 -13.228 0.000934 ***
i121        -0.006775   0.235163  -0.029 0.978825    
i122         0.386800   0.235163   1.645 0.198556    
i125        -0.006775   0.235163  -0.029 0.978825    
i128        -0.006775   0.235163  -0.029 0.978825    
i147         1.341042   0.235163   5.703 0.010694 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2103 on 3 degrees of freedom
Multiple R-squared:  0.9245,    Adjusted R-squared:  0.7986 
F-statistic: 7.342 on 5 and 3 DF,  p-value: 0.06577

ds2015.dinotefuran.gls <- gls(logdinotefuran ~ i147,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(ds2015.dinotefuran.gls)

# 12-2-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logdinotefuran ~ i147 
  Data: test 
         AIC       BIC   logLik
  -0.3338288 0.4550695 4.166914

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.4774729 

Coefficients:
                Value  Std.Error    t-value p-value
(Intercept) -1.334596 0.04248607 -31.412551   0e+00
i147         1.187544 0.18142812   6.545534   3e-04

 Correlation: 
     (Intr)
i147 -0.346

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.1881666 -0.3707440 -0.3707440  0.5673173  1.9327982 

Residual standard error: 0.1708565 
Degrees of freedom: 9 total; 7 residual

ds2015.acetamiprid.lm <- lm(logacetamiprid ~ i121+i122+i125+i128,data=test,na.action="na.omit")
summary(ds2015.acetamiprid.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logacetamiprid ~ i121 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         2         12         22         32         42         52 
-8.327e-17  9.021e-17  8.183e-18 -1.028e-16 -2.217e-01 -1.811e-01 
        62         72         82 
-4.208e-01  1.471e-01  6.765e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept) -0.46862    0.19214  -2.439   0.0713 .
i121         0.36238    0.47065   0.770   0.4843  
i122         0.13794    0.47065   0.293   0.7840  
i125        -0.04424    0.47065  -0.094   0.9296  
i128         0.19860    0.47065   0.422   0.6947  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4296 on 4 degrees of freedom
Multiple R-squared:  0.1633,    Adjusted R-squared:  -0.6734 
F-statistic: 0.1952 on 4 and 4 DF,  p-value: 0.9287

# end ds2015 ----------------------------------------------------------------

# 12-2-19 SC has 8% corn adjacent ------------------------------------------------------------------------------------------- 

test <- pesticide2015new[pesticide2015new$site=="SC",]
test$logclothianidin <- log10(test$clothianidin)
test$logthiamethoxam <- log10(test$thiamethoxam)
test$lognitenpyram <- log10(test$nitenpyram)
test$logimidacloprid <- log10(test$imidacloprid)
test$logthiacloprid <- log10(test$thiacloprid)
test$logdinotefuran <- log10(test$dinotefuran)
test$logacetamiprid <- log10(test$acetamiprid)

n <- dim(test)
test$cornplt <- rep(0,n[1])
test$cornplt[test$julian>121&test$julian<129] <- 1
test$cornplt2 <- rep(0,n[1])
test$cornplt2[test$julian>120&test$julian<129] <- 1
test$cornplt3 <- rep(0,n[1])
test$cornplt3[test$julian>120&test$julian<128] <- 1
test$i122 <- rep(0,n[1])
test$i122[test$julian==122] <- 1
test$i125 <- rep(0,n[1])
test$i125[test$julian==125] <- 1
test$i128 <- rep(0,n[1])
test$i128[test$julian==128] <- 1
test$i119 <- rep(0,n[1])
test$i119[test$julian==119] <- 1
test$i147 <- rep(0,n[1])
test$i147[test$julian==147] <- 1
test$i139 <- rep(0,n[1])
test$i139[test$julian==139] <- 1
test$i143 <- rep(0,n[1])
test$i143[test$julian==143] <- 1
test$intercept <- rep(1,n[1])

png(file="C:\\Users\\hlee05\\Documents\\references\\dendrochronology\\pesticide_2015_SC.png",width=12,height=12,units="in",res=700)
par(mfrow=c(1,1),mai=c(2,2,1,1),oma=c(1,1,1,1),cex=1.3,mex=1.4,lwd=4,cex.lab=1.4,cex.axis=1.4)
plot(test$julian,log10(test$clothianidin),type="b",pch=16,ylim=c(-2,2),ylab="Pesticide concentration in pollen sample (log ng/g)",xlab="Julian day for 2015")
points(test$julian,log10(test$thiamethoxam),type="b",pch=16,col="red")
points(test$julian,log10(test$thiacloprid),type="b",pch=16,col="orange")
points(test$julian,log10(test$dinotefuran),type="b",pch=16,col="blue")
points(test$julian,log10(test$nitenpyram),type="b",pch=16,col="green")
points(test$julian,log10(test$imidacloprid),type="b",pch=16,col="purple")
points(test$julian,log10(test$acetamiprid),type="b",pch=16,col="cyan")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)
text(125,-1.8,"Corn planting",adj=0.5,cex=1.2)

legend(135,2,legend=c("clothianidin","thiamethoxam","thiacloprid","dinotefuran","nitenpyram","imidacloprid","acetamiprid"),pch=rep(16,7),col=c("black","red","orange","blue","green","purple","cyan"),pt.cex=rep(1.2,7),bty="n")

# 12-2-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

sc2015.clothianidin.lm <- lm(logclothianidin ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(sc2015.clothianidin.lm)

Call:
lm(formula = logclothianidin ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         8         18         28         38         48         58 
-1.388e-17 -3.469e-18 -4.510e-17 -2.877e-17  4.662e-01 -2.771e-01 
        68         78         88 
-2.771e-01  3.651e-01 -2.771e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)   0.2493     0.1704   1.463   0.2174  
i119         -0.4583     0.4175  -1.098   0.3339  
i122          1.4521     0.4175   3.478   0.0254 *
i125          0.7646     0.4175   1.832   0.1410  
i128          0.3623     0.4175   0.868   0.4344  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3811 on 4 degrees of freedom
Multiple R-squared:  0.8135,    Adjusted R-squared:  0.6271 
F-statistic: 4.363 on 4 and 4 DF,  p-value: 0.09134


sc2015.clothianidin.lm <- lm(logclothianidin ~ i122,data=test,na.action="na.omit")
summary(sc2015.clothianidin.lm)

# 12-2-19 final model mean for julian=122 elevated

Call:
lm(formula = logclothianidin ~ i122, data = test, na.action = "na.omit")

Residuals:
    Min      1Q  Median      3Q     Max 
-0.5419 -0.3607  0.0000  0.2815  0.6811 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)   0.3329     0.1610   2.068   0.0775 .
i122          1.3685     0.4829   2.834   0.0253 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4553 on 7 degrees of freedom
Multiple R-squared:  0.5343,    Adjusted R-squared:  0.4677 
F-statistic:  8.03 on 1 and 7 DF,  p-value: 0.02527

sc2015.clothianidin.gls <- gls(logclothianidin ~ i122,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(sc2015.clothianidin.gls)

# 12-2-19 test for 1st order autoregressive model not significant

Generalized least squares fit by maximum likelihood
  Model: logclothianidin ~ i122 
  Data: test 
       AIC      BIC    logLik
  17.07515 17.86405 -4.537576

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
0.07790565 

Coefficients:
                Value Std.Error  t-value p-value
(Intercept) 0.3250108 0.1706912 1.904086  0.0986
i122        1.3643552 0.4751525 2.871405  0.0239

 Correlation: 
     (Intr)
i122 -0.304

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.3294165 -0.8782943  0.0298601  0.7203419  1.7150282 

Residual standard error: 0.4016968 
Degrees of freedom: 9 total; 7 residual

# 12-2-19 mean comparison tests using pulse intervention terms for individual days within corn planting period not significant

sc2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(sc2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         8         18         28         38         48         58 
-5.551e-17  4.163e-17  1.116e-16  8.659e-17  8.503e-01 -1.171e+00 
        68         78         88 
 2.938e-01  1.197e+00 -1.171e+00 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept) -0.07339    0.49922  -0.147    0.890
i119        -0.62126    1.22283  -0.508    0.638
i122        -0.62126    1.22283  -0.508    0.638
i125         0.79156    1.22283   0.647    0.553
i128         0.41403    1.22283   0.339    0.752

Residual standard error: 1.116 on 4 degrees of freedom
Multiple R-squared:  0.2395,    Adjusted R-squared:  -0.521 
F-statistic: 0.3149 on 4 and 4 DF,  p-value: 0.8554

sc2015.thiamethoxam.gls <- gls(logthiamethoxam ~ i125,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(sc2015.thiamethoxam.gls)

# 12-2-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logthiamethoxam ~ i125 
  Data: test 
       AIC      BIC    logLik
  28.82432 29.61322 -10.41216

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
    Phi 
-0.3298 

Coefficients:
                 Value Std.Error    t-value p-value
(Intercept) -0.1200554 0.2480265 -0.4840426  0.6431
i125         0.8043469 0.9134301  0.8805784  0.4078

 Correlation: 
     (Intr)
i125 -0.433

Standardized residuals:
        Min          Q1         Med          Q3         Max 
-1.38792497 -0.70946870  0.04183012  0.56883830  1.53609345 

Residual standard error: 0.8098923 
Degrees of freedom: 9 total; 7 residual

sc2015.nitenpyram.lm <- lm(lognitenpyram ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(sc2015.nitenpyram.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = lognitenpyram ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         8         18         28         38         48         58 
 0.000e+00  5.551e-17  1.450e-17  4.226e-17  2.551e-01 -5.971e-01 
        68         78         88 
 4.272e-01  5.119e-01 -5.971e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.6176     0.2472  -2.498   0.0669 .
i119          0.2551     0.6056   0.421   0.6953  
i122          0.2551     0.6056   0.421   0.6953  
i125          0.2551     0.6056   0.421   0.6953  
i128          0.2551     0.6056   0.421   0.6953  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5529 on 4 degrees of freedom
Multiple R-squared:  0.1058,    Adjusted R-squared:  -0.7885 
F-statistic: 0.1183 on 4 and 4 DF,  p-value: 0.9688

sc2015.imidacloprid.lm <- lm(logimidacloprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(sc2015.imidacloprid.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logimidacloprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         8         18         28         38         48         58 
 1.388e-17 -3.123e-17 -5.593e-18 -1.947e-17 -7.904e-02 -6.071e-01 
        68         78         88 
-1.550e-01  7.029e-01  1.382e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)   0.7016     0.2135   3.285   0.0303 *
i119         -0.2150     0.5231  -0.411   0.7021  
i122          0.2994     0.5231   0.572   0.5977  
i125         -0.3744     0.5231  -0.716   0.5137  
i128         -0.3779     0.5231  -0.722   0.5100  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4775 on 4 degrees of freedom
Multiple R-squared:  0.2882,    Adjusted R-squared:  -0.4236 
F-statistic: 0.4049 on 4 and 4 DF,  p-value: 0.7987

sc2015.imidacloprid.gls <- gls(logimidacloprid ~ i119,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(sc2015.imidacloprid.gls)

sc2015.thiacloprid.lm <- lm(logthiacloprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(sc2015.thiacloprid.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logthiacloprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         8         18         28         38         48         58 
-5.551e-17  4.163e-17  2.838e-17 -1.952e-17 -9.566e-01 -5.166e-01 
        68         78         88 
-7.257e-02  1.064e+00  4.819e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept) -0.25812    0.35714  -0.723    0.510
i119         0.26586    0.87482   0.304    0.776
i122         0.12144    0.87482   0.139    0.896
i125         0.09948    0.87482   0.114    0.915
i128         0.04558    0.87482   0.052    0.961

Residual standard error: 0.7986 on 4 degrees of freedom
Multiple R-squared:  0.02519,   Adjusted R-squared:  -0.9496 
F-statistic: 0.02584 on 4 and 4 DF,  p-value: 0.9981


sc2015.dinotefuran.lm <- lm(logdinotefuran ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(sc2015.dinotefuran.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting 

Call:
lm(formula = logdinotefuran ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         8         18         28         38         48         58 
 5.551e-17 -4.163e-17 -8.389e-17 -1.345e-16 -8.137e-01 -9.534e-01 
        68         78         88 
-3.513e-01  1.481e+00  6.373e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  -0.5842     0.4634  -1.261    0.276
i119         -0.8137     1.1351  -0.717    0.513
i122         -0.8137     1.1351  -0.717    0.513
i125         -0.8137     1.1351  -0.717    0.513
i128         -0.8137     1.1351  -0.717    0.513

Residual standard error: 1.036 on 4 degrees of freedom
Multiple R-squared:  0.2552,    Adjusted R-squared:  -0.4896 
F-statistic: 0.3426 on 4 and 4 DF,  p-value: 0.8379

sc2015.dinotefuran.gls <- gls(logdinotefuran ~ intercept-1,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(sc2015.dinotefuran.gls)

# 12-2-19 significant 1st order AR1 coef >0

Generalized least squares fit by maximum likelihood
  Model: logdinotefuran ~ intercept - 1 
  Data: test 
       AIC      BIC    logLik
  24.38489 24.97656 -9.192445

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
      Phi 
0.5670911 

Coefficients:
               Value Std.Error   t-value p-value
intercept -0.8842157 0.4726726 -1.870673  0.0983

Standardized residuals:
        Min          Q1         Med          Q3         Max 
-0.81833912 -0.64341829 -0.64341829 -0.06428405  2.23085703 

Residual standard error: 0.7984297 
Degrees of freedom: 9 total; 8 residual

sc2015.acetamiprid.lm <- lm(logacetamiprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(sc2015.acetamiprid.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logacetamiprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         8         18         28         38         48         58 
 2.776e-17  3.469e-17 -4.132e-17 -1.038e-18 -1.937e-01 -4.806e-01 
        68         78         88 
-4.806e-01  1.636e+00 -4.806e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  -0.8473     0.4126  -2.053    0.109
i119         -0.1937     1.0108  -0.192    0.857
i122          0.7525     1.0108   0.745    0.498
i125         -0.1937     1.0108  -0.192    0.857
i128         -0.1937     1.0108  -0.192    0.857

Residual standard error: 0.9227 on 4 degrees of freedom
Multiple R-squared:  0.1655,    Adjusted R-squared:  -0.6689 
F-statistic: 0.1984 on 4 and 4 DF,  p-value: 0.9269

# end sc2015 ----------------------------------------------------------------

# 12-2-19 IB has 22% corn adjacent ------------------------------------------------------------------------------------------- 

test <- pesticide2015new[pesticide2015new$site=="IB",]
test$logclothianidin <- log10(test$clothianidin)
test$logthiamethoxam <- log10(test$thiamethoxam)
test$lognitenpyram <- log10(test$nitenpyram)
test$logimidacloprid <- log10(test$imidacloprid)
test$logthiacloprid <- log10(test$thiacloprid)
test$logdinotefuran <- log10(test$dinotefuran)
test$logacetamiprid <- log10(test$acetamiprid)

n <- dim(test)
test$cornplt <- rep(0,n[1])
test$cornplt[test$julian>118&test$julian<123] <- 1
test$cornplt2 <- rep(0,n[1])
test$cornplt2[test$julian>122&test$julian<133] <- 1
test$cornplt3 <- rep(0,n[1])
test$cornplt3[test$julian>120&test$julian<128] <- 1
test$i122 <- rep(0,n[1])
test$i122[test$julian==122] <- 1
test$i125 <- rep(0,n[1])
test$i125[test$julian==125] <- 1
test$i128 <- rep(0,n[1])
test$i128[test$julian==128] <- 1
test$i119 <- rep(0,n[1])
test$i119[test$julian==119] <- 1
test$i147 <- rep(0,n[1])
test$i147[test$julian==147] <- 1
test$i131 <- rep(0,n[1])
test$i131[test$julian==131] <- 1
test$i134 <- rep(0,n[1])
test$i134[test$julian==134] <- 1
test$intercept <- rep(1,n[1])

png(file="C:\\Users\\hlee05\\Documents\\references\\dendrochronology\\pesticide_2015_IB.png",width=12,height=12,units="in",res=700)
par(mfrow=c(1,1),mai=c(2,2,1,1),oma=c(1,1,1,1),cex=1.3,mex=1.4,lwd=4,cex.lab=1.4,cex.axis=1.4)
plot(test$julian,log10(test$clothianidin),type="b",pch=16,ylim=c(-2,2),ylab="Pesticide concentration in pollen sample (log ng/g)",xlab="Julian day for 2015")
points(test$julian,log10(test$thiamethoxam),type="b",pch=16,col="red")
points(test$julian,log10(test$thiacloprid),type="b",pch=16,col="orange")
points(test$julian,log10(test$dinotefuran),type="b",pch=16,col="blue")
points(test$julian,log10(test$nitenpyram),type="b",pch=16,col="green")
points(test$julian,log10(test$imidacloprid),type="b",pch=16,col="purple")
points(test$julian,log10(test$acetamiprid),type="b",pch=16,col="cyan")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)
text(125,-1.8,"Corn planting",adj=0.5,cex=1.2)

legend(135,2,legend=c("clothianidin","thiamethoxam","thiacloprid","dinotefuran","nitenpyram","imidacloprid","acetamiprid"),pch=rep(16,7),col=c("black","red","orange","blue","green","purple","cyan"),pt.cex=rep(1.2,7),bty="n")


# 12-2-19 mean comparison tests using pulse intervention terms for individual days within & following corn planting period

ib2015.clothianidin.lm <- lm(logclothianidin ~ i119+i122+i125+i128+i131,data=test,na.action="na.omit")
summary(ib2015.clothianidin.lm)

Call:
lm(formula = logclothianidin ~ i119 + i122 + i125 + i128 + i131, 
    data = test, na.action = "na.omit")

Residuals:
         5         15         25         35         45         55 
-3.469e-18  2.602e-18 -3.431e-18 -7.852e-18  4.015e-18  1.236e-01 
        65         75         85 
-4.121e-02 -4.121e-02 -4.121e-02 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)  0.01341    0.04121   0.325  0.76622   
i119         0.56027    0.09215   6.080  0.00893 **
i122         0.69296    0.09215   7.520  0.00487 **
i125         1.02509    0.09215  11.125  0.00156 **
i128         0.88881    0.09215   9.646  0.00237 **
i131         0.94266    0.09215  10.230  0.00199 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.08242 on 3 degrees of freedom
Multiple R-squared:  0.9878,    Adjusted R-squared:  0.9674 
F-statistic: 48.49 on 5 and 3 DF,  p-value: 0.004538

ib2015.clothianidin.lm <- lm(logclothianidin ~ cornplt+cornplt2,data=test,na.action="na.omit")
summary(ib2015.clothianidin.lm)

# 12-2-19 final model mean for corn planting period (julian=119-122) differs from corn planting period 2 (julian=123-131)

Call:
lm(formula = logclothianidin ~ cornplt + cornplt2, data = test, 
    na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.06635 -0.04121 -0.04121  0.06635  0.12363 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.01341    0.04011   0.334 0.749457    
cornplt      0.62662    0.06947   9.021 0.000104 ***
cornplt2     0.95219    0.06126  15.543 4.49e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.08021 on 6 degrees of freedom
Multiple R-squared:  0.9768,    Adjusted R-squared:  0.9691 
F-statistic: 126.6 on 2 and 6 DF,  p-value: 1.242e-05

ib2015.clothianidin.gls <- gls(logclothianidin ~ cornplt+cornplt2,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(ib2015.clothianidin.gls)

# 12-2-19 since 1st order autoregressive model coef<0, use ordinary least squares above

Generalized least squares fit by maximum likelihood
  Model: logclothianidin ~ cornplt + cornplt2 
  Data: test 
        AIC      BIC   logLik
  -13.86943 -12.8833 11.93471

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.2184549 

Coefficients:
                Value  Std.Error   t-value p-value
(Intercept) 0.0098212 0.03363892  0.291959  0.7801
cornplt     0.6373044 0.06009219 10.605444  0.0000
cornplt2    0.9624707 0.05290537 18.192306  0.0000

 Correlation: 
         (Intr) crnplt
cornplt  -0.557       
cornplt2 -0.671  0.331

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.1185402 -0.5729377 -0.5729377  0.9024050  1.9375381 

Residual standard error: 0.06565872 
Degrees of freedom: 9 total; 6 residual


# 12-2-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

ib2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i119+i122+i125+i128+i131+i134,data=test,na.action="na.omit")
summary(ib2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ i119 + i122 + i125 + i128 + i131 + 
    i134, data = test, na.action = "na.omit")

Residuals:
         5         15         25         35         45         55 
-9.975e-18 -5.529e-18 -5.455e-18 -1.906e-17 -1.175e-17 -6.486e-17 
        65         75         85 
 2.998e-01 -3.028e-01  3.005e-03 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept) -0.03922    0.17394  -0.225   0.8426  
i119         0.46099    0.34789   1.325   0.3163  
i122         0.22871    0.34789   0.657   0.5785  
i125         0.54613    0.34789   1.570   0.2570  
i128         0.86768    0.34789   2.494   0.1301  
i131         0.43068    0.34789   1.238   0.3413  
i134         1.19676    0.34789   3.440   0.0751 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3013 on 2 degrees of freedom
Multiple R-squared:  0.8843,    Adjusted R-squared:  0.5371 
F-statistic: 2.547 on 6 and 2 DF,  p-value: 0.3086


# 12-2-19 final model mean for corn planting period 3 (julian=121-125)

ib2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i128+i134,data=test,na.action="na.omit")
summary(ib2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ i128 + i134, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.54084 -0.00936  0.00000  0.19261  0.30806 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)   0.1989     0.1127   1.765   0.1281  
i128          0.6296     0.3187   1.975   0.0956 .
i134          0.9587     0.3187   3.008   0.0238 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2981 on 6 degrees of freedom
Multiple R-squared:   0.66,     Adjusted R-squared:  0.5466 
F-statistic: 5.823 on 2 and 6 DF,  p-value: 0.03931

ib2015.thiamethoxam.gls <- gls(logthiamethoxam ~ i134,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(ib2015.thiamethoxam.gls)

# 12-2-19 since AR1 coef significantly > 0, do not use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logthiamethoxam ~ i134 
  Data: test 
       AIC      BIC    logLik
  10.86983 11.65873 -1.434915

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
      Phi 
0.4255787 

Coefficients:
                Value Std.Error  t-value p-value
(Intercept) 0.2686654 0.1734499 1.548951  0.1653
i134        0.8475600 0.2968078 2.855586  0.0245

 Correlation: 
     (Intr)
i134 -0.163

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.9688949 -0.2552794  0.1332323  0.4936409  1.8049332 

Residual standard error: 0.3101506 
Degrees of freedom: 9 total; 7 residual


ib2015.nitenpyram.lm <- lm(lognitenpyram ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(ib2015.nitenpyram.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = lognitenpyram ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
       5       15       25       35       45       55       65       75 
 0.00000  0.00000  0.00000  0.00000  0.16689  0.18261 -0.06577 -0.68527 
      85 
 0.40155 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.5294     0.1866  -2.837    0.047 *
i119          0.1669     0.4571   0.365    0.734  
i122          0.3025     0.4571   0.662    0.544  
i125          0.1669     0.4571   0.365    0.734  
i128          0.4254     0.4571   0.931    0.405  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4172 on 4 degrees of freedom
Multiple R-squared:  0.2256,    Adjusted R-squared:  -0.5487 
F-statistic: 0.2914 on 4 and 4 DF,  p-value: 0.8703


ib2015.nitenpyram.gls <- gls(lognitenpyram ~ intercept-1,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(ib2015.nitenpyram.gls)

# 12-2-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: lognitenpyram ~ intercept - 1 
  Data: test 
       AIC     BIC    logLik
  10.79183 11.3835 -2.395915

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
        Phi 
-0.04513427 

Coefficients:
               Value Std.Error   t-value p-value
intercept -0.4130521 0.1073261 -3.848571  0.0049

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-2.5362750  0.1599114  0.1599114  0.5888314  0.9777435 

Residual standard error: 0.3160612 
Degrees of freedom: 9 total; 8 residual


ib2015.imidacloprid.lm <- lm(logimidacloprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(ib2015.imidacloprid.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logimidacloprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         5         15         25         35         45         55 
 0.000e+00  0.000e+00  0.000e+00  5.551e-17  9.142e-02 -4.143e-01 
        65         75         85 
 3.473e-02 -1.964e-01  4.845e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   0.1176     0.1507   0.780    0.479
i119          0.4401     0.3693   1.192    0.299
i122          0.2364     0.3693   0.640    0.557
i125         -0.2161     0.3693  -0.585    0.590
i128          0.4111     0.3693   1.113    0.328

Residual standard error: 0.3371 on 4 degrees of freedom
Multiple R-squared:  0.4559,    Adjusted R-squared:  -0.08815 
F-statistic: 0.838 on 4 and 4 DF,  p-value: 0.5659

ib2015.imidacloprid.gls <- gls(logimidacloprid ~ intercept-1,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(ib2015.imidacloprid.gls)

# 12-2-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logimidacloprid ~ intercept - 1 
  Data: test 
       AIC      BIC    logLik
  9.691067 10.28274 -1.845533

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.2622572 

Coefficients:
              Value  Std.Error  t-value p-value
intercept 0.1966925 0.08485383 2.318016  0.0491

Standardized residuals:
        Min          Q1         Med          Q3         Max 
-1.60926252 -0.89864892  0.04007172  1.08273424  1.32213464 

Residual standard error: 0.3066008 
Degrees of freedom: 9 total; 8 residual


ib2015.thiacloprid.lm <- lm(logthiacloprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(ib2015.thiacloprid.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logthiacloprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
       5       15       25       35       45       55       65       75 
 0.00000  0.00000  0.00000  0.00000 -0.07261 -0.07783  0.11318 -0.61495 
      85 
 0.65222 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.5277     0.2034  -2.594   0.0604 .
i119         -0.6870     0.4983  -1.379   0.2401  
i122          0.3691     0.4983   0.741   0.5000  
i125         -0.3169     0.4983  -0.636   0.5593  
i128          0.3981     0.4983   0.799   0.4691  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4549 on 4 degrees of freedom
Multiple R-squared:  0.5098,    Adjusted R-squared:  0.01964 
F-statistic:  1.04 on 4 and 4 DF,  p-value: 0.4853


ib2015.dinotefuran.lm <- lm(logdinotefuran ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(ib2015.dinotefuran.lm)

# 12-2-19 some mean differences between corn planting and non-corn planting 

Call:
lm(formula = logdinotefuran ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         5         15         25         35         45         55 
-6.939e-18 -1.735e-18  5.204e-18 -6.770e-18  2.359e-02 -1.161e-01 
        65         75         85 
-1.161e-01  3.246e-01 -1.161e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.42153    0.08554 -16.618 7.68e-05 ***
i119         0.02359    0.20954   0.113   0.9158    
i122         0.87790    0.20954   4.190   0.0138 *  
i125         0.02359    0.20954   0.113   0.9158    
i128         0.02359    0.20954   0.113   0.9158    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1913 on 4 degrees of freedom
Multiple R-squared:  0.8212,    Adjusted R-squared:  0.6425 
F-statistic: 4.594 on 4 and 4 DF,  p-value: 0.08444

ib2015.dinotefuran.lm <- lm(logdinotefuran ~ i122,data=test,na.action="na.omit")
summary(ib2015.dinotefuran.lm)

Call:
lm(formula = logdinotefuran ~ i122, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.12492 -0.12492  0.01474  0.01474  0.31577 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -1.4127     0.0513 -27.536 2.14e-08 ***
i122          0.8690     0.1539   5.646 0.000777 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1451 on 7 degrees of freedom
Multiple R-squared:   0.82,     Adjusted R-squared:  0.7943 
F-statistic: 31.88 on 1 and 7 DF,  p-value: 0.0007774

ib2015.dinotefuran.gls <- gls(logdinotefuran ~ i122,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(ib2015.dinotefuran.gls)

# 12-2-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logdinotefuran ~ i122 
  Data: test 
        AIC       BIC   logLik
  -5.434892 -4.645993 6.717446

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.4429836 

Coefficients:
                Value  Std.Error   t-value p-value
(Intercept) -1.409238 0.03454562 -40.79354   0e+00
i122         0.873972 0.13198485   6.62176   3e-04

 Correlation: 
     (Intr)
i122 -0.456

Standardized residuals:
        Min          Q1         Med          Q3         Max 
-1.01546495 -1.01546495  0.08937826  0.08937826  2.47077737 

Residual standard error: 0.1264089 
Degrees of freedom: 9 total; 7 residual

ib2015.acetamiprid.lm <- lm(logacetamiprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(ib2015.acetamiprid.lm)

# 12-2-19 there are significant mean differences between corn planting and non-corn planting

Call:
lm(formula = logacetamiprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         5         15         25         35         45         55 
 1.388e-17 -3.469e-18  1.657e-18  1.657e-18  1.192e-01 -9.491e-04 
        65         75         85 
-1.677e-01  2.171e-01 -1.677e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.16020    0.07668 -15.130 0.000111 ***
i119         0.84505    0.18783   4.499 0.010831 *  
i122         0.90761    0.18783   4.832 0.008449 ** 
i125         0.11924    0.18783   0.635 0.560029    
i128         1.03700    0.18783   5.521 0.005256 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1715 on 4 degrees of freedom
Multiple R-squared:  0.9348,    Adjusted R-squared:  0.8697 
F-statistic: 14.35 on 4 and 4 DF,  p-value: 0.01218

ib2015.acetamiprid.lm <- lm(logacetamiprid ~ i119+i122+i128,data=test,na.action="na.omit")
summary(ib2015.acetamiprid.lm)

ib2015.acetamiprid.gls <- gls(logacetamiprid ~ i119+i122+i128,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(ib2015.acetamiprid.gls)

# 12-2-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logacetamiprid ~ i119 + i122 + i128 
  Data: test 
        AIC       BIC   logLik
  -8.211739 -7.028391 10.10587

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.8332603 

Coefficients:
                 Value  Std.Error   t-value p-value
(Intercept) -1.1543631 0.02661576 -43.37141   0e+00
i119         0.7604692 0.12897582   5.89622   2e-03
i122         0.9962704 0.11027099   9.03475   3e-04
i128         1.1427017 0.09251401  12.35166   1e-04

 Correlation: 
     (Intr) i119   i122  
i119 -0.063              
i122 -0.442 -0.545       
i128 -0.571  0.036  0.253

Standardized residuals:
        Min          Q1         Med          Q3         Max 
-1.30172908 -0.83669706 -0.05091583  0.85065546  1.58473646 

Residual standard error: 0.1333143 
Degrees of freedom: 9 total; 5 residual

# end ib2015 ----------------------------------------------------------------

# 12-2-19 BG has 22% corn adjacent ------------------------------------------------------------------------------------------- 

test <- pesticide2015new[pesticide2015new$site=="BG",]
test$logclothianidin <- log10(test$clothianidin)
test$logthiamethoxam <- log10(test$thiamethoxam)
test$lognitenpyram <- log10(test$nitenpyram)
test$logimidacloprid <- log10(test$imidacloprid)
test$logthiacloprid <- log10(test$thiacloprid)
test$logdinotefuran <- log10(test$dinotefuran)
test$logacetamiprid <- log10(test$acetamiprid)

n <- dim(test)
test$cornplt <- rep(0,n[1])
test$cornplt[test$julian>118&test$julian<123] <- 1
test$cornplt2 <- rep(0,n[1])
test$cornplt2[test$julian>123&test$julian<132] <- 1
test$cornplt3 <- rep(0,n[1])
test$cornplt3[test$julian>120&test$julian<128] <- 1
test$i122 <- rep(0,n[1])
test$i122[test$julian==122] <- 1
test$i125 <- rep(0,n[1])
test$i125[test$julian==125] <- 1
test$i128 <- rep(0,n[1])
test$i128[test$julian==128] <- 1
test$i119 <- rep(0,n[1])
test$i119[test$julian==119] <- 1
test$i147 <- rep(0,n[1])
test$i147[test$julian==147] <- 1
test$i131 <- rep(0,n[1])
test$i131[test$julian==131] <- 1
test$intercept <- rep(1,n[1])

png(file="C:\\Users\\hlee05\\Documents\\references\\dendrochronology\\pesticide_2015_BG.png",width=12,height=12,units="in",res=700)
par(mfrow=c(1,1),mai=c(2,2,1,1),oma=c(1,1,1,1),cex=1.3,mex=1.4,lwd=4,cex.lab=1.4,cex.axis=1.4)
plot(test$julian,log10(test$clothianidin),type="b",pch=16,ylim=c(-2,1.5),ylab="Pesticide concentration in pollen sample (log ng/g)",xlab="Julian day for 2015")
points(test$julian,log10(test$thiamethoxam),type="b",pch=16,col="red")
points(test$julian,log10(test$thiacloprid),type="b",pch=16,col="orange")
points(test$julian,log10(test$dinotefuran),type="b",pch=16,col="blue")
points(test$julian,log10(test$nitenpyram),type="b",pch=16,col="green")
points(test$julian,log10(test$imidacloprid),type="b",pch=16,col="purple")
points(test$julian,log10(test$acetamiprid),type="b",pch=16,col="cyan")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)
text(125,-1.8,"Corn planting",adj=0.5,cex=1.2)

legend(135,1.5,legend=c("clothianidin","thiamethoxam","thiacloprid","dinotefuran","nitenpyram","imidacloprid","acetamiprid"),pch=rep(16,7),col=c("black","red","orange","blue","green","purple","cyan"),pt.cex=rep(1.2,7),bty="n")

# 12-2-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

bg2015.clothianidin.lm <- lm(logclothianidin ~ i119+i122+i125+i128+i131,data=test,na.action="na.omit")
summary(bg2015.clothianidin.lm)

Call:
lm(formula = logclothianidin ~ i119 + i122 + i125 + i128 + i131, 
    data = test, na.action = "na.omit")

Residuals:
         1         11         21         31         41         51 
 0.000e+00  0.000e+00  1.388e-17  3.807e-18  1.768e-17 -9.667e-02 
        61         71         81 
 2.900e-01 -9.667e-02 -9.667e-02 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  0.06887    0.09667   0.712   0.5276  
i119         0.51353    0.21616   2.376   0.0980 .
i122         0.55921    0.21616   2.587   0.0813 .
i125         1.00316    0.21616   4.641   0.0189 *
i128         1.06556    0.21616   4.929   0.0160 *
i131         1.08341    0.21616   5.012   0.0153 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1933 on 3 degrees of freedom
Multiple R-squared:  0.9445,    Adjusted R-squared:  0.852 
F-statistic: 10.21 on 5 and 3 DF,  p-value: 0.04219

bg2015.clothianidin.lm <- lm(logclothianidin ~ cornplt+cornplt2,data=test,na.action="na.omit")
summary(bg2015.clothianidin.lm)

# 12-2-19 final model mean for corn planting period (julian=119-122) and corn planting period 2 (julian=123-131)

Call:
lm(formula = logclothianidin ~ cornplt + cornplt2, data = test, 
    na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.09667 -0.09667 -0.02284  0.02284  0.29001 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.06887    0.06974   0.988  0.36151    
cornplt      0.53637    0.12080   4.440  0.00437 ** 
cornplt2     1.05071    0.10653   9.863 6.27e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1395 on 6 degrees of freedom
Multiple R-squared:  0.9422,    Adjusted R-squared:  0.923 
F-statistic: 48.93 on 2 and 6 DF,  p-value: 0.0001928


bg2015.clothianidin.gls <- gls(logclothianidin ~ cornplt+cornplt2,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(bg2015.clothianidin.gls)

# 12-2-19 since 1st order autoregressive coef <0, ignore autocorrelation, use ordinary least squares above

Generalized least squares fit by maximum likelihood
  Model: logclothianidin ~ cornplt + cornplt2 
  Data: test 
        AIC      BIC   logLik
  -5.730353 -4.74423 7.865176

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.4707379 

Coefficients:
                Value  Std.Error   t-value p-value
(Intercept) 0.0863256 0.04468779  1.931749  0.1016
cornplt     0.5143481 0.08190939  6.279477  0.0008
cornplt2    1.0271597 0.07120798 14.424784  0.0000

 Correlation: 
         (Intr) crnplt
cornplt  -0.539       
cornplt2 -0.681  0.298

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.0110909 -1.0110909 -0.1618608  0.2428312  2.4147994 

Residual standard error: 0.1128709 
Degrees of freedom: 9 total; 6 residual

bg2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i119+i122+i125+i128+i131,data=test,na.action="na.omit")
summary(bg2015.thiamethoxam.lm)

# 12-2-19 no significant mean differences between corn planting and non-corn planting periods

Call:
lm(formula = logthiamethoxam ~ i119 + i122 + i125 + i128 + i131, 
    data = test, na.action = "na.omit")

Residuals:
         1         11         21         31         41         51 
 0.000e+00 -2.776e-17 -7.250e-18 -3.501e-17  1.246e-17 -1.150e+00 
        61         71         81 
 7.123e-01 -8.720e-02  5.251e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept) -0.09392    0.41969  -0.224    0.837
i119        -0.60073    0.93845  -0.640    0.568
i122         0.57779    0.93845   0.616    0.582
i125         1.47502    0.93845   1.572    0.214
i128         1.00609    0.93845   1.072    0.362
i131         1.16210    0.93845   1.238    0.304

Residual standard error: 0.8394 on 3 degrees of freedom
Multiple R-squared:  0.6412,    Adjusted R-squared:  0.04313 
F-statistic: 1.072 on 5 and 3 DF,  p-value: 0.51


bg2015.thiamethoxam.gls <- gls(logthiamethoxam ~ i125,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(bg2015.thiamethoxam.gls)

# 12-2-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logthiamethoxam ~ i125 
  Data: test 
       AIC      BIC    logLik
  25.45031 26.23921 -8.725156

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.4971536 

Coefficients:
                Value Std.Error   t-value p-value
(Intercept) 0.1414496 0.1859978 0.7604903  0.4718
i125        1.6833771 0.7199267 2.3382617  0.0520

 Correlation: 
     (Intr)
i125 -0.464

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.9144146 -0.6130904  0.4003451  0.6589381  1.2804488 

Residual standard error: 0.723759 
Degrees of freedom: 9 total; 7 residual

bg2015.nitenpyram.lm <- lm(lognitenpyram ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(bg2015.nitenpyram.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = lognitenpyram ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         1         11         21         31         41         51 
 5.551e-17 -4.163e-17  2.713e-17 -4.852e-17  2.095e-01 -4.595e-01 
        61         71         81 
-6.426e-01  4.811e-01  4.115e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.5720     0.2312  -2.475   0.0686 .
i119          0.3210     0.5662   0.567   0.6011  
i122          0.2095     0.5662   0.370   0.7301  
i125          0.6883     0.5662   1.216   0.2910  
i128          0.2095     0.5662   0.370   0.7301  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5169 on 4 degrees of freedom
Multiple R-squared:  0.2907,    Adjusted R-squared:  -0.4187 
F-statistic: 0.4098 on 4 and 4 DF,  p-value: 0.7957


bg2015.nitenpyram.gls <- gls(lognitenpyram ~ intercept-1,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(bg2015.nitenpyram.gls)

# 12-3-19 some autocorrelation; 

Generalized least squares fit by maximum likelihood
  Model: lognitenpyram ~ intercept - 1 
  Data: test 
       AIC     BIC    logLik
  14.96853 15.5602 -4.484264

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
      Phi 
0.2247591 

Coefficients:
               Value Std.Error   t-value p-value
intercept -0.4007686 0.1755337 -2.283144  0.0518

Standardized residuals:
        Min          Q1         Med          Q3         Max 
-1.99717228  0.09387931  0.09387931  0.58952337  1.26873621 

Residual standard error: 0.407527 
Degrees of freedom: 9 total; 8 residual

bg2015.imidacloprid.lm <- lm(logimidacloprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(bg2015.imidacloprid.lm)

# 12-3-19 some mean differences between corn planting and non-corn planting

Call:
lm(formula = logimidacloprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         1         11         21         31         41         51 
-1.388e-17 -3.469e-18 -3.469e-18 -4.510e-17  1.872e-01 -1.330e-01 
        61         71         81 
 4.921e-02  1.526e-01 -2.559e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)  0.43750    0.08483   5.157  0.00671 **
i119        -1.26724    0.20780  -6.098  0.00366 **
i122        -0.05639    0.20780  -0.271  0.79954   
i125         0.54891    0.20780   2.642  0.05749 . 
i128        -0.19871    0.20780  -0.956  0.39311   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1897 on 4 degrees of freedom
Multiple R-squared:  0.9276,    Adjusted R-squared:  0.8552 
F-statistic: 12.82 on 4 and 4 DF,  p-value: 0.01496

bg2015.imidacloprid.lm <- lm(logimidacloprid ~ i119+i125,data=test,na.action="na.omit")
summary(bg2015.imidacloprid.lm)

Call:
lm(formula = logimidacloprid ~ i119 + i125, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.21950 -0.09657  0.00000  0.08565  0.22363 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.40106    0.06497   6.173 0.000831 ***
i119        -1.23080    0.18377  -6.697 0.000538 ***
i125         0.58535    0.18377   3.185 0.018951 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1719 on 6 degrees of freedom
Multiple R-squared:  0.9108,    Adjusted R-squared:  0.8811 
F-statistic: 30.65 on 2 and 6 DF,  p-value: 0.0007089

bg2015.imidacloprid.gls <- gls(logimidacloprid ~ i119+i125,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(bg2015.imidacloprid.gls)

# 12-3-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logimidacloprid ~ i119 + i125 
  Data: test 
        AIC       BIC   logLik
  -6.391426 -5.405303 8.195713

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.8301986 

Coefficients:
                 Value Std.Error    t-value p-value
(Intercept)  0.4398431 0.0270038  16.288192  0.0000
i119        -1.3183373 0.1221626 -10.791661  0.0000
i125         0.4189010 0.1012714   4.136419  0.0061

 Correlation: 
     (Intr) i119  
i119 -0.405       
i125 -0.529  0.214

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.5784764 -0.8271892  0.2864440  0.7802340  1.1296951 

Residual standard error: 0.1636295 
Degrees of freedom: 9 total; 6 residual

bg2015.thiacloprid.lm <- lm(logthiacloprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(bg2015.thiacloprid.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logthiacloprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         1         11         21         31         41         51 
-6.939e-17 -3.123e-17  6.783e-18  1.440e-17  6.419e-01 -8.675e-01 
        61         71         81 
-7.844e-03  4.145e-01 -1.811e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept) -0.50928    0.26165  -1.946    0.123
i119         0.51102    0.64090   0.797    0.470
i122         0.11888    0.64090   0.185    0.862
i125         0.76672    0.64090   1.196    0.298
i128        -0.02237    0.64090  -0.035    0.974

Residual standard error: 0.5851 on 4 degrees of freedom
Multiple R-squared:  0.3232,    Adjusted R-squared:  -0.3536 
F-statistic: 0.4775 on 4 and 4 DF,  p-value: 0.7542

bg2015.dinotefuran.lm <- lm(logdinotefuran ~ i119+i122+i125+i128+i147,data=test,na.action="na.omit")
summary(bg2015.dinotefuran.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting except julian 147 is significantly higher

Call:
lm(formula = logdinotefuran ~ i119 + i122 + i125 + i128 + i147, 
    data = test, na.action = "na.omit")

Residuals:
         1         11         21         31         41         51 
-2.301e-17 -1.828e-17  4.867e-18 -5.620e-17 -2.315e-01 -3.712e-01 
        61         71         81 
 9.739e-01 -3.712e-01  1.049e-16 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -1.1664     0.3263  -3.575   0.0374 *
i119         -0.2315     0.7297  -0.317   0.7718  
i122          0.2383     0.7297   0.327   0.7654  
i125         -0.2315     0.7297  -0.317   0.7718  
i128         -0.2315     0.7297  -0.317   0.7718  
i147          0.7105     0.7297   0.974   0.4020  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.6526 on 3 degrees of freedom
Multiple R-squared:  0.3589,    Adjusted R-squared:  -0.7097 
F-statistic: 0.3358 on 5 and 3 DF,  p-value: 0.8646

bg2015.dinotefuran.gls <- gls(logdinotefuran ~ intercept-1,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(bg2015.dinotefuran.gls)

# 12-2-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logdinotefuran ~ intercept - 1 
  Data: test 
       AIC      BIC    logLik
  15.40575 15.99742 -4.702873

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
     Phi 
-0.53988 

Coefficients:
              Value  Std.Error   t-value p-value
intercept -1.156012 0.09571234 -12.07798       0

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-0.8023982 -0.5087205 -0.5087205  0.4792094  2.0261219 

Residual standard error: 0.4755621 
Degrees of freedom: 9 total; 8 residual

bg2015.acetamiprid.lm <- lm(logacetamiprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(bg2015.acetamiprid.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logacetamiprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         1         11         21         31         41         51 
-4.163e-17  3.123e-17  1.434e-17  1.815e-17  6.265e-02 -8.473e-01 
        61         71         81 
 4.472e-01  2.194e-01  1.181e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.4806     0.2218  -2.167   0.0962 .
i119         -0.5604     0.5433  -1.031   0.3606  
i122          0.2138     0.5433   0.393   0.7140  
i125          0.6670     0.5433   1.228   0.2869  
i128         -0.4550     0.5433  -0.837   0.4495  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.496 on 4 degrees of freedom
Multiple R-squared:  0.5064,    Adjusted R-squared:  0.01286 
F-statistic: 1.026 on 4 and 4 DF,  p-value: 0.4904

# end bg2015 ----------------------------------------------------------------

# 12-2-19 WB has 30% corn adjacent ------------------------------------------------------------------------------------------- 

test <- pesticide2015new[pesticide2015new$site=="WB",]
test$logclothianidin <- log10(test$clothianidin)
test$logthiamethoxam <- log10(test$thiamethoxam)
test$lognitenpyram <- log10(test$nitenpyram)
test$logimidacloprid <- log10(test$imidacloprid)
test$logthiacloprid <- log10(test$thiacloprid)
test$logdinotefuran <- log10(test$dinotefuran)
test$logacetamiprid <- log10(test$acetamiprid)

n <- dim(test)
test$cornplt <- rep(0,n[1])
test$cornplt[test$julian>119&test$julian<132] <- 1
test$cornplt2 <- rep(0,n[1])
test$cornplt2[test$julian>124&test$julian<129] <- 1
test$cornplt3 <- rep(0,n[1])
test$cornplt3[test$julian>120&test$julian<128] <- 1
test$i122 <- rep(0,n[1])
test$i122[test$julian==122] <- 1
test$i125 <- rep(0,n[1])
test$i125[test$julian==125] <- 1
test$i128 <- rep(0,n[1])
test$i128[test$julian==128] <- 1
test$i120 <- rep(0,n[1])
test$i120[test$julian==120] <- 1
test$i147 <- rep(0,n[1])
test$i147[test$julian==147] <- 1
test$i131 <- rep(0,n[1])
test$i131[test$julian==131] <- 1
test$intercept <- rep(1,n[1])

png(file="C:\\Users\\hlee05\\Documents\\references\\dendrochronology\\pesticide_2015_WB.png",width=12,height=12,units="in",res=700)
par(mfrow=c(1,1),mai=c(2,2,1,1),oma=c(1,1,1,1),cex=1.3,mex=1.4,lwd=4,cex.lab=1.4,cex.axis=1.4)
plot(test$julian,log10(test$clothianidin),type="b",pch=16,ylim=c(-2,1.5),ylab="Pesticide concentration in pollen sample (log ng/g)",xlab="Julian day for 2015")
points(test$julian,log10(test$thiamethoxam),type="b",pch=16,col="red")
points(test$julian,log10(test$thiacloprid),type="b",pch=16,col="orange")
points(test$julian,log10(test$dinotefuran),type="b",pch=16,col="blue")
points(test$julian,log10(test$nitenpyram),type="b",pch=16,col="green")
points(test$julian,log10(test$imidacloprid),type="b",pch=16,col="purple")
points(test$julian,log10(test$acetamiprid),type="b",pch=16,col="cyan")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)
text(125,-1.8,"Corn planting",adj=0.5,cex=1.2)

legend(135,1.5,legend=c("clothianidin","thiamethoxam","thiacloprid","dinotefuran","nitenpyram","imidacloprid","acetamiprid"),pch=rep(16,7),col=c("black","red","orange","blue","green","purple","cyan"),pt.cex=rep(1.2,7),bty="n")

# 12-3-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

wb2015.clothianidin.lm <- lm(logclothianidin ~ i120+i122+i125+i128+i131,data=test,na.action="na.omit")
summary(wb2015.clothianidin.lm)

Call:
lm(formula = logclothianidin ~ i120 + i122 + i125 + i128 + i131, 
    data = test, na.action = "na.omit")

Residuals:
        10         20         30         40         50         60 
-2.082e-17 -5.204e-18  2.255e-17  2.409e-18 -1.147e-17  1.001e-01 
        70         80         90 
-1.852e-01  2.703e-01 -1.852e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)   0.1574     0.1124   1.400   0.2560  
i120          0.6454     0.2514   2.568   0.0827 .
i122          0.7906     0.2514   3.145   0.0514 .
i125          1.1350     0.2514   4.515   0.0203 *
i128          0.5759     0.2514   2.291   0.1058  
i131          0.8314     0.2514   3.307   0.0455 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2248 on 3 degrees of freedom
Multiple R-squared:  0.9131,    Adjusted R-squared:  0.7684 
F-statistic: 6.308 on 5 and 3 DF,  p-value: 0.08023

wb2015.clothianidin.lm <- lm(logclothianidin ~ cornplt,data=test,na.action="na.omit")
summary(wb2015.clothianidin.lm)

# 12-3-19 final model mean for corn planting period 2 (julian=121-128)

Call:
lm(formula = logclothianidin ~ cornplt, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.21976 -0.18517 -0.00501  0.10007  0.33933 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)   0.1574     0.1100    1.43  0.19570   
cornplt       0.7957     0.1476    5.39  0.00102 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2201 on 7 degrees of freedom
Multiple R-squared:  0.8058,    Adjusted R-squared:  0.7781 
F-statistic: 29.05 on 1 and 7 DF,  p-value: 0.001019

wb2015.clothianidin.gls <- gls(logclothianidin ~ cornplt,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(wb2015.clothianidin.gls)

# 12-3-19 test for 1st order autoregressive model not significant

Generalized least squares fit by maximum likelihood
  Model: logclothianidin ~ cornplt 
  Data: test 
         AIC       BIC   logLik
  -0.4251724 0.3637259 4.212586

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.6497239 

Coefficients:
                Value  Std.Error   t-value p-value
(Intercept) 0.1733185 0.05515784  3.142229  0.0163
cornplt     0.7959135 0.07563576 10.522978  0.0000

 Correlation: 
        (Intr)
cornplt -0.769

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.2203558 -1.0401699 -0.1097004  0.4350685  1.6712526 

Residual standard error: 0.1933489 
Degrees of freedom: 9 total; 7 residual


# 12-3-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

wb2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i120+i122+i125+i128+i131,data=test,na.action="na.omit")
summary(wb2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ i120 + i122 + i125 + i128 + i131, 
    data = test, na.action = "na.omit")

Residuals:
        10         20         30         40         50         60 
 0.000e+00 -1.388e-17 -1.750e-17  2.032e-17  2.837e-17  1.901e-02 
        70         80         90 
 1.426e-01  1.552e-01 -3.168e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)   0.3869     0.1100   3.517   0.0390 *
i120          0.4892     0.2459   1.989   0.1408  
i122          0.1690     0.2459   0.687   0.5414  
i125          0.6175     0.2459   2.511   0.0869 .
i128          0.9241     0.2459   3.758   0.0329 *
i131          0.3364     0.2459   1.368   0.2649  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.22 on 3 degrees of freedom
Multiple R-squared:  0.8613,    Adjusted R-squared:  0.6302 
F-statistic: 3.727 on 5 and 3 DF,  p-value: 0.154

# 12-3-19 final model mean for corn planting period 3 (julian=121-125)

wb2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i128,data=test,na.action="na.omit")
summary(wb2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ i128, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.51832 -0.05892 -0.03254  0.13485  0.41601 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.5884     0.1020   5.768 0.000686 ***
i128          0.7226     0.3060   2.361 0.050240 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2885 on 7 degrees of freedom
Multiple R-squared:  0.4434,    Adjusted R-squared:  0.3639 
F-statistic: 5.576 on 1 and 7 DF,  p-value: 0.05024


wb2015.thiamethoxam.gls <- gls(logthiamethoxam ~ i128,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(wb2015.thiamethoxam.gls)

# 12-2-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logthiamethoxam ~ i128 
  Data: test 
      AIC      BIC    logLik
  8.79773 9.586628 -0.398865

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.3431973 

Coefficients:
                Value  Std.Error  t-value p-value
(Intercept) 0.5723252 0.08086956 7.077139  0.0002
i128        0.9175999 0.29934620 3.065347  0.0182

 Correlation: 
     (Intr)
i128 -0.436

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.8782490 -0.6224844 -0.1131054  0.5642149  1.6155639 

Residual standard error: 0.2674232 
Degrees of freedom: 9 total; 7 residual

wb2015.nitenpyram.lm <- lm(lognitenpyram ~ i120+i122+i125+i128,data=test,na.action="na.omit")
summary(wb2015.nitenpyram.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = lognitenpyram ~ i120 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
        10         20         30         40         50         60 
-5.551e-17  4.163e-17  5.613e-17  1.585e-17  9.461e-02  3.362e-01 
        70         80         90 
-7.576e-01  1.084e+00 -7.576e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept) -0.45712    0.34967  -1.307    0.261
i120         0.09461    0.85652   0.110    0.917
i122         0.09461    0.85652   0.110    0.917
i125         0.09461    0.85652   0.110    0.917
i128         0.09461    0.85652   0.110    0.917

Residual standard error: 0.7819 on 4 degrees of freedom
Multiple R-squared:  0.008068,  Adjusted R-squared:  -0.9839 
F-statistic: 0.008134 on 4 and 4 DF,  p-value: 0.9998

wb2015.imidacloprid.lm <- lm(logimidacloprid ~ i120+i122+i125+i128,data=test,na.action="na.omit")
summary(wb2015.imidacloprid.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logimidacloprid ~ i120 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
        10         20         30         40         50         60 
-1.388e-17 -6.939e-18  6.033e-18 -1.193e-17  1.772e-01 -5.220e-02 
        70         80         90 
-2.951e-02 -4.071e-02 -5.475e-02 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)   
(Intercept)  0.364165   0.044518   8.180  0.00122 **
i120         0.435933   0.109047   3.998  0.01616 * 
i122         0.008194   0.109047   0.075  0.94371   
i125         0.262893   0.109047   2.411  0.07349 . 
i128        -0.220214   0.109047  -2.019  0.11357   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.09955 on 4 degrees of freedom
Multiple R-squared:  0.8765,    Adjusted R-squared:  0.7531 
F-statistic: 7.099 on 4 and 4 DF,  p-value: 0.04197

wb2015.imidacloprid.lm <- lm(logimidacloprid ~ i120,data=test,na.action="na.omit")
summary(wb2015.imidacloprid.lm)

Call:
lm(formula = logimidacloprid ~ i120, data = test, na.action = "na.omit")

Residuals:
      Min        1Q    Median        3Q       Max 
-0.226573 -0.058559 -0.035870  0.001835  0.256534 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.37052    0.05295   6.998 0.000212 ***
i120         0.42957    0.15884   2.704 0.030443 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1498 on 7 degrees of freedom
Multiple R-squared:  0.511,     Adjusted R-squared:  0.4411 
F-statistic: 7.314 on 1 and 7 DF,  p-value: 0.03044

wb2015.imidacloprid.gls <- gls(logimidacloprid ~ i120,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(wb2015.imidacloprid.gls)

# 12-3-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logimidacloprid ~ i120 
  Data: test 
        AIC       BIC   logLik
  -7.228147 -6.439248 7.614073

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.5914282 

Coefficients:
                Value  Std.Error   t-value p-value
(Intercept) 0.3735598 0.02681483 13.931092  0.0000
i120        0.4258286 0.12262136  3.472711  0.0104

 Correlation: 
     (Intr)
i120 -0.348

Standardized residuals:
         Min           Q1          Med           Q3          Max 
-1.826230839 -0.489899180 -0.309437592  0.005645809  2.016243916 

Residual standard error: 0.1257282 
Degrees of freedom: 9 total; 7 residual

wb2015.thiacloprid.lm <- lm(logthiacloprid ~ i120+i122+i125+i128,data=test,na.action="na.omit")
summary(wb2015.thiacloprid.lm)

# 12-3-19 some mean differences between corn planting and non-corn planting

Call:
lm(formula = logthiacloprid ~ i120 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
        10         20         30         40         50         60 
 0.000e+00  3.469e-18  9.062e-19 -2.563e-18  1.546e-01 -2.374e-01 
        70         80         90 
 1.185e-02  2.919e-03  6.800e-02 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept) -0.36643    0.06520  -5.620  0.00493 **
i120         0.59019    0.15970   3.695  0.02092 * 
i122         0.25123    0.15970   1.573  0.19081   
i125         0.45492    0.15970   2.849  0.04647 * 
i128        -0.04474    0.15970  -0.280  0.79327   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1458 on 4 degrees of freedom
Multiple R-squared:   0.84,     Adjusted R-squared:   0.68 
F-statistic:  5.25 on 4 and 4 DF,  p-value: 0.06861

wb2015.thiacloprid.lm <- lm(logthiacloprid ~ i120+i125,data=test,na.action="na.omit")
summary(wb2015.thiacloprid.lm)

Call:
lm(formula = logthiacloprid ~ i120 + i125, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.26687 -0.02658  0.00000  0.03850  0.22173 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept) -0.33693    0.05856  -5.753   0.0012 **
i120         0.56069    0.16564   3.385   0.0148 * 
i125         0.42542    0.16564   2.568   0.0424 * 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1549 on 6 degrees of freedom
Multiple R-squared:  0.7289,    Adjusted R-squared:  0.6385 
F-statistic: 8.067 on 2 and 6 DF,  p-value: 0.01992

wb2015.thiacloprid.gls <- gls(logthiacloprid ~ i120+i125,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(wb2015.thiacloprid.gls)

Generalized least squares fit by maximum likelihood
  Model: logthiacloprid ~ i120 + i125 
  Data: test 
        AIC       BIC   logLik
  -3.406156 -2.420033 6.703078

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.5924638 

Coefficients:
                 Value  Std.Error   t-value p-value
(Intercept) -0.3669567 0.03719685 -9.865262  0.0001
i120         0.7398661 0.14960529  4.945454  0.0026
i125         0.5464605 0.13727519  3.980767  0.0073

 Correlation: 
     (Intr) i120  
i120 -0.396       
i125 -0.509  0.201

Standardized residuals:
        Min          Q1         Med          Q3         Max 
-1.70100417 -0.65365416  0.02473889  0.49214157  1.80807351 

Residual standard error: 0.1392377 
Degrees of freedom: 9 total; 6 residual

wb2015.dinotefuran.lm <- lm(logdinotefuran ~ i120+i122+i125+i128,data=test,na.action="na.omit")
summary(wb2015.dinotefuran.lm)

# 12-3-19 some mean differences between corn planting and non-corn planting 

Call:
lm(formula = logdinotefuran ~ i120 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
        10         20         30         40         50         60 
 6.939e-18 -5.204e-18  6.861e-18 -8.583e-18  1.117e-01 -2.793e-02 
        70         80         90 
-2.793e-02 -2.793e-02 -2.793e-02 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.50967    0.02793 -54.047 7.02e-07 ***
i120         1.47581    0.06842  21.570 2.73e-05 ***
i122         1.07904    0.06842  15.771 9.44e-05 ***
i125         0.11173    0.06842   1.633    0.178    
i128         0.11173    0.06842   1.633    0.178    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.06246 on 4 degrees of freedom
Multiple R-squared:  0.9938,    Adjusted R-squared:  0.9876 
F-statistic: 160.8 on 4 and 4 DF,  p-value: 0.0001141

wb2015.dinotefuran.lm <- lm(logdinotefuran ~ i120+i122,data=test,na.action="na.omit")
summary(wb2015.dinotefuran.lm)

Call:
lm(formula = logdinotefuran ~ i120 + i122, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.05985 -0.05985  0.00000  0.07981  0.07981 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.47775    0.02822  -52.37 3.25e-09 ***
i120         1.44389    0.07981   18.09 1.83e-06 ***
i122         1.04712    0.07981   13.12 1.21e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.07465 on 6 degrees of freedom
Multiple R-squared:  0.9868,    Adjusted R-squared:  0.9823 
F-statistic: 223.6 on 2 and 6 DF,  p-value: 2.322e-06

wb2015.dinotefuran.gls <- gls(logdinotefuran ~ i120+i122,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(wb2015.dinotefuran.gls)

# 12-3-19 since AR1 coef >0, use this ar1 model

Generalized least squares fit by maximum likelihood
  Model: logdinotefuran ~ i120 + i122 
  Data: test 
        AIC       BIC   logLik
  -19.36107 -18.37494 14.68053

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
      Phi 
0.6830219 

Coefficients:
                Value  Std.Error   t-value p-value
(Intercept) -1.473945 0.05253969 -28.05394       0
i120         1.404629 0.07338752  19.13989       0
i122         0.991406 0.05843035  16.96731       0

 Correlation: 
     (Intr) i120  
i120 -0.382       
i122 -0.285  0.608

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.0167350 -1.0167350  0.5663426  1.2139758  1.2139758 

Residual standard error: 0.06260874 
Degrees of freedom: 9 total; 6 residual

wb2015.acetamiprid.lm <- lm(logacetamiprid ~ i120+i122+i125+i128,data=test,na.action="na.omit")
summary(wb2015.acetamiprid.lm)

# 12-3-19 some mean differences between corn planting and non-corn planting

Call:
lm(formula = logacetamiprid ~ i120 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
        10         20         30         40         50         60 
 2.776e-17  6.939e-18  3.469e-17 -4.096e-17  1.503e-01  1.776e-01 
        70         80         90 
-2.651e-01 -5.121e-01  4.493e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)  -0.8158     0.1715  -4.756  0.00893 **
i120          0.9693     0.4202   2.307  0.08230 . 
i122          0.4349     0.4202   1.035  0.35908   
i125          0.6774     0.4202   1.612  0.18223   
i128          0.2645     0.4202   0.630  0.56312   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3836 on 4 degrees of freedom
Multiple R-squared:   0.64,     Adjusted R-squared:  0.2799 
F-statistic: 1.777 on 4 and 4 DF,  p-value: 0.2955

# end wb2015 ----------------------------------------------------------------

# 12-3-19 HR has 30% corn adjacent ------------------------------------------------------------------------------------------- 

test <- pesticide2015new[pesticide2015new$site=="HR",]
test$logclothianidin <- log10(test$clothianidin)
test$logthiamethoxam <- log10(test$thiamethoxam)
test$lognitenpyram <- log10(test$nitenpyram)
test$logimidacloprid <- log10(test$imidacloprid)
test$logthiacloprid <- log10(test$thiacloprid)
test$logdinotefuran <- log10(test$dinotefuran)
test$logacetamiprid <- log10(test$acetamiprid)

n <- dim(test)
test$cornplt <- rep(0,n[1])
test$cornplt[test$julian>124&test$julian<129] <- 1
test$cornplt2 <- rep(0,n[1])
test$cornplt2[test$julian>120&test$julian<129] <- 1
test$cornplt3 <- rep(0,n[1])
test$cornplt3[test$julian>120&test$julian<128] <- 1
test$i122 <- rep(0,n[1])
test$i122[test$julian==122] <- 1
test$i125 <- rep(0,n[1])
test$i125[test$julian==125] <- 1
test$i128 <- rep(0,n[1])
test$i128[test$julian==128] <- 1
test$i119 <- rep(0,n[1])
test$i119[test$julian==119] <- 1
test$i147 <- rep(0,n[1])
test$i147[test$julian==147] <- 1
test$i131 <- rep(0,n[1])
test$i131[test$julian==131] <- 1
test$intercept <- rep(1,n[1])

png(file="C:\\Users\\hlee05\\Documents\\references\\dendrochronology\\pesticide_2015_HR.png",width=12,height=12,units="in",res=700)
par(mfrow=c(1,1),mai=c(2,2,1,1),oma=c(1,1,1,1),cex=1.3,mex=1.4,lwd=4,cex.lab=1.4,cex.axis=1.4)
plot(test$julian,log10(test$clothianidin),type="b",pch=16,ylim=c(-2,1.5),ylab="Pesticide concentration in pollen sample (log ng/g)",xlab="Julian day for 2015")
points(test$julian,log10(test$thiamethoxam),type="b",pch=16,col="red")
points(test$julian,log10(test$thiacloprid),type="b",pch=16,col="orange")
points(test$julian,log10(test$dinotefuran),type="b",pch=16,col="blue")
points(test$julian,log10(test$nitenpyram),type="b",pch=16,col="green")
points(test$julian,log10(test$imidacloprid),type="b",pch=16,col="purple")
points(test$julian,log10(test$acetamiprid),type="b",pch=16,col="cyan")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)
text(125,-1.8,"Corn planting",adj=0.5,cex=1.2)

legend(135,1.5,legend=c("clothianidin","thiamethoxam","thiacloprid","dinotefuran","nitenpyram","imidacloprid","acetamiprid"),pch=rep(16,7),col=c("black","red","orange","blue","green","purple","cyan"),pt.cex=rep(1.2,7),bty="n")

# 12-3-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

hr2015.clothianidin.lm <- lm(logclothianidin ~ i119+i122+i125+i128+i131,data=test,na.action="na.omit")
summary(hr2015.clothianidin.lm)

Call:
lm(formula = logclothianidin ~ i119 + i122 + i125 + i128 + i131, 
    data = test, na.action = "na.omit")

Residuals:
         4         14         24         34         44         54 
-2.776e-17  2.082e-17  3.110e-19  3.110e-19 -5.520e-17 -1.826e-02 
        64         74         84 
-2.953e-01 -2.953e-01  6.088e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)   0.2675     0.2132   1.255   0.2984  
i119         -0.3542     0.4767  -0.743   0.5114  
i122          0.8368     0.4767   1.756   0.1774  
i125          0.9818     0.4767   2.060   0.1315  
i128          1.2743     0.4767   2.673   0.0755 .
i131          0.6764     0.4767   1.419   0.2509  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4263 on 3 degrees of freedom
Multiple R-squared:  0.8253,    Adjusted R-squared:  0.534 
F-statistic: 2.833 on 5 and 3 DF,  p-value: 0.2103

hr2015.clothianidin.lm <- lm(logclothianidin ~ cornplt,data=test,na.action="na.omit")
summary(hr2015.clothianidin.lm)

# 12-3-19 final model mean for corn planting period 2 (julian=125-128)

Call:
lm(formula = logclothianidin ~ cornplt, data = test, na.action = "na.omit")

Residuals:
    Min      1Q  Median      3Q     Max 
-0.5198 -0.4608 -0.1462  0.4432  0.6712 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)   0.4330     0.1851   2.339   0.0519 .
cornplt       0.9624     0.3927   2.451   0.0441 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4898 on 7 degrees of freedom
Multiple R-squared:  0.4618,    Adjusted R-squared:  0.3849 
F-statistic: 6.006 on 1 and 7 DF,  p-value: 0.04406

hr2015.clothianidin.gls <- gls(logclothianidin ~ cornplt,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(hr2015.clothianidin.gls)

# 12-3-19 test for 1st order autoregressive model not significant

Call:
lm(formula = logclothianidin ~ cornplt, data = test, na.action = "na.omit")

Residuals:
    Min      1Q  Median      3Q     Max 
-0.5198 -0.4608 -0.1462  0.4432  0.6712 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)   0.4330     0.1851   2.339   0.0519 .
cornplt       0.9624     0.3927   2.451   0.0441 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4898 on 7 degrees of freedom
Multiple R-squared:  0.4618,    Adjusted R-squared:  0.3849 
F-statistic: 6.006 on 1 and 7 DF,  p-value: 0.04406

# 12-3-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

hr2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(hr2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         4         14         24         34         44         54 
-5.551e-17  4.163e-17  6.219e-19 -5.489e-17  4.906e-01  1.865e-01 
        64         74         84 
-4.153e-02  4.958e-01 -1.131e+00 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  -0.1127     0.3003  -0.376    0.726
i119         -0.5819     0.7355  -0.791    0.473
i122         -0.5819     0.7355  -0.791    0.473
i125          0.6286     0.7355   0.855    0.441
i128          1.3000     0.7355   1.768    0.152

Residual standard error: 0.6714 on 4 degrees of freedom
Multiple R-squared:  0.5994,    Adjusted R-squared:  0.1987 
F-statistic: 1.496 on 4 and 4 DF,  p-value: 0.3529

# 12-3-19 final model mean for corn planting period 3 (julian=121-125)

hr2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i128+i147,data=test,na.action="na.omit")
summary(hr2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ i128 + i147, data = test, na.action = "na.omit")

Residuals:
    Min      1Q  Median      3Q     Max 
-0.6671 -0.1267  0.0000  0.4054  0.5434 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept) -0.02758    0.19177  -0.144   0.8904  
i128         1.21484    0.54242   2.240   0.0664 .
i147        -1.21655    0.54242  -2.243   0.0661 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5074 on 6 degrees of freedom
Multiple R-squared:  0.6568,    Adjusted R-squared:  0.5424 
F-statistic: 5.741 on 2 and 6 DF,  p-value: 0.04043

hr2015.thiamethoxam.gls <- gls(logthiamethoxam ~ i128+i147,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(hr2015.thiamethoxam.gls)

# 12-3-19 some AR1 coef >0, use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logthiamethoxam ~ i128 + i147 
  Data: test 
       AIC      BIC    logLik
  19.59031 20.57643 -4.795154

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
      Phi 
0.2135104 

Coefficients:
                 Value Std.Error    t-value p-value
(Intercept) -0.0119621 0.2270697 -0.0526805  0.9597
i128         1.0118434 0.5104768  1.9821536  0.0947
i147        -1.3165122 0.5343138 -2.4639307  0.0489

 Correlation: 
     (Intr) i128  
i128 -0.263       
i147 -0.334  0.088

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.6220439 -0.3381479  0.2035745  0.9261885  1.2541235 

Residual standard error: 0.4208804 
Degrees of freedom: 9 total; 6 residual


hr2015.nitenpyram.lm <- lm(lognitenpyram ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(hr2015.nitenpyram.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = lognitenpyram ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         4         14         24         34         44         54 
-2.776e-17  2.082e-17  3.110e-19  4.194e-17 -1.146e-01 -1.248e-01 
        64         74         84 
 2.496e-01  2.492e-01 -2.594e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.2479     0.1050  -2.361   0.0775 .
i119         -0.1057     0.2571  -0.411   0.7020  
i122          0.2413     0.2571   0.938   0.4011  
i125         -0.1146     0.2571  -0.446   0.6788  
i128         -0.1146     0.2571  -0.446   0.6788  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2347 on 4 degrees of freedom
Multiple R-squared:  0.3006,    Adjusted R-squared:  -0.3988 
F-statistic: 0.4298 on 4 and 4 DF,  p-value: 0.7832

hr2015.nitenpyram.gls <- gls(lognitenpyram ~ intercept-1,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(hr2015.nitenpyram.gls)

# 12-3-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: lognitenpyram ~ intercept - 1 
  Data: test 
       AIC      BIC   logLik
  1.165922 1.757595 2.417039

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.1637666 

Coefficients:
               Value  Std.Error   t-value p-value
intercept -0.2527244 0.05701129 -4.432883  0.0022

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.3593699 -0.5863682 -0.5863682  1.3147478  1.3590648 

Residual standard error: 0.1872303 
Degrees of freedom: 9 total; 8 residual

hr2015.imidacloprid.lm <- lm(logimidacloprid ~ i119+i122+i125+i128+julian,data=test,na.action="na.omit")
summary(hr2015.imidacloprid.lm)

# 12-2-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logimidacloprid ~ i119 + i122 + i125 + i128 + julian, 
    data = test, na.action = "na.omit")

Residuals:
         4         14         24         34         44         54 
 0.000e+00  0.000e+00 -1.388e-17 -1.768e-17  9.223e-02 -2.575e-01 
        64         74         84 
 7.907e-02  3.099e-01 -2.237e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept) -6.05159    2.94243  -2.057    0.132
i119        -0.41035    0.51648  -0.795    0.485
i122        -0.38614    0.46636  -0.828    0.468
i125         0.28493    0.41989   0.679    0.546
i128         0.39877    0.37841   1.054    0.369
julian       0.04733    0.02118   2.235    0.112

Residual standard error: 0.2752 on 3 degrees of freedom
Multiple R-squared:  0.9232,    Adjusted R-squared:  0.7952 
F-statistic: 7.213 on 5 and 3 DF,  p-value: 0.06733

hr2015.imidacloprid.lm <- lm(logimidacloprid ~ julian,data=test,na.action="na.omit")
summary(hr2015.imidacloprid.lm)

Call:
lm(formula = logimidacloprid ~ julian, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.32573 -0.29723  0.03814  0.23844  0.44185 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept) -7.07236    1.58853  -4.452  0.00296 **
julian       0.05497    0.01201   4.578  0.00255 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3253 on 7 degrees of freedom
Multiple R-squared:  0.7496,    Adjusted R-squared:  0.7139 
F-statistic: 20.96 on 1 and 7 DF,  p-value: 0.002549

hr2015.imidacloprid.gls <- gls(logimidacloprid ~ julian,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(hr2015.imidacloprid.gls)

# 12-3-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logimidacloprid ~ julian 
  Data: test 
       AIC      BIC    logLik
  10.92496 11.71386 -1.462478

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
      Phi 
0.1388423 

Coefficients:
                Value Std.Error   t-value p-value
(Intercept) -7.062097 1.7342972 -4.072022  0.0047
julian       0.054808 0.0131023  4.183092  0.0041

 Correlation: 
       (Intr)
julian -0.997

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.0885013 -1.0031158  0.1742807  0.8741000  1.5741674 

Residual standard error: 0.2871388 
Degrees of freedom: 9 total; 7 residual

hr2015.thiacloprid.lm <- lm(logthiacloprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(hr2015.thiacloprid.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logthiacloprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         4         14         24         34         44         54 
-1.388e-17 -3.469e-18 -3.123e-17  7.218e-17 -6.925e-01 -4.499e-01 
        64         74         84 
 8.514e-02  6.941e-01  3.632e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  -0.1903     0.2552  -0.745    0.497
i119          0.2060     0.6252   0.330    0.758
i122         -1.0244     0.6252  -1.639    0.177
i125         -1.0244     0.6252  -1.639    0.177
i128         -1.0244     0.6252  -1.639    0.177

Residual standard error: 0.5707 on 4 degrees of freedom
Multiple R-squared:  0.6361,    Adjusted R-squared:  0.2722 
F-statistic: 1.748 on 4 and 4 DF,  p-value: 0.3009

hr2015.dinotefuran.lm <- lm(logdinotefuran ~ i119+i122+i125+i128+i147,data=test,na.action="na.omit")
summary(hr2015.dinotefuran.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting except julian 147 is significantly higher

Call:
lm(formula = logdinotefuran ~ i119 + i122 + i125 + i128 + i147, 
    data = test, na.action = "na.omit")

Residuals:
         4         14         24         34         44         54 
 1.004e-17 -5.062e-17 -1.281e-17 -1.857e-17 -2.258e-01 -3.655e-01 
        64         74         84 
-3.655e-01  9.567e-01  1.389e-16 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -1.1721     0.3206  -3.656   0.0353 *
i119          1.1616     0.7169   1.620   0.2036  
i122         -0.2258     0.7169  -0.315   0.7734  
i125         -0.2258     0.7169  -0.315   0.7734  
i128         -0.2258     0.7169  -0.315   0.7734  
i147         -0.3655     0.7169  -0.510   0.6453  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.6412 on 3 degrees of freedom
Multiple R-squared:  0.5699,    Adjusted R-squared:  -0.147 
F-statistic: 0.7949 on 5 and 3 DF,  p-value: 0.6173

hr2015.acetamiprid.lm <- lm(logacetamiprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(hr2015.acetamiprid.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logacetamiprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         4         14         24         34         44         54 
-2.776e-17 -6.939e-18  4.857e-17 -7.498e-17 -1.262e-01 -4.131e-01 
        64         74         84 
-4.131e-01  1.366e+00 -4.131e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.9148     0.3459  -2.645   0.0573 .
i119         -0.1262     0.8472  -0.149   0.8888  
i122          0.3429     0.8472   0.405   0.7064  
i125         -0.1262     0.8472  -0.149   0.8888  
i128          0.2631     0.8472   0.311   0.7717  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7734 on 4 degrees of freedom
Multiple R-squared:  0.07884,   Adjusted R-squared:  -0.8423 
F-statistic: 0.08558 on 4 and 4 DF,  p-value: 0.9823

# end hr2015 ----------------------------------------------------------------

# 12-3-19 TV has 31% corn adjacent ------------------------------------------------------------------------------------------- 

test <- pesticide2015new[pesticide2015new$site=="TV",]
test$logclothianidin <- log10(test$clothianidin)
test$logthiamethoxam <- log10(test$thiamethoxam)
test$lognitenpyram <- log10(test$nitenpyram)
test$logimidacloprid <- log10(test$imidacloprid)
test$logthiacloprid <- log10(test$thiacloprid)
test$logdinotefuran <- log10(test$dinotefuran)
test$logacetamiprid <- log10(test$acetamiprid)
test$logimidacloprid[test$imidacloprid==0] <- log10(0.7)

n <- dim(test)
test$cornplt <- rep(0,n[1])
test$cornplt[test$julian>121&test$julian<132] <- 1
test$cornplt2 <- rep(0,n[1])
test$cornplt2[test$julian>120&test$julian<129] <- 1
test$cornplt3 <- rep(0,n[1])
test$cornplt3[test$julian>120&test$julian<128] <- 1
test$i122 <- rep(0,n[1])
test$i122[test$julian==122] <- 1
test$i125 <- rep(0,n[1])
test$i125[test$julian==125] <- 1
test$i128 <- rep(0,n[1])
test$i128[test$julian==128] <- 1
test$i131 <- rep(0,n[1])
test$i131[test$julian==131] <- 1
test$i119 <- rep(0,n[1])
test$i119[test$julian==119] <- 1
test$i147 <- rep(0,n[1])
test$i147[test$julian==147] <- 1
test$intercept <- rep(1,n[1])

png(file="C:\\Users\\hlee05\\Documents\\references\\dendrochronology\\pesticide_2015_TV.png",width=12,height=12,units="in",res=700)
par(mfrow=c(1,1),mai=c(2,2,1,1),oma=c(1,1,1,1),cex=1.3,mex=1.4,lwd=4,cex.lab=1.4,cex.axis=1.4)
plot(test$julian,log10(test$clothianidin),type="b",pch=16,ylim=c(-2,1.5),ylab="Pesticide concentration in pollen sample (log ng/g)",xlab="Julian day for 2015")
points(test$julian,log10(test$thiamethoxam),type="b",pch=16,col="red")
points(test$julian,log10(test$thiacloprid),type="b",pch=16,col="orange")
points(test$julian,log10(test$dinotefuran),type="b",pch=16,col="blue")
points(test$julian,log10(test$nitenpyram),type="b",pch=16,col="green")
points(test$julian,log10(test$imidacloprid),type="b",pch=16,col="purple")
points(test$julian,log10(test$acetamiprid),type="b",pch=16,col="cyan")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)
text(125,-1.8,"Corn planting",adj=0.5,cex=1.2)

legend(135,1.5,legend=c("clothianidin","thiamethoxam","thiacloprid","dinotefuran","nitenpyram","imidacloprid","acetamiprid"),pch=rep(16,7),col=c("black","red","orange","blue","green","purple","cyan"),pt.cex=rep(1.2,7),bty="n")


# 12-3-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

tv2015.clothianidin.lm <- lm(logclothianidin ~ i119+i122+i125+i128+i131,data=test,na.action="na.omit")
summary(tv2015.clothianidin.lm)

Call:
lm(formula = logclothianidin ~ i119 + i122 + i125 + i128 + i131, 
    data = test, na.action = "na.omit")

Residuals:
         9         19         29         39         49         59 
-6.939e-18 -6.939e-18  3.845e-18 -4.660e-18 -5.666e-18 -2.863e-02 
        69         79         89 
-2.863e-02  8.588e-02 -2.863e-02 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.000831   0.028628   0.029 0.978665    
i119        0.244928   0.064015   3.826 0.031443 *  
i122        1.185051   0.064015  18.512 0.000344 ***
i125        1.578312   0.064015  24.655 0.000146 ***
i128        0.877346   0.064015  13.705 0.000840 ***
i131        1.289604   0.064015  20.145 0.000267 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.05726 on 3 degrees of freedom
Multiple R-squared:  0.9971,    Adjusted R-squared:  0.9923 
F-statistic: 208.2 on 5 and 3 DF,  p-value: 0.0005217


tv2015.clothianidin.lm <- lm(logclothianidin ~ cornplt,data=test,na.action="na.omit")
summary(tv2015.clothianidin.lm)

# 12-3-19 reduced model mean for corn planting period (julian=122-131)

Call:
lm(formula = logclothianidin ~ cornplt, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.35523 -0.07761 -0.04753  0.05703  0.34573 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.04982    0.09397   0.530    0.612    
cornplt      1.18359    0.14095   8.397 6.68e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2101 on 7 degrees of freedom
Multiple R-squared:  0.9097,    Adjusted R-squared:  0.8968 
F-statistic: 70.51 on 1 and 7 DF,  p-value: 6.682e-05


tv2015.clothianidin.gls <- gls(logclothianidin ~ cornplt,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(tv2015.clothianidin.gls)

# 12-3-19 1st order autoregressive model <0, use ordinary least squares

Generalized least squares fit by maximum likelihood
  Model: logclothianidin ~ cornplt 
  Data: test 
        AIC       BIC   logLik
  -0.610874 0.1780243 4.305437

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.6013653 

Coefficients:
                Value Std.Error   t-value p-value
(Intercept) 0.0300869 0.0537185  0.560085  0.5929
cornplt     1.2131710 0.0821060 14.775668  0.0000

 Correlation: 
        (Intr)
cornplt -0.741

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.9941021 -0.3161676 -0.3133921  0.3093109  1.8346326 

Residual standard error: 0.1830804 
Degrees of freedom: 9 total; 7 residual


# 12-3-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

tv2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(tv2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         9         19         29         39         49         59 
-1.388e-17 -3.469e-18  1.041e-17  3.371e-19  7.397e-02 -3.531e-01 
        69         79         89 
 3.906e-02  4.380e-01 -1.979e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  0.14193    0.13466   1.054   0.3513  
i119         0.30616    0.32984   0.928   0.4058  
i122         0.08131    0.32984   0.247   0.8174  
i125         1.08058    0.32984   3.276   0.0306 *
i128         0.29645    0.32984   0.899   0.4196  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3011 on 4 degrees of freedom
Multiple R-squared:  0.7358,    Adjusted R-squared:  0.4716 
F-statistic: 2.785 on 4 and 4 DF,  p-value: 0.1725

# 12-3-19 reduced model mean for corn planting period 3 (julian=121-125)

tv2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i125,data=test,na.action="na.omit")
summary(tv2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ i125, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.43854 -0.04643 -0.00418  0.21096  0.35248 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)  0.22742    0.09377   2.425   0.0457 * 
i125         0.99509    0.28131   3.537   0.0095 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2652 on 7 degrees of freedom
Multiple R-squared:  0.6413,    Adjusted R-squared:   0.59 
F-statistic: 12.51 on 1 and 7 DF,  p-value: 0.009504

tv2015.thiamethoxam.gls <- gls(logthiamethoxam ~ i125,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(tv2015.thiamethoxam.gls)

# 12-3-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logthiamethoxam ~ i125 
  Data: test 
      AIC      BIC  logLik
  6.94296 7.731858 0.52852

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.2509719 

Coefficients:
                Value  Std.Error  t-value p-value
(Intercept) 0.2223359 0.07719235 2.880284  0.0236
i125        1.0513942 0.27464754 3.828158  0.0065

 Correlation: 
     (Intr)
i125 -0.414

Standardized residuals:
        Min          Q1         Med          Q3         Max 
-1.84558149 -0.21809080 -0.02739502  0.91988603  1.52242074 

Residual standard error: 0.2348641 
Degrees of freedom: 9 total; 7 residual

tv2015.nitenpyram.lm <- lm(lognitenpyram ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(tv2015.nitenpyram.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = lognitenpyram ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         9         19         29         39         49         59 
-1.388e-17 -3.469e-17  2.252e-18 -3.557e-17  1.450e-01 -7.071e-01 
        69         79         89 
-1.500e-01  6.592e-01  5.286e-02 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.5076     0.2215  -2.292   0.0837 .
i119          0.4240     0.5425   0.782   0.4781  
i122          0.1450     0.5425   0.267   0.8024  
i125          0.5316     0.5425   0.980   0.3826  
i128          0.1450     0.5425   0.267   0.8024  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4952 on 4 degrees of freedom
Multiple R-squared:  0.2529,    Adjusted R-squared:  -0.4942 
F-statistic: 0.3385 on 4 and 4 DF,  p-value: 0.8405

tv2015.nitenpyram.gls <- gls(lognitenpyram ~ intercept-1,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(tv2015.nitenpyram.gls)

# 12-2-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: lognitenpyram ~ intercept - 1 
  Data: test 
       AIC      BIC    logLik
  14.20382 14.79549 -4.101909

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
0.03708862 

Coefficients:
               Value Std.Error   t-value p-value
intercept -0.3682918 0.1395356 -2.639412  0.0297

Standardized residuals:
        Min          Q1         Med          Q3         Max 
-2.21614747 -0.22623158  0.01513833  0.74557508  1.36147844 

Residual standard error: 0.3819143 
Degrees of freedom: 9 total; 8 residual

tv2015.imidacloprid.lm <- lm(logimidacloprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(tv2015.imidacloprid.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logimidacloprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         9         19         29         39         49         59 
 0.000e+00 -2.776e-17  6.628e-18 -3.120e-17  2.122e-01 -4.603e-01 
        69         79         89 
-2.554e-01  2.839e-01  2.196e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  0.34172    0.15016   2.276   0.0852 .
i119        -0.49662    0.36782  -1.350   0.2483  
i122        -0.02303    0.36782  -0.063   0.9531  
i125        -0.39773    0.36782  -1.081   0.3404  
i128        -0.30270    0.36782  -0.823   0.4568  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3358 on 4 degrees of freedom
Multiple R-squared:  0.4237,    Adjusted R-squared:  -0.1526 
F-statistic: 0.7353 on 4 and 4 DF,  p-value: 0.6135

tv2015.thiacloprid.lm <- lm(logthiacloprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(tv2015.thiacloprid.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logthiacloprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         9         19         29         39         49         59 
 0.000e+00  0.000e+00  8.327e-17 -6.043e-17 -7.461e-01 -3.042e-02 
        69         79         89 
 2.962e-02  3.791e-01  3.678e-01 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)  
(Intercept) -0.468522   0.204642  -2.289   0.0839 .
i119        -0.746148   0.501268  -1.489   0.2108  
i122         0.001277   0.501268   0.003   0.9981  
i125        -0.394757   0.501268  -0.788   0.4750  
i128        -0.191033   0.501268  -0.381   0.7225  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4576 on 4 degrees of freedom
Multiple R-squared:  0.3974,    Adjusted R-squared:  -0.2052 
F-statistic: 0.6594 on 4 and 4 DF,  p-value: 0.6518


tv2015.dinotefuran.lm <- lm(logdinotefuran ~ i119+i122+i125+i128+i147,data=test,na.action="na.omit")
summary(tv2015.dinotefuran.lm)

# 12-3-19 some mean differences between corn planting and non-corn planting except julian 147 is significantly higher

Call:
lm(formula = logdinotefuran ~ i119 + i122 + i125 + i128 + i147, 
    data = test, na.action = "na.omit")

Residuals:
         9         19         29         39         49         59 
 2.161e-18 -1.684e-18  3.749e-19  3.256e-18  1.047e-01 -3.492e-02 
        69         79         89 
-3.492e-02 -3.492e-02  3.569e-17 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.50269    0.03492 -43.038 2.76e-05 ***
i119         0.10475    0.07807   1.342  0.27223    
i122         0.10475    0.07807   1.342  0.27223    
i125         0.10475    0.07807   1.342  0.27223    
i128         0.29508    0.07807   3.779  0.03245 *  
i147         0.94517    0.07807  12.106  0.00121 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.06983 on 3 degrees of freedom
Multiple R-squared:  0.9807,    Adjusted R-squared:  0.9486 
F-statistic: 30.55 on 5 and 3 DF,  p-value: 0.00892

tv2015.acetamiprid.lm <- lm(logacetamiprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(tv2015.acetamiprid.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logacetamiprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         9         19         29         39         49         59 
-2.776e-17 -9.021e-17  2.682e-17 -1.245e-16 -2.349e-01 -5.218e-01 
        69         79         89 
-5.218e-01  5.799e-01  6.987e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept) -0.80607    0.26686  -3.021   0.0391 *
i119        -0.23489    0.65366  -0.359   0.7375  
i122         0.30299    0.65366   0.464   0.6671  
i125         0.07558    0.65366   0.116   0.9135  
i128        -0.11475    0.65366  -0.176   0.8692  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5967 on 4 degrees of freedom
Multiple R-squared:  0.1043,    Adjusted R-squared:  -0.7915 
F-statistic: 0.1164 on 4 and 4 DF,  p-value: 0.9697

# end tv2015 ----------------------------------------------------------------

# 12-3-19 MO has 39% corn adjacent ------------------------------------------------------------------------------------------- 

test <- pesticide2015new[pesticide2015new$site=="MO",]
test$logclothianidin <- log10(test$clothianidin)
test$logthiamethoxam <- log10(test$thiamethoxam)
test$lognitenpyram <- log10(test$nitenpyram)
test$logimidacloprid <- log10(test$imidacloprid)
test$logthiacloprid <- log10(test$thiacloprid)
test$logdinotefuran <- log10(test$dinotefuran)
test$logacetamiprid <- log10(test$acetamiprid)

n <- dim(test)
test$cornplt <- rep(0,n[1])
test$cornplt[test$julian>121&test$julian<129] <- 1
test$cornplt2 <- rep(0,n[1])
test$cornplt2[test$julian>122&test$julian<129] <- 1
test$cornplt3 <- rep(0,n[1])
test$cornplt3[test$julian>118&test$julian<135] <- 1
test$i122 <- rep(0,n[1])
test$i122[test$julian==122] <- 1
test$i125 <- rep(0,n[1])
test$i125[test$julian==125] <- 1
test$i128 <- rep(0,n[1])
test$i128[test$julian==128] <- 1
test$i131 <- rep(0,n[1])
test$i131[test$julian==131] <- 1
test$i134 <- rep(0,n[1])
test$i134[test$julian==134] <- 1
test$i119 <- rep(0,n[1])
test$i119[test$julian==119] <- 1
test$i147 <- rep(0,n[1])
test$i147[test$julian==147] <- 1
test$intercept <- rep(1,n[1])

png(file="C:\\Users\\hlee05\\Documents\\references\\dendrochronology\\pesticide_2015_MO.png",width=12,height=12,units="in",res=700)
par(mfrow=c(1,1),mai=c(2,2,1,1),oma=c(1,1,1,1),cex=1.3,mex=1.4,lwd=4,cex.lab=1.4,cex.axis=1.4)
plot(test$julian,log10(test$clothianidin),type="b",pch=16,ylim=c(-2,1.5),ylab="Pesticide concentration in pollen sample (log ng/g)",xlab="Julian day for 2015")
points(test$julian,log10(test$thiamethoxam),type="b",pch=16,col="red")
points(test$julian,log10(test$thiacloprid),type="b",pch=16,col="orange")
points(test$julian,log10(test$dinotefuran),type="b",pch=16,col="blue")
points(test$julian,log10(test$nitenpyram),type="b",pch=16,col="green")
points(test$julian,log10(test$imidacloprid),type="b",pch=16,col="purple")
points(test$julian,log10(test$acetamiprid),type="b",pch=16,col="cyan")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)
text(125,-1.8,"Corn planting",adj=0.5,cex=1.2)

legend(135,1.5,legend=c("clothianidin","thiamethoxam","thiacloprid","dinotefuran","nitenpyram","imidacloprid","acetamiprid"),pch=rep(16,7),col=c("black","red","orange","blue","green","purple","cyan"),pt.cex=rep(1.2,7),bty="n")

# 12-3-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

mo2015.clothianidin.lm <- lm(logclothianidin ~ i119+i122+i125+i128+julian,data=test,na.action="na.omit")
summary(mo2015.clothianidin.lm)

Call:
lm(formula = logclothianidin ~ i119 + i122 + i125 + i128 + julian, 
    data = test, na.action = "na.omit")

Residuals:
         7         17         27         37         47         57 
 6.939e-18  5.204e-18 -4.298e-18  2.155e-17  1.979e-02 -4.239e-02 
        67         77         87 
-5.193e-02  1.625e-01 -8.794e-02 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)  
(Intercept)  4.148472   1.219334   3.402   0.0424 *
i119        -0.248139   0.214025  -1.159   0.3302  
i122         0.196826   0.193257   1.018   0.3834  
i125         0.699328   0.174000   4.019   0.0277 *
i128         0.384888   0.156814   2.454   0.0913 .
julian      -0.027812   0.008777  -3.169   0.0505 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.114 on 3 degrees of freedom
Multiple R-squared:  0.9742,    Adjusted R-squared:  0.9313 
F-statistic: 22.68 on 5 and 3 DF,  p-value: 0.01372

mo2015.clothianidin.lm <- lm(logclothianidin ~ cornplt+julian,data=test,na.action="na.omit")
summary(mo2015.clothianidin.lm)

# 12-3-19 reduced model mean for corn planting period (julian=122-128)

Call:
lm(formula = logclothianidin ~ cornplt + julian, data = test, 
    na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.20297 -0.06934 -0.04233  0.10197  0.27231 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)   
(Intercept)  2.877859   1.029347   2.796  0.03134 * 
cornplt      0.563627   0.145193   3.882  0.00815 **
julian      -0.018740   0.007579  -2.473  0.04829 * 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1717 on 6 degrees of freedom
Multiple R-squared:  0.8831,    Adjusted R-squared:  0.8441 
F-statistic: 22.66 on 2 and 6 DF,  p-value: 0.001598

mo2015.clothianidin.gls <- gls(logclothianidin ~ cornplt+julian,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(mo2015.clothianidin.gls)

# 12-3-19 1st order autoregressive model<0, use ordinary least squares

Generalized least squares fit by maximum likelihood
  Model: logclothianidin ~ cornplt + julian 
  Data: test 
        AIC       BIC   logLik
  -5.633035 -4.646912 7.816518

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.7711975 

Coefficients:
                 Value Std.Error   t-value p-value
(Intercept)  2.0178571 0.6026899  3.348085  0.0155
cornplt      0.6877678 0.0858143  8.014608  0.0002
julian      -0.0124822 0.0043824 -2.848257  0.0292

 Correlation: 
        (Intr) crnplt
cornplt -0.775       
julian  -0.999  0.754

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.5199093 -0.8864270  0.2245862  0.6655522  1.4898603 

Residual standard error: 0.1516773 
Degrees of freedom: 9 total; 6 residual

# 12-3-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

mo2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i119+i122+i125+i128+i131+i134,data=test,na.action="na.omit")
summary(mo2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ i119 + i122 + i125 + i128 + i131 + 
    i134, data = test, na.action = "na.omit")

Residuals:
         7         17         27         37         47         57 
 0.000e+00  0.000e+00  0.000e+00 -6.163e-33 -1.787e-33  2.287e-32 
        67         77         87 
-1.282e-16  6.410e-17  6.410e-17 

Coefficients:
              Estimate Std. Error    t value Pr(>|t|)    
(Intercept) -1.244e+00  6.410e-17 -1.941e+16   <2e-16 ***
i119         1.342e+00  1.282e-16  1.047e+16   <2e-16 ***
i122         1.678e+00  1.282e-16  1.309e+16   <2e-16 ***
i125         1.516e+00  1.282e-16  1.183e+16   <2e-16 ***
i128         1.753e+00  1.282e-16  1.367e+16   <2e-16 ***
i131         1.402e+00  1.282e-16  1.094e+16   <2e-16 ***
i134         1.693e+00  1.282e-16  1.320e+16   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.11e-16 on 2 degrees of freedom
Multiple R-squared:      1,     Adjusted R-squared:      1 
F-statistic: 6.809e+31 on 6 and 2 DF,  p-value: < 2.2e-16

# 12-3-19 reduced model mean for corn planting period 3 (julian=119-134)

mo2015.thiamethoxam.lm <- lm(logthiamethoxam ~ cornplt3,data=test,na.action="na.omit")
summary(mo2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ cornplt3, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.22175 -0.04774  0.00000  0.11389  0.18862 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.24413    0.08243  -15.09 1.35e-06 ***
cornplt3     1.56417    0.10096   15.49 1.13e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1428 on 7 degrees of freedom
Multiple R-squared:  0.9717,    Adjusted R-squared:  0.9676 
F-statistic:   240 on 1 and 7 DF,  p-value: 1.127e-06


mo2015.thiamethoxam.gls <- gls(logthiamethoxam ~ cornplt3,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(mo2015.thiamethoxam.gls)

# 12-3-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logthiamethoxam ~ cornplt3 
  Data: test 
        AIC       BIC   logLik
  -11.70474 -10.91585 9.852372

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.8470806 

Coefficients:
                Value  Std.Error   t-value p-value
(Intercept) -1.231557 0.03067708 -40.14584       0
cornplt3     1.564189 0.03790874  41.26198       0

 Correlation: 
         (Intr)
cornplt3 -0.847

Standardized residuals:
        Min          Q1         Med          Q3         Max 
-1.64997185 -0.42476351 -0.08849194  0.71324523  1.23945834 

Residual standard error: 0.1420234 
Degrees of freedom: 9 total; 7 residual

mo2015.nitenpyram.lm <- lm(lognitenpyram ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(mo2015.nitenpyram.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = lognitenpyram ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         7         17         27         37         47         57 
 0.000e+00  0.000e+00 -5.551e-17 -4.298e-17  4.893e-01 -3.628e-01 
        67         77         87 
-3.628e-01  5.992e-01 -3.628e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.8518     0.2229  -3.822   0.0187 *
i119          0.4893     0.5459   0.896   0.4208  
i122          0.4893     0.5459   0.896   0.4208  
i125          0.4893     0.5459   0.896   0.4208  
i128          0.4893     0.5459   0.896   0.4208  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4984 on 4 degrees of freedom
Multiple R-squared:  0.3488,    Adjusted R-squared:  -0.3025 
F-statistic: 0.5355 on 4 and 4 DF,  p-value: 0.7199

mo2015.imidacloprid.lm <- lm(logimidacloprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(mo2015.imidacloprid.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logimidacloprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         7         17         27         37         47         57 
-1.041e-17 -1.648e-17 -2.010e-17 -6.564e-18 -4.428e-01  3.006e-01 
        67         77         87 
-3.566e-02  1.170e-01  6.083e-02 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)   0.5263     0.1235   4.261    0.013 *
i119         -1.3561     0.3025  -4.482    0.011 *
i122         -0.4555     0.3025  -1.506    0.207  
i125          0.1098     0.3025   0.363    0.735  
i128          0.2318     0.3025   0.766    0.486  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2762 on 4 degrees of freedom
Multiple R-squared:  0.8599,    Adjusted R-squared:  0.7197 
F-statistic: 6.136 on 4 and 4 DF,  p-value: 0.05342

mo2015.imidacloprid.lm <- lm(logimidacloprid ~ i119,data=test,na.action="na.omit")
summary(mo2015.imidacloprid.lm)

Call:
lm(formula = logimidacloprid ~ i119, data = test, na.action = "na.omit")

Residuals:
     Min       1Q   Median       3Q      Max 
-0.44129 -0.02140  0.07508  0.13129  0.31485 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)   0.5121     0.1015   5.046  0.00149 **
i119         -1.3418     0.3045  -4.407  0.00313 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.287 on 7 degrees of freedom
Multiple R-squared:  0.7351,    Adjusted R-squared:  0.6972 
F-statistic: 19.42 on 1 and 7 DF,  p-value: 0.003129

mo2015.imidacloprid.gls <- gls(logimidacloprid ~ i119,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(mo2015.imidacloprid.gls)

# 12-3-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logimidacloprid ~ i119 
  Data: test 
       AIC      BIC logLik
  5.698399 6.487297 1.1508

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.6190315 

Coefficients:
                 Value  Std.Error   t-value p-value
(Intercept)  0.5314176 0.05397105  9.846346   0e+00
i119        -1.6463073 0.25075871 -6.565305   3e-04

 Correlation: 
     (Intr)
i119 -0.348

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-1.7452697 -0.1544193  0.3965678  0.8587703  1.1195819 

Residual standard error: 0.2639369 
Degrees of freedom: 9 total; 7 residual

mo2015.thiacloprid.lm <- lm(logthiacloprid ~ i119+i122+i125+i128+i131,data=test,na.action="na.omit")
summary(mo2015.thiacloprid.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logthiacloprid ~ i119 + i122 + i125 + i128 + i131, 
    data = test, na.action = "na.omit")

Residuals:
         7         17         27         37         47         57 
 0.000e+00  1.561e-17 -6.330e-18  6.427e-18  3.049e-17  2.058e-01 
        67         77         87 
-4.824e-01  2.388e-01  3.773e-02 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  0.03745    0.16670   0.225   0.8367  
i119        -0.25569    0.37276  -0.686   0.5420  
i122        -0.77500    0.37276  -2.079   0.1291  
i125        -0.90073    0.37276  -2.416   0.0945 .
i128        -0.36628    0.37276  -0.983   0.3983  
i131        -0.64651    0.37276  -1.734   0.1813  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3334 on 3 degrees of freedom
Multiple R-squared:  0.7618,    Adjusted R-squared:  0.3647 
F-statistic: 1.919 on 5 and 3 DF,  p-value: 0.3139

mo2015.dinotefuran.lm <- lm(logdinotefuran ~ i119+i122+i125+i128+i147,data=test,na.action="na.omit")
summary(mo2015.dinotefuran.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting except julian 147 is significantly higher

Call:
lm(formula = logdinotefuran ~ i119 + i122 + i125 + i128 + i147, 
    data = test, na.action = "na.omit")

Residuals:
         7         17         27         37         47         57 
-8.547e-19 -1.676e-18 -2.874e-18  1.110e-17  1.047e-01 -3.492e-02 
        67         77         87 
-3.492e-02 -3.492e-02  2.317e-17 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.50269    0.03492 -43.038 2.76e-05 ***
i119         0.10475    0.07807   1.342    0.272    
i122         0.10475    0.07807   1.342    0.272    
i125         0.10475    0.07807   1.342    0.272    
i128         0.10475    0.07807   1.342    0.272    
i147        -0.03492    0.07807  -0.447    0.685    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.06983 on 3 degrees of freedom
Multiple R-squared:  0.6625,    Adjusted R-squared:    0.1 
F-statistic: 1.178 on 5 and 3 DF,  p-value: 0.4763

mo2015.acetamiprid.lm <- lm(logacetamiprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(mo2015.acetamiprid.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logacetamiprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         7         17         27         37         47         57 
-6.939e-18 -6.072e-17 -2.061e-17 -1.809e-17  9.135e-02 -4.514e-01 
        67         77         87 
-4.514e-01  8.935e-01 -8.210e-02 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept) -0.87651    0.24708  -3.547   0.0239 *
i119         0.71346    0.60523   1.179   0.3038  
i122         0.27963    0.60523   0.462   0.6681  
i125         0.29148    0.60523   0.482   0.6553  
i128         0.09135    0.60523   0.151   0.8873  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5525 on 4 degrees of freedom
Multiple R-squared:  0.278,     Adjusted R-squared:  -0.444 
F-statistic: 0.3851 on 4 and 4 DF,  p-value: 0.8111

# end mo2015 ----------------------------------------------------------------

# 12-3-19 FSR has 49% corn adjacent ------------------------------------------------------------------------------------------- 

test <- pesticide2015new[pesticide2015new$site=="FSR",]
test$logclothianidin <- log10(test$clothianidin)
test$logthiamethoxam <- log10(test$thiamethoxam)
test$lognitenpyram <- log10(test$nitenpyram)
test$logimidacloprid <- log10(test$imidacloprid)
test$logthiacloprid <- log10(test$thiacloprid)
test$logdinotefuran <- log10(test$dinotefuran)
test$logacetamiprid <- log10(test$acetamiprid)

n <- dim(test)
test$cornplt <- rep(0,n[1])
test$cornplt[test$julian>121&test$julian<129] <- 1
test$cornplt2 <- rep(0,n[1])
test$cornplt2[test$julian>122&test$julian<129] <- 1
test$cornplt3 <- rep(0,n[1])
test$cornplt3[test$julian>118&test$julian<135] <- 1
test$i122 <- rep(0,n[1])
test$i122[test$julian==122] <- 1
test$i125 <- rep(0,n[1])
test$i125[test$julian==125] <- 1
test$i128 <- rep(0,n[1])
test$i128[test$julian==128] <- 1
test$i131 <- rep(0,n[1])
test$i131[test$julian==131] <- 1
test$i134 <- rep(0,n[1])
test$i134[test$julian==134] <- 1
test$i119 <- rep(0,n[1])
test$i119[test$julian==119] <- 1
test$i147 <- rep(0,n[1])
test$i147[test$julian==147] <- 1
test$intercept <- rep(1,n[1])

png(file="C:\\Users\\hlee05\\Documents\\references\\dendrochronology\\pesticide_2015_FSR.png",width=12,height=12,units="in",res=700)
par(mfrow=c(1,1),mai=c(2,2,1,1),oma=c(1,1,1,1),cex=1.3,mex=1.4,lwd=4,cex.lab=1.4,cex.axis=1.4)
plot(test$julian,log10(test$clothianidin),type="b",pch=16,ylim=c(-2,2),ylab="Pesticide concentration in pollen sample (log ng/g)",xlab="Julian day for 2015")
points(test$julian,log10(test$thiamethoxam),type="b",pch=16,col="red")
points(test$julian,log10(test$thiacloprid),type="b",pch=16,col="orange")
points(test$julian,log10(test$dinotefuran),type="b",pch=16,col="blue")
points(test$julian,log10(test$nitenpyram),type="b",pch=16,col="green")
points(test$julian,log10(test$imidacloprid),type="b",pch=16,col="purple")
points(test$julian,log10(test$acetamiprid),type="b",pch=16,col="cyan")

abline(v=31+28+31+30+2)
abline(v=31+28+31+30+8)
text(125,-1.8,"Corn planting",adj=0.5,cex=1.2)

legend(135,2,legend=c("clothianidin","thiamethoxam","thiacloprid","dinotefuran","nitenpyram","imidacloprid","acetamiprid"),pch=rep(16,7),col=c("black","red","orange","blue","green","purple","cyan"),pt.cex=rep(1.2,7),bty="n")

# 12-3-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

fsr2015.clothianidin.lm <- lm(logclothianidin ~ i119+i122+i125+i128+i131+julian,data=test,na.action="na.omit")
summary(fsr2015.clothianidin.lm)

Call:
lm(formula = logclothianidin ~ i119 + i122 + i125 + i128 + i131, 
    data = test, na.action = "na.omit")

Residuals:
         3         13         23         33         43         53 
 0.000e+00  0.000e+00 -3.469e-18 -9.516e-19 -9.516e-19 -3.518e-02 
        63         73         83 
 1.056e-01 -3.518e-02 -3.518e-02 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 0.007387   0.035184   0.210 0.847153    
i119        0.444706   0.078674   5.652 0.010961 *  
i122        1.955834   0.078674  24.860 0.000143 ***
i125        1.664110   0.078674  21.152 0.000231 ***
i128        0.339552   0.078674   4.316 0.022914 *  
i131        1.223419   0.078674  15.550 0.000578 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.07037 on 3 degrees of freedom
Multiple R-squared:  0.997,     Adjusted R-squared:  0.9919 
F-statistic: 197.3 on 5 and 3 DF,  p-value: 0.0005652


fsr2015.clothianidin.lm <- lm(logclothianidin ~ i122+i125+i131+julian,data=test,na.action="na.omit")
summary(fsr2015.clothianidin.lm)


# 12-3-19 reduced model 

Call:
lm(formula = logclothianidin ~ i122 + i125 + i131 + julian, data = test, 
    na.action = "na.omit")

Residuals:
         3         13         23         33         43         53 
 2.001e-02 -2.602e-18 -8.674e-19  8.022e-02  3.643e-17 -1.843e-01 
        63         73         83 
 4.834e-02 -1.890e-02  5.460e-02 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.618647   0.633551   4.133 0.014455 *  
i122         1.586256   0.131364  12.075 0.000270 ***
i125         1.349656   0.125476  10.756 0.000424 ***
i131         1.019212   0.117912   8.644 0.000985 ***
julian      -0.018374   0.004682  -3.925 0.017178 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1078 on 4 degrees of freedom
Multiple R-squared:  0.9905,    Adjusted R-squared:  0.981 
F-statistic: 104.5 on 4 and 4 DF,  p-value: 0.000268


fsr2015.clothianidin.gls <- gls(logclothianidin ~ i122+i125+i131+julian,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(fsr2015.clothianidin.gls)

# 12-3-19 1st order autoregressive coef<0, use ordinary least squares model above

Generalized least squares fit by maximum likelihood
  Model: logclothianidin ~ i122 + i125 + i131 + julian 
  Data: test 
        AIC      BIC   logLik
  -8.601132 -7.22056 11.30057

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.3241444 

Coefficients:
                 Value Std.Error   t-value p-value
(Intercept)  2.6445954 0.5933716  4.456896  0.0112
i122         1.5839037 0.1408409 11.246048  0.0004
i125         1.3736892 0.1252555 10.967094  0.0004
i131         0.9904721 0.1208128  8.198405  0.0012
julian      -0.0185811 0.0043257 -4.295514  0.0127

 Correlation: 
       (Intr) i122   i125   i131  
i122   -0.627                     
i125   -0.473  0.216              
i131   -0.365  0.359  0.325       
julian -0.998  0.609  0.452  0.336

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-2.5202575 -0.2112146  0.2574749  0.7057869  1.1145958 

Residual standard error: 0.07242386 
Degrees of freedom: 9 total; 4 residual


# 12-3-19 mean comparison tests using pulse intervention terms for individual days within corn planting period

fsr2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i119+i122+i125+i128+i131+julian,data=test,na.action="na.omit")
summary(fsr2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ i119 + i122 + i125 + i128 + i131 + 
    julian, data = test, na.action = "na.omit")

Residuals:
         3         13         23         33         43         53 
 2.776e-17 -6.939e-18  3.314e-18 -1.056e-17  2.705e-17 -1.330e-01 
        63         73         83 
 1.178e-02  4.087e-01 -2.874e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept) 16.96059    5.34750   3.172   0.0867 .
i119        -3.15078    0.92152  -3.419   0.0759 .
i122        -0.42319    0.82099  -0.515   0.6576  
i125        -0.44490    0.72443  -0.614   0.6017  
i128        -1.21708    0.63367  -1.921   0.1947  
i131         0.11567    0.55156   0.210   0.8533  
julian      -0.12189    0.03797  -3.210   0.0849 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3657 on 2 degrees of freedom
Multiple R-squared:  0.9625,    Adjusted R-squared:  0.8498 
F-statistic: 8.545 on 6 and 2 DF,  p-value: 0.1085

# 12-3-19 reduced model 

fsr2015.thiamethoxam.lm <- lm(logthiamethoxam ~ i119+julian,data=test,na.action="na.omit")
summary(fsr2015.thiamethoxam.lm)

Call:
lm(formula = logthiamethoxam ~ i119 + julian, data = test, na.action = "na.omit")

Residuals:
    Min      1Q  Median      3Q     Max 
-0.8127  0.0000  0.1027  0.1500  0.4357 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept) 12.95551    2.59276   4.997  0.00246 **
i119        -2.49319    0.55651  -4.480  0.00419 **
julian      -0.09376    0.01937  -4.841  0.00288 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4516 on 6 degrees of freedom
Multiple R-squared:  0.8282,    Adjusted R-squared:  0.7709 
F-statistic: 14.46 on 2 and 6 DF,  p-value: 0.005071


fsr2015.thiamethoxam.gls <- gls(logthiamethoxam ~ i119+julian,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(fsr2015.thiamethoxam.gls)

# 12-3-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logthiamethoxam ~ i119 + julian 
  Data: test 
       AIC      BIC    logLik
  15.88483 16.87095 -2.942414

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.4653313 

Coefficients:
                Value Std.Error   t-value p-value
(Intercept) 12.080524 1.7974357  6.720977  0.0005
i119        -2.308031 0.5082911 -4.540766  0.0039
julian      -0.087118 0.0134373 -6.483300  0.0006

 Correlation: 
       (Intr) i119  
i119   -0.543       
julian -0.998  0.527

Standardized residuals:
       Min         Q1        Med         Q3        Max 
-2.1053351 -0.2676945  0.2356348  0.5752771  1.1794493 

Residual standard error: 0.3739821 
Degrees of freedom: 9 total; 6 residual

fsr2015.nitenpyram.lm <- lm(lognitenpyram ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(fsr2015.nitenpyram.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = lognitenpyram ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         3         13         23         33         43         53 
 0.000e+00  0.000e+00  2.776e-17  7.613e-18  1.231e-01 -7.291e-01 
        63         73         83 
 2.998e-01 -2.969e-01  6.032e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  -0.4856     0.2333  -2.081    0.106
i119          0.1231     0.5715   0.215    0.840
i122          0.1231     0.5715   0.215    0.840
i125          0.1231     0.5715   0.215    0.840
i128          0.1231     0.5715   0.215    0.840

Residual standard error: 0.5217 on 4 degrees of freedom
Multiple R-squared:  0.02999,   Adjusted R-squared:  -0.94 
F-statistic: 0.03091 on 4 and 4 DF,  p-value: 0.9974

fsr2015.imidacloprid.lm <- lm(logimidacloprid ~ i119+i122+i125+i128+i131,data=test,na.action="na.omit")
summary(fsr2015.imidacloprid.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logimidacloprid ~ i119 + i122 + i125 + i128 + i131, 
    data = test, na.action = "na.omit")

Residuals:
         3         13         23         33         43         53 
 0.000e+00  0.000e+00  2.776e-17 -4.790e-17  4.703e-17 -3.484e-01 
        63         73         83 
-2.184e-01 -1.982e-02  5.866e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   0.4411     0.2069   2.132    0.123
i119         -0.5850     0.4626  -1.265    0.295
i122          0.1112     0.4626   0.240    0.826
i125          0.2259     0.4626   0.488    0.659
i128         -0.2679     0.4626  -0.579    0.603
i131          1.0036     0.4626   2.170    0.118

Residual standard error: 0.4137 on 3 degrees of freedom
Multiple R-squared:  0.7396,    Adjusted R-squared:  0.3055 
F-statistic: 1.704 on 5 and 3 DF,  p-value: 0.3506

fsr2015.imidacloprid.lm <- lm(logimidacloprid ~ i131,data=test,na.action="na.omit")
summary(fsr2015.imidacloprid.lm)

# 12-3-19 reduced model

Call:
lm(formula = logimidacloprid ~ i131, data = test, na.action = "na.omit")

Residuals:
    Min      1Q  Median      3Q     Max 
-0.5205 -0.2034  0.0000  0.1757  0.6511 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)   0.3766     0.1308   2.880   0.0236 *
i131          1.0681     0.3923   2.723   0.0297 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3699 on 7 degrees of freedom
Multiple R-squared:  0.5143,    Adjusted R-squared:  0.4449 
F-statistic: 7.413 on 1 and 7 DF,  p-value: 0.02965

fsr2015.imidacloprid.gls <- gls(logimidacloprid ~ i131,
correlation = corARMA(p = 1, q = 0),data=test,na.action=na.omit,method="ML")
summary(fsr2015.imidacloprid.gls)

# 12-3-19 since AR1 coef <0, ignore autocorrelation; use ordinary regression model above

Generalized least squares fit by maximum likelihood
  Model: logimidacloprid ~ i131 
  Data: test 
       AIC      BIC    logLik
  13.28849 14.07738 -2.644243

Correlation Structure: AR(1)
 Formula: ~1 
 Parameter estimate(s):
       Phi 
-0.2529773 

Coefficients:
                Value Std.Error  t-value p-value
(Intercept) 0.3891941 0.1096815 3.548403  0.0094
i131        0.9336894 0.3906244 2.390248  0.0482

 Correlation: 
     (Intr)
i131 -0.414

Standardized residuals:
        Min          Q1         Med          Q3         Max 
-1.59462126 -0.64616442  0.09596609  0.48792341  1.90995017 

Residual standard error: 0.3342923 
Degrees of freedom: 9 total; 7 residual


fsr2015.thiacloprid.lm <- lm(logthiacloprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(fsr2015.thiacloprid.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logthiacloprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         3         13         23         33         43         53 
 0.000e+00  2.776e-17 -2.051e-17  2.739e-17 -4.391e-02 -4.271e-01 
        63         73         83 
-8.306e-02 -7.995e-03  5.621e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  -0.2384     0.1593  -1.497    0.209
i119         -0.2903     0.3901  -0.744    0.498
i122         -0.5575     0.3901  -1.429    0.226
i125          0.1549     0.3901   0.397    0.712
i128         -0.1683     0.3901  -0.431    0.688

Residual standard error: 0.3561 on 4 degrees of freedom
Multiple R-squared:  0.4184,    Adjusted R-squared:  -0.1632 
F-statistic: 0.7194 on 4 and 4 DF,  p-value: 0.6213


fsr2015.dinotefuran.lm <- lm(logdinotefuran ~ i119+i122+i125+i128+i147,data=test,na.action="na.omit")
summary(fsr2015.dinotefuran.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting except julian 147 is significantly higher

Call:
lm(formula = logdinotefuran ~ i119 + i122 + i125 + i128 + i147, 
    data = test, na.action = "na.omit")

Residuals:
         3         13         23         33         43         53 
 3.429e-18  5.792e-18 -6.382e-18 -8.119e-18 -1.031e-01  8.723e-02 
        63         73         83 
 2.586e-01 -2.428e-01 -5.106e-17 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)  -1.2948     0.1096 -11.818   0.0013 **
i119         -0.1031     0.2450  -0.421   0.7022   
i122         -0.1031     0.2450  -0.421   0.7022   
i125         -0.1031     0.2450  -0.421   0.7022   
i128         -0.1031     0.2450  -0.421   0.7022   
i147         -0.2428     0.2450  -0.991   0.3948   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2191 on 3 degrees of freedom
Multiple R-squared:  0.2718,    Adjusted R-squared:  -0.942 
F-statistic: 0.2239 on 5 and 3 DF,  p-value: 0.9296

fsr2015.acetamiprid.lm <- lm(logacetamiprid ~ i119+i122+i125+i128,data=test,na.action="na.omit")
summary(fsr2015.acetamiprid.lm)

# 12-3-19 no mean differences between corn planting and non-corn planting

Call:
lm(formula = logacetamiprid ~ i119 + i122 + i125 + i128, data = test, 
    na.action = "na.omit")

Residuals:
         3         13         23         33         43         53 
 0.000e+00 -2.776e-17 -7.250e-18 -7.250e-18 -2.007e-01 -4.876e-01 
        63         73         83 
 7.228e-01  4.531e-01 -4.876e-01 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -0.8403     0.2494  -3.370    0.028 *
i119          0.1687     0.6108   0.276    0.796  
i122         -0.2007     0.6108  -0.329    0.759  
i125          0.6403     0.6108   1.048    0.354  
i128         -0.2007     0.6108  -0.329    0.759  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5576 on 4 degrees of freedom
Multiple R-squared:  0.287,     Adjusted R-squared:  -0.426 
F-statistic: 0.4025 on 4 and 4 DF,  p-value: 0.8002


# end fsr2015 ----------------------------------------------------------------


