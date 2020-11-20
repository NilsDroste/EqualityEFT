

library(plm)
library(lmtest)
library(sandwich)
library(stargazer)


# PROTECTED AREAS

#constant = 0.5 times the minimal observed value #required for log transformations of variables /w 0
c<-min(full_df$mun[which(full_df$mun > 0)])*0.5
c2<-min(full_df$sta[which(full_df$sta > 0)])*0.5


#basic random effects regression (model 1)
m1.mun <- plm(log(mun+c)~icms_e+log(agr)+log(ind)+log(pop)+log(inc)+log(fed)+log(sta+c2)+arpa, data=full_df, index=c("state", "year"), model="random", effect="individual")
summary(m1.mun)
pbgtest(m1.mun) # null is that there is no serial correlation, p < 0.5 <- serial correlation
pcdtest(m1.mun) # null is that there is no cross-sectional dependence , p < 0.5 <- cross-sectional dependence
m1.mun.rob<-coeftest(m1.mun, vcov=function(x) vcovSCC(x, type="HC3", maxlag=2)); m1.mun.rob

#random effects regression with biomes (model 2)
m2.mun <- plm(log(mun+c)~icms_e+log(agr)+log(ind)+log(pop)+log(inc)+log(fed)+log(sta+c2)+arpa+ama+cer+caa+mat+pan+pam, data = full_df, index=c("state", "year"), model="random", effect="ind")
summary(m2.mun)
pbgtest(m2.mun) # null is that there is no serial correlation, p < 0.5 <- serial correlation
pcdtest(m2.mun) # null is that there is no cross-sectional dependence , p < 0.5 <- cross-sectional dependence
m2.mun.rob<-coeftest(m2.mun, vcov=function(x) vcovSCC(x, type="HC3", maxlag=2)); m2.mun.rob

#oneway+detrend
m3.mun<-plm(log(mun+c)~icms_e+log(agr)+log(ind)+log(pop)+log(inc)+log(fed)+log(sta+c2)+arpa+ama+cer+caa+mat+pan+pam+icms_e*as.numeric(year), data=full_df, index=c("state", "year"), model="random", effect="individual")
summary(m3.mun)
pbgtest(m3.mun) # null is that there is no serial correlation, p < 0.5 <- serial correlation
pcdtest(m3.mun) # null is that there is no cross-sectional dependence , p < 0.5 <- cross-sectional dependence
m3.mun.rob<-coeftest(m3.mun, vcov=function(x) vcovSCC(x, type="HC3", maxlag=2)); m3.mun.rob

#oneway+detrend+interaction
m4.mun<-plm(log(mun+c)~icms_e+log(agr)+log(ind)+log(pop)+log(inc)+log(fed)+log(sta+c2)+arpa+ama+cer+caa+mat+pan+pam+icms_e*as.numeric(year)+icms_e:log(agr)+icms_e:log(ind)+icms_e:log(pop)+icms_e:log(inc)+icms_e:log(fed)+icms_e:log(sta+c2), data=full_df, index=c("state", "year"), model="random", effect="individual")
summary(m4.mun)
pbgtest(m4.mun) # null is that there is no serial correlation, p < 0.5 <- serial correlation
pcdtest(m4.mun) # null is that there is no cross-sectional dependence , p < 0.5 <- cross-sectional dependence
m4.mun.rob<-coeftest(m4.mun, vcov=function(x) vcovSCC(x, type="HC3", maxlag=2)); m4.mun.rob




# GINI COEFFICIENT

#basic random effects regression (model 1)
m1.mun.gin <- plm(lnGini~icms_e+log(agr)+log(ind)+log(pop)+log(inc), data=full_gini_df, index=c("state", "year"), model="random", effect="individual")
summary(m1.mun.gin)
pbgtest(m1.mun.gin) # null is that there is no serial correlation, p < 0.5 <- serial correlation
pcdtest(m1.mun.gin) # null is that there is no cross-sectional dependence , p < 0.5 <- cross-sectional dependence
m1.mun.gin.rob<-coeftest(m1.mun.gin, vcov=function(x) vcovSCC(x, type="HC3", maxlag=2)); m1.mun.gin.rob

#random effects regression with biomes (model 2)
m2.mun.gin <- plm(lnGini~icms_e+log(agr)+log(ind)+log(pop)+log(inc)+ama+cer+caa+mat+pan+pam,data=full_gini_df, index=c("state", "year"), model="random", effect="ind")
summary(m2.mun.gin)
pbgtest(m2.mun.gin) # null is that there is no serial correlation, p < 0.5 <- serial correlation
pcdtest(m2.mun.gin) # null is that there is no cross-sectional dependence , p < 0.5 <- cross-sectional dependence
m2.mun.gin.rob<-coeftest(m2.mun.gin, vcov=function(x) vcovSCC(x, type="HC3", maxlag=2)); m2.mun.gin.rob

#oneway+detrend
m3.mun.gin <-plm(lnGini~icms_e+log(agr)+log(ind)+log(pop)+log(inc)+ama+cer+caa+mat+pan+pam+icms_e*as.numeric(year), data=full_gini_df, index=c("state", "year"), model="random", effect="individual")
summary(m3.mun.gin)
pbgtest(m3.mun.gin) # null is that there is no serial correlation, p < 0.5 <- serial correlation
pcdtest(m3.mun.gin) # null is that there is no cross-sectional dependence , p < 0.5 <- cross-sectional dependence
m3.mun.gin.rob<-coeftest(m3.mun.gin, vcov=function(x) vcovSCC(x, type="HC3", maxlag=2)); m3.mun.gin.rob

#oneway+detrend+interaction
m4.mun.gin<-plm(lnGini~icms_e+log(agr)+log(ind)+log(pop)+log(inc)+ama+cer+caa+mat+pan+pam+icms_e*as.numeric(year)+icms_e:log(agr)+icms_e:log(ind)+icms_e:log(pop)+icms_e:log(inc), data=full_gini_df, index=c("state", "year"), model="random", effect="individual")
summary(m4.mun.gin)
pbgtest(m4.mun.gin) # null is that there is no serial correlation, p < 0.5 <- serial correlation
pcdtest(m4.mun.gin) # null is that there is no cross-sectional dependence , p < 0.5 <- cross-sectional dependence
m4.mun.gin.rob<-coeftest(m4.mun.gin, vcov=function(x) vcovSCC(x, type="HC3", maxlag=2)); m4.mun.gin.rob



#analytical plots
par(mfrow=c(2,2))
plot(density(resid(m1.mun.gin)),main = "residual density")
fitted.m1.mun.gin=(m1.mun.gin$model[[1]] - m1.mun.gin$residuals)
plot(resid(m1.mun.gin) %>% as.numeric ~(fitted.m1.mun.gin) %>% as.numeric(), main = "Residuals vs Fitted", ylab="residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted.m1.mun.gin, residuals(m1.mun.gin)), col="red")
qqnorm(m1.mun.gin$resid)
qqline(m1.mun.gin$resid, col="red")
lev = hat(model.matrix(m1.mun.gin))
plot(lev, main="leverage")

plot(density(resid(m2.mun.gin)),main = "residual density")
fitted.m2.mun.gin=(m2.mun.gin$model[[1]] - m2.mun.gin$residuals)
plot(resid(m2.mun.gin)%>% as.numeric ~(fitted.m2.mun.gin) %>% as.numeric, main = "Residuals vs Fitted", ylab="residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted.m2.mun.gin, residuals(m2.mun.gin)), col="red")
qqnorm(m2.mun.gin$resid)
qqline(m2.mun.gin$resid, col="red")
lev = hat(model.matrix(m2.mun.gin))
plot(lev, main="leverage")
#df[lev>0.4,]

plot(density(resid(m3.mun.gin)),main = "residual density")
fitted.m3.mun.gin=(m3.mun.gin$model[[1]] - m3.mun.gin$residuals)
plot(resid(m3.mun.gin) %>% as.numeric ~(fitted.m3.mun.gin) %>% as.numeric, main = "Residuals vs Fitted", ylab="residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted.m3.mun.gin, residuals(m3.mun.gin)), col="red")
qqnorm(m3.mun.gin$resid)
qqline(m3.mun.gin$resid, col="red")
lev = hat(model.matrix(m3.mun.gin)); 
plot(lev, main="leverage")
#df[lev>0.3,]

plot(density(resid(m4.mun.gin)),main = "residual density")
fitted.m4.mun.gin=(m4.mun.gin$model[[1]] - m4.mun.gin$residuals)
plot(resid(m4.mun.gin)%>% as.numeric~(fitted.m4.mun.gin)%>% as.numeric, main = "Residuals vs Fitted", ylab="residuals")
abline(h=0, lty=2)
lines(smooth.spline(fitted.m4.mun.gin, residuals(m4.mun.gin)), col="red")
qqnorm(m4.mun.gin$resid)
qqline(m4.mun.gin$resid, col="red")
lev = hat(model.matrix(m4.mun.gin))
plot(lev, main="leverage")
#df[lev>0.3,]

