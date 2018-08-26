# Test normality: if p > 0.01, accept H0 = data is normally distributed
shapiro.test(results$resid.model1) 
shapiro.test(results$resid.model2) 
shapiro.test(results$resid.model3) 
# All normally distributed

# Since normal, compare residual *means* with ANOVA
# Format the data for anova function
resid.allmodels <- c(results$resid.model1, results$resid.model2, results$resid.model3)
n = rep(250,3)                      # 250 observations, 3 models
group = rep(1:3, n)
resid.allmodels.gr <- data.frame(y=resid.allmodels, group=factor(group))    # Make data frame with group variable
fit <- lm(y ~ group, data=resid.allmodels.gr)
anova(fit)
# Since p > 0.01 (choose significance level), not significant so accept H0 i.e., means of models' residual are the same


# Since normal, compare residual *variances* with Bartlett test
?bartlett.test
bartlett.test(y ~ group, resid.allmodels.gr)
# p > 0.01 (choose significance level), not significant so accept H0 i.e., variances of models' residual are the same