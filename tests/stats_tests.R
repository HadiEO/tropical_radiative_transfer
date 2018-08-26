temp <- read_csv2("results/temp/temp_withLatLon.csv")


# Statistical tests of canopy structure differences -----------------------
# (1) Test homogeneity of variances with Bartlett test, if variances not the same, apply Welsh correction
# (2) If variance is the same, do ANOVA and Tukey's HSD test

# We test response variables: ECC, ACC75, LAIe, LAIt, CI
# We test 3 forest classes for sufficient sample sizes: closed-canopy, salvage-logged stage 2, and oil palm



# Test normality: if p > 0.01, accept H0 = data is normally distributed
# ECC_my, Closure75_my, LAI_my, LAIt_CIcc_my, CIcc_my
data <- as.data.frame(temp)
data3 <- data[data$Type.new == "Oil Palm", "CIcc_my"]

shapiro.test(data3)       # Note that this CI is average CI for photos in each plot. So this CI != plot_LAIe / plot_LAIt
# If  normal, compare residual *means* with ANOVA
# If normal, compare residual *variances* with Bartlett test

# Result
# Primary : all canopy variables NORMAL (P > 0.01)
# Light : all canopy variables NORMAL (P > 0.01)
# Twice : all canopy variables NORMAL (P > 0.01), except CI 
# Salvage 1 : all canopy variables NORMAL (P > 0.01), except LAIt
# Salvage 2 : all canopy variables NORMAL (P > 0.01), except ECC, Clos
# OP : all canopy variables NORMAL (P > 0.01)

# c(Prim, Light, Twice) 


# Format the data for anova function
stats.data <- temp %>% mutate(Type.new = as.factor(Type.new))
# Recode Prim, Light, Twice, and Salvage1 -> Closed Canopy
stats.data.rc <- stats.data
stats.data.rc$Type.new <- mapvalues(stats.data.rc$Type.new, 
                                    from = c("Primary", "Lightly Logged", "Twice Logged", "Salvage Logged stage 1"),
                                    to = c("Closed Canopy", "Closed Canopy", "Closed Canopy", "Closed Canopy") ) 


# Test differences for original 6 forest classes, or recoded 3 classes?
stats.data.use <- stats.data

# ANOVA
fit <- lm(ECC_my ~ Type.new, data = stats.data.use)
anova(fit)
# If p > 0.01 (choose significance level), not significant so accept H0 i.e., means of models' residual are the same


# Tukey HSD test
aov.res <- aov(ECC_my ~ Type.new, data = stats.data.use)
posthoc <- TukeyHSD(aov.res, "Type.new", conf.level = 0.95)


# Bartlett
?bartlett.test
bartlett.test(CIcc_my ~ Type.new, stats.data.rc)
# p > 0.01 (choose significance level), not significant so accept H0 i.e., variances of models' residual are the same
# Result:
# for stats.data (6 classes) p-value << 0.01 for all variables
# for stats.data.rc (3 classes) p-value << 0.01 for all variables
# reject H0 i.e., variances of models' residual are NOT the same

# ANOVA with Welch correction
oneway.test(Closure75_my ~ Type.new, data = stats.data.use, na.action=na.omit, var.equal=FALSE)

# t.test with Welch correction
# alternative  =  "two.sided", "greater", "less"
# conf.level = 0.95
# t.test(x, y, alternative = "two.sided", var.equal = FALSE, conf.level = 0.99)


##########################################################################
# The t.tests -------------------------------------------------------------
##########################################################################

# (1) Canopy structure ----------------------------------------------------
# ECC_my, Closure75_my, LAI_my, LAIt_CIcc_my, CIcc_my
data <- as.data.frame(stats.data)
x <- data[data$Type.new == "Oil Palm", "CIcc_my"]
y <- data[data$Type.new %in% c("Salvage Logged stage 2", "Salvage Logged stage 1", "Twice Logged", "Lightly Logged", "Primary"), "CIcc_my"]
t.test(x, y, alternative = "less", var.equal = FALSE, conf.level = 0.99)

# Largest
x <- data[data$Type.new == "Oil Palm", "ECC_my"]
y <- data[data$Type.new %in% c("Salvage Logged stage 2"), "ECC_my"]
# t.test(x, y, alternative = "less", var.equal = FALSE, conf.level = 0.99)

# Bartlett test of variance (H0 : variances are same)
x2 <- c(x, y)
y2 <- c(rep(1, length(x)), rep(2, length(y)))
bartlett.test(x2, y2)


# (2) Landsat BRF ----------------------------------------------------
data <- as.data.frame(stats.data)
x <- data[data$Type.new == "Oil Palm", "SR_B3_red"]
y <- data[data$Type.new %in% c("Salvage Logged stage 2", "Salvage Logged stage 1", "Twice Logged", "Lightly Logged", "Primary"), "SR_B3_red"]
t.test(x, y, alternative = "less", var.equal = FALSE, conf.level = 0.99)



# Results:
# Prim vs Light : all canopy structure NOT different (p > 0.01)
# Prim vs Twice : all canopy structure NOT different (p > 0.01)
# Light vs Twice : all canopy structure NOT different (p > 0.01)
# Salvage1 < (Prim, Light, Twice) MEANS : all significant (p > 0.01)
# Salvage1 vs (Prim, Light, Twice) VARIANCE : 











# # (1) Get pairs of factors
# allPairs <- expand.grid(levels(stats.data.use$Type.new), levels(stats.data.use$Type.new), stringsAsFactors = FALSE)  %>%
#   filter(Var1 != Var2) %>%
#   mutate(key = paste0(pmin(Var1, Var2), pmax(Var1, Var2), sep='')) %>%
#   distinct(key) %>%
#   select(-key)
# 
# 
# 
# # (2) Do Welch-corrected ANOVA for each pair
#    










