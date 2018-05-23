require(nlme)
require(multcomp)
require(lme4)
require(emmeans)

df<-read.csv("highRT.csv")
df$Subject<-factor(df$Subject)
options(contrasts=c("contr.sum","contr.poly"))

###anova on rt
summary(aov(X ~ Task*Freq*Site + Error(Subject/(Task*Freq*Site)), data=df))

#anova model using lme4 linear regression, so we can use emmeans to do posthoc test
model <-lmer(X ~ Task*Freq*Site + (1|Subject)+(1|Task:Subject)+(1|Freq:Subject)+(1|Site:Subject)+(1|Task:Freq:Subject)+(1|Task:Site:Subject)+(1|Freq:Site:Subject), data = df)

# test freq by site interaction
emm <- emmeans(model,  pairwise ~ Freq | Site ) # freq conditioned by site, averaged across task
#test cell mean and contrast with tukey adjusted t value (ignore the dof)
test(emm, combine=TRUE, adjust="tukey", df=15)

#results
# $emmeans
# Site = FEF:
#  Freq        emmean          SE df t.ratio p.value
#  Beta  -0.004271875 0.008878695 15  -0.481  0.8685
#  Gamma  0.030334375 0.008878695 15   3.417  0.0076

# Site = IPS:
#  Freq        emmean          SE df t.ratio p.value
#  Beta  -0.009384375 0.008878695 15  -1.057  0.5201
#  Gamma -0.001353125 0.008878695 15  -0.152  0.9858

# Results are averaged over the levels of: Task 
# Degrees-of-freedom method: satterthwaite 
# P value adjustment: sidak method for 2 tests 

# $contrasts
# Site = FEF:
#  contrast        estimate         SE df t.ratio p.value
#  Beta - Gamma -0.03460625 0.01051824 15  -3.290  0.0050

# Site = IPS:
#  contrast        estimate         SE df t.ratio p.value
#  Beta - Gamma -0.00803125 0.01051824 15  -0.764  0.4570

# Results are averaged over the levels of: Task 




###  ANOVA on response bias, Need to check response bias, results doesn't match with manuscript
df<-read.csv("highRespBias.csv")
df$Subject<-factor(df$Subject)
summary(aov(X ~ Task*Freq*Site + Error(Subject/(Task*Freq*Site)), data=df))

model <-lmer(X ~ Freq*Site + (1|Subject)+(1|Freq:Subject)+(1|Site:Subject), data = df)

emm <- emmeans(model,  pairwise ~ Freq | Site ) # freq by site interaction
test(emm, combine =TRUE, adjust="tukey", df=15)

#results
# $emmeans
# Site = FEF:
#  Freq     emmean         SE df t.ratio p.value
#  Beta  0.4741125 0.03828339 15  12.384  <.0001
#  Gamma 0.5960875 0.03828339 15  15.570  <.0001

# Site = IPS:
#  Freq     emmean         SE df t.ratio p.value
#  Beta  0.5277500 0.03828339 15  13.785  <.0001
#  Gamma 0.5472125 0.03828339 15  14.294  <.0001

# Degrees-of-freedom method: satterthwaite 
# P value adjustment: sidak method for 2 tests 

# $contrasts
# Site = FEF:
#  contrast       estimate         SE df t.ratio p.value
#  Beta - Gamma -0.1219750 0.04757088 15  -2.564  0.0216

# Site = IPS:
#  contrast       estimate         SE df t.ratio p.value
#  Beta - Gamma -0.0194625 0.04757088 15  -0.409  0.6882




####### accuracy ANOVA
df<-read.csv("highSearchAcc.csv")
df$Subject<-factor(df$Subject)
summary(aov(X ~ Task*Freq*Site + Error(Subject/(Task*Freq*Site)), data=df))

#anova model using lme4 linear regression, so we can use emmeans to do posthoc test
model <-lmer(X ~ Task*Freq*Site + (1|Subject)+(1|Task:Subject)+(1|Freq:Subject)+(1|Site:Subject)+(1|Task:Freq:Subject)+(1|Task:Site:Subject)+(1|Freq:Site:Subject), data = df)
anova(model)


# test freq by site interaction
emm <- emmeans(model,  pairwise ~ Freq | Task ) # freq conditioned by site, averaged across task 
#test cell mean and contrast with tukey adjusted t value (ignore the dof)
test(emm, combine =TRUE, adjust="tukey", df = 15)

#Results
# $emmeans
# Task = CJ:
#  Freq        emmean         SE df t.ratio p.value
#  Beta  -0.061053125 0.01837362 15  -3.323  0.0093
#  Gamma  0.031534375 0.01837362 15   1.716  0.2020

# Task = PO:
#  Freq        emmean         SE df t.ratio p.value
#  Beta  -0.011953125 0.01837362 15  -0.651  0.7745
#  Gamma -0.008096875 0.01837362 15  -0.441  0.8883

# Results are averaged over the levels of: Site 
# Degrees-of-freedom method: satterthwaite 
# P value adjustment: sidak method for 2 tests 

# $contrasts
# Task = CJ:
#  contrast        estimate         SE df t.ratio p.value
#  Beta - Gamma -0.09258750 0.02454285 15  -3.772  0.0018

# Task = PO:
#  contrast        estimate         SE df t.ratio p.value
#  Beta - Gamma -0.00385625 0.02454285 15  -0.157  0.8772

# Results are averaged over the levels of: Site 



###  ANOVA on search bias, Need to check response bias, results doesn't match with manuscript
df<-read.csv("highSearchBias.csv")
df$Subject<-factor(df$Subject)
summary(aov(X ~ Freq*Site + Error(Subject/(Freq*Site)), data=df))

model <-lmer(X ~ Freq*Site + (1|Subject)+(1|Freq:Subject)+(1|Site:Subject), data = df)

emm <- emmeans(model,  pairwise ~ Freq) # freq by site interaction
test(emm, combine =TRUE, adjust="tukey", df=15)


