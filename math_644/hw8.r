#!/usr/bin/env Rscript
printf = function(fmt, ..., file = ""){cat(sprintf(fmt,...), file = file)}
puts = function(...){cat(sprintf("%s",...))}
options(show.signif.stars=F)
roadway.temps = read.csv("roadway.csv")
roadway.lm = lm(Temp ~ Deflection, data = roadway.temps)
roadway.summary = summary(roadway.lm)
roadway.anova = anova(roadway.lm)
## check for outliers
outlier.limits = c((mean(roadway.temps[[1]])-3*sd(roadway.temps[[1]])),
                   (mean(roadway.temps[[1]])+3*sd(roadway.temps[[1]])))
outliers = Filter(function(x){x< outlier.limits[1] || x >outlier.limits[2]},
                  roadway.temps[[1]])
## outliers is empty
png("roadway_scatter.png")
plot(roadway.temps) #scatter plot
abline(roadway.lm) #fit line
invisible(dev.off())
puts("Summary of roadway data linear regression\n")
print(roadway.summary) #regression coefficents + other info
puts("\nConfidence interval for regression coefficents\n")
confint(roadway.lm, level=0.95)
puts("\n")
## t test for slope, formula for t value is:
## (B_0 - B_h0)/SE_b0 (i.e the slope - the value the null hypothesis
## predicts the slope will be, over the standard error of the slope
t = roadway.summary$coeff[2,][3]
p.t = roadway.summary$coeff[2,][4] # = 2*(1-pt(abs(t)))
## for the whole model we use an F test, the test stastic is:
## F = MS_regression/MS_err
F = roadway.anova$Mean[1]/roadway.anova$Mean[2]
p.f = roadway.anova$Pr[1] #= 1-pf(F)
puts("Anova table for roadway data\n")
print(roadway.anova)
puts("\n")


mlr = read.csv("mlr.csv");
mlr.lm = lm(y ~ x1 + x2 + x3 + x4, data = mlr);
mlr.anova = anova(mlr.lm)
puts("Correlation matrix for mlr data\n")
cor(mlr) #prints corrlation matrix
puts("\nAnova table for mlr data\n")
print(mlr.anova)
## This can be done programmaticly but the way R does it (the step function)
## Uses a method we haven't talked about in class, and I'm not going to
## write my own version

mlr.model1 = mlr.lm
puts("\nSummary of mlr data multiple regression initial model\n")
summary(mlr.model1)
mlr.model2 = update(mlr.model1,.~.-x4) #remove x4, the least significant value
puts("\nSummary of mlr data multiple regression 2nd model\n")
summary(mlr.model2)
mlr.model3 = update(mlr.model2,.~.-x3) #remove the next least significant value
puts("\nSummary of mlr data multiple regression 3rd model\n")
summary(mlr.model3)
mlr.model4 = update(mlr.model2,.~.-x3) #remove the next least significant value
puts("\nSummary of mlr data multiple regression 4th model\n")
summary(mlr.model4)
# model4 is worse than model 3, so model 3 is the best we can do
png("mlr_residuals.png")
par(mfrow=c(2,2))
plot(mlr.model3)
invisible(dev.off())
