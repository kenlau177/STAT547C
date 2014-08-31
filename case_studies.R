library(leaps)
library(glmnet)
library(BayesVarSel)
library(xtable)

#setwd("C://Users//Ken//Dropbox//MSC1_Fall//stat547C//project")
setwd("C://Users//Ken//Desktop//School//MSC1_Fall//stat547C//project")

#number of observations
n=350
#generate important predictors
x1 = rnorm(n=n, mean=2, sd=1)
x2 = rnorm(n=n, mean=3, sd=1)
x3 = rnorm(n=n, mean=1, sd=1)
x4 = rnorm(n=n, mean=.5, sd=1)
#response variable
y = x1*3.2 + x2*1.8 + x3 + x4*2.5 + rnorm(n=n, sd=.5)

##add good predictors but not optimal#
x5 = x1 + x2 + rnorm(n=n, sd=.5)
x6 = x1 + x3 + rnorm(n=n, sd=.5)
x7 = x1 + x4 + rnorm(n=n, sd=.5)
x8 = x2 + x3 + rnorm(n=n, sd=.5)
x9 = x2 + x4 + rnorm(n=n, sd=.5)
x10 = x3 + x4 + rnorm(n=n, sd=.5)
x11 = x1 + x2 + x3 + rnorm(n=n, sd=.5)
x12 = x1 + x2 + x4 + rnorm(n=n, sd=.5)
x13 = x1 + x3 + x4 + rnorm(n=n, sd=.5)
x14 = x2 + x3 + x4 + rnorm(n=n, sd=.5)
##construct covariate matrix
x = data.frame(y=y, x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, x6=x6, x7=x7, 
      x8=x8, x9=x9, x10=x10, x11=x11, x12=x12, x13=x13, x14=x14)

#save simulated data
#write.table(x, "x.txt", row.names=F, col.names=T, sep=",")
#read in simulated data
x = read.table("x.txt", header=T, sep=",")
#test = read.table("test.txt", header=T, sep=",")

#forward stepwise
#call forward stepwise
out_step = step(lm(y~1, data=x), list(upper=lm(y~., data=x)), 
            direction="forward")
#extract out selected variables
coef_step = names(out_step$coefficients)[-1]
coef_step
#[1] "x12" "x13" "x5"  "x7"  "x11" "x4"  "x1"  "x2"  "x3" 

#lasso
#extract predictor X matrix
X = data.matrix(x[,-match("y",names(x))])
#extract response variable
Y = data.matrix(x[,match("y",names(x)),drop=F])
#call cross validation in lasso to find optimal lambda
cv_lasso = cv.glmnet(x=X, y=Y, alpha=1)
out_lasso = cv_lasso$glmnet.fit
#extract selected variables and respective coefficients
coef_lasso = out_lasso$beta[,which.min(cv_lasso$cvm)]
coef_lasso
#        x1         x2         x3         x4         x5         x6 
#2.87020970 1.48168827 0.75196938 2.18357433 0.11383759 0.01381382 
#x7         x8         x9        x10        x11        x12 
#0.08840575 0.03285751 0.06363527 0.03374477 0.03813501 0.01257526 
#x13        x14 
#0.06746695 0.04644490

#gibbs
#call gibbs variable selection
out_gibbs = GibbsBvs(y~., data=x, prior.betas="Robust", 
              prior.models="Constant", n.burnin=300, n.iter=20000)
#outputs a table with information on selected variables
coef_gibbs = out_gibbs$HPMbin
coef_gibbs = cbind(coef_gibbs, names(x)[-1])
names(coef_gibbs) = c("bin.mod", "coef")
#extract selected variables
coef_gibbs = as.character(coef_gibbs$coef[coef_gibbs$bin.mod==1])
coef_gibbs
#[1] "x1"  "x2"  "x4"  "x14"

#lm_step = glm(y~., family=gaussian, data=x[,c("y",coef_step)])
#pr_lm_step = predict(lm_step, newdata=test, type="response")
#mean((test[,"y"]-pr_lm_step)^2)

#newx = data.matrix(test[,-match("y",names(test))])
#pr_lasso = predict(cv_lasso, newx=newx, s="lambda.min")
#mean((test[,"y"]-pr_lasso)^2)

#lm_gibbs = glm(y~., family=gaussian, data=x[,c("y",coef_gibbs)])
#pr_gibbs = predict(lm_gibbs, newdata=test, type="response")
#mean((test[,"y"]-pr_gibbs)^2)

### Starcraft data set ###

starcraft = read.table("SkillCraft1_Dataset.csv", sep=",", header=T, 
              stringsAsFactors=T, na.strings="?")
starcraft = starcraft[,grep("^((?!gameid).)",names(starcraft),
              ignore.case=T,perl=T)]
starcraft = starcraft[complete.cases(starcraft),]

##forward stepwise
out_step = step(lm(LeagueIndex~1, data=starcraft), 
            list(upper=lm(LeagueIndex~., data=starcraft)), 
            direction="forward", trace=0)
coef_step = names(out_step$coefficients)[-1]
coef_step
#[1] "ActionLatency"    "APM"              "AssignToHotkeys" 
#[4] "MinimapAttacks"   "GapBetweenPACs"   "WorkersMade"     
#[7] "NumberOfPACs"     "HoursPerWeek"     "SelectByHotkeys" 
#[10] "UniqueHotkeys"    "TotalMapExplored" "Age"             
#[13] "ActionsInPAC"     "ComplexUnitsMade" "UniqueUnitsMade"

#lasso
X = data.matrix(starcraft[,-match("LeagueIndex",names(starcraft))])
Y = data.matrix(starcraft[,match("LeagueIndex",names(starcraft))])
cv_lasso = cv.glmnet(x=X, y=Y, alpha=1)
out_lasso = cv_lasso$glmnet.fit
coef_lasso = out_lasso$beta[,which.min(cv_lasso$cvm)]
coef_lasso
#     Age         HoursPerWeek           TotalHours 
#0.000000000          0.000000000          0.000000000 
#APM      SelectByHotkeys      AssignToHotkeys 
#0.005411143          0.000000000        572.435551051 
#UniqueHotkeys       MinimapAttacks   MinimapRightClicks 
#0.000000000        117.481302610          0.000000000 
#NumberOfPACs       GapBetweenPACs        ActionLatency 
#62.543716813         -0.004358252         -0.021668480 
#ActionsInPAC     TotalMapExplored          WorkersMade 
#0.000000000          0.000000000          0.000000000 
#UniqueUnitsMade     ComplexUnitsMade ComplexAbilitiesUsed 
#0.000000000          0.000000000          0.000000000 

#gibbs
out_gibbs = GibbsBvs(LeagueIndex~., data=starcraft, 
              prior.betas="Robust", prior.models="Constant", 
              n.burnin=300, n.iter=20000)
coef_gibbs = out_gibbs$HPMbin
coef_gibbs = cbind(coef_gibbs, names(starcraft)[-1])
names(coef_gibbs) = c("bin.mod", "coef")
coef_gibbs = as.character(coef_gibbs$coef[coef_gibbs$bin.mod==1])
coef_gibbs
#[1] "Age"                "AssignToHotkeys"    "UniqueHotkeys"     
#[4] "MinimapAttacks"     "MinimapRightClicks" "ActionsInPAC"      
#[7] "TotalMapExplored"   "WorkersMade"   

r1 = c(coef_step, "MinimapRightClicks", "HoursPerWeek", "TotalHours", 
       "ComplexAbilitiesUsed")
r2 = c(rep(1,15),0,0,0,0)
r3 = c(1,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
r4 = c(0,0,1,1,0,1,0,0,0,1,1,1,1,0,0,1,0,0,0)
r5 = c(1,1,1,0,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0)
sc_table = cbind(r1,r2,r3,r4,r5)
sc_table = data.frame(sc_table)
names(sc_table) = c("variable", "stepwise", "lasso", "Gibbs", "Thompson")

sc_table_tex = xtable(sc_table, align="clcccc")
print(sc_table_tex, include.rownames=F)







