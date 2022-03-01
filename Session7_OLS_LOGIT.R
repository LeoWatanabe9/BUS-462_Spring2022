############################################################
# BUS 462 | Spring 2022 | Session 7
# OLS & LOGIT REGRESSIONS ON CHIMERA DATA
# CK 
# 01 MAR 2022
############################################################

#### PREAMBLE : ## Clearing mem buffers ####
  cat("\014")  # Clear Console
  rm(list = ls(all.names = TRUE))# clear all data objects
  gc() # clear memory
  set.seed(42) # Set a seed to ensure repeatable random samples

# libraries
  require(data.table)
  require(stargazer)
  require(ggplot2)
  require(PerformanceAnalytics)

#LOAD DATA  ###
  dt <- fread("C:/Users/CK/Dropbox/lecture slides/CK Spring 2022/Assignments/chimera_2022.csv")

# Check data
  str(dt) # check the structure
  head(dt)


###########################################

# Let's Model exit as a DV

  # DV: Exit
  # IV: Boss Survey, job satisfaction, rank, salary 

  # let's create a smaller data subset keeping only the variables we need. 
  dt1 <- dt[,c("exit", "boss_survey","rank","job_satisfaction","salary")]
  head(dt1)

  # check if analyzable -- any missing data ?
  head(dt1) # see first 6 rows of data 
  tail(dt1) # see the last 6 rows of data
  str(dt1) # see structure of each variable
  # Notice all the variables are either num or int -- this is okay. We can convert to factors later

  table(dt1$exit,dt1$rank) # see if exits and ranks are well distributed
  xtabs(~exit + rank, data = dt1) # alternate way of seeing it / with col and row names
  
  # summary stats
  stargazer(dt1,type="text",summary.stat = c("min", "p25", "median","mean", "p75", "max","sd"))
  # note that gender, rank, and exit are categorical variables - we can convert to factors later


#histograms, scatterplots, and correlation matrix
  chart.Correlation(dt1, histogram=TRUE, pch=19) # get's busy

  
# OLS - let's first make an OLS model on the data subset
  m.OLS <- lm(exit ~ ., data=dt1)
  summary(m.OLS)
  AIC(m.OLS) # calculate AIC for this model, so we can compare against LOGIT

  stargazer(m.OLS,type="text")

  # what is this model telling you about the dynamics between variables ?
  # Q: can you interpret effect of the change in value of an IV against the DV?
  # Ans: NOT really, hence we need LOGIT
  
  
# LOGIT - let's now make a LOGIT model(s)
  # First let's do a LOGIT without converting categorical variables into factors
  m.LOGIT.1 <- glm(exit ~ ., data = dt1, family = "binomial")
  summary(m.LOGIT.1)
  
  # Second, let's convert categorical variables into factors
  dt1$exit <- as.factor(dt1$exit)
  dt1$rank <- as.factor(dt1$rank)
  str(dt1) # check the structure
  
  #modeling this as a logit with factors
  m.LOGIT.2 <- glm(exit ~ ., data = dt1, family = "binomial")
  summary(m.LOGIT.2)

  # to easily get a McFadden's pseudo R2 for a fitted model in R, use the "pscl" package 
  #  http://cran.r-project.org/web/packages/pscl/index.html
  library(pscl)
  pR2(m.LOGIT.1) # this shows you the pseudoR2 for the logit model 1
  pR2(m.LOGIT.2) # this shows you the pseudoR2 for the logit model 2
  # It's the same - coz the mechanics of LOGIT are the same,
  
# Comparing OLS, LOGIT and LOGIT w. factors

  stargazer(m.OLS, m.LOGIT.1,m.LOGIT.2,type="text")
  pR2(m.LOGIT.2) # this shows you the pseudoR2 for the logit model 2
  AIC(m.OLS) # to find AIC for the OLS model
  
  
  # pseudo r-squared is higher, even though AIC is lower
  # remember - like in all else in stats, comparing AIC's are no good out of context.
  # plus DV is binary => LOGIT is better to interpret 
  
  # interpretation of LOGIT MODEL
  exp(cbind(OR = coef(m.LOGIT.2), confint(m.LOGIT.2)))

   
  # Let's look at job satisfaction
  # recall  the standard interpretation of log-odds and odds.
    
  # log odds: 
  summary(m.LOGIT.2)
  stargazer(m.LOGIT.2,type="text")
  # both give the same result - the coefficient of job satisfaction is ~ -0.4
#-> This means every unit increase in job satisfaction reduces the log-odds of exit by -0.4
# -> implies every unit increase in job satisfaction reduces the odds of exit by exp(-0.4) or 0.67
# -> Recall conversion of odds (or odds ratio) to probability
  # -> if the Odds are 0.67, it implies odds ratio is 0.67:1
  # -> to calculate probability, you tale 0.67 / (1+0.67) == 40% probability 
  # => increase in job satisfaction by one unit reduces (see the -ve coefficient) prob. of exit by 40%
  
  
############################
  
