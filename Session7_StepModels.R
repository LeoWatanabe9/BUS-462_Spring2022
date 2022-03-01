############################################################
# BUS 462 | Spring 2022 | Session 7
# STEP MODELS ON CHIMERA DATA
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



# First, let's do a kitchen sink (KS)
m.OLS.KS <- lm(exit ~ ., data=dt)

# 25 variables!
# even if theory guides you, that's a lot of variables to consider.
# one option / shortcut is to use step models

# STEP MODELS --# One way to deal with a large # of columns

# step models essentially automates the process of adding variables,
# .... or removing one variables from the kitchen sink and 
# .... seeing if the model improves (vis-a-vis R2 and AIC)

# STEP function can be forward (adding 1 var at a time)...
# ... or backward (removing 1 var from Kitchen Sink)...
#.... or both.... which is default.

# Let's talk about STEP models and why they are  useful  / dangerous
# these can be used in any regression

?step() # help on step function

# running step model on Chimera's KS model 
model.step.OLS <- step(m.OLS.KS) # step model on chimera KS model 
#recall - default is direction== both

# let's compare OLS with step!
stargazer(m.OLS.KS,model.step.OLS,type="text")
# see how variables are selected with the same Adj R2!
# let's compare AIC!
AIC(model.step.OLS)
AIC(m.OLS.KS)
# STEP has reduced AIC, has the same Adj R2, but has only 7 variables in the model!

##################
# BUT CAUTION
################
#1.  Stepwise regression takes care of control variables, but not interactions!

#2. Stepwise  regression doesn't care of theory -- can result in SPURIOUS outputs!

# e.g.: stepwise model shows # Half_day_leaves explain exits ?!  Why? How?

# careful about such spurious outputs.
# step function is very useful, but use with caution.
# Ideally, use this in conjunction with model building we have learnt.

##############################
## COMPARE Backwards, Forwards and both in stepwise
###########################

model.step.OLS.f <- step(m.OLS.KS,direction = 'forward')
model.step.OLS.b <- step(m.OLS.KS,direction = 'backward')
model.step.OLS <- step(m.OLS.KS,direction = 'both')

# compare models!
stargazer(m.OLS.KS,model.step.OLS.f,model.step.OLS.b,model.step.OLS,type="text")


#########################
## Stepwise in LOGIT!
#########################
# of course, this can be used in logit!

# first, the kitchen sink (KS) LOGIT
m.logit.KS <- glm(exit ~ ., data=dt,family = "binomial") 

# NEXT, the stepwise for logit!
model.step.Logit <- step(m.logit.KS)

# let's compare the two
stargazer(m.logit.KS,model.step.Logit,type="text")

# compare OLS and Logit STEPWISE
stargazer(model.step.OLS, model.step.Logit,type="text")

# again, half day leaves predicting exit?!
# maybe whatever is causing half-day leaves is also causing exits...?
# but stepwise without a stong theory can be spurious.


## ML uses stepwise and becomes black-boxy -- you can't EXPLAIN the model!


############################