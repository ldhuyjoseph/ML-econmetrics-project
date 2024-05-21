#this script is from regularization exercise (Lasso, post Lasso, rlasso, double lasso)

# Helper packages
library(dplyr)       # for data wrangling
library(ggplot2)     # for plotting
library(rsample)     # data splitting 
library(fBasics)    # descriptive statistics

# Modeling packages
library(glmnet) # for lasso estimation
library(hdm) # for rigorous and double lasso estimation

#1st func, for full lm, to compare with simple

full_lm<- function(){
  # ADD YOUR CODE HERE
  h1a_lm_full <- lm(y_belief_report_num ~ .,data=subset_belief)
  h1b_email_lm_full <- lm(y_share_report_email_num ~.,data=subset_share_email)
  h1b_fb_lm_full <- lm(y_share_report_fb_num ~ .,data=subset_share_fb)
  h1b_twitter_lm_full <- lm(y_share_report_twitter_num ~ .,data=subset_share_twitter)
  h1b_whatsapp_lm_full <- lm(y_share_report_whatsapp_num ~ .,data=subset_share_whatsapp)
  
  #plot
  plot <-stargazer(h1a_lm_full,type='text')
  plot <-stargazer(h1b_email_lm_full,type='text')
  plot <-stargazer(h1b_fb_lm_full,    type='text')
  plot <-stargazer(h1b_whatsapp_lm_full,    type='text')
  plot <-stargazer(h1b_twitter_lm_full,       type='text')
  
  
  #hetereskedasticity control
  h1_full_results_robust <- rep(list(NULL), 5)
  objects <- c(
    "h1a_lm_full",
    "h1b_email_lm_full",
    "h1b_fb_lm_full",
    "h1b_whatsapp_lm_full",
    "h1b_twitter_lm_full"
  )
  names(h1_full_results_robust) <- objects
  
  for (i in objects) {
    assign("fit.i", get(i))
    cov_mat.i <- sandwich::vcovHC(fit.i, type = "HC1") #originally, HC1, let's try CR1
    h1_full_results_robust[[i]] <- lmtest::coeftest(fit.i, vcov = cov_mat.i)
  }
  h1_full_results_robust
  
}

## 2nd func, for lasso-plasso,rlasso

lasso_plasso_rlasso<-function(X_belief,y_belief_report_num,subset_belief){
  #Create a grid of value for lambda varying between $10^{1}$ to $10^{-10}$. 
  #Use the seq() function to create a sequence of values
  grid=10^seq(1,-10,length=100)
  
  #cat('grid')
  #grid
  
  #plot grid
  #print('plot grid')
  plot(grid,type = 'l')
  
  ##lasso
  lasso.mod=glmnet(X_belief,y_belief_report_num,alpha=1,lambda=grid)
  cat('dim(coef(lasso.mod):',dim(coef(lasso.mod)),'\n\n')
  
  cat('coef(lasso.mod)[,1]:',coef(lasso.mod)[,1],'\n')
  #coef(lasso.mod)[,1] # coefficients for a very large value of lambda
  cat('coef(lasso.mod)[,100]:',coef(lasso.mod)[,100],'\n\n')
  #coef(lasso.mod)[,100] # coefficients for a very small value of lambda
  #grid[1]
  #grid[100]
  
  print('plot log(grid) and coef(lasso.mod)[d,)\n\n')
  plot( log(grid), coef(lasso.mod)['d_treatment_source',] , type="l") # print in logs for easiness of interpretation
  
  plot(log(grid), lasso.mod$dev.ratio, type="l")
  
  options(scipen=999)
  set.seed(1234)
  lasso.cv = cv.glmnet(X_belief, y_belief_report_num, alpha=1) # does CV on glmnet. Type ?glmnet for help. alpha=1 for lasso regression
  
  cat('lasso.cv$lambda.min:',lasso.cv$lambda.min,'\n')
  cat('log(lasso.cv$lambda.min):',log(lasso.cv$lambda.min),'\n\n')
  #lasso.cv$lambda.min # note: another popular choice is to take lasso.cv$lambda.1se: largest value of lambda such that error is within 1 standard error of the cross-validated errors for lambda.min
  #log(lasso.cv$lambda.min)
  
  # now use glmnet using the optimal lambda
  lasso.opt <-glmnet(X_belief, y_belief_report_num, alpha=1, lambda = lasso.cv$lambda.min)
  lasso.opt$beta
  
  print('refer to plot(lasso.cv)\n\n')
  plot(lasso.cv)
  
  ##Post Lasso
  c <- lasso.opt$beta
  inds<-which(c!=0)
  variables<-row.names(c)[inds]
  optX<-X_belief[,variables]
  postlasso=lm(y_belief_report_num ~ optX)
  
  print('summary of PLasso\n')
  summary(postlasso)
  
  ##RLasso
  set.seed(1234)
  Single_Selection <- rlasso(y_belief_report_num ~ ., data=subset_belief, post=T, penalty = list(homoscedastic = T))
  summary(Single_Selection)
  Single_Selection_het <- rlasso(y_belief_report_num ~ ., data=subset_belief, post=T, penalty = list(homoscedastic = F))
  
  print('RLasso without forcing:\n')
  summary(Single_Selection_het)
  
  
#   ## to fix this formula
#   # try post lasso where we force the variable gdpsh465 in the formula:
#   if (dim(subset_belief)[2]>6) {
#     selected = which(coef(Single_Selection)[-c(2,7)] !=0) # = Select relevant variables = #
#     colnames(subset_belief)[2] <-'y'
#     formula = paste(c("y ~ d_treatment_source", names(selected)), collapse = "+")
#   }
#   
#   if (dim(subset_belief)[2]==6){
#   selected = which(coef(Single_Selection)[-c(1,6)] !=0) # = Select relevant variables = #
#   formula = paste(c("y_belief_report_num ~ d_treatment_source", names(selected)), collapse = "+")
#   
#   }
#   
#   #formula = paste(c("y_belief_report_num ~ d_treatment_source", names(selected)), collapse = "+")
#   
#   print('RLasso with forcing')
#   summary(lm(formula, data = subset_belief))
}

## 3rd func, for double lasso
D <- subset_belief[,6] # target regressor
W <- subset_belief[,c(-1,-6)] # exogenous variables
set.seed(1234)
# double_selection = rlassoEffect(x = W, y = subset_belief[,1], d= D, method = "double selection")
# summary(double_selection)
# double_selection$coefficients.reg
# 
# cbind(summary(model_het[1,]), summary(double_selection))