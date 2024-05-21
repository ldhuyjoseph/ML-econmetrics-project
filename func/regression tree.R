
# Helper packages
library(dplyr)       # for data wrangling
library(ggplot2)     # for awesome plotting
library(rsample)     # data splitting 
library(fastDummies) # for creating dummy variables

# Modelling packages
library(rpart)       # package for regression/decision tree estimation
library(gbm) # package for gradient boosting estimation
library(h2o)       # package for training ML models
library(caret)  # another package for training and evaluation ML models

# Model interpretability packages
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance
library(pdp)         # for feature effects

# Helper packages
library(dplyr)       # for data wrangling
library(ggplot2)     # for awesome plotting
library(rsample)     # data splitting 
library(fastDummies) # for creating dummy variables

# Modelling packages
library(rpart)       # package for regression/decision tree estimation
library(gbm) # package for gradient boosting estimation
library(h2o)       # package for training ML models
library(caret)  # another package for training and evaluation ML models

# Model interpretability packages
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance
library(pdp)         # for feature effects

# Helper packages
library(dplyr)       # for data wrangling
library(ggplot2)     # for plotting
library(rsample)     # data splitting 
library(fBasics)    # descriptive statistics

# Modeling packages
library(glmnet) # for lasso estimation
library(hdm) # for rigorous and double lasso estimation

reg_tree<-function(subset_share_fb, y_share_report_fb_num ,method, c){
  
# Set a seed for reproducibility
set.seed(123)
# Split the data into training and testing sets
n <- nrow(subset_share_fb)
split <- sample(1:n, floor(n*7/10))
subset_train <- subset_share_fb[split, ]
subset_test <- subset_share_fb[-split, ]

tree <- rpart(formula = y_share_report_fb_num ~., #x_age+x_sex_num+x_education_num+y_belief_report_num+d_treatment_source,
              data    = subset_train,
              cp=0.01, method=method)  # use method="class" for classification # cp=0.01 #method='anova'
# check rpart.control for parameter tuning
# summary(tree) # you can use this function to check the details of the tree
rpart.plot(tree )  

cpt <- tree$cptable
cpt <- as.data.frame(cpt)
cp.min <- which.min(cpt$xerror) # find minimum error
cp.idx <- which(cpt$xerror- cpt$xerror[cp.min] < cpt$xstd)[1]  # at most one std. error from minimum error
cp.best <-cpt$CP[cp.idx]
cp.best

# Prune the tree
pruned.tree <- prune(tree, cp=cp.best)
rpart.plot(pruned.tree )

y.hat <- predict(pruned.tree, newdata = subset_train)
num.leaves <- length(unique(y.hat))

# Leaf membership, ordered by increasing prediction value
leaf <- factor(y.hat, ordered = TRUE, labels = seq(num.leaves))

add_col <- dummy_cols(subset_train[,2], remove_selected_columns = T) # transform qualitative var in a set of dummy variables
descriptive <- cbind(subset_train[,c(-1)], add_col, leaf)

p1 <- descriptive %>% group_by(leaf) %>%  summarise_all( ~ mean(.x, na.rm = TRUE))
p1

vip(pruned.tree, target = subset_train[,1], metric = "rmse")

partial(pruned.tree, pred.var = subset_train[,c], plot = TRUE)

#partial(pruned.tree, pred.var = c("Gr_Liv_Area","Overall_Qual"), plot = TRUE)
}
