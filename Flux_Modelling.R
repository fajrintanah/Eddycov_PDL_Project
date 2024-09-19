
# Modelling : using non-gap filled data

library(tidyverse) # plotting and manipulation
library(grid) # combining plots
library(gridExtra) # combining plots
library(ggpubr) # combining plots
library(patchwork) # combining plots
library(ggfortify) # nice extension for ggplot
library(mgcv) #fitting gam models
library(GGally) # displaying pairs panel
library(caret)
library(caTools) # split dataset
library(readxl)
library(randomForest)
library(e1071)
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(caret)        # an aggregator package for performing many machine learning models
library(pdp)          # model visualization
library(lime)         # model visualization
library(neuralnet)
library(rpart)     #rpart for computing decision tree models
library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(broom)
library(ranger) 	#efficient RF
library(NeuralNetTools)
library(tidymodels)
library(earth) 		#MARS model
library(iml)		#most robust and efficient relative importance 
library(xgboost)	#extreeme gradient boosting
library(ModelMetrics) #get model metrics
library(Metrics) 	#get ML model metrics
library(Cubist) #Cubist modell
library(iBreakDown)
library(DALEX)
library(viridis)
library(ICEbox)
library(hrbrthemes)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(vip)
library(fastDummies)
library(corrplot)

# quantitavely selecting column, removing NAs
model_EC <- eddy_testqc4 %>% 
  filter(daytime != 0) %>% # add this line to filter out rows where daytime == 0
  select(co2_flux2, H, LE, RG_1_1_1, TA, PPFD, VPD, ustar, RH_1_1_1, TS, SWC) %>% 
  drop_na() %>% # remove NA values
  rename(
    NEE = co2_flux2,
    Rg = RG_1_1_1, 
    Tair = TA,
    rH = RH_1_1_1,
    Tsoil = TS
  ) 
							
str(model_EC )
		tibble [1,575 × 11] (S3: tbl_df/tbl/data.frame)
		 $ NEE  : num [1:1575] 0.019 0.0273 0.0107 0.0261 0.0101 ...
		 $ H    : num [1:1575] -24.24 -12.12 -5.36 -8.67 -11.57 ...
		 $ LE   : num [1:1575] -24.24 -12.12 -5.36 -8.67 -11.57 ...
		 $ Rg   : num [1:1575] 205.3 61.9 69.4 19.5 -176.4 ...
		 $ Tair : num [1:1575] 26.7 26 25.8 25.4 25 ...
		 $ PPFD : num [1:1575] 465.4 151.2 158.2 45.3 0 ...
		 $ VPD  : num [1:1575] 0 0 0 0.245 33.278 ...
		 $ ustar: num [1:1575] 0.166 0.607 0.529 0.186 0.223 ...
		 $ rH   : num [1:1575] 95.4 92.5 93.9 93.9 95.4 ...
		 $ Tsoil: num [1:1575] 29.4 29.6 29.7 29.6 29.5 ...
		 $ SWC  : num [1:1575] 0.435 0.432 0.431 0.428 0.425 ...


## Detecting multicollinearity using correlation coefficient
corr_EC <- cor(model_EC)

# graph correlation specific columns
graph_corr_EC <- corrplot(corr_EC,
         method="color", addCoef.col = "black", 
         col = viridis(100),
         tl.col = "black")


# re-selecting colum
model_EC <- eddy_testqc4 %>% 
  filter(daytime != 0) %>% # add this line to filter out rows where daytime == 0
  select(co2_flux2, PPFD, VPD, ustar, RH_1_1_1, TS, SWC) %>% 
  drop_na() %>% # remove NA values
  rename(
    NEE = co2_flux2,
    rH = RH_1_1_1,
    Tsoil = TS
  ) 
	 


	 	 
# Set the seed for reproducibility
set.seed(123)	
					
# Split the data into training and validation sets (I prefer to use dplyr/tidyverse style, my beloved wife )
split_model_EC <- initial_split(model_EC, prop = 0.7)
trainSet_model_EC <- training(split_model_EC)
testSet_model_EC <- testing(split_model_EC)
	
model_EC_train <- trainSet_model_EC %>%
  mutate(Status = "Trainingmodel_EC")
model_EC_test <- testSet_model_EC %>%
  mutate(Status = "Validationmodel_EC")
model_EC_df <- bind_rows(model_EC_train, model_EC_test)

str(model_EC_df)		 
		tibble [1,667 × 8] (S3: tbl_df/tbl/data.frame)
		 $ NEE   : num [1:1667] -0.010516 -0.015253 -0.000515 0.011686 -0.014574 ...
		 $ PPFD  : num [1:1667] 422 1335 147 0 1484 ...
		 $ VPD   : num [1:1667] 433 2223 733 0 2087 ...
		 $ ustar : num [1:1667] 0.1578 0.1438 0.0815 0.1294 0.2839 ...
		 $ rH    : num [1:1667] 99.9 68.3 85.5 99.9 65.9 ...
		 $ Tsoil : num [1:1667] 27.5 29.3 29.7 28.5 29.2 ...
		 $ SWC   : num [1:1667] 0.364 0.34 0.395 0.354 0.376 ...
		 $ Status: chr [1:1667] "Trainingmodel_EC" "Trainingmodel_EC" "Trainingmodel_EC" "Trainingmodel_EC" ...
		 
	 
# convert dataframe tibbles to maxtrix for convenience usage for XGBoost

x_model_EC_train = subset(trainSet_model_EC, select = -NEE) %>% as.matrix() 
y_model_EC_train = trainSet_model_EC$NEE

x_model_EC_test = subset(testSet_model_EC, select = -NEE) %>% as.matrix() 
y_model_EC_test = testSet_model_EC$NEE
 
## visualize training and testing using histogram

Hist_EC <- model_EC_df %>%
			ggplot( aes(x=NEE, fill=Status)) +
			geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
			scale_fill_manual(values=c("#69b3a2", "#404080")) +
			theme_bw() +
			labs(x=expression(CO[2]~Flux~Netto~~"("~Mg~ha^{-1}~~jam^{-1}~")"))

Hist_EC

ggally_EC <- ggscatmat(model_EC_df, columns = 1:7, color = "Status", alpha = 0.8)


# ---- multiple linear model -----

set.seed = 42
tuned_lm_NEE <- train(
  x = x_model_EC_train,
  y = y_model_EC_train,
  method = "lm",
  family = "gaussian",
  metric = "RMSE",
  trControl = trainControl(method = "repeatedcv", number = 10, repeats=10)
)

summary(tuned_lm_NEE)
		Call:
		lm(formula = .outcome ~ ., data = dat, family = "gaussian")

		Residuals:
			  Min        1Q    Median        3Q       Max 
		-0.122828 -0.007361 -0.000693  0.006434  0.074034 

		Coefficients:
					  Estimate Std. Error t value Pr(>|t|)    
		(Intercept)  1.747e-05  1.874e-02   0.001 0.999256    
		PPFD        -1.696e-05  1.454e-06 -11.661  < 2e-16 ***
		VPD         -6.352e-06  1.249e-06  -5.086 4.26e-07 ***
		ustar        7.418e-03  3.969e-03   1.869 0.061854 .  
		rH          -1.338e-04  3.742e-05  -3.576 0.000363 ***
		Tsoil        1.392e-03  5.611e-04   2.481 0.013229 *  
		SWC         -4.675e-02  1.026e-02  -4.555 5.79e-06 ***
		---
		Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

		Residual standard error: 0.01278 on 1159 degrees of freedom
		Multiple R-squared:  0.4573,	Adjusted R-squared:  0.4545 
		F-statistic: 162.8 on 6 and 1159 DF,  p-value: < 2.2e-16


## ----------- Cubist ------------------

grid_EC_cub <- expand.grid(committees = c(1, 10, 50, 100, 150), neighbors = c(0, 1, 3, 5, 7, 9))

set.seed(42)
EC_Cub <- train(
  x = x_model_EC_train,
  y = y_model_EC_train,
  method = "cubist",
  metric = "RMSE",
  tuneGrid = grid_EC_cub,
  trControl = trainControl(method = "repeatedcv", number = 10, repeats=10)
  )

EC_Cub$bestTune
   committees neighbors
19        100         0

ggplot(EC_Cub)

# second try
grid_EC_cub2 <- expand.grid(committees = c(50, 75, 100), neighbors = c(0, 9))

set.seed(42)
EC_Cub2 <- train(
  x = x_model_EC_train,
  y = y_model_EC_train,
  method = "cubist",
  metric = "RMSE",
  tuneGrid = grid_EC_cub2,
  trControl = trainControl(method = "repeatedcv", number = 10, repeats=10)
  )

EC_Cub2$bestTune
  committees neighbors
5        100         0


# Final Model
grid_EC_cub_final <- expand.grid(committees = 100, neighbors = 0)

set.seed(42)
EC_Cub_final <- train(
  x = x_model_EC_train,
  y = y_model_EC_train,
  method = "cubist",
  metric = "RMSE",
  tuneGrid = grid_EC_cub_final,
  trControl = trainControl(method = "repeatedcv", number = 10, repeats=10)
  )

## ------------ Random Forest -------------------------------

### grid search within optimum range of ranger RF parameterization   

hyper_grid_RF_EC <- expand.grid(
  mtry       = seq(2, 6, by = 1),
  node_size  = seq(20, 30, by = 1),
  num.trees	 = seq(100, 1000, by = 100),
  OOB_RMSE   = 0
)

for(i in 1:nrow(hyper_grid_RF_EC)) {
  
  # train model
  model_rf_EC <- ranger(
  x = x_model_EC_train,
  y = y_model_EC_train,
    num.trees       = hyper_grid_RF_EC$num.trees[i],
    mtry            = hyper_grid_RF_EC$mtry[i],
    min.node.size   = hyper_grid_RF_EC$node_size[i],
    seed            = 42
  )
  
  # add OOB error to grid
  hyper_grid_RF_EC$OOB_RMSE[i] <- sqrt(model_rf_EC$prediction.error)
}

hyper_grid_RF_EC %>% 
  dplyr::arrange(OOB_RMSE) %>%  
  arrange(OOB_RMSE) %>%
		  top_n(-10, wt = OOB_RMSE)
		   mtry node_size num.trees   OOB_RMSE
		1     4        20       200 0.01100671
		2     4        21       200 0.01101491
		3     4        22       200 0.01101681
		4     4        20       300 0.01102338
		5     4        20       400 0.01102638
		6     4        23       200 0.01103351
		7     2        24       100 0.01103355
		8     4        21       100 0.01103584
		9     2        25       100 0.01104075
		10    4        25       200 0.01104250

### apply aforementioned ranger's tuning parameterisation to the caret
				
tunegrid_EC2 <- expand.grid(.mtry = 4) 

set.seed(123)
RF_EC2 <- train(
  x = x_model_EC_train,
  y = y_model_EC_train,
                   method = 'rf',
                   metric = 'RMSE',
                   tuneGrid = tunegrid_EC2, 
				   nodesize = 20,
					ntree = 200,
                   trControl = trainControl(method = "repeatedcv", number = 10, repeats=10))

print(RF_EC2) 
		Random Forest 

		1166 samples
		   6 predictor

		No pre-processing
		Resampling: Cross-Validated (10 fold, repeated 10 times) 
		Summary of sample sizes: 1048, 1049, 1050, 1050, 1050, 1048, ... 
		Resampling results:

		  RMSE        Rsquared   MAE        
		  0.01093331  0.6035576  0.007176801

		Tuning parameter 'mtry' was held constant at a value of 4




## extreeme gradient boosting ---------------------

# First step: Fixing nround, learning rate (eta), max tree depth
xgb_trc_EC = trainControl(
  method = "repeatedcv",
  number = 5,  
  #repeats = 10, # drop this to maximize the computational speed. 
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)


nrounds <- 1000
xgbGrid_EC1 <- expand.grid(
  nrounds = seq(from = 100, to = nrounds, by = 100),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)


set.seed(42) 
xgb_EC = train(
		x = x_model_EC_train,
		y = y_model_EC_train,
	trControl = xgb_trc_EC,
	tuneGrid = xgbGrid_EC1,
	method = "xgbTree",
	metric = 'RMSE',
	verbosity = 0
)

xgb_EC$bestTune
		nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
101     100         2 	  0.1     0                1                1         1
		
#evaluate
ggplot(xgb_EC)
# lowest RMSE is obtained at around eta = 0.1
# All curves indicate lower nrounds (< 200)
# better max tree depth is between 2 to 4


# Second step: lower nrounds and eta to 0.025,

nrounds2 <- 200
xgbGrid_EC2 <- expand.grid(
  nrounds = seq(from = 20, to = nrounds2, by = 10),
  eta = c(0.07, 0.1, 0.15, 0.2),
  max_depth = c(2, 3, 4),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

set.seed(42) 
xgb_EC2 = train(
		x = x_model_EC_train,
		y = y_model_EC_train,
	trControl = xgb_trc_EC,
	tuneGrid = xgbGrid_EC2,
	method = "xgbTree",
	metric = 'RMSE',
	verbosity = 0
)

xgb_EC2$bestTune
    nrounds max_depth eta gamma
211      30         4 0.2     0
    colsample_bytree
211                1
    min_child_weight subsample
211                1         1

#evaluate
ggplot(xgb_EC2)
# low RMSE at nrounds = 30 - 100
# choose max_depth = 4


# third step: fixing colsample_bytree, min_child_weight, and subsample. narrow nrounds

nrounds3 <- 99
xgbGrid_EC3 <- expand.grid(
  nrounds = seq(from = 9, to = nrounds3, by = 3),
  eta = 0.2,
  max_depth = 4,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = c(1,2,3),
  subsample = c(0.5, 0.75, 1.0)
)

set.seed(42) 
xgb_EC3 = train(
		x = x_model_EC_train,
		y = y_model_EC_train,
	trControl = xgb_trc_EC,
	tuneGrid = xgbGrid_EC3,
	method = "xgbTree",
	metric = 'RMSE',
	verbosity = 0
)

xgb_EC3$bestTune
    nrounds max_depth eta gamma
907      30         4 0.2     0
    colsample_bytree
907                1
    min_child_weight subsample
907                1         1

#evaluate
ggplot(xgb_EC3)

# dont forget to build full model using 10-fold repeated cv

xgb_trc_EC_final = trainControl(
  method = "repeatedcv",
  number = 10,  
  repeats = 10, 
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)


xgbGrid_EC_final <- expand.grid(
  nrounds = 30,
  eta = 0.2,
  max_depth = 4,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

set.seed(42) 
xgb_EC_final = train(
		x = x_model_EC_train,
		y = y_model_EC_train,
	trControl = xgb_trc_EC_final,
	tuneGrid = xgbGrid_EC_final,
	method = "xgbTree",
	metric = 'RMSE',
	verbosity = 0
)

xgb_EC_final 
		eXtreme Gradient Boosting 

		No pre-processing
		Resampling: Cross-Validated (10 fold, repeated 10 times) 
		Summary of sample sizes: 1049, 1050, 1050, 1050, 1050, 1049, ... 
		Resampling results:

		  RMSE        Rsquared 
		  0.01123113  0.5831406
		  MAE        
		  0.007500258

		Tuning parameter 'nrounds' was
		 1
		Tuning parameter 'subsample'
		 was held constant at a value of 1





