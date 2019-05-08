library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(caret)
library(doParallel)

train <- readxl::read_excel('Data//StudentData.xlsx')
eval <- readxl::read_excel('Data//StudentEvaluation.xlsx')

# Summary stats table
summary_stats <- function(df){
  minimum <- round(apply(df, 2, min, na.rm=T),2)
  maximum <- round(apply(df, 2, max, na.rm=T),2)
  quantile1 <- round(apply(df, 2, quantile, 0.25, na.rm=T),2)
  quantile3 <- round(apply(df, 2, quantile, 0.75, na.rm=T),2)
  medians <- round(apply(df, 2, median, na.rm=T),2)
  means <- round(apply(df, 2, mean, na.rm=T),2)
  stdev <- round(apply(df, 2, sd, na.rm=T),2)
  nas <- apply(apply(df, 2, is.na), 2, sum, na.rm=T)
  nas_p <- paste(round(nas / dim(df)[1], 4) * 100, '%')
  temp <- data.frame(means, stdev, minimum, maximum, quantile1, medians, quantile3, nas, nas_p)
  colnames(temp) <- c('Mean', 'St.Dev', 'Min.', 'Max.', '1st.Qu.', 'Median', '3rd.Qu.', "NA", "% NA")
  return(temp)
}

# Summary stats for train and eval sets
train_summary <- train %>% subset(select=-c(`Brand Code`)) %>% summary_stats()
eval_summary <- eval %>% subset(select=-c(`Brand Code`, PH)) %>% summary_stats()
train_summary
eval_summary

# Plot correlation heatmap
corr <- cor(subset(train, select = -c(`Brand Code`)), use='pairwise.complete.obs')
ggcorrplot(corr,  ggtheme=ggplot2::theme_grey)
 
# Create multi histograms
# The `multiplot` function is copied from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# The `create_multiplots` function uses the `multiplots` function to create histograms for this project
create_multiplots <- function(df, cols=4){
  plots <- list()
  i <- 0
  for(var in names(df)){
    i <- i+1
    if(var!='SET'){
      p <- ggplot(df, aes_string(x=var, fill='SET', color='SET')) + 
        geom_histogram(alpha=0.5, position='identity')
      p <-  p +
        xlab(var) +
        ylab('COUNT') +
        theme(legend.position='none', 
              axis.title=element_text(size=8),
              axis.text=element_text(size=6))
      plots[[i]] <- p
    }
  }
  multiplot(plotlist=plots, cols=cols)
}

full_set <- rbind(train, eval)
full_set <- subset(full_set, select=-c(`Brand Code`))
names(full_set) <- gsub(' ', '_', names(full_set))
full_set$SET <- NA
full_set$SET[1:dim(train)[1]] <- 'TRAIN'
full_set$SET[is.na(full_set$SET)] <- 'EVAL'
set1_vars <- c("Carb_Volume",       "Fill_Ounces",       "PC_Volume",         "Carb_Pressure",     "Carb_Temp",        
               "PSC",               "PSC_Fill",          "PSC_CO2",           "Mnf_Flow",          "Carb_Pressure1",   
               "Fill_Pressure",     "Hyd_Pressure1",     "Hyd_Pressure2",     "Hyd_Pressure3",     "Hyd_Pressure4", 
               "SET")
set2_vars <- c("Filler_Level",      "Filler_Speed",      "Temperature",       "Usage_cont",      "Carb_Flow",        
               "Density",           "MFR",               "Balling",           "Pressure_Vacuum",                 
               "Oxygen_Filler",     "Bowl_Setpoint",     "Pressure_Setpoint", "Air_Pressurer",     "Alch_Rel",         
               "Carb_Rel",          "Balling_Lvl",       "SET")
full_set1 <- subset(full_set, select=set1_vars)
full_set2 <- subset(full_set, select=set2_vars)

create_multiplots(full_set1)
create_multiplots(full_set2)

##################################################################################

# Data Preparation

library(dplyr)
library(caret)
library(doParallel)

train <- readxl::read_excel('Data//StudentData.xlsx')
eval <- readxl::read_excel('Data//StudentEvaluation.xlsx')

# Encoding categorical variable `Brand Code`
train$`Brand Code` <- addNA(train$`Brand Code`)
eval$`Brand Code` <- addNA(eval$`Brand Code`)
brandCodeTrain <- predict(dummyVars(~`Brand Code`, data=train), train)
brandCodeEval <- predict(dummyVars(~`Brand Code`, data=eval), eval)
head(brandCodeTrain, 10)
head(train$`Brand Code`, 10)
head(brandCodeEval, 10)
head(eval$`Brand Code`, 10)
train <- cbind(brandCodeTrain, subset(train, select=-c(`Brand Code`)))
eval <- cbind(brandCodeEval, subset(eval, select=-c(`Brand Code`)))

# Remove special symbols (white space and `) in names
names(train) <- gsub(patter=c(' |`'), replacement='', names(train))
names(eval) <- gsub(patter=c(' |`'), replacement='', names(eval))

# Remove rows in training set with missing target variables
train <- train[complete.cases(train$PH),]

# Check near-zero-variance variables
nearZeroVar(train, names=T)

# Separate the predictors and target, and remove nzv variable
xTrain <- subset(train, select=-c(PH,`HydPressure1`)) %>% as.data.frame()
xEval <- subset(eval, select=-c(PH,`HydPressure1`)) %>% as.data.frame()
yTrain <- train$PH

# Set up 5-folds cross validation in trainControl
set.seed(1)
cvFolds <- createFolds(yTrain, k=5)
trControl <- trainControl(verboseIter=T,
                          method='cv', 
                          number=5,
                          index=cvFolds)

# Set up multi-core processing
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

##################################################################################
# Linear Models

# PLS

plsFit1 <- train(x=xTrain,
                 y=yTrain, 
                 method='pls',
                 tuneLength=20,
                 trControl=trControl,
                 preProc=c('knnImpute', 'center', 'scale'))

plsFit2 <- train(x=xTrain,
                 y=yTrain, 
                 method='pls',
                 tuneLength=20,
                 trControl=trControl,
                 preProc=c('bagImpute', 'center', 'scale'))

plsFit3 <- train(x=xTrain,
                 y=yTrain, 
                 method='pls',
                 tuneLength=20,
                 trControl=trControl,
                 preProc=c('medianImpute', 'center', 'scale'))

# Elastic Net

enetFit1 <- train(x=xTrain,
                  y=yTrain,
                  method='enet',
                  tuneGrid=expand.grid(.fraction = seq(0, 1, by=0.1), 
                                       .lambda = seq(0, 1, by=0.1)),
                  trControl=trControl,
                  preProc=c('knnImpute', 'center', 'scale'))

enetFit2 <- train(x=xTrain,
                  y=yTrain,
                  method='enet',
                  tuneGrid=expand.grid(.fraction = seq(0, 1, by=0.1), 
                                       .lambda = seq(0, 1, by=0.1)),
                  trControl=trControl,
                  preProc=c('bagImpute', 'center', 'scale'))

enetFit3 <- train(x=xTrain,
                  y=yTrain,
                  method='enet',
                  tuneGrid=expand.grid(.fraction = seq(0, 1, by=0.1), 
                                       .lambda = seq(0, 1, by=0.1)),
                  trControl=trControl,
                  preProc=c('medianImpute', 'center', 'scale'))


resamples(list(PLS1=plsFit1, PLS2=plsFit2, PLS3=plsFit3,
               enet1=enetFit1, enet2=enetFit2, enet3=enetFit3)) %>% summary()

plsFit <- plsFit1
enetFit <- enetFit1

###################################################################################
# Non-linear Models

# KNN

knnFit <- train(x=xTrain,
                y=yTrain,
                method='knn',
                tuneLength=20,
                trControl=trControl,
                preProc=c('knnImpute', 'center', 'scale'))

# Support Vector Machine

svmFit <- train(x=xTrain,
                y=yTrain,
                method="svmRadial",
                tuneLength=20,
                trControl=trControl,
                preProc=c('knnImpute', 'center', 'scale'))

# Neural Network
nnetGrid <- expand.grid(.layer1=c(1, 5, 10, 20, 50),
                        .layer2=c(0, 1, 5, 10, 20),
                        .layer3=c(0, 1, 5, 10, 20),
                        .decay=c(0.1, 0.25, 0.50, 0.75))

nnetFit <- train(x=xTrain,
                 y=yTrain,
                 method='mlpWeightDecayML',
                 tuneGrid=nnetGrid,
                 trControl=trControl,
                 preProc=c('knnImpute', 'center', 'scale'))

nnetGrid1 <- expand.grid(.size=c(1, 5, 10, 20, 30),
                         .decay=c(0.1, 0.25, 0.5, 0.75))

nnetFit1 <- train(x=xTrain,
                 y=yTrain,
                 method='mlpWeightDecay',
                 tuneGrid=nnetGrid1,
                 trControl=trControl,
                 preProc=c('knnImpute', 'center', 'scale'))

nnetGrid2 <- expand.grid(.size=c(1, 5, 10, 20, 50),
                         .decay=c(0.1, 0.25, 0.5, 0.75),
                         .bag=c(0.1, 0.25, 0.5, 0.75))

nnetFit2 <- train(x=xTrain,
                  y=yTrain,
                  method='avNNet',
                  tuneGrid=nnetGrid2,
                  trControl=trControl,
                  preProc=c('knnImpute', 'center', 'scale'))

nnetGrid3 <- expand.grid(.size=c(1, 5, 10, 20, 50),
                         .dropout=c(0, 0.25, 0.5, 0.75),
                         .batch_size=c(128),
                         .lr=c(0.001, 0.01, 0.1, 0.5),
                         .rho=c(0.9),
                         .decay=c(0),
                         .activation=c())

nnetFit3 <- train(x=xTrain,
                  y=yTrain,
                  method='mlpKerasDropout',
                  tuneGrid=nnetGrid3,
                  trControl=trControl,
                  preProc=c('knnImpute', 'center', 'scale'))


resamples(list(one=nnetFit, two=nnetFit1, three=nnetFit2)) %>% summary()

###################################################################################
# Tree Models

# Random Forest

rf <- train(x=xTrain, 
            y=yTrain, 
            method='rf',
            tuneLength=10,
            trControl=trControl,
            preProc=c('knnImpute'),
            importance=T)

# XGBoost

xgbGrid <- expand.grid(.nrounds=c(100, 500, 1000, 1500), # boosting iterations (trees)
                       .max_depth=c(4, 6, 8, 10), # max tree depth
                       .eta=c(0.001, 0.01, 0.1, 0.5), # learning rate
                       .gamma=c(0),# minimum loss reduction
                       .colsample_bytree=c(0.4, 0.6, 0.8, 1), # subsample ratio of columns
                       .min_child_weight=c(1, 5, 15), # minimum sum of instance weight
                       .subsample=c(0.5, 0.75, 1))  # subsample ratio of rows

xgb1 <- train(x = xTrain,
              y = yTrain,
              method = 'xgbTree',
              tuneGrid = xgbGrid,
              trControl = trControl, 
              importance = T)

xgb2 <- train(x = xTrain,
              y = yTrain,
              method = 'xgbTree',
              tuneGrid = xgbGrid,
              trControl = trControl,
              preProce = c('knnImpute'),
              importance = T)

resamples(list(XGB1=bst1, XGB2=bst2)) %>% summary()

# Stop multi-core processing
stopCluster(cl)
registerDoSEQ()

xgb <- bst2

bst$bestTune
bst$finalModel
plot(varImp(bst))
xgb.plot.tree(model=bst$finalModel, trees=2)


resamples(list(PLS=plsFit, ENet=enetFit, KNN=knnFit, NNet=svmFit,
               RF=rf, XGB=bst)) %>% summary()


getRank <- function(trainObjects){
  temp <- c()
  methods <- c()
  for(object in trainObjects){
    methods <- c(methods, object$method)
    varimp <- varImp(object)[[1]]
    varimp$variables <- row.names(varimp)
    rank <- varimp[order(varimp$Overall, decreasing = T),] %>% row.names()
    temp <- cbind(temp, rank)
    
  }
  temp <- as.data.frame(temp)
  names(temp) <- methods
  return(temp)
}

getRank(list(plsFit, rf, xgb, enetFit, knnFit, svmFit))


