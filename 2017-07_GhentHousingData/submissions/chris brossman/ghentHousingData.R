library('tidyverse')
library(lazyeval)
library(XML)

#read in data
train_url = 'https://raw.githubusercontent.com/NorfolkDataSci/Naggle/master/2017-07_GhentHousingData/data/GhentDataSet%20-%20Train.csv'
test_url = 'https://raw.githubusercontent.com/NorfolkDataSci/Naggle/master/2017-07_GhentHousingData/data/GhentDataSet%20-%20Test.csv'


train_raw = read_csv(train_url); train_raw$origin = 'train'
test_raw = read_csv(test_url); test_raw$origin = 'test'
gdp_def <- readHTMLTable('http://www.multpl.com/gdp-deflator/table', header=T, which=1,stringsAsFactors=F)
names(gdp_def) <- c('d','v')
gdp_def <- gdp_def %>% mutate(year = substr(d, nchar(d)-3,nchar(d))) %>% mutate(v = as.numeric(v)/100)

#combine data set to impute values
combined <- rbind(train_raw, test_raw) %>% 
  mutate(year = substr(`Most Recent Sale Date`, nchar(`Most Recent Sale Date`)-3,nchar(`Most Recent Sale Date`))) %>%
  left_join(gdp_def, by = 'year') %>% mutate(y = as.numeric(`2016 Land`) + as.numeric(`2016 Building`)) %>%
  select(Key, `Block Number`, `Road Name`, `Property City`,`Property State`, `Zip Code Short`, Neighborhood, 
         `Property Use`, `Parcel Square Footage`,`Finished Living Space`,`2016 Land`, `2016 Building`, `Most Recent Sale Date`,
         year, `Sale Price`, gdp_def = v, y, origin)

#create funciton to take in df, columns to select, and return filtered DF
filter_df <- function(df, col, ...) {
  s <- quos(...)
  
  f1 <- paste0("`",col,"`", " != ",  "'0.0'")
  f2 <- paste0("`",col,"`", " != ",  "'0'")
  f3 <- paste0("`",col,"`", " != ",  "'#REF!'")
  f4 <- paste0('!is.na(',"`",col,"`",')')
  f5 <- paste0("`",col,"`", " != ",  "'NULL'")
  f6 = paste0("`",col,"`", " != ",  "'REF!'")

  df2 <- df %>% select(!!!s, col) %>% filter_(f1,f2,f3,f4,f5,f6)

  return(df2)
}

#calculate avg_par_sql_foot per neighborhood and property use
avg_par_sq_foot <- filter_df(df = combined,
                   col = 'Parcel Square Footage', 
                   'Neighborhood', 'Property Use') %>% group_by(Neighborhood, `Property Use`) %>% 
  summarise(median_sq_ft = median(as.numeric(`Parcel Square Footage`), na.rm = TRUE)) %>% ungroup()

#calculate avg_par_sql_foot per property use
avg_par_sq_foot_prop_use_only <- filter_df(df = combined,
                             col = 'Parcel Square Footage', 
                             'Property Use') %>% group_by(`Property Use`) %>% 
  summarise(median_sq_ft_prop_use_only = median(as.numeric(`Parcel Square Footage`), na.rm = TRUE)) %>% ungroup()

#inpute value for Parcel Square Footage
combined <- combined %>% left_join(avg_par_sq_foot, by = c("Neighborhood" = "Neighborhood", "Property Use" = "Property Use")) %>%
  left_join(avg_par_sq_foot_prop_use_only, by = c("Property Use")) %>% 
  mutate(par_sq_ft_impute = round(ifelse(is.na(median_sq_ft), median_sq_ft_prop_use_only, median_sq_ft),0)) %>%
  mutate(`Parcel Square Footage` = ifelse(`Parcel Square Footage` == 0, par_sq_ft_impute, `Parcel Square Footage`)) %>%
  select(-c(median_sq_ft,median_sq_ft_prop_use_only, par_sq_ft_impute))

#simple plot function to checkout impute
simple_plot = function(x,n){
  plot(log(x),log(combined$y), main = paste0("log(tax eval) to log(",n,')'))
  abline(lm(log(combined$y) ~ log(x)),col = 'red')
}
simple_plot(combined$`Parcel Square Footage`, 'Parcel SQ Foot')
##################################################################
#create simple linear model to estimate Finished Living Space & Impute
##################################################################
fin_lv_sp_df <- filter_df(df = combined,
                             col = 'Finished Living Space', 
                             'Neighborhood', 'Property Use','Parcel Square Footage') %>% 
  mutate(`Finished Living Space` = as.numeric(`Finished Living Space`))
fit_fin_liv_space = lm(log(`Finished Living Space`) ~ log(`Parcel Square Footage`), data = fin_lv_sp_df)

#impute value
combined <- combined %>% mutate(predict_fin_liv_space = round(exp(predict(fit_fin_liv_space, .)),0)) %>%
  mutate(`Finished Living Space` = ifelse(is.na(`Finished Living Space`),predict_fin_liv_space,
                                          ifelse(`Finished Living Space` == '0', predict_fin_liv_space,
                                                 ifelse(`Finished Living Space` == '0.0', predict_fin_liv_space,
                                                        ifelse(`Finished Living Space` == '#REF!', predict_fin_liv_space,
                                                               ifelse(`Finished Living Space` == 'NULL', predict_fin_liv_space,
                                                                      as.numeric(`Finished Living Space`))))))) %>%
  select(-c(predict_fin_liv_space))

simple_plot(combined$`Finished Living Space`, 'Finished Living Space')


##################################################################
#create simple linear model to estimate 2016 sales price
##################################################################
SP_df <- filter_df(df = combined,
                          col = 'Sale Price', 
                          'Neighborhood', 'Property Use','Parcel Square Footage', 'Finished Living Space', 'gdp_def') %>% 
  mutate(sp = as.numeric(`Sale Price`)/gdp_def)

sp_model = lm(log(sp) ~ log(`Finished Living Space`) + factor(`Property Use`),data = SP_df)

#impute value
combined <- combined %>% mutate(sp_pred = round(exp(predict(sp_model, .)),0)) %>%
  mutate(sp = ifelse(is.na(`Sale Price`),sp_pred,
                     ifelse(`Sale Price` == '0', sp_pred,
                            ifelse(`Sale Price` == '0.0', sp_pred,
                                  ifelse(`Sale Price` == '#REF!', sp_pred,
                                        ifelse(`Sale Price` == 'NULL', sp_pred,
                                              as.numeric(`Sale Price`)/gdp_def)))))) %>% select(-c(sp_pred))

simple_plot(combined$sp, 'adj_sp')


#############################################################################################
# adjust data types then split back into training and test sets
#############################################################################################
f <- c('Block Number','Road Name', 'Property City','Property State','Zip Code Short','Neighborhood','Property Use')
for(n in names(combined)){
  if(n %in% f){
    combined[[n]] <- as.factor(combined[[n]])
  }
}
combined_rename <- combined %>%
  select(key = Key, bn = `Block Number`, rn = `Road Name`, pc = `Property City`, 
         #ps = `Property State`, zip = `Zip Code Short`,
         nh = Neighborhood, pu = `Property Use`, psf = `Parcel Square Footage`, fls = `Finished Living Space`, sp, y, origin)

#remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

#split into training and remove extreme values
training_all <- combined_rename %>% filter(origin == 'train') %>% mutate(y2 = exp(remove_outliers(log(y)))) %>% 
  filter(!is.na(y2)) %>% select(-y2)
test <- combined_rename %>% filter(origin == 'test')

#split the training and test sets from the training all 
set.seed(1234)
## 75% of the sample size
train_ind <- sample(seq_len(nrow(training_all)), size = floor(0.75 * nrow(training_all)))
trainingSet = training_all[train_ind,]
testingSet = training_all[-train_ind,]

##########################################################
# setup some baselines for RMSE - what is best naive RMSE?
##########################################################

#rmse_of_average
rmse_of_average <- sqrt(mean((mean(trainingSet$y) - testingSet$y)^2))
#rmse_of_avg_by_Neighboorhood
rmse_of_avg_by_neighborhood <- trainingSet %>% group_by(nh) %>% summarise(y_neigh = mean(y)) %>% ungroup() %>% 
  right_join(testingSet, by = 'nh') %>% mutate(e_sqrd = (y_neigh - y)^2) %>% 
  summarize(rmse = sqrt(mean(e_sqrd))) %>% unlist()

#rmse_of_avg_by_Neighboorhood & Property Use
rmse_of_avg_by_Neighboorhood_and_prop_use <- trainingSet %>% group_by(nh, pu) %>% 
  summarise(y_neigh = mean(y)) %>% ungroup() %>% 
  right_join(testingSet) %>% mutate(e_sqrd = (y_neigh - y)^2) %>% summarize(rmse = sqrt(mean(e_sqrd,na.rm=TRUE))) %>% unlist()

#best niave RMSE
naive_rmse <- round(min(rmse_of_average, rmse_of_avg_by_neighborhood,rmse_of_avg_by_Neighboorhood_and_prop_use),0); naive_rmse

##########################################################
# Can a model beat naive?
##########################################################
pred_adj <- function(pred,train_y){
  adj_pred <- ifelse(pred < 0, median(train_y),ifelse(pred > max(train_y), max(train_y)*.85, pred))
  return(adj_pred)
}

######################################
# LM w/adjustment
#####################################
lm_f <- y ~ fls*pu + nh*log(sp)
m <- lm(formula = lm_f,data = trainingSet)
#prediction, doing naive things to compensate for poor model
testingSet$lm_yhat <- pred_adj(predict(m, testingSet),trainingSet$y)
lm_rmse <- round(sqrt(mean((testingSet$lm_yhat - testingSet$y)^2)),0); lm_rmse
# percentage error reduction
print(paste0('percentage RMSE reduction from naive method : ',round((1-lm_rmse/naive_rmse) * 100,2),'%'))
ggplot(aes(x=lm_yhat, y=y),data=testingSet) + geom_point()  + geom_abline(color="red") + ggtitle(paste0('LM comp level'))
#ggplot(aes(x=log(lm_yhat), y=log(y)),data=testingSet) + geom_point()  + geom_abline(color="red") + ggtitle(paste0('LM comp log'))
######################################
#Gradiant boosting
#####################################
library('caret')
gb_f <- y ~ fls + pu + nh + sp
fitControl <- trainControl( method = "repeatedcv", number = 5, repeats = 5)
fit <- train(gb_f, data = trainingSet, method = "gbm", trControl = fitControl,verbose = FALSE)
testingSet$gb_yhat= predict(fit,testingSet)
gb_rmse <- round(sqrt(mean((testingSet$gb_yhat - testingSet$y)^2)),0); gb_rmse
#percent rmse reduction
print(paste0('percentage RMSE reduction from naive method : ',round((1-gb_rmse/naive_rmse) * 100,2),'%'))
ggplot(aes(x=gb_yhat, y=y),data=testingSet) + geom_point()  + geom_abline(color="red") + ggtitle(paste0('GB comp level'))
#ggplot(aes(x=log(gb_yhat), y=log(y)),data=testingSet) + geom_point()  + geom_abline(color="red") + ggtitle(paste0('GB comp log'))

######################################
#GAM
#####################################
require(mgcv)
gam_f <- y~s(fls) + pu + nh + s(sp)+ s(log(sp),log(fls))
gamobj<-gam(gam_f, family=gaussian(link=identity),data=trainingSet, method="REML")
testingSet$gam_yhat= predict(gamobj,testingSet)
gam_rmse <- round(sqrt(mean((testingSet$gam_yhat - testingSet$y)^2)),0); gam_rmse
print(paste0('percentage RMSE reduction from naive method : ',round((1-gam_rmse/naive_rmse) * 100,2),'%'))
ggplot(aes(x=gam_yhat, y=y),data=testingSet) + geom_point()  + geom_abline(color="red") + ggtitle(paste0('GB comp level'))
#ggplot(aes(x=log(gam_yhat), y=log(y)),data=testingSet) + geom_point()  + geom_abline(color="red") + ggtitle(paste0('GB comp log'))

######################################
#SVM
#####################################
require(e1071)
svm_f <- y ~ fls + pu + nh + sp
tuneResult <- tune(svm, svm_f,  data = trainingSet,
                   ranges = list(epsilon = seq(0,0.5,0.1), cost = seq(1,50,10))
)
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)
testingSet$svm_yhat <- predict(tuneResult$best.model,testingSet)
svm_rmse <- round(sqrt(mean((testingSet$svm_yhat - testingSet$y)^2)),0); svm_rmse
print(paste0('percentage RMSE reduction from naive method : ',round((1-svm_rmse/naive_rmse) * 100,2),'%'))
ggplot(aes(x=svm_yhat, y=y),data=testingSet) + geom_point()  + geom_abline(color="red") + ggtitle(paste0('SVM comp level'))
#ggplot(aes(x=log(svm_yhat), y=log(y)),data=testingSet) + geom_point()  + geom_abline(color="red") + ggtitle(paste0('SVM comp log'))

#######################################################
#use best predictor
#######################################################
#retrain using all available data
min_rmse <- min(lm_rmse,gb_rmse,gam_rmse,svm_rmse)
if(min_rmse == lm_rmse){
  m <- lm(formula = lm_f,data = training_all)
  test$yhat <- predict(m,test)
  print(paste0('lm wins!, RMSE: ', min_rmse))
} else if(min_rmse == gb_rmse){
  m <- train(gb_f, data = training_all, method = "gbm", trControl = fitControl,verbose = FALSE)
  test$yhat <- predict(m,test)
  print(paste0('gb wins!, RMSE: ', min_rmse))
} else if(min_rmse == gam_rmse){
  m <-gam(gam_f, family=gaussian(link=identity),data=training_all, method="REML")
  test$yhat <- predict(m,test)
  print(paste0('gam wins!, RMSE: ', min_rmse))
} else {
  tuneResult <- tune(svm, svm_f,  data = training_all,
                     ranges = list(epsilon = seq(0,0.5,0.1), cost = seq(1,50,10))
  )
  test$yhat <- predict(tuneResult$best.model,test)
  print(paste0('svm wins!, RMSE: ', min_rmse))
}

test_submit <- test %>% select(key,yhat)
write_csv(test_submit, 'output.csv')
