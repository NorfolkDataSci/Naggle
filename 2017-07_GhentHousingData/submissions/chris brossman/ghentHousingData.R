library('tidyverse')
library(lazyeval)
library(XML)

#read in data
train_url = 'https://raw.githubusercontent.com/NorfolkDataSci/Naggle/master/2017-07_GhentHousingData/data/GhentDataSet%20-%20Train.csv'
test_url = 'https://raw.githubusercontent.com/NorfolkDataSci/Naggle/master/2017-07_GhentHousingData/data/GhentDataSet%20-%20Test.csv'


train_raw = read_csv(train_url)
test_raw = read_csv(test_url)
gdp_def <- readHTMLTable('http://www.multpl.com/gdp-deflator/table', header=T, which=1,stringsAsFactors=F)
names(gdp_def) <- c('d','v')
gdp_def <- gdp_def %>% mutate(year = substr(d, nchar(d)-3,nchar(d))) %>% mutate(v = as.numeric(v)/100)

#get median values
median_val_ds <- rbind(train_raw, test_raw) %>% 
  mutate(year = substr(`Most Recent Sale Date`, nchar(`Most Recent Sale Date`)-3,nchar(`Most Recent Sale Date`))) %>%
  left_join(gdp_def, by = 'year') %>%
  select(`Parcel Square Footage`,`Finished Living Space`,year, `Sale Price`, gdp_def = v)
dat <- NULL
for(j in 1:(length(names(median_val_ds))-1)){
  i = names(median_val_ds)[j]
  f1 <- paste0("`",i,"`", " != ",  "'0.0'")
  f2 <- paste0("`",i,"`", " != ",  "'0'")
  f3 <- paste0("`",i,"`", " != ",  "'#REF!'")
  f4 <- paste0('!is.na(',"`",i,"`",')')
  f5 <- paste0("`",i,"`", " != ",  "'NULL'")
  f6 = paste0("`",i,"`", " != ",  "'REF!'")
  
  if(i != "Sale Price"){
    m <- median_val_ds %>% select(i) %>% filter_(f1,f2,f3,f4,f5,f6) %>% unlist() %>% as.numeric() %>% median()
  } else{
    m <- median_val_ds %>% select(`Sale Price`, gdp_def) %>% filter_(f1,f2,f3,f4,f5,f6) %>% 
      mutate(`Sale Price` = round(as.numeric(`Sale Price`)/as.numeric(gdp_def)),0) %>%
      select(`Sale Price`) %>% unlist() %>% as.numeric() %>% median()
  }
  
  temp <- cbind.data.frame(var = i, median = m)
  dat <- rbind(dat,temp)
}

#inpute median values & convert data types
train <- train_raw %>% mutate(year = substr(`Most Recent Sale Date`, nchar(`Most Recent Sale Date`)-3,nchar(`Most Recent Sale Date`))) %>%
  mutate(year = ifelse(year == 'REF!','NULL', year)) %>% left_join(gdp_def, by = 'year')
test <- test_raw %>% mutate(year = substr(`Most Recent Sale Date`, nchar(`Most Recent Sale Date`)-3,nchar(`Most Recent Sale Date`))) %>%
  mutate(year = ifelse(year == 'REF!','NULL', year)) %>% left_join(gdp_def, by = 'year')
dat$var <- as.character(dat$var)

for(q in 1:nrow(dat)){
  for(p in 1:nrow(train)){
    train[[p,dat$var[q]]] <- ifelse(train[[p,dat$var[q]]] == '0', dat$median[q], 
                                      ifelse(train[[p,dat$var[q]]] == '0.0', dat$median[q],
                                             ifelse(train[[p,dat$var[q]]] == '#REF!', dat$median[q],
                                                    ifelse(is.na(train[[p,dat$var[q]]]),dat$median[q],
                                                           ifelse(train[[p,dat$var[q]]] == 'NULL', dat$median[q],
                                                                  ifelse(q == "Sale Price",as.numeric(train[[p,dat$var[q]]])/as.numeric(train[[p,'v']]),train[[p,dat$var[q]]]))))))
                                                           
  }
 
  for(p in 1:nrow(test)){
    test[[p,dat$var[q]]] <- ifelse(test[[p,dat$var[q]]] == '0', dat$median[q], 
                                    ifelse(test[[p,dat$var[q]]] == '0.0', dat$median[q],
                                           ifelse(test[[p,dat$var[q]]] == '#REF!', dat$median[q],
                                                  ifelse(is.na(test[[p,dat$var[q]]]),dat$median[q], 
                                                         ifelse(test[[p,dat$var[q]]] == 'NULL', dat$median[q],
                                                                ifelse(q == "Sale Price",as.numeric(test[[p,dat$var[q]]])/as.numeric(test[[p,'v']]),test[[p,dat$var[q]]]))))))
  }
  
}
#drop last two columns
train <- train[,1:(length(train)-2)]; test[,1:(length(test)-2)]
#change data type
for(i in 1:length(names(train))){
  if(names(train)[i] == 'Most Recent Sale Date' | i <= 8){
    train[,i] <- as.factor(unlist(train[,i]))
    test[,i] <- as.factor(unlist(test[,i]))
  }
  else{
    train[,i] <- as.numeric(unlist(train[,i]))
    test[,i] <- as.numeric(unlist(test[,i]))
  } 
}

#define target
train$y <- train$`2016 Land` + train$`2016 Building`

#simple plots
x = c('Parcel Square Footage','Finished Living Space', 'Sale Price')
simple_plot = function(x,n){
  plot(log(x),log(train$y), main = paste0("log(tax eval) to log(",n,')'))
  abline(lm(log(train$y) ~ log(x)),col = 'red')
}
for(n in x){
  d <-train %>% select(n) %>% unlist()
  simple_plot(d,n)
}



#split the training and test sets from the training
## 75% of the sample size
train_ind <- sample(seq_len(nrow(train)), size = floor(0.8 * nrow(train)))
trainingSet = train[train_ind,]
testingSet = train[-train_ind,]

