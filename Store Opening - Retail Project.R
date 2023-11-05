library(tidymodels)
library(visdat)
library(dplyr)
library(car)
library(pROC)
library(ggplot2)
library(tidyr)
library(ROCit)

s_train=read.csv("D:/Data Analytics/R/Projects/Downloads/store_train.csv",stringsAsFactors = FALSE)
s_test=read.csv("D:/Data Analytics/R/Projects/Downloads/store_test.csv",stringsAsFactors = FALSE)

#CUSTOM FUNCTIONS

storecode_func=function(x){
  x=substr(x,1,5)
  x=as.character(x)
  return(x)
}

#DATA PREPARATION

s_train$store=as.numeric(s_train$store)

dp_pipe=recipe(store~.,data=s_train) %>% 
  update_role(Id,Areaname,countytownname,state_alpha,new_role='drop_vars') %>% 
  update_role(country,State,countyname,store_Type,storecode,new_role='to_dummies') %>% 
  
  step_rm(has_role('drop_vars')) %>% 
  
  step_mutate_at(storecode,fn=storecode_func) %>%
  step_mutate_at(country,State,fn=as.character) %>% 
  
  step_unknown(has_role('to_dummies'),new_level='__missing__') %>% 
  step_other(has_role('to_dummies'),threshold=0.02,other='__other__') %>% 
  step_dummy(has_role('to_dummies')) %>% 
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)

train=bake(dp_pipe,new_data=NULL)
test=bake(dp_pipe,new_data=s_test)

vis_dat(train)

#BREAKING DATA

set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))
t1=train[s,]
t2=train[-s,]

#VIF CUTOFF 10

for_vif=lm(store~.-storecode_X__other__
           -store_Type_X__other__
           -sales0
           -sales2
           -sales3,data=t1)
sort(vif(for_vif),decreasing=TRUE)
  
#AIC SCORE

log_fit=glm(store~.-storecode_X__other__
            -store_Type_X__other__
            -sales0
            -sales2
            -sales3,data=t1,family='binomial')
summary(log_fit)

log_fit=stats::step(log_fit)

####

formula(log_fit)

log_fit=glm(store ~ country_X13 + country_X5 + storecode_METRO,data=t1,family='binomial')
summary(log_fit)

#PREDICTION ON t2 WHITH AUC SCORE

val.score=predict(log_fit,newdata=t2,type='response')
pROC::auc(pROC::roc(t2$store,val.score))

#FITTING THE MODEL ON ENTIRE DATA

for_vif=lm(store~.-storecode_X__other__
           -store_Type_X__other__
           -sales0
           -sales2
           -sales3,data=train)
sort(vif(for_vif),decreasing=TRUE)
summary(for_vif)

log_fit.final=glm(store~.-storecode_X__other__
                  -store_Type_X__other__
                  -sales0
                  -sales2
                  -sales3,data=train,family='binomial')
summary(log_fit.final)

log_fit.final=stats::step(log_fit.final)

formula(log_fit.final)

log_fit.final=glm(store ~ country_X13 + State_X50 + State_X37 + State_X17 + storecode_METRO,data=train,family='binomial')
summary(log_fit.final)

#FINDING CUTOFF FOR HARD CLASSES

train.score=predict(log_fit.final,newdata=train,type='response')
real=train$store

cutoffs=seq(0.001,0.999,0.001)
cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,precision=99,F1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F1=2*precision*recall/(precision+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,precision,F1,M))
}

cutoff_data=cutoff_data[-1,]

test.prob.score=predict(log_fit.final,newdata=test,type='response')
write.csv(test.prob.score,'Mayank_Phaugat_P2_part2.csv',row.names=FALSE)

####









####RANDOM FOREST

library(tidymodels)
library(ROCit)
library(visdat)
library(tidyr)
library(car)
library(dplyr)
library(pROC)
library(ggplot2)
library(vip)
library(rpart.plot)
library(DALEXtra)

s_train=read.csv("D:/Data Analytics/R/Projects/Downloads/store_train.csv",stringsAsFactors = FALSE)
s_test=read.csv("D:/Data Analytics/R/Projects/Downloads/store_test.csv",stringsAsFactors = FALSE)

#CUSTOM FUNCTIONS

storecode_func=function(x){
  x=substr(x,1,5)
  x=as.character(x)
  return(x)
}

#DATA PREPARATION

s_train$store=as.factor(as.numeric(s_train$store))

dp_pipe=recipe(store~.,data=s_train) %>% 
  update_role(Id,Areaname,countytownname,state_alpha,new_role='drop_vars') %>% 
  update_role(country,State,countyname,store_Type,storecode,new_role='to_dummies') %>% 
  
  step_rm(has_role('drop_vars')) %>% 
  
  step_mutate_at(storecode,fn=storecode_func) %>%
  step_mutate_at(country,State,fn=as.character) %>% 
  
  step_unknown(has_role('to_dummies'),new_level='__missing__') %>% 
  step_other(has_role('to_dummies'),threshold=0.02,other='__other__') %>% 
  step_dummy(has_role('to_dummies')) %>% 
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)

train=bake(dp_pipe,new_data=NULL)
test=bake(dp_pipe,new_data=s_test)

vis_dat(train)

#

rf_model=rand_forest(
  mtry=tune(),
  trees=tune(),
  min_n=tune()
) %>% 
  set_mode('classification') %>% 
  set_engine('ranger')

folds=vfold_cv(train,v=10)

rf_grid=grid_regular(mtry(c(5,25)),trees(c(100,500)),min_n(c(2,10)),levels=3)

doParallel::registerDoParallel()

my_res=tune_grid(
  rf_model,
  store~.,
  resamples=folds,
  grid=rf_grid,
  metrics=metric_set(roc_auc),
  control=control_grid(verbose=TRUE)
)

autoplot(my_res)+theme_light()

my_res %>% show_best()

final_rf_fit=rf_model %>% 
  set_engine('ranger',importance='permutation') %>% 
  finalize_model(select_best(my_res,'roc_auc')) %>% 
  fit(store~.,data=train)

#VARIABLE IMPORTANCE

final_rf_fit %>% 
  vip(geom='col',aesthetics=list(fill='midnightblue',alpha=0.8))+
  scale_y_continuous(expand=c(0,0))

#PREDICTIONS

train_pred=predict(final_rf_fit,new_data = train,type='prob') %>% select(.pred_1)
test_pred=predict(final_rf_fit,new_data = test,type='prob') %>% select(.pred_1)

#FINDING CUTOFF FOR HARD CLASSES

train.score=train_pred$.pred_1

real=train$store

#KS PLOT

rocit=ROCit::rocit(score=train.score,
                   class=real)
kplot=ROCit::ksplot(rocit)

#CUTOFF ON BASIS OF KS

my_cutoff=kplot$'KS Cutoff'

#HARD CLASSES

test_hard_class=as.numeric(test_pred>my_cutoff)

#PARTIAL DEPENDENCE PLOTS

model_explainer=explain_tidymodels(
  final_rf_fit,
  data=dplyr::select(train,-store),
  y=as.integer(train$store),
  verbose=FALSE
)

pdp=model_profile(
  model_explainer,
  variables='sales3',
  N=2000
)

plot(pdp)

write.csv(test_pred,'Mayank_Phaugat_P2_part2.csv',row.names = FALSE)

glimpse(s_train)









