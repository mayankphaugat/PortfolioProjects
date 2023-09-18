library(tidymodels)
library(visdat)
library(tidyr)
library(car)
library(dplyr)
library(pROC)
library(ggplot2)
library(vip)
library(rpart.plot)
library(DALEXtra)

h_train=read.csv("D:/Data Analytics/R/Projects/Downloads/housing_train.csv",stringsAsFactors = FALSE)
h_test=read.csv("D:/Data Analytics/R/Projects/Downloads/housing_test.csv",stringsAsFactors = FALSE)

glimpse(h_train)
vis_dat(h_train)

#CUSTOM FUNCTIONS
add_func=function(x){
  temp=data.frame(add=x)
  temp=temp %>% 
    separate(add,into=c('a1','a2'),sep=' ') %>% 
    mutate(No=a1,Street=a2) %>% 
    select(-No)
  
  return(temp[,'Street'])
}

#DATA PREP

#Suburb : categorical :: club low frequency data and create dummies
#Address : categorical :: drop
#Rooms : numeric :: 
#Type : categorical :: create dummies
#Price : numeric ::  
#Method : categorical ::  create dummies
#SellerG : categorical ::  club low frequency data and create dummies
#Distance : numeric :: custom function
#Postcode : categorical :: drop
#Bedroom2 : Numeric :: 
#Bathroom : numeric :: 
#Car : numeric :: 
#Landsize : numeric :: 
#BuildingArea : numeric :: 
#YearBuilt : numeric :: drop
#CouncilArea : numeric :: convert to catagorical and create dummies

dp_pipe=recipe(Price~.,data=h_train) %>% 
  update_role(Address,Postcode,YearBuilt,new_role='drop_vars') %>% 
  update_role(Suburb,Type,Method,SellerG,CouncilArea,new_role='to_dummies') %>% 
  
  step_rm(has_role('drop_vars')) %>% 
  step_mutate_at(CouncilArea,fn=as.character) %>% 
  step_mutate_at(Distance,fn=round) %>% 
  step_unknown(has_role('to_dummies'),new_level='__missing__') %>% 
  step_other(has_role('to_dummies'),threshold=0.005,other='__other__') %>% 
  step_dummy(has_role('to_dummies')) %>% 
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)

train=bake(dp_pipe,new_data=NULL)
test=bake(dp_pipe,new_data=h_test)

####

set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))
t1=train[s,]
t2=train[-s,]

#vif cutoff 5

fit=lm(Price~.
       -Type_X__other__
       -Suburb_X__other__
       -Distance
       -CouncilArea_X,data=t1)
sort(vif(fit),decreasing=TRUE)

#discarding by AIC score

fit=stats::step(fit)

#p-value cutoff 1

fit=lm(Price ~ Rooms + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + 
         Suburb_Airport.West + Suburb_Altona + Suburb_Altona.North + 
         Suburb_Armadale + Suburb_Avondale.Heights + Suburb_Balwyn + 
         Suburb_Balwyn.North + Suburb_Bentleigh.East + Suburb_Brighton + 
         Suburb_Brighton.East + Suburb_Brunswick + Suburb_Brunswick.East + 
         Suburb_Brunswick.West + Suburb_Bulleen + Suburb_Camberwell + 
         Suburb_Coburg + Suburb_Coburg.North + Suburb_Essendon + Suburb_Fawkner + 
         Suburb_Glen.Iris + Suburb_Glenroy + Suburb_Hadfield + Suburb_Hampton + 
         Suburb_Hawthorn + Suburb_Hawthorn.East + Suburb_Heidelberg.Heights + 
         Suburb_Heidelberg.West + Suburb_Ivanhoe + Suburb_Keilor.East + 
         Suburb_Kensington + Suburb_Kew + Suburb_Maidstone + Suburb_Malvern + 
         Suburb_Malvern.East + Suburb_Maribyrnong + Suburb_Moonee.Ponds + 
         Suburb_Newport + Suburb_Niddrie + Suburb_Northcote + Suburb_Oakleigh.South + 
         Suburb_Pascoe.Vale + Suburb_Prahran + Suburb_Preston + Suburb_Reservoir + 
         Suburb_Rosanna + Suburb_South.Yarra + Suburb_Strathmore + 
         Suburb_Sunshine + Suburb_Sunshine.North + Suburb_Sunshine.West + 
         Suburb_Surrey.Hills + Suburb_Templestowe.Lower + Suburb_Toorak + 
         Suburb_West.Footscray + Suburb_Williamstown + Type_t + Type_u + 
         Method_S + Method_SP + SellerG_Buxton + 
         SellerG_Fletchers + SellerG_Greg + SellerG_Jellis + SellerG_Kay + 
         SellerG_Marshall + SellerG_McGrath + SellerG_Miles + SellerG_RT + 
         SellerG_Sweeney + SellerG_Williams + SellerG_X__other__ + 
         CouncilArea_Darebin + CouncilArea_Hobsons.Bay + CouncilArea_Glen.Eira + 
         CouncilArea_Whitehorse + CouncilArea_Stonnington + CouncilArea_Maribyrnong + 
         CouncilArea_Banyule + CouncilArea_Bayside + CouncilArea_Yarra + 
         CouncilArea_Moonee.Valley + CouncilArea_Brimbank + CouncilArea_Manningham + 
         CouncilArea_Port.Phillip + CouncilArea_Monash + CouncilArea_Kingston + 
         CouncilArea_X__other__,data=t1)
summary(fit)

####

t2.pred=predict(fit,newdata=t2)
errors=t2$Price-t2.pred

rmse=errors**2 %>% mean() %>% sqrt()

##entire data

fit.final=lm(Price~.-Type_X__other__
             -Suburb_X__other__
             -CouncilArea_X
             -Distance,data=train)
sort(vif(fit.final),decreasing=TRUE)

fit.final=stats::step(fit.final)

formula(fit.final)

fit.final=lm(Price ~ Rooms + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + 
               Suburb_Airport.West + Suburb_Altona + Suburb_Altona.North + 
               Suburb_Armadale + Suburb_Avondale.Heights + Suburb_Balwyn + 
               Suburb_Balwyn.North + Suburb_Bentleigh.East + Suburb_Brighton + 
               Suburb_Brighton.East + Suburb_Brunswick + Suburb_Brunswick.East + 
               Suburb_Brunswick.West + Suburb_Bulleen + Suburb_Burwood + 
               Suburb_Camberwell + Suburb_Coburg + Suburb_Coburg.North + 
               Suburb_Essendon + Suburb_Fawkner + Suburb_Glen.Iris + Suburb_Glenroy + 
               Suburb_Hadfield + Suburb_Hampton + Suburb_Hawthorn + Suburb_Hawthorn.East + 
               Suburb_Heidelberg.Heights + Suburb_Heidelberg.West + Suburb_Ivanhoe + 
               Suburb_Keilor.East + Suburb_Kensington + Suburb_Kew + Suburb_Maidstone + 
               Suburb_Malvern + Suburb_Malvern.East + Suburb_Maribyrnong + 
               Suburb_Melbourne + Suburb_Newport + 
               Suburb_Niddrie + Suburb_Northcote + Suburb_Oakleigh.South + 
               Suburb_Pascoe.Vale + Suburb_Prahran + Suburb_Preston + Suburb_Reservoir + 
               Suburb_Rosanna + Suburb_South.Yarra + Suburb_Strathmore + 
               Suburb_Sunshine + Suburb_Sunshine.North + Suburb_Sunshine.West + 
               Suburb_Templestowe.Lower + Suburb_Toorak + 
               Suburb_West.Footscray + Suburb_Williamstown + Type_t + Type_u + 
               Method_S + Method_SP + Method_VB + Method_X__other__ + 
               SellerG_Greg + SellerG_Jellis + SellerG_Kay + 
               SellerG_Marshall + SellerG_Miles + SellerG_RT + 
               SellerG_X__other__ + CouncilArea_Darebin + 
               CouncilArea_Hobsons.Bay + CouncilArea_Glen.Eira + CouncilArea_Stonnington + 
               CouncilArea_Maribyrnong + CouncilArea_Banyule + CouncilArea_Bayside + 
               CouncilArea_Yarra + CouncilArea_Moonee.Valley + CouncilArea_Brimbank + 
               CouncilArea_Manningham + CouncilArea_Port.Phillip + CouncilArea_Monash + 
               CouncilArea_Kingston + CouncilArea_X__other__,data=train)
summary(fit.final)

test.pred=predict(fit.final,newdata=test)

plot(fit.final,1) # residual vs fitted values => non-linearity in the data exists or not

plot(fit.final,2) # errors are normal or not

plot(fit.final,3) # variance is constant or not

plot(fit.final,4) # outliers in the data if cook's distance >1

write.csv(test.pred,"Mayank_Phaugat_P1_part2.csv",row.names = F)

####









####DECISION TREES

h_train=read.csv("D:/Data Analytics/R/Projects/Downloads/housing_train.csv",stringsAsFactors = FALSE)
h_test=read.csv("D:/Data Analytics/R/Projects/Downloads/housing_test.csv",stringsAsFactors = FALSE)

glimpse(h_train)
vis_dat(h_train)

#CUSTOM FUNCTIONS
add_func=function(x){
  temp=data.frame(add=x)
  temp=temp %>% 
    separate(add,into=c('a1','a2'),sep=' ') %>% 
    mutate(No=a1,Street=a2) %>% 
    select(-No)
  
  return(temp[,'Street'])
}

#DATA PREP

#Suburb : categorical :: club low frequency data and create dummies
#Address : categorical :: drop
#Rooms : numeric :: 
#Type : categorical :: create dummies
#Price : numeric ::  
#Method : categorical ::  create dummies
#SellerG : categorical ::  club low frequency data and create dummies
#Distance : numeric :: custom function
#Postcode : categorical :: drop
#Bedroom2 : Numeric :: 
#Bathroom : numeric :: 
#Car : numeric :: 
#Landsize : numeric :: 
#BuildingArea : numeric :: 
#YearBuilt : numeric :: drop
#CouncilArea : numeric :: convert to catagorical and create dummies

dp_pipe=recipe(Price~.,data=h_train) %>% 
  update_role(Address,Postcode,YearBuilt,new_role='drop_vars') %>% 
  update_role(Suburb,Type,Method,SellerG,CouncilArea,new_role='to_dummies') %>% 
  
  step_rm(has_role('drop_vars')) %>% 
  step_mutate_at(CouncilArea,fn=as.character) %>% 
  step_mutate_at(Distance,fn=round) %>% 
  step_unknown(has_role('to_dummies'),new_level='__missing__') %>% 
  step_other(has_role('to_dummies'),threshold=0.005,other='__other__') %>% 
  step_dummy(has_role('to_dummies')) %>% 
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)

train=bake(dp_pipe,new_data=NULL)
test=bake(dp_pipe,new_data=h_test)

vis_dat(train,warn_large_data = FALSE)

#PARAMETER TUNING FOR REGRESSION

tree_model=decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>% 
  set_engine('rpart') %>% 
  set_mode('regression')

folds=vfold_cv(train,v=10)

tree_grid=grid_regular(cost_complexity(),tree_depth(),min_n(),levels=4)

doParallel::registerDoParallel()

my_res=tune_grid(
  tree_model,
  Price~.,
  resamples=folds,
  grid=tree_grid,
  metrics=metric_set(rmse),
  control=control_grid(verbose=TRUE)
)

autoplot(my_res)+theme_light()

fold_metrics=collect_metrics(my_res)

my_res %>% show_best()

final_tree_fit=tree_model %>% 
  finalize_model(select_best(my_res,'rmse')) %>% 
  fit(Price~.,data=train)

#FEATURE IMPORTANCE

final_tree_fit %>% 
  vip(geom='col',aesthetics=list(fill='midnightblue',alpha=0.8))+
  scale_y_continuous(expand=c(0,0))

#PLOT THE TREE

rpart.plot(final_tree_fit$fit)

#PREDICTIONS

train_pred=predict(final_tree_fit,new_data = train)
test_pred=predict(final_tree_fit,new_data = test)

####










####RANDOM FOREST

rf_model=rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>% 
  set_mode('regression') %>% 
  set_engine('ranger')

folds=vfold_cv(train,v=10)

rf_grid=grid_regular(mtry(c(5,30)),trees(c(100,500)),min_n(c(2,10)),levels=3)

doParallel::registerDoParallel()

my_res=tune_grid(
  rf_model,
  Price~.,
  resamples=folds,
  grid=rf_grid,
  metrics=metric_set(rmse),
  control=control_grid(verbose=TRUE)
)

autoplot(my_res)+theme_light()

fold_metrics=collect_metrics(my_res)

my_res %>% show_best()

final_rf_fit=rf_model %>% 
  set_engine('ranger',importance='permutation') %>% 
  finalize_model(select_best(my_res,'rmse')) %>% 
  fit(Price~.,data=train)

#VARIABLE IMPORTANCE

final_rf_fit %>% 
  vip(geom='col',aesthetics=list(fill='midnightblue',alpha=0.8))+
  scale_y_continuous(expand=c(0,0))

#PREDICTIONS

train_pred=predict(final_rf_fit,new_data = train)
test_pred=predict(final_rf_fit,new_data = test)

#PARTIAL DEPENDENCE PLOTS

model_explainer=explain_tidymodels(
  final_rf_fit,
  data=dplyr::select(train,-Price),
  y=as.integer(train$Price),
  verbose=FALSE
)

pdp=model_profile(
  model_explainer,
  variables='Distance',
  N=1000
)

plot(pdp)

write.csv(test_pred,'Mayank_Phaugat_P1_part2.csv',row.names = FALSE)






