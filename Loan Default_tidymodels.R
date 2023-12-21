library(tidymodels)
library(visdat)
library(dplyr)
library(car)
library(pROC)
library(ggplot2)
library(tidyr)
library(ROCit)

def_dat = read.csv("C:/Users/mayan/Downloads/JRT/DA/default_data.csv", stringsAsFactors = FALSE)
state = read.csv("C:/Users/mayan/Downloads/JRT/DA/state.csv", stringsAsFactors = FALSE)
manuf = read.csv("C:/Users/mayan/Downloads/JRT/DA/manufacturer.csv", stringsAsFactors = FALSE)
branch = read.csv("C:/Users/mayan/Downloads/JRT/DA/branch.csv", stringsAsFactors = FALSE)

def_dat = merge(x = def_dat, y = state, by.x = 'State_ID', by.y = 'state_id', all.x = TRUE)
def_dat = merge(x = def_dat, y = manuf, by = 'manufacturer_id', all.x = TRUE)
def_dat = merge(x = def_dat, y = branch, by = 'branch_id', all.x = TRUE)

#AVG.ACCT.AGE

library(stringr)

def_dat[c('acct_age_Y', 'acct_age_M')] = str_split_fixed(def_dat$AVERAGE.ACCT.AGE, ' ', 2)
def_dat$acct_age_Y = as.numeric(gsub("[^0-9.-]", "", def_dat$acct_age_Y))
def_dat$acct_age_M = as.numeric(gsub("[^0-9.-]", "", def_dat$acct_age_M))
def_dat$AVERAGE.ACCT.AGE_new = def_dat$acct_age_Y*12 + def_dat$acct_age_M
def_dat = def_dat[,-c(38,45,46)]

#CREDIT.HISTORY.LENGTH

def_dat[c('cred_hist__Y', 'cred_hist_M')] = str_split_fixed(def_dat$CREDIT.HISTORY.LENGTH, ' ', 2)
def_dat$cred_hist__Y = as.numeric(gsub("[^0-9.-]", "", def_dat$cred_hist__Y))
def_dat$cred_hist_M = as.numeric(gsub("[^0-9.-]", "", def_dat$cred_hist_M))
def_dat$CREDIT.HISTORY.LENGTH_new = def_dat$cred_hist__Y*12 + def_dat$cred_hist_M
def_dat = def_dat[,-c(38,45,46)]

#DATA PREPARATION

# branch_id

def_dat$branch_id = as.character(def_dat$branch_id)

#manufacturer_id

def_dat$manufacturer_id = as.character(def_dat$manufacturer_id)

#State_ID

def_dat$State_ID = as.character(def_dat$State_ID)

#supplier_id

def_dat$supplier_id = as.character(def_dat$supplier_id)

#Current_pincode_ID

def_dat$Current_pincode_ID = as.character(def_dat$Current_pincode_ID)

#Employee_code_ID

def_dat$Employee_code_ID = as.character(def_dat$Employee_code_ID)

#Aadhar_flag

def_dat$Aadhar_flag = as.character(def_dat$Aadhar_flag)

#PAN_flag

def_dat$PAN_flag = as.character(def_dat$PAN_flag)

#VoterID_flag

def_dat$VoterID_flag = as.character(def_dat$VoterID_flag)

#Driving_flag

def_dat$Driving_flag = as.character(def_dat$Driving_flag)

#Passport_flag

def_dat$Passport_flag = as.character(def_dat$Passport_flag)

#PERFORM_CNS.SCORE.DESCRIPTION

def_dat$PERFORM_CNS.SCORE.DESCRIPTION = as.character(def_dat$PERFORM_CNS.SCORE.DESCRIPTION)

#NO.OF_INQUIRIES

def_dat$NO.OF_INQUIRIES = as.character(def_dat$NO.OF_INQUIRIES)

#event_state

def_dat$event_state = as.character(def_dat$event_state)

#event_count_manufacturer            

def_dat$event_count_manufacturer = as.character(def_dat$event_count_manufacturer)

#event_count_branch

def_dat$event_count_branch = as.character(def_dat$event_count_branch)

set.seed(2)
s=sample(1:nrow(def_dat),0.8*nrow(def_dat))
def_dat_train=def_dat[s,]
def_dat_test=def_dat[-s,]

def_dat_test$loan_default = NA

def_dat_train$loan_default = as.numeric(def_dat_train$loan_default)

dp_pipe=recipe(loan_default~.,data=def_dat_train) %>% 
  update_role(UniqueID,Date.of.Birth,MobileNo_Avl_Flag,SEC.NO.OF.ACCTS,SEC.ACTIVE.ACCTS,SEC.OVERDUE.ACCTS,SEC.CURRENT.BALANCE,SEC.SANCTIONED.AMOUNT,SEC.DISBURSED.AMOUNT,new_role='drop_vars') %>% 
  update_role(branch_id,manufacturer_id,State_ID,supplier_id,Current_pincode_ID,Employment.Type,DisbursalDate,Employee_code_ID,Aadhar_flag,PAN_flag,VoterID_flag,Driving_flag,Passport_flag,PERFORM_CNS.SCORE.DESCRIPTION,NO.OF_INQUIRIES,event_state,event_count_manufacturer,event_count_branch,new_role='to_dummies') %>% 
  
  step_rm(has_role('drop_vars')) %>% 
  
  step_unknown(has_role('to_dummies'),new_level='__missing__') %>% 
  step_other(has_role('to_dummies'),threshold=0.02,other='__other__') %>% 
  step_dummy(has_role('to_dummies')) %>% 
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)


train=bake(dp_pipe,new_data=NULL)
test=bake(dp_pipe,new_data=def_dat_test)

vis_dat(train, warn_large_data = FALSE)

#BREAKING DATA

set.seed(3)
s=sample(1:nrow(train),0.8*nrow(train))
t1=train[s,]
t2=train[-s,]

#AIC SCORE

log_fit=glm(loan_default~.
            -PRI.SANCTIONED.AMOUNT
            -PRI.DISBURSED.AMOUNT
            -PRIMARY.INSTAL.AMT
            -SEC.INSTAL.AMT
            -NEW.ACCTS.IN.LAST.SIX.MONTHS
            -branch_id_X136
            -branch_id_X152
            -branch_id_X19
            -branch_id_X3
            -branch_id_X48
            -branch_id_X2
            -branch_id_X5
            -branch_id_X61
            -Current_pincode_ID_X__other__
            -PERFORM_CNS.SCORE.DESCRIPTION_Very.High.Risk
            -State_ID_X14
            -event_state_X2518
            -event_state_X2583
            -branch_id_X67
            -event_count_branch_X2430
            -DisbursalDate_X24.10.2018
            -Employment.Type_Salaried
            -DisbursalDate_X30.08.2018
            -VoterID_flag_X1
            -PERFORM_CNS.SCORE.DESCRIPTION_Medium.Risk
            -event_count_branch_X840
            -event_count_branch_X656
            -event_count_branch_X1517
            -event_count_branch_X1694
            -State_ID_X11
            -event_state_X1596,data=t1,family='binomial')
summary(log_fit)

log_fit=stats::step(log_fit)

####

formula(log_fit)

log_fit=glm(loan_default ~ disbursed_amount + asset_cost + ltv + PERFORM_CNS.SCORE + 
              PRI.NO.OF.ACCTS + PRI.ACTIVE.ACCTS + PRI.OVERDUE.ACCTS + 
              PRI.CURRENT.BALANCE + DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS + 
              AVERAGE.ACCT.AGE_new + CREDIT.HISTORY.LENGTH_new + branch_id_X146 + 
              branch_id_X16 + branch_id_X18 + branch_id_X34 + branch_id_X36 + 
              branch_id_X__other__ + manufacturer_id_X45 + manufacturer_id_X48 + 
              manufacturer_id_X49 + manufacturer_id_X51 + manufacturer_id_X86 + 
              manufacturer_id_X__other__ + State_ID_X13 + State_ID_X15 + 
              State_ID_X18 + State_ID_X3 + State_ID_X4 + State_ID_X5 + 
              State_ID_X6 + State_ID_X7 + State_ID_X8 + State_ID_X9 + State_ID_X__other__ + 
              supplier_id_X__other__ + Employment.Type_Self.employed + 
              DisbursalDate_X23.10.2018 + DisbursalDate_X25.10.2018 + DisbursalDate_X26.10.2018 + 
              DisbursalDate_X30.10.2018 + DisbursalDate_X31.08.2018 + DisbursalDate_X31.10.2018 + 
              DisbursalDate_X__other__ + Employee_code_ID_X__other__ + 
              Aadhar_flag_X1 + PAN_flag_X1 + Driving_flag_X1 + Passport_flag_X__other__ + 
              PERFORM_CNS.SCORE.DESCRIPTION_Low.Risk + PERFORM_CNS.SCORE.DESCRIPTION_No.Bureau.History.Available + 
              PERFORM_CNS.SCORE.DESCRIPTION_Not.Scored + PERFORM_CNS.SCORE.DESCRIPTION_Very.Low.Risk + 
              NO.OF_INQUIRIES_X1 + NO.OF_INQUIRIES_X2 + NO.OF_INQUIRIES_X__other__ + 
              event_count_branch_X1362 + event_count_branch_X1506 + event_count_branch_X1669 + 
              event_count_branch_X26 + event_count_branch_X3412,data=t1,family='binomial')
summary(log_fit)

#PREDICTION ON t2 WHITH AUC SCORE

val.score=predict(log_fit,newdata=t2,type='response')
pROC::auc(pROC::roc(t2$loan_default,val.score))

#FITTING THE MODEL ON ENTIRE DATA

log_fit.final=glm(loan_default~.
                  -DisbursalDate_X30.08.2018
                  -Employment.Type_Salaried
                  -branch_id_X48
                  -PRIMARY.INSTAL.AMT
                  -State_ID_X14
                  -event_state_X2518
                  -event_state_X2583
                  -event_count_branch_X656
                  -SEC.INSTAL.AMT
                  -PERFORM_CNS.SCORE.DESCRIPTION_Very.High.Risk
                  -branch_id_X19
                  -event_count_branch_X1694
                  -Current_pincode_ID_X__other__
                  -DisbursalDate_X24.10.2018
                  -PRI.DISBURSED.AMOUNT
                  -PRI.SANCTIONED.AMOUNT
                  -branch_id_X152
                  -event_count_branch_X840
                  -branch_id_X136
                  -event_count_branch_X1517
                  -VoterID_flag_X1
                  -NEW.ACCTS.IN.LAST.SIX.MONTHS
                  -supplier_id_X__other__
                  -State_ID_X11
                  -event_state_X1596
                  -PERFORM_CNS.SCORE.DESCRIPTION_Medium.Risk
                  -branch_id_X61
                  -event_count_branch_X1362
                  -branch_id_X5
                  -event_count_branch_X1506,data=train,family='binomial')
summary(log_fit.final)

log_fit.final=stats::step(log_fit.final)

formula(log_fit.final)

log_fit.final=glm(loan_default ~ disbursed_amount + asset_cost + ltv + PERFORM_CNS.SCORE + 
                    PRI.NO.OF.ACCTS + PRI.ACTIVE.ACCTS + PRI.OVERDUE.ACCTS + 
                    PRI.CURRENT.BALANCE + DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS + 
                    AVERAGE.ACCT.AGE_new + CREDIT.HISTORY.LENGTH_new + branch_id_X146 + 
                    branch_id_X16 + branch_id_X18 + branch_id_X2 + branch_id_X3 + 
                    branch_id_X34 + branch_id_X36 + branch_id_X67 + branch_id_X__other__ + 
                    manufacturer_id_X45 + manufacturer_id_X48 + manufacturer_id_X49 + 
                    manufacturer_id_X51 + manufacturer_id_X86 + manufacturer_id_X__other__ + 
                    State_ID_X13 + State_ID_X15 + State_ID_X18 + State_ID_X3 + 
                    State_ID_X4 + State_ID_X5 + State_ID_X6 + State_ID_X7 + State_ID_X8 + 
                    State_ID_X9 + State_ID_X__other__ + Employment.Type_Self.employed + 
                    DisbursalDate_X23.10.2018 + DisbursalDate_X25.10.2018 + DisbursalDate_X26.10.2018 + 
                    DisbursalDate_X30.10.2018 + DisbursalDate_X31.08.2018 + DisbursalDate_X31.10.2018 + 
                    DisbursalDate_X__other__ + Employee_code_ID_X__other__ + 
                    Aadhar_flag_X1 + PAN_flag_X1 + Driving_flag_X1 + Passport_flag_X__other__ + 
                    PERFORM_CNS.SCORE.DESCRIPTION_Low.Risk + PERFORM_CNS.SCORE.DESCRIPTION_No.Bureau.History.Available + 
                    PERFORM_CNS.SCORE.DESCRIPTION_Not.Scored + PERFORM_CNS.SCORE.DESCRIPTION_Very.Low.Risk + 
                    NO.OF_INQUIRIES_X1 + NO.OF_INQUIRIES_X2 + NO.OF_INQUIRIES_X__other__ + 
                    event_count_branch_X26,data=train,family='binomial')
summary(log_fit.final)

#FINDING CUTOFF FOR HARD CLASSES

train.score=predict(log_fit.final,newdata=train,type='response')
real=train$loan_default

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

##0.69 AUC Score

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

def_dat = read.csv("C:/Users/mayan/Downloads/JRT/DA/default_data.csv", stringsAsFactors = FALSE)
state = read.csv("C:/Users/mayan/Downloads/JRT/DA/state.csv", stringsAsFactors = FALSE)
manuf = read.csv("C:/Users/mayan/Downloads/JRT/DA/manufacturer.csv", stringsAsFactors = FALSE)
branch = read.csv("C:/Users/mayan/Downloads/JRT/DA/branch.csv", stringsAsFactors = FALSE)

def_dat = merge(x = def_dat, y = state, by.x = 'State_ID', by.y = 'state_id', all.x = TRUE)
def_dat = merge(x = def_dat, y = manuf, by = 'manufacturer_id', all.x = TRUE)
def_dat = merge(x = def_dat, y = branch, by = 'branch_id', all.x = TRUE)

#AVG.ACCT.AGE

library(stringr)

def_dat[c('acct_age_Y', 'acct_age_M')] = str_split_fixed(def_dat$AVERAGE.ACCT.AGE, ' ', 2)
def_dat$acct_age_Y = as.numeric(gsub("[^0-9.-]", "", def_dat$acct_age_Y))
def_dat$acct_age_M = as.numeric(gsub("[^0-9.-]", "", def_dat$acct_age_M))
def_dat$AVERAGE.ACCT.AGE_new = def_dat$acct_age_Y*12 + def_dat$acct_age_M
def_dat = def_dat[,-c(38,45,46)]

#CREDIT.HISTORY.LENGTH

def_dat[c('cred_hist__Y', 'cred_hist_M')] = str_split_fixed(def_dat$CREDIT.HISTORY.LENGTH, ' ', 2)
def_dat$cred_hist__Y = as.numeric(gsub("[^0-9.-]", "", def_dat$cred_hist__Y))
def_dat$cred_hist_M = as.numeric(gsub("[^0-9.-]", "", def_dat$cred_hist_M))
def_dat$CREDIT.HISTORY.LENGTH_new = def_dat$cred_hist__Y*12 + def_dat$cred_hist_M
def_dat = def_dat[,-c(38,45,46)]

#DATA PREPARATION

# branch_id

def_dat$branch_id = as.character(def_dat$branch_id)

#manufacturer_id

def_dat$manufacturer_id = as.character(def_dat$manufacturer_id)

#State_ID

def_dat$State_ID = as.character(def_dat$State_ID)

#supplier_id

def_dat$supplier_id = as.character(def_dat$supplier_id)

#Current_pincode_ID

def_dat$Current_pincode_ID = as.character(def_dat$Current_pincode_ID)

#Employee_code_ID

def_dat$Employee_code_ID = as.character(def_dat$Employee_code_ID)

#Aadhar_flag

def_dat$Aadhar_flag = as.character(def_dat$Aadhar_flag)

#PAN_flag

def_dat$PAN_flag = as.character(def_dat$PAN_flag)

#VoterID_flag

def_dat$VoterID_flag = as.character(def_dat$VoterID_flag)

#Driving_flag

def_dat$Driving_flag = as.character(def_dat$Driving_flag)

#Passport_flag

def_dat$Passport_flag = as.character(def_dat$Passport_flag)

#PERFORM_CNS.SCORE.DESCRIPTION

def_dat$PERFORM_CNS.SCORE.DESCRIPTION = as.character(def_dat$PERFORM_CNS.SCORE.DESCRIPTION)

#NO.OF_INQUIRIES

def_dat$NO.OF_INQUIRIES = as.character(def_dat$NO.OF_INQUIRIES)

#event_state

def_dat$event_state = as.character(def_dat$event_state)

#event_count_manufacturer            

def_dat$event_count_manufacturer = as.character(def_dat$event_count_manufacturer)

#event_count_branch

def_dat$event_count_branch = as.character(def_dat$event_count_branch)

set.seed(2)
s=sample(1:nrow(def_dat),0.8*nrow(def_dat))
def_dat_train=def_dat[s,]
def_dat_test=def_dat[-s,]

def_dat_test$loan_default = NA

def_dat_train$loan_default = as.factor(def_dat_train$loan_default)

dp_pipe=recipe(loan_default~.,data=def_dat_train) %>% 
  update_role(UniqueID,Date.of.Birth,MobileNo_Avl_Flag,SEC.NO.OF.ACCTS,SEC.ACTIVE.ACCTS,SEC.OVERDUE.ACCTS,SEC.CURRENT.BALANCE,SEC.SANCTIONED.AMOUNT,SEC.DISBURSED.AMOUNT,new_role='drop_vars') %>% 
  update_role(branch_id,manufacturer_id,State_ID,supplier_id,Current_pincode_ID,Employment.Type,DisbursalDate,Employee_code_ID,Aadhar_flag,PAN_flag,VoterID_flag,Driving_flag,Passport_flag,PERFORM_CNS.SCORE.DESCRIPTION,NO.OF_INQUIRIES,event_state,event_count_manufacturer,event_count_branch,new_role='to_dummies') %>% 
  
  step_rm(has_role('drop_vars')) %>% 
  
  step_unknown(has_role('to_dummies'),new_level='__missing__') %>% 
  step_other(has_role('to_dummies'),threshold=0.02,other='__other__') %>% 
  step_dummy(has_role('to_dummies')) %>% 
  step_impute_median(all_numeric(),-all_outcomes())

dp_pipe=prep(dp_pipe)


train=bake(dp_pipe,new_data=NULL)
test=bake(dp_pipe,new_data=def_dat_test)

write.csv(train,'train.csv',row.names = FALSE)

vis_dat(train, warn_large_data = FALSE)

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
  loan_default~.,
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
  fit(loan_default~.,data=train)

#VARIABLE IMPORTANCE

final_rf_fit %>% 
  vip(geom='col',aesthetics=list(fill='midnightblue',alpha=0.8))+
  scale_y_continuous(expand=c(0,0))

#PREDICTIONS

train_pred=predict(final_rf_fit,new_data = train,type='prob') %>% select(.pred_1)
test_pred=predict(final_rf_fit,new_data = test,type='prob') %>% select(.pred_1)

#FINDING CUTOFF FOR HARD CLASSES

train.score=train_pred$.pred_1

real=train$loan_default

#KS PLOT

rocit=ROCit::rocit(score=train.score,
                   class=real)
kplot=ROCit::ksplot(rocit)

#CUTOFF ON BASIS OF KS

my_cutoff=kplot$'KS Cutoff'

#HARD CLASSES

test_hard_class=as.numeric(test_pred>my_cutoff)


#AUC SCORE 0.729










