mybank= read.csv("PL_XSELL-1.csv",1)

attach(mybank)
mybank= mybank[,-c(1:3,11,15:20,22:29,33:37,40)]

str(mybank)
mybank$GENDER = as.factor(mybank$GENDER)
mybank$OCCUPATION = as.factor(mybank$OCCUPATION)
mybank$AGE_BKT = as.factor(mybank$AGE_BKT)
mybank$ACC_TYPE = as.factor(mybank$ACC_TYPE)

library(dummies)
gender = dummy(mybank$GENDER)
occupation = dummy(mybank$OCCUPATION)
agebkt= dummy(mybank$AGE_BKT)
acctype= dummy(mybank$ACC_TYPE)

mybank = subset(mybank, select =-c(GENDER,OCCUPATION,AGE_BKT,ACC_TYPE))  
mybank = cbind(mybank, gender, occupation, agebkt, acctype)

rm(gender, occupation, agebkt, acctype)
str(mybank)

mybank = cbind(mybank, TARGET)
str(mybank)

table(mybank$TARGET)
target_Variable = mybank$TARGET

independent_Variable= subset(mybank, select = -c(TARGET))

library(vegan)

independent_Variable= decostand(independent_Variable,"range")

mybank= data.frame(independent_Variable, TARGET = target_Variable)

rm(independent_Variable, target_Variable)

set.seed(1)
num_Records= nrow(mybank)
train_index= sample(1:num_Records, round(num_Records * 0.8))
train_data= mybank[train_index,]
test_data= mybank[-train_index,]
rm(train_index, num_Records)

table(train_data$TARGET)
table(test_data$TARGET)

library(neuralnet)
set.seed(1234)
str(train_data)
nn = neuralnet(TARGET ~ BALANCE+SCR+HOLDING_PERIOD+LEN_OF_RLTN_IN_MNTH+NO_OF_L_CR_TXNS+NO_OF_L_DR_TXNS+FLG_HAS_CC+
                 AMT_MIN_BAL_NMC_CHGS+NO_OF_IW_CHQ_BNC_TXNS+NO_OF_OW_CHQ_BNC_TXNS+FLG_HAS_NOMINEE+FLG_HAS_OLD_LOAN+
                 GENDERF+GENDERM+GENDERO+OCCUPATIONPROF+OCCUPATIONSAL+OCCUPATIONSELF.EMP+OCCUPATIONSENP+AGE_BKT.25+
                 AGE_BKT.50+AGE_BKT26.30+AGE_BKT31.35+AGE_BKT36.40+AGE_BKT41.45+AGE_BKT46.50+ACC_TYPECA+ACC_TYPESA
               , data = train_data, hidden = 3, linear.output = F)

plot(nn)
predicted = factor(ifelse(nn$net.result[[1]] > 0.5, 1, 0))
(conf_Matrix = table(train_data$TARGET, predicted))
test_data_n=subset(test_data, select = -c(TARGET))
nn_predict = compute(nn, covariate = test_data_n)

rm(test_data_n)
nn_predict$net.result

predicted = factor(ifelse(nn_predict$net.result > 0.5, 1, 0))
(conf_Matrix<-table(test_data$TARGET, predicted))

nn = neuralnet(TARGET ~ BALANCE+SCR+HOLDING_PERIOD+LEN_OF_RLTN_IN_MNTH+NO_OF_L_CR_TXNS+NO_OF_L_DR_TXNS+FLG_HAS_CC+
                 AMT_MIN_BAL_NMC_CHGS+NO_OF_IW_CHQ_BNC_TXNS+NO_OF_OW_CHQ_BNC_TXNS+FLG_HAS_NOMINEE+FLG_HAS_OLD_LOAN+
                 GENDERF+GENDERM+GENDERO+OCCUPATIONPROF+OCCUPATIONSAL+OCCUPATIONSELF.EMP+OCCUPATIONSENP+AGE_BKT.25+
                 AGE_BKT.50+AGE_BKT26.30+AGE_BKT31.35+AGE_BKT36.40+AGE_BKT41.45+AGE_BKT46.50+ACC_TYPECA+ACC_TYPESA
               , data = train_data, hidden = 5, linear.output = F)
plot(nn)
predicted = factor(ifelse(nn$net.result[[1]] > 0.5, 1, 0))
(conf_Matrix = table(train_data$TARGET, predicted))
test_data_n=subset(test_data, select = -c(TARGET))
nn_predict = compute(nn, covariate = test_data_n)

rm(test_data_n)
nn_predict$net.result
predicted = factor(ifelse(nn_predict$net.result > 0.5, 1, 0))
(conf_Matrix<-table(test_data$TARGET, predicted))
