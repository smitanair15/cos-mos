# cos-mos
Loan Prediction Modelling
setwd("C:/Users")

TrainData<-read.csv("train_loan.csv")
TestData<-read.csv("test_loan.csv")

#Check
View(TrainData)
summary(TrainData)
str(TrainData)
names(TrainData)
str(TrainData)
names(TrainData)
table(is.na(TrainData))
table(is.na(TestData))

#Exploratory TrainData analysis

#Dependent variable
#Loan_Status
TrainData$NewLoan_Status<-ifelse(TrainData$Loan_Status=="Y",1, 0)
TrainData$NewLoan_Status<-as.factor(TrainData$NewLoan_Status)


#Missing Value Treatment
#Gender Category miising value 13
attach(TrainData)
mode<-function(TrainData$Gender) 
{
  uni <-unique(TrainData$Gender)
  uni[which.max(tabulate(match(TrainData$Gender,uni)))]
}
TrainData$Gender[TrainData$Gender==""]<-"Male"
TestData$Gender[TestData$Gender==""]<-"Male"

#Married  Category missing value 3
mode<-function (Married)           
{uni <-unique(TrainData$Married)
uni[which.max(tabulate(match(TrainData$Married,uni)))]
}
TrainData$Married[TrainData$Married==""]<-"Yes"
TestData$Married[TestData$Married==""]<-"Yes"

#Dependents category missing value 15
mode<-function (TrainData$Dependents)           
{uni <-unique(TrainData$Dependents)
uni[which.max(tabulate(match(TrainData$Dependents,uni)))]
}

TrainData$Dependents[TrainData$Dependents==""]<-"0"
TestData$Dependents[TestData$Dependents==""]<-"0"

#Education category no missing value

#Self_Employed category missing value 32
mode<-function (TrainData$Self_Employed)           
{uni <-unique(TrainData$Self_Employed)
uni[which.max(tabulate(match(TrainData$Self_Employed,uni)))]
}
TrainData$Self_Employed[TrainData$Self_Employed==""]<-"No"
TestData$Self_Employed[TestData$Self_Employed==""]<-"No"



#ApplicantIncome integer convert into numeric
TrainData$ApplicantIncome<-as.numeric(TrainData$ApplicantIncome)
TestData$ApplicantIncome<-as.numeric(TestData$ApplicantIncome)

#CoapplicantIncome numeric no missing value
TestData$CoapplicantIncome<-as.numeric(TestData$CoapplicantIncome)

#LoanAmount Nas 22 mean imputation
TrainData$NewLoanAmount = ifelse(is.na(TrainData$LoanAmount),
                                 ave(TrainData$LoanAmount, FUN = function(x) mean(x, na.rm = TRUE)),
                                 TrainData$LoanAmount)

TrainData$NewLoanAmount<-as.numeric(TrainData$NewLoanAmount)


TestData$NewLoanAmount = ifelse(is.na(TestData$LoanAmount),
                                ave(TestData$LoanAmount, FUN = function(x) mean(x, na.rm = TRUE)),
                                TestData$LoanAmount)
TestData$NewLoanAmount<-as.numeric(TestData$NewLoanAmount)


#Loan_Amount_Term NAs 14 int convert into factors

mode<-function (TrainData$Loan_Amount_Term)           
{uni <-unique(TrainData$)Loan_Amount_Term
uni[which.max(tabulate(match(TrainData$Loan_Amount_Term,uni)))]
}

levels(TrainData$Loan_Amount_Term)<-c(levels(TrainData$Loan_Amount_Term),"360")
TrainData$Loan_Amount_Term[is.na(TrainData$Loan_Amount_Term)] <- "360"

TrainData$Loan_Amount_Term<-as.factor(TrainData$Loan_Amount_Term)

levels(TestData$Loan_Amount_Term)<-c(levels(TestData$Loan_Amount_Term),"360")
TestData$Loan_Amount_Term[is.na(TestData$Loan_Amount_Term)] <- "360"

TestData$Loan_Amount_Term<-as.factor(TestData$Loan_Amount_Term)


#Credit_History Nas 50 int convert into factors

mode<-function (TrainData$Credit_History)           
{uni <-unique(TrainData$Credit_History)
uni[which.max(tabulate(match(TrainData$Credit_History,uni)))]
}

levels(TrainData$Credit_History)<-c(levels(TrainData$Credit_History),"1")
TrainData$Credit_History[is.na(TrainData$Credit_History)] <- "1"

TrainData$Credit_History<-as.factor(TrainData$Credit_History)

levels(TestData$Credit_History)<-c(levels(TestData$Credit_History),"1")
TestData$Credit_History[is.na(TestData$Credit_History)] <- "1"

TestData$Credit_History<-as.factor(TestData$Credit_History)


# Property_Area factor no missing values

str(TrainData)
str(TestData)
summary(TrainData)
summary(TestData)



#Run Regression
Fit1<-glm(NewLoan_Status~Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+CoapplicantIncome+NewLoanAmount+Loan_Amount_Term+Credit_History+
            Property_Area, data = TrainData, family = binomial(logit))

Fit11<-glm(NewLoan_Status~ApplicantIncome+CoapplicantIncome+NewLoanAmount,TrainData = TrainData, family = binomial(logit))
vif(Fit11)
summary(Fit1)
vif(Fit1)
install.packages("VIF")
library(VIF)

Fit2<-glm(NewLoan_Status~ApplicantIncome+CoapplicantIncome+NewLoanAmount+Credit_History+
            Property_Area,TrainData = TrainData, family = binomial)
summary(Fit2)

Fit3<-glm(NewLoan_Status~ApplicantIncome+Credit_History+
            Property_Area,TrainData = TrainData, family = binomial)
summary(Fit3)

Fit4<-glm(NewLoan_Status~Married+Credit_History+Property_Area, data = TrainData, family = binomial(logit))
summary(Fit4)

#predic using test TrainData
pred<-predict(Fit4,data=TestData,type='response')

pred

write.csv(pred,"C:/Users/Smita/Desktop/Smita_ASUS/Work/Loan_Prediction")

#Storing Model Performance
install.packages("gplots")
library(ROCR)

pref<-performance(prediction(pred,TrainData$NewLoan_Status),"auc")
pref

