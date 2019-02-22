library(Metrics)
library(dplyr)
train<-read.csv('train.csv',stringsAsFactors=FALSE)
test<-read.csv('test.csv',stringsAsFactors=FALSE)

extractFeatures <- function(data, istrain=TRUE) {
  features <- c("Pclass",
                "Sex",
                "Age",
                "SibSp",
                "Parch",
                "Fare",
                "Cabin",
                "Embarked")
  fea <- data[,features]
  fea$Sex[is.na(fea$Sex)] <- "female"
  fea$Embarked[fea$Embarked==""] = "S"
  fea$Sex <- fea$Sex[fea$Sex == "male"] <- "1"
  fea$Sex <- fea$Sex[fea$Sex == "female"] <- "0"
  fea$Sex<-as.numeric(as.character(fea$Sex))
  fea$Embarked[fea$Embarked=="S"] <- "0"
  fea$Embarked[fea$Embarked=="Q"] <- "1"
  fea$Embarked[fea$Embarked=="C"] <- "2"
  
  fea[substr(fea$Cabin,0,1)=="A", "Cabin"] <-"1"
  fea[substr(fea$Cabin,0,1)=="B", "Cabin"] <-"2"
  fea[substr(fea$Cabin,0,1)=="C", "Cabin"] <-"3"
  fea[substr(fea$Cabin,0,1)=="D", "Cabin"] <-"4"
  fea[substr(fea$Cabin,0,1)=="E", "Cabin"] <-"5"
  fea[substr(fea$Cabin,0,1)=="F", "Cabin"] <-"6"
  fea[substr(fea$Cabin,0,1)=="G", "Cabin"] <-"7"
  fea[substr(fea$Cabin,0,1)=="T", "Cabin"] <-"8"
  fea[length(fea$Cabin)==0, "Cabin"] <-"0"
  
  fea$Cabin<-as.numeric(fea$Cabin)
  
  fea[is.na(fea)] <- 0
  fea[] <- lapply(fea, function(x) as.numeric(as.character(x)))
  
  if(istrain)
    fea$Survived <- as.numeric(data$Survived)
  return(fea)
}

xtrain <-extractFeatures(train,TRUE)
xtest <-extractFeatures(test,FALSE)

gp <-function(data)
{
  p<-0.200000*tanh(((((31.006277) * ((((((data$Embarked) + (data$Cabin))/2.0)) + (((((sin((tanh((data$Parch))))) * (3.0))) - (data$Pclass))))))) * 2.0)) +
    0.200000*tanh(((31.006277) * (((((data$Age) * (data$Cabin))) + (((((0.318310) - (data$Pclass))) + (pmin((2.0), (((data$Parch) * 2.0)))))))))) +
    0.200000*tanh(ifelse(rep(1.,NROW(data))*(ifelse(rep(1.,NROW(data))*(data$SibSp>0.),data$Cabin,sin((data$Parch)))>0.),(7.90205097198486328),(((((((data$Cabin) + (data$Fare))/2.0)) - (9.869604))) - (31.006277)))) +
    0.200000*tanh(((((((((((tanh(((((0.636620) < (data$Parch))*1.)))) * 2.0)) - (data$Pclass))) + (((data$Embarked) + (sin((data$Pclass))))))) * 2.0)) * 2.0)) +
    0.200000*tanh(((data$Fare) * (ifelse(rep(1.,NROW(data))*(data$Embarked>0.),sin((data$SibSp)),((((data$Cabin) + (((sin((data$Parch))) + (2.0))))) - (data$Pclass)))))) +
    0.200000*tanh(((data$Fare) - (pmax((((data$Age) + (((ifelse(rep(1.,NROW(data))*(sin((data$Fare))>0.),(9.26776313781738281),-3.0)) * (data$Pclass))))), (2.0))))) +
    0.200000*tanh(pmin(((-1.0*((((data$Age) + (((-3.0) - (31.006277)))))))), (((((cos((data$Fare))) * (data$Age))) * (data$Age))))) +
    0.200000*tanh(((((ifelse(rep(1.,NROW(data))*(sin((pmax((sin((((data$Embarked) * 2.0)))), (((data$Age) * 2.0)))))>0.),cos((((data$SibSp) / 2.0))),-3.0)) * 2.0)) * 2.0)) +
    0.200000*tanh(ifelse(rep(1.,NROW(data))*(pmax((data$Embarked), (data$Age))>0.),(((9.0)) * (((((((((9.0)) + (data$Pclass))/2.0)) + (31.006277))) - (data$Age)))),-3.0)) +
    0.200000*tanh(((((cos((ifelse(rep(1.,NROW(data))*(sin((data$Fare))>0.),data$Pclass,pmin((((data$Fare) - (9.869604))), (((9.869604) * (data$SibSp)))))))) * 2.0)) * 2.0)) +
    0.200000*tanh(((data$Fare) * (cos((((data$Fare) * (ifelse(rep(1.,NROW(data))*(data$Parch>0.),0.318310,ifelse(rep(1.,NROW(data))*(data$SibSp>0.),((0.318310) * (1.570796)),(9.0)))))))))) +
    0.200000*tanh(((sin((ifelse(rep(1.,NROW(data))*(sin(((((sin((2.0))) + (data$Age))/2.0)))>0.),pmax(((12.83479881286621094)), (((data$Age) - (data$Embarked)))),data$Fare)))) * 2.0)) +
    0.200000*tanh(((((sin((((sin((data$SibSp))) + (pmax((((3.141593) * 2.0)), ((((data$Cabin) + (data$Age))/2.0)))))))) * (data$Age))) * (data$Fare))) +
    0.199844*tanh(((((sin((((data$Age) * (((3.141593) - (cos((ifelse(rep(1.,NROW(data))*(data$Cabin>0.),data$Cabin,tanh((sin((data$Age)))))))))))))) * 2.0)) * 2.0)) +
    0.200000*tanh(((sin((((((cos((pmax((pmax(((10.0)), (data$Age))), (((((data$Fare) * 2.0)) * 2.0)))))) + (data$Parch))) - (data$Cabin))))) * 2.0)) +
    0.200000*tanh(((data$Fare) * (cos((ifelse(rep(1.,NROW(data))*(cos((((data$Fare) * (data$Fare))))>0.),((0.318310) * (data$Fare)),((data$Pclass) * (9.869604)))))))) +
    0.200000*tanh(ifelse(rep(1.,NROW(data))*(ifelse(rep(1.,NROW(data))*(data$Age>0.),data$Embarked,data$Parch)>0.),((((((data$Embarked) - (data$Parch))) * 2.0)) * 2.0),((sin((((data$Fare) * 2.0)))) * 2.0))) +
    0.200000*tanh(((((sin((((data$Age) * (pmin((pmin((data$Fare), (data$Age))), (ifelse(rep(1.,NROW(data))*(data$SibSp>0.),(3.80433893203735352),cos((data$Fare)))))))))) * 2.0)) * 2.0)) +
    0.200000*tanh(cos((pmax((pmax((3.141593), (((data$Cabin) * (((((data$Age) * 2.0)) * 2.0)))))), (((((((data$Fare) * 2.0)) * 2.0)) * 2.0)))))) +
    0.200000*tanh(((sin((((((sin(((((((6.0)) * (data$Fare))) * 2.0)))) + (sin((((((data$Fare) * (data$Fare))) * 2.0)))))) * 2.0)))) * 2.0)) +
    0.200000*tanh(((pmin((sin(((((8.0)) * (sin(((((8.0)) * (pmax(((9.0)), (data$Fare))))))))))), (((data$Fare) - ((8.0)))))) * 2.0)) +
    0.200000*tanh(((data$Age) * (sin((((data$Fare) * (((data$Age) + (ifelse(rep(1.,NROW(data))*(sin((((data$Fare) + (data$Age))))>0.),-3.0,1.570796)))))))))) +
    0.200000*tanh((((((-1.0*((((((data$Embarked) * 2.0)) * 2.0))))) + (data$Fare))) * (cos((((data$Fare) * (((((tanh((data$Pclass))) * 2.0)) * 2.0)))))))) +
    0.193436*tanh(((((((cos(((((ifelse(rep(1.,NROW(data))*(sin((data$Parch))>0.),data$Age,data$Parch)) + ((((data$Fare) + (data$Age))/2.0)))/2.0)))) * 2.0)) * 2.0)) * 2.0)) +
    0.200000*tanh(((((((sin((((data$Fare) * ((((data$Age) + (((tanh((3.141593))) - (data$Cabin))))/2.0)))))) * (data$Fare))) * 2.0)) * (data$Fare))) +
    0.200000*tanh(((sin((((cos((data$SibSp))) * (((((ifelse(rep(1.,NROW(data))*(data$Embarked>0.),data$Age,((data$Age) * 2.0))) + (data$Age))) + ((1.0)))))))) * 2.0)) +
    0.200000*tanh(((((sin((data$Parch))) + (ifelse(rep(1.,NROW(data))*(cos((data$Fare))>0.),((data$Cabin) - (sin((data$SibSp)))),sin((data$Parch)))))) - (data$SibSp))) +
    0.200000*tanh(((sin((((((((((((data$Fare) < (data$Age))*1.)) * 2.0)) * 2.0)) + ((((pmax((data$Fare), (data$Age))) + (-3.0))/2.0)))/2.0)))) * 2.0)) +
    0.200000*tanh(((((data$Age) * (((data$Age) - ((9.22764873504638672)))))) * (sin(((((((7.99594593048095703)) * (((data$Fare) - ((7.99595642089843750)))))) * (data$Age))))))) +
    0.200000*tanh(sin((ifelse(rep(1.,NROW(data))*(data$Embarked>0.),(((ifelse(rep(1.,NROW(data))*(data$SibSp>0.),3.0,(10.59025096893310547))) + (data$Age))/2.0),((((10.95433044433593750)) + (pmax(((10.95433425903320312)), (data$Fare))))/2.0))))) +
    0.200000*tanh(ifelse(rep(1.,NROW(data))*(data$Age>0.),ifelse(rep(1.,NROW(data))*(data$SibSp>0.),((data$Age) - (data$Fare)),cos((data$Age))),sin((((31.006277) * (((data$Fare) / 2.0))))))) +
    0.089041*tanh(((((((9.869604) - (data$Age))) * (ifelse(rep(1.,NROW(data))*(data$Age>0.),9.869604,((sin((9.869604))) + (sin((data$Age)))))))) * 2.0)) +
    0.200000*tanh(ifelse(rep(1.,NROW(data))*(data$SibSp>0.),data$Cabin,ifelse(rep(1.,NROW(data))*(sin((data$Parch))>0.),data$Age,((1.570796) * (sin((((((data$Fare) * (data$Age))) * 2.0)))))))) +
    0.200000*tanh(((sin((((sin((data$Fare))) - ((((-1.0*((((sin((sin((((data$Age) / 2.0)))))) * 2.0))))) - (((data$Fare) * 2.0)))))))) * 2.0)) +
    0.197382*tanh(((((ifelse(rep(1.,NROW(data))*(data$Fare>0.),(-1.0*((cos((((data$Age) + (((data$Pclass) + ((-1.0*((data$Embarked)))))))))))),(-1.0*((data$Pclass))))) * 2.0)) * 2.0)) +
    0.200000*tanh(((ifelse(rep(1.,NROW(data))*(data$Cabin>0.),sin((sin((data$Cabin)))),data$Age)) * (((data$Age) * (cos((((((31.006277) * 2.0)) * (data$Fare))))))))) +
    0.200000*tanh(((((((sin((data$Parch))) + (pmax(((((sin((sin((sin((data$Fare))))))) < (0.636620))*1.)), (data$Embarked))))/2.0)) > (cos((data$Age))))*1.)) +
    0.157648*tanh(((((31.006277) * 2.0)) * (sin((((sin((data$Age))) * (((((((9.869604) * (sin((-2.0))))) * 2.0)) * 2.0)))))))) +
    0.200000*tanh(((-3.0) * (ifelse(rep(1.,NROW(data))*(data$SibSp>0.),data$Age,sin((((pmax((data$Age), (data$Parch))) * ((9.71326065063476562))))))))) +
    0.200000*tanh(sin(((((((ifelse(rep(1.,NROW(data))*(sin((((data$SibSp) * (((data$SibSp) * (((data$Fare) / 2.0)))))))>0.),3.141593,data$Age)) / 2.0)) + (data$SibSp))/2.0)))) +
    0.200000*tanh(sin((((data$Pclass) * ((((((data$Fare) + (((-3.0) * (ifelse(rep(1.,NROW(data))*(data$Age>0.),data$Cabin,(((31.006277) < (data$Fare))*1.))))))/2.0)) / 2.0)))))) +
    0.200000*tanh(cos(((((((((data$Fare) + (data$Pclass))/2.0)) * ((((data$Fare) + ((5.0)))/2.0)))) + (cos((((sin((data$Fare))) * (data$Age))))))))) +
    0.200000*tanh(((31.006277) * (((data$Embarked) * (((((data$Embarked) * (((31.006277) - (tanh(((1.0)))))))) - (data$Age))))))) +
    0.200000*tanh(((data$Parch) + (((data$Parch) * (((pmax((data$Age), (data$Pclass))) * (sin((((pmax((data$Embarked), (data$Age))) - (data$Parch))))))))))) +
    0.200000*tanh(((((cos((pmax((((((data$Fare) - (data$SibSp))) * (1.570796))), (((((data$Age) - (data$Fare))) / 2.0)))))) * 2.0)) * 2.0)) +
    0.200000*tanh(((data$Fare) * (sin(((((8.0)) * (((((data$Fare) * (((((31.006277) * 2.0)) * 2.0)))) + (((31.006277) * 2.0)))))))))) +
    0.200000*tanh(((((((((data$Cabin) + (data$Fare))/2.0)) + (-3.0))/2.0)) * (cos(((((((data$Fare) + (data$Cabin))/2.0)) * (((-3.0) - (data$Parch))))))))) +
    0.200000*tanh(sin((((data$Pclass) * (((ifelse(rep(1.,NROW(data))*(data$SibSp>0.),data$SibSp,data$Fare)) * (data$Fare))))))) +
    0.200000*tanh(((((((sin((((((sin((data$Pclass))) * 2.0)) - ((((data$Age) + (ifelse(rep(1.,NROW(data))*(data$SibSp>0.),data$SibSp,31.006277)))/2.0)))))) * 2.0)) * 2.0)) * 2.0)) +
    0.200000*tanh(sin((((((cos((sin((ifelse(rep(1.,NROW(data))*(data$Cabin>0.),ifelse(rep(1.,NROW(data))*(data$Age>0.),data$Fare,data$SibSp),data$Embarked)))))) + (data$Age))) * (3.141593))))) +
    0.200000*tanh(((sin((((((data$Cabin) + (((2.0) + (data$Fare))))) * (((data$Age) * (((data$Cabin) + (3.141593))))))))) * (data$Age))) +
    0.200000*tanh(((sin((((cos((data$Fare))) * (((data$Fare) + (((3.0) + (((sin((data$Age))) + (data$Age))))))))))) * 2.0)) +
    0.200000*tanh(((ifelse(rep(1.,NROW(data))*(data$SibSp>0.),((data$Age) + (data$Embarked)),data$Age)) * (sin((((data$Cabin) * (((((data$Age) + (data$Cabin))) * 2.0)))))))) +
    0.200000*tanh(((((data$Cabin) * (((data$Cabin) * 2.0)))) * (sin((((data$Age) * (tanh((sin((-1.0))))))))))) +
    0.200000*tanh(((data$Fare) * (((((data$Embarked) * (31.006277))) * (sin((((ifelse(rep(1.,NROW(data))*(data$Parch>0.),(11.25038909912109375),data$Age)) * (((data$Fare) * 2.0)))))))))) +
    0.200000*tanh(((sin((pmax((((data$Fare) * (sin((((sin((data$Age))) * 2.0)))))), (((((data$Fare) * 2.0)) * (data$Parch))))))) * (data$Age))) +
    0.200000*tanh(((sin(((-1.0*((((((data$Age) - (ifelse(rep(1.,NROW(data))*(ifelse(rep(1.,NROW(data))*(sin((data$Fare))>0.),data$SibSp,data$Age)>0.),data$SibSp,data$Fare)))) * 2.0))))))) * 2.0)) +
    0.200000*tanh(((data$Fare) * (sin((((((((data$Age) * (data$Pclass))) * (((((data$Age) * (data$Pclass))) * (data$Pclass))))) * (9.869604))))))) +
    0.200000*tanh(((((cos((pmax(((10.0)), (ifelse(rep(1.,NROW(data))*(cos((pmax((data$Fare), (3.141593))))>0.),data$Fare,(((data$Fare) + (data$Age))/2.0))))))) * 2.0)) * 2.0)) +
    0.200000*tanh(((pmax((data$SibSp), (data$Cabin))) * (sin(((-1.0*((((pmax((data$SibSp), (data$Cabin))) * (((-3.0) - (cos((data$SibSp)))))))))))))) +
    0.200000*tanh(cos((((data$Fare) + ((((3.06355190277099609)) * (pmax((pmax((((data$Age) / 2.0)), (data$Pclass))), (((data$Pclass) * (data$SibSp))))))))))) +
    0.200000*tanh(((sin((data$SibSp))) + ((-1.0*(((((((data$Age) > (((9.869604) + ((((pmin((data$Fare), (data$Age))) + (31.006277))/2.0)))))*1.)) * 2.0))))))) +
    0.200000*tanh(((pmax((data$Embarked), (((data$SibSp) * 2.0)))) * (((data$Fare) * (sin((((((((data$Parch) + (data$Fare))) + (3.0))) * 2.0)))))))) +
    0.200000*tanh(sin((ifelse(rep(1.,NROW(data))*((((data$SibSp) > (data$Cabin))*1.)>0.),(11.02244758605957031),((((((0.636620) + (-2.0))) * (data$Age))) - (sin((data$Pclass)))))))) +
    0.200000*tanh(((data$Age) * (((data$Fare) * (sin((((((tanh((data$SibSp))) / 2.0)) * ((((data$SibSp) + (pmax((data$Age), ((10.03775882720947266)))))/2.0)))))))))) +
    0.200000*tanh(cos((((cos((data$Age))) * ((((data$Embarked) + (((((data$Parch) * 2.0)) + (pmax((((data$Embarked) + (data$Age))), ((14.08150863647460938)))))))/2.0)))))) +
    0.200000*tanh(((sin((((((data$Age) * (tanh((((data$Parch) - ((((((1.570796) * (tanh((data$SibSp))))) + (data$Cabin))/2.0)))))))) / 2.0)))) * 2.0)) +
    0.200000*tanh(((((sin(((((data$Age) + ((((((data$SibSp) * 2.0)) + (((data$Cabin) * (data$Cabin))))/2.0)))/2.0)))) * (data$Embarked))) * (((data$Embarked) * 2.0)))) +
    0.200000*tanh(((sin((((data$Fare) * ((((-1.0*((((sin((((data$Fare) * (((data$Age) - (data$Fare))))))) * 2.0))))) - (data$Fare))))))) * 2.0)) +
    0.171010*tanh(((((cos((((((-1.0*((0.636620)))) + (((((data$Age) * (-3.0))) + (data$Cabin))))/2.0)))) * (31.006277))) * (31.006277))) +
    0.200000*tanh(sin((sin((((data$Cabin) - ((((((((data$Age) * 2.0)) * 2.0)) + (pmax((data$Age), (((((((data$Cabin) * 2.0)) * 2.0)) * 2.0)))))/2.0)))))))) +
    0.200000*tanh(ifelse(rep(1.,NROW(data))*(pmin((data$Age), (cos((((((data$Age) * 2.0)) * (3.141593))))))>0.),((((((data$Embarked) > (1.570796))*1.)) > (data$Parch))*1.),-2.0)) +
    0.199961*tanh(((sin((((sin((((((((9.869604) * 2.0)) * (((((data$Parch) * 2.0)) - (data$Age))))) - (data$Age))))) * 2.0)))) * 2.0)) +
    0.200000*tanh(((pmin((((((9.869604) * 2.0)) - (((data$Age) - ((((data$Cabin) + (data$Age))/2.0)))))), ((((data$Parch) + (data$Cabin))/2.0)))) * (data$Age))) +
    0.200000*tanh(((sin((((((31.006277) * (data$Fare))) * (ifelse(rep(1.,NROW(data))*(data$Embarked>0.),2.0,(-1.0*((pmax((data$Fare), (data$Age))))))))))) * 2.0)) +
    0.200000*tanh((-1.0*((tanh((((cos((((((((data$Pclass) * (data$Age))) * (data$Age))) - (data$Cabin))))) * (((data$Age) + (data$Cabin)))))))))) +
    0.184606*tanh(((data$Age) * (sin((((((data$Pclass) * 2.0)) * (ifelse(rep(1.,NROW(data))*(data$Parch>0.),data$Age,((data$Age) * (((data$Pclass) * (data$Age)))))))))))) +
    0.168861*tanh(((sin((((((data$Fare) * (tanh((data$Age))))) * (tanh((tanh((cos((sin((data$Age))))))))))))) * (31.006277))) +
    0.200000*tanh(((sin(((((-1.0*((((data$Fare) - ((-1.0*((data$Cabin))))))))) * (((data$Age) / 2.0)))))) * 2.0)) +
    0.200000*tanh((((((-1.0*((cos((((((((data$Fare) + (data$Age))) + (sin((cos((data$Cabin))))))) + (sin((data$Age)))))))))) * 2.0)) * 2.0)) +
    0.200000*tanh(((sin((((data$Age) - (((((tanh((data$Age))) + (data$Embarked))) - (((data$Cabin) * 2.0)))))))) * (((data$Embarked) + (data$Cabin))))) +
    0.200000*tanh((((pmin((pmax((0.318310), (pmax((((data$Cabin) / 2.0)), (data$SibSp))))), (tanh(((((data$SibSp) + (3.141593))/2.0)))))) < (sin((data$Age))))*1.)) +
    0.200000*tanh(ifelse(rep(1.,NROW(data))*((((3.0)) - (data$SibSp))>0.),sin(((((3.0)) * (((((data$Embarked) - (data$Pclass))) - (((data$Age) / 2.0))))))),data$Age)) +
    0.200000*tanh(((((sin((((data$Cabin) * (ifelse(rep(1.,NROW(data))*(data$SibSp>0.),((data$Cabin) * 2.0),((data$Cabin) * (data$Age)))))))) * (31.006277))) - (data$Cabin))) +
    0.200000*tanh(((sin((pmin(((-1.0*((((((data$Age) * 2.0)) + (data$Fare)))))), (((data$Age) * (data$Embarked))))))) * (data$Cabin))) +
    0.200000*tanh(((ifelse(rep(1.,NROW(data))*((((cos((data$Pclass))) > (cos((data$Fare))))*1.)>0.),data$Fare,(((-1.0*((data$Parch)))) / 2.0))) * ((((data$Age) + (data$SibSp))/2.0)))) +
    0.180582*tanh(((sin(((((((2.0) * ((((sin((data$Cabin))) < (data$Embarked))*1.)))) + (((data$Age) * ((((data$Age) + ((9.0)))/2.0)))))/2.0)))) * 2.0)) +
    0.190897*tanh(sin((((pmax((data$Parch), (ifelse(rep(1.,NROW(data))*(sin((pmax((data$Cabin), (data$Parch))))>0.),data$Fare,(((0.318310) + (data$SibSp))/2.0))))) * (31.006277))))) +
    0.200000*tanh(((sin((((((pmin((((0.636620) - (data$SibSp))), ((((data$Cabin) + (data$Embarked))/2.0)))) / 2.0)) * (((data$Fare) * (data$Pclass))))))) * 2.0)) +
    0.200000*tanh(sin(((((((data$Fare) + (pmax((((3.141593) * (data$Age))), (((((data$Embarked) * (data$Age))) * (data$Pclass))))))/2.0)) / 2.0)))) +
    0.200000*tanh(((((((((sin((data$Pclass))) - (sin((sin((data$Fare))))))) * (data$Parch))) * (data$Pclass))) * (((data$Age) * (data$Parch))))) +
    0.172456*tanh((((-1.0*((ifelse(rep(1.,NROW(data))*(data$Cabin>0.),sin((ifelse(rep(1.,NROW(data))*(sin(((((((data$Cabin) / 2.0)) + (data$Fare))/2.0)))>0.),data$Pclass,data$Fare))),data$SibSp))))) * 2.0)) +
    0.200000*tanh(cos((((((((((data$Age) - (data$Fare))) - (((data$Fare) - (cos((data$Age))))))) * (31.006277))) - (cos((data$Age))))))) +
    0.200000*tanh(((sin((((((sin((sin((data$Age))))) * 2.0)) * (((data$Fare) - (data$Cabin))))))) * 2.0)) +
    0.200000*tanh(((sin(((((((data$Fare) + ((((3.0)) + (pmax((data$Age), ((((data$Fare) + (((data$Age) + ((10.0)))))/2.0)))))))/2.0)) / 2.0)))) * 2.0)) +
    0.200000*tanh(sin(((((((data$Age) + ((((data$Embarked) + (((data$Parch) * (((data$Parch) - (((data$Parch) * (data$Parch))))))))/2.0)))/2.0)) * ((2.57526826858520508)))))) +
    0.200000*tanh(sin((((ifelse(rep(1.,NROW(data))*(data$Parch>0.),((data$Age) * (31.006277)),((ifelse(rep(1.,NROW(data))*(data$Cabin>0.),data$Age,((data$Age) * (31.006277)))) / 2.0))) / 2.0)))) +
    0.200000*tanh(ifelse(rep(1.,NROW(data))*((((3.0) > (ifelse(rep(1.,NROW(data))*(data$Cabin>0.),data$Cabin,data$Age)))*1.)>0.),data$Age,((data$Cabin) * (((sin(((-1.0*((data$Cabin)))))) / 2.0))))) +
    0.200000*tanh(((pmin((sin((((sin((data$Parch))) * (((data$Age) / 2.0)))))), (sin((((((data$Age) / 2.0)) * (sin((data$Pclass))))))))) * 2.0)) +
    0.200000*tanh(sin((((((((data$Age) * (sin((cos((cos((data$Pclass))))))))) + (data$SibSp))) + ((((sin((data$Age))) < (data$Parch))*1.)))))) +
    0.200000*tanh(sin((((((data$Parch) - (((data$Fare) * (((1.570796) - (pmax((data$Age), (data$Parch))))))))) * 2.0)))) +
    0.200000*tanh(((sin((((((data$Age) + (data$Cabin))) * (((data$Cabin) + (tanh((sin((data$Embarked))))))))))) * (((data$Age) - (data$Cabin))))) +
    0.200000*tanh(sin((ifelse(rep(1.,NROW(data))*(data$Age>0.),((data$Age) * (((((data$Age) * (sin((data$Fare))))) * (data$Age)))),((1.570796) * (data$Fare)))))) +
    0.200000*tanh(((sin((((data$Age) * (ifelse(rep(1.,NROW(data))*(data$Embarked>0.),(8.0),ifelse(rep(1.,NROW(data))*(data$SibSp>0.),ifelse(rep(1.,NROW(data))*(data$Cabin>0.),9.869604,(10.36270618438720703)),-2.0))))))) * 2.0)) +
    0.200000*tanh(((sin((((((sin((sin((data$Age))))) + ((((-1.0*((data$SibSp)))) + (data$Fare))))) * (((data$Age) * 2.0)))))) * 2.0)) +
    0.200000*tanh(((data$Fare) * (ifelse(rep(1.,NROW(data))*(data$SibSp>0.),data$Cabin,ifelse(rep(1.,NROW(data))*(data$Parch>0.),data$Age,sin((((data$Cabin) * (((data$Cabin) * (data$Age))))))))))) +
    0.200000*tanh((-1.0*((((((data$Age) - (data$SibSp))) * (((((data$Fare) * ((((data$Fare) > (31.006277))*1.)))) - ((((data$Fare) > (data$Age))*1.))))))))) +
    0.178472*tanh(((cos((pmax((((data$Age) * ((2.12336945533752441)))), (data$Parch))))) * (ifelse(rep(1.,NROW(data))*(data$Embarked>0.),((data$Parch) * (0.318310)),data$Fare)))) +
    0.200000*tanh(((9.869604) * ((-1.0*(((((((9.869604) + ((((((data$Parch) + (data$Cabin))/2.0)) - (3.0))))) > (data$Fare))*1.))))))) +
    0.200000*tanh(((((((sin((((data$SibSp) - (((ifelse(rep(1.,NROW(data))*(data$Age>0.),3.141593,data$SibSp)) * (pmax((data$Age), (9.869604))))))))) * 2.0)) * 2.0)) * 2.0)) +
    0.200000*tanh(((((data$Fare) - (data$Cabin))) * (sin((sin((((((data$Age) * (cos((2.0))))) * (data$Cabin))))))))) +
    0.200000*tanh(((sin((((31.006277) * (sin((((((data$Fare) * (data$Age))) + (sin((((data$Fare) * (data$Age))))))))))))) * 2.0)) +
    0.200000*tanh(sin((pmin((data$SibSp), ((-1.0*(((((-3.0) + ((((((((data$Age) * (data$SibSp))) * 2.0)) + (((data$Age) * (data$Fare))))/2.0)))/2.0))))))))) +
    0.192655*tanh(pmin((sin((ifelse(rep(1.,NROW(data))*(data$Embarked>0.),((data$Age) / 2.0),data$Age)))), ((((data$Age) < (data$Fare))*1.)))) +
    0.200000*tanh(sin((ifelse(rep(1.,NROW(data))*(sin((((data$Fare) * (cos((data$Fare))))))>0.),((((data$Fare) * (data$Age))) * 2.0),((data$Fare) * (data$Age)))))) +
    0.200000*tanh(((cos((((((pmin((data$Fare), (((((data$Age) + (data$Parch))) / 2.0)))) + (data$SibSp))) * 2.0)))) * (((((data$Age) / 2.0)) / 2.0)))) +
    0.086658*tanh(sin((((((((((((((10.0)) * 2.0)) + (data$Fare))/2.0)) + (((data$Fare) * (data$Pclass))))/2.0)) + (((pmax(((10.0)), (data$Fare))) / 2.0)))/2.0)))) +
    0.200000*tanh(((sin((((data$Fare) * (sin((((((pmin((data$Fare), (((data$Age) + (((data$Cabin) / 2.0)))))) / 2.0)) / 2.0)))))))) * 2.0)) +
    0.200000*tanh(sin(((-1.0*((((data$Age) + (((sin((data$Pclass))) + (((((data$Age) + (((data$Fare) * (31.006277))))) * 2.0))))))))))) +
    0.076421*tanh((-1.0*(((((((data$Embarked) < (((cos((pmax(((((ifelse(rep(1.,NROW(data))*(data$Age>0.),data$Fare,data$Age)) + (data$Age))/2.0)), (data$Age))))) * 2.0)))*1.)) * 2.0))))) +
    0.200000*tanh(sin((ifelse(rep(1.,NROW(data))*(data$SibSp>0.),(-1.0*((data$Age))),((ifelse(rep(1.,NROW(data))*(cos((data$Fare))>0.),2.0,(((data$Age) + (cos((data$Fare))))/2.0))) / 2.0))))) +
    0.174409*tanh(((sin(((((13.56479930877685547)) * (((ifelse(rep(1.,NROW(data))*(data$Age>0.),31.006277,((-1.0) + (31.006277)))) * (((-1.0) + (data$Fare))))))))) * 2.0)) +
    0.200000*tanh((((-1.0*(((((((data$SibSp) * (((data$SibSp) * (sin((data$Embarked))))))) > (((data$Fare) - (((((10.70390129089355469)) + (3.141593))/2.0)))))*1.))))) * 2.0)) +
    0.191756*tanh(((sin((pmax((((((data$Cabin) * ((5.92921018600463867)))) * (data$Age))), (((((((data$Fare) * 2.0)) * (data$Pclass))) * 2.0)))))) * (31.006277))) +
    0.088064*tanh((((((data$SibSp) < ((((tanh((cos((data$Fare))))) > (pmin((sin(((((data$Age) + (data$Fare))/2.0)))), (data$SibSp))))*1.)))*1.)) * (data$Fare))) +
    0.200000*tanh(pmin((sin((((((((pmin((data$Fare), ((8.0)))) * (data$Cabin))) - (data$Embarked))) - (((data$Fare) * 2.0)))))), (((data$Cabin) / 2.0)))) +
    0.199961*tanh(sin((ifelse(rep(1.,NROW(data))*(data$SibSp>0.),((data$Fare) - (ifelse(rep(1.,NROW(data))*(data$Age>0.),data$Age,data$Pclass))),((data$Age) * ((((11.19820404052734375)) + (data$Age)))))))) +
    0.151006*tanh(pmin((data$Age), (((ifelse(rep(1.,NROW(data))*(((((((data$SibSp) > (data$Pclass))*1.)) > (sin((data$Fare))))*1.)>0.),data$SibSp,data$Embarked)) - (sin((data$Age))))))) +
    0.200000*tanh(pmin((((((sin((((data$Age) * (sin((data$Pclass))))))) * (sin((((data$Age) / 2.0)))))) * (2.0))), (data$Age))) +
    0.143661*tanh((((10.80022907257080078)) * (((((sin((((data$Age) * ((10.80022907257080078)))))) / 2.0)) * (sin((sin((((sin((data$Age))) - (data$Cabin))))))))))) +
    0.184880*tanh(((((((sin(((((-1.0*((data$Fare)))) * (((ifelse(rep(1.,NROW(data))*(data$Age>0.),data$Age,(-1.0*((3.141593))))) * ((8.96378326416015625)))))))) * 2.0)) * 2.0)) * 2.0)) +
    0.200000*tanh((-1.0*(((((((((((((sin((((9.869604) * (data$Fare))))) < (0.636620))*1.)) / 2.0)) > (sin((data$Age))))*1.)) > (((data$Cabin) / 2.0)))*1.))))) +
    0.200000*tanh(((((cos((((pmax((data$Fare), (2.0))) * (((pmax((data$Age), (((1.570796) + (0.318310))))) + (data$Fare))))))) * 2.0)) * 2.0)) +
    0.200000*tanh(((sin((((pmax((((data$Age) * (sin((data$Fare))))), (((data$Age) * (data$Cabin))))) * ((9.0)))))) * 2.0)) +
    0.200000*tanh(sin((((data$Age) * ((((-1.0*((data$SibSp)))) + (((ifelse(rep(1.,NROW(data))*(cos((data$SibSp))>0.),((data$Pclass) * 2.0),(11.23526477813720703))) + (9.869604))))))))) +
    0.200000*tanh(((sin((((ifelse(rep(1.,NROW(data))*(data$Embarked>0.),((data$Age) + (data$SibSp)),(((((-1.0*((data$SibSp)))) * (data$Age))) + (data$SibSp)))) / 2.0)))) * 2.0)) +
    0.200000*tanh(sin((ifelse(rep(1.,NROW(data))*(cos((((data$Fare) * (((-3.0) * (data$Fare))))))>0.),pmin((data$Embarked), (((1.570796) - (data$Age)))),data$Pclass)))) +
    0.200000*tanh(sin(((((((((cos((data$Embarked))) + (data$Age))) + (((((data$Fare) * (((data$Age) + ((2.0)))))) / 2.0)))/2.0)) + (data$Age))))) +
    0.200000*tanh(((cos(((((((((data$Parch) - (data$SibSp))) + (data$Age))) + ((((data$Fare) + (((data$Age) - (data$SibSp))))/2.0)))/2.0)))) * (data$Age))) +
    0.139715*tanh((-1.0*((ifelse(rep(1.,NROW(data))*(pmax((data$Cabin), (ifelse(rep(1.,NROW(data))*(cos((data$Pclass))>0.),31.006277,data$SibSp)))>0.),cos((data$Age)),(((data$SibSp) < (data$Age))*1.)))))) +
    0.200000*tanh(sin((pmax((((data$Fare) * (((((9.869604) - (tanh((-2.0))))) + (data$Cabin))))), (((9.869604) + (data$Pclass))))))) +
    0.200000*tanh(sin((pmin((((data$Age) * (pmax((pmax(((((data$Age) < (data$Fare))*1.)), (data$Cabin))), (pmax((data$SibSp), ((0.12435796856880188)))))))), (data$Fare))))) +
    0.200000*tanh(sin((((((sin((((((ifelse(rep(1.,NROW(data))*(data$Age>0.),((data$Age) - ((2.0))),2.0)) * 2.0)) * (data$Fare))))) * 2.0)) * (data$Fare))))) +
    0.200000*tanh(((sin((((((data$Fare) * (data$Age))) * ((((((-1.0*((data$Fare)))) * (data$Fare))) * (sin((sin((data$Age))))))))))) * 2.0)) +
    0.200000*tanh(sin((pmin((((data$Age) * ((-1.0*((data$Cabin)))))), ((((-1.0*((data$Embarked)))) - (((((data$Age) * (data$Embarked))) * (data$Embarked))))))))) +
    0.200000*tanh(((((((31.006277) * (data$Age))) * (sin((((sin((((data$Fare) * (data$Age))))) * ((((data$Age) + (data$Embarked))/2.0)))))))) * 2.0)) +
    0.200000*tanh(sin((ifelse(rep(1.,NROW(data))*(data$Cabin>0.),(-1.0*((((data$Embarked) + (((data$Age) + (data$Fare))))))),(((1.0) + ((((data$Age) + (data$Fare))/2.0)))/2.0))))) +
    0.132057*tanh(sin((((((data$Fare) * (sin((((((data$Fare) * (sin((data$Fare))))) * 2.0)))))) * 2.0)))) +
    0.200000*tanh(pmin((data$Parch), ((((10.22945499420166016)) * (sin((((((data$Age) * (ifelse(rep(1.,NROW(data))*(data$Cabin>0.),(10.22945785522460938),(7.61376333236694336))))) * 2.0)))))))) +
    0.200000*tanh(sin((pmax((((((data$Age) / 2.0)) * (((((31.006277) * (data$Fare))) / 2.0)))), ((((7.0)) * (((data$Fare) * (data$Fare))))))))) +
    0.200000*tanh((((((sin((((((data$Fare) * (((sin((sin((((data$Age) / 2.0)))))) / 2.0)))) / 2.0)))) > (data$Parch))*1.)) * (data$Fare))) +
    0.200000*tanh(sin((pmin((((((data$Fare) - (((data$Fare) * (data$Age))))) * (data$Fare))), (((data$SibSp) - (((data$Fare) * (data$Fare))))))))) +
    0.200000*tanh(((3.0) * (sin((((data$Pclass) * (sin((((data$Age) * (((((-3.0) - (data$SibSp))) - (data$Fare))))))))))))) +
    0.200000*tanh(sin((((((sin((data$Age))) * 2.0)) * (sin((((((data$Fare) * 2.0)) * (cos((data$Age))))))))))) +
    0.200000*tanh(((cos((((((((data$Age) - (cos((data$Age))))) - (cos((data$Embarked))))) * (cos((data$Fare))))))) * (sin((data$Age))))) +
    0.161789*tanh(sin((((9.869604) * (((data$Fare) + (ifelse(rep(1.,NROW(data))*(cos((data$Parch))>0.),pmin((pmin((9.869604), (data$Fare))), (data$Age)),-2.0)))))))) +
    0.140301*tanh(pmin((ifelse(rep(1.,NROW(data))*(cos((data$Fare))>0.),sin((data$Age)),ifelse(rep(1.,NROW(data))*(data$Cabin>0.),sin((data$Age)),cos((data$Fare))))), (cos((data$Parch))))) +
    0.200000*tanh((-1.0*(((((((cos((data$Age))) > ((0.64883369207382202)))*1.)) * (((data$Fare) * (((data$Age) * (((data$Fare) * (cos((data$Fare)))))))))))))) +
    0.200000*tanh(sin((((((((((data$Age) + (ifelse(rep(1.,NROW(data))*(data$Cabin>0.),data$Embarked,data$Cabin)))/2.0)) + (data$Age))/2.0)) * (ifelse(rep(1.,NROW(data))*(data$Cabin>0.),data$Cabin,data$Fare)))))) +
    0.200000*tanh(sin((((((sin((sin((data$Fare))))) - ((((data$Age) > (((((data$Fare) * 2.0)) * 2.0)))*1.)))) * (((data$Age) * 2.0)))))) +
    0.200000*tanh(((((((31.006277) + (data$Age))) * (data$Parch))) * (cos(((((data$Embarked) + (((((data$Age) + (data$SibSp))) * (1.570796))))/2.0)))))) +
    0.200000*tanh(((31.006277) * (sin((((((cos((sin(((((cos((((data$Age) / 2.0)))) > (data$Parch))*1.)))))) * (data$Embarked))) * (data$Age))))))) +
    0.199961*tanh(((31.006277) * (((data$Pclass) * (((data$Parch) * (sin((((3.0) * (((cos((data$Parch))) - (((data$Age) / 2.0)))))))))))))) +
    0.200000*tanh(((sin((((data$Cabin) * (((((data$Age) * 2.0)) + (data$Cabin))))))) * (-3.0))) +
    0.156788*tanh(((((((data$Fare) * (((data$Embarked) - (0.636620))))) * (data$Embarked))) * (cos((((data$Fare) * (((data$Embarked) - (0.636620))))))))) +
    0.200000*tanh(((((sin((((data$Age) * (((cos((ifelse(rep(1.,NROW(data))*(data$Embarked>0.),data$Cabin,pmax((data$Fare), ((4.22627544403076172))))))) + (data$Pclass))))))) * 2.0)) * 2.0)) +
    0.184724*tanh(sin((((((data$Embarked) + ((((((3.141593) + (((data$Embarked) + (data$Age))))/2.0)) / 2.0)))) - ((((data$Parch) > (sin((data$Age))))*1.)))))) +
    0.185778*tanh(sin((((cos((((data$Fare) / 2.0)))) * (((data$Age) * (sin((data$Fare))))))))) +
    0.176949*tanh(((ifelse(rep(1.,NROW(data))*(((data$Parch) * (data$Age))>0.),data$Pclass,(((sin((data$Age))) < (data$Cabin))*1.))) * (sin((((((data$Pclass) * 2.0)) * 2.0)))))) +
    0.200000*tanh(sin((((data$Fare) * (((((data$Parch) - (((data$Embarked) + (((data$Cabin) * (((data$Embarked) + (sin((data$Cabin))))))))))) * 2.0)))))) +
    0.161555*tanh(((sin((data$Parch))) + ((((data$Age) < (((data$Parch) - (((9.869604) - ((((((data$Cabin) - (data$SibSp))) + (data$Fare))/2.0)))))))*1.)))) +
    0.151709*tanh(sin(((((-1.0*((data$SibSp)))) - (ifelse(rep(1.,NROW(data))*(data$Parch>0.),((data$SibSp) * (data$Fare)),((31.006277) * ((((data$SibSp) + (data$Fare))/2.0))))))))) +
    0.200000*tanh(((((pmin((data$Cabin), (sin((data$Age))))) - (sin((sin((data$Fare))))))) * (((((((((data$SibSp) * 2.0)) * 2.0)) * 2.0)) * 2.0)))) +
    0.086931*tanh(((sin((((((data$Age) / 2.0)) * (((((((data$Fare) / 2.0)) / 2.0)) * (data$Fare))))))) * (((((data$Age) / 2.0)) / 2.0)))) +
    0.198242*tanh(ifelse(rep(1.,NROW(data))*(data$SibSp>0.),sin((((data$Pclass) * (((data$Cabin) - (data$Age)))))),sin((((data$Pclass) * ((((data$SibSp) + (data$Age))/2.0))))))) +
    0.200000*tanh(((pmax((sin((((cos((data$Fare))) * (data$Age))))), (sin((((((cos((data$Fare))) * (data$Age))) * (data$Age))))))) * 2.0)) +
    0.140066*tanh((-1.0*((((((((data$Embarked) < (1.0))*1.)) < (((((data$Age) + (pmax((data$Fare), (data$Embarked))))) - (31.006277))))*1.))))) +
    0.104513*tanh((((-1.0*((sin((((data$Age) * ((((9.869604) + (((data$SibSp) - (((((((9.869604) > (data$Pclass))*1.)) > (data$Embarked))*1.)))))/2.0))))))))) * 2.0)) +
    0.200000*tanh(((((data$Age) * (sin((((sin((data$Parch))) * (((((((data$Age) * (tanh((data$Pclass))))) - (data$Pclass))) / 2.0)))))))) * 2.0)) +
    0.200000*tanh((((((((data$Parch) < ((((sin((data$Age))) < (sin((data$Embarked))))*1.)))*1.)) * (sin((((data$Age) / 2.0)))))) * (((data$Embarked) * 2.0)))) +
    0.200000*tanh(sin((((((sin((((((((data$Cabin) + (data$Fare))) + (data$Fare))) * (31.006277))))) * 2.0)) * 2.0)))) +
    0.200000*tanh(((sin((((data$Embarked) * (data$Fare))))) + (sin((((31.006277) * ((((0.318310) + (((31.006277) * (data$Fare))))/2.0)))))))) +
    0.200000*tanh(((sin((((pmax((data$Parch), (((data$Fare) * (((sin((data$Fare))) * (sin((data$Age))))))))) * (((data$Fare) * 2.0)))))) * 2.0)) +
    0.200000*tanh(sin((ifelse(rep(1.,NROW(data))*(data$SibSp>0.),((((data$SibSp) * (data$Age))) * ((8.98763370513916016))),((((data$Age) * ((-1.0*((9.869604)))))) * (data$Fare)))))) +
    0.142762*tanh(((cos(((((((((10.87822341918945312)) * 2.0)) * 2.0)) * (data$Fare))))) + (cos((((data$Fare) * (((data$Fare) * (data$Fare))))))))) +
    0.055792*tanh(((data$Fare) * (((((data$SibSp) * (sin(((((((data$Fare) + (data$SibSp))/2.0)) * ((((data$Fare) + (data$SibSp))/2.0)))))))) * (data$Fare))))) +
    0.200000*tanh(((sin((((((data$Age) * (((data$Fare) + (cos((data$SibSp))))))) * ((8.79990768432617188)))))) * (data$Pclass))) +
    0.200000*tanh((-1.0*((((sin((((tanh((((((((tanh((data$Cabin))) > (data$Embarked))*1.)) > (data$SibSp))*1.)))) * (data$Fare))))) * 2.0))))) +
    0.200000*tanh(((((cos((((pmax((data$Fare), (((ifelse(rep(1.,NROW(data))*(data$Parch>0.),ifelse(rep(1.,NROW(data))*(data$Embarked>0.),data$Parch,data$Fare),31.006277)) * 2.0)))) * 2.0)))) * 2.0)) * 2.0)) +
    0.200000*tanh(((((((pmin((data$Cabin), (pmax(((((data$Embarked) > (data$Parch))*1.)), (((((((data$Parch) < (data$Cabin))*1.)) < (data$Pclass))*1.)))))) * 2.0)) * 2.0)) * 2.0)) +
    0.182145*tanh(((((sin(((((((((data$Age) + (data$Cabin))/2.0)) - (sin((data$Fare))))) * (sin((31.006277))))))) * (data$Parch))) * (data$Age))) +
    0.200000*tanh(((((((((((5.0)) * (data$Age))) < (data$SibSp))*1.)) - (sin((((data$Age) * (((data$Age) * (3.141593))))))))) * ((8.0)))) +
    0.200000*tanh(((((pmin((data$Age), (sin((data$Fare))))) * (sin((((((sin((sin((0.636620))))) * (data$Age))) * (data$Fare))))))) * 2.0)) +
    0.172377*tanh(((sin((data$Embarked))) - ((((((((data$Cabin) * (((data$Age) * (((data$Cabin) / 2.0)))))) / 2.0)) > (((data$Fare) - ((7.43029689788818359)))))*1.)))) +
    0.200000*tanh(cos(((((14.34487915039062500)) * ((((data$Age) + (((data$Pclass) + (ifelse(rep(1.,NROW(data))*(data$Embarked>0.),ifelse(rep(1.,NROW(data))*(data$Age>0.),data$Pclass,1.0),data$Embarked)))))/2.0)))))) +
    0.152569*tanh(sin(((((data$Age) + ((((((data$Age) - (data$SibSp))) + (data$Fare))/2.0)))/2.0)))) +
    0.043055*tanh((((((((((data$Pclass) / 2.0)) / 2.0)) < (sin((((data$Cabin) - (data$Age))))))*1.)) * (((((data$Fare) / 2.0)) / 2.0)))) +
    0.199961*tanh(((data$Age) * (((cos((((ifelse(rep(1.,NROW(data))*(sin((data$Parch))>0.),cos((sin((data$Parch)))),data$Age)) * 2.0)))) * (sin((data$Cabin))))))) +
    0.035124*tanh(pmax((((cos((data$SibSp))) + (-3.0))), ((((-2.0) + (tanh((cos((ifelse(rep(1.,NROW(data))*(-3.0>0.),data$Fare,(6.18699932098388672))))))))/2.0)))) +
    0.200000*tanh(((1.570796) * (sin(((((((data$Parch) + (data$Fare))/2.0)) * (((((((11.17537975311279297)) * (data$Fare))) + ((((data$Cabin) + (data$Age))/2.0)))/2.0))))))))
  p<- 1.0/(1.0+exp(-p)) 
}

trainpreds<-gp(xtrain)
testpreds<-gp(xtest)
actual<-xtrain$Survived
df_final <- data.frame(PassengerId = test$PassengerId, Survived=round(testpreds,digits=0));
write.csv(df_final, "submission.csv", row.names =F, quote=F)
