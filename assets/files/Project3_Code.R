library(glue); library(dplyr); library(lubridate); library(forecast);library(TTR)

subcategories <- read.csv("~/College/2023 DataFest/DataFest 2023 Data For Distribution/data/subcategories.csv")
statesites <- read.csv("~/College/2023 DataFest/DataFest 2023 Data For Distribution/data/statesites.csv")
questions <- read.csv("~/College/2023 DataFest/DataFest 2023 Data For Distribution/data/questions.csv")
# question posts import
clients <- read.csv("~/College/2023 DataFest/DataFest 2023 Data For Distribution/data/clients.csv")
categories <- read.csv("~/College/2023 DataFest/DataFest 2023 Data For Distribution/data/categories.csv")
attorneytimeentries <- read.csv("~/College/2023 DataFest/DataFest 2023 Data For Distribution/data/attorneytimeentries.csv")
attorneys <- read.csv("~/College/2023 DataFest/DataFest 2023 Data For Distribution/data/attorneys.csv")

anova(aov(as.numeric(ymd_hms(attorneytimeentries$EnteredOnUtc)) ~ attorneytimeentries$StateAbbr))
prop.table(table(clients$StateAbbr))
prop.table(table(attorneys$StateAbbr))
tapply(attorneytimeentries$Hours,attorneytimeentries$StateAbbr,summary)
table(questions$StateAbbr)

table(categories$Category)
plot(month(ymd_hms(AskedOnUtc) ~ data=questions))
hist(month(ymd_hms(AskedOnUtc)),)
table(month(ymd_hms(questions$AskedOnUtc)),year(ymd_hms(questions$AskedOnUtc)))


q2 <- transform(questions,myA=12*(year(ymd_hms(questions$AskedOnUtc))-2012)+month(ymd_hms(questions$AskedOnUtc)))
q2t <- q2[q2$TakenOnUtc!="NULL",]
#q3 <-transform(q2t,myT=12*(year(ymd_hms(q2t$TakenOnUtc))-2012)+month(ymd_hms(q2t$TakenOnUtc)))
q3 <-transform(q2t,myT=month(ymd_hms(q2t$TakenOnUtc))%%12)

hist(q2$my)
hist(q3$my)
plot1 <- ggplot(q2,aes(x=myA))+geom_freqpoly(aes(color=Category))
plot1+geom_vline(aes(xintercept=(12*9+3)))+geom_vline(aes(xintercept=(12*7+3)))
ggplot(q2,aes(x=AskedOnUtc)) + geom_freqpoly(aes(color="red"))
plot2 <- ggplot(q3,aes(x=myT))+geom_smooth(aes(color=Category))
plot2
plot11 <- ggplot(q2,aes(x=myA,group=Category,fill=Category))+geom_density(adjust=1.5,position="fill")
plot11

#incomeSTATe, stateCategory
clientIncome <-subset(clients[clients$AnnualIncome!="NULL",],select=c(ClientUno,AnnualIncome,AllowedIncome))
questionCategory<-subset(questions,select=c(CategoryUno:TakenOnUtc))
merged <- merge(clientIncome,questionCategory,by.x="ClientUno",by.y="AskedByClientUno")
merged <- transform(merged,logIncome=log(as.numeric(merged$AnnualIncome)))
plot3 <- ggplot(merged,aes(x=log(as.numeric(AnnualIncome)))) + geom_freqpoly(aes(color=Category))               
plot3
plot4 <- ggplot(data=merged,aes(x=log(as.numeric(AnnualIncome)),group=Category,fill=Category)) + geom_density(adjust=1.5,position="fill")
plot4
table(clients$AnnualIncome)
table(questions$Category)
incomeSummary <- summary(log(as.numeric(merged$AnnualIncome)))
incomeSummary
mergedTrim <- merged[log(as.numeric(merge$AnnualIncome))<1.5*(incomeSummary[4]-incomeSummary[2])+incomeSummary[4] |
                  log(as.numeric(merge$AnnualIncome))>-1.5*(incomeSummary[4]-incomeSummary[2])+incomeSummary[2],]
ggplot(questions,aes(x=StateAbbr,fill=Category)) + geom_density()
floorDate <- ymd_hms(questions$AskedOnUtc)
#floorDate <- month(ymd_hms(questions$AskedOnUtc))
floorDate
questionDummy <- data.frame(questions$StateAbbr,questions$Category,questions$Subcategory,questions$AskedByClientUno,questions$AskedOnUtc,
                            questions$TakenOnUtc,questions$LegalDeadline,
                            AskedM=floorDate,
                            ConsumerFinQ=ifelse(questions$Category=="Consumer Financial Questions",1,0),
                           Edu=ifelse(questions$Category=="Education",1,0),
                           Fam=ifelse(questions$Category=="Family and Children",1,0),
                           Hlth=ifelse(questions$Category=="Health and Disability",1,0),
                           Home=ifelse(questions$Category=="Housing and Homelessness",1,0),
                           Income=ifelse(questions$Category=="Income Maintenance",1,0),
                           Rights=ifelse(questions$Category=="Individual Rights",1,0),
                           Juve=ifelse(questions$Category=="Juvenile",1,0),
                           Other=ifelse(questions$Category=="Other",1,0),
                           Category=ifelse(questions$Category=="Work, Employment and Unemployment",1,0))
questionDummy <- questionDummy[order(questionDummy$AskedM),]

monthFreqInit <- questionDummy %>% transform(Asked=floor_date(questionDummy$Asked,"month"()))
View(table(questionDummy$Asked))
View(table(floor_date(questionDummy$AskedM,"month")))
monthFreq <- data.frame(table(floor_date(questionDummy$AskedM,"month")))
monthFreq2 <- transform(monthFreq,Date=rownames(catMonth))
mfTS <- ts(as.numeric(monthFreq$Freq),start=c(2012,10),frequency=12)
plot(mfTS)
mfTSsma <- SMA(mfTS)
plot(mfTSsma)
mfTScomponents <- decompose(mfTS)
plot(mfTScomponents)


catMonth <- as.data.frame.matrix(table(month(questionDummy$Asked),questionDummy$questions.Category))
colnames(catMonth) <- c("Cfq","Edu","Fam","Health","Home","Income","Rights","Juvenile","Other","Work")
catMonth2 <- transform(catMonth,Date=rownames(catMonth))
mTS <- ts(catMonth,start=c(2012,1),frequency=52)
plot(mTS)


catMonthY <- as.data.frame.matrix(table(floor_date(questionDummy$Asked,"month"),questionDummy$questions.Category))
colnames(catMonthY) <- c("Cfq","Edu","Fam","Health","Home","Income","Rights","Juvenile","Other","Work")
catMonthY2 <- transform(catMonthY,Date=rownames(catMonthY))
fullTS <- ts(catMonthY,start=c(2012,1),frequency=12)
plot(fullTS)
fullDC <- decompose(fullTS)
View(fullDC)
plot(fullDC$seasonal.x.Fam)



## Replace col name in row below to change category being examined
singleTS <- ts(catMonthY$Fam,start=c(2012,10),frequency=12)
plot(singleTS)
singleTS
singleTSsma <- SMA(singleTS)
plot(singleTSsma)
singleTScomponents <- decompose(singleTS)
plot(singleTScomponents)
singleTSadjusted <- singleTS -singleTScomponents$seasonal
plot(singleTSadjusted)
singleTSforecasts <- HoltWinters(singleTS,beta=FALSE,gamma=FALSE)
plot(singleTSforecasts)
singleTSforecast2 <- forecast:::forecast.HoltWinters(singleTSforecasts,h=4)
forecast:::plot.forecast(singleTSforecast2)
singleTSdiff1 <- diff(singleTS,differences=20)
plot(singleTSdiff1)
singleTSarima <- auto.arima(singleTS)
singleTSarima
singleTSarimaforecasts <- forecast:::forecast.Arima(singleTSarima,h=4)
forecast:::plot.forecast(singleTSarimaforecasts)
pacf(singleTS,lag.)

forecast:::plot.forecast(singleTSarima)
##
par(mfrow=c(2,2))
par(mfrow=c(1,1))
##

percentChange <- catMonth %>% mutate_at(
  vars(Cfq,Edu,Fam,Health,Home,Income,Rights,Juvenile,Other,Work),
  funs(pctChange = (./lag(.)-1)*100))
percentChange <- percentChange[-c(nrow(percentChange)-1,nrow(percentChange)),-c(0:10)]
percentTS <- ts(percentChange,start=c(2012,1),frequency=12)
plot(percentTS)

percentChangeY <- catMonthY %>% mutate_at(
  vars(Cfq,Edu,Fam,Health,Home,Income,Rights,Juvenile,Other,Work),
  funs(pctChange = (./lag(.)-1)*100))
percentChangeY <- percentChangeY[-c(1:5,nrow(percentChangeY)),-c(0:10)]
percentYTS <- ts(percentChangeY$Cfq_pctChange,start=c(1),frequency=1)
plot(percentYTS)

MONTHS <- c("January","February","March","April","May","June","July",
"August","September","October","November","December")
month(x,label=TRUE)

#familyTS <- xts(catDate$Family.and.Children,order.by=my(paste(catDate$Date,2010,sep=' ')))
familyTS <- xts(catDate2$Family.and.Children,order.by=as.Date(catDate2$Date))
plot(x=familyTS)
fitdistr(percentChangeY$Cfq_pctChange,"normal")
hist(percentChangeY$Cfq_pctChange)
plot(fitdist(percentChangeY$Cfq_pctChange,"norm"))


#EDA Stuff
plot(percentChangeY$Cfq_pctChange)



clientsIncClean <- clients[clients$AnnualIncome!="NULL",]
clientsIncTrim <- clientsIncClean[clientsIncClean$AnnualIncome <= clientsIncClean$AllowedIncome,]
nrow(clientsIncTrim)/nrow(clientsIncClean)

questionsClean <- questions[questions$TakenOnUtc!="NULL",]
questionsTime <- transform(questionsClean[questionsClean$TakenOnUtc!="NULL",],TimeForResponse=as.duration(interval(ymd_hms(questionsClean$AskedOnUtc),ymd_hms(questionsClean$TakenOnUtc))))
order(ymd_hms(questionsTime$AskedOnUtc))
questionsTime <- questionsTime %>% arrange(ymd_hms(questionsTime$AskedOnUtc))
responseTimeTS <- xts((1/3600)*questionsTime$TimeForResponse,order.by=(ymd_hms(questionsTime$AskedOnUtc)))
plot.xts(responseTimeTS)

monthFreqInit <- questionDummy %>% transform(Asked=floor_date(questionDummy$Asked,"month"()))
View(table(questionDummy$Asked))
View(table(floor_date(questionDummy$AskedM,"month")))
monthFreq <- data.frame(table(floor_date(questionDummy$AskedM,"month")))
monthFreq2 <- transform(monthFreq,Date=rownames(catMonth))
mfTS <- ts(as.numeric(monthFreq$Freq),start=c(2012,10),frequency=12)
plot(mfTS)
mfTSsma <- SMA(mfTS)
plot(mfTSsma)
plot(forecast(mfTSsma))

mfTSsnaive <- snaive(mfTS,h=12*2)
autoplot(mfTSsnaive,ylab="# of Questions")
summary(mfTSsnaive)
checkresiduals(mfTSsnaive)

mfTScomponents <- decompose(mfTS)
plot(mfTScomponents)
mfTSarima <- auto.arima(mfTS)
mfTSarima
mfTSarimaforecasts <- forecast:::forecast.Arima(mfTSarima,h=2*12,level=c(95))
forecast:::plot.forecast(mfTSarimaforecasts)
Box.test(mfTSarima$resid, lag=15, type="Ljung-Box")

plot.ts(mfTSarimaforecasts$residuals)
forecast:::plot.ForecastErrors(mfTSarimaforecasts$residuals)
plot.ts(kingstimeseriesforecasts$residuals)            # make time plot of forecast errors
> plotForecastErrors(kingstimeseriesforecasts$residuals)

mfTSforecasts <-HoltWinters(mfTS,beta=FALSE,gamma=FALSE)
plot(mfTSforecasts)
mfTSforecast2 <- forecast:::forecast.HoltWinters(mfTSforecasts,h=12)
plot(mfTSforecast2)

plot(singleTSadjusted)
singleTSforecasts <- HoltWinters(singleTS,beta=FALSE,gamma=FALSE)
plot(singleTSforecasts)
singleTSforecast2 <- forecast:::forecast.HoltWinters(singleTSforecasts,h=4)
singleTSarima <- auto.arima(singleTS)
singleTSarima
singleTSarimaforecasts <- forecast:::forecast.Arima(singleTSarima,h=4)
forecast:::plot.forecast(singleTSarimaforecasts)



laGoogleTS <- ts(laOG2$Legal.advice...United.States.,start=c(2012,11),frequency=12)
plot(laGoogleTS)
laGoogleTScomponents <- decompose(laGoogleTS)


par(mfrow=c(1,2))
plot(mfTScomponents)
plot(laGoogleTScomponents)
plot(laGoogleTScomponents$seasonal) 
View(mfTS)
df <- data.frame(laGoogleTS)
df2 <- data.frame(mfTS)
df3 <- data.frame(idx=rownames(df),lag=df$laGoogleTS,mf=df2)
df3 <- data.frame(idx=rownames(df),lag=laGoogleTScomponents$seasonal,mf=mfTScomponents$seasonal)
df3
sf <- max(mfTScomponents$seasonal)/max(laGoogleTScomponents$seasonal)
sf
plot(x=df3$idx,y=df3$mf,type="l",col='red',xlab="Time",ylab="",main=("Seasonal Trends for Questions/Month and Legal Advice Search Popularity Time Seris"))+lines(x=df3$idx,y=sf*df3$lag,col='blue') + 
  legend(73, -170, legend=c("Legal Questions/Month", "Legal Advice Search Popularity"),col=c("red", "blue"), lty=1:1, cex=0.8)
geom_line(aes(y=lag)) + geom_line(aes(y=mf))
ggplot(df, aes(x)) +                    # basic graphical object
  geom_line(aes(y=y1), colour="red") +  # first layer
  geom_line(aes(y=y2), colour="green")  # second layer
library(ggplot2)


View(table(questions$Category,questions$Subcategory))



