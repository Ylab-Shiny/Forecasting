
# 最頻出パラメータ ----------------------------------------------------------------
library(dplyr)
library(forecast)
library(readr)
library(ggplot2)

user.dir <- Sys.getenv("USERPROFILE")
work.dir <- paste0(user.dir, "\\Dropbox\\Yamaha-lab\\user files\\Bishnu\\Data\\one year data\\")
setwd(work.dir)
Dataset <- read_csv("data_2018_summarise_cor&linear_interpolation.csv", col_names = FALSE, skip = 1)

# 東キャンパス受電電力量
campus_data <- cbind.data.frame(time=Dataset$X1, Dataset$X2+Dataset$X2)
colnames(campus_data)[2] <- "value"

dataset <- campus_data
dataset1 <- mutate(dataset,dates = as.Date(substr(dataset$time,1,10)))
##############################################################
x <- data.frame(dataset$value)
tx <- strptime(unlist(Dataset$X1),"%Y/%m/%d %H:%M")
time.2015 <- format(tx, "%H:%M")
date.2015 <- format(tx, "%Y/%m/%d")
date.day <- levels(factor(date.2015))
hour.2015 <- levels(factor(time.2015))
lab.date <- list(date.day,hour.2015)
y <- matrix(x[1:nrow(x),],ncol=24,byrow=TRUE,dimnames=lab.date)

inital.v <- apply(y[,7:12],2, quantile, seq(0,1,1/7))
#inital.v <- apply(y,2, quantile, seq(0,1,1/7))
kmean.y <- kmeans(y[,7:12], inital.v[2:7,])

y1 <-data.frame(y) 
rownames(y1)<- rownames(y)
y1 <- data.frame(y1,kmean.y$cluster,date= as.Date(rownames(y1)))
###########################################################
ce<-data.frame(kmean.y$cluster)
nn<- data.frame(rep(ce$kmean.y.cluster[1],24),stringsAsFactors = F)
colnames(nn)<- "A"
#...IMP... note only for 2015 year from 2 to 366 for other year it is from 2 to 365
for(i in 2:365){
  
  n<- data.frame(rep(ce$kmean.y.cluster[i],24),stringsAsFactors = F)
  colnames(n)<-"A"
  nn<-rbind(nn,n)
  
}

Dataset1<-cbind(Dataset,nn)
#################################################
myDate <- readline(prompt="Date ")
##############
Date <- as.Date(myDate)
#................................................................
mm <- mutate(Dataset1,date = as.Date(substr(Dataset1$X1,1,10)))
mmm <- filter(mm,date ==Date)

basedata<-filter(Dataset1,Dataset1$A==mmm$A[2]) # change cluster number from here
#...............................................................
basedata <- mutate(basedata,date = as.Date(substr(basedata$X1,1,10))) 
basedata$X1 <- as.POSIXct(basedata$X1)
#unique(as.Date(basedata$X1))


fdata <- filter(basedata,date == Date)
ww <- which(basedata$date ==Date)
aa <- ww[1]-(168+0)########### number of days use garne prediction ko lagi
bb <- ww[1]
train_data <- basedata[aa:bb,]
train_ts <- ts(train_data[[2]]+train_data[[3]],start = 1, frequency = 24)

manydays <-  basedata[aa:(bb+24),]

#########

Model <- auto.arima(train_ts, test = "adf")######note 3 days vanda kam vayo vane error aucha so always more than 3 days


# 予測値
pred_values <- predict(Model, n.ahead = 24)$pred

pred_data <- cbind.data.frame(basedata[ww,], value=pred_values)
pred_data$X1 <- as.POSIXct(pred_data$X1)

# 比較図の作成
predictions24_plot <- ggplot() +
  geom_line(data =manydays, aes(x= 1:193, y=X2+X3,group=1)) +
  geom_line(data = pred_data, aes(x=170:193,group=1, y=value, color="red"), linetype=2) +theme(legend.position = "none")
print(predictions24_plot)
