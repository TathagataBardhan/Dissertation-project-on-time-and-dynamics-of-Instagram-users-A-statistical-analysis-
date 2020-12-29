rm(list=ls())
data=read.csv("cardio_train.csv",header=T)
data
Mydata=data[,-1]
sapply(Mydata,function(x) sum(is.na(x)))
head(Mydata)
summary(Mydata)
dim(Mydata)
df=unique(Mydata)
dim(df)

boxplot(df$age)
x=length(boxplot(df$height)$out);x
y=length(boxplot(df$weight)$out);y
boxplot(df$ap_hi)
boxplot(df$ap_lo)

BMI=(df$weight/(df$height*df$height))*10000
pulse_pressure=(df$ap_hi)-(df$ap_lo)
a=length(boxplot(BMI)$out);a
b=boxplot(pulse_pressure)
dim(BMI)
dim(pulse_pressure)
df[,1]
new_df=data.frame(c(df[,1],df[,2],BMI,pulse_pressure,df[,7],df[,8],df[,9],df[,10],df[,11],df[,12]))

new_df(colnames)=c("Age","Gender","BMI","Pulse_Pressure","Cholestrol","Gluc","Smoke","Alco","Active","Cardio")
head(new_df)
library(car)
Boxplot(df$age)
Boxplot(df$height)

round((cor(Mydata[,])),4)

dim(df)
head(df)

a=boxplot(df$age)$out
b=boxplot(df$height)$out
c=boxplot(df$weight)$out
d=boxplot(df$ap_hi)$out
e=boxplot(df$ap_lo)$out
x=c()
for(i in 1:length(a)){
x=c(x,which(df$age==a[i]))
}
y=c()
for(i in 1:length(b)){
y=c(y,which(df$height==b[i]))
}
z=c()
for(i in 1:length(c)){
z=c(z,which(df$weight==c[i]))
}
p=c()
for(i in 1:length(d)){
p=c(p,which(df$ap_hi==d[i]))
}
q=c()
for(i in 1:length(e)){
q=c(q,which(df$ap_lo==e[i]))
}

data_cardio=df
data_cardio$age[x]=max(data_cardio$age[-x])
data_cardio$height[y]=max(data_cardio$height[-y])
data_cardio$weight[z]=max(data_cardio$weight[-z])
data_cardio$ap_hi[p]=max(data_cardio$ap_hi[-p])
data_cardio$ap_lo[q]=max(data_cardio$ap_lo[-q])

boxplot(data_cardio$age)
boxplot(data_cardio$height)
boxplot(data_cardio$weight)
boxplot(data_cardio$ap_hi)
boxplot(data_cardio$ap_lo)
dim(data_cardio)
#free from outliers
getwd()
write.csv(data_cardio,"clean_cardio_data.csv",row.names=F)




























