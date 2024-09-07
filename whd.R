library(data.table)
library(readxl)
library(mgcv)
library(ggplot2)
whd.DT <- data.table(read_excel('whd_data/14(c) Certificate Holder Archived List_data.xlsx'))

w.by.date.DT <- whd.DT[ , .(w=sum(as.numeric(`Workers Paid Subminimum Wages`), na.rm=TRUE)), by=.(list.date=as.Date(`List Date`))]
w.by.date.DT[ , moelapsed := as.numeric((list.date-min(list.date))/365.25*12)]
w.by.date.DT[ , yrelapsed := as.numeric((list.date-min(list.date))/365.25)]
w.by.date.DT[ , yr := yrelapsed + min(year(list.date))]

fit0 <- gam(w ~ s(yr), data=w.by.date.DT)
fit1 <- gam(w ~ s(yr, bs='cr') + s(yrelapsed %% 1, bs='cc'), data=w.by.date.DT)
fit2 <- gam(w ~ te(yr, yrelapsed %% 1, bs=c('cr', 'cc')), data=w.by.date.DT)

par(mfrow=c(2,2)); plot(fit0); title('fit0'); plot(fit1); title('fit1'); plot(fit2); title('fit2')
par(mfrow=c(1,1)); plot(fit0)

ggplot(w.by.date.DT, aes(x=yr, y=w)) + geom_line() + geom_point()