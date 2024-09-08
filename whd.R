library(data.table)
library(readxl)
library(mgcv)
library(ggplot2)

states.dc.pr.us.DT <- data.table(geography.abb=c(state.abb, 'DC', 'PR', 'US'), geography.name=c(state.name, 'District of Columbia', 'Puerto Rico', 'United States'))

whd.tableau.DT <- data.table(read_excel('whd_data/14(c) Certificate Holder Archived List_data.xlsx'))

excel.sheets.both <- excel_sheets('whd_data/from archive.org/Both.xlsx')
excel.sheets.crp <- excel_sheets('whd_data/from archive.org/CRP.xlsx')
whd.pretableau.both.DT <- map(excel.sheets.both, \(sheet.name) {
  cbind(list.date=as.Date(gsub('Both ', '', sheet.name)), read_excel('whd_data/from archive.org/Both.xlsx', sheet=sheet.name) %>% select(-contains('Initial')))
}) %>% rbindlist
whd.pretableau.crp.DT <- map(excel.sheets.crp, \(sheet.name) {
  cbind(list.date=as.Date(gsub('CRP ', '', sheet.name)), read_excel('whd_data/from archive.org/CRP.xlsx', sheet.name))
}) %>% rbindlist

whd.crp.DT <- rbindlist(list(
  whd.pretableau.both.DT[`Certification Type`=='Community Rehab Program (CRP)', .(
    list.date, state=State, cert.status=`Cert Status`, workers.paid.submin=`Number of Workers Paid Subminimum Wages`
  )],
  whd.pretableau.crp.DT[ , .(
    list.date, state=State, cert.status=`Cert Status`, workers.paid.submin=`Number of Workers Paid Subminimum Wages`
  )],
  whd.tableau.DT[`Certificate Type`=='Community Rehab Program (CRP)', .(
    list.date=as.Date(`List Date`), state=State, cert.status=Status, workers.paid.submin=as.numeric(`Workers Paid Subminimum Wages`)
  )]
))

whd.crp.DT[ , .(workers.paid.submin=sum(workers.paid.submin, na.rm=TRUE), certs.issued=length(.I[cert.status=='Issued']), certs.pending=length(.I[cert.status=='Pending'])), by=list.date][order(list.date)]
whd.crp.DT[ month(list.date)==7, .(workers.paid.submin=sum(workers.paid.submin, na.rm=TRUE), certs.issued=length(.I[cert.status=='Issued']), certs.pending=length(.I[cert.status=='Pending'])), by=list.date][order(list.date)]

workers.paid.submin.DT <- states.dc.pr.us.DT[
  whd.crp.DT[ month(list.date)==7, .(workers.paid.submin=sum(c(0, workers.paid.submin), na.rm=TRUE), year=year(list.date)), by=.(state, list.date)][order(list.date)], # herehere fix code to have 0 for state/year combos with no certs
  on=c(geography.abb='state')
]

pop.by.state.dc.pr.DT <- rbindlist(list(
  data.table(get_estimates(geography='state', variable='POP', breakdown='AGEGROUP', breakdown_labels=TRUE, time_series=TRUE, vintage=2019))[DATE >= 3][ , .(GEOID, NAME, variable, year=DATE-3+2010, pop=value)],
  data.table(get_estimates(geography='state', variable='POPESTIMATE', breakdown='AGEGROUP', breakdown_labels=TRUE, time_series=TRUE, vintage=2023))[ , .(GEOID, NAME, variable, year, pop=value)]
))
setorder(pop.by.state.dc.pr.DT, GEOID, year)

workers.paid.submin.per.capita.DT <- pop.by.state.dc.pr.DT[workers.paid.submin.DT, on=c(NAME='geography.name', year='year')][ , workers.paid.submin.per.capita := workers.paid.submin/pop][]

ggplot(
  whd.crp.DT[ , .(workers.paid.submin=sum(workers.paid.submin, na.rm=TRUE), certs.issued=length(.I[cert.status=='Issued']), certs.pending=length(.I[cert.status=='Pending'])), by=list.date][order(list.date)],
  aes(x=list.date, y=workers.paid.submin)
) + geom_point() + geom_line()

ggplot(
  workers.paid.submin.per.capita.DT,
  aes(x=year, y=workers.paid.submin.per.capita)
) + geom_line() + geom_point() + facet_wrap(~NAME)



whd.tableau.DT[ , sum(as.numeric(`Workers Paid Subminimum Wages`), na.rm=TRUE)], by=`Certificate Type`]




whd.pretableau.both.DT <- data.table(cbind(list.date=as.Date('2020-01-01'), read_excel('whd_data/from archive.org/Both.xlsx', sheet='Both 2020-01-01')))

whd.pretableau.both.DT[ , sum(`Number of Workers Paid Subminimum Wages`,na.rm=TRUE), by=`Certification Type`]

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