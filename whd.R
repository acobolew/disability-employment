library(data.table)
library(readxl)
library(mgcv)
library(ggplot2)

states.dc.pr.us.DT <- data.table(geography.abb=c(state.abb, 'DC', 'PR', 'US'), geography.name=c(state.name, 'District of Columbia', 'Puerto Rico', 'United States'))

whd.tableau.DT <- data.table(read_excel('whd_data/14(c) Certificate Holder Archived List_data.xlsx'))

excel.sheets.both <- excel_sheets('whd_data/from archive.org/Both.xlsx')
excel.sheets.crp <- excel_sheets('whd_data/from archive.org/CRP.xlsx')
excel.sheets.crp.data.on.num.certs.only <- excel_sheets('whd_data/from archive.org/CRP data on num certs only.xlsx')
whd.pretableau.both.DT <- (map(excel.sheets.both, \(sheet.name) {
  cbind(list.date=as.Date(gsub('Both ', '', sheet.name)), read_excel('whd_data/from archive.org/Both.xlsx', sheet=sheet.name) %>% select(-contains('Initial')))
}) %>% rbindlist)[ list.date != '2021-10-01'] # 2021-10-01 already in tableau data table
whd.pretableau.crp.DT <- map(excel.sheets.crp, \(sheet.name) {
  cbind(list.date=as.Date(gsub('CRP ', '', sheet.name)), read_excel('whd_data/from archive.org/CRP.xlsx', sheet.name))
}) %>% rbindlist
whd.pretableau.crp.data.on.num.certs.only.DT <- (map(excel.sheets.crp.data.on.num.certs.only, \(sheet.name) {
  cbind(list.date=as.Date(sheet.name), read_excel('whd_data/from archive.org/CRP data on num certs only.xlsx', sheet.name) %>% select('State'))
}) %>% rbindlist)[!is.na(State)]


whd.crp.num.by.state.DT  <- rbindlist(list(
  whd.tableau.DT[grep('CRP', `Certificate Type`)][ , .(list.date=as.Date(`List Date`), State)],
  whd.pretableau.both.DT[grep('CRP', `Certification Type`), .(list.date, State)],
  whd.pretableau.crp.DT[ , .(list.date, State)],
  whd.pretableau.crp.data.on.num.certs.only.DT
))[ , .N, by=.(list.date, State)]
whd.crp.num.by.state.DT <- whd.crp.num.by.state.DT[CJ(State=unique(whd.crp.num.by.state.DT$State), list.date=unique(whd.crp.num.by.state.DT$list.date)), on=c('State', 'list.date')]
whd.crp.num.by.state.DT[is.na(N), N := 0]

ggplot(whd.crp.num.by.state.DT[State=='MO'], aes(x=list.date, y=N)) + facet_wrap(~State, scales='free_y') + expand_limits(y=0) + geom_line()
ggplot(whd.crp.num.by.state.DT[ , .(N=sum(N)), by=list.date], aes(x=list.date, y=N)) + expand_limits(y=0) + geom_line()


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

whd.years.DT <- data.table(year=2015:2024)[ , target.date := as.Date(paste0(year, '-07-01'))][]
whd.years.DT[year==2015, list.date := as.Date(paste0(year, '-10-01'))]
whd.years.DT[year==2017, list.date := as.Date(paste0(year, '-01-01'))]
whd.years.DT[!(year %in% c(2015, 2017)), list.date := as.Date(paste0(year, '-07-01'))]

workers.paid.submin.DT <- whd.crp.DT[ , .(workers.paid.submin=sum(c(workers.paid.submin), na.rm=TRUE)), by=.(state, list.date)][order(list.date, state)][
  whd.years.DT, on='list.date'
]

workers.paid.submin.DT <- workers.paid.submin.DT[
  CJ.dt(states.dc.pr.us.DT[!(geography.abb %in% c('PR', 'US'))], data.table(year=sort(unique(workers.paid.submin.DT$year)))),
  on=c(state='geography.abb', year='year')
][is.na(workers.paid.submin), workers.paid.submin := 0][]

pop.by.state.dc.pr.DT <- rbindlist(list(
  data.table(get_estimates(geography='state', variable='POP', breakdown='AGEGROUP', breakdown_labels=TRUE, time_series=TRUE, vintage=2019))[DATE >= 3][ , .(GEOID, NAME, variable, year=DATE-3+2010, pop=value)],
  data.table(get_estimates(geography='state', variable='POPESTIMATE', breakdown='AGEGROUP', breakdown_labels=TRUE, time_series=TRUE, vintage=2023))[ , .(GEOID, NAME, variable, year, pop=value)]
))
setorder(pop.by.state.dc.pr.DT, GEOID, year)

workers.paid.submin.per.capita.DT <- pop.by.state.dc.pr.DT[workers.paid.submin.DT, on=c(NAME='geography.name', year='year')][ , workers.paid.submin.per.capita := workers.paid.submin/pop][]

ggplot(
  workers.paid.submin.per.capita.DT[year==2023],
  aes(x=year, y=workers.paid.submin.per.capita)
) + geom_point() + geom_line() + facet_wrap(~state)


cor.dataset.DT <- workers.paid.submin.per.capita.DT[,.(NAME, state, year, workers.paid.submin.per.capita)][analysis.dataset.wide.2.DT[NAMEf != 'Puerto Rico' & (cognitive==1 | no.disability==1)], on=c(NAME='state', year='year')][!is.na(workers.paid.submin.per.capita)]
cor.dataset.DT[ , odds.employed := prop.estimate.employed/(1-prop.estimate.employed)]

cor.dataset.DT[ cognitive==1, cor(workers.paid.submin.per.capita, prop.estimate.employed, method='spearman'), by=year]

cor.dataset.DT[ , cor(workers.paid.submin.per.capita[cognitive==1], prop.estimate.employed[cognitive==1]-prop.estimate.employed[no.disability==1]), by=year]

cor.dataset.DT[ , cor(
    workers.paid.submin.per.capita[cognitive==1],
    log(
      prop.estimate.employed[cognitive==1]/(1-prop.estimate.employed[cognitive==1]) /
        ( prop.estimate.employed[no.disability==1]/(1-prop.estimate.employed[no.disability==1]) )
    )
  ),
  by=year
]

ggplot(cor.dataset.DT[cognitive==1], aes(x=(workers.paid.submin.per.capita), y=prop.estimate.employed, label=state)) + geom_text() + facet_wrap(~year)
ggplot(cor.dataset.DT[cognitive==1], aes(x=sqrt(workers.paid.submin.per.capita), y=prop.estimate.employed, label=state)) + geom_text() + facet_wrap(~year)
)

gap.DT <- cor.dataset.DT[ , .( workers.paid.submin.per.capita, OR = odds.employed[cognitive==1]/odds.employed[cognitive==0], prop.employed.gap=prop.estimate.employed[no.disability==1]-prop.estimate.employed[cognitive==1]), by=.(state, year)][seq(from=1, to=.N,by=2)]
ggplot(gap.DT, aes(x=sqrt(workers.paid.submin.per.capita), y=log(OR), label=state)) + geom_text() + facet_wrap(~year)
ggplot(gap.DT, aes(x=sqrt(workers.paid.submin.per.capita), y=log(OR), group=state)) + geom_path(arrow=arrow(angle=15, ends='last', type='closed', length=unit(5, 'points'))) + facet_wrap(~state)
gap.DT[ , cor(workers.paid.submin.per.capita, OR, method='spearman'), by=year]
gap.DT[ , cor(workers.paid.submin.per.capita, OR, method='pearson'), by=year]
gap.DT[ , cor(sqrt(workers.paid.submin.per.capita), log(OR), method='pearson'), by=year]
gap.DT[ , cor(sqrt(workers.paid.submin.per.capita), prop.employed.gap, method='pearson'), by=year]





fit.cog.0 <- bam(
  prop.estimate.employed ~
    s(year) +
    s(year, by=cognitive) +
    s(NAMEf, bs='re')
  # +
    # s(cognitive, NAMEf, bs='re'),
  , weights=neff.denom.employed,
  family=quasibinomial,
  discrete=TRUE,
  data=workers.paid.submin.per.capita.DT[,.(NAME, state, year, workers.paid.submin.per.capita)][analysis.dataset.wide.2.DT[NAMEf != 'Puerto Rico' & (cognitive==1 | no.disability==1)], on=.(state, year)]
)
fit.cog.1 <- bam(
  prop.estimate.employed ~
    s(year) +
    s(year, by=cognitive) +
    s(NAMEf, bs='re') +
    year/workers.paid.submin.per.capita
  # +
  # s(cognitive, NAMEf, bs='re'),
  , weights=neff.denom.employed,
  family=quasibinomial,
  discrete=TRUE,
  data=workers.paid.submin.per.capita.DT[,.(NAME, state, year, workers.paid.submin.per.capita)][analysis.dataset.wide.2.DT[NAMEf != 'Puerto Rico'], on=.(state, year)]
)
fit.cog.2 <- bam(
  prop.estimate.employed ~
    s(year) +
    s(year, by=cognitive) +
    s(NAMEf, bs='re') +
    s(cognitive, NAMEf, bs='re'),
  weights=neff.denom.employed,
  family=quasibinomial,
  discrete=TRUE,
  data=workers.paid.submin.per.capita.DT[,.(NAME, state, year, workers.paid.submin.per.capita)][analysis.dataset.wide.2.DT[NAMEf != 'Puerto Rico' & (cognitive==1 | no.disability==1)], on=.(state, year)]
)


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