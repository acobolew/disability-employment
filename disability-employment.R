
set.seed(66)

states.dc.pr.DT <- data.table(
  geography.abb=c(state.abb, 'DC', 'PR'),
  geography.name=c(state.name, 'District of Columbia', 'Puerto Rico')
)

acs1.loaded.variables.DT <- data.table( load_variables(2023, dataset='acs1') )
acs1.target.variables.DT <- acs1.loaded.variables.DT[toupper(concept)=='EMPLOYMENT STATUS BY DISABILITY STATUS AND TYPE']

acs1.years <- do.call(lst, as.list(as.numeric(c(2010:2019, 2021:2023))))
us.employment.estimates.and.moe.DT <- map_dfr(
  acs1.years,
  ~get_acs(geography='us', variables=acs1.target.variables.DT[ , name], survey='acs1', year=.x),
  .id='year'
) %>% data.table

us.employment.linked.data.DT <- acs1.target.variables.DT[us.employment.estimates.and.moe.DT, on=c(name='variable')]
us.employment.analysis.dataset.DT <- us.employment.linked.data.DT[grepl('With a disability:!!|No disability', label)]

us.employment.analysis.dataset.DT[, employed := as.numeric(grepl('Employed', label))]
us.employment.analysis.dataset.DT[, unemployed := as.numeric(grepl('Unemployed', label))]
us.employment.analysis.dataset.DT[, notinlaborforce := as.numeric(grepl('Not in labor force', label))]

us.employment.analysis.dataset.DT[ , variable := ifelse(employed, 'employed', ifelse(unemployed, 'unemployed', ifelse(notinlaborforce, 'notinlaborforce', NA_character_)))][!is.na(variable)]
us.employment.analysis.dataset.DT[ , rest.of.variable := label %>% gsub('In the labor force:!!Employed:!!', '', .) %>% gsub('In the labor force:!!Unemployed:!!', '', .) %>% gsub('Not in labor force:!!', '', .)]
us.employment.analysis.dataset.DT[ , denom.estimate := sum(estimate), by=.(GEOID, NAME, year, rest.of.variable)]
us.employment.analysis.dataset.DT[ , denom.moe := moe_sum(moe, estimate), by=.(GEOID, NAME, year, rest.of.variable)]
us.employment.analysis.dataset.DT[ , prop.estimate := estimate/denom.estimate]
us.employment.analysis.dataset.DT[ , prop.moe := moe_prop(estimate, denom.estimate, moe, denom.moe)]
us.employment.analysis.dataset.DT[ , prop.se := prop.moe/qnorm(.95)]
us.employment.analysis.dataset.DT[ , neff.denom := prop.estimate*(1-prop.estimate)/(prop.se^2)]

us.employment.analysis.dataset.wide.DT <- us.employment.analysis.dataset.DT %>%
  pivot_wider(
    id_cols=c('GEOID', 'NAME', 'year', 'rest.of.variable'),
    names_from=variable,
    names_sep = '.',
    values_from=c(estimate, moe, prop.estimate, prop.moe, denom.estimate, denom.moe, neff.denom)
  ) %>% data.table

us.employment.analysis.dataset.wide.DT[, no.disability := as.numeric(grepl('No disability', rest.of.variable))]
us.employment.analysis.dataset.wide.DT[, hearing := as.numeric(grepl('With a hearing difficulty', rest.of.variable))]
us.employment.analysis.dataset.wide.DT[, vision := as.numeric(grepl('With a vision difficulty', rest.of.variable))]
us.employment.analysis.dataset.wide.DT[, cognitive := as.numeric(grepl('With a cognitive difficulty', rest.of.variable))]
us.employment.analysis.dataset.wide.DT[, ambulatory := as.numeric(grepl('With an ambulatory difficulty', rest.of.variable))]
us.employment.analysis.dataset.wide.DT[, selfcare := as.numeric(grepl('With a self-care difficulty', rest.of.variable))]
us.employment.analysis.dataset.wide.DT[, independentliving := as.numeric(grepl('With an independent living difficulty', rest.of.variable))]

us.employment.analysis.dataset.wide.DT[ , year := as.numeric(year)]

us.employment.analysis.dataset.wide.DT[
  , `:=`(
    employmentratio.estimate = prop.estimate.employed/prop.estimate.employed[no.disability==1],
    employmentratio.moe = ifelse(no.disability==1, 0, moe_ratio(prop.estimate.employed, prop.estimate.employed[no.disability==1], prop.moe.employed, prop.moe.employed[no.disability==1]))
  ),
by=year]
us.employment.analysis.dataset.wide.DT[ , date := as.Date(paste0(year, '-07-01'))]
us.employment.analysis.dataset.wide.DT[ , disability.status := factor(
  ifelse(no.disability==1, 'no.disability', ifelse(hearing==1, 'hearing', ifelse(vision==1, 'vision', ifelse(cognitive==1, 'cognitive', ifelse(ambulatory==1, 'ambulatory', ifelse(selfcare==1, 'selfcare', ifelse(independentliving==1, 'independentliving', NA_character_))))))),
  levels=c('no.disability', 'hearing', 'vision', 'cognitive', 'ambulatory', 'selfcare', 'independentliving'),
  labels=c('No Disability', 'Hearing', 'Vision', 'Cognitive', 'Ambulatory', 'Self-care', 'Independent Living')
)]


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

# correct apparent error in data:
# likely error with a certificate holder on two dates:
whd.pretableau.crp.DT[`Cert Num`=='05-03943-S-027' & list.date=='2015-10-01' & `Number of Workers Paid Subminimum Wages`==2075, `Number of Workers Paid Subminimum Wages` := 107]
whd.pretableau.crp.DT[`Cert Num`=='05-03943-S-027' & list.date=='2016-01-01' & `Number of Workers Paid Subminimum Wages`==2075, `Number of Workers Paid Subminimum Wages` := 107]
# definitely error with a certificate holder on one date:
whd.pretableau.crp.DT[`Cert Num`=='05-03105-S-033' & list.date=='2016-01-01' & `Number of Workers Paid Subminimum Wages`==13674, `Number of Workers Paid Subminimum Wages` := 136]
# likely error with a certificate holder on on one date:
whd.pretableau.crp.DT[`Cert Num`=='05-11782-S-073' & list.date=='2016-01-01' & `Number of Workers Paid Subminimum Wages`==2431, `Number of Workers Paid Subminimum Wages` := 31]

whd.crp.programs.by.state.DT  <- rbindlist(list(
  whd.tableau.DT[grep('CRP', `Certificate Type`)][ , .(list.date=as.Date(`List Date`), State)],
  whd.pretableau.both.DT[grep('CRP', `Certification Type`), .(list.date, State)],
  whd.pretableau.crp.DT[ , .(list.date, State)],
  whd.pretableau.crp.data.on.num.certs.only.DT
))[ , .N, by=.(list.date, State)]
whd.crp.programs.by.state.DT <- whd.crp.programs.by.state.DT[CJ(State=unique(whd.crp.programs.by.state.DT$State), list.date=unique(whd.crp.programs.by.state.DT$list.date)), on=c('State', 'list.date')]
whd.crp.programs.by.state.DT[is.na(N), N := 0]

pop.est.by.state.DT <- states.dc.pr.DT[rbindlist(list(
    data.table(get_estimates(geography='state', variable='POP', vintage=2019, time_series=TRUE))[DATE > 2 , .(
      GEOID, NAME, variable, year=2010-3+DATE, pop=value
    )],
    data.table(get_estimates(geography='state', variable='POPESTIMATE', vintage=2023, time_series=TRUE))[ , .(GEOID, NAME, variable, year, pop=value)]
  )),
  on=c(geography.name='NAME')
]

whd.crp.workers.by.state.DT  <- rbindlist(list(
  whd.tableau.DT[grep('CRP', `Certificate Type`)][ , .(list.date=as.Date(`List Date`), State, workers.paid.submin=as.numeric(`Workers Paid Subminimum Wages`))],
  whd.pretableau.both.DT[grep('CRP', `Certification Type`), .(list.date, State, workers.paid.submin=`Number of Workers Paid Subminimum Wages`)],
  whd.pretableau.crp.DT[ , .(list.date, State, workers.paid.submin=`Number of Workers Paid Subminimum Wages`)]
))[ , .(workers.paid.submin=sum(workers.paid.submin, na.rm=TRUE)), by=.(list.date, State)]
# on next line, State=unique(whd.crp.programs.by.state.DT$State ensures that Guam and NH are included even though they dropped 14(c) after DOL started reporting # workers
whd.crp.workers.by.state.DT <- whd.crp.workers.by.state.DT[CJ(State=unique(whd.crp.programs.by.state.DT$State), list.date=unique(whd.crp.workers.by.state.DT$list.date)), on=c('State', 'list.date')]
whd.crp.workers.by.state.DT[is.na(workers.paid.submin), workers.paid.submin := 0]
whd.crp.workers.by.state.DT[ , year := year(list.date)]
whd.crp.workers.by.state.DT <- pop.est.by.state.DT[whd.crp.workers.by.state.DT, on=c(geography.abb='State', year='year')]
whd.crp.workers.by.state.DT[ , workers.paid.submin.per.capita := workers.paid.submin/pop]
whd.crp.workers.by.state.DT[ , list.dates.per.year := length(unique(list.date)), by=year]


okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
okabemod <- c("#E69F00", "#56B4E9", "#009E73", "#000000", "#0072B2", "#D55E00", "#CC79A7")

#####################################
# Figure 1 (time series nationally)

names(okabemod) <- c('Cognitive', 'Hearing', 'Vision', 'No Disability', 'Ambulatory', 'Independent Living', 'Self-care')
okabemodfills <- okabemod
okabemodfills[c('No Disability', 'Vision', 'Ambulatory', 'Self-care')] <- 'white'
dis.type.shapes <- c('No Disability'=21, Hearing=24, Vision=22, Cognitive=25, Ambulatory=24, 'Independent Living'=22, 'Self-care'=25)

us.employment.fig <- ggplot(us.employment.analysis.dataset.wide.DT, aes(x=as.Date(paste0(year, '-07-01')))) +
  scale_shape_manual(values=dis.type.shapes) +
  scale_fill_manual(values=okabemodfills) +
  scale_color_manual(values=okabemod) +
  scale_x_date(limits=as.Date(c('2010-01-01', '2033-01-01')), breaks=seq(as.Date('2010-01-01'), as.Date('2025-01-01'), by='5 year'), minor_breaks=NULL) +
  # scale_x_continuous(limits=c(2010,2030), breaks=seq(2010, 2025, by=5), minor_breaks=NULL) +
  # expand_limits(y=0) +
  geom_line(aes(y=prop.estimate.employed, color=disability.status), show.legend=FALSE) +
  geom_linerange(aes(color=disability.status, ymin=prop.estimate.employed-prop.moe.employed/qnorm(.95)*qnorm(.975), ymax=prop.estimate.employed+prop.moe.employed/qnorm(.95)*qnorm(.975)), show.legend=FALSE) +
  geom_point(aes(y=prop.estimate.employed, color=disability.status, fill=disability.status, shape=disability.status), show.legend=FALSE) +
  geom_text_repel(
    data=us.employment.analysis.dataset.wide.DT[year==max(year)],
    aes(label=disability.status, color=disability.status, y=prop.estimate.employed),
    direction='y',
    nudge_x=180,
    hjust=0,
    segment.color=NA,
    show.legend=FALSE,
    size=3
  ) +
  scale_y_continuous(label=label_percent()) +
  ylab('Employment Rate') +
  theme_grey(base_size=12)
us.employment.ratio.fig <- ggplot(us.employment.analysis.dataset.wide.DT[no.disability==0], aes(x=as.Date(paste0(year, '-07-01')))) +
  scale_shape_manual(values=dis.type.shapes) +
  scale_fill_manual(values=okabemodfills) +
  scale_color_manual(values=okabemod) +
  scale_x_date(limits=as.Date(c('2010-01-01', '2033-01-01')), breaks=seq(as.Date('2010-01-01'), as.Date('2025-01-01'), by='5 year'), minor_breaks=NULL) +
  # scale_x_continuous(limits=c(2010,2030), breaks=seq(2010, 2025, by=5), minor_breaks=NULL) +
  # expand_limits(y=0) +
  geom_line(aes(y=employmentratio.estimate, color=disability.status), show.legend=FALSE) +
  geom_linerange(aes(color=disability.status, ymin=employmentratio.estimate-employmentratio.moe/qnorm(.95)*qnorm(.975), ymax=employmentratio.estimate+employmentratio.moe/qnorm(.95)*qnorm(.975)), show.legend=FALSE) +
  geom_point(aes(y=employmentratio.estimate, color=disability.status, fill=disability.status, shape=disability.status), show.legend=FALSE) +
  geom_text_repel(
    data=us.employment.analysis.dataset.wide.DT[no.disability==0][year==max(year)],
    aes(label=disability.status, color=disability.status, y=employmentratio.estimate),
    direction='y',
    nudge_x=180,
    hjust=0,
    segment.color=NA,
    show.legend=FALSE,
    size=3
  ) +
  ylab('Employment Ratio\nre People with\nNo Disability') +
  theme_grey(base_size=12)
us.workers.fig <- ggplot(
  whd.crp.workers.by.state.DT[ , .(workers.paid.submin=sum(workers.paid.submin)), by=.(list.date)],
    aes(x=list.date, y=workers.paid.submin)
  ) +
  scale_x_date(limits=as.Date(c('2010-01-01', '2030-01-01')), breaks=seq(as.Date('2010-01-01'), as.Date('2025-01-01'), by='5 year'), minor_breaks=NULL) +
  # scale_x_continuous(limits=c(2010,2030), breaks=seq(2010, 2025, by=5), minor_breaks=NULL) +
  # expand_limits(y=0) +
  # geom_line(show.legend=FALSE) +
  geom_smooth(color='black', linewidth=1/2) +
  geom_point(size=1, show.legend=FALSE) +
  scale_y_continuous(label=label_comma()) +
  ylab('Workers Paid\nSubminimum Wage') +
  theme_grey(base_size=12)
us.programs.fig <- ggplot(
  whd.crp.programs.by.state.DT[ , .(crp.programs=sum(N)), by=.(list.date)],
  aes(x=list.date, y=crp.programs)
) +
  scale_x_date(limits=as.Date(c('2010-01-01', '2030-01-01')), breaks=seq(as.Date('2010-01-01'), as.Date('2025-01-01'), by='5 year'), minor_breaks=NULL, date_labels='%Y\n%b') +
  # scale_x_continuous(limits=c(2010,2030), breaks=seq(2010, 2025, by=5), minor_breaks=NULL) +
  # expand_limits(y=0) +
  # geom_line(show.legend=FALSE) +
  geom_smooth(color='black', linewidth=1/2) +
  geom_point(size=1, show.legend=FALSE) +
  scale_y_continuous(label=label_comma()) +
  ylab('Community\nRehab Programs') +
  xlab('Date') +
  theme_grey(base_size=12)
fig.1.us.timeseries <- egg::ggarrange(
  us.employment.fig +
    tag_facets(position='tr', tag_suffix='', tag_pool='A') +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank()),
  us.employment.ratio.fig +
    tag_facets(position='tr', tag_suffix='', tag_pool='B') +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank()),
  us.workers.fig +
    tag_facets(position='tr', tag_suffix='', tag_pool='C') +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank()),
  us.programs.fig +
    tag_facets(position='tr', tag_suffix='', tag_pool='D'),
  ncol=1
)
ggsave(width=4, height=8, filename='fig-1-us-timeseries.png', plot=fig.1.us.timeseries)

us.employment.analysis.dataset.wide.DT[no.disability==1 & (year==2010 | year == 2023), .(prop.estimate.employed)]
us.employment.analysis.dataset.wide.DT[cognitive==1 & (year==2010 | year == 2023), .(prop.estimate.employed)]
us.employment.analysis.dataset.wide.DT[cognitive==1 & (year==2010 | year == 2023), .(employmentratio.estimate)]

us.employment.analysis.dataset.wide.DT[
  cognitive==1 & (year==2010 | year == 2023)
  , .(
      year,
      diff.propemployed.estimate = prop.estimate.employed - prop.estimate.employed[year==2010],
      diff.propemployed.moe = moe_sum(prop.moe.employed, prop.moe.employed[year==2010]),
      denom.estimate.employed,
      denom.moe.employed
    )
][year==2023][ , `:=`(
  diff.ER.times.pop2023.estimate=diff.propemployed.estimate*denom.estimate.employed,
  diff.ER.times.pop2023.moe=moe_product(diff.propemployed.estimate, denom.estimate.employed, diff.propemployed.moe, denom.moe.employed)
)][]


#############

states.employment.estimates.and.moe.DT <- map_dfr(
  acs1.years,
  ~get_acs(geography='state', variables=acs1.target.variables.DT[ , name], survey='acs1', year=.x),
  .id='year'
) %>% data.table

states.employment.linked.data.DT <- acs1.target.variables.DT[states.employment.estimates.and.moe.DT, on=c(name='variable')]
states.employment.analysis.dataset.DT <- states.employment.linked.data.DT[grepl('With a disability:!!|No disability', label)]

states.employment.analysis.dataset.DT[, employed := as.numeric(grepl('Employed', label))]
states.employment.analysis.dataset.DT[, unemployed := as.numeric(grepl('Unemployed', label))]
states.employment.analysis.dataset.DT[, notinlaborforce := as.numeric(grepl('Not in labor force', label))]

states.employment.analysis.dataset.DT[ , variable := ifelse(employed, 'employed', ifelse(unemployed, 'unemployed', ifelse(notinlaborforce, 'notinlaborforce', NA_character_)))][!is.na(variable)]
states.employment.analysis.dataset.DT[ , rest.of.variable := label %>% gsub('In the labor force:!!Employed:!!', '', .) %>% gsub('In the labor force:!!Unemployed:!!', '', .) %>% gsub('Not in labor force:!!', '', .)]
states.employment.analysis.dataset.DT[ , denom.estimate := sum(estimate), by=.(GEOID, NAME, year, rest.of.variable)]
states.employment.analysis.dataset.DT[ , denom.moe := moe_sum(moe, estimate), by=.(GEOID, NAME, year, rest.of.variable)]
states.employment.analysis.dataset.DT[ , prop.estimate := estimate/denom.estimate]
states.employment.analysis.dataset.DT[ , prop.moe := moe_prop(estimate, denom.estimate, moe, denom.moe)]
states.employment.analysis.dataset.DT[ , prop.se := prop.moe/qnorm(.95)]
states.employment.analysis.dataset.DT[ , neff.denom := prop.estimate*(1-prop.estimate)/(prop.se^2)]

states.employment.analysis.dataset.wide.DT <- states.employment.analysis.dataset.DT %>%
  pivot_wider(
    id_cols=c('GEOID', 'NAME', 'year', 'rest.of.variable'),
    names_from=variable,
    names_sep = '.',
    values_from=c(estimate, moe, prop.estimate, prop.moe, denom.estimate, denom.moe, neff.denom)
  ) %>% data.table

states.employment.analysis.dataset.wide.DT[, no.disability := as.numeric(grepl('No disability', rest.of.variable))]
states.employment.analysis.dataset.wide.DT[, hearing := as.numeric(grepl('With a hearing difficulty', rest.of.variable))]
states.employment.analysis.dataset.wide.DT[, vision := as.numeric(grepl('With a vision difficulty', rest.of.variable))]
states.employment.analysis.dataset.wide.DT[, cognitive := as.numeric(grepl('With a cognitive difficulty', rest.of.variable))]
states.employment.analysis.dataset.wide.DT[, ambulatory := as.numeric(grepl('With an ambulatory difficulty', rest.of.variable))]
states.employment.analysis.dataset.wide.DT[, selfcare := as.numeric(grepl('With a self-care difficulty', rest.of.variable))]
states.employment.analysis.dataset.wide.DT[, independentliving := as.numeric(grepl('With an independent living difficulty', rest.of.variable))]

states.employment.analysis.dataset.wide.DT[ , year := as.numeric(year)]

states.employment.analysis.dataset.wide.DT[
  , `:=`(
    employmentratio.estimate = prop.estimate.employed/prop.estimate.employed[no.disability==1],
    employmentratio.moe = ifelse(no.disability==1, 0, moe_ratio(prop.estimate.employed, prop.estimate.employed[no.disability==1], prop.moe.employed, prop.moe.employed[no.disability==1]))
  ),
  by=year]
states.employment.analysis.dataset.wide.DT[ , date := as.Date(paste0(year, '-07-01'))]
states.employment.analysis.dataset.wide.DT[ , disability.status := factor(
  ifelse(no.disability==1, 'no.disability', ifelse(hearing==1, 'hearing', ifelse(vision==1, 'vision', ifelse(cognitive==1, 'cognitive', ifelse(ambulatory==1, 'ambulatory', ifelse(selfcare==1, 'selfcare', ifelse(independentliving==1, 'independentliving', NA_character_))))))),
  levels=c('no.disability', 'hearing', 'vision', 'cognitive', 'ambulatory', 'selfcare', 'independentliving'),
  labels=c('No Disability', 'Hearing', 'Vision', 'Cognitive', 'Ambulatory', 'Self-care', 'Independent Living')
)]

#####################################
# Figure 2 (time series by state)

states.employment.analysis.dataset.wide.DT[ , NAMEf := factor(NAME)]
states.employment.analysis.dataset.wide.DT[ , w.cog := NULL]
states.employment.analysis.dataset.wide.DT[cognitive==1 , w.cog := employmentratio.moe^(-2) / sum(employmentratio.moe^(-2))]

fit.ER.states.0 <-
  bam(
    employmentratio.estimate ~ t2(year, NAMEf, bs=c('tp', 're'), full=TRUE),
    data=states.employment.analysis.dataset.wide.DT[cognitive==1]
  )
fit.ER.states.1 <-
  bam(
    employmentratio.estimate ~ t2(year, NAMEf, bs=c('tp', 're'), full=TRUE),
    weights=w.cog,
    data=states.employment.analysis.dataset.wide.DT[cognitive==1]
  )

states.ER.preds.DT <- CJ(NAMEf=levels(states.employment.analysis.dataset.wide.DT$NAMEf), year=seq(2010, 2023, length=100))
states.ER.preds.DT[ , c('fit', 'se.fit') := as.data.table(predict(fit.ER.states.1, newdata=states.ER.preds.DT, se.fit=TRUE))]
states.ER.preds.DT <- states.dc.pr.DT[states.ER.preds.DT, on=c(geography.name='NAMEf')]
states.ER.preds.DT[ , date := as.Date('2010-07-01') + (year-2010)*365.25]

whd.crp.workers.by.state.DT[geography.abb != 'GU' & year==2023]

whd.crp.workers.by.state.DT[ , year.float := 2010 + as.numeric(list.date-as.Date('2010-07-01'))/365.25]
whd.crp.workers.by.state.DT[ , geography.abbf := factor(geography.abb)]
whd.crp.workers.by.state.2024.DT <- pop.est.by.state.DT[year==2023][ , year := 2024][whd.crp.workers.by.state.DT[year==2024], on='geography.abb'][ , workers.paid.submin.per.capita := workers.paid.submin/pop][]
whd.percapita.crp.workers.by.state.incl.2024.DT <- rbindlist(list(
  whd.crp.workers.by.state.DT[ year != 2024],
  whd.crp.workers.by.state.2024.DT[, names(whd.crp.workers.by.state.DT), with=FALSE]
))
fit.workers.per.capita.0 <-
  bam(
    workers.paid.submin.per.capita ~ t2(year.float, geography.abbf, bs=c('tp', 're'), full=TRUE),
    data=whd.percapita.crp.workers.by.state.incl.2024.DT[geography.abb != 'GU']
  )
whd.percapita.crp.workers.by.state.incl.2024.preds.DT <- CJ(geography.abbf=unique(whd.crp.workers.by.state.DT[geography.abb != 'GU']$geography.abbf), year.float=seq(2015.25, 2023.25, length=100))
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[ , c('fit', 'se.fit') := as.data.table(predict(fit.workers.per.capita.0, newdata=whd.percapita.crp.workers.by.state.incl.2024.preds.DT, se.fit=TRUE))]
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[ , date := as.Date('2010-07-01') + (year.float-2010)*365.25]
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[ , geography.abb := geography.abbf]



fig.2.ER.and.workers.by.state <- ggplot(
  states.dc.pr.DT[states.employment.analysis.dataset.wide.DT[cognitive==1], on=c(geography.name='NAME')],
) +
  facet_wrap(~geography.abb) +
  geom_ribbon(data=states.ER.preds.DT, aes(x=date, ymin=fit-se.fit*qnorm(.975), ymax=fit+se.fit*qnorm(.975), fill='Cog Employment Ratio'), alpha=.4) +
  geom_line(data=states.ER.preds.DT, aes(x=date, y=fit, color='Cog Employment Ratio')) +
  geom_point(size=4, aes(x=date, y=employmentratio.estimate, color='Cog Employment Ratio', shape='Cog Employment Ratio')) +
  geom_linerange(aes(x=date, ymin=employmentratio.estimate - employmentratio.moe/qnorm(.95)*qnorm(.975), ymax=employmentratio.estimate + employmentratio.moe/qnorm(.95)*qnorm(.975), color='Cog Employment Ratio') ) +
  geom_point(size=4, data=whd.percapita.crp.workers.by.state.incl.2024.DT[geography.abb != 'GU'], aes(x=list.date, y=1e5*workers.paid.submin.per.capita*.9/300, color='Workers Paid Subminimum Wage per 100k Pop', shape='Workers Paid Subminimum Wage per 100k Pop')) +
  geom_ribbon(data=whd.percapita.crp.workers.by.state.incl.2024.preds.DT, aes(x=date, ymin=1e5*(fit-se.fit*qnorm(.975))*.9/300, ymax=1e5*(fit+se.fit*qnorm(.975))*.9/300, fill='Workers Paid Subminimum Wage per 100k Pop'), alpha=.4) +
  geom_line(data=whd.percapita.crp.workers.by.state.incl.2024.preds.DT, aes(x=date, y=1e5*fit*.9/300, color='Workers Paid Subminimum Wage per 100k Pop')) +
  scale_x_date(breaks=as.Date(paste0(c(2015, 2025), '-01-01')), date_labels='%Y\n%b', expand=expand_scale(mult=.1), minor_breaks=as.Date(paste0(c(2010, 2015, 2020, 2025), '-01-01'))) +
  scale_y_continuous(
    breaks=seq(0, .9, by=.3), minor_breaks=seq(0, .9, by=.1), name='◆ Employment Ratio for People with Cognitive Disabilities',
    sec.axis=sec_axis(~.*300/.9, guide=guide_axis(title='○ Workers Paid Subminimum Wage per 100k Population'))
  ) +
  scale_shape_manual(values=c('◆', '○')) +
  scale_color_manual(values=okabe[1:2]) +
  scale_fill_manual(values=okabe[1:2]) +
  theme_gray(base_size=14) +
  theme(
    legend.position='none',
    axis.text.y.left=element_text(color=okabe[1]),
    axis.text.y.right=element_text(color=okabe[2]),
    axis.ticks.y.left=element_line(color=okabe[1]),
    axis.ticks.y.right=element_line(color=okabe[2]),
    axis.title.y.left=element_text(color=okabe[1]),
    axis.title.y.right=element_text(color=okabe[2]),
    axis.title.x=element_blank()
  )
ggsave(fig.2.ER.and.workers.by.state, filename='fig-2-ER-and-workers-by-state.png', width=10, height=14)

whd.percapita.crp.workers.by.state.incl.2024.preds.DT[geography.abb=='MO'][ , .(geography.abbf, year.float, 1e5*fit, date)]
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[geography.abb=='AR'][ , .(geography.abbf, year.float, 1e5*fit, date)]
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[geography.abb=='KS'][ , .(geography.abbf, year.float, 1e5*fit, date)]
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[geography.abb=='OH'][ , .(geography.abbf, year.float, 1e5*fit, date)]
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[geography.abb=='PA'][ , .(geography.abbf, year.float, 1e5*fit, date)]
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[geography.abb=='IN'][ , .(geography.abbf, year.float, 1e5*fit, date)]

whd.percapita.crp.workers.by.state.incl.2024.DT[geography.abb=='MO'][ , .(geography.abb, year.float, 1e5*workers.paid.submin.per.capita, list.date, pop*workers.paid.submin.per.capita)]
whd.percapita.crp.workers.by.state.incl.2024.DT[geography.abb=='AR'][ , .(geography.abb, year.float, 1e5*workers.paid.submin.per.capita, list.date, pop*workers.paid.submin.per.capita)]
whd.percapita.crp.workers.by.state.incl.2024.DT[geography.abb=='KS'][ , .(geography.abb, year.float, 1e5*workers.paid.submin.per.capita, list.date, pop*workers.paid.submin.per.capita)]
whd.percapita.crp.workers.by.state.incl.2024.DT[geography.abb=='OH'][ , .(geography.abb, year.float, 1e5*workers.paid.submin.per.capita, list.date, pop*workers.paid.submin.per.capita)]
whd.percapita.crp.workers.by.state.incl.2024.DT[geography.abb=='PA'][ , .(geography.abb, year.float, 1e5*workers.paid.submin.per.capita, list.date, pop*workers.paid.submin.per.capita)]

#####################################
# Figure 3 (scatterplot matrix)


states.cog.employmentratio.lm.DT <- states.employment.analysis.dataset.wide.DT[cognitive==1 & 2015 <= year & year <= 2023][
  , {LM <- lm(employmentratio.estimate ~ I(year-2019), data=.SD)
    PRED <- predict(LM, newdata=data.table(year=2019), se.fit=TRUE)
    list(
      cog.employmentratio.slope.estimate=coef(LM)['I(year - 2019)'],
      cog.employmentratio.slope.se=summary(LM)$coefficients['I(year - 2019)', 2],
      cog.employmentratio.mid.estimate=coef(LM)['(Intercept)'],
      cog.employmentraito.mid.se=summary(LM)$coefficients['(Intercept)', 2]
    )
  },
  by=NAME
]
states.cog.employmentratio.lm.DT <- states.dc.pr.DT[states.cog.employmentratio.lm.DT, on=c(geography.name='NAME')]
whd.crp.workers.per.capita.by.state.lm.DT <- whd.crp.workers.by.state.DT[geography.abb != 'GU' & 2015 <= year & year <= 2023][ # Guam not in pop estimates
  , {LM <- lm(workers.paid.submin.per.capita ~ I(year-2019), weights=1/list.dates.per.year, data=.SD) 
    PRED <- predict(LM, newdata=data.table(year=2019), se.fit=TRUE)
    list(
      crp.workers.per.capita.slope.estimate=coef(LM)['I(year - 2019)'],
      crp.workers.per.capita.se=summary(LM)$coefficients['I(year - 2019)', 2],
      crp.workers.per.capita.mid.estimate=coef(LM)['(Intercept)'],
      crp.workers.per.capita.mid.se=summary(LM)$coefficients['(Intercept)', 2]
    )
  },
  by=geography.abb
]

states.cor.analysis.DT <- states.cog.employmentratio.lm.DT[whd.crp.workers.per.capita.by.state.lm.DT, on='geography.abb']
fig.3.scattermat <- ggpairs(states.cor.analysis.DT[, .(geography.abb,
  `CRP Subminimum Wage\nWorkers per 100k pop\n(Intercept)`=1e5*crp.workers.per.capita.mid.estimate, `CRP Subminimum Wage\nWorkers per 100k pop\n(slope: change per year)`=1e5*crp.workers.per.capita.slope.estimate,
  `Cognitive Disability\nEmployment Ratio\n(Intercept)`=cog.employmentratio.mid.estimate, `Cognitive Disability\nEmployment Ratio\n (slope: change per year)`=cog.employmentratio.slope.estimate
  )],
  columns=2:5,
  lower = list(continuous=\(data, mapping, ...) ggally_points(data, mapping, ..., color=NA) + geom_text(aes(label=geography.abb), size=3)),
  upper = list(continuous = wrap(ggally_cor, alignPercent = 0.8, digits=2))
) + theme_gray(base_size=12)
for(r in 1:fig.3.scattermat$nrow)
  for(c in 1:fig.3.scattermat$ncol)
    fig.3.scattermat[r,c] <- fig.3.scattermat[r,c] + scale_x_continuous(expand=expand_scale(mult=.1)) + scale_y_continuous(expand=expand_scale(mult=.1))
ggsave(fig.3.scattermat, width=8, height=8, filename='fig-3-scattermat.png')
