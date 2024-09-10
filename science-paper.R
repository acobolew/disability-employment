states.dc.pr.DT <- data.table(
  geography.abb=c(state.abb, 'DC', 'PR'),
  geography.name=c(state.name, 'District of Columbia', 'Puerto Rico')
)

acs1.loaded.variables.DT <- data.table( load_variables(2022, dataset='acs1') )
acs1.target.variables.DT <- acs1.loaded.variables.DT[toupper(concept)=='EMPLOYMENT STATUS BY DISABILITY STATUS AND TYPE']

us.employment.estimates.and.moe.DT <- map_dfr(
  years,
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
names(okabemod) <- c('Cognitive', 'Hearing', 'Vision', 'No Disability', 'Ambulatory', 'Independent Living', 'Self-care')
okabemodfills <- okabemod
okabemodfills[c('No Disability', 'Vision', 'Ambulatory', 'Self-care')] <- 'white'
dis.type.shapes <- c('No Disability'=21, Hearing=24, Vision=22, Cognitive=25, Ambulatory=24, 'Independent Living'=22, 'Self-care'=25)

us.employment.fig <- ggplot(us.employment.analysis.dataset.wide.DT, aes(x=as.Date(paste0(year, '-07-01')))) +
  scale_shape_manual(values=dis.type.shapes) +
  scale_fill_manual(values=okabemodfills) +
  scale_color_manual(values=okabemod) +
  scale_x_date(limits=as.Date(c('2010-01-01', '2030-01-01')), breaks=seq(as.Date('2010-01-01'), as.Date('2025-01-01'), by='5 year'), minor_breaks=NULL) +
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
  scale_x_date(limits=as.Date(c('2010-01-01', '2030-01-01')), breaks=seq(as.Date('2010-01-01'), as.Date('2025-01-01'), by='5 year'), minor_breaks=NULL) +
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
employment.fig1 <- egg::ggarrange(
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
ggsave(width=4, height=8, filename='employment-fig1.png', plot=employment.fig1)

#############

states.employment.estimates.and.moe.DT <- map_dfr(
  years,
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

ggplot(states.employment.analysis.dataset.wide.DT[cognitive==1], aes(x=as.Date(paste0(year, '-07-01')))) +
  facet_wrap(~NAME) +
  scale_shape_manual(values=dis.type.shapes) +
  scale_fill_manual(values=okabemodfills) +
  scale_color_manual(values=okabemod) +
  scale_x_date(limits=as.Date(c('2010-01-01', '2030-01-01')), breaks=seq(as.Date('2010-01-01'), as.Date('2025-01-01'), by='5 year'), minor_breaks=NULL) +
  # scale_x_continuous(limits=c(2010,2030), breaks=seq(2010, 2025, by=5), minor_breaks=NULL) +
  # expand_limits(y=0) +
  geom_line(aes(y=employmentratio.estimate, color=disability.status), show.legend=FALSE) +
  geom_linerange(aes(color=disability.status, ymin=employmentratio.estimate-employmentratio.moe/qnorm(.95)*qnorm(.975), ymax=employmentratio.estimate+employmentratio.moe/qnorm(.95)*qnorm(.975)), show.legend=FALSE) +
  geom_point(aes(y=employmentratio.estimate, color=disability.status, fill=disability.status, shape=disability.status), show.legend=FALSE) +
  ylab('Employment Ratio\nre People with\nNo Disability') +
  theme_grey(base_size=12)

states.cog.employmentratio.lm.DT <- states.employment.analysis.dataset.wide.DT[cognitive==1 & 2015 <= year & year <= 2022][
  , {LM <- lm(employmentratio.estimate ~ year, data=.SD)
    PRED <- predict(LM, newdata=data.table(year=2019), se.fit=TRUE)
    list(
      cog.employmentratio.slope.estimate=coef(LM)['year'],
      cog.employmentratio.slope.se=summary(LM)$coefficients['year', 2],
      cog.employmentratio.mid.estimate=PRED$fit[1],
      cog.employmentraito.mid.se=PRED$se.fit[1]
    )
  },
  by=NAME
]
states.cog.employmentratio.lm.DT <- states.dc.pr.DT[states.cog.employmentratio.lm.DT, on=c(geography.name='NAME')]
whd.crp.workers.per.capita.by.state.lm.DT <- whd.crp.workers.by.state.DT[geography.abb != 'GU' & 2015 <= year & year <= 2022][ # Guam not in pop estimates
  , {LM <- lm(workers.paid.submin.per.capita ~ year, weights=1/list.dates.per.year, data=.SD) 
    PRED <- predict(LM, newdata=data.table(year=2019), se.fit=TRUE)
    list(
      crp.workers.per.capita.slope.estimate=coef(LM)['year'],
      crp.workers.per.capita.se=summary(LM)$coefficients['year', 2],
      crp.workers.per.capita.mid.estimate=PRED$fit[1],
      crp.workers.per.capita.mid.se=PRED$se.fit
    )
  },
  by=geography.abb
]

states.cor.analysis.DT <- states.cog.employmentratio.lm.DT[whd.crp.workers.per.capita.by.state.lm.DT, on='geography.abb']
scatter.fig2 <- ggpairs(states.cor.analysis.DT[, .(geography.abb,
  `CRP Subminimum Wage\nWorkers per 100k pop\n(2019 fit)`=1e5*crp.workers.per.capita.mid.estimate, `CRP Subminimum Wage\nWorkers per 100k pop\n(slope per year)`=1e5*crp.workers.per.capita.slope.estimate,
  `Cognitive Disability\nEmployment Ratio\n(2019 fit)`=cog.employmentratio.mid.estimate, `Cognitive Disability\nEmployment Ratio\n (slope per year)`=cog.employmentratio.slope.estimate
  )],
  columns=2:5,
  lower = list(continuous=\(data, mapping, ...) ggally_points(data, mapping, ..., color=NA) + geom_text(aes(label=geography.abb), size=3)),
  upper = list(continuous = wrap(ggally_cor, alignPercent = 0.8, digits=2))
) + theme_gray(base_size=12)
ggsave(scatter.fig2, width=8, height=8, filename='scatter-fig2.png')
