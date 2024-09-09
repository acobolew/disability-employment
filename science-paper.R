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

]

fit.us.employment <- bam(
  prop.estimate.employed ~
    s(year) +
    s(year, by=hearing) + s(year, by=vision) + s(year, by=cognitive) + s(year, by=ambulatory) + s(year, by=selfcare) + s(year, by=independentliving),
  weights=neff.denom.employed,
  family=quasibinomial,
  discrete=TRUE,
  data=us.employment.analysis.dataset.wide.DT
)

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

workers.paid.submin.DT[ , .(workers.paid.submin=sum(workers.paid.submin)), by=list.date] # clean up from whd.R

ggplot(us.employment.analysis.dataset.wide.DT, aes(x=year)) +
  geom_line(aes(y=prop.estimate.employed, color=disability.status), show.legend=FALSE) +
  geom_linerange(aes(color=disability.status, ymin=prop.estimate.employed-prop.moe.employed/qnorm(.95)*qnorm(.975), ymax=prop.estimate.employed+prop.moe.employed/qnorm(.95)*qnorm(.975)), show.legend=FALSE) +
  geom_point(aes(y=prop.estimate.employed, color=disability.status, shape=disability.status), show.legend=FALSE) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 6)) +
  scale_x_continuous(limits=c(2010,2030), breaks=seq(2010, 2024, by=5), minor_breaks=NULL) +
  expand_limits(y=0) +
  geom_text_repel(
    data=us.employment.analysis.dataset.wide.DT[year==max(year)],
    aes(label=disability.status, color=disability.status, y=prop.estimate.employed),
    direction='y',
    nudge_x=.5,
    hjust=0,
    segment.color=NA,
    show.legend=FALSE
  ) +
  theme_minimal()

okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
okabemod <- c("#E69F00", "#56B4E9", "#009E73", "#000000", "#0072B2", "#D55E00", "#CC79A7")
names(okabemod) <- c('Cognitive', 'Hearing', 'Vision', 'No Disability', 'Ambulatory', 'Independent Living', 'Self-care')
okabemodfills <- okabemod
okabemodfills[c('No Disability', 'Vision', 'Ambulatory', 'Self-care')] <- 'white'
# withr::with_options(
#   list(ggplot2.discrete.fill = okabe),
#   print(cty_by_var(class))
# )

us.employment.fig <- ggplot(us.employment.analysis.dataset.wide.DT, aes(x=as.Date(paste0(year, '-07-01')))) +
  scale_shape_manual(values=c(21, 24, 22, 25, 24, 22, 25)) +
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
  scale_shape_manual(values=c(21, 24, 22, 25, 24, 22, 25)) +
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

whd.crp.DT[,sum(workers.paid.submin, na.rm=TRUE), by=list.date][order(list.date)]
workers.paid.dates.DT <- data.table(
  list.date=as.Date(c('2015-10-01', '2016-07-01', '2018-01-01', '2018-07-01', '2019-07-01', '2020-07-01', '2021-07-01', '2022-07-01', '2023-07-01', '2024-07-01')),
  year=2015:2024
)

whd.crp.num.by.state.DT[ , .(crp.programs=sum(N)), by=list.date]
crp.programs.dates.DT <- data.table(
  list.date=as.Date(c('2010-01-05', '2011-09-07', '2012-')),
  year=2011, 2012
)

us.workers.fig <- ggplot(
    # whd.crp.DT[ , .(workers.paid.submin=sum(workers.paid.submin, na.rm=TRUE)), by=list.date][
    #   workers.paid.dates.DT, on='list.date'
    # ],
   # aes(x=year, y=workers.paid.submin)
    whd.crp.DT[ , .(workers.paid.submin=sum(workers.paid.submin, na.rm=TRUE)), by=list.date],
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
  whd.crp.num.by.state.DT[ , .(crp.programs=sum(N)), by=list.date],
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

ggplot(us.employment.analysis.dataset.wide.DT[ , cbind(panel='% Employed', .SD)], aes(x=year)) +
  facet_wrap(~factor(panel, levels=c('% Employed', 'Employment Ratio re People with No Disability')), ncol=1, scales='free_y') +
  facetted_pos_scales(y=list(
    scale_y_continuous(labels=label_percent()),
    NULL
  )) +
  # Employment
  geom_line(aes(y=prop.estimate.employed, color=disability.status), show.legend=FALSE) +
  geom_linerange(aes(color=disability.status, ymin=prop.estimate.employed-prop.moe.employed/qnorm(.95)*qnorm(.975), ymax=prop.estimate.employed+prop.moe.employed/qnorm(.95)*qnorm(.975)), show.legend=FALSE) +
  geom_point(aes(y=prop.estimate.employed, color=disability.status, shape=disability.status), show.legend=FALSE) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 6)) +
  scale_x_continuous(limits=c(2010,2030), breaks=seq(2010, 2024, by=5), minor_breaks=NULL) +
  # expand_limits(y=0) +
  geom_text_repel(
    data=us.employment.analysis.dataset.wide.DT[ , cbind(panel='% Employed', .SD)][year==max(year)],
    aes(label=disability.status, color=disability.status, y=prop.estimate.employed),
    direction='y',
    nudge_x=.5,
    hjust=0,
    segment.color=NA,
    show.legend=FALSE
  ) +
  # B
  geom_line(
    data=us.employment.analysis.dataset.wide.DT[ , cbind(panel='Employment Ratio re People with No Disability', .SD)][no.disability==0],
    aes(y=employmentratio.estimate, color=disability.status), show.legend=FALSE
  ) +
  geom_linerange(
    data=us.employment.analysis.dataset.wide.DT[ , cbind(panel='Employment Ratio re People with No Disability', .SD)][no.disability==0],
    aes(color=disability.status, ymin=employmentratio.estimate-employmentratio.moe/qnorm(.95)*qnorm(.975), ymax=employmentratio.estimate+employmentratio.moe/qnorm(.95)*qnorm(.975)), show.legend=FALSE
  ) +
  geom_point(
    data=us.employment.analysis.dataset.wide.DT[ , cbind(panel='Employment Ratio re People with No Disability', .SD)][no.disability==0],
    aes(y=employmentratio.estimate, color=disability.status, shape=disability.status), show.legend=FALSE
  ) +
  geom_text_repel(
    data=us.employment.analysis.dataset.wide.DT[ , cbind(panel='Employment Ratio re People with No Disability', .SD)][no.disability==0][year==max(year)],
    aes(label=disability.status, color=disability.status, y=employmentratio.estimate),
    direction='y',
    nudge_x=.5,
    hjust=0,
    segment.color=NA,
    show.legend=FALSE
  ) +
  theme_minimal()


aug.analysis.dataset.wide.us.DT <- analysis.dataset.wide.us.2.DT
aug.analysis.dataset.wide.us.DT[ , c('fit.us.2.w.2.link', 'fit.us.2.w.2.se') := as.data.table(predict(fit.us.2.w.2, se.fit=TRUE)) ]
aug.analysis.dataset.wide.us.DT[ , `:=`(fit.us.2.w.2.link.lo=fit.us.2.w.2.link-qnorm(.975)*fit.us.2.w.2.se, fit.us.2.w.2.link.hi=fit.us.2.w.2.link+qnorm(.975)*fit.us.2.w.2.se)]
ggplot(
  aug.analysis.dataset.wide.us.DT,
  aes(x=year)
) +
  geom_ribbon(aes(ymin=fit.us.2.w.2.link.lo, ymax=fit.us.2.w.2.link.hi, fill=ifelse(no.disability==1, 'no.disability', ifelse(hearing==1, 'hearing', ifelse(vision==1, 'vision', ifelse(cognitive==1, 'cognitive', ifelse(ambulatory==1, 'ambulatory', ifelse(selfcare==1, 'selfcare', ifelse(independentliving==1, 'independentliving', NA_character_)))))))), alpha=.2) +
  geom_line(aes(y=fit.us.2.w.2.link, color=ifelse(no.disability==1, 'no.disability', ifelse(hearing==1, 'hearing', ifelse(vision==1, 'vision', ifelse(cognitive==1, 'cognitive', ifelse(ambulatory==1, 'ambulatory', ifelse(selfcare==1, 'selfcare', ifelse(independentliving==1, 'independentliving', NA_character_))))))))) +
  geom_linerange(aes(
    ymin=log((prop.estimate.employed-prop.moe.employed)/(1-(prop.estimate.employed-prop.moe.employed))),
    ymax=log((prop.estimate.employed+prop.moe.employed)/(1-(prop.estimate.employed+prop.moe.employed))),
    color=ifelse(no.disability==1, 'no.disability', ifelse(hearing==1, 'hearing', ifelse(vision==1, 'vision', ifelse(cognitive==1, 'cognitive', ifelse(ambulatory==1, 'ambulatory', ifelse(selfcare==1, 'selfcare', ifelse(independentliving==1, 'independentliving', NA_character_)))))))
  )) +
  geom_point(size=1, aes(y=log(p.employed/(1-p.employed)), color=ifelse(no.disability==1, 'no.disability', ifelse(hearing==1, 'hearing', ifelse(vision==1, 'vision', ifelse(cognitive==1, 'cognitive', ifelse(ambulatory==1, 'ambulatory', ifelse(selfcare==1, 'selfcare', ifelse(independentliving==1, 'independentliving', NA_character_))))))))) +
  facet_wrap(vars(NAMEf)) +
  scale_x_continuous(breaks=seq(2010, 2022, by=2), minor_breaks=2010:2022)

aug.analysis.dataset.wide.us.DT[year %in% c(2010, 2022)][(cognitive==1 | no.disability==1)
  , .(year, prop.estimate.employed, estimate.employed, denom.estimate.employed)
]


