ggplot(
  states.employment.analysis.dataset.wide.DT[cognitive==1],
  aes(
    x=year,
    y=employmentratio.estimate,
    ymin=employmentratio.estimate - employmentratio.moe/qnorm(.95)*qnorm(.975),
    ymax=employmentratio.estimate + employmentratio.moe/qnorm(.95)*qnorm(.975)
  )
) +
  geom_smooth() +
  geom_point() +
  geom_linerange() +
  facet_wrap(~NAME)

states.employment.analysis.dataset.wide.DT[ , NAMEf := factor(NAME)]
states.employment.analysis.dataset.wide.DT[
  , w.by.disab.type := 1/(employmentratio.moe^2) / sum(1/(employmentratio.moe^2))
  , by=cognitive
]
fit.ER.states.0 <-
  bam(
    employmentratio.estimate ~ s(year, NAMEf, bs='fs'),
    data=states.employment.analysis.dataset.wide.DT[cognitive==1]
  )
fit.ER.states.1 <-
  bam(
    employmentratio.estimate ~ s(year, NAMEf, bs='fs'),
    weights=w.by.disab.type,
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
    workers.paid.submin.per.capita ~ s(year.float, geography.abbf, bs='fs'),
    data=whd.percapita.crp.workers.by.state.incl.2024.DT[geography.abb != 'GU']
  )
whd.percapita.crp.workers.by.state.incl.2024.preds.DT <- CJ(geography.abbf=unique(whd.crp.workers.by.state.DT[geography.abb != 'GU']$geography.abbf), year.float=seq(2015.25, 2023.25, length=100))
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[ , c('fit', 'se.fit') := as.data.table(predict(fit.workers.per.capita.0, newdata=whd.percapita.crp.workers.by.state.incl.2024.preds.DT, se.fit=TRUE))]
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[ , date := as.Date('2010-07-01') + (year.float-2010)*365.25]
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[ , geography.abb := geography.abbf]



ggplot(
  states.dc.pr.DT[states.employment.analysis.dataset.wide.DT[cognitive==1], on=c(geography.name='NAME')],
) +
  facet_wrap(~geography.abb) +
  geom_ribbon(data=states.ER.preds.DT, aes(x=date, ymin=fit-se.fit*qnorm(.975), ymax=fit+se.fit*qnorm(.975), fill='Cog Employment Ratio'), alpha=.4) +
  geom_line(data=states.ER.preds.DT, aes(x=date, y=fit, color='Cog Employment Ratio')) +
  geom_point(size=4, aes(x=date, y=employmentratio.estimate, color='Cog Employment Ratio', shape='Cog Employment Ratio')) +
  geom_linerange(aes(x=date, ymin=employmentratio.estimate - employmentratio.moe/qnorm(.95)*qnorm(.975), ymax=employmentratio.estimate + employmentratio.moe/qnorm(.95)*qnorm(.975), color='Cog Employment Ratio') ) +
  geom_point(size=4, data=whd.percapita.crp.workers.by.state.incl.2024.DT[geography.abb != 'GU'], aes(x=list.date, y=1e5*workers.paid.submin.per.capita*.9/300, color='Workers Paid Subminimum Wage per 100k Pop', shape='Workers Paid Subminimum Wage per 100k Pop')) +
  # geom_smooth(formula=y~x, data=whd.crp.workers.by.state.DT[geography.abb != 'GU'], aes(x=list.date, y=1e5*workers.paid.submin.per.capita*.9/300, color='Workers Paid Subminimum Wage per 100k Pop', fill='Workers Paid Subminimum Wage per 100k Pop')) +
  geom_ribbon(data=whd.percapita.crp.workers.by.state.incl.2024.preds.DT, aes(x=date, ymin=1e5*(fit-se.fit*qnorm(.975))*.9/300, ymax=1e5*(fit+se.fit*qnorm(.975))*.9/300, fill='Workers Paid Subminimum Wage per 100k Pop'), alpha=.4) +
  geom_line(data=whd.percapita.crp.workers.by.state.incl.2024.preds.DT, aes(x=date, y=1e5*fit*.9/300, color='Workers Paid Subminimum Wage per 100k Pop')) +
  # scale_x_continuous(breaks=seq(2010, 2025, by=5), minor_breaks=2010:2025, guide=guide_axis(n.dodge=2), expand=expand_scale(mult=.1)) +
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
ggsave(filename='fig-2-ER-and-workers-by-state.png', width=10, height=14)

whd.percapita.crp.workers.by.state.incl.2024.preds.DT[geography.abb=='MO']
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[geography.abb=='AR']
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[geography.abb=='KS']
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[geography.abb=='OH']
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[geography.abb=='PA']
whd.percapita.crp.workers.by.state.incl.2024.preds.DT[geography.abb=='IN']

ggplot(
  states.dc.pr.DT[states.employment.analysis.dataset.wide.DT[cognitive==1], on=c(geography.name='NAME')],
) +
  facet_wrap(~geography.abb) +
  # geom_ribbon(data=states.ER.preds.DT, aes(x=date, ymin=fit-se.fit*qnorm(.975), ymax=fit+se.fit*qnorm(.975), fill='Cog Employment Ratio'), alpha=.4) +
  # geom_line(data=states.ER.preds.DT, aes(x=date, y=fit, color='Cog Employment Ratio')) +
  # geom_point(size=4, aes(x=date, y=employmentratio.estimate, color='Cog Employment Ratio', shape='Cog Employment Ratio')) +
  # geom_linerange(aes(x=date, ymin=employmentratio.estimate - employmentratio.moe/qnorm(.95)*qnorm(.975), ymax=employmentratio.estimate + employmentratio.moe/qnorm(.95)*qnorm(.975), color='Cog Employment Ratio') ) +
  geom_point(size=4, data=whd.crp.workers.by.state.DT[geography.abb != 'GU'], aes(x=list.date, y=pop*1e5*workers.paid.submin.per.capita*.9/300, color='Workers Paid Subminimum Wage per 100k Pop', shape='Workers Paid Subminimum Wage per 100k Pop')) +
  # geom_smooth(formula=y~x, data=whd.crp.workers.by.state.DT[geography.abb != 'GU'], aes(x=list.date, y=1e5*workers.paid.submin.per.capita*.9/300, color='Workers Paid Subminimum Wage per 100k Pop', fill='Workers Paid Subminimum Wage per 100k Pop')) +
  geom_ribbon(data= c, aes(x=date, ymin=1e5*(fit-se.fit*qnorm(.975))*.9/300, ymax=1e5*(fit+se.fit*qnorm(.975))*.9/300, fill='Workers Paid Subminimum Wage per 100k Pop'), alpha=.4) +
  geom_line(data=states.workers.per.capita.preds.DT, aes(x=date, y=1e5*fit*.9/300, color='Workers Paid Subminimum Wage per 100k Pop')) +
  # scale_x_continuous(breaks=seq(2010, 2025, by=5), minor_breaks=2010:2025, guide=guide_axis(n.dodge=2), expand=expand_scale(mult=.1)) +
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


whd.crp.workers.by.state.DT[geography.abb=='MO', .(list.date, workers.paid.submin, workers.paid.submin.per.capita)]
whd.crp.workers.by.state.DT[geography.abb=='AR', .(list.date, workers.paid.submin, workers.paid.submin.per.capita)]
whd.crp.workers.by.state.DT[geography.abb=='KS', .(list.date, workers.paid.submin, workers.paid.submin.per.capita)]
whd.crp.workers.by.state.DT[geography.abb=='IN', .(list.date, workers.paid.submin, workers.paid.submin.per.capita)]
whd.crp.workers.by.state.DT[geography.abb=='OH', .(list.date, workers.paid.submin, workers.paid.submin.per.capita)]
whd.crp.workers.by.state.DT[geography.abb=='PA', .(list.date, workers.paid.submin, workers.paid.submin.per.capita)]
whd.crp.workers.by.state.DT[geography.abb=='UT', .(list.date, workers.paid.submin, workers.paid.submin.per.capita)]
whd.crp.workers.by.state.DT[geography.abb=='MN', .(list.date, workers.paid.submin, workers.paid.submin.per.capita)]

states.workers.per.capita.preds.DT[geography.abbf=='MO', .(1e5*fit), by=.(year.float)][date %in% c()]

whd.pretableau.crp.DT[State=='MN'][list.date=='2015-10-01', .(
  sum(as.numeric(`Number of Workers Paid Subminimum Wages`), na.rm=TRUE),
  .N
)]
whd.pretableau.crp.DT[State=='MN'][list.date=='2016-01-01', .(
  sum(as.numeric(`Number of Workers Paid Subminimum Wages`), na.rm=TRUE),
  .N
)]

pivot_wider(
  id_cols=c('GEOID', 'NAME', 'year', 'rest.of.variable'),
  names_from=variable,
  names_sep = '.',
  values_from=c(estimate, moe, prop.estimate, prop.moe, denom.estimate, denom.moe, neff.denom)
)

ggplot(
  (pivot_wider(
    whd.pretableau.crp.DT[State=='MN'][list.date <= '2016-01-01'][ , .(list.date, cert.num=`Cert Num`, cert.status=`Cert Status`, workers=`Number of Workers Paid Subminimum Wages`)],
    id_cols="cert.num",
    names_from=list.date,
    values_from=c(workers, cert.status)
  ) %>% data.table),
  aes(x=`workers_2015-10-01`, y=`workers_2016-01-01`)
) + geom_point() + geom_abline(intercept=0, slope=1)
(pivot_wider(
  whd.pretableau.crp.DT[State=='MN'][list.date <= '2016-01-01'][ , .(list.date, cert.num=`Cert Num`, cert.status=`Cert Status`, workers=`Number of Workers Paid Subminimum Wages`)],
  id_cols="cert.num",
  names_from=list.date,
  values_from=c(workers, cert.status)
) %>% data.table)[is.na(`workers_2015-10-01`) & !is.na(`workers_2016-01-01`)]

whd.pretableau.crp.DT[`Cert Num`=='05-03105-S-033']


ggplot(
  whd.crp.workers.by.state.DT,
  aes(
    x=list.date,
    y=workers.paid.submin/pop*1e5 #,
    # ymin=employmentratio.estimate - employmentratio.moe/qnorm(.95)*qnorm(.975),
    # ymax=employmentratio.estimate + employmentratio.moe/qnorm(.95)*qnorm(.975)
  )
) +
  facet_wrap(~geography.abb) +
  # geom_line(data=states.ER.preds.DT, aes(y=fit, ymin=NULL, ymax=NULL)) +
  # geom_ribbon(data=states.ER.preds.DT, aes(y=NULL, ymin=fit-se.fit*qnorm(.975), ymax=fit+se.fit*qnorm(.975)), alpha=.4) +
  geom_point() +
  # geom_linerange() +
  scale_x_continuous(breaks=seq(2010, 2025, by=5), minor_breaks=2010:2025, guide=guide_axis(n.dodge=2), expand=expand_scale(mult=.1)) +
  scale_y_continuous(breaks=seq(0, .8, by=.2), minor_breaks=seq(0, .8, by=.1)) +
  theme_gray(base_size=14)


whd.pretableau.crp.DT[`Cert Status`=='Issued', .(mx=max(`Number of Workers Paid Subminimum Wages`, na.rm=TRUE), mn=min(`Number of Workers Paid Subminimum Wages`, na.rm=TRUE)), by='Cert Num'][mx!=mn]
# 7 non-matches, with 3 obvious non-matches: 05-03943-S-027 , 05-03105-S-033 , 05-11782-S-073
whd.pretableau.crp.DT[`Cert Num`=='05-03943-S-027'] # the 2075 on 2015-10-01 and 2016-01-01 look wrong
whd.pretableau.crp.DT[`Cert Num`=='05-03105-S-033'] # the 13674 on 2016-01-01 should be 136
whd.pretableau.crp.DT[`Cert Num`=='05-11782-S-073'] # the 2431 on 2016-01-01 should be 31

# likely error on two dates:
whd.pretableau.crp.DT[`Cert Num`=='05-03943-S-027' & list.date=='2015-10-01' & `Number of Workers Paid Subminimum Wages`==2075, `Number of Workers Paid Subminimum Wages` := 107]
whd.pretableau.crp.DT[`Cert Num`=='05-03943-S-027' & list.date=='2016-01-01' & `Number of Workers Paid Subminimum Wages`==2075, `Number of Workers Paid Subminimum Wages` := 107]
# definitely error on one date:
whd.pretableau.crp.DT[`Cert Num`=='05-03105-S-033' & list.date=='2015-10-01' & `Number of Workers Paid Subminimum Wages`==13674, `Number of Workers Paid Subminimum Wages` := 136]
# likely error on one date:
whd.pretableau.crp.DT[`Cert Num`=='05-11782-S-073' & list.date=='2016-01-01' & `Number of Workers Paid Subminimum Wages`==2431, `Number of Workers Paid Subminimum Wages` := 31]

# PLUS FOURTH NON-MATCH!
# '04-06882-S-029' HAS MAX OF 57 AND MIN OF 0
whd.pretableau.crp.DT[ , which(`Cert Num`=='04-06882-S-029')] # row 3259, 5610, 8604, 10559
whd.pretableau.crp.DT[ 3259 + (-5:5) ] # c(27, 0, 21)
whd.pretableau.crp.DT[ 5610 + (-5:5) ] # c(27, 0, 21)
whd.pretableau.crp.DT[ 8604 + (-5:5) ] # c(34, 57, 52)
whd.pretableau.crp.DT[ 10559 + (-5:5) ] # c(21, 57, NA)

whd.pretableau.crp.DT[ , sort(`Number of Workers Paid Subminimum Wages`, decr=TRUE)]
print(whd.pretableau.crp.DT[`Cert Status`=='Issued' , max(`Number of Workers Paid Subminimum Wages`, na.rm=TRUE), by=`Cert Num`][order(-V1)], topn=10)


whd.pretableau.crp.DT[ , which(`Cert Num`=='05-03943-S-027')] # row 1212, 3615, 5939
whd.pretableau.crp.DT[ 1212 + (-5:5) ] # c(270, 2075, 106)
whd.pretableau.crp.DT[ 3615 + (-5:5) ] # c(270, 2075, 106)
whd.pretableau.crp.DT[ 5939 + (-5:5) ] # c(312, 107, 106)

whd.pretableau.crp.DT[ , which(`Cert Num`=='05-03105-S-033')] # row 3518, 5850, 7121, 10327
whd.pretableau.crp.DT[ 3518 + (-5:5) ] # c(421, 13674, 21)
whd.pretableau.crp.DT[ 5850 + (-5:5) ] # c(251, 136, 21)
whd.pretableau.crp.DT[ 7121 + (-5:5) ] # c(6, 136, 25)
whd.pretableau.crp.DT[ 10327 + (-5:5) ] # c(21, 136, 251)

whd.pretableau.crp.DT[ , which(`Cert Num`=='05-11782-S-073')] # row 3858, 6171, 8159, 10011
whd.pretableau.crp.DT[ 3858 + (-5:5) ] # c(62, 2431, NA)
whd.pretableau.crp.DT[ 6171 + (-5:5) ] # c(60, 31, 26)
whd.pretableau.crp.DT[ 8159 + (-5:5) ] # c(114, 31, 16)
whd.pretableau.crp.DT[ 10011 + (-5:5) ] # c(26, 31, 60)


whd.pretableau.both.DT[`Cert Status`=='Issued', .(mx=max(`Number of Workers Paid Subminimum Wages`, na.rm=TRUE), mn=min(`Number of Workers Paid Subminimum Wages`, na.rm=TRUE)), by='Employer'][mx!=mn]
whd.pretableau.both.DT

whd.tableau.DT[`Status`=='Issued', .(mx=max(`Workers Paid Subminimum Wages`, na.rm=TRUE), mn=min(`Workers Paid Subminimum Wages`, na.rm=TRUE)), by='Employer'][mx!=mn]

