estimates.and.moe.us.DT <- map_dfr(
  years,
  ~get_acs(geography='us', variables=target.variables.DT[ , name], survey='acs1', year=.x),
  .id='year'
) %>% data.table

linked.data.us.DT <- target.variables.DT[estimates.and.moe.us.DT, on=c(name='variable')]
analysis.dataset.us.DT <- linked.data.us.DT[grepl('With a disability:!!|No disability', label)]

analysis.dataset.us.DT[, employed := as.numeric(grepl('Employed', label))]
analysis.dataset.us.DT[, unemployed := as.numeric(grepl('Unemployed', label))]
analysis.dataset.us.DT[, notinlaborforce := as.numeric(grepl('Not in labor force', label))]

analysis.dataset.us.DT[ , variable := ifelse(employed, 'employed', ifelse(unemployed, 'unemployed', ifelse(notinlaborforce, 'notinlaborforce', NA_character_)))][!is.na(variable)]
analysis.dataset.us.DT[ , rest.of.variable := label %>% gsub('In the labor force:!!Employed:!!', '', .) %>% gsub('In the labor force:!!Unemployed:!!', '', .) %>% gsub('Not in labor force:!!', '', .)]
analysis.dataset.us.DT[ , denom.estimate := sum(estimate), by=.(GEOID, NAME, year, rest.of.variable)]
analysis.dataset.us.DT[ , denom.moe := moe_sum(moe, estimate), by=.(GEOID, NAME, year, rest.of.variable)]
analysis.dataset.us.DT[ , prop.estimate := estimate/denom.estimate]
analysis.dataset.us.DT[ , prop.moe := moe_prop(estimate, denom.estimate, moe, denom.moe)]
analysis.dataset.us.DT[ , prop.se := prop.moe/qnorm(.95)]
analysis.dataset.us.DT[ , neff.denom := prop.estimate*(1-prop.estimate)/(prop.se^2)]

analysis.dataset.wide.us.2.DT <- analysis.dataset.us.DT %>%
  pivot_wider(
    id_cols=c('GEOID', 'NAME', 'year', 'rest.of.variable'),
    names_from=variable,
    names_sep = '.',
    values_from=c(estimate, moe, prop.estimate, prop.moe, denom.estimate, denom.moe, neff.denom)
  ) %>% data.table

analysis.dataset.wide.us.2.DT[, no.disability := as.numeric(grepl('No disability', rest.of.variable))]
analysis.dataset.wide.us.2.DT[, hearing := as.numeric(grepl('With a hearing difficulty', rest.of.variable))]
analysis.dataset.wide.us.2.DT[, vision := as.numeric(grepl('With a vision difficulty', rest.of.variable))]
analysis.dataset.wide.us.2.DT[, cognitive := as.numeric(grepl('With a cognitive difficulty', rest.of.variable))]
analysis.dataset.wide.us.2.DT[, ambulatory := as.numeric(grepl('With an ambulatory difficulty', rest.of.variable))]
analysis.dataset.wide.us.2.DT[, selfcare := as.numeric(grepl('With a self-care difficulty', rest.of.variable))]
analysis.dataset.wide.us.2.DT[, independentliving := as.numeric(grepl('With an independent living difficulty', rest.of.variable))]

analysis.dataset.wide.us.2.DT[ , p.employed := estimate.employed/(estimate.employed+estimate.unemployed+estimate.notinlaborforce)]
analysis.dataset.wide.us.2.DT[ , year := as.numeric(year)]
analysis.dataset.wide.us.2.DT[ , NAMEf := factor(NAME)]

fit.us.2.w.2 <- bam(
  prop.estimate.employed ~
    s(year) +
    s(year, by=hearing) + s(year, by=vision) + s(year, by=cognitive) + s(year, by=ambulatory) + s(year, by=selfcare) + s(year, by=independentliving),
  weights=neff.denom.employed,
  family=quasibinomial,
  discrete=TRUE,
  data=analysis.dataset.wide.us.2.DT
)
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


