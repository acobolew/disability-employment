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

states.ER.preds.DT <- CJ(NAMEf=levels(states.employment.analysis.dataset.wide.DT$NAMEf), year=seq(from=2010, to=2023, length=100))
states.ER.preds.DT[ , c('fit', 'se.fit') := as.data.table(predict(fit.ER.states.1, newdata=states.ER.preds.DT, se.fit=TRUE))]
states.ER.preds.DT <- states.dc.pr.DT[states.ER.preds.DT, on=c(geography.name='NAMEf')]

ggplot(
  states.dc.pr.DT[states.employment.analysis.dataset.wide.DT[cognitive==1], on=c(geography.name='NAME')],
  aes(
    x=year,
    y=employmentratio.estimate,
    ymin=employmentratio.estimate - employmentratio.moe/qnorm(.95)*qnorm(.975),
    ymax=employmentratio.estimate + employmentratio.moe/qnorm(.95)*qnorm(.975)
  )
) +
  facet_wrap(~geography.abb) +
  geom_line(data=states.ER.preds.DT, aes(y=fit, ymin=NULL, ymax=NULL)) +
  geom_ribbon(data=states.ER.preds.DT, aes(y=NULL, ymin=fit-se.fit*qnorm(.975), ymax=fit+se.fit*qnorm(.975)), alpha=.4) +
  geom_point() +
  geom_linerange() +
  scale_x_continuous(breaks=seq(2010, 2025, by=5), minor_breaks=2010:2025, guide=guide_axis(n.dodge=2), expand=expand_scale(mult=.1)) +
  scale_y_continuous(breaks=seq(0, .8, by=.2), minor_breaks=seq(0, .8, by=.1)) +
  theme_gray(base_size=14)


ggsave(filename='fig-ER-by-state.png', width=10, height=10)