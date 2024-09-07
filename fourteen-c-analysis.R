loaded.variables.DT[grepl('cognitive difficulty', label) & grepl('18 to 64', label)]
# SEX BY AGE BY COGNITIVE DIFFICULTY

loaded.variables.DT[grepl('difficulty', label) & grepl('18 to 64', label)]

# https://walker-data.com/census-r/analyzing-census-microdata.html
# https://rpubs.com/corey_sparks/489270

library(tidyr)
library(purrr)
library(tibble)
library(mgcv)
library(ggplot2)
library(ggh4x)
library(data.table)
library(tidycensus)

elimination.year.DT <- tribble(
  ~state, ~elim.year,
  'Delaware', 2021,
  'Hawaii', 2021,
  'Washington', 2021,
  'Maine', 2020,
  'Oregon', 2019,
  'Maryland', 2016,
  'Vermont', 2016,
  'New Hampshire', 2015
) %>% data.table

target.variables.DT <- loaded.variables.DT[concept=='EMPLOYMENT STATUS BY DISABILITY STATUS AND TYPE']

years <- do.call(lst, as.list(as.numeric(c(2010:2019, 2021:2022))))
estimates.and.moe.ME.2022.DT <- get_acs(geography='state', variables=target.variables.DT[,name], survey='acs1', state='me', year=2022) %>% data.table
estimates.and.moe.2022.DT <- get_acs(geography='state', variables=target.variables.DT[,name], survey='acs1', year=2022) %>% data.table
estimates.and.moe.DT <- map_dfr(
  years,
  ~get_acs(geography='state', variables=target.variables.DT[ , name], survey='acs1', year=.x),
  .id='year'
) %>% data.table

linked.data.DT <- target.variables.DT[estimates.and.moe.DT, on=c(name='variable')]
analysis.dataset.DT <- linked.data.DT[grepl('With a disability:!!|No disability', label)]

analysis.dataset.DT[, employed := as.numeric(grepl('Employed', label))]
analysis.dataset.DT[, unemployed := as.numeric(grepl('Unemployed', label))]
analysis.dataset.DT[, notinlaborforce := as.numeric(grepl('Not in labor force', label))]

analysis.dataset.DT[ , variable := ifelse(employed, 'employed', ifelse(unemployed, 'unemployed', ifelse(notinlaborforce, 'notinlaborforce', NA_character_)))][!is.na(variable)]
analysis.dataset.DT[ , rest.of.variable := label %>% gsub('In the labor force:!!Employed:!!', '', .) %>% gsub('In the labor force:!!Unemployed:!!', '', .) %>% gsub('Not in labor force:!!', '', .)]
analysis.dataset.DT[ , denom.estimate := sum(estimate), by=.(GEOID, NAME, year, rest.of.variable)]
analysis.dataset.DT[ , denom.moe := moe_sum(moe, estimate), by=.(GEOID, NAME, year, rest.of.variable)]
analysis.dataset.DT[ , prop.estimate := estimate/denom.estimate]
analysis.dataset.DT[ , prop.moe := moe_prop(estimate, denom.estimate, moe, denom.moe)]
analysis.dataset.DT[ , prop.se := prop.moe/qnorm(.95)]
analysis.dataset.DT[ , neff.denom := prop.estimate*(1-prop.estimate)/(prop.se^2)]


analysis.dataset.wide.2.DT <- analysis.dataset.DT %>%
  pivot_wider(
    id_cols=c('GEOID', 'NAME', 'year', 'rest.of.variable'),
    names_from=variable,
    names_sep = '.',
    values_from=c(estimate, moe, prop.estimate, prop.moe, denom.estimate, denom.moe, neff.denom)
  ) %>% data.table

analysis.dataset.wide.DT <- analysis.dataset.DT %>%
  pivot_wider(
    id_cols=c('GEOID', 'NAME', 'year', 'rest.of.variable'),
    names_from=variable,
    names_sep = '.',
    values_from=c(estimate, moe)
  ) %>% data.table


analysis.dataset.wide.2.DT[, no.disability := as.numeric(grepl('No disability', rest.of.variable))]
analysis.dataset.wide.2.DT[, hearing := as.numeric(grepl('With a hearing difficulty', rest.of.variable))]
analysis.dataset.wide.2.DT[, vision := as.numeric(grepl('With a vision difficulty', rest.of.variable))]
analysis.dataset.wide.2.DT[, cognitive := as.numeric(grepl('With a cognitive difficulty', rest.of.variable))]
analysis.dataset.wide.2.DT[, ambulatory := as.numeric(grepl('With an ambulatory difficulty', rest.of.variable))]
analysis.dataset.wide.2.DT[, selfcare := as.numeric(grepl('With a self-care difficulty', rest.of.variable))]
analysis.dataset.wide.2.DT[, independentliving := as.numeric(grepl('With an independent living difficulty', rest.of.variable))]

analysis.dataset.wide.2.DT[ , p.employed := estimate.employed/(estimate.employed+estimate.unemployed+estimate.notinlaborforce)]
analysis.dataset.wide.2.DT[ , year := as.numeric(year)]
analysis.dataset.wide.2.DT[ , NAMEf := factor(NAME)]

analysis.dataset.wide.2.DT <- elimination.year.DT[analysis.dataset.wide.2.DT, on=c(state='NAME')]
analysis.dataset.wide.2.DT[is.na(elim.year), elim.year := Inf]

cbind(
  analysis.dataset.wide.DT[, .(p.employed, moe.prop, n.eff)],
  analysis.dataset.wide.2.DT[ , .(prop.estimate.employed, prop.moe.employed)]
)[moe.prop != prop.moe.employed]

analysis.dataset.wide.DT[, no.disability := as.numeric(grepl('No disability', rest.of.variable))]
analysis.dataset.wide.DT[, hearing := as.numeric(grepl('With a hearing difficulty', rest.of.variable))]
analysis.dataset.wide.DT[, vision := as.numeric(grepl('With a vision difficulty', rest.of.variable))]
analysis.dataset.wide.DT[, cognitive := as.numeric(grepl('With a cognitive difficulty', rest.of.variable))]
analysis.dataset.wide.DT[, ambulatory := as.numeric(grepl('With an ambulatory difficulty', rest.of.variable))]
analysis.dataset.wide.DT[, selfcare := as.numeric(grepl('With a self-care difficulty', rest.of.variable))]
analysis.dataset.wide.DT[, independentliving := as.numeric(grepl('With an independent living difficulty', rest.of.variable))]

analysis.dataset.wide.DT[ , p.employed := estimate.employed/(estimate.employed+estimate.unemployed+estimate.notinlaborforce)]
analysis.dataset.wide.DT[ , year := as.numeric(year)]
analysis.dataset.wide.DT[ , NAMEf := factor(NAME)]

analysis.dataset.wide.DT <- elimination.year.DT[analysis.dataset.wide.DT, on=c(state='NAME')]
analysis.dataset.wide.DT[is.na(elim.year), elim.year := Inf]

fit6 <- gam(p.employed ~ s(as.numeric(year)) + s(NAMEf, bs='re')
            + hearing + I(hearing*as.numeric(year>=elim.year))
            + vision + I(vision*as.numeric(year>=elim.year))
            + cognitive + I(cognitive*as.numeric(year>=elim.year))
            + ambulatory + I(ambulatory*as.numeric(year>=elim.year))
            + selfcare + I(selfcare*as.numeric(year>=elim.year))
            + independentliving + I(independentliving*as.numeric(year>=elim.year))
            # , subset=NAMEf != 'Puerto Rico'
            , weights=estimate.employed+estimate.unemployed+estimate.notinlaborforce, family=quasibinomial, data=analysis.dataset.wide.DT)
summary(fit6)
plot(fit6)

fit6r <- gamm(p.employed ~ s(as.numeric(year))
            + hearing + I(hearing*as.numeric(year>=elim.year))
            + vision + I(vision*as.numeric(year>=elim.year))
            + cognitive + I(cognitive*as.numeric(year>=elim.year))
            + ambulatory + I(ambulatory*as.numeric(year>=elim.year))
            + selfcare + I(selfcare*as.numeric(year>=elim.year))
            + independentliving + I(independentliving*as.numeric(year>=elim.year)),
            random=list(NAMEf=~1)
            # , subset=NAMEf != 'Puerto Rico'
            , weights=estimate.employed+estimate.unemployed+estimate.notinlaborforce, family=quasibinomial, data=analysis.dataset.wide.DT)

fit6ar <- gam(p.employed ~ s(as.numeric(year))
              + s(NAMEf, bs='re')
              + s(hearing, NAMEf, bs='re')
              + s(vision, NAMEf, bs='re')
              + s(cognitive, NAMEf, bs='re')
              + s(ambulatory, NAMEf, bs='re')
              + s(selfcare, NAMEf, bs='re')
              + s(independentliving, NAMEf, bs='re')
              + hearing + I(hearing*as.numeric(year>=elim.year))
              + vision + I(vision*as.numeric(year>=elim.year))
              + cognitive + I(cognitive*as.numeric(year>=elim.year))
              + ambulatory + I(ambulatory*as.numeric(year>=elim.year))
              + selfcare + I(selfcare*as.numeric(year>=elim.year))
              + independentliving + I(independentliving*as.numeric(year>=elim.year))
              , weights=estimate.employed+estimate.unemployed+estimate.notinlaborforce, family=quasibinomial, data=analysis.dataset.wide.DT)

fit6ar2.time <- system.time(fit6ar2 <- gam(p.employed ~ s(as.numeric(year))
              + s(as.numeric(year), by=hearing)
              + s(as.numeric(year), by=vision)
              + s(as.numeric(year), by=cognitive)
              + s(as.numeric(year), by=ambulatory)
              + s(as.numeric(year), by=selfcare)
              + s(as.numeric(year), by=independentliving)
              + s(NAMEf, bs='re')
              + s(hearing, NAMEf, bs='re')
              + s(vision, NAMEf, bs='re')
              + s(cognitive, NAMEf, bs='re')
              + s(ambulatory, NAMEf, bs='re')
              + s(selfcare, NAMEf, bs='re')
              + s(independentliving, NAMEf, bs='re')
              + hearing + I(hearing*as.numeric(year>=elim.year))
              + vision + I(vision*as.numeric(year>=elim.year))
              + cognitive + I(cognitive*as.numeric(year>=elim.year))
              + ambulatory + I(ambulatory*as.numeric(year>=elim.year))
              + selfcare + I(selfcare*as.numeric(year>=elim.year))
              + independentliving + I(independentliving*as.numeric(year>=elim.year))
              , weights=estimate.employed+estimate.unemployed+estimate.notinlaborforce, family=quasibinomial, data=analysis.dataset.wide.DT)
)
fit6a2.time <- system.time(
  fit6a2 <- gam(p.employed ~ s(as.numeric(year))
                + s(as.numeric(year), by=hearing)
                + s(as.numeric(year), by=vision)
                + s(as.numeric(year), by=cognitive)
                + s(as.numeric(year), by=ambulatory)
                + s(as.numeric(year), by=selfcare)
                + s(as.numeric(year), by=independentliving)
                + (hearing + vision + cognitive + ambulatory + selfcare + independentliving) * NAMEf
                + I(hearing*as.numeric(year>=elim.year))
                + I(vision*as.numeric(year>=elim.year))
                + I(cognitive*as.numeric(year>=elim.year))
                + I(ambulatory*as.numeric(year>=elim.year))
                + I(selfcare*as.numeric(year>=elim.year))
                + I(independentliving*as.numeric(year>=elim.year))
                , weights=estimate.employed+estimate.unemployed+estimate.notinlaborforce, family=quasibinomial, data=analysis.dataset.wide.DT)
  
)

fit6.3.time <- system.time(fit6.3 <- gam(p.employed ~ s(as.numeric(year))
                                           + s(as.numeric(year), by=hearing)
                                           + s(as.numeric(year), by=vision)
                                           + s(as.numeric(year), by=cognitive)
                                           + s(as.numeric(year), by=ambulatory)
                                           + s(as.numeric(year), by=selfcare)
                                           + s(as.numeric(year), by=independentliving)
                                           + s(NAMEf, bs='re')
                                           + s(hearing, NAMEf, bs='re')
                                           + s(vision, NAMEf, bs='re')
                                           + s(cognitive, NAMEf, bs='re')
                                           + s(ambulatory, NAMEf, bs='re')
                                           + s(selfcare, NAMEf, bs='re')
                                           + s(independentliving, NAMEf, bs='re')
                                           + I(hearing*as.numeric(year>=elim.year))
                                           + I(vision*as.numeric(year>=elim.year))
                                           + I(cognitive*as.numeric(year>=elim.year))
                                           + I(ambulatory*as.numeric(year>=elim.year))
                                           + I(selfcare*as.numeric(year>=elim.year))
                                           + I(independentliving*as.numeric(year>=elim.year))
                                           , weights=estimate.employed+estimate.unemployed+estimate.notinlaborforce, family=quasibinomial, data=analysis.dataset.wide.DT)
)
fit6.3.bam.time <- system.time(fit6.3.bam <- bam(p.employed ~ s(as.numeric(year))
                                         + s(as.numeric(year), by=hearing)
                                         + s(as.numeric(year), by=vision)
                                         + s(as.numeric(year), by=cognitive)
                                         + s(as.numeric(year), by=ambulatory)
                                         + s(as.numeric(year), by=selfcare)
                                         + s(as.numeric(year), by=independentliving)
                                         + s(NAMEf, bs='re')
                                         + s(hearing, NAMEf, bs='re')
                                         + s(vision, NAMEf, bs='re')
                                         + s(cognitive, NAMEf, bs='re')
                                         + s(ambulatory, NAMEf, bs='re')
                                         + s(selfcare, NAMEf, bs='re')
                                         + s(independentliving, NAMEf, bs='re')
                                         + I(hearing*as.numeric(year>=elim.year))
                                         + I(vision*as.numeric(year>=elim.year))
                                         + I(cognitive*as.numeric(year>=elim.year))
                                         + I(ambulatory*as.numeric(year>=elim.year))
                                         + I(selfcare*as.numeric(year>=elim.year))
                                         + I(independentliving*as.numeric(year>=elim.year))
                                         , weights=estimate.employed+estimate.unemployed+estimate.notinlaborforce, family=quasibinomial, data=analysis.dataset.wide.DT)
)
fit6.3.bam.discrete.time <- system.time(fit6.3.bam.disrete <- bam(p.employed ~ s(as.numeric(year))
                                                 + s(as.numeric(year), by=hearing)
                                                 + s(as.numeric(year), by=vision)
                                                 + s(as.numeric(year), by=cognitive)
                                                 + s(as.numeric(year), by=ambulatory)
                                                 + s(as.numeric(year), by=selfcare)
                                                 + s(as.numeric(year), by=independentliving)
                                                 + s(NAMEf, bs='re')
                                                 + s(hearing, NAMEf, bs='re')
                                                 + s(vision, NAMEf, bs='re')
                                                 + s(cognitive, NAMEf, bs='re')
                                                 + s(ambulatory, NAMEf, bs='re')
                                                 + s(selfcare, NAMEf, bs='re')
                                                 + s(independentliving, NAMEf, bs='re')
                                                 + I(hearing*as.numeric(year>=elim.year))
                                                 + I(vision*as.numeric(year>=elim.year))
                                                 + I(cognitive*as.numeric(year>=elim.year))
                                                 + I(ambulatory*as.numeric(year>=elim.year))
                                                 + I(selfcare*as.numeric(year>=elim.year))
                                                 + I(independentliving*as.numeric(year>=elim.year))
                                                 , weights=estimate.employed+estimate.unemployed+estimate.notinlaborforce, family=quasibinomial, data=analysis.dataset.wide.DT, discrete=TRUE)
)

# library(splines)
# svglm(fpl ~ ns(age,4) + gender, design = nhanesDesign)
# https://stackoverflow.com/questions/56313837/how-to-use-sample-weights-in-gam-mgcv-on-survey-data-for-logit-regression
#
# https://walker-data.com/census-r/analyzing-census-microdata.html
# https://walker-data.com/tidycensus/articles/margins-of-error.html

fit.minus1 <- bam(
  p.employed ~
    s(year) +
    s(NAMEf, bs='re') +
    hearing + vision + cognitive + ambulatory + selfcare + independentliving,
  weights=estimate.employed+estimate.unemployed+estimate.notinlaborforce,
  family=quasibinomial,
  discrete=TRUE,
  data=analysis.dataset.wide.DT
)

fit.0 <- bam(
  p.employed ~
    s(year) +
    s(NAMEf, bs='re') +
    s(hearing, NAMEf, bs='re') + s(vision, NAMEf, bs='re') + s(cognitive, NAMEf, bs='re') + s(ambulatory, NAMEf, bs='re') + s(selfcare, NAMEf, bs='re') + s(independentliving, NAMEf, bs='re') +
    I(hearing*(year>=elim.year)) + I(vision*(year>=elim.year)) + I(cognitive*(year>=elim.year)) + I(ambulatory*(year>=elim.year)) + I(selfcare*(year>=elim.year)) + I(independentliving*(year>=elim.year)),
  weights=estimate.employed+estimate.unemployed+estimate.notinlaborforce,
  family=quasibinomial,
  discrete=TRUE,
  data=analysis.dataset.wide.DT
)

fit.1 <- bam(
  p.employed ~
    s(year) +
    s(year, by=hearing) + s(year, by=vision) + s(year, by=cognitive) + s(year, by=ambulatory) + s(year, by=selfcare) + s(year, by=independentliving) +
    s(NAMEf, bs='re') +
    s(hearing, NAMEf, bs='re') + s(vision, NAMEf, bs='re') + s(cognitive, NAMEf, bs='re') + s(ambulatory, NAMEf, bs='re') + s(selfcare, NAMEf, bs='re') + s(independentliving, NAMEf, bs='re') +
    I(hearing*(year>=elim.year)) + I(vision*(year>=elim.year)) + I(cognitive*(year>=elim.year)) + I(ambulatory*(year>=elim.year)) + I(selfcare*(year>=elim.year)) + I(independentliving*(year>=elim.year)),
  weights=estimate.employed+estimate.unemployed+estimate.notinlaborforce,
  family=quasibinomial,
  discrete=TRUE,
  data=analysis.dataset.wide.DT
)

fit.2 <- bam(
  p.employed ~
    s(year) +
    s(year, by=hearing) + s(year, by=vision) + s(year, by=cognitive) + s(year, by=ambulatory) + s(year, by=selfcare) + s(year, by=independentliving) +
    s(NAMEf, bs='re') +
    s(hearing, NAMEf, bs='re') + s(vision, NAMEf, bs='re') + s(cognitive, NAMEf, bs='re') + s(ambulatory, NAMEf, bs='re') + s(selfcare, NAMEf, bs='re') + s(independentliving, NAMEf, bs='re'),
  weights=estimate.employed+estimate.unemployed+estimate.notinlaborforce,
  family=quasibinomial,
  discrete=TRUE,
  data=analysis.dataset.wide.DT
)

analysis.dataset.wide.DT[ , moe.denom := sqrt(moe.employed^2 + moe.unemployed^2 + moe.notinlaborforce^2)]
analysis.dataset.wide.DT[
  , r2 := p.employed^2 ][
  , mn2 := moe.employed^2][
  , md2 := moe.denom^2][
  , moe.ratio := (sqrt(mn2 + (r2 * md2)))/(estimate.employed + estimate.unemployed + estimate.notinlaborforce)  
]
analysis.dataset.wide.DT[ , mpx := moe.employed^2 - (p.employed^2 * moe.denom^2) ]
analysis.dataset.wide.DT[ mpx > 0 & !is.na(mpx), moe.prop := sqrt( mpx ) / ( estimate.employed + estimate.unemployed + estimate.notinlaborforce ) ]
analysis.dataset.wide.DT[ !(mpx > 0 & !is.na(mpx)), moe.prop := moe.ratio ]
analysis.dataset.wide.DT[ , se.prop := moe.prop/qnorm(.95)]
analysis.dataset.wide.DT[ , n.eff := .5*.5/(se.prop^2)]

fit.minus1.w <- bam(
  p.employed ~
    s(year) +
    s(NAMEf, bs='re') +
    hearing + vision + cognitive + ambulatory + selfcare + independentliving,
  weights=n.eff,
  family=quasibinomial,
  discrete=TRUE,
  data=analysis.dataset.wide.DT
)

fit.0.w <- bam(
  p.employed ~
    s(year) +
    s(NAMEf, bs='re') +
    s(hearing, NAMEf, bs='re') + s(vision, NAMEf, bs='re') + s(cognitive, NAMEf, bs='re') + s(ambulatory, NAMEf, bs='re') + s(selfcare, NAMEf, bs='re') + s(independentliving, NAMEf, bs='re') +
    I(hearing*(year>=elim.year)) + I(vision*(year>=elim.year)) + I(cognitive*(year>=elim.year)) + I(ambulatory*(year>=elim.year)) + I(selfcare*(year>=elim.year)) + I(independentliving*(year>=elim.year)),
  weights=n.eff,
  family=quasibinomial,
  discrete=TRUE,
  data=analysis.dataset.wide.DT
)

fit.1.w <- bam(
  p.employed ~
    s(year) +
    s(year, by=hearing) + s(year, by=vision) + s(year, by=cognitive) + s(year, by=ambulatory) + s(year, by=selfcare) + s(year, by=independentliving) +
    s(NAMEf, bs='re') +
    s(hearing, NAMEf, bs='re') + s(vision, NAMEf, bs='re') + s(cognitive, NAMEf, bs='re') + s(ambulatory, NAMEf, bs='re') + s(selfcare, NAMEf, bs='re') + s(independentliving, NAMEf, bs='re') +
    I(hearing*(year>=elim.year)) + I(vision*(year>=elim.year)) + I(cognitive*(year>=elim.year)) + I(ambulatory*(year>=elim.year)) + I(selfcare*(year>=elim.year)) + I(independentliving*(year>=elim.year)),
  weights=n.eff,
  family=quasibinomial,
  discrete=TRUE,
  data=analysis.dataset.wide.DT
)

fit.2.w <- bam(
  p.employed ~
    s(year) +
    s(year, by=hearing) + s(year, by=vision) + s(year, by=cognitive) + s(year, by=ambulatory) + s(year, by=selfcare) + s(year, by=independentliving) +
    s(NAMEf, bs='re') +
    s(hearing, NAMEf, bs='re') + s(vision, NAMEf, bs='re') + s(cognitive, NAMEf, bs='re') + s(ambulatory, NAMEf, bs='re') + s(selfcare, NAMEf, bs='re') + s(independentliving, NAMEf, bs='re'),
  weights=n.eff,
  family=quasibinomial,
  discrete=TRUE,
  data=analysis.dataset.wide.DT
)

fit.minus1.w.2 <- bam(
  prop.estimate.employed ~
    s(year) +
    s(NAMEf, bs='re') +
    hearing + vision + cognitive + ambulatory + selfcare + independentliving,
  weights=neff.denom.employed,
  family=quasibinomial,
  discrete=TRUE,
  data=analysis.dataset.wide.2.DT
)

fit.0.w.2 <- bam(
  prop.estimate.employed ~
    s(year) +
    s(NAMEf, bs='re') +
    s(hearing, NAMEf, bs='re') + s(vision, NAMEf, bs='re') + s(cognitive, NAMEf, bs='re') + s(ambulatory, NAMEf, bs='re') + s(selfcare, NAMEf, bs='re') + s(independentliving, NAMEf, bs='re') +
    I(hearing*(year>=elim.year)) + I(vision*(year>=elim.year)) + I(cognitive*(year>=elim.year)) + I(ambulatory*(year>=elim.year)) + I(selfcare*(year>=elim.year)) + I(independentliving*(year>=elim.year)),
  weights=neff.denom.employed,
  family=quasibinomial,
  discrete=TRUE,
  data=analysis.dataset.wide.2.DT
)

fit.1.w.2 <- bam(
  prop.estimate.employed ~
    s(year) +
    s(year, by=hearing) + s(year, by=vision) + s(year, by=cognitive) + s(year, by=ambulatory) + s(year, by=selfcare) + s(year, by=independentliving) +
    s(NAMEf, bs='re') +
    s(hearing, NAMEf, bs='re') + s(vision, NAMEf, bs='re') + s(cognitive, NAMEf, bs='re') + s(ambulatory, NAMEf, bs='re') + s(selfcare, NAMEf, bs='re') + s(independentliving, NAMEf, bs='re') +
    I(hearing*(year>=elim.year)) + I(vision*(year>=elim.year)) + I(cognitive*(year>=elim.year)) + I(ambulatory*(year>=elim.year)) + I(selfcare*(year>=elim.year)) + I(independentliving*(year>=elim.year)),
  weights=neff.denom.employed,
  family=quasibinomial,
  discrete=TRUE,
  data=analysis.dataset.wide.2.DT
)

fit.2.w.2 <- bam(
  prop.estimate.employed ~
    s(year) +
    s(year, by=hearing) + s(year, by=vision) + s(year, by=cognitive) + s(year, by=ambulatory) + s(year, by=selfcare) + s(year, by=independentliving) +
    s(NAMEf, bs='re') +
    s(hearing, NAMEf, bs='re') + s(vision, NAMEf, bs='re') + s(cognitive, NAMEf, bs='re') + s(ambulatory, NAMEf, bs='re') + s(selfcare, NAMEf, bs='re') + s(independentliving, NAMEf, bs='re'),
  weights=neff.denom.employed,
  family=quasibinomial,
  discrete=TRUE,
  data=analysis.dataset.wide.2.DT
)


plot(fit6a2, select=1, ylim=c(-.5,.5)) # overall
plot(fit6a2, select=2, ylim=c(-1.5,-.5)) # hearing
plot(fit6a2, select=3, ylim=c(-.5,.5)) # vision
plot(fit6a2, select=4, ylim=c(-2.5,-1.5)) # cognitive
plot(fit6a2, select=5, ylim=c(-3,-2)) # ambulatory
plot(fit6a2, select=6, ylim=c(-3.5,-2.5)) # selfcare
plot(fit6a2, select=7, ylim=c(0,1)) # independentliving



aug.analysis.dataset.wide.DT <- analysis.dataset.wide.2.DT
aug.analysis.dataset.wide.DT[ , c('fit.0.w.2.link', 'fit.0.w.2.se') := as.data.table(predict(fit.0.w.2, se.fit=TRUE)) ]
aug.analysis.dataset.wide.DT[ , `:=`(fit.0.w.2.link.lo=fit.0.w.2.link-qnorm(.975)*fit.0.w.2.se, fit.0.w.2.link.hi=fit.0.w.2.link+qnorm(.975)*fit.0.w.2.se)]

counterfactual.analysis.dataset.wide.DT <- copy(analysis.dataset.wide.2.DT)
counterfactual.analysis.dataset.wide.DT[ , elim.year := Inf]
counterfactual.analysis.dataset.wide.DT[ , c('fit.0.w.2.link', 'fit.0.w.2.se') := as.data.table(predict(fit.0.w.2, se.fit=TRUE, newdata=counterfactual.analysis.dataset.wide.DT)) ]
counterfactual.analysis.dataset.wide.DT[ , `:=`(fit.0.w.2.link.lo=fit.0.w.2.link-qnorm(.975)*fit.0.w.2.se, fit.0.w.2.link.hi=fit.0.w.2.link+qnorm(.975)*fit.0.w.2.se)]


ggplot(
  aug.analysis.dataset.wide.DT[(cognitive==1 | no.disability==1) & NAMEf %in% c('Missouri', 'Delaware', 'Hawaii', 'Maine', 'Maryland', 'New Hampshire', 'Oregon', 'Vermont', 'Washington')],
  aes(x=as.numeric(year),
      shape=ifelse(no.disability==1, 'no.disability', ifelse(cognitive==1 & year < elim.year, 'cog 14(c)', ifelse(cognitive==1 & year >= elim.year, 'cog 14(c) elim', NA_character_)))
  )
) +
  geom_ribbon(aes(ymin=fit.0.w.2.link.lo, ymax=fit.0.w.2.link.hi, fill=NAMEf, group=cognitive), alpha=.2, data=counterfactual.analysis.dataset.wide.DT[(cognitive==1 | no.disability==1) & NAMEf %in% c('Delaware', 'Hawaii', 'Maine', 'Maryland', 'New Hampshire', 'Oregon', 'Vermont', 'Washington')]) +
  geom_ribbon(aes(ymin=fit.0.w.2.link.lo, ymax=fit.0.w.2.link.hi, fill=NAMEf, group=cognitive), alpha=.2) +
  geom_line(aes(y=fit.0.w.2.link, color=NAMEf, group=cognitive), linetype='dotted',                       data=counterfactual.analysis.dataset.wide.DT[(cognitive==1 | no.disability==1) & NAMEf %in% c('Delaware', 'Hawaii', 'Maine', 'Maryland', 'New Hampshire', 'Oregon', 'Vermont', 'Washington')]) +
  geom_line(aes(y=fit.0.w.2.link, color=NAMEf, group=cognitive)) +
  geom_linerange(aes(
    ymin=log((prop.estimate.employed-prop.moe.employed)/(1-(prop.estimate.employed-prop.moe.employed))),
    ymax=log((prop.estimate.employed+prop.moe.employed)/(1-(prop.estimate.employed+prop.moe.employed))),
    color=NAMEf
  )) +
  geom_point(aes(y=log(p.employed/(1-p.employed)), color=NAMEf)) +
  facet_wrap(~NAMEf) +
  scale_x_continuous(breaks=seq(2010, 2022, by=2), minor_breaks=2010:2022)

# [NAMEf %in% c('Delaware', 'Hawaii', 'Maine', 'Maryland', 'New Hampshire', 'Oregon', 'Vermont', 'Washington')]
ggplot(
  aug.analysis.dataset.wide.DT[cognitive==1 | selfcare==1 | independentliving==1 | no.disability==1][ , cbind(.SD, gp=ifelse(NAMEf %in% c('Delaware', 'Hawaii', 'Maine', 'Maryland', 'New Hampshire', 'Oregon', 'Vermont', 'Washington'), 'eliminated 14(c)', 'kept 14(c)'))],
  aes(x=year
      # , shape=ifelse(no.disability==1, 'no.disability', ifelse(hearing==1, 'hearing', ifelse(vision==1, 'vision', ifelse(cognitive==1, 'cognitive', ifelse(ambulatory==1, 'ambulatory', ifelse(selfcare==1, 'selfcare', ifelse(independentliving==1, 'independentliving', NA_character_)))))))
  )
) +
  geom_ribbon(aes(ymin=fit.2.w.2.link.lo, ymax=fit.2.w.2.link.hi, fill=ifelse(no.disability==1, 'no.disability', ifelse(hearing==1, 'hearing', ifelse(vision==1, 'vision', ifelse(cognitive==1, 'cognitive', ifelse(ambulatory==1, 'ambulatory', ifelse(selfcare==1, 'selfcare', ifelse(independentliving==1, 'independentliving', NA_character_)))))))), alpha=.2) +
  geom_line(data=counterfactual.analysis.dataset.wide.DT[cognitive==1 | selfcare==1 | independentliving==1 | no.disability==1][ , cbind(.SD, gp=ifelse(NAMEf %in% c('Delaware', 'Hawaii', 'Maine', 'Maryland', 'New Hampshire', 'Oregon', 'Vermont', 'Washington'), 'eliminated 14(c)', 'kept 14(c)'))], linetype='dotted', aes(y=fit.2.w.2.link, color=ifelse(no.disability==1, 'no.disability', ifelse(hearing==1, 'hearing', ifelse(vision==1, 'vision', ifelse(cognitive==1, 'cognitive', ifelse(ambulatory==1, 'ambulatory', ifelse(selfcare==1, 'selfcare', ifelse(independentliving==1, 'independentliving', NA_character_))))))))) +
  geom_line(aes(y=fit.2.w.2.link, color=ifelse(no.disability==1, 'no.disability', ifelse(hearing==1, 'hearing', ifelse(vision==1, 'vision', ifelse(cognitive==1, 'cognitive', ifelse(ambulatory==1, 'ambulatory', ifelse(selfcare==1, 'selfcare', ifelse(independentliving==1, 'independentliving', NA_character_))))))))) +
  geom_point(size=1, aes(y=log(p.employed/(1-p.employed)), color=ifelse(no.disability==1, 'no.disability', ifelse(hearing==1, 'hearing', ifelse(vision==1, 'vision', ifelse(cognitive==1, 'cognitive', ifelse(ambulatory==1, 'ambulatory', ifelse(selfcare==1, 'selfcare', ifelse(independentliving==1, 'independentliving', NA_character_))))))))) +
  facet_nested_wrap(vars(gp, NAMEf)) +
  scale_x_continuous(breaks=seq(2010, 2022, by=2), minor_breaks=2010:2022)

color=ifelse(no.disability==1, 'no.disability', ifelse(hearing==1, 'hearing', ifelse(vision==1, 'vision', ifelse(cognitive==1, 'cognitive', ifelse(ambulatory==1, 'ambulatory', ifelse(selfcare==1, 'selfcare', ifelse(independentliving==1, 'independentliving', NA_character_)))))))
  