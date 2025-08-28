
# demographics.R
# for Tables 2-3

acs1.demographic.variables.DT <- acs1.loaded.variables.DT[
  !grepl('Poverty Status', concept) &
  !grepl('Group', concept) &
  !grepl('Insurance', concept) &
  (
    grepl('Age by Disability' , concept) |
      grepl('Sex by Age.*Difficulty', concept)
  ) &
  !grepl('65 years and over', label) &
  !grepl('Under 18', label) &
  !grepl('Under 5', label) &
  !grepl('65', label) &
  !grepl('75', label) &
  !grepl('5 to 17', label)
]

acs1.years <- do.call(lst, as.list(as.numeric(c(2010:2019, 2021:2023))))
states.demographics.estimates.and.moe.DT <- map_dfr(
  acs1.years,
  ~get_acs(
    geography='state',
    variables=acs1.demographic.variables.DT[ , name],
    survey='acs1',
    year=.x
  ),
  .id='year'
) %>% data.table
us.demographics.estimates.and.moe.DT <- map_dfr(
  acs1.years,
  ~get_acs(
    geography='us',
    variables=acs1.demographic.variables.DT[ , name],
    survey='acs1',
    year=.x
  ),
  .id='year'
) %>% data.table
us.demographics.estimates.and.moe.joined.DT <-
  us.demographics.estimates.and.moe.DT %>%
  left_join(acs1.demographic.variables.DT, join_by(variable==name))

############
# by sex:
# aggregate over state-specific dataset, which includes Puerto Rico
# This feeds part of Table 2

vars.by.sex.male.18.64.with.disab <- 'C18101_007'
vars.by.sex.male.18.64.no.disab <- 'C18101_008'
vars.by.sex.female.18.64.with.disab <- 'C18101_017'
vars.by.sex.female.18.64.no.disab <- 'C18101_018'

vars.by.sex.male.18.64.with.cogdisab <- 'C18104_007'
vars.by.sex.male.18.64.no.cogdisab <- 'C18104_008'
vars.by.sex.female.18.64.with.cogdisab <- 'C18104_017'
vars.by.sex.female.18.64.no.cogdisab <- 'C18104_018'

states.demographics.estimates.and.moe.joined.DT[ , .(
  num.male.18.64.with.disab = sum(estimate[variable==vars.by.sex.male.18.64.with.disab]),
  num.female.18.64.with.disab = sum(estimate[variable==vars.by.sex.female.18.64.with.disab]),
  num.male.18.64.no.disab = sum(estimate[variable==vars.by.sex.male.18.64.no.disab]),
  num.female.18.64.no.disab = sum(estimate[variable==vars.by.sex.female.18.64.no.disab]),
  num.male.18.64.with.cogdisab = sum(estimate[variable==vars.by.sex.male.18.64.with.cogdisab]),
  num.female.18.64.with.cogdisab = sum(estimate[variable==vars.by.sex.female.18.64.with.cogdisab]),
  num.male.18.64.no.cogdisab = sum(estimate[variable==vars.by.sex.male.18.64.no.cogdisab]),
  num.female.18.64.no.cogdisab = sum(estimate[variable==vars.by.sex.female.18.64.no.cogdisab])
), by=.(year)][ , .(
  age.18.64.with.disab.prop.female = num.female.18.64.with.disab / ( num.female.18.64.with.disab + num.male.18.64.with.disab ),
  age.18.64.no.disab.prop.female = num.female.18.64.no.disab / ( num.female.18.64.no.disab + num.male.18.64.no.disab ),
  age.18.64.with.cogdisab.prop.female = num.female.18.64.with.cogdisab / ( num.female.18.64.with.cogdisab + num.male.18.64.with.cogdisab ),
  age.18.64.no.cogdisab.prop.female = num.female.18.64.no.cogdisab / ( num.female.18.64.no.cogdisab + num.male.18.64.no.cogdisab )
), by=.(year)][order(-year)]

#############
# by age:
# aggregate over state-specific dataset, which includes Puerto Rico
# This feeds part of Table 2

vars.by.age.18.34.with.disab <- c('B18101_010', 'B18101_029') # M then F
vars.by.age.18.34.no.disab <- c('B18101_011', 'B18101_030') # M then F
vars.by.age.35.64.with.disab <- c('B18101_013', 'B18101_032') # M then F
vars.by.age.35.64.no.disab <- c('B18101_014', 'B18101_033') # M then F

vars.by.age.18.34.with.cogdisab <- c('B18104_007', 'B18104_023') # M then F
vars.by.age.18.34.no.cogdisab <- c('B18104_008', 'B18104_024') # M then F
vars.by.age.35.64.with.cogdisab <- c('B18104_010', 'B18104_026') # M then F
vars.by.age.35.64.no.cogdisab <- c('B18104_011', 'B18104_027') # M then F

states.demographics.estimates.and.moe.joined.DT[ , .(
  num.18.34.with.disab = sum(estimate[variable %in% vars.by.age.18.34.with.disab]),
  num.18.34.no.disab = sum(estimate[variable %in% vars.by.age.18.34.no.disab]),
  num.18.34.with.cogdisab = sum(estimate[variable %in% vars.by.age.18.34.with.cogdisab]),
  num.18.34.no.cogdisab = sum(estimate[variable %in% vars.by.age.18.34.no.cogdisab]),
  num.35.64.with.disab = sum(estimate[variable %in% vars.by.age.35.64.with.disab]),
  num.35.64.no.disab = sum(estimate[variable %in% vars.by.age.35.64.no.disab]),
  num.35.64.with.cogdisab = sum(estimate[variable %in% vars.by.age.35.64.with.cogdisab]),
  num.35.64.no.cogdisab = sum(estimate[variable %in% vars.by.age.35.64.no.cogdisab])
), by=.(year)][ , .(
  with.disab.prop.younger = num.18.34.with.disab / ( num.18.34.with.disab + num.35.64.with.disab),
  no.disab.prop.younger = num.18.34.no.disab / ( num.18.34.no.disab + num.35.64.no.disab),
  with.cogdisab.prop.younger = num.18.34.with.cogdisab / ( num.18.34.with.cogdisab + num.35.64.with.cogdisab),
  no.cogdisab.prop.younger = num.18.34.no.cogdisab / ( num.18.34.no.cogdisab + num.35.64.no.cogdisab)
), by=.(year)][order(-year)]


#############
# by ethnicity:
# aggregate over state-specific dataset, which includes Puerto Rico
# This feeds part of Table 2

vars.by.eth.18.64.wnh.with.disab <- 'B18101H_006' # 18-64 White Alone, not Hispanic or Latino with a disability
vars.by.eth.18.64.wnh.no.disab <- 'B18101H_007' # 18-64 White Alone, not Hispanic or Latino no disab
vars.by.eth.18.64.his.with.disab <- 'B18101I_006' # 18-64 Hispanic or Latino with a disability
vars.by.eth.18.64.his.no.disab <- 'B18101I_007' # 18-64 Hispanic or Latino no disab

states.demographics.estimates.and.moe.joined.DT[ , .(
  num.18.64.wnh.with.disab = sum(estimate[variable==vars.by.eth.18.64.wnh.with.disab]),
  num.18.64.wnh.no.disab = sum(estimate[variable==vars.by.eth.18.64.wnh.no.disab]),
  num.18.64.his.with.disab = sum(estimate[variable==vars.by.eth.18.64.his.with.disab]),
  num.18.64.his.no.disab = sum(estimate[variable==vars.by.eth.18.64.his.no.disab])
), by=.(year)][ , .(
  age.18.64.with.disab.relprop.his = num.18.64.his.with.disab / (num.18.64.wnh.with.disab + num.18.64.his.with.disab),
  age.18.64.no.disab.relprop.his = num.18.64.his.no.disab / (num.18.64.wnh.no.disab + num.18.64.his.no.disab)
), by=.(year)][order(-year)]


#############
# by race:
# use US dataset, even though it excludes Puerto Rico, because some states have NA for one or more racial categories, at least in some years
# This feeds Table 3

vars.by.race.white.with.disab <- 'B18101A_006' # 18-64 with a disability (White Alone)
vars.by.race.white.no.disab <- 'B18101A_007' # 18-64 no disability (White Alone)
vars.by.race.black.with.disab <- 'B18101B_006' # 18-64 with a disability (Black/AA Alone)
vars.by.race.black.no.disab <- 'B18101B_007' # 18-64 no disability (Black/AA Alone)
vars.by.race.aian.with.disab <- 'B18101C_006' # 18-64 with a disability (AIAN)
vars.by.race.aian.no.disab <- 'B18101C_007' # 18-64 no disability (AIAN)
vars.by.race.asian.with.disab <- 'B18101D_006' # 18-64 with a disability (Asian alone)
vars.by.race.asian.no.disab <- 'B18101D_007' # 18-64 no disability (Asian alone)
vars.by.race.nhopi.with.disab <- 'B18101E_006' # 18-64 with a disability (NHOPI alone)
vars.by.race.nhopi.no.disab <- 'B18101E_007' # 18-64 no disability (NHOPI alone)
vars.by.race.sor.with.disab <- 'B18101F_006' # 18-64 with a disability (some other race alone)
vars.by.race.sor.no.disab <- 'B18101F_007' # 18-64 no disability (some other race alone)
vars.by.race.2plus.with.disab <- 'B18101G_006' # 18-64 with a disability (2+ races)
vars.by.race.2plus.no.disab <- 'B18101G_007' # 18-64 no disability (2+ races)

us.demographics.estimates.and.moe.joined.DT[ , .(
  num.white.with.disab = sum(estimate[variable==vars.by.race.white.with.disab]),
  num.white.no.disab = sum(estimate[variable==vars.by.race.white.no.disab]),
  num.black.with.disab = sum(estimate[variable==vars.by.race.black.with.disab]),
  num.black.no.disab = sum(estimate[variable==vars.by.race.black.no.disab]),
  num.aian.with.disab = sum(estimate[variable==vars.by.race.aian.with.disab]),
  num.aian.no.disab = sum(estimate[variable==vars.by.race.aian.no.disab]),
  num.asian.with.disab = sum(estimate[variable==vars.by.race.asian.with.disab]),
  num.asian.no.disab = sum(estimate[variable==vars.by.race.asian.no.disab]),
  num.nhopi.with.disab = sum(estimate[variable==vars.by.race.nhopi.with.disab]),
  num.nhopi.no.disab = sum(estimate[variable==vars.by.race.nhopi.no.disab]),
  num.sor.with.disab = sum(estimate[variable==vars.by.race.sor.with.disab]),
  num.sor.no.disab = sum(estimate[variable==vars.by.race.sor.no.disab]),
  num.2plus.with.disab = sum(estimate[variable==vars.by.race.2plus.with.disab]),
  num.2plus.no.disab = sum(estimate[variable==vars.by.race.2plus.no.disab])
), by=.(year)][ , `:=`(
  num.with.disab = num.white.with.disab + num.black.with.disab + num.aian.with.disab + num.asian.with.disab + num.nhopi.with.disab + num.sor.with.disab + num.2plus.with.disab,
  num.no.disab = num.white.no.disab + num.black.no.disab + num.aian.no.disab + num.asian.no.disab + num.nhopi.no.disab + num.sor.no.disab + num.2plus.no.disab
), by=.(year)][ , .(
  with.disab.prop.white = num.white.with.disab/num.with.disab,
  no.disab.prop.white = num.white.no.disab/num.no.disab,
  with.disab.prop.black = num.black.with.disab/num.with.disab,
  no.disab.prop.black = num.black.no.disab/num.no.disab,
  with.disab.prop.aian = num.aian.with.disab/num.with.disab,
  no.disab.prop.aian = num.aian.no.disab/num.no.disab,
  with.disab.prop.asian = num.asian.with.disab/num.with.disab,
  no.disab.prop.asian = num.asian.no.disab/num.no.disab,
  with.disab.prop.nhopi = num.nhopi.with.disab/num.with.disab,
  no.disab.prop.nhopi = num.nhopi.no.disab/num.no.disab,
  with.disab.prop.sor = num.sor.with.disab/num.with.disab,
  no.disab.prop.sor = num.sor.no.disab/num.no.disab,
  with.disab.prop.2plus = num.2plus.with.disab/num.with.disab,
  no.disab.prop.2plus = num.2plus.no.disab/num.no.disab
), by=.(year)][order(-year)][]




