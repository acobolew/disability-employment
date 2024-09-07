library(tidycensus)

loaded.variables.DT <- data.table( load_variables(2021, dataset='acs1') )
loaded.variables.DT[grep('Employed:!!With a disability:!!With a cognitive difficulty', label)]
loaded.variables.DT[grep('Unemployed:!!With a disability:!!With a cognitive difficulty', label)]
loaded.variables.DT[grep('Not in labor force:!!With a disability:!!With a cognitive difficulty', label)]

cog.employment.DT <- data.table(get_acs(geography='state', variables=c('B18120_007', 'B18120_016', 'B18120_025'), survey='acs1', state='ME', year=2022))
cog.employment.DT[ , plain.lg := c('employed', 'unemployed', 'not.in.labor.force')]
cog.employment.DT[ plain.lg=='employed', estimate]/cog.employment.DT[ , sum(estimate)]

2436/21984
821/2578
2842/30358

loaded.variables.DT[grep('With a cognitive difficulty', label)]

loaded.variables.DT[label=='Estimate!!Total:!!Male:!!18 to 64 years:!!With a cognitive difficulty']
loaded.variables.DT[label=='Estimate!!Total:!!Female:!!18 to 64 years:!!With a cognitive difficulty']

male.and.female.cog.pop.18.to.64.est.DT <- data.table(get_acs(geography='state', variables=c('C18104_007', 'C18104_017'), survey='acs1', state='ME', year=2022))
male.and.female.cog.pop.18.to.64.est.DT[ , sum(estimate)] # 54920 +- c(2846, 2561) # moe +- 7.2%

# var(x/y) ~= (Ex/Ey)^2 * (varX/(Ex)^2 + varY/(Ey)^2 - 2 cov(x,y)/(ExEy))

employed.m <- 21984
employed.var <- 2436^2
tot.m <- 52920
tot.var <- 2846^2 + 2561^2
(ratio.m <- employed.m/tot.m)
(ratio.sd <- sqrt( (employed.m/tot.m)^2 * ( employed.var/(employed.m^2) + tot.var/(tot.m^2) ) ))