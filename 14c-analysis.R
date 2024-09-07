library(data.table)
library(ggplot2)
library(ggrepel)
library(ggh4x)
library(forcats)

cog.working.age.employed.DT <- fread('sd_chart_builder_data-07-02-2024_22-52.csv', skip=2, header=TRUE)
cog.working.age.employed.m.DT <- melt(cog.working.age.employed.DT, id='State')
cog.working.age.employed.m.DT[ , variable := as.numeric(as.character(variable))]
cog.working.age.employed.m.DT[ , is.Maine := State=='Maine']
cog.working.age.employed.m.DT[ , is.US := State=='U.S. Total']
ggplot(cog.working.age.employed.m.DT, aes(x=variable, y=100*value, color=State, linewidth=is.Maine, linetype=is.US)) +
  geom_line() +
  ylab('% employed among working age ppl w cog disab') +
  xlab('Year') +
  scale_x_continuous(breaks=2010:2022, minor_breaks=NULL) +
  # geom_text_repel(data=~subset(.x, variable==2022), aes(label=State), direction='y')
  geom_text(data=~subset(.x, variable==2022), aes(label=State), hjust=0) +
  scale_y_continuous(breaks=seq(0,100,by=20), minor_breaks=seq(0,100,by=10)) +
  expand_limits(y=0, x=2024)

ggplot(cog.working.age.employed.m.DT, aes(x=variable, y=100*value, color=State, linewidth=is.Maine, linetype=is.US)) +
  geom_line() +
  ylab('% employed among working age ppl w cog disab') +
  xlab('Year') +
  scale_x_continuous(breaks=2010:2022, minor_breaks=NULL) +
  # geom_text_repel(data=~subset(.x, variable==2022), aes(label=State), direction='y')
  geom_text(data=~subset(.x, variable==2022), aes(label=State), hjust=0) +
  scale_y_continuous(breaks=seq(0,100,by=20), minor_breaks=seq(0,100,by=10)) +
  expand_limits(y=0, x=2024)

cog.working.age.employed.m.DT[ , State := State %>% fct_relevel('Maine') %>% fct_relevel('U.S. Total', after=Inf)]
ggplot(cog.working.age.employed.m.DT, aes(x=variable, y=100*value, color=State, linewidth=State, linetype=State, alpha=State)) +
  geom_line() +
  ylab('% employed among working age ppl w cog disab') +
  xlab('Year') +
  scale_x_continuous(breaks=2010:2022, minor_breaks=NULL) +
  # geom_text_repel(data=~subset(.x, variable==2022), aes(label=State), direction='y')
  geom_text(data=~subset(.x, variable==2022), aes(label=State), hjust=0, show.legend=FALSE) +
  scale_y_continuous(breaks=seq(0,100,by=20), minor_breaks=seq(0,100,by=10)) +
  expand_limits(y=0, x=2025) +
  scale_color_viridis_d(begin=.95, end=0) +
  scale_linewidth_manual(values=c('Maine'=3, 'Alaska'=1, 'Delaware'=1, 'District of Columbia'=1, 'Hawaii'=1, 'Maryland'=1, 'New Hampshire'=1, 'Rhode Island'=1, 'Vermont'=1, 'U.S. Total'=1.5)) +
  scale_linetype_manual(values=c('Maine'='solid', 'Alaska'='solid', 'Delaware'='solid', 'District of Columbia'='solid', 'Hawaii'='solid', 'Maryland'='solid', 'New Hampshire'='solid', 'Rhode Island'='solid', 'Vermont'='solid', 'U.S. Total'='dashed')) +
  scale_alpha_manual(values=c('Maine'=1, 'Alaska'=.5, 'Delaware'=.5, 'District of Columbia'=.5, 'Hawaii'=.5, 'Maryland'=.5, 'New Hampshire'=.5, 'Rhode Island'=.5, 'Vermont'=.5, 'U.S. Total'=1)) +
  theme(legend.key.width=unit(4, 'line')) +
  labs(
    title='Employment Among People with Cog Disablity in US Overall and States that Have Ceased 14(c)',
    subtitle='Data Source: American Community Survey via https://www.thinkwork.org/statedata/build-a-chart?report=comparison \nStates ceasing 14(c) identified in Avellone et al. (2023)'
  )
ggsave('employment-rate-working-age-cog-disab-US-and-no14c.png', width=10, height=7)
  


cog.working.weekly.hours.worked.DT <- fread('sd_chart_builder_data-07-03-2024_03-47.csv', skip=2, header=TRUE)
cog.working.weekly.hours.worked.m.DT <- melt(cog.working.weekly.hours.worked.DT, id='State')
cog.working.weekly.hours.worked.m.DT[ , variable := as.numeric(as.character(variable))]
cog.working.weekly.hours.worked.m.DT[ , is.Maine := State=='Maine']
cog.working.weekly.hours.worked.m.DT[ , is.US := State=='U.S. Total']
ggplot(cog.working.weekly.hours.worked.m.DT, aes(x=variable, y=value, color=State, linewidth=is.Maine, linetype=is.US)) +
  geom_line() +
  ylab('weekly hours worked') +
  xlab('Year') +
  scale_x_continuous(breaks=2010:2022, minor_breaks=NULL) +
  # geom_text_repel(data=~subset(.x, variable==2022), aes(label=State), direction='y')
  geom_text(data=~subset(.x, variable==2022), aes(label=State), hjust=0) +
  scale_y_continuous(breaks=seq(0,45,by=10), minor_breaks=seq(0,45,by=5)) +
  expand_limits(y=0, x=2024)

alldisab.working.age.employed.DT <- fread('sd_chart_builder_data-07-03-2024_03-52.csv', skip=2, header=TRUE)
alldisab.working.age.employed.m.DT <- melt(alldisab.working.age.employed.DT, id='State')
alldisab.working.age.employed.m.DT[ , variable := as.numeric(as.character(variable))]
alldisab.working.age.employed.m.DT[ , is.Maine := State=='Maine']
alldisab.working.age.employed.m.DT[ , is.US := State=='U.S. Total']
ggplot(alldisab.working.age.employed.m.DT, aes(x=variable, y=100*value, color=State, linewidth=is.Maine, linetype=is.US)) +
  geom_line() +
  ylab('% employed among working age ppl w ALL DISAB') +
  xlab('Year') +
  scale_x_continuous(breaks=2010:2022, minor_breaks=NULL) +
  # geom_text_repel(data=~subset(.x, variable==2022), aes(label=State), direction='y')
  geom_text(data=~subset(.x, variable==2022), aes(label=State), hjust=0) +
  scale_y_continuous(breaks=seq(0,100,by=20), minor_breaks=seq(0,100,by=10)) +
  expand_limits(y=0, x=2024)


idd.integrated.employment.percent.DT <- fread('sd_chart_builder_data-07-03-2024_04-49 noquotes-nospaces-noskip.csv', fill=TRUE, header=TRUE)
idd.integrated.employment.percent.m.DT <- melt(idd.integrated.employment.percent.DT, id='State')
idd.integrated.employment.percent.m.DT[ , variable := as.numeric(as.character(variable))]
idd.integrated.employment.percent.m.DT[ , is.Maine := State=='Maine']
idd.integrated.employment.percent.m.DT[ , is.US := State=='U.S. Total']
ggplot(idd.integrated.employment.percent.m.DT, aes(x=variable, y=100*value, color=State, linewidth=is.Maine, linetype=is.US)) +
  geom_line() +
  ylab('IDD agency % integrated employment') +
  xlab('Year') +
  scale_x_continuous(breaks=2010:2022, minor_breaks=NULL) +
  # geom_text_repel(data=~subset(.x, variable==2022), aes(label=State), direction='y')
  geom_text(data=~subset(.x, variable==2022), aes(label=State), hjust=0) +
  scale_y_continuous(breaks=seq(0,100,by=20), minor_breaks=seq(0,100,by=10)) +
  expand_limits(y=0, x=2024)
