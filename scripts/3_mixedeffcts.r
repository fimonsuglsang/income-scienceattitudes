
library(tidyverse)
library(lmerTest)

# this script runs a series of mixed effects multilevel regressions, primarily for supplementary materials.
# objects are saved at the end, and in the objects folder, due to longer computational time

# running "/data/clean.r" is a prerequisite:
# source("scripts/clean.r")

#data for mixed effects regressions:
contdatalv2 %>% 
  #mean centering continous lvl1 predictors within clusters
  group_by(country) %>% 
  mutate(
    age = (age-mean(age, na.rm = T)),
    subcome = subcome-mean(subcome, na.rm = T),
    relcome = relcome-mean(relcome, na.rm = T)
  ) %>% 
  ungroup() %>% 
  #rescaling trust for comparability
  mutate(trust = (trust/sd(trust, na.rm = T))) %>% 
  
  #rescaling all country variables to have 0 be lowest and 1 be highest value
  mutate(
    aggtrust = aggtrust-min(aggtrust, na.rm = T),
    aggtrust = aggtrust/max(aggtrust, na.rm = T),
    
    scispend = scispend-min(scispend, na.rm = T),
    scispend = scispend/max(scispend, na.rm = T),
    
    loggdp = loggdp-min(loggdp, na.rm = T),
    loggdp = loggdp/max(loggdp, na.rm = T),
    
    gini = gini-min(gini, na.rm = T),
    gini = gini/max(gini, na.rm = T),
    
    v2x_polyarchy = v2x_polyarchy-min(v2x_polyarchy, na.rm = T),
    v2x_polyarchy = v2x_polyarchy/max(v2x_polyarchy, na.rm = T),
    
    logmort = logmort-min(logmort, na.rm = T),
    logmort = logmort/max(logmort, na.rm = T)
    
  ) -> tempdata

#recreating regressions from table 1:
lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+subcome+(1+subcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> mixed1

lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+relcome+(1+relcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> mixed2

lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+subcome+relcome+(1+subcome+relcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> mixed3


#including moderation:

#subjective
lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+subcome*aggtrust+(1+subcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4), check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))
) -> lv2aggrsub
lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+subcome*scispend+(1+subcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> lv2spensub
lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+subcome*loggdp+(1+subcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> lv2gdpsub
lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+subcome*gini+(1+subcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> lv2ginisub
lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+subcome*v2x_polyarchy+(1+subcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> lv2demsub
lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+subcome*logmort+(1+subcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> lv2mortsub 
#relative
lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+relcome*aggtrust+(1+relcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> lv2aggrrel
lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+relcome*scispend+(1+relcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> lv2spenrel 
lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+relcome*loggdp+(1+relcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> lv2gdprel
lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+relcome*gini+(1+relcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> lv2ginirel
lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+relcome*v2x_polyarchy+(1+relcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> lv2demrel 
lmer(
  data = tempdata,
  trust~1+sex+age+edu+emp+relcome*logmort+(1+relcome|country/round),
  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=3e4))
) -> lv2mortrel 


save(list = c(ls(pattern = "mixed"), ls(pattern = "lv2\\w")), file = "objects/lv2regs.rdata")

