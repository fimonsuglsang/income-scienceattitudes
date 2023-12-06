

library(tidyverse)
library(fixest)


# this script runs individual level interactions but for each region.
# object is is saved for the addnalyses at the end.

# running "/data/clean.r" is a prerequisite
# source("scripts/clean.r")



whoregions %>% select(region) %>%  distinct() -> rlist

indregio <-
  tibble(
    part = as.character(),
    term = as.character(),
    name = factor(),
    est = as.numeric(),
    se = as.numeric(),
    region = as.character()
  )

for(i in 1:6){
  
  
  print(paste0("Now running split samples for ", rlist[i,], ", ", i, "/7"))
  
  left_join(contdata, whoregions) %>% filter(region == paste(rlist[i,])) -> contdatatemp
  
  
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(age < 30), trust~sex+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Under 30"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(age >= 30 & age <=60), trust~sex+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "30 to 60"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(age <60), trust~sex+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Over 60")
            ) %>% 
              mutate(var = "Age", region = paste(rlist[i,])) %>% 
              filter(term == "relcome")
            
  ) -> indregio
  
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(age < 30), trust~sex+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Under 30"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(age >= 30 & age <=60), trust~sex+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "30 to 60"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(age <60), trust~sex+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Over 60")
            ) %>% 
              mutate(var = "Age", region = paste(rlist[i,])) %>% 
              filter(term == "subcome")
            
  ) -> indregio
  
  #sex
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(sex == "Male"), trust~age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Male"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(sex == "Female"), trust~age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Female")
              
            ) %>% 
              mutate(var = "Sex", region = paste(rlist[i,])) %>% 
              filter(term == "relcome")
            
  ) -> indregio
  
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(sex == "Male"), trust~age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Male"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(sex == "Female"), trust~age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Female")
              
            ) %>% 
              mutate(var = "Sex", region = paste(rlist[i,])) %>% 
              filter(term == "subcome")
            
  ) -> indregio
  
  #emp
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(emp == "Out of workforce"), trust~age+edu+sex+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Out of workforce"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(emp == "Unemployed"), trust~age+edu+sex+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Unemployed"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(emp == "Employed part time want full time"), trust~age+edu+sex+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Part time (wants full)"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(emp == "Employed part time do not want full time"), trust~age+edu+sex+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Part time (does not)"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(emp == "Employed full time for an employer"), trust~age+edu+sex+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Full time (employer)"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(emp == "Employed full time for self"), trust~age+edu+sex+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Full time (self)")
              
            ) %>% 
              mutate(var = "Employment", region = paste(rlist[i,])) %>% 
              filter(term == "relcome")
            
  ) -> indregio
  
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(emp == "Out of workforce"), trust~age+edu+sex+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Out of workforce"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(emp == "Unemployed"), trust~age+edu+sex+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Unemployed"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(emp == "Employed part time want full time"), trust~age+edu+sex+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Part time (wants full)"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(emp == "Employed part time do not want full time"), trust~age+edu+sex+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Part time (does not)"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(emp == "Employed full time for an employer"), trust~age+edu+sex+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Full time (employer)"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(emp == "Employed full time for self"), trust~age+edu+sex+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Full time (self)")
              
            ) %>% 
              mutate(var = "Employment", region = paste(rlist[i,])) %>% 
              filter(term == "subcome")
            
  ) -> indregio
  
  #edu
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(edu == "Elementary"), trust~age+sex+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Elementary"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(edu == "Secondary"), trust~age+sex+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Secondary"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(edu == "Tertiary"), trust~age+sex+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Tertiary")
              
            ) %>% 
              mutate(var = "Education", region = paste(rlist[i,])) %>% 
              filter(term == "relcome")
            
  ) -> indregio
  
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(edu == "Elementary"), trust~age+sex+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Elementary"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(edu == "Secondary"), trust~age+sex+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Secondary"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(edu == "Tertiary"), trust~age+sex+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Tertiary")
              
            ) %>% 
              mutate(var = "Education", region = paste(rlist[i,])) %>% 
              filter(term == "subcome")
            
  ) -> indregio
  
  #round
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(round == "WGM18"), trust~age+sex+emp+edu+relcome|country, se = "cluster")) %>% 
                mutate(value = "2018"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(round == "WGM20"), trust~age+sex+emp+edu+relcome|country, se = "cluster")) %>% 
                mutate(value = "2020")
              
            ) %>% 
              mutate(var = "Round", region = paste(rlist[i,])) %>% 
              filter(term == "relcome")
            
  ) -> indregio
  
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(round == "WGM18"), trust~age+sex+emp+edu+subcome|country, se = "cluster")) %>% 
                mutate(value = "2018"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(round == "WGM20"), trust~age+sex+emp+edu+subcome|country, se = "cluster")) %>% 
                mutate(value = "2020")
              
            ) %>% 
              mutate(var = "Round", region = paste(rlist[i,])) %>% 
              filter(term == "subcome")
            
  ) -> indregio
  
  #sci eff
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(scieff == "None"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "None"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(scieff == "Not much"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Not much"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(scieff == "Some"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Some"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(scieff == "A lot"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "A lot")
              
            ) %>% 
              mutate(var = "Science\nEfficacy", region = paste(rlist[i,])) %>% 
              filter(term == "relcome")
            
  ) -> indregio
  
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(scieff == "None"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "None"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(scieff == "Not much"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Not much"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(scieff == "Some"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Some"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(scieff == "A lot"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "A lot")
              
            ) %>% 
              mutate(var = "Science\nEfficacy", region = paste(rlist[i,])) %>% 
              filter(term == "subcome")
            
  ) -> indregio
  
  
  #ntrust
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(ntrust == "A lot"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "A lot"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(ntrust == "Some"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Some"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(ntrust == "Not much"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Not much"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(ntrust == "Not at all"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Not at all")
              
            ) %>% 
              mutate(var = "Trust\nNeighbourhood", region = paste(rlist[i,])) %>% 
              filter(term == "relcome")
            
  ) -> indregio
  
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(ntrust == "A lot"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "A lot"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(ntrust == "Some"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Some"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(ntrust == "Not much"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Not much"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(ntrust == "Not at all"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Not at all")
              
            ) %>% 
              mutate(var = "Trust\nNeighbourhood", region = paste(rlist[i,])) %>% 
              filter(term == "subcome")
            
  ) -> indregio
  
  #gtrust
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(gtrust == "A lot"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "A lot"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(gtrust == "Some"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Some"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(gtrust == "Not much"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Not much"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(gtrust == "Not at all"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
                mutate(value = "Not at all")
              
            ) %>% 
              mutate(var = "Trust\nGovernment", region = paste(rlist[i,])) %>% 
              filter(term == "relcome")
            
  ) -> indregio
  
  bind_rows(indregio,
            
            bind_rows(
              broom::tidy(feols(data = contdatatemp %>% filter(gtrust == "A lot"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "A lot"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(gtrust == "Some"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Some"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(gtrust == "Not much"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Not much"),
              
              broom::tidy(feols(data = contdatatemp %>% filter(gtrust == "Not at all"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
                mutate(value = "Not at all")
              
            ) %>% 
              mutate(var = "Trust\nGovernment", region = paste(rlist[i,])) %>% 
              filter(term == "subcome")
            
  ) -> indregio
  
  
}

save(indregio, file = "objects/indregio.rdata")



