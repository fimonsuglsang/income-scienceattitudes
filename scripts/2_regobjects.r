
library(tidyverse)
library(fixest)

# this script runs a series of country-wise and country-round-wise regression in a series of loops.
# the first object is (cregs) is saved for the addnalyses at the end.

# running "/data/clean.r" is a prerequisite
# source("scripts/clean.r")


# country regs ------------------------------------------------------------

clist <- merged %>% 
  select(country) %>% 
  distinct()

cregs <- tibble()

#relative income
for(i in 1:145){
  
  c <- as.character(clist[i,])
  
  print(paste0(i, " / 145"))
  
  if(filter(contdata, country == c) %>% filter(!is.na(relcome)) %>% count() == 0){ #check for predictor
    print("no obs")
  } else{ 
    if(filter(contdata, country == c) %>% select(round) %>% distinct() %>% count() == 1){ #check for two rounds
      
      #Single round regression
      bind_rows(cregs,
                broom::tidy(
                  lm(
                    data = contdata %>% filter(country == paste(c)) %>% filter(!is.na(relcome)),
                    trust~sex+age+edu+emp+relcome
                  )
                ) %>% 
                  filter(term == "relcome") %>% 
                  mutate(country = paste(c))
      ) -> cregs
      
    } else {
      
      #Double round regression
      bind_rows(cregs,
                broom::tidy(
                  lm(
                    data = contdata %>% filter(country == paste(c)) %>% filter(!is.na(relcome)),
                    trust~sex+age+edu+emp+relcome+round
                  )
                ) %>% 
                  filter(term == "relcome") %>% 
                  mutate(country = paste(c))
      ) -> cregs
    }}}

#subjective income
for(i in 1:145){
  
  c <- as.character(clist[i,])
  
  print(paste0(i, " / 145"))
  
  if(filter(contdata, country == c) %>% filter(!is.na(subcome)) %>% count() == 0){ #check for predictor
    print("no obs")
  } else{ 
    if(filter(contdata, country == c) %>% select(round) %>% distinct() %>% count() == 1){ #check for two rounds
      
      #Single round regression
      bind_rows(cregs,
                broom::tidy(
                  lm(
                    data = contdata %>% filter(country == paste(c)) %>% filter(!is.na(subcome)),
                    trust~sex+age+edu+emp+subcome
                  )
                ) %>% 
                  filter(term == "subcome") %>% 
                  mutate(country = paste(c))
      ) -> cregs
      
    } else {
      
      #Double round regression
      bind_rows(cregs,
                broom::tidy(
                  lm(
                    data = contdata %>% filter(country == paste(c)) %>% filter(!is.na(subcome)),
                    trust~sex+age+edu+emp+subcome+round
                  )
                ) %>% 
                  filter(term == "subcome") %>% 
                  mutate(country = paste(c))
      ) -> cregs
    }}}


# country round regs ------------------------------------------------------

cregs18 <- tibble()
cregs20 <- tibble()


#relative income, 18
for(i in 1:145){
  
  c <- as.character(clist[i,])
  
  print(paste0(i, " / 145"))
  
  if(filter(contdata18, country == c) %>% filter(!is.na(relcome)) %>% count() == 0){ #check for predictor
    print("no obs")
  } else{
      
      #Regression
      bind_rows(cregs18,
                broom::tidy(
                  lm(
                    data = contdata18 %>% filter(country == paste(c)) %>% filter(!is.na(relcome)),
                    trust~sex+age+edu+emp+relcome
                  )
                ) %>% 
                  filter(term == "relcome") %>% 
                  mutate(country = paste(c))
      ) -> cregs18
}}
    
#subjective income, 18
for(i in 1:145){
  
  c <- as.character(clist[i,])
  
  print(paste0(i, " / 145"))
  
  if(filter(contdata18, country == c) %>% filter(!is.na(subcome)) %>% count() == 0){ #check for predictor
    print("no obs")
  } else{
    
    #Regression
    bind_rows(cregs18,
              broom::tidy(
                lm(
                  data = contdata18 %>% filter(country == paste(c)) %>% filter(!is.na(subcome)),
                  trust~sex+age+edu+emp+subcome
                )
              ) %>% 
                filter(term == "subcome") %>% 
                mutate(country = paste(c))
    ) -> cregs18
  }}


#relative income, 20
for(i in 1:145){
  
  c <- as.character(clist[i,])
  
  print(paste0(i, " / 145"))
  
  if(filter(contdata20, country == c) %>% filter(!is.na(relcome)) %>% count() == 0){ #check for predictor
    print("no obs")
  } else{
    
    #Regression
    bind_rows(cregs20,
              broom::tidy(
                lm(
                  data = contdata20 %>% filter(country == paste(c)) %>% filter(!is.na(relcome)),
                  trust~sex+age+edu+emp+relcome
                )
              ) %>% 
                filter(term == "relcome") %>% 
                mutate(country = paste(c))
    ) -> cregs20
  }}

#subjective income, 20
for(i in 1:145){
  
  c <- as.character(clist[i,])
  
  print(paste0(i, " / 145"))
  
  if(filter(contdata20, country == c) %>% filter(!is.na(subcome)) %>% count() == 0){ #check for predictor
    print("no obs")
  } else{
    
    #Regression
    bind_rows(cregs20,
              broom::tidy(
                lm(
                  data = contdata20 %>% filter(country == paste(c)) %>% filter(!is.na(subcome)),
                  trust~sex+age+edu+emp+subcome
                )
              ) %>% 
                filter(term == "subcome") %>% 
                mutate(country = paste(c))
    ) -> cregs20
  }}


# region regs -------------------------------------------------------------

whoregions %>% select(region) %>%  distinct() -> rlist

rregs <- tibble()

for(i in 1:6){
  r <- as.character(rlist[i,])
  
  bind_rows(rregs,
            broom::tidy(
              feols(
                data = left_join(contdata, whoregions) %>% filter(region == paste(r)), 
                trust~sex+age+edu+emp+round+relcome|country, se = "cluster")
            ) %>% 
              filter(term == "relcome") %>% 
              mutate(region = paste(r)),
            
            broom::tidy(
              feols(
                data = left_join(contdata, whoregions) %>% filter(region == paste(r)), 
                trust~sex+age+edu+emp+round+subcome|country, se = "cluster")
            ) %>% 
              filter(term == "subcome") %>% 
              mutate(region = paste(r))
  ) -> rregs
}


# interactions ----------------------------------------------------

indiinterz <- tibble()

#age
bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(age < 30), trust~sex+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Under 30"),
            
            broom::tidy(feols(data = contdata %>% filter(age >= 30 & age <=60), trust~sex+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "30 to 60"),
            
            broom::tidy(feols(data = contdata %>% filter(age <60), trust~sex+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Over 60")
          ) %>% 
            mutate(var = "Age") %>% 
            filter(term == "relcome")
          
) -> indiinterz

bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(age < 30), trust~sex+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Under 30"),
            
            broom::tidy(feols(data = contdata %>% filter(age >= 30 & age <=60), trust~sex+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "30 to 60"),
            
            broom::tidy(feols(data = contdata %>% filter(age <60), trust~sex+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Over 60")
          ) %>% 
            mutate(var = "Age") %>% 
            filter(term == "subcome")
          
) -> indiinterz

#sex
bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(sex == "Male"), trust~age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Male"),
            
            broom::tidy(feols(data = contdata %>% filter(sex == "Female"), trust~age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Female")
            
          ) %>% 
            mutate(var = "Sex") %>% 
            filter(term == "relcome")
          
) -> indiinterz

bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(sex == "Male"), trust~age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Male"),
            
            broom::tidy(feols(data = contdata %>% filter(sex == "Female"), trust~age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Female")
            
          ) %>% 
            mutate(var = "Sex") %>% 
            filter(term == "subcome")
          
) -> indiinterz

#emp
bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(emp == "Out of workforce"), trust~age+edu+sex+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Out of workforce"),
            
            broom::tidy(feols(data = contdata %>% filter(emp == "Unemployed"), trust~age+edu+sex+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Unemployed"),
            
            broom::tidy(feols(data = contdata %>% filter(emp == "Employed part time want full time"), trust~age+edu+sex+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Part time (wants full)"),
            
            broom::tidy(feols(data = contdata %>% filter(emp == "Employed part time do not want full time"), trust~age+edu+sex+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Part time (does not)"),
            
            broom::tidy(feols(data = contdata %>% filter(emp == "Employed full time for an employer"), trust~age+edu+sex+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Full time (employer)"),
            
            broom::tidy(feols(data = contdata %>% filter(emp == "Employed full time for self"), trust~age+edu+sex+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Full time (self)")
            
          ) %>% 
            mutate(var = "Employment") %>% 
            filter(term == "relcome")
          
) -> indiinterz

bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(emp == "Out of workforce"), trust~age+edu+sex+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Out of workforce"),
            
            broom::tidy(feols(data = contdata %>% filter(emp == "Unemployed"), trust~age+edu+sex+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Unemployed"),
            
            broom::tidy(feols(data = contdata %>% filter(emp == "Employed part time want full time"), trust~age+edu+sex+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Part time (wants full)"),
            
            broom::tidy(feols(data = contdata %>% filter(emp == "Employed part time do not want full time"), trust~age+edu+sex+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Part time (does not)"),
            
            broom::tidy(feols(data = contdata %>% filter(emp == "Employed full time for an employer"), trust~age+edu+sex+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Full time (employer)"),
            
            broom::tidy(feols(data = contdata %>% filter(emp == "Employed full time for self"), trust~age+edu+sex+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Full time (self)")
            
          ) %>% 
            mutate(var = "Employment") %>% 
            filter(term == "subcome")
          
) -> indiinterz

#edu
bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(edu == "Elementary"), trust~age+sex+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Elementary"),
            
            broom::tidy(feols(data = contdata %>% filter(edu == "Secondary"), trust~age+sex+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Secondary"),
            
            broom::tidy(feols(data = contdata %>% filter(edu == "Tertiary"), trust~age+sex+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Tertiary")
            
          ) %>% 
            mutate(var = "Education") %>% 
            filter(term == "relcome")
          
) -> indiinterz

bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(edu == "Elementary"), trust~age+sex+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Elementary"),
            
            broom::tidy(feols(data = contdata %>% filter(edu == "Secondary"), trust~age+sex+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Secondary"),
            
            broom::tidy(feols(data = contdata %>% filter(edu == "Tertiary"), trust~age+sex+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Tertiary")
            
          ) %>% 
            mutate(var = "Education") %>% 
            filter(term == "subcome")
          
) -> indiinterz

#round
bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(round == "WGM18"), trust~age+sex+emp+edu+relcome|country, se = "cluster")) %>% 
              mutate(value = "2018"),
            
            broom::tidy(feols(data = contdata %>% filter(round == "WGM20"), trust~age+sex+emp+edu+relcome|country, se = "cluster")) %>% 
              mutate(value = "2020")
            
          ) %>% 
            mutate(var = "Round") %>% 
            filter(term == "relcome")
          
) -> indiinterz

bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(round == "WGM18"), trust~age+sex+emp+edu+subcome|country, se = "cluster")) %>% 
              mutate(value = "2018"),
            
            broom::tidy(feols(data = contdata %>% filter(round == "WGM20"), trust~age+sex+emp+edu+subcome|country, se = "cluster")) %>% 
              mutate(value = "2020")
            
          ) %>% 
            mutate(var = "Round") %>% 
            filter(term == "subcome")
          
) -> indiinterz

#sci eff
bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(scieff == "None"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "None"),
            
            broom::tidy(feols(data = contdata %>% filter(scieff == "Not much"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Not much"),
            
            broom::tidy(feols(data = contdata %>% filter(scieff == "Some"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Some"),
            
            broom::tidy(feols(data = contdata %>% filter(scieff == "A lot"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "A lot")
            
          ) %>% 
            mutate(var = "Science\nEfficacy") %>% 
            filter(term == "relcome")
          
) -> indiinterz

bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(scieff == "None"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "None"),
            
            broom::tidy(feols(data = contdata %>% filter(scieff == "Not much"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Not much"),
            
            broom::tidy(feols(data = contdata %>% filter(scieff == "Some"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Some"),
            
            broom::tidy(feols(data = contdata %>% filter(scieff == "A lot"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "A lot")
            
          ) %>% 
            mutate(var = "Science\nEfficacy") %>% 
            filter(term == "subcome")
          
) -> indiinterz


#ntrust
bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(ntrust == "A lot"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "A lot"),
            
            broom::tidy(feols(data = contdata %>% filter(ntrust == "Some"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Some"),
            
            broom::tidy(feols(data = contdata %>% filter(ntrust == "Not much"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Not much"),
            
            broom::tidy(feols(data = contdata %>% filter(ntrust == "Not at all"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Not at all")
            
          ) %>% 
            mutate(var = "Trust\nNeighbourhood") %>% 
            filter(term == "relcome")
          
) -> indiinterz

bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(ntrust == "A lot"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "A lot"),
            
            broom::tidy(feols(data = contdata %>% filter(ntrust == "Some"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Some"),
            
            broom::tidy(feols(data = contdata %>% filter(ntrust == "Not much"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Not much"),
            
            broom::tidy(feols(data = contdata %>% filter(ntrust == "Not at all"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Not at all")
            
          ) %>% 
            mutate(var = "Trust\nNeighbourhood") %>% 
            filter(term == "subcome")
          
) -> indiinterz

#gtrust
bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(gtrust == "A lot"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "A lot"),
            
            broom::tidy(feols(data = contdata %>% filter(gtrust == "Some"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Some"),
            
            broom::tidy(feols(data = contdata %>% filter(gtrust == "Not much"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Not much"),
            
            broom::tidy(feols(data = contdata %>% filter(gtrust == "Not at all"), trust~sex+age+edu+emp+round+relcome|country, se = "cluster")) %>% 
              mutate(value = "Not at all")
            
          ) %>% 
            mutate(var = "Trust\nGovernment") %>% 
            filter(term == "relcome")
          
) -> indiinterz

bind_rows(indiinterz,
          
          bind_rows(
            broom::tidy(feols(data = contdata %>% filter(gtrust == "A lot"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "A lot"),
            
            broom::tidy(feols(data = contdata %>% filter(gtrust == "Some"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Some"),
            
            broom::tidy(feols(data = contdata %>% filter(gtrust == "Not much"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Not much"),
            
            broom::tidy(feols(data = contdata %>% filter(gtrust == "Not at all"), trust~sex+age+edu+emp+round+subcome|country, se = "cluster")) %>% 
              mutate(value = "Not at all")
            
          ) %>% 
            mutate(var = "Trust\nGovernment") %>% 
            filter(term == "subcome")
          
) -> indiinterz


save(cregs, file = "objects/cregs.rdata")


# cleanup -----------------------------------------------------------------

rm(c, i, r, clist, rlist)
