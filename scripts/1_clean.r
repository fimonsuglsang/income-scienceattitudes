
library(tidyverse)


#reading the datasets
readxl::read_xlsx("./data/wgm2018.xlsx", sheet = 2) -> wgm18_raw
read_csv("./data/wgm2020.csv") -> wgm20_raw

#wgm18 data extraction
wgm18_raw %>%
  #NAs recoded
  mutate(
    across(c(Q1, Q11A, Q11B, Q11C, Q11E, Q12, Q13, Q14A, Q15A, Q14B, Q15B, Q16, Q19), \(x) na_if(x, 99)),
    across(c(Q1, Q11A, Q11B, Q11C, Q11E, Q12, Q13, Q14A, Q15A, Q14B, Q15B, Q16, Q19), \(x) na_if(x, 98))
  ) %>% 
  #countries and dataset indicator
  mutate(
    round = "WGM18",
    country = recode(WP5, "1"= "United States", "2"= "Egypt", "3"= "Morocco", "4"= "Lebanon", "5"= "Saudi Arabia", "6"= "Jordan", "8"= "Turkey", "9"= "Pakistan", "10"= "Indonesia", "11"= "Bangladesh", "12"= "United Kingdom", "13"= "France", "14"= "Germany", "15"= "Netherlands", "16"= "Belgium", "17"= "Spain", "18"= "Italy", "19"= "Poland", "20"= "Hungary", "21"= "Czech Republic", "22"= "Romania", "23"= "Sweden", "24"= "Greece", "25"= "Denmark", "26"= "Iran", "28"= "Singapore", "29"= "Japan", "30"= "China", "31"= "India", "32"= "Venezuela", "33"= "Brazil", "34"= "Mexico", "35"= "Nigeria", "36"= "Kenya", "37"= "Tanzania", "38"= "Israel", "39"= "Palestinian Territories", "40"= "Ghana", "41"= "Uganda", "42"= "Benin", "43"= "Madagascar", "44"= "Malawi", "45"= "South Africa", "46"= "Canada", "47"= "Australia", "48"= "Philippines", "49"= "Sri Lanka", "50"= "Vietnam", "51"= "Thailand", "52"= "Cambodia", "53"= "Laos", "54"= "Myanmar", "55"= "New Zealand", "57"= "Botswana", "60"= "Ethiopia", "61"= "Mali", "62"= "Mauritania", "63"= "Mozambique", "64"= "Niger", "65"= "Rwanda", "66"= "Senegal", "67"= "Zambia", "68"= "South Korea", "69"= "Taiwan", "70"= "Afghanistan", "71"= "Belarus", "72"= "Georgia", "73"= "Kazakhstan", "74"= "Kyrgyzstan", "75"= "Moldova", "76"= "Russia", "77"= "Ukraine", "78"= "Burkina Faso", "79"= "Cameroon", "80"= "Sierra Leone", "81"= "Zimbabwe", "82"= "Costa Rica", "83"= "Albania", "84"= "Algeria", "87"= "Argentina", "88"= "Armenia", "89"= "Austria", "90"= "Azerbaijan", "96"= "Bolivia", "97"= "Bosnia and Herzegovina", "99"= "Bulgaria", "100"= "Burundi", "103"= "Chad", "104"= "Chile", "105"= "Colombia", "106"= "Comoros", "108"= "Republic of Congo", "109"= "Croatia", "111"= "Cyprus", "114"= "Dominican Republic", "115"= "Ecuador", "116"= "El Salvador", "119"= "Estonia", "121"= "Finland", "122"= "Gabon", "124"= "Guatemala", "125"= "Guinea", "128"= "Haiti", "129"= "Honduras", "130"= "Iceland", "131"= "Iraq", "132"= "Ireland", "134"= "Ivory Coast", "137"= "Kuwait", "138"= "Latvia", "140"= "Liberia", "141"= "Libya", "143"= "Lithuania", "144"= "Luxembourg", "145"= "Macedonia", "146"= "Malaysia", "148"= "Malta", "150"= "Mauritius", "153"= "Mongolia", "154"= "Montenegro", "155"= "Namibia", "157"= "Nepal", "158"= "Nicaragua", "160"= "Norway", "163"= "Panama", "164"= "Paraguay", "165"= "Peru", "166"= "Portugal", "173"= "Serbia", "175"= "Slovakia", "176"= "Slovenia", "183"= "Eswatini", "184"= "Switzerland", "185"= "Tajikistan", "186"= "The Gambia", "187"= "Togo", "190"= "Tunisia", "191"= "Turkmenistan", "193"= "United Arab Emirates", "194"= "Uruguay", "195"= "Uzbekistan", "197"= "Yemen", "198"= "Kosovo", "202"= "Northern Cyprus")
  ) %>%
  #selecting and renaming vars
  select(
    round,
    country,
    relcome = Household_Income,
    subcome = Subjective_Income,
    age = Age,
    scieff = Q1,
    sex = Gender,
    edu = Education,
    emp = EMP_2010,
    i1 = Q11C, 
    i2 = Q11E, 
    i3 = Q12, 
    i4 = Q13, 
    i5 = Q14A, 
    i6 = Q16,
    i7 = Q19,
    i_integrity = Q14B,
    ntrust = Q11A,
    gtrust = Q11B
  ) -> wgm18

#wgm20 data extraction
wgm20_raw %>% 
  #NAs recoded
  mutate(
    across(c(W1, W5A, W5B, W5C, W5E, W6, W7A, W7B, W8, W10, Education), \(x) na_if(x, 99))
  ) %>% 
  #dataset indicator
  mutate(
    round = "WGM20"
  ) %>% 
  
  #selecting and renaming vars
  select(
    round,
    country = COUNTRYNEW,
    relcome = Household_Income,
    subcome = Subjective_Income,
    age = Age,
    scieff = W1,
    sex = Gender,
    edu = Education,
    emp = EMP_2010,
    i1 = W5C, 
    i2 = W5E, 
    i3 = W6, 
    i4 = W7A, 
    i5 = W7B, 
    i6 = W8, 
    i7 = W10,
    ntrust = W5A,
    gtrust = W5B
  ) -> wgm20

#merging data
bind_rows(wgm18, wgm20) %>% 
  #changing range of i6 and i7
  mutate(
    i6 = case_when(i6 == 1 ~ 1, i6 == 2 ~ 2.5, i6 == 3 ~ 4),
    i7 = case_when(i7 == 1 ~ 1, i7 == 3 ~ 2.5, i7 == 2 ~ 4),
    
    #Labels for ordinal income vars
    relcome = factor(relcome, labels = c("Poorest 20%", "Second 20%", "Middle 20%", "Fourth 20%", "Richest 20%")),
    relcome = fct_rev(relcome),
    subcome = case_when(
      subcome == 1 ~ "Comfortably",
      subcome == 2 ~ "Getting by",
      subcome == 3 ~ "Difficult",
      subcome == 4 ~ "Difficult"
    ),
    subcome = fct_relevel(subcome, "Difficult", after = 2),
    
    #labels for controls, moderators and countries.
    scieff = factor(scieff, labels = c("A lot", "Some", "Not much", "None")),
    sex = factor(sex, label = c("Male", "Female")),
    edu = factor(edu, label = c("Elementary", "Secondary", "Tertiary")),
    emp = factor(emp, label = c("Employed full time for an employer",
                                "Employed full time for self",
                                "Employed part time do not want full time",
                                "Unemployed",
                                "Employed part time want full time",
                                "Out of workforce")),
    country = case_when(
      country == "Bosnia Herzegovina" ~ "Bosnia and Herzegovina",
      country == "Northern Cyprus" ~ "Cyprus",
      country == "Macedonia" ~ "North Macedonia",
      country == "Congo Brazzaville" ~ "Congo",
      country == "Republic of Congo" ~ "Congo",
      T ~ country
    ),
    ntrust = factor(ntrust, labels = c("A lot", "Some", "Not much", "Not at all")),
    gtrust = factor(gtrust, labels = c("A lot", "Some", "Not much", "Not at all"))
  ) -> merged 

# trust var generation
merged %>% 
  mutate(
    trust = rowMeans(select(.,i1,i3,i4,i5), na.rm = T),
    trust = ((4-trust)/3)*100
  ) -> merged

# getting region data
read_csv("./data/whoregions.csv") %>% 
  select(country = 1, region = 4, code = 2) %>% 
  mutate(country = recode(country,
                          "Czechia" = "Czech Republic",
                          "Cote d'Ivoire" = "Ivory Coast",
                          "Gambia" = "The Gambia")
         
  ) %>% 
  bind_rows(tibble(country = c("Taiwan", "Hong Kong", "Palestinian Territories", "Kosovo"), 
                   region = c("Western Pacific", "Western Pacific", "Eastern Mediterranean", "Europe"),
                   code = c("TWN", "HKG", "PSE", "XKX")))  -> whoregions

# separate versions of the data
merged %>% filter(round == "WGM18") -> data18
merged %>% filter(round == "WGM20") -> data20

# getting a version with continuous income vars
merged %>% 
  mutate(
    relcome = (5-as.numeric(relcome))/4,
    subcome = (3-as.numeric(subcome))/2
  ) -> contdata

# separate versions of continous version
contdata %>% filter(round == "WGM18") -> contdata18
contdata %>% filter(round == "WGM20") -> contdata20

# country level data merged together
whoregions %>% 
  left_join(
    
    #gdppc
    left_join(
      read_csv("./data/wbgdppc.csv", skip = 4) %>% 
        select(code = "Country Code", gdppc18 = "2018", gdppc20 = "2020"),
    #infant mortality  
      read_csv("./data/wbinfmort.csv", skip = 4) %>% 
        select(code = "Country Code", infmort18 = "2018", infmort20 = "2020")
    ) %>% 
    #gini
      left_join(
        read_csv("./data/wbgini.csv", skip = 4) %>% 
          pivot_longer(5:66) %>% 
          mutate(name = as.numeric(name)) %>% 
          filter(name>2016) %>% 
          select(code = "Country Code", name, value) %>% 
          mutate(value = case_when(is.na(value) == T ~ lag(value), T ~ value)) %>% 
          mutate(value = case_when(is.na(value) == T ~ lag(value), T ~ value)) %>% #allow for 2 years lag 
          filter(name == 2018 | name == 2020) %>% 
          pivot_wider() %>% 
          rename(gini18 = "2018", gini20 = "2020")
      ) %>% 
    #science spending
      left_join(
        read_csv("./data/wbscispend.csv", skip = 4) %>% 
          pivot_longer(5:66) %>% 
          mutate(name = as.numeric(name)) %>% 
          filter(name>2016) %>% 
          select(code = "Country Code", name, value) %>% 
          mutate(value = case_when(is.na(value) == T ~ lag(value), T ~ value)) %>% 
          mutate(value = case_when(is.na(value) == T ~ lag(value), T ~ value)) %>% #allow for 2 years lag 
          filter(name == 2018 | name == 2020) %>% 
          pivot_wider() %>% 
          rename(scispend18 = "2018", scispend20 = "2020")
      ) %>% 
      
      #fixing up all world bank data
      pivot_longer(2:9)
      ) %>% 
      separate(name, c("var", "round"), -2) %>% 
      mutate(round = paste0("20", round)) %>% 
      pivot_wider(names_from = var, values_from = value) %>% 

  #joining vdem score
  
  left_join(
    read_csv("./data/vdem.csv") %>% 
      filter(year == 2018 | year == 2020) %>% 
      select(code = "country_text_id", round = "year", v2x_polyarchy) %>% 
      mutate(round = as.character(round))
  ) %>% 

  #and country level trust data
  left_join(
    merged %>% 
      group_by(country, round) %>% 
      summarise(
        vtrust = sd(trust, na.rm = T),
        aggtrust = mean(trust, na.rm = T),
        .groups = "drop"
      ) %>% 
      mutate(round = str_replace(round, "WGM", "20"))
  ) %>% 
  
  #creating log'd variables
  mutate(
    loggdp = log(gdppc),
    logmort = log(infmort)
  ) -> lv2

# merging continuous income data, and country level data
left_join(contdata %>% mutate(round = str_replace(round, "WGM", "20")), lv2) -> contdatalv2




