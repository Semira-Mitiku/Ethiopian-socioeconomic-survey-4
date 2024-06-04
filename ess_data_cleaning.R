# cleaning ess4
#loading packages
library(readr) # reading in csv data
library(here) # Managing folder links
library(janitor) # cleaning names of variables
library(dplyr) # Workhorse for data manipulation
library(stringr) # string data manipulation
library(tidyr) # cleaning
library(ggplot2) #
source("CEPHaStat_3.R") # Functions for the plots

# data sets
ess_date <- read_csv(here("sect_cover_hh_w4.csv"))
ess_unit_cf <-read.csv(here("Conversion factor.csv"))
ess_afe <-read.csv(here("afe_ame.csv"))
ess_gps <- read.csv(here("ETH_HouseholdGeovariables_Y4.csv"))
ess_wealth <- read_csv(here("cons_agg_w4.csv"))
ess_cons <- read_csv(here("sect6a_hh_w4.csv")) |> 
select("household_id", "item_cd", "saq14", "pw_w4", "saq01",  "saq02", "saq03", "s6aq01_os", "s6aq01", "s6aq02a", "s6aq02b", "s6aq02b_os", "s6aq04") |>
rename(urbanRural = saq14,
       zone = saq02,
       woreda = saq03,
       otherFood = s6aq01_os,
       consumedYN = s6aq01,
       consQnty = s6aq02a,
       othermeasuringUnit = s6aq02b_os,
       birrPurchasedFood = s6aq04) |>
filter(consumedYN == "1. YES")

# unit conversion factor
# already cleaned on excel for shiro unit_cd 183,182,181,172,171,153,152,151,113,111,72,71,8,4
# Make the conversion factors long format
ess_unit_cf <- ess_unit_cf |> 
mutate(unit_cd = as.character(unit_cd),
       food_cd = as.character(food_cd)) |> 
  tidyr::pivot_longer(cols=c("nat_ave","tigray","afar","amhara","oromia","benig","snnp","gambela","soddhar"),names_to = "region",values_to = "conversion_factor") |> 
# adding region code to the conversion factor
  mutate(regionCF = case_when(region =="tigray" ~ "1",
                              region == "afar" ~ "2",
                              region == "amhara" ~ "3",
                              region == "oromia" ~ "4",
                              region == "benig" ~ "6",
                              region == "snnp" ~ "7",
                              region == "gambela" ~ "12",
                              region == "nat_ave" ~ "14",
                              region == "soddhar" ~ "90",
                              
  )) 

#changing some cf with the salt cf & EFBDG
ess_unit_cf <- ess_unit_cf |> 
  mutate(conversion_factor= case_when(unit=="Gram"~0.001,
                                      unit=="Millilitres"~0.001,
                                      TRUE~conversion_factor))
ess_unit_cf <- ess_unit_cf |> 
  mutate(conversion_factor= case_when(food_cd==901 & unit_cd==141 ~0.3,
                                      food_cd==901 & unit_cd==142 ~0.37,
                                      food_cd==901 & unit_cd==143 ~0.4,
                                      food_cd==902 & unit_cd==141 ~0.1,
                                      food_cd==902 & unit_cd==142 ~0.15,
                                      food_cd==902 & unit_cd==143 ~0.3,
                                      food_cd==802 & unit_cd==141 ~0.2,
                                      food_cd==802 & unit_cd==142 ~0.25,
                                      food_cd==802 & unit_cd==143 ~0.5,
                                      food_cd==403 & unit_cd==151 ~0.248,
                                      food_cd==403 & unit_cd==153 ~0.248,
                                      food_cd==403 & unit_cd==71 ~1.8,
                                      food_cd==403 & unit_cd==8 ~1.681,
                                      food_cd==406 & unit_cd==141 ~0.02,
                                      food_cd==406 & unit_cd==142 ~0.03,
                                      food_cd==406 & unit_cd==141 ~0.045,
                                      food_cd==401 & unit_cd==102 ~1.37,
                                      TRUE~conversion_factor))


### Processing the consumption data

# converting other food to standard food list
ess_cons <- ess_cons |> 
  mutate(item_cd = case_when(otherFood %in% c("Buna" ,"buna") ~ "801. Coffee",
                             otherFood == "abish" ~ "208. Fenugreek",
                             otherFood == "Amicho" ~ "603. Bula",
                             otherFood == "Beer" ~ "804. Beer",
                             otherFood == "boloke" ~ "205. Haricot Beans",
                             otherFood == "Hayat" ~ "708. Oils (processed)",
                             otherFood %in% c("kasa (tematem )" ,"kasa (tematem)" ,"Kasa (tematem)" ,"kasea (tematem)") ~ "409. Tomato",
                             otherFood %in% c("lewuz" ,"ochonli") ~ "206. Ground nuts",
                             otherFood == "menderen" ~ "502. Orange",
                             otherFood == "selit" ~ "303. SESAME",
                             otherFood %in% c("Tbis" ,"tibs siga") ~ "701. Goat & mutton meat",
                             otherFood == "Yegelila Shinkurt" ~ "401. Onion",
                             otherFood == "Yemebtebtew weteti" ~ "705. Milk",
                             otherFood == "Yeselitzeyt" ~ "708. Oils (processed)",
                             #otherFood == "apple" ~ "1001. Pineapple", # to add food items
                             TRUE~ item_cd)) 



#converting non standard units to standard units
ess_cons <- ess_cons |> 
  mutate(s6aq02b = case_when(item_cd == "101. Teff" & othermeasuringUnit == "KG" ~ "1.Kilogram",
                             item_cd == "303. SESAME" & othermeasuringUnit == "KG" ~ "1.Kilogram",
                             item_cd %in% c("710. Sugar","712. Salt","801. Coffee") & othermeasuringUnit == "Small Birchiko" ~ "31. Birchiko Small",
                             item_cd %in% c("710. Sugar","712. Salt","801. Coffee") & othermeasuringUnit == "Small Sini" ~ "171. Sini Small",
                             item_cd == "801. Coffee" & othermeasuringUnit == "Small Tasa/Tanika/Shember/Selemon" ~ "181. Tasa/Tanika/Shember/Selemon Small",
                             item_cd == "801. Coffee" & othermeasuringUnit == "LAMBA" ~ "102. Kubaya/Cup Medium",
                             item_cd == "501. Banana" & othermeasuringUnit == "ZELELA" ~ "142. Piece/number Medium",
                             item_cd == "501. Banana" & othermeasuringUnit == "SMALL ZELELA" ~ "141. Piece/number Small",
                             item_cd == "703. Poultry" & othermeasuringUnit == " WHOLE ANIMAL (POULTARY)" ~ "142. Piece/number Mediuml",
                             item_cd == "903. Pasta/Maccaroni" & othermeasuringUnit == "PACK" ~ "142. Piece/number Medium",
                             item_cd == "802. Tea" & othermeasuringUnit == "PACK" ~ "142. Piece/number Mediuml",
                             item_cd == "802. Tea" & othermeasuringUnit == "PACK (SMALL)" ~ "141. Piece/number Small",
                             item_cd == "802. Tea" & othermeasuringUnit == "PACK (LARGEL)" ~ "143. Piece/number Large",
                             item_cd %in% c("803. Soft drinks/Soda", "804. Beer") & othermeasuringUnit == "BOTTLE" ~"142. Piece/number Medium",
                             item_cd == "708. Oils (processed)" & othermeasuringUnit == "BEER BOTTLE" ~ "33. Birchiko Large",
                             item_cd == "712. Salt" & othermeasuringUnit == "PACK" ~ "142. Piece/number Mediuml",
                             TRUE~ s6aq02b)) 


# Extract codes
ess_cons <- ess_cons |> 
  # select(item_cd) |> 
  # mutate(item_code = str_sub(item_cd,1,3)) |> 
  # select(item_cd) |>
  rowwise() |> 
  mutate(food_cd = stringr::str_split_fixed(item_cd,"\\.",2)[1]) |> 
  #mutate(item_name = stringr::str_split_fixed(item_cd,"\\.",2)[2]) |> 
  #mutate(item_name = str_trim(item_name,side="left")) |> 
  # select(s6aq02b) |>
  rowwise() |>
  mutate(unit_cd = stringr::str_split_fixed(s6aq02b, "\\.",2)[1])|> 
  # select(saq01) |>
  mutate(region_cd = stringr::str_split_fixed(saq01, "\\.",2)[1]) |>
  ungroup() |>
  #creating fake 90 region so that it can match with soddhar variable in the conversion factor region
  mutate(regionCF = case_when(region_cd == "5" ~ "90", 
                              region_cd == "13" ~ "90",
                              region_cd == "15" ~ "90",
                              TRUE ~ region_cd)) 
# joining conversion factor
ess_cons <- dplyr::left_join(ess_cons,ess_unit_cf, by = c("food_cd", "unit_cd", "regionCF")) 

#joining afe
ess_afe <- ess_afe |> dplyr::rename(household_id =HHID)
ess_cons <- left_join(ess_cons,ess_afe, by = c("household_id")) 

ess_cons <- ess_cons |> 
 mutate(amountConsumedInGperDay = (consQnty*conversion_factor*1000)/7) |> 
  # Adding unit values in gram which are not in the convunit.csv
  mutate(amountConsumedInGperDay = case_when(othermeasuringUnit %in% c("WHOLE ANIMAL (GOAT)","WHOLE ANIMAL (GOATSHEEP)") & item_cd == "701. Goat & mutton meat" ~ 5000, T~ amountConsumedInGperDay)) |>  # from the 2018 salt paper, one medium goat weighs 5kg
  #adjusting for non edible portion
  mutate(amountConsumedInGperDay = case_when(food_cd == "601" ~ (amountConsumedInGperDay*0.9),
                                     food_cd == "607" ~ (amountConsumedInGperDay*0.86),
                                     food_cd == "608" ~ (amountConsumedInGperDay*0.92),
                                     food_cd == "501" ~ (amountConsumedInGperDay*0.62),
                                     food_cd == "709" ~ (amountConsumedInGperDay*0.87),
                                     food_cd == "502" ~ (amountConsumedInGperDay*0.73),
                                     food_cd == "504" ~ (amountConsumedInGperDay*0.62),
                                     food_cd == "505" ~ (amountConsumedInGperDay*0.71),
                                     food_cd == "503" ~ (amountConsumedInGperDay*0.71),
                                     food_cd == "604" ~ (amountConsumedInGperDay*0.84),
                                     food_cd == "609" ~ (amountConsumedInGperDay*0.84),
                                     food_cd == "401" ~ (amountConsumedInGperDay*0.89),
                                     food_cd == "405" ~ (amountConsumedInGperDay*0.91),
                                     T~ amountConsumedInGperDay))

#removing unreplaced other food items and measuring units and tea, hops, chat
ess_cons <- ess_cons |> 
  #filter(!grepl("806. Chat / Kat", item_cd)) |> 
  filter(!(item_cd %in% c("806. Chat / Kat", "807. Hops (gesho)")))|> 
  filter(amountConsumedInGperDay>0)
# list(ess_cons$amountConsumedInGperDay<0)
#creating intake per afe

ess_cons <- ess_cons |>
  mutate(amountConsumedInGperDayAfe = amountConsumedInGperDay/afe) 
  
#checking outliers  

ess_cons <- ess_cons %>% mutate(log.amountConsumedInGperDayAfe.plus1 =log(amountConsumedInGperDayAfe+1)) #Calculates the log of kg_afe_d+1

#Calculate medians and SDs. Does so by each item through the group_by function.
ess_cons.summ <- ess_cons %>% group_by(item_cd) %>% 
  dplyr::summarise(n=n(), 
                   mean.logplus1=mean(log(amountConsumedInGperDayAfe+1), na.rm = TRUE), 
                   median=median(amountConsumedInGperDayAfe, na.rm = TRUE),
                   sd.logplus1=sd(log(amountConsumedInGperDayAfe+1), na.rm = TRUE),
                   max = max(amountConsumedInGperDayAfe, na.rm = TRUE))

ess_cons.summ.merg <- ess_cons.summ %>% mutate(sd4 = sd.logplus1*4) %>% mutate(sd5 = sd.logplus1*5) %>% mutate(sd3 = sd.logplus1*3) %>% mutate(sd2 = sd.logplus1*2) %>% select(item_cd, mean.logplus1, median, sd2, sd3, sd4, sd5) #Creates different Standard Deviation cutoffs, and selects only these statistical data points.
ess_cons.summ.merg <- ess_cons.summ.merg %>% mutate(cut4 = sd4 + mean.logplus1) %>% mutate(cut5 = sd5 + mean.logplus1) %>% mutate(cut3 = sd3 + mean.logplus1) %>% mutate(cut2 = sd2 + mean.logplus1) %>% select(item_cd, median, cut2, cut3, cut4, cut5) #Creates different cut points - for easy selection of different levels of outliers.


ess_cons <- ess_cons |> merge(x=ess_cons, y=ess_cons.summ.merg , by.x='item_cd', by.y='item_cd', fill=-9999, all.x = TRUE) %>% arrange(household_id) #Merges this stats data back to the df.

ess_cons <- ess_cons %>% mutate(ol2 = cut2-log.amountConsumedInGperDayAfe.plus1) #Sees if the value lies above or below the cut3
ess_cons <- ess_cons %>% mutate(outlier2 = ifelse(ol2<0, 1, NA)) #Creates the outlier3 flag based on the above decision
table(ess_cons$outlier2) # 2.6%

ess_cons <- ess_cons %>% mutate(ol3 = cut3-log.amountConsumedInGperDayAfe.plus1) #Sees if the value lies above or below the cut3
ess_cons <- ess_cons %>% mutate(outlier3 = ifelse(ol3<0, 1, NA)) #Creates the outlier3 flag based on the above decision
table(ess_cons$outlier3) # 0.48%

ess_cons <- ess_cons %>% mutate(ol4 = cut4-log.amountConsumedInGperDayAfe.plus1) #Sees if the value lies above or below the cut4
ess_cons <- ess_cons %>% mutate(outlier4 = ifelse(ol4<0, 1, NA)) #Creates the outlier4 flag based on the above decision
table(ess_cons$outlier4) # 0.16%

ess_cons <- ess_cons %>% mutate(ol5 = cut5-log.amountConsumedInGperDayAfe.plus1) #Sees if the value lies above or below the cut5
ess_cons <- ess_cons %>% mutate(outlier5 = ifelse(ol5<0, 1, NA)) #Creates the outlier5 flag based on the above decision
table(ess_cons$outlier5) # 0.08%

ess_cons$outlier.id <- paste0(as.character(ess_cons$household_id),"_", as.character(ess_cons$item_cd)) #Creates the outlier id

ess_cons$missing_kg_afe_d_outlier <- NA

ess_cons[is.na(ess_cons$amountConsumedInGperDayAfe), "missing_kg_afe_d_outlier"] <- 1
table(ess_cons$missing_kg_afe_d_outlier)

# outlier cutoffs outlier3 and 2 ----
#ess_cons$outlier <- NA #Creates Semira limit column from outlier2
ess_cons<- ess_cons |> mutate(outlier=outlier3)
ess_cons<- ess_cons |> mutate(outlier= case_when(item_cd%in% c("901. purchased Injera",
                                                          "607. Godere",
                                                          "606. Cassava",
                                                          "605. Boye/Yam",
                                                          "604. Sweet potato",
                                                          "603. Bula",
                                                          "602. Kocho") ~ outlier2,
                                                 T ~ outlier)) 


# ¬ Outlier processing ----
names(ess_cons)
outliers <- ess_cons %>% filter(outlier==1) %>% select(household_id, outlier5, item_cd, consQnty, s6aq02b, outlier.id, amountConsumedInGperDay, afe, amountConsumedInGperDayAfe, median, cut5) %>% arrange(item_cd) # Subsets and sorts the dataframe into an outliers df. 

outliers$amountConsumedInGperDayAfe_replace <-outliers$median #calculate a replacement value for the outliers.

write.csv(outliers, "intermediate_output_outliers.v.1.0.2.csv")

outliers <- outliers %>% select(outlier.id, amountConsumedInGperDayAfe_replace)

outliers <- outliers[!duplicated(outliers),]

ess_cons <- merge(x=ess_cons, y=outliers , by.x='outlier.id', by.y='outlier.id', fill=-9999, all.x = TRUE) #This merge is adding 4 rows somehow. - Fixed by duplicate removal

ess_cons <- ess_cons %>% mutate(gram_d_afe=ifelse(!is.na(amountConsumedInGperDayAfe_replace),amountConsumedInGperDayAfe_replace, amountConsumedInGperDayAfe))

#boxplot(ess_cons$gram_d_afe[ess_cons$item_cd=="602. Kocho"])
  
  
#joining with the wealth index
ess_cons <- left_join(ess_cons,ess_wealth, by = c("household_id")) 
table(ess_wealth$cons_quint)
count(ess_wealth$household_id)
names(ess_cons)

#joining with the gps data
ess_cons <- left_join(ess_cons,ess_gps, by = c("household_id")) 
#joining with the interview date dataset
ess_cons <- left_join(ess_cons,ess_date, by = c("household_id")) 
#filtering and renaming variables for the tool
ess_cons<- ess_cons |> mutate(amount_consumed_in_g = gram_d_afe*afe)
ess_cons$urbanity <- NA
ess_cons[ess_cons$urbanRural == "2. URBAN", "urbanity"] <- 1
ess_cons[ess_cons$urbanRural == "1. RURAL", "urbanity"] <- 2
table(ess_cons$urbanity)
ess_cons<- ess_cons |>
rename(latitud=lat_mod,
       longitud=lon_mod,
       wealth_quintile=cons_quint,
       household_expenditure=birrPurchasedFood,
       interview_date=InterviewStart,
       original_food_id=food_cd,
       original_food_name=item,
       survey_weight=pw_w4)

food_cons<- ess_cons |> select(household_id,latitud,longitud,urbanity,wealth_quintile,household_expenditure,interview_date,original_food_id,original_food_name,survey_weight,region)

write_csv(food_cons,here::here("food-cons_ess4_v1.0.0.1.csv"))
