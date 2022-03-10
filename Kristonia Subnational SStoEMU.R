# Kristonia Subnational (Level 2) SStoEMU

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(janitor)
library(zoo)
library(cowplot)
library(ggpubr)
library(pdftools)
library(rlang)
library(purrr)
library(openxlsx)
library(stringi)
library(viridis)
setwd("C:/Users/KristinBietsch/files/Track20/SS to EMU/Creating Fake Subnational SStoEMU")

options(scipen = 999)
##################################################################3
# Read in data

inputs <- read.csv("Kristonia Annual Subnational Service Statistics Input Data.csv") 
pop <- read.csv("Kristonia Subnational Population Data.csv") %>% rename(year=Year)
FPSource <- read.csv("Kristonia Subnational FPSource.csv") 

fpet <- read.csv("FPET Results for Subnational Kristonia.csv")
method_mix <- read.csv("Kristonia Subnational Method Mix Data.csv")
dhs_mcpr <- read.csv("Fake DHS Results for Subnational Kristonia.csv")
pop_long <- read.csv("Population 2000 2030 for Subnational Kristonia.csv")

Sectors_reporting <- read.csv("Kristonia Sectors Reporting.csv") 
prior_dist <- read.csv("Kristonia Prior Distribution.csv") 

cyp <- read.csv("CYP_Data 2022.csv")
continuation <- read.csv("Continuation_data 2022.csv")

graph_names <- read.csv("GraphNames.csv")
graph_num <- read.csv("GraphNum.csv")
##############################################################


##############################################################

# CYP for sterilization is 10 in this example
continuation_long <- continuation %>% gather(Year, Value, Yr0:yr10) %>% 
  mutate(Year=as.numeric(substr(Year, 3, 4))) %>% 
  arrange(Method_Det, Year) %>%
  group_by(Method_Det) %>%
  mutate(Continuation=case_when( Year==10  ~ 0,
                                 Year==9 & Method=="Sterilization" ~ 1,
                                 Year<10 ~ (Value+lead(Value))/2))

# Creating the percent of acceptors who will still be using each year
continuation_wide <- continuation_long %>% mutate(Year=paste("Year", Year, sep="_")) %>% select(-Value) %>% spread(Year, Continuation) %>% select(Method_Det, Year_0, Year_1 , Year_2, Year_3, Year_4, Year_5, Year_6, Year_7, Year_8, Year_9, Year_10)

##############################################################

# Editing names, transforming from proportion to percentage, transforming to long format
FPSource_clean <- FPSource %>% rename( Private_Clinic=PrivateClinic, Shop_Church_Friend=ShopChurchFriend) %>%
  mutate(Public=Public*100,
         Private_Clinic=Private_Clinic*100,
         Pharmacy=Pharmacy*100,
         Shop_Church_Friend=Shop_Church_Friend*100,
         NGO=NGO*100,
         Other=Other*100)  %>% 
  gather(Source, Value, NGO:Shop_Church_Friend) %>%
  mutate(Value=case_when(is.na(Value) ~ 0, ! is.na(Value) ~ Value))


############
# Editing names, transforming to long format
Sectors_reporting_clean <- Sectors_reporting  %>% rename(     "NGO"  = "Private_NGO"  ,   "Private_Clinic"= "Private_HospClin"   , "Pharmacy" ="Private_Pharm"   ) %>%
  gather(Source, Reporting, Public:Other) %>%
  mutate(Reporting=case_when(is.na(Reporting) ~ 0, ! is.na(Reporting) ~ Reporting)) 

##################
# want to make sure we have adjustment factors for all methods even if not in DHS
Method <- c("Female Sterilization",  "Male Sterilization", "IUD", "Implant",  "Injectable",   "OC Pills", "Male Condom",   "Female Condom",  "Other Modern Methods",  "EC"  )
Method_Num <- c(1,2,3,4,5,6,7,8,9,10)
method.df <- data.frame(Method, Method_Num)

#########################################################
# selecting just the needed data
prior_dist_clean <- prior_dist %>% select(Method_Det, Prior_Dist)

##################
# Creating the adjustment factors- notice you will need to change the names in gather for your own regions
country_sector_rep <- full_join(FPSource_clean, Sectors_reporting_clean, by="Source") %>%
  mutate(Method=case_when(Method=="Other Modern Method" ~ "Other Modern Methods",
                          Method!="Other Modern Method" ~ Method)) %>% 
  mutate(Reported=Value*Reporting) %>% 
  group_by(Region, Method, Type) %>% 
  summarise(Reported=sum(Reported)) %>%
  mutate(Reported=case_when(Reported==0 ~ 100,
                            Reported!=0 ~ Reported),
         Adjustment_Factor=100/Reported) %>% 
  ungroup() %>%
  select(-Reported, -Type) %>%
  spread(Region, Adjustment_Factor) %>%
  full_join(method.df, by="Method") %>% 
  gather(Region, Adjustment_Factor, Alexandria:Port_Alvin) %>% 
  mutate(Adjustment_Factor= case_when(is.na(Adjustment_Factor)~ 1, !is.na(Adjustment_Factor)~ Adjustment_Factor)) %>%
  filter(!is.na(Method_Num))

###########################################################################
#If you want to have unadjusted male condom data run this code:
#country_sector_rep <- country_sector_rep %>% mutate(Adjustment_Factor=case_when(Method=="Male Condom" ~ 1, Method!="Male Condom" ~ Adjustment_Factor))

###########################################################################


# We are now ready to constrct the SStoEMU 
data <- inputs  %>% rename(Method_Det=Method)

# Creating a list of the years we have data for, and identifying the first and last year of data
Year <- seq(min(data$Year), max(data$Year), 1)
year_list.df <- data.frame(Year)
year_min <- as.numeric(min(data$Year))
year_max <- as.numeric(max(data$Year))

# We need to pull out the long acting methods to estimate the number of historical users before HMIS data was recorded
lam_first <- data %>%   full_join( cyp, by="Method_Det") %>% 
  filter(Year== as.numeric(min(data$Year))) %>% filter(Method_Num<=4) %>% 
  select(Region, Method_Det,  Value) %>% rename(LAPM_First=Value)

# Creating a dataset of the first year of long acting methods and the cummulative sum multiplier from continuation long
continuation_lapm <- continuation_long %>% 
  mutate(Year=Year+as.numeric(min(data$Year))) %>% 
  select(Method_Det, Year, Continuation) %>% 
  spread(Method_Det, Continuation) %>% 
  full_join(year_list.df, by="Year") %>% 
  gather(Method_Det, Continuation, "Copper- T 380-A IUD":"Vasectomy (M)" ) %>% 
  mutate(Continuation=case_when(is.na(Continuation) ~ 0, !is.na(Continuation) ~ Continuation)) %>%
  arrange(Method_Det , -Year) %>%
  group_by(Method_Det) %>% 
  mutate(Cont_Sum1=cumsum(Continuation),
         Cont_Sum=lag(Cont_Sum1)) %>%
  mutate(Cont_Sum=case_when(is.na(Cont_Sum) ~ 0, !is.na(Cont_Sum) ~ Cont_Sum)) %>%
  select(Year, Method_Det, Cont_Sum) %>% 
  filter(Year<=year_max) %>%
  full_join(lam_first,  by=c("Method_Det"))

# This code creates the EMUs
clients <-  data %>% 
  full_join( cyp, by="Method_Det") %>% # Brining in the CYPs for each method
  select(-VisitCYP, -Method) %>%
  full_join( country_sector_rep, by=c( "Method_Num", "Region")) %>%
  full_join(continuation_lapm, by=c("Method_Det", "Year", "Region")) %>%
  full_join( continuation_wide, by=c("Method_Det")) %>% 
  full_join( prior_dist_clean, by=c("Method_Det")) %>%
  filter(!is.na(Method_Num)) %>%
  mutate(historical= Cont_Sum * LAPM_First* Prior_Dist,
         Value_0=Value*Year_0,
         Value_1=Value*Year_1,
         Value_2=Value*Year_2,
         Value_3=Value*Year_3,
         Value_4=Value*Year_4,
         Value_5=Value*Year_5,
         Value_6=Value*Year_6,
         Value_7=Value*Year_7,
         Value_8=Value*Year_8,
         Value_9=Value*Year_9,
         Value_10=Value*Year_10) %>%
  arrange(Region, Method_Det, Year) %>%
  group_by(Region, Method_Det) %>% 
  mutate( lag_1 = case_when(Year== year_min ~ 0, Year>year_min~ lag(Value_1)) ,
          lag_2=case_when(Year<= year_min+1 ~ 0, Year>year_min+1~ lag(Value_2, 2)) ,
          lag_3=case_when(Year<= year_min+2 ~ 0, Year>year_min+2~ lag(Value_3, 3)) ,
          lag_4=case_when(Year<= year_min+3 ~ 0, Year>year_min+3~ lag(Value_4, 4)) ,
          lag_5=case_when(Year<= year_min+4 ~ 0, Year>year_min+4~ lag(Value_5, 5)) ,
          lag_6=case_when(Year<= year_min+5 ~ 0, Year>year_min+5~ lag(Value_6, 6)) ,
          lag_7=case_when(Year<= year_min+6 ~ 0, Year>year_min+6~ lag(Value_7, 7)) ,
          lag_8=case_when(Year<= year_min+7 ~ 0, Year>year_min+7~ lag(Value_8, 8)) ,
          lag_9=case_when(Year<= year_min+8 ~ 0, Year>year_min+8~ lag(Value_9, 9)) ,
          lag_10=case_when(Year<= year_min+9 ~ 0, Year>year_min+9~ lag(Value_10, 10)) ) %>%
  mutate(unadj_users= historical + Value_0 + lag_1  + lag_2 + lag_3 + lag_4 + lag_5 + lag_6 + lag_7 + lag_8 + lag_9  + lag_10) %>% # this is the data for long term methods, from before HMIS data was recorded, from HMIS data from before the index year, and insertions in the index year
  mutate(users=case_when(Method_Num>=5 ~ Value*CYP*Adjustment_Factor,
                         Method_Num<=4 ~ unadj_users*Adjustment_Factor),
         users_unadj=case_when(Method_Num>=5 ~ Value*CYP,
                               Method_Num<=4 ~ unadj_users)) %>%
  mutate(type="clients") %>%
  select(Region, type, Method_Det, Year, Method, Method_Num, users, users_unadj)

# Grouping individual methods together into grouped methods
clients_user_group <- clients %>% mutate(Method_G=case_when(Method_Det=="Tubal Ligation (F)" ~ "Sterilization (female)",
                                                            Method_Det=="Vasectomy (M)"    ~  "Sterilization (male)",
                                                            Method_Det== "Copper- T 380-A IUD"   |   Method_Det=="LNG-IUS"  ~ "IUD",
                                                            Method_Det== "Implanon"      |   Method_Det=="Sino-Implant"        |   Method_Det=="Jadelle"         ~ "Implant" ,
                                                            Method_Det==  "Sayana Press"     |   Method_Det=="Other Injectable"   |   Method_Det=="Noristerat (NET-En)"  |   Method_Det=="Lunelle"  |   Method_Det=="Depo Provera (DMPA)"~ "Injectable",
                                                            Method_Det==  "Combined Oral (COC)" |   Method_Det=="Other OC Pill"   |   Method_Det=="Progestin only (POP)" ~"OC Pills" ,
                                                            Method_Det=="Male Condom"  |   Method_Det=="Female Condom"  ~"Condom (m+f)" ,
                                                            Method_Det== "LAM" ~"LAM" ,
                                                            Method_Det== "Vaginal barrier"  |   Method_Det=="EC"   |   Method_Det=="SDM (Standard Days)"  |   Method_Det=="Spermicides" ~ "Other Modern Methods" )) %>%
  ungroup() %>%
  select(Region, type,  Method_G, Year, users, users_unadj) %>% 
  group_by( Region, Method_G, Year) %>%
  summarise(users=sum(users), users_unadj=sum(users_unadj)) %>% mutate(Wout_Condom=case_when(Method_G=="Condom (m+f)" ~ 0,
                                                                                             Method_G!="Condom (m+f)" ~ 1)) %>% 
  ungroup() 

# Totalling all methods into a singular EMU
clients_year <- clients_user_group %>% group_by(Region, Year) %>% 
  summarise(EMUsers=sum(users), EMUsers_woC=sum(users*Wout_Condom), Unadj_EMUusers=sum(users_unadj))  %>% 
  filter(!is.na(Year))

######################################
# Bringing in the population data to serve as a denominator for the EMUs

EMU_Annual <- clients_year %>% rename(year=Year) %>% left_join( pop, by=c("Region", "year")) %>% 
  mutate(emu_clients_incCon=EMUsers/Pop, emu_clients_exCon=EMUsers_woC/Pop)

###########################################
#######################################################################################
# If you would like to make the graphics found in the Excel version of SStoEMU, which are used to examine data quality, the following code preps data to create these graphics. 
#######################################################################################

# for input graphic 
values_input <- data %>% left_join(graph_names, by="Method_Det") %>% 
  left_join(graph_num, by="GraphMethod") %>%
  mutate(Method_Det_name=case_when( Method_Det=="EC" ~ "EC",
                                    Method_Det!="EC" ~ paste(GraphMethod, Method_Det, sep=": ")))

# for output graphic 1
wra_mcpr_year <- full_join(pop_long, fpet, by=c("year", "Region")) %>%
  filter(year>=2005 & year<=2020) %>%
  mutate(AW_Users=Pop*mCPR_AW)

# For output graphic 2: method mix number of users
methodmix_users <- method_mix  %>%
  full_join(pop_long, by="Region") %>%
  full_join(fpet, by=c("year", "Region")) %>%
  mutate(Users_method=Pop*(mCPR_AW)*(MethodMix),
         prev_method=(mCPR_AW)*(MethodMix)) %>%
  filter(year>=2005 & year<=2020) %>%
  mutate(Method_G=case_when(Variable== "FSter" ~ "Sterilization (female)",
                            Variable=="MSter"    ~  "Sterilization (male)",
                            Variable== "IUD"    ~ "IUD",
                            Variable==  "Implant"         ~ "Implant" ,
                            Variable==   "Injectable" ~ "Injectable",
                            Variable==   "Pill"  ~"OC Pills" ,
                            Variable== "MCondom"    |   Variable== "FCondom"    ~"Condom (m+f)" ,
                            Variable==    "LAM" ~"LAM" ,
                            Variable==  "OMM"    |   Variable=="EC"   |   Variable== "SDM"    ~ "Other Modern Methods" )) %>%
  group_by(Region, year, Method_G) %>%
  summarise(Users_method=sum(Users_method), prev_method=sum(prev_method), MethodMix=sum(MethodMix)) %>% ungroup() %>%
  select(Region, year, Method_G, Users_method, prev_method, MethodMix)



EMU_FPET <- left_join(EMU_Annual, wra_mcpr_year, by=c("year", "Region"))

clients_out1 <- EMU_FPET %>% select(Region, year, EMUsers, Unadj_EMUusers, AW_Users) %>% 
  gather(Variable, Value, EMUsers:AW_Users) %>%
  mutate(Variable_name=case_when(Variable=="EMUsers" ~ "Total Users (service statistics-adjusted)", 
                                 Variable=="Unadj_EMUusers" ~ "Total Users (service statistics-unadjusted)", 
                                 Variable=="AW_Users" ~ "Total Users (survey/FPET)" ))

clients_out2 <- clients_user_group %>% rename(year=Year) %>% left_join( methodmix_users, by=c("Region", "year", "Method_G")) %>% 
  select(Region, Method_G, year, users, users_unadj, Users_method) %>%
  gather(Variable, Value, users:Users_method) %>%
  mutate(Variable_num=case_when(Variable=="users" ~ 2,
                                Variable=="users_unadj" ~ 1,
                                Variable=="Users_method" ~ 3)) %>%
  mutate(Variable_name=case_when(Variable=="users" ~ "Users by Method\n(service statistics-\nadjusted)",
                                 Variable=="users_unadj" ~ "Users by Method\n(service statistics-\nunadjusted)",
                                 Variable=="Users_method" ~ "Users by Method\n(service statistics-\nsurvey/FPET)")) %>%
  mutate(Method_Num= case_when(  Method_G== "Sterilization (female)" ~ 1,
                                 Method_G== "Sterilization (male)" ~ 2,
                                 Method_G==  "IUD" ~ 3,
                                 Method_G== "Implant" ~ 4,
                                 Method_G== "Injectable" ~ 5,
                                 Method_G== "OC Pills" ~ 6,
                                 Method_G==  "Condom (m+f)" ~ 7,
                                 Method_G==  "LAM" ~ 8,
                                 Method_G== "Other Modern Methods" ~ 9)) %>% filter(!is.na(Region))


clients_out3 <- clients_user_group %>% rename(year=Year) %>% 
  select(Region, Method_G, year, users)

clients_out5 <- clients_user_group %>% rename(year=Year) %>% left_join( methodmix_users, by=c("Region", "year", "Method_G")) %>% 
  select(Region, Method_G, year, users, users_unadj, Users_method) %>%
  gather(Variable, Value, users:Users_method) %>%
  mutate(Variable_num=case_when(Variable=="users" ~ 2,
                                Variable=="users_unadj" ~ 1,
                                Variable=="Users_method" ~ 3)) %>%
  mutate(Variable_name=case_when(Variable=="users" ~ "Users by Method\n(service statistics- adjusted)",
                                 Variable=="users_unadj" ~ "Users by Method\n(service statistics- unadjusted)",
                                 Variable=="Users_method" ~ "Users by Method\n(service statistics- survey/FPET)")) %>%
  mutate(Method_Num= case_when(  Method_G== "Sterilization (female)" ~ 1,
                                 Method_G== "Sterilization (male)" ~ 2,
                                 Method_G==  "IUD" ~ 3,
                                 Method_G== "Implant" ~ 4,
                                 Method_G== "Injectable" ~ 5,
                                 Method_G== "OC Pills" ~ 6,
                                 Method_G==  "Condom (m+f)" ~ 7,
                                 Method_G==  "LAM" ~ 8,
                                 Method_G== "Other Modern Methods" ~ 9),
         Method_Name= case_when(  Method_G== "Sterilization (female)" ~ "Ster\n(female)",
                                  Method_G== "Sterilization (male)" ~ "Ster\n(male)",
                                  Method_G==  "IUD" ~ "IUD",
                                  Method_G== "Implant" ~ "Implant",
                                  Method_G== "Injectable" ~ "Injectable",
                                  Method_G== "OC Pills" ~ "Pills",
                                  Method_G==  "Condom (m+f)" ~ "Condom\n(m+f)",
                                  Method_G==  "LAM" ~  "LAM" ,
                                  Method_G== "Other Modern Methods" ~ "Other\nModern\nMethods")) %>%
  filter(Variable_num!=1) %>% filter(!is.na(Region))

clients_out6 <-  clients_user_group %>% rename(year=Year) %>% full_join( EMU_FPET, by=c("Region", "year"))  %>% 
  left_join(methodmix_users, by=c("Region", "Method_G", "year")) %>% 
  mutate(Share=(users/EMUsers)*100,
         MethodMix=MethodMix*100) %>% 
  select(Region, Method_G, year, MethodMix, Share) %>%
  mutate(Method_Num= case_when(  Method_G== "Sterilization (female)" ~ 1,
                                 Method_G== "Sterilization (male)" ~ 2,
                                 Method_G==  "IUD" ~ 3,
                                 Method_G== "Implant" ~ 4,
                                 Method_G== "Injectable" ~ 5,
                                 Method_G== "OC Pills" ~ 6,
                                 Method_G==  "Condom (m+f)" ~ 7,
                                 Method_G==  "LAM" ~ 8,
                                 Method_G== "Other Modern Methods" ~ 9)) %>%
  arrange(Region, year, -Method_Num) %>%
  gather(Variable, Value, MethodMix:Share) %>% 
  group_by(Region, year, Variable) %>% mutate(pos = cumsum(Value)- Value/2) %>%
  mutate(Label=case_when(Value>=2.5 ~ round(Value),
                         Value<2.5 ~ NA_real_)) %>%
  mutate(Variable_Lab=case_when(Variable=="Share" ~ "Estimated Modern Method Mix",
                                Variable=="MethodMix" ~ "Survey Modern\nContraceptive Method Mix"))

clients_out7 <- left_join(EMU_FPET, dhs_mcpr, by=c("Region", "year")) %>%
  mutate(emu_clients_incCon=emu_clients_incCon*100,
         emu_clients_exCon=emu_clients_exCon*100,
         mCPR=mCPR_AW.x * 100,
         DHS_aw_mCPR = mCPR_AW.y * 100) %>%
  select(Region, year, emu_clients_incCon, emu_clients_exCon, mCPR, DHS_aw_mCPR) %>%
  gather(Variable, Value, emu_clients_incCon:DHS_aw_mCPR) %>%
  mutate(Variable_name=case_when(Variable=="emu_clients_incCon" ~ "EMU: Commodities inc. Condoms",
                                 Variable=="emu_clients_exCon" ~ "EMU: Commodities ex. Condoms",
                                 Variable=="mCPR" ~ "mCPR (AW): FPET",
                                 Variable=="DHS_aw_mCPR" ~ "mCPR (AW): DHS")) %>%
  group_by(Region, Variable) %>%
  mutate(Value_Interp=na.approx(Value,  maxgap = Inf,   na.rm = FALSE)) %>%
  mutate(annual_change=Value_Interp-lag(Value_Interp))

clients_out8 <- clients_out7 %>% group_by(Region, Variable) %>% summarise(Ave_Change=mean(annual_change, na.rm=TRUE)) %>%
  mutate(Ave_Change_Label=case_when(is.na(Ave_Change) ~ "No Data",
                                    !is.na(Ave_Change) ~ paste(round(Ave_Change, 1), "%", sep="")),
         Ave_Change=case_when(is.na(Ave_Change) ~ 0,
                              !is.na(Ave_Change) ~ Ave_Change)) %>% 
  mutate(Variable_name=case_when(Variable=="emu_clients_incCon" ~ "EMU: Commodities inc. Condoms",
                                 Variable=="emu_clients_exCon" ~ "EMU: Commodities ex. Condoms",
                                 Variable=="mCPR" ~ "mCPR (AW): FPET",
                                 Variable=="DHS_aw_mCPR" ~ "mCPR (AW): DHS")) %>%
  mutate(Variable_num=case_when(Variable=="emu_clients_incCon" ~ 6,
                                Variable=="emu_clients_exCon" ~ 5,
                                Variable=="mCPR" ~ 4,
                                Variable=="DHS_aw_mCPR" ~ 2)) %>%
  mutate(hjust_val = case_when(Ave_Change==0 ~ .6,
                               Ave_Change<0 ~ 1,
                               Ave_Change>0 ~ -.3))


#######################################################################################
# EMU Outputs
#######################################################################################

emu_trends <-EMU_Annual  %>%  select(Region, year, emu_clients_incCon, emu_clients_exCon)  %>%
  left_join(fpet, by=c("Region", "year"))  %>% 
  rename(UNPD_mCPR=mCPR_AW) %>% select(-mCPR_MW) %>%
  left_join(dhs_mcpr, by=c("Region", "year"))   %>% select(-mCPR_MW) %>% rename(DHS_aw_mCPR=mCPR_AW) %>%
  gather(Variable, Value, emu_clients_incCon:DHS_aw_mCPR) %>%
  mutate(Value=Value*100) %>%
  mutate(Variable_name=case_when(Variable=="emu_clients_incCon" ~ "EMU: Commodities to Clients inc. Condoms",
                                 Variable=="emu_clients_exCon" ~ "EMU: Commodities to Clients ex. Condoms",
                                 Variable=="UNPD_mCPR" ~ "mCPR (AW): FPET",
                                 Variable=="DHS_aw_mCPR" ~ "mCPR (AW): DHS")) %>%
  group_by(Region, Variable) %>%
  mutate(Value_Interp=na.approx(Value,  maxgap = Inf,   na.rm = FALSE)) %>%
  mutate(annual_change=Value_Interp-lag(Value_Interp)) %>%
  ungroup()

emu_trends_wide <- emu_trends %>% select(-Variable, -Value_Interp, -annual_change) %>% spread(Variable_name, Value) %>% filter(!is.na(year)) 

emu_annual_change <- emu_trends %>% group_by(Region, Variable) %>% summarise(Ave_Change=mean(annual_change, na.rm=TRUE)) %>%
  mutate(Ave_Change_Label=case_when(is.na(Ave_Change) ~ "No Data",
                                    !is.na(Ave_Change) ~ paste(round(Ave_Change, 1), "%", sep="")),
         Ave_Change=case_when(is.na(Ave_Change) ~ 0,
                              !is.na(Ave_Change) ~ Ave_Change)) %>%
  mutate(Variable_name=case_when(Variable=="emu_clients_incCon" ~ "EMU: Commodities to Clients inc. Condoms",
                                 Variable=="emu_clients_exCon" ~ "EMU: Commodities to Clients ex. Condoms",
                                 Variable=="UNPD_mCPR" ~ "mCPR (AW): FPET",
                                 Variable=="DHS_aw_mCPR" ~ "mCPR (AW): DHS")) %>%
  mutate(Variable_num=case_when(Variable=="emu_clients_incCon" ~  12,
                                Variable=="emu_clients_exCon" ~  11,
                                Variable=="UNPD_mCPR" ~ 4,
                                Variable=="DHS_aw_mCPR" ~ 2)) %>%
  mutate(hjust_val = case_when(Ave_Change==0 ~ .6,
                               Ave_Change<0 ~ 1,
                               Ave_Change>0 ~ -.3)) %>%
  filter(!is.na(Variable_name))

clients_emu_users <- clients_user_group %>% rename(year=Year) %>% select(Region,  Method_G, year, users) %>% rename(clients_users=users)
survey_users <- methodmix_users %>% select(Region,  Method_G, year, Users_method) %>% rename(survey_users=Users_method)

emu_users <- clients_emu_users %>% 
  left_join(survey_users, by=c("Region", "Method_G", "year")) %>%
  gather(Variable, Value, clients_users:survey_users) %>%
  mutate(Variable_Name=case_when(Variable=="clients_users"    ~ "Users: Commodities to Clients",
                                 Variable=="survey_users"   ~ "Users: Survey" )) %>%
  mutate(Method_Num= case_when(  Method_G== "Sterilization (female)" ~ 1,
                                 Method_G== "Sterilization (male)" ~ 2,
                                 Method_G==  "IUD" ~ 3,
                                 Method_G== "Implant" ~ 4,
                                 Method_G== "Injectable" ~ 5,
                                 Method_G== "OC Pills" ~ 6,
                                 Method_G==  "Condom (m+f)" ~ 7,
                                 Method_G==  "LAM" ~ 8,
                                 Method_G== "Other Modern Methods" ~ 9))

emu_users_wide <- emu_users %>% select(-Method_Num, -Variable) %>% spread(Variable_Name, Value) %>% filter(!is.na(Method_G))

##################################################################################################################
##################################################################################################################
##################################################################################################################

# The following code lists all subnational regions, then creates PDF documents for each region of all graphics

levels(as.factor(emu_users_wide$Region))
# Graphics
regions <- c( "Alexandria" ,  "Dorothyville" , "Hagridland",   "Port_Alvin"   )
regions.df <- data.frame(regions)


for (row in 1:nrow(regions.df)) {
  
  region_name <- as.character(regions.df[row, 1])
  
  clients_input_reg <- values_input %>% filter(Region==region_name)
  
  clients_out1_reg <- clients_out1 %>% filter(Region==region_name)
  clients_out2_reg <- clients_out2 %>% filter(Region==region_name)
  clients_out3_reg <- clients_out3 %>% filter(Region==region_name)
  clients_out5_reg <- clients_out5 %>% filter(Region==region_name)
  clients_out6_reg <- clients_out6 %>% filter(Region==region_name)
  clients_out7_reg <- clients_out7 %>% filter(Region==region_name)
  clients_out8_reg <- clients_out8 %>% filter(Region==region_name)
  
  emu_trends_reg <- emu_trends  %>% filter(Region==region_name)
  emu_annual_change_reg <- emu_annual_change %>% filter(Region==region_name)
  emu_users_reg <- emu_users %>% filter(Region==region_name)
  
  
  #################################################################
  # Input 
  ################################################################
  
  g_clients_input <- ggplot(clients_input_reg, aes(x=Year, y=Value, fill=Method_Det_name))+
    geom_bar(stat="identity") +
    scale_fill_manual(values = c("#FED98E"  , "#993404", "#333300",  "#999999", "#FEE6CE",   "#FDAE6B",  
                                 "#E6550D", 
                                 "#DADAEB", "#BCBDDC" , "#9E9AC8", "#756BB1", "#54278F",
                                 "#A1D99B" ,  "#31A354" ,    "#D7B5D8",  
                                 "#DF65B0" , "#DD1C77",  "#980043", 
                                 "#FCAE91", "#FB6A4A", "#CB181D",
                                 "#9ECAE1" ,    "#3182BD"  )) +
    facet_wrap(~fct_reorder(GraphMethod, GraphNum), scales = "free_y") +
    scale_y_continuous(limits = function(x){c(0, max(1, x))}) +
    labs(title=paste("Clients Input Data", region_name, sep=": "), x="", y="", fill="") +
    guides(fill = guide_legend(ncol = 1)) +
    theme_bw()
  
  #################################################################
  # Output 1
  ################################################################
  
  g_clients_out1 <-   ggplot(clients_out1_reg, aes(x=year, y=Value, color=Variable_name))+
    geom_point(size=4)+
    geom_line(size=2)+
    scale_color_manual(values = c("#954F72", "#4A2739" , "#C490AA")) +
    labs(title="Clients 1. Total Users Trends, FPET Estimates vs.\nService Statistics Estimates (Adjusted and Unadjusted)", subtitle =region_name, x="", y="", color="") +
    guides(color = guide_legend(nrow = 2)) +
    theme_bw()+
    theme(legend.position = "bottom",
          plot.title.position = "plot")
  
  #################################################################
  # Output 2
  ################################################################
  
  g_clients_out2 <-  ggplot(subset(clients_out2_reg, year==2020), aes(x=fct_reorder(Variable_name, -Variable_num), y=Value, fill=fct_reorder(Method_G, -Method_Num)))+
    geom_bar(stat="identity") +
    coord_flip() +
    scale_fill_manual(breaks= c("Sterilization (female)" ,
                                "Sterilization (male)" ,  
                                "IUD"     ,              
                                "Implant" ,          
                                "Injectable" ,         
                                "OC Pills" ,              
                                "Condom (m+f)" ,          
                                "LAM"   ,               
                                "Other Modern Methods"  ), 
                      values = c(    "#954F72" , "#4A2739", "#0B9444" , "#8DC645",   "#1D4E92", "#3892C6", "#F49100", "#D8B5C7", "#FCD116" )) +
    guides(fill = guide_legend(nrow = 3)) +
    labs(title="Clients 2. Comparing Estimated Total FP Users, Survey/FPET vs.\nService Statistics Estimates (Adjusted and Unadjusted)", subtitle = paste(region_name, ": 2020", sep=""), x="", y="", fill="") +
    theme_bw()+
    theme(legend.position = "bottom",
          plot.title.position = "plot")
  
  #################################################################
  # Output 3
  ################################################################
  
  g_clients_out3 <-    ggplot(clients_out3_reg, aes(x=year, y=users, color=Method_G))+
    geom_line(size=2)+
    scale_color_manual(breaks= c("Sterilization (female)" ,
                                 "Sterilization (male)" ,  
                                 "IUD"     ,              
                                 "Implant" ,          
                                 "Injectable" ,         
                                 "OC Pills" ,              
                                 "Condom (m+f)" ,          
                                 "LAM"   ,               
                                 "Other Modern Methods"  ), 
                       values = c(    "#954F72" , "#4A2739", "#0B9444" , "#8DC645",   "#1D4E92", "#3892C6", "#F49100", "#D8B5C7", "#FCD116" )) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(title="Clients 3. Users Trends by Method", subtitle = region_name, x="", y="Users", color="") +
    theme_bw()+
    theme(legend.position = "bottom",
          plot.title.position = "plot")
  
  #################################################################
  # Output 4
  ################################################################
  
  g_clients_out4 <-   ggplot(subset(clients_out2_reg, Year==2020 & Variable_num!=1), aes(x=fct_reorder(Variable_name, -Variable_num), y=Value, fill=fct_reorder(Method_G, -Method_Num)))+
    geom_bar(stat="identity") +
    coord_flip() +
    scale_fill_manual(breaks= c("Sterilization (female)" ,
                                "Sterilization (male)" ,  
                                "IUD"     ,              
                                "Implant" ,          
                                "Injectable" ,         
                                "OC Pills" ,              
                                "Condom (m+f)" ,          
                                "LAM"   ,               
                                "Other Modern Methods"  ), 
                      values = c(    "#954F72" , "#4A2739", "#0B9444" , "#8DC645",   "#1D4E92", "#3892C6", "#F49100", "#D8B5C7", "#FCD116" )) +
    guides(fill = guide_legend(nrow = 3)) +
    labs(title="Clients 4. Comparing Estimated Total FP Users:\nService Statistics and Surveys", subtitle = paste(region_name, ": 2020", sep=""), x="", y="", fill="") +
    theme_bw()+
    theme(legend.position = "bottom",
          plot.title.position = "plot")
  
  #################################################################
  # Output 5
  ################################################################

  g_clients_out5 <-   ggplot(subset(clients_out5_reg, year==2020 ), aes(x=fct_reorder(Method_Name, Method_Num), y=Value, fill=fct_reorder(Variable_name, Variable_num)))+
    geom_bar(stat="identity",  position='dodge') +
    geom_text(aes(label=round(Value, -2)), position=position_dodge(width=0.9), vjust=-0.25, size=2) +
    scale_fill_manual(values = c("#0B9444" , "#954F72"   )) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(title="Clients 5. Comparing Estimate of Modern Users by Method:\nService Statistics vs. Survey", subtitle = paste(region_name, ": 2020", sep=""), x="", y="Users", fill="") +
    theme_bw()+
    theme(legend.position = "bottom",
          plot.title.position = "plot")
  
  #################################################################
  # Output 6
  ################################################################

  g_clients_out6 <-   ggplot(subset(clients_out6_reg, year==2020), aes(x="", y=Value, fill=fct_reorder(Method_G, Method_Num)))+
    geom_bar(width = 1, stat = "identity")+ 
    scale_fill_manual(values = c(  "#954F72" , "#4A2739", "#0B9444" , "#8DC645",   "#1D4E92", "#3892C6", "#F49100", "#D8B5C7", "#FCD116"  )) +
    coord_polar("y", start=0) +
    facet_grid(facets = .~Variable_Lab, labeller = label_value) +
    geom_text(aes(x="", y=pos, label = Label), size=3) +  # note y = pos
    labs(title="Clients 6. Comparing Method Mix:\nService Statistics vs. Survey", subtitle = paste(region_name, ": 2020", sep=""), x="", y="", fill="") +
    guides(fill = guide_legend(nrow = 3)) +
    theme_bw()+
    theme(legend.position = "bottom") +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x=element_blank(),
      strip.background = element_blank(),
      panel.grid=element_blank())
  
  
  #################################################################
  # Output 7
  ################################################################
  
  g_clients_out7 <-    ggplot(clients_out7_reg, aes(x=year, y=Value, color=Variable_name))+
    geom_point(size=5)+
    geom_line(size=2) +
    scale_color_manual(values = c("#8DC645", "#0B9444", "#954F72", "#C490AA", "#4A2739", "#1D4E92" )) +
    labs(title="Clients 7. Comparing User Trends: EMU (Service Statistics) and\nSurveys (FPET)", subtitle = region_name, x="", y="", color="") +
    guides(color = guide_legend(nrow = 2)) +
    theme_bw()+
    theme(legend.position = "bottom",
          plot.title.position = "plot")
  
  #################################################################
  # Output 8
  ################################################################
  
  g_clients_out8 <-   ggplot(clients_out8_reg, aes(x=fct_reorder(Variable_name, Variable_num), y=Ave_Change, fill=Variable_name))+
    geom_bar(stat="identity") +
    geom_text(aes(label=Ave_Change_Label, hjust=hjust_val), position=position_dodge(width=0.9), vjust=0.5) +
    coord_flip() +
    scale_fill_manual( values = c(    "#8DC645", "#0B9444", "#954F72", "#C490AA", "#4A2739", "#1D4E92")) +
    scale_y_continuous(limits = c(min(clients_out8$Ave_Change)- (max(clients_out8$Ave_Change)-min(clients_out8$Ave_Change))/10 , max(clients_out8$Ave_Change) + (max(clients_out8$Ave_Change)-min(clients_out8$Ave_Change))/10)) + 
    labs(title="Clients 8. Comparing Growth Rates:\nEMU (Service Statistics) and Surveys (FPET)", subtitle = region_name,  x="", y="", fill="") +
    theme_bw()+
    theme(legend.position = "none",
          plot.title.position = "plot")
  
  #################################################################
  # EMU Outputs 
  ################################################################
  g_emu_1 <-     ggplot(emu_trends_reg, aes(x=year, y=Value, color=Variable_name))+
    geom_point(size=4)+
    geom_line(size=2) +
    scale_color_manual( values = c(    "#FC9272", "#DE2D26",  "#BCBDDC", "#756BB1",  "#9ECAE1", "#3182BD" , "#FDAE6B", "#E6550D",  "#A1D99B", "#74C476", "#31A354" , "#006D2C")) +
    labs(title="EMU 1. Comparing EMUs and mCPR (AW)", subtitle = region_name, x="", y="", color="") +
    theme_bw()+
    theme(legend.position = "bottom",
          plot.title.position = "plot")
  
  g_emu_2 <- ggplot(emu_annual_change_reg, aes(x=fct_reorder(Variable_name, Variable_num), y=Ave_Change, fill=Variable_name))+
    geom_bar(stat="identity") +
    geom_text(aes(label=Ave_Change_Label, hjust=hjust_val), position=position_dodge(width=0.9), vjust=0.5) +
    coord_flip() +
    scale_fill_manual( values = c(    "#FC9272", "#DE2D26",  "#BCBDDC", "#756BB1",  "#9ECAE1", "#3182BD" , "#FDAE6B", "#E6550D",  "#A1D99B", "#74C476", "#31A354" , "#006D2C")) +
    scale_y_continuous(limits = c(min(emu_annual_change$Ave_Change)- (max(emu_annual_change$Ave_Change)-min(emu_annual_change$Ave_Change))/10 , max(emu_annual_change$Ave_Change) + (max(emu_annual_change$Ave_Change)-min(emu_annual_change$Ave_Change))/10)) + 
    labs(title="EMU 2. Comparing Average Annual % Point Growth in mCPR/EMU", subtitle = region_name, x="", y="", fill="") +
    theme_bw()+
    theme(legend.position = "none",
          plot.title.position = "plot")
  
  g_emu_3 <-  ggplot(subset(emu_users_reg, year==2020), aes(x=Variable_Name, y=Value, fill=fct_reorder(Method_G, -Method_Num)))+
    geom_bar(stat="identity") +
    coord_flip() +
    scale_fill_manual(breaks= c("Sterilization (female)" ,
                                "Sterilization (male)" ,  
                                "IUD"     ,              
                                "Implant" ,          
                                "Injectable" ,         
                                "OC Pills" ,              
                                "Condom (m+f)" ,          
                                "LAM"   ,               
                                "Other Modern Methods"  ), 
                      values = c(    "#954F72" , "#4A2739", "#0B9444" , "#8DC645",   "#1D4E92", "#3892C6", "#F49100", "#D8B5C7", "#FCD116" )) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(title="EMU 3. Comparing Comparing Modern Method Users by Method", subtitle = paste(region_name, ": 2020", sep=""), x="", y="", fill="") +
    theme_bw()+
    theme(legend.position = "bottom",
          plot.title.position = "plot")
  
  g_emu_4 <-  ggplot(subset(emu_users_reg, Variable!="survey_users"), aes(x=year, y=Value, color=Variable_Name)) +
    geom_line(size=1) +
    facet_wrap(~Method_G, scales="free", ncol = 3) +
    scale_color_manual( values = c( "#DE2D26",  "#756BB1", "#3182BD" , "#E6550D"   )) +
    labs(title="EMU 4. Trends in Users by Method", subtitle = region_name, x="", y="", color="") +
    theme_bw()+
    theme(legend.position = "bottom",
          plot.title.position = "plot")
  
  clients_output_1 <- ggdraw() +
    draw_plot(g_clients_out1, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(g_clients_out2, x = .5, y = .5,  width = .5, height = .5) +
    draw_plot(g_clients_out3, x = 0, y = 0, width = .5, height = .5) +
    draw_plot(g_clients_out4, x = .5, y = 0,   width = .5, height = .5) 
  
  clients_output_2 <- ggdraw() +
    draw_plot(g_clients_out5, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(g_clients_out6, x = .5, y = .5,  width = .5, height = .5) +
    draw_plot(g_clients_out7, x = 0, y = 0, width = .5, height = .5) +
    draw_plot(g_clients_out8, x = .5, y = 0,   width = .5, height = .5)
  
  multi.page <- ggarrange(g_clients_input,
                          clients_output_1,
                          clients_output_2,
                          g_emu_1, g_emu_2, g_emu_3,
                          g_emu_4, nrow=1, ncol=1) # for one plot per page 
  
  ggexport(multi.page, filename=paste("Results/KristoniaSubnational_" , region_name,  "_SStoEMU Results.pdf", sep=""), height=8, width=12)
  
}


# Can use the code below to combine all the regions' PDFs together
pdf_combine(c("Results/KristoniaSubnational_Alexandria_SStoEMU Results.pdf" ,
              "Results/KristoniaSubnational_Dorothyville_SStoEMU Results.pdf" ,
              "Results/KristoniaSubnational_Hagridland_SStoEMU Results.pdf" ,
              "Results/KristoniaSubnational_Port_Alvin_SStoEMU Results.pdf"  ), output = "Results/Kristonia Subnatioanl EMU Results Unadjusted Condoms.pdf")

# Export EMU csv files
write.csv(emu_trends_wide, "Results/EMU Subnational Kristonia.csv", row.names = F, na="")
write.csv(EMU_Annual, "Results/EMU Totals Subnational Kristonia.csv", row.names = F, na="")

