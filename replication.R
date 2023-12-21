#############################
#### Coups and Democracy ####
#############################
#### Version 08/21/2021 #####
#############################
#Author: Kevin Koehler 
#k.koehler@fsw.leidenuniv.nl
#############################

library(foreign)
library(tidyverse)
library(lubridate)

### Load CAM data from militarycoups.org
### Also see the codebook at https://militarycoups.org/Codebook-v3.0.html
cam <- read.dta("https://militarycoups.org/ISQ_rep.dta")

### Convert date variables to R date format
cam <- cam %>%
  mutate(date1=as.Date(date1, origin="1960-01-01")) %>%
  mutate(date2=as.Date(date2, origin="1960-01-01")) %>%
  mutate(date3=as.Date(date3, origin="1960-01-01")) %>%
  mutate(date4=as.Date(date4, origin="1960-01-01")) 


### Figure 1: Coups by Region ###
figure1.data <- cam %>%
  group_by(region) %>%
  mutate(combatcoups=sum(combat1,combat2,combat3,combat4,na.rm = T)) %>%
  mutate(coups=sum(coup1,coup2,coup3,coup4,na.rm=T)) %>%
  mutate(successes=sum(successful1,successful2,successful3,successful4,na.rm = T))%>%
  select(region, combatcoups, coups,successes) %>%
  filter(!duplicated(region)) %>%
  mutate(region=factor(region, 
                       levels=c(1,2,3,4,5),
                       labels=c("Europe",
                                "MENA",
                                "Sub-Saharan Africa",
                                "Asia",
                                "Americas"))) %>%
  pivot_longer(combatcoups:successes,
               names_to="type",
               values_to="number") 

figure1.data$order <- c(11,10,12,2,1,3,14,13,15,8,7,9,5,4,6)
figure1.data <- figure1.data %>%
  mutate(order=order*-1) %>%
  arrange(order) %>%
  mutate(type=case_when(
    type=="coups"~"Any coup",
    type=="combatcoups"~"Combat officer coups",
    type=="successes"~"Successful coups"
  ))

fig1 <- ggplot(figure1.data, 
               aes(x=reorder(region, order),
                   y=number,
                   fill=reorder(type,order))) +
  geom_text(aes(label=number),
            position=position_dodge(width=1),
            hjust=-0.25,
            size=3,
            family="Times New Roman")+
  geom_bar(stat = "identity", position="dodge") +
  guides(fill = guide_legend(reverse=TRUE))+
  scale_fill_manual(values=c("#666666", "#999999", "#333333")) +
  labs(fill = "") +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", color="black")) +
  theme(legend.position="bottom") +
  theme(axis.text.x= element_text(colour="black", size="10", family="Times New Roman")) +
  theme(axis.text.y= element_text(colour="black", size="10", family="Times New Roman")) +
  xlab("") +
  ylab("") +
  ylim(0,200) +
  coord_flip() 
fig1

## Save eps in 300 dpi for publication
ggsave("Figure1.eps", dpi=300, width = 6, height = 6)

### Figure 2: Success Rates
figure2.data <- cam %>%
  group_by(cowcode, year) %>%
  mutate(coups=sum(coup1,coup2,coup3,coup4,na.rm=T)) %>%
  mutate(successes=sum(successful1,successful2,successful3,successful4, na.rm=T))%>%  
  mutate(combatcoups=sum(combat1,combat2,combat3,combat4,na.rm = T)) %>%
  mutate(elitecoups=coups-combatcoups) %>%
  mutate(combatsuccess1=combat1*successful1) %>%
  mutate(combatsuccess2=combat2*successful2) %>%
  mutate(combatsuccess3=combat3*successful3) %>%
  mutate(combatsuccess4=combat4*successful4) %>%
  mutate(combatsuccesses=sum(combatsuccess1,combatsuccess2,combatsuccess3,combatsuccess4,na.rm = T)) %>%
  mutate(elitesuccesses=successes-combatsuccesses) %>%
  mutate(combatfailures=combatcoups-combatsuccesses) %>%
  mutate(elitefailures=elitecoups-elitesuccesses) %>%
  ungroup %>%
  mutate(fyp=case_when(
    year>=1950 & year<1955~"1950-1954",
    year>=1955 & year<1960~"1955-1959",
    year>=1960 & year<1965~"1960-1964",
    year>=1965 & year<1970~"1965-1969",
    year>=1970 & year<1975~"1970-1974",
    year>=1975 & year<1980~"1975-1979",
    year>=1980 & year<1985~"1980-1984",
    year>=1985 & year<1990~"1985-1989",
    year>=1990 & year<1995~"1990-1994",
    year>=1995 & year<2000~"1995-1999",
    year>=2000 & year<2005~"2000-2004",
    year>=2005 & year<2010~"2005-2009",
    year>=2010 & year<2015~"2010-2014",
    year>=2015 & year<2018~"2014-2017",
  )) %>%
  filter(!is.na(fyp)) %>%
  select(combatsuccesses, combatfailures, elitesuccesses, elitefailures, fyp) %>%
  group_by(fyp)%>%
  mutate(combatsuccesses=sum(combatsuccesses)) %>%
  mutate(combatfailures=sum(combatfailures)) %>%
  mutate(elitesuccesses=sum(elitesuccesses)) %>%
  mutate(elitefailures=sum(elitefailures)) %>%
  filter(!duplicated(fyp)) %>%
  pivot_longer(combatsuccesses:elitefailures,
               names_to="type",
               values_to="number") %>%
  mutate(label=ifelse(number>0,number,NA))

fig2 <- ggplot(figure2.data,
               aes(x=fyp, y=number,fill=type))+
  geom_bar(stat = "identity",
           position="stack",
           color="white") +
  geom_text(aes(label=label),
            position="stack",
            hjust=1.5,
            size=2,
            color="white",
            family="Times New Roman")+
  scale_x_discrete(limits=rev)+
  theme_classic() +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", color="black")) +
  theme(legend.position="bottom") +
  theme(axis.text.x= element_text(colour="black", size="10", family="Times New Roman")) +
  theme(axis.text.y= element_text(colour="black", size="10", family="Times New Roman")) +
  guides(fill = guide_legend(nrow=2,byrow=TRUE))+
  scale_fill_manual(name="", 
                    values=c("elitesuccesses"="#000000",
                             "elitefailures"="#333333",
                             "combatsuccesses"="#666666",
                             "combatfailures"="#999999"),
                    labels=c("Elite success",
                             "Elite failure",
                             "Combat success",
                             "Combat failure")) +
  xlab("") +
  ylab("") +
  coord_flip()
fig2

## Save eps in 300 dpi for publication
ggsave("Figure2.eps", dpi=300, width = 6, height = 6)

### Merge GWF data, transform data for analysis
### GWF data from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/3IC00L

leader <- read.dta("leader.dta") %>%
  select(cowcode, year, ldr_duration, ldr_id, jan1leader, caseid)
data <- read.dta("GWFtscs2015.dta") %>%
  left_join(leader, by=c("cowcode","year")) %>%
  full_join(cam, by=c("cowcode","year")) %>%
  mutate(gwf_enddate=as.Date(gwf_enddate, format="%d/%m/%Y")) %>%
  mutate(gwf_startdate=as.Date(gwf_startdate, format="%d/%m/%Y")) %>%
  filter(!is.na(gwf_casename)) %>%
  mutate(combat2=case_when(
    !is.na(combat1) & is.na(combat2)~0
  )) %>%
  mutate(combat3=case_when(
    !is.na(combat2) & is.na(combat3)~0
  )) %>%
  mutate(combat4=case_when(
    !is.na(combat3) & is.na(combat4)~0
  )) %>%
  mutate(anycoup=ifelse(coup1+coup2+coup3+coup4>0,1,0)) %>%
  mutate(combatcoup=ifelse(combat1+combat2+combat3+combat4>0,1,0)) %>%
  mutate(elitecoup=ifelse(combat1+combat2+combat3+combat4==0,1,0)) %>%
  mutate(combatcoup=case_when(
    is.na(combatcoup)& !is.na(anycoup)~0,
    TRUE~combatcoup
  )) %>%
  mutate(elitecoup=case_when(
    is.na(elitecoup) & !is.na(anycoup)~0,
    TRUE~elitecoup
  )) %>%
  mutate(flag1=case_when(
    month(gwf_enddate)<month(date1) & gwf_fail==1~1
  )) %>%
  mutate(flag2=case_when(
    month(gwf_enddate)<month(date2) & gwf_fail~1
  )) %>%
  mutate(flag3=case_when(
    month(gwf_enddate)<month(date3) & gwf_fail~1
  )) %>%
  mutate(flag4=case_when(
    month(gwf_enddate)<month(date4) & gwf_fail~1
  )) %>%
  mutate(flag1=case_when(
    month(gwf_enddate)==month(date1) & 
      day(date1) > day(gwf_enddate)+2 & 
      gwf_fail==1~1,
    TRUE~flag1
  )) %>%
  mutate(flag2=case_when(
    month(gwf_enddate)==month(date2) & 
      day(date2) > day(gwf_enddate)+2 & 
      gwf_fail==1~1,
    TRUE~flag2
  )) %>%
  mutate(flag3=case_when(
    month(gwf_enddate)==month(date3) & 
      day(date3) > day(gwf_enddate)+2 & 
      gwf_fail==1~1,
    TRUE~flag3
  )) %>%
  mutate(flag4=case_when(
    month(gwf_enddate)==month(date4) & 
      day(date4) > day(gwf_enddate)+2 & 
      gwf_fail==1~1,
    TRUE~flag4
  )) %>%
  mutate(coup1=as.double(coup1))%>%
  mutate(coup2=as.double(coup2))%>%
  mutate(coup3=as.double(coup3))%>%
  mutate(coup4=as.double(coup4))%>%
  mutate(coup1=case_when(
    flag1==1~0,
    TRUE~coup1
  )) %>%
  mutate(coup2=case_when(
    flag2==1~0,
    TRUE~coup2
  )) %>%
  mutate(coup3=case_when(
    flag3==1~0,
    TRUE~coup3
  )) %>%
  mutate(coup4=case_when(
    flag4==1~0,
    TRUE~coup4
  )) %>%
  mutate(date1=ifelse(coup1==0,NA,date1)) %>%
  mutate(date2=ifelse(coup2==0,NA,date1)) %>%
  mutate(date3=ifelse(coup3==0,NA,date1)) %>%
  mutate(date4=ifelse(coup4==0,NA,date1)) %>%
  mutate(anycoup=ifelse(coup1+coup2+coup3+coup4>0,1,0)) %>%
  mutate(combatcoup=ifelse(combat1+combat2+combat3+combat4>0,1,0)) %>%
  mutate(elitecoup=ifelse(combat1+combat2+combat3+combat4==0,1,0)) %>%
  mutate(combatcoup=case_when(
    is.na(combatcoup)& !is.na(anycoup)~0,
    TRUE~combatcoup
  )) %>%
  mutate(elitecoup=case_when(
    is.na(elitecoup) & !is.na(anycoup)~0,
    TRUE~elitecoup
  )) %>%
  mutate(anysuccess=case_when(
    successful1==1 | successful2==1 | successful3==1 | successful4==1~1,
    successful1==0~0,
    successful1==0 & successful2==0~0,
    TRUE~NA_real_
  )) %>%
  mutate(elitesuc1=ifelse(combat1==0 & successful1==1,1,0)) %>%
  mutate(elitesuc2=ifelse(combat2==0 & successful2==1,1,0)) %>%
  mutate(elitesuc3=ifelse(combat3==0 & successful3==1,1,0)) %>%
  mutate(elitesuc4=ifelse(combat4==0 & successful4==1,1,0)) %>%
  mutate(elitesuccess=case_when(
    elitesuc1==1 | elitesuc2==1 | elitesuc3==1 | elitesuc4==1~1,
    elitesuc1==0~0,
    elitesuc1==0 & elitesuc2==0~0,
    TRUE~NA_real_
  )) %>%
  mutate(combatsuc1=ifelse(combat1==1 & successful1==1,1,0)) %>%
  mutate(combatsuc2=ifelse(combat2==1 & successful2==1,1,0)) %>%
  mutate(combatsuc3=ifelse(combat3==1 & successful3==1,1,0)) %>%
  mutate(combatsuc4=ifelse(combat4==1 & successful4==1,1,0)) %>%
  mutate(combatsuccess=case_when(
    combatsuc1==1 | combatsuc2==1 | combatsuc3==1 | combatsuc4==1~1,
    combatsuc1==0~0,
    combatsuc1==0 & combatsuc2==0~0,
    TRUE~NA_real_
  )) %>%
  mutate(fail1=ifelse(successful1==0,1,NA))%>% 
  mutate(fail2=ifelse(successful2==0,1,NA))%>% 
  mutate(fail3=ifelse(successful3==0,1,NA))%>% 
  mutate(fail4=ifelse(successful4==0,1,NA))%>% 
  mutate(anyfailure=case_when(
    fail1==1 | fail2==1 | fail3==1 | fail4==1~1,
    fail1==0~0,
    fail1==0 & fail2==0~0,
    TRUE~NA_real_
  )) %>%
  mutate(elitefail1=ifelse(combat1==0 & fail1==1,1,0)) %>%
  mutate(elitefail2=ifelse(combat2==0 & fail2==1,1,0)) %>%
  mutate(elitefail3=ifelse(combat3==0 & fail3==1,1,0)) %>%
  mutate(elitefail4=ifelse(combat4==0 & fail4==1,1,0)) %>%
  mutate(elitefailure=case_when(
    elitefail1==1 | elitefail2==1 | elitefail3==1 | elitefail4==1~1,
    elitefail1==0~0,
    elitefail1==0 & elitefail2==0~0,
    TRUE~NA_real_
  )) %>%
  mutate(combatfail1=ifelse(combat1==1 & fail1==1,1,0)) %>%
  mutate(combatfail2=ifelse(combat2==1 & fail2==1,1,0)) %>%
  mutate(combatfail3=ifelse(combat3==1 & fail3==1,1,0)) %>%
  mutate(combatfail4=ifelse(combat4==1 & fail4==1,1,0)) %>%
  mutate(combatfailure=case_when(
    combatfail1==1 | combatfail2==1 | combatfail3==1 | combatfail4==1~1,
    combatfail1==0~0,
    combatfail1==0 & combatfail2==0~0,
    TRUE~NA_real_
  )) %>%
  select(-flag1, -flag2, -flag3, -flag4, 
         -elitesuc1, -elitesuc2, -elitesuc3, -elitesuc4,
         -combatsuc1, -combatsuc2, -combatsuc3, -combatsuc4,
         -elitefail1, -elitefail2, -elitefail3, -elitefail4,
         -combatfail1, -combatfail2, -combatfail3, -combatfail4,
         -fail1,-fail2,-fail3,-fail4,
         -date1, -date2, -date3, -date4,
         -coup1, -coup2, -coup3, -coup4,
         -successful1, -successful2, -successful3, -successful4,
         -combat1, -combat2, -combat3, -combat4) %>%
  mutate(anysuccess=ifelse(is.na(anysuccess),0,anysuccess)) %>%
  mutate(elitesuccess=ifelse(is.na(elitesuccess),0,elitesuccess)) %>%
  mutate(combatsuccess=ifelse(is.na(combatsuccess),0,combatsuccess)) %>%
  mutate(anyfailure=ifelse(is.na(anyfailure),0,anyfailure)) %>%
  mutate(elitefailure=ifelse(is.na(elitefailure),0,elitefailure)) %>%
  mutate(combatfailure=ifelse(is.na(combatfailure),0,combatfailure))

### 3-year variables
data <- data %>%
  group_by(cowcode) %>%
  arrange(cowcode, year) %>%
  mutate(anycoup=ifelse((anycoup+lag(anycoup,1)+lag(anycoup,2))>0,1,0)) %>%
  mutate(anysuccess=ifelse((anysuccess+lag(anysuccess,1)+lag(anysuccess,2))>0,1,0)) %>%
  mutate(anyfailure=ifelse((anyfailure+lag(anyfailure,1)+lag(anyfailure,2))>0,1,0)) %>%
  mutate(elitecoup=ifelse((elitecoup+lag(elitecoup,1)+lag(elitecoup,2))>0,1,0)) %>%
  mutate(elitesuccess=ifelse((elitesuccess+lag(elitesuccess,1)+lag(elitesuccess,2))>0,1,0)) %>%
  mutate(elitefailure=ifelse((elitefailure+lag(elitefailure,1)+lag(elitefailure,2))>0,1,0)) %>%
  mutate(combatcoup=ifelse((combatcoup+lag(combatcoup,1)+lag(combatcoup,2))>0,1,0)) %>%
  mutate(combatsuccess=ifelse((combatsuccess+lag(combatsuccess,1)+lag(combatsuccess,2))>0,1,0)) %>%
  mutate(combatfailure=ifelse((combatfailure+lag(combatfailure,1)+lag(combatfailure,2))>0,1,0)) %>%
  mutate(anycoup=ifelse(is.na(anycoup),0,anycoup))%>%
  mutate(anysuccess=ifelse(is.na(anysuccess),0,anysuccess))%>%
  mutate(anyfailure=ifelse(is.na(anyfailure),0,anyfailure))%>%
  mutate(elitecoup=ifelse(is.na(elitecoup),0,elitecoup))%>%
  mutate(elitesuccess=ifelse(is.na(elitesuccess),0,elitesuccess))%>%
  mutate(elitefailure=ifelse(is.na(elitefailure),0,elitefailure))%>%
  mutate(combatcoup=ifelse(is.na(combatcoup),0,combatcoup))%>%
  mutate(combatsuccess=ifelse(is.na(combatsuccess),0,combatsuccess))%>%
  mutate(combatfailure=ifelse(is.na(combatfailure),0,combatfailure))

### Thyne & Powell data 
#from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/RYPUEF
pt <- read.dta("thyne_powell.dta") %>%
  rename(cowcode=ccode) %>%
  select(cowcode, year, prev_dem, pdem, gdppc, chgdppc)
data <- left_join(data, pt, by=c("cowcode","year"))

### Variable construction
### The corrections are from DFGW (see STATA do file at https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/3IC00L)
data <- data %>%
  mutate(gdem=case_when(
    gwf_fail_subsregime==1~1,
    gwf_fail_subsregime!=1~0,
    is.na(gwf_fail_subsregime)~NA_real_)) %>%
  mutate(gdem=case_when(
    cowcode==365 & year==1991~1,
    cowcode==265 & year==1990~1,
    TRUE~gdem
  )) %>%
  mutate(gdict=case_when(
    gwf_fail_subsregime==2~1,
    gwf_fail_subsregime!=2~0,
    is.na(gwf_fail_subsregime)~NA_real_)) %>%
  mutate(gdict=case_when(
    cowcode==365 & year==1991~0,
    cowcode==265 & year==1990~0,
    TRUE~gdict
  )) %>%
  mutate(gtime=as.double(gwf_duration)) %>%
  mutate(gtime=case_when(
    cowcode==530 & gtime>90~gtime-63,
    cowcode==550 & gtime>30~gtime-38,
    cowcode==698 & gtime>200~gtime-177,
    cowcode==790 & gtime>99~gtime-100, ##cowcode was 530 by mistake, Nepal is 790
    TRUE~gtime
  )) %>%
  mutate(gtime2=gtime^2) %>%
  mutate(gtime3=gtime^3) %>%
  mutate(ld=log(ldr_duration)) %>%
  mutate(coldwar=ifelse(year<1990,1,0))

### Models
library(sandwich)
library(lmtest)
library(stargazer)

logit1 <- glm(gdem~
                anycoup+
                gwf_military+
                gwf_personal+
                gwf_party+
                ld+
                prev_dem+
                pdem+
                gdppc+
                chgdppc +  
                coldwar+
                gtime+
                gtime2+
                gtime3,
              family="binomial",
              data=data)
nobs(logit1)
lrtest(logit1)

##Clustered SEs
vcm1 <- vcovCL(logit1, cluster=data$caseid)
m1 <- coeftest(logit1, vcov=vcm1)

logit2 <- glm(gdem~
                combatcoup+
                elitecoup+
                gwf_military+
                gwf_personal+
                gwf_party+
                ld+
                prev_dem+
                pdem+
                gdppc+
                chgdppc +  
                coldwar+
                gtime+
                gtime2+
                gtime3,
              family = "binomial",
              data=data)
nobs(logit2)
lrtest(logit2)
##Clustered SEs
vcm2 <- vcovCL(logit2, cluster=data$caseid)
m2 <- coeftest(logit2, vcov=vcm2)

logit3 <- glm(gdict~
                anycoup+
                gwf_military+
                gwf_personal+
                gwf_party+
                ld+
                prev_dem+
                pdem+
                gdppc+
                chgdppc +  
                coldwar+
                gtime+
                gtime2+
                gtime3,
              family="binomial",
              data=data)
nobs(logit3)
lrtest(logit3)
##Clustered SEs
vcm3 <- vcovCL(logit3, cluster=data$caseid)
m3 <- coeftest(logit3, vcov=vcm3)                    

logit4 <- glm(gdict~
                combatcoup+
                elitecoup+
                gwf_military+
                gwf_personal+
                gwf_party+
                ld+
                prev_dem+
                pdem+
                gdppc+
                chgdppc +  
                coldwar+
                gtime+
                gtime2+
                gtime3,
              family = "binomial",
              data=data)
nobs(logit4)
lrtest(logit4)
##Clustered SEs
vcm4 <- vcovCL(logit4, cluster=data$caseid)
m4 <- coeftest(logit4, vcov=vcm4)
table2 <- stargazer(m1, m2, m3, m4, 
                    type = "html",
                    covariate.labels = c(
                      "Any coup",
                      "Combat officer coup",
                      "Elite officer coup",
                      "Military regime",
                      "Personalist regime",
                      "Party regime",
                      "Leader duration (log)",
                      "Prior democracy",
                      "Proportion of democracies",
                      "GDP/capita (log)",
                      "Change GDP/capita",
                      "Cold War",
                      "Time",
                      "Time2",
                      "Time3",
                      "Constant"
                    ),
                    no.space = T,
                    star.cutoffs = c(0.05,0.01,0.001),
                    out = "table2.doc")

newdata0 <- newdata1 <- data
newdata0[,"combatcoup"] <- 0
newdata1[,"combatcoup"] <- 1
## combat coups increase predicted probability by 105% (Model 2)
((mean(predict(logit2, type="response", newdata = newdata1),na.rm = T) -
    mean(predict(logit2, type="response", newdata = newdata0),na.rm = T))/
    mean(predict(logit2, type="response", newdata = newdata0),na.rm = T))*100

#### Appendix
##Table A3
logit5 <- glm(gdem~
                anysuccess+
                gwf_military+
                gwf_personal+
                gwf_party+
                ld+
                prev_dem+
                pdem+
                gdppc+
                chgdppc +  
                coldwar+
                gtime+
                gtime2+
                gtime3,
              family="binomial",
              data=data)
nobs(logit5)
lrtest(logit5)
##Clustered SEs
vcm5 <- vcovCL(logit5, cluster=data$caseid)
m5 <- coeftest(logit5, vcov=vcm5)

logit6 <- glm(gdem~
                combatsuccess+
                elitesuccess+
                gwf_military+
                gwf_personal+
                gwf_party+
                ld+
                prev_dem+
                pdem+
                gdppc+
                chgdppc +  
                coldwar+
                gtime+
                gtime2+
                gtime3,
              family = "binomial",
              data=data)
nobs(logit6)
lrtest(logit6)
##Clustered SEs
vcm6 <- vcovCL(logit6, cluster=data$caseid)
m6 <- coeftest(logit6, vcov=vcm6)

logit7 <- glm(gdict~
                anysuccess+
                gwf_military+
                gwf_personal+
                gwf_party+
                ld+
                prev_dem+
                pdem+
                gdppc+
                chgdppc +  
                coldwar+
                gtime+
                gtime2+
                gtime3,
              family="binomial",
              data=data)
nobs(logit7)
lrtest(logit7)
##Clustered SEs
vcm7 <- vcovCL(logit7, cluster=data$caseid)
m7 <- coeftest(logit7, vcov=vcm7)                    

logit8 <- glm(gdict~
                combatsuccess+
                elitesuccess+
                gwf_military+
                gwf_personal+
                gwf_party+
                ld+
                prev_dem+
                pdem+
                gdppc+
                chgdppc +  
                coldwar+
                gtime+
                gtime2+
                gtime3,
              family = "binomial",
              data=data)
nobs(logit8)
lrtest(logit8)
##Clustered SEs
vcm8 <- vcovCL(logit8, cluster=data$caseid)
m8 <- coeftest(logit8, vcov=vcm8)
tableA3 <- stargazer(m5, m6, m7, m8, 
                     type = "html",
                     covariate.labels = c(
                       "Any coup",
                       "Combat officer coup",
                       "Elite officer coup",
                       "Military regime",
                       "Personalist regime",
                       "Party regime",
                       "Leader duration (log)",
                       "Prior democracy",
                       "Proportion of democracies",
                       "GDP/capita (log)",
                       "Change GDP/capita",
                       "Cold War",
                       "Time",
                       "Time2",
                       "Time3",
                       "Constant"
                     ),
                     no.space = T,
                     star.cutoffs = c(0.05,0.01,0.001),
                     out = "tableA3.doc")

#Table A1
library(xlsx)

cam <- read.dta("CAM_data_v3_new.dta", convert.dates = T) %>%
  rename(combat1=junior1) %>%
  rename(combat2=junior2) %>%
  rename(combat3=junior3) %>%
  rename(combat4=junior4) %>%
  rename(country_abb=state) %>%
  rename(country=state_name) %>%
  select(ccode,
         year,
         country,
         region,
         coup1,
         successful1,
         date1,
         combat1,
         coup2,
         successful2,
         date2,
         combat2,
         coup3,
         successful3,
         date3,
         combat3,
         coup4,
         successful4,
         date4,
         combat4) %>%
  mutate(date1=as.Date(date1, origin="1960-01-01")) %>%
  mutate(date2=as.Date(date2, origin="1960-01-01")) %>%
  mutate(date3=as.Date(date3, origin="1960-01-01")) %>%
  mutate(date4=as.Date(date4, origin="1960-01-01")) %>%
  rename(cowcode=ccode)

cam <- cam %>%
  setNames(paste0("cam_",names(.))) %>%
  rename(cowcode=cam_cowcode) %>%
  rename(year=cam_year)

pt <- read_delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_ccode_year.txt", delim = "\t", col_names = TRUE)
### Parsing failures, check if corrections still necessary (and if so, where):
pt$date4[1877] <- "03aug1981"
pt$date4[5988] <- "15dec1991"
pt$date1 <- dmy(pt$date1)
pt$date2 <- dmy(pt$date2)
pt$date3 <- dmy(pt$date3)
pt$date4 <- dmy(pt$date4)
pt <- pt %>%
  rename(cowcode=ccode)

cam  <- inner_join(cam,pt,by=c("cowcode","year"))

camvspt <- matrix(ncol=6, nrow=100)
m=0
for(n in 1:length(cam$cam_country)) {
  if(cam$coup1[n]==0 & cam$cam_coup1[n]==1) {
    m = m + 1
    camvspt[m,1] <- cam$cam_country[n]
    camvspt[m,2] <- "NA"
    camvspt[m,3] <- "NA"
    camvspt[m,4] <- format(cam$cam_date1[n], "%d-%b-%Y")
    if(cam$cam_combat1[n]==1 & cam$cam_successful1[n]==1) {
      camvspt[m,5] <- "combat (s)"
    }
    if(cam$cam_combat1[n]==0 & cam$cam_successful1[n]==1) {
      camvspt[m,5] <- "elite (s)"
    }
    if(cam$cam_combat1[n]==0 & cam$cam_successful1[n]==0) {
      camvspt[m,5] <- "elite (f)"
    }
    if(cam$cam_combat1[n]==1 & cam$cam_successful1[n]==0) {
      camvspt[m,5] <- "combat (f)"
    }
    camvspt[m,6] <- "Not in PT"
  }
}
for(n in 1:length(cam$cam_country)) {
  if(cam$coup1[n]>=1 & cam$cam_coup1[n]==0) {
    m = m + 1
    camvspt[m,1] <- cam$cam_country[n]
    camvspt[m,2] <- format(cam$date1[n], "%d-%b-%Y")
    if(cam$coup1[n]==2) {
      camvspt[m,3] <- "success"
    } 
    if(cam$coup1[n]==1) {
      camvspt[m,3] <- "failure"
    }
    camvspt[m,4] <- "NA"
    camvspt[m,5] <- "NA"
    camvspt[m,6] <- "Fill in reason!"
  }
}
for(n in 1:length(cam$cam_country)) {
  if(cam$coup2[n]==0 & cam$cam_coup2[n]==1) {
    m = m + 1
    camvspt[m,1] <- cam$cam_country[n]
    camvspt[m,2] <- "NA"
    camvspt[m,3] <- "NA"
    camvspt[m,4] <- format(cam$cam_date2[n], "%d-%b-%Y")
    if(cam$cam_combat2[n]==1 & cam$cam_successful2[n]==1) {
      camvspt[m,5] <- "combat (s)"
    }
    if(cam$cam_combat2[n]==0 & cam$cam_successful2[n]==1) {
      camvspt[m,5] <- "elite (s)"
    }
    if(cam$cam_combat2[n]==0 & cam$cam_successful2[n]==0) {
      camvspt[m,5] <- "elite (f)"
    }
    if(cam$cam_combat2[n]==1 & cam$cam_successful2[n]==0) {
      camvspt[m,5] <- "combat (f)"
    }
    camvspt[m,6] <- "Not in PT"
  }
}
for(n in 1:length(cam$cam_country)) {
  if(cam$coup2[n]>=1 & cam$cam_coup2[n]==0) {
    m = m + 1
    camvspt[m,1] <- cam$cam_country[n]
    camvspt[m,2] <- format(cam$date2[n], "%d-%b-%Y")
    if(cam$coup2[n]==2) {
      camvspt[m,3] <- "success"
    } 
    if(cam$coup2[n]==1) {
      camvspt[m,3] <- "failure"
    }
    camvspt[m,4] <- "NA"
    camvspt[m,5] <- "NA"
    camvspt[m,6] <- "Fill in reason!"
  }
}
for(n in 1:length(cam$cam_country)) {
  if(cam$coup3[n]==0 & cam$cam_coup3[n]==1) {
    m = m + 1
    camvspt[m,1] <- cam$cam_country[n]
    camvspt[m,2] <- "NA"
    camvspt[m,3] <- "NA"
    camvspt[m,4] <- format(cam$cam_date3[n], "%d-%b-%Y")
    if(cam$cam_combat3[n]==1 & cam$cam_successful3[n]==1) {
      camvspt[m,5] <- "combat (s)"
    }
    if(cam$cam_combat3[n]==0 & cam$cam_successful3[n]==1) {
      camvspt[m,5] <- "elite (s)"
    }
    if(cam$cam_combat3[n]==0 & cam$cam_successful3[n]==0) {
      camvspt[m,5] <- "elite (f)"
    }
    if(cam$cam_combat3[n]==1 & cam$cam_successful3[n]==0) {
      camvspt[m,5] <- "combat (f)"
    }
    camvspt[m,6] <- "Not in PT"
  }
}
for(n in 1:length(cam$cam_country)) {
  if(cam$coup3[n]>=1 & cam$cam_coup3[n]==0) {
    m = m + 1
    camvspt[m,1] <- cam$cam_country[n]
    camvspt[m,2] <- format(cam$date3[n], "%d-%b-%Y")
    if(cam$coup3[n]==2) {
      camvspt[m,3] <- "success"
    } 
    if(cam$coup3[n]==1) {
      camvspt[m,3] <- "failure"
    }
    camvspt[m,4] <- "NA"
    camvspt[m,5] <- "NA"
    camvspt[m,6] <- "Fill in reason!"
  }
}
for(n in 1:length(cam$cam_country)) {
  if(cam$coup4[n]==0 & cam$cam_coup4[n]==1) {
    m = m + 1
    camvspt[m,1] <- cam$cam_country[n]
    camvspt[m,2] <- "NA"
    camvspt[m,3] <- "NA"
    camvspt[m,4] <- format(cam$cam_date4[n], "%d-%b-%Y")
    if(cam$cam_combat4[n]==1 & cam$cam_successful4[n]==1) {
      camvspt[m,5] <- "combat (s)"
    }
    if(cam$cam_combat4[n]==0 & cam$cam_successful4[n]==1) {
      camvspt[m,5] <- "elite (s)"
    }
    if(cam$cam_combat4[n]==0 & cam$cam_successful4[n]==0) {
      camvspt[m,5] <- "elite (f)"
    }
    if(cam$cam_combat4[n]==1 & cam$cam_successful4[n]==0) {
      camvspt[m,5] <- "combat (f)"
    }
    camvspt[m,6] <- "Not in PT"
  }
}
for(n in 1:length(cam$cam_country)) {
  if(cam$coup4[n]>=1 & cam$cam_coup4[n]==0) {
    m = m + 1
    camvspt[m,1] <- cam$cam_country[n]
    camvspt[m,2] <- format(cam$date4[n], "%d-%b-%Y")
    if(cam$coup4[n]==2) {
      camvspt[m,3] <- "success"
    } 
    if(cam$coup4[n]==1) {
      camvspt[m,3] <- "failure"
    }
    camvspt[m,4] <- "NA"
    camvspt[m,5] <- "NA"
    camvspt[m,6] <- "Fill in reason!"
  }
}
## note that the table in the appendix has been completed manually, this code 
## only produces the basic version
camvspt <- as.data.frame(camvspt)
camvspt <- camvspt[!is.na(camvspt$V1),]
write.xlsx(camvspt, "CAMvsPT_rev.xlsx")

## Table A2
library(qwraps2)

cam <- read.dta("CAM_data_v3_new.dta", convert.dates = T) %>%
  rename(combat1=junior1) %>%
  rename(combat2=junior2) %>%
  rename(combat3=junior3) %>%
  rename(combat4=junior4) %>%
  rename(country_abb=state) %>%
  rename(country=state_name) %>%
  select(ccode,
         year,
         country,
         region,
         coup1,
         successful1,
         date1,
         combat1,
         coup2,
         successful2,
         date2,
         combat2,
         coup3,
         successful3,
         date3,
         combat3,
         coup4,
         successful4,
         date4,
         combat4) %>%
  mutate(date1=as.Date(date1, origin="1960-01-01")) %>%
  mutate(date2=as.Date(date2, origin="1960-01-01")) %>%
  mutate(date3=as.Date(date3, origin="1960-01-01")) %>%
  mutate(date4=as.Date(date4, origin="1960-01-01")) %>%
  rename(cowcode=ccode)

cam <- cam %>%
  setNames(paste0("cam_",names(.))) %>%
  rename(cowcode=cam_cowcode) %>%
  rename(year=cam_year)

## De Bruin 
# from https://www.prio.org/utility/DownloadFile.ashx?id=117&type=replicationfile

library(readstata13)

db <- read.dta13("debruin_wtbb_final.dta")
db <- db %>%
  mutate(year=year+1) %>%
  select(ccode,
         year,
         mth,
         day,
         success,
         leader,
         branch,
         general,
         colonelmajor,
         below,
         nonmilitary,
         rank)%>%
  mutate(date=paste(as.character(db$day), 
                    as.character(db$mth), 
                    as.character(db$year), sep="-"))%>%
  filter(!is.na(day)) %>%
  mutate(db_date=dmy(date)) %>%
  select(-date, - mth, -day) %>%
  mutate(leader=ifelse(leader=='',"missing",leader)) %>%
  mutate(branch=ifelse(branch=='',"missing",branch)) %>%
  rename(cowcode=ccode)

cam <- merge(cam,db,by=c("cowcode", "year")) 

### De Bruin has the 1966 coup in Sudan on 18 Dec--it actually happened on 28 Dec
cam$db_date[cam$state_name=="Sudan" & cam$year==1966] <- date("1966-12-28")

sum(!is.na(cam$general))
### De Bruin has rank information for 394 coups 
sum(cam$general, na.rm=T)
### 154 generals
sum(cam$colonelmajor, na.rm=T)
### 119 colonels/majors
sum(cam$below, na.rm=T)
### 70 below

cam$nonmilitary[cam$rank=="nonmilitary"] <- 1
cam$nonmilitary[cam$general==1] <- 0
cam$nonmilitary[cam$colonelmajor==1] <- 0
cam$nonmilitary[cam$below==1] <- 0
cam$nonmilitary[is.na(cam$nonmilitary) & cam$general==0] <- 1

### Philippines 1986 (July 6) should be coded zero on rank variables
cam$general[cam$country=="Philippines" & cam$mth==7 & cam$day==6 & cam$year==1986] <-0
cam$colonelmajor[cam$country=="Philippines" & cam$mth==7 & cam$day==6 & cam$year==1986] <-0
cam$below[cam$country=="Philippines" & cam$mth==7 & cam$day==6 & cam$year==1986] <-0

jWgen1 <- matrix(nrow=10301, ncol=3)
sWgen1 <- matrix(nrow=10301, ncol=3)
jWcm1 <- matrix(nrow=10301, ncol=3)
sWcm1 <- matrix(nrow=10301, ncol=3)
jWbl1 <- matrix(nrow=10301, ncol=3)
sWbl1 <- matrix(nrow=10301, ncol=3)
jWnm1 <- matrix(nrow=10301, ncol=3)
sWnm1 <- matrix(nrow=10301, ncol=3)
ncWgen <- matrix(nrow=10301, ncol=3)
ncWcm <- matrix(nrow=10301, ncol=3)
ncWbl <- matrix(nrow=10301, ncol=3)
ncWnm <- matrix(nrow=10301, ncol=3)

for(n in 1:length(cam$cowcode)) {
  print(n)
  jWgen1[n,1] <- cam$cowcode[n]
  jWgen1[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$general[n]))) {
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==1 & cam$general[n]==1 & (cam$cam_date1[n] >= cam$db_date[n]-3 & cam$cam_date1[n] <= cam$db_date[n]+3)) {
      jWgen1[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWgen1[n,1] <- cam$cowcode[n]
  sWgen1[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$general[n]))) {
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==0 & cam$general[n]==1 & (cam$cam_date1[n] >= cam$db_date[n]-3 & cam$cam_date1[n] <= cam$db_date[n]+3)) {
      sWgen1[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  jWcm1[n,1] <- cam$cowcode[n]
  jWcm1[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$colonelmajor[n]))) {
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==1 & cam$colonelmajor[n]==1 & (cam$cam_date1[n] >= cam$db_date[n]-3 & cam$cam_date1[n] <= cam$db_date[n]+3)) {
      jWcm1[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWcm1[n,1] <- cam$cowcode[n]
  sWcm1[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$colonelmajor[n]))) {
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==0 & cam$colonelmajor[n]==1 & (cam$cam_date1[n] >= cam$db_date[n]-3 & cam$cam_date1[n] <= cam$db_date[n]+3)) {
      sWcm1[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  jWbl1[n,1] <- cam$cowcode[n]
  jWbl1[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$below[n]))) {
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==1 & cam$below[n]==1 & (cam$cam_date1[n] >= cam$db_date[n]-3 & cam$cam_date1[n] <= cam$db_date[n]+3)) {
      jWbl1[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWbl1[n,1] <- cam$cowcode[n]
  sWbl1[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$below[n]))) {
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==0 & cam$below[n]==1 & (cam$cam_date1[n] >= cam$db_date[n]-3 & cam$cam_date1[n] <= cam$db_date[n]+3)) {
      sWbl1[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  jWnm1[n,1] <- cam$cowcode[n]
  jWnm1[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$nonmilitary[n]))) {
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==1 & cam$nonmilitary[n]==1 & (cam$cam_date1[n] >= cam$db_date[n]-3 & cam$cam_date1[n] <= cam$db_date[n]+3)) {
      jWnm1[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWnm1[n,1] <- cam$cowcode[n]
  sWnm1[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$nonmilitary[n]))) {
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==0 & cam$nonmilitary[n]==1 & (cam$cam_date1[n] >= cam$db_date[n]-3 & cam$cam_date1[n] <= cam$db_date[n]+3)) {
      sWnm1[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  ncWgen[n,1] <- cam$cowcode[n]
  ncWgen[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$general[n]))) {
    if(!is.na(cam$cam_coup1[n]) & cam$cam_coup1[n]==0 & cam$general[n]==1) {
      ncWgen[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  ncWcm[n,1] <- cam$cowcode[n]
  ncWcm[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$colonelmajor[n]))) {
    if(!is.na(cam$cam_coup1[n]) & cam$cam_coup1[n]==0 & cam$colonelmajor[n]==1) {
      ncWcm[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  ncWbl[n,1] <- cam$cowcode[n]
  ncWbl[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$below[n]))) {
    if(!is.na(cam$cam_coup1[n]) & cam$cam_coup1[n]==0 & cam$below[n]==1) {
      ncWbl[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  ncWnm[n,1] <- cam$cowcode[n]
  ncWnm[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$nonmilitary[n]))) {
    if(!is.na(cam$cam_coup1[n]) & cam$cam_coup1[n]==0 & cam$nonmilitary[n]==1) {
      ncWnm[n,3] <- 1
    }
  }
}
##2nd coup
jWgen2 <- matrix(nrow=10301, ncol=3)
sWgen2 <- matrix(nrow=10301, ncol=3)
jWcm2 <- matrix(nrow=10301, ncol=3)
sWcm2 <- matrix(nrow=10301, ncol=3)
jWbl2 <- matrix(nrow=10301, ncol=3)
sWbl2 <- matrix(nrow=10301, ncol=3)
jWnm2 <- matrix(nrow=10301, ncol=3)
sWnm2 <- matrix(nrow=10301, ncol=3)
for(n in 1:length(cam$cowcode)) {
  print(n)
  jWgen2[n,1] <- cam$cowcode[n]
  jWgen2[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$general[n]))) {
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==1 & cam$general[n]==1 & (cam$cam_date2[n] >= cam$db_date[n]-3 & cam$cam_date2[n] <= cam$db_date[n]+3)) {
      jWgen2[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWgen2[n,1] <- cam$cowcode[n]
  sWgen2[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$general[n]))) {
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==0 & cam$general[n]==1 & (cam$cam_date2[n] >= cam$db_date[n]-3 & cam$cam_date2[n] <= cam$db_date[n]+3)) {
      sWgen2[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  jWcm2[n,1] <- cam$cowcode[n]
  jWcm2[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$colonelmajor[n]))) {
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==1 & cam$colonelmajor[n]==1 & (cam$cam_date2[n] >= cam$db_date[n]-3 & cam$cam_date2[n] <= cam$db_date[n]+3)) {
      jWcm2[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWcm2[n,1] <- cam$cowcode[n]
  sWcm2[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$colonelmajor[n]))) {
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==0 & cam$colonelmajor[n]==1 & (cam$cam_date2[n] >= cam$db_date[n]-3 & cam$cam_date2[n] <= cam$db_date[n]+3)) {
      sWcm2[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  jWbl2[n,1] <- cam$cowcode[n]
  jWbl2[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$below[n]))) {
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==1 & cam$below[n]==1 & (cam$cam_date2[n] >= cam$db_date[n]-3 & cam$cam_date2[n] <= cam$db_date[n]+3)) {
      jWbl2[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWbl2[n,1] <- cam$cowcode[n]
  sWbl2[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$below[n]))) {
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==0 & cam$below[n]==1 & (cam$cam_date2[n] >= cam$db_date[n]-3 & cam$cam_date2[n] <= cam$db_date[n]+3)) {
      sWbl2[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  jWnm2[n,1] <- cam$cowcode[n]
  jWnm2[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$nonmilitary[n]))) {
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==1 & cam$nonmilitary[n]==1 & (cam$cam_date2[n] >= cam$db_date[n]-3 & cam$cam_date2[n] <= cam$db_date[n]+3)) {
      jWnm2[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWnm2[n,1] <- cam$cowcode[n]
  sWnm2[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$nonmilitary[n]))) {
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==0 & cam$nonmilitary[n]==1 & (cam$cam_date2[n] >= cam$db_date[n]-3 & cam$cam_date2[n] <= cam$db_date[n]+3)) {
      sWnm2[n,3] <- 1
    }
  }
}

##3rd coup
jWgen3 <- matrix(nrow=10301, ncol=3)
sWgen3 <- matrix(nrow=10301, ncol=3)
jWcm3 <- matrix(nrow=10301, ncol=3)
sWcm3 <- matrix(nrow=10301, ncol=3)
jWbl3 <- matrix(nrow=10301, ncol=3)
sWbl3 <- matrix(nrow=10301, ncol=3)
jWnm3 <- matrix(nrow=10301, ncol=3)
sWnm3 <- matrix(nrow=10301, ncol=3)

for(n in 1:length(cam$cowcode)) {
  print(n)
  jWgen3[n,1] <- cam$cowcode[n]
  jWgen3[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$general[n]))) {
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==1 & cam$general[n]==1 & (cam$cam_date3[n] >= cam$db_date[n]-3 & cam$cam_date3[n] <= cam$db_date[n]+3)) {
      jWgen3[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWgen3[n,1] <- cam$cowcode[n]
  sWgen3[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$general[n]))) {
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==0 & cam$general[n]==1 & (cam$cam_date3[n] >= cam$db_date[n]-3 & cam$cam_date3[n] <= cam$db_date[n]+3)) {
      sWgen3[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  jWcm3[n,1] <- cam$cowcode[n]
  jWcm3[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$colonelmajor[n]))) {
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==1 & cam$colonelmajor[n]==1 & (cam$cam_date3[n] >= cam$db_date[n]-3 & cam$cam_date3[n] <= cam$db_date[n]+3)) {
      jWcm3[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWcm3[n,1] <- cam$cowcode[n]
  sWcm3[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$colonelmajor[n]))) {
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==0 & cam$colonelmajor[n]==1 & (cam$cam_date3[n] >= cam$db_date[n]-3 & cam$cam_date3[n] <= cam$db_date[n]+3)) {
      sWcm3[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  jWbl3[n,1] <- cam$cowcode[n]
  jWbl3[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$below[n]))) {
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==1 & cam$below[n]==1 & (cam$cam_date3[n] >= cam$db_date[n]-3 & cam$cam_date3[n] <= cam$db_date[n]+3)) {
      jWbl3[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWbl3[n,1] <- cam$cowcode[n]
  sWbl3[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$below[n]))) {
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==0 & cam$below[n]==1 & (cam$cam_date3[n] >= cam$db_date[n]-3 & cam$cam_date3[n] <= cam$db_date[n]+3)) {
      sWbl3[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  jWnm3[n,1] <- cam$cowcode[n]
  jWnm3[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$nonmilitary[n]))) {
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==1 & cam$nonmilitary[n]==1 & (cam$cam_date3[n] >= cam$db_date[n]-3 & cam$cam_date3[n] <= cam$db_date[n]+3)) {
      jWnm3[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWnm3[n,1] <- cam$cowcode[n]
  sWnm3[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$nonmilitary[n]))) {
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==0 & cam$nonmilitary[n]==1 & (cam$cam_date3[n] >= cam$db_date[n]-3 & cam$cam_date3[n] <= cam$db_date[n]+3)) {
      sWnm3[n,3] <- 1
    }
  }
}
##4th coup
jWgen4 <- matrix(nrow=10301, ncol=3)
sWgen4 <- matrix(nrow=10301, ncol=3)
jWcm4 <- matrix(nrow=10301, ncol=3)
sWcm4 <- matrix(nrow=10301, ncol=3)
jWbl4 <- matrix(nrow=10301, ncol=3)
sWbl4 <- matrix(nrow=10301, ncol=3)
jWnm4 <- matrix(nrow=10301, ncol=3)
sWnm4 <- matrix(nrow=10301, ncol=3)
for(n in 1:length(cam$cowcode)) {
  print(n)
  jWgen4[n,1] <- cam$cowcode[n]
  jWgen4[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$general[n]))) {
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==1 & cam$general[n]==1 & (cam$cam_date4[n] >= cam$db_date[n]-3 & cam$cam_date4[n] <= cam$db_date[n]+3)) {
      jWgen4[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWgen4[n,1] <- cam$cowcode[n]
  sWgen4[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$general[n]))) {
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==0 & cam$general[n]==1 & (cam$cam_date4[n] >= cam$db_date[n]-3 & cam$cam_date4[n] <= cam$db_date[n]+3)) {
      sWgen4[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  jWcm4[n,1] <- cam$cowcode[n]
  jWcm4[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$colonelmajor[n]))) {
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==1 & cam$colonelmajor[n]==1 & (cam$cam_date4[n] >= cam$db_date[n]-3 & cam$cam_date4[n] <= cam$db_date[n]+3)) {
      jWcm4[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWcm4[n,1] <- cam$cowcode[n]
  sWcm4[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$colonelmajor[n]))) {
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==0 & cam$colonelmajor[n]==1 & (cam$cam_date4[n] >= cam$db_date[n]-3 & cam$cam_date4[n] <= cam$db_date[n]+3)) {
      sWcm4[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  jWbl4[n,1] <- cam$cowcode[n]
  jWbl4[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$below[n]))) {
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==1 & cam$below[n]==1 & (cam$cam_date4[n] >= cam$db_date[n]-3 & cam$cam_date4[n] <= cam$db_date[n]+3)) {
      jWbl4[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWbl4[n,1] <- cam$cowcode[n]
  sWbl4[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$below[n]))) {
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==0 & cam$below[n]==1 & (cam$cam_date4[n] >= cam$db_date[n]-3 & cam$cam_date4[n] <= cam$db_date[n]+3)) {
      sWbl4[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  jWnm4[n,1] <- cam$cowcode[n]
  jWnm4[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$nonmilitary[n]))) {
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==1 & cam$nonmilitary[n]==1 & (cam$cam_date4[n] >= cam$db_date[n]-3 & cam$cam_date4[n] <= cam$db_date[n]+3)) {
      jWnm4[n,3] <- 1
    }
  }
}
for(n in 1:length(cam$cowcode)) {
  sWnm4[n,1] <- cam$cowcode[n]
  sWnm4[n,2] <- cam$year[n]
  if(!is.na(cam$db_date[n]) & (!is.na(cam$nonmilitary[n]))) {
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==0 & cam$nonmilitary[n]==1 & (cam$cam_date4[n] >= cam$db_date[n]-3 & cam$cam_date4[n] <= cam$db_date[n]+3)) {
      sWnm4[n,3] <- 1
    }
  }
}


jWgen1 <- as.data.frame(jWgen1)
names(jWgen1) <- c("cowcode", "year", "jWgen1")
sWgen1 <- as.data.frame(sWgen1)
names(sWgen1) <- c("cowcode", "year", "sWgen1")
jWcm1 <- as.data.frame(jWcm1)
names(jWcm1) <- c("cowcode", "year", "jWcm1")
sWcm1 <- as.data.frame(sWcm1)
names(sWcm1) <- c("cowcode", "year", "sWcm1")
jWbl1 <- as.data.frame(jWbl1)
names(jWbl1) <- c("cowcode", "year", "jWbl1")
sWbl1 <- as.data.frame(sWbl1)
names(sWbl1) <- c("cowcode", "year", "sWbl1")
jWnm1 <- as.data.frame(jWnm1)
names(jWnm1) <- c("cowcode", "year", "jWnm1")
sWnm1 <- as.data.frame(sWnm1)
names(sWnm1) <- c("cowcode", "year", "sWnm1")

ncWgen <- as.data.frame(ncWgen)
names(ncWgen) <- c("cowcode", "year", "ncWgen")
ncWcm <- as.data.frame(ncWcm)
names(ncWcm) <- c("cowcode", "year", "ncWcm")
ncWbl <- as.data.frame(ncWbl)
names(ncWbl) <- c("cowcode", "year", "ncWbl")
ncWnm <- as.data.frame(ncWnm)
names(ncWnm) <- c("cowcode", "year", "ncWnm")

jWgen2 <- as.data.frame(jWgen2)
names(jWgen2) <- c("cowcode", "year", "jWgen2")
sWgen2 <- as.data.frame(sWgen2)
names(sWgen2) <- c("cowcode", "year", "sWgen2")
jWcm2 <- as.data.frame(jWcm2)
names(jWcm2) <- c("cowcode", "year", "jWcm2")
sWcm2 <- as.data.frame(sWcm2)
names(sWcm2) <- c("cowcode", "year", "sWcm2")
jWbl2 <- as.data.frame(jWbl2)
names(jWbl2) <- c("cowcode", "year", "jWbl2")
sWbl2 <- as.data.frame(sWbl2)
names(sWbl2) <- c("cowcode", "year", "sWbl2")
jWnm2 <- as.data.frame(jWnm2)
names(jWnm2) <- c("cowcode", "year", "jWnm2")
sWnm2 <- as.data.frame(sWnm2)
names(sWnm2) <- c("cowcode", "year", "sWnm2")

jWgen3 <- as.data.frame(jWgen3)
names(jWgen3) <- c("cowcode", "year", "jWgen3")
sWgen3 <- as.data.frame(sWgen3)
names(sWgen3) <- c("cowcode", "year", "sWgen3")
jWcm3 <- as.data.frame(jWcm3)
names(jWcm3) <- c("cowcode", "year", "jWcm3")
sWcm3 <- as.data.frame(sWcm3)
names(sWcm3) <- c("cowcode", "year", "sWcm3")
jWbl3 <- as.data.frame(jWbl3)
names(jWbl3) <- c("cowcode", "year", "jWbl3")
sWbl3 <- as.data.frame(sWbl3)
names(sWbl3) <- c("cowcode", "year", "sWbl3")
jWnm3 <- as.data.frame(jWnm3)
names(jWnm3) <- c("cowcode", "year", "jWnm3")
sWnm3 <- as.data.frame(sWnm3)
names(sWnm3) <- c("cowcode", "year", "sWnm3")

jWgen4 <- as.data.frame(jWgen4)
names(jWgen4) <- c("cowcode", "year", "jWgen4")
sWgen4 <- as.data.frame(sWgen4)
names(sWgen4) <- c("cowcode", "year", "sWgen4")
jWcm4 <- as.data.frame(jWcm4)
names(jWcm4) <- c("cowcode", "year", "jWcm4")
sWcm4 <- as.data.frame(sWcm4)
names(sWcm4) <- c("cowcode", "year", "sWcm4")
jWbl4 <- as.data.frame(jWbl4)
names(jWbl4) <- c("cowcode", "year", "jWbl4")
sWbl4 <- as.data.frame(sWbl4)
names(sWbl4) <- c("cowcode", "year", "sWbl4")
jWnm4 <- as.data.frame(jWnm4)
names(jWnm4) <- c("cowcode", "year", "jWnm4")
sWnm4 <- as.data.frame(sWnm4)
names(sWnm4) <- c("cowcode", "year", "sWnm4")

cam <- merge(cam, jWgen1, by=c("cowcode","year"))
cam <- merge(cam, sWgen1, by=c("cowcode","year"))
cam <- merge(cam, jWcm1, by=c("cowcode","year"))
cam <- merge(cam, sWcm1, by=c("cowcode","year"))
cam <- merge(cam, jWbl1, by=c("cowcode","year"))
cam <- merge(cam, sWbl1, by=c("cowcode","year"))
cam <- merge(cam, jWnm1, by=c("cowcode","year"))
cam <- merge(cam, sWnm1, by=c("cowcode","year"))

cam <- merge(cam, ncWgen, by=c("cowcode","year"))
cam <- merge(cam, ncWcm, by=c("cowcode","year"))
cam <- merge(cam, ncWbl, by=c("cowcode","year"))
cam <- merge(cam, ncWnm, by=c("cowcode","year"))

cam <- merge(cam, jWgen2, by=c("cowcode","year"))
cam <- merge(cam, sWgen2, by=c("cowcode","year"))
cam <- merge(cam, jWcm2, by=c("cowcode","year"))
cam <- merge(cam, sWcm2, by=c("cowcode","year"))
cam <- merge(cam, jWbl2, by=c("cowcode","year"))
cam <- merge(cam, sWbl2, by=c("cowcode","year"))
cam <- merge(cam, jWnm2, by=c("cowcode","year"))
cam <- merge(cam, sWnm2, by=c("cowcode","year"))
cam <- merge(cam, jWgen3, by=c("cowcode","year"))
cam <- merge(cam, sWgen3, by=c("cowcode","year"))
cam <- merge(cam, jWcm3, by=c("cowcode","year"))
cam <- merge(cam, sWcm3, by=c("cowcode","year"))
cam <- merge(cam, jWbl3, by=c("cowcode","year"))
cam <- merge(cam, sWbl3, by=c("cowcode","year"))
cam <- merge(cam, jWnm3, by=c("cowcode","year"))
cam <- merge(cam, sWnm3, by=c("cowcode","year"))
cam <- merge(cam, jWgen4, by=c("cowcode","year"))
cam <- merge(cam, sWgen4, by=c("cowcode","year"))
cam <- merge(cam, jWcm4, by=c("cowcode","year"))
cam <- merge(cam, sWcm4, by=c("cowcode","year"))
cam <- merge(cam, jWbl4, by=c("cowcode","year"))
cam <- merge(cam, sWbl4, by=c("cowcode","year"))
cam <- merge(cam, jWnm4, by=c("cowcode","year"))
cam <- merge(cam, sWnm4, by=c("cowcode","year"))

cam$jWgen1[cam$general==1 & is.na(cam$jWgen1)] <- 0
cam$sWgen1[cam$general==1 & is.na(cam$sWgen1)] <- 0
cam$jWcm1[cam$colonelmajor==1 & is.na(cam$jWcm1)] <- 0
cam$sWcm1[cam$colonelmajor==1 & is.na(cam$sWcm1)] <- 0
cam$jWbl1[cam$below==1 & is.na(cam$jWbl1)] <- 0
cam$sWbl1[cam$below==1 & is.na(cam$sWbl1)] <- 0
cam$jWnm1[cam$nonmilitary==1 & is.na(cam$jWnm1)] <- 0
cam$sWnm1[cam$nonmilitary==1 & is.na(cam$sWnm1)] <- 0

cam$ncWgen[cam$general==1 & is.na(cam$ncWgen)] <- 0
cam$ncWcm[cam$colonelmajor==1 & is.na(cam$ncWcm)] <- 0
cam$ncWbl[cam$below==1 & is.na(cam$ncWbl)] <- 0
cam$ncWnm[cam$nonmilitary==1 & is.na(cam$ncWnm)] <- 0

cam$jWgen2[cam$general==1 & is.na(cam$jWgen2)] <- 0
cam$sWgen2[cam$general==1 & is.na(cam$sWgen2)] <- 0
cam$jWcm2[cam$colonelmajor==1 & is.na(cam$jWcm2)] <- 0
cam$sWcm2[cam$colonelmajor==1 & is.na(cam$sWcm2)] <- 0
cam$jWbl2[cam$below==1 & is.na(cam$jWbl2)] <- 0
cam$sWbl2[cam$below==1 & is.na(cam$sWbl2)] <- 0
cam$jWnm2[cam$nonmilitary==1 & is.na(cam$jWnm2)] <- 0
cam$sWnm2[cam$nonmilitary==1 & is.na(cam$sWnm2)] <- 0

cam$jWgen3[cam$general==1 & is.na(cam$jWgen3)] <- 0
cam$sWgen3[cam$general==1 & is.na(cam$sWgen3)] <- 0
cam$jWcm3[cam$colonelmajor==1 & is.na(cam$jWcm3)] <- 0
cam$sWcm3[cam$colonelmajor==1 & is.na(cam$sWcm3)] <- 0
cam$jWbl3[cam$below==1 & is.na(cam$jWbl3)] <- 0
cam$sWbl3[cam$below==1 & is.na(cam$sWbl3)] <- 0
cam$jWnm3[cam$nonmilitary==1 & is.na(cam$jWnm3)] <- 0
cam$sWnm3[cam$nonmilitary==1 & is.na(cam$sWnm3)] <- 0

cam$jWgen4[cam$general==1 & is.na(cam$jWgen4)] <- 0
cam$sWgen4[cam$general==1 & is.na(cam$sWgen4)] <- 0
cam$jWcm4[cam$colonelmajor==1 & is.na(cam$jWcm4)] <- 0
cam$sWcm4[cam$colonelmajor==1 & is.na(cam$sWcm4)] <- 0
cam$jWbl4[cam$below==1 & is.na(cam$jWbl4)] <- 0
cam$sWbl4[cam$below==1 & is.na(cam$sWbl4)] <- 0
cam$jWnm4[cam$nonmilitary==1 & is.na(cam$jWnm4)] <- 0
cam$sWnm4[cam$nonmilitary==1 & is.na(cam$sWnm4)] <- 0

cam$jWgen <- cam$jWgen1
cam$jWgen <- ifelse(is.na(cam$jWgen),cam$jWgen2,cam$jWgen)
cam$jWgen <- ifelse(is.na(cam$jWgen),cam$jWgen3,cam$jWgen)
cam$jWgen <- ifelse(is.na(cam$jWgen),cam$jWgen4,cam$jWgen)
cam$sWgen <- cam$sWgen1
cam$sWgen <- ifelse(is.na(cam$sWgen),cam$sWgen2,cam$sWgen)
cam$sWgen <- ifelse(is.na(cam$sWgen),cam$sWgen3,cam$sWgen)
cam$sWgen <- ifelse(is.na(cam$sWgen),cam$sWgen4,cam$sWgen)
cam$jWcm <- cam$jWcm1
cam$jWcm <- ifelse(is.na(cam$jWcm),cam$jWcm2,cam$jWcm)
cam$jWcm <- ifelse(is.na(cam$jWcm),cam$jWcm3,cam$jWcm)
cam$jWcm <- ifelse(is.na(cam$jWcm),cam$jWcm4,cam$jWcm)
cam$sWcm <- cam$sWcm1
cam$sWcm <- ifelse(is.na(cam$sWcm),cam$sWcm2,cam$sWcm)
cam$sWcm <- ifelse(is.na(cam$sWcm),cam$sWcm3,cam$sWcm)
cam$sWcm <- ifelse(is.na(cam$sWcm),cam$sWcm4,cam$sWcm)
cam$jWbl <- cam$jWbl1
cam$jWbl <- ifelse(is.na(cam$jWbl),cam$jWbl2,cam$jWbl)
cam$jWbl <- ifelse(is.na(cam$jWbl),cam$jWbl3,cam$jWbl)
cam$jWbl <- ifelse(is.na(cam$jWbl),cam$jWbl4,cam$jWbl)
cam$sWbl <- cam$sWbl1
cam$sWbl <- ifelse(is.na(cam$sWbl),cam$sWbl2,cam$sWbl)
cam$sWbl <- ifelse(is.na(cam$sWbl),cam$sWbl3,cam$sWbl)
cam$sWbl <- ifelse(is.na(cam$sWbl),cam$sWbl4,cam$sWbl)
cam$jWnm <- cam$jWnm1
cam$jWnm <- ifelse(is.na(cam$jWnm),cam$jWnm2,cam$jWnm)
cam$jWnm <- ifelse(is.na(cam$jWnm),cam$jWnm3,cam$jWnm)
cam$jWnm <- ifelse(is.na(cam$jWnm),cam$jWnm4,cam$jWnm)
cam$sWnm <- cam$sWnm1
cam$sWnm <- ifelse(is.na(cam$sWnm),cam$sWnm2,cam$sWnm)
cam$sWnm <- ifelse(is.na(cam$sWnm),cam$sWnm3,cam$sWnm)
cam$sWnm <- ifelse(is.na(cam$sWnm),cam$sWnm4,cam$sWnm)

cam$notdb1 <- NA
cam$notdb2 <- NA
cam$notdb3 <- NA
cam$notdb4 <- NA
for(n in 1:length(cam$cowcode)) {
  if(is.na(cam$db_date[n]) & cam$cam_coup1[n]==1) {
    cam$notdb1[n] <- 1
  }
  if(!is.na(cam$general[n]) & cam$general[n]==1 & cam$cam_coup2[n]==1 & cam$jWgen2[n]==0 & cam$sWgen2[n]==0) {
    cam$notdb2[n] <- 1
  }
  if(!is.na(cam$colonelmajor[n]) & cam$colonelmajor[n]==1 & cam$cam_coup2[n]==1 & cam$jWcm2[n]==0 & cam$sWcm2[n]==0) {
    cam$notdb2[n] <- 1
  }
  if(!is.na(cam$below[n]) & cam$below[n]==1 & cam$cam_coup2[n]==1 & cam$jWbl2[n]==0 & cam$sWbl2[n]==0) {
    cam$notdb2[n] <- 1
  }
  if(!is.na(cam$nonmilitary[n]) & cam$nonmilitary[n]==1 & cam$cam_coup2[n]==1 & cam$jWnm2[n]==0 & cam$sWnm2[n]==0) {
    cam$notdb2[n] <- 1
  }
  if(!is.na(cam$general[n]) & cam$general[n]==1 & cam$cam_coup3[n]==1 & cam$jWgen3[n]==0 & cam$sWgen3[n]==0) {
    cam$notdb3[n] <- 1
  }
  if(!is.na(cam$colonelmajor[n]) & cam$colonelmajor[n]==1 & cam$cam_coup3[n]==1 & cam$jWcm3[n]==0 & cam$sWcm3[n]==0) {
    cam$notdb3[n] <- 1
  }
  if(!is.na(cam$below[n]) & cam$below[n]==1 & cam$cam_coup3[n]==1 & cam$jWbl3[n]==0 & cam$sWbl3[n]==0) {
    cam$notdb3[n] <- 1
  }
  if(!is.na(cam$nonmilitary[n]) & cam$nonmilitary[n]==1 & cam$cam_coup3[n]==1 & cam$jWnm3[n]==0 & cam$sWnm3[n]==0) {
    cam$notdb3[n] <- 1
  }  
  if(!is.na(cam$general[n]) & cam$general[n]==1 & cam$cam_coup4[n]==1 & cam$jWgen4[n]==0 & cam$sWgen4[n]==0) {
    cam$notdb4[n] <- 1
  }
  if(!is.na(cam$colonelmajor[n]) & cam$colonelmajor[n]==1 & cam$cam_coup4[n]==1 & cam$jWcm4[n]==0 & cam$sWcm4[n]==0) {
    cam$notdb4[n] <- 1
  }
  if(!is.na(cam$below[n]) & cam$below[n]==1 & cam$cam_coup4[n]==1 & cam$jWbl4[n]==0 & cam$sWbl4[n]==0) {
    cam$notdb4[n] <- 1
  }
  if(!is.na(cam$nonmilitary[n]) & cam$nonmilitary[n]==1 & cam$cam_coup4[n]==1 & cam$jWnm4[n]==0 & cam$sWnm4[n]==0) {
    cam$notdb4[n] <- 1  
  }
}

db_table <- matrix(nrow=5, ncol = 5)
db_table[1,1] <- "Dataset"
db_table[1,2] <- "Generals"
db_table[1,3] <- "Colonels/Majors"
db_table[1,4] <- "Lower"
db_table[1,5] <- "Non-military"
db_table[2,1] <- "De Bruin"
db_table[2,2] <- n_perc(cam$general==1, na_rm = T)
db_table[2,3] <- n_perc(cam$colonelmajor==1, na_rm = T)
db_table[2,4] <- n_perc(cam$below==1, na_rm = T)
db_table[2,5] <- n_perc(cam$nonmilitary==1, na_rm = T)
db_table[3,1] <- "CAM Elite"
db_table[3,2] <- n_perc(cam$sWgen==1, na_rm = T)
db_table[3,3] <- n_perc(cam$sWcm==1, na_rm = T)
db_table[3,4] <- n_perc(cam$sWbl==1, na_rm = T)
db_table[3,5] <- n_perc(cam$sWnm==1, na_rm = T)
db_table[4,1] <- "CAM Combat"
db_table[4,2] <- n_perc(cam$jWgen==1, na_rm = T)
db_table[4,3] <- n_perc(cam$jWcm==1, na_rm = T)
db_table[4,4] <- n_perc(cam$jWbl==1, na_rm = T)
db_table[4,5] <- n_perc(cam$jWnm==1, na_rm = T)
db_table[5,1] <- "Not in CAM"
db_table[5,2] <- n_perc(cam$ncWgen==1, na_rm = T)
db_table[5,3] <- n_perc(cam$ncWcm==1, na_rm = T)
db_table[5,4] <- n_perc(cam$ncWbl==1, na_rm = T)
db_table[5,5] <- n_perc(cam$ncWnm==1, na_rm = T)

db_table
write.xlsx(db_table, "CAM vs DeBruin Table.xlsx")

###List
### Not covered by De Bruin
CAMvsDB <- matrix(nrow=1000, ncol=4)
m = 0
for(n in 1:length(cam$cowcode)) {
  if(!is.na(cam$notdb1[n]) & cam$notdb1[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date1[n], "%d-%b-%Y")
    if(cam$cam_combat1[n]==1 & cam$cam_successful1[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(cam$cam_combat1[n]==0 & cam$cam_successful1[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(cam$cam_combat1[n]==0 & cam$cam_successful1[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(cam$cam_combat1[n]==1 & cam$cam_successful1[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "not in De Bruin"
  }
  if(!is.na(cam$notdb2[n]) & cam$notdb2[n]==1) {
    m = m + 1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date2[n], "%d-%b-%Y")
    if(cam$cam_combat2[n]==1 & cam$cam_successful2[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(cam$cam_combat2[n]==0 & cam$cam_successful2[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(cam$cam_combat2[n]==0 & cam$cam_successful2[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(cam$cam_combat2[n]==1 & cam$cam_successful2[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "not in De Bruin"
  }
  if(!is.na(cam$notdb3[n]) & cam$notdb3[n]==1) {
    m = m + 1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date3[n], "%d-%b-%Y")
    if(cam$cam_combat3[n]==1 & cam$cam_successful3[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(cam$cam_combat3[n]==0 & cam$cam_successful3[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(cam$cam_combat3[n]==0 & cam$cam_successful3[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(cam$cam_combat3[n]==1 & cam$cam_successful3[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "not in De Bruin"
  }
  if(!is.na(cam$notdb4[n]) & cam$notdb4[n]==1) {
    m = m + 1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date4[n], "%d-%b-%Y")
    if(cam$cam_combat4[n]==1 & cam$cam_successful4[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(cam$cam_combat4[n]==0 & cam$cam_successful4[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(cam$cam_combat4[n]==0 & cam$cam_successful4[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(cam$cam_combat4[n]==1 & cam$cam_successful4[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "not in De Bruin"
  }
  if(!is.na(cam$general[n]) & cam$general[n]==1 & (!is.na(cam$jWgen1[n])) & cam$jWgen1[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date1[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==1 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==0 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==0 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==1 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as General by De Bruin"
  }
  if(!is.na(cam$general[n]) & cam$general[n]==1 & (!is.na(cam$jWgen2[n])) & cam$jWgen2[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date2[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==1 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==0 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==0 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==1 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as General by De Bruin"
  }
  if(!is.na(cam$general[n]) & cam$general[n]==1 & (!is.na(cam$jWgen3[n])) & cam$jWgen3[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date3[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==1 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==0 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==0 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==1 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as General by De Bruin"
  }
  if(!is.na(cam$general[n]) & cam$general[n]==1 & (!is.na(cam$jWgen4[n])) & cam$jWgen4[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date4[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==1 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==0 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==0 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==1 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as General by De Bruin"
  }
  if(!is.na(cam$colonelmajor[n]) & cam$colonelmajor[n]==1 & (!is.na(cam$sWcm1[n])) & cam$sWcm1[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date1[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==1 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==0 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==0 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==1 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as Colonel/Major by De Bruin"
  }
  if(!is.na(cam$colonelmajor[n]) & cam$colonelmajor[n]==1 & (!is.na(cam$sWcm2[n])) & cam$sWcm2[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date2[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==1 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==0 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==0 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==1 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as Colonel/Major by De Bruin"
  }
  if(!is.na(cam$colonelmajor[n]) & cam$colonelmajor[n]==1 & (!is.na(cam$sWcm3[n])) & cam$sWcm3[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date3[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==1 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==0 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==0 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==1 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as Colonel/Major by De Bruin"
  }
  if(!is.na(cam$colonelmajor[n]) & cam$colonelmajor[n]==1 & (!is.na(cam$sWcm4[n])) & cam$sWcm4[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date4[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==1 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==0 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==0 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==1 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as Colonel/Major by De Bruin"
  }
  if(!is.na(cam$below[n]) & cam$below[n]==1 & (!is.na(cam$sWbl1[n])) & cam$sWbl1[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date1[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==1 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==0 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==0 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==1 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as Low Rank by De Bruin"
  }
  if(!is.na(cam$below[n]) & cam$below[n]==1 & (!is.na(cam$sWbl2[n])) & cam$sWbl2[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date2[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==1 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==0 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==0 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==1 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as Low Rank by De Bruin"
  }
  if(!is.na(cam$below[n]) & cam$below[n]==1 & (!is.na(cam$sWbl3[n])) & cam$sWbl3[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date3[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==1 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==0 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==0 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==1 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as Low Rank by De Bruin"
  }
  if(!is.na(cam$below[n]) & cam$below[n]==1 & (!is.na(cam$sWbl4[n])) & cam$sWbl4[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date4[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==1 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==0 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==0 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==1 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as Low Rank by De Bruin"
  }
  if(!is.na(cam$nonmilitary[n]) & cam$nonmilitary[n]==1 & (!is.na(cam$cam_coup1[n])) & cam$cam_coup1[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date1[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==1 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==0 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==0 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat1[n]) & cam$cam_combat1[n]==1 & (!is.na(cam$cam_successful1[n])) & cam$cam_successful1[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as Non-Military by De Bruin"
  }
  if(!is.na(cam$nonmilitary[n]) & cam$nonmilitary[n]==1 & (!is.na(cam$cam_coup2[n])) & cam$cam_coup2[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date2[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==1 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==0 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==0 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat2[n]) & cam$cam_combat2[n]==1 & (!is.na(cam$cam_successful2[n])) & cam$cam_successful2[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as Non-Military by De Bruin"
  }
  if(!is.na(cam$nonmilitary[n]) & cam$nonmilitary[n]==1 & (!is.na(cam$cam_coup3[n])) & cam$cam_coup3[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date3[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==1 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==0 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==0 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat3[n]) & cam$cam_combat3[n]==1 & (!is.na(cam$cam_successful3[n])) & cam$cam_successful3[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as Non-Military by De Bruin"
  }
  if(!is.na(cam$nonmilitary[n]) & cam$nonmilitary[n]==1 & (!is.na(cam$cam_coup4[n])) & cam$cam_coup4[n]==1) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$cam_date4[n], "%d-%b-%Y")
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==1 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==1) {
      CAMvsDB[m,3] <- "combat (s)"
    }
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==0 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==1) {
      CAMvsDB[m,3] <- "elite (s)"
    }
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==0 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==0) {
      CAMvsDB[m,3] <- "elite (f)"
    }
    if(!is.na(cam$cam_combat4[n]) & cam$cam_combat4[n]==1 & (!is.na(cam$cam_successful4[n])) & cam$cam_successful4[n]==0) {
      CAMvsDB[m,3] <- "combat (f)"
    }
    CAMvsDB[m,4] <- "coded as Non-Military by De Bruin"
  }
  if(!is.na(cam$db_date[n]) & (!is.na(cam$cam_coup1[n])) & cam$cam_coup1[n]==0) {
    m = m +1
    CAMvsDB[m,1] <- cam$cam_country[n]
    CAMvsDB[m,2] <- format(cam$db_date[n], "%d-%b-%Y")
    if(!is.na(cam$general[n]) & cam$general[n]==1) {
      CAMvsDB[m,3] <- "General (DB)"
    }
    if(!is.na(cam$colonelmajor[n]) & cam$colonelmajor[n]==1) {
      CAMvsDB[m,3] <- "Colonel/Major (DB)"
    }
    if(!is.na(cam$below[n]) & cam$below[n]==1) {
      CAMvsDB[m,3] <- "Low Rank (DB)"
    }
    if(!is.na(cam$nonmilitary[n]) & cam$nonmilitary[n]==1) {
      CAMvsDB[m,3] <- "Non-Military"
    }
    CAMvsDB[m,4] <- "not in CAM"
  }
}
CAMvsDB <- as.data.frame(CAMvsDB)
CAMvsDB <- CAMvsDB[!is.na(CAMvsDB$V1),]

write.xlsx(CAMvsDB, "CAM vs DB.xlsx")

