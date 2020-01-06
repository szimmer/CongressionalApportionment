library(tidyverse)

rawdat <- read_csv("RawData/nst-est2019-alldata.csv")
# Oh this population is just resident population so differs
# https://www.census.gov/content/dam/Census/library/publications/2011/dec/c2010br-08.pdf

cendat <- read_rds("DataOut/Census2010Apportionment.rds")
glimpse(rawdat)

PopNums <- rawdat %>%
  select(SUMLEV, STATE, Pop2010=CENSUS2010POP, PopRecent=POPESTIMATE2019) %>%
  filter(SUMLEV=="040", STATE != "72", STATE !="11") # remove PR and DC

PopNums


stateCW <- maps::state.fips %>%
  as_tibble() %>%
  select(fips, abb, polyname) %>%
  distinct(fips, abb, .keep_all=TRUE) %>%
  add_row(fips=c(15, 2), abb=c("HI", "AK"), polyname=c("hawaii", "alaska")) %>%
  separate(polyname, into=c("StateName", "Extra"), fill="right", sep="\\:") %>%
  mutate(State=str_pad(fips, 2, side="left", pad="0"),
         StateName=str_trim(str_squish(str_to_title(StateName)))) %>%
  select(-fips, -Extra)


cendatID <- cendat %>% 
  mutate(State=str_trim(State)) %>%
  rename(StateName=State) %>%
  left_join(stateCW, by="StateName") %>%
  arrange(abb)

PopNumsID <- PopNums %>%
  left_join(stateCW, by=c("STATE"="State")) %>%
  arrange(abb)

library(ggrepel)
PopNumsID %>%
  ggplot(aes(x=Pop2010/1000000, y=PopRecent/1000000, label=abb)) +
  geom_point(colour="red") +
  geom_abline(slope=1, intercept=0) +
  coord_trans(x="log10", y="log10") +
  xlab("2010 Pop (millions)") +
  ylab("2019 Pop (millions)") +
  ggtitle("Comparing 2010 and 2019 Population") +
  geom_text_repel(size=2.5)

# Check that resident pops match
all(cendatID$ResPop==PopNumsID$Pop2010)
# This means population from PDF and csv are the same for resident pop




AllocFun <- function(Pop, States, TotSeats=435){
  # Trying to match this: https://www.census.gov/population/apportionment/files/Priority%20Values%202010.pdf
  
  PriorityCalc <- tibble(Pop=rep(Pop, TotSeats-1),
                     State=rep(States, TotSeats-1),
                     StateSeat=rep(2:TotSeats, each=length(States))) %>%
    mutate(Priority=Pop/sqrt(StateSeat*(StateSeat-1))) %>%
    arrange(desc(Priority)) %>%
    mutate(Seat=row_number()+length(States)) %>%
    filter(Seat <= TotSeats)
  
  FirstSeats <-  tibble(State=States,
                        StateSeat=1)
  
  
  PriorityValues <- bind_rows(FirstSeats, PriorityCalc) %>%
    select(-Pop) 
  
  
  
  return(PriorityValues)
}






Alloc2010Res <- AllocFun(PopNumsID$Pop2010, PopNumsID$abb, 435)
Alloc2010Tot <- AllocFun(cendatID$TotalPop, cendatID$abb, 435)
Alloc2019 <- AllocFun(PopNumsID$PopRecent, PopNumsID$abb, 435)

# How does the representation change from using just residential
# to also including federal overseas people?

RepByStateRes <- Alloc2010Res %>% count(State)
RepByStateTot <- Alloc2010Tot %>% count(State)

RepCompare <- RepByStateRes %>%
  full_join(RepByStateTot, by="State", suffix=c(".Res", ".Tot")) %>%
  mutate(RepDiff=abs(n.Res-n.Tot)) %>%
  arrange(desc(RepDiff))

RepCompare

# This shows that the allocation is the same with and without foreign federal employees


# Compare prediction
RepByStatePredict <- Alloc2019 %>% count(State)  

RepCompareTime <- RepByStateRes %>%
  full_join(RepByStatePredict, by="State", suffix=c(".2010", ".2019")) %>%
  mutate(Change=n.2019-n.2010)

RepCompareTime %>%
  filter(Change != 0) %>%
  arrange(Change)

# TX predicted to gain 2 reps
# AZ, CO, FL, MT, NC, and OR predicted to gain 1 rep
# CA, IL, MI, MN, NY, PA, RI, and WV predicted to lose 1 rep


