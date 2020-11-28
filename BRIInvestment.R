rm(list = ls())
library(dplyr)
library(stringr)
library(foreign)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(formattable)

a<-read.csv("chitfull.csv", skip=5, stringsAsFactors=F)
a$Quantity.in.Millions <- str_replace_all(a$Quantity.in.Millions, ",", "")
a$Quantity.in.Millions <- as.numeric(str_replace_all(a$Quantity.in.Millions, "\\$", ""))
summary(a)

# Total Investment
TotalInvestment <- sum(a$Quantity.in.Millions)
TotalInvestment

# BRI Investment
BRI <- a %>% filter(BRI==1)
TotalBRI <- sum(BRI$Quantity.in.Millions)

# Share of BRI
TotalBRI/TotalInvestment*100

# Number of projects by country
n_country <- a %>% group_by(Country) %>% 
  select(BRI) %>% 
  arrange(desc(BRI)) %>% 
  table() 
n_country

n_sector <- a %>% group_by(Sector) %>% 
  select(BRI) %>% 
  arrange(desc(BRI)) %>% 
  table() 
n_sector

# Energy Investment
Energy <- a %>% filter(Sector=="Energy"|Sector=="Utilities")
AllEnergy <- sum(Energy$Quantity.in.Millions)
AllEnergy

# Share of energy investment
AllEnergy/TotalInvestment*100

#  Shares by region, energy 
by_region <- a %>% group_by(Region) %>% 
  summarise(region_subtotal=sum(Quantity.in.Millions),
          e_by_region=sum(Quantity.in.Millions[Sector=="Energy"|Sector=="Utilities"])) %>% 
  mutate(region_share=round(region_subtotal/sum(region_subtotal)*100, 1),
         region_e_share=round(e_by_region/sum(e_by_region)*100,1 )) %>% 
  arrange(desc(region_share)) 

write_csv(by_region, "by_region.csv")

# Which countries belong to each region? 
countrynames <- a %>% 
  group_by(Region) %>% 
  summarise(
    countryname=paste(sort(unique(Country)), collapse=","))
View(countrynames)

# Top 10 invested countries 
top10_country <- a %>% group_by(Country) %>% 
  summarise(country_subtotal=sum(Quantity.in.Millions)) %>% 
  mutate(country_share=country_subtotal/sum(country_subtotal)*100) %>% 
  arrange(desc(country_subtotal)) %>% 
  slice()
top10_country

# by Sector
all_by_sector <- a %>% group_by(Sector) %>% 
  summarise(sector_subtotal=sum(Quantity.in.Millions)) %>% 
  mutate(sector_share=round(sector_subtotal/sum(sector_subtotal)*100,2)) %>% 
  arrange(desc(sector_subtotal))
all_by_sector
write.csv(all_by_sector, "by_sector.csv")

# Investment Timeline 
# 1. All

timedata <- a %>% group_by(Year) %>% 
  mutate(energy = ifelse(a$Sector%in% c("Energy", "Utilities"), a$Quantity.in.Millions, "0"))

year <- a %>% group_by(Year) %>% 
  summarize(Investment=sum(Quantity.in.Millions))

png("timeline.png", width=900, height=500)
ggplot(year, aes(x=Year, y=Investment)) +
  geom_line(color="red")
dev.off()

# 2. Energy 
year_energy <- a %>% filter(Sector=="Energy"|Sector=="Utilities") %>% 
  group_by(Year) %>% 
  summarize(Investment=sum(Quantity.in.Millions))

ggplot(year_energy, aes(x=Year, y=Investment)) +
  geom_line(color="red")

# 3. Tech
year_tech <- a %>% filter(Subsector %in% c("Autos", "Telecom", "Aviation"), 
                          Sector %in% c("Technology", "Entertainment")) %>% 
  group_by(Year) %>% 
  summarize(Investment=sum(Quantity.in.Millions))

ggplot(year_tech, aes(x=Year, y=Investment)) +
  geom_line(color=c("red"))

# Closer look at Energy Projects
# Since the dataset from China Gloabl Investment Tracker does not break down energy projects into technology, 
# let us use D.A.T.A. data, which has more information on energy projects. 

b <- read.csv("BU-PITCH-EN-2020.csv", stringsAsFactors=FALSE)
b <- b %>% rename(Amount=Amount...m.)
b <- b %>% select(Date, Country, Region, EnergySource, EnergySubSector, Borrower, Lender, Amount, Description, BRI.Membership)

# Chinese Investment by Energy Sub-sector

png("Chinese energy investment by year and energy type.png", width = 1000, height = 600)
ggplot(data = buenergy, aes(x=Date, y=Amount, fill=EnergySource, color=EnergySource)) +
  geom_col() + ggtitle("Chinese Investment by Energy Sub-sector") 
dev.off()

# Which technology is the most invested in?
by_tech <- b %>% group_by(EnergySource) %>% 
  summarise(tech_subtotal=round(sum(Amount))) %>% 
  mutate(tech_share=round(tech_subtotal/sum(tech_subtotal)*100,2)) %>% 
  arrange(desc(tech_share))
formattable(by_tech)
write.csv(by_tech, "by_tech.csv")

# Oil and coal takes up 55% of the energy-related investment.
# Where are the fossil fuel projects happening? 

Fossil <- b %>% filter(EnergySource=="Oil"|EnergySource=="Coal") %>% 
  arrange(desc(Amount)) 
Fossil <- b %>% filter(EnergySource=="Oil"|EnergySource=="Coal") %>% 
  arrange(desc(Date))

after2015 <- Fossil %>% filter(Date>2015) 
sum(after2015$Amount)

View(Fossil)
formattable(arrange(Fossil, desc(Date)))

# Fossil by year and country
Fossil %>% group_by(Date, Country) %>% 
  summarise(yearly=sum(Amount))
ggplot(Fossil, aes(x=Date, y=Amount, fill=Country, color=Country)) + geom_col()

# Where are the renewable projects?
RE <- b %>% filter(EnergySource%in% c("Wind","Solar","Biomass", "Geothermal"))
re <- RE %>% group_by(Country, EnergySource) %>% 
  summarise(Total=sum(Amount)) %>% arrange(desc(Total))
png("reinvestment.png", width=900, height=500)
ggplot(re, aes(x=Country, y=Total, fill=EnergySource)) + geom_col() + ylab("Total Investment in RE")
dev.off()
write.csv(re, "re.csv")

View(RE)
unique(RE$Country)

Pakistan <- b %>% filter(Country=="Pakistan", EnergySource=="Solar")
Pakistan

# # Graph1: Regional breakdown 
# region <- a %>% group_by(Region) %>% 
#   summarise(region_subtotal=sum(Quantity.in.Millions))
# 
# png("region.png",width=900,height=670,res=90)
# pieregion <- ggplot(region,aes(x="", y=region_subtotal, fill=Region)) + 
#   geom_bar(width=1, stat="identity") + coord_polar("y") +
#   geom_text(aes(label = paste0(round(region_subtotal/sum(region_subtotal)*100), "%")), position = position_stack(vjust = 0.5)) + 
#     scale_fill_hue() + 
#   labs(x = NULL, y = NULL, fill = NULL, title = "All Global Chinese Investment by Region") + 
#   theme_classic() + theme(axis.line = element_blank(),
#                                     axis.text = element_blank(),
#                                     axis.ticks = element_blank())
# 
# pieregion
# dev.off()
# 
# # Graph: Energy Investment by region
# png("pieregion_e.png",width=900,height=670, res=80)
# pieregion_e <- ggplot(by_region,aes(x="", y=region_e_share, fill=Region)) + 
#   geom_bar(width=1, stat="identity") + coord_polar("y") +
#   geom_text(aes(label = paste0(round(region_e_share), "%")), position = position_stack(vjust = 0.5)) + 
#   scale_fill_hue() + 
#   labs(x = NULL, y = NULL, fill = NULL, title = "Energy Investment by Region") + 
#   theme_classic() + theme(axis.line = element_blank(),
#                           axis.text = element_blank(),
#                           axis.ticks = element_blank())
# pieregion_e
# dev.off()

# # # Sector share pie chart
# # png("sector.png",width=900,height=670,res=90) 
# # piesector <- ggplot(all_by_sector,aes(x="", y=sector_subtotal, fill=Sector)) + 
# #   geom_bar(width=1, stat="identity") + coord_polar("y") +
# #   geom_text(aes(label = paste0(round(sector_subtotal/sum(sector_subtotal)*100), "%")), position = position_stack(vjust = 0.5)) + 
# #   scale_fill_hue(l=70, c=70) + 
# #   labs(x = NULL, y = NULL, fill = NULL, title = "All Global Chinese Investment by Sector") + 
# #   theme_classic() + theme(axis.line = element_blank(),
# #                           axis.text = element_blank(),
# #                           axis.ticks = element_blank())
# 
# piesector
# dev.off()