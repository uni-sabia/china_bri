library(foreign)
library(tidyverse)

# Consider the dataset from Boston University's D.A.T.A on Chinese Global Investment in Energy Sector.
# Source: http://www.bu.edu/gdp/research/databases/

a<- read.csv("HelloWorld/ChinaData/BU-PITCH-EN-2020.csv", stringsAsFactors=FALSE)
str(a)
a <- a %>% rename(Amount=Amount...m.)
a <- a %>% select(Country, Region, EnergySource, EnergySubSector, Lender, Amount...m., BRI.Membership)

# Consider only BRI projects 
ab <- a[which(a$BRI.Membership==1),]
sum(ab$Amount)

# Total Amount of global BRI investment in energy sector is $183,421,000,000. 
# NOTE: The number provided by the Global China Investment Tracker is $109,700,000,000. 
# The discrepancy comes from the fact that the Global China Investment Tracker excludes "troubled transactions" and "contracts". 
# Compare the datasets here. https://www.aei.org/china-global-investment-tracker/

# Regional breakdown of BRI  projects
Region_percent <- ab %>% group_by(Region) %>% 
  summarise(subtotal_region = sum(Amount)) %>% 
  mutate(regional_share = subtotal_region/sum(subtotal_region)*100) %>% 
  arrange(desc(regional_share))

Region_percent

# Breakdown of BRI projects by Technology
Tech_percent <- ab %>% group_by(EnergySource) %>% 
  summarise(subtotal_tech = sum(Amount)) %>% 
  mutate(tech_share = subtotal_tech/sum(subtotal_tech)*100) %>% 
  arrange(desc(tech_share))

Tech_percent

# Breakdown of BRI projects by Technology 

sectorpie <- ab %>% group_by(EnergySubSector) %>% 
  summarise(subtotal_sector = sum(Amount)) %>% 
  mutate(sector_share = subtotal_sector/sum(subtotal_sector)*100) %>% 
  arrange(desc(sector_share))

#Pie chart by energy subsector in all energy investments
png("sectorpiechart.png",width=1000,height=670,res=90)
sectorpiechart <- ggplot(sectorpie,aes(x="", y=subtotal_sector, fill=EnergySubSector)) + 
  geom_bar(width=1, stat="identity") + coord_polar("y") +
  geom_text(aes(label = paste0(round(subtotal_sector/sum(subtotal_sector)*100), "%")), position = position_stack(vjust = 0.5)) + 
  scale_fill_brewer() + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Investment by Energy Subsector") + 
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))
sectorpiechart
dev.off()

# Pie chart by energy technology 
png("techpiechart.png",width=1000,height=670,res=90)
Tech_pie <- a %>% group_by(EnergySource) %>% 
  summarise(subtotal_tech = sum(Amount))
techpiechart <- ggplot(Tech_pie,aes(x="", y=subtotal_tech, fill=EnergySource)) + 
  geom_bar(width=1, stat="identity") + coord_polar("y") +
  geom_text(aes(label = paste0(round(subtotal_tech/sum(subtotal_tech)*100), "%")), position = position_stack(vjust = 0.5)) + 
  scale_fill_hue() + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Investment by Energy Source") + 
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))
techpiechart
dev.off()

# Solar and wind

solarwind <- a %>% filter(EnergySource=="Wind" | EnergySource=="Solar") %>% 
  select(Country, Region, EnergySource, Borrower, Lender, Amount, Description, BRI.Membership) %>% 
  group_by(Country) %>% 
  summarise(sum=sum(Amount)) %>% 
  arrange(desc(sum))
solarwind

