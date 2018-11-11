###In part C module, I have chosen the biodiversity of mammalian taxonomic group. It has different orders and I'm interested in records of country, the number of orders in the data set, to reduce the selected data set and plot graph of reduced data.
library(tidyverse)
#Removed install.packages for tidyverse and dplyr as it is not needed.
Assignmentdata <- read.delim("bold_data.txt")
#to check records per country
countries.count <-Assignmentdata %>%
  group_by(country) %>%
  summarize(count_records2 = length(processid)) %>%
  arrange(desc(count_records2)) %>%
  print()
#plot a histogram of the order_taxID
hist(Assignmentdata$order_taxID)
#to reduce the families
top.families <- Assignmentdata %>% #Originally written as Assignment1data. Removed the 1 as it was inconsistent with previous objects names.
  group_by(family_name) %>%
  mutate(count_by_family = length(processid)) %>%
  filter(count_by_family > 120 ) 
#to check remaining family records
length(top.families$family_name)
#to check unique family name
unique(top.families$family_name)
#plot graph of family and count of family
plot(as.factor(top.families$family_name), top.families$count_by_family)
#using vegan package
library(vegan)
comm <- Assignmentdata %>%
  group_by(bin_uri) %>%
  count(bin_uri)
commAssign1 <- specaccum(comm = n, method = "exact", permutations = 100,conditioned = TRUE)
library(dplyr)
commAssign1 <- spread(comm,bin_uri, n)
#using iNext package
library(iNEXT) #Removed install.packages(iNEXT)
g <-ggplot2::aes(top.families$family_name,top.families$count_by_family)
g1<-ggplot2::aes(Assignmentdata$species_name,Assignmentdata$species_taxID)
g1

#####the data set is interpreted by checking the records of the country and  plotted the histogram to  analyze the qunatity of different orders. so with the function histogram mostly two orders are in high frequency.The data is reduced and filtered which has more 120 families. Vegan package and inext package are used to plot a graph.
