##################################################################################################################################;
## Our People - Code to produce wellbeing measures for a multidimensional wellbeing analysis based on the Living Standards Framework
##
## File: 5 Final Our people domain and subdomain analysis for Analytical paper.R
## Description: Reads in the aggregated data from SAS programmes. Creates  visualisations for the Our people analytical paper.
##
## Code created: March 2018
## Code last edited: November 2018
##
## Author: Keith McLeod, A&I, The Treasury. 
##
## Notes: Takes tables and sampling errors from SAS programs and uses R to create graphs, visualisations and summary tables.
##
##################################################################################################################################;

setwd("\\\\hamlet/shares/xdrive/ST/4 Cross-Agency and Whole of Government Processes/7 Performance Hub/10 Analytics and Insights/Wellbeing analysis - GSS")

# Visualisations folder
dir.visualisations <- "Visualisations"

library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(plyr)
library(tidyr)
library(reshape2)
library(stringr)
library(ggfortify)
library(gplots)
library(RColorBrewer)
library(anchors)
library(tibble)
library(openxlsx)
library(Hmisc)

## Set the core wellbeing colour palette
teal=rgb(0,131,172,max=255)
lt_teal=rgb(128,192,213,max=255)
dk_teal=rgb(45,98,115,max=255)
green=rgb(103,168,84,max=255)
lt_green=rgb(179,211,170,max=255)
dk_green=rgb(99,126,88,max=255)
orange=rgb(230,94,30,max=255)
lt_orange=rgb(239,150,108,max=255)
dk_orange=rgb(156,62,17,max=255)
lt_yellow=rgb(241,164,45,max=255)
yellow=rgb(201,127,13,max=255)
dk_yellow=rgb(134,85,9,max=255)
charcoal=rgb(63,64,58,max=255)
lt_charcoal=rgb(164,158,157,max=255)
md_charcoal=rgb(128,128,126,max=255)
black=rgb(0,0,0,max=255)

# Old colours
dark_teal=rgb(0,96,128,max=255)
blue_4=rgb(0,188,226,max=255)
green_3=rgb(188,214,81,max=255)
treasury_blue=rgb(0,32,91,max=255)

# Shading
shade_vgood=rgb(128,192,213,max=255)
shade_poor=rgb(247,200,129,max=255)

## Wellbeing colours for: subj well / civic / culture / health / housing / inc&cons / know / safety / social
wellbeing_pal <- c(dk_yellow,lt_teal,dk_teal,lt_orange,dk_orange,orange,teal,lt_yellow,green)

##############################################################################
# Read in data - Firstly domain data
##############################################################################
wellbeing.df.raw1 <- read.csv("Final GSS tables 2014 2016 with std errors 1 of 5 Checked.csv",colClasses=c(rep("character",6),rep("numeric",18)))
wellbeing.df.raw2 <- read.csv("Final GSS tables 2014 2016 with std errors 2 of 5 Checked.csv",colClasses=c(rep("character",6),rep("numeric",18)))
wellbeing.df.raw3 <- read.csv("Final GSS tables 2014 2016 with std errors 3 of 5 Checked.csv",colClasses=c(rep("character",6),rep("numeric",18)))
wellbeing.df.raw4 <- read.csv("Final GSS tables 2014 2016 with std errors 4 of 5 Checked.csv",colClasses=c(rep("character",6),rep("numeric",18)))
wellbeing.df.raw5 <- read.csv("Final GSS tables 2014 2016 with std errors 5 of 5 Checked.csv",colClasses=c(rep("character",6),rep("numeric",18)))
wellbeing.df.raw <- rbind(wellbeing.df.raw1,wellbeing.df.raw2,wellbeing.df.raw3,wellbeing.df.raw4,wellbeing.df.raw5)

wellbeing.pops <- as.data.frame(read_excel("Final GSS tables 2014 2016 Metadata.xlsx", sheet = "Populations"))
wellbeing.doms <- as.data.frame(read_excel("Final GSS tables 2014 2016 Metadata.xlsx", sheet = "Domains"))
wellbeing.subpops <- as.data.frame(read_excel("Final GSS tables 2014 2016 Metadata.xlsx", sheet = "Pop order")) %>% 
  mutate(Subpop = replace(Subpop,Subpop=="Maori ethnicity",paste0("M",sprintf('\u0101'),"ori ethnicity"))) 

# Re-classify values as needed
wellbeing.df <- wellbeing.df.raw %>% 
  subset(year==9999) %>%  # Only keep combined 2014 and 2016 data (code is 9999). For my analysis I don't need the individual years 
  subset(Level1 != "No") %>%  # Exclude the no categories for each ethnic group
  filter(!grepl('Missing', Level1)) %>%  # Exclude rows matching missing population groups
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="1","Quintile 1")) %>%
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="2","Quintile 2")) %>%
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="3","Quintile 3")) %>%
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="4","Quintile 4")) %>%
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="5","Quintile 5")) %>%
  mutate(Level1 = replace(Level1,Var1=="hours_age"&Level1=="1","Under 65 not working")) %>%
  mutate(Level1 = replace(Level1,Var1=="hours_age"&Level1=="2","Under 65 working part-time")) %>%
  mutate(Level1 = replace(Level1,Var1=="hours_age"&Level1=="3","Under 65 working full-time")) %>%
  mutate(Level1 = replace(Level1,Var1=="hours_age"&Level1=="4","Under 65 working long hours")) %>%
  mutate(Level1 = replace(Level1,Var1=="hours_age"&Level1=="5","Over 65 not working")) %>%
  mutate(Level1 = replace(Level1,Var1=="hours_age"&Level1=="6","Over 65 working")) %>%
  mutate(Level1 = replace(Level1,Var1=="asian","Asian ethnicity")) %>% 
  mutate(Level1 = replace(Level1,Var1=="maori",paste0("M",sprintf('\u0101'),"ori ethnicity"))) %>% 
  mutate(Level1 = replace(Level1,Var1=="european","European ethnicity")) %>% 
  mutate(Level1 = replace(Level1,Var1=="pacific","Pacific ethnicity")) %>% 
  mutate(Level1 = replace(Level1,Level1=="65+ years","65 years and over")) %>% 
  mutate(Var2 = replace(Var2,Var2=="","ALL")) %>% 
  mutate(Level2 = replace(Level2,Level2=="","ALL")) %>% 
  mutate(Var3 = replace(Var3,Var3=="","ALL")) %>% 
  mutate(Level3 = replace(Level3,Level3=="","ALL")) %>% 
  mutate(Var1 = replace(Var1,Var1 %in% c("asian","maori","european","pacific"),"ethnicity")) %>% 
  mutate(Level2 = replace(Level2,Level2=="Very good","High")) %>%
  mutate(Level2 = replace(Level2,Level2=="Good","Medium")) %>% 
  mutate(Level2 = replace(Level2,Level2=="Poor","Low")) %>% 
  mutate(Level3 = replace(Level3,Level3=="Very good","High")) %>%
  mutate(Level3 = replace(Level3,Level3=="Good","Medium")) %>% 
  mutate(Level3 = replace(Level3,Level3=="Poor","Low")) %>% 
  mutate(Level2 = replace(Level2,Level2=="1 Very poor","Very low")) %>%
  mutate(Level2 = replace(Level2,Level2=="2 Poor","Low")) %>%
  mutate(Level2 = replace(Level2,Level2=="3 Good","Medium")) %>%
  mutate(Level2 = replace(Level2,Level2=="4 Very good","High")) %>% 
  mutate(Level2 = replace(Level2,Level2=="5 Excellent","Very high")) %>% 
  mutate(Level2 = replace(Level2,Level2=="1 Very poor to poor","Very low to low")) %>%
  mutate(Level2 = replace(Level2,Level2=="2 Good","Medium")) %>%
  mutate(Level2 = replace(Level2,Level2=="3 Very good to exce","High to very high")) %>% 
  mutate(Level3 = replace(Level3,Level3=="1 Very poor","Very low")) %>%
  mutate(Level3 = replace(Level3,Level3=="2 Poor","Low")) %>%
  mutate(Level3 = replace(Level3,Level3=="3 Good","Medium")) %>%
  mutate(Level3 = replace(Level3,Level3=="4 Very good","High")) %>% 
  mutate(Level3 = replace(Level3,Level3=="5 Excellent","Very high")) %>% 
  mutate(Level3 = replace(Level3,Level3=="1 Very poor to poor","Very low to low")) %>%
  mutate(Level3 = replace(Level3,Level3=="2 Good","Medium")) %>%
  mutate(Level3 = replace(Level3,Level3=="3 Very good to exce","High to very high")) %>% 
  filter((Var2 != "full_purpose" & Var3 != "full_purpose")|is.na(Var2)|is.na(Var3)) %>%  ## Drop sense of purpose so we have a single subjective wellbeing measure
  mutate(restsum = as.numeric(restsum)) %>% 
  mutate(restpop = as.numeric(restpop)) %>% 
  mutate(pct_rest = as.numeric(pct_rest)) %>% 
  mutate(ppt_diff = as.numeric(ppt_diff)) %>% 
  mutate(se_ppt_diff = as.numeric(se_ppt_diff)) %>% 
  dplyr::rename(PopGrp=Var1,Population=Level1,Var1=Var2,Level1=Level2,Var2=Var3,Level2=Level3) 

## Create variables as factors with appropriate ordering
wellbeing.df$PopGrp <- factor(wellbeing.df$PopGrp,levels=wellbeing.pops$var,labels=wellbeing.pops$description)
wellbeing.df$Var1 <- factor(wellbeing.df$Var1,levels=wellbeing.doms$var,labels=wellbeing.doms$description)
wellbeing.df$Var2 <- factor(wellbeing.df$Var2,levels=wellbeing.doms$var,labels=wellbeing.doms$description)

domain.vars <- wellbeing.doms$var[c(2,4:11)]
domain.names <- wellbeing.doms$description[c(2,4:11)]
pop.vars <- wellbeing.pops$var
pop.names <- wellbeing.pops$description

wellbeing.df$Population <- factor(wellbeing.df$Population,levels=wellbeing.subpops$Subpop)
populations <- unique(wellbeing.df$Population[!is.na(wellbeing.df$Population)])

wellbeing.means.df <- read.csv("Final GSS tables 2014 2016 MEANS with std errors_Checked.csv",colClasses=c(rep("character",5),rep("numeric",2))) %>% 
  subset(Level1 != "No") %>%  # Exclude the no categories for each ethnic group
  filter(!grepl('Missing', Level1)) %>%  # Exclude rows matching missing population groups
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="1","Quintile 1")) %>%
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="2","Quintile 2")) %>%
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="3","Quintile 3")) %>%
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="4","Quintile 4")) %>%
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="5","Quintile 5")) %>%
  mutate(Level1 = replace(Level1,Var1=="hours_age"&Level1=="1","Under 65 not working")) %>%
  mutate(Level1 = replace(Level1,Var1=="hours_age"&Level1=="2","Under 65 working part-time")) %>%
  mutate(Level1 = replace(Level1,Var1=="hours_age"&Level1=="3","Under 65 working full-time")) %>%
  mutate(Level1 = replace(Level1,Var1=="hours_age"&Level1=="4","Under 65 working long hours")) %>%
  mutate(Level1 = replace(Level1,Var1=="hours_age"&Level1=="5","Over 65 not working")) %>%
  mutate(Level1 = replace(Level1,Var1=="hours_age"&Level1=="6","Over 65 working")) %>%
  mutate(Level1 = replace(Level1,Var1=="asian","Asian ethnicity")) %>% 
  mutate(Level1 = replace(Level1,Var1=="maori",paste0("M",sprintf('\u0101'),"ori ethnicity"))) %>% 
  mutate(Level1 = replace(Level1,Var1=="european","European ethnicity")) %>% 
  mutate(Level1 = replace(Level1,Var1=="pacific","Pacific ethnicity")) %>% 
  mutate(Level1 = replace(Level1,Level1=="65+ years","65 years and over")) %>% 
  mutate(Var2 = replace(Var2,Var2=="","ALL")) %>% 
  mutate(Level2 = replace(Level2,Level2=="","ALL")) %>% 
  mutate(Var1 = replace(Var1,Var1 %in% c("asian","maori","european","pacific"),"ethnicity")) %>% 
  mutate(Level2 = replace(Level2,Level2=="Very good","High")) %>%
  mutate(Level2 = replace(Level2,Level2=="Good","Medium")) %>% 
  mutate(Level2 = replace(Level2,Level2=="Poor","Low")) %>% 
  mutate(Level2 = replace(Level2,Level2=="1 Very poor","Very low")) %>%
  mutate(Level2 = replace(Level2,Level2=="2 Poor","Low")) %>%
  mutate(Level2 = replace(Level2,Level2=="3 Good","Medium")) %>%
  mutate(Level2 = replace(Level2,Level2=="4 Very good","High")) %>% 
  mutate(Level2 = replace(Level2,Level2=="5 Excellent","Very high")) %>% 
  mutate(Level2 = replace(Level2,Level2=="1 Very poor to poor","Very low to low")) %>%
  mutate(Level2 = replace(Level2,Level2=="2 Good","Medium")) %>%
  mutate(Level2 = replace(Level2,Level2=="3 Very good to exce","High to very high")) %>% 
  mutate(outvar = replace(outvar,outvar=="Purpose","Sense of purpose")) %>% 
  filter(outvar != "Sense of purpose"|is.na(outvar)) %>%  ## Drop sense of purpose so we have a single subjective wellbeing measure
  separate(Level2, c("Level2", "Level3"), "_") %>% ## Separate numbers of domains in very good and poor wellbeing into Level 2 and Level 3
  mutate(value=as.numeric(value),se_value=as.numeric(se_value)) %>% 
  dplyr::rename(PopGrp=Var1,Population=Level1,Var1=Var2,Level1=Level2,Level2=Level3)

wellbeing.means.df$PopGrp <- factor(wellbeing.means.df$PopGrp,levels=wellbeing.pops$var,labels=wellbeing.pops$description)
wellbeing.means.df$Var1 <- factor(wellbeing.means.df$Var1,levels=wellbeing.doms$var,labels=wellbeing.doms$description)

theme.bar <- theme_bw()+
  theme(axis.title=element_text(size=8),
        legend.title = element_text(size=8), 
        legend.text = element_text(size=7),
        plot.caption = element_text(size=7),
        axis.text = element_text(size=8),
        legend.position = "right",
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank()) # no grid is element_blank()

##############################################################################
# Now read in the subdomain data
##############################################################################
wellbeing2.df.raw <- read.csv("Supplementary subdomain GSS tables 2014 2016 with std errors checked.csv",colClasses=c(rep("character",5),rep("numeric",20)))

wellbeing2.pops <- as.data.frame(read_excel("Final GSS tables 2014 2016 Subdomain Metadata.xlsx", sheet = "Populations"))
wellbeing2.doms <- as.data.frame(read_excel("Final GSS tables 2014 2016 Subdomain Metadata.xlsx", sheet = "Domains"))
wellbeing2.subpops <- as.data.frame(read_excel("Final GSS tables 2014 2016 Subdomain Metadata.xlsx", sheet = "Pop order")) %>% 
  mutate(Subpop = replace(Subpop,Subpop=="Maori ethnicity",paste0("M",sprintf('\u0101'),"ori ethnicity"))) 

# Re-classify values as needed
wellbeing2.df <- wellbeing2.df.raw %>% 
  subset(Level1 != "No" & !is.na(sum)) %>%  # Exclude the no categories for each ethnic group
  filter((Var2 != "full_purpose")|is.na(Var2)) %>%  ## Drop sense of purpose so we have a single subjective wellbeing measure
  mutate(Level1 = replace(Level1,Var1=="agegrp2"&Level1=="1","15 to 24 years")) %>%
  mutate(Level1 = replace(Level1,Var1=="agegrp2"&Level1=="5","80+ years")) %>%
  subset(!(Var1=="agegrp2"&Level1 %in% c("2","3","4"))) %>%  # Exclude other agegrps from agegrp2 already included in agegrp
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="1","Quintile 1")) %>%
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="2","Quintile 2")) %>%
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="3","Quintile 3")) %>%
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="4","Quintile 4")) %>%
  mutate(Level1 = replace(Level1,Var1=="nzdep5"&Level1=="5","Quintile 5")) %>%
  mutate(Level1 = replace(Level1,Var1=="asian","Asian ethnicity")) %>% 
  mutate(Level1 = replace(Level1,Var1=="maori",paste0("M",sprintf('\u0101'),"ori ethnicity"))) %>% 
  mutate(Level1 = replace(Level1,Var1=="european","European ethnicity")) %>% 
  mutate(Level1 = replace(Level1,Var1=="pacific","Pacific ethnicity")) %>% 
  mutate(Var1 = replace(Var1,Var1=="agegrp2","agegrp")) %>% 
  mutate(Var2 = replace(Var2,Var2=="","ALL")) %>% 
  mutate(Level2 = replace(Level2,Level2=="","ALL")) %>% 
  mutate(Var3 = replace(Var3,Var3=="","ALL")) %>% 
  mutate(Level3 = replace(Level3,Level3=="","ALL")) %>% 
  mutate(Var1 = replace(Var1,Var1 %in% c("asian","maori","european","pacific"),"ethnicity")) %>% 
  mutate(Level2 = replace(Level2,Level2=="Very good","High")) %>%
  mutate(Level2 = replace(Level2,Level2=="Good","Medium")) %>% 
  mutate(Level2 = replace(Level2,Level2=="Poor","Low")) %>% 
  mutate(Level2 = replace(Level2,Level2=="1 Very poor","Very low")) %>%
  mutate(Level2 = replace(Level2,Level2=="2 Poor","Low")) %>%
  mutate(Level2 = replace(Level2,Level2=="3 Good","Medium")) %>%
  mutate(Level2 = replace(Level2,Level2=="4 Very good","High")) %>% 
  mutate(Level2 = replace(Level2,Level2=="5 Excellent","Very high")) %>% 
  mutate(Level2 = replace(Level2,Level2=="1 Very poor to poor","Very low to low")) %>%
  mutate(Level2 = replace(Level2,Level2=="2 Good","Medium")) %>%
  mutate(Level2 = replace(Level2,Level2=="3 Very good to exce","High to very high")) %>% 
  mutate(Level3 = replace(Level3,Level3=="-1"&substr(Var3,1,4) %in% c("full","hous"),"Low")) %>%
  mutate(Level3 = replace(Level3,Level3=="-0.5"&substr(Var3,1,4) %in% c("full","hous"),"Low")) %>%
  mutate(Level3 = replace(Level3,Level3=="0"&substr(Var3,1,4) %in% c("full","hous"),"Medium")) %>%
  mutate(Level3 = replace(Level3,Level3=="1"&substr(Var3,1,4) %in% c("full","hous"),"High")) %>%
  mutate(Level3 = replace(Level3,Level3=="0"&substr(Var3,1,3)=="low","High")) %>%
  mutate(Level3 = replace(Level3,Level3=="1"&substr(Var3,1,3)=="low","Low")) %>%
  mutate(Level3 = replace(Level3,Level3=="High"&substr(Var3,1,8)=="house_cr","High")) %>%
  mutate(restsum = as.numeric(restsum)) %>% 
  mutate(restpop = as.numeric(restpop)) %>% 
  mutate(pct_rest = as.numeric(pct_rest)) %>% 
  mutate(ppt_diff = as.numeric(ppt_diff)) %>% 
  mutate(se_ppt_diff = as.numeric(se_ppt_diff)) %>% 
  dplyr::rename(PopGrp=Var1,Population=Level1,Var1=Var2,Level1=Level2,Var2=Var3,Level2=Level3) 

## Create variables as factors with appropriate ordering
wellbeing2.df$PopGrp <- factor(wellbeing2.df$PopGrp,levels=wellbeing2.pops$var,labels=wellbeing2.pops$description)
wellbeing2.df$Var1 <- factor(wellbeing2.df$Var1,levels=wellbeing2.doms$var,labels=wellbeing2.doms$description)
wellbeing2.df$Var2 <- factor(wellbeing2.df$Var2,levels=wellbeing2.doms$var,labels=wellbeing2.doms$description)

domain.vars2 <- wellbeing2.doms$var[c(2,4:11)]
domain.names2 <- wellbeing2.doms$description[c(2,4:11)]
subdomain.names <- wellbeing2.doms$description[17:32]
pop.vars2 <- wellbeing2.pops$var
pop.names2 <- wellbeing2.pops$description

wellbeing2.df$Population <- factor(wellbeing2.df$Population,levels=wellbeing2.subpops$Subpop)
populations2 <- unique(subset(wellbeing2.df$Population,!is.na(wellbeing2.df$Population)))

###########################################################
# Read in child weighted data
###########################################################

wellbeingchild.df.raw <- read.csv("Final GSS tables 2014 2016 Child weights checked.csv",colClasses=c(rep("character",4),rep("numeric",10)))

# Re-classify values as needed
wellbeingchild.df <- wellbeingchild.df.raw %>% 
  subset(Level1 != "No") %>%  # Exclude the no categories for each ethnic group
  filter(!grepl('Missing', Level1)) %>%  # Exclude rows matching missing population groups
  mutate(Level1 = replace(Level1,Level1=="1","Quintile 1")) %>%
  mutate(Level1 = replace(Level1,Level1=="2","Quintile 2")) %>%
  mutate(Level1 = replace(Level1,Level1=="3","Quintile 3")) %>%
  mutate(Level1 = replace(Level1,Level1=="4","Quintile 4")) %>%
  mutate(Level1 = replace(Level1,Level1=="5","Quintile 5")) %>%
  mutate(Level1 = replace(Level1,Var1=="asian","Asian ethnicity")) %>% 
  mutate(Level1 = replace(Level1,Var1=="maori",paste0("M",sprintf('\u0101'),"ori ethnicity"))) %>% 
  mutate(Level1 = replace(Level1,Var1=="european","European ethnicity")) %>% 
  mutate(Level1 = replace(Level1,Var1=="pacific","Pacific ethnicity")) %>% 
  mutate(Var2 = replace(Var2,Var2=="","ALL")) %>% 
  mutate(Level2 = replace(Level2,Level2=="","ALL")) %>% 
  mutate(Var1 = replace(Var1,Var1 %in% c("asian","maori","european","pacific"),"ethnicity")) %>% 
  mutate(Level2 = replace(Level2,Level2=="Very good","High")) %>%
  mutate(Level2 = replace(Level2,Level2=="Good","Medium")) %>% 
  mutate(Level2 = replace(Level2,Level2=="Poor","Low")) %>% 
  mutate(Level2 = replace(Level2,Level2=="1 Very poor","Very low")) %>%
  mutate(Level2 = replace(Level2,Level2=="2 Poor","Low")) %>%
  mutate(Level2 = replace(Level2,Level2=="3 Good","Medium")) %>%
  mutate(Level2 = replace(Level2,Level2=="4 Very good","High")) %>% 
  mutate(Level2 = replace(Level2,Level2=="5 Excellent","Very high")) %>% 
  mutate(Level2 = replace(Level2,Level2=="1 Very poor to poor","Very poor to poor")) %>%
  mutate(Level2 = replace(Level2,Level2=="2 Good","Medium")) %>%
  mutate(Level2 = replace(Level2,Level2=="3 Very good to exce","Very good to excellent")) %>% 
  filter((Var2 != "full_purpose")|is.na(Var2)) %>%  ## Drop sense of purpose so we have a single subjective wellbeing measure
  mutate(restsum = as.numeric(restsum)) %>% 
  mutate(restpop = as.numeric(restpop)) %>% 
  mutate(pct_rest = as.numeric(pct_rest)) %>% 
  mutate(ppt_diff = as.numeric(ppt_diff)) %>% 
  dplyr::rename(PopGrp=Var1,Population=Level1,Var1=Var2,Level1=Level2) 


## Create variables as factors with appropriate ordering
wellbeingchild.df$PopGrp <- factor(wellbeingchild.df$PopGrp,levels=wellbeing.pops$var,labels=wellbeing.pops$description)
wellbeingchild.df$Var1 <- factor(wellbeingchild.df$Var1,levels=wellbeing.doms$var,labels=wellbeing.doms$description)

## Data now ready for analysis

hours.df <- wellbeing.df %>% 
  subset(PopGrp=="Hours worked by age")

year.df <- wellbeing.df %>% 
  subset(year=="2014")

##############################################################################
## Now visualisations  
##############################################################################

##############################################################################
## First a stacked bar chart of the high level distribution of 
## wellbeing groupings across domains - Also print population distribution
##############################################################################

wellbeing.df$Population <- factor(wellbeing.df$Population,levels=c(wellbeing.subpops$Subpop,"Missing"))

popdist.df <- wellbeing.df %>%
  dplyr::filter(Var2=="Total" & Var1=="Total" & !Population %in% c("ALL","2014","2016")
                &PopGrp!="Hours worked" ) %>% 
  mutate(Population=replace(Population,is.na(Population),"Missing")) %>% 
  mutate(PopGrp=replace(PopGrp,PopGrp=="Hours worked by age","Hours worked"))

print(popdist.df[,c(1,2,10)])

plot.domain.dist <- function(pop) {
## Subset the data
## pop <- "ALL"
alldomain.df <- wellbeing.df %>%
    dplyr::filter(Population==pop & Var2=="Total" & Var1 %in% domain.names)

alldomain.df$Var1 <- droplevels(alldomain.df$Var1)
alldomain.df$Level1 <- factor(alldomain.df$Level1,levels=c("High","Medium","Low"))

(domain.plot <- ggplot(alldomain.df) +
    aes(x = Var1, y = pct_subtot, fill=Level1, label=round(pct_subtot,0))+
    geom_bar(stat="identity",position="stack",width=0.6) +
    geom_text(size =2.5, position = position_stack(vjust = 0.5),colour="grey20")+
    geom_vline(xintercept=c(8.5),lwd=0.4, colour="black")+
    coord_flip()+
    ylab("Population (%)")+
    xlab("Domain")+
    scale_y_continuous(label=comma,expand = c(0, 0),breaks=c(0,20,40,60,80,100))+
    scale_x_discrete(limits = rev(levels(alldomain.df$Var1)))+
    labs(fill="Wellbeing",caption="Source: New Zealand General Social Survey 2014/2016")+
    scale_fill_manual(values=c(teal,green,lt_yellow))+
    theme.bar+
    guides(fill = guide_legend(reverse = TRUE)))

pop2 <- chartr(",()<>","   LG",pop)
plotname<-paste(dir.visualisations,"/Domain plot - ",pop2,".png",sep="")
ggsave(plotname, plot=domain.plot, width = 16, height = 9, units = "cm")
}

## Run for every population subgroup
for(pop in populations){
  plot.domain.dist(pop)
}

####################################################################
## Output summary data to excel tables
####################################################################
hs <- createStyle(textDecoration = "BOLD")

## First need to run code that creates wellbeing.df (domain data) and wellbeing2.df (subdomain data)

## First the domain data - start by removing fields and rows not used in the dashboard

wellbeing.df.db <- wellbeing.df %>% 
  dplyr::select(year,PopGrp,Population,Var1,Level1,Var2,Level2,pct_subtot,ppt_diff,ppt_dom) %>% #Remove sampling errors etc. Just keep labels and percents and ppt differences that we use
  filter(substring(Var1,1,5) != "Multi" & substring(Var1,1,7) != "Domains"&substring(Var2,1,5) != "Multi" & substring(Var2,1,7) != "Domains") %>%
  filter(!(Level1=="Medium"&Level2=="Medium")&!(Level1=="Medium"&Level2=="Low")&!(Level1=="Medium"&Level2=="High")
         &!(Level1=="High"&Level2=="Medium")&!(Level1=="Low"&Level2=="Medium")) 

## And then format for exporting  
wellbeing.df.db.xl <- wellbeing.df.db %>% 
  mutate(PopGrp=as.character(PopGrp),Population=as.character(Population),year=as.character(year)) %>% #Reformat as characters so we can add titles easily
  mutate(year=replace(year,year=="9999","2014/2016 combined")) %>% 
  mutate(Population=replace(Population,Population=="ALL","Total NZ")) %>% 
  mutate(Level1=replace(Level1,Level1=="ALL","Total"),Level2=replace(Level2,Level2=="ALL","Total")) %>% 
  dplyr::rename('Survey year'=year,'Population characteristic'=PopGrp,'Population group'=Population,
                'Wellbeing domain 1'=Var1,'Domain 1 level'=Level1,'Wellbeing domain 2'=Var2,'Domain 2 level'=Level2,
                'Percentage of population group'=pct_subtot,'% point difference from rest of population'=ppt_diff,
                '% point difference in Domain 2 given Domain 1'=ppt_dom)

# And export to excel
write.xlsx(wellbeing.df.db.xl, 'lsf_our_people_domain_data.xlsx',headerStyle=hs,colWidths="auto")


## Now the subdomain data - start by removing fields and rows not used in the dashboard
wellbeing2.df.db <- wellbeing2.df %>% 
  filter(Var1=="Total") %>% 
  dplyr::select(PopGrp,Population,Var2,Level2,pct_subtot) #Remove sampling errors etc. Just keep labels and percents and ppt differences that we use

## And then format for exporting  
wellbeing2.df.db.xl <- wellbeing2.df.db %>% 
  mutate(PopGrp=as.character(PopGrp),Population=as.character(Population)) %>% #Reformat as characters so we can add titles easily
  mutate(Population=replace(Population,Population=="ALL","Total NZ")) %>% 
  mutate(Level2=replace(Level2,Level2=="ALL","Total")) %>% 
  dplyr::rename('Population characteristic'=PopGrp,'Population group'=Population,
                'Wellbeing subdomain'=Var2,'Subdomain level'=Level2,'Percentage of population group'=pct_subtot)

# And export to excel
write.xlsx(wellbeing2.df.db.xl, 'lsf_our_people_subdomain_data.xlsx',headerStyle=hs,colWidths="auto")


#####################################################################################################
## Now tables for the Analytical paper
#####################################################################################################
hs <- createStyle(textDecoration = "BOLD")

## Domain percentages - including by sub-population
domain.summary.xl <- wellbeing.df %>% 
  filter(year==9999&Var2=='Total'&Var1!="Total"&PopGrp!="Survey year"&!Population=="") %>% #PopGrp=='Total population'& 
  dplyr::mutate(Level1=factor(Level1,levels=c("Very low","Low","Very low to low","Medium","High to very high","High","Very high","-8","-7","-6","-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8"))) %>% 
  dplyr::arrange(PopGrp,Population,Var1,Level1) %>% 
  mutate(PopGrp=as.character(PopGrp),Population=as.character(Population)) %>% #Reformat as characters so we can add titles easily
  filter(substring(Var1,1,5) != "Multi" & substring(Var1,1,7) != "Domains"&substring(Var2,1,5) != "Multi" & substring(Var2,1,7) != "Domains") %>%
  mutate(Population=replace(Population,Population=="ALL","Total NZ")) %>% 
  dplyr::select(PopGrp,Population,Var1,Level1,pct_subtot,se_pct_subtot) %>% 
  dplyr::rename('Population characteristic'=PopGrp,'Population group'=Population,'Wellbeing domain'=Var1,'Domain level'=Level1,'% of population group'=pct_subtot,'Std err % of pop group'=se_pct_subtot)

write.xlsx(domain.summary.xl, 'Our people - Wellbeing domain percentages.xlsx',headerStyle=hs,colWidths="auto")

## Subdomain percentages
subdomain.summary.xl <- wellbeing2.df %>% 
  filter(Var1=='Total'&Var2!="Total"&PopGrp!="Survey year"&!Population==""&Population!="80+ years"&Population!="15 to 24 years") %>% #PopGrp=='Total population'& 
  mutate(Level2=factor(Level2,levels=c("Low","Medium","High"))) %>% 
  dplyr::arrange(PopGrp,Population,Var2,Level2) %>% 
  mutate(PopGrp=as.character(PopGrp),Population=as.character(Population)) %>% #Reformat as characters so we can add titles easily
  mutate(Population=replace(Population,Population=="ALL","Total NZ"),PopGrp=replace(PopGrp,PopGrp=="Employment status","Hours worked")) %>% 
  #filter(substring(Var1,1,5) != "Multi" & substring(Var1,1,7) != "Domains"&substring(Var2,1,5) != "Multi" & substring(Var2,1,7) != "Domains") %>%
  dplyr::select(PopGrp,Population,Var2,Level2,pct_subtot,se_pct_subtot) %>% 
  dplyr::rename('Population characteristic'=PopGrp,'Population group'=Population,'Wellbeing subdomain'=Var2,'Subdomain level'=Level2,'% of population'=pct_subtot,'Std err % of pop group'=se_pct_subtot)

write.xlsx(subdomain.summary.xl, 'Our people - Wellbeing subdomain percentages.xlsx',headerStyle=hs,colWidths="auto")

## Domain comparisons
group.domcomp.xl <- wellbeing.df %>% 
  filter(year==9999&PopGrp!='Total population'&Var2=='Total'&Var1!="Total"&!Population==""&PopGrp!="Survey year") %>% 
  filter(substring(Var1,1,5) != "Multi" & substring(Var1,1,7) != "Domains"&substring(Var2,1,5) != "Multi" & substring(Var2,1,7) != "Domains") %>%
  dplyr::select(PopGrp,Population,Var1,Level1,pct_subtot,pct_rest,ppt_diff,se_ppt_diff) %>% 
  dplyr::mutate(Level1=factor(Level1,levels=c("Very low","Low","Very low to low","Medium","High to very high","High","Very high","-8","-7","-6","-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8"))) %>% 
  dplyr::arrange(PopGrp,Population,Var1,Level1) %>% 
  #mutate(pct_subtot=format(round(pct_subtot, 2),nsmall=2),pct_rest=format(round(pct_rest, 2),nsmall=2),ppt_diff=format(round(ppt_diff, 2),nsmall=2),se_ppt_diff=format(round(se_ppt_diff, 2),nsmall=2)) %>%
  mutate(PopGrp=as.character(PopGrp),Population=as.character(Population)) %>% #Reformat as characters so we can add titles easily
  dplyr::rename('Population characteristic'=PopGrp,'Population group'=Population,
                'Wellbeing domain'=Var1,'Domain level'=Level1,'% of population group'=pct_subtot,'% of rest of population'=pct_rest,'% point diff from rest of pop'=ppt_diff,'Std err % point diff'=se_ppt_diff)

write.xlsx(group.domcomp.xl, 'Our people - Wellbeing domain population group comparisons.xlsx',headerStyle=hs,colWidths="auto")

## Subdomain comparisons
group.subdomcomp.xl <- wellbeing2.df %>% 
  filter(PopGrp!='Total population'&Var1=='Total'&Var2!="Total"&Level2!="Medium"&!Population==""&PopGrp!="Survey year"&!Population==""&Population!="80+ years"&Population!="15 to 24 years") %>% 
  filter(substring(Var1,1,5) != "Multi" & substring(Var1,1,7) != "Domains"&substring(Var2,1,5) != "Multi" & substring(Var2,1,7) != "Domains") %>%
  dplyr::select(PopGrp,Population,Var2,Level2,pct_subtot,pct_rest,ppt_diff,se_ppt_diff) %>% 
  dplyr::mutate(Level2=factor(Level2,levels=c("Low","Medium","High"))) %>% 
  dplyr::arrange(PopGrp,Population,Var2,Level2) %>% 
  mutate(PopGrp=as.character(PopGrp),Population=as.character(Population)) %>% #Reformat as characters so we can add titles easily
  mutate(PopGrp=replace(PopGrp,PopGrp=="Employment status","Hours worked")) %>% 
  dplyr::rename('Population characteristic'=PopGrp,'Population group'=Population,
                'Wellbeing subdomain'=Var2,'Subdomain level'=Level2,'% of population group'=pct_subtot,'% of rest of population'=pct_rest,'% point diff from rest of pop'=ppt_diff,'Std err % point diff'=se_ppt_diff)

unique(subdomain.summary.xl$`Population characteristic`)

write.xlsx(group.subdomcomp.xl, 'Our people - Wellbeing subdomain population group comparisons.xlsx',headerStyle=hs,colWidths="auto")

## Heatmaps and heatmap comparisons
wellbeing.df$Population <- factor(wellbeing.df$Population,levels=c("Total NZ",wellbeing.subpops$Subpop,"Missing"))
heatmap.xl <- wellbeing.df %>% 
  filter(year==9999&Var2!='Total'&Var1!="Total"&PopGrp!="Survey year"&!Population=="") %>% 
  filter(substring(Var1,1,5) != "Cross" & substring(Var1,1,7) != "Domains"&substring(Var2,1,5) != "Cross" & substring(Var2,1,7) != "Domains") %>%
  filter(!(Level1=="Medium"&Level2=="Medium")&!(Level1=="Medium"&Level2=="Low")&!(Level1=="Medium"&Level2=="High")
         &!(Level1=="High"&Level2=="Medium")&!(Level1=="Low"&Level2=="Medium")
         &!(Level1=="High"&Level2=="Low")&!(Level1=="Low"&Level2=="High")) %>% 
  dplyr::arrange(PopGrp,Population,Level1,Var1,Level2,Var2) %>% 
  mutate(PopGrp=as.character(PopGrp),Population=as.character(Population)) %>% #Reformat as characters so we can add titles easily
  mutate(Population=replace(Population,Population=="ALL","Total NZ")) %>% 
  mutate(pct_dom1=round(sum/domtotal*100,digits=2),pct_dom2=round((dom2total-sum)/(subtotal-domtotal)*100,digits=2)) %>% 
  dplyr::select(PopGrp,Population,Var1,Level1,Var2,Level2,pct_dom1,pct_dom2,ppt_dom,se_ppt_dom) %>% 
  dplyr::rename('Population characteristic'=PopGrp,'Population group'=Population,
                'Wellbeing domain 1'=Var1,'Dom 1 level'=Level1,'Wellbeing domain 2'=Var2,'Dom 2 level'=Level2,
                '% Dom 2 if Dom 1'=pct_dom1,'% Dom 2 if not Dom 1'=pct_dom2,
                '% point diff Dom 2 given Dom 1'=ppt_dom,
                'Std err % point diff'=se_ppt_dom)

write.xlsx(heatmap.xl, 'Our people - Domain associations by pop group.xlsx',headerStyle=hs,colWidths="auto")

# Child-weighted domain percentages
wellsumm.df<-filter(wellbeing.df,Population=="ALL" & Var2=="Total") %>% 
  dplyr::select(Var1,Level1,pct_subtot)

domain.summary.child.xl <- wellbeingchild.df %>% 
  filter(PopGrp=='Total population'&Var1!="Total") %>% 
  filter(substring(Var1,1,7) != "Domains") %>%  
  dplyr::mutate(Level1=replace(Level1,Level1=="Very poor to poor","Very low to low"),Level1=replace(Level1,Level1=="Very good to excellent","High to very high")) %>% 
  left_join(wellsumm.df,by=c('Var1','Level1')) %>% 
  dplyr::mutate(ppt_diff=pct_subtot.x-pct_subtot.y,Level1=factor(Level1,levels=c("Very low","Low","Very low to low","Medium","High to very high","High","Very high","-8","-7","-6","-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8"))) %>% 
  dplyr::arrange(PopGrp,Population,Var1,Level1) %>% 
  dplyr::select(Var1,Level1,pct_subtot.x,pct_subtot.y,ppt_diff) %>% 
  dplyr::rename('Wellbeing domain'=Var1,'Domain level'=Level1,'Child-weighted % of population'=pct_subtot.x,'Adult % of population'=pct_subtot.y,'% point diff child-weighted from adult pop'=ppt_diff)

write.xlsx(domain.summary.child.xl, 'Our people - Child-weighted wellbeing domain percentages.xlsx',headerStyle=hs,colWidths="auto")

## And mean life satisfaction by cross-domain wellbeing
domsgoodbadpop<- wellbeing.df %>% 
  filter(Population=="ALL"&((Var1=="Domains in poor wellbeing"&Var2=="Domains in very good wellbeing")|(substring(Var1,1,5)=="Cross")&Var2=="Total")) %>% 
  mutate(Var1=as.character(Var1),Var2=as.character(Var2)) %>% 
  mutate(Var2=replace(Var2,Var2=="Domains in very good wellbeing","Domains with high wellbeing"),Var1=replace(Var1,Var1=="Domains in poor wellbeing","Domains with low wellbeing")) %>% 
  mutate(Level2=replace(Level2,Var2!="Domains with high wellbeing",NA),Var2=replace(Var2,Var2!="Domains with high wellbeing",NA)) %>% 
  dplyr::select(Var1,Level1,Var2,Level2,pct_subtot)

domain.means.xl <- wellbeing.means.df %>% 
  filter(Population=="ALL"&outvar=='Life satisfaction'&(substring(Var1,1,5)=="Cross"|is.na(Var1))) %>% 
  mutate(Var1=as.character(Var1),Var2=NA) %>% 
  mutate(Var2=replace(Var2,is.na(Var1),"Domains with high wellbeing"),Var1=replace(Var1,is.na(Var1),"Domains with low wellbeing")) %>% 
  left_join(domsgoodbadpop,by=c('Var1','Level1','Var2','Level2')) %>% 
  mutate(Level1=factor(Level1,levels=c("Very low","Low","Very low to low","Medium","High to very high","High","Very high","-8","-7","-6","-5","-4","-3","-2","-1","0","1","2","3","4","5","6","7","8"))) %>% 
  dplyr::select(Var1,Level1,Var2,Level2,pct_subtot,value,se_value) %>% 
  dplyr::arrange(desc(Var1),Level1,Var2,Level2) %>% 
  dplyr::rename('Wellbeing domain 1'=Var1,'Domain level 1'=Level1,'Wellbeing domain 2'=Var2,'Domain level 2'=Level2,'% of population'=pct_subtot,'Mean life satisfaction (0-10 scale)'=value,'Std err mean life sat'=se_value)

write.xlsx(domain.means.xl, 'Our people - Domain wellbeing by mean life satisfaction.xlsx',headerStyle=hs,colWidths="auto")


##############################################################################
## Now distribution of the population by number of wellbeing indicators
##############################################################################
my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)

plot.multidomain <- function(pop) {
  # pop <- "ALL"

  ## Now get the data with the number of domains at each level of wellbeing
  domsgoodbadpop<- wellbeing.df %>% 
    filter(Population==pop&Var1=="Domains in poor wellbeing"&Var2=="Domains in very good wellbeing") %>% 
    dplyr::select(Level1,Level2,pct_subtot)
  
  domsgoodbad.df <- wellbeing.means.df %>%
    filter(Population==pop&is.na(Var1) & outvar=='Life satisfaction') %>% 
    left_join(domsgoodbadpop,by=c('Level1','Level2'))
  
  domsgoodbadls.df <- dplyr::filter(domsgoodbad.df,outvar=="Life satisfaction") 
  (domsgdbdls.plot <- ggplot(domsgoodbadls.df) +
      aes(x = Level1,y=Level2,size=pct_subtot,colour=value)+
      geom_abline(slope=1,intercept=c(-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8),colour="grey80",lwd=0.3)+
      geom_point() + #shape=21
      xlab("Domains with low wellbeing")+
      ylab("Domains with high wellbeing")+
      scale_colour_gradientn(colours=c(yellow,lt_yellow,lt_yellow,green,green,teal,dk_teal))+ 
      scale_size_area(max_size = 10,labels=comma)+ 
      labs(colour="Mean life\nsatisfaction\n(0-10)",size="Estimated\npopulation\n(%)",caption="Source: New Zealand General Social Survey 2014/2016\nDiagonal lines indicate Cross-domain wellbeing scores.")+
      #labs(colour="Mean life\nsatisfaction\n(0-10)",size="Estimated\npopulation\n(%)",caption="Source: New Zealand General Social Survey 2014/2016")+
      theme_bw()+
      annotate("text",x=6,y=6,label=paste(sprintf('\u2190')," Cross-domain wellbeing score ",sprintf('\u2192')),size=3.5,angle=315.5)+
      theme(panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(),
            legend.title = element_text(size=8), 
            legend.text = element_text(size=6),
            axis.text = element_text(size=8))
  )
  
  pop2 <- chartr(",()<>","   LG",pop)
  # plotname<-paste(dir.visualisations,"/Multi-dimension bubble plot LS 1 - ",pop2,".png",sep="")
  # ggsave(plotname, plot=domsgdbdls.plot, width = 15, height = 11, units = "cm")
  # 
  #  (domsgdbdls.plot2 <- domsgdbdls.plot+
  #      geom_abline(slope=1,intercept=c(-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8),colour="grey80",lwd=0.3))
  # 
  #  plotname<-paste(dir.visualisations,"/Multi-dimension bubble plot LS 2 - ",pop2,".png",sep="")
  #  ggsave(plotname, plot=domsgdbdls.plot2, width = 15, height = 11, units = "cm")
  # 
  for (well in -7:7){
    domsgdbdls.plot <- domsgdbdls.plot+annotate("text",x=(9-(well+7)/2),y=((well+7)/2+2),label=well,size=2.5,angle=40)
  }
  plotname<-paste(dir.visualisations,"/Multi-dimension bubble plot LS 3 - ",pop2,".png",sep="")
  ggsave(plotname, plot=domsgdbdls.plot, width = 14, height = 12, units = "cm")
    
  welldomspop.df <- wellbeing.df %>% 
    dplyr::filter(Population==pop& Var1 %in% c("Cross-domain wellbeing groups","Cross-domain wellbeing") &Level2=="ALL")
  
  welldomspop.df1<-welldomspop.df %>% 
    filter(Var1=="Cross-domain wellbeing groups")
  welldomspop.df1$Level1 <- factor(welldomspop.df1$Level1,levels=unique(welldomspop.df1$Level1))
  pal <- c(yellow,lt_yellow,green,teal,dk_teal)
  (domsdist.plot2 <- ggplot(welldomspop.df1) +
      aes(x = Level1, y = pct_tot, fill=Level1,label=round(pct_tot,0))+
      geom_bar(stat="identity",width=0.7) +
      scale_fill_manual(values=pal)+
      geom_text(size =3, vjust = -0.3)+
      ylab("Percent of population")+
      xlab("Cross-domain wellbeing groups")+
      theme.bar+
      theme(legend.position="none",panel.grid.major.x = element_blank())+
      scale_y_continuous(expand = c(0, 0),limits=c(0,max(welldomspop.df$pct_tot)+10)))
  plotname<-paste(dir.visualisations,"/Cross-domain wellbeing groups 1.png",sep="")
  ggsave(plotname, plot=domsdist.plot2, width = 15, height = 8, units = "cm")
  
  welldomspop.df2<-welldomspop.df %>% 
    filter(Var1=="Cross-domain wellbeing")
  welldomspop.df2$Level1 <- factor(welldomspop.df2$Level1,levels=unique(welldomspop.df2$Level1))
  pal <- c(lt_yellow,green,teal)
  (domsdist.plot3 <- ggplot(welldomspop.df2) +
      aes(x = Level1, y = pct_tot, fill=Level1,label=round(pct_tot,0))+
      geom_bar(stat="identity",width=0.7) +
      scale_fill_manual(values=pal)+
      geom_text(size =3, vjust = -0.3)+
      ylab("Percent of population")+
      xlab("Cross-domain wellbeing groups")+
      theme.bar+
      theme(legend.position="none",panel.grid.major.x = element_blank())+
      scale_y_continuous(expand = c(0, 0),limits=c(0,max(welldomspop.df$pct_tot)+10)))
  plotname<-paste(dir.visualisations,"/Cross-domain wellbeing groups 2.png",sep="")
  ggsave(plotname, plot=domsdist.plot3, width = 15, height = 8, units = "cm")
  }

plot.multidomain('ALL')

## Now show Cross-domain wellbeing by population subgroup
plot.multidomain.dist <- function(popgrp) {
  #popgrp<-"Ethnic group"
  
  multidomain.df <- wellbeing.df %>%
    dplyr::filter(PopGrp==popgrp & Var2=="Total" & Var1=="Cross-domain wellbeing groups"&!is.na(Population))
  multidomain.df$Level1 <- factor(multidomain.df$Level1,levels=c("Very high","High","Medium","Low","Very low"))
  
  (multidomain.plot <- ggplot(multidomain.df) +
      aes(x = Population, y = pct_subtot, fill=Level1, label=round(pct_subtot,0))+
      geom_bar(stat="identity",position="stack",width=0.6) +
      geom_text(size =2.5, position = position_stack(vjust = 0.5),aes(colour=Level1))+
      scale_colour_manual(values=c("white","grey20","grey20","grey20","white"))+
      coord_flip()+
      ylab("Population (%)")+
      xlab("Population")+
      scale_y_continuous(label=comma,expand = c(0, 0),breaks=c(0,20,40,60,80,100))+
      scale_x_discrete(limits = rev(unique(multidomain.df$Population)))+
      labs(fill="Cross-domain wellbeing",caption="Source: New Zealand General Social Survey 2014/2016")+
      scale_fill_manual(values=c(dk_teal,teal,green,lt_yellow,yellow))+
      theme.bar+
      guides(fill = guide_legend(reverse = TRUE),colour = FALSE))
  
  pop2 <- chartr(",()<>","   LG",popgrp)
  plotname<-paste(dir.visualisations,"/Cross-domain plot - ",pop2,".png",sep="")
  ht <- length(unique(multidomain.df$Population))*1.4+2
  ggsave(plotname, plot=multidomain.plot, width = 16, height = ht, units = "cm")
}

for(popgrp in pop.names[-1]){
  plot.multidomain.dist(popgrp)
}

pop <- "ALL"
my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
welldoms.ls.df <- wellbeing.means.df %>%
  dplyr::filter(Population==pop & Var1=="Cross-domain wellbeing score" & outvar=="Life satisfaction")
welldoms.df <- wellbeing.df %>%
  dplyr::filter(Population==pop & Var1=="Cross-domain wellbeing score" & Var2=="Total") %>% 
  left_join(welldoms.ls.df,by='Level1') %>% 
  dplyr::select(Level1,Var2,value,pct_tot)
welldoms.df$Level1 <- factor(as.numeric(welldoms.df$Level1),levels=c(-8:8))

(domsdist.plot <- ggplot(welldoms.df) +
    aes(x = Level1, y = pct_tot, fill=value,label=round(pct_tot,0))+
    geom_bar(stat="identity",width=0.7) +
    scale_fill_gradientn(colours=c(yellow,lt_yellow,green,teal,dk_teal),
                         values=c(0,0.25,0.6,0.85,1))+ 
    ylab("Percent of population")+
    xlab("Cross-domain wellbeing score")+
    labs(fill="Mean life\nsatisfaction\n(0-10)",caption="Source: New Zealand General Social Survey 2014/2016")+
    theme.bar+
    theme(panel.grid.major.x = element_blank())+
    scale_y_continuous(expand = c(0, 0),limits=c(0,max(welldoms.df$pct_tot)+5)))
ggsave(paste0(dir.visualisations,"/Cross-domain wellbeing score population plot 1.png"), plot=domsdist.plot, width = 17, height = 8, units = "cm")
domsdist.plot <- domsdist.plot+geom_vline(xintercept=c(5.5,7.5,11.5,13.5),lwd=0.4, colour="grey70")+annotate("text",x=2.5,y=17.5,label="Very low",size=2.5)+annotate("text",x=6.5,y=17.5,label="Low",size=2.5)+annotate("text",x=9.5,y=17.5,label="Medium",size=2.5)+annotate("text",x=12.5,y=17.5,label="High",size=2.5)+annotate("text",x=15.5,y=17.5,label="Very high",size=2.5)+geom_text(size=2, vjust = -0.3)
# domsdist.plot2 <<- domsdist.plot+scale_y_continuous(expand = c(0, 0),limits=c(0,30))+geom_vline(xintercept=c(5.5,8.5,12.5),lwd=0.4, colour="grey70")+annotate("text",x=3,y=26.5,label="Very low",size=2.5)+annotate("text",x=7,y=26.5,label="Low",size=2.5)+annotate("text",x=10.5,y=26.5,label="Medium",size=2.5)+annotate("text",x=15,y=26.5,label="Very high",size=2.5)+geom_text(size=2, vjust = -0.3)
# domsdist.plot <- domsdist.plot+geom_text(size=3, vjust = -0.3)

ggsave(paste0(dir.visualisations,"/Cross-domain wellbeing score population plot 2.png"), plot=domsdist.plot, width = 17, height = 8, units = "cm")



##############################################################################
## Now do plots the same way as in the dashboard - dodged high and low
##############################################################################
subgroup.df.ppt <- wellbeing.df %>%
  dplyr::filter(Population != "ALL" & Var1 %in% domain.names & Var2=="Total" &Level1!="Medium") %>% 
  dplyr::select(PopGrp,Population,Var1,Level1,ppt_diff,se_ppt_diff) %>% 
  mutate(ppt_diff=replace(ppt_diff,Level1=="Rest of the population",0),se_ppt_diff=replace(se_ppt_diff,Level1=="Rest of the population",NA)) %>% 
  dplyr::rename(Wellbeing=Level1) %>% 
  arrange(Var1)

PopGrps <- unique(subgroup.df.ppt$PopGrp)

for(grp in PopGrps) {
  # grp <- "Ethnic group"
  max <- 41
  min <- -41
  
  plot.df <- subset(subgroup.df.ppt,PopGrp==grp) #%>% 
  #mutate(Population=paste(Population,"-",Wellbeing,"wellbeing"))
  plot.df$Population <- gsub("\\(", "\n\\(",plot.df$Population)
  plot.df$Population = factor(plot.df$Population,unique(plot.df$Population))
  
  domainpop.ppt.plotb <- ggplot(plot.df)+
    aes(x = Var1, y = ppt_diff, fill=Wellbeing)+
    geom_bar(stat="identity",position=position_dodge(),width=0.75) +
    geom_errorbar(aes(ymin=ppt_diff-se_ppt_diff, ymax=ppt_diff+se_ppt_diff),position=position_dodge(),width=0.75,size=0.3) +     
    coord_flip()+
    geom_hline(yintercept=0,lwd=0.3, colour="black")+
    ylab(paste("Percentage point difference in wellbeing compared to the rest of the population"))+
    xlab("Population group")+
    labs(caption="Source: New Zealand General Social Survey 2014/2016\nError bars show 95% confidence interval.")+
    theme.bar+
    theme(legend.position = "right")+ 
    guides(fill = guide_legend(reverse = FALSE))+
    scale_fill_manual(values=c(teal,lt_yellow))+
    scale_x_discrete(limits = rev(levels(droplevels(plot.df$Var1))))+
    scale_y_continuous(limits=c(min,max),breaks=c(-40,-20,0,20,40))+
    scale_colour_identity()
  
  (domainpop.ppt.plotb2<-domainpop.ppt.plotb+facet_grid(rows=vars(Population))+
      theme(strip.text.x = element_text(size = 6),strip.text.y = element_text(size = 6)))
  
  ht <- length(unique(as.array(plot.df$Population)))*3.5+2
  plotname<-paste(dir.visualisations,"/Ppt differences dodged bars - ",grp,".png",sep="")
  ggsave(plotname,plot=domainpop.ppt.plotb2, width = 15.5, height = ht, units = "cm")
}


##############################################################################
## Now heatmaps of associations between good and poor wellbeing domains
##############################################################################
plot.heatmaps <- function(pop,well1,well2) {
  # pop<-"Female"
  # well1<-"Low"
  # well2<-"Low"
  
  domheatmap.df <- wellbeing.df %>%
    dplyr::filter(Population==pop & Var1 %in% domain.names & Var2 %in% domain.names & Level1==well1&Level2==well2) %>% 
    mutate(ppt_dom=replace(ppt_dom,Var1==Var2,NA),se_ppt_dom=replace(se_ppt_dom,Var1==Var2,NA)) %>% 
    mutate(sig=ifelse((ppt_dom<0 & (ppt_dom+se_ppt_dom)<0)|(ppt_dom>0 & (ppt_dom-se_ppt_dom)>0),"*","")) %>% 
    mutate(label=paste0(signif(ppt_dom, 2),sig)) %>% 
    mutate(label=replace(label,label=="NANA","")) %>% 
    dplyr::select(Var1,Var2,ppt_dom,se_ppt_dom,sig,label)
  
  if (well1=="Low" & well2=="High") {
    samedom <- data.frame(Var2=domheatmap.df$Var2[1:10]) %>% 
      mutate(Var1=Var2,ppt_dom=NA,se_ppt_dom=NA,sig="",label="") %>% 
      dplyr::select(Var1,Var2,ppt_dom,se_ppt_dom,sig,label)
    domheatmap.df <- rbind(domheatmap.df,samedom)
  }
  
  domheatmap.df$Var1 <- factor(domheatmap.df$Var1,levels=rev(domain.names))

  domheatmap.df$Var1 <- droplevels(domheatmap.df$Var1)
  domheatmap.df$Var2 <- droplevels(domheatmap.df$Var2)
  
  domain.names2<- domain.names %>% 
    replace(domain.names=="Civic engagement and governance","Civic engagement\nand governance") %>% 
    replace(domain.names=="Income and consumption","Income and\nconsumption") %>% 
    replace(domain.names=="Subjective wellbeing","Subjective\nwellbeing") %>% 
    replace(domain.names=="Knowledge and skills","Knowledge\nand skills") %>% 
    replace(domain.names=="Safety and security","Safety and\nsecurity") %>% 
    replace(domain.names=="Social connections","Social\nconnections") 
    
  text_size<-9
  xl <- paste("Domain 2",well2,"wellbeing")
  yl <- paste("Domain 1",well1,"wellbeing")
  
  if (well1=="Low" & well2=="Low") {
    lowcol <- teal
    hicol <- lt_yellow
  } else {
    lowcol <- lt_yellow
    hicol <- teal
  }
  
  (p <- ggplot(domheatmap.df) 
    + geom_tile(aes(x=formatC(Var2, width=2, flag="0"),y=formatC(Var1, width=2, flag="0"),fill = ppt_dom),colour = "white")
    + geom_text(size=2,aes(x=formatC(Var2, width=2, flag="0"),y=formatC(Var1, width=2, flag="0"),label = label))
    + scale_fill_gradient2(na.value=lt_charcoal,low = lowcol,mid="white",high = hicol,midpoint=0)
    + theme_grey(base_size = text_size) 
    + labs(x = xl,y = yl,fill="Percentage point difference",title=element_blank(),
           caption="Source: New Zealand General Social Survey 2014/2016\n* indicates statistically significant differences from 0 at the 95% level") 
    + scale_x_discrete(position="top",expand = c(0, 0),labels=domain.names2) 
    + scale_y_discrete(expand = c(0, 0),labels=rev(domain.names2))
    + theme(legend.position="none",axis.ticks=element_blank(),axis.text.x=element_text(angle = 45, hjust = 0),
            plot.margin = unit(c(0.2,1,0.2,0.2), "cm"))
  )
  
  pop2 <- chartr(",()<>","   LG",pop)
  
  plotname<-paste(dir.visualisations,"/Domain heatmap - ",pop2,well1,well2,".png",sep="")
  ggsave(plotname,plot=p, width = 15, height = 11, units = "cm")
  }

for(popn in populations){
  plot.heatmaps(pop=popn,well1="Low",well2="Low")
  plot.heatmaps(pop=popn,well1="Low",well2="High")
  plot.heatmaps(pop=popn,well1="High",well2="High")
}

#Compare heatmaps for different subpopulations - identify significant differences
compare.heat <- function(pop1,pop2,well) {
  # pop1 <- "65 years and over"
  # pop2 <- "35 to 64 years"
  domheatmap.df1 <- wellbeing.df %>%
    dplyr::filter(Population==pop1 & Var1 %in% domain.names & Var2 %in% domain.names & Level1==well&Level2==well) %>% 
    mutate(ppt_dom=replace(ppt_dom,Var1==Var2,NA),se_ppt_dom=replace(se_ppt_dom,Var1==Var2,NA)) %>% 
    dplyr::rename(Pop1=Population,ppt_dom1=ppt_dom,se_ppt_dom1=se_ppt_dom) %>% 
    dplyr::select(Pop1,Var1,Var2,ppt_dom1,se_ppt_dom1)
  
  domheatmap.df2 <- wellbeing.df %>%
    dplyr::filter(Population==pop2 & Var1 %in% domain.names & Var2 %in% domain.names & Level1==well&Level2==well) %>% 
    mutate(ppt_dom=replace(ppt_dom,Var1==Var2,NA),se_ppt_dom=replace(se_ppt_dom,Var1==Var2,NA)) %>% 
    dplyr::rename(Pop2=Population,ppt_dom2=ppt_dom,se_ppt_dom2=se_ppt_dom) %>% 
    dplyr::select(Pop2,Var1,Var2,ppt_dom2,se_ppt_dom2)
  
  domheatcomp.sig <- full_join(domheatmap.df1,domheatmap.df2,by=c('Var1','Var2')) %>% 
    mutate(diff=ppt_dom1-ppt_dom2) %>% 
    filter(abs(diff)>(se_ppt_dom1+se_ppt_dom2)) %>% 
    dplyr::select(Pop1,Pop2,Var1,Var2,diff)
  
  print(domheatcomp.sig)
}

maori_eth<-paste0("M",sprintf('\u0101'),"ori ethnicity")
compare.heat('Male','Female','Low')
compare.heat('Male','Female','High')
compare.heat(maori_eth,'European ethnicity','Low')
compare.heat(maori_eth,'European ethnicity','High')
compare.heat('Pacific ethnicity','European ethnicity','Low')
compare.heat('Pacific ethnicity','European ethnicity','High')
compare.heat('Asian ethnicity','European ethnicity','Low')
compare.heat('Asian ethnicity','European ethnicity','High')
compare.heat('65 years and over','35 to 64 years','Low')
compare.heat('65 years and over','35 to 64 years','High')
compare.heat('15 to 34 years','35 to 64 years','Low')
compare.heat('15 to 34 years','35 to 64 years','High')


##########################################################################
## Now the subdomain analysis
##########################################################################

##############################################################################
## Firstly just a descriptive stacked bar of subdomain wellbeing
##############################################################################

theme.bar <- theme_bw() + 
  theme(axis.title=element_text(size=8),
        legend.title = element_text(size=8), 
        legend.text = element_text(size=6),
        plot.caption = element_text(size=6),
        axis.text = element_text(size=8),
        legend.position = "right",
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank()) # no grid is element_blank()

plot.subdomain.dist <- function(pop) {
  ## Subset the data
  #pop="ALL"
  allsubdomain.df <- wellbeing2.df %>%
    dplyr::filter(Population==pop & Var1=="Total" & Var2 %in% subdomain.names) %>% 
    group_by(Var2,Level2) %>% 
    dplyr::summarise(pct_subtot = sum(pct_subtot))
  
  allsubdomain.df$Var2 <- droplevels(allsubdomain.df$Var2)
  allsubdomain.df$Level2 <- factor(allsubdomain.df$Level2,levels=c("High","Medium","Low"))
  
  (subdomain.plot <- ggplot(allsubdomain.df) +
      aes(x = Var2, y = pct_subtot, fill=Level2, label=round(pct_subtot,0))+
      geom_bar(stat="identity",position="stack",width=0.5) +
      geom_text(size =2, position = position_stack(vjust = 0.5),colour="grey20")+
      geom_vline(xintercept=c(3.5,6.5,8.5,12.5,14.5),lwd=0.4, colour="black")+
      coord_flip()+
      ylab("Population (%)")+
      xlab("Subdomain")+
      scale_y_continuous(label=comma,expand = c(0, 0),breaks=c(0,20,40,60,80,100))+
      scale_x_discrete(limits = rev(levels(allsubdomain.df$Var2)))+
      labs(fill="Wellbeing",caption="Source: New Zealand General Social Survey 2014/2016")+
      scale_fill_manual(values=c(teal,green,lt_yellow))+
      theme.bar+
      guides(fill = guide_legend(reverse = TRUE)))
  
  pop2 <- chartr(",()<>","   LG",pop)
  plotname<-paste(dir.visualisations,"/subdomain plot - ",pop2,".png",sep="")
  ggsave(plotname, plot=subdomain.plot, width = 16, height = 12, units = "cm")
}

## Run for every population subgroup
for(pop in populations2){
  plot.subdomain.dist(pop)
}


##############################################################################
## Now dodged bars showing subdomain percentage point differences 
##############################################################################
subgroup2.df.ppt <- wellbeing2.df %>%
  dplyr::filter(Population != "ALL" & Var2 %in% subdomain.names & Var1=="Total" &Level2!="Medium" & !(Population %in% c("15 to 24 years", "80+ years"))) %>% 
  dplyr::select(PopGrp,Population,Var2,Level2,ppt_diff,se_ppt_diff) %>% 
  mutate(ppt_diff=replace(ppt_diff,Level2=="Rest of the population",0),se_ppt_diff=replace(se_ppt_diff,Level2=="Rest of the population",NA)) %>% 
  dplyr::rename(Wellbeing=Level2,Var1=Var2) %>% 
  arrange(Var1)

PopGrps <- unique(subgroup2.df.ppt$PopGrp)

for(grp in PopGrps) {
  # grp <- "Ethnic group"
  max <- 41
  min <- -41
  
  plot.df <- subset(subgroup2.df.ppt,PopGrp==grp) #%>% 
  #mutate(Population=paste(Population,"-",Wellbeing,"wellbeing"))
  plot.df$Population <- gsub("\\(", "\n\\(",plot.df$Population)
  plot.df$Population = factor(plot.df$Population,unique(plot.df$Population))
  
  subdomainpop.ppt.plotb <- ggplot(plot.df)+
    aes(x = Var1, y = ppt_diff, fill=Wellbeing)+
    geom_bar(stat="identity",position=position_dodge(),width=0.8) +
    geom_errorbar(aes(ymin=ppt_diff-se_ppt_diff, ymax=ppt_diff+se_ppt_diff),position=position_dodge(),width=0.8,size=0.3) +     
    coord_flip()+
    geom_vline(xintercept=c(3.5,6.5,8.5,12.5,14.5),lwd=0.4, colour="grey70")+
    geom_hline(yintercept=0,lwd=0.3, colour="black")+
    ylab(paste("Percentage point difference in wellbeing compared to the rest of the population"))+
    xlab("Population group")+
    labs(caption="Source: New Zealand General Social Survey 2014/2016\nError bars show 95% confidence interval.")+
    theme.bar+
    theme(legend.position = "right")+ 
    guides(fill = guide_legend(reverse = FALSE))+
    scale_fill_manual(values=c(teal,lt_yellow))+
    scale_x_discrete(limits = rev(levels(droplevels(plot.df$Var1))))+
    scale_y_continuous(limits=c(min,max),breaks=c(-40,-20,0,20,40))+
    scale_colour_identity()
  
  # (subdomainpop.ppt.plotb2<-subdomainpop.ppt.plotb+facet_grid(rows=vars(Population))+
  #     theme(strip.text.x = element_text(size = 6),strip.text.y = element_text(size = 6)))

  (subdomainpop.ppt.plotb2<-subdomainpop.ppt.plotb+facet_wrap(~ Population, ncol=2))
    
  ht <- ceiling(length(unique(as.array(plot.df$Population)))/2)*7+1.5
  plotname<-paste(dir.visualisations,"/Subdomain Ppt differences dodged bars - ",grp,".png",sep="")
  ggsave(plotname,plot=subdomainpop.ppt.plotb2, width = 15.5, height = ht, units = "cm")
}


##############################################################################
## Now heatmaps of associations between good and poor wellbeing domains and sub-domains
##############################################################################
plot.heatmaps <- function(pop,well1,well2) {
  # pop<-"ALL"
  # well1<-"Low"
  # well2<-"Low"
  
  subdomheatmap.df <- wellbeing2.df %>%
    dplyr::filter(Population==pop & Var1 %in% domain.names2 & Var2 %in% subdomain.names & Level1==well1&Level2==well2) %>% 
    mutate(ppt_dom=replace(ppt_dom,Var1==Var2,NA),se_ppt_dom=replace(se_ppt_dom,Var1==Var2,NA)) %>% 
    mutate(sig=ifelse((ppt_dom<0 & (ppt_dom+se_ppt_dom)<0)|(ppt_dom>0 & (ppt_dom-se_ppt_dom)>0),"*","")) %>% 
    mutate(label=paste0(signif(ppt_dom, 2),sig)) %>% 
    mutate(label=replace(label,label=="NANA","")) %>% 
    dplyr::select(Var1,Var2,ppt_dom,se_ppt_dom,sig,label)
  
  subdomheatmap.df$Var1 <- factor(subdomheatmap.df$Var1,levels=rev(domain.names2))
  subdomheatmap.df$Var1 <- droplevels(subdomheatmap.df$Var1)
  subdomheatmap.df$Var2 <- factor(subdomheatmap.df$Var2,levels=subdomain.names)
  subdomheatmap.df$Var2 <- droplevels(subdomheatmap.df$Var2)
  
  text_size<-7
  xl <- paste("Subdomain",well2,"wellbeing")
  yl <- paste("Domain",well1,"wellbeing")
  
  if (well1=="Low" & well2=="Low") {
    lowcol <- teal
    hicol <- yellow
  } else {
    lowcol <- yellow
    hicol <- teal
  }
  
  (p <- ggplot(subdomheatmap.df) 
    + geom_tile(aes(x=formatC(Var2, width=2, flag="0"),y=formatC(Var1, width=2, flag="0"),fill = ppt_dom),colour = "white")
    + geom_text(size=2,aes(x=formatC(Var2, width=2, flag="0"),y=formatC(Var1, width=2, flag="0"),label = label))
    + scale_fill_gradient2(na.value=lt_charcoal,low = lowcol,mid="white",high = hicol,midpoint=0)
    + theme_grey(base_size = text_size) 
    + labs(x = xl,y = yl,fill="Percentage point difference",title=element_blank(),
           caption="Source: New Zealand General Social Survey 2014/2016\n* indicates statistically significant differences from 0 at the 95% level") 
    + scale_x_discrete(position="top",expand = c(0, 0),labels=subdomain.names) 
    + scale_y_discrete(expand = c(0, 0),labels=rev(domain.names2))
    + theme(legend.position="none",axis.ticks=element_blank(),axis.text.x=element_text(angle = 45, hjust = 0),
            plot.margin = unit(c(0.2,1,0.2,0.2), "cm"))
  )
  
  pop2 <- chartr(",()<>","   LG",pop)
  
  plotname<-paste(dir.visualisations,"/Subdomain heatmap - ",pop2,well1,well2,".png",sep="")
  ggsave(plotname,plot=p, width = 15.5, height = 12.5, units = "cm")
}

for(popn in populations2){
  plot.heatmaps(pop=popn,well1="Low",well2="Low")
  #plot.heatmaps(pop=popn,well1="Low",well2="High")
  #plot.heatmaps(pop=popn,well1="High",well2="Low")
  plot.heatmaps(pop=popn,well1="High",well2="High")
}


#Compare heatmaps for different subpopulations
compare.heat <- function(pop1,pop2) {
  # pop1 <- "Male"
  # pop2 <- "Female"
  subdomheatmap.df1 <- wellbeing2.df %>%
    dplyr::filter(Population==pop1 & Var1 %in% domain.names2 & Var2 %in% subdomain.names & Level1=="Low"&Level2=="Low") %>% 
    mutate(ppt_dom=replace(ppt_dom,Var1==Var2,NA),se_ppt_dom=replace(se_ppt_dom,Var1==Var2,NA)) %>% 
    dplyr::rename(Pop1=Population,ppt_dom1=ppt_dom,se_ppt_dom1=se_ppt_dom) %>% 
    dplyr::select(Pop1,Var1,Var2,ppt_dom1,se_ppt_dom1)
  
  subdomheatmap.df2 <- wellbeing2.df %>%
    dplyr::filter(Population==pop2 & Var1 %in% domain.names2 & Var2 %in% subdomain.names & Level1=="Low"&Level2=="Low") %>% 
    mutate(ppt_dom=replace(ppt_dom,Var1==Var2,NA),se_ppt_dom=replace(se_ppt_dom,Var1==Var2,NA)) %>% 
    dplyr::rename(Pop2=Population,ppt_dom_2=ppt_dom,se_ppt_dom_2=se_ppt_dom) %>% 
    dplyr::select(Pop2,Var1,Var2,ppt_dom_2,se_ppt_dom_2)
  
  subdomheatcomp.sig <- full_join(subdomheatmap.df1,subdomheatmap.df2,by=c('Var1','Var2')) %>% 
    mutate(diff=ppt_dom1-ppt_dom_2) %>% 
    filter(abs(diff)>(se_ppt_dom1+se_ppt_dom_2)) %>% 
    dplyr::select(Pop1,Pop2,Var1,Var2,diff)
  
  subdomheatcomp.sig
}
maori_eth<-paste0("M",sprintf('\u0101'),"ori ethnicity")
compare.heat('Male','Female')
compare.heat(maori_eth,'European ethnicity')
compare.heat('Pacific ethnicity','European ethnicity')
compare.heat('Asian ethnicity','European ethnicity')
compare.heat('65+ years','35 to 64 years')
compare.heat('15 to 34 years','35 to 64 years')



#############################################################################
# Now child-weighted analysis
##############################################################################

## First a stacked bar chart of the high level distribution of 
## wellbeing groupings across domains
##############################################################################

pop <- "ALL"
alldomain.df <- wellbeing.df %>%
  dplyr::filter(Population==pop & Var2=="Total" & Var1 %in% domain.names2)

alldomainchild.df <- wellbeingchild.df %>%
  dplyr::filter(Population==pop & Var1 %in% domain.names2) %>% 
  dplyr::select(Var1,Level1,pct_subtot) %>% 
  dplyr::rename(pct_subtotchild=pct_subtot) %>% 
  left_join(alldomain.df,by=c('Var1','Level1')) %>% 
  mutate(ppt_diff=pct_subtotchild-pct_subtot)

alldomainchild.df$Var1 <- droplevels(alldomainchild.df$Var1)
alldomainchild.df$Level1 <- factor(alldomainchild.df$Level1,levels=c("High","Medium","Low"))

  (domain.plot <- ggplot(alldomainchild.df) +
      aes(x = Var1, y = pct_subtot, fill=Level1, label=round(pct_subtot,0))+
      geom_bar(stat="identity",position="stack",width=0.6) +
      geom_text(size =2.5, position = position_stack(vjust = 0.5),colour="grey20")+
      geom_vline(xintercept=c(8.5),lwd=0.4, colour="black")+
      coord_flip()+
      ylab("Population (%)")+
      xlab("Domain")+
      scale_y_continuous(label=comma,expand = c(0, 0),breaks=c(0,20,40,60,80,100))+
      scale_x_discrete(limits = rev(levels(alldomainchild.df$Var1)))+
      labs(fill="Wellbeing",caption="Source: New Zealand General Social Survey 2014/2016")+
      scale_fill_manual(values=c(teal,green,lt_yellow))+
      theme.bar+
      guides(fill = guide_legend(reverse = TRUE)))
  
  pop2 <- chartr(",()<>","   LG",pop)
  plotname<-paste(dir.visualisations,"/Domain plot CHILD - ",pop2,".png",sep="")
  ggsave(plotname, plot=domain.plot, width = 16, height = 9, units = "cm")

  max <- 23
  min <- -23
  grp<-"Children"
  plot.df<-alldomainchild.df %>% 
    dplyr::filter(Level1!="Medium")
  plot.df$Wellbeing<-plot.df$Level1
  
  domainpop.ppt.plotb <- ggplot(plot.df)+
    aes(x = Var1, y = ppt_diff, fill=Level1)+
    geom_bar(stat="identity",position=position_dodge(),width=0.75) +
    coord_flip()+
    geom_hline(yintercept=0,lwd=0.3, colour="black")+
    ylab(paste("% point difference in wellbeing of the child-weighted population compared to all NZers aged 15+"))+
    xlab("Population group")+
    labs(caption="Source: New Zealand General Social Survey 2014/2016")+
    theme.bar+
    theme(legend.position = "right")+ 
    guides(fill = guide_legend(reverse = FALSE))+
    scale_fill_manual(values=c(teal,lt_yellow))+
    scale_x_discrete(limits = rev(levels(droplevels(plot.df$Var1))))+
    scale_y_continuous(limits=c(min,max),breaks=c(-40,-20,0,20,40))+
    scale_colour_identity()
  
  plotname<-paste(dir.visualisations,"/Ppt contribution plot children compared to adults.png",sep="")
  ggsave(plotname,plot=domainpop.ppt.plotb, width = 15.5, height = 5.5, units = "cm")
  
  
  multidomainall.df <- wellbeing.df %>%
    dplyr::filter(PopGrp=="Total population" & Var2=="Total" & Var1=="Cross-domain wellbeing groups")%>% 
    mutate(Population="Adult population") %>% 
    dplyr::select(Population,pct_subtot,Level1)
  multidomainallchild.df <- wellbeingchild.df %>%
    dplyr::filter(PopGrp=="Total population" & Var1=="Cross-domain wellbeing groups") %>% 
    mutate(Population="Child weighted") %>% 
    dplyr::select(Population,pct_subtot,Level1)
  multidomainall.df<-rbind(multidomainall.df,multidomainallchild.df)
  
  multidomainall.df$Level1 <- factor(multidomainall.df$Level1,levels=c("Very high","High","Medium","Low","Very low"))
  
  (multidomainall.plot <- ggplot(multidomainall.df) +
      aes(x = Population, y = pct_subtot, fill=Level1, label=round(pct_subtot,0))+
      geom_bar(stat="identity",position="stack",width=0.6) +
      geom_text(size =2.5, position = position_stack(vjust = 0.5),aes(colour=Level1))+
      scale_colour_manual(values=c("white","grey20","grey20","grey20","white"))+
      coord_flip()+
      ylab("Population (%)")+
      xlab("Population")+
      scale_y_continuous(label=comma,expand = c(0, 0),breaks=c(0,20,40,60,80,100))+
      scale_x_discrete(limits = rev(unique(multidomainall.df$Population)))+
      labs(fill="Cross-domain wellbeing",caption="Source: New Zealand General Social Survey 2014/2016")+
      scale_fill_manual(values=c(dk_teal,teal,green,lt_yellow,yellow))+
      theme.bar+
      guides(fill = guide_legend(reverse = TRUE),colour=FALSE))
  
  plotname<-paste(dir.visualisations,"/Cross-domain plot CHILD.png",sep="")
  ht <- length(unique(multidomainall.df$Population))*1.4+2
  ggsave(plotname, plot=multidomainall.plot, width = 16, height = ht, units = "cm")