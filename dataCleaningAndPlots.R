library(tidyverse)
library(ggthemes)
library(magrittr)
library(scales)

# read in survey data for first section of report 

dat <- read_csv("rawData/GSS Stipend Survey.csv")


# pull out rent data

rentDat <-  dat[,12]
colnames(rentDat) <- "rent"

# clean data (Just want numeric values, no letters dollar signs or whitespace)

rentDat$rent <- str_remove_all(rentDat$rent, "\\$") %>%
  str_remove("/") %>%
  str_remove_all("[[:blank:]]{1,}") %>%
  str_remove_all("[[:alpha:]]{1,}") %>%
  str_remove("^.$") 

# someone gave 2 numbers for some reason
rentDat[379,1] <- "850"

# can reclassify rent as numeric now 

rentDat %<>%
  mutate(rent=as.numeric(rent))

##### figure 3 ####

ggplot(rentDat, aes(x= rent)) +
  stat_density(size=.4, trim=TRUE, alpha=0.5, fill= "#00991AFF", color="black") +
  labs(title = "Distribution of Grad Student Monthly Rent", x= "Rent Paid Each Month ($)", y= "Density") +
  theme_get() +
  #scale_x_continuous(breaks = seq(0, 2600, by = 300)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_vline(xintercept = median(rentDat$rent, na.rm = T), linetype = "longdash", color="purple")


#### figure 4 ####

dat[,12] <- rentDat$rent
names(dat)[12] <- "rent"

# need median and standard deviation of rent for students on each class of stipend ( 9mo PhD 12mo PhD, 9mo Masters and 12mo Masters)

rentDat <- dat %>%
  filter(!is.na(`Is your stipend...` )) %>%
  group_by(`Is your stipend...`,`Which of the following best describes your current status?`) %>%
  summarize(medianRent=median(rent, na.rm=T), n=n(), sdReant=sd(rent, na.rm = T)) %>%
  ungroup()

# pull out data for plot and give variables shorter names
pdat <- rentDat[1:4,]
names(pdat) <- c("StipendTimeframe", "Degree", "medianRent", "sampleSize", "sdRent")

# clean up other columns similar to above 

pdat %<>%
  mutate(StipendTimeframe=str_remove(StipendTimeframe,"stipend.")) %>%
  mutate(StipendTimeframe=str_remove(StipendTimeframe,"[[:space:]]$")) %>%
  mutate(Degree=str_replace(Degree,"student","")) %>%
  mutate(Degree=str_remove(Degree,"[[:space:]]$"))  %>%
  mutate(minStipend=c(27377,22233,20525,16675)) %>%
  mutate(rentRatio=medianRent/(minStipend/12)) %>%
  mutate(rentRatioWfee=medianRent/((minStipend-2346)/12)) # here we account for the fee (aka net salary)


ggplot(pdat, aes(x=StipendTimeframe, y=rentRatio, fill=Degree)) +
  geom_point(shape = 21, color = "black", size = 5.5, stroke = 1.5, alpha= .85) +
  geom_hline(aes(yintercept=.35,  color= "Recommended Maximum (35%)"), linetype='dashed', size=.7) +
  scale_color_manual(values="red") +
  scale_fill_manual(values=c("#00991AFF", "#FFD320FF")) +
  labs(y="Median Rent / Minimum Stipend", x= "Stipend Timeframe", color= "", title= "Fraction of Stipend Dedicated to Rent") +
  theme_get() 

# same plot but with effect of fee taken into account

ggplot(pdat, aes(x=StipendTimeframe, y=rentRatioWfee, fill=Degree)) +
  geom_point(shape = 21, color = "black", size = 5.5, stroke = 1.5, alpha= .85) +
  geom_hline(aes(yintercept=.35,  color= "Recommended Maximum (35%)"), linetype='dashed', size=.7) +
  scale_color_manual(values="red") +
  scale_fill_manual(values=c("#00991AFF", "#FFD320FF")) +
  labs(y="Median Rent / Minimum Stipend", x= "Stipend Timeframe", color= "", title= "Fraction of Stipend Dedicated to Rent", subtitle = "When comprehensive fee is taken into account") +
  theme_get() 


#### Analysis of phdstipends.com dataset ####

# data downloaded 12/2/20 from http://www.phdstipends.com/csv 
# approach to data cleaning was guided by: https://www.kaggle.com/wguesdon/ph-d-stipends-by-research-topic-and-universities


kaggleDat <- read_csv("rawData/StipendDat.csv") %>% 
  rename(Overall_Pay = `Overall Pay`, 
         LW_Ratio = `LW Ratio`,
         Academic_Year = `Academic Year`,
         Program_Year = `Program Year`,
         Gross_Pay_12M = `12 M Gross Pay`,
         Gross_Pay_9M = `9 M Gross Pay`,
         Gross_Pay_3M = `3 M Gross Pay`
  )

# Pull out data where 12 month PhD stipend is given, remove missing values, remove clear outliers, merge similar department/subject names

stipend12mo <- kaggleDat  %>%
  filter(!is.na(Gross_Pay_12M)) %>% # make sure we're talking 12 month stipend
  filter(Gross_Pay_12M != "n/a") %>%
  select(University, Department, Overall_Pay, LW_Ratio, Academic_Year, Program_Year) %>%
  filter(!is.na(LW_Ratio)) %>% # important to use living wage ratio, so need to make sure it's not missing 
  filter(LW_Ratio != "n/a") %>%
  mutate(Overall_Pay=str_replace_all(Overall_Pay, "[[:punct:]]", "")) %>%
  mutate(Overall_Pay=as.numeric(str_replace_all(Overall_Pay, "[[$]]", ""))) %>% 
  filter(LW_Ratio>.57 & LW_Ratio <1.58) %>% # remove 5% outliers quantile(stipend12mo$LW_Ratio,probs = seq(.05,1,.05))
  mutate(Department_clean= tolower(Department)) %>%  # make all text lowercase
  mutate(Department_clean=str_replace(Department_clean,".{0,}biology.{0,}", "biology")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}genetics.{0,}", "biology")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}cell.{0,}", "biology")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}neuro.{0,}", "neuro")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}aero.{0,}", "aerospace")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}space.{0,}", "aerospace")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}health.{0,}", "health")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}pharma{0,}", "pharmacy")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}chemistry.{0,}", "chemistry")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}computer.{0,}", "computer science")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}psych.{0,}", "psychology")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}communication.{0,}", "communication")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}physics.{0,}", "physics")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}agri.{0,}", "agriculture")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}plant.{0,}", "agriculture")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}crop.{0,}", "agriculture")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}soil.{0,}", "agriculture")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}business.{0,}", "business")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}crimin.{0,}", "criminology")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}earth.{0,}", "geoscience")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}geo.{0,}", "geoscience")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}english.{0,}", "english"))  %>%
  distinct() 

#write_csv(stipend12mo, "cleanData/StipendData_clean.csv")

# check distribution of values

quantile(stipend12mo$LW_Ratio,probs = seq(.05,1,.05)) # wow


# to compare this data to UVM's average living wage ratio, we took the weighted mean of UVM 12 month stipends based on the reported number of students in each stipend class and divided that mean by Burlington living wage according to https://livingwage.mit.edu/. This calculation came out to a living wage ratio of .986

#!#!#!#!#!# note that we cannot share the raw data behind the calculation of this weighted mean, as it is data from a national survey that is under copyright. This data was shared with our committee by UVM Graduate College under the condition that we respect the copyright.


uvm_lwRatioAVG <-  .986

stipend12moAVG <- stipend12mo %>%
  group_by(University) %>%
  summarize(avgLW_Ratio= mean(LW_Ratio, na.rm = T), N=n()) %>%
  filter(N>1) %>%
  filter(University != "University of Vermont (UVM)")

#### figure 6 ####

# find quantile breaks for the plot 

qnt <- quantile(stipend12moAVG$avgLW_Ratio, probs = c(.25,.5,.75) )


ggplot(stipend12moAVG, aes(x=University, y=avgLW_Ratio)) +
  geom_point(size=2, shape=21) +
  coord_flip() +
  annotate("point", y=uvm_lwRatioAVG, x="UVM", color= "#00991AFF", size =2.5) +
  geom_hline(yintercept = qnt[1], color = "purple") +
  geom_hline(yintercept = qnt[2], color = "purple") +
  geom_hline(yintercept = qnt[3], color = "purple") +
  annotate("text", x= "West Virginia University (WVU)", y= qnt[1] + .025, color= "purple", size =3, label="25%") + # I use WVU to position the quantile lables becasue it's near the top of the y axis
  annotate("text", x= "West Virginia University (WVU)", y= qnt[2] + .025, color= "purple", size =3, label="50%") +
  annotate("text", x= "West Virginia University (WVU)", y= qnt[3] + .025, color= "purple", size =3, label="75%") +
  labs(x="University", y= "Average Stipend to Living Wage Ratio", title = "Stipend to Living Wage Ratios for 229 Universities", subtitle = "UVM average for 12 month PhD = .986 (green point). Purple lines are percentiles.", caption = "Data sources: phdstipends.com, livingwage.mit.edu") +
  theme_get() +
  theme(axis.text.y = element_text(size =3.5)) +
  theme(plot.caption= element_text(size = 8)) +
  theme(plot.caption.position = "plot")


#### analysis of fees ####

# only a subset of the phdstipends.com data entries actually list the comprehansive fee, but we can subset that data to look at as well, cleaning the same as above

#!#!#! note that we don't end up using this fee data becasue it seems to be a mix of per-semester and per-year fees (see below)

stipendFee <- kaggleDat  %>%
  filter(!is.na(Fees)) %>% # only want data where fee is reported
  filter(Fees != "n/a") %>%
  mutate(Fees= str_remove(Fees, "\\$")) %>%
  mutate(Fees= str_remove(Fees, ",")) %>%
  mutate(Fees= as.numeric(Fees)) %>%
  filter(!is.na(Fees)) %>% # check na again
  filter(!is.nan(Fees)) %>%
  select(University, Department, Overall_Pay, LW_Ratio, Academic_Year, Program_Year, Fees) %>%
  filter(!is.na(LW_Ratio)) %>% # important to use living wage ratio, so need to make sure it's not missing 
  filter(LW_Ratio != "n/a") %>%
  mutate(Overall_Pay=str_replace_all(Overall_Pay, "[[:punct:]]", "")) %>%
  mutate(Overall_Pay=as.numeric(str_replace_all(Overall_Pay, "[[$]]", ""))) %>%
  filter(LW_Ratio>.57 & LW_Ratio <1.58) %>% # remove 5% outliers quantile(stipend12mo$LW_Ratio,probs = seq(.05,1,.05))
  filter(Fees>90 & Fees <4000) %>% # quantile(stipendFee$Fees,probs = seq(.05,1,.05), na.rm = T)
  mutate(Department_clean= tolower(Department)) %>%  # make all text lowercase
  mutate(Department_clean=str_replace(Department_clean,".{0,}biol.{0,}", "biology")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}genetics.{0,}", "biology")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}cell.{0,}", "biology")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}neuro.{0,}", "neuro")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}aero.{0,}", "aerospace")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}space.{0,}", "aerospace")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}health.{0,}", "health")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}pharma{0,}", "pharmacy")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}chemistry.{0,}", "chemistry")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}computer.{0,}", "computer science")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}psych.{0,}", "psychology")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}communication.{0,}", "communication")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}physics.{0,}", "physics")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}agri.{0,}", "agriculture")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}plant.{0,}", "agriculture")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}crop.{0,}", "agriculture")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}soil.{0,}", "agriculture")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}business.{0,}", "business")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}crimin.{0,}", "criminology")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}earth.{0,}", "geoscience")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}geo.{0,}", "geoscience")) %>%
  mutate(Department_clean=str_replace(Department_clean,".{0,}english.{0,}", "english")) %>%
  distinct()

# check distribution of fees

quantile(stipendFee$Fees,probs = seq(.05,1,.05), na.rm = T) # seems like a mix of per-semester and yearly fees

feeAVG <- stipendFee %>%
  group_by(University) %>%
  summarize(avgFee= mean(Fees, na.rm = T), N=n()) %>%
  filter(N>1)

filter(feeAVG, University== "University of Vermont (UVM)") # based on this data for UVM, it looks like this is a mix of per year and per semester fees, which doesn't really work for comparison


# Instead, we use a dataset of per-semester fees provided to us by the UVM graduate college for this part of the analysis.
# note that this dataset includes the universities that UVM generally uses as "comparison schools"

feeDat <- read_csv("cleanData/fees.csv") %>%
  mutate(UVM= ifelse(School=="University of Vermont", "yes", "no"))

#### figure 9 ####

uvmFee <- 1173

meanFee <- mean(feeDat$semester_fee, na.rm = T)

fee_qnt <- quantile(feeDat$semester_fee, probs = c(.25,.5,.75), na.rm = T )


ggplot(feeDat, aes(y=semester_fee, x=School, color=UVM)) +
  geom_point(size=3) +
  coord_flip() +
  geom_hline(yintercept = fee_qnt[1], color = "purple") +
  geom_hline(yintercept = fee_qnt[2], color = "purple") +
  geom_hline(yintercept = fee_qnt[3], color = "purple") +
  annotate("text", x= "Wayne State U", y= fee_qnt[1] - 49, color= "purple", size =3.5, label="25%") +
  annotate("text", x= "Wayne State U", y= fee_qnt[2] - 49, color= "purple", size =3.5, label="50%") +
  annotate("text", x= "Wayne State U", y= fee_qnt[3] - 49, color= "purple", size =3.5, label="75%") +
  scale_color_manual(values = c("black","#00991AFF")) +
  labs(y= "Per-Semester Fee ($)", x= "University", title = "UVM Comprehensive Fee Compared to Other Universities", subtitle = "UVM Fee = $1173 (green point). Purple lines are percentiles.", caption = "Data sorce: UVM Graduate College, OSU IRIM") +
  theme_get() +
  theme(axis.text.y = element_text(size = 4.9)) +
  theme(plot.subtitle = element_text(size = 9.5)) +
  theme(plot.caption= element_text(size = 8)) +
  theme(plot.caption.position = "plot") +
  theme(legend.position = "none")
