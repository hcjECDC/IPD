rm(list=ls())
dir <- "P:/Pneumo"
setwd(dir)
require(plyr)
require(tidyverse)
require(lubridate)
require(reshape2)
require(meta)
## Read in data
data1 <- read_csv("180319IPD.csv", col_names=TRUE)
data1$Age <- as.integer(data1$Age)

pop1 <- read_csv("TESSy population.csv", col_names=TRUE)   ## Read in TESSy population data
pop2 <- read_csv("EUROSTATpop.csv", col_names=TRUE)   ## Read in EUROSTAT population data
scotPop <- read_csv("ScotPop.csv", col_names=TRUE)   ## Read in Scottish population data
## Source: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates
PCVdates <- read_csv("VaccineDates.csv", col_names=TRUE)   ## Read in dates of PCV introduction

EEA <- c("AT","BE","BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "EL", "HU", "IS", "IE", "IT", "LV", "LI", "LT", "LU", "MT", "NL", "NO", "PL", "PT", "RO", "SK", "SI", "ES", "SC", "SE", "UK") 
countries <- as.table(matrix(c(EEA, "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Liechtenstein", "Lithuania","Luxembourg", "Malta", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Scotland", "Sweden", "United Kingdom" ), ncol=2))


data2 <- data1 %>% 
  mutate(GroupType = recode(Serotype, "4"  = "PCV7",               ## Group serotypes according to vaccine (need to group others)
                                       "6B" = "PCV7",
									   "9V" = "PCV7",
									   "14"  = "PCV7",
                                       "18C" = "PCV7",
									   "19F" = "PCV7",
									   "23F" = "PCV7",
                                       "1" = "PCV10",
									   "5" = "PCV10",
									   "7F"  = "PCV10",
                                       "3" = "PCV13",
									   "6A" = "PCV13",
									   "19A" = "PCV13",
									   "2"  = "PPV23",
                                       "8" = "PPV23",
									   "9N" = "PPV23",
									   "10A" = "PPV23",
                                       "11A" = "PPV23",
									   "12F" = "PPV23",
									   "15B"  = "PPV23",
                                       "17F" = "PPV23",
									   "20" = "PPV23",
									   "22F" = "PPV23",		   
									   "33F" = "PPV23",
									   "NT" = "UNK",            ## Check what these three terms mean. For now, include in "UNK"
									   "NTYP" = "UNK",		   
									   "O" = "UNK")) 
		
## Add dates of PCV introduction 
data2 <- data2 %>% left_join(PCVdates) 

## Group other types
data2 <- data2 %>% mutate(GroupType = case_when(GroupType != "PCV7" & GroupType != "PCV10" & GroupType != "PCV13" & GroupType != "PPV23" & GroupType != "NULL" & GroupType != "UNK" ~ "Other", TRUE ~ as.character(GroupType)))

## Change date formatting
data2$Date <- as.Date(data2$DateUsedForStatisticsISO, "%d/%m/%Y")  # convert to date format
data2 <- data2 %>% mutate(Date = case_when(nchar(DateUsedForStatisticsISO)==7  ~ paste(DateUsedForStatisticsISO,"-01",sep=""), TRUE ~ as.character(Date)))   ## Where no day specified, set to 1st.
data2 <- data2 %>% mutate(Date = case_when(nchar(DateUsedForStatisticsISO)==4 ~ paste(DateUsedForStatisticsISO,"-01-01",sep=""), TRUE ~ as.character(Date)))   ## Where no day or month specified, set to 1st of the 1st.
data2$DateUsedForStatisticsISO <- NULL  # remove old variable
							
	
data2$Date <- ymd(data2$Date)  # convert to date format
data2$StartPCV1013 <- as.Date(data2$StartPCV1013, "%d/%m/%Y") # convert to date format
data2$StartPCV7 <- as.Date(data2$StartPCV7, "%d/%m/%Y") # convert to date format

## Code time periods
data2 <- data2 %>% 
  mutate(TimePeriod = case_when(Date < StartSpID1 ~ 1,								## Before the inclusion period of SpIDnet1
								Date > StartSpID1   & Date < StartPCV7 ~ 2,			## SpIDnet pre-PCV7 period
								Date > StartPCV7    & Date < StartPCV1013 ~ 3,		## PCV7 period
								Date > StartPCV1013 & Date < EndDateSpID1 ~ 4,		## PCV10/13 period
								Date > EndDateSpID1 ~ 5))							## After inclusion in SpIDnet1

 ## Code year following intro of PCV10/13
 data2 <- data2 %>% 
    mutate(PCV1013Yr  = if_else(TimePeriod == 4 | TimePeriod==5, pmax(ceiling((Date - StartPCV1013)/365.25),1), NA_real_))  ## Need max() since some records in Spain and France are classified as the exact date of PCV1013 intro
	
## Code age groups
data2 <- data2 %>% 
  mutate(AgeGroup = case_when(Age < 5 ~ 1,					## 0-4
							  Age >= 5  & Age < 15 ~ 2,		## 5-14
							  Age >= 15 & Age < 65 ~ 3,		## 15-64
							  Age >= 65 & Age < 85 ~ 4,		## 65-84
							  Age >= 85 ~ 5))				## 85+
							  
## Define year for matching
data2 <- data2 %>% mutate(Year = substr(Date,1,4))

## Include only confirmed cases
data2 <- data2[which(data2$Classification == "CONF"), ]  # confirmed cases for chosen country 

## Could speed this up by just doing once for each country and the ungrouping again.
## No. of years pre-PCV7 (needs to be calculated from first case in country although should properly be the start of the surveillance period)
firstCase <- data2 %>%     
    dplyr::group_by(ReportingCountry) %>%
    slice(which.min(Date))
	
firstCase <- firstCase %>% select(ReportingCountry, Date)
firstCase <- rename(firstCase, firstDate = Date)	
data2 <- data2 %>% left_join(firstCase)
data2 <- data2  %>% mutate(prePCV7Yrs = (StartPCV7 - firstDate)/365.25)	## need to select countries according to those that have positive value here
data2$prePCV7Yrs <- pmax(as.numeric(data2$prePCV7Yrs), 0)

## No. of years of PCV7
data2 <- data2  %>%  mutate(PCV7Yrs = (StartPCV1013 - StartPCV7)/365.25)	## need to select countries according to those that have positive value here					
data2$PCV7Yrs <- pmax(as.numeric(data2$PCV7Yrs), 0)

###############################
## Appear to have a record for Spain from 2020!!
lastCase <- data2 %>%         
    dplyr::group_by(ReportingCountry) %>%
    slice(which.max(Date))
	
lastCase <- lastCase %>% select(ReportingCountry, Date)
lastCase <- rename(lastCase, lastDate = Date)	
data2 <- data2 %>% left_join(lastCase)

data2 <- data2  %>%  mutate(PCV1013Yrs = (lastDate - StartPCV1013)/365.25)	## need to select countries according to those that have positive value here					
data2$PCV1013Yrs <- pmax(as.numeric(data2$PCV1013Yrs), 0)
	
####################################################
## Derive population estimates by country and age group
## TESSy population data
pop1 <- filter(pop1, ReportingCountry %in% EEA)     ## TESSy population data
pop1 <- filter(pop1, ReportYear >= year(min(na.omit(data2$Date))))

pop1 <- pop1 %>% 
  mutate(AgeGroup = case_when(AgeGroupId == 1  | AgeGroupId == 2  ~ 1,		    ## 0-4
							     AgeGroupId >= 3  & AgeGroupId <= 5  ~ 2,		## 5-14
							     AgeGroupId >= 6  & AgeGroupId <= 15 ~ 3,		## 15-64
							     AgeGroupId >= 16 & AgeGroupId <= 19 ~ 4,		## 65-84
							     AgeGroupId >= 20 & AgeGroupId <= 23 ~ 5))		## 85+

pop1 <- pop1 %>% filter(!is.na(AgeGroup))  ## Original population database included other ways of grouping ages. Drop these values								 
pop1 <- dplyr::rename(pop1, Year = ReportYear)   ## rename to match with data2

pop1 <- pop1 %>%            ## Group population by age groups
    dplyr::group_by(Year, ReportingCountry, AgeGroup) %>%
    dplyr::summarise(Population = sum(Population))
	
pop1$Year <- as.character(pop1$Year)

## Scottish population data (estimates only run to 2016 so assume 2017=2016 for now)
scotPop <- melt(scotPop, id.vars = "Year")
scotPop <- scotPop %>% arrange(Year)
scotPop <- dplyr::rename(scotPop, Population = value)    ## rename to match with pop1
scotPop <- dplyr::rename(scotPop, AgeGroup = variable)   ## rename to match with pop1
scotPop <- scotPop %>% mutate(ReportingCountry = as.character("SC"),
							  Year = as.character(Year),
							  AgeGroup = case_when(AgeGroup == "Gp1" ~ 1,	## 0-4
							  AgeGroup == "Gp2" ~ 2,	## 5-14
							  AgeGroup == "Gp3" ~ 3,	## 15-64
							  AgeGroup == "Gp4" ~ 4,	## 65-84
							  AgeGroup == "Gp5" ~ 5))	## 85+


## Calculate population of UK excluding Scotland
EWNI <- pop1 %>% filter(ReportingCountry == "UK")  ## Calculate population of UK - Scotland
EWNI %>% mutate(ReportingCountry = replace(ReportingCountry, ReportingCountry=="UK", "EWNI"))
EWNI$Population <- as.integer(EWNI$Population - scotPop$Population)

## Combine Scottish population with TESSy population data
pop1 <- bind_rows(pop1, scotPop)
#############################################################

## Classify known Scottish data as Scotland (UKM). Suspect that some Scottish cases have been classified loosely as "UK" and will therefore not be included
data2 <- data2 %>% mutate(ReportingCountry = case_when(GeoCode == "UKM" ~ "SC", TRUE ~ as.character(ReportingCountry)))
 
## Match each case with the appropriate population estimate for its country and age group
data2 <- data2 %>% left_join(pop1)  ## Don't really need this. Can group and then divide
data2 %>% arrange(ReportingCountry, Year, AgeGroup)

## Calculate no. of person years for each time period
## Should really find sum-product over era
data2 <- data2 %>% 
		mutate(PersonYears = case_when(TimePeriod == 1 | TimePeriod == 2 ~ prePCV7Yrs*Population,	## pre-PCV7 (including pre- and post-start of SpIDnet1 where appropriate				
									   TimePeriod == 3  ~ PCV7Yrs*Population,                       ## PCV7 era
									   TimePeriod == 4 | TimePeriod == 5 ~ as.numeric(Population)))	## PCV10/13 era (by single year)
#############
## Outputs ##
#############
chooseClinPres <- "SEPTI"
chooseAgeGroup <- 5

myColours <- c("26 107 133", "241 214 118", "168 45 23")
ECDCcol <- sapply(strsplit(myColours, " "), function(x)
    rgb(x[1], x[2], x[3], maxColorValue=255))  # convert to hexadecimal

colours1 <- colorRampPalette(ECDCcol)(11)


## Describe data: mean number of cases per year within a certain period
describe1 <- data2 %>% 
			   filter(SubsetEpi == 1, AgeGroup==chooseAgeGroup, ClinicalPresentation==chooseClinPres) %>%  ## Select subset for epi. to avoid double-counting lab.results
			   dplyr::group_by(ReportingCountry, TimePeriod, PCV1013Yr) %>%
			   dplyr::summarise(PeriodAvg = mean(n()),
			                    PropTyped = (sum(Serotype!="NULL" & Serotype!="UNK") / n())) %>%
				mutate(Period = case_when(TimePeriod == 1 | TimePeriod == 2 ~ "Average pre-PCV7 period",
										  TimePeriod == 3  ~ "Average PCV7 period",
										  TimePeriod >3 ~ paste("PCV10/13 Year", PCV1013Yr)))

describe1$ReportingCountry <- factor(describe1$ReportingCountry,
							  levels = rev(EEA),ordered = TRUE)
							  
describe1$Period <- factor(describe1$Period,
							  levels = c("Average pre-PCV7 period", "Average PCV7 period", "PCV10/13 Year 1", "PCV10/13 Year 2", "PCV10/13 Year 3", "PCV10/13 Year 4", "PCV10/13 Year 5", "PCV10/13 Year 6", "PCV10/13 Year 7"),ordered = TRUE)

typeFactor <- cut(describe1$PropTyped, 
				   breaks = seq(0,1, by=0.1) ,
				   labels = c("0%-10%", "10%-20%", "20%-30%", "30%-40%", "40%-50%", "50%-60%", "60%-70%", "70%-80%", "80%-90%", "90%-100%"),
				   include.lowest=TRUE) 
							  
describe1$PropTyped <- factor(typeFactor)
colours1 <- rev(colorRampPalette(ECDCcol)(length(unique(typeFactor))))

## Adjust colour saturation and value
light = function(cols, ds=0.6, dv=0.5) {
  cols = rgb2hsv(col2rgb(cols))
  cols["v", ] = cols["v", ] + dv*(1 - cols["v", ])
  cols["s", ] = ds*cols["s", ]
  apply(cols, 2, function(x) hsv(x[1], x[2], x[3]))
}

									  
ggplot(describe1, aes(Period, ReportingCountry, fill=PropTyped)) +
      geom_tile() +
      geom_text(aes(label = PeriodAvg))	 +
      scale_fill_manual(values=light(colours1), guide = guide_legend(keywidth = 3, title = "Prop. serotyped", label.theme = element_text(face="plain", angle=0))) + 
      labs(x = "",
           y = "Reporting country") +
      theme(text = element_text(size=16),
         #axis.text.x = element_text(angle=60, hjust=1.1),
		 axis.title.x = element_text(vjust = 1.5),
		 legend.position = "right")  +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  
  
  
			   
## Mean number of cases per year within a certain period
## Scotland has twice as many rows
casesPeriod <- data2 %>% 
			   filter(SubsetEpi == 1 & AgeGroup >=4) %>%  ## Select subset for epi. to avoid double-counting lab.results
			   dplyr::group_by(ReportingCountry, Year, GroupType, AgeGroup, TimePeriod, Population, PersonYears) %>%
               #dplyr::group_by(ReportingCountry, Year, TimePeriod, Population, PersonYears, AgeGroup) %>%
			   dplyr::summarise(Total = n())

## Define incidence per 100,000			   
casesPeriod <- casesPeriod %>% mutate(Incidence = (Total/Population)*100000)


#casesPeriod %>% filter(ReportingCountry == "SE")	# Show number of cases and incidence for 0-4 year olds in Sweden 
#check <- casesPeriod %>% filter(ReportingCountry == "SE" ) %>% arrange(AgeGroup)

averagePeriod <- casesPeriod %>% filter(AgeGroup ==4) %>%
				 dplyr::group_by(ReportingCountry, TimePeriod, GroupType) %>% # , AgeGroup) %>%
	             dplyr::summarise(PeriodAvg = mean(Total),   ## Average over vaccination period
				                  PopulationAvg = mean(Population))                
   
averagePeriod <- averagePeriod %>% mutate(Incidence = (PeriodAvg/PopulationAvg)*100000)

checkAverage <- averagePeriod #%>% arrange(AgeGroup) 

## Incidence by group of serotypes
typesTime <- data2 %>% 
			   filter(SubsetLab == 1) %>%  ## Select subset for lab.results to avoid double-counting 
			   dplyr::group_by(ReportingCountry, AgeGroup, Year, GroupType, ClinicalPresentation, Outcome) %>%
               dplyr::summarise(Total = n())
checkTypes <- typesTime %>% filter(AgeGroup >= 4 & ClinicalPresentation == "SEPTI")#filter(ReportingCountry == countryCode & AgeGroup == 1)

## Proportion of outcome by serotype
propTypes <- data2 %>% 
			   filter(SubsetLab == 1 & AgeGroup == 5) %>% # & ClinicalPresentation == "SEPTI") %>%  ## Select subset for lab.results to avoid double-counting 
			   dplyr::group_by(ReportingCountry, PCV1013Yr, GroupType) %>%
               dplyr::summarise(n = n()) %>%
			   mutate(freq = n / sum(n))
checkProps <- propTypes#%>% #filter(ReportingCountry == countryCode & AgeGroup == 1)


## Proportion of serotype for each outcome
propOutcome <- data2 %>% 
			   filter(SubsetLab == 1, GroupType == "PCV10" | GroupType == "PCV7" | GroupType == "PCV13") %>%  ## Select subset for lab.results to avoid double-counting 
			   dplyr::group_by(ReportingCountry, PCV1013Yr, ClinicalPresentation) %>%
               dplyr::summarise(n = n()) %>%
			   mutate(freq = n / sum(n))
checkOutcome <- propOutcome #%>% filter(Outcome=="D")#(ClinicalPresentation == "BACTERPNEUMO")#filter(ReportingCountry == countryCode & AgeGroup == 1)


## Distribution of clinical presentation
## N.B. Check case definitions
clinPresTime <- data2 %>% 
			   filter(SubsetEpi == 1) %>%  ## Select subset for epi. to avoid double-counting lab.results
			   dplyr::group_by(ReportingCountry, AgeGroup, GroupType, ClinicalPresentation, TimePeriod, PCV1013Yr) %>%
               dplyr::summarise(Total = n(),
								PYAvg = mean(PersonYears))   

clinPresTimeAll <- data2 %>% 
			    filter(SubsetEpi == 1) %>%  ## Select subset for epi. to avoid double-counting lab.results
			    dplyr::group_by(ReportingCountry, AgeGroup, ClinicalPresentation, TimePeriod, PCV1013Yr) %>%
                dplyr::summarise(Total = n(),
								PYAvg = mean(PersonYears))   

## Countries that first introduced PCV-7 
clinPresTime7 <- data2 %>% 
			   filter(SubsetEpi == 1,  usedPCV7 == 1) %>%  ## Select subset for epi. to avoid double-counting lab.results
			   dplyr::group_by(ReportingCountry, AgeGroup, GroupType, ClinicalPresentation, TimePeriod, PCV1013Yr) %>%
               dplyr::summarise(Total = n(),
								PYAvg = mean(PersonYears))   

clinPresTime7All <- data2 %>% 
			    filter(SubsetEpi == 1,  usedPCV7 == 1) %>%  ## Select subset for epi. to avoid double-counting lab.results
			    dplyr::group_by(ReportingCountry, AgeGroup, ClinicalPresentation, TimePeriod, PCV1013Yr) %>%
                dplyr::summarise(Total = n(),
								PYAvg = mean(PersonYears))   

## Countries that first introduced PCV-10/13 
clinPresTimeN7 <- data2 %>% 
			   filter(SubsetEpi == 1,  usedPCV7 == 0) %>%  ## Select subset for epi. to avoid double-counting lab.results
			   dplyr::group_by(ReportingCountry, AgeGroup, GroupType, ClinicalPresentation, TimePeriod, PCV1013Yr) %>%
               dplyr::summarise(Total = n(),
								PYAvg = mean(PersonYears))   

clinPresTimeN7All <- data2 %>% 
			    filter(SubsetEpi == 1,  usedPCV7 == 0) %>%  ## Select subset for epi. to avoid double-counting lab.results
			    dplyr::group_by(ReportingCountry, AgeGroup, ClinicalPresentation, TimePeriod, PCV1013Yr) %>%
                dplyr::summarise(Total = n(),
								PYAvg = mean(PersonYears))   

								
checkClin <- clinPresTime %>% filter(AgeGroup == 1 & ClinicalPresentation=="MENI")

##################################
## Pooled incidence rate ratios ##
##################################
## All types
chooseEra <- 3   ##(2=prePCV7, 3=PCV7)  Need to choose manually since may not to include both pre-SpIDnet and SpIDnet
chooseYear <- 4 ## year post intro. of PCV10/13
chooseClinPres <- "SEPTI"
chooseAgeGroup <- 1

casesNow <- clinPresTimeAll %>% 
			filter(AgeGroup == chooseAgeGroup & ClinicalPresentation==chooseClinPres & PCV1013Yr == chooseYear)#& TimePeriod==chooseEra)   ## Compare with first year of PCV10/13
			
casesNow <-	casesNow %>% 
            select(ReportingCountry, Total, PYAvg) %>%
			rename(CasesNow = Total, PYNow = PYAvg)  

			
casesThen <- clinPresTimeAll %>% 
			filter(AgeGroup == chooseAgeGroup & ClinicalPresentation==chooseClinPres & PCV1013Yr == 1)#& TimePeriod==chooseEra)   ## Compare with first year of PCV10/13
	        
casesThen <- casesThen %>% 
            select(ReportingCountry, Total, PYAvg) %>%
			rename(CasesThen = Total, PYThen = PYAvg) 			
			
IRRdata <- left_join(casesNow[,c(4:6)],casesThen[,c(4:6)])
IRRdata$PYNow <- as.numeric(IRRdata$PYNow)
IRRdata$PYThen <- as.numeric(IRRdata$PYThen)

metainc(event.e=CasesNow, time.e=PYNow, event.c=CasesThen, time.c=PYThen, data=na.omit(IRRdata), studlab=ReportingCountry)


## By grouped type
chooseType <- "PCV10"
chooseEra <- 3   ##(2=prePCV7, 3=PCV7)  Need to choose manually since may not to include both pre-SpIDnet and SpIDnet
chooseYear <- 7 ## year post intro. of PCV10/13
chooseClinPres <- "MENI"
chooseAgeGroup <- 1

casesNow <- clinPresTime %>% 
			filter(AgeGroup == chooseAgeGroup & GroupType==chooseType & ClinicalPresentation==chooseClinPres & PCV1013Yr == chooseYear)#& TimePeriod==chooseEra)   ## Compare with first year of PCV10/13
			
casesNow <-	casesNow %>% 
            select(ReportingCountry, Total, PYAvg) %>%
			rename(CasesNow = Total, PYNow = PYAvg)  

			
casesThen <- clinPresTime %>% 
			filter(AgeGroup == chooseAgeGroup & GroupType==chooseType & ClinicalPresentation==chooseClinPres & PCV1013Yr == 1)#& TimePeriod==chooseEra)   ## Compare with first year of PCV10/13
	        
casesThen <- casesThen %>% 
            select(ReportingCountry, Total, PYAvg) %>%
			rename(CasesThen = Total, PYThen = PYAvg) 			
			
IRRdata <- left_join(casesNow[,c(5:7)],casesThen[,c(5:7)])
IRRdata$PYNow <- as.numeric(IRRdata$PYNow)
IRRdata$PYThen <- as.numeric(IRRdata$PYThen)

metainc(event.e=CasesNow, time.e=PYNow, event.c=CasesThen, time.c=PYThen, data=na.omit(IRRdata), studlab=ReportingCountry)

##########################
## Case fatality ratios ##
##########################
CFRTime <- data2 %>% 
			   filter(SubsetEpi == 1, Outcome == "A" | Outcome == "D") %>%  ## Select subset for epi. to avoid double-counting lab.results
			   dplyr::group_by(ReportingCountry, PCV1013Yr, Outcome) %>%
               dplyr::summarise(n = n()) %>%
			   mutate(freq = n / sum(n)) %>%
			   filter(Outcome == "D")
			   
CFRType <- data2 %>% 
			   filter(SubsetEpi == 1, Outcome == "A" | Outcome == "D", TimePeriod > 3, GroupType != "NULL" & GroupType != "UNK") %>%  ## Select subset for epi. to avoid double-counting lab.results
			   dplyr::group_by(ReportingCountry, GroupType, Outcome) %>%
               dplyr::summarise(n = n()) %>%
			   mutate(freq = n / sum(n)) %>%
			   filter(Outcome == "D")
			   
CFRType <- within(CFRType, 
                   GroupType <- factor(GroupType, 
                                      levels=c("PCV7", "PCV10", "PCV13", "PPV23", "Other")))
			   
mixed.lmer <- lmer(freq ~ GroupType + (1|ReportingCountry), data = na.omit(CFRType))
summary(mixed.lmer)



###########
## Plots ##
###########
#Create a custom colour scale
myColours <- c("101 179 46", "124 189 196", "192 210 54", "62 91 132", "0 140 117", "130 66 141", "232 104 63", "184 26 93" )
ECDCcol <- sapply(strsplit(myColours, " "), function(x)
    rgb(x[1], x[2], x[3], maxColorValue=255))
				
			
## Plot incidence by age group (stacked)
ggplot(data = checkAverage, aes(TimePeriod, Incidence)) +
  geom_bar(stat="identity", aes(fill = AgeGroup)) +  
  labs(	x = "PCV period", y = "Incidence per 100,000") +
  facet_wrap(~ ReportingCountry,  scales = "free_y") +
  theme(panel.grid.major = element_blank(), 
					  panel.grid.minor = element_blank(), 
					  panel.background = element_blank(),
					  axis.line = element_line(colour = "black"),
					 text = element_text(size=14),
					 # axis.text.x = element_text(size = rel(0.75), angle = 60),
					  axis.text.x.top = element_text(vjust = -0.5)) +
  ggtitle(paste("Incidence by age group")) +
  theme(plot.title = element_text(hjust = 0.5))

 ## Incidence by age group (single)
  ggplot(data = checkAverage, aes(Year, Incidence)) +
  geom_bar(stat="identity", aes(fill = factor(GroupType, levels=c("NULL", "UNK", "Other", "PPV23", "PCV13", "PCV10", "PCV7")))) +  
  labs(	x = "Year", y = "Incidence per 100,000") +
  facet_wrap(~ ReportingCountry,  scales = "free_y") +
  theme(panel.grid.major = element_blank(), 
					  panel.grid.minor = element_blank(), 
					  panel.background = element_blank(),
					  axis.line = element_line(colour = "black"),
					 text = element_text(size=14),
					 # axis.text.x = element_text(size = rel(0.75), angle = 60),
					  axis.text.x.top = element_text(vjust = -0.5)) +
					  guides(fill=guide_legend(title="Vaccine type")) +
  ggtitle(paste("Incidence of IPD in 65-84 year olds")) +
  theme(plot.title = element_text(hjust = 0.5))
					  
## Plot number of cases of grouped serotype by year
ggplot(data = na.omit(checkTypes), aes(x=Year, y=Total)) +
  geom_bar(stat="identity", aes(fill = factor(GroupType, levels=c("NULL", "UNK", "Other", "PPV23", "PCV13", "PCV10", "PCV7")))) +  
  labs(	x = "Year", y = "No. of cases") +
  facet_wrap(~ ReportingCountry,  scales = "free_y") +
  theme(panel.grid.major = element_blank(), 
					  panel.grid.minor = element_blank(), 
					  panel.background = element_blank(),
					  axis.line = element_line(colour = "black"),
					  text = element_text(size=14),
					  axis.text.x = element_text(size = rel(0.75), angle = 60),
					  axis.text.x.top = element_text(vjust = -0.5)) +
					  guides(fill=guide_legend(title="Vaccine type")) +
  ggtitle(paste("Incident cases of septicaemia by vaccine type (Age 65+)")) +
  theme(plot.title = element_text(hjust = 0.5))


 	  
## Plot proportion of cases of grouped serotype by year
ggplot(data = na.omit(checkProps), aes(x=PCV1013Yr, y=freq)) +
  geom_bar(stat="identity", aes(fill = factor(GroupType, levels=c("NULL", "UNK", "Other", "PPV23", "PCV13", "PCV10", "PCV7")))) +  
  labs(	x = "No. of years since introduction of PCV10/13", y = "Prop. of cases") +
  facet_wrap(~ ReportingCountry,  scales = "free_y") +
  theme(panel.grid.major = element_blank(), 
					  panel.grid.minor = element_blank(), 
					  panel.background = element_blank(),
					  axis.line = element_line(colour = "black"),
					  text = element_text(size=14),
					  axis.text.x = element_text(size = rel(0.75), angle = 60),
					  axis.text.x.top = element_text(vjust = -0.5)) +
					  guides(fill=guide_legend(title="Vaccine type")) +
  ggtitle(paste("Proportion of IPD cases by vaccine type (age 85+)")) +
  theme(plot.title = element_text(hjust = 0.5))

  
## Plot proportion of cases of clinical presentation by year
ggplot(data = na.omit(checkOutcome), aes(x=PCV1013Yr, y=freq)) +
  geom_bar(stat="identity", aes(fill = factor(ClinicalPresentation, levels=c("NULL", NA, "UNK", "OTHER", "MENI", "SEPTI", "BACTERPNEUMO")))) +  
  labs(	x = "No. of years since introduction of PCV10/13", y = "Prop. of cases") +
  facet_wrap(~ ReportingCountry,  scales = "free_y") +
  theme(panel.grid.major = element_blank(), 
					  panel.grid.minor = element_blank(), 
					  panel.background = element_blank(),
					  axis.line = element_line(colour = "black"),
					  text = element_text(size=14),
					  axis.text.x = element_text(size = rel(0.75), angle = 60),
					  axis.text.x.top = element_text(vjust = -0.5)) +
					  guides(fill=guide_legend(title="")) +
  ggtitle(paste("Proportion of cases by clinical presentation")) +
  theme(plot.title = element_text(hjust = 0.5))

   

					  
## Distribution of clinical presentation over time
ggplot(data = na.omit(checkClin), aes(Year, Total)) +
  geom_bar(stat="identity", aes(fill = factor(ClinicalPresentation, levels=c("NULL", NA, "UNK", "OTHER", "MENI", "SEPTI", "BACTERPNEUMO")))) +  
  labs(	x = "Year", y = "No. of cases") +
  facet_wrap(~ ReportingCountry,  scales = "free_y") +
  theme(panel.grid.major = element_blank(), 
					  panel.grid.minor = element_blank(), 
					  panel.background = element_blank(),
					  axis.line = element_line(colour = "black"),
					  text = element_text(size=14),
					  axis.text.x = element_text(size = rel(0.75), angle = 60),
					  axis.text.x.top = element_text(vjust = -0.5)) +
					  guides(fill=guide_legend(title="")) +
  ggtitle(paste("Incident cases by clinical presentation")) +
  theme(plot.title = element_text(hjust = 0.5))


 	  
## Plot case fatality rate by year
ggplot(data = na.omit(CFRTime), aes(x=PCV1013Yr, y=freq)) +
  geom_bar(stat="identity") +
  labs(	x = "No. of years since introduction of PCV10/13", y = "Case fatality rate") +
  facet_wrap(~ ReportingCountry,  scales = "free_y") +
  theme(panel.grid.major = element_blank(), 
					  panel.grid.minor = element_blank(), 
					  panel.background = element_blank(),
					  axis.line = element_line(colour = "black"),
					  text = element_text(size=14),
					  axis.text.x = element_text(size = rel(0.75), angle = 60),
					  axis.text.x.top = element_text(vjust = -0.5)) +
					  guides(fill=guide_legend(title="Vaccine type")) +
  ggtitle(paste("Case fatality rate over time")) +
  theme(plot.title = element_text(hjust = 0.5))


 	  
## Plot case fatality rate by type
ggplot(data = CFRType, aes(x=GroupType, y=freq)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("101 179 46")) +
  labs(	x = "Serotype group", y = "Case fatality rate") +
  facet_wrap(~ ReportingCountry,  scales = "free_y") +
  theme(panel.grid.major = element_blank(), 
					  panel.grid.minor = element_blank(), 
					  panel.background = element_blank(),
					  axis.line = element_line(colour = "black"),
					  text = element_text(size=14),
					  axis.text.x = element_text(size = rel(0.75), angle = 60),
					  axis.text.x.top = element_text(vjust = -0.5)) +
					  guides(fill=guide_legend(title="")) +
  ggtitle(paste("Case fatality rate by serotype")) +
  theme(plot.title = element_text(hjust = 0.5))


  
  
## Plot histogram of serotypes
## N.B. these are not all over the same period. Check date of first entry from each country
ggplot(data = data2, aes(x=Serotype)) +
  geom_bar() +
  labs(	x = "", y = "Count") +
  facet_wrap(~ ReportingCountry,  scales = "free_y") +
  theme(legend.position="none")# +
  #geom_text(data = maxCluster, aes(x = 25, y = 10, label = R0))

 
 
 
 
 
 

##################################################
## Alternative for using EUROSTAT population data 

pop2 <- dplyr::rename(pop2, Year = TIME)   ## rename to match with data2

pop2 <- pop2 %>% 
  mutate(ReportingCountry = recode(GEO, "Austria"  = "AT",              
                                       "Belgium" = "BE",
									   "Bulgaria" = "BG",
									   "Croatia"  = "HR",
                                       "Cyprus" = "CY",
									   "Czech Republic" = "CZ",
									   "Denmark" = "DK",
                                       "Estonia" = "EE",
									   "Finland" = "FI",
									   "France"  = "FR",
                                       "Germany (until 1990 former territory of the FRG)" = "DE",
									   "Greece" = "EL",
									   "Hungary" = "HU",
									   "Iceland"  = "IS",
                                       "Ireland" = "IR",
									   "Italy" = "IT",
									   "Latvia" = "LV",
                                       "Liechtenstein" = "LI",
									   "Lithuania" = "LT",
									   "Luxembourg"  = "LU",
                                       "Malta" = "MA",
									   "Netherlands" = "NL",
									   "Norway" = "NO",		   
									   "Poland" = "PO",
									   "Portugal" = "PT",            
									   "Romania" = "RO",		   
									   "Slovakia" = "SK",
									   "Slovenia" = "SL",
									   "Spain" = "ES",            
									   "Sweden" = "SE",		   
									   "United Kingdom" = "UK"))
		
		
pop2 <- pop2 %>%                 ## EUROSTAT population data
  mutate(AgeGroup = case_when(AGE == "Less than 5 years"  ~ 1,		                                                                                           ## 0-4
							  AGE == "From 5 to 9 years" | AGE == "From 10 to 14 years" ~ 2,		                                                           ## 5-14
							  AGE == "From 15 to 19 years" | AGE == "From 20 to 24 years" | AGE == "From 25 to 29 years" | AGE == "From 30 to 34 years" |
							  AGE == "From 35 to 39 years" | AGE == "From 40 to 44 years" | AGE == "From 45 to 49 years" | AGE == "From 50 to 54 years" |
							  AGE == "From 55 to 59 years" | AGE == "From 60 to 64 years"~ 3,		                                                            ## 15-64
							  AGE == "From 65 to 69 years" | AGE == "From 70 to 74 years" | AGE == "From 75 to 79 years" | AGE == "From 80 to 84 years"~ 4,		## 65-84
							  AGE == "85 years or over" ~ 5))		                                                                                            ## 85+

pop2 <- pop2 %>%            ## Group population by age groups
    dplyr::group_by(Year, ReportingCountry, AgeGroup) %>%
    dplyr::summarise(Population = sum(Value))
#################################################################################


data2 %>% filter(Outcome == "D") %>% group_by(ReportingCountry) %>% summarise(Total=n())
