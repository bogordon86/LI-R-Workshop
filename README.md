# LI R Workshop
## Setup
# Load the LI_WorkshopScript_180710.zip file onto your Desktop.
# Unzip the file onto your desktop and doubleclick the .R file script
# You just launched Rstudio and opened the file. This is our operating system, like windows.


## Getting comfortable
# To execute a command that you draft here, set your cursor on the same line as your command. 
# Then hit Run in the upper right hand corner of this pane

100+435
house<-435
house
people<-710767
house*people

## Import data
# In the lower right hand corner Explorer pane, There are 5 tabs. Click Files. 
# On the right hand side there is an ellipse (...). Click the ...
# Choose the folder you loaded the workshop files to. For example, if it's your Desktop, scroll to the top and double click Desktop. Hit OK.
# One row beneath Files in that same Explorer pane, click More (to the right, between the gear symbol and down arrow)
# Click Set as Working Directory in the dropdown menu


## First we change some options 

# At the top of this window click Tools, then select Global Options. Click Code on the left
# 1. Towards the bottom "Ctrl+Enter executes" pick "Current Line" from the dropdown.
# 2. Within Code, click the Display tab up top, then towards the middle check the box for highlight R function calls.


## These are our apps
install.packages('readr')
install.packages('dplyr')
library(readr)
library(dplyr)

##Disregard these options for now:
#options(tibble.print_max=100) # for printing longer outputs
#options(scipen = 999) # for overriding default scientific notation output



## Load and analyze data

# .csv files are excel tables 
# In R they are called Data Frames or Tibbles.
# You can cheat and preview the csv with excel-like formatting by right clicking the filename in the Files Tab of the Explorer pane and hitting "Import Dataset"

data<-read_csv('LI_WorkshopData_180710.csv')
## The <- assigns a name to an object
# Your Environment (pane in the upper right corner) lists your objects, their type and size

str(data)
glimpse(data) ## Glimpse inverts the data so you don't run out of screen real estate trying to see all your columns

## Four data types: Chr, Int/Dbl(Numerics), Fctr (categorical data), Lgl
# Knowing differences can help you troubleshoot.


names(data)## See all of the columns and their column position numbers

## To analyze specific columns you must name them with (data,column) or (data$column) 

count(data,TurnoutTargetsFlag)
tally(data,TurnoutTargetsFlag) # Simpler output

sum(data$TurnoutTargetsFlag)

count(data,Turnout_Score) # Not useful for continuous variables

summary(data$Turnout_Score) # Useful for continuous variables


## Definitions for data analysis
# Quotes, $, c(,), #
# Quotes for objects. $ for columns. c(,) for specifying multiple values. # for commenting or place holders in code
# ><,=,>= for integer type data for filtering or counting numerics

count(data,Turnout_Score>=.5) # Useful for conditions on continuous variables


sum(data$onlineact) # What's the problem? 

## NAs are the problem.
count(data,onlineact) 
data$onlineact[is.na(data$onlineact)]<-0 # Replace NAs w zeroes. 
# NAs can be necessary: for example, records that weren't analyzed vs. analyzed and deemed 0.

sum(data$onlineact)

## Definitions for more complex data analysis

# &,|,!,==, <- vs = vs ==
#You should always use the assignment arrow <- not the equals sign
# and &, or |, not !, == filtering
 
count(data,Turnout_Score>=.5 & TrumpApproveFlag==1)
count(data,Turnout_Score>=.5 | TrumpApproveFlag==1)

count(data,!Turnout_Score>=.25)
count(data,Turnout_Score<.25) # same

count(data,TrumpApproveFlag=1) 
count(data,TrumpApproveFlag==1) # not same

count(data,Turnout_Score>=.25 & TrumpApproveFlag==1) ## Which TRUE count will be higher?
count(data,Turnout_Score>=.25 | TrumpApproveFlag==1) 



## Joins

joindata<-read_csv('LI_Workshop_JoinData_180710.csv')
glimpse(joindata)

# The Pipe %>% runs multiple verbs in order on the specified data frame

flagsanddata<-data %>% 
  left_join(joindata,'uniqueid') ## don't forget the quotes around the join key. '' is equivalent to "".



## Last definitions
# %in%, c("","",""), -(drop)

count(flagsanddata, registeredparty %in% c('R','O'))  #%in% is how you check a column for values, 

flagsanddata1<-flagsanddata %>% # Almost always create a new data frame during analysis. Don't overwrite.  VERSION CONTROL IS VERY IMPORTANT!
  select(-PrecinctName)

glimpse(flagsanddata1)

## Say we are sharing this data with another vendor who is targeting calls by sex, age, and precinct.
# We want a smaller, simpler file that doesn't disclose all our analysis or confuse the recipient.

vendorlist<-flagsanddata1 %>% 
  mutate(LikelyTurnoutFlag=ifelse(Turnout_Score>=.2,1,0), # multiple columns can be created with 1 mutate if comma separated
         CallTargets=ifelse(LikelyConsiderFlag==1 &
                              LikelyTurnoutFlag==1 & 
                              landline_reliabilitycode>6,1,0)) %>% # stacking conditions prevents mistakes as it's easier to read
  select(uniqueid,LikelyConsiderFlag,LikelyTurnoutFlag,CallTargets,sex,agerange,PrecinctNumber,vf_firstname,Phone=landline) ## Renaming the landline column as Phone using =

glimpse(vendorlist)
count(vendorlist,LikelyTurnoutFlag)
count(vendorlist,CallTargets)

glimpse(flagsanddata1)



#Filtering

## Suppose an ally is running some campaigns in a sub-geography of our client and a subset of our data may be legally shared.

ldlist<-flagsanddata1 %>% 
  filter(LegislativeDistrict %in% c(55,83,85)) %>% 
  filter(!(is.na(vh16p) & is.na(vh16pp))) %>% # can list conditions like with mutate but sometimes don't for clarity 
  select(uniqueid,TrumpApproveFlag,registeredparty,vh16p,vh16pp,LegislativeDistrict,sex,agerange,PrecinctNumber,vf_firstname,Phone=cellphone)

count(flagsanddata1,LegislativeDistrict)
count(ldlist,LegislativeDistrict)

flagsanddata1 %>% count(is.na(vh16p) & is.na(vh16pp))
ldlist %>% count(is.na(vh16p) & is.na(vh16pp))



# Group By
# You can group by any categorical variable
## In our data, Which age groups represent the highest count of Lean Consider voters?

glimpse(flagsanddata1)

flagsanddata1 %>% 
  group_by(agerange) %>% 
  summarize_at(vars(LeanConsiderFlag),
               funs(sum(.,na.rm=T)))




# This will help us create charts 

## Candidate wants to see how turnout scores are distributed across the entire population
install.packages('ggplot2')
library(ggplot2)
ggplot(flagsanddata1) +
  geom_density(na.rm=TRUE,aes(Turnout_Score),fill='red',alpha=0.7,color='black')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(0,1)+ ylim(0,2)+ xlab('Probability')+ ylab('Density')+ ggtitle('Turnout Scores')

## In order to create a new file for export:

targetsexport <- data %>% 
  filter(TurnoutTargetsFlag==1)

write_csv(targetsexport,'LI_TurnoutTargetsExport_180711.csv')

## Troubleshooting
# Is your working directory set to the folder containing your data?
# Did you try quotes ('objectname') and bare?
# Did you library(dplyr) after restart? did you try clicking the Packages tab, uncheck dplyr, then check it again?
# Are there NAs, NULLs, or (rarely) elusive blanks ('' or ' ')? count(data,is.na(TurnoutTargetsFlag)) / is.null()/TurnoutTargetsFlag==' '
# Is there whitespace? data$vf_countyname<-trimws(data$vf_countyname,'r')

## Closing, questions, references

# zip() manners, PII and links (5m)
# using dates in file names. calculating dates.
# as.integer/character(),trimws(), paste0(), lubridate()
# regex with gsub and stringr -This is a powerful tool, for example, finds pattern within a string 
#workflowy.com -Helps you with saving code chunks for use later!
