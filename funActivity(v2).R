library(dplyr)
library(ggplot2)
library(maps)
library(reshape2)
library(corrplot)
library(stringr)
library(janitor)

#####################################################################################
### LOADING DATA

# Don't run below code if you've already extracted the CSV file
unzip(zipfile = "loan.zip", exdir = ".")

raw.loan <- read.csv("loan.csv",header = T, na.strings=c("NA", "n/a"))

dim(raw.loan) #39717 rows and #111 columns
str(raw.loan)
summary(raw.loan)
# Some columns need to be converted to correct format manually
# Many columns are present which have only NA's or single value which won't give any relevant highlights


#####################################################################################
### CLEANING DATA

# Removing columns with no variance i.e. columns with single values (even NA)
clean_loan <- raw.loan[sapply(raw.loan, function(x) length(unique(x))>1)]
ncol(raw.loan) - ncol(clean_loan)
# 60 Columns removed

# Remove columns with no varience at all. i.e. with Constant value
clean_loan <- clean_loan[sapply(clean_loan, function(x) length(unique(na.omit(x)))>1)]
ncol(raw.loan) - ncol(clean_loan)
# Total 63 columns removed

# Since all loans are individual loans we can remove columns like id, member_id, zipcode etc
clean_loan <- subset(clean_loan, select = -c(id,member_id,url,desc,title,zip_code))

#Creating a Custom Function to Format Dates to correct format
customformatdate <- function(x) {
  x <- paste("01", x, sep = "-")
  x <- as.Date(x, format = "%d-%b-%y")
}

#Changing Column formatting
clean_loan <- clean_loan %>% mutate(
  delinq_2yrs = factor(delinq_2yrs),
  inq_last_6mths = factor(inq_last_6mths),
  
  #Changing percentage fields to numbers
  int_rate_perc = as.numeric(gsub("%", "", int_rate)),
  revol_util_perc = as.numeric(gsub("%", "", revol_util)),

  #Character to Date Conversion
  issue_d = customformatdate(issue_d),
  earliest_cr_line = customformatdate(earliest_cr_line),
  last_pymnt_d = customformatdate(last_pymnt_d),
  next_pymnt_d = customformatdate(next_pymnt_d),
  last_credit_pull_d = customformatdate(last_credit_pull_d)
)

summary(clean_loan)


#####################################################################################
###BASIC ANALYSIS

#Checking the amounts 
#loan_amnt, funded_amnt, funded_amnt_inv, installment, annual_inc
summary(clean_loan[,c(3,4,5,8, 14)])

# States with majority of Loans 
clean_loan %>% 
  group_by(addr_state) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


# Plotting basic plots to get basic understanding
# Plots related to Loan
par(mfrow = c(1,3))
plot(clean_loan$term, main = 'Loan Terms')
plot(clean_loan$grade, main = 'Assigned Loan Grade')
plot(clean_loan$loan_status, main = 'Loan Status')

# Plots related to the Borrower
par(mfrow = c(1,2))
plot(clean_loan$verification_status, main = 'Borrower Income Verification Status')
plot(clean_loan$home_ownership, main = 'Home Ownership of the Borrower')
par(mfrow = c(1,1))

# Most popular category provided by borrower for loan request.
clean_loan %>% 
  group_by(purpose) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


#####################################################################################
###Deriving columns




####################################################################################
### PREPPING FOR PLOTTING US MAP USING GGPLOT 

#states_map contains every Latitude and Longitude for each US States
states_map <-map_data("state") 

#states_code contains state names and their two letter abbreviated codes
states_code <- as.data.frame(state.abb, state.name)  %>% add_rownames("State") %>% mutate(State=tolower(State))
names(states_code) <- c('State', 'State_abrv')
states_code$State_abrv <- as.character(states_code$State_abrv)

# The dataset do not have State Code for North Dakota. Instead it has an extra DC Code
# This is not 'Washington DC' as the code for Washington DC is 'WA'
# Hence, making 'north dakota' as DC for easy joining.
states_code$State_abrv[states_code$State == "north dakota"] <- 'DC'


clean_loan <- merge(clean_loan, states_code, by.x = 'addr_state', by.y = 'State_abrv', all.x = TRUE)


# Basic structure of the US Map
map <- ggplot(clean_loan, aes(map_id = State)) + 
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank())+
  guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) + 
  scale_fill_gradient(low="#56B1F7", high="#132B43")

####################################################################################
###PLOTS

# Plot showing US States with most Loan
map + geom_map(aes(fill = loan_amnt), map = states_map) +
  guides(fill=guide_legend(title="Loan Amount")) +
  labs(title = "Loan Spread")

ggplot(clean_loan,aes(x=clean_loan$loan_status,fill=loan_status))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Loan Status")) +
  labs(x = "Loan Status ", y ="Count")+
  theme_bw()

chargedoff_subset <- subset(clean_loan, clean_loan$loan_status=="Charged Off")


ggplot(chargedoff_subset,aes(x=chargedoff_subset$grade,fill=grade))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Grades")) +
  labs(x = "Loan Grade", y ="Count") +
  theme_bw()

ggplot(chargedoff_subset,aes(x=chargedoff_subset$sub_grade,fill=sub_grade))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Sub Grades")) +
  labs(x = "Loan Sub-Grades", y ="Count")
  theme_bw()

ggplot(chargedoff_subset,aes(x=chargedoff_subset$home_ownership,fill=home_ownership))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Home Ownership")) +
  labs(x = "Home Ownership", y ="Count") +
  theme_bw()

ggplot(chargedoff_subset,aes(x=chargedoff_subset$verification_status,fill=verification_status))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Verification Status")) +
  labs(x = "Verification Status", y ="Count") +
  theme_bw()

ggplot(chargedoff_subset,aes(x=chargedoff_subset$purpose,fill=purpose))+
  geom_bar() +
  coord_flip()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Purpose")) +
  labs(x = "Loan Purpose", y ="Count") +
  theme_bw()

ggplot(chargedoff_subset,aes(x=chargedoff_subset$term,fill=term))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Term")) +
  labs(x = "Loan Term", y ="Count") +
  theme_bw()

ggplot(chargedoff_subset,aes(x=chargedoff_subset$emp_length))+
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5)) +
  labs(x = "Employement Length", y ="Count") +
  theme_bw()

#Annual income and dti are the main driving factor
ggplot(chargedoff_subset,aes(x=chargedoff_subset$annual_inc))+
  geom_histogram(bins=100) +
  labs(x = "Annual Income", y ="Count") +
  theme_bw()

ggplot(chargedoff_subset,aes(x=chargedoff_subset$dti))+
  geom_histogram() +
  labs(x = "DTI", y ="Count") +
  theme_bw()

ggplot(chargedoff_subset,aes(x=chargedoff_subset$int_rate_perc))+
  geom_histogram() +
  labs(x = "Interest Rate", y ="Count") +
  theme_bw()

ggplot(clean_loan,aes(x=clean_loan$annual_inc))+
  geom_histogram()

#Box plots on measure variables

ggplot(chargedoff_subset,aes(x=as.factor(chargedoff_subset$int_rate) , y = chargedoff_subset$annual_inc))+
  geom_boxplot() +
  labs(x = "Interest Rate", y ="Annual Income") +
  theme_bw()


# Correlation matrix

clean_loan$term2 <- as.numeric(str_replace(clean_loan$term, "months", ""))
#clean_loan2 <- Filter(var, clean_loan)
clean_loan_measures <- Filter(is.numeric, clean_loan)
#clean_loan_measures <- sapply(clean_loan, is.numeric) Filter(checkUniqueVals, clean_loan_measures)
#clean_loan_measures <- Filter(var, clean_loan_measures)
#chart.Correlation(clean_loan_measures, histogram=TRUE, pch=19)

## Correlation Matrix

clean_loan_measures_corMat<- as.matrix(cor(clean_loan_measures))

corrplot(clean_loan_measures_corMat, method='circle')
clean_loan_measures_corMat[lower.tri(clean_loan_measures_corMat)]<-NA

clean_loan_measures_corMat_melted <-melt(clean_loan_measures_corMat)
clean_loan_measures_corMat_melted <-data.frame(clean_loan_measures_corMat_melted [!is.na(clean_loan_measures_corMat_melted[,3]),]) # get rid of the NA matrix entries
clean_loan_measures_corMat_melted$value_lab<-sprintf('%.2f',clean_loan_measures_corMat_melted$value)

ggplot(clean_loan_measures_corMat_melted, aes(Var2, Var1, fill = value, label=value_lab),color='blue') + 
  geom_tile() + 
  geom_text() +
  xlab('')+
  ylab('')+
  theme_minimal() +
  theme(axis.text.x = element_text(size=10, hjust=-0.08, angle= -35 ))



clean_loan_measures_corMat_melted_2 <- filter(clean_loan_measures_corMat_melted, as.numeric(value_lab) < -0.50 | as.numeric(value_lab) > 0.50)

ggplot(clean_loan_measures_corMat_melted_2, aes(Var2, Var1, fill = value, label=value_lab),color='blue') + 
  geom_tile() + 
  geom_text() +
  xlab('')+
  ylab('')+
  theme_minimal() + 
  theme(axis.text.x = element_text(size=10, hjust=-0.08, angle= -35 ))

clean_loan_measures_corMat_melted_3 <- filter(clean_loan_measures_corMat_melted_2, Var1 != Var2)

# Quantitative variables which are co-related
# 11 in m3
# 26 in m
unique(clean_loan_measures_corMat_melted_3$Var1)


