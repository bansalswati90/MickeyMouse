# Check if package is installed in the system. If not install automatically
list.of.packages <- c("dplyr", "ggplot2", "maps", "reshape2", "corrplot", "stringr", "mapproj")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Loading Packages
library(dplyr)
library(ggplot2)
library(maps)
library(reshape2)
library(corrplot)
library(stringr)
library(mapproj)

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
### USER DEFINED FUNCTIONS USED IN THE STUDY

# Converts Month-Year character to R readable Date format
customformatdate <- function(x) {
  x <- paste("01", x, sep = "-")
  x <- as.Date(x, format = "%d-%b-%y")
}

# Create summary by grouping ... and showing Total Loan Amount and Number of Loans
sumAmnts <- function(x, ...) {
  x %>% 
    group_by(., ...) %>%
    summarise(total_issued = round(sum(loan_amnt)),
              n = n())
}

# Create summary by grouping ... and showing useful stats about the group
sumStats <- function(x, ...) {
  x %>% 
    group_by(., ...) %>%
    summarise(median = round(median(loan_amnt)),
              average = round(mean(loan_amnt)),
              stdev = round(sd(loan_amnt)))
}

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
###BASIC ANALYSIS AND CHECKING IF FURTHER CLEANING IS REQUIRED

#Checking the amounts 
#loan_amnt, funded_amnt, funded_amnt_inv, installment, annual_inc
summary(clean_loan[,c(1,2,3,6, 12)])

table(clean_loan$term)
# 2 terms - 36 months and 60 months

table(clean_loan$grade)
#Ranges from A to G

table(clean_loan$sub_grade)
#Ranges from 5 sub grades for each grades - ranging from 1 to 5

# Plotting basic plots to get basic understanding
# Plots related to Loan
par(mfrow = c(1,3))
plot(clean_loan$term, main = 'Loan Terms')
plot(clean_loan$grade, main = 'Assigned Loan Grade')
plot(clean_loan$loan_status, main = 'Loan Status')

table(clean_loan$home_ownership)
# MORTGAGE - 17659; RENT - 18899; OWN - 3058
# NONE - 3; OTHER - 98
# The count of NONE is really low and this will not provide any concrete results
# Merging NONE with OTHER

clean_loan$home_ownership <- gsub("NONE", "OTHER", clean_loan$home_ownership)
table(clean_loan$home_ownership)
# 4 levels - Mortgage, Own, Rent, Other

# Plots related to the Borrower
par(mfrow = c(1,2))
plot(clean_loan$verification_status, main = 'Borrower Income Verification Status')
plot(clean_loan$home_ownership, main = 'Home Ownership of the Borrower')
par(mfrow = c(1,1))

# Most popular category provided by borrower for loan request.
sumAmnts(clean_loan, purpose) %>% 
  arrange(desc(total_issued))

# States with majority of Loans 
sumAmnts(clean_loan, addr_state) %>% 
  arrange(desc(total_issued))

#####################################################################################
###Deriving columns

# Extracting Year and Month from Loan Issued Date
clean_loan$issue_year <- format(clean_loan$issue_d, "%Y")
clean_loan$issue_month <- format(clean_loan$issue_d, "%b")

sumAmnts(clean_loan, issue_year)
sumAmnts(clean_loan, issue_month) %>% arrange(desc(total_issued))


# Creating buckets for Income Buckets
grp <- quantile(clean_loan$annual_inc, seq(0,1,0.1))
labels <- c(0, round(grp[2:10],0), "+inf")
labels <- paste(labels[1:10], labels[2:11], sep = "-")
clean_loan <- clean_loan %>% 
  mutate(annual_inc_bucket = cut(clean_loan$annual_inc, breaks = grp, labels = factor(labels), include.lowest=TRUE))


# Creating buckets for Debt-to-Income ratio
grp <- quantile(clean_loan$dti, seq(0,1,0.1))
labels <- c(0, round(grp[2:10],0), "+inf")
labels <-  paste(labels[1:10], labels[2:11], sep = "-")
clean_loan <- clean_loan %>% 
  mutate(dti_bucket = cut(clean_loan$dti, breaks = grp, labels = factor(labels), include.lowest=TRUE))


# Creating buckets for 2 year delinquency
clean_loan$delinq_2yrs <- as.numeric(clean_loan$delinq_2yrs)
clean_loan <- clean_loan %>% 
  mutate(delinq_bucket = ifelse(delinq_2yrs >= 2, "2+", delinq_2yrs))


# Creating buckets for inq_bucket
clean_loan$inq_last_6mths <- as.numeric(clean_loan$inq_last_6mths)
clean_loan <- clean_loan %>%
  mutate(
    inq_bucket = ifelse(inq_last_6mths >= 7,"7+",
                 ifelse(inq_last_6mths >= 5,"5-6",
                 ifelse(inq_last_6mths >= 3, "3-4",
                 ifelse(inq_last_6mths >= 1, "1-2", 0)))))


# Creating buckets for Revolving Buckets
clean_loan = mutate(clean_loan, revol = as.numeric(gsub("%","",revol_util)))
grp <- quantile(clean_loan$revol_util_perc, seq(0,1,0.1), na.rm = TRUE)
labels <- c(0, round(grp[2:10], 0), "+inf")
labels <- paste(labels[1:10], labels[2:11], sep = "-")
clean_loan <- clean_loan %>% 
  mutate(revol_bucket = cut(clean_loan$revol_util_perc, breaks = grp, labels = factor(labels), include.lowest=TRUE))

# Creating buckets for Revolving Balance Buckets
grp <- quantile(clean_loan$revol_bal, seq(0,1,0.1))
labels <- c(0, round(grp[2:10], 0), "+inf")
labels <- paste(labels[1:10], labels[2:11], sep = "-")
clean_loan <- clean_loan %>% 
  mutate(revol_bal_bucket = cut(clean_loan$revol_bal, breaks = grp, labels = factor(labels), include.lowest=TRUE))



####################################################################################
### PREPPING FOR PLOTTING US MAP USING GGPLOT 

#states_map contains every Latitude and Longitude for each US States
states_map <-map_data("state") 

#states_code contains state names and their two letter abbreviated codes
states_code <- as.data.frame(state.abb, state.name)  %>% add_rownames("State") %>% mutate(State=tolower(State))
names(states_code) <- c('State', 'State_abrv')
states_code$State_abrv <- as.character(states_code$State_abrv)

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
  labs(title = "States with most Loan") +
  theme_bw()
# North Dakota (ND) is missing from the dataset.

# States Track Record
map + geom_map(aes(fill = loan_amnt), map = states_map) +
  guides(fill=guide_legend(title="Loan Amount")) +
  labs(title = "Loan Distribution") + 
  coord_map()+
  facet_grid(~ loan_status, shrink = TRUE) +
  theme_bw()

ggplot(clean_loan,aes(x=clean_loan$loan_status,fill=loan_status))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Loan Status")) +
  labs(x = "Loan Status ", y ="Count") +
  ggtitle("Frequency of Loan Status") +
  theme_bw()


ggplot(clean_loan,aes(x=clean_loan$grade,fill=loan_status))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Loan Status")) +
  labs(x = "Loan Grade", y ="Count") +
  ggtitle("Frequency of Loan Grades") +
  theme_bw()

ggplot(clean_loan,aes(x=clean_loan$sub_grade,fill=loan_status))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Loan Status")) +
  labs(x = "Loan Sub-Grades", y ="Count") +
  ggtitle("Frequency of Loan Sub-Grades") +
  theme_bw()

ggplot(clean_loan,aes(x=clean_loan$home_ownership,fill=loan_status))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Loan Status")) +
  labs(x = "Home Ownership", y ="Count") +
  ggtitle("Frequency of Home Ownership") +
  theme_bw()

ggplot(clean_loan,aes(x=clean_loan$verification_status,fill=loan_status))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Loan Status")) +
  labs(x = "Verification Status", y ="Count") +
  ggtitle("Frequency of Verification Status") +
  theme_bw()

ggplot(clean_loan,aes(x=clean_loan$purpose,fill=loan_status))+
  geom_bar(aes(y=..count..),position = "dodge") + 
  geom_text(stat = 'count', aes(y=..count.. , label = ..count..),  position = position_dodge(width =0.5) , vjust = -0.25) +
  coord_flip()+
  guides(fill=guide_legend("Loan Status")) +
  labs(x = "Loan Purpose", y ="Count") +
  ggtitle("Frequency of Loan Purpose") +
  theme_bw()

ggplot(clean_loan,aes(x=clean_loan$term,fill=loan_status))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Loan Status")) +
  labs(x = "Loan Term", y ="Count") +
  ggtitle("Frequency of Loan Term") +
  theme_bw()

ggplot(clean_loan,aes(x=clean_loan$emp_length,fill=loan_status))+
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5)) +
  labs(x = "Employement Length", y ="Count") +
  guides(fill=guide_legend("Loan Status")) +
  ggtitle("Frequency of Employement") +
  theme_bw()

#Annual income and dti are the main driving factor
ggplot(clean_loan,aes(x=clean_loan$annual_inc,fill=loan_status))+
  geom_histogram(bins=100) +
  labs(x = "Annual Income", y ="Count") +
  ggtitle("Frequency of Annual Income") +
  theme_bw()

ggplot(clean_loan,aes(x=clean_loan$dti,fill=loan_status))+
  geom_histogram() +
  labs(x = "DTI", y ="Count") +
  ggtitle("Frequency of DTI") +
  theme_bw()

ggplot(clean_loan,aes(x=clean_loan$int_rate_perc,fill=loan_status))+
  geom_histogram() +
  labs(x = "Interest Rate", y ="Count") +
  ggtitle("Frequency of Interest Rate") +
  theme_bw()

ggplot(clean_loan,aes(x=clean_loan$annual_inc))+
  geom_histogram()

#Box plots on measure variables


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

#########################################################################################
######BIVARIATE ANALYSIS PLOTS
ggplot(clean_loan %>% 
         select(issue_d, loan_amnt) %>% 
         group_by(issue_d) %>% 
         summarise(Amount = sum(loan_amnt)), aes(x = issue_d, y = Amount)) +
  geom_line() + 
  labs(x="Date issued", y="Amount")+
  ggtitle("Loan Amount by Date Issued") +
  theme_bw()

#Distribution of loan amounts by status
ggplot(clean_loan, aes(loan_status, loan_amnt))+
  geom_boxplot(aes(fill = loan_status)) +
  labs(x = "Status",y = "Amount") +
  ggtitle("Loan amount by status") +
  theme_bw()


#Loan of different grades changing over time

ggplot(clean_loan %>% 
         select(issue_d, loan_amnt, grade) %>% 
         group_by(issue_d, grade) %>% 
         summarise(Amount = sum(loan_amnt)),aes(x = issue_d, y = Amount))+
  geom_area(aes(fill=grade)) + 
  labs(x="Date issued",y="Amount")+
  ggtitle("Loan Amount by Date issued for different grades")+
  theme_bw()



ggplot(clean_loan %>% 
         select(issue_d, loan_status) %>% 
         group_by(issue_d) %>% 
         summarise(Status= n()), aes(x = issue_d, y=Status))+
  geom_line() + 
  labs(x="Date issued")+
  ggtitle("Status by Date issued") + 
  theme_bw()


ggplot(clean_loan %>% 
         select(issue_d, dti) %>% 
         group_by(issue_d) %>% 
         summarise(DTI = sum(dti)), aes(x = issue_d, y = DTI))+
  geom_line() + 
  labs(x="Date issued")+
  ggtitle("DTI by Date issued")+
  theme_bw()
