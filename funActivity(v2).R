# Check if package is installed in the system. If not install automatically
list.of.packages <- c("dplyr", "ggplot2", "maps", "reshape2", "corrplot", "stringr", "mapproj", "ggthemes","gridExtra")
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
library(ggthemes)
library(gridExtra)


#####################################################################################
### LOADING DATA

# Run this code if you've to extract the CSV file
# unzip(zipfile = "loan.zip", exdir = ".")

#Loading the loan dataset
raw.loan <- read.csv("loan.csv",header = T, na.strings=c("NA", "n/a"), stringsAsFactors = TRUE)

loan <- raw.loan

#Dimension, structure and summary of loan dataset
dim(loan) #39717 rows and 111 columns
str(loan)
summary(loan)
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

# Removing columns with only NA values and other columns with single values or no variance
loan <- loan[sapply(loan, function(x) length(unique(na.omit(x)))>1)]
ncol(raw.loan) - ncol(loan)
# Total 63 columns removed

# Since all loans are individual loans we can remove columns like id, member_id, zipcode etc
loan <- subset(loan, select = -c(id,member_id,url,desc,title,zip_code))


#Changing Column formatting
loan <- loan %>% mutate(

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

summary(loan)


#####################################################################################
###BASIC ANALYSIS AND CHECKING IF FURTHER CLEANING IS REQUIRED

#Checking the amounts 
#loan_amnt, funded_amnt, funded_amnt_inv, installment, annual_inc
summary(loan[,c(1,2,3,6, 12)])

table(loan$term)
# 2 terms - 36 months and 60 months

table(loan$grade)
#Ranges from A to G

table(loan$sub_grade)
#Ranges from 5 sub grades for each grades - ranging from 1 to 5

table(loan$home_ownership)
# MORTGAGE - 17659; RENT - 18899; OWN - 3058
# NONE - 3; OTHER - 98
# The count of NONE is really low and this will not provide any concrete results
# Merging NONE with OTHER

loan$home_ownership <- gsub("NONE", "OTHER", loan$home_ownership)
table(loan$home_ownership)
# 4 levels - Mortgage, Own, Rent, Other

# Most popular category provided by borrower for loan request.
sumAmnts(loan, purpose) %>% 
  arrange(desc(total_issued))

# States with majority of Loans 
sumAmnts(loan, addr_state) %>% 
  arrange(desc(total_issued))


#####################################################################################
###Deriving columns

#Calculating the Credit Loss as Funded Amount - Total Received Principal
loan$credit_loss <- loan$funded_amnt - loan$total_rec_prncp

# Extracting Year from Loan Issued Date
loan$issue_year <- format(loan$issue_d, "%Y")
sumAmnts(loan, issue_year)

#converting the Employment Length field into ordered factor
loan$emp_length <- factor(loan$emp_length, 
                          levels = c("< 1 year","1 year", "2 years", "3 years","4 years", "5 years", "6 years","7 years","8 years","9 years","10+ years"))

# Creating buckets for Income Buckets
loan <- loan %>%
  mutate(
    annual_inc_bucket = ifelse(annual_inc < 35000,"< 35000",
                        ifelse((annual_inc >= 35000 & annual_inc < 55000),"35000-55000",
                        ifelse((annual_inc >= 55000 & annual_inc < 75000),"55000-75000",
                        ifelse((annual_inc >= 75000 & annual_inc < 95000),"75000-95000",
                        ifelse((annual_inc >= 95000 & annual_inc < 115000),"95000-115000",
                        ifelse((annual_inc >= 115000 & annual_inc < 135000),"115000-135000","> 135000")))))))
loan$annual_inc_bucket <- factor(loan$annual_inc_bucket,
                                 levels=c("< 35000","35000-55000","55000-75000","75000-95000","95000-115000","115000-135000","> 135000"),ordered=TRUE)


# Creating buckets for Debt-to-Income ratio
loan <- loan %>%
  mutate(
    dti_bucket = ifelse(dti < 8,"< 8",
                        ifelse((dti >= 8 & dti < 12),"8-12",
                        ifelse((dti >= 12 & dti < 18),"12-18",
                        ifelse((dti >= 18 & dti < 24),"18-24","> 24")))))
loan$dti_bucket <- factor(loan$dti_bucket,
                                 levels=c("< 8","8-12","12-18","18-24","> 24"),ordered=TRUE)


# Creating buckets for 2 year delinquency
loan$delinq_2yrs <- as.numeric(loan$delinq_2yrs)
loan <- loan %>% 
  mutate(delinq_bucket = ifelse(delinq_2yrs >= 2, "2+", delinq_2yrs))


# Creating buckets for inq_bucket
loan$inq_last_6mths <- as.numeric(loan$inq_last_6mths)
loan <- loan %>%
  mutate(
    inq_bucket = ifelse(inq_last_6mths >= 7,"7+",
                 ifelse(inq_last_6mths >= 5,"5-6",
                 ifelse(inq_last_6mths >= 3, "3-4",
                 ifelse(inq_last_6mths >= 1, "1-2", 0)))))


# Creating buckets for Revolving Buckets
grp <- quantile(loan$revol_util_perc, seq(0,1,0.1), na.rm = TRUE)
labels <- c(0, round(grp[2:10], 0), "+inf")
labels <- paste(labels[1:10], labels[2:11], sep = "-")
loan <- loan %>% 
  mutate(revol_bucket = cut(loan$revol_util_perc,
                            breaks = grp, 
                            labels = factor(labels), 
                            include.lowest=TRUE))

# Creating buckets for Revolving Balance Buckets
grp <- quantile(loan$revol_bal, seq(0,1,0.1))
labels <- c(0, round(grp[2:10], 0), "+inf")
labels <- paste(labels[1:10], labels[2:11], sep = "-")
loan <- loan %>% 
  mutate(revol_bal_bucket = cut(loan$revol_bal, 
                                breaks = grp, 
                                labels = factor(labels), 
                                include.lowest=TRUE))

# Creating buckets for int_rate_perc
loan <- loan %>%
  mutate(
    int_rate_bucket = ifelse(int_rate_perc < 8,"< 8",
                        ifelse((int_rate_perc >= 8 & int_rate_perc < 11),"8-11",
                        ifelse((int_rate_perc >= 11 & int_rate_perc < 14),"11-14",
                        ifelse((int_rate_perc >= 14 & int_rate_perc < 17),"14-17",
                        ifelse(int_rate_perc >= 17, "> 17","0"))))))
loan$int_rate_bucket <- factor(loan$int_rate_bucket, 
                               levels = c("0","< 8", "8-11", "11-14", "14-17", "> 17"),ordered=TRUE)




####################################################################################
####UNIVARIATE ANALYSIS####

# Analyzing Loan Amount
p_loan_amnt_freq <- 
  loan %>%
  ggplot(aes(x = loan_amnt, fill = "red")) +
  geom_histogram() +
  ggtitle("Frequency Distribution - Loan Amount") +
  labs(x = "Loan Amount", y = "Count") +
  theme_gdocs() +
  guides(fill = FALSE)

p_loan_amnt_box <- loan %>% 
  ggplot(aes(x = factor(0), loan_amnt)) +
  geom_boxplot() +
  coord_flip()

grid.arrange(p_loan_amnt_freq, p_loan_amnt_box, nrow = 2)

# Analyzing Funded Amount
p_funded_amnt_freq <- 
  loan %>%
  ggplot(aes(x = funded_amnt, fill = "red")) +
  geom_histogram() +
  ggtitle("Funded Amount Histogram") +
  labs(x = "Funded Amount", y = "Count") +
  theme_gdocs() +
  guides(fill = FALSE)

p_funded_amnt_box <- loan %>% 
  ggplot(aes(x = factor(0), loan_amnt)) +
  geom_boxplot() +
  theme_gdocs() +
  coord_flip()

grid.arrange(p_funded_amnt_freq, p_funded_amnt_box, nrow = 2)

#Credit Loss for Charged off Status
p_creditloss_freq <- 
  loan %>% filter(loan_status=="Charged Off") %>%
  ggplot(aes(x = credit_loss, fill = "red")) +
  geom_histogram() +
  ggtitle("Credit Loss Histogram") +
  labs(x = "Credit Loss", y = "Count") +
  theme_gdocs() +
  guides(fill = FALSE)

p_creditloss_box <- loan %>% filter(loan_status=="Charged Off") %>%
  ggplot(aes(x = factor(0), credit_loss)) +
  geom_boxplot() +
  theme_gdocs() +
  coord_flip()

grid.arrange(p_creditloss_freq, p_creditloss_box, nrow = 2)


# Analyzing Purpose of the Loan
p_purpose_hist <- loan %>%
  ggplot(aes(x=purpose,fill="red")) +
  geom_bar() +
  coord_flip()+
  ggtitle("Frequency of Purpose") +
  labs(x = "Loan Purpose", y ="Count") +
  theme_gdocs() + 
  guides(fill=FALSE)

p_purpose_hist

# Analyzing Loan Status
p_loan_status <- loan %>% 
  ggplot(aes(x = loan_status, fill = "red")) + 
  geom_bar() +
  ggtitle("Loan Status Histogram") +
  labs(x = "Loan Status", y = "Count") +
  theme_gdocs() +
  guides(fill = FALSE)

p_loan_status

# Analyzing US States
p_US_bar <- loan %>% 
  group_by(addr_state) %>% 
  summarise(number = n()) %>% 
  arrange(desc(number)) %>%
  ggplot(aes(x = reorder(addr_state, number), y = number, fill = "red")) +
  geom_bar(stat = "identity") +
  coord_flip()+
  labs(x = "States", y ="Frequency") +
  ggtitle("Frequency of Loans per US States") +
  theme_gdocs()+
  guides(fill = FALSE)

p_US_bar

# Analyzing Issued Date of Loans
p_Issue_Date_hist <- loan %>%
  ggplot(aes(x = issue_d, fill = "red")) +
  geom_bar() +
  labs(x = "Year", y ="Total Loan Issued") +
  ggtitle("Loan Issued Grouped by Year") +
  theme_gdocs()+
  guides(fill = FALSE)

p_Issue_Date_hist



#########################################################################################
####SEGMENTED ANALYSIS####

#Purpose
sumAmnts(loan, purpose)

p_purpose_loan_amt <- 
  loan %>% 
  group_by(purpose) %>% 
  summarise(total_loan = sum(loan_amnt)) %>% 
  arrange(desc(total_loan)) %>%
  ggplot(aes(x = reorder(purpose,total_loan), y = total_loan, fill = "red")) +
  geom_bar(stat = "identity") +
  coord_flip()+
  labs(x = "Purpose", y ="Total Loan Issued") +
  ggtitle("Purpose vs Total Loan Amount") +
  theme_gdocs()+ 
  guides(fill=FALSE)

p_purpose_loan_amt

#Home Ownership
sumAmnts(loan, home_ownership)

p_home_ownership_loan_amt <- 
  loan %>% 
  group_by(home_ownership) %>% 
  summarise(total_loan = sum(loan_amnt)) %>% 
  arrange(desc(total_loan)) %>%
  ggplot(aes(x = reorder(home_ownership,-total_loan), y = total_loan, fill = "red")) +
  geom_bar(stat = "identity") +
  labs(x = "Home Ownership", y ="Total Loan Issued") +
  ggtitle("Home Ownership vs Total Loan Amount") +
  theme_gdocs()+ 
  guides(fill=FALSE)

p_home_ownership_loan_amt

#Term
sumAmnts(loan, term)

p_term_loan_amt <- 
  loan %>% 
  group_by(term) %>% 
  summarise(total_loan = sum(loan_amnt)) %>% 
  ggplot(aes(x = term, y = total_loan, fill = "red")) +
  geom_bar(stat = "identity") +
  labs(x = "Term", y ="Total Loan Issued") +
  ggtitle("Term vs Total Loan Amount") +
  theme_gdocs()+ 
  guides(fill=FALSE)

p_term_loan_amt

#Interest Rate
p_int_rate <- 
  loan %>% 
  ggplot(aes(x =factor(0),int_rate_perc, fill = int_rate_bucket)) +
  geom_boxplot() +
  labs(x = "",y="Interest Rate") +
  ggtitle("Interest Rate") +
  guides(fill=guide_legend("Interest Rate")) + 
  theme_gdocs()


p_int_rate


#Employment Length
sumAmnts(loan, emp_length)

p_emp_length_loan_amt <- 
  loan %>% 
  group_by(emp_length) %>% 
  summarise(total_loan = sum(loan_amnt)) %>% 
  ggplot(aes(x = emp_length, y = total_loan, fill = "red")) +
  geom_bar(stat = "identity") +
  labs(x = "Employment Length", y ="Total Loan Issued") +
  ggtitle("Employment Length vs Total Loan Amount") +
  theme_gdocs()+ 
  guides(fill=FALSE)

p_emp_length_loan_amt

#Grades
sumAmnts(loan, grade)

p_grade_loan_amt <- 
  loan %>% 
  group_by(grade) %>% 
  summarise(total_loan = sum(loan_amnt)) %>% 
  ggplot(aes(x = grade, y = total_loan, fill = "red")) +
  geom_bar(stat = "identity") +
  labs(x = "Grade", y ="Total Loan Issued") +
  ggtitle("Loan Grade vs Total Loan Amount") +
  theme_gdocs()+ 
  guides(fill=FALSE)

p_grade_loan_amt

#Grades vs Interest

p_grade_int_rate <- 
  loan %>% 
  ggplot(aes(x = grade, y = int_rate_perc, fill = "red")) +
  geom_boxplot() +
  labs(x = "Grade", y ="Interest Rates") +
  ggtitle("Loan Grade vs Interest Rates") +
  theme_gdocs()+ 
  guides(fill=FALSE)

p_grade_int_rate

#Debt-To-Income
p_dti <- 
  loan %>% 
  ggplot(aes(x =factor(0),dti, fill = dti_bucket)) +
  geom_boxplot() +
  labs(x = "",y="Debt To Income") +
  ggtitle("Debt To Income") +
  guides(fill=guide_legend("Debt To Income")) + 
  theme_gdocs()


p_dti

#########################################################################################
#####BIVARIATE ANALYSIS PLOTS####

ggplot(loan %>% 
         select(int_rate_bucket, credit_loss) %>% 
         group_by(int_rate_bucket) %>% 
         summarise(CreditLoss = sum(credit_loss)),aes(x = int_rate_bucket, y = CreditLoss, fill = "red"))+
  geom_bar(stat="identity") + 
  labs(x="Interest Rate",y="Credit Loss")+
  ggtitle("Credit Loss for interest Rate")+
  theme_gdocs()

ggplot(loan %>% 
         select(dti_bucket, credit_loss) %>% 
         group_by(dti_bucket) %>% 
         summarise(CreditLoss = sum(credit_loss)),aes(x = dti_bucket, y = CreditLoss, fill = "red"))+
  geom_bar(stat="identity") + 
  labs(x="DTI",y="Credit Loss")+
  ggtitle("Credit Loss for DTI")+
  theme_gdocs()

ggplot(loan %>% 
         select(annual_inc_bucket, credit_loss) %>% 
         group_by(annual_inc_bucket) %>% 
         summarise(CreditLoss = sum(credit_loss)),aes(x = annual_inc_bucket, y = CreditLoss, fill = "red"))+
  geom_bar(stat="identity") + 
  labs(x="Annual Income",y="Credit Loss")+
  ggtitle("Credit Loss for Annual Income")+
  theme_gdocs()


ggplot(loan %>% 
         select(term, credit_loss) %>% 
         group_by(term) %>% 
         summarise(CreditLoss = sum(credit_loss)),aes(x = term, y = CreditLoss, fill = "red"))+
  geom_bar(stat="identity") + 
  labs(x="Term",y="Credit Loss")+
  ggtitle("Credit Loss for Term")+
  theme_gdocs()

ggplot(loan %>% 
         select(home_ownership, credit_loss) %>% 
         group_by(home_ownership) %>% 
         summarise(CreditLoss = sum(credit_loss)),aes(x = home_ownership, y = CreditLoss, fill = "red"))+
  geom_bar(stat="identity") + 
  labs(x="home_ownership",y="Credit Loss")+
  ggtitle("Credit Loss for home_ownership")+
  theme_gdocs()



####################################################################################
### Plotting Map Visualization for US States 

#states_map contains every Latitude and Longitude for each US States
states_map <-map_data("state") 

#states_code contains state names and their two letter abbreviated codes
states_code <- as.data.frame(state.abb, state.name)  %>% add_rownames("State") %>% mutate(State=tolower(State))
names(states_code) <- c('State', 'State_abrv')
states_code$State_abrv <- as.character(states_code$State_abrv)

loan <- merge(loan, states_code, by.x = 'addr_state', by.y = 'State_abrv', all.x = TRUE)


# Basic structure of the US Map
map <- ggplot(loan, aes(map_id = State)) + 
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank())+
  guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) + 
  scale_fill_gradient(low="#56B1F7", high="#132B43")


######PLOT##############################################################################
###PLOTS

# Plot showing US States with most Loan
ggplot(loan, aes(map_id = State)) + 
  geom_map(aes(fill = loan_amnt), map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme(legend.position = "bottom",
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.text =  element_blank())+
  guides(fill = guide_colorbar(barwidth = 10, barheight = .5)) + 
  scale_fill_gradient(low="#56B1F7", high="#132B43") + 
  guides(fill=guide_legend(title="Loan Amount")) +
  labs(title = "States with most Loan") +
  theme_gdocs()
# North Dakota (ND) is missing from the dataset.
# We see only few states are have dense concentration. That means, it is not uniformly allocated.


# Finding States with highest Loan Amount

loan %>% 
  group_by(addr_state) %>% 
  summarise(total_loan = sum(loan_amnt)) %>% 
  top_n(10) %>%
  arrange(desc(total_loan)) %>%
  ggplot(aes(x = reorder(addr_state,total_loan), y = total_loan, fill = "red")) +
  geom_bar(stat = "identity") +
  coord_flip()+
  labs(x = "States", y ="Total Loan Issued") +
  ggtitle("Loan Issued Grouped by States") +
  theme_gdocs()

# CA is leading with the most Loan Amount
# Followed by NY, TX, FL, NJ


# States Track Record
map + geom_map(aes(fill = loan_amnt), map = states_map) +
  guides(fill=guide_legend(title="Loan Amount")) +
  labs(title = "Loan Distribution") + 
  coord_map()+
  facet_grid(~ loan_status, shrink = TRUE) +
  theme_gdocs()


# Loan Amount
summary(loan$loan_amnt)

# Looking at total Loan Amount per Loan Status
sumAmnts(loan, loan_status)

# Looking at some statistics for each Loan Status
sumStats(loan, loan_status)

# Bar chart of distribution of Loan Amount accross different Statuses.
sumAmnts(loan, loan_status) %>%
  ggplot(aes(x = loan_status, y = total_issued, fill = loan_status)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_issued), position = position_stack(vjust = .5)) +
  labs(x = "Loan Status ", y ="Total Loan Issued") +
  ggtitle("Loan Issued Grouped by Status") +
  guides(fill=guide_legend("Loan Status")) +
  theme_gdocs()


# Statistics for the year of Issuance of Loan
sumStats(loan, issue_year)


p1 <- sumAmnts(loan, issue_year) %>%
  ggplot(aes(x = issue_year, y = total_issued, fill = issue_year)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y ="Total Loan Issued") +
  ggtitle("Loan Issued Grouped by Year") +
  theme_gdocs()

p2 <- sumAmnts(loan, issue_year, issue_d) %>%
  ggplot(aes(x = issue_d, y = total_issued, fill = issue_year)) +
  geom_bar(stat = "identity") +
  labs(x = "Month-Year", y ="Total Loan Issued") +
  ggtitle("Loan Issued Grouped by Month-Year") +
  guides(fill=guide_legend("Issue Year")) +
  theme_gdocs()

# Summary of Loan Amount by Issued Year
grid.arrange(p1, p2, nrow = 2)

#Loan of different grades changing over time
ggplot(loan %>% 
         select(issue_d, loan_amnt, grade) %>% 
         group_by(issue_d, grade) %>% 
         summarise(Amount = sum(loan_amnt)),aes(x = issue_d, y = Amount))+
  geom_area(aes(fill=grade)) + 
  labs(x="Date issued",y="Amount")+
  ggtitle("Loan Amount by Date issued for different grades")+
  theme_gdocs()

# Looking at Grade Statistics
sumAmnts(loan, grade)

# Distribution of Loans accross the different Loan Grades
ggplot(loan,aes(x=loan$grade,fill=loan_status))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..),position = position_stack(vjust=0.5)) +
  guides(fill=guide_legend("Loan Status")) +
  labs(x = "Loan Grade", y ="Count") +
  ggtitle("Frequency of Loan Grades") +
  theme_gdocs()

# Distribution of Loans accross different Loan Sub-Grades
ggplot(loan,aes(x=loan$sub_grade,fill=loan_status))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..),position = position_stack(vjust=0.5)) +
  guides(fill=guide_legend("Loan Status")) +
  labs(x = "Loan Sub-Grades", y ="Count") +
  ggtitle("Frequency of Loan Sub-Grades") +
  theme_gdocs()


#Distribution of loan amounts by status
ggplot(loan, aes(loan_status, loan_amnt))+
  geom_boxplot(aes(fill = loan_status)) +
  labs(x = "Status",y = "Amount") +
  guides(fill=guide_legend("Loan Status")) +
  ggtitle("Loan amount by status") +
  theme_gdocs()


# Loan taken by different sections of the Home Owners

sumAmnts(loan, home_ownership)

sumAmnts(loan, home_ownership, loan_status) %>% 
  ggplot(aes(x = home_ownership, y = total_issued, fill = loan_status)) +
  geom_bar(stat = "identity") +
  labs(x = "Home Ownership", y ="Total Loan Issued") +
  guides(fill=guide_legend("Loan Status")) +
  ggtitle("Loan Issued grouped by Home Ownership") +
  theme_gdocs()




# Revolving Balance

sumAmnts(loan, revol_bal_bucket)

sumAmnts(loan, revol_bal_bucket) %>%
  ggplot(aes(x = revol_bal_bucket, y = total_issued, fill = "red")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_issued),position = position_stack(vjust=0.5),colour="white") +
  coord_flip() +
  labs(x = "Revolving Balance", y ="Total Loan Issued") +
  ggtitle("Revoling Balance for What Purpose") +
  theme_gdocs()

# Employment Length
sumAmnts(loan, emp_length)

sumAmnts(loan, emp_length) %>%
  ggplot(aes(x = emp_length, y = total_issued, fill = "red")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = total_issued),position = position_stack(vjust=0.5)) +
  labs(x = "Employment Length", y ="Total Loan Issued") +
  ggtitle("Employment Length of people taking loans") +
  theme_gdocs()


# Delinquency Bucket
sumAmnts(loan, delinq_2yrs)


# Debt-to-Income Ratio
sumAmnts(loan, dti_bucket)

loan %>%
  ggplot(aes(grade, dti, color = grade)) +
  geom_boxplot() +
  theme_gdocs() +
  xlab("Loan Grades ranging from A to G") +
  ylab("DTI (Debt to Income Ratio (%)") +
  ggtitle("DTI Distribution vs. Loan Grades")



ggplot(loan,aes(x=loan$verification_status,fill=loan_status))+
  geom_bar()+
  guides(fill=guide_legend("Loan Status")) +
  geom_text(stat = 'count', aes(label = ..count..),position = position_stack(vjust=0.5)) +
  labs(x = "Verification Status", y ="Count") +
  ggtitle("Frequency of Verification Status") +
  theme_gdocs()

ggplot(loan,aes(x=loan$purpose,fill=loan_status))+
  geom_bar() + 
  coord_flip()+
  guides(fill=guide_legend("Loan Status")) +
  labs(x = "Loan Purpose", y ="Count") +
  ggtitle("Frequency of Loan Purpose") +
  theme_gdocs()

ggplot(loan,aes(x=loan$term,fill=loan_status))+
  geom_bar()+
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5))+
  guides(fill=guide_legend("Loan Status")) +
  labs(x = "Loan Term", y ="Count") +
  ggtitle("Frequency of Loan Term") +
  theme_gdocs()

ggplot(loan,aes(x=loan$emp_length,fill=loan_status))+
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), position = position_stack(vjust = 0.5)) +
  labs(x = "Employement Length", y ="Count") +
  guides(fill=guide_legend("Loan Status")) +
  ggtitle("Frequency of Employement") +
  theme_gdocs()


ggplot(loan,aes(x=loan$int_rate_bucket,fill=loan_status))+
  geom_bar(stat = "count") +
  labs(x = "Interest Rate", y ="Count") +
  ggtitle("Frequency of Interest Rate") +
  theme_gdocs()


#Box plots on measure variables


## Correlation matrix

loan$term2 <- as.numeric(str_replace(loan$term, "months", ""))

loan_measures <- Filter(is.numeric, loan)

loan_measures_corMat<- as.matrix(cor(loan_measures))

loan_measures_corMat[lower.tri(loan_measures_corMat)]<-NA

loan_measures_corMat_melted <-melt(loan_measures_corMat)
loan_measures_corMat_melted <-data.frame(loan_measures_corMat_melted [!is.na(loan_measures_corMat_melted[,3]),]) # get rid of the NA matrix entries
loan_measures_corMat_melted$value_lab<-sprintf('%.2f',loan_measures_corMat_melted$value)

ggplot(loan_measures_corMat_melted, aes(Var2, Var1, fill = value, label=value_lab),color='blue') + 
  geom_tile() + 
  geom_text() +
  xlab('')+
  ylab('')+
  theme_minimal() +
  theme(axis.text.x = element_text(size=10, hjust=-0.08, angle= -35 ))

loan_measures_corMat_melted_2 <- filter(loan_measures_corMat_melted, as.numeric(value_lab) < -0.50 | as.numeric(value_lab) > 0.50)

ggplot(loan_measures_corMat_melted_2, aes(Var2, Var1, fill = value, label=value_lab),color='blue') + 
  geom_tile() + 
  geom_text() +
  xlab('')+
  ylab('')+
  theme_minimal() + 
  theme(axis.text.x = element_text(size=10, hjust=-0.08, angle= -35 ))

loan_measures_corMat_melted_3 <- filter(loan_measures_corMat_melted_2, Var1 != Var2)

# Quantitative variables which are co-related
# 11 in m3
# 26 in m
unique(loan_measures_corMat_melted_3$Var1)

