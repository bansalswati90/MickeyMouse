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

# Function to determine the outliers in a measure
CheckOutlierExist <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
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

#Creating buckets for funded amount
grp <- seq(0,35000, 5000)
labels <- paste(grp[1:7], grp[2:8], sep = ' - ')
loan <- mutate(loan, funded_amnt_bucket = cut(loan$funded_amnt, breaks = grp, labels = factor(labels), include.lowest = TRUE))


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
  labs(x = "", y = "Loan Amount") +
  theme_gdocs() +
  coord_flip()

grid.arrange(p_loan_amnt_freq, p_loan_amnt_box, nrow = 2)


# Analyzing Funded Amount
p_funded_amnt_freq <- 
  loan %>%
  ggplot(aes(x = funded_amnt, fill = "red")) +
  geom_histogram() +
  ggtitle("Frequeny Distribution - Funded Amount") +
  labs(x = "Funded Amount", y = "Count") +
  theme_gdocs() +
  guides(fill = FALSE)

p_funded_amnt_box <- loan %>% 
  ggplot(aes(x = factor(0), loan_amnt)) +
  geom_boxplot() +
  labs(x = "", y = "Funded Amount") +
  theme_gdocs() +
  coord_flip()

grid.arrange(p_funded_amnt_freq, p_funded_amnt_box, nrow = 2)


# Analyzing Annual Income
p_annual_inc_freq <- 
  loan %>%
  ggplot(aes(x = annual_inc, fill = "red")) +
  geom_histogram() +
  ggtitle("Frequency Distributino - Annual Income") +
  labs(x = "Annual Income", y = "Count") +
  theme_gdocs() +
  guides(fill = FALSE)

p_annual_inc_box <- loan %>%
  ggplot(aes(x = factor(0), annual_inc)) +
  geom_boxplot() +
  labs(y = "Annual Income", x = "") +
  theme_gdocs() +
  coord_flip()

grid.arrange(p_annual_inc_freq, p_annual_inc_box, nrow = 2)


#Credit Loss for Charged off Status
p_creditloss_freq <- 
  loan %>% filter(loan_status=="Charged Off") %>%
  ggplot(aes(x = credit_loss, fill = "red")) +
  geom_histogram() +
  ggtitle("Frequency Distribution - Credit Loss") +
  labs(x = "Credit Loss", y = "Count") +
  theme_gdocs() +
  guides(fill = FALSE)

p_creditloss_box <- loan %>% filter(loan_status=="Charged Off") %>%
  ggplot(aes(x = factor(0), credit_loss)) +
  geom_boxplot() +
  labs(x = "", y = "Credit Loss") +
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
  geom_text(stat='count',aes(label=..count..),vjust= 0.5, hjust = -0.1) +
  theme_gdocs() + 
  guides(fill=FALSE)

p_purpose_hist


# Analyzing Loan Status
p_loan_status <- loan %>% 
  ggplot(aes(x = loan_status, fill = "red")) + 
  geom_bar() +
  ggtitle("Frequency Distribution - Loan Status") +
  labs(x = "Loan Status", y = "Count") +
  geom_text(stat='count',aes(label=..count..),vjust= -.75) +
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
  ggtitle("Loan Frequency per US States") +
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

#Interest Rate
p_int_rate <- 
  loan %>% 
  ggplot(aes(x =factor(0),int_rate_perc, fill = int_rate_bucket)) +
  geom_boxplot() +
  labs(x = "",y="Interest Rate") +
  ggtitle("Interest Rate") +
  guides(fill=guide_legend("Interest Rate")) + 
  theme_gdocs()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p_int_rate


#Debt-To-Income
p_dti <- 
  loan %>% 
  ggplot(aes(x =factor(0),dti, fill = dti_bucket)) +
  geom_boxplot() +
  labs(x = "",y="Debt To Income") +
  ggtitle("Debt To Income") +
  guides(fill=guide_legend("Debt To Income")) + 
  theme_gdocs() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p_dti


#########################################################################################
####OUTLIER TREATMENT####

# Annual Income
CheckOutlierExist(loan,annual_inc) #function call
summary(loan$annual_inc)
hist(loan$annual_inc, main = "Histogram of Annual Income")

outlier_range<-1.5*IQR(loan$annual_inc) #1843 outlier
upper_whisker=unname(quantile(loan$annual_inc,0.75))+outlier_range
lower_whisker=unname(quantile(loan$annual_inc,0.25))-outlier_range
loan <- loan[which((loan$annual_inc>=upper_whisker | loan$annual_inc<=lower_whisker)==FALSE),]

summary(loan$annual_inc)
hist(loan$annual_inc, main = "Histogram of Annual Income")
#Removed extra outliers

# Loan Amount
CheckOutlierExist(loan,loan_amnt) #923 outliers
hist(loan$loan_amnt, main = "Histogram of Loan Amount")
# It is OK, we are considering the outlier. Outlier treatment is not specifically needed.

# Funded Amount
CheckOutlierExist(loan,funded_amnt) #797 outliers
hist(loan$loan_amnt, main = "Histogram of Funded Amount")
# It is OK, we are considering the outlier. Outlier treatment is not specifically needed.

#########################################################################################
####SEGMENTED ANALYSIS####

#Purpose
sumAmnts(loan, purpose)

p_purpose_loan_amt <- 
  loan %>% 
  ggplot(aes(x = purpose, fill = loan_status)) +
  geom_bar() +
  coord_flip()+
  labs(x = "Purpose", y ="Frequency") +
  ggtitle("Loan Purpose Frequency") +
  theme_gdocs()+ 
  guides(fill=guide_legend("Loan Status"))
p_purpose_loan_amt


#Home Ownership
sumAmnts(loan, home_ownership)

p_home_ownership_loan_amt <- 
  loan %>% 
  ggplot(aes(x = home_ownership, fill = loan_status)) +
  geom_bar() +
  labs(x = "Home Ownership", y ="Frequency") +
  ggtitle("Home Ownership Frequency") +
  theme_gdocs()+ 
  guides(fill=guide_legend("Loan Status"))

p_home_ownership_loan_amt


#Term
sumAmnts(loan, term)

p_term_loan_amt <- 
  loan %>% 
  ggplot(aes(x = term, fill = loan_status)) +
  geom_bar() +
  labs(x = "Loan Term", y ="Frequency") +
  ggtitle("Loan Term Frequency") +
  theme_gdocs()+ 
  guides(fill=guide_legend("Loan Status"))
p_term_loan_amt


#Employment Length
sumAmnts(loan, emp_length)

p_emp_length_loan_amt <- 
  loan %>% 
  ggplot(aes(x = emp_length, fill = loan_status)) +
  geom_bar() +
  labs(x = "Length of Employment", y ="Frequency") +
  ggtitle("Borrower's Employment Length Frequency Plot") +
  theme_gdocs()+ 
  guides(fill=guide_legend("Loan Status"))
p_emp_length_loan_amt


#Grades
sumAmnts(loan, grade)

p_grade_loan_amt <- 
  loan %>% 
  ggplot(aes(x = grade, fill = loan_status)) +
  geom_bar() +
  labs(x = "Loan Grade", y ="Frequency") +
  ggtitle("Loan Grade Frequency") +
  theme_gdocs()+ 
  guides(fill=guide_legend("Loan Status"))
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



#########################################################################################
#####BIVARIATE ANALYSIS PLOTS####

# Summary of Loan Amount by Issued Year
p_yearwise <- sumAmnts(loan, issue_year) %>%
  ggplot(aes(x = issue_year, y = total_issued, fill = issue_year)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y ="Total Loan Issued") +
  ggtitle("Loan Issued Grouped by Year") +
  guides(fill=guide_legend("Year")) +
  theme_gdocs()

p_monthyear <- sumAmnts(loan, issue_year, issue_d) %>%
  ggplot(aes(x = issue_d, y = total_issued, fill = issue_year)) +
  geom_bar(stat = "identity") +
  labs(x = "Month-Year", y ="Total Loan Issued") +
  ggtitle("Loan Issued Grouped by Month-Year") +
  guides(fill=guide_legend("Issue Year")) +
  guides(fill=guide_legend("Month Year")) +
  theme_gdocs()

grid.arrange(p_yearwise, p_monthyear, nrow = 2)


#Loan of different grades changing over time
ggplot(loan %>% 
         select(issue_d, loan_amnt, grade) %>% 
         group_by(issue_d, grade) %>% 
         summarise(Amount = sum(loan_amnt)),aes(x = issue_d, y = Amount))+
  geom_area(aes(fill=grade)) + 
  labs(x="Date issued",y="Amount")+
  ggtitle("Loan Amount by Date issued for different grades")+
  theme_gdocs()
# There is an uptrend as the year passes by
# There is a slight dip after 2008; maybe because of the Recession

# Considering only the CHARGED OFF Loan Status (i.e. the Defaulters)
loan_chargedoff <- filter(loan, loan_status=="Charged Off")


#Distribution of loan amounts by status
ggplot(loan, aes(loan_status, loan_amnt))+
  geom_boxplot(aes(fill = loan_status)) +
  labs(x = "Status",y = "Amount") +
  guides(fill=guide_legend("Loan Status")) +
  ggtitle("Loan amount by status") +
  theme_gdocs()


#Analysing only charged off loan status

#Interst Rate vs Credit Loss
p_interest_creditloss <- 
          ggplot(loan_chargedoff %>%
         select(int_rate_bucket, credit_loss) %>% 
         group_by(int_rate_bucket) %>% 
         summarise(CreditLoss = sum(credit_loss)),aes(x = int_rate_bucket, y = CreditLoss, fill = "red"))+
        geom_bar(stat="identity") + 
  labs(x="Interest Rate",y="Credit Loss")+
  ggtitle("Interst Rate vs Credit Loss")+
  geom_text(aes(label=CreditLoss),vjust= -.5,  size = 4) +
  guides(fill=FALSE) +
  theme_gdocs()

p_interest_creditloss_line <- 
  ggplot(loan_chargedoff,
aes(x = int_rate_perc, y = credit_loss))+
  geom_point(alpha=0.1, size=3) + geom_smooth()+
  labs(x="Interest Rate",y="Credit Loss")+
  ggtitle("Interst Rate vs Credit Loss")+
  guides(fill=FALSE) +
  theme_gdocs()

grid.arrange(p_interest_creditloss,p_interest_creditloss_line,ncol=2)



#Funded Amount vs Credit Loss
p_fundamt_creditloss <- 
  ggplot(loan_chargedoff %>% 
           select(funded_amnt_bucket, credit_loss) %>% 
           group_by(funded_amnt_bucket) %>% 
           summarise(CreditLoss = sum(credit_loss)),aes(x = funded_amnt_bucket, y = CreditLoss, fill = "red"))+
  geom_bar(stat="identity") + 
  labs(x="Funded Amount",y="Credit Loss")+
  ggtitle("Funded Amount vs Credit Loss Bar Chart")+
  guides(fill=FALSE) +
  geom_text(aes(label=CreditLoss),vjust= -.5,  size = 4) +
  theme_gdocs() +
  theme(axis.text.x=element_text(angle=45,hjust=1))

p_fundamt_creditloss_line <- 
  ggplot(loan_chargedoff,
         aes(x = funded_amnt, y = credit_loss))+
  geom_point(alpha=0.1, size=3) + geom_smooth()+
  labs(x="Funded Amount",y="Credit Loss")+
  ggtitle("Funded Amount vs Credit Loss Scatter Plot")+
  guides(fill=FALSE) +
  theme_gdocs()

grid.arrange(p_fundamt_creditloss,p_fundamt_creditloss_line,ncol=2)



#Debt To Income Ratio vs Credit Loss
p_dti_creditloss <-
  ggplot(loan_chargedoff %>% 
         select(dti_bucket, credit_loss) %>% 
         group_by(dti_bucket) %>% 
         summarise(CreditLoss = sum(credit_loss)),aes(x = dti_bucket, y = CreditLoss, fill = "red"))+
  geom_bar(stat="identity") + 
  labs(x="Debt To Income",y="Credit Loss")+
  ggtitle("Debt To Income Ratio vs Credit Loss")+
  geom_text(aes(label=CreditLoss),vjust= -.5,  size = 4) +
  guides(fill=FALSE) + 
  theme_gdocs()

p_dti_creditloss

#Annual Income vs Credit Loss
p_annualincome_creditloss <-
  ggplot(loan_chargedoff %>% 
         select(annual_inc_bucket, credit_loss) %>% 
         group_by(annual_inc_bucket) %>% 
         summarise(CreditLoss = sum(credit_loss)),aes(x = annual_inc_bucket, y = CreditLoss, fill = "red"))+
  geom_bar(stat="identity") + 
  labs(x="Annual Income",y="Credit Loss") +
  ggtitle("Annual Income vs Credit Loss") +
  geom_text(aes(label=CreditLoss),vjust= -.5,  size = 4) +
  guides(fill=FALSE) +
  theme_gdocs()

p_annualincome_creditloss



#Term vs Credit Loss
p_term_creditloss <-
  ggplot(loan_chargedoff %>% 
         select(term, credit_loss) %>% 
         group_by(term) %>% 
         summarise(CreditLoss = sum(credit_loss)),aes(x = term, y = CreditLoss, fill = "red"))+
  geom_bar(stat="identity") + 
  labs(x="Term",y="Credit Loss") +
  ggtitle("Credit Loss for Term") +
  geom_text(aes(label=CreditLoss),vjust= -.5,  size = 4) +
  guides(fill=FALSE) +
  theme_gdocs()

p_term_creditloss


#Home ownership vs Credit Loss
p_homeownership_creditloss <- 
  ggplot(loan_chargedoff %>% 
         select(home_ownership, credit_loss) %>% 
         group_by(home_ownership) %>% 
         summarise(CreditLoss = sum(credit_loss)),aes(x = home_ownership, y = CreditLoss, fill = "red"))+
  geom_bar(stat="identity") + 
  labs(x="home_ownership",y="Credit Loss") +
  ggtitle("Credit Loss for home_ownership") +
  geom_text(aes(label=CreditLoss),vjust= -.5,  size = 4) +
  guides(fill=FALSE) +
  theme_gdocs()

p_homeownership_creditloss



#Employment Length vs Credit Loss
p_emplen_creditloss <- 
  ggplot(loan_chargedoff %>% 
           select(emp_length, credit_loss) %>% 
           group_by(emp_length) %>% 
           summarise(CreditLoss = sum(credit_loss)),aes(x = emp_length, y = CreditLoss, fill = "red"))+
  geom_bar(stat="identity") + 
  labs(x="Employment Length",y="Credit Loss") +
  ggtitle("Employment Length vs Credit Loss") +
  geom_text(aes(label=CreditLoss),vjust= -.5,  size = 4) +
  guides(fill=FALSE) +
  theme_gdocs()

p_emplen_creditloss



####################################################################################
####MULTIVARIATE ANALYSIS####

# From Univariate Analysis it was evident that RENT and MORTGAGE are having majority of population.
#Hence, filtering the data frame with these two.
loan_charged_home <- loan_chargedoff %>%
                     filter(home_ownership == "MORTGAGE" || home_ownership == "RENT")

# Plot showing the Credit Loss against Interest Rates of Defaulters with Mortgaged or Rented houses
p_int_rate_creditloss_term <- loan_charged_home %>%
  ggplot( aes(x = int_rate_bucket, y = credit_loss, fill = term))+
  geom_bar(stat="identity", position = "stack") + 
  labs(x="Interest Rate Buckets",y="Credit Loss") +
  ggtitle("Credit Loss vs Interest Rates (for Mortaged and Rented House People)") +
  theme_gdocs()
p_int_rate_creditloss_term


# Plot showing the Credit Loss against Annual Income Ranges of Defaulters with Mortgaged or Rented house 
p_grade_creditloss_term <-loan_charged_home %>%
ggplot( aes(x = annual_inc_bucket, y = credit_loss, fill = term))+
  geom_bar(stat="identity", position = "stack") + 
  labs(x="Annual Income",y="Credit Loss") +
  ggtitle("Credit Losses across various Annual Income Range (for Mortaged and Rented House People)") +
  theme_gdocs()
p_grade_creditloss_term

# Plot showing the Credit Loss against Funded Amount Ranges of Defaulters with Mortgaged or Rented house 
p_grade_creditloss_term <-loan_charged_home %>%
ggplot( aes(x = funded_amnt_bucket, y = credit_loss, fill = term))+
  geom_bar(stat="identity", position = "stack") + 
  labs(x="Funded Amount",y="Credit Loss") +
  ggtitle("Credit Losses across various Funded Income Range (for Mortaged and Rented House People)") +
  theme_gdocs()
p_grade_creditloss_term




# In UNIVARIATE Analysis, we have noticed that Debt Consolidation, Credit Card and Home Improvement have major population
# We are not considering OTHERS as it can be anything.
loan_charged_home_purp_top3 <- loan_charged_home %>% 
  filter(purpose %in%  c("debt_consolidation", "credit_card", "home_improvement"))


# Plot showing the Credit Loss against Interest Rates of Defaulters with Mortgaged or Rented houses for Top3 Loan Purposes mentioned
p_int_rate_creditloss_purp_term <- loan_charged_home_purp_top3 %>%
  ggplot( aes(x = int_rate_bucket, y = credit_loss, fill = term))+
  geom_bar(stat="identity", position = "stack") + 
  labs(x="Interest Rate Buckets",y="Credit Loss") +
  ggtitle("Credit Loss vs Interest Rates (for TOp3 Purpose provide with  Mortaged and Rented House People)") +
  theme_gdocs()
p_int_rate_creditloss_purp_term


# Plot showing the Credit Loss against Annual Income Ranges of Defaulters with Mortgaged or Rented house for Top3 Loan Purposes mentioned
p_grade_creditloss_term <-loan_charged_home_purp_top3 %>%
  ggplot( aes(x = annual_inc_bucket, y = credit_loss, fill = term))+
  geom_bar(stat="identity", position = "stack") + 
  labs(x="Annual Income",y="Credit Loss") +
  ggtitle("Credit Losses across various Annual Income Range (for TOp3 Purpose provide with  Mortaged and Rented House People)") +
  theme_gdocs()
p_grade_creditloss_term

# Plot showing the Credit Loss against Funded Amount Ranges of Defaulters with Mortgaged or Rented house for Top3 Loan Purposes mentioned
p_grade_creditloss_term <-loan_charged_home_purp_top3 %>%
  ggplot( aes(x = funded_amnt_bucket, y = credit_loss, fill = term))+
  geom_bar(stat="identity", position = "stack") + 
  labs(x="Funded Amount",y="Credit Loss") +
  ggtitle("Credit Losses across various Funded Income Range (for TOp3 Purpose provide with  Mortaged and Rented House People)") +
  theme_gdocs()
p_grade_creditloss_term



####################################################################################
#### Plotting Map Visualization for US States ####

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


# States Track Record
map + geom_map(aes(fill = loan_amnt), map = states_map) +
  guides(fill=guide_legend(title="Loan Amount")) +
  labs(title = "Loan Distribution") + 
  coord_map()+
  facet_grid(~ loan_status, shrink = TRUE) +
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

#########################################################################################
####Correlation Matrix####

#loan$term2 <- as.numeric(str_replace(loan$term, "months", ""))
#loan_measures <- Filter(is.numeric, loan)
#loan_measures_corMat<- as.matrix(cor(loan_measures))
#loan_measures_corMat[lower.tri(loan_measures_corMat)]<-NA

#loan_measures_corMat_melted <-melt(loan_measures_corMat)
#loan_measures_corMat_melted <-data.frame(loan_measures_corMat_melted [!is.na(loan_measures_corMat_melted[,3]),]) # get rid of the NA matrix entries
#loan_measures_corMat_melted$value_lab<-sprintf('%.2f',loan_measures_corMat_melted$value)

#ggplot(loan_measures_corMat_melted, aes(Var2, Var1, fill = value, label=value_lab),color='blue') + 
#  geom_tile() + 
#  geom_text() +
#  xlab('')+
#  ylab('')+
#  theme_minimal() +
#  theme(axis.text.x = element_text(size=10, hjust=-0.08, angle= -35 ))

#loan_measures_corMat_melted_2 <- filter(loan_measures_corMat_melted, as.numeric(value_lab) < -0.50 | as.numeric(value_lab) > 0.50)

#ggplot(loan_measures_corMat_melted_2, aes(Var2, Var1, fill = value, label=value_lab),color='blue') + 
#  geom_tile() + 
#  geom_text() +
#  xlab('')+
#  ylab('')+
#  theme_minimal() + 
#  theme(axis.text.x = element_text(size=10, hjust=-0.08, angle= -35 ))

#loan_measures_corMat_melted_3 <- filter(loan_measures_corMat_melted_2, Var1 != Var2)

# Quantitative variables which are co-related
# 11 in m3
# 26 in m
#unique(loan_measures_corMat_melted_3$Var1)