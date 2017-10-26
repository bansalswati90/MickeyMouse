library(dplyr)
library(ggplot2)
library(maps)

#####################################################################################
###LOADING DATA IN CORRECT FORMAT

unzip(zipfile = "loan.zip", exdir = ".") #Don't run if you have already extracted the Excel
raw.loan <- read.csv("loan.csv",header = T)
loan <- raw.loan

dim(loan) #39717 rows and #111 columns
str(loan)
summary(loan)
#Some columns need to be converted as factors

#Creating a function for Date Formatting
customformatdate <- function(x) {
  x <- paste("01", x, sep = "-")
  x <- as.Date(x, format = "%d-%b-%y")
}

#Changing Column format
loan <- loan %>% mutate(
  int_rate_perc = as.numeric(gsub("%", "", int_rate)), #Change the percentages to numbers
  emp_title = as.character(emp_title),
  issue_d = customformatdate(issue_d),
  url = as.character(url),
  desc = as.character(desc),
  title = as.character(title),
  delinq_2yrs = factor(delinq_2yrs),
  earliest_cr_line = customformatdate(earliest_cr_line),
  inq_last_6mths = factor(inq_last_6mths),
  revol_util_perc = as.numeric(gsub("%", "", revol_util)), #Change the percentages to numbers
  last_pymnt_d = customformatdate(last_pymnt_d),
  next_pymnt_d = customformatdate(next_pymnt_d),
  last_credit_pull_d = customformatdate(last_credit_pull_d)
)

summary(loan)

# Remove columns with no variance i.e. columns with single value
clean_loan<-loan [sapply(loan, function(x) length(unique(x))>1)]
ncol(loan) - ncol(clean_loan)
# 60

# Remove columns with no varience at all. i.e. with Constant value
clean_loan <- clean_loan [sapply(clean_loan, function(x) length(unique(na.omit(x)))>1)]
ncol(loan) - ncol(clean_loan)
# Total 63 columns removed


# Since all loans are individual loans we can remove columns like id, member_id ...removing zipcode
clean_loan <- subset(clean_loan, select=-c(id,member_id,url,desc,title,zip_code))
#####################################################################################
###BASIC DATA INTEGRITY CHECK

sum(duplicated(clean_loan$id))
sum(duplicated(clean_loan$member_id))
#All unique id's and member_id's


#Checking the amounts 
#loan_amnt, funded_amnt, funded_amnt_inv, installment, annual_inc
summary(clean_loan[,c(3,4,5,8, 14)])

summary(factor(clean_loan$term))
#36 months - 29096  
#60 months - 10621 

levels(clean_loan$grade) 
#7 levels "A" "B" "C" "D" "E" "F" "G"

levels(clean_loan$sub_grade) 
# 35 levels A1-A5, B1-B5, C1-C5 , D1-D5 , E1-E5, F1-F5, G1-G5


#to determine duplicates
sum(duplicated(clean_loan$id))
sum(duplicated(clean_loan$member_id))

#to check any more NA's
sum(is.na(clean_loan)) #102011

sum(is.na(clean_loan$id))
sum(is.na(clean_loan$member_id))
sum(is.na(clean_loan$loan_amnt))
sum(is.na(clean_loan$funded_amnt))

sum(is.na(clean_loan$next_pymnt_d)) #38577
sum(is.na(clean_loan$mths_since_last_delinq)) #25682
sum(is.na(clean_loan$mths_since_last_record))#36931

########### What is this code? ##############

#checking Blank values remaining---pending not done yet
sapply(clean_loan, function(x) length(which(x=="")))

#pymnt_plan has only 'n' value
#initial_list_status has only 'f' value
#mths_since_last_major_derog has only NA value
#policy_code has only '1' value
#application_type has only 'INDIVIDUAL' value
#annual_inc_joint has only NA value
#dti_joint has only NA value
#verification_status_joint hs only NA value
#acc_now_delinq has only value of 0
#tot_coll_amt	tot_cur_bal	open_acc_6m	open_il_6m	open_il_12m	open_il_24m	mths_since_rcnt_il	total_bal_il	il_util	open_rv_12m has only NA value
#open_rv_24m	max_bal_bc	all_util	total_rev_hi_lim	inq_fi	total_cu_tl	inq_last_12m	acc_open_past_24mths	avg_cur_bal	bc_open_to_buy	bc_util has only NA value

table(clean_loan$chargeoff_within_12_mths)
#chargeoff_within_12_mths has 0 and NA values
#collections_12_mths_ex_med has 0 and NA values

#delinq_amnt has only 0 value
#mo_sin_old_il_acct	mo_sin_old_rev_tl_op	mo_sin_rcnt_rev_tl_op	mo_sin_rcnt_tl	mort_acc	mths_since_recent_bc	mths_since_recent_bc_dlq	mths_since_recent_inq	mths_since_recent_revol_delinq	num_accts_ever_120_pd	num_actv_bc_tl	num_actv_rev_tl	num_bc_sats	num_bc_tl	num_il_tl	num_op_rev_tl	num_rev_accts	num_rev_tl_bal_gt_0	num_sats	num_tl_120dpd_2m	num_tl_30dpd	num_tl_90g_dpd_24m	num_tl_op_past_12m	pct_tl_nvr_dlq	percent_bc_gt_75 has all NA values
#

table(loan$pub_rec_bankruptcies)
#has 0 1 2 and NA value

table(loan$tax_liens)
#tax_liens has 0 and NA value

#tot_hi_cred_lim	total_bal_ex_mort	total_bc_limit	total_il_high_credit_limit has NA values





write.csv(clean_loan,"clean.loan.csv",row.names = F)


#####################################################################################
###Deriving columns

# Creating interst rates Bins
clean_loan <- clean_loan %>%
  mutate(int_rate_grp = ifelse(int_rate_perc < 10, "Low",
                               ifelse((int_rate_perc >= 10 & int_rate_perc < 18), "Medium", "High"))) %>%
  mutate(int_rate_grp = factor(int_rate_grp))

summary(clean_loan$int_rate_grp)


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
  labs(x = "Loan Sub-Grades", y ="Count") +
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

ggplot(chargedoff_subset,aes(x=as.factor(chargedoff_subset$int_rate_grp) , y = chargedoff_subset$annual_inc))+
  geom_boxplot() +
  labs(x = "Interest Rate", y ="Annual Income") +
  theme_bw()


