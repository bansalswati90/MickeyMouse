library(dplyr)


###IMPORTING DATA AND BASIC ANALYSIS
unzip(zipfile = "loan.zip", exdir = ".")
raw.loan <- read.csv("loan.csv",header = T)
loan <- raw.loan

dim(loan) #39717 rows and #111 columns
str(loan)
summary(loan)
#Some columns need to be converted as factors

length(unique(loan$id))
length(unique(loan$member_id))
#All unique id's and member_id's


#Changing Column format
loan <- loan %>% mutate(
  int_rate_perc = as.numeric(gsub("%", "", int_rate)), #Change the percentages to numbers
  emp_title = as.character(emp_title),
  ##issue_d = as.Date() -- Am not getting the date format
  url = as.character(url),
  desc = as.character(desc),
  title = as.character(title),
  delinq_2yrs = factor(delinq_2yrs),
  ##earliest_cr_line = as.Date(),
  inq_last_6mths = factor(inq_last_6mths),
  revol_util_perc = as.numeric(gsub("%", "", revol_util))
  ##last_pymnt_d = as.Date()
)

#For formatting date
#Creating a function to format date
customformatdate <- function(x) {
  x <- paste("01", x, sep = "-")
  x <- as.Date(x, format = "%d-%b-%y")
}

loan$issue_d <- customformatdate(loan$issue_d)

loan$earliest_cr_line <- customformatdate(loan$earliest_cr_line)

loan$last_pymnt_d <- customformatdate(loan$last_pymnt_d)

loan$next_pymnt_d <- customformatdate(loan$next_pymnt_d)

loan$last_credit_pull_d <- customformatdate(loan$last_credit_pull_d)

#Checking the amounts - loan_amnt, funded_amnt, funded_amnt_inv
summary(loan[,c(3,4,5,8)])

summary(factor(loan$term))
#36 months - 29096  
#60 months - 10621 


levels(loan$grade) 
#7 levels "A" "B" "C" "D" "E" "F" "G"

levels(loan$sub_grade) 
# 35 levels A1-A5, B1-B5, C1-C5 , D1-D5 , E1-E5, F1-F5, G1-G5

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

table(loan$chargeoff_within_12_mths)
#chargeoff_within_12_mths has 0 and NA values

#delinq_amnt has only 0 value
#mo_sin_old_il_acct	mo_sin_old_rev_tl_op	mo_sin_rcnt_rev_tl_op	mo_sin_rcnt_tl	mort_acc	mths_since_recent_bc	mths_since_recent_bc_dlq	mths_since_recent_inq	mths_since_recent_revol_delinq	num_accts_ever_120_pd	num_actv_bc_tl	num_actv_rev_tl	num_bc_sats	num_bc_tl	num_il_tl	num_op_rev_tl	num_rev_accts	num_rev_tl_bal_gt_0	num_sats	num_tl_120dpd_2m	num_tl_30dpd	num_tl_90g_dpd_24m	num_tl_op_past_12m	pct_tl_nvr_dlq	percent_bc_gt_75 has all NA values
#

table(loan$pub_rec_bankruptcies)
#has 0 1 2 and NA value

table(loan$tax_liens)
#has 0 and NA value

#tot_hi_cred_lim	total_bal_ex_mort	total_bc_limit	total_il_high_credit_limit has NA values

#this removes columns with no variance, the columnswhich has single value
clean_loan<-loan [sapply(loan, function(x) length(unique(x))>1)]
ncol(loan) - ncol(clean_loan)
# 60

# Remove columns with no varience at all. i.e. with Constant value
clean_loan<-clean_loan [sapply(clean_loan, function(x) length(unique(na.omit(x)))>1)]
ncol(loan) - ncol(clean_loan)
# 48 columns left 63 removed

write.csv(clean_loan,"clean.loan.csv",row.names = F)
