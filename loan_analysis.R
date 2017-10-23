####SWATI-DATA CLEANING(REMOVING UNWANTED COLUMNS####
raw.loan <- read.csv("loan.csv",header = T)

loan <- raw.loan

str(loan)

summary(loan)

length(unique(loan$id))
#loan id is unique and has 39717 rows

length(unique(loan$member_id))
#member id is unique and has 39717 rows

length(unique(loan$loan_amnt)) #885 values
max(loan$loan_amnt) #35000
min(loan$loan_amnt) #500
mean(loan$loan_amnt) #11219.44

length(unique(loan$funded_amnt)) #1041
max(loan$funded_amnt) #35000
min(loan$funded_amnt) #500
mean(loan$funded_amnt) #10947.71

length(unique(loan$funded_amnt_inv)) #8205
max(loan$funded_amnt_inv) #35000
min(loan$funded_amnt_inv) #0
mean(loan$funded_amnt_inv) #10397.45

summary(loan$term)
#36 months  60 months 
#29096      10621 

summary(loan$int_rate) #371 levels
table(loan$int_rate)
levels(loan$int_rate)

length(unique(loan$installment)) #15383
max(loan$installment) #1305.19
min(loan$installment) #15.69


levels(loan$grade) #7 levels "A" "B" "C" "D" "E" "F" "G"

levels(loan$sub_grade) # 35 levels A1-A5, B1-B5, C1-C5 , D1-D5 , E1-E5, F1-F5, G1-G5

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
clean_loan<-loans[sapply(loans, function(x) length(unique(x))>1)]
ncol(loans) - ncol(clean_loan)
# 60

write.csv(clean_loan,"clean.loan.csv",row.names = F)
#################################################################################