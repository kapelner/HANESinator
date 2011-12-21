#important global scope constant that indicates the record locator for a surveyed person
ID = "SEQN"

#first handle recoding of the treatments
trt_coding_function = trt_coding_functions[[TreatmentVarCode]]
if (!is.null(trt_coding_function)){
	ALL_DATA[[TreatmentVarFilename]][, TreatmentVarCode] = trt_coding_function(ALL_DATA[[TreatmentVarFilename]][, TreatmentVarCode])
}

#now we need the seq numbers of our response variable
response_data = ALL_DATA[[ResponseVarFilename]]
#get our full y vector and name it that
y = response_data[, ResponseVarCode]
#now we reject data that we don't want
rej_function = reject_functions[[ResponseVarCode]]
if (!is.null(rej_function)){
	response_data = response_data[rej_function(y), ]
}
#now get our ID #'s for the people who gave a response variable
id_nos = response_data[, ID]
num_id_nos = length(id_nos)

#We pull out the rows of each data frame that share an ID with sleep data
ALLsub = list()
for (xpt_file in xpt_file_list){
	X = ALL_DATA[[xpt_file]]
	ALLsub[[xpt_file]] = X[X$SEQN %in% id_nos, ]
}

#get list of dimensions for each of the data frames that have overlap
dim_table_sub = matrix(NA, ncol = 3, nrow = length(xpt_file_list))
for (i in 1 : length(xpt_file_list)){
	dim_table_sub[i, ] = c(xpt_file_list[i], dim(ALLsub[[xpt_file_list[i]]]))
}
dim_table_sub = as.data.frame(dim_table_sub)
colnames(dim_table_sub) = c("filename", "n", "p")
dim_table_sub$n= as.numeric(as.character(dim_table_sub$n))
dim_table_sub$p= as.numeric(as.character(dim_table_sub$p))
#how many covariates do we have?
p = sum(dim_table_sub$p)

#now we need a hash from covariate code => covariate short descriptions
cov_code_to_description = list()
raw_cov_desc = read.csv("R\\CovariateDescriptions.csv")
for (i in 1 : nrow(raw_cov_desc)){
	cov_code_to_description[[as.character(raw_cov_desc[i, 1])]] = as.character(raw_cov_desc[i, 2])
}

#now, we need to loop through each of the data tables and get the number of
#records per covariate that is not missing data
cov_master_list = matrix(NA, ncol = 4, nrow = 0)
for (xpt_file in xpt_file_list){
	Xsub = ALLsub[[xpt_file]]
	name_cov_and_num_rows = matrix(NA, ncol = 4, nrow = 0)
	for (cov_code in colnames(Xsub)){
		if (cov_code != ID){
			num_rows = sum(!is.na(Xsub[, cov_code]))
			cov_desc = cov_code_to_description[[cov_code]]
			cov_desc = ifelse(is.null(cov_desc), "", cov_desc)
			name_cov_and_num_rows = rbind(name_cov_and_num_rows, c(xpt_file, cov_code, cov_desc, num_rows))			
		}
	}
	cov_master_list = rbind(cov_master_list, name_cov_and_num_rows)
}
cov_master_list = as.data.frame(cov_master_list)
colnames(cov_master_list) = c("filename", "cov code", "cov description", "num_obs_non_null")
cov_master_list$num_obs_non_null = as.numeric(levels(cov_master_list$num_obs_non_null)[cov_master_list$num_obs_non_null]) 

#kill inconsistencies
cov_master_list = cov_master_list[cov_master_list$num_obs_non_null <= num_id_nos, ]

#how many non-missing points are there by covariate?
#hist(cov_master_list$num_obs_non_null, br = 100, xlab = "Response Rate", main = "Covariates by Response Rate in NHANES 2007/8")

#loookup names
#cov_master_list[cov_master_list[, "cov code"] == "PAD680", ]

num_control_vars = 0
for (filename in names(control_variables)){
  num_control_vars = num_control_vars + length(control_variables[[filename]]) 
}


#want table of variables used (just subtable of master list)
control_var_descriptions = matrix(NA, ncol = 4, nrow = 0)
for (filename in names(control_variables)){
	covariates = control_variables[[filename]]
	for (cov in covariates){
		control_var_descriptions = rbind(control_var_descriptions, cov_master_list[cov_master_list[, "cov code"] == cov, ])
	}
}
control_var_descs_ordrd = control_var_descriptions[order(control_var_descriptions[, "cov code"]), ]


treatment_var_description = cov_master_list[cov_master_list[, "cov code"] == TreatmentVarCode, ]
response_var_description = cov_master_list[cov_master_list[, "cov code"] == ResponseVarCode, ]

cat(paste("comparison #", comparison_num, ":\ntreatment: ", TreatmentVarCode, " (", treatment_var_description[, "cov description"] ,")\nresponse: ", ResponseVarCode, " (", response_var_description[, "cov description"] ,")\n", sep = ""))
if (PRINT_CONTROLS){
	cat(paste("controlled by", num_control_vars, "covariates:\n"))
}