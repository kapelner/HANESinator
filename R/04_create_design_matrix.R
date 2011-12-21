
#do an intersection of all the control variables that are not NULL, 
#and the treatment var that is not null, then tally the IDs left 
#and of course see how many there are
column_selections = list()
#do control vars
for (filename in names(control_variables)){
	covariates = control_variables[[filename]]
	current_data = ALLsub[[filename]]
	for (cov in covariates){
		#save the data that is not missing
		column_selections[[cov]] = current_data[!is.na(current_data[, cov]), ID]
		if (PRINT_CONTROLS){
			#get the description of this covariate
			description = cov_master_list[cov_master_list[, "cov code"] == cov, ][, "cov description"]
			#get the num obs's left
			appended_list = c()
			for (record_list in column_selections){
				appended_list = append(appended_list, record_list)
			}	
			num_left_over = sum(table(appended_list) >= length(column_selections))
			#is this a categorical var?
			categorical_var = cov %in% control_variables_that_are_categorical
			#print to screen
			cat(paste(ifelse(categorical_var, paste("{", cov, "}", sep = ""), cov), " (", description, ") n_left = ", num_left_over, "\n", sep = ""))
		}
	}
}
#do treatment var
covariates = control_variables[[TreatmentVarFilename]]
current_data = ALLsub[[TreatmentVarFilename]]
column_selections[[TreatmentVarCode]] = current_data[!is.na(current_data[, TreatmentVarCode]), ID]


appended_list = c()
for (record_list in column_selections){
	appended_list = append(appended_list, record_list)
}

#we want to get a list of the record IDs for unique people that we'll use in our
#master data matrix
record_ids_to_use = table(appended_list) >= num_control_vars
record_ids_to_use = names(record_ids_to_use[record_ids_to_use])
record_ids_to_use = sort(record_ids_to_use)

num_records_left = length(record_ids_to_use)


##want actual dataframe now with treatment as FIRST cov and response as SECOND cov and controls as all other columns
#so what are our dimensions??
#n will be the num records left and p will be the number of control vars + 2
Xm = matrix(NA, nrow = num_records_left, ncol = num_control_vars + 2)
rownames(Xm) = record_ids_to_use

#the first column is the treatment
trtXsub = ALLsub[[TreatmentVarFilename]]
#get records that match
trtXsub = trtXsub[trtXsub[, ID] %in% record_ids_to_use, ]
trtXsub = trtXsub[order(trtXsub[, ID]), ]
#now since the record ids are sorted and we're sure they're all there, so sort this matrix by record
Xm[, 1] = trtXsub[, TreatmentVarCode]

#the second column is the response
resXsub = ALLsub[[ResponseVarFilename]]
#get records that match
resXsub = resXsub[resXsub[, ID] %in% record_ids_to_use, ]
resXsub = resXsub[order(resXsub[, ID]), ]
#now since the record ids are sorted and we're sure they're all there, so sort this matrix by record
Xm[, 2] = resXsub[, ResponseVarCode]

#now, the rest are controls
count = 1
for (filename in names(control_variables)){
#	print(filename)
	covariates = control_variables[[filename]]
	conXsub = ALLsub[[filename]]
	conXsub = conXsub[conXsub[, ID] %in% record_ids_to_use, ]
	conXsub = conXsub[order(conXsub[, ID]), ]
	for (cov in covariates){
#		print(paste("  ", cov))
		Xm[, count + 2] = conXsub[, cov]
		count = count + 1
	}
}


#now convert to data frame and set column names
Xm = as.data.frame(Xm)
colnames(Xm)[1] = TreatmentVarCode
colnames(Xm)[2] = ResponseVarCode
colnames(Xm)[3 : ncol(Xm)] = as.character(control_var_descriptions[, "cov code"])

#kill NA's even though they SHOULDN'T exist in the first place
nrow_before_killing_nas = nrow(Xm)
Xm = Xm[rowSums(is.na(Xm)) == 0, ]
nrow_after_killing_nas = nrow(Xm)
#cat(paste("deleted", (nrow_before_killing_nas - nrow_after_killing_nas), "rows that had missing data\n"))

###NOW we need to kill rows that have value that we don't care to analyze
nrow_before_killing_illegals = nrow(Xm)
for (cov in names(illegal_variable_values)){
	if (cov %in% colnames(Xm)){
		Xm = Xm[!(Xm[, cov] %in% illegal_variable_values[[cov]]), ]
	}
}
num_records_left = nrow(Xm)
if (PRINT_CONTROLS){
	cat(paste("deleted", (nrow_before_killing_illegals - num_records_left), "rows that had illegal values\n"))
}

treatment_col = paste("treatment", TreatmentVarCode, sep = "_")
response_col = paste("response", ResponseVarCode, sep = "_")
colnames(Xm)[1] = treatment_col
colnames(Xm)[2] = response_col


### now, we need to convert the categorical variables to dummies
for (covar in control_variables_that_are_categorical){
#	print(covar)
	#pull out column
	categorial_var = Xm[, covar]
	#now we need to pull out the categories
	categories = sort(names(table(categorial_var)))
	#now as usual we hack off the last category
	categories = categories[1 : (length(categories) - 1)]
	#now we need to make a new matrix of 0's with ONLY the orthogonal contrasts
	Xm_cat = matrix(0, nrow = num_records_left, ncol = length(categories))
	for (i in 1 : num_records_left){
		x = as.character(categorial_var[i])		
		Xm_cat[i, which(categories == x)] = 1
	}
	
	#now convert to a dataframe
	Xm_cat = as.data.frame(Xm_cat)
	colnames(Xm_cat) = paste(covar, categories, sep = "_")
	
	#now kill the covariate in the original matrix
	Xm[, covar] = NULL
	
	#now add on the covariates
	Xm = cbind(Xm, Xm_cat)
}

#now we need to code treatment as 0 / 1
# if (max(Xm[,treatment_col]) ==2 ){
#   Xm[, treatment_col] = Xm[, treatment_col] - 1
# }
