PRINT_CONTROLS = FALSE
PRINT_COV_CHARACTERISTICS = FALSE
VERBOSE_GAMMA_COMPUTATIONS = FALSE

#we now want a list of variables that we potentially want to control and match in our study
control_variables = list()

control_variables[["Demographics__all.xpt"]] = 
		c(
		  "RIAGENDR", 
		  "RIDAGEYR", 
		  "DMDMARTL", 
		  "RIDRETH1", 
		  "DMDFMSIZ", 
		  "INDHHIN2")
#control_variables[["Examination__body_measures.xpt"]] = 
#		c(
#		  "BMXWT", 
#		  "BMXHT", 
#		  "BMXARML", 
#		  "BMXWAIST", 
#		  "BMXTRI")
control_variables[["Examination__vision.xpt"]] = 
		c("VIQ220")   
control_variables[["Laboratory__blood_cadmium_and_lead.xpt"]] = 
		c("LBXBCD", 
		  "LBXBPB")  
control_variables[["Laboratory__blood_total_mercury_and_blood_inorganic_mercury.xpt"]] = 
		c("LBXTHG")
control_variables[["Laboratory__complete_blood_count.xpt"]] = 
		c("LBXWBCSI", 
		  "LBXLYPCT", 
		  "LBXRBCSI", 
		  "LBXMCVSI", 
		  "LBXMCHSI", 
		  "LBXPLTSI", 
		  "LBXMPSI")  
control_variables[["Laboratory__HDL_cholesterol.xpt"]] = 
		c("LBDHDD")
control_variables[["Laboratory__rbc_folate_and_serum_folate.xpt"]] = 
		c("LBDRBF")
control_variables[["Laboratory__serum_cotinine_and_urinary_total_NNAL.xpt"]] = 
		c("LBXCOT", 
		  "URXNAL")
control_variables[["Laboratory__standard_biochemistry_profile.xpt"]] = 
		c("LBXSAL", 
		  "LBXSATSI", 
		  "LBXSAPSI", 
		  "LBXSBU", 
		  "LBXSCA", 
		  "LBXSCH", 
		  "LBXSC3SI", 
		  "LBXSGTSI", 
		  "LBXSGL", 
		  "LBXSIR", 
		  "LBXSPH", 
		  "LBXSTB", 
		  "LBXSTP", 
		  "LBXSTR", 
		  "LBXSUA", 
		  "LBXSNASI", 
		  "LBXSKSI", 
		  "LBXSCLSI", 
		  "LBXSOSSI", 
		  "LBXSGB")
#control_variables[["Laboratory__thyroid_profile.xpt"]] = 
#		c("LBXATG", 
#		  "LBXT3F", 
#		  "LBXT4F", 
#		  "LBXTGN", 
#		  "LBXTSH1", 
#		  "LBXTPO", 
#		  "LBXTT3", 
#		  "LBXTT4")
control_variables[["Laboratory__urinary_iodine.xpt"]] = 
		c("URXUIO")
control_variables[["Laboratory__urinary_nitrate_perchlorate_and_thiocyanate.xpt"]] = 
		c("URXUP8", 
		  "URXNO3", 
		  "URXSCN")
#control_variables[["Laboratory__urinary_specific_gravity.xpt"]] = 
#		c("SSUSG") ##########for some reason this one don't work??
control_variables[["Questionnaire__air_quality.xpt"]] = 
		c("PAQ685")
control_variables[["Questionnaire__audiometry.xpt"]] = 
		c("AUQ131") #model this as continuous even though it's orginal categorical
control_variables[["Examination__oral_health.xpt"]] = 
		c("OHXDECAY", 
		  "OHXREST", 
		  "OHXSEAL")
control_variables[["Questionnaire__blood_pressure_and_cholesterol.xpt"]] = 
		c("BPQ020", 
		  "BPQ052", 
		  "BPQ057", 
		  "BPQ060")
control_variables[["Questionnaire__bowel_health.xpt"]] = 
		c("BHD050")
control_variables[["Questionnaire__consumer_behavior.xpt"]] = 
		c("CBD010", 
		  "CBQ020", 
		  "CBQ030", 
		  "CBQ040", 
		  "CBQ050", 
		  "CBQ060", 
		  "CBD070", 
		  "CBD090", 
		  "CBD110", 
		  "CBD120", 
		  "CBD130", 
		  "CBQ140", 
		  "CBD150", 
		  "CBD160", 
		  "CBD170")
# model a lot of these as continuous even though they're orginal categorical
control_variables[["Questionnaire__diabetes.xpt"]] = 
		c("DIQ010")
control_variables[["Questionnaire__health_insurance.xpt"]] = 
		c("HIQ011") 
control_variables[["Questionnaire__hospital_utilization_and_access_to_care.xpt"]] = 
		c("HUQ090")
control_variables[["Questionnaire__income.xpt"]] = 
		c("IND235")  
control_variables[["Questionnaire__medical_conditions.xpt"]] = 
		c("MCQ010", "MCQ160M")
control_variables[["Questionnaire__physical_functioning.xpt"]] = 
		c("PFQ090")
control_variables[["Questionnaire__respiratory_health.xpt"]] = 
		c("RDQ070")
control_variables[["Questionnaire__smoking_cigarette_use.xpt"]] = 
		c("SMQ020")  
control_variables[["Questionnaire__smoking_household_smokers.xpt"]] = 
		c("SMD410")
control_variables[["Questionnaire__vision.xpt"]] = 
		c("VIQ031")


################finished looking for covariates up to row 1296 / 2257 in cov_master_list
#cov_master_list[1296:2257, ]

control_variables_that_are_categorical = 
		c("RIAGENDR", 
			"RIDRETH1",	"DMDMARTL",	"INDHHIN2",
			"OHXDECAY", "OHXREST", "OHXSEAL",			
			"VIQ220",
			"BPQ020", "BPQ052", "BPQ057", "BPQ060", 
			"CBQ020", 
			"CBQ060", 
			"DIQ010", 
			"HIQ011", 
			"HUQ090", 
			"MCQ010", 
			"MCQ160M", 
			"PFQ090", 
			"RDQ070", 
			"SMQ020", 
			"SMD410", 
			"VIQ031",
			"PAQ685")

#all the variable values that need to be removed from the trt, resp, controls, 
#for any reason or no reason
illegal_variable_values = list()
#only mark numerical values (all NA's are removed automatically)
illegal_variable_values[["DMDMARTL"]] = c(77, 99)
illegal_variable_values[["OHXDECAY"]] = c(9)
illegal_variable_values[["OHXREST"]] = c(9)
illegal_variable_values[["OHXSEAL"]] = c(9)
illegal_variable_values[["VIQ220"]] = c(9)
illegal_variable_values[["PAQ685"]] = c(3, 7, 9)
illegal_variable_values[["AUQ131"]] = c(7, 9)
illegal_variable_values[["BHD050"]] = c(777, 999)
illegal_variable_values[["BPQ020"]] = c(7, 9)
illegal_variable_values[["BPQ052"]] = c(7, 9)
illegal_variable_values[["BPQ057"]] = c(7, 9)
illegal_variable_values[["BPQ060"]] = c(7, 9)
illegal_variable_values[["CBD010"]] = c(7, 9)
illegal_variable_values[["CBQ020"]] = c(77, 99)
illegal_variable_values[["CBQ030"]] = c(77, 99)
illegal_variable_values[["CBQ040"]] = c(77, 99)
illegal_variable_values[["CBQ050"]] = c(77, 99)
illegal_variable_values[["CBQ060"]] = c(77, 99)
illegal_variable_values[["CBQ070"]] = c(777777, 999999)
illegal_variable_values[["CBD090"]] = c(777777, 999999)
illegal_variable_values[["CBD110"]] = c(777777, 999999)
illegal_variable_values[["CBD120"]] = c(777777, 999999)
illegal_variable_values[["CBD130"]] = c(777777, 999999)
illegal_variable_values[["CBQ140"]] = c(77, 99)
illegal_variable_values[["CBD150"]] = c(77777, 99999)
illegal_variable_values[["CBD160"]] = c(777, 999)
illegal_variable_values[["CBD170"]] = c(77777, 99999)
illegal_variable_values[["DIQ010"]] = c(7, 9)
illegal_variable_values[["HIQ011"]] = c(7, 9)
illegal_variable_values[["HUQ090"]] = c(7, 9)
illegal_variable_values[["MCQ010"]] = c(7, 9)
illegal_variable_values[["MCQ160M"]] = c(7, 9)
illegal_variable_values[["PFQ090"]] = c(7, 9)
illegal_variable_values[["RDQ070"]] = c(7, 9)
illegal_variable_values[["SMQ020"]] = c(7, 9)
illegal_variable_values[["SMD410"]] = c(7, 9)
illegal_variable_values[["VIQ031"]] = c(7, 9)
illegal_variable_values[["DBQ915"]] = c(7, 9)
illegal_variable_values[["ENQ010"]] = c(7, 9)
illegal_variable_values[["ENQ020"]] = c(7, 9)
illegal_variable_values[["DMDYRSUS"]] = c(77, 99)



#code here to check if trt, resp, controls are unique
if (TreatmentVarCode %in% control_variables[[TreatmentVarFilename]]){
#	cat("WARNING: The treatment var is also a covariate. It has been removed from the controls.\n")
	index = which(control_variables[[TreatmentVarFilename]] == TreatmentVarCode)
	control_variables[[TreatmentVarFilename]] = control_variables[[TreatmentVarFilename]][-index]
}
if (ResponseVarCode %in% control_variables[[ResponseVarFilename]]){
#	cat("WARNING: The treatment var is also a covariate. It has been removed from the controls.\n")
	index = which(control_variables[[ResponseVarFilename]] == ResponseVarCode)
	control_variables[[ResponseVarFilename]] = control_variables[[ResponseVarFilename]][-index]
}
if (TreatmentVarCode %in% control_variables_that_are_categorical){
#	cat("WARNING: The treatment var is listed as a control variable that is categorical.\n")
	index = which(control_variables_that_are_categorical == TreatmentVarCode)
	control_variables_that_are_categorical = control_variables_that_are_categorical[-index]
}
if (ResponseVarCode %in% control_variables_that_are_categorical){
#	cat("WARNING: The response var is listed as a control variable that is categorical.\n")
	index = which(control_variables_that_are_categorical == ResponseVarCode)
	control_variables_that_are_categorical = control_variables_that_are_categorical[-index]	
}
cat("\n")