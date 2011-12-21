#your working directory (PLEASE LEAVE COMMENTED OUT)
setwd("C:\\Users\\kapelner\\desktop\\Dropbox\\Stat921_KM_presentation")
#setwd("C:\\Users\\magarick\\Dropbox\\Stat921_KM_presentation")

#First pick a set of response variables.
ResponseVarFilenames = c(
	"Questionnaire__sleep_disorders.xpt")
#	"Questionnaire__sleep_disorders.xpt")
ResponseVarCodes = c(
	"SLD010H")
#	"SLD020M")

#these functions allow you to alter the response by rejecting a subset of the data
reject_functions = list()
reject_functions[["SLD010H"]] = function(y){
	y < 12
}

#Now pick a set of treatment variables you would like to investigate
#causal relationships with the responses.
TreatmentVarFilenames = c(
  #"Examination__body_measures.xpt",
  "Questionnaire__medical_conditions.xpt",
	"Questionnaire__physical_activity.xpt")
#	"Questionnaire__smoking_household_smokers.xpt") #"Questionnaire__air_quality.xpt"
TreatmentVarCodes = c(
  #"BMXBMI",
  "MCQ160M",
	"PAQ605")
#	"SMD410") #"PAQ685"

#these functions you to take a continuous data vector and
#transform it to a binary 0/1 treatment variable
trt_coding_functions = list()
trt_coding_functions[["BMXBMI"]] = function(x_z){
	as.numeric(x_z >= 35)
}
trt_coding_functions[["MCQ160M"]] = function(x_z){
  as.numeric(x_z == 1)
}
trt_coding_functions[["PAQ605"]] = function(x_z){
  as.numeric(x_z == 1)
}

#the number of comparisons is the number of responses x number of treatments
num_comparisons = length(TreatmentVarCodes) * length(ResponseVarCodes)
#load up the data
source("R\\01_load_NHANES_data.R")
#initialize the object that will allow the results to persist after the script is written
causal_results = as.data.frame(matrix(NA, nrow = num_comparisons, ncol = 8))
colnames(causal_results) = c("response", "treatment", "num_controls", "effect size (mean)", "Bonf wilcox pval", "Bonf 2-samp t-test pval", "Bonf OLS t-test pval", "gamma")
cat(paste("Testing ", num_comparisons, " comparison(s)\n", sep = ""))
#begin the loops to do the comparisons, first by response
for (i_resp in 1 : length(ResponseVarCodes)){
	ResponseVarFilename = ResponseVarFilenames[i_resp]
	ResponseVarCode = ResponseVarCodes[i_resp]
	#then by treatment on this response
	for (i_trt in 1 : length(TreatmentVarCodes)){
		comparison_num = (i_resp - 1) * length(TreatmentVarCodes) + i_trt
		#pull out the treatment variable we are examining at the moment
		TreatmentVarFilename = TreatmentVarFilenames[i_trt]
		TreatmentVarCode = TreatmentVarCodes[i_trt]		
		#run the analysis for one comparison
		source("R\\02_set_controls_and_params.R")
		source("R\\03_create_info_about_covariates.R")
		source("R\\04_create_design_matrix.R")
		source("R\\05_prop_score_and_matching.R")
		source("R\\06_gamma_sensitivity.R")	
		#save the results in an object
		causal_results[comparison_num, ] = c(ResponseVarCode, 
				TreatmentVarCode, 
				num_control_vars, 
				round(avg_diff, 3), #round(mwwtest$estimate, 3), #the sample median effect (not as important)
				round(mwwtest_pval, 3),
				round(ttest_pval, 3),
				round(lin_regr_pval, 3),
				round(Gamma_min, 3))
	}	
}