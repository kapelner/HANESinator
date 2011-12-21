
#we can see what the result would have been if this was a randomized study
#mod = lm(formula(paste(response_col, "~ .")), Xm)
#summary(mod)
#t.test(Xm[Xm[,treatment_col] == 1, response_col], Xm[Xm[,treatment_col] == 0, response_col])
#wilcox.test(Xm[Xm[,treatment_col] == 1, response_col], Xm[Xm[,treatment_col] == 0, response_col], conf.int = TRUE)

#Get balance
treatment_cov_matrix <- Xm[Xm[,treatment_col] == 1, c(-1,-2)]
controls_cov_matrix <- Xm[Xm[,treatment_col] == 0, c(-1,-2)]
cat(paste("num controls obs = ", nrow(controls_cov_matrix), ", num treatment obs = ", nrow(treatment_cov_matrix), " (total n = ", (nrow(controls_cov_matrix) + nrow(treatment_cov_matrix)), ")\n\n", sep = ""))


#Get outcomes for each group
control_outcomes <- Xm[Xm[,treatment_col] == 0,2]
treatment_outcomes <- Xm[Xm[,treatment_col] == 1,2]

compute_cov_stats = function(treatment_cov_matrix, controls_cov_matrix){
	p = ncol(treatment_cov_matrix)
	n_c = nrow(controls_cov_matrix)
	n_t = nrow(treatment_cov_matrix)
	diff_mat = matrix(NA, nrow = p, ncol = 7)
	colnames(diff_mat) = c("trt_mean", "cont_mean", "trt_sd", "cont_sd", "std_sd_diff", "t", "pval")
	rownames(diff_mat) = colnames(treatment_cov_matrix)
	diff_mat[, "trt_mean"] = mean(treatment_cov_matrix)
	diff_mat[, "cont_mean"] = mean(controls_cov_matrix)
	diff_mat[, "trt_sd"] = sd(treatment_cov_matrix)
	diff_mat[, "cont_sd"] = sd(controls_cov_matrix)
	
	diff_mat[, "std_sd_diff"] = abs(diff_mat[, "trt_mean"] - diff_mat[, "cont_mean"]) / sqrt((diff_mat[, "trt_sd"]^2 + diff_mat[, "cont_sd"]^2) / 2) 
	est_var_diff = diff_mat[, "trt_sd"]^2 / n_t + diff_mat[, "cont_sd"]^2 / n_c
	diff_mat[, "t"] = (diff_mat[, "trt_mean"] - diff_mat[, "cont_mean"]) / sqrt(est_var_diff)
	df = min(n_c, n_t) #don't bother with Welch–Satterthwaite equation... who cares
	diff_mat[, "pval"] = 2 * pt(abs(diff_mat[, "t"]), df, lower.tail = FALSE)
	#if the std diff is NaN that means there's perfect separation or equal means between trt and control e.g. on a binary variable
	#all 1's are in the trt and all 0's are in the control or all are 1's or 0's in both categories. 
	#We want to bring this to the users attention. Since 
	#the output is sorted by pval, let's make sure it appears on top with a flag if the mean is different, otherwise on the bottom:
	diff_mat[, "pval"] = ifelse(is.nan(diff_mat[, "pval"]) & diff_mat[, "trt_mean"] == diff_mat[, "cont_mean"], Inf, diff_mat[, "pval"])
	diff_mat[, "pval"] = ifelse(is.nan(diff_mat[, "pval"]) & diff_mat[, "trt_mean"] != diff_mat[, "cont_mean"], -Inf, diff_mat[, "pval"])
	#return the rounded matrix
	round(diff_mat, 2)
}

diff_mat = compute_cov_stats(treatment_cov_matrix, controls_cov_matrix)
diff_mat = diff_mat[order(diff_mat[, "pval"]), ]

if (PRINT_COV_CHARACTERISTICS){
	cat("\nCovariate characteristics between treatment and control before matching (sorted by magnitude of difference)\n")
	print(diff_mat)
}


model_formula = formula(paste(paste(treatment_col, "~"), paste(names(treatment_cov_matrix), "", collapse = " + ")))
prop.model <- glm(model_formula, data = Xm, family = "binomial")

treatment_scores <- predict(prop.model, newdata=treatment_cov_matrix, type = "response")
control_scores <- predict(prop.model, newdata=controls_cov_matrix, type = "response")

#boxplot(treatment_scores, control_scores)

naive_dist <- abs(outer(control_scores, treatment_scores, FUN = "-"))

#Transpose if we have more controls than treatments
if (length(control_scores) > length(treatment_scores)) {
  naive_dist <- t(naive_dist)
}
matches <- pairmatch(naive_dist)
matches_matrix <- outer(matches, levels(matches), FUN = "==")

matches_matrix <- ifelse(is.na(matches_matrix), FALSE, matches_matrix)

Xm_trt_matched = matrix(NA, ncol = ncol(Xm), nrow = 0)
Xm_control_matched = matrix(NA, ncol = ncol(Xm), nrow = 0)

for (j in 1 : ncol(matches_matrix)){
	match = names(matches_matrix[matches_matrix[, j] == TRUE, j])
	if (match[1] %in% rownames(controls_cov_matrix)){
		#we're in a control
		Xm_control_matched = rbind(Xm_control_matched, Xm[match[1], ])
		Xm_trt_matched = rbind(Xm_trt_matched, Xm[match[2], ])
	}
	else {
		#we're in a trt	
		Xm_control_matched = rbind(Xm_control_matched, Xm[match[2], ])
		Xm_trt_matched = rbind(Xm_trt_matched, Xm[match[1], ])		
	}
}

Xm_trt_matched = as.data.frame(Xm_trt_matched)
Xm_control_matched = as.data.frame(Xm_control_matched)
#calculate std diffs after matching
diff_mat = compute_cov_stats(Xm_trt_matched, Xm_control_matched)
diff_mat = diff_mat[3 : nrow(diff_mat), ]
diff_mat = diff_mat[order(diff_mat[, "pval"]), ]

if (PRINT_COV_CHARACTERISTICS){
	cat("\n\nCovariate characteristics between treatment and control *after* matching (sorted by magnitude of difference)\n")
	print(diff_mat)
}

#now we have our matches, let's run the same tests above on them
avg_diff = mean(Xm_trt_matched[, response_col] - Xm_control_matched[, response_col])
ttest = t.test(Xm_trt_matched[, response_col], Xm_control_matched[, response_col], paired = TRUE)
mwwtest = wilcox.test(Xm_trt_matched[, response_col], Xm_control_matched[, response_col], paired = TRUE, conf.int = TRUE)
lin_mod = lm(formula(paste(response_col, "~ .")), rbind(Xm_trt_matched, Xm_control_matched))

#make sure to Bonferroni correct for multiple comparisons
ttest_pval = ttest$p.value * num_comparisons
mwwtest_pval = mwwtest$p.value * num_comparisons
lin_regr_pval = coef(summary(lin_mod))[, 4][2] * num_comparisons #the trt is always the second row

#print results to screen
cat(paste("\neffect size is\n  ", round(avg_diff, 4), "\nBonf-adj wilcox test pval / 2-samp t-test / OLS t-test pval =\n  ", round(mwwtest_pval, 4), " / ", round(ttest_pval, 4), " / ", round(lin_regr_pval, 4), "\n", sep = ""))
