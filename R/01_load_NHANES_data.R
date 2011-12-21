graphics.off()

#all referenced libraries
library(foreign)
library(optmatch)

#first of all: load all the names of the data tables in
FileListToUse = "R\\xpt_files_no_dietary.csv"
xpt_file_list = t(read.csv(FileListToUse, header = F))[1, ]


#truncate to something reasonable ONLY IF USING DIETARY DATA
#otherwise, leave this line commented out:
#xpt_file_list = xpt_file_list[1 : 10]

#Load ALL the data in, yes that's all the data in ALL the files
ALL_DATA = list()
for (xpt_file in xpt_file_list){
#	print(paste("importing...", xpt_file, "..."))
	ALL_DATA[[xpt_file]] = read.xport(paste("NHanes_Data\\", xpt_file, sep = ""))
}


#get list of dimensions for each
dim_table = matrix(NA, ncol = 3, nrow = length(xpt_file_list))
for (i in 1 : length(xpt_file_list)){
	dim_table[i, ] = c(xpt_file_list[i], dim(ALL_DATA[[xpt_file_list[i]]]))
}
dim_table = as.data.frame(dim_table)
colnames(dim_table) = c("filename", "n", "p")
dim_table$n= as.numeric(as.character(dim_table$n))
dim_table$p = as.numeric(as.character(dim_table$p))