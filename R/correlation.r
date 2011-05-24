#    correlation.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Alysha De Livera
#    Purpose:   calculates the correlation between the metabolites
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    1: cor_matrix.csv gives the correlation matrix
#               2: cor_variables.csv file in which correlation 
#                  coefficeint between all the metabolites is 
#                  calculated and is ordered in decreasing value
#                  of correlation coefficient
#    Notes:    Missing values (if any) are replaced by the half of the lowest
#              value in the entire data matrix.

# Determine which variables/objects are present before running script
rm_list<-list()
rm_list$pre=ls()

#
#    Load the data matrix
#
# Read in the .csv file
in_file<-file.choose()
input_data<-read.csv(in_file, sep=",", row.names=1, header=TRUE)

# Get groups information
Group<-input_data[,1]
# Remove groups for data processing
pre_cor_data<-input_data[,-1]

# Replacing the missing values
pre_cor_data[is.na(pre_cor_data)]<-0.5*(min(pre_cor_data,na.rm=TRUE))

# Edit the column names if necessary
colnames(pre_cor_data) <- if (
    length(
        grep("^X[\\d]",colnames(pre_cor_data),perl=TRUE)
    ) != 0
) {# then
    gsub("^X([\\d].*)","\\1",colnames(pre_cor_data),perl=TRUE)
} else {
    colnames(pre_cor_data)
}

# Determine the correlation method to use
method_sel<-readline(
    paste("    1) Pearson",
        "    2) Spearman",
        "    3) Kendall",
        "    4) cancel and exit script",
        " >> Please select a number:  ",
        sep="\n"
    )
)

# Check that the response is a number
if (is.na(as.numeric(method_sel))) {
    write(" ## Please enter numbers only next time. Exiting...","")
    stop()
}

if (method_sel==1) {
    cor_met="pearson"
} else if (method_sel==2) {
    cor_met="spearman"
} else if (method_sel==3) {
    cor_met="kendall"
} else if (method_sel>=4) {
    write(" ## Exiting script.","")
    stop()
}

# Calculate the correlation matrix
cor_data<-cor(pre_cor_data,method=cor_met)

# Generate the output data (correlation matrix) in .csv format
write.csv(cor_data,"cor_matrix.csv")

# Crop the lower triangle of the correlation matrix
lower_tri<-which(lower.tri(cor_data),TRUE)

# Rearrange the data in column form
output_1<-data.frame(
    Variable_1=rownames(cor_data)[lower_tri[,1]],
    Variable_2=colnames(cor_data)[lower_tri[,2]],
    cor_coefficient=cor_data[lower_tri]
)

# Reorder the data according to the corelation coefficient
output<-data.frame(
    output_1[order(output_1$cor_coefficient,decreasing=TRUE),],
    row.names=NULL
)

# Generate the output data in .csv format
write.csv(output,"cor_variables.csv")

#
#    Tidy up
#
# List all objects
rm_list$post=ls()
# Remove objects in rm_list$post that aren't in rm_list$pre
rm(list=rm_list$post[which(rm_list$pre!=rm_list$post)])
rm(rm_list)
