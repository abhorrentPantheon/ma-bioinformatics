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

# Calculate the correlation matrix
cor_data<-cor(pre_cor_data)

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
