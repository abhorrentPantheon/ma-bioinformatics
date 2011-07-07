#    norm_quantile.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Alysha De Livera
#    Purpose:    Data normalization using the quantiles (per row)
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    Normalised data matrix (.csv format)


# # Determine which variables/objects are present before running script
# rm_list<-list()
# rm_list$pre=ls()

#    Load necessary libraries, and install them if they are missing
tryCatch(
    library(preprocessCore),
    error=function(err) {
        # if this produces an error:
        source("http://bioconductor.org/biocLite.R")
        biocLite("preprocessCore")
        library(preprocessCore)
    }
)

#    Load and prepare the data matrix

# Read in the .csv file
in_file<-file.choose()
input_data<-read.csv(in_file, sep=",", row.names=1, header=TRUE)

# Get groups information
Group<-input_data[,1]
# Remove groups for data processing
pre_norm_data<-(input_data[,-1])
pre_norm<-as.matrix(t(pre_norm_data))

# Edit the column names if necessary
rownames(pre_norm) <- if (
    length(
        grep("^X[\\d]",rownames(pre_norm),perl=TRUE)
    ) != 0
) {# then
    gsub("^X([\\d].*)","\\1",rownames(pre_norm),perl=TRUE)
} else {
    rownames(pre_norm)
}


# Normalise by quantiles

norm_data<-normalize.quantiles(pre_norm)

# Reattach row and column names to normalised data

rownames(norm_data)<-rownames(pre_norm)
colnames(norm_data)<-colnames(pre_norm)

# Reattach group information
output<-cbind(data.frame(Group),t(norm_data))

#
#    Generate the output matrix in .csv format
#
write.csv(output,"norm_data_quantile.csv")

# #
# #    Tidy up
# #
# # List all objects
# rm_list$post=ls()
# # Remove objects in rm_list$post that aren't in rm_list$pre
# rm(list=rm_list$post[which(rm_list$pre!=rm_list$post)])
# rm(rm_list)
