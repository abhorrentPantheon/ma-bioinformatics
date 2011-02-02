#    norm_quantile.r
#
#    Author:    Amsha Nahid, Jairus Bowne,Alysha De Livera
#    Purpose:    Data normalization using the quantiles
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    Normalised data matrix (.csv format)


#    Load necessary libraries, and install them if they are missing
tryCatch(library(preprocessCore), error=function(err)
    # if this produces an error:
    install.packages("preprocessCore",repos="http://cran.ms.unimelb.edu.au/"))

#    Load and prepare the data matrix

# Read in the .csv file
data<-read.csv("input.csv", sep=",", row.names=1, header=TRUE)
# Get groups information
Group<-data[,1]
# Remove groups for data processing
pre_norm_data<-(data[,-1])
pre_norm<-as.matrix(t(pre_norm_data))

# Edit the column names if necessary
rownames(pre_norm) <- if 
    (length(grep("^X[\\d]",rownames(pre_norm),perl=TRUE)) != 0) # then
    {gsub("^X([\\d].*)","\\1",rownames(pre_norm),perl=TRUE)} else
    {rownames(pre_norm)}


#normalization by quantiles

norm_data<-normalize.quantiles(pre_norm)

#reattaching row and column names to normalized data

rownames(norm_data)<-rownames(pre_norm)
colnames(norm_data)<-colnames(pre_norm)

# Reattach groups information
output<-cbind(data.frame(Group),t(norm_data))


#    Generate the output matrix in .csv format
#
write.csv(output,"norm_data_quantile.csv")

