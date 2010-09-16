#    norm_IS.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Normalise a data matrix using an internal standard
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf where
#              the second column contains the internal standard (or other
#              normalisation factor)
#    Output:    Normalised data matrix (.csv format)

#
#    Load and prepare the data matrix
#
# Read in the .csv file
data<-read.csv("input.csv", sep=",", row.names=1, header=TRUE)
# Get groups information
Group<-data[,1]
# Get normalisation vector
norm_vector<-data[,2]
# Remove groups and internal standard for data processing
pre_norm<-data[,-(1:2)]

# Prepare an empty matrix
norm_data<-matrix(NA,nrow=nrow(pre_norm),ncol=length(pre_norm))
rownames(norm_data)<-rownames(pre_norm)
colnames(norm_data)<-colnames(pre_norm)

#
#    Normalise the data
#
for (ii in 1:nrow(pre_norm)) {
    for (jj in 1:length(pre_norm)) {   # this gives the row length
        norm_data[ii,jj]<-pre_norm[ii,jj]/norm_vector[ii]
        }
    }
# Reattach groups information
output<-cbind(data.frame(Group),norm_data)

# Edit the column names if necessary
colnames(output) <- if
    (length(grep("^X[\\d]",colnames(output),perl=TRUE)) != 0) # then
    {gsub("^X([\\d].*)","\\1",colnames(output),perl=TRUE)} else
    {colnames(output)}

#
# Generate the output matrix in .csv format
#
write.csv(output,"norm_data_IS.csv")


