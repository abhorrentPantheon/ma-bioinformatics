#    log.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Performs log (base 10) tranformation of a data matrix
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    Data matrix that has been log transformed
#
#    Notes:    Missing values (if any) are replaced by the half of the lowest
#              value in the entire data matrix.
#              Data should be log transformed prior to using this script

#
#    Load the data matrix
#
# Read in the .csv file
data<-read.csv("input.csv", sep=",", row.names=1, header=TRUE)
# Get groups information
Group<-data[,1]
# Remove groups for data processing
prelog_data<-data[,-1]
# Replace any zero values (see Notes)
prelog_data[prelog_data==0]<-0.5*(min(prelog_data[prelog_data>0],na.rm=TRUE))

#
#    Log transform the data
#
log_data<-log10(prelog_data)
# Reattach groups information
output<-cbind(Group,log_data)

# Edit the column names if necessary
colnames(output) <- if 
    (length(grep("^X[\\d]",colnames(output),perl=TRUE)) != 0) # then
    {gsub("^X([\\d].*)","\\1",colnames(output),perl=TRUE)} else
    {colnames(output)}

#
# Generate the output matrix in .csv format
#
write.csv(output,"log_transform.csv")