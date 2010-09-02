#    norm_median.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Data normalization using the median per row
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    Normalised data matrix (.csv format)

#
#    Load and prepare the data matrix
#
# Read in the .csv file
data<-read.csv("input.csv", sep=",", row.names=1, header=TRUE)
# Get groups information
Group<-data[,1]
# Remove groups for data processing
pre_norm<-data[,-1]

# Create normalisation vector
medians<-apply(pre_norm,1,median,na.rm=TRUE)

# Prepare an empty matrix
norm_data<-matrix(NA,nrow=length(medians),ncol=length(pre_norm))
rownames(norm_data)<-rownames(pre_norm)
colnames(norm_data)<-colnames(pre_norm)

#
#    Normalise the data
#
for (ii in 1:length(medians)) {
    for (jj in 1:length(pre_norm)) {   # this gives the row length
        norm_data[ii,jj]<-pre_norm[ii,jj]/medians[ii]
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
#    Generate the output matrix in .csv format
#
write.csv(output,"norm_data_median.csv")

