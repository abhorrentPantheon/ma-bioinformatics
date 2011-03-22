#    norm_TIC.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Data normalization using row sums (area normalisation)
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    Normalised data matrix (.csv format)

#
#    Load and prepare the data matrix
#
# Read in the .csv file
in_file<-file.choose()
input_data<-read.csv(in_file, sep=",", row.names=1, header=TRUE)

# Get groups information
Group<-input_data[,1]
# Remove groups for data processing
pre_norm<-input_data[,-1]

# Create normalisation vector
sample_sum<- rowSums(pre_norm,na.rm=TRUE)

# Prepare an empty matrix
norm_data<-matrix(NA,nrow=nrow(pre_norm),ncol=length(pre_norm))
rownames(norm_data)<-rownames(pre_norm)
colnames(norm_data)<-colnames(pre_norm)

#
#    Normalise the data
#
for (ii in 1:length(sample_sum)) {
    for (jj in 1:length(pre_norm)) {   # this gives the row length
        norm_data[ii,jj]<-pre_norm[ii,jj]/sample_sum[ii]
    }
}
# Reattach groups information
output<-cbind(data.frame(Group),norm_data)

# Edit the column names if necessary
colnames(output) <- if (
    length(
        grep("^X[\\d]",colnames(output),perl=TRUE)
    ) != 0
) {# then
    gsub("^X([\\d].*)","\\1",colnames(output),perl=TRUE)
} else {
    colnames(output)
}

#
#    Generate the output matrix in .csv format
#
write.csv(output,"norm_data_sum.csv")
