#    norm_IS.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Normalise a data matrix using an internal standard
#
#    Inputs:    1) Data matrix as specified in Data-matrix-format.pdf
#               2) Another matrix containing the values of the 
#                  normalisation factor. This must have the same number of 
#                  rows as the data matrix, and only one column
#                   
#    Output:    Normalised data matrix (.csv format)
#
#    Notes:    Normalisation vector must be of similar form to data
#              matrix - that is, rows are samples, variable is one
#              column with values for normalising against. Length must
#              match data matrix length

# Determine which variables/objects are present before running script
rm_list<-list()
rm_list$pre=ls()

#
#    Load and prepare the data matrix
#
# Read in the .csv file
in_file<-file.choose()
input_data<-read.csv(in_file, sep=",", row.names=1, header=TRUE)

# Get normalisation vector
in_normvec<-file.choose()
norm_vector<-read.csv(in_normvec,sep=",",row.names=1,header=TRUE)

# Get groups information
Group<-input_data[,1]
# Remove groups for data processing
pre_norm<-input_data[,-1]

# Prepare an empty matrix
norm_data<-matrix(NA,nrow=nrow(pre_norm),ncol=length(pre_norm))
rownames(norm_data)<-rownames(pre_norm)
colnames(norm_data)<-colnames(pre_norm)

#
#    Normalise the data
#
for (ii in 1:nrow(pre_norm)) {
    for (jj in 1:length(pre_norm)) {   # this gives the number of columns
        norm_data[ii,jj]<-pre_norm[ii,jj]/norm_vector[ii,]
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
write.csv(output,"norm_data_ext_vec.csv")

#
#    Tidy up
#
# List all objects
rm_list$post=ls()
# Remove objects in rm_list$post that aren't in rm_list$pre
rm(list=rm_list$post[which(rm_list$pre!=rm_list$post)])
rm(rm_list)
