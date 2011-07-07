#    fold_chg.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Creates a data matrix specifying the fold change observed
#                between two groups
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    Table of means for each variable for each sample
#               Table of fold change in each sample relative 
#               to (control) group A (.csv format)
#
#    Notes:    Group value for control must be alphanumerically first

# # Determine which variables/objects are present before running script
# rm_list<-list()
# rm_list$pre=ls()

#
#    Load the data matrix
#
# Read in the .csv file
in_file<-file.choose()
input_data<-read.csv(in_file, sep=",", row.names=1, header=TRUE)

# Edit the column names if necessary
colnames(input_data) <- if (
    length(
        grep("^X[\\d]",colnames(input_data),perl=TRUE)
    ) != 0
) {# then
    gsub("^X([\\d].*)","\\1",colnames(input_data),perl=TRUE)
} else {
    colnames(input_data)
}
    
# Get groups information
groups<-input_data[,1]
# Get levels for groups
grp_levs<-levels(groups)
# Initialise an empty vector and create a vector of group lengths
grp_len<-c()
for (ii in 1:length(grp_levs)) {
    grp_len<-c(
        # Use the initial vector,
        grp_len,
        # and then append the number of samples in the group
        length(which(groups==levels(groups)[ii]))
    )
}

#
#    Split the matrix by group
#
new_mats<-c()
for (ii in 1:length(grp_levs)) {
    new_mats[ii]<-list(input_data[which(groups==levels(groups)[ii]),])
}

#
#    Calculate the means
#
# For each matrix, calculate the averages per column
submeans<-c()
# Preallocate a matrix for the means
means<-matrix(
    nrow = length(grp_levs),
    ncol = length(colnames(input_data[,-1])),
    dimnames = list(grp_levs,colnames(input_data[,-1]))
)
# Calculate the means for each variable per sample
for (ii in 1:length(new_mats)) {
    submeans[ii]<-list(apply(new_mats[[ii]][,-1],2,mean,na.rm=TRUE))
    means[ii,]<-submeans[[ii]]
}

#
#    Calculate the fold change
#
folds<-matrix(
    nrow=length(means[,1]),
    ncol=length(means[1,]),
    dimnames=list(rownames(means),colnames(means))
)
for (ii in 1:length(means[,1])) {
    for (jj in 1:length(means[1,])) {
        folds[ii,jj]<-if ((means[ii,jj]/means[1,jj])>=1) {
            means[ii,jj]/means[1,jj]
        } else {
            -1/(means[ii,jj]/means[1,jj])
        }
    }
}

#
# Generate the output matrices in .csv format
#
write.csv(means,"means.csv")
write.csv(folds,"fold_change.csv")

# #
# #    Tidy up
# #
# # List all objects
# rm_list$post=ls()
# # Remove objects in rm_list$post that aren't in rm_list$pre
# rm(list=rm_list$post[which(rm_list$pre!=rm_list$post)])
# rm(rm_list)
