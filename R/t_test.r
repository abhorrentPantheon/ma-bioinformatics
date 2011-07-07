#    t_test.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Performs t-test and gives p-value for two group of samples.
#                If number of sample groups is more than two, function will
#                raise an error
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    Summary table of P-values for each variable (.csv format)
#
#    Notes:    Log tranformation is necessary if the variables are not
#              normally distributed prior to performing this test

# # Determine which variables/objects are present before running script
# rm_list<-list()
# rm_list$pre=ls()

#
#    Prepare the data matrix
#
# Read in the .csv file
in_file<-file.choose()
input_data<-read.csv(in_file, sep=",", row.names=1, header=TRUE)

# Get groups information
groups<-input_data[,1]
# Remove groups for data processing
ttest_data<-input_data[,-1]

# Check data; if appropriate, run script
if (length(levels(groups)) > 2) {
    print ("Number of groups is greater than 2. Exiting.")
} else {
    ttest_data_t<-t(ttest_data)

    # Edit the column names if necessary
    colnames(ttest_data) <- if (
        length(
            grep("^X[\\d]",colnames(ttest_data),perl=TRUE)
        ) != 0
    ) {# then
        gsub("^X([\\d].*)","\\1",colnames(ttest_data),perl=TRUE)
    } else {
        colnames(ttest_data)
    }
    row.names(ttest_data_t)<-colnames(ttest_data)

    #
    #    Prepare the supporting data for the test
    #
    # Determine the number of samples in each group
    # (A is levels(groups)[1])
    nA<-length(which(groups==levels(groups)[1]))
    nB<-length(which(groups==levels(groups)[2]))

    # Separate the groups
    group_A<-t(ttest_data_t[,1:nA])
    group_B<-t(ttest_data_t[,nA+1:nB])
    # Create an empty matrix
    pvals<-matrix(NA,nrow(ttest_data_t),ncol=1)

    #
    #    Perform the t-Test
    #
    for(ii in 1:nrow(ttest_data_t)) {
        pvals[ii,1]<-t.test(group_A[,ii],group_B[,ii])$p.value
    }

    # Prepare row and column labels for output
    row.names(pvals)<-row.names(ttest_data_t)
    colnames(pvals)<-"t-Test P-value"

    #
    # Generate the output matrix in .csv format
    #
    write.csv(pvals,"p_t_test.csv")
    
    # #
    # #    Tidy up
    # #
    # # List all objects
    # rm_list$post=ls()
    # # Remove objects in rm_list$post that aren't in rm_list$pre
    # rm(list=rm_list$post[which(rm_list$pre!=rm_list$post)])
    # rm(rm_list)
}