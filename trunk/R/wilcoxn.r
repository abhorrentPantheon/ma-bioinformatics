#    wilcoxn.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Performs Wilcoxon signed rank test and gives value for two
#                groups of samples. If number of sample groups is more than
#                two, function will raise an error
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    Summary table of P-values for each variable (.csv format)

#
#    Prepare the data matrix
#
# Read in the .csv file
data<-read.csv("input.csv", sep=",", row.names=1, header=TRUE)
# Get groups information
groups<-data[,1]
# Remove groups for data processing
wilcoxon_data<-data[,-1]


# Check data; if appropriate, run script
if (length(levels(groups))>2) print("Number of groups is greater than 2") else {
    wilcoxon_data_t<-t(wilcoxon_data)# Tidy up the column names

    # Edit the column names if necessary
    colnames(wilcoxon_data) <- if 
        (length(grep("^X[\\d]",colnames(wilcoxon_data),perl=TRUE)) != 0) # then
        {gsub("^X([\\d].*)","\\1",colnames(wilcoxon_data),perl=TRUE)} else
        {colnames(wilcoxon_data)}
    row.names(wilcoxon_data_t)<-colnames(wilcoxon_data)

    #
    #    Prepare the supporting data for the test
    #
    # Determine the number of samples in each group
    nA<-length(which(groups==levels(groups)[1]))
    nB<-length(which(groups==levels(groups)[2]))

    # Separate the groups
    group_A<-t(wilcoxon_data_t[,1:nA])
    group_B<-t(wilcoxon_data_t[,nA+1:nB])
    # Create an empty matrix
    pvals<-matrix(NA,nrow(wilcoxon_data_t),ncol=1)

    #
    #    Perform the Wilcoxon Rank Sum Test
    #
    for(ii in 1:nrow(wilcoxon_data_t)) {
        pvals[ii,1]<-wilcox.test(group_A[,ii],group_B[,ii])$p.value
        }

    # Prepare row and column labels for output
    row.names(pvals)<-row.names(wilcoxon_data_t)
    colnames(pvals)<-"Wilcoxon Rank Sum Test P-value"

    #
    # Generate the output matrix in .csv format
    #
    write.csv(pvals,"p_wilcoxon_test.csv")
    }