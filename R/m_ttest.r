#    m_ttest.r
#
#    Author:    Alysha M De Livera, Jairus Bowne, Amsha Nahid
#    Purpose:    Performs moderated one sample, two sample
#                and paired t tests.
#                
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    

# Determine which variables/objects are present before running script
rm_list<-list()
rm_list$pre=ls()

# Load required libraries
tryCatch(
    library(limma), 
    error=function(err) {
        # if this produces an error:
        source("http://bioconductor.org/biocLite.R")
        biocLite("limma")
        library(limma)
    }
)
mod_ttest<-function(input_matrix,paired=FALSE,...) {   
    # Extract group information
    groups<-input_matrix[,1]  
    # Exclude group information for the test
    ttest_data<-input_matrix[,-1] 
    # Length of the first group
    nA<-length(which(groups==levels(groups)[1])) 
    # Length of the second group
    nB<-length(which(groups==levels(groups)[2]))

    # If the second group doesn't exist, prepare data for one sample test
    if (nB==0) {
        #Matrix for one sample or unpaired test
        diffs<-t(ttest_data)
        gmod<-matrix(1,nr=ncol(diffs),nc=1) 
        design<-model.matrix(~gmod-1)
    } else {
        group_A<-t(ttest_data[(1:nA),])
        group_B<-t(ttest_data[(nA+1:nB),])    
        if (paired) {
            # Transformed data for a paired sample test
            diffs<-group_A-group_B   
            #Matrix for one sample or paired tests
            gmod<-matrix(1,nr=ncol(diffs),nc=1) 
            design<-model.matrix(~gmod-1)
        } else {
            # Transformed data for a two sample test
            diffs<-cbind(group_A,group_B)
            # Matrix for a two sample test
            gmod<- factor(rep(letters[1:2],c(nA,nB)))  
            # '~' separates the left & right sides for models
            # (left is optional; see ?tilde for help)
            design<-model.matrix(~gmod)
        #} else { # i.e. if !paired and nB==0
        }
    }
    
    # Perform moderated tests
    fit <-lmFit(diffs,design) 
    eb <- eBayes(fit)
    # Extract moderated p-values
    modpvals<-eb$p.value  
    modpvals<-matrix(modpvals[,ncol(modpvals)],nc=1)
    row.names(modpvals)<-colnames(ttest_data)
    colnames(modpvals)<-"Moderated t-Test P-value"
    print(modpvals)
    # Create a csv file with metabolite names and p-values
    write.csv(modpvals,"Mod_p_t_test.csv") 
}

# Read in the .csv file
in_file<-file.choose()
input_data<-read.csv(in_file,sep=",",header=TRUE,row.names=1)

# Turn off warnings temporarily
options(show.error.messages=FALSE,warn=-1)
# Determine whether or not the test is for paired samples

input=readline(
    " ?? Are the groups paired (perform paired t-Test)? (y/n/c): "
)
if (length(grep(input,"y",ignore.case=TRUE))!=0) {
    pair_val=TRUE
} else if (length(grep(input,"n",ignore.case=TRUE))!=0) {
    pair_val=FALSE
} else if (length(grep(input,"c",ignore.case=TRUE))!=0) {
    write(" ## Exiting script.","")
    stop()
} else {
    write(" ## Please press y, n or c next time. Exiting...","")
    stop()
}
# Turn warnings back on
options(show.error.messages=TRUE,warn=0)
# Perform the moderated t-test
mod_ttest(input_data,paired=pair_val)

#
#    Tidy up
#
# List all objects
rm_list$post=ls()
# Remove objects in rm_list$post that aren't in rm_list$pre
rm(list=rm_list$post[which(rm_list$pre!=rm_list$post)])
rm(rm_list)
