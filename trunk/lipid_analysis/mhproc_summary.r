#    mhproc_summary.r
#
#    Author:    Jairus Bowne
#    Purpose:    Generate a summary for MassHunter processing
#                scripts
#
#    Inputs:    Data matrix of concentrations or normalised concentrations
#               Name of data matrix type ("conc" or "norm", respectively)
#    Output:    Data matrix of summaries of compound class (i.e. sum of
#               mean and sem values for each class of compound)
#
#    Notes:    Function used in mhproc.r group of scripts

summary_fn<-function(data_matrix,type) {
    ifelse(length(grep(type,"conc",ignore.case=TRUE))!=0,
        # If true (i.e. type is conc)
        fnum<-4,
        # else test if type is "norm"
        ifelse(length(grep(type,"norm",ignore.case=TRUE))!=0,
            fnum<-5,
            stop(" ## Please check the value of 'type' ##")
        )
    )
    
    #
    #    Create a matrix of means and standard deviations per group
    #
    # Get number of groups
    grp_lvls<-levels(data_matrix$Group)
    # Split the matrix by group
    matrix_list<-c()
    for (ii in 1:length(grp_lvls)) {
        matrix_list[ii]<-list(
            data_matrix[which(data_matrix$Group==grp_lvls[ii]),]
        )
    }
    # For each matrix (list), calculate the averages per column
    sub_mean<-c()
    sub_sd<-c()
    sub_sem<-c()
    
    # Preallocate matrices for the means and standard deviations
    means<-matrix(
        nrow = length(grp_lvls),
        ncol = length(cpd_cols),
        dimnames = list(seq(grp_lvls),cpd_cols)
    )
    sems<-matrix(
        nrow = length(grp_lvls),
        ncol = length(cpd_cols),
        dimnames = list(seq(grp_lvls),cpd_cols)
    )
    # Calculate the means and standard errors of the mean
    for (ii in 1:length(matrix_list)) {
        sub_mean[ii]<-list(
            apply(matrix_list[[ii]][,-c(1:2)],2,mean,na.rm=TRUE
            )
        )
        means[ii,]<-sub_mean[[ii]]
    }
    for (ii in 1:length(matrix_list)) {
        sub_sd[ii]<-list(
            apply(matrix_list[[ii]][,-c(1:2)],2,sd,na.rm=TRUE
            )
        )
        sub_sem[[ii]]<- sub_sd[[ii]]/sqrt(nrow(matrix_list[[ii]]))
        sems[ii,]<-sub_sem[[ii]]
    }
    
    # Insert data type for both matrices
    means<-cbind(grp_lvls,
        rep("Mean",length(grp_lvls)),
        as.data.frame(means)
    )
    colnames(means)<-c("Group","DataType",cpd_cols)
    sems<-cbind(grp_lvls,
        rep("SEM",length(grp_lvls)),
        as.data.frame(sems)
    )
    colnames(sems)<-c("Group","DataType",cpd_cols) 
    # Recreate matrices
    matrix_means<-rbind(means,sems)
    # Sort by group
    matrix_means<-matrix_means[order(matrix_means$Group),]
    # Write output file
    write.csv(matrix_means,
        paste(base_fn,"_",fnum,"b_",
        paste(toupper(substring(type,1,1)),
            tolower(substring(type,2)),
            sep=""
        ),
        "_Means.csv",sep=""),
        row.names=FALSE
    )
        
    #
    #    Inform user of created files
    #
    write(
        paste("        ",base_fn,"_",fnum,"b_",
            paste(toupper(substring(type,1,1)),
                tolower(substring(type,2)),
                sep=""
            ),
        "_Means.csv",
        sep=""
        ),""
    )
    
    #
    #    Compound Class Summaries
    #
    # Prepare empty vectors and matrices for the means
    scls<-colnames(std_row)
    matrix_summary_list_m<-c()
    matrix_m_sums<-matrix(
        nrow = length(matrix_list),
        ncol = length(std_row),
        dimnames = list(seq(grp_lvls),names(std_row))
    )
    # Create lists (of data frames) for each compound class
    for (ii in 1:length(scls)) {
        for (jj in 3:length(colnames(means))) {
            # Create the lists
            matrix_summary_list_m[ii]<-list(
                # Select the columns... 
                means[,which(
                        # ... which have a compound class... 
                        gsub("(.*)(\\(.*)","\\1",
                            colnames(means)[1:length(colnames(means))]
                        )
                        # ... which matches the class in the standard row
                        ==scls[ii]
                        )
                    ]
                )
            }
        }
    # Calculate the sum of the means for each class
    sub_mean_sum<-c()
    for (ii in 1:length(matrix_summary_list_m)) {
        # Test to see if the standard is missing
        testcase<-try(
            apply(matrix_summary_list_m[[ii]],1,sum,na.rm=TRUE), 
            # Don't output to screen if missing
            silent=TRUE
        )
    #  If it's missing it will be a "try-error" class:
        if(class(testcase)=="try-error") {
            # Prepare a set of NaN values to use in place of values
            tmp_vals<-rep(NaN,length(grp_lvls))
            names(tmp_vals)<-seq(1:length(grp_lvls))
            sub_mean_sum[ii]<-list(tmp_vals)
        } else {
            # Otherwise add up the means
            sub_mean_sum[ii]<-list(
                apply(matrix_summary_list_m[[ii]],1,sum,na.rm=TRUE)
            )
        }
        matrix_m_sums[,ii]<-sub_mean_sum[[ii]]
    }
    # Reattach names etc
    mean_sums<-cbind(grp_lvls,
        rep("Mean",length(grp_lvls)),
        as.data.frame(matrix_m_sums)
    )
    colnames(mean_sums)<-c("Group","DataType",colnames(matrix_m_sums))
    # And the same for the standard error of the mean
    matrix_summary_list_sem<-c()
    matrix_sem_sums<-matrix(
        nrow = length(matrix_list),
        ncol = length(std_row),
        dimnames = list(seq(grp_lvls),names(std_row))
    )
    # Create the lists
    for (ii in 1:length(scls)) {
        for (jj in 3:length(colnames(sems))) {
            matrix_summary_list_sem[ii]<-list(           
                sems[,which(
                        gsub("(.*)(\\(.*)","\\1",
                            colnames(sems)[1:length(colnames(sems))]
                        )
                        ==scls[ii]
                    )
                ]
            )
        }
    }

    # Calculate the sum of the standard errors
    sub_sem_sum<-c()
    for (ii in 1:length(matrix_summary_list_sem)) {
        # Test to see if the standard is missing
        testcase<-try(
            apply(matrix_summary_list_sem[[ii]],1,function(x) sqrt(sum(x*x,na.rm=TRUE))), 
            # Don't output to screen if missing
            silent=TRUE)
    #  If it's missing it will be a "try-error" class:
        if(class(testcase)=="try-error") {
                # Prepare a set of NaN values to use in place of values
                tmp_vals<-rep(NaN,length(grp_lvls))
                names(tmp_vals)<-seq(1:length(grp_lvls))
                sub_sem_sum[ii]<-list(tmp_vals)
                } else {
                # Otherwise add up the means
                sub_sem_sum[ii]<-list(
                    apply(matrix_summary_list_sem[[ii]],1,
                        function(x) sqrt(sum(x*x,na.rm=TRUE)))
                    )
                }
        matrix_sem_sums[,ii]<-sub_sem_sum[[ii]]
        }
    # Reattach names etc
    sem_sums<-cbind(grp_lvls,
        rep("SEM",length(grp_lvls)),
        as.data.frame(matrix_sem_sums)
    )
    colnames(sem_sums)<-c("Group","DataType",colnames(matrix_sem_sums))
    # Rejoin summary data to one table
    matrix_summary<-rbind(mean_sums,sem_sums)
    matrix_summary<-matrix_summary[order(matrix_summary$Group),]
    # Write output file
    write.csv(matrix_summary,
        paste(base_fn,"_4c_",
            # Change type string to sentence case
            paste(toupper(substring(type,1,1)),
                tolower(substring(type,2)),
                sep=""
            ),
            "_Summ.csv",
            sep=""
        ),
        row.names=FALSE
    )
    #
    #    Inform user of created files
    #
    write(
        paste("        ",base_fn,"_",fnum,"c_",
            paste(toupper(substring(type,1,1)),
                tolower(substring(type,2)),
                sep=""
            ),
            "_Summ.csv",
            sep=""
        ),""
    )
}