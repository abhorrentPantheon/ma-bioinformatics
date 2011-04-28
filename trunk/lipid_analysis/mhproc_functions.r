#    mhproc_summary.r
#
#    Author:    Jairus Bowne
#    Purpose:    Functions used in mhproc.r group of scripts

#
#    Means and standard deviations function
#
# param data_matrix:    Matrix to summarise
# type data_matrix:    Data frame
#
# param type:    Type of input matrix (conc, norm)
# type type:    Character string
#
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
    #    Compound Class Summary function
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
        paste(base_fn,"_",fnum,"c_",
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

#
#    Change margin sizes (bottom, left, right)
#
# param x_labels:    Labels to be used for the x-axis
# type x_labels:    Character vector
#
# param legend_text:    Labels to be used in the legend
# type legend_text:    Character vector
#
# param margin_data:    Data that will be plotted
# type margin_data:    Data frame
#
# param cex_val:    Size of characters (unimplemented)
# type cex_val:    Integer
#
margin_bender<-function(
    x_labels,
    legend_text,
    margin_data,
    cex_val=par()$cex
) {
    # Largest number
    max_num<-floor(max(margin_data[,-c(1:2)],na.rm=TRUE))
    # Largest entry in the legend
    max_leg<-legend_text[
        which(nchar(legend_text)==max(nchar(legend_text)))
    ]
    # If there is a tie, take the first
    if (length(max_leg)>1) {
        max_leg<-max_leg[1]
    }
    # Largest x-axis label
    max_xlb<-x_labels[which(nchar(x_labels)==max(nchar(x_labels)))]
    if (length(max_xlb)>1) {
        max_xlb<-max_xlb[1]
    }
    # Get the size of each in inches, multiply by 5
    # (as par()$mar/par()$mai = 5,5,5,5
    new_b_mar<-5.5*strwidth(max_xlb,units="i")
    new_l_mar<-5.5*strwidth(max_num,units="i")
    new_r_mar<-5.5*strwidth(max_leg,units="i")+0.4*cex_val*nchar(max_leg)

    # Output a vector with the new bottom, left and right margins
    return(
        c(
            new_b_mar = new_b_mar,
            new_l_mar = new_l_mar,
            new_r_mar = new_r_mar
        )
    )
    dev.off()
}

#
#    Split matrix by compound class
#
# param input_matrix:    Data to be prepared for producing images
# type input_matrix:    Data frame
data_prepper<-function(input_matrix) {
    # Set any values of Inf, NA to zero (no internal standard usual cause)
    input_matrix[input_matrix==Inf]<-0
    input_matrix[is.na(input_matrix)]<-0

    # Create a dictionary of compound classes
    cls_dict<-list()
    cls_dict$Name=unique(gsub("(.*)(\\(.*)","\\1",cpd_cols,perl=TRUE))
    for (kk in 1:length(cls_dict$Name)) {
        cnt<-0
        for (ii in 1:length(cpd_cols)) {
            curr_cls<-gsub("(.*)(\\(.*)","\\1",cpd_cols[ii],perl=TRUE)
            if (curr_cls==cls_dict$Name[kk]) {
                cnt<-cnt+1
            }
            cls_dict$Count[kk]<-cnt
        }
    }

    # Split the matrix based on compound classes
    split_cpds<-list()
    cls_max<-list()
    for (cls in cls_dict$Name) {
        tmp<-c()
        for (ii in 1:length(cpd_cols)) {
            curr_cls<-gsub("(.*)(\\(.*)","\\1",cpd_cols[ii],perl=TRUE)
            if (cls==curr_cls) {
                tmp<-cbind(tmp,input_matrix[,ii+2])
                colnames(tmp)[dim(tmp)[2]]<-cpd_cols[ii]
            }
        }
        col_num<-dim(tmp)[2]
        # Find maximum
        max_mean<-max(tmp[c(which(input_matrix[,2]=="Mean")),])
        # Find where the maximum is in tmp, return the array index
        max_mean_loc<-which(tmp==max_mean, arr.ind=TRUE)
        # If the cls has no internal std, it will have more than one max
        if (length(max_mean_loc)>1) {
            max_mean_loc<-max_mean_loc[1,]
        }
        # Max to plot is max_mean plus it's SEM (below mean)
        cls_max[[cls]]=max_mean+tmp[max_mean_loc[1]+1,max_mean_loc[2]]
        # If the class is all zeroes (e.g. no internal standard; values
        # are all Inf & NaN in conc_mat), take only the first
        if (length(cls_max[[cls]])>1) {
            cls_max[[cls]]=cls_max[[cls]][1]
        }
        # If the class is too large, split it
        if (col_num>20) {
            brk_num<-max(2,floor(col_num/14))
            col_split<-cut(1:col_num,
                brk_num,
                labels=letters[1:brk_num]
            )
            # Append a tag to identify it (a:z)
            for (jj in 1:brk_num) {
                new_cls<-paste(cls,"_",letters[jj],sep="")
                sub_tmp<-tmp[,which(col_split==letters[jj])]
                split_cpds[[new_cls]]=cbind(input_matrix[,1:2],sub_tmp)
            }
        } else {
            split_cpds[[cls]]=cbind(input_matrix[,1:2],tmp)
        }
    }
    cls_list<-names(split_cpds)
        
    # Get groups information
    grp_lvls<-levels(input_matrix$Group)

    # Tidy the matrix
    colnames(input_matrix)[-c(1:2)]<-cpd_cols
    return(
        list(
            split_cpds = split_cpds,
            grp_lvls = grp_lvls,
            cls_list = cls_list,
            cls_max = cls_max
        )
    )
}

#
#
#

get_pattern_list<-function(split_data,cpd_cls,input_list,trans,bw) {
    # Transposed matrix
    if (trans==TRUE) {
        # Names for this compound class
        cpd_names<-names(split_data[[cpd_cls]])[-c(1:2)]
        # If using colours
        if (bw==FALSE) {
            # Determine whether colours or compounds is the longer list
            # If it's the colours list
            if(length(cpd_cols)<length(col_list[[1]])) {
                pattern_list_used<-list(
                    col=col_list$col[1:length(cpd_names)],
                    density=col_list$density[1:length(cpd_names)],
                    angle=col_list$angle[1:length(cpd_names)]
                )
            } else {
                # Find how many times to repeat it
                pattern_reps<-ceiling(
                    length(cpd_names)/length(col_list[[1]])
                )
                # Make the list longer
                pattern_list_long<-list(
                    col=rep(col_list$col,pattern_reps),
                    density=rep(col_list$density,pattern_reps),
                    angle=rep(col_list$angle,pattern_reps)
                )
                # Then cut as necessary
                pattern_list_used<-list(
                    col=pattern_list_long$col[1:length(cpd_names)],
                    density=pattern_list_long$density[1:length(cpd_names)],
                    angle=pattern_list_long$angle[1:length(cpd_names)]
                )
            }
        # Otherwise for b&w
        } else {
            if(length(cpd_cols)<length(bw_list[[1]])) {
                bw_list_used<-bw_list[1:length(cpd_names)]
            } else {
                pattern_reps<-ceiling(
                    length(cpd_names)/length(bw_list[[1]])
                )
                pattern_list_long<-list(
                    col=rep(bw_list$col,pattern_reps),
                    density=rep(bw_list$density,pattern_reps),
                    angle=rep(bw_list$angle,pattern_reps)
                )
                pattern_list_used<-list(
                    col=pattern_list_long$col[1:length(cpd_names)],
                    density=pattern_list_long$density[1:length(cpd_names)],
                    angle=pattern_list_long$angle[1:length(cpd_names)]
                )
            }
        }
    # Else for non-transposed matrix
    } else {
        # Colours
        if (bw==FALSE) {
            if(length(grp_lvls)<length(col_list)) {
                pattern_list_used<-list(
                    col=col_list$col[1:length(grp_lvls)],
                    density=col_list$density[1:length(grp_lvls)],
                    angle=col_list$angle[1:length(grp_lvls)]
                )
            } else {
                pattern_reps<-ceiling(
                    length(grp_lvls)/length(col_list[[1]])
                )
                pattern_list_long<-list(
                    col=rep(col_list$col,pattern_reps),
                    density=rep(col_list$density,pattern_reps),
                    angle=rep(col_list$angle,pattern_reps)
                )
                pattern_list_used<-list(
                    col=pattern_list_long$col[1:length(grp_lvls)],
                    density=pattern_list_long$density[1:length(grp_lvls)],
                    angle=pattern_list_long$angle[1:length(grp_lvls)]
                )
            }
        # Monochrome
        } else {
            if(length(grp_lvls)<length(bw_list)) {
                pattern_list_used<-list(
                    col=bw_list$col[1:length(grp_lvls)],
                    density=bw_list$density[1:length(grp_lvls)],
                    angle=bw_list$angle[1:length(grp_lvls)]
                )
            } else {
                pattern_reps<-ceiling(
                    length(grp_lvls)/length(bw_list[[1]])
                )
                pattern_list_long<-list(
                    col=rep(bw_list$col,pattern_reps),
                    density=rep(bw_list$density,pattern_reps),
                    angle=rep(bw_list$angle,pattern_reps)
                )
                pattern_list_used<-list(
                    col=pattern_list_long$col[1:length(grp_lvls)],
                    density=pattern_list_long$density[1:length(grp_lvls)],
                    angle=pattern_list_long$angle[1:length(grp_lvls)]
                )
            }
        }
    }
    return(pattern_list_used)
}


#
#    Plot compounds by class and group
#
# param flat_data:    The whole data matrix that will be plotted
# type flat_data:    Data frame
#
# param split_data:    flat_data data frame that has been split 
#                      into compound classes
# type split_data:    List containing data frames
#
# param x_labels:    Labels to be used for the x-axis
# type x_labels:    Character vector
#
# param legend_text:    Labels to be used in the legend
# type legend_text:    Character vector
#
# param trans:    Whether or not to transpose the matrix for plotting
# type trans:    Logical
#
# param device_type:    Which device to use (x11, jpeg, png, tiff)
# type device_type:    Character string
#
multibarplot<-function(flat_data,
    split_data,
    trans=TRUE,
    norm_plot=FALSE,
    device_type="x11",
    bw=FALSE
) {
    # Draw a plot for each compound class in the class list
    for (cls in cls_list) {
        # Ensure defaults are set
        par(par_defs)
        if (trans==TRUE) {
            mar_expand<-margin_bender(
                levels(split_data[[cls]][,1]),
                colnames(split_data[[cls]][-c(1:2)]),
                margin_data=split_data[[cls]]
            )
        } else {
                mar_expand<-margin_bender(
                colnames(split_data[[cls]][-c(1:2)]),
                levels(split_data[[cls]][,1]),
                margin_data=split_data[[cls]]
            )
        }
        # Turn off warnings (in regards to arrow height being zero)
        options(warn=-1)
        mar_max<-max(par()$mar)+max(mar_expand)
        if (trans==TRUE) {
            img_fn<-paste(
                base_fn,
                "_",
                "img_",
                cls,
                if (norm_plot==FALSE) {
                    "_Groups"
                } else {
                    "_Norm_Groups"
                },
                sep=""
            )
        } else {
            img_fn<-paste(
                base_fn,
                "_",
                "img_",
                cls,
                if (norm_plot==FALSE) {
                    "_Compounds"
                } else {
                    "_Norm_Compounds"
                },
                sep=""
            )
        }
        #dev.off()
        # Device-specific settings
        if (device_type=="x11") {
            x11(width=mar_max*(2**.5),height=mar_max)
        } else if (device_type=="jpg") {
            fname<-paste(img_fn,".jpg",sep="")
            jpeg(fname,
                units="in",
                    width=mar_max*(2**.5),
                    height=mar_max,
                res=100,
                bg="white",
                quality=100
            )
        } else if (device_type=="png") {
            fname<-paste(img_fn,".png",sep="")
            png(fname,
                units="in",
                    width=mar_max*(2**.5),
                    height=mar_max,
                res=100,
                bg="white"
            )
        } else if (device_type=="tif") {
            fname<-paste(img_fn,".tif",sep="")
            tiff(fname,
                units="in",
                    width=mar_max*(2**.5),
                    height=mar_max,
                res=100,
                bg="white",
                compression="none"
            )
        } else {
            stop(
                paste("Wrong device type given.",
                    "Please use one of:",
                    "\"x11\", \"jpg\", \"png\", \"tif\""
                )
            )
        }
        # If trans is true, don't need to expand the bottom margin
        if (trans==TRUE) {
            par(
                mar=par()$mar+c(
                    # bottom
                    2,
                    # left
                    mar_expand[2],
                    # top
                    0,
                    # right
                    mar_expand[3]
                ),
                las=1,                     # axis text horizontal
                xpd=TRUE
            )
            # Get patterns to use in plots
            if (bw==FALSE) {
                pattern_list_used<-get_pattern_list(
                    split_cpds,cls,col_list,trans=TRUE,bw=FALSE
                )
            } else {
                pattern_list_used<-get_pattern_list(
                    split_cpds,cls,bw_list,trans=TRUE,bw=TRUE
                )
            }

            # Select the data to plot
            bp_data<-t(
                split_data[[cls]][
                    which(split_data[[cls]]$DataType=="Mean"),
                ][,-c(1:2)]
            )
            # Select the data for error bars
            sem_data<-t(
                split_data[[cls]][
                    which(split_data[[cls]]$DataType=="SEM"),
                ][,-c(1:2)]
            )
            # Get labels for clusters
            bp_labels<-levels(split_data[[cls]][,1])
            # Title of plot
            if (norm_plot==FALSE) {
                plot_title<-paste(
                    "Concentration by Group",
                    paste("Subset:",
                        cls
                    ),
                    sep="\n"
                )
            } else {
                plot_title<-paste(
                    "Normalised Concentration by Group",
                    paste("Subset:",
                        cls
                    ),
                    sep="\n"
                )
            }
        # Untranposed
        } else {
            par(
                mar=par()$mar+c(
                    # bottom
                    mar_expand[1],
                    # left
                    mar_expand[2],
                    # top
                    0,
                    # right
                    mar_expand[3]+2
                ),
                las=2,                     # axis text perpendicular
                xpd=TRUE                   # expand plot area
            )
            # Get patterns to use in plots
            if (bw==FALSE) {
                pattern_list_used<-get_pattern_list(
                    split_cpds,cls,col_list,trans=FALSE,bw=FALSE
                )
            } else {
                pattern_list_used<-get_pattern_list(
                    split_cpds,cls,bw_list,trans=FALSE,bw=TRUE
                )
            }
            # Plot data
            bp_data<-as.matrix(
                split_data[[cls]][
                    which(split_data[[cls]]$DataType=="Mean"),
                ][,-c(1:2)]
            )
            # Error bars
            sem_data<-as.matrix(
                split_data[[cls]][
                    which(split_data[[cls]]$DataType=="SEM"),
                ][,-c(1:2)]
            )
            # Get labels for clusters
            bp_labels<-colnames(bp_data)
            # Title of plot
            if (norm_plot==FALSE) {
                plot_title<-paste(
                    "Concentration by Compound Class",
                    cls,
                    sep="\n"
                )
            } else {
                plot_title<-paste(
                    "Normalised Concentration by Compound Class",
                    cls,
                    sep="\n"
                )
            }
        }
        # y-axis limits
        if (length(grep("_",cls,perl=TRUE))!=0) {
            y_max<-cls_max[[gsub("(.+)_.","\\1",cls,perl=TRUE)]]
        } else {
            y_max<-cls_max[[cls]]
        }
        # If y_max is zero, will return an error; set it to 1 in that case
        if (y_max==0) {
            y_max=1
        }

        # Plot the data
        bp_vals<-barplot(bp_data,          # data to plot
            beside=TRUE,                   # plot as clusters
            col=pattern_list_used$col,     # patterns to use
                density=pattern_list_used$density,
                angle=pattern_list_used$angle,
            names.arg=bp_labels,           # cluster labels
            main=plot_title,               # Title
            xlab="", ylab="",              # suppress axis titles
            ylim=c(0,y_max)                # range for y-axis
        )

        # Custom y-axis label
        title(ylab="Concentration (nmol)",
            line=ceiling(mar_expand[2]+par()$cin[2])+2
        )
        # Standard error (+)
        arrows(bp_vals,                        # x0
            bp_data,                           # y0
            bp_vals,                           # x1
            bp_data+sem_data,                  # y1
            angle=90,                          # angle on arrowhead
            code=2,                            # head end
            length=0.0275*par()$cex            # length of point
        )
        # Standard error (-)
        arrows(bp_vals,                        # x0
            bp_data,                           # y0
            bp_vals,                           # x1
            bp_data-sem_data,                  # y1
            angle=90,                          # angle on arrowhead
            code=2,                            # head end
            length=0.0275*par()$cex            # length of point
        )
        
        # Custom x-axis label and legend
        if (trans==TRUE) {
            title(xlab="Group",
                line=ceiling(mar_expand[1]+par()$cin[1])+1
            )
            legend("left",                 # position of legend
                inset=1.02,                # 1 is RHS of plot area
                names(                     # labels on legend
                    split_data[[cls]]      # (only the names of the
                )[-c(1:2)],                # cpds in this cpd class)
                fill=pattern_list_used$col,# patterns to use
                    density=pattern_list_used$density,
                    angle=pattern_list_used$angle
            )
        } else {
            title(xlab="Compound",
                line=ceiling(mar_expand[1]+par()$cin[1])+1
            )
            legend("left",                 # position of legend
                inset=1.02,                # 1 is RHS of plot area
                grp_lvls,                  # labels on legend
                fill=pattern_list_used$col,# patterns to use
                    density=pattern_list_used$density,
                    angle=pattern_list_used$angle
            )
        }
        # Re-enable warnings
        options(warn=0)
        # Reset graphical parameters
        par(par_defs)
        # If it's not the screen device, turn it off (i.e. write file)
        if (device_type!="x11") dev.off()
    }
}

#
#    Plot compound class summaries
#
# param input_matrix:    Data to be plotted
# type input_matrix:    Data frame
#
#
#
#
#
#
summ_plot<-function(input_matrix,
    trans=TRUE,
    norm_plot=FALSE,
    device_type="x11",
    bw=FALSE
) {
    # Ensure defaults are set
    par(par_defs)
    # Get margin values
    if (trans==TRUE) {
        mar_expand<-margin_bender(levels(input_matrix[,1]),
            colnames(input_matrix[-c(1:2)]),
            margin_data=input_matrix
        )
    } else {
            mar_expand<-margin_bender(colnames(input_matrix[-c(1:2)]),
            levels(input_matrix[,1]),
            margin_data=input_matrix
        )
    }
    # Turn off warnings (in regards to arrow height being zero)
    options(warn=-1)
    mar_max<-max(par()$mar)+max(mar_expand)
    # Get y-axis maximum
    max_mean<-max(input_matrix[,-c(1:2)],na.rm=TRUE)
    max_mean_loc<-which(input_matrix==max_mean,arr.ind=TRUE)
    # Max to plot is max_mean plus it's SEM (in the row below the mean)
    y_max<-max_mean+input_matrix[max_mean_loc[1]+1,max_mean_loc[2]]
    # Device-specific settings
    if (trans==TRUE) {
        img_fn<-paste(
            base_fn,
            if (norm_plot==FALSE) {
                "_img_GroupsSumm"
            } else {
                "_img_NormGroupsSumm"
            },
            sep=""
        )
    } else {
        img_fn<-paste(
            base_fn,
            if (norm_plot==FALSE) {
                "_img_CompoundsSumm"
            } else {
                "_img_NormCompoundsSumm"
            },
            sep=""
        )
    }
    if (device_type=="x11") {
        x11(width=mar_max*(2**.5),height=mar_max,)
    } else if (device_type=="jpg") {
        fname<-paste(img_fn,".jpg",sep="")
        jpeg(fname,
            units="in",
                width=mar_max*(2**.5),
                height=mar_max,
            res=100,
            bg="white",
            quality=100
        )
    } else if (device_type=="png") {
        fname<-paste(img_fn,".png",sep="")
        png(fname,
            units="in",
                width=mar_max*(2**.5),
                height=mar_max,
            res=100,
            bg="white"
        )
    } else if (device_type=="tif") {
        fname<-paste(img_fn,".tif",sep="")
        tiff(fname,
            units="in",
                width=mar_max*(2**.5),
                height=mar_max,
            res=100,
            bg="white",
            compression="none"
        )
    } else {
        stop(
            paste("Wrong device type given.",
                "Please use one of:",
                "\"x11\", \"jpg\", \"png\", \"tif\""
            )
        )
    }
    # If trans is true, don't need to expand the bottom margin
    if (trans==TRUE) {
        par(
            mar=par()$mar+c(
                # bottom
                2,
                # left
                mar_expand[2],
                # top
                0,
                # right
                mar_expand[3]+1
            ),
            xpd=TRUE
        )
        bp_data<-t(input_matrix[
            which(input_matrix$DataType=="Mean"),-c(1:2)
        ])
        # Error bars
        sem_data<-t(input_matrix[
            which(input_matrix$DataType=="SEM"),-c(1:2)
        ])
        bp_labels<-grp_lvls
        cls_names<-colnames(input_matrix)[-c(1:2)]
        
        # Get patterns to use in plots
        if (bw==FALSE) {
            pattern_list_used<-get_pattern_list(trans=TRUE,bw=FALSE)
        } else {
            pattern_list_used<-get_pattern_list(trans=TRUE,bw=TRUE)
        }
        
        if (norm_plot==FALSE) {
            plot_title<-"Compound Class Summary\nBy Group"
        } else {
            plot_title<-"Normalised Compound Class Summary\nBy Group"
        }
        
    } else {
        par(
            mar=par()$mar+c(
                # bottom
                mar_expand[1],
                # left
                mar_expand[2],
                # top
                0,
                # right
                mar_expand[3]
            ),
            xpd=TRUE
        )
        # Data to plot
        bp_data<-as.matrix(input_matrix[
            which(input_matrix$DataType=="Mean"),-c(1:2)
        ])
        # Error bars
        sem_data<-as.matrix(input_matrix[
            which(input_matrix$DataType=="SEM"),-c(1:2)
        ])
        bp_labels<-colnames(bp_data)
        
        # Get patterns to use in plots
        if (bw==FALSE) {
            pattern_list_used<-get_pattern_list(trans=FALSE,bw=FALSE)
        } else {
            pattern_list_used<-get_pattern_list(trans=FALSE,bw=TRUE)
        }
        
        if (norm_plot==FALSE) {
            plot_title<-"Compound Class Summary\nBy Class"
        } else {
            plot_title<-"Normalised Compound Class Summary\nBy Class"
        }
    }

    bp_vals<-barplot(bp_data,              # data to plot
        beside=TRUE,                       # plot as clusters
        col=pattern_list_used$col,         # patterns to use
            density=pattern_list_used$density,
            angle=pattern_list_used$angle,
        names.arg=bp_labels,               # cluster labels
        las=1,                             # axis text horizontal
        main=plot_title,                   # Title
        xlab="", ylab="",                  # suppress axis titles
        ylim=c(0,y_max)                    # range for y-axis
    )
    
    # Custom y-axis label
    title(ylab="Concentration (nmol)",
        line=ceiling(mar_expand[2]+par()$cin[2])+2
    )
    # Standard error (+)
    arrows(bp_vals,                        # x0
        bp_data,                           # y0
        bp_vals,                           # x1
        bp_data+sem_data,                  # y1
        angle=90,                          # angle on arrowhead
        code=2,                            # head end
        length=0.0275*par()$cex            # length of point
    )
    # Standard error (-)
    arrows(bp_vals,                        # x0
        bp_data,                           # y0
        bp_vals,                           # x1
        bp_data-sem_data,                  # y1
        angle=90,                          # angle on arrowhead
        code=2,                            # head end
        length=0.0275*par()$cex            # length of point
    )
    if (trans==TRUE) {
        # Custom x-axis label
        title(xlab="Group",
            line=ceiling(mar_expand[1]+par()$cin[1])+1
        )
        
        legend("left",                     # position of legend
            inset=1.02,                    # 1 is RHS of plot area
            cls_names,                     # labels on legend
            fill=pattern_list_used$col,    # patterns to use
                density=pattern_list_used$density,
                angle=pattern_list_used$angle
        )
    } else {
        title(xlab="Compound",
            line=ceiling(mar_expand[1]+par()$cin[1])+1
        )
        
        legend("left",                     # position of legend
            inset=1.02,                    # 1 is RHS of plot area
            grp_lvls,                      # labels on legend
            fill=pattern_list_used$col,    # patterns to use
                density=pattern_list_used$density,
                angle=pattern_list_used$angle
        )
    }
    # Re-enable warnings
    options(warn=0)
    # Reset graphical parameters
    par(par_defs)
    # If it's not the screen device, turn it off (i.e. write file)
    if (device_type!="x11") dev.off()
}
