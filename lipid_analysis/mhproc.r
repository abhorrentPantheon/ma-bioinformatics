#    mhproc.r
#
#    Author:    Jairus Bowne
#    Purpose:    Preprocessing of MassHunter output files.
#
#    Input:      MassHunter .csv output file
#    Output(s):     1) RT matrix                (.csv)
#                   2) Areas matrix             (.csv)
#                   3) Standards areas and Rf   (.csv)
#                   4) Concentrations
#                       a)    Matrix            (.csv)
#                       b)    Means             (.csv)
#                       c)    Summary           (.csv)
#                   If norm=TRUE, will also produce:
#                   5) Normalised concentrations
#                       a)    Matrix            (.csv)
#                       b)    Means             (.csv)
#                       c)    Summary           (.csv)
#
#    Notes:    R re-write of Python script mhproc_new.py
#              The output .csv files can be imported into Excel using the
#              import_multi_csv VBA script.

# Operates as a function
# Arguments:
#   param filename Name of file to parse
#   type filename: string
#
#   param std_conc: Concentration of standard mix (nmol)
#   type std_conc: numeric
#
#   param keep_IS: Whether or not to retain standards in output matrices
#   type keep_IS: boolean
#
#   param norm: Whether or not to normalise the data by
#               the concentration of a standard
#   type norm: boolean
#
#   param norm_cpd: The compound class to use for normalisation (LPC,
#                   CE, etc. Only use the class, not the full name)
#   type norm_cpd: string

mhproc<-function(filename,
    std_conc=5000,
    keep_IS=TRUE,
    norm=FALSE, 
    norm_cpd="") {

# For cutnpaste (remove for final):
# filename<-"new_pos.csv"
# filename<-"PG_CE_301110.csv"
# filename<-"Dharmica-PC_SM_301110_1_mala.csv"
# std_conc<-5000
# keep_IS=TRUE
# filename<-"ctest.csv";std_conc<-5000;keep_IS=TRUE;norm=FALSE

# Show usage information
write(paste(" ** Using file",filename,"as input."),"")
write(paste(" ** Standard concentration:",std_conc,"nmol."),"")
if (keep_IS==TRUE) {
    write(" ** Keeping standard compounds in final matrix.","")
    } else {
    write(" ** Deleting standard compounds from final matrix.","")
    }
if (norm==TRUE) {
    write(" ** Normalisation will be performed.","")
    } else {
    write(" ** No normalisation will be performed.","")
    }
# Save base file name for reuse
base_fn<-gsub("(.*[^\\.])\\.csv","\\1",filename,perl=TRUE)
# Read in file
basefile<-read.csv(filename,header=FALSE,sep=",",quote="\"")

#
#    Filter the data set (remove unwanted columns) 
#
prefilt<-basefile[,-c(1:3,5:7)]

# Define columns and generate headers
area_cols<-which(prefilt[2,]=="Area")
rt_cols<-which(prefilt[2,]=="RT")
headers<-t(prefilt[1,rt_cols])
cpd_lbl<-rep(NA,length(headers))

for (ii in 1:length(headers)) {
    # Remove the spaces from...
    cpd_lbl[ii]<-gsub(" ","",
        # ... the name of the compound without " Results" at the end
        gsub("(.*[^\\ ])\\ Results","\\1",headers[ii],perl=TRUE),
        perl=TRUE)
    }

# Insert Sample and Group label headers
cpd_lbl<-append(cpd_lbl,c("Sample","Group"),after=0)

# Remove empty and blank rows
mt_index<-which(prefilt[,1]=="")
blank_index<-grep("blank",prefilt[,1],ignore.case=TRUE)
filt<-prefilt[-c(mt_index,blank_index),]
filt<-as.matrix(prefilt[-c(mt_index,blank_index),])
# Replace any blanks with zero
filt[filt==""]<-0
filt<-as.data.frame(filt)

# Provide some feedback
write(paste(" -> Removed ", length(mt_index), " empty row(s).",sep=""), "")
write(paste(" -> Removed ", length(blank_index), " blank row(s).",sep=""), "")

#
#    Create RT and Area matrices
#
# Generate sample and group names
# Samples
Sample<-gsub("(.*)\\.d","\\1",filt[-1,1],perl=TRUE)
# Groups
Group<-gsub("()_.*","\\1",Sample,perl=TRUE)

# RT matrix
filt_rt<-filt[-1,rt_cols]
rt_mat<-cbind(Sample,Group,filt_rt)

# Area matrix
filt_area<-filt[-1,area_cols]
area_mat<-cbind(Sample,Group,filt_area)

# Replace column headers
colnames(rt_mat)<-cpd_lbl
colnames(area_mat)<-cpd_lbl

# Find Standard row
std_rownum<-grep("std",area_mat[,1],ignore.case=TRUE)
std_cols<-grep("IS",colnames(area_mat))
std_row<-area_mat[std_rownum,std_cols]
# Get compound class names from standards
std_cls<-gsub("(.*)(\\(.*)","\\1",colnames(area_mat)[std_cols],perl=TRUE)
# Tidy the vector
if (length(std_row)==1) {
    std_row<-as.data.frame(std_row)
    }
rownames(std_row)<-"Area"
# Use compound classes for the column names
names(std_row)<-std_cls

if (keep_IS==TRUE) {
    area_mat<-area_mat[-std_rownum,]
    } else {
    # Remove standards from Area matrix
    area_mat<-area_mat[-std_rownum,-std_cols]
    }
# Sort columns
rt_mat<-rt_mat[,c(names(rt_mat[1:2]),
    sort(names(rt_mat[3:ncol(rt_mat)])))]
area_mat<-area_mat[,c(names(area_mat[1:2]),
    sort(names(area_mat[3:ncol(area_mat)])))]
    
# Store names for later use
area_heads<-names(area_mat)

#
#    Output RT and Area matrices as .csv
#
write.csv(rt_mat,paste(base_fn,"_1_RT.csv",sep=""),row.names=FALSE)
write.csv(area_mat,paste(base_fn,"_2_Area.csv",sep=""),row.names=FALSE)
write(" -> Created the following files:","")
write(paste("        ",base_fn,"_1_RT.csv",sep=""),"")
write(paste("        ",base_fn,"_2_Area.csv",sep=""),"")

#
#    Read in fresh matrices
#

#####################################################################
# The reason for this is that the original data frame is stored     #
# in such a way that the values in the data frame are factors, not  #
# numerics. This causes a large number of headaches when trying to  #
# manipulate the values (i.e. dividing a value by another)          #
#####################################################################

# Area matrix
area_mat<-read.csv(paste(base_fn,"_2_Area.csv",sep=""),
    header=TRUE,sep=",",quote="\"")
# Fix headers (read.csv converts all punctuation to ".")
colnames(area_mat)<-area_heads
# Prepare and re-read standard row
write.csv(std_row,"std_row.csv",row.names=TRUE)
std_row<-read.csv("std_row.csv",sep=",",header=TRUE,row.names=1)
# Remove the temp file (we will save a more informative version later)
file.remove("std_row.csv")

#
#    Prepare and output Standards .csv file
#
# Create response factors
Rf<-std_row/std_conc
rownames(Rf)<-"Rf"
out_std<-rbind(std_row,Rf)
# Sort columns
if (length(Rf)!=1) {
    out_std<-out_std[,sort(names(out_std))]
    } else {
    out_std<-out_std}
# Write output file
write.csv(out_std,paste(base_fn,"_3_Standards.csv",sep=""),row.names=TRUE)
# Inform user of created files
write(paste("        ",base_fn,"_3_Standards.csv",sep=""),"")

#
#    Create Concentration matrix
#
# Prepare an empty matrix
conc_mat<-as.data.frame(
    matrix(NA,nrow=nrow(area_mat),ncol=(ncol(area_mat)-2))
    )
# Get column names
cpd_cols<-colnames(area_mat[3:ncol(area_mat)])
# Apply to conc_mat
colnames(conc_mat)<-cpd_cols
# Remove standard from Sample and Group vectors
Sample<-Sample[-std_rownum]
Group<-Group[-std_rownum]
# Label matrix with samples and groups
conc_mat<-cbind(Sample,Group,conc_mat)

# Divide through Area matrix column-wise by compound class
for (jj in 3:ncol(area_mat)) {
    # Get current compound class
    cpd_cls<-gsub("(.*)(\\(.*)","\\1",colnames(area_mat)[jj],perl=TRUE)
    for (ii in 1:nrow(area_mat)) {
        # Use cpd_cls to divide by standard for that compound
        if (cpd_cls %in% colnames(Rf)==TRUE) {
            conc_mat[ii,jj]<-area_mat[ii,jj]/Rf[which(colnames(Rf)==cpd_cls)]
            } else {
            conc_mat[ii,jj]=NA
            }
        }
    }

# Replace NA with NoIS for prettiness in output file
out_conc_mat<-conc_mat
out_conc_mat[is.na(out_conc_mat)]<-"NoIS"
# Write output file
write.csv(out_conc_mat,paste(base_fn,"_4a_Conc.csv",sep=""),row.names=FALSE)
# Inform user of created files
write(paste("        ",base_fn,"_4a_Conc.csv",sep=""),"")

#
#    Summary Generator function
#
# param data_matrix: Matrix to use
# type data_matrix: data frame
#
# param type: Which values the matrix is made up of, currently either
#             "conc" or "norm" (case irrelevant)
# type type: string
summary_fn<-function(data_matrix,type) {
    ifelse(length(grep(type,"conc",ignore.case=TRUE))!=0,
        # If true (i.e. type is conc)
        fnum<-4,
        # else test if type is "norm"
        ifelse(length(grep(type,"norm",ignore.case=TRUE))!=0,
            fnum<-5,
            write(" ## Please check the value of 'type' ##","")
            )
        )
    #
    #    Create a matrix of means and standard deviations per group
    #
    # Get number of groups
    grp_lvls<-levels(data_matrix$Group)
    # Split the matrix by group
    matrix_list<-c()
    for (ii in 1:length(grp_lvls))
        matrix_list[ii]<-list(
          data_matrix[which(data_matrix$Group==grp_lvls[ii]),]
          )
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
        sub_mean[ii]<-list(apply(
          matrix_list[[ii]][,-c(1:2)],2,mean,na.rm=TRUE
          ))
        means[ii,]<-sub_mean[[ii]]
        }
    for (ii in 1:length(matrix_list)) {
        sub_sd[ii]<-list(apply(matrix_list[[ii]][,-c(1:2)],2,sd,na.rm=TRUE))
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
        paste(toupper(substring(type,1,1)),tolower(substring(type,2)),sep=""),
        "_Means.csv",sep=""),
        row.names=FALSE
        )
    #
    #    Inform user of created files
    #
    write(paste("        ",base_fn,"_",fnum,"b_",
        paste(toupper(substring(type,1,1)),tolower(substring(type,2)),sep=""),
        "_Means.csv",sep=""),"")
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
                    )]
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
            silent=TRUE)
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
                    )]
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
        paste(toupper(substring(type,1,1)),tolower(substring(type,2)),sep=""),
        "_Summ.csv",sep=""),
        row.names=FALSE
        )
    #
    #    Inform user of created files
    #
    write(paste("        ",base_fn,"_",fnum,"c_",
        paste(toupper(substring(type,1,1)),tolower(substring(type,2)),sep=""),
        "_Summ.csv",sep=""),"")
    }

# Create summary for concentrations
summary_fn(conc_mat,"conc")

#
#    Normalisation
#
# Only perform if norm=TRUE
if (norm==TRUE) {
    # Find norm_cpd
    if(norm_cpd != "") {
        # If true (i.e. norm_cpd isn't the empty string):
        # Get the actual name of the norm_cpd
        norm_cpd_fullname<-colnames(rt_mat[std_rownum,std_cols])[
            # 
            grep(norm_cpd,
            colnames(rt_mat[std_rownum,std_cols]),ignore.case=TRUE)]
        # Inform the user
        write(
            paste(" ** Using ",
                norm_cpd_fullname,
                " as compound for normalisation.",
                sep=""),
            "")
        } else {
        # Inform the user of an error
        write(" ## No value for 'norm_cpd'; exiting ##","")
        write(" ## Please supply a value next time. ##","")
        }
    # Prepare an empty matrix
    norm_mat<-as.data.frame(
        matrix(NA,nrow=nrow(area_mat),ncol=(ncol(area_mat)-2))
        )
    # Apply column names to conc_mat
    colnames(norm_mat)<-cpd_cols
    # Divide through conc mat by norm_val
    norm_mat<-conc_mat[3:(length(cpd_cols)+2)]/as.numeric(
        std_row[grep(norm_cpd,colnames(std_row),ignore.case=TRUE)]
        )
    # Label matrix with samples and groups
    norm_mat<-cbind(Sample,Group,norm_mat)
    # Create summaries
    write(" -> Created the following files:","")
    write.csv(norm_mat,paste(base_fn,"_5a_Norm.csv",sep=""),row.names=FALSE)
    write(paste("        ",base_fn,"_5a_Norm.csv",sep=""),"")
    summary_fn(norm_mat,"norm")
    }

# End function
}