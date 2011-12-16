#    mhproc_3.r
#
#    Author:    Jairus Bowne
#    Purpose:    Preprocessing of MassHunter output files for lipid analysis
#
#    Input:    Files and objects produced by mhproc_1.r and mhproc_2.r
#                  f) {base_fn}_2_Area.csv
#                  o) Sample
#                  o) Group
#                  o) std_rownum
#                  o) Rf
#    Output(s):     4) Concentrations
#                       a)    Matrix            (.csv)
#                       b)    Means             (.csv)
#                       c)    Summary           (.csv)
#
#    Notes:    Part 3 of modularised mhproc.r script

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

# Read in fresh Area matrix (converts to numeric data frame)
area_mat<-read.csv(
    paste(
        base_fn,
        "_2_Area.csv",
        sep=""
    ),
    header=TRUE,
    sep=",",
    quote="\""
)

# Repair colnames
colnames(area_mat)<-c(cpd_lbl[1:2],sort(cpd_lbl[3:length(cpd_lbl)]))

# Divide through Area matrix column-wise by compound class
for (jj in 3:ncol(area_mat)) {
    # Get current compound class
    cpd_cls<-gsub("(.*)(\\(.*)","\\1",colnames(area_mat)[jj],perl=TRUE)
    
    for (ii in 1:nrow(area_mat)) {
        # For single value standards
        if (std_point==TRUE) {
            # Use cpd_cls to divide by standard for that compound
            kk<-which(colnames(Rf)==cpd_cls)
            
            if (cpd_cls %in% colnames(Rf)==TRUE) {
                if (Rf[kk]!=0) {
                    conc_mat[ii,jj]<-area_mat[ii,jj]/Rf[kk]
                } else {
                    conc_mat[ii,jj]<-"NoIS"
                }
            } else {
                conc_mat[ii,jj]=NA
            }
        # For standard curve
        } else {
            kk<-which(colnames(lm_vals)==cpd_cls)
            if (cpd_cls %in% colnames(lm_vals)==TRUE) {
                if (lm_vals[1,kk]!=0) {
                    conc_mat[ii,jj]<-(
                        area_mat[ii,jj]-lm_vals[2,kk]
                    )/lm_vals[1,kk]
                } else {
                    conc_mat[ii,kk]<-"NoIS"
                }
            } else {
                conc_mat[ii,jj]=NA
            }
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

# Summarise concentration data by function
summary_fn(conc_mat,"conc")
