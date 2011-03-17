#    mhproc_4.r
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
#    Output(s):     5) Normalised concentrations
#                       a)    Matrix            (.csv)
#                       b)    Means             (.csv)
#                       c)    Summary           (.csv)
#
#    Notes:    Part 4 of modularised mhproc.r script

#
#    Normalisation
#
# Ask user for name of norm_cpd
is_cpds<-cpd_lbl[grep("IS",cpd_lbl,perl=TRUE)]
write("\n ** Internal standard compounds: ","")
write(
    paste("    ",
        paste(
            # Get the index of the compound
            grep("IS",is_cpds,perl=TRUE),
            ") ",
            # List the (full) compound name
            is_cpds,
            sep=""
            #collapse=" "
        ),
        sep=""
    )
    ,""
)
input<-readline(
    paste(
        " >> Please select the number of the ",
        "compound to use for normalisation: ",
        sep=""
    )
)
# Check the value is a number
options(show.error.messages=FALSE,warn=-1)
if (is.na(as.numeric(input))) {
    write(" ## Please enter numbers only next time. Exiting...","")
    stop()
} else {
    nc_index<-as.numeric(input)
}
options(show.error.messages=TRUE,warn=0)
norm_cpd_fullname<-is_cpds[nc_index]

write(
    paste(" ** Using ",
        norm_cpd_fullname,
        " as compound for normalisation.",
        sep=""),
    ""
)

norm_cpd<-gsub("(.*)(\\(.*)","\\1",norm_cpd_fullname,perl=TRUE)
# Prepare an empty matrix
norm_mat<-as.data.frame(
    matrix(NA,nrow=nrow(area_mat),ncol=(ncol(area_mat)-2))
)
# Apply column names to conc_mat
colnames(norm_mat)<-cpd_cols
# Divide through conc mat by norm_val
norm_mat<-conc_mat[3:(length(cpd_cols)+2)]/as.numeric(
    std_row[[norm_cpd]]
)
# Label matrix with samples and groups
norm_mat<-cbind(Sample,Group,norm_mat)
# Create summaries
write(" -> Created the following files:","")
write.csv(norm_mat,paste(base_fn,"_5a_Norm.csv",sep=""),row.names=FALSE)
write(paste("        ",base_fn,"_5a_Norm.csv",sep=""),"")

# Summarise normalised concentration data by function
summary_fn(norm_mat,"norm")

