#    mhproc_2.r
#
#    Author:    Jairus Bowne
#    Purpose:    Preprocessing of MassHunter output files for lipid analysis
#
#    Input:      Files and objects produced by mhproc_1.r
#                    f) std_row.csv (created by mhproc_1.r)
#                    o) base_fn
#                    o) str_row
#                    o) std_conc                    
#
#    Output(s):     3) Standards
#                          Area and Rf values   (.csv)
#
#    Notes:    Part 2 of modularised mhproc.r script

#
#    Read in the standard row file
#
std_row<-read.csv("std_row.csv",sep=",",header=TRUE,row.names=1)
# Remove the temp file (we will save a more informative version later)
file.remove("std_row.csv")

#
#    Prepare and output Standards .csv file
#
# Create response factors
Rf<-std_row/std_conc    ################################## hold
rownames(Rf)<-"Rf"
out_std<-rbind(std_row,Rf)

# Sort columns
if (length(Rf)!=1) {
    out_std<-out_std[,sort(names(out_std))]
} else {
    out_std<-out_std
}

# Write output file
write.csv(out_std,
    paste(base_fn,
        "_3_Standards.csv",
        sep=""
    ),row.names=TRUE
)

# Inform user of created files
write(
    paste("        ",base_fn,"_3_Standards.csv",sep=""),
    ""
)
