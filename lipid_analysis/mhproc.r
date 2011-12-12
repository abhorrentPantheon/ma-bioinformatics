# mhproc.r
#
#    Author:    Jairus Bowne
#    Purpose:    MassHunter processing script
#
#    Input:    MassHunter .csv output file
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

#    Notes:    This is a wrapper function for the modularised scripts

# Determine which variables/objects are present before running script
rm_list<-list()
rm_list$pre=ls()
#
# User input section
#
# Standard concentration
input=readline(
    paste(" >> Please enter the concentration ",
    "of the internal standard (nM): ",
    sep=""
    )
)
#input="5000"
options(show.error.messages=FALSE,warn=-1)
if (is.na(as.numeric(input))) {
    write(" ## Please enter numbers only next time. Exiting...","")
    stop()
} else {
    std_conc<-as.numeric(input)
}

# Internal standards
input=readline(" ?? Keep internal standards in output data? (y/n/c): ")
#input="y"
if (length(grep(input,"y",ignore.case=TRUE))!=0) {
    keep_IS=TRUE
} else if (length(grep(input,"n",ignore.case=TRUE))!=0) {
    keep_IS=FALSE
} else if (length(grep(input,"c",ignore.case=TRUE))!=0) {
    write(" ## Exiting script.","")
    stop()
} else {
    write(" ## Please press y, n or c next time. Exiting...","")
    stop()
}

# Normalisation
input=readline(" ?? Perform normalisation of concentration data? (y/n/c): ")
#input="n"
if (length(grep(input,"y",ignore.case=TRUE))!=0) {
    norm=TRUE
} else if (length(grep(input,"n",ignore.case=TRUE))!=0) {
    norm=FALSE
} else if (length(grep(input,"c",ignore.case=TRUE))!=0) {
    write(" ## Exiting script.","")
    stop()
} else {
    write(" ## Please press y, n or c next time. Exiting...","")
    stop()
}
options(show.error.messages=TRUE,warn=0)

write(paste(" ** Standard concentration:",std_conc,"nM."),"")
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

source("mhproc_functions.r")
source("mhproc_rt_area.r")
source("mhproc_stds.r")
source("mhproc_conc.r")
if (norm==TRUE) {source("mhproc_norm.r")}
source("mhproc_imgs.r")
write(" ** All done! Exiting.","")

#
#    Tidy up
#
# List all objects
rm_list$post=ls()
# Remove objects in rm_list$post that aren't in rm_list$pre
rm(list=rm_list$post[which(rm_list$pre!=rm_list$post)])
rm(rm_list)
