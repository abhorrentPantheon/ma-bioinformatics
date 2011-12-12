#    mhproc_1.r
#
#    Author:    Jairus Bowne
#    Purpose:    Preprocessing of MassHunter output files for lipid analysis
#
#    Input:      MassHunter .csv output file
#    Output(s):     1) RT matrix                (.csv)
#                   2) Areas matrix             (.csv)
#
#    Notes:    Part 1 of modularised mhproc.r script

# Select file to use
in_matrix<-choose.files(
#    filter=c(
#        c("Comma separated value files (*.csv)",
#            "Text files (*.txt)"
#        ),
#        c("*.csv","*.txt")
#    ),
    filter=c(
        "Comma separated value files (*.csv)",
        "*.csv"
    ),
    caption="Select the data matrix file"
)

#filename<-gsub(
#    paste(".+",
#        gsub(".+/(.+)$",
#            "\\1",
#            getwd(),
#            perl=TRUE
#        ),
#        ".(.+)$",
#        sep=""
#    ),
#    "\\1",
#    in_matrix,
#    perl=TRUE
#)
filename<-basename(in_matrix)

# Show usage information
write(
    paste(" ** Using file",filename,"as input."),
    ""
)

# Save base file name for reuse
base_fn<-gsub("(.*[^\\.])\\.csv",
    "\\1",
    filename,
    perl=TRUE
)
# Read in file
basefile<-read.csv(in_matrix,header=FALSE,sep=",",quote="\"")

#
#    Filter the data set (remove unwanted columns) 
#
#prefilt<-basefile[,-c(1:3,5:7)]
# Keep only Data file name from initial headers
file_col<-which(basefile[2,]=="Data File")
data_start<-min(which(basefile[1,]!="" & basefile[1,]!="Sample"))
data_end<-length(colnames(basefile))
prefilt_cols<-c(file_col,data_start:data_end)
prefilt<-basefile[,prefilt_cols]

# Define columns and generate headers
area_cols<-which(prefilt[2,]=="Area")
rt_cols<-which(prefilt[2,]=="RT")
headers<-t(prefilt[1,which(prefilt[1,]!="")])
cpd_lbl<-rep(NA,length(headers))

for (ii in 1:length(headers)) {
    # Remove the spaces from...
    cpd_lbl[ii]<-gsub(" ",
        "",
        # ... the name of the compound without " Results" at the end
        gsub("(.*[^\\ ])\\ Results",
            "\\1",
            headers[ii],
            perl=TRUE
        ),
        perl=TRUE
    )
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
write(
    paste(" -> Removed ",
        length(mt_index),
        " empty row(s).",
        sep=""
    ),
    ""
)
write(
    paste(" -> Removed ",
        length(blank_index),
        " blank row(s).",
        sep=""
    ),
    ""
)

#
#    Create RT and Area matrices
#
# Generate sample and group names
# Samples
Sample<-gsub("(.*)\\.d",
    "\\1",
    filt[-1,1],
    perl=TRUE
)
# Groups
Group<-gsub("()_.*",
    "\\1",
    Sample,
    perl=TRUE
)

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
std_rownum<-grep("std",
    area_mat[,1],
    ignore.case=TRUE,
    perl=TRUE
)
std_cols<-grep("IS",colnames(area_mat))
std_row<-area_mat[std_rownum,std_cols]
std_names_long<-colnames(std_row)

# Get compound class names from standards
std_cls<-gsub("(.*)(\\(.*)",
    "\\1",
    colnames(area_mat)[std_cols],
    perl=TRUE
)

# Tidy the vector
if (dim(std_row)[2]==1) {
    std_row<-as.data.frame(std_row)
}

# Label the row as 'Area' if single
if (dim(std_row)[1]==1) {
    rownames(std_row)<-"Area"
# Otherwise select the std row to use
} else {
    all_std_rows<-std_row
    write("\n ** Standards: ","")
    write(
        paste("    ",
            paste(
                # Get the row number
                tmp<-grep("std",area_mat[std_rownum,1],perl=TRUE,ignore.case=TRUE),
                ") ",
                # List the name of the standard
                area_mat[std_rownum[tmp],1],
                sep=""
            #collapse=" "
            ),
            sep=""
        ),
        ""
    )
    s_row<-readline(
        paste(
            " >> Please type the number for",
            " the standard you wish to use: ",
            sep=""
        )
    )
    std_row<-std_row[as.numeric(s_row),]
    rownames(std_row)<-"Area"
}

# Use compound classes for the column names
names(std_row)<-std_cls

if (keep_IS==TRUE) {
    area_mat<-area_mat[-std_rownum,]
} else {
    # Remove standards from Area matrix
    area_mat<-area_mat[-std_rownum,-std_cols]
    cpd_lbl<-cpd_lbl[-std_cols]
}

# Sort columns
rt_mat<-rt_mat[,c(names(rt_mat[1:2]),
    sort(names(rt_mat[3:ncol(rt_mat)])))]
area_mat<-area_mat[,c(names(area_mat[1:2]),
    sort(names(area_mat[3:ncol(area_mat)])))]
    
# Store names for later use
#area_heads<-names(area_mat)

#
#    Output RT and Area matrices as .csv
#
write.csv(rt_mat,
    paste(base_fn,"_1_RT.csv",sep=""),
    row.names=FALSE
)
write.csv(area_mat,
    paste(base_fn,"_2_Area.csv",sep=""),
    row.names=FALSE
)
write(" -> Created the following files:","")
write(paste("        ",base_fn,"_1_RT.csv",sep=""),"")
write(paste("        ",base_fn,"_2_Area.csv",sep=""),"")
# Objects for later use
write.csv(std_row,"std_row.csv",row.names=TRUE)