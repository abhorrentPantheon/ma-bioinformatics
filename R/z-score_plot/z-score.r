#    Z-score transform
#
#    Author:    Jairus Bowne, Amsha Nahid, Alysha De Livera
#    Purpose:    ? [See differences between metabolites]
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#               Control group must be listed first (rows 1:n)
#    Output:    Z-transformed data matrix (transpose of input)
#               ? [Image showing Z-scores]

#
#    Load and prepare the data matrix
#
# Read in the .csv file
in_file<-file.choose()
input_data<-read.csv(in_file, sep=",", row.names=1, header=TRUE)
# Ensure group data is titled "Group"
colnames(input_data)[1]<-"Group"
# Get groups information
Group<-input_data[,1]

# Remove groups for data processing
data_matrix<-t(input_data[,-1])

#
#    Split matrix into groups
#
# Get total number of groups
grp_lvls<-levels(Group)
# Control group is first
ctrl_grp<-Group[1]

# Split the matrix by group
matrix_list<-list()
list_names<-c(grp_lvls)
for (ii in 1:length(grp_lvls)) {
    matrix_list[[list_names[ii]]]<-data_matrix[,which(
        input_data$Group==grp_lvls[ii]
    )]
}

# Get mean and standard deviation for control group (per metabolite)
ctrl_means<-apply(matrix_list[[ctrl_grp]],1,mean,na.rm=TRUE)
ctrl_sds<-apply(matrix_list[[ctrl_grp]],1,sd,na.rm=TRUE)

z_matrix<-matrix(NA,
    nrow=dim(data_matrix)[1],
    ncol=dim(data_matrix)[2],
    dimnames=list(rownames(data_matrix),colnames(data_matrix))
)

#
#    Calculate z-scores
#
for (ii in 1:dim(data_matrix)[1]) {
    for (jj in 1:dim(data_matrix)[2]) {
        z_matrix[ii,jj]<-(data_matrix[ii,jj]-ctrl_means[ii])/ctrl_sds[ii]
    }
}

#
#    Import support functions
#
source("z_plot.r")
source("img_type.r")
source("img_changer.r")
source("img_maker.r")

#
#    Produce the initial graphic
#
z_plot()

# Check if user is happy with graphic
ic_result<-img_changer()
    input<-ic_result[[1]]
    min_x<-ic_result[[2]]
    max_x<-ic_result[[3]]
    if (input==1) {
        img_format<-ic_result[[4]]
        file_name<-"z-plot"
        out_file<-paste(file_name,img_format,sep=".")
    }

# Keep running image changer until user is happy
while (input==2) {
    ic_result<-img_changer()
    #img_changer()
    input<-ic_result[[1]]
    min_x<-ic_result[[2]]
    max_x<-ic_result[[3]]
    if (input==1) {
        img_format<-ic_result[[4]]
        file_name<-"z-plot"
        out_file<-paste(file_name,img_format,sep=".")
    }
}

# Generate figures
if (input==1) {
    img_maker(img_format)
}

# Close any spare device windows
while (is.na(length(dev.list()))==FALSE) {
    options(show.error.messages=FALSE,warn=-1)
    dev.off()
    options(show.error.messages=FALSE,warn=0)
}
