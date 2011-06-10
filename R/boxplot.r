#    boxplot.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Create boxplots of data
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output    Boxplots of data for samples and variables

# Determine which variables/objects are present before running script
rm_list<-list()
rm_list$pre=ls()

#
#    Prepare the data matrix
#
# Read in the .csv file
in_file<-file.choose()
input_data<-read.csv(in_file, sep=",", row.names=1, header=TRUE)

# Get groups information
groups<-input_data[,1]
# Remove groups for data processing
boxplot_data<-input_data[,-1]

# Edit the column names if necessary
colnames(boxplot_data)<-if (
    length(
        grep("^X[\\d]",colnames(boxplot_data),perl=TRUE)
    ) != 0
) {# then
    gsub("^X([\\d].*)","\\1",colnames(boxplot_data),perl=TRUE)
} else {
    colnames(boxplot_data)
}

#
#    Generate plot labels
#
plot_labels<-c()
for(ii in 1:length(rownames(boxplot_data))) {
    plot_labels[ii]<-paste(
        rownames(boxplot_data)[ii],
        "\n",
        groups[ii],
        sep=""
    )
}

#
#    Generate figure and output to the screen
#
x11()
# Set the axis (title, labels, axis) and plot area margins
par(mgp=c(5,2,0),                          # axis margins
                                           # (title, labels, line)
    mar=c(7,4,4,2),                        # plot margins (b,l,t,r)
    las=1                                  # horizontal labels
)
# Draw boxplot for samples
boxplot(t(boxplot_data),
    names=plot_labels,                          # x-axis labels
    xlab="Sample\nGroup",                  # x-axis title
    main="Boxplot - Samples",              # plot title
    cex=1                                  # font size
)

# Draw boxplot for variables
x11()
par(mgp=c(5,2,0),                          # axis margins
                                           # (title, labels, line)
    mar=c(7,4,4,2),                        # plot margins (b,l,t,r)
    las=1                                  # horizontal labels
)
boxplot(boxplot_data,
    xlab="Variable",                       # x-axis title
    main="Boxplot - Variables",            # plot title
    cex=1                                  # font size
)

#
#    Generate figure as image file
#
#    (Uncomment blocks as necessary)

##### jpg #####
# pic_jpg<-function(filename,                # name of output file
#     input_matrix,                          # data matrix to use
#     plot_title="",                              # title of plot
#     x_label="",                            # x-axis label
#     y_label="",                            # y-axis label
#     cex_val=1,                             # font size
#     plot_names=names(input_matrix)              # default
# ) { # Start jpg device with basic settings
#     jpeg(filename,
#         quality=100,                           # image quality (percent)
#         bg="white",                            # background colour
#         res=300,                               # image resolution (dpi)
#         units="in", width=8.3, height=5.8     # image dimensions (inches)
#     )
#     par(mgp=c(5,2,0),                          # axis margins 
#                                                # (title, labels, line)
#         mar=c(7,5,4,2),                        # plot margins (b,l,t,r)
#         las=1                                  # horizontal labels
#     )
#     # Draw the plot
#     boxplot(input_matrix, 
#         xlab=x_label,
#         ylab=y_label,
#         main=plot_title,
#         cex=cex_val,
#         names=plot_names
#     )
#     # Turn off the device
#     dev.off()
# }
# pic_jpg("boxplot_smpl.jpg",
#     t(boxplot_data),
#     x_label="Sample\nGroup",
#     plot_names=plot_labels,
#     plot_title="Boxplot - Samples"
# )
# pic_jpg("boxplot_var.jpg",
#     boxplot_data,
#     x_label="Variable",
#     plot_title="Boxplot - Variables"
# )
##### end jpg #####


##### png #####
# pic_png<-function(filename,                    # name of output file
#     input_matrix,                          # data matrix to use
#     plot_title="",                              # title of plot
#     x_label="",                            # x-axis label
#     y_label="",                            # y-axis label
#     cex_val=1,                             # font size
#     plot_names=names(input_matrix)              # default
# ) { # Start jpg device with basic settings
#     png(filename,
#         bg="white",                            # background colour
#         res=300,                               # image resolution (dpi)
#         units="in", width=8.3, height=5.8     # image dimensions (inches)
#     )
#     par(mgp=c(5,2,0),                          # axis margins 
#                                                # (title, labels, line)
#         mar=c(7,5,4,2),                        # plot margins (b,l,t,r)
#         las=1                                  # horizontal labels
#     )
#     # Draw the plot
#     boxplot(input_matrix,
#         xlab=x_label,
#         ylab=y_label,
#         main=plot_title,
#         cex=cex_val,
#         names=plot_names
#     )
#     # Turn off the device
#     dev.off()
# }
# pic_png("boxplot_smpl1.png",
#     t(boxplot_data),
#     x_label="Sample\nGroup",
#     plot_names=plot_labels,
#     plot_title="Boxplot - Samples"
# )
# pic_png("boxplot_var1.png",
#     boxplot_data,
#     x_label="Variable",
#     plot_title="Boxplot - Variables"
# )
##### end png #####


##### tiff #####
# pic_tiff<-function(filename,                   # name of output file
#     input_matrix,                          # data matrix to use
#     plot_title="",                              # title of plot
#     x_label="",                            # x-axis label
#     y_label="",                            # y-axis label
#     cex_val=1,                             # font size
#     plot_names=names(input_matrix)              # default
# ) { # Start jpg device with basic settings
#     tiff(filename,
#         bg="white",                            # background colour
#         res=300,                               # image resolution (dpi)
#         units="in", width=8.3, height=5.8,     # image dimensions (inches)
#         compression="none"                     # image compression 
#     )                                          #  (one of none, lzw, zip)
#     par(mgp=c(5,2,0),                          # axis margins 
#                                                # (title, labels, line)
#         mar=c(7,5,4,2),                        # plot margins (b,l,t,r)
#         las=1                                  # horizontal labels
#     )
#     # Draw the plot
#     boxplot(input_matrix,
#         xlab=x_label,
#         ylab=y_label,
#         main=plot_title,
#         cex=cex_val,
#         names=plot_names
#     )
#     # Turn off the device
#     dev.off()
# }
# pic_tiff("boxplot_smpl.tif",
#     t(boxplot_data),
#     x_label="Sample\nGroup",
#     plot_names=plot_labels,
#     plot_title="Boxplot - Samples"
# )
# pic_tiff("boxplot_var.tif",
#     boxplot_data,
#     x_label="Variable",
#     plot_title="Boxplot - Variables"
# )
##### end tiff #####

#
#    Tidy up
#
# List all objects
rm_list$post=ls()
########################################## Suppress warnings here (all scripts)
# Remove objects in rm_list$post that aren't in rm_list$pre
rm(list=rm_list$post[which(rm_list$pre!=rm_list$post)])
########################################## Reinstate warnings here (all scripts)
rm(rm_list)
