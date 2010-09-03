#    boxplot.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Create boxplots of data
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output    Boxplots of data for samples and variables

#
#    Prepare the data matrix
#
# Read in the .csv file
data<-read.csv("input.csv", sep=",", row.names=1, header=TRUE)
# Get groups information
groups<-data[,1]
# Remove groups for data processing
boxplot_data<-data[,-1]

# Edit the column names if necessary
colnames(boxplot_data)<-if 
    (length(grep("^X[\\d]",colnames(boxplot_data),perl=TRUE)) != 0) # then
    {gsub("^X([\\d].*)","\\1",colnames(boxplot_data),perl=TRUE)} else
    {colnames(boxplot_data)}

#
#    Generate plot labels
#
labels<-NA
for(ii in 1:length(rownames(boxplot_data))) {
    labels[ii]<-paste(rownames(boxplot_data)[ii], "\n", groups[ii], sep="")
    }

#
#    Generate figure and output to the screen
#
x11()
# Set the axis (title, labels, axis) and plot area margins
par(mgp=c(5,2,0),                              # axis margins
                                               # (title, labels, line)
    mar=c(7,4,4,2),                            # plot margins (b,l,t,r)
    las=1                                      # horizontal labels
    )
# Draw boxplot for samples
boxplot(t(boxplot_data),
    names=labels,                          # x-axis labels
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

# ##### jpg #####
# pic_jpg<-function(filename,                    # name of output file
#     matrix,                                    # data matrix to use
#     title="",                                  # title of plot
#     x_label="",                                # x-axis label
#     y_label="",                                # y-axis label
#     cex_val=1,                                 # font size
#     names=names(matrix)                        # default
#     )
#     {# Start jpg device with basic settings
#     jpeg(filename,
#         quality=100,                           # image quality (percent)
#         bg="white",                            # background colour
#         res=300,                               # image resolution (dpi)
#         units="in", width=8.3, height=5.8)     # image dimensions (inches)
#     par(mgp=c(5,2,0),                          # axis margins 
#                                                # (title, labels, line)
#         mar=c(7,5,4,2),                        # plot margins (b,l,t,r)
#         las=1                                  # horizontal labels
#         )
#     # Draw the plot
#     boxplot(matrix, xlab=x_label, ylab=y_label, main=title, cex=cex_val)
#     # Turn off the device
#     dev.off()
#     }
# pic_jpg("boxplot_smpl.jpg", t(boxplot_data), x_label="Sample\nGroup",
#     names=labels, title="Boxplot - Samples")
# pic_jpg("boxplot_var.jpg", boxplot_data, x_label="Variable",
#     title="Boxplot - Variables")
# ##### end jpg #####


##### png #####
# pic_png<-function(filename,                    # name of output file
#     matrix,                                    # data matrix to use
#     title="",                                  # title of plot 
#     x_label="",                                # x-axis label
#     y_label="",                                # y-axis label
#     cex_val=1,                                 # font size
#     names=names(matrix)                        # default
#     )
#     {# Start png device with basic settings
#     png(filename,
#         bg="white",                            # background colour
#         res=300,                               # image resolution (dpi)
#         units="in", width=8.3, height=5.8)     # image dimensions (inches)
#     par(mgp=c(5,2,0),                          # axis margins 
#                                                # (title, labels, line)
#         mar=c(7,5,4,2),                        # plot margins (b,l,t,r)
#         las=1                                  # horizontal labels
#         )
#     # Draw the plot
#     boxplot(matrix, xlab=x_label, ylab=y_label, main=title, cex=cex_val)
#     # Turn off the device
#     dev.off()
#     }
# pic_png("boxplot_smpl.png", t(boxplot_data), x_label="Sample\nGroup",
#     names=labels, title="Boxplot - Samples")
# pic_png("boxplot_var.png", boxplot_data, x_label="Variable",
#     title="Boxplot - Variables")
##### end png #####


# ##### tiff #####
# pic_tiff<-function(filename,                   # name of output file
#     matrix,                                    # data matrix to use
#     title="",                                  # title of plot 
#     x_label="",                                # x-axis label
#     y_label="",                                # y-axis label
#     cex_val=1,                                 # font size
#     names=names(matrix)                        # default 
#     )
#     {# Start tiff device with basic settings
#     tiff(filename,
#         bg="white",                            # background colour
#         res=300,                               # image resolution (dpi)
#         units="in", width=8.3, height=5.8)     # image dimensions (inches)
#         compression="none"                     # image compression 
#                                                #  (one of none, lzw, zip)
#     par(mgp=c(5,2,0),                          # axis margins 
#                                                # (title, labels, line)
#         mar=c(7,5,4,2),                        # plot margins (b,l,t,r)
#         las=1                                  # horizontal labels
#         )
#     # Draw the plot
#     boxplot(matrix, xlab=x_label, ylab=y_label, main=title, cex=cex_val)
#     # Turn off the device
#     dev.off()
#     }
# pic_tiff("boxplot_smpl.tif", t(boxplot_data), x_label="Sample\nGroup",
#     names=labels, title="Boxplot - Samples")
# pic_tiff("boxplot_var.tif", boxplot_data, x_label="Variable",
#     title="Boxplot - Variables")
# ##### end tiff #####