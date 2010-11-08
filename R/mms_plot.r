#    mms_plot.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Plot mean, median and standard deviation
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    Plot showing the mean and standard deviation across samples

#
#    Load the data matrix
#
# Read in the .csv file
data<-read.csv("input.csv", sep=",", row.names=1, header=TRUE)
# Remove groups for data processing
mms_data<-data[,-1]

#
#    Preparation of plotting data 
#
# Take the mean of all the variables for each sample
Mean<-apply(mms_data,1,mean,na.rm=TRUE)
# Take the median of all the variables for each sample
Median<-apply(mms_data,1,median,na.rm=TRUE)
# Take the standard deviation of all the variables for each sample
StdDev<-apply(mms_data,1,sd,na.rm=TRUE)
# Join the data into a matrix for plotting
mms<-data.frame(cbind(Mean,Median,StdDev))

rownames(mms) <- if 
    (length(grep("^X[\\d]",rownames(mms),perl=TRUE)) != 0) # then
    {gsub("^X([\\d].*)","\\1",rownames(mms),perl=TRUE)} else
    {rownames(mms)}

#
#    Generate figure and output to the screen
#
# Prepare plot characteristics
### These have been selected to be useful for both normal vision and
### colourblind vision. The multiple redundancy (colour, line style, shape)
### is of particular importance, communicating information without
### using the names of colours.
yrange<-range(mms)
xrange<-c(1,length(mms$Mean))
colours<-c("#ff0000",               # red
            "#009e73",              # blue-green
            "#9900cc")              # purple
pchlist<-c(15,16,17)                # square, circle, triangle
linetype<-c(1:3)                    # solid, dashed, dotted

#
#    Generate figure and output to the screen
#
x11()
par(las=1)                          # y-axis labels horizontal
plot(xrange, yrange,                # set min/max for x and y axes
    main="Mean, Median and Standard Deviation",
    type="n",                       # don't actually plot anything (yet)
    xlab="",                        # x-axis title
    ylab="",                        # y-axis title
    xaxt="n",                       # suppress the x-axis
    lab=c(10,10,7)                  # number of tick marks on axes (see ?par)
    )
    axis(side=1,                    # on the bottom
        labels=rownames(mms),       # use rownames as labels
        at=1:xrange[2]              # where to put the labels
        )
    # Draw lines
    for (ii in 1:3) {
        lines(c(1:xrange[2]),       # x-values
            mms[,ii],               # y-values
            pch=pchlist[ii],        # points (see above)
            col=colours[ii],        # colour (see above)
            lty=linetype[ii],       # line type (see above)
            lwd=2                   # line thickness
            )
        }
    legend("topleft",               # position of legend
        colnames(mms),              # labels on legend
        pch=pchlist,                # points (see above)
        col=colours,                # colour (see above)
        lty=linetype,               # line type (see above)
        lwd=2,                      # line thickness
        cex=1                       # character size
        )

#
#    Generate figure as image file
#
#    (Uncomment blocks as necessary)

# ##### jpg #####
# pic_jpg<-function(filename,                    # name of output file
#     matrix,                                    # data matrix to use
#     x_label="",                                # x-axis label
#     y_label="",                                # y-axis label
#     cex_val=1                                  # font size 
#     )
#     {# Start jpg device with basic settings
#     jpeg(filename,
#         quality=100,                           # image quality (percent)
#         bg="white",                            # background colour
#         res=300,                               # image resolution (dpi)
#         units="in", width=8.3, height=5.8)     # image dimensions (inches)
#     par(mgp=c(5,2,0),                          # axis margins 
#                                                # (title, labels, line)
#         mar=c(5,4,4,2),                        # plot margins (b,l,t,r)
#         las=1)                                 # y-axis labels horizontal
#     # Draw the plot
#     plot(xrange, yrange,           # set min/max for x and y axes
#         main="Mean, Median and Standard Deviation",
#         type="n",                  # don't actually plot anything (yet)
#         xlab="",                   # x-axis title
#         ylab="",                   # y-axis title
#         xaxt="n",                  # suppress the x-axis
#         lab=c(10,10,7),            # number of tick marks on axes (see ?par)
#         )
#     axis(side=1,                   # on the bottom
#         labels=rownames(matrix),   # use rownames as labels
#         at=1:xrange[2])            # where to put the labels
#         # Draw lines
#     for (ii in 1:3) {
#         lines(c(1:xrange[2]),      # x-values
#             mms[,ii],              # y-values
#             pch=pchlist[ii],       # points (see above)
#             col=colours[ii],       # colour (see above)
#             lty=linetype[ii],      # line type (see above)
#             lwd=2                  # line thickness
#             )
#         }
#     legend("topleft",              # position of legend
#         colnames(mms),             # labels on legend
#         pch=pchlist,               # points (see above)
#         col=colours,               # colour (see above)
#         lty=linetype,              # line type (see above)
#         lwd=2,                     # line thickness
#         cex=cex_val                # character size
#         )
#     # Turn off the device
#     dev.off()
#     }
# pic_jpg("MeanMedStdev_plot.jpg", mms)
# # To increase the font size, change cex accordingly:
# # pic_jpg("MeanMedStdev_plot.jpg", mms, cex_val=3)
# ##### end jpg #####


##### png #####
# pic_png<-function(filename,                    # name of output file
#     matrix,                                    # data matrix to use
#     x_label="",                                # x-axis label
#     y_label="",                                # y-axis label
#     cex_val=1                                  # font size 
#     )
#     {# Start png device with basic settings
#     png(filename,
#         bg="white",                            # background colour
#         res=300,                               # image resolution (dpi)
#         units="in", width=8.3, height=5.8)     # image dimensions (inches)
#     par(mgp=c(5,2,0),                          # axis margins 
#                                                # (title, labels, line)
#         mar=c(5,4,4,2),                        # plot margins (b,l,t,r)
#         las=1)                                 # y-axis labels horizontal
#     # Draw the plot
#     plot(xrange, yrange,           # set min/max for x and y axes
#         main="Mean, Median and Standard Deviation",
#         type="n",                  # don't actually plot anything (yet)
#         xlab="",                   # x-axis title
#         ylab="",                   # y-axis title
#         xaxt="n",                  # suppress the x-axis
#         lab=c(10,10,7),            # number of tick marks on axes (see ?par)
#         )
#     axis(side=1,                   # on the bottom
#         labels=rownames(matrix),   # use rownames as labels
#         at=1:xrange[2])            # where to put the labels
#     # Draw lines
#     for (ii in 1:3) {
#         lines(c(1:xrange[2]),      # x-values
#             mms[,ii],              # y-values
#             pch=pchlist[ii],       # points (see above)
#             col=colours[ii],       # colour (see above)
#             lty=linetype[ii],      # line type (see above)
#             lwd=2                  # line thickness
#             )
#         }
#     legend("topleft",              # position of legend
#         colnames(mms),             # labels on legend
#         pch=pchlist,               # points (see above)
#         col=colours,               # colour (see above)
#         lty=linetype,              # line type (see above)
#         lwd=2,                     # line thickness
#         cex=cex_val                # character size
#         )
#     # Turn off the device
#     dev.off()
#     }
# pic_png("MeanMedStdev_plot.png", mms)
# # To increase the font size, change cex accordingly:
# # pic_png("MeanMedStdev_plot.png", mms, cex_val=3)
##### end png #####


# ##### tiff #####
# pic_tiff<-function(filename,                   # name of output file
#     matrix,                                    # data matrix to use
#     x_label="",                                # x-axis label
#     y_label="",                                # y-axis label
#     cex_val=1                                  # font size 
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
#         mar=c(5,4,4,2),                        # plot margins (b,l,t,r)
#         las=1)                                 # y-axis labels horizontal
#     # Draw the plot
#     plot(xrange, yrange,           # set min/max for x and y axes
#         main="Mean, Median and Standard Deviation",
#         type="n",                  # don't actually plot anything (yet)
#         xlab="",                   # x-axis title
#         ylab="",                   # y-axis title
#         xaxt="n",                  # suppress the x-axis
#         lab=c(10,10,7),            # number of tick marks on axes (see ?par)
#         )
#     axis(side=1,                   # on the bottom
#         labels=rownames(mms),      # use rownames as labels
#         at=1:xrange[2])            # where to put the labels
#     # Draw lines
#     for (ii in 1:3) {
#         lines(c(1:xrange[2]),      # x-values
#             mms[,ii],              # y-values
#             pch=pchlist[ii],       # points (see above)
#             col=colours[ii],       # colour (see above)
#             lty=linetype[ii],      # line type (see above)
#             lwd=2                  # line thickness
#             )
#     }
#     legend("topleft",              # position of legend
#         colnames(mms),             # labels on legend
#         pch=pchlist,               # points (see above)
#         col=colours,               # colour (see above)
#         lty=linetype,              # line type (see above)
#         lwd=2,                     # line thickness
#         cex=cex_val                # character size
#         )
#     # Turn off the device
#     dev.off()
#     }
# pic_tiff("MeanMedStdev_plot.tif", mms)
# # To increase the font size, change cex accordingly:
# # pic_tiff("MeanMedStdev_plot.tif", mms, cex_val=3)
# ##### end tiff #####