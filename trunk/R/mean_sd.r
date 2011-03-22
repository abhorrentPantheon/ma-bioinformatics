#    mean_sd.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Plot mean and standard deviation
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    Plot showing the mean and standard deviation across
#               samples|variables

#
#    Load the data matrix
#
# Read in the .csv file
in_file<-file.choose()
input_data<-read.csv(in_file, sep=",", row.names=1, header=TRUE)

# Remove groups for data processing
msd_data<-input_data[,-1]

#
#    Prepare data for plotting
#
# Take the mean of all the variables for each sample
Mean<-apply(msd_data,2,mean,na.rm=TRUE)
# Take the standard deviation of all the variables for each sample
StdDev<-apply(msd_data,2,sd,na.rm=TRUE)
# Join the data into a matrix for plotting
msd<-data.frame(cbind(Mean,StdDev))

#
#    Generate figure and output to the screen
#
x11()
plot(msd, las=1, main="Scedasticity Plot",col="blue")

#
#    Generate figure as image file
#
#    (Uncomment blocks as necessary)

##### jpg #####
pic_jpg<-function(filename, input_matrix, cex_val=1) {
    # Start jpg device with basic settings
    jpeg(filename,
        quality=100,                       # image quality (percent)
        bg="white",                        # background colour
        res=300,                           # image resolution (dpi)
        units="in", width=8.3, height=5.8  # image dimensions (inches)
    )
    par(mgp=c(5,2,0),                      # axis margins
                                           # (title, labels, line)
        mar=c(4,4,4,2)                     # plot margins (b,l,t,r)
    )
    # Draw the plot
    plot(msd, cex=cex_val, las=1, main="Scedasticity Plot")
    # Turn off the device
    dev.off()
}
pic_jpg("mean_sd_plot.jpg", msd)
# For larger text, change the cex_val e.g.:
# pic_jpg("mean_sd_plot.jpg", msd, cex_val=3)
##### end jpg #####


#### png #####
pic_png<-function(filename, input_matrix, cex_val=1) {
    # Start png device with basic settings
    png(filename,
        bg="white",                        # background colour
        res=300,                           # image resolution (dpi)
        units="in", width=8.3, height=5.8  # image dimensions (inches)
    )
    par(mgp=c(3,1,0),                      # axis margins
                                           # (title, labels, line)
        mar=c(4,4,4,2)                     # plot margins (b,l,t,r)
    )
    # Draw the plot
    plot(msd, cex=cex_val, las=1, main="Scedasticity Plot")
    # Turn off the device
    dev.off()
}
pic_png("mean_sd_plot.png", msd)
# For larger text, change the cex_val e.g.:
# pic_png("mean_sd_plot.png", msd, cex_val=3)
#### end png #####


##### tiff #####
pic_tiff<-function(filename, input_matrix, cex_val=1) {
    # Start tiff device with basic settings
    tiff(filename,
        bg="white",                        # background colour
        res=300,                           # image resolution (dpi)
        units="in", width=8.3, height=5.8, # image dimensions (inches)
        compression="none"                 # image compression 
    )                                      #  (one of none, lzw, zip)
    par(mgp=c(5,2,0),                      # axis margins
                                           # (title, labels, line)
        mar=c(4,4,4,2)                     # plot margins (b,l,t,r)
    )
    # Draw the plot
    plot(msd, cex=cex_val, las=1, main="Scedasticity Plot")
    # Turn off the device
    dev.off()
}
pic_tiff("mean_sd_plot.tif", msd)
# For larger text, change the cex_val e.g.:
# pic_tiff("mean_sd_plot.tif", msd, cex_val=3)
##### end tiff #####
