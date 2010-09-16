#    heatmap.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Create a heatmap
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    Heatmap with dendrograms showing clustering of both
#               samples and variables.

#
#    Load necessary libraries, and install them if they are missing
#
tryCatch(library(gplots), error=function(err)
    # if this produces an error:
    install.packages("gplots",repos="http://cran.ms.unimelb.edu.au/"))

#
#    Prepare the data matrix
#
# Read in the .csv file
data<-read.csv("input.csv", header = TRUE, row.names=1)

heatmap_data<-data[,-1]

colnames(heatmap_data) <- if 
    (length(grep("^X[\\d]",colnames(heatmap_data),perl=TRUE)) != 0) # then
    {gsub("^X([\\d].*)","\\1",colnames(heatmap_data),perl=TRUE)} else
    {colnames(heatmap_data)}

# Define palette colours
rch<-rich.colors(256)
rwb<-colorRampPalette(c("red","white","blue"))(256)
rbb<-colorRampPalette(c("red","black","blue"))(256)
pwg<-colorRampPalette(c("purple","white","green"))(256)
pbg<-colorRampPalette(c("purple","black","green"))(256)

pic_onscr<-function(matrix, title="", cex_val=1, pal=rch)
    {x11()
    heatmap.2(matrix,                              # matrix to use
        #Colv=FALSE,                      # reorder dendrogram
        dendrogram="both",                         # dendrograms to draw
        col=pal,                                   # palette (see above)
        trace="none",                              # trace line (show distance)
        cexCol=cex_val,                            # size of column labels
        cexRow=cex_val,                            # size of row labels
        key=TRUE,                                  # show the colour key
        keysize=cex_val,                           # key size
        density.info="none",                       # superimpose "histogram" or a
                                                   # "density" plot on colour key
        margins=c(10,10),                          # add space for labels
        main=title,                                # title
        xlab="",                                   # x-axis title
        ylab=""                                    # y-axis title
        )
    }
# Draw the heatmap
pic_onscr(t(heatmap_data))

#
#    Generate figure as image file
#
#    (Uncomment blocks as necessary)

# ##### jpg #####
# pic_jpg<-function(filename, matrix, title="", cex_val=1, pal=rch)
#     {# Start jpg device with basic settings
#     ## Fiddle with the width and height settings to get the key to
#     ## display with a 3:2 aspect ratio for best results. 8.3x5.8 is A5.
#     jpeg(filename,
#         quality=100,                       # image quality (percent)
#         bg="white",                        # background colour
#         res=300,                           # image resolution (dpi)
#         units="in", width=8.3, height=5.8) # image dimensions (inches)
#         par(mgp=c(3,1,0),                  # axis margins
#                                            # (title, labels, line)
#             mar=c(5,4,4,2)                 # plot margins (b,l,t,r)
#             )
#     # Draw the plot
#      heatmap.2(matrix,                     # matrix to use
#         #Colv=FALSE,                       # reorder dendrogram
#         dendrogram="both",                 # dendrograms to draw
#         col=pal,                           # palette (see above)
#         trace="none",                      # trace line (show distance)
#         cexCol=cex_val,                    # size of column labels
#         cexRow=cex_val,                    # size of row labels
#         key=TRUE,                          # show the colour key
#         keysize=cex_val,                   # key size
#         density.info="none",               # superimpose "histogram" or a
#                                            # "density" plot on colour key
#         margins=c(5,5),                    # add space for labels
#         main=title,                        # title
#         xlab="",                           # x-axis title
#         ylab=""                            # y-axis title
#         )
#     # Turn off the device
#     dev.off()
#     }
# pic_jpg("heatmap.jpg", t(heatmap_data))
# # For larger text, change the cex_val:
# # pic_jpg("heatmap.jpg", t(heatmap_data), cex_val=3)
# ##### end jpg #####


#### png #####
# pic_png<-function(filename, matrix, title="", cex_val=1, pal=rch)
#     {# Start png device with basic settings
#     ## Fiddle with the width and height settings to get the key to
#     ## display with a 3:2 aspect ratio for best results. 8.3x5.8 is A5.
#     png(filename,
#         bg="white",                        # background colour
#         res=300,                           # image resolution (dpi)
#         units="in", width=5.8, height=6.5) # image dimensions (inches)
#         par(mgp=c(3,1,0),                  # axis margins
#                                            # (title, labels, line)
#             mar=c(5,4,4,2)                 # plot margins (b,l,t,r)
#             )
#     # Draw the plot
#      heatmap.2(matrix,                     # matrix to use
#         #Colv=FALSE,                       # reorder dendrogram
#         dendrogram="both",                 # dendrograms to draw
#         col=pal,                           # palette (see above)
#         trace="none",                      # trace line (show distance)
#         cexCol=cex_val,                    # size of column labels
#         cexRow=cex_val,                    # size of row labels
#         key=TRUE,                          # show the colour key
#         keysize=cex_val,                   # key size
#         density.info="none",               # superimpose "histogram" or a
#                                            # "density" plot on colour key
#         margins=c(10,10),                  # add space for labels
#         main=title,                        # title
#         xlab="",                           # x-axis title
#         ylab=""                            # y-axis title
#         )
#     # Turn off the device
#     dev.off()
#     }
# pic_png("heatmap.png", t(heatmap_data))
# # For larger text, change the cex_val:
# # pic_png("heatmap.png", t(heatmap_data), cex_val=3)
#### end png #####


# ##### tiff #####
# pic_tiff<-function(filename, matrix, title="", cex_val=1, pal=rch)
#     {# Start tiff device with basic settings
#     ## Fiddle with the width and height settings to get the key to
#     ## display with a 3:2 aspect ratio for best results. 8.3x5.8 is A5.
#     tiff(filename,
#         bg="white",                        # background colour
#         res=300,                           # image resolution (dpi)
#         units="in", width=8.3, height=5.8) # image dimensions (inches)
#         compression="none"                 # image compression 
#                                            #  (one of none, lzw, zip)
#         par(mgp=c(3,1,0),                  # axis margins
#                                            # (title, labels, line)
#             mar=c(5,4,4,2)                 # plot margins (b,l,t,r)
#             )
#     # Draw the plot
#      heatmap.2(matrix,                     # matrix to use
#         #Colv=FALSE,                       # reorder dendrogram
#         dendrogram="both",                 # dendrograms to draw
#         col=pal,                           # palette (see above)
#         trace="none",                      # trace line (show distance)
#         cexCol=cex_val,                    # size of column labels
#         cexRow=cex_val,                    # size of row labels
#         key=TRUE,                          # show the colour key
#         keysize=cex_val,                   # key size
#         density.info="none",               # superimpose "histogram" or a
#                                            # "density" plot on colour key
#         margins=c(5,5),                    # add space for labels
#         main=title,                        # title
#         xlab="",                           # x-axis title
#         ylab=""                            # y-axis title
#         )
#     # Turn off the device
#     dev.off()
#     }
# pic_tiff("heatmap.tif", t(heatmap_data))
# # For larger text, change the cex_val:
# # pic_tiff("heatmap.tif", t(heatmap_data), cex_val=3)
# ##### end tiff #####