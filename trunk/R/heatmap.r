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
