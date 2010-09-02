#    pca.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Perform PCA using prcomp command
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    PCA loading and score plots
#
#    Notes:    Missing values (if any) are replaced by the half of the lowest
#              value in the entire data matrix.
#              Data should be log transformed prior to using this script


#
#    Load necessary libraries, and install them if they are missing
#
### Why are we using vegan? Is there something in base that could do same?
tryCatch(library(vegan), error=function(err)
    install.packages("vegan",repos="http://cran.ms.unimelb.edu.au/"))

#
#    Prepare data matrix
#
# Read in the .csv file
data<-read.csv("input.csv", sep=",", row.names=1, header=TRUE)
# Get groups information
groups<-data[,1]
# Remove groups for data processing
pca_data<-data[,-1]
# Replace any missing values (see Notes)
pca_data[is.na(pca_data)]<-0.5*(min(pca_data,na.rm=TRUE))

#
#    Perform PCA
#
pca<-prcomp(pca_data,scale=TRUE)
# Get the eigenvectors (loadings)
eigenvecs<-pca$rotation

# Edit the column names if necessary
colnames(pca_data) <- if 
    (length(grep("^X[\\d]",colnames(pca_data),perl=TRUE)) != 0) # then
    {gsub("^X([\\d].*)","\\1",colnames(pca_data),perl=TRUE)} else
    {colnames(pca_data)}

# Get summary information
summ<-summary(pca)
importance<-summ$importance[2,]

# Plot the explained variance
barplot(summ$importance[2,],
            col="red",                            # colour to plot bars
            main="Variance",                      # plot title
            xlab="Principal Component",           # x-axis title
            ylab="Explained Variance",            # y-axis title
            cex.axis=1, cex.names=1,              # font size
            las=1                                 # horizontal labels
            )

### This comes from vegan lib. Can we do this without loading that lib?
pca_scores<-scores(pca)

rownames(pca_scores)<-rownames(pca_data)

#
#    Generate the figures (on screen)
#
#    Image generation - function definition
pic_onscr<-function(matrix, title, labels=rownames(matrix),
            x_label="", y_label="", cex_val=1)
    {x11()
    par(mgp=c(5,2,0),                          # axis margins 
                                               # (title, labels, line)
        mar=c(6,6,4,2),                         # plot margins (b,l,t,r)
        las=1                                  # horizontal labels
        )
    #pch=... for point shape by group - not yet implemented
    # Append the following to the previous par call?
    #par(pch=c(15,0,16,1,17,2,18,5))
        plot(matrix[,1], matrix[,2],              # x, y values
            main=title,                           # title of plot
            xlab=x_label,                         # x-axis title
            ylab=y_label,                         # y-axis title
            cex=cex_val)                          # font size
        abline(h=0, col="red", lwd=1)             # line across plot area
        abline(v=0, col="blue", lwd=1)
        # Labels on RHS of points (pos: bottom=1, left=2, top=3, right=4)
        text(matrix[,1], matrix[,2], labels, cex=cex_val, pos=4)
    }
# Plot PCA scores with sample names
pic_onscr(pca_scores,"PCA Score Plot\nSamples",
x_label="PC1", y_label="PC2")
# Plot PCA scores with group names
pic_onscr(pca_scores,"PCA Score Plot\nGroups", labels=groups,
x_label="PC1", y_label="PC2")
# Loadings plot
pic_onscr(eigenvecs,"PCA Loading Plot",
x_label="PC1", y_label="PC2")

#
#    Generate figures as image files
#
#    (Uncomment blocks as necessary)

##### jpeg #####
# pic_jpg<-function(filename, matrix, title, labels=rownames(matrix),
#            x_label="", y_label="", cex_val=1)
#     {# Start jpeg device with basic settings
#     jpeg(filename,
#         quality=100,                           # image quality (percent)
#         bg="white",                            # background colour
#         res=300,                               # image resolution (dpi)
#         units="in", width=8.3, height=5.8)     # image dimensions (inches)
#     par(mgp=c(5,2,0),                          # axis margins 
#                                                # (title, labels, line)
#         mar=c(6,6,4,2),                        # plot margins (b,l,t,r)
#         las=1                                  # horizontal labels
#         )
#     # Draw the plot
#     plot(matrix[,1], matrix[,2],
#         xlab=x_label, ylab=y_label, main=title, cex=cex_val)
#     # Draw in the zero-axes
#     abline(h=0, col="red", lwd=2)
#     abline(v=0, col="blue", lwd=2)
#     # Label the points
#     text(matrix[,1], matrix[,2], labels, pos=4, cex=cex_val)
#     dev.off()
#     }
# # Variance plot
# jpeg("PCA_variance.jpg",
#     quality=100,                               # image quality (percent)
#     bg="white",                                # background colour
#     res=300,                                   # image resolution (dpi)
#     units="in", width=8.3, height=5.8)         # image dimensions (inches)
#     {barplot(summ$importance[2,],
#         col="red",                             # colour of bars
#         main="Variance",                       # plot title
#         xlab="Principal Component",            # x-axis title
#         ylab="Explained variance",             # y-axis title
#         cex.axis=1, cex.names=1,               # font size
#         las=1)}                                # horizontal labels
# dev.off()
# # Other plots
# pic_jpg("PCA_scores_sample.jpg", pca_scores, "PCA Score Plot\nSamples",
#     x_label="PC1", y_label="PC2")
# pic_jpg("PCA_scores_group.jpg", pca_scores, "PCA Score Plot\nGroups",
#     labels=groups, x_label="PC1", y_label="PC2")
# pic_jpg("PCA_loadings.jpg", eigenvecs, "PCA Loading Plot",
#     x_label="PC1", y_label="PC2")
##### end jpeg #####


##### png #####
# pic_png<-function(filename,                    # name of output file
#     matrix,                                    # data matrix to use
#     title,                                     # title of plot
#     labels=rownames(matrix),                   # labels to use for plot
#     x_label="", y_label="",                    # axis labels
#     cex_val=1                                  # font size
#     )
#     {# Start png device with basic settings
#     png(filename,
#         bg="white",                            # background colour
#         res=300,                               # image resolution (dpi)
#         units="in", width=8.3, height=5.8)     # image dimensions (inches)
#     par(mgp=c(5,2,0),                          # axis margins 
#                                                # (title, labels, line)
#         mar=c(6,6,4,2),                        # plot margins (b,l,t,r)
#         las=1                                  # horizontal labels
#         )
#     # Draw the plot
#     plot(matrix[,1], matrix[,2],
#         xlab=x_label, ylab=y_label, main=title, cex=cex_val)
#     # Draw in the 0-axes
#     abline(h=0, col="red", lwd=2)
#     abline(v=0, col="blue", lwd=2)
#     # Label the points
#     text(matrix[,1], matrix[,2], labels, pos=4, cex=cex_val)
#     dev.off()
#     }
# # Variance plot
# png("PCA_variance.png",
#     bg="white",                                # background colour
#     res=300,                                   # image resolution (dpi)
#     units="in", width=8.3, height=5.8)         # image dimensions (inches)
#     {barplot(summ$importance[2,],
#         col="red",                             # colour of bars
#         main="Variance",                       # plot title
#         xlab="Principal Component",            # x-axis title
#         ylab="Explained variance",             # y-axis title
#         cex.axis=1, cex.names=1,               # font size
#         las=1)}                                # horizontal labels
# dev.off()
# # Other plots
# pic_png("PCA_scores_sample.png", pca_scores, "PCA Score Plot\nSamples",
#     x_label="PC1", y_label="PC2")
# pic_png("PCA_scores_group.png", pca_scores, "PCA Score Plot\nGroups",
#     labels=groups, x_label="PC1", y_label="PC2")
# pic_png("PCA_loadings.png", eigenvecs, "PCA Loading Plot",
#     x_label="PC1", y_label="PC2")
##### end png #####


# #### tiff #####
# pic_tiff<-function(filename, matrix, title, labels=rownames(matrix),
#            x_label="", y_label="", cex_val=1)
#     {# Start tiff device with basic settings
#     tiff(filename,
#         bg="white",                            # background colour
#         res=300,                               # image resolution (dpi)
#         units="in", width=8.3, height=5.8)     # image dimensions (inches)
#         compression="none"                     # image compression 
#                                                #  (one of none, lzw, zip)
#     par(mgp=c(5,2,0),                          # axis margins 
#                                                # (title, labels, line)
#         mar=c(6,6,4,2),                        # plot margins (b,l,t,r)
#         las=1                                  # horizontal labels
#         )
#     # Draw the plot
#     plot(matrix[,1], matrix[,2],
#         xlab=x_label, ylab=y_label, main=title, cex=cex_val)
#     # Draw in the 0-axes
#     abline(h=0, col="red", lwd=2)
#     abline(v=0, col="blue", lwd=2)
#     # Label the points
#     text(matrix[,1], matrix[,2], labels, pos=4, cex=cex_val)
#     dev.off()
#     }
# # Variance plot
# tiff("PCA_variance.tif",
#     bg="white",                                # background colour
#     res=300,                                   # image resolution (dpi)
#     units="in", width=8.3, height=5.8)         # image dimensions (inches)
#     {barplot(summ$importance[2,],
#         col="red",                             # colour of bars
#         main="Variance",                       # plot title
#         xlab="Principal Component",            # x-axis title
#         ylab="Explained variance",             # y-axis title
#         cex.axis=1, cex.names=1,               # font size
#         las=1)}                                # horizontal labels
# dev.off()
# # Other plots
# pic_tiff("PCA_scores_sample.tif", pca_scores, "PCA Score Plot\nSamples",
#     x_label="PC1", y_label="PC2")
# pic_tiff("PCA_scores_group.tif", pca_scores, "PCA Score Plot\nGroups",
#     labels=groups, x_label="PC1", y_label="PC2")
# pic_tiff("PCA_loadings.tif", eigenvecs, "PCA Loading Plot",
#     x_label="PC1", y_label="PC2")
# #### end tiff #####