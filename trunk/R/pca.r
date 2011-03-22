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
tryCatch(
    library(vegan), 
    error=function(err) {
        # if this produces an error:
        install.packages("vegan",
            repos="http://cran.ms.unimelb.edu.au/"
        )
    }
)

#
#    Prepare the data matrix
#
# Read in the .csv file
in_file<-file.choose()
input_data<-read.csv(in_file, sep=",", row.names=1, header=TRUE)

# Get groups information
groups<-input_data[,1]
# Remove groups for data processing
pca_data<-input_data[,-1]
# Replace any missing values (see Notes)
pca_data[is.na(pca_data)]<-0.5*(min(pca_data,na.rm=TRUE))

#
#    Perform PCA
#
pca<-prcomp(pca_data,scale=TRUE)
# Get the eigenvectors (loadings)
eigenvecs<-pca$rotation

# Edit the column names if necessary
colnames(pca_data) <- if (
    length(
        grep("^X[\\d]",colnames(pca_data),perl=TRUE)
    ) != 0
) {# then
    gsub("^X([\\d].*)","\\1",colnames(pca_data),perl=TRUE)
} else {
    colnames(pca_data)
}

# Get summary information
summ<-summary(pca)
importance<-summ$importance[2,]

# Plot the explained variance
barplot(summ$importance[2,],
    col="red",                             # colour to plot bars
    main="Variance",                       # plot title
    xlab="Principal Component",            # x-axis title
    ylab="Explained Variance",             # y-axis title
    cex.axis=1, cex.names=1,               # font size
    las=1                                  # horizontal labels
)

### This comes from vegan lib. Can we do this without loading that lib?
pca_scores<-scores(pca)

rownames(pca_scores)<-rownames(pca_data)

#
#    Generate the figures (on screen)
#
#    Image generation - function definition
pic_onscr<-function(
    input_matrix, 
    plot_title, 
    plot_labels=rownames(input_matrix),
    x_label="", y_label="", 
    cex_val=1
) {
    x11()
    par(mgp=c(5,2,0),                      # axis margins 
                                           # (title, labels, line)
        mar=c(6,6,4,2),                    # plot margins (b,l,t,r)
        las=1                              # horizontal labels
    )
    #pch=... for point shape by group - not yet implemented
    # Append the following to the previous par call?
    #par(pch=c(15,0,16,1,17,2,18,5))
    plot(input_matrix[,1],                 # x values (PC1)
        input_matrix[,2],                  # y values (PC2)
        main=plot_title,                   # title of plot
        xlab=x_label,                      # x-axis title
        ylab=y_label,                      # y-axis title
        cex=cex_val                        # font size
    )
    abline(h=0, col="red", lwd=1)          # line across plot area
    abline(v=0, col="blue", lwd=1)
    
    # Labels on RHS of points if not near top/right edge
    # (pos: bottom=1, left=2, top=3, right=4)
    pos_list<-c()
    for (ii in 1:dim(input_matrix)[1]) {
        if(input_matrix[ii,2]<0.95*max(range(input_matrix[,2]))) {
            if(input_matrix[ii,1]<0.75*max(range(input_matrix[,1]))) {
                pos_list[ii]<-4            # right if it's neither
            } else {
                pos_list[ii]<-2            # left if > 0.75 max(x_range)
            }
        } else {
            pos_list[ii]<-1                # bottom if > 0.95 max(y_range)
        }
    }

    # Label the points
    text(input_matrix[,1],
        input_matrix[,2], 
        plot_labels, 
        cex=cex_val, 
        pos=pos_list
    )
}
# Plot PCA scores with sample names
pic_onscr(pca_scores,
    "PCA Score Plot\nSamples",
    x_label="PC1", 
    y_label="PC2"
)
# Plot PCA scores with group names
pic_onscr(pca_scores,
    "PCA Score Plot\nGroups",
    plot_labels=groups,
    x_label="PC1",
    y_label="PC2"
)
# Loadings plot
pic_onscr(eigenvecs,
    "PCA Loading Plot",
    x_label="PC1", 
    y_label="PC2"
)

#
#    Generate figures as image files
#
#    (Uncomment blocks as necessary)

##### jpeg #####
# pic_jpg<-function(filename,                # name of output file
#     input_matrix,                          # data matrix to use
#     plot_title,                            # title of plot
#     plot_labels=rownames(input_matrix),    # labels to use for plot
#     x_label="", y_label="",                # axis labels
#     cex_val=1                              # font size
# ) { # Start jpeg device with basic settings
#     jpeg(filename,
#         quality=100,                       # image quality (percent)
#         bg="white",                        # background colour
#         res=300,                           # image resolution (dpi)
#         units="in", width=8.3, height=5.8  # image dimensions (inches)
#     )
#     par(mgp=c(5,2,0),                      # axis margins 
#                                            # (title, labels, line)
#         mar=c(6,6,4,2),                    # plot margins (b,l,t,r)
#         las=1                              # horizontal labels
#     )
#     # Draw the plot
#     plot(input_matrix[,1], 
#         input_matrix[,2],
#         xlab=x_label, 
#         ylab=y_label, 
#         main=plot_title, 
#         cex=cex_val
#     )
#     # Draw in the zero-axes
#     abline(h=0, col="red", lwd=2)
#     abline(v=0, col="blue", lwd=2)
#     
#     # Generate labels
#     pos_list<-c()
#     for (ii in 1:dim(input_matrix)[1]) {
#         if(input_matrix[ii,2]<0.95*max(range(input_matrix[,2]))) {
#             if(input_matrix[ii,1]<0.75*max(range(input_matrix[,1]))) {
#                 pos_list[ii]<-4            # right if it's neither
#             } else {
#                 pos_list[ii]<-2            # left if > 0.75 max(x_range)
#             }
#         } else {
#             pos_list[ii]<-1                # bottom if > 0.95 max(y_range)
#         }
#     }
# 
#     # Label the points
#     text(input_matrix[,1],
#         input_matrix[,2], 
#         plot_labels, 
#         cex=cex_val, 
#         pos=pos_list
#     )
#     dev.off()
# }
# # Variance plot
# jpeg("PCA_variance.jpg",
#     quality=100,                           # image quality (percent)
#     bg="white",                            # background colour
#     res=300,                               # image resolution (dpi)
#     units="in", width=8.3, height=5.8      # image dimensions (inches)
# )
# barplot(summ$importance[2,],
#     col="red",                             # colour of bars
#     main="Variance",                       # plot title
#     xlab="Principal Component",            # x-axis title
#     ylab="Explained variance",             # y-axis title
#     cex.axis=1, cex.names=1,               # font size
#     las=1                                  # horizontal labels
# )
# dev.off()
# # Other plots
# pic_jpg("PCA_scores_sample.jpg", 
#     pca_scores, 
#     "PCA Score Plot\nSamples",
#     x_label="PC1",
#     y_label="PC2"
# )
# pic_jpg("PCA_scores_group.jpg",
#     pca_scores,
#     "PCA Score Plot\nGroups",
#     plot_labels=groups,
#     x_label="PC1",
#     y_label="PC2"
# )
# pic_jpg("PCA_loadings.jpg",
#     eigenvecs,
#     "PCA Loading Plot",
#     x_label="PC1",
#     y_label="PC2"
# )
##### end jpeg #####


##### png #####
# pic_png<-function(filename,                # name of output file
#     input_matrix,                          # data matrix to use
#     plot_title,                            # title of plot
#     plot_labels=rownames(input_matrix),    # labels to use for plot
#     x_label="", y_label="",                # axis labels
#     cex_val=1                              # font size
# ) { # Start png device with basic settings
#     png(filename,
#         bg="white",                        # background colour
#         res=300,                           # image resolution (dpi)
#         units="in", width=8.3, height=5.8  # image dimensions (inches)
#     )
#     par(mgp=c(5,2,0),                      # axis margins 
#                                            # (title, labels, line)
#         mar=c(6,6,4,2),                    # plot margins (b,l,t,r)
#         las=1                              # horizontal labels
#     )
#     # Draw the plot
#     plot(input_matrix[,1], 
#         input_matrix[,2],
#         xlab=x_label, 
#         ylab=y_label, 
#         main=plot_title, 
#         cex=cex_val
#     )
#     # Draw in the 0-axes
#     abline(h=0, col="red", lwd=2)
#     abline(v=0, col="blue", lwd=2)
#     
#     # Generate labels
#     pos_list<-c()
#     for (ii in 1:dim(input_matrix)[1]) {
#         if(input_matrix[ii,2]<0.95*max(range(input_matrix[,2]))) {
#             if(input_matrix[ii,1]<0.75*max(range(input_matrix[,1]))) {
#                 pos_list[ii]<-4            # right if it's neither
#             } else {
#                 pos_list[ii]<-2            # left if > 0.75 max(x_range)
#             }
#         } else {
#             pos_list[ii]<-1                # bottom if > 0.95 max(y_range)
#         }
#     }
#     
#     # Label the points
#     text(input_matrix[,1],
#         input_matrix[,2], 
#         plot_labels, 
#         cex=cex_val, 
#         pos=pos_list
#     )
#     dev.off()
# }
# # Variance plot
# png("PCA_variance.png",
#     bg="white",                            # background colour
#     res=300,                               # image resolution (dpi)
#     units="in", width=8.3, height=5.8      # image dimensions (inches)
# )
# barplot(summ$importance[2,],
#     col="red",                             # colour of bars
#     main="Variance",                       # plot title
#     xlab="Principal Component",            # x-axis title
#     ylab="Explained variance",             # y-axis title
#     cex.axis=1, cex.names=1,               # font size
#     las=1                                  # horizontal labels
# )
# dev.off()
# # Other plots
# pic_png("PCA_scores_sample.png",
#     pca_scores,
#     "PCA Score Plot\nSamples",
#     x_label="PC1",
#     y_label="PC2"
# )
# pic_png("PCA_scores_group.png",
#     pca_scores,
#     "PCA Score Plot\nGroups",
#     plot_labels=groups,
#     x_label="PC1",
#     y_label="PC2"
# )
# pic_png("PCA_loadings.png",
#     eigenvecs,
#     "PCA Loading Plot",
#     x_label="PC1",
#     y_label="PC2"
# )
##### end png #####


##### tiff #####
# pic_tiff<-function(filename,               # name of output file
#     input_matrix,                          # data matrix to use
#     plot_title,                            # title of plot
#     plot_labels=rownames(input_matrix),    # labels to use for plot
#     x_label="", y_label="",                # axis labels
#     cex_val=1                              # font size
# ) { # Start tiff device with basic settings
#     tiff(filename,
#         bg="white",                        # background colour
#         res=300,                           # image resolution (dpi)
#         units="in", width=8.3, height=5.8, # image dimensions (inches)
#         compression="none"                 # image compression 
#     )                                      # (one of none, lzw, zip)
#     par(mgp=c(5,2,0),                      # axis margins 
#                                            # (title, labels, line)
#         mar=c(6,6,4,2),                    # plot margins (b,l,t,r)
#         las=1                              # horizontal labels
#     )
#     # Draw the plot
#     plot(input_matrix[,1], 
#         input_matrix[,2],
#         xlab=x_label, 
#         ylab=y_label, 
#         main=plot_title, 
#         cex=cex_val
#     )
#     # Draw in the 0-axes
#     abline(h=0, col="red", lwd=2)
#     abline(v=0, col="blue", lwd=2)
#     
#     # Generate labels
#     pos_list<-c()
#     for (ii in 1:dim(input_matrix)[1]) {
#         if(input_matrix[ii,2]<0.95*max(range(input_matrix[,2]))) {
#             if(input_matrix[ii,1]<0.75*max(range(input_matrix[,1]))) {
#                 pos_list[ii]<-4            # right if it's neither
#             } else {
#                 pos_list[ii]<-2            # left if > 0.75 max(x_range)
#             }
#         } else {
#             pos_list[ii]<-1                # bottom if > 0.95 max(y_range)
#         }
#     }
#     
#     # Label the points
#     text(input_matrix[,1],
#         input_matrix[,2], 
#         plot_labels, 
#         cex=cex_val, 
#         pos=pos_list
#     )
#     dev.off()
# }
# # Variance plot
# tiff("PCA_variance.tif",
#     bg="white",                            # background colour
#     res=300,                               # image resolution (dpi)
#     units="in", width=8.3, height=5.8,     # image dimensions (inches)
#     compression="none"                     # image compression 
# )                                          # (one of none, lzw, zip)
# barplot(summ$importance[2,],
#     col="red",                             # colour of bars
#     main="Variance",                       # plot title
#     xlab="Principal Component",            # x-axis title
#     ylab="Explained variance",             # y-axis title
#     cex.axis=1, cex.names=1,               # font size
#     las=1                                  # horizontal labels
# )
# dev.off()
# # Other plots
# pic_tiff("PCA_scores_sample.tif",
#     pca_scores,
#     "PCA Score Plot\nSamples",
#     x_label="PC1",
#     y_label="PC2"
# )
# pic_tiff("PCA_scores_group.tif",
#     pca_scores,
#     "PCA Score Plot\nGroups",
#     plot_labels=groups,
#     x_label="PC1",
#     y_label="PC2"
# )
# pic_tiff("PCA_loadings.tif",
#     eigenvecs,
#     "PCA Loading Plot",
#     x_label="PC1",
#     y_label="PC2"
# )
##### end tiff #####
