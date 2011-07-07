#    hca.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Generates HCA dendrogram plot for samples (with and
#                without grouping) as well as for variables
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#
#    Notes:    Uses Manhattan distance and complete linkage methods. Others
#              may be specified - see help(dist) or help(hclust).
#              Data matrix should be normalised and (log) transformed prior
#              to using this script

# # Determine which variables/objects are present before running script
# rm_list<-list()
# rm_list$pre=ls()

#
#    Prepare the data matrix
#
# Read in the .csv file
in_file<-file.choose()
input_data<-read.csv(in_file, sep=",", row.names=1, header=TRUE)

# Get groups information
groups<-input_data[,1]
# Remove groups for data processing
hca_data<-input_data[,-1]

# Edit the column names if necessary
colnames(hca_data) <- if (
    length(
        grep("^X[\\d]",colnames(hca_data),perl=TRUE)
    ) != 0
) {# then
    gsub("^X([\\d].*)","\\1",colnames(hca_data),perl=TRUE)
} else {
    colnames(hca_data)
}

# Prepare distance matrix for samples
dist_sample<-dist(hca_data,method="manhattan")
hca_sample<-hclust(dist_sample,method="complete")

# Prepare distance matrix for variables
dist_variable<-dist(t(hca_data),method="manhattan")
hca_variable<-hclust(dist_variable,method="complete")

smpl_labels<-NA
for(ii in 1:length(rownames(hca_data))) {
    smpl_labels[ii]<-paste(
        groups[ii], ": ", rownames(hca_data)[ii], sep=""
    )
}

# Dendrogram of samples
x11()
plot(hca_sample,
    labels=smpl_labels,                    # group labels
    hang=-1,                               # even-length ends
    main="Cluster Dendrogram\nSamples",    # title of plot
    xlab="",                               # change the x-axis title
    sub="",                                # clear the sub-heading
    cex=1                                  # font size
)

# Dendrogram of variables
x11()
plot(hca_variable,
    hang=-1,                               # even-length ends
    main="Cluster Dendrogram\nVariables",  # title of plot
    xlab="",                               # change the x-axis title
    sub="",                                # clear the sub-heading
    cex=1                                  # font size
)
#
#    Generate figures as image files
#
#    (Uncomment blocks as necessary)

##### jpeg #####
# jpeg("HCA_samples.jpg",                    # output filename
#     quality=100,                           # image quality (percent)
#     bg="white",                            # background colour
#     res=300,                               # image resolution (dpi)
#     units="in", width=8.3, height=5.8      # image dimensions (inches)
# )
# plot(hca_sample,
#     labels=smpl_labels,                    # group labels
#     hang=-1,                               # even-length ends
#     main="Cluster Dendrogram\nSamples",    # title of plot
#     xlab="",                               # x-axis title
#     sub="",                                # clear the sub-heading
#     cex=1                                  # font size
# )
# dev.off()
# 
# jpeg("HCA_variables.jpg",                  # output filename
#     quality=100,                           # image quality (percent)
#     bg="white",                            # background colour
#     res=300,                               # image resolution (dpi)
#     units="in", width=8.3, height=5.8      # image dimensions (inches)
# )
# plot(hca_variable,
#     hang=-1,                               # even-length ends
#     main="Cluster Dendrogram\nVariables",  # title of plot
#     xlab="",                               # x-axis title
#     sub="",                                # clear the sub-heading
#     cex=1                                  # font size
# )
# dev.off()
##### end jpeg #####


#### png #####
# png("HCA_samples.png",                     # output filename
#     bg="white",                            # background colour
#     res=300,                               # image resolution (dpi)
#     units="in", width=8.3, height=5.8      # image dimensions (inches)
# )
# plot(hca_sample,
#     labels=smpl_labels,                    # group labels
#     hang=-1,                               # even-length ends
#     main="Cluster Dendrogram\nSamples",    # title of plot
#     xlab="",                               # x-axis title
#     sub="",                                # clear the sub-heading
#     cex=1                                  # font size
# )
# dev.off()
# 
# png("HCA_variables.png",                   # output filename
#     bg="white",                            # background colour
#     res=300,                               # image resolution (dpi)
#     units="in", width=8.3, height=5.8      # image dimensions (inches)
# )
# plot(hca_variable,
#     hang=-1,                               # even-length ends
#     main="Cluster Dendrogram\nVariables",  # title of plot
#     xlab="",                               # x-axis title
#     sub="",                                # clear the sub-heading
#     cex=1                                  # font size
# )
# dev.off()
#### end png #####


##### tiff #####
# tiff("HCA_samples.tif",                    # output filename
#     bg="white",                            # background colour
#     res=300,                               # image resolution (dpi)
#     units="in", width=8.3, height=5.8,     # image dimensions (inches)
#     compression="none"                     # image compression 
# )                                          #  (one of none, lzw, zip)
# plot(hca_sample,
#     labels=smpl_labels,                    # group labels
#     hang=-1,                               # even-length ends
#     main="Cluster Dendrogram\nSamples",    # title of plot
#     xlab="",                               # x-axis title
#     sub="",                                # clear the sub-heading
#     cex=1                                  # font size
# )
# dev.off()
# 
# tiff("HCA_variables.tif",                  # output filename
#     bg="white",                            # background colour
#     res=300,                               # image resolution (dpi)
#     units="in", width=8.3, height=5.8,     # image dimensions (inches)
#     compression="none"                     # image compression 
# )                                          #  (one of none, lzw, zip)
# plot(hca_variable,
#     hang=-1,                               # even-length ends
#     main="Cluster Dendrogram\nVariables",  # title of plot
#     xlab="",                               # x-axis title
#     sub="",                                # clear the sub-heading
#     cex=1                                  # font size
# )
# dev.off()
# ##### end tiff #####

# #
# #    Tidy up
# #
# # List all objects
# rm_list$post=ls()
# # Remove objects in rm_list$post that aren't in rm_list$pre
# rm(list=rm_list$post[which(rm_list$pre!=rm_list$post)])
# rm(rm_list)
