#    volcano_plot.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Produces a volcano plot
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    Plots log2(fold change) vs log10(t-test P-value)
#
#    Notes:    Group value for control must be alphanumerically first
#              Script will return an error if there are more than 2 groups

#
#    Load the data matrix
#
# Read in the .csv file
data<-read.csv("input.csv", sep=",", row.names=1, header=TRUE)
# Get groups information
groups<-data[,1]
# Get levels for groups
grp_levs<-levels(groups)
if (length(levels(groups)) > 2)
    print("Number of groups is greater than 2!") else {

    #
    #    Split the matrix by group
    #
    new_mats<-c()
    for (ii in 1:length(grp_levs))
        new_mats[ii]<-list(data[which(groups==levels(groups)[ii]),])
    
    #
    #    Calculate the means
    #
    # For each matrix, calculate the averages per column
    submeans<-c()
    # Preallocate a matrix for the means
    means<-matrix(
        nrow = 2,
        ncol = length(colnames(data[,-1])),
        dimnames = list(grp_levs,colnames(data[,-1]))
        )
    # Calculate the means for each variable per sample
    for (ii in 1:length(new_mats))
        {submeans[ii]<-list(apply(new_mats[[ii]][,-1],2,mean,na.rm=TRUE))
        means[ii,]<-submeans[[ii]]}
    
    #
    #    Calculate the fold change
    #
    folds<-matrix(
        nrow=length(means[,1]),
        ncol=length(means[1,]),
        dimnames=list(rownames(means),colnames(means))
        )
    for (ii in 1:length(means[,1]))
        for (jj in 1:length(means[1,]))
            folds[ii,jj]<-means[ii,jj]/means[1,jj]
    
    #
    #    t-test P value data
    #
    pvals<-matrix(nrow=ncol(data[,-1]),ncol=1,dimnames=list(colnames(data[-1]),"P-Value"))
    
    #
    #    Perform the t-Test
    #
    for(ii in 1:nrow(pvals)) {
        pvals[ii,1]<-t.test(new_mats[[1]][,ii+1],new_mats[[2]][,ii+1])$p.value
        }
    
    m<-length(pvals)
    x_range<-range(c(
        min(
            range(log2(folds[2,])),
            range(c(-1.5,1.5))
            ),
        max(
            range(log2(folds[2,])),
            range(c(-1.5,1.5))
            )
        ))
    y_range<-range(c(
        min(range(-log10(pvals)),
            range(c(0,2))
            ),
        max(range(-log10(pvals)),
            range(c(0,2))
            )
        ))
    
    #
    #    Plot data
    #
    # Define a function, since it's rather involved
    volcano_plot<-function(fold, pval)
        {plot(x_range,                                 # x-dim 
            y_range,                                   # y-dim
            type="n",                                  # empty plot
            xlab="log2 Fold Change",                   # x-axis title
            ylab="-log10 t-Test P-value",              # y-axis title
            main="Volcano Plot",                       # plot title
            )
            abline(h=-log10(0.05),col="green",lty="44")# horizontal line at P=0.05
            abline(v=c(-1,1),col="violet",lty="1343")  # vertical lines at 2-fold
            # Plot points based on their values:
            for (ii in 1:m)
                # If it's below 0.05, we're not overly interested: purple.
                if (-log10(pvals[ii])>(-log10(0.05))) {
                    # Otherwise, more checks;
                    # if it's greater than 2-fold decrease: blue
                    if (log2(folds[2,][ii])>(-1)) {
                        # If it's significant but didn't change much: orange
                        if (log2(folds[2,][ii])<1) {
                            points(
                                log2(folds[2,][ii]),
                                -log10(pvals[ii]),
                                col="orange",
                                pch=20
                                )
                            # Otherwise, greater than 2-fold increase: red
                            } else {
                                points(
                                    log2(folds[2,][ii]), 
                                    -log10(pvals[ii]),
                                    col="red",
                                    pch=20
                                    )
                            }
                        } else {
                            points(
                                log2(folds[2,][ii]), 
                                -log10(pvals[ii]),
                                col="blue",
                                pch=20
                                )
                            }
                    } else {
                        points(
                            log2(folds[2,][ii]),
                            -log10(pvals[ii]),
                            col="purple",
                            pch=20
                            )
                    }
        }

    # Generate folds list for output table
    Fold_change<-folds[2,]
    # Join data
    data_table<-cbind(Fold_change,pvals)
    # Plot onscreen via function
    x11()
    volcano_plot(folds,pvals)
    # You could also use:
    #volcano_plot(data_table)

    # Return table to analyse results
    write.csv(data_table,"volcano_plot_data.csv")
    #
    #    Generate figures as image files
    #
    #    (Uncomment blocks as necessary)
    
    ##### jpeg #####
    # pic_jpg<-function(filename, fold, pval)
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
    #     volcano_plot(folds, pvals)
    #     dev.off()
    #     }
    # pic_jpg("volcano_plot.jpg")
    ##### end jpeg #####
    
    
    #### png #####
    # pic_png<-function(filename, fold, pval)
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
    #     volcano_plot(folds, pvals)
    #     dev.off()
    #     }
    # pic_png("volcano_plot.png")
    #### end png #####
    
    
    # #### tiff #####
    # pic_tiff<-function(filename, fold, pval)
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
    #     volcano_plot(folds, pvals)
    #     dev.off()
    #     }
    # pic_tiff("volcano_plot.tif")
    # #### end tiff #####




    #
    #    Legacy code which allows for blue/red to be independent of 0.05
    #    (purple is limited to the middle lower region)
    #
    #####
    #     for (ii in 1:m)
    #         if (log2(folds[2,][ii])<1) {
    #             if (log2(folds[2,][ii])>-1) {
    #                 if (-log10(pvals[ii])<(-log10(0.05))) {
    #                     points(
    #                         log2(folds[2,][ii]),
    #                         -log10(pvals[ii]),
    #                         col="purple",
    #                         pch=20
    #                         )
    #                     } else {
    #                         points(
    #                             log2(folds[2,][ii]), 
    #                             -log10(pvals[ii]),
    #                             col="orange",
    #                             pch=20
    #                             )
    #                     }
    #                 } else {
    #                     points(
    #                         log2(folds[2,][ii]), 
    #                         -log10(pvals[ii]),
    #                         col="blue",
    #                         pch=20
    #                         )
    #                     }
    #             } else {
    #                 points(
    #                     log2(folds[2,][ii]),
    #                     -log10(pvals[ii]),
    #                     col="red",
    #                     pch=20
    #                     )
    #             }

# If function from above needs to be closed
}