#    z_plot.r
#
#    Author:    Jairus Bowne, Amsha Nahid, Gerard Murray, Alysha De Livera
#    Purpose:    Function to display the z-scores plot
#                Support script for z-score.r
#
#    Input:    Z-scores matrix (from z-score script)
#    Output:    Onscreen plot of Z-scores

#
#    Function to produce onscreen plot
#
z_plot<-function(input_matrix=z_matrix, x_range=range(input_matrix)) {
    # Generate list of point symbols
    pch_list<-c(                           # square, circle, triangle, diamond
        15,16,17,18,                       # as solid
        0,1,2,5,                           # as open
        7,13,9                             # as open with cross (no triangle)
    )
    # Generate a list of colours (chosen to be colourblind safe)
    col_list<-c("#ee3333",                 # red
        "#3366aa",                         # blue
        "#009973",                         # green
        "#992288",                         # purple
        "#fba300",                         # orange
        "#000000",                         # black
        "#00b8ed"                          # light blue
    )
    # col_list is the shorter vector; ensure this is
    # long enough to handle the number of groups
    colour_reps<-ceiling(length(grp_lvls)/length(col_list))
    col_list<-c(rep(col_list,colour_reps))
    # Use the same value to ensure pch_list is long enough also
    pch_list<-c(rep(pch_list,colour_reps))

    #
    #    Plot z-score data
    #
    # Get defaults for the values that will be changed
    par_defs<-par(
        pin=par()$pin,
        xpd=par()$xpd,
        fin=par()$fin,
        cex=par()$cex,
        mar=par()$mar
    )
    # Get length of longest group name
    max_nchar<-8
    for (lvl in grp_lvls) {
        if (nchar(lvl)>max_nchar) {
            max_nchar<-nchar(lvl)
        }
    }
    # Change the graphical paramaters
    par(pin=c(
            par()$pin[1]/2,                # plot dimensions (inches)
            par()$pin[2]
        ),
        xpd=TRUE,
        fin=c(
            par()$fin[1]/2,                # figure dimensions
            par()$fin[2]
        ),             
        mar=par()$mar+c(0,0,0,0.75*max_nchar)
    )
    # If number of points is greater than 15, scale character size
    if (length(colnames(input_data))>15) {
        scale_val<-length(colnames(input_data))*.03
        par(cex=par()$cex/scale_val
        )
    }
    # Empty plot window
    plot(1,
        main="Z-score plot",
        type="n",                          # don't plot any data
        xlim=x_range,                      # dimensions for x-axis
        xlab="Z-score",                    # x-axis title
        ylim=c(nrow(input_matrix),1),      # dimensions for y-axis
                                           # (length of matrix to 1)
        ylab="",                           # y-axis title
        yaxt="n",                          # suppress y-axis tick mark labels
        lab=c(10,10,7)                     # tick marks on axes 
                                           # (x,y,[unused value])
    )
    # Change the y-axis
    axis(side=2,                           #left y-axis
        at=seq(nrow(input_matrix)),        
        labels=rownames(input_matrix),
        las=1
    )
    # Plot the points
    for (lvl in grp_lvls) {
        for (ii in 1:dim(input_matrix)[1]) {
            # Generate a list of interim x-values
            tmp_im<-c()
            
            tmp_im<-input_matrix[ii,which(input_data$Group==lvl)]
            # Generate a list of the plotting symbols to use
            tmp_pch<-rep(pch_list[which(grp_lvls==lvl)],length(tmp_im))
#####
### To plot group symbols instead of a cross, comment the following
#
           for (jj in 1:length(tmp_im)) {
               if (tmp_im[jj] > x_range[2]) {
                   tmp_pch[jj]<-4
               } else if (tmp_im[jj] < x_range[1]) {
                   tmp_pch[jj]<-4
               } else {
                   tmp_pch[jj]<-as.numeric(tmp_pch[jj])
               }
           }
#
### end of comment section
#####
            # If x-values are outside the x-range, change the value
            if (max(tmp_im) > x_range[2]) {
                    tmp_im[tmp_im > x_range[2]]<-x_range[2]
            } else if (min(tmp_im) < x_range[1]) {
                    tmp_im[tmp_im < x_range[1]]<-x_range[1]
            }
            points(
                # x-values
                tmp_im,
                # y-values
                rep(ii,dim(matrix_list[[lvl]])[2]),
                # symbol
                pch=tmp_pch,
                # colour
                col=col_list[which(grp_lvls==lvl)]
            )
        }
    }
    # Add in a legend
#####
### To plot group symbols instead of a cross, comment the following
#
   if (min(z_matrix)<x_range[1]) {
       legend("left",                      # position of legend
            inset=1.05,                    # 1 is RHS of plot area
            c(grp_lvls,"Out of range"),    # labels on legend
            col=c(col_list[1:length(grp_lvls)],"#000000"),
            cex=1,
        )
    } else if (max(z_matrix)>x_range[2]) {
        legend("left",                     # position of legend
            inset=1.05,                    # 1 is RHS of plot area
            c(grp_lvls,"Out of range"),    # labels on legend
            pch=c(pch_list[1:length(grp_lvls)],4),
            col=c(col_list[1:length(grp_lvls)],"#000000"),
            cex=1,
        )
    } else {
#
### end of comment section
#####
        legend("left",                     # position of legend
            inset=1.05,                    # 1 is RHS of plot area
            grp_lvls,                      # labels on legend
            pch=pch_list,
            col=col_list,
            cex=1,
        )
#####
###    Also comment this brace:
#
    }
#
###
#####
    # Restore the graphical parameters to their defaults
    par(par_defs)
}
