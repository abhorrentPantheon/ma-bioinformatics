#    mhproc_imgs.r
#
#    Author:    Jairus Bowne
#    Purpose:    Function for MassHunter processing script to create
#                bar plots for compounds and compound summaries
#
#    Inputs:    Reads in necessary data file (.csv)
#    Outputs:    Graphs

# Get defaults for the graphical parameters that will be changed later
par_defs<-par(
    mgp=par()$mgp,
    mar=par()$mar,
    xaxt=par()$xaxt,
    yaxt=par()$yaxt,
    cex=par()$cex,
    pch=par()$pch,
    col=par()$col,
    bg=par()$bg,
    font=par()$font,
    lty=par()$lty,
    xpd=par()$xpd,
    pin=par()$pin,
    fin=par()$fin
)
dev.off()

#
#    Load the data matrices
#
# Read in the means data .csv file
means_data<-read.csv(
    paste(
        base_fn,
        "_4b_Conc_Means.csv",
        sep=""
    ),
    header=TRUE,
    sep=",",
    quote="\""
)
colnames(means_data)[-c(1:2)]<-cpd_cols
# Read in the summary data .csv file
summ_data<-read.csv(
    paste(
        base_fn,
        "_4c_Conc_Summ.csv",
        sep=""
    ),
    header=TRUE,
    sep=",",
    quote="\""
)

#
#    Prepare data for plotting
#
# Means
prep_list<-data_prepper(means_data)
split_cpds<-prep_list$split_cpds
grp_lvls<-prep_list$grp_lvls
cls_list<-prep_list$cls_list
cls_max<-prep_list$cls_max
rm(prep_list)

# Determine whether graphs will be colour or black and white
input<-readline(" ?? Do you want colour images? (y/n/c): ")
if (length(grep(input,"y",ignore.case=TRUE))!=0) {
    bw=FALSE
    write(" ** Producing colour images...","")
} else if (length(grep(input,"n",ignore.case=TRUE))!=0) {
    bw=TRUE
    write(" ** Producing black and white images...","")
} else if (length(grep(input,"c",ignore.case=TRUE))!=0) {
    write(" ## Exiting script.","")
    stop()
} else {
    write(" ## Please press y, n or c next time. Exiting...","")
    stop()
}

if (bw==FALSE) {
    # Colour patterns list
    col_list<-list(
        col=c(rep(c(
            "#ee3333",                         # red
            "#3366aa",                         # blue
            "#009973",                         # green
            "#992288",                         # purple
            "#fba300",                         # orange
            "#000000",                         # black
            "#00b8ed"                          # light blue
        ),3)),
        density=c(
            rep(-1,7),
            rep(50,7),
            rep(20,7)
        ),
        angle=c(
            rep(0,7),
            rep(c(45,-45),7)
        )
    )
} else {
    # Black and white patterns list
    bw_list<-list(
        col=c(
            "#000000", "#ffffff",
            rep("#000000",4),
            rep("#aaaaaa",4),
            rep("#dddddd",4)
        ),
        density=c(
            -1,-1,
            c(rep(15,2),rep(30,2)),
            c(rep(20,2),rep(40,2)),
            c(rep(30,2),rep(60,2))
        ),
        angle=c(
            0,0,
            rep(c(
                rep(c(45,-45),2),
                rep(c(60,-60),2)
            ),3)
        )
    )
}

#
#    Onscreen plots
#
if (bw==FALSE) {
    # Plot group data
    # multibarplot(means_data,split_cpds)
    # Plot compound data (onscreen)
    multibarplot(means_data,split_cpds,trans=FALSE)

    # Plot compound class summaries
    # summ_plot(summ_data)
    summ_plot(summ_data,trans=FALSE)
    # Turn off the blank window that appears for no discernable reason
    dev.off(2)
} else {
    # Plot group data
    # multibarplot(means_data,split_cpds,bw=TRUE)
    # Plot compound data (onscreen)
    multibarplot(means_data,split_cpds,trans=FALSE,bw=TRUE)

    # Plot compound class summaries
    # summ_plot(summ_data,bw=TRUE)
    summ_plot(summ_data,trans=FALSE,bw=TRUE)
    # Turn off the blank window that appears for no discernable reason
    dev.off(2)
}
if (norm==TRUE) {
    # Repeat the above for the normalised data
    #
    #    Load the data matrices
    #
    # Read in the means data .csv file
    norm_means_data<-read.csv(
        paste(
            base_fn,
            "_5b_Norm_Means.csv",
            sep=""
        ),
        header=TRUE,
        sep=",",
        quote="\""
    )

    # Read in the summary data .csv file
    norm_summ_data<-read.csv(
        paste(
            base_fn,
            "_5c_Norm_Summ.csv",
            sep=""
        ),
        header=TRUE,
        sep=",",
        quote="\""
    )
    #
    #    Prepare data for plotting
    #
    # Means
    prep_list<-data_prepper(norm_means_data)
    split_cpds<-prep_list$split_cpds
    grp_lvls<-prep_list$grp_lvls
    cls_list<-prep_list$cls_list
    cls_max<-prep_list$cls_max
    rm(prep_list)
    # Summaries
    #split_summs<-data_prepper(summ_data)[1]

    # Plot group data (onscreen)
    # multibarplot(norm_means_data,split_cpds,norm_plot=TRUE)
    # Plot compound data (onscreen)
    multibarplot(norm_means_data,split_cpds,norm_plot=TRUE,trans=FALSE)
    # To plot in black and white:
    #multibarplot(
    #    norm_means_data,split_cpds,norm_plot=TRUE,trans=FALSE,bw=TRUE
    #)
    # Summaries
    # summ_plot(norm_summ_data,norm_plot=TRUE)
    summ_plot(norm_summ_data,norm_plot=TRUE,trans=FALSE)
}

# To make images
input<-readline(" ?? Do you wish to save these images? (y/n/c): ")
if (length(grep(input,"y",ignore.case=TRUE))!=0) {
    save_img=TRUE
} else if (length(grep(input,"n",ignore.case=TRUE))!=0) {
    save_img=FALSE
} else if (length(grep(input,"c",ignore.case=TRUE))!=0) {
    write(" ## Exiting script.","")
    for (dev in dev.list()) {dev.off()}
    stop()
} else {
    write(" ## Please press y, n or c next time. Exiting...","")
    stop()
}

if (save_img==FALSE) {
    dev_close<-readline(" ?? Do you wish to close image windows? (y/n): ")
    if (length(grep("y",dev_close,perl=TRUE,ignore.case=TRUE)!=0)) {
        for (dev in dev.list()) {dev.off()}
    } else {
        write(
            paste(
                paste(" ## Windows will remain. Copy &",
                    "paste the line below to close them all:"
                ),
                "      for (dev in dev.list()) {dev.off()}",
                sep="\n"
            ),""
        )
    }
}

if (save_img==TRUE) {
    write(" ?? What format would you like to save the image in?","")
    input<-readline(
        paste("    1) jpeg",
            "    2) png",
            "    3) tiff",
            "    4) cancel and exit script",
            " >> Please select a number:  ",
            sep="\n"
        )
    )
    # Check that the response is a number
    if (is.na(as.numeric(input))) {
        write(" ## Please enter numbers only next time. Exiting...","")
        stop()
    }
    # Determine which device type to use
    if (input==1) {
        dev_type="jpg"
    } else if (input==2) {
        dev_type="png"
    } else if (input==3) {
        dev_type="tif"
    } else if (input>=4) {
        write(" ## Exiting script.","")
        stop()
    }
    # For colour
    if (bw==FALSE) {
        # Generate the plots
        # multibarplot(means_data,
            # split_cpds,
            # device_type=dev_type
        # )
        multibarplot(means_data,
            split_cpds,
            trans=FALSE,
            device_type=dev_type
        )
        # summ_plot(summ_data,
            # device_type=dev_type
        # )
        summ_plot(summ_data,
            trans=FALSE,
            device_type=dev_type
        )
        # And for normalised data if necessary
        if (norm==TRUE) {
            # multibarplot(norm_means_data,
                # split_cpds,
                # norm_plot=TRUE,
                # device_type=dev_type
            # )
            multibarplot(norm_means_data,
                split_cpds,
                norm_plot=TRUE,
                trans=FALSE,
                device_type=dev_type
            )
            # summ_plot(norm_summ_data,
                # norm_plot=TRUE,
                # device_type=dev_type
            # )
            summ_plot(norm_summ_data,
                norm_plot=TRUE,
                trans=FALSE,
                device_type=dev_type
            )
        }
    # For b&w
    } else {
        # Generate the plots
        # multibarplot(means_data,
            # split_cpds,
            # device_type=dev_type,
            # bw=TRUE
        # )
        multibarplot(means_data,
            split_cpds,
            trans=FALSE,
            device_type=dev_type,
            bw=TRUE
        )
        # summ_plot(summ_data,
            # device_type=dev_type
        # )
        summ_plot(summ_data,
            trans=FALSE,
            device_type=dev_type,
            bw=TRUE
        )
        # And for normalised data if necessary
        if (norm==TRUE) {
            # multibarplot(norm_means_data,
                # split_cpds,
                # norm_plot=TRUE,
                # device_type=dev_type
            # )
            multibarplot(norm_means_data,
                split_cpds,
                norm_plot=TRUE,
                trans=FALSE,
                device_type=dev_type,
                bw=TRUE
            )
            # summ_plot(norm_summ_data,
                # norm_plot=TRUE,
                # device_type=dev_type,
                # bw=TRUE
            # )
            summ_plot(norm_summ_data,
                norm_plot=TRUE,
                trans=FALSE,
                device_type=dev_type,
                bw=TRUE
            )
        }
    }

    # When satisfied, turn off devices that are still open
    if (length(dev.list())!=0) {
        close_all_win<-readline(
            " ?? Do you wish to close remaining image windows? (y/n/c): "
        )
        if (length(grep("y",close_all_win,perl=TRUE,ignore.case=TRUE)!=0)) {
            for (dev in dev.list()) {dev.off()}
        } else {
            write(
                paste(
                    paste(" ## Windows will remain. Copy &",
                        "paste the line below to close them all:"
                    ),
                    "      for (dev in dev.list()) {dev.off()}",
                    sep="\n"
                ),""
            )
        }
    }
}
