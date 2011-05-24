#    img_maker.r
#
#    Author:    Jairus Bowne, Amsha Nahid, Gerard Murray, Alysha De Livera
#    Purpose:    Function for image generation.
#                Support script for z-score.r
#
#    Input:    Z-scores matrix (from z-score script)
#    Output:    An image file

#
#    Function for production of graphics
#
img_maker<-function(filename=out_file, input_matrix=z_matrix, fmt=img_format, cex_val=1) {
    # jpeg function
    pic_jpg<-function(filename, input_matrix) {
        # Start jpeg device with basic settings
        jpeg(filename,
            quality=100,                   # image quality (percent)
            bg="white",                    # background colour
            res=300,                       # image resolution (dpi)
            units="in",                    # image dimensions (inches)
                width=8.3,
                height=5.8
        )
        par(cex=cex_val)
        # Draw the plot
        z_plot(input_matrix, x_range=c(min_x,max_x))
        dev.off()
    }
    # png function
    pic_png<-function(filename, input_matrix) {
        # Start png device with basic settings
        png(filename,
            bg="white",                    # background colour
            res=300,                       # image resolution (dpi)
            units="in",                    # image dimensions (inches)
                width=8.3,
                height=5.8
        )
        par(cex=cex_val)
        # Draw the plot
        z_plot(input_matrix, x_range=c(min_x,max_x))
        dev.off()
    }
    # tiff function
    pic_tiff<-function(filename, input_matrix) {
        # Start tiff device with basic settings
        tiff(filename,
            bg="white",                    # background colour
            res=300,                       # image resolution (dpi)
            units="in",                    # image dimensions (inches)
                width=8.3,
                height=5.8,
            compression="none"             # image compression (none, lzw, zip)
        )
        par(cex=cex_val)
        # Draw the plot
        z_plot(input_matrix, x_range=c(min_x,max_x))
        dev.off()
    }

    #
    #    Generate images
    #
    if (fmt=="jpg") {
        pic_jpg(out_file, z_matrix)
    } else if (fmt=="png") {
        pic_png(out_file, z_matrix)
    } else if (fmt=="tif") {
        pic_tiff(out_file, z_matrix)
    }

    # Inform user of output file
    write(
        paste(" -> Image saved as '", out_file, "'", sep=""),
        ""
    )
    #return(out_file)
}
