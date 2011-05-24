#    img_type.r
#
#    Author:    Jairus Bowne, Amsha Nahid, Alysha De Livera
#    Purpose:    Select format of final image
#                Support script for z-score.r

#
#    Function for determing output format of graphics
#
img_type<-function() {
    write(" ?? In which format would you like to save the image?","")
    img_q<-readline(
        paste("    1) jpeg",
            "    2) png",
            "    3) tiff",
            "   (press enter to cancel)",
            " >> Please select a number: ",
            sep="\n"
        )
    )
    # Ensure warnings are enabled
    options(show.error.messages=TRUE,warn=0)
    if (img_q=="") {
        write(" ## Exiting script.","")
        stop()
    } else if (is.na(as.numeric(img_q))) {
        write(" ## Please enter numbers only next time. Exiting...","")
        stop()
    } else if (img_q==1) {
        # Produce a jpeg image
        img_format<-"jpg"

    } else if (img_q==2) {
        # Produce a png image
        img_format<-"png"

    } else if (img_q==3) {
        # Produce a tiff image
        img_format<-"tif"

    } else if (img_q>3) {
        write(
            paste(" ## Please select either 1, 2 or 3",
                "next time. Exiting...",
            ),
            ""
        )
        stop()
    }
    # Return image format type
    return(img_format)
}