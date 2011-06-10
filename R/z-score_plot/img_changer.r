#    img_changer.r
#
#    Author:    Jairus Bowne, Amsha Nahid, Gerard Murray, Alysha De Livera
#    Purpose:    Change x-axis values
#                Support script for z-score.r

#
#    Function for altering the x-axes
#
img_changer<-function(input_matrix=z_matrix) {
    # Ask for user input
    write(" ?? Is this plot as you wish to save the image?","")
    input<-readline(
        paste("    1) yes           [Provides options to save image]",
            "    2) no            [Provides options to change x-axis]",
            "    3) cancel        [Exit the script]",
            " >> Please select a number:  ",
            sep="\n"
        )
    )
    # Start a counter to record how many times it has run
    counter<-0
    # Turn off warnings if desired (don't forget to turn back on)
    #options(show.error.messages=FALSE,warn=-1)
    # Check that the response is a number
    if (is.na(as.numeric(input))) {
        write(" ## Please enter numbers only next time. Exiting...","")
        stop()
    }
    if (input==3) {
        write(" ## Exiting script.","")
        stop()
    #
    #    If user wishes to change image:
    #
    } else if (input==2) {
            min_x<-readline(
                paste(" >> New value for minimum x-value",
                    "   (press enter to keep current value): ",
                    sep="\n"
                )
            )
            max_x<-readline(
                paste(" >> New value for maximum x-value",
                    "   (press enter to keep current value): ",
                    sep="\n"
                )
            )
            # Check min_x    
            if (min_x=="") {
                # retain previous value of min_x
                min_x<-range(input_matrix)[1]
            } else {
                if (is.na(as.numeric(min_x))) {
                    write(
                        paste(" ## Please enter numbers",
                            "only next time. Exiting..."
                        ),
                        ""
                    )
                    stop()
                } else {
                    min_x<-as.numeric(min_x)
                }
            }
            # Check max_x    
            if (max_x=="") {
                # retain previous value of max_x
                max_x<-range(input_matrix)[2]
            } else {
                if (is.na(as.numeric(max_x))) {
                    write(
                        paste(
                            " ## Please enter numbers",
                            "only next time. Exiting...",
                            sep=""
                        ),
                        ""
                    )
                    stop()
                } else {
                    max_x<-as.numeric(max_x)
                }
            }
        counter<-counter+1
        dev.off()
        z_plot(x_range=c(min_x,max_x))
    # If image okay:
    } else if (input==1) {
        if (counter!=0) {
            dev.off()
            z_plot(x_range=c(min_x,max_x))
        } else {
            min_x<-range(input_matrix)[1]
            max_x<-range(input_matrix)[2]
            z_plot(x_range=c(min_x,max_x))
        }
        # graphic options
        img_format<-img_type()
    } else {
        write(" ## Please select either 1, 2 or 3 next time. Exiting...","")
        break
    }
    # Reinstate warnings if turned off earlier
    #options(show.error.messages=TRUE,warn=0)
    if (input==1) {
        ic_result<-list(input,min_x,max_x,img_format)
    } else {
        ic_result<-list(input,min_x,max_x)
    }
    return(ic_result)

}