#    yep2mzxml.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Converts .yep files to mzXML files
#
#    Input:    .yep files
#    Output:    .mzxml format files
#
#    Notes:    This script can only run on a Windows platform. This is because
#              compassxport is required to run this script, which is available
#              from http://tiny.cc/izqyb [www.bdal.com - Bruker Daltonics]
#                  Home > Software > Compass Tools > CompassXport
#                  NB: Need to register to download product
#
#    Usage:
#        source("yep2mzxml.r")
#        # If all your files are in the default locations (see script):
#        yep2mzxml()
#        # Otherwise replace the "..." with the correct paths:
#        yep2mzxml(in_dir="...", temp_dir="...", out_dir="...",compassx="...")

#
#    Define function
#
yep2mzxml <- function(
    # Specify where the .yep files are
    in_dir=paste(getwd(), "/yep", sep=""),
    # Specify working directory
    temp_dir=paste(getwd(), "/temp", sep=""),
    # Specify output directory
    out_dir=paste(getwd(), "/mzXML", sep=""),
    # Specify path to compassx program
    compassx="\"C:/Program Files/Bruker Daltonik/CompassXport/CompassXport\""
    )
    # Begin function
    {
    write("Working...","")
    #
    #    Get file names
    #
    files <- dir(in_dir,full.name=TRUE)
    index <- grep("\\.yep$", files, ignore.case=TRUE)
    # Only keep the files that are .yep
    files <- files[index]
    basefn <- gsub(".+/(.+)\\.yep$","\\1", files, ignore.case=TRUE)

    #
    #    Convert files
    #
    # Count the number of files
    n <- length(basefn)
    # Create an empty vector to hold the system commands
    com_val <- numeric(n)
    # Using compassx (command line interface)
    for (file_num in 1:n)
        {
        file <- files[file_num]
        # print(file)
        # flush(stdout())
        file.copy(file, paste(temp_dir, "/analysis.yep", sep=""), overwrite=TRUE)
        sys_command <- paste(
            compassx, " -a ",
            "\"", temp_dir, "/analysis.yep\" -o ",            # temp file
            "\"", out_dir, "/", basefn[file_num], ".mzXML\"", # output file
            sep=""
            )
        # Run the command (line by line. Output is error code of the
        # command, and zero if the command is successful)
        com_val[file_num] <- system(sys_command, intern=FALSE, wait=TRUE,
            show.output.on.console=FALSE)
        }
    # Remove temporary files
    write("Removing temporary files...","")
    file.remove(paste(temp_dir, "/analysis.yep", sep=""))
    # Return a status message
    m = 0
    for (ii in 1:length(com_val)) {
        if (com_val[ii]!=0)
        {m = m+1}
        }
    if (m==0) 
        {write("There were no errors.","")} else
        {if (m==1)
            {write("There was one error.","")} else
            {if (m>1)
            {write(paste("There were ", m, " errors.", sep=""),"")}
            }
        }
    }

