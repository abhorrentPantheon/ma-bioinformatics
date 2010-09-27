# d2mzXML.r
#
#    Authors:    Moshe Olshansky, Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Convert Agilent .d folders into open format .mzXML files
#
#    Input:    .d folders (Agilent output)
#    Output:   .mzXML (open format) files
#    Notes:    This script can only run on a Windows platform. This is because
#              trapper software is required to run this script, which
#              is available from http://tiny.cc/yqn3s [sourceforge.net]
#              An alternative is msConvert from ProteoWizard
#              (http://tiny.cc/z40om [sourceforge.net]) which may enable
#              use on linux hosts (untested)
#
#    Usage:
#        source("d2mzXML.r")
#        d2mzXML(input_dir, output_dir)
#
#        # Note: input and output directories must have a trailing "/"
#        # e.g. d2mzXML("Raw data files/", "mzxml/")

d2mzXML <- function(
    # Specify input directory
    input_dir,
    # Specify output directory
    output_dir)
    # Begin function
    {
    # Specify the .d folders to use
    dotd_folders <- list.files(input_dir)
    # Create an index
    index <- grep("\\.d$",dotd_folders)
    # Apply the index to the list of .d folders (create vector)
    dotd_folders <- dotd_folders[index]
    # Use full path names for the input .d folders
    input_files <- paste(input_dir, dotd_folders, sep="")
    # Substitute the .d in the input with .mzXML for the output
    output_files <- gsub("\\.d$","\\.mzXML",dotd_folders)
    # Create full path names for the output folders
    output_files <- paste(output_dir, output_files, sep="")
    # Loop through the files and 
    write("Processing files now. Please be patient.", "")
    for (ii in 1:length(input_files)) {
        cmd_string <- paste(
            "\"C:/Program\ Files/trapper/trapper\" --mzXML -c \"",
            input_files[ii],"\" \"",output_files[ii],"\"",sep="")
        write(cmd_string,"")
        flush(stdout())
        system(cmd_string)
        }
    }