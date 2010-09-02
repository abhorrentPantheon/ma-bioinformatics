#    dims.r
#
#    Author:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Convert Direct Infusion Mass Spectrometry (DIMS) data
#                into a data matrix
#
#    Input:    .mzxml format files (see yep2mzxml.r for conversion from .yep)
#    Output:    Data matrix (.csv format) as per Data-matrix-format.pdf
#
#    Usage:
#        source("dims.r")
#        # If all your files are in the suggested locations:
#        dims()
#        # Otherwise replace the "..." with the correct paths/values:
#        dims(inDir="...", outDir="...", lowMZ="...", highMZ="...", ...)
#        # for example:
#        dims(inDir="xml_files", outDir="xml_files/processed", thresh=50)

#
#    Define function
#
dims <- function(
    # inDir as current directory if not specified
    inDir=paste(getwd(), "/mzXML", sep=""),
    # Put files into processed_data directory unless otherwise specified
    outDir=paste(getwd(), "/processed_data", sep=""),
    # Set low and high m/z values
    lowMZ=100,
    highMZ=2200,
    # Set the bin width size
    deltaMZ=1,
    # Set the threshold (anything below this value will be set to zero)
    thresh=0
)
#
#    Begin function
#
{
# Load necessary library; install if not present.
if (require(xcms) == FALSE) {
    source("http://bioconductor.org/biocLite.R")
    biocLite("xcms")
    }
# Create a list of .mzxml files in the directory
files <- dir(inDir, full.name=TRUE)
ind <- grep("\\.mzXML$", files, ignore.case=TRUE)
files <- files[ind]
fnames <- gsub(".+/(.+)\\.mzXML$", "\\1", files, ignore.case=TRUE)

n <- length(fnames)
breaks <- seq(from=lowMZ, to=highMZ, by=deltaMZ)
# Create empty placeholders for later use
bins <- rep(0,length(breaks)-1)
posRes <- matrix(0, nrow=n, ncol=length(breaks)-1)
negRes <- matrix(0, nrow=n, ncol=length(breaks)-1)

#
#    Process mzXML files
#
for (fileNum in 1:n) {
    file <- files[fileNum]
    # Create xcms object
    x <- xcmsRaw(file)
    # Generate a matrix
    y <- rawMat(x)
    # Get time values for positive and negative scans
    posTimes <- x@scantime[x@polarity == "positive"]
    negTimes <- x@scantime[x@polarity == "negative"]
    # Generate an index with which to select values for each mode
    posInd <- which(y[,"time"] %in% posTimes)
    negInd <- which(y[,"time"] %in% negTimes)
    # Separate each mode into its own matrix
    posY <- y[posInd,]
    negY <- y[negInd,]
    # Get index for binning intensity values
    ## This doesn't round the value for mz - is this an issue?
    yp <- cut(posY[,"mz"], breaks, include.lowest=TRUE,
        right=TRUE, labels=FALSE)
    yn <- cut(negY[,"mz"], breaks, include.lowest=TRUE,
        right=TRUE, labels=FALSE)
    # Empty the bins
    posBins<-bins
    negBins<-bins
    # Get the list of intensity values for each bin, and add the
    # intensity values which are in the same bin
    if (nrow(posY) > 0) {
        ap <- aggregate(posY[,"intensity"],list(yp),sum)
        posBins[ap[,1]] <- posBins[ap[,1]] + ap[,2] / length(posTimes)
        }
    if (nrow(negY) > 0) {
        an <- aggregate(negY[,"intensity"],list(yn),sum)
        negBins[an[,1]] <- negBins[an[,1]] + an[,2] / length(negTimes)
        }
    # Zero any values that are below the threshold
    posBins[posBins < thresh] <- 0
    negBins[negBins < thresh] <- 0
    # Attach file
    posRes[fileNum,] <- posBins
    negRes[fileNum,] <- negBins
    }
# Ensure the results are a data frame
posRes <- as.data.frame(posRes)
negRes <- as.data.frame(negRes)
# Add in file names as row names
rownames(posRes) <- fnames
rownames(negRes) <- fnames
# Add 0.5 to the values in breaks, and delete the last value
a <- breaks[-length(breaks)] + 0.5*deltaMZ
# Format as string and show precision of float to 2 digits
b <- sprintf("%.2f",a)
# Use this as the column names
colnames(posRes) <- b
colnames(negRes) <- b
# Write the output files
# NB: Files are written as transpose since the number of columns is
#     usually much larger than the maximum allowed by Excel/OpenOffice
write.csv(t(posRes), file=paste(outDir, "/pos.csv", sep=""))
write.csv(t(negRes), file=paste(outDir, "/neg.csv", sep=""))
# Inform user as to location of output files
write(" -> Files written:", "")
write(paste("    ", outDir, "/pos.csv", sep=""), "")
write(paste("    ", outDir, "/neg.csv", sep=""), "")
}