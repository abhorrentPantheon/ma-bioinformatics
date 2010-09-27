# xcms_preprocess.r
#
#    Authors:    Amsha Nahid, Jairus Bowne, Gerard Murray
#    Purpose:    Process LC-QTOF data files into a raw data matrix
#
#    Input:   .mzXML (open format) files
#    Output:    Unfiltered .csv data matrix

#
#    Load necessary libraries, and install them if they are missing
#
# xcms
tryCatch(library(xcms), error=function(err)
    {source("http://bioconductor.org/biocLite.R")
    biocLite("xcms")})
# multtest
tryCatch(library(multtest), error=function(err)
    install.packages("multtest",repos="http://cran.ms.unimelb.edu.au/"))

#
#    Prepare files
#
# Get path information for files
path<-getwd()
# Specify the output directory
output_dir<-"mzxml"
# Get files (full path) for processing
xfiles<-list.files(output_dir, recursive=TRUE)
for (ii in 1:length(xfiles)) {
    xfiles[ii]<-paste(path, output_dir, xfiles[ii], sep="/")
    }

#
#    Verify peak widths
#
# Load an example raw file
x<-xcmsRaw(xfiles[10])
# Show full scan
x11()
plot(x@scantime,x@tic,type="l",col="red")
# Pull out a single peak (values from previous plot)
ind<-which(x@scantime>500 & x@scantime<600)
# Plot only this section of the chromatogram (with extra tick marks)
x11()
plot(x@scantime[ind],x@tic[ind],type="l",col="red",lab=c(20,5,7))

#
#    Retention time correction
#
write("Retention time correction (3 passes)","")
write("Working on pass 1...","")
# Load files and pick peaks
xset<-xcmsSet(xfiles, fwhm=12,mzdiff=0.02,step=0.02)
# Match peaks across samples
xset<-group(xset, bw=10)
# Correct for retention time drift
xset1<-retcor(xset,family="symmetric", plottype="mdevden")
# Regroup
xset1<-group(xset1, bw=10)
# Fill in missing peak data
xset2<-fillPeaks(xset1)

# Repeat
xset2<-group(xset2, bw=10)
write("Working on pass 2...","")
xset3<-retcor(xset2,family="symmetric", plottype="mdevden")
xset3<-group(xset3, bw=10)
xset4<-fillPeaks(xset3)

# Repeat (this is sufficient for our test data matrix)
xset4<-group(xset4, bw=10)
write("Working on pass 3...","")
xset5<-retcor(xset4,family="symmetric", plottype="mdevden")
xset5<-group(xset5, bw=10)
xset6<-fillPeaks(xset5)

#
#    Report creation
#
# Generate report, with links to Metlin
write("Generating report...","")
report<-diffreport(xset5,metlin=0.15)
# Write .csv output file
write.csv(report, "data_xcms.csv")
write("Process complete. Report saved as data_xcms.csv","")