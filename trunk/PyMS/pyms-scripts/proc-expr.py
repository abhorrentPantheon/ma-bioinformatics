"""proc-expr.py

@author: Sean O'Callaghan, file handling by Andrew Isaac
@date: Nov '10

This script processes a set of samples, grouped according 
to directory structure, and saves a set of peak lists. After 
running this script you can view the peaks, TIC and IC's
for a sample using 'proc-display-all.py', or use 'proc-algn.py'
to do the alignment by dynamic programming for the samples.


"""

import sys, os, errno, string
sys.path.append("/x/PyMS")

###################################
# import PyMS classes and functions
###################################

from pyms.GCMS.IO.ANDI.Function import ANDI_reader
from pyms.GCMS.Function import build_intensity_matrix_i
from pyms.Noise.SavitzkyGolay import savitzky_golay
from pyms.Baseline.TopHat import tophat
from pyms.Peak.Class import Peak
from pyms.Peak.Function import peak_sum_area

from pyms.Deconvolution.BillerBiemann.Function import BillerBiemann, \
    rel_threshold, num_ions_threshold

from pyms.Experiment.Class import Experiment
from pyms.Experiment.IO import store_expr

##########
# Settings
##########

# define path to data files
# This is the folder where the files exist
base_path = "/home/projects/PyMS_Projects/Metabolomic.Data/2010.11.26-Jim-Malaria/Un_Sep"

# deconvolution and peak list filtering parameters
# 'pk_points' is the estimated number of points across signal peak
# 'r' allows for spectral skewing (adjacent peaks are combined if = 2
# 'n' is the minimum number of ions you expect to see in a peak
# 't' is the threshold above which an ion is counted towards 'n'
# 'r' is the percentage of the base ion peak below which any other
# ions present in the peak object have their intensities set to zero 
pk_points = 13
pk_scans = 2
n = 3
t = 1000
r = 1

###############
# File handling
###############

# File handling to automatically determine experimental design.
# Assumes:
#
# 1) within group data are in a single directory
# 2) all within group (sub)directories are in the one base
# directory
#
# The directory name for each within-group data is taken as
# the group name

# input files extension
file_ext = ".CDF"

# Set the path to the output folder
out_path="/home/projects/PyMS_Projects/Metabolomic.Processing/2010.11.26-Jim-Malaria/Un_Sep"

# copy directory structure to 'out_path' and get list of files
expr_files = []
spliton = os.path.basename(base_path)
for root, dirs, files in os.walk(base_path):

    # get group name from name of sub-directory
    try:
        [_, group] = string.split(root, spliton + os.sep)
        # output path including group subdirectory
        local = os.path.join(out_path, group)
    except ValueError:
        local = out_path
        group = None

    # make group directories
    for dir in dirs:
        try:
            os.mkdir(os.path.join(local, dir))
        # ignore existing & overwrite contents
        except OSError:
            if OSError.errno == errno.EEXIST:
                pass

    # collect and create experiment information
    cntr = 0
    for file in files:
        # experiments: name, in file, out file
        basename, ext = os.path.splitext(file)
        if ext.lower() != file_ext.lower():
            continue
        #cntr = cntr + 1
        # Create an experiment code: <group>_<filenumber>_<filename>
        # Assumes less than 100 within group files
        expr_code = basename
        expr_files.append([expr_code, os.path.join(root, file), \
            os.path.join(local, basename)])

###########################
# processing/peak detection
###########################

# loop over all experiments
for [expr_code, andi_file, outfile] in expr_files:

    print "\n -> Processing experiment '%s'" % ( expr_code )

    # read raw data
    data = ANDI_reader(andi_file)

    # write TIC's, helpful for exploratory data analysis
    tic = data.get_tic()
    tic.write(outfile + ".tic")

    print " Building intensity matrix ...",
    # build integer intensity matrix
    im = build_intensity_matrix_i(data)
    print " done."

    # crop mass range e.g. 50-540
    # This is important, differing vector lengths will result in 
    # problems with alignment
    im.crop_mass(50,540)

    # ignore TMS ions 73 and 147, and any other Ions you wish to 
    # exclude (e.g 207 for column bleed)
    im.null_mass(73)
    im.null_mass(147)
 

    # get the size of the intensity matrix
    n_scan, n_mz = im.get_size()
    print "Size of intensity matrix (nscans, n_mz):", n_scan, n_mz

    # loop over all IC: smoothing and baseline correction
    print " Smoothing and baseline correction ...",

    for ii in im.iter_ic_indices():
        ic = im.get_ic_at_index(ii)
        ic_smooth = savitzky_golay(ic)
        ic_base = tophat(ic_smooth, struct="1.5m")
        im.set_ic_at_index(ii, ic_base)

    print " done."

    # peak detection

    print " Applying deconvolution ...",

    # get the initial list of peak objects
    pl = BillerBiemann(im, pk_points, pk_scans)

    # trim by relative intensity
    apl = rel_threshold(pl, r)

    # trim by number of ions above threshold
    peak_list = num_ions_threshold(apl, n, t)

    print " done."

    print " [ Number of peaks found: %d ]" % ( len(peak_list) )

    # find peak areas
    for peak in peak_list:
        area = peak_sum_area(im, peak)
        peak.set_area(area)

    # store the results
    print " Saving data ...",

    # create an experiment object
    expr = Experiment(expr_code, peak_list)

    # select the time range to between 310 and 1258 seconds
    expr.sele_rt_range(["310s", "1258s"])

    store_expr(outfile + ".expr", expr)

    print " done."

