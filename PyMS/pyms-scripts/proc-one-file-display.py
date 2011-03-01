"""proc-one-file-display.py

@author: Sean O'Callaghan

takes a single file and produces a display with the found peaks, TIC and ICs

Useful for setting parameters initally. 

"""

 
import sys
sys.path.append("/x/PyMS/")
 
from pyms.GCMS.IO.ANDI.Function import ANDI_reader
from pyms.GCMS.Function import build_intensity_matrix_i
from pyms.Noise.SavitzkyGolay import savitzky_golay
from pyms.Baseline.TopHat import tophat
from pyms.Peak.Class import Peak

from pyms.Display.Class import Display
 
from pyms.Deconvolution.BillerBiemann.Function import BillerBiemann, \
    rel_threshold, num_ions_threshold
    

 
 # read in raw data
andi_file = "/home/projects/PyMS_Projects/Metabolomic.Data/2010.01.28_DPI_dairy_waste_water/In/In_061108_Spring_1.CDF"
data = ANDI_reader(andi_file)

data.trim(6m, 21m)

# Build Intensity Matrix
im = build_intensity_matrix_i(data)


n_scan, n_mz = im.get_size()


 # perform necessary pre filtering
for ii in range(n_mz):
    ic = im.get_ic_at_index(ii)
    ic_smooth = savitzky_golay(ic)
    ic_bc = tophat(ic_smooth, struct="1.5m")
    im.set_ic_at_index(ii, ic_bc)
    
    
 # Detect Peaks
peak_list = BillerBiemann(im, points=3, scans=2)



print "Number of peaks found: ", len(peak_list) 


######### Filter peaks###############
# Filter the peak list,
# first by removing all intensities in a peak less than a given relative
# threshold,
# then by removing all peaks that have less than a given number of ions above
# a given value

# Parameters
# percentage ratio of ion intensity to max ion intensity
r = 2

# minimum number of ions, n
n = 3
# greater than or equal to threshold, t
t = 1000

# trim by relative intensity
pl = rel_threshold(peak_list, r)

# trim by threshold
new_peak_list = num_ions_threshold(pl, n, t)

print "Number of filtered peaks: ", len(new_peak_list)

# TIC from raw data
tic = data.get_tic()
# save TIC to a file

# Get Ion Chromatograms for all m/z channels
n_mz = len(im.get_mass_list())
ic = []

# All plotting from here on
for m in range(n_mz):
    ic.append(im.get_ic_at_index(m))


# Create a new display object, this time plot the ICs 
# and the TIC, as well as the peak list
display = Display()

display.plot_tic(tic, 'TIC')
display.plot_ics(ic)
display.plot_peaks(new_peak_list, 'PyMS peaks')
display.do_plotting()
