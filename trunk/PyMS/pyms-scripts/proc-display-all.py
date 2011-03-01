"""proc.py

@author: Sean O'Callaghan

This script reads two files, the original data, and the experiment file
which contains the peaklist. This must have been previously generated, 
for example by running proc-expr.py.

A new window is generated showing the Ion Chromatograms and the peaks
found by PyMS. Right clicking on a peak shows the mass spectrum.

"""

import sys
sys.path.append("/x/PyMS/")

from pyms.GCMS.IO.ANDI.Function import ANDI_reader
from pyms.GCMS.Function import build_intensity_matrix
from pyms.Peak.IO import load_peaks
from pyms.Display.Class import Display
from pyms.Experiment.IO import load_expr
from pyms.Utils.IO import load_object
from pyms.Noise.SavitzkyGolay import savitzky_golay
from pyms.Baseline.TopHat import tophat

andi_file = "andi_file_name.cdf"

expr_file="output/expr_filename.expr"

# read the raw data as a GCMS_data object
data = ANDI_reader(andi_file)
#data.trim(2431, 2469)

# IntensityMatrix
# default, float masses with interval (bin interval) of one from min mass
print "default intensity matrix, bin interval = 1, boundary +/- 0.5"
im = build_intensity_matrix(data)
im.null_mass(73)
im.null_mass(147)

n_scan, n_mz = im.get_size()

for ii in range(n_mz):
    ic = im.get_ic_at_index(ii)
    ic_smooth = savitzky_golay(ic)
    ic_base = tophat(ic_smooth, struct="1.5m")
    im.set_ic_at_index(ii, ic_base)

# Load the experiment
exper = load_expr(expr_file)

# Load the peak list 
peak_list = exper.get_peak_list()

# Pass Ion Chromatograms into a list of ICs
n_mz = len(im.get_mass_list())
ic = []

for m in range(n_mz):
    ic.append(im.get_ic_at_index(m))


# Create a new display object, this time plot four ICs 
# and the TIC, as well as the peak list
display = Display()


display.plot_ics(ic)
display.plot_peaks(peak_list, 'Peaks')
display.do_plotting('ICs, and PyMS Detected Peaks')
