"""proc.py

@author: Sean O'Callaghan on 12-01-11

Compares two ion traces from 2 different experiments

"""

import sys
sys.path.append("/x/PyMS")

from pyms.GCMS.IO.ANDI.Function import ANDI_reader
from pyms.GCMS.Function import build_intensity_matrix

from pyms.Display.Function import Display

# read the raw data as a GCMS_data object
#
# replace the text "The file with data .....
# with the name and location of your file
#  e.g. /home/anahid/sample1.cdf

andi_file_1 = "The file with data from sample 1"
andi_file_2 = "The file with data from sample 2"


data_1 = ANDI_reader(andi_file_1)
data_2 = ANDI_reader(andi_file_2)


# IntensityMatrix
# must build intensity matrix before accessing any intensity matrix methods.

# default, float masses with interval (bin interval) of one from min mass
print "default intensity matrix, bin interval = 1, boundary +/- 0.5"
im_1 = build_intensity_matrix(data_1)
im_2 = build_intensity_matrix(data_2)


#
# quant_ion_mass is the ion you wish to compare between samples
#

quant_ion_mass = 104   #for example

# get the ion chromatogram for some m/z channel 
ic_1 = im_1.get_ic_at_mass(quant_ion_mass)
ic_2 = im_2.get_ic_at_mass(quant_ion_mass)

ics = [ic_1, ic_2]



display = Display()
display.plot_ics(ics,['Sample 1', 'Sample 2'])
display.do_plotting('Comparing trace of mz =' + str(quant_ion_mass) + 'between \
2 samples')

