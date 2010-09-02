# two_sample.py

import sys
from data_matrix import *

#############
# define data
#############

# peak lablels
peak_label = ["m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9" ]

# true peak intensities for two samples 
# Note: itensities identical, differ only in peak "m7"
A_int = [ 0.0784, 0.6337, 0.2762, 0.3667, 0.0896, 0.3378, 0.0813, 0.9963, 0.5071 ]
B_int = [ 0.0784, 0.6337, 0.2762, 0.3667, 0.0896, 0.3378, 0.2813, 0.9963, 0.5071 ]

# number of replicates (this will apply to both sample)
N = 4

#
# create the list of true underlying experiments
#

expr_list = []
expr_list.append( experiment("A", A_int, peak_label) )
expr_list.append( experiment("B", B_int, peak_label) )

#
# generate experimental replicates (true underlying experiments + noise)
#

replicates_list = []

for expr in expr_list:
    replicates = generate_replicates(expr, N, sigma=0.2, prop=True)
    replicates_list.extend(replicates)

#
# save data matrix
#

sys.stdout.write(",Group,")

expr = replicates_list[0]
n = len(expr.peak_label)

for ii in range(n-1):
    sys.stdout.write("%s," % (expr.peak_label[ii]))
sys.stdout.write("%s\n" % ( expr.peak_label[n-1] ))

cntr = 0
for expr in replicates_list:
    cntr = cntr + 1
    sys.stdout.write("S%d,%s," % (cntr,expr.label))
    for ii in range(n-1):
        sys.stdout.write("%.4f," % (expr.peak_intensity[ii]))
    sys.stdout.write("%.4f\n" % ( expr.peak_intensity[n-1] ))

