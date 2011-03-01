"""proc-algn.py

@author: Sean O'Callaghan, Andrew Isaac

Performs the alignment of experiment objects from the folder
location set as base_path below.

Writes two .csv files to the same folder.
"""

import sys, os, string
sys.path.append("/x/PyMS/")

from pyms.Experiment.IO import load_expr
from pyms.Peak.List.DPA.Class import PairwiseAlignment
from pyms.Peak.List.DPA.Function import align_with_tree, exprl2alignment
from pyms.Peak.IO import store_peaks

############
# Settings #
############

# within replicates alignment parameters
Dw = 2.5  # rt modulation [s]
Gw = 0.30 # gap penalty

# between replicates alignment parameters
Db = 10.0 # rt modulation [s]
Gb = 0.30 # gap penalty

############################
# input file tree handling # 
############################

# define path to experiment files
base_path="/home/projects/PyMS_Projects/Metabolomic.Processing/2010.11.26-Jim-Malaria/Un_Sep/output"

# copy directory structure to 'out_path' and get list of files
group_list = []
spliton = os.path.basename(base_path)
for root, dirs, files in os.walk(base_path):
    try:
        [_, group] = string.split(root, spliton + os.sep)
    except ValueError:
        group = None

    file_list = []
    for file in files:
        # experiments: group, name, file
        if file.endswith(".expr"):
            file_list.append(os.path.join(root, file))

    if len(file_list) > 0:
        group_list.append((group, file_list))

##################
# peak alignment #
##################

# within-state alignments
alignments = []
for group, expr_files in group_list:
    print " Aligning expt:", group
    expr_list = []
    for file_name in expr_files:
        expr = load_expr(file_name)
        expr_list.append(expr)

    # require common peaks in at least half the number of files
    n = (len(expr_list)+1)/2

    F = exprl2alignment(expr_list)
    T = PairwiseAlignment(F, Dw, Gw)
    A = align_with_tree(T, min_peaks=n)

    alignments.append(A)

# between-state alignment
print "\n Aligning between-state"

Tb = PairwiseAlignment(alignments, Db, Gb)
Ab = align_with_tree(Tb)

Ab.write_csv(os.path.join(base_path, 'rt.csv'), os.path.join(base_path, 'area.csv'))

