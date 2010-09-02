# data_matrix.py

import random, copy

#
# some useful classes
#

class experiment(object):

    def __init__(self, label, peak_intensity, peak_label):

        self.label = label
        self.peak_intensity = peak_intensity
        self.peak_label = peak_label
        self.n_peaks = len(peak_intensity)

def add_noise(expr, sigma=1.0, prop=True):

    for ii in range(len(expr.peak_intensity)):

        intensity = expr.peak_intensity[ii]

        noise = random.random() - 0.5
        if prop:
            noise = noise*intensity

        intensity_noise = intensity + sigma*noise
        expr.peak_intensity[ii] = intensity_noise

    return expr

def create_replicate(expr): 

    expr = copy.deepcopy(expr)

    return expr

def generate_replicates(expr, N, sigma=1.0, prop=True):
    
    replicates_list = []
    for ii in range(N):
        expr_replicate = create_replicate(expr)
        expr_replicate_noise = add_noise(expr_replicate, sigma, prop)
        replicates_list.append(expr_replicate_noise)

    return replicates_list

