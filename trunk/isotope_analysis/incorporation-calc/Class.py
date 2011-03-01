"""
Class to model metabolite 

Metabolite is described by a single quantifying ion
and same quant ion with carbon isotopes

author: Sean O'Callaghan
date: 21-2-11
email: spoc@unimelb.edu.au

"""
import sys

class Metabolite(object):
    """
    @summary: Contains name, background isotope count and
    other information about the metabolite

    @author: Sean O'Callaghan
    """

    def __init__(self, name, non_labelled_list, labelled_list):
        """
        @summary: Initialise the Metabolite data

        @param name: The name of the metabolite
        @type name: stringType

        @param non_labelled_list: A list of lists of Ion counts for the
                                  non labelled data
        @type non_labelled_list: listType

        @param labelled_list: A list of lists of Ion counts for the labelled data
        @type labelled_list: listType
        
        """
        
        self.name = name
        self.__non_labelled_list = non_labelled_list
        self.__labelled_list = labelled_list

        # calculate the natural isotope level and subtract
        # from labelled
        self.calc_background_counts()
        # self.subtract_background()

    
    def __str__(self):
        """
        print information about the metabolite
        """
        lines =  "name: " + self.name + "\n"
        lines +=  "Background Counts: \n"
        lines +=  str(self.__average_non_labelled) + "\n"
        lines += "Main counts: \n"
        lines += str(self.__labelled_list) + "\n"

        return lines
    
    def calc_background_counts(self):
        """
        @summary: Calculate the average counts for the non-labelled data

        """
        # temporary container for the totals
        total_list = []
        # append len(non label list) zeros to the total
        for ion in self.__non_labelled_list[0]:
            total_list.append(0)

        for ion_list in self.__non_labelled_list:
            for i, ion in enumerate(ion_list):
                try:
                    total_list[i] += ion
                except:
                    print "Differing number of background mz values \
                           for the same metabolite - exit program"
                    sys.exit()

        #print "non averaged: ", total_list
        # store the averages
        self.__average_non_labelled = [float(ion_count)/ \
                                           len(self.__non_labelled_list) \
                                         for ion_count in total_list]

        #print "averaged: ", self.__average_non_labelled
        
        
    def subtract_background(self):
        """
        @summary: subtracts the natural isotope level

        not being used at the moment
        """
        self.__bg_subtracted_list = self.__labelled_list
        
        for ion_list in self.__bg_subtracted_list:
            for ion, bg_ion  in zip(ion_list, self.__average_non_labelled):
                ion = ion - bg_ion

        
    def calc_percent_incorp_by_ion(self):
        """
        @summary: calculates the percentage incorporation as the
        ratio of (count - background) to count
        """
        percentages = []

        for ion_list, sub_ion_list in zip(self.__labelled_list, \
                                              self.__bg_subtracted_list):
            for ion, sub_ion in zip(ion_list, sub_ion_list):
                percentage = 100*(ion - sub_ion)/ion
                percentages.append(percentage)

    def daves_calc_total_incorp(self):
        """
        @summary: Dave de Souza method of simple calculation to find
        percentage incorporation

        Find ratio of naturally occuring isotope by summing all counts
        for values inc & above mz+1 & divide by mz=0 value for the background
        values.
        Use this to calculate the natural background count for the
        sample, then subtract this from the >=mz+1 values for the sample
        and divide this by the total count for all mzs to get % incorp

        @return: a list of the percentage incorporation for the metabolite
        """
        
        self.__percent_incorp = []

        sum_mz_greater_0 = sum(self.__average_non_labelled[1:])
        mz_equals_0 = self.__average_non_labelled[0]

        natural_isotope_level = sum_mz_greater_0/mz_equals_0



        for ion_list in self.__labelled_list:
            #print "length of list", len(self.__labelled_list)
            mz_equals_0 = ion_list[0]
            sum_mz_greater_0 = sum(ion_list[1:])

            bg_subtracted = sum_mz_greater_0 - mz_equals_0*natural_isotope_level

            if sum(ion_list) != 0:
                self.__percent_incorp.append(bg_subtracted/sum(ion_list))
            else:
                self.__percent_incorp.append(0.0)

        return self.__percent_incorp



if __name__ == "__main__":
    """
    self test for Metabolite class

    Hand Calculation shows that answer should be
    0.25818, 0.3723

    """

    name = "Metabolite A"

    # list of background
    bg1 = [12, 1, 2, 1]
    bg2 = [13, 1, 3, 1]

    bg = [bg1, bg2]

    # list of main
    m1 = [12, 3, 4, 3]
    m2 = [12, 4, 6, 4]

    m = [m1, m2]
    
    my_metab = Metabolite(name, bg, m)
    incorp = my_metab.daves_calc_total_incorp()

    print "% incorp ", incorp
    
