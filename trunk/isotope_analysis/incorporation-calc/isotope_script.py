"""
script to process isotope incorporation data for Dave De Souza

requires Class.py

author: Sean O'Callaghan
date: 21-2-11
email: spoc@unimelb.edu.au
"""

from Class import Metabolite
import re, sys


def read_metabolites(infile):
    """
    @summary: Reads information from a csv file into Metabolite classes

    @param infile: a comma separated value file (.csv)
    @type infile: stringType

    @return: a list of Metabolites
    @rtype: listType of Class.Metabolite
    """
    

    fp = open(infile, 'r')


    lines = fp.readlines()
    names = []
    bg_list = []
    main_list = []
    metabolites = []

    # Compile some regexs for finding relevant parts of input file
    metab_name_pattern = re.compile('^[\'a-z\'][^bg]', re.IGNORECASE)
    bg_info_pattern = re.compile('bg', re.IGNORECASE)
    main_info_pattern = re.compile('^[0-9]')

    # First find the names and store
    for line in lines:
        words = line.split(',')
        test_word =  words[0].split('m/z')[0].strip('"')
        if re.search(metab_name_pattern, test_word):
            names.append(test_word)
    # print names
    # Go over each line, storing the information from the file into
    # Class.Metabolite objects.
    for line in lines:
        for i, name in enumerate(names):
            if name in line:
                # print "name: ", name, "found"
                # makes sure that this is not the first time round
                # before establishing a new metabolite
                if bg_list:
                    #print "bg_list for", names[i-1], "is populated"
                    new_metabolite = Metabolite(names[i-1], bg_list, main_list)
                    metabolites.append(new_metabolite)

                    bg_list = []
                    main_list = []
        
        if re.search(bg_info_pattern, line):
            #print "bg found"
            nums = [float(num) for num in line.split(",")[:-1]]
            bg_list.append(nums)
        
        elif re.search(main_info_pattern, line):
            nums = [float(num) for num in line.split(",")[:-1]]
            main_list.append(nums)
            
    return metabolites


def write_outfile(metabolites, outfile):
    """
    @summary: Writes a csv file with information on percent incorporation

    @param metabolites: list of metabolites
    @type metabolites: listType of Class.Metabolite

    @param outfile: the name of the outfile
    @type outfile: StringType

    """

    try:
        fp = open(outfile, 'w')
    except:
        print "Problem opening %s for writing" % outfile
        sys.exit()
    

    lines = []
    total_incorp = []
    first_line = ""
    new_line = ""

    for metabolite in metabolites:
        first_line += metabolite.name + ","
        total_incorp.append(metabolite.daves_calc_total_incorp())

    # print "total incorp: ", total_incorp

    # remove trailing comma
    first_line = first_line[:-1]
    lines.append(first_line)
    #for testing
    #print first_line

    for percentage in total_incorp[0]:
        new_line = str(percentage) + ","
        lines.append(new_line)
        new_line = ""

    for incorp in total_incorp[1:]:
        for i, percentage in enumerate(incorp):
            lines[i+1] += str(percentage) + ","

    # Uncomment if you want to see output on the console
    #for i, line in enumerate(lines):
    #    print i, ":   ", line

    for line in lines:
        fp.write(line[:-1] + "\n")
 
    
    

###############################################################################
#      
#    The bit that does the work
#
###############################################################################

metabolites = read_metabolites('infile.csv')
print "Read metabolites"

outfile = 'outfile.csv'
write_outfile(metabolites, outfile)
print "Wrote outfile: ", outfile

