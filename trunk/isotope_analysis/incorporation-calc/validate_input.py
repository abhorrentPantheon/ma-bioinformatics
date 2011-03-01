import sys, re


def validate_input(infile):
    """
    @summary: validates the format of the input file
    """

    fp = open(infile, 'r')

    lines = fp.readlines()

    # Compile some regexs for finding relevant parts of input file
    metab_name_pattern = re.compile('^[\'a-z\'][^bg]', re.IGNORECASE)
    bg_info_pattern = re.compile('bg', re.IGNORECASE)
    main_info_pattern = re.compile('^[0-9]')

    names = []

    for i, line in enumerate(lines):
        words = line.split(",")
        test_word =  words[0].split('m/z')[0].strip('"')
        if re.search(metab_name_pattern, test_word):
            names.append(test_word)

        elif re.search(bg_info_pattern, line):
            if not re.search(bg_info_pattern, words[-1]):
                print "text 'bg' found, but not at the end of the line"
                print "on line %d, perhaps a trailing comma?"%(i+1)
                sys.exit()

        elif re.search(main_info_pattern, line):
            if line[-2] is not ",":
                print "last character on line %d should be a comma (,)"%(i+1)
                sys.exit()

    if not 'end' in lines[-1].lower():
        print "The last line shoule contain the word 'end'"
    
    print "Numbers formatted correctly"
    print "The following metabolite names were found: "

    for name in names[:-1]:
        print name


################################################################################
#
#
#    The business end of the script
#
#
################################################################################
input_file = 'infile.csv'
validate_input(input_file)
