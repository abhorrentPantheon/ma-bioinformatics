#    kruskal_wallis.r
#
#    Author:    Alysha M De Livera, Jairus Bowne, Amsha Nahid
#    Purpose:    Performs Kruskal-Wallis test on a data matrix
#                
#
#    Input:    Data matrix as specified in Data-matrix-format.pdf
#    Output:    1) kw_pvals.csv - p-values for all metabolites
#               2) kwMultComp_{metabolite_name}.csv - List of sample
#                  pairs and whether or not the difference is 
#                  significant for that metabolite.
#
#    Notes:    Non-parametric one-way analysis of variance by ranks.
#    Date:    31/05/2011

# # Determine which variables/objects are present before running script
# rm_list<-list()
# rm_list$pre=ls()

# Load required libraries
tryCatch(
    library(pgirmess), 
    error=function(err) {
        # if this produces an error:
        install.packages("pgirmess",repos="http://cran.ms.unimelb.edu.au/")
        library(pgirmess)
    }
)

#
#    Function Definitions
#
# Perform Kruskal-Wallis tests for all metabolites, output p-values
kwtests<-function(input_matrix,level=0.9,...) {
    # Extract metabolite names
    met_names<-colnames(input_matrix)[3:length(colnames(input_matrix))]
    mat<-matrix(NA,nc=1,nr=length(met_names))
    for (i in 1:length(met_names)) {
        # Perform Kruskal-Wallis tests and extract p values
        mat[i,]<-kwtest(input_matrix=input_matrix,met_names[i])$p.value
    }
    colnames(mat)<-"Kruskal-Wallis P value"
    rownames(mat)<-met_names
    # Create a .csv file with metabolite names and Kruskal-Wallis p-values
    write.csv(mat,"kw_pvals.csv") 
    print(mat)
}


# Perform Kruskal-Wallis test on specific metabolite
kwtest<-function(input_matrix,metabname,...)
{   # Extract information of the data matrix
    info<-data_summary(input_matrix) 
    # Extract data corresponding to the specified metabolite
    metabolite<-info$data_mat[,which(metabname==info$metnames)]   
    # Create an empty list for Kruskal-Wallis test results
    groupslist<-list()  
    for (i in 1:info$ngroup) {
         groupslist[[i]]<-subset(
            input_matrix[,(which(metabname==info$metnames)+2)],
            input_matrix$Group==levels(info$group)[i]
        )
    }
    for (i in 1:info$nmet) {
        #Perform the Kruskal-Wallis test
        kw_result<-kruskal.test(groupslist)  
    }
    return(kw_result)
}


#This function extracts all the information about the data matrix
data_summary<-function(input_matrix,...) {
    # Classify Sample as a factor
    input_matrix[,1]<-factor(input_matrix[,1])
    # Classify Group as a factor
    input_matrix[,2]<-factor(input_matrix[,2])
    # Metabolites matrix
    data_mat<-input_matrix[,(3:ncol(input_matrix))]
    # Get number of metabolites
    nmet<-length(data_mat)
    # Metabolite names
    metnames<- names(input_matrix)[3:ncol(input_matrix)]
    # Sample names
    sample_names<-input_matrix$Sample
    # Number of samples
    nsample<-nlevels(sample_names)
    # Groups
    group<-input_matrix$Group
    # Number of groups 
    ngroup<-nlevels(group)
    # Output data summary
    return(
        list(
            data=input_matrix,
            group=group,
            ngroup=ngroup,
            data_mat=data_mat,
            nmet=nmet,
            metnames=metnames,
            sample_names=sample_names,
            nsample=nsample
        )
    )
}

get_metabolite_name<-function(input_matrix) {
    metabolite_list<-names(input_matrix)[3:ncol(input_matrix)]
    write("Metabolite names:","")
    print(metabolite_list)
    # Ask which metabolite to use for comparisons
    metab_sel<-readline(
        write(
            paste(" >> Please enter the name of the metabolite",
                "you would like to compare (enter to cancel): "
            )
            ,""
        )
    )
    if (metab_sel=="") {
        rm_list$post=ls()
        # Remove objects in rm_list$post that aren't in rm_list$pre
        rm(list=rm_list$post[which(rm_list$pre!=rm_list$post)])
        rm(rm_list)
        write(" ## Metabolite selection cancelled. Exiting script.","")
        stop("Cancelled by user")
    }
    while (
        length(
            grep(
                metab_sel,
                metabolite_list,
                ignore.case=TRUE,
                perl=TRUE
            )
        ) == 0
    ) {
        write("Metabolite not found in list! Please try again.","")
        metab_sel<-readline(" >> Please enter metabolite name: ")
    }
    return(metab_sel)
}

# Read in the .csv file
in_file<-file.choose()
input_data<-read.csv(in_file, sep=",", header=TRUE)
colnames(input_data)[1]<-"Sample"
# Perform Kruskal-Wallis test and write p-values to file
kwtests(input_data) 

# Select a metabolite to perform the specific test
metab_sel<-get_metabolite_name(input_data)
# Find the column the selected metabolite is in
metabolite_list<-names(input_data)[3:ncol(input_data)]
metab_col<-grep(metab_sel,metabolite_list,ignore.case=TRUE,perl=TRUE)

# Identify which groups are different for a specified metabolite
kruskal_result<-kruskalmc(input_data[[metab_col]], input_data[,2])
# Output which comparisons are significant only
out_kruskal<-kruskal_result$dif.com[3]
colnames(out_kruskal)<-"Significant difference"
# Write output file
fname<-paste("kwMultComp_",metab_sel,".csv",sep="")
write.csv(out_kruskal,fname)

# #
# #    Tidy up
# #
# # List all objects
# rm_list$post=ls()
# # Remove objects in rm_list$post that aren't in rm_list$pre
# rm(list=rm_list$post[which(rm_list$pre!=rm_list$post)])
# rm(rm_list)