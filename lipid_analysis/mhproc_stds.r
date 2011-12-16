#    mhproc_2.r
#
#    Author:    Jairus Bowne
#    Purpose:    Preprocessing of MassHunter output files for lipid analysis
#
#    Input:      Files and objects produced by mhproc_1.r
#                    f) std_row.csv (created by mhproc_1.r)
#                    o) base_fn
#                    o) str_row
#                    o) std_conc                    
#
#    Output(s):     3) Standards
#                          Area and Rf values   (.csv)
#
#    Notes:    Part 2 of modularised mhproc.r script

#
#    Read in the standard row file
#
std_row<-read.csv("std_row.csv",sep=",",header=TRUE,row.names=1)
# Remove the temp file (we will save a more informative version later)
file.remove("std_row.csv")

#
#    Prepare and output Standards .csv file
#
if (std_point==TRUE) {
    # Create response factors
    Rf<-std_row/std_conc
    rownames(Rf)<-"Rf"
    out_std<-rbind(std_row,Rf)
    
    # Sort columns
    if (length(Rf)!=1) {
        out_std<-out_std[,sort(names(out_std))]
    } else {
        out_std<-out_std
    }
    
    # Write output file
    write.csv(out_std,
        paste(base_fn,
            "_3_Standards.csv",
            sep=""
        ),row.names=TRUE
    )
    
    # Inform user of created files
    write(
        paste("        ",base_fn,"_3_Standards.csv",sep=""),
        ""
    )
# Otherwise calculate standard curves
} else {
    rm(input_ok,s_concs,s_concs_spl,s_conc_num,r_sq) ### for testing else
    input_ok<-FALSE
    while (input_ok==FALSE) {
        # Print file names with line numbers
        write(
            paste(
                "    ",
                grep('std',std_row[,1],perl=TRUE,ignore.case=TRUE),
                ")  ",
                std_row[,1],
                sep=""
            ),
            ""
        )
        
        # Ask for concentrations from user in comma-separated list
        s_concs<-readline(
            paste(
                paste(
                    " >> Please enter the concentrations ",
                    "(nM) of the data files",
                    sep=""
                ),
                paste(
                    "    shown as a comma separated ",
                    "list (i.e. 5000,2500,1250,etc): ",
                    sep=""
                ),
                sep="\n"
            )
        )
        # Convert to a vector
        s_concs_spl<-strsplit(s_concs,",")[[1]]
        if (length(s_concs_spl)!=length(std_row[,1])) {
            write(" !! Incorrect number of concentrations entered.","")
        } else {
            # Convert to vector of numbers
            is_warn<-tryCatch(
                s_conc_num<-as.numeric(s_concs_spl),
                warning=function(w) {
                    TRUE
                }
            )
            # If the above statement does not throw a
            # warning, then it will have length > 1
            if (length(is_warn) == 1) {
                while (is_warn==TRUE) {
                    write(
                        " !! Possible error in input data; please check:",
                        ""
                    )
                    write(
                        paste(
                            "    ",
                            grep('.',s_concs_spl,perl=TRUE),
                            ")  ",
                            s_concs_spl,
                            sep=""
                        ),
                        ""
                    )
                    chg_val<-readline(
                        paste(
                            " >> Select the number of the value you wish to ",
                            "    change (type 'a' to re-enter all values): ",
                            sep="\n"
                        )
                    )
                    # Make a value in the case of none being entered
                    if (chg_val=="") chg_val<-"empty"
                    
                    # Test if it is a number
                    val_test<-tryCatch(
                        as.numeric(chg_val),
                        warning=function(w) {
                            FALSE
                        }
                    )
                    # If it's "a", enter all again
                    if (chg_val=="a") {
                        s_concs<-readline(
                            paste(
                                paste(
                                    " >> Please enter the concentrations ", 
                                    "(nM) of the data files",
                                    sep=""
                                ),
                                paste(
                                    "    shown as a comma separated ",
                                    "list (i.e. 5000,2500,1250,etc): ",
                                    sep=""
                                ),
                                sep="\n"
                            )
                        )
                        s_concs_spl<-strsplit(s_concs,",")[[1]]
                        is_warn=FALSE
                    # Otherwise check if the value is a row number           
                    } else if (val_test!=FALSE) {
                        if (as.numeric(chg_val) <= length(s_concs_spl)) {
                            new_val<-readline(
                                paste(" >> Please enter the new value for (",
                                    chg_val,
                                    "): ",
                                    sep=""
                                )
                            )
                            new_val_test<-tryCatch(
                                new_val<-as.numeric(new_val),
                                warning=function(w) { 
                                    FALSE
                                }
                            )
                            if (new_val_test==FALSE) {
                                write(
                                    paste(
                                        " ## New value not numeric,",
                                        " please check.",
                                        sep=""
                                    ),
                                    ""
                                )
                            } else {
                                new_val<-as.numeric(new_val)
                                s_concs_spl[as.numeric(chg_val)]<-new_val
                                s_conc_num<-as.numeric(s_concs_spl)#,
                                is_warn<-FALSE
                            }
                        # Otherwise inform of input error
                        } else {
                            write(
                                paste(
                                    " ## There is no '",
                                    chg_val,
                                    "'.",
                                    sep=""
                                ),
                                ""
                            )
                        }
                    } else {
                        write(
                            paste(
                                " ## Please only enter the number ",
                                "for the value or the letter 'a'.",
                                sep=""
                            ),
                            ""
                        )
                    }
                }
            } else {
                is_warn<-FALSE
            }

            s_conc_num<-as.numeric(s_concs_spl)
        
            # Check that it's a straight line (2% tolerance)
            r_sq<-summary(
                lm(s_conc_num ~ c(1:length(s_conc_num)))
            )$r.squared
            
            if (r_sq <= 0.98 || r_sq >= 1.02) {
                write(
                    paste(
                        " !! Standard curve model does not ",
                        "appear to produce a straight line.",
                        sep=""
                    ),
                    ""
                )
            } else {
                write(
                    paste(
                        " ** Standard curve model produces a ",
                        "straight line (R-sqaured = ",
                        sprintf("%.2f",r_sq),
                        ").",
                        sep=""
                    ),
                    ""
                )
                #if (length(s_conc_num)==length(std_row[,1])) {
                input_ok<-TRUE
                #}
            }
        }
        #print(s_conc_num)
    }
    
    # Add to std_row for output 
    std_row_lm<-cbind(
        std_row[,1],
        s_conc_num,
        std_row[,2:length(colnames(std_row))]
    )
    colnames(std_row_lm)[1:2]<-c("Standard","nM")
    
    # Create a data table to store values
    lm_vals<-as.data.frame(
        matrix(
            nrow=2,
            ncol=length(std_cls),
            dimnames=list(c("slope","intercept"),std_cls)
        )
    )
    # Calculate values
    for (cls in std_cls) {
        tmp<-lm(std_row_lm[[cls]] ~ std_row_lm[,2])
        lm_vals[[cls]][1]<-tmp[[1]][2]
        lm_vals[[cls]][2]<-tmp[[1]][1]
    }

    # Write output files
    write.csv(std_row_lm,
        paste(base_fn,
            "_3a_Standards.csv",
            sep=""
        ),row.names=FALSE
    )
    write.csv(lm_vals,
        paste(base_fn,
            "_3b_Standards_model.csv",
            sep=""
        ),row.names=TRUE
    )
    
    # Inform user of created files
    write(
        paste("        ",base_fn,"_3a_Standards.csv",sep=""),
        ""
    )
    write(
        paste("        ",base_fn,"_3b_Standards_model.csv",sep=""),
        ""
    )
}
