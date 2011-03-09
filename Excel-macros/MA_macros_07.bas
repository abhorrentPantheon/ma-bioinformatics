Attribute VB_Name = "Metabolomics_macros"

Sub calc2end()

'    calc2end    [VBA]
'    Author:    Jairus Bowne
'    Purpose:    Create all remaining sheets after CalcVals
'
'    Input:    Checked CalcVals sheet
'    Output:    1)    Overview sheet showing significance by t-Test
'               2)    Overview sheet showing fold change by colour
'               3)    Folds sheet

ans = MsgBox("Do you have duplicate metabolites?", vbYesNoCancel, _
    "Clear duplicates?")
If ans = vbYes Then
    Application.Run "PERSONAL.XLSB!emboldT_pretty"
    Application.Run "PERSONAL.XLSB!calc2oview"
    ActiveSheet.Copy After:=Sheets("Overview")
    Sheets(Sheets.count).Select
    Application.Run "PERSONAL.XLSB!dupRem"
    Application.Run "PERSONAL.XLSB!fold_colour_ov"
    Application.Run "PERSONAL.XLSB!oview2folds"
ElseIf ans = vbNo Then
    Application.Run "PERSONAL.XLSB!emboldT_pretty_2"
    Application.Run "PERSONAL.XLSB!calc2oview"
    ActiveSheet.Copy After:=Sheets("Overview")
    Sheets(Sheets.count).Select
    Application.Run "PERSONAL.XLSB!fold_colour_ov"
    Application.Run "PERSONAL.XLSB!oview2folds"
ElseIf ans = vbCancel Then
    Exit Sub
End If

End Sub

Sub calc2oview()

'    calc2oview    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    Generate an Overview sheet from a CalcVals sheet
'
'    Input:    CalcVals sheet (see template)
'    Output:    Overview sheet
'

' Define variables
Dim show_box As Boolean
Dim Response As Variant
Dim counter1 As Integer
Dim counter2 As Integer
Dim counter3 As Integer
Dim counter4 As Integer
Dim counter5 As Integer
Dim counter6 As Integer

' Change the cursor to the hourglass, then don't update the screen
Application.Cursor = xlWait
Application.ScreenUpdating = False

Range("a1").Select
init = ActiveCell.Address
' Select all the data, use info later
Range("a1").Select
On Error Resume Next
    end_row = Cells.Find("*", [A1], , , xlByRows, xlPrevious).Row
    end_col = Cells.Find("*", [A1], , , xlByColumns, xlPrevious).Column
    end_cell = Cells(end_row, end_col).Address
dataset = "a1:" & end_cell
Range(dataset).Select

' Copy it all and paste links to 'Overview' sheet. If it doesn't exist,
' make a new one ('ErrHandler' section at end of this macro)
Selection.Copy
    On Error GoTo ErrHandler:
    Sheets("Overview").Activate
        Sheets("Overview").Select
    Range("A1").Select
    ActiveSheet.Paste
    ActiveSheet.Paste Link:=True

Range(init).Select

' Unmerge all the merged sample group names
Rows("2:2").Select
Selection.UnMerge
' Shift the sample group names so that they won't get deleted
Range(init).Select
ActiveCell.Offset(1, 0).Select
Selection.Insert Shift:=xlToRight
Selection.Insert Shift:=xlToRight
' Remove everything but the x-fold and % std err columns
Range(init).Select
ActiveCell.Offset(0, 1).Select
For counter1 = 1 To end_col + 1
    If ActiveCell.Value = "x-fold" Then
        ActiveCell.Offset(0, 1).Select
    ElseIf ActiveCell.Value = "sem" Then
        ActiveCell.Offset(0, 1).Select
    Else
        Selection.EntireColumn.Delete
    End If
Next counter1

' Re-merge the sample headers
For counter2 = 1 To (end_col + 1) / 6
    Range(init).Select
    ActiveCell.Offset(1, ((counter2 * 2) - 1)).Select
    Range(ActiveCell, ActiveCell.Offset(0, 1)).Select
        With Selection
            .HorizontalAlignment = xlCenter
            .VerticalAlignment = xlBottom
            .WrapText = False
            .Orientation = 0
            .AddIndent = False
            .IndentLevel = 0
            .ShrinkToFit = False
            .ReadingOrder = xlContext
            .MergeCells = False
        End With
    Selection.Merge
Next counter2

' Autofit the metabolite names
Range(init).Select
Selection.ClearContents
Columns("A:A").EntireColumn.AutoFit

' Add in a column for the ± symbol
For counter3 = 1 To (end_col + 1) / 6
Range(init).Select
ActiveCell.Offset(0, (counter3 * 3) - 1).Select
Selection.EntireColumn.Insert
Next counter3

' Add in the actual symbol, and fill down
For counter4 = 1 To (end_col + 1) / 6
    Range(init).Select
    ' Go to each location
    ActiveCell.Offset(2, (counter4 * 3) - 1).Select
    ' Add symbol
    ActiveCell.Value = "±"
    ' Fill down to end of previous column (this is one of the
    ' most elegant solutions I have seen in macro code!)
    Range(ActiveCell, _
        ActiveCell.Offset(0, -1).End(xlDown).Offset(0, 1)).FillDown
    ActiveCell.Offset(0, -1).Select
    ' Copy the colours from the previous column
    Range(ActiveCell, ActiveCell.End(xlDown)).Copy
    ActiveCell.Offset(0, 1).Select
    Range(ActiveCell, ActiveCell.End(xlDown)).Select
    Selection.PasteSpecial Paste:=xlPasteFormats, _
        operation:=xlNone, skipblanks:=False, Transpose:=False
    ' Autofit the column
    Selection.EntireColumn.AutoFit
' Rinse and repeat
Next counter4

' Make std error columns aligned to the left
For counter5 = 1 To (end_col + 1) / 6
    Range(init).Select
    ActiveCell.Offset(2, (counter5 * 3)).Select
    Range(ActiveCell, ActiveCell.End(xlDown)).Select
    With Selection
        .HorizontalAlignment = xlLeft
        .VerticalAlignment = xlBottom
        .WrapText = False
        .Orientation = 0
        .AddIndent = False
        .IndentLevel = 0
        .ShrinkToFit = False
        .ReadingOrder = xlContext
    End With
Next counter5

' Make the x-folds aligned right
For counter6 = 1 To Response
    Range(init).Select
    ActiveCell.Offset(2, (counter6 * 3) + 1).Select
    Range(ActiveCell, ActiveCell.End(xlDown)).Select
    With Selection
        .HorizontalAlignment = xlRight
        .VerticalAlignment = xlBottom
        .WrapText = False
        .Orientation = 0
        .AddIndent = False
        .IndentLevel = 0
        .ShrinkToFit = False
        .ReadingOrder = xlContext
    End With
Next counter6

On Error Resume Next
    end_row = Cells.Find("*", [A1], , , xlByRows, xlPrevious).Row
    end_col = Cells.Find("*", [A1], , , xlByColumns, xlPrevious).Column
    end_cell = Cells(end_row, end_col).Address
Range(end_cell).Select
ActiveCell.Offset(0, 1).Select
Selection.EntireColumn.Delete

Range(init).Select

'   Turn the screen updating back on and give us back a normal cursor
Application.ScreenUpdating = True
Application.Cursor = xlDefault

'Error Handler for making overview sheet
ErrHandler:
    ' If the sheet doesn't exist, create it:
    If Err.Number = 9 Then
        Worksheets.Add(After:=ActiveSheet).Name = "Overview"
        ' go back to the line of code that caused the problem
        Resume
    End If
    
End Sub

Sub cpd_sort()

'    cpd_sort    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    Sort compounds (in Overview sheet) into classes,
'                and then alphabetically
'
'    Input:    Any sheet
'    Output:    Compounds are sorted alphabetically with breaks
'               between compound classes

' Declarations
Dim cpd_dict As Scripting.Dictionary
Dim counter1 As Integer
Dim counter2 As Integer
Dim num_cpds As Integer

' Prevent screen flickering
Application.Cursor = xlWait
Application.ScreenUpdating = False

' Create the dictionary
Set cpd_dict = CreateObject("Scripting.dictionary")

'
'    Compound classes
'
' Define what compounds belong to which class
' List generated by taking all compounds from TMS and TBS retention
' time templates, copying that list, removing _nTMS or _nTBS from the
' compound names (tms_trim macro), joining that to the full list and
' removing duplicates from that merged list (250 compounds)
' To update this list, add new compounds to the required class

'amino acids
aa = Array("Alanine", "Alanine_3TMS", "Allothreonine", "Arg_Orn", _
    "Arg_Orn_3TMS", "Arg_Orn_4TMS", "Arginine", "Arginine_3TBS", _
    "Asparagine", "Asparagine_3TMS", "Asparagine_4TMS", "Aspartate", _
    "Aspartate_2TMS", "beta-Alanine", "beta-Alanine_2TBS", "Cysteine", _
    "GABA", "Glutamate", "Glutamine", "Glutamine_3TMS", "Glutamine_4TBS", _
    "Glutamine_4TMS", "Glycine", "Glycine_3TMS", "Histidine", "Homoserine", _
    "Homoserine_2TMS", "Isoleucine", "Leucine", "Lysine", "Lysine_4TMS", _
    "Methionine", "Norleucine", "Ornithine", "Phenylalanine", "Proline", _
    "Proline_2TMS", "Pyroglutamate", "Serine", "Serine_2TMS", "Serine_3TMS", _
    "Threonine", "Threonine_2TMS", "Threonine_3TMS", "Tryptophan", _
    "Tyrosine", "Valine")

'organic acids
oa = Array("2-Aminoadipate", "2-Aminoadipate_3TMS", "2-Butenoate", _
    "2-Butenoate_3TBS", "2-Oxobutanoate", "2-Oxobutyrate", "2-Oxogluconate", _
    "2-Oxoglutarate", "2-Oxovaleric_acid", "3-Methylbenzoate", _
    "4-Coumarate", "4-Hydroxybenzoate", "4-Hydroxybenzoate_2TBS", _
    "4-Hydroxycinnamate", "6-Hydroxynicotinate", "6-Hydroxynicotinate_2TBS", _
    "Acetohydroxamate", "Aconitate", "Aminoisobutyrate", "Ascorbate", _
    "Benzoate", "Caffeate", "Chlorogenate", "Citramalate", "Citrate", _
    "Decanedioate", "Dehydroascobate_dimer", "Docosanoate", "Dodecanoate", _
    "Eicosanoate", "Erythronate", "Ferulate", "Ferulate_2TBS", "Fumarate", _
    "Glycerate", "Glycerate-3-P", "Glycolate", "Heptadecanoate", _
    "Hexadecanoate", "Hydroxyindolecarbonate", _
    "Hydroxyindolecarbonate_3TBS", "Indole-3-acetate", _
    "Indole-3-acetate_2TMS", "Isocitrate", "Itaconate", "Itaconate_2TBS", _
    "Linoleic_acid", "Malate", "Maleate", "Malonate", _
    "Monomethylphosphate", "Muconate", "Nonanoate", "Octadecanoate", _
    "Oleic_acid", "Oxalate", "Oxaloacetate", "Oxaloacetate_TBS", _
    "Pentadecanoate", "Pentanoate", "Pentanoate_2TBS", _
    "Pentonate-1,4-lactone", "Phosphate", "Phytanate", "Pipecolate", _
    "Pipecolate_2TBS", "Pipecolate_2TMS", "Pyrrole-2-carboxylate", _
    "Quinate", "Quinate_4TBS", "Salicylate", "Shikimate", "Sinapinate", _
    "Succinate", "Sulfate", "Sulfate_2TBS", "Tartrate", "Tetracosanoate", _
    "Tetradecandioate", "Tetradecanoate", "Threonate", "Threonate_3TBS", _
    "Threonate_4TBS", "Threonate-1,4-lactone", "trans-Aconitate", _
    "trans-Aconitate_4TBS", "Tricosanoate", "Urate")

'sugars
sug = Array("1,6-anhydroglucose", "1-Kestose", "3-O-beta-D-gal-D-ara", _
    "6-Kestose", "Arabinose", "Arabitol", "Digalactosylglycerol", _
    "Erythritol", "Fructose", "Fructose_MX1", "Fructose_MX2", _
    "Fructose-6-P", "Fucose", "Fucose_MX2", "Galactinol", _
    "Galactinol_put", "Galactitol", "Galactonate", "Galactose", _
    "Galactose_MX1", "Galactose_MX2", "Galactosylglycerol", _
    "Galacturonate", "Galacturonate_MX1", "Galacturonate_MX2", _
    "Gentibiose", "Gentibiose_MX1", "Glucarate", "Glucarate_lactone", _
    "Gluconate", "Gluconate-6-P", "Glucose", "Glucose_MX1", "Glucose_MX2", _
    "Glucose-6-P", "Glucose-6-P_MX1", "Glucose-6-P_MX2", "Glucuronate", _
    "Glucuronate_MX1", "Glucuronate_MX2", "Glycerol", "Glycerol_2TBS", _
    "Glycerol-2-P", "Glycerol-3-P", "Glycerol-3-P_prod1", _
    "Glycerol-3-P_prod2", "Inositol", "Inositol-1-P", "Lactose", _
    "Lactose_MX1", "Laminaribiose", "Laminaribiose_MX1", _
    "Laminaribiose_MX2", "Maltose", "Maltose_MX2", "Mannitol", _
    "Mannose", "Mannose-6-P", "Mannose-6-P_MX1", "Melezitose", _
    "Melibiose", "Melibiose_MX1", "Melibiose_MX2", "Quebrachitol", _
    "Raffinose", "Rhamnose", "Rhamnose_MX1", "Ribonate", "Ribose", _
    "Sedoheptulose", "Sedoheptulose_(put)", "Sorbitol", "Sucrose", _
    "Threitol", "Trehalose", "Turanose", "Turanose_MX1", "Turanose_MX2", _
    "Xylitol", "Xylose", "Xylose_MX1", "Xylose_MX2")


'other compounds
oth = Array("1-Monooctodecanoglycerol", "5-Hydroxytryptamine", "Adenine", _
    "Adenine_2TBS", "Adenosine", "Adenosine_4TMS", "Allantoin", _
    "Allantoin 4TMS", "beta-Sitosterol", "Campesterol", "Cytosine", _
    "Diethylene_glycol", "Docosanol", "Ethanolamine", "Ethanolamine_2TBS", _
    "Ethanolamine_3TMS", "Fucosterol", "Glycerophosphoglycerol", _
    "Guanine", "Hexacosanol", "Hydroquinone-beta-glucopyranoside_1", _
    "N-Acetyl_glucosamine", "N-Acetyl_glucosamine_MX1", _
    "N-Acetyl-glutamate", "N-Acetyl-serine", "Octadecanol", "Putrescine", _
    "Putrescine_4TMS", "Stigmasterol", "Tocopherol", "Tyramine", _
    "Tyramine_3TMS", "Uracil", "Urea", "Urea_3TBS")

' Create a 2D array
cpd_array = Array(aa, oa, sug, oth)
key_labels = Array("Amino acids", "Organic acids", "Sugars", "Other compounds")
Range("a3").Select

'Loop through arrays to create dictionary
For counter1 = 0 To UBound(cpd_array)
    For counter2 = 0 To UBound(cpd_array(counter1))
        cpd_dict.Add StrConv(cpd_array(counter1)(counter2), vbLowerCase), key_labels(counter1)
    Next counter2
Next counter1

' Add in the compound class to sort by
Range(Selection, Selection.End(xlDown)).Select
num_cpds = Selection.Rows.Count
Selection.EntireColumn.Insert
Range("b3").Select
For counter1 = 1 To num_cpds
        cpd_class = cpd_dict.Item(StrConv(ActiveCell.Value, vbLowerCase))
        ' If there isn't a value for cpd_class, substitute "Other
        ' compound" (which is the last entry in key_labels)
        If cpd_class = "" Then
            cpd_class = key_labels(UBound(key_labels))
        End If
        ActiveCell.Offset(0, -1).Value = cpd_class
    ActiveCell.Offset(1, 0).Select
Next counter1

'Select the whole dataset
Range("a3").Select
On Error Resume Next
    end_row = Cells.Find("*", [A1], , , xlByRows, xlPrevious).Row
    end_col = Cells.Find("*", [A1], , , xlByColumns, xlPrevious).Column
    end_cell = Cells(end_row, end_col).Address
dataset = "a3:" & end_cell

' Sort by class, then name of compound
Range(dataset).Select
ActiveSheet.Sort.SortFields.Clear
Application.AddCustomList ListArray:=key_labels
' Count how many lists there are (+1 later as zero indexed)
custlistnum = Application.CustomListCount
Selection.Sort Key1:=Range("A3"), Order1:=xlAscending, _
    Key2:=Range("B3"), Order2:=xlAscending, _
    Header:=xlNo, OrderCustom:=custlistnum + 1, _
    MatchCase:=False, Orientation:=xlTopToBottom, _
    DataOption1:=xlSortNormal, DataOption2:=xlSortNormal
' Don't pollute user's lists
Application.DeleteCustomList custlistnum

' Loop through classes
For Each newclass In key_labels
    Range("a3").Select
    'Range(Selection, Selection.End(xlDown)).Select
    Selection.EntireColumn.Select
    Set foundcell = Selection.Find(What:=newclass, after:=ActiveCell, _
        LookIn:=xlFormulas, LookAt:=xlWhole, SearchOrder:=xlByRows, _
        SearchDirection:=xlNext, MatchCase:=False, Searchformat:=False)
    ' If the class of compound isn't present, this line prevents failure
    If Not (foundcell Is Nothing) Then
        ' Break into sections with a single line space between
        foundcell.Select
        ' Insert two rows with no formatting
        Selection.EntireRow.Insert
        Selection.EntireRow.Select
        Selection.ClearFormats
        Selection.EntireRow.Insert
        Selection.EntireRow.Select
        Selection.ClearFormats
        ' Add in the compound class label
        ActiveCell.Offset(1, 1).Select
        ActiveCell.Value = newclass
        Selection.Font.Bold = True
    End If
Next newclass

' Remove working column
Range("a1").Select
Selection.EntireColumn.Delete
' Remove blank row at the top
Range("a3").Select
Selection.EntireRow.Delete

' Return cursor to default and allow screen updating
Application.ScreenUpdating = True
Application.Cursor = xlDefault

End Sub

Sub cursorfix()
'    cursorfix    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    Reinstating the default cursor when a macro doesn't do it
'                    i.e. When a macro breaks, cursor stays as an hourglass

Application.ScreenUpdating = True
Application.Cursor = xlDefault

End Sub

Sub dupRem()

'    dupRem    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    Remove duplicate metabolites (all metabolites ending in "_2")
'
'    Notes:    This will remove anything that ends in _2
'              Check carefully before running!

' Prevent screen flicker
Application.Cursor = xlWait
Application.ScreenUpdating = False

' Find the second internal standard, or anything that ends in '_2' and delete it
Range("A3").Select
init = ActiveCell.Address
Do Until ActiveCell.Value = ""
    If ActiveCell.Value = "InSt2" Then
    ActiveCell.EntireRow.Delete
        ElseIf Right(ActiveCell.Value, 2) = "_2" Then
        ActiveCell.EntireRow.Delete
    Else
    ActiveCell.Offset(1, 0).Select
    End If
Loop

' Return cursor to default and allow screen updating
Application.ScreenUpdating = True
Application.Cursor = xlDefault

End Sub

Sub emboldT_pretty()

'    emboldT_pretty    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    This macro will make x-folds, standard errors of mean and
'                t-Test values in a CalcVals sheet (see template) bold
'                and coloured.
'
'                For t-Test values < 0.05, x-fold and standard error cells
'                are filled with cyan.
'                For t-Test values < (0.05 / number of metabolites), the
'                cells are green.
'    Notes:    This is a Bonferroni-type correction for p-value. See:
'              Benjamini, Y & Hochberg, Y (1995) Controlling the False
'              Discovery Rate: A Practical and Powerful Approach to Multiple
'              Testing. Journal of the Royal Statistical Society.
'              Series B (Methodological). 57(1):289-300.
'
'    Input:    CalcVals sheet (see template) where there are 
'              duplicate metabolites
'                  i.e. Inositol and Inositol_2
'

' Define variables
Dim rwNm As Integer

' Prevent screen flicker
Application.Cursor = xlWait
Application.ScreenUpdating = False

' Define a starting point
Range("k3").Select
init = ActiveCell.Address

' Count the number of rows to use later
ActiveCell.EntireRow.Select
Selection.End(xlToLeft).Select
ActiveCell.Select
Range(Selection, Selection.End(xlDown)).Select
rwNm = Selection.Rows.count

' Go to the starting point
Range(init).Select

' Repeat until you run out of columns
Do Until ActiveCell.Value = ""
    ' Repeat this until the cell is blank (i.e. the end of the column)
    Do Until ActiveCell.Value = ""
        ' #DIV/0! errors cause an issue; use an error handler:
        On Error GoTo panic
        ' If the value is less than p-value divided by the 
        ' number of metabolites, then:
        If ActiveCell.Value < (0.05 / (rwNm / 2)) Then
            ' Select that cell and the two to the left of it
            Range(ActiveCell, ActiveCell.Offset(0, -2)).Select
            ' Make the text bold
            Selection.Font.Bold = True
            Selection.Font.Italic = True
            ' Fill the cells with green
            With Selection.Interior
                    .ColorIndex = 4
                    .Pattern = xlSolid
            End With
            ' Move to the next t-test value in the column
            ActiveCell.Offset(1, 2).Select
            ' Here's the tricky part: If this was not the case,
            ' then we want to re-investigate, thusly:
        Else
            ' If it's not p<0.05/metabs#, then see if it's < 0.05
            If ActiveCell.Value < 0.05 Then
                ' If it is, select the two cells to the left of it too
                Range(ActiveCell, ActiveCell.Offset(0, -2)).Select
                ' Make 'em bold
                Selection.Font.Bold = True
                ' Colour them cyan
                With Selection.Interior
                        .ColorIndex = 8
                        .Pattern = xlSolid
                End With
                ' Then select the next t-test value in the column
                ActiveCell.Offset(1, 2).Select
                ' If this is not the case, then un-bold them all and un-fill
            Else
                Range(ActiveCell, ActiveCell.Offset(0, -2)).Select
                Selection.Font.Bold = False
                With Selection.Interior
                        .ColorIndex = xlNone
                    End With
                ' and move to the next one
                ActiveCell.Offset(1, 2).Select
            End If
        End If
    ' Keep doing it until getting to a blank cell
    Loop
    'now move to the next column of t-test values
    ActiveCell.Offset(-1, 0).Select
    Selection.End(xlUp).Select
    ActiveCell.Offset(0, 6).Select
' Keep doing this until the column is blank (i.e. the end of the dataset)
Loop
' When finished, go back to the start point
Range(init).Select

' Return cursor to default and allow screen updating
Application.ScreenUpdating = True
Application.Cursor = xlDefault

' Error handler:
panic:
' If it's a #DIV/0! error, then
If Err.Number = 13 Then
    ' Don't highlight the cell in any way, and undo any that has been done
    ActiveCell.Interior.ColorIndex = xlNone
    ActiveCell.Font.Bold = False
    ActiveCell.Font.Italic = False
    ActiveCell.Offset(1, 0).Select
    Resume
End If

End Sub

Sub emboldT_pretty_2()

'    emboldT_pretty    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    This macro will make x-folds, standard errors of mean and
'                t-Test values in a CalcVals sheet (see template) bold
'                and coloured.
'
'                For t-Test values < 0.05, x-fold and standard error cells
'                are filled with cyan.
'                For t-Test values < (0.05 / number of metabolites), the
'                cells are green.
'    Notes:    This is a Bonferroni-type correction for p-value. See:
'              Benjamini, Y & Hochberg, Y (1995) Controlling the False
'              Discovery Rate: A Practical and Powerful Approach to Multiple
'              Testing. Journal of the Royal Statistical Society.
'              Series B (Methodological). 57(1):289-300.
'
'    Input:    CalcVals sheet (see template) where there are
'              no duplicate metabolites

' Define variables
Dim rwNm As Integer

' Prevent screen flicker
Application.Cursor = xlWait
Application.ScreenUpdating = False

' Define a starting point
Range("k3").Select
init = ActiveCell.Address

' Count the number of rows to use later
ActiveCell.EntireRow.Select
Selection.End(xlToLeft).Select
ActiveCell.Select
Range(Selection, Selection.End(xlDown)).Select
rwNm = Selection.Rows.count

' Loop over the whole sheet as per emboldT_pretty (see that macro for
' information on what each part does, it is thoroughly commented)
Range(init).Select
Do Until ActiveCell.Value = ""
    Do Until ActiveCell.Value = ""
        On Error GoTo panic
        ' Here is where it is different - it's rwNm, not rwNm/2
        If ActiveCell.Value < (0.05 / rwNm) Then
            Range(ActiveCell, ActiveCell.Offset(0, -2)).Select
            Selection.Font.Bold = True
            Selection.Font.Italic = True
            With Selection.Interior
                        .ColorIndex = 4
                        .Pattern = xlSolid
                End With
            ActiveCell.Offset(1, 2).Select
        Else
            If ActiveCell.Value < 0.05 Then
                Range(ActiveCell, ActiveCell.Offset(0, -2)).Select
                Selection.Font.Bold = True
                With Selection.Interior
                        .ColorIndex = 8
                        .Pattern = xlSolid
                    End With
                ActiveCell.Offset(1, 2).Select
            Else
                Range(ActiveCell, ActiveCell.Offset(0, -2)).Select
                Selection.Font.Bold = False
                With Selection.Interior
                        .ColorIndex = xlNone
                    End With
                ActiveCell.Offset(1, 2).Select
            End If
        End If
    Loop
    ActiveCell.Offset(-1, 0).Select
    Selection.End(xlUp).Select
    ActiveCell.Offset(0, 6).Select
Loop
Range(init).Select

' Return cursor to default and allow screen updating
Application.ScreenUpdating = True
Application.Cursor = xlDefault

'Error handler:
panic:
' If it's a #DIV/0! error, then
If Err.Number = 13 Then
    ' Don't highlight the cell in any way, and undo any that has been done
    ActiveCell.Interior.ColorIndex = xlNone
    ActiveCell.Font.Bold = False
    ActiveCell.Font.Italic = False
    ActiveCell.Offset(1, 0).Select
    Resume
End If

End Sub

Sub find_dataset()

'    find_dataset    [VBA]
'
'    Author:    Jairus Bowne
'    Acknowledgements:    This code was basically copied wholesale from
'                         http://www.angelfire.com/biz7/julian_s/julian/julians_macros.htm
'                         (a very good site for basic VBA scripting examples)
'    Purpose:    Finds the last cell that has any information in it, and makes the range
'                from A1 to there the selected area. Invaluable for very large data sets
'

Range("a1").Select
On Error Resume Next
    end_row = Cells.Find("*", [A1], , , xlByRows, xlPrevious).Row
    end_col = Cells.Find("*", [A1], , , xlByColumns, xlPrevious).Column
    end_cell = Cells(end_row, end_col).Address
dataset = "a1:" & end_cell
Range(dataset).Select

End Sub

Sub fold_colour_fl()

'    fold_colour_fl    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    Change the colour of the cell fill based on the value of the cell.
'
'    Input:    Folds sheet (i.e.
'              Responses ---[resp2calcvals]---> CalcVals
'              CalcVals ---[calc2oview]---> Overview
'              Overview ---[oview2folds]---> Folds )
'              (see template)

' Prevent screen flickering
Application.Cursor = xlWait
Application.ScreenUpdating = False

' Define variables
Dim nmrw As Integer
Dim nmcl As Integer
Dim counter1 As Integer
Dim counter2 As Integer
Dim counter3 As Integer
Dim counter4 As Integer

' Collect information about sheet size
Range("a3").Select
Range(Selection, Selection.End(xlDown)).Select
nmrw = Selection.Rows.count
Range("a3").Select
Range(Selection, Selection.End(xlToRight)).Select
nmcl = Selection.Columns.count

' Define a starting point
Range("a1").Select
init = ActiveCell.Address

ActiveCell.Offset(2, 1).Select
Range(init).Select
For counter1 = 1 To nmcl
    Range(init).Select
    ActiveCell.Offset(2, counter1).Select
    For counter2 = 1 To nmrw
        On Error GoTo problem_skip
        ' Light blue for low negative
        If ActiveCell.Value < -2 And ActiveCell.Value > -4 Then
            With Selection.Interior
                .ColorIndex = 34
                .Pattern = xlSolid
            End With
        ' or mid blue for mid neg
        ElseIf ActiveCell.Value < -4 And ActiveCell.Value > -6 Then
            With Selection.Interior
                .ColorIndex = 33
                .Pattern = xlSolid
            End With
        ' or dark blue for severely negative.
        ElseIf ActiveCell.Value < -6 Then
            With Selection.Interior
                .ColorIndex = 41
                .Pattern = xlSolid
            End With
        End If
        ActiveCell.Offset(1, 0).Select
    Next counter2
    Range(init).Select
    ActiveCell.Offset(0, counter1).Select
Next counter1

Range(init).Select
    For counter3 = 1 To nmcl
        Range(init).Select
        ActiveCell.Offset(2, counter3).Select
        For counter4 = 1 To nmrw
            On Error GoTo problem_skip
            ' similarly for pos, yellow for weak
            If ActiveCell.Value > 2 And ActiveCell.Value < 4 Then
                With Selection.Interior
                    .ColorIndex = 6
                    .Pattern = xlSolid
                End With
            ' orange for medium
            ElseIf ActiveCell.Value > 4 And ActiveCell.Value < 6 Then
                With Selection.Interior
                    .ColorIndex = 44
                    .Pattern = xlSolid
                End With
            ' red for strong
            ElseIf ActiveCell.Value > 6 Then
                With Selection.Interior
                    .ColorIndex = 3
                    .Pattern = xlSolid
                End With
            End If
            ActiveCell.Offset(1, 0).Select
        Next counter4
        Range(init).Select
        ActiveCell.Offset(0, counter1).Select
    Next counter3
    
Range(init).Select

' Return cursor to default and allow screen updating
Application.ScreenUpdating = True
Application.Cursor = xlDefault

problem_skip:
ActiveCell.Offset(1, 0).Select

End Sub

Sub fold_colour_ov()

'    fold_colour_fl    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    Change the colour of the cell fill based on the
'                value of the cell.
'
'    Input:    Overview sheet (i.e.
'              Responses ---[resp2calcvals]---> CalcVals
'              CalcVals ---[calc2oview]---> Overview; see template)

' Define variables
Dim nmrw As Integer
Dim nmcl As Integer
Dim counter1 As Integer
Dim counter2 As Integer
Dim counter3 As Integer
Dim counter4 As Integer

' Prevent screen flickering
Application.Cursor = xlWait
Application.ScreenUpdating = False

' Collect information about sheet size
Range("a3").Select
Range(Selection, Selection.End(xlDown)).Select
Range(Selection, Selection.End(xlToRight)).Select
nmrw = Selection.Rows.count
nmcl = Selection.Columns.count
' Remove any colouring that is already present
Selection.Interior.ColorIndex = xlNone

' Define a starting point
Range("a1").Select
init = ActiveCell.Address

' Convert fold changes that are less than 1 to negative fold changes
Application.Run "PERSONAL.XLSB!neg_folds_ov"
Application.Cursor = xlWait
Application.ScreenUpdating = False

ActiveCell.Offset(2, 1).Select
Range(init).Select
    For counter1 = 1 To ((nmcl - 1) / 3)
        Range(init).Select
        ActiveCell.Offset(2, (counter1 * 3) - 2).Select
        For counter2 = 1 To nmrw
            If ActiveCell.Value < -2 And ActiveCell.Value > -4 Then
                With Selection.Interior
                    .ColorIndex = 34
                    .Pattern = xlSolid
                End With
            ElseIf ActiveCell.Value < -4 And ActiveCell.Value > -6 Then
                With Selection.Interior
                    .ColorIndex = 33
                    .Pattern = xlSolid
                End With
            ElseIf ActiveCell.Value < -6 Then
                With Selection.Interior
                    .ColorIndex = 41
                    .Pattern = xlSolid
                End With
            End If
            ActiveCell.Offset(1, 0).Select
        Next counter2
        Range(init).Select
        ActiveCell.Offset(0, (counter1 * 3) - 2).Select
    Next counter1

Range(init).Select
    For counter3 = 1 To ((nmcl - 1) / 3)
        Range(init).Select
        ActiveCell.Offset(2, (counter3 * 3) - 2).Select
        For counter4 = 1 To nmrw
            If ActiveCell.Value > 2 And ActiveCell.Value < 4 Then
                With Selection.Interior
                    .ColorIndex = 6
                    .Pattern = xlSolid
                End With
            ElseIf ActiveCell.Value > 4 And ActiveCell.Value < 6 Then
                With Selection.Interior
                    .ColorIndex = 44
                    .Pattern = xlSolid
                End With
            ElseIf ActiveCell.Value > 6 Then
                With Selection.Interior
                    .ColorIndex = 3
                    .Pattern = xlSolid
                End With
            End If
            ActiveCell.Offset(1, 0).Select
        Next counter4
        Range(init).Select
        ActiveCell.Offset(0, (counter1 * 3) - 2).Select
    Next counter3
    
Range(init).Select

' Return cursor to default and allow screen updating
Application.ScreenUpdating = True
Application.Cursor = xlDefault

End Sub

Sub import_multi_csv()

' import_multi_csv    [VBA]
'
' Author:    Jairus Bowne
' Acknowledgements:    Majority of code was adapted from the following site -
'                      http://tiny.cc/2x4r7 [bytes.com]
' Purpose:    Import selected .csv files as new worksheets
'
' Input:    None, really. Open Excel and run the macro. 
'           Files will then be imported into a new workbook.

Dim Str1 As String
Dim ii As Integer
Dim str2 As String
Dim name_check As Object

Application.SheetsInNewWorkbook = 1
Set newBook = Workbooks.Add
newBook.Activate

With Application.FileDialog(msoFileDialogFilePicker)
     .AllowMultiSelect = True
     .Filters.Clear
     .Filters.Add "CSV (Comma delimited)", "*.csv"
     .Filters.Add "Text files", "*.txt"
     .Filters.Add "All files", "*.*"
     .Show
    If .SelectedItems.Count > 0 Then
        For ii = 1 To .SelectedItems.Count
            If Worksheets.Count < ii Then
                Worksheets.Add after:=Worksheets(ii - 1)
            End If
            Worksheets(ii).Activate
            str2 = .SelectedItems.Item(ii)
            Str1 = "TEXT;" & str2
            expath = Split(str2, Application.PathSeparator)
            f_nm = expath(UBound(expath))
            s_nm = Left(f_nm, Len(f_nm) - 4)
            If Len(s_nm) > 31 Then
                sub_s_nm = Left(s_nm, 31)
            Else
                sub_s_nm = s_nm
            End If
            With ActiveSheet.QueryTables.Add(Connection:=Str1, _
                    Destination:=Range("A1"))
                .TextFileCommaDelimiter = True
                .Refresh BackgroundQuery:=False
                On Error Resume Next
                Set name_check = Sheets(sub_s_nm)
                On Error GoTo 0
                If name_check Is Nothing Then
                    If Len(s_nm) < 31 Then
                        Sheets(ii).Name = s_nm
                    Else
                        Sheets(ii).Name = sub_s_nm
                    End If
                Else
                    Sheets(ii).Name = Left(s_nm, 29) & "_" & Sheets.Count
                End If
            End With
        Next
    End If
End With

' Update the screen and fix the cursor
Application.ScreenUpdating = True
Application.Cursor = xlDefault

End Sub

Sub minmax_all()

'    minmax_all    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    Puts a red border around the highest value in a row, and
'                a blue border around the lowest value
'
'    Notes:    InSt must be the first metabolite row in the data set
'

' Define variables
Dim counter1 As Integer
Dim nmrw As Integer
Dim nmcl As Integer
Dim strw As Integer
Dim stcl As Integer
Dim wkrw As Integer
Dim c_cell As Range
Dim c_rng As Range
Dim c_min As Double
Dim c_max As Double

' Prevent screen flickering
Application.Cursor = xlWait
Application.ScreenUpdating = False

'Use the find_dataset macro to define sheet size
Application.Run "PERSONAL.XLSB!find_dataset"
nmrw = Selection.Rows.count
nmcl = Selection.Columns.count

' Define a starting point
Range("a1").Select
' Move to the first non-blank cell
If ActiveCell.Value = "" Then
    Selection.End(xlDown).Select
End If
Range(ActiveCell, Selection.End(xlDown)).Select
' Find the internal standard line (should be at the top).
Cells.Find("InSt", , , , , xlNext, False).Select
Selection.End(xlToRight).Select
ActiveCell.Offset(0, 1).Select
' Now that we've done that, define our starting point
init = ActiveCell.Address
stcl = ActiveCell.Column
strw = ActiveCell.Row

' Determine how many rows we're going to work on
' (+1 to account for the activecell position in strw)
wkrw = nmrw - strw + 1

Range(init).Select
'For counter1 = 1 To nmRw - 2
For counter1 = 1 To wkrw
    ActiveCell.Offset(0, -1).Select
    ' Select from activecell to the cell in column 2 in the same row
     Range(ActiveCell, Cells(ActiveCell.Row, 2)).Select
    ' Label this area for later
    Set c_rng = Selection
    c_min = Application.WorksheetFunction.min(c_rng)
    c_max = Application.WorksheetFunction.Max(c_rng)
    ' Test each cell sequentially
    For Each c_cell In c_rng
        With c_cell.Borders
            .ColorIndex = xlNone
            .LineStyle = xlNone
        End With
        If c_cell.Value = c_max Then
            ' MUST be "c_cell", not "Selection", as it will
            ' colour in the whole range otherwise!
            With c_cell.Borders
                .ColorIndex = 3
                .LineStyle = xlSolid
                .Weight = 3
            End With
        ElseIf c_cell.Value = c_min Then
            With c_cell.Borders
                .ColorIndex = 32
                .LineStyle = xlSolid
                .Weight = 3
            End With
        End If
    Next c_cell
    ' Go back to the starting cell, and move down by the number of rows already done
    Range(init).Select
    ActiveCell.Offset(counter1, 0).Select
Next counter1

Range(init).Select
' Return cursor to default and allow screen updating
Application.ScreenUpdating = True
Application.Cursor = xlDefault

End Sub

Sub neg_folds_fl()

'    neg_folds    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    Convert x-fold data from fractional (0.025) to
'                negative (-40.0 ) values
'
'    Input:    Folds sheet.

' Define some variables
Dim counter1 As Integer
Dim counter2 As Integer

' Prevent screen flicker
Application.Cursor = xlWait
Application.ScreenUpdating = False

' Define a starting point
Range("a1").Select
init = ActiveCell.Address
ActiveCell.Offset(2, 1).Select
' Count rows
Range(ActiveCell, Selection.End(xlDown)).Select
nm_rw = Selection.Rows.count
ActiveCell.Select
' Count columns
Range(ActiveCell, Selection.End(xlToRight)).Select
nm_col = Selection.Columns.count

For counter1 = 1 To (nm_col + 1)
    For counter2 = 1 To nm_rw
        If ActiveCell.Value < 1 Then
            ' Get the cell's current formula as a string
            curr_formula = ActiveCell.Formula
            ' Replace the '=' with '=-1/' (invert and multiply by -1)
            new_formula = Replace(curr_formula, "=", "=-1/")
            ' Use the new formula
            ActiveCell.Formula = new_formula
        End If
        ActiveCell.Offset(1, 0).Select
    Next counter2
    Range(init).Select
    ActiveCell.Offset(2, counter1).Select
Next counter1

Range(init).Select
' Return cursor to default and allow screen updating
Application.ScreenUpdating = True
Application.Cursor = xlDefault

End Sub

Sub neg_folds_ov()

'    neg_folds    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    Convert x-fold data from fractional (0.025) to
'                negative (-40.0 ) values
'
'    Input:    Overview sheet.

' Define variables
Dim counter1 As Integer
Dim counter2 As Integer

' Prevent screen flicker
Application.Cursor = xlWait
Application.ScreenUpdating = False

' Define a starting point
Range("a1").Select
init = ActiveCell.Address
ActiveCell.Offset(2, 1).Select
' Count rows
Range(ActiveCell, Selection.End(xlDown)).Select
nm_rw = Selection.Rows.count
ActiveCell.Select
' Count columns
Range(ActiveCell, Selection.End(xlToRight)).Select
nm_col = Selection.Columns.count

For counter1 = 1 To (nm_col + 1)
    For counter2 = 1 To nm_rw
        If ActiveCell.Value < 1 Then
            ' Get the cell's current formula as a string
            curr_formula = ActiveCell.Formula
            ' Replace the '=' with '=-1/' (invert and multiply by -1)
            new_formula = Replace(curr_formula, "=", "=-1/")
            ' Use the new formula
            ActiveCell.Formula = new_formula
        End If
        ActiveCell.Offset(1, 0).Select
    Next counter2
    Range(init).Select
    ActiveCell.Offset(2, (3 * counter1) - 2).Select
Next counter1

Range(init).Select
' Return cursor to default and allow screen updating
Application.ScreenUpdating = True
Application.Cursor = xlDefault

End Sub

Sub oview2folds()

'    oview2folds    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    Generate the Folds sheet from the Overview sheet
'                (this comes from the CalcVals sheet; see template)
'
'    Input:    Overview sheet

' Prevent screen flicker
Application.Cursor = xlWait
Application.ScreenUpdating = False

' Define a starting point
Range("a1").Select
init = ActiveCell.Address

' Copy all the data
Application.Run "PERSONAL.XLSB!find_dataset"
Selection.Copy
Range("a1").Select
' Go to 'Folds' sheet, otherwise create it (in ErrHandler)
On Error GoTo ErrHandler:
Sheets("Folds").Activate
Sheets("Folds").Select
Range("A1").Select
' Paste (for the formatting)
ActiveSheet.Paste
' Paste as a link (for the data)
ActiveSheet.Paste Link:=True
' Clear A1 and A2
Range(ActiveCell, ActiveCell.Offset(1, 0)).Select
Selection.ClearContents
' Fit metabolite names
Columns("A:A").EntireColumn.AutoFit
Range(init).Select
ActiveCell.Offset(0, 1).Select
' Delete any columns that aren't x-folds
Do Until ActiveCell.Value = ""
    If ActiveCell.Value = "x-fold" Then
        ActiveCell.Offset(0, 1).Select
    Else
        Selection.EntireColumn.Delete
    End If
Loop

' Return cursor to default and allow screen updating
Application.ScreenUpdating = True
Application.Cursor = xlDefault

' Create a sheet if it doesn't exist (err.number 9)
ErrHandler:
    If Err.Number = 9 Then
    Worksheets.Add(After:=ActiveSheet).Name = "Folds"
    Resume
    End If
End Sub

Sub resp2calcvals()

'    resp2calcvals    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    Generate a basic CalcVals sheet based on Group information
'
'    Input:    Responses tab (named as such). Header layout (names of rows
'              here does not matter):
'                  Row 1:    File Name
'                  Row 2:    Fresh Weight
'                  Row 3:    Sample Name
'                  Row 4:    Group Information
'                  Row >=5:  Data matrix (metabolites in rows,
'                            samples in columns)
'    Output:    CalcVals sheet where the first group on the responses
'               tab is the reference group

' Define variables
Dim nrow As Integer
Dim ncol As Integer
Dim counter1 As Integer
Dim counter2 As Integer
Dim counter3 As Integer
Dim counter4 As Integer
Dim grp_dict As New Scripting.Dictionary

' Prevent screen flicker
Application.Cursor = xlWait
Application.ScreenUpdating = False

'
'    Responses data preparation
'
' Find dataset, retain information
Range("A1").Select
 On Error Resume Next
    end_row = Cells.Find("*", [A1], , , xlByRows, xlPrevious).Row
    end_col = Cells.Find("*", [A1], , , xlByColumns, xlPrevious).Column
    end_cell = Cells(end_row, end_col).Address
dataset = "a1:" & end_cell
Range(dataset).Select

' Get data size
nrow = Selection.Rows.count
ncol = Selection.Columns.count

' Get groups info
Range("b4").Select
Range(ActiveCell, ActiveCell.End(xlToRight)).Select
Set groupcols = Selection
num_groupcols = Selection.Columns.count

' Create a dictionary to store groups and numbers of replicates
Set grp_dict = CreateObject("Scripting.dictionary")
Range("b4").Select

For counter1 = 1 To num_groupcols
    ' If the group is not in the dictionary
    If Not grp_dict.Exists(ActiveCell.Value) Then
        ' add the group to the dictionary keys, and put the
        ' column reference as the item associated with the key
        grp_dict.Add ActiveCell.Value, counter1 + 1
        'MsgBox "" & grp_dict(ActiveCell.Value)
    End If
    'MsgBox "" & grp_dict(ActiveCell.Value)
    ActiveCell.Offset(0, 1).Select
Next counter1
' Add an empty string key with the endpoint
grp_dict.Add "", end_col + 1
' Group start points can now be referenced by their keys as:
'     Cells(4, grp_dict("BC")).Select
' Keys in dictionary are 0-indexed, and can be referenced as:
'     grp_dict.Keys(0)

'
'    Create the CalcVals sheet
'
Worksheets.Add(After:=ActiveSheet).Name = "CalcVals"
Sheets("CalcVals").Activate
Sheets("calcVals").Select

'
'    Prepare the ref column (average, std err, x-fold, sem)
'
Range("b1").Select
' Run a mini-macro to dump the labels in (we will recycle this)
Call calcvals_col_labels
' Remove t-Test column for reference samples
Range("f1").Select
ActiveCell.Value = ""
Range("b2").Select
ActiveCell.Value = grp_dict.Keys(0)
Range("b3").Select
' Average
' If this ConvertFormula function is not applied, the cell references
' are absolute - i.e. they are of the form $A$3 and don't fill properly
ActiveCell.Value = Application.ConvertFormula _
    (Formula:="=average(Responses!R5C" & grp_dict.Items(0) & _
    ":R5C" & grp_dict.Items(1) - 1 & ")", _
    FromReferenceStyle:=xlR1C1, ToReferenceStyle:=xlA1, _
    ToAbsolute:=xlRelative)
        ' alternatives:
        'Application.ConvertFormula _
        '            (Formula:=RdoRange.Areas(i).Formula, _
        '            FromReferenceStyle:=xlA1, _
        '            ToReferenceStyle:=xlA1, ToAbsolute:=xlRelRowAbsColumn)
        'Application.ConvertFormula _
        '            (Formula:=RdoRange.Areas(i).Formula, _
        '            FromReferenceStyle:=xlA1, _
        '            ToReferenceStyle:=xlA1, ToAbsolute:=xlAbsRowRelColumn)
        'Application.ConvertFormula _
        '            (Formula:=RdoRange.Areas(i).Formula, _
        '            FromReferenceStyle:=xlA1, _
        '            ToReferenceStyle:=xlA1, ToAbsolute:=xlAbsolute)
        'Application.ConvertFormula _
        '            (Formula:=RdoRange.Areas(i).Formula, _
        '            FromReferenceStyle:=xlA1, _
        '            ToReferenceStyle:=xlA1, ToAbsolute:=xlRelative)

ActiveCell.Offset(0, 1).Select
' Standard error
ActiveCell.Value = Application.ConvertFormula _
    (Formula:="=stdev(Responses!R5C" & grp_dict.Items(0) & _
    ":R5C" & grp_dict.Items(1) - 1 & ")/sqrt(" & _
    grp_dict.Items(1) - grp_dict.Items(0) & ")", _
    FromReferenceStyle:=xlR1C1, ToReferenceStyle:=xlA1, _
    ToAbsolute:=xlRelative)
ActiveCell.Offset(0, 1).Select
' x-fold (as this is the reference sample, set to 1)
ActiveCell.Value = "1"
ActiveCell.Offset(0, 1).Select
' For ease of recycling this line of code, use R1C1 notation
' i.e. make the formula divide the cell 2 columns to the left by
' the column 3 to the left; if in cell P5, formula becomes '=N5/M5'
' Standard error of the mean
ActiveCell.FormulaR1C1 = "=RC[-2]/RC[-3]"

'
'    Generate the sample column headers
'
For counter2 = 1 To grp_dict.count - 2
    Range("g1").Select
    ActiveCell.Offset(0, 6 * (counter2 - 1)).Select
    Call calcvals_col_labels
    ActiveCell.Value = grp_dict.Keys(counter2)
    'ActiveCell.Offset(-1, 0).Select
Next counter2

'
'    Generate the sample column formulae
'
For counter3 = 1 To grp_dict.count - 1
    ' Only for the actual groups (i.e. not using the blank key)
    If grp_dict.Keys(counter3) <> "" Then
        Range("g3").Select
        ActiveCell.Offset(0, (counter3 - 1) * 6).Select
        ' Average
        ActiveCell.Value = Application.ConvertFormula _
            (Formula:="=average(Responses!R5C" & grp_dict.Items(counter3) & _
            ":R5C" & grp_dict.Items(counter3 + 1) - 1 & ")", _
            FromReferenceStyle:=xlR1C1, ToReferenceStyle:=xlA1, _
            ToAbsolute:=xlRelative)
        ActiveCell.Offset(0, 1).Select
        ' Standard error
        ActiveCell.Value = Application.ConvertFormula _
            (Formula:="=stdev(Responses!R5C" & grp_dict.Items(counter3) & _
            ":R5C" & grp_dict.Items(counter3 + 1) - 1 & ")/sqrt(" & _
            grp_dict.Items(counter3 + 1) - grp_dict.Items(counter3) & ")", _
            FromReferenceStyle:=xlR1C1, ToReferenceStyle:=xlA1, _
            ToAbsolute:=xlRelative)
        ActiveCell.Offset(0, 1).Select
        ' x-fold
        ActiveCell.Value = Application.ConvertFormula _
            (Formula:="=RC[-2]/R3C2", FromReferenceStyle:=xlR1C1, _
            ToReferenceStyle:=xlA1, ToAbsolute:=xlRelRowAbsColumn)
        ActiveCell.Offset(0, 1).Select
        ' Standard error of the mean
        ActiveCell.FormulaR1C1 = "=RC[-2]/RC[-3]"
        ActiveCell.Offset(0, 1).Select
        ' t-Test
        ActiveCell.Value = Application.ConvertFormula _
            (Formula:="=ttest(Responses!R5C" & grp_dict.Items(counter3) & _
            ":R5C" & grp_dict.Items(counter3 + 1) - 1 & ",Responses!R5C" & _
            grp_dict.Items(0) & ":R5C" & grp_dict.Items(1) - 1 & ",2,2)", _
            FromReferenceStyle:=xlR1C1, ToReferenceStyle:=xlA1, _
            ToAbsolute:=xlRelRowAbsColumn)
    End If
Next counter3

' Add compound names
Range("a3").Select
ActiveCell.Formula = "=responses!a5"
' Fill the rest of the sheet
    ' (This is -5 as there are 4 rows that aren't 
    ' metabolites above the active cell)
Range(ActiveCell, ActiveCell.Offset(end_row - 5, 6 * grp_dict.count)).FillDown

'
'    Format sheet appearance
'
' Change the cell format to number (with 3 dp)
Range(ActiveCell, ActiveCell.Offset(end_row - 5, 6 * grp_dict.count)).Select
Selection.NumberFormat = "0.000"

' Merge column headers
For counter4 = 1 To grp_dict.count - 1
    Range("a2").Select
    ' The first group has one less column than the rest
    If counter4 = 1 Then
        Range(ActiveCell.Offset(0, 1), ActiveCell.Offset(0, 3)).Select
        Selection.Merge
        Selection.HorizontalAlignment = xlCenter
    Else
        ActiveCell.Offset(0, 6 * (counter4 - 1)).Select
        Range(ActiveCell, ActiveCell.Offset(0, 4)).Select
        Selection.Merge
        Selection.HorizontalAlignment = xlCenter
    End If
Next counter4

' Add a border to the left hand side of the columns
For counter5 = 1 To grp_dict.count - 1
    Range("a1").Select
    If counter5 = 1 Then
        ActiveCell.Offset(0, 1).Select
    Else
        ActiveCell.Offset(0, (counter5 - 1) * 6).Select
    End If
    Range(ActiveCell.Offset(2, 0), ActiveCell.End(xlDown)).Select
    Selection.Borders(xlEdgeLeft).Weight = xlThin
Next counter5

' Autofit the metabolite names
Range("a1").Select
Selection.EntireColumn.AutoFit

' Freeze panes to B3
Range("b3").Select
ActiveWindow.FreezePanes = True

' Clear the dictionary to free up memory
Set grp_dict = Nothing

' Return cursor to default and allow screen updating
Application.ScreenUpdating = True
Application.Cursor = xlDefault

End Sub

Private Sub calcvals_col_labels()

'    calcvals_col_labels    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    Called by resp2calcvals script
'                Use this to add the column headers for each block

ActiveCell.Value = "average"
ActiveCell.Offset(0, 1).Select
ActiveCell.Value = "std err"
ActiveCell.Offset(0, 1).Select
ActiveCell.Value = "x-fold"
ActiveCell.Offset(0, 1).Select
ActiveCell.Value = "sem"
ActiveCell.Offset(0, 1).Select
ActiveCell.Value = "t-Test"
ActiveCell.Offset(1, -4).Select

End Sub

Sub tms_trim()

'    tms_trim    [VBA]
'
'    Author:    Jairus Bowne
'    Purpose:    Remove _nTMS from compound names
'
'    Input:    Any sheet that has compound names ending in _nTMS (without duplicate entries)

' Define variables
Dim counter1 As Integer

' Prevent screen flicker
Application.Cursor = xlWait
Application.ScreenUpdating = False

' Define a starting point
Range("a1").Select
init = ActiveCell.Address

' Find dataset, retain information
On Error Resume Next
    end_row = Cells.Find("*", [A1], , , xlByRows, xlPrevious).Row
    end_col = Cells.Find("*", [A1], , , xlByColumns, xlPrevious).Column
    end_cell = Cells(end_row, end_col).Address
Range(init, "a" & end_row).Select
nrw = Selection.Rows.count
Range(init).Select

For counter1 = 1 To nrw
    If Right(ActiveCell.Value, 4) = "_TMS" Then
        ActiveCell.Value = Left(ActiveCell.Value, (Len(ActiveCell.Value) - 4))
    ElseIf Right(ActiveCell.Value, 5) = "_2TMS" Then
        ActiveCell.Value = Left(ActiveCell.Value, (Len(ActiveCell.Value) - 5))
    ElseIf Right(ActiveCell.Value, 5) = "_3TMS" Then
        ActiveCell.Value = Left(ActiveCell.Value, (Len(ActiveCell.Value) - 5))
    ElseIf Right(ActiveCell.Value, 5) = "_4TMS" Then
        ActiveCell.Value = Left(ActiveCell.Value, (Len(ActiveCell.Value) - 5))
    'Also do the MX1 and MX2's
    ElseIf Right(ActiveCell.Value, 4) = "_MX1" Then
        ActiveCell.Value = Left(ActiveCell.Value, (Len(ActiveCell.Value) - 4))
    ElseIf Right(ActiveCell.Value, 4) = "_MX2" Then
        ActiveCell.Value = Left(ActiveCell.Value, (Len(ActiveCell.Value) - 4))
    End If
    ActiveCell.Offset(1, 0).Select
Next counter1

Range(init).Select
' Return cursor to default and allow screen updating
Application.ScreenUpdating = True
Application.Cursor = xlDefault

End Sub
