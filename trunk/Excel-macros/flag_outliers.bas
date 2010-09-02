Attribute VB_Name = "flag_outliers"
Sub flag_outliers()

' flag_outliers [VBA]
'
' Author:    Jairus Bowne
' Purpose:    Highlight values that are outliers with a red border
'
' Input:    Any retention time data matrix (gaps are acceptable)
' Notes:    This script has been tested on an output file of the mhpp.R script


' Define variables for use later
Dim counter1 As Integer
Dim arange As Range
Dim vcell As Range

Application.Cursor = xlWait
Application.ScreenUpdating = False

' Find the dataset
Range("a1").Select
init = ActiveCell.Address
 On Error Resume Next
    end_row = Cells.Find("*", [A1], , , xlByRows, xlPrevious).Row
    end_col = Cells.Find("*", [A1], , , xlByColumns, xlPrevious).Column
    end_cell = Cells(end_row, end_col).Address
dataset = "a1:" & end_cell

Range(dataset).Select
nrow = Selection.Rows.Count
ncol = Selection.Columns.Count

Range(init).Select
ActiveCell.Offset(1, 2).Select

' For loop #1: treat each column as a group of numbers, and get
'              some information out of them
For counter1 = 1 To (ncol - 2)
    Range(ActiveCell, ActiveCell.Offset((nrow - 2), 0)).Select
    Set arange = Selection
    Range(arange).Select
    mean = Application.WorksheetFunction.Average(arange)
    sd = Application.WorksheetFunction.StDev(arange)

    ' For loop #2: test every cell in the column individually
    ' Use z-score to determine whether or not it should be flagged
    For Each vcell In arange
    If vcell <> "" Then
        ' If the cell's value has a z-score of 3, flag it with a red border
        '     i.e. if the cell is 3 standard deviations from the mean, flag it. 
        If Abs(((vcell - mean) / sd)) >= 3 Then
            With vcell.Borders
                .ColorIndex = 3
                .LineStyle = xlSolid
                .Weight = 3
            End With
        Else
        ' Otherwise clear the borders
        With c_cell.Borders
            .ColorIndex = xlNone
            .LineStyle = xlNone
        End With
        End If
    End If
    Next vcell
    ActiveCell.Offset(0, 1).Select
Next counter1

' Update the screen and fix the cursor
Application.ScreenUpdating = True
Application.Cursor = xlDefault

End Sub