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

'
'    Define variables
'
Dim Str1 As String
Dim ii As Integer
Dim str2 As String
Dim name_check As Object

'
' Create a new workbook
'
Application.SheetsInNewWorkbook = 1
Set newBook = Workbooks.Add
newBook.Activate

' Open a file dialogue box to select files
With Application.FileDialog(msoFileDialogFilePicker)
     .AllowMultiSelect = True
     .Filters.Clear
     ' Allow (csv, txt, all) as file types
     .Filters.Add "CSV (Comma delimited)", "*.csv"
     .Filters.Add "Text files", "*.txt"
     .Filters.Add "All files", "*.*"
     .Show
    
    ' Prevent screen flickering
    Application.Cursor = xlWait
    Application.ScreenUpdating = False
    
    ' Provided there are files selected...
    If .SelectedItems.Count > 0 Then
        ' Add a worksheet for each file
        For ii = 1 To .SelectedItems.Count
            If Worksheets.Count < ii Then
                Worksheets.Add after:=Worksheets(ii - 1)
            End If
            
            '
            ' Get the base file name
            '
            Worksheets(ii).Activate
            str2 = .SelectedItems.Item(ii)
            Str1 = "TEXT;" & str2
            ' Get full path to file
            expath = Split(str2, Application.PathSeparator)
            ' Get file name from full path
            f_nm = expath(UBound(expath))
            ' Remove file extension
            s_nm = Left(f_nm, Len(f_nm) - 4)
            ' If file name > 31, cannot assign to sheet name, so trim it
            If Len(s_nm) > 31 Then
                sub_s_nm = Left(s_nm, 31)
            Else
                sub_s_nm = s_nm
            End If
            
            '
            ' Rename sheet with file name
            '
            ' Import the data
            With ActiveSheet.QueryTables.Add(Connection:=Str1, _
                    Destination:=Range("A1"))
                .TextFileCommaDelimiter = True
                .Refresh BackgroundQuery:=False
                On Error Resume Next
                Set name_check = Sheets(sub_s_nm)
                On Error GoTo 0
                ' Rename the sheets
                If name_check Is Nothing Then
                    If Len(s_nm) < 31 Then
                        Sheets(ii).Name = s_nm
                    Else
                        Sheets(ii).Name = sub_s_nm
                    End If
                Else
                    ' If the sheet name exists, append the sheet number
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
