Sub import_multi_csv()

' import_multi_csv    [VBA]
'
' Author:    Jairus Bowne
' Acknowledgements:    Majority of code was adapted from the following site -
'    http://bytes.com/topic/visual-basic/answers/  ... (continues next line)
'    655253-import-multiple-csv-files-excel-semi-colon-delimiter#post2600506
' Purpose:    Import selected .csv files as new worksheets
'
' Input: None, really. Open Excel and run the macro; data will be imported 
'        to a new workbook.

Dim Str1 As String
Dim i As Integer
Dim str2 As String

Set newbook = Workbooks.Add
newbook.Activate

With Application.FileDialog(msoFileDialogFilePicker)
     ' Accept more than one file
     .AllowMultiSelect = True
     .Filters.Clear
     ' Take .csv or .txt input
     .Filters.Add "CSV (Comma delimited)", "*.csv"
     .Filters.Add "Text files", "*.txt"
     .Filters.Add "All files", "*.*"
     .Show

        If .SelectedItems.Count > 0 Then
            For i = 1 To .SelectedItems.Count
                    If Worksheets.Count < i Then
                    Worksheets.Add After:=Worksheets(i - 1)
                    End If
                Worksheets(i).Activate
                str2 = .SelectedItems.Item(i)
                Str1 = "TEXT;" & str2
                    expath = Split(str2, Application.PathSeparator)
                    f_nm = expath(UBound(expath))
                    s_nm = Left(f_nm, Len(f_nm) - 4)

                    With ActiveSheet.QueryTables.Add(Connection:=Str1, Destination:=Range("A1"))
                                .TextFileCommaDelimiter = True
                                .Refresh BackgroundQuery:=False
                                Sheets(i).Name = s_nm
                    End With
            Next
        End If
End With

End Sub


