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
