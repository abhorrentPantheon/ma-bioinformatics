Attribute VB_Name = "cursor_fix"
Sub cursor_fix()

' cursor_fix [VBA]
'
' Author:    Jairus Bowne
' Purpose:    Reset the cursor to default style when locked into wait style.
'             May occur because a macro borked it.
'

Application.ScreenUpdating = True
Application.Cursor = xlDefault

End Sub