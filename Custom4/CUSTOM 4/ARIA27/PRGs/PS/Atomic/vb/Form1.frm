VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   1500
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   3570
   LinkTopic       =   "Form1"
   ScaleHeight     =   1500
   ScaleWidth      =   3570
   StartUpPosition =   3  'Windows Default
   Visible         =   0   'False
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Declare Function StopAuth Lib "atom32.dll" () As Integer

Private Sub Form_Load()
 Dim intHandl As Integer
 
 intHandl = StopAuth()

 If intHandl = 0 Then
   'MsgBox "removed successful"
 Else
   'MsgBox "Erorr"
 End If
 
 Unload Me
End Sub
