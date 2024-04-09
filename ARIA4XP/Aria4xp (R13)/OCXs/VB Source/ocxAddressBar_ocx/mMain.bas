Attribute VB_Name = "mMain"
Option Explicit

Private Declare Sub InitCommonControls Lib "comctl32.dll" ()

Public Sub Main()
   InitCommonControls
   frmQuickStart.Show
End Sub
