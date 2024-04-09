VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   3195
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   ScaleHeight     =   3195
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command1 
      Caption         =   "Command1"
      Height          =   375
      Left            =   1800
      TabIndex        =   0
      Top             =   1320
      Width           =   1095
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()

Dim BolTemp As Boolean

Dim objui As UIOrder.UIOrd
Set objui = New UIOrder.UIOrd



objui.ConParameter = "Provider=MSDATASHAPE;DSN=CRM;SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=YES;"


BolTemp = objui.Add()

If BolTemp Then
  MsgBox (BolTemp)
End If

Dim objchild As UIOrder.UIOrdChld


objui.Ordered = "abcdef"
objui.Order_type = "S"
objui.stats = "Z"
objui.Ordered = "1"
objui.Add_Date = Date

Set objchild = objui.childAddnew(1)

objchild.OrdType_line = "é"
objchild.Order_line = "1"
BolTemp = objui.ChildSet(OrderLine, objchild)


BolTemp = objui.Save()

Dim RS As ADODB.Recordset
Set RS = New ADODB.Recordset

End Sub
