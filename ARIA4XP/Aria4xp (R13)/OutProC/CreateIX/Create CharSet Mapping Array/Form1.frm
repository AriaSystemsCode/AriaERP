VERSION 5.00
Object = "{67397AA1-7FB1-11D0-B148-00A0C922E820}#6.0#0"; "MSADODC.OCX"
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   4440
   ClientLeft      =   60
   ClientTop       =   450
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   ScaleHeight     =   4440
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command3 
      Caption         =   "Command3"
      Height          =   495
      Left            =   1920
      TabIndex        =   2
      Top             =   3480
      Width           =   1695
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Command2"
      Height          =   615
      Left            =   2040
      TabIndex        =   1
      Top             =   1680
      Width           =   1695
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Command1"
      Height          =   255
      Left            =   1680
      TabIndex        =   0
      Top             =   360
      Width           =   2055
   End
   Begin MSAdodcLib.Adodc Adodc1 
      Height          =   330
      Left            =   960
      Top             =   840
      Width           =   3255
      _ExtentX        =   5741
      _ExtentY        =   582
      ConnectMode     =   0
      CursorLocation  =   3
      IsolationLevel  =   -1
      ConnectionTimeout=   15
      CommandTimeout  =   30
      CursorType      =   3
      LockType        =   3
      CommandType     =   8
      CursorOptions   =   0
      CacheSize       =   50
      MaxRecords      =   0
      BOFAction       =   0
      EOFAction       =   0
      ConnectStringType=   1
      Appearance      =   1
      BackColor       =   -2147483643
      ForeColor       =   -2147483640
      Orientation     =   0
      Enabled         =   -1
      Connect         =   "Provider=SQLOLEDB.1;Persist Security Info=False;User ID=sa;Initial Catalog=tempdb;Data Source=ARIA4"
      OLEDBString     =   "Provider=SQLOLEDB.1;Persist Security Info=False;User ID=sa;Initial Catalog=tempdb;Data Source=ARIA4"
      OLEDBFile       =   ""
      DataSourceName  =   ""
      OtherAttributes =   ""
      UserName        =   ""
      Password        =   ""
      RecordSource    =   ""
      Caption         =   "Adodc1"
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      _Version        =   393216
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
  
  
  Dim objCon As New ADODB.Connection
  Call objCon.Open("Provider=SQLOLEDB.1;Persist Security Info=False;User ID=sa;pwd=aria;Initial Catalog=tempdb;Data Source=ARIA4")
  
  Dim objRS As New Recordset
  Call objRS.Open("Select * FROM table3", objCon)
  
  
  For i = 1 To 100
    Debug.Print "laAscii[" & Format(Trim(objRS.Fields("ascci").Value + 1), "000") & "] = '" & Format(objRS.Fields("int").Value + 1, "000") & "'"
    Call objRS.MoveNext
  Next i


  For i = 101 To 200
    Debug.Print "laAscii[" & Format(Trim(objRS.Fields("ascci").Value + 1), "000") & "] = '" & Format(objRS.Fields("int").Value + 1, "000") & "'"
    Call objRS.MoveNext
  Next i

  For i = 201 To 256
    Debug.Print "laAscii[" & Format(Trim(objRS.Fields("ascci").Value + 1), "000") & "] = '" & Format(objRS.Fields("int").Value + 1, "000") & "'"
    Call objRS.MoveNext
  Next i
End Sub

Private Sub Command2_Click()
  Dim objCon As New ADODB.Connection
  objCon.CursorLocation = adUseClient
  Call objCon.Open("Provider=SQLOLEDB.1;Persist Security Info=False;User ID=sa;pwd=aria;Initial Catalog=tempdb;Data Source=ARIA4")
  
  Dim objRS As New Recordset
  Call objRS.Open("Select * FROM Table1", objCon, adOpenStatic, adLockBatchOptimistic)
  
  
  For i = 0 To 255
    objRS.AddNew
    objRS.Fields("a").Value = Chr(i)
    objRS.Fields("b").Value = i
    
  Next i
objRS.UpdateBatch
End Sub

Private Sub Command3_Click()
  Dim objCon As New ADODB.Connection
  objCon.CursorLocation = adUseClient
  Call objCon.Open("Provider=SQLOLEDB.1;Persist Security Info=False;User ID=sa;pwd=aria;Initial Catalog=tempdb;Data Source=ARIA4")
  
  Dim objRS As New Recordset
  Call objRS.Open("Select * FROM Table1 order by a", objCon, adOpenStatic, adLockBatchOptimistic)
  
  i = 0
  Do Until objRS.EOF
  objRS.Fields("c").Value = i
  objRS.MoveNext
  i = i + 1
  Loop
  objRS.UpdateBatch
End Sub
