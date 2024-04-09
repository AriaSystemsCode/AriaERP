VERSION 5.00
Begin VB.Form frmSetting 
   Caption         =   "Communication Settings"
   ClientHeight    =   6675
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   5580
   LinkTopic       =   "Form1"
   ScaleHeight     =   6675
   ScaleWidth      =   5580
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame Frame1 
      Caption         =   "Item Information"
      Height          =   675
      Left            =   90
      TabIndex        =   25
      Top             =   2925
      Width           =   5370
      Begin VB.TextBox txtItemMask 
         Height          =   345
         Left            =   1260
         TabIndex        =   4
         Top             =   240
         Width           =   2385
      End
      Begin VB.Label Label6 
         Caption         =   "Mask:"
         Height          =   270
         Left            =   135
         TabIndex        =   26
         Top             =   255
         Width           =   990
      End
   End
   Begin VB.Frame Frame5 
      Caption         =   "Outgoing Data File Path"
      Height          =   2835
      Left            =   2820
      TabIndex        =   21
      Top             =   75
      Width           =   2640
      Begin VB.DirListBox DirOutgoingDir 
         Height          =   1665
         Left            =   75
         TabIndex        =   3
         Top             =   1050
         Width           =   2430
      End
      Begin VB.DriveListBox drvOutgoingDrv 
         Height          =   315
         Left            =   90
         TabIndex        =   2
         Top             =   675
         Width           =   2430
      End
      Begin VB.TextBox txtOutgoingDir 
         Enabled         =   0   'False
         Height          =   330
         Left            =   90
         TabIndex        =   22
         Top             =   285
         Width           =   2430
      End
   End
   Begin VB.Frame Frame4 
      Caption         =   "Data Path"
      Height          =   2835
      Left            =   90
      TabIndex        =   19
      Top             =   75
      Width           =   2640
      Begin VB.DirListBox DirDataDir 
         Height          =   1665
         Left            =   90
         TabIndex        =   1
         Top             =   1050
         Width           =   2430
      End
      Begin VB.DriveListBox DrvDataDrive 
         Height          =   315
         Left            =   75
         TabIndex        =   0
         Top             =   675
         Width           =   2430
      End
      Begin VB.TextBox txtdataDir 
         Enabled         =   0   'False
         Height          =   330
         Left            =   90
         TabIndex        =   20
         Top             =   285
         Width           =   2430
      End
   End
   Begin VB.Frame Frame3 
      Caption         =   "Store Information"
      Height          =   1080
      Left            =   105
      TabIndex        =   16
      Top             =   5115
      Width           =   5385
      Begin VB.TextBox txtStoreName 
         Height          =   345
         Left            =   1260
         MaxLength       =   30
         TabIndex        =   10
         Top             =   630
         Width           =   3915
      End
      Begin VB.TextBox txtSenderId 
         Height          =   345
         Left            =   3345
         MaxLength       =   15
         TabIndex        =   9
         Top             =   255
         Width           =   1815
      End
      Begin VB.TextBox txtStoreId 
         Height          =   345
         Left            =   1260
         MaxLength       =   8
         TabIndex        =   8
         Top             =   255
         Width           =   960
      End
      Begin VB.Label Label1 
         Caption         =   "Name"
         Height          =   210
         Left            =   180
         TabIndex        =   23
         Top             =   660
         Width           =   735
      End
      Begin VB.Label Label7 
         Caption         =   "Store ID:"
         Height          =   300
         Left            =   150
         TabIndex        =   18
         Top             =   315
         Width           =   1080
      End
      Begin VB.Label Label4 
         Caption         =   "Phone#:"
         Height          =   270
         Left            =   2340
         TabIndex        =   17
         Top             =   315
         Width           =   765
      End
   End
   Begin VB.Frame Frame2 
      Caption         =   "Main Office Information"
      Height          =   1440
      Left            =   60
      TabIndex        =   13
      Top             =   3630
      Width           =   5385
      Begin VB.TextBox txtReceivername 
         Height          =   345
         Left            =   1260
         MaxLength       =   30
         TabIndex        =   7
         Top             =   1005
         Width           =   3915
      End
      Begin VB.TextBox txtReceiverEMail 
         Height          =   345
         Left            =   1260
         MaxLength       =   30
         TabIndex        =   6
         Top             =   630
         Width           =   3900
      End
      Begin VB.TextBox txtReceiverId 
         Height          =   345
         Left            =   1260
         MaxLength       =   15
         TabIndex        =   5
         Top             =   270
         Width           =   1815
      End
      Begin VB.Label Label2 
         Caption         =   "Name:"
         Height          =   210
         Left            =   135
         TabIndex        =   24
         Top             =   1065
         Width           =   885
      End
      Begin VB.Label Label5 
         Caption         =   "E-Mail Address:"
         Height          =   225
         Left            =   135
         TabIndex        =   15
         Top             =   690
         Width           =   1110
      End
      Begin VB.Label Label3 
         Caption         =   "Phone#:"
         Height          =   300
         Left            =   135
         TabIndex        =   14
         Top             =   315
         Width           =   750
      End
   End
   Begin VB.CommandButton cmdCancel 
      Caption         =   "&Cancel"
      Height          =   420
      Left            =   3255
      TabIndex        =   12
      Top             =   6255
      Width           =   1350
   End
   Begin VB.CommandButton cmdSave 
      Caption         =   "&Save"
      Height          =   420
      Left            =   1050
      TabIndex        =   11
      Top             =   6270
      Width           =   1500
   End
End
Attribute VB_Name = "frmSetting"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cmdCancel_Click()
Unload Me
End Sub

Private Sub cmdSave_Click()

WritePrivateProfileString "Send Activity", "DataDir", Me.txtdataDir.Text, App.Path & "\SendActi.ini"
WritePrivateProfileString "Send Activity", "OutgoingFileDir", Me.txtOutgoingDir.Text, App.Path & "\SendActi.ini"
WritePrivateProfileString "Send Activity", "ReceiverId", Me.txtReceiverId.Text, App.Path & "\SendActi.ini"
WritePrivateProfileString "Send Activity", "ReceiverEmail", Me.txtReceiverEMail.Text, App.Path & "\SendActi.ini"
WritePrivateProfileString "Send Activity", "ReceiverName", Me.txtReceivername.Text, App.Path & "\SendActi.ini"
WritePrivateProfileString "Send Activity", "SenderId", Me.txtSenderId.Text, App.Path & "\SendActi.ini"
WritePrivateProfileString "Send Activity", "StoreId", Me.txtStoreId.Text, App.Path & "\SendActi.ini"
WritePrivateProfileString "Send Activity", "StoreName", Me.txtStoreName.Text, App.Path & "\SendActi.ini"
WritePrivateProfileString "Send Activity", "ItemMask", Me.txtItemMask.Text, App.Path & "\SendActi.ini"
Unload Me
End Sub

Private Sub DirDataDir_Change()
Me.txtdataDir = Me.DirDataDir.Path
End Sub

Private Sub DirOutgoingDir_Change()
Me.txtOutgoingDir = Me.DirOutgoingDir.Path
End Sub

Private Sub DrvDataDrive_Change()
Me.DirDataDir.Path = Me.DrvDataDrive.Drive
End Sub

Private Sub drvOutgoingDrv_Change()
Me.DirOutgoingDir.Path = Me.drvOutgoingDrv.Drive
End Sub

Private Sub Form_Load()

Dim lcreturn As String
If Dir(App.Path & "\SendActi.ini") = "SendActi.ini" Then
  lcreturn = Space(100)
  GetPrivateProfileString "Send Activity", "DataDir", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
  lcreturn = Trim$(lcreturn)
  Me.DirDataDir.Path = lcreturn
  Me.txtdataDir.Text = lcreturn
  Me.DrvDataDrive.Drive = Left$(Me.txtdataDir, 1)

  lcreturn = Space(100)
  GetPrivateProfileString "Send Activity", "OutgoingFileDir", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
  lcreturn = Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1) ' & "\"
  Me.DirOutgoingDir.Path = Trim$(lcreturn)
  Me.drvOutgoingDrv.Drive = Left$(lcreturn, 1)

  lcreturn = Space(100)
  GetPrivateProfileString "Send Activity", "SenderId", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
  Me.txtSenderId = Trim$(lcreturn)

  lcreturn = Space(100)
  GetPrivateProfileString "Send Activity", "StoreId", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
  Me.txtStoreId = Trim$(lcreturn)

  lcreturn = Space(100)
  GetPrivateProfileString "Send Activity", "StoreName", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
  Me.txtStoreName = Trim$(lcreturn)

  lcreturn = Space(100)
  GetPrivateProfileString "Send Activity", "ReceiverId", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
  Me.txtReceiverId = Trim$(lcreturn)

  lcreturn = Space(100)
  GetPrivateProfileString "Send Activity", "ReceiverEmail", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
  Me.txtReceiverEMail = Trim$(lcreturn)

  lcreturn = Space(100)
  GetPrivateProfileString "Send Activity", "ReceiverName", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
  Me.txtReceivername = Trim$(lcreturn)

  lcreturn = Space(100)
  GetPrivateProfileString "Send Activity", "ItemMask", "#######/####/#######", lcreturn, 160, App.Path & "\SendActi.ini"
  Me.txtItemMask = Trim$(lcreturn)

Else
  If Dir(App.Path & "\Data", vbDirectory) <> "Data" Then
    MkDir App.Path & "\Data"
  End If
  Me.DrvDataDrive = Left(App.Path, 1)
  Me.DirDataDir.Path = Left$(Me.DrvDataDrive, 1) & Mid$(App.Path, 2) & "\Data\"
  Me.txtdataDir.Text = Me.DirDataDir.Path
  If Dir(App.Path & "\OutBox", vbDirectory) <> "OutBox" Then
    MkDir App.Path & "\OutBox"
  End If
  Me.drvOutgoingDrv = Left(App.Path, 1)
  Me.DirOutgoingDir.Path = Left$(Me.drvOutgoingDrv, 1) & Mid$(App.Path, 2) & "\OutBox\"
  Me.txtOutgoingDir.Text = Me.DirOutgoingDir.Path

  WritePrivateProfileString "Send Activity", "DataDir", Me.txtdataDir.Text, App.Path & "\SendActi.ini"
  WritePrivateProfileString "Send Activity", "OutgoingFileDir", Me.txtOutgoingDir.Text, App.Path & "\SendActi.ini"
  WritePrivateProfileString "Send Activity", "ItemMask", "#######/####/#######", App.Path & "\SendActi.ini"
  Me.txtItemMask.Text = "#######/####/#######"
End If
If Dir(App.Path & "\Data\TMACTTYP.DBF") <> "TMACTTYP.DBF" Or _
   Dir(App.Path & "\Data\TMACTIVI.DBF") <> "TMACTIVI.DBF" Then
  Dim ObjConnection As New ADODB.Connection
  Dim strCnn As String
  strCnn = "Provider=MSDATASHAPE.1;DSN=Visual FoxPro Database;UID=;SourceDB=" & Me.DirDataDir.Path & _
  ";SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=No;"
  ObjConnection.Open strCnn
  If Dir(App.Path & "\Data\TMACTTYP.DBF") <> "TMACTTYP.DBF" Then
    ObjConnection.Execute "CREATE TABLE '" & Me.DirDataDir.Path & "\TMACTTYP.DBF' (CTYPE C(5), CEDITYPE C(2), CTYPEDESC  C(30),nLastLine N(10),nUsedLine N(10))"
    ObjConnection.Execute "INSERT INTO TMACTTYP (CTYPE,CEDITYPE,CTYPEDESC,nLastLine) VALUES ('QR000','QR','Received From Vendor',0)"
    ObjConnection.Execute "INSERT INTO TMACTTYP (CTYPE,CEDITYPE,CTYPEDESC,nLastLine) VALUES ('DG000','DG','Returned to Vendor',0)"
    ObjConnection.Execute "INSERT INTO TMACTTYP (CTYPE,CEDITYPE,CTYPEDESC,nLastLine) VALUES ('QS000','QS','Sold',0)"
    ObjConnection.Execute "INSERT INTO TMACTTYP (CTYPE,CEDITYPE,CTYPEDESC,nLastLine) VALUES ('QU000','QU','Received by Consumer',0)"
  End If
  If Dir(App.Path & "\Data\TMACTIVI.DBF") <> "TMACTIVI.DBF" Then
    ObjConnection.Execute "CREATE TABLE '" & Me.DirDataDir.Path & "\TMACTIVI.DBF' (CTYPE C(5), CEDITYPE C(2), cItem C(19),cSize C(1),nQuantity N(8),nPrice N(8,2),nAmount N(10,2), dDate D,dSentDate D,lSent L,nLineNo N(10))"
  End If
  ObjConnection.Close
  Set ObjConnection = Nothing
End If
End Sub

Private Sub txtReceiverId_KeyPress(KeyAscii As Integer)
If Not IsNumeric(Chr(KeyAscii)) Then
  KeyAscii = 0
End If

End Sub

Private Sub txtSenderId_KeyPress(KeyAscii As Integer)
If Not IsNumeric(Chr(KeyAscii)) Then
  KeyAscii = 0
End If

End Sub
