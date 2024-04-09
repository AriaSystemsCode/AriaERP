VERSION 5.00
Object = "{CDE57A40-8B86-11D0-B3C6-00A0C90AEA82}#1.0#0"; "MSDATGRD.OCX"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "mscomctl.ocx"
Object = "{C932BA88-4374-101B-A56C-00AA003668DC}#1.1#0"; "MSMASK32.OCX"
Begin VB.Form frmPrdAct 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Product Activities"
   ClientHeight    =   5460
   ClientLeft      =   150
   ClientTop       =   435
   ClientWidth     =   9795
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   5460
   ScaleWidth      =   9795
   StartUpPosition =   2  'CenterScreen
   Begin MSComctlLib.Toolbar Toolbar1 
      Align           =   1  'Align Top
      Height          =   480
      Left            =   0
      TabIndex        =   19
      Top             =   0
      Width           =   9795
      _ExtentX        =   17277
      _ExtentY        =   847
      ButtonWidth     =   714
      ButtonHeight    =   688
      Appearance      =   1
      ImageList       =   "ImageList1"
      _Version        =   393216
      BeginProperty Buttons {66833FE8-8583-11D1-B16A-00C0F0283628} 
         NumButtons      =   5
         BeginProperty Button1 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Setting"
            Object.ToolTipText     =   "Communication Setting"
            ImageKey        =   "Setting"
         EndProperty
         BeginProperty Button2 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Types"
            Object.ToolTipText     =   "Activity Types"
            ImageKey        =   "Types"
         EndProperty
         BeginProperty Button3 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Print"
            Object.ToolTipText     =   "Print"
            ImageKey        =   "Print"
         EndProperty
         BeginProperty Button4 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Send"
            Object.ToolTipText     =   "Send Activity"
            ImageKey        =   "Send"
         EndProperty
         BeginProperty Button5 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Close"
            Object.ToolTipText     =   "Close"
            ImageKey        =   "Close"
         EndProperty
      EndProperty
   End
   Begin MSComctlLib.ImageList ImageList1 
      Left            =   9060
      Top             =   60
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   20
      ImageHeight     =   20
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
         NumListImages   =   6
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "ProdActi.frx":0000
            Key             =   "Types"
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "ProdActi.frx":0ACC
            Key             =   "Setting"
         EndProperty
         BeginProperty ListImage3 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "ProdActi.frx":3280
            Key             =   "Refresh"
         EndProperty
         BeginProperty ListImage4 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "ProdActi.frx":3648
            Key             =   "Print"
         EndProperty
         BeginProperty ListImage5 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "ProdActi.frx":371C
            Key             =   "Send"
         EndProperty
         BeginProperty ListImage6 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "ProdActi.frx":37F8
            Key             =   "Close"
         EndProperty
      EndProperty
   End
   Begin VB.Frame Frame5 
      Height          =   5040
      Left            =   45
      TabIndex        =   20
      Top             =   405
      Width           =   9735
      Begin VB.Frame Frame7 
         Caption         =   "Summarize By"
         Height          =   600
         Left            =   6420
         TabIndex        =   31
         Top             =   225
         Width           =   2355
         Begin VB.ComboBox cboSummary 
            Height          =   315
            ItemData        =   "ProdActi.frx":3904
            Left            =   60
            List            =   "ProdActi.frx":3917
            Style           =   2  'Dropdown List
            TabIndex        =   6
            Top             =   195
            Width           =   2205
         End
      End
      Begin VB.CommandButton cmdRefresh 
         Caption         =   "&Refresh"
         Height          =   510
         Left            =   8850
         TabIndex        =   7
         Top             =   315
         Width           =   780
      End
      Begin VB.Frame fraActivity 
         Caption         =   "Activity"
         ForeColor       =   &H8000000D&
         Height          =   1470
         Left            =   90
         TabIndex        =   24
         Top             =   3480
         Width           =   8115
         Begin VB.TextBox txtActivity 
            Enabled         =   0   'False
            Height          =   345
            Left            =   900
            TabIndex        =   15
            Top             =   1035
            Width           =   2295
         End
         Begin VB.TextBox txtNewDate 
            BeginProperty DataFormat 
               Type            =   1
               Format          =   "dd/MM/yyyy"
               HaveTrueFalseNull=   0
               FirstDayOfWeek  =   0
               FirstWeekOfYear =   0
               LCID            =   3073
               SubFormatType   =   3
            EndProperty
            Height          =   345
            Left            =   3960
            TabIndex        =   10
            Top             =   240
            Width           =   1230
         End
         Begin VB.ComboBox cboActivity 
            Height          =   315
            ItemData        =   "ProdActi.frx":3960
            Left            =   900
            List            =   "ProdActi.frx":3962
            Style           =   2  'Dropdown List
            TabIndex        =   9
            Top             =   270
            Width           =   2295
         End
         Begin VB.TextBox txtDate 
            BeginProperty DataFormat 
               Type            =   1
               Format          =   "dd.MM.yyyy"
               HaveTrueFalseNull=   0
               FirstDayOfWeek  =   0
               FirstWeekOfYear =   0
               LCID            =   3073
               SubFormatType   =   3
            EndProperty
            Height          =   345
            Left            =   3960
            TabIndex        =   16
            Top             =   1035
            Width           =   1230
         End
         Begin VB.TextBox txtQuantity 
            BeginProperty DataFormat 
               Type            =   1
               Format          =   "0;(0)"
               HaveTrueFalseNull=   0
               FirstDayOfWeek  =   0
               FirstWeekOfYear =   0
               LCID            =   3073
               SubFormatType   =   1
            EndProperty
            Height          =   345
            Left            =   3960
            MaxLength       =   5
            TabIndex        =   12
            Top             =   645
            Width           =   825
         End
         Begin VB.TextBox txtPrice 
            BeginProperty DataFormat 
               Type            =   1
               Format          =   "0.00;(0.00)"
               HaveTrueFalseNull=   0
               FirstDayOfWeek  =   0
               FirstWeekOfYear =   0
               LCID            =   3073
               SubFormatType   =   1
            EndProperty
            Height          =   345
            Left            =   5325
            MaxLength       =   7
            TabIndex        =   13
            Top             =   660
            Width           =   855
         End
         Begin VB.TextBox txtAmount 
            BeginProperty DataFormat 
               Type            =   1
               Format          =   "0.00;(0.00)"
               HaveTrueFalseNull=   0
               FirstDayOfWeek  =   0
               FirstWeekOfYear =   0
               LCID            =   3073
               SubFormatType   =   1
            EndProperty
            Enabled         =   0   'False
            Height          =   345
            Left            =   6855
            MaxLength       =   9
            TabIndex        =   14
            Top             =   660
            Width           =   1095
         End
         Begin MSMask.MaskEdBox txtItem 
            Bindings        =   "ProdActi.frx":3964
            Height          =   345
            Left            =   885
            TabIndex        =   11
            Top             =   660
            Width           =   2295
            _ExtentX        =   4048
            _ExtentY        =   609
            _Version        =   393216
            MaxLength       =   20
            Mask            =   "AAAAAAA/AAAA/AAAAAAA"
            PromptChar      =   " "
         End
         Begin VB.Label Label9 
            Caption         =   "At Date:"
            Height          =   255
            Left            =   3285
            TabIndex        =   34
            Top             =   300
            Width           =   600
         End
         Begin VB.Label Label8 
            Caption         =   "Add New:"
            Height          =   300
            Left            =   120
            TabIndex        =   33
            Top             =   300
            Width           =   705
         End
         Begin VB.Label Label1 
            Caption         =   "Activity:"
            Height          =   255
            Left            =   120
            TabIndex        =   30
            Top             =   1125
            Width           =   720
         End
         Begin VB.Label Label2 
            Caption         =   "Date:"
            Height          =   225
            Left            =   3285
            TabIndex        =   29
            Top             =   1140
            Width           =   540
         End
         Begin VB.Label Label3 
            Caption         =   "Item/Size:"
            Height          =   255
            Left            =   120
            TabIndex        =   28
            Top             =   735
            Width           =   720
         End
         Begin VB.Label Label4 
            Caption         =   "Quantity:"
            Height          =   315
            Left            =   3240
            TabIndex        =   27
            Top             =   720
            Width           =   645
         End
         Begin VB.Label Label5 
            Caption         =   "Price:"
            Height          =   195
            Left            =   4830
            TabIndex        =   26
            Top             =   720
            Width           =   435
         End
         Begin VB.Label Label6 
            Caption         =   "Amount:"
            Height          =   195
            Left            =   6255
            TabIndex        =   25
            Top             =   750
            Width           =   645
         End
      End
      Begin VB.Frame frabox 
         Height          =   1470
         Left            =   8280
         TabIndex        =   23
         Top             =   3480
         Width           =   1365
         Begin VB.CommandButton cmdReSend 
            Caption         =   "Rese&nd"
            Height          =   390
            Left            =   150
            TabIndex        =   35
            Top             =   1005
            Width           =   1110
         End
         Begin VB.CommandButton cmdNew 
            Caption         =   "&New"
            Height          =   390
            Left            =   150
            TabIndex        =   17
            Top             =   165
            Width           =   1110
         End
         Begin VB.CommandButton cmdRemove 
            Caption         =   "Remo&ve"
            Height          =   390
            Left            =   165
            TabIndex        =   18
            Top             =   585
            Width           =   1110
         End
      End
      Begin VB.Frame Frame3 
         Caption         =   "Show Filter"
         Height          =   600
         Left            =   2595
         TabIndex        =   22
         Top             =   225
         Width           =   3780
         Begin VB.TextBox txtShowDate 
            BeginProperty DataFormat 
               Type            =   1
               Format          =   "dd/MM/yyyy"
               HaveTrueFalseNull=   0
               FirstDayOfWeek  =   0
               FirstWeekOfYear =   0
               LCID            =   3073
               SubFormatType   =   3
            EndProperty
            Height          =   345
            Left            =   2475
            TabIndex        =   5
            Top             =   165
            Width           =   1215
         End
         Begin VB.OptionButton optNotSent 
            Caption         =   "Not Sent"
            Height          =   195
            Left            =   60
            TabIndex        =   3
            Top             =   255
            Width           =   945
         End
         Begin VB.OptionButton optAll 
            Caption         =   "Show All"
            Height          =   195
            Left            =   1050
            TabIndex        =   4
            Top             =   270
            Width           =   975
         End
         Begin VB.Label Label7 
            Caption         =   "Date:"
            Height          =   180
            Left            =   2025
            TabIndex        =   32
            Top             =   255
            Width           =   420
         End
      End
      Begin VB.Frame Frame6 
         Caption         =   "Sort By"
         Height          =   600
         Left            =   90
         TabIndex        =   21
         Top             =   225
         Width           =   2475
         Begin VB.OptionButton optSortBYAct 
            Caption         =   "&Activity"
            Height          =   195
            Left            =   90
            TabIndex        =   0
            Top             =   255
            Width           =   870
         End
         Begin VB.OptionButton optSortByDate 
            Caption         =   "& Date"
            Height          =   195
            Left            =   960
            TabIndex        =   1
            Top             =   255
            Width           =   735
         End
         Begin VB.OptionButton optSortByItem 
            Caption         =   "&Item"
            Height          =   195
            Left            =   1695
            TabIndex        =   2
            Top             =   255
            Width           =   690
         End
      End
      Begin MSDataGridLib.DataGrid grdProductActivities 
         Height          =   2580
         Left            =   75
         TabIndex        =   8
         Top             =   900
         Width           =   9570
         _ExtentX        =   16880
         _ExtentY        =   4551
         _Version        =   393216
         AllowUpdate     =   0   'False
         HeadLines       =   1
         RowHeight       =   15
         BeginProperty HeadFont {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   178
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   178
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Caption         =   "Product Activities"
         ColumnCount     =   2
         BeginProperty Column00 
            DataField       =   ""
            Caption         =   ""
            BeginProperty DataFormat {6D835690-900B-11D0-9484-00A0C91110ED} 
               Type            =   0
               Format          =   ""
               HaveTrueFalseNull=   0
               FirstDayOfWeek  =   0
               FirstWeekOfYear =   0
               LCID            =   3073
               SubFormatType   =   0
            EndProperty
         EndProperty
         BeginProperty Column01 
            DataField       =   ""
            Caption         =   ""
            BeginProperty DataFormat {6D835690-900B-11D0-9484-00A0C91110ED} 
               Type            =   0
               Format          =   ""
               HaveTrueFalseNull=   0
               FirstDayOfWeek  =   0
               FirstWeekOfYear =   0
               LCID            =   3073
               SubFormatType   =   0
            EndProperty
         EndProperty
         SplitCount      =   1
         BeginProperty Split0 
            BeginProperty Column00 
            EndProperty
            BeginProperty Column01 
            EndProperty
         EndProperty
      End
   End
End
Attribute VB_Name = "frmPrdAct"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private objActivityData As New GetData
Private objActivityRs As New ADODB.Recordset
Private objActTypes As New GetData
Private objActTypesRs As New ADODB.Recordset
Private objcoltypes As New Collection
Private DataPath As String
Private strItemMask As String
Private strFilter As String
Private strSelect As String
Private strOrderBy As String
Private strGroupBy As String
Private strStoreName As String
Private strOldSHowDate As String
Private bolNewMode As Boolean
Private newSequence As Integer
Private objColDates As New Collection
Property Let newmode(ByVal mbolNewMode As Boolean)
bolNewMode = mbolNewMode
End Property
Property Get newmode() As Boolean
newmode = bolNewMode
End Property
Private Sub cmdClose_Click()
Unload Me
End Sub
Private Sub cboSummary_Click()
If DataPath <> "" Then
  Call cmdRefresh_Click
End If
End Sub
Private Sub cmdNew_Click()
  If datesent(txtNewDate) Then
    MsgBox "Date " & txtNewDate & " already sent. Cannot modify", vbCritical + vbOKOnly, "POS"
    Exit Sub
  End If
  Me.newmode = True
  Me.txtItem = Replace(strItemMask, "#", " ")
  Me.txtItem.SelStart = 0
  Me.txtQuantity = ""
  Me.txtQuantity.Enabled = True
  Me.txtPrice = ""
  Me.txtAmount = ""
  Me.txtActivity = ""
  Me.txtDate = ""
  Me.txtItem.Enabled = True
  Me.txtItem.SetFocus
End Sub
Private Sub cmdRefresh_Click()
Dim strType As String

strFilter = "TMACTTYP.CTYPE = TMACTIVI.CTYPE"
If optNotSent Then
  strFilter = strFilter & " AND NOT LSENT"
End If
If Trim$(txtShowDate) <> "" Then
  strFilter = strFilter & " AND DTOS(DDATE)='" & _
              Year(txtShowDate) & Format(Month(txtShowDate), "00") & _
              Format(Day(txtShowDate), "00") & "'"
End If
strOrderBy = ""
strGroupBy = ""
Select Case cboSummary.ListIndex
Case 0     ' Details
  If optSortBYAct Then  ' Sort by Activity
    strOrderBy = "TMACTIVI.cType,dDate,cItem,cSize"
    strSelect = "TMACTTYP.cTYpeDesc + '' AS cActiDesc,dDate,cItem,cSize,nQuantity,nPrice,"
    strSelect = strSelect & "nAmount,IIF(EMPTY(dSentDate),'  /  /    ',PADL(DAY(dSentDate),2,'00')+'/'+PADL(MONTH(dSentDate),2,'00')+'/'+STR(YEAR(dSentDate),4)) AS cSentDate,IIF(lSent,'','*') AS cSent,"
    strSelect = strSelect & "dSentDate,TMACTIVI.cEdiType,TMACTIVI.cType,lSent,nlineno"
    strType = "1"
  End If
  If optSortByDate Then  ' Sort by Date
    strOrderBy = "dDate,TMACTIVI.cType,cItem,cSize"
    strSelect = "dDate,TMACTTYP.cTYpeDesc + '' AS cActiDesc,cItem,cSize,nQuantity,nPrice,"
    strSelect = strSelect & "nAmount,IIF(EMPTY(dSentDate),'  /  /    ',PADL(DAY(dSentDate),2,'00')+'/'+PADL(MONTH(dSentDate),2,'00')+'/'+STR(YEAR(dSentDate),4)) AS cSentDate,"
    strSelect = strSelect & "IIF(lSent,'','*') AS cSent,dSentDate,TMACTIVI.cEdiType,TMACTIVI.cType,lSent,nlineno"
    strType = "2"
  End If
  If optSortByItem Then  ' Sort by Item
    strOrderBy = "cItem,cSize,TMACTIVI.cType,dDate"
    strSelect = "cItem,cSize,TMACTTYP.cTYpeDesc + '' AS cActiDesc,dDate,nQuantity,nPrice,"
    strSelect = strSelect & "nAmount,IIF(EMPTY(dSentDate),'  /  /    ',PADL(DAY(dSentDate),2,'00')+'/'+PADL(MONTH(dSentDate),2,'00')+'/'+STR(YEAR(dSentDate),4)) AS cSentDate,"
    strSelect = strSelect & "IIF(lSent,'','*') AS cSent,dSentDate,TMACTIVI.cEdiType,TMACTIVI.cType,lSent,nlineno"
    strType = "3"
  End If
Case 1    ' Sumarize by Activity
  optSortBYAct = True
  strOrderBy = "TMACTIVI.cType"
  strSelect = "TMACTTYP.cTYpeDesc + '' AS cActiDesc,SUM(nQuantity) AS nQuantity,SUM(nAmount) AS nAmount,TMACTIVI.cType"
  strType = "4"
  strGroupBy = "TMACTIVI.cType"
Case 2    ' Sumarize by Activity Date
  If optSortByItem Then
    optSortBYAct = True
  End If
  If optSortBYAct Then
    strOrderBy = "TMACTIVI.cType,dDate"
    strSelect = "TMACTTYP.cTYpeDesc + '' AS cActiDesc,dDate,SUM(nQuantity) AS nQuantity,SUM(nAmount) AS nAmount,TMACTIVI.cType"
    strType = "5"
    strGroupBy = "TMACTIVI.cType,dDate"
  End If
  If optSortByDate Then
    strOrderBy = "dDate,TMACTIVI.cType"
    strSelect = "dDate,TMACTTYP.cTYpeDesc + '' AS cActiDesc,SUM(nQuantity) AS nQuantity,SUM(nAmount) AS nAmount,TMACTIVI.cType"
    strType = "6"
    strGroupBy = "dDate,TMACTIVI.cType"
  End If
Case 3   ' Summarize by Activity Item
  If optSortByDate Then
    optSortBYAct = True
  End If
  If optSortBYAct Then
    strOrderBy = "TMACTIVI.cType,cItem,cSize"
    strSelect = "TMACTTYP.cTYpeDesc + '' AS cActiDesc,cItem,cSize,SUM(nQuantity) AS nQuantity,SUM(nAmount) AS nAmount,TMACTIVI.cType"
    strType = "7"
    strGroupBy = "TMACTIVI.cType,cItem,cSize"
  End If
  If optSortByItem Then
    strOrderBy = "cItem,cSIze,TMACTIVI.cType"
    strSelect = "cItem,cSIze,TMACTTYP.cTYpeDesc + '' AS cActiDesc,SUM(nQuantity) AS nQuantity,SUM(nAmount) AS nAmount,TMACTIVI.cType"
    strType = "8"
    strGroupBy = "cItem,cSIze,TMACTIVI.cType"
  End If

Case 4 ' Summarized by Activity, Date, Item
  If optSortBYAct Then
    strOrderBy = "TMACTIVI.cType,dDate,cItem,cSize"
    strSelect = "TMACTTYP.cTYpeDesc + '' AS cActiDesc,dDate,cItem,cSize,SUM(nQuantity) AS nQuantity,SUM(nAmount) AS nAmount,TMACTIVI.cType"
    strType = "1"
    strGroupBy = "TMACTIVI.cType,dDate,cItem,cSize"
  End If
  If optSortByDate Then
    strOrderBy = "dDate,TMACTIVI.cType,cItem,cSize"
    strSelect = "dDate,TMACTTYP.cTYpeDesc + '' AS cActiDesc,cItem,cSize,SUM(nQuantity) AS nQuantity ,SUM(nAmount) AS nAmount,TMACTIVI.cType"
    strType = "2"
    strGroupBy = "dDate,TMACTIVI.cType,cItem,cSize"
  End If
  If optSortByItem Then
    strOrderBy = "cItem,cSize,TMACTIVI.cType,dDate"
    strSelect = "cItem,cSize,TMACTTYP.cTYpeDesc + '' AS cActiDesc,dDate,SUM(nQuantity) AS nquantity ,SUM(nAmount) AS namount,TMACTIVI.cType"
    strType = "3"
    strGroupBy = "cItem,cSize,dDate,TMACTIVI.cType"
  End If
End Select
If cboSummary.ListIndex = 0 Then
  Me.grdProductActivities.Height = 2580
  Me.fraActivity.Visible = True
  Me.frabox.Visible = True
Else
  Me.grdProductActivities.Height = 4065
  Me.fraActivity.Visible = False
  Me.frabox.Visible = False
End If
Call CollectDate(strSelect, strFilter, strOrderBy, strGroupBy)
Call SetGrid(strType)
Call grdProductActivities_RowColChange("1", "1")
End Sub

Private Sub cmdRemove_Click()
If MsgBox("Are you sure you want to remove this activity?", vbCritical + vbOKCancel) = vbCancel Then
    Exit Sub
End If
objActTypesRs.MoveFirst
Do While Not objActTypesRs.EOF
  If objActTypesRs!cType = objActivityRs!cType Then
    objActTypesRs!nUsedLine = objActTypesRs!nUsedLine - 1
    objActTypesRs.Update
    Exit Do
  End If
  objActTypesRs.MoveNext
Loop
objActivityRs.Delete
objActivityRs.Move 0
End Sub

Private Sub cmdReSend_Click()
objActivityRs.Update "lSent", False
objActivityRs!cSent = "*"
End Sub
Private Sub Form_Load()

strOldSHowDate = ""
cboSummary.ListIndex = 0
optSortBYAct.Value = True
Me.optNotSent = True
DataPath = getDataPath()
Call CollectTypes
Call sentDates
Me.cboActivity.ListIndex = 2
Me.txtNewDate = Str(Date)
Me.txtItem.Mask = Replace(strItemMask, "#", "A")
Call cmdRefresh_Click
End Sub

Private Sub CollectDate(ByVal strSelect As String, ByVal strFilter As String, _
                        ByVal strOrderBy As String, ByVal strGroupBy As String)
                       
objActivityData.filename = "TMACTIVI,TMACTTYP"
objActivityData.sqlSelect.clear
objActivityData.sqlSelect.add strSelect
objActivityData.SqlWhere.clear
objActivityData.SqlWhere.add strFilter
objActivityData.sqlGroupBy.clear
objActivityData.sqlGroupBy.add strGroupBy
objActivityData.sqlOrderBy.clear
objActivityData.sqlOrderBy.add strOrderBy
Set objActivityRs = objActivityData.retreivedata(DataPath, adOpenKeyset, adLockOptimistic)
Set Me.grdProductActivities.DataSource = objActivityRs
End Sub

Private Sub SetGrid(ByVal strType As String)
Me.grdProductActivities.ClearFields
Select Case strType
Case "1"
 
  Me.grdProductActivities.Columns(0).Caption = "Activity"
  Me.grdProductActivities.Columns(0).Width = 2000
  Me.grdProductActivities.Columns(1).Caption = "Date"
  Me.grdProductActivities.Columns(1).Width = 1000
  Me.grdProductActivities.Columns(2).Caption = "Item"
  Me.grdProductActivities.Columns(2).Width = 2100
  Me.grdProductActivities.Columns(3).Caption = "Size"
  Me.grdProductActivities.Columns(4).Caption = "Quantity"
  Me.grdProductActivities.Columns(4).Width = 800

  If Me.cboSummary.ListIndex = 0 Then
    Me.grdProductActivities.Columns(5).Caption = "Price"
    Me.grdProductActivities.Columns(5).Width = 500
    Me.grdProductActivities.Columns(6).Caption = "Amount"
    Me.grdProductActivities.Columns(6).Width = 900
    Me.grdProductActivities.Columns(7).Caption = "Sent Date"
    Me.grdProductActivities.Columns(7).Width = 1000
    Me.grdProductActivities.Columns(8).Caption = "Send"
    Me.grdProductActivities.Columns(8).Width = 450
    
    Me.grdProductActivities.Columns(9).Visible = False
    Me.grdProductActivities.Columns(10).Visible = False
    Me.grdProductActivities.Columns(11).Visible = False
    Me.grdProductActivities.Columns(12).Visible = False
    Me.grdProductActivities.Columns(13).Visible = False
  Else
    Me.grdProductActivities.Columns(5).Caption = "Amount"
    Me.grdProductActivities.Columns(5).Width = 1000
    Me.grdProductActivities.Columns(6).Visible = False
  End If
Case "2"
  Me.grdProductActivities.Columns(0).Caption = "Date"
  Me.grdProductActivities.Columns(0).Width = 1000
  Me.grdProductActivities.Columns(1).Caption = "Activity"
  Me.grdProductActivities.Columns(1).Width = 2000
  Me.grdProductActivities.Columns(2).Caption = "Item"
  Me.grdProductActivities.Columns(2).Width = 2100
  Me.grdProductActivities.Columns(3).Caption = "Size"
  Me.grdProductActivities.Columns(4).Caption = "Quantity"
  Me.grdProductActivities.Columns(4).Width = 800
  If Me.cboSummary.ListIndex = 0 Then
    Me.grdProductActivities.Columns(5).Caption = "Price"
    Me.grdProductActivities.Columns(5).Width = 500
    Me.grdProductActivities.Columns(6).Caption = "Amount"
    Me.grdProductActivities.Columns(6).Width = 900
    Me.grdProductActivities.Columns(7).Caption = "Sent Date"
    Me.grdProductActivities.Columns(7).Width = 1000
    Me.grdProductActivities.Columns(8).Caption = "Send"
    Me.grdProductActivities.Columns(8).Width = 450
    Me.grdProductActivities.Columns(9).Visible = False
    Me.grdProductActivities.Columns(10).Visible = False
    Me.grdProductActivities.Columns(11).Visible = False
    Me.grdProductActivities.Columns(12).Visible = False
    Me.grdProductActivities.Columns(13).Visible = False
  Else
    Me.grdProductActivities.Columns(5).Caption = "Amount"
    Me.grdProductActivities.Columns(5).Width = 1000
    Me.grdProductActivities.Columns(6).Visible = False
  End If
Case "3"
  Me.grdProductActivities.Columns(0).Caption = "Item"
  Me.grdProductActivities.Columns(0).Width = 2100
  Me.grdProductActivities.Columns(1).Caption = "Size"
  Me.grdProductActivities.Columns(2).Caption = "Activity"
  Me.grdProductActivities.Columns(2).Width = 2000
  Me.grdProductActivities.Columns(3).Caption = "Date"
  Me.grdProductActivities.Columns(3).Width = 1000
  Me.grdProductActivities.Columns(4).Caption = "Quantity"
  Me.grdProductActivities.Columns(4).Width = 800
  If Me.cboSummary.ListIndex = 0 Then
    Me.grdProductActivities.Columns(5).Caption = "Price"
    Me.grdProductActivities.Columns(5).Width = 500
    Me.grdProductActivities.Columns(6).Caption = "Amount"
    Me.grdProductActivities.Columns(6).Width = 900
    Me.grdProductActivities.Columns(7).Caption = "Sent Date"
    Me.grdProductActivities.Columns(7).Width = 1000
    Me.grdProductActivities.Columns(8).Caption = "Send"
    Me.grdProductActivities.Columns(8).Width = 450
    Me.grdProductActivities.Columns(9).Visible = False
    Me.grdProductActivities.Columns(10).Visible = False
    Me.grdProductActivities.Columns(11).Visible = False
    Me.grdProductActivities.Columns(12).Visible = False
    Me.grdProductActivities.Columns(13).Visible = False
  Else
    Me.grdProductActivities.Columns(5).Caption = "Amount"
    Me.grdProductActivities.Columns(5).Width = 1000
    Me.grdProductActivities.Columns(6).Visible = False
  End If
Case "4"
  Me.grdProductActivities.Columns(0).Caption = "Activity"
  Me.grdProductActivities.Columns(0).Width = 2000
  Me.grdProductActivities.Columns(1).Caption = "Quantity"
  Me.grdProductActivities.Columns(1).Width = 1000
  Me.grdProductActivities.Columns(2).Caption = "Amount"
  Me.grdProductActivities.Columns(2).Width = 1000
  Me.grdProductActivities.Columns(3).Visible = False
Case "5"
  Me.grdProductActivities.Columns(0).Caption = "Activity"
  Me.grdProductActivities.Columns(0).Width = 2000
  Me.grdProductActivities.Columns(1).Caption = "Date"
  Me.grdProductActivities.Columns(1).Width = 1000
  Me.grdProductActivities.Columns(2).Caption = "Quantity"
  Me.grdProductActivities.Columns(2).Width = 1000
  Me.grdProductActivities.Columns(3).Caption = "Amount"
  Me.grdProductActivities.Columns(3).Width = 1000
  Me.grdProductActivities.Columns(4).Visible = False
Case "6"
  Me.grdProductActivities.Columns(0).Caption = "Date"
  Me.grdProductActivities.Columns(0).Width = 1000
  Me.grdProductActivities.Columns(1).Caption = "Activity"
  Me.grdProductActivities.Columns(1).Width = 2000
  Me.grdProductActivities.Columns(2).Caption = "Quantity"
  Me.grdProductActivities.Columns(2).Width = 1000
  Me.grdProductActivities.Columns(3).Caption = "Amount"
  Me.grdProductActivities.Columns(3).Width = 1000
  Me.grdProductActivities.Columns(4).Visible = False
Case "7"
  Me.grdProductActivities.Columns(0).Caption = "Activity"
  Me.grdProductActivities.Columns(0).Width = 2000
  Me.grdProductActivities.Columns(1).Caption = "Item"
  Me.grdProductActivities.Columns(1).Width = 2100
  Me.grdProductActivities.Columns(2).Caption = "Size"
  Me.grdProductActivities.Columns(3).Caption = "Quantity"
  Me.grdProductActivities.Columns(3).Width = 1000
  Me.grdProductActivities.Columns(4).Caption = "Amount"
  Me.grdProductActivities.Columns(4).Width = 1000
  Me.grdProductActivities.Columns(5).Visible = False
Case "8"
  Me.grdProductActivities.Columns(0).Caption = "Item"
  Me.grdProductActivities.Columns(0).Width = 2100
  Me.grdProductActivities.Columns(1).Caption = "Size"
  Me.grdProductActivities.Columns(2).Caption = "Activity"
  Me.grdProductActivities.Columns(2).Width = 2000
  Me.grdProductActivities.Columns(3).Caption = "Quantity"
  Me.grdProductActivities.Columns(3).Width = 1000
  Me.grdProductActivities.Columns(4).Caption = "Amount"
  Me.grdProductActivities.Columns(4).Width = 1000
  Me.grdProductActivities.Columns(5).Visible = False
End Select
End Sub

Private Function getDataPath() As String
Dim lcreturn As String

If Dir(App.Path & "\SendActi.ini") <> "SendActi.ini" Then
  frmSetting.Show (1)
End If
lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "DataDir", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
getDataPath = Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1) & "\"
lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "ItemMask", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
strItemMask = Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1)

lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "StoreName", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
strStoreName = Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1)

End Function

Private Sub grdProductActivities_RowColChange(LastRow As Variant, ByVal LastCol As Integer)
If Me.cboSummary.ListIndex <> 0 Then
  Exit Sub
End If
If objActivityRs.EOF Or objActivityRs.BOF Then
  txtItem.Text = Replace(strItemMask, "#", " ")
  txtDate = ""
  txtQuantity = ""
  txtPrice = ""
  txtAmount = ""
  txtActivity = ""
  txtItem.Enabled = False
  txtDate.Enabled = False
  txtQuantity.Enabled = False
  txtPrice.Enabled = False
  cmdRefresh.Enabled = False
  cmdRemove.Enabled = False
  cmdReSend.Enabled = False
  Exit Sub
End If
If objActivityRs!cItem = "" Then
  newmode = True
  Exit Sub
End If
cmdReSend.Enabled = objActivityRs!lSent
txtItem.Enabled = Not cmdReSend.Enabled
txtDate.Enabled = Not cmdReSend.Enabled
txtQuantity.Enabled = Not cmdReSend.Enabled
txtPrice.Enabled = Not cmdReSend.Enabled
cmdRemove.Enabled = Not cmdReSend.Enabled
cmdRefresh.Enabled = True

txtItem = objActivityRs!cItem & objActivityRs!cSize
txtDate = objActivityRs!ddate
txtQuantity = objActivityRs!nQuantity
txtPrice = objActivityRs!nPrice
txtAmount = objActivityRs!nAmount
txtActivity = objActivityRs!cActiDesc
End Sub

Private Sub mnuActType_Click()
frmActSetting.Show (1)
End Sub

Private Sub mnuComSet_Click()
frmSetting.Show (1)
End Sub

Private Sub mnuExit_Click()
Unload Me
End Sub

Private Sub mnuSend_Click()
Dim osendacti As New SendActivity
osendacti.SendProductActivity
End Sub

Private Sub optAll_Click()
Call cmdRefresh_Click
End Sub

Private Sub optNotSent_Click()
If DataPath <> "" Then
Call cmdRefresh_Click
End If
End Sub

Private Sub optSortBYAct_Click()
If DataPath <> "" Then
Call cmdRefresh_Click
End If
End Sub

Private Sub optSortByDate_Click()
Call cmdRefresh_Click
End Sub

Private Sub optSortByItem_Click()
Call cmdRefresh_Click
End Sub

Private Sub Toolbar1_ButtonClick(ByVal Button As MSComctlLib.Button)
Select Case Button.Key
Case "Setting"
  frmSetting.Show (1)
Case "Types"
  frmActSetting.Show (1)
Case "Print"
  Call PrintActivity
Case "Send"
  Dim osendacti As New SendActivity
  If osendacti.sendsetting Then
    If osendacti.SendProductActivity() Then
      Call sentDates
      If optNotSent Then
        Call cmdRefresh_Click
      End If
    End If
  End If
Case "Close"
  Unload Me
End Select
End Sub

Private Sub txtDate_Validate(Cancel As Boolean)
If Not IsDate(txtDate) Then
  MsgBox "Invalid Date"
  txtDate = CStr(objActivityRs!ddate)
  Cancel = True
  Exit Sub
End If
If datesent(txtDate) Then
  MsgBox "Date " & txtDate & " already sent. Cannot modify", vbCritical + vbOKOnly, "POS"
  txtDate = CStr(objActivityRs!ddate)
  Cancel = True
  Exit Sub
End If
If objActivityRs!ddate <> CDate(txtDate) Then
  objActivityRs.Update "dDate", CDate(txtDate)
  objActivityRs.Update "lSent", False
End If
End Sub

Private Sub txtItem_Change()
If Me.newmode Then
  If Len(Trim$(Me.txtItem.Text)) = Len(strItemMask) Then
    Me.txtQuantity.SetFocus
    Call txtItem_Validate(False)
  End If
End If
End Sub
Private Sub txtItem_KeyPress(KeyAscii As Integer)
KeyAscii = Asc(UCase(Chr(KeyAscii)))
End Sub

Private Sub txtPrice_KeyPress(KeyAscii As Integer)
If Not IsNumeric(Chr(KeyAscii)) And Chr(KeyAscii) <> "." Then
  KeyAscii = 0
End If
If Not IsNumeric(txtPrice & Chr(KeyAscii)) Then
  KeyAscii = 0
End If
If Len(Trim$(txtPrice)) = 4 Then
  If IsNumeric(txtPrice & ".") Then
    txtPrice = txtPrice & "."
    txtPrice.SelStart = 6
   End If
End If
End Sub

Private Sub txtQuantity_GotFocus()
If Me.newmode Then
  Me.txtItem.SetFocus
End If
End Sub
Private Sub txtItem_Validate(Cancel As Boolean)
  If Me.newmode Then
    If Me.txtItem.Text = Replace(strItemMask, "#", " ") Then
      Me.newmode = False
      Call grdProductActivities_RowColChange(1, 1)
      Exit Sub
    End If
    If Len(Trim$(Me.txtItem.Text)) < Len(strItemMask) Then
      MsgBox "Invalid item code"
      Cancel = True
      Exit Sub
    End If
    objActTypesRs.MoveFirst
    Do While Not objActTypesRs.EOF
      If objActTypesRs!cType = Right$(objcoltypes.item(cboActivity.ListIndex + 1), 5) Then
        objActTypesRs!nlastline = objActTypesRs!nlastline + 1
        objActTypesRs!nUsedLine = objActTypesRs!nUsedLine + 1
        objActTypesRs.Update
        Exit Do
      End If
      objActTypesRs.MoveNext
    Loop
    newSequence = objActTypesRs!nlastline
    objActivityRs.AddNew
    objActivityRs!nlineno = newSequence
    objActivityRs!cEdiType = Left$(objcoltypes.item(cboActivity.ListIndex + 1), 2)
    objActivityRs!cType = Right$(objcoltypes.item(cboActivity.ListIndex + 1), 5)
    objActivityRs!ddate = CDate(txtNewDate)
    objActivityRs!cItem = Left$(txtItem, 19)
    objActivityRs!cSize = Mid$(txtItem, 20, 1)
    objActivityRs!nQuantity = 1
    Me.txtQuantity = "1"
    objActivityRs!nPrice = 0
    objActivityRs!nAmount = 0
    objActivityRs!lSent = False
    objActivityRs!dsentdate = vbEmpty
    objActivityRs!cSentDate = "  /  /    "
    objActivityRs!cSent = "*"
    objActivityRs!cActiDesc = Trim$(cboActivity.List(cboActivity.ListIndex))
    objActivityRs.Update
    Me.txtItem.Text = Replace(strItemMask, "#", " ")
  Else
    If objActivityRs.EOF Or objActivityRs.BOF Then
      Exit Sub
    End If
   
    If Len(Me.txtItem.Text) < Len(strItemMask) Then
      MsgBox "Invalid item code"
      Me.txtItem.Text = objActivityRs!cItem & objActivityRs!cSize
      Cancel = True
      Exit Sub
    End If
    If objActivityRs!cItem & objActivityRs!cSize <> txtItem Then
      objActivityRs.Update "cItem", Left$(txtItem, 19)
      objActivityRs.Update "cSize", Mid$(txtItem, 20, 1)
      objActivityRs.Update "lSent", False
    End If
  End If
End Sub

Private Sub txtNewDate_Validate(Cancel As Boolean)
If Not IsDate(txtNewDate) Then
  MsgBox "Invalid Date"
  txtNewDate = CStr(Date)
  Cancel = True
End If
End Sub

Private Sub txtPrice_Validate(Cancel As Boolean)
If objActivityRs.EOF Or objActivityRs.BOF Then
Else
  If objActivityRs!nPrice <> Val(txtPrice) Then
    objActivityRs.Update "nPrice", Val(txtPrice)
    objActivityRs.Update "nAmount", Val(txtPrice) * objActivityRs!nQuantity
    objActivityRs.Update "lSent", False
    objActivityRs.Update "cSent", "*"
    Me.txtAmount = CStr(objActivityRs!nAmount)
  End If
End If
End Sub

Private Sub txtQuantity_KeyPress(KeyAscii As Integer)
If Not IsNumeric(Chr(KeyAscii)) Then
  KeyAscii = 0
End If
End Sub

Private Sub txtQuantity_Validate(Cancel As Boolean)
If objActivityRs.EOF Or objActivityRs.BOF Then
Else
  If objActivityRs!nQuantity <> Val(txtQuantity) Then
    objActivityRs.Update "nQuantity", Val(txtQuantity)
    objActivityRs.Update "nAmount", Val(txtQuantity) * objActivityRs!nPrice
    objActivityRs.Update "lSent", False
    objActivityRs.Update "cSent", "*"
    Me.txtAmount = CStr(objActivityRs!nAmount)
  End If
End If
End Sub

Private Sub txtShowDate_Validate(Cancel As Boolean)
If txtShowDate <> strOldSHowDate Then
  If Trim$(txtShowDate) <> "" Then
    If Not IsDate(txtShowDate) Then
      MsgBox "Invalid Date"
      Cancel = True
      Exit Sub
    End If
  End If
  strOldSHowDate = txtShowDate
  Call cmdRefresh_Click
End If
End Sub
Private Sub CollectTypes()
                       
objActTypes.filename = "TMACTTYP"
objActTypes.sqlSelect.clear
objActTypes.sqlSelect.add "*"
objActTypes.SqlWhere.clear
objActTypes.SqlWhere.add ""
Set objActTypesRs = objActTypes.retreivedata(DataPath, adOpenKeyset, adLockOptimistic)
Do While Not objActTypesRs.EOF
  Me.cboActivity.AddItem Trim$(objActTypesRs!cTypeDesc)
  objcoltypes.add objActTypesRs!cEdiType & objActTypesRs!cType
  objActTypesRs.MoveNext
Loop
End Sub

Private Sub PrintActivity()
Dim txtDesc As String
  Set rptAct.DataSource = objActivityRs
  rptAct.Caption = "Product Activities Report"
  rptAct.Sections("Details").Controls("txtDate").DataField = "DDate"
  rptAct.Sections("Details").Controls("txtItem").DataField = "cItem"
  rptAct.Sections("Details").Controls("txtSize").DataField = "cSize"
  rptAct.Sections("Details").Controls("txtPrice").DataField = "nPrice"
  rptAct.Sections("Details").Controls("txtSentDate").DataField = "cSentDate"
  txtDesc = "Details"
  Select Case cboSummary.ListIndex
  Case 1
    txtDesc = "Sumarized by: (Activity)"
    rptAct.Sections("Details").Controls("txtItem").DataField = "cActiDesc"
    rptAct.Sections("Details").Controls("txtSize").DataField = "cActiDesc"
    rptAct.Sections("Details").Controls("txtDate").DataField = "cActiDesc"
    rptAct.Sections("Details").Controls("txtPrice").DataField = "cActiDesc"
    rptAct.Sections("Details").Controls("txtSentDate").DataField = "cActiDesc"
    rptAct.Sections("PageHeader").Controls("lblPrice").Visible = False
    rptAct.Sections("PageHeader").Controls("lblSentDate").Visible = False
    rptAct.Sections("PageHeader").Controls("lblItem").Visible = False
    rptAct.Sections("PageHeader").Controls("lblSize").Visible = False
    rptAct.Sections("PageHeader").Controls("lblDate").Visible = False
    rptAct.Sections("Details").Controls("txtPrice").Visible = False
    rptAct.Sections("Details").Controls("txtSentDate").Visible = False
    rptAct.Sections("Details").Controls("txtItem").Visible = False
    rptAct.Sections("Details").Controls("txtSize").Visible = False
    rptAct.Sections("Details").Controls("txtDate").Visible = False
  
    rptAct.Sections("PageHeader").Controls("lblQuantity").Left = 2270
    rptAct.Sections("PageHeader").Controls("lblAmount").Left = 3200
    rptAct.Sections("Details").Controls("txtQuantity").Left = 2270
    rptAct.Sections("Details").Controls("txtAmount").Left = 3200
  
  Case 2
    txtDesc = "Sumarized by: (Activity/Date)"
    rptAct.Sections("Details").Controls("txtItem").DataField = "cActiDesc"
    rptAct.Sections("Details").Controls("txtSize").DataField = "cActiDesc"
    rptAct.Sections("Details").Controls("txtPrice").DataField = "cActiDesc"
    rptAct.Sections("Details").Controls("txtSentDate").DataField = "cActiDesc"
    rptAct.Sections("PageHeader").Controls("lblPrice").Visible = False
    rptAct.Sections("PageHeader").Controls("lblSentDate").Visible = False
    rptAct.Sections("PageHeader").Controls("lblItem").Visible = False
    rptAct.Sections("PageHeader").Controls("lblSize").Visible = False
    rptAct.Sections("Details").Controls("txtPrice").Visible = False
    rptAct.Sections("Details").Controls("txtSentDate").Visible = False
    rptAct.Sections("Details").Controls("txtItem").Visible = False
    rptAct.Sections("Details").Controls("txtSize").Visible = False
  
    rptAct.Sections("PageHeader").Controls("lblQuantity").Left = 3270
    rptAct.Sections("PageHeader").Controls("lblAmount").Left = 4200
    rptAct.Sections("Details").Controls("txtQuantity").Left = 3270
    rptAct.Sections("Details").Controls("txtAmount").Left = 4200
  
  Case 3
    txtDesc = "Sumarized by: (Activity/Item)"
    rptAct.Sections("Details").Controls("txtDate").DataField = "cActiDesc"
    rptAct.Sections("Details").Controls("txtPrice").DataField = "cActiDesc"
    rptAct.Sections("Details").Controls("txtSentDate").DataField = "cActiDesc"
    rptAct.Sections("PageHeader").Controls("lblPrice").Visible = False
    rptAct.Sections("PageHeader").Controls("lblSentDate").Visible = False
    rptAct.Sections("PageHeader").Controls("lblDate").Visible = False
    rptAct.Sections("Details").Controls("txtPrice").Visible = False
    rptAct.Sections("Details").Controls("txtSentDate").Visible = False
    rptAct.Sections("Details").Controls("txtDate").Visible = False
  
    rptAct.Sections("PageHeader").Controls("lblQuantity").Left = 6270
    rptAct.Sections("PageHeader").Controls("lblAmount").Left = 7200
    rptAct.Sections("Details").Controls("txtQuantity").Left = 6270
    rptAct.Sections("Details").Controls("txtAmount").Left = 7200
  
  Case 4
    txtDesc = "Sumarized by: (Activity/Date/Item)"
    rptAct.Sections("Details").Controls("txtPrice").DataField = "cActiDesc"
    rptAct.Sections("Details").Controls("txtSentDate").DataField = "cActiDesc"
    rptAct.Sections("PageHeader").Controls("lblPrice").Visible = False
    rptAct.Sections("PageHeader").Controls("lblSentDate").Visible = False
    rptAct.Sections("Details").Controls("txtPrice").Visible = False
    rptAct.Sections("Details").Controls("txtSentDate").Visible = False
  End Select
  If optSortBYAct Then
    txtDesc = "Sort by: (Activity)" & "     " & txtDesc
    rptAct.Sections("PageHeader").Controls("lblActivity").Left = 100
    rptAct.Sections("PageHeader").Controls("lblDate").Left = 2270
    rptAct.Sections("PageHeader").Controls("lblItem").Left = 3400
    rptAct.Sections("PageHeader").Controls("lblSize").Left = 5500
    rptAct.Sections("Details").Controls("txtActiDesc").Left = 100
    rptAct.Sections("Details").Controls("txtDate").Left = 2270
    rptAct.Sections("Details").Controls("txtItem").Left = 3400
    rptAct.Sections("Details").Controls("txtSize").Left = 5500
  End If
  If optSortByDate Then
    txtDesc = "Sort by: (Date)" & "     " & txtDesc
    rptAct.Sections("PageHeader").Controls("lblActivity").Left = 1270
    rptAct.Sections("PageHeader").Controls("lblDate").Left = 100
    rptAct.Sections("PageHeader").Controls("lblItem").Left = 3400
    rptAct.Sections("PageHeader").Controls("lblSize").Left = 5500
    rptAct.Sections("Details").Controls("txtActiDesc").Left = 1270
    rptAct.Sections("Details").Controls("txtDate").Left = 100
    rptAct.Sections("Details").Controls("txtItem").Left = 3400
    rptAct.Sections("Details").Controls("txtSize").Left = 5500
  End If
  If optSortByItem Then
    txtDesc = "Sort by: (Item/Size)" & "     " & txtDesc
    rptAct.Sections("PageHeader").Controls("lblActivity").Left = 4070
    rptAct.Sections("PageHeader").Controls("lblDate").Left = 2900
    rptAct.Sections("PageHeader").Controls("lblItem").Left = 100
    rptAct.Sections("PageHeader").Controls("lblSize").Left = 2130
    rptAct.Sections("Details").Controls("txtActiDesc").Left = 4070
    rptAct.Sections("Details").Controls("txtDate").Left = 2900
    rptAct.Sections("Details").Controls("txtItem").Left = 100
    rptAct.Sections("Details").Controls("txtSize").Left = 2130
  End If
  rptAct.Sections("rptHeader").Controls("lblDesc").Caption = txtDesc
  rptAct.Sections("rptHeader").Controls("lblName").Caption = strStoreName
  rptAct.Show
End Sub
Private Sub sentDates()

Dim objDatesRs As New ADODB.Recordset

Do While objColDates.count > 0
  objColDates.Remove (1)
Loop
objActivityData.filename = "TMACTIVI"
objActivityData.sqlSelect.clear
objActivityData.sqlSelect.add "DIST dDate"
objActivityData.SqlWhere.clear
objActivityData.SqlWhere.add "LSENT"
objActivityData.sqlGroupBy.clear
objActivityData.sqlOrderBy.clear
Set objDatesRs = objActivityData.retreivedata(DataPath, adOpenKeyset, adLockReadOnly)
If objDatesRs.RecordCount > 0 Then
  objDatesRs.MoveFirst
  Do While Not objDatesRs.EOF
    objColDates.add CStr(objDatesRs!ddate)
    objDatesRs.MoveNext
  Loop
End If
objDatesRs.Close
End Sub

Private Function datesent(ByVal ddate As String) As Boolean
Dim intLoop As Integer
For intLoop = 1 To objColDates.count
  If ddate = objColDates.item(intLoop) Then
    datesent = True
    Exit Function
  End If
Next intLoop
datesent = False
End Function
