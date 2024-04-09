VERSION 5.00
Object = "{C4847593-972C-11D0-9567-00A0C9273C2A}#8.0#0"; "CRVIEWER.DLL"
Begin VB.Form frmShowRep 
   Caption         =   "File List"
   ClientHeight    =   7050
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   9435
   LinkTopic       =   "Form1"
   ScaleHeight     =   7050
   ScaleWidth      =   9435
   StartUpPosition =   3  'Windows Default
   WindowState     =   2  'Maximized
   Begin CRVIEWERLibCtl.CRViewer rpvFileList 
      Height          =   7005
      Left            =   45
      TabIndex        =   0
      Top             =   15
      Width           =   9345
      DisplayGroupTree=   0   'False
      DisplayToolbar  =   -1  'True
      EnableGroupTree =   0   'False
      EnableNavigationControls=   -1  'True
      EnableStopButton=   0   'False
      EnablePrintButton=   -1  'True
      EnableZoomControl=   -1  'True
      EnableCloseButton=   0   'False
      EnableProgressControl=   0   'False
      EnableSearchControl=   -1  'True
      EnableRefreshButton=   0   'False
      EnableDrillDown =   0   'False
      EnableAnimationControl=   0   'False
      EnableSelectExpertButton=   0   'False
      EnableToolbar   =   -1  'True
      DisplayBorder   =   0   'False
      DisplayTabs     =   0   'False
      DisplayBackgroundEdge=   -1  'True
      SelectionFormula=   ""
      EnablePopupMenu =   0   'False
      EnableExportButton=   0   'False
      EnableSearchExpertButton=   0   'False
      EnableHelpButton=   0   'False
   End
End
Attribute VB_Name = "frmShowRep"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private mobjReport As New rptFileList
Private mobjRecSet As ADODB.Recordset
Public Property Let ShowSelected(ByVal bolShow As Boolean)
  mobjReport.Field3.Suppress = Not bolShow
  mobjReport.Text3.Suppress = Not bolShow
End Property
Public Property Let RepTitle(ByVal strTitle As String)
  mobjReport.ReportTitle = strTitle
  Caption = strTitle
End Property
Public Property Set SourceRecordSet(ByRef objRS As ADODB.Recordset)
  Set mobjRecSet = objRS
  Call mobjReport.Database.SetDataSource(objRS, 3, 1)
End Property
Private Sub Form_Load()
  Screen.MousePointer = vbHourglass
  rpvFileList.ReportSource = mobjReport
  rpvFileList.ViewReport
  Screen.MousePointer = vbDefault
End Sub
Private Sub Form_Resize()
  rpvFileList.Top = 0
  rpvFileList.Left = 0
  rpvFileList.Height = ScaleHeight
  rpvFileList.Width = ScaleWidth
End Sub
