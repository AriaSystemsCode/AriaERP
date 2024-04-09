VERSION 5.00
Object = "{0E9D0E41-7AB8-11D1-9400-00A0248F2EF0}#1.0#0"; "DZACTX.DLL"
Object = "{6C5FD781-9ED8-11D1-87C0-444553540000}#1.0#0"; "DZSTACTX.DLL"
Begin VB.Form ZipProgress 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Zipping files please wait…"
   ClientHeight    =   1035
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   6930
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1035
   ScaleWidth      =   6930
   StartUpPosition =   3  'Windows Default
   Begin DZSTACTXLibCtl.dzstactxctrl ctrFileProgress 
      Height          =   345
      Left            =   1695
      OleObjectBlob   =   "ZipProgress.frx":0000
      TabIndex        =   0
      Top             =   615
      Width           =   5160
   End
   Begin DZSTACTXLibCtl.dzstactxctrl ctrTotalProgress 
      Height          =   345
      Left            =   1695
      OleObjectBlob   =   "ZipProgress.frx":0078
      TabIndex        =   3
      Top             =   90
      Width           =   5040
   End
   Begin DZACTXLibCtl.dzactxctrl zipZipControl 
      Left            =   360
      OleObjectBlob   =   "ZipProgress.frx":00F0
      Top             =   240
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      Caption         =   "Total progress"
      Height          =   195
      Left            =   90
      TabIndex        =   2
      Top             =   690
      Width           =   1005
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Current file progress"
      Height          =   195
      Left            =   90
      TabIndex        =   1
      Top             =   165
      Width           =   1395
   End
End
Attribute VB_Name = "ZipProgress"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private mstrZipFile As String
Private mcolFiles As Collection

Public Property Get Files() As Collection
  If mcolFiles Is Nothing Then
    Set mcolFiles = New Collection
  End If
  Set Files = mcolFiles
End Property

Public Property Get ZipFile() As String
  ZipFile = mstrZipFile
End Property

Public Property Let ZipFile(strZipFile As String)
  mstrZipFile = strZipFile
End Property

Public Sub Send()
  Dim strFile As Variant
  Dim strFiles As Variant
  
  Me.Show (0)
  zipZipControl.MajorStatusFlag = True
  zipZipControl.MinorStatusFlag = True
  
  zipZipControl.ZipFile = mstrZipFile
  
  strFiles = ""
  For Each strFile In mcolFiles
    strFiles = strFiles & IIf(Len(Trim(strFiles)) = 0, "", " ") & """" & Trim(strFile) & """"
  Next strFile
  If Len(Trim(strFiles)) = 0 Then
    Unload Me
  Else
    zipZipControl.ItemList = strFiles
  End If
  
  zipZipControl.ActionDZ = 4
End Sub

Private Sub zipZipControl_ZipMajorStatus(ItemName As String, Percent As Long, Cancel As Long)
  If (ctrTotalProgress.StatusText <> ItemName) Then
    ctrTotalProgress.StatusText = ItemName
  End If
  If (ctrTotalProgress.StatusPercent <> Percent) Then
    ctrTotalProgress.StatusPercent = Percent
  End If
  
  If Percent >= 100 Then
    Unload Me
     
  End If
End Sub

Private Sub zipZipControl_ZipMinorStatus(ItemName As String, Percent As Long, Cancel As Long)
  If (ctrFileProgress.StatusText <> ItemName) Then
    ctrFileProgress.StatusText = ItemName
  End If
    
  If (ctrFileProgress.StatusPercent <> Percent) Then
    ctrFileProgress.StatusPercent = Percent
  End If
End Sub
