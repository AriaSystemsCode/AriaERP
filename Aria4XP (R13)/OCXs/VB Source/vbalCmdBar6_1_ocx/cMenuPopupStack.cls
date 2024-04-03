VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "cMenuPopupStack"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private m_hWnd As Long
Private m_hWndSource As Long
Private m_bShownAsPopup As Boolean
Private m_iRecursionLevel As Long

Public Sub Initialise( _
      ByVal hWnd As Long, _
      ByVal hWndSource As Long, _
      ByVal bShownAsPopup As Boolean, _
      ByVal iRecursionLevel As Long _
   )
   m_hWnd = hWnd
   m_hWndSource = hWndSource
   m_bShownAsPopup = bShownAsPopup
   m_iRecursionLevel = iRecursionLevel
End Sub

Public Property Get hWndSource() As Long
   hWndSource = m_hWndSource
End Property

Public Property Get ShownAsPopup() As Boolean
   ShownAsPopup = m_bShownAsPopup
End Property

Public Property Get hWnd() As Long
   hWnd = m_hWnd
End Property

Public Property Get RecursionLevel() As Long
   RecursionLevel = m_iRecursionLevel
End Property


