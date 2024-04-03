VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "cDrawButtonParams"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public hWnd As Long
Public hDC As Long
Public FontFace As String
Public FontSize As Long
Public hFont As Long
Public left As Long
Public top As Long
Public Height As Long
Public RightToLeft As Boolean
Public MouseOverButton As Boolean
Public MouseDownButton As Boolean
Public MouseOverSplit As Boolean
Public MouseDownSplit As Boolean
Public ShowingMenu As Boolean
Public SizeStyle As Long ' see constants COMMANDBARSIZESTYLE
Public Orientation As ECommandBarOrientation
Public Enabled As Boolean ' Is the toolbar enabled
Public Size As Long
Public ToolbarSize As Long
Public ImageList As cCommandBarImageList
Public Hidden As Boolean
Public ButtonPosition As ECommandBarButtonTextPosition
