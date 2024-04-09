VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "cDisplayButtonInfo"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public Left As Long
Public Top As Long
Public Right As Long
Public Bottom As Long
Public mouseDown As Boolean
Public mouseOver As Boolean
Public MouseOverSplit As Boolean
Public MouseDownSplit As Boolean
Public menuInitiator As Boolean
Public ShowingMenu As Boolean
Public hWndMenu As Long
Public Hidden As Boolean
