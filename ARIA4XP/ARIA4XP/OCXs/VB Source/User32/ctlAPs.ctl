VERSION 5.00
Begin VB.UserControl ctlAPs 
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   ScaleHeight     =   3600
   ScaleWidth      =   4800
End
Attribute VB_Name = "ctlAPs"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit


Private Type POINTAPI
   X As Long
   Y As Long
End Type

Private Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long

Public Function GetCursorPosX() As Long
  Dim typPoint As POINTAPI
  Call GetCursorPos(typPoint)
  GetCursorPosX = typPoint.X
End Function

Public Function GetCursorPosY() As Long
  Dim typPoint As POINTAPI
  Call GetCursorPos(typPoint)
  GetCursorPosY = typPoint.Y
End Function

