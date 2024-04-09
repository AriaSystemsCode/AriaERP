Attribute VB_Name = "modTimer"
Option Explicit

' Timer APIs:
Private Declare Function SetTimer Lib "user32" _
        (ByVal hWnd As Long, _
         ByVal nIDEvent As Long, _
         ByVal uElapse As Long, _
         ByVal lpTimerFunc As Long) As Long

Private Declare Function KillTimer Lib "user32" _
        (ByVal hWnd As Long, _
         ByVal nIDEvent As Long) As Long

Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" _
        (pDest As Any, _
         pSource As Any, _
         ByVal ByteLen As Long)

' A list of pointers to timer objects. The list uses timer IDs as the keys.
Public gcTimerObjects As SortedList

' The timer code:
Private Sub TimerProc(ByVal lHwnd As Long, _
                      ByVal lMsg As Long, _
                      ByVal lTimerID As Long, _
                      ByVal lTime As Long)
  Dim nPtr As Long
  Dim oTimerObject As objTimer
  
  ' Create a Timer object from the pointer
  nPtr = gcTimerObjects.ItemByKey(lTimerID)
  Call CopyMemory(oTimerObject, nPtr, 4)
  
  ' Call a method which will fire the Timer event
  Call oTimerObject.Tick
  
  ' Get rid of the Timer object so that VB will not try to release it
  Call CopyMemory(oTimerObject, 0&, 4)
End Sub

Public Function StartTimer(lInterval As Long) As Long
  StartTimer = SetTimer(0, 0, lInterval, AddressOf TimerProc)
End Function

Public Sub StopTimer(lTimerID As Long)
  Call KillTimer(0, lTimerID)
End Sub

Public Sub SetInterval(lInterval As Long, lTimerID As Long)
  Call SetTimer(0, lTimerID, lInterval, AddressOf TimerProc)
End Sub
