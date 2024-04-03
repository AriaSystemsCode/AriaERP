Attribute VB_Name = "Module1"
Option Explicit

Public defWindowProc As Long
Public MinX As Long
Public MinY As Long
Public MaxX As Long
Public MaxY As Long
Public lHwnd As Long
Public objFE As FormExtender

Public Const GWL_WNDPROC As Long = (-4)
Public Const WM_GETMINMAXINFO As Long = &H24
Public Const WM_ACTIVATEAPP As Long = &H1C
Public Const WM_DISPLAYCHANGE As Long = &H7E
Public Const WA_INACTIVE As Long = 0
Public Const WA_ACTIVE As Long = 1
Public Const WM_SIZE = &H5


Public Type POINTAPI
X As Long
Y As Long
End Type

Type MINMAXINFO
ptReserved As POINTAPI
ptMaxSize As POINTAPI
ptMaxPosition As POINTAPI
ptMinTrackSize As POINTAPI
ptMaxTrackSize As POINTAPI
End Type

Public Declare Function SetWindowLong Lib "user32" _
Alias "SetWindowLongA" _
(ByVal hwnd As Long, _
ByVal nIndex As Long, _
ByVal dwNewLong As Long) As Long

Public Declare Function CallWindowProc Lib "user32" _
Alias "CallWindowProcA" _
(ByVal lpPrevWndFunc As Long, _
ByVal hwnd As Long, _
ByVal uMsg As Long, _
ByVal wParam As Long, _
ByVal lParam As Long) As Long

Public Declare Sub CopyMemory Lib "kernel32" _
Alias "RtlMoveMemory" _
(hpvDest As Any, _
hpvSource As Any, _
ByVal cbCopy As Long)


Public Sub SubClass(hwnd As Long)

    'assign our own window message
    'procedure (WindowProc)
    On Error Resume Next
    lHwnd = hwnd
    defWindowProc = SetWindowLong(hwnd, GWL_WNDPROC, AddressOf WindowProc)

End Sub


Public Sub UnSubClass(hwnd As Long)

    'restore the default message handling
    'before exiting
    If defWindowProc Then
        SetWindowLong hwnd, GWL_WNDPROC, defWindowProc
        defWindowProc = 0
    End If

End Sub


Public Function WindowProc(ByVal hwnd As Long, _
ByVal uMsg As Long, _
ByVal wParam As Long, _
ByVal lParam As Long) As Long


    'our window message procedure

    'On Error Resume Next

    Select Case hwnd

    'If the handle returned is to our form,
    'perform form-specific message handling
    'to deal with the notifications. If it
    'is a general system message, pass it
    'on to the default window procedure.
    Case lHwnd

        'On Error Resume Next

        'our form-specific handler
        Select Case uMsg
           
            Case WM_SIZE
               'WindowProc = 0
               WindowProc = CallWindowProc(defWindowProc, hwnd, uMsg, wParam, lParam)
               objFE.AppResize
            Case WM_GETMINMAXINFO

                Dim MMI As MINMAXINFO

                CopyMemory MMI, ByVal lParam, LenB(MMI)

                'set the minmaxinfo data to the
                'minimum and maximum values set
                'by the option choice
                With MMI
                    .ptMinTrackSize.X = MinX
                    .ptMinTrackSize.Y = MinY
                    .ptMaxTrackSize.X = MaxX
                    .ptMaxTrackSize.Y = MaxY
                End With

                CopyMemory ByVal lParam, MMI, LenB(MMI)

                'the MSDN tells us that if we process
                'the message, to return 0
                WindowProc = 0
           Case WM_DISPLAYCHANGE
               '    WindowProc = CallWindowProc(defWindowProc, hwnd, uMsg, wParam, lParam)
                   objFE.DisplayChanged

                Case WM_ACTIVATEAPP


                    'the MSDN tells us that if we process
                    'the message, to return 0
                    WindowProc = 0

                    Select Case LoWord(wParam)
                        Case WA_INACTIVE
                            'raise the event in the FormExtender control
                            objFE.FormDeActivated
                        Case WA_ACTIVE:
                            'raise the event in the FormExtender control
                            objFE.FormActivated
                    End Select
                Case Else

                    'this takes care of all the other messages
                    'coming to the form and not specifically
                    'handled above.
                    WindowProc = CallWindowProc(defWindowProc, hwnd, uMsg, wParam, lParam)

            End Select

        End Select

End Function

Public Function LoWord(ByVal dw As Long) As Long

    If dw And &H8000& Then
        LoWord = &H8000 Or (dw And &H7FFF&)
    Else
        LoWord = dw And &HFFFF&
    End If

End Function


