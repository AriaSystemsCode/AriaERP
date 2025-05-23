VERSION 5.00
Begin VB.UserControl vbalExplorerBarCtl 
   Alignable       =   -1  'True
   AutoRedraw      =   -1  'True
   ClientHeight    =   6840
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4050
   ControlContainer=   -1  'True
   ScaleHeight     =   6840
   ScaleWidth      =   4050
   ToolboxBitmap   =   "vbalExplorerBarCtl.ctx":0000
End
Attribute VB_Name = "vbalExplorerBarCtl"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

' ===========================================================================
' Name:     vbalExplorerBarCtl
' Author:   Steve McMahon (steve@vbaccelerator.com)
' Date:     10 June 2003
'
' Dependencies:
' Runtime:     SSUBTMR.DLL
' Designtime:  vbaCom.TLB, OLEGUIDS.TLB
'
' ---------------------------------------------------------------------------
' Copyright � 2003 Steve Mcon (steve@vbaccelerator.com) for
' vbAccelerator Ltd.
'
' Visit vbAccelerator - free, advanced source code for VB programmers.
'     http://vbaccelerator.com
' ---------------------------------------------------------------------------
'
' Description:
' RELEASE 1
' Implementation of XP-style explorer bar in VB.
' Best with Win98/2000 or above and > 256 colour display.
'
' 2003-07-05
'  * Fixed numerous GDI leaks under Win98/ME.
'  * Mouse wheel support for scrolling.
'  * Unicode text support under NT.
'  * Controls are now contained within the bar.
'  * Contained controls resized horizontally.
'  * Headers for non-expandable items draw correctly.
'
' FREE SOURCE CODE! - ENJOY.
' - Please report bugs to the author for incorporation into future releases
' - See licence
' ===========================================================================

' ---------------------------------------------------------------------------
' vbAccelerator Software License
' Version 1.0
' Copyright (c) 2002 vbAccelerator.com
'
' Redistribution and use in source and binary forms, with or
' without modification, are permitted provided that the following
' conditions are met:
'
' 1. Redistributions of source code must retain the above copyright
'    notice, this list of conditions and the following disclaimer.
'
' 2. Redistributions in binary form must reproduce the above copyright
'    notice, this list of conditions and the following disclaimer in
'    the documentation and/or other materials provided with the distribution.
'
' 3. The end-user documentation included with the redistribution, if any,
'    must include the following acknowledgment:
'
'  "This product includes software developed by vbAccelerator
'   (http://vbaccelerator.com/)."
'
' Alternately, this acknowledgment may appear in the software itself, if
' and wherever such third-party acknowledgments normally appear.
'
' 4. The name "vbAccelerator" must not be used to endorse or promote products
'    derived from this software without prior written permission. For written
'    permission, please contact vbAccelerator through steve@vbaccelerator.com.
'
' 5. Products derived from this software may not be called "vbAccelerator",
'    nor may "vbAccelerator" appear in their name, without prior written
'    permission of vbAccelerator.
'
' THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES,
' INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
' AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
' VBACCELERATOR OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
' INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
' BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
' USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
' THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
' (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
' THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
'
' ---------------------------------------------------------------------------


'
' Produced under the influence of
'  - Autechre - 7.30 Draft <--
'  - Julie A
'  - Budweiser Budvar
'  - Jeffery Steingarten - The Man Who Ate Anything.
'  - Makers Mark
'  - uZiq - Bilious Path
'  - Squarepusher - Do You Know Squarepusher
'  - Akufen, Venetian Snares, Junior Senior
'  - Samsung 19" TFT, Sony 52x24x52 CD
'  - BitTorrent
'  - Steady B, Tricky Tee, Mantronix
'  - Les Trois Garcons, Loungelover..
'
' Deterred by
'  - Shellstyle.dll.  What's that about?  Could it be that the theme DLL
'    and API is just a hopeless hack that needs a pile of work?
'  - Job. It was meant to be an XML job.  But yet again is actually
'    a Java job (I'm rubbish at Java.  I'm not that great at XML either
'    for that matter.)
'  - Kids stole my computer.  Brick through window, what can you do - was
'    the only computer I ever owned that worked properly too...
'

' Types required for API
Private Type POINTAPI
   x As Long
   y As Long
End Type

Private Type SIZEAPI
   cX As Long
   cY As Long
End Type

Private Type BITMAP
    bmType As Long
    bmWidth As Long
    bmHeight As Long
    bmWidthBytes As Long
    bmPlanes As Integer
    bmBitsPixel As Integer
    bmBits As Long
End Type
Private Type BLENDFUNCTION
   BlendOp As Byte
   BlendFlags As Byte
   SourceConstantAlpha As Byte
   AlphaFormat As Byte
End Type

' General Windows API
Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" ( _
    lpvDest As Any, lpvSource As Any, ByVal cbCopy As Long)
Private Declare Function SetProp Lib "user32" Alias "SetPropA" (ByVal hwnd As Long, ByVal lpString As String, ByVal hData As Long) As Long
Private Declare Function RemoveProp Lib "user32" Alias "RemovePropA" (ByVal hwnd As Long, ByVal lpString As String) As Long
Private Declare Function GetParent Lib "user32" (ByVal hwnd As Long) As Long
Private Declare Function GetClassName Lib "user32" Alias "GetClassNameA" (ByVal hwnd As Long, ByVal lpClassName As String, ByVal nMaxCount As Long) As Long
Private Declare Function GetClientRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT) As Long
Private Declare Function GetWindowRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT) As Long
Private Declare Function OffsetRect Lib "user32" (lpRect As RECT, ByVal x As Long, ByVal y As Long) As Long
Private Declare Function LoadLibraryEx Lib "kernel32" Alias "LoadLibraryExA" (ByVal lpLibFileName As String, ByVal hFile As Long, ByVal dwFlags As Long) As Long
' Missing from VB API declarations:
Private Const DONT_RESOLVE_DLL_REFERENCES = &H1&
Private Const LOAD_LIBRARY_AS_DATAFILE = &H2&
Private Const LOAD_WITH_ALTERED_SEARCH_PATH = &H8&
Private Declare Function FreeLibrary Lib "kernel32" (ByVal hLibModule As Long) As Long
Private Declare Function LoadImageLong Lib "user32" Alias "LoadImageA" (ByVal hInst As Long, ByVal lpsz As Long, ByVal uType As Long, ByVal cX As Long, ByVal cY As Long, ByVal uFlags As Long) As Long
Private Declare Function LoadImageString Lib "user32" Alias "LoadImageA" (ByVal hInst As Long, ByVal lpsz As String, ByVal uType As Long, ByVal cX As Long, ByVal cY As Long, ByVal uFlags As Long) As Long
Private Const IMAGE_BITMAP = 0
Private Const IMAGE_ICON = 1
Private Const IMAGE_CURSOR = 2
Private Const LR_DEFAULTCOLOR = &H0
Private Const LR_MONOCHROME = &H1
Private Const LR_COLOR = &H2
Private Const LR_COPYRETURNORG = &H4
Private Const LR_COPYDELETEORG = &H8
Private Const LR_LOADFROMFILE = &H10
Private Const LR_LOADTRANSPARENT = &H20
Private Const LR_DEFAULTSIZE = &H40
Private Const LR_VGACOLOR = &H80
Private Const LR_LOADMAP3DCOLORS = &H1000
Private Const LR_CREATEDIBSECTION = &H2000
Private Const LR_COPYFROMRESOURCE = &H4000
Private Const LR_SHARED = &H8000&
Private Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long
Private Declare Function ScreenToClient Lib "user32" (ByVal hwnd As Long, lpPoint As POINTAPI) As Long
Private Declare Function PtInRect Lib "user32" (lpRect As RECT, ByVal x As Long, ByVal y As Long) As Long
Private Declare Function VkKeyScan Lib "user32" Alias "VkKeyScanA" (ByVal cChar As Byte) As Integer
Private Declare Function VkKeyScanW Lib "user32" (ByVal cChar As Integer) As Integer
Private Declare Function GetVersion Lib "kernel32" () As Long
Private Declare Function GetAsyncKeyState Lib "user32" (ByVal vKey As Long) As Integer
Private Const WM_SYSCHAR& = &H106&
Private Const WM_SYSKEYDOWN& = &H104&
Private Const WM_KEYDOWN = &H100&
Private Const WM_KEYUP = &H101&
Private Const WM_SETFOCUS = &H7&
Private Const WM_SETTINGCHANGE = &H1A&

' Drawing API:
Private Declare Function CreateSolidBrush Lib "gdi32" (ByVal crColor As Long) As Long
Private Declare Function GetSysColorBrush Lib "user32" (ByVal nIndex As Long) As Long
Private Declare Function GetSysColor Lib "user32" (ByVal nIndex As Long) As Long
Private Const COLOR_GRADIENTACTIVECAPTION = 27
Private Const COLOR_GRADIENTINACTIVECAPTION = 28
Private Declare Function SetPixel Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal crColor As Long) As Long
Private Declare Function FillRect Lib "user32" (ByVal hdc As Long, lpRect As RECT, ByVal hBrush As Long) As Long
Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Private Declare Function GetObjectAPI Lib "gdi32" Alias "GetObjectA" (ByVal hObject As Long, ByVal nCount As Long, lpObject As Any) As Long
Private Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hdc As Long) As Long
Private Declare Function ReleaseDC Lib "user32" (ByVal hwnd As Long, ByVal hdc As Long) As Long
Private Declare Function GetDesktopWindow Lib "user32" () As Long
Private Declare Function GetDC Lib "user32" (ByVal hwnd As Long) As Long
Private Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long
Private Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long
Private Declare Function GetPixelAPI Lib "gdi32" Alias "GetPixel" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long) As Long
Private Declare Function CreatePen Lib "gdi32" (ByVal nPenStyle As Long, ByVal nWidth As Long, ByVal crColor As Long) As Long
Private Const PS_SOLID = 0
Private Declare Function MoveToEx Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, lpPoint As POINTAPI) As Long
Private Declare Function LineTo Lib "gdi32" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long) As Long
Private Declare Function SetBkMode Lib "gdi32" (ByVal hdc As Long, ByVal nBkMode As Long) As Long
Private Const TRANSPARENT = 1
Private Declare Function SetTextColor Lib "gdi32" (ByVal hdc As Long, ByVal crColor As Long) As Long
Private Declare Function DrawEdgeAPI Lib "user32" Alias "DrawEdge" (ByVal hdc As Long, qrc As RECT, ByVal edge As Long, ByVal grfFlags As Long) As Long
' BlendOp:
Private Const AC_SRC_OVER = &H0
' AlphaFormat:
Private Const AC_SRC_ALPHA = &H1
Private Declare Function AlphaBlend Lib "msimg32.dll" ( _
     ByVal hdcDest As Long, _
     ByVal nXOriginDest As Long, _
     ByVal nYOriginDest As Long, _
     ByVal nWidthDest As Long, _
     ByVal nHeightDest As Long, _
     ByVal hdcSrc As Long, _
     ByVal nXOriginSrc As Long, _
     ByVal nYOriginSrc As Long, _
     ByVal nWidthSrc As Long, _
     ByVal nHeightSrc As Long, _
     ByVal lBlendFunction As Long _
   ) As Long
Private Declare Function TransparentBlt Lib "msimg32.dll" ( _
   ByVal hdcDest As Long, _
   ByVal nXOriginDest As Long, _
   ByVal nYOriginDest As Long, _
   ByVal nWidthDest As Long, _
   ByVal hHeightDest As Long, _
   ByVal hdcSrc As Long, _
   ByVal nXOriginSrc As Long, _
   ByVal nYOriginSrc As Long, _
   ByVal nWidthSrc As Long, _
   ByVal nHeightSrc As Long, _
   ByVal crTransparent As Long _
   ) As Long

Private Enum DrawTextFlags
    DT_TOP = &H0
    DT_LEFT = &H0
    DT_CENTER = &H1
    DT_RIGHT = &H2
    DT_VCENTER = &H4
    DT_BOTTOM = &H8
    DT_WORDBREAK = &H10
    DT_SINGLELINE = &H20
    DT_EXPANDTABS = &H40
    DT_TABSTOP = &H80
    DT_NOCLIP = &H100
    DT_EXTERNALLEADING = &H200
    DT_CALCRECT = &H400
    DT_NOPREFIX = &H800
    DT_INTERNAL = &H1000
    DT_EDITCONTROL = &H2000
    DT_PATH_ELLIPSIS = &H4000
    DT_END_ELLIPSIS = &H8000&
    DT_MODIFYSTRING = &H10000
    DT_RTLREADING = &H20000
    DT_WORD_ELLIPSIS = &H40000
    DT_NOFULLWIDTHCHARBREAK = &H80000
    DT_HIDEPREFIX = &H100000
    DT_PREFIXONLY = &H200000
End Enum

' DrawEdge:
Private Enum DrawEdgeBorderFlags
   BDR_RAISEDOUTER = &H1
   BDR_SUNKENOUTER = &H2
   BDR_RAISEDINNER = &H4
   BDR_SUNKENINNER = &H8

   BDR_OUTER = &H3
   BDR_INNER = &HC
   BDR_RAISED = &H5
   BDR_SUNKEN = &HA

   EDGE_RAISED = (BDR_RAISEDOUTER Or BDR_RAISEDINNER)
   EDGE_SUNKEN = (BDR_SUNKENOUTER Or BDR_SUNKENINNER)
   EDGE_ETCHED = (BDR_SUNKENOUTER Or BDR_RAISEDINNER)
   EDGE_BUMP = (BDR_RAISEDOUTER Or BDR_SUNKENINNER)
End Enum

Private Enum DrawEdgeBorderPartFlags
   BF_LEFT = &H1
   BF_TOP = &H2
   BF_RIGHT = &H4
   BF_BOTTOM = &H8

   BF_TOPLEFT = (BF_TOP Or BF_LEFT)
   BF_TOPRIGHT = (BF_TOP Or BF_RIGHT)
   BF_BOTTOMLEFT = (BF_BOTTOM Or BF_LEFT)
   BF_BOTTOMRIGHT = (BF_BOTTOM Or BF_RIGHT)
   BF_RECT = (BF_LEFT Or BF_TOP Or BF_RIGHT Or BF_BOTTOM)
End Enum

' ImageList API:
Private Declare Function ImageList_GetIconSize Lib "comctl32.dll" ( _
        ByVal hIml As Long, _
        cX As Long, cY As Long _
    ) As Long
Private Declare Function ImageList_GetImageRect Lib "comctl32.dll" ( _
        ByVal hIml As Long, _
        ByVal i As Long, _
        prcImage As RECT _
    ) As Long
Private Declare Function ImageList_Draw Lib "comctl32.dll" ( _
        ByVal hIml As Long, ByVal i As Long, _
        ByVal hdcDst As Long, ByVal x As Long, ByVal y As Long, _
        ByVal fStyle As Long _
    ) As Long
' Create a new icon based on an image list icon:
Private Declare Function ImageList_GetIcon Lib "comctl32.dll" ( _
        ByVal hIml As Long, _
        ByVal i As Long, _
        ByVal diIgnore As Long _
    ) As Long
Private Declare Function ImageList_GetImageCount Lib "comctl32.dll" ( _
        ByVal hIml As Long _
    ) As Long
Private Declare Function ImageList_Create Lib "COMCTL32" (ByVal MinCx As Long, ByVal MinCy As Long, ByVal Flags As Long, ByVal cInitial As Long, ByVal cGrow As Long) As Long
Private Declare Function ImageList_AddMasked Lib "COMCTL32" (ByVal hImageList As Long, ByVal hbmImage As Long, ByVal crMask As Long) As Long
Private Declare Function ImageList_Destroy Lib "COMCTL32" (ByVal hImageList As Long) As Long
Private Const ILD_NORMAL = 0
Private Const ILD_TRANSPARENT = 1
Private Const ILD_BLEND25 = 2
Private Const ILD_SELECTED = 4
Private Const ILD_FOCUS = 4
Private Const ILD_MASK = &H10&
Private Const ILD_IMAGE = &H20&
Private Const ILD_ROP = &H40&
Private Const ILD_OVERLAYMASK = 3840
Private Const ILC_COLOR = &H0
Private Const ILC_COLOR32 = &H20
Private Const ILC_MASK = &H1&


' UXTHEME API:
Private Declare Function OpenThemeData Lib "uxtheme.dll" _
   (ByVal hwnd As Long, ByVal pszClassList As Long) As Long
Private Declare Function CloseThemeData Lib "uxtheme.dll" _
   (ByVal hTheme As Long) As Long
Private Declare Function DrawThemeBackground Lib "uxtheme.dll" _
    (ByVal hTheme As Long, _
    ByVal hdc As Long, _
    ByVal iPartId As Long, _
    ByVal iStateId As Long, _
    pRect As RECT, _
    pClipRect As RECT) As Long
Private Declare Function GetCurrentThemeName Lib "uxtheme.dll" ( _
    ByVal pszThemeFileName As Long, _
    ByVal dwMaxNameChars As Long, _
    ByVal pszColorBuff As Long, _
    ByVal cchMaxColorChars As Long, _
    ByVal pszSizeBuff As Long, _
    ByVal cchMaxSizeChars As Long _
   ) As Long
Private Declare Function GetThemeFilename Lib "uxtheme.dll" _
   (ByVal hTheme As Long, _
    ByVal iPartId As Long, _
    ByVal iStateId As Long, _
    ByVal iPropId As Long, _
    pszThemeFileName As Long, _
    ByVal cchMaxBuffChars As Long _
   ) As Long
Private Declare Function GetThemePartSize Lib "uxtheme.dll" ( _
    ByVal hTheme As Long, _
    ByVal hdc As Long, _
    ByVal iPartId As Long, _
    ByVal iStateId As Long, _
    prc As RECT, _
    ByVal eSize As Long, _
    psz As SIZEAPI _
   ) As Long
Private Declare Function DrawThemeText Lib "uxtheme.dll" _
    (ByVal hTheme As Long, _
    ByVal hdc As Long, _
    ByVal iPartId As Long, _
    ByVal iStateId As Long, _
    ByVal pszText As Long, _
    ByVal iCharCount As Long, _
    ByVal dwTextFlags As Long, _
    ByVal dwTextFlags2 As Long, _
    pRect As RECT _
   ) As Long

Private Enum EExplorerBarParts
   EBP_HEADERBACKGROUND = 1
   EBP_HEADERCLOSE
   EBP_HEADERPIN
   EBP_IEBARMENU
   EBP_NORMALGROUPBACKGROUND
   EBP_NORMALGROUPCOLLAPSE
   EBP_NORMALGROUPEXPAND
   EBP_NORMALGROUPHEAD
   EBP_SPECIALGROUPBACKGROUND
   EBP_SPECIALGROUPCOLLAPSE
   EBP_SPECIALGROUPEXPAND
   EBP_SPECIALGROUPHEAD
End Enum
Private Const TS_MIN = 0
Private Const TS_TRUE = 1
Private Const TS_DRAW = 2


' #REGION Implementation

' Enumerations
Public Enum EExplorerBarItemTypes
   eItemLink
   eItemText
   eItemControlPlaceHolder
End Enum

Public Enum EExplorerBarStyles
   eDefaultStyle
   eSearchStyle
End Enum

Public Enum EExplorerBarStates
   eBarCollapsed
   eBarExpanded
End Enum

Public Enum EExplorerBarWatermarkModes
   eWaterMarkColourise
   eWaterMarkDirect
End Enum

Public Enum EExplorerBarWatermarkHAlign
   eWaterMarkAlignLeft = 0
   eWaterMarkAlignHCentre = 1
   eWaterMarkAlignRight = 2
End Enum

Public Enum EExplorerBarWatermarkVAlign
   eWaterMarkAlignTop = 0
   eWaterMarkAlignVCentre = 1
   eWaterMarkAlignBottom = 1
End Enum

' Events:
Public Event BarRightClick(bar As cExplorerBar)
Attribute BarRightClick.VB_Description = "Raised when the user right clicks a bar."
Public Event BarClick(bar As cExplorerBar)
Attribute BarClick.VB_Description = "Raised after a bar has been clicked, and the bar's state has changed."
Public Event BeforeBarClick(bar As cExplorerBar)
Public Event ItemRightClick(itm As cExplorerBarItem)
Attribute ItemRightClick.VB_Description = "Raised when a user right clicks on an item in the control."
Public Event ItemClick(itm As cExplorerBarItem)
Attribute ItemClick.VB_Description = "Raised when an item is clicked."
Public Event Highlight(bar As cExplorerBar, itm As cExplorerBarItem)
Attribute Highlight.VB_Description = "Raised when an item or bar is highlighted by the user moving the mouse over it."
Public Event SettingChange()

' Bars is a collection of pcExplorerBars keyed on ID
' in the order the bars appear in the control:
Private m_colBars As Collection
' A collection of the Ids keyed by the item's Key:
Private m_colBarKeys As Collection

' All the items for all bars keyed by ID:
Private m_colItems As Collection

' Image List
Private m_hIml As Long
Private m_ptrVB6ImageList As Long
Private m_lIconSize As Long
' Bar Title Image List
Private m_hImlBarTitle As Long
Private m_ptrVB6ImageListBarTitle As Long
Private m_lBarTitleIconSize As Long

' Selections
Private m_lIdSelBar As Long
Private m_lIdSelItem As Long

' Explorer Style Rendering Control:
Private m_bUseExplorerTransitionStyle As Boolean
Private m_bUseExplorerTheme As Boolean
Private m_hDib(0 To 10) As Long
Private m_hDC As Long
Private m_lThemePanelColor As Long

' Non-Explorer Style Appearance:
Private m_oBackColorStart As OLE_COLOR
Private m_oBackColorEnd As OLE_COLOR
Private m_hFntTitle As Long

' General sizing and metrics:
Private m_lBarSpacing As Long
Private m_lItemSpacing As Long
Private m_lMargin As Long
Private m_cNCM As pcNCMetrics

' Helper classes:
Private WithEvents m_cScrollBar As pcScrollBars
Attribute m_cScrollBar.VB_VarHelpID = -1
Private m_cDibFade As pcAlphaDibSection
Private WithEvents m_tmr As CTimer
Attribute m_tmr.VB_VarHelpID = -1

' Control
Private m_bRunTime As Boolean
Private m_bFocus As Boolean
Private m_bShowFocusRect As Boolean
Private m_bHaveUsedKeys As Boolean
Private m_pcOver As pcExplorerBar
Private m_itmOver As pcExplorerBarItem
Private m_sToolTip As String
Private m_ePointer As MousePointerConstants
Private m_bRedraw As Boolean
Private m_eStyle As EExplorerBarStyles
Private m_lLastWidth As Long

' IOleControl support:
Private m_ptrGetControlInfoOrig As Long
Private m_ptrOnMnemonicOrig As Long
Private m_cMnemonics As pcMnemonics

' IOleInPlaceActiveObject support:
Private m_IPAOHookStruct As IPAOHookStruct
Private m_hWnd As Long
Private m_hWndContainer As Long

Implements ISubclass

Public Event Scroll()

' MAH
Private Declare Function GetDeviceCaps Lib "gdi32" (ByVal hdc As Long, ByVal nIndex As Long) As Long
' MAH

Friend Sub fContainControl(ctlThis As Control)
   On Error Resume Next
   Set ctlThis.Container = UserControl.Extender
End Sub

Friend Sub fClearBars()
On Error Resume Next
Dim pc As pcExplorerBar
Dim itm As pcExplorerBarItem
Dim iItem As Long
Dim ctl As Control

   ' Make any controls associated with this invisible:
   On Error Resume Next
   For Each pc In m_colBars
      For iItem = 1 To pc.ItemCount
         Set itm = pc.Item(iItem)
         If Not (itm.lPtrPanel = 0) Then
            Set ctl = ObjectFromPtr(itm.lPtrPanel)
            ctl.Visible = False
         End If
      Next iItem
   Next
   
   Set m_colBars = New Collection
   Set m_colBarKeys = New Collection
   Set m_colItems = New Collection
   
   
   On Error Resume Next
   m_bHaveUsedKeys = False
   
   pMeasure
   pPaint
   UserControl.Refresh
   
End Sub
Friend Sub fTextChanged( _
      ByVal sOldCaption As String, _
      ByVal sNewCaption As String _
   )
On Error Resume Next
Dim iPos As Long
Dim sToAdd As String
Dim sToRemove As String
Dim sKeys As String

   iPos = InStr(Replace(sNewCaption, "&&", ""), "&")
   If (iPos > 0) And (iPos < Len(sNewCaption)) Then
      sToAdd = UCase(Mid(sNewCaption, iPos + 1, 1))
   End If
   
   iPos = InStr(Replace(sOldCaption, "&&", ""), "&")
   If (iPos > 0) And (iPos < Len(sOldCaption)) Then
      sToRemove = UCase(Mid(sOldCaption, iPos + 1, 1))
   End If
   
   If (sToAdd = sToRemove) Then
      Exit Sub
   Else
      If Len(sToRemove) > 0 Then
         m_cMnemonics.RemoveByKey sToRemove
      End If
      m_cMnemonics.AddByKey sToAdd
      pUpdateMnemonics
   End If
   
End Sub
Private Sub pUpdateMnemonics()
On Error Resume Next
   If (UserControl.AccessKeys = "") Then
      UserControl.AccessKeys = " "
   Else
      UserControl.AccessKeys = ""
   End If
End Sub
Friend Property Get fBarIndex( _
      ByVal lId As Long _
   ) As Long
   On Error Resume Next
Dim i As Long
Dim pc As pcExplorerBar
   For i = 1 To m_colBars.Count
      Set pc = m_colBars(i)
      If (pc.ID = lId) Then
         fBarIndex = i
         Exit For
      End If
   Next i
End Property
Friend Property Let fBarIndex( _
      ByVal lId As Long, _
      ByVal lIndex As Long _
   )
   On Error Resume Next
Dim pcSwap As pcExplorerBar
Dim i As Long
Dim lIndexNow As Long
Dim colBarsTmp As New Collection
Dim colBarKeysTmp As New Collection

   If (lIndex < 0) Or (lIndex > m_colBars.Count) Then
      gErr 9, "vbalExplorerBarCtl"
   Else
      lIndexNow = fBarIndex(lId)
      If Not (lIndex = lIndexNow) Then
         If (lIndex > lIndexNow) Then
            ' Moving the bar down
            For i = 1 To m_colBars.Count
               If (i < lIndexNow) Then
                  colBarsTmp.Add m_colBars(i), "C:" & m_colBars(i).ID
                  colBarKeysTmp.Add m_colBars(i).ID, m_colBars(i).Key
               ElseIf (i < lIndex) Then
                  colBarsTmp.Add m_colBars(i + 1), "C:" & m_colBars(i + 1).ID
                  colBarKeysTmp.Add m_colBars(i + 1).ID, m_colBars(i + 1).Key
               ElseIf (i = lIndex) Then
                  colBarsTmp.Add m_colBars(lIndexNow), "C:" & m_colBars(lIndexNow).ID
                  colBarKeysTmp.Add m_colBars(lIndexNow).ID, m_colBars(lIndexNow).Key
                  Set pcSwap = m_colBars(lIndexNow)
               Else
                  colBarsTmp.Add m_colBars(i), "C:" & m_colBars(i).ID
                  colBarKeysTmp.Add m_colBars(i).ID, m_colBars(i).Key
               End If
            Next i
            Set m_colBars = colBarsTmp
            Set m_colBarKeys = colBarKeysTmp
         
         Else
            ' Moving the bar up
            For i = 1 To m_colBars.Count
               If (i < lIndex) Then
                  colBarsTmp.Add m_colBars(i), "C:" & m_colBars(i).ID
                  colBarKeysTmp.Add m_colBars(i).ID, m_colBars(i).Key
               ElseIf (i = lIndex) Then
                  colBarsTmp.Add m_colBars(lIndexNow), "C:" & m_colBars(lIndexNow).ID
                  colBarKeysTmp.Add m_colBars(lIndexNow).ID, m_colBars(lIndexNow).Key
                  Set pcSwap = m_colBars(lIndexNow)
               ElseIf (i <= lIndexNow) Then
                  colBarsTmp.Add m_colBars(i + 1), "C:" & m_colBars(i + 1).ID
                  colBarKeysTmp.Add m_colBars(i + 1).ID, m_colBars(i + 1).Key
               Else
                  colBarsTmp.Add m_colBars(i), "C:" & m_colBars(i).ID
                  colBarKeysTmp.Add m_colBars(i).ID, m_colBars(i).Key
               End If
            Next i
            Set m_colBars = colBarsTmp
            Set m_colBarKeys = colBarKeysTmp
            
         End If
         UserControl_Resize
         
         If (pcSwap.ID = m_lIdSelBar) Then
            fEnsureBarVisible pcSwap.ID
         End If
      End If
   End If
End Property
Friend Sub fBarChanged( _
      ByVal lId As Long, _
      ByVal bHeightChange As Boolean, _
      ByVal bColourChange As Boolean _
   )
   On Error Resume Next
Dim pc As pcExplorerBar
Dim itm As pcExplorerBarItem
Dim hTheme As Long
Dim tR As RECT
Dim iItem As Long
Dim lHeightOrigWith As Long
Dim lHeightOrigWithout As Long
   
   GetClientRect UserControl.hwnd, tR
   Set pc = m_colBars.Item("C:" & lId)
   
   If (bColourChange) Then
      If Not (m_bUseExplorerTheme) Then
         pbCreateBitmapWorkDC
         pbLoadShellStyleBitmaps
      End If
      pbColouriseWatermarks
   End If
   
   If (bHeightChange) Then
      fMeasureTitle lId
      
      lHeightOrigWith = pc.HeightWithScroll + pc.TitleHeightWithScroll
      lHeightOrigWithout = pc.HeightWithoutScroll + pc.TitleHeightWithoutScroll

      ' evaluate the new height of the item:
      For iItem = 1 To pc.ItemCount
         Set itm = pc.Item(iItem)
         fMeasureTitle pc.ID
         fMeasureItem pc.ID, itm.ID
      Next iItem
      pc.SetHeightFromItems
      
      ' set up a scroll to make this visible:
      If Not (lHeightOrigWith = pc.HeightWithScroll + pc.TitleHeightWithScroll) Or _
         Not (lHeightOrigWithout = pc.HeightWithoutScroll + pc.TitleHeightWithoutScroll) Then
         pMeasure
         pPaint
         UserControl.Refresh
      End If
      
   Else
      ' draw the item:
      hTheme = plGetTheme()
      pPaintBar pc, UserControl.hdc, hTheme, tR, True
      pPaintBorders UserControl.hdc, hTheme, tR
      If Not (hTheme = 0) Then
         CloseThemeData hTheme
      End If
   End If
   
End Sub
   
Friend Sub fRemoveBar( _
      Index As Variant _
   )
   On Error Resume Next
   Dim lId As Long
   lId = m_colBarKeys(Index)
   If (Err.Number = 0) Then
   
      Dim pc As pcExplorerBar
      Dim itm As pcExplorerBarItem
      Dim iItem As Long
      Dim ctl As Control
   
      Set pc = m_colBars("C:" & lId)
      For iItem = 1 To pc.ItemCount
         Set itm = pc.Item(iItem)
         If Not (itm.lPtrPanel = 0) Then
            Set ctl = ObjectFromPtr(itm.lPtrPanel)
            ctl.Visible = False
         End If
      Next iItem
      
      On Error Resume Next
      m_colBars.Remove "C:" & lId
      m_colBarKeys.Remove Index
      
      pMeasure
      pPaint
      UserControl.Refresh
      
   End If
End Sub
Friend Function fBarCount() As Long
On Error Resume Next
   fBarCount = m_colBarKeys.Count
End Function

Friend Function fGetBar( _
      Index As Variant _
   ) As cExplorerBar
   On Error Resume Next
   
   Dim lId As Long
   lId = m_colBarKeys(Index)
   If (Err.Number = 0) Then
      Dim c As New cExplorerBar
      c.fInit UserControl.hwnd, lId
      Set fGetBar = c
   End If
End Function

Friend Function fAddBar( _
      Optional Index As Variant, _
      Optional Key As Variant, _
      Optional Title As Variant _
   ) As cExplorerBar
   On Error Resume Next
   ' Verify the Index is ok:
   Dim lIndex As Long
   If Not IsMissing(Index) Then
      If (IsNumeric(Index)) Then
         On Error Resume Next
         lIndex = CLng(Index)
         If (Err.Number = 0) And (Index > 0) And (Index <= m_colBars.Count) Then
            ' ok
         Else
            gErr 9, "vbalExplorerBarCtl"
            Exit Function
         End If
      Else
         Dim i As Long
         For i = 1 To m_colBars.Count
            If (m_colBars(i).Key = Index) Then
               ' ok
               lIndex = i
               Exit For
            End If
         Next i
         If (lIndex = 0) Then
            ' Index is no good
            gErr 9, "vbalExplorerBarCtl"
            Exit Function
         End If
      End If
   End If
   
   ' Verify if the Key is ok:
   Dim sKey As String
   If Not IsMissing(Key) Then
      If IsNumeric(Key) Then
         gErr 13, "vbalExplorerBarCtl"
         Exit Function
      Else
         On Error Resume Next
         Dim lIdExisting As Long
         lIdExisting = m_colBarKeys(Key)
         If (Err.Number = 0) Then
            ' its no good
            gErr 457, "vbalExplorerBarCtl"
         Else
            sKey = Key
         End If
      End If
   End If
   
   ' Ok we're ready
   Dim lId As Long
   lId = NextId
   If (sKey = "") Then
      sKey = "C:" & lId
   End If
   
   ' Add or insert the item:
   Dim pc As New pcExplorerBar
   pc.ID = lId
   pc.Key = sKey
   If Not IsMissing(Title) Then
      pc.Title = Title
      fTextChanged "", Title
   End If
   ' First put it into m_colBars:
   If (lIndex > 0) Then
      m_colBars.Add pc, "C:" & lId, lIndex
   Else
      m_colBars.Add pc, "C:" & lId
   End If
   ' add the id to the key collection
   If (lIndex > 0) Then
      m_colBarKeys.Add lId, pc.Key, lIndex
   Else
      m_colBarKeys.Add lId, pc.Key
   End If
   
   ' Have the title measured:
   fMeasureTitle lId
   
   ' draw:
   UserControl_Resize
   
   ' Create the object:
   Dim cB As New cExplorerBar
   cB.fInit UserControl.hwnd, lId
   Set fAddBar = cB
   
End Function
Friend Function fGetBarInternal(ByVal lId As Long) As pcExplorerBar
   On Error Resume Next
   
   Set fGetBarInternal = m_colBars.Item("C:" & lId)
End Function
Friend Function fGetItemInternal(ByVal lId As Long) As pcExplorerBarItem
   On Error Resume Next
   
   Set fGetItemInternal = m_colItems.Item("C:" & lId)
End Function
Friend Sub fExpandBar(pc As pcExplorerBar, ByVal iDir As Long)
   On Error Resume Next

Dim lStart As Long
Dim lTarget As Long
Dim bScrollNow As Boolean
Dim i As Long
Dim lAlpha As Long
Dim lPos As Long
Dim iMinDir As Long
On Error Resume Next
   bScrollNow = m_cScrollBar.Visible(efsVertical)

   If (iDir > 0) Then
      pc.Expanding = True
      lStart = 0
      If (bScrollNow) Then
         lTarget = pc.HeightWithScroll
      Else
         lTarget = pc.HeightWithoutScroll
         ' TODO
         ' Need to check here whether that height will
         ' cause the scroll bar to appear.  If so,
         ' lTarget will be pc.HeightWithScroll
      End If
   Else
      pc.Collapsing = True
      lTarget = 0
      If (bScrollNow) Then
         lStart = pc.HeightWithScroll
      Else
         lStart = pc.HeightWithoutScroll
      End If
   End If
     
   ' Check if we animate or not:
   If Not (pc.ContainsControl()) Then
     
      If (pc.ItemCount > 25) Then
         iMinDir = iDir * 4
      ElseIf (pc.ItemCount > 50) Then
         iMinDir = iDir * 6
      ElseIf (pc.ItemCount > 75) Then
         iMinDir = iDir * 8
      End If
     
      lPos = lStart
      If (iDir > 0) Then
         pc.CollapseOffset = (lTarget - lPos)
      Else
         pc.CollapseOffset = 0
      End If
      
      Do While Not (lPos = lTarget)
         lPos = lPos + iDir
         If (iDir > 0) Then
            pc.CollapseOffset = pc.CollapseOffset - iDir
         Else
            pc.CollapseOffset = pc.CollapseOffset + iDir
         End If
         If (iDir > 0) Then
            If (lPos > lTarget) Then
               lPos = lTarget
               pc.CollapseOffset = 0
            End If
         Else
            If (lPos < lTarget) Then
               lPos = lTarget
               pc.CollapseOffset = 0
            End If
         End If
         pc.Height = lPos
               
         lAlpha = 255 * lPos / Abs(lStart - lTarget)
         pc.Alpha = lAlpha
         
         pMeasure
         pPaint
         pResizeContainedControls
         UserControl.Refresh
         
         If (Abs(iDir) < 32) Then
            iDir = iDir + Sgn(iDir)
         Else
            iDir = iDir + iMinDir
         End If
         
      Loop
   
   Else
      pc.Height = lTarget
      pc.Alpha = 255
   
   End If
   
   If (iDir > 0) Then
      pc.State = eBarExpanded
   Else
      pc.State = eBarCollapsed
   End If
   pc.Expanding = False
   pc.Collapsing = False
   pc.Alpha = 255
   
   fEnsureBarVisible pc.ID
   
   pMeasure
   pPaint
   pResizeContainedControls
   UserControl.Refresh
   
End Sub
Friend Sub fEnsureItemVisible(ByVal lBarId As Long, ByVal lItemId As Long)
On Error Resume Next
Dim pc As pcExplorerBar
Dim itm As pcExplorerBarItem
Dim lTop As Long
Dim iDir As Long
Dim lNow As Long
Dim lTarget As Long
Dim i As Long
Dim bComplete As Boolean
Dim tR As RECT

   ' First check if the bar which contains this item is expanded:
   Set pc = m_colBars.Item("C:" & lBarId)
   If (pc.CanExpand) Then
      If (pc.State = eBarCollapsed) Then
         fExpandBar pc, 1
      End If
   End If

   If (m_cScrollBar.Visible(efsVertical)) Then
   
      GetClientRect UserControl.hwnd, tR
      
      ' Now find out where the item is in the display and check
      ' if it can be seen:
      lTop = pc.Top - m_cScrollBar.Value(efsVertical)
      lTop = lTop + pc.TitleHeightWithScroll
      For i = 1 To pc.ItemCount
         Set itm = pc.Item(i)
         If (itm.ID = lItemId) Then
            Exit For
         End If
         lTop = lTop + itm.HeightWithScroll + itm.SpacingAfter
      Next i
            
      ' Is the thing actually off screen?
      If (lTop < tR.Top) Or (lTop + itm.HeightWithScroll > tR.bottom) Then
         
         lNow = m_cScrollBar.Value(efsVertical)
         lTarget = lNow + lTop
         
         Debug.Print "Item Is Off Screen", lTarget, lNow
         
         iDir = IIf(lTarget < lNow, -1, 1)
         Do While Not bComplete
            lNow = lNow + iDir
            If (iDir > 0) Then
               If (lNow > lTarget) Then
                  lNow = lTarget
                  bComplete = True
               End If
            Else
               If (lNow < lTarget) Then
                  lNow = lTarget
                  bComplete = True
               End If
            End If
            m_cScrollBar.Value(efsVertical) = lNow
            iDir = iDir + Sgn(iDir)
         Loop
         
         
      End If
   
   End If
   
End Sub

Friend Sub fEnsureBarVisible(ByVal lId As Long)
On Error Resume Next
Dim pc As pcExplorerBar
Dim lTop As Long
Dim lHeight As Long
Dim tR As RECT
Dim lNow As Long
Dim lTarget As Long
Dim iDir As Long
Dim bComplete As Boolean

   If (m_cScrollBar.Visible(efsVertical)) Then
      
      GetClientRect UserControl.hwnd, tR
      Set pc = m_colBars.Item("C:" & lId)
      lTop = pc.Top - m_cScrollBar.Value(efsVertical)
      lHeight = pc.HeightWithScroll + pc.TitleHeightWithScroll + m_lBarSpacing / 2
      
      ' Is the thing actually off screen?
      If (lTop < tR.Top) Or (lTop + lHeight > tR.bottom) Then
      
         lNow = m_cScrollBar.Value(efsVertical)
         If (lHeight > tR.bottom - tR.Top) Then
            ' Best we can do is to ensure the top is
            ' visible:
            lTarget = lNow + lTop
         Else
            ' We can show the entire item:
            lTarget = (pc.Top + lHeight) - (tR.bottom - tR.Top)
         End If
                 
         
         iDir = IIf(lTarget < lNow, -1, 1)
         Do While Not bComplete
            lNow = lNow + iDir
            If (iDir > 0) Then
               If (lNow > lTarget) Then
                  lNow = lTarget
                  bComplete = True
               End If
            Else
               If (lNow < lTarget) Then
                  lNow = lTarget
                  bComplete = True
               End If
            End If
            m_cScrollBar.Value(efsVertical) = lNow
            iDir = iDir + Sgn(iDir)
         Loop
         
         
      End If
   End If
   
End Sub
Friend Function fAddItem( _
      ByVal lBarId As Long, _
      Optional Index As Variant, _
      Optional Key As Variant, _
      Optional Text As Variant, _
      Optional IconIndex As Variant, _
      Optional ItemType As Variant _
   ) As cExplorerBarItem
   On Error Resume Next
   
   ' Get bar to add to:
   Dim pc As pcExplorerBar
   Set pc = m_colBars.Item("C:" & lBarId)
   
   ' Verify the Index is ok:
   Dim lIndex As Long
   If Not IsMissing(Index) Then
      If (IsNumeric(Index)) Then
         On Error Resume Next
         lIndex = CLng(Index)
         If (Err.Number = 0) And (Index > 0) And (Index <= pc.ItemCount) Then
            ' ok
         Else
            gErr 9, "vbalExplorerBarCtl"
            Exit Function
         End If
      Else
         On Error Resume Next
         lIndex = pc.ItemIndex(Index)
         If (Err.Number = 0) Then
            ' ok
         Else
            ' Index is no good
            gErr 9, "vbalExplorerBarCtl"
            Exit Function
         End If
      End If
   End If
   
   ' Check if the specified key is ok:
   Dim sKey As String
   If Not IsMissing(Key) Then
      If IsNumeric(Key) Then
         gErr 13, "vbalExplorerBarCtl"
         Exit Function
      Else
         On Error Resume Next
         Dim lIdExisting As Long
         Dim pcBar As pcExplorerBar
         For Each pcBar In m_colBars
            lIdExisting = pcBar.IDForKey(Key)
            If (Err.Number = 0) Then
               ' its no good
               gErr 457, "vbalExplorerBarCtl"
               Exit Function
            Else
               sKey = Key
            End If
         Next
      End If
   End If
   
   ' Ok we're ready:
   Dim lId As Long
   lId = NextId
   If (sKey = "") Then
      sKey = "C:" & lId
   End If
   
   ' Add or insert the item:
   Dim itm As New pcExplorerBarItem
   itm.ID = lId
   itm.Key = sKey
   If Not IsMissing(Text) Then
      itm.Text = Text
      fTextChanged "", Text
   End If
   If Not IsMissing(IconIndex) Then
      itm.IconIndex = IconIndex
   End If
   If Not IsMissing(ItemType) Then
      itm.ItemType = ItemType
   End If
   itm.BarID = lBarId
   
   ' First add it to the m_colItems collection:
   m_colItems.Add itm, "C:" & lId
   
   ' Now add it to the bar's collection:
   pc.AddItem itm, lIndex, Index
   
   ' Have the item measured:
   fMeasureItem lBarId, itm.ID
   
   ' draw:
   UserControl_Resize
   
   ' Return the object:
   Dim cI As New cExplorerBarItem
   cI.fInit UserControl.hwnd, lBarId, lId
   Set fAddItem = cI
      
      
End Function

Friend Sub fMeasureTitle(ByVal lBarId As Long)
On Error Resume Next
Dim pc As pcExplorerBar
Dim lHDC As Long
Dim hFont As Long
Dim hFontOld As Long
Dim hTheme As Long
Dim sMeasureText As String
Dim lMeasureStyle As Long
Dim lHeightWith As Long
Dim lHeightWithout As Long
Dim lHeightOrigWith As Long
Dim lHeightOrigWithout As Long
Dim tR As RECT
Dim tTextR As RECT
Dim tBmp As BITMAP
Dim lSingleLineHeight As Long
   
   Set pc = m_colBars("C:" & lBarId)
   lHeightOrigWith = pc.TitleHeightWithScroll
   lHeightOrigWithout = pc.TitleHeightWithoutScroll
   
   hTheme = plGetTheme()
   GetWindowRect UserControl.hwnd, tR
   
   lHDC = m_cDibFade.hdc
   OffsetRect tR, -tR.left, -tR.Top
         
   pc.SingleLineTitleWithScroll = True
   pc.SingleLineTitleWithoutScroll = True
         
   If (m_bUseExplorerTheme) And Not (hTheme = 0) And (m_eStyle = eDefaultStyle) Then
      
      GetObjectAPI m_hDib(8), Len(tBmp), tBmp
      lHeightWith = tBmp.bmHeight
      lHeightWithout = lHeightWith
      
   Else
   
      hFont = m_cNCM.BoldenedFontHandle(IconFont)
      hFontOld = SelectObject(m_cDibFade.hdc, hFont)
      
      tR.left = tR.left + m_lMargin + 4
      tR.right = tR.right - m_lMargin - 4
      
      If (pc.IconIndex > -1) And ((m_hImlBarTitle = 0) Or (m_ptrVB6ImageListBarTitle = 0)) Then
         tR.left = tR.left + m_lBarTitleIconSize + 4
      End If
      
      If (pc.CanExpand) Then
         If (m_hDib(0) = 0) Then
            tR.right = tR.right - 20
         Else
            GetObjectAPI m_hDib(0), Len(tBmp), tBmp
            tR.right = tR.right - tBmp.bmWidth - 4
         End If
         tR.right = tR.right - 4
      End If
      
      If (m_eStyle = eSearchStyle) Then
         
         tR.right = tR.right - 8
         ' We can have multi-line title captions
         LSet tTextR = tR
         DrawText m_cDibFade.hdc, "Xg", -1, tTextR, DT_SINGLELINE Or DT_CALCRECT
         lSingleLineHeight = tTextR.bottom - tTextR.Top
         
         sMeasureText = pc.Title
         If Len(sMeasureText) < 3 Then
            sMeasureText = "Xg"
         End If
         lMeasureStyle = DT_CALCRECT Or DT_WORDBREAK
         
         LSet tTextR = tR
         DrawText m_cDibFade.hdc, sMeasureText, -1, tTextR, lMeasureStyle
         lHeightWithout = tTextR.bottom - tTextR.Top
         pc.SingleLineTitleWithoutScroll = (lHeightWithout <= lSingleLineHeight + 2)
         lHeightWithout = lHeightWithout + 11
         
         LSet tTextR = tR
         tTextR.right = tTextR.right - m_cNCM.ScrollWidth
         DrawText m_cDibFade.hdc, sMeasureText, -1, tTextR, lMeasureStyle
         lHeightWith = tTextR.bottom - tTextR.Top
         pc.SingleLineTitleWithScroll = (lHeightWith <= lSingleLineHeight + 2)
         lHeightWith = lHeightWith + 11

      Else
         
         ' Only one line
         sMeasureText = "Xg"
         lMeasureStyle = DT_SINGLELINE Or DT_CALCRECT
         
         LSet tTextR = tR
         DrawText m_cDibFade.hdc, sMeasureText, -1, tTextR, lMeasureStyle
         lHeightWithout = tTextR.bottom - tTextR.Top + 11
         lHeightWith = lHeightWithout
                  
      End If
      
   End If
   
   lHeightWith = IIf(lHeightWith < 24, 24, lHeightWith)
   lHeightWithout = IIf(lHeightWithout < 24, 24, lHeightWithout)
   
   pc.TitleTextHeightWithScroll = lHeightWith
   pc.TitleTextHeightWithoutScroll = lHeightWithout
   
   If (pc.IconIndex > -1) And ((m_hImlBarTitle = 0) Or (m_ptrVB6ImageListBarTitle = 0)) Then
      lHeightWith = IIf(lHeightWith < m_lBarTitleIconSize, m_lBarTitleIconSize, lHeightWith)
      lHeightWithout = IIf(lHeightWithout < m_lBarTitleIconSize, m_lBarTitleIconSize, lHeightWithout)
   End If
   pc.TitleHeightWithScroll = lHeightWith
   pc.TitleHeightWithoutScroll = lHeightWithout
   
   SelectObject m_cDibFade.hdc, hFontOld ' Corrected GDI leak 2003-07-05
   DeleteObject hFont

   If Not (hTheme = 0) Then
      CloseThemeData hTheme
   End If
   
   If Not (lHeightOrigWith = pc.TitleHeightWithScroll) Or _
      Not (lHeightOrigWithout = pc.TitleHeightWithoutScroll) Then
      pMeasure
      pPaint
      UserControl.Refresh
   End If
   
End Sub

Friend Sub fMeasureItem(ByVal lBarId As Long, ByVal lItemId As Long)
On Error Resume Next
Dim pc As pcExplorerBar
Dim itm As pcExplorerBarItem
Dim lHDC As Long
Dim tR As RECT
Dim tTextR As RECT
Dim lWidthNoScroll As Long
Dim lWidthScroll As Long
Dim hFontOld As Long
Dim hTheme As Long
Dim lHeight As Long
Dim lHeightOrigWith As Long
Dim lHeightOrigWithout As Long
Dim iFnt As IFont

   Set pc = m_colBars("C:" & lBarId)
   lHeightOrigWith = pc.HeightWithScroll
   lHeightOrigWithout = pc.HeightWithoutScroll
   
   Set itm = m_colItems("C:" & lItemId)
   
   If (itm.ItemType = eItemControlPlaceHolder) Then
      itm.HeightWithoutScroll = itm.ControlHeight
      itm.HeightWithScroll = itm.ControlHeight
      pResizeContainedControl pc, itm
   Else
      hTheme = plGetTheme()
      GetWindowRect UserControl.hwnd, tR
      
      lHDC = m_cDibFade.hdc
      OffsetRect tR, -tR.left, -tR.Top
      
      lWidthNoScroll = tR.right - tR.left - m_lMargin * 2
      If (itm.IconIndex > -1) Then
         lWidthNoScroll = lWidthNoScroll - m_lIconSize - m_lMargin \ 2
      End If
      lWidthScroll = lWidthNoScroll - m_cNCM.ScrollWidth
   
      ' Choose the appropriate font:
      If (itm.Font Is Nothing) Then
         Set iFnt = m_cNCM.Font(lHDC, IconFont)
      Else
         Set iFnt = itm.Font
      End If
      If (itm.Bold) Then
         iFnt.Bold = True
      End If
      hFontOld = SelectObject(lHDC, iFnt.hFont)
      
      LSet tTextR = tR
      tTextR.right = lWidthNoScroll - 23
      DrawText lHDC, itm.Text, -1, tTextR, DT_LEFT Or DT_WORDBREAK Or DT_CALCRECT 'Or DT_NOPREFIX
      lHeight = tTextR.bottom - tTextR.Top
      If (lHeight < m_lIconSize) Then
         lHeight = m_lIconSize
      End If
      lHeight = lHeight + 2
      itm.HeightWithoutScroll = lHeight
      
      LSet tTextR = tR
      tTextR.right = lWidthScroll - 23
      DrawText lHDC, itm.Text, -1, tTextR, DT_LEFT Or DT_WORDBREAK Or DT_CALCRECT 'Or DT_NOPREFIX
      lHeight = tTextR.bottom - tTextR.Top
      If (lHeight < m_lIconSize) Then
         lHeight = m_lIconSize
      End If
      lHeight = lHeight + 2
      itm.HeightWithScroll = lHeight
      
      SelectObject lHDC, hFontOld
      If Not (hTheme = 0) Then
         CloseThemeData hTheme
      End If
   
   End If
   
   ' Now calculate the overall height of the item this is
   ' associated with:
   pc.SetHeightFromItems
   
   If Not (lHeightOrigWith = pc.HeightWithScroll) Or _
      Not (lHeightOrigWithout = pc.HeightWithoutScroll) Then
      pMeasure
      pPaint
      UserControl.Refresh
   End If
   

End Sub
   
Friend Sub fRemoveItem(ByVal lId As Long, ByVal Key As Variant)
   On Error Resume Next

Dim pc As pcExplorerBar
   If (fVerifyId(lId, 1)) Then
      Set pc = m_colBars.Item("C:" & lId)
      lId = pc.IDForKey(Key)
      If (lId > 0) Then
         Dim iItem As Long
         Dim itm As pcExplorerBarItem
         Dim ctl As Control
         ' we can remove it:
         iItem = pc.ItemIndex(Key)
         Set itm = pc.Item(iItem)
         If Not (itm.lPtrPanel = 0) Then
            Set ctl = ObjectFromPtr(itm.lPtrPanel)
            ctl.Visible = False
         End If
         
         m_colItems.Remove "C:" & lId
         pc.RemoveItem lId
         
         ' Resize the bar:
         fBarChanged pc.ID, True, False
         
         ' draw:
         UserControl_Resize
         
      End If
   End If
End Sub
Friend Function fVerifyId(ByVal lId As Long, ByVal lIdType As Long) As Boolean
   On Error Resume Next
   Select Case lIdType
   Case 0
      fVerifyId = True
   Case 1
      On Error Resume Next
      Dim o As Object
      Set o = m_colBars.Item("C:" & lId)
      fVerifyId = (Err.Number = 0)
   Case 2
      On Error Resume Next
      Set o = m_colItems.Item("C:" & lId)
      fVerifyId = (Err.Number = 0)
   Case Else
      Debug.Assert "Incorrect ID Type" = ""
   End Select
End Function

Public Property Get ShowFocusRect() As Boolean
Attribute ShowFocusRect.VB_Description = "Gets/sets whether the focus rectangle is drawn once the user makes the first keyboard action in the control.  The Bars.Clear method reset s internal tracking of whether the user has made a keyboard action or not."
   On Error Resume Next
   
   ShowFocusRect = m_bShowFocusRect
End Property
Public Property Let ShowFocusRect(ByVal bState As Boolean)
   On Error Resume Next
   
   m_bShowFocusRect = bState
   PropertyChanged "ShowFocusRect"
End Property

Public Property Get BackColorStart() As OLE_COLOR
Attribute BackColorStart.VB_Description = "Gets/sets the start colour of the gradient background of the control.  Set to -1 for the default colour."
   On Error Resume Next
   
   BackColorStart = m_oBackColorStart
End Property
Public Property Let BackColorStart(ByVal oColor As OLE_COLOR)
   On Error Resume Next
   
   If Not (oColor = m_oBackColorStart) Then
      m_oBackColorStart = oColor
      PropertyChanged "BackColorStart"
   End If
End Property
Public Property Get BackColorEnd() As OLE_COLOR
Attribute BackColorEnd.VB_Description = "Gets/sets the end colour of the gradient background of the control.  Set to -1 for the default colour."
   On Error Resume Next
   
   BackColorEnd = m_oBackColorEnd
End Property
Public Property Let BackColorEnd(ByVal oColor As OLE_COLOR)
   On Error Resume Next
   
   If Not (oColor = m_oBackColorEnd) Then
      m_oBackColorEnd = oColor
      PropertyChanged "BackColorEnd"
   End If
End Property
Public Property Get UseExplorerTransitionStyle() As Boolean
Attribute UseExplorerTransitionStyle.VB_Description = "For future expansion. Not supported in this release."
   On Error Resume Next
   
   UseExplorerTransitionStyle = m_bUseExplorerTransitionStyle
End Property
Public Property Let UseExplorerTransitionStyle(ByVal bState As Boolean)
  On Error Resume Next
   If Not (m_bUseExplorerTransitionStyle = bState) Then
      m_bUseExplorerTransitionStyle = bState
      PropertyChanged "UseExplorerTransitionStyle"
   End If
End Property
Public Property Get UseExplorerStyle() As Boolean
Attribute UseExplorerStyle.VB_Description = "Gets/sets the drawing mode of the control.  The default is True, when the control uses the XP Theme if one is set otherwise draws using a Windows Classic style.  When false, the control draws using an emulation of the themed XP version."
   On Error Resume Next
   
   UseExplorerStyle = m_bUseExplorerTheme
End Property
Public Property Let UseExplorerStyle(ByVal bState As Boolean)
   On Error Resume Next
   If Not (m_bUseExplorerTheme = bState) Then
      
      m_bUseExplorerTheme = bState
      If (IsXp And m_bUseExplorerTheme) Or (Not (m_bUseExplorerTheme)) Then
         If Not (pbLoadShellStyleBitmaps()) Then
            ' If we can't load the shell style DLL then we're stuffed
            m_bUseExplorerTheme = False
            pbLoadShellStyleBitmaps
         End If
      End If
      
      If Not m_colBars Is Nothing Then
         Dim pc As pcExplorerBar
         For Each pc In m_colBars
            fBarChanged pc.ID, False, True
         Next
         pbColouriseWatermarks
         pMeasure
      End If
      
      pPaint
      UserControl.Refresh
      
      PropertyChanged "UseExplorerTheme"
   End If
End Property
Public Property Get Style() As EExplorerBarStyles
Attribute Style.VB_Description = "Gets/sets whether the control draws like the Search bar or in the default way."
   On Error Resume Next
   
   Style = m_eStyle
End Property
Public Property Let Style(ByVal eStyle As EExplorerBarStyles)
On Error Resume Next
Dim pc As pcExplorerBar

   If (Not (m_eStyle = eStyle)) Then
      m_eStyle = eStyle
   
      pbCreateBitmapWorkDC
      pbLoadShellStyleBitmaps
   
      If Not (m_colBars Is Nothing) Then
         For Each pc In m_colBars
            fBarChanged pc.ID, False, True
         Next
         pbColouriseWatermarks
         pMeasure
      End If
      
      pPaint
      UserControl.Refresh
      
      PropertyChanged "Style"
   End If
End Property
Public Property Get DefaultPanelColor(ByVal bIsSpecial As Boolean) As OLE_COLOR
Attribute DefaultPanelColor.VB_Description = "Gets the background colour of the panel holding the items in a bar."
On Error Resume Next
Dim hTheme As Long
   hTheme = plGetTheme()
   If (bIsSpecial) Then
      DefaultPanelColor = m_lThemePanelColor
   Else
      DefaultPanelColor = BlendColor(m_lThemePanelColor, TranslateColor(vbActiveTitleBar), 230)
   End If
   If (m_bUseExplorerTheme) Then
      If (hTheme = 0) Then
         DefaultPanelColor = vbWindowBackground
      End If
   End If
   If Not (hTheme = 0) Then
      CloseThemeData hTheme
   End If
End Property
Public Property Let ImageList( _
        ByRef vImageList As Variant _
    )
Attribute ImageList.VB_Description = "Associates an ImageList with the control.  If using a Microsoft ImageList, pass the object as the value, otherwise pass the ComCtl32 hImageList handle."
   On Error Resume Next
    
    m_hIml = 0
    m_ptrVB6ImageList = 0
    If (VarType(vImageList) = vbLong) Then
        ' Assume a handle to an image list:
        m_hIml = vImageList
    ElseIf (VarType(vImageList) = vbObject) Then
        ' Assume a VB image list:
        On Error Resume Next
        ' Get the image list initialised..
        vImageList.ListImages(1).Draw 0, 0, 0, 1
        m_hIml = vImageList.hImageList
        If (Err.Number = 0) Then
            ' Check for VB6 image list:
            If (TypeName(vImageList) = "ImageList") Then
               Dim o As Object
               Set o = vImageList
               m_ptrVB6ImageList = ObjPtr(o)
            End If
        Else
            Debug.Print "Failed to Get Image list Handle", "cVGrid.ImageList"
        End If
    End If
    If (m_hIml <> 0) Then
        If (m_ptrVB6ImageList <> 0) Then
            m_lIconSize = vImageList.ImageHeight
        Else
            Dim rc As RECT
            ImageList_GetImageRect m_hIml, 0, rc
            m_lIconSize = rc.bottom - rc.Top
        End If
    End If
End Property
Public Property Let BarTitleImageList( _
        ByRef vImageList As Variant _
    )
   On Error Resume Next
    
    m_hImlBarTitle = 0
    m_ptrVB6ImageListBarTitle = 0
    If (VarType(vImageList) = vbLong) Then
        ' Assume a handle to an image list:
        m_hImlBarTitle = vImageList
    ElseIf (VarType(vImageList) = vbObject) Then
        ' Assume a VB image list:
        On Error Resume Next
        ' Get the image list initialised..
        vImageList.ListImages(1).Draw 0, 0, 0, 1
        m_hImlBarTitle = vImageList.hImageList
        If (Err.Number = 0) Then
            ' Check for VB6 image list:
            If (TypeName(vImageList) = "ImageList") Then
               Dim o As Object
               Set o = vImageList
               m_ptrVB6ImageListBarTitle = ObjPtr(o)
            End If
        Else
            Debug.Print "Failed to Get Image list Handle", "cVGrid.ImageList"
        End If
    End If
    If (m_hImlBarTitle <> 0) Then
        If (m_ptrVB6ImageListBarTitle <> 0) Then
            m_lBarTitleIconSize = vImageList.ImageHeight
        Else
            Dim rc As RECT
            ImageList_GetImageRect m_hImlBarTitle, 0, rc
            m_lBarTitleIconSize = rc.bottom - rc.Top
        End If
    End If
End Property
Public Property Get Redraw() As Boolean
Attribute Redraw.VB_Description = "Gets/sets whether changes to the appearance or items/bars in the control are redrawn.  Turn off to make many changes quickly."
   On Error Resume Next
   
   Redraw = m_bRedraw
End Property
Public Property Let Redraw(ByVal bState As Boolean)
On Error Resume Next
   If Not (m_bRedraw = bState) Then
      m_bRedraw = bState
      If (m_bRedraw) Then
         ' TODO: Here we want to scroll the items from their
         ' current positions to the new desired positions
         UserControl_Resize
      End If
      PropertyChanged "Redraw"
   End If
End Property

Private Sub ImageListDrawIcon( _
        ByVal ptrVb6ImageList As Long, _
        ByVal hdc As Long, _
        ByVal hIml As Long, _
        ByVal iIconIndex As Long, _
        ByVal lX As Long, _
        ByVal lY As Long, _
        Optional ByVal bSelected As Boolean = False, _
        Optional ByVal bBlend25 As Boolean = False _
    )
   On Error Resume Next

Dim lFlags As Long
Dim lR As Long

    lFlags = ILD_TRANSPARENT
    If (bSelected) Then
        lFlags = lFlags Or ILD_SELECTED
    End If
    If (bBlend25) Then
        lFlags = lFlags Or ILD_BLEND25
    End If
    If (ptrVb6ImageList <> 0) Then
        Dim o As Object
        On Error Resume Next
        Set o = ObjectFromPtr(ptrVb6ImageList)
        If Not (o Is Nothing) Then
            o.ListImages(iIconIndex + 1).Draw hdc, lX * Screen.TwipsPerPixelX, lY * Screen.TwipsPerPixelY, lFlags
        End If
    Else
        lR = ImageList_Draw( _
                hIml, _
                iIconIndex, _
                hdc, _
                lX, _
                lY, _
                lFlags)
        If (lR = 0) Then
            Debug.Print "Failed to draw Image: " & iIconIndex & " onto hDC " & hdc, "ImageListDrawIcon"
        End If
    End If
End Sub

Public Property Get Bars() As cExplorerBars
Attribute Bars.VB_Description = "Gets the collection of bars associated with the control."
   On Error Resume Next
   
   Dim cB As New cExplorerBars
   cB.fInit UserControl.hwnd
   Set Bars = cB
End Property

Private Sub pPaintBackground( _
      ByVal lHDC As Long, _
      ByVal hTheme As Long, _
      tR As RECT _
   )
   On Error Resume Next
   If (hTheme = 0) Then
      If (m_bUseExplorerTheme) Then
         Dim hBr As Long
         hBr = GetSysColorBrush(vbWindowBackground And &H1F&)
         FillRect lHDC, tR, hBr
         DeleteObject hBr
      Else
         ' Emulate XP Style:
         Dim lBackColorStart As Long
         Dim lBackColorEnd As Long
         If (BackColorStart = CLR_INVALID) Then
            lBackColorStart = BlendColor(vbInactiveTitleBar, &HFFFFFF, 128)
         Else
            lBackColorStart = BackColorStart
         End If
         If (BackColorEnd = CLR_INVALID) Then
            lBackColorEnd = BlendColor(vbActiveTitleBar, &HFFFFFF, 192)
         Else
            lBackColorEnd = BackColorEnd
         End If
         GradientFillRect lHDC, tR, lBackColorStart, lBackColorEnd, GRADIENT_FILL_RECT_V
      End If
   Else
      ' Draw the background:
      DrawThemeBackground hTheme, lHDC, 0, 0, tR, tR
   End If
End Sub
Private Sub pPaintBars( _
      ByVal lHDC As Long, _
      ByVal hTheme As Long, _
      tR As RECT _
   )
On Error Resume Next
Dim pc As pcExplorerBar

   If Not m_colBars Is Nothing Then
      
      For Each pc In m_colBars
         pPaintBar pc, lHDC, hTheme, tR
      Next
         
      pPaintBorders lHDC, hTheme, tR
      
   End If
End Sub

Private Sub pPaintBorders( _
      ByVal lHDC As Long, _
      ByVal hTheme As Long, _
      tR As RECT _
   )
On Error Resume Next
Dim pc As pcExplorerBar
Dim tWorkR As RECT
Dim hPen As Long
Dim hPenOld As Long
Dim tJunk As POINTAPI

   If (m_eStyle = eSearchStyle) Then
      If (m_colBars.Count > 0) Then
         
      
         Set pc = m_colBars(m_colBars.Count)
         LSet tWorkR = tR
         tWorkR.Top = tR.Top + m_lBarSpacing
         tWorkR.left = tR.left + m_lMargin
         tWorkR.right = tR.right - m_lMargin
         If (m_cScrollBar.Visible(efsVertical)) Then
            tWorkR.bottom = tR.Top + pc.Top + pc.HeightWithScroll + pc.TitleHeightWithScroll
            OffsetRect tWorkR, 0, -m_cScrollBar.Value(efsVertical)
         Else
            tWorkR.bottom = tR.Top + pc.Top + pc.HeightWithoutScroll + pc.TitleHeightWithoutScroll
         End If
         
         hPen = CreatePen(PS_SOLID, 1, &HFFFFFF)
         hPenOld = SelectObject(lHDC, hPen)
         
         ' Top
         MoveToEx lHDC, tWorkR.left + 1, tWorkR.Top - 1, tJunk
         LineTo lHDC, tWorkR.right - 1, tWorkR.Top - 1
         ' Top-left
         MoveToEx lHDC, tWorkR.left + 1, tWorkR.Top, tJunk
         LineTo lHDC, tWorkR.left - 1, tWorkR.Top
         MoveToEx lHDC, tWorkR.left, tWorkR.Top + 1, tJunk
         LineTo lHDC, tWorkR.left - 1, tWorkR.Top + 1
         ' Top-right
         MoveToEx lHDC, tWorkR.right - 2, tWorkR.Top, tJunk
         LineTo lHDC, tWorkR.right, tWorkR.Top
         MoveToEx lHDC, tWorkR.right - 1, tWorkR.Top + 1, tJunk
         LineTo lHDC, tWorkR.right - 2, tWorkR.Top + 1
                  
         ' Left
         MoveToEx lHDC, tWorkR.left - 1, tWorkR.Top + 1, tJunk
         LineTo lHDC, tWorkR.left - 1, tWorkR.bottom - 1
         ' Right
         MoveToEx lHDC, tWorkR.right, tWorkR.Top + 1, tJunk
         LineTo lHDC, tWorkR.right, tWorkR.bottom - 1
         
         ' Bottom
         MoveToEx lHDC, tWorkR.left + 1, tWorkR.bottom, tJunk
         LineTo lHDC, tWorkR.right - 1, tWorkR.bottom
         ' Bottom-left
         MoveToEx lHDC, tWorkR.left + 1, tWorkR.bottom - 1, tJunk
         LineTo lHDC, tWorkR.left - 1, tWorkR.bottom - 1
         MoveToEx lHDC, tWorkR.left, tWorkR.bottom - 2, tJunk
         LineTo lHDC, tWorkR.left - 1, tWorkR.bottom - 2
         ' Bottom-right
         MoveToEx lHDC, tWorkR.right - 2, tWorkR.bottom - 1, tJunk
         LineTo lHDC, tWorkR.right, tWorkR.bottom - 1
         MoveToEx lHDC, tWorkR.right - 1, tWorkR.bottom - 2, tJunk
         LineTo lHDC, tWorkR.right, tWorkR.bottom - 2
         
         SelectObject lHDC, hPenOld
         DeleteObject hPen
      
      End If
      
   End If
   
End Sub

Private Sub pPaintItem( _
      itm As pcExplorerBarItem, _
      ByVal lHDC As Long, _
      ByVal hTheme As Long, _
      tR As RECT _
   )
On Error Resume Next
Dim tItemR As RECT
Dim hFontOld As Long
Dim pc As pcExplorerBar
   
   Set pc = m_colBars("C:" & itm.BarID)
   
   LSet tItemR = tR
   tItemR.Top = tItemR.Top + itm.Top
   tItemR.left = tItemR.left + m_lMargin
   tItemR.right = tItemR.right - m_lMargin
   
   If (pc.Collapsing) Then
      OffsetRect tItemR, 0, pc.CollapseOffset
   End If

   ' Draw the icon:
   Dim tTextR As RECT
   LSet tTextR = tItemR
   
   If (itm.IconIndex > -1) Then
      Dim tIconR As RECT
      LSet tIconR = tItemR
      ImageListDrawIcon m_ptrVB6ImageList, lHDC, m_hIml, itm.IconIndex, tIconR.left, tIconR.Top
      tTextR.left = tTextR.left + m_lMargin \ 2 + m_lIconSize
   End If
   
   SetBkMode lHDC, TRANSPARENT
   If (itm.MouseDown Or itm.MouseOver) Then
      If Not (hTheme = 0) Or (m_bUseExplorerTheme) Or (itm.TextColorOver = CLR_INVALID) Then
         If (itm.ItemType = eItemText) Then
            ' MAH
            If GetIsXP Or GetIs2000With256 Then
              SetTextColor lHDC, TranslateColor(vbWindowText)
            Else
              SetTextColor lHDC, RGB(66, 142, 255)
            End If
            ' MAH
         Else
            ' MAH
            If GetIsXP Or GetIs2000With256 Then
              SetTextColor lHDC, BlendColor(vbHighlight, 0, 240)
            Else
              SetTextColor lHDC, RGB(66, 142, 255)
            End If
            ' MAH
         End If
      Else
        ' MAH
        If GetIsXP Or GetIs2000With256 Then
          SetTextColor lHDC, TranslateColor(itm.TextColorOver)
        Else
          SetTextColor lHDC, RGB(66, 142, 255)
        End If
        ' MAH
      End If
   Else
      If Not (hTheme = 0) Or (m_bUseExplorerTheme) Or (itm.TextColor = CLR_INVALID) Then
         If (itm.ItemType = eItemText) Then
           ' MAH
           If GetIsXP Or GetIs2000With256 Then
              SetTextColor lHDC, TranslateColor(vbWindowText)
           Else
             SetTextColor lHDC, RGB(33, 93, 198)
           End If
           ' MAH
         Else
           ' MAH
           If GetIsXP Or GetIs2000With256 Then
             SetTextColor lHDC, BlendColor(vbHighlight, 0, 192)
           Else
             SetTextColor lHDC, RGB(33, 93, 198)
           End If
           ' MAH
         End If
      Else
         ' MAH
         If GetIsXP Or GetIs2000With256 Then
           SetTextColor lHDC, TranslateColor(itm.TextColor)
        Else
          SetTextColor lHDC, RGB(33, 93, 198)
        End If
         ' MAH
      End If
   End If
   Dim iFnt As IFont
   If (itm.Font Is Nothing) Then
      Set iFnt = m_cNCM.Font(lHDC, IconFont)
   Else
      itm.Font.Clone iFnt
   End If
   If (itm.MouseDown Or itm.MouseOver) Then
      iFnt.Underline = True
   End If
   If (itm.Bold) Then
      iFnt.Bold = True
   End If
   hFontOld = SelectObject(lHDC, iFnt.hFont)
   DrawText lHDC, itm.Text, -1, tTextR, DT_LEFT Or DT_WORDBREAK 'Or DT_NOPREFIX
   SelectObject lHDC, hFontOld
   
   If (m_bShowFocusRect And m_bFocus And (itm.ID = m_lIdSelItem) And m_bHaveUsedKeys) Then
      tItemR.left = tItemR.left - 3
      tItemR.right = tTextR.right
      tItemR.Top = tItemR.Top - 3
      If (m_cScrollBar.Visible(efsVertical)) Then
         tItemR.bottom = tItemR.Top + itm.HeightWithScroll + 4
      Else
         tItemR.bottom = tItemR.Top + itm.HeightWithoutScroll + 4
      End If
      DrawFocusRect lHDC, tItemR
   End If
   
End Sub

Private Sub pGetBarRect( _
      pc As pcExplorerBar, _
      tR As RECT, _
      tBarR As RECT _
   )
   On Error Resume Next
   LSet tBarR = tR
   tBarR.Top = pc.Top
   If (m_cScrollBar.Visible(efsVertical)) Then
      OffsetRect tBarR, 0, -m_cScrollBar.Value(efsVertical)
      tBarR.bottom = tBarR.Top + pc.HeightWithScroll
      tBarR.bottom = tBarR.bottom + pc.TitleHeightWithScroll
   Else
      tBarR.bottom = tBarR.Top + pc.HeightWithoutScroll
      tBarR.bottom = tBarR.bottom + pc.TitleHeightWithoutScroll
   End If
   tBarR.left = tBarR.left + m_lMargin
   tBarR.right = tBarR.right - m_lMargin
   
End Sub

Private Sub pPaintBar( _
      pc As pcExplorerBar, _
      ByVal lHDC As Long, _
      ByVal hTheme As Long, _
      tR As RECT, _
      Optional ByVal bHighlight As Boolean _
   )
   On Error Resume Next
Dim tBarR As RECT
Dim tDCR As RECT
Dim tTitleR As RECT
Dim tWorkR As RECT
Dim tSearchWorkR As RECT
Dim iPartId As Long
Dim tBmp As BITMAP
Dim hBr As Long
Dim hPen As Long
Dim hPenBorder As Long
Dim hPenOld As Long
Dim tJunk As POINTAPI
Dim hFont As Long
Dim hFontOld As Long
Dim iItem As Long
Dim iWidth As Long
Dim iHeight As Long
Dim lDrawStyle As Long
Dim itm As pcExplorerBarItem
Dim lIconY As Long
Dim lTextX As Long
   
   If Not m_bRedraw Then
      Exit Sub
   End If
   
   pGetBarRect pc, tR, tBarR
   
   If (tBarR.bottom < tR.Top) Or (tBarR.Top > tR.bottom) Then
      ' nothing to do
      Exit Sub
   End If
   
   ' Else we can draw the bar:
   
   LSet tTitleR = tBarR
   If (pc.CanExpand) Or Len(pc.Title) > 0 Then
      If (m_cScrollBar.Visible(efsVertical)) Then
         tTitleR.bottom = tBarR.Top + pc.TitleHeightWithScroll
      Else
         tTitleR.bottom = tBarR.Top + pc.TitleHeightWithoutScroll
      End If
   Else
      tTitleR.bottom = tBarR.Top
   End If
   
   If (pc.IconIndex > -1) And ((m_hImlBarTitle = 0) Or (m_ptrVB6ImageListBarTitle = 0)) Then
      ' MAH Center the bar
      ' lIconY = tTitleR.Top
      lIconY = tTitleR.Top + 4
      ' MAH
      lTextX = m_lBarTitleIconSize - 8
      If (m_eStyle = eDefaultStyle) Then
         ' The title bar height needs to be adjusted
         If (m_cScrollBar.Visible(efsVertical)) Then
            tTitleR.Top = tTitleR.bottom - pc.TitleTextHeightWithScroll
         Else
            tTitleR.Top = tTitleR.bottom - pc.TitleTextHeightWithoutScroll
         End If
      End If
   End If
      
   If (m_bUseExplorerTheme) And (hTheme = 0) Then
         
      ' Draw as per XP with Windows Classic Mode applied:
      
      ' Draw title bar:
      If (pc.IsSpecial) Then
         hBr = GetSysColorBrush(vbActiveTitleBar And &H1F&)
         hPenBorder = CreatePen(PS_SOLID, 1, TranslateColor(vbActiveTitleBar))
         SetTextColor lHDC, TranslateColor(vbTitleBarText)
         hPen = CreatePen(PS_SOLID, 1, TranslateColor(vbTitleBarText))
      Else
         hBr = GetSysColorBrush(vbButtonFace And &H1F&)
         hPenBorder = CreatePen(PS_SOLID, 1, TranslateColor(vbButtonFace))
         SetTextColor lHDC, TranslateColor(vbWindowText)
         hPen = CreatePen(PS_SOLID, 1, TranslateColor(vbWindowText))
      End If
      FillRect lHDC, tTitleR, hBr
      DeleteObject hBr
      
      ' Text:
      LSet tWorkR = tTitleR
      tWorkR.left = tWorkR.left + m_lMargin + 2
      tWorkR.right = tWorkR.right - m_lMargin - 2
      If (pc.CanExpand) Then
         GetObjectAPI m_hDib(0), Len(tBmp), tBmp
         tWorkR.right = tWorkR.right - tBmp.bmWidth - 4
      End If
      If (pc.IconIndex > -1) And ((m_hImlBarTitle = 0) Or (m_ptrVB6ImageListBarTitle = 0)) Then
         tWorkR.left = tWorkR.left + lTextX
      End If
      
      hFont = m_cNCM.BoldenedFontHandle(IconFont)
      hFontOld = SelectObject(lHDC, hFont)
      
      lDrawStyle = DT_LEFT Or DT_SINGLELINE Or DT_VCENTER Or DT_END_ELLIPSIS
      If (m_eStyle = eSearchStyle) Then
         If m_cScrollBar.Visible(efsVertical) Then
            If (pc.SingleLineTitleWithScroll) Then
               lDrawStyle = DT_LEFT Or DT_SINGLELINE Or DT_VCENTER
            Else
               lDrawStyle = DT_LEFT Or DT_WORDBREAK
               tWorkR.Top = tWorkR.Top + 4
            End If
         Else
            If (pc.SingleLineTitleWithoutScroll) Then
               lDrawStyle = DT_LEFT Or DT_SINGLELINE Or DT_VCENTER
            Else
               lDrawStyle = DT_LEFT Or DT_WORDBREAK
               tWorkR.Top = tWorkR.Top + 4
            End If
         End If
      End If
      DrawText lHDC, pc.Title, -1, tWorkR, lDrawStyle
      
      SelectObject lHDC, hFontOld
      If Not (hFont = 0) Then
         DeleteObject hFont
      End If
      
      ' Draw the collapse/expand bitmap:
      LSet tWorkR = tTitleR
      
      If (pc.CanExpand) Then
      
         tWorkR.left = tWorkR.right - 22
         
         tWorkR.Top = tWorkR.Top + (tWorkR.bottom - tWorkR.Top - 16) \ 2
         tWorkR.right = tWorkR.left + 17
         tWorkR.bottom = tWorkR.Top + 16
         hPenOld = SelectObject(lHDC, hPen)

         If (pc.State = eBarExpanded) Then
            MoveToEx lHDC, tWorkR.left + 5, tWorkR.Top + 7, tJunk
            LineTo lHDC, tWorkR.left + 8, tWorkR.Top + 4
            LineTo lHDC, tWorkR.left + 12, tWorkR.Top + 8
            MoveToEx lHDC, tWorkR.left + 6, tWorkR.Top + 7, tJunk
            LineTo lHDC, tWorkR.left + 8, tWorkR.Top + 5
            LineTo lHDC, tWorkR.left + 11, tWorkR.Top + 8
            
            MoveToEx lHDC, tWorkR.left + 5, tWorkR.Top + 11, tJunk
            LineTo lHDC, tWorkR.left + 8, tWorkR.Top + 8
            LineTo lHDC, tWorkR.left + 12, tWorkR.Top + 12
            MoveToEx lHDC, tWorkR.left + 6, tWorkR.Top + 11, tJunk
            LineTo lHDC, tWorkR.left + 8, tWorkR.Top + 9
            LineTo lHDC, tWorkR.left + 11, tWorkR.Top + 12
            
         Else
            MoveToEx lHDC, tWorkR.left + 5, tWorkR.Top + 4, tJunk
            LineTo lHDC, tWorkR.left + 8, tWorkR.Top + 7
            LineTo lHDC, tWorkR.left + 12, tWorkR.Top + 3
            MoveToEx lHDC, tWorkR.left + 6, tWorkR.Top + 4, tJunk
            LineTo lHDC, tWorkR.left + 8, tWorkR.Top + 6
            LineTo lHDC, tWorkR.left + 11, tWorkR.Top + 3
            
            MoveToEx lHDC, tWorkR.left + 5, tWorkR.Top + 8, tJunk
            LineTo lHDC, tWorkR.left + 8, tWorkR.Top + 11
            LineTo lHDC, tWorkR.left + 12, tWorkR.Top + 7
            MoveToEx lHDC, tWorkR.left + 6, tWorkR.Top + 8, tJunk
            LineTo lHDC, tWorkR.left + 8, tWorkR.Top + 10
            LineTo lHDC, tWorkR.left + 11, tWorkR.Top + 7
            
         End If
         
         If (pc.MouseDown And pc.MouseOver) Then
            DrawEdgeAPI lHDC, tWorkR, EDGE_RAISED, BF_RECT
         ElseIf (pc.MouseOver) Then
            DrawEdgeAPI lHDC, tWorkR, EDGE_RAISED, BF_RECT
         End If
         
         SelectObject lHDC, hPenOld
         DeleteObject hPen
      End If
      
      ' draw the focus rectangle if required:
      If (m_bShowFocusRect) And (m_bFocus) And (pc.ID = m_lIdSelBar) And (m_bHaveUsedKeys) Then
         LSet tWorkR = tTitleR
         tWorkR.right = tWorkR.right - 2
         DrawFocusRect lHDC, tWorkR
      End If
      
      ' draw the icon if required:
      If (pc.IconIndex > -1) And ((m_hImlBarTitle = 0) Or (m_ptrVB6ImageListBarTitle = 0)) Then
         LSet tWorkR = tTitleR
         If Not (bHighlight) Then
            ImageListDrawIcon m_ptrVB6ImageListBarTitle, lHDC, m_hImlBarTitle, pc.IconIndex, tWorkR.left + 4, lIconY
         Else
            m_cDibFade.LoadPictureBlt lHDC, tWorkR.left + 4, tWorkR.Top, m_lBarTitleIconSize, tWorkR.bottom - tWorkR.Top
            ImageListDrawIcon m_ptrVB6ImageListBarTitle, m_cDibFade.hdc, m_hImlBarTitle, pc.IconIndex, 0, lIconY - tWorkR.Top
            m_cDibFade.PaintPicture lHDC, tWorkR.left + 4, tWorkR.Top, m_lBarTitleIconSize, tWorkR.bottom - tWorkR.Top
         End If
      End If
      
      ' if the bar is expanded, then draw it:
      If (pc.State = eBarExpanded) Or (pc.Expanding) Then
         
         ' Fill the background:
         LSet tWorkR = tBarR
         tWorkR.Top = tTitleR.bottom
         OffsetRect tWorkR, -tBarR.left, -tTitleR.bottom
         If (pc.Collapsing) Then
            tWorkR.bottom = tWorkR.bottom + pc.CollapseOffset
         ElseIf (pc.Expanding) Then
            OffsetRect tWorkR, 0, -pc.CollapseOffset
         End If
         
         hBr = GetSysColorBrush(vbWindowBackground And &H1F&)
         FillRect m_cDibFade.hdc, tWorkR, hBr
         DeleteObject hBr
               
         ' Watermark?
         pc.RenderWatermark m_cDibFade.hdc, m_hDC, tWorkR.left, tWorkR.Top, tWorkR.right, tWorkR.bottom, m_lMargin, m_lItemSpacing
         
         ' Draw the borders:
         If Not (m_eStyle = eSearchStyle) Then
            hPenOld = SelectObject(m_cDibFade.hdc, hPenBorder)
            MoveToEx m_cDibFade.hdc, tWorkR.right - 1, tWorkR.Top, tJunk
            LineTo m_cDibFade.hdc, tWorkR.right - 1, tWorkR.bottom - 1
            LineTo m_cDibFade.hdc, tWorkR.left, tWorkR.bottom - 1
            LineTo m_cDibFade.hdc, tWorkR.left, tWorkR.Top - 1
            SelectObject m_cDibFade.hdc, hPenOld
         End If
         DeleteObject hPenBorder
         
         ' Draw the subitems:
         LSet tDCR = tBarR
         OffsetRect tDCR, -tDCR.left, -tDCR.Top
         For iItem = 1 To pc.ItemCount
            Set itm = pc.Item(iItem)
            pPaintItem itm, m_cDibFade.hdc, hTheme, tDCR
         Next iItem
                  
         ' Swap mem DC back to display:
         If (pc.Expanding) Then
            OffsetRect tWorkR, 0, pc.CollapseOffset
         End If
         OffsetRect tWorkR, tBarR.left, tTitleR.bottom
         If (pc.Alpha < 255) Then
            ' HERE: ONLY PAINT THE PORTION OF ITEM WHICH SHOULD BE VISIBLE
            ' IE. CHANGE DEST HEIGHT AND SRC TOP
            m_cDibFade.AlphaPaintPicture lHDC, tWorkR.left, tWorkR.Top, tWorkR.right - tWorkR.left, tWorkR.bottom - tWorkR.Top, , , pc.Alpha, False
         Else
            m_cDibFade.PaintPicture lHDC, tWorkR.left, tWorkR.Top, tWorkR.right - tWorkR.left, tWorkR.bottom - tWorkR.Top
         End If
      End If
      
      DeleteObject hPen
               
   Else
      
      If (pc.IsSpecial) Then
         iPartId = 8 'EBP_SPECIALGROUPHEAD
      Else
         iPartId = 9 'EBP_NORMALGROUPHEAD
      End If
      ' Draw the title:
      
      ' a) Background:
      If (pc.CanExpand) And (m_eStyle = eDefaultStyle) Then
         pDrawShellStyleBitmap lHDC, tTitleR, iPartId, pc
      Else
         If (pc.IsSpecial) Then
            If (pc.PanelBackColor = -1) Then
               hBr = CreateSolidBrush(m_lThemePanelColor)
            Else
               hBr = CreateSolidBrush(TranslateColor(pc.PanelBackColor))
            End If
         Else
            If (pc.PanelBackColor = -1) Then
               hBr = CreateSolidBrush(BlendColor(m_lThemePanelColor, TranslateColor(vbActiveTitleBar), 230))
            Else
               hBr = CreateSolidBrush(TranslateColor(pc.PanelBackColor))
            End If
         End If
         FillRect lHDC, tTitleR, hBr
         DeleteObject hBr
      End If
      
      ' b) Text:
      LSet tWorkR = tTitleR
      tWorkR.left = tWorkR.left + m_lMargin
      tWorkR.right = tWorkR.right - m_lMargin
      If (pc.CanExpand) Then
         GetObjectAPI m_hDib(0), Len(tBmp), tBmp
         tWorkR.right = tWorkR.right - tBmp.bmWidth - 4
      End If
      If (pc.IconIndex > -1) And ((m_hImlBarTitle = 0) Or (m_ptrVB6ImageListBarTitle = 0)) Then
         tWorkR.left = tWorkR.left + lTextX
      End If
      hFont = m_cNCM.BoldenedFontHandle(IconFont)
      hFontOld = SelectObject(lHDC, hFont)
      If (pc.IsSpecial) Then
         If (pc.MouseOver Or pc.MouseDown) Then
            If (pc.TitleForeColorOver = -1) Then
               SetTextColor lHDC, BlendColor(&HFFFFFF, vbHighlight, 128)
            Else
               SetTextColor lHDC, TranslateColor(pc.TitleForeColorOver)
            End If
         Else
            If (pc.TitleForeColor = -1) Then
               SetTextColor lHDC, &HFFFFFF
            Else
               SetTextColor lHDC, TranslateColor(pc.TitleForeColor)
            End If
         End If
      Else
         If (pc.MouseOver Or pc.MouseDown) Then
            If (pc.TitleForeColorOver = -1) Then
               ' MAH
               If GetIsXP Or GetIs2000With256 Then
                 SetTextColor lHDC, BlendColor(vbHighlight, &H0, 240)
              Else
                 SetTextColor lHDC, RGB(66, 142, 255)
              End If
               ' MAH
            Else
               ' MAH
               If GetIsXP Or GetIs2000With256 Then
                 SetTextColor lHDC, TranslateColor(pc.TitleForeColorOver)
              Else
                 SetTextColor lHDC, RGB(66, 142, 255)
              End If
               ' MAH
            End If
         Else
            If (pc.TitleForeColor = -1) Then
               ' MAH
               If GetIsXP Or GetIs2000With256 Then
                 SetTextColor lHDC, BlendColor(vbHighlight, &H0, 192)
               Else
                 SetTextColor lHDC, RGB(33, 93, 198)
               End If
               ' MAH
            Else
               ' MAH
               If GetIsXP Or GetIs2000With256 Then
                 SetTextColor lHDC, TranslateColor(pc.TitleForeColor)
               Else
                 SetTextColor lHDC, RGB(33, 93, 198)
               End If
               ' MAH
            End If
         End If
      End If
      
      lDrawStyle = DT_LEFT Or DT_SINGLELINE Or DT_VCENTER Or DT_END_ELLIPSIS
      If (m_eStyle = eSearchStyle) Then
         If m_cScrollBar.Visible(efsVertical) Then
            If (pc.SingleLineTitleWithScroll) Then
               lDrawStyle = DT_LEFT Or DT_SINGLELINE Or DT_VCENTER
            Else
               lDrawStyle = DT_LEFT Or DT_WORDBREAK
               tWorkR.Top = tWorkR.Top + 4
            End If
         Else
            If (pc.SingleLineTitleWithoutScroll) Then
               lDrawStyle = DT_LEFT Or DT_SINGLELINE Or DT_VCENTER
            Else
               lDrawStyle = DT_LEFT Or DT_WORDBREAK
               tWorkR.Top = tWorkR.Top + 4
            End If
         End If
      End If
      DrawText lHDC, pc.Title, -1, tWorkR, lDrawStyle
      
      SelectObject lHDC, hFontOld
      If Not (hFont = 0) Then
         DeleteObject hFont
      End If
      
      ' Draw the icon:
      If (pc.CanExpand) Then
         If (pc.State = eBarExpanded) Then
            If (pc.IsSpecial) Then
               If (pc.MouseOver Or pc.MouseDown) Then
                  iPartId = 5
               Else
                  iPartId = 4 'EBP_SPECIALGROUPCOLLAPSE
               End If
            Else
               If (pc.MouseOver Or pc.MouseDown) Then
                  iPartId = 1 'EBP_NORMALGROUPCOLLAPSE
               Else
                  iPartId = 0
               End If
            End If
         Else
            If (pc.IsSpecial) Then
               If (pc.MouseOver Or pc.MouseDown) Then
                  iPartId = 7
               Else
                  iPartId = 6 'EBP_SPECIALGROUPEXPAND
               End If
            Else
               If (pc.MouseOver Or pc.MouseDown) Then
                  iPartId = 3
               Else
                  iPartId = 2 ' EBP_NORMALGROUPEXPAND
               End If
            End If
         End If
         LSet tWorkR = tTitleR
         Dim c As New pcAlphaDibSection
         c.CreateFromHBitmap m_hDib(iPartId)
         If Not (hTheme = 0) Then
            c.PreMultiplyAlpha
            c.AlphaPaintPicture lHDC, tWorkR.right - 2 - c.Width, tWorkR.Top + 1
         Else
            c.PaintPicture lHDC, tWorkR.right - 2 - c.Width, tWorkR.Top + 1
         End If
      End If
      
      ' draw the focus rectangle if required:
      If (m_bShowFocusRect) And (m_bFocus) And (pc.ID = m_lIdSelBar) And (m_bHaveUsedKeys) Then
         LSet tWorkR = tTitleR
         tWorkR.right = tWorkR.right - 2
         DrawFocusRect lHDC, tWorkR
      End If
      
      ' draw the icon if required:
      If (pc.IconIndex > -1) And ((m_hImlBarTitle = 0) Or (m_ptrVB6ImageListBarTitle = 0)) Then
         If Not (bHighlight) Then
            ' MAH Adjust the icon x pos
            ' ImageListDrawIcon m_ptrVB6ImageListBarTitle, lHDC, m_hImlBarTitle, pc.IconIndex, tWorkR.left + 4, lIconY
            ImageListDrawIcon m_ptrVB6ImageListBarTitle, lHDC, m_hImlBarTitle, pc.IconIndex, 16, lIconY
            ' MAH
         Else
            ' MAH Adjust the icon x pos
            ' m_cDibFade.LoadPictureBlt lHDC, tWorkR.left + 4, tWorkR.Top, m_lBarTitleIconSize, tWorkR.bottom - tWorkR.Top
            m_cDibFade.LoadPictureBlt lHDC, 16, tWorkR.Top, m_lBarTitleIconSize, tWorkR.bottom - tWorkR.Top
            ' MAH
            ImageListDrawIcon m_ptrVB6ImageListBarTitle, m_cDibFade.hdc, m_hImlBarTitle, pc.IconIndex, 0, lIconY - tWorkR.Top
            ' MAH Adjust the icon x pos
            ' m_cDibFade.PaintPicture lHDC, tWorkR.left + 4, tWorkR.Top, m_lBarTitleIconSize, tWorkR.bottom - tWorkR.Top
            m_cDibFade.PaintPicture lHDC, 16, tWorkR.Top, m_lBarTitleIconSize, tWorkR.bottom - tWorkR.Top
            ' MAH
         End If
      End If
      
      ' if the bar is expanded, then draw it:
      If (pc.State = eBarExpanded) Or (pc.Expanding) Then
         
         ' Fill the background:
         LSet tWorkR = tBarR
         tWorkR.Top = tTitleR.bottom
         OffsetRect tWorkR, -tBarR.left, -tTitleR.bottom
         If (pc.Collapsing) Then
            tWorkR.bottom = tWorkR.bottom + pc.CollapseOffset
         End If
         
         If (pc.IsSpecial) Then
            If (pc.PanelBackColor = -1) Then
               hBr = CreateSolidBrush(m_lThemePanelColor)
            Else
               hBr = CreateSolidBrush(TranslateColor(pc.PanelBackColor))
            End If
         Else
            If (pc.PanelBackColor = -1) Then
               hBr = CreateSolidBrush(BlendColor(m_lThemePanelColor, TranslateColor(vbActiveTitleBar), 230))
            Else
               hBr = CreateSolidBrush(TranslateColor(pc.PanelBackColor))
            End If
         End If
         ' MAH
         If GetIsXP Or GetIs2000With256 Then
           FillRect m_cDibFade.hdc, tWorkR, hBr
         Else
           hBr = CreateSolidBrush(TranslateColor(RGB(214, 223, 247)))
           FillRect m_cDibFade.hdc, tWorkR, hBr
         End If
         ' MAH
         DeleteObject hBr
         
         ' Watermark?
         pc.RenderWatermark m_cDibFade.hdc, m_hDC, tWorkR.left, tWorkR.Top, tWorkR.right, tWorkR.bottom, m_lMargin, m_lItemSpacing
         
         ' Draw the borders:
         If Not (m_eStyle = eSearchStyle) Then
            hPen = CreatePen(PS_SOLID, 1, &HFFFFFF)
            hPenOld = SelectObject(m_cDibFade.hdc, hPen)
            MoveToEx m_cDibFade.hdc, tWorkR.right - 1, tWorkR.Top, tJunk
            LineTo m_cDibFade.hdc, tWorkR.right - 1, tWorkR.bottom - 1
            LineTo m_cDibFade.hdc, tWorkR.left, tWorkR.bottom - 1
            LineTo m_cDibFade.hdc, tWorkR.left, tWorkR.Top - 1 ' 2003-07-05: This was off by 1 pixel
            SelectObject m_cDibFade.hdc, hPenOld
            DeleteObject hPen
         End If
         
         ' Draw the subitems:
         LSet tDCR = tBarR
         OffsetRect tDCR, -tDCR.left, -tDCR.Top
         For iItem = 1 To pc.ItemCount
            Set itm = pc.Item(iItem)
            pPaintItem itm, m_cDibFade.hdc, hTheme, tDCR
         Next iItem
                  
         ' Swap mem DC back to display:
         OffsetRect tWorkR, tBarR.left, tTitleR.bottom
         If (pc.Alpha < 255) Then
            ' HERE: ONLY PAINT THE PORTION OF ITEM WHICH SHOULD BE VISIBLE
            ' IE. CHANGE DEST HEIGHT AND SRC TOP
            If (pc.Expanding) Then
               m_cDibFade.AlphaPaintPicture lHDC, tWorkR.left, tWorkR.Top, tWorkR.right - tWorkR.left, tWorkR.bottom - tWorkR.Top - pc.CollapseOffset, , pc.CollapseOffset, pc.Alpha, False
            Else
               m_cDibFade.AlphaPaintPicture lHDC, tWorkR.left, tWorkR.Top, tWorkR.right - tWorkR.left, tWorkR.bottom - tWorkR.Top, , , pc.Alpha, False
            End If
         Else
            If (pc.Expanding) Then
               m_cDibFade.PaintPicture lHDC, tWorkR.left, tWorkR.Top, tWorkR.right - tWorkR.left, tWorkR.bottom - tWorkR.Top - pc.CollapseOffset, , pc.CollapseOffset
            Else
               m_cDibFade.PaintPicture lHDC, tWorkR.left, tWorkR.Top, tWorkR.right - tWorkR.left, tWorkR.bottom - tWorkR.Top
            End If
         End If
      End If
      
   End If
   
End Sub
Private Sub pPaint()
On Error Resume Next
Dim lHDC As Long
Dim tR As RECT

   If m_bRedraw Then

      lHDC = UserControl.hdc
      GetClientRect UserControl.hwnd, tR
   
      pPaintToDC lHDC, tR
   
   End If
   
End Sub

Private Sub pPaintToDC(ByVal lHDC As Long, tR As RECT)
On Error Resume Next
Dim hTheme As Long
         
   If m_bRedraw Then
         
      ' Load theme if required:
      hTheme = plGetTheme()
         
      ' Paint the background:
      pPaintBackground lHDC, hTheme, tR
      ' Paint also onto1 the fade out area:
      pPaintBackground m_cDibFade.hdc, hTheme, tR
      
      ' Paint the bars:
      pPaintBars lHDC, hTheme, tR
      
      ' Done
      If Not (hTheme = 0) Then
         CloseThemeData hTheme
      End If
   
   End If
   
End Sub

Private Sub pDrawShellStyleBitmap( _
      ByVal lHDC As Long, _
      tR As RECT, _
      ByVal iIndex As Long, _
      pc As pcExplorerBar _
   )
On Error Resume Next
Dim lhBmpOld As Long
Dim bf As BLENDFUNCTION
Dim lBlend As Long
Dim tBmp As BITMAP

   ' 2003-07-05: Added ability to draw non-expanding headers
   If ((m_bUseExplorerTheme) And (pc.CanExpand)) Then

      ' This part of the routine is only called under
      ' XP

      GetObjectAPI m_hDib(iIndex), Len(tBmp), tBmp
      lhBmpOld = SelectObject(m_hDC, m_hDib(iIndex))
      If (tBmp.bmBitsPixel < 32) Then
         TransparentBlt lHDC, _
            tR.left, tR.Top, tR.right - tR.left, tR.bottom - tR.Top, _
            m_hDC, _
            0, 0, tBmp.bmWidth, tBmp.bmHeight, _
            GetPixelAPI(m_hDC, 0, 0)
      Else
         bf.BlendOp = AC_SRC_OVER
         bf.BlendFlags = 0
         bf.SourceConstantAlpha = 255
         bf.AlphaFormat = AC_SRC_ALPHA
         CopyMemory lBlend, bf, 4
         
         AlphaBlend lHDC, _
            tR.left, tR.Top, tR.right - tR.left, tR.bottom - tR.Top, _
            m_hDC, _
            0, 0, tBmp.bmWidth, tBmp.bmHeight, _
            lBlend
      End If
      SelectObject m_hDC, lhBmpOld
   
   Else
      
      Dim cMemDC As New pcAlphaDibSection
      Dim hBr As Long
      Dim hPen As Long
      Dim hPenOld As Long
      Dim tJunk As POINTAPI
      Dim tWorkR As RECT
      Dim tBackR As RECT
      Dim lColorStart As Long
      Dim lColorEnd As Long
      
      LSet tWorkR = tR
      OffsetRect tWorkR, -tWorkR.left, -tWorkR.Top
      cMemDC.Create tR.right - tR.left, tR.bottom - tR.Top
            
      Dim tbm As BITMAP
      GetObjectAPI m_hDib(0), Len(tbm), tbm
      
      If (pc.CanExpand) Then
         Select Case iIndex
         Case 8 ' special background
            If (pc.TitleBackColorDark = CLR_INVALID) Then
               lColorStart = vbActiveTitleBar
            Else
               lColorStart = pc.TitleBackColorDark
            End If
            If (pc.TitleBackColorLight = CLR_INVALID) Then
               lColorEnd = GetSysColor(COLOR_GRADIENTACTIVECAPTION)
            Else
               lColorEnd = pc.TitleBackColorLight
            End If
            
         Case 9 ' normal background
            If (pc.TitleBackColorDark = CLR_INVALID) Then
               lColorStart = vbWindowBackground
            Else
               lColorStart = pc.TitleBackColorDark
            End If
            If (pc.TitleBackColorLight = CLR_INVALID) Then
               lColorEnd = GetSysColor(COLOR_GRADIENTINACTIVECAPTION)
            Else
               lColorEnd = pc.TitleBackColorLight
            End If
         
         End Select
         
         LSet tBackR = tWorkR
         tBackR.right = tWorkR.right - 2 - tbm.bmWidth
         
         ' MAH
         If GetIsXP Or GetIs2000With256 Then
           GradientFillRect cMemDC.hdc, tBackR, lColorStart, lColorEnd, GRADIENT_FILL_RECT_H
        Else
           GradientFillRect cMemDC.hdc, tBackR, RGB(255, 255, 255), RGB(198, 211, 247), GRADIENT_FILL_RECT_H
         End If
         ' MAH
         
         ' MAH
         If GetIsXP Or GetIs2000With256 Then
           hBr = CreateSolidBrush(TranslateColor(lColorEnd))
         Else
           hBr = CreateSolidBrush(TranslateColor(RGB(198, 211, 247)))
         End If
         ' MAH
         tBackR.left = tBackR.right
         tBackR.right = tWorkR.right
         FillRect cMemDC.hdc, tBackR, hBr
         DeleteObject hBr
      
      Else
         ' 2003-07-05: Draw top of panel when it can't be expanded:
         If (pc.PanelBackColor = -1) Then
            hBr = CreateSolidBrush(TranslateColor(DefaultPanelColor(pc.IsSpecial)))
         Else
            hBr = CreateSolidBrush(TranslateColor(pc.PanelBackColor))
         End If
         FillRect cMemDC.hdc, tWorkR, hBr
         DeleteObject hBr
         
         hPen = CreatePen(PS_SOLID, 1, &HFFFFFF)
         hPenOld = SelectObject(cMemDC.hdc, hPen)
         
         MoveToEx cMemDC.hdc, tR.left, tR.bottom, tJunk
         LineTo cMemDC.hdc, tR.left, tR.Top - 2
         SetPixel cMemDC.hdc, tR.left + 1, tR.Top - 1, &HFFFFFF
         MoveToEx cMemDC.hdc, tR.right - 1, tR.bottom, tJunk
         LineTo cMemDC.hdc, tR.right - 1, tR.Top - 2
         SetPixel cMemDC.hdc, tR.right - 2, tR.Top - 1, &HFFFFFF
         MoveToEx cMemDC.hdc, tR.left + 2, tR.Top, tJunk
         LineTo cMemDC.hdc, tR.right - 3, tR.Top
         
         SelectObject cMemDC.hdc, hPenOld
         DeleteObject hPen
      
      End If
      
      ' 2003-07-05: TransparentBlt leaked GDI under Win98, also
      ' it isn't supported on Win95 or NT4. Replaced
      ' with a sequence of Blts instead:
      cMemDC.PaintPicture lHDC, tR.left + 2, tR.Top, _
         tWorkR.right - tWorkR.left - 4, 1, 2
      cMemDC.PaintPicture lHDC, tR.left + 1, tR.Top + 1, _
         tWorkR.right - tWorkR.left - 2, 1, 1, 1
      cMemDC.PaintPicture lHDC, tR.left, tR.Top + 2, _
         , tWorkR.bottom - tWorkR.Top - 2, 0, 2
      
      'SetPixel cMemDC.hdc, tWorkR.left, tWorkR.Top, &HF1FE02
      'SetPixel cMemDC.hdc, tWorkR.left + 1, tWorkR.Top, &HF1FE02
      'SetPixel cMemDC.hdc, tWorkR.left, tWorkR.Top + 1, &HF1FE02
      'SetPixel cMemDC.hdc, tWorkR.right - 1, tWorkR.Top, &HF1FE02
      'SetPixel cMemDC.hdc, tWorkR.right - 1, tWorkR.Top + 1, &HF1FE02
      'SetPixel cMemDC.hdc, tWorkR.right - 2, tWorkR.Top, &HF1FE02
      '
      'TransparentBlt lHDC, tR.left, tR.Top, tWorkR.right - tWorkR.left, tWorkR.bottom - tWorkR.Top, _
      '   cMemDC.hdc, 0, 0, tWorkR.right - tWorkR.left, tWorkR.bottom - tWorkR.Top, _
      '   &HF1FE02
   
   End If
   
End Sub
      

Private Sub pMeasure()
On Error Resume Next
Dim pc As pcExplorerBar
Dim hTheme As Long
Dim lHDC As Long
Dim tR As RECT
Dim tTextR As RECT
Dim tSize As SIZEAPI
Dim lTop As Long
Dim hRes As Long
Dim bReEval As Boolean
Dim iItem As Long
Dim iItemTop As Long
Dim itm As pcExplorerBarItem
Dim hFont As Long
Dim hFontOld As Long
Dim bFirstBar As Boolean
Static s_lastWidth As Long
Dim bResized As Boolean
Dim lStart As Long
Dim lMaxBarHeight As Long
Dim sMeasureText As String
Dim lMeasureStyle As Long
Dim tBmp As BITMAP
   
   If Not m_colBars Is Nothing Then
      
      hTheme = plGetTheme()
         
      lHDC = UserControl.hdc
      GetClientRect UserControl.hwnd, tR
      
      Do ' Maximum number of loops around is 2; occurs when scroll bar visible changes
         bFirstBar = True
         lTop = tR.Top
         lStart = lTop
         For Each pc In m_colBars
            If (bFirstBar Or m_eStyle = eDefaultStyle) Then
               lTop = lTop + m_lBarSpacing
               bFirstBar = False
            End If
            pc.Top = lTop
            
            If (pc.CanExpand) Or Len(pc.Title) > 0 Then
               If (m_cScrollBar.Visible(efsVertical)) Then
                  lTop = lTop + pc.TitleHeightWithScroll
               Else
                  lTop = lTop + pc.TitleHeightWithoutScroll
               End If
            Else
               ' don't need to add the title height
               lTop = lTop + 16
            End If
            
            If (pc.Expanding Or pc.Collapsing) Then
               lTop = lTop + pc.Height
            ElseIf (pc.State = eBarExpanded) Then
               If (m_cScrollBar.Visible(efsVertical)) Then
                  lTop = lTop + pc.HeightWithScroll
               Else
                  lTop = lTop + pc.HeightWithoutScroll
               End If
            End If
            
            iItemTop = 6
            For iItem = 1 To pc.ItemCount
               Set itm = pc.Item(iItem)
               itm.Top = iItemTop
               If (m_cScrollBar.Visible(efsVertical)) Then
                  iItemTop = iItemTop + itm.HeightWithScroll + itm.SpacingAfter
               Else
                  iItemTop = iItemTop + itm.HeightWithoutScroll + itm.SpacingAfter
               End If
            Next iItem
            
         Next
         lTop = lTop + m_lBarSpacing
         bReEval = False
         
         If (lTop - lStart > lMaxBarHeight) Then
            lMaxBarHeight = lTop - lStart
         End If
         
         If (lTop > tR.bottom - tR.Top) Then
            If Not (m_cScrollBar.Visible(efsVertical)) Then
               m_cScrollBar.Visible(efsVertical) = True
               pResizeContainedControls
               bResized = True
               bReEval = True
            End If
            m_cScrollBar.Max(efsVertical) = lTop - (tR.bottom - tR.Top)
            m_cScrollBar.LargeChange(efsVertical) = tR.bottom - tR.Top
         Else
            If (m_cScrollBar.Visible(efsVertical)) Then
               m_cScrollBar.Visible(efsVertical) = False
               pResizeContainedControls
               bResized = True
               bReEval = True
            End If
         End If
      Loop While bReEval
      
      
      If Not (hTheme = 0) Then
         CloseThemeData hTheme
      End If
      
      If Not ((tR.right - tR.left) = s_lastWidth) Then
         s_lastWidth = tR.right - tR.left
         If Not bResized Then
            pResizeContainedControls
         End If
      End If
      
   End If
   
   If (lMaxBarHeight > m_cDibFade.Height) Then
      m_cDibFade.Create m_cDibFade.Width, lMaxBarHeight
   End If
   
End Sub

Private Sub pResizeContainedControls()
On Error Resume Next
Dim pc As pcExplorerBar
Dim itm As pcExplorerBarItem
Dim iItem As Long

   For Each pc In m_colBars
      For iItem = 1 To pc.ItemCount
         Set itm = pc.Item(iItem)
         If (itm.ItemType = eItemControlPlaceHolder) Then
            pResizeContainedControl pc, itm
         End If
      Next iItem
   Next
End Sub
Private Sub pResizeContainedControl( _
      pc As pcExplorerBar, _
      itm As pcExplorerBarItem _
   )
   On Error Resume Next
Dim tR As RECT
Dim lOffset As Long
Dim lTitleHeight As Long
   If Not (itm.lPtrPanel = 0) Then
      If (m_cScrollBar.Visible(efsVertical)) Then
         lOffset = -m_cScrollBar.Value(efsVertical)
         lTitleHeight = pc.TitleHeightWithScroll
      Else
         lTitleHeight = pc.TitleHeightWithoutScroll
      End If
      GetClientRect UserControl.hwnd, tR
      Dim ctl As Control
      Set ctl = ObjectFromPtr(itm.lPtrPanel)
      ctl.Move _
         UserControl.ScaleX(m_lMargin + m_lMargin, vbPixels, UserControl.ScaleMode), _
         UserControl.ScaleY(lOffset + pc.Top + itm.Top + lTitleHeight, vbPixels, UserControl.ScaleMode), _
         UserControl.ScaleX(tR.right - tR.left - m_lMargin * 4, vbPixels, UserControl.ScaleMode)
      ctl.Visible = (pc.State = eBarExpanded) Or (pc.Expanding)
   End If
   
End Sub

Private Function pbHitTest( _
      ByRef pc As pcExplorerBar, _
      pcSel As pcExplorerBar, _
      itmSel As pcExplorerBarItem _
   ) As Boolean
   On Error Resume Next
Dim lOffset As Long
Dim tP As POINTAPI
Dim iItem As Long
Dim tR As RECT
Dim tTestR As RECT
Dim itm As pcExplorerBarItem
Dim lItemHeight As Long

   ' Are we inside this bar?
   If (m_cScrollBar.Visible(efsVertical)) Then
      lOffset = -m_cScrollBar.Value(efsVertical)
   End If
   
   GetCursorPos tP
   ScreenToClient UserControl.hwnd, tP
   GetClientRect UserControl.hwnd, tR
   
   tR.Top = pc.Top + lOffset
   If (pc.CanExpand) Or (Len(pc.Title) > 0) Then
      If (m_cScrollBar.Visible(efsVertical)) Then
         tR.bottom = tR.Top + pc.TitleHeightWithScroll
      Else
         tR.bottom = tR.Top + pc.TitleHeightWithoutScroll
      End If
   Else
      tR.bottom = tR.Top
   End If
   tR.left = tR.left + m_lMargin
   tR.right = tR.right - m_lMargin
   
   If Not (PtInRect(tR, tP.x, tP.y) = 0) Then
      ' inside the bar title:
      Set pcSel = pc
      pbHitTest = True
   Else
      If (pc.State = eBarExpanded) Then
         ' check all the items within the bar:
         tR.Top = tR.bottom
         For iItem = 1 To pc.ItemCount
            LSet tTestR = tR
            Set itm = pc.Item(iItem)
            If (itm.ItemType = eItemLink) And (itm.CanClick) Then
               tTestR.Top = tTestR.Top + itm.Top
               If (m_cScrollBar.Visible(efsVertical)) Then
                  lItemHeight = itm.HeightWithScroll
               Else
                  lItemHeight = itm.HeightWithoutScroll
               End If
               tTestR.bottom = tTestR.Top + lItemHeight
               If Not (PtInRect(tTestR, tP.x, tP.y) = 0) Then
                  Set itmSel = itm
                  pbHitTest = True
               End If
            End If
         Next iItem
      End If
   End If
      
End Function

Private Sub pSetMousePointer(ByVal ePointer As MousePointerConstants)
On Error Resume Next
   If Not (ePointer = m_ePointer) Then
      UserControl.MousePointer = ePointer
      m_ePointer = ePointer
   End If
End Sub

Private Sub pSetToolTipText(ByVal sToolTip As String)
On Error Resume Next
   'Debug.Print sToolTip, m_sToolTip
   If Not (StrComp(m_sToolTip, sToolTip) = 0) Then
      'Debug.Print StrComp(m_sToolTip, sToolTip)
      UserControl.Extender.ToolTipText = sToolTip
      m_sToolTip = sToolTip
   End If
End Sub

Private Sub OnBarRightClick(pc As pcExplorerBar)
   On Error Resume Next
   
   Dim cB As New cExplorerBar
   cB.fInit UserControl.hwnd, pc.ID
   RaiseEvent BarRightClick(cB)
End Sub
Private Sub OnBarClick(pc As pcExplorerBar)
   On Error Resume Next
   
   ' Select the bar:
   If Not (m_lIdSelBar = pc.ID) Then
      m_lIdSelItem = 0
      m_lIdSelBar = pc.ID
      pPaint
      UserControl.Refresh
   End If
   
   ' Raise the event:
   Dim cB1 As New cExplorerBar
   cB1.fInit UserControl.hwnd, pc.ID
   RaiseEvent BeforeBarClick(cB1)
   
   ' Is this an expandable bar?
   If (pc.CanExpand) Then
      If (pc.State = eBarCollapsed) Then
         ' we want to expand the bar:
         fExpandBar pc, 1
      Else
         ' we want to collapse the bar:
         fExpandBar pc, -1
      End If
   End If

   ' Raise the event:
   Dim cB As New cExplorerBar
   cB.fInit UserControl.hwnd, pc.ID
   RaiseEvent BarClick(cB)
   
End Sub
Private Sub OnItemRightClick(itm As pcExplorerBarItem)
   On Error Resume Next

   ' Raise the event
   Dim cI As New cExplorerBarItem
   cI.fInit UserControl.hwnd, itm.BarID, itm.ID
   RaiseEvent ItemRightClick(cI)

End Sub
Private Sub OnItemClick(itm As pcExplorerBarItem)
   On Error Resume Next
   
   ' Select the item:
   If Not (m_lIdSelItem = itm.ID) Then
      m_lIdSelItem = itm.ID
      m_lIdSelBar = 0
      pPaint
      UserControl.Refresh
   End If
   
   ' Raise the event
   Dim cI As New cExplorerBarItem
   cI.fInit UserControl.hwnd, itm.BarID, itm.ID
   RaiseEvent ItemClick(cI)
   
End Sub
Private Sub OnHighlight(pc As pcExplorerBar, itm As pcExplorerBarItem)
On Error Resume Next
Static pcLast As pcExplorerBar
Static itmLast As pcExplorerBarItem
Dim cBar As cExplorerBar
Dim cItem As cExplorerBarItem
   
   If Not (itm Is Nothing) Then
      Set pc = m_colBars("C:" & itm.BarID)
      Set cItem = New cExplorerBarItem
      cItem.fInit UserControl.hwnd, pc.ID, itm.ID
   End If
   If Not (pc Is Nothing) Then
      Set cBar = New cExplorerBar
      cBar.fInit UserControl.hwnd, pc.ID
   End If

   If (pcLast Is Nothing) Then
      If Not (pc Is Nothing) Then
         ' Highlight change:
         RaiseEvent Highlight(cBar, cItem)
      End If
   Else
      ' Last item not nothing:
      If (pc Is Nothing) Then
         ' Nothing highlighted:
         RaiseEvent Highlight(Nothing, Nothing)
      Else
         ' Is this the same bar?
         If Not (pc Is pcLast) Then
            ' no
            RaiseEvent Highlight(cBar, cItem)
         Else
            ' same bar, may be a different item:
            If (itmLast Is Nothing) Then
               If Not (itm Is Nothing) Then
                  RaiseEvent Highlight(cBar, cItem)
               End If
            Else
               If Not (itmLast Is itm) Then
                  RaiseEvent Highlight(cBar, cItem)
               End If
            End If
         End If
      End If
   End If
         
   Set pcLast = pc
   Set itmLast = itm

End Sub

Public Function plGetTheme() As Long
On Error Resume Next
   If (IsXp And m_bUseExplorerTheme) Then
      ' Check if there's a theme currently in effect for this:
      On Error Resume Next
      plGetTheme = OpenThemeData(UserControl.hwnd, StrPtr("ExplorerBar"))
   End If
End Function

Private Function pbColouriseWatermarks() As Boolean
   On Error Resume Next
   Dim pc As pcExplorerBar

   If Not m_colBars Is Nothing Then
      
      For Each pc In m_colBars
         pc.ColouriseWatermark Me
      Next
      
   End If
   
End Function

Private Function pbCreateBitmapWorkDC() As Boolean
On Error Resume Next
Dim lhDCD As Long
Dim lhWndD As Long

   If Not (m_hDC = 0) Then
      DeleteDC m_hDC
   End If

   lhWndD = GetDesktopWindow()
   lhDCD = GetDC(lhWndD)
   m_hDC = CreateCompatibleDC(lhDCD)
   ReleaseDC lhWndD, lhDCD ' 2003-07-05: Corrected for GDI leak in Win98
   
   pbCreateBitmapWorkDC = (m_hDC <> 0)
   
End Function

Private Function pbLoadShellStyleBitmaps() As Boolean
On Error Resume Next
Dim hTheme As Long
Dim hRes As Long
Dim iPos As Long
Dim lPtrColorName As Long
Dim lPtrThemeFile As Long
Dim sThemeFile As String
Dim sColorName As String
Dim sShellStyle As String
Dim hLib As Long
Dim i As Long
Dim bFail As Boolean
Dim lhBmpOld As Long
Dim tR As RECT

   For i = 0 To 10
      If Not (m_hDib(i) = 0) Then
         DeleteObject m_hDib(i)
      End If
   Next i

   hTheme = plGetTheme()
   
   If Not (hTheme = 0) Then
   
      ReDim bThemeFile(0 To 260 * 2) As Byte
      lPtrThemeFile = VarPtr(bThemeFile(0))
      ReDim bColorName(0 To 260 * 2) As Byte
      lPtrColorName = VarPtr(bColorName(0))
      hRes = GetCurrentThemeName(lPtrThemeFile, 260, lPtrColorName, 260, 0, 0)
      
      sThemeFile = bThemeFile
      iPos = InStr(sThemeFile, vbNullChar)
      If (iPos > 1) Then sThemeFile = left(sThemeFile, iPos - 1)
      sColorName = bColorName
      iPos = InStr(sColorName, vbNullChar)
      If (iPos > 1) Then sColorName = left(sColorName, iPos - 1)
      
      sShellStyle = sThemeFile
      For iPos = Len(sThemeFile) To 1 Step -1
         If (Mid(sThemeFile, iPos, 1) = "\") Then
            sShellStyle = left(sThemeFile, iPos)
            Exit For
         End If
      Next iPos
      sShellStyle = sShellStyle & "Shell\" & sColorName & "\shellstyle.dll"
      If (FileExists(sShellStyle)) Then
         hLib = LoadLibraryEx(sShellStyle, 0, LOAD_LIBRARY_AS_DATAFILE)
         If Not (hLib = 0) Then
            
            m_hDib(0) = LoadImageLong(hLib, 100, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION)
            m_hDib(1) = LoadImageLong(hLib, 101, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION)
            m_hDib(2) = LoadImageLong(hLib, 102, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION)
            m_hDib(3) = LoadImageLong(hLib, 103, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION)
            m_hDib(4) = LoadImageLong(hLib, 104, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION)
            m_hDib(5) = LoadImageLong(hLib, 105, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION)
            m_hDib(6) = LoadImageLong(hLib, 106, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION)
            m_hDib(7) = LoadImageLong(hLib, 107, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION)
            m_hDib(8) = LoadImageLong(hLib, 110, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION)
            m_hDib(9) = LoadImageLong(hLib, 112, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION)
            m_hDib(10) = LoadImageLong(hLib, 14, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION)
            
            For i = 0 To 10
               If (m_hDib(i) = 0) Then
                  bFail = True
               End If
            Next i
            
            FreeLibrary hLib
            
            If Not bFail Then
               lhBmpOld = SelectObject(m_hDC, m_hDib(10))
               m_lThemePanelColor = GetPixelAPI(m_hDC, 0, 0)
               SelectObject m_hDC, lhBmpOld
            End If
            
            pbLoadShellStyleBitmaps = Not (bFail)
            
         End If
      End If
      
      CloseThemeData hTheme
      
   Else
      m_lThemePanelColor = BlendColor(vbButtonFace, &HFFFFFF, 24)

      ' Backgrounds (i.e. m_hDib(8) and m_hDib(9)) are drawn on the fly
      ' so no need to define them.
      
      ' Remember that the colours of the icons may change if the user
      ' customises the bar title colours later on, so a separate fn
      ' is used.
      pLoadColourisedFakeShellStyleBitmaps
            
      pbLoadShellStyleBitmaps = True
   End If
      
End Function

Private Sub pLoadColourisedFakeShellStyleBitmaps()
On Error Resume Next
Dim lColorDarkSpecial As Long
Dim lColorDarkNormal As Long
Dim pc As pcExplorerBar
   
   ' Determine the titlebar dark colours:
   lColorDarkSpecial = CLR_INVALID
   lColorDarkNormal = CLR_INVALID
   For Each pc In m_colBars
      If (pc.IsSpecial) And (lColorDarkSpecial = CLR_INVALID) Then
         If (pc.TitleBackColorDark = CLR_INVALID) Then
            lColorDarkSpecial = GetSysColor(COLOR_GRADIENTACTIVECAPTION)
         Else
            lColorDarkSpecial = pc.TitleBackColorLight
         End If
      ElseIf (lColorDarkNormal = CLR_INVALID) Then
         If (pc.TitleBackColorDark = CLR_INVALID) Then
            lColorDarkNormal = GetSysColor(COLOR_GRADIENTINACTIVECAPTION)
         Else
            lColorDarkNormal = pc.TitleBackColorLight
         End If
      End If
   Next
   If (lColorDarkSpecial = CLR_INVALID) Then
      lColorDarkSpecial = GetSysColor(COLOR_GRADIENTACTIVECAPTION)
   End If
   If (lColorDarkNormal = CLR_INVALID) Then
      lColorDarkNormal = GetSysColor(COLOR_GRADIENTINACTIVECAPTION)
   End If
   
   ' MAH
   If Me.Style = eSearchStyle And plGetTheme = 0 And Not (GetIsXP Or GetIs2000With256) Then
     lColorDarkNormal = RGB(227, 230, 236)
   End If
   If Me.Style = eDefaultStyle And plGetTheme = 0 And Not (GetIsXP Or GetIs2000With256) Then
     lColorDarkNormal = RGB(199, 211, 247)
   End If
   ' MAH
   
   ' Now create some bitmaps:
   
   Dim cGlyph As New pcAlphaDibSection
   Dim sPic As StdPicture
   
   ' Normal Collapse:
   Set sPic = LoadResPicture(101, vbResBitmap)
   cGlyph.CreateFromPicture sPic
   ColouriseGlyph cGlyph, lColorDarkNormal
   DeleteObject m_hDib(0)
   m_hDib(0) = cGlyph.ExtractDib
   m_hDib(1) = m_hDib(0)
   
   ' Normal Expand:
   Set sPic = LoadResPicture(102, vbResBitmap)
   cGlyph.CreateFromPicture sPic
   ColouriseGlyph cGlyph, lColorDarkNormal
   DeleteObject m_hDib(2)
   m_hDib(2) = cGlyph.ExtractDib
   m_hDib(3) = m_hDib(2)
   
   ' Special Collapse:
   Set sPic = LoadResPicture(103, vbResBitmap)
   cGlyph.CreateFromPicture sPic
   ColouriseGlyph cGlyph, lColorDarkSpecial
   DeleteObject m_hDib(4)
   m_hDib(4) = cGlyph.ExtractDib
   m_hDib(5) = m_hDib(4)
   
   ' Special Expand:
   Set sPic = LoadResPicture(104, vbResBitmap)
   cGlyph.CreateFromPicture sPic
   ColouriseGlyph cGlyph, lColorDarkSpecial
   DeleteObject m_hDib(6)
   m_hDib(6) = cGlyph.ExtractDib
   m_hDib(7) = m_hDib(6)
      
End Sub

Private Sub pInitialise()
On Error Resume Next
   '
   m_bRunTime = UserControl.Ambient.UserMode
   If (m_bRunTime) Then
      
      VerInitialise
      
      ' Get the IOLEControl interface of the control
      Dim IOleCtl As IOleControl
      Set IOleCtl = Me
      ' Replace IOLEControl methods:
      m_ptrGetControlInfoOrig = ReplaceVTableEntry( _
         ObjPtr(IOleCtl), _
         IDX_GetControlInfo, _
         AddressOf mIOleControl.IOleControl_GetControlInfo, _
         ObjPtr(Me) _
         )
      m_ptrOnMnemonicOrig = ReplaceVTableEntry( _
         ObjPtr(IOleCtl), _
         IDX_OnMnemonic, _
         AddressOf mIOleControl.IOleControl_OnMnemonic, _
         ObjPtr(Me) _
         )
         
      ' Create object to manage Mnemonics for this control:
      Set m_cMnemonics = New pcMnemonics
      
      ' For tab trapping & settings changes:
      m_hWnd = UserControl.hwnd
      Dim lhWnd As Long
      Dim sBuf As String
      lhWnd = m_hWnd
      Do
         lhWnd = GetParent(lhWnd)
         If Not (lhWnd = 0) Then
            'sBuf = String$(255, 0)
            'GetClassName lhWnd, sBuf, 255
            'If (InStr(sBuf, "Main") = 0) Then ' ThunderMain window
               m_hWndContainer = lhWnd
            'Else
            '   lhWnd = 0
            'End If
         End If
      Loop While Not (lhWnd = 0)
      
      AttachMessage Me, m_hWnd, WM_SETFOCUS
      AttachMessage Me, m_hWndContainer, WM_SETTINGCHANGE
      
      Set m_cScrollBar = New pcScrollBars
      m_cScrollBar.Create UserControl.hwnd
      m_cScrollBar.Orientation = efsoVertical
      m_cScrollBar.Visible(efsVertical) = False
      m_cScrollBar.Visible(efsHorizontal) = False
      m_cScrollBar.SmallChange(efsVertical) = 12
      
      Set m_tmr = New CTimer
      
      SetProp UserControl.hwnd, "VBALEXPLORERBARCTL", ObjPtr(Me)
      
      Set m_colBars = New Collection
      Set m_colBarKeys = New Collection
      Set m_colItems = New Collection
      
      ' Load theme if required:
      pbCreateBitmapWorkDC
      If (IsXp And m_bUseExplorerTheme) Or (Not (m_bUseExplorerTheme)) Then
         If Not (pbLoadShellStyleBitmaps()) Then
            ' If we can't load the shell style DLL then we're stuffed
            m_bUseExplorerTheme = False
            pbLoadShellStyleBitmaps
         End If
      End If
         
      Set m_cNCM = New pcNCMetrics
      m_cNCM.GetMetrics
      
      Set UserControl.MouseIcon = LoadResPicture("HAND", vbResCursor)
      
   End If
   '
End Sub
Private Sub pTerminate()
   On Error Resume Next
   
   '
   ' Perform any clear up we need here
   RemoveProp UserControl.hwnd, "VBALEXPLORERBARCTL"
   Set m_cScrollBar = Nothing
   Set m_cDibFade = Nothing
   
   Dim i As Long
   For i = 0 To 10
      If Not (m_hDib(i) = 0) Then
         DeleteObject m_hDib(i)
         m_hDib(i) = 0
      End If
   Next i
   
   If Not (m_hDC = 0) Then
      DeleteDC m_hDC
      m_hDC = 0
   End If
   
   If Not (m_ptrGetControlInfoOrig = 0) Then
   
      ' Get the IOLEControl interface of the control
      Dim IOleCtl As IOleControl
      Set IOleCtl = Me
      ' Restore IOleControl methods:
      ReplaceVTableEntry _
         ObjPtr(IOleCtl), _
         IDX_GetControlInfo, _
         m_ptrGetControlInfoOrig
      m_ptrGetControlInfoOrig = 0
   End If
   
   If Not (m_ptrOnMnemonicOrig = 0) Then
      ReplaceVTableEntry _
         ObjPtr(IOleCtl), _
         IDX_OnMnemonic, _
         m_ptrOnMnemonicOrig
      m_ptrOnMnemonicOrig = 0
   End If
      
   If Not (m_hWnd = 0) Then
      DetachMessage Me, m_hWnd, WM_SETFOCUS
      DetachMessage Me, m_hWndContainer, WM_SETTINGCHANGE
      m_hWnd = 0
      m_hWndContainer = 0
   End If
   
   '
End Sub

Private Function piKeyStringToKeyCode(ByVal sKey As String) As Integer
   On Error Resume Next

Dim b() As Byte
Dim vKey As Integer
   
   If (GetVersion() And &H80000000) = 0 Then
      ' NT
      b = sKey
      CopyMemory vKey, b(0), 2
      vKey = VkKeyScanW(vKey)
   Else
      ' 9x
      b = StrConv(sKey, vbFromUnicode)
      vKey = VkKeyScan(b(0))
   End If
   piKeyStringToKeyCode = vKey And &HFF&
   
End Function
Private Sub pFireMnemonic(ByVal sKey As String)
On Error Resume Next
Dim bClicked As Boolean

   ' Check all the bars:
   Dim pc As pcExplorerBar
   For Each pc In m_colBars
      If (pc.HasMnemonic(sKey)) Then
         OnBarClick pc
         bClicked = True
         Exit For
      End If
   Next
   
   If Not (bClicked) Then
   
      ' Check all the items:
      Dim itm As pcExplorerBarItem
      For Each itm In m_colItems
         If (itm.HasMnemonic(sKey)) Then
            OnItemClick itm
            Exit For
         End If
      Next
      
   End If
   
End Sub
Private Sub pFindNextItem( _
      ByRef lIDBar As Long, _
      ByRef lIDItem As Long, _
      ByVal iDir As Long, _
      ByVal bTab As Boolean _
   )
   On Error Resume Next

Dim pc As pcExplorerBar
Dim itm As pcExplorerBarItem
Dim i As Long, j As Long
Dim bFound As Boolean
Dim lBarIndex As Long
Dim lBarStart As Long
Dim lItemIndex As Long
Dim lItemStart As Long
Dim lItemEnd As Long
Dim bFirst As Boolean
   
   lIDBar = 0
   lIDItem = 0
   
   If (m_lIdSelBar > 0) Then
      If Not (fVerifyId(m_lIdSelBar, 1)) Then
         m_lIdSelBar = 0
      End If
   End If
   If (m_lIdSelItem > 0) Then
      If Not (fVerifyId(m_lIdSelItem, 2)) Then
         m_lIdSelItem = 0
      End If
   End If
   
   If (m_lIdSelBar = 0) And (m_lIdSelItem = 0) Then
      ' nothing selected:
      If (iDir < 0) Then
         
         ' pick the last accessible item in the control
         For i = m_colBars.Count To 1 Step -1
            Set pc = m_colBars(i)
            If (pc.State = eBarExpanded) Then
               For j = pc.ItemCount To 1 Step -1
                  Set itm = pc.Item(j)
                  If (itm.CanClick And itm.ItemType = eItemLink) Or (itm.lPtrPanel > 0) Then
                     lIDItem = itm.ID
                     bFound = True
                     Exit For
                  End If
               Next j
            End If
            If (bFound) Then
               Exit For
            Else
               If (pc.CanExpand) Then
                  lIDBar = pc.ID
                  Exit For
               End If
            End If
         Next i
         
      Else
         
         ' pick the first accessible item in the control
         For i = 1 To m_colBars.Count
            Set pc = m_colBars(i)
            If (pc.CanExpand) Then
               lIDBar = pc.ID
               Exit For
            Else
               If (pc.State = eBarExpanded) Then
                  For j = 1 To pc.ItemCount
                     Set itm = pc.Item(j)
                     If (itm.CanClick And itm.ItemType = eItemLink) Or (itm.lPtrPanel > 0) Then
                        lIDItem = itm.ID
                        bFound = True
                        Exit For
                     End If
                  Next j
               End If
            End If
            If (bFound) Then
               Exit For
            End If
         Next i
      End If
   
   Else
      
      ' First find the selected item:
      For i = m_colBars.Count To 1 Step -1
         Set pc = m_colBars(i)
         lBarIndex = i
         For j = pc.ItemCount To 1 Step -1
            Set itm = pc.Item(j)
            If (itm.ID = m_lIdSelItem) Then
               lItemIndex = j
               bFound = True
               Exit For
            End If
         Next j
         If (bFound) Or (pc.ID = m_lIdSelBar) Then
            Exit For
         End If
      Next i
      bFound = False
      bFirst = True
      
      
      ' Now determine the next item to be
      ' selected:
      If (iDir < 0) Then
         
         ' Looking for the prior item:
         If (lItemIndex > 0) Then
            lBarStart = lBarIndex
         Else
            lBarStart = lBarIndex - 1
         End If
         
         For i = lBarStart To 1 Step -1
            
            Set pc = m_colBars(i)
            If (pc.State = eBarExpanded) Then
            
               lItemStart = pc.ItemCount
               If (bFirst) Then
                  If (lItemIndex > 0) Then
                     lItemStart = lItemIndex - 1
                     If (bTab) Then
                        lItemStart = 0
                     End If
                  End If
               End If
               
               If (lItemStart > 0) Then
                  For j = lItemStart To 1 Step -1
                     Set itm = pc.Item(j)
                     If (itm.CanClick And itm.ItemType = eItemLink) Or (itm.lPtrPanel > 0) Then
                        lIDItem = itm.ID
                        bFound = True
                        Exit For
                     End If
                  Next j
               End If
            End If
            
            If (bFound) Then
               Exit For
            Else
               If (pc.CanExpand) Then
                  lIDBar = pc.ID
                  Exit For
               End If
            End If
            bFirst = False
            
         Next i
         
      Else
         
         ' Looking for the next item:
         lBarStart = lBarIndex
         
         ' Check if the next thing to select
         ' is the next bar along:
         Set pc = m_colBars(lBarStart)
         If (bTab) Or (lItemIndex = pc.ItemCount) Or (pc.State = eBarCollapsed) Then
            ' next item is a bar:
            If (lBarStart < m_colBars.Count) Then
               Set pc = m_colBars(lBarStart + 1)
               lIDBar = pc.ID
               bFound = True
            End If
         End If
         
         If Not (bFound) Then
                     
            For i = lBarStart To m_colBars.Count
               
               Set pc = m_colBars(i)
               
               lItemStart = 1
               If (bFirst) Then
               
                  If (lItemIndex > 0) Then
                     lItemStart = lItemIndex + 1
                     If (bTab) Then
                        lItemStart = pc.ItemCount + 1
                     End If
                  End If
               End If
               
               If (lItemStart <= pc.ItemCount) Then
                  If (pc.State = eBarExpanded) Then
                     For j = lItemStart To pc.ItemCount
                        Set itm = pc.Item(j)
                        If (itm.CanClick And itm.ItemType = eItemLink) Or (itm.lPtrPanel > 0) Then
                           lIDItem = itm.ID
                           bFound = True
                           Exit For
                        End If
                     Next j
                  End If
               End If
            
               If (bFound) Then
                  Exit For
               Else
                  If Not (bFirst) Or (lItemIndex = 0) Then
                     If (pc.CanExpand) Then
                        lIDBar = pc.ID
                        Exit For
                     End If
                  End If
               End If
               bFirst = False
               
            Next i
         
         End If
         
      End If
   
   End If
   
   If Not bTab Then
      If (lIDBar = 0) And (lIDItem = 0) Then
         lIDBar = m_lIdSelBar
         lIDItem = m_lIdSelItem
      End If
   Else
      If (lIDBar = m_lIdSelBar) And (lIDItem = m_lIdSelItem) Then
         lIDBar = 0
         lIDItem = 0
      End If
   End If
   
End Sub

Private Function pKeyDown(KeyCode As Integer, Shift As Integer) As Boolean
   On Error Resume Next

Dim lIDBar As Long
Dim lIDItem As Long
Dim iDir As Long
Dim bTab As Boolean
Dim bProcess As Boolean
Dim pc As pcExplorerBar
Dim i As Long
Dim lPageSize As Long
Dim tR As RECT
   
   If Not (m_colBars Is Nothing) Then
   If (m_colBars.Count > 0) Then
   
   ' Tab key processing:
   Select Case KeyCode
      Case vbKeyTab, vbKeyDown, vbKeyUp
         
         Select Case KeyCode
         Case vbKeyTab
            bTab = True
            If ((Shift And vbShiftMask) = vbShiftMask) Then
               iDir = -1
            Else
               iDir = 1
            End If
            bProcess = True
         Case vbKeyDown
            iDir = 1
            bProcess = True
         Case vbKeyUp
            iDir = -1
            bProcess = True
         End Select
         
         If (bProcess) Then
            pFindNextItem lIDBar, lIDItem, iDir, bTab
            m_lIdSelBar = lIDBar
            m_lIdSelItem = lIDItem
         End If
         
      Case vbKeyHome
         bProcess = True
         Set pc = m_colBars(1)
         m_lIdSelBar = pc.ID
         m_lIdSelItem = 0
      
      Case vbKeyEnd
         bProcess = True
         Set pc = m_colBars(m_colBars.Count)
         m_lIdSelBar = pc.ID
         m_lIdSelItem = 0
         If (pc.ItemCount > 0) Then
            If Not (pc.CanExpand) Or (pc.CanExpand And pc.State = eBarExpanded) Then
               For i = pc.ItemCount To 1 Step -1
                  If (pc.Item(i).CanClick) And (pc.Item(i).ItemType <> eItemText) Then
                     m_lIdSelItem = pc.Item(pc.ItemCount).ID
                     Exit For
                  End If
               Next i
            End If
         End If
      
      Case vbKeyPageDown, vbKeyPageUp
            
         GetClientRect m_hWnd, tR
         lPageSize = (tR.bottom - tR.Top) \ 2
         
         Select Case KeyCode
         Case vbKeyPageDown
            iDir = 1
         Case vbKeyPageUp
            iDir = -1
         End Select
         
         
            
      
   End Select
      
   If (bProcess) Then
      If (m_lIdSelBar = 0) And (m_lIdSelItem = 0) Then
         pKeyDown = False
      Else
         m_bHaveUsedKeys = True
         If (m_lIdSelItem > 0) Then
            Dim itm As pcExplorerBarItem
            Set itm = m_colItems("C:" & m_lIdSelItem)
            fEnsureItemVisible itm.BarID, m_lIdSelItem
            If (itm.lPtrPanel > 0) Then
               Dim ctl As Control
               Set ctl = ObjectFromPtr(itm.lPtrPanel)
               On Error Resume Next
               ctl.SetFocus
            End If
         ElseIf (m_lIdSelBar > 0) Then
            fEnsureBarVisible m_lIdSelBar
         End If
         pPaint
         UserControl.Refresh
         pKeyDown = True
      End If
      
   End If
   
   End If
   End If
   '
End Function
Private Function pKeyPress(KeyAscii As Integer) As Boolean
   On Error Resume Next

End Function
Private Function pKeyUp(KeyCode As Integer, Shift As Integer) As Boolean
   On Error Resume Next
   
   Select Case KeyCode
   Case vbKeySpace, vbKeyReturn
      ' like pressing an item:
      If (m_lIdSelItem > 0) Then
         Dim itm As pcExplorerBarItem
         Set itm = m_colItems("C:" & m_lIdSelItem)
         OnItemClick itm
         pKeyUp = True
      ElseIf (m_lIdSelBar > 0) Then
         Dim pc As pcExplorerBar
         Set pc = m_colBars("C:" & m_lIdSelBar)
         OnBarClick pc
         pKeyUp = True
      End If
   End Select
   
End Function


Friend Function GetControlInfo(pCI As CONTROLINFO) As Long
   On Error Resume Next
   
   Debug.Print "GetControlInfo"
   pCI.cB = LenB(pCI)
   pCI.cAccel = m_cMnemonics.Count
   pCI.hAccel = m_cMnemonics.hAccel
   pCI.dwFlags = 0
   
End Function
Friend Function OnMnemonic(pMsg As MSG) As Long
On Error Resume Next
Dim i As Long
   
   If (pMsg.Message = WM_SYSCHAR Or pMsg.Message = WM_SYSKEYDOWN) Then
      For i = 1 To m_cMnemonics.Count
         If (pMsg.wParam = m_cMnemonics.VirtKey(i)) Then
            pFireMnemonic m_cMnemonics.Key(i)
            Exit For
         End If
      Next i
   End If
   
End Function
Friend Function TranslateAccelerator(lpMsg As VBOleGuids.MSG) As Long
   On Error Resume Next

   TranslateAccelerator = S_FALSE
   ' Here you can modify the response to the key down
   ' accelerator command using the values in lpMsg.  This
   ' can be used to capture Tabs, Returns, Arrows etc.
   ' Just process the message as required and return S_OK.
   If (lpMsg.wParam And &HFFFF&) = vbKeyTab Then
      Select Case lpMsg.Message
      Case WM_KEYDOWN
         If (pKeyDown(vbKeyTab, ShiftState)) Then
            TranslateAccelerator = S_OK
         End If
      Case WM_KEYUP
         If (pKeyUp(vbKeyTab, ShiftState)) Then
            TranslateAccelerator = S_OK
         End If
      End Select
   End If
End Function
Private Property Get ShiftState() As Integer
   On Error Resume Next
   
   ' we don't need to consider Alt for a Tab key press.
   ShiftState = IIf(GetAsyncKeyState(vbKeyShift) = 0, 0, 1) * vbShiftMask Or IIf(GetAsyncKeyState(vbKeyControl) = 0, 0, 1) * vbCtrlMask
End Property

Private Property Let ISubclass_MsgResponse(ByVal RHS As EMsgResponse)
   On Error Resume Next
   '
End Property

Private Property Get ISubclass_MsgResponse() As EMsgResponse
   On Error Resume Next
   ISubclass_MsgResponse = emrPreprocess
End Property

Private Function ISubclass_WindowProc(ByVal hwnd As Long, ByVal iMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
On Error Resume Next
   Select Case iMsg
   Case WM_SETTINGCHANGE
      Debug.Print "WM_SETTINGCHANGE"
      
      ' Load theme if required:
      pbCreateBitmapWorkDC
      If (IsXp And m_bUseExplorerTheme) Or (Not (m_bUseExplorerTheme)) Then
         If Not (pbLoadShellStyleBitmaps()) Then
            ' If we can't load the shell style DLL then we're stuffed
            m_bUseExplorerTheme = False
            pbLoadShellStyleBitmaps
         End If
      End If
      pbColouriseWatermarks
      pPaint
      UserControl.Refresh
      RaiseEvent SettingChange
   
   ' --------------------------------------------------------------------------
   ' Required to allow Tab keys to be trapped:
   Case WM_SETFOCUS
      Dim pOleObject                  As VBOleGuids.IOleObject
      Dim pOleInPlaceSite             As VBOleGuids.IOleInPlaceSite
      Dim pOleInPlaceFrame            As VBOleGuids.IOleInPlaceFrame
      Dim pOleInPlaceUIWindow         As VBOleGuids.IOleInPlaceUIWindow
      Dim pOleInPlaceActiveObject     As VBOleGuids.IOleInPlaceActiveObject
      Dim PosRect                     As VBOleGuids.RECT
      Dim ClipRect                    As VBOleGuids.RECT
      Dim FrameInfo                   As VBOleGuids.OLEINPLACEFRAMEINFO
      Dim grfModifiers                As Long
      Dim AcceleratorMsg              As VBOleGuids.MSG
   
      'Get in-place frame and make sure it is set to our in-between
      'implementation of IOleInPlaceActiveObject in order to catch
      'TranslateAccelerator calls
      Set pOleObject = Me
      Set pOleInPlaceSite = pOleObject.GetClientSite
      pOleInPlaceSite.GetWindowContext pOleInPlaceFrame, pOleInPlaceUIWindow, VarPtr(PosRect), VarPtr(ClipRect), VarPtr(FrameInfo)
      CopyMemory pOleInPlaceActiveObject, m_IPAOHookStruct.ThisPointer, 4
      pOleInPlaceFrame.SetActiveObject pOleInPlaceActiveObject, vbNullString
      If Not pOleInPlaceUIWindow Is Nothing Then
         pOleInPlaceUIWindow.SetActiveObject pOleInPlaceActiveObject, vbNullString
      End If
      ' Clear up the inbetween implementation:
      CopyMemory pOleInPlaceActiveObject, 0&, 4
      
      If ((GetAsyncKeyState(vbKeyTab) And &H8000&) = &H8000&) Then
         ' we got here because of a tab press:
         pKeyDown vbKeyTab, ShiftState
      End If
   ' --------------------------------------------------------------------------
      
   End Select
End Function

Private Sub m_cScrollBar_Change(eBar As EFSScrollBarConstants)
On Error Resume Next
   '
   pPaint
   pResizeContainedControls
   UserControl.Refresh
   
   '
   RaiseEvent Scroll
   
End Sub

Private Sub m_cScrollBar_Scroll(eBar As EFSScrollBarConstants)
On Error Resume Next
   '
   pPaint
   pResizeContainedControls
   UserControl.Refresh
   '
   RaiseEvent Scroll
   
End Sub

Private Sub m_cScrollBar_ScrollClick(eBar As EFSScrollBarConstants, eButton As MouseButtonConstants)
On Error Resume Next
   '
   If Not (m_bFocus) Then
      UserControl.SetFocus
   End If
   '
End Sub

Private Sub m_tmr_ThatTime()
On Error Resume Next
Dim tR As RECT
Dim tP As POINTAPI
Dim hTheme As Long
Dim pc As pcExplorerBar
   '
   ' Do some checks:
   If Not (m_pcOver Is Nothing) Then
      If Not (fVerifyId(m_pcOver.ID, 1)) Then
         Set m_pcOver = Nothing
      End If
   End If
   If Not (m_itmOver Is Nothing) Then
      If Not (fVerifyId(m_itmOver.ID, 2)) Then
         Set m_itmOver = Nothing
      End If
   End If
   
   GetCursorPos tP
   ScreenToClient UserControl.hwnd, tP
   GetClientRect UserControl.hwnd, tR
   
   If (PtInRect(tR, tP.x, tP.y) = 0) Then
      hTheme = plGetTheme()
   
      If (m_pcOver Is Nothing) Then
         If Not (m_itmOver Is Nothing) Then
            m_itmOver.MouseOver = False
            Set pc = m_colBars("C:" & m_itmOver.BarID)
            pPaintBar pc, UserControl.hdc, hTheme, tR
            pPaintBorders UserControl.hdc, hTheme, tR
         End If
      Else
         m_pcOver.MouseOver = False
         pPaintBar m_pcOver, UserControl.hdc, hTheme, tR
         pPaintBorders UserControl.hdc, hTheme, tR
      End If
      UserControl.Refresh
      
      If Not (hTheme = 0) Then
         CloseThemeData hTheme
      End If
      
      m_tmr.Interval = 0
      
   End If
   
   '
End Sub

Private Sub UserControl_GotFocus()
   On Error Resume Next
   
   m_bFocus = True
   If (m_bShowFocusRect) Then
      pPaint
      UserControl.Refresh
   End If
End Sub

Private Sub UserControl_Initialize()
   On Error Resume Next
   '
   m_bUseExplorerTheme = True
   m_bUseExplorerTransitionStyle = True
   m_bShowFocusRect = True
   m_bRedraw = True
   
   m_lBarSpacing = 15
   m_lItemSpacing = 1
   m_lMargin = 12
   
   ' Set up information about this control for
   ' IOleInPlaceActiveObject interface:
   Dim IPAO As IOleInPlaceActiveObject

   With m_IPAOHookStruct
      Set IPAO = Me
      CopyMemory .IPAOReal, IPAO, 4
      CopyMemory .TBEx, Me, 4
      .lpVTable = IPAOVTable
      .ThisPointer = VarPtr(m_IPAOHookStruct)
   End With
      
   '
End Sub

Private Sub UserControl_InitProperties()
   On Error Resume Next
   
   '
   pInitialise
   '
End Sub

Private Sub UserControl_KeyDown(KeyCode As Integer, Shift As Integer)
   On Error Resume Next
   
   '
   pKeyDown KeyCode, Shift
   '
End Sub

Private Sub UserControl_KeyPress(KeyAscii As Integer)
   On Error Resume Next
   '
   pKeyPress KeyAscii
   '
End Sub

Private Sub UserControl_KeyUp(KeyCode As Integer, Shift As Integer)
   On Error Resume Next
   '
   pKeyUp KeyCode, Shift
   '
End Sub

Private Sub UserControl_LostFocus()
   On Error Resume Next
   m_bFocus = False
   If (m_bShowFocusRect) Then
      pPaint
      UserControl.Refresh
   End If
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
   On Error Resume Next

Dim pc As pcExplorerBar
Dim pcSel As pcExplorerBar
Dim itm As pcExplorerBarItem
Dim itmSel As pcExplorerBarItem
Dim tR As RECT
Dim hTheme As Boolean

   '
   If Not (m_colBars Is Nothing) Then
      
      m_tmr.Interval = 0
      
      ' Check if the mouse is down on something:
      For Each pc In m_colBars
         If (pbHitTest(pc, pcSel, itmSel)) Then
            Exit For
         End If
      Next
                        
      If (pcSel Is Nothing) And (itmSel Is Nothing) Then
         Exit Sub
      End If
      
      hTheme = plGetTheme()
                        
      ' Ensure no other item or bar has mouse over
      ' or down:
      For Each pc In m_colBars
         If Not pc Is pcSel Then
            If (pc.MouseDown Or pc.MouseOver) Then
               pc.MouseDown = False
               pc.MouseOver = False
               pPaintBar pc, UserControl.hdc, hTheme, tR, True
               pPaintBorders UserControl.hdc, hTheme, tR
            End If
         End If
      Next
      
      For Each itm In m_colItems
         If Not itm Is itmSel Then
            If (itm.MouseDown Or itm.MouseOver) Then
               itm.MouseDown = False
               itm.MouseOver = False
               Set pc = m_colBars("C:" & itm.BarID)
               pPaintBar pc, UserControl.hdc, hTheme, tR, True
               pPaintBorders UserControl.hdc, hTheme, tR
            End If
         End If
      Next
      
      ' Set this item to have mouse over & down:
      GetClientRect UserControl.hwnd, tR
      If (itmSel Is Nothing) Then
         pcSel.MouseDown = (Button = vbLeftButton)
         pcSel.MouseOver = True
         pPaintBar pcSel, UserControl.hdc, hTheme, tR, True
         If (Button = vbRightButton) Then
            OnBarRightClick pcSel
         End If
      Else
         itmSel.MouseDown = (Button = vbLeftButton)
         itmSel.MouseOver = True
         Set pc = m_colBars("C:" & itmSel.BarID)
         pPaintBar pc, UserControl.hdc, hTheme, tR, True
         pPaintBorders UserControl.hdc, hTheme, tR
         If (Button = vbRightButton) Then
            OnItemRightClick itmSel
         End If
      End If
      
      If Not (hTheme = 0) Then
         CloseThemeData hTheme
      End If
                              
   End If
   '
End Sub

Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
   On Error Resume Next
On Error GoTo UserControl_MouseMove_ErrHandle

Dim pc As pcExplorerBar
Dim pcSel As pcExplorerBar
Dim itm As pcExplorerBarItem
Dim itmSel As pcExplorerBarItem
Dim hTheme As Long
Dim tR As RECT
Dim sToolTip As String
Dim ePointer As MousePointerConstants

   '
   If Not (m_colBars Is Nothing) Then
      If (Button = 0) Then
      
         For Each pc In m_colBars
            If (pbHitTest(pc, pcSel, itmSel)) Then
               Exit For
            End If
         Next
         
         GetClientRect UserControl.hwnd, tR
         hTheme = plGetTheme()
         
         For Each pc In m_colBars
            If Not pc Is pcSel Then
               If (pc.MouseDown Or pc.MouseOver) Then
                  pc.MouseDown = False
                  pc.MouseOver = False
                  pPaintBar pc, UserControl.hdc, hTheme, tR, True
                  pPaintBorders UserControl.hdc, hTheme, tR
                  UserControl.Refresh
               End If
            End If
         Next
         
         For Each itm In m_colItems
            If Not itm Is itmSel Then
               If (itm.MouseDown Or itm.MouseOver) Then
                  itm.MouseDown = False
                  itm.MouseOver = False
                  Set pc = m_colBars("C:" & itm.BarID)
                  pPaintBar pc, UserControl.hdc, hTheme, tR, True
                  pPaintBorders UserControl.hdc, hTheme, tR
                  UserControl.Refresh
               End If
            End If
         Next
         
         Set m_pcOver = Nothing
         Set m_itmOver = Nothing
         If (pcSel Is Nothing) And (itmSel Is Nothing) Then
            sToolTip = ""
            ePointer = vbDefault
            m_tmr.Interval = 0
         Else
            m_tmr.Interval = 50
            If Not (pcSel Is Nothing) Then
               ePointer = IIf(pcSel.CanExpand, vbCustom, vbDefault) ' 2003-07-05: Pointer should not be a hand unless the item can be expanded
               sToolTip = pcSel.ToolTipText
               pcSel.MouseOver = True
               Set m_pcOver = pcSel
               pPaintBar pcSel, UserControl.hdc, hTheme, tR, True
               pPaintBorders UserControl.hdc, hTheme, tR
               UserControl.Refresh
            Else
               ePointer = vbCustom
               sToolTip = itmSel.ToolTipText
               itmSel.MouseOver = True
               Set m_itmOver = itmSel
               Set pc = m_colBars("C:" & m_itmOver.BarID)
               pPaintBar pc, UserControl.hdc, hTheme, tR, True
               pPaintBorders UserControl.hdc, hTheme, tR
               UserControl.Refresh
            End If
         End If
         
         If (Not (hTheme = 0)) Then
            CloseThemeData hTheme
         End If
         
         pSetToolTipText sToolTip
         pSetMousePointer ePointer
         OnHighlight m_pcOver, m_itmOver
         
      End If
   End If
   '
Exit Sub
UserControl_MouseMove_ErrHandle:
End Sub

Private Sub UserControl_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
   On Error Resume Next

Dim pc As pcExplorerBar
Dim itm As pcExplorerBarItem
Dim pcSel As pcExplorerBar
Dim itmSel As pcExplorerBarItem
Dim hTheme As Long
Dim tR As RECT
   
   '
   If (Button = vbLeftButton) Then
      For Each pc In m_colBars
         If (pbHitTest(pc, pcSel, itmSel)) Then
            If (pcSel Is Nothing) Then
               ' Click on item?
               If (itmSel.MouseDown) Then
                  OnItemClick itmSel
               End If
            Else
               ' Click on bar?
               If (pcSel.MouseDown) Then
                  OnBarClick pcSel
               End If
            End If
         End If
      Next
   End If
   
   ' nothing mouse down any more:
   hTheme = plGetTheme()
   GetClientRect UserControl.hwnd, tR
   
   For Each pc In m_colBars
      If (pc.MouseDown) Then
         pc.MouseDown = False
         pPaintBar pc, UserControl.hdc, hTheme, tR, True
         pPaintBorders UserControl.hdc, hTheme, tR
      End If
   Next
   
   If Not (hTheme = 0) Then
      CloseThemeData hTheme
   End If
   '
   
End Sub

Private Sub UserControl_ReadProperties(PropBag As PropertyBag)
   On Error Resume Next
   
   '
   UseExplorerStyle = PropBag.ReadProperty("UseExplorerTheme", True)
   UseExplorerTransitionStyle = PropBag.ReadProperty("UseExplorerTransitionStyle", True)
   m_oBackColorEnd = PropBag.ReadProperty("BackColorEnd", vbWindowBackground)
   m_oBackColorStart = PropBag.ReadProperty("BackColorStart", vbWindowBackground)
   m_bShowFocusRect = PropBag.ReadProperty("ShowFocusRect", True)
   m_bRedraw = PropBag.ReadProperty("Redraw", True)
   
   pInitialise
   '
End Sub

Private Sub UserControl_Resize()
   On Error Resume Next
Dim tR As RECT
Dim tWR As RECT
   '
   GetClientRect UserControl.hwnd, tR
   If (m_cDibFade Is Nothing) Then
      Set m_cDibFade = New pcAlphaDibSection
   End If
   If (tR.right - tR.left) > m_cDibFade.Width Or (tR.bottom - tR.Top) > m_cDibFade.Height Then
      m_cDibFade.Create tR.right - tR.left, tR.bottom - tR.Top
   End If
   
   If Not (m_colBars Is Nothing) Then
      GetWindowRect UserControl.hwnd, tWR
      If Not ((tWR.right - tWR.left) = m_lLastWidth) Then
         Dim pc As pcExplorerBar
         For Each pc In m_colBars
            fBarChanged pc.ID, True, False
         Next
      End If
            
      pMeasure
   End If
   
   pPaint
   UserControl.Refresh
   '
End Sub

Private Sub UserControl_Show()
   On Error Resume Next
   
   '
   UserControl_Resize
   '
End Sub

Private Sub UserControl_Terminate()
   On Error Resume Next
   
   '
   pTerminate
   
   ' Detach the custom IOleInPlaceActiveObject interface
   ' pointers.
   With m_IPAOHookStruct
      CopyMemory .IPAOReal, 0&, 4
      CopyMemory .TBEx, 0&, 4
   End With
   
   '
End Sub

Private Sub UserControl_WriteProperties(PropBag As PropertyBag)
   On Error Resume Next
   '
   PropBag.WriteProperty "UseExplorerTheme", UseExplorerStyle, True
   PropBag.WriteProperty "UseExplorerTransitionStyle", UseExplorerTransitionStyle, True
   PropBag.WriteProperty "BackColorEnd", m_oBackColorEnd, vbWindowBackground
   PropBag.WriteProperty "BackColorStart", m_oBackColorStart, vbWindowBackground
   PropBag.WriteProperty "ShowFocusRect", m_bShowFocusRect, True
   PropBag.WriteProperty "Redraw", m_bRedraw, True
   '
End Sub

' MAH
Public Function GetIsXP() As Boolean
   On Error Resume Next
  GetIsXP = m_bIsXp
End Function
' MAH

' MAH
Public Function GetIs2000With256() As Boolean
  On Error Resume Next
  
  If GetIsXP Then
    GetIs2000With256 = False
  Else
    Dim lnDCHandle As Long
    lnDCHandle = GetDC(0)

    ' Get device info
    Dim lnPlanes As Long, lnBitsPixel As Long
    lnPlanes = GetDeviceCaps(lnDCHandle, 14)
    lnBitsPixel = GetDeviceCaps(lnDCHandle, 12)

    ' Returns the number of colors used by the driver
    Dim lnNumColors As Long
    lnNumColors = 2 ^ (lnPlanes * lnBitsPixel)
  
    ' Rlease DC
    Call ReleaseDC(0, lnDCHandle)

    If lnNumColors = 256 Then
      GetIs2000With256 = True
    Else
      GetIs2000With256 = False
    End If
  End If
End Function
' MAH

