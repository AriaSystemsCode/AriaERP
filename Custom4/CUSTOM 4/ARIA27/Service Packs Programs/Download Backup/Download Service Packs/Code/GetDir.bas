Attribute VB_Name = "BrowseFolders"
Option Explicit

Public Type BROWSEINFO
   hOwner           As Long
   pidlRoot         As Long
   pszDisplayName   As String
   lpszTitle        As String
   ulFlags          As Long
   lpfn             As Long
   lParam           As Long
   iImage           As Long
End Type

Public Const BIF_RETURNONLYFSDIRS = &H1
Public Const BIF_DONTGOBELOWDOMAIN = &H2
Public Const BIF_STATUSTEXT = &H4
Public Const BIF_RETURNFSANCESTORS = &H8
Public Const BIF_BROWSEFORCOMPUTER = &H1000
Public Const BIF_BROWSEFORPRINTER = &H2000
Public Const MAX_PATH = 260

Public Declare Function SHGetPathFromIDList _
   Lib "shell32.dll" Alias "SHGetPathFromIDListA" _
  (ByVal pidl As Long, ByVal pszPath As String) As Long

Public Declare Function SHBrowseForFolder Lib "shell32.dll" _
   Alias "SHBrowseForFolderA" _
  (lpBrowseInfo As BROWSEINFO) As Long

Public Declare Sub CoTaskMemFree Lib "ole32.dll" (ByVal pv As Long)

Public Declare Function GetDesktopWindow Lib "user32" () As Long

Public Function GetDir(Optional ByVal hWnd As Long = -1, Optional ByVal strTitle As String = "")
  Dim typBrowseInfo As BROWSEINFO
  With typBrowseInfo
    .hOwner = IIf(hWnd = -1, GetDesktopWindow(), hWnd)
    .pidlRoot = 0&
    .lpszTitle = strTitle
    .ulFlags = BIF_RETURNONLYFSDIRS
  End With
  
  Dim lngRetVal As Long
  Dim strRetValue As String
  lngRetVal = SHBrowseForFolder(typBrowseInfo)
  strRetValue = Space$(MAX_PATH)
  If SHGetPathFromIDList(ByVal lngRetVal, ByVal strRetValue) Then
     strRetValue = Left(strRetValue, InStr(strRetValue, Chr$(0)) - 1)
  Else
    strRetValue = Space(0)
  End If
  Call CoTaskMemFree(lngRetVal)
  GetDir = strRetValue
End Function
