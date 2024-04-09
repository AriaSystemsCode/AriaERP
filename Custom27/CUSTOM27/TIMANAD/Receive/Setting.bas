Attribute VB_Name = "Setting"
Option Explicit

Public Declare Function WritePrivateProfileString Lib "kernel32" Alias "WritePrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpString As Any, ByVal lpFileName As String) As Long
Public Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationName As String, ByVal lpKeyName As Any, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long
Public Declare Function GetPrivateProfileInt Lib "kernel32" Alias "GetPrivateProfileIntA" (ByVal lpApplicationName As String, ByVal lpKeyName As String, ByVal nDefault As Long, ByVal lpFileName As String) As Long

' \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
Public Declare Function RegCreateKeyEx Lib "advapi32.dll" Alias "RegCreateKeyExA" (ByVal hKey As Long, ByVal lpSubKey As String, ByVal Reserved As Long, ByVal lpClass As String, ByVal dwOptions As Long, ByVal samDesired As Long, lpSecurityAttributes As SECURITY_ATTRIBUTES, phkResult As Long, lpdwDisposition As Long) As Long
Public Declare Function RegCloseKey Lib "advapi32.dll" (ByVal hKey As Long) As Long
Public Declare Function RegQueryValueEx Lib "advapi32.dll" Alias "RegQueryValueExA" (ByVal hKey As Long, ByVal lpValueName As String, ByVal lpReserved As Long, lpType As Long, lpData As Any, lpcbData As Long) As Long
Public Declare Function RegSetValueEx Lib "advapi32.dll" Alias "RegSetValueExA" (ByVal hKey As Long, ByVal lpValueName As String, ByVal Reserved As Long, ByVal dwType As Long, lpData As Any, ByVal cbData As Long) As Long

Private Type SECURITY_ATTRIBUTES
  nLength As Long
  lpSecurityDescriptor As Long
  bInheritHandle As Long
End Type
Private SecurAtt As SECURITY_ATTRIBUTES
Private Const SystemSetting = &H80000000
Private Const INT_TYP_STRING As Integer = 8
Private Const REG_OPTION_NON_VOLATILE = 0
Private Const MAX_BYTES As Integer = 255
Private Const REG_SZ = 1
Private Const ERROR_SUCCESS = 0&
Private Const SYNCHRONIZE = &H100000
Private Const KEY_CREATE_LINK = &H20
Private Const KEY_NOTIFY = &H10
Private Const KEY_ENUMERATE_SUB_KEYS = &H8
Private Const KEY_CREATE_SUB_KEY = &H4
Private Const KEY_SET_VALUE = &H2
Private Const KEY_QUERY_VALUE = &H1
Private Const STANDARD_RIGHTS_ALL = &H1F0000
Private Const KEY_ALL_ACCESS = ((STANDARD_RIGHTS_ALL Or KEY_QUERY_VALUE Or KEY_SET_VALUE Or KEY_CREATE_SUB_KEY Or KEY_ENUMERATE_SUB_KEYS Or KEY_NOTIFY Or KEY_CREATE_LINK) And (Not SYNCHRONIZE))
Private Const REG_OPENED_EXISTING_KEY = &H2
' \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

Public Function GetVAlue(ByVal intValueName As String, _
                         ByVal strValue As String, _
                         ByVal strSubKey As String) As String
  Dim lngKey As Long
  Dim lngOpenCreate As Long
  Dim lngStatus As Long
  Dim lngType As Long
  Dim bytBuf(MAX_BYTES) As Byte
  Dim vntValue As Variant
 
  strSubKey = "\Licenses" & strSubKey
  lngStatus = RegCreateKeyEx(ByVal SystemSetting, _
                             ByVal strSubKey, _
                             ByVal 0, _
                             ByVal INT_TYP_STRING, _
                             ByVal REG_OPTION_NON_VOLATILE, _
                             ByVal KEY_ALL_ACCESS, _
                             SecurAtt, _
                             lngKey, _
                             lngOpenCreate)


  If lngStatus = ERROR_SUCCESS Then
    
     If lngOpenCreate = REG_OPENED_EXISTING_KEY Then
      
      lngStatus = RegQueryValueEx(ByVal lngKey, ByVal intValueName, _
                                  ByVal 0, lngType, _
                                  bytBuf(0), MAX_BYTES)
      
      If lngStatus = ERROR_SUCCESS Then
        vntValue = bytBuf
        GetVAlue = StrConv(vntValue, vbUnicode)
      Else
        Call SetRegEntry(lngKey, intValueName, strValue)
        GetVAlue = strValue
      End If
    Else
      Call SetRegEntry(lngKey, intValueName, strValue)
      GetVAlue = strValue
    End If
  End If
  RegCloseKey (lngKey)
End Function


Private Function SetRegEntry(ByVal lngKey As Long, ByVal strRegEntryName As String, _
                             ByVal vntRegEntryValue As Variant) As Boolean
 
  Dim lngStatus As Long
    
  lngStatus = RegSetValueEx(ByVal lngKey, _
                            ByVal strRegEntryName, _
                            ByVal 0, _
                            ByVal REG_SZ, _
                            ByVal CStr(vntRegEntryValue), _
                            Len(vntRegEntryValue))

  If lngStatus = ERROR_SUCCESS Then
    SetRegEntry = True
  Else
    SetRegEntry = False
  End If

End Function




