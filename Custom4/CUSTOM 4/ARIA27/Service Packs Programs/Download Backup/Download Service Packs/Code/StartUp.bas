Attribute VB_Name = "StartUp"
Option Explicit

Public Const LOGIN_PROCEED           As Integer = 0
Public Const LOGIN_CANCEL            As Integer = 1
Public Const LOGIN_GOTO_LOGIN_INFO   As Integer = 2
Public Const LOGIN_RETRY             As Integer = 3
Public Const LOGIN_ERROR             As Integer = 4

' Registry Entries
Public Const APP_CATEGORY   As String = "Aria Wizards"
Public Const WIZARD_NAME    As String = "Download Service Packs"
Public Const SKIP_INTRO     As String = "Skip Introduction Screen"
Public Const SAVE_ID        As String = "Save User Code"
Public Const USER_ID        As String = "User Code"
Public Const USER_PASSWORD  As String = "User Password"
Public Const SAVE_PASSWORD  As String = "Save User Password"
Public Const SELECT_TO_DOWN As String = "Select what to download"
Public Const AUTO_UPDATE    As String = "Automatically Update"
Public Const INTERACTIVE    As String = "Interactive Log Screen"
Public Const DOWNLOAD_TO    As String = "Download Path"
Public Const ARIA_SYS_PATH  As String = "Aria SysFiles Dir"
Public Const HOST_SERVER    As String = "Host Server"
Public Const USE_PROXY      As String = "Use Proxy"
Public Const FTP_ADDRESS    As String = "FTP Address"
Public Const PORT           As String = "Port"
Public Const RETRY_CONNECT  As String = "Automatically connect"
Public Const NUM_SECONDS    As String = "Number of Seconds"
Public Const TIMES          As String = "Number of times"
Public Const DSN_IS_CREATED As String = "DSN is created"

Private Sub Main()
  Call CheckDSN
  
  Call SetSetting(DOWNLOAD_TO, LoadSetting(DOWNLOAD_TO))
  Call SetSetting(ARIA_SYS_PATH, LoadSetting(ARIA_SYS_PATH))
  Call SetSetting(SKIP_INTRO, LoadSetting(SKIP_INTRO))
  Call SetSetting(SAVE_ID, LoadSetting(SAVE_ID))
  Call SetSetting(SAVE_PASSWORD, LoadSetting(SAVE_PASSWORD))
  Call SetSetting(SELECT_TO_DOWN, LoadSetting(SELECT_TO_DOWN))
  Call SetSetting(AUTO_UPDATE, LoadSetting(AUTO_UPDATE))
  Call SetSetting(INTERACTIVE, LoadSetting(INTERACTIVE))
  Call SetSetting(HOST_SERVER, LoadSetting(HOST_SERVER))
  Call SetSetting(USE_PROXY, LoadSetting(USE_PROXY))
  Call SetSetting(FTP_ADDRESS, LoadSetting(FTP_ADDRESS))
  Call SetSetting(PORT, LoadSetting(PORT))
  Call SetSetting(RETRY_CONNECT, LoadSetting(RETRY_CONNECT))
  Call SetSetting(NUM_SECONDS, LoadSetting(NUM_SECONDS))
  Call SetSetting(TIMES, LoadSetting(TIMES))
  
  Dim objWizardForm As frmWizard
  Set objWizardForm = New frmWizard
  objWizardForm.Show
End Sub
Public Function GetDefault(ByVal strRegEntry As String) As Variant
  Select Case strRegEntry
    Case DOWNLOAD_TO
      GetDefault = App.Path & "\SPack"
    Case ARIA_SYS_PATH
      GetDefault = App.Path & "\SysFiles"
    Case SKIP_INTRO
      GetDefault = vbUnchecked
    Case SAVE_ID
      GetDefault = vbChecked
    Case SAVE_PASSWORD
      GetDefault = vbChecked
    Case SELECT_TO_DOWN
      GetDefault = vbUnchecked
    Case AUTO_UPDATE
      GetDefault = vbChecked
    Case INTERACTIVE
      GetDefault = vbUnchecked
    Case HOST_SERVER
      GetDefault = "www.ariany.com"
    Case USE_PROXY
      GetDefault = vbUnchecked
    Case FTP_ADDRESS
      GetDefault = vbNullString
    Case PORT
      GetDefault = vbNullString
    Case USER_ID
      GetDefault = vbNullString
    Case USER_PASSWORD
      GetDefault = vbNullString
    Case RETRY_CONNECT
      GetDefault = vbChecked
    Case NUM_SECONDS
      GetDefault = 5
    Case TIMES
      GetDefault = 10
    Case DSN_IS_CREATED
      GetDefault = False
  End Select
End Function
Public Function LoadSetting(ByVal strRegEntry As String) As Variant
  LoadSetting = GetSetting(APP_CATEGORY, WIZARD_NAME, strRegEntry, GetDefault(strRegEntry))
End Function
Public Sub SetSetting(ByVal strRegEntry As String, ByVal varVal As Variant)
  SaveSetting APP_CATEGORY, WIZARD_NAME, strRegEntry, varVal
End Sub
Private Sub CheckDSN()
  If Not LoadSetting(DSN_IS_CREATED) Then
    rdoEngine.rdoRegisterDataSource "DSSysFiles", "Microsoft Visual FoxPro Driver", _
                                    True, "Description=Aria System Files Path" & _
                                          Chr$(13) & _
                                          "SourceDB=" & UCase(App.Path & "\Sysfiles") & _
                                          Chr$(13) & _
                                          "SourceType=" & "DBF"
    Call SetSetting(DSN_IS_CREATED, True)
  End If
End Sub
