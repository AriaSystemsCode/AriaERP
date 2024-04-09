Attribute VB_Name = "StartUp"
Option Explicit

Public Const cnsDSNName     As String = "TrackingDataForUpload"
Public Const HOST_SERVER    As String = "Host Server"

' MBADRAN Connection String to open issues (SQL database).
Public Const cnSQLConnectionStr As String = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=OpenIssues;Data Source=AriaWeb;uid=web;pwd=ariaweb"

' Registry Entries
Public Const APP_CATEGORY   As String = "Aria Upload Files"
Public Const WIZARD_NAME    As String = "Settings"
Public Const DSN_IS_CREATED As String = "DSN is created"

Public Sub Main()
  If DSNCheck() Then
    Call SetSetting(HOST_SERVER, LoadSetting(HOST_SERVER))
    
    Dim objMainForm As frmMain
    Set objMainForm = New frmMain
    Call Load(objMainForm)
    
    Dim strError As String
    If objMainForm.DataLoad(strError) Then
      Call objMainForm.Show(vbModal)
    Else
      If Len(Trim(strError)) > 0 Then
        Call Unload(objMainForm)
        Call MsgBox(strError, vbCritical)
      End If
    End If
    
    Set objMainForm = Nothing
  End If
End Sub

Public Function GetDefault(ByVal strRegEntry As String) As Variant
  Select Case strRegEntry
    Case DSN_IS_CREATED
      GetDefault = False
    Case HOST_SERVER
      GetDefault = "www.ariany.com"
  End Select
End Function

Public Function LoadSetting(ByVal strRegEntry As String) As Variant
  LoadSetting = GetSetting(APP_CATEGORY, WIZARD_NAME, strRegEntry, GetDefault(strRegEntry))
End Function

Public Sub SetSetting(ByVal strRegEntry As String, ByVal varVal As Variant)
  Call SaveSetting(APP_CATEGORY, WIZARD_NAME, strRegEntry, varVal)
End Sub

Private Function DSNCheck() As Boolean
  Dim bolRetValue As Boolean
  bolRetValue = False

  If LoadSetting(DSN_IS_CREATED) Then
    bolRetValue = True
  Else
    Dim bolValid As Boolean
    bolValid = False
    
    Dim objFileSystem As Scripting.FileSystemObject
    Set objFileSystem = New Scripting.FileSystemObject
    
    Do While Not bolValid
      Dim strPath As String
      strPath = InputBox("Please enter the 'Tracking Database Files' path", "Database Files Path", strPath)
      
      If Len(Trim(strPath)) = 0 Then
        bolValid = True
      Else
        strPath = strPath & IIf(Right(strPath, 1) = "\", "", "\")
        
        If objFileSystem.FileExists(strPath & "CUSTOMER.DBF") Then
          bolValid = True
        Else
          bolValid = False
          Call MsgBox("Invalid path. Cannot accept!", vbCritical)
        End If
      End If
    Loop
    
    Set objFileSystem = Nothing
    
    If Len(Trim(strPath)) = 0 Then
      bolRetValue = False
    Else
      rdoEngine.rdoRegisterDataSource cnsDSNName, "Microsoft Visual FoxPro Driver", _
                                      True, "Description=Tracking data company for uploading" & _
                                            Chr$(13) & _
                                            "SourceDB=" & UCase(strPath) & _
                                            Chr$(13) & _
                                            "SourceType=" & "DBF" & _
                                            Chr$(13) & _
                                            "Null=No"
      Call SetSetting(DSN_IS_CREATED, True)
      bolRetValue = True
    End If
  End If
    
  DSNCheck = bolRetValue
End Function


