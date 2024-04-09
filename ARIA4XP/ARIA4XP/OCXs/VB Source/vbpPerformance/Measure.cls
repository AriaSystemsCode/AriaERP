VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Measure"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Private Declare Sub Sleep Lib "kernel32" (ByVal lngMilliseconds As Long)
Private Declare Function GetCurrentProcessId Lib "kernel32" () As Long

Private mobjWMIService As Object

Private Sub Class_Initialize()
  Set mobjWMIService = GetObject("winmgmts:\root\cimv2")
End Sub

Private Sub Class_Terminate()
 Set mobjWMIService = Nothing
End Sub

Private Function GetLoadPercentage() As Long
  Dim objItems As Object
  Dim objItem As Object
  
  Set objItems = mobjWMIService.ExecQuery("Select * from Win32_Processor")
  For Each objItem In objItems
    GetLoadPercentage = GetLoadPercentage + objItem.LoadPercentage
  Next objItem
End Function

Private Function GetOperatingSystem() As Object
  Set GetOperatingSystem = mobjWMIService.ExecQuery("Select * from Win32_OperatingSystem")
End Function

Private Function GetFreePhysicalMemory(ByVal objItems As Object) As Long
  Dim objItem As Object
  For Each objItem In objItems
    GetFreePhysicalMemory = GetFreePhysicalMemory + objItem.FreePhysicalMemory
  Next objItem
End Function

Private Function GetFreeSpaceInPagingFiles(ByVal objItems As Object) As Long
  Dim objItem As Object
  For Each objItem In objItems
    GetFreeSpaceInPagingFiles = GetFreeSpaceInPagingFiles + objItem.FreeSpaceInPagingFiles
  Next objItem
End Function

Private Function GetFreeVirtualMemory(ByVal objItems As Object) As Long
  Dim objItem As Object
  For Each objItem In objItems
    GetFreeVirtualMemory = GetFreeVirtualMemory + objItem.FreeVirtualMemory
  Next objItem
End Function

Private Function GetSizeStoredInPagingFiles(ByVal objItems As Object) As Long
  Dim objItem As Object
  For Each objItem In objItems
    GetSizeStoredInPagingFiles = GetSizeStoredInPagingFiles + objItem.SizeStoredInPagingFiles
  Next objItem
End Function

Private Function GetTotalVirtualMemorySize(ByVal objItems As Object) As Long
  Dim objItem As Object
  For Each objItem In objItems
    GetTotalVirtualMemorySize = GetTotalVirtualMemorySize + objItem.TotalVirtualMemorySize
  Next objItem
End Function

Private Function GetTotalVisibleMemorySize(ByVal objItems As Object) As Long
  Dim objItem As Object
  For Each objItem In objItems
    GetTotalVisibleMemorySize = GetTotalVisibleMemorySize + objItem.TotalVisibleMemorySize
  Next objItem
End Function

Private Function GetProcess(ByVal lngProcessID As Long) As Object
  Set GetProcess = mobjWMIService.ExecQuery("Select * from Win32_Process where ProcessID = '" & Trim$(CStr(lngProcessID)) & "'")
End Function

Private Function GetProcessCPUTime(ByVal objItems As Object) As Long
  Dim objItem As Object
  For Each objItem In objItems
    GetProcessCPUTime = GetProcessCPUTime + _
                         ((CSng(objItem.KernelModeTime) + CSng(objItem.UserModeTime)) / 10000000)
  Next objItem
End Function

Private Function GetWorkingSetSize(ByVal objItems As Object) As Long
  Dim objItem As Object
  For Each objItem In objItems
    GetWorkingSetSize = GetWorkingSetSize + objItem.WorkingSetSize
  Next objItem
End Function

Private Function GetReadOperationCount(ByVal objItems As Object) As Long
  Dim objItem As Object
  For Each objItem In objItems
    GetReadOperationCount = GetReadOperationCount + objItem.ReadOperationCount
  Next objItem
End Function

Private Function GetReadTransferCount(ByVal objItems As Object) As Long
  Dim objItem As Object
  For Each objItem In objItems
    GetReadTransferCount = GetReadTransferCount + objItem.ReadTransferCount
  Next objItem
End Function

Private Function GetPerfRawDataPerfProcProcess(ByVal lngProcessID As Long) As Object
  Set GetPerfRawDataPerfProcProcess = mobjWMIService.ExecQuery( _
      "select * from Win32_PerfRawData_PerfProc_Process where IDProcess = '" & Trim$(CStr(lngProcessID)) & "'")
End Function

Public Function GetPercentProcessorTime(ByVal objItems As Object) As Single
  Dim objItem As Object
  For Each objItem In objItems
    GetPercentProcessorTime = GetPercentProcessorTime + objItem.PercentProcessorTime
  Next objItem
End Function

Public Function GetTimestamp_Sys100NS(ByVal objItems As Object) As Single
  Dim objItem As Object
  For Each objItem In objItems
    GetTimestamp_Sys100NS = GetTimestamp_Sys100NS + objItem.Timestamp_Sys100NS
  Next objItem
End Function

Private Function GetCPUUsage(ByVal lngProcessID As Long) As Long
  Dim objPref1 As Object
  Dim objPref2 As Object
  Dim objTemp As Object
  Dim lngPrefN1
  Dim lngPrefD1
  Dim lngPrefN2
  Dim lngPrefD2

  Set objPref1 = mobjWMIService.ExecQuery( _
      "select * from Win32_PerfRawData_PerfProc_Process where IDProcess = '" & Trim$(CStr(lngProcessID)) & "'")

  Sleep 100
  
  Set objPref2 = mobjWMIService.ExecQuery( _
      "select * from Win32_PerfRawData_PerfProc_Process where IDProcess = '" & Trim$(CStr(lngProcessID)) & "'")

  For Each objTemp In objPref1
    If objTemp.Name <> "_Total" Then
      lngPrefN1 = objTemp.PercentProcessorTime
      lngPrefD1 = objTemp.Timestamp_Sys100NS
    End If
  Next objTemp
  
  For Each objTemp In objPref2
    If objTemp.Name <> "_Total" Then
      lngPrefN2 = objTemp.PercentProcessorTime
      lngPrefD2 = objTemp.Timestamp_Sys100NS
    End If
  Next objTemp
       
  GetCPUUsage = (lngPrefN2 - lngPrefN1) / (lngPrefD2 - lngPrefD1) * 100
End Function

Public Function GetCheckPointResult(Optional ByVal lngProcessID As Long = -1, _
                                    Optional ByVal lbolGetLoadPercentage As Boolean = False) As Result
  On Error Resume Next
  Dim objResult As New Result
  
  Dim objGetPerfRawDataPerfProcProcess
  
  If lngProcessID <> -1 Then
    Set objPerfRawDataPerfProcProcess = GetPerfRawDataPerfProcProcess(lngProcessID)
  Else
    Set objPerfRawDataPerfProcProcess = GetPerfRawDataPerfProcProcess(GetCurrentProcessId())
  End If
  
  objResult.PercentProcessorTime = GetPercentProcessorTime(objPerfRawDataPerfProcProcess)
  objResult.Timestamp_Sys100NS = GetTimestamp_Sys100NS(objPerfRawDataPerfProcProcess)
  
  If lbolGetLoadPercentage Then
    objResult.LoadPercentage = GetLoadPercentage
  End If
  
  Dim objOS As Object
  Set objOS = GetOperatingSystem
  
  objResult.FreePhysicalMemory = GetFreePhysicalMemory(objOS)
  objResult.FreeSpaceInPagingFiles = GetFreeSpaceInPagingFiles(objOS)
  objResult.FreeVirtualMemory = GetFreeVirtualMemory(objOS)
  objResult.SizeStoredInPagingFiles = GetSizeStoredInPagingFiles(objOS)
  objResult.TotalVirtualMemorySize = GetTotalVirtualMemorySize(objOS)
  objResult.TotalVisibleMemorySize = GetTotalVisibleMemorySize(objOS)
  
  Dim objProcess As Object
  If lngProcessID <> -1 Then
    Set objProcess = GetProcess(lngProcessID)
  Else
    Set objProcess = GetProcess(GetCurrentProcessId())
  End If
  
  objResult.ProcessCPUTime = GetProcessCPUTime(objProcess)
  objResult.WorkingSetSize = GetWorkingSetSize(objProcess)
  objResult.ReadOperationCount = GetReadOperationCount(objProcess)
  objResult.ReadTransferCount = GetReadTransferCount(objProcess)
  
    
  Set GetCheckPointResult = objResult
End Function


Public Function GetCheckPointResultString(Optional ByVal lngProcessID As Long = -1, _
                                          Optional ByVal lbolGetLoadPercentage As Boolean = False) As String
  Dim objResult As Result
  Set objResult = GetCheckPointResult(lngProcessID, lbolGetLoadPercentage)
  
  Dim strResult As String
  strResult = IIf(lbolGetLoadPercentage, "HW_LoadPercentage = " & Trim(Str$(objResult.LoadPercentage)) & Chr$(13), "") & _
              "OS_FreePhysicalMemory = " & Trim(Str$(objResult.FreePhysicalMemory)) & Chr$(13) & _
              "OS_FreeSpaceInPagingFiles = " & Trim(Str$(objResult.FreeSpaceInPagingFiles)) & Chr$(13) & _
              "OS_FreeVirtualMemory = " & Trim(Str$(objResult.FreeVirtualMemory)) & Chr$(13) & _
              "OS_SizeStoredInPagingFiles = " & Trim(Str$(objResult.SizeStoredInPagingFiles)) & Chr$(13) & _
              "OS_TotalVirtualMemorySize = " & Trim(Str$(objResult.TotalVirtualMemorySize)) & Chr$(13) & _
              "OS_TotalVisibleMemorySize = " & Trim(Str$(objResult.TotalVisibleMemorySize)) & Chr$(13) & _
              "PR_ProcessCPUTime = " & Trim(Str$(objResult.ProcessCPUTime)) & Chr$(13) & _
              "PR_WorkingSetSize = " & Trim(Str$(objResult.WorkingSetSize)) & Chr$(13) & _
              "PR_ReadOperationCount = " & Trim(Str$(objResult.ReadOperationCount)) & Chr$(13) & _
              "PR_ReadTransferCount = " & Trim(Str$(objResult.ReadTransferCount)) & Chr$(13) & _
              "PR_PercentProcessorTime = " & Trim(Str$(objResult.PercentProcessorTime)) & Chr$(13) & _
              "PR_Timestamp_Sys100NS = " & Trim(Str$(objResult.Timestamp_Sys100NS))
              
  GetCheckPointResultString = strResult
End Function
