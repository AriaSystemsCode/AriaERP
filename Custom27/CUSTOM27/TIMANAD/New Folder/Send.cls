VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "SendActivity"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private ISAControlSequence As Integer
Private GSControlSequence As Integer
Private STControlSequence As Integer
Private TransactionSetRecords As Long
Private TransactionSetCount As Long
Private GroupCount As Long
Private LinesCount As Long

Private SenderId As String
Private StoreId As String
Private StoreName As String

Private ReceiverID As String
Private ReceiverEMail As String
Private ReceiverName  As String
Private OutgoingFileDir As String
Private OutgoingFileName As String
Private DataPath As String

Const FieldSeparator = "*"
Const SegmentTerminator = "§"

Private objActPrdData As New GetData
Private objActPrdRs As New ADODB.Recordset
Private bolSendSetting As Boolean
Public Property Let sendsetting(ByVal mbolSendSetting As Boolean)
  bolSendSetting = mbolSendSetting
End Property
Public Property Get sendsetting() As Boolean
sendsetting = bolSendSetting
End Property
Public Function SendProductActivity() As Boolean
Dim oFileSystemObject, oFilePointer, oGetFile
Dim strItem As String
Dim strActiType As String
Dim strActiDate As Date
Dim SumQuantity, sumAmount As Long

objActPrdData.filename = "TMACTIVI"
objActPrdData.sqlSelect.clear
objActPrdData.sqlSelect.add "dDate,cItem,cSize,cEdiType,nQuantity,nAmount,lSent,dSentDate,nlineno"
objActPrdData.SqlWhere.clear
objActPrdData.SqlWhere.add "dDate IN (SELECT DIST ddate FROM TMACTIVI WHERE NOT lsent)"
objActPrdData.sqlOrderBy.clear
objActPrdData.sqlOrderBy.add "dDate,cItem,cSize,cEdiType"
Set objActPrdRs = objActPrdData.retreivedata(DataPath, adOpenKeyset, adLockBatchOptimistic)
If objActPrdRs.RecordCount > 0 Then
  Set oFileSystemObject = CreateObject("Scripting.FileSystemObject")
  oFileSystemObject.CreateTextFile OutgoingFileDir & OutgoingFileName           'Create a file
  Set oGetFile = oFileSystemObject.GetFile(OutgoingFileDir & OutgoingFileName)
  Set oFilePointer = oGetFile.OpenAsTextStream(2)
  
  oFilePointer.Write ISASegment()
  oFilePointer.Write GSSegment()
  Do While Not objActPrdRs.EOF
    strActiDate = objActPrdRs!ddate
    oFilePointer.Write STSegment()
    oFilePointer.Write XQSegment(strActiDate)
    oFilePointer.Write N1Segment()
    Do While objActPrdRs!ddate = strActiDate
      strItem = objActPrdRs!cItem & objActPrdRs!cSize
      oFilePointer.Write LINSegment(objActPrdRs!cItem, objActPrdRs!cSize)
      Do While objActPrdRs!cItem & objActPrdRs!cSize = strItem
        strActiType = objActPrdRs!cEdiType
        SumQuantity = 0
        sumAmount = 0
        Do While objActPrdRs!cItem & objActPrdRs!cSize & objActPrdRs!cEdiType = strItem & strActiType
          SumQuantity = SumQuantity + objActPrdRs!nQuantity
          sumAmount = sumAmount + objActPrdRs!nAmount
          objActPrdRs!lSent = True
          objActPrdRs!dsentdate = Date
          objActPrdRs.MoveNext
          If objActPrdRs.EOF Then
            Exit Do
          End If
        Loop
        oFilePointer.Write ZASegment(strActiType, SumQuantity)
        If objActPrdRs.EOF Then
          Exit Do
        End If
      Loop
      If objActPrdRs.EOF Then
        Exit Do
      End If
    Loop
    oFilePointer.Write CTTSegment()
    oFilePointer.Write SESegment()
  Loop
  oFilePointer.Write GESegment()
  oFilePointer.Write IEASegment()
  
  oFilePointer.Close
  WritePrivateProfileString "Send Activity", "ISAControlSequence", Str$(ISAControlSequence), App.Path & "\SendActi.ini"
  WritePrivateProfileString "Send Activity", "GSControlSequence", Str$(GSControlSequence), App.Path & "\SendActi.ini"
  WritePrivateProfileString "Send Activity", "STControlSequence", Str$(STControlSequence), App.Path & "\SendActi.ini"
  If emailfile() Then
    objActPrdRs.UpdateBatch
    SendProductActivity = True
  Else
    SendProductActivity = False
  End If
End If
objActPrdRs.Close
Set objActPrdRs = Nothing
Set objActPrdData = Nothing
End Function
Private Function ConvertDate(ByVal datConvDate As Date, ByVal bolCentury As Boolean)
If bolCentury Then
  ConvertDate = Year(datConvDate) & Format(Month(datConvDate), "00") & Format(Day(datConvDate), "00")
Else
  ConvertDate = Right(Year(datConvDate), 2) & Format(Month(datConvDate), "00") & Format(Day(datConvDate), "00")
End If
End Function
Private Function ConvertTime() As String
ConvertTime = Format(Hour(Time()), "00") & Format(Minute(Time()), "00")
End Function
Private Function ISASegment() As String

ISAControlSequence = ISAControlSequence + 1
Dim strsegmentline As String
strsegmentline = "ISA" & FieldSeparator & "00" & FieldSeparator & Space(10) & FieldSeparator & _
                 "00" & FieldSeparator & Space(10) & FieldSeparator & "ZZ" & FieldSeparator & _
                 "TIMANAD STORES " & FieldSeparator
strsegmentline = strsegmentline & "12" & FieldSeparator & Trim$(ReceiverID) & String(15 - Len(Trim$(ReceiverID)), " ") & FieldSeparator & _
                 ConvertDate(Date, False) & FieldSeparator & ConvertTime() & FieldSeparator & "U" & FieldSeparator & "00401" & FieldSeparator & _
                 Format(ISAControlSequence, "000000000") & FieldSeparator & "0" & FieldSeparator & "P" & FieldSeparator & ">" & SegmentTerminator & Chr(13) & Chr(10)
ISASegment = strsegmentline
GroupCount = 0
End Function

Private Function GSSegment() As String
Dim strsegmentline As String
GSControlSequence = GSControlSequence + 1
GroupCount = GroupCount + 1
strsegmentline = "GS" & FieldSeparator & "PD" & FieldSeparator & "TIMANAD STORES" & FieldSeparator & _
                 Trim$(ReceiverID) & FieldSeparator & ConvertDate(Date, True) & FieldSeparator & _
                 ConvertTime() & FieldSeparator & Format(GSControlSequence, "000000000") & FieldSeparator & _
                 "X" & FieldSeparator & "004010VICS" & SegmentTerminator & Chr(13) & Chr(10)
GSSegment = strsegmentline
TransactionSetCount = 0
End Function
Private Function STSegment() As String

STControlSequence = STControlSequence + 1
TransactionSetCount = TransactionSetCount + 1
TransactionSetRecords = 1
LinesCount = 0
STSegment = "ST" & FieldSeparator & "852" & FieldSeparator & Format(STControlSequence, "000000000") & SegmentTerminator & Chr(13) & Chr(10)

End Function
Private Function XQSegment(ByVal TranDate As Date) As String
XQSegment = "XQ" & FieldSeparator & "H" & FieldSeparator & ConvertDate(TranDate, True) & _
            FieldSeparator & ConvertDate(TranDate, True) & SegmentTerminator & Chr(13) & Chr(10)
TransactionSetRecords = TransactionSetRecords + 1
End Function
Private Function N1Segment() As String
N1Segment = "N1" & FieldSeparator & "RL" & FieldSeparator & StoreName & FieldSeparator & "92" & FieldSeparator & _
            StoreId & SegmentTerminator & Chr(13) & Chr(10)
TransactionSetRecords = TransactionSetRecords + 1
End Function

Private Function LINSegment(ByVal ItemCode As String, ByVal SizeCode As String) As String

LINSegment = "LIN" & FieldSeparator & FieldSeparator & "IT" & FieldSeparator & Mid$(ItemCode, 1, 12) & FieldSeparator & _
            "BO" & FieldSeparator & Mid$(ItemCode, 14, 3) & FieldSeparator & "IZ" & FieldSeparator & Mid$(ItemCode, 17, 3) & SizeCode & SegmentTerminator & Chr(13) & Chr(10)
TransactionSetRecords = TransactionSetRecords + 1
LinesCount = LinesCount + 1
End Function
Private Function ZASegment(ByVal strType As String, ByVal intQuantity As Long) As String

ZASegment = "ZA" & FieldSeparator & strType & FieldSeparator & Trim$(Str(intQuantity)) & FieldSeparator & _
            "EA" & SegmentTerminator & Chr(13) & Chr(10)
TransactionSetRecords = TransactionSetRecords + 1
End Function
Private Function CTTSegment() As String

CTTSegment = "CTT" & FieldSeparator & Trim$(Str$(LinesCount)) & SegmentTerminator & Chr(13) & Chr(10)
TransactionSetRecords = TransactionSetRecords + 1
End Function

Private Function SESegment() As String

TransactionSetRecords = TransactionSetRecords + 1
SESegment = "SE" & FieldSeparator & Trim$(Str$(TransactionSetRecords)) & FieldSeparator & _
             Format(STControlSequence, "000000000") & SegmentTerminator & Chr(13) & Chr(10)
End Function
Private Function GESegment() As String

GESegment = "GE" & FieldSeparator & Trim$(Str$(TransactionSetCount)) & FieldSeparator & _
             Format(GSControlSequence, "000000000") & SegmentTerminator & Chr(13) & Chr(10)
End Function
Private Function IEASegment() As String

IEASegment = "IEA" & FieldSeparator & Trim$(Str$(GroupCount)) & FieldSeparator & _
             Format(ISAControlSequence, "000000000") & SegmentTerminator & Chr(13) & Chr(10)
End Function
Private Function getsetting() As Boolean

Dim lcreturn As String
Dim llreturn As Boolean
lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "DataDir", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
DataPath = Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1) & "\"

lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "SenderId", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
SenderId = Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1)

lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "StoreId", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
StoreId = Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1)

lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "StoreName", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
StoreName = Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1)

lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "OutgoingFileDir", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
OutgoingFileDir = Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1) & "\"
OutgoingFileName = Left$(StoreId, 4) & Format$(Month(Date), "00") & Format$(Day(Date), "00") & ".EDI"

lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "ReceiverId", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
ReceiverID = Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1)

lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "ReceiverEMail", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
ReceiverEMail = Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1)

lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "ReceiverName", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
ReceiverName = Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1)

lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "ISAControlSequence", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
If Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1) = "NONE" Then
  ISAControlSequence = 0
Else
  ISAControlSequence = Val(Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1))
End If

lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "GSControlSequence", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
If Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1) = "NONE" Then
  GSControlSequence = 0
Else
  GSControlSequence = Val(Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1))
End If

lcreturn = Space(100)
GetPrivateProfileString "Send Activity", "STControlSequence", "NONE", lcreturn, 160, App.Path & "\SendActi.ini"
If Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1) = "NONE" Then
  STControlSequence = 0
Else
  STControlSequence = Val(Mid$(Trim$(lcreturn), 1, Len(Trim$(lcreturn)) - 1))
End If

llreturn = True
If SenderId = "NONE" Then
  MsgBox "Must specify Phone# in the setting screen", vbOKOnly, "Product Activity"
  llreturn = False
End If
If StoreId = "NONE" Then
  MsgBox "Must specify Store ID in the setting screen", vbOKOnly, "Product Activity"
  llreturn = False
End If
If StoreName = "NONE" Then
  MsgBox "Must specify Store Name in the setting screen", vbOKOnly, "Product Activity"
  llreturn = False
End If
If ReceiverID = "NONE" Then
  MsgBox "Must specify Main Office Phone# in the setting screen", vbOKOnly, "Product Activity"
  llreturn = False
End If
If ReceiverEMail = "NONE" Then
  MsgBox "Must specify Main Office E-Mail address in the setting screen", vbOKOnly, "Product Activity"
  llreturn = False
End If
If ReceiverName = "NONE" Then
  MsgBox "Must specify Main Office Name in the setting screen", vbOKOnly, "Product Activity"
  llreturn = False
End If
If OutgoingFileDir = "NONE" Then
  MsgBox "Must specify outgoing file path in the setting screen", vbOKOnly, "Product Activity"
  llreturn = False
End If
If DataPath = "NONE" Then
  MsgBox "Must specify Data file path in the setting screen", vbOKOnly, "Product Activity"
  llreturn = False
End If
getsetting = llreturn
End Function

Private Sub Class_Initialize()
sendsetting = getsetting()
End Sub

Private Function emailfile() As Boolean
Dim strReg, strReg1 As String
  
strReg = GetVAlue("", "wjsjjjlqmjpjrjjjvpqqkqmqukypoqjquoun", "\899B3E80-6AC6-11cf-8ADB-00AA00C00905")
strReg1 = GetVAlue("", "konhqhioohihphkouimonhqhvnwiqhhhnjti", "\C4145310-469C-11d1-B182-00A0C922E820")
 
On Error GoTo ErrorLn
Dim oSESSION, omessage As Object
If Len(Trim(strReg)) = 0 Or Len(Trim(strReg1)) = 0 Then
  MsgBox "You have no licenses to log on mail system.", vbCritical + vbOKOnly, "Caution"
Else
  Set oSESSION = CreateObject("MSMAPI.MAPISESSION")
  Set omessage = CreateObject("MSMAPI.MAPIMESSAGES")
  emailfile = False
  oSESSION.Action = 1
  If oSESSION.SessionID <> 0 Then
    omessage.SessionID = oSESSION.SessionID
    omessage.Compose
    omessage.RecipIndex = omessage.RecipCount
    omessage.RecipDisplayName = Trim$(ReceiverName)
    omessage.RecipType = 1
    omessage.RecipAddress = Trim$(ReceiverEMail)
    omessage.Action = 13
    omessage.MsgSubject = "Product Activity From: " & Trim$(StoreId) & " " & Trim$(StoreName)
    omessage.AttachmentIndex = omessage.AttachmentCount
    omessage.AttachmentName = OutgoingFileName
    omessage.AttachmentPathName = OutgoingFileDir & OutgoingFileName
    omessage.AttachmentPosition = omessage.AttachmentIndex
    omessage.AttachmentType = 0
    omessage.MsgNoteText = "*"
    omessage.Send
    omessage.Fetch
    emailfile = True
  Else
    emailfile = False
  End If
  Set oSESSION = Nothing
  Set omessage = Nothing
  MsgBox "File " & OutgoingFileDir & OutgoingFileName & " has been sent to " & Trim$(ReceiverName), vbOKOnly, "Send Product Activities"
End If

Exit Function
ErrorLn:
MsgBox "Error while loggin to mail system or mail system Not Installed"
Set oSESSION = Nothing
Set omessage = Nothing
emailfile = False
End Function
