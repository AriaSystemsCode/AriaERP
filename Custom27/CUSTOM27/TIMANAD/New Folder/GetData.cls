VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "GetData"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private strFileName As String
Public SqlWhere As New Fields
Public sqlSelect As New Fields
Public sqlOrderBy As New Fields
Public sqlGroupBy As New Fields

Property Let filename(ByVal mstrfilename As String)
strFileName = mstrfilename
End Property
Property Get filename() As String
filename = strFileName
End Property

Public Function retreivedata(ByVal DataPath As String, ByVal strCursorType As String, ByVal strLockType As String) As ADODB.Recordset

Dim intLoop As Integer
Dim strFields As String
Dim strwhere As String
Dim strOrderBy As String
Dim strGroupBy As String
Dim strconnenctionstring As String

strFields = ""
For intLoop = 1 To sqlSelect.count
strFields = strFields & sqlSelect.item(intLoop)
If intLoop <> sqlSelect.count Then
  strFields = strFields & ","
End If
Next intLoop

strwhere = ""
For intLoop = 1 To SqlWhere.count
  If intLoop <> 1 Then
    strwhere = strwhere & " and "
  End If
  strwhere = strwhere & SqlWhere.item(intLoop)
Next intLoop
  
strOrderBy = ""
For intLoop = 1 To sqlOrderBy.count
  strOrderBy = strOrderBy & sqlOrderBy.item(intLoop)
  If intLoop <> sqlOrderBy.count Then
    strOrderBy = strOrderBy & ","
  End If
Next intLoop
  
strGroupBy = ""
For intLoop = 1 To sqlGroupBy.count
  strGroupBy = strGroupBy & sqlGroupBy.item(intLoop)
  If intLoop <> sqlGroupBy.count Then
    strGroupBy = strGroupBy & ","
  End If
Next intLoop
 
strconnenctionstring = "SELECT " & strFields & " FROM " & filename
If strwhere <> "" Then
  strconnenctionstring = strconnenctionstring & " WHERE " & strwhere
End If
If strGroupBy <> "" Then
  strconnenctionstring = strconnenctionstring & " GROUP BY " & strGroupBy
End If
If strOrderBy <> "" Then
  strconnenctionstring = strconnenctionstring & " ORDER BY " & strOrderBy
End If

Dim ObjConnection As New ADODB.Connection
Dim objRecordSet  As New ADODB.Recordset
Dim strCnn As String
strCnn = "Provider=MSDATASHAPE.1;DSN=Visual FoxPro Database;UID=;SourceDB=" & DataPath & _
";SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=Yes;"

ObjConnection.Open strCnn
objRecordSet.Open strconnenctionstring, ObjConnection, strCursorType, strLockType ', adCmdText
Set retreivedata = objRecordSet
End Function
