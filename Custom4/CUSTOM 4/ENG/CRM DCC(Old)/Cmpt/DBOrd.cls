VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "DBOrd"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Const OBJECT_CODE As String = ""

Public Function Add(ByVal strConParameter As String, _
                    Optional ByVal strChapterID As String = "", _
                    Optional ByVal bolThisOnly As Boolean = False) As ADODB.Recordset


  'strConParameter = "Provider=MSDATASHAPE;DSN=CRM;SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=YES;"
    
  Dim strSql As String
  Dim strOrdHead As String
  Dim strOrdLin As String

  strOrdHead = "SELECT"
  strOrdHead = strOrdHead & " CORDTYPE,"
  strOrdHead = strOrdHead & " Order,"
  strOrdHead = strOrdHead & " Status,"
  strOrdHead = strOrdHead & " ACCOUNT,"
  strOrdHead = strOrdHead & " STORE,"
  strOrdHead = strOrdHead & " DEPT,"
  strOrdHead = strOrdHead & " MULTIPO,"
  strOrdHead = strOrdHead & " CUSTPO,"
  strOrdHead = strOrdHead & " NOTE1,"
  strOrdHead = strOrdHead & " NOTE2,"
  strOrdHead = strOrdHead & " PRIORITY,"
  strOrdHead = strOrdHead & " CORDERCAT,"
  strOrdHead = strOrdHead & " SEASON,"
  strOrdHead = strOrdHead & " CDIVISION,"
  strOrdHead = strOrdHead & " BULK,"
  strOrdHead = strOrdHead & " CREORDER,"
  strOrdHead = strOrdHead & " CONSOL,"
  strOrdHead = strOrdHead & " MULTI,"
  strOrdHead = strOrdHead & " CTERMCODE,"
  strOrdHead = strOrdHead & " SHIPVIA,"
  strOrdHead = strOrdHead & " SPCINST,"
  strOrdHead = strOrdHead & " STNAME,"
  strOrdHead = strOrdHead & " CINSUR,"
  strOrdHead = strOrdHead & " BUYER,"
  strOrdHead = strOrdHead & " PHONE,"
  strOrdHead = strOrdHead & " CFACCODE,"
  strOrdHead = strOrdHead & " FACTACCT,"
  strOrdHead = strOrdHead & " APPROVAL,"
  strOrdHead = strOrdHead & " APPRAMT,"
  strOrdHead = strOrdHead & " DECL_DATE,"
  strOrdHead = strOrdHead & " DECL_CODE,"
  strOrdHead = strOrdHead & " REP1,"
  strOrdHead = strOrdHead & " COMM1,"
  strOrdHead = strOrdHead & " REP2,"
  strOrdHead = strOrdHead & " COMM2,"
  strOrdHead = strOrdHead & " ENTERED,"
  strOrdHead = strOrdHead & " START,"
  strOrdHead = strOrdHead & " COMPLETE,"
  strOrdHead = strOrdHead & " CANCELLED,"
  strOrdHead = strOrdHead & " DISC,"
  strOrdHead = strOrdHead & " BOOK,"
  strOrdHead = strOrdHead & " BOOKAMT,"
  strOrdHead = strOrdHead & " Cancel,"
  strOrdHead = strOrdHead & " CANCELAMT,"
  strOrdHead = strOrdHead & " SHIP,"
  strOrdHead = strOrdHead & " SHIPAMT,"
  strOrdHead = strOrdHead & " OPEN,"
  strOrdHead = strOrdHead & " OPENAMT,"
  strOrdHead = strOrdHead & " LASTLINE,"
  strOrdHead = strOrdHead & " TOTCUT,"
  strOrdHead = strOrdHead & " FLAG,"
  strOrdHead = strOrdHead & " LINK_CODE,"
  strOrdHead = strOrdHead & " GL_SALES,"
  strOrdHead = strOrdHead & " INT_VEND,"
  strOrdHead = strOrdHead & " EVENT_COD,"
  strOrdHead = strOrdHead & " BILLNO,"
  strOrdHead = strOrdHead & " MERC_TYPE,"
  strOrdHead = strOrdHead & " BLANK_ORD,"
  strOrdHead = strOrdHead & " DISTRB_NO,"
  strOrdHead = strOrdHead & " CCLASS,"
  strOrdHead = strOrdHead & " MON_FLG,"
  strOrdHead = strOrdHead & " LABELS,"
  strOrdHead = strOrdHead & " ALT_SHPTO,"
  strOrdHead = strOrdHead & " CWARECODE,"
  strOrdHead = strOrdHead & " CCANCRESON,"
  strOrdHead = strOrdHead & " CADDRESS1,"
  strOrdHead = strOrdHead & " CADDRESS2,"
  strOrdHead = strOrdHead & " CADDRESS3,"
  strOrdHead = strOrdHead & " CADDRESS4,"
  strOrdHead = strOrdHead & " CADDRESS5,"
  strOrdHead = strOrdHead & " CCURRCODE,"
  strOrdHead = strOrdHead & " NEXRATE,"
  strOrdHead = strOrdHead & " NCURRUNIT,"
  strOrdHead = strOrdHead & " CFROMORDER,"
  strOrdHead = strOrdHead & " DIRECT_INV,"
  strOrdHead = strOrdHead & " LHASNOTES,"
  strOrdHead = strOrdHead & " CADD_USER,"
  strOrdHead = strOrdHead & " CADD_TIME,"
  strOrdHead = strOrdHead & " DADD_DATE,"
  strOrdHead = strOrdHead & " LLOK_STAT,"
  strOrdHead = strOrdHead & " CLOK_USER,"
  strOrdHead = strOrdHead & " DLOK_DATE,"
  strOrdHead = strOrdHead & " CLOK_TIME,"
  strOrdHead = strOrdHead & " LEDIORDER,"
  strOrdHead = strOrdHead & " LFROMWEB,"
  strOrdHead = strOrdHead & " CCONTREF,"
  strOrdHead = strOrdHead & " CWEBID,"
  strOrdHead = strOrdHead & " CWEBNAME,"
  strOrdHead = strOrdHead & " COWNER"
  
  strOrdHead = strOrdHead & " FROM ordhdr where 1=0"
  
  strOrdLin = "select"
  strOrdLin = strOrdLin & " CORDTYPE,"
  strOrdLin = strOrdLin & " Order,"
  strOrdLin = strOrdLin & " ACCOUNT,"
  strOrdLin = strOrdLin & " CWARECODE,"
  strOrdLin = strOrdLin & " LINENO,"
  strOrdLin = strOrdLin & " SHIPMENTS,"
  strOrdLin = strOrdLin & " STORE,"
  strOrdLin = strOrdLin & " CUSTPO,"
  strOrdLin = strOrdLin & " Style,"
  strOrdLin = strOrdLin & " ALTSTYLE,"
  strOrdLin = strOrdLin & " ALTCOLOR,"
  strOrdLin = strOrdLin & " SEASON,"
  strOrdLin = strOrdLin & " DESC1,"
  strOrdLin = strOrdLin & " Scale,"
  strOrdLin = strOrdLin & " Group,"
  strOrdLin = strOrdLin & " PREPAK,"
  strOrdLin = strOrdLin & " PPQTY,"
  strOrdLin = strOrdLin & " PRICE,"
  strOrdLin = strOrdLin & " NSUGRETPRI,"
  strOrdLin = strOrdLin & " GROS_PRICE,"
  strOrdLin = strOrdLin & " DISC_PCNT,"
  strOrdLin = strOrdLin & " START,"
  strOrdLin = strOrdLin & " COMPLETE,"
  strOrdLin = strOrdLin & " QTY1,"
  strOrdLin = strOrdLin & " QTY2,"
  strOrdLin = strOrdLin & " QTY3,"
  strOrdLin = strOrdLin & " QTY4,"
  strOrdLin = strOrdLin & " QTY5,"
  strOrdLin = strOrdLin & " QTY6,"
  strOrdLin = strOrdLin & " QTY7,"
  strOrdLin = strOrdLin & " QTY8,"
  strOrdLin = strOrdLin & " TOTQTY,"
  strOrdLin = strOrdLin & " BOOK1,"
  strOrdLin = strOrdLin & " BOOK2,"
  strOrdLin = strOrdLin & " BOOK3,"
  strOrdLin = strOrdLin & " BOOK4,"
  strOrdLin = strOrdLin & " BOOK5,"
  strOrdLin = strOrdLin & " BOOK6,"
  strOrdLin = strOrdLin & " BOOK7,"
  strOrdLin = strOrdLin & " BOOK8,"
  strOrdLin = strOrdLin & " TOTBOOK,"
  strOrdLin = strOrdLin & " PIK1,"
  strOrdLin = strOrdLin & " PIK2,"
  strOrdLin = strOrdLin & " PIK3,"
  strOrdLin = strOrdLin & " PIK4,"
  strOrdLin = strOrdLin & " PIK5,"
  strOrdLin = strOrdLin & " PIK6,"
  strOrdLin = strOrdLin & " PIK7,"
  strOrdLin = strOrdLin & " PIK8,"
  strOrdLin = strOrdLin & " TOTPIK,"
  strOrdLin = strOrdLin & " PIKTKT,"
  strOrdLin = strOrdLin & " PIKDATE,"
  strOrdLin = strOrdLin & " PICKED,"
  strOrdLin = strOrdLin & " INVOICE,"
  strOrdLin = strOrdLin & " INVDATE,"
  strOrdLin = strOrdLin & " FLAG,"
  strOrdLin = strOrdLin & " CUTTKT,"
  strOrdLin = strOrdLin & " DYELOT,"
  strOrdLin = strOrdLin & " ALLOCATED,"
  strOrdLin = strOrdLin & " CUT1,"
  strOrdLin = strOrdLin & " CUT2,"
  strOrdLin = strOrdLin & " CUT3,"
  strOrdLin = strOrdLin & " CUT4,"
  strOrdLin = strOrdLin & " CUT5,"
  strOrdLin = strOrdLin & " CUT6,"
  strOrdLin = strOrdLin & " CUT7,"
  strOrdLin = strOrdLin & " CUT8,"
  strOrdLin = strOrdLin & " TOTCUT,"
  strOrdLin = strOrdLin & " POALO1,"
  strOrdLin = strOrdLin & " POALO2,"
  strOrdLin = strOrdLin & " POALO3,"
  strOrdLin = strOrdLin & " POALO4,"
  strOrdLin = strOrdLin & " POALO5,"
  strOrdLin = strOrdLin & " POALO6,"
  strOrdLin = strOrdLin & " POALO7,"
  strOrdLin = strOrdLin & " POALO8,"
  strOrdLin = strOrdLin & " TOT_POALO,"
  strOrdLin = strOrdLin & " NPCK1,"
  strOrdLin = strOrdLin & " NPCK2,"
  strOrdLin = strOrdLin & " NPCK3,"
  strOrdLin = strOrdLin & " NPCK4,"
  strOrdLin = strOrdLin & " NPCK5,"
  strOrdLin = strOrdLin & " NPCK6,"
  strOrdLin = strOrdLin & " NPCK7,"
  strOrdLin = strOrdLin & " NPCK8,"
  strOrdLin = strOrdLin & " NPWGHT,"
  strOrdLin = strOrdLin & " NOTE_MEM,"
  strOrdLin = strOrdLin & " COMM1,"
  strOrdLin = strOrdLin & " COMM2,"
  strOrdLin = strOrdLin & " PO,"
  strOrdLin = strOrdLin & " COST,"
  strOrdLin = strOrdLin & " GL_SALES,"
  strOrdLin = strOrdLin & " GL_COST,"
  strOrdLin = strOrdLin & " PACK_ID,"
  strOrdLin = strOrdLin & " NPOLINENO,"
  strOrdLin = strOrdLin & " CFROMORDER,"
  strOrdLin = strOrdLin & " BULKLINENO,"
  strOrdLin = strOrdLin & " CLINESTAT,"
  strOrdLin = strOrdLin & " CADD_USER,"
  strOrdLin = strOrdLin & " CADD_TIME,"
  strOrdLin = strOrdLin & " DADD_DATE,"
  strOrdLin = strOrdLin & " LLOK_STAT,"
  strOrdLin = strOrdLin & " CLOK_USER,"
  strOrdLin = strOrdLin & " DLOK_DATE,"
  strOrdLin = strOrdLin & " COWNER,"
  strOrdLin = strOrdLin & " CLOK_TIME"
  
  strOrdLin = strOrdLin & " from ordline where 1=0"
  
  strSql = "SHAPE  {" + strOrdHead + "} APPEND ({" + strOrdLin + "} AS OrderDetail RELATE order TO order)"
  'strSql = strOrdLin
  Dim strErrMsg As String
  Dim objconnection As ADODB.Connection
  Set objconnection = New ADODB.Connection
  objconnection.Open (strConParameter)
            
  Dim objRecSet As ADODB.Recordset
  Set objRecSet = New ADODB.Recordset
  
  objRecSet.CursorLocation = adUseClient
  objRecSet.CursorType = adOpenStatic
  objRecSet.LockType = adLockBatchOptimistic
  objRecSet.Open strSql, objconnection
  
  'objRecSet.UpdateBatch
  
  Set objRecSet.ActiveConnection = Nothing
  objconnection.Close
  
  Set objconnection = Nothing
  If objRecSet.RecordCount = 0 Then
    objRecSet.AddNew
  Else
    Set objRecSet = Nothing
    strErrMsg = "Error"
  End If
  Set Add = objRecSet



End Function
Public Function Load(ByVal strConParameter As String, _
                     ByVal strKey As String, _
                     Optional ByVal strChapterID As String = "", _
                     Optional ByVal bolThisOnly As Boolean = False) As ADODB.Recordset
                     
                         
  'Start from here
  'strConParameter = "Provider=MSDATASHAPE;DSN=CRM;SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=No;Deleted=YES;"
    
  Dim strSql As String
  Dim strOrdHead As String
  Dim strOrdLin As String

  strOrdHead = "SELECT"
  strOrdHead = strOrdHead & " CORDTYPE,"
  strOrdHead = strOrdHead & " Order,"
  strOrdHead = strOrdHead & " Status,"
  strOrdHead = strOrdHead & " ACCOUNT,"
  strOrdHead = strOrdHead & " STORE,"
  strOrdHead = strOrdHead & " DEPT,"
  strOrdHead = strOrdHead & " MULTIPO,"
  strOrdHead = strOrdHead & " CUSTPO,"
  strOrdHead = strOrdHead & " NOTE1,"
  strOrdHead = strOrdHead & " NOTE2,"
  strOrdHead = strOrdHead & " PRIORITY,"
  strOrdHead = strOrdHead & " CORDERCAT,"
  strOrdHead = strOrdHead & " SEASON,"
  strOrdHead = strOrdHead & " CDIVISION,"
  strOrdHead = strOrdHead & " BULK,"
  strOrdHead = strOrdHead & " CREORDER,"
  strOrdHead = strOrdHead & " CONSOL,"
  strOrdHead = strOrdHead & " MULTI,"
  strOrdHead = strOrdHead & " CTERMCODE,"
  strOrdHead = strOrdHead & " SHIPVIA,"
  strOrdHead = strOrdHead & " SPCINST,"
  strOrdHead = strOrdHead & " STNAME,"
  strOrdHead = strOrdHead & " CINSUR,"
  strOrdHead = strOrdHead & " BUYER,"
  strOrdHead = strOrdHead & " PHONE,"
  strOrdHead = strOrdHead & " CFACCODE,"
  strOrdHead = strOrdHead & " FACTACCT,"
  strOrdHead = strOrdHead & " APPROVAL,"
  strOrdHead = strOrdHead & " APPRAMT,"
  strOrdHead = strOrdHead & " DECL_DATE,"
  strOrdHead = strOrdHead & " DECL_CODE,"
  strOrdHead = strOrdHead & " REP1,"
  strOrdHead = strOrdHead & " COMM1,"
  strOrdHead = strOrdHead & " REP2,"
  strOrdHead = strOrdHead & " COMM2,"
  strOrdHead = strOrdHead & " ENTERED,"
  strOrdHead = strOrdHead & " START,"
  strOrdHead = strOrdHead & " COMPLETE,"
  strOrdHead = strOrdHead & " CANCELLED,"
  strOrdHead = strOrdHead & " DISC,"
  strOrdHead = strOrdHead & " BOOK,"
  strOrdHead = strOrdHead & " BOOKAMT,"
  strOrdHead = strOrdHead & " Cancel,"
  strOrdHead = strOrdHead & " CANCELAMT,"
  strOrdHead = strOrdHead & " SHIP,"
  strOrdHead = strOrdHead & " SHIPAMT,"
  strOrdHead = strOrdHead & " OPEN,"
  strOrdHead = strOrdHead & " OPENAMT,"
  strOrdHead = strOrdHead & " LASTLINE,"
  strOrdHead = strOrdHead & " TOTCUT,"
  strOrdHead = strOrdHead & " FLAG,"
  strOrdHead = strOrdHead & " LINK_CODE,"
  strOrdHead = strOrdHead & " GL_SALES,"
  strOrdHead = strOrdHead & " INT_VEND,"
  strOrdHead = strOrdHead & " EVENT_COD,"
  strOrdHead = strOrdHead & " BILLNO,"
  strOrdHead = strOrdHead & " MERC_TYPE,"
  strOrdHead = strOrdHead & " BLANK_ORD,"
  strOrdHead = strOrdHead & " DISTRB_NO,"
  strOrdHead = strOrdHead & " CCLASS,"
  strOrdHead = strOrdHead & " MON_FLG,"
  strOrdHead = strOrdHead & " LABELS,"
  strOrdHead = strOrdHead & " ALT_SHPTO,"
  strOrdHead = strOrdHead & " CWARECODE,"
  strOrdHead = strOrdHead & " CCANCRESON,"
  strOrdHead = strOrdHead & " CADDRESS1,"
  strOrdHead = strOrdHead & " CADDRESS2,"
  strOrdHead = strOrdHead & " CADDRESS3,"
  strOrdHead = strOrdHead & " CADDRESS4,"
  strOrdHead = strOrdHead & " CADDRESS5,"
  strOrdHead = strOrdHead & " CCURRCODE,"
  strOrdHead = strOrdHead & " NEXRATE,"
  strOrdHead = strOrdHead & " NCURRUNIT,"
  strOrdHead = strOrdHead & " CFROMORDER,"
  strOrdHead = strOrdHead & " DIRECT_INV,"
  strOrdHead = strOrdHead & " LHASNOTES,"
  strOrdHead = strOrdHead & " CADD_USER,"
  strOrdHead = strOrdHead & " CADD_TIME,"
  strOrdHead = strOrdHead & " DADD_DATE,"
  strOrdHead = strOrdHead & " LLOK_STAT,"
  strOrdHead = strOrdHead & " CLOK_USER,"
  strOrdHead = strOrdHead & " DLOK_DATE,"
  strOrdHead = strOrdHead & " CLOK_TIME,"
  strOrdHead = strOrdHead & " LEDIORDER,"
  strOrdHead = strOrdHead & " COWNER"
  
  strOrdHead = strOrdHead & " FROM ordhdr " & strKey
  
  strOrdLin = "select"
  strOrdLin = strOrdLin & " CORDTYPE,"
  strOrdLin = strOrdLin & " Order,"
  strOrdLin = strOrdLin & " ACCOUNT,"
  strOrdLin = strOrdLin & " CWARECODE,"
  strOrdLin = strOrdLin & " LINENO,"
  strOrdLin = strOrdLin & " SHIPMENTS,"
  strOrdLin = strOrdLin & " STORE,"
  strOrdLin = strOrdLin & " CUSTPO,"
  strOrdLin = strOrdLin & " Style,"
  strOrdLin = strOrdLin & " ALTSTYLE,"
  strOrdLin = strOrdLin & " ALTCOLOR,"
  strOrdLin = strOrdLin & " SEASON,"
  strOrdLin = strOrdLin & " DESC1,"
  strOrdLin = strOrdLin & " Scale,"
  strOrdLin = strOrdLin & " Group,"
  strOrdLin = strOrdLin & " PREPAK,"
  strOrdLin = strOrdLin & " PPQTY,"
  strOrdLin = strOrdLin & " PRICE,"
  strOrdLin = strOrdLin & " NSUGRETPRI,"
  strOrdLin = strOrdLin & " GROS_PRICE,"
  strOrdLin = strOrdLin & " DISC_PCNT,"
  strOrdLin = strOrdLin & " START,"
  strOrdLin = strOrdLin & " COMPLETE,"
  strOrdLin = strOrdLin & " QTY1,"
  strOrdLin = strOrdLin & " QTY2,"
  strOrdLin = strOrdLin & " QTY3,"
  strOrdLin = strOrdLin & " QTY4,"
  strOrdLin = strOrdLin & " QTY5,"
  strOrdLin = strOrdLin & " QTY6,"
  strOrdLin = strOrdLin & " QTY7,"
  strOrdLin = strOrdLin & " QTY8,"
  strOrdLin = strOrdLin & " TOTQTY,"
  strOrdLin = strOrdLin & " BOOK1,"
  strOrdLin = strOrdLin & " BOOK2,"
  strOrdLin = strOrdLin & " BOOK3,"
  strOrdLin = strOrdLin & " BOOK4,"
  strOrdLin = strOrdLin & " BOOK5,"
  strOrdLin = strOrdLin & " BOOK6,"
  strOrdLin = strOrdLin & " BOOK7,"
  strOrdLin = strOrdLin & " BOOK8,"
  strOrdLin = strOrdLin & " TOTBOOK,"
  strOrdLin = strOrdLin & " PIK1,"
  strOrdLin = strOrdLin & " PIK2,"
  strOrdLin = strOrdLin & " PIK3,"
  strOrdLin = strOrdLin & " PIK4,"
  strOrdLin = strOrdLin & " PIK5,"
  strOrdLin = strOrdLin & " PIK6,"
  strOrdLin = strOrdLin & " PIK7,"
  strOrdLin = strOrdLin & " PIK8,"
  strOrdLin = strOrdLin & " TOTPIK,"
  strOrdLin = strOrdLin & " PIKTKT,"
  strOrdLin = strOrdLin & " PIKDATE,"
  strOrdLin = strOrdLin & " PICKED,"
  strOrdLin = strOrdLin & " INVOICE,"
  strOrdLin = strOrdLin & " INVDATE,"
  strOrdLin = strOrdLin & " FLAG,"
  strOrdLin = strOrdLin & " CUTTKT,"
  strOrdLin = strOrdLin & " DYELOT,"
  strOrdLin = strOrdLin & " ALLOCATED,"
  strOrdLin = strOrdLin & " CUT1,"
  strOrdLin = strOrdLin & " CUT2,"
  strOrdLin = strOrdLin & " CUT3,"
  strOrdLin = strOrdLin & " CUT4,"
  strOrdLin = strOrdLin & " CUT5,"
  strOrdLin = strOrdLin & " CUT6,"
  strOrdLin = strOrdLin & " CUT7,"
  strOrdLin = strOrdLin & " CUT8,"
  strOrdLin = strOrdLin & " TOTCUT,"
  strOrdLin = strOrdLin & " POALO1,"
  strOrdLin = strOrdLin & " POALO2,"
  strOrdLin = strOrdLin & " POALO3,"
  strOrdLin = strOrdLin & " POALO4,"
  strOrdLin = strOrdLin & " POALO5,"
  strOrdLin = strOrdLin & " POALO6,"
  strOrdLin = strOrdLin & " POALO7,"
  strOrdLin = strOrdLin & " POALO8,"
  strOrdLin = strOrdLin & " TOT_POALO,"
  strOrdLin = strOrdLin & " NPCK1,"
  strOrdLin = strOrdLin & " NPCK2,"
  strOrdLin = strOrdLin & " NPCK3,"
  strOrdLin = strOrdLin & " NPCK4,"
  strOrdLin = strOrdLin & " NPCK5,"
  strOrdLin = strOrdLin & " NPCK6,"
  strOrdLin = strOrdLin & " NPCK7,"
  strOrdLin = strOrdLin & " NPCK8,"
  strOrdLin = strOrdLin & " NPWGHT,"
  strOrdLin = strOrdLin & " NOTE_MEM,"
  strOrdLin = strOrdLin & " COMM1,"
  strOrdLin = strOrdLin & " COMM2,"
  strOrdLin = strOrdLin & " PO,"
  strOrdLin = strOrdLin & " COST,"
  strOrdLin = strOrdLin & " GL_SALES,"
  strOrdLin = strOrdLin & " GL_COST,"
  strOrdLin = strOrdLin & " PACK_ID,"
  strOrdLin = strOrdLin & " NPOLINENO,"
  strOrdLin = strOrdLin & " CFROMORDER,"
  strOrdLin = strOrdLin & " BULKLINENO,"
  strOrdLin = strOrdLin & " CLINESTAT,"
  strOrdLin = strOrdLin & " CADD_USER,"
  strOrdLin = strOrdLin & " CADD_TIME,"
  strOrdLin = strOrdLin & " DADD_DATE,"
  strOrdLin = strOrdLin & " LLOK_STAT,"
  strOrdLin = strOrdLin & " CLOK_USER,"
  strOrdLin = strOrdLin & " DLOK_DATE,"
  strOrdLin = strOrdLin & " COWNER,"
  strOrdLin = strOrdLin & " CLOK_TIME"
  
  strOrdLin = strOrdLin & " from ordline " & strKey
  
  'strOrdLin = strOrdLin & " from ordline order by order" & strKey
  
  strSql = "SHAPE  {" + strOrdHead + "} APPEND ({" + strOrdLin + "} AS OrderDetail RELATE order TO order )"
  
  Dim objconnection As ADODB.Connection
  Set objconnection = New ADODB.Connection
  objconnection.Open (strConParameter)
  
  Dim objRecSet As ADODB.Recordset
  Set objRecSet = New ADODB.Recordset
  
  objRecSet.CursorLocation = adUseClient
  objRecSet.CursorType = adOpenStatic
  objRecSet.LockType = adLockBatchOptimistic
  objRecSet.Open strSql, objconnection
  
  Set objRecSet.ActiveConnection = Nothing
  objconnection.Close
  Dim strErrMsg As String
  Set objconnection = Nothing
  If objRecSet.RecordCount = 0 Then
     Set objRecSet = Nothing
     strErrMsg = "Record(s) not found."
  End If
  Set Load = objRecSet

  
End Function
Public Function Save(ByVal strConParameter As String, _
                     ByVal objRecordset As Recordset) As Boolean

  'strConParameter = "Provider=MSDATASHAPE;Data Provider=SQLOLEDB;Initial Catalog=OpenIssue;Data Source=ARIA-DEV;uid=sa;pwd="
  
  Dim objconnection As ADODB.Connection
  Set objconnection = New ADODB.Connection
  objconnection.Open (strConParameter)
  
  Dim rsChild As ADODB.Recordset
  Set rsChild = objRecordset!OrderDetail.Value
  rsChild.ActiveConnection = objconnection
  
  rsChild.UpdateBatch
  rsChild.Close
  Set rsChild = Nothing
  
  Set objRecordset.ActiveConnection = objconnection
  objRecordset.UpdateBatch
  
  Set objconnection = Nothing

  Save = True

End Function
Public Function Delete(ByVal strConParameter As String, _
                       ByVal strKey As String) As Boolean

End Function
Public Function Move(ByVal strConParameter As String, _
                     ByVal strWhere As String, _
                     Optional ByVal strOrderBy As String = "") As ADODB.Recordset

End Function
Public Function MoveFirst(ByVal strConParameter As String, _
                          Optional ByVal strChapterID As String = "", _
                          Optional ByVal bolThisOnly As Boolean = False, _
                          Optional ByVal strOrderBy As String = "") As ADODB.Recordset

End Function
Public Function MoveLast(ByVal strConParameter As String, _
                         Optional ByVal strChapterID As String = "", _
                         Optional ByVal bolThisOnly As Boolean = False, _
                         Optional ByVal strOrderBy As String = "") As ADODB.Recordset

End Function
Public Function MoveNext(ByVal strConParameter As String, _
                         ByVal strKey As String, _
                         Optional ByVal strChapterID As String = "", _
                         Optional ByVal bolThisOnly As Boolean = False, _
                         Optional ByVal strOrderBy As String = "") As ADODB.Recordset

End Function
Public Function Moveprevious(ByVal strConParameter As String, _
                             ByVal strKey As String, _
                             Optional ByVal strChapterID As String = "", _
                             Optional ByVal bolThisOnly As Boolean = False, _
                             Optional ByVal strOrderBy As String = "") As ADODB.Recordset

End Function
