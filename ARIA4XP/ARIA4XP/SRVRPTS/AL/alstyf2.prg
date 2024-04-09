*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*PARAMETERS lcRequestID, lcXMLFileName
PARAMETERS lcRequestID, lcXMLFileName, ClientID
*T20100512.0026 Hassan 2010 05 23 [END]

PRIVATE loAgent
loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

LOCAL loEnvironment
loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
loEnvironment.ClientID = ClientID
loEnvironment.ConnectionsRefresh()
*T20100512.0026 Hassan 2010 05 23 [END]

LOCAL lcCurrentProcedure
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)
 DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID), ClientID
*T20100512.0026 Hassan 2010 05 23 [END]

PRIVATE loProgress
loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

loProgress.Percent = 0
loProgress.Description = "Opening Data Files..."
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*loAgent.UpdateObjectProgress(lcRequestID, loProgress)
loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
*T20100512.0026 Hassan 2010 05 23 [END]


lcOldLib = SET("Classlib")
PUBLIC  goAriaEnvironment
goAriaEnvironment = CREATEOBJECT("AriaEnvironment", loAgent.GetRequestCompany(lcRequestID, ClientID))

lfRestFromXML(FILETOSTR(lcXMLFileName))
*oAriaEnvironment.Xml.RestoreFromXMl(lcXMLVariable)


* MAH
*-- goAriaEnvironment.gcDevice = 'FILE'
IF LEFT(gcDevice, 7) = "PRINTER"
  goAriaEnvironment.gcDevice = "PRINTER"
ELSE
  goAriaEnvironment.gcDevice = "FILE"
ENDIF
goAriaEnvironment.gcDevice = 'FILE'
* MAH

lcOrdlTmp = goAriaEnvironment.Cursors.GetCursorTempName()
lcStyTemp = goAriaEnvironment.Cursors.GetCursorTempName()

goAriaEnvironment.remotetableaccess.OpenTable(goAriaEnvironment.DataDir + 'PIKTKT' ,'PIKTKT','SH')
goAriaEnvironment.remotetableaccess.OpenTable(goAriaEnvironment.DataDir + 'ORDLINE' ,'ORDLINE','SH')
goAriaEnvironment.remotetableaccess.OpenTable(goAriaEnvironment.DataDir + 'STYLE' ,'STYLE','SH','Style_X')
goAriaEnvironment.remotetableaccess.OpenTable(goAriaEnvironment.DataDir + 'CUSTOMER' ,'CUSTOMER','SH')
goAriaEnvironment.remotetableaccess.OpenTable(goAriaEnvironment.DataDir + 'ORDHDR' ,'ORDHDR','SH')
goAriaEnvironment.remotetableaccess.OpenTable(goAriaEnvironment.DataDir + 'Scale' ,'Scale','SH')


*IF looGscroll.llOGFltCh
  lfCrtTemp()
  lfCollectData()
*ELSE
* USE oAriaEnvironment.WorkDir +  lcOrdlTmp + ".DBF" IN 0  
*ENDIF   

IF USED(lcOrdlTmp) AND RECCOUNT(lcOrdlTmp) = 0
  *=gfModalGen('TRM00052B40011','ALERT')
  USE IN (lcOrdlTmp)
  RETURN 
ELSE
  IF USED(lcOrdlTmp)
    USE IN (lcOrdlTmp)
  ENDIF 
ENDIF 

*lodisplayreport  = CREATEOBJECT("Report")
goAriaEnvironment.report.OGLastForm = 'ALSTYAL'

*goAriaEnvironment.report.llCrystal = .T.
*goAriaEnvironment.report.llCrystal = .F.

goAriaEnvironment.report.OGPlatform = 'WINDOWS'

DIMENSION goAriaEnvironment.report.laCRParams[2,2]

goAriaEnvironment.report.laCRParams[1,1] = 'ReportName'
goAriaEnvironment.report.laCRParams[1,2] = 'Allocation Report By Style' 

*llMultWH = .T.

goAriaEnvironment.report.laCRParams[2,1] = 'llMultWH'
goAriaEnvironment.report.laCRParams[2,2] = IIF(llMultWH,1,0)

*goAriaEnvironment.report.cCROrientation = 'P'


goAriaEnvironment.report.cTextRepType  = goAriaEnvironment.cTextRepType
*goAriaEnvironment.report.cTextRepType = 'HTM'


DIMENSION goAriaEnvironment.report.lacrTABLES[1]  && array For Temp Table & pathes 
goAriaEnvironment.report.lacrTABLES[1]= goAriaEnvironment.WorkDir+lcOrdlTmp+'.DBF' 


loProgress.Percent = 0.9
loProgress.Description = "Printing Report..."
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*loAgent.UpdateObjectProgress(lcRequestID, loProgress)
loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
*T20100512.0026 Hassan 2010 05 23 [END]

PRIVATE loProxy
*loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

*HUS
*IF loProxy.GetRequest(lcRequestID, ClientID).Status = 3
*HUS
  goAriaEnvironment.report.print(goAriaEnvironment.report.OGLastForm)

  loProgress.Percent = 1.0
  loProgress.Description = "Printing Report..."
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
   loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  *T20100512.0026 Hassan 2010 05 23 [END]
  
*HUS
*ENDIF
*HUS

SET CLASSLIB TO &lcOldLib 
*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/15/2007
*! Purpose   : Printing loop for the main report
*!*************************************************************
FUNCTION lfCrtTemp


IF USED(lcOrdlTmp)
 SELECT (lcOrdlTmp)
 ZAP 
ENDIF 

*CREATE the temp file to be bounded with the crystal report
DIMENSION laTableStruct[62,4]

laTableStruct[1,1] = 'Style'
laTableStruct[1,2] = "C"
laTableStruct[1,3] = 19
laTableStruct[1,4] = 0


laTableStruct[2,1] = 'BEGN1'
laTableStruct[2,2] = "N"
laTableStruct[2,3] = 7
laTableStruct[2,4] = 0

laTableStruct[3,1] = 'BEGN2'
laTableStruct[3,2] = "N"
laTableStruct[3,3] = 7
laTableStruct[3,4] = 0

laTableStruct[4,1] = 'BEGN3'
laTableStruct[4,2] = "N"
laTableStruct[4,3] = 7
laTableStruct[4,4] = 0

laTableStruct[5,1] = 'BEGN4'
laTableStruct[5,2] = "N"
laTableStruct[5,3] = 7
laTableStruct[5,4] = 0

laTableStruct[6,1] = 'BEGN5'
laTableStruct[6,2] = "N"
laTableStruct[6,3] = 7
laTableStruct[6,4] = 0

laTableStruct[7,1] = 'BEGN6'
laTableStruct[7,2] = "N"
laTableStruct[7,3] = 7
laTableStruct[7,4] = 0

laTableStruct[8,1] = 'BEGN7'
laTableStruct[8,2] = "N"
laTableStruct[8,3] = 7
laTableStruct[8,4] = 0

laTableStruct[9,1] = 'BEGN8'
laTableStruct[9,2] = "N"
laTableStruct[9,3] = 7
laTableStruct[9,4] = 0


laTableStruct[10,1] = 'TOTBGN'
laTableStruct[10,2] = "N"
laTableStruct[10,3] = 9
laTableStruct[10,4] = 0


laTableStruct[11,1] = 'Stk1'
laTableStruct[11,2] = "N"
laTableStruct[11,3] = 7
laTableStruct[11,4] = 0


laTableStruct[12,1] = 'Stk2'
laTableStruct[12,2] = "N"
laTableStruct[12,3] = 7
laTableStruct[12,4] = 0

laTableStruct[13,1] = 'Stk3'
laTableStruct[13,2] = "N"
laTableStruct[13,3] = 7
laTableStruct[13,4] = 0


laTableStruct[14,1] = 'Stk4'
laTableStruct[14,2] = "N"
laTableStruct[14,3] = 7
laTableStruct[14,4] = 0


laTableStruct[15,1] = 'Stk5'
laTableStruct[15,2] = "N"
laTableStruct[15,3] = 7
laTableStruct[15,4] = 0

laTableStruct[16,1] = 'Stk6'
laTableStruct[16,2] = "N"
laTableStruct[16,3] = 7
laTableStruct[16,4] = 0

laTableStruct[17,1] = 'Stk7'
laTableStruct[17,2] = "N"
laTableStruct[17,3] = 7
laTableStruct[17,4] = 0

laTableStruct[18,1] = 'Stk8'
laTableStruct[18,2] = "N"
laTableStruct[18,3] = 7
laTableStruct[18,4] = 0

laTableStruct[19,1] = 'totStk'
laTableStruct[19,2] = "N"
laTableStruct[19,3] = 9
laTableStruct[19,4] = 0

laTableStruct[20,1] = 'cWare'
laTableStruct[20,2] = "C"
laTableStruct[20,3] = 6
laTableStruct[20,4] = 0

laTableStruct[21,1] = 'ACCOUNT'
laTableStruct[21,2] = "C"
laTableStruct[21,3] = 5
laTableStruct[21,4] = 0

laTableStruct[22,1] = 'STORE'
laTableStruct[22,2] = "C"
laTableStruct[22,3] = 8
laTableStruct[22,4] = 0

laTableStruct[23,1] = 'STNAME'
laTableStruct[23,2] = "C"
laTableStruct[23,3] = 30
laTableStruct[23,4] = 0

laTableStruct[24,1] = 'PRIORITY'
laTableStruct[24,2] = "C"
laTableStruct[24,3] = 3
laTableStruct[24,4] = 0

laTableStruct[25,1] = 'STATUS'
laTableStruct[25,2] = "C"
laTableStruct[25,3] = 1
laTableStruct[25,4] = 0

laTableStruct[26,1] = 'START'
laTableStruct[26,2] = "D"
laTableStruct[26,3] = 8
laTableStruct[26,4] = 0

laTableStruct[27,1] = 'COMPLETE'
laTableStruct[27,2] = "D"
laTableStruct[27,3] = 8
laTableStruct[27,4] = 0

laTableStruct[28,1] = 'QTY1'
laTableStruct[28,2] = "N"
laTableStruct[28,3] = 7
laTableStruct[28,4] = 0

laTableStruct[29,1] = 'QTY2'
laTableStruct[29,2] = "N"
laTableStruct[29,3] = 7
laTableStruct[29,4] = 0

laTableStruct[30,1] = 'QTY3'
laTableStruct[30,2] = "N"
laTableStruct[30,3] = 7
laTableStruct[30,4] = 0

laTableStruct[31,1] = 'QTY4'
laTableStruct[31,2] = "N"
laTableStruct[31,3] = 7
laTableStruct[31,4] = 0

laTableStruct[32,1] = 'QTY5'
laTableStruct[32,2] = "N"
laTableStruct[32,3] = 7
laTableStruct[32,4] = 0

laTableStruct[33,1] = 'QTY6'
laTableStruct[33,2] = "N"
laTableStruct[33,3] = 7
laTableStruct[33,4] = 0

laTableStruct[34,1] = 'QTY7'
laTableStruct[34,2] = "N"
laTableStruct[34,3] = 7
laTableStruct[34,4] = 0

laTableStruct[35,1] = 'QTY8'
laTableStruct[35,2] = "N"
laTableStruct[35,3] = 7
laTableStruct[35,4] = 0

laTableStruct[36,1] = 'totQTY'
laTableStruct[36,2] = "N"
laTableStruct[36,3] = 7
laTableStruct[36,4] = 0

laTableStruct[36,1] = 'GROUP'
laTableStruct[36,2] = "C"
laTableStruct[36,3] = 1
laTableStruct[36,4] = 0


laTableStruct[37,1] = 'ORDER'
laTableStruct[37,2] = "C"
laTableStruct[37,3] = 6
laTableStruct[37,4] = 0

laTableStruct[38,1] = 'PIK1'
laTableStruct[38,2] = "N"
laTableStruct[38,3] = 7
laTableStruct[38,4] = 0

laTableStruct[39,1] = 'PIK2'
laTableStruct[39,2] = "N"
laTableStruct[39,3] = 7
laTableStruct[39,4] = 0

laTableStruct[40,1] = 'PIK3'
laTableStruct[40,2] = "N"
laTableStruct[40,3] = 7
laTableStruct[40,4] = 0

laTableStruct[41,1] = 'PIK4'
laTableStruct[41,2] = "N"
laTableStruct[41,3] = 7
laTableStruct[41,4] = 0

laTableStruct[42,1] = 'PIK5'
laTableStruct[42,2] = "N"
laTableStruct[42,3] = 7
laTableStruct[42,4] = 0

laTableStruct[43,1] = 'PIK6'
laTableStruct[43,2] = "N"
laTableStruct[43,3] = 7
laTableStruct[43,4] = 0

laTableStruct[44,1] = 'PIK7'
laTableStruct[44,2] = "N"
laTableStruct[44,3] = 7
laTableStruct[44,4] = 0

laTableStruct[45,1] = 'PIK8'
laTableStruct[45,2] = "N"
laTableStruct[45,3] = 7
laTableStruct[45,4] = 0

laTableStruct[46,1] = 'totPIK'
laTableStruct[46,2] = "N"
laTableStruct[46,3] = 7
laTableStruct[46,4] = 0

laTableStruct[47,1] = 'SZ1'
laTableStruct[47,2] = "C"
laTableStruct[47,3] = 5
laTableStruct[47,4] = 0

laTableStruct[48,1] = 'SZ2'
laTableStruct[48,2] = "C"
laTableStruct[48,3] = 7
laTableStruct[48,4] = 0

laTableStruct[49,1] = 'SZ3'
laTableStruct[49,2] = "C"
laTableStruct[49,3] = 5
laTableStruct[49,4] = 0

laTableStruct[50,1] = 'SZ4'
laTableStruct[50,2] = "C"
laTableStruct[50,3] = 5
laTableStruct[50,4] = 0

laTableStruct[51,1] = 'SZ5'
laTableStruct[51,2] = "C"
laTableStruct[51,3] = 5
laTableStruct[51,4] = 0

laTableStruct[52,1] = 'SZ6'
laTableStruct[52,2] = "C"
laTableStruct[52,3] = 5
laTableStruct[52,4] = 0

laTableStruct[53,1] = 'SZ7'
laTableStruct[53,2] = "C"
laTableStruct[53,3] = 5
laTableStruct[53,4] = 0

laTableStruct[54,1] = 'SZ8'
laTableStruct[54,2] = "C"
laTableStruct[54,3] = 5
laTableStruct[54,4] = 0


laTableStruct[55,1] = 'ALO1'
laTableStruct[55,2] = "N"
laTableStruct[55,3] = 7
laTableStruct[55,4] = 0

laTableStruct[56,1] = 'ALO2'
laTableStruct[56,2] = "N"
laTableStruct[56,3] = 7
laTableStruct[56,4] = 0

laTableStruct[57,1] = 'ALO3'
laTableStruct[57,2] = "N"
laTableStruct[57,3] = 7
laTableStruct[57,4] = 0

laTableStruct[58,1] = 'ALO4'
laTableStruct[58,2] = "N"
laTableStruct[58,3] = 7
laTableStruct[58,4] = 0

laTableStruct[59,1] = 'ALO5'
laTableStruct[59,2] = "N"
laTableStruct[59,3] = 7
laTableStruct[59,4] = 0

laTableStruct[60,1] = 'ALO6'
laTableStruct[60,2] = "N"
laTableStruct[60,3] = 7
laTableStruct[60,4] = 0

laTableStruct[61,1] = 'ALO7'
laTableStruct[61,2] = "N"
laTableStruct[61,3] = 7
laTableStruct[61,4] = 0

laTableStruct[62,1] = 'ALO8'
laTableStruct[62,2] = "N"
laTableStruct[62,3] = 7
laTableStruct[62,4] = 0


goAriaEnvironment.Cursors.createcursor(lcOrdlTmp ,@laTableStruct,'Style',lcOrdlTmp ,.F.) 
*=gfCrtTmp(lcOrdlTmp ,@laTableStruct,'Style',lcOrdlTmp ,.F.)
*!*************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar[MMT]
*! Date      : 15/07/2007
*! Purpose   : Collecting report data
*!*************************************************************
FUNCTION lfCollectData

IF USED(lcStyTemp) AND RECCOUNT(lcStyTemp) > 0
  USE IN (lcStyTemp)
ENDIF

SELECT Style_X
DIMENSION laFileSt[1,18]
=AFIELDS(laFileSt)

goAriaEnvironment.Cursors.createcursor(lcStyTemp ,@laFileSt,'Style',lcStyTemp,.T.) 
*=gfCrtTmp(lcStyTemp ,@laFileSt,'Style',lcStyTemp,.T.)

lnMajorLen = LEN(lcStylePic)
lnPosStyle = ASCAN(laogFXflt,'STYLE.CSTYMAJOR')
lcStyFile = ''
llSeleStyle = .F.
IF lnPosStyle <> 0 
  lnPosStyle = ASUBSCRIPT(laogFXflt,lnPosStyle,1)
  lcStyFile   =  laogFXflt[lnPosStyle,6]
  IF !EMPTY(lcStyFile) AND USED(lcStyFile)
    SELECT (lcStyFile)
    LOCATE 
    IF !EOF()
      llSeleStyle = .T.
    ENDIF
  ENDIF 
ENDIF 

lnPosLoc = ASCAN(laogFXflt,'PIKTKT.CWARECODE')
lcLocFile = ''
llLocSelect = .F.
IF lnPosLoc <> 0 
  lnPosLoc = ASUBSCRIPT(laogFXflt,lnPosLoc,1)
  lcLocFile   =  laogFXflt[lnPosLoc,6]
  IF !EMPTY(lcLocFile) AND USED(lcLocFile)
    SELECT (lcLocFile)
    LOCATE 
    IF !EOF()
      llLocSelect = .T.
    ENDIF
  ENDIF 
ENDIF 

ldStart = DTOC({})
ldEnd = DTOC({})
lnPosDate = ASCAN(laogVRflt,'PIKTKT.DATE')
IF lnPosDate  <> 0 
  lnPosDate = ASUBSCRIPT(laogVRflt,lnPosDate ,1)
  ldStart = IIF(EMPTY(SUBSTR(laOGVRFlt[lnPosDate ,6],1,10)),DTOC(CTOD("")),SUBSTR(laOGVRFlt[lnPosDate ,6],1,10))
  ldEnd   = IIF(EMPTY(SUBSTR(laOGVRFlt[lnPosDate ,6],12,21)),DTOC(CTOD("")),SUBSTR(laOGVRFlt[lnPosDate ,6],12,21))
ENDIF   

*MMT
*lcRpStatus =''
*MMT

SELECT PIKTKT
=goAriaEnvironment.remotetableaccess.SeekRecord('')
SCAN FOR IIF(!EMPTY(lcRpStatus),Status $ lcRpStatus, .T.) AND;
		 IIF(llLocSelect,SEEK(PIKTKT.CWARECODE,lcLocFile),.T.) AND ;
		 IIF(!EMPTY(CTOD(ldStart)) AND !EMPTY(CTOD(ldEnd)), BETWEEN(PIKTKT.DATE,CTOD(ldStart),CTOD(ldEnd)),.T.)		 


   	
   lcOrder  = ORDER
   lcPikTkt = Piktkt
   SELECT Ordhdr 
   =goAriaEnvironment.remotetableaccess.SeekRecord('O'+lcOrder)		 
   SELECT ORDLINE
   =goAriaEnvironment.remotetableaccess.SeekRecord('O'+lcOrder)
   m.cWare = PIKTKT.CWARECODE
   SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrder ;
        FOR IIF(llSeleStyle,SEEK(SUBSTR(ORDLINE.STYLE,1,lnMajorLen ),lcStyFile),.T.) AND Piktkt==lcPikTkt 
     STORE 0 TO m.totStk,m.Sz1,m.Sz2,m.Sz3,m.Sz4,m.Sz5,m.Sz6,m.Sz7,m.Sz8,m.Stk1,;
     		    m.Stk2,m.Stk3,m.Stk5,m.Stk4,m.Stk6,m.Stk7,m.Stk8
     SCATTER MEMVAR MEMO
     m.Priority = ordhdr.Priority
     m.Status = ordhdr.Status
     m.STNAME = IIF(goAriaEnvironment.remotetableaccess.SeekRecord(IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE),'CUSTOMER'),SUBSTR(CUSTOMER.STNAME,1,30),'')
     =goAriaEnvironment.remotetableaccess.SeekRecord(ordline.Style,'Style_X','Style')
     =goAriaEnvironment.remotetableaccess.SeekRecord('S'+Style_X.Scale,'Scale')
     FOR lnI = 1 TO 8
       lcI =ALLTRIM(STR(lnI))
       m.Sz&lcI = scale.sz&lcI       
       m.STK&lcI= Style_X.STK&lcI
       m.totStk =  m.totStk + m.STK&lcI
     ENDFOR 
     INSERT INTO (lcOrdlTmp) FROM MEMVAR
   ENDSCAN 
ENDSCAN 


*SELECT (lcStyFile)

loProgress.Description = "Collecting Data..."
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
   loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  *T20100512.0026 Hassan 2010 05 23 [END]

SELECT (lcOrdlTmp)



IF RECCOUNT() > 0
*-- Scan (lcOrdlTmp) to get sum of Pik1,Pik2,...etc for each style  

SCAN

  


  *-- if found update the field with the new summation
  IF SEEK(style,lcStyTemp)
    
    REPLACE &lcStyTemp..ALO1   WITH &lcStyTemp..ALO1   + PIK1 ,;
            &lcStyTemp..ALO2   WITH &lcStyTemp..ALO2   + PIK2 ,;
            &lcStyTemp..ALO3   WITH &lcStyTemp..ALO3   + PIK3 ,;
            &lcStyTemp..ALO4   WITH &lcStyTemp..ALO4   + PIK4 ,;
            &lcStyTemp..ALO5   WITH &lcStyTemp..ALO5   + PIK5 ,;
            &lcStyTemp..ALO6   WITH &lcStyTemp..ALO6   + PIK6 ,;
            &lcStyTemp..ALO7   WITH &lcStyTemp..ALO7   + PIK7 ,;
            &lcStyTemp..ALO8   WITH &lcStyTemp..ALO8   + PIK8 ,;
            &lcStyTemp..TOTALO WITH &lcStyTemp..TOTALO + TOTPIK
  
  ELSE  && add new style with new picked quantity
    INSERT INTO (lcStyTemp)                                             ;
             (STYLE,Alo1,Alo2,Alo3,Alo4,Alo5,Alo6,Alo7,Alo8,TotAlo)     ;
      VALUES (&lcOrdlTmp..Style,&lcOrdlTmp..Pik1,&lcOrdlTmp..Pik2,&lcOrdlTmp..Pik3,&lcOrdlTmp..Pik4,;
                                &lcOrdlTmp..Pik5,&lcOrdlTmp..Pik6,&lcOrdlTmp..Pik7,&lcOrdlTmp..Pik8,;
                                &lcOrdlTmp..TotPik)
  ENDIF  

ENDSCAN  



 SELECT (lcOrdlTmp)
 lnDataCnt = RECCOUNT()
 LOCATE 
 SCAN 
   *MMT
    lnPerCent = RECNO()/lnDataCnt
    IF MOD(RECNO(),CEILING(lnDataCnt/ 10)) = 0
      loProgress.Percent = lnPerCent * 0.9
	  loProgress.Description = "Collecting Data..."
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
   loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  *T20100512.0026 Hassan 2010 05 23 [END]
    ENDIF
    *MMT
   IF SEEK(Style,lcStyTemp)
     =goAriaEnvironment.remotetableaccess.SeekRecord(Style,'Style_X','Style')
     FOR lnI = 1 TO 8
       lcI =ALLTRIM(STR(lnI))
       REPLACE BEGN&lcI WITH STK&lcI - (Style_X.ALO&lcI - &lcStyTemp..ALO&lcI )
       REPLACE ALO&lcI WITH Style_X.ALO&lcI 
     ENDFOR 
     REPLACE TOTBGN WITH Style_X.TOTSTK - (Style_X.TOTALO - &lcStyTemp..TOTALO)
   ENDIF
 ENDSCAN 
ENDIF  




FUNCTION lfRestFromXML
LPARAMETERS lcXML

LOCAL lobjDOMDocument
lobjDOMDocument = CREATEOBJECT("MSXML2.DOMDocument")
lobjDOMDocument.loadXML(lcXML)

LOCAL loRoot 
loRoot = lobjDOMDocument.childNodes(1).childNodes(0)


PUBLIC laOGHdFlt[1, 8]
PUBLIC laOGFxFlt[1, 8]
PUBLIC laOGVrFlt[1, 8]

LOCAL lnIndex
LOCAL loVariable, lcName, lcDataType, lcValue

FOR lnIndex = 0 TO loRoot.childNodes.Length - 1
  loVariable = loRoot.childNodes(lnIndex)
  lcDataType = loVariable.childNodes(0).text
  lcName     = loVariable.childNodes(1).text
  lcValue    = loVariable.childNodes(2).text

 DO CASE
  CASE UPPER(lcName) = UPPER('laOGHdFlt[')
    DIMENSION laOGHdFlt[VAL(SUBSTR(lcName, 11)), 8]
    &lcName. = lcValue
  
  CASE UPPER(lcName) = UPPER('laOGFxFlt[')
    DIMENSION laOGFxFlt[VAL(SUBSTR(lcName, 11)), 8]
    &lcName. = lcValue

  CASE UPPER(lcName) = UPPER('laOGVrFlt[')
    DIMENSION laOGVrFlt[VAL(SUBSTR(lcName, 11)), 8]
    &lcName. = lcValue
  
  CASE UPPER(lcDataType) = UPPER('System.String')
    PUBLIC &lcName.
    &lcName. = lcValue
      
  CASE UPPER(lcDataType) = UPPER('System.Boolean')
    PUBLIC  &lcName.
    IF !EMPTY(lcValue)
      &lcName. = IIF(lcValue = 'true', .T., .F.)
    ENDIF
      
  CASE UPPER(lcDataType) = UPPER('System.Decimal')
    PUBLIC  &lcName.
    &lcName. = VAL(lcValue)
      
  CASE UPPER(lcDataType) = UPPER('System.Datetime')
    PUBLIC  &lcName.
    IF !EMPTY(lcValue)
      &lcName. = DATE(VAL(LEFT(lcValue, 4)), VAL(SUBSTR(lcValue, 5, 2)), VAL(SUBSTR(lcValue, 7, 2)))
    ENDIF

  CASE UPPER(lcDataType) = UPPER('System.Table')
    IF !EMPTY(lcValue) .AND. !EMPTY(lcName)
      LOCAL lnSelected 
      lnSelected = SELECT()
    
      LOCAL loXMLParase
      loXMLParase = NEWOBJECT("wwxml")
      
      DIMENSION laStruct[loVariable.childNodes(3).childNodes.length,4]
      
      FOR nCount = 0 TO loVariable.childNodes(3).childNodes.length -1 
        laStruct[nCount+1,1] = loVariable.childNodes(3).childNodes(nCount).childNodes (0).text
        laStruct[nCount+1,2] = loVariable.childNodes(3).childNodes(nCount).childNodes (1).text
        laStruct[nCount+1,3] = loVariable.childNodes(3).childNodes(nCount).childNodes (2).text
        laStruct[nCount+1,4] = loVariable.childNodes(3).childNodes(nCount).childNodes (3).text
      ENDFOR  
      
      CREATE CURSOR (lcName) FROM ARRAY laStruct
      loXMLParase.XMLToCursor(lcValue, lcName)
       
      INDEX ON KEYEXP TAG KEYEXP
      
      SELECT(lnSelected)
      
    ENDIF
   ENDCASE
ENDFOR

cCROrientation = "P"
llCrystal      = .T.
lcoglastform   = "ALSTYAL"
lcActiveMod    = "AL"


goAriaEnvironment.gcOutFile = IIF(!EMPTY(cTextRepType),gcOutFile,STRTRAN(UPPER(gcOutFile),'.TXT','.PDF'))
goAriaEnvironment.cTextRepType = IIF(!EMPTY(cTextRepType),cTextRepType,'PDF')

* MAH
*IF UPPER(gcDevice) = 'SCREEN' OR UPPER(gcDevice) = 'PRINTER' 
*  goAriaEnvironment.cTextRepType = 'PDF'
*ENDIF
*goAriaEnvironment.gcOutFile = IIF(goAriaEnvironment.cTextRepType = 'PDF' and !EMPTY(goAriaEnvironment.gcOutFile),FORCEEXT(gcOutFile,'PDF'),goAriaEnvironment.gcOutFile)
* MAH
goAriaEnvironment.User_ID  = User_ID


goAriaEnvironment.report.cCROrientation = cCROrientation
goAriaEnvironment.report.llCrystal = llCrystal
goAriaEnvironment.report.OGLastForm =lcoglastform
goAriaEnvironment.ActiveModuleID = lcActiveMod
*goAriaEnvironment.gcDevice = gcDevice
