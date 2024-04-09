*:***************************************************************************
*: Program file  : sorevson.PRG
*: Program desc. : Custom Season to Season CustomerAnalysis 
*: Date          : 10/27/2008
*: System        : Aria Advantage Series.4XP
*: Module        : Sales order (SO)
*: Developer     : Mariam Mazhar[MMT]
*: Tracking Job Number: C201060[T20080422.0034]
*:***************************************************************************
* Modifications :
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[T20090727.0031]
*: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[T20090727.0031]
*:***************************************************************************
PARAMETERS lcRequestID, lcXMLFileName

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

PRIVATE loAgent
loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

PRIVATE loProgress
loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

loProgress.Percent = 0
loProgress.Description = "Opening Data Files..."
loAgent.UpdateObjectProgress(lcRequestID, loProgress)

LOCAL loEnvironment
loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")

LOCAL lcCurrentProcedure
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)

oAriaEnvironment.xml.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)

oAriaEnvironment.Report.gcAct_Appl = "SO"
oAriaEnvironment.report.cCROrientation = "P"
oariaenvironment.activeModuleID = 'SO'

PUBLIC gcAct_Appl 
gcAct_Appl = "SO"

IF LEFT(gcDevice, 7) = "PRINTER"
  oAriaEnvironment.gcDevice = "PRINTER"
ELSE
  oAriaEnvironment.gcDevice = "FILE"
ENDIF

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE 
  loogScroll.cCROrientation = 'P'
ENDIF
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

IF !USED('Codes_Seas')

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

 =oAriaEnvironment.remotetableaccess.OpenTable('Codes','cCode_No','SH','Codes_Seas')
 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE
  =gfOpenTABLE(oAriaApplication.DATADIR+'Codes',oAriaApplication.DATADIR+'cCode_No','SH','Codes_Seas')  
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End] 
 
ENDIF 
SELECT 'Codes_Seas'

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

IF !oAriaEnvironment.remotetableaccess.SeekRecord("NSEASON")
  RETURN  
ENDIF

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE 
  IF !gfSeek("NSEASON")
    RETURN  
  ENDI
ENDIF 

IF TYPE('lcXMLFileName') = 'C'
  DO lpCreatFil
  DO lpCollData            && Collect the data for report.
ELSE 
  IF llOGFltCh
    DO lpCreatFil
    DO lpCollData            && Collect the data for report.    
  ENDIF 
ENDIF 

IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]
 
oAriaEnvironment.report.OGLastForm = lcRpName
SELECT (lcWorkFile)
LOCATE
IF EOF()
  RETURN
ELSE
  loProgress.Percent = 0.9
  loProgress.Description = "Printing Report..."
  loAgent.UpdateObjectProgress(lcRequestID, loProgress)

  PRIVATE loProxy
  loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

  IF loProxy.GetRequest(lcRequestID).Status = 3
    oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)

    loProgress.Percent = 1.0
    loProgress.Description = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  ENDIF

ENDIF

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE 
  SELECT (lcWorkFile)
  LOCATE
  IF EOF()
    *-- Message <There are no records to display>
    *-- Buttons <               OK              >
    gfModalGen('TRM00052B00000','DIALOG' )  
    RETURN
  ELSE  
    =gfCrtFrm(lcRpName,'',llOGRefForm)
    =lfRepPltFr(lcRpName)  
    DO gfDispRe WITH EVALUATE('lcRpName')
  ENDIF 
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

IF llRpRecap
  SELECT (lcWorkFile)
  SET ORDER TO TAG (lcWorkFil)
  LOCATE
  
  *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[Start]
  IF TYPE('lcXMLFileName') = 'C'
    oAriaEnvironment.report.OGLastForm = lcRpRecap
  *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[End]  
  
    * lcOldFile = oAriaEnvironment.gcOutFile 
    * oAriaEnvironment.gcOutFile = oAriaEnvironment.gcOutFile+"_rec"
    oAriaEnvironment.report.lAdditive = .T.
    oAriaEnvironment.report.OGLastForm = lcRpRecap
    oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)
    oAriaEnvironment.report.lAdditive = .F.
    *oAriaEnvironment.gcOutFile =  lcOldFile 
    
  *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[Start]
  ELSE  
    =gfCrtFrm(lcRpRecap,'',llOGRefForm)
    =lfRepPltFr(lcRpRecap)      
  *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[End]
    loogscroll.lAdditive = .T.
    DO gfDispRe WITH EVALUATE('lcRpRecap')
    
  *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[Start]  
    lcRpName = 'SOREVSON'
  ENDIF 
  *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[Start]
ENDIF

*!*	*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
*!*	ELSE
*!*	  DO gfDispRe WITH EVALUATE('lcRpRecap')
*!*	ENDIF 
*!*	*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

*-- End of Report.

*!**************************************************************************
*! Name      : lpCreatFil
*: Developer : Mohamed Shokry (MHM)
*: Date      : 10/27/2001
*! Purpose   : Create work File.
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
*
PROCEDURE lpCreatFil
DIMENSION laFileStruc[12,4]

laFileStruc[1,1]= 'Account'
laFileStruc[1,2]= 'C'
laFileStruc[1,3]= 5
laFileStruc[1,4]= 0

laFileStruc[2,1]= 'cSeason'
laFileStruc[2,2]= 'C'
laFileStruc[2,3]= 6
laFileStruc[2,4]= 0

laFileStruc[3,1]= 'cSeasonDes'
laFileStruc[3,2]= 'C'
laFileStruc[3,3]= 30
laFileStruc[3,4]= 0

laFileStruc[4,1]= 'Acc_name'
laFileStruc[4,2]= 'C'
laFileStruc[4,3]= 30
laFileStruc[4,4]= 0

laFileStruc[5,1]= 'nShip'
laFileStruc[5,2]= 'N'
laFileStruc[5,3]= 9
laFileStruc[5,4]= 2

laFileStruc[6,1]= 'nShipUnit'
laFileStruc[6,2]= 'N'
laFileStruc[6,3]= 9
laFileStruc[6,4]= 0

laFileStruc[7,1]= 'nOpen'
laFileStruc[7,2]= 'N'
laFileStruc[7,3]= 14
laFileStruc[7,4]= 2

laFileStruc[8,1]= 'nOpenUnit'
laFileStruc[8,2]= 'N'
laFileStruc[8,3]= 14
laFileStruc[8,4]= 0

laFileStruc[9,1]= 'nBookUnit'
laFileStruc[9,2]= 'N'
laFileStruc[9,3]= 14
laFileStruc[9,4]= 0

laFileStruc[10,1]= 'nBook'
laFileStruc[10,2]= 'N'
laFileStruc[10,3]= 14
laFileStruc[10,4]= 2

laFileStruc[11,1]= 'nCancel'
laFileStruc[11,2]= 'N'
laFileStruc[11,3]= 9
laFileStruc[11,4]= 2

laFileStruc[12,1]= 'nCanclUnit'
laFileStruc[12,2]= 'N'
laFileStruc[12,3]= 7
laFileStruc[12,4]= 0

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

=oAriaEnvironment.Cursors.createcursor(lcWorkFile,@laFileStruc,"Account + cSeason",lcWorkFile,.f.)

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE 
  =gfCrtTmp(lcWorkFile,@laFileStruc,"Account + cSeason",lcWorkFile,.f.)
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

SELECT (lcWorkFile)
INDEX ON cSeason TAG (lcWorkFil) 
SET ORDER TO TAG (lcWorkFile)
*!**************************************************************************
*! Name      : lpCollData
*: Developer : Mohamed Shokry (MHM)
*: Date      : 10/25/2001
*! Purpose   : To collect data for the report.
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************
*
PROCEDURE lpCollData
PRIVATE lcOrder
lcOrder = ''     
lcRpExp = STRTRAN(lcRpExp,'ORDHDR.SEASON','SEASON')

llSelectAcc = .F.
lcAccFile = ''
lnAccPos = ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT')
IF lnAccPos > 0
  lnAccPos  = ASUBSCRIPT(laOGFxFlt,lnAccPos,1)
  IF !EMPTY(laOGFxFlt[lnAccPos,6]) AND USED(laOGFxFlt[lnAccPos ,6])
    SELECT (laOGFxFlt[lnAccPos,6])
    LOCATE 
    IF !EOF()
      llSelectAcc = .T.
      lcAccFile = laOGFxFlt[lnAccPos ,6]
    ENDIF 
  ENDIF 
ENDIF

*SEASON
llUseSeason  = .F.
lnSeaPos = ASCAN(laOGFxFlt,"ORDHDR.SEASON")
IF lnSeaPos > 0 
  lnSeaPos = ASUBSCRIPT(laOGFxFlt,lnSeaPos,1)
  lcSeaSel =IIF(!EMPTY(laOGFxFlt[lnSeaPos,6]),laOGFxFlt[lnSeaPos,6],'')
  IF !EMPTY(lcSeaSel) 
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]
  
    lcSeaFile = oAriaEnvironment.Cursors.GetCursorTempName()
    
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  ELSE
    lcSeaFile = gfTempName()
  ENDIF 
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

    llUseSeason = IIF(LEN(lcSeaSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeaSel,'SEASON',lcSeaFile)
  ENDIF   
ENDIF   


*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

=oAriaEnvironment.remotetableaccess.OpenTable('INVHDR','INVHDRA')
=oAriaEnvironment.remotetableaccess.OpenTable('ORDHDR','ORDACCT')
=oAriaEnvironment.remotetableaccess.OpenTable('OrdLine','OrdLine')
=oAriaEnvironment.remotetableaccess.OpenTable('InvLine','InvLine')
=oAriaEnvironment.remotetableaccess.OpenTable('CUSTOMER','CUSTOMER')

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE 
  =gfOpenTABLE(oAriaApplication.DATADIR+'INVHDR',oAriaApplication.DATADIR+'INVHDRA','SH')  
  =gfOpenTABLE(oAriaApplication.DATADIR+'ORDHDR',oAriaApplication.DATADIR+'ORDACCT','SH')  
  =gfOpenTABLE(oAriaApplication.DATADIR+'OrdLine',oAriaApplication.DATADIR+'OrdLine','SH')  
  =gfOpenTABLE(oAriaApplication.DATADIR+'InvLine',oAriaApplication.DATADIR+'InvLine','SH')  
  =gfOpenTABLE(oAriaApplication.DATADIR+'CUSTOMER',oAriaApplication.DATADIR+'CUSTOMER','SH') 
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

*-- If Temp. Account file is used.
IF llSelectAcc 

  SELECT ORDLINE 
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]
  
  =oAriaEnvironment.remotetableaccess.Setorderto('ORDLINE')
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  ELSE
    gfSetOrder('ORDLINE')
  ENDIF 
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  
  
  SELECT INVLINE
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  
  
  =oAriaEnvironment.remotetableaccess.Setorderto('INVLINE')
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  ELSE
    gfSetOrder('INVLINE')
  ENDIF
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  
  
  SELECT(lcAccFile)
  lnAccCnt = RECCOUNT()
  
  SCAN
  
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]   
    
    *MMT
    lnPerCent = RECNO()/lnAccCnt 
    loProgress.Percent = lnPerCent * 0.9
    loProgress.Description = "Collecting Data For Account:"+Account 
    loAgent.UpdateObjectProgress(lcRequestID, loProgress)
    *MMT

    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE
      WAIT WINDOW NOWAIT  "Collecting Data For Account:"+Account
    ENDIF
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]     
  
    *--for Open Qty (from ordhdr file)
    SELECT OrdHdr
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    
    
    =oAriaEnvironment.remotetableaccess.SeekRecord(&lcAccFile..Account)
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE
      gfSeek(&lcAccFile..Account)
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    
        
    SCAN REST WHILE Account+cOrdType+Order = &lcAccFile..Account &&FOR cOrdtype = 'O'
      IF  OrdHdr.Status <> "X" 
        SELECT OrdLine
        
      *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]            
        
        =oAriaEnvironment.remotetableaccess.SeekRecord(ORDHDR.Cordtype+ORDHDR.Order)
        
      *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
      ELSE
        gfSeek(ORDHDR.Cordtype+ORDHDR.Order)
      ENDIF 
      *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]                    
        
        SCAN REST WHILE cOrdType + Order = OrdHdr.cOrdType + OrdHdr.Order ;
                  FOR IIF(llUseSeason  ,SEEK(Ordline.Season,lcSeaFile),.T.)
          DO lpInsrtRec WITH 'O',Ordline.Account,Ordline.Season
        ENDSCAN
      ENDIF  
    ENDSCAN
      
    SELECT InvHdr

   *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
   IF TYPE('lcXMLFileName') = 'C'
   *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End] 

    =oAriaEnvironment.remotetableaccess.SeekRecord(&lcAccFile..Account)
    
   *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
   ELSE
     gfSeek(&lcAccFile..Account)
   ENDIF 
   *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]     
    
    SCAN REST WHILE Account+Invoice = &lcAccFile..Account
      IF  InvHdr.Status <> "V" 
        SELECT InvLine
        
       *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
       IF TYPE('lcXMLFileName') = 'C'
       *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]         
        
        =oAriaEnvironment.remotetableaccess.SeekRecord(InvHdr.Invoice)          
        
       *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
       ELSE
         gfSeek(InvHdr.Invoice)
       ENDIF 
       *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]                 
        
        SCAN REST WHILE Invoice = InvHdr.Invoice 
          IF IIF(llUseSeason  ,SEEK(Invline.Season,lcSeaFile),.T.)
            DO lpInsrtRec WITH 'I',InvLine.Account,InvLine.Season
          ENDIF  
        ENDSCAN
      ENDIF  
    ENDSCAN
  ENDSCAN  
  SELECT(laOGFxFlt[lnAcctPos,6])
ELSE           && Else user did not select Account.
  *-- For orders ( Open Qty )
  SELECT OrdLine 
  lnAccCnt = RECCOUNT()
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  IF TYPE('lcXMLFileName') = 'C'
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    
  
  =oAriaEnvironment.remotetableaccess.Setorderto('OrdLines')
  
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
  ELSE
    gfSetOrder('OrdLines')
  ENDIF 
  *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    
    
  SCAN FOR IIF(llUseSeason ,SEEK(Ordline.Season,lcSeaFile),.T.) AND cOrdType = 'O'
    
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]     
    
    *MMT
    IF MOD(RECNO(),CEILING(lnAccCnt/10)) = 0
      lnPerCent = RECNO()/lnAccCnt 
      loProgress.Percent = lnPerCent * 0.9
      loProgress.Description = "Collecting Data For Order:"+order
      loAgent.UpdateObjectProgress(lcRequestID, loProgress)
    ENDIF   
    *MMT
    
    =oAriaEnvironment.remotetableaccess.SeekRecord(Account+cordtype+Order,'ORDHDR','ORDACCT')
     
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE
      gfSeek(Account+cordtype+Order,'ORDHDR','ORDACCT')
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]     
          
    DO lpInsrtRec WITH 'O',Ordline.Account,Ordline.Season
  ENDSCAN  

  *-- For invoice (shiped Qty)
  SELECT InvLine 
  lnAccCnt = RECCOUNT()
  
 *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
 IF TYPE('lcXMLFileName') = 'C'
 *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    
  
  =oAriaEnvironment.remotetableaccess.Setorderto('InvLines')
  
 *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
 ELSE  
   gfSetOrder('InvLines')
 ENDIF 
 *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]      
  
  SCAN FOR IIF(llUseSeason  ,SEEK(InvLine.Season,lcSeaFile),.T.) 
  
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    
  
    *MMT
    IF MOD(RECNO(),CEILING(lnAccCnt/10)) = 0
      lnPerCent = RECNO()/lnAccCnt 
      loProgress.Percent = lnPerCent * 0.9
      loProgress.Description = "Collecting Data For Invoice:"+Invoice
      loAgent.UpdateObjectProgress(lcRequestID, loProgress)
    ENDIF   
    *MMT
    =oAriaEnvironment.remotetableaccess.SeekRecord(Invline.Account+Invline.Invoice,'INVHDR','INVHDRA')
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE
      gfSeek(Invline.Account+Invline.Invoice,'INVHDR','INVHDRA')
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]        
    
    IF  InvHdr.Status <> "V" 
      DO lpInsrtRec WITH 'I',InvLine.Account,InvLine.SEASON
    EndIF
  ENDSCAN  
ENDIF


*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 08/08/2007
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!B608212
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  
CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

ENDCASE 

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    

 = oAriaEnvironment.Cursors.createcursor(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE
  =gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]    
  
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.


*!**************************************************************************
*! Name      : lpInsrtRec
*: Developer : Mohamed Shokry (MHM)
*: Date      : 10/27/2001
*! Purpose   : To insert a new record or update an old one in temp file.
*!**************************************************************************
*! Example   : DO lpInsrtRec
*!**************************************************************************
*
PROCEDURE lpInsrtRec

PARAMETER lcFrmInOrd , lcAccount,lcSeason

PRIVATE llUpdate
STORE .F. TO llUpdate

*--Intialize variables for quantity
IF lcFrmInOrd <> 'O'
  STORE 0 TO m.nShip ,m.nShipUnit
ELSE  
  STORE 0 TO m.nOpen ,m.nBook , m.nCancel , m.nOpenUnit , m.nBookUnit , m.nCanclUnit 
ENDIF  

DO CASE
  CASE lcFrmInOrd   <> 'O'
      m.nShip      = INVLINE.TotQty * INVLINE.price
      m.nShipUnit  = INVLINE.TotQty 
      llUpdate      = .T.
      
  CASE lcFrmInOrd   = 'O'
      IF OrdHdr.Status $ 'OH'
        m.nOpen       = OrdLine.TotQty * OrdLine.price
        m.nOpenUnit   = OrdLine.TotQty 
      ENDIF  
      m.nBook       = OrdLine.TotBook * OrdLine.price
      m.nBookUnit   = OrdLine.TotBook 
      m.nCancel     = OrdHdr.cancelamt
      m.nCanclUnit = OrdHdr.cancel
      llUpdate      = .T.
ENDCASE

*--Update work temp file

IF SEEK(lcAccount+lcSeason,lcWorkFile) AND llUpdate 

  SELECT (lcWorkFile)
  IF lcFrmInOrd <> 'O'
    REPLACE nShip       WITH nShip+m.nShip,;
            nShipUnit   WITH nShipUnit +m.nShipUnit
  ELSE
    REPLACE nOpen     WITH nOpen+m.nOpen, ;
            nBook     WITH nBook+m.nBook, ;
            nOpenUnit WITH nOpenUnit+m.nOpenUnit,;
            nBookUnit WITH nBookUnit + m.nBookUnit
  ENDIF          
ELSE
  IF llUpdate 
    m.Account = lcAccount
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]     
    
    =oAriaEnvironment.remotetableaccess.SeekRecord("M"+lcAccount,"CUSTOMER")
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE
      gfSeek("M"+lcAccount,"CUSTOMER")
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]         
    
    m.Acc_name   = CUSTOMER.BtName
    m.cSeason    = Season
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]      
    
    m.cSeasonDes = oAriaEnvironment.codes.getcodedescription(Season,'SEASON')
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE
      m.cSeasonDes = gfCodDes(Season,'SEASON')
    ENDIF 
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]      
    
    INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDIF  
ENDIF
*-- End of lpInsrtRec.

