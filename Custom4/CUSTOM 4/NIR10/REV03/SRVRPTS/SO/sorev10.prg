*:**************************************************************************
*: Program file  : SOREV10
*: Program desc. : Custom Bookings Analysis Report for Revue.
*: System        : ARIA 4XP.
*: Module        : Sales Order (SO)
*: Developer     : Mostafa Eid (MOS)
*: Date          : 07/13/2008
*: Reference     : C201039
*:**************************************************************************
*: C201039,2 MMT 11/30/2008 Convert report tp work from request builder[T20080422.0025]
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data [T20090727.0031]
*: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[T20090727.0031]
*:**************************************************************************
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
oAriaEnvironment.report.cCROrientation = "L"
oariaenvironment.activeModuleID = 'SO'
oAriaEnvironment.report.cCRPapersize = 'A4'

PUBLIC gcAct_Appl 
gcAct_Appl = "SO"

IF LEFT(gcDevice, 7) = "PRINTER"
  oAriaEnvironment.gcDevice = "PRINTER"
ELSE
  oAriaEnvironment.gcDevice = "FILE"
ENDIF

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ENDIF
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

IF !USED('Codes')
 =gfopentable('Codes','CCODE_NO','SH')
ENDIF 

SELECT Codes
IF !gfseek('NCLASS','Codes','CCODE_NO')
  *-- Message <This company has no customer class codes, Cannot proceed.>
  *-- Buttons <                             OK                          >
  *=gfModalGen("TRM000000B00000","DIALOG",'','',"This company has no customer class codes, Cannot proceed.")
  RETURN  
ENDIF

*-- IF filter change collect data again.
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
  PRIVATE lcStyleExp , lcAcctExp , lcClassExp , lcDivsnExp , lcKeyValue
  lcKeyValue = ''
  STORE '.T.' TO lcStyleExp , lcAcctExp , lcClassExp , lcDivsnExp
  DO lpCreatFil
  DO lpCollData            && Collect the data for report.
ELSE 
  IF llOGFltCh
    PRIVATE lcStyleExp , lcAcctExp , lcClassExp , lcDivsnExp , lcKeyValue
    lcKeyValue = ''
    STORE '.T.' TO lcStyleExp , lcAcctExp , lcClassExp , lcDivsnExp
    DO lpCreatFil
    DO lpCollData            && Collect the data for report.
  ENDIF
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

oAriaEnvironment.report.OGLastForm = lcRpName
*-- Endif of Filter change.  
SELECT (lcTempFile)
LOCATE
IF EOF()
  *-- Message <There are no records to display>
  *-- Buttons <               OK              >
  *= gfModalGen('TRM00052B00000','DIALOG' )
  *SET DEVICE TO SCREEN
  RETURN
ELSE
  *DO gfDispRe WITH EVAL('lcRpName')
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
  SELECT (lcTempFile)
  LOCATE
  IF EOF()
    *-- Message <There are no records to display>
    *-- Buttons <               OK              >
    gfModalGen('TRM00052B00000','DIALOG' )
    *SET DEVICE TO SCREEN
    RETURN
  ELSE
    DO gfDispRe WITH EVALUATE('lcRpName')
  ENDIF
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]

*-- End of Report.

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Mostafa Eid (MOS)
*! Date      : 07/14/2008
*! Purpose   : To collect data for the report.
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************
PROCEDURE lpCollData
PRIVATE lcOrder
lcOrder = ''     
lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.

IF !USED('Ordhdr')
  =gfopentable('Ordhdr','ORDACCT','SH')
ENDIF 

IF !USED('OrdLine')
  =gfopentable('OrdLine','ORDLINES','SH')
ENDIF 

IF !USED('style')
  =gfopentable('style','style','SH')
ENDIF 

IF !USED('customer')
  =gfopentable('customer','customer','SH')
ENDIF 

IF !USED('INVLINE')
  =gfopentable('INVLINE','INVLINES','SH')
ENDIF 


IF !USED('INVHDR')
  =gfopentable('INVHDR','INVHDR','SH')
ENDIF 


*------ if select account
llSelectCusAcc = .F.
lnPosCusAcc = ASCAN(laOgFXFlt,"CUSTOMER.ACCOUNT")
IF lnPosCusAcc > 0 
   lnPosCusAcc = ASUBSCRIPT(laOgFxFlt,lnPosCusAcc,1)
   lcCusAccSel =IIF(!EMPTY(laOgFxFlt[lnPosCusAcc,6]),laOgFxFlt[lnPosCusAcc,6],'')
     IF !EMPTY(lcCusAccSel) AND USED(lcCusAccSel)
       SELECT(lcCusAccSel)
       LOCATE
         IF!EOF()
           llSelectCusAcc =.T.
         ENDIF 
     ENDIF  
ENDIF 

*----- if select style
llSelectStyMaj = .F.
lnPosStyMaj = ASCAN(laOgFXFlt,"STYLE.CSTYMAJOR")
IF lnPosStyMaj > 0 
   lnPosStyMaj = ASUBSCRIPT(laOgFxFlt,lnPosStyMaj,1)
   lcStyMajSel =IIF(!EMPTY(laOgFxFlt[lnPosStyMaj,6]),laOgFxFlt[lnPosStyMaj,6],'')
     IF !EMPTY(lcStyMajSel) AND USED(lcStyMajSel)
       SELECT(lcStyMajSel)
       LOCATE
         IF!EOF()
           llSelectStyMaj =.T.
         ENDIF 
     ENDIF  
ENDIF 

*------- if select division  
llUsediv  = .F.
lcdivFile  = ''
lndivPos = ASCAN(laOgFXFlt,"ORDHDR.CDIVISION")
IF lndivPos > 0 
   lndivPos = ASUBSCRIPT(laOgFXFlt,lndivPos,1)
   lcdivSel =IIF(!EMPTY(laOgFXFlt[lndivPos,6]),laOgFXFlt[lndivPos,6],'')
     IF !EMPTY(lcdivSel) 
       lcdivFile = gfTempName()
       llUsediv = IIF(LEN(lcdivSel)>0,.T.,.F.) AND lfConvertToCursor(lcdivSel,'CDIVISION',lcdivFile)
     ENDIF  
ENDIF

*------- if select season   
llUsesea  = .F.
lcseaFile  = ''
lnseaPos = ASCAN(laOgFXFlt,"ORDLINE.SEASON")
IF lnseaPos > 0 
   lnseaPos = ASUBSCRIPT(laOgFXFlt,lnseaPos,1)
   lcseaSel =IIF(!EMPTY(laOgFXFlt[lnseaPos,6]),laOgFXFlt[lnseaPos,6],'')
     IF !EMPTY(lcseaSel) 
      lcseaFile = gfTempName()
      llUsesea = IIF(LEN(lcseaSel)>0,.T.,.F.) AND lfConvertToCursor(lcseaSel,'SEASON',lcseaFile)
     ENDIF
ENDIF 

*------- if select STYLE group   
llUsegroup  = .F.
lcgroupFile  = ''
lngroupPos = ASCAN(laOgFXFlt,"STYLE.CSTYGROUP")
IF lngroupPos > 0 
   lngroupPos = ASUBSCRIPT(laOgFXFlt,lngroupPos,1)
   lcgroupSel =IIF(!EMPTY(laOgFXFlt[lngroupPos,6]),laOgFXFlt[lngroupPos,6],'')
     IF !EMPTY(lcgroupSel) 
       lcgroupFile = gfTempName()
       llUsegroup  = IIF(LEN(lcgroupSel)>0,.T.,.F.) AND lfConvertToCursor(lcgroupSel,'CSTYGROUP',lcgroupFile)
     ENDIF  
ENDIF

*------- if select class   
llUseclass  = .F.
lcclassFile  = ''
lnclassPos = ASCAN(laOgFXFlt,"CUSTOMER.CLASS")
IF lnclassPos > 0 
   lnclassPos = ASUBSCRIPT(laOgFXFlt,lnclassPos,1)
   lcclassSel =IIF(!EMPTY(laOgFXFlt[lnclassPos,6]),laOgFXFlt[lnclassPos,6],'')
     IF !EMPTY(lcclassSel) 
       lcclassFile = gfTempName()
       llUseclass = IIF(LEN(lcclassSel)>0,.T.,.F.) AND lfConvertToCursor(lcclassSel,'CLASS',lcclassFile)
     ELSE 
       SELECT Codes
       
       *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
       IF TYPE('lcXMLFileName') = 'C'
       *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]       
       
       =oAriaEnvironment.remotetableaccess.Setorderto('cCode_No')
       
       *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
       ELSE 
         gfSetOrder('cCode_No')
       ENDIF 
       *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]              
       
       SELECT cDiscRep , cCode_No ;
       FROM   Codes ;
       WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = "NCLASS" ;
       INTO ARRAY  laArray
       =ASORT(laArray)
       PRIVATE lnBound , lnCount
       lnBound = IIF(ALEN(laArray,1)>2,3,IIF(ALEN(laArray,1)>1,2,IIF(ALEN(laArray,1)=1,1,0)))
       lcclassSel = ""
         FOR lnCount = 1 TO lnBound
           lcclassSel = lcclassSel+ laArray[lnCount,2] + SPACE(1)      
         ENDFOR
       lcclassSel = STRTRAN(ALLTRIM(lcclassSel),' ',"|")
       lcclassFile = gfTempName()
       llUseclass = IIF(LEN(lcclassSel)>0,.T.,.F.) AND lfConvertToCursor(lcclassSel,'CLASS',lcclassFile)
     ENDIF

    SELECT Codes
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    IF TYPE('lcXMLFileName') = 'C'
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]
    
    =oAriaEnvironment.remotetableaccess.Setorderto('cCode_No')
    
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
    ELSE
      gfSetOrder('cCode_No')
    ENDIF
    *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]
    
    lcClass1 = IIF(SEEK('N'+PADR('CLASS',10)+SUBSTR(lcclassSel,1,6),'Codes'),ALLTRIM(cDiscRep),'')
      IF !EMPTY(SUBSTR(lcclassSel,8,6))
        lcClass2 = IIF(SEEK('N'+PADR('CLASS',10)+SUBSTR(lcclassSel,8,6),'Codes'),ALLTRIM(cDiscRep),'')
      ENDIF
      IF !EMPTY(SUBSTR(lcclassSel,15,6))
        lcClass3 = IIF(SEEK('N'+PADR('CLASS',10)+SUBSTR(lcclassSel,15,6),'Codes'),ALLTRIM(cDiscRep),'')
      ENDIF
ENDIF

*-- if select ACCOUNT
DO CASE
 CASE  llSelectCusAcc
 SELECT Ordline
 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  
 
 =oAriaEnvironment.remotetableaccess.Setorderto('ORDLINE')
 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE 
  gfSetOrder('ORDLINE')
ENDIF
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]   
 
 SELECT (lcCusAccSel)
 lnAccCnt = RECCOUNT()
 SCAN 
 
   *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
   IF TYPE('lcXMLFileName') = 'C'
   *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  
 
   lnPerCent = RECNO()/lnAccCnt 
   loProgress.Percent = lnPerCent * 0.9
   loProgress.Description = "Collecting Data For Account:"+&lcCusAccSel..account
   loAgent.UpdateObjectProgress(lcRequestID, loProgress)
   
   *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
   ELSE 
     WAIT WINDOW NOWAIT  "Collecting Data For Account:"+&lcCusAccSel..account
   ENDIF
   *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]     

   SELECT ordhdr
     IF gfSEEK(&lcCusAccSel..account) 
	   SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = &lcCusAccSel..account 
	     SELECT ORDLINE
         *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
         IF TYPE('lcXMLFileName') = 'C'
         *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  
 
         =oAriaEnvironment.remotetableaccess.Setorderto('ORDLINES')
 
         *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[Start]
         ELSE 
           gfSetOrder('ORDLINES')
         ENDIF
         *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[End]   
         
	     IF gfseek(ORDHDR.CORDTYPE + ORDHDR.order,'ORDLINE','ORDLINE')	      
	          gfseek(ORDLINE.STYLE,'STYLE','STYLE')
	          SCAN REST WHILE CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) = ORDHDR.CORDTYPE + ORDHDR.order FOR ;
			   IIF(llUsesea,SEEK(ORDLINE.SEASON,lcseaFile),.T.)AND ;
	           gfseek('M'+ ORDLINE.ACCOUNT,'Customer','CUSTOMER')AND;
			   IIF(llUseclass,SEEK(CUSTOMER.CLASS,lcclassFile),.T.)AND ;
			   IIF(llUsegroup,SEEK(STYLE.CSTYGROUP,lcgroupFile),.T.)and ;
			   IIF(llSelectStyMaj,SEEK(STYLE.CSTYMAJOR,lcStyMajSel),.T.)     
			     IF OrdHdr.Status $ 'COH' AND IIF(llUsediv,SEEK(ORDHDR.CDIVISION,lcdivFile),.T.)
   			         gfseek(ORDLINE.STYLE,'STYLE','STYLE') 

	                 DO lpInsrtRec
	             ENDIF   
	          ENDSCAN  
	     ENDIF  
	   ENDSCAN    
	 ENDIF    
 ENDSCAN 

*-- if select style 
 CASE  llSelectStyMaj  
  SELECT (lcStyMajSel)
  lnAccCnt = RECCOUNT()
    SCAN
 
      *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[End]  
       
	  lnPerCent = RECNO()/lnAccCnt 
	  IF MOD(RECNO(),CEILING(lnAccCnt/10)) = 0
	    loProgress.Percent = lnPerCent * 0.9
  	    loProgress.Description = "Collecting Data For Style:"+&lcStyMajSel..cstymajor
  	    loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  	  ENDIF  
  	  
      *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[Start]
      ENDIF
      *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[End]    	   
    
      SELECT ordline   
      
      *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
      IF TYPE('lcXMLFileName') = 'C'
      *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]       
        
	  =oAriaEnvironment.remotetableaccess.Setorderto('ORDLINES')
	  
      *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
      ELSE 
        *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[Start]
*!*	        gfSetOrder('ORDLINE')
        gfSetOrder('ORDLINES')
        *: C201196,2 HES 05/10/2009 Fix the problem of collecting data without conditions and some bugs[Start]
      ENDIF 
      *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]       	  
	  
	  IF gfSEEK(SUBSTR(&lcStyMajSel..cstymajor,1,lnMajorLen)) 
	     SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6)= SUBSTR(&lcStyMajSel..cstymajor,1,lnMajorLen);
	      FOR gfseek(ORDLINE.STYLE,'STYLE','STYLE') AND gfseek('M'+ ORDLINE.ACCOUNT,'Customer','CUSTOMER')AND ;
	      gfseek(cOrdType + ordline.Order,'ORDHDR','ORDHDR') AND ;
	      IIF(llSelectCusAcc,SEEK(ordline.account,lcCusAccSel),.T.) AND ;
	      IIF(llUsediv,SEEK(ORDHDR.CDIVISION,lcdivFile),.T.)AND ;
	      IIF(llUsesea,SEEK(ORDLINE.SEASON,lcseaFile),.T.)AND ;
	      IIF(llUseclass,SEEK(CUSTOMER.CLASS,lcclassFile),.T.)AND ;
	      IIF(llUsegroup,SEEK(STYLE.CSTYGROUP,lcgroupFile),.T.)
	        IF OrdHdr.Status $ 'COH'    

	          DO lpInsrtRec
	        ENDIF                  
	    ENDSCAN  
	  ENDIF    
    ENDSCAN  
 
 OTHERWISE 
 SELECT ORDLINE
 lnAccCnt = RECCOUNT()
 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]       
 
 =oAriaEnvironment.remotetableaccess.Setorderto('ORDLINE')
 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE 
  gfSetOrder('ORDLINE')
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]        
 
      SCAN FOR ;
 	     gfseek(cOrdType + ordline.Order,'ORDHDR','ORDHDR')AND;
 	     gfseek('M'+ ORDLINE.ACCOUNT,'CUSTOMER','CUSTOMER')AND ; 
	     IIF(llSelectCusAcc,SEEK(ordline.account,lcCusAccSel),.T.) AND ;
	     IIF(llUsediv,SEEK(ORDHDR.CDIVISION,lcdivFile),.T.)AND ;
	     IIF(llUsesea,SEEK(ORDLINE.SEASON,lcseaFile),.T.)AND ;
	     IIF(llUseclass,SEEK(CUSTOMER.CLASS,lcclassFile),.T.)AND ;
	     IIF(llSelectStyMaj,SEEK(STYLE.CSTYMAJOR,lcStyMajSel),.T.)
	     
          *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
          IF TYPE('lcXMLFileName') = 'C'
          *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End] 	     
	     
		  lnPerCent = RECNO()/lnAccCnt 
		  IF MOD(RECNO() ,CEILING(lnAccCnt/10)) = 0
		    loProgress.Percent = lnPerCent * 0.9
  	    	loProgress.Description = "Collecting Data For Order:"+OrdHdr.Order
	  	    loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  		  ENDIF   

          *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
          ELSE
            WAIT WINDOW NOWAIT "Collecting Data For Order:"+OrdHdr.Order
          ENDIF 
          *: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End] 
	     
	     =gfseek(ORDLINE.STYLE,'STYLE','STYLE')
	     IF llUsegroup AND !SEEK(STYLE.CSTYGROUP,lcgroupFile)
	       LOOP  
	     ENDIF    
	     IF OrdHdr.Status $ 'COH' 
		     DO lpInsrtRec
	      ENDIF     
	 ENDSCAN   
ENDCASE 

SELECT (lcTempFile)
LOCATE
IF EOF()
  RETURN
ELSE
  DO lpContColl
ENDIF

*-- End of lpCollData.
*!**************************************************************************
*! Name      : lpContColl
*! Developer : Mostafa eid (mos)
*! Date      : 07/25/2008
*! Purpose   : To continue collecting data for the report.
*!**************************************************************************
*! Example   : DO lpContColl
*!**************************************************************************
PROCEDURE lpContColl
=gfOpenTable(oAriaApplication.DataDir+'POSLN','POSLNS','SH')
=gfOpenTable(oAriaApplication.DataDir+'POSHDR','POSHDR','SH')
PRIVATE lcDivision 
lcDivision = ".T."
SELECT (lcTempFile)
SCAN
gfSEEK('0001'+&lcTempFile..STYLE+'P','POSLN','POSLNS')
*-- Update the Unit cut with the pieces recieved from purchased order lines.
  lcDivision = STRTRAN(lcDivsnExp,"ORDHDR.CDIVISION","POSHDR.CDIVISION")
  m.cSeason = cSeason
  m.cStyGroup = cStyGroup
  m.Style = Style
  m.nUnitCut = 0
  IF SEEK('0001'+&lcTempFile..STYLE+'P'+'P','POSLN','POSLNS')
     SELECT POSLN
       SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD= '0001'+&lcTempFile..Style+'P'+'P' 
	     IF !( TranCD = "2" AND gfSEEK('PP'+POSLN.PO,'POSHDR','POSHDR') AND POSHDR.Status <> "X" )
	          LOOP 
	     ENDIF 
	     m.nUnitCut = m.nUnitCut + TotQty
         ENDSCAN
  ENDIF  
  SELECT (lcTempFile)
  REPLACE nUnitCut WITH m.nUnitCut && IN (lcTempFile)

  *-- Update the Unit cut with the pieces recieved from Cut ticket lines.
  SELECT (lcTempFile) 
  IF gfSEEK('0001'+&lcTempFile..STYLE+'P'+'U','POSLN','POSLNS')
      m.cSeason = cSeason
      m.cStyGroup = cStyGroup
      m.Style = Style
      m.nUnitCut = 0
     SELECT POSLN
      SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD= '0001'+&lcTempFile..Style+'P'+'U' 
         IF !(TranCD = "2" AND gfSeek('PU'+ POSLN.PO,'POSHDR','POSHDR') AND PosHdr.Status <> "X" )
           LOOP 
         ENDIF   
        m.nUnitCut = m.nUnitCut + TotQty
      ENDSCAN
   ENDIF  
      SELECT (lcTempFile)
      REPLACE nUnitCut WITH m.nUnitCut

  *-- Update the total shipped amount from invoice line file. 
  IF gfSEEK(&lcTempFile..STYLE,'INVLINE','INVLINES')
    m.cSeason = cSeason
    m.cStyGroup = cStyGroup
    m.Style = Style
    PRIVATE lnAmount,lnQuantity
    STORE 0 TO  m.nSellPrice , lnAmount , lnQuantity
    SELECT InvLine
    SCAN REST WHILE Style + Invoice + STR(LineNo,6) = &lcTempFile..Style ; 
      FOR gfSEEK(INVLINE.Invoice ,'INVHDR','INVHDR')AND InvHdr.Status <> "X"  AND IIF(llUsediv,SEEK(InvHDR.CDIVISION,lcdivFile),.T.)
        IF gfSEEK('M'+Account,'Customer') 
          lnAmount = lnAmount + (TotQty * Price)
          lnQuantity = lnQuantity + TotQty     
        ENDIF 
    ENDSCAN
  ENDIF
ENDSCAN

*-- End of lpContColl.
*!**************************************************************************
*! Name      : lpInsrtRec
*! Developer : mostafa eid (mos)
*! Date      : 07/26/2001
*! Purpose   : To insert a new record or update an old one in temp file.
*!**************************************************************************
*! Example   : DO lpInsrtRec
*!**************************************************************************
PROCEDURE lpInsrtRec
m.cSeason = OrdLine.Season
m.cStyGroup = Style.cStyGroup
m.Style = OrdLine.Style
m.cDesc = Style.Desc
m.ntotBook = OrdLine.TotBook
m.nTotalQty = IIF(OrdHdr.Status $ 'OH',OrdLine.TotQty,0)
m.nBookAmt  = OrdLine.TotBook * OrdLine.Price
*B605961,1 ALB Fix the thru % and MU% [Begoin]
m.nSellPrice = STYLE.pricea
*B605961,1 ALB Fix the thru % and MU% [End]
STORE 0 TO m.nClass1 , m.nClass2 , m.nClass3
lcClassExp =lcclassSel  
DO CASE
  CASE AT(Customer.Class,lcClassExp) = 1
    m.nClass1 = OrdLine.TotBook
  CASE AT(Customer.Class,lcClassExp) = 8
    m.nClass2 = OrdLine.TotBook
  CASE AT(Customer.Class,lcClassExp) = 15
    m.nClass3 = OrdLine.TotBook
ENDCASE
  SELECT (lcTempFile)
  IF SEEK(EVALUATE(lcKeyValue),lcTempFile)
  REPLACE ntotBook  WITH ntotBook + m.ntotBook , ;
          nTotalQty WITH nTotalQty + m.nTotalQty , ;
          nBookAmt  WITH nBookAmt + m.nBookAmt , ;
          nClass1   WITH nClass1 + m.nClass1 , ;
          nClass2   WITH nClass2 + m.nClass2 , ;
          nClass3   WITH nClass3 + m.nClass3 
  ELSE
      INSERT INTO (lcTempFile) FROM MEMVAR
  ENDIF
*!*	*-- End of lpInsrtRec.

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Report When Function.
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
FUNCTION lfwRepWhen
lnAcctPos  = lfItmPos('CUSTOMER.ACCOUNT') && get Account Fixed filter Position.
lnStylePos = lfItmPos('STYLE.CSTYMAJOR')  && get Style Group Fixed filter Position.
lnClassPos = lfItmPos('CUSTOMER.CLASS')   && get Class filter Position.
lnDivsnPos = lfItmPos('ORDHDR.CDIVISION') && get Season filter Position.
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : To get the position of the fixed filter in OG.
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lfsrAcc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Rise change account flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrAcc()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
ENDCASE
*-- End of lfsrAcc.

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Mostafa Eid (MOS)
*! Date      : 07/13/2008
*! Purpose   : Create TEMP File .
*!**************************************************************************
*! Example   : DO lpCreatFil.
*!**************************************************************************
PROCEDURE lpCreatFil
IF USED(lcTempFile) 
 USE IN (lcTempFile)
ENDIF

*-- Create File
IF !USED(lcTempFile)
  lnI = 1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'cSeason'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'cStyGroup'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0
    
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Style'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 19
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'cDesc'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 20
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'nTotBook'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 13
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'nBookAmt'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 15
  laTempStru[lnI,4] = 2
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'nUnitCut'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 13
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'nTotalQty'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 13
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'nUnitCost'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 15
  laTempStru[lnI,4] = 2

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'nSellPrice'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 15
  laTempStru[lnI,4] = 2

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'nClass1'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 13
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'nClass2'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 13
  laTempStru[lnI,4] = 0

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'nClass3'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 13
  laTempStru[lnI,4] = 0

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  

oAriaEnvironment.Cursors.createcursor(lcTempFile,@laTempStru,'STYLE',lcTempFile,.T.)

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
ELSE 
  =gfCrtTmp(lcTempFile,@laTempStru,'STYLE',lcTempFile,.T.)
ENDIF 
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End]  

SELECT (lcTempFile) 
 IF lcRpSortBy = "S"   && sort by style group.
   lcKeyValue = "m.cStyGroup + m.cSeason + m.Style"
   INDEX ON cStyGroup + cSeason + Style  TAG (lcTempFile)  
 ELSE                  && sort by season.
   lcKeyValue = "m.cSeason + m.cStyGroup + m.Style"
   INDEX ON cSeason + cStyGroup + Style  TAG (lcTempFile)
 ENDIF  
ENDIF

*-- End of lpCreatFil.
*!**************************************************************************
*! Name      : lfEvalSegs
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Evaluate NonMajor Type and variables.
*!**************************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!**************************************************************************
*! Example     : = lfEvalSegs()
*!**************************************************************************
*
FUNCTION lfEvalSegs

*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.

*-- Compute Free/Color Items in Style Structure. [Begin]
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

*-- Loop Around Non Major elements.
FOR lnI = 1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnI,1] = "C"            && Color
    lnClrPo    = laMajSeg[lnI,4]
    lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
    lnColorLen = LEN(laMajSeg[lnI,3])
  ENDIF
ENDFOR    
RETURN ''
*-- End of lfEvalSegs.

*!**************************************************************************
*! Name      : lfsrAcc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Rise change account flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrAcc()
*!**************************************************************************
*! Note      : S symbol is [S,Set]
*!**************************************************************************
*
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
ENDCASE
*-- End of lfsrAcc.

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Example   : =lfsrvSty()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT Style
    SET ORDER TO TAG Cstyle
    SET RELATION TO Style.Style INTO STYLE_X
    GO TOP IN Style
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT Style
    SET ORDER TO TAG Style
ENDCASE
*-- End of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/25/2001
*! Purpose   : Sum a specific field for the current style in style file
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Returns   : Calculated field value.
*!**************************************************************************
*! Example   : =lfStySum()
*!**************************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)

*-- End of lfStySum.
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : Mostafa Eid(MOS)
*: Date      : 07/02/2008
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 

CASE  ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE  ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE  ALLTRIM(lcFieldName) = 'CLASS'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE  ALLTRIM(lcFieldName) = 'CSTYGROUP'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
ENDCASE 

*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[Start]
IF TYPE('lcXMLFileName') = 'C'
*: C201196,1 HES 09/09/2009 call request builder fxp to collect data[End] 

oAriaEnvironment.Cursors.createcursor(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)

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
    SELECT(lcCursorTemp) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.

*!*************************************************************
*! Name      : lfCodDes1
*: Developer : Hesham Elmasry(HES)
*: Date      : 09/10/2009
*! Purpose   : Handle Code Description field1 for each case 
*!*************************************************************
FUNCTION lfCodDes1

RETURN IIF(lcRpSortBy="S", ;
             IIF(TYPE('lcXMLFileName') = 'C', ;
               oAriaEnvironment.codes.getcodedescription(cStyGroup,'CSTYGROUP'), ;
               gfCodDes(cStyGroup,'CSTYGROUP')), ;
             IIF(TYPE('lcXMLFileName') = 'C', ;
               oAriaEnvironment.codes.getcodedescription(cSeason,'SEASON'), ;
               gfCodDes(cSeason,'SEASON')))
        
*End of lfCodDes()        

*!*************************************************************
*! Name      : lfCodDes2
*: Developer : Hesham Elmasry(HES)
*: Date      : 09/10/2009
*! Purpose   : Handle Code Description field2 for each case 
*!*************************************************************
FUNCTION lfCodDes2

RETURN IIF(lcRpSortBy="S", ;
             IIF(TYPE('lcXMLFileName') = 'C', ;
               oAriaEnvironment.codes.getcodedescription(cSeason,'SEASON'), ;
               gfCodDes(cSeason,'SEASON')), ;
             IIF(TYPE('lcXMLFileName') = 'C', ;
               oAriaEnvironment.codes.getcodedescription(cStyGroup,'CSTYGROUP'), ;
               gfCodDes(cStyGroup,'CSTYGROUP')))  

* End lfCodDes2()