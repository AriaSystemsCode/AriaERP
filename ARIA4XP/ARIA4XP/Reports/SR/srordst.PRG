*:************************************************************************
*: Program file  : SRORDST.Prg
*: Program desc. : SALESREP STATISTICS
*: System        : Aria Advantage Series VER. 4XP
*: Module        : SR
*: Developer     : Mariam Mazhar (MMT)
*: Date          : 07/27/2006
*: Reference     : *N037677
*:************************************************************************
*Modification:
*B607900,1 12/24/2006 MMT Bug while previw report and company not multi Curreny,T20061220.0021
*:************************************************************************

llRep_div = ALLTRIM(gfGetMemVar('M_REP_COMM' , oAriaApplication.ActiveCompanyID)) = 'D'
IF llRep_div AND TYPE('lodbfREP_DIV') <> 'O'
  lodbfREP_DIV = CreateObject("RemoteTable","REP_DIV","REP_DIV",'REP_DIV',SET("DATASESSION"))&&,"",.T.)
ENDIF

IF llMultCurr
  PRIVATE lcCurr
  lcCurr = ""
  lcTmpCur = LOOGSCROLL.gfTempName()
  =lfCretTemp()
ENDIF
loOgScroll.lcOGLastForm = 'SRORDST'
loOgScroll.cCRorientation = 'P'
IF loOgScroll.llOGFltCh && OG Filters changed
  WAIT WINDOW "Collecting Data......." NOWAIT 
  =lfCrtTemp()
  =lfColData()
ELSE
  IF FILE(oAriaApplication.WorkDir +  lcRepFile + ".DBF") AND !USED(lcRepFile)
    USE oAriaApplication.WorkDir +  lcRepFile + ".DBF" IN 0 
  ENDIF 
ENDIF
SELECT(lcRepFile)
LOCATE  
IF EOF()
  *-- Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

DIMENSION loOgScroll.laCRParams[3,2]
loOgScroll.laCRParams[1,1] = 'ReportName'
loOgScroll.laCRParams[1,2] = 'Salesrep Statistics'

loOgScroll.laCRParams[2,1] = 'llMultCurr'
loOgScroll.laCRParams[2,2] = IIF(llMultCurr,1,0)

loOgScroll.laCRParams[3,1] = 'GCOUNT'
loOgScroll.laCRParams[3,2] = GCOUNT



loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcRepFile + ".DBF"
SELECT(lcRepFile)
USE 

gfDispRe()
RETURN 


*:***************************************************************************
*: Name        : lfCretTemp
*: Developer   : Mariam Mazhar (MMT)
*: Date        : 07/27/2006
*: Purpose     : Create Temp file
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfCretTemp()
*:***************************************************************************
*
FUNCTION lfCretTemp
DIMENSION laFStru[1,4]
laFStru[1,1] = 'CurrCode'
laFStru[1,2] = 'C'
laFStru[1,3] = 3
laFStru[1,4] = 0
 = gfCrtTmp(lcTmpCur,@laFStru,"CurrCode" ,lcTmpCur,.T.)

*:***************************************************************************
*: Name        : lfwRepWhen
*: Developer   : Mariam Mazhar (MMT)
*: Date        : 07/27/2006
*: Purpose     : report when function
*:***************************************************************************
FUNCTION lfwRepWhen
lodbfORDHDR = CreateObject("RemoteTable","ORDHDR","ORDHDR",'ORDHDR',SET("DATASESSION"))&&,"",.T.)
lodbfSalesRep = CreateObject("RemoteTable","SalesRep","SalesRep",'SalesRep',SET("DATASESSION"))&&,"",.T.)

*:***************************************************************************
*: Name        : lfCretTemp
*: Developer   : Mariam Mazhar (MMT)
*: Date        : 07/27/2006
*: Purpose     : Create Temp file
*:***************************************************************************
FUNCTION lfCrtTemp
SELECT ORDHDR
DIMENSION laTempStru[1,18]
laTempStru = ''
lnAllFlds = AFIELDS(laTempStru)

 = gfCrtTmp(lcRepTmp,@laTempStru,IIF(llMultCurr AND lcRpCurr == "F","cCurrCode+REP1","REP1"),lcRepTmp,.T.)
 
DIMENSION laTStru[14,4]

laTStru[1,1] = 'XSALESREP'
laTStru[1,2] = 'C'
laTStru[1,3] = 3
laTStru[1,4] = 0

laTStru[2,1] = 'XSALNAME'
laTStru[2,2] = 'C'
laTStru[2,3] = 24
laTStru[2,4] = 0

laTStru[3,1] = 'BACT'
laTStru[3,2] = 'N'
laTStru[3,3] = 8
laTStru[3,4] = 0

laTStru[4,1] = 'BACTAMT'
laTStru[4,2] = 'N'
laTStru[4,3] = 10
laTStru[4,4] = 0

laTStru[5,1] = 'BAVG'
laTStru[5,2] = 'N'
laTStru[5,3] = 8
laTStru[5,4] = 0

laTStru[6,1] = 'BAVGAMT'
laTStru[6,2] = 'N'
laTStru[6,3] = 10
laTStru[6,4] = 0

laTStru[7,1] = 'BPCNT'
laTStru[7,2] = 'N'
laTStru[7,3] = 8
laTStru[7,4] = 1

laTStru[8,1] = 'CACT'
laTStru[8,2] = 'N'
laTStru[8,3] = 8
laTStru[8,4] = 0

laTStru[9,1] = 'CACTAMT'
laTStru[9,2] = 'N'
laTStru[9,3] = 10
laTStru[9,4] = 0

laTStru[10,1] = 'CAVG'
laTStru[10,2] = 'N'
laTStru[10,3] = 8
laTStru[10,4] = 0

laTStru[11,1] = 'CAVGAMT'
laTStru[11,2] = 'N'
laTStru[11,3] = 10
laTStru[11,4] = 0

laTStru[12,1] = 'CPCNT'
laTStru[12,2] = 'N'
laTStru[12,3] = 8
laTStru[12,4] = 1

laTStru[13,1] = 'Currcode'
laTStru[13,2] = 'C'
laTStru[13,3] = 3
laTStru[13,4] = 0

laTStru[14,1] = 'XCOUNT'
laTStru[14,2] = 'N'
laTStru[14,3] = 4
laTStru[14,4] = 0

= gfCrtTmp(lcRepFile,@laTStru,"XSALESREP",lcRepFile,.F.)
 


 *
 *!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 06/22/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
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

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
  
CASE   ALLTRIM(lcFieldName) = 'CCURRCODE'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CSTYCLR'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
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

 *:***************************************************************************
*: Name        : lfColData
*: Developer   : Mariam Mazhar (MMT)
*: Date        : 07/27/2006
*: Purpose     : Collect data 
*:***************************************************************************
FUNCTION lfColData

llUseDiv   = .F.
llUseSea   = .F.
llUseCurr   = .F.

LowDate  = {}
HighDate = {}
*--Division filter
lnDivPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'ORDHDR.CDIVISION'),1)
IF lnDivPos  > 0
  lcDivStr = LOOGSCROLL.laOGVRFlt[lnDivPos,6]
  lcDivFile = loOGScroll.gfTempName()
  llUseDiv = IIF(LEN(lcDivStr)>0,.T.,.F.) AND lfConvertToCursor(lcDivStr,'CDIVISION',lcDivFile)
ENDIF

*---Season Filter
lnSeaPos = ASUBSCRIPT(LOOGSCROLL.laOGVRFlt,ASCAN(loOGScroll.laOGVRFlt,'ORDHDR.SEASON'),1)
IF lnSeaPos > 0
  lcSeaStr = LOOGSCROLL.laOGVRFlt[lnSeaPos,6]
  lcSeaFile = loOGScroll.gfTempName()
  llUseSea = IIF(LEN(lcSeaStr)>0,.T.,.F.) AND lfConvertToCursor(lcSeaStr,'SEASON',lcSeaFile)
ENDIF

IF llMultCurr
*--Currency filter
	lnCurPos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'ORDHDR.CCURRCODE'),1)
	IF lnCurPos > 0
	  lcCurStr = LOOGSCROLL.laOGFXFlt[lnCurPos,6]
	  lcCurFile = loOGScroll.gfTempName()
	  llUseCurr = IIF(LEN(lcCurStr)>0,.T.,.F.) AND lfConvertToCursor(lcCurStr,'CCURRCODE',lcCurFile)
	ENDIF
ENDIF 

*date filter
lnDatePos = ASUBSCRIPT(LOOGSCROLL.laOGFXFlt,ASCAN(loOGScroll.laOGFXFlt,'ORDHDR.COMPLETE'),1)
IF lnDatePos > 0
  LowDate  = CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnDatePos,6],1,10))
  HighDate = CTOD(SUBSTR(loOGScroll.laOGFxFlt[lnDatePos ,6],12,21))
ENDIF 

STORE 0.00 TO XBOOK,XBOOKAMT,XCANCEL,XCANCELAMT,GCOUNT

SELECT ORDHDR
SCAN FOR IIF(llUseDiv,SEEK(ORDHDR.CDIVISION,lcDivFile),.T.) ;
		 AND IIF(llUseSea ,SEEK(ORDHDR.SEASON,lcSeaFile),.T.) AND ;
		 IIF(!EMPTY(LowDate) AND !EMPTY(HighDate),BETWEEN(ORDHDR.COMPLETE,LowDate,HighDate),.T.);
		 AND IIF(llMultCurr AND llUseCurr,SEEK(ORDHDR.CCURRCODE,lcCurFile),.T.)
	SCATTER MEMO MEMVAR 
	INSERT INTO (lcRepTmp) FROM MEMVAR 
ENDSCAN 		
 
SELECT(lcRepTmp)
SUM ALL BOOK,BOOKAMT,CANCEL,CANCELAMT TO XBOOK,XBOOKAMT,XCANCEL,XCANCELAMT
COUNT ALL TO GCOUNT FOR !(Status = 'X' .AND. Bulk = 'Y')


IF llMultCurr
  SELECT SycCurr
  SCAN FOR IIF(llUseCurr,SEEK(cCurrCode,lcCurFile),.T.)
    lcCurr = cCurrCode
    lnRecNo = RECNO()
    SELECT (lcRepTmp)
    IF lcRpCurr == "F"
      SEEK(lcCurr)
    ELSE
      LOCATE FOR ALLTRIM(ccurrcode)==ALLTRIM(lcCurr)
    ENDIF
    IF !FOUND()
      LOOP
    ENDIF
    SELECT (lcTmpCur)
    SEEK(lcCurr)
    IF !FOUND()
      APPEND BLANK
      REPLACE CurrCode WITH lcCurr
      lodbfSALESREP.Seek('')
      SELECT SALESREP
      LOCATE
      SCAN
        XSALESREP = REPCODE
        XREPNAME  = SALESREP->NAME
   
        WAIT WINDOW 'Processing salesrep '+ XSALESREP +'...' NOWAIT
        STORE 0   TO XCOUNT
        STORE 0.0 TO BACT,BACTAMT,BAVG,BAVGAMT,BPCNT
        STORE 0.0 TO CACT,CACTAMT,CAVG,CAVGAMT,CPCNT
        SELECT (lcRepTmp)
        IF llRep_div AND lodbfREP_DIV.SEEK(XSALESREP,'REP_DIV')
          *--Get the first record as to scan for all.
          LOCATE
          lcExp = ".T."
          *--Calculate all Booked for this Sales rep if it's Rep1 or Rep2 and for the same division
          *--assigned to it in REP_DIV
          IF lcRpCurr == "F"
            lcForCon = "FOR (lodbfREP_DIV.SEEK(REP1+cDivision,'REP_DIV') AND cCurrCode+REP1 = lcCurr+XSALESREP) ;
                         OR (lodbfREP_DIV.SEEK(REP2+cDivision,'REP_DIV') AND cCurrCode+REP2 = lcCurr+XSALESREP) "
          ELSE
            lcForCon = "FOR (lodbfREP_DIV.SEEK(REP1+cDivision,'REP_DIV') AND REP1 = XSALESREP) ;
                         OR (lodbfREP_DIV.SEEK(REP2+cDivision,'REP_DIV') AND REP2 = XSALESREP) "
          ENDIF               
        ELSE
          *--If Sales rep based on division and this sales rep DOES NOT belongs to any division,
          *--go normally.
          IF lcRpCurr == "F"
            SEEK lcCurr+XSALESREP
            lcExp = "cCurrCode+REP1 = lcCurr+XSALESREP"
          ELSE
            SEEK XSALESREP
            lcExp = "REP1 = XSALESREP"
          ENDIF
          lcForCon = ""
        ENDIF
        SCAN WHILE &lcExp &lcForCon
          XCOUNT  = XCOUNT  + IIF( Status = 'X' .AND. Bulk = 'Y', 0, 1         )
          BACT    = BACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOK      )
          CACT    = CACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCEL    )
          IF lcRpCurr == "F"
            BACTAMT = BACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOKAMT)
            CACTAMT = CACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCELAMT)
          ELSE
            BACTAMT = BACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, IIF(BOOKAMT<>0,gfAmntDisp(BOOKAMT,lcRpCurr,ldRpExDate,lcRpTmpNam),0))
            CACTAMT = CACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, IIF(CANCELAMT<>0,gfAmntDisp(CANCELAMT,lcRpCurr,ldRpExDate,lcRpTmpNam),0))
          ENDIF  
        ENDSCAN

        STORE 0 TO BAVG,BAVGAMT,CAVG,CAVGAMT
        IF XCOUNT<>0
          BAVG    = BACT/XCOUNT
          BAVGAMT = BACTAMT/XCOUNT
          CAVG    = CACT/XCOUNT
          CAVGAMT = CACTAMT/XCOUNT
        ENDIF

        BPCNT=0.0
        IF XBOOKAMT<>0
          BPCNT = ROUND((BACTAMT/XBOOKAMT)*100,1)
        ENDIF

        CPCNT=0.0
        IF XCANCELAMT<>0
          CPCNT = ROUND((CACTAMT/XCANCELAMT)*100,1)
        ENDIF
      
        IF llMultCurr AND lcRpCurr <> "F"
          SELECT SycCurr
          IF lcRpCurr == "F"
            GOTO lnRecNo
          ELSE
            GOTO BOTTOM
          ENDIF
          SELECT (lcRepTmp)
        ENDIF
        m.XSALESREP = XSALESREP
        m.XSALNAME  = SUBSTR(SALESREP->NAME,1,15)
        m.XCOUNT    = XCOUNT
        m.BACT		= FLOOR(BACT)
        m.BACTAMT   = FLOOR(BACTAMT)
        m.BAVG      = FLOOR(BAVG)
        m.BAVGAMT   = FLOOR(BAVGAMT)
        m.BPCNT     = BPCNT
        m.CACT      = FLOOR(CACT)
        m.CACTAMT   = FLOOR(CACTAMT)
        m.CAVG      = FLOOR(CAVG)
        m.CAVGAMT   = FLOOR(CAVGAMT)
        m.CPCNT     = CPCNT
        m.Currcode  = IIF(lcRpCurr <> "F",oAriaApplication.BaseCurrency ,lcCurr)
		INSERT INTO (lcRepFile) FROM MEMVAR 
      ENDSCAN


    ENDIF    
 ENDSCAN    
ELSE
  lodbfSALESREP.Seek('')
  SELECT SALESREP
  SCAN
    *B607900,1 12/24/2006 MMT Bug while previw report and company not nulti Curreny[Start]
    *m.XSALESREP = XSALESREP
    m.XSALESREP =  REPCODE
    *B607900,1 12/24/2006 MMT Bug while previw report and company not nulti Curreny[End]
    
    m.XSALNAME  = SALESREP->NAME
    WAIT WINDOW 'Processing salesrep '+ XSALESREP +'...' NOWAIT
    STORE 0   TO XCOUNT
    STORE 0.0 TO BACT,BACTAMT,BAVG,BAVGAMT,BPCNT
    STORE 0.0 TO CACT,CACTAMT,CAVG,CAVGAMT,CPCNT

	SELECT (lcRepTmp)
    *--If Sales rep based on division and this sales rep belongs to any division
    IF llRep_div AND lodbfREP_DIV.SEEK(XSALESREP,'REP_DIV')
      LOCATE
      lcExp = ".T."
      *--Calculate all Booked for this Sales rep if it's Rep1 or Rep2 and for the same division
      *--assigned to it in REP_DIV
      lcForCon = "FOR (lodbfREP_DIV.SEEK(REP1+cDivision,'REP_DIV') AND REP1 = XSALESREP) ;
                   OR (lodbfREP_DIV.SEEK(REP2+cDivision,'REP_DIV') AND REP2 = XSALESREP) "
    ELSE
      *--If Sales rep based on division and this sales rep DOES NOT belongs to any division,
      *--go normally.
      SEEK XSALESREP
      lcExp = "REP1 = XSALESREP"
      lcForCon = ""
    ENDIF
    SCAN WHILE &lcExp &lcForCon
      XCOUNT  = XCOUNT  + IIF( Status = 'X' .AND. Bulk = 'Y', 0, 1         )
      BACT    = BACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOK      )
      BACTAMT = BACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, BOOKAMT   )
      CACT    = CACT    + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCEL    )
      CACTAMT = CACTAMT + IIF( Status = 'X' .AND. Bulk = 'Y', 0, CANCELAMT )
    ENDSCAN

    STORE 0 TO BAVG,BAVGAMT,CAVG,CAVGAMT
    IF XCOUNT<>0
      BAVG    = BACT/XCOUNT
      BAVGAMT = BACTAMT/XCOUNT
      CAVG    = CACT/XCOUNT
      CAVGAMT = CACTAMT/XCOUNT
    ENDIF

    BPCNT=0.0
    IF XBOOKAMT<>0
      BPCNT = ROUND((BACTAMT/XBOOKAMT)*100,1)
    ENDIF

    CPCNT=0.0
    IF XCANCELAMT<>0
      CPCNT = ROUND((CACTAMT/XCANCELAMT)*100,1)
    ENDIF

    m.XSALESREP = XSALESREP
    m.XSALNAME  = SUBSTR(SALESREP->NAME,1,15)
    m.XCOUNT    = XCOUNT
    m.BACT		= FLOOR(BACT)
    m.BACTAMT   = FLOOR(BACTAMT)
    m.BAVG      = FLOOR(BAVG)
    m.BAVGAMT   = FLOOR(BAVGAMT)
    m.BPCNT     = BPCNT
    m.CACT      = FLOOR(CACT)
    m.CACTAMT   = FLOOR(CACTAMT)
    m.CAVG      = FLOOR(CAVG)
    m.CAVGAMT   = FLOOR(CAVGAMT)
    m.CPCNT     = CPCNT
*
	INSERT INTO (lcRepFile) FROM MEMVAR 

  ENDSCAN

  
ENDIF   
*!*************************************************************
*! Name      : lfFillVars
*: Developer : Mariam Mazhar(MMT)
*! Date      : 07/27/2006
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
FUNCTION lfFillVars

IF !USED('SYCCOMP')
  USE (oAriaApplication.SysPath+"SYCCOMP") ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  
IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    USE (oAriaApplication.SysPath+"SYCINT.DBF") IN 0 
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (oAriaApplication.SysPath+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF  

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]
ENDIF

*-- End Of lfFillVars.
