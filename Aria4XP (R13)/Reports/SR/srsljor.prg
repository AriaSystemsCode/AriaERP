*:***************************************************************************
*: Program file  : SRSLJOR
*: Program desc. : Summary Sales Journal Report.
*: For Report    : ....
*: System        : Aria4XP
*: Module        : Sales Representative (SR)
*: Developer     : AYMAN MAHMOUD AHMED(AYM)
*: Date          : 12/17/2004
*: Reference     : T20060908.0028 - E302340
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : *************************
*:               : ****************************************
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SRSLJOR
*:***************************************************************************


*----------------------- Report Code Begin -----------------------------

*!*	_SCREEN.Visible = .T.
*!*	ACTIVATE WINDOW TRACE
*!*	SUSPEND
#Include R:\aria4xp\Reports\SR\SRSLJOR.h
lcStTime   = TIME()    && Time in which we start collect data.

IF TYPE('lcLastExpr') = 'N'
  lcLastExpr = ''
ENDIF
lnInvPos = lfItmPos('INVHDR.INVDATE')

*-- lcLastExpr : Last <Run> OR <Preview> lcRpExp.
*-- llChFactor : .T. if user change Factored/Non Factored/Both selection.
*-- llChInv    : .T. if user change Invoces/Void Invoices/Both selection.

IF llClearFn OR llOGFltCh


  STORE .F. TO llClearFn,llChFactor,llChInv    && Unrise all hidden Critria variables.
  lcLastExpr = lcRpExp   && Save current report expression, To later know that user change critria.

  *-- if you have previous data clear workfile then recreate it. [begin]
  IF !USED(lcWorkFile) OR (RECCOUNT(lcWorkFile) > 0)
    IF USED(lcWorkFile)
      USE IN (lcWorkFile)
    ENDIF
    = lfCreatCur()  && Create work cursor.
  ENDIF
  *-- if you have previous data clear workfile then recreate it. [end]

	lcFullExp=" .T. "
	IF llMultCurr
		lcCURRFile = ''
		lcCURRFile = lfCheckFilter(1, 'INVHDR.CCURRCODE')
		LLCURR   = !EMPTY(lcCURRFile ) AND USED(lcCURRFile ) AND RECCOUNT(lcCURRFile ) > 0
		IF LLCURR
		  SELECT (lcCURRFile )
		  INDEX ON CCURRCODE TAG (lcCURRFile )
		  lcFullExp=lcFullExp+" AND SEEK(CCURRCODE,'"+lcCURRFile +"')"
		ELSE
		  IF TYPE("lcCURRFile ") = "C" AND USED(lcCURRFile )
		    USE IN (lcCURRFile )
		  ENDIF
		  lcCURRFile = ''
		ENDIF

	ENDIF


	* SalesRep Filter
	lcRepFltr= ''
	lcRepFltr= lfCheckFilter(1, 'INVHDR.REP1')
	llRepFltr   = !EMPTY(lcRepFltr) AND USED(lcRepFltr) AND RECCOUNT(lcRepFltr) > 0
	IF llRepFltr
	  SELECT (lcRepFltr)
	  INDEX ON REPCODE TAG (lcRepFltr)
	  lcFullExp=lcFullExp+" AND (SEEK( INVHDR.REP1,'"+lcRepFltr+"') .OR. SEEK( INVHDR.REP2,'"+lcRepFltr+"'))"
	ELSE
	  IF TYPE("lcRepFltr") = "C" AND USED(lcRepFltr)
	    USE IN (lcRepFltr)
	  ENDIF
	  lcRepFltr= ''
	ENDIF

	
	* Check if there is a filter on DIVISION
	lcCurName = lfCheckFilter(3, 'INVHDR.CDIVISION')
	lcDiv   = loOgScroll.gfTempName()
	llDiv   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcDiv ,"CDivision")
	IF llDiv
	  SELECT (lcDiv)
	  INDEX on CDivision TAG (lcDiv)
	  lcFullExp=lcFullExp+" AND SEEK(INVHDR.CDIVISION,'"+lcDiv+"')"
	ENDIF

	* Check if there is a filter on TERMCODE
	lcCurName = lfCheckFilter(3, 'INVHDR.CTERMCODE')
	lcTRM   = loOgScroll.gfTempName()
	llTRM   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcTRM   ,"CDTRM")
	IF llTRM
	  SELECT (lcTRM   )
	  INDEX on CDTRM TAG (lcTRM   )
	  lcFullExp=lcFullExp+" AND SEEK(CTERMCODE,'"+lcTRM   +"')"
	ENDIF



		
	* Check if there is a filter on Style SEASON
	lcCurName = lfCheckFilter(3, 'INVHDR.SEASON')
	lcSea  = loOgScroll.gfTempName()
	llSea   = !EMPTY(lcCurName ) AND lfStr2Curs(lcCurName ,lcSea  ,"SEASON")
	IF llSea
	  SELECT (lcSea  )
	  INDEX on SEASON TAG (lcSea  )
	  lcFullExp=lcFullExp+" AND SEEK(INVHDR.SEASON,'"+lcSea+"') "
	ENDIF

	* FACTORED AND VOIDED
	lcFullExp=lcFullExp+IIF(lcRpType = 'F'," AND !EMPTY(CFACCODE)",IIF(lcRpType = 'N'," AND EMPTY(CFACCODE) ",""))

	lcFullExp=lcFullExp+IIF(lcRpList != 'B'," AND STATUS = lcRpList ", "")



	IF  !EMPTY(laOGFxFlt[lnInvPos,6])
	  ldStrtDate = CTOD(SUBSTR(laOGFxFlt[lnInvPos,6],1, ATC('|',laOGFxFlt[lnInvPos,6])-1))
	  ldEndDate  = CTOD(SUBSTR(laOGFxFlt[lnInvPos,6],   ATC('|',laOGFxFlt[lnInvPos,6])+1))
	  IF llVoidOnly
	    lcFullExp=lcFullExp+" AND BETWEEN(VDATE,ldStrtDate,ldEndDate) "
	  ELSE
	    lcFullExp=lcFullExp+" AND BETWEEN(INVDATE,ldStrtDate,ldEndDate) "
	  ENDIF
	ENDIF
  =gfOpenTABLE(oAriaApplication.DataDir + "INVHDR" , "INVHDR",'SH', , .T.)

  SELECT INVHDR
  gfSetOrder('INVHDR')   && INVOICE
  =GFSEEK('')
  PRIVATE lcSlRep2
  SCAN REST WHILE INVOICE='' FOR &lcFullExp
    SCATTER MEMVAR MEMO
    INSERT INTO (lcWorkFile) FROM MEMVAR
    IF !EMPTY(m.Rep2)
       =lfChRp1To2()
    ENDIF

  ENDSCAN
  *-- Data collection End

ENDIF       && end If user change report critria, Collect report data.

*-- Select Master report file.
SELECT (lcWorkFile)
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO Customer ,;
                Rep1 INTO Salesrep , ccurrcode INTO Syccurr

lcEdTime = TIME()  && Time in which we finish collect data.
lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
* N000862 ,1 Thabet Handle globalization issues [Start]
*WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Records in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' TIMEOUT 2
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW Lang_Selected + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + Lang_RECORDS + ALLTRIM(STR(lnInterval,6,2)) + Lang_SECONDS  TIMEOUT 2
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Selected,oAriaApplication.GetHeaderText("Lang_Selected",AHEADERFILE)) + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_RECORDS,oAriaApplication.GetHeaderText("Lang_RECORDS",AHEADERFILE)) + ALLTRIM(STR(lnInterval,6,2)) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_SECONDS,oAriaApplication.GetHeaderText("Lang_SECONDS",AHEADERFILE))  TIMEOUT 2
*N000682,1 11/20/2012 MMT Globlization changes[End]


* N000862 ,1 Thabet Handle globalization issues [END]
SELECT (lcWorkFile)
GO TOP

IF RECCOUNT(lcWorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok >
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ELSE
INDEX on rep1+ccurrcode+invoice  TAG (lcWorkFile)

ENDIF

*-- Calculate From and To dates. [Begin]
IF EMPTY(laOGFxFlt[lnInvPos ,6])
  lcInvDateF = DTOC(CTOD(SPACE(10)))
  lcInvDateT = DTOC(gdSysDate)
ELSE
  lcInvDateF = PADR(SUBSTR(laOGFxFlt[lnInvPos ,6],1,ATC('|',laOGFxFlt[lnInvPos ,6])-1),10)
  lcInvDateT = PADL(SUBSTR(laOGFxFlt[lnInvPos ,6],ATC('|',laOGFxFlt[lnInvPos ,6])+1),10)
ENDIF

IF EMPTY(CTOD(lcInvDateT))
  lcInvDateT = DTOC(gdSysDate)
ENDIF
*-- Calculate From and To dates. [End..]
loogScroll.cCROrientation = 'P'
DO gfDispRe WITH EVAL('lcRpForm')



*!*************************************************************
*! Name      : lfChRp1To2
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/30/1998
*! Purpose   : Change line for Rep1 to have information for Rep2.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code Scan loop section.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : Null
*!*************************************************************
*! Example   : =lfChRp1To2()
*!*************************************************************
*!
FUNCTION lfChRp1To2
m.Rep1      = m.Rep2
m.Comm1     = m.Comm2
m.CommAmt1  = m.CommAmt2
m.VCommAmt1 = m.VCommAmt2
INSERT INTO (lcWorkFile) FROM MEMVAR
*-- end of lfChRp1To2.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/28/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfCreatCur
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*!
FUNCTION lfwRepWhen


  DIMENSION laTempStru[1,4]
  laTempStru = ''
  =gfOpenTABLE(oAriaApplication.DataDir + "INVHDR" , "INVHDR",'SH', , .T.)

  SELECT INVHDR
  = AFIELDS(laTempStru)
  = lfCreatCur()  && Create work cursor.

  IF llMultCurr
    SET ORDER TO CCURRCODE IN SYCCURR  && To VALIDATE currency code.
  ENDIF

*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/1998
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
*!
FUNCTION lfwOldVal
laOldVal = EVALUATE(OGSYS18())
*-- end of lfwOldVal.


*!*************************************************************
*! Name      : lfvChFact
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/98
*! Purpose   : 1- Change Factored/Non Factored logical variable To recollect data.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid [Factored/Non Factored/Both]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvChFact()
*!*************************************************************
*!
FUNCTION lfvChFact
llChFactor = .T.
*-- end of lfvChFact.

*!*************************************************************
*! Name      : lfvList
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/98
*! Purpose   : Change Invoices/Void Invoices logical variable To recollect data.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid [Invoices/Void Invoices/Both]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvList()
*!*************************************************************
*!
FUNCTION lfvList
llChInv = .T.  && Logical invoice variable.
llVoidOnly = IIF(lcRpList='V',.T.,.F.)  && To use it in .FRX
*-- end of lfvList.

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/29/98
*! Purpose   : Activate currency display screen to get user
*!           : selection for currencies.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfRepCur
*!*************************************************************
*! Called from : Option Grid [Currency Display Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
*!
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- end of lfvCurDisp.



*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/98
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code section.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Example   : =lfCollTime()
*!*************************************************************
*!
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- end of lfCollTime.

*!*************************************************************
*! Name      : lfCreatCur
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/98
*! Purpose   : Create cursor
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : Cursor Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCreatCur()
*!*************************************************************
*!
FUNCTION lfCreatCur
CREATE CURSOR (lcWorkFile) ;
   FROM ARRAY laTempStru
INDEX ON Rep1 + Invoice TAG (lcWorkFile)
*-- end of lfCreatCur.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/27/1998
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close > button.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
*!
FUNCTION lfClearRep
llClearFn = .T.    && If you run filter you must create cursor again.
*-- Close temp. opended files, if it used.
*-- Delete temporary work file.
IF USED(lcWorkFile)
 USE IN (lcWorkFile)
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfPreRun
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/29/98
*! Purpose   : Pre_Preview Function To call Temporary .FRX again
*!           : to evaluate #OBJDISP objects again.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report Generator when press <Preview> or <Run>
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPreRun()
*!*************************************************************
FUNCTION lfPreRun
RETURN .T.

*-- end of lfPreRun.



*!*************************************************************
*! Name      : lfItmPos
*! Developer : BASSEM RAAFAT ERNEST (BWA)
*! Date      : 17/04/2004
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.


*************************************************************
*! Name      : lfCheckFilter
*! Developer : Saeed Mohammed (SMM)
*! Date      : 09/07/2004
*! Purpose   : Check if the filter was selected
*!*************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter
LOCAL lcReturn, lnPOS
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  CASE lnArrayType = 3
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]
    ELSE
      lcReturn = ""
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn

*!*************************************************************
*! Name      : lfStr2Curs
*! Developer : Ayman Mahmoud Ahmed(AYM)
*! Date      : 02/11/2005
*! Purpose   : Create cursor from string filters
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfStr2Curs
PARAMETERS lcString , lccursor , lcFieldsName

CREATE CURSOR (lcCursor) (&lcFieldsName. C(6))
DO WHILE AT('|',lcString)> 0
  lcFieldsValue  = SUBSTR(lcString,1,AT('|',lcString)-1)
  lcString = SUBSTR(lcString,AT('|',lcString)+1)
  SELECT (lcCursor)
  APPEND BLANK
  REPLACE &lcFieldsName. WITH lcFieldsValue
ENDDO
SELECT (lcCursor)
APPEND BLANK
REPLACE &lcFieldsName. WITH lcString

