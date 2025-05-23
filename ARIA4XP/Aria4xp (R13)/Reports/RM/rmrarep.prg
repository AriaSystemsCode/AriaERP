*:***************************************************************************
*: Program file  : RMRAREP
*: Program desc. : RETURN AUTHORIZATION REPORT
*! Date          : 06/04/2006
*: System        : Aria 4 XP
*: Module        : RETURNS MANAGEMENT (RM)
*: Developer     : Wael M. Abo-Shawareb (WSH)
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Issue # : N037696
*:***************************************************************************
*: Example : DO MAPOREC
*:***************************************************************************
*: Modifications:
*: B608480,1 WAM 03/12/2008 Enhance collecting data when no accounts selected
*:***************************************************************************
#Include r:\aria4xp\reports\rm\rmrarep.h
STORE '' TO lcActFltr, lcWarFltr, lcResFltr
STORE '' TO lcStTime, lcEdTime, lnInterval

IF loOgScroll.llOGFltCh
  lcStTime = TIME()

  *-- Get OG Filters
  =lfGetFilters()

  *-- Sort by variable
  DO CASE
    CASE lcRpSort = 'A'
      lcSortBy = 'ACCOUNT+RANO'
    CASE lcRpSort = 'R'
      lcSortBy = 'RANO'
    CASE lcRpSort = 'W'
      lcSortBy = 'CWARECODE+RANO'
    CASE lcRpSort = 'C'
      lcSortBy = 'REASON+RANO'
  ENDCASE

  *-- Create temp file
  =lfCreateTemp()

  *-- Collect data and print the report
  =lfCollData()

  *--Make a copy of the working file to the working directory to be used by crystal
  IF loOgScroll.FileExist(oAriaApplication.WorkDir + lcRpData + ".DBF")
    ERASE (oAriaApplication.WorkDir + lcRpData + ".DBF")
  ENDIF

  SELECT (lcRaTemp)
  COPY TO oAriaApplication.WorkDir + lcRpData + ".DBF" WITH CDX

  lcEdTime   = TIME()
  lnInterval = lfCollTime(lcStTime, lcEdTime)
  * N000682 ,1 Thabet Handle globalization issues [Start]
*  WAIT WINDOW "Selected " + ALLTRIM(STR(RECCOUNT(lcRaTemp))) + " Records in " + ALLTRIM(STR(lnInterval,6,2)) + " Seconds..." NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Selected + ALLTRIM(STR(RECCOUNT(lcRaTemp))) + LANG_Record + ALLTRIM(STR(lnInterval,6,2)) + LANG_Seconds NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Selected,oAriaApplication.GetHeaderText("LANG_Selected",AHEADERFILE)) + ALLTRIM(STR(RECCOUNT(lcRaTemp))) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Record,oAriaApplication.GetHeaderText("LANG_Record",AHEADERFILE)) + ALLTRIM(STR(lnInterval,6,2)) + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Seconds,oAriaApplication.GetHeaderText("LANG_Seconds",AHEADERFILE)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * N000682 ,1 Thabet Handle globalization issues [END]
ENDIF

SELECT (lcRaTemp)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000', 'DIALOG')
  RETURN
ENDIF

*--Create report parameters and cursors arrays
=lfAdjustCRSettings()

=gfDispRe()

*!*************************************************************
*! Name      : lfCollData
*! Developer : Wael M. ABo-Shawareb (WSH)
*! Date      : 06/05/2006
*! Purpose   : COLLECT THE DATA
*!*************************************************************
*! Example   : =lfCollData
*!*************************************************************
FUNCTION lfCollData
* N000682 ,1 Thabet Handle globalization issues [Start]
*WAIT 'Selecting records for report ...' WINDOW NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_Selecting_records WINDOW NOWAIT
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Selecting_records,oAriaApplication.GetHeaderText("LANG_Selecting_records",AHEADERFILE)) WINDOW NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

* N000682 ,1 Thabet Handle globalization issues [END]
SELECT (lcRaTemp)
ZAP

*-- Fill the temporary file
IF !EMPTY(lcActFltr) AND USED(lcActFltr)
  SELECT RETAUTH
  =gfSetOrder("RETAUTHA")

  SELECT (lcActFltr)
  SCAN
    SELECT RETAUTH
    =gfSeek(EVALUATE(lcActFltr + '.Account'))

    SCAN REST WHILE ACCOUNT+RANO = EVALUATE(lcActFltr + '.Account') FOR &lcRpExp
      * N000682 ,1 Thabet Handle globalization issues [Start]
      *WAIT WINDOW 'Selecting data for RA# :' + RANO NOWAIT
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Selecting_data + RANO NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Selecting_data,oAriaApplication.GetHeaderText("LANG_Selecting_data",AHEADERFILE)) + RANO NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

      * N000682 ,1 Thabet Handle globalization issues [Start]
      IF !EMPTY(lcResFltr) AND !SEEK(RETAUTH.REASON, lcResFltr)
        LOOP
      ENDIF
      IF !EMPTY(lcWarFltr) AND !SEEK(RETAUTH.CWARECODE, lcWarFltr)
        LOOP
      ENDIF

      SELECT CUSTOMER
      =gfSeek(IIF(EMPTY(RETAUTH.STORE), 'M', 'S') + RETAUTH.ACCOUNT + RETAUTH.STORE)

      SELECT RETAUTH
      SCATTER MEMVAR
      m.StName     = CUSTOMER.StName
      m.ReasonDesc = lfCodeDesc(REASON, 'REASON')
      m.ReasonDesc = IIF(m.ReasonDesc = 'N/A', '', m.ReasonDesc)
      m.CUSTPO     = LEFT(m.CUSTPO, 10)

      SELECT (lcRaTemp)
      APPEND BLANK
      GATHER MEMVAR
    ENDSCAN
  ENDSCAN
ELSE
  SELECT RETAUTH
  =gfSetOrder("RETAUTH")
  *B608480,1 WAM 03/12/2008 Enhance collecting data when no accounts selected
*!*	  llLoop = .F.
*!*	  llExitLoop = !gfGoTop()
*!*	  DO WHILE !llExitLoop
  =gfseek('')
  SCAN
  *B608480,1 WAM 03/12/2008 (End)

    llLoop = !(&lcRpExp.)
    llLoop = llLoop OR (!EMPTY(lcActFltr) AND !SEEK(RETAUTH.Account, lcActFltr))
    llLoop = llLoop OR (!EMPTY(lcResFltr) AND !SEEK(RETAUTH.REASON, lcResFltr))
    llLoop = llLoop OR (!EMPTY(lcWarFltr) AND !SEEK(RETAUTH.CWARECODE, lcWarFltr))

    IF llLoop
      *B608480,1 WAM 03/12/2008 Enhance collecting data when no accounts selected
*!*	      SELECT RETAUTH
*!*	      llExitLoop = !gfGoNext()
      *B608480,1 WAM (End)

      LOOP
    ENDIF

    SELECT CUSTOMER
    =gfSeek(IIF(EMPTY(RETAUTH.STORE), 'M', 'S') + RETAUTH.ACCOUNT + RETAUTH.STORE)

    SELECT RETAUTH
    * N000682 ,1 Thabet Handle globalization issues [Start]
*    WAIT WINDOW 'Selecting data for RA# :' + RANO NOWAIT
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Selecting_data + RANO NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Selecting_data,oAriaApplication.GetHeaderText("LANG_Selecting_data",AHEADERFILE)) + RANO NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

    * N000682 ,1 Thabet Handle globalization issues [Start]
    SCATTER MEMVAR
    m.StName     = CUSTOMER.StName
    m.ReasonDesc = lfCodeDesc(REASON, 'REASON')
    m.CUSTPO     = LEFT(m.CUSTPO, 10)

    SELECT (lcRaTemp)
    APPEND BLANK
    GATHER MEMVAR
    *B608480,1 WAM 03/12/2008 Enhance collecting data when no accounts selected
*!*	    SELECT RETAUTH
*!*	    llExitLoop = !gfGoNext()
*!*	  ENDDO
  ENDSCAN
  *B608480,1 WAM 03/12/2008 (End)

ENDIF

SELECT RETAUTH
=gfSetOrder("RETAUTH")

SELECT (lcRaTemp)
RETURN .T.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/05/2006
*! Purpose   : Reportt When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

= gfOpenTable(oAriaApplication.DataDir + 'RETAUTH', 'RETAUTH', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'CUSTOMER', 'CUSTOMER', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'WareHous', 'WareHous', 'SH')
= gfOpenTable(oAriaApplication.DataDir + 'CODES', 'CODES', 'SH')

RETURN .T.

*!*************************************************************
*! Name      : lfStatus
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/04/2006
*! Purpose   : To Fill the arrays of status
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStatus()
*!*************************************************************
FUNCTION lfStatus

llCrIstall = (OCCURS('CR', oAriaApplication.CompanyInstalledModules) <> 0)

IF llCrIstall
  DIMENSION laStatT[5], laStatV[5]
  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
*!*	  laStatT[1]= 'Open'
*!*	  laStatT[2]= 'Complete'
*!*	  laStatT[3]= 'Cancel'
*!*	  laStatT[4]= 'Electronic'
*!*	  laStatT[5]= 'Bid'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laStatT[1]= LANG_OPEN
laStatT[1]= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OPEN,oAriaApplication.GetHeaderText("LANG_OPEN",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laStatT[2]= LANG_COMPLETE
laStatT[2]= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPLETE,oAriaApplication.GetHeaderText("LANG_COMPLETE",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laStatT[3]= LANG_CANCEL
laStatT[3]= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CANCEL,oAriaApplication.GetHeaderText("LANG_CANCEL",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laStatT[4]= LANG_ELECT
laStatT[4]= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ELECT,oAriaApplication.GetHeaderText("LANG_ELECT",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laStatT[5]= LANG_BID
laStatT[5]= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BID,oAriaApplication.GetHeaderText("LANG_BID",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 02/11/2013 Globalization changes[END]
  laStatV[1]= 'O'
  laStatV[2]= 'C'
  laStatV[3]= 'X'
  laStatV[4]= 'E'
  laStatV[5]= 'B'
ELSE
  DIMENSION laStatT[4], laStatV[4]
  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
*!*	  laStatT[1]= 'Open'
*!*	  laStatT[2]= 'Complete'
*!*	  laStatT[3]= 'Cancel'
*!*	  laStatT[4]= 'Bid'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laStatT[1]= LANG_OPEN
laStatT[1]= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_OPEN,oAriaApplication.GetHeaderText("LANG_OPEN",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laStatT[2]= LANG_COMPLETE
laStatT[2]= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_COMPLETE,oAriaApplication.GetHeaderText("LANG_COMPLETE",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laStatT[3]= LANG_CANCEL
laStatT[3]= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_CANCEL,oAriaApplication.GetHeaderText("LANG_CANCEL",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laStatT[4]= LANG_BID
laStatT[4]= IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_BID,oAriaApplication.GetHeaderText("LANG_BID",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 MMT 02/11/2013 Globalization changes[Start]
  laStatV[1]= 'O'
  laStatV[2]= 'C'
  laStatV[3]= 'X'
  laStatV[4]= 'B'
ENDIF

RETURN

*************************************************************
*! Name      : lfGetFilters
*! Developer : Wael M. Abo-Shaweareb (WSH)
*! Date      : 06/05/2006
*! Purpose   : Get Optiongrid Filter Cursors
*!*************************************************************
FUNCTION lfGetFilters

LOCAL lnAlias, lcCurName, llFound, lcCond, lnI, lnSpPos, lcDate1, lcDate2
lnAlias = SELECT(0)
lcRpExp = ".T."

*-- Status Filter
llCrIstall = (OCCURS('CR', oAriaApplication.CompanyInstalledModules) <> 0)
lcRpStatus = lfCheckFilter(1, "RETAUTH.STATUS")
lcRpStatus = IIF(EMPTY(lcRpStatus), 'OCXBE', lcRpStatus)
lcRpExp    = "Status $ '" + lcRpStatus + "'"

*-- Date Issued Filter
lcCond = lfCheckFilter(1, 'RETAUTH.RADATE')
IF !EMPTY(lcCond)
  lnSpPos = ATC('|', lcCond)
  lcDate1 = DTOS(CTOD(IIF(lnSpPos > 0, SUBSTR(lcCond, 1, lnSpPos - 1), ALLTRIM(lcCond))))
  lcDate2 = DTOS(CTOD(IIF(lnSpPos > 0, SUBSTR(lcCond, lnSpPos + 1), "")))
  lcRpExp = lcRpExp + " AND BETWEEN(DTOS(RETAUTH.RADATE), '" + lcDate1 + "', '" + lcDate2 + "')"
ENDIF

*-- Date Received Filter
lcCond = lfCheckFilter(1, 'RETAUTH.RETDATE')
IF !EMPTY(lcCond)
  lnSpPos = ATC('|', lcCond)
  lcDate1 = DTOS(CTOD(IIF(lnSpPos > 0, SUBSTR(lcCond, 1, lnSpPos - 1), ALLTRIM(lcCond))))
  lcDate2 = DTOS(CTOD(IIF(lnSpPos > 0, SUBSTR(lcCond, lnSpPos + 1), "")))
  lcRpExp = lcRpExp + " AND BETWEEN(DTOS(RETAUTH.RETDATE), '" + lcDate1 + "', '" + lcDate2 + "')"
ENDIF

*-- Account Filter
lcCurName = lfCheckFilter(1, 'CUSTOMER.ACCOUNT')
llFound   = !EMPTY(lcCurName) AND USED(lcCurName) AND RECCOUNT(lcCurName) > 0
IF llFound
  lcActFltr = lcCurName
  SELECT (lcActFltr)
  INDEX ON Account TAG (lcActFltr)
ELSE
  IF TYPE("lcActFltr") = "C" AND USED(lcActFltr)
    USE IN (lcActFltr)
  ENDIF
  lcActFltr = ''
ENDIF

*-- Warehouse Filter
lcCurName = lfCheckFilter(1, 'WAREHOUS.CWARECODE')
llFound   = !EMPTY(lcCurName) AND USED(lcCurName) AND RECCOUNT(lcCurName) > 0
IF llFound
  lcWarFltr = lcCurName
  SELECT (lcWarFltr)
  INDEX ON CWARECODE TAG (lcWarFltr)
ELSE
  IF TYPE("lcWarFltr") = "C" AND USED(lcWarFltr)
    USE IN (lcWarFltr)
  ENDIF
  lcWarFltr = ''
ENDIF

*-- Reason Filter
lcCond = lfCheckFilter(3, 'RETAUTH.REASON')
IF !EMPTY(lcCond)
  lcResFltr = loOgScroll.gfTempName()
  CREATE CURSOR (lcResFltr) (Reason C(6))
  DIMENSION laValues[1]
  =gfSubStr(lcCond, @laValues, '|')
  SELECT (lcResFltr)
  INDEX ON Reason TAG (lcResFltr)
  FOR lnI = 1 TO ALEN(laValues,1)
    APPEND BLANK
    REPLACE Reason WITH laValues[lnI]
  ENDFOR
ELSE
  IF TYPE("lcResFltr") = "C" AND USED(lcResFltr)
    USE IN (lcResFltr)
  ENDIF
  lcResFltr = ''
ENDIF

SELECT (lnAlias)
RETURN

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

*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/05/2006
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRTables[1]
loOgScroll.laCRTables[1] = oAriaApplication.WorkDir + lcRpData + ".DBF"

DIMENSION loOgScroll.laCRParams[4,2]
loOgScroll.laCRParams[1,1] = 'ReportName'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loOgScroll.laCRParams[1,2] = LANG_Return_Athorization_Report
loOgScroll.laCRParams[1,2] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Return_Athorization_Report,oAriaApplication.GetHeaderText("LANG_Return_Athorization_Report",AHEADERFILE))
*N000682,1 11/20/2012 MMT Globlization changes[End]


loOgScroll.laCRParams[2,1] = 'op_title'
loOgScroll.laCRParams[2,2] = lcRpOpTi

loOgScroll.laCRParams[3,1] = 'SortBy'
loOgScroll.laCRParams[3,2] = lcRpSort

loOgScroll.laCRParams[4,1] = 'lMultiWare'
loOgScroll.laCRParams[4,2] = IIF(ALLTRIM(UPPER(gfGetMemVar('M_WareHouse'))) = 'Y', 1, 0)

loogScroll.cCROrientation = 'L'

*************************************************************
*! Name      : lfCreateTemp
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/05/2006
*! Purpose   : Create temp file and add needed fields
*!*************************************************************
FUNCTION lfCreateTemp

*-- Create the temporary file from the ORDCANLN table and add some necessary fields to it.
SELECT RETAUTH
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)

DIMENSION laFileStru[lnFileStru+2,18]
laFileStru[lnFileStru+1,1] = 'StName'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 30
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'ReasonDesc'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 30
laFileStru[lnFileStru+2,4] = 0

FOR lnI = 7 TO 16
  STORE '' TO laFileStru[lnFileStru+1,lnI], laFileStru[lnFileStru+2,lnI]
ENDFOR
STORE .F. TO laFileStru[lnFileStru+1,5], laFileStru[lnFileStru+1,6], ;
             laFileStru[lnFileStru+2,5], laFileStru[lnFileStru+2,6]
STORE 0 TO laFileStru[lnFileStru+1,17], laFileStru[lnFileStru+1,18],;
           laFileStru[lnFileStru+2,17], laFileStru[lnFileStru+2,18]

=gfCrtTmp(lcRaTemp, @laFileStru, lcSortBy, lcRaTemp)

SELECT (lcRaTemp)
SET ORDER TO (lcRaTemp)

RETURN
*-- End of lfCreateTemp.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Mohamed Badran (MAB)
*! Date      : 12/09/1998
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*!
FUNCTION lfCollTime
LPARAMETERS lcStart, lcEnd

lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))

RETURN (lnEnd - lnStart)
*-- End of lfCollTime.

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Wael M. Abo-Shawareb (WSH)
*! Date      : 06/05/2006
*! Purpose   : Get code description
*!*************************************************************
*! Example   : =lfCodeDesc(Reason, "REASON")
*!*************************************************************
FUNCTION lfCodeDesc
LPARAMETERS lcCodeVal, lcCodeFld

LOCAL lnAlias, lcRetVal
lnAlias  = SELECT(0)
lcRetVal = ''

SELECT Codes
IF gfSeek("N" + lcCodeVal + "N" + lcCodeFld)
  lcRetVal = cDiscrep
ENDIF

SELECT (lnAlias)
RETURN lcRetVal
