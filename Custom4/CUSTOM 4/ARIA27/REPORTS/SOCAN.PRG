*:************************************************************************
*: Program file  : SOCAN.Prg
*: Program desc. : Cancellation report.
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SO, PO, MF
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 08/27/1998
*: REF           : *300979,1
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGrCodes(), lfwOGWhen(), lfNonMaj(), lfsrvSty()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO SOCAN
*:************************************************************************
*: Modifications :
*:B602541,1 AMM 02/15/1999 Fix the bug of wrong filter on style group.
*:B603102,1 ABD 08/19/1999 Fix the bug  "missing}".
*:E500271,4 SSE 07/14/1999 Add popup in OG to filter for the flag field named
*:E500271,4                lEDIOrder which indicate whether these Orders is
*:E500271,4                coming from EDI Module or not.
*:B802462,1 RAM 8/19/1999  Made some changes in the variable (lcRpType)
*:B802462,1                and added a new variable in (SYREPUVR).
*:E301430,1 AME 6/25/2000  Add Season & Division to var filter.
*:B603721,1 ABD 06/29/200  Fix Two couple of spelling mistakesin 
*:B603721,1 ABD 06/29/200  1) Cancellation has 2 "ll" not 1 "l" as on the main menu.
*:B603721,1 ABD 06/29/200     And Modify in Sycmenu 
*:B603721,1 ABD 06/29/200   2)in the status selection box there is "bit" which I 
*:B603721,1 ABD 06/29/200     presume should be "bid" 
*:B803652,1 ADEL 09/27/2000 Get the order cancelled lines missed in OrdCanLn.
*:B604019,1 ADEL 11/13/2000 Fix the bug of 'Duplicate field name' Error.
*:B604592,1 AME 07/25/2001 Fix the bug of not printing canc. reason in lines missed in OrdCanLn. 
*:B604771,1 AME 08/05/2001 Fix the bugs of not printing cancellation date in some cases
*:B604771,1 AME            and not printing scale headr for ever (also some changes in Frx)
*:B606493,1 BWA 01/06/2003 Fix the bugs of not printing the scale size in case the is differ.[FIX IN FRX]
*:B122926,1 NNA 05/26/2004 Fix bug that the report don't show the canceled style if you chose 
*:B122926,1                it only from the style option
*:B608064,1 NNA 04/29/2007 Fix bug that Order cancellation report does not work correctly by selecting account if this account
*:B608064,1 NNA            has orders with stores
*:************************************************************************

lcStyTitle = gfItemMask('HI')             && Style title
lcTime     = TIME()                       && Variable to hold the Time
STORE SPACE (0) TO lcReason, lcDivision, lcStat, lcSeason, lcName, lcStWh
lcRpForm = 'SOCAN'
lcMainF  = gfTempName()
llDyelot = (gfGetMemvar('M_DYELOT',gcAct_Comp)  ='Y')

*B122926,1 NNA 05/26/2004 (Begin) Open the style file again with another name
lcStyFil  = gfTempName()
IF !USED(lcStyFil)
  = gfOpenFile(gcDataDir+'STYLE' ,gcDataDir+'STYLE', 'SH', @lcStyFil, .T.)
ENDIF
*B122926,1 NNA (End)
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE
DO CASE
  *-- Sales Order Cancellation report
  CASE lcRepTyp = 'O'
    *-- Groups of the .FRX
    lcSecGr = 'CORDTYPE+ACCOUNT'
    lcTrdGr = 'CORDTYPE+ACCOUNT+ORDER'
    lcFthGr = 'CORDTYPE+ACCOUNT+ORDER+STORE'
    * Add the filter of status
    IF lcStatus <> "L"
      lcRpExp = '(' +lcRpExp+ ") .AND. (ORDHDR.Status = '" + lcStatus +  "')"
    ENDIF

    *-- Print Order or contract or both as the user choose
    IF lcRpType = "L"
      lcRpExp = '(' +lcRpExp+ ") .AND. (ORDHDR.CORDTYPE $ 'OC' )"
    ELSE
     *B603102,1 ABD 08/19/99 Fix the bug  "missing}". [BEGIN]
     *lcRpExp = '(' +lcRpExp+ ") .AND. (ORDHDR.CORDTYPE = '" + lcRpType +  "')"
      lcRpExp = '(' +lcRpExp+ ") .AND. (ORDHDR.CORDTYPE = [" + lcRpType +  "])"
     *B603102,1 ABD 08/19/99 Fix the bug  "missing}". [END]    
    ENDIF
    
    *E500271,4 add Field lEDIOrder to the filter expression [Begin.]
    IF !EMPTY(lcRpEdiFlt)
      lcRpExp = lcRpExp + lcRpEdiFlt
    ENDIF
    *E500271,4 add Field LEDIORDER to the filter expression [End.]

    *-- Built the temporary file
    =lfBuildST()
  *-- Purchase order cancellation report
  CASE lcRepTyp = 'P'
    *-- Groups of the .FRX
    lcSecGr = 'CORDTYPE+VENDOR'
    lcTrdGr = 'CORDTYPE+VENDOR+ORDER'
    lcFthGr = 'CORDTYPE+VENDOR+ORDER+CWARECODE'
    * Add the filter of status
    IF lcStatus <> "L"
      lcRpExp = '(' +lcRpExp+ ") .AND. (POSHDR.Status = '" + lcStatus +  "')"
    ENDIF
    *-- Print Purchase Order or contract or both as the user choose
    IF lcRpTyp = "L"
      lcRpExp = '(' +lcRpExp+ ") .AND. (POSHDR.CSTYTYPE $ 'PC' )"
    ELSE
      lcRpExp = '(' +lcRpExp+ ") .AND. (POSHDR.CSTYTYPE = '" + lcRpTyp +  "')"
    ENDIF
    *-- Build the temporary file
    = lfBuildPT()
  *-- Cutting ticket cancellation report
  CASE lcRepTyp = 'C'
    *-- Groups of the .FRX
    lcSecGr = 'CORDTYPE'
    lcTrdGr = 'CORDTYPE+ORDER'
    lcFthGr = 'CORDTYPE+ORDER+CWARECODE'
    * Add the filter of status
    IF lcStatus <> "L"
      lcRpExp = '(' +lcRpExp+ ") .AND. (CUTTKTH.Status = '" + lcStatus +  "')"
    ENDIF
    *-- Built the temporary file
    = lfBuildCT()
ENDCASE

SELECT (lcMainF)
LOCATE

*-- Display the report
DO gfDispRe WITH EVAL('lcRpForm')

SELECT STYLE
SET RELATION TO

*B122926,1 NNA 05/26/2004 (Begin) Remove The OrdCanln's Relations
SELECT ORDCANLN
SET RELATION TO
*B122926,1 NNA (End)

IF USED(lcMainF)
  USE IN (lcMainF)
  ERASE (gcWorkDir+lcMainF+'.DBF')
  ERASE (gcWorkDir+lcMainF+'.CDX')
ENDIF

*!*************************************************************
*! Name      : lfGrCodes
*! Developer : AHMED MOHAMED IBRAHIM (AMM)
*! Date      : 08/27/1998
*! Purpose   : To get some codes descriptions
*!*************************************************************
*! Called from : SOCAN.FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGrCodes()
*!*************************************************************
FUNCTION lfGrCodes

DO CASE
  *-- Sales order report
  CASE lcRepTyp = 'O'
    lcName = customer.btname
    lcStWh = IIF(ORDHDR.MULTI='Y',STORE,'******')
    lcDivision = gfCodDes(ordhdr.cdivision, 'CDIVISION')
    lcSeason   = gfCodDes(ordhdr.Season, 'SEASON')
    
    DO CASE
      CASE ORDHDR.STATUS = 'O'
        lcStat = 'Open'
      CASE ORDHDR.STATUS = 'H'
        lcStat = 'Hold'
      CASE ORDHDR.STATUS = 'C'
        lcStat = 'Complete'
      CASE ORDHDR.STATUS = 'B'
        *B603721,1 ABD Fix spelling in status selection box there is "bit" that 
        *B603721,1 ABD should To be "bid".  [Begin]
        *lcStat = 'Bit'
        lcStat = 'Bid'
        *B603721,1 ABD [End]
      CASE ORDHDR.STATUS = 'X'
        lcStat='Cancel'      
    ENDCASE
  *-- Purchase order report
  CASE lcRepTyp = 'P'
    lcName = Apvendor.cVenComp
    lcStWh = cWareCode
    lcDivision = gfCodDes(poshdr.cdivision, 'CDIVISION')
    lcSeason = ''
    DO CASE
      CASE POSHDR.STATUS = 'O'
        lcStat='Open'
      CASE POSHDR.STATUS = 'C'
        lcStat='Complete'
      CASE POSHDR.STATUS = 'X'
        lcStat='Cancel'
      CASE POSHDR.STATUS = 'S'
        lcStat='Closed'
      CASE POSHDR.STATUS = 'H'
        lcStat='Hold'
    ENDCASE
  *-- Cutting ticket report
  CASE lcRepTyp = 'C'
    lcName = ''
    lcStWh = cWareCode
    lcDivision = gfCodDes(CUTTKTH.cdivision, 'CDIVISION')
    lcSeason   = gfCodDes(CUTTKTH.Season, 'SEASON')
      DO CASE
        CASE CUTTKTH.STATUS = 'O'
          lcStat = 'Open'
        CASE CUTTKTH.STATUS = 'H'
          lcStat = 'Hold'
        CASE CUTTKTH.STATUS = 'X'
          lcStat = 'Cancel'
        CASE CUTTKTH.STATUS = 'S'
          lcStat = 'Closed'
        CASE CUTTKTH.STATUS = 'C'
          lcStat = 'Complete'
        CASE CUTTKTH.STATUS = 'A'
          lcStat = 'Actual'
      ENDCASE
ENDCASE

RETURN ''

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Ahmed Mohamed Mohamed
*! Date      : 08/27/1998
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen
*-- Set this variable to define which report we will print
DO CASE
  CASE GCACT_APPL='MF'
    lcRepTyp = 'C'
  CASE GCACT_APPL='PO'
    lcRepTyp = 'P'
  CASE GCACT_APPL='SO'
    lcRepTyp = 'O'
ENDCASE

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Ahmed Mohamed Mohamed  (AMM)
*! Date      : 08/27/1998
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

llCodeF = .F.
lnClrPo = 0
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.

* Array to collect data about all segments in the style code structure
DIMENSION laMajSeg[1,1]

= gfItemMask(@laMajSeg)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' .AND. !llCodeF
  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
    lnFreeLen = LEN(lcNonMajPi)
  ENDIF

  *-- If you Find Color Type or Find previous Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'
      
      *-- Color position
      lnClrPo    = laMajSeg[lnI,4]
      
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      *-- Picture
      lcNonMajPi = laMajSeg[lnI,3]
      *-- NonMajor title
      lcColorTt = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      lnColorLen = LEN(lcNonMajPi)
      EXIT
    ELSE
      llCodeF = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

*-- Compute Free/Color Items in Style Structure. [End]

* get the style major segement title
lcMajTtl =gfItemMask("HM")

lcMajPic = "@! " + gfItemMask("PM")
RETURN lnClrPo

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/28/98
*! Purpose   : To set relation on or off when running the in range function 
*!             of the style setting in the option grid.
*!*************************************************************
*! Called from : Report Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************
FUNCTION lfsrvSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  SET ORDER TO TAG CSTYLE IN STYLE
ELSE
  SET ORDER TO TAG STYLE IN STYLE
ENDIF

*!*************************************************************
*! Name      : lfBuildST
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/27/98
*! Purpose   : To build the temporary file in case of sales order 
*!             cancellation report.
*!*************************************************************
*! Called from : SOCAN.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfBuildST()
*!*************************************************************
FUNCTION lfBuildST

*-- Set necessary relations to get the required filter
SELECT ORDLINE
SET RELATION TO STYLE INTO STYLE ADDITIVE

SELECT ORDCANLN
SET RELATION TO CORDTYPE+ORDER+STR(LINENO,6) INTO ORDLINE ADDITIVE
SET RELATION TO CORDTYPE+ORDER INTO ORDHDR ADDITIVE
SET RELATION TO IIF(EMPTY(STORE),'M','S')+ACCOUNT+STORE INTO CUSTOMER ADDITIVE

*B122926,1 NNA 05/26/2004 (Begin) make a relation between the ORDCANLN file and the style file
*B122926,1 NNA             by the new name to get the styles that cancelled from the orders
SET RELATION TO STYLE INTO (lcStyFil) ADDITIVE 
*B122926,1 NNA (End)

*-- Create the temporary file from the ORDCANLN table and add some necessary fields to it.
*B803652,1 (Begin) Change the way the tempn file is created as Style,Account,Store,Dyelot fields
*B803652,1         has been permenantly added to OrdCanLn.dbf.
*=AFIELDS(laFileStru)
*lnFileStru = ALEN(laFileStru,1)
*DIMENSION laFileStru[lnFileStru+4,4]
*lnFileStru = lnFileStru+1
*laFileStru[lnFileStru,1] = 'ACCOUNT'
*laFileStru[lnFileStru,2] = 'C'
*laFileStru[lnFileStru,3] = 5
*laFileStru[lnFileStru,4] = 0
*lnFileStru = lnFileStru+1
*laFileStru[lnFileStru,1] = 'STORE'
*laFileStru[lnFileStru,2] = 'C'
*laFileStru[lnFileStru,3] = 8
*laFileStru[lnFileStru,4] = 0
*lnFileStru = lnFileStru+1
*laFileStru[lnFileStru,1] = 'STYLE'
*laFileStru[lnFileStru,2] = 'C'
*laFileStru[lnFileStru,3] = 19
*laFileStru[lnFileStru,4] = 0
*lnFileStru = lnFileStru+1
*laFileStru[lnFileStru,1] = 'DYELOT'
*laFileStru[lnFileStru,2] = 'C'
*laFileStru[lnFileStru,3] = 10
*laFileStru[lnFileStru,4] = 0
*Create Table (gcWorkDir+lcMainF) FROM ARRAY laFileStru
COPY STRUCTURE TO (gcWorkDir+lcMainF)
=gfOpenFile(gcWorkDir+lcMainF,'','EX')
*---Create new index.
lcNewIndex = SUBSTR(lcMainF,1,7)+'1'
INDEX ON cordtype+order+STR(lineno,6) TAG (lcNewIndex)
*B803652,1 (End)
INDEX ON CORDTYPE+ACCOUNT+ORDER+STORE+STYLE TAG (lcMainF) 
*-- Fill the temporary file

*B608064,1 NNA 04/29/2007 (Begin) Check if the Account has Stores or not to replace the lcRpExp
*B608064,1 NNA            laOgFxFlt(3,6)-->has the Temp file that hold accounts that user select
*B608064,1 NNA            lcRpExp --> has accounts that user Select but with type 'M' so it doesn't search for Stores
IF USED(laOgFxFlt(3,6)) AND RECCOUNT(laOgFxFlt(3,6))>0
  SELECT laOgFxFlt(3,6)
  LOCATE
  *B608064,3 TMI 05/13/2007 [Start] Comment this code replace it with another way that is do not rely on the customer type in filtering
  *DO WHILE !EOF()
  *  IF SEEK('S'+EVAL(laOgFxFlt(3,6)+'.ACCOUNT'),'CUSTOMER')
  *     lcRpExp = STRTRAN(lcRpExp,'M'+EVAL(laOgFxFlt(3,6)+'.ACCOUNT'),'S'+EVAL(laOgFxFlt(3,6)+'.ACCOUNT'))
  *  ENDIF
  *  SKIP
  *  LOOP
  *ENDDO
  SCAN
    lcRpExp = STRTRAN(lcRpExp,'M'+ACCOUNT,''+ACCOUNT)
  ENDSCAN
  lcRpExp = STRTRAN(lcRpExp,'CUSTOMER.TYPE','""')
  *B608064,3 TMI 05/13/2007 [End  ] Comment this code replace it with another way that is do not rely on the customer type in filtering
ENDIF
*B608064,1 NNA (End)

SELECT ORDCANLN

*B122926,1 NNA 05/26/2004 (Begin) change the lcRpExp to get style Major from the new opened file
lcOldExp = lcRpExp
lcRpExp = STRTRAN(lcRpExp,'STYLE.','&lcStyFil..')
*B122926,1 NNA (End)

SCAN FOR &lcRpExp .AND. !EOF('ORDHDR')
  SCATTER MEMVAR
  *B803652,1 (Begin) Remark the following lines as these fields are updated.
  *m.STYLE   = ORDLINE.STYLE
  *m.ACCOUNT = ORDLINE.ACCOUNT
  *m.STORE   = ORDLINE.STORE
  *m.Dyelot  = ORDLINE.Dyelot
  *B803652,1 (End)
  INSERT INTO (lcMainF) FROM MEMVAR
ENDSCAN

*B122926,1 NNA 05/26/2004 (Begin) Return to the original expression to continue collecting data
lcRpExp = lcOldExp
*B122926,1 NNA (End)

SELECT ORDCANLN
SET RELATION TO

*B803652,1 (Begin) Get missing lines.
IF lcStatus = "X" OR lcStatus = "L"
  =lfGetMisLn()
ENDIF
*B803652,1 (End)

SELECT (lcMainF)
SET RELATION TO IIF(EMPTY(STORE),'M','S')+ACCOUNT+STORE INTO CUSTOMER ADDITIVE
SET RELATION TO CORDTYPE+ORDER INTO ORDHDR ADDITIVE
*B604771,1 AME [Start] Fix bug sizes headres don't print
SET RELATION TO STYLE INTO STYLE ADDITIVE
*B604771,1 AME [End]

*!*************************************************************
*! Name      : lfBuildCT
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/27/98
*! Purpose   : To build the temporary file in case of Cut ticket
*!             cancellation report.
*!*************************************************************
*! Called from : SOCAN.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfBuildCT()
*!*************************************************************
FUNCTION lfBuildCT

*B602541,1 AMM Set relation
SELECT CUTTKTH
SET RELATION TO CUTTKT INTO CUTTKTL ADDITIVE
*B602541,1 AMM end

SELECT CUTTKTL
SET RELATION TO STYLE INTO STYLE ADDITIVE

SELECT ORDCANLN
SET RELATION TO ORDER INTO CUTTKTH ADDITIVE
*B602541,1 AMM start
SET SKIP TO CUTTKTH,CUTTKTL,STYLE
*B602541,1 AMM end
*-- Create the temporary file from the ORDCANLN table and add some necessary fields to it.
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
*B604019,1 (Begin) Decrease the array by 2 elements.
*DIMENSION laFileStru[lnFileStru+3,4]
DIMENSION laFileStru[lnFileStru+1,4]
*B604019,1 (End)
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CWARECODE'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0
*B604019,1 (Begin) Remark the following lines as STYLE AND DYELOT fields have been permenantly 
*B604019,1         added to ORDCANLN.
*lnFileStru = lnFileStru+1
*laFileStru[lnFileStru,1] = 'STYLE'
*laFileStru[lnFileStru,2] = 'C'
*laFileStru[lnFileStru,3] = 19
*laFileStru[lnFileStru,4] = 0
*lnFileStru = lnFileStru+1
*laFileStru[lnFileStru,1] = 'DYELOT'
*laFileStru[lnFileStru,2] = 'C'
*laFileStru[lnFileStru,3] = 10
*laFileStru[lnFileStru,4] = 0
*B604019,1 (End)
CREATE TABLE (gcWorkDir+lcMainF) FROM ARRAY laFileStru
INDEX ON CORDTYPE+ORDER+CWARECODE+STYLE TAG (lcMainF) 

*-- Fill the temporary file
SELECT ORDCANLN
SCAN FOR &lcRpExp .AND. !EOF('CUTTKTH')
  *B602541,1 AMM No need for these lines, there is already relation and skip to is active
  *SELECT CUTTKTL
  *=SEEK(ORDCANLN.ORDER)
  *LOCATE REST WHILE CUTTKT=ORDCANLN.Order  FOR LINENO=ORDCANLN.LINENO
  *IF FOUND() .AND. !EOF('STYLE')
  IF CUTTKTL.LINENO=ORDCANLN.LINENO
  *B602541,1 AMM end
    m.STYLE      = CUTTKTL.STYLE
    m.cWareCode  = CUTTKTL.cWareCode
    m.Dyelot     = CUTTKTL.Dyelot
    *B602541,1 AMM No need
    *SELECT ORDCANLN
    *B602541,1 AMM end
    SCATTER MEMVAR 
    INSERT INTO (lcMainF) FROM MEMVAR
  ENDIF
ENDSCAN

SELECT CUTTKTL
SET RELATION TO 

SELECT ORDCANLN
SET RELATION TO

SELECT CUTTKTH
SET RELATION TO

SELECT (lcMainF)
SET RELATION TO ORDER INTO CUTTKTH ADDITIVE

*!*************************************************************
*! Name      : lfBuildPT
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/27/98
*! Purpose   : To build the temporary file in case of Purchase order
*!             cancellation report.
*!*************************************************************
*! Called from : SOCAN.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfBuildPT()
*!*************************************************************
FUNCTION lfBuildPT

*B602541,1 AMM Set relation
SELECT POSHDR
SET RELATION TO CSTYTYPE+PO INTO POSLN ADDITIVE
*B602541,1 AMM end

SELECT POSLN
SET RELATION TO STYLE INTO STYLE ADDITIVE

SELECT ORDCANLN
SET RELATION TO CORDTYPE+ORDER INTO POSHDR ADDITIVE
*B602541,1 AMM start
SET SKIP TO POSHDR,POSLN,STYLE
*B602541,1 AMM end
*-- Create the temporary file from the ORDCANLN table and add some necessary fields to it.
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
*B604019,1 (Begin) Decrease the array by 2 elements.
*DIMENSION laFileStru[lnFileStru+4,4]
DIMENSION laFileStru[lnFileStru+2,4]
*B604019,1 (End)
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'VENDOR'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 8
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CWARECODE'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0
*B604019,1 (Begin) Remark the following lines as STYLE AND DYELOT fields have been permenantly 
*B604019,1         added to ORDCANLN.
*lnFileStru = lnFileStru+1
*laFileStru[lnFileStru,1] = 'STYLE'
*laFileStru[lnFileStru,2] = 'C'
*laFileStru[lnFileStru,3] = 19
*laFileStru[lnFileStru,4] = 0
*lnFileStru = lnFileStru+1
*laFileStru[lnFileStru,1] = 'DYELOT'
*laFileStru[lnFileStru,2] = 'C'
*laFileStru[lnFileStru,3] = 10
*laFileStru[lnFileStru,4] = 0
*B604019,1 (End)

CREATE TABLE (gcWorkDir+lcMainF) FROM ARRAY laFileStru
INDEX ON CORDTYPE+VENDOR+ORDER+CWARECODE+STYLE TAG (lcMainF) 

*-- Fill the temporary file
SELECT ORDCANLN
SCAN  FOR &lcRpExp .AND. !EOF('POSHDR')
  *B602541,1 AMM No need for these lines, there is already relation and skip to is active
  *SELECT POSLN
  *=SEEK(ORDCANLN.CORDTYPE+ORDCANLN.ORDER)
  *LOCATE REST WHILE cStyType+PO=ORDCANLN.cOrdType+ORDCANLN.Order  FOR LINENO=ORDCANLN.LINENO
  *IF FOUND() .AND. !EOF('STYLE')
  IF POSLN.LINENO=ORDCANLN.LINENO
  *B602541,1 AMM end
    m.STYLE       = POSLN.STYLE
    m.VENDOR      = POSLN.VENDOR
    m.cWareCode   = POSLN.cWareCode
    m.Dyelot      = POSLN.Dyelot
    *B602541,1 AMM No need
    *SELECT ORDCANLN
    *B602541,1 AMM end
    SCATTER MEMVAR 
    INSERT INTO (lcMainF) FROM MEMVAR
  ENDIF
ENDSCAN

SELECT POSLN
SET RELATION TO
SELECT ORDCANLN
SET RELATION TO
SELECT POSHDR
SET RELATION TO

SELECT (lcMainF)
SET RELATION TO CORDTYPE+ORDER INTO POSHDR ADDITIVE
SET RELATION TO vendor INTO Apvendor ADDITIVE
*B604771,1 AME [Start] Fix bug sizes headres don't print
SET RELATION TO STYLE INTO STYLE ADDITIVE
*B604771,1 AME [End]


*!*************************************************************
*! Name      : lfStatus
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/27/98
*! Purpose   : To Fill the arrays of status due to the type of report 
*!             (SO, PO or CT)
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

DO CASE
  CASE GCACT_APPL='SO'
    DIMENSION laStatT[6,1]
    DIMENSION laStatV[6,1]
    STORE SPACE(0) TO laStatT,laStatV
    
    laStatT[1] = 'All'
    laStatV[1] = 'L'
    laStatT[2] = 'Open'
    laStatV[2] = 'O'
    laStatT[3] = 'Hold'
    laStatV[3] = 'H'
    *B603721,1 ABD Fix spelling in status selection box there is "bit" that 
    *B603721,1 ABD should To be "bid".  [Begin]
    *laStatT[4] = 'Bit'    
    laStatT[4] = 'Bid'
    *B603721,1 ABD [End]
    laStatV[4] = 'B'
    laStatT[5] = 'Complete'
    laStatV[5] = 'C'
    laStatT[6] = 'Cancel'
    laStatV[6] = 'X'
    
  CASE GCACT_APPL='PO'
    DIMENSION laStatT[6,1]
    DIMENSION laStatV[6,1]
    STORE SPACE(0) TO laStatT,laStatV
    laStatT[1] = 'All'
    laStatV[1] = 'L'
    laStatT[2] = 'Open'
    laStatV[2] = 'O'
    laStatT[3] = 'Hold'
    laStatV[3] = 'H'
    laStatT[4] = 'Closed'
    laStatV[4] = 'S'
    laStatT[5] = 'Complete'
    laStatV[5] = 'C'
    laStatT[6] = 'Cancel'
    laStatV[6] = 'X'

  CASE GCACT_APPL='MF'
    DIMENSION laStatT[7,1]
    DIMENSION laStatV[7,1]
    STORE SPACE(0) TO laStatT,laStatV
    laStatT[1] = 'All'
    laStatV[1] = 'L'
    laStatT[2] = 'Open'
    laStatV[2] = 'O'
    laStatT[3] = 'Hold'
    laStatV[3] = 'H'
    laStatT[4] = 'Closed'
    laStatV[4] = 'S'
    laStatT[5] = 'Complete'
    laStatV[5] = 'C'
    laStatT[6] = 'Actual'
    laStatV[6] = 'A'
    laStatT[7] = 'Cancel'
    laStatV[7] = 'X'   
ENDCASE
*-- End of lfStatus.

*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/27/98
*! Purpose   : To set relation on or off when running the in range function 
*!             in the option grid.
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvTrans()
*!*************************************************************
FUNCTION lfsrvTrans
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
ENDCASE
*-- End of lfsrvTrans.

*!*************************************************************
*! Name      : lfGetTit
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/27/98
*! Purpose   : Get title to be displayed for the C/T
*!*************************************************************
*! Called from : The option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : lcTit
*!*************************************************************
*! Example     : = lfGetTit()
*!*************************************************************
FUNCTION lfGetTit
PRIVATE lcTit
lcTit   = ALLTRIM(gfGetMemvar('M_PRDLNLBL',gcAct_Comp))  
lcTit  = IIF(RIGHT(lcTit,1) ='#', lcTit,lcTit+'#')
RETURN lcTit
*-- End of lfGetTit.

*!*************************************************************
*! Name      : lfvVend
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/23/1998
*! Purpose   : Valid function of the Vendor field.
*!*************************************************************
*! Called from : Option grid (Vendor in list function)
*!*************************************************************
*! Calls       : gfApVnBrow().
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVend()
*!*************************************************************
FUNCTION lfvVend
PRIVATE lcVar, lcObj
lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(  ( EVALUATE(SYS(18)) )  )   && Varible to hold the current field value
SELECT APVENDOR
SET ORDER TO TAG VenCode 
*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'APVENDOR'))
  =gfApVnBrow(@lcObj)
  IF !EMPTY(lcObj)
    &lcVar = lcObj      && Update the field
  ELSE
    &lcVar = laOldVal
  ENDIF
ENDIF
*-- end of lfvVend.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/27/1998
*! Purpose   : To get the old value of the field
*!*************************************************************
*! Called from : Option Grid (Vendor in list function)
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfsrOrder
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrOrder()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrOrder
PARAMETERS lcParm

SELECT ORDHDR
SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
IF lcParm = "S"
  *-- SSE (sameh) commented out the three lines below 'cause it's added 
  *-- in the Browse Expression of this record
  ** to get an empty ordhdr browse
  *IF !EMPTY(lcStatus)
  *  SET FILTER TO STATUS $ lcStatus  
  *ENDIF
  GO TOP
ELSE
  *IF !EMPTY(lcStatus)
  *  SET FILTER TO
  *ENDIF
ENDIF  
*-- End of lfsrOrder.

*!**************************************************************************
*! Name      : lfvPrint
*! Developer : Sameh (SSE)
*! Date      : 08/29/99
*! Purpose   : Valid function of Print (Both,Orders,Contract)
*!**************************************************************************
*! Example   : =lfvPrint()
*!**************************************************************************
*B802462,1
FUNCTION lfvPrint

lcRpPrint = IIF(lcRpType = 'L' , "'O','C'" , "'" + lcRpType + "'")
*-- End of lfvPrint.

*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Sameh (SSE)
*! Date      : 07/28/99
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG 
*!**************************************************************************
*! Example   : =lfvEdiOrd()
*!**************************************************************************
*E500271,4
FUNCTION lfvEdiOrd

lcRpEdiFlt = ""
IF lcRpEdiPrn <> "B"
  lcRpEdiFlt = "AND (" + IIF(lcRpEdiPrn = "O" , "!OrdHdr.lEdiOrder" ,;
                             "OrdHdr.lEdiOrder") + ")"
ENDIF
llClearOrd = .T.
*-- end of lfvEdiOrd.

*!************************************************************************************
*! Name      : lfGetMisLn                                                            *
*! Developer : Adel Mohammed El Gazzar (ADEL)                                        *
*! Date      : 09/27/2000                                                            *
*! Purpose   : To get the cancelled order line(s) that hasn't been sent to ORDCANLN  *
*!************************************************************************************
*! Example   : =lfGetMisLn()                                                         *
*!************************************************************************************
*B803652,1
FUNCTION lfGetMisLn

SET ORDER TO (lcNewIndex) IN (lcMainF)
*--Make a new filter
lcNewFlter = lcRpExp
*-- Clear (CANCELLED,ORDER,CCANCRESON) From filter to apply it on OrdHdr.
lcNewFlter = STRTRAN(lcNewFlter,'ORDCANLN.','ORDHDR.')
*--If the user Status = 'All' scan Canceleld order(s) only.
IF AT('ORDHDR.Status',lcNewFlter) > 0
  lcNewFlter = STRTRAN(lcNewFlter,"ORDHDR.Status = 'L'","ORDHDR.Status = 'X'")
ELSE
  lcNewFlter = '(' +lcNewFlter+ ") .AND. (ORDHDR.Status = 'X')"
ENDIF  
SELECT ORDLINE
SET RELATION TO CORDTYPE+ORDER INTO ORDHDR ADDITIVE
SET RELATION TO IIF(EMPTY(STORE),'M','S')+ACCOUNT+STORE INTO CUSTOMER ADDITIVE
*--Now ORDLINE has relations to : OrdHdr,Customer,Style
SCAN FOR &lcNewFlter AND !EOF('ORDHDR')
  IF !SEEK(cordtype+order+STR(lineno,6),lcMainF)
    SCATTER MEMVAR 
    *B604592,1 AME [START] get cancelation reson from ordhdr
    m.cCancReson = ORDHDR.cCancReson
    *B604592,1 AME [END]
    *B604771,1 AME [START] get cancelation date from ordhdr
    m.Cancelled = ORDHDR.Cancelled
    *B604771,1 AME [END]
    INSERT INTO (lcMainF) FROM MEMVAR
  ENDIF
ENDSCAN
SET ORDER TO (lcMainF) IN (lcMainF)
