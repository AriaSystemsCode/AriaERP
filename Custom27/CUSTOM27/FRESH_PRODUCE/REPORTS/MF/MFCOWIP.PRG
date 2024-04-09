****************************************************************************
*: Program file      : MFCOWIP.PRG (C#101920)
*: Program desc.     : Contractor Open Work In Progress Report
*: System            : Aria Apparel System (A27).
*: Module            : Manufacturing (MF)
*: Developer         : Khalid Mohi El-Din Mohamed KHM
*: Date              : 07/26/2000
*C200137,1 TAK 10/10/2000
*:**************************************************************************

*-- Initialize the variables that will hold the filters for each file
STORE ".T." TO lcCTFltr,lcStyFltr,lcMfgFltr

*-- Initialize the variables that will hold the temporary files in the
*-- option grid (CutTktH,Style,Vendors,Fabric)
STORE '' TO lcCutTFile,lcStyFile,lcVenFile,lcFabFile

*-- Initialize the variables that will hold the selected values of OG
STORE '' TO lcStatus,lcDivision,lcSeasons,lcStyGroup,lDLwSDate,lDHgSDate,;
            lDLwCDate,lDHgCDate

*-- Function to create the filters.
=lfCreatFlt()

*-- Function to collect the data and print the report
=lfColData()


*!*************************************************************
*! Name      : lfColData
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 07/26/2000 
*! Purpose   : To colect the data and print the report
*!*************************************************************
*! Example     : = lfColData()
*!*************************************************************
FUNCTION lfColData

*-- Temporary file that will hold the data that will be printed
lcTmpCTFil = gfTempName()
lcTmpIndex = gfTempName()
CREATE CURSOR (lcTmpCTFil);
  (cCutTkt C(6), cStyle C(19), cStyGroup C(6), cPattern C(10), dIssDate D,;
   dCompDate D, nOrgQty1 N(6),nOrgQty2 N(6),nOrgQty3 N(6),nOrgQty4 N(6),;
   nOrgQty5 N(6),nOrgQty6 N(6),nOrgQty7 N(6),nOrgQty8 N(6),nOpnQty1 N(6),;
   nOpnQty2 N(6),nOpnQty3 N(6),nOpnQty4 N(6),nOpnQty5 N(6),nOpnQty6 N(6),;
   nOpnQty7 N(6),nOpnQty8 N(6),nOpnTotQty N(8),cContCode C(8),cContName C(30))
INDEX ON cContCode+cCutTkt TAG (lcTmpCTFil) OF (lcTmpCTFil)

*-- Creating the index according to the sort option in the O.G.
DO CASE
  CASE lcRpSortBy = 'C'    && Sorting by Cut Ticket
    INDEX ON cContCode+cCutTkt+cStyle TAG(lcTmpIndex) OF (lcTmpCTFile)

  CASE lcRpSortBy = 'S'    && Sorting by Style
    INDEX ON cContCode+cStyle+cCutTkt TAG(lcTmpIndex) OF (lcTmpCTFile)

  CASE lcRpSortBy = 'G'    && Sorting by Style Group
    INDEX ON cContCode+cStyGroup+cCutTkt+cStyle TAG(lcTmpIndex) OF (lcTmpCTFile)

  CASE lcRpSortBy = 'I'    && Sorting by Issue Date
    INDEX ON cContCode+DTOS(dIssDate)+cCutTkt+cStyle TAG(lcTmpIndex) OF (lcTmpCTFile)

  CASE lcRpSortBy = 'L'    && Sorting by Complete Date
    INDEX ON cContCode+DTOS(dCompDate)+cCutTkt+cStyle TAG(lcTmpIndex) OF (lcTmpCTFile)

  CASE lcRpSortBy = 'P'    && Sorting by Pattern #
    INDEX ON cContCode+cPattern+cCutTkt+cStyle TAG(lcTmpIndex) OF (lcTmpCTFile)  
ENDCASE
SET ORDER TO (lcTmpCTFil)

*-- Collecting the data according to the selection criteria.
DO CASE

  *-- Case of selecting Cut Tickets
  CASE !EMPTY(lcCutTFile) AND USED(lcCutTFile) AND !EOF(lcCutTFile)
    lcCTFltr = lcCTFltr + IIF(!EMPTY(lcStyFile) AND USED(lcStyFile) AND;
                          !EOF(lcStyFile),' AND SEEK(Style,lcStyFile)','')
    SET ORDER TO CutTktH IN CutTktH
    SELECT (lcCutTFile)
    SCAN
      WAIT WINDOW 'Selecting Cut Ticket #: '+CutTkt NOWAIT
      SELECT CutTktH
      IF SEEK(&lcCutTFile..CutTkt) AND &lcCTFltr AND;
         SEEK(Style,'Style') AND &lcStyFltr 
         IF lcAssgCont = 'N' AND Status = 'H'
           SELECT (lcCutTFile)
           LOOP
         ENDIF
         IF Status = 'H'
           INSERT INTO (lcTmpCTFil);
          (cCutTkt, cStyle, cStyGroup, cPattern, dIssDate, dCompDate,cContCode);
          VALUES;
          (CutTktH.CutTkt,CutTktH.Style,Style.cStyGroup,CutTktH.Pattern,;
           CutTktH.Entered,CutTktH.Complete,SPACE(8))
           
           =lfUpdCTQty(CutTktH.CutTkt)
         ELSE
           =lfUpdContr(CutTktH.CutTkt)
         ENDIF
      ENDIF
    ENDSCAN

  *-- Case of selecting styles 
  CASE !EMPTY(lcStyFile)  AND USED(lcStyFile)  AND !EOF(lcStyFile)
    SET ORDER TO Cuttkths IN CutTktH
    SELECT(lcStyFile)
    SCAN      
      SELECT CutTktH      
      IF SEEK(&lcStyFile..cStyMajor)        
        SCAN REST WHILE Style+CutTkt = &lcStyFile..cStyMajor
          WAIT WINDOW 'Selecting Cut Ticket #: '+CutTkt NOWAIT
          IF &lcCTFltr AND SEEK(Style,'Style') AND &lcStyFltr 
            IF lcAssgCont = 'N' AND Status = 'H'
              SELECT CutTktH
              LOOP
            ENDIF
            IF Status = 'H'
              INSERT INTO (lcTmpCTFil);
             (cCutTkt, cStyle, cStyGroup, cPattern, dIssDate, dCompDate,cContCode);
             VALUES;
             (CutTktH.CutTkt,CutTktH.Style,Style.cStyGroup,CutTktH.Pattern,;
             CutTktH.Entered,CutTktH.Complete,SPACE(8))
           
             =lfUpdCTQty(CutTktH.CutTkt)
            ELSE
              =lfUpdContr(CutTktH.CutTkt)
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF    
    ENDSCAN
    SET ORDER TO CutTktH IN CutTktH

  OTHERWISE
    lcCTFltr = lcCTFltr + IIF(!EMPTY(lcStyFile) AND USED(lcStyFile) AND;
                          !EOF(lcStyFile),' AND SEEK(Style,lcStyFile)','')

    SELECT CutTktH
    SCAN FOR &lcCTFltr
      WAIT WINDOW 'Selecting Cut Ticket #: '+CutTkt NOWAIT
      IF SEEK(Style,'Style') AND &lcStyFltr
        IF lcAssgCont = 'N' AND Status = 'H'
          SELECT CutTktH
          LOOP
        ENDIF
        IF Status = 'H'
          INSERT INTO (lcTmpCTFil);
          (cCutTkt, cStyle, cStyGroup, cPattern, dIssDate, dCompDate,cContCode);
          VALUES;
          (CutTktH.CutTkt,CutTktH.Style,Style.cStyGroup,CutTktH.Pattern,;
           CutTktH.Entered,CutTktH.Complete,SPACE(8))
           
          =lfUpdCTQty(CutTktH.CutTkt)
        ELSE
          =lfUpdContr(CutTktH.CutTkt)
        ENDIF
      ENDIF
    ENDSCAN  
ENDCASE
WAIT CLEAR

SELECT (lcTmpCTFil)
SET ORDER TO (lcTmpIndex)
GOTO TOP

IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*-- if user want to print new page per Contractor
IF lcRpPagBrk = "Y"
  IF !USED(lcOGTmpForm)
    lcSvErrHan = ON('ERROR')
    ON ERROR
    USE &gcWorkDir.&lcOGTmpForm..FRX IN 0 ORDER 0 EXCLUSIVE
    ON ERROR &lcSvErrHan
  ENDIF

  *-- Replace pagebreak with .T. to print new page per contractor group.
  SELECT (lcOGTmpForm)
  LOCATE FOR OBJTYPE = 9 AND OBJCODE = 3 AND 'CCONTCODE' $ UPPER(ALLTRIM(EXPR))
  REPLACE PAGEBREAK WITH .T.
  **C200137,1
  REPLACE ResetPage WITH .T.
  USE IN (lcOGTmpForm)  && Close file.
ENDIF

SELECT (lcTmpCTFil)
**C200137,1
GO BOTT
lcCntCod = cContCode
GO TOP
=gfDispRe(lcRpFormN)
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfUpdContr
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 07/26/2000 
*! Purpose   : To get all the contractors that are assigned to CT
*!*************************************************************
*! Parameters: lcCutTkt : Cut Ticket #
*!*************************************************************
*! Example     : = lfUpdContr()
*!*************************************************************
FUNCTION lfUpdContr
PARAMETERS lcCutTkt
PRIVATE lnAlias,lcVendors

lnAlias    = SELECT()
lcContName = ''
IF SEEK('M'+lcCutTkt,'MfgOprHd')
  SELECT MfgOprHd
  SCAN REST WHILE cIMTyp+cTktNo+cOprCode = 'M'+lcCutTkt;
            FOR IIF(lcAssgCont='N',!EMPTY(cContCode) AND;
                              &lcMfgFltr,EMPTY(cContCode) OR &lcMfgFltr)
    IF !SEEK(cContCode+lcCutTkt,lcTmpCTFil)
      IF !EMPTY(cContCode)
        IF EMPTY(cContName)
           lcContName =IIF(SEEK(cContCode,'ApVendor'),ApVendor.cVenComp,'')           
        ELSE
          lcContName = MfgOprHd.cContName
        ENDIF
      ENDIF
      INSERT INTO (lcTmpCTFil);
      (cCutTkt, cStyle, cStyGroup, cPattern, dIssDate, dCompDate,cContCode,;
       cContName);
      VALUES;
      (lcCutTkt,CutTktH.Style,Style.cStyGroup,CutTktH.Pattern,CutTktH.Entered,;
       CutTktH.Complete,MfgOprHd.cContCode,lcContName)
           
      =lfUpdCTQty(lcCutTkt)      
    ENDIF
  ENDSCAN
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfUpdCTQty
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 07/26/2000 
*! Purpose   : To calculate the original and open qty of Cut Ticket
*!*************************************************************
*! Parameters: lcCutTkt : Cut Ticket #
*!*************************************************************
*! Example     : = lfUpdCTQty()
*!*************************************************************
FUNCTION lfUpdCTQty
PARAMETERS lcCutTktNo
PRIVATE lnAlias

lnAlias = SELECT()

IF SEEK(lcCutTktNo,'CutTktL')
  SELECT CutTktL
  SCAN REST WHILE CutTkt+Style+Dyelot+TranCd = lcCutTktNo
    SELECT (lcTmpCTFil)
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      IF CutTktL.TranCd = '1'              
        REPLACE nOrgQty&lcCount WITH nOrgQty&lcCount + CutTktL.Qty&lcCount,;
                nOpnQty&lcCount WITH nOpnQty&lcCount + CutTktL.Qty&lcCount      
      ELSE
        REPLACE nOpnQty&lcCount WITH MAX(nOpnQty&lcCount - CutTktL.Qty&lcCount,0)
      ENDIF
    ENDFOR
    REPLACE nOpnTotQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+;
                            nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8
  ENDSCAN
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfCreatFlt
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 07/26/2000 
*! Purpose   : To create the filter of the O.G.
*!*************************************************************
*! Example     : = lfCreatFlt()
*!*************************************************************
FUNCTION lfCreatFlt

*-- Generating the CutTktH filter
*-- 1) Get the status
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.STATUS'),1)
IF lnPosition > 0
  lcStatus = laOGFxFlt[lnPosition,6] 
ENDIF
lcCTFltr = lcCTFltr + IIF(!EMPTY(lcStatus),' AND Status $ lcStatus','')

*-- Check if there are any selected cut tickets.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.CUTTKT'),1)
IF lnPosition > 0
  lcCutTFile = laOGFxFlt[lnPosition,6] 
  IF !EMPTY(lcCutTFile) AND USED(lcCutTFile)
    SELECT (lcCutTFile)
    GOTO TOP
  ENDIF
ENDIF

*-- 2) Check if there are any selected styles
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYMAJOR'),1)
IF lnPosition > 0
  lcStyFile = laOGFxFlt[lnPosition,6] 
  IF !EMPTY(lcStyFile) AND USED(lcStyFile)
    SELECT (lcStyFile)
    GOTO TOP
  ENDIF
ENDIF

*-- 3) To get the selected divisions
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.CDIVISION'),1)
IF lnPosition > 0
  lcDivision = laOGFxFlt[lnPosition,6] 
ENDIF
lcCTFltr = lcCTFltr + IIF(!EMPTY(lcdivision),' AND cDivision $ lcdivision','')

*-- 4) To get the selected seasons
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.SEASON'),1)
IF lnPosition > 0
  lcSeasons = laOGFxFlt[lnPosition,6] 
ENDIF
lcCTFltr = lcCTFltr + IIF(!EMPTY(lcSeasons),' AND Season $ lcSeasons','')

*-- 5) To get the Entered Date
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.ENTERED'),1)
IF lnPosition > 0
   lDLwSDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnPosition,6],1,10)))
   lDHgSDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnPosition,6],12,20)))
   lcCTFltr  = lcCTFltr + IIF(!EMPTY(lDHgSDate),;
                          ' AND BETWEEN(Entered,lDLwSDate,lDHgSDate)','')
ENDIF

*-- 6) To get the Complete Date
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.COMPLETE'),1)
IF lnPosition > 0
   lDLwCDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnPosition,6],1,10)))
   lDHgCDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnPosition,6],12,20)))
   lcCTFltr  = lcCTFltr + IIF(!EMPTY(lDHgCDate),;
                          ' AND BETWEEN(Complete,lDLwCDate,lDHgCDate)','')
ENDIF

*-- Getting the style filter
*-- 1) To get the selected style's groups
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYGROUP'),1)
IF lnPosition > 0
  lcStyGroup = laOGFxFlt[lnPosition,6] 
ENDIF
lcStyFltr = lcStyFltr+IIF(!EMPTY(lcStyGroup),;
                      ' AND Style.cStyGroup $ lcStyGroup','')

*-- 2) Get the primary fabric
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'FABRIC.FABRIC'),1)
IF lnPosition > 0
  lcFabFile = laOGFxFlt[lnPosition,6] 
  IF !EMPTY(lcFabFile) AND USED(lcFabFile)
    SELECT (lcFabFile)
    GOTO TOP
    IF !EOF()
     lcStyFltr = lcStyFltr + ' AND SEEK(Style.Fabric,lcFabFile)'
    ENDIF
  ENDIF
ENDIF

*-- Get the Contractor filter
*-- 1)
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'APVENDOR.CVENDCODE'),1)
IF lnPosition > 0
  lcVenFile = laOGFxFlt[lnPosition,6] 
  IF !EMPTY(lcVenFile) AND USED(lcVenFile)
    SELECT (lcVenFile)
    GOTO TOP
    IF !EOF()
      lcMfgFltr = lcMfgFltr + ' AND SEEK(MfgOprHd.cContCode,lcVenFile)'
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 07/26/2000 
*! Purpose   : The validation function of the Option Grid.
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

R_WIDTH = 'W'
IF lnOGSeting = 1
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.STATUS'),1)
  IF lnPosition > 0
    laOGFxFlt[lnPosition,6] = laOGFxFlt[lnPosition,6] + 'O|H'
  ENDIF
  lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.CDIVISION'),1)
  IF lnPosition > 0
    laOGFxFlt[lnPosition,6] = laOGFxFlt[lnPosition,6] + 'FPSW  '
  ENDIF

ENDIF

*!*************************************************************
*! Name      : lfSetVen
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 07/26/2000 
*! Purpose   : To set the Vendor file in the browse screen
*!*************************************************************
*! Example     : = lfSetVen()
*!*************************************************************
FUNCTION lfSetVen
PARAMETERS OpGrdParm

IF OpGrdParm = 'S'
   GO TOP IN ApVendor
ENDIF

*!*************************************************************
*! Name      : lfSetCT
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 07/26/2000 
*! Purpose   : To set the CutTktH file in the browse screen
*!*************************************************************
*! Example     : = lfSetCT()
*!*************************************************************
FUNCTION lfSetCT
PARAMETERS OpGrdParm

IF OpGrdParm = 'S'
   GO TOP IN CutTktH
ENDIF

*!*************************************************************
*! Name      : lfSetSty
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 07/26/2000 
*! Purpose   : To set the Style file in the browse screen
*!*************************************************************
*! Example     : = lfSetSty()
*!*************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm

IF OpGrdParm = 'S'
   GO TOP IN Style
ENDIF

*!*************************************************************
*! Name      : lfSetFab
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 07/26/2000 
*! Purpose   : To set the Fabric file in the browse screen
*!*************************************************************
*! Example     : = lfSetFab()
*!*************************************************************
FUNCTION lfSetFab
PARAMETERS OpGrdParm

IF OpGrdParm = 'S'
   USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
   SELECT FABRIC
   SET ORDER TO TAG cFabric
   GO TOP IN Fabric
ELSE
  USE IN FABRIC_X
ENDIF

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 07/26/2000 
*! Purpose   : To calculate the onhand qty.
*!*************************************************************
*! Example     : = lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp

PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)


*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 07/26/2000 
*! Purpose   : To display a notpad to write the optional messages
*!*************************************************************
*! Example     : = lfvOptMsg()
*!*************************************************************
FUNCTION lfvOptMsg

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 3rd. line Variable
laOptMsg[1,2] = 75                && Line length

=gfOptMsg('laOptMsg')


