*:***************************************************************************
*: Program file  : ICSTDTAC     *** CUSTOMIZED FOR TIMANAD  ***
*: Program desc. : Print STORE DETAILED ACTIVITY REPORT
*: For Report    : ICSTDTAC.FRX
*: System        : Aria Advantage Series VER. 2.7 
*: Module        : IC 
*: Developer     : AHMED MOHAMMED MOHAMMED
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : 
*:***************************************************************************

DIMENSION laDisc[3,2],laDamLoc[1]
*-- laDisc    Array to get the discount code related fields
*-- laDamLoc  Array to hold damage locacations
STORE SPACE(0) TO lcBckOfce,lcMainF,laDamLoc
*-- lcBckOfce  Variable holds back office ID
*-- lcMainF    Variable holds the temporary name of the main report cursor

STORE .F. TO llEndGroup
STORE {} TO ldLODT, ldHiDT, ldStart, ldEnd
*-- ldLODT  Variable holds the start date of the last week
*-- ldHiDT  Variable holds the end date of the last week
*-- ldStart Variable holds the start date of the discount code applied to styles
*-- ldEnd   Variable holds the end date of the discount code applied to styles

lnDisc  = 0
laDisc[1,1] = 'DISCPCNT'
laDisc[1,2] = "lnDisc"
laDisc[2,1] = 'START'
laDisc[2,2] = "ldStart"
laDisc[3,1] = 'DENDATE'
laDisc[3,2] = "ldEnd"
IF !('IC' $ gcComp_Mdl) 

  *-- Message
  *-- IC Module is not installed for company AH. You have to 
  *-- install it through the Company Information program.
  =gfModalGen('INM00151B00000','DIALOG','IC Module'+'|'+gcAct_Comp)
  RETURN
ENDIF
IF !('AR' $ gcComp_Mdl)
  *-- Message
  *-- RM Module is not installed for company AH. You have to 
  *-- install it through the Company Information program.
  =gfModalGen('INM00151B00000','DIALOG','AR Module'+'|'+gcAct_Comp)
  RETURN
ENDIF
*-- Calculate start and end date of the last week
lnC = 0
DO WHILE .T.
  IF CDOW(gdSysDate - lnC ) = 'Friday'
    ldLoDT = gdSysDate - lnc - 6
    ldHiDt = gdSysDate - lnc
    EXIT
  ENDIF
  lnC = lnC +1
ENDDO
*-- Get the length of the style major
lcMaj      = gfItemMask('PM')             && Get the major of the style
lnMajSz    = LEN(lcMaj)                   && Length of the major
*-- Get the name of the temporary files holds styles and warehouses selected by user
lcWarNam = ''
lcSDNam  = ''
IF ASCAN(laOGFXFlt,'STYDYE.CWARECODE') # 0
  *-- Get the name of the temporary files holds warehouses 
  lnPos = ASUBSCRIPT(laOGFXFlt, ASCAN(laOGFxFlt,'STYDYE.CWARECODE'),1)
  lcWarName = laOGFXFlt[lnPos,6]
ENDIF

IF ASCAN(laOGFXFlt,'STYDYE.STYLE') # 0
  *-- Get the name of the temporary files holds styles 
  lnPos = ASUBSCRIPT(laOGFXFlt, ASCAN(laOGFxFlt,'STYDYE.STYLE'),1)
  lcSDName = laOGFXFlt[lnPos,6]
ENDIF
*-- If the two files has no data, empty them not to use.
IF !EMPTY(lcWarName) 
  IF !USED(lcWarName) 
    lcWarName = SPACE(0)
  ELSE
    GO TOP IN (lcWarName)
    IF EOF(lcWarName)
      lcWarName = SPACE(0)
    ENDIF
  ENDIF
ENDIF
IF !EMPTY(lcSDName) 
  IF !USED(lcSDName) 
    lcSDName = SPACE(0)
  ELSE
    GO TOP IN (lcSDName)
    IF EOF(lcSDName)
      lcSDName = SPACE(0)
    ENDIF
  ENDIF
ENDIF

SELECT SYCSITES
LOCATE FOR cCmSiteTyp = 'B'
IF FOUND()
  lcBckOfce = SYCSITES.cSiteID
ELSE
  WAIT WINDOW 'There is no back office for this company, can not proceed.'
  RETURN
ENDIF
lcMainF = gfTempName()
*-- Create the main report cursor
CREATE CURSOR (lcMainF) FROM ARRAY laFlds
ZAP
lcSetPath = SET('FULLPATH')
SET FULLPATH ON
LcCDXName = STRTRAN(DBF() , '.TMP' , '.CDX')
INDEX ON cWareCOde+STYLE TAG (lcMainF) OF (LcCDXName)
SET FULLPATH &lcSetPath

*-- Get the damage locations saved in the file
IF FILE(gcDataDir+'fDamLoc.mem')
  RESTORE FROM gcDataDir+'fDamLoc.mem' ADDITIVE
ENDIF

SELECT WAREHOUS
*-- Collect the back office warehouses in the array laBkWars
lnc = 0
SCAN FOR cSiteID = lcBckOfce .AND. ASCAN(laDamLoc,alltrim(cWarecode)) = 0
  lnC = lnC + 1
  DIMENSION laBkWars[lnC]
  laBkWars[lnC] = WAREHOUS.cWareCode
ENDSCAN
*-- Set necessary relations
IF 'PO' $ gcComp_Mdl
  SELECT POSLN
  SET RELATION TO Posln.style+LEFT(Posln.vendor,6)+ Posln.crsession INTO Styinvjl ADDITIVE
ENDIF
IF 'RM' $ gcComp_Mdl
  SELECT RETLINE
  SET RELATION TO CRMEMO INTO RETHDR ADDITIVE
ENDIF
SELECT STYDYE
IF 'PO' $ gcComp_Mdl
  SET RELATION TO STYLE+'N' INTO POSLN ADDITIVE
ENDIF
SET RELATION TO STYLE INTO INVLINE ADDITIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE
IF 'RM' $ gcComp_Mdl
  SET RELATION TO STYLE INTO RETLINE ADDITIVE
ENDIF
DO CASE
  CASE EMPTY(lcWarName) .AND. EMPTY(lcSDName)
    SELECT STYDYE
    SET ORDER TO STYDYEW IN STYDYE
    SCAN FOR ASCAN(laDamLoc,cWareCode) = 0    
      =lfColData()
    ENDSCAN
  CASE EMPTY(lcWarName) .AND. !EMPTY(lcSDName)
    SET ORDER TO STYDYE IN STYDYE
    SELECT (lcSDName)
    SCAN
      SELECT STYDYE
      =SEEK(&lcSDName..STYLE)
      SCAN REST WHILE STYLE = &lcSDName..STYLE FOR ASCAN(laDamLoc,cWareCode) = 0    
        =lfColData()
      ENDSCAN
    ENDSCAN
    
  CASE !EMPTY(lcWarName) .AND. EMPTY(lcSDName)
    SET ORDER TO STYDYEW IN STYDYE
    SELECT (lcWarName)
    SCAN
      SELECT STYDYE
      =SEEK(&lcWarName..cWareCode)
      SCAN REST WHILE CWARECODE=&lcWarName..cWareCode
        =lfColData()
      ENDSCAN
    ENDSCAN
  CASE !EMPTY(lcWarName) .AND. !EMPTY(lcSDName)
    SET ORDER TO STYDYEW IN STYDYE
    SELECT (lcWarName)
    SCAN
      SELECT (lcSDName)
      SCAN
        SELECT STYDYE
        =SEEK(&lcWarName..cWareCode+&lcSDName..STYLE)
        SCAN REST WHILE CWARECODE+STYLE = &lcWarName..cWareCode+&lcSDName..STYLE
          =lfColData()
        ENDSCAN
      ENDSCAN
    ENDSCAN
ENDCASE

WAIT CLEAR
SELECT STYDYE
SET ORDER TO STYDYEW
SET RELATION TO
IF 'PO' $ gcComp_Mdl
  SELECT POSLN
  SET RELATION TO
ENDIF
IF 'RM' $ gcComp_Mdl
  SELECT RETLINE
  SET RELATION TO
ENDIF
SELECT (lcMainF)
SET RELATION TO cWareCode INTO WAREHOUS
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO cWareCode+STYLE INTO STYDYE ADDITIVE
SET RELATION TO 'S'+STYLE.SCALE INTO SCALE ADDITIVE
GO TOP

=gfDispRe(lcFormName) 


IF USED(lcMainF)
  USE IN (lcMainF)
ENDIF

*!*************************************************************
*! Name      : lfvDumW
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 05/17/99
*! Purpose   : Valid function of the Damage warehouses selection button
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvDumW()
*!*************************************************************
FUNCTION lfvDumW

DIMENSION laSrcLoc[1],laDamLoc[1]

IF FILE(gcDataDir+'fDamLoc.mem')
  RESTORE FROM gcDataDir+'fDamLoc.mem' ADDITIVE
ENDIF
SELECT cWareCode FROM (gcDataDir+'WAREHOUS') INTO ARRAY laSrcLoc

=gfMover(@laSrcLoc,@laDamLoc,"Damage Warehouses...")

SAVE TO gcDataDir+'fDamLoc.mem' ALL LIKE laDamLoc


*!*************************************************************
*! Name      : lfvShClr
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 05/17/99
*! Purpose   : Valid function of the SHOW COLOR setting in the OG
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfOGShowGet()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvShClr()
*!*************************************************************
FUNCTION lfvShClr

IF ASCAN(laOgObjType,'llRpSize') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'llRpSize'),1)
  IF llRpClr
    laOGObjCnt[lnPos] = .T. 
  ELSE
    llRpSize          = .F.
    laOGObjCnt[lnPos] = .F. 
  ENDIF
  = lfOGShowGet('llRpSize')
ENDIF
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 05/17/99
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfTempName()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*-- If show color = 'N' disable show size
IF ASCAN(laOgObjType,'llRpSize') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'llRpSize'),1)
  laOGObjCnt[lnPos] = llRpClr 
  = lfOGShowGet('llRpSize')
ENDIF

*-- Form the structure of the temporary cursor of the report
DIMENSION laFlds[102,4]
STORE 0 TO laFlds
laFlds[1,1] = 'cWareCode'
laFlds[1,2] = 'C'
laFlds[1,3] = 6
laFlds[2,1] = 'Style'
laFlds[2,2] = 'C'
laFlds[2,3] = 19
laFlds[3,1] = 'nSugRetPri'
laFlds[3,2] = 'N'
laFlds[3,3] = 8
laFlds[3,4] = 2
*-- Rec. PO YTD from back office quantity fields
FOR lnC = 1 TO 8
  lcC = STR(lnC,1)
  laFlds[3+lnC,1] = 'nTRBK'+lcC
  laFlds[3+lnC,2] = 'N'
  laFlds[3+lnC,3] = 7
ENDFOR
laFlds[12,1] = "nTTRBK"
laFlds[12,2] = 'N'
laFlds[12,3] = 8
*-- Pcs in YTD (total receipts from inter locations PO of other than back office)
FOR lnC = 1 TO 8
  lcC = STR(lnC,1)
  laFlds[12+lnC,1] = 'nTRSt'+lcC
  laFlds[12+lnC,2] = 'N'
  laFlds[12+lnC,3] = 7
ENDFOR
laFlds[21,1] = "nTTRSt"
laFlds[21,2] = 'N'
laFlds[21,3] = 8
*-- Pcs in Last Week (total receipts from inter locations PO of other than back office)
FOR lnC = 1 TO 8
  lcC = STR(lnC,1)
  laFlds[21+lnC,1] = 'nTRLW'+lcC
  laFlds[21+lnC,2] = 'N'
  laFlds[21+lnC,3] = 7
ENDFOR
laFlds[30,1] = "nTTRLW"
laFlds[30,2] = 'N'
laFlds[30,3] = 8
*-- Total sales last week quantity fields
FOR lnC = 1 TO 8
  lcC = STR(lnC,1)
  laFlds[30+lnC,1] = 'nSLW'+lcC
  laFlds[30+lnC,2] = 'N'
  laFlds[30+lnC,3] = 7
ENDFOR
laFlds[39,1] = "nTSLW"
laFlds[39,2] = 'N'
laFlds[39,3] = 8
*-- Sales YTD quantity fields
FOR lnC = 1 TO 8
  lcC = STR(lnC,1)
  laFlds[39+lnC,1] = 'nSYTD'+lcC
  laFlds[39+lnC,2] = 'N'
  laFlds[39+lnC,3] = 7
ENDFOR
laFlds[48,1] = "nTSYTD"
laFlds[48,2] = 'N'
laFlds[48,3] = 8
*-- Pcs out YTD (Issued PO's to other locations other than back office) 
FOR lnC = 1 TO 8
  lcC = STR(lnC,1)
  laFlds[48+lnC,1] = 'nOYTD'+lcC
  laFlds[48+lnC,2] = 'N'
  laFlds[48+lnC,3] = 7
ENDFOR
laFlds[57,1] = "nTOYTD"
laFlds[57,2] = 'N'
laFlds[57,3] = 8
*-- Pcs out LW (Issued PO's to other locations other than back office)   
FOR lnC = 1 TO 8
  lcC = STR(lnC,1)
  laFlds[57+lnC,1] = 'nOLW'+lcC
  laFlds[57+lnC,2] = 'N'
  laFlds[57+lnC,3] = 7
ENDFOR
laFlds[66,1] = "nTOLW"
laFlds[66,2] = 'N'
laFlds[66,3] = 8
*-- Damage In fields
FOR lnC = 1 TO 8
  lcC = STR(lnC,1)
  laFlds[66+lnC,1] = 'nDmIn'+lcC
  laFlds[66+lnC,2] = 'N'
  laFlds[66+lnC,3] = 7
ENDFOR
laFlds[75,1] = "nTDmIn"
laFlds[75,2] = 'N'
laFlds[75,3] = 8
*-- Damage out fields
FOR lnC = 1 TO 8
  lcC = STR(lnC,1)
  laFlds[75+lnC,1] = 'nDmOut'+lcC
  laFlds[75+lnC,2] = 'N'
  laFlds[75+lnC,3] = 7
ENDFOR
laFlds[84,1] = "nTDmOut"
laFlds[84,2] = 'N'
laFlds[84,3] = 8
*-- Returns last week
FOR lnC = 1 TO 8
  lcC = STR(lnC,1)
  laFlds[84+lnC,1] = 'nRLW'+lcC
  laFlds[84+lnC,2] = 'N'
  laFlds[84+lnC,3] = 7
ENDFOR
laFlds[93,1] = "nTRLW"
laFlds[93,2] = 'N'
laFlds[93,3] = 8
*-- Returns LW  
FOR lnC = 1 TO 8
  lcC = STR(lnC,1)
  laFlds[93+lnC,1] = 'nRYTD'+lcC
  laFlds[93+lnC,2] = 'N'
  laFlds[93+lnC,3] = 7
ENDFOR
laFlds[102,1] = "nTRYTD"
laFlds[102,2] = 'N'
laFlds[102,3] = 8

*!*************************************************************
*! Name      : lfSRVWar
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 05/17/99
*! Purpose   : To get the damage warehouses not to display them 
*!             in the warehouse browse
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
*! Example   : =lfSRVWar()
*!*************************************************************
FUNCTION lfSRVWar
PARAMETERS lcParam

DIMENSION laDamLoc[1]
IF FILE(gcDataDir+'fDamLoc.mem')
  RESTORE FROM gcDataDir+'fDamLoc.mem' ADDITIVE
ENDIF
SET ORDER TO TAG WAREHOUS IN WAREHOUS
GO TOP IN WAREHOUS

*!*************************************************************
*! Name      : lfSRVSty
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 05/17/99
*! Purpose   : Go top in the style IN RANGE
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
*! Example   : =lfSRVSty()
*!*************************************************************
FUNCTION lfSRVSty  
PARAMETERS lcParam

SET ORDER TO TAG STYLE IN STYLE
GO TOP IN STYLE


*!*************************************************************
*! Name      : lfEndGroup
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 05/17/1999
*! Purpose   : To state that if we would print the word "Continued" 
*!             and to initialize some variables.
*!*************************************************************
*! Called from : ICSTDTAC.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEndGroup()
*!*************************************************************
FUNCTION lfEndGroup
* Set this variable .T. to don't print the word "CONTINUED"
llEndGroup = .T.
RETURN ''



*!*************************************************************
*! Name      : lfColData
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 05/17/1999
*! Purpose   : To COLLECT DATA 
*!*************************************************************
*! Called from : ICSTDTAC.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfColData()
*!*************************************************************
FUNCTION lfColData
WAIT WINDOW 'Collecting data for location: '+ STYDYE.cWareCode+', Style:'+STYDYE.Style NOWAIT
SELECT (lcMainF)
SCATTER MEMVAR BLANK
m.cWareCode = STYDYE.cWareCode
m.Style     = STYDYE.STYLE
lnDisc = 0

IF !EMPTY(STYLE.cDiscCode)
  =gfRltFld(STYLE.cDiscCode,@laDisc,'CDISCCODE')
  *-- If discount is not applicable today, initialize its value.
  IF (EMPTY(ldEnd) .AND. !EMPTY(ldStart) .AND. gdSysDate < ldStart) .OR. ;
     (!EMPTY(ldEnd) .AND. EMPTY(ldStart) .AND. gdSysDate > ldEnd) .OR. ;
     (!EMPTY(ldEnd) .AND. !EMPTY(ldStart) .AND. !BETWEEN(gdSysDate,ldStart,ldEnd))
    lnDisc = 0
  ENDIF  
ENDIF
*-- Calculate retail price today.
m.nSugRetPri  = (1-lnDisc/100) * Style.nSugRetPri
IF 'PO' $ gcComp_Mdl
  SELECT POSLN
  SCAN WHILE STYLE+cstytype = STYDYE.Style+'N' FOR INLIST(TRANCD,'2','6') .AND. ;
          (cWareCode = STYDYE.cWareCOde .OR. Vendor = STYDYE.cWareCOde)
    DO CASE
      *-- Received PO YTD, Pcs In last week
      CASE TRANCD = '2' .AND. cWareCode = STYDYE.cWareCOde 
        *-- If received from the back office warehouses, sum total receipts from them
        IF ASCAN(laBkWars,ALLTRIM(LEFT(POSLN.Vendor,6))) # 0
          IF llRpSize
            FOR lnC = 1 TO 8
              lcC = STR(lnC,1)
              m.nTRBK&lcC = m.nTRBK&lcC+POSLN.QTY&lcC
            ENDFOR
          ENDIF
          m.nTTRBK = m.nTTRBK + POSLN.TOTQTY
        ELSE
          IF ASCAN(laDamLoc,ALLTRIM(LEFT(POSLN.Vendor,6))) # 0
            *-- Sum total damaged In
            IF llRpSize
              FOR lnC = 1 TO 8
                lcC = STR(lnC,1)
                m.nDMIN&lcC = m.nDmIn&lcC + POSLN.QTY&lcC
              ENDFOR
            ENDIF
            m.nTDmIn = m.nTDmIn + POSLN.TOTQTY
          ELSE
            *-- Sum total receipts from locations other than the back office warehouses
            IF llRpSize
              FOR lnC = 1 TO 8
                lcC = STR(lnC,1)
                m.nTRSt&lcC = m.nTRSt&lcC + POSLN.QTY&lcC
              ENDFOR
            ENDIF
            m.nTTrSt = m.nTTrSt + POSLN.TOTQTY
            IF BETWEEN(POSLN.DATE,ldLoDt,ldHiDt)
              IF llRpSize
                FOR lnC = 1 TO 8
                  lcC = STR(lnC,1)
                  m.nTRLW&lcC = m.nTRLW&lcC + POSLN.QTY&lcC
                ENDFOR
              ENDIF
              m.nTTRLW = m.nTTRLW + POSLN.TOTQTY
            ENDIF
          ENDIF
        ENDIF
      *-- Pcs out last week and YTD
      CASE TRANCD='6' .AND. Vendor = STYDYE.cWareCOde
        IF ASCAN(laBkWars,ALLTRIM(LEFT(POSLN.cWareCode,6))) = 0
          *-- If line is issued to the damage location, sum quantities to the damage out quantity
          IF ASCAN(laDamLoc,ALLTRIM(LEFT(POSLN.cWareCode,6))) # 0
            IF llRpSize
              FOR lnC = 1 TO 8
                lcC = STR(lnC,1)
                m.nDmOut&lcC = m.nDmOut&lcC - STYINVJL.nSTK&lcC
              ENDFOR
            ENDIF
            m.nTDmOut = m.nTDmOut - STYINVJL.nTotSTK
          ELSE          
            *-- Sum total Issues to locations other than the back office warehouses
            IF llRpSize
              FOR lnC = 1 TO 8
                lcC = STR(lnC,1)
                m.nOYTD&lcC = m.nOYTD&lcC - STYINVJL.nSTK&lcC
              ENDFOR
            ENDIF
            m.nTOYTD = m.nTOYTD - STYINVJL.nTotSTK
           
            IF BETWEEN(POSLN.DATE,ldLoDt,ldHiDt)
              IF llRpSize
                FOR lnC = 1 TO 8
                  lcC = STR(lnC,1)
                  m.nOLW&lcC = m.nOLW&lcC - STYINVJL.nSTK&lcC
                ENDFOR
              ENDIF
              m.nTOLW = m.nTOLW - STYINVJL.nTotSTK
            ENDIF
          ENDIF
        ENDIF
    ENDCASE
  ENDSCAN
ENDIF
*-- Get total sales
SELECT INVLINE
SCAN WHILE STYLE = STYDYE.Style FOR cWareCode = STYDYE.cWareCode 
  *-- Sum total YTD sales
  IF llRpSize
    FOR lnC = 1 TO 8
      lcC = STR(lnC,1)
      m.nSYTD&lcC = m.nSYTD&lcC + INVLINE.QTY&lcC
    ENDFOR
  ENDIF
  m.nTSYTD = m.nTSYTD + INVLINE.TOTQTY
  *-- Sum last week total sales.
  IF BETWEEN(INVLINE.INVDATE,ldLoDt,ldHiDt)
    IF llRpSize
      FOR lnC = 1 TO 8
        lcC = STR(lnC,1)
        m.nSLW&lcC = m.nSLW&lcC + INVLINE.QTY&lcC
      ENDFOR
    ENDIF
    m.nTSLW = m.nTSLW + INVLINE.TOTQTY
  ENDIF
ENDSCAN
IF 'RM' $ gcComp_Mdl
  SELECT RETLINE
  SCAN WHILE STYLE = STYDYE.Style FOR RETHDR.cWareCode = STYDYE.cWareCode 
    IF llRpSize
      FOR lnC = 1 TO 8
        lcC = STR(lnC,1)
        m.nRYTD&lcC = m.nRYTD&lcC + RETLINE.QTY&lcC
      ENDFOR
    ENDIF
    m.nTRYTD = m.nTRYTD + RETLINE.TOTQTY
    *-- Sum last week total Returns.
    IF BETWEEN(RETLINE.CRDATE,ldLoDt,ldHiDt)
      IF llRpSize
        FOR lnC = 1 TO 8
          lcC = STR(lnC,1)
          m.nRLW&lcC = m.nRLW&lcC + RETLINE.QTY&lcC
        ENDFOR
      ENDIF
      m.nTRLW = m.nTRLW + RETLINE.TOTQTY
    ENDIF
  ENDSCAN
ENDIF

IF llRpZQty .OR. !(m.nTTRBK=0 .AND. m.nTTRSt=0 .AND. m.nTTRLW=0 .AND. ;
   m.nTSLW=0 .AND. m.nTSYTD=0 .AND. m.nTOYTD=0 .AND. m.nTOLW=0 .AND. ;
   m.nTDmIn=0 .AND. m.nTDmOut=0 .AND. m.nTRLW=0 .AND. m.nTRYTD=0 .AND. ;
   STYDYE.TOTSTK = 0)
  *-- Insert record into the temporary file
  INSERT INTO (lcMainF) FROM MEMVAR
ENDIF
