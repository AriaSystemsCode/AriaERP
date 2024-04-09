*:***********************************************************************
*: PROG         : Soe07dls.PRG 
*: DATE         : 07/25/2001
*: Developer    : Ahmed Abdel Naby (AAN)
*: DESC         : Sales Order Delivery Schedule for Stuncroft 
*:***********************************************************************
*: Calls        : 
*:         Functions  : 
*:         Procedures : 
*:***********************************************************************
*: Example      : DO Soe07dls
*:***********************************************************************
*: Reference     : C200209  for Stuncroft LTD.  (ENGLAND)
*:***********************************************************************
*: Modifications ..
*:***********************************************************************
*: C200359,1 RAE 06/24/2002 Add new filters to Option Grid :
*:                          1- Purchasing Group
*:                          2- Suppress Ex-Factory / Factory columns
*:                          3- Print SOs without a PO
*:                          4- Only SO with hold PO 
*:                          5- SO with PO has no Label
*:                          6- Division
*:                          7- Sort 3: Description / Purchasing Group / Fabric
*:***********************************************************************
*--Update lcRpExp

PRIVATE lcStyle , lnLineNo , lnMajLen , lcMonth , lcYear
lcStyle = ""
lnlineno = 0
lnMajLen = 12

lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),"",[ AND ]) + [ORDHDR.STATUS =='O']

DO CASE
  CASE lcRpSort1 == "M"
    lcRpSort1 = "CMonth(cCusdldt)"
  CASE lcRpSort1 == "G"
    lcRpSort1 = "cStyGrp"
  CASE lcRpSort1 == "S"
    lcRpSort1 = "cPattern"
  CASE lcRpSort1 == "C"
    lcRpSort1 = "cAccount"
  CASE lcRpSort1 == "O"
    lcRpSort1 = "cOrdCat"
ENDCASE
DO CASE
  CASE lcRpSort2 == "M"
    lcRpSort2 = "CMonth(cCusdldt)"
  CASE lcRpSort2 == "G"
    lcRpSort2 = "cStyGrp"
  CASE lcRpSort2 == "S"
    lcRpSort2 = "cPattern"
  CASE lcRpSort2 == "C"
    lcRpSort2 = "cAccount"
  CASE lcRpSort2 == "O"
    lcRpSort2 = "cOrdCat"
ENDCASE
*C200359,1 RAE [Start]
*-- Sort 3: Description / Purchasing Group / Fabric
DO CASE
  CASE lcRpSort3 == "D"
    lcRpSort3 = "cStyDesc"
  CASE lcRpSort3 == "P"
    lcRpSort3 = "cPurchGrp"
  CASE lcRpSort3 == "F"
    lcRpSort3 = "cFabric"
ENDCASE
*C200359,1 RAE [End]
*--To print Company name
*SELECT SYCCOMP
*SEEK gcAct_Comp
*lcCompName = cCom_Name
lcCompName  = gcCom_Name

=lfCretTemp()
SELECT ORDHDR
SET ORDER TO TAG Ordhdr     

*-- To collect a data (Main loop)
SELECT ORDHDR
SET RELATION TO "O" + ORDER INTO OrdLine ADDITIVE
SELECT ORDLINE
SET RELATION TO STYLE INTO STYLE ADDITIVE
*C200359,1 RAE [Start] 
*-- Relation between STYLE file and FABRIC file.
SELECT STYLE
SET RELATION TO STYLE.FABRIC INTO FABRIC ADDITIVE

*--Check if there are data in OrdHdr file or not.
*SELECT ORDHDR
*LOCATE FOR &lcRpExp
*IF !FOUND()
*  =gfModalGen('TRM00052B00000','DIALOG' )
*  RETURN
*ENDIF

SELECT ORDHDR
LOCATE
*SCAN FOR &lcRpExp
SCAN REST WHILE CORDTYPE + ORDER = "O"
*C200359,1 RAE [End]

  WAIT WINDOW "Collecting data for order# : " + Order NOWAIT
  SELECT OrdLine
  SCAN REST WHILE cOrdType+Order+STR(lineno,6) = "O"+OrdHdr.Order FOR &lcRpExp
    lcStyle = LEFT(OrdLine.Style,lnMajLen)
    lnLineNo = lineno
    lcMonth = CMonth(OrdLine.Complete)
    lcYear = ALLTRIM(STR(Year(OrdLine.Complete)))
    *--If current style found in Temp. file don't collect a data.]
    SELECT (lcTmpOrdL)
    SET ORDER TO TAG Style
    IF !SEEK(OrdLine.Order+lcStyle+CMonth(OrdLine.Complete)+ALLTRIM(STR(Year(OrdLine.Complete))))
      SET ORDER TO TAG (lcTmpOrdL)
      SELECT OrdLine
      m.cMajor = lcStyle
      m.cCusDlDt = Complete
      m.corder = OrdHdr.Order
      m.cOrdCat = OrdHdr.cOrderCat
      m.cPattern = Style.Pattern
      m.cStyDesc = Style.Desc
      *C200359,1 RAE New items to be printed in FRX. [Start]
      m.CcustPO   = ALLTRIM(ORDHDR.CUSTPO)
      m.cFabric   = STYLE.FABRIC
      m.cLineNo   = ITEM_NO
      m.cPurchGrp = STYLE.CPURCODE
      *C200359,1 RAE [End]
      m.cStyGrp = Style.cStyGroup
      m.cBtName = IIF(SEEK('M'+Account,'Customer') , Customer.BtName , "")
      m.cAccount = IIF(SEEK('M'+Account,'Customer') , Customer.Account , "")
      m.cFabPat = lfPurCode()
      STORE ""  TO  m.cFactory  ,  lcPoNo  , m.cDockNo , m.dExFac , m.dExFac
      STORE 0 TO m.nDockQty , m.nOrdered , m.nvalue

      IF SEEK("2"+m.cOrder,'CutPick')
        =lfDockInf(@m.cDockNo,@m.cFactory,@m.dExFac)
      ELSE
        m.dExFac = LEFT(ALLTRIM(OrdLine.Note_Mem),10)
      ENDIF
      *--Get the total qty from Order Line. 
      =lfScanOrdl(@m.nOrdered)
      
      *C200359,1 RAE [Start]
      *STORE "" TO m.cGrp1Dsc,m.cGrp2Dsc
      *=lfvgroup(@m.cGrp1Dsc,@m.cGrp2Dsc)

      *-- Add m.cGrp3Dsc to hold the group discription from Sort3
      STORE "" TO m.cGrp1Dsc , m.cGrp2Dsc , m.cGrp3Dsc
      =lfvgroup(@m.cGrp1Dsc,@m.cGrp2Dsc,@m.cGrp3Dsc)

      *INSERT INTO (lcTmpOrdL) FROM MEMVAR
      *-- If Print SO with PO on hold status is 'No'
      IF lcRpHldSo = 'Y'
        IF SEEK("2"+m.cOrder,'CutPick') .AND. SEEK("P"+m.cDockNo,'PosHdr') .AND. POSHDR.STATUS = 'H'
          *-- If Print SO without a PO is 'No'
          IF lcRpLabl = 'Y'
            IF !SEEK("P"+m.cDockNo,'LBLINST')
              INSERT INTO (lcTmpOrdL) FROM MEMVAR
            ENDIF
          ELSE
            INSERT INTO (lcTmpOrdL) FROM MEMVAR
          ENDIF
        ENDIF
      ELSE
        IF lcRpLabl = 'Y'
          IF SEEK("2"+m.cOrder,'CutPick') .AND. SEEK("P"+m.cDockNo,'PosHdr') .AND. POSHDR.STATUS = 'H'
            *-- SO with PO has no Label
            IF !SEEK("P"+m.cDockNo,'LBLINST')
              INSERT INTO (lcTmpOrdL) FROM MEMVAR
            ENDIF
          ENDIF
        ELSE          
          *-- If Print SO without a PO is 'Yes'
          IF lcRpSoPo = 'Y'
            IF !SEEK("2"+m.cOrder,'CutPick')
              INSERT INTO (lcTmpOrdL) FROM MEMVAR
            ENDIF
          *C200359,4 RAE [Start]
          *ELSE
          *  INSERT INTO (lcTmpOrdL) FROM MEMVAR
          *C200359,4 RAE [End]
          ENDIF
        ENDIF
      ENDIF
      *C200359,1 RAE [End]

      lcCutPKey = "2"+m.corder
      lcPoNo = m.cDockNo
      *C200359,4 RAE [Start]
      IF lcRpSoPo = 'N'
      *C200359,4 RAE [End]
        SELECT CUTPICK
        IF SEEK("2"+m.cOrder,'CutPick')
          SCAN REST WHILE TRANCD+ORDER+CORDLINE = lcCutPKey  ;
                     FOR  CTKTNO <> lcPoNo AND CUTPICK.Style = lcStyle
            =lfDockInf(@m.cDockNo,@m.cFactory,@m.dExFac)
            SELECT (lcTmpOrdL)
            *C200359,4 RAE [Start]
            *STORE "" TO m.cCusdldt, m.cBtName, m.cOrder, m.cPattern , m.cStyDesc , m.cFabPat          
            *STORE 0 TO m.nOrdered
            IF lcRpHldSo = 'Y'
              IF SEEK("P"+m.cDockNo,'PosHdr') .AND. POSHDR.STATUS = 'H'
              *-- If Print SO without a PO is 'No'
                IF lcRpLabl = 'Y'
                  IF !SEEK("P"+m.cDockNo,'LBLINST')
                    INSERT INTO (lcTmpOrdL) FROM MEMVAR
                  ENDIF
                ELSE
                  INSERT INTO (lcTmpOrdL) FROM MEMVAR
               ENDIF
             ENDIF
           ELSE
             IF lcRpLabl = 'Y'
               IF SEEK("P"+m.cDockNo,'PosHdr') .AND. POSHDR.STATUS = 'H'
                 *-- SO with PO has no Label
                 IF !SEEK("P"+m.cDockNo,'LBLINST')
                   INSERT INTO (lcTmpOrdL) FROM MEMVAR
                 ENDIF
               ENDIF
             ELSE
               INSERT INTO (lcTmpOrdL) FROM MEMVAR      
             ENDIF
           ENDIF
          ENDSCAN          
          IF m.cdockno = lcPoNo
            IF lcRpHldSo = 'Y'
              IF SEEK("P"+m.cDockNo,'PosHdr') .AND. POSHDR.STATUS = 'H'
                *-- If Print SO without a PO is 'No'
                IF lcRpLabl = 'Y'
                  IF !SEEK("P"+m.cDockNo,'LBLINST')
                    INSERT INTO (lcTmpOrdL) FROM MEMVAR
                  ENDIF
                ELSE
                  INSERT INTO (lcTmpOrdL) FROM MEMVAR
                ENDIF
              ENDIF
            ELSE
              IF lcRpLabl = 'Y'
                IF SEEK("P"+m.cDockNo,'PosHdr') .AND. POSHDR.STATUS = 'H'
                  *-- SO with PO has no Label
                  IF !SEEK("P"+m.cDockNo,'LBLINST')
                    INSERT INTO (lcTmpOrdL) FROM MEMVAR
                  ENDIF
                ENDIF
              ELSE 
                INSERT INTO (lcTmpOrdL) FROM MEMVAR
              ENDIF
            ENDIF
          ENDIF
          *C200359,4 RAE [End]
        ENDIF
      ENDIF
      SELECT OrdLine
    ENDIF
    SELECT (lcTmpOrdL)
    SET ORDER TO TAG (lcTmpOrdL)
    SELECT OrdLine
  ENDSCAN  
  SELECT ORDHDR
ENDSCAN

SELECT (lcTmpOrdL)
*C200359,1 RAE [Start]
*LOCATE
*DO gfDispRe WITH EVAL('lcRpName')
IF RECCOUNT() = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ELSE
  LOCATE
  DO gfDispRe WITH EVAL('lcRpName')
ENDIF
*C200359,1 RAE [End]
*:***************************************************************************
*: Name        : lfCretTemp
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 02/01/2001
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfCretTemp()
*:***************************************************************************
*
FUNCTION lfCretTemp
*C200359,1 RAE Add new 5 fields. [Start]
*DIMENSION laFStru[16,4]
DIMENSION laFStru[21,4]
*C200359,1 RAE [End]
laFStru[1,1] = 'dExFac'
laFStru[1,2] = 'C'
laFStru[1,3] = 10
laFStru[1,4] = 0
laFStru[2,1] = 'cCusDlDt'
laFStru[2,2] = 'D'
laFStru[2,3] = 10
laFStru[2,4] = 0
laFStru[3,1] = 'cBtName'
laFStru[3,2] = 'C'
laFStru[3,3] = 30
laFStru[3,4] = 0
laFStru[4,1] = 'cOrder'
laFStru[4,2] = 'C'
laFStru[4,3] = 6
laFStru[4,4] = 0
laFStru[5,1] = 'cPattern'
laFStru[5,2] = 'C'
laFStru[5,3] = 10
laFStru[5,4] = 0
laFStru[6,1] = 'cStyDesc'
laFStru[6,2] = 'C'
laFStru[6,3] = 20
laFStru[6,4] = 0
laFStru[7,1] = 'nordered'
laFStru[7,2] = 'N'
laFStru[7,3] = 12
laFStru[7,4] = 0
laFStru[8,1] = 'cFactory'
laFStru[8,2] = 'C'
laFStru[8,3] = 30
laFStru[8,4] = 0
laFStru[9,1] = 'cDockNo'
laFStru[9,2] = 'C'
laFStru[9,3] = 6
laFStru[9,4] = 0
laFStru[10,1] = 'cFabPat'
laFStru[10,2] = 'C'
laFStru[10,3] = 1
laFStru[10,4] = 0
laFStru[11,1] = 'cStyGrp'
laFStru[11,2] = 'C'
laFStru[11,3] = 6
laFStru[11,4] = 0
laFStru[12,1] = 'cOrdCat'
laFStru[12,2] = 'C'
laFStru[12,3] = 6
laFStru[12,4] = 0
laFStru[13,1] = 'cAccount'
laFStru[13,2] = 'C'
laFStru[13,3] = 5
laFStru[13,4] = 0
laFStru[14,1] = 'cMajor'
laFStru[14,2] = 'C'
laFStru[14,3] = 12
laFStru[14,4] = 0
laFStru[15,1] = 'cGrp1Dsc'
laFStru[15,2] = 'C'
laFStru[15,3] = 30
laFStru[15,4] = 0
laFStru[16,1] = 'cGrp2Dsc'
laFStru[16,2] = 'C'
laFStru[16,3] = 30
laFStru[16,4] = 0
*C200359,1 RAE Add new 5 fields. [Start]
laFStru[17,1] = 'cFabric'
laFStru[17,2] = 'C'
laFStru[17,3] = 7
laFStru[17,4] = 0
laFStru[18,1] = 'cLineNo'
laFStru[18,2] = 'C'
laFStru[18,3] = 20
laFStru[18,4] = 0
laFStru[19,1] = 'CcustPO'
laFStru[19,2] = 'C'
laFStru[19,3] = 15
laFStru[19,4] = 0
laFStru[20,1] = 'cPurchGrp'
laFStru[20,2] = 'C'
laFStru[20,3] = 15
laFStru[20,4] = 0
laFStru[21,1] = 'cGrp3Dsc'
laFStru[21,2] = 'C'
laFStru[21,3] = 30
laFStru[21,4] = 0
*C200359,1 RAE [End]
CREATE DBF (gcWorkDir+lcTmpOrdL) FROM ARRAY laFStru
=gfOpenFile(gcWorkDir+'&lcTmpOrdL','','EX')
IF lcRpSort1 = "CMonth(cCusdldt)"
  lcRpSort1 = "DTOS(cCusdldt)"
ENDIF
IF lcRpSort2 = "CMonth(cCusdldt)"
  lcRpSort2= "DTOS(cCusdldt)"
ENDIF
*C200359,1 RAE Update the index. [Start]
*INDEX ON  &lcRpSort1 + &lcRpSort2  TAG (lcTmpOrdL)  OF (gcWorkDir + lcTmpOrdL+ '.CDX')
INDEX ON  &lcRpSort1 + &lcRpSort2 + &lcRpSort3 TAG (lcTmpOrdL)  OF (gcWorkDir + lcTmpOrdL+ '.CDX')
*C200359,1 RAE [End]
INDEX ON  cOrder + cMajor + CMonth(cCusDlDt) + ALLTRIM(STR(Year(cCusDlDt))) TAG Style  OF (gcWorkDir + lcTmpOrdL+ '.CDX')
SET ORDER TO TAG (lcTmpOrdL)
IF lcRpSort1 = "DTOS(cCusdldt)"
  lcRpSort1 = "CMonth(cCusdldt)"
ENDIF
IF lcRpSort2 = "DTOS(cCusdldt)"
  lcRpSort2 = "CMonth(cCusdldt)"
ENDIF

*:***************************************************************************
*: Name        : lfScanOrdl
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 02/01/2001
*: Purpose     : 
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfScanOrdl()
*:***************************************************************************
*
FUNCTION lfScanOrdl
PARAMETERS m.nOrdered
PRIVATE lnTotQty
STORE 0 TO lnTotQty
SELECT OrdLine
=SEEK( "O"+m.corder)
SCAN REST WHILE cOrdType+Order+STR(lineno,6) = "O"+m.corder  FOR Style = lcStyle AND CMONTH(Complete) = lcMonth AND ALLTRIM(STR(YEAR(Complete))) = lcYear
  lnTotQty = lnTotQty + TotQty
ENDSCAN
m.nOrdered = lnTotQty
SELECT OrdLine
=SEEK("O"+m.corder+STR(lnLineNo,6))
*--End of lfScanOrdl. 

*:***************************************************************************
*: Name        : lfPurCode
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 02/01/2001
*: Purpose     : 
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfPurCode()
*:***************************************************************************
*
FUNCTION lfPurCode
PRIVATE cFabPat
STORE ""  TO cFabPat
IF SEEK(Style.Fabric,'Fabric')
  cFabPat = LEFT(ALLTRIM(Fabric.Pattern),1)    
ENDIF
RETURN cFabPat
*--Enf of lfPurCode.

*:***************************************************************************
*: Name        : lfDockInf
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 02/01/2001
*: Purpose     : 
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfDockInf()
*:***************************************************************************
*
FUNCTION lfDockInf
PARAMETERS m.cDockNo,m.cFactory,m.dExFac
PRIVATE lcPoNo
lcPoNo =  CutPick.cTktNo        
m.cDockNo = CutPick.cTktNo         && Var. that hold Docket No.
IF SEEK( "P" + lcPoNo  , "PosLn") AND SEEK(PosLn.Vendor, "ApVendor")
  m.cFactory  = ApVendor.cVenComp
ENDIF
m.dExFac = IIF(SEEK("P" + lcPoNo ,'PosHdr'),DTOC(PosHdr.Complete),LEFT(ALLTRIM(OrdLine.Note_Mem),10))

*:***************************************************************************
*: Name        : lfwRepWhen
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 02/01/2001
*: Purpose     : 
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfwRepWhen()
*:***************************************************************************
*
FUNCTION lfwRepWhen
PRIVATE lcAlias , lnMjLn
lnMjLn = 12
lcAlias = SELECT(0)
SELECT STYLE
DECLARE laStuStyle(1,1)
=AFIELDS(laStuStyle)
CREATE CURSOR (lcTmpSty) FROM ARRAY laStuStyle
INDEX ON PATTERN TAG (lcTmpSty) OF (lcTmpSty)
SELECT (lcTmpSty)
SET ORDER TO (lcTmpSty)
SELECT STYLE
LOCATE
SCAN REST WHILE STYLE = STYLE
  IF !SEEK(PATTERN,lcTmpSty)
    SCATTER MEMVAR MEMO
    m.Style = LEFT(m.Style,lnMjLn)
    SELECT(lcTmpSty)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDIF
ENDSCAN
SELECT (lcTmpSty)
LOCATE
SELECT (lcAlias)

*:***************************************************************************
*: Name        : lfvGroup
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 02/01/2001
*: Purpose     : 
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfvGroup()
*:***************************************************************************
FUNCTION lfvGroup
*C200359,1 RAE [Start]
*PARAMETERS m.cGrp1Dsc,m.cGrp2Dsc
PARAMETERS m.cGrp1Dsc,m.cGrp2Dsc,m.cGrp3Dsc
*C200359,1 RAE [End]
DO CASE
  CASE lcRpSort1 == "CMonth(cCusdldt)"
    m.cGrp1Dsc = CMonth(m.cCusDlDt)
  CASE lcRpSort1 == "cStyGrp"
    m.cGrp1Dsc = gfCodDes(m.cStyGrp , 'CSTYGROUP ')
  CASE lcRpSort1 == "cPattern"
    m.cGrp1Dsc = m.cPattern
  CASE lcRpSort1 == "cAccount"
    m.cGrp1Dsc = m.cBtName
  CASE lcRpSort1 == "cOrdCat"
    m.cGrp1Dsc = gfCodDes(m.cOrdCat , 'CORDERCAT ')
ENDCASE
DO CASE
  CASE lcRpSort2 == "CMonth(cCusdldt)"
    m.cGrp2Dsc = CMonth(m.cCusDlDt)
  CASE lcRpSort2 == "cStyGrp"
    m.cGrp2Dsc = gfCodDes(m.cStyGrp , 'CSTYGROUP ')
  CASE lcRpSort2 == "cPattern"
    m.cGrp2Dsc = m.cPattern
  CASE lcRpSort2 == "cAccount"
    m.cGrp2Dsc = m.cBtName
  CASE lcRpSort2 == "cOrdCat"
    m.cGrp2Dsc = gfCodDes(m.cOrdCat,'CORDERCAT ')
ENDCASE
*C200359,1 RAE Case of Sort 3 [Start]
DO CASE
  CASE lcRpSort3 = "cStyDesc"
    m.cGrp3Dsc = m.cStyDesc
  CASE lcRpSort3 = "cPurchGrp"
    m.cGrp3Dsc = gfCodDes(m.cPurchGrp,'CPURCODE')
  CASE lcRpSort3 = "cFabric"
    m.cGrp3Dsc = m.cFabric
ENDCASE
*C200359,1 RAE [End]

*!*************************************************************
*! Name      : lfclearrd
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 07/03/2002
*! Purpose   : Clear the Option Grid files.
*!*************************************************************
*! Called from        : SYREPUVR
*!*************************************************************
*! Example            : =lfclearrd()
*!*************************************************************
*! C200359,1

FUNCTION lfclearrd

CLEAR READ
*C200359,4 RAE [Start]
lcRpSort1 = "M"
lcRpSort2 = "M"
lcRpSort3 = "D"
*C200359,4 RAE [End]