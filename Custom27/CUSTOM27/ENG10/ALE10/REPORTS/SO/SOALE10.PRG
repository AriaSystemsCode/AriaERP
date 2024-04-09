*:***********************************************************************
*: PROG         : SoAle10.PRG 
*: DATE         : 02/01/2001
*: Developer    : Ahmed Abdel Naby (AAN)
*: DESC         : Sales Order Analysis for Alena  
*:***********************************************************************
*: Calls        : 
*:         Functions  : 
*:         Procedures : 
*:***********************************************************************
*: Example      : DO SoAle10
*:***********************************************************************
*: Reference     : C200157  for ALEND LTD.  (ENGLAND) 
*:B605032,1 AKA  10/15/2001 Fix wrong calculation of the cancelled qty
*:C200247,1 MHM  11/26/2001 we add the folloing to program
*:C200247,1 MHM             1-print a blank line after each data line (in FRX)
*:C200247,1 MHM             2-add user defined field "BookRef"
*:C200284,1 ABD  01/29/2002 Add a Further Column to the report titled 
*:C200284,1 ABD             Fabric Del Date as a last column.
*:B605604,1 ADEL 03/07/2002 Fix the bug of accumulating docket (po) qty.
*:B605877,1 RAE  04/22/2002 1- fix ordered qty calculation.
*:***********************************************************************

PRIVATE lcCentury
*--Update lcRpExp
IF !llRpIncCom
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),"",[ AND ]) + [ORDHDR.STATUS $ 'OHB']
ELSE  
  lcRpExp = lcRpExp + IIF(EMPTY(lcRpExp),"",[ AND ]) + [ORDHDR.STATUS <>'X']
ENDIF

*--Check if there are data in OrdHdr file or not.
SELECT ORDHDR
LOCATE  FOR &lcRpExp
IF !FOUND()
  =gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF

lcCentury = SET('CENTURY')
SET CENTURY ON

STORE 0 TO lnTotBook , lnTotCanLn , lnBookAmt , lnVal2 , nDockQty
STORE '' TO m.ctype 
*-- Function to calculate major and nonmajor length.
=lfEvalSegs()

*--To print Company name
SELECT SYCCOMP
SEEK gcAct_Comp
lcCompName = cCom_Name

*-- Another Alias for OrdLine file.
lcAlias = gfTempName()
*-- Another Alias for ordcanln file.
lcCanAls = gfTempName()
*-- To create temp file.
=lfCretTemp()

*-- To open OrdLine file for second time if not opened.
IF !USED(lcalias)
  =gfOpenFile(gcDataDir+'OrdLine','Ordline','SH',@lcAlias,.T.)
ENDIF

*-- To open ordcanln file for second time if not opened.
IF !USED(lcCanAls)
  =gfOpenFile(gcDataDir+'OrdCanLn','OrdCanLn','SH',@lcCanAls,.T.)
ENDIF  

*C200284,1 Abd - Open needed files to Add a Further Column to the report titled 
*C200284,1       Fabric Del Date as a last column. [Begin]
IF !USED('MAPOALO')
  =gfOpenFile(gcDataDir+'MAPOALO',gcDataDir+'MAPOSO','SH')
ENDIF  

IF !USED('POFHDR')
  =gfOpenFile(gcDataDir+'POFHDR',gcDataDir+'POFHDR','SH')
ENDIF

IF !USED('BOM')
  =gfOpenFile(gcDataDir+'BOM',gcDataDir+'BOM','SH')
ENDIF
*B605877,1 RAE
*DIMENSION laSetups[10,2]  , laFabNo[1]
DIMENSION laSetups[10,2]  , laMFabNo[1],laIFabNo[1]
*B605877,1 RAE
STORE '' To laFabNo , laSetups
STORE 0  To lnAryCount


laSetups[1,1]  = 'M_cMType1'
laSetups[2,1]  = 'M_cMType2'
laSetups[3,1]  = 'M_cMType3'
laSetups[4,1]  = 'M_cMType4'
laSetups[5,1]  = 'M_cMType5'
*B605877,1 RAE
laSetups[6,1]   = 'M_CITYPE1'
laSetups[7,1]   = 'M_CITYPE2'
laSetups[8,1]   = 'M_CITYPE3'
laSetups[9,1]   = 'M_CITYPE4'
laSetups[10,1]  = 'M_CITYPE5'
*B605877,1 RAE
*-- Use gfGetMemVar once per all variables.
=gfGetMemVar(@laSetups,gcAct_Comp)

lcMType1   = laSetups[1,2]
lcMType2   = laSetups[2,2]
lcMType3   = laSetups[3,2]
lcMType4   = laSetups[4,2]
lcMType5   = laSetups[5,2]
*B605877,1 RAE
*FOR lnCheck = 1 TO 5
*  lcFabVar  = "lcMType" + STR(lnCheck,1)
*  IF EVAL(lcFabVar) = 'F'
*    lnAryCount = lnAryCount + 1
*    IF !EMPTY(laFabNo[1])
*      DECLARE laFabNo[lnAryCount]
*    ENDIF
*    laFabNo[lnAryCount] = STR(lnCheck,1)
*  ENDIF

lcIType1   = laSetups[6,2]
lcIType2   = laSetups[7,2]
lcIType3   = laSetups[8,2]
lcIType4   = laSetups[9,2]
lcIType5   = laSetups[10,2]

FOR lnCheck = 1 TO 5
  lcFabVar  = "lcMType" + STR(lnCheck,1)
  IF EVAL(lcFabVar) = 'F'
    lnAryCount = lnAryCount + 1
    IF !EMPTY(laMFabNo[1])
      DECLARE laMFabNo[lnAryCount]
    ENDIF
    laMFabNo[lnAryCount] = STR(lnCheck,1)
  ENDIF
  
  lnAryCount = 0
  lcFabVar  = "lcIType" + STR(lnCheck,1)
  IF EVAL(lcFabVar) = 'F'
    lnAryCount = lnAryCount + 1
    IF !EMPTY(laIFabNo[1])
      DECLARE laIFabNo[lnAryCount]
    ENDIF
    laIFabNo[lnAryCount] = STR(lnCheck,1)
  ENDIF
ENDFOR
*B605877,1 RAE
*C200284,1 [End]

SELECT ORDHDR
SET ORDER TO TAG Ordhdr     

*-- To collect a data (Main loop)
SELECT ORDHDR
SET RELATION TO "O" + ORDER INTO OrdLine ADDITIVE
SCAN FOR &lcRpExp
  WAIT WINDOW "Collecting data for order# : " + Order NOWAIT
   
  IF SEEK(ORDHDR.cOrdType +ORDHDR.Order, "OrdLine") 
    SELECT OrdLine
    SCAN REST WHILE cOrdType+Order+STR(lineno,6) = "O"+OrdHdr.Order
      *--If current style found in Temp. file don't collect a data.
      IF !SEEK(Order+SUBSTR(Style,1,LnMajorlen),lcTmpOrdL)
        m.cMajor   = SUBSTR(Style,1,LnMajorlen)
        m.corder   = OrdHdr.Order      
        m.dStart   = OrdHdr.Start
        m.dEntered = OrdHdr.Entered
        m.cCustPo  = OrdHdr.CustPo
        
        *:C200247,1 MHM  11/26/2001 get cBookRef from OrdHdr file [Start]
        m.cBookRef = OrdHdr.cBookRef
        *:C200247,1 MHM      [End]
        
        *:C200284,1 ABD - Add a Further Column to the report titled 
        *:C200284,1 ABD - Fabric Del Date as a last column. [Begin]
        m.dFabDelDte = lfGetFabDt ()
        *:C200284,1 ABD - [End]
        
        m.cBtName = IIF(SEEK('M'+Account,'Customer') , Customer.BtName , "")
        
        m.cType = lfPurCode()
        m.cStyDesc = lfStyDesc()
        STORE ""  TO  m.cFactory  ,  lcPoNo  , m.cDockNo 
        STORE 0 TO m.nDockQty , m.nOrdered , m.nvalue

        =lfScanOrdl(@m.nOrdered , @m.nValue)
        
        IF SEEK("2"+m.corder,'CutPick') 
          =lfDockInf(@m.cDockNo,@m.cFactory,@m.nDockQty)
        ENDIF
      
        INSERT INTO (lcTmpOrdL) FROM MEMVAR
        

        lcCutPKey = "2"+m.corder 
        lcPoNo = m.cDockNo
        lcStyle = m.cMajor
        SELECT CUTPICK
        SCAN REST WHILE TRANCD+ORDER+CORDLINE = lcCutPKey  ;
                  FOR  CTKTNO <> lcPoNo AND CUTPICK.Style = lcStyle 
                  
          =lfDockInf(@m.cDockNo,@m.cFactory,@m.nDockQty)        
          SELECT (lcTmpOrdL)
          *B605604,1 (Begin) Don't duplicate the same PO for the same order.
          *B605604,1         This happens only when po has more than one line.
          LOCATE FOR cDockNo = m.cDockNo
          IF !FOUND()
          *B605604,1 (End)
            STORE "" TO m.cCustPo, m.cBtName, m.cOrder, m.cMajor , m.cStyDesc , m.cType 
            STORE CTOD("")  TO m.dEntered 
            STORE 0 TO m.nOrdered , m.nValue 
		    INSERT INTO (lcTmpOrdL) FROM MEMVAR
          *B605604,1 (Begin) Don't duplicate the same PO for the same order.		  
		  ENDIF  
		  *B605604,1 (End)
        ENDSCAN
      ENDIF  
      SELECT OrdLine
    ENDSCAN  
  ENDIF   
  SELECT ORDHDR
ENDSCAN


SELECT (lcTmpOrdL)
SET ORDER TO StDate  
LOCATE 
DO gfDispRe WITH EVAL('lcRpName')
SET CENTURY &lcCentury

*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 02/01/2001
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
*
FUNCTION lfEvalSegs
PARAMETER lcReturn
STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF
*-- end of lfEvalSegs.


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
*:C200247,1 MHM  11/26/2001 Increase dimension by one for cBookRef Field [Start]
*DIMENSION laFStru[13,4]
*:C200284,1 ABD  Add a Further Column to the report titled 
*:C200284,1 ABD  Fabric Del Date as a last column. [Begin]
*DIMENSION laFStru[14,4]
DIMENSION laFStru[15,4]
*:C200284,1 ABD  [End]
*:C200247,1 MHM   [End]

laFStru[1,1] = 'dStart'
laFStru[1,2] = 'D'
laFStru[1,3] = 10
laFStru[1,4] = 0
laFStru[2,1] = 'cCustPo'
laFStru[2,2] = 'C'
laFStru[2,3] = 15
laFStru[2,4] = 0
laFStru[3,1] = 'cBtName'
laFStru[3,2] = 'C'
laFStru[3,3] = 30
laFStru[3,4] = 0
laFStru[4,1] = 'cOrder'
laFStru[4,2] = 'C'
laFStru[4,3] = 6
laFStru[4,4] = 0
laFStru[5,1] = 'dEntered'
laFStru[5,2] = 'D'
laFStru[5,3] = 10
laFStru[5,4] = 0
laFStru[6,1] = 'cMajor'
laFStru[6,2] = 'C'
laFStru[6,3] = 19
laFStru[6,4] = 0
laFStru[7,1] = 'cStyDesc'
laFStru[7,2] = 'C'
laFStru[7,3] = 20
laFStru[7,4] = 0
laFStru[8,1] = 'nordered'
laFStru[8,2] = 'N'
laFStru[8,3] = 12
laFStru[8,4] = 0
laFStru[9,1] = 'nValue'
laFStru[9,2] = 'N'
laFStru[9,3] = 12
laFStru[9,4] = 2
laFStru[10,1] = 'cFactory'
laFStru[10,2] = 'C'
laFStru[10,3] = 30
laFStru[10,4] = 0
laFStru[11,1] = 'cDockNo'
laFStru[11,2] = 'C'
laFStru[11,3] = 6
laFStru[11,4] = 0
laFStru[12,1] = 'nDockQty'
laFStru[12,2] = 'N'
laFStru[12,3] = 12
laFStru[12,4] = 0
laFStru[13,1] = 'cType'
laFStru[13,2] = 'C'
laFStru[13,3] = 1
laFStru[13,4] = 0

*:C200247,1 MHM  11/26/2001 add cBookRef to temp. file [Start]
laFStru[14,1] = 'cBookRef'
laFStru[14,2] = 'C'
laFStru[14,3] = 6
laFStru[14,4] = 0
*:C200247,1 MHM  [End]

*:C200284,1 ABD  Add a Further Column to the report titled 
*:C200284,1 ABD  Fabric Del Date as a last column. [Begin]
laFStru[15,1] = 'dFabDelDte'
laFStru[15,2] = 'D'
laFStru[15,3] = 10
laFStru[15,4] = 0
*:C200284,1 ABD  [End]

CREATE DBF (gcWorkDir+lcTmpOrdL) FROM ARRAY laFStru
=gfOpenFile(gcWorkDir+'&lcTmpOrdL','','EX')
INDEX ON  cOrder + cMajor  TAG Style  OF (gcWorkDir + lcTmpOrdL+ '.CDX') 
INDEX ON  dstart   TAG StDate  OF (gcWorkDir + lcTmpOrdL+ '.CDX') 
SET ORDER TO Style   


*INDEX ON "O" + corder TAG (lcTmpOrdL)
*SET ORDER TO TAG (lcTmpOrdL)

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
PARAMETERS m.nOrdered , m.nValue

PRIVATE lnTotBook, lnCanlAmt, lnCanlQty 
STORE 0 TO lnTotBook , lnCanlAmt ,lnCanlQty ,lnBookAmt , lnTotCnlQy
SELECT (lcalias)
=SEEK( "O"+m.corder)
SCAN REST WHILE cOrdType+Order+STR(lineno,6) = "O"+m.corder  FOR Style = m.cMajor

  *B605032,1 AKA  10/15/2001 Fix wrong calculation of the cancelled qty[Start]
  *lnTotBook = lnTotBook + TotBook
  *lnBookAmt = lnBookAmt+(TotBook*Price)
  *lnCanlQty= lfScanOrdC(OrdLine.Style) 
  *lnCanlAmt = lnCanlQty * OrdLine.Price
  

  lnTotBook = lnTotBook + TotBook
  lnBookAmt = lnBookAmt+(TotBook*Price)
  lnCanlQty= lfScanOrdC(STR(lineno,6))
  lnCanlAmt = lnCanlAmt + (lnCanlQty * Price) 
  lnTotCnlQy= lnTotCnlQy+ lnCanlQty
  *:B605032,1 AKA  10/15/2001 Fix wrong calculation of the cancelled qty[End]
  
  SELECT (lcalias)
ENDSCAN

*B605032,1 AKA  10/15/2001 Fix wrong calculation of the cancelled qty[Start]
*m.nOrdered = lnTotBook - lnCanlQty
*B605877,1 RAE fix ordered qty calculation. [Start]
*m.nOrdered = ORDHDR.OPEN
m.nOrdered = lnTotBook - lnTotCnlQy
*B605877,1 RAE [End]

*B605032,1 AKA  10/15/2001 Fix wrong calculation of the cancelled qty[End]

m.nValue = ( lnBookAmt - lnCanlAmt )  / OrdHdr.nExRate

*RETURN m.nOrdered , m.nValue
*--End of lfScanOrdl. 

*:***************************************************************************
*: Name        : lfScanOrdC
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 02/01/2001
*: Purpose     : 
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfScanOrdC()
*:***************************************************************************
*
FUNCTION lfScanOrdC
PARAMETER  lcStyLNo
PRIVATE lnCanQty, lnArea
lnCanQty = 0 
*:B605032,1 AKA  10/15/2001 Fix wrong calculation of the cancelled qty[Start]
lnArea = SELECT()
*:B605032,1 AKA  10/15/2001 Fix wrong calculation of the cancelled qty [End]
SELECT (lcCanAls)
=SEEK( "O"+m.corder+lcStyLNo)
*SCAN REST WHILE cOrdType+Order+STR(lineno,6) = "O"+m.corder FOR Style = lcStyle
SCAN REST WHILE cOrdType+Order+STR(lineno,6) = "O"+m.corder+lcStyLNo
  lnCanQty= lnCanQty + TotQty
ENDSCAN
*:B605032,1 AKA  10/15/2001 Fix wrong calculation of the cancelled qty[Start]
SELECT (lnArea) 
*:B605032,1 AKA  10/15/2001 Fix wrong calculation of the cancelled qty[End]
RETURN  lnCanQty
*--End of lfScanOrdC.

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
PRIVATE cType
STORE ""  TO cType
IF SEEK(Style,'Style') AND SEEK(Style.Fabric,'Fabric')
  cType = LEFT(ALLTRIM(gfCodDes(Fabric.cPurCode , 'cPurCode')),1)    
ENDIF
RETURN cType 
*--Enf of lfPurCode.

*:***************************************************************************
*: Name        : lfStyDesc
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 02/01/2001
*: Purpose     : 
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfStyDesc()
*:***************************************************************************
*
FUNCTION lfStyDesc
RETURN IIF (SEEK(Style,'Style'), Style.Desc , "") 
*--End of lfPurCode.

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
PARAMETERS m.cDockNo,m.cFactory,m.nDockQty
*B605604,1 (Begin) Don't make it private as it's used outside.
*PRIVATE lcPoNo  
*B605604,1 (End)


lcPoNo =  CutPick.cTktNo        
m.cDockNo = CutPick.cTktNo         && Var. that hold Docket No.

IF SEEK( "P" + lcPoNo  , "PosLn") AND SEEK(PosLn.Vendor, "ApVendor")
  m.cFactory  = ApVendor.cVenComp
ENDIF   
*B605604,1 (Begin) Always zero out this variable.
m.nDockQty = 0
*B605604,1 (End)

*-- For Docket Qty
IF SEEK( "P"  + lcPoNo ,'PosLn')
  SELECT PosLn
  SCAN REST WHILE cStyType+PO+Style+STR(lineno,6)+Trancd = "P"+lcPoNo  ;
            FOR Style = m.cMajor AND Trancd =  "1"
    m.nDockQty = m.nDockQty + TotQty
  ENDSCAN
ENDIF
*m.cDockNo,m.cFactory,m.nDockQty


*:*************************************************************
*: Name      : lfGetFabDt
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Function to get the Fabric Del Date.
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfGetFabDt()
*:*************************************************************
*:C200284,1 Add a Further Column to the report titled 
*:C200284,1 Fabric Del Date as a last column.
FUNCTION lfGetFabDt
PRIVATE ldFabDate , lnAlias , lcMajStyle
lnAlias    = SELECT (0)
ldFabDate  = {}
lcMajStyle = ''
*-- Variable hold the style major.
lcMajStyle = PADR(SUBSTR(Style,1,LnMajorlen),19)
*B605877,1 RAE
=SEEK(Style,'Style')
lnTmpCnt = IIF(Style.Make,ALEN(laMFabNo),ALEN(laIFabNo))
FOR I = 1 To lnTmpCnt
  *lcFabTypNo = STR(I,1)
  lcFabTypNo = IIF(Style.Make,ALLTRIM(laMFabNo[I]),ALLTRIM(laIFabNo[I]))
*B605877,1 RAE  
  *-- Bom Index = citmmajor+typ+citmmask+mfgcode+item+iclr
  IF SEEK(lcMajStyle+lcFabTypNo,'BOM')
    SELECT BOM
    *-- Scan in the file If the cost sheet contain more than 1 fabric.
    SCAN REST WHILE  citmmajor+typ+citmmask+mfgcode+item+iclr = lcMajStyle;
              FOR CcATGtYP="F"
      lcCurntFab =  LEFT(Bom.Item,7)
      *-- Check if this order assign to Material Po Screen.
      IF SEEK(Ordline.Order+lcCurntFab,'MAPOALO')
        SELECT MAPOALO
        SCAN WHILE Order + Fabric + Color = Ordline.Order + lcCurntFab
          *:B605877,1 RAE Commented out to remove the pofhdr conditoin.
          *IF SEEK('P'+MaPoAlo.Pomat,'POFHDR') .AND. PofHdr.Status = 'O'  .AND. POFHDR.Complete >= gdsysdate
          IF SEEK('P'+MaPoAlo.Pomat,'POFHDR') .AND. PofHdr.Status = 'O'  &&.AND. POFHDR.Complete >= gdsysdate
          *:B605877,1 RAE
            *-- Get the earliest complete date.
            ldFabDate = IIF(EMPTY(ldFabDate),POFHDR.Complete,MIN(ldFabDate,POFHDR.Complete))
          ENDIF
          SELECT MAPOALO
        ENDSCAN
      ENDIF
      SELECT BOM
    ENDSCAN
  ENDIF
ENDFOR

SELECt (lnAlias)
RETURN ldFabDate

*-- End OF lfGetFabDt
*:C200284,1 ABD - [End]
*:*************************************************************