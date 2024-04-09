*!***********************************************************************************
*! Name         : SOORCNDR.PRG
*! Developer    : NADER NABIL (NNA)
*! Date         : 01/10/2005
*! Module       : SO (Sales Order)
*! Purpose      : Custom Order Confirmation form for Direct Corporate Clothing (DIR03)
*! Reference    : C123848
*!***********************************************************************************
*! Called from  : Option Grid
*!***********************************************************************************
*! Calls        : ......
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return       : None
*!***********************************************************************************
*! Modifications:
*!***********************************************************************************

PRIVATE lnTargtFld,lcOrdKey,lnMaxSize,lcRltColor,lcFabColor
PUBLIC lcTargtFl,lcEmail,lcWebsite,lcPasPar
STORE 1 TO lnTargtFld,lnSorceFld
lcRltColor = SPACE(0)
lcFabColor = SPACE(0)
lcPasPar   = ' '
lnMaxSize  = 16								&& Max. No. of sizes per one line
lcEmail    = gfGetMemVar('M_CEMAIL')		&& Var. to hold the company email
lcWebsite  = gfGetMemVar('M_CWEBSITE')		&& Var. to hold the company website
lcTargtFl  = gfTempName()
lnFieldsNo = ALEN(laFilStruc,1)
IF !USED('FABRIC')
  =gfOpenFile(gcDataDir+'FABRIC','CFABRIC','SH')
ENDIF
IF !USED('EMPLOYEE')
  =gfOpenFile(gcDataDir+'EMPLOYEE','EMPLOYEE','SH')
ENDIF
=lfCrtTmp()

DECLARE laLngDesc[1,2]
laLngDesc[1,1] = 'CLRLNAME'
laLngDesc[1,2] = 'lcRltColor'

SELECT ORDHDR
SET RELATION TO ORDER INTO (lcEmplTmp) ADDITIVE
SET RELATION TO cordtype+ order INTO (lcTargtFl) ADDITIVE
SELECT (lcTargtFl)
SET FILTER TO TotQty != 0
SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO style INTO Style ADDITIVE

SELECT STYLE
SET RELATION TO Style.Fabric INTO Fabric ADDITIVE

lcOrdKey = ''

*--variable hold the complete date.[START]
lcCompDat = {}


lnStyleLen = lnMajorLen + lnColorLen +1
lcIndexKey = 'CORDTYPE + ORDER + STORE + SUBSTR(Style,1,lnStyleLen)'
*--if there is more than one order line with same style/colour/size/complete date/price
 *- it should accumulate into one entry 

=lfAccmulte()
=lfColData()
SELECT (lcTempOrd)
SET RELATION TO
lcSkipExpr  = [&lcTargtFl]
SELECT ORDHDR
SET SKIP TO &lcSkipExpr
lcRpExp = lcRpExp + ' AND !EOF(lcTempOrd)'
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp

llSalsOrd = .F.
IF USED('FABRIC')
  USE IN FABRIC
ENDIF
IF !USED('EMPLOYEE')
  USE IN EMPLOYEE
ENDIF

RETURN

*!***********************************************************************************
*! Name         : lfCrtTmp
*! Developer    : NADER NABIL (NNA)
*! Date         : 01/10/2005
*! Purpose      : Create Temp file
*! Reference    : C123848
*!***********************************************************************************
*! Called from  : Soorcndr.prg
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return       : None
*!***********************************************************************************
*! Example      : = lfCrtTmp()
*!***********************************************************************************
FUNCTION lfCrtTmp
*---Field to save sizes description
DIMENSION laFilStruc(lnFieldsNo+14,4)
laFilStruc(lnFieldsNo+1,1) = 'cSzesDesc'
laFilStruc(lnFieldsNo+1,2) = 'C'
laFilStruc(lnFieldsNo+1,3) = 105
laFilStruc(lnFieldsNo+1,4) = 0

*---Field to save sizes Price
laFilStruc(lnFieldsNo+2,1) = 'cSzesVal'
laFilStruc(lnFieldsNo+2,2) = 'C'
laFilStruc(lnFieldsNo+2,3) = 105
laFilStruc(lnFieldsNo+2,4) = 0

*---Field to save sizes QTY
laFilStruc(lnFieldsNo+3,1) = 'cSzesQty'
laFilStruc(lnFieldsNo+3,2) = 'C'
laFilStruc(lnFieldsNo+3,3) = 105
laFilStruc(lnFieldsNo+3,4) = 0

*---Field to save sizes total qty
laFilStruc(lnFieldsNo+4,1) = 'nSzesTotal'
laFilStruc(lnFieldsNo+4,2) = 'N'
laFilStruc(lnFieldsNo+4,3) = 8
laFilStruc(lnFieldsNo+4,4) = 0

*---Field to save sizes total value
laFilStruc(lnFieldsNo+5,1) = 'nSzToVl'
laFilStruc(lnFieldsNo+5,2) = 'N'
laFilStruc(lnFieldsNo+5,3) = 8
laFilStruc(lnFieldsNo+5,4) = 2

laFilStruc(lnFieldsNo+6,1) = 'cColorDes'
laFilStruc(lnFieldsNo+6,2) = 'C'
laFilStruc(lnFieldsNo+6,3) = 50
laFilStruc(lnFieldsNo+6,4) = 0

laFilStruc(lnFieldsNo+7,1) = 'cPURDESC'
laFilStruc(lnFieldsNo+7,2) = 'C'
laFilStruc(lnFieldsNo+7,3) = 30
laFilStruc(lnFieldsNo+7,4) = 0

laFilStruc(lnFieldsNo+8,1) = 'cFABCOLOR'
laFilStruc(lnFieldsNo+8,2) = 'C'
laFilStruc(lnFieldsNo+8,3) = 50
laFilStruc(lnFieldsNo+8,4) = 0

laFilStruc(lnFieldsNo+9,1) = 'lFIRSTREC'
laFilStruc(lnFieldsNo+9,2) = 'L'
laFilStruc(lnFieldsNo+9,3) = 1
laFilStruc(lnFieldsNo+9,4) = 0

*-- Field hold the style group data.
laFilStruc[lnFieldsNo+ 10 ,1] = 'StyGrop'
laFilStruc[lnFieldsNo+ 10 ,2] = 'C'
laFilStruc[lnFieldsNo+ 10 ,3] = 6
laFilStruc[lnFieldsNo+ 10 ,4] = 0

laFilStruc[lnFieldsNo+ 11 ,1] = 'COLOR'
laFilStruc[lnFieldsNo+ 11 ,2] = 'C'
laFilStruc[lnFieldsNo+ 11 ,3] = 30
laFilStruc[lnFieldsNo+ 11 ,4] = 0

laFilStruc[lnFieldsNo+ 12 ,1] = 'CDIM1'
laFilStruc[lnFieldsNo+ 12 ,2] = 'C'
laFilStruc[lnFieldsNo+ 12 ,3] = 5
laFilStruc[lnFieldsNo+ 12 ,4] = 0

laFilStruc[lnFieldsNo+ 13 ,1] = 'INDEXDIM'
laFilStruc[lnFieldsNo+ 13 ,2] = 'C'
laFilStruc[lnFieldsNo+ 13 ,3] = 2
laFilStruc[lnFieldsNo+ 13 ,4] = 0

laFilStruc(lnFieldsNo+14,1) = 'llPRNTSIZ'
laFilStruc(lnFieldsNo+14,2) = 'L'
laFilStruc(lnFieldsNo+14,3) = 1
laFilStruc(lnFieldsNo+14,4) = 0

CREATE CURSOR &lcTargtFl FROM ARRAY laFilStruc

IF lcRpSortDR = 'S'
  INDEX ON CORDTYPE + ORDER + STORE + STYLE TAG (lcTargtFl)
  SET ORDER TO TAG (lcTargtFl)
ELSE
  IF lcRpSortDR = 'G'
    INDEX ON CORDTYPE + ORDER + STORE + StyGrop + STYLE TAG (lcTargtFl)
    SET ORDER TO TAG (lcTargtFl)
  ELSE
    INDEX ON CORDTYPE + ORDER + STORE + STR(LINENO,6) TAG (lcTargtFl)
    SET ORDER TO TAG (lcTargtFl)
  ENDIF
ENDIF

*-- Temp File to Hold the Employee code,Name and Site for the Order
CREATE TABLE (gcWorkDir+lcEmplTmp)(ORDER C(6),EMPLCODE C(12),EMPLNAME C(30),EMPLSITE C(10))
INDEX ON (ORDER+EMPLCODE) TAG (lcEmplTmp) OF (lcEmplTmp)


*--End of function lfCrtTmp.
*!***********************************************************************************
*! Name         : lfEvalSegs
*! Developer    : NADER NABIL (NNA)
*! Date         : 01/10/2005
*! Purpose      : Get Color Length and Non major/free Length
*! Reference    : C123848
*!***********************************************************************************
*! Called from  : Option Grid
*!***********************************************************************************
*! Calls        : ......
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return       : None
*!***********************************************************************************
*! Example      : = lfEvalSegs()
*!***********************************************************************************

FUNCTION lfEvalSegs
PARAMETERS lcReturn
*-- Compute Free/Color Items in Style Structure.
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcNonMajTl = ''
lcNonMajPi = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
lcReturn = .T.
RETURN lcReturn
*-- end of lfEvalSegs.

*!***********************************************************************************
*! Name        : lfGtHedVar
*! Developer   : NADER NABIL (NNA)
*! Date        : 01/10/2005
*! Purpose     : Function to fill the approparate data for report header.
*! Reference   : C200435
*!***********************************************************************************
*! Called from : SORDCONDV.FRX [Header Band]
*!***********************************************************************************
*! Calls       : Procedures : ....
*!               Functions  : lfSolSpAdr
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return      : Null
*!***********************************************************************************
*! Example     : = lfGtHedVar()
*!***********************************************************************************
FUNCTION lfGtHedVar
PARAMETER LCRETURN , LCPARM
lcAlias = ALIAS()    && Save Current alias.
llEndGroup = .F.     && Start of new Group.
= lfGetlSpAdr()      && Call Function that fill header data [SoldTo and ShipTo]
SELECT (lcAlias)     && Restore before function alias.
RETURN ''
*-- end of lfHeadVar.

*!***********************************************************************************
*! Name        : lfGetlSpAdr
*! Developer   : NADER NABIL (NNA)
*! Date        : 01/10/2005
*! Purpose     : Function to Get the Sold to Address, Ship to Address,
*!             : the Description of the Ship Via, Season,
*!             : Special Instructions, and Terms.
*! Reference   : C123848
*!***********************************************************************************
*! Called from : lfHeadVar Function
*!***********************************************************************************
*! Calls       : Procedures : ....
*!               Functions  : gfRltFld, gfCodDes, gfGetAdr, lfAdrShift.
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return      : ....
*!***********************************************************************************
FUNCTION lfGetlSpAdr
lnSavAlias = SELECT(0)
lcStore = &lcTargtFl..Store

= gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

lcShipVia = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )
lcSeason  = gfCodDes(ORDHDR.Season  , 'SEASON'   )
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST'  )
lcTerms   = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')

SELECT CUSTOMER
IF ORDHDR.MULTI = 'Y'
  = SEEK('S' + &lcTargtFl..Account + &lcTargtFl..Store , "CUSTOMER")
  IF ALLTRIM(ORDHDR.ShipVia) = '*'
    lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
  ENDIF  
ENDIF
lcSolTName = BTName
lcShpTName = IIF(ORDHDR.Alt_ShpTo , ORDHDR.STName , IIF(EMPTY(DBA) , STName , DBA))
lcContactN = ORDHDR.CORDCONTAC
lcUserPhon = IIF(SEEK(gcUser_ID,'SYUUSER'),SYUUSER.CUSR_PHON,'')
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

= lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

*-- IF alternate ship to address
IF ORDHDR.Alt_ShpTo
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE    && Else
  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcStore = lcStore + '  Dist. Center : ' + Customer.Store
  ENDIF
  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

  IF TYPE('lcCurrKey') = 'C'
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF

ENDIF    && End of IF
= lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.
SELECT (lnSavAlias)
*-- end of lfSolSpAdr.

*!***********************************************************************************
*:* Name       : lfAccmulte
*! Developer   : NADER NABIL (NNA)
*! Date        : 01/10/2005
*:* Purpose    : When there is more than one order line with same 
*                 style/colour/size/complete date/price it should accumulate into one entry 
*!***********************************************************************************

FUNCTION lfAccmulte
PRIVATE lnSlct
lnSlct = SELECT()
SELECT (lcTempOrd)
lcNewTmp = gfTempName()
COPY STRUCTURE TO (gcWorkDir+lcNewTmp)
SELECT 0
USE (gcWorkDir+lcNewTmp) EXCL
INDEX ON CORDTYPE + ORDER + STORE + STYLE + DTOS(COMPLETE) + STR(PRICE,12,2) TAG &lcNewTmp 

SELECT (lcTempOrd)
LOCATE
SCAN
  SCATTER MEMVAR MEMO
  IF !SEEK(CORDTYPE+ORDER+STORE+STYLE+DTOS(COMPLETE)+STR(PRICE,12,2),lcNewTmp)
    INSERT INTO (gcWorkDir+lcNewTmp) FROM MEMVAR
  ELSE
    SELECT (lcNewTmp)
    REPLACE QTY1 WITH QTY1 + M.QTY1 ;
            QTY2 WITH QTY2 + M.QTY2 ;
            QTY3 WITH QTY3 + M.QTY3 ;
            QTY4 WITH QTY4 + M.QTY4 ;
            QTY5 WITH QTY5 + M.QTY5 ;
            QTY6 WITH QTY6 + M.QTY6 ;
            QTY7 WITH QTY7 + M.QTY7 ;
            QTY8 WITH QTY8 + M.QTY8
  ENDIF
ENDSCAN

SELECT (lcTempOrd)
ZAP
USE IN &lcNewTmp
APPEND FROM (gcWorkDir+lcNewTmp)
ERASE (gcWorkDir+lcNewTmp+'.DBF')
ERASE (gcWorkDir+lcNewTmp+'.CDX')
ERASE (gcWorkDir+lcNewTmp+'.FPT')

SELECT (lnSlct)
*-- end of lfAccmulte.

*!***********************************************************************************
*! Name        : lfColData
*! Developer   : NADER NABIL (NNA)
*! Date        : 01/10/2005
*! Purpose     : Collecting Data
*! Reference   : C123848
*!***********************************************************************************
*! Called from : Soorcndr.Prg
*!***********************************************************************************
*! Passed Parameters : None
*!***********************************************************************************
*! Return      : ....
*!***********************************************************************************
FUNCTION lfColData
PRIVATE lcEmplCode,lcEmplName,lcEmplSite , lnOldAlias,lcColorDsc,lcDimDesc,lcStyle,lcClrHld
STORE '' TO lcEmplCode,lcEmplName,lcEmplSite,lcColorDsc,lcDimDesc,lcStyle,lcClrHld
STORE 0 TO lnOldAlias
SELECT (lcTempOrd)
GO TOP
lcPurDesc = ''
SCAN
  IF !(SUBSTR(EVAL(lcTempOrd+'.Style'),lnNonMajSt,lnColorLen)==lcClrHld)
    lcClrHld = SUBSTR(EVAL(lcTempOrd+'.Style'),lnNonMajSt,lnColorLen)
    lcDimDesc = SCALE.CDIM1
  ENDIF
  IF !(STYLE.CSTYMAJOR ==lcStyle)
    lcStyle  = STYLE.CSTYMAJOR
    lcDimDesc = SCALE.CDIM1
  ENDIF
  IF lnTargtFld=1 
    lcDimDesc = SCALE.CDIM1
  ENDIF

  SCATTER MEMVAR MEMO
  = gfRltFld(SUBSTR(EVAL(lcTempOrd+'.Style'),lnNonMajSt,lnColorLen) , @laLngDesc , 'COLOR     ')
  lcColorDsc = gfCodDes(SUBSTR(EVAL(lcTempOrd+'.Style'),lnNonMajSt,lnColorLen), "COLOR")
  IF !EMPTY(STYLE.FABRIC) AND FABRIC.CPURCODE = '000271'
    DECLARE laFLngDesc[1,2]
    laFLngDesc[1,1] = 'CLRLNAME'
    laFLngDesc[1,2] = 'lcFabColor'
    = gfRltFld(FABRIC.COLOR , @laFLngDesc , 'COLOR     ')
    lcPurDesc = gfCodDes(Fabric.cpurcode,  'CPURCODE  ')
  ENDIF
  *--Get the Employee Code , Name and Site Ref.
  IF SEEK(&lcTempOrd..ACCOUNT+&lcTempOrd..STORE+&lcTempOrd..EMPLOYEE,'EMPLOYEE')
    SELECT EMPLOYEE
    lcEmplCode = EMPLOYEE
    lcEmplName = EMPNAME
    lcEmplSite = SITE_NO
  ELSE
    STORE '' TO lcEmplCode,lcEmplName,lcEmplSite
  ENDIF
  lnOldAlias = SELECT(0)
  IF !SEEK(&lcTempOrd..ORDER+&lcTempOrd..EMPLOYEE,lcEmplTmp)
    SELECT(lcEmplTmp)
    APPEND BLANK
    REPLACE ORDER     WITH &lcTempOrd..ORDER		,;
            EMPLCODE  WITH lcEmplCode 				,;
            EMPLNAME  WITH lcEmplName 				,;
            EMPLSITE  WITH lcEmplSite 
  ENDIF
  SELECT(lnOldAlias)
  *--If the complete date is different then add a new line.
  IF lcOrdKey # EVAL(lcIndexKey) OR lcCompDat # EVAL(lcTempOrd+'.COMPLETE')
    STORE 1 TO lnTargtFld
    SELECT (lcTargtFl)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cColorDes WITH lcRltColor 			,;
            cPURDESC  WITH lcPurDesc  			,;
            cFABCOLOR WITH lcFabColor 			,;
            lFIRSTREC WITH .T.        			,;
            llPRNTSIZ WITH .T.        			,;
            StyGrop   WITH STYLE.CSTYGROUP		,;
            COLOR     WITH lcColorDsc			,;
            CDIM1     WITH lcDimDesc			
            
            
    lnRecNo = RECNO()
    LOCATE										&& Refresh the relation
    GOTO lnRecNo
  ENDIF

  FOR lnCount = 1 TO SCALE.CNT
    lcSzQty = '.QTY' + ALLTRIM(STR(lnCOUNT))
    lcSzDes = '.SZ' + ALLTRIM(STR(lnCOUNT))
    IF lnTargtFld > lnMaxSize OR !(SCALE.CDIM1==lcDimDesc)
      IF lnTargtFld=1 OR !(SCALE.CDIM1==lcDimDesc)
        lcDimDesc = SCALE.CDIM1
      ENDIF
      STORE 1 TO lnTargtFld
      SELECT (lcTargtFl)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE cColorDes WITH lcRltColor 		,;
              cPURDESC  WITH lcPurDesc  		,;
              cFABCOLOR WITH lcFabColor 		,;
              lFIRSTREC WITH .F.        		,;
              llPRNTSIZ WITH .T.       			,;
              StyGrop   WITH STYLE.CSTYGROUP	,;
              COLOR     WITH lcColorDsc			,;
              CDIM1     WITH lcDimDesc

      lnRecNo = RECNO()
      LOCATE
      GOTO lnRecNo
    ENDIF
    lnSzQty = EVAL(lcTempOrd+lcSzQty)
    IF lnSzQty > 0
      SELECT (lcTargtFl)
      REPLACE cSzesQty   WITH RTRIM(cSzesQty)  + STR(lnSzQty,5)     ,;
              cSzesDesc  WITH RTRIM(cSzesDesc) + PADL(ALLTRIM(EVAL('SCALE'+lcSzDes)),5,' ')  ,;
              cSzesVal   WITH RTRIM(cSzesVal)  + STR(ROUND(EVAL(lcTempOrd+'.PRICE'),2),6,2),;
              nSzesTotal WITH nSzesTotal + lnSzQty,;
              nSzToVl    WITH nSzToVl    + (lnSzQty * EVAL(lcTempOrd+'.PRICE'))
      lnTargtFld = lnTargtFld +1
    ENDIF
  ENDFOR
  SELECT (lcTempOrd)
  STORE '' TO lcFabColor,lcPurDesc
  lcOrdKey = EVAL(lcIndexKey)

  *--Fill the complete variable with it's value.
  lcCompDat = EVAL(lcTempOrd+'.COMPLETE')
ENDSCAN

*--End of Function lfColData.