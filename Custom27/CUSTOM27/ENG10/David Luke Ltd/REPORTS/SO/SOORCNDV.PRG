*!*************************************************************
*! Name        : SOORCNDV.PRG
*! Developer   : Albert Raif (ALB)
*! Date        : 12/23/2002
*! Purpose     : Print the customization report for ENGLAND
*! Reference   : C200435
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : 
*!*************************************************************
*! Modifications:
*!B606847,1 BWA 1/5/2003 Fix the bug of calculating the same lines 
*!                  even if the complete date is difference.
*!B121171,1 TMI 02/29/2004 Accomulate same style/color/zise/complete date/price into one line
*!*************************************************************

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
*---Field to save sizes description
DIMENSION laFilStruc(lnFieldsNo+10,4)
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


CREATE CURSOR &lcTargtFl FROM ARRAY laFilStruc

DECLARE laLngDesc[1,2]
laLngDesc[1,1] = 'CLRLNAME'
laLngDesc[1,2] = 'lcRltColor'

IF lcRpSortDV = 'S'
  INDEX ON CORDTYPE + ORDER + STORE + STYLE TAG (lcTargtFl)
  SET ORDER TO TAG (lcTargtFl)
ELSE
  IF lcRpSortDV = 'G'
    INDEX ON CORDTYPE + ORDER + STORE + StyGrop + STYLE TAG (lcTargtFl)
    SET ORDER TO TAG (lcTargtFl)
  ELSE
    INDEX ON CORDTYPE + ORDER + STORE + STR(LINENO,6) TAG (lcTargtFl)
    SET ORDER TO TAG (lcTargtFl)
  ENDIF
ENDIF
SELECT ORDHDR
SET RELATION TO cordtype+ order INTO (lcTargtFl) ADDITIVE
SELECT (lcTargtFl)
SET FILTER TO TotQty != 0
SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO style INTO Style ADDITIVE

SELECT STYLE
SET RELATION TO Style.Fabric INTO Fabric ADDITIVE

lcOrdKey = ''

*B606847,1 BWA 1/5/2003 variable hold the complete date.[START]
lcCompDat = {}
*B606847,1 BWA 1/5/2003.[END]

lnStyleLen = lnMajorLen + lnColorLen +1
lcIndexKey = 'CORDTYPE + ORDER + STORE + SUBSTR(Style,1,lnStyleLen)'
*B121171,1   TMI [Start] when there is more than one order line with same 
*           style/colour/size/complete date/price it should accumulate into one entry 
=lfAccmulte()
*B121171,1   TMI [End  ] 
SELECT (lcTempOrd)
GO TOP
lcPurDesc = ''

SCAN
  SCATTER MEMVAR MEMO
  = gfRltFld(SUBSTR(EVAL(lcTempOrd+'.Style'),lnNonMajSt,lnColorLen) , @laLngDesc , 'COLOR     ')
  IF !EMPTY(STYLE.FABRIC) AND FABRIC.CPURCODE = '000271'
    DECLARE laFLngDesc[1,2]
    laFLngDesc[1,1] = 'CLRLNAME'
    laFLngDesc[1,2] = 'lcFabColor'
    = gfRltFld(FABRIC.COLOR , @laFLngDesc , 'COLOR     ')
    lcPurDesc = gfCodDes(Fabric.cpurcode,  'CPURCODE  ')
  ENDIF

  *B606847,1 BWA 1/5/2003 If the complete date is differ then add a new line.[START]
  *IF lcOrdKey <> EVAL(lcIndexKey)				&&if new style color
  IF lcOrdKey # EVAL(lcIndexKey) OR lcCompDat # EVAL(lcTempOrd+'.COMPLETE')
  *B606847,1 BWA 1/5/2003.[END]

    STORE 1 TO lnTargtFld
    SELECT (lcTargtFl)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cColorDes WITH lcRltColor ,;
            cPURDESC  WITH lcPurDesc  ,;
            cFABCOLOR WITH lcFabColor ,;
            lFIRSTREC WITH .T.        ,;
            StyGrop   WITH STYLE.CSTYGROUP
    lnRecNo = RECNO()
    LOCATE										&& Refresh the relation
    GOTO lnRecNo
  ENDIF
  FOR lnCount = 1 TO 8
    lcSzQty = '.QTY' + ALLTRIM(STR(lnCOUNT))
    lcSzDes = '.SZ' + ALLTRIM(STR(lnCOUNT))
    IF lnTargtFld > lnMaxSize			&&  if complete the avilable Max. Sizes
      STORE 1 TO lnTargtFld
      SELECT (lcTargtFl)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE cColorDes WITH lcRltColor ,;
              cPURDESC  WITH lcPurDesc  ,;
              cFABCOLOR WITH lcFabColor ,;
              lFIRSTREC WITH .F.        ,;
              StyGrop   WITH STYLE.CSTYGROUP

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

  *B606847,1 BWA 1/5/2003 Fill the complete variable with it's value.[START]
  lcCompDat = EVAL(lcTempOrd+'.COMPLETE')
  *B606847,1 BWA 1/5/2003.[END]

ENDSCAN
SELECT (lcTempOrd)
SET RELATION TO
lcSkipExpr  = [&lcTargtFl]
SELECT ORDHDR
SET SKIP TO &lcSkipExpr
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Albert Raif (ALB)
*! Date      : 12/12/2002
*! Purpose   : Get Color Length and Non major/free Length
*! Reference : C200435
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************

FUNCTION lfEvalSegs
PARAMETERS lcReturn
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcNonMajTl = ''
lcNonMajPi = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure. [Begin]
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
*-- Compute Free/Color Items in Style Structure. [End]
lcReturn = .T.
RETURN lcReturn
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfGetlSpAdr
*! Developer : Albrt Raif (ALB)
*! Date      : 12/23/2002
*! Purpose   : Function to fill the approparate data for report header.
*! Reference : C200435
*!*************************************************************
*! Called from : SORDCONDV.FRX [Header Band]
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : lfSolSpAdr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGtHedVar()
*!*************************************************************
FUNCTION lfGtHedVar
PARAMETER LCRETURN , LCPARM
lcAlias = ALIAS()    && Save Current alias.
llEndGroup = .F.     && Start of new Group.
= lfGetlSpAdr()      && Call Function that fill header data [SoldTo and ShipTo]
SELECT (lcAlias)     && Restore before function alias.
RETURN ''
*-- end of lfHeadVar.

*!*************************************************************
*! Name      : lfGetlSpAdr
*! Developer : Albrt Raif (ALB)
*! Date      : 12/23/2002
*! Purpose   : Function to Get the Sold to Address, Ship to Address,
*!           : the Description of the Ship Via, Season,
*!           : Special Instructions, and Terms.
*! Reference : C200435
*!*************************************************************
*! Called from : lfHeadVar Function
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : gfRltFld, gfCodDes, gfGetAdr, lfAdrShift.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
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

*:**************************************************************************
*:* Name        : lfAccmulte
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/29/2004
*:* Purpose     : When there is more than one order line with same 
*                 style/colour/size/complete date/price it should accumulate into one entry 
*:***************************************************************************
*B121171,1 

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

*B121171,3   TMI [Start] go to 1st record in the lcNewTmp file
USE IN &lcNewTmp
*B121171,3   TMI [End  ] 
APPEND FROM (gcWorkDir+lcNewTmp)
*B121171,1   TMI [Start] commect the following line
*USE IN (lcNewTmp) 
*B121171,1   TMI [End  ] 
ERASE (gcWorkDir+lcNewTmp+'.DBF')
ERASE (gcWorkDir+lcNewTmp+'.CDX')
ERASE (gcWorkDir+lcNewTmp+'.FPT')

SELECT (lnSlct)
*-- end of lfAccmulte.

