#INCLUDE R:\Aria4XP\Screens\AL\ALAUTAL.h

FUNCTION gfGetOptionGridVars

return "llRpGenPik,llRpGdExcl,lcMajorPic,lnRngAlias,lcSOrdStat,lcPOTlt,lcBrwFld,"+;
	   "llusedyes,LLALWFORCE, llRpPikSep, llRpPikCor, lcOldVal, LNDUMMYPOS, lcfree_clr, lcseason, lcdivision, lcstygroup, lcfabtlt, lcpattlt, lccolortlt, lcpattlt,laSlctDesc,laSlctVals,lcStyMajor"

FUNCTION gfInitOptionGridVars
lnRngAlias = 0
lcSOrdStat= ''
llRpGdExcl = .F.
llRpGenPik = .T.
lcPOTlt  = "Purchase order number    "
lcBrwFld = "PO :R :H= 'PO#    ', STATUS :R :H= 'Status' , Vendor :R :H= 'Vendor' ,"+;
           "APVENDOR.cVenComp :R :H= 'Name' , Entered  :R :H= 'Entered' , Complete :R :H= 'Complete' ,"+;
           "Open :R :H= 'Open' :P='999999' , POTOTAL  :R :H= 'PoTotal' :P='9999999999.99'"

lnSubtract = 0
lnSubtract = IIF('MF' $ oAriaApplication.CompanyInstalledModules,lnSubtract, lnSubtract + 1)
lnSubtract = IIF('PO' $ oAriaApplication.CompanyInstalledModules,lnSubtract, lnSubtract + 1)





STORE .T. TO llRpPikSep,llRpPikCor
LNDUMMYPOS = 0
llusedyes = .F.
LLALWFORCE = .F.

*-- Get Item Information
oGetItemMask = CREATEOBJECT('GetItemMask')
lcStyleTtl = oGetItemMask.Do("HI")
lcStylePct = oGetItemMask.Do("PI")
lnStyleWid = LEN(lcStylePct)
lcStyMajor = oGetItemMask.Do('HM')
lcMajorPic = oGetItemMask.Do('PM')
lnMajorLen = LEN(lcMajorPic)
lcSeason   = lcStyMajor + LANG_AutoAlloc_Season
lcDivision = lcStyMajor + LANG_AutoAlloc_Division
lcStyGroup = lcStyMajor + LANG_AutoAlloc_Group
lcFabTlt   = lcStyMajor + LANG_AutoAlloc_FabricCode
lcPatTlt   = lcStyMajor + LANG_AutoAlloc_Pattern
lnMajSeg   = oGetItemMask.Do('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]






DIMENSION laSlctDesc[6 - lnSubtract,1], laSlctVals[6 - lnSubtract,1]
laSlctDesc[1,1] = 'All'
laSlctDesc[2,1] = lcStyMajor
laSlctDesc[3,1] = 'Order'
laSlctDesc[4,1] = 'Account'
laSlctVals[1,1] = " "
laSlctVals[2,1] = "S"
laSlctVals[3,1] = "O"
laSlctVals[4,1] = "A"

DO CASE
  CASE lnSubtract = 0
    laSlctDesc[5,1] = 'Cutting ticket'
    laSlctDesc[6,1] = 'Purchase order'
  
    laSlctVals[5,1] = 'K'
    laSlctVals[6,1] = 'P'
  CASE lnSubtract = 1
    IF 'MF' $ oAriaApplication.CompanyInstalledModules
      laSlctDesc[5,1] = 'Cutting ticket'
      laSlctVals[5,1] = 'K'
    ELSE  
      laSlctDesc[5,1] = 'Purchase order'
      laSlctVals[5,1] = 'P'
    ENDIF
ENDCASE






DIMENSION laMajSegs[1,1]
oGetItemMask.Do(@laMajSegs)
STORE 0  TO lnNonMajSt
STORE "" TO lcNonMajPi,lcNonMajTl,lcFree_Clr
STORE .T. TO llCallScop , llFirstRun
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lnSupMajSt = lnNonMajSt
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

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = LANG_AUTOALLOC_COLROTITL1 + ALLTRIM(lcNonMajTl) + LANG_AUTOALLOC_COLROTITL2

FUNCTION lfwRepWhen
lnDummyPos = lfItmPos('llDummy')

=lfvIncHold()
=lfvExlBulk()

*!*************************************************************
*! Name      : lfvIncHold
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Include Hold Orders.
*!*************************************************************
FUNCTION lfvIncHold
lcSOrdStat = IIF(llRpIncHor,"OH","O")
*-- end of lfvIncHold.

*!*************************************************************
*! Name      : lfvExlBulk
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Exclude bulk order Orders.
*!*************************************************************
FUNCTION lfvExlBulk
lcBulkExp = IIF(llRpExlBlk,""," AND BULK='N'")
*-- end of lfvExlBulk.
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

FUNCTION lfwAloc
DIMENSION laSortAry[4,2]
IF lnOGSeting = 1
  laSortAry[1,1] = 1
  laSortAry[1,2] = [DTOS(COMPLETE)]
  laSortAry[2,1] = 2
  laSortAry[2,2] = [PRIORITY]
  laSortAry[3,1] = 3
  laSortAry[3,2] = [DTOS(START)]
  laSortAry[4,1] = 4
  laSortAry[4,2] = [ORDER]
  lnRpSort1 = 1
  lnRpSort2 = 2
  lnRpSort3 = 3
  lnRpSort4 = 4
  FOR lnCounter = 1 TO 4
    lcCounter = STR(lnCounter,1)
    =lfOGShowGet('lnRpSort'+lcCounter)
  ENDFOR
ENDIF

*--Get needed setups
DECLARE laSetups[8,2]
laSetups[1,1] = 'M_DYELOT'
laSetups[2,1] = 'M_MATDYE'
laSetups[3,1] = 'M_WareHouse'
laSetups[4,1] = 'M_FORCEALO'
laSetups[5,1] = 'M_TOTAVLBL'
laSetups[6,1] = 'M_CMPDOLN'
laSetups[7,1] = 'M_CHKAPROV'
laSetups[8,1] = 'M_STYCNFG'
=gfGetMemVar(@laSetups,oAriaApplication.ActiveCompanyID)

*B607815,1 WAM 10/31/2006 Initialize OG variable
llRpForAlo  = lfSuppForc(ALLTRIM(laSetups[4,2]))
*B607815,1 WAM 10/31/2006 (End)
IF llRpForAlo
  *-- Pick separates %
  IF llRpPikSep
    lnRpPikSep = 100
    =lfShowItem('lnRpPikSep')
  ENDIF

  *-- Pick coordinate %
  IF llRpPikCor
    lnRpPikCor = 100
    =lfShowItem('lnRpPikCor')
  ENDIF
  
  *-- cut-off units
  lnRpCutUnt = 0
  =lfShowItem('lnRpCutUnt')
  
  *-- allocate conditionally.

  IF (llUseDyes AND llFabDye)
    llRpCond = (lcRpSepCor='S')
    =lfShowItem('llRpCond')
  ENDIF

ENDIF

FUNCTION lfShowItem
PARAMETERS lcItem,llState    
PRIVATE lnItemPos
lnItemPos = ASCAN(laOGObjType,UPPER(lcItem))
IF lnItemPos > 0 
  lnItemPos = ASUBSCRIPT(laOGObjType,lnItemPos,1)
  laOGObjCnt[lnItemPos] = llState
ENDIF  
=lfOGShowGet(UPPER(lcItem))
*-- end of lfShowItem.

FUNCTION lfSuppForc
LPARAMETERS lcAlwForce

llAlwForce = .T.
IF lcAlwForce <> "Y"
  *-- No Force allocation done.
  IF lcAlwForce = "N"
    llRpForAlo = .F.
    llAlwForce = .F.  && Suppress line.
  ELSE  && User Prev.
    *-- Call user defined process.  
    llAlwForce = gfUserPriv('AL','ALAUTAL','FORCING')
  ENDIF
ENDIF
RETURN llAlwForce
*-- end of lfSuppForc.

FUNCTION lfwOldVal

lcOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value


FUNCTION lfMakeArrs

DIMENSION laIncExprs[1,ALEN(laOGFxFlt,2)],;
          laExcExprs[1,ALEN(laOGFxFlt,2)]
STORE '' TO laIncExprs,laExcExprs

lnJ = 1
FOR lnI = 1 TO ALEN(laOGFxFlt,1)
  lnJ = IIF(lnI = lnDummyPos,1,lnJ)
  IF !INLIST(lnI,lnDummyPos,lnDummyPos+1) AND !EMPTY(laOGFxFlt[lnI,6])
    IF (lnI < lnDummyPos) AND !EMPTY(laIncExprs[1,1])
      lnJ = lnJ + 1
      DIMENSION laIncExprs[lnJ,ALEN(laOGFxFlt,2)]
    ENDIF
    
    IF (lnI > lnDummyPos) AND !EMPTY(laExcExprs[1,1])
      lnJ = lnJ + 1
      DIMENSION laExcExprs[lnJ,ALEN(laOGFxFlt,2)]
    ENDIF

    lcSubArray = IIF(lnI < lnDummyPos,'laIncExprs','laExcExprs') 
    FOR lnK = 1 TO ALEN(laOGFxFlt,2)
      &lcSubArray[lnJ,lnK] = laOGFxFlt[lnI,lnK]
    ENDFOR

  ENDIF
ENDFOR

 
*-- end of lfMakeArrs.

FUNCTION lfvExclOrd

llRpGdExcl = !llRpGdExcl
lcRpExSlct = ' '  && Default select by all.

CLEARREAD()

*-- end of lfvExclOrd.

FUNCTION lfSrFab
PARAMETERS lcParm

SELECT ITEM
SET ORDER TO CSTYLE
LOCATE

*-- end of lfSrFab.

FUNCTION lfFabSum
LPARAMETERS lcFab, lccomp

LOCAL lnTotcomp, lnAlias
lnAlias   = SELECT(0)
lnTotcomp = 0

SELECT ITEM
lcLastOrd = ORDER()
SET ORDER TO CSTYLE
SUM &lcCOMP TO lnTotcomp WHILE CINVTYPE+STYLE = '0002'+lcFab
SET ORDER TO (lcLastOrd)

SELECT (lnAlias)
RETURN INT(lnTotcomp)

*-- end of lfFabSum.

FUNCTION lfvGenPktk

CLEARREAD()

*-- end of lfvGenPktk.

FUNCTION lfvPO

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field
lcObjVal = IIF(EMPTY(lcObjVal) , lcObjVal , PADL(ALLTRIM(lcObjVal) , 6 , '0'))

*-- IF The user want to Browse or if the PO number he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('PP' + lcObjVal , 'POSHDR1'))
  llObjRet = PosBrow(@lcObjVal , '' , 'P')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
ENDIF    && End of IF
&lcObjName = lcObjVal

*-- end of lfvPO.

FUNCTION lfwOldVal

lcOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value

*-- end of lfwOldVal.


FUNCTION lfvAccount

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*-- end of lfvAccount.

FUNCTION lfvScopMod

llClrSty1 = (lcRpScpMod # "S")
llClrOrd1 = (lcRpScpMod # "O")
llClrAcc1 = (lcRpScpMod # "A")

lcPOTlt  = IIF(lcRpScpMod = 'P',"Purchase order number    ","Cutting ticket number    ")
lcBrwFld = IIF(lcRpScpMod = 'P',"PO :R :H= 'PO#    ', STATUS :R :H= 'Status' , Vendor :R :H= 'Vendor' ,"+;
           "APVENDOR.cVenComp :R :H= 'Name' , Entered  :R :H= 'Entered' , Complete :R :H= 'Complete' ,"+;
           "Open :R :H= 'Open' :P='999999' , POTOTAL  :R :H= 'PoTotal' :P='9999999999.99'" ,;
           "PO :R :H='CutTkt#', STYLE :R :H='Style', STATUS :R :H='Status',"+;
           "ENTERED :R :H='Issue', COMPLETE :R :H='Complete', SEASON :R :H='Season',"+;
           "CDIVISION :R :H='Division'")
*NSTYORDER :R :H='Budget' :P = '999999',"+;
           "Receive :R :H='Received' :P = '999999', Damage :R :H='Damaged' :P = '999999', Open :R :H='Open' :P = '999999'")
CLEARREAD()

*-- end of lfvScopMod.

FUNCTION lfIncOnHld

*-- Clear read to Hide and unhide the option of order hold reason.
CLEARREAD()

*-- end of lfIncOnHld.

FUNCTION lfvSepCor
llRpPikSep = (lcRpSepCor $ 'BS')
llRpPikCor = (lcRpSepCor $ 'BC')
=lfwFilter()

*-- end of lfvSepCor.

FUNCTION lfvOGStyle

PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 

lcObjName = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field

*--IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))

  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.

  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal

ENDIF    && End of IF


SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)

*-- end of lfvOGStyle.

FUNCTION lfvOrder

PRIVATE lcObjNam , lcObjVal , llObjRet

lcObjNam = OGSYS18()      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(OGSYS18())      && Varible to hold  the value of the current GET field
lcObjVal = IIF(EMPTY(lcObjVal) , lcObjVal , PADL(ALLTRIM(lcObjVal) , 6 , '0'))

*-- IF The user want to Browse or if the Order number he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('O' + lcObjVal , 'ORDHDR'))
  llBrowse = .T.
  llObjRet = OrdBrowO(@lcObjVal , .F. , 'O')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  llBrowse = .F.
ENDIF    && End of IF
&lcObjNam = lcObjVal

*-- end of lfvOrder.


*!*************************************************************
*! Name      : lfvSlctExc
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Exclude select by validation.
*!*************************************************************
*! Called from : Selection OG
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : ....
*!*************************************************************
*! Return      : ....
*!*************************************************************
*! Example     : =lfvSlctExc()
*!*************************************************************
FUNCTION lfvSlctExc
lcRpExSlct = laOGFxFlt[lnDummyPos+1,6]

llClrSty2 = (lcRpExSlct # "S")
llClrOrd2 = (lcRpExSlct # "O")
llClrAcc2 = (lcRpExSlct # "A")


lcPOTlt  = IIF(lcRpExSlct = 'P',"Purchase order number    ","Cutting ticket number    ")
lcBrwFld = IIF(lcRpExSlct = 'P',"PO :R :H= 'PO#    ', STATUS :R :H= 'Status' , Vendor :R :H= 'Vendor' ,"+;
           "APVENDOR.cVenComp :R :H= 'Name' , Entered  :R :H= 'Entered' , Complete :R :H= 'Complete' ,"+;
           "Open :R :H= 'Open' :P='999999' , POTOTAL  :R :H= 'PoTotal' :P='9999999999.99'" ,;
           "PO :R :H='CutTkt#', STYLE :R :H='Style', STATUS :R :H='Status',"+;
           "ENTERED :R :H='Issue', COMPLETE :R :H='Complete', SEASON :R :H='Season',"+;
           "CDIVISION :R :H='Division'")
           

CLEARREAD()
*-- end of lfvSlctExc.

*!*************************************************************
*! Name      : lfsrPO1
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : PO In Range Filter. 1
*!*************************************************************
*!
FUNCTION lfsrPO1
PARAMETERS lcParm
=lfsrPO(lcParm,"1")
*-- end of lfsrPO1.

*!*************************************************************
*! Name      : lfsrPO2
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : PO In Range Filter. 2
*!*************************************************************
*!
FUNCTION lfsrPO2
PARAMETERS lcParm
=lfsrPO(lcParm,"2")
*-- end of lfsrPO2.

*!*************************************************************
*! Name      : lfsrPO
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : P/O In Range
*!*************************************************************
FUNCTION lfsrPO
PARAMETERS lcParm,lcFlagNo

IF lcRpScpMod = 'K'
  llClrPO&lcFlagNo = .F.
ELSE  
  DO CASE
    CASE lcParm = 'S'
      lnRngAlias = SELECT(0)
      SET ORDER TO VENCODE IN APVENDOR
      SELECT POSHDR
      SET RELATION TO POSHDR.vendor INTO Apvendor ADDITIVE
    CASE lcParm = 'R'
      SELECT POSHDR
      SET RELATION OFF INTO APVENDOR
      llClrPO&lcFlagNo = .F.
      SELECT (lnRngAlias)
  ENDCASE
ENDIF  
*-- end of lfsrPO.
*!*************************************************************
*! Name      : lfSrSty1
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Style In Range Filter. 1
*!*************************************************************
*!
FUNCTION lfSrSty1
PARAMETERS lcParm
=lfSRStyle(lcParm,"1")
*-- end of lfSrSty1.

*!*************************************************************
*! Name      : lfSrSty2
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Style In Range Filter. 2
*!*************************************************************
*!
FUNCTION lfSrSty2
PARAMETERS lcParm
=lfSRStyle(lcParm,"2")
*-- end of lfSrSty2.

*!*************************************************************
*! Name      : lfSRStyle
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Style In Range Filter.
*!*************************************************************
*!
FUNCTION lfSRStyle
PARAMETERS lcParm,lcFlagNo
DO CASE
  CASE lcParm = 'S'  && Set code
    lnRngAlias = SELECT(0)
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (oAriaApplication.DataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClrSty&lcFlagNo = .F.
    SELECT (lnRngAlias)
ENDCASE
*-- end of lfSRStyle.
*!*************************************************************
*! Name      : lfSrOrd1
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Order In Range Filter. 1
*!*************************************************************
*!
FUNCTION lfSrOrd1
PARAMETERS lcParm
=lfSROrder(lcParm,"1")
*-- end of lfSrOrd1.

*!*************************************************************
*! Name      : lfSrOrd2
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Order In Range Filter. 2
*!*************************************************************
*!
FUNCTION lfSrOrd2
PARAMETERS lcParm
=lfSROrder(lcParm,"2")
*-- end of lfSrOrd2.

*!*************************************************************
*! Name      : lfSROrder
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Order In Range
*!*************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm,lcFlagNo
DO CASE
  CASE lcParm = 'S'
    lnRngAlias = SELECT(0)
    SELECT ORDHDR
    lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.
    GO TOP
  
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    llClrOrd&lcFlagNo = .F.
    SELECT (lnRngAlias)

ENDCASE
*-- end of lfSROrder.
*!*************************************************************
*! Name      : lfsrAcc1
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Account In Range Filter. 1
*!*************************************************************
FUNCTION lfsrAcc1
PARAMETERS lcParm
=lfsrAcc(lcParm,"1")
*-- end of lfsrAcc1.

*!*************************************************************
*! Name      : lfsrAcc2
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Account In Range Filter. 2
*!*************************************************************
FUNCTION lfsrAcc2
PARAMETERS lcParm
=lfsrAcc(lcParm,"2")
*-- end of lfsrAcc2.

*!*************************************************************
*! Name      : lfsrAcc
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Account In Range
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm,lcFlagNo
DO CASE
  CASE lcParm = 'S'
    lnRngAlias = SELECT(0)
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClrAcc&lcFlagNo = .F.
    SELECT (lnRngAlias)
ENDCASE
*-- end of lfsrAcc.
*!*************************************************************
*! Name      : lfvIncWIP
*: Developer : HEND GHANEM (HBG)
*: Date      : 11/12/2003
*! Purpose   : Valid function to Include WIP option in Allocate O.G.
*!*************************************************************
*! Called from : Alocate O.G>
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Value of any type
*!*************************************************************
*! Return      : The passed value into String 
*!*************************************************************
*
FUNCTION lfvIncWIP

CLEARREAD()

FUNCTION lfvGenPik
CLEARREAD()