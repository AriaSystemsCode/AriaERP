**************************************************************************
*: Program file  : MAWEA600.PRG (C# 101498)
*: Program desc. : Custom material master list for Wear Wolf.
*:                 Convert WEA600 from 2.6 to 2.7
*: System        : Aria Apparel System
*: Developer     : AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: FUNCTIONS     : lfFormB,lfvVendor,lfvFabRng,lfvWare,lfRetCode,lfClearRep
*:                 lfClearRep,gfDispRe.
*:************************************************************************
*: Passed Parameters : None
*:************************************************************************
*
*--- Global Variable to Indicate if the selection criteria has been change,
*--- then recollect data.
*** LOOP FOR REPORT CRITERIA ***
XVENDOR    = lcRpVendor
XITEM_TYP  = lcRpItmType
XONHAND_Q  = lcRpOnHnd
lcWareCode = lcRpWare
lcFormat   = lcRpFormat
xFilter    = lcRpExp
IF !EMPTY(XVENDOR)
  xFilter = xFilter + ' AND FABRIC.VENDOR=XVENDOR'
ENDIF
IF !EMPTY(XITEM_TYP)
  xFilter = xFilter + ' AND FABRIC.ITEM_TYPE=XITEM_TYP'
ENDIF
IF XONHAND_Q = 'Y'
  xFilter = xFilter +' AND '+ IIF(!EMPTY(lcWarecode),'FABDYE.ONHAND <> 0',;
                                   'FABRIC.ONHAND <> 0')
ENDIF
IF !EMPTY(lcWareCode)
  xFilter = xFilter + " AND FABDYE.cWareCode = lcWareCode"
ENDIF
IF llOgFltCh
  IF FILE(gcWorkDir+lcRpTmpFab+'.DBF')
    USE IN IIF(USED(lcRpTmpFab),lcRpTmpFab,0)
    ERASE &gcWorkDir.&lcRpTmpFab+'.DBF'
  ENDIF
  SELECT DISTINCT Fabdye.fabric, Fabdye.color,Fabdye.cwarecode,;
      Fabdye.onhand, Fabric.costuse,;
      FABDYE.ONHAND*FABRIC.COSTUSE AS 'AVG_EXT', Fabric.item_type;
  FROM Fabdye,Fabric ;
  WHERE Fabdye.fabric+FABDYE.Color+FABDYE.Cwarecode+FABDYE.DYELOT =;
        Fabric.fabric + Fabric.Color;
        AND &xFilter;
  ORDER BY Fabdye.cwarecode, Fabric.item_type, Fabric.fabric,;
           Fabric.color;
  INTO DBF (gcWorkDir+lcRpTmpFab)
  IF _TALLY =0 
    =gfModalGen('TRM38155B00000','DIALOG' )
    RETURN
  ENDIF
 STORE '' TO lcP17CPI,lcP12CPI,lcP10CPI,lcPaprTyp
  IF lcFormat = "B"
   =lfFormB()
  ENDIF  
ENDIF
SELECT (lcRpTmpFab)
GOTO TOP
IF EOF()
  =gfModalGen('TRM38155B00000','DIALOG' )
  RETURN
ENDIF
DO gfDispRe WITH EVAL('lcRpForm')

*!*************************************************************
*! Name      : lfFormB
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 04/05/99
*! Purpose   : To collect the record for printing format B.
*!*************************************************************
*! Example   : lfFormB()
*!*************************************************************
FUNCTION lfFormB

lnReqQty = 0
SELECT cTktBom
SET KEY TO "I"
SELECT (lcRpTmpFab)
SCAN
  lnReqQty = 0
  IF SEEK("I"+PADR(Fabric,19)+Color+cWareCode,'cTktBom')
    SELECT cTktBom
    SCAN REST WHILE cIMTyp+Item+IClr+cWareCode+CutTkt = ;
                    "I"+PADR(&lcRpTmpFab..Fabric,19)+&lcRpTmpFab..Color+&lcRpTmpFab..cWareCode
      IF cTktBom.Used_Qty > 0 AND SEEK('P'+cTktBom.CutTkt,'PosHdr') AND;
                                  PosHdr.Open > 0

        IF SEEK("I"+"1"+cTktBom.CutTkt,'BomLine')
          SELECT BomLine
          SCAN REST WHILE CIMTYP+CTYPE+CTKTNO+STR(LINENO)+CBOMTYP+STYLE+SCLR+;
            IIF(!(CCATGTYP$"MDP"),ITEM,PADR(MFGCODE,19))+ICLR;
             = "I"+"1"+cTktBom.CutTkt;
             FOR Item+IClr = PADR(&lcRpTmpFab..Fabric,19)+&lcRpTmpFab..Color
            
            IF SEEK('P'+cTktBom.CutTkt+BomLine.Style+PADL(ALLTRIM(STR(BomLine.LINENO)),6)+'2','PosLn')
              lnReqQty = 0
              SELECT PosLn
              SUM REST TOTQTY TO lnRecPoQ WHILE cStyType+PO+STYLE+STR(LINENO,6)+TRANCD;
                   = 'P'+cTktBom.CutTkt+BomLine.Style+PADL(ALLTRIM(STR(BomLine.LINENO)),6)+'2'
              lnReqQty = lnReqQty + (lnRecPoQ * cTktBom.UntQty)
            ENDIF
          ENDSCAN  
        ENDIF
        SELECT (lcRpTmpFab)
        REPLACE OnHand  WITH OnHand+MAX((cTktBom.Used_Qty - lnReqQty),0),;
                Avg_Ext WITH OnHand*CostUse
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN
SELECT (lcRpTmpFab)
GOTO TOP
*!*************************************************************
*! Name      : lfvVendor
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 04/05/99
*! Purpose   : Vaildate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVendor()
*!*************************************************************
FUNCTION lfvVendor

SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcRPVendor) .AND.('?' $ lcRPVendor .OR. !SEEK(lcRPVendor , 'APVENDOR'))
  =gfApVnBrow(@lcRpVendor)
ENDIF

*!*************************************************************
*! Name      : lfvFabRng
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 04/05/99
*! Purpose   : To validate the fabric code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvFabRng()
*!*************************************************************
FUNCTION lfvFabRng

*-- this is the validation of from_item
lcFab = VARREAD()
lcFabVal = &lcFab.
IF !EMPTY(lcFabVal) AND (!SEEK(lcFabVal,'FABRIC'))
  SELECT FABRIC
  IF !SEEK(lcFabVal)
    DO FABROW WITH lcFabVal,'*'
    &lcFab = lcFabVal
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvWare
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 04/05/99
*! Purpose   : To validate the lfvWare code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfvWare()
*!*************************************************************
FUNCTION lfvWare

lcWarHous  = VARREAD()
lcWareCode = EVAL(lcWarHous)
SELECT WAREHOUS
IF !EMPTY(lcWareCode) .AND. !SEEK(lcWareCode , 'WAREHOUS')
  lcWareCode = gfBrowWare(.T.)  
ENDIF
&lcWarHous = lcWareCode

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 04/05/99
*! Purpose   : To validate the lfvWare code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTIO lfwRepWhen

R_WIDTH   = 'N'

*!*************************************************************
*! Name      : lfRetCode
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 04/05/99
*! Purpose   : To validate the lfvWare code.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfRetCode()
*!*************************************************************
FUNCTION lfRetCode

RETURN(gfCodDes(ITEM_TYPE,'ITEM_TYPE'))

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 04/05/99
*! Purpose   : Function to Clear temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.
*---Erase the temp file
IF lcRpFormat = 'B'
  USE IN IIF(USED(lcRpTmpFab),lcRpTmpFab,0)
  ERASE &gcWorkDir.&lcRpTmpFab+'.DBF'
ENDIF