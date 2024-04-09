*:***************************************************************************
*: Program file  : MFLTCS
*: Program desc. : Print Lot Bill of Material
*: For Report    : MFPRTCSA.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Modules       : MF,PO,MA
*: Developer     : AHMED MOHAMMED MOHAMMED
*: Date          : 07/28/98
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetTitle(), lfStGroup(), lfEndGroup(), lfGetTit(), 
*:                 lfwOldVal(), lfvCont(), lfwOGWhen(), lfsrvTrans()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MFLTCS
*:***************************************************************************
*: Mod.    : 
*B602315,1 AMM 12/08/98 Fix the bug of collecting wrong cost items for each operation
*E301338,1 AMM 10/25/1999 Call PO bill of Material after printing the PO form
*:***************************************************************************
* Initialize variables
STORE SPACE(0) TO laCompAdd, lcStyTitle, lcShpName, laShpAdr,;
                  laOldVal, lcCostItm , lcHead1, lcHead2, lcOper
* Set the group expression of the .FRX
* (The .FRX work as lot cost sheet and cuttkt cost sheet)
lcMGroup = 'CUTTKT+cLotNo+cOperation'
*E301338,1 AMM Adjust the inner group of the .FRX
lcInGroup = 'CUTTKT+cLotNo+cOperation+Typ'
*E301338,1 AMM Use these variables instead of file fields to fit running on the PO BOM form
STORE SPACE(0) TO lcStyMaj ,lcPattrn,lcLotNo
lcBomTit = lcTitle
*E301338,1 AMM end
lcTime = Time()
llEndGroup = .F.
lcTemp = gfTempName()
lcMainF = gfTempName()

* Create the main report temporary file.
SELECT cTktBom
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,4]
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cLotNo'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 2
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cOperation'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0


Create Table (gcWorkDir+lcMainF) FROM ARRAY laFileStru

*E301338,1 AMM Adjust the Index
*INDEX ON CUTTKT+cOperation+cLotNo+Typ+cCatgTyp+Item+IClr TAG (lcMainF) 
INDEX ON CUTTKT+cLotNo+cOperation+Typ+cCatgTyp+Item+IClr TAG (lcMainF) 
*E301338,1 AMM end
INDEX ON cimtyp+cuttkt+cOperation+cLotNo+item+iclr+mfgcode TAG MAIN ADDITIVE

* Collect the lot lines in a temporary file
SELECT MFGOPRDT
COPY STRUCTURE TO (gcWorkDir+lcTemp)  
USE (gcWorkDir+lcTemp) IN 0

SELECT BOMLINE
SET RELATION TO cBomTyp+cimTyp+cTktNo+Item+IClr into BOMCOST ADDITIVE
SET RELATION TO cImTyp+cTktNo+Item+IClr INTO CTKTBOM ADDITIVE

Dimension laCost[10,2]
laCost[1,1]  = 'M_CMTYPE1 '
laCost[2,1]  = 'M_CMTYPE2 '
laCost[3,1]  = 'M_CMTYPE3 '
laCost[4,1]  = 'M_CMTYPE4 '
laCost[5,1]  = 'M_CMTYPE5 '
laCost[6,1]  = 'M_CMSLBL1 '
laCost[7,1]  = 'M_CMSLBL2 '
laCost[8,1]  = 'M_CMSLBL3 '
laCost[9,1]  = 'M_CMSLBL4 '
laCost[10,1] = 'M_CMSLBL5 '
=gfGetMemvar(@laCost,gcAct_Comp)

* Add the filter of status
IF lcStatus <> "L"
   lcRpExp = '(' +lcRpExp+ ") .AND. ("+lcFile+".Status = '"+lcStatus+"')"
ENDIF

* Set relation to either CUTTKTH, POSHDR or MMFGORDH
SELECT MFGOPRHD
DO CASE
  CASE lcImTyp = 'M' .OR. lcImTyp = 'T'
    SET RELATION TO cTktNo INTO (lcFile) ADDITIVE
  CASE lcImTyp = 'I'
    SET RELATION TO 'P'+cTktNo INTO (lcFile) ADDITIVE
ENDCASE

SCAN FOR &lcRpExp
  * Collect lots lines
  SELECT MFGOPRDT
  IF SEEK(lcImTyp+IIF(lcImTyp='M',CUTTKTH.cuttkt,IIF(lcImTyp='I',POSHDR.PO,MMFGORDH.cMFGOrdNo))+MFGOPRHD.cOprCode)
    SCAN WHILE cimTyp+cTktNo+cOprCode= lcImTyp+IIF(lcImTyp='M',;
            CUTTKTH.cuttkt,IIF(lcImTyp='I',POSHDR.PO,''))+MFGOPRHD.cOprCode ;
          FOR TranCD='1' AND IIF(EMPTY(lcrplot), CLOTNO = lcrplot,.T.)
      SCATTER MEMVAR
      INSERT INTO (lcTemp) FROM MEMVAR
    ENDSCAN
  ENDIF
ENDSCAN

SET RELATION TO

SELECT (lcTemp)
*lcTemp contains lines of the lot as in MFGOPRDT

SCAN
  SELECT BOMLINE

  =SEEK(&lcTemp..cImTyp+&lcTemp..cTktNo)
  *B602315,1 AMM Get cost items assigned to each operation.
  *SCAN WHILE cImTyp+cTktNo = lcImTyp+&lcTemp..cTktNo FOR cType+Style='1'+&lcTemp..Item .AND. cCatgTyp $ 'FTS'
  SCAN WHILE cImTyp+cTktNo = lcImTyp+&lcTemp..cTktNo ;
       FOR cType+Style='1'+&lcTemp..Item .AND. cCatgTyp $ 'FTS' ;
            .AND. cOprCode = &lcTemp..cOprCode
  *B602315,1 AMM end
    IF SEEK(cImTyp+cTktNo+&lcTemp..cOprCode+&lcTemp..cLotNo+Item+IClr,lcMainF)
      SELECT (lcMainF)
      REPLACE Req_Qty   WITH Req_Qty+ BOMLINE.UnitQty*&lcTemp..nLottotQty,;
              Issue_Qty WITH Issue_Qty+BOMCOST.nTotQty
    ELSE
      SELECT CTKTBOM
      SCATTER MEMVAR
      m.cLotNo    = &lcTemp..cLotNo
      m.cOperation  = &lcTemp..cOprCode
      m.Req_Qty   = BOMLINE.UnitQty*&lcTemp..nLottotQty
      m.Issue_Qty = BOMCOST.nTotQty
      INSERT INTO (lcMainF) FROM MEMVAR
    ENDIF
  ENDSCAN
ENDSCAN
*E301338,1 AMM Change the .FRX name to work as a form
*lcFormName = 'MFPRTCSA'
lcFormName = 'MFPRCSA'
*E301338,1 AMM end

lcOper   = gfCodDes(&lcMainF..cOperation, 'MFGCODE')
SELECT (lcMainF)
SET ORDER TO TAG (lcMainF) 
SET RELATION TO LEFT(item,7)+iclr INTO Fabric ADDITIVE
DO CASE
  CASE lcImTyp = 'M'
    SET RELATION TO CutTkt INTO CUTTKTH ADDITIVE
  CASE lcImTyp = 'I'
    SET RELATION TO 'P'+CutTkt INTO POSHDR ADDITIVE
  CASE lcImTyp = 'T'
    SET RELATION TO CutTkt INTO MMFGORDH ADDITIVE
ENDCASE

* Display the report
DO gfDispRe WITH (lcFormName)

USE IN (lcMainF)
ERASE (gcWorkDir+lcMainF)
USE IN (lcTemp)
ERASE (gcWorkDir+lcTemp)
  

*!*************************************************************
*! Name      : lfGetTitle
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Get title of each cost element group
*!*************************************************************
*! Called from : MFPRTCSA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetTitle()
*!*************************************************************
FUNCTION lfGetTitle

DO CASE
  CASE  &lcMainF..cCatgTyp = 'T'
    lcCostItm = laCost[ASCAN(laCost,cCatgTyp)+10]
    lcHead1   = '                  DATE                 QTY      DATE     DIRECT'
    lcHead2   = 'VENDOR  ORDERD PO #  ORDRD RECVD. SHIPD.'
  CASE  &lcMainF..cCatgTyp = 'F'
    lcCostItm = laCost[ASCAN(laCost,cCatgTyp)+10]
    lcHead1   = '[------------ PULLED ------------]  TOTAL'
    lcHead2   = ''
  CASE  &lcMainF..cCatgTyp = 'S'
    lcCostItm = laCost[ASCAN(laCost,cCatgTyp)+10]
    lcHead1   = ''
    lcHead2   = ''
ENDCASE
RETURN ''

*!*************************************************************
*! Name      : lfStGroup
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Initialize variables.
*!*************************************************************
*! Called from : MFPRTCSA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStGroup()
*!*************************************************************
FUNCTION lfStGroup
llEndGroup = .F.
RETURN ''
*!*************************************************************
*! Name      : lfEndGroup
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Set value of variable to don't print the word "Continued"
*!*************************************************************
*! Called from : MFPRTCSA.FRX
*!*************************************************************
*! Calls       : None
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
*! Name      : lfGetTit
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Get the title of the transaction to be printed
*!*************************************************************
*! Called from : MFPRTCSA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetTit()
*!*************************************************************
FUNCTION lfGetTit
PRIVATE lcTit

DO CASE
  CASE GCACT_APPL='PO'
    lcTit = 'Purchase Order'
  CASE GCACT_APPL='MA'
    lcTit = 'MFG Order'
  CASE GCACT_APPL='MF'
    lcTit    = ALLTRIM(gfGetMemvar('M_PRDLNLBL',gcAct_Comp))  
ENDCASE
lcTit  = IIF(RIGHT(lcTit,1) ='#', lcTit,lcTit+'#')
RETURN lcTit

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Get The old value
*!*************************************************************
*! Called from : MFOPMGA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))


*!*************************************************************
*! Name      : lfvCont
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Valid function of the contractor setting in the option grid
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : gfApVnBrow()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCont()
*!*************************************************************
FUNCTION lfvCont

PRIVATE lcVar, lcObj
lnAlias = SELECT(0)
lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(  ( EVALUATE(SYS(18)) )  )   && Varible to hold the current field value
SELECT APVENDOR
SET ORDER TO TAG VenCode 
*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) .AND. ('?' $ lcObj )
  IF !SEEK(lcObj , 'APVENDOR'))
    =gfApVnBrow(@lcObj)
    IF !EMPTY(lcObj)
      &lcVar = lcObj      && Update the field
    ELSE
      &lcVar = laOldVal
    ENDIF
  ENDIF
ELSE
  IF !SEEK(lcObj , 'APVENDOR'))
    *WAIT WINDOW "This contractor doesn't exist in the vendor file !!" 
    =gfModalGen('TRM00001B00000','DIALOG','This contractor')
  ENDIF

ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : When function of the option grid
*!*************************************************************
*! Called from : MFOPMGA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen

DO CASE
  CASE GCACT_APPL='MF'
    lcImTyp = 'M'
    lcFile  = 'CUTTKTH'
  CASE GCACT_APPL='PO'
    lcImTyp = 'I'
    lcFile  = 'POSHDR'
  CASE GCACT_APPL='MA'
    lcImTyp = 'T'
    lcFile  = 'MMFGORDH'
ENDCASE

*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : To set relation on or off when running the in range function 
*!             in the option grid.
*!*************************************************************
*! Called from : MFOPMG.PRG
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

*!*************************************************************
*! Name      : lfGetOp
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
*! Purpose   : Get Operation description
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetOp()
*!*************************************************************
FUNCTION lfGetOp

lcOper   = gfCodDes(&lcMainF..cOperation, 'MFGCODE')
*E301338,1 AMM Get variable to display on the .FRX
DO CASE
  CASE lcImTyp = 'M'
    lcStyMaj = CUTTKTH.style
    lcPattrn = CUTTKTH.pattern
ENDCASE
RETURN ''
*E301338,1 AMM end
