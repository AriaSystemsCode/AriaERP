*:****************************************************************
*: Program file  : MAFBSTK.PRG
*: Program desc. : Rolls Entry Screen
*: System        : Aria Apparel System (A27).
*: Module        : Material (MA)
*: Developer     : Khalid Mohi El-Din Mohamed
*: Date          : 03/09/2003
*:****************************************************************
*:C#200472,1
*:****************************************************************
*Modifications
*B120017,1 TMI Fix the bug that the roll ID is duplicated in the Roll entry screen
*:***************************************************************************


PRIVATE lcKey , lnAdjStk ,lnAlias, ldTrDate ,;
                ldPostDate ,lcTrCode ,lcRollNP ,lnLineNo , lcRollTag


STORE ''  TO lcBatch, lcPieces, lcLoc
STORE 0   TO lnAlias

lcRollTag = ORDER("ROLLS")
SET ORDER TO Rolls IN ROLLS

IF lnQtyBuy = 0 .AND. !lfvQty()
  RETURN
ENDIF
lnAlias=SELECT(0)
lcBrTit = "Rolls"

lcTrType    = '1'
lcRollRP    = lcRemveP 
ldTrDate    = ldDate
ldPostDate  = IIF(llGlLink,ldPost,ldDate) &&-- Ask MAN

IF laRecType[lnRecType,2] = 'S'
  lcTrCode    = &lcTmpPoSp..POMAT
  lnLineNo    = &lcTmpPoSp..LineNo
ELSE
  lcTrCode    = &lcTmpPo..POMAT
  lnLineNo    = &lcTmpPo..LineNo
ENDIF  

lcFJlSess   = lcSession
lcWareCode  = laWare[lnWare,1]
llWareHous  = llMultiWH
lcToWare    = ''

IF TYPE('lcFQualty') $ 'LU'
  lcKey      = lcFabric + lcColor
  IF laRecType[lnRecType,2] = 'S'
    lnAdjStk   = &lcTmpPoSp..nfabtotqty
  ELSE
    lnAdjStk   = &lcTmpPo..nfabtotqty
  ENDIF
ELSE
  lcKey      =lcRetFab&lcFQualty+lcRetClr&lcFQualty
  lcFabric   =lcRetFab&lcFQualty
  lcColor    =lcRetClr&lcFQualty
  lnAdjStk   =lnDam&lcFQualty
ENDIF
=SEEK(lcKey,'FABRIC')

lnOldAdjStk = lnAdjStk
lnAdjStk    = IIF(lcKeyType  = 'P',lnAdjStk,lnAdjStk * -1) 
lcCostMeth  = lcMtCstMth
lnFabConv   = Fabric.Conv

lcFab  = lcFabric
lcClr  = lcColor
lcWare = laWare[lnWare,1]

STORE 0 TO lnOldVal,lnUsrApply
STORE lnAdjStk TO lnTotApply

*-- Create temporary file if it does not exist
=lfTmpRoll()

*-- Get the already applied quantity
SELECT (lcTmpRoll)
SET ORDER TO lcTmpRoll2
IF SEEK(lcFab+lcClr+lcWare)
  SUM nQtyBal TO lnUsrApply ;
      WHILE cRollItem+Color+cWareCode+cRollid = lcFab+lcClr+lcWare;
      FOR LineNo = lnLineNo
ENDIF
SET ORDER TO
LOCATE

lcEscInt = ON('KEY','ESC')
ON KEY LABEL ESC  DO lpCloseScr

PUSH KEY
DO (gcScrDir+gcWinAppl+'\MARLTDB.SPX')
POP KEY

ON KEY LABEL ESC      &lcEscInt
lnQtyBuy  = ABS(lnAdjStk)
  
STORE lnQtyBuy * lnFabConv TO lnQtyUse

SHOW GET lnQtyBuy
=lfRefresh(lcWinCh1)

lnTotQty1 = lnTotQty1 - lnOldAdjStk + lnQtyBuy

IF laRecType[lnRecType,2] = 'S'
  SELECT (lcTmpPoSp) 
ELSE
 SELECT (lcTmpPo) 
ENDIF

SET ORDER TO TAG &lcRollTag IN ROLLS

REPLACE nFabTotQty WITH lnQtyBuy
SELECT(lnAlias)
lcTmpRolls = lcTmpRoll

*!**************************************************************************
*! Name      : lfTmpRoll
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/18/2003
*! Purpose   : To create the temporary file if it does not exist
*!**************************************************************************
*! Example   : =lfTmpRoll()
*!**************************************************************************
FUNCTION lfTmpRoll
IF !USED(lcTmpRoll)
  lcTmpRoll  = gfTempName()

  DIMENSION laTags[3,3]
  laTags[1,1]='cRollID+cRollItem+Color+cWareCode+Dyelot+cRsession'
  laTags[1,2]=lcTmpRoll
  laTags[2,1]='cRollItem+Color+cWareCode+Dyelot+STR(LineNo,6)'
  laTags[2,2]='lcTmpRoll2'
  laTags[3,1]='cRsession+cRollItem+Color+cWareCode+Dyelot'
  laTags[3,2]='lcTmpRoll3'

  SELECT Rolls
  lnNewFld = AFields(laFileStru)
  DIMENSION laFileStru[ALEN(laFileStru,1)+22,4]

  laFileStru[lnNewFld+1,1] = 'cBatch'
  laFileStru[lnNewFld+1,2] = 'C'
  laFileStru[lnNewFld+1,3] = 8
  laFileStru[lnNewFld+1,4] = 0

  laFileStru[lnNewFld+2,1] = 'cPieces'
  laFileStru[lnNewFld+2,2] = 'C'
  laFileStru[lnNewFld+2,3] = 6
  laFileStru[lnNewFld+2,4] = 0

  laFileStru[lnNewFld+3,1] = 'cLoc'
  laFileStru[lnNewFld+3,2] = 'C'
  laFileStru[lnNewFld+3,3] = 4
  laFileStru[lnNewFld+3,4] = 0

  laFileStru[lnNewFld+4,1] = 'LineNo'
  laFileStru[lnNewFld+4,2] = 'N'
  laFileStru[lnNewFld+4,3] = 6
  laFileStru[lnNewFld+4,4] = 0

  laFileStru[lnNewFld+5,1] = 'cTrn_Seq'
  laFileStru[lnNewFld+5,2] = 'C'
  laFileStru[lnNewFld+5,3] = 6
  laFileStru[lnNewFld+5,4] = 0

  laFileStru[lnNewFld+6,1] = 'cFabric'
  laFileStru[lnNewFld+6,2] = 'C'
  laFileStru[lnNewFld+6,3] = 7
  laFileStru[lnNewFld+6,4] = 0


  laFileStru[lnNewFld+7,1] = 'cDyelot'
  laFileStru[lnNewFld+7,2] = 'C'
  laFileStru[lnNewFld+7,3] = 10
  laFileStru[lnNewFld+7,4] = 0

  laFileStru[lnNewFld+8,1] = 'cTran'
  laFileStru[lnNewFld+8,2] = 'C'
  laFileStru[lnNewFld+8,3] = 6
  laFileStru[lnNewFld+8,4] = 0

  laFileStru[lnNewFld+9,1] = 'cTranType'
  laFileStru[lnNewFld+9,2] = 'C'
  laFileStru[lnNewFld+9,3] = 1
  laFileStru[lnNewFld+9,4] = 0

  laFileStru[lnNewFld+10,1] = 'dTranDate'
  laFileStru[lnNewFld+10,2] = 'D'
  laFileStru[lnNewFld+10,3] = 8
  laFileStru[lnNewFld+10,4] = 0

  laFileStru[lnNewFld+11,1] = 'dPostDate'
  laFileStru[lnNewFld+11,2] = 'D'
  laFileStru[lnNewFld+11,3] = 8
  laFileStru[lnNewFld+11,4] = 0

  laFileStru[lnNewFld+12,1] = 'nUnitCost'
  laFileStru[lnNewFld+12,2] = 'N'
  laFileStru[lnNewFld+12,3] = 9
  laFileStru[lnNewFld+12,4] = 3

  laFileStru[lnNewFld+13,1] = 'nUntCstBuy'
  laFileStru[lnNewFld+13,2] = 'N'
  laFileStru[lnNewFld+13,3] = 9
  laFileStru[lnNewFld+13,4] = 3

  laFileStru[lnNewFld+14,1] = 'nBalance'
  laFileStru[lnNewFld+14,2] = 'N'
  laFileStru[lnNewFld+14,3] = 12
  laFileStru[lnNewFld+14,4] = 3

  laFileStru[lnNewFld+15,1] = 'nReceived'
  laFileStru[lnNewFld+15,2] = 'N'
  laFileStru[lnNewFld+15,3] = 12
  laFileStru[lnNewFld+15,4] = 3

  laFileStru[lnNewFld+16,1] = 'nIssued'
  laFileStru[lnNewFld+16,2] = 'N'
  laFileStru[lnNewFld+16,3] = 12
  laFileStru[lnNewFld+16,4] = 3

  laFileStru[lnNewFld+17,1] = 'nApply'
  laFileStru[lnNewFld+17,2] = 'N'
  laFileStru[lnNewFld+17,3] = 12
  laFileStru[lnNewFld+17,4] = 3

  laFileStru[lnNewFld+18,1] = 'cMarker'
  laFileStru[lnNewFld+18,2] = 'C'
  laFileStru[lnNewFld+18,3] = 1
  laFileStru[lnNewFld+18,4] = 0

  laFileStru[lnNewFld+19,1] = 'lStatus'
  laFileStru[lnNewFld+19,2] = 'C'
  laFileStru[lnNewFld+19,3] = 1
  laFileStru[lnNewFld+19,4] = 0

  laFileStru[lnNewFld+20,1] = 'lNeeded'
  laFileStru[lnNewFld+20,2] = 'L'
  laFileStru[lnNewFld+20,3] = 0
  laFileStru[lnNewFld+20,4] = 0

  laFileStru[lnNewFld+21,1] = 'RolTranCd'
  laFileStru[lnNewFld+21,2] = 'C'
  laFileStru[lnNewFld+21,3] = 1
  laFileStru[lnNewFld+21,4] = 0


  laFileStru[lnNewFld+22,1] = 'cColor'
  laFileStru[lnNewFld+22,2] = 'C'
  laFileStru[lnNewFld+22,3] = 6
  laFileStru[lnNewFld+22,4] = 0

  =gfCrtTmp(lcTmpRoll ,@laFileStru,@laTags)
ENDIF


*!**************************************************************************
*! Name      : lfBrows
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/18/2003
*! Purpose   : To browse the fields
*!**************************************************************************
*! Example   : =lfBrows()
*!**************************************************************************
FUNCTION lfBrows
PRIVATE lnAlias

lnAlias = SELECT()

SELECT (lcTmpRoll)
GOTO TOP
BROWSE FIELDS cRollId :30:H= 'Roll Id' :R,;
              nQtyBal :H='Roll balance':R;
     WINDOW    MARLTDB2  ;
     IN WINDOW MARLTDB;
     WHEN lfwBrow()    ;
     NOMENU            ;
     NOAPPEND          ;
     NODELETE          ;         
     NOWAIT            ;
     SAVE              ;
     NOCLEAR           ;
     NOEDIT            ;
     LOCK 0            ;
     FOR  cRollItem+Color+cWareCode+cRollid = lcFab+lcClr+lcWare AND LineNo = lnLineNo ;
          AND cTran = lcTrCode;
     TITLE lcBrTit

SHOW WINDOW (lcBrTit) REFRESH SAME
SELECT (lnAlias)
	
*!**************************************************************************
*! Name      : lfwBrow
*! Developer : Khalid Mohi El-Din Moahmed
*: Date      : 02/18/2003
*! Purpose   : To update the objects 
*!**************************************************************************
*! Example   : =lfwBrow()
*!**************************************************************************
FUNCTION lfwBrow

SELECT (lcTmpRoll)

lcBatch    = cBatch
lcPieces   = cPieces
lcLoc      = cLoc
lnAppRolQt = nQtyBal

SHOW GET lcBatch    ENABLE
SHOW GET lcPieces   ENABLE
SHOW GET lcLoc      ENABLE
SHOW GET lnAppRolQt ENABLE
IF !EOF()
  SHOW GET PbRLRem ENABLE
ENDIF

*!*************************************************************
*! Name      : lfvRLNew
*! Developer : KHALID MOHI EL-DIN
*! Date      : 03/10/2003
*! Purpose   : Valid function for new button
*!*************************************************************
*! Example            :  lfvRLNew()
*!*************************************************************
FUNCTION lfvRLNew

lnAppRolQt = 0
SELECT (lcTmpRoll)
lcBatch  = cBatch
lcPieces = cPieces
lcLoc    = cLoc

SHOW GET lcBatch  ENABLE
SHOW GET lcPieces ENABLE
SHOW GET lcLoc    ENABLE
SELECT (lcTmpRoll)
APPEND BLANK
REPLACE cRollItem   WITH lcFab          ,;
        Color       WITH lcClr          ,;
        cWareCode   WITH lcWare         ,;
        cBatch      WITH lcBatch        ,;
        cPieces     WITH lcPieces       ,;
        cLoc        WITH lcLoc		  ,;
        cRollId     WITH ALLTRIM(cBatch)+"-"+ALLTRIM(cPieces)+"-"+ALLTRIM(cLoc) ,;
        TranCd      WITH '1'            ,;
        cRSession   WITH lcFJlSess      ,;
        LineNo      WITH lnLineNo       ,;
		cFabric     WITH lcFab	        ,;
		cColor      WITH lcClr          ,;
		RolTranCd   WITH '1' 	        ,;
		cTran       WITH lcTrCode       ,;
        cTranType   WITH lcTrType       ,;
        dTranDate   WITH ldTrDate       ,;
        dPostDate   WITH ldPostDate
        
SHOW WINDOW (lcBrTit) REFRESH SAME
_CUROBJ = OBJNUM(lcBatch)
SHOW GET lnAppRolQt DISABLE

*!*************************************************************
*! Name      : lfvRLRem
*! Developer : KHALID MOHI EL-DIN
*! Date      : 03/10/2003
*! Purpose   : Valid function for remove button
*!*************************************************************
*! Example            :  lfvRLRem()
*!*************************************************************
FUNCTION lfvRLRem
PRIVATE lcObjSt

SELECT (lcTmpRoll)
lnUsrApply = lnUsrApply - lnAppRolQt
BLANK
DELETE
LOCATE
IF EOF()
  STORE '' TO lcBatch, lcPieces, lcLoc
  lnAppRolQt = 0
  lcObjSt = "DISABLE"
ELSE
  lcBatch    = cBatch
  lcPieces   = cPieces
  lcLoc      = cLoc
  lnAppRolQt = nQtyBal
  lcObjSt    = "ENABLE"
ENDIF
SHOW GET lcBatch    &lcObjSt
SHOW GET lcPieces   &lcObjSt
SHOW GET lcLoc      &lcObjSt
SHOW GET lnAppRolQt &lcObjSt
SHOW GET PbRLRem    &lcObjSt
=lfBrows()

=lfRefresh()
*SHOW WINDOW (lcBrTit) REFRESH SAME

*!*************************************************************
*! Name      : lfvRLClose
*! Developer : KHALID MOHI EL-DIN
*! Date      : 03/10/2003
*! Purpose   : Valid function for close button
*!*************************************************************
*! Example            :  lfvRLClose()
*!*************************************************************
FUNCTION lfvRLClose

*-- Check if the current entry is valid before closing the window.
*-- Check if the applied amount is equal to the amount specified 
*-- by the user, if not, confirm the new value from the user, 
*-- or return to the same screen for modifications.
SELECT (lcTmpRoll)
DO CASE
  CASE lnUsrApply  = 0
    IF gfModalGen('QRM36066B36001','ALERT') = 1
      *B120017,1  TMI [Start] Delete also all not needed records
      *DELETE ALL FOR nQtyBal <= 0
      DELETE ALL FOR nQtyBal <= 0 .AND. !LNEEDED
      *B120017,1  TMI [End  ] 
      LOCATE
      llRetVal = .F.
      CLEAR READ 
    ENDIF
  CASE lnUsrApply <> lnTotApply .AND. lnUsrApply # 0
    *Message Text : The quantity you have just applied does not equal 
    *Message Text : The total quantity you want to xxxxx , Would you 
    *Message Text : like to modify the applied quantity ?.    
    *Message No.  : 36173
    *Buttom  Text : YES ---- No 
    *Buttom  No.  : 36001

    lcMessage = 'receive'
    
    IF gfModalGen('QRM36173B36001','ALERT',lcMessage) = 1
      lnAdjStk = lnUsrApply
      SELECT (lcTmpRoll)
      *B120017,1  TMI [Start] delete all not needed records
      DELETE ALL FOR !LNEEDED
      *B120017,1  TMI [End  ] 
      LOCATE
      CLEAR READ
    ELSE
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
  OTHERWISE
    SELECT (lcTmpRoll)
    *B120017,1  TMI [Start] Delete all not needed records
    DELETE ALL FOR !LNEEDED
    *B120017,1  TMI [End  ] 
    LOCATE
    CLEAR READ  
ENDCASE

*!*************************************************************
*! Name      : lpCloseScr
*! Developer : KHALID MOHI EL-DIN
*! Date      : 03/10/2003
*! Purpose   : Valid the ESC key
*!*************************************************************
*! Example            :  DO lpCloseScr
*!*************************************************************
PROCEDURE lpCloseScr
=lfvRLClose()

*!*************************************************************
*! Name      : lfvLBatch
*! Developer : KHALID MOHI EL-DIN
*! Date      : 03/10/2003
*! Purpose   : Valid batch
*!*************************************************************
*! Example            :  lfvLBatch()
*!*************************************************************
FUNCTION lfvLBatch

SELECT (lcTmpRoll)
REPLACE cBatch    WITH lcBatch        ,;
        cRollId   WITH ALLTRIM(cBatch)+"-"+ALLTRIM(cPieces)+"-"+ALLTRIM(cLoc)

*!*************************************************************
*! Name      : lfvLPieces
*! Developer : KHALID MOHI EL-DIN
*! Date      : 03/10/2003
*! Purpose   : Valid Pieces
*!*************************************************************
*! Example            :  lfvLPieces()
*!*************************************************************
FUNCTION lfvLPieces

SELECT (lcTmpRoll)
REPLACE cPieces   WITH lcPieces       ,;
        cRollId   WITH ALLTRIM(cBatch)+"-"+ALLTRIM(cPieces)+"-"+ALLTRIM(cLoc)

*!*************************************************************
*! Name      : lfvLLoc
*! Developer : KHALID MOHI EL-DIN
*! Date      : 03/10/2003
*! Purpose   : Valid Location
*!*************************************************************
*! Example            :  lfvLLoc()
*!*************************************************************
FUNCTION lfvLLoc
PRIVATE lnRecNo, lcRollId, lnNoOfRoll

SELECT (lcTmpRoll)
REPLACE cLoc   WITH lcLoc       ,;
        cRollId   WITH ALLTRIM(cBatch)+"-"+ALLTRIM(cPieces)+"-"+ALLTRIM(cLoc)

lcRollId   = cRollId
lnRecNo    = RECNO()
lnNoOfRoll = 0

IF SEEK(lcRollId,'Rolls')
  =gfModalGen("QRM00000B00000",.F.,.F.,.F.,;
             "Roll #: " + ALLTRIM(cRollId) + " is already exist. Cannot proceed.")
  _CUROBJ = OBJNUM(lcBatch)
  RETURN
ENDIF

SET ORDER TO (lcTmpRoll)
IF SEEK(cRollId,lcTmpRoll)
  SELECT (lcTmpRoll)
  *LOCATE REST WHILE cRollid+cRollItem+Color+TranCd = lcRollId;
              FOR cRollItem+Color+cWareCode <> lcFab+lcClr+lcWare AND LineNo <> lnLineNo
  COUNT REST TO lnNoOfRoll WHILE cRollid+cRollItem+Color+TranCd = lcRollId

  *IF FOUND()
  IF lnNoOfRoll > 1
    =gfModalGen("QRM00000B00000",.F.,.F.,.F.,;
             "Roll #: " + ALLTRIM(lcRollId) + " is already assigned. Cannot proceed.")
    SET ORDER TO
    IF BETWEEN(lnRecNo,1,RECCOUNT())
      GOTO lnRecNo
      *B120017,1  TMI [Start] If this record is duplicated in the roll entry screen then delete it.
      *DELETE
      *B120017,1  TMI [End  ]       
    ENDIF
    SHOW WINDOW (lcBrTit) REFRESH SAME
    _CUROBJ = OBJNUM(lcBatch)
    RETURN    
  ENDIF
ENDIF

SELECT (lcTmpRoll)
IF SEEK(lcRollId+lcFab+lcClr)
  lnRecNo1 = RECNO()
  SET ORDER TO
  IF BETWEEN(lnRecNo,1,RECCOUNT()) AND lnRecNo1 <> lnRecNo
    GOTO lnRecNo
    DELETE
  ENDIF
  lnRecNo = lnRecNo1
ENDIF

SELECT (lcTmpRoll)
SET ORDER TO
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
  lnAppRolQt = nQtyBal
ENDIF
SHOW WINDOW (lcBrTit) REFRESH SAME
 
SHOW GET lnAppRolQt ENABLE
_CUROBJ = OBJNUM(lnAppRolQt)

*!*************************************************************
*! Name      : lfvRLApply
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/18/2003
*! Purpose   : Validate applied qty.
*!*************************************************************
*! Example            :  lfvRLApply()
*!*************************************************************
FUNCTION lfvRLApply

IF MDOWN() .AND. lnOldVal  = lnAppRolQt
  RETURN
ENDIF

SELECT (lcTmpRoll)
*B120017,1  TMI [Start] 
PRIVATE lnRecno,lcRollId
lnRecno = RECNO()
lcRollId = ALLTRIM(cBatch)+"-"+ALLTRIM(cPieces)+"-"+ALLTRIM(cLoc)
SET ORDER TO (lcTmpRoll)
=SEEK(lcRollId,lcTmpRoll)
COUNT REST TO lnNoOfRoll WHILE cRollid+cRollItem+Color+TranCd = lcRollId
SET ORDER TO
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
IF lnNoOfRoll > 1
  RETURN
ENDIF
*B120017,1  TMI [End  ] 
REPLACE nQtyBal WITH lnAppRolQt,;
        nQty    WITH lnAppRolQt,;
        nApply  WITH lnAppRolQt,;
        lNeeded WITH .T.         
lnUsrApply = lnUsrApply - lnOldVal + lnAppRolQt

=lfRefresh()


*!*************************************************************
*! Name      : lfwOldVals
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/18/2003
*! Purpose   : When fucntion for applied qty
*!*************************************************************
*! Example            :  lfwOldVals()
*!*************************************************************
FUNCTION lfwOldVals

lnOldVal  = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfReadAct
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/18/2003
*! Purpose   : The READ Activate function of MAEDROLL
*!*************************************************************
*! Example            :  lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

ON KEY LABEL CTRL+Q    
ON KEY LABEL CTRL+W    
ON KEY LABEL CTRL+HOME 
ON KEY LABEL CTRL+END  
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB 


*!*************************************************************
*! Name      : lfvDeact
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/18/2003
*! Purpose   : The READ Deactivate function of screen MAEDROLL
*!*************************************************************
*! Example            :  lfvDeact()
*!*************************************************************
FUNCTION lfLDeact

IF WONTOP() = lcBrTit
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB DO lpLTrapKey WITH 'MARLTDB3', 'PbRLNew'
  ON KEY LABEL BACKTAB DO lpLTrapKey WITH 'MARLTDB3', 'PbRLNew'
ENDIF

RETURN .F.

*!*************************************************************
*! Name      : lpTrapKey
*! Developer : KHALID MOHI EL-DIN
*! Date      : 02/18/2003
*! Purpose   : To handle the Trapping of keys
*!*************************************************************
*! Example            :  lpTrapKey()
*!*************************************************************
PROCEDURE lpLTrapKey
PARAMETERS lcWindName, lcObjName

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

*:**************************************************************************
*:* Name        : lfIsNeeded
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/11/2003
*:* Purpose     : Check if this record is needed
*:***************************************************************************
FUNCTION lfIsNeeded
PRIVATE lnRecno
lnRecno = RECNO()


*-- end of lfIsNeeded.