*!**************************************************************************
*! Name      : ARIAMAIN.PRG
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/21/2000
*! Purpose   : ARIA'S payment schedule Custom Process Program.
*! Reference : #C101772,1
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue


*!***************************************************************
*! Name      : lfADDBAR
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/21/2000
*! Purpose   : Add a bar called 'Payment Schedule' in Options menu.
*!***************************************************************
*! Event     : 'ADDBAR'     -> Add a bar called 'Payment Schedule'.
*!***************************************************************
*!
FUNCTION lfADDBAR
 
PRIVATE lcAlias
lcAlias = ALIAS()

DEFINE BAR 17 OF _INQURYPOP PROMPT 'Paymant Sche\<dule'
*-- Also create the temp file.
CREATE TABLE (gcWorkDir+lcPayTmp) (LineNo N(6), Order C(6), Percent N(5,2), Amount N(9,2), Desc C(60),cSele C(1),nRecNo N(6))
INDEX ON LineNo TAG (lcPayTmp)
=gfOpenFile(gcDataDir+'BOMVAR',gcDataDir+'BOMVAR','SH')
SELECT ORDHDR
SET RELATION TO 'PY'+ORDER INTO BOMVAR ADDI
SELECT IIF(!EMPTY(lcAlias),lcAlias,ALIAS())

*!***************************************************************
*! Name      : CALLSCRE
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/21/2000
*! Purpose   : Show 'Payment Schedule' screen
*!***************************************************************
*! Event     : 'CALLSCRE'     -> Show 'Payment Schedule' screen.
*!***************************************************************
*!
FUNCTION lfCALLSCRE

PRIVATE lcAlias
lcAlias = ALIAS()

*--If Add mode or edit mode and the user didn't add Booked qty (One record in lcOrdHdr file at least) or Edit mode and 
*--the user removed all lines so we have no Book Amount to restrict Payments on it
IF (laScrMode[3] OR laScrMode[4]) AND (RECCOUNT(lcOrdHdr) = 0 OR (&lcOrdHdr..BookAmt-&lcOrdHdr..CancelAmt=0))
  *--No Booked amount has been entered for this order yet.
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"No Booked amount has been entered for this order yet.")
  RETURN
ENDIF

*-- Variable to store the old value of Percent or Amount field.
lnOldVal   = 0
lnBalance  = 0
*--Save the booked amonunt (Always extract Cnacel amount from book amount.(From temp file when adding or editing and from OrdHdr otherwise))
lnBookAmnt = IIF(laScrMode[3] OR laScrMode[4],(&lcOrdHdr..BookAmt-&lcOrdHdr..CancelAmt),(OrdHdr.BookAmt-OrdHdr.CancelAmt))
=SEEK('PY'+ORDHDR.ORDER,'BOMVAR')
*--If it's not Select or Add mode.
IF laScrMode[2] OR laScrMode[3]
  *--If this order has payments and it is the first time to call schedule screen in this session.
  IF !EOF('BOMVAR') AND RECCOUNT(lcPayTmp) = 0
    *--Zap temp file.
    SELECT (lcPayTmp)
    ZAP
    lcKey = 'PY'+ORDHDR.ORDER
    SELECT BOMVAR
    SET MEMOWIDTH TO 60
    lnBalance = 0
    SCAN WHILE cidtype+ccost_id+STR(lineno,6) = lcKey
      INSERT INTO (lcPayTmp) (LineNo,Order,Percent,Amount,Desc,cSele,nRecNo);
                    VALUES  (BOMVAR.LineNo,BOMVAR.cCost_ID,BOMVAR.nPercent,BOMVAR.TotCost,MLINE(BOMVAR.mSizes,1),'S',RECNO('BOMVAR'))
      lnBalance = lnBalance + TotCost
    ENDSCAN
    lnBalance   = lnBookAmnt - lnBalance
    SELECT (lcPayTmp)
  ENDIF
ENDIF
SELECT (lcPayTmp)
*--If the user presses the new order button.
IF laScrMode[4] AND RECCOUNT(lcOrdHdr) = 0 AND RECCOUNT(lcOrdLine) = 0
  ZAP
ENDIF
*--If the user added new payments and we are in ADD mode
IF RECCOUNT(lcPayTmp) > 0 AND lnBalance = 0
  SUM AMOUNT TO lnPayAmt FOR cSele <> 'D'
  lnBalance   = lnBookAmnt - lnPayAmt
ENDIF
*--If the order does not have any payments,then the balance is equal to the book amount.
lnBalance = IIF(lnBalance = 0 AND RECCOUNT(lcPayTmp) = 0,lnBookAmnt,lnBalance)
lcBrTitle = 'Payments Schedule'
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTitle)
lcEscTr = ON("KEY","ESC")
ON KEY LABEL ESCAPE DO lpClosPay
lcRemStat = IIF(RECCOUNT(lcPayTmp)>0,'ENABLE','DISABLE')
DO (gcScrDir+gcWinAppl+"\SOPAYSCH.SPX")
ON KEY LABEL ESCAPE &lcEscTr
*ADEL THis flag is to determine whether we will update Customer Pofile files or not.
llOpenOrd = .F.

SELECT IIF(!EMPTY(lcAlias),lcAlias,ALIAS())

*!***************************************************************
*! Name      : lfWhenFun
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/21/2000
*! Purpose   : When function for the screen.
*!***************************************************************
FUNCTION lfWhenFun

SELECT (lcPayTmp)
GO TOP
lnMarker   = RECNO()
lcFieldsBr = [LineNo  :H='No' :W= lfWCanMod() :V= lfvLineNo(),]+;
             [DESC    :H='Description' :W= lfWCanMod() :V= lfvDesc() ,] +;
             [Percent :H='Percent. % ' :W= lfwPrOrAmt('P') :V= lfvPercent(),] +;
             [Amount  :H='Amount' :W= lfwPrOrAmt('A') :V= lfvPayAmnt() ]
BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F., &lcFieldsBr;
       WINDOW SOPAYSC1    ;
       IN WINDOW SOPAYSCH ;
       NOAPPEND           ;
       NODELETE           ;         
       NOCLEAR            ;
       NOMENU             ;         
       NOWAIT             ;
       SAVE               ;
       WHEN lfBrowWh()    ;
       FOR cSele <> 'D'   ;
       TITLE lcBrTitle

*!***************************************************************
*! Name      : lfvNewPay
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/21/2000
*! Purpose   : Validate New buton.
*!***************************************************************
FUNCTION lfvNewPay

SELECT (lcPayTmp)
SUM Amount to lnPaid FOR cSele <> 'D'
IF lnPaid = lnBookAmnt
  *--No balane remaining for any new payments!
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"No balance remaining for any new payments.")
  RETURN
ENDIF
*--Delete any record with amount  = 0
LOCATE FOR (Percent = 0 OR Amount = 0)
IF FOUND()
  *--'There is a pamyent with zero amount. This pamyent will be ignored !'
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"There is a pamyent with zero amount. This pamyent will be ignored !")
  REPLACE DESC WITH ''
  lnMarker   = RECNO()
  _CUROBJ = OBJNUM(LineNo)
  SHOW WINDOW (lcBrTitle) REFRESH SAME
  RETURN
ENDIF
*--Get the last record by this way to avoid setting order DESC to avoid its effect on the browse.
GO BOTT
IF cSele = 'D' AND !EOF()
  DO WHILE !BOF()
    SKIP-1
    IF cSele <> 'D'
      EXIT
    ENDIF
  ENDDO
ENDIF
lnPayNo = IIF(BOF(),1,LineNo+1)
APPEND BLANK
*-- The new added record will have the sataus 'N'
REPLACE LineNo WITH lnPayNo,;
        cSele  WITH 'N',;
        order  WITH OrdHdr.Order
lnMarker   = RECNO()
_CUROBJ = OBJNUM(LineNo)
SHOW WINDOW (lcBrTitle) REFRESH SAME

*!***************************************************************
*! Name      : lfvRemPay
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/21/2000
*! Purpose   : Validate Remove buton.
*!***************************************************************
FUNCTION lfvRemPay

SELECT (lcPayTmp)
*--  Are you sure you want to remove this payment?
IF gfModalGen('TRM00000B32000',.F.,.F.,.F.,"Are you sure you want to remove this payment?") =1
  IF cSele = 'N'
    DELETE
  ELSE
    REPLACE cSele WITH 'D'
  ENDIF
  LOCATE FOR cSele <> 'D'
  IF EOF()
    SHOW GET pbRemove DISABLE
  ENDIF
  SUM Amount to lnPaid FOR cSele <> 'D'
  lnBalance = lnBookAmnt - lnPaid
  SHOW WINDOW (lcBrTitle) REFRESH SAME
  =lfRefresh('SOPAYSCH')
ENDIF

*!***************************************************************
*! Name      : lfBrowWh
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Validate When browse.
*!***************************************************************
FUNCTION lfBrowWh

lnMarker = RECNO()
SHOW WINDOW (lcBrTitle) REFRESH SAME

*!***************************************************************
*! Name      : lfActiW
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : READ Activation function.
*!***************************************************************
FUNCTION lfActiW

ON KEY LABEL CTRL+Q 
ON KEY LABEL CTRL+W 
ON KEY LABEL CTRL+HOME 
ON KEY LABEL CTRL+END  
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB
ON KEY LABEL ESCAPE

*!***************************************************************
*! Name      : lfdActiW
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : READ Deactivation function.
*!***************************************************************
FUNCTION lfdActiW

IF WONTOP() =  lcBrTitle
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB DO lpTabKey
  ON KEY LABEL BACKTAB DO lpBackTabk
  ON KEY LABEL ESCAPE DO lpClosPay
ENDIF
RETURN .F.

*!***************************************************************
*! Name      : lpBackTabk
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Trap of tab SHIFT+key.
*!***************************************************************
PROCEDURE lpBackTabk

ON KEY LABEL TAB 
ACTIVATE WINDOW sopaysc2
_CUROBJ = OBJNUM(pbclose)

*!***************************************************************
*! Name      : lpTabKey
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Trap of tab key.
*!***************************************************************
PROCEDURE lpTabKey

ON KEY LABEL TAB 
ACTIVATE WINDOW sopaysc2
_CUROBJ = OBJNUM(pbNew)

*!***************************************************************
*! Name      : lfshowBut
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Show function.
*!***************************************************************
FUNCTION lfshowBut

*-- If the status of the order is cancel or completem don't allow the user to add or remove any payment       
lcNewStat = IIF(!INLIST(ORDHDR.STATUS,'X','C') AND !laScrMode[2] ,'ENABLE','DISABLE')
SHOW GET pbNew     &lcNewStat
*--If this is a cencelled or completed order or View mode, disable it, otherwise don't change the status.
lcRemStat = IIF(lcNewStat = 'DISABLE',lcNewStat,lcRemStat)
SHOW GET pbRemove  &lcRemStat
=lfRefresh()

*!***************************************************************
*! Name      : lfvPercent
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Validate Percent filed in the browse.
*!***************************************************************
FUNCTION lfvPercent

IF LineNo = 0  OR  Percent <= 0
  lcErrMsg = IIF(LineNo = 0,'You must enter payment no. first.','The percentage must be greater than zero.')
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,lcErrMsg)
  REPLACE Percent with lnOldVal
  RETURN
ENDIF
lnRecNo = RECNO()
SUM Percent TO lnPer FOR cSele <> 'D'
*--Get the record for any updates.
GO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,RECNO())
IF lnPer > 100
  *--Total percentage cannot exceed 100 % of the order book amount.
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Total percentage cannot exceed 100 % of the order book amount.")
  REPLACE Percent with lnOldVal
  RETURN
ENDIF
REPLACE Amount WITH (lnBookAmnt*Percent)/100
*--If it is a new record don't change the status, otherwise it's 'M'odified.
REPLACE cSele WITH IIF(cSele = 'N','N','M')
SUM Amount TO lnPaid FOR cSele <> 'D'
lnBalance = lnBookAmnt - lnPaid
GO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,RECNO())
SHOW GET pbRemove ENABLE
SHOW WINDOW (lcBrTitle) REFRESH SAME
=lfRefresh('SOPAYSCH')

*!***************************************************************
*! Name      : lfvPayAmnt
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Validate Amount filed in the browse.
*!***************************************************************
FUNCTION lfvPayAmnt

IF LineNo = 0 OR Amount <= 0
  lcErrMsg = IIF(LineNo = 0,'You must enter payment no. first!','The amount must be greater than zero.')
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,lcErrMsg)
  REPLACE amount with lnOldVal
  RETURN
ENDIF
lnRecNo = RECNO()
SUM Amount to lnPaid FOR cSele <> 'D'
*--Get the record for any updates.
GO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,RECNO())
*-- If the new amount entered is greater than the remaining balance.
IF lnPaid > lnBookAmnt
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Payment amount cannot exceed the remaining balance.")
  REPLACE Amount with lnOldVal
  RETURN
ENDIF
REPLACE Percent WITH (Amount/lnBookAmnt)*100
lnBalance = lnBookAmnt - lnPaid
*--If it is a new record don't change the status, otherwise it's 'M'odified.
REPLACE cSele WITH IIF(cSele = 'N','N','M')
SHOW GET pbRemove ENABLE
SHOW WINDOW (lcBrTitle) REFRESH SAME
=lfRefresh('SOPAYSCH')


*!***************************************************************
*! Name      : lfwPrOrAmt
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : When Percent or Amount filed in the browse.
*!***************************************************************
FUNCTION lfwPrOrAmt
PARAMETERS lcField

lnOldVal = IIF(lcField = 'P',Percent,Amount)
*--modifying isn't allowed in View mode.
RETURN IIF(laScrMode[2] ,.F.,.T.)

*!***************************************************************
*! Name      : lfvLineNo
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Validate LineNo (Payment no).
*!***************************************************************
FUNCTION lfvLineNo

*--Save the old value
lnOld = LineNo
*--Replace LineNo with 0 to seek the new payment in all remaining lines.
REPLACE LineNo WITH 0
*--We locate here and not seek as it may be more than one record and the first ma have the status 'D'
LOCATE FOR (LineNo = lnOld) AND cSele <> 'D'
IF FOUND()
  =SEEK(0)
  REPLACE LineNo WITH lnOld
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"This Payment number already exists.")
  RETURN 0
ENDIF
=SEEK(0)
REPLACE LineNo WITH lnOld,;
        cSele  WITH IIF(cSele = 'S','M',cSele)
SHOW WINDOW (lcBrTitle) REFRESH SAME

*!***************************************************************
*! Name      : lfWCanMod
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : If this record is a previously entered record we cannot change it's payment no for
*!           : the purpose of updating.
*!***************************************************************
FUNCTION lfWCanMod

RETURN IIF(laScrMode[2] ,.F.,.T.)

*!***************************************************************
*! Name      : lfSAV_PAY
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Save payment lines in BOMVAR.
*!***************************************************************
FUNCTION lfSAV_PAY

PRIVATE lcAlias
lcAlias = ALIAS()
SELECT (lcPayTmp)
SCAN FOR cSele <> 'S'
  SELECT BOMVAR
  DO CASE
    *--A modified record.
    CASE &lcPayTmp..cSele = 'M'
      GO &lcPayTmp..nRecNo
      REPLACE LineNo   WITH &lcPayTmp..LineNo,;
              nPercent WITH &lcPayTmp..Percent,;
              TotCost  WITH &lcPayTmp..Amount,;
              mSizes   WITH &lcPayTmp..Desc
    *--A deleted record.
    CASE &lcPayTmp..cSele = 'D'
      GO &lcPayTmp..nRecNo
      DELETE
    *--A new record.
    CASE &lcPayTmp..cSele = 'N'
      APPEND BLANK
      REPLACE cIDType  WITH 'PY',;
              cCost_Id WITH &lcOrdHdr..Order,;
              LineNo   WITH &lcPayTmp..LineNo,;
              nPercent WITH &lcPayTmp..Percent,;
              TotCost  WITH &lcPayTmp..Amount,;
              mSizes   WITH &lcPayTmp..Desc
  ENDCASE
ENDSCAN

SELECT (lcPayTmp)
ZAP

SELECT IIF(!EMPTY(lcAlias),lcAlias,ALIAS())

*!***************************************************************
*! Name      : lpClosPay
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Validate close button.
*!***************************************************************
PROCEDURE lpClosPay

*--Delete any record with amount  = 0
LOCATE FOR (Percent = 0 OR Amount = 0)
IF FOUND()
  *--'There is a pamyent with zero amount. This pamyent will be ignored !'
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,"There is a pamyent with zero amount. This pamyent will be ignored !")
  DELETE
ENDIF
CLEAR READ


*!***************************************************************
*! Name      : lfZAPTMP
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Zap temp file.
*!***************************************************************
FUNCTION lfZAPTMP

*--Zap tmp file when quitting without saving.
SELECT (lcPayTmp)
ZAP

*!***************************************************************
*! Name      : lfvDesc
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Validate desc field.
*!***************************************************************
FUNCTION lfvDesc

REPLACE cSele WITH IIF(cSele = 'N','N','M')


