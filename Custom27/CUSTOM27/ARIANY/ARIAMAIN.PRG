*!**************************************************************************
*! Name      : ARIAMAIN.PRG
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/21/2000
*! Purpose   : ARIA'S payment schedule Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*: Modifications : 
*!C200176,1  03/21/2001 MHM Apply partially paymnet commission
*!C200176,4  04/23/2001 
*!B604486,1  05/21/2001 MHM Custom paymnet shows wrong commission amt.
*!B604675,1  07/17/2001 AKA Fix the bug of not saving the Custom paymnet correctly
*!B605799,1 SSE 04/10/2002 Fix bug of changing screen set names to prevent its conflict with the standard screen names.
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
*--Don't update customer profile.
llOpenOrd = .F.

*!***************************************************************
*! Name      : lfvDesc
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Validate desc field.
*!***************************************************************
FUNCTION lfvDesc

REPLACE cSele WITH IIF(cSele = 'N','N','M')


*!***************************************************************
*! Name      : lfSTATCHG
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Confim with the user when changing the order status from Bid to Open.
*!***************************************************************
*! (C200115,1)
*!***************************************************************
FUNCTION lfSTATCHG

*--If Edit mode and the user changed the staus from Bid to Open and he answer the question below with 'No', Let the
*--order status be Bid as it was and don't accept changing.
*--I put ORDHDR.STATUS ='B' in the condition as we will do nothing unless the original status is Bid.
lcMissage = "Once the status of an order changed to Open, Customer profile will be updated and this order cannot be modified (Only Cancelled), Are you sure you have a signed proposal/license agreement?"
IF laScrMode[3] AND laData[5] = 'O' AND ORDHDR.STATUS ='B' AND gfModalGen('TRM00000B32000',.F.,.F.,.F.,lcMissage) =2
  REPLACE STATUS WITH 'B'
  laData[5]   = 'B'
  lnOrdStatus = 1
  SHOW GET lnOrdStatus
ENDIF
*--We will update customer profile files under this variable being .T.
llOpenOrd = (laScrMode[3] AND laData[5] = 'O' AND ORDHDR.STATUS ='B')

*!***************************************************************
*! Name      : lfSAV_CUST
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Save customer profiles.
*!***************************************************************
*! (C200115,1)
*!***************************************************************
FUNCTION lfSAV_CUST

*--If this order's status chenged fro 'Bid' to  'Open'
IF llOpenOrd
  *--Open Customer profile files.
  =gfOpenFile(gcDataDir+'CUSTPROF',gcDataDir+'CUST_ID','SH')
  =gfOpenFile(gcDataDir+'CSTSOFTW',gcDataDir+'CUST_ID','SH')
  =gfOpenFile(gcDataDir+'CSTTRAIN',gcDataDir+'CUST_ID','SH')
  *-----------------------
  *--Update CUSTPROF file.
  *-----------------------
  *--Calculate Soft. upgrade amount
  SELECT (lcOrdLine)
  *--Calculate Software Upgrade Amount.
  SUM Price*TotQty TO lnCustAmt FOR STYLE = 'A27-MNT'
  *--Calculate no of user.
  lnNoOfUsr = IIF(Customer.PriceLvl='C',1,IIF(Customer.PriceLvl='B',3,0))
  IF Customer.PriceLvl='A'
    SUM TotQty TO lnNoOfUsr FOR STYLE = 'ADD-USER'
    lnNoOfUsr = 5 + lnNoOfUsr
  ENDIF
  *--Calculate Time Allocated.
  SUM IIF(STYLE = 'ONSITE-SUP' AND TOTQTY > 30 ,30,TotQty) TO lnHoursNo FOR STYLE = 'ONSITE' 
  *--Update Customer profile file.
  SELECT CUSTPROF
  *--If this account doesn't exist.
  IF !SEEK(laData[2]+laData[3])
    APPEND BLANK
    REPLACE cCust_Id   WITH laData[2],;
            cCust_Name WITH Customer.BTName,;
            lCust_Main WITH .T.,;
            lCust_Wind WITH .T.,;
            cTot_tm_Us WITH "000:00",;
            cCust_LcId WITH laData[3],;
            nCust_Qmam WITH lnCustAmt,;
            nCust_Usrn WITH lnNoOfUsr,;
            cTot_tm_Al WITH PADL(INT(lnHoursNo),3,'0')+":00",;
            cCust_OsVr WITH ALLTRIM(&lcOrdHdr..Season),;
            cAdd_User  WITH gcUser_ID,;
            cAdd_Time  WITH TIME(),;
            dAdd_Date  WITH gdSysDate
  ELSE
    REPLACE nCust_Qmam WITH nCust_Qmam+lnCustAmt,;
            cTot_tm_Al WITH PADL(INT(VAL(cTot_tm_Al)+lnHoursNo),3,'0')+":00",;
            nCust_Usrn WITH nCust_Usrn+IIF(Customer.PriceLvl $ 'BC',0,lnNoOfUsr)
  ENDIF
  *-----------------------
  *--Update CSTSOFTW file AND CSTTRAIN.
  *-----------------------
  *--Open sydappl to get modules desc.
  =gfOpenFile(gcDataDir+'SuAppMod',gcDataDir+'Appmodul','SH')
  SELECT (lcOrdLine)
  SCAN
    *--There is a relation between (lcOrdLine) and Style file.
    IF !EOF('STYLE') AND !EMPT(STYLE.Location)
      SELECT SuAppMod
      *--Get the module.
      LOCATE FOR ALLTRIM(CMOD_ID) = ALLTRIM(STYLE.Location)
      *--Get the module (Or Custom Program) description.
      lcSoftMod = IIF(FOUND(),ALLTRIM(STYLE.Location)+'-'+ALLTRIM(SuAppMod.cMod_Desc),'')
      lcSoftMod = IIF(ALLTRIM(STYLE.Location) = 'CP','CP-'+ALLTRIM(&lcOrdLine..Desc1),lcSoftMod)
      *-----------------------
       *--Update CSTSOFTW file.
      *-----------------------
      SELECT CSTSOFTW
      *--Add a record if Account+Space(8)+Season+Module  doesn't exist. OR anothre Custom Progam.
      IF !SEEK(laData[2] + SPACE(8)+ ALLTRIM(&lcOrdHdr..Season) + ALLTRIM(STYLE.Location)) OR (ALLTRIM(STYLE.Location) = 'CP' AND !(cSoft_Modu ==lcSoftMod))
        APPEND BLANK
        REPLACE cCust_Id   WITH laData[2],;
                cSoft_ApId WITH ALLTRIM(&lcOrdHdr..Season),;
                cSoft_ApNm WITH gfCodDes(ALLTRIM(&lcOrdHdr..Season),'SEASON'),;
                cSoft_Modu WITH lcSoftMod ,;
                cSoft_FoxV WITH 'F',;
                cSoft_Plat WITH 'W',;
                cSoft_Ver  WITH '2.7',;
                cSoft_User WITH 'M',;
                cPrg_Mod   WITH IIF(ALLTRIM(STYLE.Location) = 'CP','P','M'),;
                Order      WITH &lcOrdHdr..Order,;
                cAdd_User  WITH gcUser_ID,;
                cAdd_Time  WITH TIME(),;
                dAdd_Date  WITH gdSysDate
      ENDIF
    ENDIF            
    SELECT (lcOrdLine)          
    IF STYLE = 'ONSITE'
      *-----------------------
      *--Update CSTTRAIN file.
      *-----------------------
      *--Add a record for each style beginning with 'ONSITE'
      lnNoOfHo = IIF(&lcOrdLine..TotQty>30,30,TotQty)
      SELECT CSTTRAIN
      APPEND BLANK
      REPLACE cCust_Id   WITH laData[2],;
              cTrnCode   WITH '1',;
              dTrain_Dat WITH gdSysDate,;
              mTrain_Not WITH IIF(STYLE.STYLE='ONSITE-SUP','Initial Training',IIF(STYLE.STYLE='ONSITE-CONS','Initial Consulting','')),;
              cTrain_Stm WITH '00:00:00',;
              cTrain_Etm WITH '00:00:00',;
              cTrain_Eap WITH 'F',;
              Order      WITH &lcOrdHdr..Order,;
              cSoft_ApId WITH ALLTRIM(&lcOrdHdr..Season),;
              cTrain_Hn  WITH PADL(lnNoOfHo,3,'0')+":00",;
              cAdd_User  WITH gcUser_ID,;
              cAdd_Time  WITH TIME(),;
              dAdd_Date  WITH gdSysDate
    ENDIF
  ENDSCAN
ENDIF
*--False llOpenOrd.
llOpenOrd = .F.


*!***************************************************************
*! Name      : lfNOMODI
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Don't enable the user to Modify an Open order or UnCancel a Canceleld one.
*!***************************************************************
*! (C200115,1)
*!***************************************************************
FUNCTION lfNOMODI

IF laData[5] = 'O'
  SHOW GET pbEdt DISABLE
ENDIF  
IF laData[5] = 'X'
  SHOW GET pbDlt DISABLE
ENDIF  


*!***************************************************************
*! Name      : lfDELSOFT
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 03/22/2000
*! Purpose   : Delete order's records in CSTTRAIN,CSTSOFTW files.
*!***************************************************************
*! (C200115,1)
*!***************************************************************
FUNCTION lfDELSOFT

PRIVATE lnAlias
lnAlias = SELECT(0)
*--Sum accumulated fields to be deducted from CUSTPROF
SELECT ORDLINE
=SEEK('O'+laData[1])
SUM Price*TotQty TO lnCustAmt REST WHILE cordtype+order+STR(lineno,6) = 'O'+laData[1] FOR STYLE = 'A27-MNT'
lnNoOfUsr = IIF(Customer.PriceLvl='C',1,IIF(Customer.PriceLvl='B',3,0))
IF Customer.PriceLvl='A'
  =SEEK('O'+laData[1])
  SUM TotQty TO lnNoOfUsr REST WHILE cordtype+order+STR(lineno,6) = 'O'+laData[1] FOR STYLE = 'ADD_USER'
  lnNoOfUsr = 5 + lnNoOfUsr
ENDIF
=SEEK('O'+laData[1])
SUM IIF(STYLE = 'ONSITE-SUP' AND TOTQTY > 30 ,30,TotQty) TO lnHoursNo REST WHILE cordtype+order+STR(lineno,6) = 'O'+laData[1] FOR STYLE = 'ONSITE'
=gfOpenFile(gcDataDir+'CUSTPROF',gcDataDir+'CUST_ID','SH')
=SEEK(laData[2])
REPLACE nCust_Qmam WITH nCust_Qmam - lnCustAmt,;
        cTot_tm_Al WITH PADL(INT(VAL(cTot_tm_Al)-lnHoursNo),3,'0')+":00",;
        nCust_Usrn WITH nCust_Usrn-lnNoOfUsr


*--Delete all records in CSTTRAIN
=gfOpenFile(gcDataDir+'CSTTRAIN',gcDataDir+'CUST_ID','SH')
=SEEK(laData[2])
DELETE REST WHILE ccust_id+ccust_lcid+csoft_apid = laData[2];
       FOR  Order = laData[1]
       
*--Delete all records in CSTSOFTW
=gfOpenFile(gcDataDir+'CSTSOFTW',gcDataDir+'CUST_ID','SH')
=SEEK(laData[2])
DELETE REST WHILE ccust_id+ccust_lcid+csoft_apid+csoft_modu = laData[2];
       FOR  Order = laData[1]

       
SELECT (lnAlias)
*!***************************************************************
*! Name      : ActiScr           (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/19/2001
*! Purpose   : Call function for the screen.
*!***************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =ActiScr()
*!*************************************************************
FUNCTION lfActiScr
STORE 0 TO lcBalance, lcTotComm , lcAmount ,lcPrevComm
STORE .F. TO llCheck
STORE .T. TO llCheck2
=lfOpnFile()
PUSH KEY                                     && To save the the current on key label
ON KEY LABEL ALT+T ACTIVATE WINDOW (lcBrTrttl) && To activate the browse screen when pressing ALT+B
ON KEY LABEL ESC DO lfvCancl

*B605799,1 Change screen name. [Begin]
*DO (gcScrDir + gcWinAppl + '\srreptr.SPX')
DO (gcScrDir + gcWinAppl + '\SRPayInv.SPX')
*B605799,1 Change screen name. [End]

*DO (gcScrDir + gcWinAppl + '\srpayinv.SPX')

POP KEY                                      && To Restore the previous assignments for on key label

*!*************************************************************
*! Name      : lfLCCrTrp        (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/19/2001
*! Purpose   : Clearing the previous trapping
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfLCCrTrp()
*!*************************************************************

FUNCTION lfLCCrTrp

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
*ON KEY LABEL ESC  

*!*************************************************************
*! Name      : lfTrBrTab                      (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/19/2001
*! Purpose   : Trap the Tab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfBrTab()
*!*************************************************************

FUNCTION lfTrBrTab
ON KEY LABEL TAB

*B605799,1 Change screen name. [Begin]
*ACTIVATE WINDOW srreptr3
ACTIVATE WINDOW SRPayIn3
*B605799,1 Change screen name. [End]

_CUROBJ = OBJNUM(pbOk)

*!*************************************************************
*! Name      : lfTrBrBack  (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/19/2001
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrBrBack()
*!*************************************************************

FUNCTION lfTrBrBack
ON KEY LABEL BACKTAB

*B605799,1 Change screen name. [Begin]
*ACTIVATE WINDOW srreptr3
ACTIVATE WINDOW SRPayIn3
*B605799,1 Change screen name. [End]


_CUROBJ = OBJNUM(pbOk)
*!*************************************************************
*! Name      : lfTrap             (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/19/2001
*! Purpose   : TO Assign functions to some keys to not affect the browse
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfBrTab,lfBrBack
*!*************************************************************
*! Passed Parameters  : NONE 
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrnTrap()
*!*************************************************************

FUNCTION lfLCTrnTrp

*-- This function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcBrTrttl
  ON KEY LABEL TAB     DO lfTrBrTab
  ON KEY LABEL BACKTAB DO lfTrBrBack
  *ON KEY LABEL ESC     DO lfTrBrEsc
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfTrBrEsc               (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/19/2001
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrBrEsc()
*!*************************************************************

FUNCTION lfTrBrEsc
ON KEY LABEL ESC

*B605799,1 Change screen name. [Begin]
*ACTIVATE WINDOW srreptr3
ACTIVATE WINDOW SRPayIn3
*B605799,1 Change screen name. [End]

_CUROBJ = OBJNUM(pbCan)
KEYBOARD "{ENTER}" CLEAR
*!*************************************************************
*! Name      : lfLcTrnBrw                 (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/19/2001
*! Purpose   : Browse the invoice records
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrnBrow()
*!*************************************************************

FUNCTION lfLcTrnBrw

lnCurAlias = SELECT(0)
lcBrRpCom = "cMarker=IIF(RECNO()=lnBrTrNo,'>',' '):1:H=' ':W=.F.,"+; 
            "TRAN       :8  :R :H='Trans.',"+;
            "ORDER      :8  :R :H='Order ',"+;
            "DESC       :20 :R :H='Description',"+;
            "ACCOUNT    :8  :R :H='Acct#',"+;
            "CPAID      :8  :R :H='Paid',"+;
            "AMOUNT     :10 :R :H='Total Comm.':P='9999999.99',"+;
            "nTotComm   :10 :R :H='Prev. Comm.':P='9999999.99',"+;
            "Balance    :10 :R :H='Balance':P='9999999.99',"+;
            "NAMOUNT    :10 :R :H='Amount' :P='9999999.99'"

SELECT (lcTmpComm)
LOCATE

*B605799,1 Change screen name. [Begin]
*BROWSE KEY lcRepCode ;
*   FIELDS &lcBrRpCom ;
*   WINDOW 'srreptr2' ;
*   WHEN lfwTrnBrow() ;
*   IN WINDOW 'srreptr'   ;
*   LOCK 0            ;
*   NOAPPEND          ;
*   NOEDIT            ;
*   NOCLEAR           ;
*   NODELETE          ;
*   NOMENU            ;   
*   SAVE              ;
*   TITLE lcBrTrttl   ;
*   NOWAIT
BROWSE KEY lcRepCode ;
   FIELDS &lcBrRpCom ;
   WINDOW 'SRPayIn2' ;
   WHEN lfwTrnBrow() ;
   IN WINDOW 'SRPayInv'   ;
   LOCK 0            ;
   NOAPPEND          ;
   NOEDIT            ;
   NOCLEAR           ;
   NODELETE          ;
   NOMENU            ;   
   SAVE              ;
   TITLE lcBrTrttl   ;
   NOWAIT
*B605799,1 Change screen name. [End]

ON KEY LABEL TAB        DO lfTrBrTab
ON KEY LABEL BACKTAB    DO lfBrBack
*--mhm2000
=lfvBalance()
=lfSumRst()
lcAmount   = NAMOUNT
lcTotComm  = AMOUNT
lcPrevComm = nTotComm
lcBalance  = balance

*B605799,1 Change screen name. [Begin]
*SHOW WINDOW 'srreptr3' REFRESH SAME
SHOW WINDOW 'SRPayIn3' REFRESH SAME
*B605799,1 Change screen name. [End]


_CUROBJ = OBJNUM(pbOk)
SELECT (lnCurAlias)               

*!***************************************************************
*! Name      : lfCREATTMP           (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/19/2001
*! Purpose   : Call function to add Amount Field .
*!***************************************************************
FUNCTION lfCREATTMP
*--increase Array of lcTmpComm With 2 fields For amount and cancel
DIMENSION laFileStru[lnFileStru + 3, 4]
  
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'NAMOUNT'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 11
laFileStru[lnFileStru ,4] = 2

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'lCancel'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 1
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'NORGAMT'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 11
laFileStru[lnFileStru ,4] = 2

*--CREATE temp File for PAYTHIST
IF !USED('PAYTHIST')
  =gfOpenFile(gcDataDir+'PAYTHIST',gcDataDir+'','SH')
ENDIF

lcTmpPay  = gfTempName()
IF TYPE("laTempStru[1,1]") $ "UL" 
  DIMENSION laTempStru[1,4]
  SELECT PAYTHIST
  =AFIELDS(laTempStru)
ENDIF
CREATE TABLE (gcWorkDir+lcTmpPay) FROM ARRAY laTempStru          

*--end lfCREATTMP
*!*************************************************************
*! Name      : lfvAmount            (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/19/2001
*! Purpose   : Validate Amount
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvAmount()
*!*************************************************************
FUNCTION lfvAmount
PRIVATE lcAlias


lcAmount = IIF( lcAmount < 0 ,  -1, 1  ) * lcAmount
lcAlias = ALIAS()
SELECT (lcTmpComm)
lnRecNo = RECNO()
=SEEK(RepCode+DTOS(date)+tran+trantype,'REPCOMM')
*--TranType is Invoice
IF TranType = '1'
  *--not exceed Balance
  *--mhm2000
  IF (0 =< lcAmount) AND (lcAmount =< balance)
  *IF (0 =< lcAmount) AND (lcAmount =< NAMOUNT)
  *--mhm2000
    REPLACE NAMOUNT WITH lcAmount
    *REPLACE lCancel WITH "Y"
  ELSE
    =gfModalGen('QRM00000B38018','F',' ',' ','Amount figure can not exceed balance figure.')   
    lcAmount = NAMOUNT
    RETURN
  ENDIF  
ELSE
  *--To control Amount With positive or negative
  lnAmnt = IIF( &lcTmpComm..balance < 0 ,  -1, 1  ) * &lcTmpComm..balance
  *--mhm2000
  *IF lcAmount  =< balance
  IF lcAmount  =< lnAmnt 
    IF (REPCOMM.Amount < 0 )
      IF lcAmount >0
        lcAmount = -1 * lcAmount
      ENDIF
    ENDIF
    REPLACE NAMOUNT WITH lcAmount 
    *REPLACE lCancel WITH "Y"
  ELSE
    =gfModalGen('QRM00000B38018','F',' ',' ','Amount figure can not exceed balance figure.')   
    lcAmount = NAMOUNT
    RETURN
  ENDIF  

ENDIF  

SELECT (lcTmpComm)

=lfSumRst()

*LOCATE
*--sum all added amount to lcTotAmt
*SUM REST NAMOUNT to lcTotAmt FOR RepCode = lcRepCode
*SUM NAMOUNT to lcTotAmt FOR RepCode = lcRepCode

IF lnRecNo = RECCOUNT()
  LOCATE
ELSE
  GOTO lnRecNo +1
ENDIF  
*=lfRefresh()

*B605799,1 Change screen name. [Begin]
*SHOW WINDOW 'srreptr2' REFRESH SAME
*SHOW WINDOW 'srreptr1' REFRESH SAME
*=lfRefresh('srreptr1')
SHOW WINDOW 'SRPayIn2' REFRESH SAME
SHOW WINDOW 'SRPayIn1' REFRESH SAME
=lfRefresh('SRPayIn1')
*B605799,1 Change screen name. [End]

SELECT (lcAlias)

*!*************************************************************
*! Name      : lfwTrnBrow             (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2001
*! Purpose   : When Function for browse 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwTrnBrow()
*!*************************************************************
*!
FUNCTION lfwTrnBrow
lnCurAlias = SELECT(0)
SELECT (lcTmpComm)
lnBrTrNo   = RECNO()
*--initialize fields
lcAmount   = NAMOUNT
lcTotComm  = AMOUNT
lcPrevComm = nTotComm
lcBalance  = balance

SHOW GET lcPrevComm 
SHOW GET lcTotComm  
SHOW GET lcAmount 

SELECT (lnCurAlias)
SHOW WINDOW (lcBrTrttl) REFRESH
SHOW GETS
*--End

*!*************************************************************
*! Name      : lfvOk                       (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2001
*! Purpose   : Validate Functoin for OK
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvOk()
*!*************************************************************
*!
FUNCTION lfvOk
PRIVATE lnCurAlias , lnCurrOrd
lnCurAlias = SELECT(0)

SELECT (lcTmpPay)
****   AMIN   ****
*** =SEEK(lcRepCode)  **** 
lcRepExp = " RepCode = lcRepCode "
DELETE FOR &lcRepExp 
*-- to empty the file of payment
*Zap


SELECT (lcTmpComm)

lnCurrOrd = RECNO()
*--Update Temp. file of PayTHist
SCAN FOR &lcRepExp 
  IF SEEK(RepCode+DTOS(date)+Tran ,'REPCOMM')
    lnAmunt   = IIF( REPCOMM.Amount   < 0 , -1 , 1 ) * REPCOMM.Amount
    lnTotAmnt = IIF( REPCOMM.nTotComm < 0 , -1 , 1 ) * REPCOMM.nTotComm

    IF lnTotAmnt < lnAmunt
      SELECT (lcTmpPay)
      APPEND BLANK
      SELECT (lcTmpComm)
      
      *B604486,1  05/21/2001 MHM [Start]
      *REPLACE &lcTmpPay..RepCode     WITH RepCode,;
              &lcTmpPay..Tran        WITH Tran,;
              &lcTmpPay..Account     WITH Account,;
              &lcTmpPay..Amount      WITH NAMOUNT,;
              &lcTmpPay..PayDate     WITH IIF(TranType$'12' ,ldBatch,gdSysDate),;
              &lcTmpPay..TranType    WITH TranType,;
              &lcTmpPay..nTotComm     WITH Amount,;
              &lcTmpPay..nTotPrvCom  WITH nTotComm 
              
      REPLACE &lcTmpPay..RepCode     WITH RepCode,;
              &lcTmpPay..Tran        WITH Tran,;
              &lcTmpPay..Account     WITH Account,;
              &lcTmpPay..Amount      WITH NAMOUNT,;
              &lcTmpPay..PayDate     WITH ldBatch,;
              &lcTmpPay..TranType    WITH TranType,;
              &lcTmpPay..nTotComm    WITH Amount,;
              &lcTmpPay..nTotPrvCom  WITH nTotComm ,;
              &lcTmpPay..nCommBAL    WITH balance
     *B604486,1  05/21/2001 MHM [End]
      *--to update invoice #
      IF TranType = '1'
        REPLACE &lcTmpPay..INVOICE   WITH Tran
      ENDIF  
      *REPLACE &lcTmpComm..lCancel WITH "Y"
      *--mhm200011
      *B604675,1  07/17/2001 AKA [Begin]
      REPLACE &lcTmpComm..NORGAMT WITH NAMOUNT , ;
              &lcTmpComm..FLAG WITH 'Y' 
      *B604675,1  07/17/2001 AKA [End]                   
      *--mhm2000
      llCheck = .T.
    *ELSE
    *  =gfModalGen('QRM00000B38018','F',' ',' ','Fully paid.')   
    *  RETURN
    ENDIF  
  ENDIF  
ENDSCAN

SELECT (lcTmpComm)
LOCATE
SUM REST NAMOUNT to lnAmt FOR RepCode = lcRepCode

SELECT (lcTmpComm)
GO lnCurrOrd 
SELECT (lnCurAlias )
CLEAR READ
*--END
*!*************************************************************
*! Name      : lfvBalance                    (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 20/03/2001
*! Purpose   : Validate Functoin for Balance
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvBalance()
*!*************************************************************
*!
FUNCTION lfvBalance
PRIVATE lnInvAmt , lnDbitAmt , lnTotPaid ,lnAlias , lcSavOrd , lnBalance
STORE 0 TO lnDbitAmt,  lnInvAmt  , lnBalance
lnAlias =SELECT(0)
SELECT (lcTmpComm)
* If the recods for the current salesrep was already updated through the Payment screen. we don't
* need to read the data from master file again we need only to update the amuont field from the nOrdgAmnt

IF SEEK(lcRepCode)  AND  lCancel  = "Y"
  =lfRestAmnt()
ELSE
  SCAN FOR RepCode = lcRepCode
    *--get invoice amount
    *--if there is no enter "you click cancel without  ok"
    IF TranType = '1'
      =SEEK(TRAN,'INVHDR')
      *!B604486,1  05/21/2001 MHM [start]
      *lnInvAmt = INVHDR.TOTALCHG
      *lnInvAmt = INVHDR.ShipAmt + INVHDR.Discount - (INVHDR.Trde_Disc/100)*INVHDR.ShipAmt
      lnInvamont= INVHDR.ShipAmt + INVHDR.Discount 
      lnInvAmt  = lnInvamont - (INVHDR.Trde_Disc/100)*lnInvamont
      *!B604486,1  05/21/2001 MHM [end]
      *--get debit amount from debit file
      SELECT DEBIT
      lcSavOrd = ORDER('DEBIT')
      SET ORDER TO 2
      =SEEK('1'+&lcTmpComm..tran )
      lnDbitAmt = DEBIT.AMOUNT
      *--get total invoice amount paid
      SET ORDER TO &lcSavOrd
      *!B604486,1  05/21/2001 MHM [start]
      *lnTotPaid = lnInvAmt - lnDbitAmt
      IF INVHDR.TOTALCHG > lnDbitAmt
        lnTotN = INVHDR.TOTALCHG - lnDbitAmt
        IF lnTotN <= lnInvAmt
          lnTotPaid = lnTotN
        ELSE 
          lnTotPaid = lnInvAmt
        ENDIF
      ELSE  
        lnTotPaid = 0
      ENDIF
      *!B604486,1  05/21/2001 MHM [end]
      =SEEK(&lcTmpComm..RepCode+DTOS(&lcTmpComm..date)+&lcTmpComm..Tran+ '1','REPCOMM')  
      lnBalance= ROUND((lnTotPaid*RepComm.commpcnt/100) - RepComm.nTotComm,2)
      SELECT (lcTmpComm)
      *--mhm2000
      *REPLACE balance with lnBalance
      *IF lcAncel <> 'Y'
        lnBalance =IIF(lnBalance < 0 ,0,lnBalance)
        REPLACE NAMOUNT with lnBalance
      *ENDIF  
      *--mhm2000
    ELSE
      SELECT (lcTmpComm)
      *--mhm2000
      *REPLACE &lcTmpComm..balance with 0
      *IF lcAncel <> 'Y'
        REPLACE &lcTmpComm..NAMOUNT with (AMOUNT - nTotComm )
      *ENDIF  
      *--mhm2000
    ENDIF
    SELECT (lcTmpComm)
    *--mhm2000
    REPLACE Balance with AMOUNT - nTotComm
    REPLACE NORGAMT WITH IIF(TranType = '1' , NAMOUNT  , AMOUNT - nTotComm )
    REPLACE lCancel WITH "Y"
  ENDSCAN  
ENDIF
  
SELECT (lcTmpComm)
LOCATE
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfOpnFile                   (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2001
*! Purpose   : Open Needed Files  
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfOpnFile()
*!*************************************************************
*!
FUNCTION lfOpnFile

IF !USED('PAYTHIST')
  =gfOpenFile(gcDataDir+'PAYTHIST',gcDataDir+'','SH')
ENDIF

IF !USED('INVHDR')
  =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
ENDIF

*!*************************************************************
*! Name      : CHCKVALD                         (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2001
*! Purpose   : Check Validateion of the total amount 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =CHCKVALD()
*!*************************************************************
*!
FUNCTION lfCHCKVALD
PRIVATE lnAlias

lnAlias = SELECT (0)
SELECT (lcTmpComm)
LOCATE 
IF lnActual # lnTapTot
  *-- "Actual amount not equal Tape amount."
  *-- <PROCEED> , <MODIFY> 
  llRrturn = gfModalGen("QRM30010B30001","Dialog") = 1
  _CUROBJ = OBJNUM(pbNew)
  *--C200176,4         [Start]
  IF llRrturn
    =lfUpdatcom()
  ENDIF  
  *-C200176,4 [end]
ELSE
  llRrturn = .T.
  SELECT(lcTmpRep)
  Amount = lnAmt
  =lfUpdatcom()
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfCHNGSTS          (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/21/2001
*! Purpose   : Change Status Of payment 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfCHNGSTS()
*!*************************************************************
*!
FUNCTION lfCHNGSTS

IF (REPCOMM.nTotComm = REPCOMM.Amount) 
   *REPLACE STATUS  WITH 'P',;
          PAYDATE  WITH IIF(TranType$'12' AND lcPayType = 'M',ldBatch,gdSysDate)
   REPLACE STATUS  WITH 'P', PAYDATE  WITH ldBatch

ELSE
  REPLACE STATUS   WITH 'O',;
          PAYDATE  WITH ldBatch,;
          nTotcomm WITH &lcTmpComm..nTotComm
  IF (REPCOMM.nTotComm = REPCOMM.Amount) 
      REPLACE STATUS   WITH 'P'
  ENDIF
  *PAYDATE  WITH IIF(TranType$'12' AND lcPayType = 'M',ldBatch,gdSysDate),
ENDIF

*--end
*!*************************************************************
*! Name      : lfUPDATPAY             (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/21/2001
*! Purpose   : Update PayTHist File
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfUPDATPAY()
*!*************************************************************
*!
FUNCTION lfUPDATPAY
PRIVATE lnAlias
lnAlias = SELECT (0)
SELECT (lcTmpPay)
*LOCATE
*REPLACE ALL &lcTmpPay..TRAN WITH lcTrNo
*--Update Records with Amount <> 0
SCAN FOR AMOUNT # 0
  SCATTER TO laPayData
  INSERT INTO PAYTHIST  FROM ARRAY laPayData
  =SEEK(RepCode,lcTmpRep)
  REPLACE PAYTHIST.TRAN WITH &lcTmpRep..TraN
  *--mhm2000
  REPLACE PAYTHIST.PAYDATE WITH &lcTmpRep..PAYDATE
  *--MHM2000
ENDSCAN
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfUpdatcom         (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/21/2001
*! Purpose   : Update Temp Files for comm.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfUPDATPAY()
*!*************************************************************
*!
FUNCTION lfUpdatcom
PRIVATE lnAlias

lnAlias = SELECT (0)
SELECT (lcTmpComm)
SCAN
  REPLACE &lcTmpComm..nTotComm   WITH NAMOUNT + &lcTmpComm..nTotComm ,;
          &lcTmpRep..nTotComm    WITH NAMOUNT + &lcTmpRep..nTotComm 
ENDSCAN
LOCATE
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvCancl             (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/21/2001
*! Purpose   : Validate Cancel
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvCancl()
*!*************************************************************
*!
FUNCTION lfvCancl
PRIVATE lnAlias


=lfRestAmnt()
=lfSumRst()

*lnAlias = SELECT (0)
*SELECT (lcTmpComm)
*--mhm2000
*SCAN
*--mhm2000
  *IF lCancel = "Y"
    *--mhm2000
    *REPLACE  NAMOUNT WITH 0
    *=lfvBalance()
    *--mhm2000
  *ENDIF  
*--mhm2000
*ENDSCAN
*--mhm2000
*lnAmt = IIF (lcTotAmt = 0 , 0 , lcTotAmt) 
*SELECT(lnAlias)
*-C200176,4 [Start]
CLEAR READ
*-C200176,4 [end]
*!*************************************************************
*! Name      : lfSumRst             (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/21/2001
*! Purpose   : Sum Total Amount 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfvCancl()
*!*************************************************************
*!
FUNCTION lfSumRst
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT (lcTmpComm)
*--sum all added amount to lcTotAmt
LOCATE 
SUM NAMOUNT TO lcTotAmt FOR RepCode = lcRepCode 
lnAmt = IIF (lcTotAmt = 0 , 0 , lcTotAmt) 
LOCATE
SELECT (lcAlias)



*!*************************************************************
*! Name      : lfRestAmnt             (C200176,1)
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/21/2001
*! Purpose   : Restet the amout figure with the last update
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfRestAmnt()
*!*************************************************************
*!
FUNCTION lfRestAmnt
PRIVATE lcAlias
lcAlias = ALIAS()
SELECT (lcTmpComm)
=SEEK(lcRepCode)
SCAN REST WHILE RepCode = lcRepCode 
  REPLACE nAmount WITH nOrgAmt
ENDSCAN
SELECT (lcAlias)
