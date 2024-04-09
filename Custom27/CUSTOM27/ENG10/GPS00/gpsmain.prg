*** 
*** ReFox XI+  #AU157137  Mariam  Mariam [FP25]
***
 PARAMETER lcevntfun, lcfunpars
 lcfunpars = IIF(TYPE('lcFunPars')='C', lcfunpars, '')
 lcfuntorun = 'lf'+ALLTRIM(lcevntfun)+'('+lcfunpars+')'
 llretvalue = EVALUATE(lcfuntorun)
 RETURN llretvalue
*
PROCEDURE lfaprusrpr
 IF  .NOT. lffoundpad('Options')
    DEFINE PAD _option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P, ' '
    ON PAD _option OF _MSYSMENU ACTIVATE POPUP _optionpop
    DEFINE POPUP _optionpop SHADOW MARGIN
 ENDIF
 lnbarno = CNTBAR('_OPTIONPOP')+1
 DEFINE BAR lnbarno OF _optionpop PROMPT 'Allow \<Shipment approval' SKIP FOR gcuser_lvl='O' .OR. lascrmode(1) .OR. lascrmode(4)
 ON SELECTION BAR lnbarno OF _optionpop DO lfvShpAprv IN GPSMAIN.FXP
*
FUNCTION lffoundpad
 PARAMETER lcpadname
 PRIVATE llfound
 llfound = .F.
 FOR lncount = 1 TO CNTPAD('_MSYSMENU')
    IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', lncount))=lcpadname
       llfound = .T.
       EXIT
    ENDIF
 ENDFOR
 RETURN (llfound)
*
PROCEDURE lfvshpaprv
 PRIVATE llok, llaproval, llaproval2, lledit, lausr_lvl, lnslct, llsvapr, lctmpprv
 lnslct = SELECT()
 DIMENSION lausr_lvl[ 1]
 lausr_lvl[ 1] = 'O'
 SELECT cusr_levl FROM (gcsyshome+'SYUUSER') WHERE cuser_id=ladata(1) INTO ARRAY lausr_lvl
 IF lausr_lvl='O'
    = gfmodalgen('INM00000B00000',.F.,.F.,.F.,ALLTRIM(ladata(1))+' is an operator,Use the privilege users screen.')
    RETURN
 ENDIF
 lledit = (gcuser_lvl='A')
 PRIVATE gcuser_id, gllog_requ
 gcuser_id = ladata(1)
 gllog_requ = .T.
 lctmpprv = '_'+SUBSTR(lcprv_tmp1, 2)
 IF USED(lcTmpPrv) .AND. &lcTmpPrv..USER_ID = laData[1] .AND. laScrMode[3]
    llAproval  = &lcTmpPrv..LAPRSHIP
    llAproval2 = &lcTmpPrv..LAPRCSTSH
 ELSE
    IF USED(lctmpprv)
       USE IN (lctmpprv)
    ENDIF
    STORE gfuserpriv('PO','POACFRV','APPROVESHP') TO llaproval
    STORE gfuserpriv('PO','POACFRV','APRVSHPCST') TO llaproval2
 ENDIF
 llok = .F.
 DO (gcscrdir+gcwinappl+'\SMAPRV.SPX')
 IF llok
    IF  .NOT. USED(lctmpprv)
       CREATE CURSOR &lcTmpPrv (LAPRSHIP L,LAPRCSTSH L,USER_ID C(10))
       APPEND BLANK
    ENDIF
    SELECT (lctmpprv)
    REPLACE &lcTmpPrv..USER_ID   WITH laData[1]  &lcTmpPrv..LAPRSHIP  WITH llAproval  &lcTmpPrv..LAPRCSTSH WITH llAproval2
 ENDIF
 SELECT (lnslct)
*
PROCEDURE lfchkusr
 PRIVATE lctmpprv
 lctmpprv = '_'+SUBSTR(lcprv_tmp1, 2)
 IF USED(lcTmpPrv) .AND. &lcTmpPrv..USER_ID <> laData[1]
    USE IN &lcTmpPrv
 ENDIF
*
PROCEDURE lfsavaprv
 PRIVATE lctmpprv
 lctmpprv = '_'+SUBSTR(lcprv_tmp1, 2)
 IF USED(lctmpprv)
    GO TOP IN &lcTmpPrv
    IF  .NOT. SEEK(ALLTRIM(ladata(1))+'PO'+gcact_comp+'POACFRV   ', 'SYUUSRPR')
       SELECT syuusrpr
       APPEND BLANK
       REPLACE cuser_id WITH ladata(1), capp_id WITH 'PO', ccomp_id WITH gcact_comp, cpross_id WITH 'POACFRV   ', cproctype WITH 'P', cgrporuser WITH 'U', lsubproc WITH .T.
    ENDIF
    llShp = &lcTmpPrv..LAPRSHIP
    llCst = &lcTmpPrv..LAPRCSTSH
    SELECT syuusrpr
    REPLACE msubproc WITH IIF(llshp .AND. llcst, 'APPROVESHP,APRVSHPCST|Approve Shipment,Approve Shipment Cost Sheet', IIF(llshp, 'APPROVESHP|Approve Shipment', IIF(llcst, 'APRVSHPCST|Approve Shipment Cost Sheet', '')))
    USE IN &lcTmpPrv
 ENDIF
*
PROCEDURE lfrelgpspd
 RELEASE PAD _option OF _MSYSMENU
*
PROCEDURE lfneedaprv
 IF gfgetmemva('M_APRVSHIP')
    lcstatus = 'ON HOLD'
    ladata[ 2] = 'H'
 ENDIF
*
PROCEDURE lfchkhold
 IF ladata(2)='H'
    lcstatus = 'ON HOLD'
 ENDIF
*
PROCEDURE lfupshpfld
 PRIVATE llsvlogreq, llaprvshp
 llsvlogreq = gllog_requ
 gllog_requ = .T.
 llaprvshp = gfgetmemva('M_APRVSHIP') .AND. (gfuserpriv('PO','POACFRV','APPROVESHP'))
 gllog_requ = llsvlogreq
 IF lascrmode(4)
    IF llaprvshp .AND. ladata(2)='H' .AND. gfmodalgen('INM00000B00006',.F.,.F.,.F.,'Do you want to Approve this Shipment?')=1
       REPLACE status WITH 'O'
       = lfupusrfld('LSHPAPRV',.T.)
       = lfupusrfld('CAPRVUSER',gcuser_id)
       = lfupusrfld('CAPRVDATE',gdsysdate)
       = lfupusrfld('CAPRVTIME',TIME())
    ENDIF
 ELSE
    PRIVATE lcshpaprvl
    lcshpaprvl = '_'+SUBSTR(lcwinch0, 2)
    IF USED(lcshpaprvl)
       GOTO TOP IN (lcshpaprvl)
       IF &lcShpAprvl..LSHPAPRV
          REPLACE status WITH ladata(2)
          = lfupusrfld('LSHPAPRV',.T.)
          = lfupusrfld('CAPRVUSER',gcuser_id)
          = lfupusrfld('CAPRVDATE',gdsysdate)
          = lfupusrfld('CAPRVTIME',TIME())
       ENDIF
       USE IN &lcShpAprvl
    ENDIF
 ENDIF
*
PROCEDURE lfupusrfld
 PARAMETER lcfld, lcvalue
 PRIVATE lnpos
 lnpos = ASCAN(lausrfield, lcfld)
 IF lnpos>0
    lnpos = ASUBSCRIPT(lausrfield, lnpos, 1)
    lausrfield[ lnpos, 6] = lcvalue
 ENDIF
*
PROCEDURE lfaprvship
 PRIVATE lnbarno, llsvlogreq
 llsvlogreq = gllog_requ
 gllog_requ = .T.
 llaprvshp = gfgetmemva('M_APRVSHIP') .AND. (gfuserpriv('PO','POACFRV','APPROVESHP'))
 gllog_requ = llsvlogreq
 lnbarno = CNTBAR('_lPopOpt')+1
 DEFINE BAR lnbarno OF _lpopopt PROMPT '\<Approval Status' SKIP FOR lascrmode(1) .OR. lascrmode(4) .OR. shpmthdr.status$'XH'
 ON SELECTION BAR lnbarno OF _lpopopt DO lfAprvStat IN GPSMAIN.FXP
 IF llaprvshp
    lnbarno = CNTBAR('_lPopOpt')+1
    DEFINE BAR lnbarno OF _lpopopt PROMPT 'A\<pprove Shipment' SKIP FOR  .NOT. lascrmode(3) .OR. shpmthdr.status<>'H'
    ON SELECTION BAR lnbarno OF _lpopopt DO lfApprove IN GPSMAIN.FXP
 ENDIF
*
PROCEDURE lfaprvstat
 IF shpmthdr.status$'CO' .AND. shpmthdr.lshpaprv .AND.  .NOT. EMPTY(shpmthdr.caprvuser)
    lluseropen = .F.
    IF  .NOT. USED('SYUUSER')
       lluseropen = gfopenfile(gcsyshome+'SYUUSER','CUSER_ID','SH')
    ENDIF
    = SEEK(shpmthdr.caprvuser, 'SYUUSER')
    lcmsg = 'Shipment has been approved by:'+CHR(13)+'User :'+ALLTRIM(syuuser.cuser_id)+' '+ALLTRIM(syuuser.cusr_name)+CHR(13)+'Date :'+DTOC(shpmthdr.caprvdate)+CHR(13)+'Time  '+LEFT(shpmthdr.caprvtime, 5)
    = gfmodalgen('INM00000B00000',.F.,.F.,.F.,lcmsg)
    IF shpmthdr.lspcaprv
       lcmsg = 'Ship. cost sheet has been approved by:'+CHR(13)+'User :'+ALLTRIM(syuuser.cuser_id)+' '+ALLTRIM(syuuser.cusr_name)+CHR(13)+'Date :'+DTOC(shpmthdr.cspcaprdat)+CHR(13)+'Time  '+LEFT(shpmthdr.cspcaprtim, 5)
       = gfmodalgen('INM00000B00000',.F.,.F.,.F.,lcmsg)
    ENDIF
    IF lluseropen
       = gfclosefil('SYUUSER')
    ENDIF
 ENDIF
*
PROCEDURE lfapprove
 PRIVATE lnresp, lcshpaprvl
 lcshpaprvl = '_'+SUBSTR(lcwinch0, 2)
 CREATE CURSOR &lcShpAprvl (LSHPAPRV L)
 APPEND BLANK
 lnresp = gfmodalgen('INM34212B00006','DIALOG')
 REPLACE lshpaprv WITH (lnresp=1)
 SET MARK OF BAR BAR() OF _lpopopt TO (lnresp=1)
 ladata[ 2] = IIF(lnresp=1, 'O', 'H')
 lcstatus = IIF(lnresp=1, 'OPEN', 'ON HOLD')
 SHOW GET lcstatus
*
PROCEDURE lfapshpcst
 PRIVATE llsvlogreq, llaprvshp
 llsvlogreq = gllog_requ
 gllog_requ = .T.
 llaprvshp = gfgetmemva('M_APRVSHIP') .AND. (gfuserpriv('PO','POACFRV','APRVSHPCST'))
 gllog_requ = llsvlogreq
 IF llaprvshp
    lnbarno = CNTBAR('_OPTIONPOP')+1
    DEFINE BAR lnbarno OF _optionpop PROMPT 'Approve \<Shipment cost sheet' SKIP FOR  .NOT. llbyshp .OR. lascrmode(1) .OR. lascrmode(2)
    ON SELECTION BAR lnbarno OF _optionpop DO lfvAprSCst IN GPSMAIN.FXP
 ENDIF
 lnbarno = CNTBAR('_OPTIONPOP')+1
 DEFINE BAR lnbarno OF _optionpop PROMPT 'Shipment cost sheet approval status' SKIP FOR  .NOT. llbyshp .OR. lascrmode(1) .OR. lascrmode(4)
 ON SELECTION BAR lnbarno OF _optionpop DO lfvSCstSt IN GPSMAIN.FXP
*
PROCEDURE lfvaprscst
 PRIVATE lctmpaprfl, lnslct
 lnslct = SELECT()
 lctmpaprfl = '_'+SUBSTR(lcwinch0, 2)
 IF  .NOT. USED(lctmpaprfl)
    CREATE CURSOR (lctmpaprfl) (lspcaprv L)
    APPEND BLANK
 ENDIF
 lnresp = gfmodalgen('INM00000B00006',.F.,.F.,.F.,'Do you want to Approve the Shipment Cost Sheet?')
 SET MARK OF BAR BAR() OF _optionpop TO (lnresp=1)
 SELECT (lctmpaprfl)
 REPLACE lspcaprv WITH (lnresp=1)
 SELECT (lnslct)
*
PROCEDURE lfvscstst
 PRIVATE lcmsg
 IF shpmthdr.status$'CO' .AND. shpmthdr.lspcaprv .AND.  .NOT. EMPTY(shpmthdr.cspcaprusr)
    lluseropen = .F.
    IF  .NOT. USED('SYUUSER')
       lluseropen = gfopenfile(gcsyshome+'SYUUSER','CUSER_ID','SH')
    ENDIF
    = SEEK(shpmthdr.cspcaprusr, 'SYUUSER')
    lcmsg = 'Ship. cost sheet has been approved by:'+CHR(13)+'User :'+ALLTRIM(syuuser.cuser_id)+' '+ALLTRIM(syuuser.cusr_name)+CHR(13)+'Date :'+DTOC(shpmthdr.cspcaprdat)+CHR(13)+'Time  '+LEFT(shpmthdr.cspcaprtim, 5)
    = gfmodalgen('INM00000B00000',.F.,.F.,.F.,lcmsg)
    IF lluseropen
       = gfclosefil('SYUUSER')
    ENDIF
 ENDIF
*
PROCEDURE lfaprstat
 PRIVATE lctmpaprfl, lnslct
 lnslct = SELECT()
 lctmpaprfl = '_'+SUBSTR(lcwinch0, 2)
 IF USED(lctmpaprfl)
    = SEEK(lcshipno, 'SHPMTHDR')
    SELECT shpmthdr
    REPLACE LSPCAPRV   WITH &lcTmpAprFl..LSPCAPRV  CSPCAPRUSR WITH IIF(&lcTmpAprFl..LSPCAPRV,gcUser_ID,'')  CSPCAPRDAT WITH IIF(&lcTmpAprFl..LSPCAPRV,gdSysDate,{})  CSPCAPRTIM WITH IIF(&lcTmpAprFl..LSPCAPRV,TIME(),'')
    USE IN &lcTmpAprFl
 ENDIF
 SELECT (lnslct)
*
PROCEDURE lfnoedtdel
 PRIVATE llshpapr, gllog_requ
 gllog_requ = .T.
 IF llbyshp
    = SEEK(lcshipno, 'SHPMTHDR')
    llshpapr = (gfgetmemva('M_APRVSHIP') .AND. gfuserpriv('PO','POACFRV','APRVSHPCST') .AND. shpmthdr.lspcaprv) .OR. shpmthdr.status='C'
    IF llshpapr
       SHOW GET pbedt DISABLE
       SHOW GET pbdlt DISABLE
    ENDIF
 ENDIF
*
FUNCTION lfshpaprvd
 IF gfgetmemva('M_APRVSHIP')
    IF  .NOT. shpmthdr.lspcaprv
       = gfmodalgen('TRM34213B00000','DIALOG')
       RETURN .F.
    ENDIF
 ENDIF
*
PROCEDURE lfgpsroylt
 PRIVATE lnpos, lcitm, lnslct
 lnslct = SELECT()
 SELECT &lcTmpSpLn
 lnpos = ASCAN(lacostitem, '*ROYALT*')
 IF lnpos>0
    lnpos = ASUBSCRIPT(lacostitem, lnpos, 1)
    lcitm = ALLTRIM(STR(lnpos-1))
    REPLACE COST&lcItm WITH NROYALTY
 ENDIF
 SELECT (lnslct)
*
PROCEDURE lfsetdtyrt
 REPLACE nlandurat WITH 1
*
PROCEDURE lfgpsduty
 m.ndutyrat = IIF(shprlfld.ndutyrat=0, poshdr.npricerat, shprlfld.ndutyrat)
*
PROCEDURE lfdutysgns
 lcDPMethod = &lcTmpSpLn..CCURMETH 
 lcDPUnMeth = &lcTmpSpLn..CUNTMETH
*
*** 
*** ReFox - all is not lost 
***

*C201383,1 09/19/2011 MMT Create Auto. Concession Invoice\Credit memo[T20110621.0057][Start]
function  lfvStyle
parameter llRet
PRIVATE lcObjVal
lcObjVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field
IF !USED('STYLE')
  =gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
ENDIF
lnAlsNo = SELECT(0)
SELECT STYLE 
SET ORDER TO STYLE
IF !EMPTY(lcObjVal) AND !SEEK(lcObjVal,'STYLE','STYLE')
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  SELECT STYLE
  lcObjName = SYS(18)           && Varible to hold  the name of the memory variable used to create the current GET field
  DIMENSION laTempData[1]
  laTempData[1] = ''
  
  llObjRet =gfBrows(.F.,'STYLE','laTempData','Style')  
  lcObjVal = IIF(llObjRet , laTempData[1], SPACE(19))
  &lcObjName = lcObjVal
ENDIF  
SELECT(lnAlsNo)
llRet= .T.
RETURN  llRet

FUNCTION lfvDStyle
parameter llRet
PRIVATE lcObjVal
lcObjVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field
IF !USED('STYLE')
  =gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
ENDIF
lnAlsNo = SELECT(0)
SELECT STYLE 
SET ORDER TO STYLE
IF !EMPTY(lcObjVal) AND !SEEK(lcObjVal,'STYLE','STYLE')
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  SELECT STYLE
  lcObjName = SYS(18)           && Varible to hold  the name of the memory variable used to create the current GET field
  DIMENSION laTempData[1]
  laTempData[1] = ''
  
  llObjRet =gfBrows(" FOR styLE.cstygrade = '3'",'STYLE','laTempData','Style')  
  lcObjVal = IIF(llObjRet , laTempData[1], SPACE(19))
  &lcObjName = lcObjVal
ENDIF  
SELECT(lnAlsNo)
llRet= .T.
RETURN  llRet
*C201383,1 09/19/2011 MMT Create Auto. Concession Invoice\Credit memo[T20110621.0057][END]