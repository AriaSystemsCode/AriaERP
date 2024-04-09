*!*****************************************************************************************
*! Name      : PMPRJSCH.prg
*! Developer : Mariam Mazhar (MMT)
*! Date      : 05/10/2009
*! Purpose   : Scheduling
*! Entry no. : N037574
*!*****************************************************************************************
* Modifications:
*B609028,1 MMT 10/07/2009 Fix bug of Wrong calc.date for Completed tasks[T20080429.0012]
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[T20091118.0003]
*B609420,1 MMT 10/27/2010 Fix bug of cannot complete tasks from project monitor screen[T20100929.0001]
*!*****************************************************************************************
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*PARAMETERS lcRequestID, lcXMLFileName
PARAMETERS lcRequestID, lcXMLFileName, ClientId
*T20100512.0026 Hassan 2010 05 23 [END]

IF LEN(lcRequestID) > 2
  PRIVATE loAgent
  loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  PRIVATE loProgress
  loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
  loProgress.Percent = 0
  loProgress.DESCRIPTION = "Opening Data Files..."

  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
  *T20100512.0026 Hassan 2010 05 23 [END]
ENDIF

LOCAL loEnvironment
loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
loEnvironment.ClientId = ClientId
loEnvironment.ConnectionsRefresh()
*T20100512.0026 Hassan 2010 05 23 [END]
LOCAL lcCurrentProcedure
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
*B609420,1 MMT 10/27/2010 Fix bug of cannot complete tasks from project monitor screen[Start]
IF LEN(lcRequestID) > 2
*B609420,1 MMT 10/27/2010 Fix bug of cannot complete tasks from project monitor screen[End]
  loProgress.DESCRIPTION = lcCurrentProcedure
  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
*B609420,1 MMT 10/27/2010 Fix bug of cannot complete tasks from project monitor screen[Start]
ENDIF
*B609420,1 MMT 10/27/2010 Fix bug of cannot complete tasks from project monitor screen[End]  
IF TYPE('llEditAddScr') = 'L' AND llEditAddScr
  SET CLASSLIB TO (lcCurrentProcedure + "SRVCLSS\SY\ariamain.vcx") ADDITIVE
  *B609420,1 MMT 10/27/2010 Fix bug of cannot complete tasks from project monitor screen[Start]
  *oAriaEnvironment = CREATEOBJECT('ariaenvironment', lcRequestID)  
  oAriaEnvironment = CREATEOBJECT('ariaenvironment', lcRequestID,ClientId)
  *B609420,1 MMT 10/27/2010 Fix bug of cannot complete tasks from project monitor screen[End]  
ELSE
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH IIF(LEN(lcRequestID) > 2,loAgent.GetRequestCompany(lcRequestID),lcRequestID)
  DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH IIF(LEN(lcRequestID) > 2,loAgent.GetRequestCompany(lcRequestID,ClientId),lcRequestID), ClientId
  *T20100512.0026 Hassan 2010 05 23 [END]

ENDIF

oAriaEnvironment.XML.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)

oAriaEnvironment.activeModuleID = 'SM'
PUBLIC gcAct_Appl
gcAct_Appl = "SM"


lnPosPrj = ASCAN(laogFXflt,'PMPRJHD.CPRJ_ID')
lcPrjFile = ''
llPrjSelect = .F.
IF lnPosPrj <> 0
  lnPosPrj = ASUBSCRIPT(laogFXflt,lnPosPrj,1)
  lcPrjFile  =  laogFXflt[lnPosPrj,6]
  IF !EMPTY(lcPrjFile) AND USED(lcPrjFile)
    SELECT (lcPrjFile)
    LOCATE
    IF !EOF()
      llPrjSelect = .T.
    ENDIF
  ENDIF
ENDIF

lnPosSty= ASCAN(laogFXflt,'STYLE.CSTYMAJOR')
lcStyFile = ''
llStySelect = .F.
IF lnPosSty <> 0
  lnPosSty = ASUBSCRIPT(laogFXflt,lnPosSty,1)
  lcStyFile =  laogFXflt[lnPosSty,6]
  IF !EMPTY(lcStyFile) AND USED(lcStyFile)
    SELECT (lcStyFile)
    LOCATE
    IF !EOF()
      llStySelect = .T.
    ENDIF
  ENDIF
ENDIF



IF !llEditAdd
  =oAriaEnvironment.remotetableaccess.OPENTABLE('PMPRJHD','PMPRJHD')
  =oAriaEnvironment.remotetableaccess.OPENTABLE('PMCALDT','PMCALDT')
  =oAriaEnvironment.remotetableaccess.OPENTABLE('SYSCHDUL','COPRUSR')
  =oAriaEnvironment.remotetableaccess.OPENTABLE('PMPRJRL','PMPRJRL')
  =oAriaEnvironment.remotetableaccess.OPENTABLE('PMCALHD','PMCALHD')
  =oAriaEnvironment.remotetableaccess.OPENTABLE('PMPRJDT','PMPRJDT')
ELSE
  SET DATASESSION TO lnFrmDtSe
ENDIF


IF llPrjSelect
  SELECT (lcPrjFile)
  LOCATE
  lnPrjCnt = RECCOUNT()
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *SCAN FOR '-' $ KeyExp
  SCAN FOR '+' $ KeyExp
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    lnPerCent = RECNO()/lnPrjCnt
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*	    lnDashPo = ATC('-',KeyExp)
    *!*	    lcPrjId = SUBSTR(KeyExp,1,lnDashPo-1)
    *!*	    lcStyle = SUBSTR(SUBSTR(KeyExp,lnDashPo+1),1,12)
    lnDashPo = ATC('+',KeyExp,1)
    lcPrjId = SUBSTR(KeyExp,1,lnDashPo-1)
    lnDashPo2 = ATC('+',KeyExp,2)
    lcStyle = SUBSTR(SUBSTR(KeyExp,lnDashPo+1,lnDashPo2-1),1,19)
    lcLineNo = SUBSTR(KeyExp,lnDashPo2+1)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

    IF llStySelect  AND !SEEK(ALLTRIM(lcStyle),lcStyFile)
      LOOP
    ENDIF

    IF LEN(lcRequestID) > 2 AND MOD(RECNO(), CEILING(lnPrjCnt / 10)) = 0
      loProgress.Percent = lnPerCent
      loProgress.DESCRIPTION = "Scheduling Project:"+lcPrjId
      *T20100512.0026 Hassan 2010 05 23 [BEGIN]
      *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
      loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
      *T20100512.0026 Hassan 2010 05 23 [END]
    ENDIF

    IF lcRpPrjTyp $ 'MS'
      lcPrjId = SPACE(6)
    ENDIF
    IF lcRpPrjTyp = 'H'
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *lcStyle = SPACE(12)
      lcStyle = SPACE(19)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    ENDIF
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *IF llEditAdd OR oAriaEnvironment.remotetableaccess.SeekRecord(lcRpPrjTyp+PADR(lcPrjId,6)+ALLTRIM(lcStyle ),'PMPRJHD')
    *  lfSchedule(lcRpPrjTyp,lcPrjId,lcStyle,ldRpSchDt)
    IF llEditAdd OR oAriaEnvironment.remotetableaccess.SeekRecord(lcRpPrjTyp+PADR(lcPrjId,6)+PADR(lcStyle,19)+lcLineNo ,'PMPRJHD')
      lfSchedule(lcRpPrjTyp,lcPrjId,lcStyle,VAL(lcLineNo),ldRpSchDt)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    ENDIF
  ENDSCAN
ELSE
  IF llStySelect AND !llEditAdd
    SELECT 'PMPRJHD'
    = oAriaEnvironment.remotetableaccess.SeekRecord(lcRpPrjTyp)
    SELECT (lcStyFile)
    LOCATE
    lnPrjCnt = RECCOUNT()
    SCAN
      lnPerCent = RECNO()/lnPrjCnt
      SELECT PMPRJHD
      *SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE = lcRpPrjTyp FOR Cstyle = ALLTRIM(&lcStyFile..KeyExp)
      SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+STR(LINENO,6) = lcRpPrjTyp FOR CSTYLE = ALLTRIM(&lcStyFile..KeyExp)
        lnRecNumber = RECNO()
        IF LEN(lcRequestID) > 2 AND MOD(RECNO(), CEILING(lnPrjCnt / 10)) = 0
          loProgress.Percent = lnPerCent
          loProgress.DESCRIPTION = "Scheduling Project:"+PMPRJHD.CPRJ_ID
          *T20100512.0026 Hassan 2010 05 23 [BEGIN]
          *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
          loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
          *T20100512.0026 Hassan 2010 05 23 [END]
        ENDIF
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *lfSchedule(lcRpPrjTyp,PMPRJHD.CPRJ_ID,PMPRJHD.cStyle,ldRpSchDt)
        lfSchedule(lcRpPrjTyp,PMPRJHD.CPRJ_ID,PMPRJHD.CSTYLE,PMPRJHD.LINENO,ldRpSchDt)
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        SELECT PMPRJHD
        IF BETWEEN(lnRecNumber ,1,RECCOUNT())
          GO RECORD lnRecNumber
        ENDIF
      ENDSCAN
    ENDSCAN
  ELSE
    IF !llEditAdd
      SELECT 'PMPRJHD'
      = oAriaEnvironment.remotetableaccess.SeekRecord(lcRpPrjTyp)
      lnPrjCnt = RECCOUNT()
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE = lcRpPrjTyp
      SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+STR(LINENO,6) = lcRpPrjTyp
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        lnRecNumber = RECNO()
        lnPerCent = RECNO()/lnPrjCnt
        IF LEN(lcRequestID) > 2 AND MOD(RECNO(), CEILING(lnPrjCnt / 10)) = 0
          loProgress.Percent = lnPerCent
          loProgress.DESCRIPTION = "Scheduling Project:"+PMPRJHD.CPRJ_ID
          *T20100512.0026 Hassan 2010 05 23 [BEGIN]
          *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
          loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientId)
          *T20100512.0026 Hassan 2010 05 23 [END]
        ENDIF
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *lfSchedule(lcRpPrjTyp,PMPRJHD.CPRJ_ID,PMPRJHD.cStyle,ldRpSchDt)
        lfSchedule(lcRpPrjTyp,PMPRJHD.CPRJ_ID,PMPRJHD.CSTYLE,PMPRJHD.LINENO,ldRpSchDt)
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        SELECT PMPRJHD
        IF BETWEEN(lnRecNumber ,1,RECCOUNT())
          GO RECORD lnRecNumber
        ENDIF
      ENDSCAN
    ENDIF
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfschedule
*: Developer : Mariam Mazhar[MMT]
*: Date      : 05/10/2009
*! Purpose   : function to Schedule Projects
*!*************************************************************
FUNCTION lfSchedule
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*PARAMETERS lcProjType,lcProjID,lcPStyle,ldSchDate,lcSDir,lcTempName,llMSchdul
PARAMETERS lcProjType,lcProjID,lcPStyle,lnLineNo,ldSchDate,lcSDir,lcTempName,llMSchdul
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
IF !llEditAdd
  *= oAriaEnvironment.remotetableaccess.SeekRecord(lcProjType+lcProjID+lcPStyle,'PMPRJHD','PMPRJHD')
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*	  = oAriaEnvironment.remotetableaccess.SeekRecord(lcProjType+lcProjID+lcPStyle,'PMPRJDT','PMPRJDT')
  *!*	  = oAriaEnvironment.remotetableaccess.SeekRecord(lcProjType+lcProjID+lcPStyle,'PMPRJRL','PMPRJRL')
  = oAriaEnvironment.remotetableaccess.SeekRecord(lcProjType+lcProjID+lcPStyle+STR(lnLineNo,6),'PMPRJDT','PMPRJDT')
  = oAriaEnvironment.remotetableaccess.SeekRecord(lcProjType+lcProjID+lcPStyle+STR(lnLineNo,6),'PMPRJRL','PMPRJRL')
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  lcTmpPrjRl = 'PMPRJRL'
  lcPrjHder = 'PMPRJHD'
ENDIF


lcPrj_Typ = lcProjType
lcPrj_ID  = lcProjID
lcStyle  = lcPStyle

DIMENSION  laHolidays[1]
laHolidays = ''
lnRows  = 0
SELECT PMCALDT

IF !llEditAdd
  = oAriaEnvironment.remotetableaccess.SeekRecord("")
ELSE
  =gfSeek('')
ENDIF

SCAN
  ldCal_HFrm = dCal_HFrm
  DO WHILE ldCal_HFrm <= dCal_HTo
    lnRows  = lnRows  + 1
    DIMENSION laHolidays[lnRows]
    laHolidays[lnRows] = cCal_ID + DTOC(ldCal_HFrm)
    ldCal_HFrm = ldCal_HFrm + 1
  ENDDO
ENDSCAN

IF !llEditAdd AND PMPRJHD.llok_stat AND PMPRJHD.cLok_User <> oAriaEnvironment.User_ID
  *=gfModalgen("TRM00029B38030","DIALOG")
ELSE
  IF !llEditAdd
    SELECT PMPRJHD
    =oAriaEnvironment.remotetableaccess.REPLACERECORD("llok_stat WITH .T.")
    =oAriaEnvironment.remotetableaccess.TABLEUPDATE()
    =oAriaEnvironment.remotetableaccess.REPLACERECORD("dDta_Date WITH ldSchDate ")
    =oAriaEnvironment.remotetableaccess.REPLACERECORD("lSchedual WITH .F.")
    =oAriaEnvironment.remotetableaccess.REPLACERECORD("llok_stat WITH .F.")
    =oAriaEnvironment.remotetableaccess.REPLACERECORD("cPrj_Stts WITH 'I'")
  ELSE
    SELECT (lcPrjHder)
    REPLACE dDta_Date WITH ldSchDate ,;
      lSchedual WITH .F.,;
      cPrj_Stts WITH 'I'
  ENDIF
  dSch_Date = ldSchDate
  =lfGetClcDt()
  IF !llEditAdd
    SELECT PMPRJHD
    oAriaEnvironment.remotetableaccess.TABLEUPDATE()
    SELECT PMPRJDT
    oAriaEnvironment.remotetableaccess.TABLEUPDATE()
    SELECT PMPRJRL
    oAriaEnvironment.remotetableaccess.TABLEUPDATE()
    SELECT SYSCHDUL
    oAriaEnvironment.remotetableaccess.TABLEUPDATE()
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfGetClcDt
*! Developer : Mariam MAzhar
*! Date      : 05/10/2009
*! Purpose   : Calculates calculated dates for all operations
*!             as well as the calculated finish date for the
*!             project based on the calculated start date.
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetClcDt()
*!*************************************************************
FUNCTION lfGetClcDt
PRIVATE lnCurAlias

lnCurAlias = SELECT(0)

*lcTmpPrjDt = IIF(llMSchdul,lc_PmPrjDt,'PMPRJDT')
*lcTmpPrjRl = IIF(llMSchdul,lc_PmPrjRl,'PMPRJRL')
IF !llEditAdd
  lcTmpPrjDt = 'PMPRJDT'
  lcTmpPrjRl = 'PMPRJRL'
ENDIF
*-- Change the tag of Project relations file to predecessors,
SELECT (lcTmpPrjRl)

IF !llEditAdd
  =oAriaEnvironment.remotetableaccess.SetOrderTO('PMPRJRLP')
ELSE
  =gfSetOrder('PMPRJRLP')
  SET ORDER TO 'PMPRJRLP'
ENDIF

SELECT (lcTmpPrjDt)
lnCurTag = ORDER()

IF !llEditAdd
  =oAriaEnvironment.remotetableaccess.SetOrderTO("PMPRJDT")
  lc_Parser = oAriaEnvironment.CURSORS.getcursortempname ()
ELSE
  =gfSetOrder('PMPRJDT')
  SET ORDER TO PMPRJDT
  lc_Parser = gfTempName()
ENDIF


CREATE CURSOR (lc_Parser);
  (cOprt_Ctg C(3), cOprt_ID C(5), dStrtDate D(8), nDurIndic N(1))


SELECT (lc_Parser)

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*SET RELATION TO SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjDt..cPrj_ID)) +;
SUBSTR(lcStyle,1,LEN(&lcTmpPrjDt..cStyle))+ cOprt_Ctg + cOprt_ID INTO (lcTmpPrjDt)
SET RELATION TO SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjDt..CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjDt..CPRJ_ID)) +;
  SUBSTR(lcStyle,1,LEN(&lcTmpPrjDt..CSTYLE))+STR(lnLineNo,6)+ cOprt_Ctg + cOprt_ID INTO (lcTmpPrjDt)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]


SELECT (lcTmpPrjDt)
SET RELATION OFF INTO PMPRJRL

*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*SET FILTER TO  cPrj_Typ +  cPrj_ID +  cStyle + cOprt_Ctg + cOprt_ID = ;
SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjRl..cPrj_ID)) +;
SUBSTR(lcStyle,1,LEN(&lcTmpPrjRl..cStyle));
.AND. !lVoid
SET FILTER TO  CPRJ_TYP +  CPRJ_ID +  CSTYLE +STR(LINENO,6)+ cOprt_Ctg + cOprt_ID = ;
  SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjRl..CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjRl..CPRJ_ID)) +;
  SUBSTR(lcStyle,1,LEN(&lcTmpPrjRl..CSTYLE))+STR(lnLineNo,6);
  .AND. !lVoid
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]


llonSchAdd = .F.
IF &lcPrjHder..LLASTSTRT
  LOCATE FOR !ISNULL(DONSCHDL) AND !EMPTY(DONSCHDL)
  IF FOUND()
    llonSchAdd = .T.
  ENDIF
ENDIF

*B609028,1 MMT 10/07/2009 Fix bug of Wrong calc.date for Completed tasks[Start]
SELECT * FROM (lcTmpPrjDt) WHERE !EMPTY(dAct_strt) AND !EMPTY(dact_fnsh)  INTO CURSOR 'CmplTsks'
SELECT (lcTmpPrjDt)
*B609028,1 MMT 10/07/2009 Fix bug of Wrong calc.date for Completed tasks[End]

LOCATE

SCAN
  IF !llEditAdd
    =oAriaEnvironment.remotetableaccess.REPLACERECORD("dclc_strt WITH {},"+;
      "dclc_fnsh WITH {}")
  ELSE
    REPLACE dclc_strt WITH {},;
      dclc_fnsh WITH {}

    IF TYPE("cStatus") <> 'U'
      REPLACE cStatus WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
    ENDIF
  ENDIF
ENDSCAN

LOCATE
IF llonSchAdd
  *Case of Last Start Date
  * Will start scheduling from last task till first one
  SELECT (lcTmpPrjRl)
  IF !llEditAdd
    =oAriaEnvironment.remotetableaccess.SetOrderTO('PMPRJRL')
  ELSE
    =gfSetOrder('PMPRJRL')
    SET ORDER TO PMPRJRL
  ENDIF

  SELECT (lcTmpPrjDt)
  SET ORDER TO PMPRJDT DESCENDING
  LOCATE
  dSch_Date = &lcPrjHder..DEST_FNSH

  SCAN FOR EMPTY(dclc_strt)
    lcCurOprt = cOprt_Ctg + cOprt_ID
    SELECT (lcTmpPrjDt)
    ldClcStDt ={}
    IF !ISNULL(DONSCHDL) AND !EMPTY(DONSCHDL)
      ldFnshDt = DONSCHDL
    ELSE
      ldFnshDt = dSch_Date
    ENDIF
    *!*      IF lnPrvDur > 0 AND nest_dur = 0
    *!*        ldFnshDt  = ldFnshDt +1
    *!*      ENDIF
    *!*      IF lnPrvDur = 0 AND nest_dur > 0
    *!*        ldFnshDt  = ldFnshDt -1
    *!*      ENDIF
    =lfAdjDate(ldFnshDt, @ldClcStDt,;
      MAX(0, IIF(nAct_dur > 0, nAct_dur, nest_dur) - 1),;
      cCal_ID, '-')

    IF !llEditAdd
      oAriaEnvironment.remotetableaccess.REPLACERECORD('DCLC_STRT  With ldClcStDt,dclc_fnsh with ldFnshDt')
    ELSE
      gfReplace('DCLC_STRT  With ldClcStDt,dclc_fnsh With ldFnshDt')
    ENDIF
    ldFnshDt  = ldClcStDt && - MIN(nest_dur,1)
    &&lnPrvDur = nest_dur
    =lfUpdClcDt(dclc_strt)
    lcThsOprt = cOprt_Ctg + cOprt_ID
    lldclc_fsh = dclc_strt
    lnDurIndic = MIN(nest_dur,1)

    SELECT SYSCHDUL

    IF !llEditAdd
      =oAriaEnvironment.remotetableaccess.SetOrderTO('Coprusr')
    ELSE
      =gfSetOrder('Coprusr')
    ENDIF

    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*	    IF  (!llEditAdd AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcThsOprt)) OR ;
    *!*	  	  (llEditAdd AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcThsOprt))
    IF  (!llEditAdd AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) +;
        SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19) +STR(lnLineNo,6)+ lcThsOprt)) OR ;
        (llEditAdd AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19)+STR(lnLineNo,6) + lcThsOprt))
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

      IF SYSCHDUL.COPERSTAT = 'C'
        lcStauts = SYSCHDUL.COPERSTAT
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*	        LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
        *!*	                    SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcThsOprt;
        *!*	                    FOR COPERSTAT <> lcStauts
        LOCATE REST WHILE cconttype+cseqnumber+CSTYLE+STR(LINENO,6)+ccont_id+COPERSTAT+cuser_id =;
          SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19) +STR(lnLineNo,6)+ lcThsOprt;
          FOR COPERSTAT <> lcStauts
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

      ENDIF
      IF !llEditAdd
        =oAriaEnvironment.remotetableaccess.REPLACERECORD("dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),"+;
          "dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh)")
      ELSE
        REPLACE dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),;
          dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_fnsh),&lcTmpPrjDt..DEST_FNSH,&lcTmpPrjDt..dclc_fnsh)
      ENDIF
    ENDIF
    SELECT (lcTmpPrjRl)

    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*	    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*	          SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
    *!*	      SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+COPRT_CTG+COPRT_ID+CPRD_CTG+CPRD_ID  = ;
    *!*	                   SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*	                   SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt
    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
        SUBSTR(lcStyle,1,LEN(CSTYLE)) +STR(lnLineNo,6)+ lcCurOprt)
      SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+STR(LINENO,6)+cOprt_Ctg+cOprt_ID+CPRD_CTG+CPRD_ID  = ;
          SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
          SUBSTR(lcStyle,1,LEN(CSTYLE))+STR(lnLineNo,6) + lcCurOprt
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        lcNxtOprt = cOprt_Ctg + cOprt_ID
        INSERT INTO (lc_Parser);
          VALUES(&lcTmpPrjRl..CPRD_CTG, &lcTmpPrjRl..CPRD_ID, lldclc_fsh, lnDurIndic)
      ENDSCAN
    ENDIF
    SELECT (lc_Parser)
    GO TOP
    IF EOF()
      SELECT (lcTmpPrjDt)
      IF !llEditAdd
        =oAriaEnvironment.remotetableaccess.SetOrderTO('PMPRJDT')
      ELSE
        =gfSetOrder('PMPRJDT')
        SET ORDER TO PMPRJDT
      ENDIF

      =SEEK(SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
        SUBSTR(lcStyle,1,LEN(CSTYLE)) + lcCurOprt)

      IF !EMPTY(&lcTmpPrjDt..cOprt_res) OR !EMPTY(&lcTmpPrjDt..cGroup_Id)
        SELECT SYSCHDUL
        IF !llEditAdd
          =oAriaEnvironment.remotetableaccess.SetOrderTO('Coprusr')
        ELSE
          SET ORDER TO 'Coprusr'
        ENDIF
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*	        IF  (!llEditAdd  AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcCurOprt)) OR ;
        *!*	          (llEditAdd  AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcCurOprt))
        IF  (!llEditAdd  AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19)+STR(lnLineNo,6) + lcCurOprt)) OR ;
            (llEditAdd  AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19) +STR(lnLineNo,6)+ lcCurOprt))
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          IF SYSCHDUL.COPERSTAT = 'C'
            lcStauts = SYSCHDUL.COPERSTAT

            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
            *!*	            LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
            *!*	                        SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcCurOprt;
            *!*	                        FOR COPERSTAT <> lcStauts
            LOCATE REST WHILE cconttype+cseqnumber+CSTYLE+STR(LINENO,6)+ccont_id+COPERSTAT+cuser_id =;
              SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19) +STR(lnLineNo,6)+ lcCurOprt;
              FOR COPERSTAT <> lcStauts
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

          ENDIF
          IF !llEditAdd
            =oAriaEnvironment.remotetableaccess.REPLACERECORD("dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),"+;
              "dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh),"+;
              "lpredcomp  WITH .T.")
          ELSE
            gfReplace("dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),"+;
              " dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh),"+;
              "lpredcomp  WITH .T.")
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    SELECT (lc_Parser)
    DO WHILE !EOF()
      lcNxtOprt  = cOprt_Ctg + cOprt_ID
      lnRecNo    = RECNO()
      SELECT (lcTmpPrjDt)
      ldClcStDt = {}
      IF !ISNULL(DONSCHDL) AND !EMPTY(DONSCHDL)
        ldFnshDt = DONSCHDL
      ELSE
        ldFnshDt = IIF(nest_dur = 0, &lc_Parser..dStrtDate,;
          &lc_Parser..dStrtDate - &lc_Parser..nDurIndic)
      ENDIF

      =lfAdjDate(ldFnshDt, @ldClcStDt,;
        MAX(0, IIF(nAct_dur > 0, nAct_dur, nest_dur) - 1),;
        cCal_ID, '-')

      IF !llEditAdd
        oAriaEnvironment.remotetableaccess.REPLACERECORD('DCLC_STRT  With ldClcStDt,dclc_fnsh with ldFnshDt')
      ELSE
        gfReplace('DCLC_STRT  With ldClcStDt,dclc_fnsh With ldFnshDt')
      ENDIF

      =lfUpdClcDt(dclc_strt)
      lldclc_fsh = dclc_strt
      lnDurIndic = MAX(lnDurIndic, MIN(nest_dur,1))
      SELECT (lcTmpPrjRl)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*	      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
      *!*	            SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt)
      *!*	        SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+COPRT_CTG+COPRT_ID+CPRD_CTG+CPRD_ID = ;
      *!*	                     SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
      *!*	                     SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt
      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
          SUBSTR(lcStyle,1,LEN(CSTYLE)) +STR(lnLineNo,6)+ lcNxtOprt)
        SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+STR(LINENO,6)+cOprt_Ctg+cOprt_ID+CPRD_CTG+CPRD_ID = ;
            SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
            SUBSTR(lcStyle,1,LEN(CSTYLE)) +STR(lnLineNo,6)+ lcNxtOprt
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          lcNxtOnPth = cOprt_Ctg + cOprt_ID
          INSERT INTO (lc_Parser);
            VALUES(&lcTmpPrjRl..CPRD_CTG, &lcTmpPrjRl..CPRD_ID, lldclc_fsh, lnDurIndic)
        ENDSCAN
      ENDIF
      IF !EMPTY(&lcTmpPrjDt..cOprt_res) OR !EMPTY(&lcTmpPrjDt..cGroup_Id)
        SELECT SYSCHDUL
        IF !llEditAdd
          =oAriaEnvironment.remotetableaccess.SetOrderTO('Coprusr')
        ELSE
          =gfSetOrder('Coprusr')
        ENDIF
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*	        IF  (!llEditAdd AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcNxtOprt)) OR ;
        *!*	            (llEditAdd AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcNxtOprt))
        IF  (!llEditAdd AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) +;
            SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19)+STR(lnLineNo,6) + lcNxtOprt)) OR ;
            (llEditAdd AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19) +STR(lnLineNo,6)+ lcNxtOprt))
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

          IF SYSCHDUL.COPERSTAT = 'C'
            lcStauts = SYSCHDUL.COPERSTAT
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
            *!*	            LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
            *!*	                      SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcNxtOprt ;
            *!*	                      FOR COPERSTAT <> lcStauts
            LOCATE REST WHILE cconttype+cseqnumber+CSTYLE+STR(LINENO,6)+ccont_id+COPERSTAT+cuser_id =;
              SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19) +STR(lnLineNo,6)+ lcNxtOprt ;
              FOR COPERSTAT <> lcStauts
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

          ENDIF
          IF !llEditAdd
            =oAriaEnvironment.remotetableaccess.REPLACERECORD("dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),"+;
              "dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh)")
          ELSE
            REPLACE dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),;
              dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_fnsh),&lcTmpPrjDt..DEST_FNSH,&lcTmpPrjDt..dclc_fnsh)
          ENDIF
        ENDIF
      ENDIF
      SELECT (lc_Parser)
      IF BETWEEN(lnRecNo, 1, RECCOUNT())
        GO lnRecNo
      ENDIF
      SKIP
    ENDDO
    SELECT (lc_Parser)
    ZAP
    SELECT (lcTmpPrjDt)
    IF !llEditAdd
      =oAriaEnvironment.remotetableaccess.SetOrderTO('PMPRJDT')
    ELSE
      =gfSetOrder('PMPRJDT')
      SET ORDER TO 'PMPRJDT'
    ENDIF
    SET ORDER TO 'PMPRJDT' DESCENDING
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*	    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*	          SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
      SUBSTR(lcStyle,1,LEN(CSTYLE)) +STR(lnLineNo,6)+ lcCurOprt)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  ENDSCAN
  SET ORDER TO PMPRJDT ASCENDING
ELSE
  SCAN FOR EMPTY(dclc_strt)
    *-- Save the current operation into a variable.
    lcCurOprt = cOprt_Ctg + cOprt_ID

    *-- Update dates, and get the new finish date.
    SELECT (lcTmpPrjDt)

    *-- For the first operation, use the scheduling date as a
    *-- candidate calculated start date
    =lfUpdClcDt(dSch_Date)
    lcThsOprt = cOprt_Ctg + cOprt_ID

    lldclc_fsh = dclc_fnsh
    lnDurIndic = MIN(nest_dur,1)


    SELECT SYSCHDUL

    IF !llEditAdd
      =oAriaEnvironment.remotetableaccess.SetOrderTO('Coprusr')
    ELSE
      =gfSetOrder('Coprusr')
    ENDIF
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*	    IF  (!llEditAdd AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcThsOprt)) OR ;
    *!*	      (llEditAdd AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcThsOprt))
    IF  (!llEditAdd AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) +;
        SUBSTR(lcStyle,1,19)+STR(lnLineNo,6)   + lcThsOprt)) OR ;
        (llEditAdd AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + ;
        SUBSTR(lcStyle,1,19) +STR(lnLineNo,6)  + lcThsOprt))
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

      IF SYSCHDUL.COPERSTAT = 'C'
        lcStauts = SYSCHDUL.COPERSTAT
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*	        LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
        *!*	                    SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcThsOprt;
        *!*	                    FOR COPERSTAT <> lcStauts
        LOCATE REST WHILE cconttype+cseqnumber+CSTYLE+STR(LINENO,6)+ccont_id+COPERSTAT+cuser_id =;
          SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19)+STR(lnLineNo,6) + lcThsOprt;
          FOR COPERSTAT <> lcStauts
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

      ENDIF
      IF !llEditAdd
        =oAriaEnvironment.remotetableaccess.REPLACERECORD("dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),"+;
          "dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh)")
      ELSE
        REPLACE dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),;
          dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_fnsh),&lcTmpPrjDt..DEST_FNSH,&lcTmpPrjDt..dclc_fnsh)
      ENDIF
    ENDIF



    *-- if the new operation is a predecessor to other operations,
    *-- copy their IDs to the cursor.
    SELECT (lcTmpPrjRl)

    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*	    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*	            SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
    *!*	      SCAN REST WHILE cPrj_Typ +  cPrj_ID + cStyle  +  cPrd_Ctg + cPrd_ID = ;
    *!*	                     SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*	                     SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt
    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
        SUBSTR(lcStyle,1,LEN(CSTYLE)) +STR(lnLineNo,6)+ lcCurOprt)
      SCAN REST WHILE CPRJ_TYP +  CPRJ_ID + CSTYLE +STR(LINENO,6) +  CPRD_CTG + CPRD_ID = ;
          SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
          SUBSTR(lcStyle,1,LEN(CSTYLE))+STR(lnLineNo,6) + lcCurOprt
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
        lcNxtOprt = cOprt_Ctg + cOprt_ID
        INSERT INTO (lc_Parser);
          VALUES(&lcTmpPrjRl..cOprt_Ctg, &lcTmpPrjRl..cOprt_ID, lldclc_fsh, lnDurIndic)


      ENDSCAN
    ENDIF

    SELECT (lc_Parser)
    GO TOP
    IF EOF()
      SELECT (lcTmpPrjDt)

      IF !llEditAdd
        =oAriaEnvironment.remotetableaccess.SetOrderTO('PMPRJDT')
      ELSE
        =gfSetOrder('PMPRJDT')
        SET ORDER TO PMPRJDT
      ENDIF

      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*	      =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
      *!*	          SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
      =SEEK(SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
        SUBSTR(lcStyle,1,LEN(CSTYLE))+STR(lnLineNo,6) + lcCurOprt)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

      IF !EMPTY(&lcTmpPrjDt..cOprt_res) OR !EMPTY(&lcTmpPrjDt..cGroup_Id)
        SELECT SYSCHDUL
        IF !llEditAdd
          =oAriaEnvironment.remotetableaccess.SetOrderTO('Coprusr')
        ELSE
          SET ORDER TO 'Coprusr'
        ENDIF
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*	      IF  (!llEditAdd  AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcCurOprt)) OR ;
        *!*	        (llEditAdd  AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcCurOprt))
        IF  (!llEditAdd  AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19) +STR(lnLineNo,6)+ lcCurOprt)) OR ;
            (llEditAdd  AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19) +STR(lnLineNo,6)+ lcCurOprt))
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

          IF SYSCHDUL.COPERSTAT = 'C'
            lcStauts = SYSCHDUL.COPERSTAT
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
            *!*	          LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
            *!*	                      SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcCurOprt;
            *!*	                      FOR COPERSTAT <> lcStauts
            LOCATE REST WHILE cconttype+cseqnumber+CSTYLE+STR(LINENO,6)+ccont_id+COPERSTAT+cuser_id =;
              SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19) +STR(lnLineNo,6)+ lcCurOprt;
              FOR COPERSTAT <> lcStauts
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          ENDIF
          IF !llEditAdd
            =oAriaEnvironment.remotetableaccess.REPLACERECORD("dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),"+;
              "dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh),"+;
              "lpredcomp  WITH .T.")
          ELSE
            gfReplace("dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),"+;
              " dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh),"+;
              "lpredcomp  WITH .T.")
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    SELECT (lc_Parser)
    DO WHILE !EOF()
      *-- Update the estimated date of the current detail record
      *-- Add category code field
      lcNxtOprt  = cOprt_Ctg + cOprt_ID
      lnRecNo    = RECNO()

      SELECT (lcTmpPrjDt)

      =lfUpdClcDt(IIF(nest_dur = 0, &lc_Parser..dStrtDate,;
        &lc_Parser..dStrtDate + &lc_Parser..nDurIndic))

      lldclc_fsh = dclc_fnsh

      lnDurIndic = MAX(lnDurIndic, MIN(nest_dur,1))
      SELECT (lcTmpPrjRl)
      *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
      *!*	    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
      *!*	            SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt)
      *!*	      SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cPrd_Ctg + cPrd_ID = ;
      *!*	                     SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
      *!*	                     SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt
      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
          SUBSTR(lcStyle,1,LEN(CSTYLE))+STR(lnLineNo,6) + lcNxtOprt)
        SCAN REST WHILE CPRJ_TYP + CPRJ_ID + CSTYLE +STR(LINENO,6)+ CPRD_CTG + CPRD_ID = ;
            SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
            SUBSTR(lcStyle,1,LEN(CSTYLE))+STR(lnLineNo,6) + lcNxtOprt
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          lcNxtOnPth = cOprt_Ctg + cOprt_ID
          INSERT INTO (lc_Parser);
            VALUES(&lcTmpPrjRl..cOprt_Ctg, &lcTmpPrjRl..cOprt_ID, lldclc_fsh, lnDurIndic)
        ENDSCAN
      ENDIF

      IF !EMPTY(&lcTmpPrjDt..cOprt_res) OR !EMPTY(&lcTmpPrjDt..cGroup_Id)
        SELECT SYSCHDUL
        IF !llEditAdd
          =oAriaEnvironment.remotetableaccess.SetOrderTO('Coprusr')
        ELSE
          =gfSetOrder('Coprusr')
        ENDIF
        *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
        *!*	      IF (!llEditAdd AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcNxtOprt)) OR ;
        *!*	         (llEditAdd AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcNxtOprt))
        IF (!llEditAdd AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) +;
            SUBSTR(lcStyle,1,19)  +STR(lnLineNo,6)+ lcNxtOprt)) OR ;
            (llEditAdd AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19)  +STR(lnLineNo,6)+ lcNxtOprt))
          *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          IF SYSCHDUL.COPERSTAT = 'C'
            lcStauts = SYSCHDUL.COPERSTAT

            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
            *!*	          LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
            *!*	                      SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcNxtOprt ;
            *!*	                      FOR COPERSTAT <> lcStauts
            LOCATE REST WHILE cconttype+cseqnumber+CSTYLE+STR(LINENO,6)+ccont_id+COPERSTAT+cuser_id =;
              SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,19) +STR(lnLineNo,6)+ lcNxtOprt ;
              FOR COPERSTAT <> lcStauts
            *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
          ENDIF
          IF !llEditAdd
            =oAriaEnvironment.remotetableaccess.REPLACERECORD("dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),"+;
              "dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh)")
          ELSE
            REPLACE dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),;
              dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_fnsh),&lcTmpPrjDt..DEST_FNSH,&lcTmpPrjDt..dclc_fnsh)
          ENDIF
        ENDIF
      ENDIF
      SELECT (lc_Parser)
      IF BETWEEN(lnRecNo, 1, RECCOUNT())
        GO lnRecNo
      ENDIF
      SKIP
    ENDDO

    SELECT (lc_Parser)
    ZAP

    SELECT (lcTmpPrjDt)
    IF !llEditAdd
      =oAriaEnvironment.remotetableaccess.SetOrderTO('PMPRJDT')
    ELSE
      =gfSetOrder('PMPRJDT')
      SET ORDER TO 'PMPRJDT'
    ENDIF
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*	  =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*	       SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
      SUBSTR(lcStyle,1,LEN(CSTYLE))+STR(lnLineNo,6) + lcCurOprt)
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
  ENDSCAN

ENDIF




*!*  llClcDates = .F.
*!*  IF &lcPrjHder..LLASTSTRT  AND llonSchAdd
*!*    SET ORDER TO PMPRJDT DESCENDING
*!*    LOCATE
*!*    *Here is the last group scheduling
*!*    lcLastGroup = COPRT_CTG
*!*    ldFnshDt = &lcPrjHder..DEST_FNSH
*!*    lnPrvDur = 0
*!*    SCAN WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+COPRT_CTG+COPRT_ID  = SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjRl..cPrj_ID)) +;
*!*                SUBSTR(lcStyle,1,LEN(&lcTmpPrjRl..cStyle)) FOR COPRT_CTG = lcLastGroup
*!*
*!*      IF lnPrvDur > 0 AND nest_dur = 0
*!*        ldFnshDt  = ldFnshDt +1
*!*      ENDIF
*!*      IF lnPrvDur = 0 AND nest_dur > 0  AND llClcDates
*!*        ldFnshDt  = ldFnshDt -1
*!*      ENDIF

*!*      ldClcStDt ={}
*!*      llClcDates  = .T.
*!*      =lfAdjDate(ldFnshDt, @ldClcStDt,;
*!*                 MAX(0, IIF(nAct_dur > 0, nAct_dur, nest_dur) - 1),;
*!*                 cCal_ID, '-')
*!*
*!*      IF !llEditAdd
*!*        oAriaEnvironment.remotetableaccess.ReplaceRecord('DCLC_STRT  With ldClcStDt,dclc_fnsh with ldFnshDt')
*!*      ELSE
*!*  	  gfReplace('DCLC_STRT  With ldClcStDt,dclc_fnsh With ldFnshDt')
*!*  	ENDIF
*!*      ldFnshDt  = ldClcStDt - MIN(nest_dur,1)
*!*      lnPrvDur = nest_dur
*!*    ENDSCAN
*!*    lnPrvDur = 0
*!*    LOCATE
*!*    SCAN FOR !ISNULL(DONSCHDL) AND !EMPTY(DONSCHDL)
*!*      lnRecNumber = RECNO()
*!*      lcGRPID = COPRT_CTG
*!*      ldFnshDt = DONSCHDL
*!*      SCAN REST WHILE CPRJ_TYP+CPRJ_ID+CSTYLE+COPRT_CTG+COPRT_ID = SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjRl..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjRl..cPrj_ID)) +;
*!*                SUBSTR(lcStyle,1,LEN(&lcTmpPrjRl..cStyle)) FOR COPRT_CTG = lcGRPID
*!*        ldClcStDt ={}
*!*        IF lnPrvDur > 0 AND nest_dur = 0
*!*          ldFnshDt  = ldFnshDt +1
*!*        ENDIF
*!*
*!*        IF lnPrvDur = 0 AND nest_dur > 0
*!*          ldFnshDt  = ldFnshDt -1
*!*        ENDIF
*!*
*!*        llClcDates  = .T.
*!*        =lfAdjDate(ldFnshDt, @ldClcStDt,;
*!*                    MAX(0, IIF(nAct_dur > 0, nAct_dur, nest_dur) - 1),;
*!*                    cCal_ID, '-')
*!*
*!*        IF !llEditAdd
*!*  		oAriaEnvironment.remotetableaccess.ReplaceRecord('DCLC_STRT  With ldClcStDt,dclc_fnsh with ldFnshDt')
*!*        ELSE
*!*  	    gfReplace('DCLC_STRT  With ldClcStDt,dclc_fnsh With ldFnshDt')
*!*  	  ENDIF
*!*
*!*  	  ldFnshDt  = ldClcStDt - MIN(nest_dur,1)
*!*  	  lnPrvDur = nest_dur
*!*  	ENDSCAN
*!*      IF BETWEEN(lnRecNumber ,1,RECCOUNT())
*!*        GO RECORD lnRecNumber
*!*      ENDIF
*!*    ENDSCAN
*!*    SET ORDER TO PMPRJDT ASCENDING
*!*  ENDIF


*--Clear all calculated dates fields


*!*  SCAN FOR IIF(llClcDates,.T.,EMPTY(dclc_strt)) &&EMPTY(dclc_strt)
*!*    *-- Save the current operation into a variable.
*!*    lcCurOprt = cOprt_Ctg + cOprt_ID
*!*
*!*    *-- Update dates, and get the new finish date.
*!*    SELECT (lcTmpPrjDt)

*!*    *-- For the first operation, use the scheduling date as a
*!*    *-- candidate calculated start date
*!*
*!*  *!*	  IF &lcPrjHder..LLASTSTRT AND !ISNULL(DONSCHDL) AND !EMPTY(DONSCHDL)
*!*  *!*	    =lfUpdClcDt(DONSCHDL -IIF(nest_dur > 0,(nest_dur-1),nest_dur))
*!*  *!*	  ELSE
*!*  *!*	    *mmmmt
*!*  *!*	    =lfUpdClcDt(dSch_Date)
*!*  *!*	  *MT
*!*  *!*	  ENDIF
*!*  *!*	  *MT
*!*    IF &lcPrjHder..LLASTSTRT AND llonSchAdd AND llClcDates
*!*      IF !EMPTY(dclc_fnsh) AND !EMPTY(dclc_strt)
*!*        =lfUpdClcDt(dclc_strt)
*!*      ELSE
*!*        =lfUpdClcDt(dSch_Date)
*!*      ENDIF
*!*    ELSE
*!*      =lfUpdClcDt(dSch_Date)
*!*    ENDIF
*!*
*!*    lcThsOprt = cOprt_Ctg + cOprt_ID
*!*
*!*    lldclc_fsh = dclc_fnsh
*!*    lnDurIndic = MIN(nest_dur,1)
*!*
*!*
*!*    SELECT SYSCHDUL
*!*
*!*    IF !llEditAdd
*!*      =oAriaEnvironment.remotetableaccess.SetOrderTO('Coprusr')
*!*    ELSE
*!*      =gfSetOrder('Coprusr')
*!*    ENDIF
*!*
*!*    IF  (!llEditAdd AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcThsOprt)) OR ;
*!*  	  (llEditAdd AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcThsOprt))
*!*
*!*      IF SYSCHDUL.COPERSTAT = 'C'
*!*        lcStauts = SYSCHDUL.COPERSTAT

*!*        LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
*!*                    SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcThsOprt;
*!*                    FOR COPERSTAT <> lcStauts

*!*      ENDIF
*!*      IF !llEditAdd
*!*        =oAriaEnvironment.remotetableaccess.REPLACERECORD("dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),"+;
*!*                "dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh)")
*!*  	ELSE
*!*  	  REPLACE dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),;
*!*                dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh)
*!*  	ENDIF
*!*    ENDIF

*!*
*!*
*!*    *-- if the new operation is a predecessor to other operations,
*!*    *-- copy their IDs to the cursor.
*!*    SELECT (lcTmpPrjRl)
*!*
*!*    IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
*!*            SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
*!*      SCAN REST WHILE cPrj_Typ +  cPrj_ID + cStyle  +  cPrd_Ctg + cPrd_ID = ;
*!*                     SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
*!*                     SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt
*!*        lcNxtOprt = cOprt_Ctg + cOprt_ID
*!*        INSERT INTO (lc_Parser);
*!*           VALUES(&lcTmpPrjRl..cOprt_Ctg, &lcTmpPrjRl..cOprt_ID, lldclc_fsh, lnDurIndic)


*!*      ENDSCAN
*!*    ENDIF

*!*    SELECT (lc_Parser)
*!*    GO TOP
*!*    IF EOF()
*!*      SELECT (lcTmpPrjDt)
*!*
*!*      IF !llEditAdd
*!*        =oAriaEnvironment.remotetableaccess.SetOrderTO('PMPRJDT')
*!*      ELSE
*!*        =gfSetOrder('PMPRJDT')
*!*        SET ORDER TO PMPRJDT
*!*      ENDIF
*!*
*!*      =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
*!*          SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
*!*
*!*      IF !EMPTY(&lcTmpPrjDt..cOprt_res) OR !EMPTY(&lcTmpPrjDt..cGroup_Id)
*!*        SELECT SYSCHDUL
*!*        IF !llEditAdd
*!*          =oAriaEnvironment.remotetableaccess.SetOrderTO('Coprusr')
*!*        ELSE
*!*          SET ORDER TO 'Coprusr'
*!*        ENDIF
*!*        IF  (!llEditAdd  AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcCurOprt)) OR ;
*!*  	      (llEditAdd  AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcCurOprt))
*!*          IF SYSCHDUL.COPERSTAT = 'C'
*!*            lcStauts = SYSCHDUL.COPERSTAT
*!*            LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
*!*                        SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcCurOprt;
*!*                        FOR COPERSTAT <> lcStauts
*!*          ENDIF
*!*          IF !llEditAdd
*!*            =oAriaEnvironment.remotetableaccess.REPLACERECORD("dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),"+;
*!*                  "dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh),"+;
*!*                  "lpredcomp  WITH .T.")
*!*          ELSE
*!*            gfREPLACE("dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),"+;
*!*                    	" dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh),"+;
*!*                    	"lpredcomp  WITH .T.")
*!*          ENDIF
*!*        ENDIF
*!*      ENDIF
*!*    ENDIF
*!*    SELECT (lc_Parser)
*!*    DO WHILE !EOF()
*!*      *-- Update the estimated date of the current detail record
*!*      *-- Add category code field
*!*      lcNxtOprt  = cOprt_Ctg + cOprt_ID
*!*      lnRecNo    = RECNO()
*!*
*!*      SELECT (lcTmpPrjDt)
*!*
*!*      SELECT (lcTmpPrjDt)
*!*      IF &lcPrjHder..LLASTSTRT  AND llonSchAdd AND llClcDates
*!*         IF  !EMPTY(dclc_fnsh) AND !EMPTY(dclc_strt)
*!*           =lfUpdClcDt(dclc_strt)
*!*         ELSE
*!*           =lfUpdClcDt(IIF(nest_dur = 0, &lc_Parser..dStrtDate,;
*!*                    &lc_Parser..dStrtDate + &lc_Parser..nDurIndic))
*!*         ENDIF
*!*       ELSE
*!*         =lfUpdClcDt(IIF(nest_dur = 0, &lc_Parser..dStrtDate,;
*!*                    &lc_Parser..dStrtDate + &lc_Parser..nDurIndic))
*!*      ENDIF
*!*
*!*      lldclc_fsh = dclc_fnsh
*!*
*!*      lnDurIndic = MAX(lnDurIndic, MIN(nest_dur,1))
*!*      SELECT (lcTmpPrjRl)

*!*      IF SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
*!*              SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt)
*!*        SCAN REST WHILE cPrj_Typ + cPrj_ID + cStyle + cPrd_Ctg + cPrd_ID = ;
*!*                       SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
*!*                       SUBSTR(lcStyle,1,LEN(cStyle)) + lcNxtOprt
*!*          lcNxtOnPth = cOprt_Ctg + cOprt_ID

*!*
*!*           INSERT INTO (lc_Parser);
*!*             VALUES(&lcTmpPrjRl..cOprt_Ctg, &lcTmpPrjRl..cOprt_ID, lldclc_fsh, lnDurIndic)
*!*

*!*        ENDSCAN
*!*      ENDIF
*!*
*!*      IF !EMPTY(&lcTmpPrjDt..cOprt_res) OR !EMPTY(&lcTmpPrjDt..cGroup_Id)
*!*        SELECT SYSCHDUL
*!*        IF !llEditAdd
*!*          =oAriaEnvironment.remotetableaccess.SetOrderTO('Coprusr')
*!*        ELSE
*!*          =gfSetOrder('Coprusr')
*!*        ENDIF
*!*
*!*        IF  (!llEditAdd AND oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcNxtOprt)) OR ;
*!*  		  (llEditAdd AND gfSeek(SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcNxtOprt))
*!*
*!*          IF SYSCHDUL.COPERSTAT = 'C'
*!*            lcStauts = SYSCHDUL.COPERSTAT




*!*            LOCATE REST WHILE cconttype+cseqnumber+cStyle+ccont_id+coperstat+cuser_id =;
*!*                        SUBSTR(lcPrj_Typ,1,1) + SUBSTR(lcPrj_ID,1,6) + SUBSTR(lcStyle,1,12) + lcNxtOprt ;
*!*                        FOR COPERSTAT <> lcStauts

*!*          ENDIF
*!*          IF !llEditAdd
*!*            =oAriaEnvironment.remotetableaccess.REPLACERECORD("dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),"+;
*!*                    "dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh)")
*!*  		ELSE
*!*  		  REPLACE dtrandate  WITH IIF(EMPTY(&lcTmpPrjDt..dclc_strt),&lcTmpPrjDt..dEst_strt,&lcTmpPrjDt..dclc_strt),;
*!*                    dcmpltdate WITH IIF(EMPTY(&lcTmpPrjDt..dclc_Fnsh),&lcTmpPrjDt..dEst_fnsh,&lcTmpPrjDt..dclc_Fnsh)
*!*  		ENDIF
*!*        ENDIF
*!*      ENDIF
*!*
*!*
*!*      SELECT (lc_Parser)
*!*      IF BETWEEN(lnRecNo, 1, RECCOUNT())
*!*        GO lnRecNo
*!*      ENDIF

*!*      SKIP
*!*    ENDDO
*!*
*!*    SELECT (lc_Parser)
*!*    ZAP
*!*
*!*    SELECT (lcTmpPrjDt)
*!*    IF !llEditAdd
*!*      =oAriaEnvironment.remotetableaccess.SetOrderTO('PMPRJDT')
*!*    ELSE
*!*      =gfSetOrder('PMPRJDT')
*!*      SET ORDER TO 'PMPRJDT'
*!*    ENDIF
*!*    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
*!*         SUBSTR(lcStyle,1,LEN(cStyle)) + lcCurOprt)
*!*  ENDSCAN

SELECT (lc_Parser)
SET RELATION TO
SELECT (lcTmpPrjDt)



*B609028,1 MMT 10/07/2009 Fix bug of Wrong calc.date for Completed tasks[Start]
SELECT  'CmplTsks'
SCAN
  SELECT (lcTmpPrjDt)
  IF !llEditAdd
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*	     =Seek(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*	       SUBSTR(lcStyle,1,LEN(cStyle))+CmplTsks.Coprt_CTG+CmplTsks.COPRT_ID,lcTmpPrjDt,'PMPRJDT')
    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
      SUBSTR(lcStyle,1,LEN(CSTYLE))+STR(lnLineNo,6)+CmplTsks.cOprt_Ctg+CmplTsks.cOprt_ID,lcTmpPrjDt,'PMPRJDT')
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    IF !EMPTY(dAct_strt) AND !EMPTY(dact_fnsh)
      =oAriaEnvironment.remotetableaccess.REPLACERECORD("dCLC_Strt With CmplTsks.dCLC_Strt")
      =oAriaEnvironment.remotetableaccess.REPLACERECORD("dCLC_FNSH With  CmplTsks.dCLC_FNSH")
    ENDIF
  ELSE
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
    *!*	     =Seek(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
    *!*	       SUBSTR(lcStyle,1,LEN(cStyle))+CmplTsks.Coprt_CTG+CmplTsks.COPRT_ID,lcTmpPrjDt,'PMPRJDT')
    =SEEK(SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
      SUBSTR(lcStyle,1,LEN(CSTYLE))+STR(lnLineNo,6)+CmplTsks.cOprt_Ctg+CmplTsks.cOprt_ID,lcTmpPrjDt,'PMPRJDT')
    *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
    IF !EMPTY(dAct_strt) AND !EMPTY(dact_fnsh)
      =gfReplace("dCLC_Strt With CmplTsks.dCLC_Strt")
      =gfReplace("dCLC_FNSH With  CmplTsks.dCLC_FNSH")
    ENDIF
  ENDIF
ENDSCAN
SELECT (lcTmpPrjDt)
*B609028,1 MMT 10/07/2009 Fix bug of Wrong calc.date for Completed tasks[End]

IF !llEditAdd
  oAriaEnvironment.remotetableaccess.TABLEUPDATE()
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
  *!*	  =oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,LEN(cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(cPrj_ID)) +;
  *!*	       SUBSTR(lcStyle,1,LEN(cStyle)))
  =oAriaEnvironment.remotetableaccess.SeekRecord(SUBSTR(lcPrj_Typ,1,LEN(CPRJ_TYP)) +;
    SUBSTR(lcPrj_ID,1,LEN(CPRJ_ID)) +;
    SUBSTR(lcStyle,1,LEN(CSTYLE))++STR(lnLineNo,6))
  *E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
ENDIF
*-- After all operations are scanned, get the project estimated finish date.
*-- The project estimated finish date is the greatest estimated finish
*-- date of all operations/

*B609028,1 MMT 10/07/2009 Fix bug of Wrong calc.date for Completed tasks[Start]
IF !llEditAdd
  = TABLEUPDATE(.T.)
ENDIF
*B609028,1 MMT 10/07/2009 Fix bug of Wrong calc.date for Completed tasks[End]

DIMENSION  laTmpVal[1]
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*	SELECT MIN(dclc_strt), MAX(dclc_fnsh) ;
*!*	  FROM (lcTmpPrjDt)   ;
*!*	  WHERE cPrj_Typ +  cPrj_ID +  cStyle + cOprt_Ctg + cOprt_ID = ;
*!*	              SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjDt..cPrj_Typ)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjDt..cPrj_ID)) +;
*!*	              SUBSTR(lcStyle,1,LEN(&lcTmpPrjDt..cStyle)) ;
*!*	       .AND. !lVoid;
*!*	  INTO ARRAY laTmpVal
SELECT MIN(dclc_strt), MAX(dclc_fnsh) ;
  FROM (lcTmpPrjDt)   ;
  WHERE CPRJ_TYP +  CPRJ_ID +  CSTYLE +STR(LINENO,6)+ cOprt_Ctg + cOprt_ID = ;
  SUBSTR(lcPrj_Typ,1,LEN(&lcTmpPrjDt..CPRJ_TYP)) + SUBSTR(lcPrj_ID,1,LEN(&lcTmpPrjDt..CPRJ_ID)) +;
  SUBSTR(lcStyle,1,LEN(&lcTmpPrjDt..CSTYLE)) +STR(lnLineNo,6);
  .AND. !lVoid;
  INTO ARRAY laTmpVal
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]

IF !EMPTY(laTmpVal[1])
  m.dclc_strt = laTmpVal[1]
  m.dclc_fnsh = laTmpVal[2]
  IF !llEditAdd
    SELECT PMPRJHD
    =oAriaEnvironment.remotetableaccess.REPLACERECORD(" dclc_strt WITH m.dclc_strt,"+;
      "dclc_fnsh WITH m.dclc_fnsh")
  ELSE
    SELECT (lcPrjHder)
    *B609028,1 MMT 10/07/2009 Fix bug of Wrong calc.date for Completed tasks[Start]
    *!*	    REPLACE dclc_strt WITH m.dclc_strt,;
    *!*	           dclc_fnsh WITH m.dclc_fnsh
    gfReplace("dclc_strt WITH m.dclc_strt,"+;
      "dclc_fnsh WITH m.dclc_fnsh")
    *B609028,1 MMT 10/07/2009 Fix bug of Wrong calc.date for Completed tasks[End]
  ENDIF
  SELECT (lcTmpPrjDt)
ENDIF

*--  Restore relations and tags
IF !llEditAdd
  SELECT (lcTmpPrjRl)
  =oAriaEnvironment.remotetableaccess.SetOrderTO("PMPRJRL")
  SELECT (lcTmpPrjDt)
  =oAriaEnvironment.remotetableaccess.SetOrderTO(lnCurTag)
ELSE
  SELECT (lcTmpPrjRl)
  =gfSetOrder("PMPRJRL")
  SET ORDER TO "PMPRJRL"
  SELECT (lcTmpPrjDt)
  =gfSetOrder(lnCurTag)
  SET ORDER TO (lnCurTag)
ENDIF

SET FILTER TO
SET RELATION TO
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[Start]
*!*	SET RELATION TO cPrj_Typ + cPrj_ID + cStyle + cOprt_Ctg + cOprt_ID ;
*!*	           INTO (lcTmpPrjRl)
SET RELATION TO CPRJ_TYP + CPRJ_ID + CSTYLE +STR(LINENO,6)+ cOprt_Ctg + cOprt_ID ;
  INTO (lcTmpPrjRl)
*E302650,1 MMT 12/02/2009 Modify Project Screen to enable user to create project per line[End]
SELECT (lnCurAlias)
*!*************************************************************
*! Name      : lfUpdClcDt
*! Developer : Mariam MAzhar
*! Date      : 05/10/2009
*! Purpose   : Updates the current operation's calculated start
*!             and finish date
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters : ldNxtStDte : next start date.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUpdClcDt({01/01/96})
*!*************************************************************
FUNCTION lfUpdClcDt
PARAMETERS ldNxtStDte

*--  Get the higher date value as a start date
ldNxtStDte = MAX(ldNxtStDte,dclc_strt)


DO CASE
  *--  Case Actual values are entered
CASE !EMPTY(dAct_strt) .OR. !EMPTY(dact_fnsh) .OR. nAct_dur > 0
  *--  If an actual start date exists, default the calculated start
  *--  date with its value,
  *--  If an actual finish date exists, default the calculated finish
  *--  date with its value,
  *--  else,
  *--  If an actual duration exists, add it to the calculated start
  *--  date, else add the estimated duration to the calculated start
  *--  date.
  IF !EMPTY(dAct_strt)
    ldClcFshDt = {}
    ldCalDateDt = lfAdjDate(dAct_strt, @ldClcFshDt,;
      MAX(0, IIF(nAct_dur > 0, nAct_dur, nest_dur) - 1),;
      cCal_ID, '+')

    IF !llEditAdd
      =oAriaEnvironment.remotetableaccess.REPLACERECORD("dclc_strt WITH ldCalDateDt ,"+;
        " dclc_fnsh WITH IIF(!EMPTY(dact_fnsh), dact_fnsh, ldClcFshDt)")



      IF EMPTY(DORGCLCDT) OR ISNULL(DORGCLCDT)
        =oAriaEnvironment.remotetableaccess.REPLACERECORD("DORGCLCDT With IIF(!EMPTY(dact_fnsh), dact_fnsh, ldClcFshDt)")
      ENDIF

    ELSE
      REPLACE dclc_strt WITH ldCalDateDt ,;
        dclc_fnsh WITH IIF(!EMPTY(dact_fnsh), dact_fnsh, ldClcFshDt)

      IF TYPE("cStatus") <> 'U'
        REPLACE  cStatus WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
      ENDIF


      IF EMPTY(DORGCLCDT )  OR ISNULL(DORGCLCDT)
        REPLACE DORGCLCDT WITH IIF(!EMPTY(dact_fnsh), dact_fnsh, ldClcFshDt)
      ENDIF




    ENDIF

    *--  Else,
    *--  If an actual start date is not found,
  ELSE  && ELSEIF !EMPTY(dact_strt)
    *--  If an actual finish date exists, default the calculated finish
    *--  date with its value,
    *--  If an actual finish date exists, default the calculated finish
    *--  date with its value,
    *--  Calculate the calculated start date as follows :
    *--  If an actual duration exists, subtract it from the calculated
    *--  finish date, else subtract the estimated duration from the
    *--  calculated finish date.
    IF !EMPTY(dact_fnsh)
      ldClcStDt = {}
      =lfAdjDate(dact_fnsh, @ldClcStDt,;
        MAX(0, IIF(nAct_dur > 0, nAct_dur, nest_dur) - 1),;
        cCal_ID, '-')

      IF !llEditAdd
        =oAriaEnvironment.remotetableaccess.REPLACERECORD("dclc_strt WITH ldClcStDt,"+;
          "dclc_fnsh WITH dact_fnsh")


        IF EMPTY(DORGCLCDT)  OR ISNULL(DORGCLCDT)
          =oAriaEnvironment.remotetableaccess.REPLACERECORD("DORGCLCDT With dact_fnsh")
        ENDIF


      ELSE
        REPLACE dclc_strt WITH ldClcStDt,;
          dclc_fnsh WITH dact_fnsh

        IF TYPE("cStatus") <> 'U'
          REPLACE cStatus WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
        ENDIF


        IF EMPTY(DORGCLCDT)  OR ISNULL(DORGCLCDT)
          REPLACE DORGCLCDT WITH dact_fnsh
        ENDIF


      ENDIF

      *--  Else, both actual start and finish dates are emoty,
      *--  i.e. the actual duration is not empty
      *--  Calculate the start and finish calculated dates
      *--  based on the finish date of the previous operation,
      *--  using actual duration
    ELSE  && ELSEIF !EMPTY(dact_fnsh)
      ldClcFshDt = {}

      ldCalDateDt =lfAdjDate(ldNxtStDte, @ldClcFshDt          ,;
        MAX(0, nAct_dur - 1), cCal_ID, '+')

      IF !llEditAdd
        =oAriaEnvironment.remotetableaccess.REPLACERECORD("dclc_strt WITH ldCalDateDt ,"+;
          "dclc_fnsh WITH ldClcFshDt")


        IF EMPTY(DORGCLCDT)  OR ISNULL(DORGCLCDT)
          =oAriaEnvironment.remotetableaccess.REPLACERECORD("DORGCLCDT With ldClcFshDt")
        ENDIF



      ELSE
        REPLACE dclc_strt WITH ldCalDateDt ,;
          dclc_fnsh WITH ldClcFshDt

        IF TYPE("cStatus") <> 'U'
          REPLACE cStatus WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
        ENDIF


        IF EMPTY(DORGCLCDT)  OR ISNULL(DORGCLCDT)
          REPLACE DORGCLCDT WITH ldClcFshDt
        ENDIF



      ENDIF
    ENDIF && ENDIF !EMPTY(dact_fnsh)
  ENDIF && ENDIF !EMPTY(dact_strt)
  *--  Otherwise, No Actual values are entered
OTHERWISE

  *--  If the remaining duration is different from the estimated
  *--  duration, this indicates that the current operation is in work.
  IF nrem_dur <> nest_dur
    *--  Get the calculated start date referenced to the
    *--  scheduling date (dSch_Date)
    ldClcStDt = {}
    IF nest_dur = 0
      STORE ldNxtStDte + 1 TO ldClcStDt, ldClcFshDt
    ELSE
      STORE ldNxtStDte TO ldClcStDt, ldClcFshDt
    ENDIF

    =lfAdjDate(@ldClcStDt,@ldClcFshDt,nrem_dur, cCal_ID, '+')

    IF !llEditAdd
      =oAriaEnvironment.remotetableaccess.REPLACERECORD("dclc_strt WITH ldClcStDt,"+;
        "dclc_fnsh WITH ldClcFshDt")



      IF EMPTY(DORGCLCDT)  OR ISNULL(DORGCLCDT)
        =oAriaEnvironment.remotetableaccess.REPLACERECORD("DORGCLCDT With ldClcFshDt")
      ENDIF


    ELSE
      REPLACE dclc_strt WITH ldClcStDt,;
        dclc_fnsh WITH ldClcFshDt

      IF TYPE("cStatus") <> 'U'
        REPLACE cStatus WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
      ENDIF


      IF EMPTY(DORGCLCDT)  OR ISNULL(DORGCLCDT)
        REPLACE DORGCLCDT WITH ldClcFshDt
      ENDIF


    ENDIF

    *--  Otherwise, calculate dates based on the finish date of
    *--  the previous operation and the remaining duration
  ELSE
    ldClcFshDt = {}






    ldCalDateDt =lfAdjDate(ldNxtStDte, @ldClcFshDt,;
      MAX(0, nrem_dur - 1), cCal_ID, '+')

    IF !llEditAdd
      =oAriaEnvironment.remotetableaccess.REPLACERECORD("dclc_strt WITH ldCalDateDt ,"+;
        "dclc_fnsh WITH ldClcFshDt")


      IF EMPTY(DORGCLCDT)  OR ISNULL(DORGCLCDT)
        =oAriaEnvironment.remotetableaccess.REPLACERECORD("DORGCLCDT With ldClcFshDt")
      ENDIF



    ELSE
      REPLACE  dclc_strt WITH ldCalDateDt ,;
        dclc_fnsh WITH ldClcFshDt

      IF TYPE("cStatus") <> 'U'
        REPLACE  cStatus WITH SUBSTR('MMA', AT(cStatus, 'SMA'), 1)
      ENDIF


      IF EMPTY(DORGCLCDT) OR ISNULL(DORGCLCDT)
        REPLACE DORGCLCDT WITH ldClcFshDt
      ENDIF


    ENDIF
  ENDIF
ENDCASE




*!*************************************************************
*! Name      : lfAdjDate
*! Developer : Mariam MAzhar
*! Date      : 05/10/2009
*! Purpose   : Adjusts date addition according to calendar
*!             holidays and weekends
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : ldOprt_Str : Start date
*!                      ldOprt_Fsh : pointer to finish date
*!                      lnOprt_Dur  : duration to add
*!                      lcCal_ID    : calendar ID of the operation
*!                      lcOperator  : '+', '-'
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  ldStrtDate = lfAdjDate(m.dest_strt,;
*!                                              @ldAdjEstFn,;
*!                                              MAX(0, nest_dur - 1),;
*!                                              cCal_ID, '+'),;
*!*************************************************************
FUNCTION lfAdjDate
PARAMETERS ldOprt_Str, ldOprt_Fsh, lnOprt_Dur, lcCal_ID, lcOperator
PRIVATE lnCount, ldOprt_Fsh

IF (!llEditAdd  AND oAriaEnvironment.remotetableaccess.SeekRecord(lcCal_ID, 'PMCALHD')) OR ;
    (llEditAdd  AND gfSeek(lcCal_ID, 'PMCALHD'))

  DO WHILE STR(DOW(ldOprt_Str),1) $ PMCALHD.cCal_WEnd .OR. ;
      ASCAN(laHolidays,lcCal_ID + DTOC(ldOprt_Str)) > 0
    ldOprt_Str = ldOprt_Str &lcOperator 1
  ENDDO

  lnCount = 1
  ldOprt_Fsh = ldOprt_Str
  DO WHILE lnCount <= lnOprt_Dur
    ldOprt_Fsh = ldOprt_Fsh &lcOperator 1
    lnCount    = lnCount ;
      + IIF(STR(DOW(ldOprt_Fsh),1) $ PMCALHD.cCal_WEnd .OR. ;
      ASCAN(laHolidays,lcCal_ID + DTOC(ldOprt_Fsh)) > 0 , 0 , 1)
  ENDDO
ELSE
  ldOprt_Fsh = ldOprt_Str &lcOperator lnOprt_Dur
ENDIF
RETURN ldOprt_Str


