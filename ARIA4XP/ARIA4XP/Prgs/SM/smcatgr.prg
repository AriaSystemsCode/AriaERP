*!*****************************************************************************************
*! Name      : smcatgr.PRG
*! Developer : Ahmed Maher Keshk (AMK)
*! Date      : 05/07/2009 
*! Purpose   : Category Screen
*! Entry no. : N037574 
*!*****************************************************************************************
#INCLUDE R:\Aria4XP\prgS\SM\smcatgr.h
PARAMETERS lcScrMode, lcOprt_Ctg, lcNewOprt
DO FORM (oAriaApplication.ScreenHome+ 'SM\smcatgr.SCX') WITH lcScrMode, lcOprt_Ctg, lcNewOprt
RETURN .T.
*!*************************************************************
*! Name      : lfInitCat
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : Init of Category Screen
*!*************************************************************
FUNCTION lfInitCat
PARAMETERS loCatFormSet,lcScrMode, lcOprt_Ctg, lcNewOprt
WITH loCatFormSet
  .nWorkArea                            = 'PMCTGHD'
  .DataEnvironment.InitialSelectedAlias = 'PMCTGHD'
  .cBrowseFileName                      = "PMCTGHD"
  .cBrowseIndexExpression               = "coprt_ctg"
  .cBrowseIndexFields                   = "coprt_ctg"
  .cBrowseIndexName                     = "PMCTGHD"
  .cBrowseAliasName                     = "PMCTGHD"
  .cBrowseTableName                     = "PMCTGHD"
  .BrowseTitle                          = LANG_SMCTGR_CATGTASK
  .cBrowseTableDBEngine                 = 'SQL'
  .ariaBrFields.edtBrowseFields.Value   = "coprt_ctg    :H='"+LANG_SMCTGR_CATGORY+"'  ," +;
                                          "Cctg_dsc   :H='"+LANG_SMCTGR_DESC+"' "
ENDWITH

loCatFormSet.ARIAFORM1.combRespons.Value = 'User'
loCatFormSet.ARIAFORM1.chkShowCus.Value = .T.
loCatFormSet.ARIAFORM1.txtNotify.Value=PMCTGDT.Mnotify

SET MULTILOCKS ON
=gfOpentable(oAriaApplication.DataDir + 'PMCTGHD', 'PMCTGHD','SH')
=gfOpentable(oAriaApplication.DataDir + 'PMCTGDT', 'PMCTGDT','SH')
=gfOpentable(oAriaApplication.DataDir + 'PMCTGRL', 'PMCTGRL','SH')
=gfOpentable(oAriaApplication.DataDir + 'PMCALHD', 'PMCALHD','SH')
=gfOpentable(oAriaApplication.DataDir + 'PMPCTGNT', 'PMPCTGNT','SH')
=gfOpentable(oAriaApplication.DataDir + 'SYUUSER', 'CUSER_ID','SH')
=gfOpentable(oAriaApplication.DataDir + 'SYUGROUP', 'CGROUP_ID','SH')

IF TYPE('lcScrMode') = 'C' AND TYPE('lcOprt_Ctg') = 'C'
  IF TYPE('lcNewOprt') <> 'C'
    loCatFormSet.ADDNEW()
    SELECT PMCTGHD 
    REPLACE coprt_ctg WITH lcOprt_Ctg
    loCatFormSet.ChangeMode(lcScrMode)
  ELSE
    =gfSeek(lcOprt_Ctg,'PMCTGHD')
    loCatFormSet.ChangeMode("V")
    loCatFormSet.ChangeMode(lcScrMode)
    loCatFormSet.Ariaform1.butNew.Click
    loCatFormSet.Ariaform1.txtTaskId.Value = lcNewOprt
  ENDIF   
  
ENDIF 

*!*************************************************************
*! Name      : lfNtfInit
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : init of Notify Screen
*!*************************************************************
FUNCTION lfNtfInit
PARAMETERS loNtFrmSet,lcPathID, lcCatgID, lcTaskID, lcMode

WITH loNtFrmSet
  .lcmode   = lcMode
  .lctaskid = lcTaskID
  .lccatg   = lcCatgID
  .lcpathid = lcPathID
ENDWITH

WITH loNtFrmSet.AriaForm1.Ariacontainer2
  .txtEmail.InputMask = REPLICATE('X',60)
  .UserKey.Keytextbox.InputMask = REPLICATE('!',10)
  .txtNBC.Value = 0
  .txtNOSD.Value = 0
  .txtNOCD.Value = 0
  .txtNBS.Value = 0
ENDWITH  

loNtFrmSet.lcScreentable = 'PMPCTGNT'
IF !loNtFrmSet.llifok
  WITH loNtFrmSet.Ariaform1.Ariacontainer1.grdUsers
    .RecordSource ='PMPCTGNT'
    .Column1.ControlSource = 'cuser_id'
    .Column2.ControlSource = 'nbfrstrtdy'
    .Column3.ControlSource = 'lonstrt'
    .Column4.ControlSource = 'nstrtdelay'
    .Column5.ControlSource = 'lonredrct'
    .Column6.ControlSource = 'nbfrcmpldy'
    .Column7.ControlSource = 'loncmplt'
    .Column8.ControlSource = 'ncmpldelay'
    loNtFrmSet.llifok = .T.
  ENDWITH
ENDIF 

IF !EOF('PMPCTGNT')
  loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh()
  loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AfterRowColChange
ELSE
  WITH loNtFrmSet.AriaForm1.Ariacontainer2
    .UserKey.Enabled = .F.
    .txtEmail.Enabled = .F.
    .cbnotbefst.Enabled = .F.
    .cbnotbefcom.Enabled = .F.
    .cbnor.Enabled = .F.
    .cbnoc.Enabled = .F.
    .cbnos.Enabled = .F.
    .txtNBC.Enabled = .F.
    .txtNBS.Enabled = .F.
    .txtNOSD.Enabled =.F.
    .txtNOCD.Enabled = .F.
    .chkCmpDely.Enabled = .F.
    .chkStrtDely.Enabled = .F.
  ENDWITH    
ENDIF 

loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh()
loNtFrmSet.changemode(Lcmode)

IF EOF('PMPCTGNT')
 WITH loNtFrmSet.AriaForm1.Ariacontainer2
    .UserKey.Enabled = .F.
    .txtEmail.Enabled = .F.
    .cbnotbefst.Enabled = .F.
    .cbnotbefcom.Enabled = .F.
    .cbnor.Enabled = .F.
    .cbnoc.Enabled = .F.
    .cbnos.Enabled = .F.
    .txtNBC.Enabled = .F.
    .txtNBS.Enabled = .F.
    .txtNOSD.Enabled =.F.
    .txtNOCD.Enabled = .F.
    .chkCmpDely.Enabled = .F.
    .chkStrtDely.Enabled = .F.

  ENDWITH    
ENDIF 

*!*************************************************************
*! Name      : lfChngMode
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : Change mode Notify Screen
*!*************************************************************
FUNCTION lfChngMode
PARAMETERS loNtFrmSet

*!*************************************************************
*! Name      : lfAftrRowCol
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : After row Col. Chnage of Notify Screen
*!*************************************************************
FUNCTION lfAftrRowCol
PARAMETERS loNtFrmSet
lcScreentable = loNtFrmSet.lcScreentable
IF !EOF()
  WITH loNtFrmSet.AriaForm1.Ariacontainer2
    .UserKey.Keytextbox.Value = &lcScreentable..cuser_id
    .txtEmail.Value = &lcScreentable..cemail_add
    .cbnotbefst.Value = &lcScreentable..lbfrstrt
    .cbnotbefcom.Value = &lcScreentable..lbfrcmplt
    .cbnor.Value = &lcScreentable..lonredrct
    .cbnoc.Value = &lcScreentable..loncmplt  
    .cbnos.Value = &lcScreentable..lonstrt
    .txtNBC.Value = &lcScreentable..nbfrcmpldy
    .txtNBS.Value = &lcScreentable..nbfrstrtdy
    .txtNOSD.Value = &lcScreentable..nstrtdelay
    .txtNOCD.Value = &lcScreentable..ncmpldelay
    .chkCmpDely.Value = &lcScreentable..LCMPLDELAY
    .chkStrtDely.Value = &lcScreentable..LSTRTDELAY

  ENDWITH 
  
  IF &lcScreentable..lbfrstrt
    loNtFrmSet.AriaForm1.Ariacontainer2.txtNBS.Enabled = !(loNtFrmSet.ActiveMode = 'V')
  ELSE 
    loNtFrmSet.AriaForm1.Ariacontainer2.txtNBS.Enabled = .F.
  ENDIF 
  
  IF &lcScreentable..lbfrcmplt
    loNtFrmSet.AriaForm1.Ariacontainer2.txtNBC.Enabled = !(loNtFrmSet.ActiveMode = 'V')
  ELSE 
    loNtFrmSet.AriaForm1.Ariacontainer2.txtNBC.Enabled = .F.
  ENDIF 
  loNtFrmSet.AriaForm1.Ariacontainer3.btnRemove.Enabled = !(loNtFrmSet.ActiveMode = 'V')
  
  WITH loNtFrmSet.AriaForm1.Ariacontainer2
    .UserKey.Enabled = !(loNtFrmSet.ActiveMode = 'V')
    .txtEmail.Enabled = !(loNtFrmSet.ActiveMode = 'V')
    .cbnotbefst.Enabled = !(loNtFrmSet.ActiveMode = 'V')
    .cbnotbefcom.Enabled = !(loNtFrmSet.ActiveMode = 'V')
    .cbnor.Enabled = !(loNtFrmSet.ActiveMode = 'V')
    .cbnoc.Enabled = !(loNtFrmSet.ActiveMode = 'V')
    .cbnos.Enabled = !(loNtFrmSet.ActiveMode = 'V')
    .txtNBC.Enabled = !(loNtFrmSet.ActiveMode = 'V')
    .txtNBS.Enabled = !(loNtFrmSet.ActiveMode = 'V')
    .txtNOSD.Enabled =!(loNtFrmSet.ActiveMode = 'V')
    .txtNOCD.Enabled = !(loNtFrmSet.ActiveMode = 'V')
    .chkCmpDely.Enabled = !(loNtFrmSet.ActiveMode = 'V')
    .chkStrtDely.Enabled = !(loNtFrmSet.ActiveMode = 'V')
  ENDWITH  
ENDIF

*!*************************************************************
*! Name      : lfvUsrId
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : Valid User of Notify Screen
*!*************************************************************
FUNCTION lfvUsrId
PARAMETERS loNtFrmSet

IF loNtFrmSet.ariaForm1.ariacontainer2.UserKey.Enabled = .F. 
  RETURN .T.
ENDIF 


lcOldVal = loNtFrmSet.AriaForm1.Ariacontainer2.UserKey.Keytextbox.OldValue
m.cUser_ID = loNtFrmSet.AriaForm1.Ariacontainer2.UserKey.Keytextbox.Value
llBrowse = loNtFrmSet.AriaForm1.Ariacontainer2.UserKey.selectedfrombrowse 
IF (PADR(m.cUser_ID,10) = PADR(lcOldVal,10)) .AND. !llBrowse
  RETURN .T.
ENDIF
 
lcOprt_Ctg =loNtFrmSet.lccatg 
lcoprt_id = loNtFrmSet.lctaskid 


PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
SELECT SYUUSER
lcBrFields = [cUser_Id : H = ']+LANG_SMCTGR_USERID+[', cUsr_Name : H = ']+LANG_SMCTGR_USERNAME+[']  
IF (!EMPTY(m.cUser_ID) AND !gfSEEK(PADR(m.cUser_ID,10),'SYUUSER')) .OR. llBrowse
  llBrowse = .F.
  DIMENSION laTemp[2]
  IF ARIABROW('',LANG_SMCTGR_USERS,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,'cUser_Id,cEmail_Add','laTemp')
    m.cUser_ID   = laTemp[1]
    m.cEmail_Add = laTemp[2]
  ELSE
    m.cUser_ID   = lcOldVal
  ENDIF
  IF EMPTY(m.cUser_ID)
    RETURN .F.
  ENDIF 
  loNtFrmSet.AriaForm1.Ariacontainer2.UserKey.Keytextbox.Value = m.cUser_ID
  lcOldVal = loNtFrmSet.AriaForm1.Ariacontainer2.UserKey.Keytextbox.OldValue
  IF !EMPTY(m.cUser_ID) && AND m.cUser_ID <> lcOldVal
    lnTmpRecNo = RECNO('PMPCTGNT')
    IF !SEEK(lcOprt_Ctg+lcoprt_id+m.cUser_ID,'PMPCTGNT')
      m.cEmail_Add = SYUUSER.CEMAIL_ADD
    ELSE
      WAIT WINDOW "User Exist"
      m.cUser_ID = lcOldVal
      loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AfterRowColChange
      RETURN 
    ENDIF
    IF BETWEEN(lnTmpRecNo,1,RECCOUNT('PMPCTGNT'))
      GO lnTmpRecNo IN 'PMPCTGNT'
    ENDIF   
  ENDIF
ELSE
  IF !EMPTY(m.cUser_ID)
    lnTmpRecNo = RECNO('PMPCTGNT')
    IF !SEEK(lcOprt_Ctg+lcoprt_id+m.cUser_ID,'PMPCTGNT')
      m.cEmail_Add = SYUUSER.CEMAIL_ADD
    ELSE
      WAIT WINDOW "User Exist"
      m.cUser_ID = lcOldVal
      loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AfterRowColChange
      RETURN 
    ENDIF
    IF BETWEEN(lnTmpRecNo,1,RECCOUNT('PMPCTGNT'))
      GO lnTmpRecNo IN 'PMPCTGNT'
    ENDIF   
  ENDIF
ENDIF  
SELECT 'PMPCTGNT'

gfREPLACE("cUser_ID   WITH m.cUser_ID,"+;
          "cEmail_Add WITH m.cEmail_Add")


SELECT (lnCurAlias)
loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AfterRowColChange

*!*************************************************************
*! Name      : lfvEmail
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : Valid Mail of Notify Screen
*!*************************************************************
FUNCTION lfvEmail
PARAMETERS loNtFrmSet
lcValue = loNtFrmSet.AriaForm1.Ariacontainer2.txtEmail.Value
SELECT 'PMPCTGNT'
gfREPLACE("cemail_add WITH lcValue")
loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh 

*!*************************************************************
*! Name      : lfvNtb4Strt
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : Before Start Check box validation of Notify Screen
*!*************************************************************
FUNCTION lfvNtb4Strt
PARAMETERS loNtFrmSet
llVallBfrStrt = loNtFrmSet.ariaForm1.ariacontainer2.cbnotbefst.Value
SELECT 'PMPCTGNT'
gfREPLACE("lBfrStrt WITH llVallBfrStrt") 
IF loNtFrmSet.ariaForm1.ariacontainer2.cbnotbefst.Value  = .F.
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Enabled = .F.
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Value = 0
ELSE 
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Enabled = .T.  
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Value = 1
ENDIF 
lnValueUpd = loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Value 
gfREPLACE("nBfrStrtDy  WITH lnValueUpd") 
loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh 

*!*************************************************************
*! Name      : lfvNtfb4StrtDays
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : Before Start Days validation of Notify Screen
*!*************************************************************
FUNCTION lfvNtfb4StrtDays
PARAMETERS loNtFrmSet
llnBfrStrtDy = loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Value
SELECT 'PMPCTGNT'
gfREPLACE('nBfrStrtDy  WITH llnBfrStrtDy') 
loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh 

*!*************************************************************
*! Name      : lfvNtfb4Comp
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : Before Complete Check box validation of Notify Screen
*!*************************************************************
FUNCTION lfvNtfb4Comp
PARAMETERS loNtFrmSet
SELECT 'PMPCTGNT'
lllBfrCmplt =loNtFrmSet.ariaForm1.ariacontainer2.cbnotbefcom.Value
gfREPLACE("lBfrCmplt WITH lllBfrCmplt")


IF loNtFrmSet.ariaForm1.ariacontainer2.cbnotbefcom.Value  = .F.
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Enabled = .F.
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Value = 0
ELSE 
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Enabled = .T.  
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Value = 1
ENDIF 
lnValueUpd = loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Value 
gfREPLACE("nBfrCmplDy WITH lnValueUpd") 
loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh 

*!*************************************************************
*! Name      : lfVNtfB4Cmpl
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : Before Complete Days validation of Notify Screen
*!*************************************************************
FUNCTION lfVNtfB4Cmpl
PARAMETERS loNtFrmSet
SELECT 'PMPCTGNT'
lnValuUpd = loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Value 
gfREPLACE("nBfrCmplDy WITH  lnValuUpd")
loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh 

*!*************************************************************
*! Name      : lfvNtfStrtDl
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : Start Delay Check box validation of Notify Screen
*!*************************************************************
FUNCTION lfvNtfStrtDl
PARAMETERS loNtFrmSet
SELECT 'PMPCTGNT'
lnNStr = loNtFrmSet.ariaForm1.ariacontainer2.txtNOSD.Value
gfREPLACE("nStrtDelay WITH lnNStr")  
loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh 

*!*************************************************************
*! Name      : lfvNtfCD
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : Complete Delay of Notify Screen
*!*************************************************************
FUNCTION lfvNtfCD
PARAMETERS loNtFrmSet
SELECT 'PMPCTGNT'
lnCmplSt = loNtFrmSet.ariaForm1.ariacontainer2.txtNOCD.Value 
gfREPLACE("nCmplDelay WITH lnCmplSt") 
loNtFrmSet.ariaForm1.Ariacontainer1.grdUsers.Refresh 

*!*************************************************************
*! Name      : lfvNtfOnStrt
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : on Start Validation of Notify Screen
*!*************************************************************
FUNCTION lfvNtfOnStrt
PARAMETERS loNtFrmSet
SELECT 'PMPCTGNT'
llOnStrt = loNtFrmSet.ariaForm1.ariacontainer2.cbnos.Value 
gfREPLACE("lOnStrt WITH llOnStrt")
loNtFrmSet.ariaForm1.Ariacontainer1.grdUsers.Refresh 

*!*************************************************************
*! Name      : lfvNtfOnComp
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : on Complete Validation of Notify Screen
*!*************************************************************
FUNCTION lfvNtfOnComp
PARAMETERS loNtFrmSet
SELECT 'PMPCTGNT'
llonCmp =  loNtFrmSet.ariaForm1.ariacontainer2.cbnoc.Value 
gfREPLACE("lOnCmplt WITH llonCmp")
loNtFrmSet.ariaForm1.Ariacontainer1.grdUsers.Refresh 

*!*************************************************************
*! Name      : lfVNtfonRedir
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : on Redirect Validation of Notify Screen
*!*************************************************************
FUNCTION lfVNtfonRedir
PARAMETERS loNtFrmSet
SELECT 'PMPCTGNT'
llOnDir = loNtFrmSet.ariaForm1.ariacontainer2.cbnor.Value
gfREPLACE("lOnRedrct WITH llOnDir")
loNtFrmSet.ariaForm1.Ariacontainer1.grdUsers.Refresh 

*!*************************************************************
*! Name      : lfvClosNtf
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : Close button of Notify Screen
*!*************************************************************
FUNCTION lfvClosNtf
PARAMETERS loNtFrmSet
SELECT 'PMPCTGNT'
SCAN FOR EMPTY(cUser_ID)
  =gfDelete()
ENDSCAN 

*!*************************************************************
*! Name      : lfvRmvUser
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : Remove button of Notify Screen
*!*************************************************************
FUNCTION lfvRmvUser
PARAMETERS loNtFrmSet
lnCurAlias = SELECT(0)
IF gfModalgen("TRM38208B38006","DIALOG") = 1
  SELECT 'PMPCTGNT'
  gfDELETE()
  loNtFrmSet.lfclear()
  SELECT 'PMPCTGNT'
  LOCATE
  IF EOF()
    loNtFrmSet.ariaForm1.ariacontainer3.btnRemove.Enabled = .F.
    WITH loNtFrmSet.ariaForm1.ariacontainer2
      .UserKey.Enabled = .F.  
      .txtEmail.Enabled = .F.
      .cbnotbefst.Enabled = .F.
      .cbnotbefcom.Enabled = .F.
      .cbnor.Enabled = .F.
      .cbnoc.Enabled = .F.
      .cbnos.Enabled = .F.
      .txtNBC.Enabled = .F.
      .txtNBS.Enabled = .F.
      .txtNOSD.Enabled = .F.
      .txtNOCD.Enabled = .F.
      .chkCmpDely.Enabled = .F.
      .chkStrtDely.Enabled = .F.
    ENDWITH   
  ELSE
	loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.refresH
    loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AfterRowColChange
    loNtFrmSet.ariaForm1.ariacontainer2.UserKey.Enabled = .F. 
    loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.SETFOCUS
    loNtFrmSet.ariaForm1.ariacontainer2.UserKey.Enabled = .T. 
    
  ENDIF 
  
ENDIF
SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfvNewUsr
*: Developer : Ahmed Maher Keshk (AMK)
*: Date      : 05/07/2009 
*! Purpose   : New User of Notify Screen
*!*************************************************************
FUNCTION lfvNewUsr
PARAMETERS loNtFrmSet
SELECT 'PMPCTGNT'
lcOprt_Ctg = loNtFrmSet.lccatg   
lcOprt_id = loNtFrmSet.lctaskid 
IF !Seek(lcOprt_Ctg+lcOprt_id+SPACE(10))
  gfAPPEND() 
ENDIF    
gfREPLACE("COprt_Ctg WITH lcOprt_Ctg,"+;
          "COprt_ID  WITH lcOprt_id")
=gfAdd_Info('PMPCTGNT')          
gfREPLACE("")
TABLEUPDATE(.T.)          
WITH loNtFrmSet.ariaForm1.ariacontainer2
  .UserKey.Enabled = .T.  
  .txtEmail.Enabled = .F.
  .cbnotbefst.Enabled = .F.
  .cbnotbefcom.Enabled = .F.
  .cbnor.Enabled = .F.
  .cbnoc.Enabled = .F.
  .cbnos.Enabled = .F.
  .txtNBC.Enabled = .F.
  .txtNBS.Enabled = .F.
  .txtNOSD.Enabled = .F.
  .txtNOCD.Enabled = .F.
  .chkCmpDely.Enabled = .F.
  .chkStrtDely.Enabled = .F.
ENDWITH   
loNtFrmSet.ariaForm1.Ariacontainer1.grdUsers.Refresh()


FUNCTION lfvStrtDely
PARAMETERS loNtFrmSet
WITH loNtFrmSet.ariaForm1.ariacontainer2
  SELECT 'PMPCTGNT'
  lLSTRTDELAY  = .chkStrtDely.Value 
  gfREPLACE("LSTRTDELAY WITH lLSTRTDELAY ") 
          
  IF  .chkStrtDely.Value
    .txtNOSD.Enabled = .T.
    .txtNOSD.Value = 1
    gfREPLACE("nstrtdelay WITH 1") 
  ELSE 
    .txtNOSD.Enabled = .F.
    .txtNOSD.Value = 0
    gfREPLACE("nstrtdelay WITH 0") 
  ENDIF 
ENDWITH 
loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh 




FUNCTION lfvCompDly
PARAMETERS loNtFrmSet
SELECT 'PMPCTGNT'
WITH loNtFrmSet.ariaForm1.ariacontainer2
  llLCMPLDELAY  = .chkCmpDely.Value 
  gfREPLACE("LCMPLDELAY WITH llLCMPLDELAY") 
          
  IF  .chkCmpDely.Value
    .txtNOCD.Enabled = .T.
    .txtNOCD.Value = 1
    gfREPLACE("ncmpldelay WITH 1")
  ELSE 
    .txtNOCD.Enabled = .F.
    .txtNOCD.Value = 0
    gfREPLACE("ncmpldelay WITH 0")
  ENDIF 
ENDWITH 
loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh 
