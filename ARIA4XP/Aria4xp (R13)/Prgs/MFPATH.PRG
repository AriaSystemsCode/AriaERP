*!*****************************************************************************************
*! Name      : mfpath.PRG
*! Developer : Hesham Elmasry (HES)
*! Date      : 05/07/2009
*! Purpose   : Template Screen Screen
*! Entry no. : N037574
*!*****************************************************************************************
*Modifications:
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[T20101022.0001]
*!*****************************************************************************************
#INCLUDE R:\Aria4XP\prgS\mfpath.h
DO FORM (oAriaApplication.ScreenHome+ 'mfpath.SCX')
*!*************************************************************
*! Name      : lfNtfInit
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : Init of Notification screen Screen
*!*************************************************************
FUNCTION lfNtfInit
PARAMETERS loNtFrmSet,lcPathID, lcCatgID, lcTaskID, lcMode

WITH loNtFrmSet
  .lcmode = lcMode
  .lctaskid = lcTaskID
  .lccatg = lcCatgID
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

SELECT PMPTHNTF
gfSetorder('PMPTHNTF')
SET FILTER TO coprt_ctg =lcCatgID  AND coprt_id = lcTaskID
GO TOP
loNtFrmSet.lcScreentable = 'PMPTHNTF'
IF !loNtFrmSet.llifok
  WITH loNtFrmSet.Ariaform1.Ariacontainer1.grdUsers
    .RecordSource = 'PMPTHNTF'
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

IF !EOF()
  loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh()
  loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AfterRowColChange
ENDIF

loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh()
loNtFrmSet.changemode(Lcmode)


*!*************************************************************
*! Name      : lfChngMode
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : Change mode of notiications Screen
*!*************************************************************
FUNCTION lfChngMode
PARAMETERS loNtFrmSet
SELECT PMPTHNTF
gfSetorder('PMPTHNTF')

*!*************************************************************
*! Name      : lfAftrRowCol
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : After row col. changed of Notification screen
*!*************************************************************
FUNCTION lfAftrRowCol
PARAMETERS loNtFrmSet
SELECT PMPTHNTF
gfSETORDER('PMPTHNTF')   && CPATH_ID+COPRT_CTG+COPRT_ID+CUSER_ID

IF !EOF()
  WITH loNtFrmSet.AriaForm1.Ariacontainer2
    .UserKey.Keytextbox.Value = cuser_id
    .txtEmail.Value = cemail_add
    .cbnotbefst.Value = lbfrstrt
    .cbnotbefcom.Value = lbfrcmplt
    .cbnor.Value = lonredrct
    .cbnoc.Value = loncmplt
    .cbnos.Value = lonstrt
    .txtNBC.Value = nbfrcmpldy
    .txtNBS.Value = nbfrstrtdy
    .txtNOSD.Value = nstrtdelay
    .txtNOCD.Value = ncmpldelay
    .chkCmpDely.Value= LCMPLDELAY
    .chkStrtDely.Value=LSTRTDELAY
  ENDWITH

  IF lbfrstrt
    loNtFrmSet.AriaForm1.Ariacontainer2.txtNBS.Enabled = !(loNtFrmSet.ActiveMode = 'V')
  ELSE
    loNtFrmSet.AriaForm1.Ariacontainer2.txtNBS.Enabled = .F.
  ENDIF

  IF lbfrcmplt
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
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : Validate user ID screen
*!*************************************************************
FUNCTION lfvUsrId
PARAMETERS loNtFrmSet

m.cAdd_User  = oAriaApplication.User_ID
m.dAdd_Date  = oAriaApplication.Systemdate
m.cAdd_Time  = TIME()
m.coprt_ctg = loNtFrmSet.lccatg
m.coprt_id = loNtFrmSet.lctaskid
m.cpath_id = loNtFrmSet.lcpathid
m.lbfrstrt   = .F.
m.nbfrstrtdy = 0
m.lbfrcmplt  = .F.
m.nbfrcmpldy = 0
m.lonstrt    = .F.
m.loncmplt   = .F.
m.lonredrct  = .F.
m.nstrtdelay = 0
m.ncmpldelay = 0
m.cemail_add = ''
m.LCMPLDELAY = .F.
m.LSTRTDELAY = .F.

*!*	DIMENSION laFields[1]

*!*	SELECT SYUUSER
*!*	gfseek('')

*!*	lcBrFields = "cuser_id    :H='"+LANG_MFPATH_USERID+"'  ," +;
*!*	             "cusr_name   :H='"+LANG_MFPATH_USERNAME+"'"
*!*	
*!*	lcUserID = loNtFrmSet.AriaForm1.Ariacontainer2.UserKey.Keytextbox.Value
*!*	llNew = loNtFrmSet.llifnew

*!*	IF SEEK(lcUserID) AND !loNtFrmSet.AriaForm1.Ariacontainer2.UserKey.selectedfrombrowse
*!*	  loNtFrmSet.AriaForm1.Ariacontainer2.UserKey.Keytextbox.Value = lcUserID
*!*	  m.cuser_id   = lcUserID
*!*	  loNtFrmSet.AriaForm1.Ariacontainer2.txtEmail.Value = ''
*!*	  IF llNew
*!*	    SELECT PMPTHNTF
*!*	    gfSetorder('PMPTHNTF')
*!*	    gfAppend('PMPTHNTF',.T.)
*!*	    TABLEUPDATE()
*!*	    lfShow(loNtFrmSet)
*!*	    loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh
*!*	    loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AfterRowColChange
*!*	    loNtFrmSet.llifnew = .F.
*!*	  ELSE
*!*	    SELECT PMPTHNTF
*!*	    gfSetorder('PMPTHNTF')
*!*	    IF !EOF()
*!*	      gfReplace('cuser_id with m.cuser_id')
*!*	      TABLEUPDATE()
*!*	      loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh
*!*	      loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AfterRowColChange
*!*	    ENDIF
*!*	  ENDIF
*!*	ELSE
*!*	    *** Browse to select a User
*!*	    SELECT SYUUSER
*!*	    gfSetorder('CUSER_ID')
*!*	    GO TOP
*!*	    llSel = gfBrows('','cuser_id','laFields','Users')
*!*	    IF llSel
*!*	      lcUserID = laFields[1,1]
*!*	      loNtFrmSet.AriaForm1.Ariacontainer2.UserKey.Keytextbox.Value = lcUserID
*!*	      m.cuser_id   = lcUserID
*!*	      loNtFrmSet.AriaForm1.Ariacontainer2.txtEmail.Value = ''
*!*	      IF llNew
*!*	        SELECT PMPTHNTF
*!*	        gfSetorder('PMPTHNTF')
*!*	        gfAppend('PMPTHNTF',.T.)
*!*	        TABLEUPDATE()
*!*	        lfShow(loNtFrmSet)
*!*	        loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh
*!*	        loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AfterRowColChange
*!*	        loNtFrmSet.llifnew = .F.
*!*	      ELSE
*!*	        SELECT PMPTHNTF
*!*	        gfSetorder('PMPTHNTF')
*!*	        IF !EOF()
*!*	          gfReplace('cuser_id with m.cuser_id')
*!*	          TABLEUPDATE()
*!*	          loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh
*!*	          loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AfterRowColChange
*!*	        ENDIF
*!*	      ENDIF
*!*	    ELSE
*!*	      loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AfterRowColChange
*!*	      RETURN .F.
*!*	    ENDIF
*!*	ENDIF
lcPMPHNTF = loNtFrmSet.lcScreentable
WITH loNtFrmSet.AriaForm1.Ariacontainer2.UserKey
  lcOldVal   = .Keytextbox.OldValue
  m.cUser_ID = .Keytextbox.Value
  llBrowse   = .selectedfrombrowse
ENDWITH

IF (PADR(m.cUser_ID,10) = PADR(lcOldVal,10)) .AND. !llBrowse
  RETURN .T.
ENDIF

PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
SELECT SYUUSER
lcBrFields = "cuser_id    :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPATH_USERID,loFormSet.GetHeaderText("LANG_MFPATH_USERID",loFormSet.HeaderAlias))+"'  ," +;
"cusr_name   :H='"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPATH_USERNAME,loFormSet.GetHeaderText("LANG_MFPATH_USERNAME",loFormSet.HeaderAlias))+"'"

IF (!EMPTY(m.cUser_ID) AND !gfSEEK(PADR(m.cUser_ID,10),'SYUUSER')) .OR. llBrowse
  llBrowse = .F.
  DIMENSION laTemp[2]
  *N000682,1 MMT 12/03/2012 Globalization changes[Start]
  *IF ARIABROW('','Users',gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,'cUser_Id,cEmail_Add','laTemp')  
  IF ARIABROW('',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPATH_USERS,loFormSet.GetHeaderText("LANG_MFPATH_USERS",loFormSet.HeaderAlias)),gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,.F.,.F.,'cUser_Id,cEmail_Add','laTemp')
  *N000682,1 MMT 12/03/2012 Globalization changes[END]
    m.cUser_ID   = laTemp[1]
    m.cEmail_Add = laTemp[2]
  ELSE
    m.cUser_ID   = lcOldVal
  ENDIF
  loNtFrmSet.AriaForm1.Ariacontainer2.UserKey.Keytextbox.Value = m.cUser_ID
  lcOldVal = loNtFrmSet.AriaForm1.Ariacontainer2.UserKey.Keytextbox.OldValue
  IF !EMPTY(m.cUser_ID) &&AND m.cUser_ID <> lcOldVal
    lnTmpRecNo = RECNO(lcPMPHNTF)
    IF !SEEK(m.cpath_id+m.coprt_ctg+m.coprt_id+m.cUser_ID,lcPMPHNTF)
      m.cEmail_Add = SYUUSER.CEMAIL_ADD
    ELSE
      *N000682,1 MMT 12/03/2012 Globalization changes[Start]
      *WAIT WINDOW "User Exist"
      WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPATH_EXISTINGUSER,loFormSet.GetHeaderText("LANG_MFPATH_EXISTINGUSER",loFormSet.HeaderAlias))
      *N000682,1 MMT 12/03/2012 Globalization changes[END]      
      m.cUser_ID = lcOldVal
    ENDIF
    IF BETWEEN(lnTmpRecNo,1,RECCOUNT(lcPMPHNTF))
      GO lnTmpRecNo IN (lcPMPHNTF)
    ENDIF
  ENDIF

ELSE
  IF !EMPTY(m.cUser_ID)
    lnTmpRecNo = RECNO(lcPMPHNTF)
    IF !SEEK(m.cpath_id+m.coprt_ctg+m.coprt_id+m.cUser_ID,lcPMPHNTF)
      m.cEmail_Add = SYUUSER.CEMAIL_ADD
    ELSE
      *N000682,1 MMT 12/03/2012 Globalization changes[Start]
      *WAIT WINDOW "User Exist"
      WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_MFPATH_EXISTINGUSER,loFormSet.GetHeaderText("LANG_MFPATH_EXISTINGUSER",loFormSet.HeaderAlias))
      *N000682,1 MMT 12/03/2012 Globalization changes[END]
      m.cUser_ID = lcOldVal
    ENDIF
    IF BETWEEN(lnTmpRecNo,1,RECCOUNT(lcPMPHNTF))
      GO lnTmpRecNo IN (lcPMPHNTF)
    ENDIF
  ENDIF
ENDIF

SELECT (lcPMPHNTF)
gfREPLACe('cUser_ID   WITH m.cUser_ID,cEmail_Add WITH m.cEmail_Add')


SELECT (lnCurAlias)
loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.AfterRowColChange
*!*************************************************************
*! Name      : lfShow
*: Developer : Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : Rerfesh method
*!*************************************************************
FUNCTION lfShow
PARAMETERS loNtFrmSet
WITH loNtFrmSet.AriaForm1.Ariacontainer2
  .UserKey.Enabled = .T.
  .txtEmail.Enabled = .T.
  .cbnotbefst.Enabled = .T.
  .cbnotbefcom.Enabled = .T.
  .cbnor.Enabled = .T.
  .cbnoc.Enabled = .T.
  .cbnos.Enabled = .T.
  .txtNBC.Enabled = .T.
  .txtNBS.Enabled = .T.
  .txtNOSD.Enabled = .T.
  .txtNOCD.Enabled = .T.
  .chkCmpDely.Enabled = .T.
  .chkStrtDely.Enabled = .T.
ENDWITH

WITH loNtFrmSet.AriaForm1.Ariacontainer3
  .btnRemove.Enabled = .T.
  .btnNew.Enabled = .T.
  .btnClose.Enabled = .T.
ENDWITH

*!*************************************************************
*! Name      : lfvEmail
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : Validate email Notification Screen
*!*************************************************************
FUNCTION lfvEmail
PARAMETERS loNtFrmSet
SELECT PMPTHNTF
gfSetorder('PMPTHNTF')
lcValue = loNtFrmSet.AriaForm1.Ariacontainer2.txtEmail.Value
gfREPLACE('cemail_add WITH lcValue')
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[Start]
llupdatenotify = .T.
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[End]
loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh

*!*************************************************************
*! Name      : lfvNtb4Strt
*: Developer : Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : Notify before Start of Notification screen
*!*************************************************************
FUNCTION lfvNtb4Strt
PARAMETERS loNtFrmSet
IF loNtFrmSet.ariaForm1.ariacontainer2.cbnotbefst.Value  = .F.
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Enabled = .F.
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Value = 0
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Valid
ELSE
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Enabled = .T.
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Value = 1
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Valid
ENDIF

SELECT PMPTHNTF
gfSetorder('PMPTHNTF')
lcValue = loNtFrmSet.ariaForm1.ariacontainer2.cbnotbefst.Value
gfREPLACE('lbfrstrt WITH lcValue')
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[Start]
llupdatenotify = .T.
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[End]

loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh

*!*************************************************************
*! Name      : lfvNtfb4StrtDays
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : Notify before Start days of Notification screen
*!*************************************************************
FUNCTION lfvNtfb4StrtDays
PARAMETERS loNtFrmSet
SELECT PMPTHNTF
gfSETORDER('PMPTHNTF')
lcValue = loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Value
IF EMPTY(lcValue)
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBS.Value = 0
  lcValue = 0
ENDIF
gfREPLACE('nbfrstrtdy WITH lcValue')
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[Start]
llupdatenotify = .T.
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[End]

loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh

*!*************************************************************
*! Name      : lfvNtfb4Comp
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : Notify before Completion of Notification screen
*!*************************************************************
FUNCTION lfvNtfb4Comp
PARAMETERS loNtFrmSet
IF loNtFrmSet.ariaForm1.ariacontainer2.cbnotbefcom.Value  = .F.
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Enabled = .F.
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Value = 0
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Valid
ELSE
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Enabled = .T.
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Value = 1
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Valid
ENDIF

SELECT PMPTHNTF
gfSetorder('PMPTHNTF')
lcValue =  loNtFrmSet.ariaForm1.ariacontainer2.cbnotbefcom.Value
gfREPLACE('lbfrcmplt WITH lcValue')
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[Start]
llupdatenotify = .T.
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[End]

loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh

*!*************************************************************
*! Name      : lfVNtfB4Cmpl
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : Notify before Completion days screen Screen
*!*************************************************************
FUNCTION lfVNtfB4Cmpl
PARAMETERS loNtFrmSet

SELECT PMPTHNTF
gfSetorder('PMPTHNTF')
lcValue = loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Value
IF EMPTY(lcValue)
  loNtFrmSet.ariaForm1.ariacontainer2.txtNBC.Value  = 0
  lcValue = 0
ENDIF
gfREPLACE('nbfrcmpldy WITH lcValue')
loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh

*!*************************************************************
*! Name      : lfvNtfStrtDl
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : Notify before start delay of Notification screen
*!*************************************************************
FUNCTION lfvNtfStrtDl
PARAMETERS loNtFrmSet
SELECT PMPTHNTF
gfSetorder('PMPTHNTF')
lcValue = loNtFrmSet.ariaForm1.ariacontainer2.txtNOSD.Value
IF EMPTY(lcValue)
  loNtFrmSet.ariaForm1.ariacontainer2.txtNOSD.Value = 0
  lcValue = 0
ENDIF
gfREPLACE('nstrtdelay WITH lcValue')
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[Start]
llupdatenotify = .T.
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[End]

loNtFrmSet.AriaForm1.Ariacontainer1.grdUsers.Refresh

*!*************************************************************
*! Name      : lfvNtfCD
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : Notification screen
*!*************************************************************
FUNCTION lfvNtfCD
PARAMETERS loNtFrmSet
SELECT PMPTHNTF
gfSetorder('PMPTHNTF')
lcValue = loNtFrmSet.ariaForm1.ariacontainer2.txtNOCD.Value
IF EMPTY(lcValue)
  loNtFrmSet.ariaForm1.ariacontainer2.txtNOCD.Value  = 0
  lcValue = 0
ENDIF
gfREPLACE('ncmpldelay WITH lcValue')
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[Start]
llupdatenotify = .T.
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[End]

loNtFrmSet.ariaForm1.Ariacontainer1.grdUsers.Refresh

*!*************************************************************
*! Name      : lfvNtfOnStrt
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : on Start of Notification screen
*!*************************************************************
FUNCTION lfvNtfOnStrt
PARAMETERS loNtFrmSet
SELECT PMPTHNTF
gfSetorder('PMPTHNTF')
lcValue = loNtFrmSet.ariaForm1.ariacontainer2.cbnos.Value
gfReplace('lonstrt WITH lcValue')
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[Start]
llupdatenotify = .T.
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[End]

loNtFrmSet.ariaForm1.Ariacontainer1.grdUsers.Refresh

*!*************************************************************
*! Name      : lfvNtfOnComp
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : on Complete  of Notification screen
*!*************************************************************
FUNCTION lfvNtfOnComp
PARAMETERS loNtFrmSet
SELECT PMPTHNTF
gfSetorder('PMPTHNTF')
lcValue = loNtFrmSet.ariaForm1.ariacontainer2.cbnoc.Value
gfREPLACE('loncmplt WITH lcValue')
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[Start]
llupdatenotify = .T.
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[End]

loNtFrmSet.ariaForm1.Ariacontainer1.grdUsers.Refresh

*!*************************************************************
*! Name      : lfVNtfonRedir
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : on Redirect of Notification screen
*!*************************************************************
FUNCTION lfVNtfonRedir
PARAMETERS loNtFrmSet

SELECT PMPTHNTF
gfSetorder('PMPTHNTF')
lcValue = loNtFrmSet.ariaForm1.ariacontainer2.cbnor.Value
gfREPLACE('lonredrct WITH lcValue')
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[Start]
llupdatenotify = .T.
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[End]

loNtFrmSet.ariaForm1.Ariacontainer1.grdUsers.Refresh

*!*************************************************************
*! Name      : lfvRmvUser
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : remove user of Notification screen Screen
*!*************************************************************
FUNCTION lfvRmvUser
PARAMETERS loNtFrmSet
IF gfModalgen("TRM38208B38006","DIALOG") = 1
  SELECT PMPTHNTF
  gfSETORDER('PMPTHNTF')   && CPATH_ID+COPRT_CTG+COPRT_ID+CUSER_ID
  gfDELETE()
  TABLEUPDATE(.T.)
  GO TOP
  IF !EOF()
    loNtFrmSet.ariaForm1.Ariacontainer1.grdUsers.Refresh()
    loNtFrmSet.ariaForm1.Ariacontainer1.grdUsers.AfterRowColChange()
  ELSE
    loNtFrmSet.ariaForm1.Ariacontainer1.grdUsers.Refresh()
    loNtFrmSet.lfclear()

    loNtFrmSet.ariaForm1.ariacontainer3.btnRemove.Enabled = .F.

    *-* Update the memo field in the PMPTHDT file with an empty value.
    SELECT PMPTHDT
    gfSetorder('PMPTHDT')
    lcNotf = ''
    IF SEEK(loNtFrmSet.lcpathid+loNtFrmSet.lccatg+loNtFrmSet.lctaskid)
      gfReplace('mnotify with lcNotf')
      TABLEUPDATE(.T.)
    ENDIF
  ENDIF
  *:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[Start]
  llupdatenotify = .T.
  *:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[End]

ENDIF


*!*************************************************************
*! Name      : lfvClosNtf
*: Developer :  Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : Close button of Notification screen Screen
*!*************************************************************
FUNCTION lfvClosNtf
PARAMETERS loNtFrmSet
DIMENSION LaNotf[1]
SELECT PMPTHNTF
gfSetorder('PMPTHNTF')
IF !EOF()
  *-* Update the memo field in the PMPTHDT file with new data.
  lcFilter = FILTER()
  SELECT cuser_id FROM PMPTHNTF WHERE IIF(EMPTY(lcFilter),.T.,EVALUATE(lcFilter)) INTO ARRAY LaNotf

  lnItms = ALEN(LaNotf)
  lcNotf = ''
  FOR x = 1 TO lnItms
    lcNotf = lcNotf + ALLTRIM(LaNotf[1,x]) + '\ '
  ENDFOR
  lnEnd = LEN(lcNotf)
  lcNotf = SUBSTR(lcNotf,1,lnEnd - 2)
  lcNotf = lcNotf + '.'

  SELECT PMPTHDT
  gfSetorder('PMPTHDT')

  IF SEEK(loNtFrmSet.lcpathid+loNtFrmSet.lccatg+loNtFrmSet.lctaskid)
    gfReplace('mnotify with lcNotf')
    TABLEUPDATE(.T.)
  ENDIF
ENDIF
SELECT PMPTHNTF
SCAN FOR EMPTY(cuser_id)
  =gfDelete()
ENDSCAN
TABLEUPDATE(.T.)
*!*************************************************************
*! Name      : lfvNewUsr
*: Developer : Hesham Elmasry (HES)
*: Date      : 05/07/2009
*! Purpose   : Validate New button of Notification screen
*!*************************************************************
FUNCTION lfvNewUsr
PARAMETERS loNtFrmSet
lcScreentable = loNtFrmSet.lcScreentable
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[Start]
llupdatenotify = .T.
*:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[End]

SELECT (lcScreentable)

m.coprt_ctg = loNtFrmSet.lccatg
m.coprt_id = loNtFrmSet.lctaskid
m.cpath_id = loNtFrmSet.lcpathid

IF !SEEK(m.cpath_id+m.coprt_ctg+m.coprt_id+SPACE(10))
  gfAPPEND()
ENDIF

gfREPLACE("cpath_id with m.cpath_id")
gfREPLACE("COprt_Ctg WITH m.coprt_ctg,"+;
          "COprt_ID  WITH m.coprt_id")


=gfAdd_Info(lcScreentable)
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
ENDWITH
loNtFrmSet.ariaForm1.Ariacontainer1.grdUsers.Refresh()



FUNCTION lfvStrtDely
PARAMETERS loNtFrmSet
WITH loNtFrmSet.ariaForm1.ariacontainer2
  SELECT PMPTHNTF
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
SELECT PMPTHNTF
WITH loNtFrmSet.ariaForm1.ariacontainer2
  llLCMPLDELAY  = .chkCmpDely.Value
  gfREPLACE("LCMPLDELAY WITH llLCMPLDELAY")
  *:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[Start]
  llupdatenotify = .T.
  *:E302853,1 MMT 02/08/2011 add message to update project when template notification is changed[End]

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
