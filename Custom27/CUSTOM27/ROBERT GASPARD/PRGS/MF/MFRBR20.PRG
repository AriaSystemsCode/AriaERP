*****************************************************************************
*: Program file  : MFRBR20.PRG (C#200092)
*: Program desc. : Tracking Cuting Ticket.
*: Module        : MF (Manufacturing)
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls : FUNCTION  : lfOpenFiles,lfvSLogOut,lfVbwOk,lfvUsrPass,lfvUserId
*:                     lfUnTrap,lfKey,lfTrap,lfTempFile,lfActsBrow,lfwBrows
*:                     lfvBrowse,lfVCut,lfVMfCut,lfOpenTemp,lfvCosSh,lfVStyle
*:                     lfVOpCode,lfIssOp,lfvScale,lfUpOpHdDt,lfUpBom
*:                     lfUpCutTkt,lfUpdTRHL,lfDisable,lfRestLogc,
*:                     lfvSav,lfVEdit,lfvNew,lfvMisOp,lfVMisOk,lfvRem,lfIssPreOp
*:                     lfComplete,lfvClos,lfToDo,lfCompTkt,lfPartTkt,lfvIss
*:                     lfvPen,lfvComp,lfPend,lfCanc,lfvModOk,lfErase,lfCalcTim
*:                     lfCkNxtIss,lfDelNxt.
*:****************************************************************************
*:         PROCEDURE : lpTab,lpBackTab,lpEsc
*:****************************************************************************
*: Commentes : 
*****************************************************************************
*: Passed Parameters  : None.
*****************************************************************************
*: Modifications :
*: B803310,1 SSH Fix the bug of can not browse sizes and document some function
*: B803342,1 SSH Add User Id Browse
*: B803342,1 SSH 1- The program allows the user to enter operation#3 before 
*: B803342,1 SSH    operation#1. When that happens, the program issues and completes
*: B803342,1 SSH    all previous operations automatically. This is ok except that 
*: B803342,1 SSH    the program does that for all sizes on one shot. The program 
*: B803342,1 SSH    should do that for the scanned size only. In other words, if the
*: B803342,1 SSH    user entered operation#3 for size small, do not complete 
*: B803342,1 SSH    operation#1 for size large.
*: B803342,1 SSH 2- When the previous problem happened, I noticed that the user ID 
*: B803342,1 SSH    is saved with "****" on records type "I" and "*******" for 
*: B803342,1 SSH    records type "C". I am not sure if this is going to cause a 
*: B803342,1 SSH    problem anywhere else but please make sure to fix record type 
*: B803342,1 SSH    "I" to use the same number of stars.
*: B803342,1 SSH 3- The user who is doing the scanning should be able to change and 
*: B803342,1 SSH    modify the qty before saving. Once he saves modifications can be
*: B803342,1 SSH    done by supervisors only.
*: B803342,1 SSH 4- Any field with name "USER ID" should allow the user to enter a 
*: B803342,1 SSH    "?" to open users list and any entered user ID should be valid 
*: B803342,1 SSH    in the users file.
*: B803342,1 SSH 5- The program allows the supervisor to add a new operation without
*: B803342,1 SSH    selecting a user ID. There is not point of doing something like 
*: B803342,1 SSH    that. Do not allow the user to do that.
*: B803342,1 SSH 
*: B803342,1 SSH 6- After completing an operation, the supervisor should be able to 
*: B803342,1 SSH    remove it. I tried to do that and the program gave a message 
*: B803342,1 SSH    saying "No records to display" but did not actually remove the 
*: B803342,1 SSH    line. I had one operation on the browse at that time.
*: B803342,1 SSH 
*: B803342,1 SSH 7- Since that we can have more than one size showing on the same 
*: B803342,1 SSH    browse record, there is no point to display the size code in the
*: B803342,1 SSH    browse. Please replace the size code with the scale code. This 
*: B803342,1 SSH    will be more meaningfull and less confusing for the user.
*: B603772,1 SSH    Fix sume issues according to this bug.
*: B603835,1 SSH 28/08/2000 Fix the following bugs
*: B603835,1 SSH    1- Modify the progrm to handele rework pieces
*: B603835,1 SSH    2- remove start time screen.
*: B804272,1 SSH 07/12/2001 Optimize the speed and add the required code
*: B804272,1 SSH            that should handle the case of PO.
*: B122285,1 NNA 03/29/2004 Fix bug of can't Receive after two times
*: C124152,1 NNA 11/09/2004 Modify the PRG to be sure that it will update the actual time
*: B129938,1 NNA 12/07/2005 Fix Bug that The Prg. is changing the actual time spent after we run the end process
*****************************************************************************
lcGlYear   = ''
lcGlPeriod = ''
IF !CHECKPRD(gdSysDate,'lcGlYear','lcGlPeriod','IA',.T.)
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Invalid date, cannot proceed.')    
  RETURN
ENDIF
*--- Logical Variable
STORE .T. TO llC,llWrongPass,llIssued,llFirst

*C124152,1 NNA 11/09/2004 (Begin) Add New variables
*--Add 3 variables :
 *- 1 - llFound   --> hold True if there is an operation still open or not has actual time.
 *- 2 - llOpenRep --> hold True if there is a report will display for the user.
 *- 3 - lcFilHandl -> hold the name of the text log file 
 *- 4 - lnColorLen -> hold the Length of the style color
 *- 5 - llSameUser -> hold True if this is the same user who issued the operation
 *- 6 - lnUserHrs  -> hold the user's true time 
*STORE .F. TO llS,llO,llReci
STORE .F. TO llS ,llO ,llReci ,llFound ,llOpenRep ,llSameUser
STORE '' TO lcFilHandl
STORE 0 TO lnColorLen , lnUserHrs

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')
lcMjrPct  = gfItemMask('PM')
 = lfNonMaj()
lnstylewid=LEN(lcMjrPct) + lnColorLen


*C124152,1 NNA (End)

DIME lnLastComp[9]
lnLastComp = 0
DIMEN laPreOp[1],laLastOp[1] , laseq[1] , laNxtOp[1]
laPreOp=''
laLastOp = ''
laNxtOp = ''
laseq    = ''
llUpdated = .F.
*--- Char Variable
STORE SPACE(10) TO lcCTStatus,lcHedrWare,lnCurrOpSq
lnToDo    = 0
lnOldHors = 0
llClose   = .T.
lcOp      = SPACE(6)
lcUserId  = SPACE(1)
lcStyDesc = SPACE(60)
llAdmin   = .T.
llTherIsOp = .F.
lcNewRem = 'N'
IF !gfSetup()
  RETURN
ENDIF
*---Temp File Declaration .
lcGlDTemp  = ''
lcDiffGls = gfTempName()
lcCutTran  = gfTempName()
lcPOTran   = gfTempName()
lcBomCost  = gfTempName()
lcCTktBom  = gfTempName()
lcTmpQuery = gfTempName()
lcTmpSty   = gfTempName()
lcTmpMis   = gfTempName()
lcTmpOp    = gfTempName()
lcTktOp    = gfTempName()
lcOprToDel = gfTempName()

*C124152,1 NNA 11/09/2004 (Begin) Declare new temp file to use it as log file
lcTmpLog   = gfTempName()
*C124152,1 NNA (End)

*: B603835,1 SSH 28/08/2000 Start and end time temp file.
lcTrIssTim = gfTempName()
*: B603835,1 SSH 
*: B803342,1 SSH Temp file for User Id 
lcUsrId = gfTempName()
*: B803342,1 SSH 
*--- Operation detail.
lcOprDt    = gfTempName()
*--- Track header & line
lcTrkHdr   = gfTempName()
lcTrkLin   = gfTempName()
*--- Style Picture & Title
lcStyTit = gfItemMask('HI')
lcMjrPct = gfItemMask("PI")
*---- Function to allow the user to log in the screen
*---- It get User Id and pass word .
DO WHILE .T.
  IF !lfvSLogOut()
    RETURN
  ENDIF
  lcMtBrowTt = 'Tickets'
  DIMENSION laSetups[2,2]
  laSetups[1,1] = 'M_WAREHOUS'
  laSetups[2,1] = 'M_LINK_GL'
  =gfGetMemVar(@laSetups,gcAct_Comp)
  llMultiWar = IIF( laSetups[1,2] = 'Y' , .T. , .F.)
  IF laSetups[2,2] = 'Y'
    lcGlDTempM = gfTempName()
  ENDIF
  =lfOpenFiles()
  =lfTempFile()
  llFrstTim = .T.
  DO gcScrDir+'MF\'+("MFRBR20.SPR")
ENDDO
*:*************************************************************
*: Name      : lfOpenFiles.
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 07/01/00
*: Purpose   : Function to open nessesary files.
*:*************************************************************
*: Calls     : gfOpenFile
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfOpenFiles()
*:*************************************************************
*:
FUNCTION lfOpenFiles

=gfOpenFile(gcDataDir+'CutTktH','CutTktH','SH')
=gfOpenFile(gcDataDir+'Scale','Scale','SH')
=gfOpenFile(gcDataDir+'CutTktL','CutTktL','SH')
=gfOpenFile(gcDataDir+'BomLine','MfgOpr','SH')
=gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
=gfOpenFile(gcDataDir+'POSLN','POSLN','SH')
=gfOpenFile(gcDataDir+'Style','Style','SH')
=gfOpenFile(gcDataDir+'MfgOprDt','Mfgoprdt','SH')
=gfOpenFile(gcDataDir+'mfgoprhd','mfgoprhd','SH')
=gfOpenFile(gcDataDir+'TrakLin','TrakLin','SH')
=gfOpenFile(gcDataDir+'TrakHdr','TrakHdr','SH')
=gfOpenFile(gcDataDir+'CtktBom','Ctktyp','SH')
=gfOpenFile(gcDataDir+'BomCost','Bomcstkt','SH')
*: B603835,1 SSH 28/08/2000 Start and end time temp file.
=gfOpenFile(gcDataDir+'Trstrend','Trstrend','SH')
*: B603835,1 SSH 
*:*************************************************************
*: Name      : lfvSLogOut.
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 07/01/00
*: Purpose   : Screen Log In and Validate password.
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvSLogOut()
*:*************************************************************
FUNCTION lfvSLogOut

llOpnCinst=gfOpenFile(gcSysHome+'SYCINST','','SH')
*--If there is a users defined in the system and login required.
SELECT SYUUser
GOTO TOP
llUsersExt = !EOF('SYUUser') AND SYCINST.lInsLogRq   
IF llOpnCinst AND USED('SYCINST')
  USE IN SYCINST
ENDIF
IF !llUsersExt
  RETURN
ENDIF  
*--Screen title message.
lcUsrMsg = 'CutTkt Tracking MFRBR20'
*--Current user password.
lcCurUsrPas = '' &&&IIF(SEEK(gcUser_id,'SYUUSER'),ALLTRIM(SYUUSER.cUsr_Pass),'*')
lcUsrPass = SPACE(8)
lcUsrId   = SPACE(10)
llWrongPass = .T.
PUSH KEY
ON KEY
SET CONFIRM OFF
DO WHILE llWrongPass
  lcUsrPass=SPACE(8)
  DO (gcScrDir+gcWinAppl+"\MFRBRBW.SPX")
  llAdmin = (syuuser.Cusr_Kbuf = 'SUP')
  IF !llWrongPass
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF
ENDDO
POP KEY
SET CONFIRM ON
RETURN

*:*************************************************************
*: Name      : lfVbwOk.
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 07/01/00
*: Purpose   : Valid Function for ok butt in log in screen .
*:*************************************************************
*: Calls     : gfOpenFile
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfVbwOk()
*:*************************************************************
*:
FUNCTION lfVbwOk
*ALLTRIM(lcUsrPass)=ALLTRIM(lcCurUsrPas)
IF EMPTY(lcUSerId)
    llWrongPass = .T.
    RETURN
ENDIF
IF (!EMPTY(lcCurUsrPas) AND (SYS(2007,ALLTRIM(lcUsrPass))=ALLTRIM(lcCurUsrPas) OR ALLTRIM(lcUsrPass)=ALLTRIM(lcCurUsrPas) )) OR ;
   ( EMPTY(lcCurUsrPas) AND PADR(lcUsrPass,8)=PADR(lcCurUsrPas,8) )
  llWrongPass = .F.
  CLEAR READ
ELSE
  *--Invalid user password.
  =gfModalGen('TRM42138B42000','DIALOG','user password')
  lcUsrPass=SPACE(8)
  llWrongPass = .T.
  _CUROBJ=OBJNUM(PbCancel)
  RETURN
ENDIF

*:*************************************************************
*: Name      : lfvUsrPass.
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 07/01/00
*: Purpose   : Validate user password.
*:*************************************************************
*: Calls     : gfOpenFile
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfVbwOk()
*:*************************************************************
*:
FUNCTION lfvUsrPass

IF EMPTY(lcUserId) OR EMPTY(lcUsrPass)
  RETURN
ENDIF
IF !((!EMPTY(lcCurUsrPas) AND (SYS(2007,ALLTRIM(lcUsrPass))=lcCurUsrPas OR ALLTRIM(lcUsrPass)=ALLTRIM(lcCurUsrPas))) OR ;
   ( EMPTY(lcCurUsrPas) AND PADR(lcUsrPass,8)=PADR(lcCurUsrPas,8) ))
  *--Invalid user password.
  =gfModalGen('TRM42138B42000','DIALOG','user password')
  lcUsrPass=SPACE(8)
  llWrongPass = .T.
  *: B803342,1 SSH 
  _CUROBJ=OBJNUM(lcUsrPass)
  *: B803342,1 SSH [END]
  RETURN
ELSE
  llWrongPass = .F.
  KEYBOARD "{ENTER}"
ENDIF

*:*************************************************************
*: Name      : lfvUserId
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 07/01/00
*: Purpose   : Validate user Id.
*:*************************************************************
*: Calls     : gfOpenFile
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvUserId()
*:*************************************************************
*:
FUNCTION lfvUserId

IF EMPTY(lcUserId)
  _CUROBJ = OBJNUM(PbCancel)
  RETURN
ENDIF
IF !SEEK(PADR(lcUserId,10),'SYUUSER')
  *: B803342,1 SSH Add Browse for user id
  *=gfModalGen('TRM42138B42000','DIALOG','user ID')
  lcdummy = lfBrowId()
  *lcUserId = SPACE(10)
  lcUserId = lcdummy
  lcUsrPass = ''
  IF EMPTY(lcdummy)
    _CUROBJ=OBJNUM(lcUserId)
    RETURN
  ELSE
    lcCurUsrPas = IIF(SEEK(PADR(lcUserId,10),'SYUUSER'),ALLTRIM(SYUUSER.cUsr_Pass),'*')
  ENDIF
  *: B803342,1 SSH [END]
ELSE
  lcCurUsrPas = IIF(SEEK(PADR(lcUserId,10),'SYUUSER'),ALLTRIM(SYUUSER.cUsr_Pass),'*')
ENDIF

*!*************************************************************
*! Name      : lfUnTrap
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : UnTrap function for Browse
*!           : Activate screen function....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
*!
FUNCTION lfUnTrap

* -- Clear Trap
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF
*-- If TOP window is not one of the browses window                                        
IF WONTOP() # ALLTRIM(lcMtBrowTt)
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL CTRL+TAB
  ON KEY LABEL CTRL+ENTER
  ON KEY LABEL CTRL+HOME
  ON KEY LABEL CTRL+END
  ON KEY LABEL CTRL+W
  ON KEY LABEL Esc
ENDIF
*-- end of lfUnTrap.

*!*************************************************************
*! Name      : lfKey
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 20/01/00
*! Purpose   : Screen ICINVLK When Function.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfKey()
*!*************************************************************
*!
FUNCTION lfKey

*-- Activate Browse window
*-- Define a menu bar with a short-cut key for browse window 
*-- in the program just to be able to activate the browse window
*-- using its menu bar short-cut key.
PUSH KEY
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcMtBrowTt KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcMtBrowTt)
IF llAdmin
  SHOW GET pbNew  ENABLE
  SHOW GET pbRem  ENABLE
ELSE
  SHOW GET pbNew  DISABLE
  SHOW GET pbRem  DISABLE
ENDIF
IF llFrstTim
  =lfvScan()
  llFrstTim = .F.
ENDIF
*!*************************************************************
*! Name      : lfTrap
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 20/01/00
*! Purpose   : Trap function for Browse....
*!           : Deactivate screen function.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpTab,lpBackTab
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfTrap

*-- if TOP window is one of the browse window
IF WONTOP() = ALLTRIM(lcMtBrowTt)
  glFromBrow = .T.
  ON KEY LABEL CTRL+ENTER lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+HOME  lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+W     lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+END   lnDummy = 1		&&Do nothing
  ON KEY LABEL TAB           DO lpTab
  ON KEY LABEL CTRL+TAB      DO lpTab
  ON KEY LABEL BACKTAB       DO lpBackTab
  ON KEY LABEL ESC           DO lpEsc
ENDIF
*-- end of lfTrap.

*!*************************************************************
*! Name      : lpTab
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 20/01/00
*! Purpose   : Trapping TAB order for browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
*!
PROCEDURE lpTab

ON KEY LABEL TAB
ON KEY LABEL CTRL+TAB
*-- Go to next window
IF WONTOP() = ALLTRIM((lcMtBrowTt))
   ACTIVATE WINDOW MFRBRBU
  _CUROBJ = OBJNUM(pbSav)
ELSE
   ACTIVATE WINDOW (lcMtBrowTt)
  _CUROBJ = OBJNUM(ibBBrow)
ENDIF
*-- end of lpTab.

*!*************************************************************
*! Name      : lpBackTab
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 20/01/00
*! Purpose   : Trapping BACKTAB order for browse window
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpBackTab
*!*************************************************************
PROCEDURE lpBackTab

ON KEY LABEL BACKTAB
IF WONTOP() = ALLTRIM((lcMtBrowTt))
  *-- Go to previous window
  ACTIVATE WINDOW MFRBRBU		&&Activate popup windows
  _CUROBJ = OBJNUM(pbSav)
ELSE
   ACTIVATE WINDOW (lcMtBrowTt)
  _CUROBJ = OBJNUM(ibBBrow) 
ENDIF  
*-- end of lpBackTab.

*!*************************************************************
*! Name      : lfTempFile
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio to create temp files.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTempFile()
*!*************************************************************
*!
FUNCTION lfTempFile

CREATE TABLE &gcWorkDir.&lcTmpQuery (Type C(1),CutTkt C(6) , Style C(19),StyScl C(2),;
                                     Operation C(06),Scal C(1),Sz1 N(6),Sz2 N(6),Sz3 N(6),;
                                     Sz4 N(6),Sz5 N(6),Sz6 N(6),Sz7 N(6),Sz8 N(6), TotQty N(7),;
                                     UserId C(10), Date D,;
                                     STime N(6,2) , ETime N(6,2),llDis L,Status C(1) ,;
                                     llR L,llIss L,llF L,lSort C(1),SclDes C(5) , llPending L )
INDEX ON lSort+CutTkt+Style+Operation+UserId+Scal TAG &lcTmpQuery
APPEND BLANK
REPLACE lSort WITH 'Z'
PRIVATE lnAlias
lnAlias = SELECT(0)
*: B603835,1 SSH 28/08/2000 Start and end time temp file.
SELECT Trstrend
IF !USED(lcTrIssTim)
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  laFileStru[lnFileStru+1,1] = 'cStatus'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  CREATE TABLE (gcWorkDir+lcTrIssTim) FROM ARRAY laFileStru
  INDEX ON type+cuttkt+mfgcode+style+status+DTOS(diss_date) TAG (lcTrIssTim)
ELSE
  *SELECT(lcTrIssTim)
  *ZAP
ENDIF
*: B603835,1 SSH 
SELECT MfgOprDt
IF !USED(lcTktOp)
  COPY STRUCTURE TO (gcWorkDir+lcTktOp)
ELSE
  SELECT(lcTktOp)
  ZAP
ENDIF
=gfOpenFile(gcWorkDir+lcTktOp,'','EX')
IF !USED(lcOprDt)
  COPY STRUCTURE TO (gcWorkDir+lcOprDt)
ELSE
  SELECT(lcOprDt)
  ZAP
ENDIF
IF !USED(lcOprToDel)
  COPY STRUCTURE TO (gcWorkDir+lcOprToDel)
ELSE
  SELECT(lcOprToDel)
  ZAP
ENDIF

*C124152,1 NNA 11/09/2004 (Begin) Creat the new log file
CREATE TABLE &gcWorkDir.&lcTmpLog (Ctktno C(6),Coprcode C(6))
INDEX ON Ctktno + Coprcode TAG &lcTmpLoG
*C124152,1 NNA (End)

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvDes
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 20/01/00
*! Purpose   : Valid function for scale desc.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : = lfvDes()
*!*************************************************************
*!
FUNCTION lfvDes
PARAMETER llFrmBar
*: B803310,1 SSH New variable
*PRIVATE lnAls
PRIVATE lnAls , llToBrow
llToBrow = .T.
*: B803310,1 SSH [End]
lnAls = SELECT(0)
SELECT (lcTmpQuery)
*SELECT TrakHdr
*lcOldKey = type+cuttkt+style+coprcode+id
*LOCATE FOR DTOS(dIss_date) = DTOS(gdSysDate) .AND. nHours <> 0.00
*IF FOUND()
*  *--- type+cuttkt+style+coprcode+status+coperseq
*  IF !SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'I',"TrakLin");
*     .AND. !SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'C',"TrakLin")
*    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'This date is already closed, cannot issue or complete any operations.')
*    =lfErase()
*    RETURN
*  ENDIF
*ENDIF
*=SEEK(lcOldKey)
*IF EMPTY(CutTkt) OR EMPTY(Style) OR EMPTY(Operation)
*  DO CASE
*    CASE  EMPTY(CutTkt)
*      =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Transaction # is empty, cannot proceed.')
*    CASE  EMPTY(Style)
*      =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Style code is empty, cannot proceed.')
*    CASE  EMPTY(Operation)
*      =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Operation code is empty, cannot proceed.')
*  ENDCASE
*  RETURN
*ENDIF
*: B803310,1 SSH Fix the bug of can not browse sizes
DO WHILE llToBrow
*: B803310,1 SSH [End]
  SELECT Style
  lcTempsc = IIF(SEEK(&lcTmpQuery..Style,'Style'),Style.Scale,'')
  SELECT SCALE
  =SEEK('S'+lcTempsc)
  FOR lnNewInd = 1 TO Cnt
    lcNewInd = STR(lnNewInd,1)
    SELECT SCALE
    IF &lcTmpQuery..SclDes = SCALE.Sz&lcNewInd
      SELECT (lcTmpQuery)
      REPLACE SCAL WITH lcNewInd
      =lfvScale(llFrmBar)
      SELECT(lnAls)
      *KEYBOARD "{ENTER}"
      *: B803310,1 SSH Fix the bug of can not browse sizes
      *RETURN
      llToBrow = .F.
      *: B803310,1 SSH [End]
    ENDIF
  ENDFOR
  *: B803310,1 SSH Fix the bug of can not browse sizes
  IF llToBrow
    lcToRep = lfBrowScla()
    SELECT (lcTmpQuery)
    REPLACE SclDes WITH lcToRep
    *WAIT WINDOW "Invalid Scale Description...!"
    SELECT(lnAls)  
    *RETURN .F.
  ENDIF
ENDDO
RETURN
*: B803310,1 SSH [End]
*!*************************************************************
*! Name      : lfActsBrow
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 20/01/00
*! Purpose   : Activate the browse.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfActsBrow()
*!*************************************************************
*!
FUNCTION lfActsBrow

SELECT &lcTmpQuery
lcBrfield =  [Type   :02  :H ='Type' :W=.F. ,] +;
             [CutTkt :07 :V = lfVCut();
                            :P = '@! XXXXXX';
                            :E = '';
                            :F ;
                            :H ='Ticket' :W = llC,]+;
             [Style  :20 :V = lfVStyle();
                         :P = '@ &lcMjrPct';
                         :E = '';
                         :F ;
                         :H ='Item' :W = llS,]+;
             [StyScl :04 :H ='Scale' :W=.F. ,] +;
             [Operation :07 :V = lfVOpCode();
                            :P = '@ XXXXXX';
                            :H ='Oper'  :W=llO ,] +;
             [Status :04 :H ='Stat' :W=.F. ,] +;
             [Sz1    :06 :H ='Sz1'  :W=.F. ,] +;
             [Sz2    :06 :H ='Sz2'  :W=.F. ,] +;
             [Sz3    :06 :H ='Sz3'  :W=.F. ,] +;
             [Sz4    :06 :H ='Sz4'  :W=.F. ,] +;
             [Sz5    :06 :H ='Sz5'  :W=.F. ,] +;
             [Sz6    :06 :H ='Sz6'  :W=.F. ,] +;
             [Sz7    :06 :H ='Sz7'  :W=.F. ,] +;
             [Sz8    :06 :H ='Sz8'  :W=.F. ,] +;
             [TotQty :07 :H ='TotQty' :W=.F. ,] +;
             [UserId :10 :H ='User ID' :W=.F. ,] +;
             [Date   :08 :H ='Tran. Date' :W=.F. ,] +;
             [STime  :06 :H ='Start' :W=.F. ,] +;
             [ETime  :06 :H ='End'   :W=.F. ]

BROWSE FIELDS &lcBrField;
       WHEN lfwBrows();
       VALID :F lfvBrowse() ;
       WINDOW MFRBRBR IN WINDOW (gcBaseWind);
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE lcMtBrowTt

*!*************************************************************
*! Name      : lfwBrows
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : When browse function.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrows()
*!*************************************************************
*!
FUNCTION lfwBrows

IF llAdmin
  SHOW GET pbEnd ENABLE
ELSE
  SHOW GET pbEnd DISABLE
ENDIF
IF llDis
  STORE .F. TO llC,llS,llO
ELSE
  STORE .T. TO llC,llS,llO
ENDIF
SHOW WINDOW (lcMtBrowTt) REFRESH
*: B803342,1 SSH    Add If type misci. thre is no need for edit
*IF EMPTY(&lcTmpQuery..CutTkt)
IF EMPTY(&lcTmpQuery..CutTkt) .OR. &lcTmpQuery..Type = 'M'
*: B803342,1 SSH[END]
  SHOW GET pbEdit DISABLE
ELSE
  IF (llAdmin) .OR. (!EMPTY(&lcTmpQuery..CutTkt).AND.!SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation,'TRAKHDR') .OR. TRAKHDR.Status='P')
    SHOW GET pbEdit ENABLE
  ELSE
    SHOW GET pbEdit DISABLE
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvBrowse
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid browse function.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrowse()
*!*************************************************************
*!
FUNCTION lfvBrowse

IF WONTOP() # ALLTRIM((lcMtBrowTt))
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lpEsc
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Esc Trap Procedure.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpEsc
*!*************************************************************
*!
PROCEDURE lpEsc

IF !llClose
  =lfvClos()
ELSE
  POP KEY
  RELEASE WINDOW (lcMtBrowTt)
  RELEASE PAD _Option OF _MSYSMENU
  RELEASE BAR 099 OF P01PU01
  RELEASE BAR 100 OF P01PU01
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfVCut
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Vali CutTkt or Po function.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfVCut()
*!*************************************************************
*!
FUNCTION lfVCut
PRIVATE lnAlias

lnAlias = SELECT(0)
*IF Type = 'M'
*  
*  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Current record is a miscellanious transaction.')
*  
*  RETURN
*ENDIF
llTherIsOp = .F.
DO CASE
  SELECT (lcTmpQuery)
  CASE EMPTY(&lcTmpQuery..CutTkt)
    SELECT(lnAlias)
    =lfRestLogc()
    *=gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Transaction # is empty, cannot proceed.')
    RETURN
  CASE  SEEK(&lcTmpQuery..CutTkt,"CutTktH")
    REPLACE Type WITH 'C'
    IF !lfvCosSh('M')
      *=lfDisable()
      RETURN(.F.)
    ENDIF
  CASE  SEEK('P'+&lcTmpQuery..CutTkt,'POSHDR')
    REPLACE Type WITH 'P'
    IF !lfvCosSh('I')
      *=lfDisable()
      RETURN(.F.)
    ENDIF
  CASE  !SEEK('P'+&lcTmpQuery..CutTkt,'POSHDR') AND ;
        !SEEK(&lcTmpQuery..CutTkt,"CutTktH")
    REPLACE Type WITH ''
    IF !lfVMfCut()
      SELECT(lnAlias)
      *=lfDisable()
      RETURN(.F.)
    ENDIF
ENDCASE
STORE !&lcTmpQuery..llDis TO llS,llO
SELECT(lnAlias)
SHOW GET pbClose,1 ENABLE PROMPT gcBmpHome+"Can.BMP"
llClose = .F.
=lfOpenTemp()
RETURN

*!*************************************************************
*! Name      : lfVMfCut
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Vali CutTkt or Po function.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfVMfCut()
*!*************************************************************
*!
FUNCTION lfVMfCut

xCutTkt   = &lcTmpQuery..CutTkt
SELECT CutTktH
llNoThing = CutBrow(xCutTkt)
IF EMPTY(xCutTkt)
  DO POSBrow WITH xCutTkt,"",'P'
  IF !EMPTY(xCutTkt)
    SELECT (lcTmpQuery)
    REPLACE Type WITH 'P'
  ENDIF
ELSE
  SELECT (lcTmpQuery)
  REPLACE Type WITH 'C'
ENDIF
SELECT (lcTmpQuery)
REPLACE CutTkt WITH xCutTkt
IF EMPTY(CutTkt)
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Transaction # is empty, cannot proceed.')
  RETURN (.F.)
ELSE
  IF !lfvCosSh(IIF(&lcTmpQuery..Type = 'C','M','I'))
    RETURN(.F.)
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfOpenTemp
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to open temp file.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfOpenTemp()
*!*************************************************************
*!
FUNCTION lfOpenTemp
PARAMETER lnOldAls

lnOldAls = SELECT(0)
IF !USED(lcOprDt)
  SELECT MfgOprDt
  COPY STRUCTURE TO (gcWorkDir+lcOprDt)
  =gfOpenFile(gcWorkDir+lcOprDt,'','EX')
  INDEX ON cimtyp+ctktno+coprcode+clotno+trancd  TAG (lcOprDt)
ENDIF
IF !USED(lcTrkHdr)
  SELECT TRAKHDR
  COPY STRUCTURE TO (gcWorkDir+lcTrkHdr)
  =gfOpenFile(gcWorkDir+lcTrkHdr,'','EX')
  INDEX ON type+cuttkt+style+coprcode+id TAG (lcTrkHdr)
ENDIF
IF !USED(lcTrkLin)
  SELECT TRAKLIN
  COPY STRUCTURE TO (gcWorkDir+lcTrkLin)
  =gfOpenFile(gcWorkDir+lcTrkLin,'','EX')
  INDEX ON type+cuttkt+style+coprcode+status+coperseq TAG (lcTrkLin)
ENDIF
IF !USED(lcTrIssTim)
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  laFileStru[lnFileStru+1,1] = 'cStatus'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  CREATE TABLE (gcWorkDir+lcTrIssTim) FROM ARRAY laFileStru
  INDEX ON type+cuttkt+mfgcode+style+status+DTOS(diss_date) TAG (lcTrIssTim)
ELSE
*  SELECT(lcTrIssTim)
*  ZAP
ENDIF

SELECT(lnOldAls)
*!*************************************************************
*! Name      : lfvCosSh
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to check if cost sheet exist.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCosSh()
*!*************************************************************
*!
FUNCTION lfvCosSh
PARAMETER lcType

IF !SEEK(lcType+&lcTmpQuery..CutTkt,'BomLine')
  *---Cost Sheet Not found for ~ cuttkt.
  =gfModalGen('TRM38018B00000','DIALOG','cutting ticket')
  *=lfDisable()
  RETURN (.F.)
ENDIF

*!*************************************************************
*! Name      : lfVStyle
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to Valid style.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVStyle()
*!*************************************************************
*!
FUNCTION lfVStyle

*IF Type = 'M'
*  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Current record is a miscellanious transaction.')
*  
*  RETURN
*ENDIF
IF EMPTY(&lcTmpQuery..Style)
    =lfRestLogc()
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Style code is empty, cannot proceed.')
  RETURN
ENDIF
DO CASE
  CASE Type = 'P' AND !SEEK('P'+&lcTmpQuery..CutTkt+&lcTmpQuery..Style,'POSLN')
    CREATE TABLE &gcWorkDir.&lcTmpSty (PO C(06),Vendor C(08),Style C(19),;
                                       Price N(9,2),TotQty N(7))
    INDEX ON PO+Style TAG &lcTmpSty
    SELECT POSLN
    SET ORDER TO POSLN
    =SEEK('P'+&lcTmpQuery..CutTkt)
    SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd+STR(RECNO(),7) = ;
                    'P'+&lcTmpQuery..CutTkt FOR trancd = '1'
      SCAT MEMVAR MEMO
      INSERT INTO (lcTmpSty) FROM MEMVAR
    ENDSCAN
    lcBrFields = "PO     :07   :H = 'Style Po.',"+;
                 "Vendor :10   :H = 'Vendor',"+;
                 "Style  :19   :H =  lcStyTit,"+;
                 "Price  :11   :H = 'Price',"+;
                 "TotQty :09   :H = 'TotQty'"
    SELECT (lcTmpSty)
    DIMENSION laTemp[1]
    laTemp = ''
    lcKey = &lcTmpQuery..CutTkt
    lcFile_Ttl  = 'Select Style'
    lcPbrowtitl = 'Select purchase order Line'
    SELECT (lcTmpSty)
    GOTO TOP
    llDummy=ARIABROW("lcKey",lcPbrowtitl,gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','Style','laTemp')
    SELECT (lcTmpQuery)
    REPLACE Style WITH laTemp
  CASE Type = 'C' AND !SEEK(&lcTmpQuery..CutTkt+&lcTmpQuery..Style,'CutTktL')
    CREATE TABLE &gcWorkDir.&lcTmpSty (CutTkt C(06),Style C(19),;
                                       cWareCode C(8),TotQty N(7))
    INDEX ON CutTkt+Style TAG &lcTmpSty
    SELECT CutTktL
    SET ORDER TO CutTktL
    =SEEK(&lcTmpQuery..CutTkt)
    SCAN REST WHILE cuttkt+style+dyelot+trancd = ;
                    &lcTmpQuery..CutTkt FOR trancd = '1'
      SCAT MEMVAR MEMO
      INSERT INTO (lcTmpSty) FROM MEMVAR
    ENDSCAN
    lcBrFields = "CutTkt    :10   :H = 'CutTkt#',"+;
                 "Style     :21   :H =  lcStyTit,"+;
                 "cWareCode :11   :H = 'WareHouse',"+;
                 "TotQty    :11   :H = 'TotQty'"
    SELECT (lcTmpSty)
    DIMENSION laTemp[1]
    laTemp = ''
    lcKey = &lcTmpQuery..CutTkt
    lcFile_Ttl  = 'Select Style'
    lcPbrowtitl = 'Select CutTkt Line'
    SELECT (lcTmpSty)
    GOTO TOP
    llDummy=ARIABROW("lcKey",lcPbrowtitl,gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','Style','laTemp')
    SELECT (lcTmpQuery)
    REPLACE Style WITH laTemp
ENDCASE
USE IN IIF(USED(lcTmpSty),lcTmpSty,0)
ERASE &gcWorkDir.&lcTmpSty+'.DBF'
ERASE &gcWorkDir.&lcTmpSty+'.CDX'
IF !SEEK(Style,"Style") .OR. EMPTY(Style)
  IF !SEEK(Style,"Style")
    lcStr = 'Style code not found in Style file.'
    =gfModalGen('TRM420801B00000','DIALOG',lcStr)
    SELECT (lcTmpQuery)
    REPLACE Style WITH ''
    llO = .F.
    RETURN (.T.)
  ELSE
    RETURN (.F.)
  ENDIF
ELSE
  llO = !&lcTmpQuery..llDis
  llS = .F.
  SELECT (lcTmpQuery)
  REPLACE StyScl WITH Style.Scale
ENDIF

*!*************************************************************
*! Name      : lfVOpCode
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to Valid MfgOprCode.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVOpCode()
*!*************************************************************
*!
FUNCTION lfVOpCode
PARAMETER llFrmBar
PRIVATE lcTy

*IF Type = 'M'
*  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Current record is a miscellanious transaction.')
*  
*  RETURN
*ENDIF
IF EMPTY(&lcTmpQuery..Operation)
  =lfRestLogc()
  
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Operation code is empty, cannot proceed.')
  
  RETURN
ENDIF
lcTy = IIF(Type = 'C','M','I')
IF SEEK(lcTy+&lcTmpQuery..CutTkt ,'BomLine')
  SELECT BOMLINE
  LOCATE REST WHILE cimtyp+ctktno+coprcode+ctype+style+sclr+IIF(ccatgtyp$"MDP",PADR(mfgcode,12),item)+iclr=;
                    lcTy+&lcTmpQuery..CutTkt;
              FOR   mfgcode == &lcTmpQuery..Operation
  IF !FOUND()
    CREATE TABLE &gcWorkDir.&lcTmpOp (CtktNo C(06),Style C(19),MfgCode C(06),;
                                      UnitCost N(9,2),StyQty N(11,3),ItemQty N(11,3))
    INDEX ON CtktNo+Style TAG &lcTmpOp
    SELECT BOMLINE
    =SEEK(lcTy+&lcTmpQuery..CutTkt)
    SCAN REST WHILE cimtyp+ctktno+coprcode+ctype+style+sclr+IIF(ccatgtyp$"MDP",PADR(mfgcode,12),item)+iclr=;
                    lcTy+&lcTmpQuery..CutTkt FOR cType = "1" AND Style = &lcTmpQuery..Style AND cCatgTyp = "M"
      SELECT BOMLINE
      SCAT MEMVAR MEMO
      INSERT INTO (lcTmpOp) FROM MEMVAR
    ENDSCAN
    lcHdr = IIF(&lcTmpQuery..Type = 'C','CutTkt#','Style Po#')
    lcBrFields = "CtktNo   :07 :H =  lcHdr,"+;
                 "Style    :20 :H =  lcStyTit,"+;
                 "MfgCode  :08 :H =  'Operation',"+;
                 "StyQty   :12 :H =  'StyQty',"+;
                 "ItemQty  :12 :H =  'ItemQty',"+;
                 "UnitCost :12 :H =  'Cost'"
    SELECT (lcTmpOp)
    DIMENSION laTemp[1]
    laTemp = ''
    lcKey = &lcTmpQuery..CutTkt
    lcFile_Ttl  = 'Select Operation'
    lcPbrowtitl = 'Select Operation Code'
    SELECT (lcTmpOp)
    GOTO TOP
    llDummy=ARIABROW("lcKey",lcPbrowtitl,gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','mfgCode','laTemp')
    SELECT (lcTmpQuery)
    REPLACE Operation WITH laTemp
  ENDIF
ENDIF
llO  = .F.

IF !llFrmBar
  =lfvScale()
  SELECT (lcTmpQuery)
  =SEEK('')
ELSE
  SELECT (lcTmpQuery)
  =SEEK('')
ENDIF
*--- Add If type misci. thre is no need for edit

IF EMPTY(&lcTmpQuery..CutTkt) .OR. &lcTmpQuery..Type = 'M'
  SHOW GET pbEdit DISABLE
ELSE
  IF llAdmin .OR. !SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation,'TRAKHDR')  .OR. TRAKHDR.Status='P'
    SHOW GET pbEdit ENABLE
  ELSE
    SHOW GET pbEdit DISABLE
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfIssOp
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to Check if this operation has been issued.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfIssOp()
*!*************************************************************
*!
FUNCTION lfIssOp

*--- Use this Key (cimtyp+ctktno+coprcode+clotno+trancd) to seek in MfgOprDt.
PRIVATE lnAlias
lnAlias = SELECT(0)

*--- TrakLin  && type+cuttkt+style+coprcode+status+coperseq
*--- Check for Issued sizes befor.
lcCurrScl = &lcTmpQuery..Scal
llIssued = .F.
SELECT TrakLin 

SET ORDER TO TrakLin
lcTrKey = &lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'I'
IF SEEK(lcTrKey)
  SELECT TrakLin
  llIssued = IIF(!EMPTY(lcCurrScl),(EVAL('Qty'+lcCurrScl)) <> 0,.T.)
ELSE
  IF SEEK(lcTrKey,lcTrkLin)
    SELECT(lcTrkLin)
    llIssued = IIF(!EMPTY(lcCurrScl),(EVAL('Qty'+lcCurrScl)) <> 0,.T.)
  ENDIF
ENDIF

*--- Check for Recieve

llReci = .F.
SELECT TrakLin 
SET ORDER TO TrakLin
lcTrKey = &lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'C'
IF SEEK(lcTrKey)
  SELECT TrakLin 
  llReci = IIF(!EMPTY(lcCurrScl),(EVAL('Qty'+lcCurrScl)) <> 0,.T.)
ELSE
  IF SEEK(lcTrKey,lcTrkLin)
    SELECT(lcTrkLin)
    llReci = IIF(!EMPTY(lcCurrScl),(EVAL('Qty'+lcCurrScl)) <> 0,.T.)
  ENDIF
ENDIF
lcTmpKey = &lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'P'

IF SEEK(lcTmpKey,lcTrkLin) .OR. SEEK(lcTmpKey,'TrakLin')
  llReci = .T.
ENDIF
IF llReci AND !llAdmin .AND. !(SEEK(lcTmpKey,lcTrkLin) .OR. SEEK(lcTmpKey,'TrakLin'))
 *--- TEXT    : This operation has been already Completed . Contact Your System Administrator!
 *--- BUTTONS : <OK>.
 =gfModalGen('INM38195B00000','DIALOG')
ENDIF
llUpdated = .F.
SELECT MfgOprHd     && Key == cimtyp+ctktno+coprcode  into DBF lcTktOp
GOTO TOP
lcKey = IIF(&lcTmpQuery..Type='C','M','I')+&lcTmpQuery..CutTkt+&lcTmpQuery..Operation
=SEEK(lcKey)
lnCurrOpSq = VAL(MfgOprHd.cOperSeq)
*--- Select Sql to get the previous
*--- This array will be laPreOp[1,1] hold operation code
lcTmpCut = &lcTmpQuery..CutTkt
SELECT coprCode FROM mfgoprhd WHERE CTktNo = lcTmpCut;
         AND VAL(coperseq) < lnCurrOpSq INTO ARRAY laPreOp

SELECT coprCode FROM mfgoprhd WHERE CTktNo = lcTmpCut;
         AND VAL(coperseq) > lnCurrOpSq INTO ARRAY laNxtOp

SELECT MAX(VAL(mfgoprhd.CoperSeq)) FROM mfgoprhd WHERE CTktNo = lcTmpCut;
         INTO ARRAY laseq
IF _TALLY <> 0
  SELECT coprCode FROM mfgoprhd WHERE CTktNo = lcTmpCut .AND. mfgoprhd.CoperSeq==ALLTRIM(STR(laseq,2)) ;
           INTO ARRAY laLastOp
ENDIF
=SEEK(IIF(&lcTmpQuery..Type='C','M','I')+&lcTmpQuery..CutTkt))
SCAN REST WHILE cimtyp+ctktno+coprcode = IIF(&lcTmpQuery..Type='C','M','I')+&lcTmpQuery..CutTkt;
           FOR VAL(MfgOprHd.cOperSeq) < lnCurrOpSq
  SCAT MEMVAR MEMO
  lcTrKey = IIF(cimtyp='M','C','I')+cTktNo+&lcTmpQuery..Style+coprCode+'I'
  IF !SEEK(lcTrKey,'TrakLin') .AND. !SEEK(lcTrKey,lcTrkLin)
    =gfOpenFile(gcWorkDir+lcTktOp,'','EX')
    INDEX ON cimtyp+ctktno+coprcode TAG (lcTktOp)
    SELECT (lcTktOp)
    m.cOwner = 'NI'
    INSERT INTO (lcTktOp) FROM MEMVAR
    llUpdated = .T.
    REPLACE Item WITH &lcTmpQuery..Style
  ELSE
    lcTrKey = IIF(cimtyp='M','C','I')+cTktNo+&lcTmpQuery..Style+coprCode+'C'
    IF !SEEK(lcTrKey,'TrakLin') .AND. !SEEK(lcTrKey,lcTrkLin)
      =gfOpenFile(gcWorkDir+lcTktOp,'','EX')
      INDEX ON cimtyp+ctktno+coprcode TAG (lcTktOp)
      SELECT (lcTktOp)
      m.cOwner = 'I'
      INSERT INTO (lcTktOp) FROM MEMVAR
      llUpdated = .T.
      REPLACE Item WITH &lcTmpQuery..Style
    ENDIF
  ENDIF
ENDSCAN

*C124152,1 NNA 11/09/2004 (Begin) if the person who logged isn't a Supervisor and this isn't the 
*C124152,1 NNA            first operation for this cuttkt, we'll check for previous open operations
*C124152,1 NNA            if found, a massage'll display and skipped this cuttkt from the whole
*C124152,1 NNA            process and hold this cuttkt No. and the open operation to diplay it 
*C124152,1 NNA            in a logging screen
SELECT TrakHdr

IF !llAdmin
  IF SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt)
    llFound    = .F.
    llSameUser = .T.
    llSameSty  = .T.
    SCAN FOR type+cuttkt+style+coprcode+id = &lcTmpQuery..Type+&lcTmpQuery..CutTkt 
      IF TrakHdr.Coprcode = &lcTmpQuery..Operation
        IF TrakHdr.Style = &lcTmpQuery..Style
          *-- if S_Operation - S_Style/Color - S_User
          IF ALLTRIM(lcUserId) = ALLTRIM(TrakHdr.ID)
            *-- if S_Operation - S_Style/Color - S_User - Open Operation -D_date (not allowed)
            IF STATUS = 'I' AND (Diss_Date <> gdSysDate)
              llFound = .T.
            *-- if S_Operation - S_Style/Color - S_User - Open Operation - S_date (Allowed)
            ELSE
              llFound = IIF(llFound,llFound,.F.)
            ENDIF
            llSameUser = .T.              
          *-- if S_Operation - S_Style/Color - D_User (Not Allowed)
          ELSE
            llSameUser = .F.                        
            llFound = .T.
          ENDIF
          llSameSty = .T.
        *--S_Operation - D_Style/Color         
        ELSE					
          *-- if S_Operation - D_Style or Color - S_User
          IF ALLTRIM(lcUserId) = ALLTRIM(TrakHdr.ID)
            *-- if S_Operation - D_Style/Color - S_User - Open Operation -D_date (not allowed)
            IF STATUS = 'I' AND (Diss_Date <> gdSysDate)
              llFound = .T.
             ELSE
              llFound = IIF(llFound,llFound,.F.)
            ENDIF
            llSameUser = .T.              
          *--S_Operation - D_Style/Color - D_User
          ELSE 
            llSameUser = .F.                        
            llFound = IIF(llFound,llFound,.F.)
          ENDIF
          llSameSty = .F.
        ENDIF
      *-- if Different Operation
      ELSE
        *--if D_Operation - S_Style/color
        IF TrakHdr.Style = &lcTmpQuery..Style
          *-- if D_Operation - S_Style/Color - S_User
          IF ALLTRIM(lcUserId) = ALLTRIM(TrakHdr.ID)
            *-- if S_Operation - S_Style/Color - S_User - Open Operation (not allowed)
            IF STATUS = 'I' 
              llFound = .T.
            *-- if D_Operation - S_Style/Color - S_User - Closed Operation (Allowed)
            ELSE
              llFound = IIF(llFound,llFound,.F.)
            ENDIF
            llSameUser = .T.              
          *-- if D_Operation - S_Style/Color - D_User (Not Allowed)
          ELSE
            IF STATUS = 'I' 
              llFound = .T.
            ELSE
              llFound = IIF(llFound,llFound,.F.)
            ENDIF
            llSameUser = .F.                        
          ENDIF
        *--D_Operation - D_Style/Color         
        ELSE					
          *-- if D_Operation - D_Style or Color - S_User
          IF ALLTRIM(lcUserId) = ALLTRIM(TrakHdr.ID)
            llFound = IIF(llFound,llFound,.F.)
            llSameUser = .T.              
          *--S_Operation - D_Style/Color - D_User
          ELSE 
            llSameUser = .F.                        
            llFound = IIF(llFound,llFound,.F.)
          ENDIF
          llSameSty = .F.
        ENDIF
      ENDIF
      IF llFound AND !SEEK(TrakHdr.CutTkt + TrakHdr.Coprcode , lcTmpLog)
        SELECT (lcTmpLog)
        APPEND BLANK
        REPLACE Ctktno         WITH  TrakHdr.CutTkt      ,;
                Coprcode       WITH  TrakHdr.Coprcode
      ENDIF
    ENDSCAN
    = SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt)
    *--if another open operation found or another user tried to enter to this operation.
    IF llFound 
      IF !llSameUser AND llSameSty
          =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'This Style/Color is currently processed by another user. You are not allowed to work on it.')
      ELSE
        =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'A previous operation is not closed yet , ;
                     Please check with the Traficc Process Manager.')
      ENDIF
      lnOldAlais = SELECT(0)
      SELECT(lcTmpQuery)
      DELETE
      SELECT(lnOldAlais)    
      RETURN .F.
    ENDIF
  ENDIF
ENDIF
*C124152,1 NNA (End)

IF USED(lcTktOp)
  IF llUpdated
    llUpdated = .F.
    SELECT (lcTktOp)
    GOTO TOP
    llFirst = EOF()   && .T. No prevoiuse Op.
  ELSE
    llFirst = .T.
  ENDIF
ENDIF
SELECT(lnAlias)
*-- End of Function lfIssOp

*!*************************************************************
*! Name      : lfvScale
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Scale Valid Function .
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvScale()
*!*************************************************************
*!
FUNCTION lfvScale
PARAMETER llFromBar
PRIVATE lnAlis

lnAlis = SELECT(0)

IF Type = 'M'
  RETURN
ENDIF

IF EMPTY(Operation)
  
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Operation code is empty. Cannot Proceed.')
  
  RETURN
ENDIF
*--- type+cuttkt+style+coprcode+status+coperseq
SELECT (lcTmpQuery)
IF SEEK(' '+CutTkt+Style+Operation+PADR(lcUserId,12))
  =SEEK('Z')
  DELETE
  APPEND BLANK
  REPLACE lSort WITH 'Z'
  KEYBOARD "{UPARROW}"
  IF !llFromBar
    SHOW WINDOW (lcMtBrowTt) REFRESH SAME
  ENDIF
  RETURN
ENDIF
=SEEK('Z')
*--- Function to detect if this CutTkt + ... + Operation has been issued or not.
*--- llIssued   IF .T. === Scaned before.
*--- llReci     IF .T. === Recieved i.e. Completed
*--- llFirst    IF .T. === This is the first operation i.e. not Issued not rec. and no ;
                           Issue no recev. before it in seq.
                           
llFirst = .T.
IF lfIssOp()
  SELECT (lcTmpQuery)
  DO CASE
    CASE !llFirst
      *--- There is some opration before this operation in seq.
      *--- These operation are :
      *--- 1-Issued and not received: In this case worn the user and receive
      *---   the previous operation.
      *--- 2-Not Issued Not Recieved: In this case worn the user and Issues 
      *---   and receive all previous operations.

      *--- TEXT    : Previous operation(s) has neither been issued nor Completed, 
      *---           The program will automatically issue and complete ALL previous operation(s)
      *--- BUTTONS : 
      =gfModalGen('INM38196B00000','DIALOG')
      =lfIssPreOp()
      REPLACE llF WITH .F.
      =lfColect('NF')
    CASE llFirst
      *--- Case of this operation is the the first operation
      *--- First Operation ==> '1- The smalest operation seq.'
      *---                     '2- Or all previous operaiton Received'
      *--- In this case This operation is : 1-llIssued (Scaned Before)
      *---                                  2-llReci   (Scaned and complete)
      REPLACE llF WITH .T.
      =lfColect('FR')
  ENDCASE
ENDIF
*---
*C124152,1 NNA 11/09/2004 (Begin) if the person who logged is a supervisor or there is no open
*C124152,1 NNA            operation or this is the first operation , we'll continue as usual
IF !LLFOUND .OR. llAdmin
*C124152,1 NNA (End)

  =lfEndDis(llFromBar)
ENDIF
SHOW GET pbSav ENABLE
*!*************************************************************
*! Name      : lfColect
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to collect data into temp file .
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfColect()
*!*************************************************************
*!
FUNCTION lfColect
PARAMETER lcOpType
SELECT(lcTmpQuery)

lcOldScal = Scal
DO CASE
  *--- Select the temp file you collect previos operation in and
  *--- look for Not recieved ('NR') ==> Issued but !Rec.
  *---          Not Issued   ('NF') ==> !Issued (Did not scaned before).
  CASE lcOpType = 'NF'
    *--- Look if it Scaned before or not ==> Issued ("NI").
    *--- Look If it received (Complete before).
      =lfUpOpHdDt(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,;
                  &lcTmpQuery..Operation,&lcTmpQuery..Scal,'NI')    && Update Operation header detail.
      =lfUpdTRHL(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,;
                  &lcTmpQuery..Operation,&lcTmpQuery..Scal,'NI',&lcTmpQuery..Status)     && Update Trak Files.
  CASE lcOpType = 'FR'
    *--- In this case Update the following file to Issue this operation.
    *--- 1- MfgOprHD  , 2- MfgOprDt
    IF llIssued

      lcTmpKey = &lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'P'
      IF llReci .AND. !(SEEK(lcTmpKey,lcTrkLin) .OR. SEEK(lcTmpKey,'TrakLin'))
        SELECT(lcTmpQuery)
        lcOldScal = Scal
        REPLACE Scal WITH ''
        =lfUpOpHdDt(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,;
                    &lcTmpQuery..Operation,&lcTmpQuery..Scal,'I')    && Update Operation header detail.
        =lfUpdTRHL(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,;
                   &lcTmpQuery..Operation,&lcTmpQuery..Scal,'I',"I")     && Update Trak Files.
      ELSE
        SELECT TrakHdr
        SET ORDER TO TrakHdr IN TrakHdr
        lcToSeek = &lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation
        llStart = SEEK(lcToSeek)
        IF !llStart
          SELECT(lcTrkHdr)
          llStart = SEEK(lcToSeek)
        ENDIF
        IF llStart
          
          IF (Diss_Date <> gdSysDate) .AND. Status = 'P'
            lnOldAls = SELECT(0)
            SELECT TrakHdr
            *IF nHours = 0
            *  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'This Operation has been issued on a previous date. You must first end the process on that date before the completion of this operation.')
            *  =lfCanc()
            *  SELECT(lnOldAls)
            *  RETURN
            *ELSE
            SELECT(lcTmpQuery)
            REPLACE llPending WITH .T.,;
                    Scal WITH ''
            SELECT(lnOldAls)
            =lfUpOpHdDt(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,;
                        &lcTmpQuery..Operation,&lcTmpQuery..Scal,'I')    && Update Operation header detail.
            =lfUpdTRHL(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,;
                       &lcTmpQuery..Operation,&lcTmpQuery..Scal,'I',"I")     && Update Trak Files.
            *ENDIF
          ENDIF
          IF (Diss_Date <> gdSysDate) .AND. !llAdmin
            *--- TEXT    : There are no issued or pending quantity for scanned tracking ticket on today's date ,
            *---           Please contact your system administrator . 
            *--- BUTTONS :<OK>
            *: B603772,1 SSH  Only if the status is not pending.
            *B122285,1 NNA 03/29/2004 (Begin) Prevent the message from appearing only if the C/T is complete.
            *IF Status <> 'P'
            IF Status = 'C'            
            *B122285,1 NNA (End)
              =gfModalGen('INM38198B00000','DIALOG')
              SHOW GETS DISABLE
            ELSE
              *SELECT(lcTmpQuery)
              *REPLACE  Scal WITH ''
              *=lfUpOpHdDt(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,;
              *            &lcTmpQuery..Operation,&lcTmpQuery..Scal,'I')    && Update Operation header detail.
              *=lfUpdTRHL(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,;
              *           &lcTmpQuery..Operation,&lcTmpQuery..Scal,'I',"I")     && Update Trak Files.
            ENDIF
            *: B603772,1 SSH[END]
          ELSE
            SELECT(lcTmpQuery)
            REPLACE Scal WITH ''
            IF !llPending
              =lfUpOpHdDt(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,;
                          &lcTmpQuery..Operation,&lcTmpQuery..Scal,'I')    && Update Operation header detail.

              =lfUpdTRHL(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,;
                         &lcTmpQuery..Operation,&lcTmpQuery..Scal,'I',"I")     && Update Trak Files.
              *--- Complete/Pending........
              IF TRAKHDR.Status = 'I'
                =lfToDo(gfModalGen('INM38197B38023','DIALOG'))
              ENDIF          
            ENDIF        
          ENDIF
        ENDIF
      ENDIF
    ELSE
      =lfUpOpHdDt(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,;
                  &lcTmpQuery..Operation,&lcTmpQuery..Scal,'NI')    && Update Operation header detail.
      =lfUpdTRHL(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,;
                  &lcTmpQuery..Operation,&lcTmpQuery..Scal,'NI',&lcTmpQuery..Status) && Update Trak Files.
    ENDIF
ENDCASE
SELECT (lcTrkLin)
*/--- type+cuttkt+style+coprcode+status+coperseq
=SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'I')
lnStart = &lcTrkLin..nStart
*lnIss = &lcTrkLin..Qty&lcOldScal
lnEnd = 0
IF SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'C')
  lnEnd = &lcTrkLin..nEnd
ENDIF
SELECT (lcTmpQuery)

REPLACE UserId WITH &lcTrkHdr..Id,;
        Date   WITH IIF(&lcTrkHdr..Status='C',&lcTrkHdr..dEndate,&lcTrkHdr..dIss_Date) ,;
        STime  WITH lnStart,;
        ETime  WITH lnEnd,;
        Status WITH &lcTrkHdr..Status
*-- End of Function lfColect

*!*************************************************************
*! Name      : lfUpOpHdDt
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to collect data from mfgoprdt file .
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : lcType    : Type 'C' for CutTkt 'P' for Style Po's
*!                      lcCut     : CutTkt/Po
*!                      lcSty     : Style Code
*!                      lcOper    : Mfg. Operation code
*!                      lcScal    : Scaned Scale
*!                      lcIss     : 'I' for Issued before Operation
*!                      llUpdBrow : update browse or no.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfColect()
*!*************************************************************
*!
FUNCTION lfUpOpHdDt
PARAMETER lcType,lcCut,lcSty,lcOper,lcScal,lcIss,llUpdBrow

*--- lcIss Parameter to indicate if this operation issued before('I')
*--- or this is Not Issued('NI')
PRIVATE lnAlias
lnAlias = SELECT(0)
DECLARE laMfgRFld[2,2]
STORE ''  TO laMfgRFld
lnLeadTime = 0
laMfgRFld[1,1] = 'LEADTIME'
laMfgRFld[1,2] = 'lnLeadTime'
laMfgRFld[2,1] = 'GLACCOUNT'
laMfgRFld[2,2] = 'lcMfgGlAcnt'
=gfRltFld(lcOper,@laMfgRFld,'MFGCODE')
DO CASE
  CASE  lcType = 'C'
    =SEEK(lcCut+lcSty,'CutTktL')
  CASE  lcType = 'P'
    =SEEK('P'+lcCut+lcSty,'POSLN')
ENDCASE
FOR lnInd = 1 TO 8
  lcInd1 = STR(lnInd,1)
  lnLastComp[lnInd] = IIF(lcType = 'C',EVAL('CutTktL.Qty'+lcInd1),EVAL('PoSln.Qty'+lcInd1))
ENDFOR
lnLastComp[9] = IIF(lcType = 'C',CutTktL.TotQty,PoSln.TotQty)
*---  Select Temp MFGOPRDT to update operation detail file
SELECT (lcOprDt)
lcInd = lcScal
SELECT MFGOPRDT

*B804272,1 SSH 07/12/2001 (Begin) Adding the case of PO.
IF lcType = 'C'
  =SEEK('M'+lcCut+lcOper)
  LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd  = ;
                  'M'+lcCut+lcOper FOR Item = lcSty
ELSE
  =SEEK('I'+lcCut+lcOper)
  LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd  = ;
                  'I'+lcCut+lcOper FOR Item = lcSty
ENDIF
*B804272,1 SSH 07/12/2001 (End)
IF !FOUND()
  SELECT MFGOPRDT
  SCAT MEMVAR MEMO
  SELECT (lcOprDt)
  
  *B804272,1 SSH 07/12/2001 (Begin) Adding the case of PO.
  IF lcType = 'C'
    =SEEK('M'+lcCut+lcOper,lcOprDt)
    LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd  = ;
                      'M'+lcCut+lcOper FOR Item = lcSty
  ELSE
    =SEEK('I'+lcCut+lcOper,lcOprDt)
    LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd  = ;
                      'I'+lcCut+lcOper FOR Item = lcSty
  ENDIF
  *B804272,1 SSH 07/12/2001 (End)
  
  IF !FOUND()
    SELECT (lcOprDt)
    APPEND BLANK
    GATH MEMVAR MEMO
    FOR lnInd = 1 TO 8
      lcInd1 = STR(lnInd,1)
      REPLACE   nLotQty&lcInd1 WITH 0
    ENDFOR
    REPLACE nLotTotQty  WITH 0
  ENDIF
ELSE
  SCAT MEMVAR MEMO
  SELECT (lcOprDt)
  
  *B804272,1 SSH 07/12/2001 (Begin) Adding the case of PO.
  IF lcType = 'C'
    =SEEK('M'+lcCut+lcOper,lcOprDt)
    LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd  = ;
                      'M'+lcCut+lcOper FOR Item = lcSty
  ELSE
    =SEEK('I'+lcCut+lcOper,lcOprDt)
    LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd  = ;
                      'I'+lcCut+lcOper FOR Item = lcSty
  ENDIF
  *B804272,1 SSH 07/12/2001 (End)
  
  IF !FOUND()
    INSERT INTO (lcOprDt) FROM MEMVAR
  ENDIF
ENDIF
SELECT (lcOprDt)
IF lcIss = 'NI'
  REPLACE cImTyp    WITH IIF(lcType = 'C',"M","I") ,;
          cTktNo    WITH lcCut  ,;
          cOprCode  WITH lcOper ,;
          Item      WITH lcSty  ,;
          lInHouse  WITH .T.    ,;
          TranCd    WITH '1'    ,;
          dTranDate WITH gdSysDate,;
          DueDate   WITH gdSysDate+lnLeadTime
  IF !EMPTY(laPreOp[1]) OR (!llUpdBrow AND !EMPTY(lcScal))
    lnOld = SELECT(0)
    lcGetThis = lfGetLstIs()  && Function to return the last Completed operation.
    SELECT(lnOld)
    IF EMPTY(laPreOp[1])
      REPLACE   nLotQty&lcInd WITH lnLastComp[VAL(lcInd)],;
                nLotTotQty    WITH nLotTotQty + EVAL('nLotQty'+lcInd)
    ELSE
      IF EMPTY(lcScal)
        FOR lnInd = 1 TO 8
          lcInd1 = STR(lnInd,1)
          REPLACE   nLotQty&lcInd1 WITH lnLastComp[lnInd]
        ENDFOR
        REPLACE nLotTotQty  WITH nLotTotQty + lnLastComp[9]
      ELSE
        *: B803342,1 SSH Get the correct Issued Qty 
        *REPLACE   nLotQty&lcInd WITH lnLastComp[VAL(lcScal)],;
                   nLotTotQty    WITH nLotTotQty + EVAL('nLotQty'+lcScal)
        REPLACE   nLotQty&lcInd WITH lnLastComp[VAL(lcScal)],;
                  nLotTotQty    WITH nLotTotQty + IIF(nLotTotQty<>0,(nLotTotQty-EVAL('nLotQty'+lcScal)),EVAL('nLotQty'+lcScal))

        *: B803342,1 SSH[END]
      ENDIF    
    ENDIF
  ELSE
    IF EMPTY(lcScal)
      FOR lnInd = 1 TO 8 
         lcInd1 = STR(lnInd,1)
         REPLACE  nLotQty&lcInd1 WITH lnLastComp[lnInd]
      ENDFOR
      REPLACE nLotTotQty WITH nLotTotQty + lnLastComp[9]
    ELSE
      REPLACE   nLotQty&lcInd WITH lnLastComp[VAL(lcScal)],;
                nLotTotQty    WITH nLotTotQty + EVAL('nLotQty'+lcScal)
    ENDIF
  ENDIF
ENDIF
IF !llUpdBrow
  SELECT (lcTmpQuery)
  IF EMPTY(Scal)
    FOR lnInd = 1 TO 8 
       lcInd1 = STR(lnInd,1)
       REPLACE  Sz&lcInd1 WITH IIF(EVAL(lcOprDt+'.nLotQty'+lcInd1)<>0,;
                                   EVAL(lcOprDt+'.nLotQty'+lcInd1),;
                                   lnLastComp[lnInd])
    ENDFOR
    REPLACE TotQty WITH TotQty + IIF(&lcOprDt..nLotTotQty<>0,;
                                     &lcOprDt..nLotTotQty,;
                                     lnLastComp[9])
  ELSE
    REPLACE Sz&lcInd WITH lnLastComp[VAL(lcInd)],;
            TotQty   WITH TotQty + lnLastComp[VAL(lcInd)],;
            UserId   WITH lcUserId,;
            STATUS   WITH "I"
  ENDIF
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfUpBom
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to collect data from bomcost&ctktBom file .
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : lcType    : Type 'C' for CutTkt 'P' for Style Po's
*!                      lcCut     : CutTkt/Po
*!                      lcSty     : Style Code
*!                      lcOper    : Mfg. Operation code
*!                      lcScal    : Scaned Scale
*!                      lcIss     : 'I' for Issued before Operation
*!                      llUpdBrow : update browse or no.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpBom()
*!*************************************************************
*!
FUNCTION lfUpBom
PARAMETER lcType,lcCut,lcSty,lcOper,lnIssQty

*--- Parameter to indicate if this operation issued before('I')
*--- or this is Not Issued('NI')
DO CASE
  CASE  lcType $ 'MC'
    =SEEK(lcCut+lcSty,'CutTktL')
    lcWareCode = CutTktL.CwareCode
  CASE  lcType  = 'P'
    =SEEK('P'+lcCut+lcSty,'POSLN')
    lcWareCode = POSLN.CwareCode
ENDCASE

*---- Update CtktBom
*--- Use this index in CtktBom  cimtyp+cuttkt+item+iclr+mfgcode+dyelot

SELECT CtktBom

*B804272,1 SSH 07/12/2001 (Begin) Adding the case of PO.
*IF SEEK('M'+lcCut+SPACE(19)+SPACE(6)+lcOper)
IF SEEK(IIF(lcType $ 'MC','M','I')+lcCut+SPACE(19)+SPACE(6)+lcOper)
*B804272,1 SSH 07/12/2001 (End)

  SCAT MEMVAR MEMO

  *B129938,1 NNA 12/07/2005 (Begin) Move this block from outside to inside the If statment
  m.Used_Qty  = m.Used_Qty+(lnIssQty)
  m.Issue_Qty = m.Issue_Qty+(MAX(lnIssQty,0))
  GATH MEMVAR MEMO
  *B129938,1 NNA (END)

ELSE
  m.Used_Qty  = 0
  m.Issue_Qty = 0
ENDIF
*B129938,1 NNA 12/07/2005 (Begin) Move this block to inside the If statment
*m.Used_Qty  = m.Used_Qty+(lnIssQty)
*m.Issue_Qty = m.Issue_Qty+(MAX(lnIssQty,0))
*GATH MEMVAR MEMO
*B129938,1 NNA (End)

*---- Update BomCost
SELECT BomCost
IF SEEK(CtktBom.Typ+IIF(lcType $ 'MC','M','I')+lcCut+SPACE(19)+SPACE(6)+lcOper)
  SCAT MEMVAR MEMO
  REPLACE  nTotQty   WITH nTotQty+(lnIssQty) ,;
           nTotCst   WITH nTotCst+(lnIssQty*CtktBom.untcost),;
           nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
           nTotACst  WITH nTotACst+(lnIssQty*CtktBom.untcost),;
           nUnitACst WITH IIF(nTotQty=0,0,nTotACst/nTotQty)
      *--- lnIssQty
   =lfUpCutTkt(lcType,lnIssQty*cTktBom.UntCost)   && Update CutTkt header Actual cost.
ELSE
  APPEND BLANK
  REPLACE cTktNo    WITH lcCut,;
          cWareCode WITH lcWareCode ,;
          cDyelot   WITH SPACE(08)  ,;
          Item      WITH SPACE(1)   ,;
          IClr      WITH SPACE(1)   ,;
          cBomType  WITH CtktBom.Typ,;
          cIMTyp    WITH IIF(lcType $ 'MC','M','I') ,;
          MfgCode   WITH lcOper,;
          dTranDate WITH gdSysDate,;
          cRSession WITH SPACE(6),;
          cISession WITH '',;
          cCostType WITH 'M' ,;
          nTotQty   WITH lnIssQty ,;
          nTotCst   WITH (lnIssQty*CtktBom.untcost) ,;
          nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
          nTotACst  WITH (lnIssQty*CtktBom.untcost) ,;
          nUnitACst WITH IIF(nTotQty=0,0,nTotACst/nTotQty) ,;
          cOprCode  WITH lcOper ,;
          cWipAcnt  WITH IIF(lcType $ 'MC',CutTktH.cWipAcnt,'')
   =lfUpCutTkt(lcType,lnIssQty*cTktBom.UntCost)   && Update CutTkt header Actual cost.    
ENDIF

*!*************************************************************
*! Name      : lfUpCutTkt
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to collect&Update data from CutTkt/Po file .
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : lcType    : Type 'C' for CutTkt 'P' for Style Po's
*!                      lnIssued  : Issued Qty
*!                      lnIssCost : Issued Cost
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpCutTkt()
*!*************************************************************
*!
FUNCTION lfUpCutTkt

PARAMETER lcType,lnIssued
PRIVATE lnals
lnals = SELECT(0)
lcItemType = CtktBom.Typ
*---ssh
DO CASE
  CASE lcType $ 'MC'
    SELECT CUTTKTH
    =SEEK(CtktBom.CutTkt)
    SCAT MEMVAR MEMO
    SELECT CutTktH
    REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType+lnIssued
  CASE lcType = 'P'
    SELECT POSHDR
    =SEEK('P'+CtktBom.CutTkt)
    SCAT MEMVAR MEMO
    SELECT POSHDR
    REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType+lnIssued
ENDCASE
SELECT(lnals)

*!*************************************************************
*! Name      : lfUpdTRHL
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to collect data from TrakHdr/TrakLin file .
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : lcType    : Type 'C' for CutTkt 'P' for Style Po's
*!                      lcCut     : CutTkt/Po
*!                      lcSty     : Style Code
*!                      lcOper    : Mfg. Operation code
*!                      lcScal    : Scaned Scale
*!                      lcIss     : 'I' for Issued before Operation
*!                      llStat    : "I" Issued OR "P" Pending OR "M" Miscl.
*!                      llUpdBrow : update browse or no.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpdTRHL()
*!*************************************************************
*!
FUNCTION lfUpdTRHL
PARAMETER lcType,lcCut,lcSty,lcOper,lcScal,lcIss,llStat,llUpdBrow

*--- Parameter to indicate if this operation issued before('I')
*--- or this is Not Issued('NI')

PRIVATE lnAls

*---Type +CutTkt+Style+Operation

lnAls = SELECT(0)
IF !EMPTY(lcScal)
  SELECT (lcOprDt)
  lnIss = nlotQty&lcScal
ELSE
  lnIss = &lcOprDt..nLotTotQty
ENDIF


llUpd = .T.
SELECT TrakHdr
SET ORDER TO TrakHdr
*: B603835,1 SSH 28/08/2000 Update the new start end time file.
llDummy = !SEEK(lcType+lcCut+lcSty+lcOper,'TrakHdr') .AND. lfIssTime("I",lnIss,0,0,lcType , lcCut , lcOper , lcSty)
*: B603835,1 SSH 

IF !SEEK(lcType+lcCut+lcSty+lcOper,lcTrkHdr)
  IF SEEK(lcType+lcCut+lcSty+lcOper,'TrakHdr')
    SELECT TrakHdr
    SCAT MEMVAR MEMO
    llUpd = .F.
  ENDIF
  SELECT(lcTrkHdr)
  APPEND BLANK
  GATH MEMVAR MEMO
ENDIF
SELECT(lcTrkHdr)
IF lcIss = 'NI'
*: B803342,1 SSH Replace all User Id with the same No. of '*'
*  REPLACE Type      WITH lcType,;
          CutTkt    WITH lcCut,;
          cOprCode  WITH lcOper,;
          Style     WITH lcSty,;
          STATUS    WITH "I",;
          Id        WITH IIF(llStat='WW','***',lcUserId) ,;
          nHours    WITH 0                     ,;
          Issue_Qty WITH Issue_Qty+lnIss       ,;
          NQty_Pen  WITH 0                     ,;
          nQty_Comp WITH 0                     ,;
          DIss_Date WITH gdSysDate             ,;
          DEnDate   WITH gdSysDate             ,;
          Flag      WITH 'N'

  REPLACE Type      WITH lcType,;
          CutTkt    WITH lcCut,;
          cOprCode  WITH lcOper,;
          Style     WITH lcSty,;
          Id        WITH IIF(llStat='WW','************',lcUserId) ,;
          nHours    WITH 0                     ,;
          Issue_Qty WITH IIF(llStat='WW' .AND. !EMPTY(STATUS),Issue_Qty,Issue_Qty+lnIss) ,;
          STATUS    WITH IIF(llStat='WW' .AND. !EMPTY(STATUS),STATUS,"I"),;
          NQty_Pen  WITH 0                     ,;
          nQty_Comp WITH 0                     ,;
          DIss_Date WITH gdSysDate             ,;
          DEnDate   WITH gdSysDate             ,;
          Flag      WITH 'N'

*: B803342,1 SSH[END]
ELSE
  GATH MEMVAR MEMO
  IF llUpd
    REPLACE Issue_Qty WITH Issue_Qty+(Issue_Qty - lnIss),;
            cOprCode  WITH lcOper,;
            Id        WITH IIF(llStat='WW','************',lcUserId)
  ENDIF
ENDIF

*---SELECT(lcTrkLin)   && KEY  type+cuttkt+style+coprcode+coperseq
*--- type+cuttkt+style+coprcode+scale+status
llUpd = .T.
SELECT TrakLin
SET ORDER TO TrakLin
IF !SEEK(lcType+lcCut+lcSty+lcOper,lcTrkLin)
  IF SEEK(lcType+lcCut+lcSty+lcOper,'TrakLin')
    SELECT TrakLin
    SCAN REST WHILE type+cuttkt+style+coprcode+scale+status = ;
                    lcType+lcCut+lcSty+lcOper  &&+llStat
      SELECT TrakLin
      SCAT MEMVAR MEMO
      llUpd = (&lcTmpQuery..Scal > SCALE)
      SELECT(lcTrkLin)
      APPEND BLANK
      GATH MEMVAR MEMO
      IF llStat='WW' .AND. Status = 'P'
        FOR lnInd = 1 TO 8
          lcInd = STR(lnInd,1)
          REPLACE Qty&lcInd WITH 0
        ENDFOR
        REPLACE TotQty WITH 0
      ENDIF
    ENDSCAN  
  ELSE
    SELECT(lcTrkLin)
    APPEND BLANK
  ENDIF    
ENDIF
SELECT(lcTrkLin)
=SEEK(IIF(lcType='C','M','I')+lcCut+lcOper,'mfgoprhd')
IF lcIss = 'NI'
*: B803342,1 SSH replace all user id with the same no of *
*  REPLACE Type       WITH lcType,;
          CutTkt     WITH lcCut,;
          cOprCode   WITH lcOper,;
          cOperSeq   WITH mfgoprhd.cOperSeq,;
          Status     WITH 'I'   ,;
          Style      WITH lcSty,;
          Scale      WITH lcScal,;
          Id         WITH IIF(llStat='WW','************',lcUserId) ,;
          Date       WITH gdSysDate,;
          nStart     WITH IIF(nStart=0,VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2)),nStart)

  REPLACE Type       WITH lcType,;
          CutTkt     WITH lcCut,;
          cOprCode   WITH lcOper,;
          cOperSeq   WITH mfgoprhd.cOperSeq,;
          Status     WITH IIF(llStat='WW' .AND. !EMPTY(STATUS),STATUS,"I")   ,;
          Style      WITH lcSty,;
          Scale      WITH lcScal,;
          Id         WITH IIF(llStat='WW','************',lcUserId) ,;
          Date       WITH gdSysDate,;
          nStart     WITH IIF(nStart=0,VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2)),nStart)

*: B803342,1 SSH
ELSE
  GATH MEMVAR MEMO
ENDIF
IF llUpd
  IF EMPTY(lcScal)
    FOR lnInd = 1 TO 8
      lcInd = STR(lnInd,1)
      REPLACE Qty&lcInd WITH EVAL(lcOprDt+'.nLotQty'+lcInd)
    ENDFOR
      REPLACE TotQty WITH lnIss
  ELSE
    REPLACE Qty&lcScal WITH lnIss     ,;
            TotQty     WITH TotQty+lnIss
  ENDIF
  REPLACE Id  WITH IIF(llStat='WW','************',lcUserId) ,;
          cOprCode  WITH lcOper
ENDIF
SELECT (lnAls)

*!*************************************************************
*! Name      : lfDisable
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to Disable butt .
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDisable()
*!*************************************************************
*!
FUNCTION lfDisable

*SHOW GET pbSav  DISABLE
*SHOW GET pbEdit DISABLE
*SHOW GET pbRem  DISABLE


*!*************************************************************
*! Name      : lfRestLogc
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to Reset log in variable.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfRestLogc()
*!*************************************************************
*!
FUNCTION lfRestLogc

STORE .T. TO llC,llWrongPass,llIssued,llFirst
STORE .F. TO llS,llO,llReci

*!*************************************************************
*! Name      : lfvSav
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid Function to save butt.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSav()
*!*************************************************************
*!
FUNCTION lfvSav

*--- This function will update our standard file from the created temp ones.
*--- Files to be updated 1- lcOprDt  == Operation detail file
*---                     4- lcTrkHdr == TrakHdr
*---                     5- lcTrkLin == TrakLin
*---                     8- GlDist Under condition that laSetups[2,1]='Y'

SELECT (lcTmpQuery)
GOTO TOP
SCAN FOR !EMPTY(CutTkt) .OR. Type = 'M'
  DO CASE
    CASE  Type = 'C'
      lcType = 'M'
    CASE  Type = 'P'
      lcType = 'I'
    CASE  Type = 'M'
      IF USED(lcTrkHdr)
        SELECT (lcTrkHdr)
        GOTO TOP
        *: B803342,1 SSH  Add the condition as if statement 
        *: B803342,1 SSH  to delete the record of empty cuttkt
        *SCAN FOR !EMPTY(CutTkt)
        SCAN
          IF EMPTY(CutTkt)
          *: B803342,1 SSH  [END]
            lcTOprId = coprcode+id
            SELECT TrakHdr
            SET ORDER TO TrakHdr
            *--- type+cuttkt+style+coprcode+id
            =SEEK('M'+'******'+SPACE(19)+lcTOprId)
            LOCATE REST WHILE type+cuttkt+style+coprcode+id =;
                        'M'+'******'+SPACE(19)+lcTOprId
            IF FOUND()
              DELETE
            ENDIF
          ELSE
          *: B803342,1 SSH  [End]
            SCAT MEMVAR MEMO
           
           *C124152,1 NNA 11/09/2004 (BEGIN) Accumulate actual hours for this operation
           M.NHOURS = IIF(!llAdmin , lnUserHrs+TrakHdr.NHOURS ,M.NHOURS)
           *C124152,1 NNA (END)
           
            INSERT INTO TrakHdr FROM MEMVAR
          *: B803342,1 SSH  [Start]
          ENDIF
          *: B803342,1 SSH  [END]
        ENDSCAN
      
      ENDIF    
      *: B803342,1 SSH Commented out
      *SELECT TrakHdr
      *SET ORDER TO TrakHdr
      **--- type+cuttkt+style+coprcode+id
      *=SEEK('M'+SPACE(06)+SPACE(19))
      *DELETE REST WHILE type+cuttkt+style+coprcode+id =;
      *                  'M'+SPACE(06)+SPACE(19)
      *: B803342,1 SSH  [END]
      SELECT (lcTmpQuery)
      LOOP
  ENDCASE
 
  *1*--- Update Operaion Detail mfgoprdt From lcOprDt ,'mfgoprdt'
  SELECT (lcOprDt)
  GOTO TOP
  SCAN
    SCAT MEMVAR MEMO
    *--- cimtyp+ctktno+coprcode+clotno+trancd
    IF !(&lcOprDt..cOprCode = laLastOp AND &lcOprDt..TranCd = '2')
      IF !SEEK(&lcOprDt..cimtyp+&lcOprDt..ctktNo+&lcOprDt..cOprCode+'01'+&lcOprDt..TranCd,'mfgoprdt')
        INSERT INTO mfgoprdt FROM MEMVAR
      ELSE
        SELECT mfgoprdt
        LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
                          &lcOprDt..cimtyp+&lcOprDt..ctktNo+&lcOprDt..cOprCode+'01'+&lcOprDt..TranCd;
                    FOR   Item = m.Item
        IF !FOUND()
          INSERT INTO mfgoprdt FROM MEMVAR
        ELSE
          SELECT mfgoprdt
          GATH MEMVAR MEMO
        ENDIF
      ENDIF  
      SELECT mfgoprdt
      REPLACE cLotNo WITH '01'
    ENDIF  
  ENDSCAN
  *2*--- Update lcTrkHdr From TrakHdr
  SELECT(lcTrkHdr)  &&type+cuttkt+style+coprcode+id
  GOTO TOP
  *--- type+cuttkt+style+coprcode+id
  SCAN
    SCAT MEMVAR MEMO
    SELECT TrakHdr
    SET ORDER TO TrakHdr IN TrakHdr
    IF !SEEK(&lcTrkHdr..type+&lcTrkHdr..cuttkt+&lcTrkHdr..style+&lcTrkHdr..coprcode)
      APPEND BLANK
    ENDIF

    *C124152,1 NNA 11/09/2004 (BEGIN) Accumulate actual hours for this operation
    M.NHOURS = IIF(!llAdmin , lnUserHrs+TrakHdr.NHOURS , M.NHOURS)
    lnUserHrs = 0
    *C124152,1 NNA (END)
    GATH MEMVAR MEMO    
  ENDSCAN
*3*---lcTrkLin
  SELECT(lcTrkLin)  &&type+cuttkt+style+coprcode+Status
  *---  &lcTrkLin..type+&lcTrkLin..cuttkt+&lcTrkLin..style+&lcTrkLin..coprcode+&lcTrkLin..Status+&lcTrkLin..coperseq
  GOTO TOP
  SCAN
    SCAT MEMVAR MEMO
    SELECT TRAKLIN
    SET ORDER TO TrakLin
    IF !SEEK(&lcTrkLin..type+&lcTrkLin..cuttkt+&lcTrkLin..style+&lcTrkLin..coprcode+&lcTrkLin..Status)
      APPEND BLANK
    ENDIF
    GATH MEMVAR MEMO
  ENDSCAN
ENDSCAN
*: B603835,1 SSH 28/08/2000 Update the new start end time file.
=lfUpdIssTm()
*: B603835,1 SSH 

*--- This part update the gl interies in case of modify houres for completed
*--- Tracking Ticketes.
=lfDelNxt()
IF laSetups[2,2] = 'Y' .AND. USED(lcDiffGls)
  m.cWipAcnt = &lcDiffGls..GlAccount
  lcISession = gfsequence('GLSession')
  SELECT (lcDiffGls)
  REPLACE ALL GlSession WITH lcISession
  SELECT GLDIST
  APPEND FROM (gcWorkDir+lcDiffGls)
  SELECT (lcDiffGls)
  *--- lcDiffGls
  USE IN IIF(USED(lcDiffGls),lcDiffGls,0)
  ERASE &gcWorkDir.&lcDiffGls+'.DBF'
  ERASE &gcWorkDir.&lcDiffGls+'.CDX'
ENDIF
IF USED(lcCutTran)    && CASE CutTkt
  SELECT(lcCutTran)
  GOTO TOP
  SCAN
    IF SEEK(&lcCutTran..CutTkt,'CutTktH')
      SCAT MEMVAR MEMO
      SELECT CutTktH
      GATH MEMVAR MEMO
    ENDIF      
  ENDSCAN
  USE IN (lcCutTran)
  ERASE &gcWorkDir.&lcCutTran+'.DBF'
  ERASE &gcWorkDir.&lcCutTran+'.CDX'
ENDIF
IF USED(lcPOTran)     && CASE Po's
  SELECT (lcPOTran)
  GOTO TOP
  SCAN
    IF SEEK('P'+&lcPOTran..CutTkt,'POSHDR')
      SCAT MEMVAR MEMO
      SELECT POSHDR
      GATH MEMVAR MEMO
    ENDIF      
  ENDSCAN
  USE IN (lcPOTran)
  ERASE &gcWorkDir.&lcPOTran+'.DBF'
  ERASE &gcWorkDir.&lcPOTran+'.CDX'
ENDIF
IF USED(lcCTktBom)
  SELECT (lcCTktBom)
  GOTO TOP
  SCAN
    IF SEEK(cimtyp+cuttkt+item+iclr+mfgcode+dyelot,'CTktBom')
      SCAT MEMVAR MEMO
      SELECT CTktBom
      GATH MEMVAR MEMO
    ENDIF      
  ENDSCAN
  USE IN (lcCTktBom)
  ERASE &gcWorkDir.&lcCTktBom+'.DBF'
  ERASE &gcWorkDir.&lcCTktBom+'.CDX'
ENDIF
IF USED(lcBomCost)
  SELECT (lcBomCost)
  GOTO TOP
  SCAN
    SCAT MEMVAR MEMO
    IF EMPTY(cTktNo) .AND. SEEK(' '+'M'+'******'+SPACE(19)+SPACE(06)+m.mfgcode,'BomCost')
      SELECT BomCosT
      LOCATE REST WHILE cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                        ' '+'M'+'******'+SPACE(19)+SPACE(06)+m.mfgcode;
                  FOR  nTotCst = m.nTotCst
      IF FOUND()
        DELETE
      ENDIF
    ENDIF
    SELECT (lcBomCost)
    IF !EMPTY(cTktNo)
      IF SEEK(cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession,'BOMCOST')
        SCAT MEMVAR MEMO
        SELECT BOMCOST
        GATH MEMVAR MEMO
      ELSE
        SELECT BOMCOST
        APPEND BLANK
        GATH MEMVAR MEMO
      ENDIF      
    ENDIF
  ENDSCAN
  USE IN (lcBomCost)
  ERASE &gcWorkDir.&lcBomCost+'.DBF'
  ERASE &gcWorkDir.&lcBomCost+'.CDX'
ENDIF

SHOW GET pbClose,1 ENABLE PROMPT gcBmpHome+"Close2.BMP"
*SHOW GET pbEnd ENABLE
llClose = .T.
*: B803310,1 SSH check for gl link first
*IF USED(lcGlDTempM) AND laSetups[2,2]='Y'
IF laSetups[2,2]='Y' .AND. USED(lcGlDTempM)
*: B803310,1 SSH 
  SELECT GLDIST
  APPEND FROM (gcWorkDir+lcGlDTempM)
  SELECT (lcGlDTempM)
  ZAP
ENDIF
*: B803310,1 SSH check for gl link first
*IF USED(lcGlDTempM) .AND. laSetups[2,2]='Y'
IF laSetups[2,2]='Y' .AND. USED(lcGlDTempM)
*: B803310,1 SSH 
  SELECT GLDIST
  APPEND FROM (gcWorkDir+lcGlDTempM)
  SELECT (lcGlDTempM)
  *--- lcGlDTempM
  USE IN IIF(USED(lcGlDTempM),lcGlDTempM,0)
  ERASE &gcWorkDir.&lcGlDTempM+'.DBF'
  ERASE &gcWorkDir.&lcGlDTempM+'.CDX'
ENDIF
USE IN IIF(USED('GLDIST'),'GLDIST',0)
SELECT (lcTmpQuery)
DELETE ALL
PACK
APPEND BLANK
REPLACE lSort WITH 'Z'

*--- Use this file only as indicater that there is transaction
*--- scaned. and these trans. are not misc. .
IF USED(lcOprDt)
  SELECT(lcOprDt)
  ZAP
  SELECT(lcTrkHdr)
  ZAP
  SELECT(lcTrkLin)
  ZAP
ENDIF
IF USED(lcTrIssTim)
  SELECT (lcTrIssTim)
  ZAP
ENDIF
SHOW GET pbSav DISABLE
SHOW GET pbEdit DISABLE
=lfActsBrow()

*!*************************************************************
*! Name      : lfVEdit
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid Function to save butt.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVEdit()
*!*************************************************************
*!
FUNCTION lfVEdit

PRIVATE lnAlis
lnAlis = SELECT(0)
llChang = .F.
llRwChang = .F.
*: B603772,1 SSH  Old Value variable
lcOldVal  = ''
*: B603772,1 SSH  
=lfPartTkt()
SELECT(lnAlis)

*!*************************************************************
*! Name      : lfvNew
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid Function to New butt.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : lnTyp : IF calld from remove butt
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNew()
*!*************************************************************
*!
FUNCTION lfvNew
PARAMETER lnTyp
*: B803342,1 SSH add new private variable
*PRIVATE lnAlis
PRIVATE lnAlis , llOkPress
llOkPress = .F.
*SHOW GET pbSav ENABLE
*: B803342,1 SSH[END]
lnAlis = SELECT(0)
lcUsrMsg2 = 'Misc. Transaction'
lnCost  = 0
lcNewRem = IIF(TYPE('lnTyp')='L','N','R')
DO gcScrDir+'MF\'+("MFRBRMIS.SPR")
*: B803342,1 SSH Enable only if the user add new misc.
IF llOkPress
  SHOW GET pbSav ENABLE
ENDIF
*: B803342,1 SSH
SELECT(lnAlis)

*!*************************************************************
*! Name      : lfvMisOp
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid Function Operation code in misc screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvMisOp()
*!*************************************************************
*!
FUNCTION lfvMisOp

=gfOpenFile(gcDataDir+'CODES','Ccode_no','SH')
SELECT CODES
*--- cdefcode+cfld_name+ccode_no
IF !SEEK('NMFGCODE'+lcOprCode)
  lcMisOp = gfTempName()
  SET ORDER TO
  SELECT DIST(cCode_No) AS mfgCode,cDiscRep FROM codes WHERE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam='NMFGCODE';
         AND !EMPTY(cDiscRep) INTO DBF(gcWorkDir+lcMisOp)
  
  INDEX ON mfgCode TAG (lcMisOp)
  lcBrFields = "mfgCode  :10 :H =  'Mfg. Code',"+;
               "cDiscRep :20 :H =  'Desc.    '"
  SELECT (lcMisOp)
  DIMENSION laTemp[1]
  laTemp = ''
  lcKey = 'mfgCode'
  lcFile_Ttl  = 'Select Operation'
  lcPbrowtitl = 'Select Operation Code'
  SELECT (lcMisOp)
  GOTO TOP
  llDummy=ARIABROW("",lcPbrowtitl,gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','mfgCode','laTemp')
  lcOprCode =  laTemp
ENDIF
IF !EMPTY(lcOprCode)
  SHOW GETS ENABLE
ENDIF

*!*************************************************************
*! Name      : lfVMisOk
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid Function for ok butt in Operation code in misc screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVMisOk()
*!*************************************************************
*!
FUNCTION lfVMisOk

lnOld = SELECT(0)
*: B803342,1 SSH Check if User id empty.
IF EMPTY(lcUserId)
  WAIT WINDOW "User ID cannot be empty."
  _CUROBJ=OBJNUM(lcUserId)
ELSE
*: B803342,1 SSH
  IF !EMPTY(lcOprCode)
    SELECT (lcTmpQuery)
    IF !SEEK('Z')
      APPEND BLANK
      REPLACE lSort WITH 'Z'
    ENDIF
    REPLACE Type      WITH 'M',;
            CutTkt    WITH "******",;
            Style     WITH '',;
            Operation WITH lcOprCode,;
            UserId    WITH lcUserId,;
            Date      WITH gdSysDate,;
            STime     WITH VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2)),;
            ETime     WITH VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2)),;
            Status    WITH 'M',;
            lSort     WITH ' '
    IF !USED(lcTrkHdr)
      SELECT TRAKHDR
      COPY STRUCTURE TO (gcWorkDir+lcTrkHdr)
      =gfOpenFile(gcWorkDir+lcTrkHdr,'','EX')
      INDEX ON type+cuttkt+style+coprcode+id TAG (lcTrkHdr)
    ENDIF
    SELECT (lcTrkHdr)
    IF !SEEK('M'+"******"+SPACE(19)+lcOprCode)
      APPEND BLANK
    ENDIF
    REPLACE Type      WITH "M",;
            CutTkt    WITH "******",;
            cOprCode  WITH lcOprCode,;
            Style     WITH SPACE(19),;
            STATUS    WITH "C",;
            Id        WITH lcUserId  ,;
            nHours    WITH 0         ,;
            Issue_Qty WITH 0         ,;
            NQty_Pen  WITH 0         ,;
            nQty_Comp WITH 0         ,;
            DIss_Date WITH gdSysDate ,;
            DEnDate   WITH gdSysDate ,;
            Flag      WITH 'N',;
            nHours    WITH lnHours,;
            Amount    WITH Amount + lnCost
    *--- ( INT(nhours)+((nhours - INT(nhours))/0.6)*
    =lfUpMisGl(lnCost)
    SELECT (lcTmpQuery)
    APPEND BLANK
    REPLACE lSort WITH 'Z'
    *: B803342,1 SSH Flag if the user press ok
    llOkPress = .T.
    *: B803342,1 SSH
  ENDIF
ENDIF
CLEAR READ
SELECT(lnOld)

*!*************************************************************
*! Name      : lfvRem
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid Function for Remove butt.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvRem()
*!*************************************************************
*!
FUNCTION lfvRem

PRIVATE lnAlis
*: B803342,1 SSH remove enabling and but it under condition
*SHOW GET pbSav ENABLE
*: B803342,1 SSH
lnAlis = SELECT(0)
=lfOpenTemp()
lcUsrMsg2 = 'Remove Misc. Transaction'

CREATE TABLE &gcWorkDir.&lcTmpMis (Type C(01),cOprCode C(06),Id C(10),;
                                   dIss_Date D,Amount N(11,2))
INDEX ON Id+cOprCode TAG &lcTmpMis
SELECT TrakHdr
SET ORDER TO TrakHdr
=SEEK('M')
SCAN REST WHILE type+cuttkt+style+coprcode+id = 'M'
  SCAT MEMVAR MEMO
  IF SEEK(m.type+m.cuttkt+m.style+m.coprcode+m.id,lcTrkHdr) .AND. ;
     EMPTY(&lcTrkHdr..cuttkt) .AND. (m.Amount = &lcTrkHdr..Amount)
  ELSE
    INSERT INTO (lcTmpMis) FROM MEMVAR
  ENDIF
ENDSCAN
IF USED(lcTrkHdr)
  SELECT (lcTrkHdr)
  =SEEK('M')
  *: B803342,1 SSH Commented out to add condition for Not empty cuttkt
  SCAN REST WHILE type+cuttkt+style+coprcode+id = 'M';
            FOR !EMPTY(cuttkt)
  *SCAN REST WHILE type+cuttkt+style+coprcode+id = 'M'
  *: B803342,1 SSH [END]
    SCAT MEMVAR MEMO
    INSERT INTO (lcTmpMis) FROM MEMVAR
  ENDSCAN
ENDIF
lcBrFields = "cOprCode  :15 :H = [Mfg. Code],"+;
             "Id        :15 :H = [User Id],"+;
             "dIss_Date :12 :H = [Trans. Date],"+;
             "Amount    :15 :H = [Amount] :P = '99999999.99'"
SELECT (lcTmpMis)
DIMENSION laTemp[1]
laTemp = ''
lcKey  = &lcTmpMis..Id
lcFile_Ttl  = 'Remove Misc.'
lcPbrowtitl = 'Remove Misc. Transaction'
SELECT (lcTmpMis)
GOTO TOP
llExit = EOF()
IF llExit
  *: B803342,1 SSH Change Message type
  *=gfModalGen('INM38204B00000','DIALOG')
  =gfModalGen('INM00000B00000','DIALOG',.F.,.F.,'No miscellaneous transactions found to remove!')
  *: B803342,1 SSH[End]
ELSE
  llDummy = ARIABROW("lcKey",lcPbrowtitl,gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','Amount','laTemp')
ENDIF
*: B803342,1 SSH dont browse unless there is some thing to browese
*IF llExit 
IF llExit .OR. !llDummy
*: B803342,1 SSH
  RETURN
ENDIF
lcOprCode = &lcTmpMis..cOprCode
laTemp = IIF(TYPE('laTemp')<>'N',0,-laTemp)
=lfUpMisGl(laTemp)

*: B803342,1 SSH Commented out to update when saving only
*SELECT TrakHdr
*SET ORDER TO TrakHdr
*=SEEK('M')
*LOCATE REST WHILE TYPE = 'M' FOR Amount = &lcTmpMis..Amount;
                              AND cOprCode = &lcTmpMis..cOprCode;
                              AND Id = &lcTmpMis..Id
*REPLACE CutTkt WITH ''
IF USED(lcTrkHdr)
  SELECT (lcTrkHdr)
  =SEEK('M')
  LOCATE REST WHILE TYPE = 'M' FOR Amount = &lcTmpMis..Amount;
                                AND cOprCode = &lcTmpMis..cOprCode;
                                AND Id = &lcTmpMis..Id
  IF !FOUND()
    SELECT TrakHdr
    SET ORDER TO TrakHdr
    =SEEK('M')
    LOCATE REST WHILE TYPE = 'M' FOR Amount = &lcTmpMis..Amount;
                              AND cOprCode = &lcTmpMis..cOprCode;
                              AND Id = &lcTmpMis..Id
    IF FOUND()
      SCAT MEMVAR MEMO
      INSERT INTO (lcTrkHdr) FROM MEMVAR
    ENDIF
  ENDIF
  SELECT(lcTrkHdr)
  REPLACE CutTkt WITH SPACE(06)
ENDIF
SELECT (lcTmpQuery)
IF !SEEK('Z')
  APPEND BLANK
  REPLACE lSort WITH 'Z'
ENDIF
REPLACE Type      WITH 'M',;
        CutTkt    WITH "******",;
        Style     WITH '',;
        Operation WITH lcOprCode,;
        UserId    WITH lcUserId,;
        Date      WITH gdSysDate,;
        STime     WITH VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2)),;
        ETime     WITH VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2)),;
        Status    WITH 'M',;
        lSort     WITH ' '
APPEND BLANK
REPLACE lSort WITH 'Z'
SHOW GET pbSav ENABLE
*: B803342,1 SSH [END]

SELECT(lnAlis)

*!*************************************************************
*! Name      : lfIssPreOp
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio to issue previous operation if exist.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfIssPreOp()
*!*************************************************************
*!
FUNCTION lfIssPreOp
PRIVATE lnAlis

lnAlis = SELECT(0)
SELECT (lcTktOp)
GOTO TOP
SCAN
  =lfUpOpHdDt(IIF(&lcTktOp..cimtyp='M','C','P'),&lcTktOp..CTktNo,&lcTktOp..Item,;
              &lcTktOp..coprCode,&lcTmpQuery..Scal,ALLTRIM(&lcTktOp..cOwner),.T.)    && Update Operation header detail.
  =lfUpdTRHL(IIF(&lcTktOp..cimtyp='M','C','P'),&lcTktOp..CTktNo,&lcTktOp..Item,;
              &lcTktOp..coprCode,&lcTmpQuery..Scal,ALLTRIM(&lcTktOp..cOwner),'WW',.T.) && Update Trak Files.
  =lfComplete()  && --- Function to Receive (PR)evious operation.
ENDSCAN

SELECT(lnAlis)

*!*************************************************************
*! Name      : lfComplete
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio to Complete operation.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfComplete()
*!*************************************************************
*!
FUNCTION lfComplete

*--- Files to be updated.
*INDEX == cimtyp+ctktno+coprcode+clotno+trancd  TAG (lcOprDt)
IF SEEK(&lcTktOp..cimtyp+&lcTktOp..CTktNo+&lcTktOp..coprCode,lcOprDt)
  SELECT (lcOprDt)
  LOCATE REST WHILE cimtyp+ctktno+coprcode = ;
                    &lcTktOp..cimtyp+&lcTktOp..CTktNo+&lcTktOp..coprCode;
                FOR Item = &lcTmpQuery..Style AND tranCd = '1'
  SCAT MEMVAR MEMO
  SELECT (lcOprDt)
  LOCATE REST WHILE cimtyp+ctktno+coprcode = ;
                    &lcTktOp..cimtyp+&lcTktOp..CTktNo+&lcTktOp..coprCode;
                FOR Item = &lcTmpQuery..Style AND tranCd = '2'
  IF !FOUND()
    INSERT INTO (lcOprDt) FROM MEMVAR
    REPLACE TranCd WITH '2'
    SELECT(lcTrkLin)
    APPEND BLANK
    *--Type+CutTkt+Operation
    =SEEK(&lcTktOp..cimtyp+&lcTktOp..CTktNo+&lcTktOp..coprCode,'mfgoprhd')
*: B803342,1 SSH replace all User id withe the same no. of '*' 
*    REPLACE Type       WITH IIF(&lcTktOp..cimtyp='M','C','P'),;
            CutTkt     WITH &lcTktOp..CTktNo,;
            cOprCode   WITH &lcTktOp..coprCode,;
            cOperSeq   WITH mfgoprhd.cOperSeq ,;
            Status     WITH 'C'   ,;
            Style      WITH &lcTmpQuery..Style,;
            Id         WITH '*******'   ,;
            Date       WITH gdSysDate,;
            nEnd       WITH IIF(nEnd=0,VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2)),nEnd)

    REPLACE Type       WITH IIF(&lcTktOp..cimtyp='M','C','P'),;
            CutTkt     WITH &lcTktOp..CTktNo,;
            cOprCode   WITH &lcTktOp..coprCode,;
            cOperSeq   WITH mfgoprhd.cOperSeq ,;
            Status     WITH 'C'   ,;
            Style      WITH &lcTmpQuery..Style,;
            Id         WITH '************'   ,;
            Date       WITH gdSysDate,;
            nEnd       WITH IIF(nEnd=0,VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2)),nEnd)

*: B803342,1 SSH
    FOR lnInd = 1 TO 8
      lcInd = STR(lnInd,1)
      REPLACE Qty&lcInd WITH EVAL(lcOprDt+'.nLotQty'+lcInd)
      REPLACE TotQty    WITH TotQty+Qty&lcInd
    ENDFOR
llDummy = lfIssTime("C",0,TotQty, 0,Type , CutTkt , cOprCode , Style)
    SELECT(lcTrkHdr)
    *--- type+cuttkt+style+coprcode+id
    =SEEK(IIF(&lcTktOp..cimtyp='M','C','P')+&lcTktOp..CTktNo+&lcTmpQuery..Style+&lcTktOp..coprCode)
    REPLACE Status    WITH 'C',;
            nHours    WITH 0  ,;
            NQty_Comp WITH &lcTrkLin..TotQty
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfvClos
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio to Quiit.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvClos()
*!*************************************************************
*!
FUNCTION lfvClos

IF llClose
  SELECT TrakHdr
  *--- type+cuttkt+style+coprcode+id
  SET ORDER TO TrakHdr IN TrakHdr
  =SEEK('M'+SPACE(06)+SPACE(19))
*: B803342,1 SSH replace all user id with the same no of '*'
*  REPLACE REST cuttkt WITH '******' WHILE type+cuttkt+style+coprcode+id =;
                                    'M'+SPACE(06)+SPACE(19)
  REPLACE REST cuttkt WITH '************' WHILE type+cuttkt+style+coprcode+id =;
                                                'M'+SPACE(06)+SPACE(19)

*C124152,1 NNA 11/09/2004 (Begin) IF there is an open opertion for the cuttkt that has scanned
*C124152,1 NNA            we'll display it in a logging screen
IF RECCOUNT(lcTmpLog)>0
 = lfCrtTxt()
 = lfViewRLog()
ENDIF
*C124152,1 NNA (End)


*: B803342,1 SSH
  =lfErase(.T.)     && Function to Erase All temp File.
  POP KEY
  RELEASE WINDOW (lcMtBrowTt)
  RELEASE PAD _Option OF _MSYSMENU
  RELEASE BAR 099 OF P01PU01
  RELEASE BAR 100 OF P01PU01
  CLEAR READ
ELSE
  IF gfModalGen('QRM00031B00006','Alert') = 2
    RETURN
  ENDIF
  =lfErase(.F.)     && Function to Erase All temp File.
  SHOW GET pbClose,1 ENABLE PROMPT gcBmpHome+"Close2.BMP"
  SHOW GET pbSav DISABLE
  llClose = .T.
ENDIF
*!*************************************************************
*! Name      : lfToDo
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio to Complete OR Partialy OR Pending OR Cancel
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfToDo()
*!*************************************************************
*!
FUNCTION lfToDo
PARAMETER  lnDo
PRIVATE lnOld
lnOld = SELECT(0)
IF lnDo = 0
  RETURN
ENDIF
 DO CASE
  CASE  lnDo = 1  &&--- Complete

    *C124152,1 NNA 11/09/2004 (BEGIN) Show the actual time screen to let user input his time
    *C124152,1 NNA             for the current operation
    lnUserHrs = 0
    DO gcScrDir+'MF\'+("MFACTTIM.SPR")
    *C124152,1 NNA (END)

    SELECT(lcTmpQuery)
    lcCurrKry = lSort+CutTkt+Style+Operation+UserId+Scal
    IF SEEK(' '+CutTkt+Style+Operation)
      LOCATE REST WHILE CutTkt+Style+Operation+UserId+Scal=;
                        ' '+CutTkt+Style+Operation;
                  FOR Status = 'I'
       REPLACE Status WITH "C"
    ENDIF
    =SEEK(lcCurrKry)
    =lfCompTkt()

  CASE  lnDo = 2  &&--- Partialy
    llChang = .F.
    llRwChang = .F.
    *: B603772,1 SSH  Old Value Variable
    lcOldVal  = ''
    *: B603772,1 SSH  [END]
    =lfPartTkt()
    SELECT(lcTmpQuery)
    =SEEK('Z'+'I')
    REPLACE Status WITH "P"
  CASE  lnDo = 3  &&--- Pending

    *C124152,1 NNA 11/09/2004 (BEGIN) Show the actual time screen to let user input his time for 
    *C124152,1 NNAthe current operation
    lnUserHrs = 0
    DO gcScrDir+'MF\'+("MFACTTIM.SPR")
    *C124152,1 NNA (END)

    =lfPend()
    SELECT(lcTmpQuery)
    REPLACE Status WITH "P"
  CASE  lnDo = 4  &&--- Cancel
    =lfCanc()
ENDCASE
SELECT(lnOld)
*!*************************************************************
*! Name      : lfCompTkt
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio to Complete Operarion
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCompTkt()
*!*************************************************************
*!
FUNCTION lfCompTkt

*--- If the user chose to complete Traking Tekets
PRIVATE lnAlis
lnAlis = SELECT(0)
SELECT (lcTrkHdr)

*C124152,1 NNA 11/09/2004 (BEGIN) Update hours in the Trakhdr file
*REPLACE Status    WITH 'C',;
        nQty_Comp WITH Issue_Qty,;
        dEnDate   WITH gdSysDate
REPLACE Status    WITH 'C',;
        nQty_Comp WITH Issue_Qty,;
        dEnDate   WITH gdSysDate,;
        nHours    WITH nHours+lnUserHrs
*C124152,1 NNA (END)

llDummy = lfIssTime("C",0,Issue_Qty, 0,&lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style)
SELECT (lcTrkLin)
SCAT MEMVAR MEMO
APPEND BLANK
GATH MEMVAR MEMO
REPLACE Status WITH "C",;
        Date   WITH gdSysDate,;
        nEnd   WITH IIF(nEnd=0, VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2)),nEnd)
*--- Start Update BomCost,cTktBom &&SSH
*--- End Update BomCost,cTktBom

*--- Start Update MfgDetail file.
lcType = IIF(Type = 'C','M','I')
IF SEEK(lcType+CutTkt+coprcode,lcOprDt)
  SELECT (lcOprDt)
  LOCATE REST WHILE cimtyp+ctktno+coprcode = ;
                    lcType+CutTkt+coprcode;
                FOR Item = &lcTmpQuery..Style AND tranCd = '1'
  SCAT MEMVAR MEMO
  SELECT (lcOprDt)
  LOCATE REST WHILE cimtyp+ctktno+coprcode = ;
                    lcType+CutTkt+coprcode;
                FOR Item = &lcTmpQuery..Style AND tranCd = '2'
  IF !FOUND()
    INSERT INTO (lcOprDt) FROM MEMVAR
    REPLACE TranCd WITH '2'
  ENDIF
ENDIF
SELECT(lnAlis)

*!*************************************************************
*! Name      : lfPartTkt
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio to Partialy Complete Operarion
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPartTkt()
*!*************************************************************
*!
FUNCTION lfPartTkt
lcScal = IIF(SEEK(&lcTmpQuery..Style,'Style'),Style.Scale,'')
IF SEEK('S'+lcScal,'Scale')
  FOR I = 1 TO 8
    Z = STR (I,1)
    lcSz&Z = Scale.Sz&Z
  ENDFOR
ENDIF
lcCutTkt  = &lcTmpQuery..CutTkt
lcStyClr  = &lcTmpQuery..Style
lcStyDesc = Style.Desc
lcUsrMsg1 = 'Modify Screen'

SELECT (lcTrkLin)
=SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'I')
lnTotIss = 0
FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  lnIss&lcInd = &lcTrkLin..Qty&lcInd
  lnTotIss = lnTotIss+lnIss&lcInd
ENDFOR
=SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'C')
lnComTot =0
FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  lnCom&lcInd = &lcTrkLin..Qty&lcInd
  lnComTot = lnComTot+lnCom&lcInd
ENDFOR

=SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'P')
lnPenTot = 0
FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  lnPen&lcInd = &lcTrkLin..Qty&lcInd
  lnPenTot = lnPenTot+lnPen&lcInd
ENDFOR
lcOprCode = &lcTmpQuery..Operation
*: B603835,1 SSH 28/08/2000 get Re-Work pieces
*--- Issue Rework
SELECT TrstrEnd
*--- type+cuttkt+mfgcode+style+status+DTOS(diss_date)
=SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Operation+&lcTmpQuery..Style+'W'+DTOS(gdSysDate))
lnWTotIss = 0
FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  lnWIss&lcInd = Qty&lcInd
  lnWTotIss = lnWTotIss+lnWIss&lcInd
ENDFOR
*--- Pending rework
=SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Operation+&lcTmpQuery..Style+'D')
LOCATE REST WHILE type+cuttkt+mfgcode+style+status+DTOS(diss_date)=;
                  &lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Operation+&lcTmpQuery..Style+'D';
            FOR   nQty_Pen <> 0
lnWPenTot = 0
FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  lnWPen&lcInd = Qty&lcInd
  lnWPenTot = lnWPenTot+lnWPen&lcInd
ENDFOR

*--- Complete Rework
=SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Operation+&lcTmpQuery..Style+'T'+DTOS(gdSysDate))
lnWComTot =0
FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  lnWCom&lcInd = Qty&lcInd
  lnWComTot = lnWComTot+lnWCom&lcInd
ENDFOR
*: B603835,1 SSH 28/08/2000 get Net completed  pieces
lnTotNet =0
FOR lnInd = 1 TO 8
  lcNInd = STR(lnInd,1)
  lnNet&lcNInd = lnCom&lcNInd - (lnWIss&lcNInd - lnWCom&lcNInd)
  lnTotNet = lnTotNet+lnNet&lcNInd
ENDFOR

*--- Function to check if the next operation Issued
DIMEN laFNxtSeq[1] , laFNxtOpr[1]
laFNxtOpr = ''
SELECT (lcTrkHdr)
=SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation)
lnHours   = &lcTrkHdr..nHours
lcUserId  = &lcTrkHdr..Id
PUSH KEY
ON KEY
ON KEY LABEL ESC
DO gcScrDir+'MF\'+("MFRBRMOD.SPR")
POP KEY
*!*************************************************************
*! Name      : lfvIss
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid Functio For Issyed Qty in modify screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvIss()
*!*************************************************************
*!
FUNCTION lfvIss
PRIVATE lcType , llFnd
*: B603772,1 SSH  Check if Variable change
IF !lfChekChng()
  RETURN
ENDIF
llChang = .T.

lcInd = SUBSTR(SYS(18),6,1)
IF EMPTY(lcInd) OR !(lcInd$"12345678")
  RETURN
ENDIF
IF EVAL(SYS(18)) <0
  WAIT WINDOW "Cannot accept negative value."
  lnIss&lcInd = &lcTrkLin..Qty&lcInd
  lcCurObj = SYS(18)
  _CUROBJ=OBJNUM(lcCurObj)
  SHOW GET lnIss&lcInd
  RETURN
ENDIF
SELECT (lcTrkLin)
=SEEK(&lcTrkHdr..Type+&lcTrkHdr..CutTkt+&lcTrkHdr..Style+&lcTrkHdr..cOprCode+'I')
IF &lcTrkLin..Type = 'C'
  =SEEK(&lcTrkLin..CutTkt+&lcTrkLin..Style,'CutTktL')
ELSE
  =SEEK('P'+&lcTrkLin..CutTkt+&lcTrkLin..Style,'PosLn')
ENDIF
IF BETWEEN(lcInd,'1','8') AND (EVAL(SYS(18)) >  IIF(&lcTrkLin..Type = 'C',CutTktL.Qty&lcInd,PosLn.Qty&lcInd))
  WAIT WINDOW "Issued quantity cannot be greater than budget quantity."
  lnIss&lcInd = &lcTrkLin..Qty&lcInd
  lcCurObj = SYS(18)
  _CUROBJ=OBJNUM(lcCurObj)
ENDIF
lnTotIss = lnIss1+lnIss2+lnIss3+lnIss4+lnIss5+lnIss6+lnIss7+lnIss8
SELECT (lcTrkLin)
SHOW GET lnTotIss

*!*************************************************************
*! Name      : lfvPen
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid Function For Pending Qty in modify screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPen()
*!*************************************************************
*!
FUNCTION lfvPen

*: B603772,1 SSH  Check if Variable change
IF !lfChekChng()
  RETURN
ENDIF
llChang = .T.
*: B603772,1 SSH[END]
lcInd = SUBSTR(SYS(18),6,1)
IF EMPTY(lcInd) OR !(lcInd$"12345678")
  RETURN
ENDIF
IF EVAL(SYS(18)) <0
  WAIT WINDOW "Cannot accept negative value."
  lnPen&lcInd = &lcTrkLin..Qty&lcInd
  lcCurObj = SYS(18)
  _CUROBJ=OBJNUM(lcCurObj)
  SHOW GET lnPen&lcInd
  RETURN
ENDIF


IF (EVAL(SYS(18)) + EVAL('lnCom'+lcInd)) > EVAL('lnIss'+lcInd)
  *--- "Pending Quantity and Completed can not Excced Issued Qty"
  =gfModalGen('INM38199B00000','DIALOG')
  lnPen&lcInd = 0
  lcCurObj = SYS(18)
  _CUROBJ=OBJNUM(lcCurObj)
ELSE
  lnPenTot = lnPen1+lnPen2+lnPen3+lnPen4+lnPen5+lnPen6+lnPen7+lnPen8
  SHOW GET lnPenTot
ENDIF
lnNet&lcInd = lnCom&lcInd - (lnWIss&lcInd - lnWCom&lcInd)
lnTotNet = 0
FOR lnInd = 1 TO 8
  lcNInd = STR(lnInd,1)
  lnTotNet = lnTotNet+lnNet&lcNInd
ENDFOR
SHOW GET lnTotNet
SHOW GET lnNet&lcInd

*!*************************************************************
*! Name      : lfvComp
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid Functio For Complete Qty in modify screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvComp()
*!*************************************************************
*!
FUNCTION lfvComp
PRIVATE lnOldAls

*: B603772,1 SSH  Check if Variable change
lnOldAls = SELECT(0)
IF !lfChekChng()
  RETURN
ENDIF
llChang = .T.
lcInd = SUBSTR(SYS(18),6,1)
IF EMPTY(lcInd) OR !(lcInd$"12345678")
  SELECT(lnOldAls)
  RETURN
ENDIF
=SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'C',lcTrkLin)
IF EVAL(SYS(18)) <0
  WAIT WINDOW "Cannot accept negative value."
  lnCom&lcInd = &lcTrkLin..Qty&lcInd
  lcCurObj = SYS(18)
  _CUROBJ=OBJNUM(lcCurObj)
  SHOW GET lnCom&lcInd
  SELECT(lnOldAls)
  RETURN
ENDIF
*: B603835,1 SSH 28/08/2000 Check if there is quantity completed at current date
*--- TrStrEnd   type+cuttkt+mfgcode+style+status+DTOS(diss_date)
IF lcOldVal > EVAL(SYS(18)) .AND. !SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+lcOprCode+&lcTmpQuery..Style + "C"+DTOS(gdSysDate),'TrStrEnd');
   .AND. !SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+lcOprCode+&lcTmpQuery..Style + "C"+DTOS(gdSysDate),lcTrIssTim)
   
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'No complete quantity found for current date, Cannot decrease completed quantity.')
  lnCom&lcInd = &lcTrkLin..Qty&lcInd
  SHOW GET lnCom&lcInd
  SELECT(lnOldAls)
  RETURN
ENDIF
*: B603835,1 SSH 28/08/2000 Commented Out.
*IF lcOldVal <> 0 .AND. EVAL(SYS(18)) = 0
*  =lfCkNxtIss(IIF(&lcTmpQuery..Type='C','M','I'),&lcTmpQuery..CutTkt,lcOprCode,&lcTmpQuery..Style,.T.)
*  SELECT (lcOprToDel)
*  SET ORDER TO lcToDel
*  IF SEEK(IIF(&lcTmpQuery..Type='C','M','I')+&lcTmpQuery..CutTkt+lcOprCode);
*    .AND. (gfModalGen('QRM38200B00023','DIALOG') = 2)
*    DELETE REST WHILE cimtyp+ctktno+cTrgOpr = ;
*                      IIF(&lcTmpQuery..Type='C','M','I')+&lcTmpQuery..CutTkt+lcOprCode
*    lnCom&lcInd = &lcTrkLin..Qty&lcInd
*    SET ORDER TO (lcOprToDel)
*    SELECT(lnOldAls)
*    RETURN
*  ENDIF
*  SET ORDER TO (lcOprToDel)
*ENDIF
*: B603835,1 SSH [END]
IF (EVAL(SYS(18)) + EVAL('lnPen'+lcInd)) > EVAL('lnIss'+lcInd)
  *--- "Pending Quantity and Completed can not Excced Issued Qty"
  =gfModalGen('INM38199B00000','DIALOG')
  lnCom&lcInd = lcOldVal
  lcCurObj = SYS(18)
  _CUROBJ=OBJNUM(lcCurObj)
ELSE
  lnComTot = lnCom1+lnCom2+lnCom3+lnCom4+lnCom5+lnCom6+lnCom7+lnCom8
  SHOW GET lnComTot
ENDIF
lnNet&lcInd = lnCom&lcInd - (lnWIss&lcInd - lnWCom&lcInd)
lnTotNet = 0
FOR lnInd = 1 TO 8
  lcNInd = STR(lnInd,1)
  lnTotNet = lnTotNet+lnNet&lcNInd
ENDFOR
SHOW GET lnTotNet
SHOW GET lnNet&lcInd
SELECT(lnOldAls)
*!*************************************************************
*! Name      : lfPend
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio For Pending Qty in modify screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfPend()
*!*************************************************************
*!
FUNCTION lfPend

*lcInd = SUBSTR(SYS(18),6,1)
*IF (EVAL(SYS(18)) + EVAL('lnCom'+lcInd)) > EVAL('lnIss'+lcInd)
*  *--- "Pending Quantity and Completed can not Excced Issued Qty"
*  =gfModalGen('INM38199B00000','DIALOG')
*  lnPen&lcInd = 0
*  lcCurObj = SYS(18)
*  _CUROBJ=OBJNUM(lcCurObj)
*ELSE
*  lnPenTot = lnPen1+lnPen2+lnPen3+lnPen4+lnPen5+lnPen6+lnPen7+lnPen8
*  SHOW GET lnPenTot
*ENDIF
*IF lnPenTot = 0
*  RETURN
*ENDIF
SELECT(lcTrkHdr)
=SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation)

*C124152,1 NNA 11/09/2004 (BEGIN) update hours in the trakhdr file
*REPLACE Status    WITH 'P',;
        nQty_Comp WITH 0,;
        nQty_Pen  WITH Issue_Qty,;
        dEnDate   WITH gdSysDate
REPLACE Status    WITH 'P',;
        nQty_Comp WITH 0,;
        nQty_Pen  WITH Issue_Qty,;
        dEnDate   WITH gdSysDate,;
        nHours    with nHours + lnUserHrs
*C124152,1 NNA (End)

SELECT(lcTrkLin)
=SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'I')
SCAT MEMVAR MEMO
IF SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'P')
  GATH MEMVAR MEMO
  REPLACE date WITH gdSysDate
ELSE
  APPEND BLANK
  GATH MEMVAR MEMO
  REPLACE date   WITH gdSysDate,;
          Status WITH 'P'
ENDIF

*!*************************************************************
*! Name      : lfCanc
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio For Cancel Qty in modify screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCanc()
*!*************************************************************
*!
FUNCTION lfCanc

SELECT (lcTmpQuery)
DELETE

*!*************************************************************
*! Name      : lfvModOk
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio For Ok butt in modify screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvModOk()
*!*************************************************************
*!
FUNCTION lfvModOk
PRIVATE lcTempKeY

*C124152,1 NNA 11/09/2004 (Begin) If this if a Complete Cuttkt Make llChang = .T. to Save any
*C124152,1 NNA            changes in the hours field
IF llAdmin AND STATUS='C' AND lnHours<>lnOldHors
  llChang = .T.
ENDIF
*C124152,1 NNA (End)

IF !llChang
  CLEAR READ
  RETURN
ENDIF

*: B803342,1 SSH Check if Id empty
IF EMPTY(lcUserId)
  WAIT WINDOW "User ID cannot be empty."
  _CUROBJ=OBJNUM(lcUserId)
  RETURN
ENDIF

*C124152,1 NNA 11/09/2004 (BEGIN) Show the actual time screen to let user input his time
*C124152,1 NNA             for the current operation

IF !llAdmin AND (lnPenTot>0 OR lnComTot>0)
  lnUserHrs = 0
  DO gcScrDir+'MF\'+("MFACTTIM.SPR")
ENDIF
*C124152,1 NNA (END)

*: B803342,1 SSH
*--- Get the The Sisez with Qty 0
lcType  = IIF(&lcTmpQuery..Type = 'C','M','I')
*--- type+cuttkt+style+coprcode+id
SELECT (lcTrkHdr)
lcTempKeY = type+cuttkt+style+coprcode+id
IF  lnComTot  = 0 AND &lcTrkHdr..nQty_Comp <>  0 
  IF !lfChkDele()
    RETURN
  ENDIF
ELSE
  llDummy = lfCkNxtIss(IIF(&lcTmpQuery..Type='C','M','I'),&lcTmpQuery..CutTkt,lcOprCode,&lcTmpQuery..Style)
  llDummy = lnComTot < &lcTrkHdr..nQty_Comp .AND. !EMPTY(laFNxtOpr) .AND. lfCheckMod()
ENDIF
=SEEK(lcTempKeY,lcTrkHdr)

*C124152,1 NNA 11/09/2004 (BEGIN) Update hours in the Trakhdr file
REPLACE nHours WITH nHours+lnUserHrs
*C124152,1 NNA (END)

llDummy = !EMPTY(laPreOp) .AND. lfGetPre(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,'laPreOp')
*--- SSH

*--- SSH
SELECT (lcTrkLin)
IF SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'I')
  FOR lnInd = 1 TO 8
    lcInd = STR(lnInd,1)
    REPLACE Qty&lcInd WITH lnIss&lcInd,;
            TotQty    WITH lnTotIss,;
            Id        WITH lcUserId
  ENDFOR
ENDIF
SELECT (lcTrkLin)
IF !SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'C')
   =SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'I')
  SCAT MEMVAR MEMO
  IF lnComTot <> 0
    APPEND BLANK
    GATH MEMVAR MEMO
    FOR lnInd = 1 TO 8
      lcInd = STR(lnInd,1)
      REPLACE Qty&lcInd WITH lnCom&lcInd,;
              TOTQTY    WITH lnComTot,;
              Status    WITH 'C',;
              nEnd      WITH IIF(nEnd = 0 ,VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2)) ,nEnd),;
              Id        WITH lcUserId
    ENDFOR
  ENDIF
ELSE
  FOR lnInd = 1 TO 8
    lcInd = STR(lnInd,1)
    REPLACE Qty&lcInd WITH lnCom&lcInd,;
            TOTQTY    WITH lnComTot,;
            Status    WITH 'C',;
            nEnd      WITH IIF(nEnd = 0 ,VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2)) ,nEnd),;
            Id        WITH lcUserId
    SELECT (lcTmpQuery)
    REPLACE Sz&lcInd WITH lnCom&lcInd
    SELECT (lcTrkLin)
  ENDFOR
ENDIF
lcType = IIF(&lcTmpQuery..Type = 'C','M','I')

IF SEEK(lcType+&lcTmpQuery..CutTkt+&lcTmpQuery..Operation,lcOprDt)
  SELECT (lcOprDt)
  LOCATE REST WHILE cimtyp+ctktno+coprcode = ;
                    lcType+&lcTmpQuery..CutTkt+&lcTmpQuery..Operation;
                FOR Item = &lcTmpQuery..Style AND tranCd = '1'
  FOR lnInd = 1 TO 8
    lcInd = STR(lnInd,1)
    REPLACE nLotQty&lcInd WITH lnIss&lcInd,;
            nLotTotQty    WITH lnTotIss
  ENDFOR
  SCAT MEMVAR MEMO
  SELECT (lcOprDt)
  LOCATE REST WHILE cimtyp+ctktno+coprcode = ;
                    lcType+&lcTmpQuery..CutTkt+&lcTmpQuery..Operation;
                FOR Item = &lcTmpQuery..Style AND tranCd = '2'
  
  DO CASE
    CASE FOUND()
      FOR lnInd = 1 TO 8
        lcInd = STR(lnInd,1)
        IF lnComTot = 0
          REPLACE nLotQty&lcInd WITH 0,;
                  nLotTotQty    WITH 0
        ELSE
          REPLACE nLotQty&lcInd WITH lnCom&lcInd,;
                  nLotTotQty    WITH lnComTot
        ENDIF
      ENDFOR
    CASE !FOUND() 
      INSERT INTO (lcOprDt) FROM MEMVAR
      FOR lnInd = 1 TO 8
        lcInd = STR(lnInd,1)
        IF lnComTot <> 0
          REPLACE nLotQty&lcInd WITH lnCom&lcInd,;
                  nLotTotQty    WITH lnComTot
        ELSE
          REPLACE nLotQty&lcInd WITH 0,;
                  nLotTotQty    WITH 0
        ENDIF
      ENDFOR
  ENDCASE 
  REPLACE TranCd WITH '2'
ENDIF
SELECT (lcTrkLin)
IF !SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'P')
   =SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+'I')
  SCAT MEMVAR MEMO
  IF lnPenTot <> 0 
    APPEND BLANK
    GATH MEMVAR MEMO
    FOR lnInd = 1 TO 8
      lcInd = STR(lnInd,1)
      REPLACE Qty&lcInd WITH lnPen&lcInd,;
              TOTQTY    WITH lnPenTot,;
              Status    WITH 'P'
    ENDFOR
  ENDIF
ELSE
  IF &lcTmpQuery..llPending .AND. TOTQTY > lnPenTot
    llDummy = lfIssTime("P",(TOTQTY-lnPenTot),0 ,0, &lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style)  
  ENDIF
  FOR lnInd = 1 TO 8
    lcInd = STR(lnInd,1)
    REPLACE Qty&lcInd WITH lnPen&lcInd,;
            TOTQTY    WITH lnPenTot,;
            Status    WITH 'P'
  ENDFOR
ENDIF
=lfvRework()

lcNewStat = IIF(lnPenTot <> 0,'P',IIF(lnComTot<>0,'C','I'))
SELECT (lcTmpQuery)
REPLACE Status    WITH lcNewStat
SELECT(lcTrkHdr)
=SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation)
*: B803342,1 SSH replace status with new condition
*REPLACE Status   WITH IIF(lnComTot=Issue_Qty OR lnPenTot = 0 ,'C','P'),;
        dEnDate   WITH gdSysDate,;
        nQty_Comp WITH lnComTot,;
        nQty_Pen  WITH lnPenTot,;
        Issue_Qty WITH Issue_Qty+(lnTotIss-Issue_Qty),;
        nHours    WITH lnHours,;
        Id        WITH lcUserId
*: B603835,1 SSH Update the new start end time file.
llDummy = (lnTotIss-Issue_Qty)  <> 0 .AND. lfIssTime("I",(lnTotIss-Issue_Qty),0 ,0, &lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style)
llDummy = (lnComTot- nQty_Comp) <> 0 .AND. lfIssTime("C",0,lnComTot- nQty_Comp,0, &lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style)
*: B603835,1 SSH [END]
REPLACE Status    WITH lcNewStat,;
        dEnDate   WITH gdSysDate,;
        nQty_Comp WITH lnComTot,;
        nQty_Pen  WITH lnPenTot,;
        Issue_Qty WITH Issue_Qty+(lnTotIss-Issue_Qty),;
        nHours    WITH lnHours,;
        Id        WITH lcUserId
SELECT (lcTmpQuery)
FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  REPLACE sz&lcInd WITH lnIss&lcInd,;
          TOTQTY    WITH lnTotIss
ENDFOR

*: B803342,1 SSH
=lfvHours()
CLEAR READ

*!*************************************************************
*! Name      : lfErase
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio For Erase temp files.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : llErase to erase browse or no.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfErase()
*!*************************************************************
*!
FUNCTION lfErase
PARAMETER llErase

USE IN IIF(USED(lcTmpSty),lcTmpSty,0)
ERASE &gcWorkDir.&lcTmpSty+'.DBF'
ERASE &gcWorkDir.&lcTmpSty+'.CDX'

USE IN IIF(USED(lcCutTran),lcCutTran,0)
ERASE &gcWorkDir.&lcCutTran+'.DBF'
ERASE &gcWorkDir.&lcCutTran+'.CDX'

USE IN IIF(USED(lcPOTran),lcPOTran,0)
ERASE &gcWorkDir.&lcPOTran+'.DBF'
ERASE &gcWorkDir.&lcPOTran+'.CDX'

IF llErase
  USE IN IIF(USED(lcTrIssTim),lcTrIssTim,0)
  ERASE &gcWorkDir.&lcTrIssTim+'.DBF'
  ERASE &gcWorkDir.&lcTrIssTim+'.CDX'
ELSE
  IF USED(lcTrIssTim)
    SELECT(lcTrIssTim)
    ZAP
  ENDIF
ENDIF
*--- lcTmpQuery
IF llErase
  USE IN IIF(USED(lcTmpQuery),lcTmpQuery,0)
  ERASE &gcWorkDir.&lcTmpQuery+'.DBF'
  ERASE &gcWorkDir.&lcTmpQuery+'.CDX'
ELSE
  lnAlis = SELECT(0)
  SELECT (lcTmpQuery)
  ZAP
  APPEND BLANK
  REPLACE lSort WITH 'Z'
  =lfActsBrow()
  KEYBOARD "{ALT+B}"
  =lfDisable()
  SELECT(lnAlis)
ENDIF
*--- lcTmpOp
USE IN IIF(USED(lcTmpOp),lcTmpOp,0)
ERASE &gcWorkDir.&lcTmpOp+'.DBF'
ERASE &gcWorkDir.&lcTmpOp+'.CDX'
*--- lcTktOp
IF llErase
  USE IN IIF(USED(lcTktOp),lcTktOp,0)
  ERASE &gcWorkDir.&lcTktOp+'.DBF'
  ERASE &gcWorkDir.&lcTktOp+'.CDX'
ELSE
  IF USED(lcTktOp)
    SELECT(lcTktOp)
    ZAP
  ENDIF
ENDIF
*--- lcOprToDel
IF llErase
  USE IN IIF(USED(lcOprToDel),lcOprToDel,0)
  ERASE &gcWorkDir.&lcOprToDel+'.DBF'
  ERASE &gcWorkDir.&lcOprToDel+'.CDX'
ELSE
  IF USED(lcOprToDel)
    SELECT(lcOprToDel)
    ZAP
  ENDIF
ENDIF
*--- lcOprDt
USE IN IIF(USED(lcOprDt),lcOprDt,0)
ERASE &gcWorkDir.&lcOprDt+'.DBF'
ERASE &gcWorkDir.&lcOprDt+'.CDX'
*--- lcTrkHdr
USE IN IIF(USED(lcTrkHdr),lcTrkHdr,0)
ERASE &gcWorkDir.&lcTrkHdr+'.DBF'
ERASE &gcWorkDir.&lcTrkHdr+'.CDX'
*--- lcTrkLin
USE IN IIF(USED(lcTrkLin),lcTrkLin,0)
ERASE &gcWorkDir.&lcTrkLin+'.DBF'
ERASE &gcWorkDir.&lcTrkLin+'.CDX'
*--- lcGlDTemp
USE IN IIF(USED(lcGlDTemp),lcGlDTemp,0)
ERASE &gcWorkDir.&lcGlDTemp+'.DBF'
ERASE &gcWorkDir.&lcGlDTemp+'.CDX'

*!*************************************************************
*! Name      : lfCalcTim
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio To Calculate time.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCalcTim()
*!*************************************************************
*!
*: B603835,1 SSH  Rename to rewrite 
FUNCTION RENlfCalcTim

PRIVATE lnAlias,lcCurrId

lnAlias = SELECT(0)
llBomByMe = gfOpenFile(gcDataDir+'bom','bom','SH')
SELECT TrakHdr
*: B603772,1 SSH  Prevent end date unless all previus date is ended
LOCATE FOR DTOS(dIss_date) < DTOS(gdSysDate) AND nHours = 0.00
IF FOUND()
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'You have not ended the process on a previous date .You cannot end process today until you end the process of the previous date.')
  RETURN
ENDIF
*: B603772,1 SSH  [END]
LOCATE FOR DTOS(dIss_date) = DTOS(gdSysDate)
IF !FOUND()
  WAIT WINDOW "No records found for current date. Cannot proceed."
  USE IN IIF(llBomByMe,'Bom',0)
  SELECT(lnAlias)
  RETURN(.F.)
ENDIF
LOCATE REST FOR DTOS(dIss_date) = DTOS(gdSysDate) .AND. nHours <> 0.00
IF FOUND()
  WAIT WINDOW "Transactions for current date are already closed. Cannot proceed."
  USE IN IIF(llBomByMe,'Bom',0)
  SELECT(lnAlias)
  RETURN(.F.)
ENDIF
SELECT TrakLin
lnMajLen  = LEN(gfItemMask("PM"))
SET ORDER TO TrakId
GOTO TOP
lnTotTime = 0
lnEnd     = 0
lnStart   = 0
lnQtyEst  = 0
lcTotComp = 0
lnStart   = 99.99

*--- cimtyp+ctktno+coprcode+clotno+trancd
SET ORDER TO Mfgoprdt IN Mfgoprdt

SET FILTER TO Id+DTOS(date)+coprcode <> '*'  .AND. DTOS(date) = DTOS(gdSysDate)
GOTO TOP
IF EOF()
  WAIT WINDOW "No records found for current user ID / date."
ENDIF
DO WHILE !EOF()
  lcTmpOpr = coprcode
  lcCurrId = ID
  WAIT WINDOW "Calculating actual time for user "+ALLTRIM(lcCurrId)+' / operation '+lcTmpOpr NOWAIT

  IF SEEK(lcCurrId+DTOS(gdSysDate)+lcTmpOpr)
    *--- id+DTOS(date)+coprcode
    SCAN REST WHILE id+DTOS(date)+coprcode = ;
                    lcCurrId+DTOS(gdSysDate)+lcTmpOpr

      DO CASE
        CASE Status = 'I'
          lnStart   = IIF(nStart<lnStart,nStart,lnStart)
        CASE Status = 'C' 
          lcTotComp = TotQty
          SELECT BOM
          =SEEK(SUBSTR(TrakLin.Style,1,lnMajLen))
          LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=;
                            SUBSTR(TrakLin.Style,1,lnMajLen);
                      FOR   mfgCode = lcTmpOpr
          lnEstTim = IIF(FOUND(),BOM.nEstBomQty,1)
          lnQtyEst = lnQtyEst+(lnEstTim*lcTotComp)
          lnEnd    = IIF(TrakLin.nEnd>lnEnd,TrakLin.nEnd,lnEnd)
      ENDCASE
    ENDSCAN
    lnEndSec = lnEnd   - INT(lnEnd)
    lnStaSec = lnStart - INT(lnStart)
    IF lnStaSec > lnEndSec
      lnTotTime = (ABS((lnEndSec + (0.6 - lnStaSec))))*100
      lnStart   =  IIF(lnStart + 1 > 23,1,lnStart + 1)
    ELSE
     lnTotTime = (ABS(lnEndSec - lnStaSec))*100
    ENDIF
    IF INT(lnEnd) > INT(lnStart)
      *--- (End-(Start-12))
      lnTmpH = (INT(lnEnd) - (INT(lnStart)-12))
      lnTotTime = lnTotTime + (IIF(lnTmpH>12,lnTmpH-12,lnTmpH)*60)
    ELSE
      lnTmpH = (INT(lnEnd) - INT(lnStart))
      lnTotTime = lnTotTime + (IIF(lnTmpH>12,lnTmpH-12,lnTmpH)*60)
    ENDIF
    SELECT TrakHdr
    SET ORDER TO TrakId
    =SEEK(lcCurrId+DTOS(gdSysDate))
    SCAN REST WHILE id+DTOS(dIss_date)+style+coprcode = ;
                    lcCurrId+DTOS(gdSysDate);
                    FOR Status $ 'PC' .AND. coprcode = lcTmpOpr .AND. nHours = 0.00 .AND. ID <>'*'
      SELECT BOM
      =SEEK(SUBSTR(TrakHdr.Style,1,lnMajLen))
      LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=;
                        SUBSTR(TrakHdr.Style,1,lnMajLen);
                  FOR   mfgCode = TrakHdr.cOprCode
      lnEstTim = IIF(FOUND(),BOM.nEstBomQty,1)
      SELECT TrakHdr
      lnTime = (lnTotTime*((nQty_Comp*lnEstTim)/IIF(lnQtyEst<>0,lnQtyEst,1)))
      REPLACE nhours WITH INT(lnTime/60)+((lnTime-INT(lnTime/60)*60)/100)
      IF nhours < 0 .OR. nhours=0
        *SELECT TrakLin
        *=SEEK(lcCurrId)
        *LOCATE REST FOR Id <> lcCurrId .OR.;
                        !(coprcode == lcTmpOpr)
        SELECT TrakHdr
        REPLACE nhours WITH 0

        WAIT WINDOW "Zero calculated hours for user: "+lcCurrId +" / operation: " +lcTmpOpr +". Nothing to update.!"
        LOOP
      ENDIF
      *--- Call Function to update cTktBom,BomCost WITH the following Parameter
      *--- PARAMETER lcType,lcCut,lcSty,lcOper,lnIssQty
      *IF SEEK("M"+TrakHdr.CutTkt+TrakHdr.cOprCode+'01'+'2','Mfgoprdt')
        lnIssReal = INT(nhours) + ((nhours - INT(nhours))/0.6)
        =lfUpBom(TrakHdr.Type,TrakHdr.CutTkt,TrakHdr.Style,TrakHdr.cOprCode,lnIssReal)
        IF laSetups[2,2]='Y'
          lcGlDTemp = gfTempName()
          =gfOpenFile(gcDataDir+'GLDIST','GLDISTAc','SH')
          SELECT GLDIST
          *: B803342,1 SSH check if used
          IF !USED(lcGlDTemp)
          *: B803342,1 SSH
            COPY STRUCTURE TO (gcWorkDir+lcGlDTemp)
            =gfOpenFile(gcWorkDir+lcGlDTemp,'','EX')
          *: B803342,1 SSH
          ELSE
            SELECT(lcGlDTemp)
          ENDIF
          *: B803342,1 SSH
          ZAP
          DECLARE laMfgRFld[2,2]
          STORE ''  TO laMfgRFld,lcMfgGlAcnt
          laMfgRFld[1,1] = 'LEADTIME'
          laMfgRFld[1,2] = 'lnLeadTime'
          laMfgRFld[2,1] = 'GLACCOUNT'
          laMfgRFld[2,2] = 'lcMfgGlAcnt'
          =gfRltFld(TrakHdr.coprcode,@laMfgRFld,'MFGCODE')
          *--- Liability for non material cost ( 018 )
          ldIssDate  = gdSysDate
          lcGlYear   = ''
          lcGlPeriod = ''
          lnTempHrs  =  INT(TrakHdr.nhours) + ((TrakHdr.nhours - INT(TrakHdr.nhours))/0.6)
          IF (CtktBom.untcost*lnTempHrs) <> 0 AND !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
            RETURN
          ENDIF
          IF TrakHdr.Type = 'C'
            lcLinkCode = IIF(SEEK(TrakHdr.CutTkt,'CutTktH'),CutTktH.Link_Code,'DEFDEF')
          ELSE
            lcLinkCode = IIF(SEEK('P'+TrakHdr.CutTkt,'PoSHdr'),PoSHdr.Link_Code,'DEFDEF')
          ENDIF
          DO GLDIST WITH lcLinkCode,'018',-(CtktBom.untcost*lnTempHrs),;
                         'NL',TrakHdr.CutTkt,ldIssDate,lcGlYear,lcGlPeriod,lcGlDTemp,lcMfgGlAcnt
          *---W.I.P. ( 013 )
          DO GLDIST WITH lcLinkCode,'013',(CtktBom.untcost*lnTempHrs),;
                         'NL',TrakHdr.CutTkt,ldIssDate,lcGlYear,lcGlPeriod,lcGlDTemp
          m.cWipAcnt = &lcGlDTemp..GlAccount
          lcISession = gfsequence('GLSession')
          SELECT (lcGlDTemp)
          REPLACE ALL GlSession WITH lcISession
          SELECT GLDIST
          APPEND FROM (gcWorkDir+lcGlDTemp)
          SELECT (lcGlDTemp)
          *--- lcGlDTemp
          USE IN IIF(USED(lcGlDTemp),lcGlDTemp,0)
          ERASE &gcWorkDir.&lcGlDTemp+'.DBF'
          ERASE &gcWorkDir.&lcGlDTemp+'.CDX'
        ENDIF
      *ENDIF
    ENDSCAN
  ELSE
    SELECT TrakLin
    =SEEK(lcCurrId)
    LOCATE REST FOR Id <> lcCurrId .OR. !(coprcode == lcTmpOpr)
  ENDIF
  lnTotTime = 0
  lnEnd     = 0
  lnStart   = 0
  lnQtyEst  = 0
  lcTotComp = 0
  lnStart   = 99.99
  SELECT TrakLin
ENDDO
USE IN IIF(llBomByMe,'Bom',0)
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfCkNxtIss
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio To Get next issues if exist.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : lcType,lcCutTkt,lcOprCode,lcSty
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCkNxtIss()
*!*************************************************************
*!
FUNCTION lfCkNxtIss
PARAMETER lcType,lcCutTkt,lcOprCode,lcSty,llContinuo
DIMEN laFNxtSeq[1]
laFNxtOpr = ''
laFNxtSeq  = ''

lnold = SELECT(0)
=gfOpenFile(gcWorkDir+lcOprToDel,'','EX')
INDEX ON cimtyp+ctktno+coprcode+clotno+trancd  TAG (lcOprToDel)
*: B603772,1 SSH  Check operation to del
INDEX ON cimtyp+ctktno+cTrgOpr  TAG lcToDel
*: B603772,1 SSH  Check if Variable change
SELECT (lcOprToDel)
SELECT mfgoprhd
lnCurrOpSq = VAL(MfgOprHd.cOperSeq)
SELECT MIN(VAL(mfgoprhd.CoperSeq)) FROM mfgoprhd ;
       WHERE cimtyp+ctktno+coprcode = lcType+lcCutTkt;
       .AND. VAL(MfgOprHd.cOperSeq) > lnCurrOpSq;
         INTO ARRAY laFNxtSeq
IF _TALLY <> 0
  SELECT coprCode FROM mfgoprhd WHERE cimtyp+ctktno+coprcode = lcType+lcCutTkt;
       .AND. mfgoprhd.CoperSeq==ALLTRIM(STR(laFNxtSeq,2)) ;
           INTO ARRAY laFNxtOpr
ENDIF
IF llContinuo
  =SEEK(lcType+lcCutTkt)
  SCAN REST WHILE cimtyp+ctktno+coprcode = lcType+lcCutTkt;
             FOR VAL(MfgOprHd.cOperSeq) > lnCurrOpSq
    llToAdd = .F.
    IF SEEK(cimtyp+ctktno+coprcode,lcOprDt)
      llToAdd = .T.
      SELECT (lcOprDt)
      LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
                        mfgoprhd.cimtyp+mfgoprhd.ctktno+mfgoprhd.coprcode FOR Item = lcSty
      llToAdd = FOUND()
      IF llToAdd
        SELECT (lcOprDt)
        SCAT MEMVAR MEMO
      ENDIF
    ENDIF
    IF !llToAdd AND SEEK(cimtyp+ctktno+coprcode,'MfgOprDt')
      SELECT MfgOprDt
      llToAdd = .T.
      LOCATE REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
                        mfgoprhd.cimtyp+mfgoprhd.ctktno+mfgoprhd.coprcode FOR Item = lcSty
      llToAdd = FOUND()
      IF llToAdd
        SELECT MfgOprDt
        SCAT MEMVAR MEMO
      ENDIF
    ENDIF
    IF llToAdd
      SELECT (lcOprToDel)
      INSERT INTO (lcOprToDel) FROM MEMVAR
      REPLACE cTrgOpr WITH lcOprCode
    ENDIF
  ENDSCAN
  SELECT (lcOprToDel)
  GOTO TOP
ENDIF
SELECT(lnold)


*!*************************************************************
*! Name      : lfDelNxt
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Functio To Delete next  issues if exist.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : llErase to erase browse or no.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDelNxt()
*!*************************************************************
*!
FUNCTION lfDelNxt

PRIVATE lnToDecr
lnToDecr = 0
IF !USED(lcOprToDel)
  RETURN
ENDIF
SELECT (lcOprToDel)
GOTO TOP
SCAN
  *--- Start Delete BomCost
  *--- cimtyp+cuttkt+item+iclr+mfgcode+dyelot
  =SEEK(&lcOprToDel..cimtyp+&lcOprToDel..cTktNo+SPACE(19)+SPACE(06)+&lcOprToDel..coprcode,'ctktbom')
  *--- cbomtype+cimtyp+ctktno+item+iclr+mfgcode
  
  *B804272,1 SSH 07/12/2001 (Begin) Adding the following line.
  =SEEK(CtktBom.Typ+&lcOprToDel..cimtyp+&lcOprToDel..cTktNo+SPACE(19)+SPACE(06)+&lcOprToDel..coprcode,'BomCost')
  *B804272,1 SSH 07/12/2001 (End)
  
  *--- mfgoprdt
  *--- cimtyp+ctktno+coprcode+clotno+trancd
  SELECT mfgoprdt
  =SEEK(&lcOprToDel..cimtyp+&lcOprToDel..cTktNo+&lcOprToDel..coprcode)
  SCAN REST WHILE cimtyp+ctktno+coprcode+clotno+trancd = ;
                  &lcOprToDel..cimtyp+&lcOprToDel..cTktNo+&lcOprToDel..coprcode
    DELETE
  ENDSCAN
  *--- TrakHdr
  *---type+cuttkt+style+coprcode+id
  SELECT TrakHdr
  SET ORDER TO TrakHdr
  lcTy = IIF(&lcOprToDel..cimtyp='M','C','I')
  =SEEK(lcTy+&lcOprToDel..cTktNo+&lcOprToDel..Item+&lcOprToDel..coprcode)
  SCAN REST WHILE type+cuttkt+style+coprcode+id = ;
                  lcTy+&lcOprToDel..cTktNo+&lcOprToDel..Item+&lcOprToDel..coprcode
    lnTempHrs  =  INT(nhours) + ((nhours - INT(nhours))/0.6)
    lnToDecr = lnToDecr+cTktBom.UntCost*lnTempHrs
    DELETE
  ENDSCAN
  *--- BomCst.nTotQty*BomCst.nUnitCst
  SELECT ctktbom
  REPLACE Used_Qty  WITH Used_Qty  - lnTempHrs,;
          Issue_Qty WITH Issue_Qty - lnTempHrs
  
  *--- TrakLin
  *--- type+cuttkt+style+coprcode+id
  SELECT TrakLin
  lcTy = IIF(&lcOprToDel..cimtyp='M','C','I')
  =SEEK(lcTy+&lcOprToDel..cTktNo+&lcOprToDel..Item+&lcOprToDel..coprcode)
  SCAN REST WHILE type+cuttkt+style+coprcode+id = ;
                  lcTy+&lcOprToDel..cTktNo+&lcOprToDel..Item+&lcOprToDel..coprcode
    DELETE
  ENDSCAN
  lcItemType = CtktBom.Typ
  IF lcTy = 'C'
    *--- CutTktH
    *--- cuttkt
    SELECT CutTktH
    =SEEK(&lcOprToDel..cTktNo)
    REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType - lnToDecr
    IF USED(lcCutTran)    && CASE CutTkt
      SELECT(lcCutTran)
      IF SEEK(&lcOprToDel..cTktNo)
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType - lnToDecr
      ENDIF      
    ENDIF
  ELSE
    SELECT POSHDR
    =SEEK('P'+&lcOprToDel..cTktNo)
    REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType - lnToDecr
    IF USED(lcPOTran)     && CASE Po's
      SELECT (lcPOTran)
      IF SEEK('P'+&lcOprToDel..cTktNo)
        REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType - lnToDecr
      ENDIF
    ENDIF  
  ENDIF
  IF laSetups[2,2]='Y'
    lcGlDTempD = gfTempName()
    =gfOpenFile(gcDataDir+'GLDIST','GLDISTAc','SH')
    SELECT GLDIST
    *: B803342,1 SSH Check if used
    IF !USED(lcGlDTempD)
    *: B803342,1 SSH
      COPY STRUCTURE TO (gcWorkDir+lcGlDTempD)
      =gfOpenFile(gcWorkDir+lcGlDTempD,'','EX')
    *: B803342,1 SSH
    ELSE
      SELECT(lcGlDTempD)
    ENDIF
    *: B803342,1 SSH
    DECLARE laMfgRFld[2,2]
    STORE ''  TO laMfgRFld,lcMfgGlAcnt
    laMfgRFld[1,1] = 'LEADTIME'
    laMfgRFld[1,2] = 'lnLeadTime'
    laMfgRFld[2,1] = 'GLACCOUNT'
    laMfgRFld[2,2] = 'lcMfgGlAcnt'
    =gfRltFld(&lcOprToDel..cOprCode,@laMfgRFld,'MFGCODE')
    *--- Liability for non material cost ( 018 )
    ldIssDate  = gdSysDate
    lcGlYear   = ''
    lcGlPeriod = ''
    IF (lnToDecr) <> 0 AND !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
      RETURN
    ENDIF
    IF lcType = 'M'
      lcLinkCode = IIF(SEEK(&lcOprToDel..ctktNo,'CutTktH'),CutTktH.Link_Code,'DEFDEF')
    ELSE
      lcLinkCode = IIF(SEEK('P'+&lcOprToDel..ctktNo,'PoSHdr'),PoSHdr.Link_Code,'DEFDEF')
    ENDIF
    DO GLDIST WITH lcLinkCode,'018',lnToDecr,;
                   'NL',&lcOprToDel..ctktNo,ldIssDate,lcGlYear,lcGlPeriod,lcGlDTempD,lcMfgGlAcnt
    *---W.I.P. ( 013 )
    DO GLDIST WITH lcLinkCode,'013',-(lnToDecr),;
                   'NL',&lcOprToDel..ctktNo,ldIssDate,lcGlYear,lcGlPeriod,lcGlDTempD
    m.cWipAcnt = &lcGlDTempD..GlAccount
    lcISession = gfsequence('GLSession')
    SELECT (lcGlDTempD)
    REPLACE ALL GlSession WITH lcISession
    SELECT GLDIST
    APPEND FROM (gcWorkDir+lcGlDTempD)
    SELECT (lcGlDTempD)
    *--- lcGlDTempD
    USE IN IIF(USED(lcGlDTempD),lcGlDTempD,0)
    ERASE &gcWorkDir.&lcGlDTempD+'.DBF'
    ERASE &gcWorkDir.&lcGlDTempD+'.CDX'
  ENDIF
  SELECT BomCost
  DELETE
ENDSCAN


*!*************************************************************
*! Name      : lfUpMisGl   *: B803310,1 SSH Doc.
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Update Gl-Enteries for Mis.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpMisGl()
*!*************************************************************
*!
FUNCTION lfUpMisGl
PARAMETER lnCost

*8*--- Update Gldist File.
IF laSetups[2,2]='Y'
  =gfOpenFile(gcDataDir+'GLDIST','GLDISTAc','SH')
  =gfOpenFile(gcDataDir+'gl_link','gl_link','SH')
  SELECT GLDIST
  *: B803342,1 SSH Check if used
  IF !USED(lcGlDTempM)
  *: B803342,1 SSH
    COPY STRUCTURE TO (gcWorkDir+lcGlDTempM)
    =gfOpenFile(gcWorkDir+lcGlDTempM,'','EX')
  *: B803342,1 SSH
  ELSE
    SELECT(lcGlDTempM)
  ENDIF
  *: B803342,1 SSH
  DECLARE laMfgRFld[2,2]
  STORE ''  TO laMfgRFld,lcMfgGlAcnt
  laMfgRFld[1,1] = 'LEADTIME'
  laMfgRFld[1,2] = 'lnLeadTime'
  laMfgRFld[2,1] = 'GLACCOUNT'
  laMfgRFld[2,2] = 'lcMfgGlAcnt'
  =gfRltFld(lcOprCode,@laMfgRFld,'MFGCODE')
  *--- Liability for non material cost ( 018 )
  ldIssDate  = gdSysDate
  lcGlYear   = ''
  lcGlPeriod = ''
  IF lnCost <> 0 AND !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
    RETURN
  ENDIF
  lcLinkCode = 'DEFDEF'
  DO GLDIST WITH lcLinkCode,'018',-lnCost,;
                 'NL','******',ldIssDate,lcGlYear,lcGlPeriod,lcGlDTempM,lcMfgGlAcnt
  *---W.I.P. ( 013 )
  DO GLDIST WITH lcLinkCode,'013',lnCost,;
                 'NL','******',ldIssDate,lcGlYear,lcGlPeriod,lcGlDTempM
  lcISession = gfsequence('GLSession')
  SELECT (lcGlDTempM)
  REPLACE ALL GlSession WITH lcISession
  *:  Start Update BOMCOST.
  IF !USED(lcBomCost)
    SELECT BOMCOST
    COPY STRUCTURE TO (gcWorkDir+lcBomCost)
    =gfOpenFile(gcWorkDir+lcBomCost,'','EX')
   INDEX ON cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession TAG (lcBomCost)
  ENDIF
  
  IF lnCost < 0
    *--- cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession
    IF SEEK(' '+'M'+'******'+SPACE(19)+SPACE(06)+lcOprCode,lcBomCost)
      SELECT(lcBomCost)
      LOCATE REST WHILE cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                        ' '+'M'+'******'+SPACE(19)+SPACE(06)+lcOprCode;
                  FOR  nTotCst = -1*lnCost
      IF FOUND()
        SELECT(lcBomCost)
        REPLACE ctktno WITH SPACE(06)
      ELSE
        SELECT(lcBomCost)
        APPEND BLANK
        REPLACE cTktNo    WITH SPACE(06)  ,;
                mfgCode   WITH lcOprCode ,;
                cOprCode  WITH lcOprCode ,;
                nTotQty   WITH 1         ,;
                nUnitCst  WITH lnCost    ,;
                nTotCst   WITH lnCost    ,;
                cIsession WITH lcISession,;
                cCostType WITH "M"       ,;
                cImTyp    WITH "M"       ,;
                cWipAcnt  WITH IIF(SEEK(lcLinkCode+"013",'gl_link'),gl_link.glAcnt,'')
      ENDIF
    ELSE
      IF SEEK(' '+'M'+'******'+SPACE(19)+SPACE(06)+lcOprCode,'BomCost')
        SELECT BomCost
        LOCATE REST WHILE cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession=;
                          ' '+'M'+'******'+SPACE(19)+SPACE(06)+lcOprCode;
                    FOR  nTotCst = -1*lnCost
        IF FOUND()
          SCAT MEMVAR MEMO
          INSERT INTO (lcBomCost) FROM MEMVAR
          SELECT(lcBomCost)
          REPLACE CtktNo WITH SPACE(06)
        ENDIF
      ENDIF
    ENDIF
  
  ELSE
    SELECT (lcBomCost)
    APPEND BLANK
    REPLACE cTktNo    WITH "******"  ,;
            mfgCode   WITH lcOprCode ,;
            cOprCode  WITH lcOprCode ,;
            nTotQty   WITH 1         ,;
            nUnitCst  WITH lnCost    ,;
            nTotCst   WITH lnCost    ,;
            cIsession WITH lcISession,;
            cCostType WITH "M"       ,;
            cImTyp    WITH "M"       ,;
            cWipAcnt  WITH IIF(SEEK(lcLinkCode+"013",'gl_link'),gl_link.glAcnt,'')
   ENDIF
  *: B803342,1 SSH  link_code+catgkey
ENDIF

*!*************************************************************
*! Name      : lfGetLstIs   *: B803310,1 SSH Doc.
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Get the last Issu.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetLstIs()
*!*************************************************************
*!
FUNCTION lfGetLstIs

FOR lnOpIn  = ALEN(laPreOp) TO 1 STEP -1
  SELECT(lcTrkLin)
  IF SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+laPreOp[lnOpIn]+'C')
    FOR lnInd = 1 TO 8
      lcInd1 = STR(lnInd,1)
      lnLastComp[lnInd] = IIF(&lcTrkLin..Qty&lcInd1<>0,&lcTrkLin..Qty&lcInd1,lnLastComp[lnInd])
    ENDFOR
    lnLastComp[9] = IIF(&lcTrkLin..TotQty<>0,&lcTrkLin..TotQty,lnLastComp[9])
    EXIT
  ELSE
    SELECT TrakLin
    SET ORDER TO TrakLin
    IF SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+laPreOp[lnOpIn]+'C')
      FOR lnInd = 1 TO 8
        lcInd1 = STR(lnInd,1)
        lnLastComp[lnInd] = IIF(TrakLin.Qty&lcInd1<>0,TrakLin.Qty&lcInd1,lnLastComp[lnInd])
      ENDFOR
      lnLastComp[9] = IIF(TrakLin.TotQty<>0,TrakLin.TotQty,lnLastComp[9])
      EXIT
    ENDIF
  ENDIF
ENDFOR


*!*************************************************************
*! Name      : lfVEndPros   *: B803310,1 SSH Doc.
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid Function for end Process.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVEndPros()
*!*************************************************************
*!
FUNCTION lfVEndPros

PRIVATE lnOldAlias
lnOldAlias = SELECT(0)

*C124152,1 NNA 11/09/2004 (BEGIN) don't let supervisor be able to close the cuttkt if
*C124152,1 NNA             there is any opened operations
SELECT TRAKHDR
llOpenOper = .F.
IF SEEK('C'+&lcTmpQuery..Cuttkt)
  SCAN REST WHILE Type+Cuttkt+Style+Coprcode+Id = 'C'+&lcTmpQuery..Cuttkt
    IF STATUS $ 'IP'
      llOpenOper = .T.
    ENDIF
  ENDSCAN
  IF llOpenOper
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'A previous operation is not closed yet ;
               , cannot proceed.')
    SELECT(lnOldAlias)
    RETURN
  ENDIF
ENDIF
*C124152,1 NNA (End)

=lfdateRang()
*: B603835,1 SSH Commented Out
*=lfCalcTim()
SELECT(lnOldAlias)


*!*************************************************************
*! Name      : lfWHours   *: B803310,1 SSH Doc.
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : When Function for Hourse.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWHours()
*!*************************************************************
*!
FUNCTION lfWHours
lnOldHors = EVAL(SYS(18))


*!*************************************************************
*! Name      : lfvHours   *: B803310,1 SSH Doc.
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid Function for Hourse.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvHours()
*!*************************************************************
*!
FUNCTION lfvHours
PRIVATE lnOldAls,lnDiffe

lnOldAls = SELECT(0)
lnDiffe  = (INT(lnHours) + ((lnHours - INT(lnHours))/0.6)) - ;
           (INT(lnOldHors) + ((lnOldHors - INT(lnOldHors))/0.6))
IF lnDiffe <> 0
  SELECT(lcTrkHdr)
  REPLACE nHours WITH lnHours,;
          FLAG   WITH 'Y'
  *--- Start Update CutTktH/PO File with the new actual cost
  *--- lcCutTran (With CutTktH Structure)
  *--- lcPOTran  (With PO Structure)
  DO CASE
    CASE !USED(lcCutTran) AND &lcTrkHdr..Type = 'C'     && CASE CutTkt
      SELECT CutTktH
      COPY STRUCTURE TO (gcWorkDir+lcCutTran)
      =gfOpenFile(gcWorkDir+lcCutTran,'','EX')
      INDEX ON CUTTKT TAG (lcCutTran)
    CASE !USED(lcPOTran) AND &lcTrkHdr..Type = 'P'     && CASE Po's
      SELECT POSHDR
      COPY STRUCTURE TO (gcWorkDir+lcPOTran)
      =gfOpenFile(gcWorkDir+lcPOTran,'','EX')
      INDEX ON cstytype+po TAG (lcPOTran)
  ENDCASE
  SELECT(lcTrkHdr)
  =lfUpdTTran()
  *--- Start Update GL Enteries with the new cost
  IF laSetups[2,2]='Y'
    =gfOpenFile(gcDataDir+'GLDIST','GLDISTAc','SH')
    SELECT GLDIST
    *: B803342,1 SSH Check if used
    IF !USED(lcDiffGls)
    *: B803342,1 SSH
      COPY STRUCTURE TO (gcWorkDir+lcDiffGls)
      =gfOpenFile(gcWorkDir+lcDiffGls,'','EX')
    *: B803342,1 SSH
    ELSE
      SELECT(lcDiffGls)
    ENDIF
    *: B803342,1 SSH
    DECLARE laMfgRFld[2,2]
    STORE ''  TO laMfgRFld,lcMfgGlAcnt
    laMfgRFld[1,1] = 'LEADTIME'
    laMfgRFld[1,2] = 'lnLeadTime'
    laMfgRFld[2,1] = 'GLACCOUNT'
    laMfgRFld[2,2] = 'lcMfgGlAcnt'
    =gfRltFld(TrakHdr.coprcode,@laMfgRFld,'MFGCODE')
    *--- Liability for non material cost ( 018 )
    ldIssDate  = gdSysDate
    lcGlYear   = ''
    lcGlPeriod = ''
    IF (CtktBom.untcost*lnDiffe) <> 0 AND !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
      RETURN
    ENDIF
    IF TrakHdr.Type = 'C'
      lcLinkCode = IIF(SEEK(TrakHdr.CutTkt,'CutTktH'),CutTktH.Link_Code,'DEFDEF')
    ELSE
      lcLinkCode = IIF(SEEK('P'+TrakHdr.CutTkt,'PoSHdr'),PoSHdr.Link_Code,'DEFDEF')
    ENDIF
    DO GLDIST WITH lcLinkCode,'018',-(CtktBom.untcost*lnDiffe),;
                   'NL',&lcTrkHdr..CutTkt,ldIssDate,lcGlYear,lcGlPeriod,lcDiffGls,lcMfgGlAcnt
    *---W.I.P. ( 013 )
    DO GLDIST WITH lcLinkCode,'013',(CtktBom.untcost*lnDiffe),;
                   'NL',&lcTrkHdr..CutTkt,ldIssDate,lcGlYear,lcGlPeriod,lcDiffGls
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfUpdTTran   *: B803310,1 SSH Doc.
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Update Transaction.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpdTTran()
*!*************************************************************
*!
FUNCTION lfUpdTTran

*--- lcCutTran
*--- lcPOTran
DO CASE
  CASE &lcTrkHdr..Type = 'C'     && CASE CutTkt
    SELECT CutTktH
    IF SEEK(&lcTrkHdr..CutTkt)
      =SEEK('M'+&lcTrkHdr..CutTkt+SPACE(19)+SPACE(6)+&lcTrkHdr..cOprCode,'cTktBom')
      lcItemType = CtktBom.Typ
      SCAT MEMVAR MEMO
      SELECT(lcCutTran)
      IF !SEEK(&lcTrkHdr..CutTkt)
        INSERT INTO (lcCutTran) FROM MEMVAR
      ENDIF
      SELECT(lcCutTran)
      REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType+(lnDiffe*cTktBom.UntCost)
    ENDIF
    *-- lcTrkHdr
  CASE &lcTrkHdr..Type = 'P'     && CASE Po's
    SELECT POSHDR
    IF SEEK('P'+&lcTrkHdr..CutTkt)
      
      *B804272,1 SSH 07/12/2001 (Begin) Adding the following line of code.
      =SEEK('I'+&lcTrkHdr..CutTkt+SPACE(19)+SPACE(6)+&lcTrkHdr..cOprCode,'cTktBom')
      *B804272,1 SSH 07/12/2001 (End)
      
      lcItemType = CtktBom.Typ
      SCAT MEMVAR MEMO
      SELECT(lcPOTran)
      IF !SEEK('P'+&lcTrkHdr..CutTkt)
        INSERT INTO (lcPOTran) FROM MEMVAR
      ENDIF
      SELECT(lcPOTran)
      REPLACE nAct_Cost&lcItemType WITH nAct_Cost&lcItemType+(lnDiffe*cTktBom.UntCost)
    ENDIF
ENDCASE

*--- Update CTktBom

IF !USED(lcCTktBom)
  SELECT CTKTBOM
  COPY STRUCTURE TO (gcWorkDir+lcCTktBom)
  =gfOpenFile(gcWorkDir+lcCTktBom,'','EX')
  INDEX ON cimtyp+cuttkt+item+iclr+mfgcode+dyelot TAG (lcCTktBom)
ENDIF
SELECT CtktBom

*B804272,1 SSH 07/12/2001 (Begin) Adding the case of PO.
IF SEEK(IIF(&lcTrkHdr..Type = "C","M","I")+&lcTrkHdr..CutTkt+SPACE(19)+SPACE(6)+&lcTrkHdr..cOprCode)
*B804272,1 SSH 07/12/2001 (End)

  SCAT MEMVAR MEMO
ENDIF
SELECT(lcCTktBom)

*B804272,1 SSH 07/12/2001 (Begin) Adding the case of PO.
*IF !SEEK('M'+&lcTrkHdr..CutTkt+SPACE(19)+SPACE(6)+&lcTrkHdr..cOprCode)
IF !SEEK(IIF(&lcTrkHdr..Type = "C","M","I")+&lcTrkHdr..CutTkt+SPACE(19)+SPACE(6)+&lcTrkHdr..cOprCode)
*B804272,1 SSH 07/12/2001 (End)

  INSERT INTO (lcCTktBom) FROM MEMVAR
ELSE
  GATH MEMVAR MEMO
ENDIF
REPLACE Used_Qty  WITH Used_Qty+lnDiffe,;
        Issue_Qty WITH Issue_Qty+lnDiffe

*--- Update BomCost
IF !USED(lcBomCost)
  SELECT BOMCOST
  COPY STRUCTURE TO (gcWorkDir+lcBomCost)
  =gfOpenFile(gcWorkDir+lcBomCost,'','EX')
 INDEX ON cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession TAG (lcBomCost)
ENDIF
*---
SELECT BomCost
IF SEEK(CtktBom.Typ+IIF(&lcTrkHdr..Type $ 'C','M','I')+&lcTrkHdr..CutTkt+SPACE(19)+SPACE(6)+&lcTrkHdr..cOprCode)
  SCAT MEMVAR MEMO
  SELECT (lcBomCost)
  IF !SEEK(CtktBom.Typ+IIF(&lcTrkHdr..Type $ 'C','M','I')+&lcTrkHdr..CutTkt+SPACE(19)+SPACE(6)+&lcTrkHdr..cOprCode)
    INSERT INTO (lcBomCost) FROM MEMVAR
  ELSE
    GATH MEMVAR MEMO
  ENDIF
  REPLACE  nTotQty   WITH nTotQty+(lnDiffe) ,;
           nTotCst   WITH nTotCst+(lnDiffe*CtktBom.untcost),;
           nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty) ,;
           nTotACst  WITH nTotACst+(lnDiffe*CtktBom.untcost),;
           nUnitACst WITH IIF(nTotQty=0,0,nTotACst/nTotQty)
ELSE
  SELECT (lcBomCost)
  APPEND BLANK
  REPLACE cTktNo    WITH &lcTrkHdr..CutTkt,;
          cDyelot   WITH SPACE(08)  ,;
          Item      WITH SPACE(1)   ,;
          IClr      WITH SPACE(1)   ,;
          cBomType  WITH CtktBom.Typ,;
          cIMTyp    WITH IIF(&lcTrkHdr..Type $ 'C','M','I') ,;
          MfgCode   WITH &lcTrkHdr..cOprCode,;
          dTranDate WITH gdSysDate,;
          cRSession WITH SPACE(6),;
          cISession WITH '',;
          cCostType WITH 'M' ,;
          nTotQty   WITH nTotQty+(lnDiffe) ,;
          nTotCst   WITH nTotCst+(lnDiffe*CtktBom.untcost) ,;
          nUnitCst  WITH IIF(nTotQty=0,0,nTotCst/nTotQty),;
          nTotACst  WITH nTotACst+(lnDiffe*CtktBom.untcost),;
          nUnitACst WITH IIF(nTotQty=0,0,nTotACst/nTotQty),;
          cOprCode  WITH &lcTrkHdr..cOprCode ,;
          cWipAcnt  WITH IIF(&lcTrkHdr..Type $ 'C',CutTktH.cWipAcnt,'')
ENDIF



*!*************************************************************
*! Name      : lfBrowScla   *: B803310,1
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Fix the bug of can not browse sizes
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfBrowScla()
*!*************************************************************
*!
FUNCTION lfBrowScla

PRIVATE lnOld , lcBrowTmp , lcScl , lcToRet

lnOld = SELECT(0)
lcBrowTmp   = gfTempName()
CREATE TABLE &gcWorkDir.&lcBrowTmp (SzSort C(01),SDesc C(12),Sz C(07),Style C(19))
INDEX ON SzSort + Sz TAG &lcBrowTmp
SELECT SCALE
lcScl = IIF(SEEK(&lcTmpQuery..Style,'Style'),Style.Scale,'')
SELECT SCALE
IF SEEK('S'+lcScl)
  FOR lnInd = 1 TO SCALE.Cnt
    lcInd = STR(lnInd,1)
    m.SDesc  = SCALE.cScl_Desc
    m.Sz     = SCALE.SZ&lcInd
    m.Style  = &lcTmpQuery..Style
    m.SzSort = lcInd
    IF !SEEK(m.SzSort+m.Sz,lcBrowTmp)
      INSERT INTO (lcBrowTmp) FROM MEMVAR 
    ENDIF
  ENDFOR
ENDIF
lcBrFields = "Style :23 :H = [Item ],"+;
             "Sz    :12 :H = [Size Desc ],"+;
             "sDesc :15 :H = [Scale Disc]"
SELECT (lcBrowTmp)
DIMENSION laTemp[1]
laTemp = ''
lcKey  = "" 
lcFile_Ttl  = 'Select Size'
lcPbrowtitl = 'Select Size'
SELECT (lcBrowTmp)
GOTO TOP
llExit = EOF()
llDummy = ARIABROW("lcKey",lcPbrowtitl,gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','SZ','laTemp')
SELECT(lnOld)
IF !llExit
  lcToRet = &lcBrowTmp..SZ
  RETURN lcToRet
ENDIF


*!*************************************************************
*! Name      : lfBrowId   *: B803342
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Browse Id function
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfBrowId()
*!*************************************************************
*!
*: B803342,1 SSH Add User Id Browse
FUNCTION lfBrowId
*: B######,1 SSH Add User Id Browse
*--- lcUsrId
SELECT syuuser
*SET ORDER TO
*SELECT DIST(cUser_Id),cUsr_Name,cUsr_Kbuf,cUsr_Phon  FROM syuuser WHERE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam='NMFGCODE';
       AND !EMPTY(cDiscRep) INTO DBF(gcWorkDir+lcMisOp)
  
lcBrFields = "cUser_Id   :15 :H =  'User Id',"+;
             "cUsr_Name  :10 :H =  'User Name',"+;
             "cUsr_Kbuf  :10 :H =  'Privileges',"+;             
             "cUsr_Phon  :15 :H =  'User Phone'"
DIMENSION laTemp[1]
laTemp = ''
lcKey = 'cUser_Id'
lcFile_Ttl  = 'Select User Id'
lcPbrowtitl = 'Select User Id'
GOTO TOP
llDummy = ARIABROW("",lcPbrowtitl,gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','cUser_Id','laTemp')
IF !llDummy
  laTemp = ''
ENDIF
RETURN(laTemp)


*!*************************************************************
*! Name      : lfvScan   *: B803342
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid function for Scan ticket screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvScan()
*!*************************************************************
*!
*: B803342,1 SSH Add User Id Browse
FUNCTION lfvScan

lcTrkTkt = ''
lcScnTitle = 'Scan Tracking Tickets'
lcBarPic = 'XXXXXX'+LCMJRPCT+'XXXXXXXXXXX'
PUSH KEY 
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL CTRL+TAB
ON KEY LABEL CTRL+ENTER
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL CTRL+W
ON KEY LABEL Esc
DIMENSION laconf[1,2]
laconf[1,1] = 'M_CONFIRM'
=gfGetMemVar(@laconf,gcAct_Comp)
llConfirm  = (ALLTRIM(laconf[1,2]) = "Y")
lcOldCon = SET ("CONFIRM")
lcCnfrm = IIF(llConfirm,'ON','OFF')
SET CONFIRM &lcCnfrm
IF !USED('cTktBarC')
  =gfOpenFile(gcDataDir+'cTktBarC','','SH')
ENDIF
DO gcScrDir+'MF\'+("MFRBRSCN.SPR")
POP KEY
IF USED('cTktBarC')
  USE IN cTktBarC
ENDIF
SET CONFIRM &lcOldCon
=lfEndDis()

SELECT(lcTmpQuery)
GOTO TOP
IF !EOF(lcTmpQuery) AND !EMPTY(&lcTmpQuery..CutTkt)
  ACTIVATE WINDOW MFRBRBU
  SHOW GET pbSav ENABLE 
ENDIF


*!*************************************************************
*! Name      : lfGtTktSeg   *: B803342
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid function for Scan ticket screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGtTktSeg()
*!*************************************************************
*!
FUNCTION lfGtTktSeg

PRIVATE lcTkt , lcSty , lcOpr , lcSize
IF EMPTY(lcTrkTkt)
  RETURN
ENDIF
lcTrkTkt = ALLTRIM(STRTRAN(lfReadBc(ALLTRIM(lcTrkTkt),"R"),',',' '))
lcTkt    = SUBSTR(lcTrkTkt,01,06)
lcSty    = SUBSTR(lcTrkTkt,07,19)
lcOpr    = SUBSTR(lcTrkTkt,26,6)
lcSize   = SUBSTR(lcTrkTkt,32)
lcTrkTkt = ''
IF EMPTY(lcTkt) .OR. EMPTY(lcSty) .OR. EMPTY(lcOpr) .OR. EMPTY(lcSize)

  DO CASE
    CASE EMPTY(lcTkt) OR lcTkt = '**'
      =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Invalid barcode! Transaction # is empty, cannot proceed.')
    CASE EMPTY(lcSty)
      =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Invalid barcode! Style code is empty, cannot proceed.')
    CASE EMPTY(lcOpr)
      =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Invalid barcode! Operation code is empty, cannot proceed.')
    CASE EMPTY(lcSize)
      =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Invalid barcode! Scale size is empty, cannot proceed.')
  ENDCASE

  RETURN
ELSE
  lcTrkTkt = ''
  SHOW GET lcTrkTkt ENABLE
  SELECT(lcTmpQuery)

  *REPLACE CutTkt    WITH lcTkt,;
          Style     WITH lcSty,;
          Operation WITH lcOpr,;
          SclDes    WITH lcSize
  *=lfVCut()
  *=lfVStyle()
  *=lfVOpCode(.T.)
  *=lfvDes(.T.)
  *=lfEndDis()        
  =SEEK('Z')
  REPLACE CutTkt    WITH lcTkt
  IF lfVCut()
    REPLACE Style     WITH lcSty
    IF lfVStyle()
      REPLACE Operation WITH lcOpr
      IF lfVOpCode(.T.)
        REPLACE SclDes    WITH lcSize
        =lfvDes(.T.)
        =lfEndDis()        
      ELSE
        REPLACE Operation WITH '',;
                CutTkt    WITH '',;
                Style     WITH '',;
                Type      WITH ''
      ENDIF
    ELSE
      REPLACE Style     WITH '',;
              CutTkt    WITH '',;
              Type      WITH ''
    ENDIF
  ELSE
    REPLACE CutTkt    WITH '',;
            Type      WITH ''
  ENDIF

  _CUROBJ=OBJNUM(lcTrkTkt)
  SHOW WINDOW (lcMtBrowTt) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfEndDis   *: B803342
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Enable/Disable Function.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfEndDis()
*!*************************************************************
*!
FUNCTION lfEndDis
PARAMETER llFrmBar

PRIVATE lnOld 
lnOld = SELECT(0)
SELECT (lcTmpQuery)
IF (llAdmin AND llReci) .OR. (!EMPTY(&lcTmpQuery..CutTkt).AND.!SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt,'TRAKHDR')  .OR. TRAKHDR.Status='P')
  SHOW GET pbEDit ENABLE
  SHOW GET pbSav ENABLE
ELSE
  SHOW GET pbEDit DISABLE
  SHOW GET pbSav  DISABLE
ENDIF
lcScalKey = CutTkt+Style+Operation+UserId   &&+Scal
lcSizKey  = Scal
*--- type+cuttkt+style+coprcode+status+coperseq
lnIss   = IIF(EMPTY(Scal),MFGOPRDT.nlotTotQty,EVAL('Sz'+Scal))
GOTO TOP
=SEEK('Z'+lcScalKey)
lnNewIss = IIF(EMPTY(Scal),MFGOPRDT.nlotTotQty,EVAL('Sz'+Scal))
*--- +lcSizKey
IF !llFrmBar
  SELECT (lcTmpQuery)
  IF SEEK(' '+lcScalKey)
    IF !EMPTY(lcSizKey)
      REPLACE llDis       WITH .T.,;
              Sz&lcSizKey WITH lnNewIss,;
              TotQty      WITH TotQty+IIF(Scal=lcSizKey,0,lnIss)
    ENDIF
    =SEEK('Z'+lcScalKey)
    DELETE
    IF llFrmBar
      KEYBOARD "{UPARROW}"
      SHOW WINDOW (lcMtBrowTt) REFRESH SAME
    ENDIF
    SELECT (lcTmpQuery)
    =SEEK(' '+lcScalKey+lcSizKey)
  ELSE
    IF SEEK(' '+lcScalKey)
      =SEEK('Z'+lcScalKey+lcSizKey)
      DELETE
      IF llFrmBar
        KEYBOARD "{UPARROW}"
        SHOW WINDOW (lcMtBrowTt) REFRESH SAME
      ENDIF
      SELECT (lcTmpQuery)
      =SEEK(' '+lcScalKey)
      REPLACE llDis       WITH .T.,;
              Sz&lcSizKey WITH lnIss,;
              TotQty      WITH TotQty+IIF(Scal=lcSizKey,0,lnIss)
    ENDIF
  ENDIF
  =SEEK('Z')
  IF !EMPTY(CutTkt)
    REPLACE lSort WITH  ' ',;
            llDis WITH .T.
  ENDIF
  IF !SEEK('Z')
    APPEND BLANK
    REPLACE lSort WITH  'Z'
  ENDIF
  =lfRestLogc()
  SELECT (lnOld)
  IF llFrmBar   &&MOD
    KEYBOARD "{DNARROW}"
    KEYBOARD "{ENTER}"
    SHOW WINDOW (lcMtBrowTt) REFRESH SAME
  ENDIF
ENDIF
IF !EOF(lcTmpQuery) AND !EMPTY(&lcTmpQuery..CutTkt)
  SHOW GET pbSav ENABLE
ENDIF
SELECT(lnOld)


*!*************************************************************
*! Name      : lfReadBc   *: B803342
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Read Bar Code Function.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfReadBc()
*!*************************************************************
*!
FUNCTION lfReadBc
PARAMETER lcPasBarCd, lcRW

PRIVATE lnAlias
lnAlias = SELECT()
lcBarCode = ""

SELECT cTktBarC

*IF lcRW = 'W'
*   SET ORDER TO TAG Corgcode   
*   IF !SEEK(lcPasBarCd)
*     lcBarCode = SYS(2007,PADR(lcPasBarCd,60))
*     APPEND BLANK
*     REPLACE cOrgCode WITH lcPasBarCd,;
*             cGenCode WITH lcBarCode+lcBarCode
*   ELSE
*     lcBarCode = cGenCode
*   ENDIF
*ENDIF

IF lcRW = 'R'
   SET ORDER TO TAG Cgencode
   IF SEEK(lcPasBarCd)
     
     *lcBarCode = cOrgCode
     lcBarCode = IIF(EMPTY(cOrgCode),'**',cOrgCode)
     
   ELSE
     *--Error
     lcBarCode = "**"
   ENDIF
ENDIF
SELECT(lnAlias )
RETURN ALLTRIM(lcBarCode)

FUNCTION lfvBar

lcTrkTkt = STRTRAN(lfReadBc(ALLTRIM(lcTrkTkt),"R"),',',' ')
SHOW GET lcTrkTkt


*!*************************************************************
*! Name      : lfGetPre   *: B803342
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Get prev operation to update.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetPre()
*!*************************************************************
*!
FUNCTION lfGetPre
PARAMETER lcTyp ,lcCut , lcSty , lcOprArray , llFrmNxt
PRIVATE lnOld , llUpdate
lnOld = SELECT(0)
FOR lnArLen = 1 TO ALEN(&lcOprArray)
  llUpdate = .F.
  lcOpr = &lcOprArray[lnArLen]
  *--- TrakLin
  SELECT TrakLin
  *--- type+cuttkt+style+coprcode+status+coperseq
  GOTO TOP
  IF SEEK(lcTyp+lcCut+lcSty+lcOpr)
    SCAN REST WHILE type+cuttkt+style+coprcode+status+coperseq=;
                    lcTyp+lcCut+lcSty+lcOpr && FOR DTOS(Date) = DTOS(gdSysDate)
      SCAT MEMVAR MEMO
      SELECT (lcTrkLin)
      IF !SEEK(m.type+m.cuttkt+m.style+m.coprcode+m.status)
        APPEND BLANK
        GATH MEMVAR MEMO
        llUpdate = .T.
      ENDIF
      DO CASE
        CASE !llFrmNxt AND m.status $ 'CI'
          FOR lnTmp = 1 TO 8
           lcTemp = STR(lnTmp,1)
           IF lnIss&lcTemp <> 0
             SELECT(lcTrkLin)
             REPLACE Qty&lcTemp WITH lnIss&lcTemp,;
                     TotQty     WITH lnTotIss
           ENDIF
          ENDFOR
          *--- lcTyp+lcCut+lcSty+lcOpr
          DO CASE
            CASE m.status = 'C'
              IF SEEK(lcTyp + lcCut + lcOpr + lcSty +'C' + DTOS(gdSysDate),'TrStrEnd')
                llDummy = lfIssTime("M",0,lnTotIss,0, lcTyp , lcCut , lcOpr , lcSty)
              ENDIF
            CASE m.status = 'I'
              IF SEEK(lcTyp + lcCut + lcOpr + lcSty +'I' + DTOS(gdSysDate),'TrStrEnd')
                llDummy = lfIssTime("M",lnTotIss,0,0, lcTyp , lcCut , lcOpr , lcSty)
              ENDIF
          ENDCASE
        CASE llFrmNxt .AND. lnComTot < TotQty
           FOR lnTmp = 1 TO 8
            lcTemp = STR(lnTmp,1)
            SELECT(lcTrkLin)
            REPLACE Qty&lcTemp WITH IIF(m.Status='P',0,lnCom&lcTemp),;
                    TotQty     WITH IIF(m.Status='P',0,lnComTot)
           ENDFOR
           *--- lcTyp+lcCut+lcSty+lcOpr
           IF m.Status<>'P'
             DO CASE
               CASE m.status = 'C'
                 IF SEEK(lcTyp + lcCut + lcOpr + lcSty +'C' + DTOS(gdSysDate),'TrStrEnd')
                   llDummy = lfIssTime("M",0,lnComTot,0, lcTyp , lcCut , lcOpr , lcSty)
                 ENDIF
               CASE m.status = 'I'
                 IF SEEK(lcTyp + lcCut + lcOpr + lcSty +'I' + DTOS(gdSysDate),'TrStrEnd')
                   llDummy = lfIssTime("M",lnComTot,0,0, lcTyp , lcCut , lcOpr , lcSty)
                 ENDIF
             ENDCASE
           ENDIF
      ENDCASE
    ENDSCAN
  ENDIF
  
  *--- TrakLin
  SELECT (lcTrkLin)
  *--- type+cuttkt+style+coprcode+status+coperseq
  GOTO TOP
  IF SEEK(lcTyp+lcCut+lcSty+lcOpr)
    SCAN REST WHILE type+cuttkt+style+coprcode+status+coperseq=;
                    lcTyp+lcCut+lcSty+lcOpr &&FOR DTOS(Date) = DTOS(gdSysDate)
      SCAT MEMVAR MEMO
      DO CASE
        CASE !llFrmNxt AND m.status $ 'CI'
          FOR lnTmp = 1 TO 8
           lcTemp = STR(lnTmp,1)
           IF lnIss&lcTemp <> 0
             SELECT(lcTrkLin)
             REPLACE Qty&lcTemp WITH lnIss&lcTemp,;
                     TotQty     WITH lnTotIss
           ENDIF
          ENDFOR
        CASE llFrmNxt .AND. lnComTot < TotQty
           FOR lnTmp = 1 TO 8
            lcTemp = STR(lnTmp,1)
            SELECT(lcTrkLin)
            REPLACE Qty&lcTemp WITH IIF(m.Status='P',0,lnCom&lcTemp),;
                    TotQty     WITH IIF(m.Status='P',0,lnComTot)
           ENDFOR
      ENDCASE
    ENDSCAN
  ENDIF
  SELECT TrakHdr
  *--- type+cuttkt+style+coprcode+id
  IF SEEK(lcTyp+lcCut+lcSty+lcOpr)
      SCAN REST WHILE type+cuttkt+style+coprcode+id=lcTyp+lcCut+lcSty+lcOpr &&;
                 FOR DTOS(dIss_Date) = DTOS(gdSysDate)
      SCAT MEMVAR MEMO
      SELECT (lcTrkHdr)
      IF !SEEK(m.type+m.cuttkt+m.style+m.coprcode)
        APPEND BLANK
        GATH MEMVAR MEMO
        llUpdate = .T.
      ENDIF
      IF llFrmNxt 
        IF lnComTot < nQty_Comp .OR. lnComTot < Issue_Qty
          REPLACE  Issue_Qty WITH lnComTot,;
                   nQty_Comp WITH lnComTot,;
                   nQty_Pen  WITH IIF(m.Status='P',0,nQty_Pen),;
                   Status    WITH IIF(m.Status='P','C',Status)
        ENDIF
      ELSE
        REPLACE  Issue_Qty WITH IIF(lnTotIss<>0,lnTotIss,Issue_Qty),;
                 nQty_Comp WITH Issue_Qty
      ENDIF
    ENDSCAN
  ENDIF
  SELECT (lcTrkHdr)
  *--- type+cuttkt+style+coprcode+id
  IF SEEK(lcTyp+lcCut+lcSty+lcOpr)
      SCAN REST WHILE type+cuttkt+style+coprcode+id=lcTyp+lcCut+lcSty+lcOpr &&;
              FOR DTOS(dIss_Date) = DTOS(gdSysDate)
      llUpdate = .T.
      IF llFrmNxt 
        IF lnComTot < nQty_Comp  .OR. lnComTot < Issue_Qty
          REPLACE  Issue_Qty WITH lnComTot,;
                   nQty_Comp WITH IIF(nQty_Comp<>0,lnComTot,nQty_Comp),;
                   nQty_Pen  WITH IIF(m.Status='P',0,nQty_Pen),;
                   Status    WITH IIF(m.Status='P','C',Status)
        ENDIF
      ELSE
        REPLACE  Issue_Qty WITH IIF(lnTotIss<>0,lnTotIss,Issue_Qty),;
                 nQty_Comp WITH Issue_Qty
      ENDIF
    ENDSCAN
  ENDIF
  SELECT MfgOprDt
  *--- cimtyp+ctktno+coprcode+clotno+trancd
  *--- lcTyp+lcCut+lcSty+lcOpr

  *B804272,1 SSH 07/12/2001 (Begin) Adding the case of PO.
  *IF SEEK("M"+lcCut+lcOpr) .AND. llUpdate
  IF SEEK(IIF(lcTyp="P","I","M")+lcCut+lcOpr) .AND. llUpdate
  *B804272,1 SSH 07/12/2001 (End)

    SCAN REST WHILE cimtyp+ctktno+coprcode+clotno+trancd  = ;
                      IIF(lcTyp="P","I","M")+lcCut+lcOpr FOR Item = lcSty
      SCAT MEMVAR MEMO
      SELECT(lcOprDt)
      IF !SEEK(m.cimtyp+m.ctktno+m.coprcode+m.clotno+m.trancd)
        APPEND BLANK
        GATH MEMVAR MEMO
      ENDIF
      DO CASE
        CASE  !llFrmNxt AND m.TranCd $ '12'
          FOR lnTmp = 1 TO 8
           lcTemp = STR(lnTmp,1)
           IF lnIss&lcTemp <> 0
             SELECT(lcOprDt)
             REPLACE nLotQty&lcTemp WITH lnIss&lcTemp,;
                      nLotTotQty    WITH lnTotIss
           ENDIF
          ENDFOR
        CASE llFrmNxt .AND. lnComTot < nLotTotQty
          FOR lnTmp = 1 TO 8
           lcTemp = STR(lnTmp,1)
           SELECT(lcOprDt)
           REPLACE nLotQty&lcTemp WITH lnCom&lcTemp,;
                   nLotTotQty     WITH lnComTot
          ENDFOR
      ENDCASE
    ENDSCAN
  ENDIF
  SELECT(lcOprDt)
  *--- cimtyp+ctktno+coprcode+clotno+trancd
  *- -- lcTyp+lcCut+lcSty+lcOpr
  
  *B804272,1 SSH 07/12/2001 (Begin) Adding the case of PO.
  IF SEEK(IIF(lcTyp="P","I","M")+lcCut+lcOpr) .AND. llUpdate
    SCAN REST WHILE cimtyp+ctktno+coprcode+clotno+trancd  = ;
                      IIF(lcTyp="P","I","M")+lcCut+lcOpr FOR Item = lcSty
  *B804272,1 SSH 07/12/2001 (End)
      SCAT MEMVAR MEMO
      DO CASE
        CASE  !llFrmNxt .AND. m.TranCd $ '12'
          FOR lnTmp = 1 TO 8
           lcTemp = STR(lnTmp,1)
           IF lnIss&lcTemp <> 0
             SELECT(lcOprDt)
             REPLACE nLotQty&lcTemp WITH lnIss&lcTemp,;
                      nLotTotQty     WITH lnTotIss
           ENDIF
          ENDFOR
        CASE llFrmNxt .AND. lnComTot < nLotTotQty
          FOR lnTmp = 1 TO 8
           lcTemp = STR(lnTmp,1)
           SELECT(lcOprDt)
           REPLACE nLotQty&lcTemp WITH lnCom&lcTemp,;
                   nLotTotQty     WITH lnComTot
          ENDFOR
      ENDCASE
    ENDSCAN
  ENDIF
ENDFOR
SELECT(lnOld)

*!*************************************************************
*! Name      : lfcheckMod
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to check the frst next operation.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfcheckMod()
*!*************************************************************
*!
FUNCTION lfcheckMod
PRIVATE lnOldAls , llContinuo , lnCurrOpSq
*--- laFNxtOpr == Frst next operation
*--- 
lnOldAls = SELECT(0)
llContinuo = .F.
laFNxtOpr[1] = PADR(laFNxtOpr[1],6)
SELECT mfgoprhd
=SEEK(lcType+lcCutTkt+lcOprCode)
lnCurrOpSq = VAL(MfgOprHd.cOperSeq)

SELECT(lcTrkLin)
*--- type+cuttkt+style+coprcode+status+coperseq
IF SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+laFNxtOpr+'I')
   IF  TotQty > lnComTot
     llContinuo = .T.
   ENDIF
ELSE
   SELECT TrakLin
   IF SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+laFNxtOpr+'I')
     IF  TotQty > lnComTot
       llContinuo = .T.
     ENDIF
   ENDIF
ENDIF
IF llContinuo
  lcType = IIF(&lcTmpQuery..Type = 'C','M','I')
  =lfGetPre(&lcTmpQuery..Type,&lcTmpQuery..CutTkt,&lcTmpQuery..Style,'laNxtOp',.T.)
ENDIF
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfGetOld
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Get old value.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetOld()
*!*************************************************************
*!
FUNCTION  lfGetOld
*: B603772,1 SSH  Get old value
lcOldVal = EVAL(SYS(18))

*!*************************************************************
*! Name      : lfChekChng
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to check if any thing changed.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfChekChng()
*!*************************************************************
*!
FUNCTION lfChekChng

IF TYPE('EVAL(SYS(18))') <> TYPE('lcOldVal') .OR. ;
   (TYPE('EVAL(SYS(18))') = TYPE('lcOldVal') .AND. ;
   (EVAL(SYS(18)) = lcOldVal))
  RETURN .F.
ENDIF
*: B603772,1 SSH  


*!*************************************************************
*! Name      : lfChkDele
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to Delete next operation.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfChkDele()
*!*************************************************************
*!
FUNCTION lfChkDele
*: B603772,1 SSH  Prevent end date unless all previus date is ended

lnOldAls = SELECT(0)
=lfCkNxtIss(IIF(&lcTmpQuery..Type='C','M','I'),&lcTmpQuery..CutTkt,lcOprCode,&lcTmpQuery..Style,.T.)
SELECT (lcOprToDel)
SET ORDER TO lcToDel
IF SEEK(IIF(&lcTmpQuery..Type='C','M','I')+&lcTmpQuery..CutTkt+lcOprCode);
  .AND. (gfModalGen('QRM38200B00023','DIALOG') = 2)
  DELETE REST WHILE cimtyp+ctktno+cTrgOpr = ;
                    IIF(&lcTmpQuery..Type='C','M','I')+&lcTmpQuery..CutTkt+lcOprCode
  lnCom&lcInd = &lcTrkLin..Qty&lcInd
  SET ORDER TO (lcOprToDel)
  SELECT(lnOldAls)
  RETURN .F.
ENDIF
SET ORDER TO (lcOprToDel)
*: B603772,1 SSH  Prevent end date unless all previus date is ended


*!*************************************************************
*! Name      : lfGetStrt
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Function to get start time.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetStrt()
*!*************************************************************
*!
FUNCTION lfGetStrt
*: B603772,1 SSH  

lcUsrMsg1 = 'Start time'
DO gcScrDir+'MF\'+("Mfrbstti.SPR")

*!*************************************************************
*! Name      : lfVPenHour
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/00
*! Purpose   : Valid Function For start time ok button.
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : .....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVPenHour()
*!*************************************************************
*!
FUNCTION lfVPenHour
*: B603772,1 SSH  

PRIVATE lnOldAls
lnOldAls = SELECT(0)
*--- Type C(1),CutTkt C(6) , Style Operation
*--- type+cuttkt+style+coprcode+status+coperseq
lnCurrTime = VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2))
lnEndSec   = lnCurrTime - INT(lnCurrTime)
lnStaSec   = lnStartAt  - INT(lnStartAt)
IF lnStaSec > lnEndSec
  lnTotTime = (ABS((lnEndSec + (0.6 - lnStaSec))))*100
  lnStartAt =  IIF(lnStartAt + 1 > 23,1,lnStartAt + 1)
ELSE
  lnTotTime = (ABS(lnEndSec - lnStaSec))*100
ENDIF
IF INT(lnCurrTime) > INT(lnStartAt)
  *--- (End-(Start-12))
  lnTmpH = (INT(lnCurrTime) - (INT(lnStartAt)-12))
  lnTotTime = lnTotTime + (IIF(lnTmpH>12,lnTmpH-12,lnTmpH)*60)
ELSE
  lnTmpH = (INT(lnCurrTime) - INT(lnStartAt))
  lnTotTime = lnTotTime + (IIF(lnTmpH>12,lnTmpH-12,lnTmpH)*60)
ENDIF
=SEEK(&lcTmpQuery..Type+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation,'TRAKHDR')
SELECT TRAKHDR
REPLACE nhours WITH nhours + lnTotTime
lnIssReal = INT(nhours) + ((nhours - INT(nhours))/0.6)
=lfUpBom(TrakHdr.Type,TrakHdr.CutTkt,TrakHdr.Style,TrakHdr.cOprCode,lnIssReal)
IF laSetups[2,2]='Y'
  lcGlDTemp = gfTempName()
  =gfOpenFile(gcDataDir+'GLDIST','GLDISTAc','SH')
  SELECT GLDIST
  IF !USED(lcGlDTemp)
    COPY STRUCTURE TO (gcWorkDir+lcGlDTemp)
    =gfOpenFile(gcWorkDir+lcGlDTemp,'','EX')
  ELSE
    SELECT(lcGlDTemp)
  ENDIF
  ZAP
  DECLARE laMfgRFld[2,2]
  STORE ''  TO laMfgRFld,lcMfgGlAcnt
  laMfgRFld[1,1] = 'LEADTIME'
  laMfgRFld[1,2] = 'lnLeadTime'
  laMfgRFld[2,1] = 'GLACCOUNT'
  laMfgRFld[2,2] = 'lcMfgGlAcnt'
  =gfRltFld(TrakHdr.coprcode,@laMfgRFld,'MFGCODE')
  *--- Liability for non material cost ( 018 )
  ldIssDate  = gdSysDate
  lcGlYear   = ''
  lcGlPeriod = ''
  lnTempHrs  =  INT(TrakHdr.nhours) + ((TrakHdr.nhours - INT(TrakHdr.nhours))/0.6)
  IF lnTempHrs <> 0 AND !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
    RETURN
  ENDIF
  IF TrakHdr.Type = 'C'
    lcLinkCode = IIF(SEEK(TrakHdr.CutTkt,'CutTktH'),CutTktH.Link_Code,'DEFDEF')
  ELSE
    lcLinkCode = IIF(SEEK('P'+TrakHdr.CutTkt,'PoSHdr'),PoSHdr.Link_Code,'DEFDEF')
  ENDIF
  DO GLDIST WITH lcLinkCode,'018',-(CtktBom.untcost*lnTempHrs),;
                 'NL',TrakHdr.CutTkt,ldIssDate,lcGlYear,lcGlPeriod,lcGlDTemp,lcMfgGlAcnt
  *---W.I.P. ( 013 )
  DO GLDIST WITH lcLinkCode,'013',(CtktBom.untcost*lnTempHrs),;
                 'NL',TrakHdr.CutTkt,ldIssDate,lcGlYear,lcGlPeriod,lcGlDTemp
  m.cWipAcnt = &lcGlDTemp..GlAccount
  lcISession = gfsequence('GLSession')
  SELECT (lcGlDTemp)
  REPLACE ALL GlSession WITH lcISession
  SELECT GLDIST
  APPEND FROM (gcWorkDir+lcGlDTemp)
  SELECT (lcGlDTemp)
  *--- lcGlDTemp
  USE IN IIF(USED(lcGlDTemp),lcGlDTemp,0)
  ERASE &gcWorkDir.&lcGlDTemp+'.DBF'
  ERASE &gcWorkDir.&lcGlDTemp+'.CDX'
ENDIF

CLEAR READ

*:*************************************************************
*: Name      : lfIssTime.   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Function to stamp start and end time for each scan.
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfIssTime()
*:*************************************************************
FUNCTION lfIssTime
PARAMETER lcIssComp  , lnIssQty , lnCompQty , lnPenQty,lcType , lcCut , lcOper , lcSty , llUpdQty

PRIVATE lnOldAls
*--- lcType,lcCut,lcSty,lcOper,lcScal,lcIss,llStat,llUpdBrow
lnOldAls = SELECT(0)
IF !USED(lcTrIssTim)
  =gfOpenFile(gcWorkDir+lcTrIssTim,'','EX')
  INDEX ON type+cuttkt+mfgcode+style+status+DTOS(diss_date) TAG (lcTrIssTim)
ENDIF
*--- INDEXED ON type+cuttkt+mfgcode+style+status+DTOS(diss_date)
SELECT(lcTrIssTim)
IF !SEEK(lcType+lcCut+lcOper+lcSty+IIF(lcIssComp='M',m.Status,lcIssComp)+DTOS(gdSysDate))
  APPEND BLANK
  REPLACE Type      WITH lcType,;
          CutTkt    WITH lcCut,;
          Style     WITH lcSty,;
          mfgCode   WITH lcOper,;
          Status    WITH IIF(lcIssComp='M',m.Status,lcIssComp),;
          nStart    WITH VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2)),;
          ID        WITH lcUserId,;
          dIss_Date WITH gdSysDate
ENDIF
REPLACE Issue_Qty WITH Issue_Qty+lnIssQty,;
        nQty_Comp WITH nQty_Comp+lnCompQty,;
        nQty_Pen  WITH nQty_Pen + lnPenQty,;
        nEnd      WITH VAL(SUBSTR(TIME(),1,2)+'.'+SUBSTR(TIME(),4,2))
IF lcIssComp="M"
  REPLACE cStatus WITH "M"
ENDIF
IF llUpdQty
  FOR lnInd = 1 TO 8
    lcInd = STR(lnInd,1)
    REPLACE Qty&lcInd WITH IIF(lcIssComp='W',lnWIss&lcInd,IIF(lcIssComp='T',lnwCom&lcInd,lnWPen&lcInd))
  ENDFOR
ENDIF
SELECT(lnOldAls)


*:*************************************************************
*: Name      : lfUpdIssTm.   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Function to save stamp start and end time for each scan.
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUpdIssTm()
*:*************************************************************
FUNCTION lfUpdIssTm

PRIVATE lnOldAls
lnOldAls = SELECT(0)
IF USED(lcTrIssTim)
  SELECT(lcTrIssTim)
  GOTO TOP
  SCAN
    SCATTER MEMVAR MEMO
    SELECT Trstrend
    IF !SEEK(m.type+m.cuttkt+m.mfgcode+m.style+m.status+DTOS(m.diss_date))
       IF (m.Issue_Qty > 0 .OR. m.nQty_Comp > 0 .OR. m.nQty_Pen > 0)
         INSERT INTO Trstrend FROM MEMVAR
       ENDIF
    ELSE
      IF m.cStatus == "M"
        REPLACE Issue_Qty WITH m.Issue_Qty,;
                nQty_Comp WITH m.nQty_Comp,;
                nQty_Pen  WITH m.nQty_Pen
      ELSE
        lnCompToUp = nQty_Comp + m.nQty_Comp
        lnIssToUp  = Issue_Qty + m.Issue_Qty
        lnPenToUp  = nQty_Pen  + m.nQty_Pen
        REPLACE Issue_Qty WITH IIF(lnIssToUp<0,-1 * lnIssToUp,lnIssToUp),;
                nQty_Comp WITH IIF(lnCompToUp < 0 ,-1 *lnCompToUp , lnCompToUp),;
                nQty_Pen  WITH IIF(lnPenToUp < 0 ,-1 *lnPenToUp , lnPenToUp)
        FOR lnInd = 1 TO 8
          lcInd = STR(lnInd,1)
          REPLACE Qty&lcInd WITH m.Qty&lcInd
        ENDFOR
      ENDIF
    ENDIF
    FLUSH
  ENDSCAN
ENDIF
SELECT(lnOldAls)

*:*************************************************************
*: Name      : lfvRework.   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Valid rework function.
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvRework()
*:*************************************************************
FUNCTION lfvRework

SELECT TrstrEnd

*--- Issue ReWork

IF SEEK(&lcTmpQuery..Type + &lcTmpQuery..CutTkt + &lcTmpQuery..Operation + &lcTmpQuery..Style+'W' + DTOS(gdSysDate))
  llDummy = lfIssTime("W", lnWTotIss - Issue_Qty,0 ,0, &lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style,.T.)
ELSE
*  IF SEEK(&lcTmpQuery..Type + &lcTmpQuery..CutTkt + &lcTmpQuery..Operation + &lcTmpQuery..Style+'W')
*    IF lnWTotIss - Issue_Qty <> 0
*      =lfIssTime("W", lnWTotIss - Issue_Qty ,0 ,0, &lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style,.T.)
*    ENDIF
*  ELSE
    =lfIssTime("W",lnWTotIss,0 ,0, &lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style,.T.)
*  ENDIF
ENDIF

SELECT TrstrEnd

*--- Complete ReWork

IF SEEK(&lcTmpQuery..Type + &lcTmpQuery..CutTkt + &lcTmpQuery..Operation + &lcTmpQuery..Style+'T' + DTOS(gdSysDate))
  =lfIssTime("T",0,lnWComTot - nQty_Comp , 0 ,&lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style,.T.)
ELSE
*  IF SEEK(&lcTmpQuery..Type + &lcTmpQuery..CutTkt + &lcTmpQuery..Operation + &lcTmpQuery..Style+'T')
*    IF lnWComTot - nQty_Comp <> 0
*      =lfIssTime("T", 0 ,lnWComTot - nQty_Comp ,0, &lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style,.T.)
*    ENDIF
*  ELSE
    =lfIssTime("T",0,lnWComTot ,0, &lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style,.T.)
*  ENDIF
ENDIF

SELECT TrstrEnd

*--- Pending ReWork

*--- type+cuttkt+mfgcode+style+status+DTOS(diss_date)
IF SEEK(&lcTmpQuery..Type + &lcTmpQuery..CutTkt + &lcTmpQuery..Operation + &lcTmpQuery..Style+'D' + DTOS(gdSysDate))
  =lfIssTime("D",0,0 , lnWPenTot - nQty_Pen,&lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style,.T.)
ELSE
  IF SEEK(&lcTmpQuery..Type + &lcTmpQuery..CutTkt + &lcTmpQuery..Operation + &lcTmpQuery..Style+'D')
    LOCATE REST WHILE type+cuttkt+mfgcode+style+status+DTOS(diss_date)=;
                      &lcTmpQuery..Type + &lcTmpQuery..CutTkt + &lcTmpQuery..Operation + &lcTmpQuery..Style+'D';
                FOR nQty_Pen <> 0
    IF nQty_Pen <> lnWPenTot
      IF nQty_Pen = 0
        =lfIssTime("D", 0 ,0 ,lnWPenTot, &lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style,.T.)
      ELSE
        ldTempDate = diss_date
        =lfIssTime("D",0,0 , lnWPenTot - nQty_Pen,&lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style,.T.)
        SELECT(lcTrIssTim)
        REPLACE diss_date WITH ldTempDate
        =lfIssTime("W", lnWPenTot - nQty_Pen,0 ,0, &lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style,.T.)
        SELECT TrstrEnd
        =SEEK(&lcTmpQuery..Type + &lcTmpQuery..CutTkt + &lcTmpQuery..Operation + &lcTmpQuery..Style+'D')
        LOCATE REST WHILE type+cuttkt+mfgcode+style+status+DTOS(diss_date)=;
                          &lcTmpQuery..Type + &lcTmpQuery..CutTkt + &lcTmpQuery..Operation + &lcTmpQuery..Style+'D';
                    FOR nQty_Pen <> 0
        SELECT(lcTrIssTim)
        FOR lnInd = 1 TO 8
          lcInd = STR(lnInd,1)
          REPLACE Qty&lcInd WITH TrstrEnd.Qty&lcInd
        ENDFOR
      ENDIF
    ENDIF
  ELSE
    =lfIssTime("D",0,0 ,lnWPenTot, &lcTmpQuery..Type , &lcTmpQuery..CutTkt , &lcTmpQuery..Operation , &lcTmpQuery..Style,.T.)
  ENDIF
ENDIF

FUNCTION lfWModify

lcType = &lcTmpQuery..Type
*: B803342,1 SSH Enable / Disable issued field
IF !EMPTY(&lcTmpQuery..scal)
  *--- type+cuttkt+style+coprcode+status+coperseq
  lcSiz = &lcTmpQuery..scal
  llFnd = (SEEK(lcType+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+"I",'TRAKLIN'));
         .AND.   TRAKLIN.Qty&lcSiz <> 0
ELSE
  llFnd = SEEK(lcType+&lcTmpQuery..CutTkt+&lcTmpQuery..Style+&lcTmpQuery..Operation+"I",'TRAKLIN')
ENDIF
lcEnbDis =  IIF((!EMPTY(&lcTmpQuery..CutTkt).AND. llFnd),'DISABLE','ENABLE')
lcEnbDis = IIF(llAdmin ,'ENABLE',lcEnbDis)
FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  SHOW GET lnIss&lcInd &lcEnbDis
ENDFOR
*: B803342,1 SSH [END]
SELECT TrakHdr
lcOldKey = type+cuttkt+style+coprcode+id
=SEEK(lcType + &lcTmpQuery..CutTkt)

*C124152,1 NNA 11/09/2004 (BEGIN) Locate also for STATUS = 'C' beside nHours <> 0 to make
*C124152,1 NNA            pending and completed fields enable 
*LOCATE REST WHILE type+cuttkt+style+coprcode+id = lcType + &lcTmpQuery..CutTkt;
            FOR coprcode == &lcTmpQuery..Operation .AND. nHours <> 0
LOCATE REST WHILE type+cuttkt+style+coprcode+id = lcType + &lcTmpQuery..CutTkt+;
            &lcTmpQuery..Style + &lcTmpQuery..Operation FOR nHours <> 0 AND STATUS = 'C'
*C124152,1 NNA (END)

IF FOUND()
  SHOW GETS DISABLE
  SHOW GET pbOk ENABLE
  SHOW GET PbCancel ENABLE
  IF TrakHdr.dIss_Date <> gdSysDate
    FOR lnInd = 1 TO 8
      lcInd = STR(lnInd,1)
      IF !(lnWPen&lcInd <> 0 .AND. lnWIss&lcInd = 0)
        SHOW GET lnWIss&lcInd ENABLE
        SHOW GET lnWCom&lcInd ENABLE        
      ENDIF    
      SHOW GET lnWPen&lcInd ENABLE
    ENDFOR
  ENDIF
ENDIF
=SEEK(lcOldKey)
IF llAdmin
  SHOW GET lnHours   ENABLE
  SHOW GET lcUserId  ENABLE
ELSE
  SHOW GET lnHours  DISABLE
  SHOW GET lcUserId DISABLE
ENDIF

*B804272,1 SSH 07/12/2001 (Begin) Adding the case of PO.

IF SEEK(IIF(lcType $ 'C','M','I')+&lcTmpQuery..CutTkt+SPACE(19)+SPACE(6)+&lcTmpQuery..Operation,'CtktBom')
*B804272,1 SSH 07/12/2001 (End)
  
  *C124152,1 NNA 11/09/2004 (Begin) if This Administrator
  *IF llAdmin .AND. SEEK(CtktBom.Typ+IIF(lcType $ 'C','M','I')+&lcTmpQuery..CutTkt+SPACE(19)+SPACE(6)+&lcTmpQuery..Operation,'BomCost')
  IF llAdmin 
  *C124152,1 NNA (End)
  
    SHOW GET lnHours  ENABLE
  ELSE
    SHOW GET lnHours  DISABLE
  ENDIF
ELSE
  SHOW GET lnHours  DISABLE
ENDIF

*:*************************************************************
*: Name      : lfCalcTim   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Calculate time.
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfCalcTim()   New Calculate funciton
*:*************************************************************
FUNCTION lfCalcTim

PRIVATE lnAlias,lcCurrId

lnAlias = SELECT(0)

lcScnedCut = gfTempName()
CREATE TABLE &gcWorkDir.&lcScnedCut (Type C(1) , CutTkt C(06),Style C(19),NComp N(12),mfgcode C(6))
INDEX ON Type+CutTkt+Style+mfgcode TAG &lcScnedCut

llBomByMe = gfOpenFile(gcDataDir+'bom','bom','SH')
SELECT TrStrEnd
SET ORDER TO TrDate
FOR lnEndInd  = 1 TO ALEN(laDateRng)
  ldDatToEnd = laDateRng[lnEndInd]
  IF !SEEK(DTOS(ldDatToEnd))
    =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'No transaction found for date '+DTOC(ldDatToEnd)+', Nothing to update')
  ELSE
    IF FLAG = 'Y'
      =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Date '+DTOC(ldDatToEnd)+' has been already ended, Nothing to update.')
    ELSE
      IF gfModalGen('TRM00000B00006','DIALOG',.F.,.F.,'Day '+ DTOC(ldDatToEnd) + ' will be closed. You will not be able to issue or complete transaction for that date. Are you sure you want to continue?')=1
        =lfProceed()
      ENDIF
    ENDIF
  ENDIF
ENDFOR
SELECT(lnAlias)

*:*************************************************************
*: Name      : lfUpdHour   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Function to update trakhdr hourse .
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUpdHour()
*:*************************************************************
FUNCTION lfUpdHour

SELECT TrakHdr
SET ORDER TO TrakId
=SEEK(lcCurrId)
SCAN REST WHILE Id+DTOS(dIss_date)+style+coprcode = ;
                lcCurrId;
          FOR   Status $ 'PC' .AND. coprcode = lcTmpOpr;
                .AND. SEEK(Type+CutTkt+Style+lcTmpOpr,lcScnedCut)
  SELECT BOM
  =SEEK(SUBSTR(TrakHdr.Style,1,lnMajLen))
  LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=SUBSTR(TrakHdr.Style,1,lnMajLen);
              FOR   mfgCode = TrakHdr.cOprCode
  lnEstTim = IIF(FOUND(),BOM.nEstBomQty,1)
  SELECT TrakHdr
  lnQtyTiDis = IIF(&lcScnedCut..nComp<>0,&lcScnedCut..nComp,nQty_Comp)

  *B129938,1 NNA 12/07/2005 (Begin) Prevent End Process option from updating in the Actual Time
  *lnTime = (lnTotTime*((lnQtyTiDis*lnEstTim)/IIF(lnQtyEst<>0,lnQtyEst,1)))
  *REPLACE nhours WITH nhours+INT(lnTime/60)+((lnTime-INT(lnTime/60)*60)/100)
  *lnHrsCost = INT(lnTime/60)+((lnTime-INT(lnTime/60)*60)/100)
  lnHrsCost = TrakHdr.nHours
  *B129938,1 NNA (END)

  IF nhours < 0 .OR. nhours = 0
    SELECT TrakHdr
    REPLACE nhours WITH 0
    WAIT WINDOW "Zero calculated hours for user: "+lcCurrId +" / operation: " +lcTmpOpr +". Nothing to update.!"
  ELSE
    *--- PARAMETER lcType,lcCut,lcSty,lcOper,lnIssQty
    lnIssReal = INT(lnHrsCost) + ((lnHrsCost - INT(lnHrsCost))/0.6)
    =lfUpBom(TrakHdr.Type,TrakHdr.CutTkt,TrakHdr.Style,TrakHdr.cOprCode,lnIssReal)
    =lfUHrCost()
  ENDIF
ENDSCAN

*:*************************************************************
*: Name      : lfUHrCost   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Function to update trakhdr hourse cost.
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUHrCost()
*:*************************************************************
FUNCTION lfUHrCost

IF laSetups[2,2]='Y'
  lcGlDTemp = gfTempName()
  =gfOpenFile(gcDataDir+'GLDIST','GLDISTAc','SH')
  SELECT GLDIST
  IF !USED(lcGlDTemp)
    COPY STRUCTURE TO (gcWorkDir+lcGlDTemp)
    =gfOpenFile(gcWorkDir+lcGlDTemp,'','EX')
  ELSE
    SELECT(lcGlDTemp)
  ENDIF
  ZAP
  DECLARE laMfgRFld[2,2]
  STORE ''  TO laMfgRFld,lcMfgGlAcnt
  laMfgRFld[1,1] = 'LEADTIME'
  laMfgRFld[1,2] = 'lnLeadTime'
  laMfgRFld[2,1] = 'GLACCOUNT'
  laMfgRFld[2,2] = 'lcMfgGlAcnt'
  =gfRltFld(TrakHdr.coprcode,@laMfgRFld,'MFGCODE')
  *--- Liability for non material cost ( 018 )
  ldIssDate  = ldDatToEnd
  lcGlYear   = ''
  lcGlPeriod = ''
  lnTempHrs  =  lnHrsCost
  IF (CtktBom.untcost*lnTempHrs) <> 0 AND !CHECKPRD(ldIssDate,'lcGlYear','lcGlPeriod','IA')
    RETURN
  ENDIF

  IF TrakHdr.Type = 'C'
    lcLinkCode = IIF(SEEK(TrakHdr.CutTkt,'CutTktH'),CutTktH.Link_Code,'DEFDEF')
  ELSE
    lcLinkCode = IIF(SEEK('P'+TrakHdr.CutTkt,'PoSHdr'),PoSHdr.Link_Code,'DEFDEF')
  ENDIF
  DO GLDIST WITH lcLinkCode,'018',-(CtktBom.untcost*lnTempHrs),;
                 'NL',TrakHdr.CutTkt,ldIssDate,lcGlYear,lcGlPeriod,lcGlDTemp,lcMfgGlAcnt
  *---W.I.P. ( 013 )
  DO GLDIST WITH lcLinkCode,'013',(CtktBom.untcost*lnTempHrs),;
                 'NL',TrakHdr.CutTkt,ldIssDate,lcGlYear,lcGlPeriod,lcGlDTemp
  m.cWipAcnt = &lcGlDTemp..GlAccount
  lcISession = gfsequence('GLSession')
  SELECT (lcGlDTemp)
  REPLACE ALL GlSession WITH lcISession
  SELECT GLDIST
  APPEND FROM (gcWorkDir+lcGlDTemp)
  SELECT (lcGlDTemp)
  *--- lcGlDTemp
  USE IN IIF(USED(lcGlDTemp),lcGlDTemp,0)
  ERASE &gcWorkDir.&lcGlDTemp+'.DBF'
  ERASE &gcWorkDir.&lcGlDTemp+'.CDX'
ENDIF


*:*************************************************************
*: Name      : lfvpbok   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Valid function for ok button in date rang screen.
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUHrCost()
*:*************************************************************
FUNCTION lfvpbok
IF ldTo = {}
  ldTo = gdSysDate
ENDIF
IF ldFrom > ldTo
  WAIT WINDOW "Invalid date range"
  _CUROBJ=OBJNUM(ldTo)
  RETURN .F.
ELSE
  CLEAR READ
  DIMENSION laDateRng[1]
  =lfCreateAr()
  =lfCalcTim()
ENDIF

*:*************************************************************
*: Name      : lfdateRang   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Date range function.
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfdateRang()   New Calculate funciton
*:*************************************************************
FUNCTION lfdateRang

lcTitle = "End date range"
ldFrom = gdSysDate
ldTo   = gdSysDate
DO gcScrDir+'MF\'+("DateRng.SPR")

*:*************************************************************
*: Name      : lfvSDate   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Valid start date .
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvSDate()
*:*************************************************************
FUNCTION lfvSDate

IF ldFrom > ldTo
  WAIT WINDOW "Invalid date range"
  _CUROBJ=OBJNUM(ldTo)
ENDIF

*:*************************************************************
*: Name      : lfCreateAr   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Function to create array thet hold dates to be closed .
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfCreateAr()
*:*************************************************************
FUNCTION lfCreateAr
PRIVATE lnCount
laDateRng [1] = ldFrom
lnCount = 2
DO WHILE ldFrom < ldTo
  DIMENSION laDateRng[lnCount]
  laDateRng [lnCount] = laDateRng [lnCount-1]+1
  IF laDateRng [lnCount] = ldTo
    EXIT
  ENDIF
  lnCount = lnCount + 1
ENDDO

*:*************************************************************
*: Name      : lfProceed   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Function calculate distribute hourse .
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfProceed()
*:*************************************************************
FUNCTION lfProceed
PRIVATE lnOldAls

lnOldAls = SELECT(0)
lnMajLen  = LEN(gfItemMask("PM"))
lnTotTime = 0
lnEnd     = 0
lnStart   = 0
lnQtyEst  = 0
lcTotComp = 0
lnTotDaily=0
lnStart   = 99.99
DO WHILE !EOF() .AND.  DTOS(diss_date)+id+mfgcode+status = DTOS(ldDatToEnd)
  *--- TrkHdr Key :: type+cuttkt+style+coprcode+id
  lcCurrId = ID
  lcTmpOpr = MfgCode
  =SEEK(DTOS(ldDatToEnd)+lcCurrId+lcTmpOpr)
  SCAN REST WHILE DTOS(diss_date)+id+mfgcode+status = ;
                  DTOS(ldDatToEnd)+lcCurrId+lcTmpOpr

    *B129938,1 NNA 12/07/2005 (Begin) Scan for not canceled cut tickets
    IF SEEK(Trstrend.CUTTKT,'Cuttkth') AND Cuttkth.STATUS='X'
      LOOP
    ENDIF
    *B129938,1 NNA (END)
    
    REPLACE FLAG WITH "Y"
    IF !SEEK(Type+CutTkt+Style+mfgcode,lcScnedCut)
      SELECT (lcScnedCut)
      APPEND BLANK
      REPLACE Type    WITH TrStrEnd.Type,;
              CutTkt  WITH TrStrEnd.CutTkt,;
              Style   WITH TrStrEnd.Style,;
              mfgcode WITH TrStrEnd.mfgcode,;
              nComp   WITH nComp+Trstrend.nQty_Comp
      SELECT Trstrend
    ENDIF

    DO CASE
      CASE Status $ 'PIW'
        lnStart   = IIF(nStart<lnStart,nStart,lnStart)
      CASE Status $ 'TC'
        IF Status = 'T'
          SELECT (lcScnedCut)
          REPLACE nComp WITH nComp+Trstrend.nQty_Comp
          SELECT Trstrend  
        ENDIF  
        lcTotComp = nQty_Comp
        SELECT BOM
        =SEEK(SUBSTR(TrakLin.Style,1,lnMajLen))
        LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr=;
                          SUBSTR(TrStrEnd.Style,1,lnMajLen);
                    FOR   mfgCode = lcTmpOpr
        lnEstTim = IIF(FOUND(),BOM.nEstBomQty,1)
        lnQtyEst = lnQtyEst+(lnEstTim*lcTotComp)
        lnEnd    = IIF(TrStrEnd.nEnd>lnEnd,TrStrEnd.nEnd,lnEnd)
    ENDCASE
  ENDSCAN
  lnEndSec = lnEnd   - INT(lnEnd)
  lnStaSec = lnStart - INT(lnStart)
  IF lnStaSec > lnEndSec
    lnTotTime = (ABS((lnEndSec + (0.6 - lnStaSec))))*100
    lnStart   =  IIF(lnStart + 1 > 23,1,lnStart + 1)
  ELSE
   lnTotTime = (ABS(lnEndSec - lnStaSec))*100
  ENDIF
  IF INT(lnStart) > INT(lnEnd)
    lnTmpH = 12 + (INT(lnEnd) - INT(lnStart))
  ELSE
    lnTmpH = (INT(lnEnd) - INT(lnStart))
  ENDIF
  lnTotTime = lnTotTime + (IIF(lnTmpH>12,lnTmpH-12,lnTmpH)*60)

  IF lnTotTime <> 0
    =lfUpdHour()
  ENDIF
  lnTotTime = 0
  lnEnd     = 0
  lnStart   = 0
  lnQtyEst  = 0
  lcTotComp = 0
  lnStart   = 99.99
  SELECT TrStrEnd
ENDDO
SELECT(lnOldAls)


*:*************************************************************
*: Name      : lfVRwIss   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Valid Issue Rework .
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfVRwIss()
*:*************************************************************
FUNCTION lfVRwIss


IF !lfChekChng()
  RETURN
ENDIF
llChang = .T.
lcInd = SUBSTR(SYS(18),7,1)
IF EMPTY(lcInd) OR !(lcInd$"12345678")
  RETURN
ENDIF
IF EVAL(SYS(18)) <0
  WAIT WINDOW "Cannot accept negative value."
  lnWIss&lcInd = lcOldVal
  lcCurObj = SYS(18)
  _CUROBJ=OBJNUM(lcCurObj)
  SHOW GET lnWIss&lcInd
  RETURN
ENDIF

SELECT (lcTrkLin)
=SEEK(&lcTrkHdr..Type+&lcTrkHdr..CutTkt+&lcTrkHdr..Style+&lcTrkHdr..cOprCode+'C')

IF BETWEEN(lcInd,'1','8') AND EVAL(SYS(18)) >  Qty&lcInd
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Issued rework cannot be greater than completed quantity.')
  lnWIss&lcInd = lcOldVal
  lcCurObj = SYS(18)
  _CUROBJ=OBJNUM(lcCurObj)
ENDIF
lnWTotIss = lnWIss1+lnWIss2+lnWIss3+lnWIss4+lnWIss5+lnWIss6+lnWIss7+lnWIss8
SELECT (lcTrkLin)
SHOW GET lnWTotIss

lnNet&lcInd = lnCom&lcInd - (lnWIss&lcInd - lnWCom&lcInd)
lnTotNet = 0
FOR lnInd = 1 TO 8
  lcNInd = STR(lnInd,1)
  lnTotNet = lnTotNet+lnNet&lcNInd
ENDFOR
SHOW GET lnTotNet
SHOW GET lnNet&lcInd

*:*************************************************************
*: Name      : lfvRwComp   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Valid Complete Rework .
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvRwComp()
*:*************************************************************
FUNCTION lfvRwComp

PRIVATE lnOldAls
lnOldAls = SELECT(0)
IF !lfChekChng()
  RETURN
ENDIF
llChang = .T.

lcInd = SUBSTR(SYS(18),7,1)
IF EMPTY(lcInd) OR !(lcInd$"12345678")
  SELECT(lnOldAls)
  RETURN
ENDIF
IF EVAL(SYS(18)) <0
  WAIT WINDOW "Cannot accept negative value."
  lnWCom&lcInd = lcOldVal
  lcCurObj = SYS(18)
  _CUROBJ=OBJNUM(lcCurObj)
  SHOW GET lnWCom&lcInd
  SELECT(lnOldAls)
  RETURN
ENDIF

IF (EVAL(SYS(18)) + EVAL('lnWPen'+lcInd)) > EVAL('lnWIss'+lcInd)
  *--- "Pending Quantity and Completed can not Excced Issued Qty"
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'Pending rework Quantity and Completed rework Quantity can not Excced Issued rework Quantity.')
  lnWCom&lcInd = lcOldVal
  lcCurObj = SYS(18)
  _CUROBJ=OBJNUM(lcCurObj)
ELSE
  lnWComTot = lnWCom1+lnWCom2+lnWCom3+lnWCom4+lnWCom5+lnWCom6+lnWCom7+lnWCom8
  SHOW GET lnWComTot
ENDIF
SELECT(lnOldAls)

lnNet&lcInd = lnCom&lcInd - (lnWIss&lcInd - lnWCom&lcInd)
lnTotNet = 0
FOR lnInd = 1 TO 8
  lcNInd = STR(lnInd,1)
  lnTotNet = lnTotNet+lnNet&lcNInd
ENDFOR
SHOW GET lnTotNet
SHOW GET lnNet&lcInd

*:*************************************************************
*: Name      : lfVRwPen   *: B603835,1 SSH
*: Developer : Ahmed Salah Shalaby - (SSH)
*: Date      : 28/08/00
*: Purpose   : Valid Pending Rework .
*:*************************************************************
*: Calls     : Screen MFRBRBW.SPX
*:             lfvUsrPass()
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfVRwPen()
*:*************************************************************
FUNCTION lfVRwPen

IF !lfChekChng()
  RETURN
ENDIF
llChang = .T.

lcInd = SUBSTR(SYS(18),7,1)
IF EMPTY(lcInd) OR !(lcInd$"12345678")
  RETURN
ENDIF
IF EVAL(SYS(18)) <0
  WAIT WINDOW "Cannot accept negative value."
  lnWPen&lcInd = lcOldVal
  lcCurObj = SYS(18)
  _CUROBJ=OBJNUM(lcCurObj)
  SHOW GET lnWPen&lcInd
  RETURN
ENDIF
IF (EVAL(SYS(18)) + EVAL('lnWCom'+lcInd)) > EVAL('lnWIss'+lcInd)
  *--- "Pending Quantity and Completed can not Excced Issued Qty"
  =gfModalGen('INM38199B00000','DIALOG')
  lnWPen&lcInd = lcOldVal
  lcCurObj = SYS(18)
  _CUROBJ=OBJNUM(lcCurObj)
ELSE
  lnWPenTot = lnWPen1+lnWPen2+lnWPen3+lnWPen4+lnWPen5+lnWPen6+lnWPen7+lnWPen8
  SHOW GET lnWPenTot
ENDIF
lnNet&lcInd = lnCom&lcInd - (lnWIss&lcInd - lnWCom&lcInd)
lnTotNet = 0
FOR lnInd = 1 TO 8
  lcNInd = STR(lnInd,1)
  lnTotNet = lnTotNet+lnNet&lcNInd
ENDFOR
SHOW GET lnTotNet
SHOW GET lnNet&lcInd

*-- End of Function lfVRwPen.
*!*************************************************************
*! Function  : lfCrtTxt
*! Developer : NADER NABIL (NNA)
*! Date      : 11/07/2004
*! Purpose   : - Create a text file (TrakLog.txt) to Append Tracking
*!             - Tickets notes to TrakLog file. 
*!*************************************************************
*!C124152,1 NNA
FUNCTION lfCrtTxt

IF !llOpenRep  
  llOpenRep = .T.
  lcFilHandl = FCREAT('&gcWorkDir.TrakLog.txt',0)
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,"*              TRACKING TICKETS'S PROBLEMS REPORT                  *")
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,' ')
  =FPUTS(lcFilHandl,"Skiped Tickets" + REPLICATE(' ',15) + "Opened Operations")
  =FPUTS(lcFilHandl,"--------------" + REPLICATE(' ',15) + "-----------------")
  =FPUTS(lcFilHandl,' ')

ENDIF
SELECT (lcTmpLog)
LOCATE
lcCtktNo =''
lnSerial = 1
SCAN
  IF !(&lcTmpLog..CtktNo == lcCtktNo) AND lnSerial > 1
    =FPUTS(lcFilHandl , REPLICATE('-',68))
  ENDIF
  =FPUTS(lcFilHandl , IIF(!(&lcTmpLog..CtktNo == lcCtktNo),;
        STR(lnSerial,2,1)+ ' - ' + &lcTmpLog..CtktNo,'') + IIF(!(&lcTmpLog..CtktNo == lcCtktNo),;
        REPLICATE(' ',25) , REPLICATE(' ',36)) + &lcTmpLog..CoprCode)
  IF !(&lcTmpLog..CtktNo == lcCtktNo)
    lnSerial = lnSerial + 1
    lcCtktNo = &lcTmpLog..CtktNo
  ENDIF
ENDSCAN    

*-- End of Function lfCrtTxt.
*!*************************************************************
*! Function  : lfViewRLog
*! Developer : NADER NABIL (NNA)
*! Date      : 11/07/2004
*! Purpose   : - Preview Tracking Tickets log file (TrakLog.txt). 
*!*************************************************************
*!C124152,1 NNA
FUNCTION lfViewRLog

*** If there is a report created dispaly it to the user
IF llOpenRep
  =FFLUSH(lcFilHandl)
  * Close the text file
*  DO WHILE !FCLOSE(lcFilHandl)
*  ENDDO
  =FCLOSE(lcFilHandl)
  CREATE CURSOR TMPLOG (mStrRep M(10))
  APPEND BLANK
  APPEND MEMO mStrRep FROM (gcWorkDir+"TrakLog.txt") OVERWRITE
  DO (gcScrDir + gcWinAppl + '\MFTRKLOG.SPR')
  USE IN TMPLOG
ELSE
  =gfModalGen("TRM00366B00000" , "DIALOG")   
ENDIF

*-- End of Function lfViewRLog.
*!*************************************************************
*! Name      : lfPrnt
*! Developer : NADER NABIL (NNA)
*! Date      : 11/07/2004
*! Purpose   : Print the Tracking Tickets log report
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfPrnt()
*!*************************************************************
*!C124152,1 NNA
FUNCTION lfVPrnt

IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPLOG.mStrRep TO &gcOutFile
   SELECT (gcOutFile)
   gcDevice = 'PRINTER'
   DO ENDREPORT
   gcDevice = 'SCREEN'
ENDIF
*-- End of Function lfVPrnt.

*!*************************************************************
*! Name      : lfvActTim
*! Developer : NADER NABIL (NNA)
*! Date      : 11/07/2004
*! Purpose   : Update the actual time if there is pending pieces
*!*************************************************************
*! Calls     : from the MfactTim screen
*!*************************************************************
*! Returns                : lnHours
*!*************************************************************
*! Example   : = lfvActTim()
*!*************************************************************
*!C124152,1 NNA
FUNCTION lfvActTim
IF lnUserHrs <= 0
   SHOW GET pbTotalOk  DISABLE
  _CUROBJ=OBJNUM(lnUserHrs)  
ELSE
  SHOW GET pbTotalOk  ENABLE
ENDIF
RETURN
*-- End of Function lfvActTim.
*!*******************************************************************************
*! Name      : lfWDispScr
*! Developer : NADER NABIL (NNA)
*! Date      : 11/07/2004
*! Purpose   : Valid Function to validate screen when it displaying
*!*******************************************************************************
*! Calls     : from the screen
*!*******************************************************************************
*! Example   :  =lfWDispScr()
*!*******************************************************************************
*! C123950,1 NNA 11/23/2004
*!**************************************************************************
*!C124152,1 NNA

FUNCTION lfWDispScr
SHOW GET pbTotalOk DISABLE

*-- End of lfWDispScr.
*!***************************************************************************
*! Name      : lfNonMaj
*! Developer : NADER NABIL (NNA)
*! Date      : 11/07/2004
*! Purpose   : To get the style nonmajor segement structure
*!***************************************************************************
*! Called from : Mfrbr20.prg
*!***************************************************************************
*! Calls       : ....
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*! Return      : None
*!***************************************************************************
*! Example     : = lfNonMaj()
*!***************************************************************************
*!C124152,1 NNA

FUNCTION lfNonMaj

PRIVATE lnOldAlias
STORE 0 TO lnOldAlias
lnOldAlias = SELECT(0)

*-- Compute Free/Color Items in Style Structure. [Begin]
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF

  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnColorLen
SELECT(lnOldAlias)
RETURN ''
*--End function of lfNonMaj.

