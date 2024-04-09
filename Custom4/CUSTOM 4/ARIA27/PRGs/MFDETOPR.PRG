*:----------------------------------------------------------------
*: Program file        : MFDetOpr.PRG
*: Program description : Cut Ticket Add, Modify, Delete, Inquire Det. Oper.
*: For screen          : MFDetOpr.SPR
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : Manufactring - (MF)
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : E301411.
*:----------------------------------------------------------------
*: Calls               : None.
*:----------------------------------------------------------------
*: Called From         : Style Cost Sheet
*:                       CutTkt Cost Sheet.
*:----------------------------------------------------------------
*: Passed Parameters   : lcParntOpr :
*:                       lcDOprTmp  :
*:                       lcCostVar  :
*:                       llFromMain :
*:----------------------------------------------------------------
*: Example             : DO MFDetOpr
*:----------------------------------------------------------------
PARAMETER lcParntOpr , lcDOprTmp , lcCostVar , llFromMain , llHScr , llFrmDet

PRIVATE lcMDBrowTt , lcHost, lcRatType , lnPerRat  , lnUntCost ,;
        lcOperSeq , lnOldAls , lldCancel
lldCancel = .F.
lnOldAls = SELECT(0)
lcBrfield1 = ""
lcRatType  = ''
lnPerRat   = 0
lnUntCost  = 0
DIMENSION laMaxNo[1,1]
DIMENSION laSource[1],laTarget[1]
laMaxNo[1] = 0
lcOperSeq  = ''
lcHost     = 'MFDetOpr'
lcMDBrowTt = 'PWDetail Operation'
lcCBmp     = gcBmpHome + "Can.Bmp"
lcCsBmp    = gcBmpHome + "Cls.Bmp"
llFromCtk  = IIF(TYPE("llFromCtk")<>"L",.F.,llFromCtk)
SELECT (lcDOprTmp)
GOTO TOP
*-- Call the program that add a new Detail Operations.
IF llHScr
  IF llNewSess
    lnTempLnNo = IIF(llFromMain,&lcTmpBom..nLineNo,lfGtCurLNo())
  ENDIF
  IF llNewSess
    =lfvAcc()
  ENDIF
  llNewSess  = .F.
ELSE
  DO gcScrDir+("MFDETOPR.SPR")
ENDIF
SELECT (lnOldAls)
*--- End Program

*!*************************************************************
*! Name      : lfActDBrow
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function to avtivate detail screen browse.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfActDBrow()
*!*************************************************************
*!
FUNCTION lfActDBrow

SELECT (lcDOprTmp)
SET FILTER TO !(cStatus $ 'RD')

IF llFromCtk .AND. llFrmDet
  lcBrfield1 = [ChildOpr :10 :R :H = 'Oper'      ,] +;
               [Desc     :10 :R :H = 'Desc.'     ,] +;
               [WorkCent     :R :H = 'WorkCent'  ,] +;
               [SetUpTim     :R :H = 'SetupTim'  ,] +;
               [PersTim      :R :H = 'Pers. Tim' ,] +;
               [MatchTim     :R :H = 'Mach.Tim.' ,] +;
               [Unit         :R :H = 'Unit/Oper' ,] +;
               [Seq          :R :H = 'Sequance'  ,] +;
               [RatType      :R :H = 'Rate Typ.' ,] +;
               [RatePer      :R :H = 'Rate Per.' ,] +;
               [UnitCost     :R :H = 'Unit Cost' ,] +;
               [cOverType    :R :H = 'Over Type' ,] +;
               [nOverUnit    :R :H = 'Over Unit' ,] +;
               [Tool         :R :H = 'Tools']

ELSE
  lcBrfield1 = [ChildOpr :10 :R :H = 'Oper'      ,] +;
               [Desc     :10 :R :H = 'Desc.'     ,] +;
               [WorkCent     :R :H = 'WorkCent'  ,] +;
               [SetUpTim     :R :H = 'SetupTim'  ,] +;
               [PersTim      :R :H = 'Pers. Tim' ,] +;
               [MatchTim     :R :H = 'Mach.Tim.' ,] +;
               [Unit         :R :H = 'Unit/Oper' ,] +;
               [Seq          :R :H = 'Sequance'  ,] +;
               [RatType      :R :H = 'Rate Typ.' ,] +;
               [RatePer      :R :H = 'Rate Per.' ,] +;
               [UnitCost     :R :H = 'Unit Cost' ,] +;
               [Tool         :R :H = 'Tools']
ENDIF
&&       FOR !(cStatus $ 'RD')
BROWSE FIELDS &lcBrField1;
       WHEN lfwBrows2();
       VALID :F lfvBrowse2() ;
       WINDOW MFDTOPRB IN WINDOW (lcHost);
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE (lcMDBrowTt)

*!*************************************************************
*! Name      : lfvBrowse2
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Valid Browse....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrowse2()
*!*************************************************************
*!
FUNCTION lfvBrowse2

IF WONTOP() # ALLTRIM((lcMDBrowTt))
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfvBrowse.

*!*************************************************************
*! Name      : lfwBrows2
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : When Browse....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrows2()
*!*************************************************************
*!
FUNCTION lfwBrows2

SHOW WINDOW (lcMDBrowTt) REFRESH
=lfLoadVar()

*!*************************************************************
*! Name      : lfUnTrap1
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
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
*! Example   : = lfUnTrap1()
*!*************************************************************
*!
FUNCTION lfUnTrap1

* -- Clear Trap
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If TOP window is not one of the browses window                                        
IF WONTOP() # ALLTRIM((lcMDBrowTt))
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL CTRL+TAB
  ON KEY LABEL CTRL+ENTER
  ON KEY LABEL CTRL+HOME
  ON KEY LABEL CTRL+END
  ON KEY LABEL CTRL+W
ENDIF
*-- end of lfUnTrap.

*!*************************************************************
*! Name      : lfKey1
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Screen key....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfKey1()
*!*************************************************************
*!
FUNCTION lfKey1

*-- Activate Browse window
*-- Define a menu bar with a short-cut key for browse window 
*-- in the program just to be able to activate the browse window
*-- using its menu bar short-cut key.
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcMDBrowTt KEY ALT+D
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcMDBrowTt)
IF llFromCtk
  SHOW GET pbTool DISABLE
  llDummy = llFrmDet .AND. lfDisRem()
ELSE
  IF laScrMode[2] 
    SHOW GETS WINDOW MFDTOPR2 DISABLE ONLY
    SHOW GET pbCls ENABLE
    SHOW GET ibTrap ENABLE
  ELSE
    IF llNewSess
      lnTempLnNo = IIF(llFromMain,&lcTmpBom..nLineNo,lfGtCurLNo())
     ENDIF
    IF llNewSess
      =lfvAcc()
    ENDIF
    llNewSess  = .F.
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfTrap1
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function to trap keys....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap1()
*!*************************************************************
*!
FUNCTION lfTrap1

*-- if TOP window is one of the browse window
IF WONTOP() = ALLTRIM((lcMDBrowTt))
  glFromBrow = .T.
  ON KEY LABEL CTRL+ENTER lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+HOME  lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+W     lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+END   lnDummy = 1		&&Do nothing
  ON KEY LABEL TAB        DO lpTab1
  ON KEY LABEL CTRL+TAB   DO lpTab1
  ON KEY LABEL BACKTAB    DO lpBackTab1
  ON KEY LABEL ESCAPE     lnDummy = 1		&&Do nothing
ENDIF
*-- end of lfTrap.

*!*************************************************************
*! Name      : lpTab1
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trap backtab key....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpTab1()
*!*************************************************************
*!
PROCEDURE lpTab1

ON KEY LABEL TAB
ON KEY LABEL CTRL+TAB
*-- Go to next window
IF WONTOP() = ALLTRIM((lcMDBrowTt))
   ACTIVATE WINDOW MFDTOPR2
   IF laScrMode[2] .AND. !llFromCtk
     _CUROBJ = OBJNUM(pbCls)
   ELSE
     _CUROBJ = OBJNUM(lcRatType)
   ENDIF
ELSE
  ACTIVATE WINDOW (lcMDBrowTt)
  _CUROBJ = OBJNUM(ibBBrow)
ENDIF
*-- end of lpTab.


*!*************************************************************
*! Name      : lpBackTab1
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Trap backtab key....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpBackTab1()
*!*************************************************************
*!
PROCEDURE lpBackTab1

ON KEY LABEL BACKTAB
IF WONTOP() = ALLTRIM((lcMDBrowTt))
  *-- Go to previous window
   ACTIVATE WINDOW MFDTOPR2
  _CUROBJ = OBJNUM(pbCls)
ELSE
  
  ACTIVATE WINDOW (lcMDBrowTt)
  _CUROBJ = OBJNUM(ibBBrow)
ENDIF  
*-- end of lpBackTab.

*!*************************************************************
*! Name      : lfVEdit
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Valid function for edit button.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfVEdit()
*!*************************************************************
*!
FUNCTION lfVEdit
PARAMETER lcObjNo

DO CASE
  SELECT (lcDOprTmp)
  CASE lcObjNo = '1'
    REPLACE RatType  WITH lcRatType
  CASE lcObjNo = '2'
    REPLACE RatePer  WITH lnPerRat
  CASE lcObjNo = '3'
    REPLACE UnitCost WITH lnUntCost
  CASE lcObjNo = '4'
    REPLACE Seq      WITH lcOperSeq
  CASE lcObjNo = '5' .AND.  llFromCtk .AND. llFrmDet
    REPLACE cOverType WITH IIF(puOvrLap='Quantity',"Q","T")
  CASE lcObjNo = '6'  .AND. llFromCtk .AND. llFrmDet
    REPLACE nOverUnit WITH lnOveUnit
ENDCASE
FLUSH
SHOW GET pbCls,1 PROMPT lcCBmp ENABLE
lldCancel = .T.
=lfActDBrow()

*!*************************************************************
*! Name      : lfvAcc
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Valid function for Accept Button.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvAcc()
*!*************************************************************
*!
FUNCTION lfvAcc

&lcCostVar = 0
*--- If we are calling this screen from C/t and from detail button
*--- not from new item button in the C/t cost sheet.
IF llFromCtk .AND. llFrmDet
  =lfUpdTkBom()
ELSE
  *--- citmmajor+mfgcode+STR(nlineno,6)
  lnTempLnNo = IIF(llFromMain,&lcTmpBom..nLineNo,lfGtCurLNo())
  SELECT (lcDOprTmp)
  SET FILTER TO
  GOTO TOP
  SCAN FOR  cStatus <> "D"
    DO CASE
      CASE cStatus = "R"   && Removed
        =lfUpdRem()
      CASE cStatus = "N"   && New
        =lfUpdNew()
      OTHERWISE
        =lfUpdOther()
    ENDCASE
  ENDSCAN
  IF llHScr
    SELECT(lcPwBOM)
    *--- citmmajor+mfgcode+coprcode+STR(nlineno,6)
    =SEEK(PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcParntOpr)
    REPLACE REST cStatus WITH "T"  WHILE citmmajor+mfgcode+coprcode+STR(nlineno,6)=;
                                         PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcParntOpr;
                                   FOR nLineNo = lnTempLnNo
  ENDIF
ENDIF
SHOW GET pbCls,1 PROMPT lcCsBmp ENABLE
lldCancel = .F.
llNewSess  = .F.

*!*************************************************************
*! Name      : lfVRem
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Valid function for Remove Button.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfVRem()
*!*************************************************************
*!
FUNCTION lfVRem

PRIVATE lnOld
lnOld = SELECT(0)
SELECT (lcDOprTmp)
lcTempClh = &lcDOprTmp..ChildOpr
REPLACE cStatus WITH 'R'
SHOW GET pbCls,1 PROMPT lcCBmp ENABLE
lldCancel = .T.
=lfActDBrow()
SELECT(lnOld)

*!*************************************************************
*! Name      : lfVDNew
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Valid function for New Button.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfVDNew()
*!*************************************************************
*!
FUNCTION lfVDNew

PRIVATE lnAlis
lnAlis = SELECT(0)
lcUsrMsg2 = 'Assigne new detail operation'
lcTmpNew = gfTempName()
CREATE TABLE &gcWorkDir.&lcTmpNew (mfgCode C(06),cOprCode C(06),cDesc C(30),;
                                   cOper_Type C(1),cWorkCent C(2),cOperSeq C(2) )
INDEX ON mfgCode+cOprCode TAG &lcTmpNew
SELECT PWOperat
IF SEEK(lcParntOpr)
  SCAN REST WHILE mfgcode+coprcode = lcParntOpr
    SCAT MEMVAR MEMO
    SELECT (lcDOprTmp)
    *--- MFGCODE+ChildOpr
    IF !SEEK(m.mfgcode+m.coprcode) .OR. cStatus $ 'RD'
      INSERT INTO (lcTmpNew) FROM MEMVAR
    ENDIF
  ENDSCAN
ENDIF

lcBrFields = "mfgCode    :10 :H = [Mfg. Code],"+;
             "cOprCode   :10 :H = [Det. Operation],"+;
             "cDesc      :10 :H = [Detail Operation Desc.],"+;
             "cOper_Type :10 :H = [Type],"+;
             "cWorkCent  :10 :H = [Work Cent],"+;
             "cOperSeq   :10 :H = [Seuq.]"

SELECT (lcTmpNew)
DIMENSION laTemp[1]
laTemp = ''
lcKey  = &lcTmpNew..mfgCode
lcFile_Ttl  = 'New Detail Operation.'
lcPbrowtitl = 'New Detail Operation'
SELECT (lcTmpNew)
GOTO TOP
llExit = EOF()
llDummy = ARIABROW("lcKey",lcPbrowtitl,gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'','','cOprCode','laTemp')
IF llExit .OR.  !llDummy
  USE IN (lcTmpNew)
  ERASE &gcWorkDir.&lcTmpNew..DBF
  ERASE &gcWorkDir.&lcTmpNew..CDX
ELSE
  SELECT (lcDOprTmp)
  *--- mfgcode+coprcode
  =SEEK(lcParntOpr+&lcTmpNew..cOprCode,'pwoperat')
  m.ChildOpr = &lcTmpNew..cOprCode
  m.Desc     = pwoperat.cDescRip
  m.WorkCent = pwoperat.cWorkCent
  m.SetUpTim = pwoperat.NSetup_Tim
  m.PersTim  = pwoperat.NEmp_Time
  m.MatchTim = pwoperat.NMach_Time
  m.Unit     = pwoperat.nUnit
  m.Seq      = pwoperat.cOperSeq
  m.RatType  = pwoperat.cOprAtType
  m.RatePer  = pwoperat.NOperatPer
  m.UnitCost = pwoperat.NOperCost
  IF !SEEK(lcParntOpr+m.ChildOpr) .OR. cStatus $ 'DR'
    IF !(cStatus $ 'RD')
      INSERT INTO (lcDOprTmp) FROM MEMVAR
    ENDIF
    REPLACE cStatus WITH 'N'
  ENDIF
  =lfActDBrow()
  USE IN (lcTmpNew)
  ERASE &gcWorkDir.&lcTmpNew..DBF
  ERASE &gcWorkDir.&lcTmpNew..CDX
  SHOW GET pbCls,1 PROMPT lcCBmp ENABLE
  lldCancel = .T.
ENDIF
SELECT(lnAlis)

*!*************************************************************
*! Name      : lfvTool
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Valid function for Tool Button.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvTool()
*!*************************************************************
*!
FUNCTION lfvTool

*--- PWOPTOOL  mfgcode+coprcode+ctolgrpid
PRIVATE lcDetOpr , lnOldAls
lcDetOpr = &lcDOprTmp..ChildOpr
lnOldAls = SELECT(0)

STORE '' TO laSource[1],laTarget[1]
*--- coprcode+citmmajor+ctolgrpid+STR(nlineno,6)

IF lfFilTarg()
  =lfFilTemp()
  =ACOPY(laTarget,laSource)
  IF SEEK(lcDetOpr+PADR(IIF(llFromCtk , laData[2],laData[1] ),19),lcPWTol)
    SELECT DISTINCT cTolGrpId FROM (lcPWTol) ;
          WHERE coprcode+citmmajor+ctolgrpid+STR(nlineno,6) = ;
                lcDetOpr+PADR(IIF(llFromCtk , laData[2],laData[1] ),19);
            .AND. !EMPTY(&lcPWTol..cTolGrpId);
       INTO ARRAY laTarget
  ENDIF
  llDummy =gfMover(@laSource,@laTarget,"Add Tools")
  =lfUpdTol()
ENDIF
SELECT (lnOldAls)


*!*************************************************************
*! Name      : lfFilTarg
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function to fill arrayes.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfFilTarg()
*!*************************************************************
*!
FUNCTION lfFilTarg

PRIVATE lnOld
lnOld = SELECT(0)
SELECT DISTINCT cTolGrpId FROM PWOPTOOL ;
      WHERE mfgcode+coprcode+ctolgrpid= lcParntOpr+lcDetOpr;
        .AND. !EMPTY(PWOPTOOL.cTolGrpId);
   INTO ARRAY laTarget
IF _TALLY = 0
  SELECT (lnOldAls)
  WAIT WINDOW "No tool found for detail operation "+lcDetOpr
  RETURN (.F.)
ENDIF
SELECT(lnOld)

*!*************************************************************
*! Name      : lfFilTemp
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function to fill PWBOMTOOL.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfFilTemp()
*!*************************************************************
*!
FUNCTION lfFilTemp

PRIVATE lnOld
lnOld = SELECT(0)

SELECT PWBomTol
GOTO TOP
IF SEEK(lcDetOpr+PADR(IIF(llFromCtk , laData[2],laData[1] ),19))
  SCAN REST WHILE coprcode+citmmajor+ctolgrpid+STR(nlineno,6)=;
                  lcDetOpr+PADR(IIF(llFromCtk , laData[2],laData[1] ),19)
    SCAT MEMVAR MEMO
    IF !SEEK(m.coprcode+m.citmmajor+m.ctolgrpid+STR(m.nlineno,6),lcPwtol)
      INSERT INTO (lcPwtol) FROM MEMVAR
    ENDIF
  ENDSCAN
ENDIF
SELECT(lnOld)
            
*!*************************************************************
*! Name      : lfUpPre
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function to Update previous cost .
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpPre()
*!*************************************************************
*!
FUNCTION lfUpPre
PARAMETER lnOpr , lnCost

PRIVATE lnCurr
lnCurr = SELECT(0)
IF SEEK(PADR(IIF(llFromCtk , laData[2],laData[1] ),19),lcTmpBom)
  SELECT (lcTmpBom)
  LOCATE REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = ;
                    PADR(IIF(llFromCtk , laData[2],laData[1] ),19);
              FOR mfgCode    + STR(nLineNo,6) = ;
                  lnOpr + STR(&lcPWBom..nLineNo,6)
  IF FOUND()
    REPLACE UntCost WITH UntCost + lnCost,;
            TotCost WITH TotCost + lnCost,;
            cStatus WITH IIF(cStatus ="S","M",cStatus)
  ENDIF
ENDIF
SELECT(lnCurr)

*!*************************************************************
*! Name      : lfUpdRem
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function to Update previous cost .
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpdRem()
*!*************************************************************
*!
FUNCTION lfUpdRem

SELECT (lcPWBom)
*--- citmmajor+mfgcode+coprcode+STR(nlineno,6)
IF SEEK(PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcParntOpr+&lcDOprTmp..ChildOpr+IIF(llFromMain,STR(&lcTmpBom..nLineNo,6),STR(lfGtCurLNo(),6)))
  SCAN REST WHILE citmmajor+mfgcode+coprcode+STR(nlineno,6)=;
                   PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcParntOpr+&lcDOprTmp..ChildOpr+IIF(llFromMain,STR(&lcTmpBom..nLineNo,6),STR(lfGtCurLNo(),6))
    REPLACE cStatus WITH 'D'
    lnToUp = -(nOperCost/IIF(nOperatPer<>0,nOperatPer,1))
    =lfUpPre(lcParntOpr,lnToUp)
  ENDSCAN
ENDIF
SELECT (lcDOprTmp)

*!*************************************************************
*! Name      : lfUpdNew
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function to Update New cost .
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpdNew()
*!*************************************************************
*!
FUNCTION lfUpdNew

SELECT pwoperat
SCAT MEMVAR MEMO
SELECT (lcPWBom)
*--- Get the last(MAX) line : 
*---            If we are adding anew cost sheet so we are modifing in 
*---            the last line we enterd because we can not modify
*---            after accept. (In New cost item screen)
lnTempLn = IIF(llFromMain,&lcTmpBom..nLineNo,lfGtCurLNo())

IF !SEEK(PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcParntOpr+&lcDOprTmp..ChildOpr+STR(lnTempLn,6)) .OR. cStatus $ 'RD'
  IF cStatus $ 'RD'
    REPLACE cStatus WITH ''
    lnTempLn = nLineNo
  ELSE
    APPEND BLANK
  ENDIF
ENDIF
REPLACE cItmmajor  WITH IIF(llFromCtk , laData[2],laData[1] ),;
        mfgCode    WITH lcParntOpr,;
        coprcode   WITH &lcDOprTmp..ChildOpr,;
        coperseq   WITH &lcDOprTmp..Seq,;
        NOPERATPER WITH &lcDOprTmp..RatePer,;
        Nopercost  WITH &lcDOprTmp..UnitCost,;
        nLineNo    WITH lnTempLn
lnToUp = (nOperCost/IIF(nOperatPer<>0,nOperatPer,1))
lnRatPer = IIF(&lcDOprTmp..RatePer<>0,&lcDOprTmp..RatePer,1)
&lcCostVar = &lcCostVar + (&lcDOprTmp..UnitCost/(lnRatPer))

*!*************************************************************
*! Name      : lfUpdTol
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function to Update Tools .
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpdTol()
*!*************************************************************
*!
FUNCTION lfUpdTol

SELECT (lcPWBom)
*--- If we are adding anew cost sheet so we are modifing in 
*--- the last line we enterd because we can not modify
*--- after accept. (In New cost item screen)
DIMENSION lnTempLn[1,1]
lnTempLn = IIF(llNewSess,lfGtCurLNo(),lfGtCurLNo()-1)
lnTempLn = IIF(llFromMain,&lcTmpBom..nLineNo,lnTempLn)
SELECT (lcPWTol)
FOR lnInd = 1 TO ALEN(laTarget)
  IF !EMPTY(laTarget[lnInd])
    IF !SEEK(lcDetOpr+PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+laTarget[lnInd]+IIF(llFromMain,STR(&lcTmpBom..nLineNo,6),STR(lnTempLn,6)),lcPWTol)
      SELECT(lcPWTol)
      APPEND BLANK
      REPLACE nLineNo   WITH lnTempLn,;
              cItmMajor WITH PADR(IIF(llFromCtk , laData[2],laData[1] ),19),;
              cOprCode  WITH lcDetOpr,;
              cTolGrpId WITH laTarget[lnInd]
    ELSE
      IF cStatus = 'D'
        REPLACE cStatus WITH ''
      ENDIF
    ENDIF
  ENDIF
ENDFOR
FOR lnInd = 1 TO ALEN(laSource)
  lnArr = IIF(SUBSTR(laSource[lnInd],1)='\',SUBSTR(laSource[lnInd],2,6),laSource[lnInd])
  IF SEEK(lcDetOpr+PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lnArr+IIF(llFromMain,STR(&lcTmpBom..nLineNo,6),STR(lnTempLn,6)),lcPWTol) .AND.;
    ASCAN(laTarget,lnArr) = 0
    REPLACE cStatus WITH 'D'
  ENDIF
ENDFOR

*!*************************************************************
*! Name      : lfUpdOther
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function to Update Records witch is not Removed or New .
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfUpdOther()
*!*************************************************************
*!
FUNCTION lfUpdOther

SELECT (lcDOprTmp)
lcOper  = lcParntOpr
lcChild = ChildOpr
SELECT (lcDOprTmp)
SCAT MEMVAR MEMO
lnRatPer = IIF(m.RatePer<>0,m.RatePer,1)
&lcCostVar = &lcCostVar + (m.UnitCost/lnRatPer)
SELECT (lcDOprTmp)
*--- citmmajor+mfgcode+coprcode+STR(nlineno,6)
SELECT (lcPWBom)
lcNewPath = ''
*--- Start Check The Path
IF SEEK(PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcParntOpr+lcChild+STR(&lcTmpBom..nLineNo,6))
  lnCurSeq  = coperseq
  LOCATE REST WHILE citmmajor+mfgcode+coprcode+STR(nlineno,6) = ;
                    PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcParntOpr+lcChild+STR(&lcTmpBom..nLineNo,6);
              FOR coperseq <> lnCurSeq .AND. cStatus <> 'D'
  lcNewPath = IIF(EMPTY(CBATH),CBATH,CHR(ASC("CBATH")+1))
ENDIF
*--- End
IF !llFromMain  .AND. llNewSess
  SELECT (lcPWBom)
  APPEND BLANK
  REPLACE nLineNo WITH lnTempLnNo
ELSE
  *--- m.citmmajor+m.mfgcode+m.cOprCode+STR(m.nlineno,6)
  =SEEK(PADR(IIF(llFromCtk , laData[2],laData[1] ),19)+lcParntOpr+lcChild+STR(lnTempLnNo,6))
ENDIF
SELECT (lcPWBom)
REPLACE cItmmajor  WITH IIF(llFromCtk , laData[2],laData[1] ),;
        mfgCode    WITH lcParntOpr,;
        coprcode   WITH lcChild ,;
        coperseq   WITH m.Seq,;
        NOPERATPER WITH m.RatePer,;
        Nopercost  WITH m.UnitCost,;
        CBATH      WITH lcNewPath,;
        cOpRatType WITH m.RatType
        
*!*************************************************************
*! Name      : lfGtCurLNo
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function to Get the current max line no .
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGtCurLNo()
*!*************************************************************
*!
FUNCTION lfGtCurLNo
PRIVATE lnOld , lnToRet

lnOld = SELECT(0)
SELECT (lcTmpBom)
DIMENSION lnToRet[1,1]
lnToRet[1,1] = 0
SELECT MAX(nLineNo) FROM (lcTmpBom) WHERE citmmajor+typ+citmmask+mfgcode+item+iclr=;
                                          PADR(IIF(llFromCtk , laData[2],laData[1] ),19);
                                    .AND.   mfgcode = lcParntOpr;
       INTO ARRAY lnToRet
lnToRet[1,1] = lnToRet[1,1] + 1
SELECT(lnOld)
RETURN(lnToRet[1,1])


*!*************************************************************
*! Name      : lfvDetCls
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Valid Function for close / cancel button .
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvDetCls()
*!*************************************************************
*!
FUNCTION lfvDetCls

IF lldCancel
  IF gfModalGen("QRM38028B00006" , "DIALOG") = 1
    CLEAR READ
  ENDIF
ELSE
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfLoadVar
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function To load variable.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfLoadVar()
*!*************************************************************
*!
FUNCTION lfLoadVar

lcRatType  = RatType
lnPerRat   = RatePer
lnUntCost  = UnitCost
lcOperSeq  = Seq
IF llFromCtk .AND. llFrmDet
  puOvrLap = IIF(cOverType='Q',"Quantity","Time")
  lnOveUnit = nOverUnit
  SHOW GET lnOveUnit
  SHOW GET puOvrLap
ENDIF
SHOW GET lcRatType
SHOW GET lnPerRat
SHOW GET lnUntCost
SHOW GET lcOperSeq


*!*************************************************************
*! Name      : lfUpdTkBom
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function To Update C/T Cost sheet.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpdTkBom()
*!*************************************************************
*!
FUNCTION lfUpdTkBom
PRIVATE lnOldAls , lcTempSize

lnOldAls = SELECT(0)
SELECT(lcDOprTmp )
SET FILTER TO 
GOTO TOP
SCAN
  SCATTER MEMVAR MEMO
  IF laData[3] <> "H"
    *--- cuttkt+mfgcode+coprcode+STR(nlineno,6)
    SELECT PWCtkBom
  ELSE
    *--- cuttkt+mfgcode+coprcode+STR(nlineno,6)
    SELECT(lcPWCtkBom)
  ENDIF
  DO CASE
    CASE  &lcDOprTmp..cStatus $ "RD"
      *--- cuttkt+mfgcode+coprcode+style+STR(nlineno,6)
      IF SEEK(laData[1] + lcParntOpr + &lcDOprTmp..ChildOpr + lcCurStyle + STR(&lcDOprTmp..nlineno,6))
*        LOCATE REST WHILE cuttkt+mfgcode+coprcode+Style+STR(nlineno,6)=;
*                          laData[1] + lcParntOpr + &lcDOprTmp..ChildOpr + STR(&lcDOprTmp..nlineno,6);
*                    FOR   Style = lcCurStyle
*        IF FOUND()  
          *&lcCostVar = &lcCostVar - (m.UnitCost/m.RatePer)
          DELETE
          FLUSH
*        elsE
*          wait window "not fouind"
*        ENDIF
      ENDIF
    CASE  &lcDOprTmp..cStatus = "N"
      =SEEK(laData[1] + lcParntOpr + &lcDOprTmp..ChildOpr + lcCurStyle)
      LOCATE REST WHILE cuttkt+mfgcode+coprcode+style+STR(nlineno,6) =;
                        laData[1] + lcParntOpr + &lcDOprTmp..ChildOpr + lcCurStyle;
                  FOR   Style = lcCurStyle
      lcTempSize = cSizes
      APPEN BLANK
      REPLACE CutTkt      WITH laData[1],;
              Style       WITH lcCurStyle,;
              cSizes      WITH lcTempSize,;
              mfgCode     WITH lcParntOpr,;
              cOprCode    WITH m.ChildOpr,;
              cOperSeq    WITH m.Seq,;
              cOprAtType  WITH m.cOprAtType,;
              nOperAtPer  WITH m.RatePer,;
              nOperCost   WITH m.UnitCost,;
              cOverType   WITH IIF(puOvrLap='Quantity',"Q","T"),;
              nOverUnit   WITH lnOveUnit,;
              nLineNo     WITH lfGetMaxLn()
      FLUSH
      &lcCostVar = &lcCostVar + (m.UnitCost/m.RatePer)
    OTHERWISE
      IF SEEK(laData[1] + lcParntOpr + &lcDOprTmp..ChildOpr +lcCurStyle+ STR(&lcDOprTmp..nlineno,6))
*        LOCATE REST WHILE cuttkt+mfgcode+coprcode+STR(nlineno,6)=;
*                          laData[1] + lcParntOpr + &lcDOprTmp..ChildOpr + STR(&lcDOprTmp..nlineno,6);
*                    FOR   Style = lcCurStyle
*        IF FOUND()
        REPLACE  coperseq   WITH m.Seq,;
                 NOPERATPER WITH m.RatePer,;
                 Nopercost  WITH m.UnitCost,;
                 cOpRatType WITH m.RatType,;
                 nOverUnit  WITH m.nOverUnit,;
                 cOverType  WITH m.cOverType
        &lcCostVar = &lcCostVar + (m.UnitCost/m.RatePer)
        FLUSH
*        ENDIF
      ENDIF
  ENDCASE
ENDSCAN
SELECT (lcDOprTmp)
SET FILTER TO !(cStatus $ 'RD')
SELECT(lnOldAls)

*!*************************************************************
*! Name      : lfGetMaxLn
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 31/06/2000
*! Purpose   : Function To Max line no in C/T cost sheet file.
*!*************************************************************
*! Calls     : Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Called from : 
*!         Procedures : 
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfGetMaxLn()
*!*************************************************************
*!
FUNCTION lfGetMaxLn

PRIVATE lnOld , lnToRet

lnOld = SELECT(0)
lcTempOpr = &lcDOprTmp..ChildOpr
IF laData[3] <> "H"
  SELECT PWCtkBom
ELSE
  SELECT(lcPWCtkBom)
ENDIF
DIMENSION lnToRet[1,1]
lnToRet[1,1] = 0
IF laData[3] <> "H"
  SELECT MAX(nLineNo) FROM PWCtkBom ;
                      WHERE cuttkt+mfgcode+coprcode+STR(nlineno,6)=;
                            laData[1] + lcParntOpr + lcTempOpr;
                      .AND.   Style = lcCurStyle;
         INTO ARRAY lnToRet

ELSE
  SELECT MAX(nLineNo) FROM (lcPWCtkBom) ;
                      WHERE cuttkt+mfgcode+coprcode+STR(nlineno,6)=;
                            laData[1] + lcParntOpr + lcTempOpr;
                      .AND.   Style = lcCurStyle;
         INTO ARRAY lnToRet
ENDIF
lnToRet[1,1] = lnToRet[1,1] + 1
SELECT(lnOld)
RETURN(lnToRet[1,1])


FUNCTION lfDisRem
PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT BOMLINE
SET ORDER TO BOMPOREC DESCENDING
=SEEK(lcTranType+'2'+&lcTktSheet..TYP+&lcTktSheet..MFGCODE+laData[1])
LOCATE REST WHILE cIMTyp+cType+cBomTyp+MfgCode+cTktNo+cRSession+ShipNo+;
                  STR(lineno,6)+Style+SClr = ;
                  lcTranType+'2'+&lcTktSheet..TYP+&lcTktSheet..MFGCODE+;
                  laData[1] .AND. !EMPTY(cRsession) ;
            FOR   Item+IClr =&lcTktSheet..Item+&lcTktSheet..IClr
IF FOUND()
  *--- This record has been considered as landed cost for the receiving
  SHOW GET pbRem DISABLE
ENDIF
SELECT (lcTktSheet)
IF SEEK(cIMTyp+CutTkt+Typ+Item+IClr+MfgCode+Dyelot,'CTKTBOM') .AND. ;
   cTktBom.Used_Qty <> 0
  *--- Quantity has been issued from this record.
  SHOW GET pbRem DISABLE
ENDIF
IF &lcTktSheet..cCatgTyp='M' .AND. SEEK(lcTranType+laData[1]+&lcTktSheet..MfgCode,lcOprDet)
  *--- Lots have been issued from operation.
  SHOW GET pbRem DISABLE
ENDIF
SELECT(lnOldAls)