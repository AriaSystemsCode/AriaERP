*!********************************************************************
*: Program file  : SOGMA11.PRG (REFERE TO C101818)
*: Program desc. : ASSIGN STYLES FOR A SPECIFIC DEPARTMENT FOR
*:                 GMA Inc.
*: For screen    :  SOGMA11
*:         System: ARIA 2.7
*:         Module: SO
*:      Developer: Ahmed Mohamed_EL_Anwar Abdel_Fatth . AME
*!********************************************************************
*!*: Modification
*B803351,1 MHM 07/18/2000 FIXING TRABING ESC KEY 
*B605993,1 TMI 06/12/2002 Remove the Validation that a goup can not be saved if it has no styles
*B121124,1 MHM 01/06/2004 Initialize glFromBrow, becuase in case of comming from codes screen 
*B121124,1 MHM            glFromBrow = .T. 
*!********************************************************************

EXTERNAL ARRAY laDefProc,laKeyField , laCtrStat

*-- Initializing the necessary variables
lcGroup    = SPACE(06)     && Variable for getting the Style Group
lnFileStru = 0

STORE "" TO laGrp, laBrowArr,laCodinfo , laKeyField , TempCursor , ;
            TempHdr , TmpFile, lcWindE, lcWindD,lcWindC,lcWindB,lcScFields

STORE 1 TO lnGrp

DIMENSION laScrMode[4],laBrowArr[1],laCodinfo[1,10],laGrp[2] , laKeyField[1,4] , laData[5]

STORE .F. TO laScrMode
laScrMode[1] = .T.

llClose    = .T.           && Variable to check the status of pbClose
llBrowse   = .F.           && Variable for browsing.
lcBrTit    = "Groups" 
lcBrowBmp  = (GCBmpHome + "ExtKey.BMP")

*B121124,1 MHM 01/06/2004 initialize variable [Start]
glFromBrow = .F.
*B121124,1 MHM 01/06/2004 initialize variable [Start]

*-- To force the control pannel to call the local save and close
*-- procedures (lpNxtScr ,lpPrvScr ,lpSavScr, lpClsScr ,lpDelScr )

laDefProc[03] = .F.   &&lpNxtScr
laDefProc[04] = .F.   &&lpPrvScr
laDefProc[07] = .F.   &&lpSavScr
laDefProc[09] = .F.   &&lpClsScr
laDefProc[10] = .F.   &&lpDelScr

*-- The screen keys
laKeyField[1,1] = "laData[1]"
laKeyField[1,2] =.T.
laKeyField[1,3] = "Department"
laKeyField[1,4] = 1

IF !GFSETUP()
  RETURN
ENDIF

lcBrFields =  [Dept:10:H="Department",cDeptDesc:30:H="Description"]

IF !WEXIST(gcBaseWind)
  *-- Screen fields variable, to be used to build the laData array.
  lcScFields = " dept , cdeptdesc , cstygroup , cstatus , cdiscrep"
  *-- laData Map
  *    laData[1]    laData[2]    laData[3]     laData[4]     laData[5]
  *    dept       , cdeptdesc   ,cStygroup    ,cStatus      ,cDisCrep    
  lcWindB    = gfTempName()
  lcWindC    = gfTempName()
  lcWindD    = gfTempName()
  *lcWindE    = gfTempName()
  lcWindE   = "GwcContrl1"
  TmpFile    = gfTempName()  && A cursor to hold the styles of a group
  TempHdr    = gfTempName()  && File to hold the assigned groups
  TempCursor = gfTempName()  
  SELECT IcDeptHd
  SET ORDER TO DEPTHD
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'cGrpDesc'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 30
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'cStatus'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 1
  laFileStru[lnFileStru+2,4] = 0
  CREATE TABLE (GcWorkDir+TempHdr) FROM ARRAY laFileStru
  INDEX ON cstyGroup TAG(TempHdr)
  SCATTER FIELDS &lcScFields TO laData BLANK 
  SELECT ICDEPTHD
  *GO TOP
  SCAN
    SCATTER MEMVAR
    m.cGrpDesc = gfCodDes(ICDEPTHD.cStyGroup, 'CSTYGROUP')
    m.cStatus  = "S"
    INSERT INTO (TempHdr) FROM MEMVAR
   
    *=SEEK("N"+"CSTYGROUP "+ICDEPTHD.cStyGroup,'Codes')
    *SELECT(TempHdr)
    *REPLACE cStatus  WITH "S",;
            cGrpDesc WITH Codes.Cdiscrep
    *REPLACE cStatus  WITH "S",;
            cGrpDesc WITH gfCodDes(ICDEPTHD.cStyGroup, 'CSTYGROUP')

  ENDSCAN
  SELECT (TempHdr) 
  SET FILTER TO DEPT = ladata[1]
ELSE
ENDIF

DO (GcScrDir+"SO\SOGMA11.SPX")
RELEASE WINDOW (lcBrTit)

*-- Functions and Procedures :
*:***************************************************************************
*: Name       : lpShow
*: Developer  : Ahmed Mohamed_El_Anwer  Abd_El_Fattah {AME}
*: Date       : 04/04/2000
*: Purpose    : Screen modes controlling procedure.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : DO lpShow
*:***************************************************************************
PROCEDURE lpShow

DO CASE
  *                ---> S E L E C T  M O D E <---
  CASE laScrMode[1]
    llClose = .T. 
    ladata[1] = SPACE[5]
    =lfbrows()
    SHOW GET pbBrws DISABLE     
  *******          -->V I E W     M O D E <--
  CASE laScrMode[2]

    llClose = .T.
    *SELECT IcDeptHd
    SCATTER FIELDS &lcScFields MEMO TO laData
    ladata[2]  = IcDeptHd.cDeptDesc
    SHOW WINDOW (lcBrTit) REFRESH SAME
    SHOW GET pbBrws  DISABLE  
      
  *                ---->E D I T  M O D E <----
  CASE laScrMode[3]
    llClose = .F.

  *                   ----> A D D  M O D E <----
  CASE laScrMode[4] 
    llClose = .F. 
    lnChoice = 0
ENDCASE  
*!*************************************************************
*! Name      : lfvDept
*! Developer : Khalid Mohi El-Din
*! Date      : 05/25/1998
*! Purpose   : To validate the Dept code.
*!*************************************************************
*! Example            : =lfvDept()
*!*************************************************************
FUNCTION lfvDept

IF !llBrowse .AND. EMPTY(ladata[1])
  RETURN
ENDIF

IF llBrowse OR LEFT(ladata[1],1) = "?"
  *-- To browse all the existing departments 
  =lfDeptBrow()
  *-- Rebrowse the groups
  =lfBrows()
  llBrowse = .F.
  IF EMPTY(ladata[1])
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
ENDIF

SET ORDER TO DeptHd IN IcDeptHd
IF !EMPTY(ladata[1]) 
  IF !SEEK(ladata[1],'IcDeptHd')
    lnChoice = gfDialog("I","Department "+ALLTRIM(ladata[1])+" not found ",;
                             "\!\<Add;\<Browse;\<Reenter")
    DO CASE
      CASE lnChoice = 1
         lascrmode = .F.
         laScrMode[4] = .T.
      CASE lnChoice = 2
        =lfDeptBrow()        
        =lfBrows()
        llBrowse = .F.
        IF !EMPTY(ladata[1])
          laScrMode = .F.
          laScrMode[2] = .T.

        ELSE
          _CUROBJ = _CUROBJ
          RETURN
        ENDIF
      CASE lnChoice = 3
        ladata[1] = SPACE(5)
        _CUROBJ = _CUROBJ
        RETURN
    ENDCASE
  ELSE
   laScrMode = .F.
   laScrMode[2] = .T.
  ENDIF  
ENDIF
SHOW GETS


*!*************************************************************
*! Name      : lfBrows
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/25/1998
*! Purpose   : To brows the selected of Order.
*!*************************************************************
*! Example            :  lfBrows()
*!*************************************************************
FUNCTION lfBrows
PRIVATE lnAlias

lnAlias = SELECT()
lcBrFlds = [cStyGroup :12:H='Group':R,cGrpDesc :42:H='Group Description':R]
SELECT (TempHdr)
GOTO TOP

BROWSE FIELDS &lcBrFlds;
       WINDOW    (lcwindC)  ;
       IN WINDOW (GcBaseWind) ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       NOEDIT            ;
       LOCK 0            ;
       WHEN lfwBrows()   ; 
       VALID :F lfvBrows();
       FOR cStatus <> "D";
       TITLE lcBrTit

SHOW WINDOW (lcBrTit) REFRESH SAME
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfDeptBrow
*! Developer : Khalid Mohi El-Din
*! Date      : 05/25/1998
*! Purpose   : To brows the departments.
*!*************************************************************
*! Example            : =lfDeptBrow()
*!*************************************************************
FUNCTION lfDeptBrow

SELECT IcDeptHd
GOTO TOP
IF EOF()
  =gfDialog("I","No records to display")
  ladata[1]  = SPACE(5)
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
SELECT *;
   FROM IcDeptHd;
   GROUP BY Dept;   
   INTO CURSOR(TempCursor)

lcBrFields = [Dept:10:H="Department",cDeptDesc:30:H="Description"]
IF _DOS
  lcBrFields = [Dept:10:H="Department",cDeptDesc:30:H="Description"]
  ladata[1] = IIF(ARIABROW('',"Departments",07,01,20,10,'','','Dept','laBrowArr'),Dept,SPACE(5))
ELSE
  lcBrFields = [Dept:H="Department",cDeptDesc:H="Description"]
  ladata[1] = IIF(ARIABROW('',"Departments",.F.,.F.,.F.,.F.,'','','Dept','laBrowArr'),Dept,SPACE(5))
ENDIF

*!*************************************************************
*! Name      : lfvGrpCan
*! Developer : Khalid Mohi El-Din
*! Date      : 05/25/1998
*! Purpose   : To validate the Cancel push button of SOGMA11F.SCX
*!*************************************************************
*! Example            : =lfvGrpCan()
*!*************************************************************
FUNCTION lfvGrpCan

_CUROBJ = OBJNUM(pbNew)
CLEAR READ


*!*************************************************************
*! Name      : lfvNew
*! Developer : Khalid Mohi El-Din
*! Date      : 05/25/1998
*! Purpose   : To validate the New push button.
*!*************************************************************
*! Example            : =lfvNew()
*!*************************************************************
FUNCTION lfvNew
lcGroup = "      "
laCodInfo[1,01] = "CSTYGROUP"
laCodInfo[1,02] = "laGrp"
laCodInfo[1,03] = "lnGrp"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.            
laCodInfo[1,06] = .F.            
laCodInfo[1,07] = "STYLE"     
laCodInfo[1,08] = "STYLE"     
laCodInfo[1,09] = ""
laCodInfo[1,10] = "cStyGroup"     

DO (GcScrdir+"SO\SOGMA11F.SPX")
_CUROBJ = OBJNUM(pbNew)
SELECT (TempHdr)
GOTO TOP
IF !EOF()
  SHOW GET pbRemove ENABLE
ENDIF

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Khalid Mohi El-Din
*! Date      : 05/25/1998
*! Purpose   : To validate the Remove push button.
*!*************************************************************
*! Example            : =lfvremove()
*!*************************************************************
FUNCTION lfvRemove
SELECT(TempHdr)
REPLACE cStatus WITH "D"
llCUpDate = .T.
GOTO TOP
IF EOF()
  SHOW GET pbRemove DISABLE
ENDIF
_CUROBJ = OBJNUM(ibTabs)

SHOW WINDOW (lcBrTit) REFRESH

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Ahmed Mohamed_El_Anwer Abd_El_Fattah
*! Date      : 05/04/2000
*! Purpose   : local save button procedure
*!*************************************************************
*! Example            : =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr
SELECT IcDeptHd
lnRecNo = RECNO()
XDEPT = DEPT
IF !EOF()
  =SEEK(XDEPT)
  SCAN WHILE DEPT = XDEPT
    =gfObj_Lock(.F.)   && Unlock the record
  ENDSCAN
  GO lnRecNo
ENDIF
SELECT (TempHdr)
GOTO TOP
IF EOF()
   =gfDialog("I","No groups assigned to this department. Cannot proceed.")
  _CUROBJ = OBJNUM(pbNew)
  RETURN
ELSE
  LOCATE FOR cStatus <> "D" 
  IF !FOUND()
     =gfDialog("I","No groups assigned to this department. Cannot proceed.")
     SELECT (TempHdr)
     GOTO TOP
     *--b803351 (MHM) Fixing Bug Of Not Delete Last Record From Temp File [Begin] 
     IF SEEK(laData[1],"IcDeptHD")
     *--b803351 (MHM) Fixing Bug Of Not Delete Last Record From Temp File [Begin] 
       SCAN FOR cStatus = "D"
         REPLACE cStatus WITH "S"
       ENDSCAN  
     *--b803351 (MHM) Fixing Bug Of Not Delete Last Record From Temp File [Begin] 
     ENDIF
     *--b803351 (MHM) Fixing Bug Of Not Delete Last Record From Temp File [Begin] 
    _CUROBJ = OBJNUM(pbNew)
    RETURN
  ENDIF 
ENDIF


SET ORDER TO  DeptHd IN IcDeptHd
SCAN FOR !EMPTY(cStyGroup)
 lcGroup = cstyGroup   
  IF laScrMode[3]
    SELECT IcDeptHd
    =SEEK (&TempHdr..Dept+lcGroup)
    REPLACE cDeptDesc WITH ladata[2]
  ENDIF 
  SELECT (TempHdr)
  DO CASE
    CASE cStatus = "A"
     REPLACE cStatus with "S" 
     lcOldTalk = SET("TALK")
     SET TALK ON
     SET TALK WINDOW 
    
     SELECT Style.Style,Style.cStyGroup;
       FROM Style;
       WHERE CStyGroup = lcGroup;
       INTO CURSOR(TmpFile)
       SET TALK &lcOldTalk
     
     IF _TALLY = 0
       *B605993,1 TMI [Start] Allow saving a group even if it has no styles assigned to it
       *  =gfDialog("I","No styles found for group "+lcGroup+".")
       *  SELECT (TempHdr)
       *  DELETE
       *  IF EOF("IcDepthd")
       *    SELECT IcDeptHd
       *    GO BOTTOM
       *  ENDIF
       *  LOOP
       CREATE CURSOR (TmpFile) (STYLE C(19),CSTYGROUP C(6))
     *B605993,1 TMI [End  ] Allow saving a group even if it has no styles assigned to it
     ENDIF
     SELECT IcDeptHd
     APPEND BLANK
     REPLACE Dept  WITH ladata[1],;
             cStyGroup WITH &TempHdr..cStyGroup,;
             cDeptDesc  WITH ladata[2]
     *SELECT IcDeptDt
     *B605993,1 TMI [Start] Save Style order and set order to tag "style"
     lcStyOrd = ORDER('STYLE')
     SET ORDER TO STYLE IN STYLE
     *B605993,1 TMI [End  ] Save Style order and set order to tag "style"
     SELECT (TmpFile)
     SCAN
       WAIT WINDOW "Assigning style: " + ALLTRIM(Style) NOWAIT
       SELECT IcDeptDt
       APPEND BLANK
       REPLACE Dept  WITH ladata[1],;
               cStyGroup WITH lcGroup,;
               Style WITH &TmpFile..Style
       *B605993,1 TMI [Start] Update style.dept
       =SEEK(&TmpFile..STYLE,'STYLE')
       SELECT STYLE
       REPLACE DEPT WITH ladata[1]
       *B605993,1 TMI [End  ] Update style.dept             
     ENDSCAN

     *B605993,1 TMI [Start] restore style order
     SET ORDER TO &lcStyOrd IN STYLE
     *B605993,1 TMI [End  ] restore style order 
     
    *N000385,4 TMI [Start] Case when edit is entred then save
    CASE cStatus = "S"    
     lcOldTalk = SET("TALK")
     SET TALK ON
     SET TALK WINDOW 
    
     SELECT Style.Style,Style.cStyGroup;
       FROM Style;
       WHERE CStyGroup = lcGroup;
       INTO CURSOR(TmpFile)
       SET TALK &lcOldTalk
     
     IF _TALLY = 0
       CREATE CURSOR (TmpFile) (STYLE C(19),CSTYGROUP C(6))
     ENDIF
     lcStyOrd = ORDER('STYLE')
     SET ORDER TO STYLE IN STYLE
     SELECT (TmpFile)
     SCAN
       WAIT WINDOW "Updating style: " + ALLTRIM(Style) NOWAIT
       SELECT IcDeptDt
       IF !SEEK(laData[1]+&TmpFile..cStyGroup+&TmpFile..style)
         APPEND BLANK
         REPLACE Dept  WITH ladata[1],;
                 cStyGroup WITH lcGroup,;
                 Style WITH &TmpFile..Style
       ENDIF
       =SEEK(&TmpFile..STYLE,'STYLE')
       SELECT STYLE
       REPLACE DEPT WITH ladata[1]
     ENDSCAN

     SET ORDER TO &lcStyOrd IN STYLE
    *N000385,4 TMI [End  ]  

    CASE cStatus = "D"    
      IF SEEK(&TempHdr..Dept+lcGroup,'IcDeptHd')
        SELECT IcDeptHd
        DELETE

        *B605993,1 TMI [Start] Save Style order and set order to tag "style"
        lcStyOrd = ORDER('STYLE')
        SET ORDER TO STYLE IN STYLE
        *B605993,1 TMI [End  ] Save Style order and set order to tag "style"

        SELECT IcDeptDt
        =SEEK(&TempHdr..Dept+lcGroup)
        
        SCAN REST WHILE Dept+cStyGroup+Style = &TempHdr..Dept+lcGroup
          *B605993,1 TMI [Start] Update style.dept
          =SEEK(IcDeptDt.STYLE,'STYLE')
          SELECT STYLE
          REPLACE DEPT WITH '     '
          SELECT IcDeptDt
          *B605993,1 TMI [End  ] Update style.dept                       
          WAIT WINDOW "Release style : " + ALLTRIM(Style) NOWAIT
          DELETE
        ENDSCAN
        SELECT (TempHdr)
        DELETE
        
        *B605993,1 TMI [Start]  restore style order
        SET ORDER TO &lcStyOrd IN STYLE
        *B605993,1 TMI [End  ]  restore style order
        
      ENDIF
      
  ENDCASE 
ENDSCAN

WAIT CLEAR
SELECT (TempHdr)
=lfBrows()
SHOW WINDOW (lcBrTit) REFRESH SAME


lascrmode = .F.
lascrmode[1] = .T.
=lpshow()
*--b803351 (MHM) Fixing Bug Of Not Delete Last Record From Temp File [Begin] 

SELECT IcDeptHd
GO TOP
*--b803351 (MHM) [End] 
*!*************************************************************
*! Name      : lfvSelect
*! Developer : Khalid Mohi El-Din
*! Date      : 05/25/1998
*! Purpose   : To validate the Select push button.
*!*************************************************************
*! Example            : =lfvSelect()
*!*************************************************************
FUNCTION lfvSelect

lascrmode = .F.
lascrmode[1] = .T.
=lpshow()



*!*************************************************************
*! Name      : lpClsScr
*! Developer : Ahmed Mohamed_EL_Anwer Abd_El_Fatth
*! Date      : 03/04/2000
*! Purpose   : To validate the Close push button.
*!*************************************************************
*! Example            : =lpClsScr()
*!*************************************************************
FUNCTION lpClsScr

IF !llClose 
  lnActAls = SELECT(0)
  SELECT IcDeptHd
  lnRecNo = RECNO()
  XDEPT = DEPT
  IF !EOF()
    =SEEK(XDEPT)
    SCAN WHILE DEPT = XDEPT
      =gfObj_Lock(.F.)   && Unlock the record
    ENDSCAN
    GO lnRecNo
  ENDIF
  SELECT (TEMPHDR)
  SCAN FOR cStatus <> 'S'
    DO CASE
     CASE cStatus = 'D'
       REPLACE cStatus with 'S'
     CASE cStatus = 'A'
       DELETE
    ENDCASE 
  ENDSCAN
  SELECT (lnActAls)
ELSE
  CLEAR READ  
ENDIF

*!*************************************************************
*! Name      : lfReadAct
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/25/1998
*! Purpose   : The READ Activate function of NAP800
*!*************************************************************
*! Example            :  lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

*-b803351 (MHM) FIXING TRABING ESC KEY [ BEGIN ]
*-- Restore old key settings and clear the current settings
*IF glFromBrow
*  = gfStopBrow()
  *glFromBrow = .F.
*ENDIF

*IF WONTOP() <> lcBrTit
*  ON KEY LABEL CTRL+Q    
*  ON KEY LABEL CTRL+W    
*  ON KEY LABEL CTRL+HOME 
*  ON KEY LABEL CTRL+END  
  *ON KEY LABEL ESC
*-b803351 (MHM) FIXING TRABING ESC KEY [ BEGIN ]
*  ON KEY LABEL ESC
*-b803351 (MHM) FIXING TRABING ESC KEY [ END ]   
*  ON KEY LABEL TAB 
*  ON KEY LABEL BACKTAB 
*ENDIF
=gfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTit)
*-b803351 (MHM) FIXING TRABING ESC KEY [ END ]   

*!*************************************************************
*! Name      : lfvDeact
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/25/1998
*! Purpose   : The READ Deactivate function of screen NAP500
*!*************************************************************
*! Example            :  lfvDeact()
*!*************************************************************
FUNCTION lfvDeact

*IF glFromBrow
*  = gfStopBrow()
*  glFromBrow = .F.
*ENDIF

IF WONTOP() = lcBrTit
  glFromBrow = .T.
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  *ON KEY LABEL ESC DO lpTrapKey WITH lcwindE, 'PbClose', .T.
  ON KEY LABEL ESC DO lpTrapKey WITH lcwindE, 'PbCls', .T.
  ON KEY LABEL TAB DO lpTrapKey WITH lcwindD, 'pbNew'
  ON KEY LABEL BACKTAB DO lpTrapKey WITH lcwindB, 'ladata[2]'
ENDIF

*!*************************************************************
*! Name      : lpTrapKey
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/25/1998
*! Purpose   : To handle the Trapping of keys
*!*************************************************************
*! Example            :  lpTrapKey()
*!*************************************************************
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName, llToCheck

ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF

*!*************************************************************
*! Name      : lfvActBrow
*! Developer : KHALID MOHI EL-DIN
*! Date      : 05/25/1998
*! Purpose   : Valid function for the browsing button (ibGroup)
*!*************************************************************
*! Example            :  lfvActBrow()
*!*************************************************************
FUNCTION lfvActBrow
PARAMETERS lcObjName

llBrowse = .T.
_CUROBJ = OBJNUM(&lcObjName)
KEYBOARD CHR(13)

*!*************************************************************
*! Name      : lfvOk
*! Developer : Ahmed Mohamed_El_Anwar Abd_El_Fatth (AME)
*! Date      : 26/3/2000
*! Purpose   : Valid function for the popup lagrp  (ibGroup)
*!*************************************************************
*! Example            :  lfvOk()
*!*************************************************************
FUNCTION lfvOk

SELECT IcDeptHd
SET ORDER TO cGroup
IF SEEK(lagrp[lngrp,2]) AND Dept <> ladata[1]
  =gfDialog("I","Group "+lagrp[lngrp,2]+;
              " has been assigned to another department. Cannot assign.")
  _CUROBJ = OBJNUM(lagrp[lngrp,2])
  SET ORDER TO DeptHd
  RETURN
ELSE
  IF EOF("IcDeptHd")
    llCUpdate = .T.
    GO TOP
  ENDIF
ENDIF

IF SEEK(lagrp[lngrp,2],TempHdr) AND &TempHdr..cStatus <> "D"
  =gfDialog("I","Group "+lagrp[lngrp,2]+" has been entered before.")        
  _CUROBJ = OBJNUM(lagrp[lngrp,2])
  SET ORDER TO DeptHd
  RETURN
ENDIF

SELECT (TempHdr)
APPEND BLANK
REPLACE Dept  WITH ladata[1],;
        cStyGroup   WITH laGrp[lnGrp,2],;
        cDeptDesc  WITH ladata[2] , ;
        cGrpDesc WITH RIGHT(laGrp[lnGrp,1],30),;
        cStatus  WITH "A"
SHOW WINDOW (lcBrTit) REFRESH SAME
CLEAR READ


*!*************************************************************
*! Name      : lpDelScr
*! Developer : Ahmed Mohamed_El_Anwar Abd_El_Fatth (AME)
*! Date      : 10/4/2000
*! Purpose   : Valid function for the popup lagrp  (ibGroup)
*!*************************************************************
*! Example            :  lpDelScr()
*!*************************************************************
FUNCTION lpDelScr
PRIVATE lnRecno 
IF gfDialog("?","Are you sure you want to delete this department "+;
                 ladata[1]+"?","\<Yes;\!\<No")=2
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

SELECT IcDeptHd
SET ORDER TO TAG DeptHd
=SEEK(ladata[1])
SCAN REST WHILE Dept+cStyGroup = ladata[1]
  DELETE
ENDSCAN
SELECT IcDeptDt
=SEEK(ladata[1])
SCAN REST WHILE Dept+cStyGroup+Style = ladata[1]
  WAIT WINDOW "Removing style : " +ALLTRIM(Style)+;
              " from group : "+cStyGroup NOWAIT
  DELETE
ENDSCAN
*--b803351 (MHM) Fixing Bug Of Not Delete Last Record From Temp File [Begin] 
SELECT (TempHdr)
SCAN for   Dept = ALLTRIM(ladata[1])
  DELETE
ENDSCAN
*--b803351 (MHM) [End] 
*B605993,1 TMI [Start] Remove Dept from style file
WAIT WINDOW NOWAIT 'Removing Department ID from Style file'
SELECT STYLE
lcStyOrd = ORDER('STYLE')
lnRecno = RECNO()
SET ORDER TO
GO TOP
REPLACE ALL DEPT WITH '     ' FOR DEPT = laData[1]
SET ORDER TO &lcStyOrd IN STYLE
IF BETWEEN(lnRecno,1,RECCOUNT('STYLE'))
  GOTO (lnRecno)
ENDIF  
*B605993,1 TMI [End  ] 
WAIT CLEAR
lascrmode = .F.
lascrmode[1] = .T.
=lpshow()
*--b803351 (MHM) Fixing Bug Of Not Delete Last Record From Temp File [Begin] 
SELECT IcDeptHd
GO TOP
*--b803351 (MHM)  [End] 
*:***************************************************************************
*: Name       : lfvBrows
*! Developer  : Ahmed Mohamed_El_Anwar Abd_El_Fatth (AME)   
*! Date       : 30/04/2000
*: Purpose    : "Valid" function of the Dept. browse.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : = lfvBrows()
*:***************************************************************************
FUNCTION lfvBrows

IF WONTOP() # lcBrTit AND glFromBrow
*  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*:***************************************************************************
*: Name       : lfwBrows
*! Developer  : Ahmed Mohamed_El_Anwar Abd_El_Fatth (AME)   
*! Date       : 30/04/2000
*: Purpose    : "WHEN" function of the Dept. browse.
*:***************************************************************************
*: Calls      : None.
*:***************************************************************************
*: Parameters : None.
*:***************************************************************************
*: Returns    : None.
*:***************************************************************************
*: Example    : = lfwBrows()
*:***************************************************************************
FUNCTION lfwBrows

IF WEXIST(lcBrTit)
  SHOW WINDOW (lcBrTit) REFRESH SAME
ENDIF


*IF WONTOP(lcBrTit)
  =lfvBrows()
*ENDIF

*!*************************************************************
*! Name      : lpNxtScr
*! Developer : Ahmed Mohamed_El_Anwer Abd_El_Fattah
*! Date      : 03/05/2000
*! Purpose   : local Next button procedure
*!*************************************************************
*! Example            : =lpNxtScr()
*!*************************************************************
FUNCTION lpNxtScr

lcOldDept = dept
SKIP -1
IF lcOldDept == dept
  SCAN WHILE DEPT = lcOldDept
  ENDSCAN
ELSE
  SKIP
ENDIF
IF EOF()
  GO BOTTOM
ENDIF
SCATTER FIELDS &lcScFields MEMO TO laData


*!*************************************************************
*! Name      : lpPrvScr
*! Developer : Ahmed Mohamed_El_Anwer Abd_El_Fattah
*! Date      : 03/05/2000
*! Purpose   : local Previous button procedure
*!*************************************************************
*! Example            : =lpPrvScr()
*!*************************************************************
FUNCTION lpPrvScr

lcOldDept = dept
SKIP 

IF lcOldDept == dept
  DO WHILE DEPT = lcOldDept
    IF BOF()
      EXIT
    ELSE
      SKIP -1
    ENDIF
  ENDDO
ELSE
  SKIP -1    
ENDIF
SCATTER FIELDS &lcScFields MEMO TO laData






*---------------------------   Last Line ------------------------------*
