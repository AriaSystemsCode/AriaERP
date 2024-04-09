*:***************************************************************************
*: Program file  : MFCARLB
*: Program desc. : Implement the care labels in ARIA 27
*: For screen    : MFCARLB.SCX
*:        System : Aria Advantage Series.
*:        Module : Manufactering Module (MF)
*:        Date   : 12/10/2000
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO MFCARLB
*:***************************************************************************
*: Due to C102075
*:***************************************************************************
*: Modifications :
*: B804244,1 AMH 06/09/2001 Fix Performance of collecting Data and adding
*: B804244,1                the soft seek.
*: B604671,1 AMH 07/22/2001 Check for a hardcoded text 'N/A' as default code.
*: B604671,1                This code will replace the empty check
*: B605006,1 AMH 10/10/2001 Add an option grid to select the styles.
*: B605113,1 AMH 11/11/2001 Fix the bug of not updating the care label for all the 
*: B605113,1                style's colors.
*:***************************************************************************
*

EXTERNAL ARRAY laDefProc,laCtrStat,laScrMode

laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .F.              && Disable the control panel close proc. (lpClsScr)
STORE ' ' TO laCodInfo
STORE ' ' TO laCarLb
STORE 1   TO lnCarLb
*B804244,1 AMH Variable to save old care label & variables for soft seek [Start]
STORE SPACE(0) TO lcOldCarLb
STORE ' ' TO lcExpToSeek
STORE 0   TO lnStartTrap, lnEndTrap
*B804244,1 AMH [End]
lcMajorPic = gfItemMask('PM')
lnMajorLen = LEN(lcMajorPic)

*B605006,1 AMH Add an icon for the scope in the control pannel [Start]
DECLARE laPanelObj[1,3]
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbScope'
laPanelObj[1,2] = gcBmpHome+'SCOPE.BMP'
laPanelObj[1,3] = [VALID lfvScope()]
llFirst = .T.
*B605006,1 AMH [End]

IF !gfSetup()
  RETURN
ENDIF  

laCtrStat[7] = "ENABLE"	&&Edit
laCtrStat[10] = "DISABLE"	&&se


lcTmpStyle = gfTempName()     && File to hold all Styles and to brows from it.
lcTmpStyl1 = gfTempName()     && Index of file lcTmpStyle by selection.
lcTmpStyl2 = gfTempName()     && Index of file lcTmpStyle by style.
lcTmpStyl3 = gfTempName()     && Index of file lcTmpStyle by status.

*B804244,1 AMH Add index of file lcTmpStyle by care label [Start]
lcTmpStyl4 = gfTempName()     && Index of file lcTmpStyle by care label.
*B804244,1 AMH [End]

lcTmpStySv = gfTempName()     && File to save all Styles.
lcTmpStyS1 = gfTempName()     && Index of file lcTmpStySvby style.

lcStyBrow = 'Styles'
llBrowse = .F.
*-- Define Screens Variables and Temporary file [End  ]

*-- Create Care Label Array [Begin]
DIMENSION laCodInfo [1,10]
laCodInfo[1,01] = "CCARLBL"
laCodInfo[1,02] = "laCarLb"
laCodInfo[1,03] = "lnCarLb"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.            
laCodInfo[1,06] = .F.            
laCodInfo[1,07] = "STYLE"     
laCodInfo[1,08] = "STYLE"     
laCodInfo[1,09] = ""
laCodInfo[1,10] = "cCarLbl"

DIMENSION laCarLb [1,2]
STORE "" TO laCarLb

*=gfwCodePop ( @laCodInfo, "CCARLBL" ,"L" )

*B604671,1 AMH don't add 'N/A' to popup [Start]
*= gfwCodePop ( @laCodInfo, "CCARLBL" ,"D" )
*lcDefCarLb = laCarLb[1,2]
=gfwCodePop ( @laCodInfo, "CCARLBL" ,"L" )
*lnCarLb = ASUBSCRIPT(lacarlb,ASCAN(laCarLb,lcDefCarLb),1)

IF ALEN(laCarLb,1) = 1 AND EMPTY(laCarLb[1,1])
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'You should have at least one care label code. Cannot proceed.')
  RETURN  
ENDIF

IF ASCAN(laCarLb,'N/A   ') > 0
  =ADEL(lacarlb,ASUBSCRIPT(laCarLb,ASCAN(laCarLb,'N/A   '),1))
  DIMENSION lacarlb[ALEN(laCarLb,1)-1,2]
ENDIF
*B605006,1 AMH Add select care label row [Start]
DIMENSION lacarlb[ALEN(laCarLb,1)+1,2]
=AINS(laCarLb,1)
laCarLb[1,1] = 'Select Care Label'
laCarLb[1,2] = ''
*B605006,1 AMH [End]
lnCarLb = 1
*B604671,1 AMH [End]

*-- Create Care Label Array [End  ]

*-- lnBrRecNo  : Browse Record Number
STORE 0 TO lnBrRecNo 

*-- Create Temp. File [Begin]
CREATE TABLE (gcWorkDir+lcTmpStyle) (cStyMajor C(19),desc C(20),cCarLbl C(3),cSelRec C(1),lSelRec L,cStatus C(1))
INDEX ON cSelRec TAG (lcTmpStyl1) OF (gcWorkDir+lcTmpStyl1)
INDEX ON cStatus TAG (lcTmpStyl3) OF (gcWorkDir+lcTmpStyl1)

*B804244,1 AMH Add index of file lcTmpStyle by care label [Start]
INDEX ON cCarLbl TAG (lcTmpStyl4) OF (gcWorkDir+lcTmpStyl1)
*B804244,1 AMH [End]

INDEX ON cStyMajor TAG (lcTmpStyl2) OF (gcWorkDir+lcTmpStyl1)
CREATE TABLE (gcWorkDir+lcTmpStySv) (cStyMajor C(19),desc C(20),cCarLbl C(3))
INDEX ON cStyMajor TAG (lcTmpStyS1) OF (gcWorkDir+lcTmpStyS1)

*B605006,1 AMH Comment the next line [Start]
*=lfGetCarLb(ALLTRIM(laCarlb(lnCarLb,2)))
*B605006,1 AMH [End]

*-- Create Temp. File [End  ]

=lfSetKey()
*B605006,1 AMH Comment the next line [Start]
*DO lpShow
*B605006,1 AMH [End]

DO (gcScrDir+gcWinAppl+'\MFCARLB.SPX')

*-- Screen Cleanup Code.
glQuitting = .T.  && Rise quit flag because it's modal screen.

POP KEY
RELEASE WINDOW (lcStyBrow)

IF USED(lcTmpStyle)
  USE IN (lcTmpStyle)
  ERASE &gcWorkdir.&lcTmpStyle..DBF          && Erase the Temp file.
ENDIF

IF FILE(gcWorkDir+lcTmpStyl1+'.CDX')
  ERASE &gcWorkdir.&lcTmpStyl1..CDX          && Erase the Temp file.
ENDIF

IF USED(lcTmpStySv)
  USE IN (lcTmpStySv)
  ERASE &gcWorkdir.&lcTmpStySv..DBF          && Erase the Temp file.
ENDIF


IF FILE(gcWorkDir+lcTmpStyS1+'.CDX')
  ERASE &gcWorkdir.&lcTmpStyS1..CDX          && Erase the Temp file.
ENDIF
*-- end of program code.

**********************************************************************
**************** Control Browse and trapping Functions ***************
**********************************************************************

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : Function to create the Browse
*!*************************************************************
*
FUNCTION lfDispBrow

SELECT (lcTmpStyle)
LOCATE
lnBrRecNo  = RECNO()

lcBrowFild = "lcMarker=IIF(RECNO()=lnBrRecNo , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "cSelRec :R :H= '»' ,"+;
             "cstymajor :H = 'Style'       :30:R,"+;
             "Desc  :H = 'Description' :40:R"

BROWSE FIELDS &lcBrowFild     ;
       WINDOW MFCARLB2        ;
       WHEN lfwBrows()        ;
       IN WINDOW MFCARLB ;
       LOCK 0                 ;
       NOAPPEND               ;
       NOCLEAR                ;
       NODELETE               ;
       NOWAIT                 ;
       NOEDIT                 ;
       NOMENU                 ;
       SAVE                   ;
       TITLE lcStyBrow

*-- end of lfDispBrow.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : When Browse Function.
*!*************************************************************
*
FUNCTION lfwBrows
lnBrRecNo  = RECNO(lcTmpStyle)
=lfvpbSel()
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : When function of the invisible button IbBrow
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*
FUNCTION lfwIbBrow
*-- IF The left mouse button is not pressed
IF !MDOWN()
  KEYBOARD "{ALT+B}" CLEAR 
  RETURN .T.
ENDIF    && End of IF
RETURN .F.
*-- end of lfwIbBrow.

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : Function to trap the keys for the Browse , 
*!             and save the changes if the current record was edited
*!*************************************************************
*
FUNCTION lfBrowTrap
*-- IF The window on top is the Browse
IF WONTOP(lcStyBrow)
  glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH "TAB"
  ON KEY LABEL BACKTAB DO lfTraps WITH "BACKTAB"
  ON KEY LABEL ALT+B 

  *B804244,1 AMH Add soft seek search screen [Start]
  IF SYS(21)<>"0"
    lcOrdExpr   = SYS(14,EVAL(SYS(21)))
    lcOrdExpT   = TYPE(lcOrdExpr)
    lnStartTrap = IIF(lcOrdExpT='N',48,32)
    lnEndTrap   = IIF(lcOrdExpT='N',57,126)
    FOR lnChrToTrap = lnStartTrap TO lnEndTrap
      ON KEY LABEL (CHR(lnChrToTrap)) DO lfIncSearch
    ENDFOR
  ENDIF  
  *B804244,1 AMH [End]

ENDIF    && End of IF
*-- end of lfBrowTrap.

*B804244,1 AMH Add soft seek search screen [Start]
*!**************************************************************************
*!
*!              Function: lfIncSearch
*!
*!**************************************************************************
FUNCTION lfIncSearch
PUSH KEY CLEAR
ON KEY
ON KEY LABEL F1 llDumi = .T.

PRIVATE lnBrRecNO,lcOrgTag

IF INLIST(LASTKEY(),27,13,9,15,32)
  CLEAR TYPEAHEAD
  POP KEY
  RETURN
ENDIF

DEFINE WINDOW lwIncSrch ;
  AT 21.000, 10.000 ;
  SIZE 4.167,55.444 ;
  FONT "FoxFont", 9 ;
  STYLE "B" TITLE "Aria Apparel System" ;
  FLOAT NOCLOSE SHADOW NOMINIMIZE NONE COLOR RGB(,,,192,192,192)

ACTIVATE WINDOW lwIncSrch NOSHOW

lcExpToSeek = UPPER(CHR(LASTKEY()))

IF RDLEVEL()=5
  lnBrCurObj = _CUROBJ
  lcToProc=[DO lfProcScreen WITH "DO lfActInSr"]

  CLEAR READ
  KEYBOARD "{ESCAPE}"

ELSE  

  PUSH KEY CLEAR
  CLEAR TYPEAHEAD

  DO lfActInSr
  POP KEY
  llActBrow = .T.
  _CUROBJ=_CUROBJ
ENDIF  

*!**************************************************************************
*!
*!              Function: lfActInSr
*!
*!**************************************************************************
FUNCTION lfActInSr
  @ 1.500,14.333 GET lcExpToSeek ;
    SIZE 1.167,41.000 PICTURE '@!' FONT "FoxFont", 9 MESSAGE "Press  'ENTER'  To  Locate" ;
    WHEN lfEndObj()
	
  @ 1.334,14.111 TO 1.334,51.115 PEN 2, 8 STYLE "1" ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 2.667,14.222 TO 2.667,51.115 PEN 2, 8 STYLE "1" ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 1.417,14.111 TO 2.750,14.111 PEN 2, 8 ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 1.417,50.893 TO 2.750,50.893 PEN 2, 8 ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 1.417,4.333 SAY "Locate         :"  FONT "MS Sans Serif", 8 ;
    STYLE "B"
	
  @ 0.000,0.000 TO 0.000,55.444 PEN 1, 8 STYLE "1" ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.333,0.444 TO 0.333,55.000 PEN 1, 8 STYLE "1" ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 4.083,0.000 TO 4.083,55.444 PEN 1, 8 STYLE "1" ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 3.750,0.444 TO 3.750,55.000 PEN 1, 8 STYLE "1" ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.000,0.000 TO 4.167,0.000 PEN 1, 8 ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.333,0.444 TO 3.833,0.444 PEN 1, 8 ;
    COLOR RGB(128,128,128,128,128,128)
	
  @ 0.333,54.889 TO 3.833,54.889 PEN 1, 8 ;
    COLOR RGB(255,255,255,255,255,255)
	
  @ 0.000,55.333 TO 4.167,55.333 PEN 1, 8 ;
    COLOR RGB(128,128,128,128,128,128)

ACTIVATE WINDOW lwIncSrch TOP

ACTIVATE WINDOW (lcStyBrow)
READ MODAL
RELEASE WINDOW lwIncSrch
lcExpToSeek = ALLTRIM(lcExpToSeek)

IF LASTKEY() = 13
  lnBrRecNO = IIF(RECNO()>RECCOUNT(),0,RECNO())

    IF SEEK(lcExpToSeek)
      CLEAR TYPEAHEAD
      POP KEY
      RETURN
    ELSE
      IF lnBrRecNO>0
        GO lnBrRecNO
      ENDIF  
    ENDIF
ELSE
  lcExpToSeek = ''
ENDIF
=lfDispBrow()

ON KEY
CLEAR TYPEAHEAD
POP KEY
RETURN
*B804244,1 AMH [End]

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*
FUNCTION lfBrwUnTrp

*-- IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcStyBrow) .AND. glFromBrow
  = gfStopBrow()
  glFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcStyBrow)

  *B804244,1 AMH Add soft seek search screen [Start]
  FOR lnChrToTrap = lnStartTrap TO lnEndTrap
    ON KEY LABEL (CHR(lnChrToTrap))
  ENDFOR
  *B804244,1 AMH [End]
  
ENDIF    && End of IF
*-- end of lfBrwUnTrp.

*!*************************************************************
*! Name      : lfTraps
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : Trap (Tab / Backtab) Keys
*!*************************************************************
*
FUNCTION lfTraps
PARAMETERS lcTrap
*-- Tab Case
IF lcTrap = "TAB"
  ACTI WINDOW MFCARLB3 TOP
  _CUROBJ = OBJNUM(pbSelect)

ELSE  && Backtab Case.
  ACTI WINDOW MFCARLB1 TOP
  _CUROBJ = OBJNUM(lnCarLb)
ENDIF
*-- end of lfTraps.

**********************************************************************
********************** Control Screen Functions **********************
**********************************************************************

*!*************************************************************
*! Name      : lfvSelect
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To validate the Select/UnSelect button.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*!
FUNCTION lfvSelect

PRIVATE lcMsg, lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpStyle)
lcMsg = "Style "+ALLTRIM(cStyMajor)+" is assigned to care label "+cCarLbl+;
        ". Are you sure you want to assign it to care label "+ALLTRIM(laCArLb(lnCarlb,1))+"?"

*B604671,1 AMH Check for a hardcoded text 'N/A' [Start]
*IF !EMPTY(cCarLbl) .AND. cCarLbl <> ALLTRIM(laCArLb(lnCarlb,2)) .AND.;
   gfModalGen("TRM00000B38006","DIALOG","Care Label",.F.,lcMsg) = 2
IF ALLTRIM(cCarLbl) <> 'N/A' .AND. cCarLbl <> ALLTRIM(laCArLb(lnCarlb,2)) .AND.;
   gfModalGen("TRM00000B38006","DIALOG","Care Label",.F.,lcMsg) = 2
*B604671,1 AMH [End]

  RETURN
ENDIF
IF cStatus = 'A' .AND. lSelRec .AND.;
   gfModalGen("TRM00000B38006","DIALOG","Care Label",.F.,;
              'Are you sure you want to remove the care label assigned for the following style: '+ALLTRIM(cStyMajor)+'?') = 2
  RETURN
ENDIF
REPLACE lSelRec WITH !lSelRec
REPLACE cSelRec WITH IIF(lSelRec,'»',' ')

*B604671,1 AMH Replace a hardcoded text 'N/A' for not selected style [Start]
*REPLACE cCarLbl WITH IIF(lSelRec,ALLTRIM(laCArLb(lnCarlb,2)),'')
REPLACE cCarLbl WITH IIF(lSelRec,ALLTRIM(laCArLb(lnCarlb,2)),'N/A')
*B604671,1 AMH [End]

=lfChangSt()
=lfvpbSel()
SELECT (lnAlias)
llCUpDate = .T.
SHOW GET lnCarLb  DISABLE
*-- end of lfvSelect.

*!*************************************************************
*! Name      : lfvSelAll
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To validate the Select All button.
*!*************************************************************
*!
FUNCTION lfvSelAll

PRIVATE lcMsg, lnMsg, llAll, lnAlias
llAll = .F.

lnAlias = SELECT(0)
SELECT (lcTmpStyle)
lnBrRecNo = RECNO()
SCAN FOR !lSelRec

  *B604671,1 AMH Check for a hardcoded text 'N/A' [Start]
  *IF !EMPTY(cCarLbl) .AND. cCarLbl <> ALLTRIM(laCArLb(lnCarlb,2))
  IF ALLTRIM(cCarLbl) <> 'N/A'  .AND. cCarLbl <> ALLTRIM(laCArLb(lnCarlb,2))
  *B604671,1 AMH [End]

    IF !llAll
      lcMsg = "Style "+ALLTRIM(cStyMajor)+" is assigned to care label "+cCarLbl+;
        ". Are you sure you want to assign it to care label "+ALLTRIM(laCArLb(lnCarlb,1))+"?"
      lnMsg = gfModalGen("TRM00000B38026","DIALOG","Care Label",.F.,lcMsg)
      llAll = IIF(MOD(lnMsg,2)=0,.T.,.F.)
    ENDIF
    IF lnMsg > 2
      LOOP
    ENDIF
  ENDIF
  =lfChangSt()
  REPLACE lSelRec WITH .T.
  REPLACE cSelRec WITH '»'
  REPLACE cCarLbl WITH ALLTRIM(laCArLb(lnCarlb,2))
ENDSCAN  
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GOTO lnBrRecNo
ENDIF
=lfvpbSel()
SELECT (lnAlias)
llCUpDate = .T.
SHOW GET lnCarLb  DISABLE
*-- end of lfvSelAll.

*!*************************************************************
*! Name      : lfvSelNon
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To validate the Select None button.
*!*************************************************************
*!
FUNCTION lfvSelNon

PRIVATE lnMsg, llAll, lnAlias
llAll = .F.

lnAlias = SELECT(0)
SELECT (lcTmpStyle)
lnBrRecNo = RECNO()
SCAN FOR lSelRec
  IF cStatus = 'A'
    IF !llAll
      lnMsg = gfModalGen("TRM00000B38026","DIALOG","Care Label",.F.,;
               'Are you sure you want to remove the care label assigned for the following style: '+ALLTRIM(cStyMajor)+'?')
      llAll = IIF(MOD(lnMsg,2)=0,.T.,.F.)
    ENDIF
    IF lnMsg > 2
      LOOP
    ENDIF
  ENDIF
  =lfChangSt()

  *B604671,1 AMH Replace a hardcoded text 'N/A' for not selected style [Start]
  *REPLACE lSelRec WITH .F.,;
          cSelRec WITH ' ',;
          cCarLbl WITH ''
  REPLACE lSelRec WITH .F.,;
          cSelRec WITH ' ',;
          cCarLbl WITH 'N/A'
  *B604671,1 AMH [End]

ENDSCAN  
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF  
=lfvpbSel()
SELECT (lnAlias)
llCUpDate = .T.
SHOW GET lnCarLb  DISABLE
*-- end of lfvSelNon.

*!*************************************************************
*! Name      : lfvInvert
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To validate the Invert button.
*!*************************************************************
*!
FUNCTION lfvInvert

PRIVATE lcMsg, lnMsg, llAll, lnMsgNon, llAllNon, lnAlias
llAll = .F.
llAllNon = .F.
lnAlias = SELECT(0)
SELECT (lcTmpStyle)
SCAN
  
  *B604671,1 AMH Check for a hardcoded text 'N/A' [Start]
  *IF !EMPTY(cCarLbl) .AND. cCarLbl <> ALLTRIM(laCArLb(lnCarlb,2))
  IF ALLTRIM(cCarLbl) <> 'N/A' .AND. cCarLbl <> ALLTRIM(laCArLb(lnCarlb,2))
  *B604671,1 AMH [End]
  
    IF !llAll
      lcMsg = "Style "+ALLTRIM(cStyMajor)+" is assigned to care label "+cCarLbl+;
        ". Are you sure you want to assign it to care label "+ALLTRIM(laCArLb(lnCarlb,1))+"?"
      lnMsg = gfModalGen("TRM00000B38026","DIALOG","Care Label",.F.,lcMsg)
      llAll = IIF(MOD(lnMsg,2)=0,.T.,.F.)
    ENDIF
    IF lnMsg > 2
      LOOP
    ENDIF
  ENDIF
  IF cStatus = 'A' .AND. lSelRec
    IF !llAllNon
      lnMsgNon = gfModalGen("TRM00000B38026","DIALOG","Care Label",.F.,;
                 'Are you sure you want to remove the care label assigned for the following style: '+ALLTRIM(cStyMajor)+'?')
      llAllNon = IIF(MOD(lnMsgNon,2)=0,.T.,.F.)
    ENDIF
    IF lnMsgNon > 2
      LOOP
    ENDIF
  ENDIF
  REPLACE lSelRec WITH !lSelRec
  REPLACE cSelRec WITH IIF(lSelRec,'»',' ')

  *B604671,1 AMH Replace a hardcoded text 'N/A' for not selected style [Start]
  *REPLACE cCarLbl WITH IIF(lSelRec,ALLTRIM(laCArLb(lnCarlb,2)),'')
  REPLACE cCarLbl WITH IIF(lSelRec,ALLTRIM(laCArLb(lnCarlb,2)),'N/A')
  *B604671,1 AMH [End]

  =lfChangSt()
ENDSCAN  
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF  
=lfvpbSel()
SELECT (lnAlias)
llCUpDate = .T.
SHOW GET lnCarLb  DISABLE
*-- end of lfvInvert.

*!*************************************************************
*! Name      : lfvRange
*! Developer : AMH (AMHED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To validate the Range button.
*!*************************************************************
*!
FUNCTION lfvRange

=lfClearKey()
DO (gcScrDir+gcWinAppl+'\MFGETRNG.SPX')
=lfSetKey()
=lfvpbSel()
SHOW GET lnCarLb  DISABLE
*-- end of lfvRange.

*!*************************************************************
*! Name      : lfvSelAllR
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To validate the Select All button in Range window.
*!*************************************************************
*!
FUNCTION lfvSelAllR

PRIVATE lcMsg, lnMsg, llAll, lnAlias
llAll = .F.

IF EMPTY(lcFrom) AND EMPTY(lcTo)
  RETURN
ENDIF

lnAlias = SELECT(0)
SELECT (lcTmpStyle)
IF !EMPTY(lcFrom)
  SEEK lcFrom
ELSE
  LOCATE
ENDIF
IF EMPTY(lcTo)
  lcScanExpr = "REST WHILE !EOF() FOR !lSelRec"
ELSE  
  lcScanExpr = "REST WHILE BETWEEN(cStyMajor,lcFrom,lcTo) FOR !lSelRec"
ENDIF  
SCAN &lcScanExpr

  *B604671,1 AMH Check for a hardcoded text 'N/A' [Start]
  *IF !EMPTY(cCarLbl) .AND. cCarLbl <> ALLTRIM(laCArLb(lnCarlb,2))
  IF ALLTRIM(cCarLbl) <> 'N/A' .AND. cCarLbl <> ALLTRIM(laCArLb(lnCarlb,2))
  *B604671,1 AMH [End]
  
    IF !llAll
      lcMsg = "Style "+ALLTRIM(cStyMajor)+" is assigned to care label "+cCarLbl+;
        ". Are you sure you want to assign it to care label "+ALLTRIM(laCArLb(lnCarlb,1))+"?"
      lnMsg = gfModalGen("TRM00000B38026","DIALOG","Care Label",.F.,lcMsg)
      llAll = IIF(MOD(lnMsg,2)=0,.T.,.F.)
    ENDIF
    IF lnMsg > 2
      LOOP
    ENDIF
  ENDIF
  REPLACE lSelRec WITH .T.
  REPLACE cSelRec WITH '»'
  REPLACE cCarLbl WITH ALLTRIM(laCArLb(lnCarlb,2))
  =lfChangSt()
ENDSCAN  
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF  
SELECT (lnAlias)
CLEAR READ
llCUpDate = .T.
*-- end of lfvSelAllR.

*!*************************************************************
*! Name      : lfvSelNonR
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To validate the Select None button in range window.
*!*************************************************************
*!
FUNCTION lfvSelNonR

PRIVATE lnMsg, llAll, lnAlias
llAll = .F.
IF EMPTY(lcFrom) AND EMPTY(lcTo)
  RETURN
ENDIF

lnAlias = SELECT(0)
SELECT (lcTmpStyle)
IF !EMPTY(lcFrom)
  SEEK lcFrom
ELSE
  LOCATE
ENDIF
IF EMPTY(lcTo)
  lcScanExpr = "REST WHILE !EOF() FOR lSelRec"
ELSE  
  lcScanExpr = "REST WHILE BETWEEN(cStyMajor,lcFrom,lcTo) FOR lSelRec"
ENDIF  
SCAN &lcScanExpr
  IF cStatus = 'A'
    IF !llAll
      lnMsg = gfModalGen("TRM00000B38026","DIALOG","Care Label",.F.,;
               'Are you sure you want to remove the care label assigned for the following style: '+ALLTRIM(cStyMajor)+'?')
      llAll = IIF(MOD(lnMsg,2)=0,.T.,.F.)
    ENDIF
    IF lnMsg > 2
      LOOP
    ENDIF
  ENDIF
  REPLACE lSelRec WITH .F.
  REPLACE cSelRec WITH ' '  

  *B604671,1 AMH Replace a hardcoded text 'N/A' for not selected style [Start]
  *REPLACE cCarLbl WITH ''
  REPLACE cCarLbl WITH 'N/A'
  *B604671,1 AMH [End]

  =lfChangSt()
ENDSCAN  
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF  
SELECT (lnAlias)
CLEAR READ
llCUpDate = .T.
*-- end of lfvSelNonR.

*!*************************************************************
*! Name      : lfvInvertR
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To validate the Invert button in range window.
*!*************************************************************
*!
FUNCTION lfvInvertR

PRIVATE lcMsg, lnMsg, llAll, lnMsgNon, llAllNon, lnAlias
llAll = .F.
llAllNon = .F.

IF EMPTY(lcFrom) AND EMPTY(lcTo)
  RETURN
ENDIF

lnAlias = SELECT(0)
SELECT (lcTmpStyle)
IF !EMPTY(lcFrom)
  SEEK lcFrom
ELSE
  LOCATE
ENDIF
IF EMPTY(lcTo)
  lcScanExpr = "REST WHILE !EOF()"
ELSE  
  lcScanExpr = "REST WHILE BETWEEN(cStyMajor,lcFrom,lcTo)"
ENDIF  
SCAN &lcScanExpr

  *B604671,1 AMH Check for a hardcoded text 'N/A' [Start]
  *IF !EMPTY(cCarLbl) .AND. cCarLbl <> ALLTRIM(laCArLb(lnCarlb,2))
  IF ALLTRIM(cCarLbl) <> 'N/A' .AND. cCarLbl <> ALLTRIM(laCArLb(lnCarlb,2))
  *B604671,1 AMH [End]
  
    IF !llAll
      lcMsg = "Style "+ALLTRIM(cStyMajor)+" is assigned to care label "+cCarLbl+;
        ". Are you sure you want to assign it to care label "+ALLTRIM(laCArLb(lnCarlb,1))+"?"
      lnMsg = gfModalGen("TRM00000B38026","DIALOG","Care Label",.F.,lcMsg)
      llAll = IIF(MOD(lnMsg,2)=0,.T.,.F.)
    ENDIF
    IF lnMsg > 2
      LOOP
    ENDIF
  ENDIF
  IF cStatus = 'A' .AND. lSelRec
    IF !llAllNon
      lnMsgNon = gfModalGen("TRM00000B38026","DIALOG","Care Label",.F.,;
                 'Are you sure you want to remove the care label assigned for the following style: '+ALLTRIM(cStyMajor)+'?')
      llAllNon = IIF(MOD(lnMsgNon,2)=0,.T.,.F.)
    ENDIF
    IF lnMsgNon > 2
      LOOP
    ENDIF
  ENDIF
  REPLACE lSelRec WITH !lSelRec
  REPLACE cSelRec WITH IIF(lSelRec,'»',' ')

  *B604671,1 AMH Replace a hardcoded text 'N/A' for not selected style [Start]
  *REPLACE cCarLbl WITH IIF(lSelRec,ALLTRIM(laCArLb(lnCarlb,2)),'')
  REPLACE cCarLbl WITH IIF(lSelRec,ALLTRIM(laCArLb(lnCarlb,2)),'N/A')
  *B604671,1 AMH [End]

  =lfChangSt()
ENDSCAN  
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF  
SELECT (lnAlias)
CLEAR READ
llCUpDate = .T.
*-- end of lfvInvertR.

*!*************************************************************
*! Name      : lfvpbSel
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : if Current status is Select prompt is Un-Select and vice versa
*!*************************************************************
*
FUNCTION lfvpbSel
PRIVATE lnRecNo
IF laScrMode[3]
  SELECT (lcTmpStyle)
  SHOW GET pbSelect,1 PROMPT IIF(lSelRec,'UnSe\<lect','Se\<lect')

  lnRecNo = RECNO()
  SET ORDER TO (lcTmpStyl1)
  IF SEEK(' ')
    SHOW GET pbSelAll ENABLE
  ELSE
    SHOW GET pbSelAll DISABLE
  ENDIF

  IF SEEK('»')
    SHOW GET pbSelNon ENABLE
  ELSE
    SHOW GET pbSelNon DISABLE
  ENDIF
  SET ORDER TO (lcTmpStyl2)
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF  
  *B605006,1 AMH disable selection bottons in case of lnCarLb = 1 [Start]
  IF lnCarLb = 1
    SHOW GET pbSelect DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbSelNon DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbRange  DISABLE
  ENDIF
  *B605006,1 AMH [End]
ENDIF
SHOW WINDOW (lcStyBrow) REFRESH
*-- end of lfvpbSel.

*!*************************************************************
*! Name      : lfSetKey
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : Set Active Keys
*!*************************************************************
*!
FUNCTION lfSetKey
PUSH KEY

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcStyBrow)
ON KEY LABEL Alt+L DO lfvSelect
ON KEY LABEL Alt+A DO lfvSelAll
ON KEY LABEL Alt+N DO lfvSelNon
ON KEY LABEL Alt+I DO lfvInvert
ON KEY LABEL Alt+R DO lfvRange
ON KEY LABEL Alt+O DO lfvOk
ON KEY LABEL Alt+C DO lfvCan
ON KEY LABEL ESC   DO gfCPClose
*-- end of lfSetKey.

*!*************************************************************
*! Name      : lfClearKey
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : Clear Active Keys
*!*************************************************************
*!
FUNCTION lfClearKey
ON KEY LABEL Alt+L
ON KEY LABEL Alt+A
ON KEY LABEL Alt+N
ON KEY LABEL Alt+I
ON KEY LABEL Alt+R
ON KEY LABEL Alt+O
ON KEY LABEL Alt+S
ON KEY LABEL Alt+C
ON KEY LABEL ESC
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+B
*-- end of lfClearKey.

*!*************************************************************
*! Name      : lfvCarLb
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : Valed function of Care Label PopUp
*!*************************************************************
*!
FUNCTION lfvCarLb

*B804244,1 AMH Refresh data only [Start]
*=lfDelBrow()
*=lfGetCarLb(ALLTRIM(laCArLb(lnCarlb,2)))
=lfRefCarLb(PADL(laCarLb(lnCarlb,2),3))
*B804244,1 AMH [End]
=lfDispBrow()
=lfvpbSel()
=lfRefresh()
IF laScrMode[3]
  SHOW GET lnCarLb  DISABLE
ENDIF
*B605006,1 AMH Disable/enable edit botton [Satrt]
DO CASE
  CASE lnCarLb = 1 .AND. laScrMode[2]
    laScrMode = .F.
    laScrMode[1] = .T.
    DO lpShow
  CASE lnCarLb # 1 .AND. laScrMode[1]
    laScrMode = .F.
    laScrMode[2] = .T.
    DO lpShow
  CASE lnCarLb # 1 .AND. laScrMode[3]
    SHOW GET pbInvert ENABLE
    SHOW GET pbRange  ENABLE
ENDCASE

*B605006,1 AMH [End]
*-- end of lfvCarLb.

*B804244,1 AMH Function to save old Care label [Start]
*!*************************************************************
*! Name      : lfwCarLb
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : When function of Care Label PopUp
*!*************************************************************
*!
FUNCTION lfwCarLb

lcOldCarLb = PADR(laCarLb(lnCarlb,2),3)
*-- end of lfwCarLb.
*B804244,1 AMH Function to save old Care label [End]

*!*************************************************************
*! Name      : lfGetCarLb
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : Get the Care Label for all styles.
*!*************************************************************
*!
FUNCTION lfGetCarLb
*B605006,1 AMH Collecting only selected styles [Start]
*PARAMETER lcCarLb

*SELECT STYLE
*SCAN
  *WAIT WINDOW "Selecting style: "+ALLTRIM(cstymajor) NOWAIT
*  SCATTER FIELDS cstymajor, desc, cCarLbl MEMVAR
*  m.cSelRec = IIF(CCARLBL=lcCarLb,'»',' ')
*  m.lSelRec = IIF(CCARLBL=lcCarLb,.T.,.F.)
*  m.cStatus = 'A'
*  IF !SEEK(SUBSTR(cStyMajor,1,lnMajorLen),lcTmpStyle)
*    SELECT (lcTmpStyle)
*    APPEND BLANK
*    GATHER MEMVAR    
*  ENDIF
*ENDSCAN
*WAIT CLEAR
*=lfReplace(lcTmpStyle,lcCarLb)
llUseStyle = IIF(!EMPTY(lcSelStyle) .AND. USED(lcSelStyle) .AND. RECCOUNT(lcSelStyle)>0,.T.,.F.)
llUseSeaso = !EMPTY(laRpTarSea[1])
IF llUseStyle
  IF llUseSeaso
    lcSeason = ''
    FOR lnI = 1 TO ALEN(laRpTarSea)
      lcSeason = lcSeason + LEFT(laRpTarSea[lnI],3) + '|'
    ENDFOR
  ENDIF
  SELECT (lcSelStyle)
  SCAN FOR IIF(llUseSeaso,LEFT(CSTYMAJOR,3)$lcSeason,.T.)
    =SEEK(LEFT(CSTYMAJOR,lnMajorLen),'STYLE')
    SELECT STYLE
    SCATTER FIELDS cstymajor, desc, cCarLbl MEMVAR
    m.cSelRec = ' '
    m.lSelRec = .F.
    m.cStatus = 'A'
    SELECT (lcTmpStyle)
    APPEND BLANK
    GATHER MEMVAR    
  ENDSCAN
ELSE
  IF llUseSeaso
    FOR lnI = 1 TO ALEN(laRpTarSea)
      SELECT STYLE
      SEEK LEFT(laRpTarSea[lnI],3)
      SCAN REST WHILE LEFT(STYLE,3) = LEFT(laRpTarSea[lnI],3)
        SCATTER FIELDS cstymajor, desc, cCarLbl MEMVAR
        m.cSelRec = ' '
        m.lSelRec = .F.
        m.cStatus = 'A'
        IF !SEEK(SUBSTR(cStyMajor,1,lnMajorLen),lcTmpStyle)
          SELECT (lcTmpStyle)
          APPEND BLANK
          GATHER MEMVAR
        ENDIF
      ENDSCAN
    ENDFOR
  ELSE
    SELECT STYLE
    SCAN
      SCATTER FIELDS cstymajor, desc, cCarLbl MEMVAR
      m.cSelRec = ' '
      m.lSelRec = .F.
      m.cStatus = 'A'
      IF !SEEK(SUBSTR(cStyMajor,1,lnMajorLen),lcTmpStyle)
        SELECT (lcTmpStyle)
        APPEND BLANK
        GATHER MEMVAR    
      ENDIF
    ENDSCAN
  ENDIF
ENDIF
IF USED(lcSelStyle)
  USE IN (lcSelStyle)
ENDIF
SELECT (lcTmpStyle)
LOCATE
IF EOF()
  =gfModalGen('INM38129B38018','DIALOG','styles')
ENDIF
*B605006,1 AMH [End]
*-- end of lfGetCarLb.

*!*************************************************************
*! Name      : lfDelBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To Delete the data of the Browser
*!*************************************************************
*! Example            :  lfDelBrow()
*!*************************************************************
FUNCTION lfDelBrow

IF USED(lcTmpStyle)
  SELECT(lcTmpStyle)
  ZAP
ENDIF
*-- end of lfDelBrow.

*!*************************************************************
*! Name      : lfvMajor
*! Developer : Ahmed Maher (AMH)
*! Date      : 12/12/2000
*! Purpose   : Valid function to validate From/To fields.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : lcMajor (lcFrom or lcTo)
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvMajor
PARAMETER lcField

PRIVATE llCmFrmBrw , lcMajor
STORE .F. TO llCmFrmBrw
lcMajor = ALLTRIM(IIF(lcField = 'FROM',lcFrom,lcTo))

IF llBrowse OR LASTKEY()=13 OR LASTKEY()=9
  *--Ok start validate.
ELSE
  RETURN
ENDIF

IF llbrowse OR !EMPTY(lcMajor) .OR. !SEEK(lcMajor,'STYLE')
  IF llbrowse OR ATC('?',lcMajor) <> 0
    llbrowse = .F.
    lcMajor = gfStyBrw('M',"","",.T.)
    llCmFrmBrw = .T.
  ENDIF
  IF !SEEK(lcMajor,'STYLE') .AND. !EMPTY(lcMajor)
    lcMajor = gfStyBrw('M',lcMajor,"",.F.)
    llCmFrmBrw = .T.
  ENDIF
ENDIF
IF lcField = 'FROM'
  lcFrom = PADR(lcMajor,19)
ELSE
  lcTo = PADR(lcMajor,19)
ENDIF
RETURN
*-- end of lfMajor.

*!*************************************************************
*! Name      : lfvCancel
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To validate the Cancel button in Range window.
*!*************************************************************
*!
FUNCTION lfvCancel

CLEAR READ
*-- end of lfvCancel.

**********************************************************************
************************ Business Rules Section **********************
**********************************************************************

*!*************************************************************
*! Name      : lfvOk
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To validate the Ok button.
*!*************************************************************
*! Example            :  lfvOk()
*!*************************************************************
FUNCTION lfvOk

SELECT (lcTmpStyle)
lnBrRecNo = RECNO()
SET ORDER TO (lcTmpStyl3)
SEEK 'S'
SCAN REST WHILE cStatus = 'S'
  SCATTER MEMVAR
  SELECT (lcTmpStySv)
  IF !SEEK(&lcTmpStyle..cStyMajor)
    APPEND BLANK
  ENDIF
  GATHER MEMVAR
ENDSCAN
SELECT (lcTmpStySv)
LOCATE
IF !EOF()
  SHOW GET pbSav ENABLE
ENDIF
SELECT (lcTmpStyle)
SET ORDER TO (lcTmpStyl2)
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GOTO lnBrRecNo
ENDIF
llCUpDate = .T.
SHOW GET lnCarLb ENABLE
*-- end of lfvOk.

*!*************************************************************
*! Name      : lfvCan
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To validate the Cancel button.
*!*************************************************************
*!
FUNCTION lfvCan

*B804244,1 AMH Restore data only [Start]
*=lfvCarLb()
=lfRstCarLb(PADL(laCarLb(lnCarlb,2),3))
*B804244,1 AMH [End]
SHOW GET lnCarLb ENABLE
*-- end of lfvCan.

*!*************************************************************
*! Name      : lpSavScr
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To Save care label in Style file.
*!*************************************************************
*!
PROCEDURE lpSavScr

=lfReplace('STYLE','')
SHOW GET pbSelect DISABLE
SHOW GET pbSelAll DISABLE
SHOW GET pbSelNon DISABLE
SHOW GET pbInvert DISABLE
SHOW GET pbRange  DISABLE
SHOW GET pbOk     DISABLE
SHOW GET pbCan    DISABLE
*-- end of lpSavScr.

*!*************************************************************
*! Name      : lpClsScr
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : To Cancel all changes.
*!*************************************************************
*!
PROCEDURE lpClsScr

IF laScrMode[3]
  *B804244,1 AMH Resotre old Data only [Start]
  *=lfDelBrow()
  IF USED(lcTmpStySv)
    SELECT(lcTmpStySv)
    ZAP
  ENDIF
  *=lfGetCarLb(ALLTRIM(laCArLb(lnCarlb,2)))
  =lfRstCarLb(PADL(laCarLb(lnCarlb,2),3))
  *B804244,1 AMH [End]
  =lfDispBrow()
ENDIF
*-- end of lpClsScr.

*!*************************************************************
*! Name      : lfReplace
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : Replace the cCarLbl field.
*!*************************************************************
*!
FUNCTION lfReplace
PARAMETER lcAlias,lcCarLb

SELECT (lcTmpStySv)
SCAN
  IF SEEK(SUBSTR(cStyMajor,1,lnMajorLen),lcAlias)
    SELECT (lcAlias)
    IF lcAlias = "STYLE"
      *WAIT WINDOW "Updating style: "+ALLTRIM(cstymajor) NOWAIT
      =RLOCK()
      *REPLACE cCarLbl WITH &lcTmpStySv..cCarLbl ;
      REST WHILE STYLE = SUBSTR(cStyMajor,1,lnMajorLen)
      REPLACE cCarLbl WITH &lcTmpStySv..cCarLbl ;
      REST WHILE STYLE = SUBSTR(&lcTmpStySv..cStyMajor,1,lnMajorLen)
      UNLOCK
    ENDIF  
    IF lcAlias = lcTmpStyle
      REPLACE cCarLbl WITH &lcTmpStySv..cCarLbl
      REPLACE cSelRec WITH IIF(CCARLBL=lcCarLb,'»',' ')
      REPLACE lSelRec WITH IIF(CCARLBL=lcCarLb,.T.,.F.)
    ENDIF
  ENDIF
ENDSCAN
WAIT CLEAR
*-- end of lfReplace.

*!*************************************************************
*! Name      : lpShow
*! Developer : AMH (AHMED MAHER)
*! Date      : 13/10/2000
*! Purpose   : Handle controfl show.
*!*************************************************************
*!
PROCEDURE lpShow

DO CASE
  CASE laScrMode[1]                       && Select mode.
    *B605006,1 AMH Enable/disable at select mode [Start]
    *laScrMode = .F.
    *laScrMode[2] = .T.
    SHOW GET pbSlct   DISABLE
    SHOW GET pbDlt    DISABLE
    SHOW GET pbSelect DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbRange  DISABLE
    SHOW GET pbOk     DISABLE
    SHOW GET pbCan    DISABLE
    SHOW GET lnCarLb  ENABLE
    SHOW GET pbScope  ENABLE
    SHOW GET pbEdt    DISABLE
    IF llFirst
      llFirst = .F.
      =lfvScope()
    ENDIF
    *B605006,1 AMH [End]
  CASE laScrMode[2]                       && View mode 
    SHOW GET pbSlct   DISABLE
    SHOW GET pbDlt    DISABLE
    SHOW GET pbSelect DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbRange  DISABLE
    SHOW GET pbOk     DISABLE
    SHOW GET pbCan    DISABLE
    SHOW GET lnCarLb ENABLE
    *B605006,1 AMH enable the scope & edit bottons [Start]
    SHOW GET pbScope  ENABLE
    SHOW GET pbEdt    ENABLE
    *B605006,1 AMH [End]
  CASE laScrMode[3]                       && Edit mode 
    SHOW GET lnCarLb  DISABLE
    SHOW GET pbSelect ENABLE
    SHOW GET pbInvert ENABLE
    SHOW GET pbRange  ENABLE
    SHOW GET pbOk     ENABLE
    SHOW GET pbCan    ENABLE
    SHOW GET pbSav    DISABLE
    *B605006,1 AMH disable the scope botton [Start]
    SHOW GET pbScope  DISABLE
    *B605006,1 AMH [End]
    STORE 'ENABLE' TO lcSelSt
    =lfvpbSel()
ENDCASE
*-- end of lpShow.

*!*************************************************************
*! Name      : lfChangSt
*! Developer : AMH (AHMED MAHER)
*! Date      : 12/10/2000
*! Purpose   : chang the status of saving records.
*!*************************************************************
*!
FUNCTION lfChangSt

REPLACE cStatus WITH IIF(cStatus='A','S','A')
*-- end of lfChangSt.

* B804244,1 AMH Function to refresh lcTmpStyle file [Start]
*!*************************************************************
*! Name      : lfRefCarLb
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/09/2000
*! Purpose   : Refresh the Care Label for all styles.
*!*************************************************************
*!
FUNCTION lfRefCarLb
PARAMETER lcCarLb

PRIVATE lcOrder
SELECT (lcTmpStyle)
lcOrder = SET('ORDER')
SET ORDER TO (lcTmpStyl4)
SEEK lcOldCarLb
SCAN REST WHILE cCarLbl = lcOldCarLb
  *WAIT WINDOW "UnSelecting style: "+ALLTRIM(cstymajor) NOWAIT
  REPLACE cSelRec WITH ' ',;
          lSelRec WITH .F.,;
          cStatus WITH 'A'
ENDSCAN
SEEK lcCarLb
SCAN REST WHILE cCarLbl = lcCarLb
  *WAIT WINDOW "Selecting style: "+ALLTRIM(cstymajor) NOWAIT
  REPLACE cSelRec WITH '»',;
          lSelRec WITH .T.,;
          cStatus WITH 'A'
ENDSCAN
WAIT CLEAR
SET ORDER TO &lcOrder.
=lfReplace(lcTmpStyle,lcCarLb)
SELECT (lcTmpStyle)
LOCATE
*-- end of lfRefCarLb.
* B804244,1 AMH [End]

*B804244,1 AMH Function to restore old data of lcTmpStyle file [Start]
*!*************************************************************
*! Name      : lfRstCarLb
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/09/2000
*! Purpose   : Restore the Care Label for all styles.
*!*************************************************************
*!
FUNCTION lfRstCarLb
PARAMETER lcCarLb

PRIVATE lcOrder
SELECT (lcTmpStyle)
lcOrder = SET('ORDER')
SET ORDER TO (lcTmpStyl3)
IF SEEK('S')
  SCAN FOR cStatus = 'S'
    *WAIT WINDOW "UnSelecting style: "+ALLTRIM(cstymajor) NOWAIT
    IF lSelRec
      =SEEK(SUBSTR(CSTYMAJOR,1,lnMajorLen),'STYLE')
      REPLACE cCarLbl WITH STYLE.cCarLbl
    ELSE
      REPLACE cCarLbl WITH lcCarLb
    ENDIF
    REPLACE cSelRec WITH IIF(cSelRec = ' ','»',' '),;
            lSelRec WITH IIF(lSelRec,.F.,.T.),;
            cStatus WITH 'A'
  ENDSCAN
ENDIF
WAIT CLEAR
SELECT (lcTmpStyle)
SET ORDER TO &lcOrder.
LOCATE
*-- end of lfRstCarLb.
*B804244,1 AMH [End]

*!*************************************************************
*! Name      : lfEndObj
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 12/20/97
*! Purpose   : Put cursor at end of string.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfEndObj()
*!*************************************************************
*! Note      : I separate it for some case we want changes 
*!           : to affect refresh browse only.
*!*************************************************************
*
*B602499,1
FUNCTION lfEndObj
KEYBOARD "{END}"
*-- end of lfEndObj.

*!*************************************************************
*! Name      : lfSetSty
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/10/2001
*! Purpose   : To set the Style file in the browse screen
*!*************************************************************
*! Example     : = lfSetSty()
*!*************************************************************
*B605006,1 AMH [Start]
FUNCTION lfSetSty
PARAMETERS OpGrdParm

IF OpGrdParm = 'S'
  GO TOP IN STYLE
  lcSelStyle = laOgFxFlt[1,6]
ENDIF
*B605006,1 AMH [End]

*!*************************************************************
*! Name      : lfvSeason
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/10/2001
*! Purpose   : validate the season option in OG
*!*************************************************************
*! Example     : = lfvSeason()
*!*************************************************************
*B605006,1 AMH [Start]
FUNCTION lfvSeason

= gfMover(@laRpSorSea,@laRpTarSea,'Select Season',.T.,'')  && call mover function.
*-- end of lfvSeason.
*B605006,1 AMH [End]

*!*************************************************************
*! Name      : lfvScope
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/10/2001
*! Purpose   : validate the scope botton
*!*************************************************************
*! Example     : = lfvScope()
*!*************************************************************
*B605006,1 AMH [Start]
FUNCTION lfvScope

lcSelStyle = SPACE(0)
DECLARE laRpSorSea[1] , laRpTarSea[1]
STORE SPACE(39) TO laRpSorSea , laRpTarSea
SELECT CCODE_NO + ' - ' + CDISCREP AS SEASON;
  FROM CODES;
  WHERE CFLD_NAME = 'SEASON' AND CRLTFIELD = 'N';
  INTO ARRAY laRpSorSea
PUSH KEY CLEAR  
llExpr = gfOpGrid('MFCARLB' , .T.)  && Run selection grid.

*B605113,1 AMH Restore the correct order tag of the style file after calling OG [Start]
SET ORDER TO TAG STYLE IN STYLE
*B605113,1 AMH [End]

POP KEY
IF EMPTY(ON('KEY','ALT+B')) .AND. WEXIST(lcStyBrow)
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcStyBrow)
ENDIF
glQuitting = .T.  && Rise quit flag because it's modal screen.
IF TYPE('llExpr') <> 'L'
  SELECT (lcTmpStyle)
  ZAP
  =lfGetCarLb()
ENDIF
lnCarLb = 1
=lfvCarLb()
SHOW GET lnCarLb
*-- end of lfvScope.
*B605006,1 AMH [End]