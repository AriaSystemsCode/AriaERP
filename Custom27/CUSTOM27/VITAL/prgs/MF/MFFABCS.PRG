*:***************************************************************************
*: Program file  : MFFABCS
*: Program desc. : Fix the act. cost of fabrics for Vital
*: For screen    : MFFABCS.SCX,MFFABCS1.SCX,MFFABCS2.SCX,MFFABCS3.SCX
*:        System : Aria Advantage Series.
*:        Module : Manufactering Module (MF)
*:        Date   : 10/14/2002
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO MFFABCS
*:***************************************************************************
*: Due to B606536
*:***************************************************************************
*

lcCanBmp  = gcBmpHome + "CLOSE1.bmp"
IF !gfSetup()
  RETURN
ENDIF  

lcTmpCut  = gfTempName()     && File to hold all C/T and to brows from it.
lcTmpCut1 = gfTempName()     && Index of file lcTmpCut by selection.
lcTmpCut2 = gfTempName()     && Index of file lcTmpCut by C/T.

lcCutBrow = 'C/T'
llBrowse = .F.

*-- lnBrRecNo  : Browse Record Number
STORE 0 TO lnBrRecNo 

*-- Create Temp. File [Begin]
CREATE TABLE (gcWorkDir+lcTmpCut) (cCuttkt C(6),cStyle C(19),nActCost1 N(13,3),nActcost2 N(13,3),;
              nActcost3 N(13,3),cSelRec C(1),lSelRec L)
INDEX ON cSelRec TAG (lcTmpCut1) OF (gcWorkDir+lcTmpCut1)
INDEX ON cCuttkt TAG (lcTmpCut2) OF (gcWorkDir+lcTmpCut1)
*-- Create Temp. File [End  ]

=lfSetKey()
DO (gcScrDir+gcWinAppl+'\MFFABCS.SPX')

*-- Screen Cleanup Code.
glQuitting = .T.  && Rise quit flag because it's modal screen.

POP KEY
RELEASE WINDOW (lcCutBrow)

IF USED(lcTmpCut)
  USE IN (lcTmpCut)
  ERASE &gcWorkdir.&lcTmpCut..DBF          && Erase the Temp file.
ENDIF

IF FILE(gcWorkDir+lcTmpCut1+'.CDX')
  ERASE &gcWorkdir.&lcTmpCut1..CDX          && Erase the Temp file.
ENDIF

*-- end of program code.

**********************************************************************
**************** Control Browse and trapping Functions ***************
**********************************************************************

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
*! Purpose   : Function to create the Browse
*!*************************************************************
*
FUNCTION lfDispBrow

SELECT (lcTmpCut)
LOCATE
lnBrRecNo  = RECNO()

lcBrowFild = "lcMarker=IIF(RECNO()=lnBrRecNo , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "cSelRec :R :H= '»' ,"+;
             "cCuttkt :H = 'C/T'       :9:R,"+;
             "cStyle  :H = 'Style' :28:R,"+;
             "nActCost1 :H = 'Act. Cost1' :19:R,"+;
             "nActCost2 :H = 'Act. Cost2' :19:R,"+;
             "nActCost3 :H = 'Act. Cost3' :19:R"

BROWSE FIELDS &lcBrowFild     ;
       WINDOW MFFABCS2        ;
       WHEN lfwBrows()        ;
       IN WINDOW MFFABCS      ;
       LOCK 0                 ;
       NOAPPEND               ;
       NOCLEAR                ;
       NODELETE               ;
       NOWAIT                 ;
       NOEDIT                 ;
       NOMENU                 ;
       SAVE                   ;
       TITLE lcCutBrow

*-- end of lfDispBrow.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
*! Purpose   : When Browse Function.
*!*************************************************************
*
FUNCTION lfwBrows

IF RECCOUNT(lcTmpCut) = 0
  RETURN .F.
ENDIF
lnBrRecNo  = RECNO(lcTmpCut)
=lfvpbSel()
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
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
*! Date      : 10/14/2002
*! Purpose   : Function to trap the keys for the Browse , 
*!             and save the changes if the current record was edited
*!*************************************************************
*
FUNCTION lfBrowTrap
*-- IF The window on top is the Browse
IF WONTOP(lcCutBrow)
  glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH "TAB"
  ON KEY LABEL BACKTAB DO lfTraps WITH "BACKTAB"
  ON KEY LABEL ALT+B 
ENDIF    && End of IF
*-- end of lfBrowTrap.

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*
FUNCTION lfBrwUnTrp

*-- IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcCutBrow) .AND. glFromBrow
  = gfStopBrow()
  glFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcCutBrow)
ENDIF    && End of IF
*-- end of lfBrwUnTrp.

*!*************************************************************
*! Name      : lfTraps
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
*! Purpose   : Trap (Tab / Backtab) Keys
*!*************************************************************
*
FUNCTION lfTraps
PARAMETERS lcTrap
*-- Tab Case
IF lcTrap = "TAB"
  ACTI WINDOW MFFABCS3 TOP
  _CUROBJ = OBJNUM(pbSelect)

ELSE  && Backtab Case.
  ACTI WINDOW MFFABCS1 TOP
  _CUROBJ = OBJNUM(lnCost)
ENDIF
*-- end of lfTraps.

**********************************************************************
********************** Control Screen Functions **********************
**********************************************************************

*!*************************************************************
*! Name      : lfvSelect
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
*! Purpose   : To validate the Select/UnSelect button.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*!
FUNCTION lfvSelect

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCut)
REPLACE lSelRec WITH !lSelRec
REPLACE cSelRec WITH IIF(lSelRec,'»',' ')
=lfvpbSel()
SELECT (lnAlias)
*-- end of lfvSelect.

*!*************************************************************
*! Name      : lfvSelAll
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
*! Purpose   : To validate the Select All button.
*!*************************************************************
*!
FUNCTION lfvSelAll

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCut)
lnBrRecNo = RECNO()
SCAN FOR !lSelRec
  REPLACE lSelRec WITH .T.
  REPLACE cSelRec WITH '»'
ENDSCAN  
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GOTO lnBrRecNo
ENDIF
=lfvpbSel()
SELECT (lnAlias)
*-- end of lfvSelAll.

*!*************************************************************
*! Name      : lfvSelNon
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
*! Purpose   : To validate the Select None button.
*!*************************************************************
*!
FUNCTION lfvSelNon

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCut)
lnBrRecNo = RECNO()
SCAN FOR lSelRec
  REPLACE lSelRec WITH .F.,;
          cSelRec WITH ' '
ENDSCAN  
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF  
=lfvpbSel()
SELECT (lnAlias)
*-- end of lfvSelNon.

*!*************************************************************
*! Name      : lfvInvert
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
*! Purpose   : To validate the Invert button.
*!*************************************************************
*!
FUNCTION lfvInvert

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpCut)
SCAN
  REPLACE lSelRec WITH !lSelRec
  REPLACE cSelRec WITH IIF(lSelRec,'»',' ')
ENDSCAN  
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF  
=lfvpbSel()
SELECT (lnAlias)
*-- end of lfvInvert.

*!*************************************************************
*! Name      : lfvpbSel
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
*! Purpose   : if Current status is Select prompt is Un-Select and vice versa
*!*************************************************************
*
FUNCTION lfvpbSel
PRIVATE lnRecNo
SELECT (lcTmpCut)
SHOW GET pbSelect,1 PROMPT IIF(lSelRec,'UnSe\<lect','Se\<lect')

lnRecNo = RECNO()
SET ORDER TO (lcTmpCut1)
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
SET ORDER TO (lcTmpCut2)
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF  
SHOW WINDOW (lcCutBrow) REFRESH
*-- end of lfvpbSel.

*!*************************************************************
*! Name      : lfSetKey
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
*! Purpose   : Set Active Keys
*!*************************************************************
*!
FUNCTION lfSetKey
PUSH KEY

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcCutBrow)
ON KEY LABEL Alt+L DO lfvSelect
ON KEY LABEL Alt+A DO lfvSelAll
ON KEY LABEL Alt+N DO lfvSelNon
ON KEY LABEL Alt+I DO lfvInvert
ON KEY LABEL Alt+O DO lfvProc
ON KEY LABEL ESC   DO lfvCan
*-- end of lfSetKey.

*!*************************************************************
*! Name      : lfClearKey
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
*! Purpose   : Clear Active Keys
*!*************************************************************
*!
FUNCTION lfClearKey
ON KEY LABEL Alt+L
ON KEY LABEL Alt+A
ON KEY LABEL Alt+N
ON KEY LABEL Alt+I
ON KEY LABEL Alt+O
ON KEY LABEL ESC
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+B
*-- end of lfClearKey.

**********************************************************************
************************ Business Rules Section **********************
**********************************************************************

*!*************************************************************
*! Name      : lfvProc
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
*! Purpose   : To validate the Proc. button.
*!*************************************************************
*! Example            :  lfvProc()
*!*************************************************************
FUNCTION lfvProc

IF lnCost <= 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Cost must be greater than zero. Cannot proceed.')
  RETURN
ENDIF

PRIVATE lnAlias
lnAlias = SELECT(0)
IF SEEK(lcFabric+lcColor,'FABRIC')
  SELECT FABRIC
  REPLACE NFAVE_COST WITH lnCost,;
          NSTKVAL    WITH lnCost * ONHAND
ENDIF
IF SEEK(lcFabric+lcColor,'FABDYE')
  SELECT FABDYE
  SCAN REST WHILE fabric+color+cwarecode+dyelot = lcFabric+lcColor
    REPLACE NFAVE_COST WITH lnCost,;
            NSTKVAL    WITH lnCost * ONHAND
  ENDSCAN
ENDIF
SELECT (lcTmpCut)
SET ORDER TO TAG (lcTmpCut1)
SET ORDER TO TAG BOMCSTKT IN BOMCOST
IF SEEK('»')
  SCAN REST WHILE cSelRec = '»'
    WAIT WINDOW 'Fix C/T : ' + CCUTTKT NOWAIT
    STORE 0 TO lnActCost1,lnActCost2,lnActCost3
    IF SEEK('1M'+CCUTTKT,'BOMCOST')
      SELECT BOMCOST
      SUM NTOTACST TO lnActCost1;
          REST WHILE cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession =;
                     '1M'+EVALUATE(lcTmpCut+'.CCUTTKT') FOR ITEM <> lcFabric .OR. ICLR <> lcColor
      SELECT (lcTmpCut)
    ENDIF
    IF SEEK('2M'+CCUTTKT,'BOMCOST')
      SELECT BOMCOST
      SUM NTOTACST TO lnActCost2;
          REST WHILE cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession =;
                     '2M'+EVALUATE(lcTmpCut+'.CCUTTKT') FOR ITEM <> lcFabric .OR. ICLR <> lcColor
      SELECT (lcTmpCut)
    ENDIF
    IF SEEK('3M'+CCUTTKT,'BOMCOST')
      SELECT BOMCOST
      SUM NTOTACST TO lnActCost3;
          REST WHILE cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession =;
                     '3M'+EVALUATE(lcTmpCut+'.CCUTTKT') FOR ITEM <> lcFabric .OR. ICLR <> lcColor
      SELECT (lcTmpCut)
    ENDIF
    IF SEEK(CCUTTKT,'CUTTKTH')
      SELECT CUTTKTH
      REPLACE NACT_COST1 WITH lnActCost1,;
              NACT_COST2 WITH lnActCost2,;
              NACT_COST3 WITH lnActCost3
      SELECT (lcTmpCut)
    ENDIF
    IF SEEK('1M'+CCUTTKT+PADR(lcFabric,19)+lcColor,'BOMCOST')
      SELECT BOMCOST
      DELETE REST WHILE cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession =;
                     '1M'+EVALUATE(lcTmpCut+'.CCUTTKT')+PADR(lcFabric,19)+lcColor
      SELECT (lcTmpCut)
    ENDIF
    IF SEEK('2M'+CCUTTKT+PADR(lcFabric,19)+lcColor,'BOMCOST')
      SELECT BOMCOST
      DELETE REST WHILE cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession =;
                     '2M'+EVALUATE(lcTmpCut+'.CCUTTKT')+PADR(lcFabric,19)+lcColor
      SELECT (lcTmpCut)
    ENDIF
    IF SEEK('3M'+CCUTTKT+PADR(lcFabric,19)+lcColor,'BOMCOST')
      SELECT BOMCOST
      DELETE REST WHILE cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession =;
                     '3M'+EVALUATE(lcTmpCut+'.CCUTTKT')+PADR(lcFabric,19)+lcColor
      SELECT (lcTmpCut)
    ENDIF
    IF SEEK(lcFabric+lcColor,'MATINVJL')
      SELECT MATINVJL
      DELETE REST WHILE cfabric+ccolor+cwarecode+cdyelot+crsession+cisession = lcFabric+lcColor;
                    FOR CTRANTYPE = '4' .AND. CTRAN = EVALUATE(lcTmpCut+'.CCUTTKT')
      SELECT (lcTmpCut)
    ENDIF
    IF SEEK('M'+CCUTTKT+'1'+PADR(lcFabric,19)+lcColor,'CTKTBOM')
      SELECT CTKTBOM
      SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot =;
                      'M'+EVALUATE(lcTmpCut+'.CCUTTKT')+'1'+PADR(lcFabric,19)+lcColor
        REPLACE ISSUE_QTY WITH 0,;
                USED_QTY  WITH 0,;
                ISS_QTY1  WITH 0,;
                ISS_QTY2  WITH 0,;
                ISS_QTY3  WITH 0,;
                ISS_QTY4  WITH 0,;
                ISS_QTY5  WITH 0,;
                ISS_QTY6  WITH 0,;
                ISS_QTY7  WITH 0,;
                ISS_QTY8  WITH 0,;
                USED_QTY1 WITH 0,;
                USED_QTY2 WITH 0,;
                USED_QTY3 WITH 0,;
                USED_QTY4 WITH 0,;
                USED_QTY5 WITH 0,;
                USED_QTY6 WITH 0,;
                USED_QTY7 WITH 0,;
                USED_QTY8 WITH 0,;
                ACT_COST  WITH 0
      ENDSCAN
      SELECT (lcTmpCut)
    ENDIF
    IF SEEK('M'+CCUTTKT+'2'+PADR(lcFabric,19)+lcColor,'CTKTBOM')
      SELECT CTKTBOM
      SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot =;
                      'M'+EVALUATE(lcTmpCut+'.CCUTTKT')+'2'+PADR(lcFabric,19)+lcColor
        REPLACE ISSUE_QTY WITH 0,;
                USED_QTY  WITH 0,;
                ISS_QTY1  WITH 0,;
                ISS_QTY2  WITH 0,;
                ISS_QTY3  WITH 0,;
                ISS_QTY4  WITH 0,;
                ISS_QTY5  WITH 0,;
                ISS_QTY6  WITH 0,;
                ISS_QTY7  WITH 0,;
                ISS_QTY8  WITH 0,;
                USED_QTY1 WITH 0,;
                USED_QTY2 WITH 0,;
                USED_QTY3 WITH 0,;
                USED_QTY4 WITH 0,;
                USED_QTY5 WITH 0,;
                USED_QTY6 WITH 0,;
                USED_QTY7 WITH 0,;
                USED_QTY8 WITH 0,;
                ACT_COST  WITH 0
      ENDSCAN
      SELECT (lcTmpCut)
    ENDIF
    IF SEEK('M'+CCUTTKT+'3'+PADR(lcFabric,19)+lcColor,'CTKTBOM')
      SELECT CTKTBOM
      SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot =;
                      'M'+EVALUATE(lcTmpCut+'.CCUTTKT')+'3'+PADR(lcFabric,19)+lcColor
        REPLACE ISSUE_QTY WITH 0,;
                USED_QTY  WITH 0,;
                ISS_QTY1  WITH 0,;
                ISS_QTY2  WITH 0,;
                ISS_QTY3  WITH 0,;
                ISS_QTY4  WITH 0,;
                ISS_QTY5  WITH 0,;
                ISS_QTY6  WITH 0,;
                ISS_QTY7  WITH 0,;
                ISS_QTY8  WITH 0,;
                USED_QTY1 WITH 0,;
                USED_QTY2 WITH 0,;
                USED_QTY3 WITH 0,;
                USED_QTY4 WITH 0,;
                USED_QTY5 WITH 0,;
                USED_QTY6 WITH 0,;
                USED_QTY7 WITH 0,;
                USED_QTY8 WITH 0,;
                ACT_COST  WITH 0
      ENDSCAN
      SELECT (lcTmpCut)
    ENDIF
  ENDSCAN
ENDIF
WAIT CLEAR
SET ORDER TO TAG BOMCOST IN BOMCOST
=gfModalGen('INM00000B00000',.F.,.F.,.F.,'Fix material actual cost has been completed successfully.')
SELECT (lcTmpCut)
SET ORDER TO TAG (lcTmpCut2)
ZAP
SELECT (lnAlias)
=lfDispBrow()
lcFabric = ''
lcColor  = ''
lnCost   = 0
SHOW GET pbProc DISABLE
SHOW GET lcFabric ENABLE
SHOW GET ibItem   ENABLE
SHOW GET lcColor  ENABLE
SHOW GET ibColor  ENABLE
SHOW GET lnCost
lcCanBmp  = gcBmpHome + "CLOSE1.bmp"
SHOW GET pbCan,1 PROMPT lcCanBmp
*-- end of lfvProc.

*!*************************************************************
*! Name      : lfvCan
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/14/2002
*! Purpose   : To validate the Cancel button.
*!*************************************************************
*!
FUNCTION lfvCan

IF lcCanBmp = gcBmpHome + "CAN.bmp"
  PRIVATE lnAlias
  lnAlias = SELECT(0)
  SELECT (lcTmpCut)
  ZAP
  SELECT (lnAlias)
  =lfDispBrow()
  lcFabric = ''
  lcColor  = ''
  lnCost   = 0
  SHOW GET pbProc DISABLE
  SHOW GET lcFabric ENABLE
  SHOW GET ibItem   ENABLE
  SHOW GET lcColor  ENABLE
  SHOW GET ibColor  ENABLE
  SHOW GET lnCost
  lcCanBmp  = gcBmpHome + "CLOSE1.bmp"
  SHOW GET pbCan,1 PROMPT lcCanBmp
ELSE
  CLEAR READ
ENDIF
*-- end of lfvCan.

*!*************************************************************
*! Name        : lfvFabric
*! Developer   : AMH (AHMED MAHER)
*! Date        : 10/14/2002
*! Purpose     : validate the fabric field
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT Fabric
IF llBrowse .OR. (!EMPTY(lcFabric) .AND. !SEEK(lcFabric))
  llBrowse = .F.
  DO FaBrow WITH lcFabric,'*'
ENDIF
SELECT(lnAlias)
*-- end of lfvFabric.

*!*************************************************************
*! Name        : lfvColor
*! Developer   : AMH (AHMED MAHER)
*! Date        : 10/14/2002
*! Purpose     : validate the color field
*!*************************************************************
FUNCTION lfvColor
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT Fabric
IF llBrowse .OR. (!EMPTY(lcColor) .AND. !SEEK(lcFabric+lcColor))
  llBrowse = .F.
  lcColor  = IIF(EMPTY(lcFabric),PADR(lcColor,6),CHR(240))
  lcOldF   = lcFabric
  SELECT FABRIC
  DO FaBrow WITH lcFabric,lcColor
  IF EMPTY(lcColor)
    lcFabric = lcOldF
    _CUROBJ = OBJNUM(lcColor)
    SELECT(lnAlias)
    RETURN
  ENDIF
ENDIF

IF !EMPTY(lcFabric) AND !EMPTY(lcColor)
  =lfGetCut()
ENDIF
*-- end of lfvColor.

*!*************************************************************
*! Name        : lfGetCut
*! Developer   : AMH (AHMED MAHER)
*! Date        : 10/14/2002
*! Purpose     : Get the C/Ts of fabric/color
*!*************************************************************
FUNCTION lfGetCut

PRIVATE lnAlias
IF SEEK(PADR(lcFabric,19)+lcColor,'BOMCOST')
  SELECT BOMCOST
  SCAN REST WHILE item+iclr+cwarecode+cdyelot+crsession+cisession = PADR(lcFabric,19)+lcColor;
              FOR CIMTYP = 'M'
    SCATTER MEMVAR
    =SEEK(m.cTktNo,'CUTTKTH')
    IF CUTTKTH.STATUS $ "XS"
      LOOP
    ENDIF
    SELECT (lcTmpCut)
    IF !SEEK(m.cTktNo)
      APPEND BLANK
      REPLACE CCUTTKT WITH m.cTktNo,;
              CSTYLE  WITH CUTTKTH.STYLE
    ENDIF
    REPLACE ('NACTCOST'+m.cBomType) WITH EVALUATE('NACTCOST'+m.cBomType) + m.nTotACst
  ENDSCAN
ENDIF
=lfwBrows()
SHOW GET pbProc ENABLE
SHOW GET lcFabric DISABLE
SHOW GET ibItem   DISABLE
SHOW GET lcColor  DISABLE
SHOW GET ibColor  DISABLE
lcCanBmp  = gcBmpHome + "CAN.bmp"
SHOW GET pbCan,1 PROMPT lcCanBmp