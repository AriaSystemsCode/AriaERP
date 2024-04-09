*:***************************************************************************
*: Program file  : MFLOCSt
*: Program desc. : A screen to assign cut & sew vendors in ARIA 27
*: For screen    : MFLOCATE.SCX
*:        System : Aria Advantage Series.
*:        Module : Manufactering Module (MF)
*:        Date   : 01/11/2000
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO MFLOCATE
*:***************************************************************************
*: Due to C102101
*:***************************************************************************
*: Modifications :
*: B604562,1 AMH 06/17/2001 Optimizing the speed and removing the fabric and
*: B604562,1                color from the screen and browse and from the validations.
*: B604859,1 AMH 08/29/2001 Assigning Style to different operation different location
*: B604951,1 AMH 09/24/2001 Wrong validation in the percentage field.
*: B604985,1 AMH 10/01/2001 Fix Dupllicated records in locsty file.
*: B605028,1 AMH 10/15/2001 Add an option grid to select the styles.
*: B605133,1 AMH 11/14/2001 Fix the bug of not collecting all colors.
*: B605393,1 AMH 01/17/2002 Make some modification in this screen.
*:***************************************************************************
EXTERNAL ARRAY laDefProc,laCtrStat,laScrMode

laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .F.              && Disable the control panel close proc. (lpClsScr)

*B604562,1 AMH variable to save old location [Start]
STORE SPACE(0) TO lcOldLocat
*B604562,1 AMH [End]

*B605393,1 AMH Add variable to save old locations [Start]
STORE 0 TO lnOldLocVn
*B605393,1 AMH [End]

*B604562,1 AMH Remove all variable of fabric & color fields [Start]
*STORE ' ' TO laVendor, lcStyle, lcFabric, lcColor, lcOldFab, lcExpToSeek
STORE ' ' TO laVendor, lcStyle, lcExpToSeek
*B604562,1 AMH [End]
STORE 1   TO lnVendor
STORE 0   TO lnUnit, lnPer, lnOldUnit, lnOldPer, lnStartTrap, lnEndTrap

*B605028,1 AMH Add an icon for the scope in the control pannel [Start]
DECLARE laPanelObj[1,3]
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbScope'
laPanelObj[1,2] = gcBmpHome+'SCOPE.BMP'
laPanelObj[1,3] = [VALID lfvScope()]
llFirst = .T.
lcMajorPic = gfItemMask('PM')
lnMajorLen = LEN(lcMajorPic)
*B605028,1 AMH [End]

IF !gfSetup()
  RETURN
ENDIF  

lcBaseFile = ''
laCtrStat[7] = "ENABLE"	    &&Edit
laCtrStat[10] = "DISABLE"	&&Browse

lcTmpStyle = gfTempName()     && File to hold all Styles and to brows from it.
lcTmpStyl1 = gfTempName()     && Index of file lcTmpStyle by selection.
lcTmpStyl2 = gfTempName()     && Index of file lcTmpStyle by style.
lcTmpStyl3 = gfTempName()     && Index of file lcTmpStyle by status.
*B604859,1 AMH Add two indeces to optimze change operation [Start]
lcTmpStyl4 = gfTempName()     && Index of file lcTmpStyle by Location.
lcTmpStyl5 = gfTempName()     && Index of file lcTmpStyle by LocOther.
lcOpr      = 'Cutting'
*B604859,1 AMH [End]
lcTmpStySv = gfTempName()     && File to save all Styles.
lcTmpStyS1 = gfTempName()     && Index of file lcTmpStySv by style.
*B604562,1 AMH Remove all variable of fabric & color fields [Start]
*lcTmpStyS2 = gfTempName()     && Index of file lcTmpStySv by style/fabric.
*B604562,1 AMH [End]

*B605393,1 AMH Add tmp. table to hold all locations of the current style & tmp. table 
*B605393,1     to hold save adustments of locations [Start]
lcTmpStyLc = gfTempName()
lcTmpStyLS = gfTempName()
*B605393,1 AMH [End]

lcStyBrow = 'Styles'
llBrowse = .F.

*B605393,1 AMH Add variable to brow screen for locations [Start]
lcLocBrow = 'Locations'
*B605393,1 AMH [End]

*-- Define Screens Variables and Temporary file [End  ]

*-- Create Vendor Array [Begin]
SELECT APVENDOR
lnVendCnt = 0
SCAN
  *B604562,1 AMH comment all wait window lines [Start]
  *WAIT WINDOW "Selecting Location : "+ALLTRIM(cvendcode) NOWAIT
  *B604562,1 AMH [End]
  lnVendCnt = lnVendCnt+1
  DIMENSION laVendor [lnVendCnt,2]
  laVendor[lnVendCnt,1] = CVENDCODE + ' - ' + CVENCOMP
  laVendor[lnVendCnt,2] = CVENDCODE
ENDSCAN
*B604562,1 AMH comment all wait window lines [Start]
*WAIT CLEAR
*B604562,1 AMH [End]

*B605028,1 AMH Add select Location row [Start]
DIMENSION laVendor[ALEN(laVendor,1)+1,2]
=AINS(laVendor,1)
laVendor[1,1] = 'Select Location'
laVendor[1,2] = SPACE(8)
*B605028,1 AMH [End]

*-- Create Care Label Array [End  ]

*-- lnBrRecNo  : Browse Record Number
STORE 0 TO lnBrRecNo 

*-- Create Temp. File [Begin]
*B604562,1 AMH Remove all variable of fabric & color fields [Start]
*CREATE TABLE (gcWorkDir+lcTmpStyle) (Style C(19),TotQty N(7,0),nPerc_Qty N(6,3),Fabric C(7),;
              Color C(6),Location C(8),LocOther C(8),cCut_Sew C(1),TotPlan N(7,0),cSelRec C(1),lSelRec L,cStatus C(1))
*B604859,1 AMH Add new fields to save the available units [Start]
*CREATE TABLE (gcWorkDir+lcTmpStyle) (Style C(19),TotQty N(7,0),nPerc_Qty N(6,3),;
              Location C(8),LocOther C(8),cCut_Sew C(1),TotPlan N(7,0),cSelRec C(1),lSelRec L,cStatus C(1))
CREATE TABLE (gcWorkDir+lcTmpStyle) (Style C(19),TotQty N(7,0),nCutAvl N(7,0),nSewAvl N(7,0),nPerc_Qty N(6,3),;
              Location C(8),LocOther C(8),cCut_Sew C(1),TotPlan N(7,0),cSelRec C(1),lSelRec L,cStatus C(1),TotQtyOth N(7,0),nPerc_Oth N(6,3),cChnLoc C(1))
*B604859,1 AMH [End]
*B604562,1 AMH [End]
INDEX ON cSelRec TAG (lcTmpStyl1) OF (gcWorkDir+lcTmpStyl1)
INDEX ON cStatus TAG (lcTmpStyl3) OF (gcWorkDir+lcTmpStyl1)
*B604859,1 AMH Add two indeces to optimze change of operation [Start]
INDEX ON Location+cChnLoc+LocOther TAG (lcTmpStyl4) OF (gcWorkDir+lcTmpStyl1)
INDEX ON LocOther+cChnLoc+Location TAG (lcTmpStyl5) OF (gcWorkDir+lcTmpStyl1)
*B604859,1 AMH [End]
INDEX ON Style+cCut_Sew+Location TAG (lcTmpStyl2) OF (gcWorkDir+lcTmpStyl1)
*B604562,1 AMH Remove all variable of fabric & color fields [Start]
*CREATE TABLE (gcWorkDir+lcTmpStySv) (Style C(19),TotQty N(7,0),nPerc_Qty N(6,3),Fabric C(7),;
              Color C(6),Location C(8),cCut_Sew C(1),cStatus C(1))
CREATE TABLE (gcWorkDir+lcTmpStySv) (Style C(19),TotQty N(7,0),nPerc_Qty N(6,3),;
              Location C(8),cCut_Sew C(1),cStatus C(1))
*B604562,1 AMH [End]
INDEX ON Style+cCut_Sew+Location TAG (lcTmpStyS1) OF (gcWorkDir+lcTmpStyS1)
*B604562,1 AMH Remove all variable of fabric & color fields [Start]
*INDEX ON Style+cCut_Sew+Fabric+Color TAG (lcTmpStyS2) OF (gcWorkDir+lcTmpStyS1)
*B604562,1 AMH [End]

*B605393,1 AMH Creat lcTmpStyLc file & lcTmpStyLS file [Start]
CREATE TABLE (gcWorkDir+lcTmpStyLc) (Style C(19),TotQty N(7,0),nPerc_Qty N(6,3),;
              Location C(8),cCut_Sew C(1),TotPlan N(7,0),cStatus C(1))
INDEX ON Style+cCut_Sew+Location TAG (lcTmpStyLc) OF (gcWorkDir+lcTmpStyLc)
CREATE TABLE (gcWorkDir+lcTmpStyLS) (Style C(19),TotQty N(7,0),nPerc_Qty N(6,3),;
              Location C(8),cCut_Sew C(1),cStatus C(1))
INDEX ON Style+cCut_Sew+Location TAG (lcTmpStyLS) OF (gcWorkDir+lcTmpStyLS)
*B605393,1 AMH [End]

*B605028,1 AMH Comment the next line [Start]
*=lfGetLocat(ALLTRIM(laVendor(lnVendor,2)),'C')
*B605028,1 AMH [End]

*-- Create Temp. File [End  ]

=lfSetKey()

*B605028,1 AMH Comment the next line [Start]
*DO lpShow
*B605028,1 AMH [End]

DO (gcScrDir+gcWinAppl+'\MFLOCATE.SPX')

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

*B605393,1 AMH Erase lcTmpStyLc file & lcTmpStyLS [Start]
IF USED(lcTmpStyLc)
  USE IN (lcTmpStyLc)
  ERASE &gcWorkdir.&lcTmpStyLc..DBF          && Erase the Temp file.
  ERASE &gcWorkdir.&lcTmpStyLc..CDX          && Erase the Temp file.
ENDIF
IF USED(lcTmpStyLS)
  USE IN (lcTmpStyLS)
  ERASE &gcWorkdir.&lcTmpStyLS..DBF          && Erase the Temp file.
  ERASE &gcWorkdir.&lcTmpStyLS..CDX          && Erase the Temp file.
ENDIF
*B605393,1 AMH [End]

*-- end of program code.

**********************************************************************
**************** Control Browse and trapping Functions ***************
**********************************************************************

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : Function to create the Browse
*!*************************************************************
*
FUNCTION lfDispBrow

SELECT (lcTmpStyle)
*LOCATE
lnBrRecNo  = RECNO()

*lcBrowFild = "lcMarker=IIF(RECNO()=lnBrRecNo , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "cSelRec :R :H= '»' ,"+;
             "Style :H = 'Style'       :30:R,"+;
             "TotQty  :H = 'Units' :10:R,"+;
             "nPerc_Qty  :H = 'Percentage' :10:R,"+;
             "Fabric  :H = 'Fabric' :10:R,"+;
             "Color  :H = 'Color' :10:R,"+;
             IIF(cCut_Sew='C',"Location","LocOther")+"  :H = 'Cut Location' :15:R,"+;
             IIF(cCut_Sew='S',"Location","LocOther")+"  :H = 'Sew Location' :15:R"
*B604562,1 AMH Remove fabric and color fields from browse [Start]
*lcBrowFild = "lcMarker=IIF(RECNO()=lnBrRecNo , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "cSelRec :R :H= '»' ,"+;
             "Style :H = 'Style'       :30:R,"+;
             "TotQty  :H = 'Units' :20:R,"+;
             "nPerc_Qty  :H = 'Percentage' :20:R,"+;
             "Fabric  :H = 'Fabric' :10:R,"+;
             "Color  :H = 'Color' :10:R"
*B604859,1 AMH Browse units of current operation [Start]
*lcBrowFild = "lcMarker=IIF(RECNO()=lnBrRecNo , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "cSelRec :R :H= '»' ,"+;
             "Style :H = 'Style'       :30:R,"+;
             "TotQty  :H = 'Units' :20:R,"+;
             "nPerc_Qty  :H = 'Percentage' :20:R"
lcBrowFild = "lcMarker=IIF(RECNO()=lnBrRecNo , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "cSelRec :R :H= '»' ,"+;
             "Style :H = 'Style'       :30:R,"+;
             IIF(SUBSTR(lcOpr,1,1)='C',"TotQty","TotQtyOth")+"  :H = 'Units' :20:R,"+;
             IIF(SUBSTR(lcOpr,1,1)='C',"nPerc_Qty","nPerc_Oth")+"  :H = 'Percentage' :20:R"
*B604859,1 AMH [End]
*BROWSE FIELDS &lcBrowFild     ;
       WINDOW MFLOC2          ;
       WHEN lfwBrows()        ;
       VALID :F lfvBrows(.T.,'B');
       IN WINDOW MFLOCATE     ;
       LOCK 0                 ;
       NOAPPEND               ;
       NOCLEAR                ;
       NODELETE               ;
       NOWAIT                 ;
       NOEDIT                 ;
       NOMENU                 ;
       SAVE                   ;
       TITLE lcStyBrow
BROWSE FIELDS &lcBrowFild     ;
       WINDOW MFLOC2          ;
       WHEN lfwBrows()        ;
       IN WINDOW MFLOCATE     ;
       LOCK 0                 ;
       NOAPPEND               ;
       NOCLEAR                ;
       NODELETE               ;
       NOWAIT                 ;
       NOEDIT                 ;
       NOMENU                 ;
       SAVE                   ;
       TITLE lcStyBrow
*B604562,1 AMH [End]

*-- end of lfDispBrow.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : When Browse Function.
*!*************************************************************
*
FUNCTION lfwBrows
lnBrRecNo  = RECNO(lcTmpStyle)
=lfvpbSel()
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfvBrows
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : Valid Browse Function.
*!*************************************************************
*
FUNCTION lfvBrows
PARAMETER llRet, lcCall
*B604562,1 AMH Remove all variable of fabric & color fields [Start]
*PRIVATE lnAlias, lcOrder1, lcOrder2, lcCut_Sew, lcVendor, lcMsg
PRIVATE lnAlias, lcOrder1, lcCut_Sew, lcVendor, lcMsg
*B604562,1 AMH [End]
IF !lSelRec
  RETURN
ENDIF
lnAlias = SELECT(0)
SELECT LOCSTY
lcOrder1 = SET('ORDER')
SET ORDER TO TAG LOCSTYFB
lcCut_Sew = IIF(lcOpr='Cutting','C','S')
lcVendor = ALLTRIM(laVendor(lnVendor,2))
*B604562,1 AMH Remove all variable of fabric & color fields [Start]
*IF SEEK(&lcTmpStyle..STYLE+lcCut_Sew+lcFabric+PADR(lcColor,6)) .AND. !(lcVendor == ALLTRIM(LOCATION))
*B604859,1 AMH Comment the next lines to let the user be able to assigning style [Start]
*B604859,1 AMH to different operation different location
*IF SEEK(&lcTmpStyle..STYLE+lcCut_Sew) .AND. !(lcVendor == ALLTRIM(LOCATION))
*  IF lcCall='B'
    *lcMsg = "Style " + ALLTRIM(&lcTmpStyle..STYLE) + " is already assigned to the " + lcOpr + ;
            " operation at the " + ALLTRIM(LOCATION) + " location with the same fabric and color. Can't select."
*    lcMsg = "Style " + ALLTRIM(&lcTmpStyle..STYLE) + " is already assigned to the " + lcOpr + ;
            " operation at the " + ALLTRIM(LOCATION) + " location."
*  ELSE
    *lcMsg = "One or more styles are already assigned to the " + lcOpr + ;
            " operation at another location with the same fabric and color. Can't select."
*    lcMsg = "One or more styles are already assigned to the " + lcOpr + ;
            " operation at another location."
*  ENDIF

*  IF llRet
*    = gfModalGen("TRM00000B00000","","",.F.,lcMsg)
*  ENDIF

*  SELECT (lcTmpStyle)
  *REPLACE lSelRec   WITH .F.,;
          cSelRec   WITH ' ',;
          Location  WITH '' ,;
          Fabric    WITH '' ,;
          Color     WITH ''
*  REPLACE lSelRec   WITH .F.,;
          cSelRec   WITH ' ',;
          Location  WITH ''
*B604562,1 AMH [End]
*  =lfChangSt()
*  =lfvpbSel()
*  llRet = .F.
*  SHOW GET lnVendor  DISABLE
*  SHOW GET lcOpr     DISABLE
*ELSE
  SELECT (lcTmpStySv)
  *B604562,1 AMH Remove all variable of fabric & color fields [Start]
  *lcOrder2 = SET('ORDER')
  *SET ORDER TO TAG (lcTmpStyS2)
  *IF SEEK(&lcTmpStyle..STYLE+lcCut_Sew+lcFabric+PADR(lcColor,6)) .AND. !(lcVendor == ALLTRIM(&lcTmpStySv..LOCATION))
  *IF SEEK(&lcTmpStyle..STYLE+lcCut_Sew) .AND. !(lcVendor == ALLTRIM(&lcTmpStySv..LOCATION))
  *  IF lcCall='B'
      *lcMsg = "Style " + ALLTRIM(&lcTmpStyle..STYLE) + " is already assigned to the " + lcOpr + ;
              " operation at the " + ALLTRIM(LOCATION) + " location with the same fabric and color. Can't select."
  *    lcMsg = "Style " + ALLTRIM(&lcTmpStyle..STYLE) + " is already assigned to the " + lcOpr + ;
              " operation at the " + ALLTRIM(LOCATION) + " location."
  *  ELSE
      *lcMsg = "One or more styles are already assigned to the " + lcOpr + ;
              " operation at another location with the same fabric and color. Can't select."
  *    lcMsg = "One or more styles are already assigned to the " + lcOpr + ;
              " operation at another location."
  *  ENDIF

  *  IF llRet
  *    = gfModalGen("TRM00000B00000","","",.F.,lcMsg)
  *  ENDIF
  *  SELECT (lcTmpStyle)
    *REPLACE lSelRec   WITH .F.,;
            cSelRec   WITH ' ',;
            Location  WITH '' ,;
            Fabric    WITH '' ,;
            Color     WITH ''
  *  REPLACE lSelRec   WITH .F.,;
            cSelRec   WITH ' ',;
            Location  WITH ''
  *B604562,1 AMH [End]
  *  =lfChangSt()
  *  =lfvpbSel()
  *  llRet = .F.
  *  SHOW GET lnVendor  DISABLE
  *  SHOW GET lcOpr     DISABLE
  *ENDIF
  SELECT (lcTmpStySv)
  *B604562,1 AMH Remove all variable of fabric & color fields [Start]
  *SET ORDER TO &lcOrder2.
  *B604562,1 AMH [End]
*ENDIF
*B604859,1 AMH [End]
SELECT LOCSTY
SET ORDER TO &lcOrder1.
SELECT (lnAlias)
*-- end of lfvBrows.

*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
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
*! Date      : 01/11/2000
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

  IF SYS(21)<>"0"
    lcOrdExpr   = SYS(14,EVAL(SYS(21)))
    lcOrdExpT   = TYPE(lcOrdExpr)
    lnStartTrap = IIF(lcOrdExpT='N',48,32)
    lnEndTrap   = IIF(lcOrdExpT='N',57,126)
    FOR lnChrToTrap = lnStartTrap TO lnEndTrap
      ON KEY LABEL (CHR(lnChrToTrap)) DO lfIncSearch
    ENDFOR
  ENDIF  
ENDIF    && End of IF
*-- end of lfBrowTrap.

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

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
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
  ON KEY LABEL ALT+B DO lfDispBrow
  FOR lnChrToTrap = lnStartTrap TO lnEndTrap
    ON KEY LABEL (CHR(lnChrToTrap))
  ENDFOR
ENDIF    && End of IF
*-- end of lfBrwUnTrp.

*!*************************************************************
*! Name      : lfTraps
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : Trap (Tab / Backtab) Keys
*!*************************************************************
*
FUNCTION lfTraps
PARAMETERS lcTrap
*-- Tab Case
IF lcTrap = "TAB"
  ACTI WINDOW MFLOC3 TOP
  _CUROBJ = OBJNUM(pbSelect)

ELSE  && Backtab Case.
  ACTI WINDOW MFLOC1 TOP
  _CUROBJ = OBJNUM(lnVendor)
ENDIF
*-- end of lfTraps.

**********************************************************************
********************** Control Screen Functions **********************
**********************************************************************

*!*************************************************************
*! Name      : lfvSelect
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : To validate the Select/UnSelect button.
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*!
FUNCTION lfvSelect

*B604985,1 AMH Check if this style used by another user [Start]
IF !lfChckLock(.T.)
  RETURN
ENDIF
*B604985,1 AMH [End]

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpStyle)
*B604985,1 AMH Refresh the avalible quantity [Start]
PRIVATE lnAvl
lnAvl = lfGetSvAvl(lcTmpStySv)
lnAvl = IIF(lnAvl=0 .AND. EVALUATE(lcTmpStySv+'.cStatus') # 'D',lfGetSvAvl('LOCSTY'),lnAvl)
REPLACE nCutAvl WITH MAX(TotPlan - lfGetAvl(Style,'C',TotPlan),0)
REPLACE nSewAvl WITH MAX(TotPlan - lfGetAvl(Style,'S',TotPlan),0)
IF !lSelRec
  REPLACE TotQty    WITH IIF(lnAvl=0,EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl'),lnAvl),;
          nPerc_Qty WITH IIF(TotPlan=0,0,ROUND(TotQty/TotPlan*100,3))
  lnUnit = TotQty
  lnPer  = nPerc_Qty
  SHOW GET lnUnit
  SHOW GET lnPer

*B605393,1 AMH Update totqty & nperc_qty with zero when unselect [Start]
ELSE
  REPLACE TotQty    WITH 0,;
          nPerc_Qty WITH 0
  lnUnit = TotQty
  lnPer  = nPerc_Qty
  SHOW GET lnUnit
  SHOW GET lnPer
*B605393,1 AMH [End]

ENDIF
*B604985,1 AMH [End]
IF cStatus $ 'AM' .AND. lSelRec .AND.;
   gfModalGen("TRM00000B38006","","",.F.,;
              'Are you sure you want to remove style: '+style+' from this location ?') = 2
  RETURN
ENDIF

*B604562,1 AMH update operation field [Start]
*REPLACE lSelRec   WITH !lSelRec,;
        cSelRec   WITH IIF(lSelRec,'»',' '),;
        Location  WITH IIF(lSelRec,ALLTRIM(laVendor(lnVendor,2)),'')
REPLACE lSelRec   WITH !lSelRec,;
        cSelRec   WITH IIF(lSelRec,'»',' '),;
        Location  WITH ALLTRIM(laVendor(lnVendor,2)),;
        cCut_Sew  WITH SUBSTR(lcOpr,1,1)
*B604562,1 AMH [End]

*B604985,1 AMH Unlock this style when unselect it [Start]
IF !lSelRec
  =lfUnLock(EVALUATE(lcTmpStyle+'.STYLE'),.T.)
ENDIF
*B604985,1 AMH [End]

=lfChangSt()
=lfvpbSel()
*B604562,1 AMH validate the selected line [Start]
=lfvBrows(.T.,'B')
*B604562,1 AMH [End]
SELECT (lnAlias)
llCUpDate = .T.
SHOW GET lnVendor  DISABLE
SHOW GET lcOpr     DISABLE
*-- end of lfvSelect.

*!*************************************************************
*! Name      : lfvSelAll
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : To validate the Select All button.
*!*************************************************************
*!
FUNCTION lfvSelAll

PRIVATE lnAlias
*B604562,1 AMH define variable for calling lfvBrow function [Start]
PRIVATE llRet
llRet = .T.
*B604562,1 AMH [End]

lnAlias = SELECT(0)
SELECT (lcTmpStyle)
lnBrRecNo = RECNO()
*B604985,1 AMH Check if there is a locked style before select it [Start]
PRIVATE lnAvl, llLocked
llLocked = .T.
SCAN FOR !lSelRec
  IF !lfChckLock(.F.)
    IF llLocked
      =gfModalGen("TRM00000B00000","","",.F.,'One or more Styles are used by another user, Cannot proceed.')
    ENDIF
    llLocked = .F.
    LOOP
  ENDIF
  lnAvl = lfGetSvAvl(lcTmpStySv)
  lnAvl = IIF(lnAvl=0 .AND. EVALUATE(lcTmpStySv+'.cStatus') # 'D',lfGetSvAvl('LOCSTY'),lnAvl)
  REPLACE nCutAvl WITH MAX(TotPlan - lfGetAvl(Style,'C',TotPlan),0)
  REPLACE nSewAvl WITH MAX(TotPlan - lfGetAvl(Style,'S',TotPlan),0)
  REPLACE TotQty    WITH IIF(lnAvl=0,EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl'),lnAvl),;
          nPerc_Qty WITH IIF(TotPlan=0,0,ROUND(TotQty/TotPlan*100,3))
*B604985,1 AMH [End]
  *B604562,1 AMH comment all wait window lines [Start]
  *WAIT WINDOW "Selecting style: "+ALLTRIM(style) NOWAIT
  *B604562,1 AMH [End]
  *B604562,1 AMH update operation field [Start]
  *REPLACE lSelRec  WITH .T.,;
  		  cSelRec  WITH '»',;
		  Location WITH ALLTRIM(laVendor(lnVendor,2))
  REPLACE lSelRec  WITH .T.,;
  		  cSelRec  WITH '»',;
		  Location WITH ALLTRIM(laVendor(lnVendor,2)),;
		  cCut_Sew WITH SUBSTR(lcOpr,1,1)
  *B604562,1 AMH [End]
  =lfChangSt()
  *B604562,1 AMH validate the selected line [Start]
  =lfvBrows(@llRet,'P')
  *B604562,1 AMH [End]
ENDSCAN  
*B604562,1 AMH comment all wait window lines [Start]
*WAIT CLEAR
*B604562,1 AMH [End]

IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GOTO lnBrRecNo
ENDIF
=lfvpbSel()
SELECT (lnAlias)
llCUpDate = .T.
SHOW GET lnVendor  DISABLE
SHOW GET lcOpr     DISABLE
*-- end of lfvSelAll.

*!*************************************************************
*! Name      : lfvSelNon
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
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
  *B604562,1 AMH comment all wait window lines [Start]
  *WAIT WINDOW "Removing style: "+ALLTRIM(style) NOWAIT
  *B604562,1 AMH [End]
  IF cStatus $ 'AM'
    IF !llAll
      lnMsg = gfModalGen("TRM00000B38026","","",.F.,;
              'Are you sure you want to remove style: '+style+' from this location ?')
      llAll = IIF(MOD(lnMsg,2)=0,.T.,.F.)
    ENDIF
    IF lnMsg > 2
      LOOP
    ENDIF
  ENDIF
  *B604562,1 AMH Don't blank location field [Start]
  *REPLACE lSelRec   WITH .F.,;
          cSelRec   WITH ' ',;
          Location  WITH ''
  REPLACE lSelRec   WITH .F.,;
          cSelRec   WITH ' '
  *B604562,1 AMH [End]
  =lfChangSt()
  *B604985,1 AMH Unlock unselected styles [Start]
  =lfUnLock(EVALUATE(lcTmpStyle+'.STYLE'),.T.)
  *B604985,1 AMH [End]
ENDSCAN
*B604562,1 AMH comment all wait window lines [Start]
*WAIT CLEAR
*B604562,1 AMH [End]

IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF
=lfvpbSel()
SELECT (lnAlias)
llCUpDate = .T.
SHOW GET lnVendor  DISABLE
SHOW GET lcOpr     DISABLE
*-- end of lfvSelNon.

*!*************************************************************
*! Name      : lfvInvert
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : To validate the Invert button.
*!*************************************************************
*!
FUNCTION lfvInvert

PRIVATE lnMsgNon, llAllNon, lnAlias
*B604562,1 AMH define variable for calling lfvBrow function [Start]
PRIVATE llRet
llRet = .T.
*B604562,1 AMH [End]
llAllNon = .F.
lnAlias = SELECT(0)
SELECT (lcTmpStyle)
*B604985,1 AMH Check if there is a locked style before select it [Start]
PRIVATE lnAvl, llLocked
llLocked = .T.
SCAN
  IF lSelRec
    =lfUnLock(EVALUATE(lcTmpStyle+'.STYLE'),.T.)
  ELSE
    IF !lfChckLock(.F.)
      IF llLocked
        =gfModalGen("TRM00000B00000","","",.F.,'One or more Styles are used by another user, Cannot proceed.')
      ENDIF
      llLocked = .F.
      LOOP
    ENDIF
    lnAvl = lfGetSvAvl(lcTmpStySv)
    lnAvl = IIF(lnAvl=0 .AND. EVALUATE(lcTmpStySv+'.cStatus') # 'D',lfGetSvAvl('LOCSTY'),lnAvl)
    REPLACE nCutAvl WITH MAX(TotPlan - lfGetAvl(Style,'C',TotPlan),0)
    REPLACE nSewAvl WITH MAX(TotPlan - lfGetAvl(Style,'S',TotPlan),0)
    REPLACE TotQty    WITH IIF(lnAvl=0,EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl'),lnAvl),;
            nPerc_Qty WITH IIF(TotPlan=0,0,ROUND(TotQty/TotPlan*100,3))
  ENDIF
*B604985,1 AMH [End]
  *B604562,1 AMH comment all wait window lines [Start]
  *WAIT WINDOW "Selecting style: "+ALLTRIM(style) NOWAIT
  *B604562,1 AMH [End]
  IF cStatus $ 'AM' .AND. lSelRec
    IF !llAllNon
      lnMsgNon = gfModalGen("TRM00000B38026","","",.F.,;
              'Are you sure you want to remove style: '+style+' from this location ?')
      llAllNon = IIF(MOD(lnMsgNon,2)=0,.T.,.F.)
    ENDIF
    IF lnMsgNon > 2
      LOOP
    ENDIF
  ENDIF
  *B604562,1 AMH update operation field [Start]
  *REPLACE lSelRec   WITH !lSelRec,;
          cSelRec   WITH IIF(lSelRec,'»',' '),;
          Location  WITH IIF(lSelRec,ALLTRIM(laVendor(lnVendor,2)),'')
  REPLACE lSelRec   WITH !lSelRec,;
          cSelRec   WITH IIF(lSelRec,'»',' '),;
          Location  WITH ALLTRIM(laVendor(lnVendor,2)),;
          cCut_Sew  WITH SUBSTR(lcOpr,1,1)
  *B604562,1 AMH [End]
  =lfChangSt()
  *B604562,1 AMH validate the selected line [Start]
  =lfvBrows(@llRet,'P')
  *B604562,1 AMH [End]
ENDSCAN  
*B604562,1 AMH comment all wait window lines [Start]
*WAIT CLEAR
*B604562,1 AMH [End]

IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF  
=lfvpbSel()
SELECT (lnAlias)
llCUpDate = .T.
SHOW GET lnVendor  DISABLE
SHOW GET lcOpr     DISABLE
*-- end of lfvInvert.

*!*************************************************************
*! Name      : lfvpbSel
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : if Current status is Select prompt is Un-Select and vice versa
*!*************************************************************
*
FUNCTION lfvpbSel
PRIVATE lnRecNo
lcStyle  = &lcTmpStyle..style
*B604562,1 AMH Remove all variable of fabric & color fields [Start]
*lcFabric = &lcTmpStyle..fabric
*lcColor  = &lcTmpStyle..color
*B604562,1 AMH [End]
*B604859,1 AMH get the unit value from totqtyoth [Start]
*lnUnit   = &lcTmpStyle..totqty
*lnPer    = &lcTmpStyle..nperc_qty
lnUnit   = EVALUATE(lcTmpStyle+'.totqty'+IIF(SUBSTR(lcOpr,1,1)='C','','oth'))
lnPer    = EVALUATE(lcTmpStyle+'.nperc_'+IIF(SUBSTR(lcOpr,1,1)='C','qty','oth'))
*B604859,1 AMH [End]
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
  lcEn_Dis = IIF(lSelRec,'ENABLE','DISABLE')
  SHOW GET lcStyle  &lcEn_Dis
  *B604562,1 AMH Remove all variable of fabric & color fields [Start]
  *SHOW GET lcFabric &lcEn_Dis
  SHOW GET lnUnit   &lcEn_Dis
  SHOW GET lnPer    &lcEn_Dis
  *SHOW GET ibFabric &lcEn_Dis
  *lcEn_Dis = IIF(lcEn_Dis='ENABLE' .AND. !EMPTY(lcFabric),'ENABLE','DISABLE')
  *SHOW GET lcColor  &lcEn_Dis
  *SHOW GET ibColor  &lcEn_Dis
  *B605028,1 AMH disable selection bottons in case of lnVendor = 1 [Start]
  IF lnVendor = 1
    SHOW GET pbSelect DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbSelNon DISABLE
    SHOW GET pbInvert DISABLE
  ENDIF
  *B605028,1 AMH [End]
ELSE
  SHOW GET lcStyle
  *SHOW GET lcFabric
  *SHOW GET lcColor
  *B604562,1 AMH [End]
  SHOW GET lnUnit
  SHOW GET lnPer
ENDIF
SHOW WINDOW (lcStyBrow) REFRESH
*-- end of lfvpbSel.

*!*************************************************************
*! Name      : lfSetKey
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : Set Active Keys
*!*************************************************************
*!
FUNCTION lfSetKey
PUSH KEY

ON KEY LABEL ALT+B DO lfDispBrow
ON KEY LABEL Alt+L DO lfvSelect
ON KEY LABEL Alt+A DO lfvSelAll
ON KEY LABEL Alt+N DO lfvSelNon
ON KEY LABEL Alt+I DO lfvInvert
ON KEY LABEL Alt+O DO lfvOk
ON KEY LABEL Alt+C DO lfvCan
ON KEY LABEL ESC   DO gfCPClose
*-- end of lfSetKey.

*!*************************************************************
*! Name      : lfClearKey
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : Clear Active Keys
*!*************************************************************
*!
FUNCTION lfClearKey
ON KEY LABEL Alt+L
ON KEY LABEL Alt+A
ON KEY LABEL Alt+N
ON KEY LABEL Alt+I
ON KEY LABEL Alt+O
ON KEY LABEL Alt+S
ON KEY LABEL Alt+C
ON KEY LABEL ESC
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+B
*-- end of lfClearKey.

*!*************************************************************
*! Name      : lfvVendor
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : Valed function of Vendor PopUp
*!*************************************************************
*!
FUNCTION lfvVendor

*B604562,1 AMH Use lfRefLocat function for optimizing the process [Start]
*=lfDelBrow()
*=lfGetLocat(ALLTRIM(laVendor(lnVendor,2)),IIF(lcOpr='Cutting','C','S'))
=lfRefLocat()
llCUpDate = .T.
*B604562,1 AMH [End]

=lfDispBrow()
=lfvpbSel()
=lfRefresh()
IF laScrMode[3]
  SHOW GET lnVendor  DISABLE
  SHOW GET lcOpr     DISABLE
ENDIF

*B605028,1 AMH Disable/enable edit botton [Satrt]
DO CASE
  CASE lnVendor = 1 .AND. laScrMode[2]
    laScrMode = .F.
    laScrMode[1] = .T.
    DO lpShow
  CASE lnVendor # 1 .AND. laScrMode[1]
    laScrMode = .F.
    laScrMode[2] = .T.
    DO lpShow
  CASE lnVendor # 1 .AND. laScrMode[3]
    SHOW GET pbInvert ENABLE
ENDCASE
*B605028,1 AMH [End]

*-- end of lfvVendor.

*!*************************************************************
*! Name      : lfGetLocat
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : Get the Location for all styles.
*!*************************************************************
*!
FUNCTION lfGetLocat
*B605028,1 AMH Collecting only selected styles [Start]
*PARAMETER lcVendor, lcCut_Sew

*SELECT STYLE
*SCAN
  *B604562,1 AMH comment all wait window lines [Start]
  *WAIT WINDOW "Selecting style: "+ALLTRIM(style) NOWAIT
  *B604562,1 AMH [End]
*  SCATTER FIELDS style,totplan MEMVAR
  *B604859,1 AMH get the available units for cut & sew operations [Start]
  *B604985,1 AMH let the minimum value of avaliable be 0 [Start]
  *m.nCutAvl = m.TotPlan - lfGetAvl(m.Style,'C')
  *m.nSewAvl = m.TotPlan - lfGetAvl(m.Style,'S')
*  m.nCutAvl = MAX(m.TotPlan - lfGetAvl(m.Style,'C',m.TotPlan),0)
*  m.nSewAvl = MAX(m.TotPlan - lfGetAvl(m.Style,'S',m.TotPlan),0)
  *B604985,1 AMH [End]
  *B604859,1 AMH [End]
*  IF SEEK(STYLE+lcCut_Sew+lcVendor,'LOCSTY')
*    SELECT LOCSTY
    *B604562,1 AMH Remove all variable of fabric & color fields [Start]
    *SCATTER FIELDS ccut_sew,location,totqty,nperc_qty,Fabric,Color MEMVAR
*    SCATTER FIELDS ccut_sew,location,totqty,nperc_qty MEMVAR
    *B604562,1 AMH [End]
*    m.cSelRec = '»'
*    m.lSelRec = .T.
*  ELSE

    *B604562,1 AMH Blank the operation field for not selected style [Start]
    *m.cCut_Sew  = lcCut_Sew
*    m.cCut_Sew  = ''
    *B604562,1 AMH [End]

*    m.Location  = ''
    *B604859,1 AMH But the default value of units & percentage with available [Start]
    *m.TotQty    = TotPlan
*    m.TotQty    = m.nCutAvl
    *m.nPerc_Qty = IIF(TotPlan=0,0,100)
*    m.nPerc_Qty = IIF(TotPlan=0,0,ROUND(m.TotQty/TotPlan*100,3))
    *B604859,1 AMH [End]
    *B604562,1 AMH Remove all variable of fabric & color fields [Start]
    *m.Fabric    = ''
    *m.Color     = ''
    *B604562,1 AMH [End]
*    m.cSelRec   = ' '
*    m.lSelRec   = .F.
*  ENDIF
*  IF SEEK(STYLE+IIF(lcCut_Sew='C','S','C')+lcVendor,'LOCSTY')
*    m.LocOther = lcVendor
    *B604859,1 AMH Update Sewing unit [Start]
*    m.TotQtyOth = LOCSTY.TOTQTY
*    m.nPerc_Oth = LOCSTY.NPERC_QTY
*  ELSE
*    m.LocOther = ''
*    m.TotQtyOth = m.nSewAvl
*    m.nPerc_Oth = IIF(TotPlan=0,0,ROUND(m.TotQtyOth/TotPlan*100,3))
    *B604859,1 AMH [End]
*  ENDIF
*  m.cStatus = 'A'
  *B604859,1 AMH Upcate cChnLoc field [Start]
*  m.cChnLoc = 'Z'
  *B604859,1 AMH [End]
  *B604985,1 AMH don't make duplication if style file have duplication [Start]
  *SELECT (lcTmpStyle)
  *APPEND BLANK
  *GATHER MEMVAR
*  IF !SEEK(m.Style,lcTmpStyle)
*    SELECT (lcTmpStyle)
*    APPEND BLANK
*    GATHER MEMVAR
*  ENDIF
  *B604985,1 AMH [End]
*ENDSCAN
*B604562,1 AMH comment all wait window lines [Start]
*WAIT CLEAR
*=lfReplace(lcTmpStyle,lcVendor,lcCut_Sew)
*B604562,1 AMH [End]
*SELECT (lcTmpStyle)
*LOCATE
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
    SET ORDER TO STYLE
    SCAN REST WHILE STYLE = LEFT(EVALUATE(lcSelStyle+'.CSTYMAJOR'),lnMajorLen)
      SCATTER FIELDS style,totplan MEMVAR
      m.nCutAvl = MAX(m.TotPlan - lfGetAvl(m.Style,'C',m.TotPlan),0)
      m.nSewAvl = MAX(m.TotPlan - lfGetAvl(m.Style,'S',m.TotPlan),0)
      m.cCut_Sew  = ''
      m.Location  = ''
      m.TotQty    = m.nCutAvl
      m.nPerc_Qty = IIF(TotPlan=0,0,ROUND(m.TotQty/TotPlan*100,3))
      m.cSelRec   = ' '
      m.lSelRec   = .F.
      m.LocOther = ''
      m.TotQtyOth = m.nSewAvl
      m.nPerc_Oth = IIF(TotPlan=0,0,ROUND(m.TotQtyOth/TotPlan*100,3))
      m.cStatus = 'A'
      m.cChnLoc = 'Z'
      IF !SEEK(m.Style,lcTmpStyle)
        SELECT (lcTmpStyle)
        APPEND BLANK
        GATHER MEMVAR
      ENDIF
    ENDSCAN
  ENDSCAN
ELSE
  IF llUseSeaso
    FOR lnI = 1 TO ALEN(laRpTarSea)
      SELECT STYLE
      SET ORDER TO STYLE
      SEEK LEFT(laRpTarSea[lnI],3)
      SCAN REST WHILE STYLE = LEFT(laRpTarSea[lnI],3)
        SCATTER FIELDS style,totplan MEMVAR
        m.nCutAvl = MAX(m.TotPlan - lfGetAvl(m.Style,'C',m.TotPlan),0)
        m.nSewAvl = MAX(m.TotPlan - lfGetAvl(m.Style,'S',m.TotPlan),0)
        m.cCut_Sew  = ''
        m.Location  = ''
        m.TotQty    = m.nCutAvl
        m.nPerc_Qty = IIF(TotPlan=0,0,ROUND(m.TotQty/TotPlan*100,3))
        m.cSelRec   = ' '
        m.lSelRec   = .F.
        m.LocOther = ''
        m.TotQtyOth = m.nSewAvl
        m.nPerc_Oth = IIF(TotPlan=0,0,ROUND(m.TotQtyOth/TotPlan*100,3))
        m.cStatus = 'A'
        m.cChnLoc = 'Z'
        IF !SEEK(m.Style,lcTmpStyle)
          SELECT (lcTmpStyle)
          APPEND BLANK
          GATHER MEMVAR
        ENDIF
      ENDSCAN
    ENDFOR
  ELSE
    SELECT STYLE
    SET ORDER TO STYLE
    SCAN
      SCATTER FIELDS style,totplan MEMVAR
      m.nCutAvl = MAX(m.TotPlan - lfGetAvl(m.Style,'C',m.TotPlan),0)
      m.nSewAvl = MAX(m.TotPlan - lfGetAvl(m.Style,'S',m.TotPlan),0)
      m.cCut_Sew  = ''
      m.Location  = ''
      m.TotQty    = m.nCutAvl
      m.nPerc_Qty = IIF(TotPlan=0,0,ROUND(m.TotQty/TotPlan*100,3))
      m.cSelRec   = ' '
      m.lSelRec   = .F.
      m.LocOther = ''
      m.TotQtyOth = m.nSewAvl
      m.nPerc_Oth = IIF(TotPlan=0,0,ROUND(m.TotQtyOth/TotPlan*100,3))
      m.cStatus = 'A'
      m.cChnLoc = 'Z'
      IF !SEEK(m.Style,lcTmpStyle)
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
*B605028,1 AMH [End]
*-- end of lfGetLocat.

*!*************************************************************
*! Name      : lfDelBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
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

**********************************************************************
************************ Business Rules Section **********************
**********************************************************************

*!*************************************************************
*! Name      : lfvOk
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : To validate the Ok button.
*!*************************************************************
*! Example            :  lfvOk()
*!*************************************************************
FUNCTION lfvOk

SELECT (lcTmpStyle)
lnBrRecNo = RECNO()
SET ORDER TO (lcTmpStyl3)
=lfScan('D')
=lfScan('M')
=lfScan('N')

*B605393,1 AMH Save record of lcTmpStyLS in lcTmpStySv [Start]
SELECT (lcTmpStyLS)
SCAN
  SCATTER MEMVAR
  IF !SEEK(EVALUATE(KEY()),lcTmpStySv)
    SELECT (lcTmpStySv)
    APPEND BLANK
  ENDIF
  SELECT (lcTmpStySv)
  GATHER MEMVAR
ENDSCAN
SELECT (lcTmpStyLS)
ZAP
*B605393,1 AMH [End]

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
SHOW GET lnVendor  ENABLE
SHOW GET lcOpr     ENABLE
*-- end of lfvOk.

*!*************************************************************
*! Name      : lfvCan
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : To validate the Cancel button.
*!*************************************************************
*!
FUNCTION lfvCan

*B605393,1 AMH Delete all records of lcTmpStyLS [Start]
PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpStyLS)
ZAP
SELECT (lnAlias)
*B605393,1 AMH [End]

*B604562,1 AMH Use lfRstLocat function for optimizing the process [Start]
*=lfvVendor()
=lfRstLocat()
*B604562,1 AMH [End]

SHOW GET lnVendor  ENABLE
SHOW GET lcOpr     ENABLE
llCUpDate = .F.
*-- end of lfvCan.

*!*************************************************************
*! Name      : lpSavScr
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : To Save care label in Style file.
*!*************************************************************
*!
PROCEDURE lpSavScr

=lfReplace('LOCSTY','','')
IF USED(lcTmpStySv)
  SELECT(lcTmpStySv)
  ZAP
ENDIF
*B604562,1 AMH Defining lcOldLocat to hold the current location as the old one
*B604562,1 AMH before calling lfvVendor [Start]
lcOldLocat = laVendor(lnVendor,2)
*B604562,1 AMH [End]
=lfvVendor()
SHOW GET pbSelect DISABLE
SHOW GET pbSelAll DISABLE
SHOW GET pbSelNon DISABLE
SHOW GET pbInvert DISABLE
SHOW GET pbOk     DISABLE
SHOW GET pbCan    DISABLE
*-- end of lpSavScr.

*!*************************************************************
*! Name      : lpClsScr
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : To Cancel all changes.
*!*************************************************************
*!
PROCEDURE lpClsScr

IF laScrMode[3]

  *B605393,1 AMH Delete all records of lcTmpStyLS [Start]
  PRIVATE lnAlias
  lnAlias = SELECT(0)
  SELECT (lcTmpStyLS)
  ZAP
  SELECT (lnAlias)
  *B605393,1 AMH [End]

  *B604562,1 AMH Use lfRstLocat function for optimizing the process [Start]
  *=lfDelBrow()
  IF USED(lcTmpStySv)
    SELECT(lcTmpStySv)
    *B604859,1 AMH Update Avalible values [Start]
    SCAN
      DO CASE
        CASE CSTATUS = 'D'
          IF SEEK(STYLE+CCUT_SEW+LOCATION,'LOCSTY')
            lnAvl = -LOCSTY.TOTQTY
          ELSE
            lnAvl = 0
          ENDIF
        CASE CSTATUS = 'M'
          IF SEEK(STYLE+CCUT_SEW+LOCATION,'LOCSTY')
            lnAvl = TOTQTY-LOCSTY.TOTQTY
          ELSE
            lnAvl = 0
          ENDIF
        CASE CSTATUS = 'N'
          lnAvl = TOTQTY
      ENDCASE
      SELECT (lcTmpStyle)
      SEEK EVALUATE(lcTmpStySv+'.STYLE')
      IF SUBSTR(lcOpr,1,1)='C'
        *B604985,1 AMH let the minimum value of avalible be 0 [Start]
        *REPLACE NCUTAVL WITH EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl')+lnAvl
        REPLACE NCUTAVL WITH MAX(EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl')+lnAvl,0)
      ELSE
        *REPLACE NSEWAVL WITH EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl')+lnAvl
        REPLACE NSEWAVL WITH MAX(EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl')+lnAvl,0)
        *B604985,1 AMH [End]
      ENDIF
    ENDSCAN
    *B604859,1 AMH [End]
    ZAP
  ENDIF
  *=lfGetLocat(ALLTRIM(laVendor(lnVendor,2)),IIF(lcOpr='Cutting','C','S'))
  =lfRstLocat()
  *B604562,1 AMH [End]
  =lfDispBrow()
ENDIF
*-- end of lpClsScr.

*!*************************************************************
*! Name      : lfReplace
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : Replace the selected records either in the master or in the Temp
*!*************************************************************
*!
FUNCTION lfReplace
PARAMETER lcAlias,lcVendor,lcCut_Sew

SELECT (lcTmpStySv)
SCAN
  *B604562,1 AMH comment all wait window lines [Start]
  *WAIT WINDOW "Updating style: " + Style NOWAIT
  *B604562,1 AMH [End]
  IF lcAlias = "LOCSTY"
    DO CASE
      CASE cStatus = 'N'
        SCATTER MEMVAR
        SELECT LOCSTY
        =RLOCK()
        *B604985,1 AMH don't make duplication when save [Start]
        *APPEND BLANK
        IF !SEEK(EVALUATE(lcTmpStySv+'.STYLE')+EVALUATE(lcTmpStySv+'.CCUT_SEW')+;
                 PADR(EVALUATE(lcTmpStySv+'.LOCATION'),8))
          APPEND BLANK
        ENDIF
        *B604985,1 AMH [End]
        GATHER MEMVAR
        =gfAdd_info('LOCSTY')
        UNLOCK
        SELECT (lcTmpStySv)
      CASE cStatus = 'M'
        *B604562,1 AMH Remove all variable of fabric & color fields [Start]
        *SCATTER FIELDS TotQty,nPerc_Qty,Fabric,Color MEMVAR
        SCATTER FIELDS TotQty,nPerc_Qty MEMVAR
        *B604562,1 AMH [End]
        *B604985,1 AMH Seek with padr not alltrim [Start]
        *IF SEEK(style+cCut_Sew+ALLTRIM(Location),'LOCSTY')
        IF SEEK(style+cCut_Sew+PADR(Location,8),'LOCSTY')
        *B604985,1 AMH [End]
          SELECT LOCSTY
          =RLOCK()
          GATHER MEMVAR FIELDS TotQty,nPerc_Qty
          =gfAdd_info('LOCSTY')
          UNLOCK
          SELECT (lcTmpStySv)
        ENDIF
      CASE cStatus = 'D'
        *B604985,1 AMH Seek with padr not alltrim [Start]
        *IF SEEK(style+cCut_Sew+ALLTRIM(Location),'LOCSTY')
        IF SEEK(style+cCut_Sew+PADR(Location,8),'LOCSTY')
        *B604985,1 AMH [End]
          SELECT LOCSTY
          =RLOCK()
          DELETE
          UNLOCK
          SELECT (lcTmpStySv)
          *B604562,1 AMH Changing the status of the selected record [Start]
          IF SEEK(STYLE,lcTmpStyle)
            SELECT (lcTmpStyle)
            REPLACE CSTATUS WITH 'A'
            SELECT (lcTmpStySv)
          ENDIF
          *B604562,1 AMH [End]
        ENDIF
    ENDCASE
    *B604985,1 AMH Delete the saved records from TMPLOCST file [Start]
    =lfDelLock()
    *B604985,1 AMH [End]
  ENDIF
  IF lcAlias = lcTmpStyle
    IF Location = lcVendor
      IF cCut_Sew = lcCut_Sew
        SCATTER MEMVAR
        
        *B604562,1 AMH seek for style only since opreation field is empty [Start]
        *IF SEEK(style+lcCut_Sew,lcTmpStyle)
        IF SEEK(style,lcTmpStyle)
        *B604562,1 AMH [End]
        
          SELECT (lcTmpStyle)
          
          *B604562,1 AMH Update operation field and removing Fabric & Color [Start]
          *GATHER FIELDS TotQty,nPerc_Qty,Fabric,Color,Location,cStatus MEMVAR
          *B604859,1 AMH update sewing unit [Start]
          *GATHER FIELDS TotQty,nPerc_Qty,Location,cStatus,cCut_Sew MEMVAR
          IF lcCut_Sew = 'S'
            m.TotQtyOth = m.TotQty
            m.nPerc_Oth = m.nPerc_Qty
            GATHER FIELDS TotQtyOth,nPerc_Oth,Location,cStatus,cCut_Sew MEMVAR
          ELSE
            GATHER FIELDS TotQty,nPerc_Qty,Location,cStatus,cCut_Sew MEMVAR
          ENDIF
          *B604859,1 AMH [End]
          *B604562,1 AMH [End]
          
          REPLACE cSelRec  WITH IIF(cStatus#'D','»',' '),;
                  lSelRec  WITH IIF(cStatus#'D',.T.,.F.)
        ENDIF
      ELSE
        *B604859,1 AMH Update locother field [Start]
        *IF SEEK(style+lcCut_Sew,lcTmpStyle)
        IF SEEK(style,lcTmpStyle)
          SELECT (lcTmpStyle)
          *REPLACE LocOther WITH &lcTmpStySv..Location
          IF lcCut_Sew = 'C'
            REPLACE LocOther  WITH EVALUATE(lcTmpStySv+'.Location'),;
                    TotQtyOth WITH EVALUATE(lcTmpStySv+'.TotQty'),;
                    nPerc_Oth WITH EVALUATE(lcTmpStySv+'.nPerc_Qty')
          ELSE
            REPLACE LocOther  WITH EVALUATE(lcTmpStySv+'.Location'),;
                    TotQty    WITH EVALUATE(lcTmpStySv+'.TotQty'),;
                    nPerc_Qty WITH EVALUATE(lcTmpStySv+'.nPerc_Qty')
          ENDIF
        *B604859,1 AMH [End]
        ENDIF
      ENDIF
      SELECT (lcTmpStySv)
    *B604562,1 AMH unselect style if another location [Start]
    ELSE
      *B604859,1 AMH Update lcTmpStyle file with the correct valuse [Start]
      *IF cCut_Sew = lcCut_Sew .AND. SEEK(style,lcTmpStyle) .AND. lcOldLocat <> lcVendor
      IF cCut_Sew = lcCut_Sew .AND. SEEK(style,lcTmpStyle) .AND. EVALUATE(lcTmpStyle+'.Location') <>lcVendor;
         .AND. lcOldLocat <> lcVendor
        SELECT (lcTmpStyle)
        *=SEEK(LOCSTY.STYLE,'STYLE')
        =SEEK(STYLE,'STYLE')
        *REPLACE cSelRec   WITH ' ',;
                lSelRec   WITH .F.,;
                cStatus   WITH 'A',;
                cCut_Sew  WITH '',;
                Location  WITH '',;
                TotQty    WITH STYLE.TOTPLAN,;
                nPerc_Qty WITH IIF(TotQty=0,0,100),;
                LocOther  WITH ''
        REPLACE cSelRec   WITH ' ',;
                lSelRec   WITH .F.,;
                cStatus   WITH 'A',;
                cCut_Sew  WITH '',;
                Location  WITH '',;
                TotQty    WITH nCutAvl,;
                TotQtyOth WITH nSewAvl,;
                nPerc_Qty WITH IIF(STYLE.TOTPLAN=0,0,ROUND(TOTQTY/STYLE.TOTPLAN*100,3)),;
                nPerc_Oth WITH IIF(sTYLE.TOTPLAN=0,0,ROUND(TOTQTYOTH/STYLE.TOTPLAN*100,3)),;
                LocOther  WITH ''
      ENDIF
      *B604859,1 AMH [End]
    *B604562,1 AMH [End]
    ENDIF
  ENDIF
ENDSCAN
*B604562,1 AMH comment all wait window lines [Start]
*WAIT CLEAR
*B604562,1 AMH [End]
*-- end of lfReplace.

*!*************************************************************
*! Name      : lpShow
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : Handle controfl show.
*!*************************************************************
*!
PROCEDURE lpShow

DO CASE
  CASE laScrMode[1]                       && Select mode.
    *B605028,1 AMH Display the OG screen [Start]
    *laScrMode = .F.
    *laScrMode[2] = .T.
    SHOW GET pbSlct   DISABLE
    SHOW GET pbDlt    DISABLE
    SHOW GET pbSelect DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbOk     DISABLE
    SHOW GET pbCan    DISABLE
    SHOW GET lnVendor ENABLE
    SHOW GET pbScope  ENABLE
    SHOW GET pbEdt    DISABLE
    IF llFirst
      llFirst = .F.
      =lfvScope()
    ENDIF
    *B605028,1 AMH [End]
  CASE laScrMode[2]                       && View mode 
    SHOW GET pbSlct   DISABLE
    SHOW GET pbDlt    DISABLE
    SHOW GET pbSelect DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbOk     DISABLE
    SHOW GET pbCan    DISABLE
    SHOW GET lnVendor ENABLE
    SHOW GET lcOpr    ENABLE
    *B605028,1 AMH enable the scope bottons [Start]
    SHOW GET pbScope  ENABLE
    SHOW GET pbEdt    ENABLE
    *B605028,1 AMH [End]
  CASE laScrMode[3]                       && Edit mode 
    SHOW GET lnVendor DISABLE
    SHOW GET lcOpr    DISABLE
    SHOW GET pbSelect ENABLE
    SHOW GET pbInvert ENABLE
    SHOW GET pbOk     ENABLE
    SHOW GET pbCan    ENABLE
    SHOW GET pbSav    DISABLE
    SHOW GET lcStyle  DISABLE
    SHOW GET lnUnit   DISABLE
    SHOW GET lnPer    DISABLE
    *B604562,1 AMH Remove all variable of fabric & color fields [Start]
    *SHOW GET ibFabric DISABLE
    *SHOW GET lcFabric DISABLE
    *SHOW GET ibColor  DISABLE
    *SHOW GET lcColor  DISABLE
    *B604562,1 AMH [End]
    *B605028,1 AMH disable the scope bottons [Start]
    SHOW GET pbScope  DISABLE
    *B605028,1 AMH [End]
    STORE 'ENABLE' TO lcSelSt
    =lfvpbSel()
    *B604859,1 AMH Set the current object [Start]
    _CUROBJ = OBJNUM(pbSelect)
    *B604859,1 AMH [End]
ENDCASE
*-- end of lpShow.

*!*************************************************************
*! Name      : lfChangSt
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : chang the status of saving records.
*!*************************************************************
*!
FUNCTION lfChangSt

DO CASE
  CASE cStatus = 'A'
    REPLACE cStatus WITH IIF(lSelRec,'N','D')
  CASE cStatus = 'M'
    REPLACE cStatus WITH 'D'
  CASE cStatus = 'N'
    REPLACE cStatus WITH 'A'
  CASE cStatus = 'D'
    IF SEEK(style+cCut_Sew+ALLTRIM(laVendor(lnVendor,2)),'LOCSTY')
      IF LOCSTY.TOTQTY = lnUnit .AND. LOCSTY.NPERC_QTY = lnPer
        REPLACE cStatus WITH 'A'
      ELSE
        REPLACE cStatus WITH 'M'
      ENDIF
    ENDIF
ENDCASE
*-- end of lfChangSt.

*!*************************************************************
*! Name      : lfvUnit
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/23/2000
*! Purpose   : Valid function of Unite field.
*!*************************************************************
*!
FUNCTION lfvUnit

IF lnUnit < 0
  =gfModalGen("TRM00000B00000","","",.F.,'Unit Cannot be less than zero.')
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF &lcTmpStyle..TotPlan = 0 .AND. lnOldUnit = lnUnit
  RETURN
ENDIF

*B604859,1 AMH Check if the units over the available [Start]
lnAvl = lfGetSvAvl(lcTmpStySv)
lnAvl = IIF(lnAvl=0 .AND. EVALUATE(lcTmpStySv+'.cStatus') # 'D',lfGetSvAvl('LOCSTY'),lnAvl)
IF lnUnit > EVALUATE(lcTmpStyle+'.n'+SUBSTR(lcOpr,1,3)+'Avl')+lnAvl
  =gfModalGen("TRM00000B00000","","",.F.,'Units cannot be greater than '+;
              ALLTRIM(STR(EVALUATE(lcTmpStyle+'.n'+SUBSTR(lcOpr,1,3)+'Avl')+lnAvl,7))+'.')
  lnUnit = lnOldUnit
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

*B604951,1 AMH round lnper to 3 digites [Start]
*lnPer = IIF(&lcTmpStyle..TotPlan=0,0,lnUnit/&lcTmpStyle..TotPlan*100)
lnPer = ROUND(IIF(&lcTmpStyle..TotPlan=0,0,lnUnit/&lcTmpStyle..TotPlan*100),3)
*B604951,1 AMH [End]

*REPLACE &lcTmpStyle..TotQty WITH lnUnit,;
        &lcTmpStyle..nPerc_Qty WITH lnPer,;
        cStatus WITH IIF(cStatus='A','M',cStatus)
IF SUBSTR(lcOpr,1,1) = 'C'
  REPLACE &lcTmpStyle..TotQty WITH lnUnit,;
          &lcTmpStyle..nPerc_Qty WITH lnPer,;
          cStatus WITH IIF(cStatus='A','M',cStatus)
ELSE
  REPLACE &lcTmpStyle..TotQtyOth WITH lnUnit,;
          &lcTmpStyle..nPerc_Oth WITH lnPer,;
          cStatus WITH IIF(cStatus='A','M',cStatus)
ENDIF
*B604859,1 AMH [End]
llCUpDate = .T.
SHOW GET lnPer
SHOW WINDOW (lcStyBrow) REFRESH SAME
*-- end of lfvUnit.

*!*************************************************************
*! Name      : lfvPer
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/23/2000
*! Purpose   : Valid function of Percentage field.
*!*************************************************************
*!
FUNCTION lfvPer

IF lnPer < 0
  =gfModalGen("TRM00000B00000","","",.F.,'Percentage Cannot be less than zero.')
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF &lcTmpStyle..TotPlan = 0 .AND. lnOldPer = lnPer
  RETURN
ENDIF

*B604859,1 AMH Check if the units over the available [Start]
lnAvl = lfGetSvAvl(lcTmpStySv)
lnAvl = IIF(lnAvl=0 .AND. EVALUATE(lcTmpStySv+'.cStatus') # 'D',lfGetSvAvl('LOCSTY'),lnAvl)

*B605393,1 AMH Display a screen to adjust all locations asigned to the current style [Start]
=lfGetStyLc()
IF RECCOUNT(lcTmpStyLc) > 0 .AND. lnOldPer # lnPer
  IF lnPer = 100
    IF gfModalGen("TRM00000B38006","","",.F.,;
             'Are you sure you want to remove style: '+style+' from any other locations ?') = 2
      lnPer = lnOldPer
      _CUROBJ = _CUROBJ
      RETURN
    ELSE
      SELECT (lcTmpStyLc)
      REPLACE ALL TOTQTY    WITH 0,;
                  NPERC_QTY WITH 0,;
                  CSTATUS   WITH 'D'
      =lfGetStyLS()
      SELECT (lcTmpStyle)
      lnAvl = TOTPLAN
      REPLACE ('n'+SUBSTR(lcOpr,1,3)+'Avl') WITH 0
    ENDIF
  ELSE
    =lfClearKey()
    ON KEY LABEL ESC   DO lfvLocCan
    DO (gcScrDir+gcWinAppl+'\MFSTYLOC.SPX')
    =lfSetKey()
    IF !EMPTY(laVendor[1,2])
      DIMENSION laVendor[ALEN(laVendor,1)+1,2]
      =AINS(laVendor,1)
      laVendor[1,1] = 'Select Location'
      laVendor[1,2] = SPACE(8)
    ENDIF
  ENDIF
ENDIF
*B605393,1 AMH [End]

*B604951,1 AMH round the available to 3 digites [Start]
*IF lnPer > (EVALUATE(lcTmpStyle+'.n'+SUBSTR(lcOpr,1,3)+'Avl')+lnAvl)/EVALUATE(lcTmpStyle+'.TotPlan')*100
IF lnPer > ROUND((EVALUATE(lcTmpStyle+'.n'+SUBSTR(lcOpr,1,3)+'Avl')+lnAvl)/EVALUATE(lcTmpStyle+'.TotPlan')*100,3)
*B604951,1 AMH [End]

  =gfModalGen("TRM00000B00000","","",.F.,'Percentage cannot be greater than '+;
              ALLTRIM(STR(ROUND((EVALUATE(lcTmpStyle+'.n'+SUBSTR(lcOpr,1,3)+'Avl')+lnAvl)/EVALUATE(lcTmpStyle+'.TotPlan')*100,3),7,3))+'%.')
  lnPer = lnOldPer
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

*B604951,1 AMH round the units to 0 digites [Start]
*lnUnit = lnPer*&lcTmpStyle..TotPlan/100
lnUnit = ROUND(lnPer*&lcTmpStyle..TotPlan/100,0)
*B604951,1 AMH [End]

*REPLACE &lcTmpStyle..TotQty WITH lnUnit,;
		&lcTmpStyle..nPerc_Qty WITH lnPer,;
		cStatus WITH IIF(cStatus='A','M',cStatus)
IF SUBSTR(lcOpr,1,1) = 'C'
  REPLACE &lcTmpStyle..TotQty WITH lnUnit,;
          &lcTmpStyle..nPerc_Qty WITH lnPer,;
          cStatus WITH IIF(cStatus='A','M',cStatus)
ELSE
  REPLACE &lcTmpStyle..TotQtyOth WITH lnUnit,;
          &lcTmpStyle..nPerc_Oth WITH lnPer,;
          cStatus WITH IIF(cStatus='A','M',cStatus)
ENDIF
*B604859,1 AMH [End]
llCUpDate = .T.
SHOW GET lnUnit
SHOW WINDOW (lcStyBrow) REFRESH SAME
*-- end of lfvPer.

*!*************************************************************
*! Name      : lfvFabric
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/23/2000
*! Purpose   : Valid function of Fabric field.
*!*************************************************************
*B604562,1 AMH Comment the next two functions [Start]
*!*************************************************************
*FUNCTION lfvFabric

*PRIVATE lnAlias
*lnAlias = SELECT(0)
*IF EMPTY(lcFabric) AND !llbrowse
  *-- Ok continue without change lcFabric
*ELSE
*  IF (llbrowse OR !SEEK(lcFabric,'FABRIC')) AND fabrow(@lcFabric,'*')
*  ENDIF
*  IF EMPTY(lcFabric)
*    _CUROBJ = _CUROBJ
*  ENDIF
*ENDIF
*llbrowse = .F.
*SELECT(lnalias)
*lcColor = IIF(lcOldFab==lcFabric,lcColor,SPACE(0))
*REPLACE &lcTmpStyle..Fabric WITH lcFabric,;
*        &lcTmpStyle..Color  WITH lcColor,;
*        cStatus WITH IIF(cStatus='A','M',cStatus)
*llCUpDate = .T.
*SHOW WINDOW (lcStyBrow) REFRESH
*lcEn_Dis = IIF(!EMPTY(lcFabric),'ENABLE','DISABLE')
*SHOW GET lcColor  &lcEn_Dis
*SHOW GET ibColor  &lcEn_Dis
*-- end of lfvFabric.

*!*************************************************************
*! Name      : lfvColor
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/23/2000
*! Purpose   : Valid function of Color field.
*!*************************************************************
*!
*FUNCTION lfvColor

*PRIVATE lnAlias, lcOrder
*lnAlias = SELECT(0)
*IF EMPTY(lcColor) AND !llbrowse
*  *-- Ok continue without change lcColor
*ELSE
*  SELECT FABRIC
*  lcOrder = SET('ORDER')
*  SET ORDER TO TAG FABRIC
*  IF llbrowse .OR. !SEEK(lcFabric+lcColor)
*    lcBrowItm = lcFabric
*    lcBrowClr = CHR(240)
*    DO FABROW WITH lcBrowItm,lcBrowClr
*    lcColor  = lcBrowClr
*    IF EMPTY(lcColor)
*      _CUROBJ = _CUROBJ
*    ENDIF
*  ENDIF
*  SET ORDER TO &lcOrder.
*ENDIF
*llbrowse = .F.
*SELECT(lnalias)
*REPLACE &lcTmpStyle..Color WITH lcColor,;
*        cStatus WITH IIF(cStatus='A','M',cStatus)
*llCUpDate = .T.
*SHOW WINDOW (lcStyBrow) REFRESH
*-- end of lfvColor.
*B604562,1 AMH [End]

*!*************************************************************
*! Name      : lfwUnit
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/23/2000
*! Purpose   : When function of Unite field.
*!*************************************************************
*!
FUNCTION lfwUnit

lnOldUnit = lnUnit
*B604985,1 AMH Check if this style used by another user [Start]
IF !lfChckLock(.T.)
  _CUROBJ = OBJNUM(pbOk)
  RETURN .F.
ENDIF
PRIVATE lnAvl
lnAvl = lfGetSvAvl(lcTmpStySv)
lnAvl = IIF(lnAvl=0 .AND. EVALUATE(lcTmpStySv+'.cStatus') # 'D',lfGetSvAvl('LOCSTY'),lnAvl)

*B605393,1 AMH Calculate the available correctlly [Start]
*REPLACE nCutAvl WITH MAX(TotPlan - lfGetAvl(Style,'C',TotPlan),0)
*REPLACE nSewAvl WITH MAX(TotPlan - lfGetAvl(Style,'S',TotPlan),0)
REPLACE nCutAvl WITH MAX(TotPlan - IIF(SEEK(Style+'C',lcTmpStyLS),lfGetLocAv();
                                       ,lfGetAvl(Style,'C',TotPlan)),0)
REPLACE nSewAvl WITH MAX(TotPlan - IIF(SEEK(Style+'S',lcTmpStyLS),lfGetLocAv();
                                       ,lfGetAvl(Style,'S',TotPlan)),0)
*B605393,1 AMH [End]

*B605393,1 AMH Make updates in case of status 'A' only [Start]
*REPLACE TotQty    WITH IIF(lnAvl=0,EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl'),lnAvl),;
        nPerc_Qty WITH IIF(TotPlan=0,0,ROUND(TotQty/TotPlan*100,3))
*lnUnit = TotQty
*lnPer  = nPerc_Qty
*SHOW GET lnUnit
*SHOW GET lnPer
*SHOW WINDOW (lcStyBrow) REFRESH
IF CSTATUS = 'A'
  REPLACE TotQty    WITH IIF(lnAvl=0,EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl'),lnAvl),;
          nPerc_Qty WITH IIF(TotPlan=0,0,ROUND(TotQty/TotPlan*100,3))
  lnUnit = TotQty
  lnPer  = nPerc_Qty
  SHOW GET lnUnit
  SHOW GET lnPer
  SHOW WINDOW (lcStyBrow) REFRESH
ENDIF
*B605393,1 AMH [End]

*B604985,1 AMH [End]
*-- end of lfwUnit.

*!*************************************************************
*! Name      : lfwPer
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/23/2000
*! Purpose   : When function of Percentage field.
*!*************************************************************
*!
FUNCTION lfwPer

lnOldPer = lnPer
*B604985,1 AMH check if this style used by another user [Start]
IF !lfChckLock(.T.)
  RETURN .F.
ENDIF
PRIVATE lnAvl
lnAvl = lfGetSvAvl(lcTmpStySv)
lnAvl = IIF(lnAvl=0 .AND. EVALUATE(lcTmpStySv+'.cStatus') # 'D',lfGetSvAvl('LOCSTY'),lnAvl)

*B605393,1 AMH Calculate the available correctlly [Start]
*REPLACE nCutAvl WITH MAX(TotPlan - lfGetAvl(Style,'C',TotPlan),0)
*REPLACE nSewAvl WITH MAX(TotPlan - lfGetAvl(Style,'S',TotPlan),0)
REPLACE nCutAvl WITH MAX(TotPlan - IIF(SEEK(Style+'C',lcTmpStyLS),lfGetLocAv();
                                       ,lfGetAvl(Style,'C',TotPlan)),0)
REPLACE nSewAvl WITH MAX(TotPlan - IIF(SEEK(Style+'S',lcTmpStyLS),lfGetLocAv();
                                       ,lfGetAvl(Style,'S',TotPlan)),0)
*B605393,1 AMH [End]

*B605393,1 AMH Make updates in case of status 'A' only [Start]
*REPLACE TotQty    WITH IIF(lnAvl=0,EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl'),lnAvl),;
        nPerc_Qty WITH IIF(TotPlan=0,0,ROUND(TotQty/TotPlan*100,3))
*lnUnit = TotQty
*lnPer  = nPerc_Qty
*SHOW GET lnUnit
*SHOW GET lnPer
*SHOW WINDOW (lcStyBrow) REFRESH
IF CSTATUS = 'A'
  REPLACE TotQty    WITH IIF(lnAvl=0,EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl'),lnAvl),;
          nPerc_Qty WITH IIF(TotPlan=0,0,ROUND(TotQty/TotPlan*100,3))
  lnUnit = TotQty
  lnPer  = nPerc_Qty
  SHOW GET lnUnit
  SHOW GET lnPer
  SHOW WINDOW (lcStyBrow) REFRESH
ENDIF
*B605393,1 AMH [End]

*B604985,1 AMH [End]
*-- end of lfwPer.

*!*************************************************************
*! Name      : lfwFabric
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/23/2000
*! Purpose   : When function of Fabric field.
*!*************************************************************
*B604562,1 AMH Comment the next function [Start]
*!*************************************************************
*FUNCTION lfwFabric

*lcOldFab = lcFabric
*-- end of lfwFabric.
*B604562,1 AMH [End]

*!*************************************************************
*! Name      : lfScan
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/11/2000
*! Purpose   : To Update lcTmpStySv with spce. cStatus.
*!*************************************************************
*! Example            :  lfScan()
*!*************************************************************
FUNCTION lfScan
PARAMETERS lcStatus
PRIVATE llRet
llRet = .T.
IF SEEK (lcStatus)
  *B604985,1 AMH don't save zeros [Start]
  *SCAN REST WHILE cStatus = lcStatus
  *SCAN REST WHILE cStatus = lcStatus FOR TOTQTY > 0 .AND. NPERC_QTY > 0

  *B605133,1 AMH Consider case of sewing operation [Start]
  *SCAN REST WHILE cStatus = lcStatus FOR NPERC_QTY > 0
  
  *B605393,1 AMH Scan for all unselected records [Start]
  *SCAN REST WHILE cStatus = lcStatus FOR IIF(SUBSTR(lcOpr,1,1)='C',NPERC_QTY,NPERC_OTH) > 0
  SCAN REST WHILE cStatus = lcStatus;
            FOR IIF(lcStatus = 'D',.T.,IIF(SUBSTR(lcOpr,1,1)='C',NPERC_QTY,NPERC_OTH) > 0)
  *B605393,1 AMH [End]
  
  *B605133,1 AMH [End]

  *B604985,1 AMH [End]
    *B604562,1 AMH comment all wait window lines [Start]
    *WAIT WINDOW "Updating style: " + Style NOWAIT
    *B604562,1 AMH [End]
    IF lcStatus $ 'MN'
      = lfvBrows(@llRet,'P')
    ENDIF
    *B604859,1 AMH Update the available values [Start]
    lnAvl = IIF(cStatus='D',lfGetSvAvl('LOCSTY'),lfGetSvAvl(lcTmpStySv))
    lnAvl = IIF(lnAvl=0 .AND. cStatus='M',lfGetSvAvl('LOCSTY'),lnAvl)
    IF SUBSTR(lcOpr,1,1)='C'
      *B604985,1 AMH let the minimum value of avaliable be 0 [Start]
      *REPLACE NCUTAVL WITH EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl')+lnAvl-IIF(cStatus='D',0,TotQty)
      REPLACE NCUTAVL WITH MAX(EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl')+lnAvl-IIF(cStatus='D',0,TotQty),0)
    ELSE
      *REPLACE NSEWAVL WITH EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl')+lnAvl-IIF(cStatus='D',0,TotQtyOth)
      REPLACE NSEWAVL WITH MAX(EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl')+lnAvl-IIF(cStatus='D',0,TotQtyOth),0)
      *B604985,1 AMH [End]
    ENDIF
    *B604859,1 AMH [End]
    IF cStatus = lcStatus
      SCATTER MEMVAR
      SELECT (lcTmpStySv)
      IF !SEEK(&lcTmpStyle..style+&lcTmpStyle..cCut_Sew+&lcTmpStyle..Location)
        APPEND BLANK
      ENDIF
      *B604859,1 AMH Save the correct values [Start]
      IF SUBSTR(lcOpr,1,1)='S'
        m.TotQty = m.TotQtyOth
        m.nPerc_Qty = m.nPerc_Oth
      ENDIF
      *B604859,1 AMH Save the correct values [Start]
      GATHER MEMVAR
      REPLACE Location WITH laVendor[lnVendor,2]
    ENDIF
  ENDSCAN
  *B604562,1 AMH comment all wait window lines [Start]
  *WAIT CLEAR
  *B604562,1 AMH [End]
ENDIF
*-- end of lfScan.

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
*! Name      : lfRefLocat
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/17/2001
*! Purpose   : Refresh data for all styles.
*!*************************************************************
*B604562,1 AMH Using this function instead of lfGetLocat to validate 
*B604562,1 AMH the location field.
*!*************************************************************
FUNCTION lfRefLocat

PRIVATE lcOrder
SELECT LOCSTY
lcOrder = SET('ORDER')
SET ORDER TO TAG LOCSTYLOC
SEEK lcOldLocat + SUBSTR(lcOpr,1,1)
SCAN REST WHILE LOCATION + CCUT_SEW = lcOldLocat + SUBSTR(lcOpr,1,1)
  SELECT (lcTmpStyle)
  SEEK LOCSTY.STYLE+LOCSTY.CCUT_SEW+LOCSTY.LOCATION
  =SEEK(LOCSTY.STYLE,'STYLE')
  *B604859,1 AMH Update the lcTmpStyle file with the correct values [Start]
  *REPLACE cSelRec   WITH ' ',;
          lSelRec   WITH .F.,;
          cStatus   WITH 'A',;
          cCut_Sew  WITH '',;
          Location  WITH '',;
          TotQty    WITH STYLE.TOTPLAN,;
          nPerc_Qty WITH IIF(TotQty=0,0,100),;
          LocOther  WITH ''
  REPLACE cSelRec   WITH ' ',;
          lSelRec   WITH .F.,;
          cStatus   WITH 'A',;
          cCut_Sew  WITH '',;
          Location  WITH '',;
          TotQty    WITH nCutAvl,;
          TotQtyOth WITH nSewAvl,;
          nPerc_Qty WITH IIF(STYLE.TOTPLAN=0,0,ROUND(TotQty/STYLE.TOTPLAN*100,3)),;
          nPerc_Oth WITH IIF(STYLE.TOTPLAN=0,0,ROUND(TotQtyOth/STYLE.TOTPLAN*100,3)),;
          LocOther  WITH ''
  *B604859,1 AMH [End]
ENDSCAN
*B604859,1 AMH Update locother field [Start]
SEEK lcOldLocat + IIF(SUBSTR(lcOpr,1,1)='C','S','C')
SCAN REST WHILE LOCATION + CCUT_SEW = lcOldLocat + IIF(SUBSTR(lcOpr,1,1)='C','S','C')
  SELECT (lcTmpStyle)
  SEEK LOCSTY.STYLE
  *B604859,1 AMH Update locother field [Start]
  *REPLACE LocOther  WITH ''
  REPLACE LocOther WITH '',;
          TotQty    WITH nCutAvl,;
          TotQtyOth WITH nSewAvl,;
          nPerc_Qty WITH IIF(TOTPLAN=0,0,ROUND(TotQty/TOTPLAN*100,3)),;
          nPerc_Oth WITH IIF(TOTPLAN=0,0,ROUND(TotQtyOth/TOTPLAN*100,3))
  *B604859,1 AMH [End]
ENDSCAN
*B604859,1 AMH [End]
=lfReplace(lcTmpStyle,laVendor(lnVendor,2),SUBSTR(lcOpr,1,1))
SELECT LOCSTY
SEEK laVendor(lnVendor,2) + SUBSTR(lcOpr,1,1)
SCAN REST WHILE LOCATION + CCUT_SEW = laVendor(lnVendor,2) + SUBSTR(lcOpr,1,1)
  SCATTER FIELDS ccut_sew,location,totqty,nperc_qty MEMVAR
  m.cSelRec = '»'
  m.lSelRec = .T.
  m.cStatus = 'A'
  SELECT (lcTmpStyle)
  SEEK LOCSTY.STYLE
  IF CSTATUS = 'A'
    *B604859,1 AMH Update unit for each operation [Start]
    *GATHER FIELDS ccut_sew,location,totqty,nperc_qty,cSelRec,lSelRec,cStatus MEMVAR
    IF m.cCut_Sew = 'S'
      m.TotQtyOth = m.TotQty
      m.nPerc_Oth = m.nPerc_Qty
      GATHER FIELDS ccut_sew,location,totqtyOth,nperc_Oth,cSelRec,lSelRec,cStatus MEMVAR
    ELSE
      GATHER FIELDS ccut_sew,location,totqty,nperc_qty,cSelRec,lSelRec,cStatus MEMVAR
    ENDIF
    *B604859,1 AHM [End]
  ENDIF
ENDSCAN
SEEK laVendor(lnVendor,2) + IIF(SUBSTR(lcOpr,1,1)='C','S','C')
SCAN REST WHILE LOCATION + CCUT_SEW = laVendor(lnVendor,2) + IIF(SUBSTR(lcOpr,1,1)='C','S','C')
  SELECT (lcTmpStyle)
  *B604859,1 AMH Update locother field [Start]
  *SEEK LOCSTY.STYLE+SUBSTR(lcOpr,1,1)+LOCSTY.LOCATION
  SEEK LOCSTY.STYLE
  *REPLACE LocOther WITH laVendor(lnVendor,2)
  IF SUBSTR(lcOpr,1,1)='C'
    REPLACE LocOther  WITH laVendor(lnVendor,2),;
            TotQtyOth WITH LOCSTY.TOTQTY,;
            nPerc_Oth WITH LOCSTY.NPERC_QTY
  ELSE
    REPLACE LocOther  WITH laVendor(lnVendor,2),;
            TotQty    WITH LOCSTY.TOTQTY,;
            nPerc_Qty WITH LOCSTY.NPERC_QTY
  ENDIF
  *B604859,1 AMH [End]
ENDSCAN
SET ORDER TO &lcOrder.
SELECT (lcTmpStyle)
LOCATE
*-- end of lfRefLocat.
*B604562,1 AMH [End]

*!*************************************************************
*! Name      : lfRstLocat
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/17/2001
*! Purpose   : Restore Data for all styles.
*!*************************************************************
*B604562,1 AMH Using this function instead of lfGetLocat to validate 
*B604562,1 AMH the cancel botton.
FUNCTION lfRstLocat

PRIVATE lcOrder
SELECT (lcTmpStyle)
lcOrder = SET('ORDER')
SET ORDER TO (lcTmpStyl3)
SCAN FOR cStatus <> 'A'
  *B604859,1 AMH Update the lcTmpStyle file with the correct values [Start]
  *IF cStatus = 'N' .AND. !SEEK(STYLE+CCUT_SEW+LOCATION,lcTmpStySv)
    *REPLACE cSelRec   WITH ' ',;
            lSelRec   WITH .F.,;
            cStatus   WITH 'A',;
            cCut_Sew  WITH '',;
            Location  WITH '',;
            nPerc_Qty WITH IIF(TotQty=0,0,100)
  IF !SEEK(STYLE+CCUT_SEW+LOCATION,lcTmpStySv)
    IF cStatus = 'N'
      REPLACE cSelRec   WITH ' ',;
              lSelRec   WITH .F.,;
              cStatus   WITH 'A',;
              cCut_Sew  WITH '',;
              Location  WITH '',;
              TotQty    WITH nCutAvl,;
              TotQtyOth WITH nSewAvl,;
              nPerc_Qty WITH IIF(TOTPLAN=0,0,ROUND(TotQty/TOTPLAN*100,3)),;
              nPerc_Oth WITH IIF(TOTPLAN=0,0,ROUND(TotQtyOth/TOTPLAN*100,3))
    ELSE
      =SEEK(STYLE+CCUT_SEW+LOCATION,'LOCSTY')
      SELECT LOCSTY
      SCATTER FIELDS ccut_sew,location,totqty,nperc_qty MEMVAR
      m.cSelRec = '»'
      m.lSelRec = .T.
      m.cStatus = 'A'
      SELECT (lcTmpStyle)
      *GATHER FIELDS ccut_sew,location,totqty,nperc_qty,cSelRec,lSelRec,cStatus MEMVAR
      IF m.cCut_Sew = 'S'
        m.TotQtyOth = m.TotQty
        m.nPerc_Oth = m.nPerc_Qty
        GATHER FIELDS ccut_sew,location,totqtyoth,nperc_oth,cSelRec,lSelRec,cStatus MEMVAR
      ELSE
        GATHER FIELDS ccut_sew,location,totqty,nperc_qty,cSelRec,lSelRec,cStatus MEMVAR
      ENDIF
    ENDIF
    *B604985,1 AMH Unlock this style [Start]
    SELECT (lcTmpStyle)
    =lfUnLock(STYLE,.F.)
    *B604985,1 AMH [End]
  ENDIF
  *B604859,1 AMH [End]
ENDSCAN
SELECT (lcTmpStyle)
SET ORDER TO &lcOrder.
LOCATE
*-- end of lfRstLocat.
*B604562,1 AMH [End]

*!*************************************************************
*! Name      : lfwLocat
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/17/2001
*! Purpose   : When function of Location PopUp
*!*************************************************************
*B604562,1 AMH When function for Location PopUp
*!*************************************************************
FUNCTION lfwlocat

lcOldLocat = PADR(laVendor(lnVendor,2),8)
*-- end of lfwLocat.
*B604562,1 AMH [End]

*!*************************************************************
*! Name      : lfvOper
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/17/2001
*! Purpose   : valid function of operation popup.
*!*************************************************************
*B604562,1 AMH valid function of operation PopUp.
*!*************************************************************
*B604859,1 AMH Use another function [Start]
FUNCTION lfvOperX
*B604859,1 AMH [End]
PRIVATE lcOrder, lcOprCode

SELECT LOCSTY
lcOrder = SET('ORDER')
SET ORDER TO TAG LOCSTYLOC

lcOprCode = IIF(SUBSTR(lcOpr,1,1)='C','S','C')

SEEK laVendor(lnVendor,2) + lcOprCode
SCAN REST WHILE LOCATION + CCUT_SEW = laVendor(lnVendor,2) + lcOprCode
  SELECT (lcTmpStyle)
  SEEK LOCSTY.STYLE+LOCSTY.CCUT_SEW+LOCSTY.LOCATION
  =SEEK(LOCSTY.STYLE,'STYLE')
  *B604859,1 AMH Update the lcTmpStyle file with the correct values [Start]
  *REPLACE cSelRec   WITH ' ',;
          lSelRec   WITH .F.,;
          cStatus   WITH 'A',;
          cCut_Sew  WITH '',;
          Location  WITH '',;
          TotQty    WITH STYLE.TOTPLAN,;
          nPerc_Qty WITH IIF(TotQty=0,0,100)
  REPLACE cSelRec   WITH ' ',;
          lSelRec   WITH .F.,;
          cStatus   WITH 'A',;
          cCut_Sew  WITH '',;
          Location  WITH '',;
          TotQty    WITH EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl'),;
          nPerc_Qty WITH IIF(STYLE.TOTPLAN=0,0,ROUND(TotQty/STYLE.TOTPLAN*100,3))
  *B604859,1 AMH [End]
ENDSCAN
SELECT (lcTmpStySv)
SCAN
  IF Location = laVendor(lnVendor,2)
    IF cCut_Sew = IIF(SUBSTR(lcOpr,1,1)='C','S','C')
      SELECT (lcTmpStyle)
      SEEK EVALUATE(lcTmpStySv+'.STYLE')+EVALUATE(lcTmpStySv+'.CCUT_SEW')+EVALUATE(lcTmpStySv+'.LOCATION')
      =SEEK(LOCSTY.STYLE,'STYLE')
      *B604859,1 AMH Update the lcTmpStyle file with the correct values [Start]
      *REPLACE cSelRec   WITH ' ',;
              lSelRec   WITH .F.,;
              cStatus   WITH 'A',;
              cCut_Sew  WITH '',;
              Location  WITH '',;
              TotQty    WITH STYLE.TOTPLAN,;
              nPerc_Qty WITH IIF(TotQty=0,0,100)
      REPLACE cSelRec   WITH ' ',;
              lSelRec   WITH .F.,;
              cStatus   WITH 'A',;
              cCut_Sew  WITH '',;
              Location  WITH '',;
              TotQty    WITH EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl'),;
              nPerc_Qty WITH IIF(STYLE.TOTPLAN=0,0,ROUND(TotQty/STYLE.TOTPLAN*100,3))
      *B604859,1 AMH [End]
      SELECT (lcTmpStySv)
    ENDIF
  ENDIF
ENDSCAN
SELECT LOCSTY
SEEK laVendor(lnVendor,2) + SUBSTR(lcOpr,1,1)
SCAN REST WHILE LOCATION + CCUT_SEW = laVendor(lnVendor,2) + SUBSTR(lcOpr,1,1)
  SCATTER FIELDS ccut_sew,location,totqty,nperc_qty MEMVAR
  m.cSelRec = '»'
  m.lSelRec = .T.
  m.cStatus = 'A'
  SELECT (lcTmpStyle)
  SEEK LOCSTY.STYLE
  GATHER FIELDS ccut_sew,location,totqty,nperc_qty,cSelRec,lSelRec,cStatus MEMVAR
ENDSCAN
SET ORDER TO &lcOrder.
=lfReplace(lcTmpStyle,laVendor(lnVendor,2),SUBSTR(lcOpr,1,1))
*B604859,1 AMH Update the lcTmpStyle file with the correct values [Start]
SELECT LOCSTY
SCAN FOR LOCATION <> laVendor(lnVendor,2)
  SELECT (lcTmpStyle)
  REPLACE cSelRec   WITH ' ',;
          lSelRec   WITH .F.,;
          cStatus   WITH 'A',;
          cCut_Sew  WITH '',;
          Location  WITH '',;
          TotQty    WITH EVALUATE('n'+SUBSTR(lcOpr,1,3)+'Avl'),;
          nPerc_Qty WITH IIF(STYLE.TOTPLAN=0,0,ROUND(TotQty/STYLE.TOTPLAN*100,3))
ENDSCAN
*B604859,1 AMH [End]
SELECT (lcTmpStyle)
LOCATE
llCUpDate = .T.
*-- end of lfvOper.
*B604562,1 AMH [End]

*!*************************************************************
*! Name      : lfGetAvl
*! Developer : AMH (AHMED MAHER)
*! Date      : 08/29/2001
*! Purpose   : Get the avalible units.
*!*************************************************************
*B604859,1 AMH Get the avalible units.
*!*************************************************************
FUNCTION lfGetAvl
*B604985,1 AMH Add parameter to pass the totplan for updating [Start]
*PARAMETERS lcStyle,lcCut_Sew
PARAMETERS lcStyle,lcCut_Sew,lnTotPlan

*PRIVATE lcStyle,lcCut_Sew,lnTotQty,lnAlias
PRIVATE lcStyle,lcCut_Sew,lnTotQty,lnAlias,lnPerc_Qty
*B604985,1 AMH [End]
lnAlias = SELECT(0)
lnTotQty = 0
*B604985,1 AMH Add variable to get the avaliable percentage [Start]
lnPerc_Qty = 0
*B604985,1 AMH [End]
IF SEEK(lcStyle+lcCut_Sew,'LOCSTY')
  SELECT LOCSTY
  *B604985,1 AMH sum the percentage insted of totqty [Start]
  *SUM REST WHILE Style+cCut_Sew+Location = lcStyle+lcCut_Sew TOTQTY TO lnTotQty
  
  *B605393,1 AMH Calculate percentage correctlly [Start]
  *SUM REST WHILE Style+cCut_Sew+Location = lcStyle+lcCut_Sew NPERC_QTY TO lnPerc_Qty
  SCAN REST WHILE Style+cCut_Sew+Location = lcStyle+lcCut_Sew
    IF SEEK(lcStyle+lcCut_Sew+LOCATION,lcTmpStySv)
      IF EVALUATE(lcTmpStySv+'.CSTATUS') # 'D'
        lnPerc_Qty = lnPerc_Qty + EVALUATE(lcTmpStySv+'.NPERC_QTY')
      ENDIF
    ELSE
      lnPerc_Qty = lnPerc_Qty + NPERC_QTY
    ENDIF
  ENDSCAN
  *B605393,1 AMH [End]
  
  lnTotQty = ROUND(lnPerc_Qty*lnTotPlan/100,0)
  *B604985,1 AMH [End]
ENDIF
*B604985,1 AMH add the saved quantity [Start]
IF SEEK(lcStyle+lcCut_Sew,lcTmpStySv)
  SELECT (lcTmpStySv)
  
  *B605393,1 AMH Calculate percentage correctlly [Start]
  *SUM REST WHILE Style+cCut_Sew+Location = lcStyle+lcCut_Sew NPERC_QTY TO lnPerc_Qty
  SUM REST WHILE Style+cCut_Sew+Location = lcStyle+lcCut_Sew;
           FOR CSTATUS = 'N' NPERC_QTY TO lnPerc_Qty
  *B605393,1 AMH [End]
  
  lnTotQty = lnTotQty + ROUND(lnPerc_Qty*lnTotPlan/100,0)
ENDIF
*B604985,1 AMH [End]
SELECT (lnAlias)
RETURN lnTotQty
*--end of lfGetAvl.

*!*************************************************************
*! Name      : lfGetSvAvl
*! Developer : AMH (AHMED MAHER)
*! Date      : 08/29/2001
*! Purpose   : Get the Saved avalible units.
*!*************************************************************
*B604859,1 AMH Get the Saved avalible units.
*!*************************************************************
FUNCTION lfGetSvAvl
PARAMETERS lcAlias

PRIVATE lcAlias
lnAvl = 0
IF SEEK(EVALUATE(lcTmpStyle+'.Style')+SUBSTR(lcOpr,1,1)+laVendor(lnVendor,2),lcAlias)
  *B604985,1 AMH get the avaliable based on nperc_qty [Start]
  *lnAvl = EVALUATE(lcAlias+'.TotQty')
  lnAvl = ROUND(EVALUATE(lcAlias+'.nPerc_Qty')*EVALUATE(lcTmpStyle+'.TotPlan')/100,0)
  *B604985,1 AMH [End]
ENDIF
RETURN lnAvl
*--end of lfGetSvAvl.

*!*************************************************************
*! Name      : lfvOper
*! Developer : AMH (AHMED MAHER)
*! Date      : 09/02/2001
*! Purpose   : valid function of operation popup.
*!*************************************************************
*B604859,1 AMH valid function of operation PopUp.
*!*************************************************************
FUNCTION lfvOper

PRIVATE lcOrder
SELECT (lcTmpStyle)
lcOrder = SET('ORDER')
SET ORDER TO TAG (lcTmpStyl4)
IF SEEK(laVendor(lnVendor,2)+'Z'+SPACE(8))
  SCAN REST WHILE LOCATION+CCHNLOC+LOCOTHER = laVendor(lnVendor,2)+'Z'+SPACE(8)
    REPLACE cSelRec   WITH ' ',;
            lSelRec   WITH .F.,;
            cStatus   WITH 'A',;
            cCut_Sew  WITH '',;
            LocOther  WITH Location,;
            cChnLoc   WITH 'O'
  ENDSCAN
ENDIF
IF SEEK(laVendor(lnVendor,2)+'Z'+laVendor(lnVendor,2))
  SCAN REST WHILE LOCATION+CCHNLOC+LOCOTHER = laVendor(lnVendor,2)+'Z'+laVendor(lnVendor,2)
    IF SEEK(STYLE+SUBSTR(lcOpr,1,1)+LocOther,lcTmpStySv)
      lcStatus = EVALUATE(lcTmpStySv+'.CSTATUS')
    ELSE
      lcStatus = 'A'
    ENDIF
    REPLACE cStatus   WITH lcStatus,;
            cCut_Sew  WITH SUBSTR(lcOpr,1,1)
  ENDSCAN
ENDIF
SET ORDER TO TAG (lcTmpStyl5)
IF SEEK(laVendor(lnVendor,2)+'Z'+SPACE(8))
  SCAN REST WHILE LOCOTHER+CCHNLOC+LOCATION = laVendor(lnVendor,2)+'Z'+SPACE(8)
    IF SEEK(STYLE+SUBSTR(lcOpr,1,1)+LocOther,lcTmpStySv)
      lcStatus = EVALUATE(lcTmpStySv+'.CSTATUS')
    ELSE
      lcStatus = 'A'
    ENDIF
    REPLACE cSelRec   WITH '»',;
            lSelRec   WITH .T.,;
            cStatus   WITH lcStatus,;
            cCut_Sew  WITH SUBSTR(lcOpr,1,1),;
            Location  WITH LocOther,;
            cChnLoc   WITH 'L'
    IF SEEK(laVendor(lnVendor,2)+'Z'+SPACE(8))
      SKIP -1
    ELSE
      EXIT
    ENDIF
  ENDSCAN
ENDIF
IF SEEK(laVendor(lnVendor,2)+'L')
  SCAN REST WHILE LOCOTHER+CCHNLOC+LOCATION = laVendor(lnVendor,2)+'L'
    REPLACE LocOther  WITH '',;
            cChnLoc   WITH 'Z'
  ENDSCAN
ENDIF
SET ORDER TO TAG (lcTmpStyl4)
IF SEEK(laVendor(lnVendor,2)+'O')
  SCAN REST WHILE LOCATION+CCHNLOC+LOCOTHER = laVendor(lnVendor,2)+'O'
    REPLACE Location  WITH '',;
            cChnLoc   WITH 'Z'
  ENDSCAN
ENDIF
SET ORDER TO &lcOrder.
LOCATE
=lfDispBrow()
*-- end of lfvOper.

*B604985,1 AMH [Start]
*!*************************************************************
*! Name      : lfChckLock
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/01/2001
*! Purpose   : To check locking of style
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*!
FUNCTION lfChckLock
PARAMETERS llMsg

PRIVATE lnAlias, lnSetting
lnSetting = SET('REPROCESS')
SET REPROCESS TO 1 SECONDS
llReturn = .T.
lnAlias = SELECT(0)
SELECT TMPLOCST
IF SEEK(EVALUATE(lcTmpStyle+'.STYLE'))
  IF !RLOCK()
    IF llMsg
      =gfModalGen("TRM00000B00000","","",.F.,'Style : '+ALLTRIM(STYLE)+' is used by : '+;
                                             ALLTRIM(CADD_USER)+', Cannot proceed')
    ENDIF
    llReturn = .F.
  ELSE
    =gfAdd_info('TMPLOCST')
  ENDIF
ELSE
  APPEND BLANK
  REPLACE STYLE WITH EVALUATE(lcTmpStyle+'.STYLE')
  =gfAdd_info('TMPLOCST')
  =RLOCK()
ENDIF
SELECT (lnAlias)
SET REPROCESS TO lnSetting
RETURN llReturn
*--end of lfChckLock.

*!*************************************************************
*! Name      : lfUnLock
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/01/2001
*! Purpose   : To Unlocking of style
*!*************************************************************
*! Passed Parameters  : lcStyle = style to unlock
*!*************************************************************
*!
FUNCTION lfUnLock
PARAMETERS lcStyle,llLocSty

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT TMPLOCST
IF SEEK(lcStyle) .AND. !SEEK(lcStyle,lcTmpStySv) .AND. IIF(llLocSty,!SEEK(lcStyle,'LOCSTY'),.T.)
  UNLOCK
ENDIF
SELECT (lnAlias)
*--end of lfUnLock.

*!*************************************************************
*! Name      : lfDelLock
*! Developer : AMH (AHMED MAHER)
*! Date      : 10/01/2001
*! Purpose   : To delete the lock record from TMPLOCST file.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*!
FUNCTION lfDelLock

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT TMPLOCST
IF SEEK(EVALUATE(lcTmpStySv+'.STYLE'))
  DELETE
ENDIF
SELECT (lnAlias)
*--end of lfDelLock.
*B604985,1 AMH [End]

*!*************************************************************
*! Name      : lfSetSty
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/15/2001
*! Purpose   : To set the Style file in the browse screen
*!*************************************************************
*! Example     : = lfSetSty()
*!*************************************************************
*B605028,1 AMH [Start]
FUNCTION lfSetSty
PARAMETERS OpGrdParm

IF OpGrdParm = 'S'
  GO TOP IN STYLE
  lcSelStyle = laOgFxFlt[1,6]
ENDIF
*B605028,1 AMH [End]

*!*************************************************************
*! Name      : lfvSeason
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/15/2001
*! Purpose   : validate the season option in OG
*!*************************************************************
*! Example     : = lfvSeason()
*!*************************************************************
*B605028,1 AMH [Start]
FUNCTION lfvSeason

= gfMover(@laRpSorSea,@laRpTarSea,'Select Season',.T.,'')  && call mover function.
*-- end of lfvSeason.
*B605028,1 AMH [End]

*!*************************************************************
*! Name      : lfvScope
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/15/2001
*! Purpose   : validate the scope botton
*!*************************************************************
*! Example     : = lfvScope()
*!*************************************************************
*B605028,1 AMH [Start]
FUNCTION lfvScope

lcSelStyle = SPACE(0)
DECLARE laRpSorSea[1] , laRpTarSea[1]
STORE SPACE(39) TO laRpSorSea , laRpTarSea
SELECT CCODE_NO + ' - ' + CDISCREP AS SEASON;
  FROM CODES;
  WHERE CFLD_NAME = 'SEASON' AND CRLTFIELD = 'N';
  INTO ARRAY laRpSorSea
PUSH KEY CLEAR  
llExpr = gfOpGrid('MFLOCST' , .T.)  && Run selection grid.

*B605133,1 AMH Change the order of style file to collect all colors [Start]
SET ORDER TO TAG STYLE IN STYLE
*B605133,1 AMH [End]

POP KEY
glQuitting = .T.  && Rise quit flag because it's modal screen.
IF TYPE('llExpr') <> 'L'
  SELECT (lcTmpStyle)
  ZAP
  =lfGetLocat()
ENDIF
lnVendor = 1
=lfDispBrow()
=lfvpbSel()
=lfRefresh()
SHOW GET lnCarLb
*-- end of lfvScope.
*B605028,1 AMH [End]

*B605393,1 AMH [Start]
*!*************************************************************
*! Name      : lfLocUnTrp
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/17/2002
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*
FUNCTION lfLocUnTrp

*-- IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcLocBrow) .AND. glFromBrow
  = gfStopBrow()
  glFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B DO lfDispLoc
ENDIF    && End of IF
*-- end of lfLocUnTrp.

*!*************************************************************
*! Name      : lfLocTrap
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/17/2002
*! Purpose   : Function to trap the keys for the Browse , 
*!             and save the changes if the current record was edited
*!*************************************************************
*
FUNCTION lfLocTrap
*-- IF The window on top is the Browse
IF WONTOP(lcLocBrow)
  glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfLocTraps WITH "TAB"
  ON KEY LABEL BACKTAB DO lfLocTraps WITH "BACKTAB"
  ON KEY LABEL ALT+B 
ENDIF    && End of IF
*-- end of lfLocTrap.

*!*************************************************************
*! Name      : lfLocTraps
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/17/2002
*! Purpose   : Trap (Tab / Backtab) Keys
*!*************************************************************
*
FUNCTION lfLocTraps
PARAMETERS lcTrap
*-- Tab Case
IF lcTrap = "TAB"
  ACTI WINDOW MFSTYLC2 TOP
  _CUROBJ = OBJNUM(lnVendor)

ELSE  && Backtab Case.
  ACTI WINDOW MFSTYLC2 TOP
  _CUROBJ = OBJNUM(pbCan)
ENDIF
*-- end of lfLocTraps.

*!*************************************************************
*! Name      : lfDispLoc
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/17/2002
*! Purpose   : Function to create the Browse
*!*************************************************************
*
FUNCTION lfDispLoc

SELECT (lcTmpStyLc)
lnBrRecNo  = RECNO()

lcBrowFild = "lcMarker=IIF(RECNO()=lnBrRecNo , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "lcLocation=lfLocDesc(Location) :H = 'Location'       :30:R,"+;
             "TotQty   :H = 'Units' :20:R,"+;
             "nPerc_Qty:H = 'Percentage' :20:R"
BROWSE FIELDS &lcBrowFild     ;
       WINDOW MFSTYLC1        ;
       WHEN lfwLocBrow()      ;
       IN WINDOW MFSTYLOC     ;
       LOCK 0                 ;
       NOAPPEND               ;
       NOCLEAR                ;
       NODELETE               ;
       NOWAIT                 ;
       NOEDIT                 ;
       NOMENU                 ;
       SAVE                   ;
       FOR CSTATUS#'D'        ;
       TITLE lcLocBrow
*-- end of lfDispLoc.

*!*************************************************************
*! Name      : lfwLocBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/20/2002
*! Purpose   : When Browse Function.
*!*************************************************************
*
FUNCTION lfwLocBrow

PRIVATE lcStatus
lnBrRecNo  = RECNO(lcTmpStyLc)
lnLocVend  = ASUBSCRIPT(laVendor,ASCAN(laVendor,EVALUATE(lcTmpStyLc+'.Location')),1)
lnLocUnit  = EVALUATE(lcTmpStyLc+'.totqty')
lnLocPer   = EVALUATE(lcTmpStyLc+'.nperc_qty')
lcStatus = IIF(EVALUATE(lcTmpStyLc+'.cStatus')='N','ENABLE','DISABLE')
SHOW GET lnLocVend &lcStatus.
SHOW GET lnLocUnit ENABLE
SHOW GET lnLocPer  ENABLE
SHOW WINDOW (lcLocBrow) REFRESH
*-- end of lfwLocBrow.

*!*************************************************************
*! Name      : lfLocDesc
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/20/2002
*! Purpose   : Function To Get the location description.
*!*************************************************************
*
FUNCTION lfLocDesc
PARAMETERS lcLocation

lnLocVend = ASUBSCRIPT(laVendor,ASCAN(laVendor,lcLocation),1)
RETURN laVendor[lnLocVend,1]
*-- end of lfLocDesc.

*!*************************************************************
*! Name      : lfGetStyLc
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/20/2002
*! Purpose   : Function to collecting data to lcTmpStyLc file.
*!*************************************************************
*
FUNCTION lfGetStyLc

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpStyLc)
ZAP
IF SEEK(lcStyle+SUBSTR(lcOpr,1,1),'LOCSTY')
  SELECT LOCSTY
  SCAN REST WHILE style+ccut_sew+location = lcStyle+SUBSTR(lcOpr,1,1);
            FOR LOCATION # laVendor[lnVendor,2]
    SCATTER MEMVAR
    SELECT (lcTmpStyLc)
    APPEND BLANK
    GATHER MEMVAR
    REPLACE TOTPLAN WITH EVALUATE(lcTmpStyle+'.TOTPLAN'),;
            CSTATUS WITH 'A'
  ENDSCAN
ENDIF
IF SEEK(lcStyle+SUBSTR(lcOpr,1,1),lcTmpStySv)
  SELECT (lcTmpStySv)
  SCAN REST WHILE style+ccut_sew+location = lcStyle+SUBSTR(lcOpr,1,1);
            FOR LOCATION # laVendor[lnVendor,2]
    SCATTER MEMVAR
    IF !SEEK(EVALUATE(KEY()),lcTmpStyLc)
      SELECT (lcTmpStyLc)
      APPEND BLANK
    ENDIF
    SELECT (lcTmpStyLc)
    GATHER MEMVAR
    REPLACE TOTPLAN WITH EVALUATE(lcTmpStyle+'.TOTPLAN')
    IF CSTATUS = 'D'
      DELETE
    ENDIF
  ENDSCAN
ENDIF
IF SEEK(lcStyle+SUBSTR(lcOpr,1,1),lcTmpStyLS)
  SELECT (lcTmpStyLS)
  SCAN REST WHILE style+ccut_sew+location = lcStyle+SUBSTR(lcOpr,1,1);
            FOR LOCATION # laVendor[lnVendor,2]
    SCATTER MEMVAR
    IF !SEEK(EVALUATE(KEY()),lcTmpStyLc)
      SELECT (lcTmpStyLc)
      APPEND BLANK
    ENDIF
    SELECT (lcTmpStyLc)
    GATHER MEMVAR
    REPLACE TOTPLAN WITH EVALUATE(lcTmpStyle+'.TOTPLAN')
    IF CSTATUS = 'D'
      DELETE
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcTmpStyLc)
PACK
LOCATE
SELECT (lnAlias)
*-- end of lfGetStyLc.

*!*************************************************************
*! Name      : lfGetStyLS
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/20/2002
*! Purpose   : Function to collecting data to lcTmpStyLS file.
*!*************************************************************
*
FUNCTION lfGetStyLS

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpStyLc)
SCAN
  SCATTER MEMVAR
  IF !SEEK(EVALUATE(KEY()),lcTmpStyLS)
    SELECT (lcTmpStyLS)
    APPEND BLANK
  ENDIF
  SELECT (lcTmpStyLS)
  GATHER MEMVAR
ENDSCAN
SELECT (lnAlias)
*-- end of lfGetStyLS.

*!*************************************************************
*! Name      : lfvLocUnit
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/20/2002
*! Purpose   : Valid function of Unite field.
*!*************************************************************
*!
FUNCTION lfvLocUnit

IF lnLocUnit < 0
  =gfModalGen("TRM00000B00000","","",.F.,'Unit Cannot be less than zero.')
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

lnLocPer = ROUND(IIF(&lcTmpStyLc..TotPlan=0,0,lnLocUnit/&lcTmpStyLc..TotPlan*100),3)
REPLACE &lcTmpStyLc..TotQty WITH lnLocUnit,;
        &lcTmpStyLc..nPerc_Qty WITH lnLocPer,;
        cStatus WITH IIF(cStatus='A','M',cStatus)
SHOW GET lnLocPer
SHOW WINDOW (lcLocBrow) REFRESH SAME
*-- end of lfvLocUnit.

*!*************************************************************
*! Name      : lfvLocPer
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/20/2002
*! Purpose   : Valid function of Percentage field.
*!*************************************************************
*!
FUNCTION lfvLocPer

IF lnLocPer < 0
  =gfModalGen("TRM00000B00000","","",.F.,'Percentage Cannot be less than zero.')
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

lnLocUnit = ROUND(lnLocPer*&lcTmpStyLc..TotPlan/100,0)
REPLACE &lcTmpStyLc..TotQty WITH lnLocUnit,;
        &lcTmpStyLc..nPerc_Qty WITH lnLocPer,;
        cStatus WITH IIF(cStatus='A','M',cStatus)
SHOW GET lnLocUnit
SHOW WINDOW (lcLocBrow) REFRESH SAME
*-- end of lfvLocPer.

*!*************************************************************
*! Name      : lfvLocVend
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/20/2002
*! Purpose   : Valid function of Location PopUp.
*!*************************************************************
*!
FUNCTION lfvLocVend

PRIVATE lnAlias,lnRecNo
IF IIF(EMPTY(laVendor[1,2]),lnLocVend = lnVendor,lnLocVend = lnVendor -1)
  =gfModalGen("TRM00000B00000","","",.F.,'Cannot select location '+ALLTRIM(laVendor[lnLocVend,1]))
  lnLocVend = lnOldLocVn
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
lnAlias = SELECT(0)
SELECT (lcTmpStyLc)
lnRecNo = RECNO()
LOCATE FOR LOCATION = laVendor[lnLocVend,2]
IF FOUND()
  =gfModalGen("TRM00000B00000","","",.F.,'Cannot select location '+ALLTRIM(laVendor[lnLocVend,1]))
  GOTO lnRecNo
  SELECT (lnAlias)
  lnLocVend = lnOldLocVn
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
GOTO lnRecNo
IF EMPTY(laVendor[1,2]) .AND. lnLocVend # 1
  =ADEL(laVendor,1)
  DIMENSION laVendor[ALEN(laVendor,1)-1,2]
  APPEND BLANK
  REPLACE STYLE     WITH lcStyle,;
          TOTQTY    WITH 0,;
          NPERC_QTY WITH 0,;
          CSTATUS   WITH 'N',;
          CCUT_SEW  WITH SUBSTR(lcOpr,1,1),;
          LOCATION  WITH laVendor[lnLocVend-1,2],;
          TOTPLAN   WITH EVALUATE(lcTmpStyle+'.TotPlan')
ELSE
  IF !EMPTY(laVendor[1,2])
    REPLACE LOCATION WITH laVendor[lnLocVend,2]
  ENDIF
ENDIF
=lfwLocBrow()
SELECT (lnAlias)
*-- end of lfvLocVend.

*!*************************************************************
*! Name      : lfvAddLoc
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/21/2002
*! Purpose   : Valid function of Add location.
*!*************************************************************
*!
FUNCTION lfvAddLoc

IF !EMPTY(laVendor[1,2])
  DIMENSION laVendor[ALEN(laVendor,1)+1,2]
  =AINS(laVendor,1)
  laVendor[1,1] = 'Select Location'
  laVendor[1,2] = SPACE(8)
ENDIF
SHOW GET lnLocUnit DISABLE
SHOW GET lnLocPer  DISABLE
lnLocVend = 1
SHOW GET lnLocVend ENABLE
_CUROBJ = OBJNUM(lnLocVend)
*-- end of lfvAddLoc.

*!*************************************************************
*! Name      : lfvRemLoc
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/21/2002
*! Purpose   : Valid function of Remove location.
*!*************************************************************
*!
FUNCTION lfvRemLoc

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpStyLc)
REPLACE CSTATUS   WITH 'D',;
        TOTQTY    WITH 0,;
        NPERC_QTY WITH 0
LOCATE
=lfwLocBrow()
SELECT (lnAlias)
*-- end of lfvRemLoc.

*!*************************************************************
*! Name      : lfwLocVend
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/21/2002
*! Purpose   : When function of Location PopUp
*!*************************************************************
FUNCTION lfwlocVend

lnOldLocVn = lnLocVend
*-- end of lfwLocVend.

*!*************************************************************
*! Name      : lfvLocCan
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/21/2002
*! Purpose   : To validate the Cancel button.
*!*************************************************************
*!
FUNCTION lfvLocCan

SELECT (lcTmpStyle)
CLEAR READ
*-- end of lfvLocCan.

*!*************************************************************
*! Name      : lfvLocOk
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/21/2002
*! Purpose   : To validate the OK button.
*!*************************************************************
*!
FUNCTION lfvLocOk

PRIVATE lnAlias,lnUsed
lnAlias = SELECT(0)
SELECT (lcTmpStyLc)
SUM NPERC_QTY TO lnUsed FOR CSTATUS # 'D'
LOCATE
lnUsed = lnUsed + lnPer
IF lnUsed > 100
  =gfModalGen("TRM00000B00000","","",.F.,'Cannot assign more than 100%.')
  RETURN
ENDIF
=lfGetStyLS()
SELECT (lcTmpStyle)
lnAvl = ROUND(lnUsed*TOTPLAN/100,0)
REPLACE ('n'+SUBSTR(lcOpr,1,3)+'Avl') WITH TOTPLAN - ROUND((lnUsed-lnPer)*TOTPLAN/100,0)
CLEAR READ
*-- end of lfvLocOk.

*!*************************************************************
*! Name      : lfGetLocAv
*! Developer : AMH (AHMED MAHER)
*! Date      : 01/21/2002
*! Purpose   : Get the available from lcTmpStyLS.
*!*************************************************************
*!
FUNCTION lfGetLocAv

PRIVATE lcLocSty, lcLocOpr, lnAlias, lnPerc_Qty, lnTotQty
lnAlias = SELECT(0)
SELECT (lcTmpStyLS)
lcLocSty = STYLE
lcLocOpr = CCUT_SEW
STORE 0 TO lnPerc_Qty, lnTotQty
SUM REST WHILE STYLE+CCUT_SEW+LOCATION = lcLocSty+lcLocOpr NPERC_QTY TO lnPerc_Qty
lnTotQty = ROUND(lnPerc_Qty*EVALUATE(lcTmpStyle+'.TotPlan')/100,0)
SELECT (lnAlias)
RETURN lnTotQty
*-- end of lfGetLocAv.
*B605393,1 AMH [End]