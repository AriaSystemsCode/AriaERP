*:***************************************************************************
*: Program file  : MFHNDCT
*: Program desc. : Assign marker sections information for a cutting ticket
*: For screen    : MFHNDCT.SCX,MFHNDCT1.SCX,MFHNDCT2.SCX
*:        System : Aria Advantage Series.
*:        Module : Manufactering Module (MF)
*:        Date   : 02/17/2002
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO MFHNDCT
*:***************************************************************************
*: Due to C200293,1
*:***************************************************************************
*: Modifications :
*:***************************************************************************
* B606481,1 AMH 09/15/2002 Fix bug of not get the correct marker data.
*:***************************************************************************
*

EXTERNAL ARRAY laDefProc,laData,laKeyField

laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
DECLARE laKeyField [2,4]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'CUTSECHD'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'CUTSECHD'
laKeyField[2,4] = 2

llNoShow   = .F.           && Flag to make the screen call the PROCEDURE lpShow evry time it run
glFromBrow = .T.
lcTmpSecLn = gfTempName()  && File to hold CutSecLn records and to brows from it.
lcTmpMark  = gfTempName()  && File to hold PdmMark records and to brows from it.
lcSecBrow  = 'Marker Sections'
llBrowse   = .F.
lcFabric   = ''

IF !gfSetup()
  RETURN
ENDIF
lcBaseFile = 'CUTSECHD'
DECLARE laField_N[2]
laField_N[1] = 'CUTTKT'
laField_N[2] = 'CMARKER'
DECLARE laField_H[2]
laField_H[1] = 'cutting ticket #'
laField_H[2] = 'marker'

IF !WEXIST(gcBaseWind)
  *-- Screen fields variable, to be used to build the laData array.
  
  *B606481,1 AMH Get the correct replen [Start]
  *lcScFields = "Cuttkt,CMarker,cFabricDes,cRepLenFra"
  lcScFields = "Cuttkt,CMarker,cFabricDes,nRepLenDec"
  *B606481,1 AMH [End]
  
  lcBrFields = lcScFields
  SCATTER FIELDS &lcScFields. TO laData BLANK
  =lfCreatTmp()
ENDIF

DO (gcScrDir+gcWinAppl+"\MFHNDCT.SPX")
ON KEY LABEL ALT+B 

*!**************************************************************************
*! Name      : lpShow
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/17/2002
*! Purpose   : procedure to refresh screen objects.
*!**************************************************************************
*
PROCEDURE lpShow
 
EXTERNAL ARRAY laScrMode

DO CASE
  CASE laScrMode[1]                   && SELECT Mode
    IF WEXIST(lcSecBrow)
      =lfClearTmp()
    ENDIF
  CASE laScrMode[2]                   && VIEW Mode
    =lfGetInfo()
    =lfwBrows()
  CASE laScrMode[3]                   && EDIT Mode
    =lfwBrows()
  CASE laScrMode[4]                   && ADD  Mode
    =lfGetNew()
    =lfwBrows()
    ACTIVATE WINDOW MFHNDCT1 TOP
    _CUROBJ = OBJNUM(laData[1])
ENDCASE
=lfDispBrow()

*-- end of lpShow.

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/17/2002
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*
FUNCTION lfBrwUnTrp

*-- IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcSecBrow) .AND. glFromBrow
  = gfStopBrow()
  glFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B DO lfDispBrow
ENDIF    && End of IF
*-- end of lfBrwUnTrp.

*!*************************************************************
*! Name      : lfTraps
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/17/2002
*! Purpose   : Trap (Tab / Backtab) Keys
*!*************************************************************
*
FUNCTION lfTraps
PARAMETERS lcTrap
*-- Tab Case
ACTI WINDOW MFHNDCT1 TOP
IF lcTrap = "TAB"
  _CUROBJ = OBJNUM(laData[1])
ELSE  && Backtab Case.
  _CUROBJ = OBJNUM(laData[2])
ENDIF
*-- end of lfTraps.

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/17/2002
*! Purpose   : Function to trap the keys for the Browse , 
*!             and save the changes if the current record was edited
*!*************************************************************
*
FUNCTION lfBrowTrap
*-- IF The window on top is the Browse
IF WONTOP(lcSecBrow)
  glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH "TAB"
  ON KEY LABEL BACKTAB DO lfTraps WITH "BACKTAB"
  ON KEY LABEL ALT+B 

ENDIF    && End of IF
*-- end of lfBrowTrap.

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/17/2002
*! Purpose   : Function to create the Browse
*!*************************************************************
*
FUNCTION lfDispBrow

SELECT (lcTmpSecLn)
lcBrowFild = "Iclr                           :H = 'Color'          :9  :R,"+;
             "cSectNo                        :H = 'Sec. #'         :6  :R,"+;
             "lcPattPics=MLINE(mPattPiece,1) :H = 'Pattern Pieces' :34 :R,"+;
             "nTot_Len                       :H = 'Total Length'   :12 :R,"+;
             "nNum_Rep                       :H = 'Repeats'        :10 :R,"+;
             "nPly                           :H = 'Ply'            :6  "+;
             IIF(laScrMode[3] .OR. laScrMode[4],"",":R")
BROWSE FIELDS &lcBrowFild     ;
       WINDOW MFHNDCT2        ;
       WHEN lfwBrows()        ;
       IN WINDOW (gcBaseWind) ;
       LOCK 0                 ;
       NOAPPEND               ;
       NOCLEAR                ;
       NODELETE               ;
       NOWAIT                 ;
       NOMENU                 ;
       SAVE                   ;
       TITLE lcSecBrow
*-- end of lfDispBrow.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/17/2002
*! Purpose   : When Browse Function.
*!*************************************************************
*
FUNCTION lfwBrows

SHOW WINDOW (lcSecBrow) REFRESH SAME
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/17/2002
*! Purpose   : Function to create tmp file of CUTSECLN.
*!*************************************************************
*
FUNCTION lfCreatTmp

SELECT CUTSECLN
=AFIELDS(laFStru)
CREATE CURSOR (lcTmpSecLn) FROM ARRAY laFStru
INDEX ON CUTTKT+CMARKER+ICLR+CSECTNO TAG (lcTmpSecLn) OF (lcTmpSecLn)
SELECT PDMMARK
=AFIELDS(laFStru)
CREATE CURSOR (lcTmpMark) FROM ARRAY laFStru
INDEX ON CMARK_ID TAG (lcTmpMark) OF (lcTmpMark)
SELECT (lcTmpSecLn)
*-- end of lfCreatTmp.

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/17/2002
*! Purpose   : Function to collect data from CUTSECLN to tmp file.
*!*************************************************************
*
FUNCTION lfGetInfo

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpSecLn)
ZAP
IF SEEK(laData[1]+laData[2],'CUTSECLN')
  SELECT CUTSECLN
  SCAN REST WHILE CUTTKT+CMARKER+ICLR+CSECTNO = laData[1]+laData[2]
    SCATTER MEMVAR MEMO
    SELECT (lcTmpSecLn)
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
ENDIF
SELECT (lcTmpSecLn)
LOCATE
=lfDispBrow()
SELECT (lnAlias)
*-- end of lfGetInfo.

*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/17/2002
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
*! Name      : lpSavScr
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/17/2002
*! Purpose   : To Save record in CutSecHd file & related records in CutSecLn file.
*!*************************************************************
*!
PROCEDURE lpSavScr

PRIVATE lnAlias, lcDelSet
lnAlias = SELECT(0)

*-- Updating the header file.
SELECT CUTSECHD
IF laScrMode[4]
  APPEND BLANK
ENDIF
GATHER FIELDS &lcScFields. FROM laData
*=lfUpdUsrFl('CCRPTMPCOD',laData[2])

*-- Updating the lines file.
SELECT (lcTmpSecLn)
SEEK laData[1]+laData[2]
SCAN REST WHILE CUTTKT+CMARKER+ICLR+CSECTNO = laData[1]+laData[2]
  SCATTER MEMVAR MEMO
  SELECT CUTSECLN
  IF !SEEK(m.Cuttkt+m.cMarker+m.iClr+m.cSectNo)
    APPEND BLANK
  ENDIF
  GATHER MEMVAR MEMO
ENDSCAN
SELECT (lnAlias)
*--end of lpSavScr.

*!**************************************************************************
*! Name      : lfUpdUsrFl
*! Developer : Ahmed Maher (AMH)
*! Date      : 02/17/2002
*! Purpose   : Updating User defined fields.
*!**************************************************************************
*! Example   : =lfUpdUsrFl()
*!**************************************************************************
*
FUNCTION lfUpdUsrFl
PARAMETERS lcFldName,lcFldValue

lnFldPos = ASUBSCRIPT(laUsrFields,ASCAN(laUsrFields,lcFldName),1)
laUsrFields[lnFldPos,6] = lcFldValue
*-- end of lfUpdUsrFl.

*!*************************************************************
*! Name      : lpDelScr
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/17/2002
*! Purpose   : To Delete record in CutSecHd file and related records in CutSecLn file.
*!*************************************************************
*!
PROCEDURE lpDelScr

PRIVATE lnAlias
lnAlias = SELECT(0)

*-- Delete this record from the header file.
IF SEEK(laData[1]+laData[2],'CUTSECHD')
  SELECT CUTSECHD
  =gfObj_lock(.F.)
  DELETE
  laScrMode    = .F.
  laScrMode[1] = .T.
ENDIF

*-- Delete all lines.
SELECT (lcTmpSecLn)
IF SEEK(laData[1]+laData[2],'CUTSECLN')
  SELECT CUTSECLN
  DELETE REST WHILE CUTTKT+CMARKER+ICLR+CSECTNO = laData[1]+laData[2]
ENDIF
SELECT (lnAlias)
*--end of lpDelScr.

*!*************************************************************
*! Name      : lfGetNew
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/17/2002
*! Purpose   : Function to collect new data from ??? to tmp file.
*!*************************************************************
*
FUNCTION lfGetNew

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpSecLn)
ZAP
IF SEEK(laData[2],'PDMMRKSH')
  SELECT PDMMRKSH
  SCAN REST WHILE CKEY_FLD+CSECTNO = laData[2]
    SCATTER MEMVAR MEMO
    m.Cuttkt     = laData[1]
    m.cMarker    = m.cKey_Fld
    m.nPly       = 0
    m.mPattPiece = lfGetPiece()
    IF SEEK('M'+laData[1]+lcFabric,'CTKTBOM')
      SELECT CTKTBOM
      SCAN REST WHILE cImTyp+CutTkt+Item+iClr+MfgCode+Dyelot = 'M'+laData[1]+lcFabric;
                FOR CMARKER = laData[2]
        SCATTER FIELDS ICLR MEMVAR
        *m.nPly = lfGetPly()
        SELECT (lcTmpSecLn)
        APPEND BLANK
        GATHER MEMVAR MEMO
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcTmpSecLn)
LOCATE
=lfDispBrow()
SELECT (lnAlias)
*-- end of lfGetNew.

*!*************************************************************
*! Name      : lfGetPly
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/17/2002
*! Purpose   : Function to calculate the ply field.
*!*************************************************************
*
FUNCTION lfGetPly

*-- end of lfGetPly.

*:----------------------------------------------------------------
*: Name       : lfvCutTkt
*: Developer  : AHMED MAHER (AMH)
*: Date       : 02/18/2002
*: Purpose    : Cutting Ticket object valid function.
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None.
*:----------------------------------------------------------------
*: Example    : = lfvCutTkt()
*:----------------------------------------------------------------
FUNCTION lfvCutTkt
PRIVATE lnAlias, lcOldBrwFl

lnAlias = SELECT(0)
lcOldBrwFl = lcBrFields
SELECT CutTktH
IF llBrowse OR (!EMPTY(laData[1]) AND (!SEEK(laData[1],"CutTktH") OR !(STATUS$'OA')))
  llBrowse  = .F.
  lcCutTkt  = laData[1]
  llNoThing = CutBrow(@lcCutTkt,'','STATUS$"OA"')
  IF EMPTY(lcCutTkt)
    laData[1] = SPACE(1)
    _CUROBJ = _CUROBJ
    RETURN
  ELSE
    laData[1] = lcCutTkt
  ENDIF
ENDIF
=lfGetMark()
IF !EMPTY(laData[1]) .AND. !EMPTY(laData[2]) .AND. !SEEK(laData[2],lcTmpMark)
  =gfModalGen("INM000000B00000","DIALOG",'','',"Marker "+ALLTRIM(laData[2])+;
              " have not been defined for C/T "+laData[1]+".")
  laData[1] = SPACE(6)
  _CUROBJ = _CUROBJ
ENDIF
lcBrFields = lcOldBrwFl
llNoThing = IIF(EMPTY(laData[1]) .OR. EMPTY(laData[2]), .T., gfSeekRec())
SELECT(lnAlias)
*-- end of lfvCutTkt.

*!*************************************************************
*! Name      : lfvMarker
*! Developer : Ahmed Maher (AMH)
*! Date      : 02/18/2002
*! Purpose   : Valid function of Marker field.
*!*************************************************************
*! Example   : = lfvMarker()
*!*************************************************************
*
FUNCTION lfvMarker

PRIVATE lnAlias, lcFile_Ttl, lcOldBrwFl, lcAlias
IF !EMPTY(laData[1]) .AND. !EMPTY(laData[2]) .AND. RECCOUNT(lcTmpMark)=0
  =gfModalGen("INM000000B00000","DIALOG",'','',"No markers have been defined for this C/T.")
  laData[2] = SPACE(10)
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
DIMENSION laTempData[1]
STORE '' TO laTempData

lnAlias = SELECT(0)

*B606481,1 AMH Get the style pattern [Start]
lcPattern = SPACE(12)
IF !EMPTY(laData[1]) .AND. SEEK(laData[1],'CUTTKTH') .AND. SEEK(CUTTKTH.STYLE,'STYLE')
  lcPattern = PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),10)+;
              RIGHT(ALLTRIM(STYLE.PATTERN),2)
ENDIF
*B606481,1 AMH [End]

lcAlias = IIF(EMPTY(laData[1]),'PDMMARK',lcTmpMark)
SELECT (lcAlias)

lcOldBrwFl = lcBrFields

lcFile_Ttl = 'Markers'

lcBrFields = "CMARK_ID                    :H='Marker'     ,"+;
             "CMARK_DESC                  :H='Description',"+;
             "CPATTERN = ALLTRIM(CPATT_NO)+ALLTRIM(CREV_NO) :H='Pattern# ' ,"+;
             "NTOTQTY                     :H='Gross Unit'"

*B606481,1 AMH Check for the style pattern [Start]
llPatrnChk = .F.
IF EMPTY(laData[2])
  llPatrnChk = .T.
ELSE
  IF SEEK(laData[2],lcAlias)
    LOCATE REST WHILE CMARK_ID = laData[2] FOR CPATT_NO+CREV_NO = lcPattern
    IF FOUND()
      llPatrnChk = .T.
    ENDIF
  ENDIF
ENDIF
*IF llBrowse OR (!EMPTY(laData[2]) AND !SEEK(laData[2],lcAlias))
IF llBrowse .OR. !llPatrnChk
*B606481,1 AMH [End]

  llBrowse  = .F.
  =AriaBrow([],lcFile_Ttl,gnBrFSRow1,gnBrFSCol1,gnBrFSRow2,gnBrFSCol2,.F.,.F.,'CMARK_ID','laTempData')
  laData[2] = IIF(EMPTY(laTempData[1]),SPACE(10),laTempData[1])
ENDIF
lcBrFields = lcOldBrwFl
IF !EMPTY(laData[2])
  lcFabric  = EVALUATE(lcAlias+'.CFABRIC')
  laData[3] = EVALUATE(lcAlias+'.CFABRICDES')
  
  *B606481,1 AMH Get the correct replen [Start]
  *laData[4] = EVALUATE(lcAlias+'.CREPLENFRA')
  laData[4] = EVALUATE(lcAlias+'.NREPLENDEC')
  *B606481,1 AMH [End]
  
ENDIF
llNoThing = IIF(EMPTY(laData[1]) .OR. EMPTY(laData[2]), .T., gfSeekRec())
SELECT (lnAlias)
*-- end of lfvMarker.

*!*************************************************************
*! Name      : lfGetMark
*! Developer : Ahmed Maher (AMH)
*! Date      : 02/19/2002
*! Purpose   : Get the markers assigned to the C/T.
*!*************************************************************
*! Example   : = lfGetMark()
*!*************************************************************
*
FUNCTION lfGetMark

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpMark)
ZAP
IF !EMPTY(laData[1]) .AND. SEEK('M'+laData[1],'CTKTBOM')
  SELECT CTKTBOM
  SCAN REST WHILE cImTyp+Cuttkt+Typ+Item+Iclr+MfgCode+Dyelot = 'M'+laData[1];
            FOR CCATGTYP = 'F' .AND. !EMPTY(CMARKER)
    
    *B606481,1 AMH Get marker records for the patter of the cutting ticket style 
    *B606481,1     otherwise get all marker records [Start]
    *IF SEEK(CMARKER,'PDMMARK') .AND. !SEEK(CMARKER,lcTmpMark)
    *  SELECT PDMMARK
    *  SCATTER MEMO MEMVAR
    *  INSERT INTO (lcTmpMark) FROM MEMVAR
    *ENDIF
    IF SEEK(CMARKER,'PDMMARK')
      lcPattern = SPACE(12)
      IF SEEK(laData[1],'CUTTKTH') .AND. SEEK(CUTTKTH.STYLE,'STYLE')
        lcPattern = PADR(SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),10)+;
                    RIGHT(ALLTRIM(STYLE.PATTERN),2)
      ENDIF
      SELECT PDMMARK
      LOCATE REST WHILE CMARK_ID = CTKTBOM.CMARKER FOR CPATT_NO+CREV_NO = lcPattern
      IF FOUND()
        IF SEEK(CMARK_ID,lcTmpMark)
          SELECT (lcTmpMark)
          LOCATE REST WHILE CMARK_ID = PDMMARK.CMARK_ID;
                        FOR CPATT_NO+CREV_NO = PDMMARK.CPATT_NO+PDMMARK.CREV_NO
          IF FOUND()
            LOOP
          ENDIF
          SELECT PDMMARK
        ENDIF
        SCATTER MEMO MEMVAR
        INSERT INTO (lcTmpMark) FROM MEMVAR
      ELSE
        SCAN REST WHILE CMARK_ID = CTKTBOM.CMARKER
          IF SEEK(CMARK_ID,lcTmpMark)
            SELECT (lcTmpMark)
            LOCATE REST WHILE CMARK_ID = PDMMARK.CMARK_ID;
                          FOR CPATT_NO+CREV_NO = PDMMARK.CPATT_NO+PDMMARK.CREV_NO
            IF FOUND()
              LOOP
            ENDIF
            SELECT PDMMARK
          ENDIF
          SCATTER MEMO MEMVAR
          INSERT INTO (lcTmpMark) FROM MEMVAR
        ENDSCAN
      ENDIF
    ENDIF
    *B606481,1 AMH [End]
    
  ENDSCAN
ENDIF
SELECT (lnAlias)
*-- end of lfGetMark.

*!*************************************************************
*! Name      : lfGetPiece
*! Developer : Ahmed Maher (AMH)
*! Date      : 02/19/2002
*! Purpose   : Get the pieces assigned to section.
*!*************************************************************
*! Example   : = lfGetPiece()
*!*************************************************************
*
FUNCTION lfGetPiece

PRIVATE lnAlias
lcPieces = ''
lnAlias = SELECT(0)
=SEEK(CUTTKTH.STYLE,'STYLE')
=SEEK('S'+STYLE.SCALE,'SCALE')
IF SEEK(CKEY_FLD+CSECTNO,'PDMMRKSL')
  SELECT PDMMRKSL
  SCAN REST WHILE CKEY_FLD+CSECTNO+CCUTMCODE = PDMMRKSH.CKEY_FLD+PDMMRKSH.CSECTNO
    =SEEK(CPATERN_NO+CCUTMCODE,'PDMCUTMS')
    lcSize = STR(NSIZE,1)
    lcPieces = lcPieces+IIF(EMPTY(lcPieces),'','/')+ALLTRIM(STR(PIECES,6))+;
               IIF(EMPTY(EVALUATE('SCALE.SZ'+lcSize)),'',' ')+ALLTRIM(EVALUATE('SCALE.SZ'+lcSize))+;
               IIF(EMPTY(PDMCUTMS.CDESC),'',' ')+ALLTRIM(PDMCUTMS.CDESC)
  ENDSCAN
ENDIF
SELECT (lnAlias)
RETURN lcPieces
*-- end of lfGetPiece.

*!*************************************************************
*! Name      : lfClearTmp
*! Developer : AMH (AHMED MAHER)
*! Date      : 02/18/2002
*! Purpose   : Function to zap the tmp file.
*!*************************************************************
*
FUNCTION lfClearTmp

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpSecLn)
ZAP
SELECT (lnAlias)
*-- end of lfClearTmp.