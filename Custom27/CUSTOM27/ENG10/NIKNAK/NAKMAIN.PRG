*!**************************************************************************
*! Name      : NAKMAIN.PRG
*! Developer : Nader NABIL (NNA)
*! Date      : 08/15/2004
*! Purpose   : NIKNAK Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C123656,1
*!**************************************************************************
*! Modifications
*!C123656,1 NNA 08/15/2004 show a screen to input a predicted Pack Total For NikNak
*!C123659,1 NNA 08/24/2004 add a new Bar Menu Calls(Auto Assign PO's) in the PO module to use it
*!C123659,1 NNA            with the [Letter Of Credit] To show A Screen [Cut-Off Shipment date]
*!C123659,1 NNA            that the user will key in to get all PO's that have the same Data
*!C123659,1 NNA            as Specified on the LC Screen
*!C123614,1 NNA 09/19/2004 CP. for Object auto linking by looks for saved files where the file
*!C123614,1 NNA            name represents the last 8 Chr. of the style number.
*!C123615,1 TMI 11/08/2004 Add trigger to smusers screen to update the SYUUSER.email field 
*!**************************************************************************

*-- laScrMode [1] --> SELECT MODE
*-- laScrMode [2] --> VIEW   MODE
*-- laScrMode [3] --> EDIT   MODE
*-- laScrMode [4] --> ADD    MODE


PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**************************************************************************
*! Name      : lfDispScr
*! Developer : NADER NABIL (NNA)
*! Date      : 08/15/2004
*! Purpose   : Function to Display the NikNak Total Pack Qty. Scrren
*!**************************************************************************
*! Example   : lfDispScr()
*!**************************************************************************
*! C123656,1 NNA 08/15/2004
*!**************************************************************************
FUNCTION lfDispScr

PRIVATE lnOldAlS , llChkTotal
lnOldAlS = SELECT(0)
llChkTotal = gfGetMemVar('M_ChkTotal',gcAct_Comp)  
IF llChkTotal
  lcTrgCursr = lcLinFil + 'A'
  IF !USED(lcTrgCursr)
    CREATE CURSOR (lcTrgCursr) (nTotal N(10),llUpdTot L(1),llPcopy L(1))
    INSERT INTO (lcTrgCursr) (nTotal,llUpdTot,llPcopy) VALUES  (0,.F.,.F.)
  ENDIF

  PUSH KEY                                     && To save the the current on key label
  ON KEY
  DO (gcScrDir + gcWinAppl + '\IcTotQty.SPR')   && calling the screen ICSPTMP
  POP KEY                                      && To Restore the previous assignments for on key label
  SELECT(lnOldAlS)
  RETURN lnChoice
ELSE
  RETURN
ENDIF
*-- END OF FUNCTION lfDispScr

*!**************************************************************************
*! Name      : lfvOK
*! Developer : NADER NABIL (NNA)
*! Date      : 08/15/2004
*! Purpose   : Valid function of the push button pbOk in Pack Total Screen.
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
*! C123656,1 NNA 08/15/2004
*!**************************************************************************

FUNCTION lfvOK

PRIVATE lnOldAlS
STORE 0 TO lnOldAlS
lnOldAlS = SELECT(0)
IF lnPTotal >= 0 
  lcTrgCursr = lcLinFil + 'A'
  REPLACE &lcTrgCursr..nTotal WITH lnPTotal , &lcTrgCursr..llUpdTot WITH .T.
  lnPTotQT = lnPTotal
ELSE
  lnPTotal = 0
  SHOW GET lnPTotal
  _CUROBJ = OBJNUM(lnPTotal)  
ENDIF

SELECT(lnOldAlS)
*-- END OF FUNCTION lfvOK

*!**************************************************************************
*! Name      : lfPcopy
*! Developer : NADER NABIL (NNA)
*! Date      : 08/15/2004
*! Purpose   : Valid function of the push button pbOk in Pack Total Screen.
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
*! C123656,1 NNA 08/15/2004
*!**************************************************************************

FUNCTION lfPcopy

PRIVATE lnOldAlS
STORE 0 TO lnOldAlS
lnOldAlS = SELECT(0)
lcTrgCursr = lcLinFil + 'A'

IF !USED(lcTrgCursr) 
  CREATE CURSOR (lcTrgCursr) (nTotal N(10),llUpdTot L(1),llPcopy L(1))
  INSERT INTO (lcTrgCursr) (nTotal,llUpdTot,llPcopy) VALUES  (0,.F.,.F.)
ENDIF

REPLACE &lcTrgCursr..llPCOPY WITH .T. 
SELECT(lnOldAlS)
*-- END OF FUNCTION lfPcopy

*!**************************************************************************
*! Name      : lfPackSav
*! Developer : NADER NABIL (NNA)
*! Date      : 08/15/2004
*! Purpose   : Validate Function when the user Save a New Pack to checks if the
*!           : Predicted total Agrees with the Actual total Entered Or not
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
*! C123656,1 NNA 08/15/2004
*!**************************************************************************

FUNCTION lfPackSav

PRIVATE lnOldAlS , lnRecNo , lnTotalQty
STORE 0 TO lnOldAlS , lnRecNo , lnTotalQty

lnOldAlS = SELECT(0)
lnRecNo  = RECNO(lcLinFil)
lcTrgCursr = lcLinFil + 'A'

SELECT (lcLinFil)
IF &lcTrgCursr..llPcopy
  *-- update the Flag to can update the total Field on the Screen if I edit an Exist Pack
  REPLACE &lcTrgCursr..llUpdTot WITH .F.   
  RETURN .T.
ENDIF

SUM TOTQTY TO lnTotalQty

*-- update the Flag to can update the total Field on the Screen if I edit an Exist Pack
REPLACE &lcTrgCursr..llUpdTot WITH .F.

*E300420,1 Message : 32010
*E300420,1 The Qty. entered 9999 does not agree with the Pack total 999
*E300420,1 Button : 32001 
*E300420,1 Accept/Modify

IF &lcTrgCursr..nTotal > 0 .AND. &lcTrgCursr..nTotal <> lnTotalQty
  IF gfModalGen('QRM32010B32001',.F.,.F.,.F.,'Qty. entered '+ALLTRIM(STR(lnTotalQty))+;
              ' does not agree with the Pack total ' + ALLTRIM(STR(&lcTrgCursr..nTotal))) = 2

    SELECT (lcLinFil)
    IF BETWEEN(lnRecNo,1,RECCOUNT())
      GOTO lnRecNo
    ENDIF
    SELECT(lnOldAlS)
    RETURN .F.
  ELSE
    SELECT (lcLinFil)
    IF BETWEEN(lnRecNo,1,RECCOUNT())
      GOTO lnRecNo
    ENDIF
    SELECT(lnOldAlS)
    RETURN .T.  
  ENDIF
ELSE
  SELECT (lcLinFil)
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GOTO lnRecNo
  ENDIF
  SELECT(lnOldAlS)
  RETURN .T.
ENDIF

*-- END OF FUNCTION lfPackSav

*!**************************************************************************
*! Name      : lfChkTot
*! Developer : NADER NABIL (NNA)
*! Date      : 08/15/2004
*! Purpose   : Validate Function when the user Save a New Pack to checks if the
*!           : Predicted total Agrees with the Actual total Entered Or not
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
*! C123656,1 NNA 08/15/2004
*!**************************************************************************

FUNCTION lfChkTot

PRIVATE lnOldAlS 
STORE 0 TO lnOldAlS 

lnOldAlS = SELECT(0)
lcTrgCursr = lcLinFil + 'A'

IF !USED(lcTrgCursr)
  CREATE CURSOR (lcTrgCursr) (nTotal N(10),llUpdTot L(1),llPcopy L(1))
  INSERT INTO (lcTrgCursr) (nTotal,llUpdTot,llPcopy) VALUES  (0,.F.,.F.)
ENDIF

IF &lcTrgCursr..llUpdTot
  SELECT(lnOldAlS)
  RETURN .T.
ELSE
  SELECT(lnOldAlS)
  RETURN .F.
ENDIF

*-- End of Function lfChkTot

*!**************************************************************************
*! Name      : lfAddMPad
*! Developer : NADER NABIL (NNA)
*! Date      : 08/20/2004
*! Purpose   : Add a new option titled 'Auto Assign PO's on the Options menu
*!**************************************************************************
*! Parameters: 
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
*!C123659,1
FUNCTION lfAddMPad

*--check if the option pad is already defined on the sysmenu
PRIVATE llFound
STORE .F. TO llFound

FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
  IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = 'Options'
    llfound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llfound
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ELSE
  *-- Count Options BARS to add the new BAR at the end of the Popup
  *-- 
  lnBarNo = CNTBAR(LEFT('_OPTIONSPOP',10)) + 1


  *-- Skip The New BAR if this is not the detail Folder or this not ADD Or EDIT Mode 
  *-- Or there's No lines
  ON PAD _OPTIONS OF _MSYSMENU ACTIVATE POPUP _OPTIONSPOP
  DEFINE BAR lnBarNo OF _OPTIONSPOP PROMPT "\<Auto Assign PO's" SKIP FOR ;
             !(lnactfolder= 4 AND (laScrMode[4] OR laScrMode[3]))
  ON SELECTION BAR lnBarNo OF _OPTIONSPOP DO lfCallScr in NAKMAIN
ENDIF
RETURN

*-- END OF FUNCTION lfAddMPad.

*!*************************************************************
*! Name      : lfvDate
*! Developer : NADER NABIL (NNA)
*! Date      : 08/24/2004
*! Purpose   : Validate the Cut-Off Shipment Date
*!*************************************************************
*! Calls     : gfModalGen,CHECKPRD
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDate()
*!*************************************************************
*! C123659,1

FUNCTION lfvDate
PARAMETER llOK
PRIVATE lcGlYear , lcGlPeriod 
STORE ' ' TO lcGlYear , lcGlPeriod
IF MDOWN() 
  RETURN
ENDIF
IF EMPTY(ldCtShDate)
  *-- Message : 40032
  *-- Must enter a default invoice date.
  *-- Button : 00000
  *-- Ok
  = gfModalGen('INM32010B32001',.F.,.F.,.F.,'You have to input a valid Date')
   _CUROBJ = OBJNUM(ldCtShDate)
  LLCHECKdAT = .F.
  RETURN 
ENDIF
IF !CHECKPRD(ldCtShDate,'lcGlYear','lcGlPeriod','IN',.T.)
  **MESSAGE : " This date does not belong to any period  "
  **          "                    ® Ok ¯                "
  =gfModalGen("TRM00133B00000","DIALOG")
  _CUROBJ = OBJNUM(ldCtShDate)
  LLCHECKdAT = .F.
  RETURN 
ENDIF
LLCHECKdAT = .T.
RETURN
*--END OF FUNCTION lfvDate.

*!**************************************************************************
*! Name      : lfCallScr
*! Developer : NADER NABIL (NNA)
*! Date      : 07/18/2004
*! Purpose   : Function to Display the NikNak Cut-Off Shipment Date Scrren
*!**************************************************************************
*! Example   : lfCallScr()
*!**************************************************************************
*! C123659,1 NNA 08/15/2004
*!**************************************************************************

FUNCTION lfCallScr
PRIVATE ldCtShDate , LLCHECKdAT
LLCHECKdAT = .F.
ldCtShDate = gdSysDate
*-- You have to enter the currency unit first.
IF EMPTY(laData[8]) .AND. llMulCurr
  =gfModalGen("INM34121B00000" , "DIALOG")
  RETURN
ENDIF

DO (gcScrDir + 'POCTOFSH.SPX')

*-- END OF FUNCTION lfCallScr.

*!**************************************************************************
*! Name      : lfvShpDOK
*! Developer : NADER NABIL (NNA)
*! Date      : 07/18/2004
*! Purpose   : Function to Display the NikNak Total Pack Qty. Scrren
*!**************************************************************************
*! Example   : lfCallBar()
*!**************************************************************************
*! C123659,1 NNA 08/15/2004
*!**************************************************************************

FUNCTION lfvShpDOK
PRIVATE lcOrder , lnOldAlias , lcOldTag , llFound
STORE 0 TO lnOldAlias
STORE '' TO lcOrder
STORE .F. TO llFound
=lfvDate()
IF !LLCHECKdAT 
  RETURN
ELSE

lnOldAlias = SELECT(0)
SELECT(lcPoHdrFil)
lcOrder = SET ('ORDER')            && Save the POSHDR Order.
SET ORDER TO Poshdrv
IF SEEK(laData[3]+'P')
  SCAN WHILE Vendor+cStyType+Po = laData[3]+'P' FOR CPRICECUR = laData[8] .AND. ;
                     STATUS = 'O' .AND. COMPLETE <= ldCtShDate AND EMPTY(CLCNO) .AND. ;
                     CLCSTATUS <>'S'
    SCATTER MEMVAR MEMO
    llFound = .T.
    SELECT(lcTmpPoShd)
    LOCATE FOR Vendor = &lcPoHdrFil..VENDOR .AND. PO = &lcPoHdrFil..PO 
    IF !FOUND()
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE cLCNo     WITH laData[2] ;
              lcExpire  WITH laData[7] ;
              cLcStatus WITH 'S'       ;
              cLCCstItm WITH IIF(EMPTY(cLCCstItm) , "1" , cLCCstItm) ;
              cStatus   WITH 'M'       ;
              nRecNo    WITH RECNO(lcPOHdrFil)
    ENDIF
  ENDSCAN
  IF !llFound
    = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'This Vendor has no open PO for the selected ' +;
                 'Price Currency Code before this date')
  ENDIF
  SELECT(lcTmpPoShd)
  LOCATE
  lnMarker = RECNO()
  BROWSE FIELDS ;
         cMarker  =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F. , ;
         PO = IIF(lcFromMod = "S" , PO , POMAT) :H = 'PO#' :10  :R, ;
         Status=IIF(cLcStatus="A","Added   ","Assigned"):H = 'LC Status' :17 :R, ; 
         Entered :H = 'Entered'  :11 :R, ;
         Complete:H = 'Complete' :11 :R, ;
         lnOpn = lfPoOpnAmt() :H = 'Open Amt.' :16:R:P='9999999999.999' ;
         WINDOW (lcLetCr4A)  ;
         IN WINDOW (lcLetCr4);
         LOCK 0            ;
         NOMENU            ;
         NOAPPEND          ;
         NODELETE          ;
         NOEDIT            ;
         NOWAIT            ;
         SAVE              ;
         NOCLEAR           ;
         WHEN lfwBrow()    ;
         VALID :F lfvBrow() ;
         TITLE lcPOBrTitl


ENDIF
  CLEAR READ
ENDIF
SELECT(lcPoHdrFil)
SET ORDER TO &lcOrder       && return to the Previous Order in File POSHDR.
SELECT(lnOldAlias)
*-- END OF FUNCTION lfvShpDOK.

*!**************************************************************************
*! Name      : lfClnTmp
*! Developer : NADER NABIL (NNA)
*! Date      : 07/18/2004
*! Purpose   : Function to Display the NikNak Total Pack Qty. Scrren
*!**************************************************************************
*! Example   : lfClnTmp()
*!**************************************************************************
*! C123659,1 NNA 08/26/2004
*!**************************************************************************
FUNCTION lfClnTmp
SELECT(lcTmpPoShd)
DELETE ALL FOR VENDOR <>laData[2] .AND. CPRICECUR <> laData[8]
RETURN
*-- END OF FUNCTION lfClnTmp.

*!**************************************************************************
*! Name      : lfGETDIRS
*! Developer : NADER NABIL (NNA)
*! Date      : 09/19/2004
*! Purpose   : Do every thing accordiling to the settinges allowed.m
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C123614
FUNCTION lfGETDIRS

STORE "" TO lcPath1,lcPath2,lcPath3,lcPath4,lcPath5,lcPOffice,lcSrchChr
STORE 0 TO lnLStyChr , lnStartPos , lnStyLen
*-- Check the memo file existance.
llFoundP = FILE('NAKMAIN.MEM')
IF llFoundP
  RESTORE FROM NAKMAIN.MEM ADDITIVE
ENDIF

*-- Get the Style Chars. for Auto Objects From the IC Module's Setup
lnLStyChr = IIF(gfGetMemVar('M_STYCHR') > 8 OR gfGetMemVar('M_STYCHR') = 0, 8 ,gfGetMemVar('M_STYCHR'))

lnRecUpd = 0
*--Get the 5 paths
lcFile   = ""
*--Open Objects files
IF !USED('OBJLINK')
  =gfOpenFile(gcDataDir+'OBJLINK','Objlnkty','SH')
ENDIF
IF !USED('objects')  
   =gfOpenFile(gcDataDir+'objects','Objectid','SH')
ENDIF
FOR lnFrom = 1 TO 5
  lcFrom = STR(lnFrom,1)
  STORE .F. TO llDoc,llXls,llJpg
  *--Internal loop for the 3 file types
  IF !EMPTY(lcPExt&lcFrom)
    DIMENSION laFiles[1]
    laFiles = ""
    lcFile  = ""
    llDoc =  .T.
    lnStyLen   = LEN(ALLTRIM(laData[1]))
    lnStartPos = IIF(lnStyLen <= lnLStyChr,1,(lnStyLen - lnLStyChr)+1)
    lcSrchChr  = IIF(lnLStyChr = 8,SUBSTR(ALLTRIM(laData[1]),lnStartPos,lnLStyChr) ,;
                     PADR(SUBSTR(ALLTRIM(laData[1]),lnStartPos,lnLStyChr),8,'?'))
    =ADIR(laFiles,lcPath&lcFrom + lcSrchChr + '.' + lcPExt&lcFrom)
    IF ALEN(laFiles,1) > 1
      DO lpMover
    ELSE
      IF FILE(lcPath&lcFrom+ALLTRIM(laFiles[1]))
        lcFile = lcPath&lcFrom+ALLTRIM(laFiles[1])
      ENDIF  
    ENDIF  
    IF !EMPTY(lcFile)
      DO lpUpdate
    ENDIF  
  ENDIF    
ENDFOR
WAIT WINDOW STR(lnRecUpd,2)+ ' records are updated.' NOWAIT
lnBarNo = 0
*-- END OF FUNCTION lfGETDIRS.

*!**************************************************************************
*! Name      : lpUpdate
*! Developer : NADER NABIL (NNA)
*! Date      : 09/19/2004
*! Purpose   : Save Objects to the Objlink File
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C123614
PROCEDURE lpUpdate

PRIVATE lnAlias
lnAlias = SELECT()
IF !SEEK('S'+laData[1],'OBJLINK')
  SELECT OBJECTS
  lcObjCode = lfGetLastObj()
  SELECT OBJLINK  
  lnRecUpd = lnRecUpd + 1
  APPEND BLANK
  REPLACE cobject_id WITH lcObjCode,;
          cObjLink   WITH PADR(laData[1],20),;
          cObjLnkTyp WITH 'S',;
          cObj_Desc  WITH SUBSTR(lcFile,RAT("\",lcFile)+1)

  IF INLIST(RIGHT(ALLTRIM(lcFile),3),'XLS','DOC')
    REPLACE mobj_notes WITH 'Please hit the OPEN icon to view the attached '+IIF(RIGHT(ALLTRIM(lcFile),3)='XLS','Excel Sheet.','Document.')
  ELSE
    IF INLIST(RIGHT(ALLTRIM(lcFile),3),'JPG','BMP')
      REPLACE mobj_notes WITH 'Please hit the ZOOM icon to view the attached '+RIGHT(ALLTRIM(lcFile),3)
    ENDIF  
  ENDIF

  SELECT objects
  APPEND BLANK
  REPLACE cobject_id WITH lcObjCode,;
          cImgPath   WITH lcFile,;
          llnk       WITH .T.
ELSE
  SELECT OBJLINK
  llNewLine = .T.
  SCAN REST WHILE cobjlnktyp+cobjlink =  'S'+laData[1] 
    IF SEEK(cObject_id,'objects') AND UPPER(ALLTRIM(objects.cImgPath))==UPPER(ALLTRIM(lcFile))
      llNewLine = .F.
      EXIT
    ENDIF
  ENDSCAN
  IF  llNewLine
    lnRecUpd = lnRecUpd + 1  
    SELECT OBJECTS
    lcObjCode = lfGetLastObj()
    SELECT OBJLINK
    APPEND BLANK
    REPLACE cobject_id WITH lcObjCode,;
            cObjLink   WITH PADR(laData[1],20),;
            cObjLnkTyp WITH 'S',;
            cObj_Desc  WITH SUBSTR(lcFile,RAT("\",lcFile)+1)

  IF INLIST(RIGHT(ALLTRIM(lcFile),3),'XLS','DOC')
    REPLACE mobj_notes WITH 'Please hit the OPEN icon to view the attached '+IIF(RIGHT(ALLTRIM(lcFile),3)='XLS','Excel Sheet.','Document.')
  ELSE
    IF INLIST(RIGHT(ALLTRIM(lcFile),3),'JPG','BMP')
      REPLACE mobj_notes WITH 'Please hit the ZOOM icon to view the attached '+RIGHT(ALLTRIM(lcFile),3)
    ENDIF  
  ENDIF

    SELECT objects
    APPEND BLANK
    REPLACE cobject_id WITH lcObjCode,;
            cImgPath   WITH lcFile,;
            llnk       WITH .T.
  ENDIF
ENDIF
SELECT (lnAlias)
*-- END OF Procedure lpUpdate.

*!**************************************************************************
*! Name      : lfADDBAR
*! Developer : NADER NABIL (NNA)
*! Date      : 09/19/2004
*! Purpose   : Add a new option titled 'Object Auto Search's on the Options menu
*!             with the Style Screen
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C123614
FUNCTION lfADDBAR

*--check if the option pad is already defined on the sysmenu
PRIVATE llFound
STORE .F. TO llFound

FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
  IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = 'Options'
    llfound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llfound
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _lPopOpt MARGIN SHADOW
ELSE
  *-- Count Options BARS to add the new BAR at the end of the Popup
  *-- 
  lnBarNo = CNTBAR(LEFT('_lPopOpt',10)) + 1

  ON PAD _OPTION OF _MSYSMENU ACTIVATE POPUP _lPopOpt
  *--Add 'Auto Search' bar to Option menu.
  DEFINE BAR lnBarNo OF _lPopOpt PROMPT "Object Auto Sea\<rch" SKIP FOR ;
  			(laScrMode[1] OR laScrMode[4] )
  ON SELECTION BAR lnBarNo OF _lPopOpt DO lfGETDIRS in NAKMAIN
ENDIF
RETURN

*-- END OF FUNCTION lfADDBAR.

*!*************************************************************************
*! Name      : lfGetLastObj
*! Developer : NADER NABIL (NNA)
*! Date      : 09/19/2004
*! Purpose   : function to get sequential object ID
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*! C123614
FUNCTION lfGetLastObj

lcFilter = FILTER()
SET FILTER TO
GO BOTT
lcReturn = PADL(INT(VAL(COBJECT_ID))+1,10,'0')
SET FILTER TO &lcFilter

RETURN lcReturn

*-- END OF FUNCTION lfGetLastObj.

*!*************************************************************************
*! Name      : lpMover
*! Developer : NADER NABIL (NNA)
*! Date      : 09/19/2004
*! Purpose   : Function to Show the Mover Screen
*!*************************************************************************
*! Returns   :  
*!*************************************************************************     
*! C123614
PROCEDURE lpMover

DECLARE laRpTarget[1]
laRpTarget = ""
DIMENSION laSource[ALEN(laFiles,1)]
FOR lnI = 1 TO ALEN(laFiles,1)
  laSource[lnI] = laFiles[lnI,1]
ENDFOR
*--Fill the target array
DO lpGetFile

=ASORT(laSource)
= gfMover(@laSource,@laRpTarget,'Select objects from: '+lcPath&lcFrom,.T.,'')
FOR lnI = 1 TO ALEN(laRpTarget,1)
  IF !EMPTY(laRpTarget)
    lcFile = lcPath&lcFrom+ALLTRIM(laRpTarget[lnI])
    IF !EMPTY(lcFile)
      DO lpUpdate
      lcFile = ""
    ENDIF  
  ENDIF  
ENDFOR
*-- END OF FUNCTION lpMover.

*!**************************************************************************
*! Name      : lpGetFile
*! Developer : NADER NABIL (NNA)
*! Date      : 09/19/2004
*! Purpose   : Get the previously added files to be the on right side of the mover
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*!C123614,1
PROCEDURE lpGetFile

PRIVATE lnAlias
lnAlias = SELECT()
SELECT OBJLINK
=SEEK('S'+laData[1],'OBJLINK')
lnNoOfElm = 0
SCAN REST WHILE cobjlnktyp+cobjlink = 'S'+laData[1] FOR !EMPTY(OBJLINK.cobj_desc);
          AND ASCAN(laRpTarget,ALLTRIM(OBJLINK.cobj_desc))  = 0;
          AND ASCAN(laSource,ALLTRIM(OBJLINK.cobj_desc))  <> 0
  lnNoOfElm = lnNoOfElm + 1 
  DIMENSION laRpTarget[lnNoOfElm]
  laRpTarget[lnNoOfElm]  = ALLTRIM(OBJLINK.cobj_desc)
ENDSCAN
SELECT (lnAlias)

*-- END OF FUNCTION lpGetFile.

*!**************************************************************************
*! Name      : lfOPENOBJ
*! Developer : NADER NABIL (NNA)
*! Date      : 09/19/2004
*! Purpose   : Open objects
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C123614,1
FUNCTION lfOPENOBJ

STORE "" TO lcPath1,lcPath2,lcPath3,lcPath4,lcPath5,lcPOffice
*-- Check the memo file existance.
lcPOffice = ' '
llFoundP = FILE('C:\OFICPATH.MEM')
IF llFoundP
  *-- Restore office path from c:\oficpath.mem
  RESTORE FROM C:\OFICPATH.MEM ADDITIVE
ENDIF

*--Open the object
IF EMPTY(lcPOffice)
  IF  gfModalGen('QRM00000B00006',.F.,.F.,.F.,"MS Office path is empty. Do you want to select the correct one?") = 2
    RETURN
  ENDIF
  lcPOffice = GETDIR()
  IF EMPTY(lcPOffice)
    RETURN
  ELSE
    *-- Save office path in a separte file c:\oficpath.mem
    SAVE ALL LIKE lcPOffice* TO C:\OFICPATH.MEM
  ENDIF
ENDIF
  
IF INLIST(RIGHT(ALLTRIM(objects.cImgPath),4),".DOC",".XLS")
  DO CASE 
    CASE RIGHT(ALLTRIM(objects.cImgPath),4)= ".DOC"
      *--Update setup path
      IF !FILE(lcPOffice+'WINWORD.EXE')
        IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,"MS Office path is invalid. Do you want to select the correct one?") = 2
          RETURN
        ENDIF
        lcPOffice = GETDIR()
        IF EMPTY(lcPOffice)
          RETURN
        ELSE
          IF !FILE(lcPOffice+'WINWORD.EXE')
            RETURN
          ENDIF
          *--Save office path in a separte file c:\oficpath.mem
          SAVE ALL LIKE lcPOffice* TO C:\OFICPATH.MEM
        ENDIF
      ENDIF
      lcOldPath = SYS(5)+SYS(2003)
      SET DEFA TO (lcPOffice)
      lcFileToOpen = ALLTRIM(objects.cImgPath)
      RUN /N WINWORD.EXE &lcFileToOpen
      SET DEFA TO (lcOldPath)
    CASE RIGHT(ALLTRIM(objects.cImgPath),4)= ".XLS"
      *--Update setup path
      IF !FILE(lcPOffice+'Excel.exe')
        IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,"MS Office path is invalid. Do you want to select the correct one?") = 2
          RETURN
        ENDIF
        lcPOffice = GETDIR()
        IF EMPTY(lcPOffice)
          RETURN
        ELSE
          *-- check for excel 
          IF !FILE(lcPOffice+'Excel.EXE')          
            RETURN
          ENDIF
          SAVE ALL LIKE lcPOffice* TO C:\OFICPATH.MEM
        ENDIF
      ENDIF
      lcOldPath = SYS(5)+SYS(2003)
      SET DEFA TO (lcPOffice)
      lcFileToOpen = ALLTRIM(objects.cImgPath)
      RUN /N Excel.exe &lcFileToOpen
      SET DEFA TO (lcOldPath)
    CASE RIGHT(ALLTRIM(objects.cImgPath),4)= ".TXT"
      *--Update setup path
      IF !FILE(lcPOffice+'WINWORD.EXE')
        IF gfModalGen('QRM00000B00006',.F.,.F.,.F.,"MS Office path is invalid. Do you want to select the correct one?") = 2
          RETURN
        ENDIF
        lcPOffice = GETDIR()
        IF EMPTY(lcPOffice)
          RETURN
        ELSE
          IF !FILE(lcPOffice+'WINWORD.EXE')
            RETURN
          ENDIF
          *--Save office path in a separte file c:\oficpath.mem
          SAVE ALL LIKE lcPOffice* TO C:\OFICPATH.MEM
        ENDIF
      ENDIF
      lcOldPath = SYS(5)+SYS(2003)
      SET DEFA TO (lcPOffice)
      lcFileToOpen = ALLTRIM(objects.cImgPath)
      RUN /N WINWORD.EXE &lcFileToOpen
      SET DEFA TO (lcOldPath)

  ENDCASE
ENDIF

*-- END OF FUNCTION lfOPENOBJ.

*:**************************************************************************
*:* Name        : lfSMEMAIL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/31/2004
*:* Purpose     : öööAdd option pad to the "msusers" screen to Open the screen SMEMAIL
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*C123615,1
FUNCTION lfSMEMAIL
PRIVATE lnBarNo

IF !lfFoundPad('Options')
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P,' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF

lnBarNo = CNTBAR('_OPTIONPOP') + 1
DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT 'Update User \<E-mail' SKIP FOR laScrMode[1]
ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfOpnEmail IN NAKMAIN.PRG

*-- end of lfSMEMAIL.

*:**************************************************************************
*:* Name        : lfFoundPad
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/31/2004
*!* Purpose     : check if any pad menu is exit in _sysmenu
*:***************************************************************************
*C123615,1
FUNCTION lfFoundPad
PARAMETER lcPadName

PRIVATE llFound
llFound = .F.
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
  IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = lcPadName
    llfound = .T.
	EXIT
  ENDIF
ENDFOR
RETURN(llFOund)
*-- end of lfFoundPad.

*:**************************************************************************
*:* Name        : lfOpnEmail
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/31/2004
*:* Purpose     : Open screen SMEMAIL
*:***************************************************************************
*C123615,1
FUNCTION lfOpnEmail
PRIVATE lnSlct,lcTempFl,lcOldValue

lnSlct = SELECT()
lcTempFl = '_'+SUBSTR(lcPrv_Tmp1,2)
IF !USED(lcTempFl) .OR. &lcTempFl..CUSER_ID <> laData[1]
  CREATE CURSOR (lcTempFl) (CUSER_ID C(10),EMAIL C(50))
  APPEND BLANK
  REPLACE CUSER_ID WITH laData[1] ;
          EMAIL    WITH IIF(laScrMode[4],'',SYUUSER.EMAIL)
ENDIF
SELECT &lcTempFl
GO TOP
STORE &lcTempFl..EMAIL TO M.EMAIL,lcOldValue
lcTitle = 'Update User E-mail Address.'

DO (gcScrDir + gcWinAppl + '\SMEMAIL.SPX')

REPLACE EMAIL WITH M.EMAIL

SELECT (lnSlct)
*-- end of lfOpnEmail.

*:**************************************************************************
*:* Name        : lfRELPAD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/31/2004
*:* Purpose     : Release Added option pad
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*C123615,1
FUNCTION lfRELPAD

RELEASE PAD _Option OF _MSYSMENU
*-- end of lfRELPAD.

*:**************************************************************************
*:* Name        : lfvEmail
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/31/2004
*:* Purpose     : Valid function for Email field
*:***************************************************************************
*:* Called from : SMEMAIL.SCX
*:***************************************************************************
*C123615,1
FUNCTION lfvEmail

IF !EMPTY(m.EMAIL)
  IF !'.'$m.EMAIL .OR. !'@'$m.EMAIL
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'This is not a Valid e-mail Address.')
    _CUROBJ = OBJNUM(m.EMAIL)
    RETURN 
  ENDIF
ENDIF

IF m.EMAIL <> lcOldValue
  SHOW GET pbOk ENABLE
ELSE
  SHOW GET pbOk DISABLE
ENDIF
*-- end of lfvEmail.


*:**************************************************************************
*:* Name        : lfUPDEMAIL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/31/2004
*:* Purpose     : Update the array element related to the field "EMAIL" in the file syuuser for niknak
*:***************************************************************************
*:* Called from : SMUSERS.PRG
*:***************************************************************************
*C123615,1
FUNCTION lfUPDEMAIL
PRIVATE lnSlct,lnPos,lcTempFl
lnSlct = SELECT()
lcTempFl = '_'+SUBSTR(lcPrv_Tmp1,2)

*- Update the field "lcScFields"
IF !'EMAIL' $ lcScFields
  lcScFields = lcScFields + ',EMAIL'
  lnPos = OCCURS([,] , lcScFields)+1
  DIMENSION laData[lnPos]
  *--MHM
  laData[lnPos] = &lcTempFl..EMAIL
  *--MHM
ENDIF
*--MHM
*laData[lnPos] = &lcTempFl..EMAIL
*--MHM

*- Relase the temp file used to save a the  user-email 
USE IN &lcTempFl

SELECT (lnSlct)
*-- end of lfUPDEMAIL.