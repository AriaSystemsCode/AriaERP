*************************************************************************************************
* PROG : ICDAN12.PRG
* DATE : 22/05/2000
* DESC : CONVERT CUSTOME MATRIX PROGRAM FOR DANA TO 27.
* AUTH : Adel Mohammed El Gazzar (ADEL)
* Refer to (C101786)
* Remarks:
*        This program calls two screen sets(in a project called ICDAN120) as follows:
*        1) ICDAN120A which has the following screens:
*            A)ICDAN120
*            B)ICDAN121
*            C)ICDAN122
*            D)ICDAN123
*        2) ICDAN120B which has the following screen:
*            A)ICDAN125
*************************************************************************************************

*----Variables Desc.
*-- lnLineNo    && Holds line no.
*-- laData      && Holds the values of the fields that we use from base file
*-- laScrMode   && Holds the screen modes.
*--lcStyWind    && Holds the browse title.
*----Declare arrays
DIMENSION laData[1],laScrMode[4],laTarget[1],laSource[1]
*--Initialize variables.
STORE 0        TO lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8,lnLineNo,lnRecCnt
STORE 0 TO lnMarker
STORE SPACE(0) TO laData,lcAccName,lcScFields
laScrMode[1] = .T.
STORE .F. TO llNewLine,llStyChg,llBrowse
lcStyWind = 'SIZES MAX QUANTITIES'
STORE ' ' TO lcPackTmp,lcBrFields
*--Flag to save localy.
laDefproc[9] = .F. 
*--Flag to delete localy.
laDefproc[7] = .F.
lcBrowseTl = ''
STORE .F. TO llNoShow,llCSave

IF !gfSetup()
  RETURN
ENDIF
IF !WEXIST(gcBaseWind)
  lcPackTmp  = gfTempName()
  ICDAN121   = gfTempName()
  ICDAN122   = gfTempName()
  ICDAN123   = gfTempName()
  ICDAN125   = gfTempName()
  lcMajTitle = ALLTRIM(gfItemMask('HM'))
  lcPicMaj   = ALLTRIM(gfItemMask('PM'))
  lnMajorLen = LEN(lcPicMaj)
  lcBrFields = "cAccount  :R :H='Account'    ,"+;
               "cStyle    :R :H=lcMajTitle   ,"+;
               "cMixSize  :R :H='Mix Sizes'  ,"+;
               "cMixStyle :R :H='Mix Styles' "
  lcPackNam  = ' '
  lcTitle    = lcMajTitle+ ' matrix'
  llOpen     = gfOpenFile(gcDataDir+'PackMtrx',gcDataDir+'PackAct','SH',@lcPackNam,.T.)
  SET RELATION TO cAccount INTO PACKMTRX ADDI
  SELECT STYLE
  SET RELATION TO 'S'+SCALE INTO SCALE
  lcScFields = 'cAccount,cMixSize,cMixStyle,cStyle,nMaxSize1,nMaxSize2,nMaxSize3,nMaxSize4,nMaxSize5,nMaxSize6,nMaxSize7,nMaxSize8'
  SELECT (lcPackNam)
  SCATTER FIELDS &lcScFields TO laData BLANK
ENDIF
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)
*--Call the main screen
DO (gcScrDir+gcWinAppl+"\ICDAN12A.SPX")
RELEASE WINDOW (lcStyWind)

IF glQuitting
  IF USED(lcPackTmp)
    SELECT (lcPackTmp)
    USE
    ERASE (gcWorkDir+lcPackTmp+'.DBF')
    ERASE (gcWorkDir+lcPackTmp+'.CDX')
  ENDIF  
ENDIF
RETURN
*-End



*********************************************************************************
*! Name      : lfvSize
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate size fields.
*!*************************************************************
FUNCTION lfvSize
PARAMETERS lnSizeNo

PRIVATE lnDataNo,llUpdate,lcKey,llReplace,lnAlias
lnAlias = SELECT()
lnDataNo = lnSizeNo + 4
lcSizeStr = STR(lnSizeNo,1)
IF laData[lnDataNo]<0 
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Quantity cannot be negative.')    
  _CUROBJ = _CUROBJ
ENDIF
IF laData[lnDataNo] <> nMaxSize&lcSizeStr
  llUpDate = .F.
  llReplace = .T.
  SELECT (lcPackTmp)
  *--Check for another setting for the same account\style\size having a value.
  lcKey = cAccount+cStyle+STR(nLineNo,6) && Get the key to get it back.
  IF SEEK(laData[1]+laData[4])
    SCAN WHILE cAccount+cStyle = laData[1]+laData[4] ;
        FOR INLIST(cStatus,'A','S','M')
      lcRec = cAccount+cStyle+STR(nLineNo,6)
      IF nMaxSize&lcSizeStr<>0 AND lcRec <> lcKey
        llUpDate = .T.
        EXIT
      ENDIF   
    ENDSCAN  
  ENDIF  
  IF llUpdate
    *--If there's another setting for the same account\style\size having a value.
    IF (gfModalGen('TRM00000B00006',.F.,.F.,.F.,"Another setting for this account\style has a max pack for this size.Overwrite it?") = 1)
      =SEEK(lcRec)
      REPLACE nMaxSize&lcSizeStr WITH 0 ,;
              cStatus WITH IIF(cStatus='S','M',cStatus)
      =SEEK(lcKey)
      REPLACE nMaxSize&lcSizeStr WITH laData[lnDataNo]
    ELSE
      =SEEK(lcKey)
      laData[lnDataNo] = nMaxSize&lcSizeStr
      llReplace = .F.
    ENDIF  
  ELSE
    =SEEK(lcKey)
    REPLACE nMaxSize&lcSizeStr WITH laData[lnDataNo]
  ENDIF
  IF llReplace 
    REPLACE cStatus WITH IIF(cStatus='S','M',cStatus)
    =lfWBrow()
  ENDIF  
ENDIF  
SELECT(lnAlias)

*********************************************************************************
*! Name      : lfvNew
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate new button.
*!*************************************************************
FUNCTION lfvNew

STORE 'Y' TO laData[2],laData[3]
STORE SPACE(lnMajorLen) TO laData[4]
STORE 0 TO laData[5],laData[6],laData[7],laData[8],laData[9],laData[10],laData[11],laData[12]
llNewLine = .T.
SELECT (lcPackTmp)
SHOW GETS WINDOW (ICDAN123) ENABLE ONLY
SHOW GET laData[2]  ENABLE
SHOW GET laData[3]  ENABLE
SHOW GET ibStyle    ENABLE
SHOW GET laData[4]  ENABLE
_CUROBJ = OBJNUM(laData[2])    

*********************************************************************************
*! Name      : lfvMixStyle
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate mix style field.
*!*************************************************************
FUNCTION lfvMixStyle

IF !INLIST(laData[3],'Y','N') 
  RETURN .F.
ENDIF
lnRecNo = RECNO()
*--Update temp file in in case modifying.
IF !EMPTY(laData[4])
  llRep = .T.
  *--Is this setting already exits?
  =lfChckSet()
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GO lnRecNo
  ENDIF
  lcKey = cAccount + cStyle+STR(nLineNo,6)
  IF llRep
    REPLACE cMixStyle WITH laData[3],;
            cStatus WITH IIF(cStatus='S','M',cStatus)
  ENDIF
  =lfwBrow()
  =SEEK(lcKey)
ENDIF  
*********************************************************************************
*! Name      : lfvMixSize
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate size style field.
*!*************************************************************
FUNCTION lfvMixSize

IF !INLIST(laData[2],'Y','N')
  RETURN .F.
ENDIF
lnRecNo = RECNO()
*--Update temp file in in case modifying.
IF !EMPTY(laData[4])
  llRep = .T.
  *--Is this setting already exits?
  =lfChckSet()
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GO lnRecNo
  ENDIF
  lcKey = cAccount + cStyle+STR(nLineNo,6)
  IF llRep
    REPLACE cMixSize WITH laData[2],;
            cStatus WITH IIF(cStatus='S','M',cStatus)
  ENDIF          
  =lfWBrow()
  lcKey = cAccount + cStyle+STR(nLineNo,6)
ENDIF  

*********************************************************************************
*! Name      : lfvStyle
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate style code field.
*!*************************************************************
FUNCTION lfvStyle
PRIVATE lcKey
STORE 0 TO lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8
IF !llBrowse AND MDOWN()
  RETURN
ENDIF
IF llBrowse OR (!EMPTY(laData[4]) AND !SEEK(laData[4],'STYLE'))
  llBrowse = .F.
  laData[4]= PADR(gfStyBrw("M" , laData[4] , "" , .F.),lnMajorLen)
  IF EMPTY(laData[4])
    laData[4] = IIF(llNewLine,' ',&lcPackTmp..cStyle)
    _CUROBJ = OBJNUM(laData[4])
    SELECT STYLE
    SET RELATION TO 'S'+SCALE INTO SCALE
    RETURN
  ELSE
    SELECT STYLE
    SET RELATION TO 'S'+SCALE INTO SCALE
  ENDIF  
ENDIF
llBrowse = .F.
SELECT (lcPackTmp)
IF !llNewLine AND cStyle=laData[4] AND cMixSize=laData[2] AND cMixStyle=laData[3]
  RETURN
ENDIF
llUpdate = .T.
lnRecNo = RECNO()
IF !EMPTY(laData[4])
  SHOW GET ladata[4] DISABLE
  =lfChckSet()
  IF BETWEEN(lnRecNo,1,RECCOUNT())
    GO lnRecNo
  ENDIF
  IF llUpdate
    IF llNewLine
    *--- Assuring the Line No
      IF SEEK(laData[1]+laData[4])
        SCAN REST WHILE cAccount+cStyle = laData[1]+laData[4]
        ENDSCAN
        SKIP -1
        IF cStyle <> laData[4]
          SKIP
        ENDIF  
        lnLineNo = nLineNo+1
      ELSE
        lnLineNo = 01
      ENDIF  
      APPEND BLANK
    ELSE
      *--- Assuring the Line No for the new style.
      IF cStyle  <>  laData[4]
        lcGoRec = cAccount+cStyle+STR(nLineNo,6)
        =SEEK(laData[1]+laData[4])
        SCAN REST WHILE cAccount+cStyle = laData[1]+laData[4]
        ENDSCAN
        SKIP -1
        IF cStyle <> laData[4]
          SKIP
        ENDIF  
        lnLineNo = nLineNo+1
        =SEEK(lcGoRec)
      ELSE
        lnLineNo = nLineNo
      ENDIF  
    ENDIF
    REPLACE cAccount  WITH laData[1] ,;
            cStyle    WITH laData[4] ,;
            cMixSize  WITH laData[2],;
            cMixStyle WITH laData[3],;
            cStatus   WITH IIF(llNewLine,'A',IIF(cStatus='S','M',cStatus)),;
            nLineNo   WITH lnLineNo
    SHOW GET pbRemove ENABLE
  ENDIF
ENDIF
llNewLine = .F.
*--- Get the record to get it back after the browse
lcKey = cAccount + cStyle+STR(nLineNo,6)
=lfwBrow()
=SEEK(lcKey)
 
*********************************************************************************
*! Name      : lfvRemove
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate Remove button.
*!*************************************************************
FUNCTION lfvRemove

SELECT (lcPackTmp)
REPLACE cStatus WITH IIF(cStatus<>'A','D','R')
llCUpdate  = .T.
lnRecNo = RECNO()
LOCATE FOR INLIST(cStatus,'A','S','M')
IF !FOUND()
  SHOW GET pbRemove DISABLE
ENDIF
=lfWBrow()

*********************************************************************************
*! Name      : lpSavScr
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate save button.
*!*************************************************************
PROCEDURE lpSavScr

llDelMsg = .F.
llSel = .F.
SELECT (lcPackTmp)
LOCATE FOR !INLIST(cStatus,'D','R')
IF !FOUND()
  llcSave = .F.
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No lines entered. Cannot update.') 
  RETURN
ENDIF
LOCATE FOR cStatus <>'S'
IF !FOUND()
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No changes to save.')    
  RETURN
ENDIF
*--- If the user remove all records for the current account confirm that.
LOCATE FOR INLIST(cStatus,'A','S','M')
IF !FOUND() 
  IF (gfModalGen('TRM00000B00006',.F.,.F.,.F.,"Are you sure you want to delete the account "+ALLTRIM(laData[1])+"?") = 1)
    llSel = .T.
  ELSE
    llcSave = .F.
    RETURN
  ENDIF  
ENDIF  

llZeroRec = .F.
llSelMod = .F.
IF !llSel
  SCAN 
    IF !llZeroRec AND &lcPackTmp..nMaxSize1+&lcPackTmp..nMaxSize2+&lcPackTmp..nMaxSize3+&lcPackTmp..nMaxSize4+;
       &lcPackTmp..nMaxSize5+&lcPackTmp..nMaxSize6+&lcPackTmp..nMaxSize7+&lcPackTmp..nMaxSize8 = 0 
       = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'Records with total max pack equals zero will be ignored.')
       llZeroRec = .T.
       llSelMod = .T.
    ENDIF    
    SCATTER MEMVAR
    SELECT PACKMTRX
    DO CASE
      CASE &lcPackTmp..cStatus = 'A' AND ;
        &lcPackTmp..nMaxSize1+&lcPackTmp..nMaxSize2+&lcPackTmp..nMaxSize3+&lcPackTmp..nMaxSize4;
        +&lcPackTmp..nMaxSize5+&lcPackTmp..nMaxSize6+&lcPackTmp..nMaxSize7+&lcPackTmp..nMaxSize8 <> 0
        INSERT INTO PACKMTRX FROM MEMVAR
        =gfAdd_Info() && Add the add user,time and date
      CASE &lcPackTmp..cStatus = 'M'
        GO &lcPackTmp..nRecNo
       GATHER MEMVAR
         IF &lcPackTmp..nMaxSize1+&lcPackTmp..nMaxSize2+&lcPackTmp..nMaxSize3+&lcPackTmp..nMaxSize4+;
           &lcPackTmp..nMaxSize5+&lcPackTmp..nMaxSize6+&lcPackTmp..nMaxSize7+&lcPackTmp..nMaxSize8 = 0
          DELETE
        ENDIF
      CASE &lcPackTmp..cStatus = 'D'
        GO &lcPackTmp..nRecNo
        DELETE
      OTHERWISE
        LOOP  
    ENDCASE
  ENDSCAN
ELSE  
   =lfvSelect()
   laScrMode = .F.
   laScrMode[1] = .T.
   SHOW GETS
  RETURN
ENDIF
IF RECCOUNT('&lcPackTmp')=1 AND llSelMod
  =lfvSelect()
  RETURN
ENDIF
STORE .F. TO laScrMode
laScrMode[2] = .T.
llCSave = .F.
SHOW GETS
=lfwbrow()

********************************************************\*************************
*! Name      : lfvAccount
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate Account code field.
*!*************************************************************
FUNCTION lfvAccount

IF !llBrowse AND MDOWN()
  RETURN
ENDIF
IF llBrowse OR (!EMPTY(laData[1]) AND laData[1] <> '*****' AND !SEEK('M'+laData[1],'Customer'))
  lcAccount = laData[1]
  SELECT CUSTOMER
  DO CUSBROWM WITH lcAccount
  laData[1] = lcAccount
  llBrowse = .F.
  IF EMPTY(laData[1])
    _CUROBJ = OBJNUM(laData[1])
    RETURN
  ENDIF
ENDIF
llBrowse = .F.
SELECT PACKMTRX
IF EMPTY(laData[1])
  SHOW GET laData[1]
  IF SEEK ('*****') OR ;
    gfModalGen('TRM00000B00006',.F.,.F.,.F.,"Do you want to add a generic Account?") = 1
    laData[1] = '*****'
  ENDIF
ENDIF
IF EMPTY(laData[1])
  _CUROBJ = OBJNUM(laData[1])
  RETURN
ENDIF
lcAccName = IIF(laData[1]='*****','',Customer.btName)
IF SEEK(laData[1])
  STORE .F. TO laScrMode
  laScrMode[2] = .T.
ELSE
  SHOW GET laData[1]
  llCUpdate  = .T.
  IF laData[1] = '*****' OR ;
    gfModalGen('TRM00000B42006',.F.,.F.,.F.,"Account  "+ALLTRIM(laData[1])+" does not exist?") = 1
    STORE .F. TO laScrMode
    laScrMode[4] = .T.
  ELSE   && Re-enter
    laData[1] = ' '
    _CUROBJ = OBJNUM(laData[1])
    RETURN 
  ENDIF  
ENDIF
SHOW GETS
=lfRefresh()

*!*************************************************************
*! Name      : lfBrows
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To brows styles\sizes for the selected account.
*!*************************************************************
*! Example            :  lfBrows()
*!*************************************************************
FUNCTION lfBrows

SELECT IIF(laScrMode[1] OR laScrMode[2],'PACKMTRX',lcPackTmp)
IF WEXIST(lcStyWind)
  RELEASE WINDOW (lcStyWind)
ENDIF
lnMarker = RECNO()
BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,;
       cStyle      :H = lcMajTitle      :R,;
       cMixSize    :H = 'Mix Size'   :R,;
       cMixStyle   :H = 'Mix Style'  :R,;
       nMaxSize1   :H = 'SIZ1'       :R,;
       nMaxSize2   :H = 'SIZ2'       :R,;
       nMaxSize3   :H = 'SIZ3'       :R,;
       nMaxSize4   :H = 'SIZ4'       :R,;
       nMaxSize5   :H = 'SIZ5'       :R,;
       nMaxSize6   :H = 'SIZ6'       :R,;
       nMaxSize7   :H = 'SIZ7'       :R,;
       nMaxSize8   :H = 'SIZ8'       :R;
       FOR IIF(laScrMode[1] OR laScrMode[2],.T.,INLIST(cStatus,'A','S','M')) AND cAccount = laData[1]  ;
       WINDOW     (ICDAN122) ;
       IN WINDOW  (gcBaseWind) ;
       WHEN lfwBrow()    ;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       NOEDIT            ;
       LOCK 0            ;
       TITLE lcStyWind   
       
=SEEK(laData[1])

*!*************************************************************
*! Name      : lfReadAct
*! DATE      : 24/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : Activate window function.
*!*************************************************************
*! Example            :  lfBrows()
*!*************************************************************
FUNCTION lfReadAct
=lfClearKey()
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowseTl)

*!*************************************************************
*! Name      : lfDActivate
*! DATE      : 24/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : Deactivate window function.
*!*************************************************************
*! Example            :  lfBrows()
*!*************************************************************
FUNCTION lfDActivate

IF WONTOP()=lcStyWind
  ON KEY LABEL CTRL+Q lnDummy = 1
  ON KEY LABEL CTRL+W lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL TAB DO lpTab WITH (ICDAN123),'laData[2]'
  ON KEY LABEL BACKTAB DO lpBackTab WITH IIF(laScrMode[2],(gwcContrl1),(ICDAN123)),IIF(laScrMode[2],'pbEdt','pbNew')
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lpTab 
*! DATE      : 24/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : Traping function.
*!*************************************************************
*! Example            :  lfBrows()
*!*************************************************************
PROCEDURE lpTab 
PARAMETERS lcWindName, lcObjName
ON KEY LABEL TAB 
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

PROCEDURE lpBackTab 
PARAMETERS lcWindName, lcObjName

ON KEY LABEL BACKTAB
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = OBJNUM(&lcObjName)

FUNCTION lfClearKey
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*!*************************************************************
*! Name      : lfwBrow
*! DATE      : 05/25/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To be proceesed when moving into the browse
*!*************************************************************
*! Example            :  lfwBrow()
*!*************************************************************
FUNCTION lfwBrow

SELECT IIF(laScrMode[1] OR laScrMode[2],'PACKMTRX',lcPackTmp)
=SEEK(cStyle,'STYLE')
lnMarker = RECNO()
SHOW WINDOW (lcStyWind) REFRESH SAME
lcStatus   = IIF(laScrMode[1] OR laScrMode[2] OR EOF(),'DISABLE','ENABLE')
laData[2]  = cMixSize
laData[3]  = cMixStyle
laData[4]  = cStyle
FOR lnCnt  = 1 TO 8
  lcCnt = STR(lnCnt,1)  
  laData[lnCnt+4] = nMaxSize&lcCnt
  SHOW GET laData[lnCnt+4] &lcStatus
ENDFOR  
SHOW GET laData[2] &lcStatus
SHOW GET laData[3] &lcStatus
SHOW GET ibStyle   DISABLE
SHOW GET laData[4] DISABLE

*********************************************************************************
*! Name      : lfvMulti
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate Muti button.
*!*************************************************************
FUNCTION lfvMulti

STORE 'Y' TO lcMovMixSz,lcMovMixSt
STORE ' ' TO laTarget,laSource
STORE 0   TO lnMovSize1,lnMovSize2,lnMovSize3,lnMovSize4,lnMovSize5,;
             lnMovSize6,lnMovSize7,lnMovSize8
DO (gcScrDir+gcWinAppl+"\ICDAN12B.SPX")
LOCATE FOR INLIST(cStatus,'A','S','M')
IF FOUND()
  SHOW GET pbRemove ENABLE
ENDIF

*********************************************************************************
*! Name      : lfvMvMxSt
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate multi mix style field.
*!*************************************************************
FUNCTION lfvMvMxSt

IF !INLIST(lcMovMixSt,'Y','N') 
  RETURN .F.
ENDIF

*********************************************************************************
*! Name      : lfvMvMxSz
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate multi size style field.
*!*************************************************************
FUNCTION lfvMvMxSz
IF !INLIST(lcMovMixSz,'Y','N')
  RETURN .F.
ENDIF

*********************************************************************************
*! Name      : lfvSelSty
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate SELECT STYLES button.
*!*************************************************************
FUNCTION lfvSelSty
DIMENSION laTarget[1],laSource[1]
SELECT DIST cStyMajor FROM STYLE INTO ARRAY laSource
lnRecCnt = _TALLY
IF lnRecCnt <>0
  = gfMover(@laSource,@laTarget,"Selected Styles",.T.)
ELSE
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'The style file is empty.')    
ENDIF  

*!*************************************************************
*! Name      : lfvProMulti
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To update the temp file in case of multi.
*!*************************************************************
*! Example            :  lfMulStyl()
*!*************************************************************
*!DESCRIPTION:
* The steps will be as follows:
*       *  1) If the new style does not exist at all ==> APPEND
*       *  2) If the new style exists:
*       *     A) See if there is any size = 0 for this account\style.
*       *     B) See if the new style is of the same setting for all the accoumt\found-style records.
*       *     C) IF A AND !B (size field or more=0 and the setting is new )==> APPEND 
*       *     D) IF A AND B  (size field or more=0 But there is a record entered before 
*       *        with the same setting )==> UPDATE THE ORIGINAL RECORD WITH THE NEW 
*       *        QUANTITIES FOR THE ZERO-VALUE SIZES ONLY.
*       *     E) IF !B AND A (new setting and zero-value size or more)==> APPEND FOR 
*       *        THOSE ZERO-VALUE ONLY.
*       *     F) IF !B AND !A (new setting But no zero-value sizes) ==>LOOP. (llMsg = .T.)
*       *     G) IF !A AND(B OR !B) (no size fields=0)==> LOOP. (llMsg = .T.)
*!*************************************************************************
FUNCTION lfvProMulti
PRIVATE lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8
PRIVATE lnSizeCnt,lcSizeCnt,llUpdate,llMsg

lnMovTot = lnMovSize1+lnMovSize2+lnMovSize3+lnMovSize4+lnMovSize5+lnMovSize6+lnMovSize7+lnMovSize8
IF lnMovTot = 0
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No quantities in max packs sizes! Cannot proceed.')    
  _CUROBJ=OBJNUM(lnMovSize1)
  RETURN
ENDIF    
IF EMPTY(laTarget)
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'No styles selected! Cannot proceed.')    
  _CUROBJ=OBJNUM(pbSelSty)
  RETURN
ENDIF
STORE .F. TO llMsg,llUpdate
SELECT (lcPackTmp)
FOR lnStyCnt = 1 TO ALEN(laTarget)
  *---  STEP (1)
  IF !SEEK(laData[1]+PADR(laTarget[lnStyCnt],lnMajorLen))
    lnLineNo = 01
    =lfReplace(.T.,PADR(laTarget[lnStyCnt],lnMajorLen))
  ELSE && If the style was entered before.
    *--- STEP (2)==>(A)
    SUM REST nMaxsize1,nMaxsize2,nMaxsize3,nMaxsize4,nMaxsize5,nMaxsize6,nMaxsize7,nMaxsize8;
        TO lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8;
        WHILE cAccount+cStyle = laData[1]+PADR(laTarget[lnStyCnt],lnMajorLen)
    llZeroTot = (lnTotal1= 0 OR lnTotal2= 0 OR lnTotal3= 0 OR lnTotal4= 0 OR ;
                 lnTotal5= 0 OR lnTotal6= 0 OR lnTotal7= 0 OR lnTotal8= 0)
    =SEEK(laData[1]+PADR(laTarget[lnStyCnt],lnMajorLen))
    *--- STEP (2)==>(B)
    LOCATE REST FOR cMixSize = lcMovMixSz AND  cMixStyle = lcMovMixSt ;
                WHILE cAccount+cStyle = laData[1]+PADR(laTarget[lnStyCnt],lnMajorLen)
    IF FOUND()
    *---   IF 2==>B
    *---   Same setting
      IF llZeroTot
      *---   IF 2==>D ======> UPDATE ZERO-VALUES ONLY.
      *---   Same setting and zero-values fields
        =lfReplace(.F.,'')
      ELSE
        *--- IF 2==>D
        *--- Same setting and no zero-values fields
        *--- Update the message flag and get the next style. 
        llMsg = .T.
        LOOP  
      ENDIF    
    ELSE
      *--- IF !B
      *--- Setting is new
      =SEEK(laData[1]+PADR(laTarget[lnStyCnt],lnMajorLen))
      IF llZeroTot
      *--- IF 2==>E
      *--- new setting and zero-values fields ==> Append for these zero-values fields only.
        SCAN REST WHILE cAccount+cStyle = laData[1]+PADR(laTarget[lnStyCnt],lnMajorLen)
        ENDSCAN
        SKIP -1
        lnLineNo = nLineNo+1
        =lfReplace(.T.,PADR(laTarget[lnStyCnt],lnMajorLen))
      ELSE
        *--- IF 2==>F
        *--- (new setting But no zero-value sizes)
        *--- Update the message flag and get the next style. 
        llMsg = .T.
        LOOP
      ENDIF  
    ENDIF   
  ENDIF  
ENDFOR
IF llMsg
  = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'One or more Styles has not been Added.')    
ENDIF
*--adel Browse th new fields
=lfBrows()
CLEAR READ

*!*************************************************************
*! Name      : lfReplace
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To update the temp file.
*!*************************************************************
*! Example            :  lfReplace()
*!*************************************************************
FUNCTION lfReplace
PARAMETERS llAll,lcHoldSty
IF llAll
  APPEND BLANK
  REPLACE cAccount  WITH laData[1],;
          cStyle    WITH lcHoldSty,;
          nLineNo   WITH lnLineNo,;
          cMixSize  WITH lcMovMixSz,;
          cMixStyle WITH lcMovMixSt,;
          cStatus   WITH 'A'
  =SEEK(cStyle,'STYLE')
  FOR lnSizeCnt  = 1 to SCALE.CNT
    lcSizeCnt = STR(lnSizeCnt,1)
    REPLACE nMaxSize&lcSizeCnt WITH lnMovSize&lcSizeCnt
  ENDFOR  
ELSE
  REPLACE cStatus WITH IIF(cStatus='A',cStatus,'M')
  =SEEK(cStyle,'STYLE')
  FOR lnSizeCnt  = 1 to SCALE.CNT
    lcSizeCnt = STR(lnSizeCnt,1)
    IF lnTotal&lcSizeCnt = 0
      REPLACE nMaxSize&lcSizeCnt WITH lnMovSize&lcSizeCnt
    ENDIF  
  ENDFOR
ENDIF

*********************************************************************************
*! Name      : lfvSelect
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To get into select mode.
*!*************************************************************
FUNCTION lfvSelect

STORE ' ' TO laData[2],laData[3],laData[4],laData[1]
STORE 0 TO laData[5],laData[6],laData[7],laData[8],laData[9],laData[10],laData[11],laData[12]
STORE .F. TO laScrMode
laScrMode[1] = .T.
SHOW GETS

********************************************************************************
*! Name      : lfvDelete
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To validate delete acct. button.
*!*************************************************************
PROCEDUR lpDelScr

SELECT PACKMTRX
IF SEEK(laData[1])
  SCAN FOR caccount+cstyle+STR(nlineno,6) = laData[1]
    BLANK
    DELETE
  ENDSCAN
ENDIF  
=lfvSelect()

*!*************************************************************
*! Name      : lpShow
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To show objects.
*!*************************************************************
*! Example            :  lpShow()
*!*************************************************************
PROCEDUR lpShow

DO CASE
  CASE laScrMode[1] 
    STORE ' ' TO laData[2],laData[3],laData[4],laData[1]
    STORE 0 TO laData[5],laData[6],laData[7],laData[8],laData[9],laData[10],laData[11],laData[12]
    SHOW GETS WINDOW (ICDAN121) ENABLE ONLY
    SHOW GETS WINDOW (ICDAN123) DISABLE ONLY
    _CUROBJ = OBJNUM(laData[1])
    lcAccName = ' '
    =lfRefresh()
  CASE laScrMode[2] 
    SHOW GETS WINDOW (ICDAN121) DISABLE ONLY
    SHOW GETS WINDOW (ICDAN123) DISABLE ONLY
    SHOW GET ibMatrix ENABLE
  CASE laScrMode[4] 
    SHOW GETS WINDOW (ICDAN121) DISABLE ONLY
    SHOW GETS WINDOW (ICDAN123) DISABLE ONLY
    SHOW GET ibMatrix ENABLE
    SHOW GET pbNew    ENABLE
    SHOW GET pbMuti   ENABLE
    SHOW GET pbRemove DISABLE
    =lfBrAcct()
    _CUROBJ = OBJNUM(pbNew)
  CASE laScrMode[3]
    SHOW GETS WINDOW (ICDAN121) DISABLE ONLY
    SHOW GETS WINDOW (ICDAN123) ENABLE ONLY    
    SHOW GET ibMatrix ENABLE
    =lfBrAcct()
    _CUROBJ = OBJNUM(pbNew)
    llCUpDate = .T.
ENDCASE  
=lfBrows()

*!*************************************************************
*! Name      : lfBrAcct
*! DATE      : 22/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To fill the temp file with the related fields.
*!*************************************************************
*! Example            :  lfBrAcct()
*!*************************************************************
FUNCTION lfBrAcct

PRIVATE lcCurOrd
SELECT PACKMTRX
lcCurOrd = ORDER()
SET ORDER TO
SELECT * , 'S' AS cStatus,RECNO() AS nRecNo FROM PACKMTRX WHERE PACKMTRX.cAccount = laData[1] INTO DBF (gcWorkDir+lcPackTmp)
INDEX ON cAccount+cStyle+STR(nLineNo,6) TAG  (lcPackTmp)
SET ORDER TO &lcCurOrd IN PACKMTRX

*********************************************************************************
*! Name      : lfvActBrow
*! DATE      : 05/25/98
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To Browse the accounts.
*!*************************************************************
FUNCTION lfvActBrow
PARAMETERS lcObjName

llBrowse = .T.
_CUROBJ = OBJNUM(&lcObjName)
KEYBOARD CHR(13)

*********************************************************************************
*! Name      : lpSavScr
*! DATE      : 24/05/2000
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Purpose   : To Trap ESC button.
*!*************************************************************
PROCEDURE lpClose

_CUROBJ = OBJNUM(pbCls)
KEYBOARD '{ENTER}'



FUNCTION lfChckSet

IF SEEK(laData[1]+laData[4])
  *--Locate for a previously added account\stle with the same setting.
  LOCATE REST FOR cMixSize = laData[2] AND  cMixStyle = laData[3];
         WHILE cAccount+cStyle = laData[1]+laData[4] AND INLIST(cStatus,'A','S','M')
  IF FOUND()
    *--- Focus on the record if exists.
    lnRecNo  = RECNO()
    llUpdate = .F.
    llRep    = .F.
  ELSE
    IF SEEK(laData[1]+laData[4])
      SUM REST nMaxsize1,nMaxsize2,nMaxsize3,nMaxsize4,nMaxsize5,nMaxsize6,nMaxsize7,nMaxsize8;
          TO lnTotal1,lnTotal2,lnTotal3,lnTotal4,lnTotal5,lnTotal6,lnTotal7,lnTotal8;
          WHILE cAccount+cStyle = laData[1]+laData[4] ;
          FOR INLIST(cStatus,'A','S','M')
      IF !(lnTotal1= 0 .OR. lnTotal2= 0 .OR. lnTotal3= 0 .OR. lnTotal4= 0 .OR. lnTotal5 = 0 .OR. lnTotal6 = 0 .OR. lnTotal7 = 0 .OR. lnTotal8 = 0)
        = gfModalGen('TRM00000B00000',.F.,.F.,.F.,'All sizes max packs already exist for this style.')    
        llUpdate = .F.
        llRep    = .F.
      ENDIF
    ENDIF
  ENDIF
ENDIF  
