*:************************************************************************
*: Procedure file: SMCURR.PRG
*:         System: ARIA.
*:         Module: Spec. Sheet Model.
*:         Author: Mohamed Fahmy Mohamed (MFM).
*:      Copyright (c) 
*:  Last modified: 11/12/1994
*:  Procs & Fncts: 
*:
*:      Documented 11/12/1994
*:************************************************************************
*B600871,1 M.H 12/10/95 The currency unit cann't be zero.  I added a when,valid functions in ther screen called SMCURENT.
*B600803,1 M.H 01/09/96 lfwBrwWhen.PRG Could not be found.
*B601012,1 RENEE 03/31/96 Correct the spelliung of 'Symble' in the message
*B601012,1                'Cannot save with an empty symble' (M
*B601503,1 HS 12/22/96 Correct the calling of the gfModalGen() to make  
*B601503,1             the message appear 
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*B603383,1 WMA On 04/09/2000
*B603383,1 WMA Currency codes 3 Char 
*:************************************************************************

lcBrowTtl  = "Currency"
lcBrowWind = " "
lcContWind = " "
lcCurExcept= ''
lcColor    = IIF( _DOS, "SCHEME 5", "RGB(,,,192,192,192)" )
lnCurrRec  = 0
lnFirstClk = 0
llNoContrl = .T.
llFirstEnt = .T.
llEdit     = .F.
lnOldCurUnt= 0

DIMENSION laScObj[1,5]
IF !gfSetup()
  RETURN
ENDIF

llClick=.F.
STORE 0 TO lnTimelimt,lnCurR
lnLineRange=IIF(_WINDOWS,1.77,2)
laCtrStat[10] = 'DISABLE'
IF !WEXIST(gcBaseWind)
  lcBrowWind = gfTempName()
  lcContWind = gfTempName()  
ENDIF

PUSH KEY 
ON KEY
ON KEY LABEL LEFTMOUSE  DO lfDclick WITH CHR(13) 
ON KEY LABEL TAB        DO lfKeyTrap
ON KEY LABEL BACKTAB    DO lfKeyTrap
ON KEY LABEL CTRL+W     lnDummy = 1
ON KEY LABEL Ctrl+Q     lnDummy = 1
ON KEY LABEL CTRL+HOME  lnDummy = 1 
ON KEY LABEL CTRL+END   lnDummy = 1
ON KEY LABEL ESC        DO lfKeyTrap
ON KEY LABEL INS        DO lfKeyTrap
ON KEY LABEL DEL        DO lfKeyTrap
IF _DOS
  ON KEY LABEL N          DO lfKeyTrap WITH 'N'
  ON KEY LABEL R          DO lfKeyTrap WITH 'R'
  ON KEY LABEL Ctrl+ENTER DO lfKeyTrap WITH 'C'
ENDIF  

SET REFRESH TO 1

*** In case of DOS only define a pad in the menu for Browse. ***
IF _DOS
  DEFINE PAD _BROWSE OF _MSYSMENU PROMPT '\<Browse' KEY ALT+B
  ON SELECTION PAD _BROWSE OF _msysmenu ACTIVATE WINDOW (lcBrowTtl)
ELSE
  DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
  ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTtl)  
ENDIF  

*E300683,4 Call *.SPR from screens directory
* DO SMCURR.SPR 
DO (gcScrDir + gcWinAppl + '\SMCURR.SPR')
CLEAR TYPEAHEAD
*E300683,4 end   

*** Release the browse pad from the menu. ***
RELEASE PAD _BROWSE OF _MSYSMENU
RELEASE BAR 100 OF P01PU01 
RELE WINDOW (lcBrowTtl)
SET REFRESH TO 0

ON KEY 
POP KEY

IF glQuitting
  ** In Order to prevent the flicker.
  HIDE WINDOW (gcBaseWind)
  RELEASE WINDOW (lcBrowTtl)
ENDIF

*!**************************************************************************
*!
*!      Function: lfBrowse
*!
*!**************************************************************************
*
FUNCTION lfBrowse
SELECT (lcBaseFile)
GO TOP
lnCurrRec = RECNO()
lcClrSchm = IIF(_DOS,"COLOR SCHEME 13",[FONT "FOXFONT",9])

*** cDummi is a dummi filed evaluated to display the arrow for the current rec.
BROWSE FIELDS cDummi=IIF(lnCurrRec=RECNO(),""," ") :V=lfEditRec() :F :H=" ",cCurrCode :H="Code",cCurrDesc :H="Description" :30,;
              nCurrUnit :H="Units",cCurrSmbl :H="Symbol",lcCurMeth = IIF(cCurMeth = 'M','Multiply','Divide') :H="Exch. Method" ;
              NOAPPEND  NOEDIT  SAVE     ;
              NOWAIT    LOCK 0  NOCLEAR  ;
              FREEZE cDummi ; 
              TITLE lcBrowTtl ;
              NODELETE ;
              WHEN  lfwBrwWhen() ;
              VALID :F lfvBrwVald();
              WINDOW (lcBrowWind) IN WINDOW (gcBaseWind);
              &lcClrSchm

*!**************************************************************************
*!
*!      Function: lfwBrwWhen
*!
*!**************************************************************************
*B600803,1 M.H 01/09/96 lfwBrwWhen.PRG Could not be found.
*
FUNCTION lfwBrwWhen
glFromBrow = .T.
KEYBOARD "{RIGHTARROW}"
lnCurrRec  = RECNO() 
SHOW WINDOW (lcBrowTtl) REFRESH

*!**************************************************************************
*!
*!      Function: lfvBrwVald
*!
*!**************************************************************************
*
FUNCTION lfvBrwVald
*** In case of DOS only define a pad in the menu for Browse. ***

IF !WONTOP(lcBrowTtl)
  IF _DOS
    DEFINE PAD _BROWSE OF _MSYSMENU PROMPT '\<Browse' KEY ALT+B
    ON SELECTION PAD _BROWSE OF _msysmenu ACTIVATE WINDOW (lcBrowTtl)
  ELSE
    DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
    ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTtl)      
  ENDIF  
  =gfStopBrow()
ELSE
  IF !WEXIST(lcContWind) 
    glFromBrow = .F.
    glQuitting = .T.
    CLEAR READ
    KEYBOARD CHR(13)
    RETURN TO SMCURR.SPR
  ENDIF
ENDIF    

*!**************************************************************************
*!
*!      Function: lfEntrBrow
*!
*!**************************************************************************
* Called from the activate at the screen.
FUNCTION lfEntrBrow

glFromBrow = .T.
RELEASE PAD _BROWSE OF _MSYSMENU
RELEASE BAR 100 OF P01PU01 
RETURN .T.

*!**************************************************************************
*!
*!      Function: lfEditRec
*!
*!**************************************************************************
* Edit the desc. for the current avilable record.
FUNCTION lfEditRec

IF LASTKEY() = 13 .AND. WONTOP() = (lcBrowTtl) .OR. llEdit
  IF gfObj_Lock(.T.)
    PUSH KEY
    ON KEY 
    lcCode     = cCurrCode
    lcDescrip  = cCurrDesc
    lcSymble   = cCurrSmbl    
    lnCurrUnit = nCurrUnit
    rbExBCur   = IIF(cCurMeth = 'M',1,2)
    llNewRec   = .F.

    *E300683,4 Call *.SPR from screens directory
    * DO SMCURENT.SPR 
    DO (gcScrDir + gcWinAppl + '\SMCURENT.SPR')
    *E300683,4 end  
    
    =gfObj_Lock(.F.)
    
    POP KEY

    SHOW WINDOW (lcBrowTtl) REFRESH
  ENDIF
ENDIF  

llEdit = .F.

*!**************************************************************************
*!
*!      Function: lfvOk
*!
*!**************************************************************************
* 
FUNCTION lfvOk

IF EMPTY(lcDescrip)
  =gfModalGen('QRM00244B00000','Dialog','Description')      && No empty desc.
  IF !llNewRec   
    lcDescrip = ccurrdesc
    SHOW GET lcDescrip
  ENDIF   
  _CUROBJ = OBJNUM(lcDescrip)
  RETURN
ENDIF  

IF EMPTY(lcSymble) 
  *B601012,1 Correct the spelling of 'Symble'. 
  *=gfModalGen('QRM00244B00000','Dialog','Symble')      && No empty desc.
  =gfModalGen('QRM00244B00000','Dialog','Symbol')      
  *B601012,1 end.
  IF !llNewRec   
    lcSymble   = ccurrsmbl
    lnCurrUnit = nCurrUnit
    SHOW GET lcSymble
    SHOW GET lnCurrUnit
  ENDIF   
  _CUROBJ = OBJNUM(lcSymble)
  RETURN
ENDIF  

IF llNewRec 
  llNewRec   = .F.
  APPEND BLANK
  REPLACE cCurrCode  WITH lcCode    ;
          cCurrSmbl  WITH lcSymble  ;
          nCurrUnit  WITH lnCurrUnit;
          ccurrdesc  WITH lcDescrip ;
          cCurMeth   WITH IIF(rbExBCur = 1,'M','D');
          MCUREXCEPT WITH lcCurExcept

  IF !SEEK(lcCode+lcCode,'SYCEXCH')
    INSERT INTO SYCEXCH (CBASECURR,CCURRCODE,NEXRATE) VALUES (lcCode,lcCode,1)
  ENDIF  
ELSE
  REPLACE ccurrdesc  WITH lcDescrip ;
          cCurrSmbl  WITH lcSymble  ;
          nCurrUnit  WITH lnCurrUnit;
          cCurMeth   WITH IIF(rbExBCur = 1,'M','D');
          MCUREXCEPT WITH lcCurExcept
ENDIF

= gfAdd_Info()

CLEAR READ

*!**************************************************************************
*!
*!      Function: lfwNew
*!
*!**************************************************************************
*
FUNCTION lfwNew

IF llFirstEnt 
  llFirstEnt = .F.
  KEYBOARD "{ALT+B}"
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvNew
*!
*!**************************************************************************
*
FUNCTION lfvNew

CLEAR TYPEAHEAD
PUSH KEY
ON KEY 

*B603383,1 WMA On 04/09/2000    *** Begin ***
*B603383,1 Currency Code 3 Char
*lcCode     = SPACE(04)
lcCode     = SPACE(03)
*B603383,1 WMA On 04/09/2000    *** End   ***
lcDescrip  = SPACE(30)
lcSymble   = SPACE(3)
lnCurrUnit = 1
rbExBCur   = 1
llNewRec   = .T.
llAbort    = .F.

*E300683,4 Call *.SPR from screens directory
* DO SMCURENT.SPR 
DO (gcScrDir + gcWinAppl + '\SMCURENT.SPR')
*E300683,4 end  
POP KEY

** Jump to the browse window. **
IF WONTOP() <> lcBrowTtl
  KEYBOARD "{Alt+B}"
ENDIF

*!**************************************************************************
*!
*!      Function: lfvCode 
*!
*!**************************************************************************
* Valid the code to privent the duplication of records.
*
FUNCTION lfvCode

IF !EMPTY(lcCode) .AND. LASTKEY() = 13
  IF SEEK(PADR(ALLTRIM(lcCode),5))
    llNewRec   = .F.
    lcDescrip  = ccurrdesc 
    lcSymble   = cCurrSmbl
    lnCurrUnit = nCurrUnit
    rbExBCur   = IIF(cCurMeth = 'M',1,2)
    
    SHOW GET lcCode     DISABLE
    SHOW GET lcDescrip  ENABLE
    SHOW GET lcSymble   ENABLE
    SHOW GET lnCurrUnit ENABLE
    SHOW GET rbExBCur   ENABLE
    SHOW GET pbExcept   ENABLE
  ELSE
    SHOW GET lcCode     DISABLE
    SHOW GET lcDescrip  ENABLE
    SHOW GET lcSymble   ENABLE            
    SHOW GET lnCurrUnit ENABLE                
    SHOW GET rbExBCur   ENABLE
    SHOW GET pbExcept   ENABLE
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvDesc
*!
*!**************************************************************************
* Valid the entered desc no empty desc. is allowded.
*
FUNCTION lfvDesc

IF EMPTY(lcDescrip)
  =gfModalGen('QRM00244B00000','Dialog','Description')  && No empty desc.
  IF !llNewRec   
    lcDescrip = ccurrdesc
    SHOW GET lcDescrip
  ENDIF   
  _CUROBJ = OBJNUM(lcDescrip)
  RETURN
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvSymble
*!
*!**************************************************************************
* Valid the entered Symble no empty desc. is allowded.
*
FUNCTION lfvSymble

IF EMPTY(lcSymble) 
  *B601012,1 Correct the spelling of 'Symble'. 
  *=gfModalGen('QRM00244B00000','Dialog','Symble')      && No empty desc.

*B601503,1 Change this line [Begin]
  *=gfModalGen('TRTM00244B00000','Dialog','Symbol')      && No empty desc.
  =gfModalGen('TRM00244B00000','Dialog','Symbol')      && No empty desc.
*B601503,1 Change this line [End]

  *B601012,1 end.
  IF !llNewRec   
    lcSymble   = ccurrsmbl
    lnCurrUnit = nCurrUnit
    SHOW GET lcSymble
    SHOW GET lnCurrUnit
  ENDIF   
  _CUROBJ = OBJNUM(lcSymble)
  RETURN
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvRemove
*!
*!**************************************************************************
* Remove the current record.
*
FUNCTION lfvRemove

CLEAR TYPEAHEAD
** Check if the mesurament is been used in the gade rule then
** don't delete.
IF !EOF()
  PRIVATE lcAlias,llFound,lcCode
  lcCode = CCURRCODE
  lcAlias = SELECT()
  *E301077,78 Hesham (Start)
  =gfOpenFile(gcSysHome+'SYCINT')
  *IF !USED('SYCINT')
  *  USE (gcSysHome+'SYCINT') IN 0
  *ENDIF
  *SELECT SYCINT
  *E301077,78 Hesham (Start)

  LOCATE FOR ccurrcode=lcCode
  llFound = FOUND()
  SELECT (lcAlias)

  IF llFound OR lcCode = 'USDLR' OR lcCode = 'STRLN'
    ** 'You cannot delete this record' message
    =gfModalGen('INM00242B00000','ALERT',IIF(lcCode = 'USDLR' OR lcCode = 'STRLN',;
    'This record has been defined by the system.','This Code is currently used by one or more international(s).'))
  ELSE
    IF gfModalGen('QRM00007B00007','Alert') = 1      && Confirm to delete.
      IF gfObj_Lock(.T.)
        SCATTER MEMVAR BLANK
        GATHER MEMVAR
        DELETE
      ENDIF  
    ENDIF
  ENDIF

  ** Jump to the browse window. **
  IF WONTOP() <> lcBrowTtl
    SHOW WINDOW (lcBrowTtl) REFRESH 
    KEYBOARD "{Alt+B}" CLEAR 
  ENDIF  
ENDIF

*!**************************************************************************
*!
*!      Function: lfKeyTrap
*!
*!**************************************************************************
* Trap all required key board keys to control the browse.
*
FUNCTION lfKeyTrap
PARAMETERS lcWhichBut

IF _DOS
  HIDE MENU _MSYSMENU 
  SHOW MENU _MSYSMENU          
ENDIF  

IF PARAM()<>0
  DO CASE
    CASE lcWhichBut='N'
      =lfvNew()
    CASE lcWhichBut='R'
      =lfvRemove()
    CASE lcWhichBut='E'
      =lfEditRec()
    CASE lcWhichBut='C'
       ACTIVATE WINDOW (lcContWind) 
       _CUROBJ= OBJNUM(pbClose) 
       KEYBOARD "{SPACEBAR}"
  ENDCASE
  RETURN
ENDIF

DO CASE 
  CASE LASTKEY() = 9                   && TAB
    DO CASE
      CASE WONTOP() = lcBrowTtl
        _CUROBJ = OBJNUM(pbNew)
        ACTIVATE WINDOW (lcContWind) 
      CASE _CUROBJ = OBJNUM(pbClose)
        ACTIVATE WINDOW (lcBrowTtl)
      OTHERWISE 
        _CUROBJ = _CUROBJ + 1        
    ENDCASE      

  CASE LASTKEY() = 15                 && BAKTAB
    DO CASE
      CASE WONTOP() = lcBrowTtl
        _CUROBJ = OBJNUM(pbClose)
        ACTIVATE WINDOW (lcContWind) 
      CASE _CUROBJ = OBJNUM(pbNew)
        ACTIVATE WINDOW (lcBrowTtl)
      OTHERWISE 
        _CUROBJ = _CUROBJ - 1        
    ENDCASE      

  CASE LASTKEY() = 22                && INS
    =lfvNew()

  CASE LASTKEY() = 7                 && DEL 
    =lfvRemove()

  CASE LASTKEY() = 27                && ESC
    ACTIVATE WINDOW (lcContWind) 
    _CUROBJ= OBJNUM(pbClose) 
    KEYBOARD "{SPACEBAR}"  
ENDCASE
   
*!******************************************************************
*!
*!              Function: lfDclick
*!
*!******************************************************************
*
FUNCTION lfDclick
PARAMETERS lcKeyLable

** Coordination Array **
lnX =INT(MROW())
lnY =INT(MCOL())

IF  (MROW(lcBrowTtl)<>-1) AND (MCOL(lcBrowTtl)<>-1) AND (MROW(lcBrowTtl)>lnLineRange)
  IF !llClick
    lnTimelimt = SECONDS()  
    lnCurR     = lnX
    llClick    = .T.
  ELSE
    IF SECONDS() < lnTimelimt + _DBLCLICK .AND. lnCurR  = INT(MROW()); 
       AND  lnX=lnCurR &&INT(ABS(MROW(lcBrowTtl)-lnCurR))=0
      llClick    = .F.
      KEYBOARD lcKeyLable CLEAR  
    ENDIF
    IF lnCurR    = INT(MROW())
      lnTimelimt = SECONDS()  
    ELSE
      llClick    = .F.
    ENDIF   
  ENDIF
ELSE
  llClick    = .F.
  lcObjName=lfGetObj(lnX,lnY)
  IF !EMPTY(lcObjName)
    _CUROBJ=OBJNUM(&lcObjName)
    KEYBOARD "{ENTER}" PLAIN CLEAR
  ELSE
    CLEAR TYPEAHEAD  
  ENDIF
ENDIF

*!******************************************************************
*!
*!              Function: lfGetObj
*!
*!******************************************************************
*
FUNCTION lfGetObj
PARAMETER lnX,lnY

lcObj = ''
FOR lnCount=1 TO ALEN(laScObj,1)
  IF BETWEEN(lnX,laScObj[lnCount,1],laScObj[lnCount,3]) AND BETWEEN(lnY,laScObj[lnCount,2],laScObj[lnCount,4])
    lcObj=laScObj[lnCount,5]
    EXIT
  ENDIF
ENDFOR
RETURN lcObj

*!******************************************************************
*!
*!              Function: lfShow
*!
*!******************************************************************
*
FUNCTION lfShow

IF llNewRec 
  SHOW GET lcCode     ENABLE
  SHOW GET lcDescrip  DISABLE
  SHOW GET lcSymble   DISABLE
  SHOW GET lnCurrUnit DISABLE
  SHOW GET rbExBCur   DISABLE
  SHOW GET pbExcept   DISABLE
ELSE
  SHOW GET lcCode     DISABLE
  SHOW GET lcDescrip  ENABLE
  SHOW GET lcSymble   ENABLE
  SHOW GET lnCurrUnit ENABLE
  SHOW GET rbExBCur   ENABLE
  SHOW GET pbExcept   ENABLE
ENDIF

*!******************************************************************
*!
*!              Function: lfvExcept
*!
*!******************************************************************
*
FUNCTION lfvExcept

STORE ALLTRIM(MCUREXCEPT) TO lcOldExcept,lcCurExcept

DECLARE laExceptFld[1],laCurrency[1],laScrMode[4]
STORE '' TO laCurrency, laExceptFld
laScrmode = .F.

lcCurrCode = cCurrCode

lnRecNo = RECNO()

SELECT CCURRDESC+' '+CCURRCODE AS 'CCURRCODE';
  FROM SYCCURR;
 WHERE SYCCURR.CCURRCODE <> lcCurrCode;
  INTO ARRAY laCurrency

IF !EMPTY(lcCurExcept)
  =gfSubStr(lcCurExcept,@laExceptFld,"|")
  SELECT SYCCURR

  lnExcept = ALEN(laExceptFld,1)

  FOR lnCount = 1 TO lnExcept
    IF SEEK(laExceptFld[lnCount])
      laExceptFld[lnCount] = CCURRDESC+' '+CCURRCODE
    ELSE
      =ADEL(laExceptFld,lnCount)
      IF ALEN(laExceptFld) > 1
        DIMENSION laExceptFld[ALEN(laExceptFld)-1]
      ELSE
        DIMENSION laExceptFld[1]
      ENDIF  
    ENDIF
  ENDFOR
  SELECT SYCCURR
ENDIF  
laScrMode[3] = .T.

IF !llNewRec
  GO lnRecNo
ENDIF

=gfMover(@laCurrency,@laExceptFld,"Select the exception currencies")

IF !EMPTY(laExceptFld[1])
  lcFieldstr   = ''
  lcCurExcept  = ''
  FOR lnCount  = 1 TO ALEN(laExceptFld,1)
    lcFieldstr = lcFieldstr + IIF(EMPTY(lcFieldstr),'','|')+;
                 ALLTRIM(SUBSTR(laExceptFld[lnCount],32,6))
  ENDFOR
  lcCurExcept = lcFieldstr
ELSE
  lcCurExcept = ' '
ENDIF    

*!******************************************************************
*!
*!              Function: lfvEdit
*!
*!******************************************************************
*
FUNCTION lfvEdit

llEdit = .T.

=lfEditRec()

*!******************************************************************
*!
*!              Function: lfwUnit
*!
*!******************************************************************
*B600871,1 M.H 12/10/95 The currency unit cann't be zero.  When function to save the old currency unit.
*
FUNCTION lfwUnit
lnOldCurUnt = lnCurrUnit

*!******************************************************************
*!
*!              Function: lfvUnit
*!
*!******************************************************************
*B600871,1 M.H 12/10/95 The currency unit cann't be zero.
*
FUNCTION lfvUnit
IF lnCurrUnit = 0
  lnCurrUnit = lnOldCurUnt
ENDIF
SHOW GET lnCurrUnit