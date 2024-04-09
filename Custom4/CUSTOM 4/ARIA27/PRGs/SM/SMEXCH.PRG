*:************************************************************************
*: Procedure file: SMEXCH.PRG
*:         System: ARIA.
*:         Module: SYSTEM MANAGER
*:         Author: HESHAM EL_SHELTAWI
*:      Copyright (c) 
*:  Last modified: 11/12/1994
*:  Procs & Fncts: 
*:               : lfwCurrency
*:               : lfvCurrency
*:               : lfBrowse
*:               : lfwBrwWhen
*:               : lfvBrwVald
*:               : lfEntrBrow
*:               : lfEditRec
*:               : lfwNew
*:               : lfvNew
*:               : lfvRemove
*:               : lfKeyTrap
*:               : lfDclick1
*:               : lfDclick
*:               : lfGetObj
*:               : lfCurFlter
*:               : lfvOrder
*:               : lfActTrap
*:               : lfvCurCode
*:               : lfvCurDate
*:               : lfvCurOk
*:               :
*:      Documented 11/12/1994
*:************************************************************************
*B600800,1 M.H 01/14/96 Fix the of the browse in the currency scope.
*B601101,1 HS 12/05/96  Fix the browses of the screen  
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*B601892,1 HS 12/02/97  1) Fix the problem [When you are in the
*B601892,1                 (Exchange rates to) field and press (Esc)]
*B601892,1                 The modifiecation made for this bug are in the 
*B601892,1                 screen SMEXCH.SCX [I added the key traping (The
*B601892,1                 Esc key traping) in the Screen Activate].
*B601892,1              2) Fix the problem of :
*B601892,1                 if the user has an active tool bar :
*B601892,1                 Run the screen, and close it. Run the screen 
*B601892,1                 again, the READ CYCLE is terminated, and we get
*B601892,1                 a sequence of error mesages.
*B603383,1 WMA On 04/09/2000
*B603383,1 WMA Fix the problem of opening the browse currency codes everytime 
*B603383,1 you enter a valid code 
*:************************************************************************

lcBrowTtl  = "Exchange Rates"
lcBrowWind = " "
lcContWind = " "
lcExchang  = " "
*B603383,1 WMA On 04/09/2000    *** Begin ***
*B603383,1 Currency Code 3 Char
*lcBaseCurr = SPACE(5)
lcBaseCurr = SPACE(3)
*B603383,1 WMA On 04/09/2000    *** End   ***
lcCurrDesc = ''
lcCurrency = ''
lcOldData  = ''
lcColor    = IIF( _DOS, "SCHEME 5", "RGB(,,,192,192,192)" )
llNoContrl = .T.
llFirstEnt = .T.
llBrowse   = .F.
ldDate     = {  /  /  }
lnCurrRec  = 0
lnFirstClk = 0
rbOrder  = 1

DIMENSION laScObj[1,5]
IF !gfSetup()
  RETURN
ENDIF

laCtrStat[10] = 'DISABLE'
llClick=.F.
STORE 0 TO lnTimelimt,lnCurR
lnLineRange=IIF(_WINDOWS,1.77,2)

IF !WEXIST(gcBaseWind)
  lcBrowWind = gfTempName()
  lcContWind = gfTempName()  
  lcExchang  = gfTempName()  
ENDIF

SET RELATION TO CCURRCODE INTO SYCCURR
PUSH KEY 
ON KEY

*** In case of DOS only define a pad in the menu for Browse. ***
IF _DOS
  DEFINE PAD _BROWSE OF _MSYSMENU PROMPT '\<Browse' KEY ALT+B
  ON SELECTION PAD _BROWSE OF _msysmenu ACTIVATE WINDOW (lcBrowTtl)
ELSE
  DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
  ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTtl)  
ENDIF  

*E300683,4 Call *.SPR from screens directory
* DO SMEXCH.SPR 
DO (gcScrDir + gcWinAppl + '\SMEXCH.SPR')
*E300683,4 end  

*B601892,1 If coming from browse, clear the browse
IF glFromBrow 
  =gfStopBrow()
ENDIF
*B601892,1 end

*** Release the browse pad from the menu. ***
RELEASE PAD _BROWSE OF _MSYSMENU
RELEASE BAR 100 OF P01PU01 
RELE WINDOW (lcBrowTtl)
SET RELATION TO
SET FILTER TO
ON KEY 
POP KEY

IF glQuitting
  *** In Order to prevent the flicker.
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
=lfCurFlter()
GO TOP
lnCurrRec = RECNO()
lcLen     = IIF(_DOS,'30','23')
lcClrSchm = IIF(_DOS,"COLOR SCHEME 13",[FONT "FOXFONT",9])
*lcClrSchm = [FONT "MS SANS SERIF",8]
*** cDummi is a dummi filed evaluated to display the arrow for the current rec.
BROWSE FIELDS cDummi=IIF(lnCurrRec=RECNO(),""," ") :V=lfEditRec() :F :H=" ",cCurrCode :H="Currency",;
       CDESC=IIF(EOF('SYCCURR'),'N/A',SYCCURR.CCURRDESC):&lcLen:H="Currency Description";
       ,dRateDate :H="Date" ,NExRate :H="Exch. Rate";
       NOAPPEND;
       NOEDIT  ;
       SAVE    ;
       NOWAIT  ;
       LOCK 0  ;
       NOCLEAR ;
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

** In case of DOS only define a pad in the menu for Browse. **
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
    RETURN TO SMEXCH.SPR
  ENDIF
ENDIF    

*!**************************************************************************
*!
*!      Function: lfEntrBrow
*!
*!**************************************************************************
* Called from the activate at the screen.
FUNCTION lfEntrBrow

*B601892,1 Set browsing flag only if going into the browse window.
*B601892,1 Move this line into the ELSE condition.
*glFromBrow = .T.
*B601892,1 end
IF !WONTOP(lcBrowTtl)
  ON KEY
ELSE  
  *B601892,1 Set browsing flag only if going into the browse window
  glFromBrow = .T.
  *B601892,1 end
  RELEASE PAD _BROWSE OF _MSYSMENU
  RELEASE BAR 100 OF P01PU01 
  =lfActTrap()
ENDIF  

RETURN .T.

*!**************************************************************************
*!
*!      Function: lfEditRec
*!
*!**************************************************************************
* Edit the desc. for the current avilable record.
FUNCTION lfEditRec

IF LASTKEY() = 13 .AND. WONTOP() = (lcBrowTtl)
  IF !EOF('SYCEXCH') AND gfObj_Lock(.T.)
    lcCurrCode = CCURRCODE
    ldCurDate  = DRATEDATE
    lnExRate   = NExRate
    =lfvNew()
    =gfObj_Lock(.F.)
  ENDIF
ENDIF

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
PARAMETERS llNew

CLEAR TYPEAHEAD
IF EMPTY(lcBaseCurr) AND llNew
  RETURN gfModalGen('QRM00245B00000','Dialog')
ENDIF

PUSH KEY CLEAR

IF llNew
  ldCurDate = DATE()
ENDIF

lcKeyStat  = IIF(llNew,'ENABLE','DISABLE')
PRIVATE lcKeyStat,lcFilter,lnRefr
lcFilter   = FILTER()
lnRefr     = SET('REFR')

SET FILTER TO

*E300683,4 Call *.SPR from screens directory
* DO SMUPEXCH.SPR 
DO (gcScrDir + gcWinAppl + '\SMUPEXCH.SPR')
*E300683,4 end  
SET FILTER TO &lcFilter

POP KEY

IF llNew AND WONTOP() <> lcBrowTtl
  SHOW WINDOW (lcBrowTtl) REFRESH 
  KEYBOARD "{Alt+B}" CLEAR 
ENDIF

*!**************************************************************************
*!
*!      Function: lfvRemove
*!
*!**************************************************************************
* Remove the current record.
FUNCTION lfvRemove

CLEAR TYPEAHEAD
** Check if the mesurament is been used in the gade rule then
** don't delete.
IF !EOF() 
 GO RECNO()
 IF !DELETED() 
   IF gfModalGen('QRM00007B00007','Alert') = 1      && Confirm to delete.
     IF gfObj_Lock(.T.)
       SCATTER MEMVAR BLANK
       GATHER MEMVAR
       DELETE
     ENDIF  
   ENDIF
 ELSE
   =gfModalGen('QRM00095B00000','Alert') 
 ENDIF 
  *** Jump to the browse window.
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
* Trap all required key board keys to control the browse
FUNCTION lfKeyTrap
PARAMETERS lcWhichBut

IF _DOS
  HIDE MENU _MSYSMENU 
  SHOW MENU _MSYSMENU          
ENDIF  

IF PARAM()<>0
  DO CASE
    CASE lcWhichBut='N'
      =lfvNew(.t.)
    CASE lcWhichBut='R'
      =lfvRemove()
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
      CASE _CUROBJ = OBJNUM(lcBaseCurr)
        ACTIVATE WINDOW (lcBrowTtl)      
      OTHERWISE 
        _CUROBJ = _CUROBJ + 1        
    ENDCASE      
  CASE LASTKEY() = 15                 && BAKTAB
    DO CASE
      CASE WONTOP() = lcBrowTtl
        _CUROBJ = OBJNUM(lcBaseCurr)
        ACTIVATE WINDOW (lcExchang) 
      CASE _CUROBJ = OBJNUM(pbNew)
        ACTIVATE WINDOW (lcBrowTtl)
      CASE _CUROBJ = OBJNUM(lcBaseCurr)        
        _CUROBJ = OBJNUM(pbClose)
      OTHERWISE 
        _CUROBJ = _CUROBJ - 1        
    ENDCASE      
  CASE LASTKEY() = 22                && INS
    =lfvNew(.T.)
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
FUNCTION lfDclick1
PARAMETERS lcKeyLable

****** Coordination Array ******
lnX =MROW()
lnY =MCOL()
IF  (MROW(lcBrowTtl)<>-1) AND (MCOL(lcBrowTtl)<>-1) AND (MROW(lcBrowTtl)>lnLineRange)
  IF !llClick
    *WAIT 'no click before' window
    lnTimelimt = SECONDS()  
    lnCurR     = INT(lnX)
    llClick    = .T.
  ELSE
    IF SECONDS() < lnTimelimt + _DBLCLICK .AND. lnCurR  = INT(lnX); 
      *AND  lnX=lnCurR &&INT(ABS(MROW(lcBrowTtl)-lnCurR))=0
      *wait 'click before' window 
      llClick    = .F.
      KEYBOARD lcKeyLable CLEAR  
    ENDIF
    IF lnCurR    = INT(lnX)
      lnTimelimt = SECONDS()  
    ELSE
      llClick    = .F.
    ENDIF   
  ENDIF
ELSE
  *wait 'not in browse window' window
  llClick    = .F.
  lcObjName=lfGetObj(lnX,lnY)
  IF !EMPTY(lcObjName)
    _CUROBJ=OBJNUM(&lcObjName)
    IF ! (LEFT(lcObjName,2) $ 'LCLD')
      KEYBOARD "{ENTER}" PLAIN CLEAR
    ENDIF  
  ELSE
    CLEAR TYPEAHEAD  
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfDclick
*!
*!**************************************************************************
*
FUNCTION lfDclick
PARAMETERS lcKeyLable

****** Coordination Array ******
lnX =INT(MROW())
lnY =INT(MCOL())
IF  (MROW(lcBrowTtl)<>-1) AND (MCOL(lcBrowTtl)<>-1) AND (MROW(lcBrowTtl)>lnLineRange)
  IF !llClick
    *wait 'no click before' window
    lnTimelimt = SECONDS()  
    lnCurR     = lnX
    llClick    = .T.
  ELSE
    IF SECONDS() < lnTimelimt + _DBLCLICK .AND. lnCurR  = INT(MROW()); 
       AND  lnX=lnCurR &&INT(ABS(MROW(lcBrowTtl)-lnCurR))=0
      *wait 'click before' window 
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
*  *wait 'not in browse window' window
*  llClick    = .F.
*  lcObjName=lfGetObj(lnX,lnY)
*  IF !EMPTY(lcObjName)
*    _CUROBJ=OBJNUM(&lcObjName)
*    IF ! (LEFT(lcObjName,2) $ 'LCLD')
*      KEYBOARD "{ENTER}" PLAIN CLEAR
*    ENDIF  
*  ELSE
*    CLEAR TYPEAHEAD  
*  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfGetObj
*!
*!**************************************************************************
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

*!**************************************************************************
*!
*!      Function: lfCurFlter
*!
*!**************************************************************************
*
FUNCTION lfCurFlter

IF rbOrder=1
  SET FILTER TO CBASECURR+CCURRCODE+DTOS(DRATEDATE)='' AND;
                CBASECURR=(lcBaseCurr) AND CCURRCODE=ALLT((lcCurrency));
                AND DTOS(DRATEDATE)=ALLT(DTOS((ldDate))) AND CBASECURR<>CCURRCODE
ELSE
  SET FILTER TO CBASECURR+DTOS(DRATEDATE)+CCURRCODE='' AND;
  CBASECURR=(lcBaseCurr) AND DTOS(DRATEDATE)=ALLT(DTOS((ldDate)));
             AND CCURRCODE=ALLT((lcCurrency)) AND CBASECURR<>CCURRCODE  
ENDIF

*!**************************************************************************
*!
*!      Function: lfvOrder
*!
*!**************************************************************************
*
FUNCTION lfvOrder

IF rbOrder=1
  SET ORDER TO CURRENCY
ELSE
  SET ORDER TO EXCHDATE
ENDIF  

=lfCurFlter()

*B601101,1 This line was added by HS [Begin]
GO TOP  
*B601101,1 This line was added by HS [End]

SHOW WINDOW (lcBrowTtl) REFRESH SAME

*!**************************************************************************
*!
*!      Function: lfActTrap
*!
*!**************************************************************************
*
FUNCTION lfActTrap

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

*!**************************************************************************
*!
*!      Function: lfvCurCode
*!
*!**************************************************************************
*
FUNCTION lfvCurCode

llBrowse = IIF(TYPE('llBrowse')='U',.F.,llBrowse) && variable to determine forcing browse or not

IF (!EMPTY(lcCurrCode) AND !SEEK(lcCurrCode,'SYCCURR')) OR llBrowse 
  PRIVATE lcBrFields,lcFile_ttl,lcSelect,lcRelation
  lcSelect = SELECT()
  lcRelation = SET('RELATION')
  SET RELATION TO  
  SELECT SYCCURR
  lcBrFields=gfDbfField('SYCCURR')
  DIMENSION laTemp[1]
  STORE '' TO laTemp
  lcFile_ttl    = "Select Currency"
  =gfBrows('FOR cCurrCode<>lcBaseCurr',"cCurrCode","laTemp")
  lcCurrCode = laTemp[1]
  SELECT (lcSelect)
  SET RELATION TO &lcRelation ADDI
  SHOW GET lcCurrCode
ENDIF

IF !EMPTY(lcCurrCode)
  lcSeekValue = IIF(rbOrder=1,lcCurrCode+DTOS(ldCurDate),DTOS(ldCurDate)+lcCurrCode)
  IF SEEK(lcBaseCurr+lcSeekValue)
    lnExRate = NExRate
    SHOW GET lnExRate
  ENDIF
ENDIF

*B601101,1 Reset the varible to .F. [Begin]
llBrowse = .F.
*B601101,1 Reset the varible to .F. [End]

*!**************************************************************************
*!
*!      Function: lfvCurDate
*!
*!**************************************************************************
*
FUNCTION lfvCurDate

IF !EMPTY(lcCurrCode)
  lcSeekValue = IIF(rbOrder=1,lcCurrCode+DTOS(ldCurDate),DTOS(ldCurDate)+lcCurrCode)
  IF SEEK(lcBaseCurr+lcSeekValue)
    lnExRate = NExRate
    SHOW GET lnExRate
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      Function: lfvCurOk
*!
*!**************************************************************************
*
FUNCTION lfvCurOk

IF llNew
  IF EMPTY(lcCurrCode) OR EMPTY(ldCurDate)
    _CUROBJ=IIF(EMPTY(lcCurrCode),1,2)
    RETURN gfModalGen('QRM00248B00000','Dialog',IIF(EMPTY(lcCurrCode),'Currency','Date'))    
  ENDIF
  lcSeekValue = IIF(rbOrder=1,lcCurrCode+DTOS(ldCurDate),DTOS(ldCurDate)+lcCurrCode)
  IF lnExRate<=0
    _CUROBJ=OBJNUM(lnExRate)
    RETURN gfModalGen('QRM00247B00000','Dialog')
  ENDIF  
  IF SEEK(lcBaseCurr+lcSeekValue)
    IF gfModalGen('QRM00246B00012','Dialog')=2
      _CUROBJ = OBJNUM(lcCurrCode)
      RETURN
    ENDIF
  ELSE  
    APPEND BLANK
    REPLACE cBaseCurr WITH lcBaseCurr,;
            cCurrCode WITH lcCurrCode,;
            DRATEDATE WITH ldCurDate
  ENDIF
ELSE
  IF lnExRate<=0
    _CUROBJ=OBJNUM(lnExRate)
    RETURN gfModalGen('QRM00247B00000','Dialog')
  ENDIF  
ENDIF
REPLACE nExRate WITH lnExRate
= gfAdd_Info()
CLEAR READ

*!**************************************************************************
*!
*!      Function: lfwBaseCurr
*!
*!**************************************************************************
*
FUNCTION lfwBaseCurr

lcOldData = lcBaseCurr
IF EMPTY(lcBaseCurr)
  lcCurrDesc = ' '
  SHOW GET rbOrder    DISABLE
  SHOW GET pbNew      DISABLE
  SHOW GET pbRemove   DISABLE
  SHOW GET lcCurrency DISABLE
  SHOW GET ibCurrency DISABLE
  SHOW GET ldDate     DISABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfvBaseCurr
*!
*!**************************************************************************
*
FUNCTION lfvBaseCurr

*B601101,1 This lines was added by HS [Begin]
*B601101,1 IF Statment to check if the field was empty 
IF EMPTY(lcBaseCurr) .AND. !llBrowse
   lcCurrDesc = ' '        && Clear the varible 
   lcCurrency = SPACE(5)   && Clear the varible
   ldDate     = {}         && Clear the varible
   SHOW GET rbOrder    DISABLE
   SHOW GET pbNew      DISABLE
   SHOW GET pbRemove   DISABLE
   SHOW GET lcCurrency DISABLE
   SHOW GET ibCurrency DISABLE
   SHOW GET ldDate     DISABLE
   =lfRefresh(lcExchang)     
   SHOW WINDOW (lcBrowTtl) REFRESH SAME
   RETURN
ENDIF       &&End of IF
*B601101,1 This lines was added by HS [End]
 
IF (llBrowse .OR. !SEEK(lcBaseCurr,'SYCCURR') .OR. ATC("?",lcBaseCurr) > 0 ) .AND. LASTKEY() = 13
  SELECT SYCCURR

  DIMENSION laTemp[2]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Select Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  
  =gfBrows('','CCURRCODE,CCURRDESC','laTemp')

  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    lcBaseCurr = lcOldData
  ELSE
    lcBaseCurr = laTemp[1]
    lcCurrDesc = laTemp[2]
  ENDIF
  =lfRefresh(lcExchang)
ELSE
  lcCurrDesc = LOOKUP(SYCCURR.CCURRDESC,lcBaseCurr,SYCCURR.CCURRDESC,'CCURRCODE')
  =lfRefresh(lcExchang)
ENDIF
llBrowse = .F.

IF EMPTY(lcBaseCurr)
  lcCurrDesc = ' '
  lcCurrency = SPACE(5)
  ldDate     = {}
  SHOW GET rbOrder    DISABLE
  SHOW GET pbNew      DISABLE
  SHOW GET pbRemove   DISABLE
  SHOW GET lcCurrency DISABLE
  SHOW GET ibCurrency DISABLE
  SHOW GET ldDate     DISABLE
ELSE
  SHOW GET rbOrder    ENABLE
  SHOW GET pbNew      ENABLE
  SHOW GET pbRemove   ENABLE
  SHOW GET lcCurrency ENABLE
  SHOW GET ibCurrency ENABLE
  SHOW GET ldDate     ENABLE
ENDIF
SELECT SYCEXCH
GO TOP

SHOW WINDOW (lcBrowTtl) REFRESH SAME

*!**************************************************************************
*!
*!      Function: lfwCurrency
*!
*!**************************************************************************
*
FUNCTION lfwCurrency

lcOldData = lcCurrency

*!**************************************************************************
*!
*!      Function: lfvCurrency
*!
*!**************************************************************************
*
FUNCTION lfvCurrency

PRIVATE lcRelation

*B601101,1 This lines was added by HS [Begin]
*B601101,1 IF Statment to check if the field was empty 
IF EMPTY(lcCurrency) .AND. !llBrowse
   SELECT SYCEXCH
   GO TOP
   SELECT SYCCURR
   SHOW WINDOW (lcBrowTtl) REFRESH SAME
   RETURN
ENDIF    &&End of IF
*B601101,1 This lines was added by HS [End]

*B601101,1 Change this line [Begin]
*IF llBrowse .OR. !EMPTY(lcCurrency) .OR. !SEEK(lcCurrency,'SYCCURR') .OR. ATC("?",lcCurrency) > 0 .AND. LASTKEY() = 13
IF llBrowse .OR. (!EMPTY(lcCurrency) .AND. !SEEK(lcCurrency,'SYCCURR')) .OR. ATC("?",lcCurrency) > 0 .AND. c() = 13
*B601101,1 Change this line [End]

  SELECT SYCEXCH
  lcRelation = SET('RELATION')
  SET RELATION TO
  SELECT SYCCURR
  DIMENSION laTemp[1]
  laTemp     = ''
  lcSavBrFld = lcBrFields
  lcSavTitle = lcFile_Ttl
  lcFile_Ttl = "Select Currency"
  lcBrFields = "CCURRCODE :R :H= 'Currency code'," +;
               "CCURRDESC :R :H= 'Description',  " +;
               "CCURRSMBL :R :H= 'Symbol'"
  =gfBrows('FOR CCURRCODE <> lcBaseCurr','CCURRCODE','laTemp')
  lcBrFields = lcSavBrFld
  lcFile_Ttl = lcSavTitle
  IF EMPTY(laTemp[1])
    lcCurrency = lcOldData
  ELSE
    lcCurrency = laTemp[1]
  ENDIF
  SELECT SYCEXCH
  SET RELATION TO &lcRelation ADDI
  GO TOP
  SELECT SYCCURR
ENDIF
llBrowse = .F.
SHOW WINDOW (lcBrowTtl) REFRESH SAME
