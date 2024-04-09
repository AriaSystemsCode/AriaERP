*:************************************************************************
*: Program file  : GFREPCUR.PRG
*: Program desc. : Currency selection grid.
*: For screen    : Called from any report that is to use
*:                 multi currency.
*:         System: 1. ARIA APPAREL SYSTEM
*:                 2. ARIA ADVANTAGE SYSTEM 
*:      Developer: RENEE EZZAT
*:************************************************************************
*: Called From   : May be called from any Advantage or Apparel report's 
*:                 option grid
*:************************************************************************
*: Calls         : SYREPCUR.SPR
*:                 SYCURBR.SPR
*:*************************************************************************
*: Passed Parameters  : llCalledFromAdv : .T. if called from RIA ADV.,
*:                                        .F. if called from APPAREL
*:                      lcRepCurr       : returns the currency display 
*:                                        option.
*:                      ldExRtDate      : returns the entered exchange 
*:                                        rate date if lcRepCurr = 'D',
*:                                        or empty, if any other option.
*:                      lcTmpFlName     : A temporary file random name.
*:*************************************************************************
*: Returns            : .T. if exit this cuntion with 'Proceed',
*:                      .F. if exit this function with 'Cancel'
*:*************************************************************************
*: Example            : llProceed = gfRepCur(.t., @lcRepCurr,;
*:                                            @ldExDate, lc_TmpName)   
*:************************************************************************
*E300323,1 RENEE 12/12/1995
*B601526,1  HS  01/23/1997  Fix the function [gfRepCur]
*:************************************************************************
FUNCTION gfRepCur
PARAMETERS llCalledFromAdv, lcRepCurr, ldExRtDate, lcTmpFlName 
PRIVATE lcBrTtl, lcOldClrSet, lgOldVal, lnBrRecNo, lnCurAlias 

IF EMPTY(lcTmpFlName)
  RETURN
ENDIF    

*E300323,1 lnBrRecNo : Record number, used fro the browse pointer.
*E300323,1 lgOldVal  : get fields old value
STORE 0 TO lnBrRecNo, lgOldVal

*E300323,1 llExitCode : .F. if exit with Cancel
llExitCode = .F.

*E300323,1 If called from Aria Advantage System,
IF llCalledFromAdv
  *E300323,1 READ CYCLE COLOR clause
  lcReadColor = ',' + gcReadClr
  *E300323,1 When function of READ CYCLE, called from .SPRs
  lcWhenFunc  = 'gfActHelp()'
  *E300323,1 READ CYCLE MESSAGE clause, called from get fields
  *E300323,1 in .SPRs
  lcObj_Msg   = 'gfObj_Msg()'
  *E300323,1 Default date width of the system
  lnDateWdth  =  gnDateWdth 
  *E300323,1 Default exchange rate date with the system date if it is 
  *E300323,1 empty.
  ldExDate  =  IIF(EMPTY(ldExRtDate), gdSysDate, ldExRtDate)
  *E300323,1 Browse windows default color scheme that is used by
  *E300323,1 the system.
  lcClrSchm = IIF(_DOS .OR. _UNIX," COLOR SCHEME 13","")
  
*E300323,1 Else, if called from Aria Apparel System 
ELSE
  *E300323,1 READ CYCLE COLOR clause
  lcReadColor = gcReadColr
  *E300323,1 When function of READ CYCLE, called from .SPRs
  *E300323,1 Default to '.T.' it there is no function.
  lcWhenFunc  = '.T.'
  *E300323,1 READ CYCLE MESSAGE clause, called from get fields
  *E300323,1 in .SPRs. Call local message function (lfObj_Msg())
  lcObj_Msg   = 'lfObj_Msg()'
  *E300323,1 Default date width of the system
  lnDateWdth  =  8
  *E300323,1 Default exchange rate date with the system date if it is 
  *E300323,1 empty.
  ldExDate  =  IIF(EMPTY(ldExRtDate), DATE(), ldExRtDate)

  *E300323,1 Set colorset to APPARELSET
  *E300323,1 Browse windows default color scheme that is used by
  *E300323,1 the system.
  lcOldClrSet = SET('COLOR') 
  IF _DOS .OR. _UNIX
    SET COLOR SET TO APPARELSET
    lcClrSchm = " COLOR SCHEME 10"
  ELSE
    lcClrSchm = ""
  ENDIF
ENDIF   && ENDIF llCalledFromAdv         

*E300323,1 Screen variables declaration
*E300323,1 larbRepCur : Currency base selection radio buttons array
*E300323,1 rbRepCur   :  
DECLARE larbRepCur[4]
larbRepCur    = 0

*E300323,1 If empty, default currency base to foreign curreny (option 1)
IF EMPTY(lcRepCurr)
  larbRepCur[1] = 1
  rbRepCur      = 1
ELSE
  rbRepCur             = AT(lcRepCurr, 'FODU')
  larbRepCur[rbRepCur] = 1
ENDIF
lcDispStat = IIF(rbRepCur = 3, 'ENABLE', 'DISABLE')

*E300323,1 The following colors are used if WINDOWS or MAC,
*E300323,1 to hide or display the borders of get field ldExDate
*E300323,1 Colors used under Windows for objects' borders and
*E300323,1 windows backgrounds
*E300323,1 - Color used for top and left corners of an object
lcTopLeft   = 'RGB(128,128,128,128,128,128)'      && Dark gray
*E300323,1 - Color used for bottom and right corners of an object
lcBotRight  = 'RGB(255,255,255,255,255,255)'      && White 
*E300323,1 - Background color of windows 
lcBackGrnd  = 'RGB(192,192,192,192,192,192)'      && Light gray

*E300323,1 Change disable color of Date field to text if disabled
*E300323,1 Store color in variable lcDateColor
lcDateColor = IIF(_DOS .OR. _UNIX, SCHEME(5,1), 'RGB(0,0,0,192,192,192)') 

*E300323,1 lcCursrName :  cursor temporaru name
lcCursrName = gfTempName()

*E300323,1 Save the current work area.
lnCurAlias = SELECT()

PUSH KEY
DO SYREPCUR.SPR
POP KEY

*E300323,1 Restore the calling work area.
SELECT (lnCurAlias)

*E300323,1 If exit with Proceed fill the return variable,
*E300323,1 otherwise do not change the variables.
IF llExitCode
  *E300323,1 Fill lcRepCurr variable
  lcRepCurr  = SUBSTR('FODU', rbRepCur, 1)  
  *E300323,1 Fill exchange rate date according to the option.
  ldExRtDate = IIF(rbRepCur = 3, ldExDate, {})
ENDIF

*E300323,1 close an open cursor.
IF USED (lcCursrName)
  USE IN (lcCursrName)
ENDIF  
IF !llCalledFromAdv
  SET COLOR TO (lcOldClrSet)
ENDIF  
RETURN llExitCode

*!*************************************************************
*! Name      : lfvProceed
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/13/1995
*! Purpose   : VALID function of pbProceed
*!*************************************************************
*! Called    : SYREPCUR.SPR
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None 
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : =lfvProceed()
*!*************************************************************
FUNCTION lfvProceed

*E300323,1 If any other option, return .T.
llExitCode = .T.

*E300323,1 If option 4 is selected, create a temporary file
*E300323,1 with all available currency codes, default all 
*E300323,1 rates to 0.
IF rbRepCur = 4 
  *E300323,1 Create the file if it is not already created.
  IF !USED(lcTmpFlName)
    *E300323,1 Default date to the system date.
    ldExDate  = IIF(llCalledFromAdv, gdSysDate, DATE())
  
    SELECT cCurrcode,    cCurrDesc,;
           nCurrUnit,    cCurrSmbl,;
           ldExDate   AS dRateDate,;
           0000.0000  AS nExRate;
      FROM (gcSysHome + 'SYCCURR');
      INTO DBF (gcWorkDir + lcTmpFlName)     
    INDEX ON cCurrCode TAG CURRENCY
  ENDIF
  *E300323,1 Get the current selection into a cursor
  *E300323,1 so as to return them if cancelling.
  SELECT * FROM (lcTmpFlName) ;    
    INTO CURSOR (lcCursrName)  

  *E300323,1 Prepare key setting for the browse window activation
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)
  DO SYCURBR.SPR
  ON KEY LABEL ALT+B 
ENDIF
IF llExitCode
  CLEAR READ
ENDIF


*!*************************************************************
*! Name      : lfvCancel
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/13/1995
*! Purpose   : Valid function for push button pbCandel, from 
*!             the browse screen.
*!*************************************************************
*! Called    : SYCURBR.SPR
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None 
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            : =lfvCancel()
*!*************************************************************
FUNCTION lfvCancel
*E300323,1 Exit with .F.
llExitCode = .F.

*E300323,1 Copy the cursor old contents to the temporary file
SELECT * FROM (lcCursrName) ;    
  INTO DBF (gcWorkDir + lcTmpFlName);

*B601526,1 Add this line [Begin]
INDEX ON cCurrCode TAG CURRENCY
*B601526,1 Add this line [End]


*!*************************************************************
*! Name      : lfObj_Msg
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/13/1995
*! Purpose   : READ CYCLE MESSAGE clause 
*!             If the function is called frm ARIA APPAREL SYSTEM,
*!             this function replaces gfObj_Msg() (that is called
*!             from the apparel.
*!*************************************************************
*! Called    : SYREPCUR.SPR, SYCURBR.SPR
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Returns            : A message string.
*!*************************************************************
*! Example            : MESSAGE lfObj_Msg()
*!*************************************************************
FUNCTION lfObj_Msg
DO CASE
  CASE 'RBREPCUR' $ SYS(18) 
    lcMsgStr = 'Select currency base for report amounts'
  CASE SYS(18) = 'LDEXDATE'
    lcMsgStr = 'Enter the exchange rates date'
  CASE SYS(18) = 'PBPROCEED'
    lcMsgStr = 'Proceed with the report using the current selections '
  CASE SYS(18) = 'PBCANCEL'
    lcMsgStr = 'Cancel'
ENDCASE
RETURN lcMsgStr

*!*************************************************************
*! Name      : lfvRepCur
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/13/1995
*! Purpose   : VALID function of radio buttons (larbRepCur)
*!*************************************************************
*! Called             :  SYREPCUR.SPR
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Passed Parameters  :  Radio button number(1, 2, 3, or 4)
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRepCur()
*!*************************************************************
FUNCTION lfvRepCur
PARAMETERS lnRepCur

IF rbRepCur <> lnRepCur
  larbRepCur[rbRepCur] = 0
  SHOW GET larbRepCur[rbRepCur]
  rbRepCur  = lnRepCur

  *E300323,1 If third option, enable the exchange date field,
  *E300323,1 otherwise disable the field.
  IF rbRepCur = 3
    SHOW GET ldExDate  ENABLE
  ELSE
    SHOW GET ldExDate DISABLE
  ENDIF  
  IF _WINDOWS .OR. _MAC
    =lfRefresh()
  ENDIF  
ENDIF

*!*************************************************************
*! Name      : lfBrowse
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/13/1995
*! Purpose   : Browse activation function
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  lfBrowse()
*!*************************************************************
FUNCTION lfBrowse

SELECT (lcTmpFlName)
GO TOP
lnBrRecNo = RECNO()
BROWSE FIELDS cMarker   = IIF(RECNO() = lnBrRecNo, '', ' ');
                        : H = ' ' :R :W = .F.,;
              cCurrCode : H = 'Currency':R :W = .F.,; 
              cCurrDesc : H = 'Currency Description':R :W = .F.:30,;
              nExRate   : H = 'Ex. Rate':W = lfwCrOldVals() :F:V = lfvCrExRate();         
         WINDOW SYCURBR0 ;
         IN WINDOW SYCURBR ;
         WHEN lfwBrwWhen();
         NOMENU;
         NOAPPEND;
         NODELETE;
         NOWAIT;
         SAVE;
         NOCLEAR;
         &lcClrSchm;
         TITLE lcBrTtl 

*!*************************************************************
*! Name      : lfwBrwWhen
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/13/1995
*! Purpose   : WHEN function of the browse command
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  BROWSE ..  WHEN lfwBrwWhen()
*!*************************************************************
FUNCTION lfwBrwWhen
lnBrRecNo = RECNO()
SHOW WINDOW (lcBrTtl) REFRESH SAME

*!*************************************************************
*! Name      : lfwCrOldVals
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/13/1995
*! Purpose   : WHEN function of fields. Stores current field values.
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            : - BROWSE ..  WHEN lfwBrwWhen()
*!*************************************************************
FUNCTION lfwCrOldVals
lgOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvExRtDate
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/13/1995
*! Purpose   : VALID function of ldExDate field.
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvExRtDate()
*!*************************************************************
FUNCTION lfvExRtDate
IF EMPTY(ldExDate)
  ldExDate = IIF(llCalledFromAdv, DATE(), gdSysDate)
ENDIF
  
*!*************************************************************
*! Name      : lfvCrExRate
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/13/1995
*! Purpose   : VALID function of nExRate field in the browse command
*!*************************************************************
*! Calls              :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  nExRate:V=lfvCrExRate()
*!*************************************************************
FUNCTION lfvCrExRate
*E300323,1 If the entered rate is less than 0, present the following
*E300323,1 message and return the old value.
IF nExRate < 0
  *E300323,1 Message : "  ð cannot be negative.  "
  *E300323,1                    <   OK   >
  =gfModalGen("INM00251B00000", "DIALOG", lcBrTtl)
  REPLACE nExRate WITH lgOldVal
*E300323,1 If the entry is valid ( > -1),
*E300323,1 Go to the next record 
ELSE
  *E300323,1 If an ENTER is pressed, proceed to the next record,
  *E300323,1 otherwise, proceed according to the key or mouse pressed.
  IF LASTKEY() = 13
    KEYBOARD "{DNARROW}" CLEAR
  ENDIF  
ENDIF    


*!*************************************************************
*! Name      : lfReadAct
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/13/1995
*! Purpose   : READ Activate function of SYCURBR.SCX
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
*E300323,1 Restore old key settings and clear the current settings
ON KEY LABEL TAB        
ON KEY LABEL BACKTAB    
ON KEY LABEL CTRL+ENTER 
ON KEY LABEL ESC        
ON KEY LABEL CTRL+Q     
ON KEY LABEL CTRL+W     
ON KEY LABEL CTRL+HOME  
ON KEY LABEL CTRL+END   
ON KEY LABEL ALT+P      
ON KEY LABEL ALT+C      
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrTtl)

*!*************************************************************
*! Name      : lfReadDeact
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/13/1995
*! Purpose   : READ Deactivate function of SYCURBR.SCX
*!*************************************************************
*! Calls     : lfTrapKey, lpTab
*!*************************************************************
*! Returns            :  .F.
*!*************************************************************
*! Example            :  =lfReadDeact()
*!*************************************************************
FUNCTION lfReadDeact
*E300323,1 Trap keys only if the browse window is on top.
IF WONTOP() = lcBrTtl
  ON KEY LABEL TAB        DO lpTab     WITH 'SYCURBR1', OBJNUM(pbProceed)
  ON KEY LABEL BACKTAB    DO lpTab     WITH 'SYCURBR1', OBJNUM(pbCancel)
  ON KEY LABEL CTRL+ENTER llDummy = lfTrapKey('SYCURBR1', OBJNUM(pbProceed))
  ON KEY LABEL ESC        llDummy = lfTrapKey('SYCURBR1', OBJNUM(pbCancel))
  ON KEY LABEL CTRL+Q     lnDummy = 1
  ON KEY LABEL CTRL+W     lnDummy = 1
  ON KEY LABEL CTRL+HOME  GO TOP
  ON KEY LABEL CTRL+END   GO BOTTOM
  ON KEY LABEL ALT+P      llDummy = lfTrapKey('SYCURBR1', OBJNUM(pbProceed))
  ON KEY LABEL ALT+C      llDummy = lfTrapKey('SYCURBR1', OBJNUM(pbCancel))
ENDIF  
RETURN .F.

*!*************************************************************
*! Name      : lfTrapKey
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/13/1995
*! Purpose   : Trap of keys
*!*************************************************************
*! Passed Parameters  :  lcWindName : Window name to activate
*!                       lnObjNum   : object number to focus.
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  llDummy = lfTrapKey('MFG10002', OBJNUM(pbNew)
*!*************************************************************
FUNCTION lfTrapKey
PARAMETERS lcWindName, lnObjNum
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = lnObjNum
KEYBOARD CHR(13) CLEAR

*!*************************************************************
*! Name      : lpTab
*! Developer : RENEE - Renee Ezzat
*! Date      : 12/13/1995
*! Purpose   : Trap of tab keys
*!*************************************************************
*! Calls     : None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            :  DO lpTab WITH 'MFG10002', OBJNUM(pbNew)
*!*************************************************************
PROCEDURE lpTab
PARAMETERS lcWindName, lnObjNum
ACTIVATE WINDOW (lcWindNAme)
_CUROBJ = lnObjNum

