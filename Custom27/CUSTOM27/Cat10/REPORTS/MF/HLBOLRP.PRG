*:****************************************************************
*: Program file  : HLBOLRP.PRG
*: Program desc. : Bill of Lading.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/21/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwRepWhen , lfvStatus , lfvDateRng
*:               :            : lfvpbok    , lfCreatemp, lfCollect
*:               :            : lfCont.
*:               -----------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lcTemp     :- Variable Hold the temp file that collect the data.
*-- laRpSource :- Source Array Hold the Codes.
*-- laRpTarget :- Target Array Hold the Codes.
*-- laRpcPYSrc :- Copy From the Source array.
*-- ldFrom     :- From Date 
*-- ldTo       :- To date.
*-- ldOldFrDte :- Variable Save the Old from Date
*-- ldOldTDte  :- Variable Save the Old to Date
*-- lcContain  :- Variable to hold the Container Number

STORE '' TO lcKeyPad , lcKeyBol , lcContain
STORE 0 TO lnTotPad , lnTotBol
STORE {} TO ldOldFrDte , ldOldTDte

*-- End Declaration variables.



*- Message Text   :- You must Select a status for the report to be displayed.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
IF EMPTY(laRpTarget)
  = gfModalGen('INM00000B00000','F','ALERT',' ','You must Select a status for the report to be displayed.')
  RETURN
ENDIF


DO CASE
  CASE EMPTY(ldFrom) .AND. EMPTY(ldTo)
    *- Message Text   :- You must enter "From" and "To" date for the report to be displayed.
    *- Message No.    :- 000000.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM00000B00000','F','ALERT',' ','You must enter "From" and "To" date for the report to be displayed.')
    RETURN
  CASE EMPTY(ldFrom) .AND. !EMPTY(ldTo)
    *- Message Text   :- You must enter "From" date for the report to be displayed.
    *- Message No.    :- 000000.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM00000B00000','F','ALERT',' ','You must enter "From" date for the report to be displayed.')
    RETURN
  
  CASE EMPTY(ldFrom) .AND. !EMPTY(ldTo)
    *- Message Text   :- You must Enter "To" date for the report to be displayed.
    *- Message No.    :- 000000.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM00000B00000','F','ALERT',' ','You must Enter "To" date for the report to be displayed.')
    RETURN
ENDCASE

*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh

  *-- Function To Create temp File
  = lfCreatemp()

  *-- Collecting Data In The First Time.
  = lfCollect ()


ENDIF

SELECT (lcTemp)
LOCATE
DO gfDispRe WITH EVAL('lcRpForm')

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/21/2002
*: Purpose   : When function of OG
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwRepWhen()
*:*************************************************************
*
FUNCTION lfwRepWhen


DIMENSION laRpSource[3] , laRpTarget[1] , laRpcPYSrc[1,2]
STORE '' TO laRpTarget , laRpSource , laRpcPYSrc

laRpSource[1] = 'Open     '
laRpSource[2] = 'Void     '
laRpSource[3] = 'Paid     '

*-- End OF lfwRepWhen
*:*************************************************************
*: Name      : lfvStatus
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/21/2002
*: Purpose   : to get all codes.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvStatus()
*:*************************************************************
*
FUNCTION lfvStatus

DIMENSION laOldCode [1]
laOldCode = ''
= ACOPY(laRpTarget,laOldCode)
=gfMover(@laRpSource,@laRpTarget,"Status ",.T.,'')

IF ALEN (laOldCode) = ALEN(laRpTarget)
   =ASORT(laOldCode)
   =ASORT(laRpTarget)
  FOR X = 1 TO ALEN (laRpTarget)
    IF laOldCode[X] # laRpTarget[X]
      llOGFltCh = .T.
      EXIT
    ENDIF  
  ENDFOR
ELSE
  llOGFltCh = .T.
ENDIF

*-- End Of lfvStatus
*:*************************************************************
*: Name      : lfvDateRng
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/21/2002
*: Purpose   : Showes date range screen
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvDateRng()
*:*************************************************************
FUNCTION lfvDateRng

ldOldFrDte = ldFrom  
ldOldTDte  = ldTo

lcTitle = 'Sailing Date'
DO DateRng.Spx

*-- End of lfvDateRng.
*:*************************************************************
*: Name      : lfvpbok
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/21/2002
*: Purpose   : Validate date range screen's OK button
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : =lfvpbok()
*:*************************************************************
FUNCTION lfvpbok

IF ldFrom > ldTo
  *- Message Text   :- "From" value must be less than or equal to "To" value
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
    = gfModalGen('INM00000B00000','F','ALERT',' ','"From" value must be less than or equal to "To" value')
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  CLEAR READ
ENDIF

IF !(ldOldFrDte = ldFrom) .OR. !(ldOldTDte= ldTo)
  llOGFltCh  = .T.
ENDIF

*-- End of lfvpbok.
*:**************************************************************************
*: Name      : lfCreatemp
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Create the temp files.
*:**************************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:**************************************************************************
*: Called from : Report code
*:**************************************************************************
*: Passed Parameters  : ...
*:**************************************************************************
*: Returns            : None.
*:**************************************************************************
*: Example   : = lfCreatemp()
*:**************************************************************************
*
FUNCTION lfCreatemp

IF !USED(lcTemp)
  CREATE Table (lcTemp) (ctsuvencd C(6)   , dsaildate D(8)   , cbolno C(15)    ,;
                         dduedate  D(8)   , ccinconno C(30)  , ccinvno C(12)   ,;
                          ntotpiece N(7)  , nbolamont N(10,2), namountp N(9,2),;
                          dcheckdat D(8)  , ccheckno C(20)   , cbolstat C (1))
ENDIF

SELECT (lcTemp)
ZAP
INDEX ON ctsuvencd + DTOC(dsaildate) TAG (lcTemp) OF (gcWorkDir+lcTemp+'.CDX')


*-- End OF lfCreatemp
*:*************************************************************
*: Name      : lfCollect
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : Collecting Data In The First Time.
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCollect ()
*:*************************************************************
*
FUNCTION lfCollect
PRIVATE lcStatus
lcStatus =''

FOR I = 1 TO ALEN(laRpTarget)
  DO CASE
    CASE laRpTarget[I] = 'Open'
      lcStatus = lcStatus + 'O'
    CASE laRpTarget[I] = 'Void'
      lcStatus = lcStatus + 'V'
    CASE laRpTarget[I] = 'Paid'
      lcStatus = lcStatus + 'P'
  ENDCASE
ENDFOR

SELECT HLCBOL
SCAN FOR cbolstat $ lcStatus  .AND. BETWEEN(dsaildate,ldFrom,ldTo)
  IF SEEK (ctsuvencd+cbolno,'HlContnr')
    SELECT HlContnr
    SCAN REST WHILE ctsuvencd+cbolno = Hlcbol.ctsuvencd+Hlcbol.cbolno
    
      IF SEEK(ctsuvencd+cbolno,'HlcPaymn')
        SELECT HlcPaymn 
        SCAN REST WHILE ctsuvencd+cbolno = Hlcbol.ctsuvencd+Hlcbol.cbolno ;
           FOR Hlcpaymn.namountp <> 0
           = SEEK(ctsuvencd+cCheckNo,'hlchecks')
          SELECT (lcTemp)
          APPEND BLANK
          REPLACE ctsuvencd WITH Hlcbol.ctsuvencd  ,;
                  dsaildate WITH Hlcbol.dsaildate  ,;
                  cbolno    WITH Hlcbol.cbolno     ,;
                  dduedate  WITH Hlcbol.dduedate   ,;
                  nbolamont WITH Hlcbol.nbolamont  ,;
                  cbolstat  WITH Hlcbol.cbolstat   ,;
                  ccinconno WITH Hlcontnr.ccinconno,;
                  ccinvno   WITH Hlcontnr.ccinvno  ,;
                  ntotpiece WITH Hlcontnr.ntotpiece,;
                  namountp  WITH Hlcpaymn.namountp ,;
                  ccheckno  WITH Hlcpaymn.ccheckno ,;
                  dcheckdat WITH Hlchecks.dchkdate
          SELECT HlcPaymn          
        ENDSCAN
  
      ELSE
        SELECT (lcTemp)
        APPEND BLANK
        REPLACE ctsuvencd WITH Hlcbol.ctsuvencd  ,;
                dsaildate WITH Hlcbol.dsaildate  ,;
                cbolno    WITH Hlcbol.cbolno     ,;
                dduedate  WITH Hlcbol.dduedate   ,;
                nbolamont WITH Hlcbol.nbolamont  ,;
                cbolstat  WITH Hlcbol.cbolstat   ,;
                ccinconno WITH Hlcontnr.ccinconno,;
                ccinvno   WITH Hlcontnr.ccinvno  ,;
                ntotpiece WITH Hlcontnr.ntotpiece
        SELECT HlContnr
      ENDIF
      SELECT HlContnr
    ENDSCAN
    SELECT HLCBOL
  ENDIF  
ENDSCAN

SELECT (lcTemp)
LOCATE
SCAN
  IF lcKeyPad <> cBOLNo+ccheckno
    lnTotPad = lnTotPad + nAmountp
    lcKeyPad = cBOLNo+ccheckno
  ENDIF
  IF lcKeyBol <> cBOLNo
    lnTotBol = lnTotBol + nBolAmont
    lcKeyBol = cBOLNo
  ENDIF
ENDSCAN
lnTotPad = IIF(lcKeyPad <> cCInvNo+ccheckno,lnTotPad + nAmountp,lnTotPad)
lnTotBol = IIF(lcKeyBol <> CBOLNO,lnTotBol + nBolAmont,lnTotBol)


*-- End OF lfCollect
*:*************************************************************
*: Name      : lfCont
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/09/2002
*: Purpose   : to return the Container Description
*:*************************************************************
*: Called from : FRX
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Container Description.
*:*************************************************************
*: Example     : = lfCont ()
*:*************************************************************
*
FUNCTION lfCont

IF lcContain = ccinconno  
  RETURN ''
ELSE
  lcContain = ccinconno
  RETURN ALLTRIM(ccinconno)
ENDIF

*-- End Of lfCont
*:*************************************************************