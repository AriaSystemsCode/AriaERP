*:*************************************************************
*: Program file  : ALPKTKVA.PRG
*: Program desc. : Print Picking Ticket For Van Mar
*: For Report    : ALPKTKVA.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : AL
*: Developer     : Abdou Elgendy (Abd)
*: Reference     : C102808,1
*:*************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : 
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*:Modifications:
*:*************************************************************
*:

RETURN
*-- End Of alpktkva
*:*************************************************************
*: Name      : lfGetStDec
*: Developer : Abdou Elgendy (ABD)
*: Date      : 03/19/2003
*: Purpose   : to get the Name for the dec. center for printed store.
*:*************************************************************
*: Called from : ALPKTKVA.FRX
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters : lcReturn
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfGetStDec()
*:*************************************************************
*:
FUNCTION lfGetStDec
PARAMETERS lcReturn

PRIVATE lnAlias , lnRecNo , lcStrDec , lcRpStorDC
lnAlias = SELECT(0)
lcRpStorDC = ''
*-- Check if this Store has a DC
lcTempacct = gfTempName()       && NAME OF THE TEMPORARY FILE 
= gfOpenFile(gcDataDir+"CUSTOMER", "CUSTOMER", "SH",@lcTempacct,.T.)

IF !EMPTY(Piktkt.Store) .AND. SEEK('S'+Piktkt.Account+Piktkt.Store,lcTempacct)
  IF EMPTY(&lcTempacct..Dist_ctr)
    lcRpStorDC = &lcTempacct..Btname + &lcTempacct..cAddress12
  ELSE
    lcStrDec = &lcTempacct..Account + &lcTempacct..Dist_ctr
    IF SEEK('S'+lcStrDec,lcTempacct)
      lcRpStorDC = &lcTempacct..Stname +  &lcTempacct..cAddress1
    ENDIF
  ENDIF
    
ELSE
  = SEEK('M'+Piktkt.Account+Piktkt.Store,lcTempacct)
  lcRpStorDC = &lcTempacct..Stname +  &lcTempacct..cAddress1
ENDIF

USE IN (lcTempacct)
lcReturn = lcRpStorDC

SELECT(lnAlias )

RETURN
*-- End OF lfGetStDec
*:*************************************************************
