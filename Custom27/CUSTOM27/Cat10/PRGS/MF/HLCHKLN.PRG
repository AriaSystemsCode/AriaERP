*:****************************************************************
*: Program file  : HLChkLn.PRG
*: Program desc. : Apply the Check Payment.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/06/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwLine   , lfShow    , lfvOk 
*:               :            : lfvCancel , lfwStatus , lfvStatus
*:               -----------------------------------------------
*:               : PROCEDURE  : lpClose.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:

EXTERNAL ARRAY laData,laScrMode

lcCheckNo = laData[1]  && Variable to hold the Check No 
lnAmount  = laData[3]  && Variable To Hold the Check Amount
lnUnAppl  = laData[5]

SELECT (lc_Temp)
GO TOP 

SUM nAmountP to lnPaid

lsLine = 1


PUSH KEY
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose
DO (gcScrDir+gcAct_Appl+"\HlchkLn.SPX")
POP KEY


RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwLine
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/06/2002
*: Purpose   : To high light the new line
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  = lfwLine ()
*:*************************************************************
*
FUNCTION lfwLine

** To high light the new line **

lsLine = SUBSTR(cVendor,1,10)+' '+;
         IIF(cinvtype='R','Regular',IIF(cinvtype = 'S',;
            'Seconds','Balance'))+'  '+SUBSTR(ccinvno,1,11)+' '+;
            STR(ncintval,10,2)+' '+STR(nBalance,10,2)+' '+;
            STR(namountp,10,2)

SHOW GET lsLine
SHOW GETS ONLY
SHOW GET namountp

*-- End Of lfwLine
*:*************************************************************
*: Name      : lfShow
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/06/2002
*: Purpose   : Show Function
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  = lfShow ()
*:*************************************************************
*
PROCEDURE lfShow

EXTERNAL ARRAY laScrMode
DO CASE
  CASE laScrMode[2]                      && View Mode
    SHOW GETS ONLY
    SHOW GET pbOk     ENABLE
  OTHERWISE                                 && Add or Edit Mode
    SHOW GET lcCheckNo
    SHOW GET lnAmount 
	SHOW GET lnPaid   
	SHOW GET lnUnAppl 
	SHOW GET nAmountP  ENABLE
    SHOW GET pbOk      ENABLE
    SHOW GET pbCancel  ENABLE
ENDCASE

*-- End Of lfShow.
*:*************************************************************
*: Name      : lfvOk
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/06/2002
*: Purpose   : Close the Screen.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  = lfvOk ()
*:*************************************************************
*
FUNCTION lfvOk

llUpdApp = .T.
 
 *-- End OF lfvOk
*:*************************************************************
*: Name      : lfvCancel
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/06/2002
*: Purpose   : Canncel the screen.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  = lfvCancel ()
*:*************************************************************
*
FUNCTION lfvCancel

llUpdApp = .F.

*-- End OF lfvCancel
*:*************************************************************
*: Name      : lfwStatus
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/06/2002
*: Purpose   : When Function to the Status.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  = lfwStatus ()
*:*************************************************************
*
FUNCTION lfwStatus

IF lModifi = 'T'
  REPLACE nBalance WITH nBalance+nAmountP

  lnPaid   = lnPaid-nAmountP
  lnUnAppl = lnUnAppl+nAmountP

  SHOW GET lnPaid  
  SHOW GET lnUnAppl
  
  lsLine = SUBSTR(cVendor,1,10)+' '+;
           IIF(cinvtype='R','Regular',IIF(cinvtype = 'S',;
              'Seconds','Balance'))+'  '+SUBSTR(ccinvno,1,11)+' '+;
              STR(ncintval,10,2)+' '+STR(nBalance,10,2)+' '+;
              STR(namountp,10,2)

  SHOW GET lsLine
ENDIF

*-- End Of lfwStatus.
*:*************************************************************
*: Name      : lfvStatus
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/06/2002
*: Purpose   : Valid Function to the status.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   :  = lfvStatus ()
*:*************************************************************
*
FUNCTION lfvStatus

lcRecStat = ''

IF nAmountP = 0 
  lcRecStat = SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify
  REPLACE cStatus  WITH lcRecStat

  lsLine = SUBSTR(cVendor,1,10)+' '+;
           IIF(cinvtype='R','Regular',IIF(cinvtype = 'S',;
              'Seconds','Balance'))+'  '+SUBSTR(ccinvno,1,11)+' '+;
               STR(ncintval,10,2)+' '+STR(nBalance,10,2)+' '+;
               STR(namountp,10,2)

  SHOW GET lsLine
ELSE  
  IF nAmountP <= nBalance .AND. nAmountP <= lnUnAppl
    lcRecStat = SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify
    REPLACE cStatus WITH lcRecStat,;
            nBalance  WITH nBalance-nAmountP,;
            lModifi  WITH 'T'

    lnPaid   = lnPaid + nAmountP
    lnUnAppl = lnUnAppl - nAmountP
   
    SHOW GET lnPaid   
    SHOW GET lnUnAppl 

    lsLine = SUBSTR(cVendor,1,10)+' '+;
             IIF(cinvtype='R','Regular',IIF(cinvtype = 'S',;
                'Seconds','Balance'))+'  '+SUBSTR(ccinvno,1,11)+' '+;
                STR(ncintval,10,2)+' '+STR(nBalance,10,2)+' '+;
                STR(namountp,10,2)

    SHOW GET lsLine
  ELSE

    IF nAmountP > lnUnAppl
      *- Message Text   :- Amount entered cannot be greater than unapplied amount on check
      *- Message No.    :- 00000.
      *- Buttom Message :- OK.
      *- Buttom Number  :- 00000.
      = gfModalGen('INM00000B00000','F','ALERT',' ','Amount entered cannot be greater than unapplied amount on check.')
      REPLACE nAmountP WITH 0
      _CUROBJ = OBJNUM(nAmountP)
      SHOW GET nAmountP
    ENDIF
    
    IF nAmountP > nBalance
      *- Message Text   :- Amount cannot be greater than the balance of the Invoice.
      *- Message No.    :- 00000.
      *- Buttom Message :- OK.
      *- Buttom Number  :- 00000.
      = gfModalGen('INM00000B00000','F','ALERT',' ','Amount cannot be greater than the balance of the Invoice.')
      REPLACE nAmountP WITH 0
      _CUROBJ = OBJNUM(nAmountP)
      SHOW GET nAmountP
    ENDIF
 
    lsLine = SUBSTR(cVendor,1,10)+' '+;
             IIF(cinvtype='R','Regular',IIF(cinvtype = 'S',;
                'Seconds','Balance'))+'  '+SUBSTR(ccinvno,1,11)+' '+;
                 STR(ncintval,10,2)+' '+STR(nBalance,10,2)+' '+;
                 STR(namountp,10,2)

    SHOW GET lsLine
  ENDIF
ENDIF  

*-- End OF lfvStatus.
*:*************************************************************
*: Name      : lpClose
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/06/2002
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpClose
*:*************************************************************
*
PROCEDURE lpClose

IF laScrMode [3] .OR. laScrMode [4]
  _CUROBJ = OBJNUM(pbCancel)
ELSE
  _CUROBJ = OBJNUM(pbOk)
ENDIF  
KEYBOARD '{ENTER}'
RETURN

*-- End Of lpClose
*:*************************************************************