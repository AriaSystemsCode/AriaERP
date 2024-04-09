*:****************************************************************
*: Program file  : HLCAPPLY.PRG
*: Program desc. : Apply Screen.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 12/18/2001
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwLine  , lfvOk  , lfvCancel
*:               :            : lfwStatus,lfvStatus.
*:               -----------------------------------------------
*:               : PROCEDURE  : lfShow   , lpClose .
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lcVendor  :- Vendor Description
*-- lcCheckNo :- Variable to hold the Check No.
*-- lnAmount  :- Variable To Hold the Check Amount.

*-- End Declaration variables.
EXTERNAL ARRAY laData,laScrMode

lcVendor  = ALLTRIM(laVendCod((ASCAN(laVendcod,laData[1])/2),1))
lcCheckNo = laData[2]
lnAmount  = laData[4]

SELECT (lc_Temp)
GO TOP 

SUM nAmountP to lnPaid

lsLine = 1


PUSH KEY
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose
DO (gcScrDir+gcAct_Appl+"\Hlcapply.SPX")
POP KEY


RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwLine.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : To high light the new line.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfwLine ()
*:*************************************************************
*:
FUNCTION lfwLine

** To high light the new line **

lsline = SUBSTR(cBOLNo,1,15)+'  '+STR(nBolAmnt,10,2)+'    '+STR(nVenBal,10,2)+'  '+STR(nAmountP,10,2)

SHOW GETS ONLY

*-- End OF lfwLine.
*:*************************************************************
*: Name      : lfShow
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Show Function.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : Do lfShow
*:*************************************************************
*:
PROCEDURE lfShow

EXTERNAL ARRAY laScrMode
DO CASE
  CASE laScrMode[2]                      && View Mode
    SHOW GETS DISABLE ONLY
    SHOW GET pbOk     ENABLE
  OTHERWISE                                 && Add or Edit Mode
    SHOW GET lcVendor 
    SHOW GET lnTotBal
    SHOW GET lcCheckNo
    SHOW GET lnAmount 
	SHOW GET lnPaid   
	SHOW GET lnUnAppl 
    SHOW GET Cbolno   
	SHOW GET nVenBal  
	SHOW GET nAmountP  ENABLE
    SHOW GET pbOk      ENABLE
    SHOW GET pbCancel  ENABLE
ENDCASE

SHOW GET lsLine ENABLE

*-- End Of lfShow.
*:*************************************************************
*: Name      : lfvOk.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Ok Button.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvOk ()
*:*************************************************************
*:
FUNCTION lfvOk

llUpdApp = .T.
 
*-- End OF lfvOk.
*:*************************************************************
*: Name      : lfvCancel.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Canncel Button.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvCancel ()
*:*************************************************************
*:
FUNCTION lfvCancel

llUpdApp = .F.

*-- End OF lfvCancel
*:*************************************************************
*: Name      : lfwStatus.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Valid status for the current line.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfwStatus ()
*:*************************************************************
*:
FUNCTION lfwStatus

IF lModifi = 'T'
  REPLACE nVenBal WITH nVenBal+nAmountP
  lnPaid   = lnPaid-nAmountP
  lnUnAppl = lnUnAppl+nAmountP
  lnTotBal = lnTotBal+nAmountP

  SHOW GET lnPaid  
  SHOW GET lnUnAppl
  SHOW GET lnTotBal
  SHOW GET nVenBal
  
  lsline = SUBSTR(cBOLNo,1,15)+'  '+STR(nBolAmnt,10,2)+'    '+STR(nVenBal,10,2)+'  '+STR(nAmountP,10,2)
  SHOW GET lsLine
ENDIF

*-- End OF lfwStatus.
*:*************************************************************
*: Name      : lfvStatus.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Valid status for the current line.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvStatus ()
*:*************************************************************
*:
FUNCTION lfvStatus

lcRecStat = ''

IF nAmountP = 0 
  lcRecStat = SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify
  REPLACE cStatus  WITH lcRecStat
  lsline = SUBSTR(cBOLNo,1,15)+'  '+STR(nBolAmnt,10,2)+'    '+STR(nVenBal,10,2)+'  '+STR(nAmountP,10,2)
  SHOW GET lsLine
ELSE  
  IF nAmountP <= nVenBal .AND. nAmountP <= lnUnAppl
    lcRecStat = SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify
    REPLACE cStatus  WITH lcRecStat
    REPLACE nVenBal  WITH nVenBal-nAmountP,;
            lModifi  WITH 'T'

    lnPaid   = lnPaid+nAmountP
    lnUnAppl = lnUnAppl-nAmountP
    lnTotBal = lnTotBal-nAmountP

    SHOW GET lnPaid   
    SHOW GET lnUnAppl 
    SHOW GET lnTotBal 
    SHOW GET nVenBal
    
    lsline = SUBSTR(cBOLNo,1,15)+'  '+STR(nBolAmnt,10,2)+'    '+STR(nVenBal,10,2)+'  '+STR(nAmountP,10,2)
    SHOW GET lsLine
  ELSE

    IF nAmountP > lnUnAppl
      =gfModalGen('INM214B000','ALERT'," ")
      REPLACE nAmountP WITH 0
      _CUROBJ = OBJNUM(nAmountP)
      SHOW GET nAmountP
    ENDIF
    
    IF nAmountP > nVenBal
      =gfModalGen('INM215B000','ALERT',"BOL")
      REPLACE nAmountP WITH 0
      _CUROBJ = OBJNUM(nAmountP)
      SHOW GET nAmountP
    ENDIF
    lsline = SUBSTR(cBOLNo,1,15)+'  '+STR(nBolAmnt,10,2)+'    '+STR(nVenBal,10,2)+'  '+STR(nAmountP,10,2)
    SHOW GET lsLine
  ENDIF
ENDIF  

*-- End OF lfvStatus.
*:*************************************************************
*: Name      : lpClose
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
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
