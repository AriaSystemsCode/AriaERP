*:****************************************************************
*: Program file  : HLCONTAN.PRG
*: Program desc. : Containers.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/09/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwLine  , lfvNew   , lfvRemove
*:               :            : lfvOk    , lfvCancel, lfvStatus
*:               -----------------------------------------------
*:               : PROCEDURE  : lfShow , lpClose.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- lcStatus :- Variable to hold the record status
*-- lcVendor :- Vendor discription.
*-- lcBillNo :- Variable to hold the Bill No.
*-- End Declaration variables.
EXTERNAL ARRAY laData,laScrMode

lcStatus  = ' '
lcVendor  = ALLTRIM(laVendCod((ASCAN(laVendcod,laData[1])/2),1))
lcBillNo  = laData[2]

lsLine    = 1


PUSH KEY
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose
DO (gcScrDir+gcAct_Appl+"\HlContan.SPX")
POP KEY

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwLine
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/09/2002
*: Purpose   : To high light the new line.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfwLine ()
*:*************************************************************
*
FUNCTION lfwLine

** To high light the new line **

lsLine = CCINCONNO+' '+CCINVNO+' '+STR(NTOTPIECE,7)

SHOW GETS ONLY

*-- En Of lfwLine.
*:*************************************************************
*: Name      : lfShow
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/09/2002
*: Purpose   : Zoom In the Picture.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  Do lfShow 
*:*************************************************************
*
PROCEDURE lfShow

EXTERNAL ARRAY laScrMode

DO CASE
  CASE laScrMode[2]                      && View Mode
    SHOW GETS DISABLE ONLY
    SHOW GET pbOk     ENABLE

  CASE laScrMode[1] .OR. laScrMode[4]    && Select or Add Mode
    SHOW GET pbNew     ENABLE  
    SHOW GET pbCancel  ENABLE
    SHOW GET ccinconno ENABLE
    SHOW GET ccinvno   ENABLE
    SHOW GET ntotpiece ENABLE

    IF lnLinNo > 0
      SHOW GET PBNew     ENABLE 
      SHOW GET pbCancel  ENABLE
      SHOW GET ccinconno ENABLE
      SHOW GET ccinvno   ENABLE
      SHOW GET ntotpiece ENABLE
    ELSE
      SHOW GET ccinconno ENABLE
      SHOW GET ccinvno   ENABLE
      SHOW GET ntotpiece ENABLE
    ENDIF

  CASE laScrMode[3]                      && Edit Mode
    SHOW GET PBNew     ENABLE 
    SHOW GET pbCancel  ENABLE
    SHOW GET ccinconno ENABLE
    SHOW GET ccinvno   ENABLE
    SHOW GET ntotpiece ENABLE
ENDCASE

IF laScrMode[2] = .F.

  IF lnLinNo > 0 
    SHOW GET pbRemove ENABLE
  ELSE
    SHOW GET pbRemove DISABLE
  ENDIF 

ENDIF  

IF lnLinNo = 0
  _CUROBJ = OBJNUM(pbNew)
  SHOW GET pbNew
ENDIF

SHOW GET lsLine    ENABLE

*-- End OF lfShow.
*:*************************************************************
*: Name      : lfvNew
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/09/2002
*: Purpose   : For adding new line in the list.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfvNew()
*:*************************************************************
*
FUNCTION lfvNew

COUNT TO lnFcount
lnFcount = lnFcount+1
lnLinNo  = lnLinNo +1

** For adding new line in the list **
APPEND BLANK
REPLACE cStatus WITH "A",;
        ctsuvencd WITH laData[1],;
        cBOLNo    WITH laData[2]

IF lnLinNo > 0 
  SHOW GET pbRemove ENABLE
ELSE
  SHOW GET pbRemove DISABLE
ENDIF 
                            

** To high light the new line **
lsLine = CCINCONNO+' '+CCINVNO+' '+STR(NTOTPIECE,7)

SHOW GET lsLine    
SHOW GET ccinconno ENABLE
SHOW GET ccinvno   ENABLE
SHOW GET ntotpiece ENABLE

_CUROBJ = OBJNUM(ccinconno)

SHOW GET ccinconno ENABLE

*-- End OF lfvNew
*:*************************************************************
*: Name      : lfvRemove
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/09/2002
*: Purpose   : For remove the line from the list.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfvRemove()
*:*************************************************************
*
FUNCTION lfvRemove

lcStatus = SUBSTR('DDS',AT(cStatus,'SMA'),1)  && DELETE
REPLACE cStatus WITH lcStatus

DELETE

IF !EOF()
  SKIP
ELSE
  SKIP-1  
ENDIF  

** To high light the new line **
lsLine = CCINCONNO+' '+CCINVNO+' '+STR(NTOTPIECE,7)

IF lnLinNo > 0
  lnLinNo = lnLinNo-1
ENDIF  

IF lnLinNo > 0 
  SHOW GET pbRemove ENABLE
ELSE
  SHOW GET pbRemove DISABLE
ENDIF 

SHOW GET pbNew    ENABLE
SHOW GET pbOk     ENABLE
SHOW GET pbCancel ENABLE

_CUROBJ=OBJNUM(lsLine)

SHOW GET lsLine

*-- End OF lfvRemove.
*:*************************************************************
*: Name      : lfvOk
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/09/2002
*: Purpose   : On Button
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfvOk()
*:*************************************************************
*
FUNCTION lfvOk

llUpdCon=.T.

*-- End OF lfvOk.
*:*************************************************************
*: Name      : lfvCancel
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/09/2002
*: Purpose   : Canncel Button
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfvCancel()
*:*************************************************************
*
FUNCTION lfvCancel

llUpdCon = .F.

*-- End OF lfvCancel.
*:*************************************************************
*: Name      : lfvStatus
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/09/2002
*: Purpose   : Change the status.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  = lfvStatus()
*:*************************************************************
*
FUNCTION lfvStatus

REPLACE cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify

lsLine = CCINCONNO+' '+CCINVNO+' '+STR(NTOTPIECE,7)

SHOW GET lsLine

*-- End OF lfvStatus.
*:*************************************************************
*: Name      : lpClose
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/09/2002
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