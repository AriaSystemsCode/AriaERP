*:****************************************************************
*: Program file  : HLQUOTA.PRG
*: Program desc. : Quota Allotements.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/03/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lpShow , lfvData_1  ,lfvData_2
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Begin Declaration variables.
*-- laCodInfo :- Array hold the Vendor Code.

*-- End Declaration variables.

EXTERNAL ARRAY laData,laKeyField
DECLARE laKeyField [2,4] , laCodInfo [1,10] , laVend [1,2]

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.F.
laKeyField[1,3] = 'VENQTALM'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[2]'
laKeyField[2,2] =.T.
laKeyField[2,3] = 'VENQTALM'
laKeyField[2,4] = 2


STORE ' ' TO laCodInfo , lcModal , laVend 
STORE 1   TO lnVend

IF !gfSetup()
  RETURN
ENDIF  
 
*-- Create Care Label Array [Begin]

laCodInfo[1,01] = "CTSUVENCD"
laCodInfo[1,02] = "laVend"
laCodInfo[1,03] = "lnVend"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.
laCodInfo[1,06] = .F.
laCodInfo[1,10] = "cTsuVenCd"

IF !WEXIST(gcBaseWind)
  *-- Screen fields variable, to be used to build the laData array.
  lcScFields = "cTsuVenCd,cTsuQtCat,dcInvDate,nTotAlotm"
  SCATTER FIELDS &lcScFields. MEMO TO laData BLANK
  =gfwCodePop ( @laCodInfo, "CTSUVENCD" ,"L" )
ENDIF
 
PUSH KEY
DO (gcScrDir+gcAct_Appl+"\HLQUOTA.SPX")
POP KEY

 
RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lpShow
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/03/2002
*: Purpose   : Show function
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : Do lpShow
*:*************************************************************
*:
PROCEDURE lpShow
 
EXTERNAL ARRAY laScrMode,laGrmntDis

DO CASE

  CASE laScrMode[2]                   && VIEW Mode
    lnVend  = ASUBSCRIPT(laVend,ASCAN(laVend,laData[1]),1)
    SHOW GET lnVend   DISABLE
    SHOW GET laData[2]  DISABLE
    SHOW GET laData[3]  DISABLE
    SHOW GET laData[4]  DISABLE
    
  CASE laScrMode[1]                   && SELECT Mode
    IF EMPTY(laData[1])
      STORE 1 TO lnVend
    ENDIF
    SHOW GET lnVend 
  
  CASE laScrMode[4] .OR. laScrMode[3] && ADD OR EDIT Mode
    IF laScrMode[4]
      laData[3] = gdSysDate
    ENDIF
    SHOW GET laData[3]  ENABLE
    SHOW GET laData[4]  ENABLE
ENDCASE    

*-- End OF lpShow
*:*************************************************************
*: Name      : lfvData_1
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Function to Validate laData[1]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_1
*:*************************************************************
*:
FUNCTION lfvData_1

IF !( ALLTRIM(laData[1])) == ALLTRIM(laVend[lnVend,2]))
  laData[1] = ALLTRIM(laVend[lnVend,2])
ENDIF

  
*-- End OF lfvData_1.
*:*************************************************************
*: Name      : lfvData_2
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Function to Validate laData[2]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_2
*:*************************************************************
*:
FUNCTION lfvData_2

IF !( ALLTRIM(laData[1])) == ALLTRIM(laVend[lnVend,2]))
  laData[1] = ALLTRIM(laVend[lnVend,2])
ENDIF

 
IF !EMPTY(ALLTRIM(ladata[1])) AND !EMPTY(laData[2]) AND LASTKEY()= 13

  = SEEK (laData[1]+laData[2])
  IF FOUND()
    SCATTER FIELDS &lcScFields MEMO TO laData
    laScrMode=.F.
    laScrMode[2]=.T.
    SHOW GETS 
  ELSE
    *- Text Message   :- XXXXXXX is not found in the data file.
    *- Text Number    :- 00001
    *- button message :- \<Add;\<Browse;\?\<Reenter
    *- button Number  :- 42003.

    lcMessage = ALLTRIM(laVend((ASCAN(laVend,laData[1])/2),1)) + ' / ';
                + laData[2]
    
    lnOption = gfModalGen('INM00001B42003','DIALOG',lcMessage )
    DO CASE

      CASE lnOption = 1
        laScrMode = .F.
        laScrMode[4] = .T.
        SHOW GETS
      CASE lnOption = 2
        = gfBrows()
      CASE lnOption = 3
        STORE '' TO laData[1] , laData[2]
        laScrMode    = .F.
        laScrMode[1] = .T.
        SHOW GETS
    ENDCASE  
  ENDIF  
  
ENDIF

*-- End OF lfvData_2.
*:*************************************************************