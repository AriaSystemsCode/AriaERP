*:****************************************************************
*: Program file  : HLCINLN.PRG
*: Program desc. : Commercial Invoice Lines.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/01/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwLine  , lfvNew  , lfvStyle  , 
*:               :            : lfvRemove, lfvOk   , lfvCancel ,
*:               :            : lfvStatus, lfvUnits, lfvTotLin ,
*:               :            : lpClose.
*:               -------------------------------------------------
*:               : PROCEDURE  : lfShow  . 
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
EXTERNAL ARRAY laData,laScrMode

lnPoint   = 0         && Variable to hold the RECNO
lcStatus  = ' ' 

lcVendor  = ALLTRIM(laVendCod((ASCAN(laVendcod,laData[1])/2),1))
lcInvType = IIF(laData[8]='R','Regulars',IIF(laData[8]='S','Seconds','Balances'))
lccinvno  = laData[2]  && Variable to hold the Invoice no 
lsLine    = 1

PUSH KEY
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose
DO (gcScrDir+gcAct_Appl+"\Hlcinln.SPX")
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
lsLine = ccvllinno+'  '+cstynum+'  '+ccinlotno+;
         ' '+STR(ncinunits)+' '+STR(ncindozen,8,3);
         +'   '+STR(ncindzval,8,3)+'      '+STR(ncintlnvl,10,2)

SHOW GETS ONLY

*-- End Of lfwLine.
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

  CASE laScrMode[1] .OR. laScrMode[4]    && Select or Add Mode
    SHOW GET pbNew     ENABLE  
    SHOW GET pbCancel  ENABLE
    SHOW GET cstynum   ENABLE
    SHOW GET lnTotUnit 
    SHOW GET lnTotDoz  
    SHOW GET lnTotVal  
    SHOW GET ncintlnvl
    
    IF lnLinNo > 0
      SHOW GET PBNew     ENABLE 
      SHOW GET pbCancel  ENABLE
      SHOW GET ccinlotno ENABLE
      SHOW GET ncinunits ENABLE
      SHOW GET ncinDzVal ENABLE
      SHOW GET ctsuqtcat ENABLE
    ELSE
      SHOW GET ccinlotno ENABLE
      SHOW GET ncinunits ENABLE
      SHOW GET ncinDzVal ENABLE
    ENDIF

  CASE laScrMode[3]                      && Edit Mode
    SHOW GET PBNew     ENABLE 
    SHOW GET pbCancel  ENABLE
    SHOW GET ccinlotno ENABLE
    SHOW GET ncinunits ENABLE
    SHOW GET ncinDzVal ENABLE
    SHOW GET cstynum   ENABLE
    SHOW GET ctsuqtcat ENABLE
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

SHOW GET ncinDozen 
SHOW GET lcVendor 
SHOW GET lcInvType
SHOW GET lccinvno 

*-- End Of lfShow.
*:*************************************************************
*: Name      : lfvNew.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Valid For adding new line in the list.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvNew ()
*:*************************************************************
*:
FUNCTION lfvNew

COUNT TO lnFcount
lnFcount = lnFcount+1
lnLinNo  = lnLinNo +1

** For adding new line in the list **
APPEND BLANK
REPLACE cStatus WITH "A",;
        ctsuvencd WITH laData[1],;
        cInvType  WITH laData[8],;
        ccinvno   WITH laData[2],;
        cCvlLinNo WITH IIF(len(ALLTRIM(STR(lnFcount)))=1,;
                           '0'+ALLTRIM(STR(lnFcount)),;
                            ALLTRIM(STR(lnFcount)))

IF lnLinNo > 0 
  SHOW GET pbRemove ENABLE
ELSE
  SHOW GET pbRemove DISABLE
ENDIF 
                            

** To high light the new line **
lsLine = ccvllinno+'  '+cstynum+'  '+ccinlotno+;
         ' '+STR(ncinunits)+' '+STR(ncindozen,8,3);
         +'   '+STR(ncindzval,8,3)+'      '+STR(ncintlnvl,10,2)

SHOW GET pbNew     DISABLE
SHOW GET pbOk      DISABLE
SHOW GET pbCancel  DISABLE
SHOW GET lsLine    
SHOW GET ccinlotno ENABLE
SHOW GET ncinunits ENABLE
SHOW GET ctsuqtcat ENABLE
SHOW GET ncinDzVal ENABLE
SHOW GET ncinDozen 
SHOW GET ncintlnvl

_CUROBJ = OBJNUM(cstynum)
SHOW GET cstynum ENABLE

*-- End Of lfvNew.
*:*************************************************************
*: Name      : lfvStyle.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Valid Style.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvStyle ()
*:*************************************************************
*:
FUNCTION lfvStyle

IF !EMPTY(cstynum) .AND. LASTKEY() = 13

  SELECT HLCSTYLE

  IF SEEK(&lcTempInv..cstynum)
    IF EMPTY(&lcTempInv..ctsuqtcat)    
      REPLACE &lcTempInv..ctsuqtcat WITH HLCSTYLE.ctsuqtcat
    ENDIF  

    SELECT hlcvslin

    LOCATE FOR hlcvslin.cstynum = &lcTempInv..cstynum;
               AND UPPER(ALLTRIM(cCvsCtDis)) = 'LABOR'

    IF FOUND()

      IF !EMPTY(nCvLitmEx)
        SELECT (lcTempInv)
        IF laScrMode[4] OR hlcvslin.nCvLitmEx <> 0
          REPLACE nCinDzVal WITH hlcvslin.nCvLitmEx
        ENDIF  
      ENDIF  

    ELSE 
      SELECT (lcTempInv)
      IF laScrMode[4] OR hlcvslin.nCvLitmEx <> 0
        REPLACE nCinDzVal WITH 0
        SHOW GET nCinDzVal
      ENDIF  
    ENDIF         

    SELECT (lcTempInv)

    REPLACE cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify

    lsLine = ccvllinno+'  '+cstynum+'  '+ccinlotno+;
             ' '+STR(ncinunits)+' '+STR(ncindozen,8,3);
             +'   '+STR(ncindzval,8,3)+'      '+STR(ncintlnvl,10,2)

    SHOW GET lsLine
    SHOW GET cstynum   
    SHOW GET cInvType  ENABLE
    SHOW GET ccinlotno ENABLE
    SHOW GET ncinunits ENABLE
    SHOW GET ncinDzVal ENABLE
    SHOW GET ctsuqtcat ENABLE
    SHOW GET ncinDozen 
    SHOW GET ncintlnvl
  ELSE

    *- Text Message   :- XXXXXXX is not found in the data file.
    *- Text Number    :- 00001
    *- button message :- \<Browse;\?\<Reenter
    *- button Number  :- 42014.
    lnOption = gfModalGen('INM00001B42014','DIALOG',(&lcTempInv..cstynum))

    DO CASE

      CASE lnOption = 1
        DIMENSION laTemp[1]
        laTemp[1]  = ''
        lcSavBrFld = lcBrfields
        lcSaveTtl  = lcFile_Ttl
        lcFile_Ttl = 'Styles'
        lcBrfields ="CSTYNUM :H= 'Style Number',;
                    CSTYSEACD :H= 'Style Season Code',;
                    CSTYRANG :H= 'Style Range',;
                    CSTRANGDS :H= 'Range Description'"
        =gfbrows(.F.,'CSTYNUM,CTSUQTCAT','laTemp')
        lcFile_Ttl = lcSaveTtl
        lcBrfields = lcSavBrFld
        SELECT (lcTempInv)

        IF !EMPTY(laTemp[1])
          REPLACE cstynum   WITH laTemp[1],;
                  ctsuqtcat WITH laTemp[2],;
                  cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify
          lsLine = ccvllinno+'  '+cstynum+'  '+ccinlotno+;
                   ' '+STR(ncinunits)+' '+STR(ncindozen,8,3);
                   +'   '+STR(ncindzval,8,3)+'      '+STR(ncintlnvl,10,2)
          SHOW GET lsLine
          SELECT hlcvslin
          LOCATE FOR hlcvslin.cstynum = &lcTempInv..cstynum;
                 .AND. UPPER(ALLTRIM(cCvsCtDis)) = 'LABOR'

          IF FOUND()
            IF !EMPTY(nCvLitmEx)
              SELECT (lcTempInv)
              IF laScrMode[4] OR hlcvslin.nCvLitmEx <> 0
                REPLACE nCinDzVal WITH hlcvslin.nCvLitmEx
              ENDIF  
            ENDIF  
          ELSE 
            SELECT (lcTempInv)
            IF laScrMode[4] OR hlcvslin.nCvLitmEx <> 0
              REPLACE nCinDzVal WITH 0
            ENDIF  
            SHOW GET nCinDzVal
          ENDIF         
          SHOW GET lsLine
          SHOW GET cstynum   
          SHOW GET ccinlotno ENABLE
          SHOW GET ncinunits ENABLE
          SHOW GET ncinDzVal ENABLE
          SHOW GET ctsuqtcat ENABLE
          SHOW GET ncinDozen
          SHOW GET ncintlnvl
        ELSE
          REPLACE cstynum WITH ' ',;
                  nCinDzVal WITH 0,;
                  ctsuqtcat WITH ' '
                  
          SHOW GET nCinDzVal 
          SHOW GET ctsuqtcat 
                   
          _CUROBJ = OBJNUM(cstynum)
          SHOW GET cstynum
        ENDIF  

      CASE lnOption = 2
        SELECT (lcTempInv)
        REPLACE cstynum WITH ' '
        _CUROBJ = OBJNUM(cstynum)
        SHOW GET cstynum

    ENDCASE  
  ENDIF
ENDIF

IF !EMPTY(cstynum)
  SHOW GET pbNew    ENABLE
  SHOW GET pbOk     ENABLE
  SHOW GET pbCancel ENABLE
ENDIF

SELECT (lcTempInv)
SHOW GET lsLine

*-- End OF lfvStyle.
*:*************************************************************
*: Name      : lfvRemove.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Valid For remove line from the list.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvRemove ()
*:*************************************************************
*:
FUNCTION lfvRemove

lcStatus = SUBSTR('DDS',AT(cStatus,'SMA'),1)  && DELETE

REPLACE cStatus WITH lcStatus

DELETE

IF !EOF()
  SKIP
ELSE
  SKIP-1  
ENDIF  

REPLACE REST ccvllinno WITH;
             IIF(LEN(ALLTRIM(STR(VAL(ccvllinno)-1,2)))=1,;
                 "0"+STR(VAL(ccvllinno)-1,1),;
                 STR(VAL(ccvllinno)-1,2)),;
             cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)

** To high light the new line **
lsLine = ccvllinno+'  '+cstynum+'  '+ccinlotno+;
         ' '+STR(ncinunits)+' '+STR(ncindozen,8,3);
         +'   '+STR(ncindzval,8,3)+'      '+STR(ncintlnvl,10,2)

lnFcount = IIF(RECNO() > RECCOUNT(),0,RECNO())

** Summsion on the totals **
SUM ncinunits TO lnTotUnt
SUM ncindozen TO lnTotDoz
SUM ncindzval TO lnTotVal

lnTotVal = ROUND(lnTotVal,2)
IF lnFcount > 0
  GO lnFcount
ENDIF  

IF lnLinNo > 0
  lnLinNo = lnLinNo-1
ENDIF  

IF lnLinNo > 0 
  SHOW GET pbRemove ENABLE
ELSE
  SHOW GET pbRemove DISABLE
ENDIF 

SHOW GET lnTotUnt
SHOW GET lnTotDoz
SHOW GET lnTotVal

SHOW GET pbNew    ENABLE
SHOW GET pbOk     ENABLE
SHOW GET pbCancel ENABLE

_CUROBJ=OBJNUM(lsLine)

SHOW GET lsLine

*-- End Of lfvRemove.
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

llUpdInv=.T.
 
*-- End Of lfvOk.
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

lnTotUnt = laData[9]
lnTotDoz = laData[10]   
lnTotVal = ROUND(laData[11],2)
llUpdInv = .F.

*-- End Of lfvCancel.
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

REPLACE cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify

lnFcount = IIF(RECNO() > RECCOUNT(),0,RECNO())

** Summsion on the totals **
SUM ncinunits TO lnTotUnt
SUM ncindozen TO lnTotDoz
SUM ncindzval TO lnTotVal

lnTotVal = ROUND(lnTotVal,2)

IF lnFcount > 0
  GO lnFcount
ENDIF  

SHOW GET lnTotUnt
SHOW GET lnTotDoz
SHOW GET lnTotVal

lsLine = ccvllinno+'  '+cstynum+'  '+ccinlotno+;
         ' '+STR(ncinunits)+' '+STR(ncindozen,8,3);
         +'   '+STR(ncindzval,8,3)+'      '+STR(ncintlnvl,10,2)

SHOW GET lsLine

*-- End OF lfvStatus.
*:*************************************************************
*: Name      : lfvUnits.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvUnits ()
*:*************************************************************
*:
FUNCTION lfvUnits

IF ncinunits > 0
  lcStatus = SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify
  REPLACE nCinDozen WITH ncinunits/12,;
            cStatus WITH lcStatus
ENDIF

lnFcount = IIF(RECNO() > RECCOUNT(),0,RECNO())

SUM ncinunits TO lnTotUnt
SUM ncindozen TO lnTotDoz
SUM ncindzval TO lnTotVal
lnTotVal = ROUND(lnTotVal,2)

IF lnFcount > 0
  GO lnFcount
ENDIF  

SHOW GET lnTotUnt
SHOW GET lnTotDoz
SHOW GET lnTotVal
SHOW GET nCinDozen

lsLine = ccvllinno+'  '+cstynum+'  '+ccinlotno+;
         ' '+STR(ncinunits)+' '+STR(ncindozen,8,3);
         +'   '+STR(ncindzval,8,3)+'      '+STR(ncintlnvl,10,2)

SHOW GET lsLine

*-- End Of lfvUnits.
*:*************************************************************
*: Name      : lfvTotLin.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvTotLin ()
*:*************************************************************
*:
FUNCTION lfvTotLin

lnExt = ncinDozen * ncindzval  && Multiplacation of the Total Line field
IF lnExt # 0
  IF lnExt > 999999.99
    *- Message Text   :- Computed extention too long.
    *- Message No.    :- 000000.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM00000B00000','F','ALERT',' ','Computed extension is too long.')
    _CUROBJ = OBJNUM(ncindzval)  && For saving the object number that I came from 
  ELSE
    lcStatus = SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify
    REPLACE ncintlnvl WITH ROUND(ncinDozen * ncindzval,2),;
            cStatus WITH lcStatus
  ENDIF  

  lnFcount = IIF(RECNO() > RECCOUNT(),0,RECNO())

  SUM ncinunits TO lnTotUnt
  SUM ncindozen TO lnTotDoz
  SUM ncindzval TO lnTotVal
  lnTotVal = ROUND(lnTotVal,2)

  IF lnFcount > 0
    GO lnFcount
  ENDIF  

  SHOW GET lnTotUnt
  SHOW GET lnTotDoz
  SHOW GET lnTotVal
  SHOW GET nCinDozen
  SHOW GET ncintlnvl

  lsLine = ccvllinno+'  '+cstynum+'  '+ccinlotno+;
           ' '+STR(ncinunits)+' '+STR(ncindozen,8,3);
           +'   '+STR(ncindzval,8,3)+'      '+STR(ncintlnvl,10,2)

  SHOW GET lsLine 
ENDIF

*-- End Of lfvTotLin.
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
