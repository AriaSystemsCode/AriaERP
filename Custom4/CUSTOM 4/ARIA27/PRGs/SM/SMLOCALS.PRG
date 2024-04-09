*:************************************************************************
*:
*: Procedure file: SMLOCALS.PRG 
*:                 (Locals)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: 
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*
*B600698,1 M.H 09/21/95 'Subscript out of bounds' when press the locals button.
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*:************************************************************************

PARAMETERS lcScrMode
EXTERNAL ARRAY laData,laScrMode

*B600698,1 M.H 09/21/95
*DIMENSION laSavData[26],laSavSet[6] && By Mohamed Hassan
DIMENSION laSavData[ALEN(laData)],laSavSet[6]
*B600698,1 M.H 09/21/95 End

IF TYPE('laScrMode') = 'U'
  DECLARE laScrMode[4,1]
  laScrMode    = .F.
  laScrMode[3] = .T.
ENDIF

lcScrMode=IIF(PARAMETERS()>0,lcScrMode,"C")

laSavSet=""

DO lpSavSet                 

=ACOPY(laData,laSavData)
lcCurrTemp = 123456.7890
lcDateTemp = gdSysDate
lcDateSep  = SUBSTR(DTOC(gdSysDate),IIF(SET("DATE")="JAPAN",;
                 IIF(SET("CENTURY")="ON",5,3),3),1)

laData[21] = IIF(!EMPTY(laData[21]),ALLTRIM(laData[21]),SET("CURRENCY"))
laData[22] = IIF(!EMPTY(laData[22]),ALLTRIM(laData[22]),SET("CURRENCY",2))
laData[23] = IIF(!EMPTY(laData[23]),ALLTRIM(laData[23]),SET("DATE"))
laData[24] = IIF(!EMPTY(laData[24]),ALLTRIM(laData[24]),SET("CENTURY"))
laData[25] = IIF(!EMPTY(laData[25]),ALLTRIM(laData[25]),SET("SEPARATOR"))
laData[26] = IIF(!EMPTY(laData[26]),ALLTRIM(laData[26]),gcTelTemp)
lcDateTemp = gdSysDate
lcDateSep  = SUBSTR(DTOC(gdSysDate),IIF(SET("DATE")="JAPAN",;
                    IIF(SET("CENTURY")="ON",5,3),3),1)
lcCent     = IIF(SET("CENTURY")="ON","\<Century OFF","\<Century ON")

=lfModiSet()

*laData[26]=gcTelTemp

*E300683,4 Call *.SPR from screens directory
* DO smlocals.SPR 
DO (gcScrDir + gcWinAppl + '\smlocals.SPR')
*E300683,4 end   
IF lcScrMode<>"C" 
  DO lpRstorSet
ELSE
  =gpDispStat()  
ENDIF

*!**************************************************************************
*!
*!      Function : lflocShow
*!
*!**************************************************************************
*
FUNCTION lflocShow
EXTERNAL ARRAY laData,laScrMode

laData[21] = SET("CURR")
rbCurr     = laData[21]
puDateTyp  = SET("DATE")
laData[25] = PADR(SET("SEPARATOR"),1)
laData[24] = IIF(SET("CENTURY")="ON","ON","OFF")
lcDateTemp = gdSysDate
lcCurrTemp = 123456.7890
lcDateSep  = SUBSTR(DTOC(gdSysDate),IIF(pudateTyp="JAPAN",;
                    IIF(SET("CENTURY")="ON",5,3),3),1)
lcCent     = IIF(SET("CENTURY")="ON","\<Century OFF","\<Century ON")

IF laScrMode[2] OR laScrMode[1]
  SHOW GETS DISABLE ONLY
  SHOW GET PBOK ENABLE
ELSE
  SHOW GET pbCent,1 PROMPT lcCent
  SHOW GETS ONLY
  SHOW GET lcCurrTemp
ENDIF

*!**************************************************************************
*!
*!      Function : lfvCurr
*!
*!**************************************************************************
*
FUNCTION lfvCurr
SET CURRENCY &rbCurr
SHOW GETS

*!**************************************************************************
*!
*!      Function : lfvCurrDat
*!
*!**************************************************************************
* 
FUNCTION lfvCurrDat
EXTERNAL ARRAY laData

lnCount = 1
DO WHILE lnCount <= LEN(laData[22])
  IF SUBSTR(laData[22],lnCount,1) $ "0123456789"
    RETURN .F.
  ENDIF
  lnCount = lnCount + 1
ENDDO   
SET CURR TO RTRIM(laData[22])
SHOW GETS  

*!**************************************************************************
*!
*!      Function : lfvDateTyp
*!
*!**************************************************************************
* 
FUNCTION lfvDateTyp

SET DATE TO (puDateTyp)
lcDateSep  = SUBSTR(DTOC(gdSysDate),IIF(pudateTyp="JAPAN",;
                    IIF(SET("CENTURY")="ON",5,3),3),1)
laData[23] = puDateTyp                

SHOW GETS 
 
*!**************************************************************************
*!
*!      Function : lfvSepar
*!
*!**************************************************************************
*
FUNCTION lfvSepar
EXTERNAL ARRAY laData

IF laData[25] $ "0123456789"
  RETURN .F.
ELSE  
  SET SEPARATOR TO (ALLTRIM(laData[25]))
  SHOW GETS
ENDIF  

*!**************************************************************************
*!
*!      Function : lfvCent
*!
*!**************************************************************************
*  
FUNCTION lfvCent
lcCent = IIF(SET("CENTURY") = "ON" , "OFF" , "ON") 
SET CENTURY &lcCent
SHOW GETS

*!**************************************************************************
*!
*!      Function : lfvTelTemp
*!
*!**************************************************************************
*
FUNCTION lfvTelTemp
EXTERNAL ARRAY laData

lnCount = 1
DO WHILE lnCount <= LEN(laData[26])
  IF SUBSTR(laData[26],lnCount,1) $ "0123456789"
    RETURN .F.
  ENDIF
  lnCount = lnCount + 1
ENDDO 
IF OCCURS('#',laData[26]) < 7
  RETURN .F.
ENDIF  
gcTelTemp = laData[26]

*!**************************************************************************
*!
*!      Function : lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel
EXTERNAL ARRAY laData

=ACOPY(laSavData,laData)

IF lcScrMode = "C"
  DO lpRstorSet
ENDIF    

*!**************************************************************************
*!
*!      Function : lfModiSet
*!
*!**************************************************************************
*
FUNCTION lfModiSet

SET CURRENCY &laData[21]
SET CURRENCY TO laData[22]
SET DATE TO (laData[23])

IF !EMPTY(laData[24]) 
  SET CENTURY &laData[24]
ELSE
  SET CENTURY OFF
ENDIF 

SET SEPARATOR TO (laData[25])

gcTelTemp = laData[26]

*!**************************************************************************
*!
*!      PROCEDURE : lpSavSet
*!
*!**************************************************************************
*
PROCEDURE lpSavSet
EXTERNAL ARRAY laData

laSavSet[1] = SET("CURR")
laSavSet[2] = SET("CURR",2)
laSavSet[3] = SET("DATE")
laSavSet[4] = SET("CENTURY")
laSavSet[5] = SET("SEPA")
laSavSet[6] = gcTelTemp

IF lcScrMode = "C"
  laData[21] = SET("CURR")
  laData[22] = SET("CURR",2)
  laData[23] = SET("DATE")
  laData[24] = SET("CENTURY")
  laData[25] = SET("SEPA")
  laData[26] = gcTelTemp
ENDIF  

*!**************************************************************************
*!
*!      PROCEDURE  : lpRstorSet
*!
*!**************************************************************************
*  
PROCEDURE lpRstorSet

IF lcScrMode<>"C"
  SET CURRENCY &laSavSet[1]
  SET CURRENCY TO laSavSet[2]
  SET DATE TO (laSavSet[3])
  SET CENTURY &laSavSet[4]
  SET SEPARATOR TO (laSavSet[5])
  gcTelTemp = laSavSet[6]
ENDIF
