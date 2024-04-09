*:************************************************************************
*: Program file  : LFVARBET.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
PARAMETERS lcOGHeader,lcOGDataTyp,lcOGPicture,lnOGSize,lcOGVld,lcOGVar,lcOGVar2
lnOGSize=IIF(lcOGDataType='D',10,lnOGSize)
lcOGWhen= IIF(EMPTY(lcOGVld),'','Valid '+lcOGVld )
lcOGValueF = &lcOGVar
lcOGValueTo = &lcOGVar2

DO OGBetwen.SPR
  lcOGPrompt=IIF(EMPTY(ALLTRIM(&lcOGVar)),'Between',;
                  SUBSTR(STRTRAN(&lcOGVar,lcElmSep,','),1,20))  
*  SHOW OBJECT _CUROBJ PROMPT lcOGPrompt



FUNCTION lfvOk
IF lcOGValueTo < lcOGValueF
  WAIT '"From'+'" value must be Less than or equal to the '+'"TO'+'" Value' WINDOW NOWAIT
  _CUROBJ=OBJNUM(lcOGValueTo)
  RETURN
ELSE
  &lcOGVar = lcOGValueF
  &lcOGVar2 = lcOGValueTo  
  CLEAR READ  
ENDIF
