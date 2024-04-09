*:************************************************************************
*: Program file  : LFBETWEN.PRG
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
*: Modifications
*: *B604146,1 AME 01/18/2001 Fixed the bug of message no records to disply when filtering in
*: *B604146,1     01/18/2001 Dates due to y2k problem.
*:************************************************************************
PARAMETERS lcOGHeader,lcOGDataTyp,lcOGPicture,lnOGSize,lcOGVld,lcOGVar
lnOGSize=IIF(lcOGDataType='D',10,lnOGSize)
lcOGWhen= IIF(EMPTY(lcOGVld),'','Valid '+lcOGVld )
lcOGValueF=lfTrnsStr(IIF(ATC(lcElmSep,&lcOGVar)>0,SUBSTR(&lcOGVar,;
                     1,ATC(lcElmSep,&lcOGVar)-1),&lcOGVar))
                     
lcOGValueTo=lfTrnsStr(IIF(ATC(lcElmSep,&lcOGVar)>0,SUBSTR(&lcOGVar,;
                     ATC(lcElmSep,&lcOGVar)+1),''))
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
  &lcOGVar=lfChngToStr(lcOGValueF)+lcElmSep+lfChngToStr(lcOGValueTo)
  CLEAR READ  
ENDIF


FUNCTION lfTrnsStr
PARAMETER lcOGValue
DO CASE
  CASE lcOGDataTyp $ 'CM'
    RETURN lcOGValue
  CASE lcOGDataTyp = "N"
    RETURN VAL(lcOGValue)     
  CASE lcOGDataTyp = 'D'
    RETURN CTOD(lcOGValue)     
  CASE lcOGDataTyp = 'L'
    RETURN lcOGValue  
ENDCASE  


FUNCTION lfChngToStr
PARAMETER lcOGValue
PRIVATE  lcSetcen,lcReturn
DO CASE
  CASE lcOGDataTyp $ 'CM'
    RETURN lcOGValue
  CASE lcOGDataTyp = "N"
    RETURN ALLTRIM(STR(lcOGValue))     
  CASE lcOGDataTyp = 'D'
    *B604146,1 AME [Start]  set cent on to get complete date into string.
    *RETURN DTOC(lcOGValue) 
    lcSetCen = SET("CENTURY")
    SET CENTURY ON
    lcReturn = DTOC(lcOGValue)     
    SET CENTURY &lcSetCen
    RETURN lcReturn
    *B604146,1 AME [End]
  CASE lcOGDataTyp = 'L'
    RETURN lcOGValue
ENDCASE  
