*:***************************************************************************
*: Program file  : SOREV30
*: Program desc. : Open Orders Summaries for REV10
*: For Report    : SOVEN30.FRX
*: System        : ARIA4XP.
*: Module        : Sales Order (SO)
*: Developer     : Mostafa Eid (MOS)
*: Date          : 05/26/2008
*: Reference     : C201006
*:**************************************************************************
*: Procedures    : lpCreatFil , lpCollData , 
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
SELECT Codes
SET ORDER TO TAG cCode_No
 IF !gfSEEK("NSEASON",'Codes')
  *-- Message <This company has no Season codes, Cannot proceed.>
  *-- Buttons <                             OK                             >
  =gfModalGen("TRM000000B00000","DIALOG",'','',"This company has no  Season codes, Cannot proceed.")
  RETURN  
 ENDIF
 
*--if filter changed
loOgScroll.cCRorientation = 'P'
IF llOGFltCh
  DO lfCreateTemp
  DO lpCollData

ENDIF  
SELECT (lcWorkFile)
LOCATE
IF EOF()
  *-- Message <There are no records to display>
  *-- Buttons <               OK              >
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ELSE
DO gfDispRe WITH EVAL('lcRpName')
ENDIF
*-- End of Report.

*!**************************************************************************
*! Name      : lpCollData
*: Developer : Mostafa Eid (mos)
*: Date      : 05/26/2008
*! Purpose   : To collect data for the report.
*!**************************************************************************
*! Example   : DO lpCollData
*!**************************************************************************

PROCEDURE lpCollData
PRIVATE lcOrder
lcOrder = ''     

SELECT OrdLine
llUseSeason  = .F.
lcSeaFile  = ''
lnSeaPos = ASCAN(loOgScroll.laOgFXFlt,"ORDLINE.SEASON")
IF lnSeaPos > 0 
  lnSeaPos = ASUBSCRIPT(loOgScroll.laOgFXFlt,lnSeaPos,1)
  lcSeaSel =IIF(!EMPTY(loOgScroll.laOgFXFlt[lnSeaPos,6]),loOgScroll.laOgFXFlt[lnSeaPos,6],'')
  IF !EMPTY(lcSeaSel) 
    lcSeaFile = loOGScroll.gfTempName()
    llUseSeason = IIF(LEN(lcSeaSel)>0,.T.,.F.) AND lfConvertToCursor(lcSeaSel,'SEASON',lcSeaFile)
  ENDIF   
ENDIF          
SELECT ordline 
SCAN FOR  cOrdType <> 'T' AND IIF(llUseSeason,SEEK(OrdLine.SEASON,lcSeaFile),.T.)         
   IF GFSEEK(CORDTYPE+ORDER,'Ordhdr') AND(OrdHdr.Status $ "OH") && AND 
   ELSE 
     LOOP
   ENDIF
  *--for Open Qty (from ordhdr file)
  WAIT WINDOW "Collecting data for order# " + OrdLine.Order NOWAIT
  
  IF SEEK(OrdLine.Season,lcWorkFile) 
    SELECT (lcWorkFile)
    REPLACE nOpenQty   WITH nOpenQty  + OrdLine.TotQty,;
            nOpenAmnt  WITH nOpenAmnt + OrdLine.TotQty * OrdLine.Price,;
            nAlcatQty  WITH nAlcatQty + OrdLine.TotPik  ,;
            nAlcatAMNT WITH nAlcatAmnt+ OrdLine.TotPik * OrdLine.Price
  ELSE
    m.nOpenQty   = OrdLine.TotQty
    m.nOpenAmnt  = OrdLine.TotQty * OrdLine.Price
    m.nAlcatQty  = OrdLine.TotPik  
    m.nAlcatAmnt = OrdLine.TotPik * OrdLine.Price
    m.cSesnCod   = OrdLine.SEASON
    m.cSeason    = gfCodDes(OrdLine.SEASON,'SEASON')  
    INSERT INTO (lcWorkFile) FROM MEMVAR
  ENDIF
ENDSCAN      

*-- End of lpCollData.

*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Mostafa Eid [MOS]
*! Date      : 05/29/2008
*! Purpose   : Function to create temp.  file 
*!*************************************************************
FUNCTION lfCreateTemp

DIMENSION laStruArr[6, 4]

laStruArr[1  ,1] = 'cSeason'
laStruArr[1  ,2] = 'C'
laStruArr[1  ,3] = 30
laStruArr[1  ,4] = 0

laStruArr[2  ,1] = 'cSesnCod'
laStruArr[2  ,2] = 'C'
laStruArr[2  ,3] = 6
laStruArr[2  ,4] = 0

laStruArr[3  ,1] = 'nAlcatAmnt'
laStruArr[3  ,2] = 'N'
laStruArr[3  ,3] = 13
laStruArr[3  ,4] = 2

laStruArr[4  ,1] = 'nAlcatQty'
laStruArr[4  ,2] = 'N'
laStruArr[4  ,3] = 5
laStruArr[4  ,4] = 0

laStruArr[5  ,1] = 'nOpenAmnt'
laStruArr[5  ,2] = 'N'
laStruArr[5  ,3] = 11
laStruArr[5  ,4] = 2

laStruArr[6  ,1] = 'nOpenQty'
laStruArr[6  ,2] = 'N'
laStruArr[6  ,3] = 7
laStruArr[6  ,4] = 0

=gfCrtTmp(lcWorkFile,@laStruArr,'cSesnCod',lcWorkFile,.T.)

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 06/22/2006
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************

FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 

CASE  ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
ENDCASE 

 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.

FUNCTION lfwRepWhen()
IF !(USED('OrdLine'))  
=gfOpenTable('OrdLine','OrdLine','SH') 
ENDIF 
IF !(USED('OrdHdr'))
=gfOpenTable('OrdHdr','OrdHdr','SH') 
ENDIF 
RETURN .T.
