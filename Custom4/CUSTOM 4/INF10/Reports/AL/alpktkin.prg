*:***************************************************************************
*: Program file  : ALPKTKIN.PRG
*: Program desc. : CUSTOMIZED PICKING TICKET Form FOR INF10
*: Date          : 08/30/2007
*: System        : ARIA4XP
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mariam Mazhar [MMT]
*: Tracking NO   : C200844
*: TICKET NO     : T20070810.0003
*:***************************************************************************
*Modifications:
*:***************************************************************************
IF USED(lcOrdFile) AND lcTmpOrdL = lcOrdFile
  lcOrdFile = loogscroll.gftempName()
ENDIF 

SELECT (lcTmpOrdL)
lcOrdRelat = SET("Relation")
lcIndex = KEY()

DIMENSION laFlStruct[1,18]
lnFldLen = AFIELDS(laFlStruct)

lnMBins = 0
lnNfrst = 0

lnMBins = ASCAN(laFlStruct,"MBINS")
lnNfrst = ASCAN(laFlStruct,"CFRSTBIN")

IF lnMBins = 0 AND lnNfrst = 0
  DIMENSION laFlStruct[lnFldLen +2,18]
  laFlStruct[lnFldLen +1,1] =  "MBINS"
  laFlStruct[lnFldLen +1,2] =  "M"
  laFlStruct[lnFldLen +1,3] = 10
  laFlStruct[lnFldLen +1,4] = 0

  laFlStruct[lnFldLen +2,1] = "CFRSTBIN"
  laFlStruct[lnFldLen +2,2] = "C"
  laFlStruct[lnFldLen +2,3] = 10
  laFlStruct[lnFldLen +2,4] = 0



  FOR lncnt=7 TO 16
      STORE SPACE(0) TO laFlStruct[lnFldLen +1,lnCnt]
  ENDFOR  
  STORE 0 TO laFlStruct[lnFldLen +1,17],laFlStruct[lnFldLen +1,18]

  FOR lncnt=7 TO 16
      STORE SPACE(0) TO laFlStruct[lnFldLen +2,lnCnt]
  ENDFOR  
  STORE 0 TO laFlStruct[lnFldLen +2,17],laFlStruct[lnFldLen +2,18]
ENDIF 

*C200844,1 MMT 09/24/2007 Fix bug of picking ticket not printing if WHSLOC file is empty[Start]
*=gfCrtTmp(lcOrdFile ,@laFlStruct,iif(lcRpSrtBin = 'Y',"CGRUPDETAL+CFRSTBIN+PIKTKT+ORDER+STR(LINENO,6)",lcIndex),lcOrdFile,.T.)
=gfCrtTmp(lcOrdFile ,@laFlStruct,iif( lcRpPrnBin = 'Y' AND lcRpSrtBin = 'Y',"PIKTKT+CGRUPDETAL+CFRSTBIN+ORDER+STR(LINENO,6)",lcIndex),lcOrdFile,.T.)
*C200844,1 MMT 09/24/2007 Fix bug of picking ticket not printing if WHSLOC file is empty[End]

IF USED('whsloc')
  =gfOpenTable(oAriaApplication.DataDir+'WHSLOC',oAriaApplication.DataDir+'WHSLOC','SH')
ENDIF 




SELECT (lcTmpOrdL)
llFirst = .F.

SCAN 
 SCATTER MEMO MEMVAR
 
 *C200844,1 MMT 09/24/2007 Fix bug of picking ticket not printing if WHSLOC file is empty[Start]
 m.MBins = ''
 m.cFrstBin = ''
 llFirst = .F.
 *C200844,1 MMT 09/24/2007 Fix bug of picking ticket not printing if WHSLOC file is empty[End]
 
 IF gfSEEK(EVAL(lcPiktktTemp+'.cWareCode'),'WHSLOC') 
   SELECT WHSLOC
   llFirst = .F.
   m.MBins = ''
   m.cFrstBin = ''
   SCAN REST WHILE CWARECODE+CLOCATION+STYLE+COLOR =EVAL(lcPiktktTemp+'.cWareCode') FOR ;
     style = EVALUATE(lcTmpOrdL+'.style')
     IF !llFirst 
       m.cFrstBin = WHSLOC.CLOCATION
       llFirst = .T.
     ENDIF   
     m.MBins = m.MBins + IIF(EMPTY(m.MBins),'',',')+ ALLTRIM(WHSLOC.CLOCATION)
   ENDSCAN 
 
 *C200844,1 MMT 09/24/2007 Fix bug of picking ticket not printing if WHSLOC file is empty[Start]
 ENDIF  
 *C200844,1 MMT 09/24/2007 Fix bug of picking ticket not printing if WHSLOC file is empty[End]
 
 SELECT (lcOrdFile)
 APPEND BLANK 
 GATHER MEMO MEMVAR   
 

 
 *C200844,1 MMT 09/24/2007 Fix bug of picking ticket not printing if WHSLOC file is empty[Start]
 *ENDIF  
 *C200844,1 MMT 09/24/2007 Fix bug of picking ticket not printing if WHSLOC file is empty[End]
 
 SELECT (lcTmpOrdL)
ENDSCAN


SELECT (lcOrdFile) 

SET RELATION TO &lcOrdRelat.
lcTmpOrdL = lcOrdFile

RETURN 


