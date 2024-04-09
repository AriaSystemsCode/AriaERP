**********************************************************************
*: Program file  : ICOTSREN.PRG (C102754)
*:                 This report is a conversion of the 2.6 report
*:                 REN200.PRG (C# 101368) (For Aria2.6)
*: Program desc. : OPEN TO SELL REPORT
*:               : (FOR REN)
*:         System: Aria Apparel System (A27)
*:      Developer: TMI - TAREK MOHAMED IBRAHIM
*:      Entry ID : C#102754,1
*:********************************************************************
*: Calls : FUNCTIONS  : lfSayHdr,lfSayBody,lfSaySub,
*:                    : lfPrnQty,lfPrSubGrd,lfPrnGrd
*:                    : lfCollData.
*:         PROCEDURE  : ENDPRT,RPT_HDR,MSG22,HEADER,GRID
*:********************************************************************
*- NOTE :  exclude the last size.

lcGroup  = SPACE(02)
R_WIDTH  = 'W'
ROW      = 0
PAGENO   = 0
lcCurrSty = SPACE(12)
R_TITLE   = " OPEN TO SELL REPORT "

DIMENSION laStyPrc[5],laGrpPrc[5],laGrnPrc[5],;
          laSubTot[5,9],laSubGrp[5,9],laGrndTot[5,9]
STORE 0 TO laStyPrc,laGrpPrc,laGrnPrc,;
           laSubTot,laSubGrp,laGrndTot

IF llOgFltCh 
  =lfCollData()
ENDIF
IF !USED(lcRpTmp)
  USE (gcWorkDir+lcRpTmp+'.DBF') ORDER TAG &lcRpTmp IN 0
ENDIF
SELECT (lcRpTmp)
SET ORDER TO TAG &lcRpTmp

GOTO TOP
IF EOF()
  *-- No record to browse.
  =gfModalGen('INM44032B00000','DIALOG')
  RETURN
ENDIF

IF lcRPSel <> 'A'
  lcBalance = 'TOTPLAN-(TOTORD+TOTSHP)'
  LOCATE FOR IIF( lcRPSel = 'P' , &lcBalance > lnRPMinQ ,;
                               &lcBalance < lnRPMinQ)
ELSE
  LOCATE FOR TOTPLAN <> 0 OR TOTORD <> 0 OR TOTSHP <> 0
ENDIF

IF !FOUND()
  *-- No record to browse.
  =gfModalGen('INM44032B00000','DIALOG')
  RETURN
ENDIF

SET DEVICE TO PRINT

=lfSayHdr()
=lfSAYBody()

DO ENDREPORT
SET DEVICE TO SCREEN

IF USED(lcRpTmp)
  USE IN (lcRpTmp)
ENDIF

*--- END MAIN PROG

*!*************************************************************
*! Name      : lfSayHdr.
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 10/23/2002
*! Purpose   : Function to say header.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : = lfSayHdr()
*!************************************************************
FUNCTION lfSayHdr

PAGENO = PAGENO + 1
DO RPT_HDR WITH 'REN200',''+ lcRpOpTlt,R_WIDTH
ROW = 6
*0****0****1****0****2****0****3****0****4****0****5****0****6****0****7****0****8****0****9****0****0****0****1****0****2****0****3
*STYLE       -COLOR   DESCRIPTION               04      06      08      10      12      14      16        UNITS          TOTAL     "

@ ROW , 000 SAY 'STYLE'
@ ROW , 014 SAY 'COLOR'
@ ROW , 021 SAY "DESCRIPTION"

@ ROW , 043 SAY "    04"
@ ROW , 051 SAY "    06"
@ ROW , 059 SAY "    08"
@ ROW , 067 SAY "    10"
@ ROW , 075 SAY "    12"
@ ROW , 083 SAY "    14"
@ ROW , 091 SAY "    16"
@ ROW , 110 SAY "UNITS    "
@ ROW , 125 SAY "TOTAL"
ROW = ROW + 1

@ ROW , 000 SAY REPLICATE('-',12)
@ ROW , 014 SAY REPLICATE('-',6 )
@ ROW , 021 SAY REPLICATE('-',20)
@ ROW , 043 SAY REPLICATE('-',6 )
@ ROW , 051 SAY REPLICATE('-',6 )
@ ROW , 059 SAY REPLICATE('-',6 )
@ ROW , 067 SAY REPLICATE('-',6 )
@ ROW , 075 SAY REPLICATE('-',6 )
@ ROW , 083 SAY REPLICATE('-',6 )
@ ROW , 091 SAY REPLICATE('-',6 )
@ ROW , 105 SAY REPLICATE('-',10)
@ ROW , 120 SAY REPLICATE('-',10)
ROW = ROW + 1

*!*************************************************************
*! Name      : lfSayBody.
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 10/23/2002
*! Purpose   : Function to say detail.10/23/2002
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : = lfSayBody()
*!************************************************************
FUNCTION lfSayBody

lcCurrSty = SPACE(12)
DO WHILE !EOF() .AND. INKEY() <> 32
  lcCurrSty = STYLE
  *-- Add to print style group subtotal.
  lcGroup = CSTYGROUP
  SCAN REST WHILE CSTYGROUP+STYLE  = lcGroup
    lnBalance = TOTPLAN-(TOTORD+TOTSHP)
    IF IIF(lcRPSel = 'A',TOTPLAN=0 AND TOTORD=0 AND TOTSHP=0,IIF(lcRPSel = 'P',lnBalance < lnRPMinQ,lnBalance > lnRPMinQ))
      LOOP
    ENDIF
    IF ROW >= 54
      =lfSayHdr()
    ENDIF    
    @ ROW , 000 SAY SUBSTR(STYLE,1,lnMajorLen)
    @ ROW , 014 SAY SUBSTR(STYLE,lnNonMajSt,lnColorLen)
    @ ROW , 021 SAY DESC
    ROW = ROW + 1
    @ ROW ,14 SAY ALLT(gfCodDes( SUBSTR(STYLE,lnNonMajSt,lnColorLen) , "COLOR"))
    ROW = ROW + 1
    @ ROW,0 SAY "WHOLESALE "
    @ ROW,13 SAY PRICEA   PICTURE '9999'
    @ ROW , 028 SAY "BUY      :"  
    =lfPrnQty('PLAN',1)
    ROW = ROW + 1
    @ ROW,0 SAY "RETAIL :"
    @ ROW,13 SAY PRICEB   PICTURE '9999'
    @ ROW , 028 SAY "BOOKED   :"  
    =lfPrnQty('ORD',2)
    ROW = ROW + 1
    @ ROW , 028 SAY "% BOOKED :"
    =lfPrnQty('',5)
    ROW = ROW + 1
    @ ROW , 028 SAY "O-BOOKED :"  
    =lfPrnQty('SHP',3)
    ROW = ROW + 1
    @ ROW , 028 SAY "LTS      :"  
    =lfPrnQty('',4)
    ROW = ROW + 1
  ENDSCAN
  =lfSaySub(.F.)
  laSubGrp = 0
  laGrpPrc = 0
ENDDO
=lfPrnGrd()

*!*************************************************************
*! Name      : lfSaySub.
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 10/23/2002
*! Purpose   : Function to say Fotter.10/23/2002
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : = lfSaySub()
*!************************************************************
FUNCTION lfSaySub
PARAMETERS llStySub

@ ROW,0 SAY REPLICATE ('-',131)
ROW = ROW + 1
@ ROW , 000 SAY IIF(llStySub,"STYLE" +' '+lcCurrSty+' ',"Group" +' '+lcGroup+' ');
+"SUBTOTAL"

@ ROW , 028 SAY "BUY      :"  
=lfPrSubGrd(IIF(llStySub,'laSubTot','laSubGrp'),1)
ROW = ROW + 1
@ ROW , 028 SAY "BOOKED   :"  
=lfPrSubGrd(IIF(llStySub,'laSubTot','laSubGrp'),2)
ROW = ROW + 1
@ ROW , 028 SAY "% BOOKED :"
=lfPrSubGrd(IIF(llStySub,'laSubTot','laSubGrp'),5)
ROW = ROW + 1
@ ROW , 028 SAY "O-BOOKED :"  
=lfPrSubGrd(IIF(llStySub,'laSubTot','laSubGrp'),3)
ROW = ROW + 1
@ ROW , 028 SAY "LTS      :"  
=lfPrSubGrd(IIF(llStySub,'laSubTot','laSubGrp'),4)
ROW = ROW + 1
laStyPrc = 0
laSubTot  = 0
@ ROW,0 SAY REPLICATE ('-',131)
ROW = ROW + 1
*!*************************************************************
*! Name      : lfPrnQty.
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 10/23/2002
*! Purpose   : Function to say Fotter.10/23/2002
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : FIELD NAME
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : = lfPrnQty('STRING',SUBSCR)
*!************************************************************
FUNCTION lfPrnQty
PARAMETER lcField , lnSubScr

lnColum  = 43
lnTotPcs = 0
lnValue  = 0
FOR lnIndex = 1 TO 7
  lcSubScr = ALLTRIM(STR(lnIndex))

  DO CASE
    CASE lnSubScr = 1
      *--- BUY == Plan
      lnValue = EVAL(lcField+lcSubScr)
    CASE lnSubScr = 2
     *--- BOOKED == (ORD + SHP)
      lnValue = EVAL('ORd'+lcSubScr)+EVAL('SHP'+lcSubScr)
    CASE lnSubScr = 3
     *--- O-BOOKED == Plan - BOOKED If Negative else 0
      lnTemp = EVAL('PLAN'+lcSubScr)-(EVAL('ORd'+lcSubScr)+EVAL('SHP'+lcSubScr))
      lnValue = IIF(lnTemp < 0 ,lnTemp * -1,0)
    CASE lnSubScr = 4
     *--- LTS == Plan - BOOKED 
     lnValue = EVAL('PLAN'+lcSubScr)-(EVAL('ORd'+lcSubScr) +EVAL('SHP'+lcSubScr))
    CASE lnSubScr = 5
     *--- %BOOKED == BOOKED / Plan
      lnValue = IIF(EVAL('PLAN'+lcSubScr) <> 0,(EVAL('ORD'+lcSubScr)+EVAL('SHP'+lcSubScr))/EVAL('PLAN'+lcSubScr),0)
  ENDCASE

  IF lnSubScr = 5
    @ ROW , lnColum + 1 SAY lnValue * 100 PICTURE '999.99'
  ELSE
    @ ROW , lnColum SAY lnValue PICTURE '999999'
  ENDIF
  lnColum = lnColum + 8
  *-- Add to accumulate style group subtotal.
  laSubGrp[lnSubScr,lnIndex] = laSubGrp[lnSubScr,lnIndex]+lnValue
  laSubTot[lnSubScr,lnIndex] =laSubTot[lnSubScr,lnIndex]+lnValue
  laGrndTot[lnSubScr,lnIndex]=laGrndTot[lnSubScr,lnIndex]+lnValue
  lnTotPcs = lnTotPcs + lnValue
ENDFOR
IF lnSubScr = 5
   lnTotPcs = (TOTORD+TOTSHP)/TOTPLAN * 100
ENDIF
laStyPrc(lnSubScr) = laStyPrc(lnSubScr) + (lnTotPcs * PriceA)
laGrpPrc(lnSubScr) = laGrpPrc(lnSubScr) + (lnTotPcs * PriceA)
laGrnPrc(lnSubScr) = laGrnPrc(lnSubScr) + (lnTotPcs * PriceA)
laSubTot[lnSubScr,9]  = laSubTot[lnSubScr,9]   + lnTotPcs
*-- Add to accumulate style group subtotal.
laSubGrp[lnSubScr,9] = laSubGrp[lnSubScr,9] + lnTotPcs
laGrndTot[lnSubScr,9] = laGrndTot[lnSubScr,9]  + lnTotPcs
IF lnSubScr<> 5
  @ ROW , 108 SAY lnTotPcs PICTURE '9999999'
ELSE
   @ ROW , 110 SAY IIF(TOTPLAN <> 0,(TOTORD+TOTSHP)/TOTPLAN * 100,0) PICTURE '999.99'
ENDIF
@ ROW,121 SAY IIF(lnSubScr <> 5,lnTotPcs * PRICEA,0) PICTURE '999999999'

*!*************************************************************
*! Name      : lfPrSubGrd.
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 10/23/2002
*! Purpose   : Function to say SUB TOTAL.10/23/2002
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : ARRAY NAME,SubScrpt
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : = lfPrSubGrd(ARRAY NAME,2)
*!************************************************************
FUNCTION lfPrSubGrd
PARAMETER lcArray,lnSubScr

lnIndex = 1
lnColum = 43
FOR lnIndex = 1 TO 7
  IF lnSubScr <> 5
    @ ROW ,lnColum SAY &lcArray[lnSubScr,lnIndex] PICTURE '999999'
  ELSE
    @ ROW ,lnColum + 1 SAY IIF(&lcArray[1,lnIndex] <> 0,;
     &lcArray[2,lnIndex]/&lcArray[1,lnIndex] * 100,0) PICTURE '999.99'
  ENDIF
  lnColum = lnColum + 8
ENDFOR
IF lnSubScr <> 5
  @ ROW ,108 SAY &lcArray[lnSubScr,9] PICTURE '9999999'
ELSE
  @ ROW ,110 SAY IIF(&lcArray[1,9] <> 0,;
     &lcArray[2,9]/&lcArray[1,9] * 100,0) PICTURE '999.99'
ENDIF
IF lnSubScr <> 5
  @ ROW,121 SAY IIF(lcArray = "laSubTot",laStyPrc(lnSubScr),;
   IIF(lcArray = "laSubGrp",laGrpPrc(lnSubScr),laGrnPrc(lnSubScr))) ;
   PICTURE '999999999'
ENDIF
*!*************************************************************
*! Name      : lfPrnGrd.
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 10/23/2002
*! Purpose   : Function to say Grand TOTAL.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : = lfPrnGrd()
*!************************************************************
FUNCTION lfPrnGrd

@ ROW , 000 SAY REPLICATE ('-',131)
ROW = ROW + 1
@ ROW,0 SAY "GRAND TOTAL "
@ ROW , 028 SAY "BUY      :"  
=lfPrSubGrd('laGrndTot',1)
ROW = ROW + 1
@ ROW , 028 SAY "BOOKED   :"  
=lfPrSubGrd('laGrndTot',2)
ROW = ROW + 1
@ ROW , 028 SAY "% BOOKED :" 
=lfPrSubGrd('laGrndTot',5)
ROW = ROW + 1
@ ROW , 028 SAY "O-BOOKED :"  
=lfPrSubGrd('laGrndTot',3)
ROW = ROW + 1
@ ROW , 028 SAY "LTS      :"  
=lfPrSubGrd('laGrndTot',4)
ROW = ROW + 1
@ ROW , 000 SAY REPLICATE ('-',131)
ROW = ROW + 1
laSubTot  = 0

*!*************************************************************
*! Name      : lfCollData.
*! Developer : TMI - TAREK MOHAMED IBRAHIM
*! Date      : 10/23/2002
*! Purpose   : Selection grid function.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Returns           : None
*!*************************************************************
*! Example           : = lfCollData()
*!************************************************************
FUNCTION lfCollData

lcRpExp = lcRpExp + IIF(!EMPTY(lcRpPat),' AND PATTERN = "'+lcRpPat+'"','')
*lcRpExp = lcRpExp + IIF(lcRPACB='A',' AND STATUS  = "A"',;
                         IIF(lcRPACB='C',' AND STATUS  = "C"',''))
lcRpExp = lcRpExp + IIF(lcRPACB='A',' AND STATUS  = "A"',;
                         IIF(lcRPACB='C',' AND STATUS  = "X"',''))
lcRpExp = lcRpExp + IIF(lcRpMakUse='M',' AND STYLE.MAKE',;
                         IIF(lcRpMakUse='Y',' AND !STYLE.MAKE',''))

WAIT WINDOW NOWAIT 'Selecting records, Please wait...'
SELECT STYLE
IF USED(lcRpTmp)
  USE IN &lcRpTmp
ENDIF
COPY ALL TO &gcWorkDir.&lcRpTmp FOR EVAL(lcRpExp)
SELECT 0
USE (gcWorkDir+lcRpTmp+'.DBF') EXCL
INDEX ON CSTYGROUP+STYLE TAG &lcRpTmp
*-- end of lfCollData.

*!*************************************************************
*! Name      : lfvOGStyle 
*! Developer : Ahmed Salah Shalaby -(SSH)
*!      Date : 05/10/99
*! Purpose   : Valid function of the Style
*!*************************************************************
*! Called from : Option grid [Style Get field]
*!*************************************************************
*! Calls       : gfStyBrw()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOGStyle
PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = ALLTRIM(EVALUATE(SYS(18)))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))

  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.

  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , laOldVal)
  &lcObjName = ALLTRIM(lcObjVal)

ENDIF    

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)
*-- end of lfvOGStyle.

*!*************************************************************
*! Name      : lfwOldVal 
*! Developer : Ahmed Salah Shalaby -(SSH)
*!      Date : 05/10/99
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvFabric 
*! Developer : 
*! Date      : 01/27/99
*! Purpose   : Validation function for validating Fabric Code
*!*************************************************************
*! Called from : Only this color [Option Grid]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

lnAlias   = SELECT(0)
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SELECT Fabric
lcFabOrder = ORDER()
SET ORDER TO Fabric
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    
SELECT Fabric
SET ORDER TO &lcFabOrder
SELECT(lnAlias)

*:**************************************************************************
*:* Name        : lfSupExp  
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/24/2002
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSupExp()
*:***************************************************************************
FUNCTION lfSupExp
CLEAR READ
DO CASE
  CASE lcRPSel='P'
    lnRPMinQ = 1  
  CASE lcRPSel='N'
    lnRPMinQ = -1
  CASE lcRPSel='A'
    lnRPMinQ = 0
ENDCASE

*!*************************************************************
*! Name        : lfEvalSegs 
*! Developer   : 
*! Date        : 
*! Purpose     : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs

lnMajSeg = gfItemMask('SM')  && No. of major segments.

*-- Compute Free/Color Items in Style code Structure. [Begin]

DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.      
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
*lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'  && remove "those" as per Banna
lcColorTlt = 'Only This ' + ALLTRIM(lcNonMajTlt)  
*-- Compute Free/Color Items in Style code Structure. [End]

*-- Eval Sort By arrays. [Begin]
DIMENSION laSortDesc[2,1],laSortVal[2,1]

laSortDesc[1,1] = lcStyMajor + ' Group' 
laSortDesc[2,1] = lcStyMajor
  
*-- laSortVal to hold the sorting method "S" for style "G" for group
laSortVal[1,1] = 'G'
laSortVal[2,1] = 'S'

RETURN ''

*!*************************************************************
*! Name      : lfClearRep
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Purpose   : Function to Clear temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.
*---Erase the temp file
USE IN IIF(USED(lcRpTmp),lcRpTmp,0)
ERASE &gcWorkDir.&lcRpTmp+'.DBF'
ERASE &gcWorkDir.&lcRpTmp+'.CDX'