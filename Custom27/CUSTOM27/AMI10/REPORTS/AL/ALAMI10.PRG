*:**************************************************************************
*: Program file  : ALAMI10
*: Program desc. : Custom Pick Ticket Label
*: System        : Aria Advantage Series.
*: Module        : Sales Order Allocation (AL)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 02/29/2000
*: Reference     : C101754
*:**************************************************************************
*: Calls : 
*:    Functions  : gfModalGen, lfwRepWhen, lfsAccount, lfsPikTkt,
*:                 lfvStyle, lfwOldVal, lfEvalSegs, lfClearRep
*:    Procedure  : lpCreatFil
*:**************************************************************************
*: Example : DO ALAMI10
*:**************************************************************************
*B603546,1 SSE 04/04/2000 1) Printing OrdLine Qtys inspite of PikQtys
*B603546,1                2) Fix Bug of Variable Date not found
*B603556,1 SSE 04/09/2000 Print Sorted by Style 
*B803608,1 SSE 08/31/2000 Fix Bug of Enabling user to Filter out only the styles
*B803608,1                mentioned per pick ticket.
*:**************************************************************************

PRIVATE lcSetCent
lcSetCent = SET('CENTURY')
SET CENTURY ON

*-- If filter is changed collect data again [Begin]
IF llOGFltCh
  *B603556,1 SSE 04/09/2000 Check for Temp Cursor if used [Begin]
  *-- If Temp file is used and has records inside
  IF USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0
    DO lpCreatFil
  ENDIF
  *B603556,1 SSE 04/09/2000 [End]
  
  *-- Creating the text file which will be used for printing 
  PRIVATE lcLineType
  lcLineType = 'H'
  lcOutput = gcWorkDir+lcTextFile+".TXT"
  lnHandle = FCREATE(lcOutput)
  IF lnHandle < 0  
     =gfModalGen("TRM000000B00000","DIALOG",'','','Cannot open output file.')
      SET CENTURY &lcSetCent     && Restore old Century settings (On/Off)
     RETURN
  ENDIF
  *-- Make all relations in order to collect data [Begin]
  SELECT PikTkt
  SET RELATION TO 'O' + Order INTO OrdLine ADDITIVE  
  *-- Make all relations in order to collect data [End]

  PRIVATE lcColor , lcStyle , lcStore , lcSizes
  *-- Scan Loop on Pick Ticket header with Full index expression [Begin]
  lcLineExpr = STRTRAN(lcRpExp,"PIKTKT.","ORDLINE.")

  *B603546,1 SSE 04/04/2000 Fix Bug variable Date not found [Begin]
  *-- Changing field name from DATE to PIKDATE to meet appropriate field in OrdLine

  *B803608,1 Modify the Expression to search in lcLineExpr instead of lcRpExp
  *lcLineExpr = STRTRAN(lcRpExp,".DATE",".PIKDATE")
  lcLineExpr = STRTRAN(lcLineExpr,".DATE",".PIKDATE")
  lcLineExpr = STRTRAN(lcLineExpr,"ORDLINE.STATUS","PIKTKT.STATUS")  
  *B603546,1 SSE 04/04/2000 [End]

  *B803608,1 Checking if Account , Style , Color found in lcRpExp [Begin]
  *-- we have to remove it from lcRpExp
  IF "ORDLINE.ACCOUNT" $ lcRpExp OR "SUBSTR(ORDLINE.STYLE,1,lnMajorLen)" $ lcRpExp OR ;
    "SUBSTR(ORDLINE.STYLE,lnClrPo,lnColorLen)" $ lcRpExp

    *-- Copy all laOGFxFlt to another array to save the old value.
    DIMENSION laTempExpr[1]
    =ACOPY(laOGFxFlt,laTempExpr)

    *-- Define new Fixed filter array to hold one expression only.
    DIMENSION laOGFxFlt[1,7]

    PRIVATE lcOrdLExp       && Variable to hold the new expression
    
    *-- [If Account is found in lcRpExp]
    IF "ORDLINE.ACCOUNT" $ lcRpExp
      STORE "" TO lcOrdLExp , laOGFxFlt
      PRIVATE lcBrowPos    

      *-- Copy all Browse Filter array which is used in case of INLIST 
      *-- to another temp array to save the old value.
      DIMENSION laBrTmpFlt[1]
      =ACOPY(laBrFldFlt,laBrTmpFlt)

      *-- resize Browse filter array to hold one browse expression only.
      DIMENSION laBrFldFlt[1,5]
      laBrFldFlt = ""

      *-- Copy the Account expression to laOGFxFlt
      =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnAcctPos,1),7)
  
      *-- Convert numeric position to string.
      lcBrowPos = ALLTRIM(STR(lnAcctPos))

      FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
        IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
          =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
          EXIT
        ENDIF
      ENDFOR
      laBrFldFlt[1,5] = 'laOGFxFlt:1' 
  
      *-- Generate expression for Account.
      lcOrdLExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  
      *- Replace the Inlist in Poshdr.po withe .T.
      lcRpExp = STRTRAN(lcRpExp,lcOrdLExp,".T.")
      
      *-- Restore original laBrFldFlt
      DIMENSION laBrFldFlt[1]
      =ACOPY(laBrTmpFlt,laBrFldFlt)      
    ENDIF
    *-- [EndIf Account is found in lcRpExp]
    
    *-- [If Ordline.Style is found in lcRpExp]
    IF "SUBSTR(ORDLINE.STYLE,1,lnMajorLen)" $ lcRpExp
      STORE "" TO lcOrdLExp , laOGFxFlt

      *-- Copy the Style expression to laOGFxFlt
      =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnStylePos,1),7)
  
      *-- Generate expression for Style.
      lcOrdLExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  
      *- Replace the Inlist in Poshdr.po withe .T.
      lcRpExp = STRTRAN(lcRpExp,lcOrdLExp,".T.")
    ENDIF
    *-- [EndIf Ordline.Style is found in lcRpExp]

    *-- [If Ordline.Color is found in lcRpExp]
    IF "SUBSTR(ORDLINE.STYLE,lnClrPo,lnColorLen)" $ lcRpExp
      STORE "" TO lcOrdLExp , laOGFxFlt

      *-- Copy the Color expression to laOGFxFlt
      =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnColorPos,1),7)
  
      *-- Generate expression for Color.
      lcOrdLExp = gfGenFlt('laOGFxFlt',.T.,.T.)
  
      *- Replace the Inlist in Poshdr.po withe .T.
      lcRpExp = STRTRAN(lcRpExp,lcOrdLExp,".T.")
    ENDIF
    *-- [EndIf Ordline.Color is found in lcRpExp]

    *-- Restore original laOGFxFlt
    DIMENSION laOGFxFlt[1]
    =ACOPY(laTempExpr,laOGFxFlt)    && restore Original array
  ENDIF
  *B803608,1 Checking if Account , Style , Color found in lcRpExp [End]

  SCAN FOR &lcRpExp

    SELECT OrdLine
    *-- Scan around Pick Ticket line [Begin]
    SCAN REST WHILE cOrdType + Order + STR(LineNo,6) = 'O' + PikTkt.Order FOR &lcLineExpr
      *-- If PikTkt found in OrdLine file
      IF PikTkt = PikTkt.PikTkt
        WAIT WINDOW 'Collecting data for Pick Ticket # ' + PikTkt NOWAIT
        lcColor = PADR('Color: ' + SUBSTR(Style,lnClrPo,lnColorLen),25)
        lcStyle = PADR('Style#: ' + SUBSTR(Style,1,lnMajorLen),25)
        lcStore = PADR('Store#: ' + Store,20)
        lcSizes = ''
       
        IF SEEK('S'+OrdLine.Scale,'SCALE')
          lnCount = 5
          FOR lnI = 1 TO 8
            lcI = ALLTRIM(STR(lnI))

			*B603546,1 SSE 04/04/2000 Change OrdLine QTYs to PIKs [Begin]
            *IF QTY&lcI > 0
            *  lcSizes = lcSizes + IIF(EMPTY(SUBSTR(ALLTRIM(Scale.Sz&lcI),1,lnCount)),;
            *                      '',SUBSTR(ALLTRIM(Scale.Sz&lcI),1,lnCount) + ':' + ;
            *                      ALLTRIM(STR(QTY&lcI)) + ' ')
            *ENDIF                                  

            IF PIK&lcI > 0
              lcSizes = lcSizes + IIF(EMPTY(SUBSTR(ALLTRIM(Scale.Sz&lcI),1,lnCount)),;
                                  '',SUBSTR(ALLTRIM(Scale.Sz&lcI),1,lnCount) + ':' + ;
                                  ALLTRIM(STR(PIK&lcI)) + ' ')
            ENDIF                                  
			*B603546,1 SSE 04/04/2000 [End]

            *-- checking for sizes if bigger than 32 Char then we have to 
            *-- reduce with one char
            IF LEN(lcSizes) > 32 AND lnCount > 1
              lnI = 0
              lnCount = lnCount - 1
              lcSizes = ''
            ENDIF
          ENDFOR
        ENDIF      
      
        IF LEN(lcSizes) > 32
          *-- Message < Style-Color 'Style name' 
          *--           exceeds 32 Characters in sizes
          *--           so it will be ignored >
          *-- Button          < OK >
          =gfModalGen("TRM000000B00000","DIALOG",'','',;
          'Style-Color ' + Style + ' exceeds 32 Characters in sizes' +;
          ' so it will be ignored')  
          LOOP
        ENDIF
      
        *-- Print the Printer settings once at the beginning
        IF lcLineType = 'H'
          lcLineType = 'L'
          =FPUTS(lnHandle,[{ F,1,A,R,E,100,300,"1BMIEE"|])
          =FPUTS(lnHandle,"T,1,25,V,31,61,0,2,2,2,B,L,0,0|")
          =FPUTS(lnHandle,"T,2,25,V,51,61,0,2,2,2,B,L,0,0|")
          =FPUTS(lnHandle,"T,3,20,V,70,63,0,2,3,2,B,L,0,0|")
          =FPUTS(lnHandle,"T,4,32,V,13,7,0,2,2,2,B,L,0,0|")
          =FPUTS(lnHandle," }")
          =FPUTS(lnHandle,"")
        ENDIF

        *B603556,1 SSE 04/09/2000 Commented out [Begin]
        *B603556,1 we have to insert data to Cursor and then TEXT file 
        *B603556,1 so as to make index on Style 
        *=FPUTS(lnHandle,"{ B,1,N,1|")
        *=FPUTS(lnHandle,[ 1,"&lcColor"|])
        *=FPUTS(lnHandle,[ 2,"&lcStyle"|])
        *=FPUTS(lnHandle,[ 3,"&lcStore"|])
        *=FPUTS(lnHandle,[ 4,"&lcSizes"| }])
        *B603556,1 SSE [End]
        
        *B603556,1 SSE 04/09/2000 Add collected data to temp Cursor [Begin]
        INSERT INTO (lcWorkFile) (cColor, cStyle, cStore, cSizes) VALUES ;
        ([ 1,"&lcColor"|], [ 2,"&lcStyle"|], [ 3,"&lcStore"|], [ 4,"&lcSizes"| }])

        *B603556,1 SSE 04/09/2000 [End]
      ENDIF
      *-- EndIf of PikTkt found in OrdLine file
    ENDSCAN
    *-- Scan around Pick Ticket line [End]

  ENDSCAN
  *-- Scan Loop on Pick Ticket header with Full index expression [End]
  WAIT CLEAR
  *-- Break relations 
  SELECT PikTkt
  SET RELATION OFF INTO OrdLine
  
  *B603556,1 SSE 04/09/2000 Send sorted data to the TEXT file [Begin]
  SELECT (lcWorkFile)
  SCAN
    WAIT WINDOW 'Printing ' + cStyle NOWAIT
    =FPUTS(lnHandle,"{ B,1,N,1|")
    =FPUTS(lnHandle,cColor)
    =FPUTS(lnHandle,cStyle)
    =FPUTS(lnHandle,cStore)
    =FPUTS(lnHandle,cSizes)  
  ENDSCAN
  WAIT CLEAR
  *B603556,1 SSE 04/09/2000 [End]
  
ENDIF
*-- Endif of filter is changed collect data again [End]

=FCLOSE(lnHandle)
*-- If lcLineType = 'H' then there is no lines printed
IF lcLineType = 'H'
  *-- There are no records to display.
  = gfModalGen('TRM00052B00000','DIALOG')
ELSE  && Else print report  
  ! TYPE &lcOutput > LPT2
ENDIF
*-- Checking if Text File is Empty [End]

SET CENTURY &lcSetCent     && Restore old Century settings (On/Off)
*-- End of report 

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/29/2000
*! Purpose   : OG when function
*!**************************************************************************
*! Called from : OG
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
*lnPikTkPos = lfItmPos('PIKTKT.PIKTKT')     && Pick Ticket Position
*lnPkTkDtPo = lfItmPos('PIKTKT.DATE')       && Pick Ticket Date Position

*B803608,1 Get Account , Style , Color Filter position [Begin]
lnAcctPos  = lfItmPos('ORDLINE.ACCOUNT')
lnStylePos = lfItmPos('SUBSTR(ORDLINE.STYLE,1,lnMajorLen)')
lnColorPos = lfItmPos('SUBSTR(ORDLINE.STYLE,lnClrPo,lnColorLen)')
*B803608,1 Get Account , Style , Color Filter position [End]

*B603556,1 SSE 04/09/2000 Create the Temp Cursor for the first time [Begin]
DO lpCreatFil
*B603556,1 SSE 04/09/2000 [End]

SHOW GET pbOGPrevie DISABLE
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 08/31/2000
*! Purpose   : To get the position of the fixed filter in OG
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*B803608,1 function is added to get Fixed Filt pos for Account , Style , Color.
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lfsAccount
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/29/2000
*! Purpose   : Directs Customer File to Go Top in the InRange
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsAccount()
*!**************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!**************************************************************************
*
FUNCTION lfsAccount
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SELECT Customer
  GO TOP
ENDIF    
*-- End of lfsAccount.

*!**************************************************************************
*! Name      : lfsPikTkt 
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/29/2000
*! Purpose   : Directs PikTkt File to Go Top in the InRange
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsPikTkt()
*!**************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!**************************************************************************
*
FUNCTION lfsPikTkt
PARAMETERS lcParm

SELECT PikTkt

DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO TAG PikTkt
    GO TOP
  CASE lcParm = 'R'  && Reset code
    SET ORDER TO TAG OrdPik
ENDCASE

*-- End of lfsPikTkt.

*!**************************************************************************
*! Name      : lfvStyle
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/29/2000
*! Purpose   : browse the style file to select a style
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfvStyle()
*!**************************************************************************
*
FUNCTION lfvStyle

PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 
*-- Varible to hold  the name of the memory variable used to create the current GET field
lcObjName = SYS(18)
*-- Varible to hold  the value of the current GET field
lcObjVal = EVALUATE(SYS(18)) 

*--IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))
  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.
  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF 
SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)
*-- End of lfvStyle.

*!**************************************************************************
*! Name      : lfwOldVal
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/29/2000
*! Purpose   : To store the old value of the style
*!**************************************************************************
*! Example     : = lfwOldVal()
*!**************************************************************************
*
FUNCTION lfwOldVal
lcOldVal = EVALUATE(SYS(18))
*-- End of lfwOldVal

*!**************************************************************************
*! Name      : lfEvalSegs
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/29/2000
*! Purpose   : To get the style non major segement properties
*!**************************************************************************
*! Called from : lfwRepWhen()
*!**************************************************************************
*! Example     : = lfEvalSegs()
*!**************************************************************************
*
FUNCTION lfEvalSegs
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc 
    lcFree_Clr = IIF(EMPTY(lcFree_Clr),laMajSeg[lnI,1],lcFree_Clr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFree_Clr = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTtl = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

*!**************************************************************************
*! Name      : lfPrevRun
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Function that we call in place of the Functions called when
*!             Pressing <PREVIEW> or <RUN>
*!**************************************************************************
*! Called from : [Option Grid] < Close > button.
*!**************************************************************************
*! Example     : = lfPrevRun()
*!**************************************************************************
*
FUNCTION lfPrevRun
llOGEscPrsd = .F.
lnOGHD=IIF(EMPTY(laOGHdFlt[1,1]),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(laOGFxFlt[1,1]),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(laOGVrFlt[1,1]),0,ALEN(laOGVrFlt,1))
lcOGSelect=SELECT()
=lfInitValue('laOGVrFlt','T')
=lfInitValue('laOGFXFlt','T')
  
lcOGHDFlt=gfGenFlt('laOGHDFlt' , llOGFilter , .T.)
lcOGFxFlt=gfGenFlt('laOGFxFlt' , llOGFilter , .T.)
lcOGVrFlt=gfGenFlt('laOGVrFlt' , llOGFilter , .T.)
  
lnOGHD=IIF(EMPTY(lcOGHDFlt),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(lcOGFxFlt),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(lcOGVrFlt),0,ALEN(laOGVrFlt,1))  
=lfInitValue('laOGVrFlt','C')
=lfInitValue('laOGFXFlt','C')

lcRpExp = lcOGHDFlt+IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,' AND (','')+;
          lcOGFxFlt+IIF(lnOGVr>0,IIF(lnOgFx>0,' AND ',''),'')+lcOGVrFlt+;
          IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,')','')

lcRpExp=IIF(EMPTY(lcRpExp),'.T.',lcRpExp)
lcogExc=SET('EXACT')        
SET EXACT ON
llOGFltCh = IIF(llOGFltCh,llOGFltCh,lcogfltr<>lcrpexp)        

SET EXACT &lcOGExc
IF llOGFltCh
  lcOGFltr=lcRpExp
ENDIF
SET CONSOLE OFF

IF !EMPTY(lcOGFormV)
  IF !(lcOGLastForm==&lcOGFormV)
      
    lcOGFormArr='laRpObj'+SUBSTR(lcOGPlatForm,1,3)
      
    lcOGLastForm=&lcOGFormV
    llOGStyleCh=.T.
  ENDIF  

  IF llOGRefForm .OR. lcRepAvlM = 'B' .OR. llOGStyleCh
    =gfCrtFrm(EVAL(lcOGFormV),lcOGFormArr,llOGRefForm)
  ENDIF

ENDIF  

DO (gcRepHome+gcAct_Appl+'REPORT.APP') WITH lcOGPrgName, .T.

SET CONSOLE ON

IF !VARREAD() == "PBRUN")
  gcDevice=lcOGDevice
  gcOutFile=lcOGOutFile
  glHeader = llOGHeader
ENDIF  

llOGFltCh = .F.

RETURN .F.
*-- End of lfPrevRun.

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 04/09/2000
*! Purpose   : Create work Cursor.
*!**************************************************************************
*! Called from : Report code.
*!**************************************************************************
*! Example   : DO lpCreatFil
*!**************************************************************************
*B603556,1 SSE 04/09/2000

PROCEDURE lpCreatFil

CREATE CURSOR (lcWorkFile) ;
(cStyle C(31), cColor C(31), cStore C(26), cSizes C(40))

SELECT (lcWorkFile)
ZAP

INDEX ON cStyle TAG (lcWorkFile)


*-- End of lpCreatFil.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/29/2000
*! Purpose   : Clear the Text file before closing the report
*!**************************************************************************
*! Called from : OG
*!**************************************************************************
*! Example   : = lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep
llOGFltCh = .F.
IF lnHandle > 0
  =FCLOSE(lnHandle)
ENDIF  
*-- Erase the Text File used in Printing.
ERASE (gcWorkDir+lcTextFile+".TXT")

*B603556,1 SSE 04/09/2000 Close Temp Cursor [Begin]
*-- Close Temp. cursor opended file, if it is used.
IF USED(lcWorkFile)
  USE IN (lcWorkFile)
ENDIF
*B603556,1 SSE 04/09/2000 [End]

*-- End of lfClearRep.
