*:**************************************************************************
*: Program file  : ICMUL200
*: Program desc. : Open To Sell Report for (Mulberry Neckwear)
*: Date          : 08/25/1999
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Sameh (SSE)
*:**************************************************************************
*: Calls :  
*:         Procedures : lpCollData
*:                    : lpFillDate
*:          
*:         Functions  : lfwRepWhen()
*:                    : lfCollTime()
*:                    : lfOldDate()
*:                    : lfvRpDate1()
*:                    : lfvRpDate2()
*:                    : lfvRpDate3()
*:                    : lfLocatExp()
*:                    : lfvLocaton()
*:                    : lfClearRep()
*:                    : lfvRpFormt()
*:                    : lfvRpSelct()
*:                    : lfCreatTxt() 
*:                    : lfvPath()
*:                    : lfItmPos()
*:                    : gfModalGen()
*:                    : gfOpenFile()
*:                    : gfItemMask()
*:                    : gfStyBrw()
*:                    : FaBrow()
*:                    : lfCreatFil() 
*:                    : lfNonMaj()
*:                    : lfSRVSty()
*:                    : lfStySum()
*:                    : lfvStyle()
*:                    : lfvFabric()
*:**************************************************************************
*: Example : DO ICMUL200
*:**************************************************************************
*: This Program is due to CUSTOM PROGRAM FOR Mulberry Neckwear (101620) 
*:**************************************************************************
*: Modification        :
*:**************************************************************************
*:C101620
*:B802781,1 ABD 11/17/1999 Fix bug 'File Cutkhdr not found'
*:**************************************************************************


*:B802781,1 ABD 11/17/1999 Fix bug 'File Cutkhdr not found' [ Begin ]
llCutTkt = ('MF' $ gcCmpModules )
*:B802781,1 ABD [ End ]

*-- validate first the 3 report dates [Begin]
IF ldRpDate1<=gdSysDate .OR. ldRpDate2<=ldRpDate1 .OR. ldRpDate3<=ldRpDate2
	IF ldRpDate1<=gdSysDate
		*Message : M04072 => 'Report Date 1 Should be greater than Today's date'
		*Button  : B00000 => '                     <OK>                           '
		= gfModalGen('TRM04072B00000','DIALOG',"Report date 1|Today's date")
	ELSE
		lnNumber = IIF(ldRpDate2<=ldRpDate1,2,3)
		*Message : M04072 => 'Report Date (no) Should be greater than Report Date (no)'
		*Button  : B00000 => '                     <OK>                           '
		= gfModalGen('TRM04072B00000','DIALOG',"Report Date " + ALLTRIM(STR(lnNumber)) + ;
								"|Report Date " + ALLTRIM(STR(lnNumber-1)))
	ENDIF
	SET DEVICE TO SCREEN
	RETURN			
ENDIF
*-- validate first the 3 report dates [End]

lcStTime = TIME()       && To store the current time
lcRpExp  = STRTRAN(lcRpExp,"STYLE.","")

*-- Logical (GLOBAL) Flag used to determine whether the report is run for 
*-- the first time or not , if first time llOGFltCh = .T. and report will 
*-- collect data , if not first time llOGFltch = .F. and report will skip
*-- collecting data (save time) and goes directly to @ SAY (print loop)
*-- also if user changes the filter in Option Grid or presses Reset Push
*-- Button llOGFltCh will be .T. and collects data again.
IF llOGFltCh	
	*-- If Temp file is used and has records inside
	IF USED(lcMaintemp) AND RECCOUNT(lcMaintemp) > 0
		= lfCreatFil()
	ENDIF
	
	IF !EMPTY(lcRpExp)
		lcRpExp = lcRpExp + [ AND ]
	ENDIF
	lcRpExp = lcRpExp + [SUBSTR(Style,lnClrPo,lnColorLen) <> PADR('- -',lnColorLen)]
  
  *-- make the Style File match the criteria selected
  SELECT Style
  SET FILTER TO &lcRpExp

  *-- make the following 4 relations [Begin]
  SELECT OrdLine
  SET RELATION TO cOrdType + Order INTO OrdHdr ADDITIVE

  *-- OrdLine
  *--       |__
  *--          OrdHdr 
  
  *:B802781,1 ABD Fix bug 'File Cutkhdr not found' [ Begin ]
  IF llCutTkt
    *:B802781,1 ABD [ End ]
    SELECT CutTktL
    SET RELATION TO CutTkt INTO CutTktH ADDITIVE
    *:B802781,1 ABD Fix bug 'File Cutkhdr not found' [ Begin ]
  ENDIF
  *:B802781,1 ABD  [ End ]

  *-- CutTktL
  *--       |__
  *--          CutTktH


  SELECT PosLn
  SET RELATION TO cStyType + Po INTO PosHdr ADDITIVE

  *-- PosLn
  *--     |__
  *--        PosHdr

  SELECT Style
  SET RELATION TO 'S'+ SCALE INTO Scale ADDITIVE
  
  *-- Style
  *--      |__
  *--         Scale
  *-- make the following 4 relations [End]

  *-- store report location expresssion to detect if it is changed 
  lcLstLocEx = lcRpLocExp

  DIMENSION laOrd[8,4],laCut[8,4],laPoQty[8,4],laStock[8,4]

  *-- scan the filtered style file [Begin]
  SCAN
      WAIT WINDOW 'Collecting data for ' + lcMajTtl + ' ' + Style NOWAIT
      DO lpCollData
  ENDSCAN
  *-- scan the filtered style file [End]  
ENDIF
*-- Endif of llOGFltCh

SELECT (lcMaintemp)
GO TOP
*-- If no records in temp file (empty)
IF RECCOUNT(lcMainTemp) = 0
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN
ELSE    && Else Temp file has records (not empty)
  
  *-- Calculate spent time in collecting data.
  lcEdTime = TIME()  && Time in which we finish collect data.
  lnInterval = lfCollTime(lcStTime,lcEdTime)  && Calculate collecting data spent time.
  WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT())) + ' records in ' + ;
               ALLTRIM(STR(lnInterval,6,2)) + ' Seconds...' TIMEOUT 2

  DO gfDispRe WITH EVALUATE('lcRpName')
  = lfCreatTxt()
ENDIF
*-- Endif of no records in temp file

****************************************************************************
*************************** *--End of Report--* ****************************
****************************************************************************

*!**************************************************************************
*! Name      : lfCreatTxt
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : To save data to the text file.
*!**************************************************************************
*! Example   : =lfCreatTxt()
*!**************************************************************************
*
FUNCTION lfCreatTxt
PRIVATE lnHandle , lnChoice , lcCurrDir , lcCurTxtFl
lnHandle   = 0
lnChoice   = 2
lcCurrDir  = ALLTRIM(lcDirName) + IIF(RIGHT(lcDirName,1)<>'\','\','')
lcCurTxtFl = lcCurrDir + ALLTRIM(lcTextFile)+".TXT"

IF FILE(lcCurTxtFl)
  *Message : M42201 => 'Open to sell file ð already exists!'
  *Button  : B42016 => '<Append> <Overwrite> <Cancel>'
  lnChoice = gfModalGen('QRM42201B42016','DIALOG',lcCurTxtFl)
ENDIF

DO CASE
  CASE lnChoice = 1
    lnHandle = FOPEN(lcCurTxtFl,2)
     IF lnHandle < 0
       *Message : M42203 => 'Can not open the file ð'
       *Button  : B00000 => '<Ok> '
       = gfModalGen('TRM42203B00000','DIALOG',lcCurTxtFl) 
       RETURN
     ENDIF
     =FSEEK(lnHandle,0,2)
  CASE lnChoice = 2
     lnHandle = FCREATE(lcCurTxtFl)
     IF lnHandle < 0
       *Message : M42202 => 'Can not Create the file ð'
       *Button  : B00000 => '<Ok> '
       = gfModalGen('TRM42202B00000','DIALOG',lcCurTxtFl)
       RETURN
     ENDIF
  CASE lnChoice = 3
    RETURN
ENDCASE

*-- Copying the temp file to TXT file [Begin]
SELECT (lcMaintemp)
SCAN
  WAIT WINDOW "Copying " + lcMajTtl + " " + ALLTRIM(Style) + " to file " NOWAIT

  lcWriteTxt = ALLTRIM(SUBSTR(Style,1,lnMajLen)) + ',' + ALLTRIM(Desc) ;
               + ',' + ALLTRIM(SUBSTR(Style,lnClrPo,lnColorLen)) + ',' + ;
               ALLTRIM(ColorDes) + ',' + ALLTRIM(Size) + ',' + ;
               IIF(SEEK(Style+SizeNo,'StyleUPC'),StyleUPC.cUPCnum2,;
               IIF(SEEK(SUBSTR(Style,1,lnMajLen+1)+PADR("- -",lnColorLen)+;
               SizeNo,'StyleUPC'),StyleUPC.cUPCnum2,'')) + ',' + ;
               ALLTRIM(STR(nOTS1)) + ',' + ALLTRIM(STR(nOTS2)) + ',' + ;
               ALLTRIM(STR(nOTS3)) + ',' + ALLTRIM(STR(nOTS4))
  =FPUTS(lnHandle,lcWriteTxt)
ENDSCAN
WAIT CLEAR
=FCLOSE(lnHandle)
*-- Copying the temp file to TXT file [End]
*-- End of lfCreatTxt.

*!**************************************************************************
*! Name      : lfvPath
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : To validate the directory
*!**************************************************************************
*! Example   : =lfvPath()
*!**************************************************************************
FUNCTION lfvPath
lcOldVal  = FULLPATH("")

lcDirName = ALLTRIM(lcDirName)
IF lcDirName == lcOldVal 
  RETURN
ENDIF
PRIVATE llError,lcOldErr
*-- Initializing the variable to hold the error. If the path
*-- was correct then set the path to the new path in order
*-- to be able to create the TXT file. Otherwise display a message
*-- informing that there was an error occured.
lcOldErr = ON('ERROR')

llError = .F.
ON ERROR llError = .T.
SET DEFAULT TO (lcDirName)

ON ERROR &lcOldErr

IF llError OR EMPTY(lcDirName)
  lcDirName = GetDir(lcOldVal,'Select source directory')
  IF EMPTY(lcDirName)
    =gfDialog('I','Invalid directory.')
    lcDirName = lcOldVal
  ENDIF
ENDIF
lcDirName = ALLTRIM(lcDirName)
SET DEFAULT TO (lcDirName)
*-- End of lfvPath.


*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : Option Grid When function
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : lfItmPos()
*!**************************************************************************
*! Example     : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen
*-- save the old path which is aria27 directory
lcDirPath  = FULLPATH("")

lcRpName  = 'ICML200D'  && When report starts it is defaulted to Detail first
lnStatPos = lfItmPos('STYLE.STATUS')  && store the Style Status position
laOGFxFlt[lnStatPos,6] = 'A'          && make status target defaulted to "A"

*-- Call the function which create the main Temp file [Begin]
= lfCreatFil()
*-- Call the function which create the main Temp file [End]

*-- If WareHouse Codes are empty
IF EMPTY(laRpLocCd)
  DECLARE laRpLocCd[1,1]

  STORE '' TO lcRpLocExp 
  *-- Collect all WareHouses by SELECT - SQL
  SELECT cWareCode FROM WareHous INTO ARRAY laRpLocCd ORDER BY 1

  *-- If the Mem. File exists get the Locations Data from it  
  IF FILE(gcDataDir+lcMemFile+'.MEM')
    RESTORE FROM gcDataDir+lcMemFile+'.MEM' ADDITIVE
  ELSE
    DECLARE laRpDataSr[ALEN(laRpLocCd,1),1],laRpDataTr[ALEN(laRpLocCd,1),1]
    FOR lnI = 1 TO ALEN(laRpLocCd,1)
      STORE laRpLocCd[lnI,1] TO laRpDataSr[lnI,1],laRpDataTr[lnI,1]
    ENDFOR
  ENDIF
  *-- EndIf of Mem. File exists 
ENDIF
*-- Endif of WareHouse Codes are empty

= lfLocatExp()   && check if Selected Locations in the target array is changed         
*-- end of lfwRepWhen.

*!**************************************************************************
*! Name      : lfCreatFil
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : Create temporary file structure.
*!**************************************************************************
*! Called from : OG When function. OR Main PRG
*!**************************************************************************
*! Example   : = lfCreatFil()
*!**************************************************************************
*
FUNCTION lfCreatFil
*-- If file is used we have to close it first [Begin]
IF USED(lcMaintemp)
  USE IN (lcMaintemp)
ENDIF
*-- If file is used we have to close it first [End]

*-- Create the Main Temp File [Begin]
CREATE TABLE (gcWorkDir+lcMaintemp);
	         (Style C(19),Desc C(20),ColorDes C(40),cStyGroup C(6),cDivision C(6),Size C(5),;
              SizeNo C(1),Pattern C(10),nOTS1 N(7),nOTS2 N(7),nOTS3 N(7),;
              nOTS4 N(7),Total N(8))
INDEX ON cDivision+cStyGroup+Style+Size TAG (lcMaintemp)
*-- Create the Main Temp File [End]
*-- End of lfCreatFil.

*!**************************************************************************
*! Name      : lfLocatExp
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : Evaluate WareHouses Codes expression.
*!**************************************************************************
*! Called from : lfvWareHos,lfwRepWhen
*!**************************************************************************
*! Example   : = lfLocatExp()
*!**************************************************************************
FUNCTION lfLocatExp
PRIVATE laTarget

IF EMPTY(laRpDataTr)
  = ACOPY(laRpDataSr,laTarget)
ELSE
  = ACOPY(laRpDataTr,laTarget)
ENDIF
  
= ASORT(laTarget)
lcRpLocExp = ''

FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpLocExp = IIF(EMPTY(lcRpLocExp),PADR(laTarget[lnI],6),;
                    lcRpLocExp + ','+PADR(laTarget[lnI],6))
ENDFOR
llOGFltCh = llOGFltCh OR !(lcLstLocEx == lcRpLocExp)

IF !(lcLstLocEx == lcRpLocExp)
  *-- we've used Wildcard (?) to save both Report Dates and Locations
  SAVE TO gcDataDir+lcMemFile+'.MEM' ALL LIKE l?RpDat*
ENDIF
*-- End of lfLocatExp.

*!**************************************************************************
*! Name      : lfvLocaton
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : Call mover function then Evaluate WareHouses Codes expression 
*!**************************************************************************
*! Calls     : 
*!             Functions  : gfMover , lfLocatExp 
*!**************************************************************************
*! Called from : OG
*!**************************************************************************
*! Example   : = lfvLocaton()
*!**************************************************************************
FUNCTION lfvLocaton
= gfMover(@laRpDataSr,@laRpDataTr,'Select Location',.T.,'')  && call mover function.
= lfLocatExp()
*-- End of lfvLocaton.

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : when closing the OG
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep
*-- erase the main temporary file at the end of report
IF USED(lcMaintemp)
  USE IN (lcMainTemp)
  ERASE (gcWorkDir+lcMainTemp+'.DBF')
  ERASE (gcWorkDir+lcMainTemp+'.CDX') 
ENDIF

*-- restore the old default path to --> aria27
SET DEFAULT TO &lcDirPath
*-- End of lfClearRep.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : Evaluate fixed filter position within array.
*!**************************************************************************
*! Called from : Report code
*!**************************************************************************
*! Returns   : Position
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
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
*! Name      : lfNonMaj
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : To get the style nonmajor segment structure
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfItemMask()
*!**************************************************************************
*! Example     : = lfNonMaj()
*!**************************************************************************
*
FUNCTION lfNonMaj
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'  
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
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- End of lfNonMaj.

*!**************************************************************************
*! Name      : lfvStyle
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : validate style
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : gfStyBrw()
*!**************************************************************************
*! Example     : = lfvStyle()
*!**************************************************************************
* 
FUNCTION lfvStyle
lcStyle = VARREAD()
lcTag = ORDER('STYLE')
SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF
SET ORDER TO lcTag IN STYLE
*--End of lfvStyle.

*!**************************************************************************
*! Name      : lfvFabric
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : validate fabric
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : FaBrow()
*!**************************************************************************
*! Example     : = lfvFabric()
*!**************************************************************************
* 
FUNCTION lfvFabric
lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC
IF llUseByMe
  USE IN FABRIC
ENDIF  
*-- End of lfvFabric.

*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : Rise change style flag, in range browse screen.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : lcParm
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfsrvSty()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!**************************************************************************
* 
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!**************************************************************************
*! Name      : lfStySum
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : sum a specific field for the current style in style file
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!**************************************************************************
*! Passed Parameters  : lcSty,lccomp,lnAddToVar
*!**************************************************************************
*! Returns            : Calculated field value.
*!**************************************************************************
*! Example   : =lfStySum()
*!**************************************************************************
* 
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  *SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)  
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  

  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- End of lfStySum.

*!**************************************************************************
*! Name      : lfvRpFormt
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : validate Report Format
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example     : = lfvRpFormt()
*!**************************************************************************
* 
FUNCTION lfvRpFormt
lcRpName = IIF(lcRpFormat='D','ICML200D','ICML200S')
*-- End of lfvRpFormt.

*!**************************************************************************
*! Name      : lfvRpSelct
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : validate Select (Positive / Negative / Both)
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example     : = lfvRpSelct()
*!**************************************************************************
* 
FUNCTION lfvRpSelct
lcMinQtPic = IIF(lcRpSelect = 'N','-99999','99999') && to update the sign (+ve or -ve)
CLEAR READ  
*-- End of lfvRpSelct.

*!**************************************************************************
*! Name      : lfOldDate
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : When entering the Report Date 1 , 2 , 3
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example    : = lfOldDate()
*!**************************************************************************
* 
FUNCTION lfOldDate
PARAMETERS lcDateNo
ldOldDate = ldRpDate&lcDateNo
*-- End of lfOldDate.

*!**************************************************************************
*! Name      : lfvRpDate1
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : validate the first report Date
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example     : = lfvRpDate1()
*!**************************************************************************
* 
FUNCTION lfvRpDate1
*IF !EMPTY(ldRpDate1) AND ldRpDate1 > gdSysDate
IF ldRpDate1 > gdSysDate
  IF ldOldDate <> ldRpDate1
    llOGFltCh = .T.
    *-- we've used the Wildcard (?) to save both the Dates and the Locations
  ENDIF
ELSE
	WAIT WINDOW "This date must be greater than today's date !" NOWAIT
ENDIF
SAVE TO gcDataDir+lcMemFile+'.MEM' ALL LIKE l?RpDat*
*-- End of lfvRpDate1.

*!**************************************************************************
*! Name      : lfvRpDate2
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : validate the second report Date
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example     : = lfvRpDate2()
*!**************************************************************************
* 
FUNCTION lfvRpDate2
*IF !EMPTY(ldRpDate2) AND ldRpDate2 > ldRpDate1
IF ldRpDate2 > ldRpDate1
  IF ldOldDate <> ldRpDate2
    llOGFltCh = .T.
    *-- we've used the Wildcard (?) to save both the Dates and the Locations
  ENDIF  
ELSE
  WAIT WINDOW "This date must be greater than the previous date !" NOWAIT
ENDIF
SAVE TO gcDataDir+lcMemFile+'.MEM' ALL LIKE l?RpDat*
*-- End of lfvRpDate2.

*!**************************************************************************
*! Name      : lfvRpDate3
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : validate the third report Date
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example     : = lfvRpDate3()
*!**************************************************************************
* 
FUNCTION lfvRpDate3
*IF !EMPTY(ldRpDate3) AND ldRpDate3 > ldRpDate2
IF ldRpDate3 > ldRpDate2
  IF ldOldDate <> ldRpDate3
    llOGFltCh = .T.
    *-- we've used the Wildcard (?) to save both the Dates and the Locations
  ENDIF  
ELSE
	WAIT WINDOW "This date must be greater than the previous date !" NOWAIT
ENDIF
SAVE TO gcDataDir+lcMemFile+'.MEM' ALL LIKE l?RpDat*
*-- End of lfvRpDate3.

*!**************************************************************************
*! Name      : lfCollTime
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : get the time of collecting data
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Returns     : time of collecting data
*!**************************************************************************
* 
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)
*-- End of lfCollTime.

*!**************************************************************************
*! Name      : lpCollData
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : Collect data into Temporary file
*!**************************************************************************
*! Called from : Main PRG 
*!**************************************************************************
*! Calls       : lpFillDate
*!**************************************************************************
*! Passed Parameters : Style , Desc , Pattern
*!**************************************************************************
*! Example     : DO lpCollData WITH Style,Desc,Pattern
*!**************************************************************************
*
PROCEDURE lpCollData
IF SEEK("S"+Scale,"Scale") AND Scale.Cnt > 0
  lnScalCnt = Scale.Cnt
ELSE
  RETURN
ENDIF  
lcStyle   = Style
lcDesc    = Desc
lcPattern = Pattern

STORE 0 TO laOrd,laCut,laPoQty,laStock

*-- IF this current style is found in OrdLine file 
IF SEEK(lcStyle,"OrdLine")
  SELECT OrdLine
  *-- Collect all Orders for this style from OrdLine file [Begin]
  SCAN REST WHILE Style + DTOS(Complete) + cOrdtype + Order + ;
                Store+STR(LineNo,6) = lcStyle
    IF OrdHdr.Status $ 'OH' AND (EMPTY(laRpDataTr) OR OrdHdr.cWareCode $ lcRpLocExp)
      DO lpFillDate WITH OrdHdr.Start , 'laOrd' , '+' , 'Qty'
    ENDIF
  ENDSCAN
  *-- Collect all Orders for this style from OrdLine file [End]
ENDIF
*-- EndIF of this current style in Ordline file

*:B802781,1 ABD 11/17/1999 Fix bug 'File Cutkhdr not found' [ Begin ]
IF llCutTkt
  *:B802781,1 ABD [ End ]
  *-- IF this current style is found in CutTktL file 
  IF SEEK(lcStyle,"CutTktL")
    SELECT CutTktL
    *-- Collect all Cut ticket for this style from CutTktL file [Begin]
    SCAN REST WHILE Style + CutTkt + Trancd = lcStyle
      IF CutTktH.Status $ 'OHA' AND (EMPTY(laRpDataTr) OR Cuttkth.cWareCode $ lcRpLocExp)
        DO lpFillDate WITH CutTktH.Complete,'laCut',IIF(TranCd='1','+','-'),'Qty'
      ENDIF
    ENDSCAN
    *-- Collect all Cut ticket for this style from CutTktL file [End]
  ENDIF
  *:B802781,1 ABD 11/17/1999 Fix bug 'File Cutkhdr not found' [ Begin ]  
ENDIF
*:B802781,1 ABD [ End ]

  
*-- EndIF of this current style in CutTktL file
  
*-- IF this current style is found in PosLn file 
IF SEEK(lcStyle,"PosLn")
  SELECT PosLn
  *-- Collect all purchase Orders for this style from PosLn file [Begin]  
  SCAN REST WHILE Style + cStyType + Po + STR(LineNo,6) + Trancd = lcStyle
    IF PosHdr.Status $ 'OHA' AND (EMPTY(laRpDataTr) OR PosHdr.cWareCode $ lcRpLocExp)
      DO lpFillDate WITH PosHdr.Available,'laPoQty',IIF(TranCd='1','+','-'),'Qty'
    ENDIF  
  ENDSCAN
  *-- Collect all purchase Orders for this style from PosLn file [End]
ENDIF
*-- EndIF of this current style in PosLn file

*-- IF this current style is found in StyInvJl file 
IF SEEK(lcStyle,"StyInvJl")
  SELECT StyInvJl
  *-- Collect all Stocks for this style from StyInvJl file [Begin]
  SCAN REST WHILE Style + cWareCode + cSession + DTOS(dTrDate) + cTrCode = lcStyle
    IF (EMPTY(laRpDataTr) OR cWareCode $ lcRpLocExp)
      DO lpFillDate WITH DtrDate,'laStock','+','nStk'
    ENDIF
  ENDSCAN
  *-- Collect all Stocks for this style from StyInvJl file [End]
ENDIF
*-- EndIF of this current style in StyInvJl file

*-- Add the style in the temp file [Begin]
PRIVATE lnOTS
lnOTS = 0

*-- declare laOTS every time according to the number of sizes in each style 
DIMENSION laOTS[lnScalCnt,4]

*-- For loop to get the OTS for the number of sizes (lnScalCnt) 
*-- in each style for all Dates 
FOR lnOTS = 1 to lnScalCnt
    laOTS[lnOTS,1] = laStock[lnOTS,1] + laCut[lnOTS,1] +;
                     laPoQty[lnOTS,1]
    laOTS[lnOTS,2] = laStock[lnOTS,2] + laCut[lnOTS,2] +;
                     laPoQty[lnOTS,2]
    laOTS[lnOTS,3] = laStock[lnOTS,3] + laCut[lnOTS,3] +;
                     laPoQty[lnOTS,3]
    laOTS[lnOTS,4] = laStock[lnOTS,4] + laCut[lnOTS,4] +;
                     laPoQty[lnOTS,4]

  *-- If lnTotStk not equal zero for the 4 stocks date
  IF laOTS[lnOTS,1]<>0 OR laOTS[lnOTS,2]<>0 OR ;
     laOTS[lnOTS,3]<>0 OR laOTS[lnOTS,4]<>0

    *-- For Loop to update the OTS for all Dates
    FOR lnCount = 4 TO 1 STEP -1
      *lnTotStk = laOTS[lnOTS,1] + laOTS[lnOTS,2] + laOTS[lnOTS,3] + laOTS[lnOTS,4]      
      lnTotStk = 0
      FOR lnI = 1 TO lnCount
        lnTotStk = lnTotStk + laOTS[lnOTS,lnI]
      ENDFOR
      
      *-- If..Else..Endif to update the OTS for all dates [Begin] 
      IF laOrd[lnOTS,lnCount] <= laOTS[lnOTS,lnCount] OR laOrd[lnOTS,lnCount] > lnTotStk
        laOTS[lnOTS,lnCount] = laOTS[lnOTS,lnCount] - laOrd[lnOTS,lnCount]
      ELSE
        FOR lnCount1 = lnCount TO 1 STEP -1          
          IF laOrd[lnOTS,lnCount] <= laOTS[lnOTS,lnCount1]
            laOTS[lnOTS,lnCount1] = laOTS[lnOTS,lnCount1] - laOrd[lnOTS,lnCount]
            EXIT
          ELSE
            lnReduce = MIN(laOrd[lnOTS,lnCount]-laOTS[lnOTS,lnCount1],laOTS[lnOTS,lnCount1])
            laOTS[lnOTS,lnCount1] = laOTS[lnOTS,lnCount1] - lnReduce
            laOrd[lnOTS,lnCount] = laOrd[lnOTS,lnCount] - lnReduce
          ENDIF          
        ENDFOR
      ENDIF
      *-- If..Else..Endif to update the OTS for all dates [End] 
    ENDFOR
    *-- For Loop to update the OTS for all Dates

    *-- If user select POSITIVE OTS quantity in OG (Min Qty > 0)  
    IF lcRpSelect='P'
      lnI = 0
      FOR lnI = 1 TO 4
        laOTS[lnOTS,lnI] = IIF(laOTS[lnOTS,lnI] >= lnMinQty ,laOTS[lnOTS,lnI],0)
      ENDFOR
    ENDIF
    *-- EndIf user select POSITIVE OTS quantity in OG (Min Qty > 0)  

    *-- If user select POSITIVE OTS quantity in OG (Min Qty < 0)      
    IF lcRpSelect='N'
      lnI = 0
      FOR lnI = 1 TO 4
        laOTS[lnOTS,lnI] = IIF(laOTS[lnOTS,lnI] <= (lnMinQty*-1) ,laOTS[lnOTS,lnI],0)
      ENDFOR 
    ENDIF
    *-- EndIf user select POSITIVE OTS quantity in OG (Min Qty < 0)  
    
    *-- If total Open to sell for all dates not equal zero
    *-- add the record in the Temp file else there is no need to add this record 
    IF laOTS[lnOTS,1]<>0 OR laOTS[lnOTS,2]<>0 OR ;
       laOTS[lnOTS,3]<>0 OR laOTS[lnOTS,4]<>0
      
      lnTotStk = laOTS[lnOTS,1] + laOTS[lnOTS,2] + laOTS[lnOTS,3] + laOTS[lnOTS,4]      
      lcOTS = STR(lnOTS,1)
      m.cDivision = Style.cDivision
      m.cStyGroup = Style.cStyGroup
      m.Style     = lcStyle
      m.Size      = Scale.Sz&lcOTS
      m.Desc      = lcDesc
      m.ColorDes  = gfCodDes(SUBSTR(Style.Style,lnClrPo,lnColorLen),'Color')
      m.SizeNo    = lcOTS
      m.Pattern   = lcPattern
      m.nOTS1     = laOts[lnOTS,1]
      m.nOTS2     = laOts[lnOTS,2]
      m.nOTS3     = laOts[lnOTS,3]
      m.nOTS4     = laOts[lnOTS,4]
      m.Total     = lnTotStk
      INSERT INTO (lcMaintemp) FROM MEMVAR
    
    ENDIF  
    *-- EndIf of total Open to sell for all dates not equal zero
  ENDIF
  *-- EndIf of lnTotStk not equal zero for the 4 stocks date
ENDFOR
*-- EndFor loop to get the OTS for the number of sizes (lnScalCnt) 
*-- Add the style in the temp file [End]
*-- End of lpCollData.

*!***************************************************************************
*! Name      : lpFillDate
*! Developer : Sameh (SSE)
*! Date      : 08/25/1999
*! Purpose   : Collect data which lies between the 4 dates for the 4 relations
*!**************************************************************************
*! Called from : Main PRG 
*!**************************************************************************
*! Passed Parameters : ldDateVal , lcArray , lcSign , lcQtyOrStk
*!**************************************************************************
*! Example     : DO lpFillDate WITH 'OrdHdr.Start' , 'laOrd' , '+' , 'Qty'
*!**************************************************************************
*
PROCEDURE lpFillDate
PARAMETERS ldDateVal , lcArray , lcSign , lcQtyOrStk
lnNumber = 0
DO CASE
  CASE ldDateVal <= ldToday
    lnNumber = 1
  CASE ldDateVal > ldToday AND ldDateVal <= ldRpDate1
    lnNumber = 2          
  CASE ldDateVal > ldRpDate1 AND ldDateVal <= ldRpDate2
    lnNumber = 3        
  CASE ldDateVal > ldRpDate2 AND ldDateVal <= ldRpDate3
    lnNumber = 4        
ENDCASE
IF lnNumber <> 0
  FOR lnX = 1 TO lnScalCnt
    lcNo = STR(lnX,1)         
    &lcArray.[lnX,lnNumber] = &lcArray.[lnX,lnNumber] &lcSign &lcQtyOrStk.&lcNo
  ENDFOR
ENDIF  
*-- End of lpFillDate.
