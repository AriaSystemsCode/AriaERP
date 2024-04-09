*!********************************************************************
*: Program file  : ICPDLCAT.PRG  (REFERENCE, C102269)
*: Program desc. : PRODUCTION DESCRIPTIVE LIST (FOR CAT)
*: For screen    : None.
*:         System: ARIA APPAREL SERIES 2.7
*:         Module: REPORT
*:      Developer: ABD ELRAIM KOURA . KAM
*:      Date     : 04/08/2001 
*!********************************************************************
*: Calls : FUNCTIONS  : 
*:                    : lfwRepWhen()
*:                    : lfSVSty()
*:                    : lfEvalSegs()
*:                    : lfStySum()
*:                    : lfvStyle()
*:                    : lfOldValue()
*:                    : lfItmPos()
*:                    : lfvUpdSes()
*:                    : lfGetDefVl()
*:                    : lfSesLngNm()
*:                    : lfCreatCur()
*:                    : lfColData()
*:                    : lfGetClrD()
*:                    : lfGetColor()
*:                    : lfFrmtDate()
*:                    : lfvGroup()
*:                    : lfvDivi()
*:                    : lfvCrATVar()
*:********************************************************************
*: Passed Parameters  : None
*:********************************************************************
*:Modifications  :
*:B604437,1 TMI 05/07/2001 Put mover for division and groups and other bugs
*:B604468,1 TMI 05/09/2001 Some modifications for discriptive reports
*:B605043,1 ASH 10/18/2001 Print the fabric short desc. instead of fabric desc.
*:B605126,1 BWA 11/15/2001 Fix the bug of printing the style description in 2 pages.
*:B605246,1 BWA 12/19/2001 Fix the bug of printing the style description in 2 pages.
*:B605406,1 BWA 01/22/2002 Fix the bug of printing the report in different size of paper due to the printer setup.
*:E301837,1 BWA 02/14/2002 Add new filter for the style status to the grid
*:B605696,1 ASH 03/17/2002 Apply the status filter to Style/Color not style major only.
*:B606325,1 BWA 07/31/2002 Fix the bug of printing empty space in the report.
*:**************************************************************************

*-- TMI B604437 [Start] If no BMP file is selected then exit
IF EMPTY(lcRpBmpFil)
  =gfDialog('I','A BMP file must be selected.')
  RETURN
ENDIF
*-- Link the bmp file selected to the field Logo
lcBmpFile = gfTempName()
CREATE CURSOR (lcBmpFile) (Logo G)
APPEND BLANK
*-- TMI B604468,1 [Start] Add default path to the BMP
*APPEND GENERAL Logo FROM (lcRpBmpFil) LINK
APPEND GENERAL Logo FROM (gcDef_Path+'DLBMPS\'+lcRpBmpFil) LINK
*-- TMI B604468,1 [END  ]

*-- TMI B604437 [End  ]

*-- TMI B604437 [Start]
STORE SPACE(0) TO lcDivision,lcGroup
=lfvCrATVar()
*-- TMI B604437 [End  ]

*--variable(s) hold color position and color length 
STOR 0 TO lnClrLen,lnClrPos

lcTmpCur = gfTempName()           && varaible hold tmp cursor for report
lcLF = CHR(13) + CHR(10)
*--call function to get color position and color length
=lfGetClrD()

*--call function to create tmp cursor
=lfCreatCur()

*--set needed relation for style file
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE  
SET RELATION TO FABRIC  INTO FABRIC ADDITIVE
*--call function to collect data

=lfColData()


SELECT (lcTmpCur)
GO TOP
IF RECCOUNT()=0
  =gfDialog('I','There are no records to display.')
  RETURN
ENDIF

SET RELATION TO 'S'+STYLE INTO OBJLINK ADDITIVE
SELECT OBJLINK
SET RELATION TO cobject_id INTO OBJECTS
SELECT (lcTmpCur)
GO TOP
DO gfDispRe WITH EVAL('lcRpForm')
RETURN

                    *--End main program iccat10.prg
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AbdElrahim Koura-KAM
*! Date      : 04/05/2001
*! Purpose   : Called when OG is opened
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : ...
*!*************************************************************
*! Example   : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
lnSeaPos = lfItmPos("STYLE.SEASON")
*-- TMI B604437 [Start] CDIVISION and CSTYGROUP are removed from fixed filter list,so comment these two lines:
*lnDivPos = lfItmPos("STYLE.CDIVISION")
*lnGrpPos = lfItmPos("STYLE.CSTYGROUP")
*-- TMI B604437 [End  ]

*-- if it is the default case
IF lnOgSeting = 1

  *-- Assign default values
  *-- TMI B604468,1 [Start] the season fixed filter is deleted
  *laOGFxFlt[lnSeaPos,6] = lcRpSeaVal
  *-- TMI B604468,1 [End  ]
  *-- TMI B604437 [Start] Comment these two lines  
  *laOGFxFlt[lnDivPos,6] = lcRpDivVal
  *laOGFxFlt[lnGrpPos,6] = lcRpGrpVal
  *-- TMI B604437 [End  ]

  *-- Show get the default values.
  *-- TMI B604468,1 [Start] the season fixed filter is deleted
  *= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnSeaPos)) + ',6]')  
  *-- TMI B604468,1 [End  ]
  *-- TMI B604437 [Start] Comment these two lines
  *= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnDivPos)) + ',6]')  
  *= lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnGrpPos)) + ',6]')  
  *-- TMI B604437 [End  ]  
 
  *-- TMI B604468,1 [Start] Update OG with bmp and season value
   lcRpBmpFil = laBmpFils[1,1]
   = lfOGShowGet('lcRpBmpFil')
  lcRpSeaVal = ALLTRIM(lfGetDefVl("SEASON"))
  lcRpSeaDes = gfCodDes(lcRpSeaVal,'SEASON')
  =lfOGShowGet('lcRpSeaVal')
  *-- TMI B604468,1 [End  ]     
  
ENDIF
*-- end of lfwRepWhen.
*!*************************************************************
*! Name      : lfSVSty
*! Developer : Tarek (TMI)
*! Date      : 04/05/2001
*! Purpose   : SRV ( or Set-Reset-Valid function for InRange)
*!*************************************************************
*! Passed Parameters  : lcParm ('S','R' or 'V')
*!*************************************************************
*! Returns            : ...
*!*************************************************************
*! Example   : = lfSVSty()
*!*************************************************************
FUNCTION lfSVSty
PARAMETERS lcParm
PRIVATE lcFltExpr
lcFltExpr = "SET FILTER TO"

SELECT STYLE
IF lcParm = "S"
  lcStyOrder = ORDER("STYLE")
  SET ORDER TO Cstyle  
  IF !EMPTY(lcRpSeaVal)
    *C102269,4 KAM [start]
    *--change filter to substring of cStymajore
    *--stopping 
    *lcFltExpr = [SET FILTER TO cStyMajor = '] + lcRpSeaVal + "'" 
    lcFltExpr = [SET FILTER TO SUBSTR(cStyMajor,1,3) = '] + ALLTRIM(lcRpSeaVal) + "'"             
    *C102269,4 KAM [end]
       
  ENDIF

ELSE  && ELSE Rest case
  SET ORDER TO &lcStyOrder.
  
ENDIF
&lcFltExpr.
LOCATE
*-- end of lfSVSty.
*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : ABD ELRAHIM KOURA (KAM)
*: Date        : 20/02/2001
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
*
FUNCTION lfEvalSegs
STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt
lnMajSeg    = gfItemMask('SM')  && No. of major segments.
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
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF
*-- end of lfEvalSegs.
*!*************************************************************
*! Name      : lfStySum
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 08/03/2001
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec

lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT(lcTStyFile)
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
IF BETWEEN(lnStyRec,1,RECCOUNT('STYLE'))
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
RETURN INT(lnTotcomp)
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfvStyle
*! Developer : AbdElrahim Koura -KAM 
*! Date      : 04/08/2001
*! Purpose   : Validate style code.
*!*************************************************************
*! Calls     : gfStyBrw()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle
lcMStyle = EVALUATE(SYS(18))
IF !EMPTY(lcOldValue) AND lcMStyle = lcOldValue
  RETURN
ENDIF
lcFldStyNam = SYS(18)
IF !EMPTY(lcMStyle) AND !SEEK(lcMStyle,'STYLE')
  *-- Abdou
  *lcMStyle = gfStyBrw('I',"","",.F.)
  lcMStyle = gfStyBrw('M','','',.T.)
  *-- Abdou
  &lcFldStyNam = IIF(!EMPTY(lcMStyle),lcMStyle,lcOldValue)
ENDIF
RETURN
*-end function lfvStyle

*!*************************************************************
*! Name      : lfOldValue
*! Developer : AbdElrahim Koura -KAM 
*! Date      : 04/08/2001
*! Purpose   : Function to store old value of the current filed.Validate style code.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfOldValue()
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))
RETURN
*-end function lfOldValue
*!*************************************************************
*! Name      : lfItmPos
*! Developer : AbdElrahim Koura-KAM 
*! Date      : 04/05/2001
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : lcItmInFlt : Field name
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfItmPos()
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- end of lfItmPos.
*!*************************************************************
*! Name      : lfvUpdSes
*! Developer : AbdElrahim Koura-KAM
*! Date      : 04/05/2001
*! Purpose   : (Valid funciton of Season Popup)
*!             Updates 'm.lcRpSeaVal'
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
FUNCTION lfvUpdSes
*-- TMI B604468,1 [Start] lcRpSeaVal is updated from the popup
*lcRpSeaVal = ALLTRIM(laOgFxFlt[lnSeaPos,6])
*-- TMI B604468,1 [End  ]
*-- TMI B604437 [Start] No need to Long Name 1,2 in OG , Add season description to OG 
*lcRpSeLng1 = lfSesLngNm('CSEALNAME')
*lcRpSeLng2 = lfSesLngNm('CSEA2NAME')
*= lfOGShowGet('lcRpSeLng1')  
*= lfOGShowGet('lcRpSeLng2')  
lcRpSeaDes = gfCodDes(lcRpSeaVal,'SEASON')
= lfOGShowGet('lcRpSeaDes')
*-- TMI B604437 [End  ]
*-- end of lfvUpdSes.
*!*************************************************************
*! Name      : lfGetDefVl
*! Developer : AbdElrahim Koura-KAM
*! Date      : 04/05/2001
*! Purpose   : Get default value of a code from Codes.dbf
*!*************************************************************
*! Passed Parameters  : lcCode : Code value
*!*************************************************************
*! Returns            : Code default value
*!*************************************************************
*! Example   : = lfGetDefVl("SEASON")
*!*************************************************************
FUNCTION lfGetDefVl
PARAMETERS lcCode
PRIVATE lcRetVal , lcCodeOrd
lcCodeOrd = ORDER("CODES")
SET ORDER TO Ccode_no IN CODES
lcRetVal = IIF(SEEK("D"+lcCode,"CODES"),Codes.cCode_No,"")
SET ORDER TO &lcCodeOrd. IN CODES
RETURN lcRetVal
*-- end of lfGetDefVl
*!*************************************************************
*! Name      : lfFillCode
*! Developer : AbdElrahim Koura-KAM
*! Date      : 04/05/2001
*! Purpose   : Dummy function Called from OG to fill popup arrays
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example   : 
*!*************************************************************
PROCEDURE lfFillCode
PRIVATE lcCodeOrd , lnCount

lcCodeOrd = ORDER("CODES")

SET ORDER TO 0 IN CODES
= lfFillOne("laSeason"  ,"laSeaVal","SEASON"   )
*-- TMI B604437 [Start] No need to these two lines
*= lfFillOne("laDivision","laDivVal","CDIVISION")
*= lfFillOne("laGroup"   ,"laGrpVal","CSTYGROUP")
*-- TMI B604437 [End  ]

*-- TMI B604437 [Start] Fill mover arrays
DIMENSION laRpSDivi[1,1],laRpTDivi[1,1],laRpSGrp[1,1],laRpTGrp[1,1]
STORE '' TO laRpSDivi,laRpTDivi,laRpSGrp,laRpTGrp
lnOldAlias = SELECT(0)
SELECT CODES
LOCATE
SELECT CCODE_NO + ' - ' + CDISCREP FROM CODES ;
WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NNCDIVISION ' ;
INTO ARRAY laRpSDivi

LOCATE
SELECT CCODE_NO + ' - ' + CDISCREP FROM CODES ;
WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NNCSTYGROUP ' ;
INTO ARRAY laRpSGrp

SELECT (lnOldAlias)
*-- TMI B604437 [End  ]

SET ORDER TO &lcCodeOrd. IN CODES

*-- TMI B604468,1 [Start] Fill laBmpFils Array
DIME laDummy[1,1]
DIME laBmpFils[1,1]
STORE '' TO laDummy,laBmpFils
=ADIR(laDummy,gcDef_Path+'DLBMPS\*.BMP')
DIME laBmpFils[ALEN(laDummy,1),1]
FOR I = 1 TO ALEN(laDummy,1) 
  laBmpFils[I] = laDummy[I,1]
NEXT
=ASORT(laBmpFils)
*-- TMI B604468,1 [End  ]

*-- end of lfFillCode.
*!*************************************************************
*! Name      : lfFillOne
*! Developer : AbdElrahim Koura-KAM
*! Date      : 04/05/2001
*! Purpose   : Fill one array from codes file
*!*************************************************************
*! Passed Parameters  : lcShowArry  : Array that appears in popup
*!                      lcValArray  : Return values array
*!                      lcCodeVal   : Filter Codes table on this Code 
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfFillOne("laSeason","laSeaVal","SEASON")
*!*************************************************************
PROCEDURE lfFillOne
PARAMETERS lcShowArry , lcValArray , lcCodeVal
DECLARE &lcShowArry.[1,1]

SELECT CCODE_NO+"-"+CDISCREP FROM CODES ;
  WHERE cDefCode + cRltField + cFld_Name = "NN" + lcCodeVal ;
  ORDER BY CCODE_NO ;
  INTO ARRAY &lcShowArry.

IF _TALLY > 0
  DECLARE &lcValArray.[_TALLY,1]
  lnCount = 0
  FOR lnCount = 1 TO _TALLY
    &lcValArray.[lnCount] = PADL(&lcShowArry.[lnCount],6) 
  ENDFOR
ENDIF
*-- end of lfFillOne.


*!*************************************************************
*! Name      : lfSesLngNm
*! Developer : AbdElrahim Koura-KAM
*! Date      : 04/05/2001
*! Purpose   : get season long desc
*!*************************************************************
*! Passed Parameters  : lcShowArry  : Array that appears in popup
*!                      lcValArray  : Return values array
*!                      lcCodeVal   : Filter Codes table on this Code 
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Example   : = lfSesLngNm("laSeason","laSeaVal","SEASON")
*!*************************************************************
FUNCTION lfSesLngNm
PARAMETERS lcLngName
lcToRet=''
lnAlias = SELECT(0)
SELECT CATHCODE
IF SEEK("Y"+PADR("SEASON",10)+PADR(lcRpSeaVal,6)+PADR(lcLngName,10))
  *-- TMI B604437 [Start] To center the Long name on the report
  *lcToRet = CRLTD_VLU
  lcToRet = ALLT(CRLTD_VLU)  
  *-- TMI B604437 [End  ]
ENDIF
SELECT(lnAlias)
RETURN lcToRet
*-- end of flSesLngNm.
*!*************************************************************
*! Name      : lfCreatCur
*! Developer : AbdElrahim Koura -KAM 
*! Date      : 04/08/2001
*! Purpose   : create tmp cursor for report
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfCreatCur()
*!*************************************************************
FUNCTION lfCreatCur
*CREATE CURSOR (lcTmpCur) (FABRIC C(7),STYLE C(19),DESC C(20),SCALE M,COLOR M,mproddesc M,msalesdesc M,llEnd L)

*B605246,1 BWA 12/19/2001 Add a new field to use it in the sordting when adding empty records.[START]
*CREATE CURSOR (lcTmpCur) (FABRIC C(7),STYLE C(19),SCALE M,COLOR M,mproddesc M)
CREATE CURSOR (lcTmpCur) (FABRIC C(7),STYLE C(19),SCALE M,COLOR M,mproddesc M , Sort C(1))
*B605246,1 BWA 12/19/2001.[END]

*-- TMI B604437 [Start] INDEX only on STYLE 
*INDEX ON FABRIC+STYLE TAG (lcTmpCur)

*B605246,1 BWA 12/19/2001 Modify the index with the new field.[START]
*INDEX ON STYLE TAG (lcTmpCur) OF (lcTmpCur)
INDEX ON STYLE + SORT TAG (lcTmpCur) OF (lcTmpCur)
*B605246,1 BWA 12/19/2001.[END]

*-- TMI B604437 [End  ]
*-end function lfCreatCur                          
*!*************************************************************
*! Name      : lfColData
*! Developer : AbdElrahim Koura -KAM 
*! Date      : 04/08/2001
*! Purpose   : collect data for report
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfColData()
*!*************************************************************
FUNCTION lfColData
PRIVATE lcOldSlct,lcOldOrd
PRIVATE M.STYLE,M.SCALE,M.mproddesc,M.msalesdesc
STORE '' TO lcSeason,M.STYLE,M.SCALE,M.mproddesc,M.msalesdesc,M.Desc,M.FABRIC

*B605246,1 BWA 12/19/2001 Fix the bug of printing the style description in 2 pages.[START]
lnOldMemW = SET("MEMOWIDTH")

*B606325,1 BWA 07/31/2002 Change the width of the memo field.[START]
*SET MEMOWIDTH TO 50
SET MEMOWIDTH TO 70
*B606325,1 BWA 07/31/2002.[END]

*B605246,1 BWA 12/19/2001.[END]

*B605126,1 BWA 11/15/2001 Fix the bug of printing the style description in 2 pages.[START]
PRIVATE lcAlias
STORE 0  TO lnRowCont , lnContLin
STORE SPACE(0) TO lcFilEmpty
*B605126,1 BWA 11/15/2001.[END]

*-- TMI B604437 [Start] Open STYLE in a new work aria to collect colors from
lcStyCursr = gfTempName()
USE (gcDataDir+'STYLE') AGAIN IN 0 ALIAS (lcStyCursr) ORDER TAG STYLE
*-- TMI B604437 [End  ]

lcOldSlct =SELECT()

lcOldOrd=ORDER('STYLE')
SET ORDER TO CSTYLE IN STYLE
SELECT STYLE
=SEEK(ALLTRIM(lcRpSeaVal))
SCAN WHILE cStyMajor = ALLTRIM(lcRpSeaVal) FOR &lcRpExp
  WAIT WINDOW 'Selecting record for style : ' + CSTYMAJOR NOWAIT
  STORE '' TO M.SCALE,M.COLOR
  M.STYLE     = cSTYMAJOR
  M.FABRIC    = STYLE.FABRIC
  FOR lcLoop = 1 TO 8
    lcN = "SZ" + ALLTRIM(STR(lcLoop))
    IF !EMPTY(SCALE.&lcN)
      IF lcN="8"
        M.SCALE = ALLTRIM(M.SCALE) + ALLTRIM(SCALE.&lcN) 
      ELSE
         M.SCALE = ALLTRIM(M.SCALE) + ALLTRIM(SCALE.&lcN)+lcLF 
      ENDIF
    ENDIF
  ENDFOR
  *TMI B604437 [Start] Remove last lcLF      
  IF ASC(RIGHT(M.SCALE,1))=10
    M.SCALE = LEFT(M.SCALE,LEN(M.SCALE)-2)
  ENDIF
  *TMI B604437 [End  ]
  *-- TMI B604437 [Start] Remove the extra lcLF
  *M.mProdDesc  = ALLTRIM(STYLE.mSalesDesc)+lcLF+ALLTRIM(FABRIC.DESC)+lcLF+ALLTRIM(STYLE.mProdDesc)+lcLF
  *B605043,1 ASH 10/18/2001 (Begin) Print the fabric short desc. instead of fabric desc.
  *M.mProdDesc  = ALLTRIM(STYLE.mSalesDesc)+;
                 IIF(!EMPTY(FABRIC.DESC    ),lcLF,'')+ALLTRIM(FABRIC.DESC)+;
                 IIF(!EMPTY(STYLE.mProdDesc),lcLF,'')+ALLTRIM(STYLE.mProdDesc)
   M.mProdDesc  = ALLTRIM(STYLE.mSalesDesc)+;
                 IIF(!EMPTY(FABRIC.cFabShDesc),lcLF,'')+ALLTRIM(FABRIC.cFabShDesc)+;
                 IIF(!EMPTY(STYLE.mProdDesc),lcLF,'')+ALLTRIM(STYLE.mProdDesc)                 
   *B605043,1 ASH 10/18/2001 (End)                 
  *-- TMI B604437 [End  ]  
  M.COLOR      = lfGetColor(STYLE.cSTYMAJOR) 
  *B605696,1 ASH 03/17/2002 (Begin) Don't print the style if all it's colors not matching the status filter.
  IF !EMPTY(M.COLOR)
  *B605696,1 ASH 03/17/2002 (End)
    INSERT INTO (lcTmpCur) FROM MEMVAR
  ENDIF
ENDSCAN

*B605406,1 BWA 01/22/2002 Use variable hold the lenght of the rows due to the paper size.[SATRT]
*--Check the size of the printer paper.
IF PRTINFO(2) = 5
  *B606325,1 BWA 07/31/2002 Change the # of the page.[START]
  *lnLegalLen = 69
  lnLegalLen = 67
ELSE

  *lnLegalLen = 48
  lnLegalLen = 48
  *B606325,1 BWA 07/31/2002.[END]

ENDIF
*B605406,1 BWA 01/22/2002.[END]

*B605246,1 BWA 12/19/2001 Modify the number of calculate from 47 to 69.[START]
SELECT (lcTmpCur)

SCAN

  *B605126,1 BWA 11/15/2001 Fix the bug of printing the style description in 2 pages.[START]
  *-- 69 the printed rows in one page.
  STORE 0 TO lnFullLin
  FOR lnEmptyLin = 1 TO MEMLINES(EVAL(lcTmpCur+'.mProdDesc'))
    lnFullLin = lnFullLin + IIF(EMPTY(ALLTRIM(MLINE(EVAL(lcTmpCur+'.mProdDesc'),lnEmptyLin))) , 0 , 1)
  ENDFOR

  lnContLin = lnFullLin + 1

  *B605246,1 BWA 12/19/2001 Fix the bug of printing the style description in 2 pages.[START]
  IF lnContLin < 7
    lnContLin = 7
  ENDIF
  *B605246,1 BWA 12/19/2001.[END]

  lnRowCont = lnRowCont +  lnContLin

  *B605406,1 BWA 01/22/2002 Use variable hold the lenght of the rows due to the paper size.[SATRT]
  *IF lnRowCont >= 69 AND lnContLin <= 7
  *  IF 69 - (lnRowCont - lnContLin) <= 7       && The 7 for the space of the picture in the FRX.

  IF lnRowCont >= lnLegalLen AND lnContLin <= 7
    IF lnLegalLen - (lnRowCont - lnContLin) <= 7       && The 7 for the space of the picture in the FRX.
  *B605406,1 BWA 01/22/2002.[END]

      lnRowCont = lnContLin
    ENDIF 
  ENDIF

  *B605406,1 BWA 01/22/2002 Use variable hold the lenght of the rows due to the paper size.[SATRT]
  *IF lnRowCont <= 69
  IF lnRowCont <= lnLegalLen
  *B605406,1 BWA 01/22/2002.[END]

    *lnContLin = MEMLINES(EVAL(lcTmpCur+'.mProdDesc')) + 1
    lnContLin = lnFullLin + 1

    *B605246,1 BWA 12/19/2001 Fix the bug of printing the style description in 2 pages.[START]
    IF lnContLin < 7
      lnContLin = 7
    ENDIF
    *B605246,1 BWA 12/19/2001.[END]

  ELSE
    STORE SPACE(0) TO lcFilEmpty
    *B605406,1 BWA 01/22/2002 Use variable hold the lenght of the rows due to the paper size.[SATRT]
    *lnToPrt = 69 - (lnRowCont - lnContLin)
    lnToPrt = lnLegalLen - (lnRowCont - lnContLin)
    *B605406,1 BWA 01/22/2002.[END]

    *B606325,1 BWA 07/31/2002 Add a small dummy record to print the scale and color in the last page.[START]
    *IF lnToPrt > 7
    *  FOR lnEmpty = 1 To lnToPrt
    *    IF lnEmpty = lnToPrt

    IF lnToPrt > 7 OR lnToPrt = 7
      FOR lnEmpty = 1 To IIF( lnToPrt > 7 , lnToPrt , 3)
        IF lnEmpty = IIF( lnToPrt > 7 , lnToPrt , 3)
    *B606325,1 BWA 07/31/2002.[END]

          lcFilEmpty = lcFilEmpty + REPLICATE(' ',10)
        ELSE
          lcFilEmpty = lcFilEmpty + REPLICATE(' ',10) + CHR(13) + CHR(10)
        ENDIF
      ENDFOR
      lnRowCont = lnContLin
      lcAlias = SELECT(0)
      SELECT (lcTmpCur)
      REPLACE &lcTmpCur..Sort WITH "B"

      SELECT(lcTmpCur)
      lcStyle = &lcTmpCur..STYLE
      APPEND BLANK
      REPLACE &lcTmpCur..STYLE     WITH lcStyle    ,;
              &lcTmpCur..FABRIC    WITH "BASSEM"   ,;
              &lcTmpCur..mproddesc WITH lcFilEmpty ,;
              &lcTmpCur..Sort      WITH "A"

      *B606325,1 BWA 07/31/2002 Must skip to stand in the right postion.[START]
      IF !EOF()
        SKIP 1
      ENDIF
      *B606325,1 BWA 07/31/2002.[END]

      SELECT(lcAlias)
    ELSE
      lnRowCont = lnContLin
    ENDIF
  ENDIF
  *B605126,1 BWA 11/15/2001.[END]
ENDSCAN
*B605246,1 BWA 12/19/2001.[END]

*B605246,1 BWA 12/19/2001 Fix the bug of printing the style description in 2 pages.[START]
SET MEMOWIDTH TO lnOldMemW
*B605246,1 BWA 12/19/2001.[END]

*-- TMI B604437 [Start] Close (lcStyCursr)
IF USED(lcStyCursr)
  USE IN (lcStyCursr)
ENDIF
*-- TMI B604437 [End  ]
SET ORDER TO &lcOldOrd IN STYLE
SELECT(lcOldSlct) 
RETURN
*--end function  lfColData
*!*************************************************************
*! Name      : lfGetClrD
*! Developer : Abd Elrahim Koura-KAM
*! Date      : 04/03/2001
*! Purpose   : To get color position also color length
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetClrD
*!*************************************************************
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*--end function lfGetClrD

*!*************************************************************
*! Name      : lfGetColor
*! Developer : AbdElrahim Koura -KAM 
*! Date      : 04/08/2001
*! Purpose   : to get all color(s) for each style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetColor()
*!*************************************************************
FUNCTION  lfGetColor
PARAMETERS lcSTYLE
PRIVATE lcOldArea,lcOldOrd,lcRetVal,lnRecNo

DIMENSION laColoR[1]
STORE ' ' TO laColoR,lcRetVal
*-- TMI B604437 [Start] Select the alias (lcStyCursr) instead of cheange the order CSTYLE in STYLE
*lnRecNo  =RECNO()
*lcOldArea=SELECT()
*lcOldOrd=ORDER('STYLE')  
*SET ORDER TO STYLE IN STYLE
SELECT (lcStyCursr)
SEEK SUBSTR(M.STYLE,1,lnClrPos-2)
*-- TMI B604437 [End  ]

*SELECT SUBSTR(STYLE,lnClrPos,lnClrLen);
FROM STYLE;
WHERE cSTYMAJOR=lcSTYLE;
INTO ARRAY laColoR
*-- TMI B604437 [Start] Optimize the scan expression
*B605696,1 ASH 03/17/2002 (Begin) Add the status filter to the scan expression.
*SCAN REST FOR cSTYMAJOR=lcSTYLE
SCAN REST FOR STYLE=SUBSTR(M.STYLE,1,lnClrPos-2) AND IIF(!EMPTY(lcRpPrSt),Status $ lcRpPrSt,.T.)
*B605696,1 ASH 03/17/2002 (End)
  *-- TMI B604437 [Start] Trim extra spaces.
  *lcColorDesc = gfCodDes(SUBSTR(STYLE,lnClrPos,lnClrLen),'COLOR')
  lcColorDesc = ALLT(gfCodDes(SUBSTR(STYLE,lnClrPos,lnClrLen),'COLOR'))
  *-- TMI B604437 [End  ]  
  IF !EMPTY(lcRetVal)
    lcRetVal=lcRetVal+lcColorDesc+lcLF
  ELSE  
    lcRetVal=lcColorDesc+ lcLF 
  ENDIF
ENDSCAN
*C102269,4 KAM [start]
*--remove the last carige return from variable
lcRetVal = LEFT(lcRetVal,LEN(lcRetVal)-2)
*C102269,4 KAM [end]
*-- TMI B604437 [Start] These lines are not needed
*SET ORDER TO &lcOldOrd IN STYLE
*SELECT(lcOldArea)
*IF BETWEEN(lnRecNo,1,RECCOUNT())
*  GOTO lnRecNo
*ENDIF
*-- TMI B604437 [End  ]

RETURN lcRetVal
*--end function lfGetColor

*:**************************************************************************
*:* Name        : lfFrmtDate                          Entry B604437
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/30/2001
*:* Purpose     : To format date as "April 30,2001"
*:***************************************************************************
*:* Called from : frx
*:***************************************************************************
*:* Passed Parameters : date to format
*:***************************************************************************
*:* Return      : formated date
*:***************************************************************************
*:* Example     :  = lfFrmtDate(date())
*:***************************************************************************
FUNCTION lfFrmtDate
PARAMETERS ldThisDate
*-- TMI B604468,1 [Start] IF NO ldThisDate PASSED RETUTN SPACE(0)
*RETURN CMONTH(ldThisDate)+' '+PADL(LTRIM(STR(DAY(ldThisDate),2)),2,'0')+','+STR(YEAR(ldThisDate),4)
RETURN IIF(EMPTY(ldThisDate),'',CMONTH(ldThisDate)+' '+PADL(LTRIM(STR(DAY(ldThisDate),2)),2,'0')+','+STR(YEAR(ldThisDate),4))
*-- TMI B604468,1 [End  ]
*-- end of lfFrmtDate.

*:**************************************************************************
*:* Name        : lfvGroup
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/03/2001
*:* Purpose     : Valid function for Group
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  =lfvGroup()
*:***************************************************************************
FUNCTION lfvGroup
=gfMover(@laRpSGrp,@laRpTGrp,'Select Groups',.T.,'')
*-- end of lfvGroup.

*:**************************************************************************
*:* Name        : lfvDivi
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/03/2001
*:* Purpose     : Valid function for Divi
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvDivi()
*:***************************************************************************
FUNCTION lfvDivi
=gfMover(@laRpSDivi,@laRpTDivi,'Select Divisions',.T.,'')
*-- end of lfvDivi.

*!*************************************************************
*! Name      : lfvCrATVar
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 06/08/2000
*! Purpose   : Function to prepare the variables holds the collection
*!           : Criteria
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvCrATVar

*--The filter
LCRPEXP = IIF(!EMPTY(LCRPEXP),LCRPEXP,'.T.')

*--The Division.
FOR lnInd = 1 TO ALEN(laRpTDivi)
  lcDivision = lcDivision + PADR(laRpTDivi[lnInd],6) + ' | '
ENDFOR
lcDivision = IIF(ALLTRIM(lcDivision) = '|','',lcDivision)

IF !EMPTY(lcDivision)
  LCRPEXP = LCRPEXP + ' AND STYLE.cDivision $ lcDivision'
ENDIF

*--The Groups.
FOR lnInd = 1 TO ALEN(laRpTGrp)
  lcGroup = lcGroup + PADR(laRpTGrp[lnInd],6) + ' | '
ENDFOR
lcGroup = IIF(ALLTRIM(lcGroup) = '|','',lcGroup)

IF !EMPTY(lcGroup)
  LCRPEXP = LCRPEXP + ' AND STYLE.CSTYGROUP $ lcGroup'
ENDIF

*-- TMI B604468,1 [Start] Update lcRpExp with Season 
*--The season
IF !EMPTY(lcRpSeaVal)
  LCRPEXP = LCRPEXP + ' AND STYLE.SEASON = lcRpSeaVal'
ENDIF
*-- TMI B604468,1 [End  ]

*E301837,1 BWA 02/14/2002 Add new filter for the style status to the grid.[START]
*B605696,1 ASH 03/17/2002 (Begin) Remove the status filter from the expression to put it on color level.
*IF !EMPTY(lcRpPrSt)
*  LCRPEXP = LCRPEXP + ' AND STYLE.STATUS $ lcRpPrSt'
*ENDIF
*B605696,1 ASH 03/17/2002 (End)
*E301837,1 BWA 02/14/2002.[END]

*-- end of lfvCrATVar.
*:**************************************************************************
*:* Name        : lfvGetBmp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/06/2001
*:* Purpose     : Selects a bmp file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvGetBmp()
*:***************************************************************************
FUNCTION lfvGetBmp
lcRpBmpFil = GETFILE('BMP')
*-- end of lfvGetBmp.