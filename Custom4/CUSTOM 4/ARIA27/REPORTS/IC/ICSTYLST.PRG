*:***************************************************************************
*: Program file  : ICSTYLST
*: Program desc. : Style master list
*: For Report    : 
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : AHMED AMER (AHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICSTYLST
*:***************************************************************************
*:B802264,1 SSE 05/13/1999 Change Transaction# in OG from INLIST select 
*:                         one by one to SELECT RANGES 
*:B802296,1 SSE 05/26/1999 1) Print Style Desc. first line after Style Code. 
*:                         2) Move Second Col. (Fabric,Start,Soldout..) to 
*:	        				  right to be clear from first column.	
*:						   3) "Commission" field need to be lined up with 	
*:			    			  the rest of second column.
*:E301271,1 SSH Add Option to select Bins
*:B603533,1 BWA 03/27/2000 Fix the case of having different season per style / color 
*:B603533,1                the style master list still validates the entered season 
*:B603533,1                against the first color of the style
*:B603631,1 AMM 05/18/2000 Centeralaize titles of the report (done in .frx)
*-B803531,1 RAMY 09/21/2000 Fix the follwing bugs
*-                           1- Print the style pictures
*-                           2- Remove the "Print Style Picture" Option in case we print Text Format
*-                           3- Remove some lines that we put them in the SYREPUVR file
*:B804200,1 BWA 07/04/2001 Fix the bug of selection grid criteria is removed when previewing 2nd time.
*:B122257,1 NNA 04/06/2004 Fix bug of that the list only print 12 color for each style as a maximum
*:B039660,1 NNA 01/30/2006 fix bug that in the inventory locking screen when you select a location then pressed
*:B039660,1 NNA            Scope button then an option grid will appear if you pressed Bins option , it'll
*:B039660,1 NNA            give you all bins for all locations (if you're multi locations) not the only selected
*:C132193,1 NNA 05/18/2006 Add filtering by Bin Section to the option grid
*T20061103.0009 TMI 11/15/2006 fix a bug that laData variable not found
*****************************************************************************************************


*-- lcRepNmTtl hold the header of the non major segment for the frx

llDontPrnt = .F.
*-B803531,1 RAMY Remove this line as we add it in the report variable file to get 
*-                      its value only once[start]
*lcRepNmTtl =  gfItemMask("HN")
*-B803531,1 RAMY [end]

lcTime     =  gfGetTime()
lnMajLen   =  LEN(SUBSTR(lcMajPic,4))
*B804200,1 BWA 07/04/2001 Fix the bug of selection grid criteria is removed when previewing 2nd time.[START]
*llClearSty = .T.
*B804200,1 BWA 07/04/2001 [END]
    
*-B803531,1 RAMY Remove these lines we do not nee then anymore cuase we use temp. file now [start]
*SELECT STYLE
*SET RELATION TO 'S'+SUBSTR(Style,1,lnMajLen) INTO OBJLINK ADDITIVE
*
*SELECT OBJLINK
*SET RELATION TO cobject_id INTO OBJECTS  ADDITIVE
*
*SELECT STYLE
*-B803531,1 RAMY [end]

IF lcRpDomImp <>  'B'
  lcRpExp = lcRpExp + 'AND' + IIF(lcRpDomImp = 'I' , ' !Make' , ' Make' )
ENDIF

*B603533,1 Fix the case of having different season per style / color [START]
*DO gfDispRe WITH gcRepHome + 'IC\' + EVALUATE('lcRepNam') , 'FOR ' + lcRpExp
PRIVATE lcAlias

*-B803531,1 RAMY Remove these lines as we added them in the report variable file to get 
*-                      there values only once[start]
*lcStyleS  = gfTempName()
*lnLen  = LEN(gfItemMask("PM"))
*-B803531,1 RAMY [end]

lcStyMaj  = SPACE(0)


lnMOLnFlds = AFIELDS(laFileStru)
lnMOLnFlds = lnMOLnFlds + 1
DIMENSION laFileStru[lnMOLnFlds, 4]
laFileStru[lnMOLnFlds, 1] = "cColor"

*B122257,1 NNA 04/06/2004 (begin) change the field of the color to Memo field to get the all colors
*laFileStru[lnMOLnFlds, 2] = "C"
*laFileStru[lnMOLnFlds, 3] = 80
laFileStru[lnMOLnFlds, 2] = "M"
laFileStru[lnMOLnFlds, 3] = 10
*B122257,1 NNA (End)

laFileStru[lnMOLnFlds, 4] = 0
CREATE TABLE (gcWorkDir+ lcStyleS) FROM ARRAY laFileStru
INDEX ON STYLE TAG (lcStyleS) OF (gcWorkDir+lcStyleS)

SELECT STYLE
SET ORDER TO STYLE TAG STYLE
SCAN FOR &lcRpExp
  WAIT WINDOW "Collecting Data..." NOWAIT
  IF (lcStyMaj # STYLE.cStyMajor)
    m.cColor = SPACE(0)
  ENDIF
  m.cColor = m.cColor + SUBSTR(STYLE.Style,lnClrPo,lnColorLen) + " "
  IF (lcStyMaj # STYLE.cStyMajor)
    SCATTER MEMVAR MEMO
    INSERT INTO (lcStyleS) FROM MEMVAR
  ELSE
    lcAlias = SELECT(0)
    SELECT (lcStyleS)
    *-B803531,1 RAMY [start]
    IF SEEK(ALLTRIM(STYLE.cStyMajor))
    *-B803531,1 RAMY [end]
      REPLACE cColor WITH m.cColor
      SELECT (lcAlias)
    *-B803531,1 RAMY [start]
    ENDIF
    *-B803531,1 RAMY [start]
  ENDIF
  lcStyMaj = STYLE.cStyMajor
ENDSCAN
WAIT CLEAR


*-B803531,1 RAMY [start]
SELECT (lcStyleS)

IF llRpPrnPic = 'Y' AND UPPER(ALLTRIM(lcRepMode)) <> "TEXT"
  SELECT (lcStyleS)
  SET RELATION TO 'S'+SUBSTR(Style,1,lnMajLen) INTO OBJLINK ADDITIVE
  SET SKIP TO OBJLINK
   
  SELECT OBJLINK
  SET RELATION TO
  SET RELATION TO cobject_id INTO OBJECTS  ADDITIVE
ENDIF
SELECT (lcStyleS)

*-B803531,1 RAMY [end]

DO gfDispRe WITH gcRepHome + 'IC\' + EVALUATE('lcRepNam')
  
IF FILE(gcWorkDir + lcStyleS + '.DBF')
  IF USED(lcStyleS)
    USE IN (lcStyleS)
    ERASE (gcWorkDir+lcStyleS+'.DBF')
    ERASE (gcWorkDir+lcStyleS+'.CDX')
  ENDIF
ENDIF    
*B603533,1 [END]

SELECT STYLE
SET RELATION TO

*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style major segement title
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************

FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : AAMER (AHM)
*! Date      : 03/25/1998
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************

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

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

*-- Check the cost access

lnClrSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

*lnFreSgPo = ASUBSCRIPT(laOGVrFlt,;
            ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

*-- Disable/enable Only This colors, Free Segment. [begin]

DO CASE
  CASE lcFreeClr = 'C'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .T.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
    *laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
    *= lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
  CASE lcFreeClr = 'F'
    *laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .T.
    *= lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
ENDCASE
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfAdjPrnArr
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : Prepare the arrays that hold the print by values 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjPrnArr()
*!*************************************************************

FUNCTION lfAdjPrnArr

DIMENSION laRPPrnDsp[2,1],laRPPrnRet[2,1]

laRPPrnDsp[1,1]=lcMajTtl
laRPPrnDsp[2,1]=lcNonMajT

laRPPrnRet[1,1]='S'
laRPPrnRet[2,1]='C'

*!*************************************************************
*! Name      : lfMajPic
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : get major segment picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************

FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")

RETURN lcMajPic

*!*************************************************************
*! Name      : lfvStyle
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************

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

*!*************************************************************
*! Name      : lfvFabric
*! Developer : AAMER (AHM)
*! Date      : 05/27/1998
*! Purpose   : validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************

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


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
*B802264,1
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

    *B804200,1 BWA 07/04/2001 Fix the bug of selection grid criteria is removed when previewing 2nd time.[START]
    *llClearSty = .F.
    *B804200,1 BWA 07/04/2001 [END]
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 05/27/98
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
*B802264,1
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)  
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
*-- end of lfStySum.

*!*************************************************************
*! Name      : lfvBins      *:E301271,1
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 01/07/99
*! Purpose   : Valid Function for Bins
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvBins()
*!*************************************************************
FUNCTION lfvBins

*C132193,1 NNA 05/18/2006 (Begin) filter on Bins according to the selected sections
IF ASCAN(laEvntTrig , PADR('BINSFLTR',10)) <> 0 AND !gfDoTriger('ICINVLK',PADR('BINSFLTR',10))
  RETURN
ENDIF
*C132193,1 NNA (End)

= gfMover(@laRpSource,@laRpTarget,'Style Bins',.T.,'')


*!*************************************************************
*! Name      : lfFillBin     *:E301271,1
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 01/07/99
*! Purpose   : Function to fill bins arrays
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Example   : = lfFillBin()
*!*************************************************************
FUNCTION lfFillBin
llUSdBy =gfOpenFile(gcDataDir+'WHSLOC' ,'WHSLOC','SH')
DIME laRpSource[1,1]
DIME laRpTarget[1,1]
SELECT WHSLOC

*T20061016.0001 TMI [Start] use this part on code only if this program is called from icinvlk.prg only, i.e. there is a defined program name
IF TYPE('lcProgName')='C'
  *T20061016.0001 TMI [End  ] 

  *B039660,1 NNA 01/30/2006 (Start) Get bins only for the selected Location
  *SELECT DISTINCT CLOCATION FROM WHSLOC WHERE !EMPTY(CLOCATION)INTO ARRAY laRpSource
  SELECT DISTINCT CLOCATION FROM WHSLOC WHERE CWARECODE =laData[8] AND !EMPTY(CLOCATION) INTO ARRAY laRpSource
  *B039660,1 NNA (End)

  *T20061016.0001 TMI [Start] 
ENDIF  
*T20061016.0001 TMI [End  ] 
  

*C132193,1 NNA 05/18/2006 (Begin) Fill Section's Array
IF ASCAN(laEvntTrig , PADR('FILLSEC',10)) <> 0
  =gfDoTriger('ICINVLK',PADR('FILLSEC',10))
ENDIF
*C132193,1 NNA (End)

USE IN IIF(llUSdBy,'WHSLOC',0)

*!*************************************************************
*! Name      : lfClearRed *:E301271,1
*! Developer : Ahmed Salah Shalaby -(SSH)
*! Date      : 01/07/99
*! Purpose   : Functio to clear read
*!           : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Example   : = lfClearRed()
*!*************************************************************
FUNCTION lfClearRed
CLEAR READ

*****************************************

FUNCTION lfGetStr

PRIVATE lcTmpAlias
lcTmpAlias  = SELECT(0)

*B122257,1 NNA 04/06/2004 (Begin) Stop the variable of the colors because I get it from the temp file Directly
*lcClrStr    = ''
*B122257,1 NNA (End)

lcSizStr    = ''
lcPrePakStr = ''
SELECT STYLE_A
*B603533,1 Print the color of the recored selected not for all colors. [START]
*IF SEEK(ALLTRIM(STYLE.cStymajor))
*  SCAN WHILE cStymajor = STYLE.cStymajor
*    lcClrStr = lcClrStr + ' ' + strtran(alltrim(STRTRAN(STYLE_A.Style,alltrim(STYLE.cStyMajor))),'-')
*  ENDSCAN
*ENDIF

*B122257,1 NNA 04/06/2004 (Begin) Stop the variable of the colors because I get it from the temp file Directly
*lcClrStr = &lcStyleS..cColor
*B122257,1 NNA (End)

*SELECT SCALE
*IF SEEK('S'+STYLE.Scale)
*  lcSizStr = Sz1 + ' ' + Sz2 + ' ' + Sz3 + ' ' + Sz4 + ' ' + Sz5 + ' ';
*             + Sz6 + ' ' + Sz7 + ' ' + Sz8
*ENDIF
*IF SEEK('P'+STYLE.Scale+STYLE.PrePak)
*  lcPrePakStr = STR(Pp1) + STR(Pp2) + STR(Pp3) + STR(Pp4) + STR(Pp5) + ;
*                STR(Pp6) + STR(Pp7) + STR(Pp8)
*ENDIF

SELECT SCALE
IF SEEK('S'+&lcStyleS..Scale)
  lcSizStr = Sz1 + ' ' + Sz2 + ' ' + Sz3 + ' ' + Sz4 + ' ' + Sz5 + ' ';
             + Sz6 + ' ' + Sz7 + ' ' + Sz8
ENDIF

IF SEEK('P'+&lcStyleS..Scale+&lcStyleS..PrePak)
  lcPrePakStr = STR(Pp1) + STR(Pp2) + STR(Pp3) + STR(Pp4) + STR(Pp5) + ;
                STR(Pp6) + STR(Pp7) + STR(Pp8)
ENDIF
*B603533,1 [END]

SELECT (lcTmpAlias)

RETURN lcPrePakStr

*!************************************************************** Name      : lfModeVld
* Developer : Ramy Mabrouk
* Date      : 11/24/99
* Purpose   : Report Mode Validation 
* Job No.   : B# 803531,1
*!*************************************************************
* Called from : Option Grid
*!*************************************************************
* Passed Parameters  : None
*!*************************************************************
* Returns            : None
*!*************************************************************
* Example   : =lfModeVld()
*!*************************************************************
*-B803531,1 RAMY [start]
FUNCTION lfModeVld
CLEAR READ
*-- end of lfModeVld.

