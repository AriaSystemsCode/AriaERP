 *:************************************************************************
*: Program file  : ICSPICFM.PRG
*: Program desc. : Style Pictures Report
*:         System: ARIA 2.7
*:         Module: Inventory Control
*:         Author: Hend Ghanem (HBG)
*:      Copyright (c) 
*:  Last modified: 10/02/2000
*:
*:  Procs & Fncts: 
*:
*:************************************************************************
*: For custom# C101971
*:***************************************************************************
*:

********* Variabels Decleared in OG **********************
*-- lcRpImgDir   && Variabel hold Image Direcctory   
*-- lcRpOpTlt    && Variabel hold Optional Title           
*-- lcRpSrtBy    && Variabel hold Sort By value
*-- lcRpPrcLvl   && Variabel hold Price Level                    
*-- lnO_T_S      && Variabel hold Open to Sell quantity    
*-- lnColorLen   && Variabel hold Color Length             
*-- lcColorTt    && Variabel hold Color Title              
*-- lnClrPo      && Variabel hold Color Position           
*-- lnMajSeg     && Variabel hold Length of Major Segmant  
*-- laMajSeg     && Array hold Major Segmant  
*-- lcNonMajPi   && Variabel hold None Major Picture
*-- lcNonMajT    && Variabel hold None Major Title
*-- lnNonMajPo   && Variabel hold None Major Position
*-- lcStyTemp    && Style temp file          
*-- lcStyPrePr   && Style temp file to collect Data
*-- lcStyPrnt    && Style temp file to print 
*-- lcStySes     && Style Index  on "Seasson"      
*-- lcStyfab     && Style Index on "Fabric"        
*-- lcColTemp    && Color temp file          



*-- if Filter was changed
IF llOGFltCh
  =lfCreaTemp()    && Create Temp Cursor
  =lfCollect()     && Collect data
ENDIF

*-- Asking if no records (Display message) otherwise print report [Begin.]
IF RECCOUNT(lcStyPrnt) = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

SELECT STYLE
SET ORDER TO CSTYLE

*-- Select the file Which will Print from
SELECT (lcStyPrnt)
GOTO TOP
*-- If user want to sort by Seasson
IF lcRpSrtBy = 'S'
  SET ORDER TO TAG (lcStySes)
ELSE  && user want to sort by Fabric
  SET ORDER TO TAG (lcStyfab)
ENDIF && End if user want to sort by Seasson

*-- Call the Report
DO gfDispRe WITH EVALUATE ('lcRpName')


*!*************************************************************
*! Name      : lfCreaTemp
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/02/2000
*! Purpose   : Create Files need to The Report
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  =lfCreaTemp()
*!*************************************************************
FUNCTION lfCreaTemp


IF !USED(lcStyTemp)
  CREATE TABLE (gcWorkDir+lcStyTemp) (fabric c(7),season c(6),style c(19),Color c(6),desc c(20),scale c(3),picture c(8),Price n(12))
  INDEX ON  Season+Fabric+Style+Color TAG (lcStyTemp) OF (gcWorkDir+lcStyTemp+".CDX")
  USE
  =gfOpenFile(gcWorkDir+lcStyTemp,lcStyTemp,'EX')
ELSE
  SELECT (lcStyTemp)
  ZAP
ENDIF

IF !USED(lcStyPrePr)
  CREATE TABLE (gcWorkDir+lcStyPrePr) (Fabric c(7),Season c(6),Style c(19),PICTURE c(8),Scale c(3),Desc c(20),Price n(12),Color c(6))
  INDEX ON  Season+Fabric+Style TAG (lcStyPrePr) OF (gcWorkDir+lcStyPrePr+".CDX")
  USE
  =gfOpenFile(gcWorkDir+lcStyPrePr,lcStyPrePr,'EX')
ELSE
  SELECT (lcStyPrePr)
  ZAP  
ENDIF

IF !USED(lcStyPrnt)
  CREATE TABLE (gcWorkDir+lcStyPrnt) (Fabric C(7),Season C(6),;
                         Sty1 C(19),STY2 C(19),Sty3 C(19),STY4 C(19),;
                         Sty5 C(19),STY6 C(19),Sty7 C(19),STY8 C(19),;
                         Sty9 C(19),;
                         GEN1 GENERAL,GEN2 GENERAL,GEN3 GENERAL,GEN4 GENERAL,;
                         GEN5 GENERAL,GEN6 GENERAL,GEN7 GENERAL,GEN8 GENERAL,;
                         GEN9 GENERAL,;
                         ColG1 MEMORY,ColG2 MEMORY,ColG3 MEMORY,ColG4 MEMORY,;
                         ColG5 MEMORY,ColG6 MEMORY,ColG7 MEMORY,ColG8 MEMORY,;
                         ColG9 MEMORY,PricL C(1))
  INDEX ON  Season TAG (lcStySes) OF (gcWorkDir+lcStyPrnt+'.CDX')
  INDEX ON  Fabric TAG (lcStyfab) OF (gcWorkDir+lcStyPrnt+'.CDX')
  USE
  =gfOpenFile(gcWorkDir+lcStyPrnt,lcStySes,'EX')
ELSE
  SELECT (lcStyPrnt)
  ZAP
ENDIF

IF !USED(lcColTemp)
  CREATE TABLE (gcWorkDir+lcColTemp) (Color c(6),Index n(2))
  INDEX ON Index TAG (lcColTemp) OF (gcWorkDir+lcColTemp+".CDX")
  USE
  =gfOpenFile(gcWorkDir+lcColTemp,lcColTemp,'EX')
ELSE
  SELECT (lcColTemp)
  ZAP
ENDIF      


*- End of lfCreaTemp
*!*************************************************************
*! Name      : lfCollect
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/02/2000
*! Purpose   : Collect Data needed for the Report
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  =lfCollect()
*!*************************************************************
FUNCTION lfCollect
PRIVATE lcColor , lcPrice 

*--lcColor   && Variabel to Hold the Color of the style
*--lcPrice   && Variabel to Hold the Price of the style "A or B or C"

lcColor = ""
lcPrice = 0

SELECT STYLE
SET ORDER TO STYLE
SET RELATION TO fabric INTO Fabric ADDITIVE

SCAN FOR &lcRpExp
  
  lcColor   = RIGHT(STYLE,lnColorLen)
  lcPrice   = IIF(lcRpPrcLvl = 'A' ,Pricea,IIF(lcRpPrcLvl = 'B' ,Priceb,IIF(lcRpPrcLvl = 'C' ,;
                  Pricec,0)))
  INSERT INTO (lcStyTemp) (Fabric,Season,Style,Color,Desc,Scale,Picture,Price);
                   VALUES (STYLE.Fabric,STYLE.Season,STYLE.cstymajor,lcColor,;
                           STYLE.Desc,STYLE.Scale,STYLE.Picture,lcPrice)
ENDSCAN
SET RELATION TO

*-- Prepare data for printing.
=lfPrePare()


*-- End of lfCollect


*:************************************************************************
*: Program file   : lfPrePare   
*: Customer       : Morelle Products, Inc.
*: Program desc.  : Prepare data for printing.
*:      Developer : Hend Ghanem (HBG)
*:************************************************************************
*: Calls          : None.
*:************************************************************************
*: Passed Params  : None
*:************************************************************************
*: Modifications  : -------
*:************************************************************************
*: Comments       : -------
*:************************************************************************
*
FUNCTION lfPrePare

SELECT (lcStyTemp)
SELECT DIST Fabric,Season,Style,PICTURE, Scale,Desc,Price,COUNT(Color);
       FROM (lcStyTemp);
       WHERE .T. INTO DBF (gcWorkDir+lcStyPrePr);
       GROUP BY Fabric,Season,STYLE
INDEX ON  Season+Fabric+Style TAG (lcStyPrePr)


SELECT (lcStyPrePr)
GOTO TOP
DO WHILE !EOF()
  lcFabric = Fabric
  lcSeason = Season
  SELECT(lcStyPrePr)
  =SEEK(lcSeason+lcFabric)
  lnCounter = 0
  SCAN REST WHILE  Season+Fabric+Style = lcSeason+lcFabric
    lcTSty   = Style
    WAIT WINDOW "Colect data for Fabric+Season+Style  :"+lcFabric+lcSeason+lcTSty NOWAIT
    lcPic    = ALLTRIM(Picture)
    IF USED(lcColTemp)
      SELECT (lcColTemp)
      ZAP
      SELECT(lcStyPrePr)
    ENDIF
    SELECT DIST COLOR,0 AS Index FROM (lcStyTemp);
           WHERE &lcStyTemp..Fabric+&lcStyTemp..Season+&lcStyTemp..Style = ;
                 lcFabric+lcSeason+lcTSty INTO DBF (gcWorkDir+lcColTemp) GROUP BY COLOR

    GOTO TOP
    lnColGr = INT(RECCOUNT()/5)
    lnRecCount = RECCOUNT()
    GOTO TOP
    lnCounter = lnCounter + 1
    lnCounter = IIF(lnCounter> 9,1,lnCounter)
    lnTemp    = lnCounter
    FOR ln = 1 TO lnColGr
      REPLACE Index WITH lnTemp NEXT 5
      IF lnCounter >= 9 OR  lnTemp >= 9
        lnTemp = 1
      ELSE
        lnTemp = lnTemp + 1
      ENDIF
      SKIP
    ENDFOR
    REPLACE REST Index WITH lnTemp
    SELECT (lcStyPrnt)
    IF lnCounter > 9 OR lnCounter = 1
      lnCounter = 1
      APPEND BLANK
    ENDIF
    
    REPLACE FABRIC  WITH lcFabric;
            Season  WITH lcSeason;
            PricL   WITH lcRpPrcLvl
    lcCouInd = STR(lnCounter,1)
    FOR I = lnCounter TO lnCounter+lnColGr+IIF(MOD(lnRecCount,5) <> 0,0,-1)
      IF I > 9
        IF lnCounter > 9 OR lnCounter = 1
          lnCounter = 1
          APPEND BLANK
        ENDIF
        REPLACE FABRIC  WITH lcFabric;
                Season  WITH lcSeason;
                STY1    WITH lcTSty;
                PricL   WITH lcRpPrcLvl
        lcColrStr = ''
        =lfAppCol('1')
        REPLACE ColG1 WITH lcColrStr
        DO CASE
          CASE FILE(lcRpImgDir+'\'+lcPic+'.BMP')
            WAIT WINDOW "Get Style  :"+lcTSty +" Picture  : "+lcPic NOWAIT
            APPEND GENERAL GEN1 FROM lcRpImgDir+'\'+lcPic+'.BMP' LINK
          CASE FILE(lcRpImgDir+'\'+lcPic+'.PCX')
            WAIT WINDOW "Get Style  :"+lcTSty +" Picture  : "+lcPic NOWAIT
            APPEND GENERAL GEN1 FROM lcRpImgDir+'\'+lcPic+'.PCX' LINK
          CASE FILE(lcRpImgDir+'\'+lcPic+'.GIF')
            WAIT WINDOW "Get Style  :"+lcTSty +" Picture  : "+lcPic NOWAIT
            APPEND GENERAL GEN1 FROM lcRpImgDir+'\'+lcPic+'.GIF' LINK
          CASE FILE(lcRpImgDir+'\'+lcPic+'.JPG')
            WAIT WINDOW "Get Style  :"+lcTSty +" Picture  : "+lcPic NOWAIT
            APPEND GENERAL GEN1 FROM lcRpImgDir+'\'+lcPic+'.JPG' LINK
          CASE FILE(lcRpImgDir+'\'+lcPic+'.TIF')
            WAIT WINDOW "Get Style  :"+lcTSty +" Picture  : "+lcPic NOWAIT
            APPEND GENERAL GEN1 FROM lcRpImgDir+'\'+lcPic+'.TIF' LINK
        ENDCASE
      ELSE
        lnCounter = I
        lcI = STR(I,1)
        REPLACE STY&lcI  WITH lcTSty
        lcColrStr = ''
        =lfAppCol(lcI)
        REPLACE ColG&lcI WITH lcColrStr
        DO CASE
          CASE FILE(lcRpImgDir+'\'+lcPic+'.BMP')
            WAIT WINDOW "Get Style  :"+lcTSty +" Picture  : "+lcPic NOWAIT
            APPEND GENERAL GEN&lcI FROM lcRpImgDir+'\'+lcPic+'.BMP' LINK
          CASE FILE(lcRpImgDir+'\'+lcPic+'.PCX')
            WAIT WINDOW "Get Style  :"+lcTSty +" Picture  : "+lcPic NOWAIT
            APPEND GENERAL GEN&lcI FROM lcRpImgDir+'\'+lcPic+'.PCX' LINK
          CASE FILE(lcRpImgDir+'\'+lcPic+'.GIF')
            WAIT WINDOW "Get Style  :"+lcTSty +" Picture  : "+lcPic NOWAIT
            APPEND GENERAL GEN&lcI FROM lcRpImgDir+'\'+lcPic+'.GIF' LINK
          CASE FILE(lcRpImgDir+'\'+lcPic+'.JPG')
            WAIT WINDOW "Get Style  :"+lcTSty +" Picture  : "+lcPic NOWAIT
            APPEND GENERAL GEN&lcI FROM lcRpImgDir+'\'+lcPic+'.JPG' LINK
          CASE FILE(lcRpImgDir+'\'+lcPic+'.TIF')
            WAIT WINDOW "Get Style  :"+lcTSty +" Picture  : "+lcPic NOWAIT
            APPEND GENERAL GEN&lcI FROM lcRpImgDir+'\'+lcPic+'.TIF' LINK
        ENDCASE
      ENDIF
    ENDFOR
  ENDSCAN
ENDDO

*:************************************************************************
*: Program file   : lfAppCol   
*: Customer       : Morelle Products, Inc.
*: Program desc.  : Function to colect colors.
*:      Developer : Hend Ghanem (HBG)
*:************************************************************************
*: Calls          : None.
*:************************************************************************
*: Passed Params  : Color Group No.
*:************************************************************************
*: Modifications  : -------
*:************************************************************************
*: Comments       : -------
*:************************************************************************
*
FUNCTION lfAppCol
PARAMETER ln

lnOld = SELECT(0)
SELECT(lcColTemp)
SCAN FOR Index = VAL(ln)
  WAIT WINDOW "Get Style  :"+lcTSty +" Color  : "+Color NOWAIT
  lcColor = gfCodDes(Color , 'COLOR')
  lcColrStr = lcColrStr  + ALLTRIM(Color) +' '+ IIF(!EMPTY(lcColor),LEFT(ALLTRIM(lcColor),10),"") +CHR(13)
ENDSCAN
SELECT(lnOld)


*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Hend Ghanem
*! Date      : 10/02/2000
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
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Hend Ghanem
*! Date      : 10/02/2000
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
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
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


!*************************************************************
*! Name      : lfNonMaj
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/02/2000
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


*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.

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
  IF laMajSeg[lnI,1] = 'C' 

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

  ENDIF   && end If you Find Color Type 

ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) To lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''


*!*************************************************************
*! Name      : lfScale
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/02/2000
*! Purpose   : Function print style sizes scale.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  =lfScale()
*!*************************************************************
*
FUNCTION lfScale

lcToPrint = ''
IF SEEK('S'+Style.Scale,'SCALE')
  FOR lnInd = 1 TO 8
    lcInd = STR(lnInd,1)
    IF !EMPTY(ALLTRIM(SCALE.Sz&lcInd))
      lcToPrint = lcToPrint + IIF(ALLTRIM(SCALE.Sz&lcInd) <> 'NBR',ALLTRIM(SUBSTR(SCALE.Sz&lcInd,1,3))+',','' )
    ENDIF
  ENDFOR
ENDIF
RETURN(lcToPrint)


*!*************************************************************
*! Name      : lfGetDir
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/02/2000
*! Purpose   : Function to Get the Image Directory
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  =lfGetDir()
*!*************************************************************
*
FUNCTION lfGetDir

lcRpImgDir = GETDIR()

*-- End of lfGetDir

*!*************************************************************
*! Name      : lfvFabric 
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/02/2000
*! Purpose   : Function For Fabric in range
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  =lfvFabric()
*!*************************************************************
*
FUNCTION lfvFabric 
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    SELECT FABRIC
    LOCATE
ENDCASE  


*!*************************************************************
*! Name      : lfClearRep
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/02/2000
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep


IF USED(lcStyTemp)
  USE IN (lcStyTemp)
  ERASE (gcWorkDir+lcStyTemp+'.DBF')
  ERASE (gcWorkDir+lcStyTemp+'.CDX')
ENDIF

IF USED(lcStyPrePr)
  USE IN (lcStyPrePr)
  ERASE (gcWorkDir+lcStyPrePr+'.DBF')
  ERASE (gcWorkDir+lcStyPrePr+'.CDX')
ENDIF

IF USED(lcStyPrnt)
  USE IN (lcStyPrnt)
  ERASE (gcWorkDir+lcStyPrnt+'.DBF')
  ERASE (gcWorkDir+lcStyPrnt+'.CDX')
ENDIF

IF USED(lcColTemp)
  USE IN (lcColTemp)
  ERASE (gcWorkDir+lcColTemp+'.DBF')
  ERASE (gcWorkDir+lcColTemp+'.CDX')
ENDIF      


*- End of lfClearRep

