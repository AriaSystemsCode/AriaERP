*:****************************************************************
*: Program file  : ICINVDC.PRG
*: Program desc. : Generate Inventory Declaration file.
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Inventory Control (IC)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 07/15/2003
*: Tracking Job Number : C#200568,1
*:****************************************************************
*: Calls         : G.FUNCTIONS:  gfSetup , gfOpGrid
*:               : FUNCTIONS  :  lfProceed , lfPrt1Part , lfPrt2Part
*:               :            :  lfPrt3Part, lfvFpath   , lfOpenFls
*:               :            :  lfClosFls , lfStySum   , lfsrvSty
*:               :            :  lfFltr_Exp, lfItmPos   , lfGetSkuNo
*:               :            :  lfGetScPos.
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200568,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- laOpenFile :- Array Hold the Opened Files.
*-- lnSequence :- Sequence lines at the output file.
*-- lcPurGrp   :- Variable hold the selected Purchasing Group.
*-- lcRpfName  :- Variable hold the File name.
*-- lcRpFPath  :- Variable hold the File Path.
*-- lcRplctext :- Variable hold the input text.
*-- lcLastPlNo :- Variable hold the last and Meximam No. of the PLD Number.
*-- lcDefWare  :- Variable hold the First Default WeareHouse.
*-- lcFile     :- Variable Hold The File Path and name. 
*-- lcChngtext :- Variable Hold The text that will change at the end of the processing.
*-- lnExtScPos :- Variable to hold the Scale Start Pos.
*-- lnExtScLen  :- Variable to hold the Scale Len.

DIMENSION laOpenFile[4,3]
STORE 0 TO lnFilHandl , lnO_T_S , lnSequence , lnSumSkAlo , lcLastPlNo ,;
           lnExtScPos , lnExtScSep , lnExtScLen

PRIVATE lcPurGrp
DIMENSION laMajSegs[1,1]

STORE '' TO lcRpfName , lcRpFPath , lcRplctext , lcPurGrp , lcDefWare , lcFile , lcChngtext

IF !gfSetup()
  RETURN
ENDIF

*-- The Picture of the style at the option grid.
lcStylePic = gfItemMask("PM")
*-- the majour titel at the option grid.
lcStyMajor = gfItemMask('HM')

DO WHILE EMPTY(lcRpFPath) .OR. EMPTY(lcPurGrp)
  *-- Call the option grid.
  lcExpr     = gfOpGrid('ICINVDC',.T.)
  
  IF TYPE('lcExpr') = "L" .AND. !lcExpr
    EXIT
  ENDIF
  
  DO CASE
    *-- Check on the File Path.
    CASE EMPTY(lcRpFPath)
      = gfModalGen('INM00000B00000','F','ALERT', ' ','You should select a file directory, Can not proceed.')
      *-- Check for selected group.
    CASE EMPTY(lcPurGrp)
      = gfModalGen('INM00000B00000','F','ALERT', ' ','You must select a purchasing group, Can not proceed.')
  ENDCASE
ENDDO

IF TYPE('lcExpr') = "L" .AND. !lcExpr
  RETURN
ENDIF

*-- Open needed files
= lfOpenFls ()

llTermColl = .F.
SELECT STYLE
LOCATE FOR &lcExpr
IF EOF()
  llTermColl = .T.
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
ENDIF


IF !llTermColl 
  *-- Funtion to collect the data into text file.
 = lfProceed ()
ENDIF

*-- Close opened files 
= lfClosFls ()
IF llTermColl 
  RETURN
ENDIF

*-- this loop to close the file, the program should close this file under any condation.
IF !(lnFilHandl < 0)
  DO WHILE !FCLOSE(lnFilHandl)

  ENDDO
ENDIF

*-- Update the Global Variables.
CREATE CURSOR lcTmpCur (cTempMemo M(10))
APPEND BLANK
APPEND MEMO cTempMemo FROM  (lcFile)
lcTempMemo = cTempMemo

*-- this line to update the Style.cdefware and Maximam PLD Number.

lcLastPlNo = RIGHT(ALLTRIM(STR(lcLastPlNo)),3)
*ash changes as per Tony's email on 01/15/2004
lcLastPlNo = IIF(LEN(lcLastPlNo)<3,PADL(lcLastPlNo,3,'0'),lcLastPlNo )
lcSysDate  = STRTRAN(DTOC(gdSysdate),'/','')
*ash changed to YYMMDD as per Tony email on 09/30/03
*lcSysDate  = LEFT(lcSysDate,4)+RIGHT(lcSysDate,2)
lcSysDate  = RIGHT(lcSysDate,2)+SUBSTR(lcSysDate,3,2)+LEFT(lcSysDate,2)

lcTextRepl = 'ARF ' + lcDefWare + lcLastPlNo + '   '+ lcSysDate + ' ' + lcSysDate
lcTempMemo = STRTRAN(lcTempMemo,lcChngtext,lcTextRepl)

REPLACE cTempMemo WITH lcTempMemo

COPY MEMO cTempMemo To (lcFile)

*- Message Text   :- Process complete successfully.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
lcMessage = 'Process completed successfully, file '+lcRpfName +'.MA3 is saved on directory '+lcRpFPath+' .'
= gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)

RETURN
*-- END of Code.
*:*************************************************************
*: Name      : lfProceed
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Funtion to collect the data into text file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfProceed ()
*:*************************************************************
*:
FUNCTION lfProceed

lcFileSeq = gfsequence('CSEQUENMA3','','','')
*- Abdou Change 'S-' To be '$I'
*lcRpfName = 'S-'+RIGHT(lcPurGrp,3)+'_'+RIGHT(lcFileSeq,2)
PRIVATE lcCrntGrpD
lcCrntGrpD = LEFT(ALLTRIM(gfCodDes(lcPurGrp,'CPURCODE')),3)
lcRpfName = '$I'+lcCrntGrpD+'_'+RIGHT(lcFileSeq,2)
*- Abdou

lcFile = lcRpFPath+ALLTRIM(lcRpfName)+'.MA3'
lnFilHandl = FCREAT(lcFile)

IF lnFilHandl < 0
  *- Message Text   :- File can not be create. can not proceed.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  = gfModalGen('INM00000B00000','F','ALERT',' ','File can not be create. can not proceed.')
  RETURN
ENDIF
 
*-- Function to get the Scale Pos. 
= lfGetScPos()

*-- Function to Get the 1 part.
= lfPrt1Part ()

*-- Function to Get the 2 part.
= lfPrt2Part ()

*-- Function to Get the 3 part.
= lfPrt3Part ()

*-- End of lfProceed
*:*************************************************************
*: Name      : lfPrt1Part
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Funtion to get the hedaer part.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfPrt1Part ()
*:*************************************************************
*:
FUNCTION lfPrt1Part
DECLARE laVRltFld[2,2]
STORE '' TO lcAvilbil2 , lcSysTime , lcSysDate , lcAna

*-- R e c o r d   1 --*
laVRltFld[1,1] = 'CAVILABIL2'
laVRltFld[1,2] = 'lcAvilbil2'

laVRltFld[2,1] = 'CANA'
laVRltFld[2,2] = 'lcAna'
STORE '' TO lcAna , lcAvilbil2

=gfRltFld(lcPurGrp,@laVRltFld,'CPURCODE')

lcSysTime = gfGetTime()
lcSysTime = LEFT(STRTRAN(lcSysTime,':',''),6)

lcSysDate = STRTRAN(DTOC(gdSysdate),'/','')
*ash changed to YYMMDD as per Tony email on 09/30/03
*lcSysDate  = LEFT(lcSysDate,4)+RIGHT(lcSysDate,2)
lcSysDate  = RIGHT(lcSysDate,2)+SUBSTR(lcSysDate,3,2)+LEFT(lcSysDate,2)
lcGrpDesc = LEFT(ALLTRIM(gfCodDes(lcPurGrp,'CPURCODE')),4)

*lcRplctext = 'X' + '5012068035505 ' + 'AVLHDR        ' + 'N' + lcGrpDesc + ;
             lcFileSeq + '  ' + lcSysDate + lcSysTime + SPACE(45)
*ash changed as per email from tony on 11/07/03
*lcRplctext = 'Z' + '5012068035505 ' + 'AVLHDR        ' + 'N' + lcGrpDesc + ;
             lcFileSeq + '   ' + lcSysDate + lcSysTime + SPACE(45)
LCRPLCTEXT = 'X'+'5012068035505 '+'AVLHDR        '+'N'+LCGRPDESC+padr(lcana,5)+substr(LCFILESEQ,5,6)+'  '+LCSYSDATE+LCSYSTIME+SPACE(45)
=FPUTS(lnFilHandl,lcRplctext)

*-- R e c o r d   2 --*
lcRplctext = 'TYP 2000' + SPACE(03) + 'AVLHDR'
=FPUTS(lnFilHandl,lcRplctext)

*-- R e c o r d   3 --*
lcRplctext = 'SDT ' + lcGrpDesc + PADR(lcAna,8)

=FPUTS(lnFilHandl,lcRplctext)

*-- R e c o r d   4 --*
lcRplctext = 'CDT 4151' 
=FPUTS(lnFilHandl,lcRplctext)

*-- R e c o r d   5 --*
lcRplctext = 'FIL 1 1 ' + lcSysDate
=FPUTS(lnFilHandl,lcRplctext)


*-- R e c o r d   6 --*
lcRplctext = 'ARF ' + 'XX' +'XXX   '+ lcSysDate + ' ' + lcSysDate
lcChngtext = lcRplctext
=FPUTS(lnFilHandl,lcRplctext)

*-- End OF lfPrt1Part
*:*************************************************************
*: Name      : lfPrt2Part
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Funtion to get the middle part.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfPrt2Part ()
*:*************************************************************
*:
FUNCTION lfPrt2Part
PRIVATE lcSku_No , lCurStkVal

STORE 0 TO lnSumSkAlo , lnSequence
lnSequence = 0

SELECT STYLE
SCAN FOR &lcExpr
  WAIT WINDOW 'Style # :' + Style NOWAIT
  
  *-- Variable hold the last and Meximam No. of the PLD Number.
  lcLastPlNo = MAX(MAX(lcLastPlNo,VAL(STYLE.CLASTPLDNO)),0)

  *-- Variable hold the First Default WeareHouse.
  IF EMPTY(lcDefWare)
    lcDefWare   = ALLTRIM(LEFT(Style.cDefware,2))
  ENDIF

  IF SEEK('S'+Scale,'SCALE')
    FOR lnStart = 1 TO Scale.Cnt
      lcCurSize = STR(lnStart,1)
      *ash comment this line as per tony's email on 11/07/03
      *lnSequence = lnSequence + 1
      *-- type+style+account+pack_id
      lcSku_No   = lfGetSkuNo (lnStart)
      lnStkAlo   = Style.Stk&lcCurSize - Style.Alo&lcCurSize
      *ash Don't include the -ve stock size as per tony's email on 09/30/03
      IF lnStkAlo < 0
        LOOP
      ENDIF
      lnSumSkAlo = lnSumSkAlo + lnStkAlo
      *ash Add the if statement as per Tony's email on 11/07/03
      IF LCSKU_NO>' ' 
        LNSEQUENCE = LNSEQUENCE+1
        lcRplctext = 'AVD '+STR(lnSequence,4)+' 01'+ PADR(lcSku_No,8) + ' 1 ' + STR(lnStkAlo,10) + SPACE(10)+'0     0'
        =FPUTS(lnFilHandl,lcRplctext)
      ENDIF
    ENDFOR  
  ENDIF

ENDSCAN
WAIT CLEAR

*-- End OF lfPrt2Part
*:*************************************************************
*: Name      : lfPrt3Part
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Funtion to get the bottom part.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : = lfPrt3Part ()
*:*************************************************************
*:
FUNCTION lfPrt3Part

*-- R e c o r d   1 --*
lcRplctext = 'ATR ' + STR(lnSequence,8)+' ' + STR(lnSumSkAlo,10) + SPACE(10) + '0' + SPACE(10) + '0'
=FPUTS(lnFilHandl,lcRplctext)

*-- R e c o r d   2 --*
lcRplctext = 'AFT          1'
=FPUTS(lnFilHandl,lcRplctext)

*-- End OF lfPrt3Part
*:*************************************************************
*: Name      : lfvFpath
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Funtion to get the fil path.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvFpath()
*:*************************************************************
*:
FUNCTION lfvFpath
PRIVATE lcoldPath

lcoldPath = FULLPATH('')

lcRpFPath = GETDIR()

SET DEFA TO &lcoldPath
*-- End of lfvFpath
*:*************************************************************
*: Name      : lfOpenFls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Function to open needed files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfOpenFls ()
*:*************************************************************
*:
FUNCTION lfOpenFls

*--- Array laOpenFile column 1 :- Hold the name of the file.
*--- Array laOpenFile column 2 :- Hold the name of the index file.
*--- Array laOpenFile column 3 :- Hold true in case open the file .

laOpenFile[1,1] = 'STYLE'
laOpenFile[1,2] = 'STYLE'

laOpenFile[2,1] = 'SCALE'
laOpenFile[2,2] = 'SCALE'

laOpenFile[3,1] = 'SPCK_HDR'
laOpenFile[3,2] = 'SKU_STYLE'

laOpenFile[4,1] = 'SPCK_LIN'
laOpenFile[4,2] = 'SPCKLNST'

FOR I = 1 To ALEN(laOpenFile,1)
  IF !USED(laOpenFile[I,1])
    laOpenFile[I,3] =gfOpenFile(gcDataDir+laOpenFile[I,1],laOpenFile[I,2], "SH")
  ENDIF
ENDFOR

*-- End of lfOpenFls.
*:*************************************************************
*: Name      : lfClosFls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Function to close opened files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfClosFls ()
*:*************************************************************
*:
FUNCTION lfClosFls

FOR I = 1 To ALEN(laOpenFile,1)
  IF USED(laOpenFile[I,1]) .AND.  laOpenFile[I,3]
    = gfCloseFile(laOpenFile[I,1])
  ENDIF
ENDFOR

*-- End of lfClosFls
*:**************************************************************************
*: Name      : lfStySum
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : sum a specific field for the current style in style file
*:**************************************************************************
*: Calls     : 
*:**************************************************************************
*: Called from : Option Grid,style browse calculated fields.
*:**************************************************************************
*: Passed Parameters  : lcSty,lccomp,lnAddToVar
*:**************************************************************************
*: Returns            : Calculated field value.
*:**************************************************************************
*: Example   : =lfStySum()
*:**************************************************************************
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
*:**************************************************************************
*: Name      : lfsrvSty
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Rise change style flag, in range browse screen.
*:**************************************************************************
*: Calls     : 
*:**************************************************************************
*: Called from : Option Grid
*:**************************************************************************
*: Passed Parameters  : lcParm
*:**************************************************************************
*: Returns            : None
*:**************************************************************************
*: Example   : =lfsrvSty()
*:**************************************************************************
*: Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*:**************************************************************************
* 
FUNCTION lfSRVSty
PARAMETERS lcParm


DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    
    IF !EMPTY(lcRpPurcod)
      SET FILTER TO &lcRpPurcod
    ENDIF
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    IF !EMPTY(lcRpPurcod)
      SET FILTER TO
    ENDIF
    SET ORDER TO TAG STYLE
ENDCASE

*-- End of lfsrvSty.
*:*************************************************************
*: Name      : lfFltr_Exp
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Create filter expration.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Report code
*:*************************************************************
*: Passed Parameters  : ...
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfFltr_Exp()
*:*************************************************************
*
FUNCTION lfFltr_Exp
PRIVATE lnCurrPos
lnCurrPos  = lfItmPos('STYLE.CPURCODE')
lcPurGrp   = laOGFxFlt[lnCurrPos,6]

IF EMPTY(lcPurGrp)
  lcRpPurcod = ''
ELSE
  lcRpPurcod = "STYLE.CPURCODE $ lcPurGrp"
ENDIF

*-- End OF lfFltr_Exp
*:*************************************************************
*: Name      : lfItmPos
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Evaluate fixed filter position within array.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Report code
*:*************************************************************
*: Passed Parameters  : Option Grid field.
*:*************************************************************
*: Returns            : Position
*:*************************************************************
*: Example   : = lfItmPos()
*:*************************************************************
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
*:*************************************************************
*: Name      : lfGetSkuNo
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : get the Sku no for current Style/Size
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Program code
*:*************************************************************
*: Passed Parameters  : lnSizePar ==> Style size number.
*:*************************************************************
*: Returns            : SKU#
*:*************************************************************
*: Example   : = lfGetSkuNo()
*:*************************************************************
*
FUNCTION lfGetSkuNo
PARAMETER lnSizePar
PRIVATE lnPrvAlis , lcRtnSkuNo , lnDime1 , lcStyle

lcRtnSkuNo = ''
lnPrvAlis  = SELECT(0)

*-- Check if the style has SKU# or Not.
IF SEEK('S'+Style.style,'SPCK_LIN')
  lcStyle    = IIF(lnExtScPos = 0,Style.style,LEFT(Style.style,LEN(Style.style)-lnExtScLen))
  *-- Check for this account and get the first record for only first account.
  IF SEEK('S'+Spck_lin.Account+lcStyle,'SPCK_HDR')
    SELECT Spck_lin
    lnDime1    = LEN(ALLTRIM(Spck_Hdr.pack_id))
    lcCurSkuNo = LEFT(ALLTRIM(SPCK_HDR.SKU),lnDime1)
    
    SCAN REST WHILE type+style+account+pack_id = 'S'+Style.Style+Spck_hdr.Account;
      FOR LEFT(Pack_Id,lnDime1) = lcCurSkuNo
      *-- if the current record is the curreent size assign it and then get out.
      IF EVALUATE('Spck_Lin.Qty'+ALLTRIM(STR(lnSizePar))) = 1
         lcRtnSkuNo = SUBSTR(ALLTRIM(Pack_id),lnDime1+1)
         EXIT
      ENDIF
    ENDSCAN
  ENDIF
ENDIF

SELECT(lnPrvAlis)
RETUR lcRtnSkuNo
*-- End OF lfGetSkuNo
*:*************************************************************
*: Name      : lfGetScPos
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 07/15/2003
*: Purpose   : Function to get rhe Scale Position.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Program code
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfGetScPos()
*:*************************************************************
*
FUNCTION lfGetScPos

= gfItemMask(@laMajSegs)
FOR lnI = 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'S'
    lnExtScPos = IIF(lnExtScPos=0,laMajSegs[lnI,4],lnExtScPos)
    lnExtScSep = IIF(lnExtScSep=0,LEN(ALLTRIM(laMajSegs[lnI,6])),lnExtScLen)    
    lnExtScLen = IIF(lnExtScLen=0,LEN(laMajSegs[lnI,3]),lnExtScLen) + lnExtScSep
  ENDIF
ENDFOR

*-- End OF lfGetScPos
*:*************************************************************