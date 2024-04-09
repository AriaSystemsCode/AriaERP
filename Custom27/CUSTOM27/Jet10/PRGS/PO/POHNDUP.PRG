*:****************************************************************
*: Program file  : POHNDUP.PRG
*: Program desc. : Download UPC Information.
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Style Purchase Order (PO)
*: Developer     : Abdou Elgendy - (ABD)
*: Date          : 02/17/2004.
*: Tracking Job Number : C#121147,1
*:****************************************************************
*: Calls         : 
*:               : FUNCTIONS  : 
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#121147,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- lcRpfName  :- Variable hold the file name.
*-- lcDirectry :- Variable hold the directory that will hold the path for printed file.
*-- lcStyMajor :- Variable hold the style titel into aria.

*-- Start Declaration Variabls
STORE '' TO lcStyMajor,lcMajorPic,lcColorTlt,lcNonMajTlt,lcNonMajPi,;
            lcFree_Clr , lcDirectry , lcRpFName , lcFilHandl
            
STORE 0 TO lnO_T_S,lnMajSeg,lnNonMajSt,lnSupMajSt,lnFreeLen,;
           lnColorLen,lnDummyPos,lnRngAlias,lnMajorLen

llOpenRep  = .F.
llFound    = .F.
lcDirectry = FULLPATH('')

*-- Evaluate NonMajor Type and variables.
= lfEvalSegs ()
lcSeason   = lcStyMajor + ' season'
lcDivision = lcStyMajor + ' division'
lcStyGroup = lcStyMajor + ' group'
lcStyPGrup = lcStyMajor + ' purchasing group'

*-- End Declaration Variabls
IF !gfSetup()
  RETURN
ENDIF


DO WHILE .T.
  *-- Cal the Option Grid.
  lcExpr = gfOpGrid('POHNDUPC' , .T.)

  DO CASE 
    CASE Type('lcExpr') = 'C' .AND. !EMPTY(lcRpfName) .AND. !EMPTY(lcDirectry)
      *- Message Text   :- Are you sure you want to Download UPC Information.
      *- Message No.    :- 000000.
      *- Buttom Message :- Ok
      *- Buttom Number  :- 00000.
      lcMessage = 'Are you sure you want to Download UPC Information.'
      IF gfModalGen('INM00000B00006','F','ALERT',' ',lcMessage) = 1
        EXIT
      ENDIF
  CASE Type('lcExpr') = 'C' .AND. EMPTY(lcRpfName)
      *- Message Text   :- You must specify a name for the download UPC information file.
      *- Message No.    :- 000000.
      *- Buttom Message :- Ok
      *- Buttom Number  :- 00000.
      lcMessage = 'You must specify a name for the download UPC information file.'
      = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)


  CASE Type('lcExpr') = 'C' .AND. EMPTY(lcDirectry)
      *- Message Text   :- You must specify a directory for the download UPC information file.
      *- Message No.    :- 000000.
      *- Buttom Message :- Ok
      *- Buttom Number  :- 00000.
      lcMessage = 'You must specify a directory for the download UPC information file.'
      = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
        
  CASE Type('lcExpr') = 'L' .AND.  lcExpr = .F.
    EXIT
  ENDCASE  
ENDDO

IF  Type('lcExpr') = 'L' .AND.  lcExpr = .F.
  RETURN
ENDIF

*-- Function to download the selected styles.
= lfDownUpc ()
IF !llFound  
  RETURN
ENDIF
* Close the text file
*DO WHILE !FCLOSE(lcFilHandl)
*ENDDO
=FCLOSE(lcFilHandl)

*- Message Text   :- Process completed successful.
*- Message No.    :- 000000.
*- Buttom Message :- Ok
*- Buttom Number  :- 00000.
lcMessage = 'Process completed successful.'
= gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)


RETURN

*-- End OF Code.
*:*************************************************************
*: Name        : lfEvalSegs
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 02/17/2004.
*: Purpose     : Evaluate NonMajor Type and variables.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfEvalSegs()
*:*************************************************************
*:
FUNCTION lfEvalSegs

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
lcStyMajor  = gfItemMask('HM')
lcMajorPic  = gfItemMask('PM')
lnMajorLen  = LEN(lcMajorPic)


*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]

    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lnSupMajSt = lnNonMajSt
       
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
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style code Structure. [End]

*-- End OF lfEvalSegs
*:*************************************************************
*: Name        : lfSrSty1
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 02/17/2004.
*: Purpose     : Style In Range Filter. 1.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfSrSty1()
*:*************************************************************
*:
FUNCTION lfSrSty1

PARAMETERS lcParm
=lfSRStyle(lcParm,"1")

*-- End Of lfSrSty1.
*:*************************************************************
*: Name        : lfSrSty2
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 02/17/2004.
*: Purpose     : Style In Range Filter. 2
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfSrSty2()
*:*************************************************************
*:
FUNCTION lfSrSty2
PARAMETERS lcParm

=lfSRStyle(lcParm,"2")

*-- end of lfSrSty2.
*:*************************************************************
*: Name        : lfSRStyle
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 02/17/2004.
*: Purpose     : Style In Range Filter.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfSRStyle()
*:*************************************************************
*:
FUNCTION lfSRStyle
PARAMETERS lcParm,lcFlagNo

DO CASE
  CASE lcParm = 'S'  && Set code
    lnRngAlias = SELECT(0)
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
    llClrSty&lcFlagNo = .F.
    SELECT (lnRngAlias)
ENDCASE

*-- End of lfSRStyle.
*:*************************************************************
*: Name        : lfStySum
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 02/17/2004.
*: Purpose     : sum a specific field for the current style in style file
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfStySum()
*:*************************************************************
*:
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) = ALLTRIM(lcSty)
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
*:*************************************************************
*: Name        : lfGetDir
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 02/17/2004.
*: Purpose     : get the directory for the output file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfGetDir()
*:*************************************************************
*:
FUNCTION lfGetDir
PRIVATE lcOld_Dir 

*-- Get the directory that will hold the path for printed file.
lcOld_Dir = FULLPATH('')

lcDirectry = GETDIR()

SET DEFA TO &lcOld_Dir
*-- End OF lfGetDir
*:*************************************************************
*: Name        : lfDownUpc
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 02/17/2004.
*: Purpose     : Function to convert the style UPC top text file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfDownUpc()
*:*************************************************************
*:
FUNCTION lfDownUpc
PRIVATE lnPrv_Alas , lcSendCode
lcSendCode = ''
lnPrv_Alas = SELECT(0)

*-- open styleUPC file.
llopenUPC = gfOpenFile(gcDataDir+'STYLEUPC',gcDataDir+'STYLEUPC','SH')
llopenScl = gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
llSome = .F.
SELECT STYLE
SET RELATION TO 'S'+Scale INTO SCALE ADDITIVE
LOCATE
*-- scan for the selcted style.
SCAN FOR &lcExpr
  WAIT WINDOW Style NOWAIT
  *-- check if the current style exist into the styleupc file.
  IF SEEK(Style,"STYLEUPC")
    llFound=.T.
    SELECT STYLEUPC
    *-- Get all the upc for the current style.
    SCAN REST WHILE Style + Size = Style.Style
      lcSc = ALLTRIM(Size)
      lcColor = SUBSTR(Style ,lnNonMajSt,lnColorLen)
      lcClrDesc  = gfCodDes(lcColor, 'COLOR     ')
      lcSendCode = Style +';'+ Size + ';' +Scale.Sz&lcSc +';'+ Style.Desc+';'+cupcnum1+cupcnum2+cupcnum3+';'+lcClrDesc  
      = lfPrntTxt(lcSendCode)
    ENDSCAN
    SELECT STYLE
  ELSE
    llSome = .T.
  ENDIF  
ENDSCAN
IF llSome
  lcMessage = 'One or more Styles have NO UPC.'
  =gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)

ENDIF
IF !llFound
  lcMessage = 'The selected styles have NO UPC.'
  =gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
ENDIF
wait clea
*-- close the styleupc file in case open from this program
IF llopenUPC
  =gfCloseFile('STYLEUPC')
ENDIF
*-- close the scale file in case open from this program
IF llopenScl
  =gfCloseFile('SCALE')
ENDIF

SELECT(lnPrv_Alas)
*-- End OF lfDownUpc.
*:*************************************************************
*: Name        : lfPrntTxt
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 02/17/2004.
*: Purpose     : Function to open the text file for convertion.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfPrntTxt()
*:*************************************************************
*:
FUNCTION lfPrntTxt
PARAMETER lcSendtext
IF !llOpenRep
  llOpenRep = .T.
  lcRpfName = ALLTRIM(lcRpfName)
  lcTextFnam = '&lcDirectry.'+'STYLE.TXT'
  lcFilHandl = FCREAT(lcTextFnam)
  =FPUTS(lcFilHandl,'STYLE;SIZE#;SIZE;DESC;UPC;COLOR')
ENDIF

=FPUTS(lcFilHandl,lcSendtext)



*-- END OF lfPrntTxt
*:*************************************************************

FUNCTION lfwRepWhen
lnPLoPo = ASCAN(laOGObjType,'lcRpFName')
lnPLoPo = ASUBSCRIPT(laOGObjType,lnPLoPo,1)
laOGObjCnt[lnPLoPo] = .F.
= lfOGShowGet('lcRpFName')
