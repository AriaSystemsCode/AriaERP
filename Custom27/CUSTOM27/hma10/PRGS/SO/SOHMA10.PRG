*!******************************************************************************
*! Program file  : SOHMA10   (C # 102135)
*! Program desc. : A Comma DELIMITED for HMA100 
*! System        : Aria Advantage Series.
*! Module        : Sales Order (SO)
*! Developer     : Ahmed Mohmaed ElAnwar (AME)
*! Date          : 05/16/2001
*! Reference     : C102136
*!******************************************************************************
*! Notes : Also refer to C101044. (Aria26)
*!************************************************************************
*! Modifications :
*! C119695,1 ABD 09/17/2003 Modifications to open to sell text file, we will do the following:
*!               1- Remove the styles that ended with 'A' from the text file.
*!               2- Add its OTS quantity to the original style.
*!******************************************************************************

MEMFILE = gcDataDir+'SOHMA100'
lnColorLen = ''
lnClrPo  = ''
lcPath = ''
lcFile = ''
lcRPDomImp = ''
STORE '' TO laOGFxFlt

*C119695,1 ABD - Define New Variables hold the the filter Expr. [Begin]
STORE '' TO lcOldFilter
*C119695,1 ABD - [end]

DO WHILE .T.
  lcExpr = gfOpGrid('SOHMA10' , .T.)
  IF TYPE('lcExpr')= 'L' .AND. lcExpr = .F.
    RETURN
  ENDIF
  llError = .F.
  lcOldErr = ON('ERROR')
  ON ERROR llError = .T.
  lcOldDef = FULLPATH("")
  SET DEFAUL TO (lcPath)
  ON ERROR &lcOldErr
  SET DEFAULT TO (lcOldDef)
  IF llError .OR.  EMPTY(lcPath)
    IF  gfDialog('!','Invalid Path!','\<Reenter;\!\?\<Cancel')=2
      RETURN
    ELSE
      LOOP
    ENDIF  
  ENDIF
  IF EMPTY(lcFile)
    IF  gfDialog('?','No output file Name was entered!!','\<Proceed;\!\?\<Cancel')=2
      RETURN
    ELSE
      LOOP
    ENDIF  
  ENDIF
  EXIT
ENDDO
SAVE ALL LIKE LCPATH TO &MEMFILE
IF FILE("&MEMFILE..MEM")
  RESTORE ADDITIVE FROM &MEMFILE
ENDIF
lcOutFile = ALLTRIM(lcPath)+'\'+ALLTRIM(lcFile)+'.TXT'
IF FILE(lcOutFile) 
  lnOutFile = 1
  IF  gfDialog('?','Output file '+lcOutFile+' already exists. Overwrite it?','\<Proceed;\!\?\<Cancel')=2
    RETURN
  ENDIF  
ENDIF
lnOutFile = FCREATE(lcOutFile)
IF lnOutFile < 0
  =gfDialog('I','Can not open output file '+lcOutFile+'. Cannot proceed.')
  RETURN
ENDIF

=gfOpenFile(GCDataDir+'Style','Style')

SELECT STYLE

*C119695,1 ABD - Define New Variables hold the ots For Style emnded with 'A', and Scan for all 
*C119695,1 ABD - style that not ended with 'A'. [Begin]
STORE 0 TO lnRetOTS 
*SCAN FOR &lcExpr
SCAN FOR &lcExpr .AND. !(RIGHT(ALLTRIM(cStyMajor),1) = 'A')
*C119695,1 ABD - [End]
  STORE 0 TO lnOpen1,lnOpen2,lnOpen3,lnOpen4,lnOpen5,lnOpen6,lnOpen7,lnOpen8,OTS
  FOR lnSize = 1 to 8
    lcSize = STR(lnSize,1)
    lnOpen&lcSize =  WIP&lcSize + STK&lcSize - ORD&lcSize 
    IF lnOpen&lcSize > 0
      OTS = OTS + lnOpen&lcSize
    ENDIF
  ENDFOR

  *C119695,1 ABD - Call funtion to get the OTS for the same style ended with 'A'. [Begin]
  lnRetOTS = lfGet8AOts(cStyMajor,Style)
  *C119695,1 ABD - [End]
  
  IF OTS > 0
    lcClrCod = gfCodDes(SUBSTR(STYLE,lnClrPo,lnColorLen),'COLOR')
    *C119695,1 ABD - Update the Text File With [Begin]
    *lcRepStr = '"'+ALLTRIM(CSTYMAJOR)+'"'+','+'"'+ALLTRIM(SUBSTR(STYLE,lnClrPo,lnColorLen))+'"'+','+'"'+IIF(!EMPTY(lcClrCod),ALLTRIM(lcClrCod),SPACE(01))+'"'+','+;
    *            '"'+ALLTRIM(DESC)+'"'+','+'"'+ALLTRIM(SCALE)+'"'+','+'"'+IIF(lnOpen1>0,ALLTRIM(STR(lnOpen1)),SPACE(01))+'"'+','+'"'+;
    *            IIF(lnOpen2>0,ALLTRIM(STR(lnOpen2)),SPACE(01))+'"'+','+'"'+;
    *            IIF(lnOpen3>0,ALLTRIM(STR(lnOpen3)),SPACE(01))+'"'+','+'"'+;
    *            IIF(lnOpen4>0,ALLTRIM(STR(lnOpen4)),SPACE(01))+'"'+','+'"'+;
    *            IIF(lnOpen5>0,ALLTRIM(STR(lnOpen5)),SPACE(01))+'"'+','+'"'+;
    *            IIF(lnOpen6>0,ALLTRIM(STR(lnOpen6)),SPACE(01))+'"'+','+'"'+;
    *            IIF(lnOpen7>0,ALLTRIM(STR(lnOpen7)),SPACE(01))+'"'+','+'"'+;
    *            IIF(lnOpen8>0,ALLTRIM(STR(lnOpen8)),SPACE(01))+'"'+','+'"'+;
    *            ALLTRIM(STR(OTS))+'"'
    lcRepStr = '"'+ALLTRIM(CSTYMAJOR)+'"'+','+'"'+ALLTRIM(SUBSTR(STYLE,lnClrPo,lnColorLen))+'"'+','+'"'+IIF(!EMPTY(lcClrCod),ALLTRIM(lcClrCod),SPACE(01))+'"'+','+;
                '"'+ALLTRIM(DESC)+'"'+','+'"'+ALLTRIM(SCALE)+'"'+','+'"'+IIF(lnOpen1>0,ALLTRIM(STR(lnOpen1)),SPACE(01))+'"'+','+'"'+;
                IIF(lnOpen2>0,ALLTRIM(STR(lnOpen2)),SPACE(01))+'"'+','+'"'+;
                IIF(lnOpen3>0,ALLTRIM(STR(lnOpen3)),SPACE(01))+'"'+','+'"'+;
                IIF(lnOpen4>0,ALLTRIM(STR(lnOpen4)),SPACE(01))+'"'+','+'"'+;
                IIF(lnOpen5>0,ALLTRIM(STR(lnOpen5)),SPACE(01))+'"'+','+'"'+;
                IIF(lnOpen6>0,ALLTRIM(STR(lnOpen6)),SPACE(01))+'"'+','+'"'+;
                IIF(lnOpen7>0,ALLTRIM(STR(lnOpen7)),SPACE(01))+'"'+','+'"'+;
                IIF(lnOpen8>0,ALLTRIM(STR(lnOpen8)),SPACE(01))+'"'+','+'"'+;
                IIF(lnRetOTS>0,ALLTRIM(STR(lnRetOTS)),SPACE(01))+'"'+','+'"'+;
                ALLTRIM(STR(OTS))+'"'
    *C119695,1 ABD - [End]
    =FPUTS(lnOutFile,lcRepStr)
  ELSE
    LOOP    
  ENDIF  
ENDSCAN
=FSEEK(lnOutFile,0)
IF FEOF(lnOutFile)
  =gfDialog('I','No records to update the text file')
ELSE
  =gfDialog('I','Output file '+lcOutFile+' has been created.')
ENDIF
=FCLOSE(lnOutFile)

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 05/08/2001
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfOGShowGet
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

lnClrSgPo = ASUBSCRIPT(laOGFxFlt,;
            ASCAN(laOGFxFlt,'SUBSTR(STYLE.STYLE,lnClrPo,lnColorLen)'),1)

*-- Disable/enable Only These colors  [begin]

IF lcFreeClr = 'C'
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnClrSgPo] = .T.
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
ELSE
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGFxFlt,1) + lnClrSgPo] = .F.
  = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
ENDIF

*-- Disable/enable Only These colors [end]
*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
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
*! Name      : lfvFabric
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
*! Purpose   : Validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
*! Name      : lfNonMaj
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : lcNMajDum from Option Grid
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

STORE LEN(lcNonMajPi) TO lnFreeeLen , lnColorLen
lcColorTt = 'Only These ' + ALLTRIM(lcNonMajT) + 's'
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''
*!*************************************************************
*! Name      : lfStySum  (C# 102135)
*! Developer : AHMED MOHAMED ELANWAR (AME)
*! Date      : 04/05/2001
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
*! Name      : lfMajExp
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
*! Purpose   : Reconstruct the major seg. expression
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajExp()
*!*************************************************************

FUNCTION lfMajExp

RETURN lcMajTtl + SPACE(1) + PADL(': ',2+MAX(LEN('Warehouse')-LEN(lcMajTtl),0)) + ALLTRIM(EVAL(lcTrns+".cStyMajor"))

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
*! Purpose   : Validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************

FUNCTION lfvStyle

lcStyMaj = VARREAD()

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyMaj = STYLE.cStyMajor
  ELSE
    &lcStyMaj = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyMaj = ''
ENDIF

SET ORDER TO lcTag IN STYLE

*!*************************************************************
*! Name      : lfMajPic
*! Developer : Ahmed ElAnwar (AME)
*! Date      : 03/22/2001
*! Purpose   : Get major seg. picture
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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
*! Name      : lfsrvSty  (C# 102135)
*! Developer : AHMED MOHAMED ELANWAR (AME)
*! Date      : 04/05/2001
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
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE

    *C119695,1 ABD - Define New Variables hold the the old filter Expr. [Begin]
    lcOldFilter = FILTER()
    SET FILTER TO !(RIGHT(ALLTRIM(cStyMajor),1) = 'A')
    *C119695,1 ABD - [End]
    
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    
    *C119695,1 ABD - Return the old filter expr. [Begin]
    SET FILTER TO &lcOldFilter
    *C119695,1 ABD - [End]
    
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.


*!*************************************************************
*! Name      : lfPath  (C# 102135)
*! Developer : AHMED MOHAMED ELANWAR (AME)
*! Date      : 04/05/2001
*! Purpose   : Default Value of Path .
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
*! Example   : =lfPath()
*!*************************************************************
FUNCTION lfPath
lcPath = ''
IF FILE("&MEMFILE..MEM")
  RESTORE ADDITIVE FROM &MEMFILE
ENDIF
RETURN lcPath
*!*************************************************************
*! Name      : lfGet8AOts
*! Developer : Abdou Elgendy
*! Date      : 09/17/2003
*! Purpose   : get the next style hold 'A' at the end.
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
*! Example   : =lfGet8AOts()
*!*************************************************************
*!C119695,1 Begin
FUNCTION lfGet8AOts
PARAMETER lcSndStylM , lcSndStyle
PRIVATE lnPrvAls , lnRecNo , lnOpenTS , lnOpen   
STORE 0 TO lnOpen , lnOpenTS
lnPrvAls   = SELECT(0)
lnRecNo    = RECNO()

SELECT STYLE 
lcSeekStyle = STRTRAn(lcSndStyle,ALLTRIM(lcSndStylM)+' ',ALLTRIM(lcSndStylM)+'A')

*-- Check if the style is exist or not.
IF SEEK(lcSeekStyle)
  FOR lnSize = 1 to 8
    lcSize = STR(lnSize,1)
    lnOpen   = (WIP&lcSize + STK&lcSize - ORD&lcSize )

    IF lnOpen > 0
      OTS = OTS + lnOpen
      lnOpenTS =  lnOpenTS + lnOpen
    ENDIF
  ENDFOR
ENDIF


SELECT (lnPrvAls)
GOTO lnRecNo

RETURN lnOpenTS
*-- End OF lfGet8AOts
*!C119695,1 End
*!*************************************************************