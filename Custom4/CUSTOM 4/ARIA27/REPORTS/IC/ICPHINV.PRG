*:***************************************************************************
*: Program file  : ICPHINV.PRG
*: Program desc. : Physical Inventory WorkSheet Report
*: For Report    : (ICPHINV.FRX)
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfDispRe,gfItemMask.
*:               : lfwRepWhen,lfwOldVal,lfClearRep,
*:               : lfEvalSegs,lfSRVSty,lfStySum,lfvFabric,FaBrow,
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ICPHINV
*:***************************************************************************
*: This Report Program is due to E301080 ...
*:***************************************************************************
*:Modifications : 
*:B603856,1 MHM 08/29/2000  When trying to print to Phy Invt worksheet got message 
*:                          Alias 'fabric' not found, Lfwrepwhen reports\ic\ICPHINV
*:C125875,1 BWA 12/25/2004 Add new FRX to print page for every Bin.
*:***************************************************************************

lcStTime   = TIME()
llRpByBins = llMultBin AND llRpByBins

lcMakeOp  = IIF(lcRpMake='M','','!')
lcRepExpr = IIF(lcRpMake='B','', [ AND ] + lcMakeOp+[STYLE.Make]) +;
            IIF(lcRpStatus='B','',[ AND (STYLE.Status="] + lcRpStatus + [")])

lcRpExp =  lcRepExpr + IIF( '.T.' $ lcRpExp , '' , [ AND ] + lcRpExp )

SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE

*-- If user want to print by Bin.
IF llRpByBins
  SELECT WHSLOC
  SET RELATION TO STYLE INTO STYLE , cWareCode INTO WAREHOUS
  lcOptiExpr = [cwarecode+clocation+style+color = '']

ELSE  && else normal print.

  lcRpExp    = STRTRAN(lcRpExp,'STYLE.','') 
  lcOptiExpr = [STYLE = '']
ENDIF
lcRpExp = lcOptiExpr + lcRpExp

*-- Assign Report Variables [begin]
*-- if style have color segment.
IF lcFree_Clr = 'C'
  lcHeader = [lcStyTitle + ' ' + lcNonMajTl + ' Description             ' + lcStyMajor + ' Desc.']
  lcDetail = [STYLE.STYLE + ' ' + gfCodDes(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen) , 'COLOR') + ' ' + STYLE.DESC ]

ELSE  && else style does not have color segment

  lcHeader = [lcStyTitle  + ' ' + 'Description']
  lcDetail = [STYLE.STYLE + ' ' + PADR(STYLE.DESC1,52)]
ENDIF
*-- Assign Report Variables [end]

DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRpExp
lcLastExpr = lcRpExp
*-- end of report code.

*-- Start of function and procedure section.
*-------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/30/1998
*! Purpose   : When function of OG
*!*************************************************************
*! Called from : OG read cycle
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*
FUNCTION lfwRepWhen

IF TYPE('lcLastExpr') $ 'UL' OR EMPTY(lcLastExpr)
  *:B603856,1 MHM 08/29/2000[Begin]
  IF ('MA' $ gcComp_Mdl)
    SET ORDER TO FABRIC IN FABRIC
  ENDIF 
  *:B603856,1 MHM 08/29/2000 [End]
  SET ORDER TO STYLE IN STYLE
ENDIF
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/30/1998
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
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/30/1998
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs
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

  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
  *-- Compute Free/Color Items in Style code Structure. [End]

  *-- Compute Variables titles. [begin]
  lcStyBinTl = 'Print ' + lcStyMajor + '(s) by Bins' 
  lcStyStat  = lcStyMajor + ' Status'
  lcStyMake  = lcStyMajor + ' (Make/Buy)'
  *-- Compute Variables titles. [end]

RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/30/1998
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
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/30/1998
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

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/30/1998
*! Purpose   : Valid function of the Fabric
*!*************************************************************
*! Called from : Option grid [Fabric Get field]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvFabric

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Fabric he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*--End of lfvFabric.

*C125875,1 BWA 12/25/2004 Add new FRX to print page for every Bin.[START]
*!*************************************************************
*! Name      : lfChngForm
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 08/25/2004
*! Purpose   : Change printed form.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChngForm()
*!*************************************************************
FUNCTION lfChngForm

IF llRpByBins
  lcRPForm = "ICPHINVB"
ELSE
  lcRPForm = "ICPHINV"
ENDIF

=lfRepPltFr(lcRPForm)

*--End of lfChngForm.
*C125875,1 BWA 12/25/2004.[END]