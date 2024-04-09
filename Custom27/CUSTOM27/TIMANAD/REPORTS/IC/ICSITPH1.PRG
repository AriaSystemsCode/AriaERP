 *!********************************************************************
*: Program file  : ICSITPH1 (REFERENCE, C200172)
*: Program desc. : CUSTOMIZED Print Sites Activity and Stock on Hand for APPAREL RESOURCE
*: For screen    : None.
*:         System: ARIA APPAREL SERIES 2.7
*:         Module: REPORT
*:      Developer: WAB - Walid A. Wahab
*:      Date     : 05/08/2001
*!********************************************************************
*: Calls         :=lfSlctPo()
*:               :=lfSlcSty()
*:               :=lfSitTyp(style)
*:               :=lfGetSit('A'+ACCOUNT)
*:               :=lfGetLocation(site)
*:               :=lfPrvBal(STYLE)
*:               :=lfPoslPvBa(STYLE)
*:               :=lfSitTyp(site)
*:               :=lfComputQTy(STYLE) 
*:               :=lfSolRet(STYLE)
*:               :=lfZeroVal()
*:               :=lfCreatCusr()
*:               :=lfEvalSegs()
*:               :=lfsrvSty() 
*:               :=lfStySum()
*:               :=lfvcust() 
*:               :=lfRecved()
*:               :=lfPCsIn()  
*:               :=lfPCsOut() 
*:               :=lfRetunS()
*:               :=lfRetunD() 
*!********************************************************************
*: Passed Parameters  :NONE
*!********************************************************************
*: Example            : ICSITTIM
*!********************************************************************
STORE ' ' TO m.Style,m.Scale,m.Account,m.difsolprc,m.DIFRCPRC
STORE 0 TO m.Prv1,m.Prv2,m.Prv3,m.Prv4,m.Prv5,m.Prv6,m.Prv7,m.Prv8,m.TotPrv,;
           m.Rev1,m.Rev2,m.Rev3,m.Rev4,m.Rev5,m.Rev6,m.Rev7,m.Rev8,m.TotRev,;
           m.Rets1,m.Rets2,m.Rets3,m.Rets4,m.Rets5,m.Rets6,m.Rets7,m.Rets8,m.TotRets,;
           m.Retd1,m.Retd2,m.Retd3,m.Retd4,m.Retd5,m.Retd6,m.Retd7,m.Retd8,m.TotRetd,;
           m.Pcsi1,m.Pcsi2,m.Pcsi3,m.Pcsi4,m.Pcsi5,m.Pcsi6,m.Pcsi7,m.Pcsi8,m.TotPcsi,;
           m.PcsO1,m.PcsO2,m.PcsO3,m.PcsO4,m.PcsO5,m.PcsO6,m.PcsO7,m.PcsO8,m.TotPcso,;
           m.Sol1,m.Sol2,m.Sol3,m.Sol4,m.Sol5,m.Sol6,m.Sol7,m.Sol8,m.TotSol,;
           m.Rcst1,m.Rcst2,m.Rcst3,m.Rcst4,m.Rcst5,m.Rcst6,m.Rcst7,m.Rcst8,m.TotRcst,m.price,;
           m.Amount,m.nSprice,lnAmount,m.lnpRice,m.RAMOUNT


STORE .F. TO m. lPrintGrnd
STORE {} TO ldRldSrt , ldRldEnd
lcRldAfect = ''
lnRldPcnt  = 0
*--
DECLARE laDRltFld[4,2]
laDRltFld[1,1] = 'START'
laDRltFld[1,2] ='ldRldSrt'
laDRltFld[2,1] = 'DENDATE'
laDRltFld[2,2] ='ldRldEnd'
laDRltFld[3,1] = 'CCOSTAFECT'
laDRltFld[3,2] = 'lcRldAfect'
laDRltFld[4,1] = 'DISCPCNT'
laDRltFld[4,2] = 'lnRldPcnt'

STORE "" TO lcPoFile,lcStyFile,lcMasterFile,lcFilter,lcDetail,lcRelatFld,lcAccount
STORE .F. TO llSlctPo,llStyPo
STORE {} TO dLowDate,dHighDate 
STORE 0 TO lnFirstOcc
STORE '' TO lcExp
DIMENSION laRelatFld[1,2]
laRelatFld[1,1]='CCMSITETYP'
laRelatFld[1,2]='lcRelatFld'
*--array to hold fields for temproray file
DIMENSION laFileStru[84,4]

*-set needed relation
SELECT POSLN
SET RELATION TO cStyType+PO INTO POSHDR
*--get the postion of account in laogfxflt
=lfSlctPo()
*--get the style position in laogfxflt
=lfSlcSty()
lnStyPos     = AT('INLIST(STYLE.STYLE',lcRpExp)
IF lnStyPos>0
  lnPos1       = AT('AND' , SUBSTR(lcRpExp,lnStyPos ))
  IF lnPos1>0
    lcStyExp  = SUBSTR(lcRpExp ,lnStyPos , lnPos1-1)
  ELSE
    lnPos1   = LEN(lcRpExp)
    lcStyExp  = SUBSTR(lcRpExp ,lnStyPos , lnPos1+2) 
  ENDIF

ELSE
  lcStyExp=".T."
ENDIF
*--assign files name to variable
lcMasterFile =IIF(llSlctPo,lcPoFile,"WAREHOUS")
lcFilter     = " "

*--set relation we needed  in case we are not use style file but we use temporary file contain 
IF llSlctPo
  SELECT (lcPoFile)
  SET RELATION TO cWareCOde INTO WareHous
ENDIF

IF EMPTY(ldRpFDt) AND EMPTY(ldRpTDt)
  dLowDate=DATE()
  dHighDate=DATE()  
ELSE
  dLowDate=ldRpFDt
  dHighDate=ldRpTDt
ENDIF


*--create tmp file
lcTmpCursr  = gfTempName() 
=lfCreatCusr()
SELECT (lcMasterFile) 
*--scan through sites
SCAN 
  *wab wait window
  WAIT WINDOW 'Location # :' + cWareCode NOWAIT

  *--get location which assigned to site from warehouse file
   lcWareCode=WareHous.cWARECODE 
   M.location=WareHous.cWARECODE
   M.DESC    =WAREHOUS.cDESC 
   lcSit     =WareHous.cSITEID
  *--get site type of mysit
  lcMySitTyp=lfSitTyp(lcSit)
  lcMySitTyp=ALLTRIM(lcMySitTyp)
  IF lcMySitTyp<>'P'
    LOOP
  ENDIF
  *--put zero value to all memvar
  =lfZeroVal()
  SELECT STYLE
  SCAN FOR &lcStyExp
    *wab wait window
    WAIT WINDOW 'STYLE IS' + STYLE.STYLE NOWAIT
    *--compute previous balance from styinvjl file
    STORE '' TO   m.difsolprc,m.DIFRCPRC
    m.Style=STYLE.STYLE
    m.PRICE=STYLE.nSugRetPri
    m.SCALE=STYLE.SCALE
    m.lnPRICE=STYLE.nSugRetPri
    m.nSprice=STYLE.nSugRetPri
    =lfPrvBal(STYLE)
    *--compute previous balance from posline file
    =lfPoslPvBa(STYLE)
    *--compute receive from vendor &return to vedor &pcsin &pcsout
    =lfComputQTy(STYLE) 
    *--compute sold and return from customer
    =lfSolRet(STYLE)
    *--insert record in cursor
    
    IF !llRpZero
      IF m.TotRev!=0 OR m.TotRets!=0 OR m.TotRetd!=0 ;
         OR m.TotPcsi!=0 OR m.TotPcso!=0 OR m.TotSol!=0;
         OR m.TotRcst!=0 
         INSERT INTO (lcTmpCursr) FROM MEMVAR
        
      ENDIF
    ELSE
      INSERT INTO (lcTmpCursr) FROM MEMVAR
    ENDIF  
    *--calling function to put zero in all memvar
    =lfZeroVal()
    
  ENDSCAN  && for lcDetail file
ENDSCAN && for master file
*--
*KAM
SELECT STYLE 
lcOrd=ORDER()
SET ORDER TO STYLE 

SELECT (lcTmpCursr)
SET ORDER TO ACOUNT
IF RECCOUNT()=0
  =gfDialog('I','There are no records to display')
  RETURN
ELSE
  GO BOTTOM
  REPLACE lPrintGrnd WITH .T.
ENDIF
LOCATE
SET RELATION TO "S"+SCALE INTO SCALE ADDTIVE
SET RELATION TO STYLE INTO STYLE ADDITIVE 

DO gfDispRe WITH EVAL('lcRpForm')
SELECT STYLE 
SET ORDER TO &lcOrd
RETURN
*--
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
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
  *lcStyStat = lcStyMajor + ' Status'
  *lcStyMake = lcStyMajor + ' (Make/Buy)'
  *-- Compute Variables titles. [end]

RETURN ''
*-- end of lfEvalSegs.
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
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
    *USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'cStyle','SH',@lcTStyFile,.T.)
    SELECT STYLE
    SET ORDER TO TAG STYLE
    *SET RELATION TO STYLE.STYLE INTO STYLE_X
    *GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN (lcTStyFile)
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.
*!*************************************************************
*! Name      : lfStySum
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
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
*SELECT Style
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
*! Name      : lfvcust
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
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
*! Example   : =lfvcust
*!*************************************************************
*! Note      : 
*!*************************************************************
FUNCTION lfVCUST
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SELECT CUSTOMER
    SET FILTER TO SEEK('A'+ACCOUNT,'EDIACPRT') AND EDIACPRT.linterComp
  CASE lcParm = 'R'  && Reset code
    SELECT CUSTOMER
    SET FILTER TO
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfGetSit
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : GET SITE ID
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
*! Example   : =lfGetSit
*!*************************************************************
*! Note      : to get sit from ediacprt
*!*************************************************************
FUNCTION lfGetSit
PARAMETERS lcSearhSit
PRIVATE lcOldArea,lcRetVal
lcRetVal=' '  
lcOldArea=SELECT()
SELECT EDIACPRT
IF SEEK(lcSearhSit)
 lcRetVal=cSiteId
ENDIF  
SELECT(lcOldArea)
RETURN lcRetVal
*--end of function lfGetSit

*!*************************************************************
*! Name      : lfSitTyp
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : GET TYPE OF SITE
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
*! Example   : =lfSitTyp
*!*************************************************************
*! Note      : to get site type from codes file
*!*************************************************************
FUNCTION lfSitTyp
PARAMETERS lcSit
PRIVATE lcOldArea,lcRetVal
lcRetVal=' '  
lcOldArea=SELECT()
=gfRltFld(lcSit, @laRelatFld , 'CSITEID')
lcRetVal=lcRelatFld
RETURN lcRetVal
*-end of function lfSitTyp 
*!*************************************************************
*! Name      : lfGetLocation
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : GET THE WAREHOUSE CODE FOR THE SITE ID
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
*! Example   : =lfGetLocation
*!*************************************************************
*! Note      : to get location from warehouse file
*!*************************************************************
FUNCTION lfGetLocation
PARAMETERS lcSit
PRIVATE lcOldArea,lcRetVal
STORE ' '  TO laRetVal
lcOldArea=SELECT()
SELECT WAREHOUS
LOCATE FOR cSITEID=lcSit
IF FOUND()
  lcRetVal=cWARECODE 
ENDIF
SELECT(lcOldArea)
RETURN lcRetVal
*--end of function lfGetLocation

*!*************************************************************
*! Name      : lfCreatCusr
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : create temporaray file
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : style
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatCusr
*-- Fields for the po line quantity.

laFileStru[ 1 , 1] = 'Style'
laFileStru[ 1 , 2] = 'C'
laFileStru[ 1 , 3] = 19
laFileStru[ 1 , 4] = 0



*--fields for previous qty
lnCount=1
FOR X = 2 TO 9
  laFileStru[X , 1] = 'Prv'+ALLTRIM(STR(lnCount))
  laFileStru[X , 2] = 'N'
  laFileStru[X , 3] = 6
  laFileStru[X , 4] = 0
  lnCount=lnCount+1
ENDFOR

*-- Field for the Total previous quantity.
laFileStru[10 , 1] = 'TotPrv'
laFileStru[10 , 2] = 'N'
laFileStru[10 , 3] = 7
laFileStru[10 , 4] = 0


*--fields for receive from vendor qty

lnCount=1
FOR X = 11 TO 18
  laFileStru[X , 1] = 'Rev'+ALLTRIM(STR(lnCount))
  laFileStru[X , 2] = 'N'
  laFileStru[X , 3] = 6
  laFileStru[X , 4] = 0
  lnCount=lnCount+1
ENDFOR

*-- Field for the Total of receive from vendor quantity.
laFileStru[19 , 1] = 'TotRev'
laFileStru[19 , 2] = 'N'
laFileStru[19 , 3] = 7
laFileStru[19 , 4] = 0



*--fields for return to vendor normal stok qty

lnCount=1
FOR X = 20 TO 27
  
  laFileStru[X , 1] = 'Rets'+ALLTRIM(STR(lnCount))
  laFileStru[X , 2] = 'N'
  laFileStru[X , 3] = 6
  laFileStru[X , 4] = 0
  lnCount=lnCount+1
ENDFOR

*-- Field for the Total of return to vendor normal stok qty quantity.
laFileStru[28 , 1] = 'TotRets'
laFileStru[28 , 2] = 'N'
laFileStru[28 , 3] = 7
laFileStru[28 , 4] = 0




*--fields for return to vendor demage stok qty
lnCount=1
FOR X = 29 TO 36
  
  laFileStru[X , 1] = 'Retd'+ALLTRIM(STR(lnCount))
  laFileStru[X , 2] = 'N'
  laFileStru[X , 3] = 6
  laFileStru[X , 4] = 0
  lnCount=lnCount+1
ENDFOR

*-- Field for the Total of return to vendor normal stok qty quantity.
laFileStru[37 , 1] = 'TotRetd'
laFileStru[37 , 2] = 'N'
laFileStru[37 , 3] = 7
laFileStru[37 , 4] = 0


*--fields for pcsin stok qty

lnCount=1
FOR X = 38 TO 45
  
  laFileStru[X , 1] = 'Pcsi'+ALLTRIM(STR(lnCount))
  laFileStru[X , 2] = 'N'
  laFileStru[X , 3] = 6
  laFileStru[X , 4] = 0
  lnCount=lnCount+1
ENDFOR

*-- Field for the Total of pcsin quantity.
laFileStru[46 , 1] = 'TotPcsi'
laFileStru[46 , 2] = 'N'
laFileStru[46 , 3] = 7
laFileStru[46 , 4] = 0


*--fields for pcsout stok qty

lnCount=1
FOR X = 47 TO 54
  
  laFileStru[X , 1] = 'PcsO'+ALLTRIM(STR(lnCount))
  laFileStru[X , 2] = 'N'
  laFileStru[X , 3] = 6
  laFileStru[X , 4] = 0
  lnCount=lnCount+1
ENDFOR

*-- Field for the Total of pcsout quantity.
laFileStru[55 , 1] = 'TotPcsO'
laFileStru[55 , 2] = 'N'
laFileStru[55 , 3] = 7
laFileStru[55 , 4] = 0



*--fields for sold stok qty
lnCount=1
FOR X = 56 TO 63
  
  laFileStru[X , 1] = 'Sol'+ALLTRIM(STR(lnCount))
  laFileStru[X , 2] = 'N'
  laFileStru[X , 3] = 6
  laFileStru[X , 4] = 0
  lnCount=lnCount+1
ENDFOR

*-- Field for the Total of sold quantity.
laFileStru[64 , 1] = 'TotSol'
laFileStru[64 , 2] = 'N'
laFileStru[64 , 3] = 7
laFileStru[64 , 4] = 0



*--fields for return from customer

lnCount=1
FOR X = 65 TO 72
  
  laFileStru[X , 1] = 'Rcst'+ALLTRIM(STR(lnCount))
  laFileStru[X , 2] = 'N'
  laFileStru[X , 3] = 6
  laFileStru[X , 4] = 0
  lnCount=lnCount+1
ENDFOR

*-- Field for the Total of return from customer quantity.
laFileStru[73 , 1] = 'TotRcst'
laFileStru[73 , 2] = 'N'
laFileStru[73 , 3] = 7
laFileStru[73 , 4] = 0

*-- field for price
laFileStru[74, 1] = 'Price'
laFileStru[74, 2] = 'N'
laFileStru[74, 3] = 7
laFileStru[74, 4] = 2

laFileStru[75, 1] = 'Scale'
laFileStru[75, 2] = 'C'
laFileStru[75, 3] = 3
laFileStru[75, 4] = 0

laFileStru[76, 1] = 'location'
laFileStru[76, 2] = 'C'
laFileStru[76, 3] = 6
laFileStru[76, 4] = 0

laFileStru[77, 1] = 'DESC'
laFileStru[77, 2] = 'C'
laFileStru[77, 3] = 30
laFileStru[77, 4] = 0


laFileStru[78, 1] = 'AMOUNT'
laFileStru[78, 2] = 'N'
laFileStru[78, 3] = 10
laFileStru[78, 4] = 2

laFileStru[79, 1] = 'nSPRICE'
laFileStru[79, 2] = 'N'
laFileStru[79, 3] = 6
laFileStru[79, 4] = 2

laFileStru[80, 1] = 'lnprice '
laFileStru[80, 2] = 'N'
laFileStru[80, 3] = 6
laFileStru[80, 4] = 2


laFileStru[81, 1] = 'RAMOUNT'
laFileStru[81, 2] = 'N'
laFileStru[81, 3] = 10
laFileStru[81, 4] = 2


laFileStru[82, 1] = 'difsolprc'
laFileStru[82, 2] = 'C'
laFileStru[82, 3] = 6
laFileStru[82, 4] = 0


laFileStru[83, 1] = 'DIFRCPRC'
laFileStru[83, 2] = 'C'
laFileStru[83, 3] = 6
laFileStru[83, 4] = 0



laFileStru[84, 1] = 'lPrintGrnd'
laFileStru[84, 2] = 'L'
laFileStru[84, 3] = 1
laFileStru[84, 4] = 0

CREATE CURSOR (lcTmpCursr) FROM ARRAY laFileStru
INDEX ON location+STYLE TAG ACOUNT
*--end of function lfCREATCUSR

*!*************************************************************
*! Name      : lfPrvBal
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : compute previous balance from styinvjl file
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : style
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfPrvBal
PARAMETERS lcStyle
PRIVATE lcOldArea
lcOldArea=SELECT()
lcExp = ' Dtrdate < dLowDate AND ctrtype !="6" '
SELECT STYINVJL
IF !SEEK(lcStyle+lcWareCode)
  SELECT(lcOldArea) 
  RETURN
ENDIF

SCAN REST WHILE  Style+cWareCode=lcStyle+lcWareCode FOR &lcExp
  m.Prv1   =m.Prv1   + NStk1
  m.Prv2   =m.Prv2   + NStk2
  m.Prv3   =m.Prv3   + NStk3
  m.Prv4   =m.Prv4   + NStk4
  m.Prv5   =m.Prv5   + NStk5
  m.Prv6   =m.Prv6   + NStk6
  m.Prv7   =m.Prv7   + NStk7
  m.Prv8   =m.Prv8   + NStk8
  m.TotPrv =m.TotPrv + NTotStk
ENDSCAN

SELECT(lcOldArea) 
RETURN
*--end of function lfPrvBal
*!*************************************************************
*! Name      : lfPoslPvBa
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : compute previous balance from posln file
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : style
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfPoslPvBa
PARAMETERS lcStyle
PRIVATE lcOldArea,lcOldOrder,lnQt1,lnQt2,lnQt3,lnQt4,lnQt5,lnQt6,lnQt7,lnQt8,lnQt9
STORE 0 TO lnQt1,lnQt2,lnQt3,lnQt4,lnQt5,lnQt6,lnQt7,lnQt8,lnQt9
lcOldArea=SELECT()
lcExp='POSHDR.ENTERED < dLowDate AND TRANCD ="6" AND (VENDOR=lcWareCode OR cWARECODE=lcWareCode )'
SELECT POSLN
lcOldOrder=ORDER()
SET ORDER TO TAG Poslns
IF !SEEK(lcStyle)
  SELECT POSLN
  SET ORDER TO &lcOldOrder
  SELECT(lcOldArea)
  RETURN
ENDIF
SCAN REST WHILE Style=lcStyle  FOR &lcExp
  IF VENDOR=lcWareCode
    lnQt1=-Qty1
    lnQt2=-Qty2
    lnQt3=-Qty3
    lnQt4=-Qty4
    lnQt5=-Qty5
    lnQt6=-Qty6
    lnQt7=-Qty7
    lnQt8=-Qty8
    lnQt9=-TotQty
  ENDIF
  IF cWARECODE=lcWareCode
    lnQt1=Qty1
    lnQt2=Qty2
    lnQt3=Qty3
    lnQt4=Qty4
    lnQt5=Qty5
    lnQt6=Qty6
    lnQt7=Qty7
    lnQt8=Qty8
    lnQt9=TotQty
  ENDIF
  m.Prv1   =m.Prv1   +lnQt1
  m.Prv2   =m.Prv2   +lnQt2
  m.Prv3   =m.Prv3   +lnQt3
  m.Prv4   =m.Prv4   +lnQt4
  m.Prv5   =m.Prv5   +lnQt5
  m.Prv6   =m.Prv6   +lnQt6
  m.Prv7   =m.Prv7   +lnQt7
  m.Prv8   =m.Prv8   +lnQt8
  m.TotPrv =m.TotPrv +lnQt9
ENDSCAN
SELECT POSLN
SET ORDER TO &lcOldOrder
SELECT(lcOldArea)
RETURN 
*--end of function lfPoslPvBa
*!*************************************************************
*! Name      : lfSlctPo
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : Function to check if there is selected critira.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfSlctPo

lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'WAREHOUS.CWARECODE'),1)
IF lnPosition > 0
  lcPoFile = laOGFxFlt[lnPosition,6]
  llSlctPo = IIF(!EMPTY(lcPoFile) .AND. USED(lcPoFile) .AND. RECCOUNT(lcPoFile) > 0 , .T. , .F. )
ENDIF
*--end of function  lfSlctPo
*!*************************************************************
*! Name      : lfSlcSty
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : Function to check if there is selected critira.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfSlcSty

lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.STYLE'),1)
IF lnPosition > 0
  lcStyFile = laOGFxFlt[lnPosition,6]
  llStyPo = IIF(!EMPTY(lcStyFile) .AND. USED(lcStyFile) .AND. RECCOUNT(lcStyFile) > 0 , .T. , .F. )
ENDIF
*--end of function lfSlcSty

*!*************************************************************
*! Name      : lfComputQTy
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters :STYLE 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfComputQTy
PARAMETERS lcStyle

PRIVATE lcOldArea,lcExp,lcSitCode,lcSitType,lcOldOrder
lcOldArea=SELECT()
*lcExp = ' BETWEEN(dAdd_date,dLowDate,dHighDate)  AND (VENDOR=lcWareCode OR cWareCode=lcWareCode) '
lcExp ='BETWEEN(POSHDR.ENTERED,dLowDate,dHighDate)  AND TranCd ="6"  AND (VENDOR=lcWareCode OR cWareCode=lcWareCode) '
SELECT POSLN
lcOldOrder=ORDER()
SET ORDER TO TAG Poslns
IF !SEEK(lcStyle)
  SELECT POSLN
  SET ORDER TO &lcOldOrder
  SELECT(lcOldArea)
  RETURN
ENDIF
SCAN REST WHILE Style=lcStyle FOR &lcExp
 IF VENDOR =lcWareCode  
   lcSitCode=lfSitFWrh(POSLN.cWARECODE)
   lcSitType=lfSitTyp(lcSitCode)
   lcSitType=ALLTRIM(lcSitType)
   IF lcSitType = 'P'
     =lfPCsOut()
   ELSE
     lcWareDesc=lfGetDesc(POSLN.cWARECODE)      
     IF SUBSTR(lcWareDesc,1,2) ="ST"
       =lfRetunS()
     ELSE
       =lfRetunD()
     ENDIF  
   ENDIF
 ELSE
   lcSitCode=lfSitFWrh(POSLN.VENDOR)
   lcSitType=lfSitTyp(lcSitCode) 
   lcSitType=ALLTRIM(lcSitType)
   IF lcSitType = 'P'
     =lfPCsIn()
   ELSE
     =lfRecved()
   ENDIF
 ENDIF
ENDSCAN
SELECT POSLN
SET ORDER TO &lcOldOrder
SELECT (lcOldArea)
RETURN 
*!*************************************************************
*! Name      : lfRecved
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRecved
PRIVATE lcOldArea
lcOldArea=SELECT()
SELECT POSLN
m.Rev1  =m.Rev1+Qty1
m.Rev2  =m.Rev2+Qty2
m.Rev3  =m.Rev3+Qty3
m.Rev4  =m.Rev4+Qty4
m.Rev5  =m.Rev5+Qty5
m.Rev6  =m.Rev6+Qty6
m.Rev7  =m.Rev7+Qty7
m.Rev8  =m.Rev8+Qty8
m.TotRev=m.TotRev+TotQty
SELECT(lcOldArea)
RETURN
*--end of function lfRecved
*****************************************************
*! Name      : lfRetunS
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRetunS
PRIVATE lcOldArea
lcOldArea=SELECT()
SELECT POSLN
m.RetS1  =m.RetS1+Qty1
m.RetS2  =m.RetS2+Qty2
m.RetS3  =m.RetS3+Qty3
m.RetS4  =m.RetS4+Qty4
m.RetS5  =m.RetS5+Qty5
m.RetS6  =m.RetS6+Qty6
m.RetS7  =m.RetS7+Qty7
m.RetS8  =m.RetS8+Qty8
m.TotRets=m.TotRetS+TotQty
SELECT(lcOldArea)
RETURN
*--end of function lfRetun

*****************************************************
*! Name      : lfRetunD
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRetunD
PRIVATE lcOldArea
lcOldArea=SELECT()
SELECT POSLN
m.RetD1  =m.RetD1+Qty1
m.RetD2  =m.RetD2+Qty2
m.RetD3  =m.RetD3+Qty3
m.RetD4  =m.RetD4+Qty4
m.RetD5  =m.RetD5+Qty5
m.RetD6  =m.RetD6+Qty6
m.RetD7  =m.RetD7+Qty7
m.RetD8  =m.RetD8+Qty8
m.TotRetd=m.TotRetD+TotQty
SELECT(lcOldArea)
RETURN
*--end of function lfRetun

*!*************************************************************
*! Name      : lfPCsIn
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfPCsIn
PRIVATE lcOldArea
lcOldArea=SELECT()
SELECT POSLN
m.Pcsi1 =m.Pcsi1+Qty1
m.Pcsi2 =m.Pcsi2+Qty2
m.Pcsi3 =m.Pcsi3+Qty3
m.Pcsi4 =m.Pcsi4+Qty4
m.Pcsi5 =m.Pcsi5+Qty5
m.Pcsi6 =m.Pcsi6+Qty6
m.Pcsi7 =m.Pcsi7+Qty7
m.Pcsi8 =m.Pcsi8+Qty8
m.TotPcsi=m.TotPcsi+TotQty
SELECT(lcOldArea)
RETURN
*--end of function lfRecved           
           
*!*************************************************************
*! Name      : lfPCsOut
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfPCsOut
PRIVATE lcOldArea
lcOldArea=SELECT()
SELECT POSLN
m.PcsO1  =m.PcsO1+Qty1
m.PcsO2  =m.PcsO2+Qty2
m.PcsO3  =m.PcsO3+Qty3
m.PcsO4  =m.PcsO4+Qty4
m.PcsO5  =m.PcsO5+Qty5
m.PcsO6  =m.PcsO6+Qty6
m.PcsO7  =m.PcsO7+Qty7
m.PcsO8  =m.PcsO8+Qty8
m.TotPcso=m.TotPcso+TotQty
SELECT(lcOldArea)
RETURN
*--end of function lfRecved           
                                 

*****************************************************
*! Name      : lfSolRet
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfSolRet
PARAMETERS lcStyle
PRIVATE lcOldArea,lcExp,lcOrder,llStartlnFldPRC1,lnFldPRC2,llRCStart
STORE 0 TO lnFldPRC1,lnFldPRC2
lcOldArea=SELECT()
lcEXP='BETWEEN(dPrd_Bgn,dLowDate,dHighDate)  AND STORE =lcWareCode '
SELECT PRODACTI
lcOrder=ORDER()
SET ORDER TO TAG ACTIPRD
IF !SEEK('TIMND'+lcStyle)
  SET ORDER TO(lcOrder)
  SELECT(lcOldArea)
  RETURN 
ENDIF
llStart=.F.
llRCStart=.F.

SCAN REST WHILE ACCOUNT+STYLE =  'TIMND'+lcStyle FOR &lcEXP
  lcCode=lfGetDisc(PRODACTI.style,PRODACTI.store)
  IF !EMPTY(lcCode)
    =gfRltFld(lcCode,@laDRltFld,'CDISCCODE')      
  ENDIF
  IF ALLTRIM(lcRldAfect)='R' OR ALLTRIM(lcRldAfect)='B'
    IF BETWEEN(PRODACTI.dprd_bgn,ldRldSrt,ldRldEnd)
      IF  TRAN_TYPE="QS"
        m.nSprice=m.Price
        m.nSPrice=m.nSPrice-(m.nSPrice*(lnRldPcnt/100))  
      ELSE
        m.lnprice=m.Price
        m.lnPrice=m.lnPrice-(m.lnPrice*(lnRldPcnt/100))  
      ENDIF  
    ELSE
      m.nSPrice=m.Price
      m.lnprice=m.Price
    ENDIF
  ELSE
    m.nSPrice=m.Price
    m.lnprice=m.Price
  ENDIF
  IF  TRAN_TYPE="QS"
    =lfGetSld()
  ELSE
    =lfRCust()
  ENDIF
  
  IF  TRAN_TYPE="QS"
    IF !llStart
      lnFldPRC1=m.nSPrice
      llStart=.T.
    ENDIF
  ELSE
   IF !llRCStart
     lnFldPRC2=m.lnprice 
     llRCStart=.T.
   ENDIF
 ENDIF  
  
 IF  TRAN_TYPE="QS" 
   m.difsolprc = IIF(lnFldPRC1<>m.nSPrice ,'***.**','')
 ELSE
  m.DIFRCPRC  = IIF(lnFldPRC2<>m.lnprice ,'***.**','')
ENDIF  
ENDSCAN
SET ORDER TO(lcOrder)
SELECT(lcOldArea)
RETURN  

*!*************************************************************
*! Name      : lfGetSld
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGetSld
PRIVATE lcOldArea,lnTotOne
STORE 0 TO lnTotOne
lcOldArea=SELECT()
SELECT PRODACTI
m.Sol1  =m.Sol1+nActiQty1
m.Sol2  =m.Sol2+nActiQty2
m.Sol3  =m.Sol3+nActiQty3
m.Sol4  =m.Sol4+nActiQty4
m.Sol5  =m.Sol5+nActiQty5
m.Sol6  =m.Sol6+nActiQty6
m.Sol7  =m.Sol7+nActiQty7
m.Sol8  =m.Sol8+nActiQty8
lnTotOne=nActiQty1+nActiQty2+nActiQty3+nActiQty4+nActiQty5+nActiQty6+nActiQty7+nActiQty8
m.TotSol=m.TotSol+ntotactqty
m.Amount=m.Amount+(m.nSPrice*lnTotOne)
SELECT(lcOldArea)
RETURN
*--end of function lfSol
*!*************************************************************
*! Name      : lfRCust
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRCust
PRIVATE lcOldArea,lnTotOne
STORE 0 TO lnTotOne
lcOldArea=SELECT()
SELECT PRODACTI
m.Rcst1  =m.Rcst1+nActiQty1
m.Rcst2  =m.Rcst2+nActiQty2
m.Rcst3  =m.Rcst3+nActiQty3
m.Rcst4  =m.Rcst4+nActiQty4
m.Rcst5  =m.Rcst5+nActiQty5
m.Rcst6  =m.Rcst6+nActiQty6
m.Rcst7  =m.Rcst7+nActiQty7
m.Rcst8  =m.Rcst8+nActiQty8
lnTotOne=nActiQty1+nActiQty2+nActiQty3+nActiQty4+nActiQty5+nActiQty6+nActiQty7+nActiQty8
m.TotRcst=m.TotRcst+ntotactqty
m.RAmount=m.RAmount+(m.lnPrice*lnTotOne)
SELECT(lcOldArea)
RETURN
*--end of function
*****************************************************
*! Name      : lfSitFWrh
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfSitFWrh
PARAMETERS lcWare
PRIVATE lcOldArea,lcSitName
STORE ' ' TO lcSitName
lcOldArea=SELECT()
SELECT WAREHOUS
IF SEEK(ALLTRIM(lcWare))
  lcSitName=cSiteId
ENDIF
SELECT(lcOldArea)
RETURN lcSitName
*--end of function lfSitFWrh

*****************************************************
*! Name      : lfGetDesc
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGetDesc
PARAMETERS lcWare
PRIVATE lcOldArea,lcDesc
STORE ' ' TO lcDesc
lcOldArea=SELECT()
SELECT WAREHOUS
IF SEEK(ALLTRIM(lcWare))
  lcDesc=cDesc
ENDIF
SELECT(lcOldArea)
RETURN lcDesc
*--end of function lfGetDesc
*****************************************************
*! Name      : lfZeroVal
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfZeroVal

STORE 0 TO m.Prv1,m.Prv2,m.Prv3,m.Prv4,m.Prv5,m.Prv6,m.Prv7,m.Prv8,m.TotPrv,;
           m.Rev1,m.Rev2,m.Rev3,m.Rev4,m.Rev5,m.Rev6,m.Rev7,m.Rev8,m.TotRev,;
           m.Rets1,m.Rets2,m.Rets3,m.Rets4,m.Rets5,m.Rets6,m.Rets7,m.Rets8,m.TotRets,;
           m.Retd1,m.Retd2,m.Retd3,m.Retd4,m.Retd5,m.Retd6,m.Retd7,m.Retd8,m.TotRetd,;
           m.Pcsi1,m.Pcsi2,m.Pcsi3,m.Pcsi4,m.Pcsi5,m.Pcsi6,m.Pcsi7,m.Pcsi8,m.TotPcsi,;
           m.PcsO1,m.PcsO2,m.PcsO3,m.PcsO4,m.PcsO5,m.PcsO6,m.PcsO7,m.PcsO8,m.TotPcso,;
           m.Sol1,m.Sol2,m.Sol3,m.Sol4,m.Sol5,m.Sol6,m.Sol7,m.Sol8,m.TotSol,;
           m.Rcst1,m.Rcst2,m.Rcst3,m.Rcst4,m.Rcst5,m.Rcst6,m.Rcst7,m.Rcst8,m.TotRcst,m.price,;
           m.AmounT,m.nSprice,lnAmount,m.lnpRice,m.RAMOUNT
RETURN
*--end of function lfZeroVal                               

FUNCTION lfSrLoc   
PARAMETERS lcParm
SELECT WareHous
LOCATE


*!*************************************************************
*! Name      : lfvDate
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : The validation the date range
*!*************************************************************
*! Example     : = lfvDate()
*!*************************************************************
FUNCTION lfvDate
PARAMETERS lcType
PRIVATE ldFrom,ldTo
STORE {} TO ldFrom,ldTo
IF !EMPTY(ldRpTDt) 
  ldFrom = ldRpFDt
  ldTo   = ldRpTDt
ENDIF
lcTitle = 'Dates'
DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
*!*************************************************************
*! Name      : lfvpbOk
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 04/16/2000 
*! Purpose   : The validation the date range
*!*************************************************************
*! Example   : = lfvpbOk()
*!*************************************************************
FUNCTION lfvpbOk
IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
 ldRpFDt = ldFrom
 ldRpTDt = ldTo
 CLEAR READ
ENDIF
*!****************************************
*****************************************************
*! Name      : lfwRepWhen
*! Developer : WAB - Walid A. Wahab
*! Date      : 05/08/2001
*! Purpose   : 
*!*************************************************************
*! Called from :OG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfwRepWhen
ldRpFDt=DATE()  
ldRpTDt=DATE()
RETURN


FUNCTION lfGetDisc
PARAMETERS lcStyPar,lcStorePar
PRIVATE lcOldArea,lcDiscCode
STORE '' TO lcDiscCode
lcOldArea=SELECT()
SELECT STYDYE
IF !SEEK(lcStyPar+lcStorePar)
  SELECT(lcOldArea)
  RETURN lcDiscCode
ENDIF
lcDiscCode=STYDYE.cdisccode
SELECT(lcOldArea)
RETURN lcDiscCode


 








