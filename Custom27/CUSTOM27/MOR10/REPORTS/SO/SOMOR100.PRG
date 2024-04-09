*:**************************************************************************
*: Program file  : SOMOR100.PRG (Converted from 26 TO 27 FOR MORELLE)
*: Program desc. : ROYALTIES REPORT
*! Developer     : Adel Mohammed El Gazzar (ADEL)
*! Date          : 09/21/2000
*:         System: ARIA APPAREL FOR   M O R E L L E
*:       Refer to: (C101970)
*:**************************************************************************

*----Report variables.
*-- lcStyMajor  && Major Title.
*-- lcRpMakBuy  && Domestic/Imported.
*-- lnColorLen  && Color length.
*-- llRpExRoyl  && Exclude royalties.
*-- llRpPayRoy  && Pay royalties.
*-- llRpExChrg  && Exclude charge backs.
*-- llRpDedRet  && Deduct returns.
*-- lnComPrcnt  && Commision percentage.
*-- lcAddCrdt   && Add credits.
*-- ldRpRetFrm  && Date range From.
*-- ldRpRetFrm  && Date range To.

*=========================Prepare filter=========================*
*--Add Make of Pucr.
lcRpExp  = lcRpExp + ' AND '+IIF(lcRpMakBuy = 'B','.T.',IIF(lcRpMakBuy = 'D','STYLE.MAKE','!STYLE.MAKE'))
lcRpExp  = lcRpExp + ' AND (TOTORD<>0 OR TOTWIP<> 0 OR TOTSTK <>0)' 
*--Cut the customer account code from the filter as the filter will be on Style file.
*-The customer filter is optimizied so it begins with 'CUSTOMER.TYPE'.
lnActPos = AT("CUSTOMER.TYPE",lcRpExp)
llSelAct = lnActPos <>0
IF lnActPos > 0
  lnActPosA = ASCAN(laOGFxFlt,"CUSTOMER.ACCOUNT")
  lnActPosA = ASUBSCRIPT(laOGFxFlt,lnActPosA,1)
  lcAcctTmp = STRTRAN(laOGFxFlt[lnActPosA,6],'|',',')
  *--Take the length of 'INLIST(' into consederation.
  llBetween = SUBSTR(lcRpExp,lnActPos-2,1) = 'N'
  lnActPos  = lnActPos -IIF(llBetween,8,7) 
  lnExpEnd  = AT(')',SUBSTR(lcRpExp,lnActPos))
  lnExpEnd1 = AT(')',SUBSTR(lcRpExp,lnExpEnd+1))
  lcExlExp  = SUBSTR(lcRpExp,lnActPos,lnExpEnd+IIF(llBetween,lnExpEnd1,0))
  lcRpExp   = STRTRAN(lcRpExp,lcExlExp,'.T. ')
ELSE
  lcAcctTmp = SPACE(0)
ENDIF
SELECt STYLE
*--Clear filter as it may be filtered from a previous run.
SET FILTER TO
LOCATE FOR &lcRpExp
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
IF ldRpRetFrm = {}  AND ldRpRetTo = {}
  *-- Message : Date range must be entered!
  =gfModalGen('TRM32089B40011','ALERT')
  RETURN
ENDIF

*========B e g i n  P r i n t i n g============*
lnChargeBk = 0             && CHARGE BACK AMOUNT
lnRepAmnt  = 0             && SALES REP COMMISSION AMOUNT
lnRetAmnt  = 0             && RETURN MERCHANDISE AMOUNT
lnRetQty   = 0             && RETURN MERCHANDISE QUANTITY
lnShipQty  = 0             && SHIPPED QAUNTITY
lnShipAmnt = 0             && SHIPPED AMOUNT
lnDiscount = 0
**** CALCULATE THE SHIPPED AND RETURNS AMOUNTS ****
SELECT STYLE
GO TOP
SCAN REST WHILE INKEY() <> 32 FOR &lcRpExp
  lcStyle = Style
  WAIT WINDOW "Computing royalties for Style-Color : " +ALLTRIM(STYLE) NOWAIT
  IF SEEK(lcStyle,'INVLINE')
    SELECT INVLINE
    SCAN WHILE style+invoice+STR(lineno,6) = lcStyle AND INKEY() <> 32;
         FOR IIF(llSelAct,!SEEK(InvHdr.ACCOUNT,(lcAcctTmp)),.T.)
      IF (llRpPayRoy AND SEEK(InvHdr.Account+InvHdr.Invoice,'ARHIST') AND ;
        BETWEEN(ARHIST.HISTDATE,ldRpRetFrm,ldRpRetTo));
        OR (!llRpPayRoy AND BETWEEN(ARHIST.HISTDATE,ldRpRetFrm,ldRpRetTo))
          ***** COMPUTE THE REP COMMISSION FOR THIS LINE ***
          XTSHIPAMT=PRICE*TOTQTY
          *-- Get Trade Discount If there is Any.
          IF !EMPTY(InvHdr.cTermCode)
            *-- Get the trade discount for the current term.
            DECLARE laTrmRltFd[1,2]
            *laTrmRltFd[1,1] = 'NTERDISCR'
            laTrmRltFd[1,1] = 'TRDE_DISC'
            laTrmRltFd[1,2] = 'lnDiscount'
            =gfRltFld(InvHdr.cTermCode , @laTrmRltFd , "CTERMCODE")
          ELSE
            lnDiscount = 0
          ENDIF
          SELE INVLINE
          XCOMM1=INVHDR.COMM1
          XCOMM2=INVHDR.COMM2
          DO CASE
            CASE INVHDR.DISCPCNT =0  .AND. lnDiscount = 0
              XNETSHIP   = XTSHIPAMT
            CASE INVHDR.DISCPCNT <>0 .AND. lnDiscount =0
              XNETSHIP   = XTSHIPAMT-(XTSHIPAMT*INVHDR.DISCPCNT/100)
            CASE INVHDR.DISCPCNT = 0  .AND. lnDiscount<>0
              XNETSHIP   = XTSHIPAMT-(XTSHIPAMT*lnDiscount/100)
            CASE INVHDR.DISCPCNT <>0 .AND. lnDiscount <>0
              XNETSHIP   = (XTSHIPAMT-(XTSHIPAMT*INVHDR.DISCPCNT/100)) * ((100-lnDiscount)/100)
          ENDCASE
          *-- Exclude royalties? (Yes).
          IF llRpExRoyl
            lnRepAmnt=lnRepAmnt + ((XCOMM1/100) * XNETSHIP)
            lnRepAmnt=lnRepAmnt + ((XCOMM2/100) * XNETSHIP)
          ENDIF
          lnShipQty=lnShipQty + (INVLINE.TOTQTY)
          lnShipAmnt=lnShipAmnt + XNETSHIP
          SELE INVLINE
        ENDIF  
      ENDSCAN
  ENDIF
  *-- Deduct returns? (Yes)
  IF llRpDedRet
    WAIT WINDOW 'Deducting returns from royalties. Press <SPACE BAR> to ABORT .......' NOWAIT
    SELE RETLINE
    GOTO TOP
    XAMT=0
    XQTY=0
    IF SEEK( lcStyle )
      SUM AMOUNT,TOTQTY TO XAMT,XQTY WHILE (style+crmemo) = (lcStyle) ;
          FOR (BETWEEN(RETHDR.CRDATE,ldRpRetFrm,ldRpRetTo)) AND IIF(USED(lcAcctTmp),!SEEK(ACCOUNT,(lcAcctTmp)),.T.)
      lnRetAmnt=lnRetAmnt + XAMT
      lnRetQty=lnRetQty + XQTY
    ENDIF
  ENDIF
  SELE STYLE
ENDSCAN
**** CALCULATE THE CHARGE BACK AMOUNTS *****
*-- Exclude charge backs? (Yes).
IF llRpExChrg
  WAIT WINDOW 'Deducting charge backs from royalties. Press <SPACE BAR> to ABORT..' NOWA
  SELE DEBIT
  GOTO TOP
  SUM AMOUNT TO lnChargeBk FOR TRANTYPE='3' AND ;
     (BETWEEN(TRANDATE,ldRpRetFrm,ldRpRetTo)) AND IIF(USED(lcAcctTmp),!SEEK(ACCOUNT,(lcAcctTmp)),.T.)
  SELE ARHIST
  GOTO TOP
  SUM AMOUNT TO lnPaidChrB FOR TRANTYPE='3' AND ;
     (BETWEEN(TRANDATE,ldRpRetFrm,ldRpRetTo)) AND IIF(USED(lcAcctTmp),!SEEK(ACCOUNT,(lcAcctTmp)),.T.)  
     lnChargeBk = lnChargeBk + lnPaidChrB
ENDIF
WAIT WINDOW 'Deducting credits from royalties. Press <SPACE BAR> to ABORT..' NOWAI
lnCrdtAmnt = 0
lnPdCrdt=0
lnUpCrdt=0
*-- If Paid credits or both
IF INLIST(lcAddCrdt ,'P','B')
  SELECT ARHIST
  GOTO TOP
  SUM AMOUNT TO lnPdCrdt FOR INLIST(TRANTYPE,'5','6','7') AND ;
      (BETWEEN(TRANDATE,ldRpRetFrm,ldRpRetTo)) AND IIF(USED(lcAcctTmp),!SEEK(ACCOUNT,(lcAcctTmp)),.T.)  
ENDIF
*-- If Unpaid credits or both
IF INLIST(lcAddCrdt ,'U','B')
  SELECT CREDIT
  GOTO TOP
  SUM AMOUNT TO lnUpCrdt FOR INLIST(TRANTYPE,'5','6') AND ;
    (BETWEEN(TRANDATE,ldRpRetFrm,ldRpRetTo)) AND IIF(USED(lcAcctTmp),!SEEK(ACCOUNT,(lcAcctTmp)),.T.)  
ENDIF                                            
lnCrdtAmnt =  lnPdCrdt +  lnUpCrdt                                      

**** NOW COMPUTE THE GROSS SALES AMT AND ROYALTIES ****
lnGrosSales = lnShipAmnt - lnRepAmnt - lnChargeBk - lnRetAmnt-ABS(lnCrdtAmnt)
lnGrossQty  = lnShipQty - lnRetQty
lnRoyalAmnt = (lnGrosSales*(lnComPrcnt/100))

**** NOW PRINT THE REPORT *****

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
* .......SALES...... COMMISSIONS  CHG. BACK  ....RETURNS....  ....ROYALITIES....
* 9999999 9999999.99   999999.99  999999.99  99999 999999.99  99.99%  9999999.99
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
* ...GROSS SALES....  ....ROYALITIES....
* 9999999 9999999.99  99.99%  9999999.99
*
R_WIDTH = 'W'
R_TITLE = 'Royalties report'
XTITLE  = 'FROM:'+DTOC(ldRpRetFrm)+' THRU:'+DTOC(ldRpRetTo)
WAIT WINDOW 'Report printing, press <SPACE BAR>  to abort ..' NOWAI
ROW    = 99
PAGENO = 0
SET DEVICE TO PRINT
PAGENO = PAGENO+1
DO RPT_HDR WITH 'SOMOR100',XTITLE,R_WIDTH
@ 05,00 SAY ' .......SALES...... COMMISSIONS  CHG. BACK  ....RETURNS....  .....ROYALTIES....'
@ 06,00 SAY REPLICATE('=',132)
ROW = 07

@ ROW,01 SAY lnShipQty PICTURE '9999999'
@ ROW,09 SAY lnShipAmnt PICTURE '9999999.99'
@ ROW,22 SAY lnRepAmnt PICTURE '99999.99'
@ ROW,33 SAY lnChargeBk PICTURE '999999.99'
@ ROW,44 SAY lnRetQty PICTURE '99999'
@ ROW,50 SAY lnRetAmnt PICTURE '999999.99'
@ ROW,61 SAY lnComPrcnt PICTURE '99.99%'
@ ROW,69 SAY lnRoyalAmnt PICTURE '9999999.99'


***** NOW PRINT THE SECOND FORMAT *****
PAGENO = 0
SET DEVICE TO PRINT

PAGENO = PAGENO+1
DO RPT_HDR WITH 'SOMOR100',XTITLE,R_WIDTH
@ 05,00 SAY ' ...GROSS SALES....  .....ROYALTIES....'
@ 06,00 SAY REPLICATE('=',132)
ROW = 07

@ ROW,01 SAY (lnShipQty-lnRetQty)                    PICTURE '9999999'
@ ROW,09 SAY lnGrosSales PICTURE '9999999.99'
@ ROW,21 SAY lnComPrcnt PICTURE '99.99%'
@ ROW,29 SAY lnRoyalAmnt PICTURE '9999999.99'
SET DEVICE TO SCREEN
DO ENDREPORT
RETURN

*****************************************************************************
* PROG: ACCNT_CHK
* DESC: FUNCTION TO VALIDATE ACCOUNT. CHECKS THE CUSTOMER FILE TO SEE IF 
*       AN ACCOUNT EXISTS IF NOT, IT RUNS CUSBROWN
*****************************************************************************
FUNCTION ACCNT_CHK               
PARAM XNUM

IF !USED('CUSTOMER')
  SELECT 0
  DO NETUSE WITH '&QDD.CUSTOMER','&QDD.CUSTOMER','SH'
ENDIF

IF !EMPTY(XACCNT&XNUM)
  SELECT CUSTOMER
  IF !SEEK( 'M'+XACCNT&XNUM )
    SAVE SCREEN TO XACC_1
    DO CUSBROWM WITH XACCNT&XNUM
    RESTORE SCREEN FROM XACC_1
  ENDIF
ENDIF
RETURN (.T.)
*****************************************************************************

*****************************************************************************
* PROG: XEXCL()
* DESC: DOES A SMALL FOR..ENDFOR LOOP TO SEE IF THE CURRENT 
*       ACCOUNT NUMBER IS THE SAME AS THE EXCLUDED ACCOUNT NUMBERS
*       IF THEY MATCH, IT RETURNS A FALSE
*       IF THEY DO NOT MATCH, IT RETURNS A TRUE
*****************************************************************************
FUNCTION XEXCL
FOR X = 1 TO 5
  Y = STR(X,1)
  IF XACCNT&Y = ACCOUNT
    RETURN (.F.)
  ENDIF
ENDFOR
RETURN (.T.)
*****************************************************************************
*!**************************************************************************
*! Name      : lfsrvSty
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/21/2000
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
    SELECT STYLE
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/21/2000
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
*-- end of lfStySum.

****************************************************************************
* FUNC: lfwRepWhen
* DESC: To validate OG When.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* Date: 09/21/2000
****************************************************************************
FUNCTION lfwRepWhen

*--Open files
=gfOpenFile(gcDataDir+'CREDIT',gcDataDir+'CREDIT','SH')
=gfOpenFile(gcDataDir+'InvHdr',gcDataDir+'InvHdr','SH')
=gfOpenFile(gcDataDir+'InvLine',gcDataDir+'InvLines','SH')
=gfOpenFile(gcDataDir+'Debit',gcDataDir+'Debit','SH')
=gfOpenFile(gcDataDir+'RetHdr',gcDataDir+'RetHdr','SH')
=gfOpenFile(gcDataDir+'ARHIST',gcDataDir+'ARHISTT','SH')
=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
=gfOpenFile(gcDataDir+'RETLINE',gcDataDir+'RETLINES','SH')
*--Set needed relations
SET RELATION TO CRMEMO INTO RETHDR

SELECT INVLINE
SET RELATION TO
SET RELATION TO INVOICE INTO INVHDR

*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/21/2000
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR STYLE.Fabric = Fabric.Fabric
    llHaveSty = FOUND()
    *-- If no styles found for this fabric
    IF !llHaveSty
      *-- the following message is
      *-- No styles in fabric group XXX .
      *--           <Ok>
      = gfModalGen("TRM32055B36000","Dialog",Fabric.Fabric)
    ENDIF
    SELECT (lcAlias)
    RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE
*-- end of lfSRVFab.
*!*************************************************************
*! Name      : lfFabSum
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/21/2000
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
IF RECCOUNT() != 0
  lnFabRec = RECNO('FABRIC')

  SELECT Fabric_X
  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
  SELECT Fabric
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfFabSum.
*!*************************************************************
*! Name      : lfEvalSegs
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/21/2000
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
FUNCTION lfEvalSegs

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcNonMajTl = ''
lcNonMajPi = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- end of lfEvalSegs.

*!*************************************************************
*! Name      : lfvDateRng
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/21/2000
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.spr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************
FUNCTION lfvDateRng

PRIVATE ldFrom,ldTo

IF EMPTY(ldRpRetFrm) AND EMPTY(ldRpRetTo)
   STORE {} TO ldFrom,ldTo
ELSE
  ldFrom = ldRpRetFrm
  ldTo   = ldRpRetTo
ENDIF   

lcTitle = 'Sales order comp. date'
DO DateRng.SPR   && Run the advance payment screen 

ldRpRetFrm = ldFrom
ldRpRetTo  = ldTo

*!*************************************************************
*! Name      : lfvpbOk
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/21/2000
*! Purpose   : Validate OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbOk()
*!*************************************************************

FUNCTION lfvpbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  CLEAR READ
ENDIF  

