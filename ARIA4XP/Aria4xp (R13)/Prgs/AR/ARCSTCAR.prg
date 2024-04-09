*E612644,1 MMT 11/28/2022 Adjust the customer screen to add Other Shipping from address options in Carriers accounts option grid[T20221002.0001][Start]
*!**************************************************************************
*! Name      : lfvAltStore
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/18/2022
*! Purpose   : For Carriers OG Account validation
*!**************************************************************************
FUNCTION lfvAltAccount()

PRIVATE lcObjVal
 * IF !(lcRpAcct == laOldVal)
PRIVATE lnAlsNo,lcCustOrd,lcObjName
lnAlsNo = SELECT(0)
IF !USED('CUSTOMER')
  =gfOpenTable('Customer')
ENDIF
SELECT CUSTOMER
lcCustOrd = ORDER()
gfSetOrder('CUSTOMER')
IF '?' $ lcRpAcct .OR. (!EMPTY(lcRpAcct) .AND. !GFSEEK('M' + lcRpAcct , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcRpAcct , '' , 'M')
  lcObjVal = IIF(llObjRet , lcRpAcct , laOldVal)
ENDIF    && End of IF

SELECT CUSTOMER
gfsetOrder(lcCustOrd)
SELECT(lnAlsNo)
*  ENDIF
*!**************************************************************************
*! Name      : lfvAltStore
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/18/2022
*! Purpose   : For Carriers OG Store validation
*!**************************************************************************
FUNCTION lfvAltStore
PRIVATE lcObjVal
 * IF !(lcRpAcct == laOldVal)
PRIVATE lnAlsNo,lcCustOrd,lcObjName
lnAlsNo = SELECT(0)
IF !USED('CUSTOMER')
  =gfOpenTable('Customer')
ENDIF
SELECT CUSTOMER
lcCustOrd = ORDER()
gfSetOrder('CUSTOMER')

IF '?' $ lcRpStore .OR. (!EMPTY(lcRpStore) AND !EMPTY(lcRpAcct) .AND. !GFSEEK('S' + PADR(lcRpAcct,5)+ lcRpStore, 'CUSTOMER'))
  XSTORE= lcRpStore 
  llObjRet = CUSBROWS(@lcRpAcct )
  lcObjVal = IIF(llObjRet ,XSTORE, laOldVal)
  lcRpStore = XSTORE
ENDIF    && End of IF

SELECT CUSTOMER
gfsetOrder(lcCustOrd)
SELECT(lnAlsNo)

*!**************************************************************************
*! Name      : lfwCusWhen
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/18/2022
*! Purpose   : For Carriers OG when function
*!**************************************************************************
FUNCTION lfwCusWhen
IF llFirst 
lcRPUPSAC = lcRPUPSAC1 
lcRPFEDAC = lcRPFEDAC1 
lcRPUSPAC = lcRPUSPAC1
lcRPUSSAC  =  lcRPUSSAC1 
lcRpAcct  =  lcRpAcct1  
lcRpShpFr  =  lcRpShpFr1 
lcRpStore  =  lcRpStore1 
lcRpShpAd =   lcRpShpAd1 
loogscroll.RefreshScroll()
IF llDisableOGCar
  loOGScroll.EnableObject("lcRPUPSAC",.F.)
  loOGScroll.EnableObject("lcRPFEDAC",.F.)
  loOGScroll.EnableObject("lcRPUSPAC",.F.)
  loOGScroll.EnableObject("lcRPUSSAC",.F.)
  loOGScroll.EnableObject("lcRpAcct",.F.)
  loOGScroll.EnableObject("lcRpShpFr",.F.)
  loOGScroll.EnableObject("lcRpStore",.F.)
  loOGScroll.EnableObject("lcRpShpAd",.F.)  
ENDIF
llFirst =.f.
ENDIF
*E612644,1 MMT 11/28/2022 Adjust the customer screen to add Other Shipping from address options in Carriers accounts option grid[T20221002.0001][End]