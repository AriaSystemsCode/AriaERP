*:**************************************************************************
*! Name : ICDAN45.PRG    C101778   Done for (Dana Undies)
*: Program desc. : Custom conversion printing .
*:         System: Aria Apparel system
*:      Developer: RAMY MABROUK
*:           Date: 03/07/2000
*:**************************************************************************
*: Calls :
*:**************************************************************************
*! MODIFICATIONS:
*:**************************************************************************

SELECT DANACONV
SET RELATION TO 'N' + 'CSTYGROUP ' + DANACONV.cStyGroup INTO CODES
*lcRepNam = 'ICDAN45.FRX'


DO gfDispRe WITH EVALUATE('lcRpForm') , 'FOR ' + lcRpExp

SELECT DANACONV
SET RELATION TO