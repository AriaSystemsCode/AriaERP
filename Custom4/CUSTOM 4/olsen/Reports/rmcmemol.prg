****************************************************************************
* PROG : RMCMEMOL.PRG  (Converted from 27 to A4xp for OLSEN)
* DESC : CREDIT MEMO FORM ol
* DATE : 02/11/2008
* AUTH : Mohamed Shokry(MHM)
* Refer to : (
*****************************************************************************
*--- If no recorded match the filter
SELECT RETHDR
SET SKIP TO
LOCATE FOR &lcRpExp
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  SET DEVICE TO SCREEN
  llNoRec = .T.
  RETURN
ENDIF

llTax     = (gfGetMemvar('M_TAX') ='Y')
lcTaxDesc = gfGetMemvar('M_TAX_DESC')
llMultiWH = (gfGetMemvar('M_WareHouse') ='Y')
*-- Get the major and nonmajor titles and lengths.
lcMajTitle = ALLTRIM(gfItemMask('HM'))
lnMajorLen = LEN(ALLTRIM(gfItemMask('PM')))
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
llPrntClr = (gfModalGen('QRM46035B00006','DIALOG' ) = 1)

SELECT RETHDR
SET SKIP TO &lcSkipExpr

DO gfDispRe WITH gcRepHome+'\RMCMEMOL.FRX'

RETURN

