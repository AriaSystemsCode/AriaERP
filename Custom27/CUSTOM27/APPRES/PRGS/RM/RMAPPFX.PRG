*!*****************************************************************
*! Name      : RMAPPFX
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 10/30/02
*! Purpose   : RA Fix program for App10
*!*************************************************************
*! Refer to  : B606570
*!*************************************************************

*--Initiliaze a Counter variable for the RAs being updated
lnModfdRa  = 0

*--Open needed files
=gfOpenFile(gcDataDir+'RetAuth',gcDataDir+'RetAuth','SH')
=gfOpenFile(gcDataDir+'raline',gcDataDir+'raline','SH')
*--Set needed relations
SELECT RetAuth
SET RELATION TO RetAuth.rano INTO Raline ADDITIVE
SCAN
  WAIT WINDOW 'Fixing RA# ' + RANO NOWAIT
  *--Calculate the budget quantities from the Raline file.
  STORE 0 TO  lnlineBud,lnlineOpn,lnOpnQty
  lnHdrBud = nReta_bud
  llChgOpn = .F.
  SELE Raline  
  SCAN REST WHILE rano+style+cra_linno = retauth.RANO
    lnlineBud   = lnlineBud  +  Raline.totqty
    lnOpnQty    = lnOpnQty   +  Raline.ntotopnqty
    *--If the RA is either Complete or Cancelled, clear the Open qtys  
    IF INLIST(RetAuth.STATUS,'C','X') AND ntotopnqty <> 0
      REPLACE nopnqty1 WITH 0, nopnqty2 WITH 0, nopnqty3 WITH 0,nopnqty4 WITH 0,;
              nopnqty5 WITH 0, nopnqty6 WITH 0, nopnqty7 WITH 0,nopnqty8 WITH 0,ntotopnqty WITH 0
      llChgOpn = .T.
    ENDIF
    IF RetAuth.STATUS = 'O'
      FOR lnFrom = 1 TO 8
        lcFrom = STR(lnFrom,1)
        REPLACE nopnqty&lcFrom WITH MIN(nopnqty&lcFrom,QTY&lcFrom)
      ENDFOR
      REPLACE ntotopnqty WITH nopnqty1+nopnqty2+nopnqty3+nopnqty4+nopnqty5+nopnqty6+nopnqty7+nopnqty8
    ENDIF  
  ENDSCAN
  SELECT RetAuth
  *---------------------------------------------------------------------------------------------
  *---( Calculations are done in the following sequence:)
  *--(1) Unless the RA is cancelled, the Canceled + Damaged qtys will be added to the 
  *      lines' budget to make the new header budget.
  *--(2) Buget = Open + Received + Cancelled + Damaged.
  *      In case of Any difference:
  *      A)Case Buget>(Open+Received+Cancelled+Damaged):the difference is added to Cancelled qty.  
  *      B)Otherwise: Nothing is added to Cancelled as we may receive more than budget.
  *--(3) Lastly, the Final new Budget will be recalculated after modified Cancelled.
  *---------------------------------------------------------------------------------------------

  *--(1)
  REPLACE nReta_bud  WITH lnlineBud+IIF(STATUS<>'X',nReta_can,0)
  *--(2)
  IF RetAuth.STATUS = 'O' AND nReta_bud < lnOpnQty
    REPLACE nReta_opn WITH nReta_bud
    llChgOpn = .T.
  ENDIF
  IF (nReta_bud <> nReta_rec+nReta_can+nReta_opn+nReta_dam ;
                              AND nReta_bud > nReta_rec+nReta_can+nReta_opn+nReta_dam)
    REPLACE nReta_can  WITH nReta_bud - nReta_rec+nReta_can+nReta_opn+nReta_dam
    *--(3)
    REPLACE nReta_bud  WITH nReta_rec+nReta_can+nReta_opn+nReta_dam
  ENDIF
  lnModfdRa  = lnModfdRa + IIF((nReta_bud <> lnHdrBud) OR llChgOpn,1,0)
ENDSCAN

WAIT WINDOW STR(lnModfdRa) + " Return Auhorizations have been updated." NOWAIT

