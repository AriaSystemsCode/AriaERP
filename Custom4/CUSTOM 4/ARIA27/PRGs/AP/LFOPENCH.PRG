*!*******************************************************************
*!
*! Created By : Malak Hanna Aziz
*!
*!*******************************************************************
*E300921,1 TAK 07/15/98 Added to not Check the APSETUP 
*E300921,1      if AP module no installed
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
IF USED('lcLinkChar')  && Check if chart of account file is openned.
  llReturnVal = .F.
ELSE
  *E300921,1 Put if AP installed condition.
  *IF APSETUP.cApSglLink = 'Y'
  IF (OCCURS('AP',gcCmpModules)<>0) AND APSETUP.cApSglLink = 'Y'
  *E300921,1 end.
    IF gcComp_Lvl = 'P'
      *** Open chart of account file ***
      SELECT 0
      USE &gcDataDir.GLACCHAR AGAIN ALIAS lcLinkChar
      SET ORDER TO ACCTCODE
      llReturnVal = .T.
    ELSE
      *** Get a path of chart of account file. ***
      *E301098,1 Hesham (Start)
      *lcDataDir = ALLTRIM(LOOKUP(SYCCOMP.CCOM_DDIR,APSETUP.cApSgLCom,SYCCOMP.CCOMP_ID,"CCOMP_ID"))
      lcDataDir = gfGetDataDir(ALLTRIM(LOOKUP(SYCCOMP.CCOM_DDIR,APSETUP.cApSgLCom,SYCCOMP.CCOMP_ID,"CCOMP_ID")))
      *E301098,1 Hesham (End)
      ** Check if the file is icluded in the directory or not.
      IF FILE(lcDataDir+'GLACCHAR.DBF')
        SELECT 0  
        USE &lcDataDir.GLACCHAR AGAIN ALIAS lcLinkChar
        SET ORDER TO ACCTCODE
        llReturnVal = .T.
      ELSE
        llReturnVal = .F.
      ENDIF
    ENDIF
  ELSE
    llReturnVal = .F.
 ENDIF  
ENDIF 

RETURN llReturnVal 