*:****************************************************************
*: Program file  : ARPINVMT.PRG
*: Program desc. : Invoice Form [MT] (For Momentrends Invoice)
*: System        : Aria Apparel System (A27).
*: Module        : Accounts Recevible (AR)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#102383,1
*: Date          : 06/19/2001
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : 
*:               -----------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*:Modifications :
*:****************************************************************
*:B605318,1 ABD 02/18/2002 Fix bug of not printing more than 20 pages.
*:B605421,1 ABD 02/24/2002 Fix bug of printing currency sign (All modifications in FRX). 
*:B608172,1 NNA 23/07/2007 Fix bug of error when printing the division logo if the logo picture not found in the folder (T20070130.0010) 
*:****************************************************************

*-- Declaration variables.

IF FILE('&gcDataDir.VATID.TXT')
  lnTextHand = FOPEN('&gcDataDir.VATID.TXT',0)
  lcVatId    = FGETS(lnTextHand)
  =FCLOSE(lnTextHand)
ENDIF
IF llOGFltCh 
  =lfCratBmp()
ENDIF	
*-- End Declaration variables.


SELECT INVHDR


******************************

FUNCTION lfCratBmp


CREATE CURSOR (lcBmpFile) ( InvoiceNo C(6) , cDivision C(6) , Logo G)
INDEX ON cDivision TAG Division OF (gcWorkDir+lcBmpFile)

lnPos = ASCAN(laOgVrFlt,'INVHDR.INVOICE')
lnPos = ASUBSCRIPT(laOgVrFlt,lnPos,1)

*B605318,1 Check if the file exist. [Begin]
*IF RECCOUNT(laOgVrFlt[lnPos,6]) > 0
IF USED(laOgVrFlt[lnPos,6]) AND RECCOUNT(laOgVrFlt[lnPos,6]) > 0
*B605318,1 Check if the file exist. [End]

  SELECT (laOgVrFlt[lnPos,6])
  SET RELATION TO INVOICE INTO INVHDR ADDTIVE
  SCAN
    SELECT (lcBmpFile)

    *B608172,1 NNA 23/07/2007 (Begin) Check if Division's Picture found or not in the folder
    *IF !SEEK(InvHdr.cDivision) 
    IF !SEEK(InvHdr.cDivision) AND FILE((gcDef_Path+'LoGo\'+ALLTRIM(InvHdr.cDivision)) +'.BMP')
    *B608172,1 NNA (End)
    
      APPEND BLANK
      REPLACE InvoiceNo WITH INVHDR.INVOICE  ,;
              cDivision WITH InvHdr.cDivision
      APPEND GENERAL Logo FROM (gcDef_Path+'LoGo\'+ALLTRIM(InvHdr.cDivision)) +'.BMP'
    ENDIF
  ENDSCAN
  
  *B605318,1 Break the relation. [Begin]
  SET RELATION TO
  *B605318,1 Break the relation. [End]
ENDIF

RETURN ''

******************************
FUNCTION lfRunBmp
PARAMETERS LCRETURN
LCRETURN = .T.

IF !SEEK(INVHDR.CDIVISION,lcBmpFile)
  *B605318,1 SSE Commented out. [Begin]
  *GOTO 1 IN (lcBmpFile)
  *B605318,1 SSE Commented out. [End]
ENDIF

RETURN LCRETURN