*:***************************************************************************
*: Program file       : ALPBOLIK.PRG
*: Program description: Custom Bill of Lading Report for IKK00
*: Module             : Sales Order Allocation (AL)
*: Developer          : Abdelrahman Essam (AEG)
*: Tracking Job Number: C201862.EXE
*: Date               : 09/4/2016
*:***************************************************************************
*Modifications:
*!*
*:***************************************************************************

LOCAL lcOldAlias
lcOldAlias = SELECT(0)
set step on
=lfCrtTmpe()
=lfCollDt()
IF USED('TMPSHIP')
  USE IN TMPSHIP
ENDIF
SELECT (lcOldAlias)

*!*************************************************************
*! Name      : lfShipTo
*! Developer : Abdelrahman Essam
*! Date      : 09/4/2016
*! Purpose   : Function to get Ship To info
*!*************************************************************
FUNCTION lfShipTo

PRIVATE laShipTo
DECLARE laShipTo[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laShipTo[1,1] = 'lcRpShp1'        && 1st. line Variable
laShipTo[2,1] = 'lcRpShp2'        && 2nd. line Variable
laShipTo[3,1] = 'lcRpShp3'        && 2nd. line Variable

laShipTo[1,2] = 30                && Line length

*IF EMPTY(lcRpShp1) .AND. EMPTY(lcRpShp1) .AND. EMPTY(lcRpShp1)
  =gfOptMsg('laShipTo')
*ENDIF
SET MEMOWIDTH TO 30              && the length of the memo field.




*!*************************************************************
*! Name      : lfCrtTmpe
*: Developer : Abdelrahman Essam (AEG)
*: Date      : 09/4/2016
*! Purpose   : Procedure to create temp.file 
*!*************************************************************
FUNCTION lfCrtTmpe

 ALTER TABLE BolHdr ADD COLUMN CHL_BL1 C(6)
 ALTER TABLE BolHdr ADD COLUMN CHL_BL2 C(6)
 ALTER TABLE BolHdr ADD COLUMN CHL_BL3 C(6)
 ALTER TABLE BolHdr ADD COLUMN CHL_BL4 C(6)
 ALTER TABLE BolHdr ADD COLUMN CHL_BL5 C(6)

ENDFUNC


*!*************************************************************
*! Name      : lfCollDt
*: Developer : Abdelrahman Essam (AEG)
*: Date      : 04/4/2016
*! Purpose   : Procedure to collect child bol related to parent bol
*!*************************************************************
PROCEDURE lfCollDt



LOCAL llEmpRpShp

  SELECT BOLHDR
  GO top
  SCAN
  
   =SEEK(BOLHDR.BOL_NO,'BOL_HDR','BOL_HDR')
    Replace LMASTERBOL WITH BOL_HDR.LMASTERBOL,;
    cshipment WITH BOL_HDR.cshipment,;
    cproject WITH BOL_HDR.cproject,;
    tot_plt WITH BOL_HDR.tot_plt,;
    tot_pltwt WITH BOL_HDR.tot_pltwt,;
    tot_cubft WITH BOL_HDR.tot_cubft
  
      IF BOLHDR.LMASTERBOL
        SELECT BOL_NO FROM BOL_HDR WHERE cmasterbol=BOLHDR.BOL_NO INTO CURSOR lctempchld
        lnCnt =1
        SELECT lctempchld
        GO top
       SCAN
         IF lnCnt >5
          exit
         ENDIF
         lnNum=ALLTRIM(STR(lnCnt))
         SELECT BOLHDR
          REPLACE CHL_BL&lnNum with lctempchld.BOL_NO
         lnCnt= lnCnt+1
       endscan

        APPEND BLANK IN BolLin
        replace BOL_NO  WITH BOLHDR.BOL_NO IN BolLin
       ENDIF
  ENDSCAN




ENDPROC


