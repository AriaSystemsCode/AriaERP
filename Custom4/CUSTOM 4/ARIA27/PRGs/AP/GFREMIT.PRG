*!**************************************************************************
*!
*!      Function: lfRemit
*!
*!**************************************************************************
*
*E301148,1 AMM 02/28/99 Enhance opening and closing files
****************************************************************************
FUNCTION gfRemit
PARAMETERS lcRmtToCode, llActPopup, lcVendCode, lcFactCode, lcRemitStat,;
           lcRemitCmp, lcRemitAd1, lcRemitAd2, lcRemitAd3,lcRemitAd4,lcRemitAd5, lnRow, lnCol,;
           lcArrName, lcDispStr, lnPopWdth
*** lcRmtToCode : normally ('V', 'F', 'O')
*** llActPopup  : .T. if the popup is to be activated
*** lcVendCode  : vendor code
*** lcFactCode  : factor code
*** lcRemitStat : remit objects display status
*** lcRemitCmp  : remit company object name
*** lcRemitAd1  : remit address1 object name
*** lcRemitAd2  : remit address2 object name
*** lcRemitAd3  : remit address(3+4+5) object name
*** lnRow       : DOS popup top left row
*** lnCol       : DOS popup top left column
*** lcArrName   : remit to array name
*** lcDispStr   : display string of the popup
*** lnPopWdth   : dos popup width
*** example call from APPYINV.PRG
*** laData[5]    = lfRemit(laData[5], !llJustShow, laData[1], laData[36],;
***              @lcRemitStat, 'laData[6]', 'laData[7]', 'laData[8]', 'laData[9]',;
***              6, 39, 'laRemitTo', @lcRemitTo, lnRemitLen)
PRIVATE lcRemitFil, lcRemitTag, lcKeyCode

IF llActPopup
  lcRmtToCode = lfvPopups(lcArrName, @lcDispStr,;
                       lnRow, lnCol, lnPopWdth)
ELSE
  lcRmtToCode = UPPER(ALLTRIM(lcRmtToCode)) 
ENDIF                           

lcKeyCode = IIF(lcRmtToCode = 'V', lcVendCode, lcFactCode)
*E301148,1 AMM Initialize
STORE .F. TO llOpVen, llOpFact
*E301148,1 AMM end

IF lcRmtToCode = 'V'
  lcRemitFil = 'APVENDOR'
  lcRemitTag = 'VENCODE'
  *E301148,1 AMM Open File
  llOpVen    = gfOpenFile(gcDataDir+'APVENDOR','VENCODE','SH')
  *E301148,1 AMM end
ELSE
  lcRemitFil = 'SYCFACT'
  lcRemitTag = 'CFACCODE'
  *E301148,1 AMM Open file
  llOpFact   = gfOpenFile(gcSysHome+'SYCFACT','CFACCODE','SH')
  *E301148,1 AMM end
ENDIF  

DO CASE
  CASE lcRmtToCode $ 'VF'
    IF SEEK(lcKeyCode, lcRemitFil) 
      &lcRemitCmp  = PADR(IIF(lcRmtToCode = 'V',;
                       APVENDOR.cVenComp, SYCFACT.cFacComp), 40)
      &lcRemitAd1 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,1)
      &lcRemitAd2 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,2)
      &lcRemitAd3 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,3)
      &lcRemitAd4 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,4)      
      &lcRemitAd5 = gfGetAdr(lcRemitFil, lcRemitTag,.F.,.F.,5)      
    ELSE
      STORE SPACE(40) TO &lcRemitCmp, &lcRemitAd1,;
                          (lcRemitAd2), (lcRemitAd3),;
                          (lcRemitAd4), (lcRemitAd5)              
    ENDIF
    lcRemitStat= 'DISABLE'    
  OTHERWISE
    lcRemitStat= IIF(laScrMode[2],'DISABLE','ENABLE')  && Mohamed Hassan 10/02/95
ENDCASE
*E301148,1 AMM Close files
IF USED('APVENDOR') .AND. llOpVen
  =gfCloseFile('APVENDOR')
ENDIF
IF USED('SYCFACT') .AND. llOpFact
  =gfCloseFile('SYCFACT')
ENDIF
*E301148,1 AMM end
SHOW GET (lcRemitCmp) &lcRemitStat
SHOW GET (lcRemitAd1) &lcRemitStat
SHOW GET (lcRemitAd2) &lcRemitStat
SHOW GET (lcRemitAd3) &lcRemitStat              
SHOW GET (lcRemitAd4) &lcRemitStat
SHOW GET (lcRemitAd5) &lcRemitStat              
RETURN lcRmtToCode


*!**************************************************************************
*!
*!      Function: lfvPopups
*!
*!**************************************************************************
*
FUNCTION lfvPopups
PARAMETERS lcArrName, lcDispFld, lnStRow, lnStCol, lnPopWidth
PRIVATE lcRetVal, lnPopLen
DO CASE
  CASE _DOS
    lnPopLen     = ALEN((lcArrName),1)
    lcRetVal     = gfActPop(lnStRow, lnStCol, ;
                            IIF(lnPopLen>5,lnStRow + 7,lnStRow + 2 + lnPopLen),;
                            lnStCol + 3 + lnPopWidth,;
                            lcArrName, 2, 1, @lcDispFld)
  CASE _WINDOWS
    lcRetVal    = IIF(EVALUATE(SYS(18)) > 0,;
                      &lcArrName[EVALUATE(SYS(18)),2]," ")
                      
ENDCASE
RETURN lcRetVal

*!**************************************************************************
*!
*!      Function: lfGetArrElem
*!
*!**************************************************************************
* scans an array for an array element
*
FUNCTION lfGetArrElem
PARAMETERS lcSrchCode,laSrchArr
PRIVATE lnArrElem
lnArrElem = 1
DO WHILE lcSrchCode <> &laSrchArr[lnArrElem, 2] .AND. ;
         lnArrElem < ALEN((laSrchArr),1)
  lnArrElem = lnArrElem + 1
ENDDO  
RETURN IIF(BETWEEN(lnArrElem, 1, ALEN((laSrchArr), 1)),lnArrElem, 0)

