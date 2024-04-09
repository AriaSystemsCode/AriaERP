*:****************************************************************************************
*: Program file  : MULMAIN.PRG
*: Program desc. : Main Program.
*: System        : Aria Apparel System (A27).
*: Developer     : ALBERT RAIF - (ALB) Due to C#200444,1
*: Date          : 12/29/2002
*: Purpose       : Custom Process Program .
*:****************************************************************************************
*: Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*:             lcFunPars -> Process function parameters, sent as a string.
*:****************************************************************************************
*: Returns   : Logical value.       
*:****************************************************************************************
*C102777,1
*:****************************************************************************************
*:Modifications :
*:****************************************************************************************
*
PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'
*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*---102777,1
*!*************************************************************
*! Name      : lfvGenTxt
*! Developer : Albert Raif 
*! Date      : 12/26/2002
*! Purpose   : Function to Enable or Disable Generate TXT
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  lfvGenTxt
*!*************************************************************

FUNCTION lfvGenTxt
PARAMETER LCRETURN , LCPARM

laOGObjCnt[ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lnRpGetFil'),1)] = llRpGenTxt
=lfOGShowGet('lnRpGetFil')

*!*************************************************************
*! Name      : lfvGetFile
*! Developer : Albert Raif 
*! Date      : 12/26/2002
*! Purpose   : Function to get TXT file name
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  lfvGetFile
*!*************************************************************

FUNCTION lfvGetFile
PRIVATE lcVar1,lcOldFile,lcOldSafe
lcOldSafe = SET('SAFE')
lcOldFile = lcRpFilNam 
SET SAFE ON
lcRpFilNam = PUTFILE('',lcRpFilNam,'TXT')
lcRpFilNam = IIF(EMPTY(lcRpFilNam),lcOldFile,lcRpFilNam)
SET SAFE &lcOldSafe
SHOW GET lcRpFilNam
lcVar1 = lcRpFilNam
SAVE TO gcDataDir+'APCHK.MEM' ALL LIKE lcvar*

*!*************************************************************
*! Name      : lfGenTxt
*! Developer : Albert Raif 
*! Date      : 12/26/2002
*! Purpose   : Function to generate TXT file 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  lfGenTxt()
*!*************************************************************

FUNCTION lfGenTxt
PRIVATE lcTempTxt,lcPageNo,lcChkNo, lnChkAmnt,lnChkPay ,lnTotChks,lnChkCount,lcChkAcct
IF llRpGenTxt AND lfGetFlNa()
  lnFileNo = FCREATE(lcRpFilNam)
  lcTempFil = gfTempName()
  lcWhereCon = "cPayMeth $ 'PM' AND cPayType = 'P' " + lfGenExp()
  SELECT * FROM APPAYMNT WHERE &lcWhereCon  ORDER BY cBnkCode,cChkAcct,cPayDocNo ;
           INTO CURSOR &lcTempFil
  IF _TALLY > 0 
    SELECT (lcTempFil)
    GOTO TOP
    lcSortBy   = cBnkCode+cChkAcct
    lnChkAmnt  = 0
    lnChkCount = 0
    SCAN 
      IF lcSortBy   = cBnkCode+cChkAcct
        lnChkAmnt = lnChkAmnt + IIF(cPayStat='V',0,nPayAmnt)
        lcChkNo = PADL(ALLTRIM(cPayDocNo),10,'0')
        *khalid
        *lcTxtValue = IIF(cPayStat='V','V',' ') + SUBSTR(cChkAcct,1,10)+ SPACE(1) +STR(MONTH(dPayDate),2)+;
                     STR(DAY(dPayDate),2) + SUBSTR(STR(YEAR(dPayDate),4),3,2)+SPACE(1) +lcChkNo +;
                     SPACE(1) + PADL(ALLTRIM(STR(INT(nPayAmnt))) +SUBSTR(STR(nPayAmnt-INT(nPayAmnt),4,2),3,2),10,'0')+;
                     SPACE(1) + PADL(cPayClNo,15,' ')+SPACE(24)
        
        lcTxtValue = IIF(cPayStat='V','V',' ') + SUBSTR(cChkAcct,1,10)+ SPACE(1) +PADL(ALLTRIM(STR(MONTH(dPayDate),2)),2,'0')+;
                     PADL(ALLTRIM(STR(DAY(dPayDate),2)),2,'0') + SUBSTR(STR(YEAR(dPayDate),4),3,2)+SPACE(1) +lcChkNo +;
                     SPACE(1) + PADL(ALLTRIM(STR(INT(nPayAmnt))) +SUBSTR(STR(nPayAmnt-INT(nPayAmnt),4,2),3,2),10,'0')+;
                     SPACE(1) + PADR(ALLTRIM(cPayComp),15,' ')+SPACE(24)
        *khalid
        =FPUTS(lnFileNo ,lcTxtValue)
        lnChkCount = lnChkCount + IIF(cPayStat='V',0,1)
        SCATTER MEMVAR MEMO
      ELSE
        lcSortBy   = cBnkCode+cChkAcct
        
        lcTxtValue = 'T' + SUBSTR(m.cChkAcct,1,10)+ SPACE(1) +PADL(ALLTRIM(STR(MONTH(m.dPayDate),2)),2,'0')+;
                     PADL(ALLTRIM(STR(DAY(m.dPayDate),2)),2,'0') + SUBSTR(STR(YEAR(m.dPayDate),4),3,2)+SPACE(1) +STR(lnChkCount,10)+;
                     SPACE(1) + PADL(ALLTRIM(STR(INT(lnChkAmnt))) +SUBSTR(STR(lnChkAmnt-INT(lnChkAmnt),4,2),3,2),10,'0')+;
                     SPACE(40) 
        
        =FPUTS(lnFileNo ,lcTxtValue)
        lnChkAmnt =  IIF(cPayStat='V',0,nPayAmnt)
        lcChkNo = PADL(ALLTRIM(cPayDocNo),10,'0')
        lnChkCount = IIF(cPayStat='V',0,1)
        *khalid
        *lcTxtValue = IIF(cPayStat='V','V',' ') + SUBSTR(cChkAcct,1,10)+ SPACE(1) +STR(MONTH(dPayDate),2)+;
                     STR(DAY(dPayDate),2) + SUBSTR(STR(YEAR(dPayDate),4),3,2)+SPACE(1) +lcChkNo +;
                     SPACE(1) + PADL(ALLTRIM(STR(INT(nPayAmnt))) +SUBSTR(STR(nPayAmnt-INT(nPayAmnt),4,2),3,2),10,'0')+;
                     SPACE(1) + PADL(cPayClNo,15,' ')+SPACE(24)

        lcTxtValue = IIF(cPayStat='V','V',' ') + SUBSTR(cChkAcct,1,10)+ SPACE(1) +PADL(ALLTRIM(STR(MONTH(dPayDate),2)),2,'0')+;
                     PADL(ALLTRIM(STR(DAY(dPayDate),2)),2,'0') + SUBSTR(STR(YEAR(dPayDate),4),3,2)+SPACE(1) +lcChkNo +;
                     SPACE(1) + PADL(ALLTRIM(STR(INT(nPayAmnt))) +SUBSTR(STR(nPayAmnt-INT(nPayAmnt),4,2),3,2),10,'0')+;
                     SPACE(1) + PADR(cPayComp,15,' ')+SPACE(24)

        *khalid
        =FPUTS(lnFileNo ,lcTxtValue)
        SCATTER MEMVAR MEMO
      ENDIF
    ENDSCAN
    lcTxtValue = 'T' + SUBSTR(m.cChkAcct,1,10)+ SPACE(1) +PADL(ALLTRIM(STR(MONTH(m.dPayDate),2)),2,'0')+;
                 PADL(ALLTRIM(STR(DAY(m.dPayDate),2)),2,'0') + SUBSTR(STR(YEAR(m.dPayDate),4),3,2)+SPACE(1) +STR(lnChkCount,10)+;
                 SPACE(1) + PADL(ALLTRIM(STR(INT(lnChkAmnt))) +SUBSTR(STR(lnChkAmnt-INT(lnChkAmnt),4,2),3,2),10,'0')+;
                 SPACE(40) 

    =FWRITE(lnFileNo ,lcTxtValue)
    =FCLOSE(lnFileNo)
    USE IN (lcTempFil)
  ENDIF
ENDIF
SELECT APPAYMNT
*!*************************************************************
*! Name      : lfGetFlNa
*! Developer : Albert Raif 
*! Date      : 12/26/2002
*! Purpose   : Function to generate TXT file 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  lfGenTxt()
*!*************************************************************

FUNCTION lfGetFlNa
PRIVATE llReturn
llReturn = .T.
IF EMPTY(lcRpFilNam)
  lcRpFilNam = PUTFILE('',lcRpFilNam,'TXT')
ENDIF
IF FILE(lcRpFilNam)
  lnSelect = gfModalGen("QRM00000B04004","DIALOG",'','',lcRpFilNam +' allready exists, overwrite it?')
  llReturn = IIF(lnSelect=1,.T.,.F.)
ENDIF
RETURN llReturn

*!*************************************************************
*! Name      : lfwCustOG
*! Developer : Albert Raif 
*! Date      : 12/26/2002
*! Purpose   : Function to generate TXT file 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  lfwCustOG()
*!*************************************************************

FUNCTION lfwCustOG
PRIVATE lcvar1

laOGObjCnt[ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRpFilNam'),1)] = .F.
IF llRpGenTxt
  laOGObjCnt[ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lnRpGetFil'),1)] = .T.
ELSE
  laOGObjCnt[ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lnRpGetFil'),1)] = .F.
ENDIF
STORE SPACE(0) TO lcvar1
IF FILE(gcDataDir+'APCHK.MEM')
  RESTORE FROM gcDataDir+'APCHK.MEM' ADDITIVE
ENDIF
lcRpFilNam     = lcVar1

*!*************************************************************
*! Name      : lfGenExp
*! Developer : Albert Raif 
*! Date      : 12/26/2002
*! Purpose   : Function to generate Exp. from OG
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  lfGenExp()
*!*************************************************************

FUNCTION lfGenExp
PRIVATE lcExpGen
lcExpGen = ''
FOR lnCount = 1 TO ALEN(laOgVrFlt,1)
  IF !EMPTY(laOgVrFlt[lnCount,6])
    DO CASE
      CASE laOgVrFlt[lnCount,3] = 'C'
        lcExpGen = lcExpGen + ' AND ' + laOgVrFlt[lnCount,1] +" = '" + laOgVrFlt[lnCount,6] + "'"
      CASE laOgVrFlt[lnCount,3] = 'N'
        lcExpGen = lcExpGen + ' AND ' + laOgVrFlt[lnCount,1] +' = ' + VAL(laOgVrFlt[lnCount,6])
      CASE laOgVrFlt[lnCount,3] = 'D'
        lcExpGen = lcExpGen + ' AND ' + laOgVrFlt[lnCount,1] +' BETWEEN {' + STRTRAN(laOgVrFlt[lnCount,6],'|','} AND {')+'}'
    ENDCASE
  ENDIF
ENDFOR

RETURN lcExpGen

*!*************************************************************
*! Name      : lfRefrance    102793
*! Developer : Albert Raif 
*! Date      : 01/26/2003
*! Purpose   : Function to get TXT file name
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  lfRefrance
*!*************************************************************

FUNCTION lfRefrance
laFooter[lnFooter,5] = SUBSTR(CINVREF,1,15)