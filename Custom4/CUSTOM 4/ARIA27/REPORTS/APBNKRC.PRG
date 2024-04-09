*B500704,1 Hehsam El-sheltawi 05/16/95 The date range filtering was not coded
*B500704,1                             We added a case stsment to build the filtering expresssion
*B500704,1                             in case of filtering by Date not by Period
*E300296,1 M.H 01/04/96 Add the currency to the AP reports.
*B601013,1 RENEE 04/02/96. Correct the spelling of 'Curruncy' to 'Currency'
*B601013,1                 in APBNKRC.FRX
*B800709,1 HS 11/27/96 Made some change in the functions [lfvActBy,lfvChecks 
*B800709,1              , lfwChecks , lfwDefBnk , lfwDefChk] and add some
*B800709,1              functions [lfwBnCod , lfvBnCod]  
*B601609,1 HS 02/16/97 1)Fix the defult value of the varible [lcRpTmpNam]
*B601609,1               from the report generator [change it to expression]. 
*B601609,1             2)Add the field [cAdd_User] to the varible fields
*B601609,1               in the report generator to change the header
*B601609,1               to [Added By].
*E300921,1 TAK 07/12/98 Added to not open the APSETUP file in Ap was Not Installed.
*E300921,4 RENEE 09/14/98 Fix some problems in running the report
*B801832,1 AMM 12/24/98 Adjust totals to include the totals of the previous periods
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
*B802272,1 BWA 05/31/99 Fix the bug of printting voided checks
*E301284,1 AKA 06/27/99  Adding more options in bank reconc. screen 
*E301284,1 				 1)Sorted by Transaction type or Transaction Date.
*E301284,1   			 2)Display AR tran. with same batch no. into one record. 
*B802408,1 AKA 07/08/99 Add subtotal amount for of each transaction type.
*B802408,1              Add grand total amount for of each criteria 
*B802408,1              Add grand total for whole transactions
*B604733,1 SSE 09/18/2001 Fix bug of displaying wrong currency conversion.
*B607443,1 ALB 07/28/2003 Fix Bug of incorrect subtotal and total in the form1 and form2
*B119451,1 ALB 09/03/2003 Rename the label of end total and add new total to form2
*B121387,1 MHM 03/15/2003 fix bug of not getting Eq. currancies in form1 
*B122204,1 NNA 04/13/2004 Fix bug of that the report for period (01) does not show starting balance
*B128787,1 NNA 09/07/2005 Fix Bug of that the report has incorrect Openning and Booked Balance if
*B128787,1 NNA            A/R Display Set to Detail
**************************************************************************************************
*E301284,1 (Start)
IF !USED('Apbanks')
  SELECT 0
  =gfOpenFile(gcDataDir+'APBANKS', 'Bankcode', 'SH')
ENDIF
SELECT APBANKS
*SELECT APPAYMNT 
*E301284,1 (End)

*B802408,1 AKA (Start)  Initialize some variables.
STORE 0 TO lnWholCler,lnWholOpen, lnWholBook
*B802408,1 AKA (End)


*B801832,1 AMM Initialize some variables.
STORE 0 TO lntClear,lntOpen,lnTBooks
lcTmpPmt = ''             && gfTempName()

*B801832,1 AMM end
*B500704,1  Start of the code to build filtering expression in case of Date filter
lcDateFltExp=''                                 && Intialize the Date filtering expression
IF lcRpActBy='D'                                && If filterring by date
  DO CASE
    CASE !EMPTY(ldRpFrDat) AND !EMPTY(ldRpToDat)  
      lcDateFltExp = [BETWEEN(DPAYDATE,ldRpFrDat,ldRpToDat)]

    CASE !EMPTY(ldRpFrDat) 
      lcDateFltExp = [DPAYDATE >= ldRpFrDat]   
    CASE !EMPTY(ldRpToDat)
      lcDateFltExp = [DPAYDATE <= ldRpToDat]   
  ENDCASE    
ENDIF

*B802272,1 Modify to the filter condtion to prevent printting the vioded check 
*lcRpExp = lcRpExp + ".AND. Appaymnt.Cpaystat <> 'V' "
lcRpExp = IIF( EMPTY(ALLTRIM(lcRpExp)) , ""  , lcRpExp + " .AND. ") + " Appaymnt.Cpaystat <> 'V' .AND. Appaymnt.CPAYMETH <> 'H' "
*B802272,1 BWA [END]
IF !EMPTY(lcDateFltExp)
  lcRpExp = lcRpExp+IIF(EMPTY(lcRpExp),'',' AND ')+lcDateFltExp
ENDIF  
*B500704,1 End of modification


*E301284,1  (Start)
* If report creteria changed .
* Note : Variable llOgfltch wil be false if report creteria didn't change.
*        Variable llOgfltch wil be true if report creteria have changed.
IF llOgfltch 
*B122204,1 NNA 04/13/2004 (Begin) if report creteria have changed I delete the Temp File and 
*B122204,1 NNA             the Temporary Index from the master File Because if not a massege error
*B122204,1 NNA             Appears (File not open) or Ending the fox Program
  lcOldAlias = Alias()
  IF FILE(gcWorkDir +lcTmpApPay+ '.DBF') 
    SELECT PAYTEMP
    USE 
    ERASE (gcWorkDir +lcTmpApPay+ '.DBF')
    ERASE (gcWorkDir +lcTmpApPay+ '.CDX')
  ENDIF
  IF FILE(gcWorkDir +lcTmpApPay+ '.CDX') 
    SELECT APPAYMNT
    CLOSE INDEX
    ERASE (gcWorkDir +lcTmpApPay+ '.CDX')
  ENDIF
  SELECT &lcOldAlias
*B122204,1 NNA (End)

  =lfSetfile()
ENDIF
*E301284,1 (End)

*E301284,1 (Start)
SELECT IIF(lcRpType = 'NOBATCH'   , 'APPAYMNT' , 'PayTemp' )
*E301284,1 (End)


*E301284,1 (Start)
IF llOgfltch 
*E301284,1 (End)
  *IHB Fix alias not found error (APVENDOR) [start]
  SET RELATION TO cpayclno INTO Apvendor ADDITIVE
  *BAS
  SET RELATION TO cbnkcode INTO Apbanks ADDITIVE
  *BAS
  *IHB [end]
*E301284,1 (Start)  
ENDIF  
*E301284,1 (End)

*E301284,1 (Start)
*SELECT APPAYMNT 
=lfvOrder()
IF lcRpType = 'BATCH'  
  lcRpExp = STRTRAN( UPPER(lcRpExp) , 'APPAYMNT', 'PAYTEMP')
ENDIF
*E301284,1 (End)

DO gfDispRe WITH EVAL('LCRPFORM'),IIF(!EMPTY(lcRpExp),'FOR ','')+lcRpExp

*!**************************************************************************
*!
*!      Function: lfwDefBnk
*!
*!**************************************************************************
*
FUNCTION lfwDefBnk

*E300921,1 Added AP check.
IF (OCCURS('AP',gcCmpModules)<>0) AND EMPTY(&lcOGVarName) 
  &lcOGVarName=APSETUP.CBNKCODE
ENDIF

*B800709,1 Change this line [Begin]
*RETURN REPLI('X',8)
RETURN "@! XXXXXXXX"
*B800709,1 Change this line [End]

*!**************************************************************************
*!
*!      Function: lfwDefChk
*!
*!**************************************************************************
*
FUNCTION lfwDefChk

*E300921,1 Added AP check.
IF (OCCURS('AP',gcCmpModules)<>0) AND EMPTY(&lcOGVarName) 
  &lcOGVarName=APSETUP.CCHKACCT
ENDIF

*B800709,1 Change this line [Begin]
*RETURN REPLI('X',12)
RETURN "@! XXXXXXXXXXXX"
*B800709,1 Change this line [End]

*!**************************************************************************
*!
*!      Function: lfwChecks
*!
*!**************************************************************************
*
FUNCTION lfwChecks

lcOldchk=(EVAL(SYS(18)))

*!**************************************************************************
*!
*!      Function: lfvChecks
*!
*!**************************************************************************
*
FUNCTION lfvChecks

IF lcOldchk=(EVAL(SYS(18)))
  RETURN
ENDIF

*B800709,1 This lines was add by HS [Begin]
*B800709,1 IF Statment to check if the Checking Account is empty
IF EMPTY(LAOGVRFLT(lnChCodEl,6))
   RETURN
ENDIF      &&End of IF
*B800709,1 This lines was add by HS [End]
    
SELECT APCHECKS
lcSavOrder = SET('ORDER')   && Save old order
SET ORDER TO TAG BANKCHECK

*B800709,1 This lines was add by HS [Begin]
lcBnCod = ' '     &&Varible to hold the Bank Code
*B800709,1 IF Statment to check if ther is a Bank Code GET FIELD
IF lnBnCodEl <> 0
   lcBnCod = PADR(LAOGVRFLT(lnBnCodEl,6) , 8 , ' ')    &&Varible to hold the Bank Code
ENDIF      &&End of IF
*B800709,1 This lines was add by HS [End]

*B800709,1 Ther is no need for this lines now [Begin]
*lcFldNam = SYS(18)
*IF SEEK(EVAL(SYS(18)))
*  &lcFldNam=APCHECKS.CCHKACCT
*B800709,1 Ther is no need for this lines now [End]

IF !SEEK(lcBnCod + LAOGVRFLT(lnChCodEl,6))

*B800709,1 Ther was no need for this lines [Begin]
*ELSE
* IF !SEEK(EVAL(SYS(18))) .OR. ATC("?",EVAL(SYS(18))) > 0
*B800709,1 Ther was no need for this lines [End]

    DIMENSION laTemp[2]   && Define an array get Vendors codes and company.
    laTemp = ''
    lcSavBrFld=lcBrFields
    lcSavTitle=lcFile_Ttl
  
    lcBrFields="CBNKCODE :H= 'Bank Code',;
                CCHKACCT :H= 'Check Code'"
    lcFile_Ttl= "Checks"

*B800709,1 Change this line [Begin]
*   =gfBrows(.F.,'CBNKCODE,CCHKACCT','laTemp')
    =gfBrows(IIF(EMPTY(lcBnCod),.F.,[FOR CBNKCODE = lcBnCod]),'CBNKCODE,CCHKACCT','laTemp')
*B800709,1 Change this line [End]

    lcBrFields=lcSavBrFld
    lcFile_Ttl=lcSavTitle
    IF !EMPTY(laTemp[2])
      
      *B800709,1 Change this line [Bigen]
      *&lcFldNam=laTemp[2]
      LAOGVRFLT(lnChCodEl,6) = laTemp[2]
      *B800709,1 Change this line [End]
      
      *B800709,1 This lines was add by HS [Begin]
      *B800709,1 IF Statment to check if ther is a Bank Code GET FIELD
      IF lnBnCodEl <> 0
         LAOGVRFLT(lnBnCodEl,6) = laTemp[1]
         SHOW GETS       
      ENDIF      &&End of IF
     *B800709,1 This lines was add by HS [End]

    ELSE
      
      *B800709,1 Change this line [Bigen]
      *&lcFldNam=SPACE(24)
      LAOGVRFLT(lnChCodEl,6) = lcOldchk
      *B800709,1 Change this line [End]
      
    ENDIF

*B800709,1 Ther was no need for the IF Statment
* ENDIF

ENDIF
SET ORDER TO &lcSavOrder
SELECT APPAYMNT

*!************************************************************************
*!
*!      Function lfvFisYer
*!
*!************************************************************************
*
 ****  Check if current company has this entried year or not
FUNCTION lfvFisYer

DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
laRpRetFld[1] = ''

&& Check If year field is empty
lcRpFiscYr = ALLTRIM(EVALUATE(SYS(18)))
lcRpCurFld = VARREAD()
*B500704,1  
lnLastkey=LASTKEY()
IF .NOT.EMPTY(lcRpFiscYr)  
  *E300921,4 Use FISHD instead of SYCFISHD
  *lcOldAlias = SELECT()    && Save the current alias
  *llUesdBefo = .F.         && Check if used before or this the first time
  *IF NOT USED("SYCFISHD") 
  *  SELECT 0
  *  USE (gcSysHome+'SYCFISHD') ORDER TAG compfyear
  *  llUesdBefo = .T.
  *ENDIF
  *SELECT SYCFISHD
  lcOldAlias = SELECT(0)    && Save the current alias
  *E300789,4  AMM Open FISHD tabel from the right location
  *llUsedBefo = gfOpenFile(gcDataDir+'FISHD', 'COMPFYEAR', 'SH')
  *SELECT FISHD
  = gfOpenFile(gcSysHome+'SYCCOMP','Ccomp_id','SH')
  lcDataDir = ALLTRIM(IIF(SEEK(gcPrnt_Cmp, 'SYCCOMP'),gfGetDataDir(ALLT(SYCCOMP.cCom_DDir)), gcDataDir))
  llUsedBefo = gfOpenFile(lcDataDir+'FISHD', 'COMPFYEAR', 'SH')
  *E300789,4  AMM END
  *E300921,4 end
  
  *** Search for the current company+year
    *E300789,4  AMM Adjust to fit the new index
    *IF ('?' $ &lcRpCurFld. .OR. !SEEK(gcPrnt_Cmp+lcRpFiscYr)) 
    IF ('?' $ &lcRpCurFld. .OR. !SEEK(lcRpFiscYr)) 
    *E300789,4  AMM end
       *MAN 
       *B500704,1 Changed browse field heading from  Fiscal Periods to Banks Codes
       *=gfBrows([gcPrnt_Cmp],'CFisFyear',"laRpRetFld",'Banks Codes ',.F.)
        *=gfBrows([gcPrnt_Cmp],'CFisFyear',"laRpRetFld",'Fiscal Periods',.F.)
        =gfBrows("",'CFisFyear',"laRpRetFld",'Fiscal Periods',.F.)
      &lcRpCurFld. = laRpRetFld[1]
      lcRpFiscYr   = laRpRetFld[1]
      SHOW GET (lcRpCurFld)
    ENDIF
  

  *E300921,4 Use FISHD instead of SYCFISHD
  *IF llUesdBefo       && .F.- this file used by the system
    *USE IN SYCFISHD
  IF llUsedBefo       && .F.- this file used by the system
    USE IN FISHD
    *E300921,4 end
  ENDIF
  SELECT (lcOldAlias)
ENDIF
RETURN 

*!************************************************************************
*!
*!      Function lfvFisPrd
*!
*!************************************************************************
*
****  Check if current company has this entried period or not
FUNCTION lfvFisPrd

*B500704,1 Add an element to the array laRpRetFld
*DECLARE laRpRetFld(1)
DECLARE laRpRetFld(2)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",CFspprdid:H="Period",CFsppDesc:H="Month"'
*B500704,1 Change the array intialization to intialize all elements
laRpRetFld[1] = ''
STORE '' TO laRpRetFld

lcRpCurFld      = VARREAD()
*B500704,1  Cancel the checking for the year field
&& Check If year field is empty
*IF .NOT. EMPTY(lcRpFiscYr) 
  IF .NOT.EMPTY(ALLTRIM(&lcRpCurFld.))  
    *E300921,4 Use FISHD instead of SYCFISHD
    *lcOldAlias = SELECT()    && Save the current alias
    *llUesdBefo = .F.        && Check if used before or this the first time
    *IF NOT USED("SYCFSPRD") 
    *  SELECT 0
    *  USE &gcSysHome.SYCFSPRD ORDER TAG comfyrprdi
    *  llUesdBefo = .T.
    *ENDIF
    *SELECT SYCFSPRD
    lcOldAlias = SELECT(0)    && Save the current alias
    *E300789,4  AMM Open FSPRD from the right location
    *llUsedBefo = gfOpenFile(gcDataDir+'FSPRD', 'COMFYRPRDI', 'SH')
    *SELECT FSPRD
    = gfOpenFile(gcSysHome+'SYCCOMP','Ccomp_id','SH')
    lcDataDir = ALLTRIM(IIF(SEEK(gcPrnt_Cmp, 'SYCCOMP'),gfGetDataDir(ALLT(SYCCOMP.cCom_DDir)), gcDataDir))
    llUsedBefo = gfOpenFile(lcDataDir+'FSPRD', 'COMFYRPRDI', 'SH')  
    *E300789,4  AMM end

    *E300921,4 end
    *E300921,4 The following lines of code gave an error because
    *E300921,4 they were not following each other
    *** Search for the current company+year+Prd
    *IF ('?' $ &lcRpCurFld. .OR.;  
     *B500704,1   Code Changed
     *!SEEK(gcAct_comp+ALLTRIM(lcRpFiscYr)+ALLTRIM(&lcRpCurFld.)))
     *!SEEK(gcAct_comp+laOGFxFlt[1,6]+ALLTRIM(&lcRpCurFld.)))
     *E300789,4  AMM Adjust to fit the new index
     *IF ('?' $ &lcRpCurFld. .OR.;  
        !SEEK(gcAct_comp+laOGFxFlt[1,6]+ALLTRIM(&lcRpCurFld.)))
     IF ('?' $ &lcRpCurFld. .OR.;  
        !SEEK(laOGFxFlt[1,6]+ALLTRIM(&lcRpCurFld.)))
     *E300789,4  AMM end

     *E300921,4 end
     *MAN    
     *B500704,1   Code Changed 
     *=gfBrows([gcPrnt_Cmp],'CFSPPRDID',"laRpRetFld",'Banks Codes ',.F.)
     *=gfBrows([gcPrnt_Cmp]+IIF(EMPTY(laOGFxFlt[1,6]),'','+laOGFxFlt[1,6]'),'CFSPPRDID,CFisFyear',"laRpRetFld",'Fiscal Periods',.F.)
     =gfBrows(IIF(EMPTY(laOGFxFlt[1,6]),'','+laOGFxFlt[1,6]'),'CFSPPRDID,CFisFyear',"laRpRetFld",'Fiscal Periods',.F.)
      
      &lcRpCurFld = laRpRetFld[1]
      
      *B500704,1   Code Added Start
      IF EMPTY(laOGFxFlt[1,6])
        laOGFxFlt[1,6] = laRpRetFld[2]
      ENDIF
      *B500704,1   Code Added Finish
      
      SHOW GET (lcRpCurFld)
      SHOW GET  laOGFxFlt[1,6] LEVEL RDLEVEL()-1
    ENDIF

      *E300921,4 Use FISHD instead of SYCFISHD
    *IF llUesdBefo       && .F.- this file used by the system
      *USE IN SYCFSPRD
    IF llUsedBefo       && .F.- this file used by the system
      USE IN FSPRD
      *E300921,4 end
    ENDIF
    SELECT (lcOldAlias)
  ENDIF
*B500704,1  Cancel the checking for the year field
*ENDIF
RETURN 

*!************************************************************************
*!
*!      Function lfvToDay
*!
*!************************************************************************
* Check if the ldRpFrDat is smaller than ldRpToDat or not

Function lfvToDay

IF ldRpFrDat > ldRpToDat
  ** MESSAGE : "From Data can not be less than Through Date"
  **           "                       ® Ok ¯ 
  =gfModalGen("TRM04028B00000","DIALOG","to"+"|"+"from")
  _CUROBJ = _CUROBJ
ENDIF


*!************************************************************************
*!
*!      Function lfvActBy
*!
*!************************************************************************
* Control the variable related to Activity way

Function lfvActBy

DO CASE
  CASE lcRpActBy = 'P'
    *B802408,1 AKA (Start)
    *laOGObjCnt[3] = .F.    && From Dat
    *laOGObjCnt[4] = .F.    && To Dat
    =lfGetVarPos("ldRpFrDat")
    =lfGetVarPos("ldRpToDat")
    *B802408,1 AKA (End)

    *E300921,4 Enable year/period
    *laOGObjCnt[5] = .T.    && Fiscal year
    *laOGObjCnt[6] = .T.    && Period

    *B802408,1 AKA (Start)
    *laOGObjCnt[6] = .T.    && Fiscal year
    *laOGObjCnt[7] = .T.    && Period
    =lfGetVarPos("APPAYMNT.CFISFYEAR" , .T.)
    =lfGetVarPos("APPAYMNT.CFSPPRDID" , .T.)
    *B802408,1 AKA (End)    
    
    *E300921,4 end

    ldRpFrDat     = {}
    ldRpToDat     = {}

  CASE lcRpActBy = 'D'
    *B802408,1 AKA (Start)    
    *laOGObjCnt[3] = .T.
    *laOGObjCnt[4] = .T.
    =lfGetVarPos("ldRpFrDat" , .T.)
    =lfGetVarPos("ldRpToDat" , .T.)
    *B802408,1 AKA (End)
    
    
    *E300921,4 Disable year/period
    *laOGObjCnt[5] = .F.
    *laOGObjCnt[6] = .F.
    *B802408,1 AKA (Start)    
    *laOGObjCnt[6] = .F.
    *laOGObjCnt[7] = .F.
    =lfGetVarPos("APPAYMNT.CFISFYEAR")
    =lfGetVarPos("APPAYMNT.CFSPPRDID")
    *B802408,1 AKA (End)    
    *E300921,4 end 
    *B500704,1  
    
    laOGFxFlt[1,6] = ''
    laOGFxFlt[2,6] = ''
ENDCASE

*B802408,1 AKA (Start)    
* Display  Report format , AR display , Sort by 
=lfGetVarPos("lcRpForm" , .T.)
=lfGetVarPos("lcRpType" , .T.)      
=lfGetVarPos("lcRpSortBy" , .T.)
*B802408,1 AKA (End)    


=lfActvateWin(lnWinDisp)

*B800709,1 This lines was added by HS [Begin]
lnBnCodEl = 0      &&Varible to hold the LAOGVRFLT element number of the Bank Code  
lnChCodEl = 0      &&Varible to hold the LAOGVRFLT element number of the Checking Account 
*B800709,1 FOR Loop to find the LAOGVRFLT element number of the Bank Code 
*B800709,1 and Checking Account
FOR lnCount = 1 TO ALEN(LAOGVRFLT , 1)
    *B800709,1 IF Statment to check if the curent element of the LAOGVRFLT
    *B800709,1 array is the Bank Code element
    IF UPPER(ALLTRIM(LAOGVRFLT(lnCount,1))) = 'APPAYMNT.CBNKCODE'
       lnBnCodEl = lnCount
    ENDIF    &&End of IF
    *B800709,1 IF Statment to check if the curent element of the LAOGVRFLT
    *B800709,1 array is the Checking Account element
    IF UPPER(ALLTRIM(LAOGVRFLT(lnCount,1))) = 'APPAYMNT.CCHKACCT'
       lnChCodEl = lnCount
    ENDIF    &&End of IF
ENDFOR   &&End of the FOR Loop     
*B800709,1 This lines was added by HS [End]

*!**************************************************************************
*!
*!      Function: lfRepShow
*!
*!**************************************************************************
*
FUNCTION lfRepShow

*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
laOGObjCnt[5] = gfGetMemVar('LLMULCURR')
=lfOGShowGet("lnRepCurr")
*E300296,1 End

*!**************************************************************************
*!
*!      Function: lfvCurDisp
*!
*!**************************************************************************
*E300296,1 M.H 01/02/96 Add the currency to the AP reports.
*
FUNCTION lfvCurDisp

llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*!*************************************************************
*! Name      : lfwBnCod
*! Developer : Haytham El_Sheltawi
*! Date      : 11/27/1996
*! Purpose   : This function is to save the old Bank code  
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*B800709,1 This function was added by HS
*!*************************************************************
*
FUNCTION lfwBnCod

lcOldBnCod = LAOGVRFLT(lnBnCodEl,6)    &&Varible to hold the old Bank Code

*!*************************************************************
*! Name      : lfvBnCod
*! Developer : Haytham El_Sheltawi
*! Date      : 11/27/1996
*! Purpose   : This function is to valid the Bank code  
*!*************************************************************
*! Called from : The Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*B800709,1 This function was added by HS
*!*************************************************************
*
FUNCTION lfvBnCod

*IF Statment to check if the Bank Code was changed
IF lcOldBnCod <> LAOGVRFLT(lnBnCodEl,6)
   *IF Statment to check if the new Bank Code is empty
   IF EMPTY(LAOGVRFLT(lnBnCodEl,6)) .AND. lnChCodEl <> 0
      LAOGVRFLT(lnChCodEl,6) = SPACE(12)   &&Clear the Checking Account
      SHOW GETS 
      RETURN
   ENDIF    &&End of IF
   lcSavAlias = ALIAS()     && Save old alias
   SELECT APCHECKS
   lcSavOrder = SET('ORDER')   && Save old order
   lcSavExact = SET('EXACT')   && Save old exact status
   SET ORDER TO TAG BANKCHECK
   SET EXACT OFF
   *IF Statment to check if the new Bank Code exist in the file
   IF SEEK(LAOGVRFLT(lnBnCodEl,6)) .AND. lnChCodEl <> 0 
      LAOGVRFLT(lnChCodEl,6) = APCHECKS.CCHKACCT
      SHOW GETS
   ENDIF     &&End of IF
   SET ORDER TO &lcSavOrder
   SET EXACT &lcSavExact
   SELECT &lcSavAlias
ENDIF   &&End of IF
*!*************************************************************
*! Name      : lfgetTot
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 12/24/98
*! Purpose   : Function to get totals of the periods previous to 
*!             the chosen ones
*! Ref       : *B801832,1
*!*************************************************************
*! Called from : APBNKRC.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
FUNCTION lfgetTot
lnAlias = SELECT(0)
STORE 0 TO lntClear,lntOpen,lnTBooks

lcTotFlt = "CPAYSTAT <> 'V' .AND. CPAYMETH <> 'H'"

*B801832,1 AMM If select by date and no from date, there is no previous total to 
*B801832,1 AMM calculate. so return
*B801832,1 AMM If select by period , and empty of FROM period, also, there is no 
*B801832,1 AMM previous total to calculate

*E301284,1  AKA (Start)
*IF (lcRpActBy='D' .AND. EMPTY(ldRpFrDat) ) .OR. ;
      (lcRpActBy='P' .AND. AT("BETWEEN(APPAYMNT.CFSPPRDID,",lcRpExp) = 0)

IF lcRpType = 'NOBATCH'  
  IF (lcRpActBy='D' .AND. EMPTY(ldRpFrDat) ) .OR. ;
     (lcRpActBy='P' .AND. AT("BETWEEN(APPAYMNT.CFSPPRDID,",lcRpExp) = 0)
    RETURN 
  ENDIF   
ELSE
  IF (lcRpActBy='D' .AND. EMPTY(ldRpFrDat) ) .OR. ;
     (lcRpActBy='P' .AND. AT("BETWEEN(PAYTEMP.CFSPPRDID,",lcRpExp) = 0)
    RETURN 
  ENDIF   
ENDIF
*E301284,1  AKA (End)

*B801832,1 AMM If select by date, calculate the totals up to the FROM date.
IF lcRpActBy = 'D'                           && If filterring by date
  lcTotFlt =  lcTotFlt+" .AND. DPAYDATE < ldRpFrDat "
ELSE
  *B801832,1 AMM If select by period, Calculate totals up to the FROM period
  
  *E301284,1  AKA (Start)
  *lcPrd = SUBSTR(lcRpExp,AT("BETWEEN(APPAYMNT.CFSPPRDID,",lcRpExp) + ;
  *               LEN("BETWEEN(APPAYMNT.CFSPPRDID,'"),2)
  IF lcRpType = 'NOBATCH'  
    lcPrd = SUBSTR(lcRpExp,AT("BETWEEN(APPAYMNT.CFSPPRDID,",lcRpExp) + ;
                   LEN("BETWEEN(APPAYMNT.CFSPPRDID,'"),2)
  ELSE
    *B128787,1 NNA 09/07/2005 (Start) in case of A/R display set to Summary we use PAYTEMP not APPAYMNT
    *lcPrd = SUBSTR(lcRpExp,AT("BETWEEN(APPAYMNT.CFSPPRDID,",lcRpExp) + ;
                   LEN("BETWEEN(PAYTEMP.CFSPPRDID,'"),2)
    lcPrd = SUBSTR(lcRpExp,AT("BETWEEN(PAYTEMP.CFSPPRDID,",lcRpExp) + ;
                   LEN("BETWEEN(PAYTEMP.CFSPPRDID,'"),2)
    *B128787,1 NNA (End)
  ENDIF
  *E301284,1  AKA (End)


  *E301284,1  AKA (Start)
  * lcTotFlt =  lcTotFlt+" .AND. cFisfYear=APPAYMNT.cFisFYear"+;
                       " .AND. VAL(cfspprdid) < VAL(lcPrd)"
 
  IF lcRpType = 'NOBATCH'  
    *B122204,1 NNA 04/13/2004 (Begin) Change the filter to scan for the years before the fiscal year
    *lcTotFlt =  lcTotFlt+" .AND. cFisfYear=APPAYMNT.cFisFYear"+;
                         " .AND. VAL(cfspprdid) < VAL(lcPrd)"

    *B128787,1 NNA 09/07/2005 (Start) Seek for the Previous Year or the current Year
    *--consider Brackts
    *lcTotFlt =  lcTotFlt+" .AND. cFisfYear < APPAYMNT.cFisFYear"+ ;
                         " .OR. (cFisfYear=APPAYMNT.cFisFYear .AND. VAL(cfspprdid) < VAL(lcPrd))"
    lcTotFlt =  lcTotFlt+" .AND. (cFisfYear < APPAYMNT.cFisFYear"+ ;
                         " .OR. (cFisfYear=APPAYMNT.cFisFYear .AND. VAL(cfspprdid) < VAL(lcPrd)))"
    *B128787,1 NNA (End)

    *B122204,1 NNA (End)
  ELSE
    *B122204,1 NNA 04/13/2004 (Begin) Change the filter to scan for the years before the fiscal year
    *lcTotFlt =  lcTotFlt+" .AND. cFisfYear=PAYTEMP.cFisFYear"+;
                         " .AND. VAL(cfspprdid) < VAL(lcPrd)"

    *B128787,1 NNA 09/07/2005 (Start) Seek for the Previous Year or the current Year
    *--consider Brackts
    *lcTotFlt =  lcTotFlt+" .AND. cFisfYear < PAYTEMP.cFisFYear"+ ;
                         " .OR. (cFisfYear=PAYTEMP.cFisFYear .AND. VAL(cfspprdid) < VAL(lcPrd))"
    lcTotFlt =  lcTotFlt+" .AND. (cFisfYear < PAYTEMP.cFisFYear"+ ;
                         " .OR. (cFisfYear=PAYTEMP.cFisFYear .AND. VAL(cfspprdid) < VAL(lcPrd)))"
    *B128787,1 NNA (End)

    *B122204,1 NNA (End)
  ENDIF
  *E301284,1  AKA (End)                       
  
ENDIF

USE (gcDataDir+'APPAYMNT') ORDER CHKTMNO AGAIN ALIAS lcTmpPmt IN 0

SELECT lcTmpPmt
*E301284,1  AKA (Start)
*=SEEK(APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT)
IF lcRpType = 'NOBATCH'  
  =SEEK(APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT)
ELSE
  =SEEK(PayTemp.CBNKCODE+PayTemp.CCHKACCT)
ENDIF   
*E301284,1  AKA (End)

*E301284,1  AKA (Start)
IF lcRpType = 'NOBATCH'  
  lcCondStr  = "CBNKCODE+CCHKACCT = APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT"
ELSE
  lcCondStr  = "CBNKCODE+CCHKACCT = PayTemp.CBNKCODE+PayTemp.CCHKACCT"
ENDIF 
*E301284,1  AKA (End)

*B801832,1 AMM Loop on the Bank+check account, to calculate the former totals 
*B801832,1 AMM of the chosen period or date

*E301284,1  AKA (Start)
*SCAN WHILE CBNKCODE+CCHKACCT = APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT ;
     FOR &lcTotFlt
     
SCAN REST WHILE &lcCondStr FOR &lcTotFlt
  *E301284,1  AKA (End)     
  
  *IF lcTmpPmt.cCurrCode <> gcBaseCurr AND lcRpCurr <> 'F'
    * Initialize lnEqvAmnt variable
*	lcExSin2  = ''
*	lcExSin1  = gfGetExSin(@lcExSin2, lcTmpPmt.cCurrCode ) 
*	lnValue   = ROUND( lcTmpPmt.nPayAmnt &lcExSin1 lcTmpPmt.nExRate &lcExSin2 lcTmpPmt.nCurrUnit ,2)
*  ELSE
	lnValue   =  lcTmpPmt.nPayAmnt
 * ENDIF  
  lntClear = lntClear + IIF(CPAYRECST = 'C' , -lnValue  , 0 )
  lntOpen  = lntOpen  + IIF(CPAYRECST = 'O' , -lnValue  , 0 )
  lnTBooks = lnTBooks + IIF(CPAYRECST = 'V' , 0 , -lnValue  )
  
ENDSCAN

USE IN lcTmpPmt
SELECT (lnAlias)

*!**************************************************************************
*!
*!      Function: lfvOrder
*!
*!**************************************************************************
*
FUNCTION lfvOrder

IF lcRpSortBy = 'N'
  SET ORDER TO TAG CHKTMNO
ELSE
  SET ORDER TO TAG PayDate 
ENDIF 
RETURN 


*!*************************************************************
*! Name      : lfGenTmp
*! Developer : Amin Khodary 
*! Date      : 06/20/1999
*! Purpose   : Copy structure of temp. table form  APPATMNT 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      :  None
*!*************************************************************
PROCEDURE lfGenTmp
PRIVATE lcExpr

STORE 0   TO lnAmount 
STORE ""  TO lcBatchNo 

*B604733,1 Open ApDist file to get the currency from there. [Begin]
PRIVATE llApDistOp
llApDistOp = gfOpenFile(gcDataDir+'ApDist', 'Paymnts', 'SH')
*B604733,1 Open ApDist file to get the currency from there. [End]

=lfCreatTmp()
SELECT PayTemp
SELECT APPAYMNT
lcGenOrd  = ORDER() 
SET ORDER TO TAG Chkbtch OF (gcWorkDir +lcTmpIndx  + '.CDX')
SCAN WHILE !EOF('APPAYMNT')  
  SELECT APPAYMNT 
  IF EMPTY(APPAYMNT.Batch)
    DO lfInsrtRow with APPAYMNT.NPAYAMNT 
  ELSE
    * summary data   
    IF lcRpType = 'BATCH'
      * Get batch no. 
      lcBatchNo  = APPAYMNT.Batch
      * Get record no. 
      nlRecNo = RECNO('APPAYMNT')
      * Initialze amount variable 
      lnAmount     = 0 
      SCAN REST WHILE lcBatchNo = APPAYMNT.BATCH .AND. !EOF('APPAYMNT')  
        * Get record no. 
        nlRecNo = RECNO('APPAYMNT')
        lnAmount     = lnAmount  + APPAYMNT.NPAYAMNT 
      ENDSCAN
      IF lnAmount      <> 0  
        GOTO nlRecNo
        DO lfInsrtRow WITH lnAmount     
      ENDIF

    ELSE 
      * Detail data 
      DO lfInsrtRow WITH APPAYMNT.NPAYAMNT
    ENDIF
  ENDIF
  
ENDSCAN
*B604733,1 Close ApDist file. [Begin]
IF llApDistOp
  USE IN 'ApDist'
ENDIF
*B604733,1 Close ApDist file. [End]

SELECT APPAYMNT 
SET ORDER TO &lcGenOrd 



*!*************************************************************
*! Name      : lfInsrtRow
*! Developer : Amin Khodary 
*! Date      : 06/20/1999
*! Purpose   : Copy the current data of APPATMNT row to temp. table 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 ) nAmtValue
*!*************************************************************
*! Return      :  None
*!*************************************************************
PROCEDURE lfInsrtRow
PARAMETER nAmtValue

*B604733,1 get currency from ApDist. [Begin]
*INSERT INTO PayTemp ( CPAYTYPE , CPAYMETH , CBNKCODE ,;
*                          CCHKACCT , CPAYDOCNO, CPAYSTAT ,;
*		                  DPAYDATE , CFISFYEAR, CFSPPRDID,;
*	  					  DPAYVDATE, LPAYADVAN, CPAYCLNO ,;
*					      CPAYCOMP , NPAYAMNT , NPAYDISC ,;
*					      NPAYADJ  , CPAYRECST, DPAYRECDT,;
*					      CPAYRECRF, CBNKADJ  , BATCH    ,;
*					      cCurrCode, nExRate  , nCurrUnit,; 
*					      NRECNO  );
*         VALUES (   APPAYMNT.CPAYTYPE  ,;
*                    APPAYMNT.CPAYMETH  ,;
*                    APPAYMNT.CBNKCODE  ,;
*                    APPAYMNT.CCHKACCT  ,;
*                    APPAYMNT.CPAYDOCNO ,;
*     			     APPAYMNT.CPAYSTAT  ,;
*                    APPAYMNT.DPAYDATE  ,;
*                    APPAYMNT.CFISFYEAR ,;  
*                    APPAYMNT.CFSPPRDID ,;
*		             APPAYMNT.DPAYVDATE ,; 
*                    APPAYMNT.LPAYADVAN ,;
*                    APPAYMNT.CPAYCLNO  ,;
*                    APPAYMNT.CPAYCOMP  ,;
*                    nAmtValue          ,; 
*                    APPAYMNT.NPAYDISC  ,;
*                    APPAYMNT.NPAYADJ   ,;
*                    APPAYMNT.CPAYRECST ,;
*                    APPAYMNT.DPAYRECDT ,;
*                    APPAYMNT.CPAYRECRF ,;
*                    APPAYMNT.CBNKADJ   ,;
*                    APPAYMNT.BATCH     ,;
*                    APPAYMNT.cCurrCode ,;
*                    APPAYMNT.nExRate   ,;
*                    APPAYMNT.nCurrUnit ,;
*                    RECNO('APPAYMNT')   )

PRIVATE lcCurrency , lnExRate , lnCurrUnit
lcCurrency = ApPaymnt.cCurrCode
lnExRate   = ApPaymnt.nExRate
lnCurrUnit = ApPaymnt.nCurrUnit
IF SEEK(ApPaymnt.cPayMeth+ApPaymnt.cBnkCode+ApPaymnt.cChkAcct+ApPaymnt.cPayDocNo,'ApDist')
  SELECT ApDist  
  LOCATE REST WHILE capdtrtyp+cbnkcode+cchkacct+capdref+cinvno+capdactid = ;
              ApPaymnt.cPayMeth+ApPaymnt.cBnkCode+ApPaymnt.cChkAcct+ApPaymnt.cPayDocNo ; 
              FOR capdactid = 'C'
  IF FOUND()
    lcCurrency = cCurrCode
    lnExRate   = nExRate
    lnCurrUnit = nCurrUnit
  ENDIF                
ENDIF

INSERT INTO PayTemp ( CPAYTYPE , CPAYMETH , CBNKCODE ,;
                          CCHKACCT , CPAYDOCNO, CPAYSTAT ,;
		                  DPAYDATE , CFISFYEAR, CFSPPRDID,;
	  					  DPAYVDATE, LPAYADVAN, CPAYCLNO ,;
					      CPAYCOMP , NPAYAMNT , NPAYDISC ,;
					      NPAYADJ  , CPAYRECST, DPAYRECDT,;
					      CPAYRECRF, CBNKADJ  , BATCH    ,;
					      cCurrCode, nExRate  , nCurrUnit,; 
					      NRECNO  );
         VALUES (   APPAYMNT.CPAYTYPE  ,;
                    APPAYMNT.CPAYMETH  ,;
                    APPAYMNT.CBNKCODE  ,;
                    APPAYMNT.CCHKACCT  ,;
                    APPAYMNT.CPAYDOCNO ,;
     			    APPAYMNT.CPAYSTAT  ,;
                    APPAYMNT.DPAYDATE  ,;
                    APPAYMNT.CFISFYEAR ,;  
                    APPAYMNT.CFSPPRDID ,;
					APPAYMNT.DPAYVDATE ,; 
					APPAYMNT.LPAYADVAN ,;
					APPAYMNT.CPAYCLNO  ,;
					APPAYMNT.CPAYCOMP  ,;
                    nAmtValue          ,; 
					APPAYMNT.NPAYDISC  ,;
					APPAYMNT.NPAYADJ   ,;
					APPAYMNT.CPAYRECST ,;
					APPAYMNT.DPAYRECDT ,;
					APPAYMNT.CPAYRECRF ,;
					APPAYMNT.CBNKADJ   ,;
				    APPAYMNT.BATCH     ,;
					lcCurrency         ,;
					lnExRate           ,;
					lnCurrUnit         ,;
				    RECNO('APPAYMNT')   )
*B604733,1 get currency from ApDist. [End]

SELECT APPAYMNT 
RETURN 

*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Amin Khodary 
*! Date      : 27/20/1999
*! Purpose   : 1) Create temp. table 
*!             2) Open or close table based on parameter
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 ) True to create and open the file 
*!                     2)  False to close and erase the file 
*!*************************************************************
*! Return      :  None
*!*************************************************************
FUNCTION  lfCreatTmp
PRIVATE  llBalnBfor           && Flag to know if the temp. table has been created 
IF FILE(gcWorkDir +lcTmpApPay+ '.DBF') 
  SELECT PayTemp
  ZAP
ELSE
  DIME ArryPayTmp(1,4)
  SELECT APPAYMNT
  =AFIELDS(ArryPayTmp)
  nAryLen = ALEN(ArryPayTmp ,1 ) + 1
  DIME ArryPayTmp( nAryLen  , 4 )
  ArryPayTmp(nAryLen  , 1 ) = 'NRECNO'
  ArryPayTmp(nAryLen  , 2 ) = 'N'
  ArryPayTmp(nAryLen  , 3 ) = 6
  ArryPayTmp(nAryLen  , 4 ) = 0
  lcTmpApPay = lfChckNdx('BATCH' , 'DBF')
  lcTmpApPay = lfChckNdx('BATCH' , 'CDX')
  CREATE DBF (gcWorkDir + lcTmpApPay)  FROM ARRAY ArryPayTmp
  USE 
  SELECT 0 
  USE (gcWorkDir + lcTmpApPay) ALIAS PayTemp EXCLUSIVE 
  INDEX ON cbnkcode+cchkacct+cpaytype+cpaymeth+cpaydocno TAG CHKTMNO OF (gcWorkDir + lcTmpApPay+ '.CDX')
  INDEX ON cbnkcode+cchkacct+cpaytype+cpaymeth+DTOS(dPayDate) TAG PayDate OF (gcWorkDir + lcTmpApPay+ '.CDX') 
  
  SELECT PayTemp
ENDIF
RETURN


*FUNCTION lfvSortBy
*llChSortBy = .T.
*--


*!**************************************************************************
*!
*!      FUNCTION : lfChckNdx
*!
*!**************************************************************************
* 
FUNCTION lfChckNdx
PARAMETERS  lcParmName , lcFileType
IF TYPE('lcFileType') <> 'C'
  lcFileType = ''
ENDIF
STORE "" TO lcNdxName  

DO CASE
  
  CASE lcParmName = 'NOBATCH'
    * If index name is already defined 
    IF TYPE("lcTmpApPay") <> 'U'
      * If index file is already creadted 
      IF !FILE(gcWorkDir +lcTmpApPay+ '.CDX') 
        lcNdxName  = gfTempName()        
      ELSE
        lcNdxName  = lcTmpApPay
      ENDIF
    ELSE    && ELASE OF  "If index name is already defined "
      lcNdxName = gfTempName()          
    ENDIF  && ENDIF OF  "If index name is already defined "


  CASE lcParmName = 'BATCH' AND EMPTY(lcFileType)
    * If index name is already defined 
    IF TYPE("lcTmpIndx") <> 'U'
      * If index file is already creadted 
      IF !FILE(gcWorkDir +lcTmpIndx+ '.CDX') 
        lcNdxName  = gfTempName()        
      ELSE
        lcNdxName  = lcTmpApPay
      ENDIF
    ELSE    && ELASE OF  "If index name is already defined "
      lcNdxName = gfTempName()          
    ENDIF  && ENDIF OF  "If index name is already defined "

    
  CASE lcParmName = 'BATCH' AND UPPER(lcFileType)  = 'CDX'
    * If index name is already defined 
    IF TYPE("lcTmpApPay") <> 'U'
      * If index file is already creadted 
      IF !FILE(gcWorkDir +lcTmpApPay+ '.CDX') 
        lcNdxName  = gfTempName()        
      ELSE
        lcNdxName  = lcTmpApPay
      ENDIF
    ELSE    && ELASE OF  "If index name is already defined "
      lcNdxName = gfTempName()          
    ENDIF  && ENDIF OF  "If index name is already defined "
    
  CASE lcParmName = 'BATCH' AND UPPER(lcFileType)  = 'DBF'
    * If index name is already defined 
    IF TYPE("lcTmpApPay") <> 'U'
      * If index file is already creadted 
      IF !FILE(gcWorkDir +lcTmpApPay+ '.DBF') 
        lcNdxName  = gfTempName()        
      ELSE
        lcNdxName  = lcTmpApPay
      ENDIF
    ELSE    && ELASE OF  "If index name is already defined "
      lcNdxName = gfTempName()          
    ENDIF  && ENDIF OF  "If index name is already defined "
    
ENDCASE
RETURN lcNdxName 



*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
* 
FUNCTION lfClearRep

*! Based on the report type whetehr is batch or nonbatch the index file 
*! lcTmpApPay will be created. so if user have selected batch option report this file 
*! (lcTmpApPay) will be created as DBF and CDX ,  but if the user have selected 
*! nonbatch option report this file (lcTmpApPay) will be created CDX only based on
*! related to master file APPAYMNT. 
*! Therfore while erasing that file i can't now it's status so to avoid foxpro error 
*! message i wrote the below command becuase the index file lcTmpApPay will be
*! deleted in one of the below commands.

PRIVATE lcCurErHnd
lcCurErHnd = ON("ERROR")
ON ERROR STORE .T. TO llDumErHnd


IF FILE(gcWorkDir +lcTmpApPay+ '.DBF') 
  SELECT PAYTEMP
  USE 
  ERASE (gcWorkDir +lcTmpApPay+ '.DBF')
  ERASE (gcWorkDir +lcTmpApPay+ '.CDX')
ENDIF

IF FILE(gcWorkDir +lcTmpApPay+ '.CDX') 
  SELECT APPAYMNT
  CLOSE INDEX
  ERASE (gcWorkDir +lcTmpApPay+ '.CDX')
ENDIF
ON ERROR &lcCurErHnd

lcCurErHnd = ON("ERROR")
ON ERROR STORE .T. TO llDumErHnd

IF TYPE('lcTmpIndx') <> 'U'
  IF FILE(gcWorkDir +lcTmpIndx+ '.CDX') 
    SELECT APPAYMNT  
    CLOSE INDEX
    ERASE (gcWorkDir +lcTmpIndx+ '.CDX')
  ENDIF  
ENDIF
    
ON ERROR &lcCurErHnd


*!*************************************************************
*! Name      : lfSetfile
*! Developer : Amin Khodary 
*! Date      : 07/08/1999
*! Purpose   : Sets the temp. file and index
*!*************************************************************
*! Called from : REPORTS\APBNKRC.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      :  None
*!*************************************************************
*E301284,1 (Start)
FUNCTION lfSetfile
SELECT APPAYMNT 
IF lcRpType = 'NOBATCH'  
  * Be sure you are using the master PAPAYMNT FILE.
  USE (gcDataDir) + 'APPAYMNT'  ALIAS APPAYMNT IN APPAYMNT
  lcTmpApPay = lfChckNdx(lcRpType)
  INDEX ON cbnkcode+cchkacct+cpaytype+cpaymeth+DTOS(dPayDate) TAG PayDate OF (gcWorkDir  + lcTmpApPay + '.CDX' ) 
ELSE
  lcTmpIndx =lfChckNdx(lcRpType)
  * Be sure you are using the master APAPAYMNT FILE.
  USE (gcDataDir) + 'APPAYMNT'  ALIAS APPAYMNT IN APPAYMNT  
  INDEX ON cbnkcode+cchkacct+batch TAG chkbtch OF (gcWorkDir + lcTmpIndx+ '.CDX') 
  =lfGenTmp()
ENDIF
*E301284,1 (End)




*!*************************************************************
*! Name      : lfWholeTot
*! Developer : Amin Khodary 
*! Date      : 07/08/1999
*! Purpose   : 1) Sum book, cleared and open amount of whole transactions
*!*************************************************************
*! Called from : APBNKRCS.FRX 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      :  None
*!*************************************************************
*B802408,1 AKA (Start)
FUNCTION lfWholeTot
lnAlias = SELECT(0)
STORE 0 TO lnWholCler,lnWholOpen, lnWholBook
lcTotFlt = "CPAYSTAT <> 'V' .AND. CPAYMETH <> 'H'"
USE (gcDataDir+'APPAYMNT') ORDER CHKTMNO AGAIN ALIAS lcTmpPmt IN 0
SELECT lcTmpPmt
IF lcRpType = 'NOBATCH'  
  =SEEK(APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT)
ELSE
  =SEEK(PayTemp.CBNKCODE+PayTemp.CCHKACCT)
ENDIF   

IF lcRpType = 'NOBATCH'  
  lcCondStr  = "CBNKCODE+CCHKACCT = APPAYMNT.CBNKCODE+APPAYMNT.CCHKACCT"
ELSE
  lcCondStr  = "CBNKCODE+CCHKACCT = PayTemp.CBNKCODE+PayTemp.CCHKACCT"
ENDIF 

SCAN REST WHILE &lcCondStr  FOR &lcTotFlt
  *IF lcTmpPmt.cCurrCode <> gcBaseCurr AND lcRpCurr <> 'F'
    * Initialize lnEqvAmnt variable
*	lcExSin2  = ''
*	lcExSin1  = gfGetExSin(@lcExSin2, lcTmpPmt.cCurrCode ) 
*	lnValue   = ROUND( lcTmpPmt.nPayAmnt &lcExSin1 lcTmpPmt.nExRate &lcExSin2 lcTmpPmt.nCurrUnit ,2)
 * ELSE
	lnValue   =  lcTmpPmt.nPayAmnt
  *ENDIF  
  lnWholCler = lnWholCler + IIF(CPAYRECST = 'C', -lnValue , 0  ) 
  lnWholOpen = lnWholOpen + IIF(CPAYRECST = 'O', -lnValue , 0  ) 
  lnWholBook = lnWholBook - lnValue
ENDSCAN

* SUM IIF(CPAYRECST = 'C' ,-NPAYAMNT,0) , 
* IIF(CPAYRECST = 'O' ,-NPAYAMNT,0) , -npayamnt  TO 
* lnWholCler   , lnWholOpen  , lnWholBook   
* WHILE &lcCondStr  FOR &lcTotFlt
USE IN lcTmpPmt
SELECT (lnAlias)
*B802408,1 AKA (End)



*!*************************************************************
*! Name      : lfGetVarPos
*! Developer : Amin Khodary 
*!*************************************************************
*B802408,1 AKA (Start)

FUNCTION lfGetVarPos

PARAMETERS lcVarName , llEnabDis
PRIVATE lnVarPos
lnVarPos = ASCAN(laOGObjType,lcVarName)
IF lnVarPos > 0
  lnVarPos = ASUBSCRIPT(laOGObjType,lnVarPos,1)
  laOGObjCnt[lnVarPos] = llEnabDis
ENDIF
RETURN lnVarPos
