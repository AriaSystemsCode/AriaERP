*!**************************************************************************
*!
*!      Function: lfBnkChk
*!
*!**************************************************************************
* Valid function for get field laData[43] representing factor
*E300296,4 REN 10/24/95 Add a parameter to include a condition 
*E300296,4              for selecting a bank/checking account with 
*E300296,4              a specific currency.
*B600761,1 REN 10/30/95 Fix the problem of bank/checking account mismatch.
*B800456,1 M.H 02/11/96 Add a new parameter to hold the browse title.

FUNCTION lfBnkChk
*E300824,1 AMM start, Add a new parameter llVldBnk holds the return value.
*PARAMETERS laObjArr, lcOldVal, llBrowse, lcChkState,;
           lcBankAlias, lcCheckAlias, lcCurrCode
PARAMETERS laObjArr, lcOldVal, llBrowse, lcChkState,;
           lcBankAlias, lcCheckAlias, lcCurrCode, llVldBnk
*E300824,1 AMM end
*E300296,4 Add parameter lcCurrCode : currency code to browse for.

*** laObjArr   : pointer to an array holding the bank's object names
*** lcOldVal   : old value of the field
*** llBrowse   : .T. if called from a browsing invisible button
*** lcChkState  : pointer to a variable, returns object display status
***               for bank's related objects. (optional)

*E300824,1 AMM start, Exclude the variable llVldBnk from being private.
*PRIVATE lnCallObj, lnSavTag, laRetVal, llVldBnk, llChgChk, lcCurFltr 
PRIVATE lnCallObj, lnSavTag, laRetVal, llChgChk, lcCurFltr 
*E300824,1 AMM end
*** lnCallObj  : calling object, whether a bank or a check
***             (1 if bank, 2 if checking accounts)
*** laRetVal   : array to hold return values from lfGetExt function
*E300296,4 lcCurFltr : current filter expression

llVldBnk = .T.
*** Get the calling object from the array
lcBankAlias  = IIF(!EMPTY(lcBankAlias) , lcBankAlias ,'APBANKS' )
lcCheckAlias = IIF(!EMPTY(lcCheckAlias), lcCheckAlias,'APCHECKS')

IF TYPE('laObjArr') = 'C'
  FOR lnCallObj = 1 TO ALEN(laObjArr,1)
    laObjArr[lnCallObj, 2] = UPPER(STRTRAN(STRTRAN(laObjArr[lnCallObj, 2],;
                               "[","("),"]",")"))
  ENDFOR 
  lnCallObj = ASCAN(laObjArr, SYS(18))
  lcOldVal  = IIF(TYPE('lcOldVal') <> 'C', " ", lcOldVal)
ELSE
  lnCallObj = 0
ENDIF
IF lnCallObj > 0  .AND. USED(lcBankAlias) .AND. USED(lcCheckAlias) ;
  .AND. (llBrowse .OR. EVALUATE(SYS(18)) <> lcOldVal)
  lnCallObj  = ASUBSCRIPT(laObjArr, lnCallObj, 1)
  DECLARE laRetVal[2]
  laRetVal   = " "
  
  lcCurAlias = ALIAS()

  llChgChk   = .T.     && Checking account is assumed to change
                       && It also reflects if a bank is changed or not
                       && if called from the first case (banks)
  
  *E300296,4 Check passed parameter lcCurrCode
  SELECT (lcCheckAlias)
  lcCurFltr    = FILTER()
  IF !EMPTY(lcCurrCode)
    SET FILTER TO cCurrCode = lcCurrCode
  ELSE
    SET FILTER TO 
  ENDIF  
  *E300296,4 end.
  
  DO CASE
    *** Case called from a bank
    CASE lnCallObj = 1
      *** If a valid bank entry is selected, 
      *E300296,4 Check if a passed bank code has corresponding
      *E300296,4 entries in the filtered CHECKS file (lcCheckAlias).
      *E300296,4 If not, return old values.
      *IF lfGetExt(lcBankAlias, 'BANKCODE', lcOldVal, llBrowse,;
         'cBnkCode', @laRetVal, 'cBnkCode,cBnkLnDes') .AND.;
         SEEK(laRetVal[1], lcCheckAlias)
         
*B800456,1 M.H 02/11/96 Begin.
*      IF lfGetExt(lcBankAlias, 'BANKCODE', lcOldVal, llBrowse,;
        'cBnkCode', @laRetVal, 'cBnkCode,cBnkLnDes') 
      IF lfGetExt(lcBankAlias, 'BANKCODE', lcOldVal, llBrowse,;
        'cBnkCode', @laRetVal, 'cBnkCode,cBnkLnDes','', '', '', 'Banks') 
*B800456,1 M.H 02/11/96 End.
*B600761,1 Position the record pointer on the returned checking account.
        IF SEEK(laRetVal[1], lcCheckAlias)
          &laObjArr[1,2] = laRetVal[1]        && cBnkCode
          *B600761,1 Use parameter check alias instead of APCHECKS
          *&laObjArr[2,2] = APCHECKS.cChkAcct          
          &laObjArr[2,2] = &lcCheckAlias..cChkAcct          
          *B600761,1 end.
        ELSE
          =gfModalGen('INM04155B00000','DIALOG',;
                      ALLTRIM(lcCurrCode) + '|'+;
                      ALLTRIM(&lcBankAlias..cBnkCode)+'-'+ALLTRIM(&lcBankAlias..cBnkShDes))

          *E300296,4 If not, if the current object is not empty, return its
          *E300296,4 old value whatever it is (whether empty or another valid 
          *E300296,4 entry)
          IF !EMPTY(EVALUATE(laObjArr[1,2]))
            &laObjArr[1,2] = lcOldVal
            llChgChk       = .F.
          ELSE
            *E300296,4 Clear objects before display
            &laObjArr[1,2]  = SPACE(FSIZE('cBnkCode', lcBankAlias))
            &laObjArr[2,2]  = SPACE(FSIZE('cChkAcct', lcCheckAlias))
          ENDIF  
        ENDIF 
        *E300296,4 end. 
      ELSE    
        *** If not, if the current object is not empty, return its
        *** old value whatever it is (whether empty or another valid 
        *** entry)
        IF !EMPTY(EVALUATE(laObjArr[1,2]))
          &laObjArr[1,2] = lcOldVal
          llChgChk       = .F.
        ELSE
          *** Clear objects before display
          &laObjArr[1,2]  = SPACE(FSIZE('cBnkCode', lcBankAlias))
          &laObjArr[2,2]  = SPACE(FSIZE('cChkAcct', lcCheckAlias))
        ENDIF  
      ENDIF
      
    *** Case called from a checking account
    CASE lnCallObj = 2
      lcBnkCode  = laObjArr[1,2]
*B800456,1 M.H 02/11/96 Begin.
*      IF lfGetExt(lcCheckAlias, 'BANKCHECK', lcOldVal, llBrowse,;
         'cChkAcct', @laRetVal, 'cBnkCode,cChkAcct,cChkShDes,cChkGlAcc',;
          &lcBnkCode + ALLTRIM(EVALUATE(SYS(18))), lcBnkCode)  
      IF lfGetExt(lcCheckAlias, 'BANKCHECK', lcOldVal, llBrowse,;
         'cChkAcct', @laRetVal, 'cBnkCode,cChkAcct,cChkShDes,cChkGlAcc',;
          &lcBnkCode + ALLTRIM(EVALUATE(SYS(18))), lcBnkCode, '', 'Checking Accounts')
*B800456,1 M.H 02/11/96 End.
        llChgChk   = .T.
        *B600761,1 Use parameter check alias instead of APCHECKS
        *&laObjArr[2,2] = APCHECKS.cChkAcct
        &laObjArr[2,2] = &lcCheckAlias..cChkAcct          
        *B600761,1 end.
      ELSE     
        llChgChk   = .F.
        &laObjArr[2,2] = lcOldVal
      ENDIF
  ENDCASE

  llVldBnk   = !EMPTY(EVALUATE(SYS(18)))   
  lcChkState = IIF(llVldBnk, 'ENABLE', 'DISABLE')
  SHOW GET (laObjArr[1,2]) 
  SHOW GET (laObjArr[2,1]) &lcChkState 
  SHOW GET (laObjArr[2,2]) &lcChkState 

  IF llChgChk
    *** Bank description 
    IF !EMPTY(laObjArr[1,3]) .AND. lnCallObj = 1 
      *B600761,1 Use parameter bank alias instead of APBANKS
      *&laObjArr[1,3]  = IIF(llVldBnk, APBANKS.cBnkShDes, "")
      &laObjArr[1,3]  = IIF(llVldBnk, &lcBankAlias..cBnkShDes, "")
      *B600761,1 end.
      SHOW GET (laObjArr[1,3]) 
    ENDIF  
    *** Checking account description
    IF !EMPTY(laObjArr[2,3]) 
      *B600761,1 Use parameter check alias instead of APCHECKS
      *&laObjArr[2,3]  = IIF(llVldBnk, APCHECKS.cChkShDes, "")      
      &laObjArr[2,3]  = IIF(llVldBnk, &lcCheckAlias..cChkShDes, "")      
      *B600761,1 end.
      SHOW GET (laObjArr[2,3]) 
    ENDIF  
  
    *** If a G/L account entry exists 
    IF ALEN(laObjArr,1) > 2 
      *B600761,1 Use parameter check alias instead of APCHECKS
      *&laObjArr[3,2] = IIF(llVldBnk .AND. !EMPTY(APCHECKS.cChkGlAcc),;
                           APCHECKS.cChkGlAcc, lcEmptyAcc)
      &laObjArr[3,2] = IIF(llVldBnk .AND. !EMPTY(&lcCheckAlias..cChkGlAcc),;
                           &lcCheckAlias..cChkGlAcc, lcEmptyAcc)                           
      *B600761,1 end.                      
      *** G/L account description
      IF !EMPTY(laObjArr[3,3])
        &laObjArr[3,3]  = IIF(llVldBnk .AND. llApGlLink,;
                        ALLTRIM(LOOKUP(lcLinkChar.cAccnLDes,&laObjArr[3,2] ,;
                        lcLinkChar.cAcctCode,"ACCTCODE"))," ")
        SHOW GET (laObjArr[3,3]) 
      ENDIF  
      SHOW GET (laObjArr[3,1]) &lcChkState 
      SHOW GET (laObjArr[3,2]) &lcChkState   
    ENDIF  
  ENDIF 

  *E300296,4 Restore filter in (lcCheckAlias)
  SELECT (lcCheckAlias)
  SET FILTER TO &lcCurFltr
  *E300296,4 end.

  SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
  
  *** If bank is empty (invalid), return to the same object
  IF !llVldBnk
    _CUROBJ = _CUROBJ
    KEYBOARD "{HOME}"
  ENDIF  
ENDIF  
llBrowse = .F.
RETURN llVldBnk
  