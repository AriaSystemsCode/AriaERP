*!*************************************************************************
*! Created By : Mohamed Hassan Mohamed
*!
*! This function is to validate the accounts from the chart of account
*! of the active company or the chart of account of another company.
*!
*!*************************************************************************
*
*E300824,1 AMM start, Add a new parameter llValidAcs holds the return value.
*B606903,1 KHM 02/22/2003 Fix the bug of enabling the user to select inactive account.
*PARAMETERS lcAcsDes,llBrowse
PARAMETERS lcAcsDes,llBrowse, llValidAcs
*E300824,1 AMM end

** If the parameter is undefined.

lcAcsDes    = IIF(TYPE('lcAcsDes') $ 'UL','',lcAcsDes)

lcSavAlias  = ALIAS()  && Variable to save the selected alias.

llBrowse    = IIF(TYPE('llBrowse') $ 'U',.F.,llBrowse)

llValidAcs  = .T.

lcFieldCont = EVALUATE(SYS(18))   && Assign the content of the field to the variable.

*** Variable hold an empty account to compair with. ***
lcEmptyAcs = REPLICATE('0',lnApsAcLen)

IF llApGlLink .AND. ( llBrowse .OR. lcFieldCont <> lcEmptyAcs)

  SELECT('lcLinkChar')
  SET ORDER TO TAG ACCTCODE

  IF llBrowse .OR. !SEEK(lcFieldCont) .OR. ATC('?',lcFieldCont) > 0
    llBrowse = .F. 
    DIMENSION laTemp[2]
    laTemp = ''
    lcSavBrFld=lcBrfields
    lcSavTitle=lcFile_Ttl

    lcBrfields="CACCTCODE :H= 'Account Code',;
                CACCNLDES :H= 'Account Description'"

    lcFile_Ttl="Chart of accounts"
        
    =gfbrows(' ','CACCTCODE,CACCNLDES','laTemp')

    lcFile_Ttl=lcSavTitle
    lcBrfields=lcSavBrFld

    IF !EMPTY(laTemp[1])
      lcFieldCont = ALLTRIM(laTemp[1])
      lcAcsDes    = ALLTRIM(laTemp[2])
    ELSE
      lcFieldCont = REPLICATE('0',lnApsAcLen)  
      lcAcsDes    = ' '
      llValidAcs  = .F.
    ENDIF
  ELSE
    lcAcsDes      = ALLTRIM(CACCNLDES)
  ENDIF  

  *B606903,1 KHM 02/22/2003 (Begin) Do not select the inactive account
  IF !EOF() AND cSegActiv = "I"
    =gfModalGen("TRM04202B00000","DIALOG",ALLTRIM(lcFieldCont))
    lcFieldCont = REPLICATE('0',lnApsAcLen)  
    lcAcsDes    = ' '
    llValidAcs  = .F.
    _CUROBJ = _CUROBJ
  ENDIF
  *B606903,1 KHM 02/22/2003 (End)

  IF !EMPTY(lcSavAlias)
    SELECT(lcSavAlias)
  ENDIF  
  lcVarName = SYS(18)
  &lcVarName= lcFieldCont
ENDIF

IF !EMPTY(lcSavAlias)
  SELECT(lcSavAlias)
ENDIF  

RETURN llValidAcs
