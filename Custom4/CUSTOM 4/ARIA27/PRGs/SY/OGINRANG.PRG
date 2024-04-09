*:************************************************************************
*:
*: Procedure file: OGINRANG.PRG
*:
*:         System: ARIA 2.7
*:         Module: SY
*:         Author: Haytham el_Sheltawi
*:      Copyright (c) 
*:  Last modified: 04/22/1998
*:
*:  Procs & Fncts: None
*:
*:  Documented    04/22/1998
*:************************************************************************
*: Passed Parameters : 1) The file that will be used as the In Range
*:                        browse file.
*:                        
*:                     The following parameters are optional
*:                     -------------------------------------      
*:                     2) The In Range browse fields.
*:                     3) The In Range Filter expressions
*:                        [FOR & FOR expressions].
*:                     4) The name of the flag used to clear the user
*:                        selection [If the flag is .T. we are going to
*:                        clear the user selection].
*:                     5) The name of the Set function [The one that we
*:                        use to set the relations and clear theme and
*:                        validate the record before we select it].
*:                     6) Set function Parameters [a string of 3
*:                        characters the 1st. Y or N (If Y we are going
*:                        to call the Set function with Parameter 'S'
*:                        to set the relations before we call gfrange()
*:                        , 2nd. Y or N (If Y we are going to call the
*:                        Set function with Parameter 'R' to release the
*:                        relations after we call gfrange() , 3rd Y or N
*:                        (If Y we are going to use the Set function
*:                        {with Parameter 'V'} as a valid function for
*:                        the select process in the In Range screen)].
*:                     7) The Field that will be returned by the gfrange()
*:                        in the temp. cursor.
*:                     8) The temp. name of the cursor that the gfrange()
*:                        will create.
*E300799,4    Add 2 parameters 
*:                     9) lcSeekFlag : 'Y' : if using SEEK. SCAN REST search
*:                      		       (indexed searh), 'N' otherwise	
*:                     10)lcPict     : Picture for the get fields of the
*:                     				   From, To get fields
*B602931,4    Add a new parameter
*:                     11)lcIndxExpr : this new parameter should hold the 
*:                     				   name of the variable to be filled
*:                     				   with the first part - until the field
*:                     				   that will be returned by the gfrange()
*:                     				   in the temp. cursor. - of index
*:                     				   expression of the active (controlling)
*:                     				   index tag on the "In Range" browse
*:                     				   file.
*:************************************************************************
*: Example			  : =OGInRang(['APVENDOR'], [cVendCode, cVenComp],;
*:                        		  [lcVendCode FOR lVenMisc], 'llClearSet',;
*:                        		  'lfSetRel', 'YYN', 'Vendor', lcTmpCurs)
*:                       Or
*:                       =OGInRang([IIF(llPO, 'POSHDR', 'POFHDR'],;
*:                       	       [Vendor, PO],;
*:                        		  [lcVendCode FOR Status $ 'OH'], 'llClearSet',;
*:                        		  'lfSetRel', 'YYN', 'Vendor', lcTmpCurs) 		  
*:************************************************************************
*E300857,1 This program was coded by HS for the Enhancement
*E300799,4  HS  06/02/98   Add 2 parameters to pass when calling gfRange(),
*E300799,4                 parameter descruption may be found at the
*E300799,4                 header of the program.
*E301170,1 MAB 03/15/1999 detect Changes to OG Filter.
*B602931,4  HS  05/26/1999 Enhance Performance in In Range
*E301302,1  HS  08/01/99  Give the capability to use macro substitution
*E301302,1                in the in range fields.
*:************************************************************************
*

*E300799,4 add 2 new parameters (lcSeekFlag  , lcPict) [Begin]

*PARAMETERS lcBrwFile   , lcBrwFields , lcBrwFltExp , lcClrRngSel ,;
*           lcSetFunc   , lcSetParm   , lcBrwSelFld , lcTmpCursr

*B602931,4 Change this line to add a new parameter, the new parameter should
*          hold the name of the variable to be filled with the first part
*          - until the field that will be returned by the gfrange() in the
*          temp. cursor. - of index expression of the active (controlling)
*          index tag on the "In Range" browse file [Begin]
PARAMETERS lcBrwFile   , lcBrwFields , lcBrwFltExp , lcClrRngSel ,;
           lcSetFunc   , lcSetParm   , lcBrwSelFld , lcTmpCursr  ,;
           lcSeekFlag  , lcPict , lcIndxExpr

*B602931,4 Change this line to add a new parameter [End]

*E300799,4 add 2 new parameters (lcSeekFlag  , lcPict) [End]

*E301170,1 Change OG Filter [Begin
*PRIVATE lnAlias
PRIVATE lnAlias,llChngIt
llChngIt = .F.
*E301170,1 Change OG Filter [End

lnAlias = SELECT(0)              && Save the old alias

*E301302,1 Change these lines to give the capability to use macro
*          substitution in the in range fields [Begin]
*lcBrwFile   = EVALUATE(&lcBrwFile)
*lcBrwFields = IIF(EMPTY(&lcBrwFields) , .F. , &lcBrwFields)
*lcBrwFltExp = IIF(EMPTY(&lcBrwFltExp) , .F. , &lcBrwFltExp)
lcBrwFile   = lfMacroSub(EVALUATE(&lcBrwFile))
lcBrwFields = IIF(EMPTY(&lcBrwFields) , .F. , lfMacroSub(&lcBrwFields))
lcBrwFltExp = IIF(EMPTY(&lcBrwFltExp) , .F. , lfMacroSub(&lcBrwFltExp))
*E301302,1 Change these lines to give the capability to use macro [End]

lcSetFunc   = ALLTRIM(lcSetFunc)

*-- If we are to clear the old user selection
IF !EMPTY(lcClrRngSel) .AND. EVALUATE(lcClrRngSel)
  
  &lcClrRngSel = .F.
  
  *-- If the In Range temp. cursor is opened
  IF USED(lcTmpCursr)
    USE IN &lcTmpCursr
  ENDIF    && End of IF USED(lcTmpCursr)
  
ENDIF    && End of IF !EMPTY(lcClrRngSel) .AND. EVALUATE(lcClrRngSel)

*-- If we are to call the Set function to set the relations
IF !EMPTY(lcSetFunc) .AND. UPPER(SUBSTR(lcSetParm , 1 , 1)) = 'Y'
  =&lcSetFunc.('S')
ENDIF    && End of IF !EMPTY(lcSetFunc) .AND. UPPER(SUBSTR(lcSetParm , 1 , 1)) = 'Y'

SELECT (lcBrwFile)

*-- If we are going to use the Set function as a valid function for
*-- the select process in the In Range screen
IF !EMPTY(lcSetFunc) .AND. UPPER(SUBSTR(lcSetParm , 3 , 1)) = 'Y'
  
  *B602931,4 Add this line to get the first part - until the field that will
  *          be returned by the gfrange() in the temp. cursor. - of index
  *          expression of the active (controlling) index tag on the
  *          "In Range" browse file [Begin]
  
  *-- If the option type is not "Expression"
  IF !EMPTY(lcIndxExpr)
    &lcIndxExpr = SUBSTR(KEY() , 1 , AT(UPPER(ALLTRIM(lcBrwSelFld)) ,;
                         KEY()) - 1)
    
    &lcIndxExpr = SUBSTR(&lcIndxExpr , 1 , RAT('+' , &lcIndxExpr) - 1)
  ENDIF    && End of IF !EMPTY(lcIndxExpr)
  *B602931,4 Add this line to get the index expression of the active [End]
  
  *E300799,4 Change this line to pass 2 new parameters to gfrange() [Begin]
  
  *=gfrange(lcBrwFields , lcTmpCursr , lcBrwSelFld , lcBrwFltExp ,;
  *         lcSetFunc + '("V")')
  
  *E301170,1 MAB 03/15/1999 detect Changes to OG Filter. [Begin]
  *=gfrange(lcBrwFields , lcTmpCursr , lcBrwSelFld , lcBrwFltExp ,;
  *         lcSetFunc + '("V")' , lcSeekFlag  , lcPict)
  llChngIt = gfrange(lcBrwFields , lcTmpCursr , lcBrwSelFld , lcBrwFltExp ,;
             lcSetFunc + '("V")' , lcSeekFlag  , lcPict)
  *E301170,1 MAB 03/15/1999 detect Changes to OG Filter. [End  ]

  
  *E300799,4 Change this line to pass 2 new parameters to gfrange() [End]

ELSE    && Else [If there will be no validation for the select process]
  
  *B602931,4 Add this line to get the first part - until the field that will
  *          be returned by the gfrange() in the temp. cursor. - of index
  *          expression of the active (controlling) index tag on the
  *          "In Range" browse file [Begin]
  
  *-- If the option type is not "Expression"
  IF !EMPTY(lcIndxExpr)
    &lcIndxExpr = SUBSTR(KEY() , 1 , AT(UPPER(ALLTRIM(lcBrwSelFld)) ,;
                        KEY()) - 1)
    
    &lcIndxExpr = SUBSTR(&lcIndxExpr , 1 , RAT('+' , &lcIndxExpr) - 1)
  ENDIF    && End of IF !EMPTY(lcIndxExpr)
  *B602931,4 Add this line to get the index expression of the active [End]
  
  *E300799,4 Change this line to pass 2 new parameters to gfrange() [Begin]
  
  *=gfrange(lcBrwFields , lcTmpCursr , lcBrwSelFld , lcBrwFltExp)
  
  *E301170,1 MAB 03/15/1999 detect Changes to OG Filter. [Begin]
  *=gfrange(lcBrwFields , lcTmpCursr , lcBrwSelFld , lcBrwFltExp , '' ,;
  *         lcSeekFlag  , lcPict)
  llChngIt = gfrange(lcBrwFields , lcTmpCursr , lcBrwSelFld , lcBrwFltExp , '' ,;
             lcSeekFlag  , lcPict)
  *E301170,1 MAB 03/15/1999 detect Changes to OG Filter. [End  ]
  
  *E300799,4 Change this line to pass 2 new parameters to gfrange() [End]
  
ENDIF    && End of IF !EMPTY(lcSetFunc) .AND. UPPER(SUBSTR(lcSetParm , 3 , 1)) = 'Y'

*-- If we are to call the Set function to clear the relations
IF !EMPTY(lcSetFunc) .AND. UPPER(SUBSTR(lcSetParm , 2 , 1)) = 'Y'
  =&lcSetFunc.('R')
ENDIF    && End of IF !EMPTY(lcSetFunc) .AND. UPPER(SUBSTR(lcSetParm , 2 , 1)) = 'Y'

SELECT (lnAlias)              && Restore the old alias

*E301170,1 MAB 03/15/1999 detect Changes to OG Filter. [Begin]
llOGFltCh = IIF(llOGFltCh,llOGFltCh,llChngIt)        
*E301170,1 MAB 03/15/1999 detect Changes to OG Filter. [End  ]

*!*************************************************************
*! Name      : lfMacroSub
*! Developer : Haytham El_Sheltawi
*! Date      : 08/11/1999
*! Purpose   : Function to perform the macro substitution within
*!             any giving string.
*!*************************************************************
*! Called from : lfDefVarPart()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) lcString, Any string with macros
*!                        that need to be substituted.
*!*************************************************************
*! Return      : The string after performing the macro substitution.
*!*************************************************************
*E301302,1 This function was add by HS for the Enhancement.
*!*************************************************************
*
FUNCTION lfMacroSub

PARAMETERS lcString

PRIVATE lcReturStr , lnNoOfMacr , lnCount , lnMacroEnd , lcMacroStr

*-- Get the number of occurrences of the macro character "&" in the string
lnNoOfMacr = OCCURS('&' , lcString)

*-- If the string has no macro character "&" return the string as it is.
IF lnNoOfMacr = 0
  RETURN lcString
ENDIF    && End of IF lnNoOfMacr = 0

lcReturStr = ''        && Variable to hold the returned string

*-- For loop to perform the necessary macro substitution
FOR lnCount = 1 TO lnNoOfMacr
  
  *-- Add the part of the string that is ahead of the macro character "&"
  *-- to the returned string
  lcReturStr = lcReturStr + SUBSTR(lcString , 1 , AT('&' , lcString) - 1)
  
  *-- Remove the part of the string that is ahead of the macro character "&"
  *-- from the original string
  lcString   = SUBSTR(lcString , AT('&' , lcString))
  
  *-- Get the end position of the macro
  lnMacroEnd = MIN(IIF(AT('.' , lcString) = 0 ,;
                       LEN(lcString) , AT('.' , lcString)) ,;
                   IIF(AT(' ' , lcString) = 0 ,;
                       LEN(lcString) , AT(' ' , lcString) - 1) ,;
                   IIF(AT('&' , lcString , 2) = 0 ,;
                       LEN(lcString) , AT('&' , lcString , 2) - 1))
  
  *-- Get the part of the string that belongs to the macro substitution
  lcMacroStr = SUBSTR(lcString , 1 , lnMacroEnd)
  
  *-- Remove the part of the string that belongs to the macro substitution
  *-- from the original string
  lcString   = SUBSTR(lcString , lnMacroEnd + 1)
  
  *-- Perform the macro substitution and add the result to the returned
  *-- string
  lcReturStr = lcReturStr + [&lcMacroStr]
ENDFOR    && End of FOR lnCount = 1 TO lnNoOfMacr

*-- Add any string left from the original string that doesn't have a
*-- macro character "&"
lcReturStr = lcReturStr + lcString

RETURN lcReturStr
