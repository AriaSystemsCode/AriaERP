*:*************************************************************************
*: Program file  : gfrange.prg
*: Program desc. : global range selection browse
*: For Screen    : range
*:        System : SY
*:        Module : SY
*:     Developer : Hesham El-Sheltawi 
*:*************************************************************************
*: Passed parameters  : lcbrowfld   Browse Fields
*:                      lccursor    Cursor name to select into
*:                      lcselfld    Field name to select range to
*:*************************************************************************
*E300857,1  HS  04/22/98  add a new parameter (lcValidFun) that hold the
*E300857,1                name of a validation function to be used to check
*E300857,1                if the record is valid before we select it.
*E300799,4  HS  06/02/98   1) Add 2 new parameters :
*E300799,4                    i) (lcSeekFlag) to know if we are going to use
*E300799,4                       the [SEEK. SCAN WHILE] or the [SCAN FOR]
*E300799,4                       technique with the Select all and Invert
*E300799,4                       push buttons.
*E300799,4                    parameter value :
*E300799,4                       - 'N' Use the SCAN FOR technique.
*E300799,4                       - 'Y' (Or otherwise) Use the
*E300799,4                         [SEEK. SCAN WHILE] technique.
*E300799,4                    ii) (lcPict) Picture for the From , To get
*E300799,4                        fields in the range screen.
*E300799,4                    parameter value :
*E300799,4                       - Picture format [For example "@! XXXXXX"]
*E300799,4                 2) Fix the size of the From and To get fields
*E300799,4                    in the range screen.
*E300799,4                 3) Fix the bug "Data type mismatch".
*B602349,1 RENEE 12/14/98. Sometimes 'Select'ing does not refresh the browse
*B602349,1 				   marker.
*B602686,1  HS   03/18/99  Fix the error "Index does not match table
*B602686,1                 file.  Recreate index."
*B602915,1 MAB 05/25/1999  If the index expression does not start with the transaction number,
*B602915,1                 that the user is selecting, the user will NOT be able to find,
*B602915,1                 a specific transaction number by typing the number and pressing,
*B602915,1                 the ENTER key, also he can not select specific range. 
*B602915,1                 My Modification is to pass all possible keys and lcSeekFlag must 
*B602915,1                 be "N" to locate for transaction when select range.
*B802439,1 ABD 07/20/1999  Fix the bug  " Mising')'  "
*B802267,1 MAB 09/08/1999  File Structure use by all programmers thus variable must be private.
*B606987,1 ASH 02/23/2003 Fix bug "Variable Order not found" in order inrange.
*B607317,1 AMH 06/12/2003  Fix the error "Index does not match table
*B607317,1                 file.  Recreate index."
*:*************************************************************************
*

*E300857,1 Change this line to add a new parameter (lcValidFun) that hold
*the name of a validation function to be used to check if the record is
*valid before we select it [Begin]

*PARAMETERS lcBrowFld,lcCursor,lcSelFld,lcFltrExp

*E300799,4 Change this line to add 2 new parameters [Begin]

*PARAMETERS lcBrowFld , lcCursor , lcSelFld , lcFltrExp , lcValidFun
PARAMETERS lcBrowFld  , lcCursor , lcSelFld , lcFltrExp , lcValidFun ,;
           lcSeekFlag , lcPict

*E300799,4 Change this line to add 2 new parameters [End]

*E300857,1 Change this line to add a new parameter (lcValidFun) [End]
*B606987,1 ASH 02/23/2003 (Begin) Initialize a new variable to hold inrange file.
STORE "" TO lcBrRngFld
*B606987,1 ASH 02/23/2003 (End)
REGIONAL lcNewObj,lcFindExp,llClick,lnCol,lnCount,;
        lnCurR,lnCurRec,lcWinTitl,lnLineRange,laScObj,lnY,lcExpToSeek,;
        lnSeeklimt,lcBrowCnt,lnY1,lnX1,lnY2,lnX2,lckeyexp,;
        lnSoftSeek,lnY,lnX,laFldStruct,lcOrdExpr,lakeyExp,lcFltrExp,lcForExp,;
        lcTmpCurs,llOk,lcCurrKey

*B802267,1 09/08/1999 File Structure use by all programmers [Begin]
PRIVATE laFileStru
*B802267,1 09/08/1999 File Structure use by all programmers [End  ]

*E300857,1 Add this line to add a new parameter (lcValidFun) that hold
*the name of a validation function to be used to check if the record is
*valid before we select it [Begin]

lcValidFun = IIF(TYPE('lcValidFun') <> 'C' .OR. EMPTY(lcValidFun) , '' ,;
                 '.AND. ' + ALLTRIM(lcValidFun))

*E300857,1 Add this line to add a new parameter (lcValidFun) [End]

DIMENSION lakeyExp[1]
STORE '' TO lakeyExp,lcForExp,lckeyexp,lcCurrKey

STORE .F. TO llFromTr        && .t. if user use the up dn page to select 
STORE .F. TO llOk            && .t. if user selected OK button
STORE '' TO lcToRec,lcFrRec  && start record and end record for selection
lcBrowCnt  = SYS(2015)       && control window name
lcWinTitl  = 'BROWWIND'      && browse window name

PRIVATE lnAlias              && the selected alias when entring the program
lnAlias = SELECT()
* check if the cursor name was passed as a parameter then use this cursor
* if not then get a temprory name to use and return it back to the calling
* program
IF TYPE('lcCursor') # 'C'
  lcCursor = 'X' + RIGHT(SYS(3),7)
ENDIF

* check if there is a selection field or not if not then return .f.
IF TYPE('lcSelFld') # 'C' OR EMPTY(lcSelFld)
  RETURN .F.
ENDIF

lcSelFld = UPPER(lcSelFld)
* if the cursor is not initialized then get the structure of the selection 
* field and create cursor with the same structure 
IF !USED(lcCursor)

  DIMENSION laFldStruct[1,4],laFileStru[1,4]
  STORE '' TO laFldStruct,laFileStru
  =AFIELDS(laFileStru)
  lnFldPos = ASCAN(laFileStru,lcSelFld)
  IF lnFldPos = 0
    RETURN .F.
  ENDIF
  lnFldPos = ASUBSCRIPT(laFileStru,lnFldPos,1)
  laFldStruct[1,1] = laFileStru[lnFldPos,1]
  laFldStruct[1,2] = laFileStru[lnFldPos,2]
  laFldStruct[1,3] = laFileStru[lnFldPos,3]
  laFldStruct[1,4] = laFileStru[lnFldPos,4]
  CREATE CURSOR &lcCursor FROM ARRAY laFldStruct
  
  *B602686,1 Change this line to use the full path when creating the index
  *          to fix the error "Index does not match table
  *          file.  Recreate index." [Begin]
  *INDEX ON &lcSelFld TAG &lcSelFld OF (lcCursor)
  
  *B607317,1 AMH Set full path before get the cursor location [Start]
  lcFullPath = SET('FULLPATH')
  SET FULLPATH ON
  lcCDXPath = STRTRAN(DBF(lcCursor) , '.TMP' , '.CDX')
  SET FULLPATH &lcFullPath.
  *03/24/2003
  *INDEX ON &lcSelFld TAG &lcSelFld OF (lcCDXPath)
  *INDEX ON &lcSelFld TAG &lcSelFld OF (gcWorkDir+STRTRAN(lcCursor,".TMP")+'.CDX')
  INDEX ON &lcSelFld TAG &lcSelFld OF (lcCDXPath)
  *B607317,1 AMH [End]
  
  *B602686,1 Change this line to use the full path when creating [End]
ENDIF
lcTmpCurs =  'X' + RIGHT(SYS(3),7)

SELECT (lcCursor)
DIMENSION laFldStruct[1,4]

*E300799,4 Add this line to Place the temp. file (lcCursor) structure
*information into the array (laFldStruct) for we are going to use
*laFldStruct[1,3] to calculate the From , To get fields sizes
*(the "Data type mismatch" bug) [Begin]

=AFIELDS(laFldStruct)

*E300799,4 Add this line to Place the temp. file (lcCursor) structure [End]

=AFIELDS(laFileStru)
CREATE CURSOR &lcTmpCurs FROM ARRAY laFileStru
=lfAppend(lcCursor,lcTmpCurs)
SELECT (lcTmpCurs)

*B602686,1 Change this line to use the full path when creating the index
*          to fix the error "Index does not match table
*          file.  Recreate index." [Begin]
*INDEX ON &lcSelFld TAG &lcSelFld OF (lcTmpCurs)

*B607317,1 AMH Set full path before get the cursor location [Start]
lcFullPath = SET('FULLPATH')
SET FULLPATH ON
lcCDXPath = STRTRAN(DBF(lcTmpCurs) , '.TMP' , '.CDX')
SET FULLPATH &lcFullPath.
*03/24/2003
*INDEX ON &lcSelFld TAG &lcSelFld OF (lcCDXPath)
*INDEX ON &lcSelFld TAG &lcSelFld OF (gcWorkDir+STRTRAN(lcTmpCurs,".TMP")+'.CDX')
INDEX ON &lcSelFld TAG &lcSelFld OF (lcCDXPath)
*B607317,1 AMH [End]

*B602686,1 Change this line to use the full path when creating [End]

lcSwap = lcCursor
lcCursor = lcTmpCurs
lcTmpCurs = lcSwap

*E300799,4 Add this line to add 2 new parameters [Begin]

lcSeekFlag = IIF(TYPE('lcSeekFlag') <> 'C' , 'Y' , UPPER(lcSeekFlag))
lcPict     = IIF(TYPE('lcPict') <> 'C' .OR. EMPTY(lcPict) ,;
                 '@! ' + REPLICATE('X' , laFldStruct[1,3]) , lcPict)

*E300799,4 Add this line to add 2 new parameters [End]

* if there is a browse fields passed then add a selection indicator to 
* the browse fields 
* if there is no browse fields then add a selection indicator to the selection
* field and assume that the selection field is the browse fields

IF TYPE('lcBrowFld') # 'C'
  lcBrowFld = [cMarkFld=IIF(SEEK(]+lcSelFld+[,lcCursor),'»',''):H='':2,]+lcSelFld
ELSE
  lcBrowFld = [cMarkFld=IIF(SEEK(]+lcSelFld+[,lcCursor),'»',''):H='':2,]+lcBrowFld
ENDIF

SELECT (lnAlias)
IF TYPE('lcFltrExp')='C'
  lcForExp=IIF(ATC('FOR ',lcFltrExp)>0,SUBSTR(lcFltrExp,ATC('FOR ',lcFltrExp)),'')
ENDIF  

lcFltrExp=IIF(!EMPTY(lcForExp),SUBSTR(lcFltrExp,1,ATC(lcForExp,lcFltrExp)-1),lcFltrExp)        
lcKeyExp = IIF(TYPE('lcFltrExp')='C' AND !EMPTY(lcFltrExp),lcFltrExp,'')
=gfSubStr(lcKeyExp,@laKeyExp,',')

*B602915,1 Sort array to find First and Last key to Key selection between them [Begin]
*B602915,1 Then Create KEY Expression.
=ASORT(laKeyExp)
IF EMPTY(laKeyExp[1]) OR EMPTY(EVALUATE(laKeyExp[1]))
  lcCurrKey =  ""
  IF !EMPTY(laKeyExp[1])
    =ADEL(laKeyExp,1)
    IF ALEN(laKeyExp,1) > 1
      DIMENSION laKeyExp[ALEN(laKeyExp,1) - 1]
    ENDIF  
  ENDIF  
  
*B802439,1   If The Array Is Empty move To Be  = ''    [ Begin ]
  IF EMPTY(laKeyexp[1])
    laKeyExp = ""
  ENDIF
*B802439,1  [ END ]  
ELSE
  lcCurrKey = laKeyExp[1] + IIF(ALEN(laKeyExp,1) > 1, ',' + laKeyExp[ALEN(laKeyExp,1)],'')
ENDIF
*B602915,1 Comment out the following line and evaluate key from key expression 
*B602915,1 Now programmer can write more than two keys and replace cIndxSrch field with "N".
*lcKeyExp = IIF(TYPE('lcFltrExp')='C' AND !EMPTY(lcFltrExp),"KEY "+lcFltrExp,'')
lcKeyExp = IIF(EMPTY(lcCurrKey),lcCurrKey,"KEY "+lcCurrKey)

*B602915,1 Sort array to find First and Last key to Key selection between them [End  ]

lnLineRange=IIF(_WINDOWS,1.77,2)  && the line height for the dbl click
lnTimelimt = 0                    && time limit for the dbl click
lnCol      = 0                    && column position of the mouse
lnCount    = 1                    && Acumelator variable
lnCurR     = MROW()               && Row postion of the mouse
lnCurRec = 0                      && the current record
llClick    =.F.                   && logical variable to control the dbl click selection

* call the range screen
DO range.spx
ON KEY 
IF llOk
  USE IN (lcTmpCurs)
  SELECT (lcCursor)
  DIMENSION laFldStruct[1,4]
  =AFIELDS(laFileStru)
  CREATE CURSOR &lcTmpCurs FROM ARRAY laFileStru
  =lfAppend(lcCursor,lcTmpCurs)
  SELECT (lcTmpCurs)
  
  *B602686,1 Change this line to use the full path when creating the index
  *          to fix the error "Index does not match table
  *          file.  Recreate index." [Begin]
  *INDEX ON &lcSelFld TAG &lcSelFld OF (lcTmpCurs)
  
  *B607317,1 AMH Set full path before get the cursor location [Start]
  lcFullPath = SET('FULLPATH')
  SET FULLPATH ON
  lcCDXPath = STRTRAN(DBF(lcTmpCurs) , '.TMP' , '.CDX')
  SET FULLPATH &lcFullPath.
  *03/24/2003
  *INDEX ON &lcSelFld TAG &lcSelFld OF (lcCDXPath)
  *INDEX ON &lcSelFld TAG &lcSelFld OF (gcWorkDir+STRTRAN(lcTmpCurs,".TMP")+'.CDX')
  INDEX ON &lcSelFld TAG &lcSelFld OF (lcCDXPath)
  *B607317,1 AMH [End]
  
  *B602686,1 Change this line to use the full path when creating [End]
  
ENDIF

USE IN (lcCursor)
SELECT (lnAlias)
CLEAR TYPEAHEAD
RETURN llOk

*!*************************************************************************
*! Name      : LFBROWSE
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : display the browse in the window set
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfBrowse
*B606987,1 ASH 02/23/2003 (Begin) Assign Current alias to a variable.
lcBrRngFld = ALIAS()
*B606987,1 ASH 02/23/2003 (End)

BROWSE FIELDS &lcBrowFld WINDOW range1 IN window gwRange;
NOAPPEND;
NOEDIT;
NODELETE;
NOCLEAR;
NOWAIT;
NOMENU;
TITLE lcWinTitl;
WHEN lfwBrowse();
VALID :F lfvBrowse();
&lcKeyExp &lcForExp;
COLOR SCHEME 10 FONT "MS Sans Serif", 9



*!*************************************************************************
*! Name      : LFVSELECT
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : select ,unselect records and invert
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfWBrowse
*!*************************************************************************      
*: Passed parameters  : lcselall A ALL,I Invert,N None
*:*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvSelect
PARAMETERS lcSelAll
PRIVATE lcValue,lcDelete

*B602349,1 Get PARAMETERS() value at the beginning of the function
PRIVATE llBrowRef 
llBrowRef = PARAMETERS()=0
*B602349,1 end

lcSelAll = IIF(TYPE('lcSelAll')#'C','I',lcSelAll)
CLEAR TYPEAHEAD
* get the value of the current record
*B606987,1 ASH 02/23/2003 (Begin) Comment this line and assign it after checking the current alias.
*lcValue = EVAL(lcSelFld)
*B606987,1 ASH 02/23/2003 (End)
lnAlias = SELECT()
*B606987,1 ASH 02/23/2003 (Begin) Check if the current alias is the browse file or not.
IF ALIAS() <> lcBrRngFld
  SELECT (lcBrRngFld)
ENDIF
lcValue = EVAL(lcSelFld)
*B606987,1 ASH 02/23/2003 (End)

SELECT (lcCursor)
* if the value was previously selected
IF SEEK(lcValue,lcCursor)
* if the selection is for all records do not deleted the selected record
* if invert selection or select none then delete the record
  IF lcSelAll <> 'A'
    REPLACE &lcSelFld WITH ''
    DELETE
  ENDIF
ELSE
* if the value is selected for the first time
* then check if we are selecting none do nothing else
* add the record to the temprary cursor
  
  *E300857,1 Change this line to add a valid function [Begin]
  *IF lcSelAll <> 'N'
  IF lcSelAll <> 'N' &lcValidFun
  *E300857,1 Change this line to add a valid function [Begin]
    
    lcDelete = SET('DELETE')
    SET DELETE OFF
    IF SEEK(' ')
      RECALL
      REPLACE &lcSelFld WITH lcValue
    ELSE
      INSERT INTO (lcCursor) (&lcSelFld) VALUES (lcValue)
    ENDIF
    SET DELETE &lcDelete
  ENDIF
ENDIF
SELECT (lnAlias)
* if the function was called while the browse window on top do nothing
* else refresh the browse window and make it the active window
*B602349,1 Use llBrowRef instead of the PARAMETERS() statement because,
*B602349,1 if the 'Select' button performs a validation function, the latter's
*B602349,1 parameters are reflected, and not those of the lfvSelect function.
*IF PARAMETERS()=0
IF llBrowRef
*B602349,1 end
  =lfwBrowse()
  IF WONTOP() <> lcWinTitl
    KEYBOARD "{ALT+B}"
  ELSE
    SHOW WINDOW (lcWinTitl) Refresh Same
    =lfWBrowse()
  ENDIF
ENDIF

*!*************************************************************************
*! Name      : LFVSELALL
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : function to select all records
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvSelAll
CLEAR TYPEAHEAD
=lfSetRange(' ','Z','A')
IF .F.
lnCurRec = RECNO()
GO TOP
* scan through the file and call the valid funciton of the select push button
* with parameter that we are selecting all record so it dont invert the
* previously selected records
SCAN FOR lfvSelect('A')
ENDSCAN
GO lnCurRec
ENDIF
* activate the browse window
IF EOF() AND !BOF()
  SKIP -1
ENDIF
=lfwBrowse()
KEYBOARD "{ALT+B}"


*!*************************************************************************
*! Name      : LFVSELNONE
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : function to select none
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvSelNone
PRIVATE lcSafty
CLEAR TYPEAHEAD
lnAlias = SELECT()
* Select the temprory cursor & zap it
SELECT (lcCursor)
lcSafty = SET('SAFETY')
SET SAFETY OFF
ZAP
SET SAFETY &lcSafty
SELECT (lnAlias)
* make the browse window the active window
=lfwBrowse()
KEYBOARD "{ALT+B}"


*!*************************************************************************
*! Name      : LFVINVERT
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : function to invert the selection 
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvInvert
CLEAR TYPEAHEAD
IF .F.
lnCurRec = RECNO()
GO TOP
* scan through the temprory cursor and call the select function with
* invert as a paramter
SCAN FOR lfvSelect('I')
ENDSCAN
GO lnCurRec
ENDIF
=lfSetRange(' ','Z','I')
=lfwBrowse()
KEYBOARD "{ALT+B}"

*!*************************************************************************
*! Name      : LFVRANGE
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : function to select range of records
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfSetRange
*:            FUNCTION : lfwBrowse
*!*************************************************************************      
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvRange
CLEAR TYPEAHEAD
lcScope = ""

lcRangeFr = ''
lcRangeTo = ''
* call the get range screen to make the user enter the records s/he want
* to select
DO GETRANGE.SPR
IF !EMPTY(lcScope)
  =lfSetRange(lcRangeFr,lcRangeTo,lcScope)
ENDIF
=lfwBrowse()
KEYBOARD "{ALT+B}"


*!*************************************************************************
*! Name      : LFSETRANGE
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : function to select range of records
*!*************************************************************************
*: Passed parameters  : lcrangefr
*:                      lcrangeto
*:                      lcselcrit
*:*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfSetRange

PARAMETERS lcRangeFr,lcRangeTo,lcSelCrit

*E300799,4 Change this line to add new variables lcKeyVal , lnRecNo [Begin]

*PRIVATE lcSeekExp
PRIVATE lcSeekExp , lcKeyVal , lnRecNo

*E300799,4 Change this line to add new variables lcKeyVal , lnRecNo [END]

lcSeekExp = SYS(14,EVAL(SYS(21)))
lcSeekExp = IIF(EMPTY(lcSeekExp),lcSelFld,lcSeekExp)
lcRangeFr = ALLT(lcRangeFr)
lcRangeTo = ALLT(lcRangeTo)
IF !EMPTY(lcRangeFr+lcRangeTo)
  IF lcRangeFr > lcRangeTo
    lcSwap = lcRangeFr
    lcRangeFr = lcRangeTo
    lcRangeTo = lcSwap
  ENDIF
  
  *E300799,4 Remove these lines to add a new parameter (lcSeekFlag) [Begin]
  
  *lcRangeFr = IIF(TYPE('lcFltrExp') # 'C' .OR. EMPTY(lcFltrExp) ,;
  *                 lcRangeFr , EVALUATE(lcFltrExp) + lcRangeFr)
  *
  *lcRangeTo = IIF(TYPE('lcFltrExp') # 'C' .OR. EMPTY(lcFltrExp) ,;
  *                 lcRangeTo , EVALUATE(lcFltrExp) + lcRangeTo)  
  
  *E300799,4 Remove these lines to add a new parameter (lcSeekFlag) [End]
  
  *E300799,4 Variable to hold the key value

  *B602915,1 if you have more than one item in array scan only [Begin]
  *B602915,1 note: when you have empty key value you do Otherwise code block 
  *lcKeyVal = IIF(TYPE('lcFltrExp') # 'C' .OR. EMPTY(lcFltrExp) ,;
  *               '' , EVALUATE(lcFltrExp))
  lcKeyVal = IIF(TYPE('lcFltrExp') # 'C' .OR. EMPTY(lcFltrExp) OR ALEN(lakeyExp,1) > 1 ,;
                 '' , EVALUATE(lcFltrExp))

  *B602915,1 if you have more than one item in array scan only [End  ]

  *E300799,4 Change these lines to add a new parameter (lcSeekFlag) [Begin]
  
  *SEEK (lcRangeFr)
  *SCAN REST WHILE BETWEEN(&lcSeekExp,lcRangeFr,lcRangeTo) FOR lfvSelect(lcSelCrit)
  *ENDSCAN
  
  lnRecNo = IIF(EOF() , 0 , RECNO())          && Save the record pointer
  
  DO CASE
    *E300799,4 In case of using SEEK & SCAN WHILE
    CASE !(lcSeekFlag == 'N')
      
      *E300799,4 Searche for the from value using soft seek
      IF !SEEK(lcKeyVal + lcRangeFr)
        lnCloseRec = RECNO(0)
        IF lnCloseRec <> 0
          GO lnCloseRec
        ENDIF
      ENDIF
      SCAN REST WHILE BETWEEN(&lcSeekExp , lcKeyVal + lcRangeFr ,;
                              lcKeyVal + lcRangeTo);
                  FOR lfvSelect(lcSelCrit)
      
      ENDSCAN
    
    *E300799,4 In case of browsing with key expression
    CASE !EMPTY(lcKeyVal)
      SEEK lcKeyVal
      SCAN REST WHILE &lcSeekExp = lcKeyVal;
                  FOR BETWEEN(&lcSelFld , lcRangeFr , lcRangeTo);
                      .AND. lfvSelect(lcSelCrit)
      
      ENDSCAN
    
    *E300799,4 Otherwise
    OTHERWISE

      *B602915,1 if you have more than one item in key array [Begin]
      IF ALEN(lakeyExp,1) > 1
        PRIVATE lnJ
        FOR lnJ = 1 TO ALEN(lakeyExp,1)
          IF SEEK(&lakeyExp[lnJ])
            EXIT
          ENDIF
        ENDFOR

        IF EOF()
          GO TOP
        ENDIF

        SCAN REST WHILE .T. FOR BETWEEN(&lcSelFld , lcRangeFr , lcRangeTo);
                 .AND. lfvSelect(lcSelCrit)

        ENDSCAN         
      *B602915,1 if you have more than one item in key array [Begin]

      ELSE  && The following else is the original code
      
        SCAN FOR BETWEEN(&lcSelFld , lcRangeFr , lcRangeTo);
                 .AND. lfvSelect(lcSelCrit)
      
        ENDSCAN
      ENDIF  
  
  ENDCASE
  
  *E300799,4 Restore the old record pointer
  IF lnRecNo <> 0
    GO lnRecNo
  ELSE
    GO BOTTOM
  ENDIF
  *E300799,4 Change these lines to add a new parameter (lcSeekFlag) [End]
  
ENDIF

*!*************************************************************************
*! Name      : LFGETRANGE
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : traping function for the selection
*!             this function is fired whenever the use press any char key 
*!             while s\he was in the browse window
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfvRange
*!*************************************************************************      
*! Returns   :  
*:*************************************************************************     
FUNCTION lfGetRange
CLEAR TYPEAHEAD
ON KEY
ACTIVATE WINDOW (lcBrowCnt)
_CUROBJ = OBJNUM(pbRange)
KEYBOARD UPPER(CHR(LASTKEY())) CLEAR PLAIN
=lfvRange()



*!*************************************************************************
*! Name      : LFVCANCEL
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : cancel function
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvCancel
ACTIVATE WINDOW (lcBrowCnt)
_CUROBJ = OBJNUM(pbCancel)
KEYBOARD "{ENTER}" CLEAR PLAIN



*!*************************************************************************
*! Name      : LFACTWIND
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : read activate function
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfActWind
* if the browse window is not the window on top then clear the trapping
*IF WONTOP(lcBrowCnt)
*  ON KEY
*  ON KEY LABEL ESC DO lfvcancel
*ENDIF
IF WONTOP(lcWinTitl)
  =lfTrapKey()
ELSE  
  ON KEY
ENDIF


*!*************************************************************************
*! Name      : LFDEACTWIN
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : read deactivate function
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfDeactWin
* if the master window is not visible then the user is trying to close the 
* window from the upper left corner so terminate the read
IF !WVISIBLE('gwrange')
  ON KEY
  CLEAR READ
  RETURN .T.
ENDIF
* if the control window is the window on top then clear the trapping else
* if the browse window is the window on top the trapp the keyboard
IF WONTOP(lcWinTitl)
  =lfTrapKey()
ELSE
  ON KEY
ENDIF
RETURN .F.




*!*************************************************************************
*! Name      : LFTRAPKEY
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : keyboard trapping initialize function
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfDclick
*:            FUNCTION : lfvCancel
*:            FUNCTION : lpBrowTrap
*:            FUNCTION : lfvSelect
*:            FUNCTION : lfGetRange
*:            FUNCTION : lfUpDn
*:            FUNCTION : lfpgUpDn
*!*************************************************************************      
*! Returns   :  
*:*************************************************************************     
FUNCTION lfTrapKey
CLEAR TYPEAHEAD
ON KEY
lnCurRec = IIF(RECNO()<=RECCOUNT(),RECNO(),1)
lcSeekExp= ""
ON KEY LABEL CTRL+W     lnDummy = 1
ON KEY LABEL Ctrl+Q     lnDummy = 1
ON KEY LABEL CTRL+HOME  GO TOP
ON KEY LABEL CTRL+END   GO BOTT

ON KEY LABEL LEFTMOUSE  DO lfDclick WITH CHR(13)
ON KEY LABEL ESC DO lfvCancel

ON KEY LABEL TAB     DO lpBrowTrap WITH 1
ON KEY LABEL BACKTAB DO lpBrowTrap WITH 2

*ON KEY LABEL CTRL+G     DO lfvGoTo
*ON KEY LABEL CTRL+F     DO lfvLocate

ON KEY LABEL ENTER DO lfvSelect

ON KEY LABEL CTRL+S DO lfvSelect
ON KEY LABEL CTRL+A DO lfDoFunc WITH 'lfvSelAll'
ON KEY LABEL CTRL+N DO lfDoFunc WITH 'lfvSelNone'
ON KEY LABEL CTRL+I DO lfDoFunc WITH 'LFVINVERT'
ON KEY LABEL CTRL+R DO lfGetRange
ON KEY LABEL CTRL+C KEYBOARD "{ESC}"
ON KEY LABEL CTRL+O DO lfvOk
on key label CTRL+ENTER DO lfvOk

*ON KEY = 10 DO lfvOk

lcExpToSeek=""
lcOrdExpr=""

IF SYS(21)<>"0"
  lcOrdExpr   = SYS(14,EVAL(SYS(21)))
  lcOrdExpT   = TYPE(lcOrdExpr)
  lnStartTrap = IIF(lcOrdExpT='N',48,32)
  lnEndTrap   = IIF(lcOrdExpT='N',57,126)
  FOR lnChrToTrap = lnStartTrap TO lnEndTrap
    ON KEY LABEL (CHR(lnChrToTrap)) DO lfIncSearch &&lfGetRange
  ENDFOR
ENDIF

ON KEY LABEL SPACEBAR DO lfvSelect
ON KEY LABEL CTRL+UPARROW DO lfUpDn WITH 'UP'
ON KEY LABEL CTRL+DNARROW DO lfUpDn WITH 'DN'
ON KEY LABEL Ctrl+PgUp	DO lfpgUpDn WITH 'UP'
ON KEY LABEL Ctrl+PgDN	DO lfpgUpDn WITH 'DN'

*!*************************************************************************
*! Name      : LFUPDN
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : trapping function for ctrl+uparrow ctrl+dnarrow
*!*************************************************************************
*: Passed parameters  : lcupdn
*:*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfUpDn
PARAMETERS lcUpDn
KEYBOARD "{SPACEBAR}"
IF lcUpDn = 'UP'
  KEYBOARD "{UPARROW}" CLEAR PLAIN
ELSE
  KEYBOARD "{DNARROW}" CLEAR PLAIN
ENDIF


*!*************************************************************************
*! Name      : LFPGUPDN
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : trapping function for ctrl+page up ctrl+page down
*!*************************************************************************
*: Passed parameters  : lcupdn
*:*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfpgUpDn
PARAMETERS lcUpDn
lnCurRec = RECNO()
lcToRec = ""
llFromTr = .T.
IF lcUpDn = 'UP'
  KEYBOARD "{PGUP}"
ELSE
  KEYBOARD "{PGDN}"
ENDIF


*!*************************************************************************
*! Name      : LFDCLICK
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : trapping function for the leftmouse
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfGetObj
*!*************************************************************************      
*: Passed parameters  : lckeylable
*:*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfDclick
PARAMETERS lcKeyLable

CLEAR TYPEAHEAD
lnX =INT(MROW())  && get the mouse row
lnY =INT(MCOL())  && get the mouse column
* if the mouse click is inside the browse window 
IF  (MROW(lcWinTitl)<>-1) AND (MCOL(lcWinTitl)<>-1) AND (MROW(lcWinTitl)>lnLineRange);
      AND BETWEEN(MCOL(lcWinTitl),2,WCOL(lcWinTitl)-IIF(_DOS OR _UNIX,2,1.77));
     AND BETWEEN(MROW(lcWinTitl),2,WROWS(lcWinTitl)-IIF(_DOS OR _UNIX,2,1.77))
  * it is the first click
  IF !llClick
    lnTimelimt = SECONDS()
    lnCurR     = lnX
    llClick    = .T.
  ELSE
  * if it was the second click then check if it is in the double click time
  * range or not if it is the select the record 
    IF SECONDS() < lnTimelimt + _DBLCLICK .AND. lnCurR  = INT(MROW());
       AND  lnX=lnCurR &&INT(ABS(MROW(lcBrowTtl)-lnCurR))=0
      llClick    = .F.
      KEYBOARD lcKeyLable CLEAR
    ENDIF
    IF lnCurR    = INT(MROW())
      lnTimelimt = SECONDS()
    ELSE
      llClick    = .F.
    ENDIF
  ENDIF
ELSE
  * if the click was outside the browse window then check if the user is selecting
  * any object from the control window
  llClick    = .F.
  ON KEY
  lcObjName=lfGetObj(lnX,lnY)
  CLEAR TYPEAHEAD
  * if the use is selecting an object from the control window the make
  * this object the current object and press it to go to the validation function
  * of this object
 IF !EMPTY(lcObjName) AND UPPER(lcObjName)<>"INVDUMY" &&AND TYPE(EVAL(lcObjName))!='U'
    ACTIVATE WINDOW (lcBrowCnt)
    _CUROBJ=OBJNUM(&lcObjName)
    CLEAR TYPEAHEAD
    KEYBOARD "{ENTER}" PLAIN CLEAR
  ELSE
    CLEAR TYPEAHEAD
  ENDIF
ENDIF


*!*************************************************************************
*! Name      : LFGETOBJ
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : function to get the object name from its corrdinates
*!*************************************************************************
*: Passed parameters  : lnx  row corrdinate
*:                      lny  column corrdinate
*:*************************************************************************
*! Returns   :     object name that has the same corrdinate like the par.
*:*************************************************************************     
FUNCTION lfGetObj
PARAMETER lnX,lnY
lcObj=''
* loop through the screen object array and check for the first object that
* it's cordinates is the passed conrrdinate to the function
FOR lnCount=1 TO ALEN(laScObj,1)
  IF BETWEEN(lnX,laScObj[lnCount,1],laScObj[lnCount,3]) AND BETWEEN(lnY,laScObj[lnCount,2],laScObj[lnCount,4])
    lcObj=laScObj[lnCount,5]
    EXIT
  ENDIF
ENDFOR
RETURN lcObj



*!*************************************************************************
*! Name      : LPBROWTRAP
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : trapping function for the tab and shift+tab keys
*!*************************************************************************
*: Passed parameters  : lncaseswicth
*:*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lpBrowTrap
PARAMETERS lnCaseSwicth

PUSH KEY CLEAR
ON KEY


IF !WREAD(lcbrowcnt)
  POP KEY   
  RETURN
ENDIF
lcVarNam = VARREAD()

IF EMPTY(lcVarNam)
  POP KEY   
  RETURN
ENDIF


DO CASE
  * if tab key was pressed then make the current object the select button
  CASE lnCaseSwich = 1
    DO CASE
      CASE WONTOP() = lcWinTitl
        ACTIV WINDOW (lcBrowCnt)
      _CUROBJ = 2
     CASE !EMPTY(lcVarNam) AND OBJNUM(&lcVarNam) = OBJNUM(pbOk)
       KEYBOARD "{ALT+B}"
       *ACTIV WINDOW (lcWinTitl)
     OTHERWISE
       KEYBOARD "{TAB}" PLAIN CLEAR
     ENDCASE
  * if shift+tab key was pressed then make the current object the select button
  CASE lnCaseSwich = 2
    DO CASE
      CASE WONTOP() = lcWinTitl
        ACTIV WINDOW (lcBrowCnt)
      _CUROBJ = OBJNUM(pbOk)
     CASE !EMPTY(lcVarNam) AND OBJNUM(&lcVarNam) = 2
       KEYBOARD "{ALT+B}"
     OTHERWISE
       KEYBOARD "{BACKTAB}" PLAIN CLEAR
     ENDCASE
ENDCASE

POP KEY



*!*************************************************************************
*! Name      : LFWBROWSE
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : when function for the browse
*!*************************************************************************
*: Calls       :
*:            FUNCTION : lfSetRange
*!*************************************************************************      
*! Returns   :  
*:*************************************************************************     
FUNCTION lfwBrowse
* if coming from tapping the page up and page down
* then get the range of records and select this range
IF llFromTr
  llFromTr = .F.
  lcToRec = EVAL(lcSelFld)
  GO lnCurRec
  lcFrRec = EVAL(lcSelFld)
  =lfSetRange(lcFrRec,lcToRec,'I')
  SHOW WINDOW (lcWinTitl) REFRESH SAME
ENDIF
* if the current record is selected then change the prompt of the push
* button from select to unselect
*B606987,1 ASH 02/23/2003 (Begin) Check if the current alias is the browse file or not.
IF ALIAS() <> lcBrRngFld
  SELECT (lcBrRngFld)
ENDIF
*B606987,1 ASH 02/23/2003 (End)
IF SEEK(EVAL(lcSelFld),lcCursor)
  SHOW GET pbSelect,1 PROMPT "Un\<Select"
ELSE
  SHOW GET pbSelect,1 PROMPT "\<Select"
ENDIF
IF WONTOP(lcWinTitl)
  =lfTrapKey()
ENDIF


*!*************************************************************************
*! Name      : LFVBROWSE
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : valid function for the browse
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfvBrowse
* if the master window is invisble then terminate the read
IF !WVISIBLE('gwrange')
  ACTI WINDOW GWRANGE
  ACTIV WINDOW (lcBrowCnt)
  _CUROBJ = OBJNUM(PBCANCEL)
  KEYBOARD "{ENTER}" CLEAR PLAIN
ENDIF


*!*************************************************************************
*! Name      : lfIncSearch
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : Incremental search function
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfIncSearch
* clear trapping
ON KEY
ON KEY LABEL F1 llDumi = .T.

PRIVATE lnBrRecNO,lcOrgTag

CLEAR TYPEAHEAD
* get the key was pressed
lcExpToSeek = UPPER(CHR(LASTKEY()))

* if the key was pressed is one of "esc,enter,tab,shift+tab" do nothing
* else run the incremental search screen
IF !INLIST(LASTKEY(),27,13,9,15)
    DEFINE WINDOW lwIncSrch ;
      AT 21.000, 10.000 ;
      SIZE 4.167,55.444 ;
      FONT "FoxFont", 9 ;
      STYLE "B" TITLE "Aria Apparel System" ;
      FLOAT NOCLOSE SHADOW NOMINIMIZE NONE COLOR RGB(,,,192,192,192)

    ACTIVATE WINDOW lwIncSrch NOSHOW

  CLEAR TYPEAHEAD
  KEYBOARD "{END}"  && go to the end of the edit field
  DO lfActInSr
ENDIF
ON KEY LABEL F1
=lfDeActWin() && reinitialize the trapping


*!*************************************************************************
*! Name      : lfActInSr
*! Developer : Hesham El-Sheltawi 
*! Date      : 02/10/98    
*! Purpose   : function to get the value of the inc. search and seek in file
*!*************************************************************************
*! Returns   :  
*:*************************************************************************     
FUNCTION lfActInSr
* initialize the get field
@ 1.500,14.333 GET lcExpToSeek ;
  SIZE 1.167,41.000 PICTURE '@!' FONT "FoxFont", 9 MESSAGE "Press  'ENTER'  To  Locate"
	
@ 1.334,14.111 TO 1.334,51.115 PEN 2, 8 STYLE "1" ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 2.667,14.222 TO 2.667,51.115 PEN 2, 8 STYLE "1" ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 1.417,14.111 TO 2.750,14.111 PEN 2, 8 ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 1.417,50.893 TO 2.750,50.893 PEN 2, 8 ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 1.417,4.333 SAY "Locate         :"  FONT "MS Sans Serif", 8 ;
  STYLE "B"
	
@ 0.000,0.000 TO 0.000,55.444 PEN 1, 8 STYLE "1" ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.333,0.444 TO 0.333,55.000 PEN 1, 8 STYLE "1" ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 4.083,0.000 TO 4.083,55.444 PEN 1, 8 STYLE "1" ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 3.750,0.444 TO 3.750,55.000 PEN 1, 8 STYLE "1" ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.000,0.000 TO 4.167,0.000 PEN 1, 8 ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.333,0.444 TO 3.833,0.444 PEN 1, 8 ;
  COLOR RGB(128,128,128,128,128,128)
	
@ 0.333,54.889 TO 3.833,54.889 PEN 1, 8 ;
  COLOR RGB(255,255,255,255,255,255)
	
@ 0.000,55.333 TO 4.167,55.333 PEN 1, 8 ;
  COLOR RGB(128,128,128,128,128,128)


ACTIVATE WINDOW lwIncSrch TOP

READ MODAL
RELEASE WINDOW lwIncSrch
lcExpToSeek = ALLTRIM(lcExpToSeek)
* if the user exit the window by using the enter
IF LASTKEY() = 13
  * get the current record #
  lnBrRecNO = IIF(RECNO()>RECCOUNT(),0,RECNO())
  * if the used tag is not the tag the programer want to use in
  * the inc. search function then change the active order to use
  * the programer's tag
  IF TYPE("lcSeekTag")='C' AND !EMPTY(lcSeekTag)
    lcOrgTag = SYS(22)
    lcOrgExp = SYS(14,VAL(SYS(21)))
    SET ORDER TO TAG (lcSeekTag)
  ENDIF

  
  * if the index expression is a compound index then seek
  * one by one of the value the user want to find until the
  * end of the expression or value not found in the file  
   FOR lnCount = 1 TO ALEN(laKeyExp,1)
    * if the value found in the file then go to the record that have
    * this value

    IF SEEK(&lakeyExp[lnCount]+lcExpToSeek)
      IF TYPE("lcOrgTag")='C'
        lcOrgKey = &lcOrgExp
        SET ORDER TO TAG lcOrgTag
        
        IF !EMPTY(lcOrgTag)
          SEEK lcOrgKey
        ENDIF  
      ENDIF
      
      CLEAR TYPEAHEAD
      POP KEY
      RETURN
    * else if the value does not exist in the file then go back to the
    * previous record  
    ELSE
      IF TYPE("lcOrgTag")='C'
        SET ORDER TO TAG lcOrgTag
      ENDIF
      IF lnBrRecNO>0
        GO lnBrRecNO
      ENDIF  
    ENDIF
  ENDFOR
ELSE
  lcExpToSeek = ''
ENDIF
RETURN




**********
FUNCTION gfSubStr
PARAMETERS lcString,lnAryOrPos,lcSepta

lcSubstr  =' '
lnAryDim  = 1
lnAryRows = 1
lnAryCols = 1
lcSepta   = IIF(TYPE('lcSepta')='C',lcSepta,',') 

IF LEN(ALLTRIM(lcSepta))>1
  lcColSep  = SUBSTR(lcSepta,2,1)
  lcSepta   = LEFT(lcSepta,1)
  lnAryDim  = IIF(OCCURS(lcSepta,lcString)>0,;
              OCCURS(lcSepta,lcString)+;
              IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
              lnAryDim)
  lnAryCols = IIF(OCCURS(lcColSep,lcString)>0,;
              OCCURS(lcColSep,lcString)+;
              IIF(RIGHT(lcString,1)<>lcColSep,1,0),;
              lnAryDim)
  lnAryRows = (lnAryDim+(lnAryCols-1)) / lnAryCols
  lnAryDim  = lnAryDim +(lnAryCols-1)     
  lcString  = STRTRAN(lcString,lcColSep,lcSepta)
ELSE
  lnAryDim = IIF(OCCURS(lcSepta,lcString)>0,;
             OCCURS(lcSepta,lcString)+;
             IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
             lnAryDim)
ENDIF

*** Chek if second parameter array or numeric
DO CASE
  *** If no parameter found assume firest part of string
  CASE TYPE ('lnAryOrPos')='U'
    lnAryOrPos = 1

  *** If array strich it to hold all string parts
  CASE TYPE ('lnAryOrPos') $ 'C,L'    
    IF lnAryCols > 1
      DIMENSION lnAryOrPos[lnAryRows,lnAryCols]
    ELSE
      IF ALEN(lnAryOrPos,2) > 0
        DIMENSION lnAryOrPos[lnAryDim,ALEN(lnAryOrPos,2)]
      ELSE
        DIMENSION lnAryOrPos[lnAryDim]
      ENDIF  

    ENDIF
    lnAryOrPos  = ' '

ENDCASE

FOR lnArElem  = 1 TO lnAryDim
  IF TYPE ('lnAryOrPos')='N'
    lnArElem = lnAryOrPos
  ENDIF  

  DO CASE
    *** In case of firest string part
    CASE lnArElem = 1
      lcSubstr = SUBSTR(lcString,1,;
      IIF(lcSepta $ lcString,AT(lcSepta,lcString)-1,LEN(lcString)))

    *** In case of last string part
    CASE lnArElem = lnAryDim
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1)
      lcSubstr = IIF(RIGHT(lcSubstr,1)=lcSepta,;
                 SUBSTR(lcSubstr,1,LEN(lcSubstr)-1),lcSubstr)
    *** In case of any string part from the meddel
    CASE lnArElem > 1
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1,;
                 AT(lcSepta,lcString,lnArElem)-;
                 AT(lcSepta,lcString,lnArElem-1)-1)
  ENDCAS

  IF TYPE ('lnAryOrPos')='N'
    RETURN lcSubstr
  ENDIF  
  
  IF lnAryCols > 1
    lnAryOrPos[((lnArElem-1)%lnAryRows)+1,INT((lnArElem-1)/lnAryRows)+1] = lcSubstr
  ELSE
    lnAryOrPos[lnArElem] = lcSubstr
  ENDIF
ENDFOR



FUNCTION lfAppend
PARAMETERS lcSource,lcDest
PRIVATE lnAlias
lnAlias = SELECT()
SELECT (lcSource)
SCAN ALL
  SCATT MEMVAR MEMO
  INSERT INTO &lcDest FROM MEMVAR
ENDSCAN
SELECT (lnAlias)


FUNCTION lfvOk
ACTIVATE WINDOW (lcBrowCnt)
_CUROBJ = OBJNUM(pbOk)
KEYBOARD "{SPACEBAR}" CLEAR PLAIN


FUNCTION lfDoFunc
PARAMETERS  lcToDo
DO (lcToDo)
SHOW WINDOW (lcWinTitl) Refresh Same


