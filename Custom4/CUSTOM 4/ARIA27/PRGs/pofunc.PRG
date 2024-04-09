*************************************************************
FUNCTION gfModalGen
PARAMETER lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg

RETURN oAriaApplication.MessageBox(lcDlgID,lcDlgTyp,lcVarsStr,lcDlgValid,lcDlgMessg)

*************************************************************
FUNCTION gfGetExSin
PARAMETER lcUntSin, lcCurrency, lcBaseCurr

LOCAL oGetExSin

oGetExSin = CREATEOBJECT('GetExSin')

RETURN oGetExSin.Do(@lcUntSin, lcCurrency, lcBaseCurr)

*************************************************************
FUNCTION NotePad
PARAMETER TEMP1 , TEMP2

LOCAL oNotePad , lnSelect , lcNotes

lnSelect = SELECT(0)
SELECT NOTEPAD

IF SEEK('B' + PADR('T' + EDILIBDT.cEDITranNo , 20))
  lcNotes = mNotes
  IF !SEEK(TEMP1 , PADR(TEMP2 , 20))
    APPEND BLANK
  ENDIF
  REPLACE mNotes WITH lcNotes ADDITIVE
ENDIF
oNotePad = CREATEOBJECT('NotePad')
=oNotePad.Do(TEMP1 , TEMP2)
SELECT (lnSelect)

*************************************************************
FUNCTION CHECKPRD
PARAMETERS ldDate,lcFYear,lcPeriod,lcTranTyp,llHideMsg
LOCAL oCheckPrd,lcReturn

oCheckPrd = CREATEOBJECT('CheckPrd')
llReturn = oCheckPrd.Do(ldDate,lcFYear,lcPeriod,lcTranTyp,llHideMsg)
RELEASE oCheckPrd
RETURN llReturn

*************************************************************
FUNCTION gfSequence
PARAMETERS lcSeqType,lcCompanyId,lcGroupId,lcDivision,lcField
LOCAL oSeqObj,lcReturn

*B605873,1 HASSAN [BEGIN]
*oSeqObj  = CREATEOBJECT('GetSequence')
IF TYPE('poform')="O"
  oSeqObj  = CREATEOBJECT('GetSequence',poform)
ELSE
  oSeqObj  = CREATEOBJECT('GetSequence')
ENDIF 
*B605873,1 HASSAN [END  ]
lcReturn = oSeqObj.Do(lcSeqType , lcCompanyId , lcGroupId ,lcDivision , lcField)
RELEASE oSeqObj 
RETURN lcReturn

*************************************************************
FUNCTION gfAdd_Info
PARAMETERS lcFileName

LOCAL oForm
oForm = IIF(TYPE('ThisForm') = 'O' , ThisForm , .NULL.)

RETURN oAriaApplication.AddUserInformation(lcFileName , oForm)

*************************************************************
FUNCTION lfCanReason

RETURN ''

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : Wael Aly Mohamed
*! Date      : 01/01/1996
*! Purpose   : Restore invoice information
*!*************************************************************
*! Calls     : gfGetAdr
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfGetInfo()
*!*************************************************************
FUNCTION lfGetInfo

DO lfGet_Info IN (oAriaApplication.ApplicationHome+ 'EB\SOUPDATE.FXP')

*************************************************************
FUNCTION gfGetAdr
PARAMETERS lcAlias, lcTag, lcKeyCode, lcAdrCode,lnLineNo,lcAddGrp,lcCurrCode
LOCAL oGetAdr,lcReturn
oGetAdr = CREATEOBJECT('GetAdr')
lcReturn = oGetAdr.Do(lcAlias, lcTag, lcKeyCode, lcAdrCode,lnLineNo,lcAddGrp,lcCurrCode)
RELEASE oGetAdr 
RETURN lcReturn

*************************************************************
FUNCTION gfwCodePop
PARAMETERS laInfArray, lcField, lcFillWith, lcActComp
LOCAL oCodePop,lcReturn

oCodePop = CREATEOBJECT('CodePop')
lcReturn = oCodePop.Do(@laInfArray, lcField, lcFillWith,lcActComp)
RELEASE oCodePop
RETURN lcReturn

*************************************************************
FUNCTION gfIsEdtble
PARAMETERS lcPField, lnFieldW, lcActvComp
LOCAL oIsEdtble,lcReturn

oIsEdtble = CREATEOBJECT('IsEdtble')
lcReturn  = oIsEdtble.Do(lcPField, @lnFieldW, lcActvComp)
RELEASE oIsEdtble
RETURN lcReturn

*************************************************************
FUNCTION lfGtOrder
LOCAL lcOrder

lcOrder = ''
DO FORM (oAriaApplication.ScreenHome + 'eb\eborder') WITH 'O' TO lcOrder

RETURN lcOrder

*************************************************************
FUNCTION gfOpenFile
*B602015,1 AHM 05/07/98
*PARAMETERS NFILE,lcIndex,MODE
PARAMETERS NFILE,lcIndex,MODE,lcAliasNam,llForceOp
LOCAL lcFileName , lcOrderTag,llReturn

llReturn = .F.
IF llForceOp
  oTempName  = CREATEOBJECT('GetTempName')
  lcAliasNam = oTempName.DO()
  RELEASE oTempName
ENDIF
lcFileName = IIF(ATC('\',nfile)<>0,SUBSTR(NFILE,RAT('\',nfile)+1),NFILE)
lcOrderTag = IIF(TYPE('lcIndex')='C',SUBSTR(lcIndex,IIF('\' $ lcIndex,ATC('\',lcIndex,OCCURS('\',lcIndex)),0) +1),'')
IF llForceOp
  USE (NFILE) AGAIN ALIAS (lcAliasNam) IN 0
  llReturn = .T.
ELSE  
  IF !USED(lcFileName)
    USE (NFILE) IN 0
    llReturn = .T.
  ENDIF
ENDIF
IF llForceOP
  SELECT (lcAliasNam)
ELSE
  SELECT (lcFileName)
ENDIF  
SET ORDER TO (lcOrderTag)
RETURN llReturn

*!*************************************************************
*! Name      : gfCloseFile
*! Developer : Hesham 
*! Date      : 12/17/1998
*! Purpose   : To Close database file
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Example            : =gfCloseFile('ORDHDR')
*!*************************************************************
*!*************************************************************
FUNCTION gfCloseFile
PARAMETER lcFileName
USE IN (lcFileName)

**************** Wael

*!*************************************************************************
*! Name      : gfCrtTmp
*! Developer : Hesham El-Sheltawi 
*! Date      : 12/08/97    
*! Purpose   : to Create uncomplete session temprory files
*!*************************************************************************
*: Calls       :
*!*************************************************************************      
*: Passed parameters  : lcFileStruc
*:                      lcTagExp
*:                      lcTag
*:*************************************************************************
*! Returns   :  Temprary file name
*:*************************************************************************     
FUNCTION gfCrtTmp
PARAMETERS lcFile,lcFileStruc,lcTagExp,lcTag
LOCAL oSeqObj
oSeqObj = CREATEOBJECT('CREATETEMPFILE')

RETURN oSeqObj.Do(lcFile,@lcFileStruc,@lcTagExp,lcTag)

*!*************************************************************
FUNCTION gfTraceKey
PARAMETERS lcFileName,lcKeyExpr,lcEventOccr,lcUpdtDir,lcUpdtModl
RETURN

******************************
FUNCTION gfSavSess

*B602018,1 Change this line to add a new parameter (Session number)
*lcSession [Begin]

*PARAMETERS lcSessType,lcFiles,laVars
PARAMETERS lcSessType , lcFiles , laVars , lcSession

*B602018,1 Change this line to add a new parameter (Session number) [End]

*B602018,1 Add this line to add a new parameter (Session number)
*lcSession [Begin]
lcSession = IIF(TYPE('lcSession') <> 'C' , '' , lcSession)
*B602018,1 Add this line to add a new parameter (Session number) [End]

PRIVATE lnAlias,lnVarNum , lcReturnSt , lcVarNam
lcSessType = UPPER(PADR(lcSessType,10))

*B602018,1 Add these lines to make sure that the UNCMSESS file is opened
*with the correct index tag [Begin]
PRIVATE laFilsTUse
DIMENSION laFilsTUse[1,8]

laFilsTUse[1,1] = 'UNCMSESS'
laFilsTUse[1,2] = 'TRANS'

=gfChkFile(.T. , @laFilsTUse)
*B602018,1 Add these lines to make sure that the UNCMSESS file is opened [End]

lnAlias = SELECT(0)

lcReturnSt = SPACE(0)
FOR lnVarNum = 1 TO ALEN(laVars)
  lcVarNam   = laVars[lnVarNum]
  lcReturnSt = lcReturnSt + lcVarNam + "|" + TYPE(lcVarNam) + "|" +      ;
               IIF(TYPE(lcVarNam)="N", ALLTRIM(STR(EVALUATE(lcVarNam),13,4)) ,;
               IIF(TYPE(lcVarNam)="D", ALLTRIM(DTOC(EVALUATE(lcVarNam))),;
               IIF(TYPE(lcVarNam)="L", IIF(EVALUATE(lcVarNam),"Y","N")  ,;
               &lcVarNam))) + '~'
ENDFOR

*B602018,1 Change this line to add a new parameter (Session number)
*lcSession [Begin]

*IF SEEK('O'+PADR(lcSessType,10)+gcUser_id,'UNCMSESS')
IF EMPTY(lcSession) .OR. ;
   SEEK('O' + PADR(lcSessType , 10) + gcUser_id + lcSession , 'UNCMSESS')

*B602018,1 Change this line to add a new parameter (Session number) [End]
  
  SELECT UnCmSess
  REPLACE mTmpFiles  WITH lcFiles   ,;
          mComent    WITH lcReturnSt
  
ENDIF          
llNoThing  = RLOCK()

SELECT(lnAlias)

*B602018,1 Add this line to restore the old environment [Begin]
=gfChkFile(.F. , @laFilsTUse)
*B602018,1 Add this line to restore the old environment [End]

*!*************************************************************
*! Name      : gfChkFile
*! Developer : Haytham El_Sheltawi
*! Date      : 05/11/98
*! Purpose   : 1) make sure that the needed files [The files
*!                needed by any global function or any program]
*!                is opened with the needed index tags after
*!                saving environment.
*!             
*!             2) restore the old environment.
*!             
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : None.
*!*************************************************************
*! Passed Parameters  : llOpen       .T. to save env. and open file
*!                                   .F. to restore environment
*!                      
*!                      laFilsAray   reference to Array to hold the 
*!                                   needed files and index tags.
*!
*!           Note :     1) The array should be of dimension [X , 8]
*!                         where X : number of files to open
*!                      2) If llOpen = .T. the function will add
*!                         a new row to the array to save the
*!                         current alias
*!                         
*!  Array columns :  1 - File name [If its empty* this row will
*!                       be ignored].
*!                   2 - Index tag [If empty* the order will be
*!                       set to 0].
*!                   3 - Path [If .T. the system directory will
*!                       be used and if empty* the data directory
*!                       will be used if the file is not opened].
*!                       Path may also include a character expression
*!                       to hold a specific path to use
*!                       e.g. 'P:\SBTPRO\'
*!                   4 - Don't restore the old record pointer
*!                       [if .T. the function will not restore the
*!                       old record pointer].
*!                   5 - 8 these columns are used by the function.
*!                   
*!                   (* Or not of the correct type)
*!
*!*************************************************************
*! Returns            : 1) .F. If llOpen is not of type logic
*!                         or if laFilsAray is not an array.
*!                      2) .T. Otherwise.
*!*************************************************************
*! Example   : = gfChkFile(.T. , @laFiles)
*!*************************************************************
*!E300875,1 This function was add by HS for the Enhancement.
*!*************************************************************
*
FUNCTION gfChkFile

PARAMETERS llOpen , laFilsAray

PRIVATE laTmpArray , lnCount , lnRow , lnCol , lcFileTUse , lcPath ,;
        lcUserPath , lcUsedPath , lcUsedDbf , lcTagToUse , llRestrRec ,;
        lcOldDbf , lcSetFull , lnFileTUse

*--If llOpen is not of type logic or laFilsAray is not an array
IF TYPE('llOpen') <> 'L' .OR. TYPE('laFilsAray[1,1]') <> 'C'
  RETURN .F.
ENDIF    && End of IF TYPE('llOpen' <> 'L') .OR. TYPE('laFilsAray[1,1]' <> 'C')

*-- If the function is to open the needed files and to save environment
IF llOpen
  lcSetFull = SET('FULLPATH')      && Save the SET FULLPATH status
  SET FULLPATH ON
  
  *-- If the array dimension is not correct
  IF ALEN(laFilsAray , 2) < 8
    DIMENSION laTmpArray[ALEN(laFilsAray , 1) , 8]
    
    *-- For loop to scan the array rows
    FOR lnRow = 1 TO ALEN(laFilsAray , 1)
      
      *-- For loop to scan the array columns
      FOR lnCol = 1 TO ALEN(laFilsAray , 2)
        
        *-- Copy the elements from the array laFilsAray to the array
        *-- laTmpArray [For if we used the DIMENSION command and changed
        *-- the number of columns the elements will be moved from one
        *-- position to another]
        laTmpArray[lnRow , lnCol] = laFilsAray[lnRow , lnCol]
      ENDFOR    && End of FOR lnCol = 1 TO ALEN(laFilsAray , 2)
    ENDFOR    && End of FOR lnRow = 1 TO ALEN(laFilsAray , 1)
    
    DIMENSION laFilsAray[ALEN(laFilsAray , 1) , 8]
    =ACOPY(laTmpArray , laFilsAray)
  ENDIF    && End of IF ALEN(laFilsAray , 2) < 8
  
  *-- For loop to scan the array rows
  FOR lnCount = 1 TO ALEN(laFilsAray , 1)
    
    *-- Variable to hold the name of the file to be opened
    lcFileTUse = UPPER(ALLTRIM(laFilsAray[lnCount , 1]))
    
    laFilsAray[lnCount , 8] = ''
    
    *-- If lcFileTUse is of type character and not empty
    IF TYPE('lcFileTUse') = 'C' .AND. !EMPTY(lcFileTUse)
      
      *-- If the file is opened
      IF !USED(lcFileTUse)
        
        *-- Get the path that will be used to open the file
        lcPath = laFilsAray[lnCount , 3]
        lcPath = IIF(TYPE('lcPath') = 'C' , ALLTRIM(lcPath) ,;
                     IIF(TYPE('lcPath') = 'L' .AND. lcPath , gcSysHome ,;
                         gcDataDir))
        
        USE (lcPath + lcFileTUse) IN 0
        laFilsAray[lnCount , 5] = .T.          && Flag to know that the file was not opened befor
      ELSE    && Else [If the file is not opened]
        laFilsAray[lnCount , 5] = .F.          && Flag to know that the file was opened befor
        
        *-- Save the controlling index tag
        laFilsAray[lnCount , 6] = ORDER(lcFileTUse)
        
        *-- Save the record pointer position
        laFilsAray[lnCount , 7] = IIF(EOF(lcFileTUse) , 0 , RECNO(lcFileTUse))
        
        *-- Variable to hold the file path that was sent to the function
        lcUserPath = laFilsAray[lnCount , 3]
        lcUserPath = IIF(TYPE('lcPath') = 'C' , ALLTRIM(lcPath) ,;
                     IIF(TYPE('lcPath') = 'L' .AND. lcPath , gcSysHome , ''))
        
        *-- Variable to hold the file path of the file used with alias lcFileTUse
        lcUsedPath = SUBSTR(DBF(lcFileTUse) , 1 , AT('\' , DBF(lcFileTUse) ,;
                     OCCURS('\' , DBF(lcFileTUse))))
        
        *-- Variable to hold the name of the file used with alias lcFileTUse
        lcUsedDbf  = SUBSTR(DBF(lcFileTUse) , AT('\' , DBF(lcFileTUse) ,;
                     OCCURS('\' , DBF(lcFileTUse))) + 1)
        
        lcUsedDbf = STRTRAN(UPPER(lcUsedDbf) , '.DBF')
        
        *-- If the file that is used with alias lcFileTUse is not the
        *-- needed one
        IF UPPER(lcUsedPath) <> UPPER(lcUserPath) .OR. !(lcUsedDbf == lcFileTUse)
          
          *-- Save the name and path of the file that is used with
          *-- alias lcFileTUse
          laFilsAray[lnCount , 8] = DBF(lcFileTUse)
          
          lnSelect = SELECT(lcFileTUse)
          
          *-- Get the path that will be used to open the file
          lcPath   = laFilsAray[lnCount , 3]
          lcPath   = IIF(TYPE('lcPath') = 'C' , ALLTRIM(lcPath) ,;
                     IIF(TYPE('lcPath') = 'L' .AND. lcPath , gcSysHome ,;
                     gcDataDir))
          
          USE (lcPath + lcFileTUse) IN lnSelect
        ENDIF    && End of IF UPPER(lcUsedPath) <> UPPER(lcUserPath) .OR. !(lcUsedDbf == lcFileTUse)
      ENDIF    && End of IF !USED(lcFileTUse)
      
      *-- The index tag that will be used
      lcTagToUse = laFilsAray[lnCount , 2]
      lcTagToUse = IIF(TYPE('lcTagToUse') <> 'C' .OR. EMPTY(lcTagToUse) , '' ,;
                       'TAG ' + ALLTRIM(lcTagToUse))
      
      SET ORDER TO &lcTagToUse IN (lcFileTUse)
    ENDIF    && End of IF TYPE('lcFileTUse') = 'C' .AND. !EMPTY(lcFileTUse)
  ENDFOR    && End of FOR lnCount = 1 TO ALEN(laFilsAray , 1)
  
  *-- Save the current alias
  DIMENSION laFilsAray[ALEN(laFilsAray , 1) + 1 , 8]
  laFilsAray[ALEN(laFilsAray , 1) , 1] = SELECT(0)
  
  *-- Restore the SET FULLPATH status
  SET FULLPATH &lcSetFull

ELSE    && Else [If the function is to restore the old environment]
  
  *-- For loop to scan the array rows except the last one
  FOR lnCount = 1 TO ALEN(laFilsAray , 1) - 1
    
    *-- Variable to hold the name of the file
    lcFileTUse = UPPER(ALLTRIM(laFilsAray[lnCount , 1]))
    
    *-- Variable to hold the name and path of the file that was used with
    *-- alias lcFileTUse
    lcOldDbf   = laFilsAray[lnCount , 8]
    
    *-- If the file is opened
    IF TYPE('lcFileTUse') = 'C' .AND. !EMPTY(lcFileTUse) .AND. USED(lcFileTUse)
      
      *-- If the file is to be closed
      IF laFilsAray[lnCount , 5]
        USE IN (lcFileTUse)
      ELSE    && Else [If the file is not to be closed]
        
        *-- Flag to know if we are to restore the old record pointer position
        llRestrRec = laFilsAray[lnCount , 4]
        llRestrRec = IIF(TYPE('llRestrRec') <> 'L' , .T. , !llRestrRec)
        
        *-- If there is another file that was open with alias lcFileTUse
        IF !EMPTY(lcOldDbf)
          llRestrRec = .T.
          lnSelect = SELECT(lcFileTUse)
          
          *-- Reopen the old file
          USE (lcOldDbf) IN lnSelect
        ENDIF    && End of IF !EMPTY(lcOldDbf)
        
        *-- If we are to restore the old record pointer position
        IF llRestrRec
          
          *-- If the old record pointer position is end of file
          IF laFilsAray[lnCount , 7] = 0
            
            *-- If we are not at the end of the file
            IF !EOF(lcFileTUse)
              GO BOTTOM IN (lcFileTUse)
              SKIP 1 IN (lcFileTUse)
            ENDIF    && End of IF !EOF(lcFileTUse)
          ELSE    && Else [If the old record pointer position is not end of file]
            
            *-- Restore the old record pointer position
            GO laFilsAray[lnCount , 7] IN (lcFileTUse)
          ENDIF    && End of IF laFilsAray[lnCount , 7] = 0
        ENDIF    && End of IF llRestrRec
        
        *-- Restore the old controlling index tag
        SET ORDER TO laFilsAray[lnCount , 6] IN (lcFileTUse)
      ENDIF    && End of IF laFilsAray[lnCount , 5]
    ENDIF    && End of IF TYPE('lcFileTUse') = 'C' .AND. ... .AND. USED(lcFileTUse)
  ENDFOR    && End of FOR lnCount = 1 TO ALEN(laFilsAray , 1) - 1
  
  *-- Restore the old alias
  lnFileTUse = laFilsAray[ALEN(laFilsAray , 1) , 1]
  SELECT (lnFileTUse)
ENDIF    && End of IF llOpen

RETURN .T.

*!*************************************************************
*! Name      : gfGetMemVar
*! Developer : Wael Ali Mohamed
*! Date      : 10/08/2000
*! Purpose   : Get Memory Variable
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : File Name
*!*************************************************************
*! Example            : =gfCloseFile('ORDHDR')
*!*************************************************************
*!*************************************************************
FUNCTION gfGetMemVar
PARAMETERS lcArray,lcCompID
PRIVATE oGetMemVar,lcReturn

oGetMemVar = CREATEOBJECT('GetMemVar')
RETURN (oGetMemVar.Do(@lcArray,lcCompID))

*****************************************************************************
* PROC: GLDIST.PRG
* DESC: A GLOBAL PROCEDURE  THAT WILL BE  CALLED FROM  SEVERAL PLACES WITH
*       DIFFERENT PARAMETERS, DEPENDING ON THE  PARAMETERS THE PROGRAM WILL
*       UPDATE THE G/L DISTRIBUTION FILE WITH THE CORRESPONDING G/L ACCOUNT
*       AND AMOUNT. 
* DATE: 12/24/93
* AUTH: WAEL ALY MOHAMED
* NOTE: PARAMETERS USED:-
*       - _GLLINK  : GL LINK CODE -> THE LINK CODE USED FOR POSTING.
*
*       - _CATGKEY : CATEGORY KEY
*
*       - _AMOUNT  : AMOUNT       
*                    DEBITS ARE POSITIVE, CREDITS ARE NEGATIVE.
*
*       - _TRANTYP : TRANSACTION TYPE :-
*                    'IN' -> INVOICE
*                    'VI' -> VOID INVOICE
*                    'CR' -> CASH RECEIPT
*                    'CA' -> CREDIT ADJUSTMENT                         
*                    'DA' -> DEBIT ADJUSTMENT                         
*                    'RM' -> RETURN MERCHANDISE
*                    'VR' -> VOID RETURN
*                    'IP' -> INVENTORY PHYSICAL
*                    'MA' -> MATERIAL INVENTORY ADJUSTMENT
*                    'MP' -> MATERIAL INVENTORY PHYSICAL
*                    'IA' -> INVENTORY ADJUSTMENT
*                    'PO' -> P/O RECEIVING
*                    'MO' -> MATERIAL P/O RECEIVING
*                    'CT' -> C/T RECEIVING
*                    'ZE' -> ZERO OUT STOCK
*                    'NL' -> NON MAT. LIABILITY
*                    'JC' -> JOB COST CLOSING ADJ
*                    'RO' -> MATERIAL P/O RECEIVING
*                    'RS' -> C/T RECEIVING
*                    'MM' -> ZERO OUT STOCK
*                    'EX' -> EXCHANGE RATE DIFFERENCE  
*                    'KO' -> KEY OFF
*
*      - _TRANNO   : TRANSACTION NUMBER.
*
*      - _TRANDAT  : TRANSACTION DATE.
*
*      - _NFILE    : NAME OF FILE WILL BE USED. 
*      - _FYEAR    : TRANSACTION FISCAL YEAR.
*      - _PRDID    : PERIOD ID.
*E300325,1 If base currency, or, single currency setting,
*E300325,1 these parameters may not be passed.
*E300325,1 lcCurrCode : the current transaction currency code.
*E300325,1              (if not passed, defaults to the base currency)
*E300325,1 lnCurrUnit : the current transaction currency unit.
*E300325,1              (if lcCurrCode is not passed, defaults to 1)
*E300325,1 lnExRate   : the exchange rate used for the current transaction
*E300325,1              (if lcCurrCode is not passed, defaults to 1)
* MODI:
*    WAM 03/29/94 
*        1) Add a new field 'GlACNTTYPE' in 'GLDIST.MDL' file to hold
*           Type of account [ Control account or Distribution account].
*        2) Update the field 'GLACNTTYPE' with the account type depends
*           on the passed category key.
*        3) If the link code not found in 'GL_LINK' file (Zap the file 
*           for example), Default link code to 'DEF'.
*   WAM 04/20/94
*        1) Add thre category keyes '015', '016' and '017' for material
*           inventory control, inventory adjustment and P/O clearing.
*        2) Add three transaction types
*           'MP', 'MA' and 'MO' for material inventory physical, inventory 
*            adjustment and P/O receiving.
*   ARH 11/10/94
*       1) Added new transaction type for non material cost liability.
*!  WAM 03/05/95
*!      1) Add a new account '020' for Return Merchandise
*!      2) Do not add entry for zero amount.
*E100219,9 WAM 07/04/95 Add new type codes for receiving materials & styles
*N100219,4              from operation
*N000016,6 WAM 07/04/95 Add new type code for M.F.G. order receivin.
*E300325,1 RENEE 12/25/95 Update currency and equivalent amount fields
*E300325,1                in GLDIST.DBF 
*E300325,1                (cCurrCode, nCurrUnit. nExRate, nEqvAmnt) 
*E300344,2 WAM 02/14/96 Add new transaction type for Differences in Exchange rate
*E300592,1 WAM 02/25/97 Increase the link_Code field to be 6 characters
*B603504,1 ADEL 12/04/2000 Add a new transaction type for Closing material MFG and Closing Material PO.
*B603862,1 SSE 09/07/2000 Add new type for the Manufacturing closing cost sheet also modify 
*B603862,1                'JC' type to be on P/O Closing Cost sheet
*****************************************************************************
PROCEDURE GLDIST
*MAN
*PARAMETERS _GLLINK,_CATGKEY,_AMOUNT,_TRANTYP,_TRANNO,_TRANDAT,_FYEAR,_PRDID,_NFILE
*E300325,1 Add currency fields parameters:
*E300325,1 (lcCurrCode, lnCurrUnit, lnExRate, 
*PARAMETERS _GLLINK,_CATGKEY,_AMOUNT,_TRANTYP,_TRANNO,_TRANDAT,_FYEAR,_PRDID,_NFILE,lcGlAct
PARAMETERS _GLLINK,_CATGKEY,_AMOUNT,_TRANTYP,_TRANNO,_TRANDAT,_FYEAR,_PRDID,;
           _NFILE,lcGlAct, lcCurrCode, lnCurrUnit, lnExRate 

*E300325,1 Add private variables for equivalent amounts calculations
*PRIVATE XGLACNT,XTRANDESC,lcAcntType
PRIVATE XGLACNT, XTRANDESC, lcAcntType, lcExRSin, lcUntSin, lnEqvAmnt 
*E300325,1 end.

*B803032,1 BWA 14/02/2000 [START]
* Fix the bug of Period & year fields in GL distribution file are empty,So there is no way 
* to release those entries unless we are replace in those fields manually.
IF EMPTY(_FYEAR) OR EMPTY(_PRDID)
  =CheckPrd(_TRANDAT,"_FYEAR","_PRDID",'_TRANTYP' , .T.)
  IF EMPTY(_FYEAR) OR EMPTY(_PRDID)
    DO (gcapphome +'GL\GLDDATE.PRG')
  ENDIF
ENDIF
*B803032,1 BWA 14/02/2000 [END]


*E301210,1 ASH 04/27/99 (Begin) Don't create GL entries for some transactions 
*E301210,1                      due to the new parameter M_GL_COST.
llGlCost = ALLTRIM(gfGetMemVar('M_GL_COST',gcAct_Comp))='Y'

IF !llGlCost AND _CATGKEY $ '006,007,008,011,012,013,015,016,017,018,019,021,022,023,024,025,026,027'
  RETURN
ENDIF
*E301210,1 ASH 04/27/99 (End)
*** OPEN GL_LINK FILE TO GET G/L ACCOUNT FOR THIS CATEGORY/GL LINK CODE

IF _AMOUNT = 0
  RETURN
ENDIF  
*E300325,1 If the currency code is not passed, or
*E300325,1 If the currency code is the base currency, or
*E300325,1 If the category key of the transaction is either,
*E300325,1   '006' : 'FINISHED GOODS INVENTORY'
*E300325,1    or
*E300325,1   '008' : 'COST OF GOODS' 
*E300325,1 Default the currency fields to the base currency values.
IF EMPTY(lcCurrCode) .OR. INLIST(_CATGKEY, '006', '008');
  .OR. lcCurrCode = gcBaseCurr
  lcCurrCode = gcBaseCurr
  lnCurrUnit = 1
  lnExRate   = 1
  lnEqvAmnt  = _AMOUNT
ELSE
  *E300325,1 If either of the exchange rate or the currency unit
  *E300325,1 is not greater than 0, return .F., otherwise, calculate
  *E300325,1 as follows. 
  IF lnExRate > 0 .AND. lnCurrUnit > 0
    *E300325,1 Get the exchange rate sign for the curreny code
    lcUntSin = ''
    lcExRSin = gfGetExSin(@lcUntSin, lcCurrCode)
    *E300325,1 Get the currency unit sign for the curreny code
    lnEqvAmnt  = ROUND(_AMOUNT &lcExRSin lnExRate ;
                               &lcUntSin lnCurrUnit, 2)
  ELSE
    RETURN .F.
  ENDIF  
ENDIF
*E300325,1 end.

*E300637,1 Use the new global variables names
*IF !OPENED('GL_LINK')
*  DO NETUSE WITH '&QDD.GL_LINK','&QDD.GL_LINK','SH'
*ENDIF
=gfOpenFile(gcDataDir+'GL_LINK',gcDataDir+'GL_LINK','SH')
*E300637,1 (End)

SELE GL_LINK  
*-- 03/29/94 WAM 
*-- If the link code not found (Zap the file for example), default to 'DEF'

*E300592,1 Increase the link_Code field to be 6 characters
*IF !SEEK(_GLLINK+_CATGKEY)    
*  SEEK('DEF'+_CATGKEY)
*ENDIF   
IF !SEEK(PADR(_GLLINK,6)+_CATGKEY)    
  SEEK('DEFDEF'+_CATGKEY)
ENDIF   
*E300592,1 (End)

*MAN
*XGLACNT = GLACNT
XGLACNT = IIF(EMPTY(lcGlAct),GLACNT,lcGlAct)

*-- WAM 03/29/94
*-- Get Account Type

DO CASE
  CASE _CATGKEY = '001'   && Account Receivable
    *-- Control Account
    lcAcntType = 'C'
  
  CASE _CATGKEY = '002'   && Cash Receipts
    *-- Distribition Account
    lcAcntType = 'D'  
  
  CASE _CATGKEY = '003'   && Sales Revenue
    *-- Control Account
    lcAcntType = 'C'  
  
  CASE _CATGKEY = '004'   && Freight
    *-- Control Account
    lcAcntType = 'C'  
  
  CASE _CATGKEY = '005'   && Discount
    *-- Distribition Account
    lcAcntType = 'D'  
    
  *-- WAM 04/20/94  
  *-- Add category key for material inventory control  
  CASE _CATGKEY = '006' .OR. _CATGKEY = '015'  && Inventory Control
    *-- It is a Control account if the inventory decrease, 
    *-- and a distribution account if the inventory increase.
    lcAcntType = IIF(_AMOUNT < 0 , 'C', 'D')
    
  *-- WAM 04/20/94
  *-- Add category key for material inventory adjustment
  CASE _CATGKEY = '007' .OR. _CATGKEY = '016'  && Inventory Adjustments
    *-- It is a distribution account if the inventory decrease, 
    *-- and a Control account if the inventory increase. 
    lcAcntType = IIF(_AMOUNT < 0 , 'D', 'C')

  CASE _CATGKEY = '008'   && Cost of Goods
    *-- It is a distribution account if the inventory decrease, 
    *-- and a Control account if the inventory increase. 
    lcAcntType = IIF(_AMOUNT < 0 , 'D', 'C')

  CASE _CATGKEY = '009'   && Credit Adjustments
    *-- Distribition Account
    lcAcntType = 'D'  

  CASE _CATGKEY = '010'   && Debit Adjustments
    *-- Distribition Account
    lcAcntType = 'D'  

  CASE _CATGKEY = '011'   && Return Merchandise
    *-- Distribition Account
    lcAcntType = 'D'  
    
  *-- WAM 04/20/94  
  *-- Add category key for material P/O clearing

  CASE _CATGKEY = '012' .OR. _CATGKEY = '017'  && P/O Clearing
    *-- It is a distribution account if the inventory decrease, 
    *-- and a Control account if the inventory increase. 
    lcAcntType = IIF(_AMOUNT < 0 , 'D', 'C')

  CASE _CATGKEY = '013'   && C/T Clearing
    *-- It is a distribution account if the inventory decrease, 
    *-- and a Control account if the inventory increase. 
    lcAcntType = 'C'

  CASE _CATGKEY = '014'   && Sales Tax Liability
    *-- Control Account
    lcAcntType = 'C'
  
  CASE _CATGKEY = '018'   && Non material cost liability
    *-- Control Account
    lcAcntType = 'D'    
  CASE _CATGKEY = '019'   && Cost of goods variance
    *-- Distribution Account
    lcAcntType = 'D'    
  CASE _CATGKEY = '020'   && Return Merchandise
    *-- Distribution Account
    lcAcntType = 'C'    
  *E100219,9 WAM 07/04/95 Add new category key '021' in the GL_CATG for
  *E100219,9              Cost of Material Variance.
  CASE _CATGKEY = '021'   && Cost of material variance
    *-- Distribution Account
    lcAcntType = 'D'    

  CASE _CATGKEY = '022'   && Cost of goods variance 1
    *-- Distribution Account
    lcAcntType = 'D'    
  CASE _CATGKEY = '023'   && Cost of goods variance 2
    *-- Distribution Account
    lcAcntType = 'D'    
  CASE _CATGKEY = '024'   && Cost of goods variance 3
    *-- Distribution Account
    lcAcntType = 'D'    
  CASE _CATGKEY = '025'   && Cost of goods variance 4
    *-- Distribution Account
    lcAcntType = 'D'    
  *B603573,1 SSH 16/04/00 Add New Categ key in GlDist Procedure. [Begin]
  CASE _CATGKEY = '026'   && Cost of goods variance 5
    lcAcntType = 'D'
  *B603573,1 SSH 16/04/00 Add New Categ key in GlDist Procedure. [End]
ENDCASE

*** GET DESCRIBTION OF THIS TRANSACTION TYPE

*-- WAM 04/20/94
*-- Add three transaction types
*-- 'MP', 'MA' and 'MO' for material inventory physical, inventory adjustment
*-- and P/O receiving.

DO CASE
  CASE _TRANTYP = 'IN' 
    XTRANDESC = 'INVOICE             '
  CASE _TRANTYP = 'VI' 
    XTRANDESC = 'VOID INVOICE        '
  CASE _TRANTYP = 'CR' 
    XTRANDESC = 'CASH RECEIPT        '
  CASE _TRANTYP = 'CA' 
    XTRANDESC = 'CREDIT ADJUSTMENT   '
  CASE _TRANTYP = 'DA' 
    XTRANDESC = 'DEBIT ADJUSTMENT    '                         
  CASE _TRANTYP = 'RM' 
    XTRANDESC = 'RETURN MERCHANDISE  '
  CASE _TRANTYP = 'VR' 
    XTRANDESC = 'VOID RETURN         '
  CASE _TRANTYP = 'IP' 
    XTRANDESC = 'INVENTORY PHYSICAL  '
  CASE _TRANTYP = 'IA' 
    XTRANDESC = 'INVENTORY ADJUSTMENT'
  CASE _TRANTYP = 'MP' 
    XTRANDESC = 'MATERIAL INV. PHYSI.'
  CASE _TRANTYP = 'MA' 
    XTRANDESC = 'MATERIAL INV. ADJUS.'
  CASE _TRANTYP = 'PO' 
    XTRANDESC = 'P/O RECEIVING       '
  CASE _TRANTYP = 'MO' 
    XTRANDESC = 'MATERIAL P/O RECEIV.'
  CASE _TRANTYP = 'CT' 
    XTRANDESC = 'C/T RECEIVING       '
  CASE _TRANTYP = 'ZE' 
    XTRANDESC = 'ZERO OUT STOCK      '

  *-- ARH 11/10/94
  CASE _TRANTYP = 'NL'
    XTRANDESC = 'NON MAT. LIABILITY  '
    
  *B603862,1 Modify 'JC' type description [Begin] 
  CASE _TRANTYP = 'JC'
    *XTRANDESC = 'JOB COST CLOSING ADJ'
    XTRANDESC = 'P/O JOB COST CLOSING'
  *B603862,1 Modify 'JC' type description [End] 
      
  *-- END  ARH  11/10/94
  *E100219,9 WAM 07/04/95 Add new type codes for receiving materials & styles
  *N100219,4              from operation
  *N000016,6 WAM 07/04/95 Add new type code for M.F.G. order receivin.
  CASE _TRANTYP = 'RO' 
    XTRANDESC = 'MATERIAL OP. RECEIVE'
  CASE _TRANTYP = 'RS' 
    XTRANDESC = 'STYLE OP. RECEIVE   '
  CASE _TRANTYP = 'MM' 
    XTRANDESC = 'RECEIVE M.F.G. ORDER'
  *E300344,2 Add new transaction type for Differences in Exchange rate
  CASE _TRANTYP = 'EX' 
    XTRANDESC = 'EX. RATE DIFFERENCES'
  CASE _TRANTYP = 'KO' 
    XTRANDESC = 'KEY OFF'
  *B603504,1 (Begin) Add a new transaction type for Closing material MFG and Closing Material PO.
  CASE _TRANTYP = 'MC' 
    XTRANDESC = 'MATERIAL JOB ClOSING'
  *B603504,1 (End)

  *B603862,1 Add new type for the Manufacturing closing cost sheet [Begin] 
  CASE _TRANTYP = 'JP'
    XTRANDESC = 'C/T JOB COST CLOSING'
  *B603862,1 Add new type for the Manufacturing closing cost sheet [End] 
ENDCASE

***
*-- WAM 03/29/94
*-- Update the field 'GLACNTTYPE' with the account type

SELE &_NFILE
APPEND BLANK
*E300325,1 Update currency fields and equivalent amount field
*E300325,1 in GLDIST (cCurrCode, nCurrUnit, nExRate, nEqvAmnt)
*REPLACE CATG_KEY   WITH _CATGKEY, ;
*        TRAN_TYPE  WITH _TRANTYP, ;
        TRAN_NO    WITH _TRANNO,  ;
*        AMOUNT     WITH _AMOUNT,  ;
*        TRAN_DATE  WITH _TRANDAT, ;
        GLACCOUNT  WITH XGLACNT,  ;
        ADD_DATE   WITH DATE(),   ;
        ADD_TIME   WITH TIME(),   ;
*        ADD_USERID WITH QUSERID,  ;
*        TRAN_DESC  WITH XTRANDESC, ;
*        GLPERIOD   WITH _PRDID, ;
*        GLFYEAR    WITH _FYEAR, ;
*        GLACNTTYPE WITH lcAcntType

*E300637,1 Use file new fields names
*REPLACE CATG_KEY   WITH _CATGKEY, ;
        TRAN_TYPE  WITH _TRANTYP, ;
        TRAN_NO    WITH _TRANNO,  ;
        AMOUNT     WITH _AMOUNT,  ;
        TRAN_DATE  WITH _TRANDAT, ;
        GLACCOUNT  WITH XGLACNT,  ;
        ADD_DATE   WITH DATE(),   ;
        ADD_TIME   WITH TIME(),   ;
        ADD_USERID WITH QUSERID,  ;
        TRAN_DESC  WITH XTRANDESC, ;
        GLPERIOD   WITH _PRDID, ;
        GLFYEAR    WITH _FYEAR, ;
        GLACNTTYPE WITH lcAcntType,;
        cCurrCode  WITH lcCurrCode,;
        nCurrUnit  WITH lnCurrUnit,;
        nExRate    WITH lnExRate,;
        nEqvAmnt   WITH lnEqvAmnt

* MAB 03/07/99 Remove updating odded fields [Begin
*REPLACE CATG_KEY   WITH _CATGKEY  ,;
        TRAN_TYPE  WITH _TRANTYP  ,;
        TRAN_NO    WITH _TRANNO   ,;
        nGlAmount  WITH _AMOUNT   ,;
        TRAN_DATE  WITH _TRANDAT  ,;
        GLACCOUNT  WITH XGLACNT   ,;
        ADD_DATE   WITH gdSysDate ,;
        ADD_TIME   WITH TIME()    ,;
        ADD_USERID WITH gcUser_Id ,;
        TRAN_DESC  WITH XTRANDESC ,;
        GLPERIOD   WITH _PRDID    ,;
        GLFYEAR    WITH _FYEAR    ,;
        GLACNTTYPE WITH lcAcntType,;
        cCurrCode  WITH lcCurrCode,;
        nCurrUnit  WITH lnCurrUnit,;
        nExRate    WITH lnExRate  ,;
        nEqvAmnt   WITH lnEqvAmnt

REPLACE CATG_KEY   WITH _CATGKEY  ,;
        TRAN_TYPE  WITH _TRANTYP  ,;
        TRAN_NO    WITH _TRANNO   ,;
        nGlAmount  WITH _AMOUNT   ,;
        TRAN_DATE  WITH _TRANDAT  ,;
        GLACCOUNT  WITH XGLACNT   ,;
        TRAN_DESC  WITH XTRANDESC ,;
        GLPERIOD   WITH _PRDID    ,;
        GLFYEAR    WITH _FYEAR    ,;
        GLACNTTYPE WITH lcAcntType,;
        cCurrCode  WITH lcCurrCode,;
        nCurrUnit  WITH lnCurrUnit,;
        nExRate    WITH lnExRate  ,;
        nEqvAmnt   WITH lnEqvAmnt
* MAB 03/07/99 Remove updating odded fields [End

*E300637,1 (End)
*E300325,1 end.
RETURN

********************
*** EOF of GLDIST
********************

FUNCTION lfTraceTag
PARAMETERS lcFileNam,lcCommStat


*!*************************************************************
*! Name      : gfObj_Lock
*! Developer : HS Haytham El-Sheltawi
*! Date      : 10/24/2000
*! Purpose   : Dummy function to prevent errors when the PO
*!             Cancellation is called from EDI.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : flage to lock or unlock
*!*************************************************************
*! Returns            : .T.
*!*************************************************************
*! Example   : =gfObj_Lock(.F.)
*!*************************************************************
*:->
FUNCTION gfObj_Lock
PARAMETERS lLok_Set

***************************************
FUNCTION gfThermo
PARAMETERS lnToBeDone,lnDone,lcMessage,lcVariable
*!*************************************************************
FUNCTION gfTherm
PARAMETERS lnToBeDone,lnDone,lcMessage,lcVariable

*********************************************
FUNCTION gfRltFld
PARAMETERS lcCodeVal,laArrayNam, lcFldName
PRIVATE lcCodeVal,laArrayNam, lcFldName
LOCAL oTermRltdFl

oTermRltdFl = CREATEOBJECT('GetRelatedFields')
RETURN oTermRltdFl.Do(lcCodeVal,@laArrayNam,lcFldName)

*********************************************
FUNCTION GFCODDES
*C200353,1 ADEL ON 06/13/02 (Begin) Fix the bug of mis-arrainging the parameters.
*PARAMETERS lcCodeName,lcCodeVal
PARAMETERS lcCodeVal,lcCodeName
*C200353,1 ADEL ON 06/13/02 (End)

=SEEK('N'+PADR(lcCodeVal,6)+'N'+PADR(lcCodeName,10),'CODES','CODES')
RETURN CODES.cDiscrep