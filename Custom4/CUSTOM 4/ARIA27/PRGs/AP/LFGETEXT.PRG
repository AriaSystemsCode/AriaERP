*!**************************************************************************
*!
*!      Function: lfGetExt
*!
*!**************************************************************************
*B800456,1 M.H 02/11/96 Add a new parameter to hold the browse title.
* Author : Renee Ezzat
* Validates or browses for an external key field (parameter)
*
FUNCTION lfGetExt
*B800456,1 M.H 02/11/96 Begin.
*PARAMETERS lcSrchFile, lcSrchTag, lcOldVal, llBrowse,;
           lcFldName, laTemp, lcSrchFlds, lcSrchExp, lcKeyCode,lcDataDir
PARAMETERS lcSrchFile, lcSrchTag, lcOldVal, llBrowse,;
           lcFldName, laTemp, lcSrchFlds, lcSrchExp, lcKeyCode,lcDataDir,;
           lcFileTitle
*B800456,1 M.H 02/11/96 End.

** .T. if browsed from browsing invisible button
PRIVATE lcSrchVal, lcSrchObj, lcCurAlias, lcSavBrFld, lcSavTitle,;
        lnClosRec, lnSavTag, llOpenFile, llSrchFound, lcSavExact

lcSrchObj   = SYS(18)
lcSrchVal   = ALLTRIM(EVALUATE(lcSrchObj))
lcKeyCode   = IIF(EMPTY(lcKeyCode), .F., lcKeyCode)
llSrchFound = .T.
lcSavExact  = SET('EXACT')
IF llBrowse .OR. lcSrchVal <> lcOldVal
  laTemp     = ' '
  IF llBrowse .OR. !EMPTY(lcSrchVal)
    lcSrchFlds = IIF(EMPTY(lcSrchFlds), " ", STRTRAN(lcSrchFlds,' '))
    lcDataDir  = IIF(EMPTY(lcDataDir), gcDataDir, lcDataDir)
    lcCurAlias = ALIAS()

    *** Open the search file if it is not already open
    IF !USED(lcSrchFile)
      SELECT 0
      USE (lcDataDir+lcSrchFile) ORDER TAG (lcSrchTag) AGAIN 
      llOpenFile = .T.
    ELSE
      llOpenFile = .F.
      SELECT (lcSrchFile)
      lnSavTag   = VAL(SYS(21))
      SET ORDER TO TAG (lcSrchTag)
    ENDIF  

    *** Prepare current object for show and search
    lcSrchVal  = PADR(lcSrchVal, FSIZE(lcFldName, lcSrchFile))
    &lcSrchObj = lcSrchVal
    lcSrchExp  = IIF(EMPTY(lcSrchExp), lcSrchVal, lcSrchExp)
    SHOW GET (lcSrchObj)

    SET EXACT ON

    *** If not seeked in file, browse
    IF llBrowse .OR. lcSrchVal <> lcOldVal ;
                     .AND. !SEEK(lcSrchExp, lcSrchFile)
      lnClosRec  = RECNO(0)
      *** If a record is to be selected 
      lcSavBrFld = lcBrFields
      lcSavTitle = lcFile_Ttl
      *** Get browse fields from dictionary
      =lfGetBrF(@lcFile_Ttl, @lcBrFields, lcSrchFile,@lcSrchFlds)
*B800456,1 M.H 02/11/96 Begin.
      IF !EMPTY(lcFileTitle)
        lcFile_Ttl = lcFileTitle
      ENDIF        
*B800456,1 M.H 02/11/96 End.
      IF BETWEEN(lnClosRec,1,RECCOUNT())
        GO lnClosRec
      ELSE
        GO TOP
      ENDIF        

      SET EXACT OFF
      =gfBrows(lcKeyCode,lcSrchFlds,'laTemp')
    
      lcBrFields=lcSavBrFld
      lcFile_Ttl=lcSavTitle
         
      *** If a selection occurs
      IF !EMPTY(laTemp[1])
        &lcSrchObj  = laTemp[1]
        llSrchFound = .T.      
      *** else, return the old value
      ELSE
        &lcSrchObj  = lcOldVal
        llSrchFound = .F.       
      ENDIF
      SHOW GET (lcSrchObj)
    *** If it is found in the file, fill array with corresponding
    *** elements. One may also use the data found on the current
    *** seekred record upon return from the function execution.
    ELSE
      IF !EMPTY(lcSrchFlds)
        =gfSubStr(lcSrchFlds,@laTemp,",")
        FOR lnCount = 1 TO ALEN(laTemp)
          laTemp[lnCount] = &laTemp[lnCount]
        ENDFOR  
      ELSE
        laTemp[1] = lcSrchVal
      ENDIF	  
      llSrchFound = .T.         
    ENDIF
    
    IF USED(lcSrchFile)
      IF llOpenFile
        USE IN (lcSrchFile)
      ELSE
        SET ORDER TO lnSavTag
      ENDIF    
    ENDIF
    SELECT IIF(!EMPTY(lcCurAlias), (lcCurAlias), 0)
  *** If the object is empty, return .F.
  ELSE
    llSrchFound = .F.         
  ENDIF
ENDIF
SET EXACT &lcSavExact
RETURN llSrchFound
