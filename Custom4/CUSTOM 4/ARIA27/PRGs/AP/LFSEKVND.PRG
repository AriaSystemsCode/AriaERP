***********************************************************************************************
*MODIFICATIONS :
*E301148,1 AMM 02/28/1999 Enhance opening and closing files.
*B124426,1 NNA 09/22/2004 Fix bug that the soft seek isn't working in the vendor browse in all
*B124426,1 NNA            payment and advanced payment screens
***********************************************************************************************
PARAMETERS lcSrchFile, lcOldVal, llBrowse, llClearFlds
*** lcSrchFile  : file to be searched, normally the base file
***               for the session
*** lcOldVal    : old value of the object
*** llBrowse    : .T. if browsing from a browse invisible button,
***               .F. otherwise
*** llClearFlds : .T. if fields are to be cleared if an
***               invalid entry is selected or the field is emptied  ,
***               .F. otherwise 
*MAN Changed the var lcSetExact to lcVenOExac
*MAN Added lcSetExact to the following statement
PRIVATE lcCurrFile,lcCurrObj, lcSetExct, llFound, llSydField, laRetVal;
        lcSavBrFlds, lcSavFlTtl, llSydFiles, lcRetFields, lnCurrTag,;
        lcIndxExp, lcMess. lnClosRec, lcVenOExac 

llFound            = .F.
lcCurrObj          = SYS(18)
lcVenOExac         = SET('EXACT')

*B124426,1 NNA 09/22/2004 (Begin) Stop this Command to let the soft seek Working
*SET EXACT  ON 
*B124426,1 NNA (End)

lnObjOrder         = 0

IF TYPE('laFields') = 'C'
  FOR lncount = 1 TO ALEN(laFields,1)
    laFields[lnCount, 1] = UPPER(STRTRAN(STRTRAN(laFields[lnCount, 1],"[","("),;
                               "]",")"))
  ENDFOR 
 lnObjOrder         = ASCAN(laFields, lcCurrObj) 
ENDIF
IF lnObjOrder > 0 
  lnObjOrder       = ASUBSCRIPT(laFields, lnObjOrder, 1)
  &lcCurrObj       = PADR(ALLTRIM(EVALUATE(SYS(18))),;
                          FSIZE(laFields[lnObjOrder,2],lcSrchFile)) 
  IF llBrowse .OR. ;
     !(ALLTRIM(UPPER(&laFields[lnObjOrder,1])) == ALLTRIM(UPPER(lcOldVal)))
    IF llBrowse .OR. !EMPTY(&laFields[lnObjOrder,1]) 
      *** Save current environment
      lcCurrFile    = ALIAS()
      lcSavBrFlds   = lcBrFields
      lcSavFlTtl    = lcFile_Ttl

      DECLARE laRetVals[ALEN(laFields,1)]
      laRetVals     = " "

      *** Get browse fields and browse title
      lcRetFields   = ""
      FOR lnFieldNum = 1 TO ALEN(laFields,1)
        lcRetFields = lcRetFields + ALLTRIM(laFields[lnFieldNum,2])+","
      ENDFOR
      lcRetFields   = SUBSTR(lcRetFields, 1, LEN(lcRetFields)-1)                            
      =lfGetBrF(@lcFile_Ttl, @lcBrFields, lcSrchFile, lcRetFields) 
                            
      SELECT (lcSrchFile)

      *** Search for the current value (search string) in the
      *** selected file after selecting the appropriate tag 
      lnCurrTag          = VAL(SYS(21))
      SET ORDER TO TAG &laFields[lnObjOrder,3]

      IF !(llBrowse .OR. '?' $ &laFields[lnObjOrder,1])
        IF SEEK(ALLTRIM(UPPER(&laFields[lnObjOrder,1])),lcSrchFile)
          llFound = .T.
          *** Check if this is the only matching record,
          *** if it is, display it, otherwise, browse key 
          IF !EOF()
            SKIP 1
            IF UPPER(EVALUATE(laFields[lnObjOrder,2])) = ;
               UPPER(EVALUATE(laFields[lnObjOrder,1])) 
              SKIP -1
              *** Browse key
              lcIndxExp = SYS(14,VAL(SYS(21)))

              *** Store the old data before browsing on a key.
              &laFields[lnObjOrder,1] = lcOldVal
              =gfBrows(lcIndxExp, lcRetFields, 'laRetVals')
              llFound = !EMPTY(laRetVals[1])
            ELSE
              SKIP -1  
              FOR lnCount = 1 TO ALEN(laFields,1)
                &laFields[lnCount,1] = &lcSrchFile..&laFields[lnCount,2]
                SHOW GET (laFields[lnCount,1])
              ENDFOR
            ENDIF  
          ENDIF
        ELSE
          lnClosRec = RECNO(0)
          *E301148,1 AMM Open files.
          lnAlais = SELECT(0)
          llOpFld = gfOpenFile(gcSysHome+'SYDFIELD','Cfld_name','SH')
          *E301148,1 AMM end
          *** Get the logical field name from the fields file to present
          *** within the message :
          lcMess     = IIF(SEEK(UPPER(laFields[lnObjOrder,2]),'SYDFIELD'), ;
                       ALLTRIM(SYDFIELD.cFld_Head)+" : ", "") + ;
                       ALLTRIM(&laFields[lnObjOrder,1])     
          *E301148,1 AMM Close files
          IF USED('SYDFIELD') .AND. llOpFld
            =gfCloseFile('SYDFIELD')
          ENDIF
          SELECT (lnAlais)
          *E301148,1 AMM end
          
          IF gfModalGen("QRM00001B00014","DIALOG",lcMess) = 1
            *** go to the closest matching record if one is found, or
            *** go to the top of the file .
            IF BETWEEN(lnClosRec,1, RECCOUNT(lcSrchFile))
              GO lnClosRec
            ELSE
              GO TOP
            ENDIF
  
            *** Call global function gfBrows
            =gfBrows(.F., lcRetFields, 'laRetVals')  
            llFound = !EMPTY(laRetVals[1])
          ENDIF   
        ENDIF   
      ELSE
        =gfBrows(.F., lcRetFields, 'laRetVals')  
        llFound = !EMPTY(laRetVals[1])
      ENDIF  
    
      *** If 'Browsed', or 'Reentered' 
      *** Check if the user pressed ESC or Cancelled, or Reentered
      *** by comparing laData elemet by its value before browsing (if browsed)
      IF !llFound 
        IF llClearFlds
          FOR lnCount = 1 TO ALEN(laFields,1)
            &laFields[lnCount,1] = SPACE(FSIZE(laFields[lnCount,2], lcSrchFile))
            SHOW GET (laFields[lnCount,1])
          ENDFOR
        ELSE
          &laFields[lnObjOrder,1] = lcOldVal
          SHOW GET (lcCurrObj)
        ENDIF            
        _CUROBJ = OBJNUM(&lcCurrObj)
      ELSE
        IF !EMPTY(laRetVals[1])
          FOR lnCount = 1 TO ALEN(laFields,1)
            &laFields[lnCount,1] = laRetVals[lnCount]
            SHOW GET (laFields[lnCount,1])
          ENDFOR
        ENDIF  
      ENDIF

      *** Restore environment and return llFound
      *** Save current environment
      lcBrFields         = lcSavBrFlds
      lcFile_Ttl         = lcSavFlTtl
      SET ORDER TO lnCurrTag
      SELECT (lcCurrFile)
    ELSE
      IF llClearFlds
        FOR lnCount = 1 TO ALEN(laFields,1)
          &laFields[lnCount,1] = SPACE(FSIZE(laFields[lnCount,2], lcSrchFile))
          SHOW GET (laFields[lnCount,1])
        ENDFOR
      ELSE
        &laFields[lnObjOrder,1] = lcOldVal
        _CUROBJ = OBJNUM(&lcCurrObj)
        SHOW GET (lcCurrObj)      
      ENDIF
    ENDIF
  ELSE
    &laFields[lnObjOrder,1] = lcOldVal
    SHOW GET (lcCurrObj)          
  ENDIF    
ENDIF  
SET EXACT &lcVenOExac
RETURN llFound
