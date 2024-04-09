*!**************************************************************************
*!
*!      Function:  lfGetAdr
*!
*!**************************************************************************
*  Gets address according to the address code, returns address
*
FUNCTION gfGetAdr
PARAMETERS lcAlias, lcTag, lcKeyCode, lcAdrCode,lnLineNo
*** lcAlias   : source file name 
*** lcTag     : source file tag that is to be used in seeking
*** lckeycode : search key code (of the source file) (optional)
*** lcAdrCode : address code (optional)
*** lnLineNo  : The Address line number to return

PRIVATE lnSavIntTg, lnSavCmpTg, lcCurAlias, lnOldTag,;
        llOpenInt, llOpenCmp, llContinue, laAddress
*MAN Start
 
 * You have to send the source file and 1 or more from the following parameters
 * 1 - The alias name for the source file or you have it the currently selected
 * 2 - Address code to be used in getting the address line  OR
 * 3 - Tag name and Seek Expression to get the  Address code
 * 4 - You can have the source file opened with the proper tag and just send
 *     the seek expr. (In case of not sending Tag ID there must be an active one)      
 
 IF EMPTY(lcAlias) .OR. TYPE('lcTag') <> 'C'
   IF EMPTY(ALIAS())
     RETURN .F.
   ELSE   
     lcAlias = ALIAS()
   ENDIF  
 ENDIF
 
 lcCurAlias = ALIAS()
 SELECT (lcAlias)
 *-- No Address code has been sent
 IF EMPTY(lcAdrCode) 
   IF !EMPTY(lcKeyCode) .AND. TYPE('lcKeyCode') <> 'C'
     DO CASE

       *-- A Search Expr has been sent and no Tag Has been Sent and no active tag
       CASE (EMPTY(lcTag) .OR. TYPE('lcTag') <> 'C') AND EMPTY(SYS(22))
       SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
       RETURN .F.

       *-- A Search Expr and a Tag ID have been sent 
       CASE !EMPTY(lcTag)
        lnOldTag = VAL(SYS(21))   
        SET ORDER TO TAG (lcTag)
        *-- The Search expr is not found
        IF !SEEK(lcKeyCode)
          SET ORDER TO lnOldTag
          SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
          RETURN .F.
        ENDIF

       *-- A search expr has been sent without a Tag 
       OTHERWISE 
         *-- There is no active tag
         IF EMPTY(SYS(22)) 
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
           RETURN .F.
         ENDIF
         *-- The Search Expr. is not found
         IF !SEEK(lcKeyCode)
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
           RETURN .F.
         ENDIF
     ENDCASE 
   ENDIF  

   *-- Just to be able to set the old tag even if it has not been 
   *-- changed in the above DO CASE
   lnOldTag = VAL(SYS(21))   

   lcAdrCode = cCont_Code
 ENDIF

DECLARE laAddress[6,3]
laAddress = " "
lnLineNo  = IIF(TYPE('lnLineNo')='N' AND BETWEEN(lnLineNo,1,5),INT(lnLineNo),1)
STORE .F. TO llOpenInt, llOpenCmp
*** Check being on a correct alias
   
IF !USED('SYCINT')  && Check if the internationals file is open or not.
  llOpenInt  = .T.     && Indicates that the file is open by the function.
  ** Use the file and assign the index.
  USE &gcSysHome.SYCINT ORDER TAG cContCode IN 0 
ELSE
  SELECT SYCINT       
  lnSavIntTg = VAL(SYS(21))
  SET ORDER TO TAG cContCode   && Change the order
ENDIF  

IF !USED('SYCCOMP')  && Check if the internationals file is open or not.
  llOpenCmp  = .T.     && Indicates that the file is open by the function.
  ** Use the file and assign the index.
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0 
ELSE
  SELECT SYCCOMP       
  lnSavCmpTg = VAL(SYS(21))
  SET ORDER TO TAG cComp_ID   && Change the order
ENDIF  

IF SEEK(lcAdrCode,'SYCINT') .OR. (SEEK(gcAct_Comp,'SYCCOMP') ;
   .AND.  SEEK(SYCCOMP.cCont_Code,'SYCINT'))
  laAddress[1,1] = SYCINT.nPart1Ord
  laAddress[1,2] = &lcAlias..cAddress1
  laAddress[1,3] = SYCINT.nPart1LEN
  laAddress[2,1] = SYCINT.nPart2Ord
  laAddress[2,2] = &lcAlias..cAddress2
  laAddress[2,3] = SYCINT.nPart2LEN
  laAddress[3,1] = SYCINT.nPart3Ord
  laAddress[3,2] = &lcAlias..cAddress3
  laAddress[3,3] = SYCINT.nPart3LEN      
  laAddress[4,1] = SYCINT.nPart4Ord
  laAddress[4,2] = &lcAlias..cAddress4      
  laAddress[4,3] = SYCINT.nPart4LEN      
  laAddress[5,1] = SYCINT.nPart5Ord
  laAddress[5,2] = &lcAlias..cAddress5
  laAddress[5,3] = SYCINT.nPart5LEN      
  laAddress[6,1] = SYCINT.nPart6Ord
  laAddress[6,2] = &lcAlias..cAddress6
  laAddress[6,3] = SYCINT.nPart6LEN            
      
  =ASORT(laAddress,1)
  lcRetVal=''
  FOR lnCount = 1 TO ALEN(laAddress,1)
    IF laAddress[lnCount,1] = lnLineNo
      *lcRetVal=lcRetVal+IIF(EMPTY(lcRetVal),'',',')+PADR(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]),laAddress[lnCount,3])
      lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
      lcRetVal  = lcRetVal+IIF(EMPTY(lcRetVal) .OR. RIGHT(lcRetVal,1) = ',' ,'',', ') + lcAddPart
     ENDIF
  ENDFOR
ELSE
  lcLineNo=STR(lnLineNo,1)
  lcRetVal=&lcAlias..cAddress&lcLineNo
ENDIF  

IF USED('SYCCOMP')
  IF llOpenCmp 
    USE IN SYCCOMP
  ELSE
    SET ORDER TO lnSavCmpTg IN SYCCOMP  
  ENDIF
ENDIF
  
IF USED('SYCINT')
  IF llOpenInt
    USE IN SYCINT
  ELSE
    SET ORDER TO lnSavIntTg IN SYCCOMP  
  ENDIF  
ENDIF
SET ORDER TO lnOldTag IN (lcAlias)
SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  

*ENDMAN

* MAN Remarked all the following lines
*DECLARE laAddress[6,3]
*laAddress = " "

*lcTag = IIF(TYPE('lcTag') = 'C' .AND. !EMPTY(lcTag),lcTag,;
            IIF(TYPE('lcAlias') = 'C' .AND. ALIAS() = lcAlias,;
            SYS(22),"")) 
*IF TYPE('lcAlias') = 'C' .AND. !EMPTY(lcAlias) .AND.;
  !EMPTY(lcTag)

*  llContinue = .T.
*  lcCurAlias = ALIAS()
  
*  SELECT (lcAlias)
*  lnOldTag = VAL(SYS(21))   
*  SET ORDER TO TAG (lcTag)
    
*  lcAdrCode = IIF(TYPE('lcAdrCode') = 'C'.AND. !EMPTY(lcAdrCode), lcAdrCode,;
                  &lcAlias..cCont_Code)
*  IF (TYPE('lcKeyCode') = 'C' .AND. !SEEK(lcKeyCode,lcAlias)) ;
     .OR. lcAdrCode <> &lcAlias..cCont_Code
*   llContinue = .F.  
*  ENDIF
  
*  IF llContinue
*    STORE .F. TO llOpenInt, llOpenCmp
    *** Check being on a correct alias
   
*    IF !USED('SYCINT')  && Check if the internationals file is open or not.
*      llOpenInt  = .T.     && Indicates that the file is open by the function.
      ** Use the file and assign the index.
*      USE &gcSysHome.SYCINT ORDER TAG cContCode IN 0 
*    ELSE
*      SELECT SYCINT       
*      lnSavIntTg = VAL(SYS(21))
*      SET ORDER TO TAG cContCode   && Change the order
*    ENDIF  

*    IF !USED('SYCCOMP')  && Check if the internationals file is open or not.
*      llOpenCmp  = .T.     && Indicates that the file is open by the function.
      ** Use the file and assign the index.
*      USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0 
*    ELSE
*      SELECT SYCCOMP       
*      lnSavCmpTg = VAL(SYS(21))
*      SET ORDER TO TAG cComp_ID   && Change the order
*    ENDIF  

*    IF SEEK(&lcAlias..cCont_Code,'SYCINT');
      .OR. (SEEK(gcAct_Comp,'SYCCOMP') ;
           .AND. SEEK(SYCCOMP.cCont_Code,'SYCINT'))
*      laAddress[1,1] = SYCINT.nPart1Ord
*      laAddress[1,2] = &lcAlias..cAddress1
*      laAddress[1,3] = SYCINT.nPart1LEN
*      laAddress[2,1] = SYCINT.nPart2Ord
*      laAddress[2,2] = &lcAlias..cAddress2
*      laAddress[2,3] = SYCINT.nPart2LEN
*      laAddress[3,1] = SYCINT.nPart3Ord
*      laAddress[3,2] = &lcAlias..cAddress3
*      laAddress[3,3] = SYCINT.nPart3LEN      
*      laAddress[4,1] = SYCINT.nPart4Ord
*      laAddress[4,2] = &lcAlias..cAddress4      
*      laAddress[4,3] = SYCINT.nPart4LEN      
*      laAddress[5,1] = SYCINT.nPart5Ord
*      laAddress[5,2] = &lcAlias..cAddress5
*      laAddress[5,3] = SYCINT.nPart5LEN      
*      laAddress[6,1] = SYCINT.nPart6Ord
*      laAddress[6,2] = &lcAlias..cAddress6
*      laAddress[6,3] = SYCINT.nPart6LEN            
      
*      =ASORT(laAddress,1)
*      lcRetVal=''
*      FOR lnCount = 1 TO ALEN(laAddress,1)
*        IF laAddress[lnCount,1] = lnLineNo
          *lcRetVal=lcRetVal+IIF(EMPTY(lcRetVal),'',',')+PADR(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]),laAddress[lnCount,3])
*           lcRetVal=lcRetVal+IIF(EMPTY(lcRetVal),'',',')+ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
*        ENDIF
*      ENDFOR
*    ELSE
*       lcLineNo=STR(lnLineNo,1)
*       lcRetVal=&lcAlias..cAddress&lcLineNo
*    ENDIF  

*    IF USED('SYCCOMP')
*      IF llOpenCmp 
*        USE IN SYCCOMP
*      ELSE
*        SET ORDER TO lnSavCmpTg IN SYCCOMP  
*      ENDIF
*    ENDIF
  
*    IF USED('SYCINT')
*      IF llOpenInt
*        USE IN SYCINT
*      ELSE
*        SET ORDER TO lnSavIntTg IN SYCCOMP  
*      ENDIF  
*    ENDIF
*  ENDIF    
*  SET ORDER TO lnOldTag IN (lcAlias)
*  SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
*ENDIF  


*RETURN PADR(RTRIM(laAddress[1,2]) + RTRIM(laAddress[2,2]) +;
       RTRIM(laAddress[3,2]),40)

RETURN lcRetVal





