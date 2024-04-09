*!**************************************************************************
*!
*!      Function:  lfGetAdr
*!
*!**************************************************************************
*  Gets address according to the address code, returns address
*
FUNCTION lfGetAdr
PARAMETERS lcAlias, lcTag, lcKeyCode, lcAdrCode
*** lcAlias   : source file name 
*** lcTag     : source file tag that is to be used in seeking
*** lckeycode : search key code (of the source file) (optional)
*** lcAdrCode : address code (optional)

PRIVATE lnSavIntTg, lnSavCmpTg, lcCurAlias, lnOldTag,;
        llOpenInt, llOpenCmp, llContinue, laAddress

DECLARE laAddress[3,2]
laAddress = " "

lcTag = IIF(TYPE('lcTag') = 'C' .AND. !EMPTY(lcTag),lcTag,;
            IIF(TYPE('lcAlias') = 'C' .AND. ALIAS() = lcAlias,;
            SYS(22),"")) 
IF TYPE('lcAlias') = 'C' .AND. !EMPTY(lcAlias) .AND.;
  !EMPTY(lcTag)

  llContinue = .T.
  lcCurAlias = ALIAS()
  
  SELECT (lcAlias)
  lnOldTag = VAL(SYS(21))   
  SET ORDER TO TAG (lcTag)
    
  lcAdrCode = IIF(TYPE('lcAdrCode') = 'C'.AND. !EMPTY(lcAdrCode), lcAdrCode,;
                  &lcAlias..cCont_Code)
  IF (TYPE('lcKeyCode') = 'C' .AND. !SEEK(lcKeyCode,lcAlias)) ;
     .OR. lcAdrCode <> &lcAlias..cCont_Code
    llContinue = .F.  
  ENDIF
  
  IF llContinue
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

    IF SEEK(&lcAlias..cCont_Code,'SYCINT');
      .OR. (SEEK(gcAct_Comp,'SYCCOMP') ;
           .AND. SEEK(SYCCOMP.cCont_Code,'SYCINT'))
      laAddress[1,1] = SYCINT.nPart3Ord
      laAddress[1,2] = &lcAlias..cAddress3
      laAddress[2,1] = SYCINT.nPart4Ord
      laAddress[2,2] = &lcAlias..cAddress4
      laAddress[3,1] = SYCINT.nPart5Ord
      laAddress[3,2] = &lcAlias..cAddress5
      =ASORT(laAddress,1)
    ELSE
      laAddress[1,2] = &lcAlias..cAddress3
      laAddress[2,2] = &lcAlias..cAddress4
      laAddress[3,2] = &lcAlias..cAddress5
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
  ENDIF    
  SET ORDER TO lnOldTag IN (lcAlias)
  SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
ENDIF  

*RETURN PADR(RTRIM(laAddress[1,2]) + RTRIM(laAddress[2,2]) +;
       RTRIM(laAddress[3,2]),40)

RETURN ALLTRIM(laAddress[1,2]) +" "+ ALLTRIM(laAddress[2,2]) + " " + ALLTRIM(laAddress[3,2])