*:************************************************************************
*: Program file  : MFCSTSZ.PRG
*: Program desc. : Screen To Select Sizes.
*: For screen    : MFCSTSZ.SCX
*:         System: ARIA BUSINESS SYSTEM
*:         Module: Manufactering Module
*:      Developer: Reham Al-Allamy
*:************************************************************************
*: Documented *E300722,1 Rewrite the program to work in the 2.7 version
*:************************************************************************
*: Calls         : 
*:         Procedures : None
*:         Programs   : None
*:         Screens    : MFCSTZ1.SPX (Select sizes for fabric, trim, duty....)
*:                      MFCSTZ2.SPX (Select sizes for style component)
*:         Functions  : lfvGetSiz1
*:                      lfvAllSiz
*:                      lfvSelSiz
*:                      lfShwSiz1
*:                      lfvSizOk1
*:                      lfvGetSiz2
*:                      lfShwSiz2
*:                      lfvPrntSiz
*:                      lfvChldSiz
*:************************************************************************
*:  Passed Parameters : lcCstType:S    -> Style Component-> Call MFCSTZ2.Scx
*:                                FTMPD-> Fabric,trim,Mfg-> Call MFCSTZ1.Scx
*:************************************************************************
*: Example            :
*:  Call the size program & screen.
*:  DO (gcAppHome+"MFCSTSZ") WITH &lcTmpBom..cCatgTyp
*:************************************************************************
*:  Modifications :
*:B606979,1 AMH 02/25/2002 Fix the bug of scale code error in case of style componant
*:B606979,1 AMH            for extended size scale style.
*:************************************************************************
*
PARAMETERS lcCstType
PRIVATE lcCstType , lcExclSave

*-- Save the current exclusive setting.
lcExclSave = SET("EXCLUSIVE")
SET EXCLUSIVE ON

IF lcCstType = "S"
  *-- If the cost item type is : (Style component)
  DO (gcScrDir+"MFCSTZ2.SPX")
ELSE
  *-- If the cost item type is : (Fabric - Trim - Mfg - Price - Duty)
  DO (gcScrDir+"MFCSTZ1.SPX")
ENDIF

*-- Restore the exclusive setting.
SET EXCLUSIVE &lcExclSave

*!*************************************************************
*****             Functions for MFCSTZ1 Screen            *****
***** Mover to select specific sizes to the created masks *****
*!*************************************************************

*!*************************************************************
*! Name      : lfvGetSiz1
*! Developer : Reham Al-Allamy
*! Date      : 09/02/1997
*! Purpose   : Function for scale popup in MFCSTZ1.SPX
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvGetSiz1()
*!*************************************************************
*
FUNCTION lfvGetSiz1
PRIVATE lnCnt , lcCnt

IF !(lnOldScale == puScale1)
  *-- Save the selected sizes for the previous popup bar.
  IF !EMPTY(laSelSiz[1])
    laScale[lnOldScale,2] = ""
    FOR lnCnt = 1 TO ALEN(laSelSiz,1)
      laScale[lnOldScale,2] = laScale[lnOldScale,2] + ;
                              IIF(!EMPTY(laScale[lnOldScale,2]) , "," , "") + ;
                              RIGHT(laSelSiz[lnCnt],1)
    ENDFOR
  ELSE
    laScale[lnOldScale,2] = ""
  ENDIF
  
  *-- Select the sizes for the new popup bar.
  DECLARE laAllSiz[1,1] , laSelSiz[1,1]
  STORE "" TO laAllSiz , laSelSiz
  IF SEEK("S" + SUBSTR(laScale[puScale1,3],1,3) , "SCALE")
    FOR lnCnt = 1 TO Scale.Cnt
      lcCnt = STR(lnCnt,1)
      *-- Fill the source array.
      IF !EMPTY(laAllSiz[1,1])
        DECLARE laAllSiz[ALEN(laAllSiz,1)+1,1]
      ENDIF
      laAllSiz[ALEN(laAllSiz,1),1] = PADR(SZ&lcCnt,40) + lcCnt
      
      *-- Fill the target array.
      IF lcCnt $ laScale[puScale1,2]
        IF !EMPTY(laSelSiz[1,1])
          DECLARE laSelSiz[ALEN(laSelSiz,1)+1,1]
        ENDIF
        laSelSiz[ALEN(laSelSiz,1),1] = PADR(SZ&lcCnt,40) + lcCnt
      ENDIF
    ENDFOR
    
    *-- Define the source sizes list popup.
    FOR lnCnt = 1 TO 8
      IF lnCnt <= ALEN('laAllSiz',1)
        DEFINE BAR lnCnt OF puAllSiz PROMPT (ALLTRIM(laAllSiz[lnCnt,1]))
        IF ASCAN('laSelSiz',ALLTRIM(laAllSiz[lnCnt,1])) > 0
          SET SKIP OF BAR lnCnt OF puAllSiz .T.
        ENDIF
      ELSE
        RELEASE BAR lnCnt OF puAllSiz
      ENDIF
    ENDFOR
  ELSE
    puScale1 = lnOldScale
  ENDIF
  SHOW GETS
ENDIF

*!*************************************************************
*! Name      : lfvAllSiz
*! Developer : Reham Al-Allamy
*! Date      : 09/03/1997
*! Purpose   : Function for moving sizes in MFCSTZ1.SPX
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvAllSiz()
*!*************************************************************
*
FUNCTION lfvAllSiz

IF lsAllSiz <= ALEN('laAllSiz',1) AND lsAllSiz <> 0
  *-- Disable the bar in the source array.
  SET SKIP OF BAR lsAllSiz OF puAllSiz .T.
  
  *-- Increase the target dimension with 1.
  IF !EMPTY(laSelSiz[1,1])
    DIMENSION laSelSiz [ALEN(laSelSiz,1) +1,1]
  ENDIF
  laSelSiz[ALEN(laSelSiz,1),1] = ALLTRIM(laAllSiz[lsAllSiz,1])
ENDIF

lnStart  = lsAllSiz
lsAllSiz = 0

FOR lnCount = lnStart TO CNTBAR('puAllSiz')
  IF !SKPBAR('puAllSiz',lnCount)
    lsAllSiz = lnCount
    EXIT
  ENDIF
ENDFOR

IF lsAllSiz = 0
  FOR lnCount = 1 TO CNTBAR('puAllSiz')
    IF !SKPBAR('puAllSiz',lnCount)
      lsAllSiz = lnCount
      EXIT
    ENDIF
  ENDFOR
ENDIF

*-- Refresh the lists.
SHOW GETS
_CUROBJ = OBJNUM(lsAllSiz)

*!*************************************************************
*! Name      : lfvSelSiz
*! Developer : Reham Al-Allamy
*! Date      : 09/03/1997
*! Purpose   : Function for removing sizes in MFCSTZ1.SPX
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSelSiz()
*!*************************************************************
*
FUNCTION lfvSelSiz

IF lsSelSiz <= ALEN('laSelSiz',1) AND lsSelSiz <> 0
  *-- Get the removed bar #. in the source array.
  lsAllSiz  = IIF(ASCAN('laAllSiz',laSelSiz[lsSelSiz,1]) > 0 ,;
                  ASUBSCRIPT('laAllSiz',;
                  ASCAN('laAllSiz',laSelSiz[lsSelSiz,1]),1),1)
  
  *-- Enable the bar in the source array.
  SET MARK OF POPUP puAllSiz .F.
  SET SKIP OF BAR lsAllSiz OF puAllSiz .F.
  
  *-- Delete the array row from the target array.
  =ADEL(laSelSiz,lsSelSiz)
  IF ALEN(laSelSiz,1) > 1
    DIMENSION laSelSiz [ALEN(laSelSiz,1) -1,1]
  ELSE
    STORE " " TO laSelSiz
  ENDIF
ENDIF

*-- Refresh the lists.
lsSelSiz = 1
SHOW GETS
_CUROBJ = OBJNUM(lsSelSiz)

*!*************************************************************
*! Name      : lfShwSiz1
*! Developer : Reham Al-Allamy
*! Date      : 09/14/1997
*! Purpose   : Function called from all the size screen objects
*!           : to refresh the objects in screen MFCSTZ1.SCX
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfShwSiz1()
*!*************************************************************
*
FUNCTION lfShwSiz1

*-- If all the items selected, disable all items list & move button.
IF ALEN(laSelSiz,1) = ALEN(laAllSiz,1) .AND. !EMPTY(laSelSiz[1]) ;
  .OR. EMPTY(laAllSiz[1])
  SHOW GET lsAllSiz DISABLE
  SHOW GET pbMove   DISABLE
ELSE
  *-- If some of the items selected, enable the all items list & move button.
  SHOW GET lsAllSiz ENABLE
  SHOW GET pbMove   ENABLE
ENDIF

*-- If there is no selected items, disable the select list & remove button.
IF EMPTY(laSelSiz[1])
  SHOW GET lsSelSiz DISABLE
  SHOW GET pbRemove DISABLE
ELSE
  *-- If there is selected items, enable the select list & remove button.
  SHOW GET lsSelSiz ENABLE
  SHOW GET pbRemove ENABLE
ENDIF

*!*************************************************************
*! Name      : lfvSizOk1
*! Developer : Reham Al-Allamy
*! Date      : 09/03/1997
*! Purpose   : Valid function of the OK button in MFCSTZ1.SPX
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvSizOk1()
*!*************************************************************
*
FUNCTION lfvSizOk1
PRIVATE lnCnt

llSaveSize = .T.

*-- Save the selected sizes for the previous popup bar.
IF !EMPTY(laSelSiz[1])
  laScale[puScale1,2] = ""
  FOR lnCnt = 1 TO ALEN(laSelSiz,1)
    laScale[puScale1,2] = laScale[puScale1,2] + ;
                            IIF(!EMPTY(laScale[puScale1,2]) , "," , "") + ;
                            RIGHT(laSelSiz[lnCnt],1)
  ENDFOR
ELSE
  laScale[puScale1,2] = ""
ENDIF


*!*************************************************************
*****             Functions for MFCSTZ2 Screen            *****
*****     Select specific sizes to the created masks      *****
*!*************************************************************

*!*************************************************************
*! Name      : lfvGetSiz2
*! Developer : Reham Al-Allamy
*! Date      : 10/13/1997
*! Purpose   : Function for scale popup in MFCSTZ2.SPX
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvGetSiz2()
*!*************************************************************
*
FUNCTION lfvGetSiz2
PRIVATE lnCnt , lcCnt

IF !(lnOldScale == puScale2)
  IF SEEK("S" + SUBSTR(laScale[puScale2,3],1,3) , "SCALE")
    = lfSizeRef()
    *-- Fill the source array with the sizes of the first scale in the popup.
    lnCurScCnt = Scale.Cnt
    *-- Get the started array element of the current parent scale.
    lnAryStart = IIF(ASCAN(laMainScal , "*"+SUBSTR(laScale[puScale2,3],1,3)) > 0 , ;
                     ASUBSCRIPT('laMainScal' , ASCAN(laMainScal , "*"+SUBSTR(laScale[puScale2,3],1,3)) , 1) , 0)
    FOR lnCnt = 1 TO 8
      lcCnt = STR(lnCnt,1)
      IF lnCnt <= lnCurScCnt
        *-- Store the size description in the check box prompt.
        lcPrmt&lcCnt = PADR(Scale.SZ&lcCnt,6)
        IF !EMPTY(laMainScal[lnAryStart,2])
          *-- Default the check box to be checked.
          cbPrntSz&lcCnt = .T.
          
          *-- Save record pointer in the scale file.
          lnSclRecNo = RECNO("SCALE")
          
          =SEEK("S"+SUBSTR(laMainScal[lnAryStart,2],2,3) , "SCALE")
          lcChildNo = SUBSTR(laMainScal[lnAryStart,2],6,1)
          
          *B606979,1 AMH Get the correct scale width [Start]
          *lcScanExp = PADR(Scale.Scale,1)+" * "+PADR(Scale.cScl_Desc,10)+" * "+PADR(Scale.SZ&lcChildNo,5)
          lcScanExp = PADR(Scale.Scale,3)+" * "+PADR(Scale.cScl_Desc,10)+" * "+PADR(Scale.SZ&lcChildNo,5)
          *B606979,1 AMH [End]
          
          *-- Refresh & enable the child size popup with its value.
          puChldSz&lcCnt = IIF(ASCAN(laChldSz , lcScanExp) > 0 , ASUBSCRIPT('laChldSz' , ASCAN(laChldSz , lcScanExp) , 1) , 1)
          SHOW GET puChldSz&lcCnt ENABLE
          
          *-- Restore record pointer in scale file.
          IF lnSclRecNo > 0 .AND. lnSclRecNo <= RECCOUNT("SCALE")
            GOTO lnSclRecNo IN SCALE
          ENDIF
        ELSE
          *-- Default the check box to be unchecked.
          cbPrntSz&lcCnt = .F.
          *-- Refresh & enable the child size popup with its value.
          puChldSz&lcCnt = 1
          SHOW GET puChldSz&lcCnt DISABLE
        ENDIF
        *-- Refresh & enable the check box of the parent size with the its prompt.
        SHOW GET cbPrntSz&lcCnt,1 PROMPT lcPrmt&lcCnt ENABLE
          
      ELSE
        *-- Fill the parent size check box with blank.
        lcPrmt&lcCnt = "N/A   "
        *-- Get the check box to be unchecked.
        cbPrntSz&lcCnt = .F.
        *-- Refresh & disable the check box of the parent size with the its prompt.
        SHOW GET cbPrntSz&lcCnt,1 PROMPT lcPrmt&lcCnt DISABLE
        *-- Force the popup to be N/A.
        puChldSz&lcCnt  = 1
        *-- Refresh & disable the child size popup with its value.
        SHOW GET puChldSz&lcCnt DISABLE
      ENDIF
      lnAryStart = lnAryStart + 1
    ENDFOR
  ELSE
    puScale2 = lnOldScale
    SHOW GET puScale2
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfShwSiz2
*! Developer : Reham Al-Allamy
*! Date      : 10/13/1997
*! Purpose   : Function called from all the size screen objects
*!           : to refresh the objects in screen MFCSTZ2.SCX
*!*************************************************************
*! Calls     : lfSizeRef
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfShwSiz2()
*!*************************************************************
*
FUNCTION lfShwSiz2
PRIVATE lnCnt , lcCnt

= lfSizeRef()

*-- Fill the source array with the sizes of the first scale in the popup.
lnCurScCnt = IIF(SEEK("S" + SUBSTR(laScale[puScale2,3],1,3) , "SCALE") , Scale.Cnt , 0)
*-- Get the started array element of the current parent scale.
lnAryStart = IIF(ASCAN(laMainScal , "*"+SUBSTR(laScale[puScale2,3],1,3)) > 0 , ;
                 ASUBSCRIPT('laMainScal' , ASCAN(laMainScal , "*"+SUBSTR(laScale[puScale2,3],1,3)) , 1) , 0)

*-- Fill the prompt of the parent check boxes sizes.
FOR lnCnt = 1 TO 8
  lcCnt   = STR(lnCnt,1)
  *-- Give the child size popup the counter no.
  puChldSz&lcCnt = lnCnt + 1
  IF lnCnt <= lnCurScCnt
    *-- Store the size description in the check box prompt.
    lcPrmt&lcCnt   = PADR(Scale.SZ&lcCnt,5)
    IF !EMPTY(laMainScal[lnAryStart,2])
      *-- Default the check box to be checked.
      cbPrntSz&lcCnt = .T.
      *-- Save record pointer in the scale file.
      lnSclRecNo = RECNO("SCALE")
      
      =SEEK("S"+SUBSTR(laMainScal[lnAryStart,2],2,3) , "SCALE")
      lcChildNo = SUBSTR(laMainScal[lnAryStart,2],6,1)
      
      *B606979,1 AMH Get the correct scale width [Start]
      *lcScanExp = PADR(Scale.Scale,1)+" * "+PADR(Scale.cScl_Desc,10)+" * "+PADR(Scale.SZ&lcChildNo,5)
      lcScanExp = PADR(Scale.Scale,3)+" * "+PADR(Scale.cScl_Desc,10)+" * "+PADR(Scale.SZ&lcChildNo,5)
      *B606979,1 AMH [End]
      
      *-- Refresh & enable the child size popup with its value.
      puChldSz&lcCnt = IIF(ASCAN(laChldSz , lcScanExp) > 0 , ASUBSCRIPT('laChldSz' , ASCAN(laChldSz , lcScanExp) , 1) , 1)
      SHOW GET puChldSz&lcCnt ENABLE
      
      *-- Restore record pointer in scale file.
      IF lnSclRecNo > 0 .AND. lnSclRecNo <= RECCOUNT("SCALE")
        GOTO lnSclRecNo IN SCALE
      ENDIF
    ELSE
      *-- Default the check box to be checked.
      cbPrntSz&lcCnt = .F.
      *-- Refresh & disable the child size popup with its value.
      puChldSz&lcCnt = 1
      SHOW GET puChldSz&lcCnt DISABLE
    ENDIF
    
    *-- Refresh & enable the check box of the parent size with the its prompt.
    SHOW GET cbPrntSz&lcCnt,1 PROMPT lcPrmt&lcCnt ENABLE
  ELSE
    *-- Fill the parent size check box with blank.
    lcPrmt&lcCnt   = "N/A   "
    *-- Default the check box to be checked.
    cbPrntSz&lcCnt = .F.
    *-- Refresh & disable the check box of the parent size with the its prompt.
    SHOW GET cbPrntSz&lcCnt,1 PROMPT lcPrmt&lcCnt DISABLE
    *-- Refresh & disable the child size popup with its value.
    puChldSz&lcCnt = 1
    SHOW GET puChldSz&lcCnt DISABLE
  ENDIF
  lnAryStart = lnAryStart + 1
ENDFOR

*!*************************************************************
*! Name      : lfvPrntSiz
*! Developer : Reham Al-Allamy
*! Date      : 10/23/1997
*! Purpose   : Function called from all the parent check boxes'
*!           : sizes in screen MFCSTZ2.Scx
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lnPrntSzNo -> No of parent check box size.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvPrntSiz()
*!*************************************************************
*
FUNCTION lfvPrntSiz
PARAMETERS lnPrntSzNo
PRIVATE lnPrntSzNo , lcPrntSzNo

lcPrntSzNo = STR(lnPrntSzNo,1)
*-- Define the expersion to be scaned.
lcScanExp = "*"+SUBSTR(laScale[puScale2,3],1,3)+","+lcPrntSzNo
*-- Get the started array element of the current parent scale.
lnAryStart = IIF(ASCAN(laMainScal , lcScanExp) > 0 , ;
             ASUBSCRIPT('laMainScal',ASCAN(laMainScal , lcScanExp) , 1),0)

*-- If the check box was checked.
IF cbPrntSz&lcPrntSzNo
  laMainScal[lnAryStart,2] = "#"+IIF(lnAryStart+1 <= ALEN(laChldSz,1) , ;
        SUBSTR(laChldSz[lnAryStart+1,3],1,3)+","+STR(laChldSz[lnAryStart+1,2],1) , ;
        SUBSTR(laChldSz[ALEN(laChldSz,1),3],1,3)+","+STR(laChldSz[ALEN(laChldSz,1),2],1) )
  
  *-- Refresh & disable the child size popup with its value.
  puChldSz&lcPrntSzNo = IIF(lnAryStart+1 <= ALEN(laChldSz,1) , lnAryStart+1 , ALEN(laChldSz,1) )
  SHOW GET puChldSz&lcPrntSzNo ENABLE
ELSE
*-- If the check box was unchecked.
  *-- Blank the assigning of the sizes.
  laMainScal[lnAryStart,2] = ""
  
  *-- Refresh & disable the child size popup with its value.
  puChldSz&lcPrntSzNo = 1
  SHOW GET puChldSz&lcPrntSzNo DISABLE
ENDIF

*!*************************************************************
*! Name      : lfvChldSiz
*! Developer : Reham Al-Allamy
*! Date      : 10/13/1997
*! Purpose   : Function called from all the child popups' sizes
*!           : in screen MFCSTSZ.Scx
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lnChldSzNo -> No. of child poup.
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfvChldSiz()
*!*************************************************************
*
FUNCTION lfvChldSiz
PARAMETERS lnChldSzNo
PRIVATE lnChldSzNo , lcChldSzNo

lcChldSzNo = STR(lnChldSzNo,1)

*-- Define the expersion to be scaned.
lcScanExp = "*"+SUBSTR(laScale[puScale2,3],1,3)+","+lcChldSzNo
*-- Get the started array element of the current parent scale.
lnAryStart = IIF(ASCAN(laMainScal , lcScanExp) > 0 , ;
             ASUBSCRIPT('laMainScal',ASCAN(laMainScal , lcScanExp) , 1),0)

*-- If the poup value was "N/A"
IF puChldSz&lcChldSzNo = 1
  laMainScal[lnAryStart,2] = ""
  *-- Uncheck the parent size check box.
  cbPrntSz&lcChldSzNo  = .F.
  *-- Refresh the check box.
  SHOW GET cbPrntSz&lcChldSzNo
  *-- Disable the popup.
  SHOW GET puChldSz&lcChldSzNo DISABLE
ELSE
  laMainScal[lnAryStart,2] = "#"+SUBSTR(laChldSz[puChldSz&lcChldSzNo,3],1,3) + ;
                             "," + STR(laChldSz[puChldSz&lcChldSzNo,2],1)

  *-- Check the parent size check box.
  cbPrntSz&lcChldSzNo  = .T.
  *-- Refresh the check box.
  SHOW GET cbPrntSz&lcChldSzNo
  *-- Enable the popup.
  SHOW GET puChldSz&lcChldSzNo ENABLE
ENDIF
