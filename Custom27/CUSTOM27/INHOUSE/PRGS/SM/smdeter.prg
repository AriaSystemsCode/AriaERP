*:********************************************************************
*: Procedure file: SMDETER.PRG 
*:                 (Copy of Codes program) with modifications
*:                 to generate errors
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Hesham El-Sheltawi 
*:      Copyright (c) 
*:********************************************************************
*:E000000,1 
*: Modification :

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc,laCtrStat
PRIVATE lnOldRecNo
DECLARE laKeyField [1,4],laRelFld[1,14],laSeqNo[1,2],laComMdls[1,1]
DECLARE laORelFld[1],laScrError[1,2]
STORE '' TO laScrError
IF FILE(gcSysHome+'ERROR.MEM')
   RESTORE FROM (gcSysHome+'ERROR.MEM') ADDI
ENDIF
=ASORT(laScrError,2)
=lfGenError(1)



DIMENSION laComp[1], laCodes[1], laCodInfo[1,10]
laCodInfo    = SPACE(1)
laComp       = SPACE(1)
laCodes      = SPACE(1)
lcBrowTitl   = "Codes"
lcRltField = SPACE(1)
STORE SPACE(0) TO laORelFld , lcORltFld,laScrError
laRelFld     = SPACE(0)
laSeqNo      = SPACE(0)
llDisPop     = .F.
lcPopName    = SPACE(0)
lcAryName    = SPACE(0)
lnActComp    = 1
llDefCode    = .F.
lcNoDefault  = REPLICATE("~",6)
lcDefCode    = lcNoDefault
lcDefDesc    = SPACE(0)
lnSession    = gnProgCopy
lcDummyCom   = REPLICATE("~",2)
lnActCode    = 1
llIsEditable = .F.
lcPic        = SPACE(1)
lnCodeWdth   = 0
lcCode1      = SPACE(0)
lcCode2      = SPACE(0)
lcCode3      = SPACE(0)
lcCode4      = SPACE(0)
lc_Tmpfl     = SPACE(0)
lc_TmpRcl    = SPACE(0)
lc_TmRctm    = SPACE(0)
lc_TmHold    = SPACE(0)
lcOldCod     = SPACE(0)
lcCodeRltd   = SPACE(0)
lcCode_no    = SPACE(0)
lcOldRltd    = SPACE(0)
llRlt_Fld    = .F.
laDefProc[9] = .F.
lnProgSeq    = 0 
lnRecFld     = 1
lnOldRecNo   = 0 
lcOnEscape   = ON('KEY','ESC')
lcRelStat    = SPACE(0)
lcOldDesc    = SPACE(0)
lcOldComp    = SPACE(0)
laComMdls    = SPACE(0)
lcListColor  = SCHEME(6,3)
lcListColor  = STRTRAN(SUBSTR(lcListColor,1,ATC('/',lcListColor)-1),'+','')+;
               SUBSTR(lcListColor,ATC('/',lcListColor))
lcXorColor   = SCHEME(6,3)
lcXorColor   = STRTRAN(SUBSTR(lcXorColor,ATC('/',lcXorColor)+1),'*','')+'/'+;
               STRTRAN(SUBSTR(lcXorColor,1,ATC('/',lcXorColor)-1),'+','')

IF !USED("SYDField")
  USE (gcSysHome + "SYDField") IN 0
ENDIF
               
IF !lfCkhFiles() OR !gfSetup()
  RETURN
ENDIF  



llFrstEntr = .T.  
lnObject   = 1
lcBrFields = "cFld_name :H='Field Name',cFld_head :H='Description',lRltfields :H='Related Fields'"

IF !WEXIST(lcBaseWind)
  IF FILE(gcSysHome+'ERROR.MEM')
     RESTORE FROM (gcSysHome+'ERROR.MEM') ADDI
  ENDIF
  =ASORT(laScrError,2)
  lc_Tmpfl  = gfTempName()
  lc_TmpRcl = gfTempName()
  lc_TmRcTm = gfTempName()
  lc_TmHold = gfTempName()
  lcCode1   = gfTempName()
  lcCode2   = gfTempName()
  lcCode3   = gfTempName()
  lcCode4   = gfTempName()

  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  = lfCopyStr(lc_TmpRcl)
  = lfCopyStr(lc_Tmpfl)
  CREATE TABLE (gcWorkDir+lc_TmHold) (CFILE_NAM C(8), CFILE_TTL C(34))
  SELECT (lc_Tmpfl)
  INDEX ON cComp_ID+cCode_No+cRltField+cFld_Name TAG cCODE_NO
  SET ORDER TO TAG CCODE_NO

  =gfSubstr(ALLTRIM(SYCCOMP.mcomp_mdl),@laComMdls,"|")
  DECLARE laComMdls[ALEN(laComMdls,1),1]
    
  SELECT CCOMP_ID + " - " + CCOM_NAME, CCOMP_ID, cCom_DDir ;
  FROM SYCCOMP INTO ARRAY laComp ORDER BY CCOMP_ID
  DIMENSION laComp[ALEN(laComp,1)+1,3]
  = AINS(laComp,1)
  laComp[1] = "Select a Company"
  laComp[2] = lcDummyCom
  laComp[3] = ""

  IF !EMPTY(gcAct_Comp)
    lnComp    = INT(ASCAN(laComp, gcAct_Comp) / 3)+1
    lnActComp = lnComp
    llNoThing = lfvComp(.T.)
  ENDIF
    
  SELECT CFLD_HEAD, CFLD_NAME, lRltfields, PADR("|"+UPPER(ALLTRIM(mRltfields))+"|",250);
  FROM SYDFIELD WHERE LVLDENTRY = .T. INTO ARRAY laCodes ;
  ORDER BY CFLD_HEAD
  
  DIMENSION laCodes[ALEN(laCodes,1)+1,4]
  = AINS(laCodes,1)
  laCodes[1] = "Select a Code"
  laCodes[2] = SPACE(10)
  laCodes[3] = .F.
  laCodes[4] = SPACE(0)
ELSE
  =lfNavStatus(lnActCode)
ENDIF
*E301040,1 AMM end
IF !glFirstime
  lnComp = lnActComp
  lnCode = lnActCode
ENDIF

lcRelStat    = IIF(laScrMode[1],"DISABLE",IIF(llRlt_Fld,"ENABLE","DISABLE"))
laCtrStat[10] = "DISABLE"	&&browse
laCtrStat[8] = "DISABLE"	&&Delete
laCtrStat[9] = "DISABLE"	&&Select

PUSH MENU _MSYSMENU
USE (gcSysHome+"SYCMenu") IN 0 ORDER Pross_ID AGAIN ALIAS MenuFile
lnSavBarNo = 0
lnSavBarNo = lfGtSavBar("GFCPTOP")
IF lnSavBarNo # 0
  ON SELECTION BAR lnSavBarNo OF P03PU03 DO gfCpTop
ENDIF
lnSavBarNo = lfGtSavBar("GFCPPRVIS")
IF lnSavBarNo # 0
  ON SELECTION BAR lnSavBarNo OF P03PU03 DO gfCpPrvis
ENDIF
lnSavBarNo = lfGtSavBar("GFCPNEXT")
IF lnSavBarNo # 0
  ON SELECTION BAR lnSavBarNo OF P03PU03 DO gfCpNext
ENDIF
lnSavBarNo = lfGtSavBar("GFCPBTTM")
IF lnSavBarNo # 0
  ON SELECTION BAR lnSavBarNo OF P03PU03 DO gfCpBttm
ENDIF
lnSavBarNo = lfGtSavBar('GFSETFILTR')

DEFINE BAR (lnSavBarNo) OF P03PU03 PROMPT "Fil\<ter"
SET SKIP OF BAR (lnSavBarNo) OF P03PU03 .T.
USE IN MenuFile


SELECT (lc_Tmpfl)
DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTitl)


=lfGenError(2)
DO (gcScrDir + '\SMDETER.SPR')
=lfGenError(3)

RELEASE WINDOW (lcBrowTitl)
POP MENU _MSYSMENU

SELECT (lc_Tmpfl)
= lfClrTrap()
RELEASE BAR 100 OF P01PU01 

IF glQuitting

  
  ** If exit from this program, erasing all the temp & text files **
  IF USED(lc_Tmpfl)
    USE IN ALIAS(lc_Tmpfl)
  ENDIF
  ERASE (gcWorkDir+lc_Tmpfl+".DBF")
  ERASE (gcWorkDir+lc_Tmpfl+".FPT")
  ERASE (gcWorkDir+lc_Tmpfl+".CDX")

  IF USED(lc_TmpRcl)
    USE IN ALIAS(lc_TmpRcl)
  ENDIF
  ERASE (gcWorkDir+lc_TmpRcl+".DBF")
  ERASE (gcWorkDir+lc_TmpRcl+".FPT")
  ERASE (gcWorkDir+lc_TmpRcl+".CDX")

  IF USED(lc_TmRctm)
    USE IN ALIAS(lc_TmRctm)
  ENDIF
  ERASE (gcWorkDir+lc_TmRctm+".DBF")
  ERASE (gcWorkDir+lc_TmRctm+".FPT")
  ERASE (gcWorkDir+lc_TmRctm+".CDX")
  
  IF USED(lc_TmHold)
    USE IN ALIAS(lc_TmHold)
  ENDIF
  ERASE (gcWorkDir+lc_TmHold+".DBF")
ENDIF   

*:********************************************************************
*!
*!      Procedure: lpShow
*!
*:********************************************************************
*
PROCEDURE lpShow

EXTERNAL ARRAY laScrMode

*** Disable the delete button in the control pannel. ***
SHOW GET pbDlt  DISABLE
SHOW GET pbBrws DISABLE

laCtrStat[8]  = "DISABLE"	&&Delete
laCtrStat[10] = "DISABLE"	&&Browse

=lfGenError(4) 
DO CASE
  CASE laScrMode[1]                       && Select mode.
    llRlt_Fld  = .F.
    lnProgSeq  = 0
    DECLARE laSeqNo[1,2]
    laSeqNo    = " "
    SHOW GET pbReltdFld DISABLE
    
    STORE 1 TO lnComp, lnCode, lnActComp, lnActCode
    IF !EMPTY(gcAct_Comp)
      lnComp    = INT(ASCAN(laComp, gcAct_Comp) / 3)+1
      lnActComp = lnComp
      llNothing = lfvComp()
    ENDIF
    DIMENSION laCodInfo[1,10]
    laCodInfo = SPACE(0)
    
    SELECT (lc_Tmpfl)
    ZAP
    = lfBrowse()
    
  CASE laScrMode[2]                       && View mode 
    *E301040,1 AMM get the status of navigation buttons and refresh.
    =lfNavStatus(lnActCode)

    lnProgSeq  = 0
    DECLARE laSeqNo[1,2]
    laSeqNo    = " "
    lcOldCod   = " "
    SELECT CODES
    LOCATE FOR cFld_Name+cRltField = laCodes[lnActCode,2]+"N" AND !EMPTY(cComp_ID)
    lcStat = IIF(FOUND(), "ENABLE", "DISABLE")
    IF laCodes[lnActCode,3]
      SHOW GET pbReltdFld &lcStat
    ELSE
      SHOW GET pbReltdFld DISABLE
    ENDIF  
    SHOW GET llDefCode  DISABLE

    
  CASE laScrMode[3]                       && Edit mode.
    lnProgSeq  = 0
    DECLARE laSeqNo[1,2]
    laSeqNo    = " "
    lcOldCod   = " "

    *-- fill the temprary file from the main file.
    SELECT Codes
    SET ORDER TO
    SELECT *,RECNO() AS 'NRECNO' ,'S' AS 'CSTATUS'        ;
           FROM CODES                                     ;
           WHERE cComp_Id+cFld_name+ccode_no+cDiscrep =   ;
                 laComp[lnActComp,2]+laCodes[lnActCode,2] ;
           INTO DBF (gcWorkDir+lc_Tmpfl) ;
           ORDER BY cDiscrep
    INDEX ON cComp_ID+cCode_No+cRltField+cFld_Name TAG cCODE_NO
    SET ORDER TO TAG CCODE_NO
    GOTO TOP

    lcStat = IIF(EOF(), "DISABLE", "ENABLE")
    SHOW GET llDefCode  &lcStat
    IF laCodes[lnActCode,3]
      SHOW GET pbReltdFld &lcStat
    ELSE
      SHOW GET pbReltdFld DISABLE
    ENDIF  
    SELECT Codes
    SET ORDER TO TAG Codes

ENDCASE

IF laScrMode[3] OR laScrMode[2]
  *** See if there is related fields for this code. ***
  laRelFld   = " "
  IF laCodes[lnActCode,3]
    lcRltField = "|" + lfGetRFStr(laCodes[lnActCode,2],laComp[lnActComp,2]) + "|" 

    lcORltFld = lcRltField
    lnOccur = OCCUR('~',lcRltField)
    IF lnOccur > 0
      FOR lnC = 1 TO lnOccur
        lnPos      = ATC("~",lcRltField,lnC)
        lnOccur1   = OCCUR('|',SUBSTR(lcRltField,1,lnPos))
        lnPos1     = ATC("|",lcRltField,lnOccur1+1)
        lcRltField = STRTRAN(lcRltField,SUBSTR(lcRltField,lnPos,lnPos1-lnpos),'')
      ENDFOR
    ENDIF

    llRlt_Fld  = .T.
       SELECT DISTINCT sydField.cFld_name,;
             PADR(IIF(!EMPTY(sydField.cFld_head),sydField.cFld_head,;
             sydField.cFld_name+SPACE(15)),25),sydField.cData_typ,;
             sydField.nFld_Wdth,sydField.nFld_dec,"",;
             "",sydField.mVald_str,sydField.cVldfnloc,"",;
             sydField.lVldEntry, PADR(ALLTRIM(sydField.mVEntries),250),.F.,'';
       FROM  sydField ;
       WHERE "|"+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
         .OR. "|"+'$'+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
       INTO  ARRAY laRelFld

       FOR lnArayCont = 1 TO ALEN(laRelFld , 1)
         lnPos = ATC(ALLTRIM(laRelFld[lnArayCont,1]),lcRltField)
         IF SUBSTR(lcRltField,lnPos-1,1) = '$'
           laRelFld[lnArayCont,13] = .T.
           IF SUBSTR(lcORltFld , lnPos+LEN(ALLTRIM(laRelFld[lnArayCont,1])) , 1) = '~'
             lnOccur = OCCUR('|',SUBSTR(lcORltFld,1,lnPos))
             lnPos1 = ATC("|",lcORltFld,lnOccur+1)
             laRelFld[lnArayCont,14] = SUBSTR(lcORltFld , lnPos+LEN(ALLTRIM(laRelFld[lnArayCont,1]))+1 , ;
                lnPos1 - (lnPos+LEN(ALLTRIM(laRelFld[lnArayCont,1])))-1    )
           ENDIF
         ENDIF
       ENDFOR

       DIMENSION laORelFld[1]
       =ACOPY(laRelFld,laORelFld)


  ELSE
    llRlt_Fld = .F.
  ENDIF
  = lfBrowse()
ENDIF


*:********************************************************************
*!
*!      Function: lfBrowse
*!
*:********************************************************************
*
FUNCTION lfBrowse

DO CASE 
  CASE laScrMode[2]
   llMaster   = .T.
   RELEASE WINDOW (lcBrowTitl)
   lcBrowTitl = "View Codes"

   SELECT CODES
   lnOldRecNo = RECNO()
   lcFields = "cMarker=IIF(RECNO()=lnOldRecNo,' >',' '):2:R:H=' ':W=.F.,"+;
               IIF(llIsEditable,"cCode_No :15:H='Code',",'')+;
               "cDiscrep :45:H='Description'"

    BROWSE FOR cComp_ID+cRltField+cFld_Name = laComp[lnActComp,2]+"N"+laCodes[lnActCode,2];
           FIELDS &lcFields;
           WINDOW (lcCode2);
           IN WINDOW (gcBaseWind) ;
           WHEN lfwBrWhen();
           VALID :F lfvBrows() ; 
           LOCK 0;
           NOAPPEND;
           NOCLEAR;
           NODELETE;
           NOWAIT;
           NOEDIT;
           SAVE;
           NOMENU;
           TITLE lcBrowTitl      
  
  OTHERWISE
    llMaster   = .F.

    RELEASE WINDOW (lcBrowTitl)

    lcBrowTitl = IIF(laScrMode[1], "Codes", "View/Edit Codes")

    SELECT (lc_Tmpfl)
    lnOldRecNo = RECNO()
    lcFields   = "cMarker=IIF(RECNO()=lnOldRecNo,' >','  '):2:R:H=' ':W=.F.,"+;
                 IIF(llIsEditable,"cCode_No :P=lcPic:15:H='Code' :V=lfvBCode():;
                 W=lfwBCode(),",'')+"cDiscrep :H='Description' ;
                 :45:V=lfvDiscrep(): W=lfwDiscrep()"


    BROWSE FOR cRltField = "N" ;
           FIELDS &lcFields;
           WINDOW (lcCode2);
           IN WINDOW (gcBaseWind) ;
           WHEN lfwBrWhen();
           VALID :F lfvBrows();
           LOCK 0;
           NOAPPEND;
           NOCLEAR;
           NODELETE;
           NOWAIT;
           SAVE;
           NOMENU;
           TITLE lcBrowTitl
ENDCASE

*:********************************************************************
*!
*!      Function: lfDeactBrw
*!
*:********************************************************************
*
FUNCTION lfDeactBrw
IF WONTOP()  = lcBrowTitl 
  *RELEASE BAR 100 OF P01PU01
  ON KEY LABEL Alt+I      DO lpTrap   WITH '=lfvInsert()', laScrMode[3]
  ON KEY LABEL Alt+V      DO lpTrap   WITH '=lfvRemove()', laScrMode[3]
  ON KEY LABEL ESC        DO lpSelObj WITH 'GWCCONTRL1', OBJNUM(pbCls)
  ON KEY LABEL Alt+A      DO lpTrap   WITH '=lfvRecall()', laScrMode[3]
  ON KEY LABEL Alt+D      DO lpTrap   WITH '=lfvReltdFld()',!laScrMode[1] .AND. llRlt_Fld
  ON KEY LABEL TAB        DO lpTab    WITH lcCode3, OBJNUM(ibBackTab)
  ON KEY LABEL BACKTAB    DO lpTab    WITH lcCode1, OBJNUM(ibTab)
  ON KEY LABEL Ctrl+ENTER DO lpSelObj WITH 'GWCCONTRL1', IIF(laScrMode[1] .OR. laScrMode[2], OBJNUM(pbCls), OBJNUM(pbSav))
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
ELSE
  =lfClrTrap()  
ENDIF
RETURN lfGenError(6)

*!**************************************************************************
*!
*!      Function: lfActBrw
*!
*!**************************************************************************
*
*B600464,1 Activate function of READ CYCLE of SMCODES.SCX.
*B600464,1 Clears browse key traps, and stops the browse
*B600464,1 READ if not already released.
FUNCTION lfActBrw
IF glFromBrow
  =lfClrTrap() .AND. gfStopBrow()
ENDIF
*B600464,1 end.

*!**************************************************************************
*!
*!      PROCEDURE: lpTab
*!
*!**************************************************************************
*
PROCEDURE lpTab
PARAMETERS lcWindName, lnObjNum

ACTIVATE WINDOW (lcWindName)
_CUROBJ = lnObjNum

*!**************************************************************************
*!
*!      PROCEDURE: lpSelObj
*!
*!**************************************************************************
*

PROCEDURE lpSelObj
PARAMETERS lcWindName, lnObjNum

ACTIVATE WINDOW (lcWindName)
_CUROBJ = lnObjNum
KEYBOARD "{ENTER}" PLAIN



*:********************************************************************
*!
*!      PROCEDURE: lpTrap
*!
*:********************************************************************
*
PROCEDURE lpTrap
PARAMETER lcFuncName, llMaySelect
IF llMaySelect
  &lcFuncName
ENDIF  

*!**************************************************************************
*!
*!      FUNCTION : lfClrTrap
*!
*!**************************************************************************
*
FUNCTION lfClrTrap

ON KEY LABEL Alt+I
ON KEY LABEL Alt+V
ON KEY LABEL ESC &lcOnEscape
ON KEY LABEL Alt+A
ON KEY LABEL Alt+D
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL Ctrl+ENTER 
ON KEY LABEL Ctrl+HOME
ON KEY LABEL Ctrl+END


*!**************************************************************************
*!
*!      Function: lfvComp
*!
*!**************************************************************************
*
FUNCTION lfvComp
PARAMETER llDontErr
PRIVATE lnAlias

lnAlias   = SELECT(0)
lnActComp = lnComp
IF lnComp <> 1
  SELECT IIF(USED("Codes"), "Codes", 0)
  USE (ALLTRIM(laComp[lnActComp,3]) + "Codes") ORDER Codes
  = gfSubstr(ALLTRIM(SYCCOMP.mcomp_mdl),@laComMdls,"|")
  DECLARE laComMdls[ALEN(laComMdls,1),1]
ENDIF 
SELECT(lnAlias)
IF !llDontErr
  =lfGenError(5)
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvCode
*!
*!**************************************************************************
*
FUNCTION lfvCode

IF laComp[lnActComp,2] = lcDummyCom
  *** You have to select a company first. ***
  *** <  Ok  > ***
  = gfModalGen("TRM00219B00000","DIALOG")
  STORE 1 TO lnCode, lnActCode
  _CUROBJ   = OBJNUM(laComp)
  SHOW GET lnCode
ELSE
  *-- If the user selects a field
  IF lnCode <> 1
    lnActCode    = lnCode
    laData[1]    = laCodes[lnActCode,2]
    lnCodeWdth   = 0
    llIsEditable = gfIsEdtble(laCodes[lnActCode,2], @lnCodeWdth, laComp[lnActComp,2])
    lcPic        = REPLICATE("!",lnCodeWdth)
    laScrMode    = .F.
    laScrMode[2] = .T.
    

    lcOrdFld = ORDER('SYDFIELD')
    SET ORDER TO TAG cFld_Name IN SYDFIELD
    =SEEK(laCodes[lnActCode,2],'SYDFIELD')
    SET ORDER TO TAG (lcOrdFld) IN SYDFIELD

  
    SET ORDER TO TAG IDRltFName IN Codes
    IF SEEK(SPACE(2)+"N"+laCodes[lnActCode,2],"CODES")
      lcDefCode = Codes.cCode_No
      lcDefDesc = Codes.cDiscrep
    ELSE
      lcDefCode = lcNoDefault
      lcDefDesc = SPACE(0)
    ENDIF
    SET ORDER TO TAG Codes IN Codes
    SELECT SYDField
    SHOW GETS
  ENDIF
ENDIF
=lfGenError(5)

*!**************************************************************************
*!
*!      Function: lfwDiscrep
*!
*!**************************************************************************
*
FUNCTION lfwDiscrep

lcOldDesc = &lc_Tmpfl..cDiscrep

*!**************************************************************************
*!
*!      Function: lfvDiscrep
*!
*!**************************************************************************
*
FUNCTION lfvDiscrep

IF !llIsEditable AND !EMPTY(&lc_Tmpfl..cDiscrep)
  lcDiscrep = &lc_Tmpfl..cDiscrep

  SELECT (lc_Tmpfl)
  lnCodRec = RECNO(lc_Tmpfl)
  LOCATE FOR ALLTRIM(UPPER(cDiscrep)) = ALLTRIM(UPPER(lcDiscrep)) .AND. ;
             RECNO(lc_Tmpfl) <> lnCodRec
  IF FOUND()
    *** This field already exists. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00053B00000","DIALOG")
    IF lnCodRec > 0  .AND. lnCodRec <= RECCOUNT()
      GO lnCodRec
      REPLACE &lc_Tmpfl..cDiscrep WITH SPACE(30)
    ENDIF
  ELSE
    IF lnCodRec > 0 .AND. lnCodRec <= RECCOUNT()
      GO lnCodRec
      lcStatus = IIF(AT(&lc_Tmpfl..cStatus,"RASM") > 0,;
                 SUBSTR("RAMM",AT(&lc_Tmpfl..cStatus,"RASM"),1),"S")
      REPLACE &lc_Tmpfl..cStatus WITH lcStatus
    ENDIF
  ENDIF
  REPLACE &lc_Tmpfl..cDiscrep WITH STRTRAN(cdiscrep, CHR(255), "")
  
  =gfUpdate()

  SHOW WINDOW (lcBrowTitl) REFRESH
ELSE
  IF llIsEditable
    lcStatus = IIF(AT(&lc_Tmpfl..cStatus,"RASM") > 0,;
                 SUBSTR("RAMM",AT(&lc_Tmpfl..cStatus,"RASM"),1),"S")
    REPLACE &lc_Tmpfl..cStatus  WITH lcStatus,;
            &lc_Tmpfl..cDiscrep WITH STRTRAN(cdiscrep, CHR(255), "")
    lcDefDesc = IIF(llDefCode, &lc_Tmpfl..cDiscrep, lcDefDesc)
    =gfUpdate()
    SHOW WINDOW (lcBrowTitl) REFRESH
  ENDIF

  
ENDIF

*!**************************************************************************
*!
*!      Function: lfvInsert
*!
*!**************************************************************************
*
FUNCTION lfvInsert

*** Insert code in the temp file. ***
SELECT (lc_Tmpfl)
IF llIsEditable
  SEEK laComp[lnActComp,2]+SPACE(6)+"N"+laCodes[lnActCode,2]
ELSE
  LOCATE FOR (EMPTY(cdiscrep) OR ALLTRIM(cdiscrep)=CHR(255)) AND cRltField = "N"
ENDIF

IF !FOUND()
  *** Get a temp. seq. no. ***
  lnProgSeq = lnProgSeq + 1
  
  lcCode = IIF(llIsEditable, SPACE(6), "A"+ALLTRIM(STR(lnProgSeq))) 
  
  IF !EMPTY(laSeqNo[1,1])
    DIMENSION laSeqNo[ALEN(laSeqNo,1)+1,2]
    =AINS(laSeqNo,1)
  ENDIF
  laSeqNo[1,1] = lcCode
  laSeqNo[1,2] = " "
  

  lcDisc = ''
  PUSH KEY
  ON KEY
  DO INSCODE
  POP KEY
  

  IF !(IIF(llIsEditable,EMPTY(lcCode),EMPTY(lcDisc)))

    INSERT INTO (gcWorkDir+lc_Tmpfl) ;
           (ccomp_id,cfld_name,ccode_no, ;
            cdiscrep,crltfield, ;
            cadd_user,dadd_date,cadd_time,cStatus) ;
    VALUES (laComp[lnActComp,2],laCodes[lnActCode,2],lcCode, ;
            lcDisc,"N", ;
            gcUser_ID,DATE(),gfGetTime(),"A")

    lnCodRec = RECNO(lc_Tmpfl)
 
    *** If it has related fields add its records. ***
    IF llRlt_Fld 

      DIMENSION laRelFld[1]
      =ACOPY(laORelFld,laRelFld)

      FOR lnCount3 = 1 TO ALEN(laRelFld,1)
        INSERT INTO (gcWorkDir+lc_Tmpfl) ;
               (ccomp_id,cfld_name,ccode_no, ;
                cdiscrep,crltfield, ;
                cRltd_Nam,cRltd_Typ,cRltd_Vlu,;
                cadd_user,dadd_date,cadd_time,cStatus) ;
        VALUES (laComp[lnActComp,2],laCodes[lnActCode,2],lcCode, ;
                "","Y",laRelFld[lnCount3,1],;
                laRelFld[lnCount3,3],"", ;
                gcUser_ID,DATE(),gfGetTime(),"A")
      ENDFOR
    ENDIF
    =gfUpdate()
    IF lnCodRec > 0 .AND. lnCodRec <= RECCOUNT()
      GO lnCodRec
    ENDIF

    IF llRlt_Fld
      =lfvReltdFld()
    ENDIF
  ENDIF

  
ENDIF

SELECT (lc_Tmpfl)
lnCodRec = RECNO()
GOTO TOP
lcStat = IIF(EOF(), "DISABLE", "ENABLE")
SHOW GET llDefCode  &lcStat
IF lnCodRec > 0 .AND. lnCodRec <= RECCOUNT()
  GO lnCodRec
ENDIF

IF laCodes[lnActCode,3]
  SHOW GET pbReltdFld &lcStat
ELSE
  SHOW GET pbReltdFld DISABLE
ENDIF  

KEYBOARD "{ALT+B}"

*!**************************************************************************
*!
*!      Function: lfvRemove
*!
*!**************************************************************************
*
FUNCTION lfvRemove

CLEAR TYPEAHEAD
SELECT (lc_Tmpfl)

IF cStatus = "A"
  IF gfModalGen("QRM00035B00007","ALERT") = 1
    IF (llIsEditable AND !EMPTY(cCode_No)) OR (!llIsEditable AND !EMPTY(cDiscrep)) 
      SCATTER MEMVAR MEMO
      INSERT INTO (lc_TmpRcl) FROM MEMVAR
    ENDIF
    REPLACE cStatus WITH "S"
    lcDefCode = IIF(cCode_No=lcDefCode, lcNoDefault, lcDefCode) 
    lcCode    = CCODE_NO
    DELETE
    REPLACE ALL cStatus WITH "S" FOR CCODE_NO = lcCode
    DELETE  ALL FOR CCODE_NO = lcCode
    GO TOP
    = lfwBrWhen()
    = gfUpdate()
  ENDIF
ELSE
  *** Do you wish to check the data files ***
  *** before removing this code? ***
  *** < Check data > - < Cancel >
  IF gfModalGen("QRM00040B00013","DIALOG") = 1
    = lfChckCode()
  ENDIF
ENDIF

IF laCodes[lnActCode,3]
  SELECT (lc_Tmpfl)
  GOTO TOP
  lcStat = IIF(EOF(), "DISABLE", "ENABLE")
  SHOW GET pbReltdFld &lcStat
  SHOW GET llDefCode  &lcStat
ELSE
  SHOW GET pbReltdFld DISABLE
  SHOW GET llDefCode  DISABLE
ENDIF  

SELECT SYDFIELD
=lfGenError(5)

*!**************************************************************************
*!
*!      Function: lfChckCode
*!
*!**************************************************************************
*
FUNCTION lfChckCode
PRIVATE lcDataDir

lcFile     = ''     && Hold the file name.
llOpenFile = .F.    && To know if the file opened from before or not.
llFound    = .F.    && To know if there is any file have this code or not.

*** Array to store the files names that contain the field ***
*** that has to be remove from the temp. file .....
DECLARE laFileList[1,1]

_TALLY = 0
*** Gather the files that contain the removed field ***
lcTalk = SET("TALK")
SET TALK ON
SET TALK WINDOW
SELECT sydFlFld
SET ORDER TO
SELECT cFile_Nam FROM sydFlFld ;
       INTO ARRAY laFileList;
       WHERE cFld_Name+STR(NFLD_POS) = laCodes[lnActCode,2]
SET TALK &lcTalk

IF _TALLY = 0
  *** There is no file have this code, you can ***
  *** delete it.  Do you want to delete it?    ***
  *** <  Yes  >  -  <  No  > ***
  IF gfModalGen("TRM00201B00006","DIALOG") = 1
    SELECT (lc_Tmpfl)

    SCATTER MEMVAR MEMO
    m.cStatus = IIF(m.cStatus = 'S', 'O', m.cStatus)
    INSERT INTO (lc_TmpRcl) FROM MEMVAR
    lcDefCode = IIF(cCode_No=lcDefCode, lcNoDefault, lcDefCode) 
    lcStatus = IIF(cStatus $ 'RA' , 'S' , 'D')
    lcCode   = CCODE_NO
    REPLACE cStatus WITH lcStatus
    DELETE
    REPLACE ALL cStatus WITH lcStatus FOR CCODE_NO = lcCode
    DELETE  ALL FOR CCODE_NO = lcCode
    GO TOP
    = lfwBrWhen ()
    = gfUpdate ()
  ENDIF
ELSE
  SELECT (lc_TmHold)
  ZAP
  lcDataDir = IIF(SEEK(laComp[lnComp,2],'SYCCOMP'),ALLTRIM(SYCCOMP.cCom_DDir), gcDataDir)
  lnTotRec  = ALEN(laFileList,1)
  lnCurRec  = 0
  lcTitle   = "Checking the existance of " + &lc_Tmpfl..cCode_no + " in... "
  FOR lnCount2 = 1 TO ALEN(laFileList,1)
    lnCurRec   = lnCurRec + 1
    lcFile     = ''
    lcPath     = IIF(UPPER(LEFT(laFileList[lnCount2,1],2)) = 'SY',gcSysHome , lcDataDir )
    lcPathFile = lcPath + ALLTRIM(laFileList[lnCount2,1])
    lcFile     = ALLTRIM(laFileList[lnCount2,1])
    llNothing  = gfThermo(lnTotRec,lnCurRec, lcTitle, "the file named : "+lcFile)
    
    IF FILE(lcPathFile+".DBF")
      IF !USED(lcFile)
        SELECT 0
        USE &lcPath.&lcFile
        llOpenFile = .T.
      ELSE
        SELECT (lcFile)
      ENDIF
      
      LOCATE FOR &laCodes[lnActCode,2] = &lc_Tmpfl..cCode_no
      
      IF FOUND()
        llFound = .T.
        SELECT (lc_TmHold)
        INSERT INTO (gcWorkDir+lc_TmHold) ;
               (cFile_nam,cFile_ttl) ;
        VALUES (laFileList[lnCount2,1],;
               LOOKUP(SYDFILES.cFile_ttl,;
               laFileList[lnCount2,1],SYDFILES.cFILE_nam,'CFILE_NAM'))
      ENDIF
      
      IF llOpenFile
        llOpenFile = .F.
        USE IN ALIAS(lcFile)
      ENDIF
    ENDIF
  ENDFOR
  
  IF llFound
    *** Special trap for this browse that hold ***
    *** all the files that has this code...
    ON KEY LABEL TAB        DO lfTrapFlCd
    ON KEY LABEL BACKTAB    DO lfTrapFlCd
    

    *lcFileTitl = "Files have code "+"'"+ALLTRIM(SYDFIELD.cFld_head)+"'"
    lcFileTitl = "Files using " + ALLTRIM(&lc_TmpFl..cDiscrep)
    

    RELEASE BAR 100 OF P01PU01

    
    *** Display all the files have this code in its data. ***

    *** Back to the main trap of the main screen. ***
    ON KEY LABEL TAB        
    ON KEY LABEL BACKTAB    
    ON KEY LABEL ESC &lcOnEscape
    DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
    ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTitl)
  ELSE
    *** There is no file have this code, you can ***
    *** delete it.  Do you want to delete it?    ***
    *** <  Yes  >  -  <  No  > ***
    IF gfModalGen("TRM00201B00006","DIALOG") = 1
      SELECT (lc_Tmpfl)
      SCATTER MEMVAR MEMO
      m.cStatus = IIF(m.cStatus = 'S', 'O', m.cStatus)
      INSERT INTO (lc_TmpRcl) FROM MEMVAR
      lcDefCode = IIF(cCode_No=lcDefCode, lcNoDefault, lcDefCode) 
      lcStatus  = IIF(cStatus $ 'RA' , 'S' , 'D')
      lcCode    = CCODE_NO
      REPLACE cStatus WITH lcStatus
      DELETE
      REPLACE ALL cStatus WITH lcStatus FOR CCODE_NO = lcCode
      DELETE  ALL FOR CCODE_NO = lcCode
      GO TOP
      =lfwBrWhen()
      =gfUpdate()
    ENDIF
  ENDIF
ENDIF

SELECT SYDFIELD

*!**************************************************************************
*!
*!      Function: lfFileBrow
*!
*!**************************************************************************
*
FUNCTION lfFileBrow

SELECT (lc_TmHold)
GO TOP
BROWSE FIELDS CFILE_NAM  :H="File Name",CFILE_TTL :H="File Title" ;
       WINDOW AWDSMFLCD1;
       IN WINDOW AWDSMFLCD ;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOEDIT;
       NOWAIT;
       SAVE;
       TITLE lcFileTitl

*!**************************************************************************
*!
*!      Function: lfTrapFlCd
*!
*!**************************************************************************
*
FUNCTION lfTrapFlCd

IF WONTOP("AWDSMFLCD2")
  ACTIVATE WINDOW (lcFileTitl)
ELSE
  ACTIVATE WINDOW AWDSMFLCD2
ENDIF

*!**************************************************************************
*!
*!      Function: lfvRecall
*!
*!**************************************************************************
*
FUNCTION lfvRecall
=lfGenError(5)

*!**************************************************************************
*!
*!      Function: lfvRecl
*!
*!**************************************************************************
*
FUNCTION lfvRecl



*!**************************************************************************
*!
*!      Function: lfvReltdFld
*!
*!**************************************************************************
*
FUNCTION lfvReltdFld
=lfGenError(5)

*!**************************************************************************
*!
*!      Procedure: lfObj_Typ
*!
*!**************************************************************************
*
FUNCTION lfObj_Typ
PARAMETERS lnAryNo


DO CASE
  CASE laRelFld[lnAryNo,11]
    lnLen = ALEN(laCodInfo,1)
    IF !(lnLen = 1 AND EMPTY(laCodInfo[lnLen,1]))
      lnLen = lnLen + 1
      DIMENSION laCodInfo [lnLen,10]
    ENDIF
    laCodInfo[lnLen,01] = laRelFld[lnAryNo,1]
    laCodInfo[lnLen,02] = gfTempName()
    laCodInfo[lnLen,03] = gfTempName()
    laCodInfo[lnLen,04] = ""
    laCodInfo[lnLen,05] = .F.
    laCodInfo[lnLen,06] = .F.
    laCodInfo[lnLen,07] = IIF(laScrMode[2],'Codes',lc_Tmpfl)
    laCodInfo[lnLen,08] = IIF(laScrMode[2],'Codes',"cCODE_NO")
    laCodInfo[lnLen,09] = "laComp[lnActComp,2]+IIF(laScrMode[2],Codes.cCode_No,&lc_Tmpfl..cCode_No)+'Y'+laCodes[lnActCode,2]"
    laCodInfo[lnLen,10] = "cRltd_Vlu"

    lcPopName = laCodInfo[lnLen,03]
    lcAryName = laCodInfo[lnLen,02]
    PUBLIC &lcAryName
    DIMENSION &lcAryName[1]
    &lcAryName = SPACE(0)
    llDisPop   = .T.
    = lfSetPop()

  CASE !EMPTY(laRelFld[lnAryNo,12]) OR ;
       laRelFld[lnAryNo,3] = 'L'
    lnLen = ALEN(laCodInfo,1)
    IF !(lnLen = 1 AND EMPTY(laCodInfo[lnLen,1]))
      lnLen = lnLen + 1
      DIMENSION laCodInfo [lnLen,10]
    ENDIF
    laCodInfo[lnLen,01] = laRelFld[lnAryNo,1]
    laCodInfo[lnLen,02] = gfTempName()
    laCodInfo[lnLen,03] = gfTempName()
    laCodInfo[lnLen,04] = "L"
    STORE ""  TO laCodInfo[lnLen,07], laCodInfo[lnLen,08]
    STORE ""  TO laCodInfo[lnLen,09], laCodInfo[lnLen,10]
    STORE .F. TO laCodInfo[lnLen,05], laCodInfo[lnLen,06]
    lcPopName = laCodInfo[lnLen,03]
    lcAryName = laCodInfo[lnLen,02]
    PUBLIC &lcAryName
    DIMENSION &lcAryName[1]
    &lcAryName = SPACE(0)
    lcStr      = IIF(laRelFld[lnAryNo,3] = 'L', "Yes|No~Y|N", ALLTRIM(laRelFld[lnAryNo,12]))
    = gfSubstr(lcStr,@&lcAryName,"|~")
    lnPos    = ASCAN(&lcAryName,laRelFld[lnAryNo,6])
    IF lnPos <> 0
      PUBLIC &lcPopName
      &lcPopName = ASUBSCRIPT(&lcAryName,lnPos,1)
    ENDIF
    llDisPop = .T.
  
  CASE laRelFld[lnAryNo,3] = 'C'
    laRelFld[lnAryNO,6] = IIF(EMPTY(laRelFld[lnAryNO,6]),;
                          SPACE(laRelFld[lnAryNo,4]),laRelFld[lnAryNO,6])
    IF UPPER(ALLTRIM(laRelFld[lnAryNO,1])) = "CADJACCT"
      lcGetPic = lfSetGLMsk(laComp[lnComp,2])
      laRelFld[lnAryNo,7] = lcGetPic
    ELSE
      laRelFld[lnAryNo,7] = REPLICATE("X",laRelFld[lnAryNo,4])
    ENDIF
    lcDefault           = " "
    llDisPop            = .F.

  CASE laRelFld[lnAryNo,3] = 'N'
    laRelFld[lnAryNO,6] = IIF(EMPTY(laRelFld[lnAryNO,6]),;
                          0,VAL(laRelFld[lnAryNO,6]))
    lcGet_Pic = IIF(EMPTY(laRelFld[lnAryNo,5]),;
                    REPLICATE('9',laRelFld[lnAryNo,4]),;
                    REPLICATE('9',laRelFld[lnAryNo,4]-laRelFld[lnAryNo,5]);
                    +'.'+ REPLICATE('9',laRelFld[lnAryNo,5]))
    laRelFld[lnAryNo,7] = lcGet_Pic
    lcDefault           = "0"
    llDisPop            = .F.

  CASE laRelFld[lnAryNo,3] = 'D'
    laRelFld[lnAryNo,6] = IIF(EMPTY(laRelFld[lnAryNo,6]),{},CTOD(laRelFld[lnAryNo,6]))
    laRelFld[lnAryNo,7] = "@D"
    lcDefault           = "{}"
    llDisPop            = .F.
ENDCASE

IF !EMPTY(laRelFld[lnAryNo,8]) .AND. !EMPTY(laRelFld[lnAryNo,9])
  laRelFld[lnAryNo,8]  = IIF(AT("(",laRelFld[lnAryNo,8]) > 0,;
                         SUBSTR(laRelFld[lnAryNo,8],1,;
                         AT("(",laRelFld[lnAryNo,8])-1),;
                         laRelFld[lnAryNo,8])
  laRelFld[lnAryNo,10] = "VALID lfvVldFld()"
ELSE
  laRelFld[lnAryNo,10] = ""
ENDIF

*!**************************************************************************
*!
*!      FUNCTION: lfwWhnFld
*!
*!**************************************************************************
*
FUNCTION lfwWhnFld

lcOldRltd = EVALUATE(VARREAD())

lcVar   = SYS(18)
lnVarNo = ASCAN(laCodInfo, lcVar) 
IF lnVarNo <> 0
  = gfwCodePop (@laCodInfo, laCodInfo[lnVarNo-2],  "L", laComp[lnActComp,2])
ENDIF  

*!**************************************************************************
*!
*!      FUNCTION: lfvValid
*!
*!**************************************************************************
*
FUNCTION lfvValid

lcCurObj = VARREAD()
IF !EMPTY(&lcCurObj) .AND. LASTKEY() = 13
  SHOW  GET   &lcCurObj
ELSE
  &lcCurObj = lcOldRltd
  SHOW  GET   &lcCurObj
ENDIF

*!**************************************************************************
*!
*!      FUNCTION: lfvVldFld
*!
*!**************************************************************************
*
FUNCTION lfvVldFld

llRetVld = .T.

lcCurObj = VARREAD()

*B602089,1 AMM Fixing the bug of not calling validation of POPUPs
*lnAryNo  = VAL(SUBSTR(lcCurObj,AT("(",lcCurObj)+1,AT(",",lcCurObj)-1))
lnAryNo = 0
IF "LARELFLD" $ lcCurObj
  *B602089,1 AMM If text field, the field will be a member of LARELFLD
  lnAryNo  = VAL(SUBSTR(lcCurObj,AT("(",lcCurObj)+1,AT(",",lcCurObj)-1))
ELSE
  *B602089,1 AMM If POPUP get its position from the LACODINFO array.
  IF ASCAN(laCodInfo,lcCurObj,1) # 0
    lnPos = ASUBSCRIPT(laCodInfo,ASCAN(laCodInfo,lcCurObj,1),1)
    IF ASCAN(laRelFld,laCodInfo[lnPos,1],1) # 0
      lnAryNo = ASUBSCRIPT(laRelFld,ASCAN(laRelFld,laCodInfo[lnPos,1],1) ,1)
    ENDIF
  ENDIF
ENDIF
*B602089,1 AMM end

IF lnAryNo > 0 .AND. lnAryNo <= ALEN(laRelFld,1)
  lcVldFunc = ALLTRIM(laRelFld[lnAryNo,8])
  lcFuncLoc = gcAppHome+ALLTRIM(laRelFld[lnAryNo,9])+".APP"
  lcCurVal  = "laRelFld[lnAryNo,6]"
  DO &lcVldFunc IN &lcFuncLoc WITH lcCurVal , "llRetVld" , laComp[lnComp,2] , lcOldRltd
ELSE
  llRetVld = .F.
ENDIF

IF !llRetVld
  &lcCurObj = lcOldRltd
  SHOW  GET   &lcCurObj
  _CUROBJ   = _CUROBJ
ENDIF


*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*
PROCEDURE lpSavScr
=lfGenError(9)

*!**************************************************************************
*!
*!      Function: lfChekMDLS
*!
*!**************************************************************************
*
FUNCTION lfChekMDLS
PARAMETERS lcvldfnloc
llRetMFlag = .F.

IF !EMPTY(laComMdls[1,1])
  FOR lnMdlCnt = 1 TO ALEN(laComMdls,1)
    IF laComMdls[lnMdlCnt,1] $ lcvldfnloc
      llRetMFlag = .T.
      EXIT
    ENDIF 
  ENDFOR
ENDIF

RETURN llRetMFlag

*!**************************************************************************
*!
*!      Function: lfActFlBrw
*!
*!**************************************************************************
*
FUNCTION lfActFlBrw
DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcFileTitl)
ON KEY LABEL Ctrl+ENTER 
ON KEY LABEL ESC 
ON KEY LABEL ALT+O      
ON KEY LABEL Ctrl+HOME  
ON KEY LABEL Ctrl+END   


*!**************************************************************************
*!
*!      Function: lfDctFlBrw
*!
*!**************************************************************************
*
FUNCTION lfDctFlBrw
IF WONTOP() = lcFileTitl

  ON KEY LABEL Ctrl+ENTER DO lpSelObj WITH 'AWDSMFLCD2', OBJNUM(pbOK)
  ON KEY LABEL ESC        DO lpSelObj WITH 'AWDSMFLCD2', OBJNUM(pbOK)
  ON KEY LABEL ALT+O      DO lpSelObj WITH 'AWDSMFLCD2', OBJNUM(pbOK)
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  RETURN .F.
ENDIF



*!**************************************************************************
*!
*!      Function: lfwBrWhen
*!
*!**************************************************************************
*
FUNCTION lfwBrWhen

lnOldRecNo = RECNO()
IF laScrMode[2]
  llDefCode = (CODES.cCode_No = lcDefCode)
  lcDefDesc = IIF(llDefCode, CODES.cDiscrep, lcDefDesc)
ELSE
  llDefCode = (&lc_tmpfl..cCode_No = lcDefCode)
  lcDefDesc = IIF(llDefCode, &lc_tmpfl..cDiscrep, lcDefDesc)
ENDIF  
glFromBrow = .T.
SHOW GET llDefCode
SHOW WINDOW (lcBrowTitl) REFRESH

*!**************************************************************************
*!
*!      Function: lfCopyStr
*!
*!**************************************************************************
*
FUNCTION lfCopyStr
PARAMETERS lcFileName
PRIVATE lcFileName, lnAlias, llFlFld, llField, lcOrder, lcOrder1

lnAlias = SELECT()

IF USED("SYDField")
  SELECT SYDField
  lcOrder1 = TAG()
  SET ORDER TO 
  llField = .F.
ELSE
  USE (gcSysHome + "SYDField") IN 0
  lcOrder1  = SPACE(0)
  llField = .T.
ENDIF

IF USED("SYDFlFld")
  SELECT SYDFlFld
  lcOrder = TAG()
  SET ORDER TO 
  llFlFld = .F.
ELSE
  USE (gcSysHome + "SYDFlFld") IN 0
  lcOrder  = SPACE(0)
  llFlFld = .T.
ENDIF

SELECT SYDFlFld.cFld_Name, SYDField.cData_Typ, SYDField.nFld_Wdth, SYDField.nFld_dec, SYDFlFld.nFld_Pos ;
  FROM SYDFlFld, SYDField;
 WHERE SYDFlFld.cFile_Nam = "CODES   " AND ;
       SYDField.cFld_Name = SYDFlFld.cFld_Name ;
ORDER BY SYDFlFld.nFld_Pos ;
 INTO ARRAY laStruct 

= gfADel(@laStruct,5,2)

lnLen = ALEN(laStruct,1)
DIMENSION laStruct[lnLen+2,4]
laStruct[lnLen+1,1] = "NRECNO"
laStruct[lnLen+1,2] = "N"
laStruct[lnLen+1,3] = 10
laStruct[lnLen+1,4] = 0
laStruct[lnLen+2,1] = "CSTATUS"
laStruct[lnLen+2,2] = "C"
laStruct[lnLen+2,3] = 1
laStruct[lnLen+2,4] = 0

CREATE TABLE (gcWorkDir+lcFileName) FROM ARRAY laStruct

IF llFlFld
  USE IN SYDFlFld
ELSE
  IF !EMPTY(lcOrder)
    SET ORDER TO TAG &lcOrder IN SYDFlFld
  ENDIF  
ENDIF

IF llFlFld
  USE IN SYDFlFld
ELSE
  IF !EMPTY(lcOrder)
    SET ORDER TO TAG &lcOrder IN SYDFlFld
  ENDIF  
ENDIF

SELECT (lnAlias)

*!*****************************************************************
*!
*!      Function: lfvBrows
*!
*!*****************************************************************
*
FUNCTION lfvBrows
IF WONTOP(lcBrowTitl)
  glFromBrow = .T.
ELSE
  = gfStopBrow()
  glFromBrow = .F.  
  IF !WVISIBLE(gcBaseWind)
    glQuitting = .T.
    CLEAR READ
    RETURN TO SMCODES.SPR
  ENDIF  
ENDIF  

*!*****************************************************************
*!
*!      Function: lfvBCode
*!
*!*****************************************************************
*
FUNCTION lfvBCode


*!*****************************************************************
*!
*!      Function: lfwBCode
*!
*!*****************************************************************
*
FUNCTION lfwBCode

*!*****************************************************************
*!
*!      Function: lfvDefa
*!
*!*****************************************************************
*
FUNCTION lfvDefa
llNoThing = gfUpdate()
=lfGenError(5)

*!*****************************************************************
*!
*!      Function: lfSetPop
*!
*!*****************************************************************
*
FUNCTION lfSetPop
PRIVATE lnAlias

IF llDisPop 
  lcOldgcCom = gcAct_Comp
  lcParm = IIF(EMPTY(laRelFld[lnAryNo,6]), "D", "T")
  = gfwCodePop ( @laCodInfo, laRelFld[lnAryNo ,1],  lcParm, laComp[lnActComp,2])
ENDIF

*!*****************************************************************
*!
*!      Function: lfGetRFStr
*!
*!*****************************************************************
*
FUNCTION lfGetRFStr
PARAMETERS lcCodField, lcActComp
PRIVATE lcRet, llOpen, lcTag, lnPos, lnAlias, llApInst

IF TYPE("lcCodField") # "C" OR TYPE("lcActComp") # "C"
  lcRet = SPACE(0)
ELSE
  lcRet      = SPACE(0)
  lcCodField = UPPER(ALLTRIM(lcCodField))
  lcActComp  = ALLTRIM(lcActComp)
  llGlLink   = (gfGetMemVar("M_LINK_GL", lcActComp) = "Y")
  lcCountry  = SPACE(0)
  lnAlias    = SELECT()
  llOpen     = .T.
  IF !USED("SYCCOMP")
    USE (gcSysHome + "SYCCOMP") IN 0 ORDER cComp_Id
  ELSE
    SELECT SYCCOMP
    lcTag  = TAG()
    lnPos  = RECNO()
    llOpen = .F.
    SET ORDER TO TAG cComp_Id
  ENDIF
  IF SEEK(lcActComp, "SYCCOMP")
    lcCountry = UPPER(ALLTRIM(SYCCOMP.cCont_Code))
    llApInst  = 'AP' $ UPPER(SYCCOMP.mModlSet)
  ENDIF
  IF llOpen
    USE IN SYCCOMP
  ELSE
    SELECT SYCCOMP
    SET ORDER TO TAG &lcTag
    IF lnPos > 0  AND lnPos <= RECCOUNT()
      GO lnPos
    ENDIF
  ENDIF

  IF !EMPTY(lcCountry)
    DO CASE
      CASE lcCodField = "STATE"
        lcRet = "NTAXRATE" + IIF(lcCountry = "CANADA", "|CTAXRULE", "")

      CASE lcCodField = "SHIPVIA"
        lcRet = "CUPS" + IIF(lcCountry = "ENG", "", "|NCODCHARGE|NINSCHARGE|NFXDPRCNT")
      
      
      OTHERWISE
        llOpen     = .T.
        IF !USED("SYDFIELD")
          USE (gcSysHome + "SYDFIELD") IN 0 ORDER cFld_Name
        ELSE
          SELECT SYDFIELD
          lcTag  = TAG()
          lnPos  = RECNO()
          llOpen = .F.
          SET ORDER TO TAG cFld_Name
        ENDIF
        IF SEEK(lcCodField, "SYDFIELD")
          lcRet = UPPER(ALLTRIM(SYDFIELD.mRltFields))
          IF !EMPTY(lcRet) 
            DO CASE
            

              CASE lcCodField = "CDIVISION" 
               
                IF !(llGlLink .AND. gfGetMemVar("M_DIV_LINK", lcActComp)='Y')
                  lnPos = ATC('LINK_CODE',lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1)
                    lcRet = STUFF(lcRet,lnPos-lnMand,LEN('LINK_CODE')+lnMand,'')
                  ENDIF
                  lnPos = ATC('CSLSGLLINK',lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1)
                    lcRet = STUFF(lcRet,lnPos-lnMand,LEN('CSLSGLLINK')+lnMand,'')
                  ENDIF
                ENDIF
              
			  CASE INLIST(lcCodField, "TRANCODE", "CCREDITCOD")

                IF !llApInst
                  lnPos = ATC('CBNKCODE',lcRet)
                  IF lnPos > 0
                    lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1)
                    lcRet = STUFF(lcRet,lnPos-lnMand,LEN('CBNKCODE')+lnMand,'')
                  ENDIF
                  lnPos = ATC('CCHKACCT',lcRet)
                  IF lnPos > 0 
                    lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1)
                    lcRet = STUFF(lcRet,lnPos-lnMand,LEN('CCHKACCT')+lnMand,'')
                  ENDIF
                ENDIF
	 		ENDCASE
          ENDIF && !EMPTY(lcRet) 

          
        ENDIF
        IF llOpen
          USE IN SYDFIELD
        ELSE
          SELECT SYDFIELD
          SET ORDER TO TAG &lcTag
          IF lnPos > 0  AND lnPos <= RECCOUNT()
            GO lnPos
          ENDIF
        ENDIF
    ENDCASE
  ENDIF  
ENDIF

RETURN (lcRet)

*!*****************************************************************
*!
*!      Function: lfCkhFiles
*!
*!*****************************************************************
*
FUNCTION lfCkhFiles

IF !WEXIST(lcBaseWind)
  SELECT SYCCOMP
  GO TOP
  IF EOF()
    *** There is no comanies available.  You have ***
    *** to enter companies first before you enter ***
    *** their codes...
    *** <  Ok  > ***
    =gfModalGen("TRM00223B00000","DIALOG")
    glQuitting = .T.
    RETURN .F.
  ENDIF

  SELECT sydField
  SET ORDER TO TAG VLDENTRY
  IF !SEEK(.T.)
    *** There is no fields have valid entries. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00222B00000","DIALOG")
    glQuitting = .T.
    SET ORDER TO TAG CFLD_NAME
    RETURN .F.
  ENDIF
  SET ORDER TO
ENDIF

RETURN .T.

*!*****************************************************************
*!
*!      Function: gfCodExst
*!
*!*****************************************************************
*
FUNCTION gfCodExst
PARAMETERS laCodeAray, lcActComp
PRIVATE llRet, lnAlias, lcOldTag, lnRecNo, lnCodes, lnCode

llRet = .T.
IF TYPE("ALEN(laCodeAray)") # "U"
  lnAlias = SELECT()
  SELECT Codes
  lcOldTag  = TAG()
  lnRecNo   = RECNO()
  SET ORDER TO TAG cCode_No
  lcActComp = IIF(TYPE("lcActComp")$"UL",gcAct_Com,lnActComp)
  lnCodes   = ALEN(laCodeAray,1)
  lnCode    = 1
  DO WHILE lnCode <= lnCodes AND llRet
    llRet  = SEEK(lcActComp+laCodeAray[lnCode,1])
    lnCode = lnCode + 1
  ENDDO
  IF lnRecNo > 0 AND lnRecNo <= RECCOUNT()
    GOTO lnRecNo
  ENDIF  
  SET ORDER TO TAG &lcOldTag
  SELECT(lnAlias)
ENDIF

RETURN (llRet)

*!*************************************************************
*! Name      : lfvSCode
*! Developer : Ahmed Mohammed
*! Date      : 05/06/1998
*! Purpose   : Valid function of the code in the insert code screen
*! REF       : *E300867,1
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvSCode()
*!*************************************************************
FUNCTION lfvSCode
IF !EMPTY(lcCode) .AND. SEEK(laComp[lnActComp,2]+PADR(lcCode,6)+"N"+laCodes[lnActCode,2], lc_Tmpfl)
  lcCode = SPACE(6)
  RETURN .F.
ENDIF

*!*************************************************************
*! Name      : lfvCancel
*! Developer : Ahmed Mohammed
*! Date      : 05/06/1998
*! Purpose   : Valid function of the cancel button in the related fields screen.
*! REF       : *E300867,1
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvCancel()
*!*************************************************************
FUNCTION lfvCancel

lnAlias=SELECT(0)
SELECT (lc_TmpFl)
FOR lnCount = 1 TO ALEN(laRelFld,1)
  IF laRelFld[lnCount,13] .AND. SEEK(laComp[lnActComp,2]+lcCodeRltd+"Y"+laCodes[lnActCode,2])
    LOCATE REST FOR cRltd_nam = laRelFld[lnCount,1]
    IF !(FOUND() .AND. !EMPTY(cRltd_vlu))
      IF gfModalGen("QRM00301B00007","DIALOG",ALLTRIM(laRelFld[lnCount,2]))=2
       RETURN
      ELSE
        =SEEK(laComp[lnActComp,2]+lcCodeRltd+"N"+laCodes[lnActCode,2])
        lcCode    = CCODE_NO
        DELETE
        DELETE  ALL FOR CCODE_NO = lcCode
        GO TOP
      ENDIF
    ENDIF
  ENDIF
ENDFOR
SELECT(lnAlias)
CLEAR READ

*!*************************************************************
*! Name      : lfvsDis
*! Developer : Ahmed Mohammed
*! Date      : 05/06/1998
*! Purpose   : Valid function of the description field in the insert code 
*!             screen
*! REF       : *E300867,1
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfvsDis()
*!*************************************************************
FUNCTION lfvsDis

lnAlias = SELECT(0)
IF !llIsEditable AND !EMPTY(lcDisc)
  SELECT (lc_Tmpfl)
  lnCodRec = RECNO(lc_Tmpfl)
  LOCATE FOR ALLTRIM(UPPER(cDiscrep)) = ALLTRIM(UPPER(lcDisc)) 
  IF FOUND()
    *** This field already exists. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00053B00000","DIALOG")
    _CUROBJ = _CUROBJ
  ENDIF
ENDIF

SELECT (lnAlias)


*!*************************************************************
*! Name      : gfCpTop
*! Developer : Ahmed Mohammed Ibrahim
*! Date      : 10/20/1998
*! Purpose   : Valid function of the GO TOP button
*! REF       : *E301040,1 AMM
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfNavg()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =gfCpTop()
*! Note      : This function has the same name of the valid global 
*!             function of that button, we did so to over write the 
*!             global function with our local one
*!*************************************************************
FUNCTION gfCpTop

lnCode = 2
=lfNavg(lnCode)

*!*************************************************************
*! Name      : gfCpPrvis
*! Developer : Ahmed Mohammed Ibrahim
*! Date      : 10/20/1998
*! Purpose   : Valid function of the GO PREVIOUS button
*! REF       : *E301040,1 AMM
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfNavg()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =gfCpPrvis()
*! Note      : This function has the same name of the valid global 
*!             function of that button, we did so to over write the 
*!             global function with our local one
*!*************************************************************
FUNCTION gfCpPrvis
IF lnCode > 2
  lnCode = lnCode - 1
  =lfNavg(lnCode)
ENDIF

*!*************************************************************
*! Name      : gfCpNext
*! Developer : Ahmed Mohammed Ibrahim
*! Date      : 10/20/1998
*! Purpose   : Valid function of the GO NEXT button
*! REF       : *E301040,1 AMM
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfNavg()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =gfCpNext()
*! Note      : This function has the same name of the valid global 
*!             function of that button, we did so to over write the 
*!             global function with our local one
*!*************************************************************
FUNCTION gfCpNext
IF lnCode < ALEN(laCodes,1)
  lnCode = lnCode + 1
  =lfNavg(lnCode)
ENDIF

*!*************************************************************
*! Name      : gfCpBttm
*! Developer : Ahmed Mohammed Ibrahim
*! Date      : 10/20/1998
*! Purpose   : Valid function of the GO BUTTOM button
*! REF       : *E301040,1 AMM
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfNavg()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =gfCpBttm()
*! Note      : This function has the same name of the valid global 
*!             function of that button, we did so to over write the 
*!             global function with our local one
*!*************************************************************
FUNCTION gfCpBttm
lnCode = ALEN(laCodes,1)
=lfNavg(lnCode)

*!*************************************************************
*! Name      : lfNavg
*! Developer : Ahmed Mohammed Ibrahim
*! Date      : 10/20/1998
*! Purpose   : Collect data of the passed parameter code
*! REF       : *E301040,1 AMM
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfNavg()
*!*************************************************************
*! Parameters: lnCurCode
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfNavg()
*!*************************************************************
FUNCTION lfNavg
PARAMETERS lnCurCode

lnActCode    = lnCurCode
laData[1]    = laCodes[lnActCode,2]
lnCodeWdth   = 0
llIsEditable = gfIsEdtble(laCodes[lnActCode,2], @lnCodeWdth, laComp[lnActComp,2])
lcPic        = REPLICATE("!",lnCodeWdth)

*E301040,1 AMM Go to the code record in sydfield to lock it if edit mode
lcOrdFld = ORDER('SYDFIELD')
SET ORDER TO TAG cFld_Name IN SYDFIELD
=SEEK(laCodes[lnActCode,2],'SYDFIELD')
SET ORDER TO TAG (lcOrdFld) IN SYDFIELD

SET ORDER TO TAG IDRltFName IN Codes
IF SEEK(SPACE(2)+"N"+laCodes[lnActCode,2],"CODES")
  lcDefCode = Codes.cCode_No
  lcDefDesc = Codes.cDiscrep
ELSE
  lcDefCode = lcNoDefault
  lcDefDesc = SPACE(0)
ENDIF
SET ORDER TO TAG Codes IN Codes
SELECT SYDField
DO lpShow


*!*************************************************************
*! Name      : lfNavStatus
*! Developer : Ahmed Mohammed Ibrahim
*! Date      : 10/20/1998
*! Purpose   : Get the status of the navigation buttons and refresh
*! REF       : *E301040,1 AMM
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfNavg()
*!*************************************************************
*! Parameters: lnCode
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfNavStatus()
*!*************************************************************

FUNCTION lfNavStatus
PARAMETERS lnCode

IF ALEN(laCodes,1) <= 2
  *-- Disable all navigation objects
  SHOW GET pbTop  DISABLE
  SHOW GET pbPrvs DISABLE
  SHOW GET pbNxt  DISABLE
  SHOW GET pbBtm  DISABLE
  laCtrStat[1] = "DISABLE"
  laCtrStat[2] = "DISABLE"
  laCtrStat[3] = "DISABLE"
  laCtrStat[4] = "DISABLE"
ELSE
  DO CASE
    *-- Case top
    CASE lnCode <= 2 
      SHOW GET pbTop  DISABLE
      SHOW GET pbPrvs DISABLE
      SHOW GET pbNxt  ENABLE
      SHOW GET pbBtm  ENABLE
      laCtrStat[1] = "DISABLE"  && TOP
      laCtrStat[2] = "ENABLE"   && Buttom
      laCtrStat[3] = "ENABLE"   && Next
      laCtrStat[4] = "DISABLE"  && Prev
      
    *-- Case top
    CASE lnCode = ALEN(laCodes,1)
      SHOW GET pbTop  ENABLE
      SHOW GET pbPrvs ENABLE
      SHOW GET pbNxt  DISABLE
      SHOW GET pbBtm  DISABLE
      laCtrStat[1] = "ENABLE"
      laCtrStat[2] = "DISABLE"
      laCtrStat[3] = "DISABLE"
      laCtrStat[4] = "ENABLE"
    
    OTHERWISE
      SHOW GET pbTop  ENABLE
      SHOW GET pbPrvs ENABLE
      SHOW GET pbNxt  ENABLE
      SHOW GET pbBtm  ENABLE
      laCtrStat[1] = "ENABLE"
      laCtrStat[2] = "ENABLE"
      laCtrStat[3] = "ENABLE"
      laCtrStat[4] = "ENABLE"

  ENDCASE
ENDIF
SHOW GET lnCode
IF WEXIST(lcBrowTitl)
  SHOW WINDOW (lcBrowTitl) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfGtSavBar
*! Developer : Ahmed Mohammed Ibrahim
*! Date      : 10/20/1998
*! Purpose   : Get the Location in menu bar of the passed valid function
*! REF       : *E301040,1 AMM
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : lfNavg()
*!*************************************************************
*! Parameters: lnCode
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfGtSavBar()
*!*************************************************************
FUNCTION lfGtSavBar
PARAMETERS lcFunc

lnRetVal = IIF(SEEK(lcFunc, "MenuFile"), VAL(MenuFile.cBar_Pos), 0)
RETURN (lnRetVal)





FUNCTION lfGenError
PARAMETERS lnErrType
lnErrPos = ASCAN(laScrError,lnErrType)
IF lnErrPos>0
  lnErrPos = ASUBSCRIPT(laScrError,lnErrPos,1)
  DO WHILE lnErrPos <= ALEN(laScrError,1) AND laScrError[lnErrPos,2]=lnErrType AND !EMPTY(ON('ERROR'))
    lcErrorHnd = ON('ERROR')  
    lcErrNo = ALLT(SUBSTR(laScrError[lnErrPos,1],1,ATC(' ',laScrError[lnErrPos,1])-1))
    lcErrMsg = ALLT(SUBSTR(laScrError[lnErrPos,1],ATC(' ',laScrError[lnErrPos,1])+1))
    lcErrorHnd = STRTRAN(lcErrorHnd,'ERROR()',lcErrNo)
    lcErrorHnd = STRTRAN(lcErrorHnd,'MESSAGE()','"'+lcErrMsg+'"')  
    lcErrorHnd = STRTRAN(lcErrorHnd,'MESSAGE(1)','"'+lcErrMsg+'"')    
    &lcErrorHnd
    lnErrPos = lnErrPos + 1
  ENDDO
ENDIF






FUNCTION INSCODE
*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º 11/04/98            SMINSCOD.SPR               11:11:59 º
*       º                                                         º
*       ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*       º                                                         º
*       º Author's Name                                           º
*       º                                                         º
*       º Copyright (c) 1998 Company Name                         º
*       º Address                                                 º
*       º City,     Zip                                           º
*       º                                                         º
*       º Description:                                            º
*       º This program was automatically generated by GENSCRN.    º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½


#REGION 0
REGIONAL m.currarea, m.talkstat, m.compstat

IF SET("TALK") = "ON"
	SET TALK OFF
	m.talkstat = "ON"
ELSE
	m.talkstat = "OFF"
ENDIF
m.compstat = SET("COMPATIBLE")
SET COMPATIBLE FOXPLUS

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º               Windows Window definitions                º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*

lcIcon = IIF(FILE("ARIA.ICO"),[ICON FILE "ARIA.ICO"],"")
IF NOT WEXIST("_rzm0o06fc")
	DEFINE WINDOW _rzm0o06fc ;
		AT  0.000, 0.000  ;
		SIZE 8.000,81.800 ;
		TITLE "Insert Code" ;
		FONT "MS Sans Serif", 8 ;
		FLOAT ;
		CLOSE ;
		NOMINIMIZE ;
		DOUBLE ;
		  COLOR RGB(,,,192,192,192) ;
		&lcIcon
	MOVE WINDOW _rzm0o06fc CENTER
ENDIF


*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º             SMINSCOD/Windows Screen Layout              º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*

#REGION 1
IF WVISIBLE("_rzm0o06fc")
	ACTIVATE WINDOW _rzm0o06fc SAME
    llSayCnt=.F.
ELSE
	ACTIVATE WINDOW _rzm0o06fc NOSHOW
    llSayCnt=.T.   
ENDIF

IF llIsEditable
    @ 0.923,4.400 SAY "Code"  ;
	  FONT "MS Sans Serif", 8 ;
	   STYLE "BT"
ENDIF 

  @ 2.615,4.400 SAY "Description"  ;
	FONT "MS Sans Serif", 8 ;
	 STYLE "BT"

IF llIsEditable
    @ 1.000,23.600 GET lcCode ;
	  SIZE 1.000,10.200 ;
	  DEFAULT " " ;
	  FONT "MS Sans Serif", 8 ;
	  PICTURE "@!" ;
	  VALID lfvSCode() ;
	  MESSAGE gfObj_Msg()
ENDIF

IF llIsEditable
    @ 0.846,23.200 TO 0.846,34.400 ;
    PEN 2, 8 ;
	   STYLE "1" ;
	  COLOR RGB(128,128,128,128,128,128)
ENDIF

IF llIsEditable
    @ 2.000,23.400 TO 2.000,34.400 ;
    PEN 2, 8 ;
	   STYLE "1" ;
	  COLOR RGB(255,255,255,255,255,255)
ENDIF

IF llIsEditable
    @ 0.923,23.200 TO 2.077,23.200 ;
    PEN 2, 8 ;
	  COLOR RGB(128,128,128,128,128,128)
ENDIF

IF llIsEditable
    @ 0.923,34.000 TO 2.077,34.000 ;
    PEN 2, 8 ;
	  COLOR RGB(255,255,255,255,255,255)
ENDIF

  @ 2.692,23.600 GET lcDisc ;
	SIZE 1.000,49.600 ;
	DEFAULT " " ;
	FONT "MS Sans Serif", 8 ;
	VALID lfvsDis() ;
	MESSAGE gfObj_Msg()

  @ 2.538,23.200 TO 2.538,73.800 ;
    PEN 2, 8 ;
	 STYLE "1" ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 3.692,23.400 TO 3.692,73.800 ;
    PEN 2, 8 ;
	 STYLE "1" ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 2.615,23.200 TO 3.769,23.200 ;
    PEN 2, 8 ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 2.615,73.400 TO 3.769,73.400 ;
    PEN 2, 8 ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 4.846,22.200 GET lcOk ;
	  PICTURE "@*HT \!\<Ok" ;
	  SIZE 1.923,11.167,0.667 ;
	  DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	 STYLE "B"

  @ 4.692,21.800 TO 4.692,36.200 ;
    PEN 2, 8 ;
	 STYLE "1" ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 6.769,22.000 TO 6.769,36.200 ;
    PEN 2, 8 ;
	 STYLE "1" ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 4.769,21.800 TO 6.846,21.800 ;
    PEN 2, 8 ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 4.769,35.800 TO 6.846,35.800 ;
    PEN 2, 8 ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 4.923,46.200 GET lcCancel ;
	  PICTURE "@*HT \!\<Cancel" ;
	  SIZE 1.923,11.167,0.667 ;
	  DEFAULT 1 ;
	FONT "MS Sans Serif", 8 ;
	 STYLE "B" ;
	VALID _rzm0o06jq()

  @ 4.769,45.800 TO 4.769,60.200 ;
    PEN 2, 8 ;
	 STYLE "1" ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 6.846,46.000 TO 6.846,60.200 ;
    PEN 2, 8 ;
	 STYLE "1" ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 4.846,45.800 TO 6.923,45.800 ;
    PEN 2, 8 ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 4.846,59.800 TO 6.923,59.800 ;
    PEN 2, 8 ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 2.615,19.200 SAY ":"  ;
	FONT "MS Sans Serif", 8 ;
	 STYLE "BT"

IF llIsEditable
    @ 0.923,19.200 SAY ":"  ;
	  FONT "MS Sans Serif", 8 ;
	   STYLE "BT"
ENDIF 

  @ 0.000,0.000 TO 0.000,81.800 ;
    PEN 1, 8 ;
	 STYLE "1" ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 0.000,0.000 TO 8.000,0.000 ;
    PEN 1, 8 ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 0.308,1.000 TO 0.308,81.000 ;
    PEN 1, 8 ;
	 STYLE "1" ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 0.308,0.800 TO 7.693,0.800 ;
    PEN 1, 8

  @ 0.000,81.600 TO 8.000,81.600 ;
    PEN 1, 8 ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 0.308,80.800 TO 7.693,80.800 ;
    PEN 1, 8 ;
	  COLOR RGB(255,255,255,255,255,255)

  @ 7.923,0.000 TO 7.923,81.800 ;
    PEN 1, 8 ;
	 STYLE "1" ;
	  COLOR RGB(128,128,128,128,128,128)

  @ 7.615,0.800 TO 7.615,81.000 ;
    PEN 1, 8 ;
	 STYLE "1" ;
	  COLOR RGB(255,255,255,255,255,255)

lcWindNoGr = ''

IF NOT WVISIBLE("_rzm0o06fc")
	ACTIVATE WINDOW _rzm0o06fc     
ENDIF     


*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º    WindowsREAD contains clauses from SCREEN sminscod    º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*

READ CYCLE MODAL ;
	ACTIVATE lfAriaAct() ;
	COLOR, &GCREADCLR

  RELEASE WINDOW _rzm0o06fc

#REGION 0
IF m.talkstat = "ON"
	SET TALK ON
ENDIF
IF m.compstat = "ON"
	SET COMPATIBLE ON
ENDIF



*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RZM0O06JQ           lcCancel VALID                     º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       Windows                            º
*       º From Screen:         SMINSCOD,     Record Number:   19  º
*       º Variable:            lcCancel                           º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Push Button                        º
*       º Snippet Number:      1                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rzm0o06jq     &&  lcCancel VALID
#REGION 1
lcCode = SPACE(6)

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º LFARIAACT           Read Level Activate                 º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º                                                         º
*       º From Platform:       Windows                            º
*       º From Screen:         SMINSCOD                           º
*       º Called By:           READ Statement                     º
*       º Snippet Number:      2                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION lfAriaAct     && Read Level Activate
IF TYPE('lcWindNoGr') = 'C' AND !EMPTY(lcWindNoGr)
  DO CASE
    CASE WONTOP() $ lcWindNoGr 
      MODIFY WINDOW (WONTOP()) NOGROW
      lcWindNoGr = STRTRAN(lcWindNoGr,WONTOP())
    CASE WPARENT(WONTOP()) $ lcWindNoGr
      MODIFY WINDOW (WPARENT(WONTOP())) NOGROW
      lcWindNoGr = STRTRAN(lcWindNoGr,WPARENT(WONTOP()))
  ENDCASE
ENDIF


