*:********************************************************************
*: Procedure file: SMCODES.PRG 
*:                 (Codes)
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:********************************************************************
*: Modification :
*E300631,1 YMA 05/22/97 Modified the program to work with the codes
*                       file from the data directory instead of working
*                       with the SYDCode from the sysfiles directory.
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*E300459,1 Reham On 08/10/97
*E300459,1 Add function for the picture of the related field {cAdjAcct}.
*E300786,1 RENEE 01/18/98 Sort codes alphabetically
*E300697,4 RENEE 02/25/98 Add cbnkCode, cChkAcct to 
*E300697,4 				  debit\credit adjustment codes
*E300872,5 AMM 05/02/98 Update the communication file 'cmTrace' with 
*E300872,5 AMM          required records.
*E300867,1 AMM 05/06/98 Add a new screen to insert code and add the 
*E300867,1 AMM          checking for the mandatory fields
*B602089,1 AMM 10/20/98 Some Bugs in the code screen
*E301040,1 AMM 10/20/98 Enable the navigation buttons.
*E300930,1 AMM 11/05/98 Related fields validations.
*E301058,4 AMM 11/09/98 Pass two new parameters to gfTracekey .
*B801829,1 AMM 12/15/98 Check the existance of current code before removing 
*B801829,1 AMM          it if that code is included in the style code 
*B801829,1 AMM          structure.
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*B602524,1 AMM 02/07/99 Adjust field width to fit the size entered in 
*B602524,1 AMM          CODESET when inserting a new code 
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*E301217,1 AMM 05/10/99 Adjust scrolling of the related fields screen
*B602941,1 AMM 05/25/99 Remove division group from division code related 
*B602941,1 			    fields if the company setup 'Sequence number based 
*B602941,1 			    on division' is No. Also, refresh default display
*B802096,1 AMM 05/27/99 Enable the user to add new code even if it's the 
*B802096,1 AMM          first part of a previously entered code.
*B802355,1 AMM 06/24/99 Remove the spaces from the variable that holds related fields
*E500308,1 AMM 11/12/99 Allow adding CARRIERCOD as a related field to ShipVia
*B603359,1 AMM 12/22/1999 Fix the bug of not accepting 4 chrs for years in related field type "date"
*B603468,1 NAD 02/22/2000 Do not display division related fields upc type & ucc manufacturer id 
*B603468,1 NAD            when the system is setup to maintain UCC manufaturer ID at company level
*B802969,1 AME 05/23/2000 Rearange the elements of laRelFld array 
*E301513,1 WAM 11/30/2000 Automatic set new sites with communication information
*E301515,1 MAB 12/20/2000 Make a Find button to search codes
*B803923,1 ADEL 02/15/2001 Get the defaut value for the related UD fields.
*B605423,1 RAE 03/04/2002 Disable GL Acount request for not GL linked company
*B605929,1 SSE 05/09/2002 Fix bug variable 'cCode_No' not found on pressing Find after Remove push button.
*B123486,1 NNA 08/05/2004 Fix bug that in Random cases some Empty entries Appear when new colors 
*B123486,1 NNA            are added
*C200597,1 TMI 08/19/2004 Call a customized tracekey function for Bong Wha
*B038431,1 NNA 09/09/2004 Fix bug that when you try to remove a code and you didn't add item code
*B038431,1 NNA            structure yet, you'll get an error massage [Variable 'laseg' not found].
*C123847,1 TMI 12/28/2004 Add a trigger for DIR03 to add custom charge codes.
*B607905,1 TMI 12/25/2006 Fix a bug that when saving the UPS Type in SHIPVIA and reopen the screen does not show right saved value ( Ticket # T20061201.0004 )
*E302472,1 MHM 11/18/2007  Multiple Taxes for Canada on Invoice [T20060709.0008] 
*:********************************************************************

EXTERNAL ARRAY laData,laKeyField,laScrMode,laDefProc,laCtrStat
PRIVATE lnOldRecNo
DECLARE laKeyField [1,4],laRelFld[1,14],laSeqNo[1,2],laComMdls[1,1]
*B602089,1 AMM This array hold all related fields gotten from the SYDFIELD
DECLARE laORelFld[1]
*B602089,1 AMM end
DIMENSION laComp[1], laCodes[1], laCodInfo[1,10]
laCodInfo    = SPACE(1)
laComp       = SPACE(1)
laCodes      = SPACE(1)


*B602089,1 AMM Change the browse title 
*lcBrowTitl   = SUBSTR(gcBaseWindow,2,1) + "Code descriptions"
lcBrowTitl   = "Codes"
*B602089,1 AMM Initialize here to be seen in all functions
lcRltField = SPACE(1)
STORE SPACE(0) TO laORelFld , lcORltFld
*B602089,1 AMM end
*lcBrowTitl   = "Codes"
*B602089,1 AMM Comment out because it isn't used.
*lcBrowWin    = lcBrowTitl
*B602089,1 AMM end
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
*E301077,78 Hesham (Start)               
*IF !lfCkhFiles() OR !gfSetup()
IF !gfSetup() OR !lfCkhFiles() 
*E301077,78 Hesham (End)
  RETURN
ENDIF  

*E301515,1 MAB 12/20/2000 Make a Find button to search codes [Begin]
lcBtFndRec  =  gcBMPHome + "FIND.BMP"
*E301515,1 MAB 12/20/2000 Make a Find button to search codes [End  ]

llFrstEntr = .T.  
lnObject   = 1
lcBrFields = "cFld_name :H='Field Name',cFld_head :H='Description',lRltfields :H='Related Fields'"

IF !WEXIST(lcBaseWind)
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
  *B801829,1 AMM Create an index to be able to seek in the file
  INDEX ON CFILE_NAM TAG CFILE_NAM
  *B801829,1 AMM end
  SELECT (lc_Tmpfl)
  *E300789,1 Hesham (Start)
  *INDEX ON cComp_ID+cCode_No+cRltField+cFld_Name TAG cCODE_NO
  INDEX ON cdefcode+cCode_No+cRltField+cFld_Name TAG cCODE_NO
  *E300789,1 Hesham (End)
  SET ORDER TO TAG CCODE_NO
  *B602524,1 AMM Open file 
  =gfOpenFile(gcSysHome+'SYCCOMP','cComp_Id','SH')
  *B602524,1 AMM end
  
  =gfSubstr(ALLTRIM(SYCCOMP.mcomp_mdl),@laComMdls,"|")
  DECLARE laComMdls[ALEN(laComMdls,1),1]

  *E301098,1 Hesham (Start)        
  *SELECT CCOMP_ID + " - " + CCOM_NAME, CCOMP_ID, cCom_DDir ;
  FROM SYCCOMP INTO ARRAY laComp ORDER BY CCOMP_ID

  SELECT CCOMP_ID + " - " + CCOM_NAME, CCOMP_ID,PADR(gfGetDataDir(ALLT(cCom_DDir)),LEN(cCom_dDir)) ;
  FROM SYCCOMP INTO ARRAY laComp ORDER BY CCOMP_ID
  *E301098,1 Hesham (end)  
  
  
  DIMENSION laComp[ALEN(laComp,1)+1,3]
  = AINS(laComp,1)
  laComp[1] = "Select a Company"
  laComp[2] = lcDummyCom
  laComp[3] = ""

  IF !EMPTY(gcAct_Comp)
    lnComp    = INT(ASCAN(laComp, gcAct_Comp) / 3)+1
    lnActComp = lnComp
    llNoThing = lfvComp()
  ENDIF
    
  *E300786,1 Sort codes alphabetically according to their description
  *SELECT CFLD_HEAD, CFLD_NAME, lRltfields, PADR("|"+UPPER(ALLTRIM(mRltfields))+"|",250);
  FROM SYDFIELD WHERE LVLDENTRY = .T. INTO ARRAY laCodes ;
  ORDER BY CFLD_NAME
  SELECT CFLD_HEAD, CFLD_NAME, lRltfields, PADR("|"+UPPER(ALLTRIM(mRltfields))+"|",250);
  FROM SYDFIELD WHERE LVLDENTRY = .T. INTO ARRAY laCodes ;
  ORDER BY CFLD_HEAD
  *E300786,1 end
  
  DIMENSION laCodes[ALEN(laCodes,1)+1,4]
  = AINS(laCodes,1)
  laCodes[1] = "Select a Code"
  laCodes[2] = SPACE(10)
  laCodes[3] = .F.
  laCodes[4] = SPACE(0)
*E301040,1 AMM Refresh the navigation buttons.
ELSE
  *E301077,78 Hesham (Start) 
  lnComp = lnActComp
  llNoThing = lfvComp()
  *E301077,78 Hesham (End) 
  =lfNavStatus(lnActCode)
ENDIF
*E301040,1 AMM end
IF !glFirstime
  lnComp = lnActComp
  lnCode = lnActCode
ENDIF
lcRelStat    = IIF(laScrMode[1],"DISABLE",IIF(llRlt_Fld,"ENABLE","DISABLE"))

*E301040,1 AMM Disable the browse Bar in the menu and postpone the navigation buttons into the 
*E301040,1 AMM LPSHOW function.
laCtrStat[10] = "DISABLE"	&&browse
*laCtrStat[1] = "DISABLE"
*laCtrStat[2] = "DISABLE"
*laCtrStat[3] = "DISABLE"
*laCtrStat[4] = "DISABLE"
*E301040,1 AMM end
laCtrStat[8] = "DISABLE"	&&Delete
laCtrStat[9] = "DISABLE"	&&Select

*E301040,1 AMM Start, Make the valid function of the navigation bars in the menu 
*E301040,1 AMM indicate the local function in this .PRG
PUSH MENU _MSYSMENU
USE (gcSysHome+"SYCMenu") IN 0 ORDER Pross_ID AGAIN ALIAS MenuFile
*E301040,1 AMM Top Bar
lnSavBarNo = 0
lnSavBarNo = lfGtSavBar("GFCPTOP")
IF lnSavBarNo # 0
  ON SELECTION BAR lnSavBarNo OF P03PU03 DO gfCpTop
ENDIF
*E301040,1 AMM PREVIOUS Bar
lnSavBarNo = lfGtSavBar("GFCPPRVIS")
IF lnSavBarNo # 0
  ON SELECTION BAR lnSavBarNo OF P03PU03 DO gfCpPrvis
ENDIF
*E301040,1 AMM Next Bar
lnSavBarNo = lfGtSavBar("GFCPNEXT")
IF lnSavBarNo # 0
  ON SELECTION BAR lnSavBarNo OF P03PU03 DO gfCpNext
ENDIF
*E301040,1 AMM Buttom Bar
lnSavBarNo = lfGtSavBar("GFCPBTTM")
IF lnSavBarNo # 0
  ON SELECTION BAR lnSavBarNo OF P03PU03 DO gfCpBttm
ENDIF
lnSavBarNo = lfGtSavBar('GFSETFILTR')
*E301040,1 AMM Disable the FILTER Bar
DEFINE BAR (lnSavBarNo) OF P03PU03 PROMPT "Fil\<ter"
SET SKIP OF BAR (lnSavBarNo) OF P03PU03 .T.
USE IN MenuFile
*E301040,1 AMM end

SELECT (lc_Tmpfl)
DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTitl)


*E300683,4 Call *.SPR from screens directory
* DO SMCODES.SPR 
DO (gcScrDir + gcWinAppl + '\SMCODES.SPR')

*E301040,1 AMM Restore the Menu.
RELEASE WINDOW (lcBrowTitl)
POP MENU _MSYSMENU
*E301040,1 AMM end
*E300683,4 end  
SELECT (lc_Tmpfl)
= lfClrTrap()
RELEASE BAR 100 OF P01PU01 

*E301077,78 Hesham (Start) 
*USE IN IIF(USED('CODES'),'CODES',0)
*E301077,78 Hesham (End) 

IF glQuitting
  *RELEASE WINDOW (lcBrowTitl)
  
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
  *B801829,1 AMM delete the temporary index 
  ERASE (gcWorkDir+lc_TmHold+".CDX")
  *B801829,1 AMM end
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
*E301040,1 AMM Enable or disable these buttons due to the screen mode.
*SHOW GET pbTop  DISABLE
*SHOW GET pbPrvs DISABLE
*SHOW GET pbNxt  DISABLE
*SHOW GET pbBtm  DISABLE
*E301040,1 AMM end
SHOW GET pbBrws DISABLE
*E301040,1 AMM Disable the delete and browse buttons
laCtrStat[8]  = "DISABLE"	&&Delete
laCtrStat[10] = "DISABLE"	&&Browse
*E301040,1 AMM end
 
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
    *E301040,1 AMM end
    lnProgSeq  = 0
    DECLARE laSeqNo[1,2]
    laSeqNo    = " "
    lcOldCod   = " "
    SELECT CODES
    *E300789,1 Hesham (Start)
    *LOCATE FOR cFld_Name+cRltField = laCodes[lnActCode,2]+"N" AND !EMPTY(cComp_ID)
    LOCATE FOR cFld_Name+cRltField = laCodes[lnActCode,2]+"N" AND !EMPTY(cDEFCODE)
    lcStat = IIF(FOUND(), "ENABLE", "DISABLE")
    *E300789,1 Hesham (End)
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
    *E300789,1 Hesham (Start)
    *SELECT *,RECNO() AS 'NRECNO' ,'S' AS 'CSTATUS'        ;
           FROM CODES                                     ;
           WHERE cComp_Id+cFld_name+ccode_no+cDiscrep =   ;
                 laComp[lnActComp,2]+laCodes[lnActCode,2] ;
           INTO DBF (gcWorkDir+lc_Tmpfl) ;
           ORDER BY cDiscrep
    *INDEX ON cComp_ID+cCode_No+cRltField+cFld_Name TAG cCODE_NO
    SELECT *,RECNO() AS 'NRECNO' ,'S' AS 'CSTATUS'        ;
           FROM CODES                                     ;
           WHERE cdefcode+cFld_name+ccode_no+cDiscrep =   ;
                 'N'+laCodes[lnActCode,2] ;
           INTO DBF (gcWorkDir+lc_Tmpfl) ;
           ORDER BY cDiscrep
    INDEX ON cdefcode+cCode_No+cRltField+cFld_Name TAG cCODE_NO
    *E300789,1 Hesham (End)
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
    *B802355,1 AMM Remove the spaces
    lcRltField = STRTRAN(lcRltField,SPACE(1))
    *B802355,1 AMM end
    *E300930,1 AMM Extract the mandatory condition function from the related fields string
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
    *E300930,1 AMM end
    llRlt_Fld  = .T.
    *E300867,1 AMM start Adjust the where condition to fit the new figure of saving 
    *E300867,1 AMM related fields .
    *SELECT DISTINCT sydField.cFld_name,;
             PADR(IIF(!EMPTY(sydField.cFld_head),sydField.cFld_head,;
             sydField.cFld_name+SPACE(15)),25),sydField.cData_typ,;
             sydField.nFld_Wdth,sydField.nFld_dec,"",;
             "",sydField.mVald_str,sydField.cVldfnloc,"",;
             sydField.lVldEntry, PADR(ALLTRIM(sydField.mVEntries),250);
       FROM  sydField ;
       WHERE "|"+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
       INTO  ARRAY laRelFld
       *B602089,1 AMM Add a new element to hold the conditional mandatory function 
       *B602089,1 AMM and adjust the where condition
       *SELECT DISTINCT sydField.cFld_name,;
             PADR(IIF(!EMPTY(sydField.cFld_head),sydField.cFld_head,;
             sydField.cFld_name+SPACE(15)),25),sydField.cData_typ,;
             sydField.nFld_Wdth,sydField.nFld_dec,"",;
             "",sydField.mVald_str,sydField.cVldfnloc,"",;
             sydField.lVldEntry, PADR(ALLTRIM(sydField.mVEntries),250),.F.;
       FROM  sydField ;
       WHERE "|"+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
         .OR. "|"+'$'+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
       INTO  ARRAY laRelFld
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
       *B602089,1 AMM Extract the mandatory function and put it in LARELFLD[X,14]
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
       *B602089,1 AMM end
       *E300867,1 AMM end

       *B802969,1 AME Rearange the elements of laRelFld array 
       *B802969,1 AME to be in order with lcRltfield
       FOR lnArayCont = 1 TO ALEN(laRelFld , 1)-1
         FOR I = lnArayCont+1 TO ALEN(laRelFld , 1)
           IF ATC(ALLTRIM(laRelFld[lnArayCont,1]),lcRltField) > ATC(ALLTRIM(laRelFld[I,1]),lcRltField)
             =lfXChng(@laRelFld,lnArayCont,I)
           ENDIF
         ENDFOR
       ENDFOR 
       *B802969,1 AME [end]

       *B602089,1 AMM Get the related fields array, save it in that array 
       *B602089,1 AMM to be reference.
       DIMENSION laORelFld[1]
       =ACOPY(laRelFld,laORelFld)
       *B602089,1 AMM end

  ELSE
    llRlt_Fld = .F.
  ENDIF
  = lfBrowse()
  *--- SSH [Start]
  IF ALLTRIM(laCodes[lnActCode,2]) = "SEASON" .AND. ASCAN(laEvntTrig , PADR('ENABSEA',10)) <> 0
    =gfDoTriger('SMCODES',PADR('ENABSEA',10))
  ENDIF
  *--- SSH [End..]
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
   *B602089,1 AMM start
   *B602089,1 AMM Release the browse before making a new one
   RELEASE WINDOW (lcBrowTitl)
   *B602089,1 AMM Change the browse title
   *lcBrowTitl = substr(gcBaseWindow,2,1)+"View code descriptions"
   lcBrowTitl = "View Codes"
   *B602089,1 AMM Comment out, Not used
   *lcBrowWin  = lcBrowTitl
   SELECT CODES
   lnOldRecNo = RECNO()
   *B602089,1 AMM Adjust browse fields to fit the new font.
   *lcFields = "cMarker=IIF(RECNO()=lnOldRecNo,'',' '):1:R:H=' ':W=.F.,"+IIF(llIsEditable,"cCode_No :6:H='Code',",'')+"cDiscrep :30:H='Description'"
   *B602524,1 AMM Adjust to display with required width
   *lcFields = "cMarker=IIF(RECNO()=lnOldRecNo,' >',' '):2:R:H=' ':W=.F.,"+;
               IIF(llIsEditable,"cCode_No :15:H='Code',",'')+;
               "cDiscrep :45:H='Description'"
   lcFields = "cMarker=IIF(RECNO()=lnOldRecNo,' >',' '):2:R:H=' ':W=.F.,"+;
               IIF(llIsEditable,"cCode_No :P=lcPic:15:H='Code',",'')+;
               "cDiscrep :45:H='Description'"
   *B602524,1 AMM end
    *B602089,1 AMM end        
    *E300789,1 Hesham (Start)
    *BROWSE FOR cComp_ID+cRltField+cFld_Name = laComp[lnActComp,2]+"N"+laCodes[lnActCode,2];
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

    BROWSE FOR cdefcode+cRltField+cFld_Name = "NN"+laCodes[lnActCode,2];
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

  *E300789,1 Hesham (End)
  OTHERWISE
    llMaster   = .F.
    *B602089,1 AMM Release the browse window before making a new one
    RELEASE WINDOW (lcBrowTitl)
    *B602089,1 AMM Change the browse title
    *lcBrowTitl = substr(gcBaseWindow,2,1)+"Add/Edit codes"
    *lcBrowTitl = "Edit Codes"
    lcBrowTitl = IIF(laScrMode[1], "Codes", "View/Edit Codes")
    *B602089,1 AMM Comment out, not used.
    *lcBrowWin  = lcBrowTitl
    *B602089,1 AMM end
    SELECT (lc_Tmpfl)
    lnOldRecNo = RECNO()
    *B602089,1 AMM Change the browse fields to fit the new font
    *lcFields   = "cMarker=IIF(RECNO()=lnOldRecNo,'',' '):1:R:H=' ':W=.F.,"+IIF(llIsEditable,"cCode_No :P=lcPic:6:H='Code' :V=lfvBCode(): W=lfwBCode(),",'')+"cDiscrep :H='Description' :30:V=lfvDiscrep(): W=lfwDiscrep()"
    lcFields   = "cMarker=IIF(RECNO()=lnOldRecNo,' >','  '):2:R:H=' ':W=.F.,"+;
                 IIF(llIsEditable,"cCode_No :P=lcPic:15:H='Code' :V=lfvBCode():;
                 W=lfwBCode(),",'')+"cDiscrep :H='Description' ;
                 :45:V=lfvDiscrep(): W=lfwDiscrep()"

    *B602089,1 AMM end
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
*B600464,1 Object selection function. Used for key traps of 
*B600464,1 Ctrl+Enter and ESC functions.
*B600464,1 Activates a specified window and selects a specified
*B600464,1 object, Both the window name and object number are 
*B600464,1 passed as parameters 

PROCEDURE lpSelObj
PARAMETERS lcWindName, lnObjNum

*B600464,1 lcWindName : window name to be activated
*B600464,1 lnObjNum   : OBJNUM() of the object to be selected.

ACTIVATE WINDOW (lcWindName)
_CUROBJ = lnObjNum
KEYBOARD "{ENTER}" PLAIN
*B600464,1 end.


*:********************************************************************
*!
*!      PROCEDURE: lpTrap
*!
*:********************************************************************
*B600464,1 Modified. The old code is not used any more. Commented out below.
*
PROCEDURE lpTrap
PARAMETER lcFuncName, llMaySelect
*B600464,1 Pressing Alt + character activates the system menu, which is 
*B600464,1 not required here. So, add a dummy keyboard character to get 
*B600464,1 the focus down from the system menu. (any unused character)
*KEYBOARD '{SHIFT+HOME}'
IF llMaySelect
*  ACTIVATE WINDOW (lcCode3)
*  =lfActBrw()
  &lcFuncName
*  =lfDeactBrw()
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

*DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
*ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTitl)

*!**************************************************************************
*!
*!      Function: lfvComp
*!
*!**************************************************************************
*
FUNCTION lfvComp
PRIVATE lnAlias

lnAlias   = SELECT(0)
lnActComp = lnComp
IF lnComp <> 1
*  SELECT IIF(USED("Codes"), "Codes", 0)
*  USE (ALLTRIM(laComp[lnActComp,3]) + "Codes") ORDER Codes
  =IIF(USED("Codes"), gfCloseFile("CODES"), 0)
  =gfOpenFile(ALLTRIM(laComp[lnActComp,3]) + "CODES",'Codes') 
  = gfSubstr(ALLTRIM(SYCCOMP.mcomp_mdl),@laComMdls,"|")
  DECLARE laComMdls[ALEN(laComMdls,1),1]
ENDIF 
SELECT(lnAlias)

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
    *B602089,1 AMM Go to the code record in sydfield to lock it if edit mode
    lcOrdFld = ORDER('SYDFIELD')
    SET ORDER TO TAG cFld_Name IN SYDFIELD
    =SEEK(laCodes[lnActCode,2],'SYDFIELD')
    SET ORDER TO TAG (lcOrdFld) IN SYDFIELD
    *B602089,1 AMM end
  
    SET ORDER TO TAG IDRltFName IN Codes
    *E300789,1 Hesham (Start)
    *IF SEEK(SPACE(2)+"N"+laCodes[lnActCode,2],"CODES")
    IF SEEK("DN"+laCodes[lnActCode,2],"CODES")
    *E300789,1 Hesham (End)
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
* KEYBOARD "{ALT+B}"
*B602089,1 AMM Save the discription of the code
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
*B602089,1 AMM end
  
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
  *E300789,1 Hesham (Start)
  *SEEK laComp[lnActComp,2]+SPACE(6)+"N"+laCodes[lnActCode,2]
  SEEK 'N'+SPACE(6)+"N"+laCodes[lnActCode,2]  
  *E300789,1 Hesham (End)
ELSE
  LOCATE FOR (EMPTY(cdiscrep) OR ALLTRIM(cdiscrep)=CHR(255)) AND cRltField = "N"
ENDIF

IF !FOUND()
  *** Get a temp. seq. no. ***
  lnProgSeq = lnProgSeq + 1
  
  *B602524,1 AMM  Adjust the code width
  *lcCode = IIF(llIsEditable, SPACE(6), "A"+ALLTRIM(STR(lnProgSeq))) 
  lcCode = IIF(llIsEditable, SPACE(lnCodeWdth), "A"+ALLTRIM(STR(lnProgSeq))) 
  *B602524,1 AMM end
  
  IF !EMPTY(laSeqNo[1,1])
    DIMENSION laSeqNo[ALEN(laSeqNo,1)+1,2]
    =AINS(laSeqNo,1)
  ENDIF
  laSeqNo[1,1] = lcCode
  laSeqNo[1,2] = " "
  
  *E300867,1 AMM start, Call the insert field screen
  lcDisc = ''
  PUSH KEY
  ON KEY
  DO (gcScrDir + gcWinAppl + '\SMInsCode.SPR')
  POP KEY
  
  *E300867,1 AMM if the user entered the valid data make records in the temp file.
  IF !(IIF(llIsEditable,EMPTY(lcCode),EMPTY(lcDisc)))
  *E300867,1 AMM end
    *E300867,1 AMM Insert record with a description field
    *INSERT INTO (gcWorkDir+lc_Tmpfl) ;
           (ccomp_id,cfld_name,ccode_no, ;
            cdiscrep,crltfield, ;
            cadd_user,dadd_date,cadd_time,cStatus) ;
    VALUES (laComp[lnActComp,2],laCodes[lnActCode,2],lcCode, ;
            CHR(255),"N", ;
            gcUser_ID,DATE(),gfGetTime(),"A")

    *E300789,1 Hesham (Start)
    *INSERT INTO (gcWorkDir+lc_Tmpfl) ;
           (ccomp_id,cfld_name,ccode_no, ;
            cdiscrep,crltfield, ;
            cadd_user,dadd_date,cadd_time,cStatus) ;
    VALUES (laComp[lnActComp,2],laCodes[lnActCode,2],lcCode, ;
            lcDisc,"N", ;
            gcUser_ID,DATE(),gfGetTime(),"A")

    INSERT INTO (gcWorkDir+lc_Tmpfl) ;
           (cdefcode,cfld_name,ccode_no, ;
            cdiscrep,crltfield, ;
            cadd_user,dadd_date,cadd_time,cStatus) ;
    VALUES ('N',laCodes[lnActCode,2],lcCode, ;
            lcDisc,"N", ;
            gcUser_ID,DATE(),gfGetTime(),"A")

    *E300789,1 Hesham (End)
    *E300867,1 AMM end

    lnCodRec = RECNO(lc_Tmpfl)
 
    *** If it has related fields add its records. ***
    IF llRlt_Fld 
      *B602089,1 AMM Initialize the related field array.
      DIMENSION laRelFld[1]
      =ACOPY(laORelFld,laRelFld)
      *B602089,1 AMM end
      FOR lnCount3 = 1 TO ALEN(laRelFld,1)
        *E300789,1 Hesham (Start)
        *INSERT INTO (gcWorkDir+lc_Tmpfl) ;
               (ccomp_id,cfld_name,ccode_no, ;
                cdiscrep,crltfield, ;
                cRltd_Nam,cRltd_Typ,cRltd_Vlu,;
                cadd_user,dadd_date,cadd_time,cStatus) ;
        VALUES (laComp[lnActComp,2],laCodes[lnActCode,2],lcCode, ;
                "","Y",laRelFld[lnCount3,1],;
                laRelFld[lnCount3,3],"", ;
                gcUser_ID,DATE(),gfGetTime(),"A")

        INSERT INTO (gcWorkDir+lc_Tmpfl) ;
               (cdefcode,cfld_name,ccode_no, ;
                cdiscrep,crltfield, ;
                cRltd_Nam,cRltd_Typ,cRltd_Vlu,;
                cadd_user,dadd_date,cadd_time,cStatus) ;
        VALUES ('N',laCodes[lnActCode,2],lcCode, ;
                "","Y",laRelFld[lnCount3,1],;
                laRelFld[lnCount3,3],"", ;
                gcUser_ID,DATE(),gfGetTime(),"A")
      
      *E300789,1 Hesham (End)
      ENDFOR
    ENDIF
    =gfUpdate()
    IF lnCodRec > 0 .AND. lnCodRec <= RECCOUNT()
      GO lnCodRec
    ENDIF
  *E300867,1 AMM start, Call the related field screen
    IF llRlt_Fld
      =lfvReltdFld()
    ENDIF
  ENDIF
  *E300867,1 AMM end
  
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

*B038431,1 NNA 09/09/2004 (Begin) Check if there is item code Structure or not
IF !lfSegInfo() 
  *--Item structure not found, Cannot Proceed.
  =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
  glQuitting = .T.
  RETURN
ENDIF
*B038431,1 NNA (End)

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

*!**************************************************************************
*!
*!      Function: lfChckCode
*!
*!**************************************************************************
*
FUNCTION lfChckCode
*B600464,1 Add a variable to get the data directory path of
*B600464,1 the selected company (lcDataDir)
PRIVATE lcDataDir
*B600464,1 end.

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

*B801829,1 AMM Comment out, trannsfer this peice of code into another location
*SELECT cFile_Nam FROM sydFlFld ;
       INTO ARRAY laFileList;
       WHERE cFld_Name+STR(NFLD_POS) = laCodes[lnActCode,2]

*B801829,1 AMM Variable to hold the length of the code segment of the current 
*B801829,1 AMM code in style code structure
lnLen   = 1
*B801829,1 AMM Variable to hold the start position of  the code segment of the 
*B801829,1 AMM current code in style code structure
lnStLen = 1
*B801829,1 AMM Variable hold the Type of the code segment in the style code structure
lcCdTyp = SPACE(0)
llStyCod = .F.
IF INLIST(laCodes[lnActCode,2] ,"SEASON","COLOR","CDIVISION","CSTYGROUP")
  lcCdTyp = IIF( laCodes[lnActCode,2]= 'SEASON' ,'Z', IIF(laCodes[lnActCode,2] = 'COLOR', 'C', IIF(laCodes[lnActCode,2]= 'CDIVISION','D','G') ) )
ENDIF
DIMENSION laMajSeg[1,1]
*B801829,1 AMM Get the style code structure
= gfItemMask(@laMajSeg,ALLTRIM(laComp[lnComp,3]))
lcSetEx = SET('EXACT')
SET EXACT ON
*B801829,1 AMM If the type of the current code is included in the style code 
*B801829,1 AMM struct. get its position and length
IF !EMPTY(lcCdTyp) .AND. ASCAN(laMajSeg,lcCdTyp) # 0
  FOR lnC = 1 TO ALEN(laMajSeg,1)
    IF laMajSeg[lnC,1] = lcCdTyp
      lnLen = LEN(laMajSeg[lnC,3])
      llStyCod = .T.
      EXIT
    ENDIF
    lnStLen = lnStLen + LEN(laMajSeg[lnC,3])       
  ENDFOR
ENDIF
SET EXACT &lcSetEx

llopchg = .F.
IF llStyCod
  *B801829,1 AMM If the current code is included in the style code structure, 
  *B801829,1 AMM Collect the files that has the style field and its alternatives.
  llopchg = gfOpenFile(gcSysHome+'SYDFDCHG','SYDFDCHG','SH')
  SELECT DISTINCT cFile_Nam,'N' FROM sydFlFld ;
       WHERE cFld_Name+STR(NFLD_POS) = laCodes[lnActCode,2];
       UNION ;
        SELECT cFile_Nam ,'S' FROM sydFlFld WHERE cFld_Name = 'STYLE';
        UNION   ;
          SELECT cFile_Nam,'K' FROM SYDFDCHG ;
             WHERE SYDFDCHG.cKeyType+SYDFDCHG.cFld_Name = 'K'+'STYLE';
       INTO ARRAY laFileList
  SELECT SYDFLFLD
  SET ORDER TO TAG Cfld_name
ELSE
  SELECT cFile_Nam,'N' FROM sydFlFld ;
       INTO ARRAY laFileList;
       WHERE cFld_Name+STR(NFLD_POS) = laCodes[lnActCode,2]
ENDIF
*B801829,1 AMM end

SET TALK &lcTalk

IF _TALLY = 0
  *B602089,1 AMM Change the message
  *** There is no file have this code, you can ***
  *** delete it.  Do you want to delete it?    ***
  *B602089,1 AMM "No files are using this code.  Would you like to delete it ?"
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
  *E301098,1 Hesham (Start)
  *lcDataDir = IIF(SEEK(laComp[lnComp,2],'SYCCOMP'),ALLTRIM(SYCCOMP.cCom_DDir), gcDataDir)
  lcDataDir = gfGetDataDir(IIF(SEEK(laComp[lnComp,2],'SYCCOMP'),ALLTRIM(SYCCOMP.cCom_DDir), gcDataDir))
  *E301098,1 Hesham (End)

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
      
      *B801829,1 AMM start Comment out, Make this peice of code under condition.
      *LOCATE FOR &laCodes[lnActCode,2] = &lc_Tmpfl..cCode_no
      *IF FOUND()
        *llFound = .T.
        *SELECT (lc_TmHold)
        *INSERT INTO (gcWorkDir+lc_TmHold) ;
               (cFile_nam,cFile_ttl) ;
        VALUES (laFileList[lnCount2,1],;
               LOOKUP(SYDFILES.cFile_ttl,;
               laFileList[lnCount2,1],SYDFILES.cFILE_nam,'CFILE_NAM'))
      *ENDIF
      DO CASE
        *B801829,1 AMM Case of normal field
        CASE laFileList[lnCount2,2] = 'N'
          LOCATE FOR &laCodes[lnActCode,2] = &lc_Tmpfl..cCode_no
          IF FOUND() .AND. !SEEK(laFileList[lnCount2,1],lc_TmHold)
            llFound = .T.
            SELECT (lc_TmHold)
            INSERT INTO (gcWorkDir+lc_TmHold) ;
                 (cFile_nam,cFile_ttl) ;
            VALUES (laFileList[lnCount2,1],;
                 LOOKUP(SYDFILES.cFile_ttl,;
                 laFileList[lnCount2,1],SYDFILES.cFILE_nam,'CFILE_NAM'))
          ENDIF
        *B801829,1 AMM Case of Code is included in the Style code structure and 
        *B801829,1 AMM the field name is "STYLE"
        CASE laFileList[lnCount2,2] = 'S'
          LOCATE FOR SUBSTR(Style,lnStLen+1,lnLen) = &lc_Tmpfl..cCode_no
          IF FOUND() .AND. !SEEK(laFileList[lnCount2,1],lc_TmHold)
            llFound = .T.
            INSERT INTO (gcWorkDir+lc_TmHold) ;
               (cFile_nam,cFile_ttl) ;
            VALUES (laFileList[lnCount2,1],;
               LOOKUP(SYDFILES.cFile_ttl,;
               laFileList[lnCount2,1],SYDFILES.cFILE_nam,'CFILE_NAM'))
          ENDIF
        *B801829,1 AMM Case of Code is included in the Style code structure and 
        *B801829,1 AMM the field is alternative to the "STYLE" field
        CASE laFileList[lnCount2,2] = 'K'
          =SEEK('K'+PADR('STYLE',10)+lcFile,'SYDFDCHG')
          lcFldtmp = ALLTRIM(SYDFDCHG.cAltField)
          lcFldtmp =IIF(EMPTY(lcFldtmp),'STYLE',lcFldtmp)
          LOCATE FOR SUBSTR(&lcFldtmp,lnStLen+1,lnLen) = &lc_Tmpfl..cCode_no
          IF FOUND() .AND. !SEEK(lcFldtmp,lc_TmHold)
            llFound = .T.
            INSERT INTO (gcWorkDir+lc_TmHold) ;
               (cFile_nam,cFile_ttl) ;
            VALUES (lcFldtmp,;
               LOOKUP(SYDFILES.cFile_ttl,;
               lcFldtmp,SYDFILES.cFILE_nam,'CFILE_NAM'))
          ENDIF

      ENDCASE
      *B801829,1 AMM end
      
      IF llOpenFile
        llOpenFile = .F.
        USE IN ALIAS(lcFile)
      ENDIF
    ENDIF
  ENDFOR
*B801829,1 AMM Close file
IF llopchg .AND. USED('SYDFDCHG')
  USE IN SYDFDCHG
ENDIF
*B801829,1 AMM end
  
  IF llFound
    *** Special trap for this browse that hold ***
    *** all the files that has this code...
    ON KEY LABEL TAB        DO lfTrapFlCd
    ON KEY LABEL BACKTAB    DO lfTrapFlCd
    
    *B600464,1 Change English text of the browse title.
    *lcFileTitl = "Files have code "+"'"+ALLTRIM(SYDFIELD.cFld_head)+"'"
    lcFileTitl = "Files using " + ALLTRIM(&lc_TmpFl..cDiscrep)
    
    *B600464,1 Release browse pad upon entry.
    RELEASE BAR 100 OF P01PU01
    *B600464,1 end.
    
    *** Display all the files have this code in its data. ***
    *E300683,4 Call *.SPR from screens directory
    * DO SMFLCD.SPR 
    DO (gcScrDir + gcWinAppl + '\SMFLCD.SPR')
    *E300683,4 end  
    *** Back to the main trap of the main screen. ***
    *B600464,1 Removed the following line.
    *B600464,1 If called from a browse, key traps are reset through
    *B600464,1 the calling function (lpTrap)
    *B600464,1 Otherwise, there are no key traps.
    *B600464,1 Added redefinition of browse pads and release traps.
    *=lfPushKey()  
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



IF lcOldCod <> laComp[lnComp,2] + laCodes[lnActCode,2]
  ** Collect all the deleted records from th code file **
  ** in the temp. Recall file ..
  SELECT * FROM (gcWorkDir+lc_TmpRcl) INTO DBF (gcWorkDir+lc_TmRcTm)
  
  lcSaveDel = SET ('DELETE')
  SET DELETE OFF
  *E300789,1 Hesham (Start)
  *SELECT *,RECNO() AS 'NRECNO', 'S' AS 'CSTATUS'     ;
         FROM CODES                                  ;
         WHERE cComp_Id + cRltField + cFld_name =    ;
               laComp[lnActComp,2]+"N"+ laCodes[lnActCode,2]    ;
              .AND. DELETED()                        ;
         UNION (SELECT * FROM (gcWorkDir+lc_TmRcTm)) ;
         INTO DBF (gcWorkDir+lc_TmpRcl)

  SELECT *,RECNO() AS 'NRECNO', 'S' AS 'CSTATUS'     ;
         FROM CODES                                  ;
         WHERE cdefcode + cRltField + cFld_name =    ;
               "NN"+ laCodes[lnActCode,2]    ;
              .AND. DELETED()                        ;
         UNION (SELECT * FROM (gcWorkDir+lc_TmRcTm)) ;
         INTO DBF (gcWorkDir+lc_TmpRcl)

  *E300789,1 Hesham (End)
  SET DELETE &lcSaveDel
ENDIF
*E300789,1 Hesham (Start)
*lcOldCod = laComp[lnComp,2] + laCodes[lnActCode,2]
lcOldCod = 'N' + laCodes[lnActCode,2]
*E300789,1 Hesham (End)

SELECT (lc_TmpRcl)
GO TOP
IF EOF()
  *** If there is no deleted records to be recalled. ***
  *** There are no deleted records for this code. ***
  *** <  Ok  > ***
  =gfModalGen("INM00045B00000","DIALOG")
ELSE
  *** If there is records to be recall, excute **
  *** the DELCOD screen to display the records **
  PUSh KEY
  ON KEY
  lsDelCod = 1
  RELEASE BAR 100 OF P01PU01
  *E300683,4 Call *.SPR from screens directory
  * DO SMDELCD.SPR 

  DO (gcScrDir + gcWinAppl + '\SMDELCD.SPR')
  *E300683,4 end  
  POP KEY
  DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
  ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTitl)
  SHOW WINDOW (lcBrowTitl) REFRESH
ENDIF


SELECT SYDFIELD



*!**************************************************************************
*!
*!      Function: lfvRecl
*!
*!**************************************************************************
*
FUNCTION lfvRecl

SELECT (lc_Tmpfl)
lnCodRec = RECNO(lc_Tmpfl)
*B801829,1 AMM Change to fit the editable codes
*LOCATE FOR cDiscrep = &lc_TmpRcl..cDiscrep
IF !llIsEditable
  LOCATE FOR cDiscrep = &lc_TmpRcl..cDiscrep
ELSE
  LOCATE FOR cCode_No = &lc_TmpRcl..cCode_No
ENDIF
*B801829,1 AMM end

IF FOUND()
  *** This field already exists. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00053B00000","DIALOG")
  
  SELECT (lc_Tmpfl)
  IF lnCodRec > 0 .AND. lnCodRec <= RECCOUNT()
    GO lnCodRec
  ENDIF
  RETURN
ELSE
  lcSaveDel = SET ('DELETE')
  SET DELETE OFF
  SELECT (lc_Tmpfl)
  *E300789,1 Hesham (Start)
  *LOCATE FOR &lc_Tmpfl..cComp_Id+&lc_Tmpfl..cFld_name+;
             &lc_Tmpfl..ccode_no+&lc_Tmpfl..cDiscrep = ;
             &lc_TmpRcl..cComp_Id+&lc_TmpRcl..cFld_name+;
             &lc_TmpRcl..ccode_no+&lc_TmpRcl..cDiscrep ;
             .AND. DELETED()

  LOCATE FOR &lc_Tmpfl..cdefcode+&lc_Tmpfl..cFld_name+;
             &lc_Tmpfl..ccode_no+&lc_Tmpfl..cDiscrep = ;
             &lc_TmpRcl..cdefcode+&lc_TmpRcl..cFld_name+;
             &lc_TmpRcl..ccode_no+&lc_TmpRcl..cDiscrep ;
             .AND. DELETED()


  *E300789,1 Hesham (End)
  IF FOUND()
    ** If the recalled record  was deleted  from the **
    ** temp file, Recall the record in the temp file **
    ** file & deleted from the Recalled file ..
    RECALL
    
    *B600464,1 If the status is an 'O'ld record, meaning, 
    *B600464,1 it is an original record coming from the file,
    *B600464,1 removed and then recalled, replace its status
    *B600464,1 with 'S', if it is previously deleted ('S'),
    *B600464,1 'R'ecall it, otherwise keep the same status.
    *REPLACE cStatus WITH &lc_TmpRcl..cStatus
    REPLACE cStatus WITH SUBSTR('RASMR',;
                                AT(&lc_TmpRcl..cStatus, 'SAOMR'), 1)
    *B600464,1 end.
    =gfAdd_Info()
    
    lnCodRec = RECNO(lc_Tmpfl)
    
    lcCode_no = &lc_Tmpfl..cCode_no

    *** If it has related fields add its records. ***
    IF llRlt_Fld
      FOR lnCount3 = 1 TO ALEN(laRelFld,1)
      *E300789,1 Hesham (Start)
        *INSERT INTO (gcWorkDir+lc_Tmpfl) ;
               (ccomp_id,cfld_name,ccode_no, ;
                cdiscrep,crltfield, ;
                cRltd_Nam,cRltd_Typ,cRltd_Vlu, ;
                cadd_user,dadd_date,cadd_time,cStatus) ;
        VALUES (laComp[lnComp,2],laCodes[lnActCode,2],lcCode_no, ;
                "","Y",laRelFld[lnCount3,1], ;
                laRelFld[lnCount3,3],"", ;
                gcUser_ID,DATE(),gfGetTime(),"A")

        INSERT INTO (gcWorkDir+lc_Tmpfl) ;
               (cdefcode,cfld_name,ccode_no, ;
                cdiscrep,crltfield, ;
                cRltd_Nam,cRltd_Typ,cRltd_Vlu, ;
                cadd_user,dadd_date,cadd_time,cStatus) ;
        VALUES ('N',laCodes[lnActCode,2],lcCode_no, ;
                "","Y",laRelFld[lnCount3,1], ;
                laRelFld[lnCount3,3],"", ;
                gcUser_ID,DATE(),gfGetTime(),"A")

     *E300789,1 Hesham (End)
      ENDFOR
    ENDIF
    
    IF lnCodRec > 0 .AND. lnCodRec <= RECCOUNT()
      GO lnCodRec
    ENDIF

    SELECT (lc_TmpRcl)
    DELETE
  ELSE

    ** If the recalled record was deleted from the **
    ** code file put it back in the temp file ..
    SELECT (lc_TmpRcl)
    SCATTER MEMVAR MEMO
    DELETE
    
    SELECT (lc_Tmpfl)
    APPEND BLANK
    GATHER MEMVAR MEMO	

    *B600464,1 If the status is an 'O'ld record, meaning, 
    *B600464,1 it is an original record coming from the file,
    *B600464,1 removed and then recalled, replace its status
    *B600464,1 with 'S', if it is previously deleted ('S'),
    *B600464,1 'R'ecall it, otherwise keep the same status.
    *REPLACE cStatus WITH 'R'
    REPLACE cStatus WITH SUBSTR('RASMR',;
                                AT(&lc_TmpRcl..cStatus, 'SAOMR'), 1)
    *B600464,1 end.
    
    =gfAdd_Info()
    
    lnCodRec = RECNO(lc_Tmpfl)

    lcCode_no = &lc_Tmpfl..cCode_no

    *** If it has related fields add its records. ***
    IF llRlt_Fld
      FOR lnCount3 = 1 TO ALEN(laRelFld,1)
        *E300789,1 Hesham (Start)
        *INSERT INTO (gcWorkDir+lc_Tmpfl) ;
               (ccomp_id,cfld_name,ccode_no, ;
                cdiscrep,crltfield, ;
                cRltd_Nam,cRltd_Typ,cRltd_Vlu, ;
                cadd_user,dadd_date,cadd_time,cStatus) ;
        VALUES (laComp[lnComp,2],laCodes[lnActCode,2],lcCode_no, ;
                "","Y",laRelFld[lnCount3,1], ;
                laRelFld[lnCount3,3],"", ;
                gcUser_ID,DATE(),gfGetTime(),"A")

        INSERT INTO (gcWorkDir+lc_Tmpfl) ;
               (cdefcode,cfld_name,ccode_no, ;
                cdiscrep,crltfield, ;
                cRltd_Nam,cRltd_Typ,cRltd_Vlu, ;
                cadd_user,dadd_date,cadd_time,cStatus) ;
        VALUES ('N',laCodes[lnActCode,2],lcCode_no, ;
                "","Y",laRelFld[lnCount3,1], ;
                laRelFld[lnCount3,3],"", ;
                gcUser_ID,DATE(),gfGetTime(),"A")

       *E300789,1 Hesham (End)
      ENDFOR
    ENDIF
    IF lnCodRec > 0 .AND. lnCodRec <= RECCOUNT()
      GO lnCodRec
    ENDIF
    
  ENDIF
  SET DELETE &lcSaveDel
  
  SELECT (lc_Tmpfl)
  IF lnCodRec > 0 .AND. lnCodRec <= RECCOUNT()
    GO lnCodRec
  ENDIF
  =gfUpdate()
ENDIF
CLEAR READ

*!**************************************************************************
*!
*!      Function: lfvReltdFld
*!
*!**************************************************************************
*
FUNCTION lfvReltdFld
PRIVATE lcCurCodNo




*--- SSH [Start]
IF ALLTRIM(laCodes[lnActCode,2]) = "SEASON" .AND. ASCAN(laEvntTrig , PADR('CODESCR',10)) <> 0
  =gfDoTriger('SMCODES',PADR('CODESCR',10))
  RETURN
ENDIF
*--- SSH [End..]

IF llIsEditable
  lcCode = IIF(laScrMode[2],CODES.cCode_No,&lc_TmpFl..cCode_No)
  IF EMPTY(lcCode)
    *** You should enter a description for this code first...! ***
    *** <  Ok  > ***
    = gfModalGen("TRM00041B00000","DIALOG")
    RETURN
  ENDIF
ELSE
  lcDescrip = IIF(laScrMode[2],CODES.cdiscrep,&lc_TmpFl..cdiscrep)
  IF EMPTY(lcDescrip)
    *** You should enter a description for this code first...! ***
    *** <  Ok  > ***
    = gfModalGen("TRM00041B00000","DIALOG")
    RETURN
  ENDIF
ENDIF  

*** Collect the data from codes or field. ***
lcRltField = "|" + lfGetRFStr(laCodes[lnActCode,2],laComp[lnActComp,2]) + "|" 
*B802355,1 AMM start, remove the spaces
lcRltField = STRTRAN(lcRltField,SPACE(1))
*B802355,1 AMM end
*E300930,1 AMM Extract the mandatory condition function from the related fields varable
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
*E300930,1 AMM end

lcCurCodNo = IIF(laScrMode[2], CODES.cCode_No, &lc_TmpFl..cCode_No)

DIMENSION laCodInfo[1,10]
laCodInfo = SPACE(0)
*E300867,1 AMM start, Add element to the array to hold mandatory data.
*DECLARE laRelFld[1,12]
DECLARE laRelFld[1,14]
*E300867,1 AMM end
laRelFld = " "

IF laScrMode[2]
  lcCode_no = Codes.cCode_no
  lnCodRec  = RECNO("CODES")

  _TALLY = 0
  *E300789,1 Hesham (Start)
  *SELECT DISTINCT sydField.cFld_name,;
         PADR(IIF(!EMPTY(sydField.cFld_head),sydField.cFld_head,  ;
         sydField.cFld_name+SPACE(15)),25),sydField.cData_typ,    ;
         sydField.nFld_Wdth,sydField.nFld_dec,CODES.cRltd_vlu,    ;
         "",sydField.mVald_str,sydField.cVldfnloc," ",            ;
         sydField.lVldEntry, PADR(ALLTRIM(sydField.mVEntries),250);
   FROM  sydField,CODES                                           ;
   WHERE CODES.cRltd_Nam = SYDFIELD.cFld_Name                     ;
         AND cComp_id + cCode_no = laComp[lnActComp,2]+lcCode_no  ;
         AND cComp_id + cRltfield + CODES.cFld_Name =             ;
              laComp[lnActComp,2] + "Y" + laCodes[lnActCode,2]                 ;
    INTO ARRAY laRelFld

  SELECT DISTINCT sydField.cFld_name,;
         PADR(IIF(!EMPTY(sydField.cFld_head),sydField.cFld_head,  ;
         sydField.cFld_name+SPACE(15)),25),sydField.cData_typ,    ;
         sydField.nFld_Wdth,sydField.nFld_dec,CODES.cRltd_vlu,    ;
         "",sydField.mVald_str,sydField.cVldfnloc," ",            ;
         sydField.lVldEntry, PADR(ALLTRIM(sydField.mVEntries),250);
   FROM  sydField,CODES                                           ;
   WHERE CODES.cRltd_Nam = SYDFIELD.cFld_Name                     ;
         AND cdefcode + cCode_no = 'N'+lcCode_no  ;
         AND cdefcode + cRltfield + CODES.cFld_Name =             ;
              'N' + "Y" + laCodes[lnActCode,2]                 ;
    INTO ARRAY laRelFld

  *E300789,1 Hesham (End)
  IF _TALLY = 0
    *E300867,1 AMM Adjust the where clause to fit the new figure of the related 
    *E300867,1 AMM field saving.
    *SELECT DISTINCT sydField.cFld_name,;
           PADR(IIF(!EMPTY(sydField.cFld_head),sydField.cFld_head,  ;
           sydField.cFld_name+SPACE(15)),25),sydField.cData_typ,    ;
           sydField.nFld_Wdth,sydField.nFld_dec,"",                 ;
           "",sydField.mVald_str,sydField.cVldfnloc,"",             ;
           sydField.lVldEntry,                                      ;
           PADR(ALLTRIM(sydField.mVEntries),250)                    ; 
      FROM  (gcSysHome+"sydField") ;
     WHERE "|"+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
      INTO ARRAY laRelFld

    *B602089,1 AMM No need to execute this SQL again, the data is already in laORelFld
    *SELECT DISTINCT sydField.cFld_name,;
           PADR(IIF(!EMPTY(sydField.cFld_head),sydField.cFld_head,  ;
           sydField.cFld_name+SPACE(15)),25),sydField.cData_typ,    ;
           sydField.nFld_Wdth,sydField.nFld_dec,"",                 ;
           "",sydField.mVald_str,sydField.cVldfnloc,"",             ;
           sydField.lVldEntry,                                      ;
           PADR(ALLTRIM(sydField.mVEntries),250)                    ; 
      FROM  (gcSysHome+"sydField") ;
     WHERE "|"+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
       .OR. "|"+'$'+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
      INTO ARRAY laRelFld
    DIMENSION laRelFld[1]
    =ACOPY(laORelFld,laRelFld)
    *B602089,1 AMM end
      *E300867,1 AMM end
  ENDIF
  *B602089,1 AMM If there is extra related fields in the original array, add it to related field array
  llCopy = .F.
  IF ALEN(laRelFld,1) # ALEN(laORelFld,1)
    llCopy = .T.
  ELSE
    FOR lnC = 1 TO ALEN(laORelFld,1)
      IF laORelFld[lnC,1] # laRelFld[lnC,1]
        llCopy = .T.
      ENDIF
    ENDFOR
  ENDIF
  IF llCopy
    =ACOPY(laORelFld,laTmpRel)
    FOR lnC=1 TO ALEN(laRelFld,1)
      IF ASCAN(laTmpRel,laRelFld[lnC,1]) # 0
        laTmpRel[ASCAN(laTmpRel,laRelFld[lnC,1])+5] = laRelFld[lnC,6]
      ENDIF
    ENDFOR
    DIMENSION laRelFld[1]
    =ACOPY(laTmpRel,laRelFld)
    RELEASE laTmpRel
  ENDIF
  *B602089,1 AMM end
 
  
  FOR lnArayCont = 1 TO ALEN(laRelFld , 1)
    *B603359,1 AMM Adjust in case of date
    *laRelFld[lnArayCont,6] = PADR(laRelFld[lnArayCont,6] , laRelFld[lnArayCont,4])
    IF laRelFld[lnArayCont,3] # 'D'
      laRelFld[lnArayCont,6] = PADR(laRelFld[lnArayCont,6] , laRelFld[lnArayCont,4])
    ENDIF
    *B603359,1 AMM end
    *B602089,1 AMM start, replace the values of True & False with Yes & No
    IF laRelFld[lnArayCont,3] = 'L'
      laRelFld[lnArayCont,6] = IIF(laRelFld[lnArayCont,6]='T','Y','N')
    ENDIF
    *B602089,1 AMM end
    
  ENDFOR   && End of FOR

  SELECT CODES
  IF lnCodRec > 0 .AND. lnCodRec <= RECCOUNT()
    GO lnCodRec
  ENDIF
ELSE
  lcCodeRltd = &lc_Tmpfl..cCode_no
  lcCode_no  = &lc_Tmpfl..cCode_no
  lnCodRec   = RECNO(lc_Tmpfl)

  _TALLY = 0
  *E300867,1 AMM start, Add a field to the array to hold if the field is mandatory or not.
  *SELECT DISTINCT sydField.cFld_name,;
         PADR(IIF(!EMPTY(sydField.cFld_head),sydField.cFld_head,  ;
         sydField.cFld_name+SPACE(15)),25),sydField.cData_typ,    ;
         sydField.nFld_Wdth,sydField.nFld_dec,&lc_Tmpfl..cRltd_vlu,;
         "",sydField.mVald_str,sydField.cVldfnloc," ",            ;
         sydField.lVldEntry,                                      ;
         PADR(ALLTRIM(sydField.mVEntries),250)                    ; 
   FROM  sydField,(lc_Tmpfl)                                      ;
   WHERE  &lc_Tmpfl..cRltd_Nam = SYDFIELD.cFld_Name               ;
         AND cComp_id + cCode_no =                                ;
              laComp[lnActComp,2] + lcCode_no                     ;
         AND cComp_id + cRltfield + &lc_Tmpfl..cFld_Name =        ;
              laComp[lnActComp,2] + "Y" + laCodes[lnActCode,2]               ;
   INTO  ARRAY laRelFld
   *E300789,1 Hesham (Start)
   *SELECT DISTINCT sydField.cFld_name,;
         PADR(IIF(!EMPTY(sydField.cFld_head),sydField.cFld_head,  ;
         sydField.cFld_name+SPACE(15)),25),sydField.cData_typ,    ;
         sydField.nFld_Wdth,sydField.nFld_dec,&lc_Tmpfl..cRltd_vlu,;
         "",sydField.mVald_str,sydField.cVldfnloc," ",            ;
         sydField.lVldEntry,                                      ;
         PADR(ALLTRIM(sydField.mVEntries),250),.F.,''                ; 
   FROM  sydField,(lc_Tmpfl)                                      ;
   WHERE  &lc_Tmpfl..cRltd_Nam = SYDFIELD.cFld_Name               ;
         AND cComp_id + cCode_no =                                ;
              laComp[lnActComp,2] + lcCode_no                     ;
         AND cComp_id + cRltfield + &lc_Tmpfl..cFld_Name =        ;
              laComp[lnActComp,2] + "Y" + laCodes[lnActCode,2]               ;
   INTO  ARRAY laRelFld

   SELECT DISTINCT sydField.cFld_name,;
         PADR(IIF(!EMPTY(sydField.cFld_head),sydField.cFld_head,  ;
         sydField.cFld_name+SPACE(15)),25),sydField.cData_typ,    ;
         sydField.nFld_Wdth,sydField.nFld_dec,&lc_Tmpfl..cRltd_vlu,;
         "",sydField.mVald_str,sydField.cVldfnloc," ",            ;
         sydField.lVldEntry,                                      ;
         PADR(ALLTRIM(sydField.mVEntries),250),.F.,''                ; 
   FROM  sydField,(lc_Tmpfl)                                      ;
   WHERE  &lc_Tmpfl..cRltd_Nam = SYDFIELD.cFld_Name               ;
         AND cdefcode + cCode_no =                                ;
              'N' + lcCode_no                     ;
         AND cdefcode + cRltfield + &lc_Tmpfl..cFld_Name =        ;
              'N' + "Y" + laCodes[lnActCode,2]               ;
   INTO  ARRAY laRelFld

   *E300789,1 Hesham (End)
   *E300867,1 AMM end


  IF _TALLY = 0
    *E300867,1 AMM Adjust the where clause to fit the new figure of related field saving
    *SELECT DISTINCT sydField.cFld_name,;
           PADR(IIF(!EMPTY(sydField.cFld_head),sydField.cFld_head,  ;
           sydField.cFld_name+SPACE(15)),25),sydField.cData_typ,    ;
           sydField.nFld_Wdth,sydField.nFld_dec,"",                 ;
           "",sydField.mVald_str,sydField.cVldfnloc,"",             ;
           sydField.lVldEntry,                                      ;
           PADR(ALLTRIM(sydField.mVEntries),250)                    ; 
     FROM  (gcSysHome+"sydField") ;
     WHERE "|"+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
     INTO  ARRAY laRelFld
     
     *SELECT DISTINCT sydField.cFld_name,;
           PADR(IIF(!EMPTY(sydField.cFld_head),sydField.cFld_head,  ;
           sydField.cFld_name+SPACE(15)),25),sydField.cData_typ,    ;
           sydField.nFld_Wdth,sydField.nFld_dec,"",                 ;
           "",sydField.mVald_str,sydField.cVldfnloc,"",             ;
           sydField.lVldEntry,                                      ;
           PADR(ALLTRIM(sydField.mVEntries),250), .F.               ; 
     FROM  (gcSysHome+"sydField") ;
     WHERE "|"+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
       .OR. "|"+'$'+UPPER(ALLTRIM(sydField.cFld_name))+"|" $ lcRltField ;
     INTO  ARRAY laRelFld
     DIMENSION laRelFld[1]
     =ACOPY(laORelFld,laRelFld)
     *E300867,1 AMM end
  ENDIF
  *B602089,1 AMM If there is extra related fields in the original related fields 
  *B602089,1 AMM array, add it to related fields array.
  llCopy = .F.
  IF ALEN(laRelFld,1) # ALEN(laORelFld,1)
    llCopy = .T.
  ELSE
    FOR lnC = 1 TO ALEN(laORelFld,1)
      IF laORelFld[lnC,1] # laRelFld[lnC,1]
        llCopy = .T.
      ENDIF
    ENDFOR
  ENDIF
  IF llCopy
    =ACOPY(laORelFld,laTmpRel)
    FOR lnC=1 TO ALEN(laRelFld,1)
      IF ASCAN(laTmpRel,laRelFld[lnC,1]) # 0
        laTmpRel[ASCAN(laTmpRel,laRelFld[lnC,1])+5] = laRelFld[lnC,6]
      ENDIF
    ENDFOR
    DIMENSION laRelFld[1]
    =ACOPY(laTmpRel,laRelFld)
    RELEASE laTmpRel
  ENDIF
  *B602089,1 AMM end
  
  FOR lnArayCont = 1 TO ALEN(laRelFld , 1)
    *B603359,1 AMM Adjust in case of date
    *laRelFld[lnArayCont,6] = PADR(laRelFld[lnArayCont,6] , laRelFld[lnArayCont,4])
    IF laRelFld[lnArayCont,3] # 'D'
      laRelFld[lnArayCont,6] = PADR(laRelFld[lnArayCont,6] , laRelFld[lnArayCont,4])
    ENDIF
    *B603359,1 AMM end
    *B602089,1 AMM start, replace the values of True & False with Yes & No
    IF laRelFld[lnArayCont,3] = 'L'
      laRelFld[lnArayCont,6] = IIF(laRelFld[lnArayCont,6]='T','Y','N')
    ENDIF
    *B602089,1 AMM end

    *E300867,1 AMM set the mandatory element in the array due to the dollar sign
    lnPos = ATC(ALLTRIM(laRelFld[lnArayCont,1]),lcRltField)
    IF SUBSTR(lcRltField,lnPos-1,1) = '$'
      laRelFld[lnArayCont,13] = .T.
      IF SUBSTR(lcORltFld , lnPos+LEN(ALLTRIM(laRelFld[lnArayCont,1])) , 1) = '~'
        lnOccur = OCCUR('|',SUBSTR(lcORltFld,1,lnPos))
        lnPos1 = ATC("|",lcORltFld,lnOccur+1)
        laRelFld[lnArayCont,14] = SUBSTR(lcORltFld , lnPos+LEN(ALLTRIM(laRelFld[lnArayCont,1]))+1 , ;
              lnPos1 - (lnPos+LEN(ALLTRIM(laRelFld[lnArayCont,1])))-1   )
      ENDIF

    ENDIF
    *E300867,1 AMM end
  ENDFOR

  SELECT (lc_Tmpfl)
  IF lnCodRec > 0 .AND. lnCodRec <= RECCOUNT()
    GO lnCodRec
  ENDIF
ENDIF

SELECT SYDFIELD
lcRltdTitl = "Related fields for code " + "( " + ALLTRIM(SYDFIELD.cFld_head) + " )"
lnColPos   = IIF(LEN(lcRltdTitl) > 74,0,INT((74 - LEN(lcRltdTitl))/2))
lnCount3   = 1
lcGet_Pic  = ""
lcStat     = IIF(laScrMode[2],"DISABLE","ENABLE")
lcDefault  = ""
lnScrolRow = 2
lnObjMax   = 7
lnObjNum   = 1
lnStrtObj  = 0
lnMaxObj   = ALEN(laRelFld,1)
llFromObj  = .F.
lnRecFld   = RECNO("SYDFIELD")

SELECT SYDFIELD

FOR lnI = 1 TO ALEN(laCodInfo,1)
  lcAryName = laCodInfo[lnI,02]
  lcPopName = laCodInfo[lnI,03]
  IF !EMPTY(lcAryName)
    RELEASE &lcAryName
    RELEASE &lcPopName
  ENDIF  
ENDFOR

*B600464,1 Release browse pad upon entry.
RELEASE BAR 100 OF P01PU01
*B600464,1 end.

PUSH KEY
ON KEY
*E300683,4 Call *.SPR from screens directory
* DO SMRELFD.SPR 


*B605423,1 RAE [START]
*DO (gcScrDir + gcWinAppl + '\SMRELFD.SPR')

ON KEY LABEL ESC DO LPVCANCEL
DO (gcScrDir + gcWinAppl + '\SMRELFD.SPR')
ON KEY LABEL ESC &lcOnEscape

*B605423,1 RAE [END]

*E300683,4 end  
POP KEY

*B600464,1 Redefine browse pad, or bar.
DEFINE BAR 100 OF P01PU01 PROMPT "" KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTitl)

IF laScrMode[3]
  _CUROBJ = OBJNUM(pbInsert)
ENDIF




*!**************************************************************************
*!
*!      Procedure: lfvOk
*!
*!**************************************************************************
*
FUNCTION lfvOk

*B602089,1 AMM Add this variable (the return value from the conditional mandatory function)
llRet = .T.
*B602089,1 AMM end
IF !laScrMode[2]
  SELECT (lc_TmpFl)
  lnCodRec  = RECNO(lc_Tmpfl)
  lcCode_no = &lc_TmpFl..cCode_no
  
  *E300867,1 AMM Check if there is an empty mandatory field, if so display message 
  *E300867,1 AMM and don't exit
  FOR lnCount4 = 1 TO ALEN(laRelFld,1)
    IF laRelFld[lnCount4,13] .AND. EMPTY(laRelFld[lnCount4,6]) 
      *B602089,1 AMM Execute the conditional mandatory function
      IF !EMPTY(laRelFld[lnCount4,14])
        lcFuncLoc = gcAppHome+ALLTRIM(laRelFld[lnCount4,9])+".APP"
        lcFunct = STRTRAN(laRelFld[lnCount4,14],"()")
        *C123847,1  TMI [Start] if this is DIR03 run a trigger instead
        IF ASCAN(laEvntTrig , PADR('CHGACC',10)) <> 0
          llRet = !gfDoTriger('SMCODES',laRelFld[lnCount4,1])
        ELSE
          *C123847,1  TMI [End  ] 
          DO &lcFunct IN &lcFuncLoc WITH "llRet"
          *C123847,1  TMI [Start] 
        ENDIF
        *C123847,1  TMI [End  ] 
      ENDIF
      IF llRet
      *B602089,1 AMM end
        =gfModalGen("INM00250B00000","DIALOG",ALLTRIM(laRelFld[lnCount4,2]))
        _CUROBJ   = OBJNUM(laRelFld[lnCount4,1])
        RETURN
      *B602089,1 AMM start
      ENDIF
      *B602089,1 AMM end
    ENDIF
    llRet = .T.
  ENDFOR
  *E300867,1 AMM end
  
  FOR lnCount4 = 1 TO ALEN(laRelFld,1)
    lcTmpValue = ""
    DO CASE
      CASE laRelFld[lnCount4,11] OR !EMPTY(laRelFld[lnCount4,12]) OR ;
           laRelFld[lnCount4,3] = 'L'
        lnNo = ASCAN(laCodInfo, laRelFld[lnCount4,1]) 
        lcRAryName = laCodInfo[lnNo+1]
        lcRAryValu = laCodInfo[lnNo+2]
        lcTmpValue = &lcRAryName[&lcRAryValu,2]
        IF laRelFld[lnCount4,3] = 'L'
          lcTmpValue = IIF(lcTmpValue="Y","T","F")
        ENDIF
      CASE laRelFld[lnCount4,3] = "C"
        lcTmpValue = laRelFld[lnCount4,6]
      CASE laRelFld[lnCount4,3] = "D"
        lcTmpValue = DTOC(laRelFld[lnCount4,6])
      CASE laRelFld[lnCount4,3] = "N"
        lcTmpValue = ALLTRIM(STR(laRelFld[lnCount4,6],;
                             laRelFld[lnCount4,4],laRelFld[lnCount4,5]))
    ENDCASE
    *E300789,1 Hesham (Start)
    *LOCATE FOR cComp_ID+cCode_No+cRltField+cFld_Name = ;
               laComp[lnActComp,2]+lcCodeRltd+"Y"+laCodes[lnActCode,2] AND ;
               cRltd_nam = laRelFld[lnCount4,1]

    LOCATE FOR cdefcode+cCode_No+cRltField+cFld_Name = ;
               'N'+lcCodeRltd+"Y"+laCodes[lnActCode,2] AND ;
               cRltd_nam = laRelFld[lnCount4,1]

    *E300789,1 Hesham (End)
    
   
    IF FOUND()
      lcStatus = IIF(&lc_TmpFl..cStatus = "A","A","M")
      REPLACE cRltd_vlu WITH lcTmpValue ;
              cStatus   WITH lcStatus
    ELSE
      *E300789,1 Hesham (Start)
      *INSERT INTO (gcWorkDir+lc_Tmpfl) ;
             (ccomp_id,cfld_name,ccode_no, ;
              cdiscrep,crltfield, ;
              cRltd_Nam,cRltd_Typ,cRltd_Vlu,;
              cadd_user,dadd_date,cadd_time,cStatus) ;
      VALUES (laComp[lnComp,2],laCodes[lnActCode,2],lcCode_no, ;
              "","Y",laRelFld[lnCount4,1],;
              laRelFld[lnCount4,3],lcTmpValue, ;
              gcUser_ID,DATE(),gfGetTime(),"A")

      INSERT INTO (gcWorkDir+lc_Tmpfl) ;
             (cdefcode,cfld_name,ccode_no, ;
              cdiscrep,crltfield, ;
              cRltd_Nam,cRltd_Typ,cRltd_Vlu,;
              cadd_user,dadd_date,cadd_time,cStatus) ;
      VALUES ('N',laCodes[lnActCode,2],lcCode_no, ;
              "","Y",laRelFld[lnCount4,1],;
              laRelFld[lnCount4,3],lcTmpValue, ;
              gcUser_ID,DATE(),gfGetTime(),"A")

      *E300789,1 Hesham (End)
    ENDIF
    *C123847,4  TMI [Start] Add a trigger to update codes file with a code in "Non-merchandise charges" for each new code in "Order charges"
    IF ASCAN(laEvntTrig , PADR('MRCHDSCD',10)) <> 0 
      =gfDoTriger('SMCODES','MRCHDSCD') 
    ENDIF
    *C123847,4  TMI [End  ] 
  ENDFOR

  IF lnCodRec > 0 .AND. lnCodRec <= RECCOUNT()
    GO lnCodRec
  ENDIF
ENDIF

CLEAR READ

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
    *E300789,1 Hesham (Start)
    *laCodInfo[lnLen,09] = "laComp[lnActComp,2]+IIF(laScrMode[2],Codes.cCode_No,&lc_Tmpfl..cCode_No)+'Y'+laCodes[lnActCode,2]"
    laCodInfo[lnLen,09] = "'N'+IIF(laScrMode[2],Codes.cCode_No,&lc_Tmpfl..cCode_No)+'Y'+laCodes[lnActCode,2]"
    *E300789,1 Hesham (End)
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
    *B803923,1 (Begin) Get the defaut value.
    *lnPos    = ASCAN(&lcAryName,laRelFld[lnAryNo,6])
    IF EMPTY(ALLTRIM(laRelFld[lnAryNo,6]))
       IF !EMPTY(lcStr)
        *--Get the default value.
        lnSt  = ATC("@",lcStr)
        lnEnd = ATC("|",SUBSTR(lcStr,lnSt))
        lnEnd = IIF(lnEnd = 0,LEN(lcStr),lnEnd-1)
        lcDefVal = SUBSTR(lcStr,lnSt,lnEnd)
        laRelFld[lnAryNo,6] = lcDefVal
      ENDIF  
    ENDIF
    lnPos    = ASCAN(&lcAryName,ALLTRIM(laRelFld[lnAryNo,6]))
    *B803923,1 (End)
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
    *B603359,1 AMM Set the size of the date field object on the related field screen
    laRelFld[lnAryNo,4] = 10
    *B603359,1 AMM end
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

*C123847,1  TMI [Start] update the valid function if it is a trigger 
IF ASCAN(laEvntTrig , PADR('ADDVLDFN',10)) <> 0
  =gfDoTriger('SMCODES',PADR('ADDVLDFN',10))
ENDIF 
*C123847,1  TMI [End  ] 

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
  *B607905,1 TMI [Start] calling the gfwCodePop with third parameter is 'L' refills the array 
  *                           laCodInfo[lnVarNo-1], while it is now at least with one line, so passing the current code value
  *                           to the second element and it will used to get the correct current value   
  PRIVATE lnLine,lcArr
  lnLine = ASUBSCRIPT(laRelFld,ASCAN(laRelFld,laCodInfo[lnVarNo-2]),1)
  IF laRelFld[lnLine,11] .AND. EMPTY(laRelFld[lnLine,12])
    lcArr = laCodInfo[lnVarNo-1]
    &lcArr.[2] = laRelFld[lnLine,6]
  ENDIF
  *B607905,1 TMI [End  ] 
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
*!      FUNCTION: lfScrollUp
*!
*!**************************************************************************
*
FUNCTION lfScrollUp

lnObjNo1=lfObjPos('IBSCRUP')
*ahmed
IF !MDOWN() .OR. !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   .AND. BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
*IF !MDOWN() .OR. laScrMode[2] .OR. !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   .AND. BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))

  RETURN .F.
ENDIF

DO WHILE MDOWN() .AND. lnObjNum > 1 ;
        .AND. BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
        .AND. BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4])
  lnObjNum = lnObjNum - 1
  lnObjMax = lnObjNum + 6
  
  *E301217,1 AMM Adjust position
  *MOVE WINDOW AWDSMREL1 TO ;
  *     IIF((lnObjNum-1)*2=0,0,-((lnObjNum-1)*2-1)+(11*0.083)),0  
  MOVE WINDOW AWDSMREL1 TO ;
       IIF((lnObjNum-1)*2=0,-0.50,-(((lnObjNum+1)*2)-2.500)+(11*0.088)),0  
  *E301217,1 AMM end  
  
  =lfDiamodn()
ENDDO

_CUROBJ = lnObjNum + lnStrtObj

*!**************************************************************************
*!
*!      FUNCTION: lfScrollDn
*!
*!**************************************************************************
*
FUNCTION lfScrollDn

lnObjNo1=lfObjPos('IBSCRDN')
*ahmed
IF !MDOWN() .OR. laScrMode[2] .OR. !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   .AND. BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
*IF !MDOWN() .OR. !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   .AND. BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))

  RETURN .F.
ENDIF

DO WHILE MDOWN() .AND. lnObjNum + 6 < lnMaxObj ;
         .AND. BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
         .AND. BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4])
  
  lnObjNum = lnObjNum + 1
  lnObjMax = lnObjNum + 6
  
  *E301217,1 AMM Adjust position
  *MOVE WINDOW AWDSMREL1 TO ;
  *     IIF((lnObjNum-1)*2=0,0,-((lnObjNum-1)*2-1)+(11*0.083)),0  

  MOVE WINDOW AWDSMREL1 TO ;
       IIF((lnObjNum-1)*2=0,-0.50,-(((lnObjNum+1)*2)-2.500)+(11*0.088)),0  
  *E301217,1 AMM end

  =lfDiamodn()
ENDDO

_CUROBJ = lnObjNum + lnStrtObj

*!**************************************************************************
*!
*!      FUNCTION: lfScrollVr
*!
*!**************************************************************************
*
FUNCTION lfScrollVr

lnObjNo1 = lfObjPos("IBSCRHBAR")
IF !MDOWN() .OR. laScrMode[2] .OR. !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   .AND. BETWEEN(MCOL("AWDSMREL4"),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
  RETURN .F.
ENDIF

lnCurCol  = MROW("AWDSMREL4")
lnWCurPos = MAX(CEILING((MROW("AWDSMREL4")-laObjScrP[lnObjNo1,1])/;
                ((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/;
                ((lnMaxObj-6)))),1)

DO CASE 
  CASE IIF(_DOS , lnCurCol <> lnScrolRow , lnWCurPos <> lnObjNum)
    lnSign = IIF(lnWCurPos > lnObjNum , 1 , -1 )
    DO WHILE MDOWN()
      IF BETWEEN(MCOL("AWDSMREL4"),laObjScrP[lnObjNo1,2],;
                 laObjScrP[lnObjNo1,4]) .AND. ;
         BETWEEN(MROW("AWDSMREL4"),laObjScrP[lnObjNo1,1],;
                 laObjScrP[lnObjNo1,3])
        lnWCurPos = MAX(CEILING((MROW("AWDSMREL4")-laObjScrP[lnObjNo1,1])/;
                        ((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/;
                        ((lnMaxObj-6)))),1)
        lnSign2   = IIF(lnWCurPos > lnObjNum , 1 , -1 )
        IF lnSign = lnSign2
          lnObjNum = lnObjNum + (lnSign*;
                     IIF(ABS(lnWCurPos-lnObjNum)>1,1,ABS(lnWCurPos-lnObjNum)))
          lnObjMax = lnObjNum + 6
          *E301217,1 AMM Adjust position
          *MOVE WINDOW AWDSMREL1 TO ;
               IIF((lnObjNum-1)*2=0,0,-((lnObjNum-1)*2-1)+(11*0.083)),0
          MOVE WINDOW AWDSMREL1 TO ;
               IIF((lnObjNum-1)*2=0,-0.50,-(((lnObjNum+1)*2)-2.500)+(11*0.088)),0  
          *E301217,1 AMM end  
          =lfDiamodn()
        ENDIF  
      ENDIF  
      =INKEY(.01,'HM')
    ENDDO
  OTHERWISE
    lnTmpCol  = lnScrolROW
    lnWCurPos = MAX(CEILING((MROW("AWDSMREL4")-laObjScrP[lnObjNo1,1])/;
                    ((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/ ;
                    ((lnMaxObj-6)))),1)
    =lfVerCurs()                                
    DO WHILE MDOWN() 
      IF (BETWEEN(MROW("AWDSMREL4"),laObjScrP[lnObjNo1,1],;
          laObjScrP[lnObjNo1,3]))
        
        lnWCurPos = MAX(CEILING((MROW("AWDSMREL4")-laObjScrP[lnObjNo1,1])/;
                        ((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/;
                        ((lnMaxObj-6)))),1)
        =lfVerCurs()                                
        =INKEY(.01,'HM')         
      ENDIF  
      =INKEY(.01,'HM')     
    ENDDO    
    IF lnObjNum <> lnWcurPos
      lnObjNum   = lnWCurPos
      lnObjMax   = lnObjNum + 6
      lnScrolRow = lnTmpCol
      *E301217,1 AMM Adjust position 
      *MOVE WINDOW AWDSMREL1 TO ;
           IIF((lnObjNum-1)*2=0,0,-((lnObjNum-1)*2-1)+(11*0.083)),0

      MOVE WINDOW AWDSMREL1 TO ;
           IIF((lnObjNum-1)*2=0,-0.50,-(((lnObjNum+1)*2)-2.500)+(11*0.088)),0  
      *E301217,1 AMM end
      =lfDiamodn()    
    ENDIF      
ENDCASE   

_CUROBJ = lnObjNum + lnStrtObj

*!********************************************************************
*!
*!              Function: lfVerCurs
*!
*!********************************************************************
*
FUNCTION lfVerCurs

lnObjPos = lfObjPos("IBSCRHBAR")

lnPos      = lnScrolRow
lnScrolRow = MAX(laObjScrP[lnObjPos,1],MIN(laObjScrP[lnObjPos,3]-1.00,;
             FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/;
             ((lnMaxObj-7)-1)*(lnWCurPos-1)+laObjScrP[lnObjPos,1])))
IF lnPos <> lnScrolRow
  STORE WOUTPUT() TO currwind
  ACTIVATE WINDOW AWDSMREL4 SAME 
  IF NOT EMPTY(currwind)
    ACTIVATE WINDOW (currwind) SAME
  ENDIF
ENDIF
         
*!********************************************************************
*!
*!              Function: lfDiamodn
*!
*!********************************************************************
*
FUNCTION lfDiamodn

lnObjPos = lfObjPos("IBSCRHBAR")

lnPos      = lnScrolRow
lnScrolRow = MAX(laObjScrP[lnObjPos,1],MIN(laObjScrP[lnObjPos,3]-1.00,;
             FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/;
             ((lnMaxObj-7)-1)*(lnObjNum-1)+laObjScrP[lnObjPos,1])))
IF lnPos <> lnScrolRow
  STORE WOUTPUT() TO currwind
  ACTIVATE WINDOW AWDSMREL4 SAME
  @ lnPos,laObjScrP[lnObjPos,2] CLEAR TO ;
    lnPos+.3,laObjScrP[lnObjPos,2]+.7
      
  @ lnScrolRow,laObjScrP[lnObjPos,2] TO ;
    lnScrolRow,laObjScrP[lnObjPos,4] ;
    PEN 2, 8 STYLE "1" ;
    COLOR RGB(255,255,255,255,255,255)
      
  @ lnScrolRow+1.167,laObjScrP[lnObjPos,2] TO ;
    lnScrolRow+1.167,laObjScrP[lnObjPos,4] ;
    PEN 2, 8 STYLE "1" ;
    COLOR RGB(128,128,128,128,128,128)
      
  @ lnScrolRow,laObjScrP[lnObjPos,2] TO ;
    lnScrolRow+1.167,laObjScrP[lnObjPos,2] ;
    PEN 2, 8 ;
    COLOR RGB(255,255,255,255,255,255)
      
  @ lnScrolRow,laObjScrP[lnObjPos,4]-.25 TO ;
    lnScrolRow+1.167,laObjScrP[lnObjPos,4]-.25 ;
    PEN 2, 8 ;
    COLOR RGB(128,128,128,128,128,128)
      
  IF NOT EMPTY(currwind)
    ACTIVATE WINDOW (currwind) SAME 
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      FUNCTION: lfWhen
*!
*!**************************************************************************
*
FUNCTION lfWhen

IF BETWEEN( _CUROBJ - lnStrtObj , lnObjNum , lnObjMax)
  llFromObj = .F.
ELSE
  IF INLIST(LASTKEY(),5,4,19,24) .AND. !llFromObj
      DO CASE
      CASE lnObjMax < _CUROBJ - lnStrtObj
        lnObjMax = _CUROBJ  - lnStrtObj
        lnObjNum = lnObjMax - 6
        MOVE WINDOW AWDSMREL1 TO ;
             IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1)),0
        =lfDiamodn()
      CASE lnObjNum > _CUROBJ - lnStrtObj
        lnObjNum = _CUROBJ  - lnStrtObj
        lnObjMax = lnObjNum + 6
        MOVE WINDOW AWDSMREL1 TO ;
             IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1)),0
        =lfDiamodn()
    ENDCASE
  ELSE
    RETURN .F.
  ENDIF
ENDIF

*!**************************************************************************
*!
*!      FUNCTION: lfPageUp
*!
*!**************************************************************************
*
FUNCTION lfPageUp

IF laScrMode[2]
  RETURN .F.
ENDIF
IF LASTKEY() = 18 
  lnOldObjN = lnObjNum
  lnObjNum  = IIF((lnObjNum-6) <= 1 , 1 , lnObjNum-6)
  IF lnObjNum <> lnOldObjN
    MOVE WINDOW AWDSMREL1 TO ;
         IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1)),0  
    lnObjMax = lnObjNum + 6
    =lfDiamodn()   
    _CUROBJ = lnObjNum + lnStrtObj
  ENDIF
ELSE
  llFromObj = .T.    
ENDIF  

RETURN .F.

*!**************************************************************************
*!
*!      FUNCTION: lfPageDn
*!
*!**************************************************************************
*
FUNCTION lfPageDn

IF laScrMode[2]
  RETURN .F.
ENDIF
IF LASTKEY() = 3
  lnOldObjN = lnObjNum
  lnObjNum  = IIF((lnObjNum+6) >= lnMaxObj-6 , lnMaxObj-6 , lnObjNum+6)
  IF lnObjNum <> lnOldObjN
    MOVE WINDOW AWDSMREL1 TO ;
         IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1)),0
    lnObjMax = lnObjNum + 6
    =lfDiamodn()
    _CUROBJ  = lnObjNum + lnStrtObj
  ENDIF
ELSE
  llFromObj = .T.
ENDIF

RETURN .F.

*!*************************************************************************
*!
*!              Function: lfObjPos
*!
*!*************************************************************************
*
FUNCTION lfObjPos
PARAMETERS lcObj_Nam
PRIVATE lnCount

llObjFound = .F.
FOR lnCount = 1 TO ALEN(laObjScrP,1)
  IF UPPER(laObjScrP[lnCount,5]) = UPPER(lcObj_Nam)
    llObjFound = .T.
    EXIT  
  ENDIF
ENDFOR
RETURN IIF(llObjFound,lnCount,0)

*!**************************************************************************
*!
*!      Procedure: lpSavScr
*!
*!**************************************************************************
*
PROCEDURE lpSavScr

SELECT (lc_Tmpfl)
IF llIsEditable
  *E300789,1 Hesham (Start)
  *IF SEEK(laComp[lnActComp,2]+SPACE(6)+"N"+laCodes[lnActCode,2])
  IF SEEK('N'+SPACE(6)+"N"+laCodes[lnActCode,2])
  *E300789,1 Hesham (End)
    = gfModalGen("TRM00211B00000","DIALOG","code|code")
    llCSave = .F.
    KEYBOARD "{ALT+B}"
    RETURN
  ENDIF
ELSE
  LOCATE FOR EMPTY(cDiscrep) AND cRltField = "N"
  IF FOUND()
    *** You cannot save an empty description, ***
    *** You  have to  enter a description  or ***
    *** remove this code. ***
    = gfModalGen("TRM00211B00000","DIALOG","description|description")
    llCSave = .F.
    KEYBOARD "{ALT+B}"
    RETURN
  ENDIF
ENDIF

GOTO TOP
IF !EOF() AND lcDefCode = lcNoDefault
  *** A default code value should be assigned, cannot save. ***
  = gfModalGen("TRM00256B00000","DIALOG")
  llCSave = .F.
  KEYBOARD "{ALT+B}"
  RETURN
ENDIF

lcSaveDel = SET ('DELETE')
SET DELETE OFF

lcExact = SET("EXACT")
SET EXACT ON

IF !llIsEditable
  *** Get the sequence numbers for each code. ***
  FOR lnCount5 = 1 TO ALEN(laSeqNo,1)
    lcSeq     = ALLTRIM(gfSequence("cCode_No",laComp[lnActComp,2]))
    lcDefCode = IIF(lcDefCode = laSeqNo[lnCount5,1], lcSeq, lcDefCode)
    laSeqNo[lnCount5,2] = lcSeq
  ENDFOR
  
  SELECT (lc_Tmpfl)
  REPLACE ALL cCode_No WITH ;
          IIF(LEFT(&lc_Tmpfl..cCode_No,1) = "A" .AND. ;
              ASCAN(laSeqNo,&lc_Tmpfl..cCode_No) > 0 ,;
              laSeqNo[ASUBSCRIPT(laSeqNo,ASCAN(laSeqNo,&lc_Tmpfl..cCode_No),1),2],;
              &lc_Tmpfl..cCode_No)
ENDIF

SET EXACT &lcExact
lnTotRec = 0
COUNT FOR cStatus <> "S" TO lnTotRec
lnCurRec = 0
*E301058,4 AMM start, Get the installed modules in a variable.
lcInstMdl = ''
IF SEEK(laComp[lnComp,2],'SYCCOMP')
  lcInstMdl = SYCCOMP.mModlSet
ELSE
  lcInstMdl = gcComp_Mdl
ENDIF
*E301058,4 AMM end

*E301513,1 Automatic set new sites with communication information
LOCATE FOR cFld_Name = 'CSITEID' AND cStatus = 'A' AND cCode_No = lcdefcode
IF FOUND() AND 'NC' $ lcInstMdl
  lcDataDir = ALLTRIM(SycComp.cCom_DDir)
  =gfOpenFile(lcDataDir+'SETUPS','MODVAR','SH','SETUPS'+laComp[lnComp,2])
  SELECT ('SETUPS'+laComp[lnComp,2])
  =SEEK('NC'+'M_CURRSITE')
  REPLACE MData_Def WITH lcdefcode
  USE IN ('SETUPS'+laComp[lnComp,2])
ENDIF
SELECT (lc_Tmpfl)
GO TOP
LOCATE FOR cFld_Name = 'CSITEID' AND cStatus = 'A' AND cCode_No <> lcdefcode
IF FOUND() AND 'NC' $ lcInstMdl AND gfGetMemVar('M_AUTOSET',laComp[lnComp,2])='Y' 
  lcDataDir = ALLTRIM(SycComp.cCom_DDir)
  =gfOpenFile(gcSysHome+'SYCEDIPH','Partner','SH','SYCEDIPH'+laComp[lnComp,2])
  =gfOpenFile(gcSysHome+'SYCEDIPD','Trans','SH','SYCEDIPD'+laComp[lnComp,2])
  =gfOpenFile(gcSysHome+'SYCEDITR','Codetype','SH','SYCEDITR'+laComp[lnComp,2])
  =gfOpenFile(lcDataDir+'CUSTOMER','CUSTOMER','SH','CUSTOMER'+laComp[lnComp,2])
  =gfOpenFile(lcDataDir+'EDIPH','Partner','SH','EDIPH'+laComp[lnComp,2])
  =gfOpenFile(lcDataDir+'EDIPD','Parttrans','SH','EDIPD'+laComp[lnComp,2])
  =gfOpenFile(lcDataDir+'EDIACPRT','Partner','SH','EDIACPRT'+laComp[lnComp,2])
  =gfOpenFile(lcDataDir+'EDINET','Networkid','SH','EDINET'+laComp[lnComp,2])

  *E301513,1 Get Default Class, Payment terms, Ship Via, Special Instruction
  lcCodeOrd = ORDER('CODES')
  SET ORDER TO TAG Idrltfname IN CODES
  =SEEK('DNCLASS','CODES')
  lcClass = Codes.cCode_No
  =SEEK('DNCTERMCODE','CODES')
  lcTermCode = Codes.cCode_No
  =SEEK('DNSHIPVIA','CODES')
  lcShipVia = Codes.cCode_No
  =SEEK('DNSPCINST','CODES')
  lcSpcInst = Codes.cCode_No
  =SEEK('DNREGION','CODES')
  lcRegion = Codes.cCode_No
  
  SET ORDER TO TAG &lcCodeOrd IN CODES
  SELECT (lc_Tmpfl)
  GO TOP
  DO WHILE !EOF()
    lcSiteId   = ccode_no
    lcSiteName = cDiscRep
    STORE '' TO lcAccount,lcPHoneNo,lcEMail
    llFound = .F.
    SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = 'N'+lcSiteId ;
              FOR   cCode_No <> lcdefcode AND cStatus = 'A'
      llFound = .T.
      IF cRltd_Nam = 'ACCOUNT'
        lcAccount = PADR(cRltd_Vlu,5)
      ENDIF  
      IF cRltd_Nam = 'PHONE'
        lcPHoneNo = PADR(cRltd_Vlu,15)
      ENDIF  
      IF cRltd_Nam = 'CEMAIL_ADD'
        lcEMail = ALLTRIM(cRltd_Vlu)
      ENDIF  
    ENDSCAN
    IF llFound
      *E301513,1 Add new customer
      IF !EMPTY(lcAccount) AND !SEEK('M'+lcAccount,'CUSTOMER'+laComp[lnComp,2])
        INSERT INTO ('CUSTOMER'+laComp[lnComp,2]) ;
        (TYPE,ACCOUNT,BillTo,Status,StName,btName,Link_Code,Consol,Priority,Region,Class,;
         cTermCode,ShipVia,Spcinst,cInSur,PriceLvl,SkuTmpl,Prnt_Statm,cTaxRule,cCont_Code,;
         cCont_Cod2,cAddress6,cAddress62) VALUES ;
        ('M',lcAccount,'M','A',lcSiteName,lcSiteName,'DEFDEF','N','5',lcRegion,lcClass,;
         lcTermCode,lcShipVia,lcSpcInst,'Y','A','DEF','Y',' 1',SycComp.cCont_Code,SycComp.cCont_Code,SycComp.cCont_Code,SycComp.cCont_Code)
      ENDIF
      *E301513,1 Add new trading partner header record
      IF !SEEK(lcSiteId,'SYCEDIPH'+laComp[lnComp,2])
        INSERT INTO ('SYCEDIPH'+laComp[lnComp,2]) ;
        (cPartCOde,cPartName,cIntChgVer,cVersion,cFieldSep,cLineSep,cNetWork,ccrtntype) VALUES ;
        (lcSiteId,lcSiteName,'00401','004010VICS',"'*'","''",lcSiteId,'P')
      ENDIF
      IF !SEEK(lcSiteId,'EDIPH'+laComp[lnComp,2])
        INSERT INTO ('EDIPH'+laComp[lnComp,2]) ;
        (cPartCOde,cPartName,cIntChgVer,cVersion,cFieldSep,cLineSep,cNetWork,ccrtntype) VALUES ;
        (lcSiteId,lcSiteName,'00401','004010VICS',"'*'","''",lcSiteId,'P')
      ENDIF
      IF !SEEK(lcSiteId,'EDIACPRT'+laComp[lnComp,2])
        INSERT INTO ('EDIACPRT'+laComp[lnComp,2]) ;
        (Type,cPartner,cPartCOde,cCmpIsaQl,cCmpIsaId,cCmpGsId,lInterComp,cSiteId,cmtchpoprc) VALUES ;
        ('A',lcAccount,lcSiteId,'12',SycComp.cCom_Phon,SycComp.cCom_Phon,.T.,lcSiteId,'T')
      ENDIF
      *E301513,1 Add new net work record
      IF !SEEK(lcSiteId,'EDINET'+laComp[lnComp,2])
        INSERT INTO ('EDINET'+laComp[lnComp,2]) ;
        (cNetWork,cNetName,cOutFile,cInFile,lSendAck,lSendByEm) VALUES ;
        (lcSiteId,lcSiteName,ALLTRIM(lcSiteId)+'OU.EDI',ALLTRIM(lcSiteId)+'IN.EDI',.T.,.T.)
      ENDIF
      SELECT ('SYCEDITR'+laComp[lnComp,2])
      SCAN FOR ('NC' $ cBarModule)
        m.cEdiTrnTyp = cEdiTrnTyp
        IF !SEEK(lcSiteId+m.cEdiTrnTyp,'SYCEDIPD'+laComp[lnComp,2])
          INSERT INTO ('SYCEDIPD'+laComp[lnComp,2]) ;
          (cPartCode,cEdiTrnTyp,cVersion,ctranactv,cPartQual,cpartid,cPartGsId,cMapSet) VALUES ;
          (lcSiteId,m.cEdiTrnTyp,'004010','P','12',lcPHoneNo,lcPHoneNo,'POS')
        ENDIF
        IF !SEEK(lcSiteId+m.cEdiTrnTyp,'EDIPD'+laComp[lnComp,2])
          INSERT INTO ('EDIPD'+laComp[lnComp,2]) ;
          (cPartCode,cEdiTrnTyp,cVersion,ctranactv,cPartQual,cpartid,cPartGsId,cMapSet,lTrade) VALUES ;
          (lcSiteId,m.cEdiTrnTyp,'004010','P','12',lcPHoneNo,lcPHoneNo,'POS',.T.)
        ENDIF
      ENDSCAN
    ENDIF
    SELECT (lc_Tmpfl)
  ENDDO
  USE IN 'SYCEDIPH'+laComp[lnComp,2]
  USE IN 'SYCEDIPD'+laComp[lnComp,2]
  USE IN 'SYCEDITR'+laComp[lnComp,2]
  USE IN 'CUSTOMER'+laComp[lnComp,2]
  USE IN 'EDIPH'+laComp[lnComp,2]
  USE IN 'EDIPD'+laComp[lnComp,2]
  USE IN 'EDIACPRT'+laComp[lnComp,2]
  USE IN 'EDINET'+laComp[lnComp,2]
  SELECT (lc_Tmpfl)
ENDIF
*E301513,1 (End)

SCAN FOR cStatus <> 'S'
  lnCurRec = lnCurRec + 1
  =gfThermo(lnTotRec,lnCurRec,"Saving code "+&lc_TmpFl..cFld_name," ")
  
  DO CASE 
    CASE cStatus = 'A'                    && New added record   
      REPLACE cStatus  WITH 'S'
      SCATTER MEMVAR MEMO
      INSERT INTO CODES FROM MEMVAR
      *E300872,5 AMM start, Update the cmtrace file.
      *E301058,4 AMM Pass two new parameters for the data directory and insalled modules
      *=gfTraceKey('CODES',m.ccomp_id+m.cfld_name+m.ccode_no+m.cdiscrep+;
                          m.crltd_nam,'A')
      *E300789,1 Hesham (Start)
      *=gfTraceKey('CODES',m.ccomp_id+m.cfld_name+m.ccode_no+m.cdiscrep+;
                     m.crltd_nam,'A',ALLTRIM(laComp[lnActComp,3]),lcInstMdl)
      =gfTraceKey('CODES',m.cdefcode+m.cfld_name+m.ccode_no+m.cdiscrep+;
                     m.crltd_nam,'A',ALLTRIM(laComp[lnActComp,3]),lcInstMdl)

      *E300789,1 Hesham (End)
      *E301058,4 AMM end
      
      *E300872,5 AMM end
      *C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
      IF ASCAN(laEvntTrig , PADR('CODES_A',10)) <> 0
        =gfDoTriger('SMCODES',PADR('CODES_A',10))
      ENDIF
      *C200597,1  TMI [End  ] 
    CASE cStatus = 'M'                    && Record was modified
      SCATTER MEMVAR MEMO
      REPLACE cStatus  WITH  'S'  
      SELECT  CODES
      GO &lc_TmpFl..nRecNo
      GATHER  MEMVAR MEMO
      *E300872,5 AMM start, Update cmTrace file
      *E301058,4 AMM Pass two new parameters for the data directory and insalled modules
      *=gfTraceKey('CODES',m.ccomp_id+m.cfld_name+m.ccode_no+m.cdiscrep+;
                          m.crltd_nam,'M')
      *E300789,1 Hesham (Start)
      *=gfTraceKey('CODES',m.ccomp_id+m.cfld_name+m.ccode_no+m.cdiscrep+;
                     m.crltd_nam,'M',ALLTRIM(laComp[lnActComp,3]),lcInstMdl)
      =gfTraceKey('CODES',m.cdefcode+m.cfld_name+m.ccode_no+m.cdiscrep+;
                     m.crltd_nam,'M',ALLTRIM(laComp[lnActComp,3]),lcInstMdl)

      *E300789,1 Hesham (End)
      *E301058,4 AMM end
      *E300872,5 AMM end
      *C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
      IF ASCAN(laEvntTrig , PADR('CODES_S',10)) <> 0
        =gfDoTriger('SMCODES',PADR('CODES_S',10))
      ENDIF
      *C200597,1  TMI [End  ] 
    CASE cStatus = 'R'                    && Record was recalled
      REPLACE cStatus  WITH  'S'  
      SELECT  CODES
      GO &lc_TmpFl..nRecNo
      RECALL
      REPLACE cdiscrep WITH &lc_TmpFl..cdiscrep
      =gfAdd_Info()
      *E300872,5 AMM start, Update the cmTrace file
      *E301058,4 AMM Pass two new parameters for the data directory and insalled modules
      *=gfTraceKey('CODES',ccomp_id+cfld_name+ccode_no+cdiscrep+;
                          crltd_nam,'A')
      *E300789,1 Hesham (Start)
      *=gfTraceKey('CODES',ccomp_id+cfld_name+ccode_no+cdiscrep+;
                      crltd_nam,'A',ALLTRIM(laComp[lnActComp,3]),lcInstMdl)
      =gfTraceKey('CODES',cdefcode+cfld_name+ccode_no+cdiscrep+;
                      crltd_nam,'A',ALLTRIM(laComp[lnActComp,3]),lcInstMdl)

      *E300789,1 Hesham (End)
      *E301058,4 AMM end

      *E300872,5 AMM end
      *C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
      IF ASCAN(laEvntTrig , PADR('CODES_R',10)) <> 0
        =gfDoTriger('SMCODES',PADR('CODES_R',10))
      ENDIF
      *C200597,1  TMI [End  ] 
    CASE cStatus = "D" .AND. DELETED()    && Delete the deleted records
      REPLACE cStatus  WITH  'S'  
      SELECT CODES
      GO &lc_TmpFl..nRecNo
      =gfAdd_Info()
      *E300872,5 AMM start, Update the cmTrace file
      *E301058,4 AMM Pass two new parameters for the data directory and insalled modules
      *=gfTraceKey('CODES',ccomp_id+cfld_name+ccode_no+cdiscrep+crltd_nam,'D')
      *E300789,1 Hesham (Start)
      *=gfTraceKey('CODES',ccomp_id+cfld_name+ccode_no+cdiscrep+crltd_nam,'D',;
                    ALLTRIM(laComp[lnActComp,3]),lcInstMdl)
      =gfTraceKey('CODES',cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam,'D',;
                    ALLTRIM(laComp[lnActComp,3]),lcInstMdl)
      
      *E300789,1 Hesham (End)
      *E301058,4 AMM end

      *E300872,5 AMM end
      *C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
      IF ASCAN(laEvntTrig , PADR('CODES_D',10)) <> 0
        =gfDoTriger('SMCODES',PADR('CODES_D',10))
      ENDIF
      *C200597,1  TMI [End  ] 
      DELETE
  ENDCASE
  SELECT (lc_TmpFl)
ENDSCAN  

IF lnTotRec > lnCurRec
  FOR lnThCount = lnCurRec TO lnTotRec
    =gfThermo(lnTotRec,lnThCount,"Saving code "+&lc_TmpFl..cFld_name," ")
  ENDFOR
ENDIF

SET DELETE &lcSaveDel
SELECT (lc_TmpRcl)
ZAP
SELECT Codes
SET ORDER TO TAG IDRltFName
*E300789,1 Hesham (Start)
*llFound = SEEK(SPACE(2)+"N"+laCodes[lnActCode,2])
llFound = SEEK("DN"+laCodes[lnActCode,2])
*E300789,1 Hesham (End)
IF lcDefCode = lcNoDefault
  IF llFound
    *E300872,5 AMM start, Update the cmTrace file
    *E301058,4 AMM Pass two new parameters for the data directory and insalled modules
    *=gfTraceKey('CODES',SPACE(2)+cfld_name,'D')    
    *E300789,1 Hesham (Start)
    *=gfTraceKey('CODES',SPACE(2)+cfld_name,'D',ALLTRIM(laComp[lnActComp,3]),lcInstMdl)
    =gfTraceKey('CODES','D'+cfld_name,'D',ALLTRIM(laComp[lnActComp,3]),lcInstMdl)
    *E300789,1 Hesham (End)
    *E301058,4 AMM end
    *E300872,5 AMM end
    *C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
    IF ASCAN(laEvntTrig , PADR('CODES_D2',10)) <> 0
      =gfDoTriger('SMCODES',PADR('CODES_D2',10))
    ENDIF
    *C200597,1  TMI [End  ] 
    DELETE
  ENDIF
ELSE
  IF !llFound
    APPEND BLANK
    REPLACE cFld_Name WITH laCodes[lnActCode,2] ,;
            cRltField WITH "N"
  ENDIF  
  *E300872,5 AMM Get expression.
  lcExp = cCode_No+cDiscrep
  *E300872,5 AMM end
  
*E300789,1 Hesham (Start)

  *REPLACE cCode_No WITH lcDefCode ,;
          cDiscrep WITH lcDefDesc,;

  REPLACE cCode_No WITH lcDefCode ,;
          cDiscrep WITH lcDefDesc,;
          cdefcode with 'D'

*E300789,1 Hesham (eND)  

  *E300872,5 AMM start, Update the cmTrace file
  IF lcExp # lcDefCode+lcDefDesc
    *E301058,4 AMM Pass two new parameters for the data directory and insalled modules
    *=gfTraceKey('CODES',SPACE(2)+cfld_name+ccode_no+cdiscrep,IIF(llFound,'M','A'))
    *E300789,1 Hesham (Start)
    *=gfTraceKey('CODES',SPACE(2)+cfld_name+ccode_no+cdiscrep,IIF(llFound,'M',;
                  'A'),ALLTRIM(laComp[lnActComp,3]),lcInstMdl)
    =gfTraceKey('CODES','D'+cfld_name+ccode_no+cdiscrep,IIF(llFound,'M',;
                  'A'),ALLTRIM(laComp[lnActComp,3]),lcInstMdl)
    
    *E300789,1 Hesham (End)
    *E301058,4 AMM end
    *C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
    IF ASCAN(laEvntTrig , PADR('CODES_M2',10)) <> 0
      IF llFound
        =gfDoTriger('SMCODES',PADR('CODES_M2',10))
      ELSE
        =gfDoTriger('SMCODES',PADR('CODES_A2',10))
      ENDIF
    ENDIF
    *C200597,1  TMI [End  ] 

  ENDIF
  *E300872,5 AMM end
ENDIF
SET ORDER TO TAG Codes

*B123486,1 NNA 08/05/2004 (Begin) Remove Records that doesn't have a cdefcode
DELETE ALL FOR EMPTY(cDefCode)
*B123486,1 NNA (END)

SELECT sydField
laScrMode    = .F.
laScrMode[2] = .T.

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
*B600464,1 New funtion.
*B600464,1 READ ACTIVATE CLAUSE function for SMFLCD.SPR
FUNCTION lfActFlBrw
*B600464,1 Set key Alt+B to activate the browse to be used
*B600464,1 under windows
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
*B600464,1 New funtion.
*B600464,1 READ DEACTIVATE CLAUSE function for SMFLCD.SPR
FUNCTION lfDctFlBrw
*B600464,1 Release browse pad upon entry
IF WONTOP() = lcFileTitl
 *RELEASE BAR 100 OF P01PU01
 
  *B600464,1 Add the following key traps as well
  ON KEY LABEL Ctrl+ENTER DO lpSelObj WITH 'AWDSMFLCD2', OBJNUM(pbOK)
  ON KEY LABEL ESC        DO lpSelObj WITH 'AWDSMFLCD2', OBJNUM(pbOK)
  ON KEY LABEL ALT+O      DO lpSelObj WITH 'AWDSMFLCD2', OBJNUM(pbOK)
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  RETURN .F.
ENDIF
*B600464,1 end.


*!**************************************************************************
*!
*!      Function: lfwBrWhen
*!
*!**************************************************************************
*
*B600464,1 BROWSE COMMAND When function, called from lfBrowse()
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
  USE (gcSysDir + "SYDField") IN 0
  lcOrder1  = SPACE(0)
  llField = .T.
ENDIF

IF USED("SYDFlFld")
  SELECT SYDFlFld
  lcOrder = TAG()
  SET ORDER TO 
  llFlFld = .F.
ELSE
  USE (gcSysDir + "SYDFlFld") IN 0
  lcOrder  = SPACE(0)
  llFlFld = .T.
ENDIF

SELECT DIST SYDFlFld.cFld_Name, SYDField.cData_Typ, SYDField.nFld_Wdth, SYDField.nFld_dec, SYDFlFld.nFld_Pos ;
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

lcCCode = &lc_Tmpfl..cCode_No
IF EMPTY(lcCCode)
  RETURN .F.
ELSE
  lnRecNo = RECNO()
  *E300789,1 Hesham (Start)
  *IF SEEK(laComp[lnActComp,2]+lcCCode+"N"+laCodes[lnActCode,2], lc_Tmpfl) AND RECNO() <> lnRecNo
  IF SEEK('N'+lcCCode+"N"+laCodes[lnActCode,2], lc_Tmpfl) AND RECNO() <> lnRecNo
  *E300789,1 Hesham (End)
    GOTO lnRecNo
    REPLACE cCode_No WITH SPACE(6)
    SHOW WINDOW (lcBrowTitl) REFRESH
    RETURN .F.
  ENDIF
  GOTO lnRecNo
ENDIF
REPLACE cdiscrep WITH STRTRAN(cdiscrep, CHR(255), "")

*!*****************************************************************
*!
*!      Function: lfwBCode
*!
*!*****************************************************************
*
FUNCTION lfwBCode

RETURN EMPTY(&lc_Tmpfl..cCode_No)

*!*****************************************************************
*!
*!      Function: lfvDefa
*!
*!*****************************************************************
*
FUNCTION lfvDefa

lcDefCode = IIF(llDefCode, &lc_Tmpfl..cCode_No, lcNoDefault)
lcDefDesc = IIF(llDefCode, &lc_Tmpfl..cDiscrep, lcDefDesc)
llNoThing = gfUpdate()

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
*E300697,4 Declare private variable llApInst  
*PRIVATE lcRet, llOpen, lcTag, lnPos, lnAlias
PRIVATE lcRet, llOpen, lcTag, lnPos, lnAlias, llApInst
*E300697,4 end 

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
    *E300697,4 llApInst  is set if an A/P is linked with the current company
    llApInst  = 'AP' $ UPPER(SYCCOMP.mModlSet)
    *E300697,4 end
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
        *E302472,1 MHM 11/18/2007  Multiple Taxes for Canada on Invoice [T20060709.0008] [Start]
        *lcRet = "NTAXRATE" + IIF(lcCountry = "CANADA", "|CTAXRULE", "")
        lcRet = "NTAXRATE" + IIF(lcCountry = "CANADA", "|CTAXRULE|LHSTTAX", "")
        *E302472,1 MHM 11/18/2007  Multiple Taxes for Canada on Invoice [T20060709.0008] [Start]

      *E500308,1 Move SHIPVIA to the OTHERWISE so as to get related fields
      *E500308,1 from dictionary
      *CASE lcCodField = "SHIPVIA"
        *lcRet = "CUPS" + IIF(lcCountry = "ENG", "", "|NCODCHARGE|NINSCHARGE|NFXDPRCNT")
      *E500308,1 end
      
      *E300697,4 Instead of assigning related fields to the following codes,
      *E300697,4 we will selectively remove some codes, in the OTHERWISE below
      *E300697,4 hence, comment out the following piece of code
	  *CASE lcCodField = "DIVISION"
	  *  lcRet = "DIVLNAME|DIVGROUP|CUPCMAN" + IIF(llGlLink,"|LINK_CODE","")
      *	
	  *CASE lcCodField = "TRANCODE"
	  *  lcRet = "ALLOW_TYPE|CADJACCT" + IIF(llGlLink,"|LINK_CODE","")
	  *       
	  *CASE lcCodField = "CCREDITCOD"
	  *  lcRet = "ALLOW_TYPE|CADJACCT" + IIF(llGlLink,"|LINK_CODE","")
	  *E300697,4 end             
      
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
          *E300697,4 If related fields are found, 
          IF !EMPTY(lcRet) 
            DO CASE
            
              *E300697,4 Case code = 'CDIVISION'
              CASE lcCodField = "CDIVISION" 
               
                *E300697,4 remove link code if there is no g/l link
                *E300867,1 In case of division code, check if not link code at 
                *E300867,1 division level, don't display the link codeor sales 
                *E300867,1 link code.
                *lcRet = IIF(llGlLink, lcRet, STRTRAN(lcRet, "|LINK_CODE"))
                IF !(llGlLink .AND. gfGetMemVar("M_DIV_LINK", lcActComp)='Y')
                  lnPos = ATC('LINK_CODE',lcRet)
                  IF lnPos > 0
                    *E300867,1 check if the field is mandatory, to exactly erasethe field.
                    lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1)
                    lcRet = STUFF(lcRet,lnPos-lnMand,LEN('LINK_CODE')+lnMand,'')
                  ENDIF
                  lnPos = ATC('CSLSGLLINK',lcRet)
                  IF lnPos > 0
                    *E300867,1 check if the field is mandatory, to exactly erasethe field.
                    lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1)
                    *E300867,1 remove the related field from the related string.
                    lcRet = STUFF(lcRet,lnPos-lnMand,LEN('CSLSGLLINK')+lnMand,'')
                  ENDIF
                ENDIF
                
                *E300867,1 end
                *B602941,1 Get M_DIV_SEQ company setting. 
                IF gfGetMemVar("M_DIV_SEQ", laComp[lnComp,2]) # 'Y'
                  lnPos = ATC('DIVGROUP',lcRet)
                  IF lnPos > 0
                    *E300867,1 check if the field is mandatory, to exactly erasethe field.
                    lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1)
                    lcRet = STUFF(lcRet,lnPos-lnMand,LEN('DIVGROUP')+lnMand,'')
                  ENDIF
                ENDIF
                *B602941,1 end
                *B603468,1 NAD (Start) Display upc type , ucc manufacturer id only at the Division level  
                IF gfGetMemVar("M_UCCDIV", laComp[lnComp,2]) = 'Y' OR ;
                   gfGetMemVar("M_UPC_USE", laComp[lnComp,2]) <> 'Y'
                  lnPos = ATC('CUPCMAN',lcRet)
                  IF lnPos>0
                    lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1) 
                    lcRet = STUFF(lcRet,lnPos-lnMand,LEN('CUPCMAN')+lnMand,'')
                  ENDIF
                  lnPos = ATC('CUPCGENTYP',lcRet)
                  IF lnPos>0
                    *lcRet = STUFF(lcRet,lnPos,LEN('CUPCGENTYP'),'')
                    lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1) 
                    lcRet = STUFF(lcRet,lnPos-lnMand,LEN('CUPCGENTYP')+lnMand,'')
                  ENDIF  
                ENDIF   
                *B603468,1 NAD (End)
              *E300697,4 Case code is debit or credit adjustment
			  CASE INLIST(lcCodField, "TRANCODE", "CCREDITCOD")
 			    *E300930,1 AMM start, Don't hide the adjustment accout.
 			    *IF llApInst
                  *lnPos = ATC('CADJACCT',lcRet)
                  *IF lnPos > 0
                    *E300867,1 check if the field is mandatory, to exactly erasethe field.
                    *lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1)
                    *E300867,1 remove the related field from the related string.
                    *lcRet = STUFF(lcRet,lnPos-lnMand,LEN('CADJACCT')+lnMand,'')
                  *ENDIF
                *ELSE
                IF !llApInst
                *E300930,1 AMM end
                  lnPos = ATC('CBNKCODE',lcRet)
                  IF lnPos > 0
                    *E300867,1 check if the field is mandatory, to exactly erasethe field.
                    lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1)
                    *E300867,1 remove the related field from the related string.
                    lcRet = STUFF(lcRet,lnPos-lnMand,LEN('CBNKCODE')+lnMand,'')
                  ENDIF
                  lnPos = ATC('CCHKACCT',lcRet)
                  IF lnPos > 0 
                    *E300867,1 check if the field is mandatory, to exactly erasethe field.
                    lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1)
                    *E300867,1 remove the related field from the related string.
                    lcRet = STUFF(lcRet,lnPos-lnMand,LEN('CCHKACCT')+lnMand,'')
                  ENDIF
                ENDIF
 			  *E300867,1 end
	 		  *E500308,1 If country is England, remove NCODCHARGE,
	 		  *E500308,1 NINSCHARGE, NFXDPRCNT from related field
              CASE lcCodField = "SHIPVIA" .AND. lcCountry = "ENG"
	 		     *ahmed remove the fields : NCODCHARGE, NINSCHARGE, NFXDPRCNT if country is england
                 lnPos = ATC('NCODCHARGE',lcRet)
                 IF lnPos > 0
                   *ahmed check if the field is mandatory, to exactly erase the field.
                   lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1)
                   *ahmed remove the related field from the related string.
                   lcRet = STUFF(lcRet,lnPos-lnMand,LEN('NCODCHARGE')+lnMand,'')
                 ENDIF
                 lnPos = ATC('NINSCHARGE',lcRet)
                 IF lnPos > 0
                   lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1)
                   lcRet = STUFF(lcRet,lnPos-lnMand,LEN('NINSCHARGE')+lnMand,'')
                 ENDIF
                 lnPos = ATC('NFXDPRCNT',lcRet)
                 IF lnPos > 0
                   lnMand = IIF(SUBSTR(lcRet,lnPos-1,1)='$',2,1)
                   lcRet = STUFF(lcRet,lnPos-lnMand,LEN('NFXDPRCNT')+lnMand,'')
                 ENDIF
	 		   *E500308,1 end
	 		
	 		ENDCASE
          ENDIF && !EMPTY(lcRet) 
 		  *E300697,4 end	          
          
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
    *E300789,1 Hesham (Start)  
    *llRet  = SEEK(lcActComp+laCodeAray[lnCode,1])
    llRet  = SEEK('N'+laCodeAray[lnCode,1])
    *E300789,1 Hesham (End)
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
*E300789,1 Hesham (Start)
*IF !EMPTY(lcCode) .AND. SEEK(laComp[lnActComp,2]+PADR(lcCode,6)+"N"+laCodes[lnActCode,2], lc_Tmpfl)
IF !EMPTY(lcCode) .AND. SEEK('N'+PADR(lcCode,6)+"N"+laCodes[lnActCode,2], lc_Tmpfl)
*E300789,1 Hesham (End)
  *B602524,1 AMM Adjust code width
  *lcCode = SPACE(6)
  *ahmed
  *lcCode = SPACE(lnCodeWdth)
  *B602524,1 AMM end
  *** This field already exists. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00053B00000","DIALOG")
  *RETURN .F.
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
  *E300789,1 Hesham (Start)
  *IF laRelFld[lnCount,13] .AND. SEEK(laComp[lnActComp,2]+lcCodeRltd+"Y"+laCodes[lnActCode,2])
  IF laRelFld[lnCount,13] .AND. SEEK('N'+lcCodeRltd+"Y"+laCodes[lnActCode,2])
  *E300789,1 Hesham (End)
    LOCATE REST FOR cRltd_nam = laRelFld[lnCount,1]
    IF !(FOUND() .AND. !EMPTY(cRltd_vlu))
    
      IF gfModalGen("QRM00301B00007","DIALOG",ALLTRIM(laRelFld[lnCount,2]))=2
       RETURN
      ELSE
        *E300789,1 Hesham (Start)
        *=SEEK(laComp[lnActComp,2]+lcCodeRltd+"N"+laCodes[lnActCode,2])
        =SEEK('N'+lcCodeRltd+"N"+laCodes[lnActCode,2])
        *E300789,1 Hesham (End)
        lcCode    = CCODE_NO
        *-- replace status not to save in the master files
        *DELETE
        REPLACE ALL CSTATUS WITH 'S' FOR CCODE_NO = lcCode
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
  *B802096,1 AMM 05/27/99 (begin) search for the exact description entered.
  *LOCATE FOR ALLTRIM(UPPER(cDiscrep)) = ALLTRIM(UPPER(lcDisc)) 
  LOCATE FOR UPPER(cDiscrep) = LEFT(UPPER(lcDisc),30)
  *B802096,1 AMM end
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
*E300789,1 Hesham (Start)
*IF SEEK(SPACE(2)+"N"+laCodes[lnActCode,2],"CODES")
IF SEEK('D'+"N"+laCodes[lnActCode,2],"CODES")
*E300789,1 Hesham (End)
  lcDefCode = Codes.cCode_No
  lcDefDesc = Codes.cDiscrep
ELSE
  lcDefCode = lcNoDefault
  lcDefDesc = SPACE(0)
ENDIF
*B602941,1 Go to first record in the chosen code
=SEEK('N'+'N'+laCodes[lnActCode,2],"CODES")
*B602941,1 end

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
  *B602941,1 Refresh the browse and the "default" check box
  ACTIVATE WINDOW (lcBrowTitl)
  =lfwBrWhen()
  *B602941,1 end
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



FUNCTION lfvInsOK

IF llIsEditable 
  IF !EMPTY(lcCode) .AND. SEEK('N'+PADR(lcCode,6)+"N"+laCodes[lnActCode,2], lc_Tmpfl)
    lcCode = SPACE(lnCodeWdth)
    *** This field already exists. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00053B00000","DIALOG")
    RETURN .F.
  ENDIF
ELSE
  lnAlias = SELECT(0)
  IF !EMPTY(lcDisc)
    SELECT (lc_Tmpfl)
    lnCodRec = RECNO(lc_Tmpfl)
    *B802096,1 AMM 05/27/99 (begin) search for the exact description entered.
    LOCATE FOR UPPER(cDiscrep) = LEFT(UPPER(lcDisc),30)
    *B802096,1 AMM end
    IF FOUND()
      *** This field already exists. ***
      *** <  Ok  > ***
      =gfModalGen("TRM00053B00000","DIALOG")
      SELECT (lnAlias)
      RETURN .F.
    ENDIF
    SELECT (lnAlias)
  ENDIF
ENDIF
CLEAR READ



*!*************************************************************
*! Name      : lfXChng
*! Developer : Ahmed Mohamed El_Anwar
*! Date      : 05/22/2000
*! Purpose   : Exchange two Rows of an array
*! REF       : *B802969,1 AME
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : None
*!*************************************************************
*! Parameters: lcArray, lnBeg , lnEnd
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfxChng(@array,row1,row2)
*!*************************************************************

*B802969,1 

FUNCTION lfXchng
PARAMETERS lcArray, lnBeg, lnEnd
PRIVATE DIMENSION LAtEMP(1)
=ACOPY(LCARRAY,LATEMP,1+(LNBEG-1)*ALEN(LCARRAY,2),ALEN(LCARRAY,2))
=ACOPY(LCARRAY,LCARRAY,1+(LNEND-1)*ALEN(LCARRAY,2),ALEN(LCARRAY,2),1+(LNBEG-1)*ALEN(LCARRAY,2))
=ACOPY(LATEMP,LCARRAY,1,ALEN(LCARRAY,2),1+(LNEND-1)*ALEN(LCARRAY,2))


*!**************************************************************************
*! Function    : lfvFindCod
*! Author      : Mohamed Atia Badran (MAB)
*! Date        : 12/20/2000
*! Purpose     : Find a valid code 
*!**************************************************************************
*E301515,1 MAB 12/20/2000 Make a Find button to search codes.
FUNCTION lfvFindCod
IF EOF(lc_Tmpfl)
  RETURN
ENDIF

PRIVATE lnActAlias , lnFindCode , laFindCode , llFindOk , lnSavedRec
DECLARE laFindCode[1]
lnActAlias = SELECT(0)

*B605929,1 Save alias at beginning of function. [Begin]
SELECT &lc_Tmpfl
*B605929,1 Save alias at beginning of function. [End]

SELECT IIF(llIsEditable,cCode_No + " - " + cDiscRep,cDiscRep) ;
       FROM (lc_Tmpfl) ;
       WHERE cDefCode = "N" AND cRltField = "N" ;
       ORDER BY cCode_No ;
       INTO ARRAY laFindCode

IF _TALLY = 0
  *= gfModalGen(.F.,.F.,.F.,.F.,"There are no codes.")
ELSE
  lnFindCode = ASCAN(laFindCode,IIF(llIsEditable,cCode_No + " - " + cDiscRep,cDiscRep))
  lnFindCode = IIF(lnFindCode=0,1,lnFindCode)
  llFindOk   = .F.
  DO (gcScrDir + gcWinAppl + '\SMFNDCOD.SPX')
  IF llFindOk
    SELECT (lc_Tmpfl)
    lnSavedRec = RECNO()
    LOCATE FOR IIF(llIsEditable,cCode_No + " - " + cDiscRep,cDiscRep) = ;
               laFindCode[lnFindCode] AND cDefCode = "N" AND cRltField = "N"
    IF FOUND()
      lnOldRecNo = RECNO()
    ELSE
      GO lnSavedRec
    ENDIF               
    SHOW WINDOW (lcBrowTitl) REFRESH

  ENDIF
ENDIF
SELECT (lnActAlias)
*-- end of lfvFindCod.

*!*************************************************************
*! Name      : LPVCANCEL
*! Developer : RAE
*! Date      : 03/22/2002
*! Purpose   : calling the Cancel function if the user press ESc
*! REF       : *B605423,1
*!*************************************************************
*! Calls     : 
*!              Procedures : None
*!              FUNCTION   : None
*!*************************************************************
*! Parameters: lcArray, lnBeg , lnEnd
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : = DO LPVCANCEL 
*!*************************************************************
PROCEDURE LPVCANCEL
*--call the validation of the Cancle buttons of the related fields screen
=lfvCancel()

*!*************************************************************
*! Name      : lfSegInfo
*! Developer : Nader Nabil (NNA)
*! Date      : 09/09/2004
*! Purpose   : Check if The item code Structure found or not.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns : False or True
*!*************************************************************
*! Example   : =lfSegInfo()
*!*************************************************************
*!038431 (Bld41)
FUNCTION lfSegInfo

lnAlias=SELECT()
llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
IF !SEEK('U1','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
  RETURN .F.
ENDIF
SELECT(lnalias)
RETURN
*-- End of function lfSegInfo.