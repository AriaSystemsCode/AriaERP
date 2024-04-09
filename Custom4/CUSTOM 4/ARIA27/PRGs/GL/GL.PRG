*:************************************************************************
*:
*: Procedure file: GL.PRG
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: General Ledger
*:         Author: 
*:      Copyright (c) 
*:  Last modified: 12/05/1993
*:
*:  Procs & Fncts: lfGetIndTag
*:               : lfAccntStr
*:               : gfMousTrap
*:               : lfVldAccnt
*:               : lfDelAcnt
*:               : gfPeriod
*:               : lfDigit
*:               : lfSamType
*:               : lfSamAcode
*:               : lfwGLActBr
*:               : lfvGLActBr
*:               
*:      Documented  /  /   at   :  
*:************************************************************************
*B600423,1 Reham On 06/12/95
*B600423,1 _ Both {SYCACCOD-SYCFISHD} was opened in the beginning of the
*B600423,1   program & their closing process was in a condition, so they
*B600423,1   were never closed.
*E300297,1 M.H 10/10/95 Add currency to the GL module.
*E300683,6 RENEE 06/04/97 - Call programs from disk instead of applications 
*E300683,6                - Add BMP variables
*E300693,1 Hesham El_Sheltawi 06/22/97
*E300693,1 add arrays and variables needed for printing the default printer
*E300693,1 for different screens
*E300692,1 ESSMAT 06/30/97. Change name and path of SYCACCOD, SYCFISHD, 
*E300692,1 					SYCFSPRD, SYCFSHLD
*E300581,1 RENEE Add program paramemters
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*E301077,72 MAB 02/28/1999 Open Files with  command and let 
*E301077,72 MAB 02/28/1999 system handle closing.
*E301077,71 WALID (WAM) 03/07/1999 OPEN AND CLOSE SYCCOMP FILE IN lfSamaCode function with gfOpenFile() . 
*E300789,6 MAB 03/06/99 Remove comp. Id from files.
*E301164,1 Hesham 03/08/99
*E301164,1 declare array to store all temprory files created by any program 
*E301176,1 HDM 03/21/1999 Define new Variables as The Notepad Controled Globally
*C101459,1 Hesham 03/24/99
*C101459,1 define array laUsrFields to hold user defined fields for the program base file
*B602835,1 Walid 04/27/1999 Fix variable lcUsPrv not found .
*E301297,1  HS  07/22/99  Implement the Object/Events triggers, Add a new
*E301297,1                function (gfDoTriger) to handle the Triggers that
*E301297,1                need to be executed when a certain event takes
*E301297,1                place from a certain screen (object).
*E301434,1 Ramy 06/28/2000 Trap the F3 key to clear the current get field
*B605916,1 ASH  04/28/2002 Missing 'Add' button when entering a wrong account if the user is 'ADMN'.
*B127253,1 MHM 04/11/2005 Fix bug of not deleting all record from GLACBALS.
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
************************************************************************************
*:Ver 2.0
*E300581,1 Add parameters variable
*PARAMETERS lcProgName,lcjumpAll
PARAMETERS lcProgName,lcPrgPram,lcjumpAll
*E300581,1 end

EXTERNAL PROCEDURE ARIA27,ARIABROW,LFFIND,LFGOTO,LFLOCATE,LFVACT_K,;
                    GETFILE,GFEHAN

*E300581,1 Fill parameter variable
lcPrgPram = IIF(TYPE('lcPrgPram')='C',lcPrgPram,'')
*E300581,1 end

lcjumpAll =IIF (TYPE('lcjumpAll')='C',lcjumpAll,'F')

*** If comming from another program calling procedure or function in gl project
*** so there is no need for defning variables and doing the following validation
IF lcjumpAll = 'F'

  #REGION 1
  *E300693,1 Hesham El_Sheltawi (Start)
  *DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
            laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
            laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
            laCtrStat[12]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2]
*E301164,1 Hesham (Start)
*E301164,1 declare array to store all temprory files created by any program 

  *DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
            laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
            laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
            laCtrStat[12]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2],;
		    laOGFxFlt[1,7],laOGVrFlt[1,7],laRepFltr[1,2]
*-HDM E301176,1 03/21/1999[Start] Changed laCtrStat[12] To laCtrStat[13]
*C101459,1 Hesham (Start)

*  DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
            laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
            laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
            laCtrStat[13]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2],;
		    laOGFxFlt[1,7],laOGVrFlt[1,7],laRepFltr[1,2],laPrgTemps[1]

  *E301297,1 Change this line to add a new row to the array (laCtrStat) for
  *          the Audit Trail button [Begin]
  *DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
  *          laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
  *          laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
  *          laCtrStat[14]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2],;
  *          laOGFxFlt[1,7],laOGVrFlt[1,7],laRepFltr[1,2],laPrgTemps[1],laUsrFields[1,7]
  DECLARE   laData[1,1]     , laDataCopy[1,1] , laFld_msg[1,2]  , laScrMode[5,1] ,;
            laKeyField[3,1] , laDefProc[10,1] , laFileName[1,2] , laField_H[1,1] ,;
            laField_N[1,1]  , laWndObj[1,3]   , laFiltrExp[1,7] , laFixFltr[1,7] ,;
            laCtrStat[15]   , laWobjects[1,3] , laArrayes[1,2]  , laSubProc[1,2] ,;
            laOGFxFlt[1,7]  , laOGVrFlt[1,7]  , laRepFltr[1,2]  , laPrgTemps[1]  ,;
            laUsrFields[1,7]
  
  *E301297,1 Change this line to add a new row to the array (laCtrStat) [End]
  
*C101459,1 Hesham (End)


*-HDM E301176,1 03/21/1999[End]

*E301164,1 Hesham (End)
  *E300693,1 Hesham El_Sheltawi (End)
            
  STORE .F. TO llAddRec     , llEditRec       , llDeleRec       , laScrMode      ,;
               llUserSet    , llUpdate        , llDoLocal       , llNoContrl     ,;
               llFromBrow   , glQuitting ,llCUpdate

  STORE  1  TO lnRecNo      , lnCurObj        , lnBar_No        , lnDataNo       ,;
               pbRight      , pbLeft          , pbButt
  
  *E300693,1 Hesham El_Sheltawi (Start)
  *STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , laSubProc       
*E301164,1 Hesham (Start)
*E301164,1 declare array to store all temprory files created by any program 
  *STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , laSubProc       ,;
               laOGFxFlt    , laOGVrFlt       , lcReportID      ,laRepFltr
*-- HDM E301176,1 03/21/1999 [Start]Define new Variables as The Notepad Controled Globally

*  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , laSubProc       ,;
               laOGFxFlt    , laOGVrFlt       , lcReportID      ,laRepFltr,laPrgTemps,lcMenUnProce

*C101459,1 Hesham (Start)

*  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , laSubProc       ,;
               laOGFxFlt    , laOGVrFlt       , lcReportID      ,laRepFltr,laPrgTemps,lcMenUnProce,;
               lcSydKey     , lcNoteType      , lcNoteKey


  STORE ''  TO laKeyField   , laFileName      , laWndObj        , laField_N      ,;
               laField_H    , laPrgNames      , laArrayes       , lcWindTitl     ,;
               lcFileName   , lcScFields      , lcFile_Ttl      , lcBrFields     ,;
               lcBaseFile   , lcWinAppl       , lc_TablNam      , lcStamp        ,;
               lcPop_Name   , laFiltrExp      , laFixFltr       , lcFileFltr     ,;
               laWobjects   , lcButtNam       , laSubProc       ,;
               laOGFxFlt    , laOGVrFlt       , lcReportID      ,laRepFltr,laPrgTemps,lcMenUnProce,;
               lcSydKey     , lcNoteType      , lcNoteKey,laUsrFields

*C101459,1 Hesham (End)


*-- HDM E301176,1 03/21/1999 [End]



*E301164,1 Hesham (End)

  *E300693,1 Hesham El_Sheltawi (End)               
               

*E300252,1
*  laCtrStat[1]  = "DISABLE"                && First button    
*  laCtrStat[2]  = "DISABLE"                && Last button
*  laCtrStat[3]  = "DISABLE"                && Next button
*  laCtrStat[4]  = "DISABLE"                && Priviious button   
*  laCtrStat[5]  = "DISABLE"                && Copy button
*  laCtrStat[6]  = "DISABLE"                && Past Button
*  laCtrStat[7]  = "DISABLE"                && Edit button 
*  laCtrStat[8]  = "DISABLE"                && Delete button
*  laCtrStat[9]  = "DISABLE"                && Select button
*  laCtrStat[10] = "ENABLE"                 && Browse button
*  laCtrStat[11] = "DISABLE"                && Save button 
*  laCtrStat[12] = "ENABLE"                 && Close/Cancel button 

  laCtrStat     = "DISABLE"                
  laCtrStat[10] = "ENABLE"                 && Browse button
  laCtrStat[12] = "ENABLE"                 && Close/Cancel

  STORE .T. TO laScrMode[1] , llglobShow      , laDefProc
 
  laFld_msg     = SPACE (51)

  lcSelCont     = SCHEME(1,6) 
  lcEnbCont     = SCHEME(1,2) 
  lcDisCont     = SCHEME(1,10) 
  lcBaseWind    = " "
  lcLoclShow    = "lpShow"
  llMultiRun    = glMultiRun

  lnDateWdth    = IIF('ON'$SET('CENT'),10,8)

  IF _WINDOWS
    *E301434,1 Ramy [start]
    *ON KEY LABEL ESC
    lcESC  = ON('KEY' , 'ESC')
    *E301434,1 Ramy [end]
    ON KEY LABEL ESC DO gfEscap
  ENDIF

  ** Check if the Account Code structure has been set yet

  *IF gcAct_appl <> "GL"
  
  *E301077,72 Comment out the following block of code and  [Begin]
  *E301077,72 open file using  gfSysOpen.
  *E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
  *IF !USED('SYCACCOD')
  *IF !USED('ACCOD')
    *E300692,1 end
    *llGlAccods = .T.
    *SELECT 0
    *USE &gcSysHome.SYCACCOD ORDER TAG COMPID
    *USE &gcDataDir.ACCOD ORDER TAG COMPID
    *E300692,1 CHANGE FILE NAME AND PATH FROM SYCACCOD TO ACCOD
    
  *E300692,1 end
  *ELSE

  *  llGlAccods = .F.
    *E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
    *SET ORDER TO TAG ACCSEGNO IN SYCACCOD 
  *  SET ORDER TO TAG ACCSEGNO IN ACCOD 
    *E300692,1 end
    
  *ENDIF   
  *E301077,72 Comment out the following block of code and  [End..]

  *-- MAB Open account code file.
  *E301077,72 This code is instead of the above commented code. [Begin]
  = gfSysOpen(gcDataDir+'ACCOD',gcDataDir+'ACCSEGNO','SH')
  *E301077,72 This code is instead of the above commented code. [End  ]


  *E301077,72 Comment out the following block of code and  [Begin]
  *E301077,72 open file using  gfSysOpen.
  *IF !USED('SYCCOMP')
  *  llSycComp  = .T.
  *  SELECT 0
  *  USE &gcSysHome.SYCCOMP ORDER TAG CCOMP_ID
  *ELSE
  *  llSycComp  = .F.
  *  SET ORDER TO TAG CCOMP_ID IN  SYCCOMP 
  *ENDIF   
  *E301077,72 Comment out the following block of code and  [End  ]

  *-- MAB Open Company file.
  *E301077,72 This code is instead of the above commented code. [Begin]
  = gfSysOpen(gcSysHome+'SYCCOMP',gcSysHome+'CCOMP_ID','SH')
  *E301077,72 This code is instead of the above commented code. [End  ]


  *E301077,72 Comment out the following block of code and  [Begin]
  *E301077,72 open file using  gfSysOpen.
  *E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
  *IF !USED('SYCFISHD')
    *llSycFishd = .T.
  *IF !USED('FISHD')
  *  llFishd = .T. 
  *E300692,1 end  
  
  *  SELECT 0
  *E300692,1 CHANGE FILE NAME AND PATH FROM SYCFISHD TO FISHD
    *USE &gcSysHome.SYCFISHD ORDER TAG COMPFYEAR
    *USE &gcDataDir.FISHD ORDER TAG COMPFYEAR
   *ELSE
    *llSycFishd = .F.  
    *SET ORDER TO TAG COMPFYEAR IN SYCFISHD    
    *llFishd = .F.  
    *SET ORDER TO TAG COMPFYEAR IN FISHD    
  *E300692,1 end
  *ENDIF   
  *ENDIF
  *E301077,72 Comment out the following block of code and  [End  ]

  *-- MAB Open Fiscal year header file.
  *E301077,72 This code is instead of the above commented code. [Begin]
  *E301077,72 Note:May we change index name.
  = gfSysOpen(gcDataDir+'FISHD',gcDataDir+'COMPFYEAR','SH')
  *E301077,72 This code is instead of the above commented code. [End  ]

  *-- MAB Evaluate variables used by gl module [Begin]
  
  IF ALLTRIM(UPPER(lcProgName)) <> "GLACCOD";
    .AND. ALLTRIM(UPPER(lcProgName)) <> "GLSETUP"

    
    *E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
    *SELECT SYCACCOD
    SELECT ACCOD
    *E300692,1 end
    
    *E301077,72 Comment out the following block of code and  [Begin]
    *E301077,72 replace it with code check for eof.
    *,SET ORDER TO TAG COMPID
    *,IF SEEK(gcAct_Comp)
    *,  IF nAcsSegSz = 0
        *** You have to add the account code structure ***
        *** before entering this option !!! ***
        *** <  Ok  > ***
    *,    =gfModalGen("TRM02000B00000","DIALOG")
    *,    RETURN 
    *,  ELSE
    *,    lnAcsSegSz = nAcsSegSz 
    *,    lnAcsNoSeg = nAcsNoSeg
    *,    lcAcsMask  = "X"+SUBSTR(cAcsMask,2)
    *,    lcAcsMask  = ALLTRIM(STRTRAN(lcAcsMask,'#','9'))
    *,    lcAcsegDes = cAcsegDes
    *,    SKIP 1
    *,    lnFrsSegSz = nAcssize
    *,  ENDIF
    *,ELSE
      *** You have to add the account code structure ***
      *** before entering this option !!! ***
      *** <  Ok  > ***
    *,  =gfModalGen("TRM02000B00000","DIALOG")
    *,  RETURN 
    *,ENDIF  
    *E301077,72 Comment out the following block of code and  [End  ]

    *E300789,6 This code is instead of the above commented code. [Begin]
    GO TOP
    *-- No records founded in Account code file.
    IF EOF()

      *** You have to add the account code structure ***
      *** before entering this option !!! ***
      *** <  Ok  > ***
      =gfModalGen("TRM02000B00000","DIALOG")
      RETURN 

    ELSE  && find account code record.

      IF nAcsSegSz = 0
        *** You have to add the account code structure ***
        *** before entering this option !!! ***
        *** <  Ok  > ***
        =gfModalGen("TRM02000B00000","DIALOG")
        RETURN 
      ELSE
        lnAcsSegSz = nAcsSegSz 
        lnAcsNoSeg = nAcsNoSeg
        lcAcsMask  = "X"+SUBSTR(cAcsMask,2)
        lcAcsMask  = ALLTRIM(STRTRAN(lcAcsMask,'#','9'))
        lcAcsegDes = cAcsegDes
        SKIP 1
        lnFrsSegSz = nAcssize
      ENDIF

    ENDIF  
    *E300789,6 This code is instead of the above commented code. [End  ]
  
  ENDIF

  lcCurr_yer = IIF(SEEK(gcPrnt_Cmp,'SYCCOMP'),sycComp.cCurr_Yer,'')
  lnCurr_yer = INT(VAL(lcCurr_yer))

  lcCurr_prd = sycComp.cCurr_prd
  lnCurr_prd = INT(VAL(sycComp.cCurr_prd))

  *E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
  *SELECT SYCFISHD
  *ldFisBgDat = IIF(SEEK(gcPrnt_Cmp+lcCurr_yer,'SYCFISHD'),sycFisHd.dFisBgDat,{})
  *ldFisEnDat = sycFisHd.dFisEnDat
  SELECT FISHD
  
  *E300789,6 Now FISHD file does not have company Id in its index. [Begin]
  *ldFisBgDat = IIF(SEEK(gcPrnt_Cmp+lcCurr_yer,'FISHD'),FisHd.dFisBgDat,{})  
  ldFisBgDat = IIF(SEEK(lcCurr_yer,'FISHD'),FisHd.dFisBgDat,{})  
  *E300789,6 Now FISHD file does not have company Id in its index. [End  ]
  
  ldFisEnDat = FisHd.dFisEnDat
  *E300692,1 end

  SKIP -1
  *E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
  *ldPyBgDate = sycFisHd.dfisbgdat
  ldPyBgDate = FisHd.dfisbgdat
  *E300692,1 end
  
  SKIP 2
  *E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
  *ldNyEnDate = sycFisHd.dFisEnDat
  ldNyEnDate = FisHd.dFisEnDat
  *E300692,1 end

  lcSJ_Def   = 'GJ'
  *-- MAB Evaluate variables used by gl module [End  ]

  *B600423,1 Reham On 06/12/95
  *B600423,1 _ Remark this line to close the files opened in the previous
  *B600423,1   lines. because the same condition was remarked in the previous
  *B600423,1   lines.
  *B600423,1 _ This was making a problem in every GL program after quitting
  *B600423,1   the program it was leaving both {SYCACCOD-SYCFISHD} open.

  *E301077,72 Comment out the following block of code [Begin]
  *E301077,72 because now we close files either run from GL or other program.
  *,IF gcAct_appl <> "GL"
    *,IF llGlAccods 
    *E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
      *USE IN ALIAS("SYCACCOD")
    *,  USE IN ALIAS("ACCOD")
    *E300692,1 end
    *,ENDIF
    *,IF llSycComp 
    *,  USE IN ALIAS("SYCCOMP")
    *,ENDIF
    *E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
    *IF llSycFishd 
      *USE IN ALIAS("SYCFISHD")
    *,  IF llFishd 
    *,  USE IN ALIAS("FISHD")
    *E300692,1 end
    *,ENDIF  
  *,ENDIF
  *E301077,72 Comment out the following block of code [End  ]

  *E301434,1 Ramy [start]
  lcCtrW = ON('KEY' , 'CTRL+W')
  lcCtrQ = ON('KEY' , 'CTRL+Q')
  lcCtrE = ON('KEY' , 'CTRL+END')
  lcCtrH = ON('KEY' , 'CTRL+HOME')
  *E301434,1 Ramy [end]


  ON KEY LABEL CTRL+W         lnDummy = 1
  ON KEY LABEL Ctrl+Q         lnDummy = 1
  ON KEY LABEL CTRL+END       lnDummy = 1
  ON KEY LABEL CTRL+HOME      lnDummy = 1

  *E301434,1 Ramy Add this line to trap F3 key [start]
  lcF3Key = ON('KEY' , 'F3')
  ON KEY LABEL F3               DO lfF3Key
  *E301434,1 Ramy [end]
  
ENDIF

*E301077,72 This code is instead of the above commented code. [Begin]
*E301077,72 close using gfSysClose
*E301077,72 Note: The following function close file if it used only.
=gfSysClose("ACCOD")
=gfSysClose("SYCCOMP")
=gfSysClose("FISHD")
*E301077,72 This code is instead of the above commented code. [End  ]

*E300683,6 Add variables holding BMP names for all BMPS use in all AP screens
lcBtMpPrc  = gcBMPHome + "PROCEED.BMP"
lcBtMpNew  = gcBMPHome + "NEW.BMP"
lcBtMpRem  = gcBMPHome + "REM.BMP"
lcBtMpCan  = gcBMPHome + "CAN.BMP"
lcBtMpCls  = gcBMPHome + "CLS.BMP"
lcBtMpOK   = gcBMPHome + "OK.BMP"
lcBtMpExt  = gcBMPHome + "EXTKEY.BMP"
lcBtMpPrnt = gcBMPHome + "PRINTER.BMP"
lcBtMpSel  = gcBMPHome + "SEL.BMP"
lcBtMpZoom = gcBMPHome + "ZOOM.BMP"
lcBtMpPst1 = gcBMPHome + "POST1.BMP"
lcBtMpHold = gcBMPHome + "HOLD.BMP"
lcBtMpDn3  = gcBMPHome + "DN3.BMP"
lcBtMpCpy1 = gcBMPHome + "COPY1.BMP"
*E300683,6 end

*E300683,6 Check if the file exists on disk.
*** Call the screen program or the procedure to be done
*DO &lcProgName
lcPrgTmp = IIF(ATC(' ',lcProgName)>0,SUBSTR(lcProgName,1,ATC(' ',lcProgName)-1),lcProgName)
IF ATC('WITH ',lcProgName)>0 AND EMPTY(lcPrgPram)
  DO &lcProgName
ELSE  
*gcDosApps = gcAppHome+gcWinAppl+'\'
gcDosApps = gcAppHome+'GL\'

*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
IF GCMULTIINST
  gcCDosApps = gcCappHome+'GL\'
ENDIF   
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]

SET FULLPATH ON
LLPRGFOUND = .F.

*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
*llPrgFound = FILE(gcDosApps+lcPrgTmp +'.FXP') .OR. ;
   FILE(gcDosApps+lcPrgTmp +'.PRG') .OR. ;
   FILE(gcDosApps+lcPrgTmp +'.EXE')
llPrgFound = (GCMULTIINST AND (FILE(gcCDosApps+lcPrgTmp +'.FXP') .OR. ;
   FILE(gcCDosApps+lcPrgTmp +'.PRG') .OR. ;
   FILE(gcCDosApps+lcPrgTmp +'.EXE'))) OR ;
   FILE(gcDosApps+lcPrgTmp +'.FXP') .OR. ;
   FILE(gcDosApps+lcPrgTmp +'.PRG') .OR. ;
   FILE(gcDosApps+lcPrgTmp +'.EXE')
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]

IF !llPrgFound
  gcDosApps = gcAppHome
  
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
  *llPrgFound = FILE(gcDosApps+lcPrgTmp +'.FXP') .OR. ;
     FILE(gcDosApps+lcPrgTmp +'.PRG') .OR. ;
     FILE(gcDosApps+lcPrgTmp +'.EXE')
  IF GCMULTIINST   
    gcCDosApps = gcCappHome         
  ENDIF   
  llPrgFound =(GCMULTIINST AND (FILE(gcCDosApps+lcPrgTmp +'.FXP') .OR. ;
	 FILE(gcCDosApps+lcPrgTmp +'.PRG') .OR. ;
	 FILE(gcCDosApps+lcPrgTmp +'.EXE'))) OR ;
	 FILE(gcDosApps+lcPrgTmp +'.FXP') .OR. ;
     FILE(gcDosApps+lcPrgTmp +'.PRG') .OR. ;
     FILE(gcDosApps+lcPrgTmp +'.EXE')   
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]
  
ENDIF   
IF llPrgFound
  *E300581,1 Hesham El-Sheltawi (Start)
  *E300581,1 make the menu can call the program with paramters
  IF !EMPTY(lcPrgPram)
    
    *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
    *DO (gcDosApps+lcPrgTmp) WITH &lcPrgPram
    IF (GCMULTIINST AND (FILE(gcCDosApps +lcPrgTmp +'.FXP') .OR. ;
 	   FILE(gcCDosApps +lcPrgTmp +'.PRG') .OR. ;
	   FILE(gcCDosApps +lcPrgTmp +'.EXE')))
	 
 	 DO (gcCDosApps +lcPrgTmp) WITH &lcPrgPram
	ELSE
	 DO (gcDosApps+lcPrgTmp) WITH &lcPrgPram 
	ENDIF 
    *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]
    
  ELSE
    
    *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
    *DO (gcDosApps+lcPrgTmp)
    IF (GCMULTIINST AND (FILE(gcCDosApps +lcPrgTmp +'.FXP') .OR. ;
	  FILE(gcCDosApps +lcPrgTmp +'.PRG') .OR. ;
	  FILE(gcCDosApps +lcPrgTmp +'.EXE')))
	  
	  DO (gcCDosApps +lcPrgTmp)
	  
	ELSE
  	  DO (gcDosApps+lcPrgTmp)
	ENDIF  
    *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]
    
  ENDIF  
  *E300581,1 Hesham El-Sheltawi (End)  
ELSE
  DO &lcProgName
ENDIF  
*E300683,6 end
ENDIF
*YMO

*E301077,72 Comment out the following block of code and  [Begin]
*E301077,72 because we have closed these files in an earlier stage.
*IF gcAct_appl = "GL"

  *;IF llGlAccods 
    *E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
    *USE IN ALIAS("SYCACCOD")
    *;USE IN ALIAS("ACCOD")
    *E300692,1 end
  *;ENDIF
  
  *;IF llSycComp 
    *;USE IN ALIAS("SYCCOMP")
  *;ENDIF
  
  *E300692,1 CHANGE FILE NAME FROM SYCFISHD TO FISHD
  *IF llSycFishd 
  *USE IN ALIAS("SYCFISHD")
  *;IF llFishd 
    *;USE IN ALIAS("FISHD")
  *E300692,1 end  
  *;ENDIF  

*ENDIF
*E301077,72 Comment out the following block of code and  [End  ]
*YMO

IF lcjumpAll = 'F'
  *ON KEY
  ON KEY LABEL ESC &lcESC
  ON KEY LABEL F3 &lcF3Key
  *E301434,1 Ramy [end]

  #REGION 1
  =gfCleanUp()
  
ENDIF

*!**************************************************************************
*!
*!      Function : lfGetIndTag
*!
*!**************************************************************************
* Author  : Yasser Saad Ibrahim
*
FUNCTION lfGetIndTag

lcTag = ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')
RETURN lcTag

*!**************************************************************************
*!
*!      Function : lfAccntStr
*!
*!**************************************************************************
* Author  : Yasser Saad Ibrahim
*
FUNCTION lfAccntStr
*E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
*lnAcsSegSz = SYCACCOD.nAcsSegSz 
*lcAcsMask  = SYCACCOD.cAcsMask
*lcAcsegDes = SYCACCOD.cAcsegDes
lnAcsSegSz = ACCOD.nAcsSegSz 
lcAcsMask  = ACCOD.cAcsMask
lcAcsegDes = ACCOD.cAcsegDes
*E300692,1 end

*!**************************************************************************
*!
*!      PROCEDURE : gfMousTrap
*!
*!**************************************************************************
* Author  : Yasser Saad Ibrahim
*
PROCEDURE gfMousTrap
PARAMETERS lcBrow_Win,lcObjCord,lcHome_Win

lnRow = MROW(lcHome_Win)
lnCol = MCOL(lcHome_Win)

IF lnRow < 0 .OR. lnCol < 0
  =INKEY(0,'M')
  ??CHR(7)
  RETURN
ENDIF

IF MCOL(lcBrow_Win) < 0 .OR. MROW(lcBrow_Win) < 0
  FOR  lnCount = 1 TO ALEN(&lcObjCord,1)
    IF BETWEEN(lnRow,&lcObjCord[lnCount,1],&lcObjCord[lnCount,3]) .AND.;
       BETWEEN(lnCol,&lcObjCord[lnCount,2],&lcObjCord[lnCount,4])

      POP KEY 
      KEYBOARD CHR(17)
      lcObj   = &lcObjCord[lnCount,5]
      _CUROBJ = OBJNUM(&lcObj)
      KEYBOARD "{ENTER}" 
    ENDIF
  ENDFOR
ENDIF

*!**************************************************************************
*!
*!      Function: lfVldAccnt
*!
*!**************************************************************************
*  Author  : Reham Aly Alallamy
* Modifications :
*E301077,72 MAB Now this function open the required files adding them 
*E301077,72 MAB to alFileName array (Then if called from main program 
*E301077,72 MAB close them else does not close ). 
FUNCTION lfVldAccnt
PARAMETERS lcAccType1,lcAccStat1,lcAccPost1,llAddAct1,;
                       lcAccDes1,lcTypCode1,lcTypDesc1,llSngAcn

** Set defaults if no sending of parameters **
lcAccType1 = IIF(TYPE("lcAccType1") $ "UL" , 'A' , lcAccType1)
lcAccStat1 = IIF(TYPE("lcAccStat1") $ "UL" , 'A' , lcAccStat1)
lcAccPost1 = IIF(TYPE("lcAccPost1") $ "UL" , 'A' , lcAccPost1)

lcTAccCode = 'Account code is added'
lcTSegVal  = 'Segment value is added'
lcAccCode  = LEFT(EVALUATE(SYS(18)),lnAcsSegSz)  && Var to hold the account
lcFrsSeg   = LEFT(lcAccCode,lnFrsSegSz)          && Var to hold the 1st segment

lcObj_Nam  = SYS(18)          && This var. hold the current object.

llAddSeg   = .T.              && This flag to add segment in the segment
                              && value file .
llFoundAcn = .F.              && This var. to know if the accont found
                              && found in the chart of account or not.

llCallRet  = .T.              && This flag to at the end of this function
                              && if I cancel the operation or the account
                              && has value..

lcAliasSav = ALIAS()          && Save the old alias..

llOpenSet  = .F.              && Open GLSETUP or not.
llOpenUsr  = .F.              && Open SYUUSER or not.
llOpenPrv  = .F.              && Open SYUUSRPR or not.

SELECT GLACCHAR
lcCurrTag = SYS(22)
SET ORDER TO TAG ACCTCODE

*E301077,72 Comment out the following block of code and  [Begin]
*E301077,72 open file using gfOpenFile.
*;IF !USED('GLSETUP')
*;  SELECT 0
*;  USE &gcDataDir.GLSETUP 
*;  llOpenSet  = .T.
*;ENDIF   

*;IF !USED("SYUUSER")
*;  SELECT 0
*;  USE &gcSysHome.SYUUSER 
*;  llOpenUsr  = .T.
*;ENDIF

*;IF !USED("SYUUSRPR")
*;  SELECT 0
*;  USE &gcSysHome.SYUUSRPR
*;  llOpenPrv  = .T.
*;ENDIF
*E301077,72 Comment out the following block of code and  [End  ]

*E301077,72 The following is instead the above commented one  [Begin]
llOpenSet = gfOpenfile(gcDataDir+'GLSETUP','','SH')

IF USED('SYUUSER')
  SELECT SYUUSER
  lcUserTag=TAG()
  SET ORDER TO TAG CUSER_ID
  llOpenUsr = .F.
ELSE  
  llOpenUsr = gfOpenfile(gcSysHome+'SYUUSER',gcSysHome+'CUSER_ID','SH')
ENDIF  

IF USED('SYUUSRPR')
  SELECT SYUUSRPR
  lcUsPrv = TAG()
  SET ORDER TO TAG CUSER_ID
  llOpenPrv = .F.
ELSE
  
  *B602835,1 set index in this case [Begin] 
  *llOpenPrv = gfOpenfile(gcSysHome+'SYUUSRPR','','SH')
  
  llOpenPrv = gfOpenfile(gcSysHome+'SYUUSRPR','CUSER_ID','SH')
  
  *B602835,1 set index in this case [End..] 

ENDIF  
*E301077,72 The following is instead the above commented one  [End  ]

SELECT GLSETUP
IF RECCOUNT() <> 0
  GO 1
ENDIF

IF GLSETUP.lSetAlAdd .OR. llSngAcn

   *E301077,72 Open international file [Begin]
   llOpenInt = gfOpenfile(gcSysHome+'SYCINST','','SH')  && Open International file.
   GO TOP
   *E301077,72 Open international file [End  ]
   *B605916,1 ASH 04/28/2002 (Begin) The 'Add' button not appearing in case pf the user ID is 'ADMN'
   *IF SYCINST.linslogrq
   IF SYCINST.linslogrq AND UPPER(gcUser_id) <> 'ADMN'
   *B605916,1 ASH 04/28/2002 (End)
     =SEEK(gcUser_id,'SYUUSER')   
     IF SYUUSER.cusr_levl <> 'A'
       IF SEEK(ALLTRIM(gcUser_ID)+gcAct_Appl+gcAct_Comp+'GLSNGAC','SYUUSRPR')
         glAddAct  = SYUUSRPR.lAddRec
       ELSE
         glAddAct  = .F. 
       ENDIF 
     ELSE 
       glAddAct  = .T. 
     ENDIF
   ELSE
     glAddAct  = .T.
   ENDIF

   *E301077,72 Close international file [Begin]
   IF llOpenInt
     = gfSysClose('SYCINST')  && Close International file, if not called screen.
   ENDIF  
   *E301077,72 Close international file [End  ]

ELSE
  glAddAct  = .F.
ENDIF

** If the acc <> ? but contain any spaces **
IF LEFT(lcAccCode,1) <> "?" .AND. VAL(STRTRAN(lcAccCode,'-','')) = 0
  ** You have to enter an account code. **
  **               < Ok >               **
  =gfModalGen("TRM02040B00000","DIALOG",lcFrsSeg)
  IF FSIZE(lcObj_Nam) > 0
    REPLACE &lcObj_Nam WITH REPLICATE("0",lnAcsSegSz)   
  ELSE
    &lcObj_Nam  = REPLICATE("0",lnAcsSegSz)   
  ENDIF  
  _CUROBJ = OBJNUM(lcObj_Nam)
  SHOW GET &lcObj_Nam ENABLE
  *B602835,1 The variables lcUserTag,lcUsPrv will be declared only if [Begin]
  *         the file files are used . Else thay are not defined so we
  *         can't use them without condition .
   
  **B Hesham El-Sheltawi setting the right tag
  lcCurArea = SELECT()
  *SELECT SYUUSER
  *SET ORDER TO TAG (lcUserTag)
  *SELECT SYUUSRPR
  *SET ORDER TO TAG (lcUsPrv )
  *SELECT (lcCurArea)
  **B Hesham El-Sheltawi end modification  
  
  
  IF !llOpenUsr
    SELECT SYUUSER
    SET ORDER TO TAG (lcUserTag)
  ENDIF
  
  IF !llOpenPrv
    SELECT SYUUSRPR
    SET ORDER TO TAG (lcUsPrv )
  ENDIF
  *B602835,1 The variables lcUserTag,lcUsPrv will be declared only if [End..]
  SELECT (lcCurArea)
  
  RETURN .F.
ENDIF

IF LEFT(lcAccCode,1) <> "?"
  IF lnAcsNoSeg > 1
    lcAcnTry  = STRTRAN(lcAccCode, '-', '', 1, lnAcsNoSeg-1)
  ELSE
    lcAcnTry  = lcAccCode
  ENDIF
  llNumeric = lfDigit(ALLTRIM(lcAcnTry),.T.)

  *** If the account contain any charcters ***
  IF !llNumeric
    *** Account code cannot have charcters. ***
    *** < Ok > ***
    =gfModalGen("INM02050B00000","DIALOG")
    IF FSIZE(lcObj_Nam) > 0
      REPLACE &lcObj_Nam WITH REPLICATE("0",lnAcsSegSz)   
    ELSE
      &lcObj_Nam  = REPLICATE("0",lnAcsSegSz)   
    ENDIF  
    _CUROBJ = OBJNUM(lcObj_Nam)
    SHOW GET &lcObj_Nam ENABLE
    
    **B Hesham El-Sheltawi setting the right tag
    lcCurArea = SELECT()
    *SELECT SYUUSER
    *SET ORDER TO TAG (lcUserTag)
    *SELECT SYUUSRPR
    *SET ORDER TO TAG (lcUsPrv )
    *B602835,1 The variables lcUserTag,lcUsPrv will be declared only if [Begin]
    *         the file files are used . Else thay are not defined so we
    *         can't use them without condition .
    
    IF !llOpenUsr
      SELECT SYUUSER
      SET ORDER TO TAG (lcUserTag)
    ENDIF
  
    IF !llOpenPrv
      SELECT SYUUSRPR
      SET ORDER TO TAG (lcUsPrv )
    ENDIF
  *B602835,1 The variables lcUserTag,lcUsPrv will be declared only if [End..]
    
    SELECT (lcCurArea)
    **B Hesham El-Sheltawi end modification    
    RETURN .F.
  ENDIF
  
*  IF VAL(LEFT(lcAccCode,AT("-",lcAccCode)-1)) = 0
  IF VAL(lcAccCode) = 0                    && Changed by Yasser on 3-7-95
    *** Segment no. 1 cannot be zeros. ***
    *** <  Ok > ***
    =gfModalGen("TRM02217B00000","DIALOG")
    &lcObj_Nam  = REPLICATE("0",lnAcsSegSz)   
    _CUROBJ = OBJNUM(lcObj_Nam)
    SHOW GET &lcObj_Nam ENABLE
    
    *B602835,1 The variables lcUserTag,lcUsPrv will be declared only if [Begin]
    *         the file files are used . Else thay are not defined so we
    *         can't use them without condition .
    
    **B Hesham El-Sheltawi setting the right tag
    lcCurArea = SELECT()
    *SELECT SYUUSER
    *SET ORDER TO TAG (lcUserTag)
    *SELECT SYUUSRPR
    *SET ORDER TO TAG (lcUsPrv )

    
    IF !llOpenUsr
      SELECT SYUUSER
      SET ORDER TO TAG (lcUserTag)
    ENDIF
  
    IF !llOpenPrv
      SELECT SYUUSRPR
      SET ORDER TO TAG (lcUsPrv )
    ENDIF
  *B602835,1 The variables lcUserTag,lcUsPrv will be declared only if [End..]    
    SELECT (lcCurArea)
**B Hesham El-Sheltawi end modification    
    RETURN .F.
  ENDIF
ENDIF

IF !EMPTY(lcAccCode)
  IF LEFT(lcAccCode,1) = "?" .OR. (" " $ lcAccCode)

    *** If the 1st char. of acc. = ? or acc. contain any spaces ...
    *** Select List : ***
    *** Browse - Segments - Reenter ***
    lnOption = gfModalGen("QRM02023B02002","DIALOG")
    DO CASE
      CASE lnOption = 1                    && Browse Accounts
        DO GLACBROW.PRG
      CASE lnOption = 2                    && Browse Segments
        DO GLACSEG.PRG 
      CASE lnOption = 3                    && Reenter 
        llCallRet  = .F.
    ENDCASE
  ELSE
    IF SEEK(lcAccCode,"GLACCHAR")
      *** If the value found in the chart of account file ***
      llFoundAcn = .T.
      IF FSIZE(lcObj_Nam) > 0
        REPLACE &lcObj_Nam WITH GLACCHAR.cacctcode
      ELSE
        &lcObj_Nam  = GLACCHAR.cacctcode
      ENDIF  
      lcAccDes1  = GLACCHAR.caccnldes
      lcTypCode1 = GLACCHAR.ctypecode
      lcTypDesc1 = LOOKUP(GLTYPES.cTypedesc,lcTypCode1,GLTYPES.cTypecode,'Typecode')
    ELSE
      *** If the value not found in the chart of account file ***

      IF llAddAct1 .AND. glAddAct
        *** If adding new record is allowed in calling program ***
        *** Account {lcAccCode} not found     ***
        *** Browse - Segments - Add - Reenter ***
        lnOption = gfModalGen("QRM02025B02003","DIALOG",SUBSTR(lcAccCode,1,lnAcsSegSz))
        DO CASE
          CASE lnOption = 1                    && Browse Accounts
            DO GLACBROW.PRG
          CASE lnOption = 2                    && Browse Segments
            DO GLACSEG.PRG 
          CASE lnOption = 3                    && Add New Account
            DO GLACADD.PRG 
          CASE lnOption = 4                    && Reenter
            llCallRet  = .F.
        ENDCASE
      ELSE

        *** If adding new record not allowed in the calling program ***
        *** Account {lcAccCode} not found ***
        *** Browse - Segments - Reenter ***
        lnOption = gfModalGen("QRM02025B02002","DIALOG",SUBSTR(lcAccCode,1,lnAcsSegSz))
        DO CASE
          CASE lnOption = 1                    && Browse Accounts
            DO GLACBROW.PRG
          CASE lnOption = 2                    && Browse Segments
            DO GLACSEG.PRG 
          CASE lnOption = 3                    && Reenter
            llCallRet  = .F.
        ENDCASE
      ENDIF
    ENDIF
  ENDIF
ENDIF

IF llCallRet
  *** If the account already exist or just added in the ***
  *** chart of account, check if there is any changes   ***
  *** in the status or allow posting or type ......     ***
  IF SEEK(&lcObj_Nam,"GLACCHAR")
    lcSegactiv = IIF(lcAccStat1 = 'C' , 'A' , 'I')
    lcSegalpos = IIF(lcAccPost1 = 'L' , 'Y' , 'N')
    lcStandard = IIF(lcAccType1 = 'T' , 'Y' , 'N')
    DO CASE
      CASE lcAccStat1 <> 'A' .AND. GLACCHAR.cSegactiv <> lcSegactiv
        *** Account ð is Inactive. ***
        *** < Ok > ***
        =gfModalGen("QRM02052B00000","DIALOG",ALLTRIM(&lcObj_Nam))
        llFoundAcn = .F.
        llCallRet  = .F.
      CASE lcAccPost1 <> 'A' .AND. GLACCHAR.cSegalpos <> lcSegalpos
        *** Account ð is not allowed for posting from G.L. ***
        *** < Ok > ***
        =gfModalGen("QRM02054B00000","DIALOG",ALLTRIM(&lcObj_Nam))
        llFoundAcn = .F.
        llCallRet  = .F.
      CASE lcAccType1 <> 'A'
        DO CASE
          CASE lcStandard = 'Y' .AND. GLACCHAR.cStandard <> lcStandard
            *** Account major ð has a statistical type.  ***
            *** Only standard type is allowed here. ***
            *** < Ok > ***
            =gfModalGen("QRM02031B00000","DIALOG",ALLTRIM(&lcObj_Nam))
            llFoundAcn = .F.
            llCallRet  = .F.
          CASE lcStandard = 'N' .AND. GLACCHAR.cStandard <> lcStandard
            *** Account major ð has a standard type.   *** 
            *** Only statistical type is allowed here. ***
            *** < Ok > ***
            =gfModalGen("QRM02030B00000","DIALOG",ALLTRIM(&lcObj_Nam))
            llFoundAcn = .F.
            llCallRet  = .F.
        ENDCASE
    ENDCASE
  ENDIF
ENDIF

** If the user didn't selct any account, the account object **
** should be equal zeros ....
IF !llCallRet
  IF FSIZE(lcObj_Nam) > 0
    REPLACE &lcObj_Nam WITH REPLICATE("0",lnAcsSegSz)   
  ELSE
    &lcObj_Nam  = REPLICATE("0",lnAcsSegSz)   
  ENDIF  
  _CUROBJ = OBJNUM(lcObj_Nam)
  SHOW GET &lcObj_Nam ENABLE
ENDIF

** Close the opened files. **
IF llOpenSet
  llOpenSet = .F.
  *E301077,72 Close GLSETUP using gfSysClose
  *USE IN GLSETUP
  =gfSysClose('GLSETUP')
ENDIF

IF llOpenUsr
  llOpenUsr = .F.
  *E301077,72 Close SYUUSER using gfSysClose
  *USE IN SYUUSER
  =gfSysClose('SYUUSER')
ELSE
  SELECT SYUUSER
  SET ORDER TO TAG (lcUserTag)
ENDIF

IF llOpenPrv
  llOpenPrv = .F.
  *E301077,72 Close SYUUSRPR using gfSysClose
  *USE IN SYUUSRPR
  =gfSysClose('SYUUSRPR')
ELSE
  SELECT SYUUSRPR
  SET ORDER TO TAG (lcUsPrv )
ENDIF

SELECT GLACCHAR
SET ORDER TO TAG (lcCurrTag)

** Select the old alias **
SELECT (lcAliasSav)

** Return .T. means the account is not empty **
** Return .F. means the account is empty ... **
RETURN llCallRet

*!**************************************************************************
*!
*!      Function : lfDelAcnt
*!
*!**************************************************************************
*  Author  : Yasser Saad Ibrahim
*
FUNCTION lfDelAcnt
PARAMETERS lcDelAcnt

lcDelAcnt  = ALLTRIM(lcDelAcnt)+SPACE(24-LEN(ALLTRIM(lcDelAcnt)))
lcDespAcnt = ALLTRIM(lcDelAcnt) 

llRemGrp = .F.
llRemBud = .F.
llRemAut = .F.

lcSavAlias = ALIAS()

*E301077,72 Open Setups file using gfOpenFile.[Begin
= gfOpenfile(gcDataDir+'GLSETUP','','SH')
*E301077,72 Open Setups file using gfOpenFile.[End  

** Check the default retained account
IF VAL(lcDelAcnt) = VAL(GLSETUP.CSETRETMJ) .AND. ;
   VAL(STRTRAN(SUBSTR(lcDelAcnt,AT('-',lcDelAcnt)+1),'-','')) = 0

  ** Account ð is the default retained earning account.
  ** You can not delete it.
  ** < Ok > **
  =gfModalGen("INM02042B00000","ALERT",lcDespAcnt)
  
  *E301077,72 Close Setups file using gfSysClose.[Begin
  =gfSysClose('GLSETUP')
  *E301077,72 Close Setups file using gfSysClose.[End

  SELECT (lcSavAlias)
  
  RETURN .F. 
ENDIF

** Check the default retained account
IF VAL(lcDelAcnt) = VAL(GLSETUP.CSETSUSMJ) .AND. ;
   VAL(STRTRAN(SUBSTR(lcDelAcnt,AT('-',lcDelAcnt)+1),'-','')) = 0

  ** Account ð is the suspend account.
  ** You can not delete it.
  ** < Ok > **
  =gfModalGen("INM02049B00000","ALERT",lcDespAcnt)

  *E301077,72 Close Setups file using gfSysClose.[Begin
  =gfSysClose('GLSETUP')
  *E301077,72 Close Setups file using gfSysClose.[End

  SELECT (lcSavAlias)
  RETURN .F. 
ENDIF

*E300297,1 M.H 10/10/95 Add currency to the GL module.
**Begin E300297,1
** Check the default exchange reates accounts
IF gfGetMemVar('LLMULCURR')
  IF VAL(lcDelAcnt) = VAL(GLSETUP.CSETEXMJ) .AND. ;
    VAL(STRTRAN(SUBSTR(lcDelAcnt,AT('-',lcDelAcnt)+1),'-','')) = 0

    ** "Account ð is the default exchange rates"
    ** "account You can not delete it.         "
    ** "                 ® Ok ¯                "
    =gfModalGen("INM02247B00000","ALERT",lcDespAcnt)

    *E301077,72 Close Setups file using gfSysClose.[Begin
    =gfSysClose('GLSETUP')
    *E301077,72 Close Setups file using gfSysClose.[End

    SELECT (lcSavAlias)
    RETURN .F. 
  ENDIF
ENDIF
*End E300297,1
** Check if the account has activities in the balance file
SELECT GLACBALS
lcNext_yer = STR(VAL(lcCurr_yer)+1,4)
=gfWait("02222","NOWAIT")

IF SEEK(lcDelAcnt)
  SCAN REST WHILE  CACCTCODE = lcDelAcnt 
    DO CASE
      CASE  (cFisfYear = lcCurr_yer .OR. cFisfYear = lcCurr_yer);
            .AND. nacbopbal>0
         ** Account ð has balaces. **
         ** You can not delete it. **
         ** < Ok > **
         =gfModalGen("INM02043B00000","ALERT",lcDespAcnt)

        *E301077,72 Close Setups file using gfSysClose.[Begin
        =gfSysClose('GLSETUP')
        *E301077,72 Close Setups file using gfSysClose.[End

         SELECT (lcSavAlias)
         RETURN .F. 
      CASE  nacbptddr+nacbptdcr > 0 
        ** Account ð has activities in balaces file.
        ** You can not delete it.
        ** < Ok > **
        =gfModalGen("INM02056B00000","ALERT",lcDespAcnt)

        *E301077,72 Close Setups file using gfSysClose.[Begin
        =gfSysClose('GLSETUP')
        *E301077,72 Close Setups file using gfSysClose.[End

        SELECT (lcSavAlias)
        RETURN .F. 
    ENDCASE  
  ENDSCAN
ENDIF

** Is there any transactions including this account with 
** status 'U' or 'O'
SELECT GLTRNSDT
SET ORDER TO TAG ACCTCODE

=gfWait("02223","NOWAIT")

IF SEEK(lcDelAcnt)              
    ** There is one or more unposted transaction for account ð.
    ** You must void those transactions befor deleting the account.
    ** Deletion is canceled.
    ** < Ok > **
    =gfModalGen("INM02044B00000","ALERT",lcDespAcnt)

    *E301077,72 Close Setups file using gfSysClose.[Begin
    =gfSysClose('GLSETUP')
    *E301077,72 Close Setups file using gfSysClose.[End

    SELECT (lcSavAlias) 
    RETURN .F. 
ENDIF 

** Check if there is any group include this account
SELECT GLGRPDT
SET ORDER TO TAG ACCTCODE

WAIT "Checking the group file.." WINDOW NOWAIT
=gfWait("02224","NOWAIT")

IF SEEK(lcDelAcnt)
  ** There is one or more groups include account ð .
  ** Do you want to remove this account from group(s),
  ** or cancel deletion  ?
  ** < Remove > - < Cancel >
  IF gfModalGen("INM02045B00007","ALERT",lcDespAcnt) = 1
    llRemGrp = .T.
  ELSE

    *E301077,72 Close Setups file using gfSysClose.[Begin
    =gfSysClose('GLSETUP')
    *E301077,72 Close Setups file using gfSysClose.[End

    SELECT (lcSavAlias)
    RETURN .F. 
  ENDIF
ENDIF

** Check if there is any budget include this account
SELECT GLBUDDT
SET ORDER TO TAG ACCTCODE
=gfWait("02225","NOWAIT")

IF SEEK(lcDelAcnt)
  ** There is one or more budgets include account ð .
  ** Do you want to remove this account from the budget(s),
  ** or cancel deletion?
  ** < Remove > - < Cancel > **
  IF gfModalGen("INM02046B00007","ALERT",lcDespAcnt) = 1
    llRemBud = .T.
  ELSE

    *E301077,72 Close Setups file using gfSysClose.[Begin
    =gfSysClose('GLSETUP')
    *E301077,72 Close Setups file using gfSysClose.[End

    SELECT (lcSavAlias)
    RETURN .F. 
  ENDIF
ENDIF

** Check if there is any automatic transaction include this account
SELECT GLAUTDT

SET ORDER TO TAG ACCTCODE

=gfWait("02226","NOWAIT")

IF SEEK(lcDelAcnt)
  ** There is one or more automatic transactions include account ð .
  ** Do you want to delete these automatic transactions, or cancel deletion? 
  ** < Remove > < Cancel > **
  IF gfModalGen("INM02047B00007","ALERT",lcDespAcnt) = 1
    llRemAut = .T.
  ELSE

    *E301077,72 Close Setups file using gfSysClose.[Begin
    =gfSysClose('GLSETUP')
    *E301077,72 Close Setups file using gfSysClose.[End

    SELECT (lcSavAlias)
    RETURN .F.
  ENDIF
ENDIF

** Deletion process

IF llRemGrp 
  DECLARE laGrpLins[1]
  laGrpLins = ' '
  SELECT cGrpcode FROM &gcDataDir.GLGRPDT ;
         WHERE cAcctcode = lcDelAcnt ;
         ORDER BY cGrpcode ;
         INTO ARRAY laGrpLins ;
         DISTINCT
  
  SELECT GLGRPDT
  IF SEEK(lcDelAcnt)
    =gfWait("02227","NOWAIT",ALLTRIM(lcDelAcnt))
    DELETE REST WHILE cAcctCode = lcDelAcnt  
  ENDIF
  
  SELECT GLGRPDT
  SET ORDER TO TAG GRCODACC
  IF !EMPTY(laGrpLins[1])
    FOR lnCount = 1 TO ALEN(laGrpLins,1)
      SELECT GLGRPDT
      IF !SEEK(laGrpLins[lnCount])
        SELECT GLGRPHD
        IF SEEK(laGrpLins[lnCount])
          DELETE
        ENDIF
      ENDIF
    ENDFOR
  ENDIF
ENDIF

IF llRemBud 
  SELECT GLBUDDT
  IF SEEK(lcDelAcnt)
    =gfWait("02228","NOWAIT",ALLTRIM(lcDelAcnt))
    DELETE REST WHILE cAcctCode = lcDelAcnt
  ENDIF  
ENDIF

IF llRemAut 
  =gfWait("02229","NOWAIT",ALLTRIM(lcDelAcnt))
  DECLARE laAutLins [1]
  laAutLins = ' '
  SELECT cautcode FROM &gcDataDir.GLAUTDT ;
         WHERE cacctcode = lcDelAcnt ;
         ORDER BY cautcode ;
         INTO ARRAY laAutLins;
         DISTINCT
  IF !EMPTY(laAutLins [1])  
    FOR lnCount = 1 TO ALEN(laAutLins ,1)
     SELECT GLAUTDT     
     DELETE ALL FOR cautcode = laAutLins[lnCount]
     SELECT GLAUTHD
     IF SEEK(laAutLins[lnCount])
       DELETE
     ENDIF
    ENDFOR
  ENDIF  
ENDIF
    
SELECT GLACBALS 
IF SEEK(lcDelAcnt)
  =gfWait("02230","NOWAIT",ALLTRIM(lcDelAcnt))
  *B127253,1 MHM 04/11/2005 Fix bug of not deleting all record from GLACBALS.[Start]
  *DELETE REST WHILE cAcctCode = lcDelAcnt
  DELETE ALL FOR  cAcctCode = lcDelAcnt
  *B127253,1 MHM 04/11/2005 [End]
ENDIF

SELECT GLACCHAR
IF SEEK(lcDelAcnt)
  =gfWait("02231","NOWAIT",ALLTRIM(lcDelAcnt))
  DELETE
ENDIF

*E301077,72 Close Setups file using gfSysClose.[Begin
=gfSysClose('GLSETUP')
*E301077,72 Close Setups file using gfSysClose.[End
SELECT (lcSavAlias)
*-- end of lfDelAcnt.

*!*************************************************************************
*!
*!        FUNCTION : gfPeriod
*!
*!**************************************************************************
*  Author  : Mohamed Hassan Mohamed
*
* This function is to return the period of a date and a company
* the first parameter is for the date and the scond one is for 
* the company.
* Modifications :
*E301077,72 MAB Now this function open the required files adding them 
*E301077,72 MAB to alFileName array (Then if called from main program 
*E301077,72 MAB close them else does not close ). 
*
FUNCTION gfPeriod

PARAMETERS ldDate,lcCompany,lcPerYear,ldBgDatPer,ldEnDatPer,llLockStat

lcSavSelct = ALIAS()
llOpenby = .F.

*E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
*IF !USED('sycfsprd')
IF !USED('FSPRD')

  *E301077,72 Open file using gfOpenFile Function.[Begin]
  *E300692,1 end
  *;llOpenBy = .T.
  SELECT 0
  *E300692,1 CHANGE FILE NAME AND PATH FROM SYCFSPRD TO FSPRD
  *USE &gcSysHome.sycfsprd ORDER TAG Comfyrprdi
  *;USE &gcDataDir.fsprd ORDER TAG Comfyrprdi
  llOpenBy = gfOpenfile(gcDataDir+'FSPRD',gcDataDir+'Comfyrprdi','SH')
  *E301077,72 Open file using gfOpenFile Function.[End  ]

ELSE

  *SELECT sycfsprd
  SELECT fsprd
  *E300692,1 end
  lcSavOrder = SET('ORDER')
  SET ORDER TO TAG Comfyrprdi 

ENDIF  

ldDate     = IIF(TYPE('ldDate') <> 'D',gdSysDate,ldDate)
lcCompany  = IIF(TYPE('lcCompany') <> 'C',gcAct_Comp,lcCompany)
lcSavExact = SET('EXACT')
lcSavCent  = SET('CENTURY')

SET EXACT OFF
SET CENTURY ON

*E300789,6 now this file does not have company Id Code [Begin]
*SEEK lcCompany
GO TOP
*LOCATE REST FOR BETWEEN(ldDate,dfsppbgdt,dfsppendt);
*                WHILE cComp_Id = lcCompany
LOCATE REST FOR BETWEEN(ldDate,dfsppbgdt,dfsppendt)
*E300789,6 now this file does not have company Id Code [End  ]

IF FOUND() &&.AND. BETWEEN(ldDate,ldPyBgDate,ldNyEnDate)
  lnPeriod   = INT(VAL(cFsppRdid))
  lcPerYear  = cFisfYear
  ldBgDatPer = dFsppbgdt
  ldEnDatPer = dFsppendt
  llLockStat = lFsPLockS
*E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
  *IF USED('sycfsprd') .AND. llOpenBy
  IF USED('fsprd') .AND. llOpenBy
    *E300692,1 end
    llOpenBy = .F.
    *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD  
    *SELECT sycfsprd 
    *USE IN sycfsprd 
    *E301077,72 Close file using gfSysClose Function.[Begin]    
    *;SELECT fsprd 
    *;USE IN fsprd 
    =gfSysClose('FSPRD')
  *E300692,1 end
  ELSE
    SET ORDER TO &lcSavOrder
  ENDIF  
  
  IF !EMPTY(lcSavSelct)
    SELECT(lcSavSelct)
  ENDIF  
  SET EXACT &lcSavExact
  SET CENTURY &lcSavCent
  RETURN lnPeriod
ELSE
  *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD
  *IF USED('sycfsprd') .AND. llOpenBy
  IF USED('fsprd') .AND. llOpenBy
    *E300692,1 end
    llOpenBy = .F.
    *E300692,1 CHANGE FILE NAME FROM SYCFSPRD TO FSPRD 
    *SELECT sycfsprd 
    *USE IN sycfsprd 
    *E301077,72 Close file using gfSysClose Function.[Begin]    
    *;SELECT fsprd 
    *;USE IN fsprd 
    =gfSysClose('FSPRD')
    *E300692,1 end
  ELSE
    SET ORDER TO &lcSavOrder
  ENDIF  
  lcPerYear  = ' '
  ldBgDatPer = {}
  ldEnDatPer = {}
  llLockStat = .F.
  IF !EMPTY(lcSavSelct)
    SELECT(lcSavSelct)  
  ENDIF
  SET EXACT &lcSavExact
  SET CENTURY &lcSavCent
  RETURN 0
ENDIF

*!**************************************************************************
*!
*!      FUNCTION: lfDigit
*!
*!**************************************************************************
* Author : Hesham Alsheltwy
*
FUNCTION lfDigit

PARAMETERS lcNum,llPeriod

** Func. to know if the sended value have any char. **
** or the value sended is numeric. **

llPersel = .F.
llNum    = .T.
lnCount  = 1

DO WHILE lnCount <= LEN(lcNum) .AND. llNum
  llNum = ISDIGIT(SUBSTR(lcNum,lnCount,1))
  IF !llPeriod .AND. !llNum .AND. !llPersel
    llNum    = IIF(SUBSTR(lcNum,lcCount,1) = '.' , .T. , .F. )
    llPerSel = llNum
  ENDIF
  lnCount = lnCount + 1
ENDDO

RETURN llNum

*!**************************************************************************
*!
*!      Function:  lfSamType
*!
*!**************************************************************************
*
FUNCTION lfSamType
*PARAMETERS lcCompID
PARAMETERS lcCompID, lcDataDir
*
*BADRAN GL (I think we must call error handler here...)
PRIVATE lnCurAlias, lcTempName, llRetVal

lnCurAlias = SELECT()
lcTempName = gfTempName()
llRetVal   = .F.

IF FILE(lcDataDir + 'ACCOD.DBF')
  SELECT 0
  USE (lcDataDir + 'ACCOD.DBF') AGAIN ALIAS (lcTempName) ORDER TAG ACCSEGNO
  *E300789,6 go top of file, no need to use index.
  GO TOP
  llRetVal = ALLTRIM(lcMainMask) == ALLTRIM(cAcsMask)
  USE IN (lcTempName)
ENDIF

SELECT (lnCurAlias)
RETURN llRetVal
  
** check if the source company have the same code structure,
** in this case returen true, else returs false.

*E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
*SELECT SYCACCOD
*SELECT ACCOD
*E300692,1 end
*IF SEEK(lcCompID)
*E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD  
  *lcMask = SYCACCOD.cAcsMask
 * lcMask = ACCOD.cAcsMask
*E300692,1 end
 * RETURN lcMask == lcMainMask
*ENDIF
*RETURN .F.

*!**************************************************************************
*!
*!      Function:  lfSamAcode
*!
*!**************************************************************************
*
FUNCTION lfSamAcode
*E301077,71 [BEGIN] OPEN SYCCOMP FILE for SELECT-SQL COMMAND
=gfOpenFile(gcSysHome+'SYCCOMP','CCOMP_ID','SH')
*E301077,71 [END..]

*E300692,1 CHANGE FILE NAME FROM SYCACCOD TO ACCOD
*SELECT SYCACCOD
SELECT ACCOD
GO TOP
*E300692,1 end

*E300789,6 After now this file does not have company Id Code [Begin]
*SET ORDER TO TAG COMPID
*IF SEEK(gcAct_Comp)
  lcMainMask = cAcsMask
*ENDIF
*E300789,6 After now this file does not have company Id Code [End  ]

** select all companys from SYCCOMP that have the same accounts 
** code structure and ignore empty record into array laComLst.
*E301098,1 Hesham (Start)
*SELECT  cComp_ID+" "+cCom_Name,cCom_DDir;
        FROM &gcSysHome.SYCCOMP;
        WHERE syccomp.ccomp_id <> gcAct_Comp ;
        .AND. EMPTY(syccomp.ccompprnt) ;
        .AND. "GL" $ mComp_mdl ;
        .AND. lfSamType(cComp_ID, ALLTRIM(cCom_DDir)) ;
        INTO ARRAY laComLst

SELECT  cComp_ID+" "+cCom_Name,PADR(gfGetDataDir(ALLT(cCom_DDir)),LEN(cCom_dDir));
        FROM &gcSysHome.SYCCOMP;
        WHERE syccomp.ccomp_id <> gcAct_Comp ;
        .AND. EMPTY(syccomp.ccompprnt) ;
        .AND. "GL" $ mComp_mdl ;
        .AND. lfSamType(cComp_ID, gfGetDataDir(ALLTRIM(cCom_DDir))) ;
        INTO ARRAY laComLst

*E301098,1 Hesham (End)

** case there is no companys have the same account code structure 
IF _TALLY = 0 
  ** Message " No companies availabe have a same code structure. "
  ** then exit from this program
  =gfModalGen("TRM02102B00000","Dialog")      
  RETURN .F.
ENDIF

*!**************************************************************************
*!
*!      Function: lfwGLActBr
*!
*!**************************************************************************
*
FUNCTION lfwGLActBr

IF !MDOWN()
  RETURN .F.
ENDIF

*!**************************************************************************
*!
*!      Function: lfvGLActBr
*!
*!**************************************************************************
*
FUNCTION lfvGLActBr
PARAMETERS lcObjName

_CUROBJ  = OBJNUM(lcObjName)
llBrowse = .T.
KEYBOARD "{ENTER}"