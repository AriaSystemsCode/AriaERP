*****************************************************************
* Program          : UPDGLDIST.PRG
*----------------------------------------------------------------
* Passed Parameter : Data Path
*----------------------------------------------------------------
* Developer        : Hossam El Etreby [HDM]
*----------------------------------------------------------------
* Date             : 06/09/1999
*----------------------------------------------------------------
* Due to           : Bug 802312
*****************************************************************
PARAMETER lcDataDir
PRIVATE ldTranDate , llGlDistUs , llErrors
STORE .F. TO llErrors
gnMsgRec = 0
gcSysName = 'Aria Advantage Series'
WAIT WINDOW 'Running fix program UPDGLDIST' NOWAIT

IF EMPTY(lcDataDir)
  lcDataDir = GETDIR('C:','Select Data Directory')
  IF EMPTY(lcDataDir)
    RETURN .F.
  ENDIF
ENDIF
WAIT WINDOW 'Opening GLDIST file in ' + lcDataDir NOWAIT
llGlDistUs = gfOpenFile(lcDataDir+'GLDist')
SELECT GLDist
GO TOP

SCAN FOR EMPTY(glPeriod) .OR. EMPTY(glfyear)

  WAIT WINDOW 'Updating '+ALLTRIM(Tran_desc) + ' # ' + Tran_No NOWAIT
  lcFSYear = ''
  lcPrid  = ''
  
  IF EMPTY(TRAN_DATE)
    llErrors = .T.
  ENDIF
  
  llNthing = Checkprd(TRAN_DATE , 'lcFSYear', 'lcPrid' , tran_type)
  IF !EMPTY(lcPrid) OR !EMPTY(lcFSYear)
    REPLACE GLPERIOD WITH lcPrid,;
            GLFYEAR  WITH lcFSYear
  ELSE
    llErrors = .T.
  ENDIF
ENDSCAN
IF llErrors
  =gfModalGen(.F.,.F.,.F.,.F.,'One or more records were not fixed in G/L distribution file,'+;
             'Either because transaction date in G/L distribution file is empty or there were no valid year and/or period in the physical year.')
ENDIF
WAIT CLEAR
IF llGlDistUs
  USE IN GLDist
ENDIF
RETURN .T.
*************************************************************************
*FUNCTION CheckPrd
*************************************************************************
FUNCTION Checkprd
PARAMETERS ldDate,lcFYear,lcPeriod,lcTranTyp,llHideMsg

PRIVATE lcDType,lcAddMes1,lcAddMes2,lcSysDir,lcGlVers,lcGlComp, ;
        lcDate,llContinue,lcErrorM1,lcErrorM2, lnAlias,gcAct_Comp
        
lnAlias = SELECT()
STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO
STORE '' TO gcAct_Comp
gcAct_Comp = IIF(EMPTY(gcAct_Comp),SUBSTR(lcDataDir,LEN(lcDataDir)-1),gcAct_Comp)
gcDataDir = lcDataDir
gcPrnt_Cmp = LOOKUP(sycComp.cCompPrnt,gcAct_Comp,sycComp.cComp_ID,'CCOMP_ID')


=gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',gcAct_Comp)
lcSysDir   = ALLTRIM(M_SYS_DIR)
lcGlVers   = ALLTRIM(M_GL_VERS)
lcGlComp   = ALLTRIM(M_GL_CO)
STORE SPACE(1) TO lcDType,lcAddMes1,lcAddMes2

lcDate = DTOC(ldDate)      && Transaction date as a string used in messages
IF lcGlVers = 'S'            &&   <<<... SBT 2.5 ... >>>
  USE lcSysDir+'SYCCOMP' ORDER TAG 'COMPID' IN 0 AGAIN ALIAS 'SBTCOMP'
  =SEEK(lcGlComp,'SBTCOMP')
  =gfOpenFile(lcSysDir+'SYCHFIS',lcSysDir+'COMPID1','SH')
  =gfOpenFile(lcSysDir+'SYCDFIS',lcSysDir+'COMPID1','SH')

  llContinue = .T.
  IF SEEK(lcGlComp)
    LOCATE REST FOR BETWEEN(ldDate,Bdate,Edate) ;
                WHILE (ldDate >= Bdate) .AND. (CompId = lcGlComp)
  ENDIF
  IF !FOUND()                && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = SUBSTR(Yearprd,1,4)      && Transaction date year
    &lcPeriod = SUBSTR(Yearprd,5,2)      && Transaction date period     
  ENDIF  
  IF llContinue .AND. Permlck         && Permanently locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a permanently locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue .AND. Plocked         && Locked period
    llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF
  IF llContinue              && So far so good
    IF Pclosed               && Closed period
      IF !(lcTranTyp $ 'VI2VR2')  && Transaction is neither 
                                  && 'Void invoice' nor 'void return'.
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        lcErrorM2 = ''
      ELSE  
        llContinue = .F.
      ENDIF
    ELSE    && Period not closed. Check if it is a future period
      IF Yearprd <>  SBTCOMP.CURYR+SBTCOMP.CURPRD .AND. !(lcTranTyp $ 'VI2VR2')
        llDummy   =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        lcErrorM2 = ''
      ENDIF
    ENDIF    
  ENDIF  
  USE IN SBTCOMP
ELSE
  =gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'CCOMP_ID','SH')
  =SEEK(gcPrnt_Cmp,'SYCCOMP')
  IF 'GL' $ SYCCOMP.mModlset
    USE (gfGetDataDir(ALLTRIM(SYCCOMP.CCOM_DDIR))+'GLSETUP') SHARED AGAIN ALIAS TGLSETUP IN 0
    lDSETBBDAT=TGLSETUP.DSETBBDAT
    llAllPBB = TGLSETUP.LSETALBBE
    USE IN TGLSETUP 
  ELSE  
    lDSETBBDAT={}
    llAllPBB = .T.
  ENDIF  
  =gfOpenFile(gcDataDir+'FISHD',gcDataDir+'COMPFYEAR','SH')
  =gfOpenFile(gcDataDir+'FSPRD',gcDataDir+'COMFYRPRDI','SH')
  llContinue = .T.
  LOCATE
  IF FOUND()
    LOCATE REST FOR BETWEEN(ldDate,Dfsppbgdt,Dfsppendt) ;
                WHILE (ldDate >= Dfsppbgdt)                 
  ENDIF
  IF !FOUND()                  && No period match checked date
    llContinue = .F.
    lcErrorM1 = ' does not fall within any period. '
    lcErrorM2 = ''
  ELSE
    &lcFYear  = Cfisfyear      && Transaction date year
    &lcPeriod = Cfspprdid      && Transaction date period   
  ENDIF  
  IF llHideMsg
    SELECT (lnAlias)
    RETURN(llContinue)
  ENDIF
  *** Check if transaction date falls in a history period.
  IF llContinue .AND. Cfisfyear < STR(VAL(SYCCOMP.CCURR_YER)-1)
    llContinue = .F.
    lcErrorM1 = ' belongs to a history fiscal year.'
    lcErrorM2 = ''
  ENDIF 
  IF llContinue         
    *** Check if the transaction date before the begining balance
    *** date, and if the user is allowed to post before the begining
    *** balance date
    IF lcGlVers='A' AND !llAllPBB AND !EMPTY(lDSETBBDAT) .AND. ldDate < lDSETBBDAT
      llContinue = .F.
      lcErrorM1 = ' falls before the begining balance date.'
      lcErrorM2 = ' No posting allowed before the begining balance date. '
    ENDIF  
  ENDIF  
  IF llContinue .AND. Lfsplocks         && Locked period
    *llContinue = .F.
    lcErrorM1 = ' falls in a locked period.'
    lcErrorM2 = ''
  ENDIF  
  IF llContinue 
    IF Lfspclsds               && Closed period
      IF !(lcTranTyp $ 'VI2VR2')
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to prior period.'
        lcErrorM2 = ''
      ELSE  
        llContinue = .F.
      ENDIF
    ELSE      && Period not closed. Check if it is a future period.
      IF Cfisfyear+Cfspprdid <> SYCCOMP.CCURR_YER+SYCCOMP.CCURR_PRD .AND. !(lcTranTyp $ 'VI2VR2')
        llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
        lcErrorM1 = '&lcDType&lcDate belongs to a future period.'
        lcErrorM2 = ''
      ENDIF
    ENDIF    
  ENDIF  
ENDIF
IF !llContinue             && There is an error.
  IF lcTranTyp $ 'VI2VR2'       && Transaction is either 'Void invoice'
                                && or 'Void return'
    lcErrorM1  = ' not in the current period. '
    lcErrorM2 = ''
  ENDIF
  llDummy =  FErrInfo(lcTranTyp,'lcDType','lcAddMes1','lcAddMes2')
  lcErrorM1= lcDType + lcDate + lcErrorM1
  SELECT (lnAlias)
  RETURN(.F.)
ENDIF
SELECT (lnAlias)
RETURN(.T.)
