*:***************************************************************************
*: Program file  : SMNUMCNT
*: Program desc. : Numeric Sequence Control
*: For screen    : SMNUMCNT.SPR
*:        System : Aria Advantage Series.
*:        Module : System Manager (SM)
*:     Developer : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:     Procedures : lpShow,lpSavScr,lpDelScr,lpTab,lpBackTab
*:     Functions  : lfvCompany,lfWOldVal,lfStrtInfo,lfDispBrow,lfwBrows,lfvBrowse,lfValidSeq,
*:                : lfUnTrap,lfTrap,lfDivInfo,lfSelGroup,lfvGroup,lfOpenFls,lfFillData,
*:                : lfFillTmpl,gfModalGen,gfGetMemVar,gfOpenFile,gfAdd_Info
*:***************************************************************************
*: Passed Parameters  : ....
*:***************************************************************************
*: Example : DO SMNUMCNT
*:***************************************************************************
*: Due to E301046,1 []
*: Modification :
*:
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*E301077,26 03/07/99 MAB Open files in another alias using gfOpenFile and close them
*E301077,26              using gfCloseFile.
*B602906,1  MAB 05/17/1999 Add Select Company in all cases (also if you one company only).
*B602906,4  MAB 06/28/1999 Add if company not based on division select all codes by default
*B602906,4                 then disable all thing because you are in view mode.
*B603094,1  MAB 08/05/1999 Avoid decrease division groups if user select All
*B603094,1                 and it is view mode.
*B802446,1 Ramy 05/25/2000 Fix dublicate the records in the browse
*B603917,1 HBG 08/10/2000  Fix Bug of :
*B603917,1                   1- Alias not found
*B603917,1                   2- Disabel Edit and Delete buttons if no company selected
*B040098,1 02/16/2006 Oms ZF sequence error
*B040098,1                     When Create new Divison Group From Sequence Scereen 
*B040098,1                     it Update the Sequence Table it was Fixed to Update the cseq_chr
*B040098,1                     by Chr(0) not with Speace
*B040098,1                     1- add cseq_chr in the Structur temp table which update Sequence Table
*B040098,1                     2- assigen Chr(0) to cseq_chr to be insert in Sequence Table
*:***************************************************************************
PRIVATE lcTemplate
DECLARE laScrMode[4],laDivArray[1,1],laCompany[1,3]

*-- Declare multi session saved variables [Begin]
STORE ' ' TO lcWinCh0,lcWinCh1,lcWinCh2 
STORE ''  TO lcBrowTitl,lcGroup,lcOldValue,lcTmpSeq,lcTemplate

STORE 'ENABLE' TO lcCompStat,lcDivStat

STORE 'DISABLE' TO lcTopBtn,lcBotBtn,lcNxtBtn,lcPrvBtn
STORE 0 TO lnTop,lnBottom,lnSelRec

lcCompCode = gcAct_Comp
lcCompDir  = gcDataDir
lcCompMod  = gcCmpModules

llNewGrp   = .F.  && add New Group flag. [Enable New Button if .T. and select or view modes.]
llNewMode  = .F.  && add mode flag.      [Control save and cancel process]
llDivBased = .F.  && if Sequence based on division
llEmptyGrp = .F.  && if there is one group but it is empty group.
llFindPrev = .F.  && if we find previous group in current company.
llVldComp  = .T.  && .T. if user press tab when passing company popup.

STORE .F. TO glFromBrow,llNoShow,llFromEdit

lnMarker   = 1    && Browse marker

*B602906,1 There is no need to this variable [Begin] 
*lnSelComp  = 1    && Select mode default company.
*B602906,1 There is no need to this variable [End  ] 

lnCompany  = 1    && Default company item is equal 1
lnGroup    = 1    && Default group item is equal 1
*-- Declare multi session saved variables [End]

IF !gfSetup()
  RETURN
ENDIF

laDefProc[7]  = .F.      && Disable the control panel del proc.(lpDelScr)
laDefProc[9]  = .F.      && Disable the control panel sav proc.(lpSavScr)

lcScFields    = 'cComp_Id' 
*-- Default values for pre-declare first time run screen.
IF !WEXIST(gcBaseWind)
  *-- Collect all companies
  *E301098,1 Hesham (Start)
  *SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,mModlSet ;
    FROM SycComp ;
    INTO ARRAY laCompany ;
    ORDER BY 1
  SELECT ccomp_id+" - "+cCom_Name,PADR(gfGetDataDir(ALLT(cCom_dDir)),LEN(cCom_dDir)),mModlSet ;
    FROM syccomp ;
    INTO ARRAY laCompany ;
    ORDER BY 1
  *E301098,1 Hesham (End)
  
  IF _TALLY > 0

    lnCompany = 1  && Select Company is the default selection.
    *-- if there is more than one company.
    *B602906,1 Add select company if we have one company also.
    *B602906,1 Thus Comment out IF .... ENDIF block.
    *IF _TALLY > 1

      DECLARE laCompany[_TALLY + 1, 3]
      =AINS(laCompany,1)
      laCompany[1,1] = '                 Select Company'
      STORE '' TO laCompany[1,2],laCompany[1,3]
      
      PRIVATE lnI
      *-- Loop to get default company .

      FOR lnI = 2 TO ALEN(laCompany,1) 

        *-- Default company is current used company
        IF gcAct_Comp == PADR(laCompany[lnI,1],2)
          *FIX
          *STORE lnI TO lnCompany,lnSelComp
          lnCompany = lnI
          EXIT
        ENDIF

      ENDFOR

    *ENDIF  
    *B602906,1 Thus Comment out IF .... ENDIF block. [End  ] 
    
  ELSE

    *-- Message : 'There are no companies installed in system'
    *--                           <Ok>
    =gfModalGen('INM00339B00000','DIALOG')
    RETURN
    
  ENDIF  

  llAlowNew = .T. && Allowing add new Division Group if Based on division company.

  lcWinCh0    = gfTempName()
  lcWinCh1    = gfTempName()
  lcWinCh2    = gfTempName()
  lcTmpSeq    = gfTempName()
  lcTemplate  = gfTempName()

  lcBrowTitl  = 'Sequences'

  *-- Create Temp. File that hold sequences.
 *B040098,1 02/16/2006 Oms ZF sequence error  [START]
*  CREATE TABLE (gcWorkDir+lcTmpSeq) ;
*    (cComp_Id C(2),cFile_Nam C(8),cFld_Name C(10),nFld_Wdth N(3,0),nSeq_no N(12,0),   ;
*    nOldSeq N(12,0),cFld_Head C(25),mFld_Des M(10), cSeq_Type C(10) , cSeq_Group C(3),;
*    nSeqRec N(7,0),cData_Typ C(1),cFile_Tag C(10),lPerComp L(1),lNoEdit L(1),mModExpr M(10),; 
*    cAdd_User C(10),dAdd_Date D(8),                                                   ;
*    cAdd_Time C(11),lLok_Stat L(1),cLok_User C(10),dLok_date D(8),cLok_Time C(8))

  CREATE TABLE (gcWorkDir+lcTmpSeq) ;
    (cComp_Id C(2),cFile_Nam C(8),cFld_Name C(10),nFld_Wdth N(3,0),nSeq_no N(12,0),   ;
    nOldSeq N(12,0),cFld_Head C(25),mFld_Des M(10), cSeq_Type C(10) , cSeq_Group C(3),;
    nSeqRec N(7,0),cData_Typ C(1),cFile_Tag C(10),lPerComp L(1),lNoEdit L(1),         ; 
    mModExpr M(10),cAdd_User C(10),dAdd_Date D(8),cAdd_Time C(11),lLok_Stat L(1),     ;
    cLok_User C(10),dLok_date D(8),cLok_Time C(8),cseq_chr  c(1))

 *B040098,1 02/16/2006 Oms ZF sequence error  [END]
  INDEX ON cComp_Id + cSeq_Group + cFld_Name TAG (lcTmpSeq) 
  APPEND BLANK  && Append dummy record because initially we does not have data in temp. file

  = AFIELDS(laTempStru)
  CREATE CURSOR (lcTemplate) ;
     FROM ARRAY laTempStru
     
  SELECT (lcTemplate)   
  ZAP
  INDEX ON cSeq_Type TAG (lcTemplate) OF (gcWorkDir+lcTemplate+'.CDX')
  
  =lfOpenFls()

ENDIF

lcBaseFile = lcTmpSeq  && Navigate in Temporary file.

PUSH KEY

*--Activate Browse window
*-- Define a menu bar with a short-cut key for browse window 
*-- in the program just to be able to activate the browse window
*-- using its menu bar short-cut key.
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcBrowTitl KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTitl)
DO (gcScrDir+gcWinAppl+'\SMNMCNT.SPR')
*--

POP KEY

RELEASE WINDOW (lcBrowTitl)
RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01

*-- Close any used files in this session.
IF glQuitting

  IF USED(lcTemplate)
    USE IN (lcTemplate)  && Close Templete cursor.
  ENDIF  
  
  *E301077,26 Close Sequence file oppened in another alias. [Begin]
  *IF USED('Seq_File')
  *  USE IN Seq_File
  *ENDIF
  =lfClos_Fil('Seq_File')
  *E301077,26 Close Sequence file oppened in another alias. [Begin]

  *IF Statment to check if the temp file is open
  IF USED(lcTmpSeq)  && We are going to close the Temp Seq and then Erase it.
    USE IN (lcTmpSeq)
  ENDIF   && End of IF
  
  *IF The Temp file .DBF exist we are going to erase it
  IF FILE(gcWorkdir + lcTmpSeq + '.DBF')
    ERASE &gcWorkdir.&lcTmpSeq..DBF          && Erase the Temp file.
  ENDIF    && End of IF FILE(gcWorkdir + lcTmpSeq + '.DBF')
  
  *IF The Temp file .CDX exist we are going to erase it
  IF FILE(gcWorkdir + lcTmpSeq + '.CDX')
    ERASE &gcWorkdir.&lcTmpSeq..CDX          && Erase the Temp index.
  ENDIF    && End of IF FILE(gcWorkdir + lcTmpSeq + '.CDX')

ENDIF
*-- End Closing any used files in this session.

*-- Functions and procedures.
*--------------------------------------------------------------

*!*************************************************************
*! Name      : lfStrtInfo
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Get start information.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfDivInfo,lfSelGroup
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfStrtInfo()
*!*************************************************************
*!
FUNCTION lfStrtInfo

=lfDivInfo()   && Get division group info.

*B602906,4 if not based on division go to select group. [Begin]
*-- if system have only one company.
*IF ALEN(laCompany,1) = 2
IF !llDivBased OR ALEN(laCompany,1) = 2
  *lnCompany = 2
  lnCompany = IIF(llDivBased,2,lnCompany)
*B602906,4 if not based on division go to select group. [End  ]
  lcCompStat = 'DISABLE'
  SHOW GET lnCompany &lcCompStat
  SHOW GET ibCompany &lcCompStat
    
  = lfSelGroup()  && Detect coming screen mode and switch to it.
ENDIF
*-- end of lfStrtInfo.

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Browse Temp. sequence file.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfwBrows,lfvBrowse
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDispBrow()
*!*************************************************************
*!
FUNCTION lfDispBrow
SELECT (lcTmpSeq)
lnMarker = RECNO()		&&Save record pointer to draw > shape.

BROWSE FIELDS cMarker = IIF(RECNO()=lnMarker ,'>',' '):H=' ':01:R :W =.F.,;
      cfld_head :H='Transaction' :75 :P= 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX' :W=.F.,;
       nSeq_no :H='Sequence #' :P= REPLICATE('9',nFld_Wdth) :20 ;
       :W= (laScrMode[3] OR laScrMode[4]) AND !lNoEdit        ;
       :F :V= lfValidSeq()   ;
       FOR !EMPTY(cComp_Id)  ;
       FREEZE nseq_no        ;
       WINDOW (lcWinCh1)     ;
       IN WINDOW (gcBaseWind);        
       WHEN lfwBrows()       ;
       VALID :F lfvBrowse()  ;
       LOCK 0;
       NOMENU;
       NOAPPEND;
       NODELETE;
       NOCLEAR;
       NOWAIT ;
       TITLE (lcBrowTitl)
*-- end of lfDispBrow.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : When Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrows()
*!*************************************************************
*!
FUNCTION lfwBrows
SELECT (lcTmpSeq)
lnMarker = RECNO()
SHOW WINDOW (lcBrowTitl) REFRESH

IF !laScrMode[1]
  lcDesc = &lcTmpSeq..mFld_Des
  SHOW GET lcDesc
  DO CASE

    CASE lnMarker = lnTop
      STORE 'DISABLE' TO lcTopBtn,lcPrvBtn
      IF lnSelRec > 1
        STORE 'ENABLE' TO lcBotBtn,lcNxtBtn
      ENDIF

    CASE lnMarker = lnBottom
      STORE 'DISABLE' TO lcBotBtn,lcNxtBtn
      IF lnSelRec > 1
        STORE 'ENABLE' TO lcTopBtn,lcPrvBtn
      ENDIF

    CASE (lnMarker > lnTop) AND (lnMarker < lnBottom)
      STORE 'ENABLE' TO lcTopBtn,lcPrvBtn,lcBotBtn,lcNxtBtn
  ENDCASE
  =lfShowBtns()
ENDIF  
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfvBrowse
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Valid Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrowse()
*!*************************************************************
*!
FUNCTION lfvBrowse
IF WONTOP() # (lcBrowTitl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfvBrowse.

*!*************************************************************
*! Name      : lfValidSeq
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Valid Entered sequence Number.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfValidSeq()
*!*************************************************************
*!
FUNCTION lfValidSeq

IF nSeq_no <> nOldSeq
  = gfUpDate()
ENDIF  

IF (laScrMode[3] OR laScrMode[4]) AND llDivBased AND ;
   !('ALL' $ laDivArray[lnGroup,1]) AND (nSeq_no <> nOldSeq)
  PRIVATE lcAlias
  lcAlias = ALIAS()

  *-- Scan the same sequences in another groups if based on division.
  SELECT Seq_File
  SCAN FOR cseq_type+cseq_group = &lcTmpSeq..cFld_Name AND (cseq_group <> &lcTmpSeq..cseq_group)
    IF ABS(&lcTmpSeq..nSeq_No - nSeq_No) < 10000
      *-- Message : Another XXX Transaction in group YYY occupied the same sequence range,
      *-- this sequence may be redundant in the future.
      *--                                   <Ok>
      =gfModalGen('INM00337B00000','DIALOG',ALLTRIM(&lcTmpSeq..cFld_Head) + '|' + ALLTRIM(cseq_group))
      EXIT
    ENDIF
  ENDSCAN
  SELECT (lcAlias)
  REPLACE nOldSeq WITH nSeq_no
ENDIF  
RETURN(.T.)
*-- end of lfValidSeq.


*!*************************************************************
*! Name      : lfTrap
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Trap function for Browse....
*!           : Deactivate screen function.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpTab,lpBackTab
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfTrap

*-- if TOP window is one of the browse window
IF WONTOP() = (lcBrowTitl)

  glFromBrow = .T.
  ON KEY LABEL CTRL+ENTER lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+HOME  lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+W     lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+END   lnDummy = 1		&&Do nothing

  ON KEY LABEL TAB           DO lpTab
  ON KEY LABEL CTRL+TAB      DO lpTab
  ON KEY LABEL BACKTAB       DO lpBackTab

ENDIF
*-- end of lfTrap.

*!*************************************************************
*! Name      : lfUnTrap
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/20/98
*! Purpose   : UnTrap function for Browse
*!           : Activate screen function....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfUnTrap
* -- Clear Trap
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- if TOP window is not one of the browses window                                        
IF WONTOP() # (lcBrowTitl)
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL CTRL+TAB
  ON KEY LABEL CTRL+ENTER
  ON KEY LABEL CTRL+HOME
  ON KEY LABEL CTRL+END
  ON KEY LABEL CTRL+W
ENDIF
*-- end of lfUnTrap.

*!*************************************************************
*! Name      : lpTab
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/20/98
*! Purpose   : Trapping TAB order for browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
PROCEDURE lpTab
ON KEY LABEL TAB
ON KEY LABEL CTRL+TAB

*-- Go to next window
IF laScrMode[1]

  ACTIVATE WINDOW gwcContrl1		&& Activate control panel window
  _CUROBJ = OBJNUM(pbTop)

ELSE

  ACTIVATE WINDOW (lcWinCh2)		&& Activate Description window
  _CUROBJ = OBJNUM(lcDesc)

ENDIF
*-- end of lpTab.

*!*************************************************************
*! Name      : lpBackTab
*! Developer : Mohamed Badran (MAB)
*! Date      : 01/20/98
*! Purpose   : Trapping BACKTAB order for browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpBackTab
*!*************************************************************
PROCEDURE lpBackTab
ON KEY LABEL BACKTAB

IF laScrMode[1]
  *-- Go to previous window
  ACTIVATE WINDOW (lcWinCh0)		&&Activate popup windows
  _CUROBJ = OBJNUM(lnCompany)
ELSE
  ACTIVATE WINDOW gwcContrl1		&& Activate control panel window
  _CUROBJ = OBJNUM(pbCls)
ENDIF  

*-- end of lpBackTab.

*!*************************************************************
*! Name      : lpShow
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Show gets screen procedure.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfFillData,lfwBrows
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpShow
*!*************************************************************
*!
PROCEDURE lpShow
laCtrStat[10] = 'DISABLE'
SHOW GET pbBrws &laCtrStat[10]

*-- if selected is not based on division fields.

*B603917,1 HBG 10/08/2000 Disabel Edit and Delete buttons if Division is N/A [Begin]
*IF ('ALL' $ laDivArray[lnGroup,1]) AND laScrMode[2]
*  SHOW GET pbDlt DISABLE
*ENDIF
IF "N/A" $ laDivArray[lnGroup,1]
  SHOW GET pbDlt DISABLE
  SHOW GET pbEdt DISABLE
ELSE
  IF ('ALL' $ laDivArray[lnGroup,1]) AND laScrMode[2]
    SHOW GET pbDlt DISABLE
  ENDIF
ENDIF
*B603917,1 [End]

DO CASE

  CASE laScrMode[1]  && Select Mode

    *B602906,4 Do this block if company based on division only. [Begin]
    IF llDivBased
      *-- User Come from add mode by press cancel [begin]
      IF llNewMode AND !('ALL' $ laDivArray[lnGroup,1]) AND ALEN(laDivArray,1) > 2 AND ;
         !EMPTY(&lcTmpSeq..cSeq_Type) AND ;
         !SEEK(PADR(&lcTmpSeq..cSeq_Type,10)+&lcTmpSeq..cSeq_Group,'Seq_File')
        DIMENSION laDivArray[ALEN(laDivArray,1) - 1,1]
        llNewGrp  = .T.
      ENDIF
      *-- User Come from add mode by press cancel [end]
    
    ELSE  && not based on division code
      *-- Select Company is defaulted.
      lnCompany = 1
      laDivArray[1,1] = "N/A"
    ENDIF  
    *B602906,4 Do this block if company based on division only. [Begin]

    *B603094,1 Disable New Group flag [Begin]
    llNewMode = .F.
    *B603094,1 Disable New Group flag [End  ]

    llVldComp = .T.
    STORE 'ENABLE' TO lcCompStat,lcDivStat
    lnGroup   = 1
    lcDesc = ''

    SHOW GET lnCompany &lcCompStat
    SHOW GET ibCompany &lcCompStat
    SHOW GET lnGroup   &lcDivStat
    SHOW GET lcDesc
    _CUROBJ = OBJNUM(lnCompany) 

  CASE laScrMode[2]  && View Mode
    
    IF llFromEdit
      = lfFillData()
      llFromEdit = .F.
    ENDIF  

  CASE laScrMode[3]  && Edit Mode

    llFromEdit = .T.

  CASE laScrMode[4]  && Add Mode

    STORE 'DISABLE' TO lcCompStat,lcDivStat

    IF ALEN(laDivArray,1) = 2
      lcCurrGrp = '001'
    ELSE
      lcCurrGrp = PADL(ALLTRIM(STR(VAL(laDivArray[ALEN(laDivArray,1),1])+1)),3,'0')
    ENDIF
   
    DECLARE laDivArray [ALEN(laDivArray,1) + 1 , 1] 
    laDivArray [ALEN(laDivArray,1),1] = lcCurrGrp

    lnGroup = ALEN(laDivArray,1)
    SHOW GET lnCompany &lcCompStat
    SHOW GET lnGroup &lcDivStat
    
    = lfFillData(.T.)
    llNewGrp  = llDivBased

    *B603094,1 Enable New Group flag [Begin]
    llNewMode = llNewGrp
    *B603094,1 Enable New Group flag [End  ]

ENDCASE

IF llDivBased AND llNewGrp AND (laScrMode[2] OR (laScrMode[1] AND lnCompany <> 1 ))
  lcNewBtn = 'ENABLE'
ELSE
  lcNewBtn = 'DISABLE'
ENDIF
SHOW GET pbCpNew &lcNewBtn

=lfwBrows()
*-- end of lpshow.

*!*************************************************************
*! Name      : lfWOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Current get old value
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : None
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWOldVal()
*!*************************************************************
*!
FUNCTION lfWOldVal
lcOldValue = EVALUATE(SYS(18))
*-- end of lfWOldVal.

*!*************************************************************
*! Name      : lfvCompany
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Validation code for select company popup.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfSelGroup
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvCompany()
*!*************************************************************
*!
FUNCTION lfvCompany
llVldComp = .F.
IF (_CUROBJ = OBJNUM(lnCompany) ) OR (lnCompany <> lcOldValue)
  =lfDivInfo()
ENDIF
IF lnCompany > 1
  = lfSelGroup()
ENDIF  
*-- end of lfvCompany.

*!*************************************************************
*! Name      : lfDivInfo
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Selected division information.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfGetMemVar
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfDivInfo()
*!*************************************************************
*!
FUNCTION lfDivInfo

*-- if it's select company item
IF lnCompany = 1

  DECLARE laDivArray [1 , 1] 
  laDivArray[1,1] = ' N/A '

ELSE  && User select specific company.

  *-- Open Default Company Sequence file.
  lcCompCode = PADR(laCompany[lnCompany,1],2)
  lcCompDir  = LOWER(ALLTRIM(laCompany[lnCompany,2]))
  lcCompMod  = laCompany[lnCompany,3]
  llDivBased = (gfGetMemVar('M_DIV_SEQ',lcCompCode) = 'Y')
  
  *B602906,4 if not based on division default is All. [Begin]
  *DECLARE laDivArray [2 , 1] 
  *laDivArray[1,1] = ' N/A '
  *laDivArray[2,1] = 'ALL'
  IF llDivBased
    DECLARE laDivArray [2 , 1] 
    laDivArray[1,1] = ' N/A '
    laDivArray[2,1] = 'ALL'
  ELSE
    DECLARE laDivArray [1 , 1] 
    laDivArray[1,1] = 'ALL'
  ENDIF  
  *B602906,4 if not based on division default is All. [End  ]

  *E301077,26 Close Sequence file oppened in another alias. [Begin]
  *IF USED('Seq_File')
  *  USE IN Seq_File
  *ENDIF
  =lfClos_Fil('Seq_File')
  *E301077,26 Close Sequence file oppened in another alias. [Begin]

  *-- Open sequence file in another alias to support multi companies system.
  *E301077,26 Open File calling a function open it. [Begin]
  *USE (lcCompDir+'SEQUENCE') IN 0 AGAIN ALIAS Seq_File ORDER TAG Cseq_type  
  =lfOpen_Fil(lcCompDir+'SEQUENCE',lcCompDir+'Cseq_type','Seq_File','SH')  && Open Current Company Sequence file.
  *E301077,26 Open File calling a function open it. [End  ]

  *B602906,4 add if block avoid do any thing if company not based on division. [Begin]
  IF llDivBased
    *-- Collect all division group data from sequence file. [begin]
    SELECT Seq_File
    SET RELATION TO cSeq_Type INTO (lcTemplate)

    SCAN FOR !(&lcTemplate..lPerComp OR EOF(lcTemplate))

      IF EMPTY(cSeq_Group)  
 
        llEmptyGrp   = .T.
        lcSeq_Grp    = '001'

      ELSE

        lcSeq_Grp = PADR(cSeq_Group,3)

      ENDIF

      IF ASCAN(laDivArray,lcSeq_Grp,1)  = 0
        DECLARE laDivArray [ALEN(laDivArray,1) + 1 , 1] 
        laDivArray [ALEN(laDivArray,1) , 1] = lcSeq_Grp
      ENDIF

    ENDSCAN

    SET RELATION OFF INTO (lcTemplate)
    *-- Collect all division group data from sequence file. [end]
  ENDIF  
  *B602906,4 add if block avoid do any thing if company not based on division. [End  ]

  llFindPrev = ALEN(laDivArray,1) > 2  && yes i find previous group.

  llNewGrp   = llDivBased OR !(llFindPrev OR llEmptyGrp)
  llNewMode  = llNewGrp

  lcNewBtn   = IIF(llNewGrp,'ENABLE','DISABLE')
  ACTIVATE WINDOW gwcContrl1		&& Activate control panel window
  SHOW GET pbCpNew &lcNewBtn

ENDIF    && end if it's select company item.
*-- end of lfDivInfo.

*!*************************************************************
*! Name      : lfSelGroup
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Select group1.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfFillData
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfSelGroup()
*!*************************************************************
*!
FUNCTION lfSelGroup
*-- Default Select mode value.
lnGroup   = 1

*B602906,4 if company not based on division, direct do group collection. [Begin]
*SHOW GET lnGroup &lcDivStat
*_CUROBJ   = OBJNUM(lnGroup)
IF llDivBased
  SHOW GET lnGroup &lcDivStat
  _CUROBJ   = OBJNUM(lnGroup)
ELSE
  =lfvGroup()
ENDIF  
*B602906,4 if company not based on division, direct do group collection. [End  ]
*-- end of lfSelGroup.

*!*************************************************************
*! Name      : lfvGroup
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Division group popup valid fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfFillData
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvGroup()
*!*************************************************************
*!
FUNCTION lfvGroup

*B602906,4 Also Switch mode if company not based on division. [Begin]
*IF lnGroup <> lcOldValue
IF !llDivBased OR (lnGroup <> lcOldValue)
*B602906,4 Also Switch mode if company not based on division. [End  ]
  laScrMode    = .F.
  laScrMode[2] = .T.
   = lfFillData()
  llFromEdit = .F.
  SHOW GETS
ENDIF  
*-- end of lfvGroup.

*!*************************************************************
*! Name      : lfOpenFls
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Open required files and set relation between them.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfSysOpen,gfCloseFile
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfOpenFls()
*!*************************************************************
*!
FUNCTION lfOpenFls
PRIVATE llFlFld,llFields,llFiles,lcFlFldOrd,lcFieldOrd,lcFilesOrd

STORE .F. TO llFlFld,llFields,llFiles          && Flags used in closing files.
STORE '' TO lcFlFldOrd,lcFieldOrd,lcFilesOrd   && variables used to set order of files.

IF USED('SYDFLFLD')
  lcFlFldOrd = ORDER('SYDFLFLD')
  SET ORDER TO TAG Cfld_name IN SYDFLFLD
ELSE
  llFlFld = gfOpenFile(gcSysHome+'SYDFLFLD',gcSysHome+'Cfld_name','SH')
ENDIF

IF USED('SYDFIELD')
  lcFieldOrd = ORDER('SYDFIELD')
  SET ORDER TO TAG Cfld_name IN SYDFIELD
ELSE
  llFields = gfOpenFile(gcSysHome+'SYDFIELD',gcSysHome+'Cfld_name','SH')
ENDIF

IF USED('SYDFILES')
  lcFilesOrd = ORDER('SYDFILES')
  SET ORDER TO TAG Cfile_nam IN SYDFILES
ELSE
  llFiles = gfOpenFile(gcSysHome+'SYDFILES',gcSysHome+'Cfile_nam','SH')
ENDIF
*E301077,26 Open files using gfSysOpen avoiding add new row to [End  ]

SELECT SYDFLFLD
SET RELATION TO cfld_name INTO Sydfield
SET RELATION TO cfile_nam INTO Sydfiles ADDITIVE

= lfFillTmpl()

SELECT SYDFLFLD
SET RELATION OFF INTO Sydfield
SET RELATION OFF INTO Sydfiles

*E301077,26 Close files using gfCloseFile [Begin]
IF llFlFld
  *USE IN SYDFLFLD
  =gfCloseFile('SYDFLFLD')
ELSE
  SET ORDER TO (lcFlFldOrd) IN SYDFLFLD
ENDIF
  
IF llFields
  *USE IN SYDFIELD
  =gfCloseFile('SYDFIELD')
ELSE
  SET ORDER TO (lcFieldOrd) IN SYDFIELD
ENDIF
  
IF llFiles
  *USE IN SYDFILES
  =gfCloseFile('SYDFILES')
ELSE
  SET ORDER TO (lcFilesOrd) IN SYDFILES
ENDIF
*E301077,26 Close files using gfCloseFile [End  ]
*-- end of lfOpenFls.

*!*************************************************************
*! Name      : lfFillTmpl
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Fill template cursor template data.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfFillTmpl()
*!*************************************************************
*!
FUNCTION lfFillTmpl

SELECT SYDFLFLD
*B802446,1 Ramy check if we added that line before or not [start]
*SCAN FOR lEnumerate AND !EOF("SYDFIELD")
SCAN FOR lEnumerate AND !EOF("SYDFIELD") .AND. !SEEK(PADR(cFld_Name,10) , lcTemplate)
*B802446,1 Ramy  [end]
  SCATTER MEMVAR
  m.cSeq_Type  = PADR(m.cFld_Name,10)
  m.nFld_Wdth  = SYDFIELD.nFld_Wdth
  m.cFld_Head  = IIF(cFld_Name = 'CPAYDOCNO','Bank Adjustments',SYDFIELD.cFld_Head)
  m.mFld_Des   = SYDFIELD.mFld_Des
  m.cData_Typ  = SYDFIELD.cData_Typ
  m.cFile_Tag  = SYDFILES.cFile_Tag
  m.lPerComp   = 'SEQPERCOMP' $ ALLTRIM(UPPER(SYDFIELD.mCodeInfo))
  m.lNoEdit    = 'DISABLESEQ' $ ALLTRIM(UPPER(SYDFIELD.mCodeInfo))
  m.cSeq_Group = IIF(m.lPerComp,'ALL','')
  m.mModExpr   = '.T.'
  
  STORE nDef_Seq TO m.nSeq_No,m.nOldSeq
  
  INSERT INTO (lcTemplate) FROM MEMVAR

ENDSCAN

*-- add hard coded records... [Begin]
DIMENSION laHardFlds[3,6]
laHardFlds[1,1] = 'CREDIT'
laHardFlds[2,1] = 'DEBIT'
laHardFlds[3,1] = 'CCLOSENT'

laHardFlds[1,2] = 'Credit Adjustments'
laHardFlds[2,2] = 'Debit Adjustments'
laHardFlds[3,2] = 'Closing Entry'

laHardFlds[1,3] = 6
laHardFlds[2,3] = 6
laHardFlds[3,3] = 8

laHardFlds[1,4] = 'C'
laHardFlds[2,4] = 'C'
laHardFlds[3,4] = 'C'

laHardFlds[1,5] = 'TF'
laHardFlds[2,5] = 'TF'
laHardFlds[3,5] = 'TF'

laHardFlds[1,6] = [('AR' $ lcCompMod) OR ('PS' $ lcCompMod)]
laHardFlds[2,6] = [('AR' $ lcCompMod) OR ('PS' $ lcCompMod)]
laHardFlds[3,6] = ['SM' $ lcCompMod]

FOR lnI = 1 TO ALEN(laHardFlds,1)

  STORE PADR(laHardFlds[lnI,1],10) TO m.cFld_Name,m.cSeq_Type
  m.cFld_Head  = laHardFlds[lnI,2]
  m.nFld_Wdth  = laHardFlds[lnI,3]
  m.cData_Typ  = laHardFlds[lnI,4]
  lcPerComp    = '.' + LEFT(laHardFlds[lnI,5],1) + '.'
  lcNoEdit     = '.' + RIGHT(laHardFlds[lnI,5],1) + '.'
  m.lPerComp   = EVALUATE(lcPerComp)
  m.lNoEdit    = EVALUATE(lcNoEdit)
  m.cSeq_Group = IIF(m.lPerComp,'ALL','')
  m.mModExpr   = laHardFlds[lnI,6]

  INSERT INTO (lcTemplate) FROM MEMVAR

ENDFOR
*-- add hard coded records... [End]
*-- end of lfFillTmpl.

*!*************************************************************
*! Name      : lfFillData
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Fill temp. file with selected company division data.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfFillData()
*!*************************************************************
*!
FUNCTION lfFillData
PARAMETERS llAddMode

PRIVATE lcGrpCode

lnSelRec = 0
*-- Collect Sequence lines for current company.

*B602906,4 Empty group if ALL or not based on division values. [Begin]
*lcGrpCode = IIF('ALL' $ laDivArray[lnGroup,1] ,'   ' , laDivArray[lnGroup,1])
lcGrpCode = IIF(!llDivBased OR ('ALL' $ laDivArray[lnGroup,1]) ,'   ' , laDivArray[lnGroup,1])
*B602906,4 Empty group if ALL or not based on division values. [End  ]

*-- If you find this selection data before.
IF SEEK(lcCompCode+laDivArray[lnGroup,1],lcTmpSeq)

  *-- Only adjust sequences if another user change it.  
  SELECT (lcTmpSeq)
  SCAN REST FOR cComp_Id + cSeq_Group + cFld_Name = lcCompCode + laDivArray[lnGroup,1]

    lnSelRec = lnSelRec + 1
    IF SEEK(cFld_Name+IIF(llEmptyGrp,'  ',lcGrpCode),'Seq_File')
    
      REPLACE nSeq_No WITH Seq_File.nSeq_No ,;
              nOldSeq WITH Seq_File.nSeq_No
    
    ENDIF
  
  ENDSCAN

ELSE  && else first time you enter selection locations.

  *B603917,1 HBG IF Sequance table is used so there is a Company selected collect the data[Begin]
  IF USED('Seq_File')
  *B603917,1 [End]
    PRIVATE lcForExp

    *B602906,4 Scan all file if company not based on division. [Begin]
    *lcForExp = IIF(EMPTY(lcGrpCode),'lPerComp','!lPerComp') + ' AND EVALUATE(ALLTRIM(mModExpr))'
    IF llDivBased
      lcForExp = IIF(EMPTY(lcGrpCode),'lPerComp','!lPerComp') + ' AND EVALUATE(ALLTRIM(mModExpr))'
    ELSE
      lcForExp = 'EVALUATE(ALLTRIM(mModExpr))'
    ENDIF  
    *B602906,4 Scan all file if company not based on division. [End  ]

    SELECT (lcTemplate)
    SCAN FOR &lcForExp

      lnSelRec = lnSelRec + 1

      SCATTER MEMVAR MEMO
      m.cComp_Id   = lcCompCode
      m.cSeq_Group = laDivArray[lnGroup,1]
      *B040098,1 02/16/2006 Oms ZF sequence error  [START]
      m.cSeq_chr=chr(0)
      *B040098,1 02/16/2006 Oms ZF sequence error  [END]
      IF !llAddMode AND SEEK(cFld_Name+IIF(llEmptyGrp,'  ',lcGrpCode),'Seq_File')
        STORE Seq_File.nSeq_No TO m.nSeq_No,m.nOldSeq
        m.nSeqRec = RECNO('Seq_File')
      ENDIF

      INSERT INTO (lcTmpSeq) FROM MEMVAR
    ENDSCAN
    
    *B603917,1 HBG These lins moved here to fix bug of "End of file encountered" [Begin]
    SET FILTER TO cComp_Id + cSeq_Group + cFld_Name = lcCompCode + laDivArray[lnGroup,1]
    GO BOTTOM
    lnBottom = RECNO(lcTmpSeq)

    *B603917,1 [End]

  *B603917,1 HBG End IF Sequance table is used [Begin]
  ENDIF
  *B603917,1 [END]
ENDIF

SELECT (lcTmpSeq)

*B603917,1 HBG These lins moved up to fix bug of "End of file encountered" [Begin]
*SET FILTER TO cComp_Id + cSeq_Group + cFld_Name = lcCompCode + laDivArray[lnGroup,1]
*GO BOTTOM
*lnBottom = RECNO(lcTmpSeq)
*B603917,1 [End]


*B603917,1 HBG Chang GO TOP to LOCATE[Begin]
*GO TOP
LOCATE
*B603917,1 [END]

lnTop = RECNO(lcTmpSeq) 

STORE 'DISABLE' TO lcCompStat,lcDivStat
  
SHOW GET lnCompany &lcCompStat
SHOW GET ibCompany &lcCompStat
SHOW GET lnGroup &lcDivStat

KEYBOARD '{ALT+B}' CLEAR PLAIN  && Top screen is browse screen.
*-- end of lfFillData.

*!*************************************************************
*! Name      : lfShowBtns
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Show get navigation buttons.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfShowBtns()
*!*************************************************************
*!
FUNCTION lfShowBtns

SHOW GET pbTop  &lcTopBtn
SHOW GET pbPrvs &lcPrvBtn

SHOW GET pbBtm  &lcBotBtn
SHOW GET pbNxt  &lcNxtBtn
*-- end of lfShowBtns.

*!*************************************************************
*! Name      : lpSavScr
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Transfer sequence data to sequence file.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfAdd_Info
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpSavScr
*!*************************************************************
*!
PROCEDURE lpSavScr

PRIVATE lcGrpCode,lcCurrCode
lcGrpCode = IIF('ALL' $ laDivArray[lnGroup,1] ,'   ' , laDivArray[lnGroup,1])

lcCurrCode = &lcTmpSeq..cComp_Id + &lcTmpSeq..cSeq_Group + &lcTmpSeq..cFld_Name
 
= SEEK(lcCompCode+laDivArray[lnGroup,1],lcTmpSeq) 

*-- Scan current group in sequence file.  
SELECT (lcTmpSeq)

SCAN REST FOR cComp_Id + cSeq_Group + cFld_Name = lcCompCode + laDivArray[lnGroup,1]

  *-- Modify exist record.
  IF SEEK(cFld_Name+IIF(llEmptyGrp,'  ',lcGrpCode),'Seq_File')
    SELECT Seq_File

    =RLOCK()
    REPLACE cSeq_Group WITH lcGrpCode ,;
            nSeq_No WITH &lcTmpSeq..nSeq_No
    UNLOCK
    
  ELSE  && New Record.
    
    SCATTER MEMVAR
    m.cSeq_Group = lcGrpCode
    INSERT INTO ('SEQ_FILE') FROM MEMVAR
    
  ENDIF

  SELECT Seq_File
  *-- update user information.
  
  =gfAdd_Info('SEQ_FILE')
  SELECT (lcTmpSeq)

ENDSCAN

= SEEK(lcCurrCode,lcTmpSeq) 

llEmptyGrp = .F.
llFindPrev = .T.
llNewMode  = .F.
*-- end of lpSavScr.

*!*************************************************************
*! Name      : lpDelScr
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/02/98
*! Purpose   : Delete current group data from sequence file.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpDelScr
*!*************************************************************
*!
FUNCTION lpDelScr
PRIVATE llDelIt,lcAlias,lcGrpCode
lcAlias = SELECT(0)
llDelIt = .T.

lcGrpCode = IIF(llEmptyGrp,'   ',laDivArray[lnGroup,1])

IF llEmptyGrp

  llEmptyGrp = .F.

ELSE

  *E301077,26 Open File calling a function open it. [Begin]
  *USE (lcCompDir+'CODES') IN 0 AGAIN ALIAS Code_File ORDER TAG Idrltfname  && Current Company Codes file.
  =lfOpen_Fil(lcCompDir+'CODES',lcCompDir+'Idrltfname','Code_File','SH')  && Open Current Company Codes file.
  *E301077,26 Open File calling a function open it. [End  ]
  *E300789,1 Hesham (Start)
  *IF SEEK(lcCompCode+'Y'+'CDIVISION','Code_File')
  IF SEEK('N'+'Y'+'CDIVISION','Code_File')
  *E300789,1 Hesham (Start)
    SELECT Code_File
    LOCATE REST FOR (CRLTD_NAM = 'DIVGROUP  ') AND (ALLTRIM(laDivArray[lnGroup,1]) $ cRltd_Vlu)
    IF FOUND()
      llDelIt = .F.
      *-- Message : One or more Division(s) have been assigned to this group. Cannot remove.
      *--                                     <Ok>
      =gfModalGen('INM00338B00000','DIALOG')
    ENDIF
  ENDIF

ENDIF


*-- Delete current division group.
IF llDelIt
  
  *-- Delete data from sequence file.
  SELECT Seq_File
  BLANK FOR cSeq_Group = ALLTRIM(lcGrpCode)
  DELETE FOR EMPTY(cSeq_Type)
  GO TOP IN Seq_File

  *-- delete data from temp. sequence file.
  SELECT (lcTmpSeq)
  BLANK FOR cComp_Id + cSeq_Group + cFld_Name = lcCompCode + ALLTRIM(laDivArray[lnGroup,1])
  
  *-- remove array element
  = ADEL(laDivArray,lnGroup)
  DIMENSION laDivArray[ALEN(laDivArray,1)-1,1]

  lnGroup = 1
  IF ALEN(laCompany,1) = 1
    lcDivStat = 'ENABLE'
  ENDIF  
  SHOW GET lnGroup &lcDivStat
  
  IF ALEN(laDivArray,1) = 2
    llNewGrp  = .T.
    llNewMode = llNewGrp
  ENDIF

  laScrMode    = .F.
  laScrMode[1] = .T.
  SHOW GETS

ENDIF

*E301077,26 Close Codes file oppened in another alias. [Begin]
*IF USED('Code_File')
*  USE IN Code_File
*ENDIF  
=lfClos_Fil('Code_File')
*E301077,26 Close Codes file oppened in another alias. [End  ]

SELECT (lcAlias)
*-- end of lpDelScr.

*!*************************************************************
*! Name      : lfOpen_Fil
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/01/99
*! Purpose   : Open Files in another alias name
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfOpen_Fil()
*!*************************************************************
*!
FUNCTION lfOpen_Fil
PARAMETERS lcFileNam,lcTagNam,lcAliasNam,lcOpnStat
*-- Open file in another alias name.

*E301077,26 Open files in another alias using gfOpenFile [Begin
*USE (lcFileNam) ORDER TAG (lcTagNam) AGAIN ALIAS (lcAliasNam) IN 0  &lcOpnStat
=gfOpenFile(lcFileNam,lcTagNam,lcOpnStat,lcAliasNam)
*E301077,26 Open files in another alias using gfOpenFile [End

*-- end of lfOpen_Fil.

*!*************************************************************
*! Name      : lfClos_Fil
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/01/99
*! Purpose   : Close files oppened in another alias name.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfClos_Fil()
*!*************************************************************
*!
FUNCTION lfClos_Fil
PARAMETERS lcFileName
*E301077,26 Close files oppened in another alias using gfCloseFile [Begin
*IF USED(lcFileName)
*  USE IN (lcFileName)
*ENDIF
=gfCloseFile(lcFileName)
*E301077,26 Close files oppened in another alias using gfCloseFile [End

*-- end of lfClos_Fil.