*:************************************************************************
*: Program file  : ICCLSST.PRG
*: Program desc. : Classificaion Setup screen for GMA (C#102451)
*: For screen    : ICSTYLE.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Inventory Control (IC).
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
*:B605582,1 HBG 11/03/2002 - Enable all the drop down lists in view mode
*:B605582,1                - The drop down lists should filter only the value
*:B605582,1                  selected in the previous folder
*:C200505,1 TMI 02/20/2003 Fill arrays automatically
*:B608478,1 WAM 03/10/2008 Fix error too many variables
*:************************************************************************
EXTERNAL ARRAY laData , laDefProc
DIMENSION lafoldwinds[5,2],laDept[1],laGroup[1],laSCatgry[1,2],laTCatgry[1,2],;
          laSClass[1,2],laTClass[1,2],laSSCls1[1,2],laTSCls1[1,2],laSSCls2[1,2],laTSCls2[1,2],;
          laSSCls3[1,2],laTSCls3[1,2]

DIMENSION laCodInf1[1,10]   && Codes array for scound Folder
DIMENSION laCodInf2[2,10]   && Codes array for third  Folder
DIMENSION laCodInf3[3,10]   && Codes array for forth  Folder
DIMENSION laCodInf4[4,10]   && Codes array for fifth  Folder
STORE SPACE(0) TO laCodInf1,laCodInf2,laCodInf3,laCodInf4

DIMENSION  laCat1[1,2],laCat2[1,2],laCat3[1,2],laCat4[1,2] && Popup arrays for Categories for the different folders
DIMENSION  laCls1[1,2],laCls2[1,2],laCls3[1,2]           && Popup arrays for Class for the different folders
DIMENSION  laSCls11[1,2],laSCls12[1,2]                 && Popup arrays for Sub Class 1 for the different folders
DIMENSION  laSCls21[1,2]                             && Popup arrays for Sub Class 2 for the different folders

STORE ''  TO laCat1,laCat2,laCat3,laCat4,laCls1,laCls2,laCls3,laSCls11,laSCls12,laSCls21
STORE ''  TO lcCat1,lcCat2,lcCat3,lcCat4,lcCls1,lcCls2,lcCls3,lcSCls11,lcSCls12,lcSCls21
STORE 1   TO lnCat1,lnCat2,lnCat3,lnCat4,lnCls1,lnCls2,lnCls3,lnSCls11,lnSCls12,lnSCls21


STORE 1   TO lnActFolder,lncurobj,lnGrp,lnDept,lsSCatgry,lsTCatgry,lsSClass,lsTClass,;
             lsSSCls1,lsTSCls1,lsSSCls2,lsTSCls2,lsSSCls3,lsTSCls3,lcOldValue
STORE ' ' TO lcfolder,lcfoldprnt,lafoldwinds,lcMajor,laGroup,laDept,;
             lcGroup,lcDept,lcDepDesc,lcGrpDesc,laSCatgry,laTCatgry,laSClass,laTClass,;
             laSSCls1,laTSCls1,laSSCls2,laTSCls2,laSSCls3,laTSCls3
STORE ' ' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh4,lcWinCh5

STORE '' TO lcWorkFile,lcTmGrpCop,lcScFields

STORE .T. TO llCUpdate  && flag to check If any object is updated 
                        && in global control panel close proc.
STORE .T. TO llFillPop
STORE .F. TO llBrowse


STORE .F. TO llSCatgD,llTCatgD,llSClassD,llTClassD,llSCls1D,llTCls1D,llSCls2D,llTCls2D,llSCls3D,llTCls3D  
llFld2 = .F.

STORE 'DISABLE' TO lcSCatgSt,lcTCatgSt,lcAllCtSt,lcMovCtSt,lcRemCtSt,lcRAlCtsSt
STORE 'DISABLE' TO lcSClasSt,lcTClasSt,lcAllClSt,lcMovClSt,lcRemClSt,lcRAlClsSt
STORE 'DISABLE' TO lcSSCl1St,lcTSCl1St,lcAlCs1St,lcMvCs1St,lcRmCs1St,lcRAlCs1St
STORE 'DISABLE' TO lcSSCl2St,lcTSCl2St,lcAlCs2St,lcMvCs2St,lcRmCs2St,lcRAlCs2St
STORE 'DISABLE' TO lcSSCl3St,lcTSCl3St,lcAlCs3St,lcMvCs3St,lcRmCs3St,lcRAlCs3St

*C200505,1 TMI [Start] "llMltSlct" is assigned .T. when adding new group to a dept and the user 
*C200505,1             need not to copy from another Group
llMltSlct = .F.
*C200505,1 TMI [End  ] 

*--Initialize the Folders array.
lcwfoldchng = '=lfActFolder()'  && function to control shows after change the folder
lcfoldpush  = 'pbFolder'        && push button name for the next folder
lnFolderCEnd= 103.00
lnfolderrend=   2.00

*--ToolBar external proc.
laDefProc[7]  = .F.              && Disable the control panel delete proc. (lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .F.              && Disable the control panel close proc. (lpClsScr)

IF !gfSetup()
  RETURN
ENDIF

lcScFields = 'dept,cstygroup,cstycatg,cstyclass,cstysbclas,cstysbcls2,cstysbcls3'

IF !WEXIST(gcBaseWind)
  lcWinCh0    = gfTempName()
  lcWinCh1    = gfTempName()
  lcWinCh2    = gfTempName()
  lcWinCh3    = gfTempName()
  lcWinCh4    = gfTempName()
  lcWinCh5    = gfTempName()
  lcfolder    = gfTempName()        && Folder Window Name
  lcfoldprnt  = gcBaseWind          && window parent name for the folder
  lnActFolder = 1                   && active folder

  lcWorkFile = gfTempName()
  lcTmGrpCop = gfTempName()
  
  SELECT ICCLSSTP
  =AFIELDS(laWorkArry)
  DIMENSION laIndx[1,2]
  laIndx[1,1] = 'dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3'
  laIndx[1,2] = lcWorkFile
  =gfCrtTmp(lcWorkFile,@laWorkArry,@laIndx)           
  
  SELECT ICCLSSTP
  =AFIELDS(laCpGrpAry)
  DIMENSION laIndx[1,2]
  laIndx[1,1] = 'dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3'
  laIndx[1,2] = lcTmGrpCop 
  =gfCrtTmp(lcTmGrpCop,@laCpGrpAry,@laIndx)     
  
  lafoldwinds[1,1] = 'Category'
  lafoldwinds[1,2] = lcWinCh1
  lafoldwinds[2,1] = 'Class'
  lafoldwinds[2,2] = lcWinCh2
  lafoldwinds[3,1] = 'Sub_Class1'
  lafoldwinds[3,2] = lcWinCh3
  lafoldwinds[4,1] = 'Sub_Class2'
  lafoldwinds[4,2] = lcWinCh4
  lafoldwinds[5,1] = 'Sub_Class3'
  lafoldwinds[5,2] = lcWinCh5

  *-- Intialize arrays of code popup
  =lfintCdpop()
  = gfwCodePop(@laCodInf1, "CSTYCATG","L" )
  = gfwCodePop(@laCodInf2, "CSTYCATG","L" )
  = gfwCodePop(@laCodInf3, "CSTYCATG","L" )
  = gfwCodePop(@laCodInf4, "CSTYCATG","L" )
  =lfintCdpop()
  = gfwCodePop(@laCodInf2, "CSTYCLASS","L" )
  = gfwCodePop(@laCodInf3, "CSTYCLASS","L" )
  = gfwCodePop(@laCodInf4, "CSTYCLASS","L" )
  =lfintCdpop()
  = gfwCodePop(@laCodInf3, "CSTYSBCLAS","L" )
  = gfwCodePop(@laCodInf4, "CSTYSBCLAS","L" )
  =lfintCdpop()
  = gfwCodePop(@laCodInf4, "CSTYSBCLS2","L" )
  
  laDept[1]  = 'N/A'
  laGroup[1] = 'N/A'

  =lfGetDept()          && Get the departments abd Groups
  =lfGetCatg()          && Get Categories
  =lfGetClas()          && Get Classes
  =lfGetCls1()          && Get Sub Classes 1
  =lfGetCls2()          && Get Sub Classes 2
  =lfGetCls3()          && Get Sub Classes 3
ENDIF

laCtrStat[10] = "DISABLE"
DO (gcScrDir+gcWinAppl+"\ICCLSST.SPX")
POP KEY


*!*************************************************************
*! Name      : lpshow
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Show Screen objects when Mode Changes.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lpShow()
*!*************************************************************
FUNCTION lpShow
PRIVATE lnPrevAl , lcPrevOrd


DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]  && Select mode. 
    STORE 'DISABLE' TO lcSCatgSt,lcTCatgSt,lcAllCtSt,lcMovCtSt,lcRemCtSt,lcRAlCtsSt
    STORE 'DISABLE' TO lcSClasSt,lcTClasSt,lcAllClSt,lcMovClSt,lcRemClSt,lcRAlClsSt
    STORE 'DISABLE' TO lcSSCl1St,lcTSCl1St,lcAlCs1St,lcMvCs1St,lcRmCs1St,lcRAlCs1St
    STORE 'DISABLE' TO lcSSCl2St,lcTSCl2St,lcAlCs2St,lcMvCs2St,lcRmCs2St,lcRAlCs2St
    STORE 'DISABLE' TO lcSSCl3St,lcTSCl3St,lcAlCs3St,lcMvCs3St,lcRmCs3St,lcRAlCs3St
    *--Initialize variables
    =lfIntVar()

    SHOW GET lcDepDesc
    SHOW GET lcGrpDesc
    SHOW GET lnDept ENABLE
    SHOW GET lnGrp  DISABLE 
     
    lnlastfold = lnactfolder
    lnActFolder = 1
    = lfChngFolder(lnActFolder)

    SHOW GET ibFolder[2] DISABLE 
    SHOW GET ibFolder[3] DISABLE 
    SHOW GET ibFolder[4] DISABLE 
    SHOW GET ibFolder[5] DISABLE 

  *B605582,1 HBG 11/03/2002 Enable all the drop down lists in view mode [Begin]
  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]  && View mode.   
    SHOW GET lnCat1   ENABLE
    SHOW GET lnCat2   ENABLE
    SHOW GET lnCat3   ENABLE
    SHOW GET lnCat4   ENABLE
    SHOW GET lnCls1   ENABLE
    SHOW GET lnCls2   ENABLE
    SHOW GET lnCls3   ENABLE     
    SHOW GET lnSCls11 ENABLE     
    SHOW GET lnSCls12 ENABLE          
    SHOW GET lnSCls21 ENABLE     
    STORE 'DISABLE' TO lcSCatgSt,lcTCatgSt,lcAllCtSt,lcMovCtSt,lcRemCtSt,lcRAlCtsSt
    STORE 'DISABLE' TO lcSClasSt,lcTClasSt,lcAllClSt,lcMovClSt,lcRemClSt,lcRAlClsSt
    STORE 'DISABLE' TO lcSSCl1St,lcTSCl1St,lcAlCs1St,lcMvCs1St,lcRmCs1St,lcRAlCs1St
    STORE 'DISABLE' TO lcSSCl2St,lcTSCl2St,lcAlCs2St,lcMvCs2St,lcRmCs2St,lcRAlCs2St
    STORE 'DISABLE' TO lcSSCl3St,lcTSCl3St,lcAlCs3St,lcMvCs3St,lcRmCs3St,lcRAlCs3St

    lnlastfold = lnactfolder
    lnActFolder = 1
    = lfChngFolder(lnActFolder)
    
    SHOW GET ibFolder[2] ENABLE 
    SHOW GET ibFolder[3] ENABLE 
    SHOW GET ibFolder[4] ENABLE 
    SHOW GET ibFolder[5] ENABLE 
  *B605582,1 [End]
  
  ***--- E D I T   M O D E ---***
  CASE laScrMode[3]  && Edit mode. 
    STORE 'ENABLE' TO lcSCatgSt,lcTCatgSt
    STORE 'ENABLE' TO lcSClasSt,lcTClasSt
    STORE 'ENABLE' TO lcSSCl1St,lcTSCl1St
    STORE 'ENABLE' TO lcSSCl2St,lcTSCl2St
    STORE 'ENABLE' TO lcSSCl3St,lcTSCl3St
    
    IF llSCatgD
      STORE 'DISABLE' TO lcAllCtSt,lcMovCtSt
      llSCatgD= .F.
    ELSE 
      STORE 'ENABLE' TO lcAllCtSt,lcMovCtSt 
    ENDIF  

    IF llTCatgD 
      STORE 'DISABLE' TO lcRemCtSt,lcRAlCtsSt
      llTCatgD = .F.
    ELSE
      STORE 'ENABLE' TO lcRemCtSt,lcRAlCtsSt
    ENDIF  
    
    IF llSClassD 
      STORE 'DISABLE' TO lcAllClSt,lcMovClSt
      llSClassD = .F.
    ELSE 
      STORE 'ENABLE' TO lcAllClSt,lcMovClSt  
    ENDIF  
    IF llTClassD 
      STORE 'DISABLE' TO lcRemClSt,lcRAlClsSt
      llTClassD = .F.
    ELSE
      STORE 'ENABLE' TO lcRemClSt,lcRAlClsSt  
    ENDIF  

    IF llSCls1D 
      STORE 'DISABLE' TO lcAlCs1St,lcMvCs1St
      llSCls1D = .F.
    ELSE
      STORE 'ENABLE' TO lcAlCs1St,lcMvCs1St  
    ENDIF  
    IF llTCls1D 
      STORE 'DISABLE' TO lcRmCs1St,lcRAlCs1St
      llTCls1D = .F.
    ELSE
      STORE 'ENABLE' TO lcRmCs1St,lcRAlCs1St  
    ENDIF  

    IF llSCls2D 
      STORE 'DISABLE' TO lcAlCs2St,lcMvCs2St
      llSCls2D = .F.
    ELSE
      STORE 'ENABLE' TO lcAlCs2St,lcMvCs2St
    ENDIF  
    IF llTCls2D 
      STORE 'DISABLE' TO lcRmCs2St,lcRAlCs2St
      llTCls2D = .F.
    ELSE
      STORE 'ENABLE' TO lcRmCs2St,lcRAlCs2St
    ENDIF  

    IF llSCls3D 
      STORE 'DISABLE' TO lcAlCs3St,lcMvCs3St
      llSCls3D = .F.
    ELSE
      STORE 'ENABLE' TO lcAlCs3St,lcMvCs3St
    ENDIF  
    IF llTCls3D 
      STORE 'DISABLE' TO lcRmCs3St,lcRAlCs3St
      llTCls3D = .F.
    ELSE
      STORE 'ENABLE' TO lcRmCs3St,lcRAlCs3St
    ENDIF  
    
    STORE .T. TO llCUpdate  && flag to check If any object is updated 
                            && in global control panel close proc.

    SHOW GETS WINDOW (lcWinCh0) DISABLE ONLY
  
    = lfChngFolder(lnActFolder)
    SHOW GET ibFolder[2] ENABLE 
    SHOW GET ibFolder[3] ENABLE 
    SHOW GET ibFolder[4] ENABLE 
    SHOW GET ibFolder[5] ENABLE 

ENDCASE

RETURN

*!*************************************************************
*! Name      : lfReadAct
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : READ Activate function of INV700
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfReadAct

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

IF laScrMode[1]
  SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  SHOW GET ibFolder[1] DISABLE
  SHOW GET ibFolder[2] DISABLE
  SHOW GET ibFolder[3] DISABLE
  SHOW GET ibFolder[4] DISABLE
  SHOW GET ibFolder[5] DISABLE
ELSE
  SHOW GET ibFolder[1] ENABLE
  SHOW GET ibFolder[2] ENABLE
  SHOW GET ibFolder[3] ENABLE
  SHOW GET ibFolder[4] ENABLE
  SHOW GET ibFolder[5] ENABLE
ENDIF


*:*************************************************************
*! Name      : lfActFolder
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Activate folder when Change.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfActFolder()
*!*************************************************************
FUNCTION lfActFolder

lnAlias=SELECT()

DO CASE
  *--First Folder < Category > ----------------------------
  CASE lnactfolder = 1
    SHOW WINDOW (lcWinCh1) TOP
    IF laScrMode[3] OR laScrMode[4]
      SHOW GETS WINDOW (lcWinCh1) ENABLE ONLY
      
      llSCatgD= .T.
      FOR lnI = 1 TO ALEN(laSCatgry,1)
        IF PADL(laSCatgry[lnI,1],1) <> '\'
          llSCatgD = .F.
          EXIT
        ENDIF
      ENDFOR
      
      IF llSCatgD    
        STORE 'DISABLE' TO lcAllCtSt,lcMovCtSt
        llSCatgD= .F.
      ELSE 
        STORE 'ENABLE' TO lcAllCtSt,lcMovCtSt 
      ENDIF  

      IF ALEN(laTCatgry,1) = 1 AND laTCatgry[1,1] = ' ' 
        STORE 'DISABLE' TO lcRemCtSt,lcRAlCtsSt
      ELSE
        STORE 'ENABLE' TO lcRemCtSt,lcRAlCtsSt
      ENDIF  

      SHOW GET lsSCatgry  &lcSCatgSt
      SHOW GET lsTCatgry  &lcTCatgSt
      SHOW GET pbAllCat   &lcAllCtSt
      SHOW GET pbMovCat   &lcMovCtSt
      SHOW GET pbRemCat   &lcRemCtSt
      SHOW GET pbRAllCat  &lcRAlCtsSt
    ENDIF  
    =lfRefresh(lcWinCh1)

  *--Second Folder < Class > ------------------------------
  CASE lnactfolder = 2
    SHOW WINDOW (lcWinCh2) TOP
    *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
    *B605582,1               selected in the previous folder [Begin]
    =lfCatgPop()
    *B605582,1 [End]
    
    IF laScrMode[3]  OR laScrMode[4]
      SHOW GETS WINDOW (lcWinCh2) ENABLE ONLY
      
      llSClassD = .T.
      FOR lnI = 1 TO ALEN(laSClass,1)
       IF PADL(laSClass[lnI,1],1) <> '\'
          llSClassD = .F.
          EXIT
        ENDIF
      ENDFOR
      
      IF lnCat1 = 1 OR ASCAN(laTCatgry,laCat1[lnCat1,2]) = 0
        llSClassD = .T.
        STORE 'DISABLE' TO lcSClasSt,lcTClasSt
      ELSE
        llSClassD = .F.
        STORE 'ENABLE' TO lcSClasSt,lcTClasSt
      ENDIF  
      
      IF llSClassD 
        STORE 'DISABLE' TO lcAllClSt,lcMovClSt
        llSClassD = .F.
      ELSE 
        STORE 'ENABLE' TO lcAllClSt,lcMovClSt
      ENDIF  

      IF ALEN(laTClass,1) = 1 AND laTClass[1,1] = ' '
        STORE 'DISABLE' TO lcRemClSt,lcRAlClsSt
      ELSE
        STORE 'ENABLE' TO lcRemClSt,lcRAlClsSt
      ENDIF  

      SHOW GET lsSClass  &lcSClasSt
      SHOW GET lsTClass  &lcTClasSt
      SHOW GET pbAllCls  &lcAllClSt
      SHOW GET pbMovCls  &lcMovClSt
      SHOW GET pbRemCls  &lcRemClSt
      SHOW GET pbRAllCls &lcRAlClsSt
    ENDIF  
    *B605582,1 HBG 11/03/2002 Enable all the drop down lists in view mode and disable the movers[Begin]
    IF laScrMode[2]
      SHOW GET lnCat1    ENABLE
      STORE 'DISABLE' TO lcSClasSt,lcTClasSt,lcAllClSt,lcMovClSt,lcRemClSt,lcRAlClsSt
      SHOW GET lsSClass  &lcSClasSt
      SHOW GET lsTClass  &lcTClasSt
      SHOW GET pbAllCls  &lcAllClSt
      SHOW GET pbMovCls  &lcMovClSt
      SHOW GET pbRemCls  &lcRemClSt
      SHOW GET pbRAllCls &lcRAlClsSt
    ENDIF
    *B605582,1 [End]
    =lfRefresh(lcWinCh2)
     
  *--Therd Folder < Sub_Class1 > --------------------------
  CASE lnactfolder = 3
    SHOW WINDOW (lcWinCh3) TOP
    
    *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
    *B605582,1               selected in the previous folder [Begin]
    =lfCatgPop()
    =lfClasPop()
    *B605582,1 [End]
    
    IF laScrMode[3]  OR laScrMode[4]
      SHOW GETS WINDOW (lcWinCh3) ENABLE ONLY

      llSCls1D = .T.
      FOR lnI = 1 TO ALEN(laSSCls1,1)
        IF PADL(laSSCls1[lnI,1],1) <> '\'
          llSCls1D = .F.
          EXIT
        ENDIF
      ENDFOR
      
      IF lnCls1 = 1 OR ASCAN(laTClass,laCls1[lnCls1,2]) = 0
        llSCls1D = .T.
        STORE 'DISABLE' TO lcSSCl1St,lcTSCl1St
      ELSE
        llSCls1D = .F.
        STORE 'ENABLE' TO lcSSCl1St,lcTSCl1St
      ENDIF  

      IF llSCls1D 
        STORE 'DISABLE' TO lcAlCs1St,lcMvCs1St
        llSCls1D = .F.
      ELSE 
        STORE 'ENABLE' TO lcAlCs1St,lcMvCs1St
      ENDIF  

      IF ALEN(laTSCls1,1) = 1 AND laTSCls1[1,1] = ' ' 
        STORE 'DISABLE' TO lcRmCs1St,lcRAlCs1St
      ELSE
        STORE 'ENABLE' TO lcRmCs1St,lcRAlCs1St
      ENDIF  
            
      SHOW GET lsSSCls1  &lcSSCl1St
      SHOW GET lsTSCls1  &lcTSCl1St
      SHOW GET pbAllCs1  &lcAlCs1St
      SHOW GET pbMovCs1  &lcMvCs1St
      SHOW GET pbRemCs1  &lcRmCs1St
      SHOW GET pbRAllCs1 &lcRAlCs1St
    ENDIF  
    *B605582,1 HBG 11/03/2002 Enable all the drop down lists in view mode and disable the movers[Begin]    
    IF laScrMode[2]
      SHOW GET lnCat2   ENABLE
      SHOW GET lnCls1   ENABLE
      STORE 'DISABLE' TO lcSSCl1St,lcTSCl1St,lcAlCs1St,lcMvCs1St,lcRmCs1St,lcRAlCs1St
      SHOW GET lsSSCls1  &lcSSCl1St
      SHOW GET lsTSCls1  &lcTSCl1St
      SHOW GET pbAllCs1  &lcAlCs1St
      SHOW GET pbMovCs1  &lcMvCs1St
      SHOW GET pbRemCs1  &lcRmCs1St
      SHOW GET pbRAllCs1 &lcRAlCs1St
    ENDIF
    *B605582,1 [End]
    =lfRefresh(lcWinCh3) 
    
  *--Fourth Folder < Sub_Class2 > -------------------------
  CASE lnactfolder = 4
    SHOW WINDOW (lcWinCh4) TOP
    *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
    *B605582,1               selected in the previous folder [Begin]
    =lfCatgPop()
    =lfClasPop()
    =lfSCls1Pop()
    *B605582,1 [End]

    IF laScrMode[3]  OR laScrMode[4]
      SHOW GETS WINDOW (lcWinCh4) ENABLE ONLY

      llSCls2D = .T.
      FOR lnI = 1 TO ALEN(laSSCls2,1)
        IF PADL(laSSCls2[lnI,1],1) <> '\'
          llSCls2D = .F.
          EXIT
        ENDIF
      ENDFOR

      IF lnSCls11 = 1 OR ASCAN(laTSCls1,laSCls11[lnSCls11,2]) = 0
        llSCls2D = .T.
        STORE 'DISABLE' TO lcSSCl2St,lcTSCl2St
      ELSE
        llSCls2D = .F.
        STORE 'ENABLE' TO lcSSCl2St,lcTSCl2St
      ENDIF  
            
      IF llSCls2D 
        STORE 'DISABLE' TO lcAlCs2St,lcMvCs2St
        llSCls2D = .F.
      ELSE 
        STORE 'ENABLE' TO lcAlCs2St,lcMvCs2St
      ENDIF  

      IF ALEN(laTSCls2,1) = 1 AND laTSCls2[1,1] = ' ' 
        STORE 'DISABLE' TO lcRmCs2St,lcRAlCs2St
      ELSE
        STORE 'ENABLE' TO lcRmCs2St,lcRAlCs2St
      ENDIF  

      SHOW GET lsSSCls2  &lcSSCl2St
      SHOW GET lsTSCls2  &lcTSCl2St
      SHOW GET pbAllCs2  &lcAlCs2St
      SHOW GET pbMovCs2  &lcMvCs2St
      SHOW GET pbRemCs2  &lcRmCs2St
      SHOW GET pbRAllCs2 &lcRAlCs2St

    ENDIF  
    *B605582,1 HBG 11/03/2002 Enable all the drop down lists in view mode and disable the movers[Begin]
    IF laScrMode[2]
      SHOW GET lnCat3   ENABLE
      SHOW GET lnCls2   ENABLE
      SHOW GET lnSCls11 ENABLE     
      STORE 'DISABLE' TO lcSSCl2St,lcTSCl2St,lcAlCs2St,lcMvCs2St,lcRmCs2St,lcRAlCs2St
      SHOW GET lsSSCls2  &lcSSCl2St
      SHOW GET lsTSCls2  &lcTSCl2St
      SHOW GET pbAllCs2  &lcAlCs2St
      SHOW GET pbMovCs2  &lcMvCs2St
      SHOW GET pbRemCs2  &lcRmCs2St
      SHOW GET pbRAllCs2 &lcRAlCs2St
    ENDIF
    *B605582,1 [End]
    =lfRefresh(lcWinCh4) 
    
  *--Fifth Folder < Sub_Class2> ---------------------------
  CASE lnactfolder = 5 
    SHOW WINDOW (lcWinCh5) TOP
    *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
    *B605582,1               selected in the previous folder [Begin]
    =lfCatgPop()
    =lfClasPop()
    =lfSCls1Pop()
    =lfSCls2Pop()    
    *B605582,1 [End]
    

    IF laScrMode[3]  OR laScrMode[4]
      SHOW GETS WINDOW (lcWinCh5) ENABLE ONLY

      llSCls3D = .T.
      FOR lnI = 1 TO ALEN(laSSCls3,1)
        IF PADL(laSSCls3[lnI,1],1) <> '\'
          llSCls3D = .F.
          EXIT
        ENDIF
      ENDFOR

      IF lnSCls21 = 1 OR ASCAN(laTSCls2,laSCls21[lnSCls21,2]) = 0
        llSCls3D = .T.
        STORE 'DISABLE' TO lcSSCl3St,lcTSCl3St
      ELSE
        llSCls3D = .F.
        STORE 'ENABLE' TO lcSSCl3St,lcTSCl3St
      ENDIF  
            
      IF llSCls3D 
        STORE 'DISABLE' TO lcAlCs3St,lcMvCs3St
        llSCls3D = .F.
      ELSE 
        STORE 'ENABLE' TO lcAlCs3St,lcMvCs3St
      ENDIF  

      IF ALEN(laTSCls3,1) = 1 AND laTSCls3[1,1] = ' ' 
        STORE 'DISABLE' TO lcRmCs3St,lcRAlCs3St
      ELSE
        STORE 'ENABLE' TO lcRmCs3St,lcRAlCs3St
      ENDIF  
            
      SHOW GET lsSSCls3  &lcSSCl3St
      SHOW GET lsTSCls3  &lcTSCl3St
      SHOW GET pbAllCs3  &lcAlCs3St
      SHOW GET pbMovCs3  &lcMvCs3St
      SHOW GET pbRemCs3  &lcRmCs3St
      SHOW GET pbRAllCs3 &lcRAlCs3St
    ENDIF  
    *B605582,1 HBG 11/03/2002 Enable all the drop down lists in view mode and disable the movers[Begin]
    IF laScrMode[2]
      SHOW GET lnCat4   ENABLE
      SHOW GET lnCls3   ENABLE     
      SHOW GET lnSCls12 ENABLE          
      SHOW GET lnSCls21 ENABLE     
      STORE 'DISABLE' TO lcSSCl3St,lcTSCl3St,lcAlCs3St,lcMvCs3St,lcRmCs3St,lcRAlCs3St
      SHOW GET lsSSCls3  &lcSSCl3St
      SHOW GET lsTSCls3  &lcTSCl3St
      SHOW GET pbAllCs3  &lcAlCs3St
      SHOW GET pbMovCs3  &lcMvCs3St
      SHOW GET pbRemCs3  &lcRmCs3St
      SHOW GET pbRAllCs3 &lcRAlCs3St
    ENDIF
    *B605582,1 [End]
    =lfRefresh(lcWinCh5) 
    
ENDCASE

RETURN


*:*************************************************************
*! Name      : lfGetDept
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get the departments to fill the popup in the screen
*:*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetDept()
*!*************************************************************
FUNCTION lfGetDept

SELECT ICDEPTHD
GO TOP
SCAN
  lnFound = ASCAN(laDept,Dept) 
  IF lnFound = 0
    DIMENSION laDept[ALEN(laDept,1)+1]
    laDept[ALEN(laDept,1)] = Dept
  ENDIF
ENDSCAN


*:*************************************************************
*! Name      : lfvDept
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Valid function of department field
*:*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvDept()
*!*************************************************************
FUNCTION lfvDept

lcDept = laDept[lnDept]
SELECT ICDEPTHD
SET ORDER TO TAG DEPTHD 
IF SEEK(lcDept)
  lcDepDesc = ALLTRIM(ICDEPTHD.cdeptdesc)
ENDIF

IF lcDept = 'N/A'
  DIMENSION laGroup[1]
  laGroup[1] = 'N/A'
  lnGrp = 1
  SHOW GET lcDepDesc
  SHOW GET lnGrp DISABLE
  lcGroup = laGroup[lnGrp]
  STORE '' To lcDepDesc , lcGrpDesc 
ELSE
  *--Get the Groups of this departments
  =lfGetGrop()
  SHOW GET lcDepDesc
  SHOW GET lnGrp ENABLE
ENDIF


*:*************************************************************
*! Name      : lfGetGrop
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get the Groups of this departments to fill the 
*!           : popup in the screen
*:*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetGrop()
*!*************************************************************
FUNCTION lfGetGrop

SELECT ICDEPTHD 
SET ORDER TO TAG DEPTHD

DIMENSION laGroup[1]
laGroup[1] = 'N/A'

IF SEEK(lcDept)
  SCAN REST WHILE dept+cstygroup = lcDept
    DIMENSION laGroup[ALEN(laGroup,1)+1]
    laGroup[ALEN(laGroup,1)] = cStyGroup 
  ENDSCAN
ENDIF  
lnGrp = 1
lcGroup = laGroup[lnGrp]

*:*************************************************************
*! Name      : lfvGroup
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Valid function of Groupfield
*:*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvGroup()
*!*************************************************************
FUNCTION lfvGroup

lcGroup   = laGroup[lnGrp]
lcGrpDesc = gfCodDes(lcGroup,'CSTYGROUP')

SELECT ICCLSSTP
SET ORDER TO TAG ICCLSSTP
LOCATE
IF !EOF() AND !SEEK(lcDept+lcGroup,'ICCLSSTP')
  *-- Get Categories
  IF gfModalGen('QRM00000B00006','Alert',.F.,.F.,'Do you want to copy from another Group?') = 1
    
    STORE ' ' TO lcCpyDpt,lcCpyGrp
    SELECT ICCLSSTP
    SCAN
      IF !SEEK(ICCLSSTP.Dept+ICCLSSTP.cStyGroup,lcTmGrpCop)
        SCATTER MEMVAR MEMO
        INSERT INTO (lcTmGrpCop) FROM MEMVAR
      ENDIF
    ENDSCAN
    lcWName    = "ICGCopy"
    lcWSPTitl  = "Copy From another Group"
    PUSH KEY                                
    DO (gcScrDir + gcWinAppl + '\ICCPYGRP.SPR')    
    POP KEY 
    
    IF !EMPTY(lcCpyGrp)
      =lfFillFile(lcCpyDpt,lcCpyGrp,.T.)        && Fill the temp file
      =lfGetCatg(lcCpyDpt,lcCpyGrp)
      =lfGetClas(lcCpyDpt,lcCpyGrp)
      =lfGetCls1(lcCpyDpt,lcCpyGrp)
      =lfGetCls2(lcCpyDpt,lcCpyGrp)
      =lfGetCls3(lcCpyDpt,lcCpyGrp)
    ELSE
      =lfFillFile(lcDept,lcGroup,.F.)        && Fill the temp file
      =lfGetCatg()
      =lfGetClas()
      =lfGetCls1()
      =lfGetCls2()
      =lfGetCls3()      
    ENDIF  
    *C200505,4 TMI [Start] Fill arrays automatically using this variable
    llMltSlct = .T.
    *C200505,4 TMI [End  ] 
  
  ELSE
    IF lcDept <> 'N/A' AND lcGroup <> 'N/A'
      =lfFillFile(lcDept,lcGroup,.F.)        && Fill the temp file
      =lfGetCatg()
      =lfGetClas()
      =lfGetCls1()
      =lfGetCls2()
      =lfGetCls3()
      *C200505,1 TMI [Start] Fill arrays automatically using this variable
      llMltSlct = .T.
      *C200505,1 TMI [End  ] 
    ENDIF  
  ENDIF
ELSE
  *-- Get Categories
  =lfFillFile(lcDept,lcGroup,.T.)        && Fill the temp file
  =lfGetCatg(lcDept,lcGroup)
  =lfGetClas(lcDept,lcGroup)
  =lfGetCls1(lcDept,lcGroup)
  =lfGetCls2(lcDept,lcGroup)
  =lfGetCls3(lcDept,lcGroup)
ENDIF


SELECT ICCLSSTP
SET ORDER TO TAG ICCLSSTP
IF SEEK(lcDept+lcGroup,'ICCLSSTP') 
  STORE .F. To laScrMode
  laScrMode[2] = .T.
ELSE
  IF lcDept <> 'N/A' AND lcGroup <> 'N/A'
    STORE .F. To laScrMode
    laScrMode[3] = .T.
  ENDIF  
ENDIF

SHOW GETS

*!*************************************************************
*! Name      : lfGetCatg
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get Categories from Codes File
*:*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetCatg()
*!*************************************************************
FUNCTION lfGetCatg
PARAMETERS lcCopyDpt,lcCopyGrp

STORE 1   TO lsSCatgry,lsTCatgry  
STORE ' ' TO laSCatgry,laTCatgry
DIMENSION laSCatgry[1,2]
DIMENSION laTCatgry[1,2]

*-- Flag to check if fill Target with Categories from ICCLSSTP file
llGetInfo = IIF(TYPE("lcCopyDpt")<> "C",.F.,.T.)

*-- Get Categories from codes file and fill source
SELECT CODES
SET ORDER TO TAG Ccode_no
llFirst = .T.
IF SEEK('N'+'CSTYCATG  ')
  SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+'CSTYCATG  '
    IF !llFirst
      lnI = ALEN(laSCatgry,1)+1
      DIMENSION laSCatgry[lnI,2]
    ELSE
      lnI = 1  
    ENDIF  
    laSCatgry[lnI,1] = CODES.cdiscrep
    laSCatgry[lnI,2] = CODES.ccode_no
    llFirst = .F.
  ENDSCAN
ENDIF

*-- Fill Target with Categories from ICCLSSTP file
IF llGetInfo
  SELECT (lcWorkFile) 
  SET ORDER TO TAG (lcWorkFile) 
  IF SEEK(lcDept+lcGroup)
    llFirst = .T.
    STORE 1 TO lnCat1,lnCat2,lnCat3,lnCat4
    
    lcTmpDep = lcDept
    lcTmpGrp = lcGroup
    lcDept   = lcCopyDpt
    lcGroup  = lcCopyGrp
    = lfintCdpop()
    *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
    *B605582,1               selected in the previous folder not from codes file[Begin]
    *= gfwCodePop(@laCodInf1, "CSTYCATG","L" )
    *= gfwCodePop(@laCodInf2, "CSTYCATG","L" )
    *= gfwCodePop(@laCodInf3, "CSTYCATG","L" )
    *= gfwCodePop(@laCodInf4, "CSTYCATG","L" )
    *B605582,1 [End]
    
    lcDept  = lcTmpDep
    lcGroup = lcTmpGrp
    SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                    lcDept+lcGroup 
      IF ASCAN('laTCatgry',&lcWorkFile..cstycatg) = 0 AND !EMPTY(&lcWorkFile..cstycatg)
        IF !llFirst
          lnI = ALEN(laTCatgry,1)+1
          DIMENSION laTCatgry[lnI,2]
          *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
          *B605582,1               selected in the previous folder [Begin]
          lnJ = ALEN(laCat1,1)+1
          DIMENSION laCat1[lnJ,2]
          *B605582,1[End]
        ELSE
          lnI = 1  
          *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
          *B605582,1               selected in the previous folder [Begin]
          DIMENSION laCat1[2,2]          
          laCat1[1,1] = 'N/A'
          laCat1[1,2] = ' '
          lnJ = 2
          *B605582,1 [End]
        ENDIF 
        *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
        *B605582,1               selected in the previous folder not from codes file[Begin]
        laCat1[lnJ,1] = gfCodDes(&lcWorkFile..cstycatg,'CSTYCATG')
        laCat1[lnJ,2] = &lcWorkFile..cstycatg
        *B605582,1 [End]
        laTCatgry[lnI,1] = gfCodDes(&lcWorkFile..cstycatg,'CSTYCATG')
        laTCatgry[lnI,2] = &lcWorkFile..cstycatg
        llFirst = .F.
      ENDIF
    ENDSCAN
  *B605582,1 HBG 11/03/2002 IF there is no Category , dimention the popups of category by 1[Begin]
  ELSE
    DIMENSION laCat1[1,2]
    laCat1[1,1] = 'N/A'
    laCat1[1,2] = ' '
  *B605582,1 [End]

  ENDIF
ENDIF

*B605582,1 HBG 11/03/2002 dimention all popups of category by same dimention and fill it with same data[Begin]
DIMENSION laCat2[ALEN(laCat1,1),2],laCat3[ALEN(laCat1,1),2],laCat4[ALEN(laCat1,1),2]
=ACOPY(laCat1,laCat2)
=ACOPY(laCat1,laCat3)
=ACOPY(laCat1,laCat4)
*B605582,1 [End]

IF llGetInfo
  lnFound = ASCAN(laCat1,laTCatgry[1,1])
  IF lnFound > 0
    lnFound = ASUBSCRIPT(laCat1,lnFound,1)
    STORE lnFound TO lnCat1,lnCat2,lnCat3,lnCat4
  ELSE  
    STORE 1 TO lnCat1,lnCat2,lnCat3,lnCat4
  ENDIF  
ELSE
  STORE 1 TO lnCat1,lnCat2,lnCat3,lnCat4
ENDIF  


EXTERNAL ARRAY laSCatgry,laTCatgry
lnOldDim =ALEN(laTCatgry,1)
DECLARE laOldTCat[lnOldDim,2]
=ACOPY(laTCatgry,laOldTCat)

lnOldSCat =ALEN(laSCatgry,1)
DECLARE laOldSCat[lnOldSCat,2]
=ACOPY(laSCatgry,laOldSCat)

IF ALEN(laTCatgry,1) = 1 .AND. TYPE('laTCatgry[1,2]')="L"
  laTCatgry =' '
ENDIF  

FOR lnCount = 1 TO ALEN('laSCatgry',1)
  IF ASCAN('laTCatgry',ALLTRIM(laSCatgry[lnCount,1])) > 0
    laSCatgry[lnCount,1] = '\'+laSCatgry[lnCount,1]
  ENDIF
ENDFOR

lsSCatgry = IIF(LEFT(laSCatgry[1,1] ,1) = "\",0,1)
lsTCatgry = 1

*B605582,1 HBG 11/03/2002 Refresh the folder on top[Begin]
IF WEXIST(lcWinCh1)
  DO CASE
    CASE lnactfolder = 1
      SHOW GETS WINDOW (lcWinCh1) ONLY
    CASE lnactfolder = 2
      SHOW GETS WINDOW (lcWinCh2) ONLY
    CASE lnactfolder = 3
      SHOW GETS WINDOW (lcWinCh3) ONLY
    CASE lnactfolder = 4
      SHOW GETS WINDOW (lcWinCh4) ONLY
    CASE lnactfolder = 5
      SHOW GETS WINDOW (lcWinCh5) ONLY
  ENDCASE
ENDIF  
*B605582,1 [End]

*!*************************************************************
*! Name      : lfGetClas
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get Style Class from Codes File
*:*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetClas()
*!*************************************************************
FUNCTION lfGetClas
PARAMETERS lcCopyDpt,lcCopyGrp

STORE 1   TO lsSClass,lsTClass  
STORE ' ' TO laSClass,laTClass
DIMENSION laSClass[1,2]
DIMENSION laTClass[1,2]
llRefresh = .F.

*-- Flag to check if fill Target with Categories from ICCLSSTP file
llGetInfo = IIF(TYPE("lcCopyDpt")<> "C",.F.,.T.)

*-- Get Categories from codes file and fill source
SELECT CODES
SET ORDER TO TAG Ccode_no
llFirst = .T.
IF SEEK('N'+'CSTYCLASS')
  SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+'CSTYCLASS'
    IF !llFirst
      lnI = ALEN(laSClass,1)+1
      DIMENSION laSClass[lnI,2]
    ELSE
      lnI = 1
    ENDIF  
    laSClass[lnI,1] = CODES.cdiscrep
    laSClass[lnI,2] = CODES.ccode_no
    llFirst = .F.
  ENDSCAN
ENDIF

*-- Fill Target with Categories from ICCLSSTP file
IF llGetInfo
  SELECT (lcWorkFile) 
  SET ORDER TO TAG (lcWorkFile) 
  *B605582,1 HBG 12/3/2002 Get the previous value for the popup[Begin]
  *IF SEEK(lcDept+lcGroup)
  IF TYPE('lcFld') = 'C'
    lnPrvCls1 = lnCls&lcFld
  ELSE
    lnPrvCls1 = lnCls1
  ENDIF  
  IF SEEK(lcDept+lcGroup+laCat1[lnCat1,2])
  *B605582,1 [End]
    llFirst = .T.
    STORE 1 TO lnCls1,lnCls2,lnCls3
    lcTmpDep = lcDept
    lcTmpGrp = lcGroup
    lcDept   = lcCopyDpt
    lcGroup  = lcCopyGrp
    =lfintCdpop()
    *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
    *B605582,1               selected in the previous folder not from codes file[Begin]
    *= gfwCodePop(@laCodInf2, "CSTYCLASS","L" )
    *= gfwCodePop(@laCodInf3, "CSTYCLASS","L" )
    *= gfwCodePop(@laCodInf4, "CSTYCLASS","L" )
    *B605582,1 [End]
    lcDept  = lcTmpDep
    lcDept  = lcTmpDep
    lcGroup = lcTmpGrp 
    SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                    lcDept+lcGroup+laCat1[lnCat1,2]
      IF ASCAN('laTClass',&lcWorkFile..cstyclass) = 0 AND !EMPTY(&lcWorkFile..cstyclass)

        IF !llFirst
          lnI = ALEN(laTClass,1)+1
          DIMENSION laTClass[lnI,2]
          *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
          *B605582,1               selected in the previous folder [Begin]
          lnJ = ALEN(laCls1,1)+1
          DIMENSION laCls1[lnJ,2]
          *B605582,1 [End]
        ELSE
          lnI = 1  
          *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
          *B605582,1               selected in the previous folder not from codes file[Begin]
          DIMENSION laCls1[2,2]
          laCls1[1,1] = 'N/A'
          laCls1[1,2] = ' '
          lnJ = 2                      
          *B605582,1 [End]
        ENDIF 
        *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
        *B605582,1               selected in the previous folder not from codes file[Begin]
        laCls1[lnJ,1] = gfCodDes(&lcWorkFile..cstyclass,'CSTYCLASS')
        laCls1[lnJ,2] = &lcWorkFile..cstyclass
        *B605582,1 [End]
        laTClass[lnI,1] = gfCodDes(&lcWorkFile..cstyclass,'CSTYCLASS')
        laTClass[lnI,2] = &lcWorkFile..cstyclass
        llFirst = .F.
      ENDIF  
    ENDSCAN
  *B605582,1 HBG 11/03/2002 IF there is no Category , dimention the popups of classes by 1[Begin]
  ELSE
    DIMENSION laCls1[1,2]
    laCls1[1,1] = 'N/A'
    laCls1[1,2] = ' '
  *B605582,1 [End]
  
  ENDIF
ENDIF

*B605582,1 HBG 11/03/2002 Dimention all popups of classes and fill it with same data [Begin]
DIMENSION laCls2[ALEN(laCls1,1),2],laCls3[ALEN(laCls1,1),2]
=ACOPY(laCls1,laCls2)
=ACOPY(laCls1,laCls3)
*B605582,1 [End]

IF llGetInfo
  lnFound = ASCAN(laCls1,laTClass[1,1])
  IF lnFound > 0
    lnFound = ASUBSCRIPT(laCls1,lnFound,1)
    *B605582,1 HBG 12/3/2002 IF the previous value for the popup is still found in 
    *B605582,1               it , assign it to the popup[Begin]
    IF lnPrvCls1 > ALEN(laCls1,1)
    *B605582,1 [End]
      STORE lnFound TO lnCls1,lnCls2,lnCls3
    *B605582,1 HBG 12/3/2002 IF the previous value for the popup is still found in 
    *B605582,1               it , assign it to the popup[Begin]
    ELSE
      STORE lnPrvCls1 TO lnCls1,lnCls2,lnCls3
    ENDIF
    *B605582,1 [End] 
  ELSE 
    STORE 1 TO lnCls1,lnCls2,lnCls3
  ENDIF  
ELSE
  STORE 1 TO lnCls1,lnCls2,lnCls3
ENDIF

EXTERNAL ARRAY laSClass,laTClass
lnOldDim =ALEN(laTClass,1)
DECLARE laOldTCls[lnOldDim,2]
=ACOPY(laTClass,laOldTCls)

lnOldSCat =ALEN(laSClass,1)
DECLARE laOldSCls[lnOldSCat,2]
=ACOPY(laSClass,laOldSCls)

IF ALEN(laTClass,1) = 1 .AND. TYPE('laTClass[1,2]')="L"
  laTClass =' '
ENDIF  

FOR lnCount = 1 TO ALEN('laSClass',1)
  IF ASCAN('laTClass',ALLTRIM(laSClass[lnCount,1])) > 0
    laSClass[lnCount,1] = '\'+laSClass[lnCount,1]
  ENDIF
ENDFOR

lsSClass = IIF(LEFT(laSClass[1,1] ,1) = "\",0,1)
lsTClass = 1

*B605582,1 HBG 11/03/2002 Refresh the folder on top[Begin]
IF WEXIST(lcWinCh1)
  DO CASE
    CASE lnactfolder = 1
      SHOW GETS WINDOW (lcWinCh1) ONLY
    CASE lnactfolder = 2
      SHOW GETS WINDOW (lcWinCh2) ONLY
    CASE lnactfolder = 3
      SHOW GETS WINDOW (lcWinCh3) ONLY
    CASE lnactfolder = 4
      SHOW GETS WINDOW (lcWinCh4) ONLY
    CASE lnactfolder = 5
      SHOW GETS WINDOW (lcWinCh5) ONLY
  ENDCASE
ENDIF  
*B605582,1 [End]

*!*************************************************************
*! Name      : lfGetCls1
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get Categories from Codes File
*:*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetCls1()
*!*************************************************************
FUNCTION lfGetCls1
PARAMETERS lcCopyDpt,lcCopyGrp

STORE 1   TO lsSSCls1,lsTSCls1 
STORE ' ' TO laSSCls1,laTSCls1
DIMENSION laSSCls1[1,2]
DIMENSION laTSCls1[1,2]
llRefresh = .F.

*-- Flag to check if fill Target with Categories from ICCLSSTP file
llGetInfo = IIF(TYPE("lcCopyDpt")<> "C",.F.,.T.)

*-- Get Categories from codes file and fill source
SELECT CODES
SET ORDER TO TAG Ccode_no
llFirst = .T.
IF SEEK('N'+'CSTYSBCLAS')
  SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+'CSTYSBCLAS'
    IF !llFirst
      lnI = ALEN(laSSCls1,1)+1
      DIMENSION laSSCls1[lnI,2]
    ELSE
      lnI = 1
    ENDIF  
    laSSCls1[lnI,1] = CODES.cdiscrep
    laSSCls1[lnI,2] = CODES.ccode_no
    llFirst = .F.
  ENDSCAN
ENDIF

*-- Fill Target with Categories from ICCLSSTP file
IF llGetInfo
  SELECT (lcWorkFile) 
  SET ORDER TO TAG (lcWorkFile) 
  *B605582,1 HBG 12/3/2002 Get the previous value for the popup[Begin]
  *IF SEEK(lcDept+lcGroup)
  IF TYPE('lcFld') = 'C'
    lnPrvSCls1 = lnSCls1&lcFld
  ELSE
    lnPrvSCls1 = lnSCls11
  ENDIF  

  IF SEEK(lcDept+lcGroup+laCat1[lnCat1,2]+laCls1[lnCls1,2])
  *B605582,1 [End]
    llFirst = .T.
    STORE 1 TO lnSCls11,lnSCls12
    lcTmpDep = lcDept
    lcTmpGrp = lcGroup
    lcDept   = lcCopyDpt
    lcGroup  = lcCopyGrp
    =lfintCdpop()
    *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
    *B605582,1               selected in the previous folder not from codes file[Begin]
    *= gfwCodePop(@laCodInf3, "CSTYSBCLAS","L" )
    *= gfwCodePop(@laCodInf4, "CSTYSBCLAS","L" )
    *B605582,1 [End]
    lcDept  = lcTmpDep 
    lcGroup = lcTmpGrp 
    SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                    lcDept+lcGroup+laCat1[lnCat1,2]+laCls1[lnCls1,2]
      IF ASCAN('laTSCls1',&lcWorkFile..cstysbclas) = 0 AND !EMPTY(&lcWorkFile..cstysbclas)

        IF !llFirst
          lnI = ALEN(laTSCls1,1)+1
          DIMENSION laTSCls1[lnI,2]
          *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
          *B605582,1               selected in the previous folder [Begin]
          lnJ = ALEN(laSCls11,1)+1
          DIMENSION laSCls11[lnJ,2]
          *B605582,1 [End]
        ELSE
          lnI = 1  
          *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
          *B605582,1               selected in the previous folder[Begin]
          DIMENSION laSCls11[2,2]
          laSCls11[1,1] = 'N/A'
          laSCls11[1,2] = ' '
          lnJ = 2            
          *B605582,1 [End]
        ENDIF 
        *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
        *B605582,1               selected in the previous folder [Begin]
        laSCls11[lnJ,1] = gfCodDes(&lcWorkFile..cstysbclas,'CSTYSBCLAS')
        laSCls11[lnJ,2] = &lcWorkFile..cstysbclas
        *B605582,1 [End]
        laTSCls1[lnI,1] = gfCodDes(&lcWorkFile..cstysbclas,'CSTYSBCLAS')
        laTSCls1[lnI,2] = &lcWorkFile..cstysbclas
        llFirst = .F.
      ENDIF  
    ENDSCAN
  *B605582,1 HBG 11/03/2002 IF there is no Sub Classes 1 , dimention the popups of Sub Classes 1 by 1[Begin]
  ELSE
    DIMENSION laSCls11[1,2]
    laSCls11[1,1] = 'N/A'
    laSCls11[1,2] = ' '
  *B605582,1 [End]
  
  ENDIF
ENDIF

*B605582,1 HBG 11/03/2002 Dimention all popups of Sub Classes 1 and fill it with same data[Begin]
DIMENSION laSCls12[ALEN(laSCls11,1),2]
=ACOPY(laSCls11,laSCls12)
*B605582,1 [End]

IF llGetInfo
  lnFound = ASCAN(laSCls11,laTSCls1[1,1])
  IF lnFound > 0
    lnFound = ASUBSCRIPT(laSCls11,lnFound,1)
    *B605582,1 HBG 12/3/2002 IF the previous value for the popup is still found in 
    *B605582,1               it , assign it to the popup[Begin]
    IF lnPrvSCls1 > ALEN(laSCls11,1)
    *B605582,1 [End]
      STORE lnFound TO lnSCls11,lnSCls12
    *B605582,1 HBG 12/3/2002 IF the previous value for the popup is still found in 
    *B605582,1               it , assign it to the popup[Begin]
    ELSE
      STORE lnPrvSCls1 TO lnSCls11,lnSCls12
    ENDIF
    *B605582,1 [End]
  ELSE 
    STORE 1 TO lnSCls11,lnSCls12
  ENDIF  
ELSE
  STORE 1 TO lnSCls11,lnSCls12
ENDIF  


EXTERNAL ARRAY laSSCls1,laTSCls1
lnOldDim =ALEN(laTSCls1,1)
DECLARE laOldTCl1[lnOldDim,2]
=ACOPY(laTSCls1,laOldTCl1)

lnOldSCl1 =ALEN(laSSCls1,1)
DECLARE laOldSCl1[lnOldSCl1,2]
=ACOPY(laSSCls1,laOldSCl1)

IF ALEN(laTSCls1,1) = 1 .AND. TYPE('laTSCls1[1,2]')="L"
  laTSCls1 =' '
ENDIF  

FOR lnCount = 1 TO ALEN('laSSCls1',1)
  IF ASCAN('laTSCls1',ALLTRIM(laSSCls1[lnCount,1])) > 0
    laSSCls1[lnCount,1] = '\'+laSSCls1[lnCount,1]
  ENDIF
ENDFOR

lsSSCls1 = IIF(LEFT(laSSCls1[1,1] ,1) = "\",0,1)
lsTSCls1 = 1

*B605582,1 HBG 11/03/2002 Refresh the folder on top[Begin]
IF WEXIST(lcWinCh1)
  DO CASE
    CASE lnactfolder = 1
      SHOW GETS WINDOW (lcWinCh1) ONLY
    CASE lnactfolder = 2
      SHOW GETS WINDOW (lcWinCh2) ONLY
    CASE lnactfolder = 3
      SHOW GETS WINDOW (lcWinCh3) ONLY
    CASE lnactfolder = 4
      SHOW GETS WINDOW (lcWinCh4) ONLY
    CASE lnactfolder = 5
      SHOW GETS WINDOW (lcWinCh5) ONLY
  ENDCASE
ENDIF  
*B605582,1 [End]

*!*************************************************************
*! Name      : lfGetCls2
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get Categories from Codes File
*:*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetCls2()
*!*************************************************************
FUNCTION lfGetCls2
PARAMETERS lcCopyDpt,lcCopyGrp

STORE 1   TO lsSSCls2,lsTSCls2 
STORE ' ' TO laSSCls2,laTSCls2
DIMENSION laSSCls2[1,2]
DIMENSION laTSCls2[1,2]
llRefresh = .F.
*-- Flag to check if fill Target with Categories from ICCLSSTP file
llGetInfo = IIF(TYPE("lcCopyDpt")<> "C",.F.,.T.)

*-- Get Categories from codes file and fill source
SELECT CODES
SET ORDER TO TAG Ccode_no
llFirst = .T.
IF SEEK('N'+'CSTYSBCLS2')
  SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+'CSTYSBCLS2'
    IF !llFirst
      lnI = ALEN(laSSCls2,1)+1
      DIMENSION laSSCls2[lnI,2]
    ELSE
      lnI = 1
    ENDIF  
    laSSCls2[lnI,1] = CODES.cdiscrep
    laSSCls2[lnI,2] = CODES.ccode_no
    llFirst = .F.
  ENDSCAN
ENDIF

*-- Fill Target with Categories from ICCLSSTP file
IF llGetInfo
  SELECT (lcWorkFile) 
  SET ORDER TO TAG (lcWorkFile) 
  *B605582,1 HBG 12/3/2002 Get the previous value for the popup[Begin]
  *IF SEEK(lcDept+lcGroup)
  lnPrvSCls2 = lnSCls21
  IF SEEK(lcDept+lcGroup+laCat1[lnCat1,2]+laCls1[lnCls1,2]+laSCls11[lnSCls11,2])
  *B605582,1 [End]
    llFirst = .T.
    STORE 1 TO lnSCls21
    lcTmpDep = lcDept
    lcTmpGrp = lcGroup
    lcDept   = lcCopyDpt
    lcGroup  = lcCopyGrp
    =lfintCdpop()
    *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
    *B605582,1               selected in the previous folder not from codes file[Begin]
    *= gfwCodePop(@laCodInf4, "CSTYSBCLS2","L" )
    *B605582,1 [End]
    lcDept  = lcTmpDep
    lcGroup = lcTmpGrp 
    SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                    lcDept+lcGroup+laCat1[lnCat1,2]+laCls1[lnCls1,2]+laSCls11[lnSCls11,2]
      
      IF ASCAN('laTSCls2',&lcWorkFile..cstysbcls2) = 0 AND !EMPTY(&lcWorkFile..cstysbcls2)
        IF !llFirst
          lnI = ALEN(laTSCls2,1)+1
          DIMENSION laTSCls2[lnI,2]
          *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
          *B605582,1               selected in the previous folder[Begin]
          lnJ = ALEN(laSCls21,1)+1
          DIMENSION laSCls21[lnJ,2]
          *B605582,1 [End]
        ELSE
          lnI = 1  
          *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
          *B605582,1               selected in the previous folder[Begin]
          DIMENSION laSCls21[2,2]
          laSCls21[1,1] = 'N/A'
          laSCls21[1,2] = ' '
          lnJ = 2  
          *B605582,1 [End]
        ENDIF 
        *B605582,1 HBG 12/3/2002 Fill The drop down lists with the value
        *B605582,1               selected in the previous folder[Begin]
        laSCls21[lnJ,1] = gfCodDes(&lcWorkFile..cstysbcls2,'CSTYSBCLS2')
        laSCls21[lnJ,2] = &lcWorkFile..cstysbcls2
        *B605582,1 [End]
        laTSCls2[lnI,1] = gfCodDes(&lcWorkFile..cstysbcls2,'CSTYSBCLS2')
        laTSCls2[lnI,2] = &lcWorkFile..cstysbcls2
        llFirst = .F.
      ENDIF  
    ENDSCAN
  ENDIF
ENDIF

IF llGetInfo
  lnFound = ASCAN(laSCls21,laTSCls2[1,1])
  IF lnFound > 0
    lnFound = ASUBSCRIPT(laSCls21,lnFound,1)
    *B605582,1 HBG 12/3/2002 IF the previous value for the popup is still found in 
    *B605582,1               it , assign it to the popup[Begin]
    IF lnPrvSCls2 > ALEN(laSCls21,1)
    *B605582,1 [End]
      STORE lnFound TO lnSCls21
    *B605582,1 HBG 12/3/2002 IF the previous value for the popup is still found in 
    *B605582,1               it , assign it to the popup[Begin]  
    ELSE
      STORE lnPrvSCls2 TO lnSCls21    
    ENDIF
    *B605582,1 [End]
  ELSE  
    STORE 1 TO lnSCls21
  ENDIF  
ELSE
  STORE 1 TO lnSCls21 
ENDIF  

EXTERNAL ARRAY laSSCls2,laTSCls2
lnOldDim =ALEN(laTSCls2,1)
DECLARE laOldTCl2[lnOldDim,2]
=ACOPY(laTSCls2,laOldTCl2)

lnOldSCl2 =ALEN(laSSCls2,1)
DECLARE laOldSCl2[lnOldSCl2,2]
=ACOPY(laSSCls2,laOldSCl2)

IF ALEN(laTSCls2,1) = 1 .AND. TYPE('laTSCls2[1,2]')="L"
  laTSCls2 =' '
ENDIF  

FOR lnCount = 1 TO ALEN('laSSCls2',1)
  IF ASCAN('laTSCls2',ALLTRIM(laSSCls2[lnCount,1])) > 0
    laSSCls2[lnCount,1] = '\'+laSSCls2[lnCount,1]
  ENDIF
ENDFOR

lsSSCls2 = IIF(LEFT(laSSCls2[1,1] ,1) = "\",0,1)
lsTSCls2 = 1

*B605582,1 HBG 11/03/2002 Refresh the folder on top[Begin]
IF WEXIST(lcWinCh1)
  DO CASE
    CASE lnactfolder = 1
      SHOW GETS WINDOW (lcWinCh1) ONLY
    CASE lnactfolder = 2
      SHOW GETS WINDOW (lcWinCh2) ONLY
    CASE lnactfolder = 3
      SHOW GETS WINDOW (lcWinCh3) ONLY
    CASE lnactfolder = 4
      SHOW GETS WINDOW (lcWinCh4) ONLY
    CASE lnactfolder = 5
      SHOW GETS WINDOW (lcWinCh5) ONLY
  ENDCASE
ENDIF  
*B605582,1 [End]

*!*************************************************************
*! Name      : lfGetCls3
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get Categories from Codes File
*:*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetCls3()
*!*************************************************************
FUNCTION lfGetCls3
PARAMETERS lcCopyDpt,lcCopyGrp  

STORE 1   TO lsSSCls3,lsTSCls3 
STORE ' ' TO laSSCls3,laTSCls3
DIMENSION laSSCls3[1,2]
DIMENSION laTSCls3[1,2]
llRefresh = .F.

*-- Flag to check if fill Target with Categories from ICCLSSTP file
llGetInfo = IIF(TYPE("lcCopyDpt")<> "C",.F.,.T.)

*-- Get Categories from codes file and fill source
SELECT CODES
SET ORDER TO TAG Ccode_no
llFirst = .T.
IF SEEK('N'+'CSTYSBCLS3')
  SCAN REST WHILE cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = 'N'+'CSTYSBCLS3'
    IF !llFirst
      lnI = ALEN(laSSCls3,1)+1
      DIMENSION laSSCls3[lnI,2]
    ELSE
      lnI = 1
    ENDIF  
    laSSCls3[lnI,1] = CODES.cdiscrep
    laSSCls3[lnI,2] = CODES.ccode_no
    llFirst = .F.
  ENDSCAN
ENDIF


*-- Fill Target with Categories from ICCLSSTP file
IF llGetInfo
  SELECT (lcWorkFile) 
  SET ORDER TO TAG (lcWorkFile) 
  *B605582,1 HBG 12/3/2002 Seek by the all previous values selected [Begin]
  *IF SEEK(lcDept+lcGroup)
  IF SEEK(lcDept+lcGroup+laCat1[lnCat1,2]+laCls1[lnCls1,2]+laSCls11[lnSCls11,2]+laSCls21[lnSCls21,2])
  *B605582,1 [End]
    llFirst = .T.
    SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                    lcDept+lcGroup+laCat1[lnCat1,2]+laCls1[lnCls1,2]+laSCls11[lnSCls11,2]+laSCls21[lnSCls21,2]
      IF ASCAN('laTSCls3',&lcWorkFile..cstysbcls3) = 0 AND !EMPTY(&lcWorkFile..cstysbcls3)
        IF !llFirst
          lnI = ALEN(laTSCls3,1)+1
          DIMENSION laTSCls3[lnI,2]
        ELSE
          lnI = 1  
        ENDIF  
        laTSCls3[lnI,1] = gfCodDes(&lcWorkFile..cstysbcls3,'CSTYSBCLS3')
        laTSCls3[lnI,2] = &lcWorkFile..cstysbcls3
        llFirst = .F.
      ENDIF  
    ENDSCAN
  ENDIF
ENDIF

EXTERNAL ARRAY laSSCls3,laTSCls3
lnOldDim =ALEN(laTSCls3,1)
DECLARE laOldTCl3[lnOldDim,2]
=ACOPY(laTSCls3,laOldTCl3)

lnOldSCl3 =ALEN(laSSCls3,1)
DECLARE laOldSCl3[lnOldSCl3,2]
=ACOPY(laSSCls3,laOldSCl3)

IF ALEN(laTSCls3,1) = 1 .AND. TYPE('laTSCls3[1,2]')="L"
  laTSCls3 =' '
ENDIF  

FOR lnCount = 1 TO ALEN('laSSCls3',1)
  IF ASCAN('laTSCls3',ALLTRIM(laSSCls3[lnCount,1])) > 0
    laSSCls3[lnCount,1] = '\'+laSSCls3[lnCount,1]
  ENDIF
ENDFOR

lsSSCls3 = IIF(LEFT(laSSCls3[1,1] ,1) = "\",0,1)
lsTSCls3 = 1

*B605582,1 HBG 11/03/2002 Refresh the folder on top[Begin]
IF WEXIST(lcWinCh1)
  DO CASE
    CASE lnactfolder = 1
      SHOW GETS WINDOW (lcWinCh1) ONLY
    CASE lnactfolder = 2
      SHOW GETS WINDOW (lcWinCh2) ONLY
    CASE lnactfolder = 3
      SHOW GETS WINDOW (lcWinCh3) ONLY
    CASE lnactfolder = 4
      SHOW GETS WINDOW (lcWinCh4) ONLY
    CASE lnactfolder = 5
      SHOW GETS WINDOW (lcWinCh5) ONLY
  ENDCASE
ENDIF  
*B605582,1 [End]

*!*************************************************************
*! Name      : lfvSource
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : valid function for the source Categories
*:*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSource()
*!*************************************************************
FUNCTION lfvSource
PARAMETERS lcItemMov 

DO CASE 
  CASE lcItemMov = 'CG'

    IF lsSCatgry = 0 OR LEFT(laSCatgry[lsSCatgry,1],1) = "\"
      RETURN .F.
    ENDIF

    IF lsSCatgry <= ALEN('laSCatgry',1) AND lsSCatgry <> 0
      IF !EMPTY(laTCatgry[1,1]) 
        DIMENSION laTCatgry[ALEN(laTCatgry,1)+1,2]
      ENDIF
      laTCatgry[ALEN(laTCatgry,1),1]= ALLTRIM(laSCatgry[lsSCatgry,1])
      laTCatgry[ALEN(laTCatgry,1),2]= ALLTRIM(laSCatgry[lsSCatgry,2])
      *-- When moving a record we replace "\" 
      *-- before the moved element in order to disable it.
      laSCatgry[lsSCatgry,1] = "\"+laSCatgry[lsSCatgry,1]
      IF lsSCatgry < ALEN(laSCatgry,1)
        DO WHILE LEFT(laSCatgry[lsSCatgry,1],1) = "\" 
          lsSCatgry = lsSCatgry + 1
          IF lsSCatgry = ALEN(laSCatgry,1)
            EXIT
          ENDIF
        ENDDO  
      ELSE
        lsSCatgry = 1  
      ENDIF
    ENDIF  
  
    IF lsSCatgry > ALEN(laSCatgry,1)
      lsSCatgry = ALEN(laSCatgry,1)
    ENDIF
  
    _CUROBJ = OBJNUM(lsSCatgry)
    
    llSCatgD = .T.
    FOR lnI = 1 TO ALEN(laSCatgry,1)
      IF PADL(laSCatgry[lnI,1],1) <> '\'
        llSCatgD = .F.
        EXIT
      ENDIF
    ENDFOR
    SHOW GETS
  
  CASE lcItemMov = 'CS'
    IF lsSClass = 0 OR LEFT(laSClass[lsSClass,1],1) = "\"
      RETURN .F.
    ENDIF

    IF lsSClass <= ALEN('laSClass',1) AND lsSClass <> 0
      IF !EMPTY(laTClass[1,1]) 
        DIMENSION laTClass[ALEN(laTClass,1)+1,2]
      ENDIF
      laTClass[ALEN(laTClass,1),1]= ALLTRIM(laSClass[lsSClass,1])
      laTClass[ALEN(laTClass,1),2]= ALLTRIM(laSClass[lsSClass,2])
      *-- When moving a record we replace "\" 
      *-- before the moved element in order to disable it.
      laSClass[lsSClass,1] = "\"+laSClass[lsSClass,1]
      IF lsSClass < ALEN(laSClass,1)
        DO WHILE LEFT(laSClass[lsSClass,1],1) = "\" 
          lsSClass = lsSClass + 1
          IF lsSClass = ALEN(laSClass,1)
            EXIT
          ENDIF
        ENDDO  
      ELSE
        lsSClass = 1  
      ENDIF
    ENDIF  
  
    IF lsSClass > ALEN(laSClass,1)
      lsSClass = ALEN(laSClass,1)
    ENDIF
  
    _CUROBJ = OBJNUM(lsSClass)
    
    llSClassD = .T.
    FOR lnI = 1 TO ALEN(laSClass,1)
      IF PADL(laSClass[lnI,1],1) <> '\'
        llSClassD = .F.
        EXIT
      ENDIF
    ENDFOR

    SHOW GETS

  CASE lcItemMov = 'S1'
 
    IF lsSSCls1 = 0 OR LEFT(laSSCls1[lsSSCls1,1],1) = "\"
      RETURN .F.
    ENDIF

    IF lsSSCls1 <= ALEN('laSSCls1',1) AND lsSSCls1 <> 0
      IF !EMPTY(laTSCls1[1,1]) 
        DIMENSION laTSCls1[ALEN(laTSCls1,1)+1,2]
      ENDIF
      laTSCls1[ALEN(laTSCls1,1),1]= ALLTRIM(laSSCls1[lsSSCls1,1])
      laTSCls1[ALEN(laTSCls1,1),2]= ALLTRIM(laSSCls1[lsSSCls1,2])
      *-- When moving a record we replace "\" 
      *-- before the moved element in order to disable it.
      laSSCls1[lsSSCls1,1] = "\"+laSSCls1[lsSSCls1,1]
      IF lsSSCls1 < ALEN(laSSCls1,1)
        DO WHILE LEFT(laSSCls1[lsSSCls1,1],1) = "\" 
          lsSSCls1 = lsSSCls1 + 1
          IF lsSSCls1 = ALEN(laSSCls1,1)
            EXIT
          ENDIF
        ENDDO  
      ELSE
        lsSSCls1 = 1  
      ENDIF
    ENDIF  
  
    IF lsSSCls1 > ALEN(laSSCls1,1)
      lsSSCls1 = ALEN(laSSCls1,1)
    ENDIF
  
    _CUROBJ = OBJNUM(lsSSCls1)
    
    llSCls1D = .T.
    FOR lnI = 1 TO ALEN(laSSCls1,1)
      IF PADL(laSSCls1[lnI,1],1) <> '\'
        llSCls1D = .F.
        EXIT
      ENDIF
    ENDFOR
    SHOW GETS

  CASE lcItemMov = 'S2'
    IF lsSSCls2 = 0 OR LEFT(laSSCls2[lsSSCls2,1],1) = "\"
      RETURN .F.
    ENDIF

    IF lsSSCls2 <= ALEN('laSSCls2',1) AND lsSSCls2 <> 0
      IF !EMPTY(laTSCls2[1,1]) 
        DIMENSION laTSCls2[ALEN(laTSCls2,1)+1,2]
      ENDIF
      laTSCls2[ALEN(laTSCls2,1),1]= ALLTRIM(laSSCls2[lsSSCls2,1])
      laTSCls2[ALEN(laTSCls2,1),2]= ALLTRIM(laSSCls2[lsSSCls2,2])
      *-- When moving a record we replace "\" 
      *-- before the moved element in order to disable it.
      laSSCls2[lsSSCls2,1] = "\"+laSSCls2[lsSSCls2,1]
      IF lsSSCls2 < ALEN(laSSCls2,1)
        DO WHILE LEFT(laSSCls2[lsSSCls2,1],1) = "\" 
          lsSSCls2 = lsSSCls2 + 1
          IF lsSSCls2 = ALEN(laSSCls2,1)
            EXIT
          ENDIF
        ENDDO  
      ELSE
        lsSSCls2 = 1  
      ENDIF
    ENDIF  
  
    IF lsSSCls2 > ALEN(laSSCls2,1)
      lsSSCls2 = ALEN(laSSCls2,1)
    ENDIF
  
    _CUROBJ = OBJNUM(lsSSCls2)
    
    llSCls2D = .T.
    FOR lnI = 1 TO ALEN(laSSCls2,1)
      IF PADL(laSSCls2[lnI,1],1) <> '\'
        llSCls2D = .F.
        EXIT
      ENDIF
    ENDFOR
    SHOW GETS

  CASE lcItemMov = 'S3'
  
    IF lsSSCls3 = 0 OR LEFT(laSSCls3[lsSSCls3,1],1) = "\"
      RETURN .F.
    ENDIF

    IF lsSSCls3 <= ALEN('laSSCls3',1) AND lsSSCls3 <> 0
      IF !EMPTY(laTSCls3[1,1]) 
        DIMENSION laTSCls3[ALEN(laTSCls3,1)+1,2]
      ENDIF
      laTSCls3[ALEN(laTSCls3,1),1]= ALLTRIM(laSSCls3[lsSSCls3,1])
      laTSCls3[ALEN(laTSCls3,1),2]= ALLTRIM(laSSCls3[lsSSCls3,2])
      *-- When moving a record we replace "\" 
      *-- before the moved element in order to disable it.
      laSSCls3[lsSSCls3,1] = "\"+laSSCls3[lsSSCls3,1]
      IF lsSSCls3 < ALEN(laSSCls3,1)
        DO WHILE LEFT(laSSCls3[lsSSCls3,1],1) = "\" 
          lsSSCls3 = lsSSCls3 + 1
          IF lsSSCls3 = ALEN(laSSCls3,1)
            EXIT
          ENDIF
        ENDDO  
      ELSE
        lsSSCls3 = 1  
      ENDIF
    ENDIF  
  
    IF lsSSCls3 > ALEN(laSSCls3,1)
      lsSSCls3 = ALEN(laSSCls3,1)
    ENDIF
  
    _CUROBJ = OBJNUM(lsSSCls3)
    
    llSCls3D = .T.
    FOR lnI = 1 TO ALEN(laSSCls3,1)
      IF PADL(laSSCls3[lnI,1],1) <> '\'
        llSCls3D = .F.
        EXIT
      ENDIF
    ENDFOR
    SHOW GETS
ENDCASE

*!*************************************************************
*! Name      : lfvTarget
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : valid function for the Target Categories
*:*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvTarget()
*!*************************************************************
FUNCTION lfvTarget
PARAMETERS lcItemMov 


DO CASE 
  CASE lcItemMov = 'CG'
    lcExactSav = SET("EXACT")
    SET EXACT ON

    IF lsTCatgry<= ALEN('laTCatgry',1) .AND.;
      lsTCatgry<> 0 .AND.;
      ASCAN('laSCatgry',"\"+laTCatgry[lsTCatgry,1]) > 0

      lsSCatgry  = ASUBSCRIPT('laSCatgry',;
                       ASCAN('laSCatgry',"\"+laTCatgry[lsTCatgry,1]),1)
      *-- Enable the element in the source list 
      *-- when removing it from the target one.
      laSCatgry[lsSCatgry,1] = SUBSTR(laSCatgry[lsSCatgry,1],2,LEN(laSCatgry[lsSCatgry,1]))
      =ADEL(laTCatgry,lsTCatgry)
      IF ALEN(laTCatgry,1) > 1
        DIMENSION laTCatgry[ALEN(laTCatgry,1)-1,2]
      ELSE
        laTCatgry[1,1] =' '
        laTCatgry[1,2] =' '
      ENDIF  
    ENDIF


    IF lsTCatgry= 0
      lsTCatgry= 1
    ENDIF  

    IF lsTCatgry> ALEN(laTCatgry,1)
      lsTCatgry = ALEN(laTCatgry,1)
    ENDIF

    SET EXACT &lcExactSav

    _CUROBJ = OBJNUM(lsTCatgry)
    
    IF ALEN(laTCatgry,1) = 1 AND laTCatgry[1,1] = ' '
      llTCatgD = .T.
    ELSE
      llTCatgD = .F.
    ENDIF
    SHOW GETS
  
  CASE lcItemMov = 'CS'
    lcExactSav = SET("EXACT")
    SET EXACT ON

    IF lsTClass<= ALEN('laTClass',1) .AND.;
      lsTClass<> 0 .AND.;
      ASCAN('laSClass',"\"+laTClass[lsTClass,1]) > 0

      lsSClass  = ASUBSCRIPT('laSClass',;
                       ASCAN('laSClass',"\"+laTClass[lsTClass,1]),1)
      *-- Enable the element in the source list 
      *-- when removing it from the target one.
      laSClass[lsSClass,1] = SUBSTR(laSClass[lsSClass,1],2,LEN(laSClass[lsSClass,1]))
      =ADEL(laTClass,lsTClass)
      IF ALEN(laTClass,1) > 1
        DIMENSION laTClass[ALEN(laTClass,1)-1,2]
      ELSE
        laTClass[1,1] =' '
        laTClass[1,2] =' '
      ENDIF  
    ENDIF


    IF lsTClass= 0
      lsTClass= 1
    ENDIF  

    IF lsTClass> ALEN(laTClass,1)
      lsTClass = ALEN(laTClass,1)
    ENDIF

    SET EXACT &lcExactSav

    _CUROBJ = OBJNUM(lsTClass)

    IF ALEN(laTClass,1) = 1 AND laTClass[1,1] = ' '
      llTClassD = .T.
    ELSE
      llTClassD = .F.
    ENDIF
    SHOW GETS

  
  CASE lcItemMov = 'S1'
    lcExactSav = SET("EXACT")
    SET EXACT ON

    IF lsTSCls1<= ALEN('laTSCls1',1) .AND.;
      lsTSCls1<> 0 .AND.;
      ASCAN('laSSCls1',"\"+laTSCls1[lsTSCls1,1]) > 0

      lsSSCls1  = ASUBSCRIPT('laSSCls1',;
                       ASCAN('laSSCls1',"\"+laTSCls1[lsTSCls1,1]),1)
      *-- Enable the element in the source list 
      *-- when removing it from the target one.
      laSSCls1[lsSSCls1,1] = SUBSTR(laSSCls1[lsSSCls1,1],2,LEN(laSSCls1[lsSSCls1,1]))
      =ADEL(laTSCls1,lsTSCls1)
      IF ALEN(laTSCls1,1) > 1
        DIMENSION laTSCls1[ALEN(laTSCls1,1)-1,2]
      ELSE
        laTSCls1[1,1] =' '
        laTSCls1[1,2] =' '
      ENDIF  
    ENDIF


    IF lsTSCls1= 0
      lsTSCls1= 1
    ENDIF  

    IF lsTSCls1> ALEN(laTSCls1,1)
      lsTSCls1 = ALEN(laTSCls1,1)
    ENDIF

    SET EXACT &lcExactSav

    _CUROBJ = OBJNUM(lsTSCls1)
    
    IF ALEN(laTSCls1,1) = 1 AND laTSCls1[1,1] = ' '
      llTCls1D = .T.
    ELSE
      llTCls1D = .F.
    ENDIF    
    SHOW GETS

  CASE lcItemMov = 'S2'

    lcExactSav = SET("EXACT")
    SET EXACT ON

    IF lsTSCls2<= ALEN('laTSCls2',1) .AND.;
      lsTSCls2<> 0 .AND.;
      ASCAN('laSSCls2',"\"+laTSCls2[lsTSCls2,1]) > 0

      lsSSCls2  = ASUBSCRIPT('laSSCls2',;
                       ASCAN('laSSCls2',"\"+laTSCls2[lsTSCls2,1]),1)
      *-- Enable the element in the source list 
      *-- when removing it from the target one.
      laSSCls2[lsSSCls2,1] = SUBSTR(laSSCls2[lsSSCls2,1],2,LEN(laSSCls2[lsSSCls2,1]))
      =ADEL(laTSCls2,lsTSCls2)
      IF ALEN(laTSCls2,1) > 1
        DIMENSION laTSCls2[ALEN(laTSCls2,1)-1,2]
      ELSE
        laTSCls2[1,1] =' '
        laTSCls2[1,2] =' '
      ENDIF  
    ENDIF


    IF lsTSCls2= 0
      lsTSCls2= 1
    ENDIF  

    IF lsTSCls2> ALEN(laTSCls2,1)
      lsTSCls2 = ALEN(laTSCls2,1)
    ENDIF

    SET EXACT &lcExactSav

    _CUROBJ = OBJNUM(lsTSCls2)
    
    IF ALEN(laTSCls2,1) = 1 AND laTSCls2[1,1] = ' '
      llTCls2D = .T.
    ELSE
      llTCls2D = .F.
    ENDIF    
    SHOW GETS
    
  CASE lcItemMov = 'S3'
    lcExactSav = SET("EXACT")
    SET EXACT ON

    IF lsTSCls3<= ALEN('laTSCls3',1) .AND.;
      lsTSCls3<> 0 .AND.;
      ASCAN('laSSCls3',"\"+laTSCls3[lsTSCls3,1]) > 0

      lsSSCls3  = ASUBSCRIPT('laSSCls3',;
                       ASCAN('laSSCls3',"\"+laTSCls3[lsTSCls3,1]),1)
      *-- Enable the element in the source list 
      *-- when removing it from the target one.
      laSSCls3[lsSSCls3,1] = SUBSTR(laSSCls3[lsSSCls3,1],2,LEN(laSSCls3[lsSSCls3,1]))
      =ADEL(laTSCls3,lsTSCls3)
      IF ALEN(laTSCls3,1) > 1
        DIMENSION laTSCls3[ALEN(laTSCls3,1)-1,2]
      ELSE
        laTSCls3[1,1] =' '
        laTSCls3[1,2] =' '
      ENDIF  
    ENDIF


    IF lsTSCls3= 0
      lsTSCls3= 1
    ENDIF  

    IF lsTSCls3> ALEN(laTSCls3,1)
      lsTSCls3 = ALEN(laTSCls3,1)
    ENDIF

    SET EXACT &lcExactSav

    _CUROBJ = OBJNUM(lsTSCls3)
    
    IF ALEN(laTSCls3,1) = 1 AND laTSCls3[1,1] = ' '
      llTCls3D = .T.
    ELSE
      llTCls3D = .F.
    ENDIF    
    SHOW GETS
ENDCASE
*:********************************************************************
*! Name      : lfvMovment
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : valid function for movment buttons for Categories mover
*:********************************************************************
*! Passed Parameters  : ............
*:********************************************************************
*! Returns            : ............
*:********************************************************************
*! Example   : =lfvMovment()
*:********************************************************************
FUNCTION lfvMovment
PARAMETERS lcItemMov,lnMovmnts


DO CASE 
  CASE lcItemMov = 'CG'

    DO CASE
      CASE lnMovmnts = 1
        =lfvSource('CG')
      CASE lnMovmnts = 2
        DECLARE laTCatgry[ALEN('laSCatgry',1),2]
        FOR lnCounter = 1 TO ALEN(laSCatgry,1)
          lnPos = IIF(OCCURS("\",laSCatgry[lnCounter,1])>0,2,1)
          laTCatgry[lnCounter,1] = SUBSTR(laSCatgry[lnCounter,1],lnPos,LEN(laSCatgry[lnCounter,1]))
          laTCatgry[lnCounter,2] = SUBSTR(laSCatgry[lnCounter,2],lnPos,LEN(laSCatgry[lnCounter,2]))
          laSCatgry[lnCounter,1] = IIF(OCCURS("\",laSCatgry[lnCounter,1])>0,"","\") +  laSCatgry[lnCounter,1]
        ENDFOR
        
        llSCatgD = .T.
        lsTCatgry = 1
        _CUROBJ = OBJNUM(lsTCatgry)
        SHOW GETS
      CASE lnMovmnts = 3
        =lfvTarget('CG')
      CASE lnMovmnts = 4
        *-- Looping through the laSCatgry array and
        *-- enabling all the elements of laSCatgry array
        FOR lnCounter = 1 TO ALEN(laSCatgry,1)
          lnPos = IIF(OCCURS("\",laSCatgry[lnCounter,1])>0,2,1)
          laSCatgry[lnCounter,1] = SUBSTR(laSCatgry[lnCounter,1],lnPos,LEN(laSCatgry[lnCounter,1]))
        ENDFOR
        DECLARE laTCatgry[1,2]
        laTCatgry =' '
        lsSCatgry = 1
        _CUROBJ = OBJNUM(lsSCatgry)
        llTCatgD = .T.
        SHOW GETS
    ENDCASE
    
  CASE lcItemMov = 'CS'

    DO CASE
      CASE lnMovmnts = 1
        =lfvSource('CS')
      CASE lnMovmnts = 2
        DECLARE laTClass[ALEN('laSClass',1),2]
        FOR lnCounter = 1 TO ALEN(laSClass,1)
          lnPos = IIF(OCCURS("\",laSClass[lnCounter,1])>0,2,1)
          laTClass[lnCounter,1] = SUBSTR(laSClass[lnCounter,1],lnPos,LEN(laSClass[lnCounter,1]))
          laTClass[lnCounter,2] = SUBSTR(laSClass[lnCounter,2],lnPos,LEN(laSClass[lnCounter,2]))
          laSClass[lnCounter,1] = IIF(OCCURS("\",laSClass[lnCounter,1])>0,"","\") +  laSClass[lnCounter,1]
        ENDFOR
        
        llSClassD = .T.
        lsTClass = 1
        _CUROBJ = OBJNUM(lsTClass)
        SHOW GETS
      CASE lnMovmnts = 3
        =lfvTarget('CS')
      CASE lnMovmnts = 4
        *-- Looping through the laSClass array and
        *-- enabling all the elements of laSClass array
        FOR lnCounter = 1 TO ALEN(laSClass,1)
          lnPos = IIF(OCCURS("\",laSClass[lnCounter,1])>0,2,1)
          laSClass[lnCounter,1] = SUBSTR(laSClass[lnCounter,1],lnPos,LEN(laSClass[lnCounter,1]))
        ENDFOR
        DECLARE laTClass[1,2]
        laTClass =' '
        lsSClass = 1
        _CUROBJ = OBJNUM(lsSClass)
        llTClassD = .T.
        SHOW GETS
    ENDCASE

  CASE lcItemMov = 'S1'
    DO CASE
      CASE lnMovmnts = 1
        =lfvSource('S1')
      CASE lnMovmnts = 2
        DECLARE laTSCls1[ALEN('laSSCls1',1),2]
        FOR lnCounter = 1 TO ALEN(laSSCls1,1)
          lnPos = IIF(OCCURS("\",laSSCls1[lnCounter,1])>0,2,1)
          laTSCls1[lnCounter,1] = SUBSTR(laSSCls1[lnCounter,1],lnPos,LEN(laSSCls1[lnCounter,1]))
          laTSCls1[lnCounter,2] = SUBSTR(laSSCls1[lnCounter,2],lnPos,LEN(laSSCls1[lnCounter,2]))
          laSSCls1[lnCounter,1] = IIF(OCCURS("\",laSSCls1[lnCounter,1])>0,"","\") +  laSSCls1[lnCounter,1]
        ENDFOR
        lsTSCls1 = 1
        _CUROBJ = OBJNUM(lsTSCls1)
        llSCls1D = .T.
        SHOW GETS
      CASE lnMovmnts = 3
        =lfvTarget('S1')
      CASE lnMovmnts = 4
        *-- Looping through the laSSCls1 array and
        *-- enabling all the elements of laSSCls1 array
        FOR lnCounter = 1 TO ALEN(laSSCls1,1)
          lnPos = IIF(OCCURS("\",laSSCls1[lnCounter,1])>0,2,1)
          laSSCls1[lnCounter,1] = SUBSTR(laSSCls1[lnCounter,1],lnPos,LEN(laSSCls1[lnCounter,1]))
        ENDFOR
        DECLARE laTSCls1[1,2]
        laTSCls1 =' '
        lsSSCls1 = 1
        _CUROBJ = OBJNUM(lsSSCls1)
        llTCls1D = .T.
        SHOW GETS
    ENDCASE
    
  CASE lcItemMov = 'S2'
    DO CASE
      CASE lnMovmnts = 1
        =lfvSource('S2')
      CASE lnMovmnts = 2
        DECLARE laTSCls2[ALEN('laSSCls2',1),2]
        FOR lnCounter = 1 TO ALEN(laSSCls2,1)
          lnPos = IIF(OCCURS("\",laSSCls2[lnCounter,1])>0,2,1)
          laTSCls2[lnCounter,1] = SUBSTR(laSSCls2[lnCounter,1],lnPos,LEN(laSSCls2[lnCounter,1]))
          laTSCls2[lnCounter,2] = SUBSTR(laSSCls2[lnCounter,2],lnPos,LEN(laSSCls2[lnCounter,2]))
          laSSCls2[lnCounter,1] = IIF(OCCURS("\",laSSCls2[lnCounter,1])>0,"","\") +  laSSCls2[lnCounter,1]
        ENDFOR
        lsTSCls2 = 1
        _CUROBJ = OBJNUM(lsTSCls2)
        llSCls2D = .T.
        SHOW GETS
      CASE lnMovmnts = 3
        =lfvTarget('S2')
      CASE lnMovmnts = 4
        *-- Looping through the laSSCls2 array and
        *-- enabling all the elements of laSSCls2 array
        FOR lnCounter = 1 TO ALEN(laSSCls2,1)
          lnPos = IIF(OCCURS("\",laSSCls2[lnCounter,1])>0,2,1)
          laSSCls2[lnCounter,1] = SUBSTR(laSSCls2[lnCounter,1],lnPos,LEN(laSSCls2[lnCounter,1]))
        ENDFOR
        DECLARE laTSCls2[1,2]
        laTSCls2 =' '
        lsSSCls2 = 1
        _CUROBJ = OBJNUM(lsSSCls2)
        llTCls2D = .T.
        SHOW GETS
    ENDCASE

  CASE lcItemMov = 'S3'
    DO CASE
      CASE lnMovmnts = 1
        =lfvSource('S3')
      CASE lnMovmnts = 2
        DECLARE laTSCls3[ALEN('laSSCls3',1),2]
        FOR lnCounter = 1 TO ALEN(laSSCls3,1)
          lnPos = IIF(OCCURS("\",laSSCls3[lnCounter,1])>0,2,1)
          laTSCls3[lnCounter,1] = SUBSTR(laSSCls3[lnCounter,1],lnPos,LEN(laSSCls3[lnCounter,1]))
          laTSCls3[lnCounter,2] = SUBSTR(laSSCls3[lnCounter,2],lnPos,LEN(laSSCls3[lnCounter,2]))
          laSSCls3[lnCounter,1] = IIF(OCCURS("\",laSSCls3[lnCounter,1])>0,"","\") +  laSSCls3[lnCounter,1]
        ENDFOR
        lsTSCls3 = 1
        _CUROBJ = OBJNUM(lsSSCls3)
        llSCls3D = .T.
        SHOW GETS
      CASE lnMovmnts = 3
        =lfvTarget('S3')
      CASE lnMovmnts = 4
        *-- Looping through the laSSCls3 array and
        *-- enabling all the elements of laSSCls3 array
        FOR lnCounter = 1 TO ALEN(laSSCls3,1)
          lnPos = IIF(OCCURS("\",laSSCls3[lnCounter,1])>0,2,1)
          laSSCls3[lnCounter,1] = SUBSTR(laSSCls3[lnCounter,1],lnPos,LEN(laSSCls3[lnCounter,1]))
        ENDFOR
        DECLARE laTSCls3[1,2]
        laTSCls3 =' '
        lsSSCls3 = 1
        _CUROBJ = OBJNUM(lsSSCls3)
        llTCls3D = .T.
        SHOW GETS
    ENDCASE

ENDCASE
*:*************************************************************
*! Name     : lpClsScr
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose  : Close procedure
*:*************************************************************
PROCEDURE lpClsScr

IF SEEK(lcDept+lcGroup,'ICCLSSTP')
  *-- Get Categories
  SELECT (lcWorkFile)
  LOCATE
  SCAN
    DELETE
  ENDSCAN
  =lfFillFile(lcDept,lcGroup,.T.)        && Fill the temp file
  =lfGetCatg(lcDept,lcGroup)
  =lfGetClas(lcDept,lcGroup)
  =lfGetCls1(lcDept,lcGroup)
  =lfGetCls2(lcDept,lcGroup)
  =lfGetCls3(lcDept,lcGroup)

  STORE .F. To laScrMode
  laScrMode[3] = .T.
ELSE
  STORE .F. To laScrMode
  laScrMode[4] = .T.
ENDIF  


*:********************************************************************
*! Name      : lfvCpyGrp
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : valid function for Copy Group 
*:********************************************************************
*! Passed Parameters  : ............
*:********************************************************************
*! Returns            : ............
*:********************************************************************
*! Example   : =lfvCpyGrp()
*:********************************************************************
FUNCTION lfvCpyGrp

DIMENSION laSelected[2]
SELECT (lcTmGrpCop)

llComp  = .F.
lcCpyDpt = lcDept

IF llBrowse OR (!EMPTY(lcCpygrp) AND !SEEK(lcCpyDpt+lcCpygrp,lcTmGrpCop))
  llBrowse  = .F.
  lcBrFields  = [Dept      :12:H='Department',]+;
                [cStyGroup :10:H='Group']
                
  llComp = AriaBrow('',"Style Groups",;
               15, 01,15,02,;
               .F.,.F.,"Dept,cStyGroup","laSelected")
ENDIF

IF llComp 
  lcCpyDpt  = ALLTRIM(laSelected[1])
  lcCpyGrp  = ALLTRIM(laSelected[2])
ELSE
  lcCpyDpt  = ''
  lcCpyGrp  = ''  
ENDIF 

_CUROBJ = OBJNUM(pbCopyOk)

*!*************************************************************
*! Name      : lfOldValue
*! Auth   : HEND GHANEM (HBG).
*! Date   : 18/10/2001.
*! Purpose: Function to store old value of the current field.
*!*************************************************************
FUNCTION lfoldvalue

lcOldValue = EVALUATE(SYS(18))
RETURN


*!*************************************************************
*! Name      : lfGropAct
*! Auth      : HEND GHANEM (HBG).
*! Date      : 18/10/2001.
*! Purpose   : Trap the keys in screen ICCPYGRP
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfGropAct()
*!*************************************************************

FUNCTION lfGropAct

*-- This function called in activate snippet for screen packScr
*-- which use to copy data from another pack id

ON KEY LABEL ESCAPE DO lfGropEsc

*!*************************************************************
*! Name      : lfGropEsc
*! Auth      : HEND GHANEM (HBG).
*! Date      : 18/10/2001.
*! Purpose   : Trap the Esacpe Key in screen ICCPYGRP
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfGropEsc()
*!*************************************************************

FUNCTION lfGropEsc

ON KEY LABEL ESCAPE

_CUROBJ = OBJNUM(pbCancel)
KEYBOARD "{ENTER}" CLEAR PLAIN


*!*************************************************************
*! Name      : lfIntVar
*! Auth      : HEND GHANEM (HBG).
*! Date      : 18/10/2001.
*! Purpose   : Initialze Variables
*!*************************************************************
*! Example   : =lfIntVar()
*!*************************************************************
FUNCTION lfIntVar

DIMENSION laSCatgry[1,2],laTCatgry[1,2],;
          laSClass[1,2],laTClass[1,2],laSSCls1[1,2],laTSCls1[1,2],laSSCls2[1,2],laTSCls2[1,2],;
          laSSCls3[1,2],laTSCls3[1,2]
          
DIMENSION  laCat1[1,2],laCat2[1,2],laCat3[1,2],laCat4[1,2] && Popup arrays for Categories for the different folders
DIMENSION  laCls1[1,2],laCls2[1,2],laCls3[1,2]           && Popup arrays for Class for the different folders
DIMENSION  laSCls11[1,2],laSCls12[1,2]                 && Popup arrays for Sub Class 1 for the different folders
DIMENSION  laSCls21[1,2]                             && Popup arrays for Sub Class 2 for the different folders

STORE ''  TO laCat1,laCat2,laCat3,laCat4,laCls1,laCls2,laCls3,laSCls11,laSCls12,laSCls21
STORE ''  TO lcCat1,lcCat2,lcCat3,lcCat4,lcCls1,lcCls2,lcCls3,lcSCls11,lcSCls12,lcSCls21
STORE 1   TO lnCat1,lnCat2,lnCat3,lnCat4,lnCls1,lnCls2,lnCls3,lnSCls11,lnSCls12,lnSCls21

STORE 1   TO lsSCatgry,lsTCatgry,lsSClass,lsTClass,;
             lsSSCls1,lsTSCls1,lsSSCls2,lsTSCls2,lsSSCls3,lsTSCls3
STORE ' ' TO laSCatgry,laTCatgry,laSClass,laTClass,;
             laSSCls1,laTSCls1,laSSCls2,laTSCls2,laSSCls3,laTSCls3

STORE 1  TO lnDept , lnGrp 
STORE ' ' TO lcGroup,lcDept,lcDepDesc,lcGrpDesc
STORE .T. TO llFillPop


*-- Intialize arrays of code popup
=lfintCdpop()
= gfwCodePop(@laCodInf1, "CSTYCATG","L" )
= gfwCodePop(@laCodInf2, "CSTYCATG","L" )
= gfwCodePop(@laCodInf3, "CSTYCATG","L" )
= gfwCodePop(@laCodInf4, "CSTYCATG","L" )
=lfintCdpop()
= gfwCodePop(@laCodInf2, "CSTYCLASS","L" )
= gfwCodePop(@laCodInf3, "CSTYCLASS","L" )
= gfwCodePop(@laCodInf4, "CSTYCLASS","L" )
=lfintCdpop()
= gfwCodePop(@laCodInf3, "CSTYSBCLAS","L" )
= gfwCodePop(@laCodInf4, "CSTYSBCLAS","L" )
=lfintCdpop()
= gfwCodePop(@laCodInf4, "CSTYSBCLS2","L" )

IF USED(lcWorkFile)
  USE IN (lcWorkFile)
ENDIF
USE (gcWorkDir+lcWorkFile) EXCLUSIVE IN 0
SELECT (lcWorkFile)
ZAP

=lfGetCatg()
=lfGetClas()
=lfGetCls1()
=lfGetCls2()
=lfGetCls3()
          
*:*************************************************************
*! Name      : lpSavScr     
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Save procedure
*:*************************************************************
PROCEDURE lpSavScr 


*-- If user didn't assign any catgories , class , sub class , sub class 2 , or sub class 3. 
*-- we can't save
IF EMPTY(laTCatgry[1,1]) AND EMPTY(laTClass[1,1]) AND EMPTY(laTSCls1[1,1]) AND;
   EMPTY(laTSCls2[1,1]) AND EMPTY(laTSCls3[1,1]) AND;
   lnCat1 = 1 AND lnCat2 = 1 AND lnCat3 = 1 AND lnCat4 = 1 AND lnCls1 = 1 AND;
   lnCls2 = 1 AND lnCls3 = 1 AND lnSCls11 = 1 AND lnSCls12 = 1 AND lnSCls21 = 1   

  lcMsge = 'Cannot save. You have to assign any categories, class, sub class, sub class 2,;
            or sub class 3 to this Department-Group.'
  =gfModalGen('QRM00000B00000','Alert',.F.,.F.,lcMsge)
  STORE .F. TO laScrMode
  laScrMode[4] = .T.
  RETURN
ENDIF


SELECT (lcWorkFile)
LOCATE
SCAN
  DO CASE
    CASE SEEK(&lcWorkFile..dept+&lcWorkFile..cstygroup,'ICCLSSTP') AND EMPTY(ICCLSSTP.cstycatg)
      REPLACE ICCLSSTP.cstycatg   WITH &lcWorkFile..cstycatg,;
              ICCLSSTP.cstyclass  WITH &lcWorkFile..cstyclass,;
              ICCLSSTP.cstysbclas WITH &lcWorkFile..cstysbclas,;
              ICCLSSTP.cstysbcls2 WITH &lcWorkFile..cstysbcls2,;
              ICCLSSTP.cstysbcls3 WITH &lcWorkFile..cstysbcls3
              
    CASE SEEK(&lcWorkFile..dept+&lcWorkFile..cstygroup+&lcWorkFile..cstycatg,'ICCLSSTP');
              AND EMPTY(ICCLSSTP.cstyclass)
      REPLACE ICCLSSTP.cstyclass  WITH &lcWorkFile..cstyclass,;
             ICCLSSTP.cstysbclas WITH &lcWorkFile..cstysbclas,;
              ICCLSSTP.cstysbcls2 WITH &lcWorkFile..cstysbcls2,;
              ICCLSSTP.cstysbcls3 WITH &lcWorkFile..cstysbcls3
              
    CASE SEEK(&lcWorkFile..dept+&lcWorkFile..cstygroup+&lcWorkFile..cstycatg+;
              &lcWorkFile..cstyclass,'ICCLSSTP')  AND EMPTY(ICCLSSTP.cstysbclas)
      REPLACE ICCLSSTP.cstysbclas WITH &lcWorkFile..cstysbclas,;
              ICCLSSTP.cstysbcls2 WITH &lcWorkFile..cstysbcls2,;
              ICCLSSTP.cstysbcls3 WITH &lcWorkFile..cstysbcls3

    CASE SEEK(&lcWorkFile..dept+&lcWorkFile..cstygroup+&lcWorkFile..cstycatg+&lcWorkFile..cstyclass+;
           &lcWorkFile..cstysbclas,'ICCLSSTP')  AND EMPTY(ICCLSSTP.cstysbcls2)
      REPLACE ICCLSSTP.cstysbcls2 WITH &lcWorkFile..cstysbcls2,;
              ICCLSSTP.cstysbcls3 WITH &lcWorkFile..cstysbcls3
                 
    CASE SEEK(&lcWorkFile..dept+&lcWorkFile..cstygroup+&lcWorkFile..cstycatg+&lcWorkFile..cstyclass+;
           &lcWorkFile..cstysbclas+&lcWorkFile..cstysbcls2,'ICCLSSTP')  AND EMPTY(ICCLSSTP.cstysbcls3)
      REPLACE ICCLSSTP.cstysbcls3 WITH &lcWorkFile..cstysbcls3

    CASE !SEEK(&lcWorkFile..dept+&lcWorkFile..cstygroup+&lcWorkFile..cstycatg+&lcWorkFile..cstyclass+;
           &lcWorkFile..cstysbclas+&lcWorkFile..cstysbcls2+&lcWorkFile..cstysbcls3,'ICCLSSTP')
      INSERT INTO ICCLSSTP (dept,cstygroup,cstycatg,cstyclass,cstysbclas,cstysbcls2,cstysbcls3);
                   VALUES  (&lcWorkFile..dept,&lcWorkFile..cstygroup,&lcWorkFile..cstycatg,;
                            &lcWorkFile..cstyclass,&lcWorkFile..cstysbclas,&lcWorkFile..cstysbcls2,;
                            &lcWorkFile..cstysbcls3)      
  ENDCASE
ENDSCAN

IF SEEK(lcDept+lcGroup,'ICCLSSTP')
  SELECT ICCLSSTP
  SCAN REST WHILE Dept+cStygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = lcDept+lcGroup
    IF !SEEK(Dept+cStygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3,lcWorkFile)
      REPLACE cstysbcls3 WITH 'DELETE'
    ENDIF
  ENDSCAN
ENDIF

SELECT ICCLSSTP
DELETE FOR cstysbcls3 = 'DELETE'
*C200505,1 TMI [Start] Rest the variable "llMltSlct" to .F.
llMltSlct = .F.
*C200505,1 TMI [End  ] 


*:*************************************************************
*! Name      : lpDelScr     
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Delete procedure
*:*************************************************************
PROCEDURE lpDelScr 

IF SEEK(lcDept+lcGroup,'ICCLSSTP')
  SELECT ICCLSSTP
  *B605582,1 HBG 11/03/2002 Fix bug of not deleting all lines[Begin]
  *SCAN REST WHILE Dept+cStygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = lcDept+lcGroup
  *  REPLACE cstysbcls3 WITH 'DELETE'
  *ENDSCAN
  DELETE ALL FOR Dept+cStygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = lcDept+lcGroup
  *B605582,1 [End]
ENDIF

*B605582,1 HBG 11/03/2002 Fix bug of not deleting all lines[Begin]
*SELECT ICCLSSTP
*DELETE FOR cstysbcls3 = 'DELETE'
*B605582,1 [End]

STORE .F. TO laScrMode
laScrMode[1] = .T.

*!*************************************************************
*! Name      : lfChkCat
*! Auth      : HEND GHANEM (HBG).
*! Date      : 18/10/2001.
*! Purpose   : Check if this catgory selected in the firest file
*!*************************************************************
*! Example   : =lfChkCat()
*!*************************************************************
FUNCTION lfChkCat
PARAMETERS lcCatgory

IF EMPTY(lcCatgory)
  STORE SPACE(1) TO lcCatgory
ENDIF

*B605582,1 HBG 12/3/2002 It Shouldn't refresh the folder in previous folder according 
*B605582,1               to changes in the drop down list[Begin]
*DIMENSION laTCatgry[1,2]
*laTCatgry=' '
*FOR lnJ = 1 TO ALEN(laSCatgry,1)
*  lnPos = IIF(OCCURS("\",laSCatgry[lnJ,1])>0,2,1)
*  laSCatgry[lnJ,1] = SUBSTR(laSCatgry[lnJ,1],lnPos,LEN(laSCatgry[lnJ,1]))
*ENDFOR
*IF SEEK(lcDept+lcGroup+lcCatgory,lcWorkFile)
*  lnI = 1
*  SELECT (lcWorkFile)
*  lcTmpCat = ' '
*  SCAN REST WHILE Dept+cStygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = lcDept+lcGroup
*    IF &lcWorkFile..cstycatg <> lcTmpCat 
*      DIMENSION laTCatgry[lnI,2]
*      lcTmpCat = &lcWorkFile..cstycatg
*      lnFound = ASCAN(laSCatgry,lcTmpCat) 
*      IF lnFound > 0
*        lnFound = ASUBSCRIPT(laSCatgry,lnFound,1)
*        laTCatgry[lnI,1] = laSCatgry[lnFound,1]
*        laTCatgry[lnI,2] = laSCatgry[lnFound,2] 
*        laSCatgry[lnFound,1] = "\"+laSCatgry[lnFound,1]
*        lnI = lnI + 1
*      ENDIF
*    ENDIF  
*  ENDSCAN  
*ENDIF
*B605582,1 [End]

lnFound = ASCAN(laTCatgry,lcCatgory)
lnFound = IIF(lnCat1 <> 1,lnFound,0)
IF lnFound = 0
  FOR lnCounter = 1 TO ALEN(laSClass,1)
    lnPos = IIF(OCCURS("\",laSClass[lnCounter,1])>0,2,1)
    laSClass[lnCounter,1] = SUBSTR(laSClass[lnCounter,1],lnPos,LEN(laSClass[lnCounter,1]))
  ENDFOR
  DECLARE laTClass[1,2]
  laTClass =' '
  STORE 'DISABLE' TO lcSClasSt,lcTClasSt,lcAllClSt,lcMovClSt,lcRemClSt,lcRAlClsSt
  SHOW GET lsSClass  DISABLE
  SHOW GET lsTClass  DISABLE
  SHOW GET pbAllCls  DISABLE
  SHOW GET pbMovCls  DISABLE
  SHOW GET pbRemCls  DISABLE
  SHOW GET pbRAllCls DISABLE
ELSE
  FOR lnCounter = 1 TO ALEN(laSClass,1)
    lnPos = IIF(OCCURS("\",laSClass[lnCounter,1])>0,2,1)
    laSClass[lnCounter,1] = SUBSTR(laSClass[lnCounter,1],lnPos,LEN(laSClass[lnCounter,1]))
  ENDFOR
  DECLARE laTClass[1,2]
  laTClass =' '
  SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
  =SEEK(lcDept+lcGroup+lcCatgory,lcWorkFile)
  SELECT (lcWorkFile)
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  lcDept+lcGroup+lcCatgory  
    lcClass = &lcWorkFile..cstyclass
    lnFound = ASCAN(laSClass,lcClass) 
    IF lnFound > 0
      *-- Update class mover with the new selection
      lsSClass = ASUBSCRIPT(laSClass,lnFound,1)
      IF lsSClass = 0 OR LEFT(laSClass[lsSClass,1],1) = "\"
        EXIT
      ENDIF

      IF lsSClass <= ALEN('laSClass',1) AND lsSClass <> 0
        IF !EMPTY(laTClass[1,1]) 
          DIMENSION laTClass[ALEN(laTClass,1)+1,2]
        ENDIF
        laTClass[ALEN(laTClass,1),1]= ALLTRIM(laSClass[lsSClass,1])
        laTClass[ALEN(laTClass,1),2]= ALLTRIM(laSClass[lsSClass,2])
        *-- When moving a record we replace "\" 
        *-- before the moved element in order to disable it.
        laSClass[lsSClass,1] = "\"+laSClass[lsSClass,1]
        IF lsSClass < ALEN(laSClass,1)
          DO WHILE LEFT(laSClass[lsSClass,1],1) = "\" 
            lsSClass = lsSClass + 1
            IF lsSClass = ALEN(laSClass,1)
              EXIT
            ENDIF
          ENDDO  
        ELSE
          lsSClass = 1  
        ENDIF
      ENDIF  
  
      IF lsSClass > ALEN(laSClass,1)
        lsSClass = ALEN(laSClass,1)
      ENDIF
      
      
    ENDIF
  ENDSCAN
  llEnbMov = .F.
  FOR lnI = 1 TO ALEN(laSClass,1)
    IF OCCURS("\",laSClass[lnI,1]) = 0
      llEnbMov = .T.
      EXIT
    ENDIF
  ENDFOR
  IF llEnbMov 
    STORE 'ENABLE' TO lcSClasSt,lcAllClSt,lcMovClSt
  ELSE 
    STORE 'DISABLE' TO lcSClasSt,lcAllClSt,lcMovClSt
  ENDIF

  llEnbRMov = .F.
  IF lsTClass  = 1 AND laTClass[lsTClass,1] <> ' '
    llEnbRMov = .T.
  ENDIF
  IF llEnbRMov 
    STORE 'ENABLE' TO lcTClasSt,lcRemClSt,lcRAlClsSt
  ELSE 
    STORE 'DISABLE' TO lcTClasSt,lcRemClSt,lcRAlClsSt
  ENDIF

  *B605582,1 HBG 11/03/2002 Disable all movers in view mode[Begin]
  IF laScrMode[2]
    STORE 'DISABLE' TO lcSClasSt,lcTClasSt,lcAllClSt,lcMovClSt,lcRemClSt,lcRAlClsSt
  ENDIF
  *B605582,1 [End]

  SHOW GET lsSClass  &lcSClasSt
  SHOW GET lsTClass  &lcTClasSt
  SHOW GET pbAllCls  &lcAllClSt
  SHOW GET pbMovCls  &lcMovClSt
  SHOW GET pbRemCls  &lcRemClSt
  SHOW GET pbRAllCls &lcRAlClsSt
ENDIF


GO TOP IN (lcWorkFile)

=lfClsPop(lcCatgory,'1')


*!*************************************************************
*! Name      : lfClsPop
*! Auth      : HEND GHANEM (HBG).
*! Date      : 18/10/2001.
*! Purpose   : Refresh class popup
*!*************************************************************
*! Example   : =lfClsPop()
*!*************************************************************
FUNCTION lfClsPop
PARAMETERS lcCatg,lcFld

IF EMPTY(lcCatg)
  STORE SPACE(1) TO lcCatg
ENDIF

SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
IF SEEK(lcDept+lcGroup+lcCatg,lcWorkFile)
  lcCls = &lcWorkFile..cstyclass
  *B605582,1 HBG 12/3/2002 If there is no class assign, set the popup to 'N/A' [Begin]
  IF EMPTY(lcCls)
    lnCls&lcFld = 1
    lcCls&lcFld = laCls&lcFld[lnCls&lcFld,2]
  ELSE
  *B605582,1 [End]
    lnFound = ASCAN(laCls&lcFld,lcCls) 
    IF lnFound > 0
      lnCls&lcFld = ASUBSCRIPT(laCls&lcFld,lnFound,1)
      lcCls&lcFld = laCls&lcFld[lnCls&lcFld,2]
    *B605582,1 HBG 12/3/2002 If there is aclass assign but not found in the popup,add it to the popup [Begin]
    ELSE
      DIMENSION laCls&lcFld[ALEN(laCls&lcFld,1)+1,2]
      laCls&lcFld[ALEN(laCls&lcFld,1),1] = gfCodDes(&lcWorkFile..cstyclass,'CSTYCLASS')
      laCls&lcFld[ALEN(laCls&lcFld,1),2] = &lcWorkFile..cstyclass
      lnCls&lcFld = ALEN(laCls&lcFld,1)
      lcCls&lcFld = laCls&lcFld[lnCls&lcFld,2]
    ENDIF
    lcFl = STR(EVAL(lcFld)+1,1) 
    =lfCatgPop(lnCat&lcFl)
    *B605582,1 [End]
  ENDIF  
ELSE
  lnCls&lcFld = 1
  lcCls&lcFld = laCls&lcFld[lnCls&lcFld,2]
ENDIF  

STORE lnCls&lcFld TO lnCls1,lnCls2,lnCls3
lcCls1 = laCls1[lnCls1,2]
lcCls2 = laCls2[lnCls2,2]
lcCls3 = laCls3[lnCls3,2]

SHOW GET lnCls1
SHOW GET lnCls2
SHOW GET lnCls3

lnFound = ASCAN(laTClass,lcCls1) 
lnFound = IIF(lnFound > 0 AND !EMPTY(laTClass[1,1]) AND lcCls1 <> ' ',lnFound,0)
IF lnFound = 0
  FOR lnCounter = 1 TO ALEN(laSSCls1,1)
    lnPos = IIF(OCCURS("\",laSSCls1[lnCounter,1])>0,2,1)
    laSSCls1[lnCounter,1] = SUBSTR(laSSCls1[lnCounter,1],lnPos,LEN(laSSCls1[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls1[1,2]
  laTSCls1 =' '
  STORE 'DISABLE' TO lcSSCl1St,lcTSCl1St,lcAlCs1St,lcMvCs1St,lcRmCs1St,lcRAlCs1St
ELSE
  FOR lnCounter = 1 TO ALEN(laSSCls1,1)
    lnPos = IIF(OCCURS("\",laSSCls1[lnCounter,1])>0,2,1)
    laSSCls1[lnCounter,1] = SUBSTR(laSSCls1[lnCounter,1],lnPos,LEN(laSSCls1[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls1[1,2]
  laTSCls1 =' '
  SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
  =SEEK(lcDept+lcGroup+lcCatg+lcCls1,lcWorkFile)
  SELECT (lcWorkFile)
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  lcDept+lcGroup+lcCatg+lcCls1
    lcSClas1 = &lcWorkFile..cstysbclas
    lnFound  = ASCAN(laSSCls1,lcSClas1) 
    IF lnFound > 0
      lsSSCls1  = ASUBSCRIPT(laSSCls1  ,lnFound,1)
      *-- Update sub class 1 mover with the new selection
      IF lsSSCls1 = 0 OR LEFT(laSSCls1[lsSSCls1,1],1) = "\"
        EXIT
      ENDIF

      IF lsSSCls1 <= ALEN('laSSCls1',1) AND lsSSCls1 <> 0
        IF !EMPTY(laTSCls1[1,1]) 
          DIMENSION laTSCls1[ALEN(laTSCls1,1)+1,2]
        ENDIF
        laTSCls1[ALEN(laTSCls1,1),1]= ALLTRIM(laSSCls1[lsSSCls1,1])
        laTSCls1[ALEN(laTSCls1,1),2]= ALLTRIM(laSSCls1[lsSSCls1,2])
        *-- When moving a record we replace "\" 
        *-- before the moved element in order to disable it.
        laSSCls1[lsSSCls1,1] = "\"+laSSCls1[lsSSCls1,1]
        IF lsSSCls1 < ALEN(laSSCls1,1)
          DO WHILE LEFT(laSSCls1[lsSSCls1,1],1) = "\" 
            lsSSCls1 = lsSSCls1 + 1
            IF lsSSCls1 = ALEN(laSSCls1,1)
              EXIT
            ENDIF
          ENDDO  
        ELSE
          lsSSCls1 = 1  
        ENDIF
      ENDIF  
   
      IF lsSSCls1 > ALEN(laSSCls1,1)
        lsSSCls1 = ALEN(laSSCls1,1)
      ENDIF
      
    ENDIF
  ENDSCAN
 *B605582,1 HBG 11/03/2002 Disable all movers in view mode[Begin]
  IF laScrMode[2]
    STORE 'DISABLE' TO lcSSCl1St,lcTSCl1St,lcAlCs1St,lcMvCs1St,lcRmCs1St,lcRAlCs1St
  ELSE
  *B605582,1 [End]
    STORE 'ENABLE' TO lcSSCl1St,lcTSCl1St,lcAlCs1St,lcMvCs1St,lcRmCs1St,lcRAlCs1St
  *B605582,1 HBG 11/03/2002 End if view mode[Begin]  
  ENDIF
  *B605582,1 [End]
ENDIF

IF lcFld <> '1'
  lcSFld = STR(VAL(lcFld)-1,1)
  =lfCls1Pop(lcCatg,lcCls&lcFld,lcSFld)
ELSE
  =lfCls1Pop(lcCatg,lcCls&lcFld,lcFld)  
ENDIF
IF llFld2
  =lfChkCls1(lcCatg,lcCls&lcFld,laSCls11[lnSCls11,2])
  llFld2 = .F.
ENDIF  

*!*************************************************************
*! Name      : lfCls1Pop
*! Auth      : HEND GHANEM (HBG).
*! Date      : 18/10/2001.
*! Purpose   : Refresh class 1 popup
*!*************************************************************
*! Example   : =lfCls1Pop()
*!*************************************************************
FUNCTION lfCls1Pop
PARAMETERS lcCatg,lcClass,lcFld

IF EMPTY(lcClass)
  STORE SPACE(1) TO lcClass
ENDIF

SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
IF SEEK(lcDept+lcGroup+lcCatg+lcClass,lcWorkFile)
  lcClas1 = &lcWorkFile..cstysbclas
  *B605582,1 HBG 12/3/2002 If there is no Sub class 1 assign, set the popup to 'N/A' [Begin]
  IF EMPTY(lcClas1)
    lnSCls1&lcFld = 1
    lcSCls1&lcFld = laSCls1&lcFld[lnSCls1&lcFld,2]
  ELSE
  *B605582,1 [End]
    lnFound = ASCAN(laSCls1&lcFld,lcClas1) 
    IF lnFound > 0
      lnSCls1&lcFld = ASUBSCRIPT(laSCls1&lcFld,lnFound,1)
      lcSCls1&lcFld = laSCls1&lcFld[lnSCls1&lcFld,2]
    *B605582,1 HBG 12/3/2002 If there is Sub class 1 a assign but not found in the 
    *B605582,1               popup,add it to the popup [Begin]
    ELSE
      DIMENSION laSCls1&lcFld[ALEN(laSCls1&lcFld,1)+1,2]
      laSCls1&lcFld[ALEN(laSCls1&lcFld,1),1] = gfCodDes(&lcWorkFile..cstysbclas,'CSTYSBCLAS')
      laSCls1&lcFld[ALEN(laSCls1&lcFld,1),2] = &lcWorkFile..cstysbclas
      lnSCls1&lcFld = ALEN(laSCls1&lcFld,1)
      lcSCls1&lcFld = laSCls1&lcFld[lnSCls1&lcFld,2]
    ENDIF
     lcFl = STR(EVAL(lcFld)+1,1) 
     =lfClasPop(lnCls&lcFl)
    *B605582,1 [End]

  ENDIF  
ELSE
  lnSCls1&lcFld = 1
  lcSCls1&lcFld = laSCls1&lcFld[lnSCls1&lcFld,2]
ENDIF  

STORE lnSCls1&lcFld TO lnSCls11,lnSCls12

SHOW GET lnSCls11
SHOW GET lnSCls12


IF EMPTY(lcSCls11)
  STORE SPACE(1) TO lcSCls11
ENDIF

lnFound = ASCAN(laTSCls1,lcSCls11)
lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls1[1,1]),lnFound,0) 
IF lnFound = 0
  FOR lnCounter = 1 TO ALEN(laSSCls2,1)
    lnPos = IIF(OCCURS("\",laSSCls2[lnCounter,1])>0,2,1)
    laSSCls2[lnCounter,1] = SUBSTR(laSSCls2[lnCounter,1],lnPos,LEN(laSSCls2[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls2[1,2]
  laTSCls2=' '
  STORE 'DISABLE' TO lcSSCl2St,lcTSCl2St,lcAlCs2St,lcMvCs2St,lcRmCs2St,lcRAlCs2St
ELSE
  FOR lnCounter = 1 TO ALEN(laSSCls2,1)
    lnPos = IIF(OCCURS("\",laSSCls2[lnCounter,1])>0,2,1)
    laSSCls2[lnCounter,1] = SUBSTR(laSSCls2[lnCounter,1],lnPos,LEN(laSSCls2[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls2[1,2]
  laTSCls2 =' '
  SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
  =SEEK(lcDept+lcGroup+lcCatg+lcClass+lcSCls11,lcWorkFile)
  SELECT (lcWorkFile)
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  lcDept+lcGroup+lcCatg+lcClass+lcSCls11
    lcSClas2 = &lcWorkFile..cstysbcls2
    lnFound  = ASCAN(laSSCls2,lcSClas2) 
    IF lnFound > 0
      lsSSCls2  = ASUBSCRIPT(laSSCls2,lnFound,1)
      *-- Update Sub class 2 mover with the new selection
      IF lsSSCls2 = 0 OR LEFT(laSSCls2[lsSSCls2,1],1) = "\"
        EXIT
      ENDIF

      IF lsSSCls2 <= ALEN('laSSCls2',1) AND lsSSCls2 <> 0
        IF !EMPTY(laTSCls2[1,1]) 
          DIMENSION laTSCls2[ALEN(laTSCls2,1)+1,2]
        ENDIF
        laTSCls2[ALEN(laTSCls2,1),1]= ALLTRIM(laSSCls2[lsSSCls2,1])
        laTSCls2[ALEN(laTSCls2,1),2]= ALLTRIM(laSSCls2[lsSSCls2,2])
        *-- When moving a record we replace "\" 
        *-- before the moved element in order to disable it.
        laSSCls2[lsSSCls2,1] = "\"+laSSCls2[lsSSCls2,1]
        IF lsSSCls2 < ALEN(laSSCls2,1)
          DO WHILE LEFT(laSSCls2[lsSSCls2,1],1) = "\" 
            lsSSCls2 = lsSSCls2 + 1
            IF lsSSCls2 = ALEN(laSSCls2,1)
              EXIT
            ENDIF
          ENDDO  
        ELSE
          lsSSCls2 = 1  
        ENDIF
      ENDIF  
  
      IF lsSSCls2 > ALEN(laSSCls2,1)
        lsSSCls2 = ALEN(laSSCls2,1)
      ENDIF
      
    ENDIF
  ENDSCAN
  *B605582,1 HBG 11/03/2002 Disable all movers in view mode[Begin]
  IF laScrMode[2]
    STORE 'DISABLE' TO lcSSCl2St,lcTSCl2St,lcAlCs2St,lcMvCs2St,lcRmCs2St,lcRAlCs2St
  ELSE
  *B605582,1 [End]
    STORE 'ENABLE' TO lcSSCl2St,lcTSCl2St,lcAlCs2St,lcMvCs2St,lcRmCs2St,lcRAlCs2St
  *B605582,1 HBG 11/03/2002 End if view mode[Begin]
  ENDIF
  *B605582,1 [End]
 
ENDIF


=lfCls2Pop(lcCatg,lcClass,lcSCls1&lcFld,'21')
=lfChkCls2(lcCatg,lcClass,lcSCls1&lcFld,laSCls21[lnSCls21,2])

*!*************************************************************
*! Name      : lfCls2Pop
*! Auth      : HEND GHANEM (HBG).
*! Date      : 18/10/2001.
*! Purpose   : Refresh class 2 popup
*!*************************************************************
*! Example   : =lfCls2Pop()
*!*************************************************************
FUNCTION lfCls2Pop
PARAMETERS lcCatg,lcClass,lcSClass,lcFld

IF EMPTY(lcSClass)
  STORE SPACE(1) TO lcSClass
ENDIF
SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
IF SEEK(lcDept+lcGroup+lcCatg+lcClass+lcSClass,lcWorkFile)
  lcClas2 = &lcWorkFile..cstysbcls2
  *B605582,1 HBG 12/3/2002 If there is no Sub class 2 assign, set the popup to 'N/A' [Begin]
  IF EMPTY(lcClas2)
    lnSCls&lcFld = 1
    lcSCls&lcFld = laSCls&lcFld[lnSCls&lcFld,2]
  ELSE
  *B605582,1 [End]
    lnFound = ASCAN(laSCls&lcFld,lcClas2 ) 
    IF lnFound > 0
      lnSCls&lcFld = ASUBSCRIPT(laSCls&lcFld,lnFound,1)
      lcSCls&lcFld = laSCls&lcFld[lnSCls&lcFld,2]
    *B605582,1 HBG 12/3/2002 If there is Sub class 2 a assign but not found in the 
    *B605582,1               popup,add it to the popup [Begin]
    ELSE
      DIMENSION laSCls&lcFld[ALEN(laSCls&lcFld,1)+1,2]
      laSCls&lcFld[ALEN(laSCls&lcFld,1),1] = gfCodDes(&lcWorkFile..cstysbcls2,'CSTYSBCLS2')
      laSCls&lcFld[ALEN(laSCls&lcFld,1),2] = &lcWorkFile..cstysbcls2
      lnSCls&lcFld = ALEN(laSCls&lcFld,1)
      lcSCls&lcFld = laSCls&lcFld[lnSCls&lcFld,2]
    ENDIF
    =lfSCls1Pop(lnSCls12)
    *B605582,1 [End]
  ENDIF  
ELSE    
  DIMENSION laSCls&lcFld[1,2]
  lnSCls&lcFld = 1
  lcSCls&lcFld = laSCls&lcFld[lnSCls&lcFld,2]
ENDIF

SHOW GET lnSCls&lcFld 


IF EMPTY(lcSCls21)
  STORE SPACE(1) TO lcSCls21
ENDIF

lnFound = ASCAN(laTSCls2,lcSCls21) 
lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls2[1,1]),lnFound,0)
IF lnFound = 0
  FOR lnCounter = 1 TO ALEN(laSSCls3,1)
    lnPos = IIF(OCCURS("\",laSSCls3[lnCounter,1])>0,2,1)
    laSSCls3[lnCounter,1] = SUBSTR(laSSCls3[lnCounter,1],lnPos,LEN(laSSCls3[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls3[1,2]
  laTSCls3=' '
  STORE 'DISABLE' TO lcSSCl3St,lcTSCl3St,lcAlCs3St,lcMvCs3St,lcRmCs3St,lcRAlCs3St
ELSE
  FOR lnCounter = 1 TO ALEN(laSSCls3,1)
    lnPos = IIF(OCCURS("\",laSSCls3[lnCounter,1])>0,2,1)
    laSSCls3[lnCounter,1] = SUBSTR(laSSCls3[lnCounter,1],lnPos,LEN(laSSCls3[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls3[1,2]
  laTSCls3 =' '
  SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
  =SEEK(lcDept+lcGroup+lcCatg+lcClass+lcSClass+lcSCls21,lcWorkFile)
  SELECT (lcWorkFile)
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  lcDept+lcGroup+lcCatg+lcClass+lcSClass+lcSCls21
    lcSClas3 = &lcWorkFile..cstysbcls3
    lnFound  = ASCAN(laSSCls3,lcSClas3) 
    IF lnFound > 0
      lsSSCls3  = ASUBSCRIPT(laSSCls3,lnFound,1)
      *-- Update Sub class 3 mover with the new selection     
      IF lsSSCls3 = 0 OR LEFT(laSSCls3[lsSSCls3,1],1) = "\"
        EXIT
      ENDIF

      IF lsSSCls3 <= ALEN('laSSCls3',1) AND lsSSCls3 <> 0
        IF !EMPTY(laTSCls3[1,1]) 
          DIMENSION laTSCls3[ALEN(laTSCls3,1)+1,2]
        ENDIF
        laTSCls3[ALEN(laTSCls3,1),1]= ALLTRIM(laSSCls3[lsSSCls3,1])
        laTSCls3[ALEN(laTSCls3,1),2]= ALLTRIM(laSSCls3[lsSSCls3,2])
        *-- When moving a record we replace "\" 
        *-- before the moved element in order to disable it.
        laSSCls3[lsSSCls3,1] = "\"+laSSCls3[lsSSCls3,1]
        IF lsSSCls3 < ALEN(laSSCls3,1)
          DO WHILE LEFT(laSSCls3[lsSSCls3,1],1) = "\" 
            lsSSCls3 = lsSSCls3 + 1
            IF lsSSCls3 = ALEN(laSSCls3,1)
              EXIT
            ENDIF
          ENDDO  
        ELSE
          lsSSCls3 = 1  
        ENDIF
      ENDIF  
  
      IF lsSSCls3 > ALEN(laSSCls3,1)
        lsSSCls3 = ALEN(laSSCls3,1)
      ENDIF
      
    ENDIF
    
  ENDSCAN
  *B605582,1 HBG 11/03/2002 Disable all movers in view mode[Begin]
  IF laScrMode[2]
    STORE 'DISABLE' TO lcSSCl3St,lcTSCl3St,lcAlCs3St,lcMvCs3St,lcRmCs3St,lcRAlCs3St
  ELSE
  *B605582,1 [End]
    STORE 'ENABLE' TO lcSSCl3St,lcTSCl3St,lcAlCs3St,lcMvCs3St,lcRmCs3St,lcRAlCs3St
  *B605582,1 HBG 11/03/2002 End if view mode[Begin]
  ENDIF
  *B605582,1 [End]

ENDIF
*!*************************************************************
*! Name      : lfChkCls
*! Auth      : HEND GHANEM (HBG).
*! Date      : 18/10/2001.
*! Purpose   : Check if this class selected in the 2nd folder
*!*************************************************************
*! Example   : =lfChkCls()
*!*************************************************************
FUNCTION lfChkCls
PARAMETERS lcCatgory,lcClass

IF EMPTY(lcClass)
  STORE SPACE(1) TO lcClass
ENDIF

*B605582,1 HBG 12/3/2002 It Shouldn't refresh the folder in previous folder according 
*B605582,1               to changes in the drop down list[Begin]
*DIMENSION laTClass[1,2]
*laTClass =' '
*FOR lnJ = 1 TO ALEN(laSClass,1)
*  lnPos = IIF(OCCURS("\",laSClass[lnJ,1])>0,2,1)
*  laSClass[lnJ,1] = SUBSTR(laSClass[lnJ,1],lnPos,LEN(laSClass[lnJ,1]))
*ENDFOR
*IF SEEK(lcDept+lcGroup+lcCatgory+lcClass,lcWorkFile)
*  lnI = 1
*  SELECT (lcWorkFile)
*  lcTmpCls = ' '
*  SCAN REST WHILE Dept+cStygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = lcDept+lcGroup+lcCatgory
*    IF lcTmpCls <> &lcWorkFile..cstyclass
*      DIMENSION laTClass[lnI,2]
*      lcTmpCls = &lcWorkFile..cstyclass
*      lnFound = ASCAN(laSClass,lcTmpCls ) 
*      IF lnFound > 0
*        lnFound = ASUBSCRIPT(laSClass,lnFound,1)
*        laTClass[lnI,1] = laSClass[lnFound,1]
*        laTClass[lnI,2] = laSClass[lnFound,2] 
*        laSClass[lnFound,1] = "\"+laSClass[lnFound,1]
*        lnI = lnI + 1
*      ENDIF
*    ENDIF  
*  ENDSCAN  
*ENDIF
*B605582,1 [End]

lnFound = ASCAN(laTClass,lcClass) 
lnFound = IIF(lnCls1<> 1,lnFound,0)
IF lnFound = 0
  FOR lnCounter = 1 TO ALEN(laSSCls1,1)
    lnPos = IIF(OCCURS("\",laSSCls1[lnCounter,1])>0 ,2,1)
    laSSCls1[lnCounter,1] = SUBSTR(laSSCls1[lnCounter,1],lnPos,LEN(laSSCls1[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls1[1,2]
  laTSCls1 =' '
  STORE 'DISABLE' TO lcSSCl1St,lcTSCl1St,lcAlCs1St,lcMvCs1St,lcRmCs1St,lcRAlCs1St
  SHOW GET lsSSCls1  DISABLE
  SHOW GET lsTSCls1  DISABLE
  SHOW GET pbAllCs1  DISABLE
  SHOW GET pbMovCs1  DISABLE
  SHOW GET pbRemCs1  DISABLE
  SHOW GET pbRAllCs1 DISABLE
ELSE
  FOR lnCounter = 1 TO ALEN(laSSCls1,1)
    lnPos = IIF(OCCURS("\",laSSCls1[lnCounter,1])>0,2,1)
    laSSCls1[lnCounter,1] = SUBSTR(laSSCls1[lnCounter,1],lnPos,LEN(laSSCls1[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls1[1,2]
  laTSCls1 =' '
  SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
  =SEEK(lcDept+lcGroup+lcCatgory+lcClass,lcWorkFile)
  SELECT (lcWorkFile)
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  lcDept+lcGroup+lcCatgory+lcClass
    lcSClas1 = &lcWorkFile..cstysbclas
    lnFound  = ASCAN(laSSCls1,lcSClas1) 
    IF lnFound > 0
      lsSSCls1  = ASUBSCRIPT(laSSCls1  ,lnFound,1)
      *-- Update Sub class 1 mover with the new selection     
      IF lsSSCls1 = 0 OR LEFT(laSSCls1[lsSSCls1,1],1) = "\"
        EXIT
      ENDIF

      IF lsSSCls1 <= ALEN('laSSCls1',1) AND lsSSCls1 <> 0
        IF !EMPTY(laTSCls1[1,1]) 
          DIMENSION laTSCls1[ALEN(laTSCls1,1)+1,2]
        ENDIF
        laTSCls1[ALEN(laTSCls1,1),1]= ALLTRIM(laSSCls1[lsSSCls1,1])
        laTSCls1[ALEN(laTSCls1,1),2]= ALLTRIM(laSSCls1[lsSSCls1,2])
        *-- When moving a record we replace "\" 
        *-- before the moved element in order to disable it.
        laSSCls1[lsSSCls1,1] = "\"+laSSCls1[lsSSCls1,1]
        IF lsSSCls1 < ALEN(laSSCls1,1)
          DO WHILE LEFT(laSSCls1[lsSSCls1,1],1) = "\" 
            lsSSCls1 = lsSSCls1 + 1
            IF lsSSCls1 = ALEN(laSSCls1,1)
              EXIT
            ENDIF
          ENDDO  
        ELSE
          lsSSCls1 = 1  
        ENDIF
      ENDIF  
  
      IF lsSSCls1 > ALEN(laSSCls1,1)
        lsSSCls1 = ALEN(laSSCls1,1)
      ENDIF
      
    ENDIF
    
  ENDSCAN
  llEnbMov = .F.
  FOR lnI = 1 TO ALEN(laSSCls1,1)
    IF OCCURS("\",laSSCls1[lnI,1]) = 0
      llEnbMov = .T.
      EXIT
    ENDIF
  ENDFOR
  IF llEnbMov 
    STORE 'ENABLE' TO lcSSCl1St,lcAlCs1St,lcMvCs1St
  ELSE 
    STORE 'DISABLE' TO lcSSCl1St,lcAlCs1St,lcMvCs1St  
  ENDIF

  llEnbRMov = .F.
  IF lsTSCls1 = 1 AND laTSCls1[lsTSCls1,1] <> ' '
    llEnbRMov = .T.
  ENDIF
  IF llEnbRMov 
    STORE 'ENABLE' TO lcTSCl1St,lcRmCs1St,lcRAlCs1St
  ELSE 
    STORE 'DISABLE' TO lcTSCl1St,lcRmCs1St,lcRAlCs1St
  ENDIF
  *B605582,1 HBG 11/03/2002 Disable all movers in view mode[Begin]
  IF laScrMode[2]
    STORE 'DISABLE' TO lcSSCl1St,lcTSCl1St,lcAlCs1St,lcMvCs1St,lcRmCs1St,lcRAlCs1St
  ENDIF
  *B605582,1 [End]
  SHOW GET lsSSCls1  &lcSSCl1St
  SHOW GET lsTSCls1  &lcTSCl1St
  SHOW GET pbAllCs1  &lcAlCs1St
  SHOW GET pbMovCs1  &lcMvCs1St
  SHOW GET pbRemCs1  &lcRmCs1St
  SHOW GET pbRAllCs1 &lcRAlCs1St
ENDIF

GO TOP IN (lcWorkFile)


=lfCls1Pop(lcCatgory,lcClass,'1')  


*!*************************************************************
*! Name      : lfChkCls1
*! Auth      : HEND GHANEM (HBG).
*! Date      : 18/10/2001.
*! Purpose   : Check if this sub class 1 selected in the 3nd folder
*!*************************************************************
*! Example   : =lfChkCls()
*!*************************************************************
FUNCTION lfChkCls1
PARAMETERS lcCatgory,lcClass,lcSClass1

IF EMPTY(lcSClass1)
  STORE SPACE(1) TO lcSClass1
ENDIF

*B605582,1 HBG 12/3/2002 It Shouldn't refresh the folder in previous folder according 
*B605582,1               to changes in the drop down list[Begin]
*DIMENSION laTSCls1[1,2]
*laTSCls1=' '
*FOR lnJ = 1 TO ALEN(laSSCls1,1)
*  lnPos = IIF(OCCURS("\",laSSCls1[lnJ,1])>0,2,1)
*  laSSCls1[lnJ,1] = SUBSTR(laSSCls1[lnJ,1],lnPos,LEN(laSSCls1[lnJ,1]))    
*ENDFOR
*IF SEEK(lcDept+lcGroup+lcCatgory+lcClass+lcSClass1,lcWorkFile)
*  lnI = 1
*  SELECT (lcWorkFile)
*  lcTmpCls1 = ' '
*  SCAN REST WHILE Dept+cStygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
*                  lcDept+lcGroup+lcCatgory+lcClass
*    IF lcTmpCls1 <> &lcWorkFile..cstysbclas
*      DIMENSION laTSCls1[lnI,2]                       
*      lcTmpCls1 = &lcWorkFile..cstysbclas
*      lnFound = ASCAN(laSSCls1,lcTmpCls1) 
*      IF lnFound > 0
*        lnFound = ASUBSCRIPT(laSSCls1,lnFound,1)
*        laTSCls1[lnI,1] = laSSCls1[lnFound,1]
*        laTSCls1[lnI,2] = laSSCls1[lnFound,2] 
*        laSSCls1[lnFound,1] = "\"+laSSCls1[lnFound,1]
*        lnI = lnI + 1
*      ENDIF
*    ENDIF  
*  ENDSCAN  
*ENDIF  
*B605582,1 [End]

lnFound = ASCAN(laTSCls1,lcSClass1) 
lnFound = IIF(lnSCls11<> 1,lnFound,0)
IF lnFound = 0
  FOR lnCounter = 1 TO ALEN(laSSCls2,1)
    lnPos = IIF(OCCURS("\",laSSCls2[lnCounter,1])>0,2,1)
    laSSCls2[lnCounter,1] = SUBSTR(laSSCls2[lnCounter,1],lnPos,LEN(laSSCls2[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls2[1,2]
  laTSCls2=' '
  STORE 'DISABLE' TO lcSSCl2St,lcTSCl2St,lcAlCs2St,lcMvCs2St,lcRmCs2St,lcRAlCs2St
  SHOW GET lsSSCls2  DISABLE
  SHOW GET lsTSCls2  DISABLE
  SHOW GET pbAllCs2  DISABLE
  SHOW GET pbMovCs2  DISABLE
  SHOW GET pbRemCs2  DISABLE
  SHOW GET pbRAllCs2 DISABLE

ELSE
  FOR lnCounter = 1 TO ALEN(laSSCls2,1)
    lnPos = IIF(OCCURS("\",laSSCls2[lnCounter,1])>0,2,1)
    laSSCls2[lnCounter,1] = SUBSTR(laSSCls2[lnCounter,1],lnPos,LEN(laSSCls2[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls2[1,2]
  laTSCls2 =' '
  SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
  =SEEK(lcDept+lcGroup+lcCatgory+lcClass+lcSClass1,lcWorkFile)
  SELECT (lcWorkFile)
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  lcDept+lcGroup+lcCatgory+lcClass+lcSClass1
    lcSClas2 = &lcWorkFile..cstysbcls2
    lnFound  = ASCAN(laSSCls2,lcSClas2) 
    IF lnFound > 0
      lsSSCls2  = ASUBSCRIPT(laSSCls2,lnFound,1)
      *-- Update Sub class 2 mover with the new selection     
      IF lsSSCls2 = 0 OR LEFT(laSSCls2[lsSSCls2,1],1) = "\"
        EXIT
      ENDIF

      IF lsSSCls2 <= ALEN('laSSCls2',1) AND lsSSCls2 <> 0
        IF !EMPTY(laTSCls2[1,1]) 
          DIMENSION laTSCls2[ALEN(laTSCls2,1)+1,2]
        ENDIF
        laTSCls2[ALEN(laTSCls2,1),1]= ALLTRIM(laSSCls2[lsSSCls2,1])
        laTSCls2[ALEN(laTSCls2,1),2]= ALLTRIM(laSSCls2[lsSSCls2,2])
        *-- When moving a record we replace "\" 
        *-- before the moved element in order to disable it.
        laSSCls2[lsSSCls2,1] = "\"+laSSCls2[lsSSCls2,1]
        IF lsSSCls2 < ALEN(laSSCls2,1)
          DO WHILE LEFT(laSSCls2[lsSSCls2,1],1) = "\" 
            lsSSCls2 = lsSSCls2 + 1
            IF lsSSCls2 = ALEN(laSSCls2,1)
              EXIT
            ENDIF
          ENDDO  
        ELSE
          lsSSCls2 = 1  
        ENDIF
      ENDIF  
  
      IF lsSSCls2 > ALEN(laSSCls2,1)
        lsSSCls2 = ALEN(laSSCls2,1)
      ENDIF
      
    ENDIF
    
  ENDSCAN
  llEnbMov = .F.
  FOR lnI = 1 TO ALEN(laSSCls2,1)
    IF OCCURS("\",laSSCls2[lnI,1]) = 0
      llEnbMov = .T.
      EXIT
    ENDIF
  ENDFOR
  IF llEnbMov 
    STORE 'ENABLE' TO lcSSCl2St,lcAlCs2St,lcMvCs2St
  ELSE 
    STORE 'DISABLE' TO lcSSCl2St,lcAlCs2St,lcMvCs2St  
  ENDIF

  llEnbRMov = .F.
  IF lsTSCls2 = 1 AND laTSCls2[lsTSCls2,1] <> ' '
    llEnbRMov = .T.
  ENDIF
  IF llEnbRMov 
    STORE 'ENABLE' TO lcTSCl2St,lcRmCs2St,lcRAlCs2St
  ELSE 
    STORE 'DISABLE' TO lcTSCl2St,lcRmCs2St,lcRAlCs2St
  ENDIF
  *B605582,1 HBG 11/03/2002 Disable all movers in view mode[Begin]
  IF laScrMode[2]
    STORE 'DISABLE' TO lcSSCl2St,lcTSCl2St,lcAlCs2St,lcMvCs2St,lcRmCs2St,lcRAlCs2St
  ENDIF
  *B605582,1 [End]

  SHOW GET lsSSCls2  &lcSSCl2St
  SHOW GET lsTSCls2  &lcTSCl2St
  SHOW GET pbAllCs2  &lcAlCs2St
  SHOW GET pbMovCs2  &lcMvCs2St
  SHOW GET pbRemCs2  &lcRmCs2St
  SHOW GET pbRAllCs2 &lcRAlCs2St
ENDIF

GO TOP IN (lcWorkFile)

=lfCls2Pop(lcCatgory,lcClass,lcSCls11,'21')
    
*!*************************************************************
*! Name      : lfChkCls2
*! Auth      : HEND GHANEM (HBG).
*! Date      : 18/10/2001.
*! Purpose   : Check if this sub class 2 selected in the 4th folder
*!*************************************************************
*! Example   : =lfChkCls2()
*!*************************************************************
FUNCTION lfChkCls2
PARAMETERS lcCatgory,lcClass,lcSClass1,lcSClass2


IF EMPTY(lcSClass2)
  STORE SPACE(1) TO lcSClass2
ENDIF

*B605582,1 HBG 12/3/2002 It Shouldn't refresh the folder in previous folder according 
*B605582,1               to changes in the drop down list[Begin]
*DIMENSION laTSCls2[1,2]
*laTSCls2=' '
*FOR lnJ = 1 TO ALEN(laSSCls2,1)
*  lnPos = IIF(OCCURS("\",laSSCls2[lnJ,1])>0,2,1)
*  laSSCls2[lnJ,1] = SUBSTR(laSSCls2[lnJ,1],lnPos,LEN(laSSCls2[lnJ,1]))    
*ENDFOR  
*IF SEEK(lcDept+lcGroup+lcCatgory+lcClass+lcSClass1+lcSClass2,lcWorkFile)
*  lnI = 1
*  SELECT (lcWorkFile)
*  lcTmpCls2 = ' '
*  SCAN REST WHILE Dept+cStygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
*                  lcDept+lcGroup+lcCatgory+lcClass+lcSClass1
*    IF lcTmpCls2 <> &lcWorkFile..cstysbcls2
*      DIMENSION laTSCls2[lnI,2]                  
*      lcTmpCls2 = &lcWorkFile..cstysbcls2
*      lnFound = ASCAN(laSSCls2,lcTmpCls2) 
*      IF lnFound > 0
*        lnFound = ASUBSCRIPT(laSSCls2,lnFound,1)
*        laTSCls2[lnI,1] = laSSCls2[lnFound,1]
*        laTSCls2[lnI,2] = laSSCls2[lnFound,2] 
*        laSSCls2[lnFound,1] = "\"+laSSCls2[lnFound,1]
*        lnI = lnI + 1
*      ENDIF
*    ENDIF  
*  ENDSCAN  
*ENDIF  
*B605582,1 [End]

lnFound = ASCAN(laTSCls2,lcSClass2) 
lnFound = IIF(lnSCls21 <> 1,lnFound,0)
IF lnFound = 0
  FOR lnCounter = 1 TO ALEN(laSSCls3,1)
    lnPos = IIF(OCCURS("\",laSSCls3[lnCounter,1])>0,2,1)
    laSSCls3[lnCounter,1] = SUBSTR(laSSCls3[lnCounter,1],lnPos,LEN(laSSCls3[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls3[1,2]
  laTSCls3=' '
  STORE 'DISABLE' TO lcSSCl3St,lcTSCl3St,lcAlCs3St,lcMvCs3St,lcRmCs3St,lcRAlCs3St
  SHOW GET lsSSCls3  DISABLE
  SHOW GET lsTSCls3  DISABLE
  SHOW GET pbAllCs3  DISABLE
  SHOW GET pbMovCs3  DISABLE
  SHOW GET pbRemCs3  DISABLE
  SHOW GET pbRAllCs3 DISABLE
ELSE
  FOR lnCounter = 1 TO ALEN(laSSCls3,1)
    lnPos = IIF(OCCURS("\",laSSCls3[lnCounter,1])>0,2,1)
    laSSCls3[lnCounter,1] = SUBSTR(laSSCls3[lnCounter,1],lnPos,LEN(laSSCls3[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls3[1,2]
  laTSCls3 =' '
  SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
  =SEEK(lcDept+lcGroup+lcCatgory+lcClass+lcSClass1+lcSClass2,lcWorkFile)
  SELECT (lcWorkFile)
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  lcDept+lcGroup+lcCatgory+lcClass+lcSClass1+lcSClass2
    lcSClas3 = &lcWorkFile..cstysbcls3
    lnFound  = ASCAN(laSSCls3,lcSClas3) 
    IF lnFound > 0
      *-- Update Sub class 3 mover with the new selection     
      lsSSCls3  = ASUBSCRIPT(laSSCls3,lnFound,1)
     
      IF lsSSCls3 = 0 OR LEFT(laSSCls3[lsSSCls3,1],1) = "\"
        EXIT
      ENDIF

      IF lsSSCls3 <= ALEN('laSSCls3',1) AND lsSSCls3 <> 0
        IF !EMPTY(laTSCls3[1,1]) 
          DIMENSION laTSCls3[ALEN(laTSCls3,1)+1,2]
        ENDIF
        laTSCls3[ALEN(laTSCls3,1),1]= ALLTRIM(laSSCls3[lsSSCls3,1])
        laTSCls3[ALEN(laTSCls3,1),2]= ALLTRIM(laSSCls3[lsSSCls3,2])
        *-- When moving a record we replace "\" 
        *-- before the moved element in order to disable it.
        laSSCls3[lsSSCls3,1] = "\"+laSSCls3[lsSSCls3,1]
        IF lsSSCls3 < ALEN(laSSCls3,1)
          DO WHILE LEFT(laSSCls3[lsSSCls3,1],1) = "\" 
            lsSSCls3 = lsSSCls3 + 1
            IF lsSSCls3 = ALEN(laSSCls3,1)
              EXIT
            ENDIF
          ENDDO  
        ELSE
          lsSSCls3 = 1  
        ENDIF
      ENDIF  
  
      IF lsSSCls3 > ALEN(laSSCls3,1)
        lsSSCls3 = ALEN(laSSCls3,1)
      ENDIF
      
    ENDIF
  ENDSCAN

  llEnbMov = .F.
  FOR lnI = 1 TO ALEN(laSSCls3,1)
    IF OCCURS("\",laSSCls3[lnI,1]) = 0
      llEnbMov = .T.
      EXIT
    ENDIF
  ENDFOR
  IF llEnbMov 
    STORE 'ENABLE' TO lcSSCl3St,lcAlCs3St,lcMvCs3St
  ELSE 
    STORE 'DISABLE' TO lcSSCl3St,lcAlCs3St,lcMvCs3St  
  ENDIF

  llEnbRMov = .F.
  IF lsTSCls3 = 1 AND laTSCls3[lsTSCls3,1] <> ' '
    llEnbRMov = .T.
  ENDIF
  IF llEnbRMov 
    STORE 'ENABLE' TO lcTSCl3St,lcRmCs3St,lcRAlCs3St
  ELSE 
    STORE 'DISABLE' TO lcTSCl3St,lcRmCs3St,lcRAlCs3St
  ENDIF
  *B605582,1 HBG 11/03/2002 Disable all movers in view mode[Begin]
  IF laScrMode[2]
    STORE 'DISABLE' TO lcSSCl3St,lcTSCl3St,lcAlCs3St,lcMvCs3St,lcRmCs3St,lcRAlCs3St
  ENDIF
  *B605582,1 [End]
  
  SHOW GET lsSSCls3  &lcSSCl3St
  SHOW GET lsTSCls3  &lcTSCl3St
  SHOW GET pbAllCs3  &lcAlCs3St
  SHOW GET pbMovCs3  &lcMvCs3St
  SHOW GET pbRemCs3  &lcRmCs3St
  SHOW GET pbRAllCs3 &lcRAlCs3St
ENDIF

GO TOP IN (lcWorkFile)


*!*************************************************************
*! Name      : lfMoveAll
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Move all function
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfMoveAll()
*!*************************************************************
FUNCTION lfMoveAll
PARAMETERS lcItemMov
DO CASE 
   CASE lcItemMov = 'CG'
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     FOR lnI = 1 To ALEN(laSCatgry,1)
       IF SEEK(lcDept+lcGroup) AND EMPTY(cstycatg)     
         REPLACE cstycatg WITH laSCatgry[lnI,2]
       ELSE  
         IF !SEEK(lcDept+lcGroup+laSCatgry[lnI,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg);
                             VALUES (lcDept,lcGroup,laSCatgry[lnI,2])
         ENDIF
       ENDIF  
     ENDFOR
     =lfvMovment('CG',2)
     lnFound = ASCAN(laCat1,laTCatgry[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTCatgry[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laCat1,lnFound,1)
       STORE lnFound TO lnCat1,lnCat2,lnCat3,lnCat4
     ELSE  
       STORE 1 TO lnCat1,lnCat2,lnCat3,lnCat4
     ENDIF   
     
     =lfClsPop(laCat1[lnCat1,2],'1')
          
   CASE lcItemMov = 'CS'

     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     FOR lnI = 1 To ALEN(laSClass,1)
       IF SEEK(lcDept+lcGroup+laCat1[lnCat1,2]) AND EMPTY(cstyclass)     
         REPLACE cstyclass WITH laSClass[lnI,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laCat1[lnCat1,2]+laSClass[lnI,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass);
                        VALUES (lcDept,lcGroup,laCat1[lnCat1,2],laSClass[lnI,2])
         ENDIF                     
       ENDIF
     ENDFOR
     =lfvMovment('CS',2)     
     lnFound = ASCAN(laCls1,laTClass[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTClass[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laCls1,lnFound,1)
       STORE lnFound TO lnCls1,lnCls2,lnCls3
     ELSE  
       STORE 1 TO lnCls1,lnCls2,lnCls3
     ENDIF   
     
     =lfCls1Pop(laCat2[lnCat2,2],laCls1[lnCls1,2],'1')
          
   CASE lcItemMov = 'S1'
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     FOR lnI = 1 To ALEN(laSSCls1,1)
       IF SEEK(lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]) AND EMPTY(cstysbclas)     
         REPLACE cstysbclas WITH laSSCls1[lnI,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+laSSCls1[lnI,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass,cstysbclas);
                      VALUES (lcDept,lcGroup,laCat2[lnCat2,2],laCls1[lnCls1,2],laSSCls1[lnI,2])
         ENDIF                     
       ENDIF
     ENDFOR
     =lfvMovment('S1',2)     
     lnFound = ASCAN(laSCls11,laTSCls1[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls1[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laSCls11,lnFound,1)
       STORE lnFound TO lnSCls11,lnSCls12
     ELSE  
       STORE 1 TO lnSCls11,lnSCls12
     ENDIF   
     
     =lfCls2Pop(laCat3[lnCat3,2],laCls2[lnCls2,2],laSCls11[lnSCls11,2],'21')
          
   CASE lcItemMov = 'S2'
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     FOR lnI = 1 To ALEN(laSSCls2,1)
       IF SEEK(lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]) AND EMPTY(cstysbcls2)         
         REPLACE cstysbcls2 WITH laSSCls2[lnI,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+laSSCls2[lnI,2])
            INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass,cstysbclas,cstysbcls2);
                         VALUES (lcDept,lcGroup,laCat3[lnCat3,2],laCls2[lnCls2,2],;
                              laSCls11[lnSCls11,2],laSSCls2[lnI,2])
         ENDIF                     
       ENDIF
     ENDFOR
     =lfvMovment('S2',2)     
     lnFound = ASCAN(laSCls21,laTSCls2[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls2[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laSCls21,lnFound,1)
       STORE lnFound TO lnSCls21
     ELSE  
       STORE 1 TO lnSCls21
     ENDIF  
     
   CASE lcItemMov = 'S3'        

     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     FOR lnI = 1 To ALEN(laSSCls3,1)
       IF SEEK(lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+laSCls21[lnSCls21,2]) ;
             AND EMPTY(cstysbcls3)     
         REPLACE cstysbcls3 WITH laSSCls3[lnI,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+laSCls21[lnSCls21,2]+;
             laSSCls3[lnI,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass,cstysbclas,cstysbcls2,;
                                cstysbcls3);
                      VALUES (lcDept,lcGroup,laCat4[lnCat4,2],laCls3[lnCls3,2],laSCls12[lnSCls12,2],;
                              laSCls21[lnSCls21,2],laSSCls3[lnI,2])
         ENDIF                     
       ENDIF
     ENDFOR
     =lfvMovment('S3',2)     
ENDCASE   
*C200505,1 TMI [Start] Add selected items to all upper levels
=lfAdToOthr(.T.)
*C200505,1 TMI [End  ] 
*!*************************************************************
*! Name      : lfMove
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Move function
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfMove()
*!*************************************************************
FUNCTION lfMove
PARAMETERS lcItemMov

DO CASE 
   CASE lcItemMov = 'CG'
     lnIndex = lsSCatgry
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF lnIndex <> 0
       IF SEEK(lcDept+lcGroup) AND EMPTY(cstycatg)
         REPLACE cstycatg WITH laSCatgry[lnIndex,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laSCatgry[lnIndex ,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg);
                             VALUES (lcDept,lcGroup,laSCatgry[lnIndex ,2])
         ENDIF
       ENDIF  
     ENDIF  
     =lfvMovment('CG',1)     
     lnFound = ASCAN(laCat1,laTCatgry[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTCatgry[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laCat1,lnFound,1)
       STORE lnFound TO lnCat1,lnCat2,lnCat3,lnCat4
     ELSE  
       STORE 1 TO lnCat1,lnCat2,lnCat3,lnCat4
     ENDIF   

     =lfClsPop(laCat1[lnCat1,2],'1')
   CASE lcItemMov = 'CS'
     lnIndex = lsSClass
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF lnIndex <> 0
       IF SEEK(lcDept+lcGroup+laCat1[lnCat1,2]) AND EMPTY(cstyclass)     
         REPLACE cstyclass WITH laSClass[lnIndex,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laCat1[lnCat1,2]+laSClass[lnIndex,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass);
                        VALUES (lcDept,lcGroup,laCat1[lnCat1,2],laSClass[lnIndex,2])
         ENDIF                     
       ENDIF
     ENDIF
     =lfvMovment('CS',1)
     lnFound = ASCAN(laCls1,laTClass[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTClass[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laCls1,lnFound,1)
       STORE lnFound TO lnCls1,lnCls2,lnCls3
     ELSE  
       STORE 1 TO lnCls1,lnCls2,lnCls3
     ENDIF
     
     =lfCls1Pop(laCat2[lnCat2,2],laCls1[lnCls1,2],'1')
     
   CASE lcItemMov = 'S1'
     lnIndex = lsSSCls1

     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF lnIndex <> 0
       IF SEEK(lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]) AND EMPTY(cstysbclas)     
         REPLACE cstysbclas WITH laSSCls1[lnIndex,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+laSSCls1[lnIndex,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass,cstysbclas);
                        VALUES (lcDept,lcGroup,laCat2[lnCat2,2],laCls1[lnCls1,2],laSSCls1[lnIndex,2])
         ENDIF                     
       ENDIF
     ENDIF
     =lfvMovment('S1',1)     
     lnFound = ASCAN(laSCls11,laTSCls1[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls1[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laSCls11,lnFound,1)
       STORE lnFound TO lnSCls11,lnSCls12
     ELSE  
       STORE 1 TO lnSCls11,lnSCls12
     ENDIF
     
     =lfCls2Pop(laCat3[lnCat3,2],laCls2[lnCls2,2],laSCls11[lnSCls11,2],'21')
         
   CASE lcItemMov = 'S2'
     lnIndex = lsSSCls2

     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF lnIndex <> 0
       IF SEEK(lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]) AND EMPTY(cstysbcls2)       
         REPLACE cstysbcls2 WITH laSSCls2[lnIndex ,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+laSSCls2[lnIndex ,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass,cstysbclas,cstysbcls2);
                             VALUES (lcDept,lcGroup,laCat3[lnCat3,2],laCls2[lnCls2,2],;
                                     laSCls11[lnSCls11,2],laSSCls2[lnIndex ,2])
         ENDIF                     
       ENDIF
     ENDIF
     =lfvMovment('S2',1)
     lnFound = ASCAN(laSCls21,laTSCls2[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls2[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laSCls21,lnFound,1)
       STORE lnFound TO lnSCls21
     ELSE  
       STORE 1 TO lnSCls21
     ENDIF
          
   CASE lcItemMov = 'S3'        
     lnIndex = lsSSCls3

     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF lnIndex <> 0     
       IF SEEK(lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+laSCls21[lnSCls21,2]);
           AND EMPTY(cstysbcls3)     
         REPLACE cstysbcls3 WITH laSSCls3[lnIndex,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+laSCls21[lnSCls21,2]+;
                  laSSCls3[lnIndex,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass,cstysbclas,cstysbcls2,;
                                cstysbcls3);
                    VALUES (lcDept,lcGroup,laCat4[lnCat4,2],laCls3[lnCls3,2],laSCls12[lnSCls12,2],;
                            laSCls21[lnSCls21,2],laSSCls3[lnIndex,2])
         ENDIF                     
       ENDIF
     ENDIF  
     =lfvMovment('S3',1)
ENDCASE   

*C200505,1 TMI [Start] Add one selected item to all upper levels
=lfAdToOthr()
*C200505,1 TMI [End  ] 

*!*************************************************************
*! Name      : lfRemovAll
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Remove All function
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfRemovAll()
*!*************************************************************
FUNCTION lfRemovAll
PARAMETERS lcItemMov

*C200505,1 TMI [Start] Save the selected items in "laSavSlct" array to use in "lfRmvOther" fn.
PRIVATE laSavSlct
DIMENSION laSavSlct[1,2]
=lfSavarr()
*C200505,1 TMI [End  ] 

DO CASE 
   CASE lcItemMov = 'CG'
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     FOR lnI = 1 To ALEN(laTCatgry,1)
       IF SEEK(lcDept+lcGroup+laTCatgry[lnI,2])
         SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = ;
                         lcDept+lcGroup+laTCatgry[lnI,2]
           DELETE
         ENDSCAN  
       ENDIF
     ENDFOR

     =lfvMovment('CG',4)
     
     lnFound = ASCAN(laCat1,laTCatgry[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTCatgry[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laCat1,lnFound,1)
       STORE lnFound TO lnCat1,lnCat2,lnCat3,lnCat4
     ELSE  
       STORE 1 TO lnCat1,lnCat2,lnCat3,lnCat4,lnCls1,lnCls2,lnCls3,lnSCls11,lnSCls12,lnSCls21
     ENDIF   
     
     =lfCatMov(laCat1[lnCat1,2])    
     =lfClsPop(laCat1[lnCat1,2],'1')
               
   CASE lcItemMov = 'CS'
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     FOR lnI = 1 To ALEN(laTClass,1)
       IF SEEK(lcDept+lcGroup+laCat1[lnCat1,2]+laTClass[lnI,2])
         SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = ;
                         lcDept+lcGroup+laCat1[lnCat1,2]+laTClass[lnI,2]
           REPLACE cstyclass  WITH '',;
                   cstysbclas WITH '',;
                   cstysbcls2 WITH '',;
                   cstysbcls3 WITH ''
         ENDSCAN          
       ENDIF
     ENDFOR
     *C200505,1 TMI [Start] remove all selected items from all upper levels
     =lfRmvOther(.T.)
     *C200505,1 TMI [End  ] 

     =lfvMovment('CS',4)
    
     lnFound = ASCAN(laCls1,laTClass[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTClass[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laCls1,lnFound,1)
       STORE lnFound TO lnCls1,lnCls2,lnCls3
     ELSE  
       STORE 1 TO lnCls1,lnCls2,lnCls3,lnSCls11,lnSCls12,lnSCls21
     ENDIF

     =lfClsMov(laCat2[lnCat2,2],laCls1[lnCls1,2])    
     =lfCls1Pop(laCat2[lnCat2,2],laCls1[lnCls1,2],'1')
     
   CASE lcItemMov = 'S1'
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     FOR lnI = 1 To ALEN(laTSCls1,1)
       IF SEEK(lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+laTSCls1[lnI,2])
         SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = ;
                         lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+laTSCls1[lnI,2]
           REPLACE cstysbclas WITH '',;
                   cstysbcls2 WITH '',;
                   cstysbcls3 WITH ''
         ENDSCAN
       ENDIF
     ENDFOR

     *C200505,1 TMI [Start] remove all selected items from all upper levels
     =lfRmvOther(.T.)
     *C200505,1 TMI [End  ] 
     =lfvMovment('S1',4)

     lnFound = ASCAN(laSCls11,laTSCls1[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls1[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laSCls11,lnFound,1)
       STORE lnFound TO lnSCls11,lnSCls12
     ELSE  
       STORE 1 TO lnSCls11,lnSCls12,lnSCls21
     ENDIF   

     =lfCls1Mov(laCat3[lnCat3,2],laCls2[lnCls2,2],laSCls11[lnSCls11,2])    
     =lfCls2Pop(laCat3[lnCat3,2],laCls2[lnCls2,2],laSCls11[lnSCls11,2],'21')
     
   CASE lcItemMov = 'S2'
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     FOR lnI = 1 To ALEN(laTSCls2,1)
       IF SEEK(lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+laTSCls2[lnI,2])
         SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = ;
                         lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+;
                         laTSCls2[lnI,2]

           REPLACE cstysbcls2 WITH '',;
                   cstysbcls3 WITH ''
         ENDSCAN
      ENDIF
     ENDFOR
     *C200505,1 TMI [Start] remove all selected items from all upper levels
     =lfRmvOther(.T.)
     *C200505,1 TMI [End  ] 
     
     =lfvMovment('S2',4)

     lnFound = ASCAN(laSCls21,laTSCls2[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls2[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laSCls21,lnFound,1)
       STORE lnFound TO lnSCls21
     ELSE  
       STORE 1 TO lnSCls21
     ENDIF  

     =lfCls2Mov(laCat4[lnCat4,2],laCls3[lnCls3,2],laSCls12[lnSCls12,2],laSCls21[lnSCls21,2])    
     
   CASE lcItemMov = 'S3'        
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     FOR lnI = 1 To ALEN(laTSCls3,1)
       IF SEEK(lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+laSCls21[lnSCls21,2]+;
               laTSCls3[lnI,2])
         SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = ;
                         lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+;
                         laSCls21[lnSCls21,2]+laTSCls3[lnI,2]
           REPLACE cstysbcls3 WITH ''
         EnDSCAN           
       ENDIF
     ENDFOR
     *C200505,1 TMI [Start] remove all selected items from all upper levels
     =lfRmvOther(.T.)
     *C200505,1 TMI [End  ] 
     
     =lfvMovment('S3',4)
ENDCASE   

*!*************************************************************
*! Name      : lfRemov
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Remove function
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfRemov()
*!*************************************************************
FUNCTION lfRemov
PARAMETERS lcItemMov

*C200505,1 TMI [Start] Save the selected items in "laSavSlct" array to use in "lfRmvOther" fn.
PRIVATE laSavSlct
DIMENSION laSavSlct[1,2]
=lfSavarr()
*C200505,1 TMI [End  ] 

DO CASE 
   CASE lcItemMov = 'CG'
     
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF SEEK(lcDept+lcGroup+laTCatgry[lsTCatgry,2])
       SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3  = ;
                       lcDept+lcGroup+laTCatgry[lsTCatgry,2]
         DELETE
       ENDSCAN  
     ENDIF

     =lfvMovment('CG',3)

     lnFound = ASCAN(laCat1,laTCatgry[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTCatgry[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laCat1,lnFound,1)
       STORE lnFound TO lnCat1,lnCat2,lnCat3,lnCat4
     ELSE  
       STORE 1 TO lnCat1,lnCat2,lnCat3,lnCat4,lnCls1,lnCls2,lnCls3,lnSCls11,lnSCls12,lnSCls21
     ENDIF   
     =lfCatMov(laCat1[lnCat1,2])    
     =lfClsPop(laCat1[lnCat1,2],'1')
     
   CASE lcItemMov = 'CS'
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF SEEK(lcDept+lcGroup+laCat1[lnCat1,2]+laTClass[lsTClass,2])
       SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3  = ;
                       lcDept+lcGroup+laCat1[lnCat1,2]+laTClass[lsTClass,2]
         REPLACE cstyclass  WITH '',;
                 cstysbclas WITH '',;
                 cstysbcls2 WITH '',;
                 cstysbcls3 WITH ''
       ENDSCAN         
       IF (lsTClass <> 1 AND !EMPTY(laTClass[lsTClass-1,2])) OR ;
          (lsTClass = 1  AND ALEN(laTClass,1) > 1 AND !EMPTY(laTClass[lsTClass+1,2])) 
         =SEEK(lcDept+lcGroup+laCat1[lnCat1,2]+' ')
         SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3  = ;
                       lcDept+lcGroup+laCat1[lnCat1,2]+' '
           DELETE
         ENDSCAN  
       ENDIF
     ENDIF
     *C200505,1 TMI [Start] remove only one selected items from all upper levels
     =lfRmvOther()
     *C200505,1 TMI [End  ] 

     =lfvMovment('CS',3)
     
     lnFound = ASCAN(laCls1,laTClass[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTClass[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laCls1,lnFound,1)
       STORE lnFound TO lnCls1,lnCls2,lnCls3
     ELSE  
       STORE 1 TO lnCls1,lnCls2,lnCls3,lnSCls11,lnSCls12,lnSCls21
     ENDIF
     
     =lfClsMov(laCat2[lnCat2,2],laCls1[lnCls1,2])    
     =lfCls1Pop(laCat2[lnCat2,2],laCls1[lnCls1,2],'1')
     
   CASE lcItemMov = 'S1'
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF SEEK(lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+laTSCls1[lsTSCls1,2])
       SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3  = ;
                       lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+laTSCls1[lsTSCls1,2]
         REPLACE cstysbclas WITH '',;
                 cstysbcls2 WITH '',;
                 cstysbcls3 WITH ''
       ENDSCAN          
       IF (lsTSCls1 <> 1 AND !EMPTY(laTSCls1[lsTSCls1-1,2])) OR;
          (lsTSCls1 = 1 AND ALEN(laTSCls1,1) > 1 AND !EMPTY(laTSCls1[lsTSCls1+1,2]))
         =SEEK(lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+' ')
         SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3  = ;
                       lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+' '
           DELETE
         ENDSCAN  
       ENDIF
     ENDIF
     *C200505,1 TMI [Start] remove only one selected items from all upper levels
     =lfRmvOther()
     *C200505,1 TMI [End  ] 

     =lfvMovment('S1',3)

     lnFound = ASCAN(laSCls11,laTSCls1[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls1[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laSCls11,lnFound,1)
       STORE lnFound TO lnSCls11,lnSCls12
     ELSE  
       STORE 1 TO lnSCls11,lnSCls12,lnSCls21
     ENDIF   
     
     =lfCls1Mov(laCat3[lnCat3,2],laCls2[lnCls2,2],laSCls11[lnSCls11,2])    
     =lfCls2Pop(laCat3[lnCat3,2],laCls2[lnCls2,2],laSCls11[lnSCls11,2],'21')
     
   CASE lcItemMov = 'S2'
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF SEEK(lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+laTSCls2[lsTSCls2,2])
       SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3  = ;
                       lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+;
                       laTSCls2[lsTSCls2,2]

         REPLACE cstysbcls2 WITH '',;
                 cstysbcls3 WITH ''
       ENDSCAN           
       IF (lsTSCls2 <> 1 AND !EMPTY(laTSCls2[lsTSCls2-1,2])) OR;
          (lsTSCls2 = 1 AND ALEN(laTSCls2,1) > 1 AND !EMPTY(laTSCls2[lsTSCls2+1,2]))
         =SEEK(lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+' ')
         SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3  = ;
                       lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+' '
           DELETE
         ENDSCAN  
       ENDIF

     ENDIF
     *C200505,1 TMI [Start] remove only one selected items from all upper levels
     =lfRmvOther()
     *C200505,1 TMI [End  ] 

     =lfvMovment('S2',3)

     lnFound = ASCAN(laSCls21,laTSCls2[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls2[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laSCls21,lnFound,1)
       STORE lnFound TO lnSCls21
     ELSE  
       STORE 1 TO lnSCls21
     ENDIF  
     
     =lfCls2Mov(laCat4[lnCat4,2],laCls3[lnCls3,2],laSCls12[lnSCls12,2],laSCls21[lnSCls21,2])    
     
   CASE lcItemMov = 'S3'        
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF SEEK(lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+laSCls21[lnSCls21,2]+;
             laTSCls3[lsTSCls3,2])
       SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3  = ;
                       lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+;
                       laSCls21[lnSCls21,2]+;
             laTSCls3[lsTSCls3,2]

         REPLACE cstysbcls3 WITH ''
       ENDSCAN  
       IF (lnSCls21 <> 1 AND !EMPTY(laSCls21[lnSCls21-1,2])) OR;
          (lnSCls21 = 1 AND ALEN(laTSCls3,1) > 1 AND !EMPTY(laSCls21[lnSCls21+1,2]))
         =SEEK(lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+;
                       laSCls21[lnSCls21,2]+' ')
         SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3  = ;
                       lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+;
                       laSCls21[lnSCls21,2]+' ' 
           DELETE
         ENDSCAN  
       ENDIF
     ENDIF
     *C200505,1 TMI [Start] remove only one selected items from all upper levels
     =lfRmvOther()
     *C200505,1 TMI [End  ] 
     
     =lfvMovment('S3',3)

ENDCASE   

*!*************************************************************
*! Name      : lfFillFile
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Collect data in temp file
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfFillFile()
*!*************************************************************
FUNCTION lfFillFile
PARAMETER lcDepart,lcGrp,llCopy

IF !llCopy
  INSERT INTO (lcWorkFile) (dept,cstygroup);
                  VALUES   (lcDepart,lcGrp)
ELSE 
  SELECT ICCLSSTP
  IF SEEK(lcDepart+lcGrp)                  
    SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+;
                    cstysbcls3 = lcDepart+lcGrp
      SCATTER MEMVAR MEMO
      m.dept      = lcDept
      m.cstygroup = lcGroup
      INSERT INTO (lcWorkFile) FROM MEMVAR                     
    ENDSCAN                
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfvSrClick
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Validation of source mover
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSrClick()
*!*************************************************************
FUNCTION lfvSrClick
PARAMETERS lcItemMov


DO CASE 
   CASE lcItemMov = 'CG'
     lnIndex = lsSCatgry
     *B605582,1 HBG 11/03/2002 Update the file befor move the item in mover[Begin]
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF lnIndex <> 0
       IF SEEK(lcDept+lcGroup) AND EMPTY(cstycatg)
         REPLACE cstycatg WITH laSCatgry[lnIndex,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laSCatgry[lnIndex ,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg);
                             VALUES (lcDept,lcGroup,laSCatgry[lnIndex ,2])
         ENDIF
       ENDIF  
     ENDIF 
     *B605582,1 [End]
     =lfvSource('CG')
     *B605582,1 HBG 11/03/2002 Move this block of code to Update the file befor move the item in mover[Begin]
     *SELECT (lcWorkFile)
     *SET ORDER TO TAG (lcWorkFile)
     *IF lnIndex <> 0
     *  IF SEEK(lcDept+lcGroup) AND EMPTY(cstycatg)
     *    REPLACE cstycatg WITH laSCatgry[lnIndex,2]
     *  ELSE
     *    IF !SEEK(lcDept+lcGroup+laSCatgry[lnIndex ,2])
     *      INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg);
     *                        VALUES (lcDept,lcGroup,laSCatgry[lnIndex ,2])
     *    ENDIF
     *  ENDIF  
     *ENDIF 
     *B605582,1 [End]
      
     lnFound = ASCAN(laCat1,laTCatgry[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTCatgry[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laCat1,lnFound,1)
       STORE lnFound TO lnCat1,lnCat2,lnCat3,lnCat4
     ELSE  
       STORE 1 TO lnCat1,lnCat2,lnCat3,lnCat4
     ENDIF   
     
     =lfClsPop(laCat1[lnCat1,2],'1')
     
   CASE lcItemMov = 'CS'
     lnIndex = lsSClass
     *B605582,1 HBG 11/03/2002 Update the file befor move the item in mover[Begin]
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF lnIndex <> 0     
       IF SEEK(lcDept+lcGroup+laCat1[lnCat1,2]) AND EMPTY(cstyclass)     
         REPLACE cstyclass WITH laSClass[lnIndex,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laCat1[lnCat1,2]+laSClass[lnIndex,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass);
                        VALUES (lcDept,lcGroup,laCat1[lnCat1,2],laSClass[lnIndex,2])
         ENDIF                     
       ENDIF
     ENDIF 
     *B605582,1 [End]
     =lfvSource('CS')
     *B605582,1 HBG 11/03/2002 Move this block of code to Update the file befor move the item in mover[Begin]
     *SELECT (lcWorkFile)
     *SET ORDER TO TAG (lcWorkFile)
     *IF lnIndex <> 0     
     *  IF SEEK(lcDept+lcGroup+laCat1[lnCat1,2]) AND EMPTY(cstyclass)     
     *    REPLACE cstyclass WITH laSClass[lnIndex,2]
     *  ELSE
     *    IF !SEEK(lcDept+lcGroup+laCat1[lnCat1,2]+laSClass[lnIndex,2])
     *      INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass);
     *                   VALUES (lcDept,lcGroup,laCat1[lnCat1,2],laSClass[lnIndex,2])
     *    ENDIF                     
     *  ENDIF
     *ENDIF 
     *B605582,1 [End]
     
     lnFound = ASCAN(laCls1,laTClass[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTClass[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laCls1,lnFound,1)
       STORE lnFound TO lnCls1,lnCls2,lnCls3
     ELSE  
       STORE 1 TO lnCls1,lnCls2,lnCls3
     ENDIF
     
     =lfCls1Pop(laCat2[lnCat2,2],laCls1[lnCls1,2],'1')
     
   CASE lcItemMov = 'S1'
     lnIndex = lsSSCls1
     *B605582,1 HBG 11/03/2002 Update the file befor move the item in mover[Begin]
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF lnIndex <> 0
       IF SEEK(lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]) AND EMPTY(cstysbclas)     
         REPLACE cstysbclas WITH laSSCls1[lnIndex,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+laSSCls1[lnIndex,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass,cstysbclas);
                        VALUES (lcDept,lcGroup,laCat2[lnCat2,2],laCls1[lnCls1,2],laSSCls1[lnIndex,2])
         ENDIF                     
       ENDIF
     ENDIF
     *B605582,1 [End]
     =lfvSource('S1')
     *B605582,1 HBG 11/03/2002 Mode thid block of code to Update the file befor move the item in mover[Begin]
     *SELECT (lcWorkFile)
     *SET ORDER TO TAG (lcWorkFile)
     *IF lnIndex <> 0
     *  IF SEEK(lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]) AND EMPTY(cstysbclas)     
     *    REPLACE cstysbclas WITH laSSCls1[lnIndex,2]
     *  ELSE
     *    IF !SEEK(lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+laSSCls1[lnIndex,2])
     *      INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass,cstysbclas);
     *                   VALUES (lcDept,lcGroup,laCat2[lnCat2,2],laCls1[lnCls1,2],laSSCls1[lnIndex,2])
     *    ENDIF                     
     *  ENDIF
     *ENDIF
     *B605582,1 [End]
     lnFound = ASCAN(laSCls11,laTSCls1[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls1[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laSCls11,lnFound,1)
       STORE lnFound TO lnSCls11,lnSCls12
     ELSE  
       STORE 1 TO lnSCls11,lnSCls12
     ENDIF
     
     =lfCls2Pop(laCat3[lnCat3,2],laCls2[lnCls2,2],laSCls11[lnSCls11,2],'21')
         
   CASE lcItemMov = 'S2'
     lnIndex = lsSSCls2
     *B605582,1 HBG 11/03/2002 Update the file befor move the item in mover[Begin]
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF lnIndex <> 0
       IF SEEK(lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]) AND EMPTY(cstysbcls2)       
         REPLACE cstysbcls2 WITH laSSCls2[lnIndex ,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+laSSCls2[lnIndex ,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass,cstysbclas,cstysbcls2);
                             VALUES (lcDept,lcGroup,laCat3[lnCat3,2],laCls2[lnCls2,2],;
                                     laSCls11[lnSCls11,2],laSSCls2[lnIndex ,2])
         ENDIF                     
       ENDIF
     ENDIF
     *B605582,1 [End]

     =lfvSource('S2')
     *B605582,1 HBG 11/03/2002 Move this block of code to Update the file befor move the item in mover[Begin]
     *SELECT (lcWorkFile)
     *SET ORDER TO TAG (lcWorkFile)
     *IF lnIndex <> 0
     *  IF SEEK(lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]) AND EMPTY(cstysbcls2)       
     *    REPLACE cstysbcls2 WITH laSSCls2[lnIndex ,2]
     *  ELSE
     *    IF !SEEK(lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+laSSCls2[lnIndex ,2])
     *      INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass,cstysbclas,cstysbcls2);
     *                        VALUES (lcDept,lcGroup,laCat3[lnCat3,2],laCls2[lnCls2,2],;
     *                                laSCls11[lnSCls11,2],laSSCls2[lnIndex ,2])
     *    ENDIF                     
     *  ENDIF
     *ENDIF
     *B605582,1 [End]
     lnFound = ASCAN(laSCls21,laTSCls2[1,1])
     lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls2[1,1]),lnFound,0)
     IF lnFound > 0
       lnFound = ASUBSCRIPT(laSCls21,lnFound,1)
       STORE lnFound TO lnSCls21
     ELSE  
       STORE 1 TO lnSCls21
     ENDIF
          
   CASE lcItemMov = 'S3'        
     lnIndex = lsSSCls3
     *B605582,1 HBG 11/03/2002 Update the file befor move the item in mover[Begin]
     SELECT (lcWorkFile)
     SET ORDER TO TAG (lcWorkFile)
     IF lnIndex <> 0
       IF SEEK(lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+laSCls21[lnSCls21,2]);
           AND EMPTY(cstysbcls3)     
         REPLACE cstysbcls3 WITH laSSCls3[lnIndex,2]
       ELSE
         IF !SEEK(lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+laSCls21[lnSCls21,2]+;
                  laSSCls3[lnIndex,2])
           INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass,cstysbclas,cstysbcls2,;
                                cstysbcls3);
                    VALUES (lcDept,lcGroup,laCat4[lnCat4,2],laCls3[lnCls3,2],laSCls12[lnSCls12,2],;
                            laSCls21[lnSCls21,2],laSSCls3[lnIndex,2])
         ENDIF                     
       ENDIF
     ENDIF
     *B605582,1 [End]
     =lfvSource('S3')
     *B605582,1 HBG 11/03/2002 Move this block of code to Update the file befor move the item in mover[Begin]
     *SELECT (lcWorkFile)
     *SET ORDER TO TAG (lcWorkFile)
     *IF lnIndex <> 0
     *  IF SEEK(lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+laSCls21[lnSCls21,2]);
     *      AND EMPTY(cstysbcls3)     
     *    REPLACE cstysbcls3 WITH laSSCls3[lnIndex,2]
     *  ELSE
     *    IF !SEEK(lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+laSCls21[lnSCls21,2]+;
     *             laSSCls3[lnIndex,2])
     *      INSERT INTO (lcWorkFile) (dept,cstygroup,cstycatg,cstyclass,cstysbclas,cstysbcls2,;
     *                           cstysbcls3);
     *               VALUES (lcDept,lcGroup,laCat4[lnCat4,2],laCls3[lnCls3,2],laSCls12[lnSCls12,2],;
     *                       laSCls21[lnSCls21],laSSCls3[lnIndex,2])
     *    ENDIF                     
     *  ENDIF
     *ENDIF
     *B605582,1 [End]
ENDCASE

*C200505,1 TMI [Start] Add one selected item to all upper levels
=lfAdToOthr()
*C200505,1 TMI [End  ] 

*!*************************************************************
*! Name      : lfvTrClick
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Validation of target mover
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvTrClick()
*!*************************************************************
FUNCTION lfvTrClick
PARAMETERS lcItemMov
*C200505,1 TMI [Start] Save the selected items in "laSavSlct" array to use in "lfRmvOther" fn.
PRIVATE laSavSlct
DIMENSION laSavSlct[1,2]
=lfSavarr()
IF EMPTY(laSavSlct)
  RETURN
ENDIF  
*C200505,1 TMI [End  ] 

DO CASE 
  CASE lcItemMov = 'CG'
    SELECT (lcWorkFile)
    SET ORDER TO TAG (lcWorkFile)
    IF SEEK(lcDept+lcGroup+laTCatgry[lsTCatgry,2])
      SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = ;
                      lcDept+lcGroup+laTCatgry[lsTCatgry,2]
        DELETE
      ENDSCAN  
    ENDIF

    =lfvTarget('CG')

    lnFound = ASCAN(laCat1,laTCatgry[1,1])
    lnFound = IIF(lnFound > 0 AND !EMPTY(laTCatgry[1,1]),lnFound,0)
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laCat1,lnFound,1)
      STORE lnFound TO lnCat1,lnCat2,lnCat3,lnCat4
    ELSE  
      STORE 1 TO lnCat1,lnCat2,lnCat3,lnCat4
    ENDIF   
    
    =lfCatMov(laCat1[lnCat1,2])
    =lfClsPop(laCat1[lnCat1,2],'1')
    
  CASE lcItemMov = 'CS'
    SELECT (lcWorkFile)
    SET ORDER TO TAG (lcWorkFile)
    IF SEEK(lcDept+lcGroup+laCat1[lnCat1,2]+laTClass[lsTClass,2])
      SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = ;
                      lcDept+lcGroup+laCat1[lnCat1,2]+laTClass[lsTClass,2]
        REPLACE cstyclass  WITH '',;
                cstysbclas WITH '',;
                cstysbcls2 WITH '',;
                cstysbcls3 WITH ''
      ENDSCAN          
      IF (lsTClass <> 1 AND !EMPTY(laTClass[lsTClass-1,2])) OR ;
         (lsTClass = 1  AND ALEN(laTClass,1) > 1 AND !EMPTY(laTClass[lsTClass+1,2])) 
        =SEEK(lcDept+lcGroup+laCat1[lnCat1,2]+' ')
        SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3  = ;
                      lcDept+lcGroup+laCat1[lnCat1,2]+' '
          DELETE
        ENDSCAN  
      ENDIF

    ENDIF 
    *C200505,1 TMI [Start] remove only one selected items from all upper levels
    =lfRmvOther()
    *C200505,1 TMI [End  ] 
    
    =lfvTarget('CS')   

    lnFound = ASCAN(laCls1,laTClass[1,1])
    lnFound = IIF(lnFound > 0 AND !EMPTY(laTClass[1,1]),lnFound,0)
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laCls1,lnFound,1)
      STORE lnFound TO lnCls1,lnCls2,lnCls3
    ELSE  
      STORE 1 TO lnCls1,lnCls2,lnCls3
    ENDIF
    
    =lfClsMov(laCat2[lnCat2,2],laCls1[lnCls1,2])    
    =lfCls1Pop(laCat2[lnCat2,2],laCls1[lnCls1,2],'1')
         
  CASE lcItemMov = 'S1'   
    SELECT (lcWorkFile)
    SET ORDER TO TAG (lcWorkFile)
    IF SEEK(lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+laTSCls1[lsTSCls1,2])
      SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = ;
                      lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+laTSCls1[lsTSCls1,2]
        REPLACE cstysbclas WITH '',;
                cstysbcls2 WITH '',;
                cstysbcls3 WITH ''
      ENDSCAN           
      IF (lsTSCls1 <> 1 AND !EMPTY(laTSCls1[lsTSCls1-1,2])) OR;
         (lsTSCls1 = 1 AND ALEN(laTSCls1,1) > 1 AND !EMPTY(laTSCls1[lsTSCls1+1,2]))
        =SEEK(lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+' ')
        SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3  = ;
                      lcDept+lcGroup+laCat2[lnCat2,2]+laCls1[lnCls1,2]+' '
          DELETE
        ENDSCAN  
      ENDIF

    ENDIF 
    *C200505,1 TMI [Start] remove only one selected items from all upper levels
    =lfRmvOther()
    *C200505,1 TMI [End  ] 
    
    =lfvTarget('S1')   

    lnFound = ASCAN(laSCls11,laTSCls1[1,1])
    lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls1[1,1]),lnFound,0)
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laSCls11,lnFound,1)
      STORE lnFound TO lnSCls11,lnSCls12
    ELSE  
      STORE 1 TO lnSCls11,lnSCls12
    ENDIF       
    
    =lfCls1Mov(laCat3[lnCat3,2],laCls2[lnCls2,2],laSCls11[lnSCls11,2])    
    =lfCls2Pop(laCat3[lnCat3,2],laCls2[lnCls2,2],laSCls11[lnSCls11,2],'21')
    
  CASE lcItemMov = 'S2' 
    SELECT (lcWorkFile)
    SET ORDER TO TAG (lcWorkFile)
    IF SEEK(lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+laTSCls2[lsTSCls2,2])
      SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = ;
                      lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+laTSCls2[lsTSCls2,2]
        REPLACE cstysbcls2 WITH '',;
                cstysbcls3 WITH ''
      ENDSCAN          
      IF (lsTSCls2 <> 1 AND !EMPTY(laTSCls2[lsTSCls2-1,2])) OR;
         (lsTSCls2 = 1 AND ALEN(laTSCls2,1) > 1 AND !EMPTY(laTSCls2[lsTSCls2+1,2]))
        =SEEK(lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+' ')
        SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3  = ;
                      lcDept+lcGroup+laCat3[lnCat3,2]+laCls2[lnCls2,2]+laSCls11[lnSCls11,2]+' '
          DELETE
        ENDSCAN  
      ENDIF

    ENDIF  
    *C200505,1 TMI [Start] remove only one selected items from all upper levels
    =lfRmvOther()
    *C200505,1 TMI [End  ] 
     
    =lfvTarget('S2')   

    lnFound = ASCAN(laSCls21,laTSCls2[1,1])
    lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls2[1,1]),lnFound,0)
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laSCls21,lnFound,1)
      STORE lnFound TO lnSCls21
    ELSE  
      STORE 1 TO lnSCls21
    ENDIF  
    
    =lfCls2Mov(laCat4[lnCat4,2],laCls3[lnCls3,2],laSCls12[lnSCls12,2],laSCls21[lnSCls21,2])    
    
  CASE lcItemMov = 'S3'   
    SELECT (lcWorkFile)
    SET ORDER TO TAG (lcWorkFile)
    IF SEEK(lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+laSCls21[lnSCls21,2]+;
            laTSCls3[lsTSCls3,2])
      SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = ;
                      lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+;
                      laSCls21[lnSCls21,2]+laTSCls3[lsTSCls3,2]
        REPLACE cstysbcls3 WITH ''
      ENDSCAN  
      IF (lnSCls21 <> 1 AND !EMPTY(laSCls21[lnSCls21-1,2])) OR;
         (lnSCls21 = 1 AND ALEN(laSCls21,1) > 1 AND !EMPTY(laSCls21[lnSCls21+1,2]))
        =SEEK(lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+;
                      laSCls21[lnSCls21,2]+' ')
        SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3  = ;
                      lcDept+lcGroup+laCat4[lnCat4,2]+laCls3[lnCls3,2]+laSCls12[lnSCls12,2]+;
                      laSCls21[lnSCls21,2]+' ' 
          DELETE
        ENDSCAN  
      ENDIF

    ENDIF
    *C200505,1 TMI [Start] remove only one selected items from all upper levels
    =lfRmvOther()
    *C200505,1 TMI [End  ] 
    
    =lfvTarget('S3')
ENDCASE    

*!*************************************************************
*! Name      : lfintCdpop
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Intialize arrays of code popup
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfintCdpop()
*!*************************************************************
FUNCTION lfintCdpop

*****Fill Codes array for scound Folder************
*-Style Category array.
laCodInf1[1,01] = "CSTYCATG"
laCodInf1[1,02] = "laCat1"
laCodInf1[1,03] = "lnCat1"
laCodInf1[1,04] = ""
laCodInf1[1,05] = .F.            
laCodInf1[1,06] = .T.            
laCodInf1[1,07] = "ICCLSSTP"     
laCodInf1[1,08] = "ICCLSSTP"     
laCodInf1[1,09] = "lcDept+lcGroup"
laCodInf1[1,10] = "cStyCatg"  
*-Style Category array.
laCodInf2[1,01] = "CSTYCATG"
laCodInf2[1,02] = "laCat2"
laCodInf2[1,03] = "lnCat2"
laCodInf2[1,04] = ""
laCodInf2[1,05] = .F.            
laCodInf2[1,06] = .T.            
laCodInf2[1,07] = "ICCLSSTP"     
laCodInf2[1,08] = "ICCLSSTP"     
laCodInf2[1,09] = "lcDept+lcGroup"
laCodInf2[1,10] = "cStyCatg"  
*-Style Category array.
laCodInf3[1,01] = "CSTYCATG"
laCodInf3[1,02] = "laCat3"
laCodInf3[1,03] = "lnCat3"
laCodInf3[1,04] = ""
laCodInf3[1,05] = .F.            
laCodInf3[1,06] = .T.            
laCodInf3[1,07] = "ICCLSSTP"     
laCodInf3[1,08] = "ICCLSSTP"     
laCodInf3[1,09] = "lcDept+lcGroup"
laCodInf3[1,10] = "cStyCatg"  
*-Style Category array.
laCodInf4[1,01] = "CSTYCATG"
laCodInf4[1,02] = "laCat4"
laCodInf4[1,03] = "lnCat4"
laCodInf4[1,04] = ""
laCodInf4[1,05] = .F.            
laCodInf4[1,06] = .T.            
laCodInf4[1,07] = "ICCLSSTP"     
laCodInf4[1,08] = "ICCLSSTP"     
laCodInf4[1,09] = "lcDept+lcGroup"
laCodInf4[1,10] = "cStyCatg"  

*-Style Class array.
laCodInf2[2,01] = "CSTYCLASS"
laCodInf2[2,02] = "laCls1"
laCodInf2[2,03] = "lnCls1"
laCodInf2[2,04] = ""
laCodInf2[2,05] = .F.            
laCodInf2[2,06] = .T.            
laCodInf2[2,07] = "ICCLSSTP"     
laCodInf2[2,08] = "ICCLSSTP"     
laCodInf2[2,09] = "lcDept+lcGroup"
laCodInf2[2,10] = "CSTYCLASS" 
*-Style Class array.
laCodInf3[2,01] = "CSTYCLASS"
laCodInf3[2,02] = "laCls2"
laCodInf3[2,03] = "lnCls2"
laCodInf3[2,04] = ""
laCodInf3[2,05] = .F.            
laCodInf3[2,06] = .T.            
laCodInf3[2,07] = "ICCLSSTP"     
laCodInf3[2,08] = "ICCLSSTP"     
laCodInf3[2,09] = "lcDept+lcGroup"
laCodInf3[2,10] = "CSTYCLASS" 
*-Style Class array.
laCodInf4[2,01] = "CSTYCLASS"
laCodInf4[2,02] = "laCls3"
laCodInf4[2,03] = "lnCls3"
laCodInf4[2,04] = ""
laCodInf4[2,05] = .F.            
laCodInf4[2,06] = .T.            
laCodInf4[2,07] = "ICCLSSTP"     
laCodInf4[2,08] = "ICCLSSTP"     
laCodInf4[2,09] = "lcDept+lcGroup"
laCodInf4[2,10] = "CSTYCLASS" 

*-Style Sub Class 1 array.
laCodInf3[3,01] = "CSTYSBCLAS"
laCodInf3[3,02] = "laSCls11"
laCodInf3[3,03] = "lnSCls11"
laCodInf3[3,04] = ""
laCodInf3[3,05] = .F.            
laCodInf3[3,06] = .T.            
laCodInf3[3,07] = "ICCLSSTP"     
laCodInf3[3,08] = "ICCLSSTP"     
laCodInf3[3,09] = "lcDept+lcGroup"
laCodInf3[3,10] = "CSTYSBCLAS" 
*-Style Sub Class 1 array.
laCodInf4[3,01] = "CSTYSBCLAS"
laCodInf4[3,02] = "laSCls12"
laCodInf4[3,03] = "lnSCls12"
laCodInf4[3,04] = ""
laCodInf4[3,05] = .F.            
laCodInf4[3,06] = .T.            
laCodInf4[3,07] = "ICCLSSTP"     
laCodInf4[3,08] = "ICCLSSTP"     
laCodInf4[3,09] = "lcDept+lcGroup"
laCodInf4[3,10] = "CSTYSBCLAS"

*-Style Sub Class 2 array.
laCodInf4[4,01] = "CSTYSBCLS2"
laCodInf4[4,02] = "laSCls21"
laCodInf4[4,03] = "lnSCls21"
laCodInf4[4,04] = ""
laCodInf4[4,05] = .F.            
laCodInf4[4,06] = .T.            
laCodInf4[4,07] = "ICCLSSTP"     
laCodInf4[4,08] = "ICCLSSTP"     
laCodInf4[4,09] = "lcDept+lcGroup"
laCodInf4[4,10] = "CSTYSBCLS2" 

*!*************************************************************
*! Name      : lfCatMov
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Update class mover
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfCatMov()
*!*************************************************************
FUNCTION lfCatMov
PARAMETERS lcCatg

lnFound = ASCAN(laTCatgry,lcCatg) 
lnFound = IIF(lnFound > 0 AND !EMPTY(laTCatgry[1,1]),lnFound,0)
IF lnFound = 0
  FOR lnCounter = 1 TO ALEN(laSClass,1)
    lnPos = IIF(OCCURS("\",laSClass[lnCounter,1])>0,2,1)
    laSClass[lnCounter,1] = SUBSTR(laSClass[lnCounter,1],lnPos,LEN(laSClass[lnCounter,1]))
  ENDFOR
  DECLARE laTClass[1,2]
  laTClass =' '
  STORE 'DISABLE' TO lcSClasSt,lcTClasSt,lcAllClSt,lcMovClSt,lcRemClSt,lcRAlClsSt
ELSE
  FOR lnCounter = 1 TO ALEN(laSClass,1)
    lnPos = IIF(OCCURS("\",laSClass[lnCounter,1])>0,2,1)
    laSClass[lnCounter,1] = SUBSTR(laSClass[lnCounter,1],lnPos,LEN(laSClass[lnCounter,1]))
  ENDFOR
  DECLARE laTClass[1,2]
  laTClass =' '
  SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
  =SEEK(lcDept+lcGroup+lcCatg,lcWorkFile)
  SELECT (lcWorkFile)
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  lcDept+lcGroup+lcCatg
    lcClass  = cstyclass
    lnFound  = ASCAN(laSClass,lcClass) 
    IF lnFound > 0
      *-- Update class mover with the new selection     
      lsSClass = ASUBSCRIPT(laSClass,lnFound,1)
      
      IF lsSClass = 0 OR LEFT(laSClass[lsSClass,1],1) = "\"
        EXIT
      ENDIF

      IF lsSClass <= ALEN('laSClass',1) AND lsSClass <> 0
        IF !EMPTY(laTClass[1,1]) 
          DIMENSION laTClass[ALEN(laTClass,1)+1,2]
        ENDIF
        laTClass[ALEN(laTClass,1),1]= ALLTRIM(laSClass[lsSClass,1])
        laTClass[ALEN(laTClass,1),2]= ALLTRIM(laSClass[lsSClass,2])
        *-- When moving a record we replace "\" 
        *-- before the moved element in order to disable it.
        laSClass[lsSClass,1] = "\"+laSClass[lsSClass,1]
        IF lsSClass < ALEN(laSClass,1)
          DO WHILE LEFT(laSClass[lsSClass,1],1) = "\" 
            lsSClass = lsSClass + 1
            IF lsSClass = ALEN(laSClass,1)
              EXIT
            ENDIF
          ENDDO  
        ELSE
          lsSClass = 1  
        ENDIF
      ENDIF  
  
      IF lsSClass > ALEN(laSClass,1)
        lsSClass = ALEN(laSClass,1)
      ENDIF
      
    ENDIF
  ENDSCAN
  STORE 'ENABLE' TO lcSClasSt,lcTClasSt,lcAllClSt,lcMovClSt,lcRemClSt,lcRAlClsSt
ENDIF


*!*************************************************************
*! Name      : lfClsMov
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Update Sub class 1 mover
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfClsMov()
*!*************************************************************
FUNCTION lfClsMov
PARAMETERS lcCatg,lcClas

lnFound = ASCAN(laTClass,lcClas) 
lnFound = IIF(lnFound > 0 AND !EMPTY(laTClass[1,1]),lnFound,0)
IF lnFound = 0
  FOR lnCounter = 1 TO ALEN(laSSCls1,1)
    lnPos = IIF(OCCURS("\",laSSCls1[lnCounter,1])>0,2,1)
    laSSCls1[lnCounter,1] = SUBSTR(laSSCls1[lnCounter,1],lnPos,LEN(laSSCls1[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls1[1,2]
  laTSCls1 =' '
  STORE 'DISABLE' TO lcSSCl1St,lcTSCl1St,lcAlCs1St,lcMvCs1St,lcRmCs1St,lcRAlCs1St
ELSE
  FOR lnCounter = 1 TO ALEN(laSSCls1,1)
    lnPos = IIF(OCCURS("\",laSSCls1[lnCounter,1])>0,2,1)
    laSSCls1[lnCounter,1] = SUBSTR(laSSCls1[lnCounter,1],lnPos,LEN(laSSCls1[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls1[1,2]
  laTSCls1 =' '
  SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
  =SEEK(lcDept+lcGroup+lcCatg+lcClas,lcWorkFile)
  SELECT (lcWorkFile)
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  lcDept+lcGroup+lcCatg+lcClas
    lcSClas1  = cstysbclas
    lnFound  = ASCAN(laSSCls1,lcSClas1) 
    IF lnFound > 0
      *-- Update Sub class 1 mover with the new selection     
      lsSSCls1 = ASUBSCRIPT(laSSCls1,lnFound,1)
      
      IF lsSSCls1 = 0 OR LEFT(laSSCls1[lsSSCls1,1],1) = "\"
        EXIT
      ENDIF

      IF lsSSCls1 <= ALEN('laSSCls1',1) AND lsSSCls1 <> 0
        IF !EMPTY(laTSCls1[1,1]) 
          DIMENSION laTSCls1[ALEN(laTSCls1,1)+1,2]
        ENDIF
        laTSCls1[ALEN(laTSCls1,1),1]= ALLTRIM(laSSCls1[lsSSCls1,1])
        laTSCls1[ALEN(laTSCls1,1),2]= ALLTRIM(laSSCls1[lsSSCls1,2])
        *-- When moving a record we replace "\" 
        *-- before the moved element in order to disable it.
        laSSCls1[lsSSCls1,1] = "\"+laSSCls1[lsSSCls1,1]
        IF lsSSCls1 < ALEN(laSSCls1,1)
          DO WHILE LEFT(laSSCls1[lsSSCls1,1],1) = "\" 
            lsSSCls1 = lsSSCls1 + 1
            IF lsSSCls1 = ALEN(laSSCls1,1)
              EXIT
            ENDIF
          ENDDO  
        ELSE
          lsSSCls1 = 1  
        ENDIF
      ENDIF  
  
      IF lsSSCls1 > ALEN(laSSCls1,1)
        lsSSCls1 = ALEN(laSSCls1,1)
      ENDIF
      
    ENDIF
  ENDSCAN
  *B605582,1 HBG 11/03/2002 Disable all movers in view mode[Begin]
  IF laScrMode[2]
    STORE 'DISABLE' TO lcSSCl1St,lcTSCl1St,lcAlCs1St,lcMvCs1St,lcRmCs1St,lcRAlCs1St
  ELSE
  *B605582,1 [End]
    STORE 'ENABLE' TO lcSSCl1St,lcTSCl1St,lcAlCs1St,lcMvCs1St,lcRmCs1St,lcRAlCs1St
  *B605582,1 HBG 11/03/2002 End if view mode[Begin]  
  ENDIF
  *B605582,1 [End]

ENDIF


*!*************************************************************
*! Name      : lfCls1Mov
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Update Sub class 2 mover
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfClsMov()
*!*************************************************************
FUNCTION lfCls1Mov
PARAMETERS lcCatg,lcClas,lcSClas1

lnFound = ASCAN(laTSCls1,lcSClas1) 
lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls1[1,1]),lnFound,0)
IF lnFound = 0
  FOR lnCounter = 1 TO ALEN(laSSCls2,1)
    lnPos = IIF(OCCURS("\",laSSCls2[lnCounter,1])>0,2,1)
    laSSCls2[lnCounter,1] = SUBSTR(laSSCls2[lnCounter,1],lnPos,LEN(laSSCls2[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls2[1,2]
  laTSCls2 =' '
  STORE 'DISABLE' TO lcSSCl2St,lcTSCl2St,lcAlCs2St,lcMvCs2St,lcRmCs2St,lcRAlCs2St
ELSE
  FOR lnCounter = 1 TO ALEN(laSSCls2,1)
    lnPos = IIF(OCCURS("\",laSSCls2[lnCounter,1])>0,2,1)
    laSSCls2[lnCounter,1] = SUBSTR(laSSCls2[lnCounter,1],lnPos,LEN(laSSCls2[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls2[1,2]
  laTSCls2 =' '
  SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
  =SEEK(lcDept+lcGroup+lcCatg+lcClas+lcSClas1,lcWorkFile)
  SELECT (lcWorkFile)
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  lcDept+lcGroup+lcCatg+lcClas+lcSClas1
    lcSClas2  = cstysbcls2
    lnFound  = ASCAN(laSSCls2,lcSClas2) 
    IF lnFound > 0
      *-- Update Sub class 2 mover with the new selection     
      lsSSCls2 = ASUBSCRIPT(laSSCls2,lnFound,1)
      
      IF lsSSCls2 = 0 OR LEFT(laSSCls2[lsSSCls2,1],1) = "\"
        EXIT
      ENDIF

      IF lsSSCls2 <= ALEN('laSSCls2',1) AND lsSSCls2 <> 0
        IF !EMPTY(laTSCls2[1,1]) 
          DIMENSION laTSCls2[ALEN(laTSCls2,1)+1,2]
        ENDIF
        laTSCls2[ALEN(laTSCls2,1),1]= ALLTRIM(laSSCls2[lsSSCls2,1])
        laTSCls2[ALEN(laTSCls2,1),2]= ALLTRIM(laSSCls2[lsSSCls2,2])
        *-- When moving a record we replace "\" 
        *-- before the moved element in order to disable it.
        laSSCls2[lsSSCls2,1] = "\"+laSSCls2[lsSSCls2,1]
        IF lsSSCls2 < ALEN(laSSCls2,1)
          DO WHILE LEFT(laSSCls2[lsSSCls2,1],1) = "\" 
            lsSSCls2 = lsSSCls2 + 1
            IF lsSSCls2 = ALEN(laSSCls2,1)
              EXIT
            ENDIF
          ENDDO  
        ELSE
          lsSSCls2 = 1  
        ENDIF
      ENDIF  
  
      IF lsSSCls2 > ALEN(laSSCls2,1)
        lsSSCls2 = ALEN(laSSCls2,1)
      ENDIF
      
    ENDIF
  ENDSCAN
  *B605582,1 HBG 11/03/2002 Disable all movers in view mode[Begin]
  IF laScrMode[2]
    STORE 'DISABLE' TO lcSSCl2St,lcTSCl2St,lcAlCs2St,lcMvCs2St,lcRmCs2St,lcRAlCs2St
  ELSE
  *B605582,1 [End]
    STORE 'ENABLE' TO lcSSCl2St,lcTSCl2St,lcAlCs2St,lcMvCs2St,lcRmCs2St,lcRAlCs2St
  *B605582,1 HBG 11/03/2002 End if view mode[Begin]    
  ENDIF
  *B605582,1 [End]  
ENDIF

*!*************************************************************
*! Name      : lfCls2Mov
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Update Sub class 3 mover
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfCls2Mov()
*!*************************************************************
FUNCTION lfCls2Mov
PARAMETERS lcCatg,lcClas,lcSClas1,lcSClas2

lnFound = ASCAN(laTSCls2,lcSClas2) 
lnFound = IIF(lnFound > 0 AND !EMPTY(laTSCls2[1,1]),lnFound,0)
IF lnFound = 0
  FOR lnCounter = 1 TO ALEN(laSSCls3,1)
    lnPos = IIF(OCCURS("\",laSSCls3[lnCounter,1])>0,2,1)
    laSSCls3[lnCounter,1] = SUBSTR(laSSCls3[lnCounter,1],lnPos,LEN(laSSCls3[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls3[1,2]
  laTSCls3 =' '
  STORE 'DISABLE' TO lcSSCl3St,lcTSCl3St,lcAlCs3St,lcMvCs3St,lcRmCs3St,lcRAlCs3St
ELSE
  FOR lnCounter = 1 TO ALEN(laSSCls3,1)
    *C200505,1 TMI [Start] it is not "laSSCls2" , it seems to be "laSSCls3"
    *lnPos = IIF(OCCURS("\",laSSCls2[lnCounter,1])>0,2,1)
    lnPos = IIF(OCCURS("\",laSSCls3[lnCounter,1])>0,2,1)
    *C200505,1 TMI [End  ] 
    laSSCls3[lnCounter,1] = SUBSTR(laSSCls3[lnCounter,1],lnPos,LEN(laSSCls3[lnCounter,1]))
  ENDFOR
  DECLARE laTSCls3[1,2]
  laTSCls3 =' '
  SET ORDER TO TAG (lcWorkFile) IN (lcWorkFile)
  =SEEK(lcDept+lcGroup+lcCatg+lcClas+lcSClas1+lcSClas2,lcWorkFile)
  SELECT (lcWorkFile)
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  lcDept+lcGroup+lcCatg+lcClas+lcSClas1+lcSClas2
    lcSClas3  = cstysbcls3 
    lnFound   = ASCAN(laSSCls3,lcSClas3) 
    IF lnFound > 0
      *-- Update Sub class 3 mover with the new selection     
      lsSSCls3 = ASUBSCRIPT(laSSCls3,lnFound,1)
      
      IF lsSSCls3 = 0 OR LEFT(laSSCls3[lsSSCls3,1],1) = "\"
        EXIT
      ENDIF

      IF lsSSCls3 <= ALEN('laSSCls3',1) AND lsSSCls3 <> 0
        IF !EMPTY(laTSCls3[1,1]) 
          DIMENSION laTSCls3[ALEN(laTSCls3,1)+1,2]
        ENDIF
        laTSCls3[ALEN(laTSCls3,1),1]= ALLTRIM(laSSCls3[lsSSCls3,1])
        laTSCls3[ALEN(laTSCls3,1),2]= ALLTRIM(laSSCls3[lsSSCls3,2])
        *-- When moving a record we replace "\" 
        *-- before the moved element in order to disable it.
        laSSCls3[lsSSCls3,1] = "\"+laSSCls3[lsSSCls3,1]
        IF lsSSCls3 < ALEN(laSSCls3,1)
          DO WHILE LEFT(laSSCls3[lsSSCls3,1],1) = "\" 
            lsSSCls3 = lsSSCls3 + 1
            IF lsSSCls3 = ALEN(laSSCls3,1)
              EXIT
            ENDIF
          ENDDO  
        ELSE
          lsSSCls3 = 1  
        ENDIF
      ENDIF  
  
      IF lsSSCls3 > ALEN(laSSCls3,1)
        lsSSCls3 = ALEN(laSSCls3,1)
      ENDIF
      
    ENDIF
  ENDSCAN
  *B605582,1 HBG 11/03/2002 Disable all movers in view mode[Begin]
  IF laScrMode[2]
    STORE 'DISABLE' TO lcSSCl3St,lcTSCl3St,lcAlCs3St,lcMvCs3St,lcRmCs3St,lcRAlCs3St
  ELSE
  *B605582,1 [End]
    STORE 'ENABLE' TO lcSSCl3St,lcTSCl3St,lcAlCs3St,lcMvCs3St,lcRmCs3St,lcRAlCs3St
  *B605582,1 HBG 11/03/2002 End if view mode[Begin]  
  ENDIF
  *B605582,1 [End]
ENDIF


*!*************************************************************
*! Name      : lfCatgPop
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Fill Category Popup
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfCatgPop()
*!*************************************************************
*B605582,1 
FUNCTION lfCatgPop
PARAMETER lnCur

IF EMPTY(laTCatgry[1,1])
  lnI = 1
ELSE
  lnI = ALEN(laTCatgry,1)+1
ENDIF

DIMENSION laCat1[lnI,2]
DIMENSION laCat2[lnI,2]
DIMENSION laCat3[lnI,2]
DIMENSION laCat4[lnI,2]

laCat1[1,1] = 'N/A'
laCat1[1,2] = ' '
laCat2[1,1] = 'N/A'
laCat2[1,2] = ' '
laCat3[1,1] = 'N/A'
laCat3[1,2] = ' '
laCat4[1,1] = 'N/A'
laCat4[1,2] = ' '

IF !EMPTY(laTCatgry[1,1])
  FOR lnI = 1 TO ALEN(laTCatgry,1)
    lnJ = lnI + 1
    laCat1[lnJ,1] = laTCatgry[lnI,1]
    laCat1[lnJ,2] = laTCatgry[lnI,2]
    laCat2[lnJ,1] = laTCatgry[lnI,1]
    laCat2[lnJ,2] = laTCatgry[lnI,2]
    laCat3[lnJ,1] = laTCatgry[lnI,1]
    laCat3[lnJ,2] = laTCatgry[lnI,2]
    laCat4[lnJ,1] = laTCatgry[lnI,1]
    laCat4[lnJ,2] = laTCatgry[lnI,2]
  ENDFOR
ENDIF  

IF ALEN(laCat1,1) > 1
  IF lnCat1 > ALEN(laCat1,1)
    STORE 2 TO lnCat1,lnCat2,lnCat3,lnCat4
  ELSE
    IF TYPE('lnCur') = 'N'
      STORE lnCur TO lnCat1,lnCat2,lnCat3,lnCat4  
    ELSE
      IF lnCat1 = 1
        lnFound = ASCAN(laCat1,laTCatgry[1,1])
        IF lnFound > 0
          lnFound = ASUBSCRIPT(laCat1,lnFound,1)
          STORE lnFound TO lnCat1,lnCat2,lnCat3,lnCat4
        ENDIF
      ELSE
        STORE lnCat1 TO lnCat2,lnCat3,lnCat4  
      ENDIF  
    ENDIF  
  ENDIF  
ELSE
  STORE 1 TO lnCat1,lnCat2,lnCat3,lnCat4
ENDIF

=lfGetClas(lcDept,lcGroup)

*!*************************************************************
*! Name      : lfClasPop
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Fill Class Popup
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfClasPop()
*!*************************************************************
*B605582,1 
FUNCTION lfClasPop
PARAMETER lnCur

*B605582,1 HBG 11/03/2002 If Category is N/A so all Class popup shold be N/A[Begin]
*IF EMPTY(laTClass[1,1])
IF EMPTY(laTClass[1,1]) OR lnCat1 = 1
*B605582,1 [End]
  lnI = 1
ELSE
  lnI = ALEN(laTClass,1)+1
ENDIF  

DIMENSION laCls1[lnI,2]
DIMENSION laCls2[lnI,2]
DIMENSION laCls3[lnI,2]

laCls1[1,1] = 'N/A'
laCls1[1,2] = ' '
laCls2[1,1] = 'N/A'
laCls2[1,2] = ' '
laCls3[1,1] = 'N/A'
laCls3[1,2] = ' '

*B605582,1 HBG 11/03/2002 If Category is N/A so all Class popup shold be N/A[Begin]
*IF !EMPTY(laTClass[1,1]) 
IF !EMPTY(laTClass[1,1]) AND lnCat1 <> 1
*B605582,1 [End]
  FOR lnI = 1 TO ALEN(laTClass,1)
    lnJ = lnI + 1
    laCls1[lnJ,1] = laTClass[lnI,1]
    laCls1[lnJ,2] = laTClass[lnI,2]
    laCls2[lnJ,1] = laTClass[lnI,1]
    laCls2[lnJ,2] = laTClass[lnI,2]
    laCls3[lnJ,1] = laTClass[lnI,1]
    laCls3[lnJ,2] = laTClass[lnI,2]
  ENDFOR
ENDIF

IF ALEN(laCls1,1) > 1
  IF lnCls1 > ALEN(laCls1,1)
    STORE 2 TO lnCls1,lnCls2,lnCls3
  ELSE
    IF TYPE('lnCur') = 'N'
      STORE lnCur TO lnCls1,lnCls2,lnCls3
    ELSE
      IF lnCls1 = 1
        lnFound = ASCAN(laCls1,laTClass[1,1])
        IF lnFound > 0
          lnFound = ASUBSCRIPT(laCls1,lnFound,1)
          STORE lnFound TO lnCls1,lnCls2,lnCls3
        ENDIF
      ELSE
        STORE lnCls1 TO lnCls2,lnCls3
      ENDIF  
    ENDIF  
  ENDIF  
ELSE
  STORE 1 TO lnCls1,lnCls2,lnCls3
ENDIF

=lfGetCls1(lcDept,lcGroup)

*!*************************************************************
*! Name      : lfSCls1Pop
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Fill Sub Class 1 Popup
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfSCls1Pop()
*!*************************************************************
*B605582,1 
FUNCTION lfSCls1Pop
PARAMETER lnCur

*B605582,1 HBG 11/03/2002 If Class is N/A so all Sub Class 1 popup shold be N/A[Begin]
*IF EMPTY(laTSCls1[1,1])
IF EMPTY(laTSCls1[1,1]) OR lnCls1 = 1
*B605582,1 [End]
  lnI = 1
ELSE
  lnI = ALEN(laTSCls1,1)+1
ENDIF

DIMENSION laSCls11[lnI,2]
DIMENSION laSCls12[lnI,2]

laSCls11[1,1] = 'N/A'
laSCls11[1,2] = ' '
laSCls12[1,1] = 'N/A'
laSCls12[1,2] = ' '

*B605582,1 HBG 11/03/2002 If Class is N/A so all Sub Class 1 popup shold be N/A[Begin]
*IF !EMPTY(laTSCls1[1,1]) 
IF !EMPTY(laTSCls1[1,1]) AND lnCls1 <> 1
*B605582,1 [End]
  FOR lnI = 1 TO ALEN(laTSCls1,1)
    lnJ = lnI + 1
    laSCls11[lnJ,1] = laTSCls1[lnI,1]
    laSCls11[lnJ,2] = laTSCls1[lnI,2]
    laSCls12[lnJ,1] = laTSCls1[lnI,1]
    laSCls12[lnJ,2] = laTSCls1[lnI,2]
  ENDFOR
ENDIF


IF ALEN(laSCls11,1) > 1
  IF lnSCls11 > ALEN(laSCls11,1)
    STORE 2 TO lnSCls11,lnSCls12
  ELSE
    IF TYPE('lnCur') = 'N'
      STORE lnCur TO lnSCls11,lnSCls12
    ELSE
      IF lnSCls11 = 1
        lnFound = ASCAN(laSCls11,laTSCls1[1,1])
        IF lnFound > 0
          lnFound = ASUBSCRIPT(laSCls11,lnFound,1)
          STORE lnFound TO lnSCls11,lnSCls12
        ENDIF
      ELSE
        STORE lnSCls11 TO lnSCls12
      ENDIF  
    ENDIF  
  ENDIF  
ELSE
  STORE 1 TO lnSCls11,lnSCls12
ENDIF

=lfGetCls2(lcDept,lcGroup)
*!*************************************************************
*! Name      : lfSCls2Pop
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Fill Sub Class 2 Popup
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfSCls2Pop()
*!*************************************************************
*B605582,1 
FUNCTION lfSCls2Pop
PARAMETER lnCur

*B605582,1 HBG 11/03/2002 If Sub Class 1 is N/A so Sub Class 2 popup shold be N/A[Begin]
*IF EMPTY(laTSCls2[1,1]) 
IF EMPTY(laTSCls2[1,1]) OR lnSCls11 = 1
*B605582,1 [End]
  lnI = 1
ELSE
  lnI = ALEN(laTSCls2,1)+1
ENDIF

DIMENSION laSCls21[lnI,2]

laSCls21[1,1] = 'N/A'
laSCls21[1,2] = ' '

*B605582,1 HBG 11/03/2002 If Sub Class 1 is N/A so Sub Class 2 popup shold be N/A[Begin]
*IF !EMPTY(laTSCls2[1,1])
IF !EMPTY(laTSCls2[1,1]) AND lnSCls11 <> 1
*B605582,1 [End]
  FOR lnI = 1 TO ALEN(laTSCls2,1)
    lnJ = lnI + 1
    laSCls21[lnJ,1] = laTSCls2[lnI,1]
    laSCls21[lnJ,2] = laTSCls2[lnI,2]
  ENDFOR
ENDIF

IF ALEN(laSCls21,1) > 1
  IF lnSCls21 > ALEN(laSCls21,1)
    STORE 2 TO lnSCls21
  ELSE
    IF TYPE('lnCur') = 'N'
      STORE lnCur TO lnSCls21
    ELSE
      IF lnSCls21 = 1
        lnFound = ASCAN(laSCls21,laTSCls2[1,1])
        IF lnFound > 0
          lnFound = ASUBSCRIPT(laSCls21,lnFound,1)
          STORE lnFound TO lnSCls21
        ENDIF
      ENDIF  
    ENDIF  
  ENDIF  
ELSE
  STORE 1 TO lnSCls21
ENDIF

=lfGetCls3(lcDept,lcGroup)


*:**************************************************************************
*:* Name        : lfAdToOthr                                     *C200505,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/24/2003
*:* Purpose     : Add the selected class to other categories
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAdToOthr()
*:***************************************************************************
FUNCTION lfAdToOthr
PARAMETERS llAll
PRIVATE laFld,lcFields,lcWhere,laClsfctn,lcSeek,lnSlct,lnCurr,lnTyp,lnPos
*C200505,4 TMI [Start] Apply adding to all upper levels in add mode if the user selects not 
*C200505,4             to copy from another category and also in edit mode
*IF llMltSlct 
  *C200505,4 TMI [End  ] 
  lnSlct = SELECT()
  SELECT &lcWorkFile
  lnCurr = RECNO(lcWorkFile)
  
  *C200505,4 TMI [Start] change array laFld dim
  *DIMENSION laFld[5,3],laClsfctn[1,4]
  DIMENSION laFld[5,4],laClsfctn[1,4]
  *C200505,4 TMI [End  ] 
  lcS = IIF(llAll,'es','')  && Add the ploral 'es' in case to add all    &&*C200505,4 TMI
  laFld = ''
  laFld[2,1] = 'CSTYCLASS'
  laFld[2,2] = 'CS'
  laFld[2,3] = 'laSClass'
  laFld[2,4] = ' Class&lcS | Categories'       &&*C200505,4 TMI 
  laFld[3,1] = 'CSTYSBCLAS'
  laFld[3,2] = 'S1'
  laFld[3,3] = 'laSSCls1'
  laFld[3,4] = ' Subclass&lcS | Classes'       &&*C200505,4 TMI 
  laFld[4,1] = 'CSTYSBCLS2'
  laFld[4,2] = 'S2'
  laFld[4,3] = 'laSSCls2'
  laFld[4,4] = ' Subclass&lcS2 | Subclasses'   &&*C200505,4 TMI 
  laFld[5,1] = 'CSTYSBCLS3'
  laFld[5,2] = 'S3'
  laFld[5,3] = 'laSSCls3'
  laFld[5,4] = ' Subclass&lcS3 | Subclasses2'  &&*C200505,4 TMI 
  lnPos = ASCAN(laFld,lcItemMov)
  IF lnPos>0
    lnTyp = ASUBSCRIPT(laFld,lnPos,1)  
  ELSE
    SELECT (lnSlct)
    RETURN
  ENDIF
  
  *C200505,4 TMI [Start] in edit mode add message if the user want to copy the selected item to all upper levels ?
  IF !llMltSlct
    lnPipe = AT('|',laFld[lnTyp,4])    
    lcItem = ''
    IF !llAll
      lcItem = ALLTRIM(&laFld[lnTyp,3].[lnIndex,1])
      lcItem = "'" + IIF(LEFT(lcItem,1)='\',SUBSTR(lcItem,2),lcItem) + "'"
    ENDIF
    lcMsg = "Do you want to copy " + IIF(llAll,'All','the') + ;
            SUBSTR(laFld[lnTyp,4],1,lnPipe-1) + lcItem + ;
            " to all upper " + SUBSTR(laFld[lnTyp,4],lnPipe+1) + "?"
    IF gfModalGen('INM00000B32000',.F.,.F.,.F.,lcMsg) # 1  
      SELECT (lnSlct)
      RETURN
    ENDIF    
  ENDIF
  *C200505,4 TMI [End  ] 

  *B608478,1 WAM 03/10/2008 Get required fields only
  *lcFields = IIF(lnTyp>=2,' CSTYCATG'  ,' "" ')+;
             IIF(lnTyp>=3,',CSTYCLASS' ,',"" ')+;
             IIF(lnTyp>=4,',CSTYSBCLAS',',"" ')+;
             IIF(lnTyp>=5,',CSTYSBCLS2',',"" ')
  lcFields = IIF(lnTyp>=2,' CSTYCATG'  ,'')+;
             IIF(lnTyp>=3,',CSTYCLASS' ,'')+;
             IIF(lnTyp>=4,',CSTYSBCLAS','')+;
             IIF(lnTyp>=5,',CSTYSBCLS2','')
  *B608478,1 WAM 03/10/2008 (End)
  
  lcWhere = IIF(lnTyp>=3,'!EMPTY(CSTYCLASS)' ,'.T.') +'.AND.'+;
            IIF(lnTyp>=4,'!EMPTY(CSTYSBCLAS)','.T.') +'.AND.'+;
            IIF(lnTyp>=5,'!EMPTY(CSTYSBCLS2)','.T.')

  laClsfctn = ' '
  SELECT DISTINCT &lcFields FROM &lcWorkFile WHERE &lcWhere INTO ARRAY laClsfctn         
  FOR lnIndex = IIF(llAll,1,lnIndex) TO IIF(llAll,ALEN(&laFld[lnTyp,3],1),lnIndex)
    IF lnIndex <> 0

      *B608478,1 WAM 03/10/2008 Store array value
      lcFldValue = &laFld[lnTyp,3].[lnIndex,2]
      *B608478,1 WAM 03/10/2008 (End)
      
      FOR lnCnt = 1 TO ALEN(laClsfctn,1)
        lcSeek = lcDept+lcGroup+laClsfctn[lnCnt,1]+;
                 IIF(lnTyp>2,laClsfctn[lnCnt,2],'')+;
                 IIF(lnTyp>3,laClsfctn[lnCnt,3],'')+;
                 IIF(lnTyp>4,laClsfctn[lnCnt,4],'')

        *B608478,1 WAM 03/10/2008 Following lines are commented out
        *IF !SEEK(lcSeek+&laFld[lnTyp,3].[lnIndex,2],lcWorkFile)
        *  IF SEEK(lcSeek) AND EMPTY(&laFld[lnTyp,1])     
        *    REPLACE &laFld[lnTyp,1] WITH &laFld[lnTyp,3].[lnIndex,2]
        *  ELSE
        *   IF !SEEK(lcSeek+&laFld[lnTyp,3].[lnIndex,2])                        
        *      INSERT INTO (lcWorkFile) ;
        *                  (dept,cstygroup,cstycatg,cstyclass,cstysbclas,cstysbcls2);
        *             VALUES (lcDept,lcGroup,laClsfctn[lnCnt,1],laClsfctn[lnCnt,2],laClsfctn[lnCnt,3],laClsfctn[lnCnt,4])
        *      REPLACE &laFld[lnTyp,1] WITH &laFld[lnTyp,3].[lnIndex,2]
        *    ENDIF
        *  ENDIF
        *ENDIF                     

        *B608478,1 WAM 03/10/2008 Add required values only
        IF !SEEK(lcSeek+lcFldValue,lcWorkFile)
          IF SEEK(lcSeek) AND EMPTY(&laFld[lnTyp,1])     
            REPLACE &laFld[lnTyp,1] WITH lcFldValue
          ELSE
            INSERT INTO (lcWorkFile) ;
                        (dept,cstygroup,cstycatg,cstyclass,cstysbclas,cstysbcls2);
                   VALUES (lcDept,lcGroup,laClsfctn[lnCnt,1],IIF(lnTyp>2,laClsfctn[lnCnt,2],''),IIF(lnTyp>3,laClsfctn[lnCnt,3],''),IIF(lnTyp>4,laClsfctn[lnCnt,4],''))
            REPLACE &laFld[lnTyp,1] WITH lcFldValue
          ENDIF
        ENDIF                     
        *B608478,1 WAM 03/10/2008 (End)
      ENDFOR   
    ENDIF
  ENDFOR
  IF BETWEEN(lnCurr,1,RECCOUNT(lcWorkFile))    
    GOTO (lnCurr)
  ENDIF
  
  SELECT (lnSlct)
*C200505,4 TMI [Start] comment this line
*ENDIF
*C200505,4 TMI [End  ] 
*-- end of lfAdToOthr.



*:**************************************************************************
*:* Name        : lfRmvOther                                      *C200505,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/24/2003
*:* Purpose     : Remove the selected Class ( or subclass,subclass2,subclass3) 
*:*               from other Categories( or Classes , subclasses , subclasses2
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfRmvOther()
*:***************************************************************************
FUNCTION lfRmvOther
PARAMETERS llAll
PRIVATE laTyp,laClsfctn,lcSeek,lcDel,lnSlct,lnCurr,lnPos,lnTyp,lcMsg,lcID,lnI,lnSavdim,llChanged
*C200505,4 TMI [Start] ask to remove the selected item from upper levels in Add mode ,edit mode
*IF llMltSlct
  *C200505,4 TMI [End  ] 
  lnSlct = SELECT()
  SELECT &lcWorkFile
  lnCurr = RECNO(lcWorkFile)
  
  DIMENSION laTyp[5,4],laTypes[5]
  laTyp = ""
  lcS = IIF(llAll,'es','')  && Add the ploral 'es' in case to remove all
  laTyp[2,1] = "CSTYCLASS"
  laTyp[2,2] = "laTClass"
  laTyp[2,3] = "lsTClass"
  laTyp[2,4] = "Class&lcS.     '|' from all Categories?"
  laTyp[3,1] = "CSTYSBCLAS"
  laTyp[3,2] = "laTSCls1"
  laTyp[3,3] = "lsTSCls1"
  laTyp[3,4] = "Subclass&lcS.  '|' from all Classes?"
  laTyp[4,1] = "CSTYSBCLS2"
  laTyp[4,2] = "laTSCls2"
  laTyp[4,3] = "lsTSCls2"
  laTyp[4,4] = "Subclass&lcS.2 '|' from all Subclasses?"
  laTyp[5,1] = "CSTYSBCLS3"
  laTyp[5,2] = "laTSCls3"
  laTyp[5,3] = "lsTSCls3"
  laTyp[5,4] = "Subclass&lcS.3 '|' from all Subclasses2?"
  
  laTypes[1] = ''
  laTypes[2] = 'CS'
  laTypes[3] = 'S1'
  laTypes[4] = 'S2'
  laTypes[5] = 'S3'
  
  lnTyp = ASCAN(laTypes,lcItemMov)
  IF lnTyp = 0 
    RETURN
  ENDIF

  llChanged = .F.
  IF EMPTY(&laTyp[lnTyp,2].[1])
    llChanged = .T.
    DIMENSION &laTyp[lnTyp,2].[ALEN(laSavSlct,1),2]
    =ACOPY(laSavSlct,&laTyp[lnTyp,2])
  ENDIF  
  IF llAll
    lcMsg = "Do you want to remove " + STRTRAN(laTyp[lnTyp,4],"'|'")
  ELSE   
    lcDesc = ALLT(&laTyp[lnTyp,2].[&laTyp[lnTyp,3],1])
    IF EMPTY(lcDesc)
      RETURN
    ENDIF
    laTyp[lnTyp,4] = STUFF(laTyp[lnTyp,4],AT('|',laTyp[lnTyp,4]),1,lcDesc)
    lcMsg = "Do you want to remove the " + laTyp[lnTyp,4]
  ENDIF

  *C200505,4 TMI [Start] Show the removal from all warning in edit mode only
  *IF gfModalGen('INM00000B32000',.F.,.F.,.F.,lcMsg) = 1
  IF !llMltSlct && in edit mode ask to add an item to all upper levels
    IF gfModalGen('INM00000B32000',.F.,.F.,.F.,lcMsg) # 1
      RETURN
    ENDIF
  ENDIF    
  *C200505,4 TMI [End  ] 
  
    FOR lnI = IIF(llAll,1,&laTyp[lnTyp,3]) TO IIF(llAll,ALEN(&laTyp[lnTyp,2],1),&laTyp[lnTyp,3])
      lcID = &laTyp[lnTyp,2].[lnI,2]
      DIMENSION laClsfctn[1,5]
      laClsfctn = ' '
      SELECT CSTYCATG,CSTYCLASS,CSTYSBCLAS,CSTYSBCLS2,CSTYSBCLS3 ;
         FROM &lcWorkFile ;
         WHERE &laTyp[lnTyp,1] = lcID ;
         INTO ARRAY laClsfctn       
      FOR lnCnt = 1 TO ALEN(laClsfctn,1)
      
        lcSeek = lcDept+lcGroup+laClsfctn[lnCnt,1]+;
                 IIF(lnTyp>=2,laClsfctn[lnCnt,2],'')+;
                 IIF(lnTyp>=3,laClsfctn[lnCnt,3],'')+;
                 IIF(lnTyp>=4,laClsfctn[lnCnt,4],'')+;
                 IIF(lnTyp>=5,laClsfctn[lnCnt,5],'')
      
        IF SEEK(lcSeek,lcWorkFile)
          SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = lcSeek
            DO CASE
              CASE lnTyp = 2
                REPLACE cstyclass  WITH '',;
                        cstysbclas WITH '',;
                        cstysbcls2 WITH '',;
                        cstysbcls3 WITH ''

              CASE lnTyp = 3
                REPLACE cstysbclas WITH '',;
                        cstysbcls2 WITH '',;
                        cstysbcls3 WITH ''

              CASE lnTyp = 4
                REPLACE cstysbcls2 WITH '',;
                        cstysbcls3 WITH ''
  
              CASE lnTyp = 5
                REPLACE cstysbcls3 WITH ''
            ENDCASE    
          ENDSCAN  
      
          lcDel = lcDept+lcGroup+laClsfctn[lnCnt,1]+;
                  IIF(lnTyp>=3,laClsfctn[lnCnt,2],' ')+;
                  IIF(lnTyp>=4,laClsfctn[lnCnt,3],' ')+;
                  IIF(lnTyp>=5,laClsfctn[lnCnt,4],' ')+;
                  IIF(lnTyp>=6,laClsfctn[lnCnt,5],' ')
                  
          =SEEK(lcDel,lcWorkFile)
          SKIP
          SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 = lcDel 
            DELETE
          ENDSCAN  
        ENDIF
      ENDFOR
    ENDFOR
  *C200505,4 TMI [Start] comment this line
  *ENDIF
  *C200505,4 TMI [End  ] 
  *-- Restore the saved &laTyp[lnTyp,2] array dime
  IF llChanged
    DIMENSION &laTyp[lnTyp,2].[1,2]
    &laTyp[lnTyp,2] = ' '
  ENDIF
  
  IF BETWEEN(lnCurr,1,RECCOUNT(lcWorkFile))
    GOTO (lnCurr) IN &lcWorkFile
  ENDIF
  
  SELECT (lnSlct)
*C200505,4 TMI [Start] ask to remove the selected item from upper levels in Add mode ,edit mode
*ENDIF
*C200505,4 TMI [End  ] 
*-- end of lfRmvOther.

*:**************************************************************************
*:* Name        : lfSavarr                                       *C200505,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/24/2003
*:* Purpose     : Save the target array to reuse it in "lfRmvOther" fn.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSavarr()
*:***************************************************************************
FUNCTION lfSavarr
*C200505,4 TMI [Start]  apply this in all cases
*IF llMltSlct
*C200505,4 TMI [End  ] 
  DO CASE
     CASE lcItemMov = 'CG'
       *-- nothing to be done        
     CASE lcItemMov = 'CS'
       DIMENSION laSavSlct[ALEN(laTClass,1),2]
       =ACOPY(laTClass,laSavSlct)
     CASE lcItemMov = 'S1'
       DIMENSION laSavSlct[ALEN(laTSCls1,1),2]
       =ACOPY(laTSCls1,laSavSlct)
     CASE lcItemMov = 'S2'
       DIMENSION laSavSlct[ALEN(laTSCls2,1),2]
       =ACOPY(laTSCls2,laSavSlct)
     CASE lcItemMov = 'S3'
       DIMENSION laSavSlct[ALEN(laTSCls3,1),2]
       =ACOPY(laTSCls3,laSavSlct)
  ENDCASE
*C200505,4 TMI [Start]  comment this line
*ENDIF    
*C200505,4 TMI [End  ] 
*-- end of lfSavarr.
