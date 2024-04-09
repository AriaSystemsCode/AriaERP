*:************************************************************************
*:
*: Procedure file: ALRELPI.PRG
*:
*:         System: ARIA 2.7
*:         Module: Allocation
*:         Author: Haytham El_Sheltawi
*:      Copyright (c) 
*:  Last modified: 05/13/97
*:
*:  Procs & Fncts: lpShow
*:               : lfRelScr
*:               : lfDelScr
*:               : lfSwchSty
*:               : lfMoveOrd
*:               : lfvScope
*:               : lfvSelect
*:               : lfvSelAll
*:               : lfvSelNon
*:               : lfvInvert
*:               : lfDispBrow
*:               : lfCrTmKtTk
*:               : lfwBrows
*:               : lfvpbSel
*:               : lfwIbBrow
*:               : lfBrowTrap
*:               : lfBrwUnTrp
*:               : lfTraps
*:               : lfwPikTkt
*:               : lfvPikTkt
*:               : lfvDate
*:               : lfwStyle
*:               : lfvStyle
*:               : lfFindStyl
*:               : lfOpenFile
*:               : lfCreatExp
*:               : lfSekFirst
*:               : lfSekPak
*:               : lfFndUnCSe
*:               : lfActPad
*:               : lfvActBar
*:
*:  Documented    05/13/1997
*:************************************************************************
*E300666,1 This program was coded by HS for the Enhancement
*:************************************************************************
*: Modification :
*E301077,25 MAB 01/07/1999 Open and Close files.
*B801992,1  MAB 02/25/1999 1- Select specific range of piktkts
*B801992,1  MAB 02/25/1999 2- Validate Pick ticket
*B801992,1  MAB 02/25/1999 3- Adjust spelling of (tikit to ticket)
*E602610,1  MAB 02/27/1999 if no selected records disable all buttons.
*B801961,1  HS  03/23/1999 Declare some new arrays that will be used in the
*B801961,1                 filter expression that will be returned by the
*B801961,1                 option grid in the case of "In list" operator.
*B602809,1 HDM  06/17/1999 Fixing bug of not releasing the allocation in 
*                          ordline file
*MAB            07/26/1999 Fix a bug.
*B802758,1 SSE  11/30/1999 1) Resizing the OGBETWEN in aas_win project
*B802758,1                 2) after selecting a Style which we had P/T
*B802758,1                    it doesn't display any Pick Ticket on screen
*B603318,1 TAK 12/05/1999 Added a custom process to release the adornments.
*B603347,1 ARD 02/28/2000 Elemnate complete piktkt to apper in the browse 
*B607246,1 ABD 06/08/2003 Add triger to release piktk form custom table for custom mem10.
*C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*E126905,1 EIH 04/11/2005 Update in new field 'CALLOCATB' by ' ' in case of Release                        
*B039660,1 NNA 09/01/2005 Transfer all the bin location's Programs To Binmain.prg instead of Davmain.prg
*E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt.
*:************************************************************************

EXTERNAL ARRAY laScrMode

** lcRelPkCh1   Variable to hold child window name no 1
** lcRelPkCh2   Variable to hold child window name no 2
** lcExpr       Variable to hold the option grid expression
** lcOldPikTk   Variable to hold the old pick ticket
** lcOldStyle   Variable to hold the old style
** lcSession    Variable to hold the program session number
** lcCurrObj    Variable to hold the current object in the rall back
** lcTmpKtTk    Variable to hold temp name for the temp file
** lcPrtCond    Variable to hold the pick ticket print status condition
** lcStylePct   Varible to hold the Style field mask

** lnSelRec     Variable to hold the number of selected records
** lnDelRec     Variable to hold the number of released records
** lnUnCmRec    Variable to hold the record number of the uncomplete session 
** lnRpPrtFlg   Variable to hold the user choice for the pick ticket print status
** lnUnSelRec   Variable to hold the number of unselected records
 
** llCallScop   Flag to hold the first time of the session
** llDyelot     Flag to hold the system dyelot status
** llMultiWH    Flag to hold the system multi warehouse status
** llSekPak     Flag to hold if any of the selected pick ticket has a packing list
** llContinue   Flag to hold if there is uncompleted session
** llCallUnCS   Flag to know if we are going to call lfFndUnCSe from lpShow

** laFileStru   Array to hold the temp file structure
** laScopExpr   Array to hold the scope values and expressions
** laUsedFile   Array to hold the files to be open and there tags

STORE '' TO lcRelPkCh1 , lcRelPkCh2 , lcExpr ,;
            lcOldPikTk , lcOldStyle , lcSession , lcCurrObj , lcTmpKtTk ,;
            lcPrtCond , lcStylePct

STORE 0 TO lnSelRec , lnDelRec , lnUnCmRec , lnUnSelRec

STORE 0 TO lnSelRec , lnDelRec , lnUnCmRec , lnUnSelRec

STORE 1 TO lnRpPrtFlg , lnOPrtFlag

STORE .F. TO llCallScop , llDyelot , llMultiWH , llSekPak ,;
             llContinue , llCallUnCS

lcPickBrow = 'Picking Tickets'    && Varible to hold the browse title
lnBrRecNo = 1                     && Varible to hold the browse record number

*E301077,25 Now laUsedFile array became 7 items because we open picket file [Begin
*E301077,25 with module object.
*DECLARE laFileStru[1] , laScopExpr[3,3] , laUsedFile[8,3] ,;
*        laVarTSav[10,1] , laSavScop[1,7]
DECLARE laFileStru[1] , laScopExpr[3,3] , laUsedFile[7,2] ,;
        laVarTSav[10,1] , laSavScop[1,7]
*E301077,25 Now laUsedFile array became 7 items because we open picket file [End..

*B801961,1 Add this line to declare some new arrays that will be used in the
*          filter expression that will be returned by the option grid in
*          the case of "In list" operator [Begin]
DIMENSION laHdFlt[1] , laFxFlt[1] , laVrFlt[1]
*B801961,1 Add this line to declare some new arrays that will be used [End]

laVarTSav[1,1] = 'laScopExpr[1,1]'
laVarTSav[2,1] = 'laScopExpr[1,2]'
laVarTSav[3,1] = 'laScopExpr[1,3]'
laVarTSav[4,1] = 'laScopExpr[2,1]'
laVarTSav[5,1] = 'laScopExpr[2,2]'
laVarTSav[6,1] = 'laScopExpr[2,3]'
laVarTSav[7,1] = 'laScopExpr[3,1]'
laVarTSav[8,1] = 'laScopExpr[3,2]'
laVarTSav[9,1] = 'laScopExpr[3,3]'
laVarTSav[10,1] = 'lnRpPrtFlg'

laScopExpr = ''
laScopExpr[2,1] = {}
laScopExpr[2,2] = {}
laUsedFile = ''

*E301077,25 Now laUsedFile array became 7 items because we open picket file [Begin
*E301077,25 with module object.
*laUsedFile[1,1] = 'PIKTKT'
*laUsedFile[1,2] = 'PIKTKT'
*laUsedFile[2,1] = 'STYLE'
*laUsedFile[2,2] = 'STYLE'
*laUsedFile[3,1] = 'ORDLINE
*laUsedFile[3,2] = 'ORDLINST'
*laUsedFile[4,1] = 'SCALE'
*laUsedFile[4,2] = 'SCALE'
*laUsedFile[5,1] = 'ORDHDR'
*laUsedFile[5,2] = 'ORDHDR'
*laUsedFile[6,1] = 'PACK_HDR'
*laUsedFile[6,2] = 'PACK_HDR'
*laUsedFile[7,1] = 'PIKLINE'
*laUsedFile[7,2] = ''
*laUsedFile[8,1] = 'STYDYE'
*laUsedFile[8,2] = 'STYDYE'

laUsedFile[1,1] = 'STYLE'
laUsedFile[1,2] = 'STYLE'

laUsedFile[2,1] = 'ORDLINE'
*laUsedFile[2,2] = 'ORDLINST'
laUsedFile[2,2] = 'ORDLINES'

laUsedFile[3,1] = 'SCALE'
laUsedFile[3,2] = 'SCALE'
laUsedFile[4,1] = 'ORDHDR'
laUsedFile[4,2] = 'ORDHDR'
laUsedFile[5,1] = 'PACK_HDR'
laUsedFile[5,2] = 'PACK_HDR'
laUsedFile[6,1] = 'PIKLINE'
laUsedFile[6,2] = ''
laUsedFile[7,1] = 'STYDYE'
laUsedFile[7,2] = 'STYDYE'
*E301077,25 Now laUsedFile array became 7 items because we open picket file [End..

lnSessNo = gnProgCopy

IF !gfSetup()
  RETURN
ENDIF


*E130074,1 EIH 01/02/2006 Check if the field name 'CSNDPKT' exist in the PIKTKT file. [Begin]
lnAlias = SELECT(0) 
STORE .F. TO llSndPkFld
SELECT PIKTKT
FOR I = 1 TO FCOUNT('PIKTKT')
  IF UPPER(FIELD(I))='CSNDPKT'
    llSndPkFld = .T.
	EXIT
  ENDIF 
ENDFOR
SELECT (lnAlias)
*E130074,1 EIH 01/02/2006 [End]

llNoShow = .F.            && Flag to make the screen call the PROCEDURE lpShow evry time it run
lcScFields = 'PIKTKT'     && Varible to hold the screen fields
llFrstTime = .T.          && Flag to know if we are going to call lpShow for the first time

DIMENSION laPanelObj(2,3),laSetups[2,2]    && Array to add new buttons to the control panel
laPanelObj[1,1] = 'pbScop'
laPanelObj[1,2] = gcBmpHome + 'SCOPE.BMP'
laPanelObj[1,3] = 'VALID lfvScope()'
laPanelObj[2,1] = 'pbRel'
laPanelObj[2,2] = gcBmpHome + 'RELEASE2.BMP'
laPanelObj[2,3] = 'VALID lfRelScr()'

*E301077,25 Call gfGetMemVar Once per this screen [Begin]
laSetups[1,1] = 'M_WAREHOUS'
laSetups[2,1] = 'M_DYELOT'
=gfGetMemVar(@laSetups,gcAct_Comp)

*llMultiWH = IIF(gfGetMemVar('M_WareHouse') = 'Y' , .T. , .F.)
*llDyelot  = IIF(gfGetMemVar('M_DYELOT') = 'Y' , .T. , .F.)
llMultiWH  = (UPPER(ALLTRIM(laSetups[2,1])) = 'Y')
llDyelot   = (UPPER(ALLTRIM(laSetups[2,2])) = 'Y')
*E301077,25 Call gfGetMemVar Once per this screen [End  ]

*B603318,1 Initialize GL information if custom release was found.
IF ASCAN(laEvntTrig , PADR('UPDRELEASE',10)) <> 0
  STORE '' TO lcGLFYear,lcGLPeriod,lcGlDist,lcGlSession
  llLinkToGl = (gfGetMemVar('M_Link_GL')='Y')
  IF llLinkToGl 
    llLinkToGl = CHECKPRD(gdSysDate,'lcGLFYear','lcGLPeriod','PO')
  ENDIF 
ENDIF
*B603318,1 End.


*E301077,25 Open files after check avoiding open them them colse. [Begin]
*=lfOpenFile(.T.)
IF RECCOUNT('PIKTKT') = 0
  =gfModalGen("TRM44010B00000" , "DIALOG" , "picking tickets")
  *E301077,25 Now we open all required files after this check.
  *=lfOpenFile(.F.)
  RETURN
ENDIF
=lfOpenFile()
*E301077,25 Open files after check avoiding open them them colse. [End  ]



IF !WEXIST(gcBaseWind)

  lcRelPkCh1 = gfTempName()   
  lcRelPkCh2 = gfTempName()   

  lcStylePct   = gfItemMask("PI")
  
  llContinue = lfFndUnCSe()

  lcSession = IIF(llContinue , lcSession , gfSequence('CSESSION'))
  IF !llContinue
    llCallScop = .T.
    lcTmpKtTk = gfTempName()
  
    SELECT PIKTKT
    =AFIELDS(laFileStru)
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+3,4]

    laFileStru[lnFileStru+1,1] = 'LLSEL'
    laFileStru[lnFileStru+1,2] = 'L'
    laFileStru[lnFileStru+1,3] = 1
    laFileStru[lnFileStru+1,4] = 0

    laFileStru[lnFileStru+2,1] = 'NORDLNNO'
    laFileStru[lnFileStru+2,2] = 'N'
    laFileStru[lnFileStru+2,3] = 9
    laFileStru[lnFileStru+2,4] = 0

    laFileStru[lnFileStru+3,1] = 'NPROCNO'
    laFileStru[lnFileStru+3,2] = 'N'
    laFileStru[lnFileStru+3,3] = 2
    laFileStru[lnFileStru+3,4] = 0

    CREATE TABLE &gcWorkDir.&lcTmpKtTk FROM ARRAY laFileStru
    *SELECT(lcTmpKtTk)
    *INDEX ON PIKTKT TAG (lcTmpKtTk)
  ENDIF   && End of IF
  
ENDIF   && End of IF

lcBaseFile = lcTmpKtTk    && Varible that hold the screen main file
SELECT(lcTmpKtTk)

PUSH KEY
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcPickBrow)

=lfActPad()
DO (gcScrDir + gcWinAppl + '\ALRELPI.SPR')

RELEASE PAD _Option OF _MSYSMENU
RELEASE WINDOW (lcPickBrow)

POP KEY

IF glQuitting
  
  *IF Statment to check if this session has a record in the UNCMSESS file
  IF lnUnCmRec <> 0
    SELECT UNCMSESS
    GO lnUnCmRec
    
    *REPLACE Statment to make the status completed
    REPLACE Status WITH 'C'
    UNLOCK
  ENDIF   && End of IF

  *E301077,25 Transfer the following command after glQuitting block to close files every 
  *E301077,25 time user change to another screen.
  *=lfOpenFile(.F.)

  *IF Statment to check if the temp file is open
  IF USED(lcTmpKtTk)  && We are going to close the Temp Pick ticket & Erase it.
    SELECT(lcTmpKtTk)
    USE IN &lcTmpKtTk
  ENDIF   && End of IF
  ERASE &gcWorkdir.&lcTmpKtTk..DBF  && Erase the Temp file.
  ERASE &gcWorkdir.&lcTmpKtTk..CDX  && Erase the Temp index.
  
ENDIF   && End of IF
*lfOpenFile(.F.) && Transfered from glQuitting block.
*-- end of program code.


*!*************************************************************
*! Name      : lpShow
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Local Show Procedure
*!*************************************************************
*! Called from : Control panel global Show
*!*************************************************************
*! Calls       : lfvScope()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
PROCEDURE lpShow

SHOW GET pbEdt DISABLE
SHOW GET pbBrws DISABLE
SHOW GET pbcpPrint DISABLE
SHOW GET pbSlct DISABLE
SHOW GET pbDlt DISABLE

laCtrStat[7]  = "DISABLE"                && Edit button 
laCtrStat[8]  = "DISABLE"                && Delete button
laCtrStat[9]  = "DISABLE"                && Select button
laCtrStat[10] = "DISABLE"                && Browse button

SHOW MENU _mSysMenu

*IF The user has released all piking tikets in scope
IF llCallUnCS
  
  llCallUnCS = .F.
  llFrstTime = .T.
  lcOldTmpF  = lcTmpKtTk
  llContinue = lfFndUnCSe()
  
  *If there is another uncomplete session
  IF llContinue
    =lfDispBrow()
    
    *IF Statment to check if the old temp file is open
    IF USED(lcOldTmpF)  && We are going to close the Temp Order line & Erase it.
      USE IN &lcOldTmpF
    ENDIF   && End of IF
    
    *IF The old temp file .DBF exist we are going to erase it
    IF FILE(gcWorkdir + lcOldTmpF + '.DBF')
      ERASE &gcWorkdir.&lcOldTmpF..DBF          && Erase the Temp file.
    ENDIF    && End of IF FILE(gcWorkdir + lcOldTmpF + '.DBF')
    
    *IF The old temp file .CDX exist we are going to erase it
    IF FILE(gcWorkdir + lcOldTmpF + '.CDX')
      ERASE &gcWorkdir.&lcOldTmpF..CDX          && Erase the Temp index.
    ENDIF    && End of IF FILE(gcWorkdir + lcOldTmpF + '.CDX')
    
    *IF The old temp file .FPT exist we are going to erase it
    IF FILE(gcWorkdir + lcOldTmpF + '.FPT')
      ERASE &gcWorkdir.&lcOldTmpF..FPT  && Erase the Temp index.
    ENDIF    && End of IF FILE(gcWorkdir + lcOldTmpF + '.FPT')
    
  ENDIF    && End of IF llContinue
ENDIF    && End of IF llCallUnCS

*IF Statment to Check if uncopleted session and first time for lpShow
* OR not the first time for this session of the program and first time
*for lpShow
IF (llContinue .AND. llFrstTime) .OR. (!llCallScop .AND. llFrstTime)

  *E602610,1 add variable to detect that all record was deleted then disable 
  *E602610,1 all buttons. [Begin]
  IF !EOF(lcTmpKtTk)
    *IF There is no selected pick tickets
    IF lnSelRec = 0
      SHOW GET pbSelNon DISABLE
    ELSE   && Else
      SHOW GET pbSelNon ENABLE
    ENDIF  && End of IF
  
    *IF All pick tickets is selected
    IF lnUnSelRec = 0
      SHOW GET pbSelAll DISABLE
    ELSE   && Else
      SHOW GET pbSelAll ENABLE
    ENDIF  && End of IF

    *IF The current record is selected
    IF llSel
      SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
    ELSE   && Else
      SHOW GET pbSelect,1 PROMPT 'Se\<lect'
    ENDIF  && End of IF

  ENDIF
  *E602610,1 add variable to detect that all record was deleted then disable 
  *E602610,1 all buttons. [End  ]
  
  *IF Statment to check for uncompleted session that was 
  *termenated while releasing
  IF !EMPTY(lcCurrObj)
    =&lcCurrObj
  ENDIF  && End of IF

ELSE    && Else
  
  *IF Statment to check if we are going to call the option grid for the user
  *[IF these is the first time for these session of the program]
  IF llCallScop
    llCallScop = .F.
    SHOW GET pbRel DISABLE
    =lfvScope()
  ENDIF  && End of IF
ENDIF  && End of IF

llFrstTime = .F.

*IF There is no selected pick tickets
IF lnSelRec = 0
  SHOW GET pbRel DISABLE
ELSE   && Else
  SHOW GET pbRel ENABLE
ENDIF  && End of IF

lnBrRecNo  = RECNO(lcTmpKtTk)
SHOW WINDOW (lcPickBrow) REFRESH

*!*************************************************************
*! Name      : lfRelScr
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Valid function for Release push button
*!*************************************************************
*! Called from : Control panel [Release push button]
*!*************************************************************
*! Calls       : gfModalGen() , lfDelScr()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfRelScr

*IF Statment to make sure that the user want to release the selected records
*** Message : "Are you sure you want to release the selected picking ticket(s) "
***           "                   < Yes >           < No >                    "
IF gfModalGen("TRM44005B00006","DIALOG") = 1
  =lfDelScr()
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfDelScr
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function to release the selected picking tickets
*!*************************************************************
*! Called from : lfRelScr() , lpShow()
*!*************************************************************
*! Calls       : gfModalGen()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfDelScr

*--HDM B602809,1 [start] Ddefine a new variable to hold the ordline index
PRIVATE lcOrdLnTag
*--HDM B602809,1 [End]


=lfAdUnCmSR(.F. , 'pbRel')

SELECT ORDLINE
SET RELATION TO STYLE INTO STYLE

SELECT(lcTmpKtTk)
SET RELATION TO PikTkt INTO PIKTKT

lcWhileCon = IIF(EMPTY(laScopExpr[3,2]) , "" , " .AND. STYLE <= laScopExpr[3,2]")
lcPrtCond = IIF(lnRpPrtFlg = 1 , '' , IIF(lnRpPrtFlg = 2 , ' .AND. PIKTKT.PrtFlag = "P"' , ' .AND. PIKTKT.PrtFlag <> "P"'))


*B603318,1 Start.
IF ASCAN(laEvntTrig , PADR('UPDRELEASE',10)) <> 0
  llPrepere = .T.
  =gfDoTriger('ALRELPI',PADR('UPDRELEASE',10))
ENDIF
*B603318,1 End.

*B607246,1 Add triger to release piktk form custom table for custom mem10. [Begin]
IF ASCAN(laEvntTrig , PADR('RELEASPT',10)) <> 0
  =gfDoTriger('ALRELPI',PADR('RELEASPT',10))
ENDIF
*B607246,1 [End]

SELECT(lcTmpKtTk)
GO TOP
llSekPak = .F.   && Flag to hold if any of the selected pick ticket has a packing list

*SCAN Loop to scan the temp file For selected records and not canceled
*Pick tickets and do not have packing list and print status like the user
*selected

*B603347,1 MAB 04/23/2000 == Apply the same filter to collect condition [Begin]
*SCAN FOR llSel .AND. PIKTKT.Status <> "X" .AND. lfSekPak() &lcPrtCond
SCAN FOR llSel .AND. !(PIKTKT.Status$"CX") .AND. lfSekPak() &lcPrtCond
*B603347,1 MAB 04/23/2000 == Apply the same filter to collect condition [End  ]


  *E130074,1 EIH 01/02/2006 Fix bug that when warehouse shipping order send for piktkt then we can not release the piktkt [Begin].
  IF 'AS' $ gcCmpModules AND llSndPkFld
    IF PIKTKT.csndpkt = 'Y'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Warehouse shipping order send for piktkt number '+PIKTKT.piktkt+' can not release ')
      LOOP
    ENDIF
  ENDIF
  *E130074,1 EIH 01/02/2006 [End]

  SELECT ORDLINE
  *IF Statment to find the first record in ORDLINE
  IF lfSekFirst()

    *SCAN Loop to scan the ORDLINE file For the selected critirea
    SCAN REST ;
        WHILE ORDER + STORE = PIKTKT.Order + PIKTKT.Store &lcWhileCon ;
          FOR PIKTKT = PIKTKT.PikTkt

      SCATTER MEMVAR MEMO

      *IF Statment for the rall back
      IF &lcTmpKtTk..nProcNo = 1
      
        SELECT PIKLINE
        APPEND BLANK
        GATHER MEMVAR MEMO
        =gfAdd_Info()
        REPLACE &lcTmpKtTk..nProcNo WITH 2
        =RLOCK(lcTmpKtTk)
        UNLOCK IN &lcTmpKtTk
      ENDIF    && End of IF
     
      *IF Statment for the rall back

      IF &lcTmpKtTk..nProcNo = 2

        *IF Statment to check if the system is Dyelot and this style
        * has Dye
        IF llDyelot .AND. STYLE.cDye_Flg = 'Y'
          SELECT STYDYE
          
          *IF Statment to find the STYDYE record for the current PIKTKT record
          *and ORDLINE record
          
          IF SEEK( ORDLINE.Style + PIKTKT.cWareCode + ORDLINE.Dyelot )
            =RLOCK()
            REPLACE Alo1 WITH MAX( Alo1 - ORDLINE.Pik1, 0 ),;
                    Alo2 WITH MAX( Alo2 - ORDLINE.Pik2, 0 ),;
                    Alo3 WITH MAX( Alo3 - ORDLINE.Pik3, 0 ),;
                    Alo4 WITH MAX( Alo4 - ORDLINE.Pik4, 0 ),;
                    Alo5 WITH MAX( Alo5 - ORDLINE.Pik5, 0 ),;
                    Alo6 WITH MAX( Alo6 - ORDLINE.Pik6, 0 ),;
                    Alo7 WITH MAX( Alo7 - ORDLINE.Pik7, 0 ),;
                    Alo8 WITH MAX( Alo8 - ORDLINE.Pik8, 0 ),;
                  TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8

            =gfAdd_Info()
            UNLOCK
          ENDIF    && End of IF
        ENDIF    && End of IF

        REPLACE &lcTmpKtTk..nProcNo WITH 3
        =RLOCK(lcTmpKtTk)
        UNLOCK IN &lcTmpKtTk
      ENDIF    && End of IF
      
      *IF Statment for the rall back
      IF &lcTmpKtTk..nProcNo = 3
      
        *IF Statment to check if the system use multi warehouse
        *-- B602809,1 HDM [Start] we have to update styedye also in case single warehouse
        *IF llMultiWH
        *-- B602809,1 HDM [End]
          SELECT STYDYE
          
          *IF Statment to find the STYDYE record for the current PIKTKT record
          *and ORDLINE record
          IF SEEK(ORDLINE.Style + PIKTKT.cWareCode + SPACE(10))
            =RLOCK()
            REPLACE NEXT 1 Alo1 WITH MAX( Alo1 - ORDLINE.Pik1, 0 ),;
                           Alo2 WITH MAX( Alo2 - ORDLINE.Pik2, 0 ),;
                           Alo3 WITH MAX( Alo3 - ORDLINE.Pik3, 0 ),;
                           Alo4 WITH MAX( Alo4 - ORDLINE.Pik4, 0 ),;
                           Alo5 WITH MAX( Alo5 - ORDLINE.Pik5, 0 ),;
                           Alo6 WITH MAX( Alo6 - ORDLINE.Pik6, 0 ),;
                           Alo7 WITH MAX( Alo7 - ORDLINE.Pik7, 0 ),;
                           Alo8 WITH MAX( Alo8 - ORDLINE.Pik8, 0 ),;
                         TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
          
            =gfAdd_Info()
            UNLOCK
          ENDIF    && End of IF
        ENDIF    && End of IF

        REPLACE &lcTmpKtTk..nProcNo WITH 4
        =RLOCK(lcTmpKtTk)
        UNLOCK IN &lcTmpKtTk
        *-- B602809,1 HDM [Start] we have to update styedye also in case single warehouse
        *ENDIF    && End of IF
        *-- B602809,1 HDM [End]

  
      *IF Statment for the rall back
      IF &lcTmpKtTk..nProcNo = 4
      
        SELECT STYLE
        *IF Statment to find the STYLE record for the current ORDLINE record
        IF SEEK( ORDLINE.Style)
          =RLOCK()
          REPLACE Alo1 WITH MAX(Alo1 - ORDLINE.Pik1, 0),;
                  Alo2 WITH MAX(Alo2 - ORDLINE.Pik2, 0),;
                  Alo3 WITH MAX(Alo3 - ORDLINE.Pik3, 0),;
                  Alo4 WITH MAX(Alo4 - ORDLINE.Pik4, 0),;
                  Alo5 WITH MAX(Alo5 - ORDLINE.Pik5, 0),;
                  Alo6 WITH MAX(Alo6 - ORDLINE.Pik6, 0),;
                  Alo7 WITH MAX(Alo7 - ORDLINE.Pik7, 0),;
                  Alo8 WITH MAX(Alo8 - ORDLINE.Pik8, 0),;
                TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
          
          =gfAdd_Info()
          UNLOCK
        ENDIF    && End of IF
        
        REPLACE &lcTmpKtTk..nProcNo WITH 5
        =RLOCK(lcTmpKtTk)
        UNLOCK IN &lcTmpKtTk
      ENDIF    && End of IF

      *C037816,1 MHM 04/06/2004 Custom Release Location For David Luke[Start]
      *B039660,1 NNA 09/01/2005 (START) Transfer all the bin location's Programs To Binmain.prg instead of Davmain.prg
      IF ASCAN(laEvntTrig,PADR("ALRELORD",10)) <> 0
        = gfDoTriger("ALRELPI",PADR("ALRELORD",10))
      ENDIF 
      *B039660,1 NNA (END)
      *C037816,1 MHM [End]
      
      SELECT ORDLINE
      
      *IF Statment for the rall back
      IF &lcTmpKtTk..nProcNo = 5
      
        =RLOCK()
        REPLACE Pik1 WITH 0,;
                Pik2 WITH 0,;
                Pik3 WITH 0,;
                Pik4 WITH 0,;
                Pik5 WITH 0,;
                Pik6 WITH 0,;
                Pik7 WITH 0,;
                Pik8 WITH 0,;
              PikTkt WITH ' ',;
              TotPik WITH 0,;
              Picked WITH .F.,;
             PikDate WITH {}

        *E126905,1 EIH 04/11/2005 [Begin] Case Release.
        REPLACE cAllocatBy      WITH  ' '
        *E126905,1 EIH 04/11/2005 [End]
        =gfAdd_Info()
        UNLOCK

        REPLACE &lcTmpKtTk..nProcNo WITH 6
        =RLOCK(lcTmpKtTk)
        UNLOCK IN &lcTmpKtTk
      ENDIF    && End of IF


      *B603318,1 Start, Release the pick ticket adornments.
      *--ORDLINE record correct, PikTkt record correct.
      IF ASCAN(laEvntTrig , PADR('RELEASE_PK',10)) <> 0 AND PIKTKT.cPickType = 'A'
        =gfDoTriger('ALRELPI',PADR('RELEASE_PK',10))

      ELSE
      *B603318,1 End.

        *IF Statment for the rall back
        IF BETWEEN(&lcTmpKtTk..nProcNo , 6 , 11) 
          =SEEK('O' + ORDLINE.Order,'ORDHDR')
          *IF There is an alternative style
          IF !EMPTY(AltStyle)
            =lfSwchSty(Style,AltStyle)
          ELSE     && Else
            =lfMoveOrd(Style , Style , PIKTKT.cWareCode , ORDHDR.cWareCode)
          ENDIF    && End of IF
        ENDIF    && End of IF

      ENDIF 


      SELECT ORDLINE
      SKIP 1
      REPLACE &lcTmpKtTk..nProcNo WITH 1 ,;
              &lcTmpKtTk..nOrdLnNo WITH RECNO()
              
      SKIP -1
      =RLOCK(lcTmpKtTk)
      UNLOCK IN &lcTmpKtTk
      
    ENDSCAN    && End of SCAN Loop

  ENDIF    && End of IF

  *--HDM B602809,1 [start] Adjust ordline index to fix seek
  lcOrdLnTag = ORDER()
  SET ORDER TO ORDLINST IN ORDLINE
  *--HDM B602809,1 [End]

  =SEEK('O' + PIKTKT.Order + PIKTKT.STORE)
  
  *LOCATE Statment to loock for another record for this Picking ticket
  *in the ORDLINE file [a record with style not in the style range]
  LOCATE REST WHILE ORDER + STORE = PIKTKT.Order + PIKTKT.STORE;
          FOR PIKTKT = PIKTKT.PIKTKT

  lcDelPktkt = PIKTKT.PIKTKT
  *IF Statment to check if the last LOCATE Statment did not found a record
  IF !FOUND()

    SELECT PIKTKT
    *MAB Replace all piktkt records. [Begin]
    *REPLACE Status WITH "X"
    lcPkTkTag  = EVALUATE(KEY())
    SCAN FOR PIKTKT = lcDelPktkt
      
      =RLOCK()
      REPLACE Status WITH "X"
      UNLOCK

      *-- Audit Trial.
      IF ASCAN(laEvntTrig,PADR("REL_PIK",10)) <> 0
        =gfDoTriger('ALRELPI',PADR("REL_PIK",10))
      ENDIF
    
    ENDSCAN
    =SEEK(lcPkTkTag)
    *MAB Replace all piktkt records. [End  ]
  
  ENDIF    && End of IF

  *--HDM B602809,1 [start] Restore ordline index
  lcOrdLnTag = ORDER()
  SET ORDER TO ORDLINST IN ORDLINE
  *--HDM B602809,1 [End]

  SELECT(lcTmpKtTk)
  *MAB Scan for all piktkt Records [Begin]
  lnTmpRec = RECNO()
  SCAN FOR PIKTKT = lcDelPktkt REST WHILE llSel
    DELETE
    =RLOCK(lcTmpKtTk)
    UNLOCK IN &lcTmpKtTk
    lnSelRec = lnSelRec - 1
    lnDelRec = lnDelRec + 1
  ENDSCAN  

  IF BETWEEN(lnTmpRec,1,RECCOUNT())
    GO lnTmpRec
  ENDIF
  *MAB Scan for all piktkt Records [End  ]

ENDSCAN    && End of SCAN Loop

*B603318,1 Start.
IF ASCAN(laEvntTrig , PADR('UPDRELEASE',10)) <> 0 AND llLinkToGl
  llPrepere = .F.
  =gfDoTriger('ALRELPI',PADR('UPDRELEASE',10))
ENDIF
*B603318,1 End.

SELECT(lcTmpKtTk)
GO TOP
*IF Statment to check if all the temp file was released
IF EOF(lcTmpKtTk)
  =lfAdUnCmSR(.T. , '')
  llContinue = .F.
  lcCurrObj = ''
ELSE    && Else
  =lfAdUnCmSR(.F. , '')
  lcCurrObj = ''
ENDIF    && End of IF

*IF One or more of the selected picking tickets has a packing list
IF llSekPak

  **Message:"Warning: picking tickets having packing lists where not released"
  **        "                             < Ok >                             "
  =gfModalGen("TRM44004B00000","ALERT")
ENDIF    && End of IF

SELECT (lcTmpKtTk)

*-- If there is no records selected
IF lnSelRec = 0
  SHOW GET pbSelNon DISABLE
  SHOW GET pbRel DISABLE
ENDIF    && End of IF lnSelRec = 0

GO TOP 
laScrMode = .F.

*IF Statment to check if there is some of the picking tickets in the 
*temp file was not released for they have not being selected
IF !EOF(lcTmpKtTk)
  laScrMode[2] = .T.
  SHOW GET pbInvert ENABLE
  =lfvpbSel()
ELSE    && Else
  llCallUnCS   = .T.
  laScrMode[1] = .T.
  SHOW GET pbSelect,1 PROMPT 'Se\<lect' DISABLE
  SHOW GET pbInvert DISABLE
ENDIF    && End of IF

SELECT ORDLINE
SET RELATION TO

SELECT(lcTmpKtTk)
SET RELATION TO

SHOW WINDOW (lcPickBrow) REFRESH
SHOW GETS

*!*************************************************************
*! Name      : lfSwchSty
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function to restore the old style
*!*************************************************************
*! Called from : lfDelScr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Current Style , Old Style
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfSwchSty

PARAMETERS lcSStyle , lcOStyle

*IF Statment for the rall back
IF &lcTmpKtTk..nProcNo = 6
      
  *IF Statment to check if the system uses multi warehouse
  *-- B602809,1 HDM [Start] we have to update styedye also in case single warehouse
  *IF llMultiWH
  *-- B602809,1 HDM [End]
    SELECT STYDYE
    =SEEK(lcSStyle + PIKTKT.cWareCode + SPACE(10))
    =RLOCK()
    REPLACE Ord1 WITH MAX(Ord1 - ORDLINE.Qty1, 0),;
            Ord2 WITH MAX(Ord2 - ORDLINE.Qty2, 0),;
            Ord3 WITH MAX(Ord3 - ORDLINE.Qty3, 0),;
            Ord4 WITH MAX(Ord4 - ORDLINE.Qty4, 0),;
            Ord5 WITH MAX(Ord5 - ORDLINE.Qty5, 0),;
            Ord6 WITH MAX(Ord6 - ORDLINE.Qty6, 0),;
            Ord7 WITH MAX(Ord7 - ORDLINE.Qty7, 0),;
            Ord8 WITH MAX(Ord8 - ORDLINE.Qty8, 0),;
          TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

    =gfAdd_Info()
    UNLOCK
  *-- B602809,1 HDM [Start] we have to update styedye also in case single warehouse
  *ENDIF    && End of IF
  *-- B602809,1 HDM [End]

  REPLACE &lcTmpKtTk..nProcNo WITH 7
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF    && End of IF

*IF Statment for the rall back
IF &lcTmpKtTk..nProcNo = 7
      
  SELECT STYLE
  =SEEK(lcSStyle)
  =RLOCK()

  REPLACE Ord1 WITH MAX(Ord1 - ORDLINE.Qty1, 0),;
          Ord2 WITH MAX(Ord2 - ORDLINE.Qty2, 0),;
          Ord3 WITH MAX(Ord3 - ORDLINE.Qty3, 0),;
          Ord4 WITH MAX(Ord4 - ORDLINE.Qty4, 0),;
          Ord5 WITH MAX(Ord5 - ORDLINE.Qty5, 0),;
          Ord6 WITH MAX(Ord6 - ORDLINE.Qty6, 0),;
          Ord7 WITH MAX(Ord7 - ORDLINE.Qty7, 0),;
          Ord8 WITH MAX(Ord8 - ORDLINE.Qty8, 0),;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

  =gfAdd_Info()
  UNLOCK

  REPLACE &lcTmpKtTk..nProcNo WITH 8
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF    && End of IF

*IF Statment for the rall back
IF &lcTmpKtTk..nProcNo = 8
      
  SELECT STYLE
  =SEEK(lcSStyle)
  =SEEK( 'S'+STYLE.Scale ,'SCALE')
  lnCnT1 = SCALE.Cnt
  =SEEK(lcOStyle)
  =SEEK( 'S'+STYLE.Scale ,'SCALE')
  lnCnT2 = SCALE.Cnt
  lcOScale=SCALE.Scale

  *IF Statment to check if the current Style has more Scales than
  *the old Style
  IF lnCnT1 > lnCnT2
    SELECT ORDLINE
    lnCanc = 0          && Varible to hold the canceled quantity
    lnCancAmt = 0       && Varible to hold the canceled amount

    *FOR Loop to get the  canceled quantity and amount
    FOR lnElm = lnCnT2 + 1 TO lnCnT1
      lcElm = STR(lnElm , 1)
      lnCanc = lnCanc + Qty&lcElm
      lnCancAmt = lnCancAmt + (Qty&lcElm * Price)
    ENDFOR    && End of FOR Loop

    =RLOCK()
    REPLACE Qty2 WITH IIF(lnCnT2 < 2 , 0 , Qty2) ,;
            Qty3 WITH IIF(lnCnT2 < 3 , 0 , Qty3) ,;
            Qty4 WITH IIF(lnCnT2 < 4 , 0 , Qty4) ,;
            Qty5 WITH IIF(lnCnT2 < 5 , 0 , Qty5) ,;
            Qty6 WITH IIF(lnCnT2 < 6 , 0 , Qty6) ,;
            Qty7 WITH IIF(lnCnT2 < 7 , 0 , Qty7) ,;
            Qty8 WITH IIF(lnCnT2 < 8 , 0 , Qty8) ,;
          TotQty WITH Qty1 + Qty2 + Qty3 + Qty4 + Qty5 + Qty6 + Qty7 + Qty8 ,;
           Book2 WITH IIF(lnCnT2 < 2 , 0 , Book1) ,;
           Book3 WITH IIF(lnCnT2 < 3 , 0 , Book3) ,;
           Book4 WITH IIF(lnCnT2 < 4 , 0 , Book4) ,;
           Book5 WITH IIF(lnCnT2 < 5 , 0 , Book5) ,;
           Book6 WITH IIF(lnCnT2 < 6 , 0 , Book6) ,;
           Book7 WITH IIF(lnCnT2 < 7 , 0 , Book7) ,;
           Book8 WITH IIF(lnCnT2 < 8 , 0 , Book8) ,;
         TotBook WITH Book1 + Book2 + Book3 + Book4 + Book5 + Book6 + Book7 + Book8

    =gfAdd_Info()
    UNLOCK

    SELECT ORDHDR
    =RLOCK()
    REPLACE Book    WITH Book - lnCanc,;
            BookAmt WITH BookAmt - lnCancAmt,;
            Open    WITH Open - lnCanc,;
            OpenAmt WITH OpenAmt - lnCancAmt
    
    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF

  REPLACE &lcTmpKtTk..nProcNo WITH 9
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF    && End of IF
        

*IF Statment for the rall back
IF &lcTmpKtTk..nProcNo = 9
      
  SELECT STYDYE
  =SEEK(lcOStyle)
  =RLOCK()
  REPLACE Ord1 WITH (Ord1 + ORDLINE.Qty1),;
          Ord2 WITH (Ord2 + ORDLINE.Qty2),;
          Ord3 WITH (Ord3 + ORDLINE.Qty3),;
          Ord4 WITH (Ord4 + ORDLINE.Qty4),;
          Ord5 WITH (Ord5 + ORDLINE.Qty5),;
          Ord6 WITH (Ord6 + ORDLINE.Qty6),;
          Ord7 WITH (Ord7 + ORDLINE.Qty7),;
          Ord8 WITH (Ord8 + ORDLINE.Qty8),;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

  =gfAdd_Info()
  UNLOCK

  REPLACE &lcTmpKtTk..nProcNo WITH 10
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF    && End of IF

*IF Statment for the rall back
IF &lcTmpKtTk..nProcNo = 10
      
  SELECT Style
  =SEEK(lcOStyle)
  =RLOCK()
  REPLACE Ord1 WITH (Ord1 + ORDLINE.Qty1),;
          Ord2 WITH (Ord2 + ORDLINE.Qty2),;
          Ord3 WITH (Ord3 + ORDLINE.Qty3),;
          Ord4 WITH (Ord4 + ORDLINE.Qty4),;
          Ord5 WITH (Ord5 + ORDLINE.Qty5),;
          Ord6 WITH (Ord6 + ORDLINE.Qty6),;
          Ord7 WITH (Ord7 + ORDLINE.Qty7),;
          Ord8 WITH (Ord8 + ORDLINE.Qty8),;
        TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

  =gfAdd_Info()
  UNLOCK

  REPLACE &lcTmpKtTk..nProcNo WITH 11
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF    && End of IF


*IF Statment for the rall back
IF &lcTmpKtTk..nProcNo = 11
      
  SELECT ORDLINE
  =RLOCK()
  REPLACE Style    WITH lcOStyle,;
          AltStyle WITH SPACE(19),;
          Scale    WITH lcOScale
  
  =gfAdd_Info()
  UNLOCK

  REPLACE &lcTmpKtTk..nProcNo WITH 12
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfMoveOrd
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function to remove the style from one warehouse
*!             to another
*!*************************************************************
*! Called from : lfDelScr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Current Style , Old Style ,
*!                     Current warehouse , Old warehouse
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfMoveOrd

PARAMETERS lcFrmSty,lcToSty,lcFrmWare,lcToWare

IF llDyelot .OR. !llMultiWH .OR. (lcFrmSty+lcFrmWare=lcToSty+lcToWare)
  RETURN
ENDIF

lnSe=SELECT()

*IF Statment for the rall back
IF &lcTmpKtTk..nProcNo = 6

  SELECT STYDYE

  *IF Statment to seek the current Style Warehouse
  IF SEEK( lcFrmSty + lcFrmWare + SPACE(10))
    =RLOCK()

    REPLACE Ord1 WITH MAX(Ord1 - ORDLINE.Qty1, 0),;
            Ord2 WITH MAX(Ord2 - ORDLINE.Qty2, 0),;
            Ord3 WITH MAX(Ord3 - ORDLINE.Qty3, 0),;
            Ord4 WITH MAX(Ord4 - ORDLINE.Qty4, 0),;
            Ord5 WITH MAX(Ord5 - ORDLINE.Qty5, 0),;
            Ord6 WITH MAX(Ord6 - ORDLINE.Qty6, 0),;
            Ord7 WITH MAX(Ord7 - ORDLINE.Qty7, 0),;
            Ord8 WITH MAX(Ord8 - ORDLINE.Qty8, 0),;
          TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF

  REPLACE &lcTmpKtTk..nProcNo WITH 7
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF    && End of IF

*IF Statment for the rall back
IF &lcTmpKtTk..nProcNo = 7

  *IF Statment to seek the old Style Warehouse
  IF SEEK(lcToSty + lcToWare + SPACE(10))
    =RLOCK()

    REPLACE Ord1 WITH (Ord1 + ORDLINE.Qty1),;
            Ord2 WITH (Ord2 + ORDLINE.Qty2),;
            Ord3 WITH (Ord3 + ORDLINE.Qty3),;
            Ord4 WITH (Ord4 + ORDLINE.Qty4),;
            Ord5 WITH (Ord5 + ORDLINE.Qty5),;
            Ord6 WITH (Ord6 + ORDLINE.Qty6),;
            Ord7 WITH (Ord7 + ORDLINE.Qty7),;
            Ord8 WITH (Ord8 + ORDLINE.Qty8),;
          TotOrd WITH Ord1+Ord2+Ord3+Ord4+Ord5+Ord6+Ord7+Ord8

    =gfAdd_Info()
    UNLOCK
  ENDIF    && End of IF

  REPLACE &lcTmpKtTk..nProcNo WITH 12
  =RLOCK(lcTmpKtTk)
  UNLOCK IN &lcTmpKtTk
ENDIF    && End of IF

SELECT(lnSe)

*!*************************************************************
*! Name      : lfvScope
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Valid function of push button Scope
*!*************************************************************
*! Called from : Control Panel , lpShow
*!*************************************************************
*! Calls       : gfOpGrid() , lfCrTmKtTk() , lfDispBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvScope

=ACOPY(laScopExpr , laTmpAray1)
=ACOPY(laScrMode , laTmpAray2)


lcExpr = gfOpGrid('ALRELPI' , .T.)

*IF The user Selected cansel [Close] From the Option Grid
IF TYPE('lcExpr') = 'L'

  =ACOPY(laTmpAray1 , laScopExpr)
  =ACOPY(laTmpAray2 , laScrMode)
  
ELSE    && Else

  llCallScop = .F.
  
  =gfCrtTmp(lcTmpKtTk , @laFileStru , 'PikTkt' , lcTmpKtTk)
  =lfDispBrow()
  
  =lfCrTmKtTk()
  SELECT (lcTmpKtTk)
  GO TOP

  lnBrRecNo = RECNO() 
  =lfDispBrow()
  lnSelRec   = 0
  lnDelRec   = 0
  lnUnSelRec = RECCOUNT()
  laScrMode = .F.

  *IF The temp file [lcTmpKtTk] is not empty
  IF !EOF()

    laScrMode[2] = .T.
    
    =lfAdUnCmSR(.F. , '')
    
    SHOW GET pbSelect ENABLE
    SHOW GET pbSelAll ENABLE
    SHOW GET pbInvert ENABLE
    SHOW GET pbSelNon DISABLE
  ELSE    && Else
    laScrMode[1] = .T.
    SHOW GET pbSelect DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbInvert DISABLE
    SHOW GET pbSelNon DISABLE
  ENDIF    && End of IF

  SHOW GETS ONLY
  
  *IF The temp file [lcTmpKtTk] is not empty and it has more than one record
  IF !EOF(lcTmpKtTk) .AND. RECCOUNT(lcTmpKtTk) > 1
    SHOW GET pbNxt ENABLE
    SHOW GET pbBtm ENABLE
    laCtrStat[2]  = "ENABLE"                && Last button
    laCtrStat[3] = "ENABLE"                 && Next button
  ELSE    && Else
    SHOW GET pbNxt DISABLE
    SHOW GET pbBtm DISABLE
    laCtrStat[2]  = "DISABLE"                && Last button
    laCtrStat[3]  = "DISABLE"                && Next button
  ENDIF    && End of IF

  SHOW GET pbTop DISABLE
  SHOW GET pbPrvs DISABLE
  laCtrStat[1]  = "DISABLE"               && First button 
  laCtrStat[4]  = "DISABLE"               && Priveus button
  SHOW MENU _mSysMenu

ENDIF    && End of IF

_CUROBJ = OBJNUM(IbBrow)

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Valid function of push button Select
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelect

REPLACE LLSEL WITH !LLSEL
=lfvpbSel()
lnSelRec   = IIF(llSel , lnSelRec + 1 , lnSelRec - 1)
lnUnSelRec = IIF(llSel , lnUnSelRec - 1 , lnUnSelRec + 1)
SHOW WINDOW (lcPickBrow) REFRESH

*IF No records was selected
IF lnSelRec = 0
  SHOW GET pbSelNon DISABLE
  SHOW GET pbRel DISABLE
  SHOW GET pbSelAll ENABLE
ELSE    && Else
  SHOW GET pbSelNon ENABLE
  SHOW GET pbRel ENABLE
  
  *IF All the records was selected
  IF lnUnSelRec = 0
    SHOW GET pbSelAll DISABLE
  ELSE    && Else
    SHOW GET pbSelAll ENABLE
  ENDIF    && End of IF
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfvSelAll
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Valid function of push button Select all
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelAll

REPLACE ALL LLSEL WITH .T.
lnSelRec   = RECCOUNT() - lnDelRec
lnUnSelRec = 0
GO lnBrRecNo
SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
SHOW WINDOW (lcPickBrow) REFRESH
SHOW GET pbRel ENABLE
SHOW GET pbSelNon ENABLE
SHOW GET pbSelAll DISABLE

*!*************************************************************
*! Name      : lfvSelNon
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Valid function of push button Select none
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelNon

REPLACE ALL LLSEL WITH .F.
lnSelRec   = 0
lnUnSelRec = RECCOUNT() - lnDelRec

GO lnBrRecNo
SHOW GET pbSelect,1 PROMPT 'Se\<lect'
SHOW WINDOW (lcPickBrow) REFRESH
SHOW GET pbRel DISABLE
SHOW GET pbSelNon DISABLE
SHOW GET pbSelAll ENABLE

*!*************************************************************
*! Name      : lfvInvert
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Valid function of push button Invert
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvInvert

REPLACE ALL LLSEL WITH !LLSEL
GO lnBrRecNo
=lfvpbSel()
lnUnSelRec = lnSelRec
lnSelRec   = RECCOUNT() - lnDelRec - lnSelRec
SHOW WINDOW (lcPickBrow) REFRESH

*IF there is no selected records
IF lnSelRec = 0
  SHOW GET pbRel DISABLE
  SHOW GET pbSelNon DISABLE
  SHOW GET pbSelAll ENABLE
ELSE    && Else
  SHOW GET pbRel ENABLE
  SHOW GET pbSelNon ENABLE

  *IF All the records was selected
  IF lnUnSelRec = 0
    SHOW GET pbSelAll DISABLE
  ENDIF    && End of IF
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function to create the Browse
*!*************************************************************
*! Called from : Scrren ALRELPIK , lfvScope()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfDispBrow

SELECT(lcTmpKtTk)
lnBrRecNo  = RECNO()

lcBrowStr="lcMarker=IIF(RECNO()=lnBrRecNo,'>',' '):1:H=' ':W=.F.,"+;
          "lcSelect=IIF(LLSEL , '»', ' '):R :H='»',"+;
          "PIKTKT    :R :H= 'Pick ticket' :13 ,"+;
          "ACCOUNT   :R :H= 'Account' :10 ,"+;
          "STORE     :R :H= 'Store' :16 ,"+;
          "ORDER     :R :H= 'Order' :9 ,"+;
          "DATE      :R :H= 'Date' ,"+;
          "CWARECODE :R :H= 'Warehouse' :13 ,"+;
          "CUSTPO    :R :H= 'Custom PO' :30 "


  BROWSE FIELDS &lcBrowStr;
         WINDOW (lcRelPkCh1);
         WHEN lfwBrows() .AND. lfvpbSel();
         VALID :F lfvBrow() ;
         IN WINDOW (gcBaseWind) ;
         LOCK 0;
         NOAPPEND;
         NOCLEAR;
         NODELETE;
         NOWAIT;
         NOEDIT;
         NOMENU;
         SAVE;
         TITLE lcPickBrow

       
*!*************************************************************
*! Name      : lfCrTmKtTk
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function to create the Temp file [lcTmpKtTk]
*!*************************************************************
*! Called from : lfvScope()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfCrTmKtTk

*SELECT PIKTKT
*B801992,1 Adjust Picket ticket order [Begin]
lnCurrAl = SELECT(0)

*B802758,1 Save current Ordline index [Begin.] 
*-- change Ordline Index to be on 'O'+Order 
PRIVATE lcSavOrder
SELECT OrdLine
lcSavOrder = ORDER()
SET ORDER TO OrdLinSt
*B802758,1 Save current Ordline index [End.] 

SELECT PIKTKT
lcCurOrder = ORDER()
lcCurKey   = PIKTKT
SET ORDER TO PIKTKT
*B801992,1 Adjust Picket ticket order [End  ]

M.LLSEL = .F.
M.NORDLNNO = 0
M.NPROCNO = 1

lcWhileCon = IIF(EMPTY(laScopExpr[3,2]) , "" , " .AND. STYLE <= laScopExpr[3,2]")

*IF The From Picking ticket is empty
IF EMPTY(laScopExpr[1,1])
  GO TOP
ELSE    && Else
  SEEK laScopExpr[1,1]
ENDIF    && End of IF

lcPrtCond  = IIF(lnRpPrtFlg = 1 , '' , IIF(lnRpPrtFlg = 2 , ' .AND. PIKTKT.PrtFlag = "P"' , ' .AND. PIKTKT.PrtFlag <> "P"'))
lcScanExp1 = laScopExpr[1,3]         && Varible to hold the While condition
lcScanExp2 = lcPrtCond + laScopExpr[2,3] + laScopExpr[3,3]       && Varible to hold the For condition

lcSetNear = SET('NEAR')
SET NEAR ON
  
*B603347,1 MAB 04/23/2000 == Apply the same filter to collect condition [Begin]
*SCAN Loop to scan the PIKTKT file for the selected criterea
*SCAN REST &lcScanExp1 ;
      FOR STATUS <> "X" .AND. PIKTKT <> "******" .AND. !SEEK(PIKTKT,"PACK_HDR") &lcScanExp2
SCAN REST &lcScanExp1 ;
      FOR !(STATUS$"CX") .AND. PIKTKT <> "******" .AND. !SEEK(PIKTKT,"PACK_HDR") &lcScanExp2
*B603347,1 MAB 04/23/2000 == Apply the same filter to collect condition [End  ]

  SCATTER MEMVAR MEMO
  INSERT INTO (lcTmpKtTk) FROM MEMVAR
    
ENDSCAN    && End of SCAN Loop
SET NEAR &lcSetNear

*B802758,1 Restore Old Ordline index [Begin.]
SELECT OrdLine
SET ORDER TO &lcSavOrder
*B802758,1 Restore Old Ordline index [End.]

*B801992,1 Restore Order and Record [Begin]
SELECT PIKTKT
=SEEK(lcCurKey)
SET ORDER TO &lcCurOrder
SELECT (lnCurrAl)
*B801992,1 Restore Order and Record [End  ]

*-- end of lfCrTmKtTk.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : When function of the Browse [lcPickBrow]
*!*************************************************************
*! Called from : The Browse [lcPickBrow]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwBrows

lnBrRecNo  = RECNO(lcTmpKtTk)
SHOW WINDOW (lcPickBrow) REFRESH

    
llEOF = .F.
llBOF = .F.

IF !EOF() .AND. !BOF()
  SKIP 1
  IF EOF()
    llEOF = .T.
    SKIP -2
    IF BOF()
      llBOF = .T.
    ELSE
      SKIP 1
    ENDIF    && End of IF
  ELSE
    SKIP -2
    IF BOF()
      llBOF = .T.
    ELSE
      SKIP 1
    ENDIF    && End of IF
  ENDIF    && End of IF
ELSE
  llEOF = .T.
  llBOF = .T.
ENDIF

laCtrStat[1]  = IIF(llBOF , 'DISABLE' , 'ENABLE')         && First button 
laCtrStat[4]  = laCtrStat[1]                              && Priveus button
laCtrStat[2]  = IIF(llEOF , 'DISABLE' , 'ENABLE')         && Last button
laCtrStat[3] = laCtrStat[2]                               && Next button

SHOW GET pbTop  &laCtrStat[1]
SHOW GET pbPrvs &laCtrStat[4]
SHOW GET pbBtm  &laCtrStat[2]
SHOW GET pbNxt  &laCtrStat[3]
SHOW MENU _mSysMenu

*!*************************************************************
*! Name      : lfvpbSel
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function to arange the push button select prompt
*!*************************************************************
*! Called from : lfvSelect() , lfvInvert() , The Browse [lcPickBrow]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T.
*!*************************************************************
*
FUNCTION lfvpbSel

*IF The record is selected
IF LLSEL
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
ELSE    && Else
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'
ENDIF    && End of IF

RETURN .T.

*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : When function of the invisible button IbBrow
*!*************************************************************
*! Called from : invisible button IbBrow
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*
FUNCTION lfwIbBrow

*IF The left mouse button is not pressed
IF !MDOWN()
  KEYBOARD "{ALT+B}" CLEAR 
  RETURN .T.
ENDIF    && End of IF

RETURN .F.

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function to trap the keys for the Browse
*!*************************************************************
*! Called from : Scrren ALRELPIK Deactivate Function
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfBrowTrap

*IF The window on top is the Browse
IF WONTOP(lcPickBrow)
  glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH 1
  ON KEY LABEL BACKTAB DO lfTraps WITH 2
  ON KEY LABEL Alt+L DO lfvSelect
  ON KEY LABEL Alt+A DO lfvSelAll
  ON KEY LABEL Alt+N DO lfvSelNon
  ON KEY LABEL Alt+I DO lfvInvert
  ON KEY LABEL ALT+B 
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*! Called from : Scrren ALRELPIK Activate Function
*!*************************************************************
*! Calls       : gfStopBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfBrwUnTrp

*IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcPickBrow) .AND. glFromBrow
  =gfStopBrow()
  glFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL Alt+L
  ON KEY LABEL Alt+A
  ON KEY LABEL Alt+N
  ON KEY LABEL Alt+I
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcPickBrow)
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfTraps
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function to be called from some traped keys
*!*************************************************************
*! Called from : lfBrowTrap()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 , 2
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfTraps

PARAMETERS lnTrap


DO CASE 
  
  CASE lnTrap = 1
    ACTI WINDOW (lcRelPkCh2) TOP
    _CUROBJ = OBJNUM(pbSelect)

  CASE lnTrap = 2
    ACTI WINDOW (lcRelPkCh2) TOP  
    _CUROBJ = OBJNUM(ibDum1)

ENDCASE

*!*************************************************************
*! Name      : lfwPikTkt
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : When function of the Picking tickit number
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwPikTkt

lcOldPikTk = EVALUATE(SYS(18))      && Varible to hold the old Picking tickit number

*!*************************************************************
*! Name      : lfvPikTkt
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Valid function of the Picking tickit number
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvPikTkt

*MAB 04/20/2000
*lcPrtCond = IIF(lnRpPrtFlg = 1 , '' , IIF(lnRpPrtFlg = 2 , ' .AND. PIKTKT.PrtFlag = "P"' , ' .AND. PIKTKT.PrtFlag <> "P"'))
lcPrtCond = ""
IF lnRpPrtFlg=2
 lcPrtCond = [ AND (PrtFlag = "P")]
ENDIF

IF lnRpPrtFlg>2
 lcPrtCond = [ AND (PrtFlag <> "P")]
ENDIF

lcVar = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

*B801992,1 Adjust Picket ticket order [Begin]
lnCurrAl = SELECT(0)
SELECT PIKTKT
lcCurOrder = ORDER()
lcCurKey   = PIKTKT
SET ORDER TO PIKTKT
*B801992,1 Adjust Picket ticket order [End  ]


*MAB 04/20/2000 [Begin]
*IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj) .OR. PIKTKT.STATUS$ "X" )
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj) .OR. PIKTKT.STATUS$ "CX" .OR. SEEK(lcObj,"PACK_HDR"))
*MAB 04/20/2000 [End  ]

  PRIVATE lcBrowFilt	
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "PIKTKT    :R :H= 'Pick ticket',"+;
               "ACCOUNT   :R :H= 'Account' ,"+;
               "STORE     :R :H= 'Store' ,"+;
               "ORDER     :R :H= 'Order' ,"+;
               "LABELS    :R :H= 'Labels' ,"+;
               "DATE      :R :H= 'Date' ,"+;
               "CWARECODE :R :H= 'WH. Code' ,"+;
               "CUSTPO    :R :H= 'Customer PO' "
 
  lcFile_Ttl = "Pick tickets"
  lcBrowFilt = [FOR !(STATUS$"CX") AND (PIKTKT<>"******")] + lcPrtCond +;
  		       [ AND !SEEK(PIKTKT,"PACK_HDR")] 

  *B603347,1 elemnate comlet piktkt to apper in the browse [Begin]          
  *=gfBrows('FOR STATUS <> "X" .AND. PIKTKT <> "******" .AND. !SEEK(PIKTKT,"PACK_HDR") &lcPrtCond','PIKTKT','laTemp')  =gfBrows('FOR STATUS <> "X" .AND. STATUS <> "C" .AND. PIKTKT <> "******" .AND. !SEEK(PIKTKT,"PACK_HDR") &lcPrtCond','PIKTKT','laTemp')  =gfBrows('FOR STATUS <> "X" .AND. STATUS <> "C" .AND. PIKTKT <> "******" .AND. !SEEK(PIKTKT,"PACK_HDR") &lcPrtCond','PIKTKT','laTemp')
  =gfBrows(lcBrowFilt,'PIKTKT','laTemp')
  *B603347,1 elemnate comlet piktkt to apper in the browse [End  ]          

  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = lcOldPikTk
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field

*B801992,1 Restore Order and Record [Begin]
SELECT PIKTKT
=SEEK(lcCurKey)
SET ORDER TO &lcCurOrder
SELECT (lnCurrAl)
*B801992,1 Restore Order and Record [End  ]
RETURN



*!*************************************************************
*! Name      : lfvDate
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Valid function of the Date
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvDate

*!*************************************************************
*! Name      : lfwStyle
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : When function of the Style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfwStyle

lcOldStyle = EVALUATE(SYS(18))      && Varible to hold the old Style

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Valid function of the Style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvStyle
lcVar = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*IF Statment to check if we are going to Browse

*B802758 correct case of making Style Empty without browsing [Begin.]
*-- when Style is already selected and we want to Remove selection
*-- a browse screen appears even though if there is style is empty
PRIVATE lcPicture,lcStyleObj,lnI,llEmptySty

lcPicture  = SUBSTR(lcStylePct,1,1)   && Var hold 1st Char from lcStylePct
lcStyleObj = ''                       && Var hold Style (Maj/NonMaj) contents without separators
llEmptySty = .T.                      && Flag to detect if style is empty

*-- loop around lcStylePct Char by Char
FOR lnI = 1 TO LEN(lcStylePct)
  *-- If Current Char in Style Pict LIKE lcPicture and is not separator
  IF SUBSTR(lcStylePct,lnI,1) = lcPicture AND !EMPTY(SUBSTR(lcObj,lnI,1))
    llEmptySty = .F.
    EXIT 
  ENDIF  
ENDFOR
*-- End loop around lcStylePct

lcObj = IIF(llEmptySty,'',lcObj)
*B802758 correct case of making Style Empty without browsing [End.]

IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'STYLE'))
  SELECT STYLE
  lcObj = gfStyBrw('I',"","",.F.)
  lcObj = IIF(EMPTY(lcObj) , lcOldStyle , lcObj)
ENDIF    && End of IF

&lcVar = lcObj      && Update the field

*!*************************************************************
*! Name      : lfFindStyl
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function to check if the Pick ticket has one 
*!             or more Style in the Style range
*!*************************************************************
*! Called from : lfCrTmKtTk
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*
FUNCTION lfFindStyl

SELECT ORDLINE

=SEEK('O' + PIKTKT.Order + PIKTKT.STORE + laScopExpr[3,1])

LOCATE REST WHILE ORDER + STORE = PIKTKT.Order + PIKTKT.STORE &lcWhileCon ;
        FOR PIKTKT = PIKTKT.PIKTKT

SELECT PIKTKT
RETURN FOUND('ORDLINE')

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : Haytham El_Sheltawi
*! Date      : 09/07/1997
*! Purpose   : Function to open and close the needed files
*!*************************************************************
*! Called from : ALAUTAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .T. [To open the files] ,
*!                     .F. [To close the files]
*!*************************************************************
*! Return      : None
*!*************************************************************
*E301077,25 MAB 01/07/1999 Change the way used to open and close files.
*
FUNCTION lfOpenFile
*E301077,25 02/27/99 No close code (The main system deal with files in laFileName)
*PARAMETERS llOpen

PRIVATE lnCount

*E301077,25 02/27/99 Comment out the following if else endif block [Begin]
*E301077,25 02/27/99 Because main system close files in laFileName array.
*IF The function is to open the files
*02/27/99 IF llOpen
  *FOR Loop to loop the array laUsedFile
*02/27/99   FOR lnCount = 1 TO ALEN(laUsedFile , 1)
    
    *IF The file is not open
*02/27/99     IF !USED(laUsedFile[lnCount , 1])
 
      *E301077,25 Change the way open files. [Begin]
      *SELECT 0
      *USE (gcDataDir + laUsedFile[lnCount , 1])
      
      *IF The file is to be opened with an Index Tag
      *IF !EMPTY(laUsedFile[lnCount , 2])
      *  SET ORDER TO TAG (laUsedFile[lnCount , 2])
      *ENDIF    && End of IF

*02/27/99       = gfOpenFile(gcDataDir+laUsedFile[lnCount,1],;
*02/27/99                    IIF(EMPTY(laUsedFile[lnCount , 2]),'',gcDataDir+laUsedFile[lnCount , 2]),'SH')
      *E301077,25 Change the way open files. [End  ]

*02/27/99       laUsedFile[lnCount , 3] = .T.

*02/27/99     ELSE    && Else

*02/27/99       SELECT (laUsedFile[lnCount , 1])

      *IF The file is to be opened with an Index Tag
*02/27/99       IF !EMPTY(laUsedFile[lnCount , 2])
*02/27/99         SET ORDER TO TAG (laUsedFile[lnCount , 2])
*02/27/99       ELSE    && Else
*02/27/99         SET ORDER TO
*02/27/99       ENDIF    && End of IF
*02/27/99       SET FILTER TO 
*02/27/99       laUsedFile[lnCount , 3] = .F.
*02/27/99     ENDIF    && End of IF
*02/27/99     GO TOP
*02/27/99   ENDFOR    && End of FOR Loop

*02/27/99 ELSE    && Else

  *FOR Loop to loop the array laUsedFile
*02/27/99   FOR lnCount = 1 TO ALEN(laUsedFile , 1)

    *IF The file was not open befor
*02/27/99     IF laUsedFile[lnCount , 3]

      *E301077,25 Change the way close files. [Begin]
      *USE IN (laUsedFile[lnCount , 1])
*02/27/99       =gfCloseFile(laUsedFile[lnCount , 1])
      *E301077,25 Change the way close files. [End  ]

*02/27/99     ENDIF    && End of IF
*02/27/99   ENDFOR    && End of FOR Loop
*02/27/99 ENDIF    && End of IF
*E301077,25 02/27/99 Comment out the following if else endif block [End  ]

*E301077,25 02/27/99 Adjust code to open files only. [Begin]
*FOR Loop to loop the array laUsedFile
FOR lnCount = 1 TO ALEN(laUsedFile , 1)
    
  *IF The file is not open
  IF !USED(laUsedFile[lnCount , 1])
 
    = gfOpenFile(gcDataDir+laUsedFile[lnCount,1],;
                 IIF(EMPTY(laUsedFile[lnCount , 2]),'',gcDataDir+laUsedFile[lnCount , 2]),'SH')
  ELSE 

    SELECT (laUsedFile[lnCount,1])
    SET ORDER TO (laUsedFile[lnCount,2])

  ENDIF
  
ENDFOR
*E301077,25 02/27/99 Adjust code to open files only. [End  ]
*-- end of lfOpenFile.

*!*************************************************************
*! Name      : lfCreatExp
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function to create the selected critirea expression
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfCreatExp
laScopExpr[1,1] = SUBSTR(laOGFxFlt[1,6] , 1 , ATC('|' , laOGFxFlt[1,6]) - 1)
laScopExpr[1,2] = SUBSTR(laOGFxFlt[1,6] , ATC('|' , laOGFxFlt[1,6]) + 1)
laScopExpr[1,3] = IIF(EMPTY(laScopExpr[1,2]) , " " , "WHILE PikTkt <= laScopExpr[1,2]")

laScopExpr[2,1] = CTOD(SUBSTR(laOGFxFlt[2,6] , 1 , ATC('|' , laOGFxFlt[2,6]) - 1))
laScopExpr[2,2] = CTOD(SUBSTR(laOGFxFlt[2,6] , ATC('|' , laOGFxFlt[2,6]) + 1))
laScopExpr[2,3] = IIF(!EMPTY(laScopExpr[2,1]) , " .AND. DATE >= laScopExpr[2,1]" , "");
                    + IIF(!EMPTY(laScopExpr[2,2]) , " .AND. DATE <= laScopExpr[2,2]" , "")
laScopExpr[3,1] = SUBSTR(laOGFxFlt[3,6] , 1 , ATC('|' , laOGFxFlt[3,6]) - 1)
laScopExpr[3,2] = SUBSTR(laOGFxFlt[3,6] , ATC('|' , laOGFxFlt[3,6]) + 1)
laScopExpr[3,3] = IIF(!EMPTY(laScopExpr[3,1]) .OR. !EMPTY(laScopExpr[3,2]) , " .AND. lfFindStyl()" , "")

DIMENSION laSavScop(ALEN(laOGFxFlt , 1) , ALEN(laOGFxFlt , 2))
=ACOPY(laOGFxFlt , laSavScop)
lnOPrtFlag = lnRpPrtFlg

*!*************************************************************
*! Name      : lfSekFirst
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function find the first record in ORDLINE for the 
*!             current record in PIKTKT that meet the selected
*!             critirea
*!*************************************************************
*! Called from : lfDelScr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*
FUNCTION lfSekFirst

*--HDM B602809,1 [start] Ddefine a new variable to hold the ordline index
PRIVATE lcOrdLnTag
*--HDM B602809,1 [End]

*IF Uncompleted session and there is a ORDLINE record number saved in
*The temp file [lcTmpKtTk]
IF llContinue .AND. &lcTmpKtTk..nOrdLnNo <> 0
  
  *IF The ORDLINE record number is valid
  IF &lcTmpKtTk..nOrdLnNo <= RECCOUNT()
    GO &lcTmpKtTk..nOrdLnNo
    RETURN .T.
  ELSE    && Else
    RETURN .F.
  ENDIF    && End of IF
ELSE    && Else find the first record

  *--HDM B602809,1 [start] Adjust ordline index to fix seek
  lcOrdLnTag = ORDER()
  SET ORDER TO ORDLINST IN ORDLINE
  *--HDM B602809,1 [End]

  =SEEK('O' + PIKTKT.Order + PIKTKT.STORE + laScopExpr[3,1])


  LOCATE REST WHILE ORDER + STORE = PIKTKT.Order + PIKTKT.STORE &lcWhileCon ;
          FOR PIKTKT = PIKTKT.PIKTKT

  *--HDM B602809,1 [start] Restore ordline index
  lcOrdLnTag = ORDER()
  SET ORDER TO ORDLINST IN ORDLINE
  *--HDM B602809,1 [End]

  RETURN FOUND()
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfSekPak
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function to check if the Pick ticket has a packing list
*!*************************************************************
*! Called from : lfDelScr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*
FUNCTION lfSekPak

*IF The Pick ticket has a packing list
IF SEEK(PIKTKT.PIKTKT,"PACK_HDR")
  llSekPak = .T.
  RETURN .F.
ELSE    && Else
  RETURN .T.
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfFndUnCSe
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Function to check for the exist of uncompleted sessions
*!*************************************************************
*! Called from : ALRELPI.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfFndUnCSe

PRIVATE llReturn

llReturn = gfUnCompSession('ALRELPI' , lnSessNo , 'Release Allocation')

IF llReturn
  lcCurrObj  = ALLTRIM(UNCMSESS.cCurrObj) + '()'
  lcSession  = UNCMSESS.cSession
  lnUnCmRec  = RECNO('UNCMSESS')
  =lfPrepVar()
ENDIF

RETURN llReturn

*!*************************************************************
*! Name      : lfActPad
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Called from : ALRELPI.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P
ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP

DEFINE POPUP _OPTIONPOP MARGIN SHADOW
DEFINE BAR 1 OF _OPTIONPOP PROMPT 'Release Pick tickets'   SKIP FOR (lnSelRec = 0)
DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Scope'

ON SELECTION POPUP _OPTIONPOP DO lfvActBar

*!*************************************************************
*! Name      : lfvActBar
*! Developer : Haytham El_Sheltawi
*! Date      : 05/13/1997
*! Purpose   : POPUP _OPTIONPOP SELECTION 
*!*************************************************************
*! Called from : POPUP _OPTIONPOP
*!*************************************************************
*! Calls       : lfRelScr() , lfvScope()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvActBar

DO CASE
  CASE BAR() = 1      && Release Pick tickets
    = lfRelScr()
    
  CASE BAR() = 2      && Scope
    = lfvScope()
    
ENDCASE

*!*************************************************************
*! Name      : lfOGWhen
*! Developer : Haytham El_Sheltawi
*! Date      : 05/14/1997
*! Purpose   : When function for the Option Grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfOGWhen

IF ALEN(laSavScop , 1) > 1
  =ACOPY(laSavScop , laOGFxFlt)
  lnRpPrtFlg = lnOPrtFlag
ENDIF

*!*************************************************************
*! Name      : lfAdUnCmSR
*! Developer : Haytham El_Sheltawi
*! Date      : 05/06/98
*! Purpose   : Adding record in uncomplete session file 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAdUnCmSR()
*!*************************************************************
*
FUNCTION lfAdUnCmSR

PARAMETERS llComplete , lcCurObj

PRIVATE lnAlias

llComplete = IIF(TYPE('llComplete') <> 'L' , .F. , llComplete)
lcCurObj   = IIF(TYPE('lcCurObj') <> 'C' , '' , lcCurObj)
lnAlias = SELECT(0)

SELECT UnCmSess
IF lnUnCmRec = 0
  IF !SEEK('I')
    APPEND BLANK
  ENDIF
  lnUnCmRec = RECNO()
  =RLOCK()
ELSE
  GO lnUnCmRec
ENDIF

BLANK
REPLACE Status     WITH IIF(llComplete , 'C' , 'O')       ,;
        cUTranType WITH 'ALRELPI' ,;
        cUserId    WITH gcUser_id ,;
        cSession   WITH lcSession ,;
        cProgram   WITH 'ALRELPI' ,;
        cCurrScr   WITH 'ALRELPI' ,;
        cCurrObj   WITH lcCurObj  ,;
        dTranDate  WITH gdSysDate ,;
        cTranTime  WITH TIME()


IF !llComplete
  lcFiles = "lcTmpKtTk," + lcTmpKtTk + "," + ORDER(lcTmpKtTk) + ";"
  =gfSavSess('ALRELPI' , lcFiles , @laVarTSav , lcSession)
ENDIF

UNLOCK
=RLOCK()

SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvBrow
*! Developer : Haytham El_Sheltawi
*! Date      : 05/14/98
*! Purpose   : The browse valid function
*!*************************************************************
*! Calls     : gfStopBrow()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvBrow()
*!*************************************************************
*
FUNCTION lfvBrow

IF !WONTOP(lcPickBrow)
  =gfStopBrow()
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL Alt+L
  ON KEY LABEL Alt+A
  ON KEY LABEL Alt+N
  ON KEY LABEL Alt+I
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcPickBrow)
ENDIF

*!*************************************************************
*! Name      : lfPrepVar
*! Developer : Haytham El_Sheltawi
*! Date      : 05/17/1998
*! Purpose   : Function to prepare the needed variabels in case
*!             of incompleted session
*!*************************************************************
*! Called from : lfFndUnCSe()
*!*************************************************************
*! Calls       : gfThermo()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfPrepVar

PRIVATE lnTotRec , lnCurnRec
SELECT (lcTmpKtTk)

lnSelRec   = 0
lnUnSelRec = 0
lnDelRec   = RECCOUNT()
lnTotRec   = RECCOUNT()          && Varible to hold the Total count to be done for the thermometer
lnCurnRec  = 0                   && Varible to hold the current count to be done for the thermometer

*SCAN Loop to scan the temp. Order lines file
SCAN
  lnCurnRec = lnCurnRec + 1
  lnDelRec  = lnDellRec - 1
  
  *IF the current record is selected
  IF llSel
    lnSelRec   = lnSelRec + 1
  ELSE
    lnUnSelRec = lnUnSelRec + 1
  ENDIF    && End of IF
  =gfThermo(lnTotRec , lnCurnRec , "Preparing data for incompleted session... ")
ENDSCAN    && End of SCAN Loop
GO TOP

=gfThermo(lnTotRec , lnTotRec , "Preparing data for incompleted session... ")


