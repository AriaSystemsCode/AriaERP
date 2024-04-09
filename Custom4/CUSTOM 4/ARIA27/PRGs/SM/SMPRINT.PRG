*E300399,1 Hesham 05/26/96 Define variable to hold if the printing
*E300399,1 is auto compression or not the variable name is m.g_pdAcomp
*E300399,1 to be saved with the printer setup values
*B600897,1  HS  04/02/1997  1)Remove the ports screen and use a popup
*B600897,1                  2)Add some new ports if the platform was Win95
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*:************************************************************************
*:
*: Procedure file: SMPRINT.PRG 
*:                 (Printers)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Hesham El-Sheltawy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*:************************************************************************

EXTERNAL ARRAY laData,laDefProc
*************************************************************
 STORE " " TO m.g_foxuser, m.talk, m.fox, m.deleted, m.workarea, ;
  		m.g_pddriver, m.g_pdorientation, m.g_pdstyle, m.g_pdstroke, m.g_pdleading, ;
		m.g_pdcpi, m.g_pdlpi, m.g_pdfontsize, m.g_pdfont, m.g_pdtmargin, ;
		m.g_pdlmargin, m.g_pdname, m.g_saved, m.found, m.g_action, m.g_pdpgsize, ;
		m.chg_flag, m.g_pdprocs, m.save_setup, m.trbetween, m.escape, m.none, m.exact, ;
		m.readonly, m.memofield, m.tempfile, m.g_setfields, m.save_err, m.g_err_flag,m.g_pdAcomp

*************************************************************

=SetMemVar()
DIMENSION laData[5],laDefProc[10],laKeyField[1,4],g_pdfiles[9]

laDefProc     = .T.
laDefProc[9]  = .F.
STORE "" TO g_pdFiles,lapEsc
lcOld_ID      = ""
llFirstTime   = .T.
llBrowse      = .F.

*B600897,1 Add this lines to add a new varibles and to make the array
*          laPorts visible to the program [Begin]
puPort = ' '  && Varible to create the Popup
lcOldPort = ' '   && Varible to hold the old Port
DIMENSION laPorts(1,1)    && Array to hold the Ports
lcPortSt = 'DISABLE'      && Varible to hold the popup status
*B600897,1 Add this lines [End]

IF !gfSetup()    
 RETURN
ENDIF  

llDoLocal  = .T.

DIMENSION lapEsc[1]
lapEsc=""

*B600897,1 Remove this line [Begin]
*STORE SPACE(30) TO laData[5]
*B600897,1 Remove this line [End]

llFirstTime    = .T.

IF !WEXIST(lcBaseWind)
  g_pdfiles        = " "
  laKeyField[1,1]  = 'laData[1]'
  laKeyField[1,2]  = .T.
  laKeyField[1,3]  = 'cPrint_ID'
  laKeyField[1,4]  = 1
  laData           = ""
  laData[5]        = .F.
  laData[6]        = 80
ENDIF  

IF !glFlNotOpn
  *E300683,4 Call *.SPR from screens directory
  * DO SMprint.SPR 
  DO (gcScrDir + gcWinAppl + '\SMprint.SPR')
  *E300683,4 end  
ELSE
  glFlNotOpn=.F.  
  =gfStopRead()
ENDIF  

  
*!*******************************************************************
*!
*!      PROCEDURE : lpShow
*!
*!*******************************************************************
*
PROCEDURE lpShow
EXTERNAL ARRAY laScrMode

IF llFirstTime
  lcSavAlias=SELECT(0)
  SELECT SYCP_ESC
*  DIMENSION lapEsc[RECCOUNT('sycp_esc'),1]
*  lapEsc=''
*  COPY TO ARRAY lapEsc FIELDS p_name
  SELECT DIST p_name FROM (gcSysHome+"sycp_esc") ;
  ORDER BY p_name;
  INTO ARRAY laPEsc
  SELECT(lcSavAlias)
  llFirstTime = .F.
ENDIF

IF laScrMode[1] OR laScrMode[4] 
  puWidth = "80  Column"
ELSE
  puWidth = ALLTRIM(STR(laData[6],3,0)) + ;
            SPACE(3-LEN(ALLTRIM(STR(laData[6],3,0)))) + ;
            " Column"  
ENDIF  

IF !EMPTY(laData[1])
  =SetMemVar()
  IF SEEK(laData[1])
    RESTORE FROM MEMO Mprnt_drv ADDITIVE
  ENDIF
ENDIF
pblan=IIF(TYPE('laData[5]')='L' AND laData[5]=.T.,1,2)
SHOW GET puWidth
SHOW GET pblan

*B600897,1 Add this lines to control the Enabling and disabling of the
*          Port popup [Begin]
*B600897,1 IF Statment to check if the Radio button Lan was checked
IF laData[5]
  puPort = ' '    && Clear the popup
  SHOW GET puPort DISABLE
  lcPortSt = 'DISABLE'      && Varible to hold the popup status
ELSE     && Else
  *B600897,1 IF Statment to check if the screen mode was Edit or Add
  IF laScrMode[3] OR laScrMode[4] 

    *B600897,1 IF Statment to check if the platform is Win95
    IF RIGHT( OS(1) , 2) = '95'
      DIMEN laPorts[12]
      laPorts[1] = 'LPT1'
      laPorts[2] = 'LPT2'
      laPorts[3] = 'LPT3'
      laPorts[4] = 'LPT4'
      laPorts[5] = 'LPT5'
      laPorts[6] = 'LPT6'
      laPorts[7] = 'LPT7'
      laPorts[8] = 'LPT8'
      laPorts[9] = 'LPT9'
      laPorts[10] = 'COM1'
      laPorts[11] = 'COM2'
      laPorts[12] = 'COM3'
    ELSE    && Else
      DIMENSION laPorts[7,1]    && Array to hold the Ports
      laPorts[1,1] = "PRN"
      laPorts[2,1] = "LPT1"
      laPorts[3,1] = "LPT2"   
      laPorts[4,1] = "LPT3"
      laPorts[5,1] = "COM1"
      laPorts[6,1] = "COM2"
      laPorts[7,1] = "COM3"
    ENDIF    && End of IF

    laData[12] = IIF(!EMPTY(laData[12]) , laData[12] , 'LPT1' )
    puPort = laData[12]     && Restore the old Value of the popup
    SHOW GET puPort ENABLE
    lcPortSt = 'ENABLE'      && Varible to hold the popup status
  ELSE     && Else
    puPort = laData[12]     && Restore the old Value of the popup
    SHOW GET puPort DISABLE
    lcPortSt = 'DISABLE'      && Varible to hold the popup status
  ENDIF    && End of IF
ENDIF    && End of IF
*B600897,1 Add this lines [End]

*!*******************************************************************
*!
*!       PROCEDURE : lpSavScr
*!
*!*******************************************************************
*
PROCEDURE lpSavScr

EXTERNAL ARRAY laData,laScrMode
m.g_pdname = laData[1]  
laData[3]  = IIF(EMPTY(ALLTRIM(laData[3])),laPEsc[1],laData[3])
G_PDDRIVER = laData[3]  
IF laScrMode[4]
  IF !SEEK(" ")
    APPEND BLANK
  ENDIF
ENDIF

GATHER FROM laData FIELDS &lcScFields  
SAVE TO MEMO mPrnt_drv ALL LIKE g_pd*

*!*******************************************************************
*!
*!      Function : lfvData_1 
*!           
*!*******************************************************************   
*
FUNCTION lfvData_1

IF llBrowse .OR. (LASTKEY()=13 AND !EMPTY(laData[1]))
  =gfSeekRec()
ENDIF
llBrowse = .F.

*!*******************************************************************
*!
*!      Function : lfvData_2 
*!           
*!*******************************************************************
*
FUNCTION lfvData_2

*!*******************************************************************
*!
*!      Function : lfvSetUp 
*!
*!*******************************************************************
*
FUNCTION lfvSetUp

IF laScrMode[4]
  =SetMemVar()
ELSE
  m.g_pdname = laData[1]  
  G_PDDRIVER = laData[3]  
ENDIF
m.g_pdname   = laData[1]  
G_PDDRIVER   = laData[3]  
IF !EMPTY(laData[1])
  *E300683,4 Call *.SPR from screens directory
  *DO SMPRTDF.SPR 
  DO (gcScrDir + gcWinAppl + '\SMPRTDF.SPR')
  *E300683,4 end  
ENDIF

*!*******************************************************************
*!
*!      Function : lfvData_4 
*!
*!*******************************************************************
*
FUNCTION lfvData_4

lcAlias    = SELECT(0)
SELECT sycWrkSt
lcTempBr   = lcBrFields
DIMENSION laTempData[1]
laTempData =  ""
SET ORDER TO 1
IF !EMPTY(laData[4])
  IF !SEEK(laData[4])
     lcBrFields = "cwrkst_id :H='WorkStation ID',cwrk_Name :H='WorkStation Name',"+;
                  "cwrk_loc :H='WorkStation Location',cwrk_sNum :H='WorkStation Number'"
     DO gfBrows WITH .F.,"cwrkst_id " ,"laTempData","WorkStation File"
     IF !EMPTY(laTempData[1])
       laData[4] = laTempData[1]
       SHOW GET laData[4]
     ELSE
       _CUROBJ = OBJNUM(laData[4])  
     ENDIF  
  ENDIF  
ENDIF
lcBrFields  =  lcTempBr
SELECT(lcAlias) 

*!*******************************************************************
*!
*!       Function : lfvLan 
*!           
*!*******************************************************************
*
FUNCTION lfvLan

laData[5] = IIF(pblan=1,.T.,.F.)
IF laData[5]

  *B600897,1 Add this lines to control the Enabling and disabling of the
  *          Port popup [Begin]
  puPort = ' '     && Clear the popup
  SHOW GET puPort DISABLE
  lcPortSt = 'DISABLE'      && Varible to hold the popup status
  *B600897,1 Add this lines [End]

  *E300683,4 Call *.SPR from screens directory
  * DO SMLANOP.SPR 
  DO (gcScrDir + gcWinAppl + '\SMLANOP.SPR')
  *E300683,4 end  
ELSE

  *B600897,1 Add this lines to use more ports if the platform was Win95 [Begin]
  *B600897,1 IF Statment to check if the platform is Win95
  IF RIGHT( OS(1) , 2) = '95'
    DIMEN laPorts[12]
    laPorts[1] = 'LPT1'
    laPorts[2] = 'LPT2'
    laPorts[3] = 'LPT3'
    laPorts[4] = 'LPT4'
    laPorts[5] = 'LPT5'
    laPorts[6] = 'LPT6'
    laPorts[7] = 'LPT7'
    laPorts[8] = 'LPT8'
    laPorts[9] = 'LPT9'
    laPorts[10] = 'COM1'
    laPorts[11] = 'COM2'
    laPorts[12] = 'COM3'
  ELSE    && Else
  *B600897,1 Add this lines to use more ports if the platform was Win95 [End]

    DIMENSION laPorts[7]
    laPorts[1] = "PRN"
    laPorts[2] = "LPT1"
    laPorts[3] = "LPT2"   
    laPorts[4] = "LPT3"

    *B600897,1 Change this lines to use the correct Ports [Begin]
    *laPorts[5,1] = "COM4"
    *laPorts[6,1] = "COM5"
    *laPorts[7,1] = "COM6"
    laPorts[5,1] = "COM1"
    laPorts[6,1] = "COM2"
    laPorts[7,1] = "COM3"
    *B600897,1 Change this lines to use the correct Ports [End]

  *B600897,1 Add this lines to use more ports if the platform was Win95 [Begin]
  ENDIF    && End of IF Statment that was added to use more ports if the platform was Win95
  *B600897,1 Add this lines to use more ports if the platform was Win95 [End]
  
  *B600897,1 Add this lines to control the Enabling and disabling of the
  *          Port popup [Begin]
  laData[12] = IIF(!EMPTY(laData[12]) , laData[12] , 'LPT1' )
  puPort = laData[12]     && Restore the old Value of the popup
  SHOW GET puPort ENABLE
  lcPortSt = 'ENABLE'      && Varible to hold the popup status
  *B600897,1 Add this lines [End]

  *B600897,1 Remove this line [Begin]
  *DO SMLOCOP.SPR
  *B600897,1 Remove this line [End]

ENDIF

*!*******************************************************************
*!
*!      Function : lfwPrntNam
*!
*!*******************************************************************
*
FUNCTION lfwPrntNam

m.g_pdname = laData[1]  

*!*******************************************************************
*!
*!      Function : lfvPrinter
*!
*!*******************************************************************
*
FUNCTION lfvprinter

*E300683,4 Pass parameters to gpDoProg
*DO gpDoProg WITH 'awrSmPrtEs WITH ;
            "&laData[3]",2,"&laData[3]","&lcUsdFlTmp"','AWRSMPRINT'
DO gpDoProg WITH 'awrSmPrtEs', .F., 'SM', ;
            "'" + laData[3] + "',2,'" + laData[3] + "','&lcUsdFlTmp'"
*E300683,4 end       
     
*!*******************************************************************
*!
*!      Function : lfvAddPrnt
*!
*!*******************************************************************
*
FUNCTION lfvAddPrnt

DO CASE
  CASE pbPrint=1
    *E300683,4 Pass parameters to gpDoProg
    *DO gpDoProg WITH 'awrSmPrtEs WITH;
                "&laData[3]",4,"&laData[3]","&lcUsdFlTmp"','AWRSMPRINT'
    DO gpDoProg WITH 'awrSmPrtEs', .F., 'SM', ;
            "'" + laData[3] + "',4,'" + laData[3] + "','" + lcUsdFlTmp + "'"
    *E300683,4 end       
  CASE pbPrint=2
    *E300683,4 Pass parameters to gpDoProg
    *DO gpDoProg WITH 'awrSmPrtEs WITH;
                "&laData[3]",3,"&laData[3]","&lcUsdFlTmp"','AWRSMPRINT'
    DO gpDoProg WITH 'awrSmPrtEs', .F., 'SM', ;
            "'" + laData[3] + "',3,'" + laData[3] + "','" +;
            lcUsdFlTmp + "'"
    *E300683,4 end       
ENDCASE                    

*!*******************************************************************
*!
*!      Function : lfwWidth
*!
*!*******************************************************************
*
FUNCTION lfwWidth

laData[6] = VAL(ALLTRIM(SUBSTR(puWidth,1,3)))

*!*******************************************************************
*!
*!      Function : lfvWidth
*!
*!*******************************************************************
*
FUNCTION lfvWidth

laData[6] = VAL(ALLTRIM(SUBSTR(puWidth,1,3)))

*!*******************************************************************
*!
*!      Function : lfShowLan
*!
*!*******************************************************************
*
FUNCTION lfShowLan

laData[10] = IIF(TYPE('laData[10]')<>"L",.F.,laData[10])
cbNoBaner  = laData[10]
IF laData[10]
  SHOW GET laData[11] ENABLE 
ELSE
  SHOW GET laData[11] DISABLE   
ENDIF
SHOW GET cbNoBaner

*!*******************************************************************
*!
*!      Function : lfvNoBaner
*!
*!*******************************************************************
*
FUNCTION lfvNoBaner

laData[10] = cbNoBaner
IF cbNoBaner
  SHOW GET laData[11] ENABLE  
ELSE
  SHOW GET laData[11] DISABLE   
ENDIF

*!*******************************************************************
*!
*!      Function : lfShowLocal
*!
*!*******************************************************************
*
FUNCTION lfShowLocal

ls_Port = ASCAN(laPorts,laData[12])
SHOW GET ls_Port
  
*!*******************************************************************
*!
*!      Function : lfvPort
*!
*!*******************************************************************
*
FUNCTION lfvPort

laData[12] = laPorts[ls_Port]

*!*******************************************************************
*!
*!      Function : SetMemVar
*!
*!*******************************************************************
*
FUNCTION SetMemVar

m.g_action        = 5
g_pdfiles         = ""
g_pdchkbox        = 0
m.g_pddriver      = ""
m.g_pdorientation = 1
m.g_pdstyle       = 1
m.g_pdstroke      = 1
m.g_pdcpi         = 1
m.g_pdlpi         = 1
m.g_pdfontsize    = "10.0"
m.g_pdfont        = "Times"
m.g_pdtmargin     = 0				&& In Lines
m.g_pdlmargin     = 0				&& In Points
m.g_pdleading     = 0
m.g_pdname        = IIF(TYPE('laData[1]') $ "UL","",laData[1])
m.chg_flag        = .F.
m.g_pdpgsize      = 1
m.g_pdprocs       = 0
m.none            = .F.
g_pddriver        = IIF(TYPE('laData[3]') $ "UL","",laData[3])
lnprnt_No         = 0
*E300399,1 Hesham (Start)
m.g_pdAcomp = .T.
*E300399,1 Hesham (End)

*!*************************************************************
*! Name      : lfvPopPort                              
*! Developer : Haytham El_Sheltawi
*! Date      : 04/01/1997
*! Purpose   : Validation function of the puPort [For the popup]
*!*************************************************************
*! Calls     : gfActPop() , lfRef()
*!*************************************************************
*! Called From  : Screen SMPRINT.SCX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvPopPort()
*!*************************************************************
*B600897,1 This function was added by HS for the bug
*!*************************************************************
*
FUNCTION lfvPopPort

laData[12] = puPort

laData[12] = IIF(!EMPTY(laData[12]) , laData[12] , IIF(!EMPTY(lcOldPort) , lcOldPort , 'LPT1' ) ) 
puPort = laData[12]

*!*************************************************************
*! Name      : lfwPopPort                              
*! Developer : Haytham El_Sheltawi
*! Date      : 04/01/1997
*! Purpose   : WHIN function of the puPort [For the popup]
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Called From  : Screen SMPRINT.SCX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwPopPort()
*!*************************************************************
*B600897,1 This function was added by HS for the bug
*!*************************************************************
*
FUNCTION lfwPopPort

lcOldPort = laData[12]    && Varible to hold the old Port