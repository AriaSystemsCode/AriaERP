*:************************************************************************
*: Program file  : PSETUP.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*B600647,1 HESHAM EL_SHELTAWI 07/20/95
*B600647,1 CHANGE THE POPUP VARIABLE NAME IN THE PSETUP SCREEN TO lcPrnt_Port
*B600647,1 INSTED OF GCPRNT_PORT
*B800459,1 ORABY 03/18/1996 When outputing to the file, the user had to enter a path
*B800459,1                  Else he gets an error message. I solved that by sending the
*B800459,1                  out put to the root dir of the advantage.
*B601005,1 HESHAM 03/20/96 refresh the port after changing the printer
*B601005,1 driver setup
*B600823,1 Hesham El-Sheltawi 06/25/96
*B600823,1 save the setting of the resource functionalty
*B600823,1 because if it is off it gives an error in the genpd
*E300399,1 Hesham El_Sheltawi 10/07/96
*E300399,1 Add the Batch output device to the output devices
*E300617,1 Hesham El-Sheltawi 27/03/97
*E300617,1 Add array for the Ports to be used in the sel_dev
*E300617,1 screen under windows so in windows 95 we can give
*E300617,1 the user more ports
*E300719,1 Hesham 08/10/97
*E300719,1 Add the Batch process to the select device
*E300900,1  HS  07/21/98   Give the user the to choose the report mode.
*E300900,1                 Note: 
*E300900,1                       I made some changes in the screen
*E300900,1                       PSETUP1.SCX
*B603034,1  Hesham 07/18/1999
*B603034,1  Solve the General Protection error while printing under NT platform
*B603543,1 Hesham 4/3/2000
*B603543,1 Make the print ports from lpt1 .. lpt9 instead of lpt1..lpt3 in case
*B603543,1 of windows NT & 2000
*:************************************************************************
*
*E300399,1 Hesham El_Sheltawi (Start)
*PARAMETERS lcRprtNam
*E300719,1 Hesham (Start)
PARAMETERS llScreen,llBatch
*PARAMETERS llScreen

*E300900,1 Add these lines to get the report mode [Begin]
PRIVATE lcRepMode
lcRepMode = IIF(TYPE('lcOGPlatForm') = 'C' , lcOGPlatForm , '')

*B603034,1 Fix problems encountered when printing under Window NT
SET PRINTER OFF
*B603034,1 end
*E300900,1 Add these lines to get the report mode [Begin]
*B602772,1 Hesham (Start)
IF TYPE('lcOGPlatForm') = 'C'
  IF lcOGPlatForm = 'WINDOWS' AND gcDevice = 'FILE'
    gcDevice = 'PRINTER'
  ENDIF
ENDIF
*B602772,1 Hesham (End)
*E300719,1 Hesham (End)
*E300399,1 Hesham El_Sheltawi (End)
PRIVATE ALL LIKE l* EXCEPT lcOGPlatForm
*E300399,1 Hesham El_Sheltawi (Start)
llBatch = IIF(TYPE('llBatch')='L',llBatch,.F.)
llservUsed = .F.
lnCurArea = SELECT()
lcPriority = '9'
IF !USED('SYUBSERV')
  llservUsed = .T.
  USE (gcSysHome+'SYUBSERV') IN 0 
ENDIF

*E300399,1 Hesham El_Sheltawi (End)

DIMEN laDevice[1],lafoldwinds[3,2],laServer[1,2],;
      laOutput[1]
STORE '' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,lcFolder,lcBrTitle;
            ,lafoldwinds,laServer,laOutput,LCTO,LCCC,puoutput,PUSERVER
llBatch = IIF(TYPE('llBatch')#'U',llBatch,.T.)

lcfoldprnt  = 'PSETUP'          && window parent name for the folder
lcwfoldchng = '=lfActFolder()'  && function to control shows after change the folder

lnFolderCEnd= 67.500            && Folder window end column
lnfolderrend=   2.00            && Folder window end row


lcWinCh0 = gfTempName()
lcWinCh1 = gfTempName()
lcWinCh2 = gfTempName()
lcWinCh3 = gfTempName()  
lcFolder = gfTempName()


lafoldwinds[1,1] = 'Printer'
lafoldwinds[1,2] = lcWinCh1
lafoldwinds[2,1] = 'File'
lafoldwinds[2,2] = lcWinCh2
lafoldwinds[3,1] = 'Batch'
lafoldwinds[3,2] = lcWinCh3





llPrint    = .T.
llDisScr   = .F.
lcFileName = ''
*E300617,1 Hesham El-Sheltawi (Start)
*E300617,1 Add array for the Ports to be used in the sel_dev
*E300617,1 screen under windows so in windows 95 we can give
*E300617,1 the user more ports
DECLARE laPrtPorts[1]
IF _WINDOWS
  *B603543,1 Hesham (Start) 
  *IF RIGHT(OS(1),2)='95'
  *B603543,1 Hesham (Start) 
    DIMEN laPrtPorts[12]
    laPrtPorts[1] = 'LPT1'
    laPrtPorts[2] = 'LPT2'
    laPrtPorts[3] = 'LPT3'
    laPrtPorts[4] = 'LPT4'
    laPrtPorts[5] = 'LPT5'
    laPrtPorts[6] = 'LPT6'
    laPrtPorts[7] = 'LPT7'
    laPrtPorts[8] = 'LPT8'
    laPrtPorts[9] = 'LPT9'
    laPrtPorts[10] = 'COM1'
    laPrtPorts[11] = 'COM2'
    laPrtPorts[12] = 'COM3'
  *B603543,1 Hesham (Start)   
  *ELSE
  *  DIMEN laPrtPorts[6]
  *  laPrtPorts[1] = 'LPT1'
  *  laPrtPorts[2] = 'LPT2'
  *  laPrtPorts[3] = 'LPT3'
  *  laPrtPorts[4] = 'COM1'
  *  laPrtPorts[5] = 'COM2'
  *  laPrtPorts[6] = 'COM3'
  *ENDIF
  *B603543,1 Hesham (End)   
ENDIF
*E300617,1 Hesham El-Sheltawi (Start)
LISTWIND = gfTempName()
*** Check Whether the Screen Device Will Be Enable or not  
*E300719,1 Hesham (Start)
*IF PARAMETERS()>0
*  rboutput = IIF(gcDevice="FILE",3,1)  
*  llDisScr = .T.
*ELSE
*  rboutput = IIF(gcDevice="FILE",3,IIF(gcDevice="SCREEN",2,1))  
*ENDIF
  rboutput = ATC(LEFT(gcDevice,1),'PFB')
*ENDIF
*E300719,1 Hesham (End)
=lfGetServ()
llBatch = llBatch AND !EMPTY(laServer[1,1])

lnActFolder = MIN(IIF(llBatch,3,2),rboutput)
lnActFolder = MAX(1,lnActFolder)


*** Initialization for the Device Screen Variables
****** Just added in 11/09/94
glPrnt_lan=IIF(_WINDOWS,.F.,glPrnt_lan)
*****
rbLan        = IIF(glPrnt_lan,1,2)
llBaner      = glBaner
lcBaner_H    = gcBaner_H
lcServ_Nam   = gcServ_Nam
lcQueu_Nam   = gcQueu_Nam
lnPrnt_no    = IIF(TYPE('gnPrnt_No')<>"N",0,gnPrnt_No)
lcPrnt_Port  = gcPrnt_Port
**************
*B603543,1 Hesham START
lcWinPrint = ""
*B603543,1 Hesham END
rbWidth   = IIF(gnPrint_Wd=240,3,IIF(gnPrint_Wd=132,2,1))
IF TYPE('OGPlatForm')='C' AND (OGPlatForm='DOS' OR OGPlatForm='UNIX')
*  cbsetpd = !EMPTY(_PDSETUP)
  lcWinPrint = IIF(!EMPTY(_PDSETUP),_pdsetup,'None')
ELSE  
*B603543,1 Hesham START
  *KEYBOARD "{ENTER}"
  *lcWinPrint = prtwin(SYS(2015))
  *lcWinPrint = IIF(!EMPTY(lcWinPrint),SUBSTR(lcWinPrint,1,ATC(' on ',lcWinPrint)),lcWinPrint)
  *CLEAR TYPEAHEAD
*B603543,1 Hesham END  
*  cbsetpd = !EMPTY(lcWinPrint)  
ENDIF

lnNumCop  = _PCOPIES
lnStPage  = _PBPAGE
lnEndPage = _PEPAGE
cbBefore  = (_PEJECT $ "BEFORE,BOTH")
cbAfter   = (_PEJECT $ "AFTER,BOTH")
cbHeader  = glHeader
***********
IF rbOutPut  = 2
  lcFileName = gcOutFile
ENDIF


DO PSETUP.SPR

IF llservUsed AND USED('SYUBSERV')
  USE IN SYUBSERV
ENDIF
SELECT (lnCurArea)
*** Return .T. if User select the ok push button ,.F. otherwise

RETURN llPrint




*!*******************************************************************
*!                     Function lfvGetFile 
*!           Show  procedure of PSetup Screen
*!*******************************************************************  
*
FUNCTION lfvGetFile

lcName = GETFILE("*.*","Select file for output","Select",0) && Save the Curr.
                                                              &&  OutPut File Name 		
lcFileName = IIF(EMPTY(lcName),lcFileName,lcName)           && Get New File Name

SHOW GETS

*!*******************************************************************
*!                     Function lvWidth 
*!           Valid Function For the Width Radio Button
*!*******************************************************************  
*
FUNCTION lfvWidth


*!*******************************************************************
*!                     Function lvSetup 
*!           Valid Function For Printer Driver Check Box
*!*******************************************************************  
*
FUNCTION lfvSetup    
*B600823,1 Hesham El-Sheltawi (Start)
*B600823,1 save the setting of the resource functionalty
*B600823,1 because if it is off it gives an error in the genpd
lcResouce = SET('RESOU')
SET RESOU ON
*B600823,1 Hesham El-Sheltawi (End)
IF _WINDOWS
  IF TYPE('OGPlatForm')='C' AND (OGPlatForm='DOS' OR OGPlatForm='UNIX')
    lcTPrint = _PDSETUP
    _PDSETUP = "?"
    IF ALLTRIM(lcTPrint)<>ALLTRIM(_PDSETUP)
      rbWidth    = IIF(gnPrint_Wd=240,3,IIF(gnPrint_Wd=132,2,1))
      lcServ_Nam = gcServ_Nam
      lcQueu_Nam = gcQueu_Nam
  *    lnPrint_No = gnPrint_No
    ENDIF
    *B601005,1 Hesham (Start)
    *B601005,1 refresh the selected output port after changing the printer
    *B601005,1 driver setting
    lcPrnt_Port = gcPrnt_Port
    *B601005,1 (END)    
*    cbsetpd = !EMPTY(_PDSETUP)  
     lcWinPrint = IIF(!EMPTY(_PDSETUP),_pdsetup,'None')
    =lfRefresh()
*    SHOW GET cbsetpd,1 PROMPT IIF(cbsetpd,_PDSETUP+'...','Printer \<Driver...')
    SHOW GETS
  ELSE
*    =gfWinFn('PRINTERS')
*B603543,1 Hesham START
    *=prtwin()
    *KEYBOARD "{ENTER}"
    *lcWinPrint = prtwin(SYS(2015))
    *lcWinPrint = IIF(!EMPTY(lcWinPrint),SUBSTR(lcWinPrint,1,ATC(' on ',lcWinPrint)),lcWinPrint)    
    *CLEAR TYPEAHEAD
    =SYS(1037)
*B603543,1 Hesham END    
*    cbsetpd = !EMPTY(lcWinPrint)  
    =lfRefresh()
*    SHOW GET cbsetpd,1 PROMPT IIF(cbsetpd,lcWinPrint+'...','Printer \<Driver...')
   * =SYS(1037)  
  ENDIF
ELSE
  lcTPrint = _PDSETUP
  _PDSETUP = "?"
  IF ALLTRIM(lcTPrint)<>ALLTRIM(_PDSETUP)
    rbWidth    = IIF(gnPrint_Wd=240,3,IIF(gnPrint_Wd=132,2,1))
    lcServ_Nam = gcServ_Nam
    lcQueu_Nam = gcQueu_Nam
*    lnPrint_No = gnPrint_No
  ENDIF
*  cbsetpd = !EMPTY(_PDSETUP)  
  =lfRefresh()
*  SHOW GET cbsetpd,1 PROMPT IIF(cbsetpd,_PDSETUP+'...','Printer \<Driver...')
  SHOW GETS
ENDIF  
*B600823,1 Hesham El-Sheltawi (Start)
*B600823,1 restore the setting of the resource functionalty
SET RESOU &lcResouce
*B600823,1 Hesham El-Sheltawi (End)

*!*******************************************************************
*!                     Function lfvNumCop
*!
*!*******************************************************************  
*
FUNCTION lfvNumCop



*!*******************************************************************
*!                     Function lfvOk
*!           Valid Function For OK push button
*!*******************************************************************  
*

FUNCTION lfvOk

DO CASE
*E300719,1 Hesham (Start)
  CASE lnActFolder = 3              && Case Output device = "Batch"
    gcDevice = 'BATCH'
    lnSrvNo = ASCAN(laServer,puServer)
    IF lnSrvNo>0
      lcServerID = laServer[ASUB(laServer,lnSrvNo,1),2]
      SELECT SYUBSRVJ
      APPEND BLANK
      REPLACE CPSERVERID WITH lcServerID,;
              CUSER_ID   WITH gcUser_ID,;
              cComp_id   WITH gcAct_Comp,;
              cApp_ID    WITH gcAct_Appl,;
              MOUTPUT    WITH PUOUTPUT,;
              cpriority  WITH lcPriority
     =gfAdd_info()         
    ENDIF  
    
*E300719,1 Hesham (End)    
  CASE lnActFolder = 2              && Case Output device = "FILE"
    IF !EMPTY(lcFileName)        
      IF FILE(lcFileName)        && check if the file name is exist 
        lnOption = gfModalGen("QRM00048B00010","DIALOGE")  && Display warrning msg.
        DO CASE
          CASE lnOption = 1      && user select OverWrite the file option
            gcOutFile  = lcFileName
            gcDevice   = "FILE"
            lcPrintSet = gcOutFile
          CASE lnOption = 2       && user select Append to the file option
            gcOutFile=lcFileName+" ADDITIVE"
            gcDevice="FILE"
            lcPrintSet=gcOutFile
          OTHERWISE    && user Cancel
             RETURN  
        ENDCASE
      ELSE              &&  if file does not exist
        IF OCCURS("\",lcFileName)>0   
          IF !gfValdPath(SUBSTR(lcFileName,1,RAT("\",lcFileName)-1)) && Check if the file have a valid path
            =gfModalGen("INM00049B00000","DIALOGE")
            RETURN
          ELSE
            gcOutFile=lcFileName        &&  the file has a valid path
            gcDevice="FILE"
            lcPrintSet = gcOutFile            
          ENDIF
        *B800459,1 (Start) Add the following lines
        ELSE && Case the file name is without a path
          gcOutFile=lcFileName        &&  the file has a valid path
          gcDevice="FILE"
          lcPrintSet = gcOutFile            
        ENDIF
        *B800459,1 (End)        
      ENDIF  
    ELSE
      =gfModalGen("INM00047B00000","DIALOGE")  
      RETURN
    ENDIF  
  CASE lnActFolder = 1        && Case Output device = "Printer"
    gcOutFile    = ""
    gcDevice     = "PRINTER" 
    gnPrint_Wd  = IIF(rbWidth=3,240,IIF(rbWidth=2,132,80))
    lcPrintset  = ""
ENDCASE   


_PCOPIES     = lnNumCop
_PBPAGE      = lnStPage
_PEPAGE      = lnEndPage
_PEJECT      = IIF(cbBefore AND cbAfter,"BOTH",;
               IIF(cbBefore,"BEFORE",IIF(cbAfter,"AFTER","NONE"))) 
_WRAP        = .T.

gcPrntr_ID  = _PDSETUP     
glHeader    = cbHeader
glBaner     = llBaner
glPrnt_Lan  = (rbLan=1)
gcBaner_H   = lcBaner_H
gcServ_Nam  = lcServ_Nam
gcQueu_Nam  =  lcQueu_Nam
gnPrnt_no   = lnPrnt_No
*gcPrnt_Port = lcPrnt_Port

CLEAR READ     



FUNCTION lfvCancel
  llPrint=.F.
RETURN



FUNCTION lfActFolder
DO CASE 
  CASE lnActFolder = 2
    IF EMPTY(gcOutFile)
      lcFileName = gcWorkDir+'Report.TXT'
    ENDIF
    SHOW GETS WINDOW (lcWinCh2) ENABLE ONLY
  CASE lnActFolder = 3
    SHOW GETS WINDOW (lcWinCh3) ENABLE ONLY
  CASE lnActFolder = 1
    SHOW GETS WINDOW (lcWinCh1) ENABLE ONLY
ENDCASE
*B602772,1 Hesham (Start)
IF lcRepMode = 'WINDOWS'
  SHOW GET ibFolder[2] DISABLE
ENDIF
*B602772,1 Hesham (End)

SHOW GET pbOk     ENABLE
SHOW GET pbCancel ENABLE     
IF !llBatch
  SHOW GET ibfolder[3] DISABLE
ENDIF



FUNCTION lfRDeact
IF !WVISIBLE('PSETUP')
  CLEAR READ
  RETURN .T.
ENDIF
RETURN .F.



FUNCTION lfGetServ
PRIVATE lnRecNo,lnRePro,lcMultiLocks,lcExtended,lnCurArea
DIMENSION laServer[1,2]
STORE '' TO laServer
SELECT SYUBSERV
SET ORDER TO TAG SERVERID
lnRePro = SET('REPRO')
SET REPROCESS TO 5
GO TOP
SCAN
  IF !EMPTY(laServer[1,1])
    DIMENSION laServer[ALEN(laServer,1)+1,2]  
  ENDIF  
  lcExtended = ''
  IF RLOCK()
      UNLOCK
      lcExtended = ' Idle Server'
  ENDIF  
  laServer[ALEN(laServer,1),1] = ALLT(cpServerId)+' '+ALLT(cserverdes)+lcExtended
  laServer[ALEN(laServer,1),2] = ALLTR(cpServerId)
ENDSCAN
SET REPROCESS TO  lnRePro



FUNCTION lfvServerID
lnSrvNo = ASCAN(laServer,puServer)
IF lnSrvNo>0
  lcServerID = laServer[ASUB(laServer,lnSrvNo,1),2]
  DIMENSION laoutput[1]
  STORE '' TO laOutPut
  SELECT SYUBSERV
  =SEEK(lcServerID)
  =gfSubStr(ALLTRIM(MOUTPUT),@laOutput,CHR(27))
  puoutput = laOutput[1]
  SHOW GET puserver
  SHOW GET puoutput
ENDIF  
