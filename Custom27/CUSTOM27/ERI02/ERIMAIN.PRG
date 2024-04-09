*!**************************************************************************
*! Name      : ERIMAIN.PRG
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 11/11/2003
*! Purpose   : Custom main program for ERIC JAVITS, INC              
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C120205,1
*!**************************************************************************
*! Modifications
*!***************************************************************************
*!C120205,3 TMI 01/18/2004 Add style description and color description to the generated text file.
*!C122981,1 NNA 08/26/2004 Blank the color cost when we add a new color to a Style 
*!B130339,1 TMI 12/21/2005 Do not update Icost fields by the cost entered in style screen if all colors selected
*:***************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*:**************************************************************************
*:* Name        : lfSENDMAIL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/11/2003
*:* Purpose     : Call VB exe to send mail to notify users of Aria with new 
*:*             : added styles
*:***************************************************************************
*:* Called from : ICSTYLE.PRG
*:***************************************************************************
FUNCTION lfSENDMAIL

*- Create a temp file to store the contents of sent mail

lcTempFile = gcWorkDir + ALLTRIM(PADR('X'+lcfolder,8))  && Use the value of a temp variable , to not be forced to 
									   && define a new variable in the calling statndard program
lnHndl = FCREATE(lcTempFile)

LOCATE FOR cStatus = 'A'
IF FOUND()
  IF !SEEK(SUBSTR(STYLE,1,lnstylewid),'STYLE')
    *120205,3   TMI [Start] Add style description
    *=FPUTS(lnHndl,"NEW STYLE # " + SUBSTR(STYLE,1,lnstylewid) + " ADDED")
    lcAddStr = 'NEW STYLE # ' + ALLTRIM(SUBSTR(STYLE,1,lnstylewid)) + ;
                              + IIF(!EMPTY(Desc),' ('+ALLTRIM(Desc)+')','')+' is Added'
    =FPUTS(lnHndl,lcAddStr)
    *120205,3   TMI [End  ] 
  ENDIF  
  SCAN REST FOR cstatus = 'A'
    *120205,3   TMI [Start] Add color description
    *=FPUTS(lnHndl,"ADDDED COLOR " + SUBSTR(STYLE,lnstylewid+2) + " TO STYLE # " + SUBSTR(STYLE,1,lnstylewid))
    lcAddStr = 'ADDDED COLOR ' + ALLTRIM(SUBSTR(STYLE,lnstylewid+2)) +' ('+ALLTRIM(gfCodDes(SUBSTR(STYLE,lnstylewid+2),'COLOR'))+;
               ') TO STYLE # ' + ALLTRIM(SUBSTR(STYLE,1,lnstylewid)) +IIF(!EMPTY(Desc),' ('+ALLTRIM(Desc)+')','')
    =FPUTS(lnHndl,'')  && add new empty line
    =FPUTS(lnHndl,lcAddStr)
    *120205,3   TMI [End  ] 
    =FPUTS(lnHndl,'PRICE of STYLE/COLOR # "' + STYLE + '" IS ' + LTRIM(STR(PRICEA,10,2)))
  ENDSCAN
ELSE
  RETURN
ENDIF  
GO TOP
=FCLOSE(lnHndl)

lcCommLine = lcTempFile

*- Call the VB exe that send mails
lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  *--Now run the VFP exe program that runs the report
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
    ",[SENDMAIL.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  llPoDisp = .F.
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
ENDIF

*-- end of lfSENDMAIL.
*C122981,1 NNA
*:**************************************************************************
*:* Name        : lfClrCost
*:* Developer   : NADER NABIL (NNA)
*:* Date        : 08/26/2004
*:* Purpose     : Blank the Mcost,Nicost,Total Cost,Avg_Cost when we add a new
*:*             : Color for a Style
*:***************************************************************************
*:* Called from : ICSTYLE.PRG
*:***************************************************************************
*:C122981,1
FUNCTION lfClrCost
PRIVATE lcI
STORE '' TO lcI
SELECT STYLE
FOR I = 1 TO 5
  lcI = STR(I,1)
  REPLACE NMCOST&lcI WITH 0 , NICOST&lcI WITH 0
ENDFOR
REPLACE TOTCOST WITH 0 , AVE_COST WITH 0 , GROS_PRICE WITH 0
RETURN

*--END OF FUNCTION lfClrCost

*:**************************************************************************
*:* Name        : lfDNYUPCLR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/21/2005
*:* Purpose     : Do not update the nICost1 , ... , nICost5 by the cost 
*:*             : entered in style screen if all colors selected
*:***************************************************************************
*:* Called from : lpSavScr in ICSTYLE.PRG
*:***************************************************************************
*B130339,1
FUNCTION lfDNYUPCLR

  GATHER FIELDS nICost1,nICost2,nICost3,nICost4,nICost5,GROS_PRICE FROM laOldICost

*-- end of lfDNYUPCLR.