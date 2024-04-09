*E302789,1 MMT 10/28/2010 modify invoice form to call custom forms from RB[T20100226.0004]
*B609470,2 MMT 12/05/2010 Custom Invoice Forms does not work from the request builder[T20100226.0004]
*E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE []
*E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media]
*B610504,1 SAB 09/08/2013 Use Create Remote Object instead of Create Object [T20130826.0003]
*B610559,1 SAB 10/24/2013 Merge Modify AriaMain program and class to add  gfAddCurSm function [T20130904.0003]
*E303437,1 SAB 12/24/2013 Modify Aria RB Troubleshooting tool to run with R13[Troublshooting R13]
*B612314,1 MMT 07/05/2021 Add missing function from Aria5 globals
*B612455,1 MMT 09/04/2023 Get setting from RB
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*LPARAMETERS lcActiveCompany
*BADRAN - A
*LPARAMETERS lcActiveCompany,ClientId
LPARAMETERS lcActiveCompany, ClientId, lcCurrentProcedure, loEnvironment
*BADRAN - Z
*T20100512.0026 Hassan 2010 05 23 [END]

*E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE [Start]
IF TYPE('lcActiveCompany') = 'C' .AND. 'TEMP.TXT' $ UPPER(lcActiveCompany)  
  *! E303437,1 SAB 12/24/2013 Modify Aria RB Troubleshooting tool to run with R13[Troublshooting R13][Start]
  *STRTOFILE("2.0.0.1", lcActiveCompany, .F.)
  STRTOFILE("3.0.0.0", lcActiveCompany, .F.)
  *! E303437,1 SAB 12/24/2013 Modify Aria RB Troubleshooting tool to run with R13[Troublshooting R13][End]
  RETURN
ENDIF
*E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE [End]

ON ERROR DO =errHandler(ERROR( ), MESSAGE( ), MESSAGE(1), PROGRAM( ), LINENO( ))

*BADRAN - A
*!*	LOCAL loEnvironment
*!*	loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
*!*	*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*!*	loEnvironment.ClientID = ClientId
*!*	*loEnvironment.ConnectionsRefresh() 											Commented by Saber 04-28-2011

*!*	*T20100512.0026 Hassan 2010 05 23 [END]

*!*	LOCAL lcCurrentProcedure
*!*	*lcCurrentProcedure = UPPER(SYS(16))       
*!*	*lcCurrentProcedure = STRTRAN(lcCurrentProcedure, "\SRVPRGS\SY\ariamain.fxp", "", -1, 1, 1)
*!*	*lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
*!*	*lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
*!*	  *lcCurrentProcedure = ADDBS(UPPER(loEnvironment.Aria40SystemFilesPath))		Commented by Saber 04-28-2011
*!*	  lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDICTIONARY\"), "", -1, 1, 1)
*!*	loEnvironment.ConnectionsRefresh()												&&Added by Saber 04-28-2011  
IF (VARTYPE(lcCurrentProcedure) <> "C") OR EMPTY(lcCurrentProcedure)
  *BADRAN - A LOCAL loEnvironment
  *B610504,1 SAB 09/08/2013 Use Create Remote Object instead of Create Object [T20130826.0003] [Start]
  **E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  *loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
  **loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
  **E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]
  loEnvironment = goRemoteCall.GetRemoteObject("Aria.Environment.AriaEnviromentVariables")
  *B610504,1 SAB 09/08/2013 Use Create Remote Object instead of Create Object [T20130826.0003] [End]
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  loEnvironment.ClientID = ClientId
  *loEnvironment.ConnectionsRefresh() 											Commented by Saber 04-28-2011
  *T20100512.0026 Hassan 2010 05 23 [END]
  *BADRAN - A LOCAL lcCurrentProcedure
  *lcCurrentProcedure = UPPER(SYS(16))
  *lcCurrentProcedure = STRTRAN(lcCurrentProcedure, "\SRVPRGS\SY\ariamain.fxp", "", -1, 1, 1)
  *lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
  *lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
  *lcCurrentProcedure = ADDBS(UPPER(loEnvironment.Aria40SystemFilesPath))		Commented by Saber 04-28-2011
  
  *!* T20110801.0008 MAH 8/2/2011
  *-- lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDICTIONARY\"), "", -1, 1, 1)
  lcCurrentProcedure = loEnvironment.Aria40SharedPath
  *!* T20110801.0008 MAH 8/2/2011 End

  loEnvironment.ConnectionsRefresh()												&&Added by Saber 04-28-2011  
ENDIF
*BADRAN - Z

SET PROCEDURE TO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") ADDITIVE
SET PROCEDURE TO (lcCurrentProcedure + "SRVPRGS\SY\genhtml.fxp") ADDITIVE
SET PROCEDURE TO (lcCurrentProcedure + "SRVPRGS\SY\runcode.fxp") ADDITIVE
SET CLASSLIB TO (lcCurrentProcedure + "SRVCLSS\SY\ariamain.vcx") ADDITIVE
SET CLASSLIB TO (lcCurrentProcedure + "SRVCLSS\SY\_html.vcx") ADDITIVE
* MAHMAH
SET PROCEDURE TO (lcCurrentProcedure + "PRGS\SY\xfrx.fxp") ADDITIVE
SET CLASSLIB TO (lcCurrentProcedure + "Classes\_reportlistener.vcx") ADDITIVE

* MAHMAH


PUBLIC oAriaEnvironment, oAriaApplication, gdSysDate, gcCom_Name, loogScroll


PUBLIC gcRepHome,gcWorkDir,gcSysHome ,gcDataDir,gcAct_Comp, GCUSER_ID

*BADRAN - A  
*oAriaEnvironment = CREATEOBJECT('ariaenvironment', lcActiveCompany,ClientId)
oAriaEnvironment = CREATEOBJECT('ariaenvironment', lcActiveCompany,ClientId,lcCurrentProcedure, loEnvironment)
*BADRAN - Z
oAriaApplication = oAriaEnvironment

loogScroll = oAriaEnvironment.Report

SET PATH TO (lcCurrentProcedure) ADDITIVE

gdSysDate = oAriaEnvironment.systemdate
gcCom_Name = oAriaEnvironment.activecompanyname

*MMT
gcRepHome  = oAriaEnvironment.ReportHome 
gcWorkDir  = oAriaEnvironment.WorkDir
gcSysHome  = oAriaEnvironment.systemfilespath 
gcDataDir  = oAriaEnvironment.DataDir  
gcAct_Comp = oAriaEnvironment.activecompanyID
*MMT

*! B609470,2 MMT 12/05/2010 Custom Invoice Forms does not work from the request builder[start]
PUBLIC gcContCode 
gcContCode = oAriaEnvironment.DefaultCountry 
*! B609470,2 MMT 12/05/2010 Custom Invoice Forms does not work from the request builder[End]

*!*************************************************************
*! Name      : gfPhoneTem
*! Developer : Hesham Shereef
*! Date      : 1993-1995 
*! Purpose   : Fix the phone template
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*  FUNCTION RETURN THE PHONE TEMPLATE FORMAT "PICTURE"
*:->
FUNCTION gfPhoneTem

*B132026,1 WSH 05/16/2006 Add parameter to allow getting Phone Template for any Country. [Start]
*PARAMETER lcPhoneTem
LPARAMETER lcPhoneTem, lcCont_ID
*B132026,1 WSH 05/16/2006 [End]

*IF PARAMETERS()>0
 * RETURN "@R "+STRTRAN(lcPhoneTem,'#','X')
*ELSE
 * RETURN "@R "+STRTRAN(oAriaEnvironment.PhoneMask,'#','X')
*ENDIF  

*B132026,1 WSH 05/16/2006 Add parameter to allow getting Phone Template for any Country. [Start]
*IF PARAMETERS() = 0
*  *If the function get called at design time, the check on oAriaEnvironment will prevent errors
*  *while opening a form 
*  lcPhoneTem = IIF(TYPE("oAriaEnvironment.PhoneMask")='U','',oAriaEnvironment.PhoneMask)
*ENDIF
IF PARAMETERS() = 0 OR TYPE("lcPhoneTem") # 'C' OR EMPTY(lcPhoneTem)
  lcPhoneTem = IIF(TYPE("oAriaEnvironment.PhoneMask") = 'U', '', oAriaEnvironment.PhoneMask)
ENDIF

IF TYPE("lcCont_ID") = "C" AND !EMPTY(lcCont_ID) AND lcCont_ID # PADR(oAriaEnvironment.DefaultCountry,6)
  LOCAL lnRemResult, lnAlias
  lnAlias     = SELECT(0)
  lnRemResult = oAriaEnvironment.RemoteSystemData.Execute("SELECT * FROM SYCINT WHERE cCont_Code = '" + lcCont_ID + "'",;
                        "", "TMPSYCINT", "", oAriaEnvironment.SystemConnectionString, 3, "", SET("Datasession"))
  
  IF lnRemResult >= 1
    SELECT TMPSYCINT
    LOCATE
    lcPhoneTem = IIF(FOUND() AND !EMPTY(cPhoneTemp), ALLTRIM(cPhoneTemp), lcPhoneTem)
    USE IN TMPSYCINT
  ENDIF
  SELECT (lnAlias)
ENDIF
*B132026,1 WSH 05/16/2006 [End]

RETURN STRTRAN(lcPhoneTem,'#','X')


*!**************************************************************************
*!
*!      Function:  gfGetAdr
*!
*!**************************************************************************
*  Gets address according to the address code, returns address
*
FUNCTION gfGetAdr
PARAMETERS lcAlias, lcTag, lcKeyCode, lcAdrCode,lnLineNo,lcAddGrp,lcCurrCode

*** lcAlias   : source file name 
*** lcTag     : source file tag that is to be used in seeking
*** lckeycode : search key code (of the source file) (optional)
*** lcAdrCode : address code (optional)
*** lnLineNo  : The Address line number to return

PRIVATE lnSavIntTg, lnSavCmpTg, lcCurAlias, lnOldTag,;
        llOpenInt, llOpenCmp, llContinue, lnCompRec,lcCurrCode,lnCounter

lnCounter = 0 
 * You have to send the source file and 1 or more from the following parameters
 * 1 - The alias name for the source file or you have it the currently selected
 * 2 - Address code to be used in getting the address line  OR
 * 3 - Tag name and Seek Expression to get the  Address code
 * 4 - You can have the source file opened with the proper tag and just send
 *     the seek expr. (In case of not sending Tag ID there must be an active one)      
 
 IF EMPTY(lcAlias) .OR. TYPE('lcTag') <> 'C'
   IF EMPTY(ALIAS())
     RETURN .F.
   ELSE   
     lcAlias = ALIAS()
   ENDIF  
 ENDIF
 IF EMPTY(lcAddGrp) OR TYPE('lcAddGrp') <> 'C'
   lcAddGrp  = ''
   lcGrpCode = 'E'
 ELSE  
   lcAddGrp  = ALLTRIM(lcAddGrp)
   lcGrpCode = lcAddGrp
 ENDIF   
 lcCurAlias = ALIAS()
 SELECT (lcAlias)
 lnOldTag = VAL(SYS(21))   

 *-- No Address code has been sent
 IF EMPTY(lcAdrCode) 
   IF !EMPTY(lcKeyCode) .AND. TYPE('lcKeyCode') <> 'C'
     DO CASE

       *-- A Search Expr has been sent and no Tag Has been Sent and no active tag
       CASE (EMPTY(lcTag) .OR. TYPE('lcTag') <> 'C') AND EMPTY(SYS(22))
         SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
         RETURN .F.

       *-- A Search Expr and a Tag ID have been sent 
       CASE !EMPTY(lcTag)
         *lnOldTag = VAL(SYS(21))   
         SET ORDER TO TAG (lcTag)
         *-- The Search expr is not found
         IF !SEEK(lcKeyCode)
           SET ORDER TO lnOldTag
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
           RETURN .F.
         ENDIF
 
       *-- A search expr has been sent without a Tag 
       OTHERWISE 
         *-- There is no active tag
         IF EMPTY(SYS(22)) 
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
           RETURN .F.
         ENDIF
         *-- The Search Expr. is not found
         IF !SEEK(lcKeyCode)
           SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  
           RETURN .F.
         ENDIF
     ENDCASE 
   ENDIF  

   *-- Just to be able to set the old tag even if it has not been 
   *-- changed in the above DO CASE
   *lnOldTag = VAL(SYS(21))   

   lcAdrCode = cCont_Cod&lcGrpCode
 ENDIF

DECLARE laAddress[6,3]
laAddress = " "
*E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [begin]
*lnLineNo  = IIF(TYPE('lnLineNo')='N' AND BETWEEN(lnLineNo,1,5),INT(lnLineNo),1)
llRetArray = (TYPE("lnLineNo[1]") = "C")
IF llRetArray
  lnLineNo = ""
ELSE
  lnLineNo  = IIF(TYPE('lnLineNo')='N' AND BETWEEN(lnLineNo,1,5),INT(lnLineNo),1)
ENDIF
*E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]

*-- Hesham (Start)
*-- select data from syccurr remotely and procced the addcurrency
*-- normaly

*!*	STORE .F. TO llOpenInt, llOpenCmp
*!*	*** Check being on a correct alias
*!*	   
*!*	IF !USED('SYCINT')  && Check if the internationals file is open or not.
*!*	  llOpenInt  = .T.     && Indicates that the file is open by the function.
*!*	  ** Use the file and assign the index.
*!*	  USE (oAriaEnvironment.SysPath + 'SYCINT') ORDER TAG cContCode IN 0 
*!*	ELSE
*!*	  SELECT SYCINT       
*!*	  lnSavIntTg = VAL(SYS(21))
*!*	  SET ORDER TO TAG cContCode   && Change the order
*!*	ENDIF  

*!*	IF !USED('SYCCOMP')  && Check if the internationals file is open or not.
*!*	  llOpenCmp  = .T.     && Indicates that the file is open by the function.
*!*	  ** Use the file and assign the index.
*!*	  USE (oAriaEnvironment.SysPath + 'SYCCOMP') ORDER TAG cComp_ID IN 0 
*!*	ELSE
*!*	  SELECT SYCCOMP       
*!*	  lnSavCmpTg = VAL(SYS(21))
*!*	  lnCompRec  = RECNO()
*!*	  SET ORDER TO TAG cComp_ID   && Change the order
*!*	ENDIF  

*!*	IF SEEK(lcAdrCode,'SYCINT') .OR. (SEEK(oAriaEnvironment.ActiveCompanyID,'SYCCOMP') ;
*!*	   .AND. SEEK(SYCCOMP.cCont_Code,'SYCINT'))
*!*	  laAddress[1,1] = SYCINT.nPart1Ord
*!*	  laAddress[1,2] = EVAL(lcAlias+'.cAddress1'+lcAddGrp)
*!*	  laAddress[1,3] = SYCINT.nPart1LEN
*!*	  laAddress[2,1] = SYCINT.nPart2Ord
*!*	  laAddress[2,2] = EVAL(lcAlias+'.cAddress2'+lcAddGrp) 
*!*	  laAddress[2,3] = SYCINT.nPart2LEN
*!*	  laAddress[3,1] = SYCINT.nPart3Ord
*!*	  laAddress[3,2] = EVAL(lcAlias+'.cAddress3'+lcAddGrp)
*!*	  laAddress[3,3] = SYCINT.nPart3LEN      
*!*	  laAddress[4,1] = SYCINT.nPart4Ord
*!*	  laAddress[4,2] = EVAL(lcAlias+'.cAddress4'+lcAddGrp)
*!*	  laAddress[4,3] = SYCINT.nPart4LEN      
*!*	  laAddress[5,1] = SYCINT.nPart5Ord
*!*	  laAddress[5,2] = EVAL(lcAlias+'.cAddress5'+lcAddGrp)
*!*	  laAddress[5,3] = SYCINT.nPart5LEN      
*!*	  laAddress[6,1] = SYCINT.nPart6Ord
*!*	  laAddress[6,2] = EVAL(lcAlias+'.cAddress6'+lcAddGrp)
*!*	  laAddress[6,3] = SYCINT.nPart6LEN
*!*	  IF TYPE("lcCurrCode") = 'C'
*!*	    &lcCurrCode = SYCINT.cCurrCode
*!*	    *SHOW GET (lcCurrCode)
*!*	  ENDIF  
*!*	  =ASORT(laAddress,1)
*!*	  lcRetVal=''
*!*	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [begin]
*!*	  *FOR lnCount = 1 TO ALEN(laAddress,1)
*!*	  *  IF laAddress[lnCount,1] = lnLineNo
*!*	  *    *lcRetVal=lcRetVal+IIF(EMPTY(lcRetVal),'',',')+PADR(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]),laAddress[lnCount,3])
*!*	  *    lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
*!*	  *    lcRetVal  = lcRetVal+IIF(EMPTY(lcRetVal) .OR. RIGHT(lcRetVal,1) = ',' ,'',', ') + lcAddPart
*!*	  *   ENDIF
*!*	  *ENDFOR
*!*	  *-- if it is array
*!*	  IF llRetArray
*!*	    PRIVATE lnArrLen
*!*	    lnArrLen = 0
*!*	    FOR lnArrLen = 1 TO ALEN(lnLineNo,1)
*!*	      FOR lnCount = 1 TO ALEN(laAddress,1)
*!*	        IF laAddress[lnCount,1] = lnArrLen
*!*	          lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
*!*	          lnLineNo[lnArrLen] = lnLineNo[lnArrLen]+;
*!*	                               IIF(EMPTY(lnLineNo[lnArrLen]) .OR. ;
*!*	                               RIGHT(lnLineNo[lnArrLen],1) = ',' ,'',', ') + lcAddPart
*!*	         ENDIF
*!*	      ENDFOR
*!*	    ENDFOR
*!*	  ELSE  && else numeric value.

*!*	    FOR lnCount = 1 TO ALEN(laAddress,1)
*!*	      IF laAddress[lnCount,1] = lnLineNo
*!*	        lnCounter = lnCounter + 1
*!*	        lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
*!*	        lcRetVal  = lcRetVal+IIF(EMPTY(lcRetVal) .OR. RIGHT(lcRetVal,1) = ',' ,'',;
*!*	                             IIF(ALLTRIM(gcContCode) = "USA" AND lnCounter > 2 ,'  ' ,', ')) + lcAddPart
*!*	       ENDIF
*!*	    ENDFOR
*!*	  ENDIF
*!*	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]
*!*	  
*!*	ELSE
*!*	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [begin]
*!*	  *lcRetVal= EVAL(lcAlias+'.cAddress'+STR(lnLineNo,1)+lcAddGrp)
*!*	  *-- if it is an array
*!*	  IF llRetArray
*!*	    PRIVATE lnArrLen
*!*	    lnArrLen = 0
*!*	    FOR lnArrLen = 1 TO ALEN(lnLineNo,1)
*!*	      lnLineNo[lnArrLen]= EVAL(lcAlias+'.cAddress'+STR(lnArrLen,1)+lcAddGrp)
*!*	    ENDFOR
*!*	  ELSE  && else it is numeric value.
*!*	    lcRetVal= EVAL(lcAlias+'.cAddress'+STR(lnLineNo,1)+lcAddGrp)
*!*	  ENDIF  
*!*	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]
*!*	ENDIF  

*!*	IF USED('SYCCOMP')
*!*	  IF llOpenCmp 
*!*	    USE IN SYCCOMP
*!*	  ELSE
*!*	    SET ORDER TO lnSavCmpTg IN SYCCOMP  
*!*	    IF BETWEEN(lnCompRec,1,RECCOUNT("SYCCOMP"))
*!*	      GOTO lnCompRec IN SYCCOMP
*!*	    ENDIF  
*!*	  ENDIF
*!*	ENDIF
*!*	  
*!*	IF USED('SYCINT')
*!*	  IF llOpenInt
*!*	    USE IN SYCINT
*!*	  ELSE
*!*	    SET ORDER TO lnSavIntTg IN SYCINT
*!*	  ENDIF  
*!*	ENDIF
lnRemResult = oAriaEnvironment.remotesystemdata.execute("Select * from SYCINT where ccont_code='"+lcAdrCode+"'",'',"SYCINTTMP","",oAriaEnvironment.SystemConnectionString,3,"",SET("DATAS"))
IF lnRemResult=1
    LOCATE
	IF !FOUND()  
  	  lnRemResult = oAriaEnvironment.remotesystemdata.execute("Select * from SYCINT where ccont_code='"+oAriaEnvironment.DefaultCountry+"'",'',"SYCINTTMP","",oAriaEnvironment.SystemConnectionString,3,"",SET("DATAS"))
  	  *N000300,1 HBG 05/17/2004 Locate to fix bug of EOF()
  	  LOCATE
  	  *N000300,1 [End]
	ENDIF
	IF lnRemResult>=1 AND FOUND()
	  laAddress[1,1] = SYCINTTMP.nPart1Ord
  	laAddress[1,2] = EVAL(lcAlias+'.cAddress1'+lcAddGrp)
	  laAddress[1,3] = SYCINTTMP.nPart1LEN
  	laAddress[2,1] = SYCINTTMP.nPart2Ord
	  laAddress[2,2] = EVAL(lcAlias+'.cAddress2'+lcAddGrp) 
  	laAddress[2,3] = SYCINTTMP.nPart2LEN
	  laAddress[3,1] = SYCINTTMP.nPart3Ord
  	laAddress[3,2] = EVAL(lcAlias+'.cAddress3'+lcAddGrp)
	  laAddress[3,3] = SYCINTTMP.nPart3LEN      
	  laAddress[4,1] = SYCINTTMP.nPart4Ord
  	laAddress[4,2] = EVAL(lcAlias+'.cAddress4'+lcAddGrp)
	  laAddress[4,3] = SYCINTTMP.nPart4LEN      
  	laAddress[5,1] = SYCINTTMP.nPart5Ord
	  laAddress[5,2] = EVAL(lcAlias+'.cAddress5'+lcAddGrp)
    laAddress[5,3] = SYCINTTMP.nPart5LEN      
  	laAddress[6,1] = SYCINTTMP.nPart6Ord
	  laAddress[6,2] = EVAL(lcAlias+'.cAddress6'+lcAddGrp)
 	  laAddress[6,3] = SYCINTTMP.nPart6LEN
  	IF TYPE("lcCurrCode") = 'C'
   	 &lcCurrCode = SYCINTTMP.cCurrCode
	  ENDIF  
 	 =ASORT(laAddress,1)
	  lcRetVal=''
  *-- if it is array
	  IF llRetArray
 	   PRIVATE lnArrLen
  	  lnArrLen = 0
   	 FOR lnArrLen = 1 TO ALEN(lnLineNo,1)
    	  FOR lnCount = 1 TO ALEN(laAddress,1)
     	   IF laAddress[lnCount,1] = lnArrLen
      	    lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
       	   lnLineNo[lnArrLen] = lnLineNo[lnArrLen]+;
        	                       IIF(EMPTY(lnLineNo[lnArrLen]) .OR. ;
         	                      RIGHT(lnLineNo[lnArrLen],1) = ',' ,'',', ') + lcAddPart
	         ENDIF
  	    ENDFOR
    	ENDFOR
	  ELSE  && else numeric value.

  	  FOR lnCount = 1 TO ALEN(laAddress,1)
    	  IF laAddress[lnCount,1] = lnLineNo
      	  lnCounter = lnCounter + 1
        	lcAddPart = ALLTRIM(SUBSTR(laAddress[lnCount,2],1,laAddress[lnCount,3]))
	        lcRetVal  = lcRetVal+IIF(EMPTY(lcRetVal) .OR. RIGHT(lcRetVal,1) = ',' ,'',;
  	                           IIF(ALLTRIM(oAriaEnvironment.DefaultCountry) = "USA" AND lnCounter > 2 ,'  ' ,', ')) + lcAddPart
    	   ENDIF
	    ENDFOR
	  ENDIF
  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]
  
	ELSE
	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [begin]
 	 *lcRetVal= EVAL(lcAlias+'.cAddress'+STR(lnLineNo,1)+lcAddGrp)
  *-- if it is an array
	  IF llRetArray
 	   PRIVATE lnArrLen
  	  lnArrLen = 0
   	 FOR lnArrLen = 1 TO ALEN(lnLineNo,1)
    	  lnLineNo[lnArrLen]= EVAL(lcAlias+'.cAddress'+STR(lnArrLen,1)+lcAddGrp)
    	ENDFOR
	  ELSE  && else it is numeric value.
 	   lcRetVal= EVAL(lcAlias+'.cAddress'+STR(lnLineNo,1)+lcAddGrp)
  	ENDIF  
	  *E301278,1 lnLineNo may be a numeric or an array hold addresses (Passed by reference) [end  ]
	ENDIF  
ENDIF

IF USED("SYCINTTMP")
	USE IN SYCINTTMP
ENDIF
*-- Hesham (End)
SET ORDER TO lnOldTag IN (lcAlias)
SELECT IIF(!EMPTY(lcCurAlias),(lcCurAlias),0)  

RETURN lcRetVal


*!*****************************************************************************************
*! Name      : DoUserValidFunction
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 12/11/2002 12:55:05 PM
*! Purpose   : Do the user valid function inside custom program.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*!Modifications:
*! B037932,1 MAH 04/06/2004 problem in (loOGScroll.DoUserValidFunction),when trying to find 
*!           the program contains the user valid function. we have to add the extension 
*!           (.FXP) to find the related program.
*!*****************************************************************************************
*!
*-- NB:- Parameters are Self-Descriptive
LPARAMETERS cFunctionName, vFunctionProgram, cFunctionParameter, vFunctionProgram

*-- lcReturnVal: Variable to hold the return value.
IF EMPTY(cFunctionName)  && No Function to do.
  RETURN ""  && May it's called from within .FRX, OR .LBX
ENDIF 

PRIVATE lcReturnVal  && Private to store the changed values in called function.
lcReturnVal  = ""

cFunctionName = ALLTRIM(cFunctionName)

*-- If the user pass an object reference, instead of a program
*-- (New Feature in Version 4.0) 
IF VARTYPE(vFunctionProgram) = "O"
  IF !PEMSTATUS(vFunctionProgram,cFunctionName,5) && if method not exist.
    RETURN ""
  ENDIF
  IF EMPTY(cFunctionParameter)
    vFunctionProgram.&cFunctionName.(lcReturnVal)
  ELSE
    vFunctionProgram.&cFunctionName.(lcReturnVal, &cFunctionParameter.)
  ENDIF
  RETURN lcReturnVal   	
ENDIF 

*-- Normal as Aria27 section.
IF EMPTY(vFunctionProgram) .AND. !EMPTY(This.lcOptProg)
  vFunctionProgram = ALLTRIM(This.lcOptProg)
ENDIF

IF EMPTY(vFunctionProgram)   && No Function program passed.
  RETURN ""
ENDIF 

*-- Get the Function Program.
*!*	vFunctionProgram = IIF(gfFileExist(This.gcRepHome+vFunctionProgram+'.FXP'),;
*!*	                       This.gcRepHome+vFunctionProgram,;
*!*	                   IIF(gfFileExist(This.gcRepHome+This.gcAct_Appl+'\'+vFunctionProgram+'.FXP'),;
*!*	                       This.gcRepHome+This.gcAct_Appl+'\'+vFunctionProgram,;
*!*	                       This.gcRepHome+LEFT(vFunctionProgram,2)+'\'+vFunctionProgram))

* B037932,1 MAH Add FXP suffix to function program name
*!* vFunctionProgram = ALLTRIM(vFunctionProgram) + '.FXP'
* B037932,1 MAH End

IF !gfFileExist(vFunctionProgram)   && No Function program found.
  RETURN ""
ENDIF 

IF EMPTY(cFunctionParameter)
  DO (cFunctionName) IN (vFunctionProgram) WITH lcReturnVal
ELSE
  DO (cFunctionName) IN (vFunctionProgram) WITH lcReturnVal, &cFunctionParameter.
ENDIF
RETURN lcReturnVal

FUNCTION lfOptProg
*!*****************************************************************************************
*! Name      : DoOptionalProgram
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 12/11/2002 12:55:05 PM
*! Purpose   : Do the report form optional program (If their)
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Modifications
*! B038289,1 SMM 07/12/2004 add extension in file exist
*!*****************************************************************************************
LPARAMETERS lcOptionalProgram

LOCAL lcProgToDo

lcProgToDo = ALLTRIM(lcOptionalProgram)
*!*	IF EMPTY(lcProgToDo)  && Nothing passed to the method.
*!*	  RETURN .F.
*!*	ENDIF 

*!*	*-- Get Optional Program name.
*!*	lcProgToDo = IIF(gfFileExist(gcRepHome+lcProgToDo+'.FXP'),;
*!*	                 This.gcRepHome+lcProgToDo,;
*!*	             IIF(gfFileExist(This.gcRepHome+This.gcAct_Appl+'\'+lcProgToDo+'.FXP'),;
*!*	                 This.gcRepHome+This.gcAct_Appl+'\'+lcProgToDo,;
*!*	                 This.gcRepHome+LEFT(lcProgToDo,2)+'\'+lcProgToDo))

*!*	* B038289,1 SMM 07/12/2004 add extension in file exist [START]
*!*	* IF gfFileExist(lcProgToDo)
IF gfFileExist(lcProgToDo + '.FXP')
* B038289,1 SMM 07/12/2004 add extension in file exist [END]
  DO (lcProgToDo)  && Run the optional program.
ENDIF   

RETURN .T.


*!*****************************************************************************************
*! Name      : gfFileExist
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 01/26/2003 11:53:32 AM
*! Purpose   : Check the existance of a file in a disk.
*! Entry no. : 
*!*****************************************************************************************
*!
FUNCTION gfFileExist
LPARAMETERS cFileToCheckFor
LOCAL llExists, lcOldDefaSet, llError, lcOldError, lcJustFile, lcJustPath

*-- Return .F. if wrong passed parameters.
IF (VARTYPE(cFileToCheckFor)!= "C") OR EMPTY(cFileToCheckFor)
  RETURN .F.
ENDIF 

cFileToCheckFor  = ALLTRIM(cFileToCheckFor)
lcOldError       = ON("ERROR")
ON ERROR llError = .T.
lcOldDefaSet = FULLPATH(SET("Default"))

lcJustPath = ADDBS(JUSTPATH(cFileToCheckFor))
lcJustFile = JUSTFNAME(cFileToCheckFor)

SET DEFAULT TO (lcJustPath)
llExists = !llError AND FILE(lcJustFile)  && If set defa return error it's not exists.
llExists = !llError AND llExists          && If File returns error it's not exists.

ON ERROR &lcOldError.
SET DEFAULT TO (lcOldDefaSet)
RETURN llExists

FUNCTION gfRltFld
PARAMETERS lcCodeVal,laArrayNam, lcFldName
PRIVATE laTempCodes,lcObjValue,llFileUsd,lcOldOrdr
*-- Hesham (Start)
*lcSavSelct  = ALIAS()   && Variable to save the currently selected file.

*!*	*B603246,1 (Begin) Open sydfield file with Cfld_name tag .
*!*	*--Is 'sydfield ' file used?
*!*	llFileUsd = .F.
*!*	IF !USED('sydfield')
*!*	  USE oAriaEnvironment.SysPath+'sydfield' ORDER TAG Cfld_name IN 0
*!*	  llFileUsd = .T.
*!*	ELSE
*!*	  *-- Save old order OF sydfield.
*!*	  lcOldOrdr = ORDER('sydfield')
*!*	  SET ORDER TO TAG Cfld_name IN sydfield
*!*	ENDIF
*!*	*B603246,1 (End)

*!*	IF EMPTY(lcCodeVal)   && Case N/A 
*!*	  FOR lnCount  = 1 TO ALEN(laArrayNam,1) 
*!*	    *B603246,1 (Begin) Remark the following lines and get the field type from sydfield file.
*!*	    *DO CASE
*!*	    *  CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Cc'
*!*	    *    lcObjValue = ''
*!*	    *  CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Nn'
*!*	    *    lcObjValue = 0
*!*	    *  CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Ll'
*!*	    *    lcObjValue = .F.
*!*	    *  CASE SUBSTR(laArrayNam[lnCount,1],1,1) $ 'Dd'
*!*	    *    lcObjValue = {}
*!*	    *ENDCASE
*!*	    *--Get field type from sydfield instead of any guess cases like naming convensions errors..
*!*	    lcObjValue = UPPER(IIF(SEEK(PADR(laArrayNam[lnCount,1],10),'sydfield'),sydfield.cData_Typ,''))
*!*	    *--Initialize the related filed variable according to the field type.
*!*	    DO CASE
*!*	      *-- Case char
*!*	      CASE lcObjValue = "C"
*!*	        lcObjValue = ''
*!*	      *-- Case Numeric
*!*	      CASE lcObjValue = "N"  
*!*	        lcObjValue = 0
*!*	      *-- Case Logical
*!*	      CASE lcObjValue = "L"  
*!*	        lcObjValue = .F.
*!*	      *-- Case date
*!*	      CASE lcObjValue = "D"
*!*	        lcObjValue = {}
*!*	    ENDCASE
*!*	    *B603246,1 (End)    
*!*	    lcFieldNam  = laArrayNam[lnCount,2]
*!*	    &lcFieldNam = lcObjValue
*!*	  ENDFOR  
*!*	  RETURN
*!*	ENDIF

*!*	*E300631,1 YMA 04/06/97 Select the codes file instead of SYCCodes.
*!*	*SELECT SYCCODES         && Select CODES file
*!*	llUseCodes = .F.
*!*	IF !USED("CODES")
*!*	  USE (oAriaEnvironment.DataDir+"Codes") IN 0
*!*	  llUseCodes = .T.
*!*	ENDIF
*!*	SELECT CODES         && Select CODES file
*!*	*E300631,1 YMA 04/06/97 End.
*!*	lcSavOrder = SYS(22)    && Save the file order
*!*	SET ORDER TO 0          && To activate rushmore

*!*	DECLARE laTempCodes[1]
*!*	laTempCodes = ' '

*!*	*E300631,1 YMA 04/06/97 Changed the file name to be "Codes" instead of "SYCCodes".
*!*	*E300631,1 YMA 04/06/97 And include the lcFldName in the "where" clause.
*!*	*SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
*!*	*  FROM SYCCODES;
*!*	*  WHERE CCOMP_ID + CRLTFIELD + CFLD_NAME = gcAct_Comp + 'Y' ;
*!*	*  AND   CCODE_NO = lcCodeVal ;
*!*	*  INTO ARRAY laTempCodes
*!*	*TAK E300973,1 Changed to work under visual.
*!*	*SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
*!*	*  FROM CODES;
*!*	*  WHERE CCOMP_ID + CRLTFIELD + CFLD_NAME = gcAct_Comp + 'Y' + lcFldName;
*!*	*  AND   CCODE_NO = lcCodeVal ;
*!*	*  INTO ARRAY laTempCodes
*!*	SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
*!*	  FROM CODES;
*!*	  WHERE cDefCode + CRLTFIELD + CFLD_NAME = 'N' + 'Y' + lcFldName;
*!*	  AND   CCODE_NO = lcCodeVal ;
*!*	  INTO ARRAY laTempCodes
*!*	*TAK E300973,1 End.
*!*	*E300631,1 YMA 04/06/97 End.

*!*	FOR lnCount  = 1 TO ALEN(laArrayNam,1) 
*!*	  lnPosition = ASCAN(laTempCodes,laArrayNam[lnCount,1])

*!*	  IF lnPosition = 0     && not found
*!*	    *B603246,1 (Begin) Get the field type from sydfield file and if 
*!*	    *B603246,1         the related field was not saved in the codes file.
*!*	    *B603246,1         then inisialize it's value.
*!*	    *--Get field type.    
*!*	    lcObjValue = UPPER(IIF(SEEK(PADR(laArrayNam[lnCount,1],10),'sydfield'),sydfield.cData_Typ,''))
*!*	    *--Initialize the related filed variable according to the field type.
*!*	    DO CASE
*!*	      *-- case char
*!*	      CASE lcObjValue = "C"
*!*	        lcObjValue = ''
*!*	      *-- case Numeric
*!*	      CASE lcObjValue = "N"  
*!*	        lcObjValue = 0
*!*	      *-- case Logical
*!*	      CASE lcObjValue = "L"  
*!*	        lcObjValue = .F.
*!*	      *-- case date
*!*	      CASE lcObjValue = "D"
*!*	        lcObjValue = {}
*!*	    ENDCASE
*!*	    lcFieldNam  = laArrayNam[lnCount,2]
*!*	    &lcFieldNam = lcObjValue
*!*	    *B603246,1  (End)    
*!*	  ELSE
*!*	    lnPosition = ASUBSCRIPT(laTempCodes,lnPosition,1)
*!*	    DO CASE
*!*	      CASE laTempCodes[lnPosition,2] = 'C'
*!*	        lcObjValue = laTempCodes[lnPosition,3]
*!*	      CASE laTempCodes[lnPosition,2] = 'N'
*!*	        lnDecimPos = AT('.',laTempCodes[lnPosition,3])
*!*	        IF lnDecimPos > 0
*!*	          lcSavDecim = SET('DECIMALS')  && Save old decimals setting
*!*	          SET DECIMALS TO lnDecimPos
*!*	          lcObjValue = VAL(laTempCodes[lnPosition,3])
*!*	          SET DECIMALS TO &lcSavDecim          
*!*	        ELSE
*!*	          lcObjValue = VAL(laTempCodes[lnPosition,3])
*!*	        ENDIF  
*!*	      CASE laTempCodes[lnPosition,2] = 'L'
*!*	        lcObjValue = IIF(ALLTRIM(laTempCodes[lnPosition,3]) $ 'YT',.T.,.F.)

*!*	      CASE laTempCodes[lnPosition,2] = 'D'      
*!*	        lcObjValue = CTOD(laTempCodes[lnPosition,3])
*!*	    ENDCASE

*!*	    lcFieldNam  = laArrayNam[lnCount,2]
*!*	    &lcFieldNam = lcObjValue
*!*	  ENDIF  
*!*	ENDFOR  

*!*	SET ORDER TO &lcSavOrder
*!*	IF llUseCodes
*!*	  USE IN Codes
*!*	ENDIF
*!*	*B603246,1 (Begin) If the sydfield file was not used close it, else restore the old order.
*!*	IF llFileUsd
*!*	  USE IN sydfield
*!*	ELSE
*!*	  SET ORDER TO TAG lcOldOrdr IN sydfield
*!*	ENDIF  
*!*	*B603246,1 (End)

*B039255,1 KHM 04/27/2005 Initialize a temporary name for the sydfield file [Begin] 
PRIVATE lcTmpFile
lcTmpFile   = oAriaEnvironment.Cursors.GetCursorTempName()
*B039255,1 KHM 04/27/2005 [End]

lnSessionID = SET("DATASESSION")
lcSavSelct  = ALIAS()  && Variable to save the currently selected file.

* Open sydfield file Remotely.
lcWhereCond = ""
FOR lnCount = 1 TO ALEN(laArrayNam,1)
  *E303030,1 BEGIN
  *lcWhereCond = lcWhereCond + IIF(lnCount>1," OR ","")+[CFLD_NAME=']+PADR(laArrayNam[lnCount,1],10)+[']
  lcWhereCond = lcWhereCond + IIF(lnCount>1," OR ","")+[CFLD_NAME=']+PADR(laArrayNam[lnCount,1],oAriaEnvironment.FieldW)+[']
  *E303030,1 END
NEXT 
lcSqlCmd = "Select * from sydfield " + IIF(!EMPTY(lcWhereCond),"WHERE ","") + lcWhereCond

*B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
*lnRemResult = oAriaEnvironment.remotesystemdata.execute(lcSqlCmd,'',"sydfieldtmp","",oAriaEnvironment.SystemConnectionString,3,"",lnSessionID)
lnRemResult = oAriaEnvironment.remotesystemdata.execute(lcSqlCmd,'',lcTmpFile,"",oAriaEnvironment.SystemConnectionString,3,"",lnSessionID)
*B039255,1 KHM 04/27/2005 [End]

IF EMPTY(lcCodeVal)   && Case N/A 
  FOR lnCount  = 1 TO ALEN(laArrayNam,1) 
    *E303030,1 BEGIN
    *LOCATE FOR cfld_name = PADR(laArrayNam[lnCount,1],10)
    LOCATE FOR cfld_name = PADR(laArrayNam[lnCount,1],oAriaEnvironment.FieldW)
    *E303030,1 END

    *B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
    *lcObjValue = UPPER(IIF(FOUND(),sydfieldtmp.cData_Typ,''))
    lcObjValue = UPPER(IIF(FOUND(),EVALUATE(lcTmpFile+'.cData_Typ'),''))
    *B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [End]

    *--Initialize the related filed variable according to the field type.
    DO CASE
      *-- Case char
      CASE lcObjValue = "C"
        lcObjValue = ''
      *-- Case Numeric
      CASE lcObjValue = "N"  
        lcObjValue = 0
      *-- Case Logical
      CASE lcObjValue = "L"  
        lcObjValue = .F.
      *-- Case date
      CASE lcObjValue = "D"
        lcObjValue = {}
    ENDCASE
    lcFieldNam  = laArrayNam[lnCount,2]
    &lcFieldNam = lcObjValue
  ENDFOR  
  *-- Amin
  SET DATASESSION TO lnSessionID
	SELECT IIF(EMPTY(lcSavSelct),0,lcSavSelct)
  *-- Amin
  RETURN
ENDIF

llUseCodes = .F.
IF !USED("CODES")
  USE (oAriaEnvironment.DataDir+"Codes") IN 0
  llUseCodes = .T.
ENDIF
SELECT CODES         && Select CODES file
*E300631,1 YMA 04/06/97 End.
lcSavOrder = SYS(22)    && Save the file order
SET ORDER TO 0          && To activate rushmore

DECLARE laTempCodes[1]
laTempCodes = ' '

SELECT CRLTD_NAM,CRLTD_TYP,CRLTD_VLU ;
  FROM CODES;
  WHERE cDefCode + CRLTFIELD + CFLD_NAME = 'N' + 'Y' + lcFldName;
  AND   CCODE_NO = lcCodeVal ;
  INTO ARRAY laTempCodes
*TAK E300973,1 End.
*E300631,1 YMA 04/06/97 End.

*B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
*SELECT sydfieldtmp
SELECT (lcTmpFile)
*B039255,1 KHM 04/27/2005 [End]

FOR lnCount  = 1 TO ALEN(laArrayNam,1) 
  lnPosition = ASCAN(laTempCodes,laArrayNam[lnCount,1])

  IF lnPosition = 0     && not found
    *--Get field type.    
    *E303030,1 BEGIN
    *LOCATE FOR CFLD_NAME = PADR(laArrayNam[lnCount,1],10)
    LOCATE FOR CFLD_NAME = PADR(laArrayNam[lnCount,1],oAriaEnvironment.FieldW)
    *E303030,1 end

    *B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
    *lcObjValue = UPPER(IIF(FOUND(),sydfieldtmp.cData_Typ,''))
    lcObjValue = UPPER(IIF(FOUND(),EVALUATE(lcTmpFile+'.cData_Typ'),''))
    *B039255,1 KHM 04/27/2005 [End]

    *--Initialize the related filed variable according to the field type.
    DO CASE
      *-- case char
      CASE lcObjValue = "C"
        lcObjValue = ''
      *-- case Numeric
      CASE lcObjValue = "N"  
        lcObjValue = 0
      *-- case Logical
      CASE lcObjValue = "L"  
        lcObjValue = .F.
      *-- case date
      CASE lcObjValue = "D"
        lcObjValue = {}
    ENDCASE
    lcFieldNam  = laArrayNam[lnCount,2]
    &lcFieldNam = lcObjValue
  ELSE
    lnPosition = ASUBSCRIPT(laTempCodes,lnPosition,1)
    DO CASE
      CASE laTempCodes[lnPosition,2] = 'C'
        lcObjValue = laTempCodes[lnPosition,3]
      CASE laTempCodes[lnPosition,2] = 'N'
        lnDecimPos = AT('.',laTempCodes[lnPosition,3])
        IF lnDecimPos > 0
          lcSavDecim = SET('DECIMALS')  && Save old decimals setting
          SET DECIMALS TO lnDecimPos
          lcObjValue = VAL(laTempCodes[lnPosition,3])
          SET DECIMALS TO &lcSavDecim          
        ELSE
          lcObjValue = VAL(laTempCodes[lnPosition,3])
        ENDIF  
      CASE laTempCodes[lnPosition,2] = 'L'
        lcObjValue = IIF(ALLTRIM(laTempCodes[lnPosition,3]) $ 'YT',.T.,.F.)

      CASE laTempCodes[lnPosition,2] = 'D'      
        lcObjValue = CTOD(laTempCodes[lnPosition,3])
    ENDCASE

    lcFieldNam  = laArrayNam[lnCount,2]
    &lcFieldNam = lcObjValue
  ENDIF  
ENDFOR  
SELECT CODES
SET ORDER TO &lcSavOrder

IF llUseCodes
  USE IN Codes
ENDIF
*B039255,1 KHM 04/27/2005 Use lcTmpFile instead of sydfieldtmp [Begin]
*!*	IF USED("sydfieldtmp")
*!*	  USE IN sydfieldtmp
*!*	ENDIF

IF USED(lcTmpFile)
  USE IN (lcTmpFile)
ENDIF
*B039255,1 KHM 04/27/2005 [End]

SET DATASESSION TO lnSessionID
*-- Hesham (End)  

SELECT IIF(EMPTY(lcSavSelct),0,lcSavSelct)


*!*************************************************************
*! Name        : gfOpenTable
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Create a Remote Table Object if Trying to open Fox or Sql Table Defined in the Sysem Files
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : File Name, Index Tag, Open Mode ("EX" or "SH"), Alias Name, Force Open
*!*************************************************************
*! Returns            :  True  ----> If passed file is open by this function
*!                       False ----> If passed file is already open.
*!*************************************************************
FUNCTION gfOpenTable
LPARAMETERS NFILE,lcIndex,MODE,lcAliasNam,llForceOp,lcCompId,llFastTable
LOCAL lnTable, lnL

lcAliasNam = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),JUSTSTEM(NFILE),lcAliasNam)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAliasNam)

IF lnTable=0 && No Remote Table Object was Found

  lnL = ALEN(oAriaApplication.laRemoteTable)
  IF TYPE('oAriaApplication.laRemoteTable[lnL]')='O'
    lnL = lnL + 1 
    DIMENSION oAriaApplication.laRemoteTable[lnL]
  ENDIF
  *WAIT WINDOW NFILE+', '+lcIndex+', '+PADR(SET("Datasession"),10) TIMEOUT 0.5
  lcIndex = IIF(EMPTY(lcIndex),'',JUSTSTEM(lcIndex))
  oAriaApplication.laRemoteTable[lnL] = CREATEOBJECT("RemoteTable",JUSTSTEM(NFILE),lcIndex,lcAliasNam,SET("Datasession"),lcCompId,llFastTable)
  IF TYPE('oAriaApplication.laRemoteTable[lnL]')<>'O'
    lnL = MAX(lnL - 1,1)
    DIMENSION oAriaApplication.laRemoteTable[lnL]
    RETURN gfOpenFile(NFILE,lcIndex,MODE,lcAliasNam,llForceOp)
  ENDIF

ELSE && a Remote Table Object Already Exist

  IF !EMPTY(lcIndex)
    *WAIT WINDOW lcIndex TIMEOUT 0.5
    oAriaApplication.laRemoteTable[lnTable].SetOrder(JUSTSTEM(lcIndex))
  ENDIF

ENDIF

RETURN 
*--end of gfOpenTable


*!*************************************************************
*! Name      : gfOpenFile
*! Developer : MALAK - Malak Hanna
*! Date      : 04/18/1995
*! Purpose   : To open database needed by the program.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : File Name, Index Tag, 
*!                          Open Mode "EX" ----> "EXCLUSIVE"
*!                                    "SH" ----> "SHARED"
*!*************************************************************
*! Returns            :  True  ----> If passed file is open by this function
*!                       False ----> If passed file is already open.
*!*************************************************************
*! Example            : =gfOpenFile(QDD+'ORDHDR',QDD+'ORDHDR','SH')
*!*************************************************************
*E300247,1 YMA 06/13/95 Changed the displaying of the file
*E300247,1 name to be in the status message.
*B602015,1 AHM 05/07/98 using alias name to use the file by this alias
*!*************************************************************

FUNCTION gfOpenFile
PARAMETERS NFILE,lcIndex,MODE,lcAliasNam,llForceOp
PRIVATE MODE,lcFileName,lcPath,llReturnVal,lcMsg,lcSetExact

PRIVATE lcMacroSub
lcMacroSub=""
lcFileName = IIF(ATC('\',nfile)<>0,SUBSTR(NFILE,RAT('\',nfile)+1),NFILE)
lcOpenMode = IIF(TYPE('MODE')='C' AND MODE='EX', "EXCLUSIVE", "SHARED")
lcOrderTag = IIF(TYPE('lcIndex')='C',SUBSTR(lcIndex,IIF('\' $ lcIndex,ATC('\',lcIndex,OCCURS('\',lcIndex)),0) +1),'')

PRIVATE llOpen 
llOpen = .F.

lcAliasNam = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),ALLTRIM(STRTRAN(UPPER(lcFileName),".DBF")),lcAliasNam)
      
lcMsg = 'Opening '+NFILE+IIF(EMPTY(lcIndex),'', ' Index Tag '+lcOrderTag)+'....'
lcMsg = PROPER(lcMsg)
IF 'SCREEN' $ SYS(101)
  SET MESSAGE TO lcMsg
ENDIF
llReturnVal = .T.
lcFPathSt   = SET('FULLPATH')
SET FULLPATH ON
IF USED(lcAliasNam)
  lcOpenMode = "SHARED"
  *-- if the file is used and it is from the same data directory
  IF DBF(lcAliasNam) == ALLTRIM(STRTRAN(UPPER(nFile), ".DBF") + ".DBF")
    *-- if forced open is desired
    IF llForceOp
      lcAliasNam  = gfTempName()
      lcMacroSub="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
      &lcMacroSub
      llOpen = .T.
      IF !EMPTY(lcOrderTag)
        SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
      ENDIF    &&IF !EMPTY(lcOrderTag)
    ELSE
      *-- if forced open is not desired
      llReturnVal = .F.
      *-- if there is no tag is desired to set order to
      IF EMPTY(lcOrderTag)
        SET ORDER TO 0 IN (lcAliasNam)
      ELSE
        SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
      ENDIF   &&IF EMPTY(lcOrderTag)
    ENDIF     &&IF llForceOp
  ELSE
    *-- if the file is used but not from the same data directory
    lcAliasNam  = gfTempName()
    lcMacroSub="USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
    &lcMacroSub
    llOpen = .T.
    IF !EMPTY(lcOrderTag)
      SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
    ENDIF  &&IF !EMPTY(lcOrderTag)
  ENDIF   &&IF DBF(lcFilename) == .......
ELSE
  *-- if the file is not used

  lcMacroSub = "USE (NFILE) ALIAS (lcAliasNam) AGAIN IN 0 &lcOpenMode"
  &lcMacroSub
  llOpen = .T.
  IF !EMPTY(lcOrderTag)
    SET ORDER TO TAG lcOrderTag IN (lcAliasNam)
  ENDIF    &&IF !EMPTY(lcOrderTag)
ENDIF    &&IF IF USED(lcFilename)
SELECT (lcAliasNam)
SET FULLPATH &lcFPathSt
IF !UPPER(oAriaEnvironment.WorkDir) $ UPPER(NFILE)
  lcErrOn = ON('ERROR')
  llError = .F.
  ON ERROR llError = .T.
  IF TYPE('laFileName')<>'U'
    lcSetExact = SET('Exact')
    SET EXACT ON
    FOR lnFilePos = 1 TO ALEN(laFileName,1)
      IF ALLTRIM(laFileName[lnFilePos,1]) == ALLTRIM(lcAliasNam)
        EXIT
      ENDIF
    ENDFOR
    IF lnFilePos > ALEN(laFileName,1)
      lnFilePos = 0
    ENDIF
    IF lnFilePos = 0
      IF !EMPTY(laFileName[1,1])
        DIMEN laFileName[ALEN(laFileName,1)+1,ALEN(laFileName,2)]
      *-- MAN Added ELSE Cond.  
      ELSE
        DIME laFileName[1,4] 
      ENDIF
      laFileName[ALEN(laFileName,1),1] = lcAliasNam
      laFileName[ALEN(laFileName,1),2] = lcOrderTag
      laFileName[ALEN(laFileName,1),3] = NFILE
      laFileName[ALEN(laFileName,1),4] = lcFilename
      FOR lnFileElm = 1 TO ALEN(gaMnu_Fl,1)
        IF ALLTRIM(gaMnu_Fl[lnFileElm,1]) == ALLTRIM(lcAliasNam)
          EXIT
        ENDIF
      ENDFOR
      IF lnFileElm > ALEN(gaMnu_Fl,1)
        lnFileElm = 0
      ENDIF
      IF lnFileElm > 0 
        lnRowNo = lnFileElm 
        gaMnu_Fl[lnRowNo,4] = gaMnu_Fl[lnRowNo,4] + 1
      ELSE
        DECLARE gaMnu_Fl[ALEN(gaMnu_Fl,1)+1,ALEN(gaMnu_Fl,2)]
        =AINS(gaMnu_Fl,1)
        gaMnu_Fl[1,1] = lcAliasNam
        gaMnu_Fl[1,2] = lcOrderTag
        gaMnu_Fl[1,3] = SELECT(0)
        gaMnu_Fl[1,4] = 1
        gaMnu_Fl[1,5] = IIF(!llOpen ,'S','P')
        gaMnu_Fl[1,6] = " "
      ENDIF
    ENDIF
    SET EXACT &lcSetExact
  ENDIF
  ON ERROR &lcErrOn
ENDIF  
IF 'SCREEN' $ SYS(101)
  SET MESSAGE TO ""
ENDIF
RETURN llReturnVal


*!*****************************************************************************************
*! Name      : GFCRTFRM
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/16/2002 10:02:28 PM
*! Purpose   : 
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: 
*!****************************************************************************************
*! Returns   : 
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*!
FUNCTION gfCrtFrm 
PARAMETERS lcFormName,lcArrayName,llChkChanges

LOCAL lnActiveAlias, llReturn
lnActiveAlias = SELECT(0)
SELECT 0
llReturn = CreateForm(lcFormName,lcArrayName,llChkChanges)
SELECT (lnActiveAlias)
RETURN llReturn
*-- end of main program.

*!*****************************************************************************************
*! Name      : CreateForm
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 10/16/2002 10:02:28 PM
*! Purpose   : Creates a report temporary form.
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters: 
*!****************************************************************************************
*! Returns   : 
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*!
FUNCTION CreateForm
  PARAMETERS lcFormName,lcArrayName,llChkChanges
   
  lcOGTmpForm =oAriaenvironment.report.lcreptmpnm

  lnMemoWid=SET('MEMOWIDTH')
  SET MEMOWIDTH TO 254

  PRIVATE lcFileName
  IF RAT('\',lcFormName)=0
    lcFullSet = SET('FULLPATH')
    SET FULLPATH ON
    lcFormName= IIF(gfFileExist(gcRepHome+lcFormName+'.FRX') .OR. gfFileExist(gcRepHome+lcFormName+'.LBX'),;
              gcRepHome+lcFormName,gcRepHome+gcAct_Appl+'\'+lcFormName)
    SET FULL &lcFullSet
  ENDIF  
  lcFileName = IIF(RAT('\',lcFormName)>0,SUBSTR(lcFormName,RAT('\',lcFormName)+1),lcFormName)

  IF gfFileExist(gcWorkDir+lcOGTmpForm+'.FRX')
    ERASE &gcWorkDir.&lcOGTmpForm..FRX
    ERASE &gcWorkDir.&lcOGTmpForm..FRT  
  ENDIF

  IF gfFileExist(gcWorkDir+lcOGTmpForm+'.LBX')
    ERASE &gcWorkDir.&lcOGTmpForm..LBX
    ERASE &gcWorkDir.&lcOGTmpForm..LBT  
  ENDIF

  IF gfFileExist(ALLTRIM(lcFormName) + '.FRX') .OR. gfFileExist(ALLTRIM(lcFormName) + '.LBX')
    IF gfFileExist(ALLTRIM(lcFormName) + '.FRX')
      SELECT * FROM (lcFormName+".FRX");
        WHERE PLATFORM=oAriaEnvironment.Report.lcOGPlatForm AND objtype<>10;
        INTO DBF (gcWorkDir+lcOGTmpForm+".FRX")
      
      *-- Handle the FRX in England Case (A4 instead to Letter size)
      IF (oAriaEnvironment.Report.lcOGPlatForm = "WINDOWS") AND (oAriaApplication.DefaultCountry = "ENG") AND USED(lcOGTmpForm)
        SELECT (lcOGTmpForm)
        LOCATE FOR OBJTYPE = 1 AND OBJCODE = 53 AND ASC(SUBSTR(Tag2,47,47))=1
        IF FOUND()
          REPLACE TAG2 WITH STUFF(TAG2,47,1,CHR(9))
        ENDIF
      ENDIF
    
    ELSE
      SELECT * FROM (lcFormName+".LBX");
        WHERE PLATFORM=oAriaEnvironment.Report.lcOGPlatForm AND objtype<>10;
        INTO DBF (gcWorkDir+lcOGTmpForm+".LBX")
    ENDIF
  ELSE
    STORE '' TO lcOGFormV, lcOGTmpForm
    RETURN
  ENDIF

  IF llChkChanges
    DELETE FOR !lfCanDisp() AND;
                INLIST(OBJTYPE ,8,5,7) AND BETWEEN(OBJCODE,0,7)           
    REPLACE VPOS WITH lfOGVPos(),;
            HPOS WITH lfOGHPos(),;
            HEIGHT WITH lfOGHSize(),;
            WIDTH WITH lfOGWSize();
        FOR INLIST(OBJTYPE ,8,5,7) AND BETWEEN(OBJCODE,0,7)
    PACK              
  ENDIF

  IF USED(lcOGTmpForm)
    USE IN (lcOGTmpForm)
  ENDIF

  IF USED(lcFileName)
    USE IN (lcFileName)
  ENDIF
  SET MEMOWIDTH TO lnMemoWid
ENDFUNC 


*!*********************************************************************************
*!
*!             FUNCTION : lfModiStyle
*!
*!*********************************************************************************
*  function to change the style of the objects in the frx
FUNCTION lfModiStyle
PARAMETERS lnRecNo
lnArrPos=ASCAN(laOGObjPos,lnRecNo)
lnArrPos=IIF(lnArrPos>0,ASUBSCRIPT(laOGObjPos,lnArrPos,1),0)
RETURN IIF(lnArrPos=0,'',ALLTRIM(&lcArrayName[lnArrPos,6]))



*!*********************************************************************************
*!
*!             FUNCTION : lfOGVPos 
*!
*!*********************************************************************************
*
FUNCTION lfOGVPos 
PARAMETERS lnObjPos
m.YPos=VPOS
IF ATCLINE('#VPOSITION ',comment)>0
  lcClaus=MLINE(comment,ATCLINE('#VPOSITION ',comment))
  lcSize=ALLTRIM(SUBSTR(lcClaus,ATC("#VPOSITION ",lcClaus)+10))
  m.YPos=EVAL(lcSize)
ENDIF   
RETURN m.Ypos

*!*********************************************************************************
*!
*!             FUNCTION : lfOGHPos 
*!
*!*********************************************************************************
*
FUNCTION lfOGHPos 
PARAMETERS lnObjPos
m.XPos=HPOS
IF ATCLINE('#HPOSITION ',comment)>0
  lcClaus=MLINE(comment,ATCLINE('#HPOSITION ',comment))
  lcSize=ALLTRIM(SUBSTR(lcClaus,ATC("#HPOSITION ",lcClaus)+10))
  m.XPos =EVAL(lcSize)
  m.XPos=ROUND(m.Xpos*IIF(oAriaEnvironment.Report.lcOGPlatForm='WINDOWS' OR oAriaEnvironment.Report.lcOGPlatForm='MAC',FONTMETRIC(6,ALLTRIM(FONTFACE),FONTSIZE)*104.16665,1),3)
ENDIF   
RETURN m.XPos

*!*********************************************************************************
*!
*!             FUNCTION : lfOGHSize
*!
*!*********************************************************************************
*
FUNCTION lfOGHSize
PARAMETERS lnObjPos
m.Height = HEIGHT
lnClaus = ATCLINE('#HSIZE ',comment)
IF lnClaus > 0
  lcClaus=MLINE(comment,ATCLINE('#HSIZE ',comment))
  lcSize=ALLTRIM(SUBSTR(lcClaus,ATC("#HSIZE ",lcClaus)+6))
  m.Height=EVAL(lcSize)
ENDIF
RETURN m.HEIGHT

*!*********************************************************************************
*!
*!             FUNCTION : lfOGWSize
*!
*!*********************************************************************************
*
FUNCTION lfOGWSize
PARAMETERS lnObjPos
m.Width  = WIDTH 
lnClaus = ATCLINE('#WSIZE ',comment)
IF lnClaus > 0
  lcClaus=MLINE(comment,ATCLINE('#WSIZE ',comment))
  lcSize=ALLTRIM(SUBSTR(lcClaus,ATC("#WSIZE ",lcClaus)+6))
  m.Width =EVAL(lcSize)
  m.width=ROUND(m.WIDTH*IIF(oAriaEnvironment.Report.lcOGPlatForm='WINDOWS' OR oAriaEnvironment.Report.lcOGPlatForm='MAC',FONTMETRIC(6,ALLTRIM(FONTFACE),FONTSIZE)*104.16665,1),3)
ENDIF
RETURN m.Width

*!*********************************************************************************
*!
*!             FUNCTION : lfCanDisp
*!
*!*********************************************************************************
*  Function to See if the object can be selected to the frx or not
FUNCTION lfCanDisp
lcCondition=''
llCanDisp=.T.
IF !EMPTY(Comment)
  IF ATCLINE("#OBJDISP",comment)>0
    lcCondition = SUBSTR(MLINE(comment,ATCLINE("#OBJDISP",comment)),;
    ATC("#OBJDISP",MLINE(comment,ATCLINE("#OBJDISP",comment)))+9)
  ENDIF
ENDIF   
IF !EMPTY(lcCondition)
  lnNoCond=OCCURS('#OBJDISP',UPPER(comment))
  FOR lnCount=1 to lnNoCond
    lcCondition = ALLTRIM(SUBSTR(comment,ATC("#OBJDISP",comment,lnCount)))
    lcCondition = SUBSTR(MLINE(lcCondition,ATCLINE("#OBJDISP",lcCondition)),;
    ATC("#OBJDISP",MLINE(lcCondition,ATCLINE("#OBJDISP",lcCondition)))+9)
    llCanDisp=EVAL(lcCondition)
    IF !llCanDisp
      EXIT
    ENDIF
  ENDFOR  
ENDIF 
RETURN llCanDisp

*!*********************************************************************************
*!
*!             FUNCTION : lfGetCom
*!
*!*********************************************************************************
*
FUNCTION lfGetCom
PARAMETERS lcString
PRIVATE ALL
IF ATC('(',lcString)=0
  RETURN ATC(',',lcString)
ELSE
  lnCouts=0
  lnComPos=0
  FOR lnCount = 1 TO LEN(lcString)
    DO CASE
      CASE SUBSTR(lcString,lnCount,1)='('
        lnCouts=lnCouts+1
      CASE SUBSTR(lcString,lnCount,1)=')'
        lnCouts=lnCouts-1       
      CASE SUBSTR(lcString,lnCount,1)=','
        IF lnCouts=0
          lnComPos=lnCount
          EXIT
        ENDIF  
    ENDCASE
  ENDFOR
ENDIF
RETURN lnComPos

FUNCTION lfRepPltFr
  LPARAMETERS lcRepFrmName
  *MMT22
  *loOGScroll = CREATEOBJECT('Report')
  loOGScroll = oAriaEnvironment.Report
  *MMT22
  RETURN loOGScroll.GetPlatForm(lcRepFrmName)
ENDFUNC 

FUNCTION gfTempName
RETURN oAriaEnvironment.Cursors.GetCursorTempName()

FUNCTION gfcrttmp
PARAMETERS lcFile,lcFileStruc,lcTagExp,lcTag,llCursor
*N000635,1 MMT 10/19/2009 Convert Print Check report to Aria4[Start]
*RETURN oAriaEnvironment.Cursors.createcursor(lcOrdlTmp ,@laTableStruct,'Style',lcOrdlTmp ,.F.)
RETURN oAriaEnvironment.Cursors.createcursor(lcFile,@lcFileStruc,@lcTagExp,lcTag,llCursor)
*N000635,1 MMT 10/19/2009 Convert Print Check report to Aria4[End]
FUNCTION gfDispRe
PARAMETERS lcOGRprtNam,lcCriteria,llEndJob,lcRprtLbl,llPrntToFile
oAriaEnvironment.report.print(lcOGRprtNam,lcCriteria,llEndJob,lcRprtLbl,llPrntToFile)

FUNCTION gfopenFile
PARAMETERS NFILE,lcIndex,MODE,lcAliasNam,llForceOp
oAriaEnvironment.remotetableaccess.OpenTable(oAriaEnvironment.DataDir + NFILE, lcIndex, MODE, lcAliasNam, llForceOp)

FUNCTION gfCodDes
PARAMETERS lcCodeVal, lcFldName , llChkEdit
*E302789,1 MMT 10/28/2010 modify invoice form to call custom forms from RB[Start]
*oAriaEnvironment.codes.getcodedescription(lcCodeVal, lcFldName , llChkEdit) 
RETURN oAriaEnvironment.codes.getcodedescription(lcCodeVal, lcFldName , llChkEdit) 
*E302789,1 MMT 10/28/2010 modify invoice form to call custom forms from RB[End]

FUNCTION gfGetMemVar
PARAMETERS lcArray,lcCompID
*B612455,1 MMT 09/04/2023 Get setting from RB[Start]
*RETURN oAriaEnvironment.setups.getSetting(@lcArray,lcCompID)
RETURN oAriaEnvironment.setups.getSetting(@lcArray,lcCompID)
*B612455,1 MMT 09/04/2023 Get setting from RB[End]


FUNCTION gfSequence
*B608122,1  TMI [Start] Add three parameters for Document#,Sql Table, sql Tag
*PARAMETERS lcSeqType,lcCompanyId,lcGroupId,lcDivision,lcField
PARAMETERS lcSeqType,lcCompanyId,lcGroupId,lcDivision,lcField,lcTranType,lcTable,lcTag
*B608122,1  TMI [End  ] 
PRIVATE lnRetVal,lcSavAlias,lcDataDir,lnOldGenNm,lcExtraStr,lcToFind,lcKeyExp,;
        gcDataDir,gcComp_Mdl,gcSysHome,gcCurSite,gcAct_Comp,gcOrgPath

*!*	gcDataDir  = oAriaApplication.DataDir
*!*	gcComp_Mdl = oAriaApplication.CompanyInstalledModules
*!*	gcSysHome  = oAriaApplication.SysPath
*!*	gcCurSite  = oAriaApplication.CurrentSite
*!*	gcAct_Comp = oAriaApplication.ActiveCompanyId
*!*	gcOrgPath  = oAriaApplication.DefaultPath
*!*	*TAK E300973,1 end.

*!*	*E300894,1 06/18/98 YMA Validate the optional passed parameter.
*!*	lcField    = IIF(TYPE("lcField")="C", ALLTRIM(UPPER(lcField)), SPACE(0))
*!*	*E300894,1 06/18/98 YMA End.

*!*	lcSavAlias = SELECT(0)
*!*	lcSeqType  = UPPER(lcSeqType)
*!*	lcDataDir = gcDataDir

*!*	*E300888 06/04/98 YMA If the communication module is installed, then
*!*	*E300888              get the unique site prefix for the active site
*!*	*E300888              from the sites file.
*!*	*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [Begin]
*!*	*lcUnqPreFx = SPACE(0)
*!*	*IF "CM" $ gcComp_Mdl
*!*	*  USE (gcSysHome+"SYCSITES") IN 0 AGAIN ALIAS Sites ORDER cSiteID
*!*	*  lcUnqPreFx = IIF(SEEK(gcCurSite, "Sites"), Sites.cUniqStPre, lcUnqPreFx)
*!*	*  USE IN Sites
*!*	*ENDIF
*!*	PRIVATE lcCmpCode
*!*	PRIVATE oGetMemVar
*!*	oGetMemVar = CREATEObject("GetMemVar", This.oForm)

*!*	lcCmpCode = IIF(TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId), lcCompanyId , gcAct_Comp)
*!*	lcUnqPreFx = oGetMemVar.Do('M_UNQSTPRX' , lcCmpCode)
*!*	*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [End  ]

*!*	*E300888 06/04/98 YMA End.

*!*	IF TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId) AND lcCompanyId <> gcAct_Comp
*!*	  IF USED("sycComp")
*!*	    SELECT sycComp
*!*	    luSycComp = .F.
*!*	    ltSycComp = VAL(SYS(21))
*!*	    leSycComp = RECNO()
*!*	    SET ORDER TO TAG cComp_Id IN syccomp
*!*	  ELSE
*!*	    luSycComp = .T.
*!*	    USE (gcSysHome+"syccomp") ORDER TAG cComp_Id IN 0
*!*	  ENDIF

*!*	  IF SEEK(lcCompanyId,'syccomp')
*!*	    *E301098,1 Hesham (Start)
*!*	    *lcDataDir = ALLTRIM(syccomp.cCom_dDir)
*!*	    *IF UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) = ;
*!*	    *   UPPER(SUBSTR(lcDataDir,1,ATC('\',lcDataDir,2))) AND ;
*!*	    *   UPPER(SUBSTR(gcOrgPath,1,ATC('\',gcOrgPath,2))) <>  ;
*!*	    *   UPPER(SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2)))
*!*	    *  lcDataDir= SUBSTR(gcSysHome,1,ATC('\',gcSysHome,2))+;
*!*	    *             SUBSTR(lcDataDir,ATC('\',lcDataDir,2)+1)
*!*	    *ENDIF
*!*	    lcDataDir = gfGetDataDir(ALLTRIM(syccomp.cCom_dDir))
*!*	  ENDIF
*!*	  IF luSycComp
*!*	    USE IN syccomp
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSycComp IN syccomp
*!*	    IF BETWEEN(leSycComp,1,RECCOUNT('syccomp'))
*!*	      GOTO leSycComp IN 'syccomp'
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
*!*	*E301046,4 Assure that lcGroupId is 3 Char. only
*!*	*lcGroupId  = IIF(TYPE('lcGroupId') ='C',ALLTRIM(lcGroupId),SPACE(2))
*!*	lcGroupId  = IIF(TYPE('lcGroupId') ='C',PADR(lcGroupId,3),SPACE(3))
*!*	*E301046,4 end
*!*	lcDivision = IIF(TYPE('lcDivision')='C',ALLTRIM(lcDivision),SPACE(10))
*!*	lnRetVal   = 0

*!*	*300632,1 Get division sequence group
*!*	*B802982,1 [start] Don't GET the GroupID if the system is not set to
*!*	*                  generate seq.# based on division
*!*	llDivOnSeq = oGetMemVar.Do('M_DIV_SEQ' , lcCompanyId) = 'Y'
*!*	RELEASE oGetMemVar
*!*	*Change this line to check the llDivOnSeq
*!*	*IF EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
*!*	IF llDivOnSeq AND EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
*!*	*B802982,1 [End]

*!*	  DECLARE laDivDlt[1,2]
*!*	  laDivDlt[1,1] = 'DIVGROUP'
*!*	  laDivDlt[1,2] = 'lcGroupId'
*!*	  *TAK E300973,1 Changed to work under visual.
*!*	  *=gfRltFld(PADR(lcDivision,6),@laDivDlt,'CDIVISION')
*!*	  PRIVATE oRlatdFields
*!*	  oRlatdFields = CREATEObject("GetRelatedFields", This.oForm)
*!*	  oRlatdFields.Do(PADR(lcDivision,6),@laDivDlt,'CDIVISION')
*!*	  RELEASE oRlatdFields
*!*	  *TAK E300973,1 End.

*!*	  *E301046,4 Change lcGroupId to be 3 Char. only
*!*	  *lcGroupId = SUBSTR(lcGroupId,1,10)
*!*	  lcGroupId = SUBSTR(lcGroupId,1,3)
*!*	  *E301046,4 end  
*!*	ENDIF
*!*	*B802982,1 [start] make sure the group id is empty if the system 
*!*	*                  is not set to generate seq.# based on division
*!*	*                  This case will BE FEASABLE ONLY 
*!*	*                  IF llDivOnSeq = .F.
*!*	*                  AND !EMPTY(lcGroupId)
*!*	lcGroupId = IIF(llDivOnSeq , SUBSTR(lcGroupId,1,3) , SPACE(3))
*!*	*B802982,1 [End]

*!*	IF !USED('SEQUENCE')
*!*	  luSequence = .T.
*!*	  USE &lcDataDir.SEQUENCE IN 0 ORDER TAG 'cSeq_Type'
*!*	ELSE
*!*	  SELECT SEQUENCE
*!*	  luSequence = .F.
*!*	  ltSequence = VAL(SYS(21))
*!*	  leSequence = RECNO()
*!*	  SET ORDER TO TAG Cseq_type IN SEQUENCE
*!*	ENDIF

*!*	IF !SEEK(PADR(lcSeqType,10)+lcGroupId,'SEQUENCE')
*!*	  IF !USED('sydflfld')
*!*	    luSydflfld = .T.
*!*	    USE &gcSysHome.sydflfld ORDER TAG 'Cfld_name' IN 0 SHARED
*!*	  ELSE
*!*	    SELECT Sydflfld
*!*	    luSydflfld = .F.
*!*	    ltSydflfld = VAL(SYS(21))
*!*	    leSydflfld = RECNO()
*!*	    SET ORDER TO TAG Cfld_name IN 'sydflfld'
*!*	  ENDIF
*!*	  IF !USED('sydfield')
*!*	    luSydfield = .T.
*!*	    USE &gcSysHome.sydfield ORDER TAG 'Cfld_name' IN 0 SHARED
*!*	  ELSE
*!*	    SELECT Sydfield
*!*	    luSydfield = .F.
*!*	    ltSydfield = VAL(SYS(21))
*!*	    leSydfield  = RECNO()
*!*	    SET ORDER TO TAG Cfld_name IN 'sydfield'
*!*	  ENDIF
*!*	  
*!*	  *E300894,1 06/18/98 YMA Use the optional field to get the sequence
*!*	  *E300894,1              proprities instead of the sequence field
*!*	  *E300894,1              if any.
*!*	  lcPropFld = IIF(EMPTY(lcField), lcSeqType, lcField)
*!*	  = SEEK(PADR(lcPropFld,10),'sydfield')
*!*	  SELECT sydflfld
*!*	  = SEEK(PADR(lcPropFld,10))
*!*	  LOCATE REST WHILE cFld_Name=PADR(lcPropFld,10) FOR lEnumerate
*!*	  
*!*	  *=SEEK(PADR(lcSeqType,10),'sydfield')
*!*	  *SELECT sydflfld
*!*	  *=SEEK(PADR(lcSeqType,10))
*!*	  *LOCATE REST WHILE cFld_Name=PADR(lcSeqType,10) FOR lEnumerate
*!*	  *E300894,1 06/18/98 YMA End.

*!*	  lnDefSeq = sydflfld.nDef_Seq
*!*	  IF !EMPTY(lcGroupId) AND SEEK(PADR(lcSeqType,10),'SEQUENCE')
*!*	    SELECT SEQUENCE
*!*	    lnDefSeq = 0
*!*	    SCAN REST WHILE cseq_type+cseq_group = PADR(lcSeqType,10)
*!*	      lnDefSeq = MAX(lnDefSeq,nSeq_No)
*!*	    ENDSCAN
*!*	    lnDefSeq = (INT(lnDefSeq/50000)+1)*50000
*!*	  ENDIF
*!*	  
*!*	  INSERT INTO SEQUENCE (cSeq_Type,nSeq_No,cSeq_Group,cData_Typ,nFld_Wdth) ;
*!*	       VALUES (lcSeqType,lnDefSeq,lcGroupId,sydfield.cData_Typ,;
*!*	       sydfield.nFld_Wdth)
*!*	  IF sydflfld.lEnumerate
*!*	    IF !USED('sydfiles')
*!*	      luSydfiles = .T.
*!*	      USE &gcSysHome.sydfiles ORDER TAG 'Cfile_nam' IN 0 SHARED
*!*	    ELSE
*!*	      SELECT Sydfiles
*!*	      luSydfiles = .F.
*!*	      ltSydfiles = VAL(SYS(21))
*!*	      leSydfiles = RECNO()
*!*	      SET ORDER TO TAG Cfile_nam IN 'sydfiles'
*!*	    ENDIF
*!*	    =SEEK(sydflfld.cFile_Nam,'sydfiles')
*!*	    SELECT SEQUENCE
*!*	    REPLACE cFile_Nam WITH sydfiles.cFile_Nam ,;
*!*	            cFile_Tag WITH sydfiles.cFile_Tag
*!*	    IF luSydfiles
*!*	      USE IN Sydfiles
*!*	    ELSE
*!*	      SET ORDER TO TAG ltSydfiles IN Sydfiles
*!*	      IF BETWEEN(leSydfiles,1,RECCOUNT('Sydfiles'))
*!*	        GOTO leSydfiles IN 'Sydfiles'
*!*	      ENDIF
*!*	    ENDIF
*!*	  ENDIF
*!*	  IF luSydflfld
*!*	    USE IN Sydflfld
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSydflfld IN Sydflfld
*!*	    IF BETWEEN(leSydflfld,1,RECCOUNT('Sydflfld'))
*!*	      GOTO leSydflfld IN 'Sydflfld'
*!*	    ENDIF
*!*	  ENDIF
*!*	  IF luSydfield
*!*	    USE IN Sydfield
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSydfield IN Sydfield
*!*	    IF BETWEEN(leSydfield,1,RECCOUNT('Sydfield'))
*!*	      GOTO leSydfield IN 'Sydfield'
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
*!*	*--MAN Added RLOCK Condition[Start]
*!*	   DO WHILE !RLOCK("SEQUENCE")
*!*	   ENDDO
*!*	  lnRetVal = SEQUENCE.nSeq_No
*!*	*--MAN Added RLOCK Condition[End]

*!*	*E300888 06/04/98 YMA Compute the required code width assuming that
*!*	*E300888              the minemum code field width = 6.
*!*	lnRetLen = SEQUENCE.nFld_Wdth - LEN(lcUnqPreFx)
*!*	*E300888 06/04/98 YMA End.
*!*	*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	lnOldGenNm = SEQUENCE.nSeq_No
*!*	lcExtraStr = ''
*!*	IF !EMPTY(SEQUENCE.cSeq_Chr)
*!*	  lcExtraStr = SEQUENCE.cSeq_Chr
*!*	ENDIF
*!*	*B603586,1 SSH 29/02/00 (End)

*!*	IF !EMPTY(SEQUENCE.cFile_Nam) .AND. !EMPTY(SEQUENCE.cFile_Tag)
*!*	  lcSeqFile = ALLTRIM(SEQUENCE.cFile_Nam)
*!*	  lcSeqTag  = ALLTRIM(SEQUENCE.cFile_Tag)
*!*	  IF !USED(lcSeqFile)
*!*	    luSeqFile = .T.
*!*	    *B601946,1 Use the file again to prevent 'File is in use' message
*!*	    *USE &gcDataDir.&lcSeqFile ORDER TAG (lcSeqTag) IN 0 SHARED
*!*	    USE &gcDataDir.&lcSeqFile AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED 
*!*	    *B601946,1 end
*!*	  ELSE
*!*	    SELECT (lcSeqFile)
*!*	    luSeqFile = .F.
*!*	    ltSeqFile = VAL(SYS(21))
*!*	    leSeqFile = RECNO()
*!*	    SET ORDER TO TAG (lcSeqTag) IN (lcSeqFile)
*!*	  ENDIF
*!*	  SELECT (lcSeqFile)
*!*	  lcKeyField = SUBSTR(KEY(),1,AT('+'+lcSeqType,KEY())-1)
*!*	  DECLARE laVldEnt[1]

*!*	  *TAK E300973,1 Changed to work under visual.
*!*	  PRIVATE oGetValid
*!*	  oGetValid = CREATEObject("GetValidEntries")
*!*	* IF !EMPTY(lcKeyField) .AND. gfGetVld(lcKeyField,@laVldEnt) > 0
*!*	  IF !EMPTY(lcKeyField) .AND. oGetValid.Do(lcKeyField,@laVldEnt) > 0
*!*	    FOR lnCount = 1 TO ALEN(laVldEnt,1)
*!*	      *E300888 06/04/98 YMA Search for the generated code prefixed with
*!*	      *E300888              the unique site prefix.
*!*	      *DO WHILE SEEK(laVldEnt[lnCount,2]+PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'),lcSeqFile)
*!*	      *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	      lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                       ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
*!*	      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+PADL(lnRetVal,lnRetLen,"0"),lcSeqFile)
*!*	      DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcSeqFile)
*!*	      *B603586,1 SSH 29/02/00  (End)      
*!*	      *E300888 06/04/98 YMA End.
*!*	        lnRetVal = lnRetVal + 1
*!*	        *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	        IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
*!*	          lcExtraStr = CHR(ASC(lcExtraStr)+1)
*!*	        ENDIF
*!*	        lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                         ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
*!*	        *B603586,1 SSH 29/02/00  (End)

*!*	      ENDDO
*!*	    ENDFOR
*!*	  ELSE  
*!*	    *E300888 06/04/98 YMA Search for the generated code prefixed with
*!*	    *E300888              the unique site prefix.
*!*	    *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	    lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                     ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
*!*	    *DO WHILE SEEK(PADL(lnRetVal,lnRetLen,'0'),lcSeqFile)
*!*	    DO WHILE SEEK(lcKeyExp,lcSeqFile)
*!*	    *E300888 06/04/98 YMA End.
*!*	      lnRetVal = lnRetVal + 1
*!*	      *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	      IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
*!*	        lcExtraStr = CHR(ASC(lcExtraStr)+1)
*!*	      ENDIF
*!*	      lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
*!*	                                       ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
*!*	      *B603586,1 SSH 29/02/00  (End)
*!*	    ENDDO
*!*	  ENDIF  
*!*	  RELEASE oGetValid
*!*	    
*!*	  IF luSeqFile
*!*	    USE IN (lcSeqFile)
*!*	  ELSE
*!*	    SET ORDER TO TAG ltSeqFile IN (lcSeqFile)
*!*	    IF BETWEEN(leSeqFile,1,RECCOUNT(lcSeqFile))
*!*	      GOTO leSeqFile IN (lcSeqFile)
*!*	    ENDIF
*!*	  ENDIF
*!*	ENDIF
*!*	SELECT SEQUENCE
*!*	*B603586,1 SSH 29/02/00  (Begin) Check if [lnRetVal+1] exceed 6 digit.
*!*	*REPLACE nSeq_No WITH lnRetVal+1
*!*	REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)
*!*	*B603586,1 SSH 29/02/00 (End)

*!*	*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*!*	IF nSeq_No = 0 .AND. lnOldGenNm <> 0
*!*	  REPLACE cSeq_Chr WITH IIF(EMPTY(cSeq_Chr),'A',CHR(ASC(cSeq_Chr)+1))
*!*	ENDIF
*!*	IF !EMPTY(lcExtraStr)
*!*	  lnRetVal = ALLTRIM(lcExtraStr) + PADL(lnRetVal,lnRetLen-1,"0")
*!*	ENDIF
*!*	*B603586,1 SSH 29/02/00 (End)

*!*	*--MAN Added RLOCK Condition[Start]
*!*	UNLOCK
*!*	*--MAN Added RLOCK Condition[End]

*!*	*TAK E300973,1 Changed to work under visual.
*!*	*=gfAdd_info('SEQUENCE')
*!*	*oAriaApplication.AddUserInformation('SEQUENCE')

*!*	        
*!*	*E300888 06/04/98 YMA Never return a numeric code, and return the code
*!*	*E300888              prefixed with the active site unique prefix code 
*!*	*E300888              if any.
*!*	*lnRetVal = IIF(SEQUENCE.cData_Typ='N',lnRetVal,;
*!*	*                PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'))
*!*	 lnRetVal = lcUnqPreFx + PADL(lnRetVal, lnRetLen, "0")
*!*	*E300888 06/04/98 YMA End.

*!*	IF luSequence
*!*	  USE IN Sequence
*!*	ELSE
*!*	  SET ORDER TO TAG ltSequence IN Sequence
*!*	  IF BETWEEN(leSequence,1,RECCOUNT('Sequence'))
*!*	    GOTO leSequence IN 'Sequence'
*!*	  ENDIF
*!*	ENDIF
*!*	SELECT (lcSavAlias)
*!*	RETURN(lnRetVal)

gcDataDir  = oAriaApplication.DataDir
gcComp_Mdl = oAriaApplication.CompanyInstalledModules
gcSysHome  = oAriaApplication.SysPath
gcCurSite  = oAriaApplication.CurrentSite
gcAct_Comp = oAriaApplication.ActiveCompanyId
gcOrgPath  = oAriaApplication.DefaultPath
*TAK E300973,1 end.

*E300894,1 06/18/98 YMA Validate the optional passed parameter.
lcField    = IIF(TYPE("lcField")="C", ALLTRIM(UPPER(lcField)), SPACE(0))
*E300894,1 06/18/98 YMA End.

lcSavAlias = SELECT(0)
lcSeqType  = UPPER(lcSeqType)
lcDataDir = gcDataDir

*E300888 06/04/98 YMA If the communication module is installed, then
*E300888              get the unique site prefix for the active site
*E300888              from the sites file.
*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [Begin]
*lcUnqPreFx = SPACE(0)
*IF "CM" $ gcComp_Mdl
*  USE (gcSysHome+"SYCSITES") IN 0 AGAIN ALIAS Sites ORDER cSiteID
*  lcUnqPreFx = IIF(SEEK(gcCurSite, "Sites"), Sites.cUniqStPre, lcUnqPreFx)
*  USE IN Sites
*ENDIF
PRIVATE lcCmpCode

*B606902,1 Define a variable for cSeq_Chr updating in sequence file. [Begin]
PRIVATE lcChrToUpd
lcChrToUpd = CHR(0)
*B606902,1 Define a variable for cSeq_Chr updating in sequence file. [End]


lcCmpCode = IIF(TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId), lcCompanyId , gcAct_Comp)
lcUnqPreFx = gfGetMemVar('M_UNQSTPRX' , lcCmpCode)
*E301488,1 12/03/2000 MAB Get PreFix Value from SETUPS FILE. [End  ]

*E300888 06/04/98 YMA End.

IF TYPE('lcCompanyId')='C' AND !EMPTY(lcCompanyId) AND lcCompanyId <> gcAct_Comp
	lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from syccomp where cComp_ID='"+lcCompanyId+"'",'',"syccomptmp","",oAriaApplication.SystemConnectionString,3)
	IF lnRemResult>=1
  	LOCATE
    * lcDataDir = gfGetDataDir(ALLTRIM(syccomptmp.cCom_dDir))
  ENDIF
  IF USED("syccomptmp")
    USE IN syccomptmp
  ENDIF
ENDIF
*E301046,4 Assure that lcGroupId is 3 Char. only
*lcGroupId  = IIF(TYPE('lcGroupId') ='C',ALLTRIM(lcGroupId),SPACE(2))
lcGroupId  = IIF(TYPE('lcGroupId') ='C',PADR(lcGroupId,3),SPACE(3))
*E301046,4 end
lcDivision = IIF(TYPE('lcDivision')='C',ALLTRIM(lcDivision),SPACE(10))
lnRetVal   = 0

*300632,1 Get division sequence group
*B802982,1 [start] Don't GET the GroupID if the system is not set to
*                  generate seq.# based on division
llDivOnSeq = gfGetMemVar('M_DIV_SEQ' , lcCompanyId) = 'Y'
*Change this line to check the llDivOnSeq
*IF EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
IF llDivOnSeq AND EMPTY(lcGroupId) .AND. !EMPTY(lcDivision)
*B802982,1 [End]

  DECLARE laDivDlt[1,2]
  laDivDlt[1,1] = 'DIVGROUP'
  laDivDlt[1,2] = 'lcGroupId'
  =gfRltFld(PADR(lcDivision,6),@laDivDlt,'CDIVISION')

  *E301046,4 Change lcGroupId to be 3 Char. only
  *lcGroupId = SUBSTR(lcGroupId,1,10)
  lcGroupId = SUBSTR(lcGroupId,1,3)
  *E301046,4 end  
ENDIF
*B802982,1 [start] make sure the group id is empty if the system 
*                  is not set to generate seq.# based on division
*                  This case will BE FEASABLE ONLY 
*                  IF llDivOnSeq = .F.
*                  AND !EMPTY(lcGroupId)
lcGroupId = IIF(llDivOnSeq , SUBSTR(lcGroupId,1,3) , SPACE(3))
*B802982,1 [End]

IF !USED('SEQUENCE')
  luSequence = .T.
  USE (lcDataDir+"SEQUENCE") IN 0 ORDER TAG 'cSeq_Type'
ELSE
  SELECT SEQUENCE
  luSequence = .F.
  ltSequence = VAL(SYS(21))
  leSequence = RECNO()
  SET ORDER TO TAG Cseq_type IN SEQUENCE
ENDIF

IF !SEEK(PADR(lcSeqType,10)+lcGroupId,'SEQUENCE')
  *-- Hesham Start
  *-- B606591,1 Get the active datasession and send it to the RemoteDataAccess Object
  LOCAL lnDataSess
  lnDataSess = SET("Datasession")  
  *-- Hesham End
  *E300894,1 06/18/98 YMA Use the optional field to get the sequence
  *E300894,1              proprities instead of the sequence field
  *E300894,1              if any.
	lcPropFld = IIF(EMPTY(lcField), lcSeqType, lcField)
  *-- Hesham Start
  *-- B606591,1  send the active datasession to the RemoteDataAccess Object
    *E303030,1 BEGIN
	*lnRemFldResult = oAriaApplication.remotesystemdata.execute("Select * from sydfield where Cfld_name='"+PADR(lcPropFld,10)+"'",'',"sydfieldtmp","",oAriaApplication.SystemConnectionString,3,"",lnDataSess)
	lnRemFldResult = oAriaApplication.remotesystemdata.execute("Select * from sydfield where Cfld_name='"+PADR(lcPropFld,oAriaApplication.FieldW)+"'",'',"sydfieldtmp","",oAriaApplication.SystemConnectionString,3,"",lnDataSess)
    *E303030,1 end
  *-- Hesham End
	IF lnRemFldResult=1
   LOCATE
  ENDIF

  *-- Hesham Start
  *-- B606591,1  send the active datasession to the RemoteDataAccess Object
    *E303030,1 BEGIN
	*lnRemFlFldResult = oAriaApplication.remotesystemdata.execute("Select * from sydflfld where Cfld_name='"+PADR(lcPropFld,10)+"' AND lEnumerate=1",'',"sydflfldtmp","",oAriaApplication.SystemConnectionString,3,"",lnDataSess)
	lnRemFlFldResult = oAriaApplication.remotesystemdata.execute("Select * from sydflfld where Cfld_name='"+PADR(lcPropFld,oAriaApplication.FieldW)+"' AND lEnumerate=1",'',"sydflfldtmp","",oAriaApplication.SystemConnectionString,3,"",lnDataSess)
    *E303030,1 end
  *-- Hesham End
	IF lnRemFlFldResult=1
	  LOCATE
	ENDIF
		
  *E303030,1 BEGIN
  *LOCATE REST WHILE cFld_Name=PADR(lcPropFld,10) FOR lEnumerate
  LOCATE REST WHILE cFld_Name=PADR(lcPropFld,oAriaApplication.FieldW) FOR lEnumerate
  *E303030,1 end
    
  lnDefSeq = sydflfldtmp.nDef_Seq
  IF !EMPTY(lcGroupId) AND SEEK(PADR(lcSeqType,10),'SEQUENCE')
    SELECT SEQUENCE
    lnDefSeq = 0
    SCAN REST WHILE cseq_type+cseq_group = PADR(lcSeqType,10)
      lnDefSeq = MAX(lnDefSeq,nSeq_No)
    ENDSCAN
    lnDefSeq = (INT(lnDefSeq/50000)+1)*50000
  ENDIF
  *B606902,4 KHM 02/09/2003 (Begin) Replacing the cSeq_Chr with CHR(0)  
  *INSERT INTO SEQUENCE (cSeq_Type,nSeq_No,cSeq_Group,cData_Typ,nFld_Wdth) ;
       VALUES (lcSeqType,lnDefSeq,lcGroupId,sydfieldtmp.cData_Typ,;
       sydfieldtmp.nFld_Wdth)

  INSERT INTO SEQUENCE (cSeq_Type,nSeq_No,cSeq_Group,cData_Typ,nFld_Wdth,cSeq_Chr) ;
       VALUES (lcSeqType,lnDefSeq,lcGroupId,sydfieldtmp.cData_Typ,;
       sydfieldtmp.nFld_Wdth,CHR(0))
  *B606902,4 KHM 02/09/2003 (End)
       
  IF sydflfldtmp.lEnumerate
      *-- Hesham Start
      *-- B606591,1  send the active datasession to the RemoteDataAccess Object
  	lnRemFlResult = oAriaApplication.remotesystemdata.execute("Select * from sydfiles where Cfile_nam='"+sydflfldtmp.cFile_Nam+"'",'',"sydfilestmp","",oAriaApplication.SystemConnectionString,3,"",lnDataSess)
  	*-- Hesham End
	  IF lnRemFlResult=1
      LOCATE
    ENDIF
    SELECT SEQUENCE
    REPLACE cFile_Nam WITH sydfilestmp.cFile_Nam ,;
            cFile_Tag WITH sydfilestmp.cFile_Tag
    IF USED("SYDFILESTMP")
      USE IN Sydfilestmp
    ENDIF
  ENDIF
  IF USED("SYDFLFLDTMP")
    USE IN Sydflfldtmp
  ENDIF
  IF USED("sydfieldtmp")
    USE IN Sydfieldtmp
  ENDIF
ENDIF
*--MAN Added RLOCK Condition[Start]
   DO WHILE !RLOCK("SEQUENCE")
   ENDDO
  lnRetVal = SEQUENCE.nSeq_No

  *B606902,1 Get the character expression. [Begin]
  lcChrToUpd = Sequence.cSeq_Chr
  *B606902,1 Get the character expression. [End]
  
*--MAN Added RLOCK Condition[End]

*E300888 06/04/98 YMA Compute the required code width assuming that
*E300888              the minemum code field width = 6.
lnRetLen = SEQUENCE.nFld_Wdth - LEN(lcUnqPreFx)
*E300888 06/04/98 YMA End.
*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
lnOldGenNm = SEQUENCE.nSeq_No
lcExtraStr = ''
IF !EMPTY(SEQUENCE.cSeq_Chr)
  *B606902,1 Get the Extra string added to the file. [Begin]
  *lcExtraStr = SEQUENCE.cSeq_Chr
  PRIVATE lcChar , lnCharPos , lnI
  IF !(SEQUENCE.cSeq_Chr = CHR(0))
    IF MOD(ASC(SEQUENCE.cSeq_Chr),26) = 0
      lcChar = "Z"
      lnCharPos = ASC(SEQUENCE.cSeq_Chr)/26
    ELSE
      lcChar =  CHR(MOD(ASC(SEQUENCE.cSeq_Chr),26)+64)
      lnCharPos = INT(ASC(SEQUENCE.cSeq_Chr)/26)+1
    ENDIF  
    FOR lnI = 1 TO lnCharPos - 1
      lcExtraStr = lcExtraStr + "Z"
    ENDFOR
    lcExtraStr = lcExtraStr + lcChar
  ELSE
  lcChar=""
  ENDIF
  *B606902,1 Get the Extra string added to the file. [End]
ENDIF
*B603586,1 SSH 29/02/00 (End)
*IF !EMPTY(SEQUENCE.cFile_Nam) AND !EMPTY(SEQUENCE.cFile_Tag)
IF !EMPTY(SEQUENCE.cFile_Nam) .AND. UPPER(LEFT(SEQUENCE.cFile_Nam,2))<> 'SY' AND !EMPTY(SEQUENCE.cFile_Tag)
  lcSeqFile = ALLTRIM(SEQUENCE.cFile_Nam)
  lcSeqTag  = ALLTRIM(SEQUENCE.cFile_Tag)  

  *B608122,1  TMI [Start] check if the parameter is empty
  lcTranType = IIF(EMPTY(lcTranType),'',lcTranType)
  lcSeqFile = IIF(EMPTY(lcTable),lcSeqFile,lcTable)
  lcSeqTag  = IIF(EMPTY(lcTag)  ,lcSeqTag ,lcTag)
  *B608122,1  TMI [End  ] 
  
  *B132218,1 KHM 05/25/2006 [Start]
  *IF !USED(lcSeqFile)
  *  luSeqFile = .T.
    *B601946,1 Use the file again to prevent 'File is in use' message
    *USE &gcDataDir.&lcSeqFile ORDER TAG (lcSeqTag) IN 0 SHARED
  *  USE (gcDataDir+lcSeqFile) AGAIN ORDER TAG (lcSeqTag) IN 0 SHARED 
    *B601946,1 end
  *ELSE
  *  SELECT (lcSeqFile)
  *  luSeqFile = .F.
  *  ltSeqFile = VAL(SYS(21))
  *  leSeqFile = RECNO()
  *  SET ORDER TO TAG (lcSeqTag) IN (lcSeqFile)
  *ENDIF
  PRIVATE lcNewSeqFile
  lcNewSeqFile = gfTempName()
  =gfOpenTable(gcDataDir+lcSeqFile,lcSeqTag,'SH',lcNewSeqFile)
  *B132218,1 KHM 05/25/2006 [End]
 

  *B132218,1 KHM 05/25/2006 [Start]
  *SELECT (lcSeqFile)
  SELECT (lcNewSeqFile)
  *B132218,1 KHM 05/25/2006 [End]
  lcKeyField = SUBSTR(KEY(),1,AT('+'+lcSeqType,KEY())-1)
  DECLARE laVldEnt[1]
  
  *B608122,1  TMI [Start] Define this variable as local so it will not be affected with defining variables with same name in any called function.
  LOCAL lnCount  
  *B608122,1  TMI [End  ] 

  *TAK E300973,1 Changed to work under visual.
  IF !EMPTY(lcKeyField) .AND. gfGetVld(lcKeyField,@laVldEnt) > 0
    FOR lnCount = 1 TO ALEN(laVldEnt,1)
      *E300888 06/04/98 YMA Search for the generated code prefixed with
      *E300888              the unique site prefix.
      *DO WHILE SEEK(laVldEnt[lnCount,2]+PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'),lcSeqFile)
      *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.

      *B606902,1 Check if next sequence number is valid. [Begin]
      *lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
      *                                 ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
      lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
      *B606902,1 Check if next sequence number is valid. [End]

      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+PADL(lnRetVal,lnRetLen,"0"),lcSeqFile)
      
      *B132218,1 KHM 05/25/2006 [Start]
      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcSeqFile)
      *B608122,1  TMI [Start] call the global funciton gfSeek instead
      *DO WHILE SEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcNewSeqFile)
      DO WHILE gfSEEK(laVldEnt[lnCount,2]+lcUnqPreFx+lcKeyExp,lcNewSeqFile)      
      *B608122,1  TMI [End  ] 
      
      *B132218,1 KHM 05/25/2006 [End]
      
      *B603586,1 SSH 29/02/00  (End)      
      *E300888 06/04/98 YMA End.

        *B606902,1 Check if next sequence number is valid. [Begin]
        *lnRetVal = lnRetVal + 1
        =gfGetSeq(lnRetVal,lcChrToUpd)
        *B606902,1 Check if next sequence number is valid. [End]

        *B606902,1 SSE Commented out. [Begin]
        *IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
        *  lcExtraStr = CHR(ASC(lcExtraStr)+1)
        *ENDIF
        *B606902,1 SSE Commented out. [End]
                
        *B606902,1 Get the new key expression. [Begin]
        *lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
        *                                 ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
        lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
        *B606902,1 Get the new key expression. [end]

      ENDDO
    ENDFOR
  ELSE  
    *E300888 06/04/98 YMA Search for the generated code prefixed with
    *E300888              the unique site prefix.
    *B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.

    *B606902,1 Check if next sequence number is valid. [Begin]
    *lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
    *                                 ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
    lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
    *B606902,1 Check if next sequence number is valid. [End]
    

    *DO WHILE SEEK(PADL(lnRetVal,lnRetLen,'0'),lcSeqFile)
    
    *B132218,1 KHM 05/25/2006 [Start]
    *B608122,1  TMI [Start] call the global function gfSeek instead with the lcTranType passed
    *DO WHILE SEEK(lcKeyExp,lcSeqFile)
    DO WHILE gfSEEK(lcTranType+lcKeyExp,lcNewSeqFile)
    *B608122,1  TMI [End  ] 
    *B132218,1 KHM 05/25/2006 [End]
    *E300888 06/04/98 YMA End.

      *B606902,1 Check if next sequence number is valid. [Begin]
      *lnRetVal = lnRetVal + 1
      =gfGetSeq(lnRetVal,lcChrToUpd)
      *B606902,1 Check if next sequence number is valid. [End]

      *B606902,1 SSE Commented out. [Begin]
      *IF !EMPTY(lcExtraStr) .AND. lnRetVal > 99999999
      *  lcExtraStr = CHR(ASC(lcExtraStr)+1)
      *ENDIF
      *B606902,1 SSE Commented out. [End]
      
      *B606902,1 Get the new key expression. [Begin]
      *lcKeyExp = IIF(EMPTY(lcExtraStr),PADL(lnRetVal,lnRetLen,"0"),;
      *                                 ALLTRIM(lcExtraStr)+PADL(lnRetVal,lnRetLen-1,"0")) 
      lcKeyExp = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
      *B606902,1 Get the new key expression. [end]

      *B603586,1 SSH 29/02/00  (End)
    ENDDO
  ENDIF  
    
  *B132218,1 KHM 05/25/2006 [Start]
  *IF luSeqFile
  *  USE IN (lcSeqFile)
  *ELSE
  *  SET ORDER TO TAG ltSeqFile IN (lcSeqFile)
    *IF BETWEEN(leSeqFile,1,RECCOUNT(lcSeqFile))
  *  IF leSeqFile < 0 OR BETWEEN(leSeqFile,1,RECCOUNT(lcSeqFile))
  *    GOTO leSeqFile IN (lcSeqFile)
  *  ENDIF
  *ENDIF
  =gfCloseTable(lcNewSeqFile)
  *B132218,1 KHM 05/25/2006 [End]
ENDIF
SELECT SEQUENCE
*B603586,1 SSH 29/02/00  (Begin) Check if [lnRetVal+1] exceed 6 digit.
*REPLACE nSeq_No WITH lnRetVal+1
REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)
*B603586,1 SSH 29/02/00 (End)

*B606902,1 Always check if we used Characters before. [Begin]
*REPLACE nSeq_No WITH IIF(lnRetVal + 1 > 999999,0,lnRetVal + 1)
*lnRetVal = lnRetVal + 1
*lnRetVal = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
lnOldGenNm = lcExtraStr+PADL(lnRetVal,lnRetLen-LEN(lcExtraStr),"0")
=gfGetSeq(lnRetVal,lcChrToUpd)
REPLACE nSeq_No WITH lnRetVal , cSeq_Chr WITH lcChrToUpd
*B606902,1 Always check if we used Characters before. [End]

*B603586,1 SSH 29/02/00  (Begin) Use the new field (cSeq_Chr) from sequence file.
*B608122,1  TMI [Start] comment out, no need for this code segment
*IF nSeq_No = 0 .AND. VAL(lnOldGenNm) <> 0
*  REPLACE cSeq_Chr WITH IIF(EMPTY(cSeq_Chr),'A',CHR(ASC(cSeq_Chr)+1))
*ENDIF
*B608122,1  TMI [End  ] 
IF !EMPTY(lcExtraStr)
  lnRetVal = ALLTRIM(lcExtraStr) + PADL(lnRetVal,lnRetLen-1,"0")
ENDIF
*B603586,1 SSH 29/02/00 (End)

*--MAN Added RLOCK Condition[Start]
UNLOCK
*--MAN Added RLOCK Condition[End]

*TAK E300973,1 Changed to work under visual.
*=gfAdd_info('SEQUENCE')
*oAriaApplication.AddUserInformation('SEQUENCE')

        
*E300888 06/04/98 YMA Never return a numeric code, and return the code
*E300888              prefixed with the active site unique prefix code 
*E300888              if any.
*lnRetVal = IIF(SEQUENCE.cData_Typ='N',lnRetVal,;
*                PADL(lnRetVal,SEQUENCE.nFld_Wdth,'0'))

 *B606902,1 Get the value that will be displayed in message box. [Begin]
 *lnRetVal = lcUnqPreFx + PADL(lnRetVal, lnRetLen, "0")
 lnRetVal = lcUnqPreFx + PADL(lnOldGenNm, lnRetLen, "0")
 *B606902,1 Get the value that will be displayed in message box. [End]

*E300888 06/04/98 YMA End.

IF luSequence
  USE IN Sequence
ELSE
  SET ORDER TO TAG ltSequence IN Sequence
  IF BETWEEN(leSequence,1,RECCOUNT('Sequence'))
    GOTO leSequence IN 'Sequence'
  ENDIF
ENDIF
SELECT (lcSavAlias)
RETURN(lnRetVal)

FUNCTION gfGetRemoteTable
LPARAMETERS lnDataSessionID, lcAlias
LOCAL lnC, lnL, lnV

lnV=0
lnL = ALEN(oAriaApplication.laRemoteTable)
FOR lnC=1 TO lnL
  IF TYPE('oAriaApplication.laRemoteTable[lnC]')='O' AND ;
      oAriaApplication.laRemoteTable[lnC].lnDataSession == lnDataSessionID AND ;
      UPPER(oAriaApplication.laRemoteTable[lnC].lcCursorView)==UPPER(lcAlias)
     lnV = lnC
     EXIT
  ENDIF
NEXT

RETURN lnV

*!*************************************************************
*! Name        : gfSeek
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Seek Function
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Expression, Alias, Tag Name, Fast Seek
*!*************************************************************
*! Returns     :  Seek Result
*!*************************************************************
FUNCTION gfSeek
LPARAMETERS lcExp, lcAlias, lcTagName, llFastSeek
LOCAL lnTable

lcAlias = IIF(TYPE('lcAlias')='C',lcAlias,ALIAS())
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  RETURN oAriaApplication.laRemoteTable[lnTable].Seek(lcExp,lcTagName,llFastSeek)
ELSE
  LOCAL lcParam
  lcParam = 'lcExp' + IIF(!EMPTY(lcAlias), ',lcAlias' + IIF(!EMPTY(lcTagName), ',lcTagName', ''), '')
  
  RETURN SEEK(&lcParam.)
ENDIF

RETURN
*--end of gfSeek

*!*************************************************************
*! Name        : gfCloseTable
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Called Instead of the Use / Use in statemnet
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Alias
*!*************************************************************
*! Returns     : None
*!*************************************************************
FUNCTION gfCloseTable
LPARAMETERS lcAlias
LOCAL lnTable, lnLen

IF TYPE('lcAlias')='C' AND 'IN ' $ lcAlias
  lcAlias = gfGetAlias(@lcAlias)
ELSE
  lcAlias = IIF(TYPE('lcAlias')='C',lcAlias,ALIAS())
ENDIF
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  oAriaApplication.laRemoteTable[lnTable] = Null
  ADEL(oAriaApplication.laRemoteTable,lnTable)
  lnLen = MAX(ALEN(oAriaApplication.laRemoteTable)-1,1)
  DIMENSION oAriaApplication.laRemoteTable[lnLen]
ENDIF

IF USED(lcAlias)
  USE IN &lcAlias.
ENDIF

RETURN
*--end of gfCloseTable 

*!***************************************************************************
*! Name      : gfGetSeq
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2003
*! Purpose   : Get the next sequence number.
*!***************************************************************************
*! Example   : =gfGetSeq()
*!***************************************************************************
*! Notes     : This function was written by MAN.
*!***************************************************************************
*B606902,1
FUNCTION gfGetSeq
PARAMETER lnSeq,cChr
PRIVATE lcSPrefix , lnCharASCI , lnI , lcAlias
lcAlias = ALIAS()
SELECT Sequence
lcSPrefix = ""
IF !(cChr = CHR(0))
  lnCharASCI = ASC(cChr)
  IF MOD(lnCharASCI,26) = 0
    lcChar = "Z"
    lnCharPos = lnCharASCI/26
  ELSE
    lnCharPos = INT(lnCharASCI/26)+1
    lcChar =  chr(mod(lnCharASCI,26)+64)
  ENDIF  
  FOR lnI = 1 TO lnCharPos - 1
    lcSPrefix = lcSPrefix + "Z"
  ENDFOR
  lcSPrefix = lcSPrefix + lcChar

ELSE
 lcChar =""
ENDIF
IF lnSeq = VAL(REPLICATE("9",lnRetLen-LEN(lcSPrefix)))
  IF EMPTY(cChr)
    lcSPrefix = "A"
    *REPLACE cSeq_Chr WITH CHR(1)
    lcChrToUpd = CHR(1)
  ELSE
    IF lcChar = "Z"
      lcSPrefix = lcSPrefix + "A"
    ELSE
      lcSPrefix = LEFT(lcSPrefix,LEN(lcSPrefix)-1) + CHR(ASC(lcChar)+1)
    ENDIF       
    *REPLACE cSeq_Chr WITH CHR(ASC(cSeq_Chr)+1)
    lcChrToUpd = CHR(ASC(cSeq_Chr)+1)
  ENDIF  
  lnSeq = 0
ELSE
  lnSeq = lnSeq + 1
ENDIF

*lnSeq = lnSeq + 1
*REPLACE nSeq_No with lnSeq
*? lcSPrefix+PADL(lnSeq,lnRetLen-LEN(lcSPrefix),"0")

lnRetVal = lnSeq
lcExtraStr = lcSPrefix

IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
*-- End of gfGetSeq.
*MMT22
FUNCTION gfSubStr
PARAMETERS lcString,lnAryOrPos,lcSepta

lcSubstr  =' '
lnAryDim  = 1
lnAryRows = 1
lnAryCols = 1
lcSepta   = IIF(TYPE('lcSepta')='C',lcSepta,',') 

IF LEN(ALLTRIM(lcSepta))>1
  lcColSep  = SUBSTR(lcSepta,2,1)
  lcSepta   = LEFT(lcSepta,1)
  lnAryDim  = IIF(OCCURS(lcSepta,lcString)>0,;
              OCCURS(lcSepta,lcString)+;
              IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
              lnAryDim)
  lnAryCols = IIF(OCCURS(lcColSep,lcString)>0,;
              OCCURS(lcColSep,lcString)+;
              IIF(RIGHT(lcString,1)<>lcColSep,1,0),;
              lnAryDim)
  lnAryRows = (lnAryDim+(lnAryCols-1)) / lnAryCols
  lnAryDim  = lnAryDim +(lnAryCols-1)     
  lcString  = STRTRAN(lcString,lcColSep,lcSepta)
ELSE
  lnAryDim = IIF(OCCURS(lcSepta,lcString)>0,;
             OCCURS(lcSepta,lcString)+;
             IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
             lnAryDim)
ENDIF

*** Chek if second parameter array or numeric
DO CASE
  *** If no parameter found assume firest part of string
  CASE TYPE ('lnAryOrPos')='U'
    lnAryOrPos = 1

  *** If array strich it to hold all string parts
  CASE TYPE ('lnAryOrPos') $ 'C,L'    
    IF lnAryCols > 1
      DIMENSION lnAryOrPos[lnAryRows,lnAryCols]
    ELSE
      IF ALEN(lnAryOrPos,2) > 0
        DIMENSION lnAryOrPos[lnAryDim,ALEN(lnAryOrPos,2)]
      ELSE
        DIMENSION lnAryOrPos[lnAryDim]
      ENDIF  

    ENDIF
    lnAryOrPos  = ' '

ENDCASE

FOR lnArElem  = 1 TO lnAryDim
  IF TYPE ('lnAryOrPos')='N'
    lnArElem = lnAryOrPos
  ENDIF  

  DO CASE
    *** In case of firest string part
    CASE lnArElem = 1
      lcSubstr = SUBSTR(lcString,1,;
      IIF(lcSepta $ lcString,AT(lcSepta,lcString)-1,LEN(lcString)))

    *** In case of last string part
    CASE lnArElem = lnAryDim
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1)
      lcSubstr = IIF(RIGHT(lcSubstr,1)=lcSepta,;
                 SUBSTR(lcSubstr,1,LEN(lcSubstr)-1),lcSubstr)
    *** In case of any string part from the meddel
    CASE lnArElem > 1
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1,;
                 AT(lcSepta,lcString,lnArElem)-;
                 AT(lcSepta,lcString,lnArElem-1)-1)
  ENDCAS

  IF TYPE ('lnAryOrPos')='N'
    RETURN lcSubstr
  ENDIF  
  
  IF lnAryCols > 1
    lnAryOrPos[((lnArElem-1)%lnAryRows)+1,INT((lnArElem-1)/lnAryRows)+1] = lcSubstr
  ELSE
    lnAryOrPos[lnArElem] = lcSubstr
  ENDIF
ENDFOR
*MMT22
*MMT2222
*!********************************************************************
*! Name      : gfAmntDisp
*! Developer : Mohamed Hassan
*! Date      : 12/26/95
*! Purpose   : Return the amount according to the display condition.
*!********************************************************************
*! Parameters: lnAmount     && The amount that you want to display.
*!           : lcRpDispCur  && The way to display the amount.
*!           : ldExRateDt   && If you are going to display the amount
*!           :                 with an exchange rate of a specific date.
*!           : lcTmepFile   && The temp file name that hold the temp. 
*!           :                 exchange rates.
*!           : llAprvCurr   && If you are using the Approved currency.
*!           : lcGetFile    detect which alias we get currency values from it.
*!           : lcGetField   detect which Field we get currency values from it.
*!********************************************************************
*! Call      : From all the AP reports that is using the currency display
*!           : feature.
*!********************************************************************
*! Returns   : lnAmount
*!********************************************************************
*! Example   : gfAmntDisp(APINVHDR.NINVAMNT,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.).
*!********************************************************************
*E301214,1   : Transfer this function from AP Module and AR Module to be a global function.
**********
FUNCTION gfAmntDisp
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile,lcGetField
PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias,lcGetField

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)

lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)
lcGetField  = IIF(TYPE('lcGetField') ='C',lcGetField,'')

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.
lnSavAlias  = SELECT(0)  && Variable to save the alias.
DO CASE
  CASE lcRpDispCur = 'F'

  CASE lcRpDispCur = 'O'
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      *Get filed currency if send . 
       lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,;
                   IIF(EMPTY(lcGetField),&lcGetFile..CCURRCODE,EVAL(lcGetFile+'.'+lcGetField)))
    ENDIF  
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnExRate = 0
    IF EMPTY(lcGetFile)
      lnUnit = NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , DINVDATE , .F.) , NEXRATE)
    ELSE
      lnUnit = &lcGetFile..NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , &lcGetFile..DINVDATE , .F.) , &lcGetFile..NEXRATE)
    ENDIF  
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnUnit <> 0 , lnUnit , 1)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit , 2)

  CASE lcRpDispCur = 'D'
    lnExRate   = 0
    lnUnit     = 0
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,;
                   IIF(EMPTY(lcGetField),&lcGetFile..CCURRCODE,EVAL(lcGetFile+'.'+lcGetField)))
    ENDIF
    IF lcCurrCode = oAriaApplication.BaseCurrency
      lnExRate = 1
      lnUnit   = 1
    ELSE
      lnExRate   = gfChkRate('lnUnit',lcCurrCode,ldExRateDt,.F.)
    ENDIF
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnUnit <> 0 , lnUnit , 1)
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)

  CASE lcRpDispCur = 'N'
    lnExRate   = 0
    lnUnit     = 0
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,;
                   IIF(EMPTY(lcGetField),&lcGetFile..CCURRCODE,EVAL(lcGetFile+'.'+lcGetField)))
    ENDIF  
    IF lcCurrCode = oAriaApplication.BaseCurrency
      lnExRate = 1
      lnUnit   = 1
    ELSE
      IF SEEK(lcCurrCode,lcTmepFile)
        lnExRate = &lcTmepFile..NEXRATE
        lnUnit   = &lcTmepFile..NCURRUNIT
      ENDIF
    ENDIF
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnUnit <> 0 , lnUnit , 1)
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
ENDCASE
SELECT (lnSavAlias)
RETURN lnAmount

FUNCTION gfChkRate
*E300309,1 RENEE 11/15/95. Add a parameter to control the display
*E300309,1                 of the message that is displayed if a 
*E300309,1                 valid exchange rate is not found.
*E300309,1                 parameter : llNoErrMsg
*
*E300336,1 RENEE 01/08/96. Enhance performance as concerning to speed
*E300336,1                 of execution.
*E300309,1 Add parameter llNoErrMsg  
*PARAMETERS lcExUnit,lcCurrency,ldDate,llDispMsg,lcCompID,lcBaseCurr
PARAMETERS lcExUnit,lcCurrency,ldDate,llDispMsg,lcCompID,lcBaseCurr, llNoErrMsg
*E300309,1 end.

LOCAL lnCurDataSess

PRIVATE llExUsedBy,lcOldAlias,lcOldTag,lnRetRate,ldCurrDay,lcOldFlt,;
        llCurUsedBy, lnExRate
lnCurDataSess = SET("Datasession")         
lnRetRate  = 0
lcCompID   = IIF(TYPE('lcCompId') <> 'C' ,oAriaApplication.ActiveCompanyID , lcCompID)
lcBaseCurr = PADR(IIF(TYPE('lcBaseCurr') <> 'C',oAriaApplication.BaseCurrency,lcBaseCurr),3)
lcOldAlias  = SELECT()
llCurUsedBy = .F.
IF lcCurrency = lcBaseCurr
  IF TYPE('lcExUnit') = 'C'
    &lcExUnit = 1
    RETURN 1.0000    
  ENDIF
ENDIF
IF !USED('SYCCURR')
  llExUsedBy=.T.
  SELECT 0
  USE (oAriaApplication.SysPath + 'SYCCURR') 
ELSE
  SELECT SYCCURR
ENDIF

llExUsedBy=.F.
IF !USED('SYCEXCH')
  llExUsedBy=.T.
  SELECT 0
  USE (oAriaApplication.SysPath + 'SYCEXCH') 
ELSE
  SELECT SYCEXCH
ENDIF

lcOldFlt=FILTER()
lcOldTag=TAG()
SET ORDER TO TAG CURRENCY DESCENDING
lcSetNear = SET('NEAR')
SET NEAR ON 
SET FILTER TO
IF SEEK(lcBaseCurr+PADR(lcCurrency,3)+DTOS(ldDate))
  lnRetRate= nExRate
ELSE
  STORE .F. TO LLMULCURR,LLEXCHRATE
  STORE 0 TO LNEXRATDAY
 
  lnNoVar    = gfGetMemVar('LLMULCURR,LLEXCHRATE,LNEXRATDAY' , lcCompID)
  IF cBaseCurr + cCurrCode = lcBaseCurr + PADR(lcCurrency,3) ;
    .AND. dRateDate >= ldDate - lnExRatDay
    lnRetRate = nExRate    
  ELSE
    IF llExchRate AND llDispMsg
      DO FORM SYCHRATE WITH lcCurrency, lcBaseCurr, ldDate,SET("Datasession") TO lnRetRate
      lnRetRate=IIF(TYPE('lnRetRate')<>'N',0,lnRetRate)
    ELSE
      *E300309,1 Display the default error message only if 
      *E300309,1 llDispMsg is .T. and llNoErrMsg is .F.
      *IF llDispMsg
      IF llDispMsg .AND. !llNoErrMsg
      *E300309,1 end.
        ** Message : "The last defined excahnge rate exceeds     "
        **           "the valid number of days."+CHR(13)+CHR(10)+"
        **           "The currency will be defaulted to the base "
        **           "currency.                                  "
        **           "                       ® Ok ¯              "
        =oAriaApplication.MessageBox("TRM00249B00000","DIALOG")
      ENDIF
    ENDIF
  ENDIF  
ENDIF
IF TYPE('lcExUnit') = 'C'
  &lcExUnit = LOOKUP(SYCCURR.NCURRUNIT,lcCurrency,SYCCURR.CCURRCODE,"CCURRCODE")
ENDIF
SELECT SYCEXCH
IF !EMPTY(lcOldTag)
  SET ORDER TO TAG (lcOldTag)
ENDIF
SET FILTER TO &lcOldFlt
IF llExUsedBy
  USE IN SYCEXCH
ENDIF
IF llCurUsedBy
  USE IN SYCCURR
ENDIF

*E300336,1 Restore near settings
SET NEAR &lcSetNear
*E300336,1 end.

SELECT (lcOldAlias)
*!*	SET DATASESSION TO lnCurDataSess
RETURN lnRetRate 

FUNCTION gfGetExSin
PARAMETERS lcUntSin, lcCurrency, lcBaseCurr
LOCAL lcReturn , llClose

IF TYPE('lcUntSin') = 'C' 
  lcUntSin = '/'
ENDIF
IF TYPE('lcBaseCurr') $ 'UL'
  lcBaseCurr = oAriaApplication.BaseCurrency
ENDIF
IF lcCurrency = lcBaseCurr
  RETURN '*'
ENDIF

*-- Hesham (Start)
*-- select data from syccurr remotely and procced the addcurrency
*-- normaly
** This file should always be in use if the system is multi currency,
** Sometimes it is closed by Apparel programs, so, if it is
** not used,open the file.
llClose = .F.
IF !USED('SYCCURR')
  llClose = .T.
  USE (oAriaApplication.SysPath+"SYCCURR") IN 0 ORDER TAG CCURRCODE
ENDIF  
=SEEK(lcBaseCurr,'SYCCURR','CCURRCODE')
lcReturn = IIF((SYCCURR.cCurMeth = 'M' .AND.;
               !(ALLTRIM(lcCurrency) $ SYCCURR.mCurExcept)) .OR.;
               SYCCURR.cCurMeth = 'D' .AND.;
               (ALLTRIM(lcCurrency) $ SYCCURR.mCurExcept) , '*' , '/')
*!*	IF llClose
*!*	  USE IN SYCCURR
*!*	ENDIF

*!*	lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCCURR where ccurrcode='"+lcBaseCurr+"'",'',"SYCCURRTMP","",oAriaApplication.SystemConnectionString,3)
*!*	IF lnRemResult=1
*!*	  LOCATE
*!*		lcReturn = IIF((SYCCURRTMP.cCurMeth = 'M' .AND.;
*!*	               !(ALLTRIM(lcCurrency) $ SYCCURRTMP.mCurExcept)) .OR.;
*!*	               SYCCURRTMP.cCurMeth = 'D' .AND.;
*!*	               (ALLTRIM(lcCurrency) $ SYCCURRTMP.mCurExcept) , '*' , '/')
*!*	  USE IN SYCCURRTMP
*!*	ENDIF

RETURN lcReturn


*MMT2222

FUNCTION gfItemMask
LPARAMETERS lcMaskOrHead, lcDataDr, lcInvType

LOCAL ItemMask
ItemMask = CREATEOBJECT("GetItemMask")
RETURN ItemMask.Do(lcMaskOrHead, lcDataDr, lcInvType)

*!*************************************************************
*! Name : gfGetUOMData
*! Auth : Wael M. Abo-Shawareb (WSH)
*! Date : 07/28/2004.
*!*************************************************************
*! Synopsis : To Get UOM Data from UOM Table
*!*************************************************************
*! Passed Parameters : 
*!      lcRelCode -> UOM-Relation Code     "By Reference"
*!      lcUOMFrom -> Convert from UOM-Code "By Reference"
*!      lcUOMTo   -> Convert to UOM-Code   "By Reference"
*!      lnConv    -> Conversion Factor     "By Reference"
*!      llDispMsg -> Flag to Display "Add, Browse, Reenter" message
*!                   or not if Relation not found. "By Value"
*!*************************************************************
*! Returned : 1 -> The User Select Add But Relation not Added.
*!            2 -> Relation Added, Found, or the User Browse and select Relation.
*!            3 -> The User Choose Reenter or Relation not found and not added
*!*************************************************************
*! Example :
*!        =gfGetUOMData(@lcRelCode, @lcUOMBuy, @lcUOMUse, @lnConv, .T.)
*!*************************************************************
*! E038220,1 WSH
FUNCTION gfGetUOMData
LPARAMETERS lcRelCode, lcUOMFrom, lcUOMTo, lnConv, llDispMsg

LOCAL lnRetVal, llCodesOpened, lcOldAlias, lcWhereCond, lcDefUOM, lnHandler, llAddRel, llBrowseRel
llCodesOpened = .F.
lnRetVal      = 3
lcOldAlias    = SELECT()
lcDefUOM      = SPACE(6)
llAddRel      = .F.
llBrowseRel   = .F.

IF EMPTY(lcRelCode) AND (lcUOMFrom == lcUOMTo) AND !EMPTY(lnConv) AND (lnConv <> 1)
  *--Can not create a Relation between a UOM Code and itself.
  =gfModalGen('QRM00414B42001', 'DIALOG')
  SELECT (lcOldAlias)
  RETURN 3
ENDIF

LOCAL lcUOMCurs
lcUOMCurs = gfTempName()

DIMENSION laUOMData[4]
laUOMData = ''

LOCAL lcUOMCode, lcUOM_B, lcUOM_V, lnConf
lcUOMCode = lcRelCode
lcUOM_B   = lcUOMFrom
lcUOM_V   = lcUOMTo
lnConf    = IIF(EMPTY(lcRelCode), lnConv, 0)
llDispMsg = IIF(EMPTY(lcRelCode), llDispMsg, .F.)

IF !USED('Codes')
  llCodesOpened = gfOpenFile(oAriaApplication.Datadir + 'CODES', 'CCODE_NO', 'SH')
ENDIF
lcDefUOM = IIF(SEEK("DCUNTOFMGR", "CODES", "CCODE_NO"), CODES.cCode_No, "EAC   ")

*--Open UOM Cursor based on passed parameters.
LOCAL lcUOMTag

IF !EMPTY(lcUOMCode)
  lcWhereCond = "CUOMCODE = '" + lcUOMCode + "'"
  lcUOMTag = 'UOMCODE'
ELSE
  lcUOM_B = IIF(!EMPTY(lcUOM_B), lcUOM_B, lcDefUOM)
  lcUOM_V = IIF(!EMPTY(lcUOM_V), lcUOM_V, lcDefUOM)
  lcWhereCond = "CUOM_B = '" + lcUOM_B + "'" +;
                " AND CUOM_V = '" + lcUOM_V + "'" +;
                  IIF(!EMPTY(lnConf), " AND NCONF = " + STR(lnConf), "")
  lcUOMTag = 'UOM'
ENDIF
IF !gfOpnSqlFl("UOM", lcUOMCurs, lcWhereCond, '', "UOMCODE")
  SELECT (lcOldAlias)
  RETURN 3
ENDIF

*--Case of one Relation found
IF RECCOUNT(lcUOMCurs) = 1
  lcRelCode = EVALUATE(lcUOMCurs + '.cUOMCode')
  lcUOMFrom = EVALUATE(lcUOMCurs + '.cUOM_B')
  lcUOMTo   = EVALUATE(lcUOMCurs + '.cUOM_V')
  lnConv    = EVALUATE(lcUOMCurs + '.nConF')
  lnRetVal  = 2
ELSE
  IF llDispMsg
    *--Display Query Message based on Current Situation.
    LOCAL lnSelect, llWasSel
    
    IF EOF(lcUOMCurs)
      IF EMPTY(lnConf)
        *--No Relations Found Between <UOM_B> and <UOM_V>
        *--  "Add,Reenter"
        lnSelect = gfModalGen('TRM00411B42006', 'DIALOG', RTRIM(lcUOM_B) + '|' + RTRIM(lcUOM_V))
        IF lnSelect = 2   && Reenter
          lnSelect = 3
        ENDIF
      ELSE
        *--No Relations Found Between <UOM_B> and <UOM_V> with Conversion <lnConf> 
        *--  "Add,Browse,Reenter"
        lnSelect = gfModalGen('TRM00412B42003', 'DIALOG', RTRIM(lcUOM_B) + '|' + RTRIM(lcUOM_V) + '|' + ALLTRIM(STR(lnConf)))
      ENDIF
    ELSE
      *--More than one Relation Found Between <UOM_B> and <UOM_V>
      *--  "Add,Browse,Reenter"
      lnSelect = gfModalGen('TRM00413B42003', 'DIALOG', RTRIM(lcUOM_B) + '|' + RTRIM(lcUOM_V))
    ENDIF
    
    DO CASE
      *--Case Select Add
      CASE lnSelect = 1
        llAddRel = .T.
      *--Case Select Browse
      CASE lnSelect = 2
        lcWhereCond = "CUOM_B = '" + lcUOM_B + "'" +;
                      " AND CUOM_V = '" + lcUOM_V + "'"
        
        IF EMPTY(lnConf) OR gfOpnSqlFl("UOM", lcUOMCurs, lcWhereCond, '', "UOM")
          llBrowseRel = .T.
        ENDIF
    ENDCASE
  ELSE
    IF EOF(lcUOMCurs)
      IF EMPTY(lnConf)
        *--Nothing to ADD or Browse for.
        *IF !EMPTY(lcUOMCode)
        IF EMPTY(lcUOMCode)
          *=gfModalGen('TRM00415B42001', 'DIALOG')
        *ELSE
          =gfModalGen('TRM00411B42001', 'DIALOG', RTRIM(lcUOM_B) + '|' + RTRIM(lcUOM_V))
        ENDIF
      ELSE
        llAddRel = .T.
      ENDIF
    ELSE
      IF EMPTY(lcUOMCode) AND EMPTY(lnConf)
        llBrowseRel = .T.
      ENDIF
    ENDIF
  ENDIF
ENDIF

*--Relation not found and not added then browse.
IF llBrowseRel
  IF !EOF()
    PRIVATE lcBrFields
    
    *--Browse UOMs
    *lcBrFields = "CUOMCODE:H='"+LANG_ARIA_UOM_REL+"':22,lcUOM_B=gfCodDes(CUOM_B,'CUNTOFMGR'):H='"+LANG_ARIA_UOM_B+"':22,"+;
                 "lcUOM_V=gfCodDes(CUOM_V,'CUNTOFMGR'):H='"+LANG_ARIA_UOM_V+"':22,NCONF:H='"+LANG_ARIA_UOM_CONF+"':22"
    lcBrFields = "lcUOM_B=gfCodDes(CUOM_B,'CUNTOFMGR'):H='"+LANG_ARIA_UOM_B+"':22,"+;
                 "lcUOM_V=gfCodDes(CUOM_V,'CUNTOFMGR'):H='"+LANG_ARIA_UOM_V+"':22,"+;
                 "NCONF:H='"+LANG_ARIA_UOM_CONF+"':25"

    SELECT (lcUOMCurs)
    llWasSel = ARIABROW('', LANG_ARIA_UOM, .F., .F., .F., .F., '', '', 'CUOMCODE,CUOM_B,CUOM_V,NCONF', 'laUOMData')
    
    *--Fill UOM Variables if Relation Code Selected
    IF llWasSel
      lcRelCode = laUOMData[1]
      lcUOMFrom = laUOMData[2]
      lcUOMTo   = laUOMData[3]
      lnConv    = laUOMData[4]
      lnRetVal  = 2
    ENDIF
  ELSE
    *--Nothing to ADD or Browse for.
    =gfModalGen('TRM00415B42001', 'DIALOG')
  ENDIF
ENDIF

IF llAddRel
  LOCAL lcTranCode
  
  *--Add Record to UOM Table if we have a conversion factor.
  IF !EMPTY(lnConf)
    LOCAL lcStatement, lcTempCurs, lcTopRel
    lcTempCurs = gfTempName()
    
    *--Get Last Relation Code Value to Set New One.
    lcStatement = "SELECT TOP 1 CUOMCODE FROM UOM (INDEX=UOMCODE) ORDER BY CUOMCODE DESC"
    lnHandlar = oAriaApplication.RemoteCompanyData.SQLRun(lcStatement, lcTempCurs, "UOM",;
                oAriaApplication.ActiveCompanyConStr, 3, 'SAVE', SET("Datasession"))

    IF lnHandlar = 1
      *--Set the Highest Relation Code in UOM file
      lcTopRel = STRTRAN(STR(VAL(EVALUATE(lcTempCurs + '.CUOMCODE')) + 1, 6, 0), ' ', '0')
      
      *--Add record in UOM table for new Relation.
      INSERT INTO (lcUOMCurs) (CUOMCODE, CUOM_B, CUOM_V, NCONF) VALUES (lcTopRel, lcUOM_B, lcUOM_V, lnConf)
      =gfAdd_info(lcUOMCurs)
      
      IF gfUpdSqlFl(lcUOMCurs, 'CUOMCODE', 'UOM')
        lcRelCode = lcTopRel
        lcUOMFrom = lcUOM_B
        lcUOMTo   = lcUOM_V
        lnConv    = lnConf
        lnRetVal  = 2
      ENDIF
    ELSE
      =oAriaApplication.RemoteCompanyData.CheckRetResult("SQLRUN", lnHandlar, .T.)
    ENDIF
          
    USE IN (lcTempCurs)
  ELSE
    lnRetVal = 1
  ENDIF
ENDIF

USE IN (lcUOMCurs)
IF llCodesOpened

  gfCloseTable("CODES")
ENDIF

SELECT (lcOldAlias)
RETURN lnRetVal


FUNCTION gfOpnSqlFl

*!B999999,1 WSH 02/07/2005, Add Parameter to allow opening Native Files Remotely. [Start]
*LPARAMETERS lcTable,lcCursor,lcWhereCond,laIndex,lcTagName
LPARAMETERS lcTable, lcCursor, lcWhereCond, laIndex, lcTagName, llNative
*!B999999,1 WSH 02/07/2005, [End]

*!B999999,1 WSH 02/07/2005, Add Variable to allow opening Native Files Remotely. [Start]
*--Get Connection String Type
IF llNative
  lcConnString = oAriaApplication.cAriaNativeDataFilesConStr
ELSE
  lcConnString = oAriaApplication.ActiveCompanyConStr
ENDIF
*!B999999,1 WSH 02/07/2005, [End]

LOCAL lnConnectionHandlar, lcODBC, lcUserName, lcPassWord, lnBuffering, lcSqlStatment

*!B999999,1 WSH 02/07/2005, Don't use index if it is not passed. [Start]
*lcSqlStatment = "SELECT * FROM " + lcTable + " (INDEX="+lcTagName+")" + " WHERE "+lcWhereCond

*B131608,1 WSH 05/03/2006 No "Index" if it is native. [Start]
*lcSqlStatment = "SELECT * FROM " + lcTable + IIF(TYPE("lcTagName") = 'C' AND !EMPTY(lcTagName), " (INDEX="+lcTagName+")", "") + " WHERE "+ lcWhereCond
lcSqlStatment = "SELECT * FROM " + lcTable + IIF(!llNative AND TYPE("lcTagName") = 'C' AND !EMPTY(lcTagName), " (INDEX="+lcTagName+")", "") + " WHERE "+ lcWhereCond
*B131608,1 WSH 05/03/2006 [End]

*!B999999,1 WSH 02/07/2005, [End]

lcODBC        = 'DS'+oAriaApplication.ActiveCompanyID
lcUserName    = 'sa'
lcPassWord    = ''

*!B999999,1 WSH 02/07/2005, Add Variable to allow opening Native Files Remotely. [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
                                      'SAVE',SET("DATASESSION"))

*B131608,1 WSH 05/03/2006 Pass the Table Name to SQLRun method. [Start]
*lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,"",lcConnString,3,;
                                      'SAVE',SET("DATASESSION"))
lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,lcTable,lcConnString,3,;
                                      'SAVE',SET("DATASESSION"))
*B131608,1 WSH 05/03/2006 [Start]

*!B999999,1 WSH 02/07/2005, [End]

IF lnConnectionHandlar = 1
  lnBuffering = CURSORGETPROP("Buffering",lcCursor)
  =CURSORSETPROP("Buffering",3,lcCursor)
  IF !EMPTY(laIndex)
    FOR lnCntr = 1 TO ALEN(laIndex,1)
      lcIndex = laIndex[lnCntr,1]
      lcTag   = laIndex[lnCntr,2]
      lcUnique = laIndex[lnCntr,3]
      IF TYPE("lcUnique") = "C"
        INDEX ON &lcIndex. &lcUnique. TAG (lcTag) OF (lcCursor)
      ELSE
        INDEX ON &lcIndex. TAG (lcTag) OF (lcCursor)
      ENDIF
    ENDFOR
    lcTag = laIndex[1,2]
    SET ORDER TO TAG (lcTag)
  ENDIF
  =CURSORSETPROP("Buffering",lnBuffering,lcCursor)
ELSE
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlrun",lnConnectionHandlar,.T.)
  RETURN .F.
ENDIF


*!*************************************************************
*! Name : gfUpdSqlFl (037923)
*! Auth : Khalid Mohi El-Din Mohamed (KHM)
*! Date : 04/01/2004.
*!*************************************************************
*! Synopsis : To update the SQL tables
*!*************************************************************
*! Passed :
*!        Parameters : 
*!        lcTable 		   -> SQL table
*!        LcPrimaryKeyList -> List of the index fields.
*!        lcSQLTable 	   -> SQL table
*!*************************************************************
*! Example :
*!        DO gfUpdSqlFl WITH "ITEM","cInvType,Style"
*!*************************************************************
FUNCTION gfUpdSqlFl
LPARAMETERS lcTable,LcPrimaryKeyList, lcSQLTable

LOCAL lnConnectionHandlar, lcTranCode, lcODBC, lcUserName, lcPassWord, llReturn

lcODBC     = 'DS'+oAriaApplication.ActiveCompanyID
lcUserName = 'sa'
lcPassWord = ''
llReturn   = .T.

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')

IF TYPE('lcTranCode') = 'N'
  =oAriaApplication.RemoteCompanyData.CheckRetResult("BeginTran",lcTranCode,.T.)
  IF BETWEEN(lnRecNo,1,RECCOUNT(lcTable))
    GOTO lnRecNo IN (lcTable)
  ENDIF
  RETURN .F.
ENDIF

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.sqlupdate(lcTable,lcTranCode,SET("DATASESSION"),LcPrimaryKeyList,lcSQLTable)
IF lnConnectionHandlar # 1 .AND. lnConnectionHandlar # 2
  =oAriaApplication.RemoteCompanyData.CheckRetResult("sqlupdate",lnConnectionHandlar,.T.)
  llReturn = .F.
ENDIF

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.CheckRetResult("CommitTran",lnConnectionHandlar,.T.)
  llReturn = .F.
ENDIF
RETURN llReturn

*!************************************************************
*! Name      : gfDoTriger
*! Developer : WAB - Walid A. Wahab
*! Date      : 04/22/2002
*! Purpose   : Function to control any triggers found in the
*!             triggers file, customized processes and workflow
*!             server requests.
*!*************************************************************
*! Calls              : None.
*!*************************************************************
*! Passed Parameters  : 1) lcProgName, Object ID.
*!                      2) lcEvent, Event ID.
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example            :  =gfDoTriger()
*!*************************************************************
*E301903,1 WAB 
*!*************************************************************
*
FUNCTION gfDoTriger
PARAMETERS lcProgName , lcEvent

PRIVATE lnOldAlias , lcProgToDo , laParamExp , laParam , lcParmStr ,;
        lnCount    , llReturn , llIsOpen
llReturn = .T.
*-- If any of the parameters is not passed or passed incorrectly 
IF TYPE('lcProgName') <> 'C' .OR. EMPTY(lcProgName) .OR.;
   TYPE('lcEvent') <> 'C' .OR. EMPTY(lcEvent)
  RETURN
ENDIF

*-- Save the old alias
lnOldAlias = SELECT(0)

*-- Open the Trigger file if it was not opened
*-- Hesham (Start)
*-- Use trigger file remotely in temprory alias
*!*	llIsOpen = .F.
*!*	IF !USED('SYCTRIGG')
*!*	    SELECT 0
*!*	    *** Open the
*!*	    USE (oARiaApplication.SysPath+"SYCTRIGG")
*!*	    SET ORDER TO 1
*!*	ENDIF

*!*	SELECT SYCTRIGG

lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYCTRIGG where cAPObjNam='"+PADR(lcProgName , 10)+"' AND cEvent_ID ='"+PADR(lcEvent , 10)+"'",'',"SYCTRIGGTMP","",oAriaApplication.SystemConnectionString,3,'',SET("DATASESSION"))
IF lnRemResult>=1  AND RECCOUNT() > 0
  LOCATE
*-- Hesham (End)  

*-- If there is triggers for this Object/Event
*IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))
  IF FOUND()
  *-- Scan loop to scan the Object/Event triggers
    SCAN REST;
        WHILE cAPObjNam + cEvent_ID = PADR(lcProgName , 10) +;
              PADR(lcEvent , 10)
    
      *-- Get the name of the program that should be executed
      lcProgToDo = cTrig_ID
      *-- Initialize the parameter string variable
      lcParmStr  = ''
    
      *-- Restore the old alias to be able to evaluate the parameter
      *-- expressions properly
      SELECT (lnOldAlias)
    
      *-- If there is one or more parameters that should be passed to the
      *-- program
      IF !EMPTY(SYCTRIGGTMP.mParmExpr)
      
        *-- Get the parameter expressions
        DIMENSION laParamExp[OCCURS('~' , SYCTRIGGTMP.mParmExpr) + 1]
        =gfSubStr(SYCTRIGGTMP.mParmExpr , @laParamExp , '~')
      
        *-- Initialize the parameters array
        DIMENSION laParam[ALEN(laParamExp , 1)]
        laParam = ""
      
        *-- Get the parameters values that will be passed to the program
        FOR lnCount = 1 TO ALEN(laParamExp , 1)
          laParam[lnCount] = EVALUATE(laParamExp[lnCount])
          lcParmStr = lcParmStr + IIF(lnCount = 1 , '' , ' , ') +;
                      'laParam[' + ALLTRIM(STR(lnCount)) + ']'
        
        ENDFOR    && End of FOR lnCount = 1 TO ALEN(laParamExp , 1)
      ENDIF    && End of IF !EMPTY(SYCTRIGG.mParmExpr)
      lcParmStr = "''"+IIF(!EMPTY(lcParmStr),",","")+lcParmStr && Wael
      *-- If custom process
      *Hassan [Begin]
      lcOldPath = FullPath('')
      lcNewPath = SubStr(oAriaApplication.ApplicationHome,1,Rat("\",oAriaApplication.ApplicationHome,2))
      CD (lcNewPath) 
      *Hassan [End]
      IF SYCTRIGGTMP.cActvTyp = 'C'
        *-- Call the program and get the returned value
        llReturn = &lcProgToDo(&lcParmStr)
      ENDIF    && End of IF SYCTRIGG.cActvTyp = 'C'
      *Hassan [Begin]
      CD (lcOldPath) 
      *Hassan [End]
      SELECT SYCTRIGGTMP
    ENDSCAN    && End of SCAN REST WHILE cAPObjNam + cEvent_ID = ...
  ENDIF

ELSE    &&  *In case the process doesn't exist.[START]
  llReturn = .F.
  
ENDIF    && End of IF SEEK(PADR(lcProgName , 10) + PADR(lcEvent , 10))
IF USED("SYCTRIGGTMP")
  USE IN SYCTRIGGTMP
ENDIF
*-- Restore the old alias
SELECT (lnOldAlias)

RETURN (llReturn)

*N000635,1 MMT 10/19/2009 Convert Print Check report to Aria4[Start]
*!*************************************************************
*! Name      : gfObj_Lock
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose   : Function to logicaly lock a record
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  Lock or unlock
*!                       
*!*************************************************************
*! Returns            :  .T. --> succeded
*!                       .F. --> unsuccess
*!*************************************************************
*! Example            :  gfObj_Lock(.T.)
*!*************************************************************
FUNCTION gfObj_Lock
PARAMETERS lLok_Set
PRIVATE lnRecNo,lRet_Flag
lnWorkArea = ALIAS()
IF EMPTY(lnWorkArea)
  RETURN
ENDIF
PRIVATE lnOldrpSt
SELECT (lnWorkArea)
lnDataSession = SET("DATASESSION")
lnAlias = SELECT()
lRet_Flag = .F.
lLok_It   = .F.
llLocked  = .F.
*** Go to the same record to get a fresh copy in the buffer
lnRecNo = RECNO()

DO WHILE .T.
  SELECT (lnWorkArea)
  IF lnRecNo <= RECCOUNT()
    GO lnRecNo
   llLocked = RLOCK() 
   UNLOCK RECORD lnRecNo
   IF llLocked
     TABLEREVERT(.F.)
   ENDIF 
   IF DELETED()
     *=gfModalGen('INM00095B00000','ALERT')
     SELECT (lnAlias)
     *THIS.CHangemode("S")
     RETURN .F.
   ENDIF
  ENDIF  

  *** Chek if the record is in use by another user
  IF lLok_Set 
    *** Chek if the field cLok_User in the structur
    IF !lLok_Stat .AND. llLocked
      *** Record is not locked you may lock it
      lLok_It   = .T.
    ELSE
      lcLok_User = cLok_User
      IF !EMPTY(lcLok_User)
        IF ALLTRIM(lcLok_User) = ALLTRIM(oAriaEnvironment.User_ID)
          * Messaging the user that he cannot edit the same record
          * from more than one session and permit him from editing
          * the same record
*!*	          IF gfModalGen("INM00240B00006","ALERT")=2
            lLok_It    = .F.
            lRet_Flag  = .F.
*!*	          ELSE      
*!*	            lLok_It    = .T.
*!*	          ENDIF
        ELSE

          *We save old value of reprocess first.[START]
          lnOldrpSt = SET('REPROCESS')
          SET REPROCESS TO 1
          
  		  SET DATASESSION TO 1
		  llLoop = .F.
		  IF !USED('syuStatc')
		    =gfOpenFile('syuStatc','CUSER_ID','SH')
		  ENDIF 
		  SELECT syuStatc
          IF SEEK ('INI'+'OLDVARS'+lcLok_User,'syuStatc') 
            LOCAL lnStatcRec
            SCAN REST WHILE cobj_typ+ALLTRIM(cobj_name)+cuser_id = 'INI'+'OLDVARS'+lcLok_User
                lnStatcRec = RECNO()
	            IF RLOCK('syuStatc')
  	            UNLOCK RECORD lnStatcRec IN  syuStatc 
        	      lLok_It    = .T.  	            
*!*	  	            lnStatcRec = RECNO()
*!*	    	          GO (oAriaApplication.UserStaticRecord) IN syuStatc 
*!*	      	        =RLOCK('syuStatc')
*!*	        	      GO lnStatcRec
          	  ELSE
            	  UNLOCK
              	 GO (oAriaEnvironment.UserStaticRecord) IN syuStatc 
              	=RLOCK('syuStatc') 
              	*** Display the message "Record is in use by user AAAA"
              	lcLok_User = oAriaEnvironment.getUserName(lcLok_User)
              	*** Record is in use by user ????    
              	SET DATASESSION TO (lnDataSession)
*!*	              	IF  gfModalGen("INM00028B00015","ALERT",lcLok_User) = 1
*!*	                  llLoop = .T.
*!*	              	ENDIF  
              	lLok_It    = .F.
              	lRet_Flag  = .F.
              	EXIT 
            	ENDIF
           ENDSCAN 	
          ELSE
            lLok_It    = .T. 
          ENDIF
          * Return the old value of reprocess.
          SET REPROCESS TO  lnOldrpSt
          SET DATASESSION TO (lnDataSession)
          IF llLoop
            LOOP 
          ENDIF 

        ENDIF
      ELSE
        *** Display the message "Record is in use by another"
        SET DATASESSION TO (lnDataSession)
*!*	        IF gfModalGen("INM00029B00015","ALERT") = 1
*!*	          LOOP
*!*	        ENDIF  
        lLok_It    = .F.
        lRet_Flag  = .F.
      ENDIF   
    ENDIF

  ELSE
    *** Chek if these three field in the file structur
    IF TYPE ('cLok_User') <> "U" .AND. ;
       TYPE ('dLok_Date') <> "U" .AND. ;
       TYPE ('cLok_Time') <> "U" 

      *** Unlock the record
      REPLACE lLok_Stat WITH .F. , ;   
              cLok_User WITH ""  , ;
              dLok_Date WITH {}  , ;
              cLok_Time WITH ""
			=TABLEUPDATE(0,.T.)              
      lRet_Flag  = .T.
    ENDIF  
  ENDIF

  EXIT
ENDDO

*** Chek if you have to lock the record or not
SET DATASESSION TO (lnDataSession)
IF lLok_It  
  *** Chek if these three field in the file structur
  IF TYPE ('cLok_User') <> "U" .AND. ;
     TYPE ('dLok_Date') <> "U" .AND. ;
     TYPE ('cLok_Time') <> "U" 
    *** Lock the record for this user with date and time
    REPLACE lLok_Stat WITH .T.       , ;   
             cLok_User WITH oAriaApplication.User_ID , ;
             dLok_Date WITH DATE()    , ;
             cLok_Time WITH gfGetTime()
    =TABLEUPDATE(0,.T.)
    lRet_Flag  = .T.    
  ENDIF
ENDIF
SELECT (lnWorkArea)
UNLOCK
SELECT (lnAlias)

RETURN lRet_Flag
*!*************************************************************
*! Name        : gfTableUpdate
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose     : Called Instead of the TableUpdate Function
****************************************************************************
*! Parameters  : Paremeters of the TableUpdate Function
*!*************************************************************
*! Returns     : TableUpdate Function Result
*!*************************************************************
FUNCTION gfTableUpdate
LPARAMETERS llForce, lcAlias, lcError
LOCAL lnTable, lcOldAlias, lcTranCode, llRetValue

lcOldAlias = ALIAS()
lcAlias = IIF(TYPE('lcAlias')='C',lcAlias,ALIAS())
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

llRetValue = .T.
IF lnTable<>0 && Remote Table Object was Found
  IF TYPE('llForce')<>'C'
    lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
    IF TYPE('lcTranCode') = 'N'
      llRetValue = .F.
    ENDIF
  ELSE
    lcTranCode = llForce
  ENDIF
  llRetValue = llRetValue AND oAriaApplication.laRemoteTable[lnTable].TableUpdate(lcTranCode)
  IF TYPE('llForce')<>'C' AND TYPE('lcTranCode')='C'
    IF llRetValue
      llRetValue = (oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)=1)  AND llRetValue
    ELSE
      llRetValue = (oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)=1)
    ENDIF
  ENDIF
ELSE
  LOCAL lcParam
  lcParam = IIF(TYPE('llForce') = 'L', PADR(llForce,3), '.T.') + ', '+;
            IIF(!EMPTY(lcAlias), lcAlias ,' .F.')+ ', '+;
            IIF(!EMPTY(lcError), lcError,' .F.')
  
  llRetValue = TableUpdate(&lcParam.)
ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN llRetValue
*--end of gfTableUpdate
*!*************************************************************
*! Name        : gfReplace
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose     : Called Instead of the Replace Statement
****************************************************************************
*! Parameters  : Replace Statement
*!*************************************************************
*! Returns     : None
*!*************************************************************
FUNCTION gfReplace
LPARAMETERS lcStat
LOCAL lnTable, lcAlias, lcOldAlias

lcOldAlias = ALIAS()
lcAlias = gfGetAlias(@lcStat)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 AND !oAriaApplication.laRemoteTable[lnTable].llNative && Remote Table Object was Found and the Table is SQL
  IF TYPE('lcStat')='C' AND !EMPTY(lcStat)
    oAriaApplication.laRemoteTable[lnTable].Replace(lcStat)
  ELSE
    oAriaApplication.laRemoteTable[lnTable].Replace()
  ENDIF
ENDIF
SELECT (lcAlias)

IF TYPE('lcStat')='C' AND !EMPTY(lcStat)
  REPLACE &lcStat
ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN
*--end of gfReplace
*!*************************************************************
*! Name        : gfSqlRun
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose     : Called Instead of the Locate Command
****************************************************************************
*! Parameters  : SQL Select Statement, Cursor Receiving Results, Ignore Creating Index
*!*************************************************************
*! Returns     :  False if Falied to Create the Cursor
*!*************************************************************
FUNCTION gfSqlRun
LPARAMETERS lcSqlStatement, lcAlias, llNoIndex, lcTempAlias
LOCAL lnTable

lcAlias = IIF(TYPE('lcAlias')='C',lcAlias,ALIAS())
lcTempAlias = IIF(TYPE('lcTempAlias')='C',lcTempAlias,lcAlias)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  RETURN oAriaApplication.laRemoteTable[lnTable].SqlRun(lcSqlStatement, lcTempAlias, llNoIndex)
ELSE
  RETURN .F.
ENDIF

RETURN
*--end of SqlRun
*!*************************************************************
*! Name        : gfAppend
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose     : Called Instead of the Append Blank Statement
****************************************************************************
*! Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
FUNCTION gfAppend

LPARAMETERS lcStat, llFormMemVar


LOCAL lnTable, lcAlias, lcOldAlias

lcOldAlias = ALIAS()
lcAlias = gfGetAlias(@lcStat)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 AND !oAriaApplication.laRemoteTable[lnTable].llNative && Remote Table Object was Found and the Table is SQL
  SELECT (oAriaApplication.laRemoteTable[lnTable].lcCursorUpdate)
  APPEND BLANK
  
  *N039550,1 WSH 09/18/2005 add new parameter to append from memvar. [Start]
  IF llFormMemVar
    GATHER MEMVAR MEMO
  ENDIF
  *N039550,1 WSH 09/18/2005 [End]
  
ENDIF
SELECT (lcAlias)
APPEND BLANK 

*N039550,1 WSH 09/18/2005 add new parameter to append from memvar. [Start]
IF llFormMemVar
  GATHER MEMVAR MEMO
ENDIF
*N039550,1 WSH 09/18/2005 [End]

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN
*--end of gfAppend
*!*************************************************************
*! Name        : gfSetOrder
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose     : Called Instead of the Set Order To Command
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : Index Tag
*!*************************************************************
*! Returns     :  None
*!*************************************************************
FUNCTION gfSetOrder
LPARAMETERS lcStat
LOCAL lnTable, lcAlias, lcOldAlias

lcOldAlias = ALIAS()
lcAlias = gfGetAlias(@lcStat)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAlias)

IF lnTable<>0 && Remote Table Object was Found
  oAriaApplication.laRemoteTable[lnTable].SetOrder(lcStat)
ELSE
  SELECT (lcAlias)
  IF TYPE('lcStat')='C' AND !EMPTY(lcStat)
    SET ORDER TO &lcStat
  ELSE
    SET ORDER TO
  ENDIF
ENDIF

IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF

RETURN
*--end of gfSetOrder
*!*************************************************************
*! Name        : gfGetAlias
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose     : Extracts Alias Name from a Statement
****************************************************************************
*! Parameters  : Statment
*!*************************************************************
*! Returns     :  Alias Name
*!*************************************************************
FUNCTION gfGetAlias
LPARAMETERS lcStat
LOCAL lcAlias
DO CASE
  CASE TYPE('lcStat')='C' AND (' IN ' $ UPPER(lcStat) OR UPPER(lcStat)='IN ')
    *! B040241,1 ASM 05/18/2006 Bug in gfReplace, fixed in gfGetAlias [Start]
    *lcAlias = ALLTRIM(SUBSTR(lcStat,AT(' IN ',UPPER(lcStat))+4))
    lcAlias = ALLTRIM(SUBSTR(lcStat,RAT(' IN ',UPPER(lcStat))+4))
    *lcStat = LEFT(lcStat,AT(' IN ',UPPER(lcStat))-1)
    IF "'" $ lcAlias or '"' $ lcAlias or "]" $ lcAlias
       lcAlias = ALIAS()
    ELSE
      lcStat = LEFT(lcStat,RAT(' IN ',UPPER(lcStat))-1)
    ENDIF
    *! B040241,1 ASM 05/18/2006 Bug in gfReplace, fixed in gfGetAlias [End]
  OTHERWISE
    lcAlias = ALIAS()
ENDCASE

RETURN lcAlias
*--end of gfGetAlias
*!*************************************************************
*! Name      : gfGetTime
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose   : get the current time with am,pm format
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : current time
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
*:->
FUNCTION gfGetTime

lcCurrHour = IIF(VAL(SUBSTR(TIME(),1,2))=12 OR VAL(SUBSTR(TIME(),1,2))=0,;
             '12',ALLTRIM(STR(VAL(SUBSTR(TIME(),1,2))%12)))
             
lcCurrTime = IIF(VAL(lcCurrHour)<10,'0','')+lcCurrHour+;
             SUBSTR(TIME(),3)+IIF(VAL(SUBSTR(TIME(),1,2))>=12,' pm',' am')
             
RETURN (lcCurrTime)             
*!*************************************************************
*! Name      : gfRlock
*! Developer : Mariam Mazhar
*! Date      : 10/19/2009
*! Purpose   : Phisycal lock to record in one or more files
*!*************************************************************
*! Calls     : 
*!          Calls: GFSUBSTR()               (function  in ARIA3.PRG)
*!          Calls: GFMODALGEN()             (function  in ARIA3.PRG)
*!*************************************************************
*! Passed Parameters  : Name of files
*!                      Lock or unlock 
*!                      Number of attempts    
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : 
*!*************************************************************
* This function will atempt to lock multiple files and let the user to retry
* agian till he select to cancel the operation
* parameters : Files list to lock
*              Flag to lock or unlock the files
*
*:->
FUNCTION gfRlock
PARAMETERS lcFls2lock,llLokUnlok,lnTry

lcFls2lock = IIF(TYPE('lcFls2lock')<>'C',ALIAS(),lcFls2lock)
llLokUnlok = IIF(TYPE('llLokUnlok')='U',.F.,llLokUnlok)
lnTry      = IIF(TYPE('lnTry')     ='N',lnTry,gnLockTry) 
llRetFlag  = .F.
llRetry    = .F.

*** If no files was sent return with .F.
IF EMPTY(lcFls2lock)
  RETURN llRetFlag
ENDIF

*** Put files to be record locked in array
DECLARE laFls2lock[1]
IF ',' $ lcFls2lock 
  =gfSubStr(lcFls2lock,@laFls2lock,',')
ELSE
  laFls2lock[1] = lcFls2lock
ENDIF  


SET REPROCESS TO lnTry

*** Lock one or multiple files
IF llLokUnlok
  *** Keep tring till the user decide to cancle
  DO WHILE .T.
    *** Loop to lock all files to be locked
    FOR lnFCount =  1 TO ALEN(laFls2lock,1)
      *** If files was locked all return with .T.
      IF RLOCK(laFls2lock[lnFCount])
        llRetFlag = .T.
      ELSE
        *** Give the user message to retry or cancel
        *** Files is in use by another user
*!*	        IF gfModalgen("QRM00109B00015","ALERT",UPPER(ALLTRIM(laFls2lock[lnFCount]))) = 1
*!*	          llRetFlag = .F.
*!*	          llRetry   = .T.
*!*	          *** Exit from the for loop and retry again
*!*	          EXIT
*!*	        ELSE
          llRetFlag = .F.
          llRetry   = .F.
          *** Exit from the for loop and quit function
          EXIT
*!*	        ENDIF
      ENDIF
    ENDFOR
    *** If at least one files was not locked
    IF !llRetFlag 
      *** If the user select to retry loop again
      IF llRetry 
        LOOP
      *** If not quit the function with .F.
      ELSE
        *** If cancel unlock in all alias 
        FOR lnFCount =  1 TO ALEN(laFls2lock,1)
          UNLOCK IN (laFls2lock[lnFCount])
        ENDFOR
        llRetFlag = .F.    
        EXIT
      ENDIF
    ELSE
      *** If all lockes went ok terminat the loop
      EXIT
    ENDIF  
  ENDDO

*** Unlock multiple files 
ELSE
  FOR lnFCount =  1 TO ALEN(laFls2lock,1)
    UNLOCK IN (laFls2lock[lnFCount])
  ENDFOR
  llRetFlag = .T.
ENDIF

SET REPROCESS TO gnLockTry

RETURN llRetFlag
*N000635,2 MMT 11/16/2009 Convert Some Function to Aria5[Start]
FUNCTION gfAmntDisp
PARAMETER lnAmount,lcRpDispCur,ldExRateDt,lcTmepFile,llAprvCurr,lcGetFile,lcGetField
PRIVATE lnAmount,lcRpDispCur,ldExRateDt,lcTmepFil,llAprvCurr,lcExSin1,lcExSin2,lnSavAlias,lcGetField

lnAmount    = IIF(TYPE('lnAmount') = 'N',lnAmount,0)
lcRpDispCur = IIF(TYPE('lcRpDispCur') ='C',lcRpDispCur,'')
ldExRateDt  = IIF(TYPE('ldExRateDt') = 'D',ldExRateDt,{})
lcTmepFile  = IIF(TYPE('lcTmepFile') = 'C',lcTmepFile,'')
llAprvCurr  = IIF(TYPE('llAprvCurr') = 'L',llAprvCurr,.F.)

lcGetFile   = IIF(TYPE('lcGetFile')$"UL",'',lcGetFile)
lcGetField  = IIF(TYPE('lcGetField') ='C',lcGetField,'')

lcExSin1    = ''       && Variable to hold the first sign in the equation.
lcExSin2    = ''       && Variable to hold the second sign in the equation.
lnSavAlias  = SELECT(0)  && Variable to save the alias.
DO CASE
  CASE lcRpDispCur = 'F'

  CASE lcRpDispCur = 'O'
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      *Get filed currency if send . 
       lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,;
                   IIF(EMPTY(lcGetField),&lcGetFile..CCURRCODE,EVAL(lcGetFile+'.'+lcGetField)))
    ENDIF  
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnExRate = 0
    IF EMPTY(lcGetFile)
      lnUnit = NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , DINVDATE , .F.) , NEXRATE)
    ELSE
      lnUnit = &lcGetFile..NCURRUNIT
      lnExRate = IIF(llAprvCurr , gfChkRate('lnUnit' , lcCurrCode , &lcGetFile..DINVDATE , .F.) , &lcGetFile..NEXRATE)
    ENDIF  
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnUnit <> 0 , lnUnit , 1)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit , 2)

  CASE lcRpDispCur = 'D'
    lnExRate   = 0
    lnUnit     = 0
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,;
                   IIF(EMPTY(lcGetField),&lcGetFile..CCURRCODE,EVAL(lcGetFile+'.'+lcGetField)))
    ENDIF
    IF lcCurrCode = oAriaApplication.BaseCurrency
      lnExRate = 1
      lnUnit   = 1
    ELSE
      lnExRate   = gfChkRate('lnUnit',lcCurrCode,ldExRateDt,.F.)
    ENDIF
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnUnit <> 0 , lnUnit , 1)
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)

  CASE lcRpDispCur = 'N'
    lnExRate   = 0
    lnUnit     = 0
    IF EMPTY(lcGetFile)
      lcCurrCode = IIF(llAprvCurr,CAPRCURCOD,CCURRCODE)
    ELSE
      lcCurrCode = IIF(llAprvCurr,&lcGetFile..CAPRCURCOD,;
                   IIF(EMPTY(lcGetField),&lcGetFile..CCURRCODE,EVAL(lcGetFile+'.'+lcGetField)))
    ENDIF  
    IF lcCurrCode = oAriaApplication.BaseCurrency
      lnExRate = 1
      lnUnit   = 1
    ELSE
      IF SEEK(lcCurrCode,lcTmepFile)
        lnExRate = &lcTmepFile..NEXRATE
        lnUnit   = &lcTmepFile..NCURRUNIT
      ENDIF
    ENDIF
    lnExRate = IIF(lnExRate <> 0 , lnExRate , 1)
    lnUnit = IIF(lnUnit <> 0 , lnUnit , 1)
    lcExSin2   = ' '
    lcExSin1   = gfGetExSin(@lcExSin2,lcCurrCode)
    lnAmount   = ROUND(lnAmount &lcExSin1 lnExRate &lcExSin2 lnUnit,2)
ENDCASE
SELECT (lnSavAlias)
RETURN lnAmount

FUNCTION gfChkRate
PARAMETERS lcExUnit,lcCurrency,ldDate,llDispMsg,lcCompID,lcBaseCurr, llNoErrMsg

LOCAL lnCurDataSess

PRIVATE llExUsedBy,lcOldAlias,lcOldTag,lnRetRate,ldCurrDay,lcOldFlt,;
        llCurUsedBy, lnExRate
lnCurDataSess = SET("Datasession")         
lnRetRate  = 0
lcCompID   = IIF(TYPE('lcCompId') <> 'C' ,oAriaApplication.ActiveCompanyID , lcCompID)
lcBaseCurr = PADR(IIF(TYPE('lcBaseCurr') <> 'C',oAriaApplication.BaseCurrency,lcBaseCurr),3)
lcOldAlias  = SELECT()
llCurUsedBy = .F.
IF lcCurrency = lcBaseCurr
  IF TYPE('lcExUnit') = 'C'
    &lcExUnit = 1
    RETURN 1.0000    
  ENDIF
ENDIF
IF !USED('SYCCURR')
  llExUsedBy=.T.
  SELECT 0
  =gfOpenTable(oAriaApplication.SysPath + 'SYCCURR') 
ELSE
  SELECT SYCCURR
ENDIF

llExUsedBy=.F.
IF !USED('SYCEXCH')
  llExUsedBy=.T.
  SELECT 0
   =gfOpenTable(oAriaApplication.SysPath + 'SYCEXCH') 
ELSE
  SELECT SYCEXCH
ENDIF

lcOldFlt=FILTER()
lcOldTag=ORDER()
=gfSetOrder('CURRENCY')
SET ORDER TO  CURRENCY DESCENDING
lcSetNear = SET('NEAR')
SET NEAR ON 
SET FILTER TO
IF gfSEEK(lcBaseCurr+PADR(lcCurrency,3)+DTOS(ldDate))
  lnRetRate= nExRate
ELSE
  STORE .F. TO LLMULCURR,LLEXCHRATE
  STORE 0 TO LNEXRATDAY
  lnNoVar    = gfGetMemVar('LLMULCURR,LLEXCHRATE,LNEXRATDAY' , lcCompID)
  IF cBaseCurr + cCurrCode = lcBaseCurr + PADR(lcCurrency,3) ;
    .AND. dRateDate >= ldDate - lnExRatDay
    lnRetRate = nExRate    
  ELSE
    IF llExchRate AND llDispMsg
     * DO FORM SYCHRATE WITH lcCurrency, lcBaseCurr, ldDate,SET("Datasession") TO lnRetRate
      lnRetRate=IIF(TYPE('lnRetRate')<>'N',0,lnRetRate)
    ELSE
      *E300309,1 Display the default error message only if 
      *E300309,1 llDispMsg is .T. and llNoErrMsg is .F.
      *IF llDispMsg
      IF llDispMsg .AND. !llNoErrMsg
      *E300309,1 end.
        ** Message : "The last defined excahnge rate exceeds     "
        **           "the valid number of days."+CHR(13)+CHR(10)+"
        **           "The currency will be defaulted to the base "
        **           "currency.                                  "
        **           "                       ® Ok ¯              "
*        =oAriaApplication.MessageBox("TRM00249B00000","DIALOG")
      ENDIF
    ENDIF
  ENDIF  
ENDIF
IF TYPE('lcExUnit') = 'C'
  &lcExUnit = LOOKUP(SYCCURR.NCURRUNIT,lcCurrency,SYCCURR.CCURRCODE,"CCURRCODE")
ENDIF
SELECT SYCEXCH
IF !EMPTY(lcOldTag)
  gfSEToRDER(lcOldTag)
ENDIF
SET FILTER TO &lcOldFlt
IF llExUsedBy
  gfCloseTable('SYCEXCH')
ENDIF
IF llCurUsedBy
  gfCloseTable('SYCCURR')
ENDIF

SET NEAR &lcSetNear
SELECT (lcOldAlias)
RETURN lnRetRate 

FUNCTION gfGetExSin
PARAMETERS lcUntSin, lcCurrency, lcBaseCurr
LOCAL lcReturn , llClose

IF TYPE('lcUntSin') = 'C' 
  lcUntSin = '/'
ENDIF
IF TYPE('lcBaseCurr') $ 'UL'
  lcBaseCurr = oAriaApplication.BaseCurrency
ENDIF
IF lcCurrency = lcBaseCurr
  RETURN '*'
ENDIF

*-- Hesham (Start)
*-- select data from syccurr remotely and procced the addcurrency
*-- normaly
** This file should always be in use if the system is multi currency,
** Sometimes it is closed by Apparel programs, so, if it is
** not used,open the file.
llClose = .F.
IF !USED('SYCCURR')
  llClose = .T.
  gfOpenTable(oAriaApplication.SysPath+"SYCCURR",'CCURRCODE') 
ENDIF  
=gfSEEK(lcBaseCurr,'SYCCURR','CCURRCODE')
lcReturn = IIF((SYCCURR.cCurMeth = 'M' .AND.;
               !(ALLTRIM(lcCurrency) $ SYCCURR.mCurExcept)) .OR.;
               SYCCURR.cCurMeth = 'D' .AND.;
               (ALLTRIM(lcCurrency) $ SYCCURR.mCurExcept) , '*' , '/')

RETURN lcReturn
*N000635,2 MMT 11/16/2009 Convert Some Function to Aria5[End]
*N000635,1 MMT 10/19/2009 Convert Print Check report to Aria4[End]


FUNCTION errHandler
PARAMETER merror, mess, mess1, mprog, mlineno

public ErrorFlag

LOCAL lcResult
lcResult = 'Error number: ' + LTRIM(STR(merror)) + CHR(13) + CHR(10)
lcResult = lcResult + 'Error message: ' + mess + CHR(13) + CHR(10)
lcResult = lcResult + 'Line of code with error: ' + mess1 + CHR(13) + CHR(10)
lcResult = lcResult + 'Line number of error: ' + LTRIM(STR(mlineno)) + CHR(13) + CHR(10)
lcResult = lcResult + 'Program with error: ' + mprog

IF !ErrorFlag
  TRY
  LOCAL loAgent  
  *B610504,1 SAB 09/08/2013 Use Create Remote Object instead of Create Object [T20130826.0003] [Start]
  **E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][Start]
  *loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  **loAgent = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  **E303361,1 SAB 02/28/2013 Merge RB R13 modification with R12 and update R13 VSS [Aria5.2 R13 Media][End]
  loAgent = goRemoteCall.GetRemoteObject("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
  *B610504,1 SAB 09/08/2013 Use Create Remote Object instead of Create Object [T20130826.0003] [End]
  loAgent.UpdateRequestStatus(gcRequestID, 6, lcResult, gcClientID)
  CATCH
  ENDTRY
ENDIF

ErrorFlag = .T.

RETURN TO MASTER


*B610194,1 SAB 01/14/2013 Add functions used in Option Grid [Start]
*!*****************************************************************************************
*! Name       : gfConvertDataType
*! Developer  : Saber A Razek (SAB)
*! Date       : 01-14-2013
*! Purpose    : Convert data type from Fox Format to SQL Format
*! Parameters : lcFoxStruct : Comma separated string or array 
*!*****************************************************************************************
FUNCTION gfConvertDataType
LPARAMETERS lcFoxStruct


LOCAL lcSQLStruct,lcCommaPos,lcSpacePos,lcField,lcStruct, lnCount,lcStr
LOCAL lcOpenBracketPos, lcCloseBracketPos

lcFileType = IIF(TYPE("lcFoxStruct[1]") ='C', 'A', 'C')
IF lcFileType = 'C' && Structure passed as comma separated string
  LOCAL lcRetStruct
  lcStr = ALLTRIM(lcFoxStruct) + ","
  lcRetStruct = ""
  DO WHILE LEN(lcStr) > 1	
  lcSpacePos = AT(SPACE(1),lcStr)
	lcOpenBracketPos  = AT('(',lcStr)
	lcCloseBracketPos = AT(')',lcStr)
	lcCommaPos = IIF(lcOpenBracketPos < AT(',',lcStr) AND lcCloseBracketPos > ;
					AT(',',lcStr),AT(',',lcStr,2),AT(',',lcStr))
    lcField  = SUBSTR(lcStr,1,lcSpacePos-1)
    lcStruct = ALLTRIM(SUBSTR(lcStr,lcSpacePos,lcCommaPos-lcSpacePos))
    DO CASE && Check For the connection Driver
      CASE oAriaApplication.ConnectionDriver = 'SQL'
      DO CASE 
	    CASE LEFT(lcStruct,1) = 'C'
		      lcSQLStruct = "Char" + SUBSTR(lcStruct,2,LEN(lcStruct)-1)
        CASE LEFT(lcStruct,1) = 'N'
		      lcSQLStruct = "Numeric"+ SUBSTR(lcStruct,2,LEN(lcStruct)-1)
        CASE lcStruct = 'L'
	  	    lcSQLStruct = "bit"
        CASE lcStruct = 'D'
	    	  lcSQLStruct = "datetime"
        CASE lcStruct = 'T'
	      	lcSQLStruct = "datetime"
        CASE lcStruct = 'MEMO'
		      lcSQLStruct = "Text"
        CASE lcStruct = 'M'
		      lcSQLStruct = "Text"
        OTHERWISE
   	      lcSQLStruct = ""
      ENDCASE
    ENDCASE
	IF EMPTY(lcRetStruct) 
  	  lcRetStruct = lcField + SPACE(1) + lcSQLStruct    		    
	ELSE
	  lcRetStruct = lcRetStruct + "," + lcField + SPACE(1) + lcSQLStruct    		    
	ENDIF
	lcStr = ALLTRIM(SUBSTR(lcSTR,lcCommaPos+1,LEN(lcSTR)-lcCommaPos))
  ENDDO	
  RETURN lcRetStruct

ELSE && AFIELDS
  FOR lnCount = 1 TO ALEN(lcFoxStruct,1)
    * C = Character, D = Date, L = Logical, M = Memo, N = Numeric, F = Float, I = Integer
    DO CASE && Check For the connection Driver
      CASE oAriaApplication.ConnectionDriver = 'SQL'
      DO CASE 
	    CASE lcFoxStruct[lnCount,2] = 'C'
	  	    lcSQLStruct = "Char"
        CASE lcFoxStruct[lnCount,2] = 'N'
	    	  lcSQLStruct = "Numeric"
        CASE lcFoxStruct[lnCount,2] = 'L'
	      	lcSQLStruct = "bit"
        CASE lcFoxStruct[lnCount,2]	= 'D'
		      lcSQLStruct = "datetime"
        CASE lcFoxStruct[lnCount,2]	= 'T'
		      lcSQLStruct = "datetime"
       CASE lcFoxStruct[lnCount,2]	= 'M'
		      lcSQLStruct = "Text"
       CASE lcFoxStruct[lnCount,2]	= 'MEMO'
		      lcSQLStruct = "Text"
        CASE lcFoxStruct[lnCount,2]	= 'F'
		      lcSQLStruct = "Float"
        CASE lcFoxStruct[lnCount,2]	= 'I'
		      lcSQLStruct = "Integer"
        OTHERWISE
   	      lcSQLStruct = ""
      ENDCASE
    ENDCASE
	lcFoxStruct[lnCount,2] = lcSQLStruct
  ENDFOR
ENDIF


*!*************************************************************
*! Name      : gfGetStyleImageInfo
*! Developer : Saber A Razek (SAB)
*! Date      : 01-14-2013
*! Purpose   : Get Style Image Info
*!*************************************************************
FUNCTION gfGetStyleImageInfo
LPARAMETERS lcReturnType, lcStyleKey, llNewSession

IF llNewSession
  LOCAL lnOldSession
  lnOldSession = SET("Datasession")

  LOCAL loNewSession
  loNewSession = CREATEOBJECT('session')
  SET DATASESSION TO loNewSession.DATASESSIONID
  SET MULTILOCKS ON
ENDIF
lcDeleteSet =  SET("Deleted")
SET DELETED ON

LOCAL lnSelect
lnSelect = SELECT(0)

DIMENSION laSegInfo[1,1]
LOCAL loGetItemMask
loGetItemMask = CREATEOBJECT('GetItemMask')
loGetItemMask.DO(@laSegInfo, '', "0001")
IF ALEN(laSegInfo, 1) > 1
  IF EMPTY(SUBSTR(lcStyleKey, laSegInfo[2, 4], LEN(laSegInfo[2, 3])))
    lcStyleKey = SUBSTR(lcStyleKey, laSegInfo[1, 4], LEN(laSegInfo[1, 3]))
  ELSE
    lcStyleKey = SUBSTR(lcStyleKey, laSegInfo[1, 4], LEN(laSegInfo[1, 3])) + laSegInfo[1, 6] + ;
      SUBSTR(lcStyleKey, laSegInfo[2, 4], LEN(laSegInfo[2, 3]))
  ENDIF
ENDIF

LOCAL lnMajLen, lcRetValue

lcStyleKey = PADR(lcStyleKey, 19)

IF !USED('OBJLINKTMP')
  USE (oAriaEnvironment.DataDir+"OBJLINK") IN 0 SHARED ALIAS OBJLINKTMP ORDER OBJLNKTY
ENDIF
IF !USED('OBJECTSTMP')
  USE (oAriaEnvironment.DataDir+"OBJECTS") IN 0 SHARED ALIAS OBJECTSTMP ORDER OBJECTID
ENDIF

IF gfSeek('D' + 'S' + lcStyleKey, 'OBJLINKTMP', 'OBJDEFA') .OR. gfSeek('S' + lcStyleKey, 'OBJLINKTMP', 'OBJLNKTY')
  IF ALLTRIM(lcReturnType) == "K"
    lcRetValue = lcStyleKey
  ELSE
    IF gfSeek(OBJLINKTMP.cobject_id, 'OBJECTSTMP', 'OBJECTID')
      lcRetValue  = OBJECTSTMP.mimgpath
    ELSE
      lcRetValue  = ""
    ENDIF
  ENDIF
ELSE
  lnMajLen = LEN(gfItemMask('PM'))

  IF ALLTRIM(lcReturnType) == "K"
    lcRetValue = SUBSTR(lcStyleKey, 1, lnMajLen)
  ELSE
    lcRetValue = PADR(SUBSTR(lcStyleKey, 1, lnMajLen),19)
    IF gfSeek('D' + 'S' + lcRetValue, 'OBJLINKTMP', 'OBJDEFA') .OR. gfSeek('S' + lcRetValue, 'OBJLINKTMP', 'OBJLNKTY')
      IF gfSeek(OBJLINKTMP.cobject_id, 'OBJECTSTMP', 'OBJECTID')
        lcRetValue  = OBJECTSTMP.mimgpath
      ELSE
        lcRetValue  = ""
      ENDIF
    ELSE
      lcRetValue = ""
    ENDIF
  ENDIF
ENDIF

IF USED('OBJLINKTMP')
  USE IN OBJLINKTMP
ENDIF
IF USED('OBJECTSTMP')
  USE IN OBJECTSTMP
ENDIF


SELECT(lnSelect)
SET DELETED &lcDeleteSet.
IF llNewSession
  loNewSession = NULL
  SET DATASESSION TO lnOldSession
ENDIF

IF ALLTRIM(lcReturnType) == "K"
  RETURN PADR(lcRetValue, 19)
ELSE
  RETURN lcRetValue
ENDIF

RETURN ""

ENDFUNC
*- End of gfGetStyleImageInfo


*!*************************************************************
*! Name      : gfGetItemImageInfo
*! Developer : Saber A Razek (SAB)
*! Date      : 01-14-2013
*! Purpose   : Get Item Image Info
*!*************************************************************
FUNCTION gfGetItemImageInfo
LPARAMETERS lcReturnType, lcStyleKey, llNewSession

IF llNewSession
  LOCAL lnOldSession
  lnOldSession = SET("Datasession")

  LOCAL loNewSession
  loNewSession = CREATEOBJECT('session')
  SET DATASESSION TO loNewSession.DATASESSIONID
  SET MULTILOCKS ON
ENDIF
lcDeleteSet =  SET("Deleted")
SET DELETED ON
lnSelect = SELECT(0)

DIMENSION laSegInfo[1,1]
LOCAL loGetItemMask
loGetItemMask = CREATEOBJECT('GetItemMask')
loGetItemMask.DO(@laSegInfo, '', "0002")


IF ALEN(laSegInfo, 1) > 1
  IF EMPTY(SUBSTR(lcStyleKey, laSegInfo[2, 4], LEN(laSegInfo[2, 3])))
    lcStyleKey = SUBSTR(lcStyleKey, laSegInfo[1, 4], LEN(laSegInfo[1, 3]))
  ELSE
    lcStyleKey = SUBSTR(lcStyleKey, laSegInfo[1, 4], LEN(laSegInfo[1, 3])) + laSegInfo[1, 6] + ;
      SUBSTR(lcStyleKey, laSegInfo[2, 4], LEN(laSegInfo[2, 3]))
  ENDIF
ENDIF

LOCAL lnMajLen, lcRetValue

lcStyleKey  = PADR(lcStyleKey, 19)

IF !USED('OBJLINKTMP')
  USE (oAriaEnvironment.DataDir+"OBJLINK") IN 0 SHARED ALIAS OBJLINKTMP ORDER OBJLNKTY
ENDIF

IF !USED('OBJECTSTMP')
  USE (oAriaEnvironment.DataDir+"OBJECTS") IN 0 SHARED ALIAS OBJECTSTMP ORDER OBJECTID
ENDIF

IF gfSeek('D' + 'M' + lcStyleKey, 'OBJLINKTMP', 'OBJDEFA') .OR. gfSeek('M' + lcStyleKey, 'OBJLINKTMP', 'OBJLNKTY')
  IF ALLTRIM(lcReturnType) == "K"
    lcRetValue = lcStyleKey
  ELSE
    IF gfSeek(OBJLINKTMP.cobject_id, 'OBJECTSTMP', 'OBJECTID')
      lcRetValue  = objectstmp.mimgpath
    ELSE
      lcRetValue  = ""
    ENDIF
  ENDIF
ELSE
  lnMajLen    = LEN(gfItemMask('PM', '', '0002'))
  IF ALLTRIM(lcReturnType) == "K"
    lcRetValue  = SUBSTR(lcStyleKey, 1, lnMajLen)
  ELSE
    lcRetValue  = PADR(SUBSTR(lcStyleKey, 1, lnMajLen),19)
    IF gfSeek('D' + 'M' + lcRetValue, 'OBJLINKTMP', 'OBJDEFA') .OR. gfSeek('M' + lcRetValue, 'OBJLINKTMP', 'OBJLNKTY')
      IF gfSeek(OBJLINKTMP.cobject_id, 'OBJECTSTMP', 'OBJECTID')
        lcRetValue  = objectstmp.mimgpath
      ELSE
        lcRetValue  = ""
      ENDIF
    ELSE
      lcRetValue  = ""
    ENDIF
  ENDIF
ENDIF

IF USED('OBJLINKTMP')
  USE IN OBJLINKTMP
ENDIF
IF USED('OBJECTSTMP')
  USE IN OBJECTSTMP
ENDIF


SELECT(lnSelect)
SET DELETED &lcDeleteSet.
IF llNewSession
  loNewSession = NULL
  SET DATASESSION TO lnOldSession
ENDIF

IF ALLTRIM(lcReturnType) == "K"
  RETURN PADR(lcRetValue, 19)
ELSE
  RETURN lcRetValue
ENDIF

RETURN ""

ENDFUNC
*- End of gfGetItemImageInfo
*B610194,1 SAB 01/14/2013 Add functions used in Option Grid [End]

*E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE [Start]
*!*************************************************************
*! Name      : gfGetVld
*! Developer : Saber A Razek (SAB)
*! Date      : 02-14-2013
*! Purpose   : Get Valid Entries
*!*************************************************************
FUNCTION gfGetVld
PARAMETERS lcFieldNam, laArrName,llAddAll
PRIVATE lcCurrFile,lcOldTag,lcSetExct,llSydField,lnMaxLen

*** Save current environment
lcCurrFile    = SELECT()
lcSetExact    = SET('EXACT')
llSydField    = .F.
lnMaxLen      = 0

*-- select the field information from sydfield remotely
lnDataS = SET("DATASESSION")
lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYDFIELD where cFld_Name='"+UPPER(lcFieldNam)+"'",'',"SYDFIELDTMP","",oAriaApplication.SystemConnectionString,3,"",lnDataS)
*-- if the connection succeded
IF lnRemResult=1
  LOCATE
  ** Check if the field was found then get the field name
  IF FOUND()
    =gfSubStr(SYDFIELDTMP.mVEntries, @laArrName, "|~")
    lnMaxLen = LEN(laArrName[1,1])
    FOR lnCount = 2 TO ALEN(laArrName,1)
      lnMaxLen = MAX(lnMaxLen,LEN(laArrName[lnCount,1]))
    ENDFOR
  ELSE
    laArrName = " "
  ENDIF
ENDIF
IF llAddAll
  *** Add one element on top to be used for 'All' option.
  *** Assign a value to this element later.
  IF !EMPTY(laArrName[1,1])
    DIMENSION laArrName[ALEN(laArrName,1)+1,2]
    =AINS(laArrName, 1, 1)
  ENDIF
  laArrName[1,1] = "All"
  laArrName[1,2] = " "
ENDIF

*** Restore environment
** if the connection cursor was created then close it
IF USED('SYDFIELDTMP')
  USE IN 'SYDFIELDTMP'
ENDIF
SET DATASESSION TO (lnDataS)
SELECT (lcCurrFile)
SET EXACT &lcSetExact
RETURN lnMaxLen

ENDFUNC
*- End of gfGetVld


*!*************************************************************
*! Name      : gfAdd_Info
*! Developer : Saber A Razek (SAB)
*! Date      : 02-14-2013
*! Purpose   : Add Log Info to Table
*!*************************************************************
FUNCTION gfAdd_Info
LPARAMETERS lcFileName, oForm

lnCurrData  = SET("Datasession")
RETURN oAriaEnvironment.AddUserInformation(lcFileName, oForm,lnCurrData)

ENDFUNC
*- End of gfAdd_Info


*!*************************************************************
*! Name      : lfUsrVldFn
*! Developer : Saber A Razek (SAB)
*! Date      : 02-14-2013
*! Purpose   : Do user defined function inside custom program.
*!*************************************************************
FUNCTION lfUsrVldFn
LPARAMETERS cFunctionName,vFunctionProgram,cFunctionParameter

  RETURN loOGScroll.DoUserValidFunction(cFunctionName, vFunctionProgram, cFunctionParameter)

ENDFUNC 
*- End of lfUsrVldFn
*E303352,1 SAB 02/14/2013 RB Enhancement to work with one EXE [End]

*B610559,1 SAB 10/24/2013 Merge Modify AriaMain program and class to add  gfAddCurSm function [T20130904.0003][Start]
*!*************************************************************
*! Name      : gfAddCurSm
*! Developer : Saber A Razek (SAB)
*! Date      : 10-24-2013
*! Purpose   : Function to add the currency symbol to a numeric value
*!*************************************************************
FUNCTION gfAddCurSm
PARAMETERS lcCurrCode, lnAmount, lnDecimals

RETURN oAriaEnvironment.AddCurrencySymbol(lcCurrCode, lnAmount, lnDecimals)

ENDFUNC
*-End of gfAddCurSm
*B610559,1 SAB 10/24/2013 Merge Modify AriaMain program and class to add  gfAddCurSm function [T20130904.0003][End]
**********************************************************************************************************************
*! Name      	: gfAddPerfLog
*! Developer 	: Mahmoud Said [MAH]
*! Date      	: 08/31/2004
*! Purpose   	: Add record to performance log file
*! Tracking   	: B128052,1 MAH 06/05/2005 Takes a long time to log in.
**********************************************************************************************************************
*! Returns 	:
*!  lcProcType     : Porcess Type "LOGIN", "OPEN_SCREEN" ...etc
*!  lcProcID       : Porcess ID
*!  lcProcChkID    : Check Point ID "START", "END" ...etc
*!  lmMoreInfo     : Additional Information
*!  llGetFullData  : (Optional) Measure all data.
**********************************************************************************************************************
PROCEDURE gfAddPerfLog
LPARAMETERS lcProcType, lcProcID, lcProcChkID, lmMoreInfo, llGetFullData

IF TYPE('goPrefObject') # 'O' .OR. ISNULL(goPrefObject)
  RETURN
ENDIF

LOCAL lnSelected
lnSelected = SELECT()

IF TYPE('gcSessUserID') = 'C'
  LOCAL lmResCons
  lmResCons = ''

  IF llGetFullData .OR. !EMPTY(gcSessUserID)
    LOCAL lnSecFrom
    lnSecFrom = SECONDS()

    lmResCons = goPrefObject.GetCheckPointResultString(_VFP.ProcessId, llGetFullData)

    LOCAL lnSecTo
    lnSecTo = SECONDS()

    lmResCons = lmResCons + CHR(13) + 'CS_GetConsumptionDataTime = ' + STR(lnSecTo - lnSecFrom, 10, 3)
  ENDIF

  INSERT INTO (gcPerfLogFileName)(cSessID,         cUserID,      dSessD,            cSessT,           nTime,      cProcType,  cProcID,  cProcChkID,   mResCons, mMoreInfo) ;
                          VALUES (gcPerfSessionID, gcSessUserID, gcPerfSessionDate, gcPerfSessionTime, SECONDS(), lcProcType, lcProcID, lcProcChkID, lmResCons, lmMoreInfo)
ENDIF

IF USED("SyPrefLg")
 USE IN SyPrefLg
ENDIF

SELECT(lnSelected)

*****************************************************************************************************************
*B612314,1 MMT 07/05/2021 Add missing function from Aria5 globals[Start]
*:---------------------------------------------------------------------
*! Name      : gfIsEdtble
*! Developer : Yasser Mohammed Aly - (YMA)
*! Date      : 04/27/97
*! Purpose   : Function to tell if a specific code is editable by the
*!             user or not.
*: Job ID    : E# 300631
*:---------------------------------------------------------------------
*: Calls              : None
*:---------------------------------------------------------------------
*: Passed Parameters  : lcField  -> The code to be checked.
*:                      lnFieldW -> Pointer to a numeric variable to
*:                                  hold the field width.
*:---------------------------------------------------------------------
*: Example            : = gfIsEdtble("TERMS", @lnWidth)
*:---------------------------------------------------------------------
FUNCTION GFISEDTBLE
PARAMETERS LCPFIELD, LNFIELDW, LCACTVCOMP
LOCAL OISEDTBLE
set step on
*B607903,1 MMT 12/24/2006 Fix bug of Field width in codes screen is not read from Codeset File [Start]
*!*	oIsEdtble = CREATEOBJECT('IsEdtble')
*!*	RETURN oIsEdtble.Do(lcPField, lnFieldW, lcActvComp)
PRIVATE LLRETVAL, LCDATADIR
GCACT_COMP = OARIAAPPLICATION.ACTIVECOMPANYID
GCDATADIR  = OARIAAPPLICATION.DATADIR
GCSYSHOME  = OARIAAPPLICATION.SYSPATH

LOCAL LNCURALIAS
LNCURALIAS = SELECT(0)

LCACTVCOMP = IIF(TYPE("lcActvComp")#"C", GCACT_COMP, LCACTVCOMP)
LCDATADIR  = GCDATADIR

IF !(LCACTVCOMP == GCACT_COMP)
  LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from syccomp where cComp_ID='"+LCACTVCOMP+"'",'',"CompFile","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("DATAS"))
  IF LNREMRESULT>=1
    LOCATE
  ENDIF
  IF LNREMRESULT>=1 AND FOUND()
    LCDATADIR = GFGETDATADIR(ALLTRIM(COMPFILE.CCOM_DDIR))
  ENDIF
  USE IN COMPFILE
ENDIF
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[Start]
*lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from SYDFIELD where CFLD_NAME='"+lcPField+"'",'',"FieldFile","",oAriaApplication.SystemConnectionString,3,"",SET("DATAS"))
LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select MCODEINFO,NFLD_WDTH from SYDFIELD where CFLD_NAME='"+LCPFIELD+"' AND (CVER='A27' OR EMPTY(CVER))",'',"FieldFile","",OARIAAPPLICATION.SYSTEMCONNECTIONSTRING,3,"",SET("DATAS"))
*N000682,1 MMT 03/06/2013 Merge SysFiles and SqlDictionary folder[END]
LOCAL LNFIELDWIDTH
IF LNREMRESULT>=1
  LOCATE
ENDIF
LLRETVAL = IIF(LNREMRESULT>=1 AND FOUND(),;
  ("EDITABLE" $ UPPER(ALLTRIM(FIELDFILE.MCODEINFO))), .F.)
LNFIELDWIDTH = IIF(LNREMRESULT>=1 AND FOUND(),FIELDFILE.NFLD_WDTH, 6)
USE (LCDATADIR+"CODESET" ) IN 0 ORDER FILDNAME  AGAIN ALIAS CODESETF
LNFIELDW = IIF(SEEK(LCPFIELD, "CodeSetF" ), CODESETF.NFLD_WDTH,LNFIELDWIDTH)
USE IN FIELDFILE
USE IN CODESETF
SELECT(LNCURALIAS)


RETURN LLRETVAL

*:************************************************************************
*: Program file  : GFGENFLT.PRG
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
*!************************************************************************
*! Modifications :
*! E038142, 2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*! E037237,2 MAH 10/01/2004 Enhance the interface of color scheme.
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
*!************************************************************************
*:
FUNCTION GFGENFLT
*! E038142,2 MAH 08/31/2004 Add new paramter to pass the each table engine lcDBEngine [BEGIN]
*-- * N038038,1 SMM Add a parameter laFilterArray that will hold the expressions[Start]
*-- *--	PARAMETERS lcArray , llFilter , llUseArray
*-- PARAMETERS lcArray , llFilter , llUseArray ,laFilterArray
*-- * N038038,1 SMM [End]

*! E038650,1 MAH 10/27/2004 Add new parameters: remove case sensitive and another to return english filter string [BEGIN]
*-- PARAMETERS lcArray, llFilter, llUseArray, laFilterArray, lcDBEngine
PARAMETERS LCARRAY, LLFILTER, LLUSEARRAY, LAFILTERARRAY, LCDBENGINE, LLCASEINSENSITIVE, LLENGLISHQUERY
*! E038650,1 MAH 10/27/2004 [END]
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
LCDBENGINE = IIF(TYPE('lcDBEngine') = 'C', LCDBENGINE, OARIAAPPLICATION.CNATIVEDBID)

*! E038142,2 MAH 08/31/2004 [END]

LLUSEARRAY = IIF(TYPE('llUseArray') <> 'L' , .F. , LLUSEARRAY)

* N038038,1 SMM To check if the parameter was send or not[Start]
LLUSEFILTERARRAY = IIF(TYPE('laFilterArray') <> 'C', .F. , .T.)
* N038038,1 SMM [End]

LCQUERY=''
LCELMSEP='|'
LCVALSEP='~'

LCLINEFEED=' ' &&+CHR(10)+CHR(13)
LNFLTSTART=1
DO WHILE (&LCARRAY[lnFltStart,1]='.OR.' OR EMPTY(&LCARRAY[lnFltStart,1]));
    AND !LNFLTSTART=ALEN(&LCARRAY,1)
  LNFLTSTART=LNFLTSTART+1
ENDDO
LNFLTEND=ALEN(&LCARRAY,1)
DO WHILE (&LCARRAY[lnFltEnd,1]='.OR.' OR EMPTY(&LCARRAY[lnFltEnd,1]));
    AND LNFLTEND>1
  LNFLTEND=LNFLTEND-1
ENDDO

LNOR=0
IF LNFLTSTART>ALEN(&LCARRAY,1)
  RETURN ''
ENDIF
IF LNFLTEND=ALEN(&LCARRAY,1)
  LCWHICHELM=LCARRAY+'['+ALLTRIM(STR(ALEN(&LCARRAY,1)))+',1]'
  IF TYPE(LCWHICHELM)<>'C'
    RETURN ''
  ENDIF
  IF &LCARRAY[ALEN(&lcArray,1),1]='.OR.'
    RETURN ''
  ENDIF
ENDIF
LNCOUNT=LNFLTSTART

DO WHILE  LNCOUNT<=LNFLTEND
  *! E037237,2 MAH 10/01/2004 Remove the VAL becasue it condseder 0 is empty [BEGIN]
  *-- IF &lcArray[lnCount,3]='N' AND EMPTY(VAL(&lcArray[lnCount,6])) ;
  *--     AND &lcArray[lnCount,7]='V'
  *--   lnCount=lnCount+1
  *--   LOOP
  *-- ENDIF
  IF &LCARRAY[lnCount,3]='N' AND EMPTY(&LCARRAY[lnCount,6]) ;
      AND &LCARRAY[lnCount,7]='V'
    LNCOUNT=LNCOUNT+1
    LOOP
  ENDIF
  *! E037237,2 MAH 10/01/2004 [END]

  IF IIF(TYPE(ALLTRIM(LCARRAY) + "[lnCount,6]") = 'C' ,;
      !EMPTY(ALLTRIM(STRTRAN(STRTRAN(&LCARRAY[lnCount,6] , LCELMSEP ,;
      '') , IIF(&LCARRAY[lnCount,3] = 'D' ,'/' , '') , ''))) ,;
      !EMPTY(&LCARRAY[lnCount,6])) .OR. &LCARRAY[lnCount,1] = '.OR.'

    IF !EMPTY(&LCARRAY[lnCount,1])
      IF &LCARRAY[lnCount,1]<>'.OR.'
        *! E038142,2 MAH 08/31/2004 Pass lcDBEngine to lfGetQCond [BEGIN]
        *-- lcQuery=lcQuery+lfGetQCond(lnCount,lcArray,llFilter)
        *! E038650,1 MAH 10/27/2004 apply new parameters remove case sensitive and return english filter string [BEGIN]
        *-- lcQuery = lcQuery + lfGetQCond(lnCount, lcArray, llFilter, lcDBEngine)
        LCQUERY = LCQUERY + LFGETQCOND(LNCOUNT, LCARRAY, LLFILTER, LCDBENGINE, LLCASEINSENSITIVE, LLENGLISHQUERY)
        *! E038650,1 MAH 10/27/2004 [END]
        *! E038142,2 MAH 08/31/2004 [END]

        * N038038,1 SMM Filling laFilterArray with the filters one filter on the record[Start]
        IF LLUSEFILTERARRAY && If laFilterArray parameter was passed
          *-- Fill the laFilterArray column 2 with the filter expression after removing | .AND. |
          *! E038142,2 MAH 08/31/2004 Pass lcDBEngine to lfGetQCond [BEGIN]
          *-- laFilterArray[lnCount,2] = SUBSTR(lfGetQCond(lnCount,lcArray,llFilter),1,LEN(lfGetQCond(lnCount,lcArray,llFilter))-9)
          LAFILTERARRAY[lnCount, 2] = SUBSTR(LFGETQCOND(LNCOUNT, LCARRAY, LLFILTER, LCDBENGINE), ;
            1, ;
            LEN(LFGETQCOND(LNCOUNT, LCARRAY, LLFILTER, LCDBENGINE)) - 9)
          *! E038142,2 MAH 08/31/2004 [END]

          IF AT("SUBSTR",&LCARRAY[lnCount,1])>0
            LNTABLEFIRST = AT("(",&LCARRAY[lnCount,1])+1 && get the position of "("
            LNTABLEEND = AT(".",&LCARRAY[lnCount,1]) && get the position of "."
          ELSE
            LNTABLEFIRST = 1
            LNTABLEEND = AT(".",&LCARRAY[lnCount,1]) && get the position of "."
          ENDIF
          *-- Fill the laFilterArray column 1 with the Table Name
          LAFILTERARRAY[lnCount,1] = SUBSTR(&LCARRAY[lnCount,1],LNTABLEFIRST,LNTABLEEND-LNTABLEFIRST)
        ENDIF
        * N038038,1 SMM [END]
      ELSE
        LCQUERY= IIF(RIGHT(LCQUERY,9)=LCELMSEP+' .AND. '+LCELMSEP,SUBSTR(LCQUERY,1,LEN(LCQUERY)-9),LCQUERY)
        IF LNOR>0
          LCQUERY=LCQUERY+' ) '
          LNOR=0
        ENDIF

        DO WHILE LNCOUNT<LNFLTEND-1 AND (EMPTY(ALLTRIM(STRTRAN(STRTRAN(&LCARRAY[lnCount+1,6],LCELMSEP,'');
            ,IIF(&LCARRAY[lnCount+1,3]='D','/  /',''),''))) OR &LCARRAY[lnCount+1,1]='.OR.')
          LNCOUNT=LNCOUNT+1
        ENDDO
        IF !EMPTY(ALLTRIM(STRTRAN(&LCARRAY[lnCount+1,6],LCELMSEP,''))) AND !EMPTY(ALLTRIM(LCQUERY))
          LCQUERY=ALLTRIM(LCQUERY)+' '+LCELMSEP+' OR '+LCELMSEP+'( '
          LNOR=1
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  LNCOUNT=LNCOUNT+1
ENDDO
LCQUERY= IIF(RIGHT(LCQUERY,9)=LCELMSEP+' .AND. '+LCELMSEP,SUBSTR(LCQUERY,1,LEN(LCQUERY)-9),LCQUERY)
IF LNOR>0
  LCQUERY=LCQUERY+' ) '
ENDIF
LCQUERY=STRTRAN(LCQUERY,LCELMSEP+' .AND. '+LCELMSEP,LCLINEFEED+' AND '+LCLINEFEED)
LCQUERY=STRTRAN(LCQUERY,LCELMSEP+' OR '+LCELMSEP,LCLINEFEED+' OR '+LCLINEFEED)
RETURN LCQUERY
*-- end of gfGenFlt.

*!*****************************************************************************************
*! Name      : lfGetQCond
*! Developer :
*! Date      : 10/13/2002 10:40:09 AM
*! Purpose   : Get Filter condition
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters:
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!************************************************************************
*! Modifications :
*! E038142,2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
*! B999999,1 MAH 03/01/2005 Fix apply filter problem.
*!************************************************************************
FUNCTION LFGETQCOND
*! E038142,2 MAH 08/31/2004 Add new paramter to pass the each table engine lcDBEngine [BEGIN]
*-- PARAMETERS lnCount,lcArray,llFilter

*! E038650,1 MAH 10/27/2004 Add new parameters: remove case sensitive and another to return english filter string [BEGIN]
*-- PARAMETERS lcArray, llFilter, llUseArray, laFilterArray, lcDBEngine
PARAMETERS LNCOUNT, LCARRAY, LLFILTER, LCDBENGINE, LLCASEINSENSITIVE, LLENGLISHQUERY
*! E038650,1 MAH 10/27/2004 [END]

LCDBENGINE = IIF(TYPE('lcDBEngine') = 'C', LCDBENGINE, OARIAAPPLICATION.CNATIVEDBID)
*! E038142,2 MAH 08/31/2004 [END]

*! E038142,2 MAH 08/31/2004 Convert left side according DB engine [BEGIN]
LOCAL LCLEFTSIDE
*! E038650,1 MAH 01/18/2005 [BEGIN]
*-- lcLeftSide = lfGetConditionLeftSide(&lcArray[lnCount, 1], lcDBEngine)
LCLEFTSIDE = LFGETCONDITIONLEFTSIDE(&LCARRAY[lnCount, 1], LCDBENGINE)
LCLEFTSIDE = LCLEFTSIDE + ' '
*! E038650,1 MAH 01/18/2005 [END]
*! E038142,2 MAH 08/31/2004 [END]

LCFILTEXP=''
DO CASE
CASE &LCARRAY[lnCount,5] = 'Contains'
  *! E038142,2 MAH 08/31/2004 Build condition according to DB Engine [BEGIN]
  DO CASE
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
    *! E038142,2 MAH 08/31/2004 [END]

    *! E038650,1 MAH 10/27/2004 Apply case Insensitive and english query if required [BEGIN]
    *-- lcFiltExp=IIF(&lcArray[lnCount,4],'','!(')+;
    *--           lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
    *--           &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
    *--           ' $ '+ALLTRIM(&lcArray[lnCount,1])+' '+;
    *--           IIF(&lcArray[lnCount,4],'',' ) ')+lcElmSep+' .AND. '+lcElmSep
    IF LLENGLISHQUERY
      LCFILTEXP = ALLTRIM(&LCARRAY.[lnCount, 1]) + ;
        IIF(&LCARRAY.[lnCount, 4], ' contains ', ' not contains ') + ;
        LFRIGHTGET(&LCARRAY.[lnCount, 6], ;
        &LCARRAY.[lnCount, 3], ;
        &LCARRAY.[lnCount, 5], ;
        LCELMSEP, ;
        &LCARRAY.[lnCount, 7], ;
        LCDBENGINE, ;
        LLCASEINSENSITIVE, ;
        .T.) + ;
        LCELMSEP + ' .AND. ' + LCELMSEP
    ELSE
      LCFILTEXP = IIF(&LCARRAY.[lnCount, 4], '',  '!(') + ;
        LFRIGHTGET(&LCARRAY.[lnCount, 6], ;
        &LCARRAY.[lnCount, 3], ;
        &LCARRAY.[lnCount, 5], ;
        LCELMSEP, ;
        &LCARRAY.[lnCount, 7], ;
        LCDBENGINE, ;
        LLCASEINSENSITIVE, ;
        LLENGLISHQUERY) + ;
        ' $ ' + ;
        IIF(LLCASEINSENSITIVE, 'RTRIM(UPPER(', '') + ;
        ALLTRIM(&LCARRAY.[lnCount, 1]) + ' ' + ;
        IIF(LLCASEINSENSITIVE, '))', '') + ;
        IIF(&LCARRAY.[lnCount, 4], '', ' )') + ;
        LCELMSEP + ' .AND. ' + LCELMSEP
    ENDIF
    *! E038650,1 MAH 10/27/2004 [END]

    *! E038142,2 MAH 08/31/2004 Get DB Engine type [BEGIN]
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
    *! E038650,1 MAH 01/18/2005 [BEGIN]
    *-- lcFiltExp = lcLeftSide + ;
    IIF(&lcArray[lnCount, 4], "LIKE '" , "NOT LIKE '%") + ;
    lfRightGet(&lcArray[lnCount, 6], ;
    &lcArray[lnCount, 3], ;
    &lcArray[lnCount, 5], ;
    lcElmSep, ;
    &lcArray[lnCount, 7], ;
    lcDBEngine) + '% ' + ;
    lcElmSep + ' .AND. ' + lcElmSep
    LOCAL LCRIGHT
    LCRIGHT = LFRIGHTGET(&LCARRAY[lnCount, 6], ;
      &LCARRAY[lnCount, 3], ;
      &LCARRAY[lnCount, 5], ;
      LCELMSEP, ;
      &LCARRAY[lnCount, 7], ;
      LCDBENGINE)
    LCRIGHT = ALLTRIM(LCRIGHT)
    LCRIGHT = SUBSTR(LCRIGHT, 2, LEN(LCRIGHT) - 2)
    *! B999999,1 MAH 03/01/2005 [BEGIN]
    *-- IF ALLTRIM(lcArray[lnCount, 7]) == 'F'
    IF ALLTRIM(&LCARRAY[lnCount, 7]) == 'F'
      *! B999999,1 MAH 03/01/2005 [END]
      LCRIGHT = SUBSTR(LCRIGHT, 2, LEN(LCRIGHT) - 2)
    ENDIF
    LCFILTEXP = LCLEFTSIDE + ;
      IIF(&LCARRAY[lnCount, 4], "LIKE '%" , "NOT LIKE '%") + ;
      LCRIGHT + "%' " + ;
      LCELMSEP + ' .AND. ' + LCELMSEP
    *! E038650,1 MAH 01/18/2005 [END]
  ENDCASE
  *! E038142,2 MAH 08/31/2004 [END]

CASE &LCARRAY[lnCount,5] = 'Like' OR &LCARRAY[lnCount,5] = 'Exactly Like'
  *! E038142,2 MAH 08/31/2004 Build condition according to DB Engine [BEGIN]
  DO CASE
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
    *! E038142,2 MAH 08/31/2004 [END]

    *! E038650,1 MAH 10/27/2004 Apply case Insensitive and english query if required [BEGIN]
    *-- lcFiltExp=IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
    *--           +IIF(&lcArray[lnCount,3]='D',")",'')+' '+IIF(&lcArray[lnCount,4],;
    *--            IIF(&lcArray[lnCount,5] = 'Like','=','=='),'<>')+' '+;
    *--           lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
    *--          &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+lcElmSep+' .AND. '+lcElmSep
    LCFILTEXP = IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 3] = 'D', 'DTOS(', '')) + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', IIF(&LCARRAY.[lnCount, 4], '', '!('), '') + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', 'UPPER(RTRIM(', '') + ;
      ALLTRIM(&LCARRAY.[lnCount, 1]) + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', '))', '') + ;
      IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 3] = 'D', ')', '')) + ;
      IIF(LLENGLISHQUERY, ;
      IIF(&LCARRAY.[lnCount, 4], ' like ', ' not like '), ;
      IIF(LLCASEINSENSITIVE  .AND. &LCARRAY.[lnCount, 3] $ 'CM', ;
      ' == ', ;
      IIF(&LCARRAY.[lnCount, 4], ;
      IIF(&LCARRAY.[lnCount, 5] = 'Like', ' = ', ' == '), ;
      ' <> '))) + ;
      LFRIGHTGET(&LCARRAY.[lnCount, 6], ;
      &LCARRAY.[lnCount, 3], ;
      &LCARRAY.[lnCount, 5], ;
      LCELMSEP, ;
      &LCARRAY.[lnCount, 7], ;
      LCDBENGINE, ;
      LLCASEINSENSITIVE, ;
      LLENGLISHQUERY) + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', IIF(&LCARRAY.[lnCount, 4], '', ')'), '') + ;
      LCELMSEP + ' .AND. ' + LCELMSEP
    *! E038650,1 MAH 10/27/2004 [END]

    *! E038142,2 MAH 08/31/2004 Get DB Engine type [BEGIN]
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
    LCFILTEXP = IIF(&LCARRAY[lnCount, 3] = 'D', 'CONVERT(CHAR(8), ', ' ') + ;
      LCLEFTSIDE + ;
      IIF(&LCARRAY[lnCount, 3] = 'D', ', 112) ', ' ') + ;
      IIF(&LCARRAY[lnCount, 4], '= ', '<> ') + ;
      LFRIGHTGET(&LCARRAY[lnCount, 6], ;
      &LCARRAY[lnCount, 3], ;
      &LCARRAY[lnCount, 5], ;
      LCELMSEP, ;
      &LCARRAY[lnCount, 7], ;
      LCDBENGINE) + ' ' + ;
      LCELMSEP + ' .AND. ' + LCELMSEP
  ENDCASE
  *! E038142,2 MAH 08/31/2004 [END]

CASE INLIST(&LCARRAY[lnCount,5],'Greater Than','Less Than','Greater Or Equal',;
    'Less Or Equal')
  *! E038142,2 MAH 08/31/2004 Build condition according to DB Engine [BEGIN]
  DO CASE
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
    *! E038142,2 MAH 08/31/2004 [END]

    *! E038650,1 MAH 10/27/2004 Apply case Insensitive and english query if required [BEGIN]
    *-- lcOperator=lfGetOper(ALLTRIM(&lcArray[lnCount,5]),&lcArray[lnCount,4])
    *-- lcFiltExp=IIF(&lcArray[lnCount,4],'','!(')+;
    *--         IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
    *--           +IIF(&lcArray[lnCount,3]='D',")",'')+' '+lcOperator+' '+;
    *--           lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
    *--          &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+IIF(&lcArray[lnCount,4],'',' ) ')+;
    *--          lcElmSep+' .AND. '+lcElmSep
    LCOPERATOR = LFGETOPER(ALLTRIM(&LCARRAY.[lnCount, 5]), &LCARRAY[lnCount, 4], LLENGLISHQUERY)
    LCFILTEXP  = IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 4], '', '!(')) + ;
      IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 3] = 'D', 'DTOS(', '')) + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', 'UPPER(RTRIM(', '') + ;
      ALLTRIM(&LCARRAY.[lnCount, 1]) + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', '))', '') + ;
      IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 3] = 'D',')', '')) + ;
      ' ' + LCOPERATOR + ' ' + ;
      LFRIGHTGET(&LCARRAY.[lnCount, 6], ;
      &LCARRAY.[lnCount, 3], ;
      &LCARRAY.[lnCount, 5], ;
      LCELMSEP, ;
      &LCARRAY.[lnCount, 7], ;
      LCDBENGINE, ;
      LLCASEINSENSITIVE, ;
      LLENGLISHQUERY) + ;
      IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 4], '', ' )')) + ;
      LCELMSEP + ' .AND. ' + LCELMSEP
    *! E038650,1 MAH 10/27/2004 [END]

    *! E038142,2 MAH 08/31/2004 Get DB Engine type [BEGIN]
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
    LCOPERATOR = LFGETOPER(ALLTRIM(&LCARRAY[lnCount, 5]), &LCARRAY[lnCount, 4])
    LCFILTEXP  = IIF(&LCARRAY[lnCount, 4], ' ' , 'NOT (') + ;
      IIF(&LCARRAY[lnCount, 3]='D', 'CONVERT(CHAR(8), ' ,' ') + ;
      LCLEFTSIDE + ;
      IIF(&LCARRAY[lnCount, 3] = 'D', ', 112) ', ' ') + ;
      LCOPERATOR + ' ' + ;
      LFRIGHTGET(&LCARRAY[lnCount, 6], ;
      &LCARRAY[lnCount, 3], ;
      &LCARRAY[lnCount, 5], ;
      LCELMSEP, ;
      &LCARRAY[lnCount, 7], ;
      LCDBENGINE) + ;
      IIF(&LCARRAY[lnCount, 4], ' ', ') ') + ;
      LCELMSEP + ' .AND. ' + LCELMSEP
  ENDCASE
  *! E038142,2 MAH 08/31/2004 [END]

CASE &LCARRAY[lnCount,5] = 'Between'
  IF LLFILTER
    *! E038650,1 MAH 10/27/2004 Apply case Insensitive if required [BEGIN]
    *-- lcFiltExp=IIF(&lcArray[lnCount,4],'BETWEEN(','!BETWEEN(')+;
    *--         IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
    *--             +IIF(&lcArray[lnCount,3]='D',")",'')+','+;
    *--             lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
    *--             &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
    *--             ')'+lcElmSep+' .AND. '+lcElmSep
    LCFILTEXP = IIF(&LCARRAY.[lnCount, 4], 'BETWEEN(', '!BETWEEN(') + ;
      IIF(&LCARRAY.[lnCount, 3] = 'D', 'DTOS(', '') + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', 'UPPER(RTRIM(', '') + ;
      ALLTRIM(&LCARRAY.[lnCount, 1]) + ;
      IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', '))', '') + ;
      IIF(&LCARRAY.[lnCount, 3] = 'D', ')', '') + ',' + ;
      LFRIGHTGET(&LCARRAY.[lnCount, 6], ;
      &LCARRAY.[lnCount, 3], ;
      &LCARRAY.[lnCount, 5], ;
      LCELMSEP, ;
      &LCARRAY.[lnCount, 7], ;
      LCDBENGINE, ;
      LLCASEINSENSITIVE, ;
      LLENGLISHQUERY) + ;
      ')' + LCELMSEP + ' .AND. ' + LCELMSEP
    *! E038650,1 MAH 10/27/2004 [END]
  ELSE
    *! E038142,2 MAH 08/31/2004 Build condition according to DB Engine [BEGIN]
    DO CASE
    CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
      *! E038142,2 MAH 08/31/2004 [END]

      *! E038650,1 MAH 10/27/2004 Apply case Insensitive and english query if required [BEGIN]
      *-- lcFiltExp = IIF(&lcArray[lnCount,4],'','!(')+;
      *--             IIF(&lcArray[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&lcArray[lnCount,1]);
      *--             +IIF(&lcArray[lnCount,3]='D',")",'')+' BETWEEN '+;
      *--             lfrightGet(&lcArray[lnCount,6],&lcArray[lnCount,3],;
      *--             &lcArray[lnCount,5],lcElmSep,&lcArray[lnCount,7])+;
      *--             IIF(&lcArray[lnCount,4],'',')')+lcElmSep+' .AND. '+lcElmSep
      LCFILTEXP = IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 4], '', '!(')) + ;
        IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 3] = 'D', 'DTOS(', '')) + ;
        IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', 'UPPER(RTRIM(', '') + ;
        ALLTRIM(&LCARRAY.[lnCount, 1]) + ;
        IIF(LLCASEINSENSITIVE .AND. &LCARRAY.[lnCount, 3] $ 'CM', '))', '') + ;
        IIF(LLENGLISHQUERY, '', IIF(&LCARRAY.[lnCount, 3] = 'D', ')', '')) + ;
        IIF(LLENGLISHQUERY, ;
        IIF(&LCARRAY.[lnCount, 4], ' between ', ' not between '), ;
        ' BETWEEN ') + ;
        LFRIGHTGET(&LCARRAY.[lnCount, 6], ;
        &LCARRAY.[lnCount, 3], ;
        &LCARRAY.[lnCount, 5], ;
        LCELMSEP, ;
        &LCARRAY.[lnCount, 7], ;
        LCDBENGINE, ;
        LLCASEINSENSITIVE, ;
        LLENGLISHQUERY) + ;
        IIF(LLENGLISHQUERY,  '', IIF(&LCARRAY.[lnCount, 4], '', ')')) + ;
        LCELMSEP + ' .AND. ' + LCELMSEP
      *! E038650,1 MAH 10/27/2004 [END]

      *! E038142,2 MAH 08/31/2004 Get DB Engine type [BEGIN]
    CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
      LCFILTEXP = IIF(&LCARRAY[lnCount, 4], ' ' , 'NOT ( ') + ;
        IIF(&LCARRAY[lnCount, 3]='D', 'CONVERT(CHAR(8), ', ' ') + ;
        LCLEFTSIDE + ;
        IIF(&LCARRAY[lnCount, 3] = 'D', ', 112) ', ' ') + ;
        ' BETWEEN '+ ;
        LFRIGHTGET(&LCARRAY[lnCount, 6], ;
        &LCARRAY[lnCount, 3], ;
        &LCARRAY[lnCount, 5], ;
        LCELMSEP, ;
        &LCARRAY[lnCount, 7], ;
        LCDBENGINE) + ;
        IIF(&LCARRAY[lnCount, 4], ' ', ') ') + ;
        LCELMSEP + ' .AND. ' + LCELMSEP
    ENDCASE
    *! E038142,2 MAH 08/31/2004 [END]
  ENDIF
CASE &LCARRAY[lnCount,5] = 'In List'
  IF &LCARRAY[lnCount,7] <> 'R'
    IF !LLUSEARRAY OR OCCURS("|",&LCARRAY[lnCount,6]) < 25
      IF LLFILTER
        LCFILTEXP=IIF(&LCARRAY[lnCount,4],'INLIST(','!INLIST(')+;
          IIF(&LCARRAY[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&LCARRAY[lnCount,1]);
          +IIF(&LCARRAY[lnCount,3]='D',")",'')+','+;
          LFRIGHTGET(&LCARRAY[lnCount,6],&LCARRAY[lnCount,3],;
          &LCARRAY[lnCount,5],LCELMSEP,&LCARRAY[lnCount,7])+;
          ')'+LCELMSEP+' .AND. '+LCELMSEP
      ELSE
        LCFILTEXP= IIF(&LCARRAY[lnCount,4],'','!(')+;
          IIF(&LCARRAY[lnCount,3]='D',"DTOS(",'')+ALLTRIM(&LCARRAY[lnCount,1]);
          +IIF(&LCARRAY[lnCount,3]='D',")",'')+' IN('+;
          LFRIGHTGET(&LCARRAY[lnCount,6],&LCARRAY[lnCount,3],;
          &LCARRAY[lnCount,5],LCELMSEP,&LCARRAY[lnCount,7])+')'+;
          IIF(&LCARRAY[lnCount,4],'',')')+LCELMSEP+' .AND. '+LCELMSEP
      ENDIF
    ELSE    && Else [If the function can use the filter array in the filter expression]
      DO CASE

      CASE &LCARRAY[lnCount,3] = 'N'
        *-- Variable to hold the Numeric field Picture
        LCPICT    = IIF(ATC(LCELMSEP , &LCARRAY[lnCount,6]) = 0 ,;
          &LCARRAY[lnCount,6] ,;
          SUBSTR(&LCARRAY[lnCount,6] , 1 ,;
          ATC(LCELMSEP , &LCARRAY[lnCount,6]) - 1))

        LCPICT    = STUF(REPLICATE('9' , LEN(LCPICT)) ,;
          ATC('.' , LCPICT) ,;
          IIF(ATC('.' , LCPICT) = 0 , 0 , 1) ,;
          IIF(ATC('.' , LCPICT) = 0 , '' , '.'))

        *-- Variable to hold the Numeric field Length (In character
        *-- string)
        LCLENGTH  = ALLTRIM(STR(LEN(LCPICT)))

        *-- Variable to hold the Numeric field Decimal Length (In
        *-- character string)
        LCDECIMAL = ALLTRIM(STR(IIF(AT('.' , LCPICT) = 0 , 0 ,;
          LEN(LCPICT) - AT('.' , LCPICT))))

        LCFILTEXP = IIF(&LCARRAY[lnCount,4] , '(' , '!(') +;
          'STR(' + ALLTRIM(&LCARRAY[lnCount,1]) +;
          ' , ' + LCLENGTH + ' , ' + LCDECIMAL + ')' +;
          ' $ ' + ALLTRIM(LCARRAY) +;
          '[' + ALLTRIM(STR(LNCOUNT)) + ',6]' + ')' +;
          LCELMSEP + ' .AND. ' + LCELMSEP

      CASE &LCARRAY[lnCount,3] = 'D'
        LCFILTEXP = IIF(&LCARRAY[lnCount,4] , '(' , '!(') +;
          'DTOC(' + ALLTRIM(&LCARRAY[lnCount,1]) + ')' +;
          ' $ ' + ALLTRIM(LCARRAY) +;
          '[' + ALLTRIM(STR(LNCOUNT)) + ',6]' + ')' +;
          LCELMSEP + ' .AND. ' + LCELMSEP

      OTHERWISE
        LCFILTEXP = IIF(&LCARRAY[lnCount,4] , '(' , '!(') +;
          ALLTRIM(&LCARRAY[lnCount,1]) +;
          ' $ '+ ALLTRIM(LCARRAY) +;
          '[' + ALLTRIM(STR(LNCOUNT)) + ',6]' + ')' +;
          LCELMSEP + ' .AND. ' + LCELMSEP

      ENDCASE
    ENDIF    && End of IF !llUseArray

    *In list operator [Begin]
  ELSE    && Else [If this filter option is using the In range screen]

    IF USED(&LCARRAY[lnCount,6]) .AND. SEEK('' , &LCARRAY[lnCount,6])
      PRIVAT LNSAVALS , LNSELCNT , LCINLSTEXP , LCOPTEXP , LCKEY
      LNSAVALS   = SELECT(0)        && Save the old alias.
      LNSELCNT   = 0                && Variable to be used to count the user selection
      LCINLSTEXP = ""               && Variable to hold the user selection
      LCKEY      = ''

      *-- Get an optimize-able expression, if applicable.
      LCOPTEXP   = LFGETOPTEX(LCARRAY , LNCOUNT , @LCKEY)

      SELECT (&LCARRAY[lnCount,6])

      *-- Scan loop to count and get the user selection as far as it
      *-- doesn't exceeds 24 selection.
      SCAN WHILE LNSELCNT <= 25
        *-- count the user selection
        LNSELCNT   = LNSELCNT   + 1
        *-- Accumulate the key - if there is one - and the user selection
        *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[Start]
        *lcInlstExp = lcInlstExp + ',"' + lcKey + EVAL(KEY()) + '"'
        * B609993,1 MMT 07/10/2012 Error in Approve for payment screen if Vendor code has '[T20120620.0041][Start]
        *lcInlstExp = lcInlstExp + ",'" + lcKey + EVAL(KEY()) + "'"
        IF ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
          LCINLSTEXP = LCINLSTEXP + ",[" + LCKEY + EVAL(KEY()) + "]"
        ELSE
          LCINLSTEXP = LCINLSTEXP + ",'" + LCKEY + STRTRAN(EVAL(KEY()),"'","''") + "'"
        ENDIF
        * B609993,1 MMT 07/10/2012 Error in Approve for payment screen if Vendor code has '[T20120620.0041][END]
        *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[End]
      ENDSCAN    && End of SCAN WHILE lnSelCnt <= 25

      *-- If the user selected more than 24 selection
      IF LNSELCNT > 24

        *-- Use BETWEEN() function, if applicable.
        IF &LCARRAY[lnCount,4]
          GO TOP
          *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[Start]
          *!*	            lcFiltExp = 'BETWEEN(' + lcOptExp +;
          *!*	                        ',"' + lcKey + EVAL(KEY()) + '",'
          * B609993,1 MMT 07/10/2012 Error in Approve for payment screen if Vendor code has '[T20120620.0041][Start]
          *lcFiltExp = 'BETWEEN(' + lcOptExp +;
          ",'" + lcKey + EVAL(KEY()) + "',"
          IF ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
            LCFILTEXP = 'BETWEEN(' + LCOPTEXP +;
              ",[" + LCKEY + EVAL(KEY()) + "],"
          ELSE
            LCFILTEXP = 'BETWEEN(' + LCOPTEXP +;
              ",'" + LCKEY + STRTRAN(EVAL(KEY()),"'","''") + "',"
          ENDIF
          * B609993,1 MMT 07/10/2012 Error in Approve for payment screen if Vendor code has '[T20120620.0041][END]
          *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[End]
          GO BOTTOM
          *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[Start]
          *!*	            lcFiltExp = lcFiltExp + '"' + lcKey + EVAL(KEY()) + '")' +;
          *!*	                        lcElmSep + ' .AND. ' + lcElmSep
          *lcFiltExp = lcFiltExp + "'" + lcKey + EVAL(KEY()) + "')" +;
          lcElmSep + ' .AND. ' + lcElmSep

          * B609993,1 MMT 07/10/2012 Error in Approve for payment screen if Vendor code has '[T20120620.0041][Start]
          IF ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
            LCFILTEXP = LCFILTEXP + "[" + LCKEY + EVAL(KEY()) + "])" +;
              LCELMSEP + ' .AND. ' + LCELMSEP
          ELSE
            LCFILTEXP = LCFILTEXP + "'" + LCKEY + STRTRAN(EVAL(KEY()),"'","''") + "')" +;
              LCELMSEP + ' .AND. ' + LCELMSEP
          ENDIF
          * B609993,1 MMT 07/10/2012 Error in Approve for payment screen if Vendor code has '[T20120620.0041][END]
          *B609328,1 MMT 07/01/2010 material master file listing crashes when fabric has "'"[End]

        ENDIF    && End of IF &lcArray[lnCount,4]

        *-- Use the SEEK() function
        LCFILTEXP = LCFILTEXP + IIF(&LCARRAY[lnCount,4] , '' , '!') + 'SEEK(' + ;
          ALLTRIM(&LCARRAY[lnCount,1]) + ' , "' + ;
          &LCARRAY[lnCount,6] + '")' +;
          LCELMSEP + ' .AND. ' + LCELMSEP

        *-- Use INLIST when the selection less than 24
      ELSE    && Else (If the user selected 24 selection or less.)
        LCFILTEXP = 'INLIST(' + LCOPTEXP + LCINLSTEXP + ;
          ')' + LCELMSEP + ' .AND. ' + LCELMSEP
      ENDIF
      SELECT (LNSAVALS)

    ELSE    && Else [If the In Range cursor is not opened or if it has no records]
      LCFILTEXP = ''
    ENDIF    && End of IF USED(&lcArray[lnCount,6]) .AND. SEEK('' , &lcArray[lnCount,6])
  ENDIF    && End of IF &lcArray[lnCount,7] <> 'R'

ENDCASE

*-- BADRAN (N000398,1 - Build Aria3 Option Grid)
*-- IF it's 8 dimension array, save the filter expression to column 8...BEGIN
IF ALEN(&LCARRAY.,2) = 8
  LOCAL LNATELMSEP
  LNATELMSEP = RAT(LCELMSEP,LCFILTEXP,2)
  &LCARRAY.[lnCount,8] = LEFT(LCFILTEXP,LNATELMSEP-1)
ENDIF
*-- IF it's 8 dimension array, save the filter expression to column 8...END
RETURN LCFILTEXP &&
*--  end of lfGetQCond.

*!*****************************************************************************************
*! Name      : lfGetOper
*! Developer :
*! Date      : 10/13/2002 10:41:02 AM
*! Purpose   : Get Filter operator
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters:
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*! Modifications :
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
*!****************************************************************************************
FUNCTION LFGETOPER

*! E038650,1 MAH 10/27/2004 Add new parameter to return english filter string [BEGIN]
*-- PARAMETER lcOperator,llisnot
PARAMETERS LCOPERATOR, LLISNOT, LLENGLISHQUERY
*! E038650,1 MAH 10/27/2004 [END]

*! E038650,1 MAH 10/27/2004 Apply return english query if required [BEGIN]
IF LLENGLISHQUERY
  DO CASE
  CASE LCOPERATOR = 'Greater Than'
    RETURN IIF(LLISNOT, '', 'not ') + 'greater than'

  CASE LCOPERATOR = 'Less Than'
    RETURN IIF(LLISNOT, '', 'not ') + 'less than'

  CASE LCOPERATOR = 'Greater Or Equal'
    RETURN IIF(LLISNOT, '', 'not ') + 'greater or equal'

  CASE LCOPERATOR = 'Less Or Equal'
    RETURN IIF(LLISNOT, '', 'not ') + 'less or equal'
  ENDCASE
ELSE
  *! E038650,1 MAH 10/27/2004 [END]
  DO CASE
  CASE LCOPERATOR = 'Greater Than'
    RETURN '>'
  CASE LCOPERATOR = 'Less Than'
    RETURN '<'
  CASE LCOPERATOR = 'Greater Or Equal'
    RETURN '>='
  CASE LCOPERATOR = 'Less Or Equal'
    RETURN '<='
  ENDCASE
  *! E038650,1 MAH 10/27/2004 Apply return english query if required [BEGIN]
ENDIF
*! E038650,1 MAH 10/27/2004 [END]

*-- end of lfGetOper.

*!*****************************************************************************************
*! Name      : lfRightGet
*! Developer :
*! Date      : 10/13/2002 10:41:47 AM
*! Purpose   : Get Right value
*! Entry no. : N000398,1 - Build Aria3 Option Grid
*!*****************************************************************************************
*! Parameters:
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*! Modifications :
*! 08/31/2004 Full support for run forms with SQL with high Performance.
*! E038650,1 MAH 10/27/2004 Enhance the Filter.
*! E037885,2 MAH 12/03/2004 Separate screen in different session.
*! B127002,1 MAH 04/05/2005 Greater than condition not returning correct values.
*! B039435,1 MAH Fix bug of there is a quote in the field value.
*! B129154,1 MAH 07/28/2005 Error in PO Report.
*!****************************************************************************************

FUNCTION LFRIGHTGET
*! E038142,2 MAH 08/31/2004 Add new paramter to pass the each table engine lcDBEngine [BEGIN]
*-- PARAMETERS mRightHead,cLeftType,cOperator,lcElmSep,cRightType

*! E038650,1 MAH 10/27/2004 Add new parameters: remove case sensitive and another to return english filter string [BEGIN]
*-- PARAMETERS mRightHead, cLeftType, cOperator, lcElmSep, cRightType, lcDBEngine
PARAMETERS MRIGHTHEAD, CLEFTTYPE, COPERATOR, LCELMSEP, CRIGHTTYPE, LCDBENGINE, LLCASEINSENSITIVE, LLENGLISHQUERY
*! E038650,1 MAH 10/27/2004 [END]

LCDBENGINE = IIF(TYPE('lcDBEngine') = 'C', LCDBENGINE, OARIAAPPLICATION.CNATIVEDBID)
*! E038142,2 MAH 08/31/2004 [END]

LCRETVAL=MRIGHTHEAD
DO CASE
CASE CRIGHTTYPE='V'
  DO CASE
  CASE CLEFTTYPE $ 'CM'
    IF INLIST(COPERATOR,'Between','In List')
      *! E038142,2 MAH 08/31/2004 Apply return english query parameters [BEGIN]
      *-- lcSeper=IIF(!llFilter AND cOperator='Between',' AND ',',')
      LCSEPER = IIF(!LLFILTER AND COPERATOR = 'Between', IIF(LLENGLISHQUERY, ' and ', ' AND '), ',')
      *! E038142,2 MAH 08/31/2004 [END]

      *! E038142,2 MAH 08/31/2004 Apply remove case sensitive  parameters [BEGIN]
      *-- lcRetVal='"'+STRTRAN(mRightHead,lcElmSep,'"'+lcSeper+'"')+'"'

      *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
      *-- IF llCaseInsensitive
      *--   IF !EMPTY(mRightHead)
      *--     LOCAL lcRight
      *--     lcRight = UPPER(mRightHead)
      *--
      *--     LOCAL laRight[1]
      *--     =gfSubStr(lcRight, @laRight, lcElmSep)
      *--
      *--     lcRight = ''
      *--     LOCAL lnIndex
      *--     FOR lnIndex = 1 TO ALEN(laRight, 1)
      *--       lcRight = lcRight + laRight[lnIndex] + lcElmSep
      *--     ENDFOR
      *--     lcRight = SUBSTR(lcRight, 1, LEN(lcRight) - 1)
      *--   ENDIF
      *--
      *--   lcRetVal = '"' + STRTRAN(UPPER(lcRight), lcElmSep, '"' + lcSeper + '"') + '"'
      *-- ELSE
      *--   lcRetVal = '"' + STRTRAN(mRightHead, lcElmSep, '"' + lcSeper + '"') + '"'
      *-- ENDIF

      IF !EMPTY(MRIGHTHEAD)
        LOCAL LCRIGHT

        IF LLCASEINSENSITIVE
          LCRIGHT = UPPER(MRIGHTHEAD)
          *! B129154,1 MAH 07/28/2005 [BEGIN]
        ELSE
          LCRIGHT = MRIGHTHEAD
          *! B129154,1 MAH 07/28/2005 [END]
        ENDIF

        LOCAL LARIGHT[1]
        =GFSUBSTR(LCRIGHT, @LARIGHT, LCELMSEP)

        LCRIGHT = ''
        LOCAL LNINDEX
        FOR LNINDEX = 1 TO ALEN(LARIGHT, 1)
          LCRIGHT = LCRIGHT + GFSTRTOEXP(LARIGHT[lnIndex], LCDBENGINE, LLENGLISHQUERY) + LCELMSEP
        ENDFOR

        LCRIGHT = SUBSTR(LCRIGHT, 1, LEN(LCRIGHT) - 1)
      ENDIF

      LCRETVAL = STRTRAN(UPPER(LCRIGHT), LCELMSEP, LCSEPER)
      *! B039435,1 MAH [END]
      *! E038142,2 MAH 08/31/2004 [END]
    ELSE
      *! E038142,2 MAH 08/31/2004 Apply remove case sensitive  parameters [BEGIN]
      *-- RETURN '"'+IIF(cOperator='Contains',ALLTRIM(mrightHead),mrightHead)+'"'

      *! B127002,1 MAH 04/05/2005 Greater than condition not returning correct values [BEGIN]
      IF COPERATOR = 'Greater Than'
        IF LLCASEINSENSITIVE
          *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
          *-- RETURN '"' + RTRIM(UPPER(mrightHead)) + ' "'
          RETURN GFSTRTOEXP(RTRIM(UPPER(MRIGHTHEAD)), LCDBENGINE, LLENGLISHQUERY)
          *! B039435,1 MAH [END]
        ELSE
          *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
          *-- RETURN '"' + RTRIM(mrightHead) + ' "'
          RETURN GFSTRTOEXP(RTRIM(MRIGHTHEAD), LCDBENGINE, LLENGLISHQUERY)
          *! B039435,1 MAH [END]
        ENDIF
      ELSE
        *! B127002,1 MAH 04/05/2005[END]
        IF LLCASEINSENSITIVE
          *! E037885,2 MAH 12/03/2004 [BEGIN]
          *-- RETURN '"' + IIF(cOperator = 'Contains', UPPER(RTRIM(mrightHead)), UPPER(mrightHead)) + '"'
          *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
          *-- RETURN '"' + IIF(cOperator = 'Contains', UPPER(RTRIM(mrightHead)), RTRIM(UPPER(mrightHead))) + '"'
          RETURN GFSTRTOEXP(IIF(COPERATOR = 'Contains', UPPER(RTRIM(MRIGHTHEAD)), RTRIM(UPPER(MRIGHTHEAD))), LCDBENGINE, LLENGLISHQUERY)
          *! B039435,1 MAH [END]
          *! E037885,2 MAH 12/03/2004 [END]
        ELSE
          *! E037885,2 MAH 12/03/2004 [BEGIN]
          *-- RETURN '"' + IIF(cOperator = 'Contains', ALLTRIM(mrightHead), mrightHead) + '"'
          *! B039435,1 MAH Fix bug of there is a quote in the field value [BEGIN]
          *-- RETURN '"' + IIF(cOperator = 'Contains', ALLTRIM(mrightHead), RTRIM(mrightHead)) + '"'
          RETURN GFSTRTOEXP(IIF(COPERATOR = 'Contains', ALLTRIM(MRIGHTHEAD), RTRIM(MRIGHTHEAD)), LCDBENGINE, LLENGLISHQUERY)
          *! B039435,1 MAH [END]
          *! E037885,2 MAH 12/03/2004 [END]
        ENDIF
        *! E038142,2 MAH 08/31/2004 [END]

        *! B127002,1 MAH 04/05/2005 Greater than condition not returning correct values [BEGIN]
      ENDIF
      *! B127002,1 MAH 04/05/2005[END]

    ENDIF
  CASE CLEFTTYPE = 'N'
    *! E038142,2 MAH 08/31/2004 Apply return english query parameters [BEGIN]
    *-- lcSeper=IIF(COPERATOR='Between' AND !llFilter,' AND ',',')
    LCSEPER = IIF(COPERATOR='Between' AND !LLFILTER, IIF(LLENGLISHQUERY, ' and ', ' AND '), ',')
    *! E038142,2 MAH 08/31/2004 [END]

    LCRETVAL=STRTRAN(MRIGHTHEAD,LCELMSEP,LCSEPER)
    IF EMPTY(LCRETVAL)
      LCRETVAL='0'
    ENDIF

  CASE CLEFTTYPE = 'D'
    LCOLDCEN = SET('CENT')
    SET CENTURY ON
    IF INLIST(COPERATOR,'Between','In List')
      LOCAL LNDATES, LCRIGHTHAND, LNDATE, LCDATE
      LCRIGHTHAND = LCELMSEP + ALLTRIM(MRIGHTHEAD) + LCELMSEP
      LNDATES = OCCURS(LCELMSEP,LCRIGHTHAND) - 1
      LCRETVAL = ""
      FOR LNDATE = 1 TO LNDATES
        LCDATE = STREXTRACT(LCRIGHTHAND,LCELMSEP,LCELMSEP,LNDATE)


        *! E038650,1 MAH 01/18/2005 [BEGIN]
        *-- lcRetVal = lcRetVal + ",'" + CDateToString(lcDate) + "'"
        IF !LLFILTER AND COPERATOR = 'Between'
          IF LLENGLISHQUERY
            IF EMPTY(LCRETVAL)
              LCRETVAL = LCRETVAL + " '" + CDATETOSTRING(LCDATE) + "'"
            ELSE
              LCRETVAL = LCRETVAL + " and '" + CDATETOSTRING(LCDATE) + "'"
            ENDIF
          ELSE
            IF EMPTY(LCRETVAL)
              LCRETVAL = LCRETVAL + " '" + CDATETOSTRING(LCDATE) + "'"
            ELSE
              LCRETVAL = LCRETVAL + " AND '" + CDATETOSTRING(LCDATE) + "'"
            ENDIF
          ENDIF
        ELSE
          LCRETVAL = LCRETVAL + ",'" + CDATETOSTRING(LCDATE) + "'"
        ENDIF
        *! E038650,1 MAH 01/18/2005 [END]
      ENDFOR
      LCRETVAL = SUBSTR(LCRETVAL,2)

      *--             lcSeper=IIF(!llFilter AND cOperator='Between',' AND ALLTRIM(DTOS(',',ALLTRIM(DTOS(')
      *--              lcRetVal='ALLTRIM(DTOS({  '+STRTRAN(ALLTRIM(mRightHead),lcElmSep,'  }))'+lcSeper+'{  ')+'  }))'
    ELSE
      IF BETWEEN(YEAR(CTOD(MRIGHTHEAD)) ,1900,1950)
        LCTMPYEAR  = ALLTRIM(STR(YEAR(CTOD(MRIGHTHEAD)) + 100))
        MRIGHTHEAD = SUBSTR(MRIGHTHEAD,1,6)+LCTMPYEAR
      ENDIF
      *lcRetVal='ALLTRIM(DTOS({  '+ALLTRIM(MRIGHTHEAD)+'  }))'
      *! E038142,2 MAH 08/31/2004 Correct the code [BEGIN]
      *-- lcRetVal = CDateToString(mRightHead)
      LCRETVAL = "'" + CDATETOSTRING(MRIGHTHEAD) + "'"
      *! E038142,2 MAH 08/31/2004 [END]
    ENDIF
    SET CENTURY &LCOLDCEN

  CASE CLEFTTYPE = 'L'
    *! E038142,2 MAH 08/31/2004 Return value condition according to DB Engine [BEGIN]
    *-- RETURN ' '+lcRetVal+' '
    DO CASE
    CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
      *B609526,1 MMT 02/17/2011 Error in Automatic allocation screen OG[T20110201.0005][Start]
      *RETURN ' ' + lcRetVal + ' '
      RETURN ' ' + IIF(TYPE('lcRetVal')='L', IIF(LCRETVAL = .T.,".T.",".F."),LCRETVAL)+ ' '
      *B609526,1 MMT 02/17/2011 Error in Automatic allocation screen OG[T20110201.0005][End]

    CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
      *B608900,1 WAM 06/21/2009 Fix expression
      *RETURN ' ' + IIF(UPPER(ALLTRIM(lcRetVal)) = '.T.' .OR. UPPER(ALLTRIM(lcRetVal)) = 'T', 1, 0) + ' '
      RETURN ' ' + IIF(UPPER(ALLTRIM(LCRETVAL)) = '.T.' .OR. UPPER(ALLTRIM(LCRETVAL)) = 'T', '1','0') + ' '
      *B608900,1 WAM 06/21/2009 (End)
    ENDCASE
    *! E038142,2 MAH 08/31/2004 [END]
  ENDCASE

CASE CRIGHTTYPE='F'
  *! E038142,2 MAH 08/31/2004 Apply remove case sensitive  parameters [BEGIN]
  *-- lcRetVal=STRTRAN(IIF(cOperator='Contains',ALLTRIM(mrightHead),mrightHead),lcElmSep,',')
  *! E038650,1 MAH 01/18/2005 [BEGIN]
  DO CASE
  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
    IF CLEFTTYPE $ 'MC'
      IF LLCASEINSENSITIVE
        LCRETVAL = 'UPPER(RTRIM(' + ALLTRIM(MRIGHTHEAD) + '))'
      ELSE
        LCRETVAL = STRTRAN(IIF(COPERATOR = 'Contains', ALLTRIM(MRIGHTHEAD) , MRIGHTHEAD), LCELMSEP, ',')
      ENDIF
    ENDIF

    IF CLEFTTYPE $ 'NL'
      LCRETVAL = ALLTRIM(MRIGHTHEAD)
    ENDIF

    IF CLEFTTYPE $ 'D'
      LCRETVAL = 'DTOS(' + ALLTRIM(MRIGHTHEAD) + ')'
    ENDIF

  CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
    LCRETVAL = '[' + ALLTRIM(MRIGHTHEAD) + ']'
    IF CLEFTTYPE = 'D'
      LCRETVAL = ' CONVERT(CHAR(8),' + LCRETVAL + ', 112) '
    ENDIF
  ENDCASE
  *! E038650,1 MAH 01/18/2005 [END]
  *! E038142,2 MAH 08/31/2004 [END]
ENDCASE

IF INLIST(COPERATOR,'Between','In List') AND EMPTY(ALLTRIM(MRIGHTHEAD))
  *! E038142,2 MAH 08/31/2004 Apply return english query parameters [BEGIN]
  *-- lcSeper=IIF(!llFilter AND cOperator='Between',' AND ',',')
  LCSEPER = IIF(!LLFILTER AND COPERATOR='Between', IIF(LLENGLISHQUERY, ' and ', ' AND '), ',')
  *! E038142,2 MAH 08/31/2004 [END]

  LCRETVAL=LCRETVAL+LCSEPER+LCRETVAL
ENDIF
RETURN LCRETVAL
*-- end of lfRightGet. DTOS

*!*************************************************************
*! Name      : lfGetOptEx
*! Developer : HS (Haytham El_Sheltawi)
*! Date      : 05/25/1999
*! Purpose   : create an optimize-able expression - if applicable -
*!             for the In Range options.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetOptEx()
*!*************************************************************
*B602931,4 this function was added by HS for the bug entry B602931,4.
*!*************************************************************
FUNCTION LFGETOPTEX

PARAMETERS LCARRAY , LNROW , LCKEY

LCKEY = ''

PRIVATE LNARRAYROW , LCFILE , LCFIELD , LCKEYEXPR , LCOPTEXP , LNSELECT ,;
  LNCOUNT , LAOPTEXP

LNARRAYROW = ASCAN(LARANGEINFO, UPPER(ALLTRIM(&LCARRAY.[lnRow,1])))

*-- If the filter option is of type expression.
IF LNARRAYROW = 0
  RETURN ALLTRIM(&LCARRAY[lnRow,1])
ENDIF
LNARRAYROW = ASUBSCRIPT(LARANGEINFO, LNARRAYROW , 1)


*-- Get the file of the filter option
LCFILE = SUBSTR(ALLTRIM(&LCARRAY[lnRow,1]) ,;
  1 , AT('.' , ALLTRIM(&LCARRAY[lnRow,1])) - 1)

*-- If the file of the filter option is not open, we will not be able to
*-- get the indexes
IF !USED(LCFILE)
  RETURN ALLTRIM(&LCARRAY[lnRow,1])
ENDIF

*-- Get the field of the filter option
LCFIELD = UPPER(SUBSTR(ALLTRIM(&LCARRAY[lnRow,1]) ,;
  AT('.' , ALLTRIM(&LCARRAY[lnRow,1])) + 1))

*-- Get the key expression that was passed to the In Range function
LCKEYEXPR = IIF(AT('FOR' , LARANGEINFO[lnArrayRow,2]) = 0 ,;
  LARANGEINFO[lnArrayRow,2] ,;
  SUBSTR(LARANGEINFO[lnArrayRow,2] , 1 ,;
  AT('FOR' , LARANGEINFO[lnArrayRow,2]) - 1))

LNSELECT = SELECT(0)        && Save the alias
SELECT (LCFILE)

LCOPTEXP = ''
LNCOUNT  = 0

*-- DO WHILE loop to search for an index that starts with the field of the
*-- filter option.
DO WHILE LCOPTEXP <> LCFIELD
  LNCOUNT  = LNCOUNT + 1
  LCOPTEXP = KEY(LNCOUNT)
  IF EMPTY(LCOPTEXP)
    EXIT
  ENDIF
ENDDO    && End of DO WHILE lcOptExp <> lcField

*-- If we didn't find an index that starts with the field of the filter
*-- option we are going to search for one that starts with the same key
*-- used with the In Range function
IF EMPTY(LCOPTEXP) .AND. TYPE(LCKEYEXPR) = 'C'

  IF LEN(&LCKEYEXPR) = 0
    LNCOUNT  = 0
    DO WHILE LCOPTEXP <> LCFIELD
      LNCOUNT  = LNCOUNT + 1
      LCOPTEXP = KEY(LNCOUNT)
      IF EMPTY(LCOPTEXP)
        EXIT
      ENDIF    && End of IF EMPTY(lcOptExp)
    ENDDO    && End of DO WHILE lcOptExp <> laBrFldFlt[lnArrayRow,4] + ...

    *-- If we found an index that can be used
    IF !EMPTY(LCOPTEXP)
      *-- Get the key that will be used with this index expression
      LCKEY = &LCKEYEXPR
    ENDIF    && End of IF !EMPTY(lcOptExp)
  ENDIF    && End of IF LEN(&lcKeyExpr) = LEN(&laBrFldFlt[lnArrayRow,4])
ENDIF    && End of EMPTY(lcOptExp) .AND. TYPE(laBrFldFlt[lnArrayRow,4]) ...

SELECT (LNSELECT)

*-- If we did not find an index that can be used
IF EMPTY(LCOPTEXP)
  RETURN ALLTRIM(&LCARRAY[lnRow,1])
ELSE    && Else (If we found an index that can be used)
  DIMENSION LAOPTEXP[1,1]
  =GFSUBSTR(LCOPTEXP , @LAOPTEXP , '+')
  LCOPTEXP = ''

  *-- FOR loop to add the alias to the index expression
  FOR LNCOUNT = 1 TO ALEN(LAOPTEXP , 1)
    LCOPTEXP = LCOPTEXP + IIF(EMPTY(LCOPTEXP) , '' , '+') +;
      IIF(OCCURS('(' , LAOPTEXP[lnCount,1]) = 0 ,;
      LCFILE + '.' + LAOPTEXP[lnCount,1] ,;
      STUFF(LAOPTEXP[lnCount,1] ,;
      RAT('(' , LAOPTEXP[lnCount,1]) + 1 , 0 ,;
      LCFILE + '.'))

  ENDFOR    && End of FOR lnCount = 1 TO ALEN(laOptExp , 1)
  RETURN LCOPTEXP
ENDIF    && End of IF EMPTY(lcOptExp)
*-- end of lfGetOptEx.
*-- Added by Badran in 10/13/2002 ... END

*!************************************************************************************
*! Name : GetExpr.
*!************************************************************************************
*! Synopsis : Build an expression
*!************************************************************************************
*! Passed :
*!        Parameters :
*!           lcExp       : The Default Expression
*!           lcFile      : The File to use it's fields
*!           lcExpPrmpt  : The Title of the expression builder
*!           la_Field    : The name of the fields header array
*!           laField     : The phiscal fields array
*!           llAddAlias  : add alias to the expression or not
*!           lcType      : the type of expression that will return
*!           laFiles     : files is used
*!           la_Var      : array that holds variable that can be use in expression
*!************************************************************************************
*! Returned : Expression String
*!************************************************************************************
*! Example :
*!
*!************************************************************************************
FUNCTION GETEXPR
PARAMETER LCEXP,LCFILE,LCEXPPRMPT,LA_FIELD,LAFIELD,LLADDALIAS,LCTYPE,LAFILES,LA_VAR
*SET CLASSLIB TO r:\aria4\classes\utility.vcx
PRIVATE OEXPR
OEXPR=CREATEOBJECT("ariaexpression",LCEXP,LCFILE,LCEXPPRMPT,LA_FIELD,@LAFIELD,LLADDALIAS,LCTYPE,LAFILES,LA_VAR)

IF !EMPTY(OEXPR.LAFIELD) AND !('U' $ OEXPR.LCTYPE)
  FOR LNCOUNT = 1 TO ALEN(OEXPR.LAFIELD)
    LCFLDNAME = OEXPR.LAFIELD[lnCount]
    DO CASE
    CASE LEFT(OEXPR.LAFIELD[lnCount],1)="M" OR LEFT(OEXPR.LAFIELD[lnCount],1)="C"
      &LCFLDNAME = " "
    CASE LEFT(OEXPR.LAFIELD[lnCount],1)="N"
      &LCFLDNAME  = 1
    CASE LEFT(OEXPR.LAFIELD[lnCount],1)="D"
      &LCFLDNAME = DATE()
    ENDCASE
  ENDFOR
ELSE
  IF TYPE('oExpr.laFiles') $ 'UL' AND EMPTY(OEXPR.LAFIELD)
    OEXPR.LAFIELD = " "
  ENDIF
ENDIF
OEXPR.SHOW
LCRETU = OEXPR.MVEXP
RETURN LCRETU
*!***********************************************************************************************************************
*! Name      	: lfGetConditionLeftSide
*! Developer 	: Mahmoud Said [MAH]
*! Date      	: 08/31/2004
*! Purpose   	: Get the left side name, which is used in any expression.
*! Tracking   	: E038142,2 MAH 08/31/2004 Full support for run forms with SQL with high Performance.
*!***********************************************************************************************************************
*! Parameters  	:
*!  lcExpression : Expression
*!  lcDBEngine   : DB engine ID
*!***********************************************************************************************************************
*! Returns 	: Table name
*!***********************************************************************************************************************
FUNCTION LFGETCONDITIONLEFTSIDE
LPARAMETERS LCEXPRESSION, LCDBENGINE

LCDBENGINE = IIF(TYPE('lcDBEngine') = 'C', LCDBENGINE, OARIAAPPLICATION.CNATIVEDBID)

LOCAL LCTORETEXPRESSION
LCTORETEXPRESSION = ''

DO CASE
CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CNATIVEDBID
  RETURN LCEXPRESSION

CASE ALLTRIM(LCDBENGINE) == OARIAAPPLICATION.CSQLDBID
  IF AT('(', LCEXPRESSION) = 0
    LCTORETEXPRESSION = LCEXPRESSION
  ELSE
    *-- Convert SUBSTR   , PADR, UPPER, DTOS   , STR, ALLTRIM, LEFT, LOWER to the new list
    *-- Convert SUBSTRING,     ,      , CONVERT, STR, LTRIM  , LEFT,
    LOCAL LNINDEX
    FOR LNINDEX = 1 TO LEN(LCEXPRESSION)
      IF SUBSTR(LCEXPRESSION, LNINDEX, 1) == "("

        *-- Get Command
        LOCAL LNINTERNALINDEX
        FOR LNINTERNALINDEX = LNINDEX - 1 TO 1 STEP -1
          IF LNINTERNALINDEX = 1
            EXIT
          ENDIF

          IF !ISALPHA(SUBSTR(LCEXPRESSION, LNINTERNALINDEX, 1))
            EXIT
          ENDIF
        ENDFOR

        LOCAL LCCOMMAND, LCREPLACE
        LCREPLACE = ''

        *-- Replace command
        LCCOMMAND = UPPER(ALLTRIM(SUBSTR(LCEXPRESSION, LNINTERNALINDEX + 1, LNINDEX - LNINTERNALINDEX - 1)))
        DO CASE
        CASE LCCOMMAND == 'SUBSTR'
          LCREPLACE = 'SUBSTRING'

        CASE LCCOMMAND == 'PADR'
          LCREPLACE = ''

        CASE LCCOMMAND == 'UPPER'
          LCREPLACE = ''

        CASE LCCOMMAND == 'LOWER'
          LCREPLACE = ''

        CASE LCCOMMAND == 'ALLTRIM'
          LCREPLACE = 'LTRIM'

        CASE LCCOMMAND == 'DTOS'
          LCREPLACE = 'CONVERT(CHAR(8), '
        ENDCASE

        LCTORETEXPRESSION = LEFT(LCTORETEXPRESSION, LEN(LCTORETEXPRESSION) - LEN(LCCOMMAND))
        LCTORETEXPRESSION = LCTORETEXPRESSION + LCREPLACE

        *-- Handle DTOS
        IF LCCOMMAND == 'DTOS'
          LOCAL LNBRACKETS
          LNBRACKETS = 1

          FOR LNINTERNALINDEX = LNINDEX + 1 TO LEN(LCEXPRESSION)
            LOCAL LCCURRENTCHAR
            LCCURRENTCHAR = SUBSTR(LCEXPRESSION, LNINTERNALINDEX, 1)
            LNBRACKETS = IIF(LCCURRENTCHAR = ')', -1, IIF(LCCURRENTCHAR = '(', 1, 0))

            IF LNBRACKETS = 0
              LCEXPRESSION = LEFT(LCEXPRESSION, LNINTERNALINDEX - 1) + ;
                '|' + ;
                SUBSTR(LCEXPRESSION, LNINTERNALINDEX + 1)
            ENDIF
          ENDFOR
        ENDIF
      ELSE
        IF SUBSTR(LCEXPRESSION, LNINDEX, 1) = "|"
          LCTORETEXPRESSION = LCTORETEXPRESSION + ', 112)'
        ELSE
          LCTORETEXPRESSION = LCTORETEXPRESSION + SUBSTR(LCEXPRESSION, LNINDEX, 1)
        ENDIF
      ENDIF
    ENDFOR
  ENDIF

  *-- Replace fields names with [FieldName]
  LOCAL LNDOTNO, LNDOTPOS
  LNDOTNO = 1

  LNDOTPOS = AT('.', LCTORETEXPRESSION, LNDOTNO)
  DO WHILE LNDOTPOS > 0
    LOCAL LNINDEX
    FOR LNINDEX = LNDOTPOS + 1 TO LEN(LCTORETEXPRESSION)
      IF LNINDEX = LEN(LCTORETEXPRESSION)
        LCTORETEXPRESSION = SUBSTR(LCTORETEXPRESSION, 1, LNDOTPOS) + ;
          '[' + ;
          SUBSTR(LCTORETEXPRESSION, LNDOTPOS + 1, LNINDEX - LNDOTPOS) + ;
          ']'
      ELSE
        * MAH T20080331.0008 20 May 2008 Check Also for underscor
        *-- IF !ISALPHA(SUBSTR(lcToRetExpression, lnIndex, 1))
        IF !ISALPHA(SUBSTR(LCTORETEXPRESSION, LNINDEX, 1)) .AND. !(SUBSTR(LCTORETEXPRESSION, LNINDEX, 1) = "_")
          * MAH T20080331.0008 20 May 2008 Check Also for underscor
          LCTORETEXPRESSION = SUBSTR(LCTORETEXPRESSION, 1, LNDOTPOS) + ;
            '[' + ;
            SUBSTR(LCTORETEXPRESSION, LNDOTPOS + 1, LNINDEX - LNDOTPOS - 1) + ;
            ']' + ;
            SUBSTR(LCTORETEXPRESSION, LNINDEX)
          EXIT
        ENDIF
      ENDIF
    ENDFOR

    LNDOTNO  = LNDOTNO + 1
    LNDOTPOS = AT('.', LCTORETEXPRESSION, LNDOTNO)
  ENDDO

  RETURN LCTORETEXPRESSION
ENDCASE
*!*****************************************************************************************
*! Name      : CDateToString
*! Developer : MAB - Mohamed Atia Badran
*! Date      : 11/26/2002 01:45:04 AM
*! Purpose   : Changes the char date to string date.
*! Entry no. :
*!*****************************************************************************************
*! Parameters:
*!****************************************************************************************
*! Returns   :
*!****************************************************************************************
*! Runs as following:
*!
*!****************************************************************************************
*!
FUNCTION CDATETOSTRING
LPARAMETERS CCHARDATE
IF (VARTYPE(CCHARDATE) != "C") OR EMPTY(CCHARDATE)
  RETURN ""
ENDIF
LOCAL LDDATE, LLERROR, LCOLDERROR
LCOLDERROR = ON("ERROR")
ON ERROR LLERROR = .T.
LDDATE = CTOD(ALLTRIM(CCHARDATE))
ON ERROR &LCOLDERROR.
IF LLERROR OR (VARTYPE(LDDATE) != "D")
  RETURN ""
ENDIF
RETURN DTOS(LDDATE)
ENDFUNC

*B612314,1 MMT 07/05/2021 Add missing function from Aria5 globals[End]