*:***************************************************************************
*: Program file  : ARREBAL
*: Program desc. : Custom Rebalance AR For GMA
*: For screen    : 
*:        System : Aria Advantage Series.
*:        Module : Accounts Receivable (AR)
*:     Developer : Mohamed Shokry (MHM)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : lfvCompany,lfvSelAll,lfWOldVal,lfvCheck,lfShowChk,lfRebalanc,
*:                : lfFilesStr,lfOpnFiles,gfModalGen,lfRelatFls,lfStartReb,lfUpdSty,lfUpdFab
*:                : lfCloseThm,gfThermo,gfTraceKey,lfUpStyDye,lpRepStyDy,lfUpdtLoop,lpGathFlds,
*:                : lfUpdtOper,lpUpDtWork,lpCalGrpDt,lpStyDyeUp
*:***************************************************************************
*: Passed Parameters  : 1- Short cut string for databases to rebalance
*:                    : 2- Company Code (Current if nothing) 
*:***************************************************************************
*: Example : DO ArRebal                (C#200481)
*:***************************************************************************
*Modify this program to run only for AR according to GMA Needs
*!***************************************************************************
PARAMETERS lcToReb,lcCompCode,llPurge,llOgForAut,llAutoBal
llConvert = .F.  && if it's .T. means that called from another program

* Verify Variables
laRebMsg   = ''     && Array holds the verify rebalance log message
lcWinTitl  = "Rebalance Log Report"
*E301516,1 (Begin) If called from Auto balance, don't create log file, rather add to it.
*llOpenRep  = .F.
*lcFilHandl = ''
IF !llAutoBal
  llOpenRep  = .F.
  lcFilHandl = ''
ELSE
  =FPUTS(lcFilHandl,'Rebalancing:')
  =FPUTS(lcFilHandl,'START PROCESSING     '+DTOC(DATE())+SPACE(5)+TIME())
ENDIF
llSaveAut = llOgForAut
*E301516,1 (End)


lnPrevVal  = 0
* Process Variables
laTempName = ""    && Array holds the temporar yfile name
lcInfoHdr  = ""    && Variable holds header temporary file name
lcInfoDtl  = ""    && Variable holds detail temporary file name


*** AP variables
lnInvAmt   = 0                  && Varible to hold the invoice amount
lnDiscTakn = 0                  && Varible to hold the total discount taken per invoice
lnDiscOffr = 0                  && Varible to hold the discount offeramount per invoice
lnAdjtAmt  = 0                  && Varible to hold the total adjustment amount per invoice
lnAmtPaid  = 0                  && Varible to hold the total paid amount per invoice
lnPurchAmt = 0                  && Varible to hold the purchase amount per invoice
lcInvNo    = ""                 && Varible to hold the A/P invoice no.

*** GL variables
STORE '' TO lcTmpBBtch, lcTmpJBtch, lcTmpCBtch, lcTmpTran, lcTmpGLTHd, lcTmpGLTDt     
llBalnBfor = .F.         && Flag to know if the Rebalance process was done befor in another session without Updating the master file
lnPostTran = 0           && Varible to hold the number of transactions that was Reposted in the Rebalance process
lcRpFiscYr = ""          && Variable holds the fiscal year if GL module was selected
lcFiscYear = ""          && Variable holds the fiscal year if GL module was selected
lcCurr_yer = ""
lnCurr_yer = 0
lcAcsMask  = ""
lnAcsSegSz = 0 

*E301337,1 ASH 10/18/1999 (Begin) Assign Variables to be used during rebalance.
lcKey    = ''            && Variable to hold the key that we seek with in the detail file while rebalance header&detail (ex. Invoice#)
laOldVal = ''            && Varible to hold the old value in the option grid.
lcType   = ''            && Variable to hold the order type.
lcFilePath = gcDataDir
*E301387,1 ASH 03/22/2000 (Begin) Add 2 elements to be used while rebalancing another fields other than the original qty&amt. [for example SO booked fields.]
*DIMENSION laFChck [2,2]  && Array to hold the fields that we will check while rebalancing header&detail.

*B604500,1 Add 2 elements to be used while rebalancing Cancel Qty and Amount in Order Header. [Begin]
*DIMENSION laFChck [2,4]  && Array to hold the fields that we will check while rebalancing header&detail.
DIMENSION laFChck [2,6]  && Array to hold the fields that we will check while rebalancing header&detail.
*B604500,1 Add 2 elements to be used while rebalancing Cancel Qty and Amount in Order Header. [End]

*E301387,1 ASH 03/22/2000 (End)
STORE SPACE(6) TO lcHigh,lcLow,lcInvN,lcOrdN,lcCTN,lcPON
*E301516,1 (Begin) If called from Auto balance then don't overwrite fixed filter array.
*DIMENSION laFXFLT [1,1]  && Array to hold the fix filter comming from the option grid.
IF !llAutoBal
  DIMENSION laFXFLT [1,1]  && Array to hold the fix filter comming from the option grid.
ENDIF
*E301516,1 (End)
DECLARE laRpCmpCod[1,3]  && Array to hold the companies information that will be rebalance.
*E301337,1 ASH 10/18/1999 (End)
lcRpCmpExp = ''
lcPathStat = SET('FULLPATH ')     && Varible to save the SET FULLPATH status
SET FULLPATH ON
llAdoDye = .F.
* Build tempoaray files and gathering all info. into this temp.

DO lfInfoHdr
STORE .F. TO llExpr1,llUpdt
*E301516,1 (Begin) If called from Auto balance copy this to another variable to empty it to avoid running lpMainReb
lcAutoBal = ''
IF llAutoBal
  lcAutoBal = lcToReb
  lcToReb = ''
ENDIF
*E301516,1 (End)
IF TYPE('lcToReb') = 'C' AND !EMPTY(lcToReb)
    DO lpMainReb WITH lcToReb,lcCompCode,'','U'
ELSE
  llNComp = .T.
  *E301516,1 (Begin) Don't call OG when called from Auto balance.
  *llExpr1 = gfOpGrid('ARREBAL' , .T.)  && Run selection grid.
  *--If we want only to specify databases to rebalance from Auto balance then only show OG.
  IF llOgForAut
    llClrReadN = .T.
    llExpr1 = gfOpGrid('ARREBAL' , .T.)  && Run selection grid.
    RETURN
  ENDIF
  IF !llAutoBal
    llExpr1 = gfOpGrid('ARREBAL' , .T.)  && Run selection grid.
  ELSE
    llExpr1 = lfUpBalance()
  ENDIF

  *E301516,1 (End)
  IF TYPE('llExpr1')  <> 'L'
    * Define varibales that will be used as alias name.
    * Select Header file
    SELECT lcTempHdr
    GO TOP
    IF USED('ORDHDR')	
      USE IN ORDHDR
    ENDIF
    IF USED('CUTTKTH')	
      USE IN CUTTKTH
    ENDIF
    IF USED('POSHDR')	
      USE IN POSHDR
    ENDIF
    IF USED('MATINVJL')	
      USE IN MATINVJL
    ENDIF
    IF USED('POFHDR')	
      USE IN POFHDR
    ENDIF
    IF USED('INVHDR')	
      USE IN INVHDR
    ENDIF

    SCAN FOR lcTempHdr.cUpdVryIgn $ 'UV' 
      * Get item name
      *E301337,1 ASH 10/18/1999 (Begin) Run the rebalance for multiple companies.

      FOR lnAllComp = 1 TO ALEN(laRpCmpCod,1)
        DIME laFileName [1,2]      
        laFileName = ''
        lcComp_ID  = PADR(laRpCmpCod[lnAllComp,1],2)
        IF lcComp_ID $ lcRpCmpExp
          lcFilePath = ALLTRIM(LOWER(laRpCmpCod[lnAllComp,2]))
          lcCompName = ALLTRIM(SUBSTR(laRpCmpCod[lnAllComp,1],6))
          lcCompMods = laRpCmpCod[lnAllComp,3]
          lcItemId   = UPPER(lcTempHdr.cItemName)
       *E301337,1 ASH 10/18/1999 (End)
          * If there is no error occured while opening file, continue 
          IF lfOpnFiles(lcItemId) 
            * Get current record no.
            lnCurNo = RECNO('lcTempHdr') 
            * Check mandatory data.
            IF !EMPTY(ALLTRIM(lcTempHdr.cValidFun))
              lcChkMntry = .T.
              lcDoProg = ALLTRIM(lcTempHdr.cValidFun)
              IF &lcDoProg() 
  		        * Call any additional setup program.
                IF !EMPTY(ALLTRIM(lcTempHdr.cAddSetup))
                  lcDoProg = ALLTRIM(lcTempHdr.cAddSetup)
	              DO &lcDoProg
                ENDIF      
                * Call Verify or Update program. 
	            lcUpVrProg = ALLTRIM(IIF(cUpdVryIgn='U', lcTempHdr.cUpdFunNam ,lcTempHdr.cVryFunNam))
  	            IF !EMPTY(lcDoProg)
	              DO &lcUpVrProg 
	            ENDIF
              ENDIF   && End of   IF &lcDoProg() 
            ENDIF     && !EMPTY(ALLTRIM(lcTempHdr.cValidFun)) 
            * Close all files that were open to complete process of current item.
            =lfClosFile(lcItemId)
        
            IF BETWEEN( lnCurNo,1,RECCOUNT('lcTempHdr') )
              SELECT lcTempHdr
              GO lnCurNo 
            ELSE
              EXIT  
            ENDIF
          ENDIF 
        ENDIF
      ENDFOR
    ENDSCAN
    
    
    *C102574,1 & C102575,1 HBG 28/05/2002 Trigger for GMA to rebalance the Invoice [Begin]
    =gfDoTriger('SMREBAL',PADR('INVREBAL',10))
    *C102574,1 & C102575,1 [End]     
    * Close info. temp. files.
    =lfErasTmp()  
  
    * Call rebalance Log screen.
    *E301516,1 (Begin) If called from Auto balance, don't view log report.
    *=lfViewRLog()
    IF !llAutoBal
     =lfViewRLog()
    ENDIF
    *E301516,1 (End)
  ENDIF
ENDIF

*E301337,1 ASH 10/18/1999 (Begin) Close the temp. files.
IF USED('lcTempHdr')
  SELECT lcTempHdr
  USE
ENDIF
IF USED('lcTempDtl')
  SELECT lcTempDtl
  USE
ENDIF
IF USED('INVHDR')	
  USE IN INVHDR
ENDIF
IF USED('ORDHDR')	
  USE IN ORDHDR
ENDIF
IF USED('CUTTKTH')	
  USE IN CUTTKTH
ENDIF
IF USED('POSHDR')	
  USE IN POSHDR
ENDIF
IF USED('FABDYE')	
  USE IN FABDYE
ENDIF
IF USED('STYDYE')	
  USE IN STYDYE
ENDIF
IF USED('APRBALV')	
  USE IN APRBALV
ENDIF
IF USED('APRBALVH')	
  USE IN APRBALVH
ENDIF

IF USED('GLPTRHD')
  USE IN GLPTRHD
ENDIF
IF USED('GLTRNSHD')
  USE IN GLTRNSHD
ENDIF
IF USED('GLTRNSDT')
  USE IN GLTRNSDT
ENDIF
IF USED('GLTMPBAL')
  USE IN GLTMPBAL
ENDIF
IF USED('GLPTRNHD')
  USE IN GLPTRNHD
ENDIF
IF USED('GLPTRNDT')
  USE IN GLPTRNDT
ENDIF


*E301337,1 ASH 10/18/1999 (End)
* Reset the FULLPATH setting.
SET FULLPATH &lcPathStat
* AMIN
 
*!*************************************************************
*! Name      : lfCloseThm
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Assure closing thermometer.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfUpdtLoop
*!*************************************************************
*! Passed Parameters      : ....
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfCloseThm()
*!*************************************************************
*!
FUNCTION lfCloseThm
PARAMETERS lcThemoTlt
*-- Asure that Thermometer was closed.
IF BETWEEN(lnThermNo,0,lnUpThermo)
  FOR lnCloseThm = lnThermNo TO lnUpThermo
    =gfThermo(lnUpThermo,lnCloseThm,lcThemoTlt," ")
  ENDFOR
ENDIF
*-- end of lfCloseThm.



*!*************************************************************
*! Name      : lfClosFile
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : Close all open files 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : 
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfClosFile()
*!*************************************************************
FUNCTION lfClosFile
PARAMETER lcItemId
* Convert it to upper case
lcItemId = UPPER(lcItemId)

* First close all master files belong to item (lcItemId). 
SELECT lcTempDtl
=SEEK(lcItemId)
SCAN REST WHILE cItemName = lcItemId  
  lcFileName = ALLTRIM(lcTempDtl.cFileName)
  *B803053,1 ASH 02/16/2000 (Begin) Don't close the file unless it is not opened.
  IF USED(lcFileName)
  *B803053,1 ASH 02/16/2000 (End)
    =gfCloseFile(lcFileName)
  ENDIF
ENDSCAN  

** Second close & delete all temporary files belong to item (lcItemId).
*PRIVATE lcSetPath
*lcSetPath = SET('FULLPATH')
*SET FULLPATH OFF
*FOR lnI = 1  TO 225
*  lcDbfName = DBF(lnI)   
*  IF !EMPTY(lcDbfName) 
*    lcDbfName = SUBSTR(lcDbfName , 3 ) 
*    * If third character is 'X' that means temporary file.
*    IF UPPER(LEFT(lcDbfName,3)) = 'X'  AND ;
*       lcDbfName <> lcInfoHdr   AND lcDbfName <> lcInfoDtl  
*     IF FILE(gcWorkDir+lcDbfName)
*        SELECT (lnI)
*        USE 
*        ERASE (gcWorkDir+lcDbfName)
*        lcCdxName = SUBSTR(lcDbfName  , 1 , LEN(lcDbfName) - 3) + 'CDX'
*        IF FILE(gcWorkDir+lcCdxName)
*          ERASE (gcWorkDir+lcCdxName)
*        ENDIF
*      ENDIF 
*    ENDIF
*  ENDIF 
*ENDFOR
*SET FULLPATH &lcSetPath 

** Second close & delete all temporary files belong to item (lcItemId).
IF !EMPTY(laTempName)
  lnTempName = ALEN(laTempName,1)
  FOR lnCount = 1  TO lnTempName 
    IF !EMPTY(laTempName[lnCount,1]) AND ;
        laTempName[lnCount,1] <> lcInfoHdr AND ;
        laTempName[lnCount,1] <> lcInfoDtl  
        
      IF FILE(gcWorkDir+laTempName[lnCount,1]+'.DBF')
        IF USED(laTempName[lnCount,1])
          USE IN (laTempName[lnCount,1])
        ENDIF
        ERASE (gcWorkDir+laTempName[lnCount,1]+'.DBF')
      ENDIF
      
      IF FILE(gcWorkDir+laTempName[lnCount,1]+'.CDX')
        IF USED(laTempName[lnCount,1])
          USE IN (laTempName[lnCount,1])
        ENDIF
        ERASE (gcWorkDir+laTempName[lnCount,1]+'.CDX')
      ENDIF
    ENDIF
  ENDFOR
ENDIF  

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : When function of Option Grid
*!*************************************************************
*! Called from : Option Grid read cycle
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*E301294,1  AKA 08/23/99  
***************************************************************
FUNCTION lfwRepWhen
*-- Define companies array that be used in company mover

IF EMPTY(laRpCmpCod)

  DECLARE laRpCmpCod[1,3]
  STORE '' TO lcRpCmpExp
  *-- Collect all companies

  *--mhm C#200481 Run only for the current company [Start]
  *SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,mModlSet ;
  *  FROM SYCCOMP                            ;
  *  INTO ARRAY laRpCmpCod                   ;
  *  ORDER BY 1

  SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,mModlSet ;
    FROM SYCCOMP                            ;
    INTO ARRAY laRpCmpCod                   ;
    WHERE ALLTRIM(ccomp_id) = gcAct_Comp    ;
    ORDER BY 1
  *--mhm C#200481 Run only for the current company [End]

  DECLARE laRpSorCmp[ALEN(laRpCmpCod,1),1],laRpTarCmp[ALEN(laRpCmpCod,1),1]
  FOR lnI = 1 TO ALEN(laRpCmpCod,1)
    STORE laRpCmpCod[lnI,1] TO laRpSorCmp[lnI,1],laRpTarCmp[lnI,1]
  ENDFOR
  =lfCmpExpr()
*B604531,4 AME [Start] 
ELSE
  IF llNComp .AND. ALEN(laRpTarCmp,1) = 1
    llNComp = .F.
    CLEAR READ
  ENDIF
*B604531,4 AME [End]
ENDIF
SHOW GET lnOGSeting DISABLE

*B606139,1 ABD - Disable the svae Filter option   [Begin]
SHOW GET pbOGSave DISABLE
*B606139,1 ABD - [End]

=lfvDfinMod()
*-- end of lfwRepWhen.


*!*************************************************************
*! Name      : lfvCompany
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : - Call Companies mover function 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover,lfCmpExpr
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvCompany()
*!*************************************************************
*E301294,1  AKA 08/23/99  
*!*************************************************************

FUNCTION lfvCompany
= gfMover(@laRpSorCmp,@laRpTarCmp,'Select Company',.T.,'')  && call mover function.
= lfCmpExpr()
*-- end of lfvCompany.

*!***************************************************************************
*! Name      : lfCmpExpr
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : - Evaluate Company expression.
*!***************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!***************************************************************************
*! Called from : lfvCategory(),lfvCompany(),lfwRepWhen()
*!***************************************************************************
*! Passed Parameters  : ....
*!***************************************************************************
*! Returns            : ....
*!***************************************************************************
*! Example   : = lfCmpExpr()
*!***************************************************************************
*E301294,1  AKA 08/23/99  
*!***************************************************************************

FUNCTION lfCmpExpr
PRIVATE laTarget

IF EMPTY(laRpTarCmp)
  = ACOPY(laRpSorCmp,laTarget)
ELSE
  = ACOPY(laRpTarCmp,laTarget)
ENDIF
= ASORT(laTarget)
lcRpCmpExp = ''
FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpCmpExp = IIF(EMPTY(lcRpCmpExp),PADR(laTarget[lnI],2),;
                    lcRpCmpExp + ','+PADR(laTarget[lnI],2))
ENDFOR

IF LEN(lcRpCmpExp) > 2
  llNComp = .T.
ELSE
  llNComp = .F.
ENDIF
CLEAR READ
*-- end of lfCmpExpr.


*!***************************************************************************
*! Name      : lfModExpr
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : - Evaluate Category expression.
*!***************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!***************************************************************************
*! Called from : lfvCategory(),lfwRepWhen()
*!***************************************************************************
*! Passed Parameters  : ....
*!***************************************************************************
*! Returns            : ....
*!***************************************************************************
*! Example   : = lfModExpr()
*!***************************************************************************
*E301294,1  AKA 08/23/99  
*!***************************************************************************

FUNCTION lfModExpr
PRIVATE laTarget

IF EMPTY(laRpTarMod)
  =ACOPY(laRpSorMod,laTarget)
ELSE
  =ACOPY(laRpTarMod,laTarget)
ENDIF
=ASORT(laTarget)
lcRpModExp = ''
FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpModExp = IIF(EMPTY(lcRpModExp),PADR(laTarget[lnI],2),;
                    lcRpModExp + ','+PADR(laTarget[lnI],2))
ENDFOR
*-- end of lfModExpr.


*!*************************************************************
*! Name      : lfvModule
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : - Call Categories mover function 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover,lfModExpr
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvModule()
*!*************************************************************
*E301294,1  AKA 08/23/99  
*!*************************************************************
FUNCTION lfvModule
= gfMover(@laRpSorMod,@laRpTarMod,'Select Module',.T.,'')  && call mover function.
CLEAR READ
=lfModExpr()
*-- end of lfvModule.

*!*************************************************************
*! Name      : lfvDclTarg
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : Declare target array to avoid error message
*!             'lcRpTarCat' not an array' 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvDclTarg()
*!*************************************************************
*E301294,1  AKA 08/23/99  
*!*************************************************************
FUNCTION lfvDclTarg
DECLARE laRpTarMod[1] 
*-- end of lfvDclTarg

*!*************************************************************
*! Name      : lfvGetModl
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : Check and assign the active module to module array
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfvGetModl()
*!*************************************************************
*E301294,1  AKA 08/23/99  
*!*************************************************************
FUNCTION lfvGetModl

* Save prevoius area , order and filter 
PRIVATE lcAreaNo ,  lnAreaOrd , lcAreaFlt
lcAreaNo   = SELECT(0)
lnAreaOrd  = ORDER()
lcAreaFlt  = FILTER()

DECLARE laRpCodMod[1,2]

PRIVATE  lcModId  , lnModlNo 

SELECT SYCCOMP
GO TOP
SCAN WHILE !EOF()
  
  * Get & assign all modules to string varibale
  lcComp_mdl = ALLTRIM(SYCCOMP.mComp_mdl)
  
  * If no module has been defined in this record skip to next record
  IF EMPTY(ALLTRIM(lcComp_mdl))
    SKIP
    LOOP
  ENDIF
  
  * Variable used as a counter to redclare the module array.
  lnModlNo =  0 
  
  DO WHILE !EMPTY(lcComp_mdl)

    * Get position of '|' character 
    lnPosition =   ATC (  '|' , lcComp_mdl  )

    * Accumulate no. of modules that found in SYCCOMP  
    lnModlNo =  lnModlNo  + 1

    * Declare module code array
   	*DECLARE laRpCodMod[lnModlNo,2]
	IF lnModlNo  = 1 
	  * Split module id from whole module string
      lcModId    =  SUBSTR(lcComp_mdl , 1 ,  2  )  
      * Remove module ID from modules string       
      lcComp_mdl = SUBSTR(lcComp_mdl , 3    )          
   	ELSE
	  * Split module id from whole module string   	
      lcModId    =  SUBSTR(lcComp_mdl , 2 ,  2  )  

      * Remove module ID from modules string 
      lcComp_mdl = SUBSTR(lcComp_mdl , 4    )  
   	ENDIF
   	
   	* Check duplicated module id , If this module id was inserted before in 
   	* module array, skip it. 
    IF ASCAN(laRpCodMod , lcModId  ) = 0  AND !(lcModId $ 'SUEBPSRGSP')
      * Get module name , then svae module id & module name.  
	  IF SEEK( lcModId, 'SYDAPPL')
        * Declare module code array

        *--mhm C#200481 Run only for the AR Module [Start]
        *IF EMPTY(laRpCodMod) 
        *   DECLARE laRpCodMod[1,2]           
        *ELSE
   	    *  DECLARE laRpCodMod[ALEN(laRpCodMod,1)+1,2]           
        *ENDIF
        DECLARE laRpCodMod[1,2]           
        *--mhm C#200481 Run only for the AR Module [End]

        *--mhm C#200481 Run only for the AR Module [Start]
        *laRpCodMod[ALEN(laRpCodMod,1),1]  = lcModId  
        *laRpCodMod[ALEN(laRpCodMod,1),2]  = ALLTRIM(SYDAPPL.cApp_Name)
        IF lcModId   = "AR"
          laRpCodMod[ALEN(laRpCodMod,1),1]  = lcModId  
	      laRpCodMod[ALEN(laRpCodMod,1),2]  = ALLTRIM(SYDAPPL.cApp_Name)
	    ENDIF  
        *--mhm C#200481 Run only for the AR Module [End]
	  ENDIF
	ENDIF 
  ENDDO   
ENDSCAN
IF !EMPTY(laRpCodMod)
  =ASORT(laRpCodMod,2)
ENDIF  


*FOR I = 1 TO  ALEN(laRpCodMod,1)
*   WAIT WIND laRpCodMod[I,1]
*ENDFOR



* Return to prevoius area , order and filter 
SELECT (lcAreaNo)  
SET ORDER TO &lnAreaOrd
SET FILTER TO  &lcAreaFlt

*-- end of lfvGetModl


*!*************************************************************
*! Name      : lfvChkModul
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : Appear or disappear each item in OG
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : Option Grid  
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfvChkModul()
*!*************************************************************
*E301294,1  AKA 08/23/99  
*!*************************************************************
FUNCTION lfvChkModul
PARAMETER lcChkModul 

*, lcChkFlNam 
* If module id is empty disappear it from option grid.
IF EMPTY(lcChkModul) 
  RETURN .T.
ENDIF   

*IF TYPE("lcChkFlNam") =  'U'
*  lcChkFlNam = ""
*ENDIF

* If module array not defined , build it first 
=lfvDfinMod()

lnRec = RECNO('lcTempHdr')
IF SEEK(UPPER(MLINE(MFLD_NAME,1)),'lcTempHdr')
  REPLACE lcTempHdr.cUpdVryIgn WITH 'I'
  *E301516,1 (Begin) If called from Auto balance, Updat variables also.
  IF llSaveAut
    lcItemV  = UPPER(MLINE(MFLD_NAME,1))
    &lcItemV = 'I'
  ENDIF  
  *E301516,1 (End)
ENDIF
GOTO lnRec IN lcTempHdr


* Variable to hold module array length
PRIVATE lnModLen 
lnModLen = ALEN(laRpCodMod,1)

* Variable to hold Module name 
PRIVATE lcModlName 
lcModlName = ''

* Get module name from module array to compare it with module target array
FOR lnI = 1  TO  lnModLen 
  IF lcChkModul = laRpCodMod[lnI , 1 ]
    lcModlName  = laRpCodMod[lnI , 2 ]
    EXIT
  ENDIF
ENDFOR

* If module name is empty disable option grid 
IF EMPTY (lcModlName)
  RETURN .T.
ENDIF

* If passed module id (lcChkModul) is not defined in module array, disable the 
*  variable from option grid.
IF ASCAN(laRpTarMod , lcModlName ) = 0 
  *E301337,1 ASH 10/18/1999 (Begin) Replace cupdvrign field with ignore if this line supressed.
  lnRec = RECNO('lcTempHdr')
  IF SEEK(UPPER(MLINE(MFLD_NAME,1)),'lcTempHdr')
    REPLACE lcTempHdr.cUpdVryIgn WITH 'I'
    *E301516,1 (Begin) If called from Auto balance, Updat variables also.
    IF llSaveAut
      lcItemV  = UPPER(MLINE(MFLD_NAME,1))
      &lcItemV = 'I'
    ENDIF  
    *E301516,1 (End)
  ENDIF
  GOTO lnRec IN lcTempHdr
  *E301337,1 ASH 10/18/1999 (End)
  RETURN .T.
ELSE
  lnRec = RECNO('lcTempHdr')
  IF SEEK(UPPER(MLINE(MFLD_NAME,1)),'lcTempHdr')
    REPLACE lcTempHdr.cUpdVryIgn WITH 'V'
    *E301516,1 (Begin) If called from Auto balance, Updat variables also.
    IF llSaveAut
      lcItemV  = UPPER(MLINE(MFLD_NAME,1))
      &lcItemV = 'V'
    ENDIF  
    *E301516,1 (End)
  ENDIF
  GOTO lnRec IN lcTempHdr
ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfvDfinMod
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : Build source & target module array from main module array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfvDfinMod()
*!*************************************************************
*E301294,1  AKA 08/23/99  
*!*************************************************************
FUNCTION lfvDfinMod
*-- Define modules array that be used in modules mover
IF EMPTY(laRpCodMod)
  * Declare and define modules.
  =lfvGetModl()

  * Declare soruce & Target of module array
  DECLARE laRpSorMod[1] , laRpTarMod[1]
  
  * Copy module array to source module array.
  lnModLen = ALEN(laRpCodMod,1)
  FOR lnI = 1  TO  lnModLen 
    DECLARE laRpSorMod[lnI] , laRpTarMod[lnI]  
    laRpSorMod[lnI]  =  laRpCodMod[lnI , 2] 
    laRpTarMod[lnI]  =  laRpCodMod[lnI , 2] 
  ENDFOR
  =lfModExpr()
ENDIF


*!*************************************************************
*! Name      : lfvUVIAll
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : - Change the stauts (Update / Verify /Ignore) of all 
*!               rows of option grid. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvUVIAll()
*!*************************************************************
*E301294,1  AKA 08/23/99  
*!*************************************************************
FUNCTION lfvUVIAll
*Get Length of Visiable rows in Option Grid
PRIVATE lnOGTypLen 
lnOGTypLen = ALEN(laOGObjTyp,1)

* Note:
* I know the next FOR ... ENDFOR loop it's not good to start from hard code 
* number 5. but I will try to find out a way to get first item by generic function.
*Get the first position item that can be used as a begining of first item that 
* going to be rebalanced.

FOR lnI = 1  TO lnOGTypLen
  IF 'LCRPITEM' $ UPPER(laOGObjTyp[lnI ,1])
    &laOGObjTyp[lnI ,1] =  lcUpdVry
    =lfOGShowGet("&laOGObjTyp[lnI ,1]")
  ENDIF
ENDFOR


* Now change the laInfoTemp.cUpdVryIgn  with the value of (Update all / vrify all / Ignore all)
SELECT lcTempHdr
GO TOP

SCAN WHILE !EOF() FOR  'LCRPITEM' $ UPPER(lcTempHdr.cItemName)
  REPLACE lcTempHdr.cUpdVryIgn WITH lcUpdVry
ENDSCAN
 *-- end of lfvUVIAllf.
 

 

*!*************************************************************
*! Name      : lfvUpVrIg
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : - Replace the stauts flag (Update / Verify /Ignore) in temp.
*!               file with the value of Option Grid.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvUpVrIg()
*!*************************************************************
*E301294,1  AKA 08/23/99  
*!*************************************************************
FUNCTION lfvUpVrIg
PARAMETER  lcCurRow
* Now change the laInfoTemp.cUpdVryIgn  with the value of (Update/verify/Ignore)
IF SEEK( UPPER(lcCurRow) , "lcTempHdr")
  lcGetValue = &lcCurRow
  REPLACE lcTempHdr.cUpdVryIgn WITH lcGetValue 
ENDIF
RETURN 
*--- End of lfvUpVrIg()




*!*************************************************************
*! Name      : lfInfoHdr
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : - Create one temporary file
*!             - Fill it with header info. that will need it while process
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : ARREBAL.PRG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfInfoHdr()
*!*************************************************************
*E301294,1  AKA 08/23/99  
*!*************************************************************
FUNCTION lfInfoHdr

* lcUpVrIg is a varibale represents the default value of Update all / Verify all/ Ignore all.
PRIVATE  lcUpVrIg 
lcUpVrIg = 'I'  

*E301337,1 ASH 10/18/1999 (Begin) Close the temp. files.
IF USED('lcTempHdr')
  SELECT lcTempHdr
  USE
ENDIF
IF USED('lcTempDtl')
  SELECT lcTempDtl
  USE
ENDIF
*E301337,1 ASH 10/18/1999 (END)
* Get temporary file header and index name.
lcInfoHdr = gfTempName()  
*Build the information file structure.
*E301337,1 ASH 10/18/1999 (Begin) Add 3 new fields to the temp file :
*E301337,1                1- cTempName  : To hold the temp file name used to verify in.
*E301337,1                2- cFileStr   : To hold the temp file structure.
*E301337,1                3- cMFiles    : To hold the header and detail files.
*E301387,1 ASH 03/22/2000 (Begin) Change cfilestr field width to be 180 to add the SO book fields.
CREATE TABLE (gcWorkDir + lcInfoHdr ) ;
	(cItemName  C(10)  ,;
	 cUpdFunNam C(10)  ,;
	 cVryFunNam C(10)  ,;
     cIgnFunNam C(10)  ,;	  
	 cItemDesc  C(30)  ,;
	 cAddSetup  C(10)  ,;
	 cValidFun  C(10)  ,;
	 cUpdVryIgn C(1)   ,;
	 cLogFile   C(1)   ,;
	 cTempName  C(8)   ,;
	 cFileStr   C(180) ,;
	 cMFiles    C(80))
*E301337,1 ASH 10/18/1999 (End)
*E301387,1 ASH 03/22/2000 (End)
* Create index on temporary file 	 
INDEX ON  cItemName TAG ItemHdr  OF (gcWorkDir + lcInfoHdr + '.CDX') 
USE     && Colse the temp. file and open it with alias name 
USE (gcWorkDir + lcInfoHdr ) ALIAS lcTempHdr IN 0 
SET ORDER TO TAG ItemHdr


* Get temporary file detail and index name.
lcInfoDtl  = gfTempName()  
*Build the information file structure.
CREATE TABLE (gcWorkDir + lcInfoDtl ) ;
	(cItemName  C(10)  ,;
	 cFileName  C(10)  ,;
	 cNdxName   C(10)  ,;
     cAliasName C(10)  ,;	  
	 lOpenBefor L(1))   

* Fields description
*Field name         Description
*----------     -----------------------------------------------------------
*cItemName     Represents SyRepUvr.mfld_name field
*cFileName     File name 
*cNdxName      index name
*cAliasName    Alias name
*lOpenBefor    '.T.' file was open before , '.F.' file was not open before
*----------     -----------------------------------------------------------
* Create index on temporary file 	 
INDEX ON  cItemName TAG ItemDtl OF (gcWorkDir + lcInfoDtl + '.CDX') 
USE     && Colse the temp. file and open it with alias name 
USE (gcWorkDir + lcInfoDtl  ) ALIAS lcTempDtl IN 0 
SET ORDER TO TAG ItemDtl



* Insert generate rebalance log information (LCLOGFILE). 
* ------------------------------------------------------
SELECT lcTempHdr
APPEND BLANK

REPLACE cItemName  WITH 'LCLOGFILE' ,;
		cUpdFunNam WITH 'UpdName'   ,;
		cVryFunNam WITH 'VryName'   ,; 
		cIgnFunNam WITH 'IgnName'   ,; 		
		cLogFile   WITH  'Y'

* Header info. about GL relabance information (lcRpItem1).
* --------------------------------------------------------
SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM1'  ,;
		cUpdFunNam WITH 'lfUpdItem1' ,;
		cVryFunNam WITH 'lfVryItem1' ,; 
		cIgnFunNam WITH 'IgnName'    ,; 		
		cUpdVryIgn WITH lcUpVrIg     ,;
		cAddSetup  WITH 'lfStupItm1' ,;
		cValidFun  WITH 'lfValdItm1' 


* Detail info. about GL rebalance master files 'lcRpItem1'.
* --------------------------------------------------------
SELECT lcTempDtl
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM1' ,;
	    cFileName  WITH 'GLBATCH'   ,;
   	    cNdxName   WITH 'BATSTAT'   ,;
        cAliasName WITH 'GLBATCH'   ,;	  
	    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM1' ,;
	    cFileName  WITH 'GLPTRNHD'  ,;
   	    cNdxName   WITH ''          ,;
        cAliasName WITH 'GLTRNSHD'  ,;	  
	    lOpenBefor WITH .F.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM1' ,;
	    cFileName  WITH 'GLPTRNDT'  ,;
   	    cNdxName   WITH 'BATCHTRN'  ,;
        cAliasName WITH 'GLTRNSDT'  ,;	  
	    lOpenBefor WITH .F.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM1' ,;
	    cFileName  WITH 'GLACBALS'  ,;
   	    cNdxName   WITH 'ACCYRPRD'  ,;
        cAliasName WITH 'GLTMPBAL'  ,;	  
	    lOpenBefor WITH .F.


APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM1' ,;
	    cFileName  WITH 'GLACCHAR' ,;
   	    cNdxName   WITH 'ACCTCODE'  ,;
        cAliasName WITH 'GLACCHAR'  ,;	  
	    lOpenBefor WITH .F.

*B605300,1 Fix bug Alias not found [Start]
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM1' ,;
	    cFileName  WITH 'FSPRD'     ,;
   	    cNdxName   WITH 'COMFYRPRDI',;
        cAliasName WITH 'FSPRD'     ,;	  
	    lOpenBefor WITH .F.  
*B605300,1 Fix bug Alias not found [End]	    

	    
*-- End of Setup Header and detail files of GL (lcRpItem1) --*


* Header info. about AP relabance information (lcRpItem2).
* --------------------------------------------------------
SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM2'  ,;
		cUpdFunNam WITH 'lfUpdItem2' ,;
		cVryFunNam WITH 'lfVryItem2' ,; 
		cIgnFunNam WITH 'IgnName'    ,; 		
		cUpdVryIgn WITH lcUpVrIg     ,;
		cAddSetup  WITH 'lfStupItm2' ,;
		cValidFun  WITH 'lfValdItm2' 

* Detail info. about GL rebalance master files (lcRpItem2).
* --------------------------------------------------------
SELECT lcTempDtl
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM2' ,;
	    cFileName  WITH 'APVENDOR'  ,;
   	    cNdxName   WITH 'VENCODE'   ,;
        cAliasName WITH 'APVENDOR'  ,;	  
	    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM2' ,;
	    cFileName  WITH 'FISHD'     ,;
   	    cNdxName   WITH 'COMPFYEAR' ,;
        cAliasName WITH 'FISHD'     ,;	  
	    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM2' ,;
	    cFileName  WITH 'APDIST'    ,;
   	    cNdxName   WITH 'INVVEND'   ,;
        cAliasName WITH 'APDIST'    ,;	  
	    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM2' ,;
	    cFileName  WITH 'APINVHDR'  ,;
   	    cNdxName   WITH 'VENDINV'   ,;
        cAliasName WITH 'APINVHDR'  ,;	  
	    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM2' ,;
	    cFileName  WITH 'APVENHST'  ,;
   	    cNdxName   WITH 'VENDYEAR' ,;
        cAliasName WITH 'APVENHST'  ,;	  
	    lOpenBefor WITH .F.  

	    
*-- End of Setup Header and detail files of AP (lcRpItem2) --*

		
*E301337,1 ASH 10/18/1999 (Begin) Add the following items to be rebalanced from 3 to 16.
* Insert Material on-hand information (lcRpItem3).
* ------------------------------------------------
SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM3' ,;
		cUpdFunNam WITH 'lfUpdt'    ,;
		cVryFunNam WITH 'lfVerf'    ,; 
		cIgnFunNam WITH 'lfIgn'     ,; 		
		cValidFun  WITH 'lfVldMat'  ,;		
		cUpdVryIgn WITH  lcUpVrIg

* Insert Material on-order information (lcRpItem4).
* ------------------------------------------------
SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM4' ,;
		cUpdFunNam WITH 'lfUpdt'    ,;
		cVryFunNam WITH 'lfVerf'    ,; 
		cIgnFunNam WITH 'lfIgn'     ,; 		
		cValidFun  WITH 'lfVldMat'  ,;
		cUpdVryIgn WITH  lcUpVrIg
		
* Insert Style on-hand information (lcRpItem5).
* ----------------------------------------------
SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM5' ,;
		cUpdFunNam WITH 'lfUpdt'    ,;
		cVryFunNam WITH 'lfVerf'    ,; 
		cIgnFunNam WITH 'lfIgn'     ,; 		
		cValidFun  WITH 'lfVldMat'  ,;
		cUpdVryIgn WITH  lcUpVrIg
		
* Insert Style in transit information (lcRpItem6).
* ------------------------------------------------
SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM6' ,;
		cUpdFunNam WITH 'lfUpdt'    ,;
		cVryFunNam WITH 'lfVerf'    ,; 
		cIgnFunNam WITH 'lfIgn'     ,; 		
		cValidFun  WITH 'lfVldMat'  ,;
		cUpdVryIgn WITH  lcUpVrIg


* Insert Style work orders information (lcRpItem7).
* -------------------------------------------------
SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM7' ,;
		cUpdFunNam WITH 'lfUpdt'    ,;
		cVryFunNam WITH 'lfVerf'    ,; 
		cIgnFunNam WITH 'lfIgn'     ,; 		
		cValidFun  WITH 'lfVldMat'  ,;
		cUpdVryIgn WITH  lcUpVrIg

* Insert Style work in process information (lcRpItem8).
* -----------------------------------------------------
SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM8' ,;
		cUpdFunNam WITH 'lfUpdt'    ,;
		cVryFunNam WITH 'lfVerf'    ,; 
		cIgnFunNam WITH 'lfIgn'     ,; 		
		cValidFun  WITH 'lfVldMat'  ,;
		cUpdVryIgn WITH  lcUpVrIg

* Insert Style shipped quantity information (lcRpItem9).
* ------------------------------------------------------
*--mhm C#200481 Update Flag to Verfy only for the AR Module [Start]
lcUpVrIg = 'V'  
*--mhm C#200481  [ENd]

SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM9' ,;
		cUpdFunNam WITH 'lfUpdt'    ,;
		cVryFunNam WITH 'lfVerf'    ,; 
		cIgnFunNam WITH 'lfIgn'     ,; 		
		cValidFun  WITH 'lfVldMat'  ,;
		cUpdVryIgn WITH  lcUpVrIg

*--mhm C#200481 Update Flag to Verfy only for the AR Module [Start]
lcUpVrIg = 'I'  
*--mhm C#200481  [End]

* Insert Style allocated quantity information (lcRpItem10).
* ------------------------------------------------------
SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM10' ,;
		cUpdFunNam WITH 'lfUpdt'    ,;
		cVryFunNam WITH 'lfVerf'    ,; 
		cIgnFunNam WITH 'lfIgn'     ,; 		
		cValidFun  WITH 'lfVldMat'  ,;
		cUpdVryIgn WITH  lcUpVrIg

* Insert Style ordered quantity information (lcRpItem11).
* ------------------------------------------------------
SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM11' ,;
		cUpdFunNam WITH 'lfUpdt'    ,;
		cVryFunNam WITH 'lfVerf'    ,; 
		cIgnFunNam WITH 'lfIgn'     ,; 		
		cValidFun  WITH 'lfVldMat'  ,;
		cUpdVryIgn WITH  lcUpVrIg

* Insert Return Authorization (lcRpItem12).
* -----------------------------------------
SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM12' ,;
		cUpdFunNam WITH 'lfUpdt'    ,;
		cVryFunNam WITH 'lfVerf'    ,; 
		cIgnFunNam WITH 'lfIgn'     ,; 		
		cValidFun  WITH 'lfVldMat'  ,;
		cUpdVryIgn WITH  lcUpVrIg

* Insert Return Credit Memo (lcRpItem13).
* ---------------------------------------
SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM13' ,;
		cUpdFunNam WITH 'lfUpdt'    ,;
		cVryFunNam WITH 'lfVerf'    ,; 
		cIgnFunNam WITH 'lfIgn'     ,; 		
		cValidFun  WITH 'lfVldMat'  ,;
		cUpdVryIgn WITH  lcUpVrIg

* Header info. of AR invoice header (lcRpItem14).
* --------------------------------------------------------
lcFName  = '(Invoice C(6)|Ship N(7)|ShipAmt N(14,2)|cAdd_time C(11)|dAdd_Date D)'
lcMFiles = 'InvHdr,InvLine'
*--mhm C#200481 Update Flag to Verfy only for the AR Module [Start]
lcUpVrIg = 'V'  
*--mhm C#200481  [End]

SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM14'  ,;
		cUpdFunNam WITH 'lfUpdHdDtl' ,;
		cVryFunNam WITH 'lfVryHdDtl' ,; 
		cIgnFunNam WITH 'IgnName'    ,; 		
		cUpdVryIgn WITH lcUpVrIg     ,;
		cAddSetup  WITH 'lfStpHdDtl' ,;
		cValidFun  WITH 'lfVldHdDtl' ,;
		cTempName  WITH 'lcArInv'    ,;
		cFileStr   WITH lcFName      ,;
		cMFiles    WITH lcMFiles 

*--mhm C#200481 Update Flag to Verfy only for the AR Module [Start]
lcUpVrIg = 'I'  
*--mhm C#200481 [End]

* Detail info. of AR invoice header (lcRpItem14).
* --------------------------------------------------------
SELECT lcTempDtl
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM14' ,;
	    cFileName  WITH 'INVHDR'    ,;
   	    cNdxName   WITH 'INVHDR'   ,;
        cAliasName WITH 'INVHDR'  ,;	  
	    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM14' ,;
	    cFileName  WITH 'INVLINE'    ,;
   	    cNdxName   WITH 'INVLINE'   ,;
        cAliasName WITH 'INVLINE'  ,;	  
	    lOpenBefor WITH .F.  


* Header info. of SO sales order header (lcRpItem15).
* --------------------------------------------------------
*E301387,1 ASH 03/22/2000 (Begin) Add the book fields to be rebalanced.
*lcFName  = '(cOrdType C(1)|Order C(6)|Open N(8)|OpenAmt N(13,2)|cAdd_time C(11)|dAdd_Date D)'
*B803172,1 ASH 04/12/2000 (Begin) Add ship and shipamt to be rebalanced.
*lcFName  = '(cOrdType C(1)|Order C(6)|Open N(8)|OpenAmt N(13,2)|Book N(8)|BookAmt N(11,2)|cAdd_time C(11)|dAdd_Date D)'

*B604500,1 Add the Cancel Qty and Amount to Temp file. [Begin]
*lcFName  = '(cOrdType C(1)|Order C(6)|Open N(8)|OpenAmt N(13,2)|Book N(8)|BookAmt N(11,2)|Ship N(8)|ShipAmt N(14,2)|cAdd_time C(11)|dAdd_Date D)'
lcFName  = '(cOrdType C(1)|Order C(6)|Open N(8)|OpenAmt N(13,2)|Cancel N(8)|CancelAmt N(13,2)|Book N(8)|BookAmt N(11,2)|Ship N(8)|ShipAmt N(14,2)|cAdd_time C(11)|dAdd_Date D)'
*B604500,1 Add the Cancel Qty and Amount to Temp file. [End]

*B803172,1 ASH 04/12/2000 (End)
*E301387,1 ASH 03/22/2000 (End)
lcMFiles = 'OrdHdr,OrdLine'

SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
		cUpdFunNam WITH 'lfUpdHdDtl' ,;
		cVryFunNam WITH 'lfVryHdDtl' ,; 
		cIgnFunNam WITH 'IgnName'    ,; 		
		cUpdVryIgn WITH lcUpVrIg     ,;
		cAddSetup  WITH 'lfStpHdDtl' ,;
		cValidFun  WITH 'lfVldHdDtl' ,;
		cTempName  WITH 'lcSoOrd'    ,;
		cFileStr   WITH lcFName      ,;
		cMFiles    WITH lcMFiles 


* Detail info. of SO sales order header (lcRpItem15).
* --------------------------------------------------------
SELECT lcTempDtl
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15',;
	    cFileName  WITH 'ORDHDR'    ,;
   	    cNdxName   WITH 'ORDHDR'    ,;
        cAliasName WITH 'ORDHDR'    ,;	  
	    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
	    cFileName  WITH 'ORDLINE'    ,;
   	    cNdxName   WITH 'ORDLINE'    ,;
        cAliasName WITH 'ORDLINE'    ,;	  
	    lOpenBefor WITH .F.  

*B803172,1 ASH 04/12/2000 (Begin) Add invline&invhdr files to be opened while rebalancing sales order.
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
	    cFileName  WITH 'INVHDR'     ,;
   	    cNdxName   WITH 'INVHDR'     ,;
        cAliasName WITH 'INVHDR'     ,;	  
	    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
	    cFileName  WITH 'INVLINE'    ,;
   	    cNdxName   WITH 'INVLINEO'   ,;
        cAliasName WITH 'INVLINE'    ,;	  
	    lOpenBefor WITH .F.  
*B803172,1 ASH 04/12/2000 (End)

*B604500,1 Add Order Cancellation file to be opened while rebalancing sales order. [Begin]
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM15' ,;
	    cFileName  WITH 'ORDCANLN'   ,;
   	    cNdxName   WITH 'ORDCANLN'   ,;
        cAliasName WITH 'ORDCANLN'   ,;	  
	    lOpenBefor WITH .F.  
*B604500,1 Add Order Cancellation file to be opened while rebalancing sales order. [End]

* Header info. of CT header (lcRpItem16).
* ---------------------------------------
lcFName  = '(CutTkt C(6)|Pcs_Bud N(7)|Pcs_Rec N(7)|cAdd_time C(11)|dAdd_Date D)'
lcMFiles = 'CutTktH,CutTktL'

SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM16' ,;
		cUpdFunNam WITH 'lfUpdHdDtl' ,;
		cVryFunNam WITH 'lfVryHdDtl' ,; 
		cIgnFunNam WITH 'IgnName'    ,; 		
		cUpdVryIgn WITH lcUpVrIg     ,;
		cAddSetup  WITH 'lfStpHdDtl' ,;
		cValidFun  WITH 'lfVldHdDtl' ,;
		cTempName  WITH 'lcMfCt'     ,;
		cFileStr   WITH lcFName      ,;
		cMFiles    WITH lcMFiles 


* Detail info. of CT header (lcRpItem16).
* ---------------------------------------
SELECT lcTempDtl
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM16',;
	    cFileName  WITH 'CUTTKTH'   ,;
   	    cNdxName   WITH 'CUTTKTH'   ,;
        cAliasName WITH 'CUTTKTH'   ,;	  
	    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM16' ,;
	    cFileName  WITH 'CUTTKTL'    ,;
   	    cNdxName   WITH 'CUTTKTL'    ,;
        cAliasName WITH 'CUTTKTL'    ,;	  
	    lOpenBefor WITH .F.  

* Header info. of PO header (lcRpItem17).
* ---------------------------------------

*B604758,1 KHM 08/09/2001 (Begin) Adding the Open field to hold the open Qty.
*lcFName  = '(cStyType C(1)|Po C(6)|NStyOrder N(7)|Receive N(7)|cAdd_time C(11)|dAdd_Date D)'
lcFName  = '(cStyType C(1)|Po C(6)|NStyOrder N(7)|Receive N(7)|Open N(7)|cAdd_time C(11)|dAdd_Date D)'
*B604758,1 KHM 08/09/2001 (End)

lcMFiles = 'PoSHdr,PoSLn'

SELECT lcTempHdr
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM17' ,;
		cUpdFunNam WITH 'lfUpdHdDtl' ,;
		cVryFunNam WITH 'lfVryHdDtl' ,; 
		cIgnFunNam WITH 'IgnName'    ,; 		
		cUpdVryIgn WITH lcUpVrIg     ,;
		cAddSetup  WITH 'lfStpHdDtl' ,;
		cValidFun  WITH 'lfVldHdDtl' ,;
		cTempName  WITH 'lcSPO'      ,;
		cFileStr   WITH lcFName      ,;
		cMFiles    WITH lcMFiles 


* Detail info. of CT header (lcRpItem17).
* ---------------------------------------
SELECT lcTempDtl
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM17',;
	    cFileName  WITH 'POSHDR'    ,;
   	    cNdxName   WITH 'POSHDR'    ,;
        cAliasName WITH 'POSHDR'    ,;	  
	    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM17',;
	    cFileName  WITH 'POSLN'     ,;
   	    cNdxName   WITH 'POSLN'   ,;
        cAliasName WITH 'POSLN'   ,;	  
	    lOpenBefor WITH .F.  

*E301337,1 ASH 10/18/1999 (End)

*B605667,1 Header info for both Customer history and Style history files. [Begin]
*-- This item is related to the AR Module.
SELECT lcTempHdr

lcMFiles = 'InvHdr,InvLine,OrdHdr,RetHdr,Debit,Credit,ArHist,ArCusHst'

*--mhm C#200481 Update Flag to Verfy only for the AR Module [Start]
lcUpVrIg = 'V'  
*--mhm C#200481 [End]

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18' ,;
		cUpdFunNam WITH 'lfUpdCsHst' ,;
		cVryFunNam WITH 'lfVryCsHst' ,; 
		cIgnFunNam WITH 'IgnName'    ,; 		
		cUpdVryIgn WITH lcUpVrIg     ,;
		cAddSetup  WITH 'lfStpCsHst' ,;
		cValidFun  WITH 'lfVldHdDtl' ,;
		cTempName  WITH 'lcArCsHst'   ,;
		cFileStr   WITH ''           ,;
		cMFiles    WITH lcMFiles 

*--mhm C#200481 Update Flag to Verfy only for the AR Module [Start]
lcUpVrIg = 'I'  
*--mhm C#200481 [End]
		
* Detail info. of AR Customer History (lcRpItem18).
* --------------------------------------------------------
SELECT lcTempDtl
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18' ,;
	    cFileName  WITH 'INVHDR'     ,;
   	    cNdxName   WITH ''           ,;
        cAliasName WITH 'INVHDR'     ,;	  
	    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18' ,;
	    cFileName  WITH 'INVLINE'    ,;
   	    cNdxName   WITH 'INVLINE'   ,;
        cAliasName WITH 'INVLINE'  ,;	  
	    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18' ,;
	    cFileName  WITH 'ORDHDR'     ,;
   	    cNdxName   WITH ''           ,;
        cAliasName WITH 'ORDHDR'     ,;	  
	    lOpenBefor WITH .F.  

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18' ,;
	    cFileName  WITH 'RETHDR'     ,;
   	    cNdxName   WITH ''           ,;
        cAliasName WITH 'RETHDR'     ,;	  
	    lOpenBefor WITH .F.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18' ,;
	    cFileName  WITH 'DEBIT'      ,;
   	    cNdxName   WITH ''           ,;
        cAliasName WITH 'DEBIT'      ,;	  
	    lOpenBefor WITH .F.				

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18' ,;
	    cFileName  WITH 'CREDIT'     ,;
   	    cNdxName   WITH ''           ,;
        cAliasName WITH 'CREDIT'     ,;	  
	    lOpenBefor WITH .F.				

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18' ,;
	    cFileName  WITH 'ARHIST'     ,;
   	    cNdxName   WITH ''           ,;
        cAliasName WITH 'ARHIST'     ,;	  
	    lOpenBefor WITH .F.				

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM18' ,;
	    cFileName  WITH 'ARCUSHST'   ,;
   	    cNdxName   WITH 'ACTHST'     ,;
        cAliasName WITH 'ARCUSHST'   ,;	  
	    lOpenBefor WITH .F.				

*-- This item is related to the IC Module.
SELECT lcTempHdr

lcMFiles = 'InvHdr,InvLine,OrdHdr,OrdLine,RetHdr,RetLine,ICStyHst'

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM19' ,;
		cUpdFunNam WITH 'lfUpdStHst' ,;
		cVryFunNam WITH 'lfVryStHst' ,; 
		cIgnFunNam WITH 'IgnName'    ,; 		
		cUpdVryIgn WITH lcUpVrIg     ,;
		cAddSetup  WITH 'lfStpStHst' ,;
		cValidFun  WITH 'lfVldHdDtl' ,;
		cTempName  WITH 'lcIcStHst'   ,;
		cFileStr   WITH ''           ,;
		cMFiles    WITH lcMFiles 
		
* Detail info. of IC Style History (lcRpItem19).
* --------------------------------------------------------
SELECT lcTempDtl
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM19' ,;
	    cFileName  WITH 'INVHDR'     ,;
   	    cNdxName   WITH 'INVHDR'     ,;
        cAliasName WITH 'INVHDR'     ,;	  
	    lOpenBefor WITH .F.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM19' ,;
	    cFileName  WITH 'INVLINE'    ,;
   	    cNdxName   WITH ''           ,;
        cAliasName WITH 'INVLINE'    ,;	  
	    lOpenBefor WITH .F.

SELECT lcTempDtl
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM19' ,;
	    cFileName  WITH 'ORDHDR'     ,;
   	    cNdxName   WITH 'ORDHDR'     ,;
        cAliasName WITH 'ORDHDR'     ,;	  
	    lOpenBefor WITH .F.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM19' ,;
	    cFileName  WITH 'ORDLINE'    ,;
   	    cNdxName   WITH ''           ,;
        cAliasName WITH 'ORDLINE'    ,;	  
	    lOpenBefor WITH .F.

SELECT lcTempDtl
APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM19' ,;
	    cFileName  WITH 'RETHDR'     ,;
   	    cNdxName   WITH 'RETHDR'     ,;
        cAliasName WITH 'RETHDR'     ,;	  
	    lOpenBefor WITH .F.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM19' ,;
	    cFileName  WITH 'RETLINE'    ,;
   	    cNdxName   WITH ''           ,;
        cAliasName WITH 'RETLINE'    ,;	  
	    lOpenBefor WITH .F.

APPEND BLANK
REPLACE cItemName  WITH 'LCRPITEM19' ,;
	    cFileName  WITH 'ICSTYHST'   ,;
   	    cNdxName   WITH 'STYHST'     ,;
        cAliasName WITH 'ICSTYHST'   ,;	  
	    lOpenBefor WITH .F.				
*B605667,1 Header info for both Customer history and Style history files. [End]
		
*-- End of lfInfoHdr()

*!*************************************************************
*! Name      : lfErasTmp
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : - Erase temp. files 
*!               Erase temp. index. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfErasTmp()
*!*************************************************************
*E301294,1  AKA 08/23/99  
*!*************************************************************
FUNCTION lfErasTmp

IF FILE(gcWorkDir + lcInfoHdr  + '.DBF' )
  IF USED('lcTempHdr')
    SELECT lcTempHdr
    *E301387,1 ASH 03/22/2000 (Begin) Erase the temp files used in Hdr&Dtl reb.
    SCAN
      IF !EMPTY(cTempName) AND FILE(gcDataDir+cTempName+'.DBF')
        ERASE(gcDataDir+cTempName+'.DBF')
        IF FILE(gcDataDir+cTempName+'.CDX')
          ERASE(gcDataDir+cTempName+'.CDX')
        ENDIF
      ENDIF  
    ENDSCAN
    *E301387,1 ASH 03/22/2000 (End)
    USE    && Close info. header file 
  ENDIF
  ERASE (gcWorkDir + lcInfoHdr + '.DBF' )
  ERASE (gcWorkDir + lcInfoHdr + '.CDX' )
ENDIF  

IF FILE(gcWorkDir + lcInfoDtl  + '.DBF' )
  IF USED('lcTempDtl')
    SELECT lcTempDtl  
    USE    && Close info. detail file 
  ENDIF
  ERASE (gcWorkDir + lcInfoDtl + '.DBF' )
  ERASE (gcWorkDir + lcInfoDtl + '.CDX' )
ENDIF  

RETURN 
*-- End of lfErasTmp
 


*!*************************************************************
*! Name      : lfvFisYer
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : - Browse fiscal years 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvFisYer()
*!*************************************************************
*E301294,1  AKA 08/23/99  
*!*************************************************************
FUNCTION lfvFisYer
DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'CFisFYear:H="Year",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
laRpRetFld[1] = ''
lcFicsYr = EVALUATE(SYS(18))
lcRpCurFld = VARREAD()
lcOldAlias = SELECT()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time
IF NOT USED("FISHD") 
  SELECT 0
  USE (lcFilePath+'FISHD') ORDER TAG compfyear
  llUesdBefo = .T.
ENDIF
SELECT FISHD
 
IF EMPTY(lcFicsYr)   OR '?' $ &lcRpCurFld. .OR. !SEEK(lcFicsYr)  
 * lcBrowCond =  " FOR cFisYStat <> 'H' "
  =gfBrows( " FOR cFisYStat <> 'H' ",'CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
  &lcRpCurFld. = laRpRetFld[1]
  lcFicsYr   = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF
lcFiscYear = lcFicsYr   
IF llUesdBefo       && Close file if it was not used before.
  USE IN FISHD
ENDIF
SELECT (lcOldAlias)
RETURN 


*!*************************************************************
*! Name      : lfOpnFiles
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : Open files.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters      : ....
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfOpnFiles()
*!*************************************************************
*!
FUNCTION lfOpnFiles
PARAMETER lcItemId
* Convert it to upper case
lcItemId = UPPER(lcItemId)


PRIVATE lcCurrArea
lcCurrArea = SELECT(0)

PRIVATE llOpnError  , lcDoProg
llOpnError = .F.
lcDoProg = ""   && Holds any additional setu program

SELECT lcTempDtl
=SEEK(lcItemId)
SCAN REST WHILE cItemName = lcItemId  
  * Assign file name , Order name to variables
  lcFileName = lcFilePath+ALLTRIM(lcTempDtl.cFileName)+'.DBF'
  lcNdxName  = ALLTRIM(lcTempDtl.cNdxName)    
  lcAliasNam = ALLTRIM(lcTempDtl.cAliasName)    
  * If file if existed 
  IF FILE(lcFileName)
    IF USED(ALLTRIM(lcTempDtl.cFileName))
      SELECT (ALLTRIM(lcTempDtl.cFileName))
      USE
    ENDIF
    IF USED(lcAliasNam)
      SELECT (lcAliasNam)
      USE
    ENDIF
    =gfOpenFile(lcFileName , lcNdxName , 'SH', lcAliasNam )
  ELSE
  
    *-- Message : XXX Not found in data file.
    *--                   <Ok>
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = lcFileName+ " Not found in data file"
    laRebMsg[3] = " "
    *E301516,1 (Begin) If called from Auto balance End process
    IF llAutoBal
      laRebMsg[3] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
    ENDIF
    *E301516,1 (End)
    =lfVryRport()
    *= gfModalGen('TRM00001B00000','Alert',lcFileName)
    llOpnError = .T.
    * If there is error don't continue opening file and quit
    EXIT
  ENDIF
ENDSCAN

* If open files is succeed, check whether any additional setup need
* to be called. Otherwise close all other open files.
IF llOpnError 
  * Close all other open files
  =lfClosFile(lcItemId)
ENDIF

SELECT (lcCurrArea)
RETURN !llOpnError 
*-- end if lfOpnFiles.



*!*************************************************************
*! Name      : lfValdItm1
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : Check whether all mandatory factors that be used in
*!             GL are defined or not. 
*!*************************************************************
*! Example   : =lfValdItm1()
*!*************************************************************
FUNCTION lfValdItm1

PRIVATE lcCurrArea
lcCurrArea = SELECT(0)

PRIVATE llValdMand
llValdMand =  .T.

* Check fiscal year variable
IF EMPTY(lcRpFiscYr)
  llValdMand =.F.
  DECLARE laRebMsg[3]
  laRebMsg[1] = " "
  laRebMsg[2] = "Fiscal year is not valid, You have to enter valid year"
  laRebMsg[3] = " "
  *E301516,1 (Begin) If called from Auto balance End process
  IF llAutoBal
    laRebMsg[3] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
  ENDIF
  *E301516,1 (End)
  =lfVryRport()
ENDIF
SELECT (lcCurrArea)
RETURN llValdMand 
*--- End of lfValdItm1() 


*!*************************************************************
*! Name      : lfStupItm1
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : Additional GL setup that needs to process GL rebalance
*!*************************************************************
*! Example   : = lfStupItm1()
*!*************************************************************
FUNCTION  lfStupItm1

PRIVATE lcCurrArea
lcCurrArea = SELECT(0)

* Call this function here to get the current year form SycComp file
=lfGetCurYr()


lcTmpBBtch  = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Beginning batches to be Reposted
lcTmpJBtch  = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Normal batches to be Reposted
lcTmpCBtch  = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Closing batches to be Reposted
lcTmpTran   = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Single transactions to be Reposted
lcTmpGLTHd  = gfTempName()        && Varible to hold a Temp. name for a Temp. file to be used as a dum GLPTRNHD file
lcTmpGLTDt  = gfTempName()        && Varible to hold a Temp. name for a Temp. file to be used as a dum GLTRNSDT file

DECLARE laTempName[6] 
laTempName[1] =  lcTmpBBtch  
laTempName[2] =  lcTmpJBtch
laTempName[3] =  lcTmpCBtch 
laTempName[4] =  lcTmpTran 
laTempName[5] =  lcTmpGLTHd 
laTempName[6] =  lcTmpGLTDt


SELECT GLTRNSHD
COPY STRUCTURE TO (gcWorkDir + lcTmpGLTHd) WITH CDX
SELECT GLTRNSDT
COPY STRUCTURE TO (gcWorkDir + lcTmpGLTDt) WITH CDX

*IF There is any file that is opened with the Alias GLPTRNHD
IF USED('GLPTRNHD')
  llUsdTrnHd = .T.
  SELECT GLPTRNHD
  lcUsdTrnHd = DBF()          && Varible to hold the name and path of the file that was opened with the Alias GLPTRNHD
  lnUsdTrnHd = SELECT(0)      && Varible to hold the number of the work area that has the Alias GLPTRNHD
  USE
ENDIF    && End of IF

*IF There is any file that is opened with the Alias GLPTRNDT
IF USED('GLPTRNDT')
  llUsdTrnDt = .T.
  SELECT GLPTRNDT
  lcUsdTrnDt = DBF()          && Varible to hold the name and path of the file that was opened with the Alias GLPTRNDT
  lnUsdTrnDt = SELECT(0)      && Varible to hold the number of the work area that has the Alias GLPTRNDT
  USE
ENDIF    && End of IF

USE (gcWorkDir + lcTmpGLTHd) ALIAS GLPTRNHD IN 0
USE (gcWorkDir + lcTmpGLTDt) ALIAS GLPTRNDT IN 0

*IF There is any file that is opened with the Alias GLACBALS
IF USED('GLACBALS')
  llUsdAcBal = .T.
  SELECT GLACBALS
  lnUsdAcBal = SELECT(0)      && Varible to hold the number of the work area that has the Alias GLACBALS
  lcUsdAcBal = DBF()          && Varible to hold the name and path of the file that was opened with the Alias GLACBALS
  USE
ENDIF    && End of IF

IF lcTempHdr.cUpdVryIgn='U' AND !FILE(lcFilePath + 'GLTMPBAL.DBF')
  =lfVryItem1()
ENDIF

*IF The file GLTMPBAL exist
IF FILE(lcFilePath + 'GLTMPBAL.DBF')
  llBalnBfor = .T.
  lcUpDateSt = 'ENABLE'
  USE (lcFilePath + 'GLTMPBAL.DBF') ALIAS GLACBALS IN 0 EXCLUSIVE
ENDIF    && End of IF


*IF The memory variable file exist
IF FILE(lcFilePath + 'GLRBAL.MEM')
  RESTORE FROM (lcFilePath + 'GLRBAL.MEM') ADDITIVE
ENDIF    && End of IF
lnPostTran = IIF(TYPE('mvPostTran') <> 'N' , 0 , mvPostTran)    && Varible to hold the number of transactions that was Reposted in the Rebalance process
RELEASE ALL LIKE mv*

SELECT (lcCurrArea)
RETURN 




*!*************************************************************
*! Name      : lfVryItem1
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : Valid function for the Verify of (lcRpItem1)
*!*************************************************************
*! Called from : ARREBAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfVryItem1
lcRBalYear = lcFiscYear
llBalnBfor = .F.

*IF The Starting year is the Previous year
IF VAL(lcFiscYear) < VAL(lcCurr_yer)
  * Meesage : "You have selected to rebalance starting previous year (ð). "
  *           "The system allows purging files of the previous year.      "
  *           "Rebalancing starting from previous year wan't be accurate  "
  *           "if you purged the files through this year.                 "
  * Buttons :           <Proceed> <Cacel>

  *E301516,1 (Begin) If called from Auto balance send messag to log file.
  *IF gfModalGen("TRM02250B00012" , "DIALOG" , lcFiscYear) = 2
  *  RETURN
  *ENDIF    
  IF !llAutoBal
    IF gfModalGen("TRM02250B00012" , "DIALOG" , lcFiscYear) = 2
      RETURN
    ENDIF    
  ELSE
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = "You have selected to rebalance starting previous year."
    laRebMsg[3] = "The system allows purging files of the previous year."
    laRebMsg[4] = "Rebalancing starting from previous year won't be accurate"
    laRebMsg[5] = "if you purged the files through this year.                "
    laRebMsg[6] = " "
    =lfVryRport()
  ENDIF
  *E301516,1 (End)
ENDIF    
=lfFixBatch()

*IF There is any Batches or Transactions to be Reposted
IF lfSelData()
  *IF There is Single transactions to be Reposted and not all of them was  
  *Reposted succesfully
  *E301337,1 ASH 10/18/1999 (Begin) Change the way of calling LFTBPOST function.
  *IF RECCOUNT(lcTmpTran) <> 0 ;
     .AND. RECCOUNT(lcTmpTran) <> lfTBPost("Transactions" , lcTmpTran , '', "GLACBALS")
  
  IF RECCOUNT(lcTmpTran) <> 0    
    lnTBPosted = 0
    llFrReb = .T.
    DO (gcAppHome+"GL.APP") WITH "LFTBPOST WITH 'Transactions',lcTmpTran ,'CLOSING','GLACBALS',lcComp_Id,lcFilePath,llFrReb",'',"T"     
    IF RECCOUNT(lcTmpTran) <> lnTBPosted
  *E301337,1 ASH 10/18/1999 (End)  
    * Meesage : "Some of the ð were not succesfully reposted to the "
    *           "balances file , Unable to proceed with rebalancing."
    * Buttons :                           <Ok>
    
      DECLARE laRebMsg[5]
      laRebMsg[1] = " "
      laRebMsg[2] = PADC("GL Rebalance" , 68) 
      laRebMsg[3] = "Some of the transactions were not succesfully reposted to the " 
      laRebMsg[4] = "balances file , Unable to proceed with rebalancing."
      laRebMsg[5] = " "
      *E301516,1 (Begin) If called from Auto balance End process
      IF llAutoBal
        laRebMsg[5] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
      ENDIF
      *E301516,1 (End)
      =lfVryRport()
      *=gfModalGen("TRM02251B00000" , "DIALOG" , "transactions")
      =lfRmTmpBal()
      RETURN
    ENDIF    && End of IF
  ENDIF
  *IF There is Beginning batches to be Reposted and not all of them was  
  *Reposted succesfully
  *E301337,1 ASH 10/18/1999 (Begin) Change the way of calling LFTBPOST function.
  *IF RECCOUNT(lcTmpBBtch) <> 0 ;
     .AND. RECCOUNT(lcTmpBBtch) <> lfTBPost("Batch" , lcTmpBBtch , "Beginning","GLACBALS")
  IF RECCOUNT(lcTmpBBtch) <> 0      
    lnTBPosted = 0
    llFrReb = .T.
    DO (gcAppHome+"GL.APP") WITH "LFTBPOST WITH 'Batch',lcTmpBBtch ,'Beginning','GLACBALS',lcComp_Id,lcFilePath,llFrReb",'',"T"          
    IF RECCOUNT(lcTmpBBtch) <> lnTBPosted
   *E301337,1 ASH 10/18/1999 (End)
    * Meesage : "Some of the ð were not succesfully reposted to the "
    *           "balances file , Unable to proceed with rebalancing."
    * Buttons :           <Ok>
    
      DECLARE laRebMsg[5]
      laRebMsg[1] = " "
      laRebMsg[2] = PADC("Beginning Batches" , 68) 
      laRebMsg[3] = "Some of the Batches were not succesfully reposted to the " 
      laRebMsg[4] = "balances file , Unable to proceed with rebalancing."
      laRebMsg[5] = " "
      *E301516,1 (Begin) If called from Auto balance End process
      IF llAutoBal
        laRebMsg[5] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
      ENDIF
      *E301516,1 (End)
      =lfVryRport()
      *=gfModalGen("TRM02251B00000" , "DIALOG" , "beginning batches")    
      =lfRmTmpBal()
      RETURN
    ENDIF    && End of IF
  ENDIF
  *IF There is Normal batches to be Reposted and not all of them was  
  *Reposted succesfully
  *E301337,1 ASH 10/18/1999 (Begin) Change the way of calling LFTBPOST function.
  *IF RECCOUNT(lcTmpJBtch) <> 0 ;
     .AND. RECCOUNT(lcTmpJBtch) <> lfTBPost("Batch" , lcTmpJBtch , "Nonbeginning","GLACBALS")
  IF RECCOUNT(lcTmpJBtch) <> 0 
    lnTBPosted = 0 
    llFrReb = .T.
    DO (gcAppHome+"GL.APP") WITH "LFTBPOST WITH 'Batch',lcTmpJBtch ,'Nonbeginning','GLACBALS',lcComp_Id,lcFilePath,llFrReb",'',"T"          
    IF RECCOUNT(lcTmpJBtch) <> lnTBPosted
      *E301337,1 ASH 10/18/1999 (End)
      * Message : "Some of the ð were not succesfully reposted to the "
      *           "balances file , Unable to proceed with rebalancing."
      * Buttons :           <Ok>
      DECLARE laRebMsg[5]
      laRebMsg[1] = " "
      laRebMsg[2] = PADC("Batches" , 68) 
      laRebMsg[3] = "Some of the Batches were not succesfully reposted to the " 
      laRebMsg[4] = "balances file , Unable to proceed with rebalancing."
      laRebMsg[5] = " "
      *E301516,1 (Begin) If called from Auto balance End process
      IF llAutoBal
        laRebMsg[5] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
      ENDIF
      *E301516,1 (End)
      =lfVryRport()
      *=gfModalGen("TRM02251B00000" , "DIALOG" , "batches")
      =lfRmTmpBal()
      RETURN
    ENDIF    && End of IF
  ENDIF
  mvYear = lcFiscYear
  mvPostTran = lnPostTran
  SAVE TO (lcFilePath + 'GLRBAL.MEM') ALL LIKE mv*
  SHOW GET pbReport ENABLE
  SHOW GET pbUpDate ENABLE
ELSE    && Else
  * Meesage : "No batche's or transaction's were reposted."
  * Buttons :                <Ok>
  DECLARE laRebMsg[3]
  laRebMsg[1] = " "
  laRebMsg[2] = "No batche's or transaction's were reposted."
  laRebMsg[3] = " "
  *E301516,1 (Begin) If called from Auto balance End process
  IF llAutoBal
    laRebMsg[3] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
  ENDIF
  *E301516,1 (End)
  =lfVryRport()
  *=gfModalGen("TRM02252B00000" , "DIALOG")  
  
  mvYear = lcFiscYear
  mvPostTran = lnPostTran
  SAVE TO (lcFilePath + 'GLRBAL.MEM') ALL LIKE mv*
  SHOW GET pbReport ENABLE
  SHOW GET pbUpDate ENABLE
ENDIF    && End of IF
IF USED('GLTMPBAL')
  USE IN GLTMPBAL
ENDIF
IF USED('GLACBALS')
  USE IN GLACBALS
ENDIF
*-- End of lfVryItem1()

*!*************************************************************
*! Name      : lfFixBatch
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : Function to Fix the GLBATCH file
*!*************************************************************
*! Called from : lfVryItem1()
*!*************************************************************
*! Calls       : lfPreData() , gfThermo()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfFixBatch
PRIVATE lcSetDelSt , llFixSom

llFixSom = .F.                  && Flag to know if we have fixed any records
lcSetDelSt = SET('DELETED')     && Varible to save the SET DELETED status
SET DELETED OFF

lcBatType = ''          && Varible to hold the Batch type
ldBatPBeg = {}          && Varible to hold the Batch beging poste date
ldBatPEnd = {}          && Varible to hold the Batch ending poste date
lnBatCnTot = 0          && Varible to hold the Batch Total Dibet

SET ORDER TO TAG BATCHNO  IN GLBATCH
SELECT GLTRNSHD
SET ORDER TO TAG BATCHTRN
SET RELATION TO cBatchNo INTO GLBATCH
GO TOP

lnCurRec = 0                && Varible to use for the thermometer
lnTotRec = RECCOUNT()       && Varible to use for the thermometer

*SCAN Loop to scan the Posted transaction header for the records that
*has Batch number and this Batch number dose not exist in the GLBATCH
*file or was deleted
SCAN FOR cBatchNo + cTranNo <> '000000' ;
    .AND. (EOF('GLBATCH') .OR. DELETED('GLBATCH'))

  llFixSom = .T.
  
  *IF The Batch number was deleted
  IF DELETED('GLBATCH')
    lnCurRec = lnCurRec + 1
    SELECT GLBATCH
    RECALL
    SELECT GLTRNSHD
  ELSE    && Else
    =lfPreData()
    SKIP -1
    INSERT INTO GLBATCH (cBatchNo , cBatStat , lBatInd , cBatType ,;
                         cBatPyr , dBatPBeg , dBatPEnd , nBatCnTot ,;
                         nBaTotDR , nBaTotCR , cSrcModul , cComp_ID ,;
                         cPostSess , cPostProg , cPostUser , dPostDate ,;
                         cPostTime , cAdd_User , dAdd_Date , cAdd_Time);
         VALUES (GLTRNSHD.cBatchNo , GLTRNSHD.cTrnStat , .F. , lcBatType ,;
                 GLTRNSHD.cTrnPYr , ldBatPBeg , ldBatPEnd , lnBatCnTot ,;
                 lnBatCnTot , lnBatCnTot , GLTRNSHD.cSrcModul ,;
                 GLTRNSHD.cComp_ID , GLTRNSHD.cPostSess , GLTRNSHD.cPostProg ,;
                 GLTRNSHD.cPostUser , GLTRNSHD.dPostDate , GLTRNSHD.cPostTime ,;
                 GLTRNSHD.cAdd_User , GLTRNSHD.dAdd_Date , GLTRNSHD.cAdd_Time)
  ENDIF    && End of IF
  =gfThermo(lnTotRec , lnCurRec , 'Fixing Batches file')
ENDSCAN    && End of Scan Loop
*IF we had Fix any records
IF llFixSom
  *FOR Loop to complete the thermometer
  FOR lnRst = lnCurRec TO lnTotRec
    =gfThermo(lnTotRec , lnRst , 'Fixing Batches file')
  ENDFOR    && End of FOR Loop
ENDIF    && End of IF
SET RELATION TO
SET ORDER TO
SET ORDER TO TAG BATSTAT IN GLBATCH
SET DELETED &lcSetDelSt
*-- End of lfFixBatch()

*!*************************************************************
*! Name      : lfSelData
*! Developer : Amin Khodary Amin
*! Date      : 08/23/1999
*! Purpose   : Function to get the Single transactions ,
*!             Beginning batches , Normal batches and Closing batches
*!             to be Reposted and repare the Temp. Balance file for
*!             Reposting
*!*************************************************************
*! Called from : lfVryItem1()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T. if ther is any Batches or Transactions to be
*!               Reposted , .F. Otherwise
*!*************************************************************
*
FUNCTION lfSelData

PRIVATE lnPBatTran , lnTotal , lnCurrent , lcAccount , llBalShtAc

lnPBatTran = 0        && Varible to hold the number of Transactions in the Batches to be Reposted
lnPostTran = 0

CREATE TABLE (gcWorkDir + lcTmpBBtch) (cBatchNo C(6) , cBatStat C(1))
INDEX ON cBatchNo TAG CBATCHNO 
SET ORDER TO CBATCHNO      

CREATE TABLE (gcWorkDir + lcTmpJBtch) (cBatchNo C(6) , cBatStat C(1))
INDEX ON cBatchNo TAG CBATCHNO 
SET ORDER TO CBATCHNO      

CREATE TABLE (gcWorkDir + lcTmpCBtch) (cBatchNo C(6) , cBatStat C(1))
INDEX ON cBatchNo TAG CBATCHNO 
SET ORDER TO CBATCHNO      

CREATE TABLE (gcWorkDir + lcTmpTran) (cTranNo C(8) , cTrnStat C(1))
INDEX ON cTranNo TAG CTRANNO 

SELECT GLBATCH

*IF There is a posted Batches
IF SEEK('P')
  *SCAN Loop to scan the GLBATCH file [The Batches file] FOR the posted
  SCAN REST WHILE cBatStat = 'P'  FOR cBatPYr >= lcFiscYear
    *DO CASE Statment
    DO CASE
      *Case of Closing Batches
      CASE cBatType = 'C'
        INSERT INTO &lcTmpCBtch (cBatchNo , cBatStat) VALUES;
                                (GLBATCH.cBatchNo , GLBATCH.cBatStat)
      
      *Case of Beginning Batches
      CASE cBatType = 'B'
        INSERT INTO &lcTmpBBtch (cBatchNo , cBatStat) VALUES;
                                (GLBATCH.cBatchNo , GLBATCH.cBatStat)
      
      *Otherwise
      OTHERWISE
        INSERT INTO &lcTmpJBtch (cBatchNo , cBatStat) VALUES;
                                (GLBATCH.cBatchNo , GLBATCH.cBatStat)
    
    ENDCASE    && End of DO CASE Statment
    
  ENDSCAN    && End of SCAN Loop
ENDIF    && End of IF

SELECT GLTRNSHD

*SCAN Loop to scan the GLTRNSHD file [The Posted Transactios header file
*opened with the Alias GLTRNSHD] FOR Batch # = 0 and cTrnPYr > or = the
*Starting year
SCAN FOR cBatchNo + cTranNo = '000000' .AND. cTrnPYr >= lcFiscYear
  lnPostTran = lnPostTran + 1
  INSERT INTO &lcTmpTran (cTranNo , cTrnStat) VALUES;
                         (GLTRNSHD.cTranNo , GLTRNSHD.cTrnStat)
ENDSCAN    && End of SCAN Loop

COUNT FOR cBatchNo + cTranNo <> '000000';
     .AND. cTrnPYr >= lcFiscYear;
       TO lnPBatTran
lnPostTran = lnPostTran + lnPBatTran
*IF The Temp. Balance file [Opened with the Alias GLACBALS] exist
IF USED('GLACBALS')
  USE IN GLACBALS
  *IF The file GLTMPBAL [Temp. Balance file] exist
  IF FILE(lcFilePath + 'GLTMPBAL.DBF')
    ERASE (lcFilePath + 'GLTMPBAL.DBF')
  ENDIF    && End of IF
  *IF The CDX file of GLTMPBAL [Temp. Balance file] exist
  IF FILE(lcFilePath + 'GLTMPBAL.CDX')
    ERASE (lcFilePath + 'GLTMPBAL.CDX')
  ENDIF    && End of IF
ENDIF    && End of IF

SELECT GLTMPBAL
COPY STRUCTURE TO (lcFilePath + 'GLTMPBAL.DBF') WITH CDX
USE (lcFilePath + 'GLTMPBAL.DBF') ALIAS GLACBALS IN 0

SELECT GLTMPBAL
lnTotal = RECCOUNT()        && Varible to hold the number of Account Balances file records [Opened with the Alias GLTMPBAL]
lnCurrent = 0               && Varible to hold the number of records that was finished
GO TOP

*SCAN Loop to Scan the Account Balances file 
SCAN
  lcAccount = cAcctCode        && Varible to hold the current account code
  =SEEK(cAcctCode , 'GLACCHAR')
  llBalShtAc = IIF(LEFT(GLACCHAR.cTypeCode , 1) $ 'ALQY' , .T. , .F.)    && Flag to know if the account is one of the Balance shet accounts
  
  *SCAN Loop to Scan the Account Balances file for the current account
  *and Fiscal year < the Starting from year
  SCAN REST WHILE cAcctCode = lcAccount .AND. cFisFYear < lcFiscYear
    lnCurrent = lnCurrent + 1
    SCATTER MEMVAR MEMO
    INSERT INTO GLACBALS FROM MEMVAR
    =gfThermo(lnTotal , lnCurrent , 'Fixing Account balance file')
  ENDSCAN    && End of SCAN Loop
  
  SKIP -1
  IF lcAccount <> cAcctCode
    lnClosBal = 0
  ELSE  
    lnClosBal = nAcbClBal
  ENDIF
  SCATTER FIELDS LIKE N* MEMVAR BLANK
  
  *IF We are at the Begin of the Account Balances file
  IF BOF()
    GO RECNO()
  ELSE    && Else
    SKIP 1
  ENDIF    && End of IF
  
  *SCAN Loop to Scan the Account Balances file for the current account
  *and Fiscal year > or = the Starting from year
  SCAN REST WHILE cAcctCode = lcAccount
    lnCurrent = lnCurrent + 1
    SCATTER FIELDS EXCEPT N* MEMVAR MEMO
    INSERT INTO GLACBALS FROM MEMVAR

    *IF the account is one of the Balance shet accounts
    IF llBalShtAc
      REPLACE GLACBALS.nAcbOpBal WITH lnClosBal;
              GLACBALS.nAcbClBal WITH lnClosBal
    ENDIF    && End of IF
    =gfThermo(lnTotal , lnCurrent , 'Fixing Account balance file')
  ENDSCAN    && End of SCAN Loop
  SKIP -1  
ENDSCAN    && End of SCAN Loop
=gfThermo(lnTotal , lnTotal , 'Fixing Account balance file')
RETURN lnPostTran <> 0
*-- End of lfSelData()

*!*************************************************************
*! Name      : lfRmTmpBal
*! Developer : Amin Khodary Amin
*! Date      : 08/23/1999
*! Purpose   : Function to delete the GLTMPBAL file and the
*!             GLRBAL.MEM memory variable file
*!*************************************************************
*! Called from : lfVryItem1()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfRmTmpBal

lcRBalYear = lcFiscYear
lnPostTran = 0

*IF The Alias GLACBALS exist [That was used for the Temp. balance file]
IF USED('GLACBALS')
  USE IN GLACBALS
ENDIF    && End of IF

*IF The file GLTMPBAL exist [The Temp. balance file]
IF FILE(lcFilePath + 'GLTMPBAL.DBF')
  ERASE (lcFilePath + 'GLTMPBAL.DBF')
ENDIF    && End of IF

*IF The CDX file of GLTMPBAL [Temp. Balance file] exist
IF FILE(lcFilePath + 'GLTMPBAL.CDX')
  ERASE (lcFilePath + 'GLTMPBAL.CDX')
ENDIF    && End of IF

*IF The memory variable file exist
IF FILE(lcFilePath + 'GLRBAL.MEM')
  ERASE (lcFilePath + 'GLRBAL.MEM')
ENDIF    && End of IF
*-- End of lfRmTmpBal()


*!*************************************************************
*! Name      : lfPreData
*! Developer : Amin Khodary Amin
*! Date      : 08/23/1999
*! Purpose   : Function to get the values for cBatType , nBatCnTot
*!             dBatPBeg , dBatPEnd
*!*************************************************************
*! Called from : lfFixBatch()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfPreData
PRIVATE lcFisYear , lcMinPrd , lcMaxPrd , lcBatchNo
*DO CASE Statment to get the Batch type
DO CASE
  *Case The transaction type is 'Y'
  CASE GLTRNSHD.cTrnType = 'Y'
    lcBatType = 'C'
  
  *Case The transaction type is 'B'
  CASE GLTRNSHD.cTrnType = 'B'
    lcBatType = 'B'
  
  *Otherwise
  OTHERWISE
    *IF the accounts in the Transactions lines for this Batch is
    *Statystical account
    IF SEEK(GLTRNSHD.cBatchNo , 'GLTRNSDT') .AND.;
       SEEK(GLTRNSDT.cAcctCode , 'GLACCHAR') .AND.;
       LEFT(GLACCHAR.cTypeCode , 1) = 'Y'
      lcBatType = 'S'
    ELSE    && Else
      lcBatType = 'N'
    ENDIF    && End of IF  
ENDCASE    && End of CASE Statment

lcBatchNo = cBatchNo      && Varible to hold the Batch number
lcFisYear = cTrnPYr       && Varible to hold the Fiscal year
lcMinPrd = cTrnPPrd       && Varible to hold the Min. Prioud
lcMaxPrd = cTrnPPrd       && Varible to hold the Max. Prioud
lnBatCnTot = 0

*SCAN Loop to scan the Posted transaction header for the current Batch
SCAN REST  WHILE cBatchNo = lcBatchNo
  lnCurRec = lnCurRec + 1
  lcMinPrd = MIN(lcMinPrd , cTrnPPrd)
  lcMaxPrd = MAX(lcMaxPrd , cTrnPPrd)
  lnBatCnTot = lnBatCnTot + nTrnTotDR
ENDSCAN    && End of SCAN Loop

ldBatPBeg = IIF(SEEK(lcFisYear + lcMinPrd , 'FSPRD') ,;
                FSPRD.dFspPBgDt , {})

ldBatPEnd = IIF(SEEK(lcFisYear + lcMaxPrd , 'FSPRD') ,;
                FSPRD.dFspPEnDt , {})

*-- End of lfPreData()


*!*************************************************************
*! Name      : lfUpdItem1
*! Developer : Amin Khodary Amin
*! Date      : 08/23/1999
*! Purpose   : Update master file of (lcRpItem1)
*!*************************************************************
*! Called from : Screen GLRBAL
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfUpdItem1


PRIVATE lnCurPost
lnCurPost = 0          && Varible to hold the number of Transactions posted with cTrnPYr > or = Starting year

*IF the Rebalance process was done befor in another session without Updating
*the master file
IF llBalnBfor
  SELECT GLTRNSHD
  COUNT FOR cTrnPYr >= lcRpFiscYr TO lnCurPost
  
  *IF the number of Transactions posted with cTrnPYr > or = Starting year
  *dose not equal the the number of transactions that was Reposted in the
  *Rebalance process
  IF lnCurPost <> lnPostTran
    
    * Meesage : "One or more transaction were posted since the last "
    *           "recaculation of balances. You need to run the      "
    *           "recalculation process again.                       " 
    * Buttons :                     <Ok>
    DECLARE laRebMsg[5]
    laRebMsg[1] = " "
    laRebMsg[2] = "One or more transaction were posted since the last "
    laRebMsg[3] = "recaculation of balances. You need to run the "
    laRebMsg[4] =  "recalculation process again.                       " 
    laRebMsg[5] = " "
    *E301516,1 (Begin) If called from Auto balance End process
    IF llAutoBal
      laRebMsg[5] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
    ENDIF
    *E301516,1 (End)
    =lfVryRport()
    *=gfModalGen("TRM02253B00000" , "DIALOG")    
    =lfRmTmpBal()
    RETURN
  ENDIF    && End of IF
  
  SELECT GLACBALS
  SET ORDER TO TAG ACCYRPRD
  SET RELATION TO cAcctCode INTO GLACCHAR
  LOCATE FOR EOF('GLACCHAR')
  
  *IF There is an account that exist in the Temp. balance file and dose
  *not exist in the GLACCHAR file [Chart of accounts file]
  IF FOUND()
    SET RELATION TO
    * Meesage : "Found mismatch between the current chart of account's, and"
    *           "the recalculated balances file. You need to run the       "
    *           "recalculation process again.                              "
    * Buttons :                         <Ok>


    DECLARE laRebMsg[5]
    laRebMsg[1] = " "
    laRebMsg[2] = "Found mismatch between the current chart of account's, and"
    laRebMsg[3] = "the recalculated balances file. You need to run the       "
    laRebMsg[4] = "recalculation process again. " 
    laRebMsg[5] = " "
    *E301516,1 (Begin) If called from Auto balance End process
    IF llAutoBal
      laRebMsg[5] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
    ENDIF
    *E301516,1 (End)
    =lfVryRport()
    *=gfModalGen("TRM02254B00000" , "DIALOG")
    =lfRmTmpBal()
    RETURN
  ENDIF    && End of IF
  
  SET RELATION TO
  SELECT GLACCHAR
  SET RELATION TO cAcctCode INTO GLACBALS
  LOCATE FOR EOF('GLACBALS')
  
  *IF There is an account that exist in the GLACCHAR file [Chart of accounts
  *file] and dose not exist in the Temp. balance file
  IF FOUND()
    SET RELATION TO
    
    * Meesage : "Found mismatch between the current chart of account's, and"
    *           "the recalculated balances file. You need to run the       "
    *           "recalculation process again.                              "
    * Buttons :                     <Ok>
    DECLARE laRebMsg[5]
    laRebMsg[1] = " "
    laRebMsg[2] = "Found mismatch between the current chart of account's, and"
    laRebMsg[3] = "the recalculated balances file. You need to run the"
    laRebMsg[4] = "recalculation process again.       "
    laRebMsg[5] = " "
    *E301516,1 (Begin) If called from Auto balance End process
    IF llAutoBal
      laRebMsg[5] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
    ENDIF
    *E301516,1 (End)
    =lfVryRport()
    *=gfModalGen("TRM02254B00000" , "DIALOG")    
    =lfRmTmpBal()
    RETURN
  ENDIF    && End of IF
  SET RELATION TO
ENDIF    && End of IF

IF !FILE(lcFilePath + 'GLTMPBAL.DBF') 
  RETURN
ENDIF
* Meesage : "We strongly recommend that a backup is done for company ð data"
*           "files immediately prior to the ð process.  Do you wish to     "
*           "continue?                                                     "
* Buttons :                     <Proceed>    <Cancel>
*IF The user selected to cancel

*E301516,1 (Begin) If called from Auto balance always make a backup.
*IF gfModalGen("TRM02255B00012" , "ALERT" , lcComp_ID + '|Update') = 2
*  RETURN
*ENDIF    && End of IF
IF !llAutoBal
  IF gfModalGen("TRM02255B00012" , "ALERT" , lcComp_ID + '|Update') = 2
    RETURN
  ENDIF    && End of IF
ENDIF  
*E301516,1 (End) 
IF USED('GLACBALS')
  USE IN GLACBALS
ENDIF
IF USED('GLTMPBAL')
  USE IN GLTMPBAL
ENDIF
RENAME (lcFilePath + 'GLACBALS.DBF') TO (lcFilePath + 'TMPNAME.DBF')
RENAME (lcFilePath + 'GLACBALS.CDX') TO (lcFilePath + 'TMPNAME.CDX')
RENAME (lcFilePath + 'GLTMPBAL.DBF') TO (lcFilePath + 'GLACBALS.DBF')
RENAME (lcFilePath + 'GLTMPBAL.CDX') TO (lcFilePath + 'GLACBALS.CDX')

ERASE (lcFilePath + 'TMPNAME.DBF')
ERASE (lcFilePath + 'TMPNAME.CDX')

*IF The memory variable file exist
IF FILE(lcFilePath + 'GLRBAL.MEM')
  ERASE (lcFilePath + 'GLRBAL.MEM')
ENDIF    && End of IF
USE (lcFilePath + 'GLACBALS.DBF') ALIAS GLTMPBAL ORDER TAG ACCYRPRD IN 0
*--- End of lfUpdItem1()




*!*************************************************************
*! Name      : lfGetCurYr
*! Developer : Amin Khodary Amin
*! Date      : 08/23/1999
*! Purpose   : Get the cuurent year. This varuable was defined before in   
*!             GL module.   
*!*************************************************************
FUNCTION lfGetCurYr

=gfSysOpen(gcSysHome+'SYCCOMP',gcSysHome+'CCOMP_ID','SH')
lcCurr_yer = IIF(SEEK(gcPrnt_Cmp,'SYCCOMP'),sycComp.cCurr_Yer,'')
lnCurr_yer = INT(VAL(lcCurr_yer))
=gfSysClose("SYCCOMP")

=gfOpenFile(lcFilePath+'ACCOD', '' ,'SH')
SELECT ACCOD
GO TOP
lcAcsMask  = ACCOD.cAcsMask
lcAcsMask  = "X"+SUBSTR(cAcsMask,2)
lcAcsMask  = ALLTRIM(STRTRAN(lcAcsMask,'#','9'))
lnAcsSegSz = ACCOD.nAcsSegSz 
=gfCloseFile("ACCOD")


*!*************************************************************
*! Function  : lfVryRport
*! Developer : Amin Khodary Amin
*! Date      : 08/23/1999
*! Purpose   : - Create a low level text file (ReBalLog.txt). 
*!             - Append rebalance notes to rebalance log file. 
*!*************************************************************
FUNCTION lfVryRport

IF !llOpenRep  
  llOpenRep = .T.
  lcFilHandl = FCREAT('&gcWorkDir.ReBalLog.txt')
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,"*                  REBALANCE VERIFICATION  REPORT                  *")
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,' ')
  =FPUTS(lcFilHandl,' ')
ENDIF
lnRebMsg  =  ALEN(laRebMsg,1) 
FOR lnCount = 1 TO lnRebMsg  
  =FPUTS(lcFilHandl,laRebMsg[lnCount])
ENDFOR  
    

*-- End of lfVryRport()

*!*************************************************************
*! Function  : lfViewRLog
*! Developer : Amin Khodary Amin
*! Date      : 08/23/1999
*! Purpose   : - Preview rebalance log file (ReBalLog.txt). 
*!*************************************************************
FUNCTION lfViewRLog

*** If there is a report created dispaly it to the user
IF llOpenRep
  =FFLUSH(lcFilHandl)
  * Close the text file
  DO WHILE !FCLOSE(lcFilHandl)
  ENDDO
  CREATE CURSOR TMPSTR (mStrRep M(10))
  APPEND BLANK
  APPEND MEMO mStrRep FROM (gcWorkDir+"ReBalLog.txt") OVERWRITE
  
  *--mhm C#200481 Get Screen of Report from SM Module [Start]
  *DO (gcScrDir + gcWinAppl + '\SMSTRREP.SPR')
  DO (gcScrDir + 'SM' + '\SMSTRREP.SPR')
  *--mhm C#200481  [End]
  
  USE IN TMPSTR
ELSE
  =gfModalGen("TRM00366B00000" , "DIALOG")   
ENDIF





*!*************************************************************
*! Function  : lfwShowRep
*! Developer : Amin Khodary Amin
*! Date      : 08/23/1999
*! Purpose   : - Show report
*!*************************************************************
FUNCTION lfwShowRep
=gfSysOpen(gcSysHome+'SYCCOMP',gcSysHome+'CCOMP_ID','SH')
lcRpFiscYr = IIF(SEEK(gcPrnt_Cmp,'SYCCOMP'),sycComp.cCurr_Yer,'')
lcFiscYear = lcRpFiscYr 
*=gfSysClose("SYCCOMP")
RETURN 



*!*************************************************************
*! Name      : lfVryItem2
*! Developer : Amin Khodary Amin
*! Date      : 08/23/1999
*! Purpose   : Verify function for AP vendor  (lcRpItem2)
*!*************************************************************
FUNCTION lfVryItem2
PRIVATE laStructur , lnCount

*Set the Order Tags for all needed files
SET ORDER TO TAG VENCODE  IN APRBALV
SET ORDER TO TAG VENDYEAR IN APRBALVH
SET ORDER TO TAG VENCODE  IN APVENDOR
SET ORDER TO TAG VENDYEAR IN APVENHST
SET ORDER TO TAG VENDINV  IN APINVHDR
SET ORDER TO TAG INVVEND  IN APDIST

SELECT APVENDOR

*If We are going to recalculate the balances for all Vendors
lnTotal = RECCOUNT()              && These variable is for the thermometer
lnCurrent = 0                     && These variable is for the thermometer
*SCAN Loop to scan the Vendor file
SCAN
  =lfRbalVend()
  lnCurrent = lnCurrent  + 1
  =gfThermo(lnTotal , lnCurrent , 'Recalculating balances...' ,;
            'Vendor : ' + ALLTRIM(APVENDOR.cVendCode))
  SELECT APVENDOR
ENDSCAN    && End of SCAN Loop to scan the Vendor file
  
*If The thermometer is not finished
IF lnCurrent < lnTotal
  *FOR Loop to finish the thermometer
  FOR lnCount = lnCurrent TO lnTotal
    =gfThermo(lnTotal , lnCount , 'Recalculating balances...')
  ENDFOR    && End of FOR Loop to finish the thermometer
ENDIF   
*-- End of lfVryItem2()


*!*************************************************************
*! Name      : lfRbalVend
*! Developer : Amin Khodary Amin
*! Date      : 08/23/1999
*! Purpose   : Function to recalculate the Vendor balances
*!*************************************************************
*! Called from : lfVryItem2()
*!*************************************************************
FUNCTION lfRbalVend
PRIVATE lcPeriod , lcField
* Initialize OpenDebit variables
lnInvAmnt = 0       
lnInvPaid = 0 
lnInvDisTk= 0 
lnInvAdj  = 0 

SCATTER MEMVAR MEMO
M.NVENBAL = 0
M.NVENCPUR = 0
M.NVENCPAY = 0
M.NVENOPNDR = 0

SELECT APRBALV

*If The Vendor code dose not exist in the Vendor rebalance file
IF !SEEK(M.cVendCode)
  APPEND BLANK
ENDIF    && End of IF The Vendor code dose not exist in the Vendor rebalance file
GATHER MEMVAR MEMO

SELECT APVENHST
SCATTER MEMVAR MEMO BLANK
M.cVendCode = APVENDOR.cVendCode

SELECT FISHD
GO TOP
*SCAN Loop to scan the Fiscal Calendar file for the current company
SCAN 
  M.cFisFYear = cFisFYear
  SELECT APRBALVH
  *If The Vendor code and Fiscal year dose not exist in the Vendor History
  *rebalance file insert it. 
  IF !SEEK(M.cVendCode + M.cFisFYear)
    APPEND BLANK
  ENDIF    && End of IF The Vendor code and Fiscal year dose not exist
  GATHER MEMVAR MEMO
  SELECT FISHD
ENDSCAN    && End of SCAN Loop to scan the Fiscal Calendar file for the current company

SELECT APINVHDR

*If Statment to replace the record pointer in the first record for this
*Vendor in the Invoice Header file
IF SEEK(M.cVendCode)
  *SCAN Loop To scan the Vendor invoices in the Invoice Header file
  *with status <> Void
  SCAN REST;
      WHILE cVendCode = M.cVendCode;
        FOR cInvStat <> 'V'
    lnDiscOffr = lfInvInfo(APINVHDR.cInvNo , @lnInvAmt , @lnDiscTakn , lnDiscOffr , @lnAdjtAmt  , @lnAmtPaid , @lnPurchAmt)  
    
    *If Not af advance payment, update cumulative purchases
    IF APINVHDR.cInvStat <> 'A'
      SELECT APRBALV
      REPLACE nVenCPur WITH nVenCPur + lnInvAmt

      *Update Vendor History file for APINVHDR.cFisfYear + APINVHDR.cFspPrdId
      SELECT APRBALVH
      
      *If Statment to replace the record pointer in the correct record for
      *this Vendor and Fiscal Year in the Vendor History file
      IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
        lcPeriod = ALLTRIM(STR(VAL(APINVHDR.cFspPrdId)))
        REPLACE nVnhPurch        WITH nVnhPurch + lnPurchAmt ,;
                nVnhPur&lcPeriod WITH nVnhPur&lcPeriod + lnPurchAmt 
      ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
    ELSE       && APINVHDR.cInvStat <> 'A'
      * Calculate Open debit 
      lnInvAmnt = 0       
      lnInvPaid = 0 
      lnInvDisTk= 0 
      lnInvAdj  = 0 
      lcExSin2   = ''
      lcExSin1   = gfGetExSin(@lcExSin2, APINVHDR.cCurrCode ) 
      lnInvAmnt = lnInvAmnt + ROUND(APINVHDR.nInvAmnt   &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
      lnInvPaid = lnInvPaid + ROUND(APINVHDR.nInvPaid   &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)
      lnInvDisTk= lnInvDisTk+ ROUND(APINVHDR.nInvDisTk  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)      
      lnInvAdj  = lnInvAdj  + ROUND(APINVHDR.nInvAdj    &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)            
    ENDIF    && End of IF Not an advance payment, update cumulative purchases
  
    *Update vendor balances if the invoice is not a credit card payment
    *If Not credit card payment
    IF APINVHDR.cVenPMeth <> 'C'       
      *Update Vendor file
      SELECT APRBALV
      REPLACE nVenBal WITH nVenBal + lnInvAmt - lnAmtPaid;
                                   - lnDiscTakn - lnAdjtAmt 

      *Update Vendor History file
      SELECT APRBALVH
      *If Statment to replace the record pointer in the correct record for
      *this Vendor and Fiscal Year in the Vendor History file
      IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
        REPLACE nVnhDisOf  WITH nVnhDisOf +  lnDiscOffr ,;
                nVnhDisTkn WITH nVnhDisTkn + lnDiscTakn ,;
                nVnhAdj    WITH nVnhAdj + lnAdjtAmt
      ENDIF    && SEEK(M.cVendCode + APINVHDR.cFisfYear)
      
      *Update payments from APDIST (including advance payments)
      SELECT APDIST
      =SEEK(APINVHDR.cInvNo + M.cVendCode)
      SCAN REST;
          WHILE cInvNo + cVendCode = APINVHDR.cInvNo + M.cVendCode ;
            FOR cAPdStat<> 'V'
        *If the record is a payment record (cApdActId = 'C') 
        IF cApdActId = 'C'
          *Update Vendor file
          SELECT APRBALV
          REPLACE nVenCPay WITH nVenCPay -  APDIST.nEqvAmnt 
          *Update Vendor History file for APDIST.cFisfYear + APDIST.cFspPrdId
          SELECT APRBALVH
          *If Statment to replace the record pointer in the correct record
          *for this Vendor and Fiscal Year in the Vendor History file
          IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
            lcPeriod = ALLTRIM(STR(VAL(APDIST.cFspPrdId)))
            lcField = IIF(APDIST.cApdTrTyp = 'P' , 'nVnhPChkP' ,;
                      IIF(APDIST.cApdTrTyp = 'M' , 'nVnhMChkP' ,;
                      IIF(APDIST.cApdTrTyp = 'N' , 'nVnhNChkP' ,;
                          'nVnhCashP' )))
            REPLACE nVnhTotPa        WITH nVnhTotPa - APDIST.nEqvAmnt  ,;
                    &lcField         WITH &lcField  - APDIST.nEqvAmnt ,;
                    nVnhPay&lcPeriod WITH nVnhPay&lcPeriod -  APDIST.nEqvAmnt 
                    
          ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
        ENDIF    && End IF cApdActId = 'C'
        
        *If the record is an application record (applied bdebit memo)
        IF APDIST.cApdTrTyp = 'A' .AND. cApdActID = 'A' .AND. nApdAmnt < 0
          
          *Update Vendor History file for APDIST.cFisfYear
          SELECT APRBALVH
          *If Statment to replace the record pointer in the correct record
          *for this Vendor and Fiscal Year in the Vendor History file
          IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
            REPLACE nVnhDMApP WITH nVnhDMApP + - APDIST.nEqvAmnt ,;
                    nVnHAdj   WITH nVnHAdj   +  lnAdjtAmt 
          ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
        ENDIF    && End IF APDIST.cApdTrTyp = 'A' ,,,
        
        SELECT APDIST
      ENDSCAN    && End of SCAN Loop To scan the Vendor invoices lines in the Dist. file with status <> Void
    
    ELSE      && IF Credit card payment, update payments
    
      *Update Vendor file
      SELECT APRBALV
      REPLACE nVenCPay WITH nVenCPay + lnInvAmt 
  
      
      *Update Vendor History file
      SELECT APRBALVH
      
      *If Statment to replace the record pointer in the correct record
      *for this Vendor and Fiscal Year in the Vendor History file
      IF SEEK(M.cVendCode + APINVHDR.cFisfYear)
        lcPeriod = ALLTRIM(STR(VAL(APINVHDR.cFspPrdId)))
        REPLACE nVnhTotPa        WITH nVnhTotPa + lnInvAmt ,;
                nVnhCCPay        WITH nVnhCCPay + lnInvAmt ,;
                nVnhPay&lcPeriod WITH nVnhPay&lcPeriod + lnInvAmt
      ENDIF    && End of IF Statment to replace the record pointer in the correct record for this Vendor and Fiscal Year in the Vendor History file
    ENDIF    && End of IF Not credit card payment
    
    SELECT APINVHDR
  ENDSCAN    && End of SCAN Loop To scan the Vendor invoices in the Invoice Header file
ENDIF    && End of IF Statment to replace the record pointer in the first record for this Vendor in the Invoice Header file

** Update OpenDebit value in Vendor Master
SELECT APRBALV      
IF SEEK(M.cVendCode)
   REPLACE nVenOpnDR WITH -lnInvAmnt  + lnInvPaid + ;
                          lnInvDisTk + lnInvAdj  
  IF ApVendor.nVenOpnDr <> nVenOpnDr AND !llUpdt
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = "Company" + lcComp_ID +": Vendor ("+ ALLTRIM(M.cVendCode) + ") has wrong open debit amount ."
    laRebMsg[3] = " "
    *E301516,1 (Begin) If called from Auto balance End process
    IF llAutoBal
      laRebMsg[3] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
    ENDIF
    *E301516,1 (End)
    =lfVryRport()
  ENDIF

  IF ApVendor.nVenBal <> nVenBal AND !llUpdt
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = "Company" + lcComp_ID +": Vendor ("+ ALLTRIM(M.cVendCode) + ") has wrong current balance."
    laRebMsg[3] = " "
    *E301516,1 (Begin) If called from Auto balance End process
    IF llAutoBal
      laRebMsg[3] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
    ENDIF
    *E301516,1 (End)
    =lfVryRport()
  ENDIF

ENDIF  

* Initialize OpenDebit variables
lnInvAmnt = 0       
lnInvPaid = 0 
lnInvDisTk= 0 
lnInvAdj  = 0 

SELECT APINVHDR
*--- End of lfRbalVend()




*!*************************************************************
*! Name      : lfStupItm2
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : Additional AP setup that needs to process AP rebalance
*! Purpose   : of item  (lcRpItem2)
*!*************************************************************
*! Example   : = lfStupItm2()
*!*************************************************************
FUNCTION  lfStupItm2

PRIVATE lcCurrArea
lcCurrArea = SELECT(0)

*lcApRBalv  = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Beginning batches to be Reposted
*lcApVenHst = gfTempName()        && Varible to hold a Temp. name for the file that will hold the Normal batches to be Reposted
llVerify = .T.
DECLARE laStructur[1,4]
*If The Vendor rebalance file is not opened
IF !USED('APRBALV')
  SELECT APVENDOR
  IF !FILE(lcFilePath + 'APRBALV.DBF' )
    =AFIELDS(laStructur)
    CREATE TABLE (lcFilePath + 'APRBALV' ) FROM ARRAY laStructur
    INDEX ON cVendCode TAG VENCODE
    USE 
    llVerify = .F.
  ENDIF
  SELECT 0
  USE (lcFilePath + 'APRBALV' ) 
ENDIF    && End of IF The Vendor rebalance file is not opened

*If The Vendor History rebalance file is not opened
IF !USED('APRBALVH')
  SELECT APVENHST
  IF !FILE(lcFilePath + 'APRBALVH.DBF' ) 
    =AFIELDS(laStructur)
    CREATE TABLE (lcFilePath + 'APRBALVH' ) FROM ARRAY laStructur
    INDEX ON cVendCode + cFisFYear TAG VENDYEAR
    USE
    llVerify = .F.
  ENDIF
  SELECT 0 
  USE (lcFilePath + 'APRBALVH')      && ApVenHst
ENDIF    && End of IF The Vendor History rebalance file is not opened
IF lcTempHdr.cUpdVryIgn='U' AND !llVerify 
  llUpdt = .T.
  =lfVryItem2()
ENDIF
SELECT (lcCurrArea)
*-- End of lfStupItm2()

*!*************************************************************
*! Name      : lfValdItm2
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : Check whether all mandatory factors that be used in
*!             AP are defined or not. 
*!*************************************************************
*! Example   : =lfValdItm2()
*!*************************************************************
FUNCTION  lfValdItm2
RETURN .T.
*--- End of lfValdItm2() 

*!*************************************************************
*! Name      : lfUpdItem2
*! Developer : Amin Khodary Amin (AKA)
*! Date      : 08/23/1999
*! Purpose   : Update Fuction for A/P.
*!*************************************************************
*! Example   : =lfUpdItem2()
*!*************************************************************

FUNCTION  lfUpdItem2
PRIVATE llNotUpDat , lcVendor , lnCount

*Set the Order Tags for all needed files
SET ORDER TO TAG VENCODE IN APRBALV
SET ORDER TO TAG VENDYEAR IN APRBALVH
SET ORDER TO TAG VENCODE IN APVENDOR
SET ORDER TO TAG VENDYEAR IN APVENHST
SET ORDER TO TAG VENDINV IN APINVHDR
SET ORDER TO TAG INVVEND IN APDIST


*E301258,1 AKA Start
*E301258,1 *** After having each company it's own chart of account and data file,
*E301258,1 *** so it's important to change the system directory to be data directory.
*E301258,1 *** Note :  Files in version 2.6 changed it's name in version 2.7
*E301258,1 ***         Old File Name ( Version 2.6)      New File Name (Version 2.7)
*E301258,1 ***         ..\sysfiles\SYCFISHD              ..\gcDataDir\FISHD
*E301258,1 ***         ..\sysfiles\SYCFSPRD              ..\gcDataDir\FSPRD

*COUNT FOR cComp_Id + cFisFYear = gcAct_Comp TO lnFisYerNo    && Variable to hold the number of fiscal years for the current company
*** To get the number of records in calender year table withoud deleted records
SELECT FISHD
COUNT TO lnFisYerNo FOR !DELETED()  &&  Get no. of calender years

llNotUpDat = .F.                             && Flag to know if one or more Vendors was not updated
SELECT APRBALV
lnTotal = RECCOUNT() * lnFisYerNo            && These variable is for the Thermometer
lnCurrent = 0                                && These variable is for the Thermometer

*SCAN Loop to scan the Vendor rebalance file
SCAN
  *If The Vendor time stamp is not changed
  IF SEEK(cVendCode , 'APVENDOR') .AND. ;
     (dAdd_Date = APVENDOR.dAdd_Date .AND. cAdd_Time = APVENDOR.cAdd_Time)

    SELECT APVENHST
    
      
      *SCAN Loop to scan the Vendor records in the Vendor History file
      SCAN FOR cVendCode + cFisFYear = APVENDOR.cVendCode
      
        BLANK
        DELETE
      ENDSCAN    && End of SCAN Loop to scan the Vendor records in the Vendor History file
   
    SET DELETED OFF
    
    SELECT FISHD 
    GO TOP
    
    SCAN FOR !DELETED()
      
      SELECT APRBALVH
      *If There is a record for this Fiscal Year and Vendor in the
      *Vendor History rebalance file
      IF SEEK(APVENDOR.cVendCode + FISHD.cFisFYear)
        SCATTER MEMVAR MEMO
      ELSE    && Else [IF There is no records for this Fiscal Year and Vendor]
        SCATTER MEMVAR MEMO BLANK
        M.cVendCode = APVENDOR.cVendCode
        M.cFisFYear = FISHD.cFisFYear
      ENDIF    && End of IF There is a record for this Fiscal Year and Vendor
      
      SELECT APVENHST
      
      *If There is a deleted record in the Vendor History file
      IF SEEK(SPACE(12))
        RECALL
      ELSE    && Else [IF There is no deleted record in the Vendor History file]
        APPEND BLANK
      ENDIF    && End of IF There is a deleted record in the Vendor History file
      
      GATHER MEMVAR MEMO
      =gfAdd_Info('APVENHST')
      
      lnCurrent = lnCurrent  + 1
      =gfThermo(lnTotal , lnCurrent , 'Updating balances...' ,;
                'Vendor : ' + ALLTRIM(APRBALV.cVendCode))
      
      SELECT FISHD
      
    ENDSCAN    && End of SCAN Loop to scan the Fiscal calendar for this company
    
    SET DELETED ON
    
    SELECT APRBALV
    SCATTER MEMVAR MEMO
    SELECT APVENDOR
    GATHER MEMVAR MEMO
    =gfAdd_Info('APVENDOR')
    SELECT APRBALV
    DELETE
  ELSE    && Else [IF The Vendor time stamp was changed]
    llNotUpDat = .T.
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = "Company" + lcComp_ID +": "+ ALLTRIM(cVendCode) + " information has changed since rebalancing. Balances are not updated."
    laRebMsg[3] = " "
    *E301516,1 (Begin) If called from Auto balance End process
    IF llAutoBal
      laRebMsg[3] = "END PROCESSING       "+DTOC(DATE())+SPACE(5)+TIME()
    ENDIF
    *E301516,1 (End)
    =lfVryRport()

    lcVendor = ALLTRIM(APVENDOR.cVendCode)          && Variable to hold the current Vendor code
  ENDIF    && End of IF The Vendor time stamp is not changed
  SELECT APRBALV
ENDSCAN    && End of SCAN Loop to scan the Vendor rebalance file

*If The thermometer is not finished
IF lnCurrent < lnTotal
  
  *FOR Loop to finish the thermometer
  FOR lnCount = lnCurrent TO lnTotal
    =gfThermo(lnTotal , lnCount , 'Updating balances...')
  ENDFOR    && End of FOR Loop to finish the thermometer
ENDIF    && End of IF The thermometer is not finished

*IF llNotUpDat
*  =gfModalGen('QRM04191B00000' , 'Dialog' , IIF(RECCOUNT('APRBALV') > 1 ,;
*              'One or more vendor(s)' , 'Vendor ' + lcVendor))
*ENDIF    && End of IF There is one or more Vendors that was not updated
*E301258,1 AKA End


=lfErsVBalF()

*!*************************************************************
*! Name      : lfErsVBalF
*! Developer : Haytham El_Sheltawi
*! Date      : 02/11/1998
*! Purpose   : Function to Erases the Vendor rebalance file
*!             and the Vendor History rebalance file
*!*************************************************************
*! Called from : lfvRBalnce()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfErsVBalF


*If The Vendor rebalance file is open
IF USED('APRBALV')
  USE IN APRBALV
ENDIF    && End of IF The Vendor rebalance file is open

*If The APRBALV.DBF file exist
IF FILE(lcFilePath + 'APRBALV.DBF')
  ERASE (lcFilePath + 'APRBALV.DBF')
ENDIF    && End of IF The APRBALV.DBF file exist

*If The APRBALV.FPT file exist
IF FILE(lcFilePath + 'APRBALV.FPT')
  ERASE (lcFilePath + 'APRBALV.FPT')
ENDIF    && End of IF The APRBALV.FPT file exist

*If The APRBALV.CDX file exist
IF FILE(lcFilePath + 'APRBALV.CDX')
  ERASE (lcFilePath + 'APRBALV.CDX')
ENDIF    && End of IF The APRBALV.CDX file exist


*If The Vendor History rebalance file is open
IF USED('APRBALVH')
  USE IN APRBALVH
ENDIF    && End of IF The Vendor History rebalance file is open

*If The APRBALVH.DBF file exist
IF FILE(lcFilePath + 'APRBALVH.DBF')
  ERASE (lcFilePath + 'APRBALVH.DBF')
ENDIF    && End of IF The APRBALVH.DBF file exist

*If The APRBALVH.FPT file exist
IF FILE(lcFilePath + 'APRBALVH.FPT')
  ERASE (lcFilePath + 'APRBALVH.FPT')
ENDIF    && End of IF The APRBALVH.FPT file exist

*If The APRBALVH.CDX file exist
IF FILE(lcFilePath + 'APRBALVH.CDX')
  ERASE (lcFilePath + 'APRBALVH.CDX')
ENDIF    && End of IF The APRBALVH.CDX file exist


*!*************************************************************
*! Name      : lfInvInfo
*! Developer : Amin Khodary 
*! Date      : 08/23/1999
*! Purpose   : Function to calculate and return the values of A/P invoice
*!             amount , total discount taken , total adj. amount , discount offer
*!             as well as the amount paid from invoice distribution file.
*!*************************************************************
*! Called from : lfRbalVend()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1 ) lcInvNo 
*!                     2 ) lnInvAmt   
*!					   3 ) lnDiscTakn 
*!					   4 ) lnDiscOffr 
*!					   5 ) lnAdjtAmt  
*!                     6 ) lnAmtPaid
*!*************************************************************
*! Return      : 1 ) lnInvAmt holds invoice amount. 
*!               2 ) lnDiscTakn holds total discount taken.
*!               3 ) lnDiscOffr holds discount offer amount.
*!               4 ) lnAdjtAmt holds total adj. amount.  
*!               5 ) lnAmtPaid holds total paid amount. 
*!               6 ) lnPurchAmt holds purchase amount per invoiuce. 
*!*************************************************************
FUNCTION lfInvInfo
PARAMETERS  lcInvNo , lnInvAmt , lnDiscTakn , lnDiscOffr , lnAdjtAmt  , lnAmtPaid  , lnPurchAmt 

*Save the current area
lcCurArea  = SELECT()          && Varible to save the number of the current work area

* Select the A/P invoice distribution
SELECT APDIST

* Define private variable to save the current record no. of invoice distribution
PRIVATE  lnSaveRec
lnSaveRec = RECNO('APDIST')

* Initialize variables 
lnInvAmt   = 0                  
lnDiscTakn = 0                  
lnDiscOffr = 0                  
lnAdjtAmt  = 0                  
lnAmtPaid  = 0 
lnPurchAmt  = 0 
*If condition not found set the record pointer to the saved record no. then retrun
IF !SEEK( lcInvNo + M.cVendCode  , 'APDIST') 
  IF BETWEEN( lnSaveRec  , 1, RECCOUNT('APDIST')  ) 
    GOTO (lnSaveRec) IN APDIST
  ENDIF
  SELECT (lcCurArea)
  RETURN lnDiscOffr 
ENDIF 
 
* Loop the required records and calcluate the values.
SCAN REST WHILE APDIST.cVendCode + APDIST.cInvNo = M.cVendCode + lcInvNo ;
          FOR ( !DELETED() AND  APDIST.cApdStat <> 'V' ) 
  
  * Get equivelent value of A/P invoice amount or debit memo amount. 
  * Note: It must only one record per invoice.  
  IF APDIST.cApDtrTyp = 'I' AND APDIST.cApdActId = 'A'  
    lnInvAmt   = - APDIST.nEqvAmnt 
    lnPurchAmt = - APDIST.nEqvAmnt 
  ENDIF 

  * Notes
  * APDIST.cApDtrTyp contains 'A' means debit memo
  * APDIST.cApDtrTyp contains 'H' means cash payments 
  * APDIST.cApDtrTyp contains 'M' means manuals checks payments 
  * APDIST.cApDtrTyp contains 'N' means non checks payments 
  * APDIST.cApDtrTyp contains 'P' means printed checks
  
  * Get and sum the equivelent value of paid amount 
  IF APDIST.cApDtrTyp $ 'HMNP' AND APDIST.cApdActId = 'C'
    lnAmtPaid = lnAmtPaid -  APDIST.nEqvAmnt 
  ENDIF 

  * Get and sum the equivelent value of discount taken 
  IF APDIST.cApDtrTyp $ 'AHMNP' AND APDIST.cApdActId = 'S'
    lnDiscTakn = lnDiscTakn - APDIST.nEqvAmnt 
  ENDIF 

  
  * Get and sum the equivelent value of adj. amount.
  IF APDIST.cApDtrTyp $ 'HMNP' AND APDIST.cApdActId = 'J'
    lnAdjtAmt = lnAdjtAmt - APDIST.nEqvAmnt
  ENDIF
  
ENDSCAN  && End of Loop the required records and calcluate the values.       

IF APINVHDR.nInvAmnt > 0 && Payable Invoice
  SUM IIF(cApdActId ="A" and nEqvAmnt<0,-nEqvAmnt,0) , IIF ( cApdActId ="S",-nEqvAmnt,0) to lnp,lndis;
  FOR cApdTrTyp+cBnkCode+cChkAcct+cApdRef+cInvNo+cApdActId = "A"+SPACE(20)+PADR("lcInvNo",10)
  lnAmtPaid  = lnAmtPaid   + lnp
  lnDiscTakn = lnDiscTakn  + lndis
ENDIF  

* Get and calculate the value of discount offer from invoice header file.
lcExSin2   = ''
lcExSin1   = gfGetExSin(@lcExSin2, APINVHDR.cCurrCode ) 
lnDiscOffr = ROUND(APINVHDR.nInvDisOf  &lcExSin1 APINVHDR.nExRate &lcExSin2 APINVHDR.nCurrUnit ,2)


* Back to the initial record
SELECT APDIST
IF BETWEEN(lnSaveRec  , 1 , RECCOUNT() ) 
  GOTO (lnSaveRec) IN APDIST
ENDIF  

* Back to the initial file and record no.
SELECT (lcCurArea)
RETURN lnDiscOffr 
*--- End of lfInvInfo()


*!*************************************************************
*! Name      : lfvInvNo
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 10/18/1999
*! Purpose   : Valid function for the invoice number.
*E301337,1 ASH 10/18/1999
*!*************************************************************
*! Example   : = lfvInvNo()
*!*************************************************************

FUNCTION lfvInvNo
PRIVATE lcObjName , lcObjVal , laRetVal , lcInvHdTag , lcCstmrTag
lcObjName  = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal   = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
IF !USED('INVHDR')
  =gfOpenFile(lcFilePath+'INVHDR', 'INVHDR' ,'SH')
ENDIF
IF !USED('CUSTOMER')
  =gfOpenFile(lcFilePath+'CUSTOMER', 'CUSTOMER' ,'SH')
ENDIF

lcInvHdTag = ORDER('INVHDR')
lcCstmrTag = ORDER('CUSTOMER')
SET ORDER TO TAG INVHDR IN INVHDR
SET ORDER TO TAG CUSTOMER IN CUSTOMER

*-- if the user want to browse or if the account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'INVHDR'))
  lcBrFields = "Invoice :R :H= 'Invoice' , " +;
               "InvDate :R :H= 'Date' , " +;
               "Account :R :H= 'Account' , " +;
               "Order   :R :H= 'Order' , " +;
               "CustPO  :R :H= 'Reference' , " +;
               "CUSTOMER.BTName :R :H= 'Bill to' , " +;
               "Rep1    :R :H= 'Sales Rep.' , " +;
               "Ship    :R :H= 'Pieces' , " +;
               "ShipAmt :R :H= 'Merchandise'"
  lcFile_Ttl = 'Receivable invoices' 
  SELECT INVHDR
  SET RELATION TO 'M' + Account INTO CUSTOMER ADDITIVE
  DECLARE laRetVal[1]
  IF gfBrows('' , 'Invoice' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE    && else no value is returned
    &lcObjName = laOldVal
  ENDIF    && end of if
  SET RELATION OFF INTO CUSTOMER
ENDIF    && end of if
*-- if the INVHDR file did not have an active index
IF EMPTY(lcInvHdTag)
  SET ORDER TO 0 IN INVHDR
ELSE    && Else
  SET ORDER TO TAG (lcInvHdTag) IN INVHDR
ENDIF    && End of IF
*-- if the CUSTOMER file did not have an active index
IF EMPTY(lcCstmrTag)
  SET ORDER TO 0 IN CUSTOMER
ELSE    && Else
  SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
ENDIF    && End of IF

*B605667,1 SSE Commented out. [Begin]
*FUNCTION lfwOldVal
*laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*                                  && this is a global solution for any object type.
*B605667,1 SSE Commented out. [End]
*!*************************************************************
*! Name      : lfvOrdNo
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 10/18/1999
*! Purpose   : Valid function for the order number.
*E301337,1 ASH 10/18/1999
*!*************************************************************
*! Example   : = lfvOrdNo()
*!*************************************************************

FUNCTION lfvOrdNo
PRIVATE lcObjName , lcObjVal , laRetVal , lcInvHdTag 
lcObjName  = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal   = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
IF !USED('ORDHDR')
  =gfOpenFile(lcFilePath+'ORDHDR', 'ORDHDR' ,'SH')
ENDIF

lcOrdHdTag = ORDER('ORDHDR')
SET ORDER TO TAG ORDHDR IN ORDHDR

*-- if the user want to browse or if the account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('O'+lcObjVal , 'ORDHDR'))
  lcBrFields = "ORDER     :R :H= 'Order #' , "   +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "ENTERED   :R :H= 'Entered Date',"+;
               "SEASON    :R :H= 'Season' ,"     +;
               "cDIVISION :R :H= 'Division' ,"   +;
               "CTERMCODE :R :H= 'Terms' ,"      +;
               "SHIPVIA   :R :H= 'ShipVia' ,"    +;
               "STATUS    :R :H= 'Status ' ,"    +; 
               "OPEN      :R :H= 'Open Amt. ',"  +; 
               "BULK      :R :H= 'Bulk' "

  lcFile_Ttl = 'Orders'
  SELECT ORDHDR
  DECLARE laRetVal[1]
  IF gfBrows('FOR cOrdType="O"' , 'Order' , 'laRetVal')
    &lcObjName = laRetVal[1]
  ELSE    && else no value is returned
    &lcObjName = laOldVal
  ENDIF    && end of if
ENDIF    && end of if
*-- if ORDHDR file did not have an active index
IF EMPTY(lcOrdHdTag)
  SET ORDER TO 0 IN ORDHDR
ELSE    && Else
  SET ORDER TO TAG (lcOrdHdTag) IN ORDHDR
ENDIF    && End of IF


*!*************************************************************
*! Name      : lfvCtNo
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 10/18/1999
*! Purpose   : Valid function for the C/T number.
*E301337,1 ASH 10/18/1999
*!*************************************************************
*! Example   : = lfvCtNo()
*!*************************************************************

FUNCTION lfvCtNo
PRIVATE lnAlias, lcObjVal, lcObjName  

lnAlias   = SELECT(0)
lcObjName = SYS(18)
lcObjVal  = EVALUATE(SYS(18))      
IF !USED('CutTktH')
  =gfOpenFile(lcFilePath+'CutTktH', 'CutTktH' ,'SH')
ENDIF

SELECT CutTktH
IF '?' $ lcObjVal  OR (!EMPTY(lcObjVal) AND !SEEK(lcObjVal,"CutTktH"))
  xCutTkt   = lcObjVal
  llNoThing = CutBrow(xCutTkt)
  IF EMPTY(xCutTkt)
    &lcObjName = laOldVal
    _CUROBJ    = _CUROBJ
    RETURN
  ELSE
    &lcObjName = xCutTkt
  ENDIF
ENDIF
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvPONO
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 10/18/1999
*! Purpose   : Valid function for the P/O number.
*E301337,1 ASH 10/18/1999
*!*************************************************************
*! Example   : = lfvPONO()
*!*************************************************************

FUNCTION lfvPONO

lnAlias   = SELECT(0)
lcObjName = SYS(18)
lcObjVal  = EVALUATE(SYS(18))      
IF !USED('POSHDR')
  =gfOpenFile(lcFilePath+'POSHDR', 'POSHDR' ,'SH')
ENDIF

SELECT POSHDR
IF '?' $ lcObjVal  OR (!EMPTY(lcObjVal) AND !SEEK('P'+lcObjVal,"POSHDR"))
  lcPoNo = lcObjVal
  DO POSBrow WITH lcPoNo,"",'P'
  &lcObjName= lcPONo
ENDIF



*!*************************************************************
*! Name      : lfStpHdDtl
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 10/18/1999
*! Purpose   : Setup function for the Header/Detail rebalance.
*E301337,1 ASH 10/18/1999
*!*************************************************************
*! Example   : = lfStpHdDtl()
*!*************************************************************

FUNCTION lfStpHdDtl
PRIVATE lcCurrArea
laFChck = ''

DO CASE
  CASE UPPER(ALLTRIM(lcTempHdr.cTempName))='LCARINV'
    lcType       = ''
    lcKey        = 'Invoice'
    laFChck[1,1] = 'Ship'
    laFChck[1,2] = 'TotQty'
    laFChck[2,1] = 'ShipAmt'
    laFChck[2,2] = 'Price'
    lcLow        = SUBSTR(laFXFLT[1,6],1,6)
    lcHigh       = SUBSTR(laFXFLT[1,6],8,6)
  CASE UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSOORD'
    lcType       = 'cOrdType'
    lcKey        = 'Order'
    laFChck[1,1] = 'Open'
    laFChck[1,2] = 'TotQty'
    laFChck[2,1] = 'OpenAmt'
    laFChck[2,2] = 'Price'

    *E301387,1 ASH 03/22/2000 (Begin) Add the book fields to be rebalanced.
    laFChck[1,3] = 'Book'
    laFChck[1,4] = 'TotBook'
    laFChck[2,3] = 'BookAmt'
    laFChck[2,4] = 'Price'
    *E301387,1 ASH 03/22/2000 (End)
    
    *B604500,1 Add the Cancel Qty and Amount to be rebalanced. [Begin]
    laFChck[1,5] = 'Cancel'
    laFChck[1,6] = 'TotQty'
    laFChck[2,5] = 'CancelAmt'
    laFChck[2,6] = 'Price'    
    *B604500,1 Add the Cancel Qty and Amount to be rebalanced. [End]

    lcLow        = SUBSTR(laFXFLT[2,6],1,6)
    lcHigh       = SUBSTR(laFXFLT[2,6],8,6)
  CASE UPPER(ALLTRIM(lcTempHdr.cTempName))='LCMFCT'
    lcType       = ''
    lcKey        = 'CutTkt'
    laFChck[1,1] = 'Pcs_Bud'
    laFChck[1,2] = 'TotQty'
    laFChck[2,1] = 'Pcs_Rec'
    laFChck[2,2] = 'TotQty'
    lcLow        = SUBSTR(laFXFLT[3,6],1,6)
    lcHigh       = SUBSTR(laFXFLT[3,6],8,6)
  CASE UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSPO'
    lcType       = 'cStyType'
    lcKey        = 'PO'
    laFChck[1,1] = 'nStyOrder'
    laFChck[1,2] = 'TotQty'
    laFChck[2,1] = 'Receive'
    laFChck[2,2] = 'TotQty'
    lcLow        = SUBSTR(laFXFLT[4,6],1,6)
    lcHigh       = SUBSTR(laFXFLT[4,6],8,6)
    
ENDCASE

IF lcTempHdr.cUpdVryIgn='U' AND !llUpdt
  RETURN
ENDIF
lcCurrArea = SELECT(0)
IF FILE(lcFilePath + ALLTRIM(lcTempHdr.cTempName)+'.DBF') 
  USE (lcFilePath + ALLTRIM(lcTempHdr.cTempName)) IN 0 EXCL
  SELECT ALLTRIM(lcTempHdr.cTempName)
  SET ORDER TO TAG (lcTempHdr.cTempName)
ELSE
  lcTag = lcTempHdr.cTempName
  lcFlds   = STRTRAN(lcTempHdr.cFileStr,'|',',')
  CREATE TABLE (lcFilePath + ALLTRIM(lcTempHdr.cTempName)) ;
               &lcFlds
ENDIF
IF FILE(lcFilePath + ALLTRIM(lcTempHdr.cTempName)+'.CDX')               
  SET ORDER TO TAG ALLTRIM(lcTempHdr.cTempName)
ELSE
  * Create index on temporary file 	 
  INDEX ON  &lcType+&lcKey TAG &lctag OF (lcFilePath +ALLTRIM(lcTempHdr.cTempName)+'.CDX')
ENDIF

SELECT (lcCurrArea)

*!*************************************************************
*! Name      : lfVldHdDtl
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 10/18/1999
*! Purpose   : Valid function for the Header/Detail rebalance.
*E301337,1 ASH 10/18/1999
*!*************************************************************
*! Example   : = lfVldHdDtl()
*!*************************************************************

FUNCTION lfVldHdDtl
RETURN .T.

*!*************************************************************
*! Name      : lfVryHdDtl
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 10/18/1999
*! Purpose   : Verify function for the Header/Detail rebalance.
*E301337,1 ASH 10/18/1999
*!*************************************************************
*! Example   : = lfVryHdDtl()
*!*************************************************************

FUNCTION lfVryHdDtl

*B604500,1 Add variable to hold the delete setting. [Begin]
PRIVATE lcSetDelet , lnCanQty , lnCanAmt
*B604500,1 Add variable to hold the delete setting. [End]

lnSep     = AT(',',lcTempHdr.cMFiles)
lcHdrFile = SUBSTR(lcTempHdr.cMFiles,1,lnSep-1)
lcDetFile = SUBSTR(ALLTRIM(lcTempHdr.cMFiles),lnSep+1)
SELECT ALLTRIM(lcTempHdr.cTempName)
ZAP

DO CASE 
  CASE EMPTY(lcLow) AND EMPTY(lcHigh)
    lcFilter = '.T.'
  CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcFilter = lcKey+' <= lcHigh'
  CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
    lcFilter = lcKey+' >= lcLow'
  CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcFilter = lcKey+'>= lcLow AND '+lcKey +'<= lcHigh'
ENDCASE
IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSPO' 
  lcFilter = lcFilter + " AND cStyType='P'"
ENDIF
IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSOORD' 
  lcFilter = lcFilter + " AND cOrdType='O'"
  *B803172,1 ASH 04/12/2000 (Begin) Make relation between invline and invhdr to avoid computing void invoices.
  SELECT InvLine
  SET RELATION TO Invoice INTO Invhdr ADDITIVE
  IF !USED('CUSTOMER')
    =gfOpenFile(lcFilePath+'CUSTOMER', 'CUSTOMER' ,'SH')
  ENDIF
  SELECT InvHdr
  SET RELATION TO 'M'+ Invhdr.Account INTO Customer ADDITIVE
  *B803172,1 ASH 04/12/2000 (End)
ENDIF

SELECT &lcHdrFile 
*B606506,1 TMI [START] Allow to scan on canceled orders
*SCAN FOR &lcFilter AND !(Status $ 'SX')
lcCrit = IIF(UPPER(ALLTRIM(lcTempHdr.cTempName))<>'LCSOORD', "!(Status $ 'SX')" ,".T.")
SCAN FOR &lcFilter AND &lcCrit
  *B606506,1 TMI [END  ]

  SELECT &lcDetFile 
  IF EMPTY(lcType)
    lcExpr = &lcHdrFile..&lcKey
  ELSE
    IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSPO' 
      lcExpr = 'P'+&lcHdrFile..&lcKey
    ELSE
      lcExpr = 'O'+&lcHdrFile..&lcKey
    ENDIF
  ENDIF
  =SEEK(lcExpr)
  lnQty = 0
  lnAmt = 0
  
  *B604758,1 KHM 08/09/2001 (Begin) Initializing the following variables
  *B604758,1                in order to hold the open quantities.
  DIMENSION laOpenQty[8]
  STORE 0 TO laOpenQty, lnOpenQty
  *B604758,1 KHM 08/09/2001 (End)
  
  *E301387,1 ASH 03/22/2000 (Begin) Assign 2 new variables if rebalancing other fields than the original amt&qty.[for example SO book fields]
  IF !EMPTY(laFChck[1,3])
    lnQty1 = 0
    lnAmt1 = 0
  ENDIF    
  *E301387,1 ASH 03/22/2000 (End)
  
  *B604500,1 Assign new variable for rebalancing Cancel Qty and Amount in Order Header file. [Begin]
  IF !EMPTY(laFChck[1,5])
    STORE 0 TO lnCanQty , lnCanAmt
  ENDIF  
  *B604500,1 Assign new variable for rebalancing Cancel Qty and Amount in Order Header file. [End]
  
  lnCanPcs = 0
  llCancel = .T.
  *B803172,1 ASH 04/12/2000 (Begin) Compute the ship amt&qty from invline.
  IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSOORD' 
    lnShipQty = 0
    lnShipAmt = 0
    *B803303,1 ASH 05/28/2000 (Begin) Add a new flag to compute the shipped qty&amt from invline in case of single store invoice.
    *B606506,4 ASH 12/31/2002 (Begin) Don't get the shipped qty&amt from the consinvl file, and use the Invline instead.
    *llCons = .F.
    *B803303,1 ASH 05/28/2000 (End)
    *IF Customer.Consol = 'Y' 
    *  IF !USED('ConsInvH')
    *    =gfOpenFile(lcFilePath+'ConsInvH', 'ConsInvH' ,'SH')
    *  ENDIF
    *  IF !USED('ConsInvL')
    *    =gfOpenFile(lcFilePath+'ConsInvL', 'cInvLine' ,'SH')
    *    SET RELATION TO Invoice INTO ConsInvH ADDITIVE
    *  ENDIF
    *  IF SEEK(OrdHdr.Order,'ConsInvL')
    *    *B803303,1 ASH 05/28/2000 (Begin) Change the flag to .T. if the order record found in ConsInvl file. 
    *    llCons = .T.
    *    *B803303,1 ASH 05/28/2000 (End)
    *    SELECT ConsInvL
    *    SCAN WHILE Order = OrdHdr.Order FOR ConsInvH.Status <> 'V'
    *      lnShipQty = lnShipQty + TotQty
    *      lnShipAmt = lnShipAmt + TotQty*Price
    *    ENDSCAN
    *  *B803303,1 ASH 05/28/2000 (Begin) Make the flag false to compute the shipped amt&qty from Invline file.
    *  ELSE
    *    llCons = .F.
    *  ENDIF
    *ENDIF
    *IF !llCons
    *B803303,1 ASH 05/28/2000 (End)
    *B606506,4 ASH 12/31/2002 (End)
    IF SEEK(OrdHdr.Order,'InvLine')
      SELECT InvLine
      SCAN WHILE Order = OrdHdr.Order FOR InvHdr.Status <> 'V'
        lnShipQty = lnShipQty + TotQty
        lnShipAmt = lnShipAmt + TotQty*Price
      ENDSCAN
    ENDIF
    *ENDIF
  ENDIF
  SELECT &lcDetFile  
  *B803172,1 ASH 04/12/2000 (End)
  
  *B604758,1 KHM 08/09/2001 (Begin) Changing the way of scanning in the
  *B604758,1                lcDetFile by spliting the case of PO,MF and 
  *B604758,1                other cases.

  *SCAN WHILE &lcType+&lcKey = lcExpr 
  *  IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCMFCT' OR UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSPO' 
  *    DO CASE
  *      CASE Trancd = '1'
  *        lnQty   = lnQty  + &laFChck[1,2] 
  *      CASE Trancd = '2'
  *        lnAmt   = lnAmt  + &laFChck[2,2] 
  *      CASE Trancd = '4'
  *        llCancel = .F.
  *    ENDCASE
  *    IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCMFCT'
  *      lnCanPcs = IIF(llCancel,&lcHdrFile..Pcs_Can,0)
  *    ELSE
  *      *lnCanPcs = IIF(llCancel,&lcHdrFile..Cancel,0)
  *      lnCanPcs = 0
  *    ENDIF
  *  ELSE
  *    lnQty   = lnQty  + &laFChck[1,2] 
  *    lnAmt   = lnAmt  + &laFChck[1,2] * &laFChck[2,2] 
  *    *E301387,1 ASH 03/22/2000 (Begin) Compute the SO booked amt & qty. 
  *    IF !EMPTY(laFChck[1,3])
  *      lnQty1   = lnQty1  + &laFChck[1,4] 
  *      lnAmt1   = lnAmt1  + &laFChck[1,4] * &laFChck[2,4] 
  *    ENDIF
  *    *E301387,1 ASH 03/22/2000 (End)
  *  ENDIF
  *ENDSCAN
  
  IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCMFCT' OR UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSPO' 
    
    lcWhileExp = IIF(UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSPO', ;
               "&lcType+&lcKey+Style+STR(Lineno,6)= lcExpr+lcTransSty+lcTransLin",;
               "&lcType+&lcKey = lcExpr")
               
    DO WHILE &lcType+&lcKey = lcExpr
      lcTransSty = Style
      lcTransLin = STR(LineNo,6)
      STORE 0 TO laOpenQty
      SCAN REST WHILE &lcWhileExp
        DO CASE
          CASE Trancd = '1'
            lnQty   = lnQty  + &laFChck[1,2] 

            FOR lnCntr = 1 TO 8
              lcCntr = STR(lnCntr,1)
              laOpenQty[lnCntr] = &lcDetFile..Qty&lcCntr
            ENDFOR

          CASE Trancd = '2'
            lnAmt   = lnAmt  + &laFChck[2,2] 

            FOR lnCntr = 1 TO 8
              lcCntr = STR(lnCntr,1)
              laOpenQty[lnCntr] = MAX(laOpenQty[lnCntr] - &lcDetFile..Qty&lcCntr,0)
            ENDFOR

          CASE Trancd = '4'

            FOR lnCntr = 1 TO 8
              lcCntr = STR(lnCntr,1)
              laOpenQty[lnCntr] = MAX(laOpenQty[lnCntr] - &lcDetFile..Qty&lcCntr,0)
            ENDFOR

            llCancel = IIF(UPPER(ALLTRIM(lcTempHdr.cTempName))='LCMFCT',.F.,llCancel)

          CASE UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSPO' AND Trancd = '5'
             FOR lnCntr = 1 TO 8
              lcCntr = STR(lnCntr,1)
              laOpenQty[lnCntr]  = MAX(laOpenQty[lnCntr] - &lcDetFile..Qty&lcCntr,0)
            ENDFOR
            llCancel = .F.
        ENDCASE
        IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCMFCT'
          lnCanPcs = IIF(llCancel,&lcHdrFile..Pcs_Can,0)
        ELSE
          lnCanPcs = 0
        ENDIF      
      ENDSCAN 
      FOR lnCntr = 1 TO 8
        lnOpenQty = lnOpenQty + laOpenQty[lnCntr]
      ENDFOR  
    ENDDO
  ELSE
    *:B605061,1 MHM 10/30/2001 scan for lines [Start]
    SCAN WHILE &lcType+&lcKey = lcExpr 
      *B606506,1 TMI [START] Sum canceled qty if order is canceled
      IF &lcHdrFile..STATUS = 'X' AND !EMPTY(laFChck[1,5])
        lnCanQty = lnCanQty + &laFChck[1,2] 
        lnCanAmt = lnCanAmt + &laFChck[1,2] * &laFChck[2,2] 
      ELSE
        *B606506,1 TMI [END  ] Sum canceled qty if order is canceled
        lnQty   = lnQty  + &laFChck[1,2] 
        lnAmt   = lnAmt  + &laFChck[1,2] * &laFChck[2,2] 
      *B606506,1 TMI [START]
      ENDIF
      *B606506,1 TMI [END  ]
      IF !EMPTY(laFChck[1,3])
        lnQty1   = lnQty1  + &laFChck[1,4] 
        lnAmt1   = lnAmt1  + &laFChck[1,4] * &laFChck[2,4] 
      ENDIF
    ENDSCAN  
    *:B605061,1 MHM  [End]
  ENDIF
  *B604758,1 KHM 08/09/2001 (End)
  
  *E301387,1 ASH 03/22/2000 (Begin) Change the if expression in case of rebalancing SO booked qty.
  *IF lnQty <> &lcHdrFile..&laFChck[1,1]-lnCanPcs  OR lnAmt <> &lcHdrFile..&laFChck[2,1]
  IF !EMPTY(laFChck[1,3])

    *B604500,1 Calculate the Cancelled Qty and Amount. [Begin]
    IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSOORD'
      SELECT OrdCanLn
      IF SEEK(lcExpr)
        SCAN REST WHILE &lcType+&lcKey = lcExpr
          lnCanQty = lnCanQty + TotQty
          
          IF SEEK(cOrdType+Order+STR(LineNo,6),'OrdLine')                    
            lnCanAmt = lnCanAmt + (TotQty*Price)          
          ELSE
            lnQty1 = lnQty1  + TotQty
            
            *-- if price iz zero check for price in ordline file
            IF Price = 0
              lcSetDelet = SET('DELETE')
              SET DELETE OFF
  
              IF SEEK(cOrdType+Order+STR(LineNo,6),'OrdLine')
                lnAmt1   = lnAmt1  + (TotQty*OrdLine.Price)
                lnCanAmt = lnCanAmt + (TotQty*OrdLine.Price)
              ENDIF            
  
              SET DELETE &lcSetDelet  
            ELSE
              lnAmt1   = lnAmt1  + (TotQty*Price)            
              lnCanAmt = lnCanAmt + (TotQty*Price)
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
    *B604500,1 Calculate the Cancelled Qty and Amount. [End]

    *B604500,1 Add the check on Cancel Qty and Amount. [Begin]
    *lcIfExp = "lnQty <> &lcHdrFile..&laFChck[1,1]-lnCanPcs  OR lnAmt <> &lcHdrFile..&laFChck[2,1] ;
    *           OR lnQty1 <> &lcHdrFile..&laFChck[1,3] OR lnAmt1 <> &lcHdrFile..&laFChck[2,3]"
    *B606506,1 TMI [Start]  When tracing I found that the variable lcIfExp does not contains 
    *B606506,1              all these 3 lines , so write each line as a separate string
    *lcIfExp = "lnQty <> &lcHdrFile..&laFChck[1,1]-lnCanPcs  OR lnAmt <> &lcHdrFile..&laFChck[2,1] ;
               OR lnQty1 <> &lcHdrFile..&laFChck[1,3] OR lnAmt1 <> &lcHdrFile..&laFChck[2,3] OR ;
               lnCanQty <> &lcHdrFile..&laFChck[1,5] OR lnCanAmt <> &lcHdrFile..&laFChck[2,5]"
    lcIfExp = "lnQty <> &lcHdrFile..&laFChck[1,1]-lnCanPcs  OR "+;
              "lnAmt <> &lcHdrFile..&laFChck[2,1] OR  "+;
              "lnQty1 <> &lcHdrFile..&laFChck[1,3] OR "+;
              "lnAmt1 <> &lcHdrFile..&laFChck[2,3] OR "+;
              "lnCanQty <> &lcHdrFile..&laFChck[1,5] OR "+;
              "lnCanAmt <> &lcHdrFile..&laFChck[2,5]"
    *B606506,1 TMI [End  ]  
    *B604500,1 Add the check on Cancel Qty and Amount. [End]

  ELSE
    *B603677,1 ASH 06/08/2000 (Begin) Add expression to check the void invoices.
    IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCARINV' AND &lcHdrFile..Status = 'V'
      lcIfExp = "lnQty <> &lcHdrFile..vShip  OR lnAmt <> &lcHdrFile..vShipAmt"
    ELSE
    *B603677,1 ASH 06/08/2000 (End)
      lcIfExp = "lnQty <> &lcHdrFile..&laFChck[1,1]-lnCanPcs  OR lnAmt <> &lcHdrFile..&laFChck[2,1]"
      *B604758,1 KHM 08/09/2001 (Begin) Add the checking of the open Qty in
      *B604758,1            case of PO
      IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSPO'
          lcIfExp = lcIfExp + " OR lnOpenQty <> PosHdr.Open"
      ENDIF    
      *B604758,1 KHM 08/09/2001 (End)
    ENDIF
  ENDIF
  *B803172,1 ASH 04/12/2000 (Begin) Add ship check to the IF condition expr.
  IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSOORD' 
    lcIfExp = lcIfExp + " OR lnShipQty <> &lcHdrFile..Ship OR lnShipAmt <> &lcHdrFile..ShipAmt"
  ENDIF
  *B803172,1 ASH 04/12/2000 (End)
  IF &lcIfExp 
  *E301387,1 ASH 03/22/2000 (End)
    SELECT ALLTRIM(lcTempHdr.cTempName)
    APPEND BLANK
    FOR mCount = 1 TO FCOUNT( )
	  lcFld = FIELD(mCount)
	  REPLACE &lcFld  WITH &lcHdrFile..&lcFld 
    ENDFOR
    REPLACE &laFChck[1,1] WITH lnQty
    REPLACE &laFChck[2,1] WITH lnAmt
    
    *B604758,1 KHM 08/09/2001 (Begin) Update the open quantity in the 
    *B604758,1                temporary file
    IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSPO'
      REPLACE Open WITH lnOpenQty
    ENDIF
    *B604758,1 KHM 08/09/2001 (End)

    *E301387,1 ASH 03/22/2000 (Begin) Replace the SO booked qty&amt.
    *B803191,1 ASH 04/05/2000 (Begin) Fix the bug of 'Unrecognized Phrase/keyword in command'
    *IF !EMPTY(&laFChck[1,3])
    IF !EMPTY(laFChck[1,3])
    *B803191,1 ASH 04/05/2000 (End)
      REPLACE &laFChck[1,3] WITH lnQty1
      REPLACE &laFChck[2,3] WITH lnAmt1
    ENDIF
    *E301387,1 ASH 03/22/2000 (End)
    *B803172,1 ASH 04/12/2000 (Begin) Add the ship qty&amt to the verify temp file.
    IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSOORD' 
      REPLACE Ship    WITH lnShipQty ,;
              ShipAmt WITH lnShipAmt 
              
      *B604500,1 Update the Cancel Qty and Amount. [Begin]
      REPLACE Cancel WITH lnCanQty , CancelAmt WITH lnCanAmt
      *B604500,1 Update the Cancel Qty and Amount. [End]
    ENDIF
    *B803172,1 ASH 04/12/2000 (End)
    IF !llUpdt
      DECLARE laRebMsg[3]
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcComp_ID +": "+ lcKey+' # '+&lcHdrFile..&lcKey+ " has quantity or amount different than its lines."
      laRebMsg[3] = " "
      =lfVryRport()
    ENDIF
  ENDIF
ENDSCAN
*B803191,1 ASH 04/05/2000 (Begin) Remove the relation between invline and invhdr.
IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSOORD' 
  SELECT InvLine
  SET RELATION OFF INTO Invhdr
  IF USED('ConsInvH')
    SELECT ConsInvH
    SET RELATION OFF INTO ConsInvH
  ENDIF
  SELECT InvHdr
  SET RELATION OFF INTO Customer
ENDIF
*B803191,1 ASH 04/05/2000 (End)
SELECT ALLTRIM(lcTempHdr.cTempName)
USE


*!*************************************************************
*! Name      : lfUpdHdDtl
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 10/18/1999
*! Purpose   : Update function for the Header/Detail rebalance.
*E301337,1 ASH 10/18/1999
*!*************************************************************
*! Example   : = lfUpdHdDtl()
*!*************************************************************

FUNCTION lfUpdHdDtl

lnSep     = AT(',',lcTempHdr.cMFiles)
lcHdrFile = SUBSTR(lcTempHdr.cMFiles,1,lnSep-1)
lcDetFile = RIGHT(ALLTRIM(lcTempHdr.cMFiles),lnSep)
IF FILE(lcFilePath + ALLTRIM(lcTempHdr.cTempName) + '.DBF')
  IF !USED(ALLTRIM(lcTempHdr.cTempName))
    USE (lcFilePath + ALLTRIM(lcTempHdr.cTempName)) IN 0 
  ENDIF
  SELECT ALLTRIM(lcTempHdr.cTempName)
  SET ORDER TO TAG (lcTempHdr.cTempName)
  SET RELATION TO &lcType+&lcKey INTO &lcHdrFile  ADDITIVE
  SCAN
    IF DTOC(DADD_DATE)+CADD_TIME <> DTOC(&lcHdrFile..DADD_DATE)+&lcHdrFile..CADD_TIME
      DECLARE laRebMsg[3]
      DO CASE
        CASE UPPER(lcHdrFile) = 'INVHDR'
          lcExpr = 'Invoice #'
        CASE UPPER(lcHdrFile) = 'ORDHDR'
          lcExpr = 'Order #'
        CASE UPPER(lcHdrFile) = 'CUTTKTH'
          lcExpr = 'CutTkt #'
        CASE UPPER(lcHdrFile) = 'POSHDR'
          lcExpr = 'P/O #'
      ENDCASE          
      laRebMsg[1] = " "
      laRebMsg[2] = "Can not update "+lcExpr+&lcHdrFile..&lcKey+ " because its data was changed before updating."
      laRebMsg[3] = " "
      =lfVryRport()
    ELSE
      SELECT &lcHdrFile
      lcTFile = ALLTRIM(lcTempHdr.cTempName)
      REPLACE &laFChck[1,1] WITH &lcTFile..&laFChck[1,1] 
      REPLACE &laFChck[2,1] WITH &lcTFile..&laFChck[2,1] 
      
      *B604758,1 KHM 08/09/2001 (Begin) Updating the open quantity in the
      *B604758,1                master file.
      IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSPO'
        REPLACE Open   WITH &lcTFile..Open
      ENDIF
      *B604758,1 KHM 08/09/2001 (End)

      *E301387,1 ASH 03/22/2000 (Begin) Replace the SO booked qty&amt.
      IF !EMPTY(laFChck[1,3])
        REPLACE &laFChck[1,3] WITH &lcTFile..&laFChck[1,3] 
        REPLACE &laFChck[2,3] WITH &lcTFile..&laFChck[2,3] 
      ENDIF
      *E301387,1 ASH 03/22/2000 (End)
      *B803172,1 ASH 04/12/2000 (Begin) Update the ship qty&amt in the ordhdr file.
      IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCSOORD' 
        REPLACE Ship    WITH &lcTFile..Ship ,;
                ShipAmt WITH &lcTFile..ShipAmt
                
        *B604500,1 Update the Cancel Qty and Amount. [Begin]
        REPLACE Cancel    WITH &lcTFile..Cancel ,;
                CancelAmt WITH &lcTFile..CancelAmt        
        *B604500,1 Update the Cancel Qty and Amount. [End]
        
      ENDIF
      *B803172,1 ASH 04/12/2000 (End)
      *B603677,1 ASH 06/08/2000 (Begin) Added to update the void fields in case of void invoices.
      IF UPPER(ALLTRIM(lcTempHdr.cTempName))='LCARINV' AND &lcHdrFile..Status ='V'
        REPLACE Ship     WITH 0 ,;
                ShipAmt  WITH 0 ,;
                VShip    WITH &lcTFile..&laFChck[1,1] ,;
                VShipAmt WITH &lcTFile..&laFChck[2,1]        
      ENDIF
    *B603677,1 ASH 06/08/2000 (End)
    ENDIF
  ENDSCAN
  SELECT ALLTRIM(lcTempHdr.cTempName)
  USE
  ERASE(lcFilePath + ALLTRIM(lcTempHdr.cTempName)+'.DBF')
  ERASE(lcFilePath + ALLTRIM(lcTempHdr.cTempName)+'.CDX')
  
ELSE
  *=gfModalGen("TRM00366B00000" , "DIALOG") 
  llUpdt = .T.
  =lfStpHdDtl()
  =lfVryHdDtl()
  =lfUpdHdDtl()
  llUpdt = .F.
ENDIF


*!*************************************************************
*! Name      : lfUpdt
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 10/18/1999
*! Purpose   : Update function for the Style & Material rebalance.
*E301337,1 ASH 10/18/1999
*!*************************************************************
*! Example   : = lfUpdt()
*!*************************************************************

FUNCTION lfUpdt
DO lpUpdVr WITH 'U'

*!*************************************************************
*! Name      : lfVerf
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 10/18/1999
*! Purpose   : Verfiy function for the Style & Material rebalance.
*E301337,1 ASH 10/18/1999
*!*************************************************************
*! Example   : = lfVerf()
*!*************************************************************

FUNCTION lfVerf
DO lpUpdVr WITH 'V'

*!*************************************************************
*! Name      : lfVldMat
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 10/18/1999
*! Purpose   : Valid function for the Style & Material rebalance.
*E301337,1 ASH 10/18/1999
*!*************************************************************
*! Example   : = lfVldMat()
*!*************************************************************

FUNCTION lfVldMat
RETURN .T.

*!*************************************************************
*! Name      : lpUpdVr
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 10/18/1999
*! Purpose   : Procedure to verify & update the style&material rebalance.
*! Parameters: lcUp_Vr   && to indicate if it is verify or update.
*E301337,1 ASH 10/18/1999
*!*************************************************************
*! Example   : DO lpUpdVr
*!*************************************************************

PROCEDURE lpUpdVr
PARAMETER lcUp_Vr

lcRebItem = STRTRAN(UPPER(ALLTRIM(lcTempHdr.cItemName)),'LCRPITEM')
STORE 0 TO lnOnOrder, lnWip, lnWo, lnIntrans, lnOrd, lnAlo, lnShp, lnRet, lnRa,lnStock,lnOnHand
DO CASE 
  CASE lcRebItem = '3'
    lnOnHand = 1
    lcFileN = 'lcFabAls.DBF'
  CASE lcRebItem = '4'
    lnOnOrder = 1
    lcFileN = 'lcFabAls.DBF'
  CASE lcRebItem = '5'
    lnStock = 1
    lcFileN = 'lcStyAls.DBF'
  CASE lcRebItem = '6'
    lnIntrans = 1
    lcFileN = 'lcStyAls.DBF'
  CASE lcRebItem = '7'
    lnWo    = 1
    lcFileN = 'lcStyAls.DBF'
  CASE lcRebItem = '8'
    lnWip   = 1
    lcFileN = 'lcStyAls.DBF'
  CASE lcRebItem = '9'
    lnShp   = 1
    lcFileN = 'lcStyAls.DBF'
  CASE lcRebItem = '10'
    lnAlo   = 1
    lcFileN = 'lcStyAls.DBF'
  CASE lcRebItem = '11'
    lnOrd   = 1
    lcFileN = 'lcStyAls.DBF'
  CASE lcRebItem = '12'
    lnRa    = 1
    lcFileN = 'lcStyAls.DBF'
  CASE lcRebItem = '13'
    lnRet   = 1
    lcFileN = 'lcStyAls.DBF'
    
ENDCASE
**** Call the old program with some modifications to mainipulate the verify option befroe updating.

DO lpMainReb WITH lcToReb,lcComp_ID,lcRebItem,lcUp_Vr,lcFilePath,lcCompName,lcCompMods 
*B605038,1 BWA 10/17/2001 Bug the files was not erased after using the varify option.[START]
*IF lcUp_Vr = 'U'
*  DO CASE
*    CASE lcRebItem = '3' OR lcRebItem = '4'
*      ERASE (lcFilePath + 'lcFabAls.DBF')
*      ERASE (lcFilePath + 'lcFDyAls.DBF')
*      ERASE (lcFilePath + 'lcFabAls.CDX')
*      ERASE (lcFilePath + 'lcFDyAls.CDX')
*    OTHERWISE
*      ERASE (lcFilePath +'lcStyAls.DBF')
*      ERASE (lcFilePath +'lcStyAls.CDX')
*      ERASE (lcFilePath +'lcSDyAls.DBF')
*      ERASE (lcFilePath +'lcSDyAls.CDX')
*   ENDCASE
*ENDIF

ERASE (lcFilePath + 'lcFabAls.DBF')
ERASE (lcFilePath + 'lcFDyAls.DBF')
ERASE (lcFilePath + 'lcFabAls.CDX')
ERASE (lcFilePath + 'lcFDyAls.CDX')
ERASE (lcFilePath + 'lcFabAls.FPT')
ERASE (lcFilePath + 'lcFDyAls.FPT')

ERASE (lcFilePath +'lcStyAls.DBF')
ERASE (lcFilePath +'lcSDyAls.DBF')
ERASE (lcFilePath +'lcStyAls.CDX')
ERASE (lcFilePath +'lcSDyAls.CDX')
ERASE (lcFilePath +'lcStyAls.FPT')
ERASE (lcFilePath +'lcSDyAls.FPT')

*B605038,1 BWA 10/17/2001.[END]


*!*************************************************************
*! Name      : lpMainReb
*! Developer : Ashraf Sherif Mohammad (ASH)
*! Date      : 10/18/1999
*! Purpose   : This is the old code of rebalance program done by Badran with some modifications
*! Parameters: lcToReb      && which items want to be rebalanced if called from another program .
*!             lcCompCode   && company code 
*!             lcRebal      && item to be rebalanced.
*!             lcVrUpd      && verify or update.
*!             lcFilePath   && path to open the file from.
*!             lcCompName   && company name
*!             lcCompMods   && modules installed in each company.
*E301337,1 ASH 10/18/1999 
*!*************************************************************
*! Example   : DO lpMainReb
*!*************************************************************

PROCEDURE lpMainReb
PARAMETERS lcToReb,lcCompCode,lcRebal,lcVrUpd,lcFilePath,lcCompName,lcCompMods 

llFinish  = .F.  && .T. if finish rebalnce selected company.
*B802768,1 SSH 09/21/99 New variable for totonhand
lnLocOnHnd = 0
lnLocOnHVl = 0
*B802768,1 SSH (END)
*E301337,1 ASH 10/18/1999 (Begin) Open necessary temp file for each item.
IF TYPE('lcToReb') = 'C' AND !EMPTY(lcToReb)
  lcFabAls    = gfTempName()  && FABRIC alias 
  lcFDyAls    = gfTempName()  && FABDYE alias 
  lcStyAls    = gfTempName()  && STYLE alias 
  lcSDyAls    = gfTempName()  && STYDYE alias 
  lcPoLAls    = gfTempName()  && POSLN  alias       
  lcPoHdAls   = gfTempName()  && POFHDR alias 
  lcPoLnAls   = gfTempName()  && POFLN alias 
  lcMfHdAls   = gfTempName()  && MMFGORDH alias 
  lcMfLnAls   = gfTempName()  && MMFGORDD alias 
  lcMatJlAls  = gfTempName()  && MATINVJL alias 
  lcPoHAls    = gfTempName()  && POSHDR alias 
  lcPoLAls    = gfTempName()  && POSLN  alias 
  lcOrdHAls   = gfTempName()  && ORDHDR alias 
  lcOrdLAls   = gfTempName()  && ORDLINE alias 
  lcWipJAls   = gfTempName()  && WIPADJ alias         
  lcCutHdAls  = gfTempName()  && CUTTKTH alias 
  lcCutLnAls  = gfTempName()  && CUTTKTL alias 
  lcStyJlAls  = gfTempName()  && STYINVJL alias 
  lcInvHAls   = gfTempName()  && INVHDR alias 
  lcInvLAls   = gfTempName()  && INVLINE alias 
  lcRaHdAls   = gfTempName()  && RETAUTH alias 
  lcRaLnAls   = gfTempName()  && RALINE alias 
  lcRetHdAls  = gfTempName()  && RETHDR alias 
  lcRetLnAls  = gfTempName()  && RETLINE alias 
  lcTmpMDyeFl = gftempName()
ELSE
  IF lcRebal = '3' OR lcRebal = '4'
    IF lcVrUpd = 'V'
      IF USED('FABRIC')
        USE IN FABRIC
      ENDIF
      IF USED('FABDYE')
        USE IN FABDYE
      ENDIF
      IF 'MA' $ lcCompMods
        COPY FILE (lcFilePath +'Fabric.DBF') TO (lcFilePath +'lcFabAls.DBF')
        COPY FILE (lcFilePath +'Fabric.CDX') TO (lcFilePath +'lcFabAls.CDX')
        COPY FILE (lcFilePath +'FabDye.DBF') TO (lcFilePath +'lcFDyAls.DBF')
        COPY FILE (lcFilePath +'FabDye.CDX') TO (lcFilePath +'lcFDyAls.CDX')

        *B605038,1 BWA 10/17/2001 Copy the FPT file if found.[START]
        IF FILE(lcFilePath +'Fabric.FPT')
          COPY FILE (lcFilePath +'Fabric.FPT')  TO (lcFilePath +'lcFabAls.FPT')
        ENDIF

        IF FILE(lcFilePath +'FabDye.FPT')
          COPY FILE (lcFilePath +'FabDye.FPT')  TO (lcFilePath +'lcFDyAls.FPT')
        ENDIF
        *B605038,1 BWA 10/17/2001.[END]

        =gfOpenFile(lcFilePath+'FabDye', 'FabDye' ,'SH')
      ELSE
        RETURN
      ENDIF
    ELSE
      lcFabAls   = gfTempName()  && FABRIC alias 
      lcFDyAls   = gfTempName()  && FABDYE alias 
    ENDIF
    lcPoLAls   = gfTempName()  && POSLN  alias       
    lcPoHdAls  = gfTempName()  && POFHDR alias 
    lcPoLnAls  = gfTempName()  && POFLN alias 
    lcMfHdAls  = gfTempName()  && MMFGORDH alias 
    lcMfLnAls  = gfTempName()  && MMFGORDD alias 
    lcMatJlAls = gfTempName()  && MATINVJL alias 
    lcTmpMDyeFl = gftempName()
  ELSE
    IF lcVrUpd = 'V'
      IF USED('STYLE')
        USE IN STYLE
      ENDIF
      IF USED('STYDYE')
        USE IN STYDYE
      ENDIF
      COPY FILE (lcFilePath +'Style.DBF')  TO (lcFilePath +'lcStyAls.DBF')
      COPY FILE (lcFilePath +'Style.CDX')  TO (lcFilePath +'lcStyAls.CDX')
      COPY FILE (lcFilePath +'StyDye.DBF') TO (lcFilePath +'lcSDyAls.DBF')
      COPY FILE (lcFilePath +'StyDye.CDX') TO (lcFilePath +'lcSDyAls.CDX')

      *B605038,1 BWA 10/17/2001 Copy the FPT file if found.[START]
      IF FILE(lcFilePath +'Style.FPT')
        COPY FILE (lcFilePath +'Style.FPT')  TO (lcFilePath +'lcStyAls.FPT')
      ENDIF

      IF FILE(lcFilePath +'StyDye.FPT')
        COPY FILE (lcFilePath +'StyDye.FPT')  TO (lcFilePath +'lcSDyAls.FPT')
      ENDIF
      *B605038,1 BWA 10/17/2001.[END]

      =gfOpenFile(lcFilePath+'StyDye', 'StyDye' ,'SH')
    ELSE
      lcStyAls   = gfTempName()  && STYLE alias 
      lcSDyAls   = gfTempName()  && STYDYE alias 
    ENDIF      
    IF lcRebal = '5' OR lcRebal = '6' OR lcRebal = '7' OR lcRebal = '8' OR lcRebal = '10'  OR lcRebal = '11'
      lcPoHAls   = gfTempName()  && POSHDR alias 
      lcPoLAls   = gfTempName()  && POSLN  alias 
      lcOrdHAls  = gfTempName()  && ORDHDR alias 
      lcOrdLAls  = gfTempName()  && ORDLINE alias 
      lcWipJAls  = gfTempName()  && WIPADJ alias         
      lcCutHdAls = gfTempName()  && CUTTKTH alias 
      lcCutLnAls = gfTempName()  && CUTTKTL alias 
      lcStyJlAls = gfTempName()  && STYINVJL alias 
    ENDIF
    IF lcRebal = '9'
      lcInvHAls = gfTempName()  && INVHDR alias 
      lcInvLAls = gfTempName()  && INVLINE alias 
    ENDIF
    IF lcRebal = '12' OR lcRebal = '13'
      lcRaHdAls  = gfTempName()  && RETAUTH alias 
      lcRaLnAls  = gfTempName()  && RALINE alias 
      lcRetHdAls = gfTempName()  && RETHDR alias 
      lcRetLnAls = gfTempName()  && RETLINE alias 
    ENDIF    
  ENDIF
ENDIF
*E301337,1 ASH 10/18/1999 (End)

llOldRel   = .F.  && Variable to indicate if this ver. of 2.7 contain Ntokal fileds.
lnNPRVSQTY = 0    && Hold Old Qty.
lnNPRVSVAL = 0    && Hold Old Val.

*-- llSysComp : .T. if this program open system company file.

*E301077,27 Open company file after gfsetup and define array [Begin]
*e301077,27 hold company setups used in this screen.
llSysComp = .F.
DECLARE laSetups[3,2]
laSetups[1,1] = 'M_Dyelot'
laSetups[2,1] = 'M_WareHouse'
laSetups[3,1] = 'M_MATDYE'
*E301077,27 Open company file after gfsetup and define array [Begin]

*-- if called from another program.
IF TYPE('lcToReb') = 'C' AND !EMPTY(lcToReb)
  
  *E301077,27 Open syscomp file.
  *-- llSysComp : .T. if this program open company file.
  llSysComp = gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'Ccomp_id','SH')
  
  IF TYPE('lcCompCode') $ 'UL' OR EMPTY(lcCompCode)
    lcCompCode = gcAct_Comp
  ENDIF

  SET ORDER TO TAG Ccomp_id IN SYCCOMP
  *-- if you find passed company in company files.
  IF SEEK(lcCompCode,'SYCCOMP')
    *E301098,1 Hesham (Start)
    *lcCmPath  = ALLTRIM(LOWER(SYCCOMP.cCom_dDir))
    lcCmPath  = gfGetDataDir(ALLTRIM(LOWER(SYCCOMP.cCom_dDir)))
    *E301098,1 Hesham (End)
    lcCmName  = ALLTRIM(SYCCOMP.cCom_Name)  
    lcInstMod = SYCCOMP.mModlSet
    llConvert = .T. && avoiding run screen.  
  ELSE
    RETURN
  ENDIF
ENDIF

*-- llOpnError : Flag to detect if you can lock file.
*-- llDyelot   : Flag to detect that current company use dyelot.
*-- llMultWare : Flag to detect that current company use multi warehouse or not.
STORE .F. TO llOpnError,llDyelot,llMultWare,llFabDye

*-- if come from another program.
IF llConvert

  *E301232,1 Add lnStock to the list [Begin]
  *STORE 0 TO lnWip,lnIntrans,lnWo,lnOrd,lnAlo,lnShp,;
  *           lnRet,lnRa,lnOnOrder,lnOnHand
  STORE 0 TO lnWip,lnIntrans,lnWo,lnOrd,lnAlo,lnShp,;
             lnRet,lnRa,lnOnOrder,lnOnHand,lnStock
  *E301232,1 Add lnStock to the list [End  ]
  
  lcInsModul = ''  && have installed modules.

  *-- Style Work Orders.
  IF ('WO' $ lcToReb) OR ('WI' $ lcToReb)
    lcInsModul = IIF('PO' $ lcInstMod,'PO','')
    lcInsModul = IIF('MF' $ lcInstMod, IIF(EMPTY(lcInsModul),'MF',lcInsModul+',MF'),lcInsModul)

    IF ('WI' $ lcToReb)
      lnWip = 1
    ENDIF

    IF ('WO' $ lcToReb) AND (('PO' $ lcInsModul) OR ('MF' $ lcInsModul))
      lnWo = 1
    ENDIF

  ENDIF  

  *-- Style InTransit.
  IF 'IN' $ lcToReb AND ('PO' $ lcInstMod)
    lcInsModul = IIF(EMPTY(lcInsModul),'PO',;
                 IIF('PO' $ lcInsModul,lcInsModul,lcInsModul+',PO'))
    lnIntrans = 1
  ENDIF  
  
  *-- Return auth.
  IF 'RA' $ lcToReb AND ('RM' $ lcInstMod)
    lcInsModul = IIF(EMPTY(lcInsModul),'RM',lcInsModul+',RM')
    lnRa = 1
  ENDIF  

  IF 'ONO' $ lcToReb AND ('MA' $ lcInstMod)
    lcInsModul = IIF(EMPTY(lcInsModul),'MA',lcInsModul+',MA')
    lnOnOrder = 1
  ENDIF
  
  *-- Material OnHand.
  *--[Coment until check with Omar] IF 'MH' $ lcToReb AND ('MA' $ lcInstMod)
  *--[Coment until check with Omar]   lcInsModul = IIF(EMPTY(lcInsModul),'MA',lcInsModul+',MA')
  *--[Coment until check with Omar]   lnOnHand = 1
  *--[Coment until check with Omar] ENDIF  
  *E301261,1 SSH 06/14/99 Add material module.
  IF 'MH' $ lcToReb AND ('MA' $ lcInstMod)
    lcInsModul = IIF(EMPTY(lcInsModul),'MA',lcInsModul+',MA')
    lnOnHand = 1
  ENDIF
  *E301261,1 SSH(END)
  *E301469,1 AMH 10/19/2000 Add modules for calling from SMPURGE [Start]
  *-- Invoice details.
  IF 'SH' $ lcToReb AND ('AR' $ lcInstMod)
    lcInsModul = IIF(EMPTY(lcInsModul),'AR',lcInsModul+',AR')
    lnShp = 1
  ENDIF
  
  *-- Sales Order or Picking ticktes.
  IF (('OR' $ lcToReb) .OR. ('AL' $ lcToReb)) AND (('SO' $ lcInstMod) .OR. ('AL' $ lcInstMod))
    lcInsModul = IIF('SO' $ lcInstMod,IIF(EMPTY(lcInsModul),'SO',lcInsModul+',SO'),lcInsModul)
    lcInsModul = IIF('AL' $ lcInstMod,IIF(EMPTY(lcInsModul),'AL',lcInsModul+',AL'),lcInsModul)
    
    IF ('OR' $ lcToReb)
      lnOrd = 1
    ENDIF
    
    IF ('AL' $ lcToReb)
      lnAlo = 1
    ENDIF
  ENDIF  
  
  *-- Return Details.
  IF 'RE' $ lcToReb AND ('RM' $ lcInstMod)
    lcInsModul = IIF(EMPTY(lcInsModul),'RM',lcInsModul+',RM')
    lnRet = 1
  ENDIF
  
  *-- Inventory Control.
  IF 'ST' $ lcToReb AND ('IC' $ lcInstMod)
    lcInsModul = IIF(EMPTY(lcInsModul),'IC',lcInsModul+',IC')
    lnStock = 1
  ENDIF
  *E301469,1 AMH [End]
  IF !EMPTY(lcInsModul)
    lcFilePath = lcCmPath
    *lcCompMods = lcInsModul
    =lfRebalanc(lcCompCode,lcCmName,lcCmPath,lcInsModul)  && Rebalance Passed company.
  ENDIF  
  
ELSE

  *E301337,1 ASH 10/18/1999 (Begin) Commented out.
  *IF !gfSetup()
  *  RETURN
  *ENDIF  

  *E301077,27 Open syscomp file.
  *-- llSysComp : .T. if this program open company file.
*  llSysComp = gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'Ccomp_id','SH')

  *-- declare variables.
  *E301337,1 ASH 10/18/1999 (Begin) Commented out to be declared at the beginninh of the program.
  *DECLARE laCompany[1,3]
  *laCompany = ''
  *E301337,1 ASH 10/18/1999 (End)  
  *-- Variables hold enable/disable to control object showen in screen.
  
  *E301232,1  Add variable control Style on hand Status (Enable/Disable) [Begin]
  *-- lcStkModSt : variable control Style on hand Status (Enable/Disable).
  *STORE '' TO lcComp_ID,lcOpModSt1,lcOpModSt2,lcSoModSt,;
  *            lcArModSt,lcRmModSt,lcMaModSt,lcSelSt
  *E301261,1 SSH 06/14/99 Add variable control Fabric on hand Status (Enable/Disable).
  *STORE '' TO lcComp_ID,lcOpModSt1,lcOpModSt2,lcSoModSt,;
              lcArModSt,lcRmModSt,lcMaModSt,lcSelSt,lcStkModSt
  *E301337,1 ASH 10/18/1999 (Begin) Commented out
  *STORE '' TO lcComp_ID,lcOpModSt1,lcOpModSt2,lcSoModSt,;
              lcArModSt,lcRmModSt,lcMaModSt,lcSelSt,lcStkModSt,lcMonHModSt
  *E301261,1 SSH (END)
  *E301232,1  Add variable control Style on hand Status (Enable/Disable) [End  ]
  
  
*  STORE 'DISABLE' TO lcReBalSt,lcNoneSt  && Rebalance Object and Select None Obj.

  *-- Variables used in calculate no. of enabled/disabled objects 
  STORE 0 TO lnSelRebal,lnOldVal,lnMaxToSel
*E301337,1 ASH 10/18/1999 (Begin) Commented out and declared at the begining ofthe program.
*  SELECT SYCCOMP
  *-- Collect all companies
  *E301098,1 Hesham (Start)
  *SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,mModlSet ;
    FROM (gcSyshome+"syccomp") ;
    INTO ARRAY laCompany ;
    ORDER BY 1
*  SELECT ccomp_id+" - "+cCom_Name,PADR(gfGetDataDir(ALLT(cCom_dDir)),LEN(cCom_dDir)),mModlSet ;
    FROM (gcSyshome+"syccomp") ;
    INTO ARRAY laCompany ;
    ORDER BY 1
  *E301098,1 Hesham (End)
  
*  IF _TALLY > 0

*    DECLARE laCompany [_TALLY+1,3] 
*
*    =AINS('laCompany',1)
*    laCompany[1,1] = "                  Select All Companies "
*    laCompany[1,2] = " "
*    laCompany[1,3] = laCompany[2,3]
*    puCompany      = 2

*    PRIVATE lnI
*    *-- Loop to get first company modules.
*    FOR lnI = 3 TO ALEN(laCompany,1) 
*      laCompany[1,3] = laCompany[1,3] + ALLTRIM(laCompany[lnI,3])
*      *-- Default company is current used company
*      IF (puCompany = 2) AND (gcAct_Comp == PADR(laCompany[lnI,1],2))
*        puCompany = lnI
*      ENDIF
*    ENDFOR
*    *-- 
*    =lfvCompany(.T.)  && Get initial value which is the same code for company validation.
*    DO (gcScrDir+gcWinAppl+'\SMREBAL.SPR')
*  ELSE  && No Companies 
*    *-- Message : There are no companies installes in the system.
*    *--                            < Ok > 
*    =gfModalGen('INM00339B00000','DIALOG')
*  ENDIF
*E301337,1 ASH 10/18/1999 (End)
  = lfRebalanc(lcCompCode,lcCompName,lcFilePath,lcCompMods)  && Rebalnce this company
  glQuitting = .T.  && Rise quit flag because it's modal screen.
ENDIF
*-- end of program code.

*E301077,27 Close system company file.
IF llSysComp
  =gfCloseFile('SYCCOMP')
ENDIF

*-- Functions and procedures.
*--------------------------------------------------------------


*!*************************************************************
*! Name      : lfWOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Get current object old value.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : ....
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfWOldVal()
*!*************************************************************
*!
FUNCTION lfWOldVal

*B605667,1 Correct variable name. [Begin]
*lnOldVal = EVALUATE(SYS(18))
laOldVal = EVALUATE(SYS(18))
*B605667,1 Correct variable name. [End]
*-- end of lfWOldVal.

*!*************************************************************
*! Name      : lfRebalanc
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Rebalnce Code
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfOpenFls,lfFilesStr,lfRelatFls,lfStartReb
*!*************************************************************
*! Passed Parameters      : ....
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfRebalanc()
*!*************************************************************
*!
FUNCTION lfRebalanc
PARAMETERS lcCurrComp,lcCurrName,lcCurrPath,lcCurModul

*-- if pass from another program and no passed codes.
IF TYPE('lcCurrPath') $ 'UL' OR EMPTY(lcCurrPath) OR ;
   TYPE('lcCurrName') $ 'UL' OR EMPTY(lcCurrName) OR ;
   TYPE('lcCurModul') $ 'UL' OR EMPTY(lcCurModul)
  lcCurrPath = gcDataDir
  lcCurrName = gcCom_Name
  lcCurModul = gcCmpModules
ENDIF

llOpnError = .F.  && initially there is No errors in open files.

*B602205 Add new Col to array hold Alias Name...
*DIMENSION laFileStr[1,3]
DIMENSION laFileStr[1,4]

laFileStr = ''  && array hold files to open.

*-- Setup flags for installed modules [Begin]

*E301077,27 Call gfGetMemVar once per company [Begin]
*llDyelot    =  gfGetMemVar('M_Dyelot',lcCurrComp)    = 'Y'
*llMultWare  =  gfGetMemVar('M_WareHouse',lcCurrComp) = 'Y'
=gfGetMemVar(@laSetups,lcCurrComp)
llDyelot    = (UPPER(ALLTRIM(laSetups[1,2])) = 'Y')
llMultWare  = (UPPER(ALLTRIM(laSetups[2,2])) = 'Y')
llFabDye    = (UPPER(ALLTRIM(laSetups[3,2])) = 'Y')
*

*E301077,27 Call gfGetMemVar once per company [End  ]

llWip_nWo   = ('MF' $ lcCurModul OR 'PO' $ lcCurModul) AND ;
              ((lnWip=1) OR (lnWo=1))

llInTrans   = ('PO' $ lcCurModul) AND (lnIntrans=1)

llSoOrder   = ('SO' $ lcCurModul) AND ((lnOrd=1) OR (lnAlo=1))
llInvoice   = ('AR' $ lcCurModul) AND (lnShp=1)

llRetMemo   = ('RM' $ lcCurModul) AND (lnRet=1)
llRauth     = ('RM' $ lcCurModul) AND (lnRa=1)

*E301232,1  Add Stock on hand to open style file [Begin]
*llStyFile  = llWip_nWo OR llInTrans  OR llSoOrder OR llInvoice OR llRetMemo OR llRauth
llStyFile   = (lnStock = 1) OR llWip_nWo OR llInTrans  OR llSoOrder OR ;
                  llInvoice OR llRetMemo OR llRauth
*E301232,1  Add Stock on hand to open style file [End  ]
*llStyDyeFl  = llStyFile AND (llDyelot OR llMultWare)
llStyDyeFl  = llStyFile 

llMaterial  = ('MA' $ lcCurModul) AND ((lnOnOrder=1) OR (lnOnHand=1))
*llFabDyeFl  = llMaterial AND (llFabDye OR llMultWare)
llFabDyeFl = llMaterial 
*-- Setup flags for installed modules [End]

*-- Add Fabric file to string array if its module used.
IF llMaterial
  *E301337,1 ASH 10/18/1999 (Begin) Create another file to verify in.
  IF lcVrUpd = 'V'
    = lfFilesStr('lcFabAls','Fabric','lcFabAls','Material module')
    = lfFilesStr('lcFDyAls','FabDye','lcFDyAls','Material module')
  ELSE  
    IF USED('FABDYE')	
      USE IN FABDYE
    ENDIF
   *E301337,1 ASH 10/18/1999 (End)
    = lfFilesStr('FABRIC','FABRIC',lcFabAls,'Material module')
    = lfFilesStr('FABDYE','FABDYE',lcFDyAls,'Material module')
  ENDIF
  = lfFilesStr('POFHDR','POFHDR',lcPoHdAls,'Material module') AND lfFilesStr('POFLN','POFLNF',lcPoLnAls,'Material module')  
  *-- Open the following files but its code after check with renee to add fabric index.
  = lfFilesStr('MMFGORDH','MMFGORDD',lcMfHdAls,'Material module') AND lfFilesStr('MMFGORDD','MMFGORDD',lcMfLnAls,'Material module')
*B803312,1 ASH 06/04/2000 (Begin) Don't Mat. onhand and onorder rebalance if the material not installed for the company.
ELSE
  lnOnOrder = 0
  lnOnHand  = 0
*B803312,1 ASH 06/04/2000 (End)  
ENDIF

*-- Add Style and StyDye Files to string array.
IF llStyFile
  *E301337,1 ASH 10/18/1999 (Begin) Create another file to verify in.
  IF lcVrUpd = 'V'
    = lfFilesStr('lcStyAls','STYLE','lcStyAls') AND lfFilesStr('lcSDyAls','STYDYE','lcSDyAls')
  ELSE
  *E301337,1 ASH 10/18/1999 (End)
    IF USED('STYDYE')
      USE IN STYDYE
    ENDIF
    = lfFilesStr('STYLE','STYLE',lcStyAls) AND lfFilesStr('STYDYE','STYDYE',lcSDyAls)
  ENDIF
*B803312,1 ASH 06/04/2000 (Begin) Don't run stock on hand rebalance if IC module not installed.
ELSE
  lnStock = 0
*B803312,1 ASH 06/04/2000 (End)  
ENDIF

*E301232,1 Add Style inventory journal file to string array [Begin]

IF lnStock = 1
  = lfFilesStr('STYINVJL','STYINVJL',lcStyJlAls)
ENDIF
*E301232,1 Add Style inventory journal file to string array [End  ]
*E301261,1 SSH 06/14/99 Add Material inventory journal file to string array

IF lnOnHand = 1 
  = lfFilesStr('MATINVJL','MATINVJL',lcMatJlAls)
ENDIF
*E301261,1 SSH(END)

*-- Add C/T Files to string array.
IF llWip_nWo AND ('MF' $ lcCurModul)
  = lfFilesStr('CUTTKTH','CUTTKTH',lcCutHdAls,'Manufacture module') AND lfFilesStr('CUTTKTL','CUTTKTLS',lcCutLnAls,'Manufacture module')
ENDIF

*-- Add P/O Files to string array.
IF llInTrans OR (llWip_nWo AND ('PO' $ lcCurModul))
  = lfFilesStr('POSHDR','POSHDR',lcPoHAls,'Purchase Order module') AND lfFilesStr('POSLN','POSLNS',lcPoLAls,'Purchase Order module')
ENDIF

*-- Add Wip adjustment File to string array.
IF llWip_nWo AND (lnWip=1)
  = lfFilesStr('WIPADJ','WIPADJ',lcWipJAls)
ENDIF

*-- Add Sales order Files to string array.
IF llSoOrder
  = lfFilesStr('ORDHDR','ORDHDR',lcOrdHAls,'Sales Order module') AND lfFilesStr('ORDLINE','ORDLINES',lcOrdLAls,'Sales Order module')
ENDIF

*-- Add A/R Files to string array.
IF llInvoice
  = lfFilesStr('INVHDR','INVHDR',lcInvHAls,'Account Receivable module') AND lfFilesStr('INVLINE','INVLINES',lcInvLAls,'Account Receivable module')
ENDIF

*-- Add RM credit memo Files to string array.
IF llRetMemo
  = lfFilesStr('RETHDR','RETHDR',lcRetHdAls,'Ret. Merch. Credit memo files') AND lfFilesStr('RETLINE','RETLINES',lcRetLnAls,'Ret. Merch. Credit memo files')
ENDIF

*-- Add RM return authorization Files to string array.
IF llRauth
  = lfFilesStr('RETAUTH','RETAUTH',lcRaHdAls,'Ret. Merch. Authorization files') AND lfFilesStr('RALINE','RALINES',lcRaLnAls,'Ret. Merch. Authorization files')
ENDIF


IF !EMPTY(laFileStr[1,1])
  =lfOpenFls()  && Open files in string array.
ENDIF  

*-- if there is no errors in open files start rebalancing.
IF !EMPTY(laFileStr[1,1]) AND !llOpnError

  = lfRelatFls()  && Make relation between files.

  = lfStartReb()  && Start rebalance.

  *-- Close company opend files.
  *E301077,27 Close All open files calling function do this task [Begin]
  *E301077,27 to include error handler in it. 
  *FOR lnI = 1 TO ALEN(laFileStr,1)
    *USE IN (laFileStr[lnI,4])  && Close this alias...
  *ENDFOR
  =lfCloseFls(ALEN(laFileStr,1))
  *E301077,27 Close All open files calling function do this task [End  ]

ENDIF
*-- end of lfRebalanc.

*!*************************************************************
*! Name      : lfFilesStr
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Fill file string array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : ....
*!*************************************************************
*! Returns                : .T. to concatenade it in logical expressions.
*!*************************************************************
*! Example   : = lfFilesStr()
*!*************************************************************
*!
FUNCTION lfFilesStr
PARAMETERS lcFileName,lcFileTag,lcAliasNam,lcErrMesg,lcReltMode

IF TYPE('lcErrMesg') $ 'UL' OR EMPTY(lcErrMesg)
  lcErrMesg = PROPER(lcFileName) + ' File'
ENDIF

IF TYPE('lcReltMode') $ 'UL' OR EMPTY(lcReltMode)
  lcReltMode = ''
ENDIF

IF !EMPTY(laFileStr[1,1])
  *B602205 Add new colomn.
  *DIMENSION laFileStr[ALEN(laFileStr,1)+1,3])
  DIMENSION laFileStr[ALEN(laFileStr,1)+1,4])
ENDIF

laFileStr[ALEN(laFileStr,1),1] = lcFileName  && File Name.
laFileStr[ALEN(laFileStr,1),2] = lcFileTag   && Active index.
laFileStr[ALEN(laFileStr,1),3] = lcErrMesg   && Error message.
*B602205 Add new colomn.
laFileStr[ALEN(laFileStr,1),4] = lcAliasNam  && Alias name.

RETURN .T.
*-- end of lfFilesStr.

*!*************************************************************
*! Name      : lfOpenFls
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Open files in file string array.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Passed Parameters      : ....
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfOpenFls()
*!*************************************************************
*!
FUNCTION lfOpenFls
PRIVATE lnI,lnBreak
lnBreak = 0

*-- loop around file string array to open files and lock it.
FOR lnI = 1 TO ALEN(laFileStr,1,1)

  IF FILE(lcCurrPath+laFileStr[lnI,1]+'.DBF')
    *E301077,27 Create Temp. alias using gfOpenFile [Begin
    *USE (lcCurrPath+laFileStr[lnI,1]) ORDER TAG (laFileStr[lnI,2]) AGAIN ALIAS (laFileStr[lnI,4]) IN 0 SHARED
    *B803107,1 ASH 03/13/2000 (Begin) Set the order manualy not from gfopenfile to avoid alias not found.
    *=gfOpenFile(lcCurrPath+laFileStr[lnI,1],;
                IIF(EMPTY(laFileStr[lnI,2]),'',lcCurrPath+laFileStr[lnI,2]),;
                'SH',laFileStr[lnI,4])
    =gfOpenFile(lcCurrPath+laFileStr[lnI,1],'','SH',laFileStr[lnI,4])
    SET ORDER TO laFileStr[lnI,2]
    *B803107,1 ASH 03/13/2000 (End)
    
    *E301077,27 Create Temp. alias using gfOpenFile [End]
    IF lcVrUpd = 'V' AND UPPER(laFileStr[lnI,1]) = 'LCFDYALS'
      SET RELATION TO Fabric+Color+cWareCode+Dyelot INTO FabDye
    ENDIF
    IF lcVrUpd = 'V' AND UPPER(laFileStr[lnI,1]) = 'LCSDYALS'
      SET RELATION TO Style+cWareCode+Dyelot INTO StyDye
    ENDIF
    
  ELSE  && File does not exist in data directory.
    *-- Message : XXX Not found in data file.
    *--                   <Ok>
    = gfModalGen('TRM00001B00000','Alert',laFileStr[lnI,1])
    llOpnError = .T.
  ENDIF  

  *-- if you open file savely then try locking it.
  IF !llOpnError
    *-- loop until you can lock current file.
    DO WHILE !FLOCK(laFileStr[lnI,4])
      lcErrMesg = UPPER(lcCurrPath+laFileStr[lnI,1])+'|'+laFileStr[lnI,3]
      *-- Message : ð file is used by another user, Rebalancing required single use of ð.
      *--                                <Retry>  <Cancel>
      IF gfModalGen('QRM00329B00015','Dialog',lcErrMesg) = 1
        LOOP
      ELSE
        llOpnError = .T.
        EXIT  && exit do loop.
      ENDIF  
    ENDDO
  ENDIF    && end if you open file savely then try locking it.
  
  *-- if file open error or this file is used by another and you press Cancel. [Begin]
  IF llOpnError
    lnBreak = lnI
    EXIT    && exit for loop.
  ENDIF
  *-- if file open error or this file is used by another and you press Cancel. [End  ]

ENDFOR

*-- Close opened files if any because error occured.
IF lnBreak > 0
  *E301077,27 Close All open files calling function do this task [Begin]
  *E301077,27 to include error handler in it. 
  *FOR lnI = 1 TO lnBreak
  *  USE IN (laFileStr[lnI,4])  && Close This alias...
  *ENDFOR
  =lfCloseFls(lnBreak)
  *E301077,27 Close All open files calling function do this task [End  ]
ENDIF
*-- end if lfOpenFls.

*!*************************************************************
*! Name      : lfRelatFls
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Make relation between Open files.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : ....
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfRelatFls()
*!*************************************************************
*!
FUNCTION lfRelatFls
  
*-- Make relation between files.

IF llMaterial
  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    SELECT lcFabAls
  ELSE
  *E301337,1 ASH 10/18/1999 (End)
    SELECT (lcFabAls)
  ENDIF
  SET RELATION TO FABRIC+COLOR INTO (lcPoLnAls) ADDITIVE  
  SET RELATION TO FABRIC INTO (lcMfHdAls) ADDITIVE
  *E301261,1 SSH 06/14/99 Set relation between Fabric and Fabric inventory journal file.
  IF lnOnHand = 1
    *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    IF lcVrUpd = 'V'
      SELECT lcFabAls
    ELSE
    *E301337,1 ASH 10/18/1999 (End)
      SELECT (lcFabAls)
    ENDIF

    SET RELATION TO FABRIC+COLOR INTO (lcMatJlAls) ADDITIVE
  ENDIF
  *E301261,1 SSH(END)
  SELECT (lcPoLnAls)
  SET RELATION TO cMatType + PoMat INTO (lcPoHdAls)
  SELECT (lcMfHdAls)
  SET RELATION TO cMfgordno INTO (lcMfLnAls)

ENDIF
  
IF llStyFile

  *E301232,1 Set relation between Style and Style inventory journal file [Begin]
  IF lnStock = 1
    *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    IF lcVrUpd = 'V'
      SELECT lcStyAls
    ELSE
    *E301337,1 ASH 10/18/1999 (End)
      SELECT (lcStyAls)
    ENDIF
    SET RELATION TO STYLE INTO (lcStyJlAls) ADDITIVE
  ENDIF
  *E301232,1 Set relation between Style and Style inventory journal file [Begin]
  *-- C/T module.
  IF llWip_nWo AND ('MF' $ lcCurModul)
    IF lcVrUpd = 'V'
      SELECT lcStyAls
    ELSE
      SELECT (lcStyAls)
    ENDIF
    
    SET RELATION TO STYLE INTO (lcCutLnAls) ADDITIVE

    SELECT (lcCutLnAls)
    SET RELATION TO CUTTKT INTO (lcCutHdAls)
  ENDIF

  *-- P/O Module.
  IF llInTrans OR (llWip_nWo AND ('PO' $ lcCurModul))
    IF lcVrUpd = 'V'
      SELECT lcStyAls
    ELSE
      SELECT (lcStyAls)
    ENDIF
    SET RELATION TO STYLE INTO (lcPoLAls) ADDITIVE
    SELECT (lcPoLAls)
    SET RELATION TO cStyType+PO INTO (lcPoHAls)
    
  ENDIF

  *-- Wip Adjustment file.
  IF llWip_nWo AND (lnWip=1)
    IF lcVrUpd = 'V'
      SELECT lcStyAls
    ELSE
      SELECT (lcStyAls)
    ENDIF
    SET RELATION TO STYLE INTO (lcWipJAls) ADDITIVE
  ENDIF

  *-- sales order module.
  IF llSoOrder
    IF lcVrUpd = 'V'
      SELECT lcStyAls
    ELSE
      SELECT (lcStyAls)
    ENDIF
    SET RELATION TO STYLE INTO (lcOrdLAls) ADDITIVE

    SELECT (lcOrdLAls)
    SET RELATION TO 'O'+Order INTO (lcOrdHAls) 
  ENDIF

  *-- A/R module.
  IF llInvoice
    IF lcVrUpd = 'V'
      SELECT lcStyAls
    ELSE
      SELECT (lcStyAls)
    ENDIF
    SET RELATION TO STYLE INTO (lcInvLAls) ADDITIVE

    SELECT (lcInvLAls)
    SET RELATION TO Invoice INTO (lcInvHAls)
  ENDIF

  *-- Credit memo files.
  IF llRetMemo
    IF lcVrUpd = 'V'
      SELECT lcStyAls
    ELSE
      SELECT (lcStyAls)
    ENDIF
    SET RELATION TO STYLE INTO (lcRetLnAls) ADDITIVE

    SELECT (lcRetLnAls)
    SET RELATION TO CrMemo INTO (lcRetHdAls)
  ENDIF

  *-- Return authorization files.
  IF llRauth
    IF lcVrUpd = 'V'
      SELECT lcStyAls
    ELSE
      SELECT (lcStyAls)
    ENDIF
    SET RELATION TO STYLE INTO (lcRaLnAls) ADDITIVE

    SELECT (lcRaLnAls)
    SET RELATION TO RaNo INTO (lcRaHdAls)
  ENDIF
  
ENDIF
*-- end of lfRelatFls.

*!*************************************************************
*! Name      : lfStartReb
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Start rebalance if records found greater than 0.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfUpdSty,lfUpdFab
*!*************************************************************
*! Passed Parameters      : ....
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfStartReb()
*!*************************************************************
*!
FUNCTION lfStartReb
STORE 0 TO lnThermNo,lnUpThermo

*-- if update style file.
IF llStyFile
  IF lcVrUpd = 'V'
    lnRecCnt = RECCOUNT('lcStyAls')
  ELSE
    lnRecCnt = RECCOUNT(lcStyAls)
  ENDIF  
  IF lnRecCnt > 0
    IF lcVrUpd = 'V'
      SELECT lcStyAls
    ELSE
      SELECT (lcStyAls)
    ENDIF
    COUNT TO lnUpThermo FOR !DELETED()  && All Style(s) in style file.
    IF lnUpThermo > 0
      llFinish = .T.
      IF !llConvert
        WAIT WINDOW 'Rebalancing selected database(s) for company ' + lcCurrName NOWAIT
      ENDIF

      = lfUpdSty()  && Update style file
          
      *-- mab
      IF llDyelot AND TYPE("lcTmpDyeFl")$"C" AND USED(lcTmpDyeFl)
        USE IN (lcTmpDyeFl)
      ENDIF

    ENDIF
  ENDIF
ENDIF
*-- if material module.

IF llMaterial AND IIF(lcVrUpd='V',RECCOUNT('lcFabAls'),RECCOUNT(lcFabAls)) > 0
  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    SELECT lcFabAls
  ELSE
  *E301337,1 ASH 10/18/1999 (End)
    SELECT (lcFabAls)
  ENDIF

  COUNT TO lnUpThermo FOR !DELETED()  && All fabric/color(s) in fabric file.
  IF lnUpThermo > 0
    IF !llFinish
      llFinish = .T.

      IF !llConvert
        WAIT WINDOW 'Rebalancing selected database(s) for company ' + lcCurrName NOWAIT
      ENDIF  
    ENDIF
    = lfUpdFab()  && Update fabric file.
  ENDIF
ENDIF      && end if material module.

IF llFinish AND !llConvert
  WAIT WINDOW 'Finishing rebalance selected database(s) for company ' + lcCurrName TIMEOUT 2
  WAIT CLEAR
ENDIF
llFinish = .F.
*-- end of lfStartReb.

*!*************************************************************
*! Name      : lfUpdSty
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Rebalance Style file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfUpdtOper,lfUpdtLoop,gfThermo
*!*************************************************************
*! Passed Parameters      : ....
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfUpdSty()
*!*************************************************************
*!
FUNCTION lfUpdSty

lnThermNo  = 0		     		 && Initiall Value for thermo Fn.

DIMENSION laUpdatArr[2,4]        && array used to update stydye file.

*-- Assign initial values to arrays used in data collection. [begin]
*-- each part is separate code for faster scan file and get more flexability.


*E301232,1 Define array hold style on hand values [Begin]
*-- Style On-Hand case.
IF lnStock = 1
  *-- laStock    : Array hold stock per style.
  *-- laLocStock : Array hold stock per style,location.
  *-- 1 -> 8 hold Stk1 -> Stk8
  *-- 9  hold Totstk
  *-- 10 hold nStkVal
  DIMENSION laStock[10] , laLocStock[10] , laStkArr[1,4]
  STORE 0 TO laStock,laLocStock

  *-- laStkArr : Array hold Stock arrays information.
  laStkArr[1,1] = 'lnStock=1'
  laStkArr[1,2] = 'laLocStock'
  laStkArr[1,3] = 'STK'
  laStkArr[1,4] = 'NSTK'

  IF llDyelot
    lcTmpDyeFl = gfTempName()
    =lfCrtTmpDy()
  ENDIF  

ENDIF
*E301232,1 Define array hold style on hand values [End  ]
*-- WIP or nWo cases
IF llWip_nWo

  IF lnWip=1
    DIMENSION laWip[9],laTWip[9],laSWip[9]
    STORE 0 TO laWip,laTWip,laSWip
  ENDIF

  IF lnWo=1
    DIMENSION laWo[9],laTempWo[9],laTWo1[9],laSWo1[9]
    STORE 0 TO laWo,laTempWo,laTWo1,laSWo1
  ENDIF

ENDIF

*-- Intransit case
IF llInTrans
  IF lnIntrans = 1
    DIMENSION laIntrans[9]
    laIntrans = 0
  ENDIF
ENDIF  

*-- Sales Order module ( Ord 1->8 and Alo 1->8 )
IF llSoOrder
    
  IF lnOrd=1
    DIMENSION laOrder[9]
    laOrder = 0

    DIMENSION laOrdArr[1,4]
    laOrdArr[1,1] = 'lnOrd=1'
    laOrdArr[1,2] = 'laOrder'
    laOrdArr[1,3] = 'ORD'
    laOrdArr[1,4] = 'QTY'
    
  ENDIF  
    
  IF lnAlo=1
    DIMENSION laAlo[9]
    laAlo = 0

    IF TYPE('laOrdArr') $ 'UL'
      DIMENSION laOrdArr[1,4]
    ELSE
      DIMENSION laOrdArr[ALEN(laOrdArr,1) + 1,4]
    ENDIF

    laOrdArr[ALEN(laOrdArr,1),1] = 'lnAlo = 1'
    laOrdArr[ALEN(laOrdArr,1),2] = 'laAlo'
    laOrdArr[ALEN(laOrdArr,1),3] = 'ALO'
    laOrdArr[ALEN(laOrdArr,1),4] = 'PIK'

  ENDIF  
  
ENDIF  

IF llInvoice
  *-- Note: Because llInvoice Flag contains (lnShp = 1) Condition alone
  *--       I can use this condition only for Defination and code 
  *--       but for get more flexiability to code to increase another 
  *--       conditions in the future, I make separate block code
  *--       under its unique condition .
  IF lnShp = 1
    DIMENSION laShp[9]
    laShp = 0
    DIMENSION laInvArr[1,4]
    laInvArr[1,1] = 'lnShp = 1'
    laInvArr[1,2] = 'laShp'
    laInvArr[1,3] = 'SHP'
    laInvArr[1,4] = 'QTY'
  ENDIF

ENDIF

IF llRauth

  *-- Note: Because llRauth Flag contains (lnRa = 1) Condition alone
  *--       I can use this condition only for Defination and code 
  *--       but for get more flexiability to code to increase another 
  *--       conditions in the future, I make separate block code
  *--       under its unique condition .
  IF lnRa = 1
    DIMENSION laRa[9]
    laRa = 0
    DIMENSION laAuthArr[1,4]
    laAuthArr[1,1] = 'lnRa = 1'
    laAuthArr[1,2] = 'laRa'
    laAuthArr[1,3] = 'RA'
    laAuthArr[1,4] = 'NOPNQTY'
  ENDIF

ENDIF

IF llRetMemo

  *-- Note: Because llRetMemo Flag contains (lnRet = 1) Condition alone
  *--       I can use this condition only for Defination and code 
  *--       but for get more flexiability to code to increase another 
  *--       conditions in the future, I make separate block code
  *--       under its unique condition .
  IF lnRet = 1
    DIMENSION laRet[9]
    laRet = 0
    DIMENSION laMemoArr[1,4]
    laMemoArr[1,1] = 'lnRet = 1'
    laMemoArr[1,2] = 'laRet'
    laMemoArr[1,3] = 'RET'
    laMemoArr[1,4] = 'QTY'
  ENDIF  

ENDIF
*-- Assign initial values to arrays used in data collection. [end]


*-- initially Clear Stydye data to accomulate it for both [begin]
*-- Location record and dyelot record.
IF llStyDyeFl
  
  *E301232,1 For optimizing performance not for this entry I do the following
  *          - Instead of direct replace for every transaction add replace expression.
  *          - One replace was done for all transaction types.
  * Note     Old Code have replace for every case and I delete them for shortness of code
  *          to be more readable.
  PRIVATE lcReplcExp
  lcReplcExp = ''
  
  *E301232,1 Define array hold style on hand values [Begin]
  *-- Style On-Hand case.
  IF lnStock = 1
    lcReplcExp = IIF(EMPTY(lcReplcExp),'', lcReplcExp + ',')        + ;
                 [Stk1 WITH 0,Stk2 WITH 0,Stk3 WITH 0,Stk4 WITH 0,] + ;
                 [Stk5 WITH 0,Stk6 WITH 0,Stk7 WITH 0,Stk8 WITH 0,] + ;
                 [TotStk WITH 0,nStkVal WITH 0]

  ENDIF
  *E301232,1 Define array hold style on hand values [End  ]

  *-- Wips and nWos
  IF llWip_nWo
    IF lnWip=1
      lcReplcExp = IIF(EMPTY(lcReplcExp),'', lcReplcExp + ',')        + ;
                   [Wip1 WITH 0,Wip2 WITH 0,Wip3 WITH 0,Wip4 WITH 0,] + ;
                   [Wip5 WITH 0,Wip6 WITH 0,Wip7 WITH 0,Wip8 WITH 0,] + ;
                   [TotWip WITH 0]
    ENDIF
    IF lnWo=1
      lcReplcExp = IIF(EMPTY(lcReplcExp),'', lcReplcExp + ',')        + ;
                   [nWo1 WITH 0,nWo2 WITH 0,nWo3 WITH 0,nWo4 WITH 0,] + ;
                   [nWo5 WITH 0,nWo6 WITH 0,nWo7 WITH 0,nWo8 WITH 0,] + ;
                   [nTotWo WITH 0]
    ENDIF
  ENDIF

  *-- Intransit case
  IF llInTrans
    IF lnIntrans = 1
      lcReplcExp = IIF(EMPTY(lcReplcExp),'', lcReplcExp + ',')                        + ;
                   [InTrans1 WITH 0,InTrans2 WITH 0,InTrans3 WITH 0,InTrans4 WITH 0,] + ;
                   [InTrans5 WITH 0,InTrans6 WITH 0,InTrans7 WITH 0,InTrans8 WITH 0,] + ;
                   [TotInTrn WITH 0]
    ENDIF
  ENDIF  

  *-- Ord(S) and Alo(s) fields.
  IF llSoOrder  && Sales order module.
    IF lnOrd=1
      lcReplcExp = IIF(EMPTY(lcReplcExp),'', lcReplcExp + ',')        + ;
                   [Ord1 WITH 0,Ord2 WITH 0,Ord3 WITH 0,Ord4 WITH 0,] + ;
                   [Ord5 WITH 0,Ord6 WITH 0,Ord7 WITH 0,Ord8 WITH 0,] + ;
                   [TotOrd WITH 0]
    ENDIF  
    IF lnAlo=1
      lcReplcExp = IIF(EMPTY(lcReplcExp),'', lcReplcExp + ',')        + ;
                   [Alo1 WITH 0,Alo2 WITH 0,Alo3 WITH 0,Alo4 WITH 0,] + ;
                   [Alo5 WITH 0,Alo6 WITH 0,Alo7 WITH 0,Alo8 WITH 0,] + ;
                   [TotAlo WITH 0]
    ENDIF  
  ENDIF  
  
  *-- Shp(s) Fields.
  IF llInvoice  && AR module.
    IF lnShp = 1
      lcReplcExp = IIF(EMPTY(lcReplcExp),'', lcReplcExp + ',')        + ;
                   [Shp1 WITH 0,Shp2 WITH 0,Shp3 WITH 0,Shp4 WITH 0,] + ;
                   [Shp5 WITH 0,Shp6 WITH 0,Shp7 WITH 0,Shp8 WITH 0,] + ;
                   [TotShp WITH 0]
    ENDIF  
  ENDIF
  
  *-- Ra(s) Fields.
  IF llRauth
    IF lnRa = 1
      lcReplcExp = IIF(EMPTY(lcReplcExp),'', lcReplcExp + ',')    + ;
                   [Ra1 WITH 0,Ra2 WITH 0,Ra3 WITH 0,Ra4 WITH 0,] + ;
                   [Ra5 WITH 0,Ra6 WITH 0,Ra7 WITH 0,Ra8 WITH 0,] + ;
                   [TotRa WITH 0]
    ENDIF
  ENDIF

  *-- Ret(s) fields.
  IF llRetMemo
    IF lnRet = 1
      lcReplcExp = IIF(EMPTY(lcReplcExp),'', lcReplcExp + ',')        + ;
                   [Ret1 WITH 0,Ret2 WITH 0,Ret3 WITH 0,Ret4 WITH 0,] + ;
                   [Ret5 WITH 0,Ret6 WITH 0,Ret7 WITH 0,Ret8 WITH 0,] + ;
                   [TotRet WITH 0]
    ENDIF
  ENDIF

  *-- Update stydye file with zero values and Trace its key if CM module is installed 
  *-- and stydye file is one of its view files.
  IF !EMPTY(lcReplcExp)
    IF lcVrUpd = 'V'
      SELECT lcSDyAls
      lcExpr = lcSDyAls.STYLE+lcSDyAls.CWARECODE+lcSDyAls.DYELOT
    ELSE
      SELECT (lcSDyAls)
      lcExpr = &lcSDyAls..STYLE+&lcSDyAls..CWARECODE+&lcSDyAls..DYELOT
    ENDIF
    REPLACE ALL &lcReplcExp FOR gfTraceKey('STYDYE',lcExpr,'M',lcFilePath,lcCompMods)
  ENDIF
  IF lcVrUpd = 'V'
    SELECT lcStyAls
  ELSE
    SELECT (lcStyAls)
  ENDIF
ENDIF
*-- initially Clear Stydye data to accomulate it for both [end]

GO TOP
*-- Scan Style file.
SCAN
  
  IF llConvert
    *E301469,1 AMH [Start]
    IF llPurge
      lnThermNo = lnThermNo + 1
      lcTermoMsg = "Rebalancing Company " + lcCurrComp + " Style:  "
      = gfThermo(lnUpThermo,lnThermNo,lcTermoMsg,STYLE)
    ELSE
      lcMFleDesc = 'Rebalance Styles...'
      SHOW GET lcMFleDesc
      =lfThrmo(lnCurNm1,lnUpThermo,'123')
    ENDIF  
    *E301469,1 AMH [End]
  ELSE
    lnThermNo = lnThermNo + 1
    lcTermoMsg = "Rebalancing Company " + lcCurrComp + " Style:  "
    = gfThermo(lnUpThermo,lnThermNo,lcTermoMsg,STYLE)
  ENDIF
  
  *E301232,1 Calculate Style on hand [Begin]
  IF lnStock = 1
    *-- We can make it easy like all transaction (Ord,Alo), by calling lfUpdtLoop function
    *-- after copy of its array, but I found that Style inventory journal file is one of the 
    *-- huge files in the system and its index start with Style + cWareCode 
    *-- and this case help us in special optimizing technique for this file.
    =lfStyStock()  && Calculate Stocks
  ENDIF
  *E301232,1 Calculate Style on hand [End  ]

  lcWorkFile = ''  && used in update Wip/nWo/InTrans
  *-- Update Wip(s) and nWo(s)
  IF llWip_nWo
    lnPrevVal = 0  
    DO CASE

      CASE Make AND ('MF' $ lcCurModul)
        lcWorkFile = lcCutLnAls
        *-- Set scan and while expressions for all transactions.
        IF lcVrUpd = 'V'
          lcWhilExpr = 'Style+Cuttkt+Trancd = lcStyAls.Style'
        ELSE
          lcWhilExpr = 'Style+Cuttkt+Trancd = &lcStyAls..Style'
        ENDIF
        lcForExpr  = IIF(lnWo = 1,[&lcCutHdAls..STATUS <> "X"],[!(&lcCutHdAls..STATUS $ "CX")])
        lcBatChng  = 'Style+Cuttkt'

      CASE ('PO' $ lcCurModul)
        lcWorkFile = lcPoLAls

        *-- Set scan and while expressions for all transactions.
        IF lcVrUpd = 'V'
          lcWhilExpr = 'Style+cStyType+PO+STR(lineno,6)+Trancd = lcStyAls.Style'
        ELSE
          lcWhilExpr = 'Style+cStyType+PO+STR(lineno,6)+Trancd = &lcStyAls..Style'
        ENDIF
        *B603853,1 ASH 08/28/2000 (Begin) Not include the closed P/O while computing the WIP.
        *lcForExpr  = [TOTQTY > 0 AND CSTYTYPE <> 'C' AND &lcPoHAls..STATUS <> "X"]
        lcForExpr  = IIF(lnWo=1,[TOTQTY > 0 AND CSTYTYPE <> 'C' AND &lcPoHAls..STATUS <> "X"],[TOTQTY > 0 AND CSTYTYPE <> 'C' AND !(&lcPoHAls..STATUS $ "SX")])
        *B603853,1 ASH 08/28/2000 (End)
        *B602616 Recalculate data if Inter-Location P/O every [Begin]
        *B602616 location change.
        *lcBatChng  = 'Style+cStyType+PO+STR(lineno,6)'
        lcBatChng  = 'Style+cStyType+PO+STR(lineno,6)+PADR(cWareCode,6)'
        *B602616 Recalculate data if Inter-Location P/O every [End  ]

      OTHERWISE
        lcWorkFile = lcWipJAls
    ENDCASE
  ENDIF
  
  *-- if in transit case only.
  IF llInTrans AND EMPTY(lcWorkFile)

    lcWorkFile = lcPoLAls
    *-- Set scan and while expressions for all transactions.
    *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    *lcWhilExpr = 'Style+cStyType+PO+STR(lineno,6)+Trancd = &lcStyAls..Style'
    IF lcVrUpd = 'V'
      lcWhilExpr = 'Style+cStyType+PO+STR(lineno,6)+Trancd = lcStyAls.Style'
    ELSE
    *E301337,1 ASH 10/18/1999 (End)
      lcWhilExpr = 'Style+cStyType+PO+STR(lineno,6)+Trancd = &lcStyAls..Style'
    ENDIF
    *B603992,1 ASH 07/11/2002 (Begin) Don't recompute intransit for status Complete and Closed.
    *lcForExpr  = [TOTQTY > 0 AND CSTYTYPE <> 'C' AND &lcPoHAls..STATUS <> "X"]    
    lcForExpr  = [TOTQTY > 0 AND CSTYTYPE <> 'C' AND &lcPoHAls..STATUS <> "CSX"]    
    *B603992,1 ASH 07/11/2002 (End)

    *B602616 Recalculate data if Inter-Location P/O every [Begin]
    *B602616 location change.
    *lcBatChng  = 'Style+cStyType+PO+STR(lineno,6)'
    lcBatChng  = 'Style+cStyType+PO+STR(lineno,6)+PADR(cWareCode,6)'
    *B602616 Recalculate data if Inter-Location P/O every [End  ]

  ENDIF
  llAdoDye = .F.
  *-- Update Work process.  
  IF llWip_nWo OR llInTrans
    = lfUpdtOper()
  ENDIF

  **CASE DYE ORDER OR ADORNMENT ORDER CHECK THE MAKE STYLE IN POSLN FILE.
  IF llWip_nWo AND MAKE AND ('PO' $ lcCurModul)
    lcWorkFile = lcPoLAls
    llAdoDye = .T.
    *-- Set scan and while expressions for all transactions.
    IF lcVrUpd = 'V'
      lcWhilExpr = 'Style+cStyType+PO+STR(lineno,6)+Trancd = lcStyAls.Style'
    ELSE
      lcWhilExpr = 'Style+cStyType+PO+STR(lineno,6)+Trancd = &lcStyAls..Style'
    ENDIF
    lcForExpr  = [TOTQTY > 0 AND IIF(lnWo=1,!(CSTYTYPE $ 'AC'),CSTYTYPE <> 'C') AND &lcPoHAls..STATUS <> "X"]
    lcBatChng  = 'Style+cStyType+PO+STR(lineno,6)+PADR(cWareCode,6)'
    = lfUpdtOper()
  ENDIF
  IF lnWip=1 AND lcVrUpD = 'V'  AND !llAdoDye
    IF lnPrevVal <> laWip[9]
      DECLARE laRebMsg[3]
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcCompCode +": "+ Style + " has wrong Work in process quantity."
      laRebMsg[3] = " "
      =lfVryRport()
    ENDIF
  ENDIF  
  IF lnWo=1 AND lcVrUpD = 'V'  AND !llAdoDye
    IF lnPrevVal <> laWo[9]
      DECLARE laRebMsg[3]
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcCompCode +": "+ Style + " has wrong Work order quantity."
      laRebMsg[3] = " "
      =lfVryRport()
    ENDIF
  ENDIF  
  IF lcVrUpd = 'V' 
    llError = .F.
    IF SEEK(lcStyAls.Style,'lcSDyAls')
      lcAlias = ALIAS()
      SELECT lcSDyAls
      SCAN WHILE STYLE = lcStyAls.Style
        IF lnWo=1 AND nTotWo<>StyDye.nTotWo
          llError = .T.
        ENDIF
        IF lnWip = 1 AND TotWip<> StyDye.TotWip
          llError = .T.
        ENDIF
      ENDSCAN
      *B606749,1 ASH 12/10/2002 (Begin) Fix the bug of wrong pointer in stydye.
      =SEEK(lcStyAls.Style,'lcSDyAls')
      *B606749,1 ASH 12/10/2002 (End)
      SELECT &lcAlias 
      IF llError  
        DECLARE laRebMsg[3]
        laRebMsg[1] = " "
        laRebMsg[2] = "Company" + lcCompCode +": "+ Style + " has wrong "+ IIF(lnWo=1,"WORK ORDER","WORK IN PROCESS") +" quantity."
        laRebMsg[3] = " "
        =lfVryRport()
      ENDIF
  
    ENDIF  
  ENDIF
  
  laWo  = 0  
  laWip = 0
  *-- Update Ord(s) and Alo(s)
  IF llSoOrder  
    =ACOPY(laOrdArr,laUpdatArr)  && copy order array to laUpdatArr to call generic code.
    *-- By pass parameters used in rebalance.
    IF lcVrUpd = 'V'
      lcExpr = "Style+DTOS(complete)+cordtype+order+Store+STR(lineno,6)=lcStyAls.Style"
    ELSE
      lcExpr = "Style+DTOS(complete)+cordtype+order+Store+STR(lineno,6)=&lcStyAls..Style"
    ENDIF
    =lfUpdtLoop(lcOrdLAls,lcExpr,;
                "(&lcOrdHAls..STATUS $ 'OH') AND (cordtype = 'O')",;
                "cWareCode")  
    laUpdatArr = .F.
  ENDIF  && end Update Ord(s) and Alo(s).

  *-- Update Shp Quantity.
  IF llInvoice
    =ACOPY(laInvArr,laUpdatArr)  && copy invoice array to alUpdatArr to call generic code.
    IF lcVrUpd = 'V'
      lcExpr = 'Style+invoice+STR(lineno,6) =lcStyAls.Style'
    ELSE
      lcExpr = 'Style+invoice+STR(lineno,6) =&lcStyAls..Style'
    ENDIF
    
    =lfUpdtLoop(lcInvLAls, lcExpr,;
                "(&lcInvHAls..STATUS = 'C')","&lcInvHAls..cWareCode")  
  ENDIF  && end of update invoice ship quantity.

  IF llRauth
    =ACOPY(laAuthArr,laUpdatArr)  && copy Ra array to alUpdatArr to call generic code.
    IF lcVrUpd = 'V'
      lcExpr = "Style+rano+cra_linno = lcStyAls.Style"
    ELSE
      lcExpr = "Style+rano+cra_linno = &lcStyAls..Style"
    ENDIF
    =lfUpdtLoop(lcRaLnAls,lcExpr,;
                "(&lcRaHdAls..STATUS = 'O')","&lcRaHdAls..cWareCode")  
  ENDIF

  *-- Ret(s) fields.
  IF llRetMemo
    =ACOPY(laMemoArr,laUpdatArr)  && copy credit memo array to alUpdatArr to call generic code.
    IF lcVrUpd = 'V'
      lcExpr = "Style+CrMemo = lcStyAls.Style"
    ELSE
      lcExpr = "Style+CrMemo = &lcStyAls..Style"
    ENDIF
    =lfUpdtLoop(lcRetLnAls,lcExpr,;
                "(&lcRetHdAls..STATUS <> 'V')","&lcRetHdAls..cWareCode")  
  ENDIF

  *-- Another case if new feature added in the future.


  *-- Trace current style.
  IF lcVrUpd='V'
    lcExpr = lcStyAls.STYLE
  ELSE
    lcExpr = &lcStyAls..STYLE
  ENDIF
  =gfTraceKey('STYLE',lcExpr ,'M',lcFilePath,lcCompMods)

ENDSCAN  && end Scan Style file.

IF !llConvert
  = lfCloseThm("Finish Rebalancing Style File for company " + lcCurrComp + " ...")
ENDIF  
*-- end of lfUpdSty.

*!*************************************************************
*! Name      : lfUpdFab
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Rebalance Fabric file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfUpdtLoop,gfThermo
*!*************************************************************
*! Passed Parameters      : ....
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfUpdFab()
*!*************************************************************
*!
FUNCTION lfUpdFab
lnThermNo  = 0		     		 && Initiall Value for thermo Fn.

GO TOP
*E301059 Update OnOrder in location record.
*E301059 intially replace all onorder in fabdye with 0 to accomulate it later. [begin]
IF lnOnOrder=1
  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    SELECT lcFDyAls
  ELSE
  *E301337,1 ASH 10/18/1999 (End)
    SELECT (lcFDyAls)
  ENDIF
  REPLACE ALL ONORDER WITH 0
  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    SELECT lcFabAls
  ELSE
  *E301337,1 ASH 10/18/1999 (End)
    SELECT (lcFabAls)
  ENDIF
  GO TOP
ENDIF
*E301059 intially replace all onorder in fabdye with 0 to accomulate it later. [end]


*-- MAB PUT ONHAND CODE HERE.
*E301261,1 SSH 06/14/99 intially replace all onhand in fabdye with 0 to accomulate it later
IF (llMultWare OR llFabDye) AND lnOnHand = 1
  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    SELECT lcFDyAls
  ELSE
  *E301337,1 ASH 10/18/1999 (End)
    SELECT (lcFDyAls)
  ENDIF
  REPLACE ALL ONHAND WITH 0
  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    SELECT lcFabAls
  ELSE
  *E301337,1 ASH 10/18/1999 (End)
    SELECT (lcFabAls)
  ENDIF

  GO TOP
ENDIF
*E301261,1 SSH(END)
*-- scan fabric file.
lnCurNm1 = 1
*--mhm2001
*-variable to get temp cursor and its index
lcTmpWar = gfTempName()
lcTmpWar2 = gfTempName()
*--mhm2001


SCAN
  IF llConvert
    *E301469,1 AMH [Start]
    IF llPurge
      lnThermNo = lnThermNo + 1
      lcTermoMsg = "Rebalancing Company " + lcCurrComp + " Fabric/Color:  "
      = gfThermo(lnUpThermo,lnThermNo,lcTermoMsg,FABRIC+'/'+COLOR)
    ELSE
      lcMFleDesc = 'Rebalance Materials...'
      SHOW GET lcMFleDesc
      =lfThrmo(lnCurNm1,lnUpThermo,'123')
    ENDIF
    *E301469,1 AMH [End]
  ELSE
    lnThermNo = lnThermNo + 1
    lcTermoMsg = "Rebalancing Company " + lcCurrComp + " Fabric/Color:  "
    = gfThermo(lnUpThermo,lnThermNo,lcTermoMsg,FABRIC+'/'+COLOR)
  ENDIF  

  lcFabLines = IIF(Make,lcMfHdAls,lcPoLnAls)

  *-- if end of transaction file.
  IF EOF(lcFabLines)
    IF lnOnOrder = 1
      REPLACE ONORDER  WITH 0
    ENDIF  

    *E301261,1 SSH 06/14/99 intially replace all onhand in fabric with 0 to accomulate it later
    *E301337,1 ASH 10/18/1999 (Begin) Commented because we do this for only the fabdye file not the fabric file.
    *IF !llFabDyeFl AND lnOnHand = 1
      *REPLACE ONHAND WITH 0
    *ENDIF
    *E301337,1 ASH 10/18/1999 (End)
    *E301261,1 SSH(END)
  ELSE
    STORE 0 TO lnOn_Order
    SELECT (lcFabLines)
    *-- Make Fabric.
    *wab - commet the fllowing line and let the implemation of the purchased fabric working for manufactred fabric 
    IF lcFabLines = lcMfHdAls
    *  SELECT (lcMfHdAls)
    *  *-- Scan header file.
    *  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    *  IF lcVrUpd='V'
    *    lcFabExp = lcFabAls.Fabric
    *  ELSE
    *  *E301337,1 ASH 10/18/1999 (End)
    *    lcFabExp = &lcFabAls..Fabric
    *  ENDIF
    *  SCAN REST WHILE cfabric+cmfgordno = lcFabExp ;
    *       FOR !(Status $ 'CX')
    *    SELECT (lcMfLnAls)
    *    lcLastClr = Color
    *    *-- Scan detail file.
    *    SCAN REST WHILE cmfgordno+cfabric+color+dyelot+trancd = &lcMfHdAls..cmfgordno + &lcMfHdAls..CFabric
    *      lnUpOnOrd = 0      &&E301059 Variable used to update fabdye record.
    *      
    *      IF Color <> lcLastClr
    *        
    *        IF lnOnOrder=1
    *          *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    *          *lnOn_Order = IIF(&lcFabAls..CONV<>0, ROUND(lnOn_Order * &lcFabAls..CONV,1), lnOn_Order)
    *          *REPLACE &lcFabAls..ONORDER WITH lnOn_Order
    *          IF lcVrUpd = 'V'
    *            lnConv = lcFabAls.Conv
    *          ELSE
    *            lnConv = &lcFabAls..CONV
    *          ENDIF
    *          *E301337,1 ASH 10/18/1999 (End)
    *          lnOn_Order = IIF(lnConv<> 0 , ROUND(lnOn_Order * lnConv,1), lnOn_Order)
    *          IF lcVrUpd='V'
    *            lcRepExp = "lcFabAls.OnOrder WITH lnOn_Order"
    *          ELSE
    *            lcRepExp = "&lcFabAls..ONORDER WITH lnOn_Order"
    *          ENDIF
    *          REPLACE &lcRepExp
    *         
    *         *E301059 update fabdye in Single location company...[begin]
    *         *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    *         *IF !llFabDyeFl AND (lnOn_Order <> 0) AND SEEK(&lcFabAls..Fabric+&lcFabAls..Color,lcFDyAls)
    *         IF lcVrUpd='V'
    *           lcExpr = lcFabAls.Fabric+lcFabAls.Color
    *         ELSE
    *           lcExpr = &lcFabAls..Fabric+&lcFabAls..Color
    *         ENDIF
    *         *E301337,1 ASH 10/18/1999 (End)
    *         IF !llFabDyeFl AND (lnOn_Order <> 0) AND SEEK(lcExpr,IIF(lcVrUpd='V','lcFDyAls',lcFDyAls))
    *           =lfUpFabDye('ONORDER',lnOn_Order)
    *         ENDIF
    *         *E301059 update fabdye in Single location company...[end]
    *         
    *         
    *          lnOn_Order = 0  && Start new accomulate on it.
    *        ENDIF
    *      
    *      ENDIF
    *      *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    *      *lcCurrent = &lcFabAls..FABRIC+&lcFabAls..COLOR
    *      *IF lcVrUpd='V'
    *      *  lcCurrent = lcFabAls.FABRIC+lcFabAls.COLOR
    *      *ELSE
    *      *  lcCurrent = &lcFabAls..FABRIC+&lcFabAls..COLOR
    *      *ENDIF
    *      *E301337,1 ASH 10/18/1999 (End)
    *      *IF SEEK(cFabric+Color,IIF(lcVrUpd='V','lcFabAls',lcFabAls))
    *        IF Trancd = '1'
    *          IF lnOnOrder = 1
    *            lnOn_Order = lnOn_Order + MAX( nMfgTotQty  , 0 )
    *
    *            *E301059 Check for existance of Fabdye record to update onorder. [begin]
    *            *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    *            *IF llFabDyeFl AND SEEK(&lcFabAls..Fabric+&lcFabAls..Color+&lcMfHdAls..cWareCode,lcFDyAls)
    *            IF lcVrUpd ='V'
    *              lcExpr = lcFabAls.Fabric+lcFabAls.Color+&lcMfHdAls..cWareCode
    *            ELSE
    *              lcExpr = &lcFabAls..Fabric+&lcFabAls..Color+&lcMfHdAls..cWareCode
    *            ENDIF
    *            
    *            IF llFabDyeFl AND SEEK(lcExpr,IIF(lcVrUpd='V','lcFDyAls',lcFDyAls))
    *              IF lcVrUpd = 'V'
    *                lnConv = lcFabAls.Conv
    *              ELSE
    *                lnConv = &lcFabAls..CONV
    *              ENDIF
    *              *E301337,1 ASH 10/18/1999 (End)
    *              lnUpOnOrd = IIF(lnConv=0, MAX( nMfgTotQty , 0 ),;
    *                              ROUND(MAX( nMfgTotQty , 0 ) * lnConv,1))
    *            ENDIF
    *            *E301059 Check for existance of Fabdye record to update onorder. [end]
    *          ENDIF  
    *        ELSE && Receive, damage, Or Cancel.
    *          IF lnOnOrder = 1
    *            lnOn_Order = MAX( lnOn_Order - ABS( nMfgTotQty ) , 0 )
    *            *E301059 Check for existance of Fabdye record to update onorder. [begin]
    *            *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    *            IF lcVrUpd = 'V'
    *              lcExpr = lcFabAls.Fabric+lcFabAls.Color+&lcMfHdAls..cWareCode
    *            ELSE
    *              lcExpr = &lcFabAls..Fabric+&lcFabAls..Color+&lcMfHdAls..cWareCode
    *            ENDIF
    *            IF llFabDyeFl AND SEEK(lcExpr,IIF(lcVrUpd ='V','lcFDyAls',lcFDyAls))
    *              IF lcVrUpd = 'V'
    *                lnConv = lcFabAls.Conv
    *              ELSE
    *                lnConv = &lcFabAls..CONV
    *              ENDIF
    *              *E301337,1 ASH 10/18/1999 (End)
    *              lnUpOnOrd = -1 * IIF(lnConv=0, MAX( nMfgTotQty , 0 ),;
    *                               ROUND(MAX( nMfgTotQty , 0 ) * lnConv,1))
    *            ENDIF
    *            *E301059 Check for existance of Fabdye record to update onorder. [end]
    *          ENDIF
    *        ENDIF
    *      *ELSE
    *      *  = SEEK(lcCurrent,lcFabAls)
    *      *ENDIF  
    *     *E301059 if value not equal zero, means that we are on fabdye record. [begin]
    *     IF lnUpOnOrd <> 0
    *       =lfUpFabDye('ONORDER',lnUpOnOrd)
    *     ENDIF  
    *     *E301059 if value not equal zero, means that we are on fabdye record. [end]
    *      lcLastClr = Color
    *    ENDSCAN   
    *    
    *    *-- Update last fabric/Color in the group.
    *    IF lnOnOrder=1
    *      IF lcVrUpd = 'V'
    *        lnConv = lcFabAls.Conv
    *      ELSE
    *        lnConv = &lcFabAls..CONV
    *      ENDIF
    *      lnOn_Order = IIF(lnConv<>0, ROUND(lnOn_Order * lnConv ,1), lnOn_Order)
    *      IF lcVrUpd ='V'
    *        lcRepExp = "lcFabAls.ONORDER WITH lnOn_Order"
    *      ELSE
    *        lcRepExp = "&lcFabAls..ONORDER WITH lnOn_Order"
    *      ENDIF
    *      REPLACE &lcRepExp
    *      lnOn_Order = 0
    *    ENDIF
    *  ENDSCAN     
*--mhm2000    
      *-- scan Mf lines .
      
      IF lcVrUpd ='V'
        lcFabExp = lcFabAls.Fabric
      ELSE
        lcFabExp = &lcFabAls..Fabric
      ENDIF

      IF lcVrUpd ='V'
        lcColExp = lcFabAls.Color
      ELSE
        lcColExp = &lcFabAls..Color
      ENDIF
      *--create cursor in case of multi location
      =lfCreatCr()
      *--scan for header
      SELECT (lcMfHdAls)

      SCAN REST WHILE cFabric = lcFabExp 
        
        IF !(lmultiware)
          SELECT (lcMfLnAls)
          lnUpOnOrd = 0      &&E301059 Variable used to update fabdye record.
          SCAN REST WHILE cMfgOrdNo+cfabric+color+dyelot+trancd = &lcMfHdAls..cMfgOrdNo;
               FOR !(&lcMfHdAls..STATUS $ 'BCX') AND color = lcColExp
            IF TRANCD = '1'    && Original line in PO.
              IF lnOnOrder=1
                lnOn_Order = lnOn_Order +  MAX( nMfgTotQty, 0 )
                IF llFabDyeFl AND SEEK(cFabric+Color+cWareCode,IIF(lcVrUpd='V','lcFDyAls',lcFDyAls))
                  IF lcVrUpd ='V'
                    lnConv = lcFabAls.Conv
                  ELSE
                    lnConv = &lcFabAls..CONV
                  ENDIF
                  lnUpOnOrd = lnUpOnOrd + IIF(lnConv=0,  MAX( nMfgTotQty , 0 ),;
                                 ROUND( MAX( nMfgTotQty , 0 ) * lnConv,1))
                ENDIF
              ENDIF
              *-- MAB PUT ONHAND CODE HERE.
           ELSE  && Received, damage, or cancelled.
              IF lnOnOrder=1
                lnOn_Order = MAX( lnOn_Order - ABS( nMfgTotQty ) , 0 )
                IF llFabDyeFl AND SEEK(cFabric+Color+&lcMfHdAls..cWarecode,IIF(lcVrUpd='V','lcFDyAls',lcFDyAls))
                  IF lcVrUpd ='V'
                    lnConv = lcFabAls.Conv
                  ELSE
                    lnConv = &lcFabAls..Conv
                  ENDIF
                  lnUpOnOrd =lnUpOnOrd+ IIF(lnConv=0 , -1 *  MAX( nMfgTotQty , 0 ),;
                                 ROUND(-1 *  MAX( nMfgTotQty , 0 ) * lnConv,1))
                ENDIF                  
              ENDIF
            ENDIF 
          ENDSCAN  && end scan Material MFlines.
          *--mhm2000
          IF lnUpOnOrd <> 0 
            =lfUpFabDye('ONORDER',lnUpOnOrd)
            lnUpOnOrd = 0
          ENDIF  
          
        ELSE && handle case tow of multi warehouse
          SELECT (lcTmpWar)
          ZAP  
          SET ORDER TO &lcTmpWar
          SELECT (lcMfLnAls)
          lnUpOnOrd = 0      &&E301059 Variable used to update fabdye record.
          SCAN REST WHILE cMfgOrdNo+cfabric+color+dyelot+trancd = &lcMfHdAls..cMfgOrdNo;
               FOR !(&lcMfHdAls..STATUS $ 'BCX') AND color = lcColExp
            IF TRANCD = '1'    && Original line in PO.
              IF lnOnOrder=1
                lnOn_Order = lnOn_Order +  MAX( nMfgTotQty, 0 )
                IF llFabDyeFl AND SEEK(cFabric+Color+cWareCode,IIF(lcVrUpd='V','lcFDyAls',lcFDyAls))
                  IF lcVrUpd ='V'
                    lnConv = lcFabAls.Conv
                  ELSE
                    lnConv = &lcFabAls..CONV
                  ENDIF
                  lnUpOnOrd = IIF(lnConv=0,  MAX( nMfgTotQty , 0 ),;
                                 ROUND( MAX( nMfgTotQty , 0 ) * lnConv,1))
                                 
                  IF lnUpOnOrd <> 0 AND !SEEK(cFabric+Color+cWarecode,lcTmpWar)
                    SELECT (lcTmpWar)
                    APPEND BLANK
                    REPLACE cFABRIC       WITH &lcMfLnAls..cFabric,;
                            cCOLOR        WITH &lcMfLnAls..COLOR,;
                            cWarecode    WITH &lcMfLnAls..cWarecode,;
                            nOrgQty      WITH &lcMfLnAls..nMfgTotQty ,;
                            nWipQty      WITH lnUpOnOrd  ,;
                            nLineNo      WITH &lcMfLnAls..LineNo
                    lnUpOnOrd = 0
                  ENDIF  
                ENDIF
              ENDIF
              *-- MAB PUT ONHAND CODE HERE.
           ELSE  && Received, damage, or cancelled.
              IF lnOnOrder=1
                lnOn_Order = MAX( lnOn_Order - ABS( nMfgTotQty ) , 0 )
                IF llFabDyeFl AND SEEK(cFabric+Color+&lcMfHdAls..cWarecode,IIF(lcVrUpd='V','lcFDyAls',lcFDyAls))
                  IF lcVrUpd ='V'
                    lnConv = lcFabAls.Conv
                  ELSE
                    lnConv = &lcFabAls..Conv
                  ENDIF
                  lnUpOnOrd =lnUpOnOrd+ IIF(lnConv=0 , -1 *  MAX( nMfgTotQty , 0 ),;
                                 ROUND(-1 *  MAX( nMfgTotQty , 0 ) * lnConv,1))
                  lcLineno = LineNo               
                  IF lnUpOnOrd <> 0 
                     IF SEEK(cFabric+Color+cWarecode,lcTmpWar)
                       SELECT (lcTmpWar)
                       REPLACE nWipQty      WITH lnUpOnOrd + nWipQty
                     ELSE
                       SELECT (lcTmpWar)
                       SET ORDER TO TAG lcTmpWar2
                       =SEEK(lcLineno)    
                       REPLACE nWipQty      WITH lnUpOnOrd + nWipQty
                     ENDIF
                     lnUpOnOrd = 0
                   ENDIF  
                ENDIF                  
              ENDIF
            ENDIF 
          ENDSCAN  && end scan Material MFlines.
          *--mhm2000
          SELECT (lcTmpWar)
          LOCATE
          IF !EOF()
            SCAN
              =SEEK(cFabric+cColor+cWarecode,IIF(lcVrUpd='V','lcFDyAls',lcFDyAls))
              =lfUpFabDye('ONORDER',&lcTmpWar..nWipQty)
            ENDSCAN 
          ENDIF  

        ENDIF    
        
      *--mhm2000   
      ENDSCAN    && end scan Material MF Header.
      
      IF lcVrUpD = 'V'
        SELECT lcFabAls
      ELSE
        SELECT (lcFabAls)
      ENDIF  
      IF lnOnOrder=1
        lnOn_Order = IIF(Conv<>0, ROUND(lnOn_Order * Conv,1), lnOn_Order)
        IF ONORDER <> lnOn_Order
          IF lcVrUpD = 'V'
            DECLARE laRebMsg[3]
            laRebMsg[1] = " "
            laRebMsg[2] = "Company" + lcCompCode +": "+ Fabric+'/'+Color+" has wrong ON ORDER quantity."
            laRebMsg[3] = " "
            =lfVryRport()
          ENDIF
          REPLACE ONORDER WITH lnOn_Order
        ENDIF

        IF !llFabDyeFl AND (lnOn_Order <> 0) AND SEEK(Fabric+Color,IIF(lcVrUpD = 'V','lcFDyAls',lcFDyAls))
          =lfUpFabDye('ONORDER',lnOn_Order)
        ENDIF
      ENDIF

    *--mhm200
    ELSE
      *-- scan Po lines .
      lcCalOper = [IIF(cMatType='P', 1,-1) *]  && Transaction sign.
      *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
      IF lcVrUpd ='V'
        lcFabExp = lcFabAls.Fabric+lcFabAls.Color
      ELSE
        lcFabExp = &lcFabAls..Fabric+&lcFabAls..Color
      ENDIF
      *E301337,1 ASH 10/18/1999 (End)
      *--mhm2000
      *SCAN REST WHILE Fabric+Color = lcFabExp ;
      *     FOR cMatType <> 'C' AND !(&lcPoHdAls..STATUS $ 'CX')
      *--mhm2000 we move variable here to initialize for each PO
      lnUpOnOrd = 0      &&E301059 Variable used to update fabdye record.
      lcPoMat = &lcPoHdAls..PoMat
      SCAN REST WHILE Fabric+Color = lcFabExp ;
           FOR cMatType <> 'C' AND !(&lcPoHdAls..STATUS $ 'BCX')
           
        IF lnUpOnOrd <> 0 AND (&lcPoHdAls..PoMat <> lcPoMat)
          =lfUpFabDye('ONORDER',lnUpOnOrd)
          lnUpOnOrd = 0
          lcPoMat = &lcPoHdAls..PoMat 
        ENDIF  
                 
        *lnUpOnOrd = 0      &&E301059 Variable used to update fabdye record.
        *--mhm2000

        IF TRANCD = '1'    && Original line in PO.
          IF lnOnOrder=1
            lnOn_Order = lnOn_Order + &lcCalOper. MAX( nFabTotQty , 0 )
            *E301059 Check for existance of Fabdye record to update onorder. [begin]
            *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
            *IF llFabDyeFl AND SEEK(Fabric+Color+cWareCode,lcFDyAls)
            IF llFabDyeFl AND SEEK(Fabric+Color+cWareCode,IIF(lcVrUpd='V','lcFDyAls',lcFDyAls))
              IF lcVrUpd ='V'
                lnConv = lcFabAls.Conv
              ELSE
                lnConv = &lcFabAls..CONV
              ENDIF
              *E301337,1 ASH 10/18/1999 (End)
              *--mhm2000 
              *lnUpOnOrd = IIF(lnConv=0, &lcCalOper. MAX( nFabTotQty , 0 ),;
                             ROUND(&lcCalOper. MAX( nFabTotQty , 0 ) * lnConv,1))
              lnUpOnOrd = lnUpOnOrd + IIF(lnConv=0, &lcCalOper. MAX( nFabTotQty , 0 ),;
                             ROUND(&lcCalOper. MAX( nFabTotQty , 0 ) * lnConv,1))
              *--mhm2000 
                             
            ENDIF
            *E301059 Check for existance of Fabdye record to update onorder. [end]
          ENDIF
          *-- MAB PUT ONHAND CODE HERE.
        ELSE  && Received, damage, or cancelled.
          IF lnOnOrder=1
            lnOn_Order = MAX( lnOn_Order - &lcCalOper. ABS( nFabTotQty ) , 0 )
            *E301059 Check for existance of Fabdye record to update onorder. [begin]
            *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
            *IF llFabDyeFl AND SEEK(Fabric+Color+cWareCode,lcFDyAls)
            *wab
            *IF llFabDyeFl AND SEEK(Fabric+Color+cWareCode,IIF(lcVrUpd='V','lcFDyAls',lcFDyAls))
            IF llFabDyeFl AND SEEK(Fabric+Color+&lcPoHdAls..cWarecode,IIF(lcVrUpd='V','lcFDyAls',lcFDyAls))
            *wab
              IF lcVrUpd ='V'
                lnConv = lcFabAls.Conv
              ELSE
                lnConv = &lcFabAls..Conv
              ENDIF
              *E301337,1 ASH 10/18/1999 (End)
              *--mhm2000
              *lnUpOnOrd = IIF(lnConv=0 , -1 * &lcCalOper. MAX( nFabTotQty , 0 ),;
                             ROUND(-1 * &lcCalOper. MAX( nFabTotQty , 0 ) * lnConv,1))
              lnUpOnOrd = lnUpOnOrd + IIF(lnConv=0 , -1 * &lcCalOper. MAX( nFabTotQty , 0 ),;
                             ROUND(-1 * &lcCalOper. MAX( nFabTotQty , 0 ) * lnConv,1))
              *--mhm2000

            ENDIF                  
            *E301059 Check for existance of Fabdye record to update onorder. [end]
          ENDIF
        ENDIF 
        *E301059 if value not equal zero, means that we are on fabdye record. [begin]
        
        *--mhm2000
        *IF lnUpOnOrd <> 0
        *  =lfUpFabDye('ONORDER',lnUpOnOrd)
        *ENDIF  
        *--mhm2000
        
        *E301059 if value not equal zero, means that we are on fabdye record. [end]
      ENDSCAN  && end scan Material Po lines.
      *--mhm2000
      IF lnUpOnOrd <> 0 
        SKIP-1
        =lfUpFabDye('ONORDER',lnUpOnOrd)
        lnUpOnOrd = 0
        SKIP
      ENDIF  
      *--mhm2000   
      
      *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
      IF lcVrUpD = 'V'
        SELECT lcFabAls
      ELSE
        SELECT (lcFabAls)
      ENDIF  
      *E301337,1 ASH 10/18/1999 (End)
      IF lnOnOrder=1
        lnOn_Order = IIF(Conv<>0, ROUND(lnOn_Order * Conv,1), lnOn_Order)
        *E301337,1 ASH 10/18/1999 (Begin) Add the wrong fabrics to the log file.
        IF ONORDER <> lnOn_Order
          IF lcVrUpD = 'V'
            DECLARE laRebMsg[3]
            laRebMsg[1] = " "
            laRebMsg[2] = "Company" + lcCompCode +": "+ Fabric+'/'+Color+" has wrong ON ORDER quantity."
            laRebMsg[3] = " "
            =lfVryRport()
          ENDIF
          *E301337,1 ASH 10/18/1999 (End)
          REPLACE ONORDER WITH lnOn_Order
        ENDIF

        *E301059 update fabdye in Single location company...
        IF !llFabDyeFl AND (lnOn_Order <> 0) AND SEEK(Fabric+Color,IIF(lcVrUpD = 'V','lcFDyAls',lcFDyAls))
          =lfUpFabDye('ONORDER',lnOn_Order)
        ENDIF
      ENDIF
    ENDIF  && end of make/imported fabrics if.
    *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    IF lcVrUpD = 'V'
      SELECT lcFabAls
    ELSE
      SELECT (lcFabAls)
    ENDIF  
    IF lcVrUpd = 'V' AND lnOnOrder=1 
      IF SEEK(lcFabAls.Fabric+lcFabAls.Color,'lcFDyAls')
        SELECT lcFDyAls
        SCAN WHILE Fabric+Color = lcFabAls.Fabric+lcFabAls.Color
          IF OnOrder <> FabDye.OnOrder
            DECLARE laRebMsg[3]
            laRebMsg[1] = " "
            laRebMsg[2] = "Company" + lcCompCode +": "+ Fabric+"/"+Color+" has wrong ONORDER quantity."
            laRebMsg[3] = " "
            =lfVryRport()
            EXIT
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF  
  ENDIF
    
 *E301337,1 ASH 10/18/1999 (End)

  

  *E301261,1 SSH 06/14/99 Start rebalance onhand.
  IF lnOnHand = 1
    DIMENSION laONHAND[2] , laLocONHAND[2]
    DIMENSION laOnhArr[1,3]
    STORE 0 TO laONHAND,laLocONHAND,lnLocOnHnd
    DIMENSION laUpdatArr[2,4]        && array used to update stydye file.
    *-- laStkArr : Array hold Stock arrays information.
    laOnhArr[1,1] = 'lnONHAND=1'
    laOnhArr[1,2] = 'laLocONAHDN'
    laOnhArr[1,3] = 'ONHAND'
    =lfMatOnHnd()
  ENDIF
  *E301261,1 SSH (END)          
  *-- trace fabric file.
  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    lcExpr = lcFabAls.Fabric
  ELSE
    lcExpr = &lcFabAls..FABRIC
  ENDIF
  *=gfTraceKey('FABRIC',&lcFabAls..FABRIC,'M',lcFilePath,lcCompMods)
  =gfTraceKey('FABRIC',lcExpr,'M',lcFilePath,lcCompMods)
  *E301337,1 ASH 10/18/1999 (End)
ENDSCAN
IF !llConvert
  = lfCloseThm("Finish Rebalancing Fabric File for company " + lcCurrComp + " ...")
ENDIF  
*-- end of lfUpdFab.

*!*************************************************************
*! Name      : lfCloseThm
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Assure closing thermometer.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfThermo
*!*************************************************************
*! Passed Parameters      : ....
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfCloseThm()
*!*************************************************************
*!
FUNCTION lfCloseThm
PARAMETERS lcThemoTlt
*-- Asure that Thermometer was closed.
IF BETWEEN(lnThermNo,0,lnUpThermo)
  FOR lnCloseThm = lnThermNo TO lnUpThermo
    =gfThermo(lnUpThermo,lnCloseThm,lcThemoTlt," ")
  ENDFOR
ENDIF
*-- end of lfCloseThm.

*!*************************************************************
*! Name      : lfUpStyDye
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Update stydye record for all rebalance cases except
*!           : work process cases.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpRepStyDy
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : 1- Style code
*!                        : 2- warehouse code
*!                        : 3- dyelot
*!                        : 4- Name of field to replace
*!                        : 5- Name of total field to replace
*!                        : 6- Name of field to replace from it.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfUpStyDye()
*!*************************************************************
*!
FUNCTION lfUpStyDye
PARAMETERS lcUpSty,lcUpWare,lcUpDye,lcFieldNam,lcTotNam,lcRepFrNam

*-- if stydye file is found .
IF llStyDyeFl AND (TYPE('lcUpSty')  = 'C') AND !EMPTY(lcUpSty)  AND ;
                  (TYPE('lcUpWare') = 'C') AND !EMPTY(lcUpWare)
  
  lcUpSty  = PADR(lcUpSty,19)
  lcUpWare = PADR(lcUpWare,6)
  
  IF TYPE('lcUpDye') $ 'UL'
    lcUpDye = SPACE(10)
  ENDIF
  lcUpDye = PADR(lcUpDye,10)

  *-- Update Location Record if found.
   *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    llExpr = SEEK(lcUpSty+lcUpWare+SPACE(10),'lcSDyAls')
  ELSE
    llExpr = SEEK(lcUpSty+lcUpWare+SPACE(10),lcSDyAls)
  ENDIF

  *B603992,1 ASH 07/11/2002 (Begin) Add missing record in STYDYE.
  IF !llExpr AND lcVrUpd = 'U'
    =lfAddRec(lcUpSty,lcUpWare,0)
    llExpr = .T.
  ENDIF
  *B603992,1 ASH 07/11/2002 (End)
  
  IF llExpr
   *E301337,1 ASH 10/18/1999 (End)
    PRIVATE lcCurAlias
    lcCurAlias = SELECT(0)
    IF lcVrUpd = 'V'
      SELECT lcSDyAls
    ELSE
      SELECT (lcSDyAls)
    ENDIF
    DO lpRepStyDy WITH lcFieldNam,lcTotNam,lcRepFrNam  && Replace current record with passed values.
  
    *-- Update dyelot record if found.
    IF llDyelot AND !EMPTY(lcUpDye) AND SEEK(lcUpSty+lcUpWare+lcUpDye)
      DO lpRepStyDy WITH lcFieldNam,lcTotNam,lcRepFrNam
    ENDIF
  
    SELECT (lcCurAlias)
  ENDIF
ENDIF
*-- end of Update Stydye file.

*!*************************************************************
*! Name      : lpRepStyDy
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Update stydye record with passed values
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : 1- Name of field to replace
*!                        : 2- Name of total field to replace
*!                        : 3- Name of field to replace from it.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfUpStyDye()
*!*************************************************************
*!
PROCEDURE lpRepStyDy
PARAMETERS lcFieldNam,lcTotNam,lcRepFrNam
REPLACE (lcFieldNam+'1') WITH EVALUATE(lcFieldNam+'1') + EVALUATE(lcRepFrNam+'1') ,;
        (lcFieldNam+'2') WITH EVALUATE(lcFieldNam+'2') + EVALUATE(lcRepFrNam+'2') ,;
        (lcFieldNam+'3') WITH EVALUATE(lcFieldNam+'3') + EVALUATE(lcRepFrNam+'3') ,;
        (lcFieldNam+'4') WITH EVALUATE(lcFieldNam+'4') + EVALUATE(lcRepFrNam+'4') ,;
        (lcFieldNam+'5') WITH EVALUATE(lcFieldNam+'5') + EVALUATE(lcRepFrNam+'5') ,;
        (lcFieldNam+'6') WITH EVALUATE(lcFieldNam+'6') + EVALUATE(lcRepFrNam+'6') ,;
        (lcFieldNam+'7') WITH EVALUATE(lcFieldNam+'7') + EVALUATE(lcRepFrNam+'7') ,;
        (lcFieldNam+'8') WITH EVALUATE(lcFieldNam+'8') + EVALUATE(lcRepFrNam+'8') ,;
        (lcTotNam      ) WITH EVALUATE(lcTotNam)             +;
        EVALUATE(lcRepFrNam+'1') + EVALUATE(lcRepFrNam+'2')  +;
        EVALUATE(lcRepFrNam+'3') + EVALUATE(lcRepFrNam+'4')  +;
        EVALUATE(lcRepFrNam+'5') + EVALUATE(lcRepFrNam+'6')  +;
        EVALUATE(lcRepFrNam+'7') + EVALUATE(lcRepFrNam+'8')


  *-- Trace current style in stydye file if not traced before.
   *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    lcExpr = lcSDyAls.STYLE+lcSDyAls.CWARECODE+lcSDyAls.DYELOT
  ELSE
    lcExpr = &lcSDyAls..STYLE+&lcSDyAls..CWARECODE+&lcSDyAls..DYELOT
  ENDIF
   *E301337,1 ASH 10/18/1999 (End)
  =!llStyDyeFl AND gfTraceKey('STYDYE',lcExpr,'M',lcFilePath,lcCompMods)

*-- end of lpRepStyDy.        

*!*************************************************************
*! Name      : lfUpdtLoop
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Update all cases except work process ones.
*!           : note that it can update multiple cases in the same file.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpGathFlds,lfUpStyDye
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : 1- Name of file to scan
*!                        : 2- While condition expression
*!                        : 3- For condition expression
*!                        : 4- Array hold transaction information
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfUpdtLoop()
*!*************************************************************
*!
FUNCTION lfUpdtLoop
PARAMETERS lcScanFile,lcWhilCond,lcForCond,lcUptWare

*-- if no transactions found.
IF EOF(lcScanFile)

  DO lpGathFlds  && Update style record with zeros.

  *-- update stydye record if system does not have multiware and dyelots.
  IF lcVrUpd = 'V'
    llExpr = SEEK(lcStyAls.STYLE,'lcSDyAls')
  ELSE
    llExpr = SEEK(&lcStyAls..STYLE,lcSDyAls)
  ENDIF
  IF !llStyDyeFl AND llExpr
    IF lcVrUpd = 'V'
      SELECT lcSDyAls
    ELSE
      SELECT (lcSDyAls)
    ENDIF
    DO lpGathFlds  && Update stydye record with zeros.
    IF lcVrUpd = 'V'
      SELECT lcStyAls
    ELSE
      SELECT (lcStyAls)
    ENDIF
  ENDIF

ELSE

  SELECT (lcScanFile)
  SCAN REST WHILE &lcWhilCond FOR &lcForCond

    *-- loop all databases to rebalance it.
    FOR lnAllTrans = 1 TO ALEN(laUpdatArr,1)

      IF EMPTY(laUpdatArr[lnAllTrans,1])
        EXIT
      ENDIF
      
      *-- if current database condition is true.
      IF EVALUATE(laUpdatArr[lnAllTrans,1])
        *-- accomulate transactions in database array.
        FOR lnJ = 1 To 8
          lcJ = STR(lnJ,1)
          &laUpdatArr[lnAllTrans,2].[lnJ] = &laUpdatArr[lnAllTrans,2].[lnJ] + &laUpdatArr[lnAllTrans,4].&lcJ
        ENDFOR
        &laUpdatArr[lnAllTrans,2].[9] = &laUpdatArr[lnAllTrans,2].[1] + &laUpdatArr[lnAllTrans,2].[2] +;
                                        &laUpdatArr[lnAllTrans,2].[3] + &laUpdatArr[lnAllTrans,2].[4] +;
                                        &laUpdatArr[lnAllTrans,2].[5] + &laUpdatArr[lnAllTrans,2].[6] +;
                                        &laUpdatArr[lnAllTrans,2].[7] + &laUpdatArr[lnAllTrans,2].[8]
        
        *-- update stydye record.
        =lfUpStyDye(Style,&lcUptWare,IIF(TYPE('Dyelot') $ "UL",SPACE(10),Dyelot),;
                   laUpdatArr[lnAllTrans,3],;
                   'TOT'+laUpdatArr[lnAllTrans,3],lcScanFile+'.'+ laUpdatArr[lnAllTrans,4])
      ENDIF

    ENDFOR

  ENDSCAN
                               
  IF lcVrUpd = 'V'
    SELECT lcStyAls
  ELSE
    SELECT (lcStyAls)
  ENDIF
  *E301337,1 ASH 10/18/1999 (Begin) Add the wrong style to the log file.
  IF lcVrUpD = 'V' AND lnShp = 1 AND (TotShp <> laShp[9] OR lcSDyAls.TotShp <> StyDye.TotShp)
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = "Company" + lcCompCode +": "+ Style + " has wrong Shipped quantity."
    laRebMsg[3] = " "
    =lfVryRport()
  ENDIF  
  IF lcVrUpD = 'V' AND lnAlo = 1 AND (TotAlo <> laAlo[9] OR lcSDyAls.TotAlo <> StyDye.TotAlo)
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = "Company" + lcCompCode +": "+ Style + " has wrong Allocation quantity."
    laRebMsg[3] = " "
    =lfVryRport()
  ENDIF  
  llError = .F.
  IF lcVrUpD = 'V' AND lnOrd = 1 
    IF TotOrd <> laOrder[9]
      llError = .T.
    ENDIF
    IF SEEK(lcStyAls.Style,'lcSDyAls')
      lcAlias = ALIAS()
      SELECT lcSDyAls
      SCAN WHILE STYLE = lcStyAls.Style
        IF TotOrd <> StyDye.TotOrd
          llError = .T.
        ENDIF
      ENDSCAN
      SELECT &lcAlias 
    ENDIF
    IF llError  
      DECLARE laRebMsg[3]
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcCompCode +": "+ Style + " has wrong Ordered quantity."
      laRebMsg[3] = " "
      =lfVryRport()
    ENDIF
  ENDIF
  
  IF lcVrUpD = 'V' AND lnRa = 1 AND (TotRa <> laRa[9] OR lcSDyAls.TotRa <> StyDye.TotRa )
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = "Company" + lcCompCode +": "+ Style + " has wrong retrun quantity."
    laRebMsg[3] = " "
    =lfVryRport()
  ENDIF  
  IF lcVrUpD = 'V' AND lnRet = 1 AND (TotRet <> laRet[9] OR lcSDyAls.TotRet <> StyDye.TotRet )
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = "Company" + lcCompCode +": "+ Style + " has wrong retrun quantity."
    laRebMsg[3] = " "
    =lfVryRport()
  ENDIF  
   *E301337,1 ASH 10/18/1999 (End)
  DO lpGathFlds  && Update style record.

  *-- update stydye record if system does not have multiware and dyelots.
  IF lcVrUpd = 'V'
    llExpr = SEEK(lcStyAls.STYLE,'lcSDyAls')
  ELSE
    llExpr = SEEK(&lcStyAls..STYLE,lcSDyAls)
  ENDIF
  IF !llStyDyeFl AND llExpr
    IF lcVrUpd = 'V'
      SELECT lcSDyAls
    ELSE
      SELECT (lcSDyAls)
    ENDIF
  
    DO lpGathFlds  && Update stydye record with zeros.

    IF lcVrUpd = 'V'
      SELECT lcStyAls
    ELSE
      SELECT (lcStyAls)
    ENDIF
    
  ENDIF

ENDIF
*-- end of lfUpdtLoop.

*!*************************************************************
*! Name      : lpGathFlds
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Update Style record.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpGathFlds,lfUpStyDye
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : Replace expression.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lpGathFlds()
*!*************************************************************
*!
PROCEDURE lpGathFlds
*E301232,1 Add new parameter to this function to update this replace parameter [Begin]
*PARAMETERS lcNewReplc
*E301232,1 Add new parameter to this function to update this replace parameter [End  ]
*E301261,1 SSH  Add new parameter to this function to update onhand.
PARAMETERS lcNewReplc,lcOnhand
*E301261,1 SSH(END)


FOR lnAllTrans = 1 TO ALEN(laUpdatArr,1)

  IF EMPTY(laUpdatArr[lnAllTrans,1])
    EXIT
  ENDIF
  IF EMPTY(lcOnhand) 
    IF EVALUATE(laUpdatArr[lnAllTrans,1]) AND !EOF()
       GATHER FROM (laUpdatArr[lnAllTrans,2]) FIELDS ;
                  (laUpdatArr[lnAllTrans,3]+'1'),;
                  (laUpdatArr[lnAllTrans,3]+'2'),;
                  (laUpdatArr[lnAllTrans,3]+'3'),;
                  (laUpdatArr[lnAllTrans,3]+'4'),;
                  (laUpdatArr[lnAllTrans,3]+'5'),;
                  (laUpdatArr[lnAllTrans,3]+'6'),;
                  (laUpdatArr[lnAllTrans,3]+'7'),;
                  (laUpdatArr[lnAllTrans,3]+'8'),;
                  ('TOT'+laUpdatArr[lnAllTrans,3])
      *E301232,1 if you pass replace expression, do it [Begin]
      IF TYPE('lcNewReplc') $ "C"
        REPLACE &lcNewReplc
      ENDIF
      *E301232,1 if you pass replace expression, do it [End]
      &laUpdatArr[lnAllTrans,2] = 0  && restore initial value for another accomulate.
    ENDIF
  ELSE
    IF EVALUATE(laUpdatArr[lnAllTrans,1])
       *E301337,1 ASH 10/18/1999 (Begin) Add the wrong fabric to the log file.
      IF OnHand <> lnLocOnHnd AND UPPER(ALIAS())='LCFABALS'
        IF lcVrUpD = 'V'
          DECLARE laRebMsg[3]
          laRebMsg[1] = " "
          laRebMsg[2] = "Company" + lcCompCode +": "+ Fabric+"/"+COLOR+" has wrong onhand quantity."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF
         *E301337,1 ASH 10/18/1999 (End)
      ENDIF
      *B802768,1 SSH 09/21/99 If Alias if fabric
      IF lcVrUpd='V'
        lcAlias = 'lcFabAls'
      ELSE
        lcAlias = lcFabAls
      ENDIF
      IF ALIAS() = lcAlias 
        REPLACE ONHAND WITH  lnLocOnHnd
      ELSE
        *B802768,1 SSH (END)
        REPLACE ONHAND WITH  laONHAND
      ENDIF
      IF TYPE('lcNewReplc') $ "C"
        REPLACE &lcNewReplc
      ENDIF
      &laUpdatArr[lnAllTrans,2] = 0  && restore initial value for another accomulate.
    ENDIF
  ENDIF
ENDFOR

IF llStyFile AND ALIAS() = IIF(lcVrUpd='V','lcSDyAls',lcSDyAls)
  *-- Trace current style in stydye file if not traced before.
   *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    lcExpr = lcSDyAls.STYLE+lcSDyAls.CWARECODE+lcSDyAls.DYELOT
  ELSE
    lcExpr = &lcSDyAls..STYLE+&lcSDyAls..CWARECODE+&lcSDyAls..DYELOT
  ENDIF
   *E301337,1 ASH 10/18/1999 (End)
  =!llStyDyeFl AND gfTraceKey('STYDYE',lcExpr,'M',lcFilePath,lcCompMods)
ENDIF
*--- end of lpGathFlds.

*!*************************************************************
*! Name      : lfUpdtOper
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Update Work process cases.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpUpDtWork,lfUpStyDye,lpCalGrpDt
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : ....
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfUpdtOper()
*!*************************************************************
*!
FUNCTION lfUpdtOper

*-- if there no records in transaction file.
IF EOF(lcWorkFile)
  
  IF lnWip = 1 AND !llAdoDye
    DO lpUpDtWork WITH laWip,'WIP','TOTWIP'            && Update WIP

    *-- update stydye record if system does not have multiware and dyelots.
    IF lcVrUpd = 'V'
      llExpr = SEEK(lcStyAls.STYLE,'lcSDyAls')
    ELSE
      llExpr = SEEK(&lcStyAls..STYLE,lcSDyAls)
    ENDIF  
    IF !llStyDyeFl AND llExpr
      IF lcVrUpd = 'V'
        SELECT lcSDyAls
      ELSE
        SELECT (lcSDyAls)
      ENDIF
      DO lpUpDtWork WITH laWip,'WIP','TOTWIP'            && Update WIP
      IF lcVrUpd = 'V'
        SELECT lcStyAls
      ELSE
        SELECT (lcStyAls)
      ENDIF
    ENDIF
    
  ENDIF

  IF lnWo = 1 AND !llAdoDye AND (lcWorkFile <> lcWipJAls)
    DO lpUpDtWork WITH laWo,'NWO','NTOTWO'             && Update now

    *-- update stydye record if system does not have multiware and dyelots.
     *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    IF lcVrUpd = 'V'
      llExpr = SEEK(lcStyAls.STYLE,'lcSDyAls')
    ELSE
      llExpr = SEEK(&lcStyAls..STYLE,lcSDyAls)
    ENDIF
     *E301337,1 ASH 10/18/1999 (End)
    IF !llStyDyeFl AND llExpr
      IF lcVrUpd = 'V'
        SELECT lcSDyAls
      ELSE
        SELECT (lcSDyAls)
      ENDIF
      DO lpUpDtWork WITH laWo,'NWO','NTOTWO'             && Update nwo
      IF lcVrUpd = 'V'
        SELECT lcStyAls
      ELSE
        SELECT (lcStyAls)
      ENDIF
    ENDIF
  ENDIF
  IF lnIntrans = 1 AND (lcWorkFile = lcPoLAls)
    DO lpUpDtWork WITH laIntrans,'INTRANS','TOTINTRN'  && Update Intrans

    *-- update stydye record if system does not have multiware and dyelots.
    IF lcVrUpd = 'V'
      llExpr = SEEK(lcStyAls.STYLE,'lcSDyAls')
    ELSE
      llExpr = SEEK(&lcStyAls..STYLE,lcSDyAls)    
    ENDIF
    IF !llStyDyeFl AND llExpr
      IF lcVrUpd = 'V'
        SELECT lcSDyAls
      ELSE
        SELECT (lcSDyAls)
      ENDIF
      DO lpUpDtWork WITH laIntrans,'INTRANS','TOTINTRN'  && Update Intrans
      IF lcVrUpd = 'V'
        SELECT lcStyAls
      ELSE
        SELECT (lcStyAls)
      ENDIF
    ENDIF
    
  ENDIF

ELSE  && Find transactions in current file.

  SELECT (lcWorkFile)
  *-- If Wip Only from W.I.P. adjustment file.
  IF lcWorkFile = lcWipJAls

    *-- Case of Base module only or Manufacturing module for imported
    *-- styles, Scan the WIP adjustment file
    IF lcVrUpd = 'V'
      lcExpr = lcStyAls.Style
    ELSE
      lcExpr = &lcStyAls..Style
    ENDIF
    SCAN WHILE Style+cWareCode = lcExpr
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        laWip[lnCount] = laWip[lnCount] + Adj&lcCount
      ENDFOR
    ENDSCAN

    *-- Update stydye file if there is warehouse records.
    =lfUpStyDye(Style,cWareCode,SPACE(10),'WIP','TOTWIP','&lcWipJAls..ADJ')

  ELSE  && Find Transaction file.
  
    *-- Save current record values to update it.
    lcStyle    = Style
    lcLastType = IIF(lcWorkFile = lcCutLnAls,'',cStyType)
    lcSource   = IIF(lcWorkFile = lcCutLnAls,'',Vendor)
    lcGrpWare  = cWareCode
    *B605618,1 ASH 03/21/2002 (Begin) Don't compute the WIP based on location to fix the probelm of wrong computing
    *B605618,1                        WIP if the PO recieved on more than on location.
    IF lnWip=1 AND '+PADR(cWareCode,6)' $ lcBatChng
      lcBatChng = STRTRAN(lcBatChng,'+PADR(cWareCode,6)')
    ENDIF
    *B605618,1 ASH 03/21/2002 (End)
    lcKey      = &lcBatChng

    *-- Scan Master Transaction file (Cuttktl OR Posln) which ordered by Style.
    *B603992,1 ASH 07/11/2002 (Begin) To include all the records matches the for condition.
    *SCAN REST WHILE &lcWhilExpr FOR &lcForExpr
    SCAN REST WHILE &lcWhilExpr 
      llExVal = EVAL(lcForExpr)
      IF !llExVal 
        LOOP 
      ENDIF
    *B603992,1 ASH 07/11/2002 (End)
      *-- if current group was changed.
      IF lcKey <> &lcBatChng
        
        *-- adjust current used arrays and update stydye record for this group.
        DO lpCalGrpDt WITH lcGrpWare,IIF(lnWip=1,'laTWip',''),IIF(lnWo=1,'laTWo1','')

        *-- if inter-location case OR Adornment Order.
        IF (lcWorkFile = lcPoLAls) AND lcLastType $ 'AN' 
          DO lpCalGrpDt WITH lcSource,IIF(lnWip=1,'laSWip',''),IIF(lnWo=1,'laSWo1','')
        ENDIF  
      
      ENDIF

      *-- if Make Style.
      IF lcWorkFile = lcCutLnAls
        
        *-- If original line.
        IF Trancd = '1'
          lcStyle   = Style
          lcGrpWare = cWareCode
          FOR lnW = 1 TO 8
            lcW = STR(lnW,1)
          
            *-- Accomulate Wip array.
            IF lnWip=1
              laTWip[lnW] = laTWip[lnW] + Qty&lcW
            ENDIF
              
            *-- Accomulate nWo array.
            IF lnWo = 1
              laTWo1[lnW] = laTWo1[lnW] + Qty&lcW
            ENDIF
              	
          ENDFOR
          
        ELSE  && Received, damage or canceled line.

          FOR lnW = 1 TO 8
            lcW = STR(lnW,1)
            IF lnWip=1
              laTWip[lnW] = MAX(laTWip[lnW] - Qty&lcW,0)
            ENDIF
          ENDFOR

        ENDIF  && end If original line.
    
      ELSE  && P/O module (Imported Style).
        
        lnTranSign = IIF(cStyType='R',-1,1)  && Transcation sign [Reverse sign in Return Case]
        
        *-- Transaction code cases.
        DO CASE
        
          CASE Trancd = '1'
            
            lcStyle    = Style
            lcLastType = cStyType
            lcSource   = Vendor
            lcGrpWare  = cWareCode

            FOR lnW = 1 TO 8
              lcW = STR(lnW,1)
              
              *-- if want to update wips and this P/O not completed
              IF (lnWip=1) AND &lcPoHAls..STATUS <> 'C'
                laTWip[lnW] = laTWip[lnW] + (lnTranSign * Qty&lcW)  && accomulate target wip array
                
                *-- if Inter-Location lines 
                IF cStyType = 'N' 
                  laSWip[lnW] = laSWip[lnW] - Qty&lcW   && accomulate source wip array
                ENDIF  
              
              ENDIF
              
              IF (lnWo = 1) AND &lcPoHAls..STATUS <> 'C'
                
                laTWo1[lnW] = laTWo1[lnW] + (lnTranSign * Qty&lcW)  && accomulate target nWo array

                *-- if Inter-Location lines.
                IF cStyType = 'N'
                  laSWo1[lnW] = laSWo1[lnW] - Qty&lcW  && accomulate source wip array
                ENDIF

              ENDIF
            ENDFOR

          CASE Trancd $ '245'  && Receive, Damage, Or Canceled
          
            FOR lnW = 1 TO 8
              lcW = STR(lnW,1)

              IF (lnWip=1) AND &lcPoHAls..STATUS <> 'C'
                laTWip[lnW] = laTWip[lnW] - (lnTranSign * Qty&lcW)

                *-- if Inter-Location line OR Adornment Order.
                IF cStyType = 'N' 
                  laSWip[lnW] = laSWip[lnW] + Qty&lcW
                ENDIF  
              
              ENDIF

              *-- if Inter-Location line.
              IF (lnWo = 1)  AND (cStyType = 'N') AND &lcPoHAls..STATUS <> 'C'
                laSWo1[lnW] = laSWo1[lnW] + Qty&lcW
              ENDIF
              
              *-- if this P/O was completed.
              IF (lnWo = 1) AND &lcPoHAls..STATUS = 'C' 
                laTWo1[lnW] = laTWo1[lnW] + (lnTranSign * Qty&lcW)  && accomulate target nWo array from rest transaction codes.
                IF (cStyType = 'N')
                  laSWo1[lnW] = laSWo1[lnW] - Qty&lcW
                ENDIF
              ENDIF

            ENDFOR
            
          CASE Trancd = '3'    && P/O Shipment case.

            *-- if user want to rebalance in transit.
            IF lnIntrans = 1
              FOR lnW = 1 TO 8
                lcW = STR(lnW,1)
                laIntrans[lnW] = laIntrans[lnW] + Qty&lcW
              ENDFOR  
              =lfUpStyDye(Style,cWareCode,SPACE(10),'INTRANS','TOTINTRN','&lcPoLAls..QTY')
            ENDIF
          
          CASE Trancd = '6'    && Inter-Location line.

            FOR lnW = 1 TO 8
            
              lcW = STR(lnW,1)
                        
              IF (lnWip=1) AND &lcPoHAls..STATUS <> 'C' AND &lcPoHAls..cStyType <> 'A' 
                laSWip[lnW] = laSWip[lnW] + Qty&lcW
              ENDIF
              
              IF lnWo = 1
                laSWo1[lnW] = laSWo1[lnW] + Qty&lcW
              ENDIF
              
              IF lnIntrans = 1
                laIntrans[lnW] = laIntrans[lnW] + Qty&lcW
              ENDIF  
            
            ENDFOR

            *-- Update intransit fields.
            IF lnIntrans = 1 
              =lfUpStyDye(Style,cWareCode,SPACE(10),'INTRANS','TOTINTRN','&lcPoLAls..QTY')
            ENDIF  
        
        ENDCASE

      ENDIF

      lcKey = &lcBatChng
    ENDSCAN
    
    *-- Update last group. [begin]
    DO lpCalGrpDt WITH lcGrpWare,IIF(lnWip=1,'laTWip',''),IIF(lnWo=1,'laTWo1','')
    IF (lcWorkFile = lcPoLAls) AND lcLastType $ 'AN'
      DO lpCalGrpDt WITH lcSource,IIF(lnWip=1,'laSWip',''),IIF(lnWo=1,'laSWo1','')
    ENDIF  
    *-- Update last group. [end]

    *-- Update style record.
    IF lcVrUpd = 'V'
      SELECT lcStyAls
    ELSE
      SELECT (lcStyAls)
    ENDIF
    IF lnWip = 1
      *E301337,1 ASH 10/18/1999 (Begin) Add the wrong style to the log file.
      *IF lcVrUpD = 'V' AND TOTWIP <> laWip[9] AND !llAdoDye
      *  DECLARE laRebMsg[3]
      *  laRebMsg[1] = " "
      *  laRebMsg[2] = "Company" + lcCompCode +": "+ Style + " has wrong Work in process quantity."
      *  laRebMsg[3] = " "
      *  =lfVryRport()
      *ENDIF
      IF lcVrUpD = 'V'
        lnPrevVal = lcStyAls.Totwip
      ENDIF
      *E301337,1 ASH 10/18/1999 (End)
      DO lpUpDtWork WITH laWip,'WIP','TOTWIP'

      *-- update stydye record if system does not have multiware and dyelots.
      *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
      IF lcVrUpd = 'V'
        llExpr = SEEK(lcStyAls.STYLE,'lcSDyAls')
      ELSE
        llExpr = SEEK(&lcStyAls..STYLE,lcSDyAls)
      ENDIF
      IF !llStyDyeFl AND llExpr 
        IF lcVrUpd = 'V'
          SELECT lcSDyAls
        ELSE
          SELECT (lcSDyAls)
        ENDIF
        DO lpUpDtWork WITH laWip,'WIP','TOTWIP'
        IF lcVrUpd = 'V'
          SELECT lcStyAls
        ELSE
          SELECT (lcStyAls)
        ENDIF
      ENDIF
      *E301337,1 ASH 10/18/1999 (End)
      *laWip = 0
    ENDIF

    IF lnWo = 1
      *IF lcVrUpD = 'V' AND NTOTWO <> laWo[9] AND !llAdoDye
      *  DECLARE laRebMsg[3]
      *  laRebMsg[1] = " "
      *  laRebMsg[2] = "Company" + lcCompCode +": "+ Style + " has wrong Work Order quantity."
      *  laRebMsg[3] = " "
      *  =lfVryRport()
      *ENDIF
      IF lcVrUpD = 'V'
        lnPrevVal = lcStyAls.nTotWo
      ENDIF
    
      DO lpUpDtWork WITH laWo,'NWO','NTOTWO'

      *-- update stydye record if system does not have multiware and dyelots.
      *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
      IF lcVrUpd = 'V'
        llExpr = SEEK(lcStyAls.STYLE,'lcSDyAls')
      ELSE
        llExpr = SEEK(&lcStyAls..STYLE,lcSDyAls)
      ENDIF
      IF !llStyDyeFl AND llExpr
        IF lcVrUpd = 'V'
          SELECT lcSDyAls
        ELSE
          SELECT (lcSDyAls)
        ENDIF
        DO lpUpDtWork WITH laWo,'NWO','NTOTWO'
        IF lcVrUpd = 'V'
          SELECT lcStyAls
        ELSE
          SELECT (lcStyAls)
        ENDIF
      ENDIF
     
      *E301337,1 ASH 10/18/1999 (End)
      *laWo = 0
    ENDIF

    IF lnIntrans = 1
      laIntrans[9] = laIntrans[1]+laIntrans[2]+laIntrans[3]+laIntrans[4]+;
                     laIntrans[5]+laIntrans[6]+laIntrans[7]+laIntrans[8]
      *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
      IF lcVrUpD = 'V' AND TotInTrn <> laIntrans[9] 
        DECLARE laRebMsg[3]
        laRebMsg[1] = " "
        laRebMsg[2] = "Company" + lcCompCode +": "+ Style + " has wrong in transit quantity."
        laRebMsg[3] = " "
        =lfVryRport()
      ENDIF
      DO lpUpDtWork WITH laIntrans,'INTRANS','TOTINTRN'

      *-- update stydye record if system does not have multiware and dyelots.
      
      IF lcVrUpd = 'V'
        llExpr =  SEEK(lcStyAls.STYLE,'lcSDyAls')
      ELSE
        llExpr =  SEEK(&lcStyAls..STYLE,lcSDyAls)
      ENDIF
      IF !llStyDyeFl AND llExpr
        IF lcVrUpd = 'V'
          SELECT lcSDyAls
        ELSE
          SELECT (lcSDyAls)
        ENDIF
        DO lpUpDtWork WITH laIntrans,'INTRANS','TOTINTRN'
        IF lcVrUpd = 'V'
          SELECT lcStyAls
        ELSE
          SELECT (lcStyAls)
        ENDIF
      ENDIF
      IF lcVrUpd = 'V' 
        IF SEEK(lcStyAls.Style,'lcSDyAls')
          SELECT lcSDyAls
          SCAN WHILE Style = lcStyAls.Style
            IF TOTINTRN <> StyDye.TOTINTRN
              DECLARE laRebMsg[3]
              laRebMsg[1] = " "
              laRebMsg[2] = "Company" + lcCompCode +": "+ Style+" has wrong IN TRANSIT quantity."
              laRebMsg[3] = " "
              =lfVryRport()
              EXIT
            ENDIF
          ENDSCAN
        ENDIF
      ENDIF  
      
      *E301337,1 ASH 10/18/1999 (End)
      laIntrans = 0
    ENDIF  

  ENDIF  && end If Wip Only from W.I.P. adjustment file.
  
ENDIF
*-- end of lfUpdtOper.

*!*************************************************************
*! Name      : lpUpDtWork
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Update Style file record for work process cases.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : 1- Name of update array
*!                        : 2- Name of update field
*!                        : 3- Name of total field.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lpUpDtWork()
*!*************************************************************
*!
PROCEDURE lpUpDtWork
PARAMETERS laUpdArr,lcUpdFld,lcTotFld
GATHER FROM laUpdArr FIELDS ;
            (lcUpdFld+'1'),;
            (lcUpdFld+'2'),;
            (lcUpdFld+'3'),;
            (lcUpdFld+'4'),;
            (lcUpdFld+'5'),;
            (lcUpdFld+'6'),;
            (lcUpdFld+'7'),;
            (lcUpdFld+'8'),;
            (lcTotFld)
*-- end of lpUpDtWork.

*!*************************************************************
*! Name      : lpCalGrpDt
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Adjust passed group data then update stydye file .
*!*************************************************************
*! Calls     : 
*!             Procedures : lpStyDyeUp
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : 1- Location code
*!                        : 2- Wip array name (Target/Source)
*!                        : 3- nWo array name (Target/Source)
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lpCalGrpDt()
*!*************************************************************
*!
PROCEDURE lpCalGrpDt
PARAMETERS lcMastWare,laTempArr1,laTempArr2

IF lnWip=1
  FOR lnAdj = 1 TO 8
    *-- detect (C/T or P/O) and Return P/O Over Receive Quantities.
    IF (((lcWorkFile = lcCutLnAls) OR (lcLastType = 'P')) AND (&laTempArr1[lnAdj] < 0)) OR ;
       ((lcWorkFile = lcPoLAls) AND (lcLastType = 'R') AND (&laTempArr1[lnAdj] > 0))
      &laTempArr1[lnAdj] = 0
    ENDIF
    &laTempArr1[9] = &laTempArr1[9] + &laTempArr1[lnAdj]
  ENDFOR

  IF &laTempArr1[9] <> 0
    FOR lnAdd = 1 TO 9
      laWip[lnAdd] = laWip[lnAdd] + &laTempArr1[lnAdd]
    ENDFOR
    DO lpStyDyeUp WITH lcStyle,lcMastWare,&laTempArr1,'WIP','TOTWIP'
  ENDIF
    
ENDIF  

IF lnWo = 1

  &laTempArr2[9] = &laTempArr2[1]+&laTempArr2[2]+&laTempArr2[3]+&laTempArr2[4]+;
                   &laTempArr2[5]+&laTempArr2[6]+&laTempArr2[7]+&laTempArr2[8]

  IF &laTempArr2[9] <> 0
    FOR lnAdd = 1 TO 9
      laWo[lnAdd] = laWo[lnAdd] + &laTempArr2[lnAdd]
    ENDFOR
    DO lpStyDyeUp WITH lcStyle,lcMastWare,&laTempArr2,'NWO','NTOTWO'    
    laTempWo = 0
  ENDIF  
            
ENDIF
*-- end of lpCalGrpDt.

*!*************************************************************
*! Name      : lpStyDyeUp
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/12/98
*! Purpose   : Update stydye record for current group.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpStyDyeUp
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : 1- Style code
*!                        : 2- Location code
*!                        : 3- Update array name (Target/Source)
*!                        : 4- Updated field Name
*!                        : 5- Updated total field 
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lpStyDyeUp()
*!*************************************************************
*!
PROCEDURE lpStyDyeUp
PARAMETERS lcGrpStyle,lcGrpWare,laUpdArr,lcUpdFld,lcTotFld
lcGrpStyle = PADR(lcGrpStyle,19)
lcGrpWare  = PADR(lcGrpWare,6)

*E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
IF lcVrUpd = 'V'
  llExpr = SEEK(lcGrpStyle+lcGrpWare+SPACE(10),'lcSDyAls')
ELSE
  llExpr = SEEK(lcGrpStyle+lcGrpWare+SPACE(10),lcSDyAls)
  *B603992,1 ASH 07/11/2002 (Begin) Create missing records in STYDYE file.
  IF !llExpr
    PRIVATE lcCurAlias
    lcCurAlias = SELECT(0)
    SELECT (lcSDyAls)
    APPEND BLANK
    REPLACE STYLE     WITH lcGrpStyle
    REPLACE cWareCode WITH lcGrpWare
    SELECT (lcCurAlias)
  ENDIF
  *B603992,1 ASH 07/11/2002 (End)
ENDIF

IF llStyDyeFl AND llExpr
  PRIVATE lcCurAlias
  lcCurAlias = SELECT(0)
  IF lcVrUpd = 'V'
    SELECT lcSDyAls
  ELSE
    SELECT (lcSDyAls)
  ENDIF
  *E301337,1 ASH 10/18/1999 (End)
  REPLACE (lcUpdFld+'1') WITH EVALUATE(lcUpdFld+'1') + laUpdArr[1] ,;
          (lcUpdFld+'2') WITH EVALUATE(lcUpdFld+'2') + laUpdArr[2] ,;
          (lcUpdFld+'3') WITH EVALUATE(lcUpdFld+'3') + laUpdArr[3] ,;
          (lcUpdFld+'4') WITH EVALUATE(lcUpdFld+'4') + laUpdArr[4] ,;
          (lcUpdFld+'5') WITH EVALUATE(lcUpdFld+'5') + laUpdArr[5] ,;
          (lcUpdFld+'6') WITH EVALUATE(lcUpdFld+'6') + laUpdArr[6] ,;
          (lcUpdFld+'7') WITH EVALUATE(lcUpdFld+'7') + laUpdArr[7] ,;
          (lcUpdFld+'8') WITH EVALUATE(lcUpdFld+'8') + laUpdArr[8] ,;
          (lcTotFld)     WITH EVALUATE(lcTotFld)     + laUpdArr[9]

  
  *-- Trace current style in stydye file if not traced before.
  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    lcExpr = lcSDyAls.STYLE+lcSDyAls.CWARECODE+lcSDyAls.DYELOT
  ELSE
    lcExpr = &lcSDyAls..STYLE+&lcSDyAls..CWARECODE+&lcSDyAls..DYELOT
  ENDIF
  =!llStyDyeFl AND gfTraceKey('STYDYE',lcExpr,'M',lcFilePath,lcCompMods)
  *E301337,1 ASH 10/18/1999 (End)
  SELECT (lcCurAlias)
ENDIF
laUpdArr = 0  && Clear Temp. Arrays for another use.
*-- end of Update Stydye file.

*!*************************************************************
*! Name      : lfUpFabDye
*! Developer : Mohamed Badran (MAB)
*! Date      : 11/10/98
*! Purpose   : Update fabdye record for current group.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : 1- Updated field.
*!                        : 2- Updated Value.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfUpFabDye()
*!*************************************************************
*!*E301059
*!
FUNCTION lfUpFabDye
PARAMETERS lcUpdtFld,lnUpDtVal
PRIVATE lnAlias
lnAlias = SELECT(0)
*E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
IF lcVrUpd = 'V'
  SELECT lcFDyAls
ELSE
  SELECT (lcFDyAls)
ENDIF
*E301337,1 ASH 10/18/1999 (End)

*B604660,1 MHM 07/25/2001 Accept Minus quantity in case of clos MA po. [START]
REPLACE (lcUpdtFld) WITH MAX(EVALUATE(lcUpdtFld) + lnUpDtVal,0)

*REPLACE (lcUpdtFld) WITH (EVALUATE(lcUpdtFld) + lnUpDtVal)

*B604660,1 MHM [END]

*-- trace fabdye file.
*E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
*=gfTraceKey('FABDYE',&lcFDyAls..FABRIC+&lcFDyAls..COLOR+&lcFDyAls..CWARECODE,'M',lcFilePath,lcCompMods)
IF lcVrUpd = 'V'
  lcExpr = lcFDyAls.FABRIC+lcFDyAls.COLOR+lcFDyAls.CWARECODE
ELSE
  lcExpr = &lcFDyAls..FABRIC+&lcFDyAls..COLOR+&lcFDyAls..CWARECODE
ENDIF 
=gfTraceKey('FABDYE',lcExpr,'M',lcFilePath,lcCompMods)
*E301337,1 ASH 10/18/1999 (End)
SELECT (lnAlias)
*-- end of lfUpFabDye.

*!*************************************************************
*! Name      : lfCloseFls
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/01/99
*! Purpose   : Close all open files (another alias name)
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters      : 1- No. of files opened.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfCloseFls()
*!*************************************************************
*!*E301077,27
*!
FUNCTION lfCloseFls
PARAMETERS lnFlArrLen
PRIVATE lnFileCnt
FOR lnFileCnt = 1 TO lnFlArrLen

  *E301077,27 Close Temp. alias using gfCloseFile [Begin
  USE IN (laFileStr[lnFileCnt,4])  && Close this alias...
  *=gfCloseFile(laFileStr[lnFileCnt,4])
  *E301077,27 Close Temp. alias using gfCloseFile [End
ENDFOR
*-- end of lfCloseFls.


*!*************************************************************
*! Name      : lfStyStock
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/20/1999
*! Purpose   : Calculate Style and Style/Location Stock.
*!*************************************************************
*! Passed Parameters      : 1- No. of files opened.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfStyStock()
*!*************************************************************
*!E301232,1
*!
FUNCTION lfStyStock
PRIVATE lnAliasNow , lcStyWare , lnI

=ACOPY(laStkArr,laUpdatArr)  && copy stock array to laUpdatArr to call generic code.
*-- if no transactions found for this style in style inventory journal.
IF EOF(lcStyJlAls)

  *-- Update Style record with zero values if single location.
  DO lpGathFlds WITH "NSTKVAL WITH 0.00 , AVE_COST WITH TotCost"

  *-- update stydye record if system does not have multiware and dyelots.
  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    llExpr = SEEK(lcStyAls.STYLE,'lcSDyAls')
  ELSE
    llExpr = SEEK(&lcStyAls..STYLE,lcSDyAls)
  ENDIF
  IF !llMultWare AND llExpr 
    IF lcVrUpd = 'V'
      SELECT lcSDyAls
      lcExpr = "NSTKVAL WITH 0.00 , AVE_COST WITH lcStyAls.TotCost"  
    ELSE
      SELECT (lcSDyAls)
      lcExpr = "NSTKVAL WITH 0.00 , AVE_COST WITH &lcStyAls..TotCost"  
    ENDIF
    *-- Update StyDye record with zero values.
    DO lpGathFlds WITH lcExpr && Stydye record.
    IF lcVrUpd = 'V'
      SELECT lcStyAls
    ELSE
      SELECT (lcStyAls)
    ENDIF
  *B603992,1 ASH 07/11/2002 (Begin) Add record to stydye file of not found.
  ELSE
    IF !llExpr AND lcVrUpd = 'U'
      =lfAddRec(&lcStyAls..Style,&lcStyAls..cDefWare,&lcStyAls..Ave_cost)
    ENDIF  
  *B603992,1 ASH 07/11/2002 (End)
    
  ENDIF
  *E301337,1 ASH 10/18/1999 (End)
ELSE  && Else there are some transactions in style inventory journal file.
  
  SELECT (lcStyJlAls)
  IF llMultWare
    lcStyWare = Style + cWareCode
  ENDIF  
  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    lcStyExp = lcStyAls.Style
  ELSE
    lcStyExp = &lcStyAls..Style
  ENDIF
  *E301337,1 ASH 10/18/1999 (End)
  SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrDate)+cTrCode= lcStyExp 
    *B603992,1 ASH 07/11/2002 (Begin) Variable to indicate if stydye record found.
    llFound = .T.
    *B603992,1 ASH 07/11/2002 (End)
    *-- if you go to another Style/Location group
    IF llMultWare AND (Style + cWareCode <>  lcStyWare)
      *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
      IF lcVrUpd = 'V'
        SELECT lcSDyAls
        =SEEK(lcStyWare)
        lnTotCost = lcStyAls.TotCost
      ELSE
      *E301337,1 ASH 10/18/1999 (End)
        SELECT (lcSDyAls)
        *B603992,1 ASH 07/11/2002 (Begin) To check if the record in stydye file or not.
        *=SEEK(lcStyWare)
        llFound=SEEK(lcStyWare)
        *B603992,1 ASH 07/11/2002 (End)
        lnTotCost = &lcStyAls..TotCost
      ENDIF
      *B603992,1 ASH 07/11/2002 (Begin) Add record to stydye file of not found.
      IF !llFound AND lcVrUpd ='U'
        =lfAddRec(SUBSTR(lcStyWare,1,19),SUBSTR(lcStyWare,20,5),&lcStyAls..Ave_Cost)
      ENDIF
      *B603992,1 ASH 07/11/2002 (End)
      DO lpGathFlds WITH "nStkVal WITH laLocStock[10] ," +;
                         "Ave_Cost WITH IIF(TotStk=0,lnTotCost ,ABS(nStkVal/TotStk))"
      SELECT (lcStyJlAls)
    
    ENDIF
    
    *-- accomulate transactions in database array.
    FOR lnJ = 1 To 8
      lcJ = STR(lnJ,1)
      laStock[lnJ]    = laStock[lnJ]    + nStk&lcJ

      IF llMultWare
        laLocStock[lnJ] = laLocStock[lnJ] + nStk&lcJ
      ENDIF
        
    ENDFOR
    
    laStock[9]     = laStock[1] + laStock[2] + laStock[3] + laStock[4] +;
                     laStock[5] + laStock[6] + laStock[7] + laStock[8]
    laStock[10]    = laStock[10] + nStkVal

    IF llMultWare
      laLocStock[9]  = laLocStock[1] + laLocStock[2] + laLocStock[3] + laLocStock[4] +;
                       laLocStock[5] + laLocStock[6] + laLocStock[7] + laLocStock[8]
      laLocStock[10] = laLocStock[10] + nStkVal
      lcStyWare = PADR(Style,19) + PADR(cWareCode,6)
    ENDIF  
    *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    IF lcVrUpd = 'V'
      llExpr = lcStyAls.cDye_Flg='Y'
    ELSE
      llExpr = &lcStyAls..cDye_Flg='Y'
    ENDIF
    *E301337,1 ASH 10/18/1999 (End)
    IF llDyelot AND llExpr AND !EMPTY(cDyelot)
      IF SEEK(Style + cWareCode + cDyelot, lcTmpDyeFl)
        lnAliasNow = SELECT(0)
        SELECT (lcTmpDyeFl)
        REPLACE Stk1 WITH Stk1   + &lcStyJlAls..nStk1 ,;
                Stk2 WITH Stk2   + &lcStyJlAls..nStk2 ,;
                Stk3 WITH Stk3   + &lcStyJlAls..nStk3 ,;
                Stk4 WITH Stk4   + &lcStyJlAls..nStk4 ,;
                Stk5 WITH Stk5   + &lcStyJlAls..nStk5 ,;
                Stk6 WITH Stk6   + &lcStyJlAls..nStk6 ,;
                Stk7 WITH Stk7   + &lcStyJlAls..nStk7 ,;
                Stk8 WITH Stk8   + &lcStyJlAls..nStk8 ,;
              TotStk WITH TotStk + &lcStyJlAls..nTotStk
        SELECT (lnAliasNow)
      ELSE
        STORE 0 TO lnI,m.TotStk
        m.Style     = Style
        m.cWareCode = cWareCode
        m.Dyelot    = cDyelot
        FOR lnI = 1 TO 8
          lcStkDye = "m.Stk"+STR(lnI,1)
          lcStkJl  = "nStk" +STR(lnI,1)
          &lcStkDye = &lcStkJl
          m.TotStk = m.TotStk + &lcStkDye
        ENDFOR
        INSERT INTO (lcTmpDyeFl) FROM MEMVAR
      ENDIF
      
    ENDIF

  ENDSCAN
                               
  *-- Update last Style/Location group if system have multiple locations.
  IF llMultWare
    *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    IF lcVrUpd = 'V'
      =SEEK(lcStyWare ,'lcSDyAls')
     *E301337,1 ASH 10/18/1999 (Begin) Add the wrong styles to the log file.
     IF !EOF('STYDYE') AND laLocStock[9] <> StyDye.TotStk  
       DECLARE laRebMsg[3]
       laRebMsg[1] = " "
       laRebMsg[2] = "Company" + lcCompCode +": "+ lcStyAls.Style+" has wrong onhand quantity."
       laRebMsg[3] = " "
       =lfVryRport()
     ENDIF
     *E301337,1 ASH 10/18/1999 (End)
    ELSE
      *B603992,1 ASH 07/11/2002 (Begin) Add record to stydye file of not found.
      *=SEEK(lcStyWare , lcSDyAls)
      IF !SEEK(lcStyWare , lcSDyAls) AND lcVrUpd = 'U'
        =lfAddRec(SUBSTR(lcStyWare,1,19),SUBSTR(lcStyWare,20,5),&lcStyAls..Ave_Cost)
      ENDIF
      *B603992,1 ASH 07/11/2002 (End)
    ENDIF
  ELSE  && Else update stydye record if system does not have multiple locations.
    IF lcVrUpd = 'V'
      =SEEK(lcStyAls.STYLE,'lcSDyAls')
    ELSE
      *B603992,1 ASH 07/11/2002 (Begin) Add record to stydye file of not found.
      *=SEEK(&lcStyAls..STYLE,lcSDyAls)
      IF !SEEK(&lcStyAls..STYLE,lcSDyAls) AND lcVrUpd = 'U'
        =lfAddRec(&lcStyAls..STYLE,&lcStyAls..cDefWare,&lcStyAls..Ave_Cost)
      ENDIF
      *B603992,1 ASH 07/11/2002 (End)
    ENDIF
    *E301337,1 ASH 10/18/1999 (End)
    =ACOPY(laStock,laLocStock)  && copy Stock array to Stock location array avoid zero stock array.
  ENDIF
  IF lcVrUpd = 'V'
    *E301337,1 ASH 10/18/1999 (Begin) Add the wrong styles to the log file.
    IF !EOF('STYDYE') AND laLocStock[9] <> StyDye.TotStk  
      DECLARE laRebMsg[3]
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcCompCode +": "+ lcStyAls.Style+" has wrong ONHAND quantity."
      laRebMsg[3] = " "
      =lfVryRport()
    ENDIF
    *E301337,1 ASH 10/18/1999 (End)
  
    SELECT lcSDyAls
    lnTotCost = lcStyAls.TotCost
  ELSE
    SELECT (lcSDyAls)
    lnTotCost = &lcStyAls..TotCost
  ENDIF
  DO lpGathFlds WITH "nStkVal WITH laLocStock[10] ," +;
                     "Ave_Cost WITH IIF(TotStk=0,lnTotCost,ABS(nStkVal/TotStk))"

  *-- Update Style Record.
  IF lcVrUpd = 'V'
    SELECT lcStyAls
    lnTotStk = lcStyAls.TotStk
  ELSE
    SELECT (lcStyAls)
    lnTotStk = &lcStyAls..TotStk
  ENDIF
  *E301337,1 ASH 10/18/1999 (Begin) Add the wrong styles to the log file.
  IF lcVrUpD = 'V' AND laStock[9] <> lnTotStk 
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = "Company" + lcCompCode +": "+ Style+" has wrong onhand quantity."
    laRebMsg[3] = " "
    =lfVryRport()
  ENDIF
  *E301337,1 ASH 10/18/1999 (End)
  laUpdatArr[1,2] = 'laStock'
  DO lpGathFlds WITH "nStkVal WITH laStock[10] ," +;
                     "Ave_Cost WITH IIF(TotStk=0,TotCost,ABS(nStkVal/TotStk))"

  IF llDyelot AND TYPE('lcTmpDyeFl') $ "C" AND USED(lcTmpDyeFl) AND RECCOUNT(lcTmpDyeFl) > 0
    lnAliasNow = SELECT(0)
    SELECT (lcTmpDyeFl)
    SCAN
      *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
      IF lcVrUpd = 'V'
        llExpr = SEEK(Style+cWareCode+Dyelot,'lcSDyAls')
      ELSE
        llExpr = SEEK(Style+cWareCode+Dyelot,lcSDyAls)
      ENDIF
      SCATTER MEMVAR
      IF lcVrUpd = 'V'
        SELECT lcSDyAls
      ELSE
        SELECT (lcSDyAls)
      ENDIF
      IF !llExpr
        APPEND BLANK
      ENDIF
      GATHER MEMVAR 
      *E301337,1 ASH 10/18/1999 (End)
      *B603992,1 ASH 07/11/2002 (Begin) Add missing main record in stydye in case of using dyelots. m
      lcSTYDYE = Style+cWareCode
      IF !SEEK(&lcTmpDyeFl..Style+&lcTmpDyeFl..cWareCode+SPACE(10))
        lcAlias = ALIAS()
        SELECT (lcTmpDyeFl)
        lnRec = RECNO()
        SUM STK1,STK2,STK3,STK4,STK5,STK6,STK7,STK8,TOTSTK ;
         TO lnTSTK1,lnTSTK2,lnTSTK3,lnTSTK4,lnTSTK5,lnTSTK6,lnTSTK7,lnTSTK8,lnTStk ;
         FOR STYLE+CWARECODE=lcSTYDYE 
        GOTO lnRec    
        SELECT (lcAlias)
        APPEND BLANK
        REPLACE Style WITH &lcTmpDyeFl..Style ,;
                cWareCode WITH &lcTmpDyeFl..cWareCode ,;
                STK1 WITH lnTStk1 ,;
                STK2 WITH lnTStk2 ,;
                STK3 WITH lnTStk3 ,;
                STK4 WITH lnTStk4 ,;
                STK5 WITH lnTStk5 ,;
                STK6 WITH lnTStk6 ,;
                STK7 WITH lnTStk7 ,;
                STK8 WITH lnTStk8 ,;
                TOTSTK WITH lnTStk 
      ENDIF
      *B603992,1 ASH 07/11/2002 (End)
    ENDSCAN
    ZAP
    SELECT (lnAliasNow)
  ENDIF

ENDIF
*-- end of lfStyStock.

*!*************************************************************
*! Name      : lfCrtTmpDy
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 06/01/1999
*! Purpose   : Create temporary file used to update dyelot records.
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfCrtTmpDy()
*!*************************************************************
*!E301232,1
*!
FUNCTION lfCrtTmpDy
PRIVATE lcAliasNow
lcAliasNow = SELECT(0)

IF USED(lcTmpDyeFl)
  USE IN (lcTmpDyeFl)
ENDIF

*E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
*CREATE CURSOR (lcTmpDyeFl) (Style C(19) , cWareCode C(6) , Dyelot C(10) ,;
                            Stk1 N(7,0),Stk2 N(7,0),Stk3 N(7,0),Stk4 N(7,0),;
                            Stk5 N(7,0),Stk6 N(7,0),Stk7 N(7,0),Stk8 N(7,0),TotStk N(8,0))

CREATE CURSOR (lcTmpDyeFl) (Style C(19) , cWareCode C(6) , Dyelot C(10) ,;
                            Stk1 N(7,0),Stk2 N(7,0),Stk3 N(7,0),Stk4 N(7,0),;
                            Stk5 N(7,0),Stk6 N(7,0),Stk7 N(7,0),Stk8 N(7,0),TotStk N(9,0))
*E301800,1 BWA 01/21/2002 (End)
ZAP
INDEX ON Style+cWareCode+Dyelot TAG (lcTmpDyeFl) OF (gcWorkDir+lcTmpDyeFl+'.CDX') 

SELECT (lcAliasNow)
*-- end of lfCrtTmpDy.


*!*************************************************************
*! Name      : lfMatOnHnd
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 06/14/99
*! Purpose   : Calculate onhand.
*!*************************************************************
*! Passed Parameters      : NONE
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfMatOnHnd()
*!*************************************************************
*!*E301261,1 SSH 06/14/99 Function to rebalance onhand.
FUNCTION lfMatOnHnd
PRIVATE lnAliasNow , lcFabWare , lnI

=ACOPY(laOnhArr,laUpdatArr)  && copy stock array to laUpdatArr to call generic code.

IF llFabDye
  =lfCrtMTmpDy()
ENDIF
*--- If no transactions found for this Fabric in Material inventory journal.
IF EOF(lcMatJlAls)
  *-- Update Fabric record with zero values if single location.
  DO lpGathFlds WITH "NSTKVAL WITH 0.00 ,nfAVE_COST WITH CostUse","Y"
  *-- update Fabric record if system does not have multiware and dyelots.
  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    llExpr = SEEK(lcFabAls.Fabric+lcFabAls.COLOR,'lcFDyAls')
  ELSE
    llExpr = SEEK(&lcFabAls..Fabric+&lcFabAls..COLOR,lcFDyAls)
  ENDIF
  IF !llMultWare AND llExpr
    IF lcVrUpd = 'V'
      SELECT lcFDyAls
      lnStkVal  = lcFabAls.NSTKVAL
      lnCostUse = lcFabAls.CostUse
    ELSE
      SELECT (lcFDyAls)
      lnStkVal  = &lcFabAls..NSTKVAL
      lnCostUse = &lcFabAls..CostUse
    ENDIF
    *-- Update StyDye record with zero values.
    DO lpGathFlds WITH "NSTKVAL WITH lnStkVal, nfAVE_COST WITH lnCostUse","Y"
    IF lcVrUpd = 'V'
      SELECT lcFabAls
    ELSE
      SELECT (lcFabAls)
    ENDIF
    *E301337,1 ASH 10/18/1999 (End)
  ENDIF
ELSE  && Else there are some transactions in style inventory journal file.
  SELECT (lcMatJlAls)
  IF TYPE('NSTKVAL') = "U"
    llOldRel = .T.
  ENDIF
  IF llMultWare
    lcFabWare = cFabric + cColor + cWareCode
  ENDIF  

  lnNPRVSQTY = 0
  lnNPRVSVAL = 0
  *B802768,1 SSH 09/21/99 Reset Variable
  lnLocOnHnd = 0
  lnLocOnHVl = 0
  *B802768,1 SSH (END)

  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    lcFabExp = lcFabAls.Fabric+lcFabAls.Color
  ELSE
    lcFabExp = &lcFabAls..Fabric+&lcFabAls..Color
  ENDIF
  *E301337,1 ASH 10/18/1999 (End)
  SCAN REST WHILE cfabric+ccolor+cwarecode+cdyelot+crsession+cisession = lcFabExp 
    *-- If you go to another Fabric/Location group
    IF llMultWare AND (cFABRIC+cCOLOR+cWareCode <>  lcFabWare)
      =SEEK(lcFabWare , IIF(lcVrUpd='V','lcFDyAls',lcFDyAls))
      IF lcVrUpd = 'V'
        SELECT lcFDyAls
        lnCost = lcFabAls.CostUse
      ELSE
        SELECT (lcFDyAls)
        lnCost = &lcFabAls..CostUse
      ENDIF
        
      DO lpGathFlds WITH "nStkVal WITH laLocONHAND[2] ," +;
                         "nfAve_Cost WITH IIF(ONHAND = 0,lnCost ,ABS(nStkVal/ONHAND))","Y"
      SELECT (lcMatJlAls)
      *B802768,1 SSH 09/21/99 Reset Onhand variables.
      STORE 0 TO laONHAND,laLocONHAND
      *B802768,1 SSH(END)
      
    ENDIF

    IF !llOldRel
      REPLACE NSTKVAL   WITH IIF(nreceived=0 AND nissued=0,NSTKVAL,nunitcost*(nreceived-nissued));
              NmPRVSQTY  WITH lnNPRVSQTY;
              NPRVSVAL  WITH lnNPRVSVAL
      lnNPRVSQTY = nreceived + lnNPRVSQTY
      lnNPRVSVAL = NSTKVAL   + lnNPRVSVAL
    ENDIF
    *-- Accomulate transactions in database array.
    laONHAND[1]  = laONHAND[1] + (nreceived - nissued)
    *B802768,1 SSH 09/21/99 Acoumulate onhand for allwarehouses to update fabric
    lnLocOnHnd     = lnLocOnHnd + (nreceived - nissued)
    *B802768,1 SSH (END)
    laONHAND[2]  = laONHAND[2] + IIF(!llOldRel,NSTKVAL,nunitcost*(nreceived-nissued))
    *B802768,1 SSH 09/21/99 New variable for stkVal
    lnLocOnHVl = lnLocOnHVl + IIF(!llOldRel,NSTKVAL,nunitcost*(nreceived-nissued))
    *B802768,1 SSH (END)
    
    IF llMultWare
      laLocONHAND[1]  = laLocONHAND[1] + (nreceived - nissued)
      laLocONHAND[2]  = laLocONHAND[2] + IIF(!llOldRel,NSTKVAL,nunitcost*(nreceived - nissued))
      lcFabWare = cFABRIC + cCOLOR + PADR(cWareCode,6)
    ENDIF
    IF lcVrUpd = 'V'
      lcExpr = lcFabAls.cDye_Flg
    ELSE
      lcExpr = &lcFabAls..cDye_Flg
    ENDIF
    IF llFabDye AND lcExpr = "Y" AND !EMPTY(cDyelot)

      *B603165,1 Field Names is cFabric and cColor [Begin]
      *IF SEEK(FABRIC+COLOR + cWareCode + cDyelot, lcTmpMDyeFl)
      IF SEEK(cFABRIC+cCOLOR + cWareCode + cDyelot, lcTmpMDyeFl)
      *B603165,1 Field Names is cFabric and cColor [End]

        lnAliasNow = SELECT(0)
        SELECT (lcTmpMDyeFl)
        REPLACE ONHAND WITH ONHAND + (&lcMATJlAls..nReceived - &lcMATJlAls..nissued)
        SELECT (lnAliasNow)
      ELSE
        STORE 0 TO m.TotStk

        *B603165,1 Field Names is cFabric and cColor [Begin]
        *m.FABRIC    = FABRIC
        m.FABRIC    = cFABRIC
        m.Color     = cColor  && You does not add it before.
        *B603165,1 Field Names is cFabric and cColor [End]
        
        m.cWareCode = cWareCode
        m.Dyelot    = cDyelot
    
        *B603165,1 On Hand field computed from Received - issued  [Begin]
        *m.Onhand    = onhand
        m.Onhand    = nReceived - nissued
        *B603165,1 On Hand field computed from Received - issued  [End]

        INSERT INTO (lcTmpMDyeFl) FROM MEMVAR
      ENDIF
    ENDIF
  ENDSCAN
  *-- Update last Style/Location group if system have multiple locations.
  IF llMultWare
    =SEEK(lcFabWare , IIF(lcVrUpd='V','lcFDyAls',lcFDyAls))
  ELSE  && Else update stydye record if system does not have multiple locations.
    *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
    IF lcVrUpd = 'V'
      =SEEK(lcFabAls.FABRIC+lcFabAls.COLOR,'lcFDyAls')
    ELSE
      =SEEK(&lcFabAls..FABRIC+&lcFabAls..COLOR,lcFDyAls)
    ENDIF
    *E301337,1 ASH 10/18/1999 (End)
    =ACOPY(laONHAND,laLocONHAND)  && copy Stock array to Stock location array avoid zero stock array.
  ENDIF
  IF lcVrUpd = 'V' AND laLocONHAND[1] <> FabDye.OnHand AND !EMPTY(FabDye.Fabric)
    DECLARE laRebMsg[3]
    laRebMsg[1] = " "
    laRebMsg[2] = "Company" + lcCompCode +": "+ FabDye.Fabric+"/"+FabDye.Color+" has wrong ONHAND quantity."
    laRebMsg[3] = " "
    =lfVryRport()
  ENDIF
  
  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    SELECT lcFDyAls
    lnCostUse = lcFabAls.CostUse
  ELSE
  *E301337,1 ASH 10/18/1999 (End)
    SELECT (lcFDyAls)
    lnCostUse = &lcFabAls..CostUse
  ENDIF
  DO lpGathFlds WITH "nStkVal WITH laLocONHAND[2] ," +;
                     "nfAve_Cost WITH IIF(ONHAND=0,lnCostUse ,ABS(nStkVal/ONHAND))","Y"
  *-- Update Style Record.
  *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
  IF lcVrUpd = 'V'
    SELECT lcFabAls
  ELSE
  *E301337,1 ASH 10/18/1999 (End)
    SELECT (lcFabAls)
  ENDIF
  laUpdatArr[1,2] = 'laONHAND'
  *B802768,1 SSH 09/21/99 New variable for stkVal
  *DO lpGathFlds WITH "nStkVal WITH laONHAND[2] ," +;
                     "nfAve_Cost WITH IIF(ONHAND=0,CostUse,ABS(nStkVal/ONHAND))","Y"
  DO lpGathFlds WITH "nStkVal WITH lnLocOnHVl ," +;
                     "nfAve_Cost WITH IIF(ONHAND=0,CostUse,ABS(nStkVal/ONHAND))","Y"
  *B802768,1 SSH(END)
  IF llFabDye AND TYPE('lcTmpMDyeFl') $ "C" AND USED(lcTmpMDyeFl) AND RECCOUNT(lcTmpMDyeFl) > 0
    lnAliasNow = SELECT(0)
    SELECT (lcTmpMDyeFl)
    SCAN

      *B603165,1 Miss spelling [Begin]
      *IF SEEK(FABRC+COLOR+cWareCode+Dyelot,lcFDyAls)
      IF SEEK(FABRIC+COLOR+cWareCode+Dyelot,IIF(lcVrUpd='V','lcFDyAls',lcFDyAls))
      *B603165,1 Miss spelling [End  ]

        SCATTER MEMVAR
        *E301337,1 ASH 10/18/1999 (Begin) Use the file created for verifying.
        IF lcVrUpd = 'V' 
          SELECT lcFDyAls
        ELSE
        *E301337,1 ASH 10/18/1999 (End)
          SELECT (lcFDyAls)
        ENDIF
        GATHER MEMVAR
      ENDIF
    ENDSCAN
    ZAP
    SELECT (lnAliasNow)
  ENDIF
  
ENDIF

*!*************************************************************
*! Name      : lfCrtMTmpDy
*! Developer : 
*! Date      : 
*! Purpose   : Create temporary file used to update dyelot records.
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfCrtMTmpDy()
*!*************************************************************
*!*E301261,1 SSH 06/14/99 Function to Create temp. file.
FUNCTION lfCrtMTmpDy
PRIVATE lcAliasNow
lcAliasNow = SELECT(0)

IF USED(lcTmpMDyeFl)
  USE IN (lcTmpMDyeFl)
ENDIF
CREATE CURSOR (lcTmpMDyeFl) (Fabric C(07) , Color C(06), cWareCode C(6) ,;
                             Dyelot C(10) , ONHAND N(12,3))
ZAP
INDEX ON FABRIC+COLOR+cWareCode+Dyelot TAG (lcTmpMDyeFl) OF (gcWorkDir+lcTmpMDyeFl+'.CDX')

SELECT (lcAliasNow)
*-- end of lfCrtTmpDy.


*!*************************************************************
*! Name      : lfPrnt
*! Developer : Ramy Mabrouk
*! Date      : 
*! Purpose   : Print the rebalance log report
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfPrnt()
*!*************************************************************

*603703,1 Ramy 
FUNCTION lfVPrnt

IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF


*!*************************************************************
*! Name      : lfUpBalance
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 29/01/2001
*! Purpose   : Update temp header file with selected elemets
*!           : to be rebalanced.
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfUpBalance()
*!*************************************************************
*E301516,1 
FUNCTION lfUpBalance

PRIVATE lnSelect
lnSelect = SELECT()
SELECT lcTempHdr
*--Initialize all option with 'I'gnore.
REPLACE ALL cUpdVryIgn WITH 'I'
*FOR lnCnt = 1 TO LEN(lcAutoBal)
FOR lnCnt = 1 TO 99
  lnNoOfIt = IIF(lnCnt<10,1,2)
  IF TYPE('LCRPITEM'+STR(lnCnt,lnNoOfIt)) <> 'C'
    EXIT
  ENDIF
  IF EVAL('LCRPITEM'+STR(lnCnt,lnNoOfIt)) <> 'I' AND ;
     SEEK('LCRPITEM'+STR(lnCnt,lnNoOfIt))
    REPLACE cUpdVryIgn WITH EVAL('LCRPITEM'+STR(lnCnt,lnNoOfIt))
  ENDIF  
ENDFOR
lcRpCmpExp = lcCompCode 
SELECT (lnSelect)
RETURN '.T.'

*!*************************************************************
*! Name      : lfCreatCr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 15/08/2001
*! Purpose   : Create temp. Cursor to collect warehouse QTY to be 
*!           : rebalanced in case of Multi location Manefactured po.
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfCreatCr()
*!*************************************************************
*mhm2000 
FUNCTION lfCreatCr
IF USED(lcTmpWar)
  USE IN (lcTmpWar)
ENDIF

CREATE CURSOR (lcTmpWar) (cFabric C(7) , cColor C(6), cWareCode C(6) ,;
                            nOrgQty N(7,0),nWipQty N(7,0), nLineNo N(6))
ZAP

INDEX ON cFabric +cColor +cWareCode TAG (lcTmpWar) 
INDEX ON nLineNo TAG (lcTmpWar2) 


*!***************************************************************************
*! Name      : lfvAccount
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/31/2002
*! Purpose   : Validation for Account code.
*!***************************************************************************
*! Example   : = lfvAccount()
*!***************************************************************************
*B605667,1
FUNCTION lfvAccount
PRIVATE lcObjName , lcObjVal , lcCstmrTag , llObjRet
lcObjName  = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal   = EVALUATE(SYS(18))   && Varible to hold  the value of the current GET field
IF !USED('Customer')
  =gfOpenFile(lcFilePath+'Customer', 'Customer' ,'SH')
ENDIF

lcCstmrTag = ORDER('Customer')
SET ORDER TO TAG Customer IN Customer

*-- if the user want to browse or if the account he entered is not in the file
IF '?' $ lcObjVal OR (!EMPTY(lcObjVal) AND !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  &lcObjName = IIF(llObjRet , lcObjVal , laOldVal)
ENDIF    && end of if

*-- if the CUSTOMER file did not have an active index
IF EMPTY(lcCstmrTag)
  SET ORDER TO 0 IN CUSTOMER
ELSE
  SET ORDER TO TAG (lcCstmrTag) IN CUSTOMER
ENDIF
*-- End of lfvAccount.

*!***************************************************************************
*! Name      : lfvHisYear
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/31/2002
*! Purpose   : Validation for Fiscal year ArCusHst case.
*!***************************************************************************
*! Example   : = lfvHisYear()
*!***************************************************************************
*B605667,1
FUNCTION lfvHisYear
DECLARE laRpRetFld(1)
lcOldBrFld    = lcBrFields
lcBrFields    = 'cFisFYear:H="Year",cFisYStat:H="Status",DFisBgDat:H="Begin date",DFisEnDat:H="End date"'
laRpRetFld[1] = ''
lcFicsYr = EVALUATE(SYS(18))
lcRpCurFld = VARREAD()
lcOldAlias = SELECT()    && Save the current alias
llUesdBefo = .F.        && Check if used before or this the first time
IF !USED("FISHD") 
  SELECT 0
  USE (lcFilePath+'FISHD') ORDER TAG compfyear
  llUesdBefo = .T.
ENDIF
SELECT FISHD
 
IF EMPTY(lcFicsYr) OR '?' $ &lcRpCurFld. OR !SEEK(lcFicsYr)  
  =gfBrows( " FOR .T. ",'CFisFyear',"laRpRetFld",'Transaction Codes ',.F.)
  &lcRpCurFld. = laRpRetFld[1]
  lcFicsYr   = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF
lcFiscYear = lcFicsYr   
IF llUesdBefo       && Close file if it was not used before.
  USE IN FISHD
ENDIF
SELECT (lcOldAlias)
RETURN 
*-- End of lfvHisYear.

*!***************************************************************************
*! Name      : lfStpCsHst
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/31/2002
*! Purpose   : Setup function for ArCusHst.
*!***************************************************************************
*! Example   : = lfStpCsHst()
*!***************************************************************************
*B605667,1
FUNCTION lfStpCsHst
PRIVATE lcCurrArea , laFields

IF lcTempHdr.cUpdVryIgn='U' AND !llUpdt
  RETURN
ENDIF
lcCurrArea = SELECT(0)
IF FILE(lcFilePath + ALLTRIM(lcTempHdr.cTempName)+'.DBF') 
  USE (lcFilePath + ALLTRIM(lcTempHdr.cTempName)) IN 0 EXCL
  SELECT ALLTRIM(lcTempHdr.cTempName)
  SET ORDER TO TAG (lcTempHdr.cTempName)
ELSE
  lcTag = lcTempHdr.cTempName
  SELECT ArCusHst 
  =AFIELDS(laFields)
  CREATE TABLE (lcFilePath + ALLTRIM(lcTempHdr.cTempName)) FROM ARRAY laFields  
ENDIF

IF FILE(lcFilePath + ALLTRIM(lcTempHdr.cTempName)+'.CDX')               
  SET ORDER TO TAG ALLTRIM(lcTempHdr.cTempName)
ELSE
  INDEX ON Account+cFisFYear TAG &lcTag OF (lcFilePath +ALLTRIM(lcTempHdr.cTempName)+'.CDX')
ENDIF
SELECT (lcCurrArea)
*-- End of lfStpCsHst.

*!***************************************************************************
*! Name      : lfVryCsHst
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/31/2002
*! Purpose   : Verify function for ArCusHst.
*!***************************************************************************
*! Example   : = lfVryCsHst()
*!***************************************************************************
*B605667,1
FUNCTION lfVryCsHst

PRIVATE lcUntSin,lcExRSin,lcGlYear,lcGlPeriod,lnCOGSAmt,lnShipAmnt,lnDiscAmnt,;
        lcFilter,lcDateExpr,lcExpr
STORE '' TO lcGlYear,lcGlPeriod,lcUntSin
STORE 0 TO lnCOGSAmt,lnShipAmnt,lnDiscAmnt
DECLARE laRebMsg[3]

*-- Zap Temp ArCusHst file first.
SELECT ALLTRIM(lcTempHdr.cTempName)

DO CASE 
  CASE EMPTY(SUBSTR(laFXFLT[5,6],1,5)) AND EMPTY(SUBSTR(laFXFLT[5,6],7,5))
    lcFilter = '.T.'
  CASE EMPTY(SUBSTR(laFXFLT[5,6],1,5)) AND !EMPTY(SUBSTR(laFXFLT[5,6],7,5))
    lcFilter = 'Account <= SUBSTR(laFXFLT[5,6],7,5)'
  CASE !EMPTY(SUBSTR(laFXFLT[5,6],1,5)) AND EMPTY(SUBSTR(laFXFLT[5,6],7,5))
    lcFilter = 'Account >= SUBSTR(laFXFLT[5,6],1,5)'
  CASE !EMPTY(SUBSTR(laFXFLT[5,6],1,5)) AND !EMPTY(SUBSTR(laFXFLT[5,6],7,5))
    lcFilter = 'Account >= SUBSTR(laFXFLT[5,6],1,5) AND Account <= SUBSTR(laFXFLT[5,6],7,5)'
ENDCASE

*--mhm C#200481 get Invoice date [Start]
DO CASE 
  CASE EMPTY(SUBSTR(laFXFLT[8,6],1,10)) AND EMPTY(SUBSTR(laFXFLT[8,6],12,10))
    lcFilter1 = '.T.'
  CASE EMPTY(SUBSTR(laFXFLT[8,6],1,10)) AND !EMPTY(SUBSTR(laFXFLT[8,6],12,10))
    lcFilter1 = 'InvDate <= CTOD(SUBSTR(laFXFLT[8,6],12,10))'
  CASE !EMPTY(SUBSTR(laFXFLT[8,6],1,10)) AND EMPTY(SUBSTR(laFXFLT[8,6],12,10))
    lcFilter1 = 'InvDate >= CTOD(SUBSTR(laFXFLT[8,6],1,10))'
  CASE !EMPTY(SUBSTR(laFXFLT[8,6],1,10)) AND !EMPTY(SUBSTR(laFXFLT[8,6],12,10))
    lcFilter1 = 'InvDate >= CTOD(SUBSTR(laFXFLT[8,6],1,10)) AND InvDate <= CTOD(SUBSTR(laFXFLT[8,6],12,10))'
ENDCASE
IF !(lcFilter1 = '.T.')
  IF (lcFilter = '.T.')
    lcOldFiltr =  lcFilter1
  ELSE
    lcOldFiltr = lcFilter +'.AND.' + lcFilter1
  ENDIF  
ENDIF
*--mhm C#200481 get Invoice date [End]

*-- 1) check InvHdr and InvLine files.
lcLow = SUBSTR(laFXFLT[6,6],1,4)
lcHigh = SUBSTR(laFXFLT[6,6],6,4)

DO CASE 
  CASE EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = '.T.'
  CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(dPostDate),4) <= lcHigh'
  CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(dPostDate),4) >= lcLow'
  CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(dPostDate),4) >= lcLow AND STR(YEAR(dPostDate),4) <= lcHigh'
ENDCASE

*-- To make rushmore expression.
*--mhm C#200481 get Invoice date Filter If Found [Start]
*lcExpr = STRTRAN(lcFilter,'Account','Account + Invoice')
IF !(lcFilter1 = '.T.')
  lcExpr = STRTRAN(lcOldFiltr,'Account','Account + Invoice')
ELSE
  lcExpr = STRTRAN(lcFilter,'Account','Account + Invoice')
ENDIF  
*--mhm C#200481  [End]

SELECT InvHdr
SCAN FOR &lcExpr
  IF &lcDateExpr AND Status <> 'V'
    WAIT WINDOW 'Fix Customer History File from Invoice # ' + Invoice NOWAIT
    STORE '' TO lcGlYear,lcGlPeriod,lcUntSin
    STORE 0 TO lnCOGSAmt,lnShipAmnt,lnDiscAmnt
    
    =CHECKPRD(dPostDate,'lcGlYear','lcGlPeriod','IN',.T.)
    IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcComp_ID + ": Invoice # " + InvHdr.Invoice + " has Transaction date " + DTOC(InvHdr.dPostDate) + " which does not fall within any period."
      laRebMsg[3] = " "
      =lfVryRport()
    ELSE
      lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
      lcUntSin = ''  
      lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)

      lnShipAmnt = ShipAmt &lcExRSin nExRate &lcUntSin nCurrUnit
      lnDiscAmnt = ABS(Discount) &lcExRSin nExRate &lcUntSin nCurrUnit
    
      IF SEEK(Invoice,'InvLine')
        SELECT InvLine
        SCAN REST WHILE Invoice + STR(LineNo,6) = InvHdr.Invoice
          lnCOGSAmt = lnCOGSAmt + (TotQty * Cost)
        ENDSCAN
      ENDIF
      
      SELECT ALLTRIM(lcTempHdr.cTempName)
      IF !SEEK(InvHdr.Account+lcGlYear)
        APPEND BLANK
        REPLACE Account WITH InvHdr.Account , cFisFYear WITH lcGlYear
      ENDIF        
      REPLACE nSlsQty&lcGlPeriod  WITH nSlsQty&lcGlPeriod  + InvHdr.Ship ,;
              nSlsQty             WITH nSlsQty             + InvHdr.Ship ,;
              nSlsAmt&lcGlPeriod  WITH nSlsAmt&lcGlPeriod  + lnShipAmnt ,;
              nSlsAmt             WITH nSlsAmt             + lnShipAmnt ,;
              nDisAmt&lcGlPeriod  WITH nDisAmt&lcGlPeriod  + lnDiscAmnt ,;
              nDisAmt             WITH nDisAmt             + lnDiscAmnt ,;
              nCOGSAmt&lcGlPeriod WITH nCOGSAmt&lcGlPeriod + lnCOGSAmt ,;
              nCOGSAmt            WITH nCOGSAmt            + lnCOGSAmt
    ENDIF          
  ENDIF
ENDSCAN

*-- 2) check OrdHdr file.
DO CASE 
  CASE EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = '.T.'
  CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(Entered),4) <= lcHigh'
  CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(Entered),4) >= lcLow'
  CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(Entered),4) >= lcLow AND STR(YEAR(Entered),4) <= lcHigh'
ENDCASE

*-- To make rushmore expression.
lcExpr = STRTRAN(lcFilter,'Account','Account + cOrdType + Order')

SELECT OrdHdr
SCAN FOR &lcExpr
  IF &lcDateExpr AND !(Status $ 'BX') AND !Direct_Inv
    WAIT WINDOW 'Fix Customer History File from Order # ' + Order NOWAIT

    STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
    =CHECKPRD(Entered,'lcGlYear','lcGlPeriod','',.T.)
    IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcComp_ID + ": Order # " + OrdHdr.Order + " has Transaction date " + DTOC(OrdHdr.Entered) + " which does not fall within any period."
      laRebMsg[3] = " "
      =lfVryRport()
    ELSE
      lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
      lcUntSin = ''  
      lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)

      SELECT ALLTRIM(lcTempHdr.cTempName)
      IF !SEEK(OrdHdr.Account+lcGlYear)
        APPEND BLANK
        REPLACE Account WITH OrdHdr.Account , cFisFYear WITH lcGlYear
      ENDIF
      REPLACE  nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + OrdHdr.Book , ;
               nOrdQty WITH nOrdQty + OrdHdr.Book , ;
               nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + OrdHdr.BookAmt , ;
               nOrdAmt WITH nOrdAmt + OrdHdr.BookAmt
    ENDIF           
  ENDIF
ENDSCAN

*-- 3) check RetHdr file.
DO CASE 
  CASE EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = '.T.'
  CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(CrDate),4) <= lcHigh'
  CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(CrDate),4) >= lcLow'
  CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(CrDate),4) >= lcLow AND STR(YEAR(CrDate),4) <= lcHigh'
ENDCASE

*-- To make rushmore expression.
lcExpr = STRTRAN(lcFilter,'Account','Account + CrMemo')

SELECT RetHdr
SCAN FOR &lcExpr
  IF &lcDateExpr
    WAIT WINDOW 'Fix Customer History File from Return # ' + CrMemo NOWAIT

    STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
    =CHECKPRD(CrDate,'lcGlYear','lcGlPeriod','RM',.T.)
    IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcComp_ID + ": Return # " + RetHdr.CrMemo + " has Transaction date " + DTOC(RetHdr.CrDate) + " which does not fall within any period."
      laRebMsg[3] = " "
      =lfVryRport()
    ELSE
      lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
      lcUntSin = ''  
      lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)

      SELECT ALLTRIM(lcTempHdr.cTempName)
      IF !SEEK(RetHdr.Account+lcGlYear)
        APPEND BLANK
        REPLACE Account WITH RetHdr.Account , cFisFYear WITH lcGlYear
      ENDIF        
      REPLACE nRetAmt            WITH nRetAmt + ROUND(RetHdr.Gross_Amt &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2) ,;
              nRetQty            WITH nRetQty + RetHdr.Pieces ,;
              nDisAmt            WITH nDisAmt - ROUND(RetHdr.Disc_Amt &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2) ,;
              nRetAmt&lcGlPeriod WITH nRetAmt&lcGlPeriod + ROUND(RetHdr.Gross_Amt &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2) ,;
              nRetQty&lcGlPeriod WITH nRetQty&lcGlPeriod + RetHdr.Pieces ,;
              nDisAmt&lcGlPeriod WITH nDisAmt&lcGlPeriod - ROUND(RetHdr.Disc_Amt &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2)
    ENDIF          
  ENDIF
ENDSCAN

*-- 4) check Debit file.
DO CASE 
  CASE EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = '.T.'
  CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(TranDate),4) <= lcHigh'
  CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(TranDate),4) >= lcLow'
  CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(TranDate),4) >= lcLow AND STR(YEAR(TranDate),4) <= lcHigh'
ENDCASE

*-- To make rushmore expression.
lcExpr = STRTRAN(lcFilter,'Account','Account + Tran + cInstalNo + DTOS(TranDate)')

SELECT Debit
SCAN FOR &lcExpr
  IF &lcDateExpr AND TranType <> '1'
    WAIT WINDOW 'Fix Customer History File from Debit Transaction # ' + Tran NOWAIT
  
    STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
    =CHECKPRD(TranDate,'lcGlYear','lcGlPeriod','',.T.)
    IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcComp_ID + ": Debit Tran # " + Debit.Tran + " has Transaction date " + DTOC(Debit.TranDate) + " which does not fall within any period."
      laRebMsg[3] = " "
      =lfVryRport()
    ELSE
      lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
      lcUntSin = ''  
      lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)

      SELECT ALLTRIM(lcTempHdr.cTempName)
      IF Debit.TranType = '3' AND !EMPTY(Debit.Chgbk_Date)
        LOOP
      ENDIF
      IF !SEEK(Debit.Account+lcGlYear)
        APPEND BLANK
        REPLACE Account WITH Debit.Account , cFisFYear WITH lcGlYear
      ENDIF
      REPLACE nDrAdj            WITH nDrAdj + ROUND(Debit.Amount &lcExRSin Debit.nExRate &lcUntSin Debit.nCurrUnit,2) ,;
              nDrAdj&lcGLPeriod WITH nDrAdj&lcGLPeriod + ROUND(Debit.Amount &lcExRSin Debit.nExRate &lcUntSin Debit.nCurrUnit,2)
    ENDIF          
  ENDIF
    
ENDSCAN

*-- 5) check Credit file.
DO CASE 
  CASE EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = '.T.'
  CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(TranDate),4) <= lcHigh'
  CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(TranDate),4) >= lcLow'
  CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(TranDate),4) >= lcLow AND STR(YEAR(TranDate),4) <= lcHigh'
ENDCASE

*-- To make rushmore expression.
lcExpr = STRTRAN(lcFilter,'Account','Account + Tran + DTOS(TranDate)')

SELECT Credit
SCAN FOR &lcExpr
  IF &lcDateExpr AND TranType <> '0'
    WAIT WINDOW 'Fix Customer History File from Credit Transaction # ' + Tran NOWAIT
  
    STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
    =CHECKPRD(TranDate,'lcGlYear','lcGlPeriod','CR',.T.)
    IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcComp_ID + ": Credit Tran # " + Credit.Tran + " has Transaction date " + DTOC(Credit.TranDate) + " which does not fall within any period."
      laRebMsg[3] = " "
      =lfVryRport()
    ELSE
      lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
      lcUntSin = ''  
      lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)

      SELECT ALLTRIM(lcTempHdr.cTempName)
      IF Credit.TranType = '6' AND !EMPTY(Credit.Credt_Date)
        LOOP
      ENDIF
      IF !SEEK(Credit.Account+lcGlYear)
        APPEND BLANK
        REPLACE Account WITH Credit.Account , cFisFYear WITH lcGlYear
      ENDIF
      IF Credit.TranType = '4'    
        REPLACE nPayment            WITH nPayment + ABS(ROUND(Credit.Amount &lcExRSin Credit.nExRate &lcUntSin Credit.nCurrUnit,2)) ,;
                nPayment&lcGLPeriod WITH nPayment&lcGLPeriod + ABS(ROUND(Credit.Amount &lcExRSin Credit.nExRate &lcUntSin Credit.nCurrUnit,2))
      ELSE
        REPLACE nCrAdj            WITH nCrAdj + ABS(ROUND(Credit.Amount &lcExRSin Credit.nExRate &lcUntSin Credit.nCurrUnit,2)) ,;
                nCrAdj&lcGLPeriod WITH nCrAdj&lcGLPeriod + ABS(ROUND(Credit.Amount &lcExRSin Credit.nExRate &lcUntSin Credit.nCurrUnit,2))
      ENDIF
    ENDIF  
  ENDIF
ENDSCAN

*-- 6) check ArHist file.
DO CASE 
  CASE EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = '.T.'
  CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(TranDate),4) <= lcHigh'
  CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(TranDate),4) >= lcLow'
  CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(TranDate),4) >= lcLow AND STR(YEAR(TranDate),4) <= lcHigh'
ENDCASE

*-- To make rushmore expression.
lcExpr = STRTRAN(lcFilter,'Account','Account + Tran + cInstalNo + DTOS(TranDate)')

SELECT ArHist
SCAN FOR &lcExpr
  IF &lcDateExpr AND !(TranType $ '89')
    WAIT WINDOW 'Fix Customer History File from History Transaction # ' + Tran NOWAIT

    STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
    =CHECKPRD(TranDate,'lcGlYear','lcGlPeriod','',.T.)
    IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcComp_ID + ": History Tran # " + ArHist.Tran + " has Transaction date " + DTOC(Arhist.TranDate) + " which does not fall within any period."
      laRebMsg[3] = " "
      =lfVryRport()
    ELSE
      lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
      lcUntSin = ''  
      lcExRSin = gfGetExSin(@lcUntSin,cCurrCode)

      SELECT ALLTRIM(lcTempHdr.cTempName)
      IF (ArHist.TranType = '3' AND !EMPTY(ArHist.ChgBk_Date)) OR (ArHist.TranType = '6' AND !EMPTY(ArHist.Credt_Date))
        LOOP
      ENDIF
      IF !SEEK(ArHist.Account+lcGlYear)
        APPEND BLANK
        REPLACE Account WITH ArHist.Account , cFisFYear WITH lcGlYear
      ENDIF
    
      DO CASE
        CASE ArHist.TranType $ '23'
          REPLACE nDrAdj            WITH nDrAdj + ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2) ,;
                  nDrAdj&lcGLPeriod WITH nDrAdj&lcGLPeriod + ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2)
        CASE ArHist.TranType = '4'
          REPLACE nPayment            WITH nPayment + ABS(ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2)) ,;
                  nPayment&lcGLPeriod WITH nPayment&lcGLPeriod + ABS(ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2))
        CASE ArHist.TranType $ '56'
          REPLACE nCrAdj            WITH nCrAdj + ABS(ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2)) ,;
                  nCrAdj&lcGLPeriod WITH nCrAdj&lcGLPeriod + ABS(ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2))
        CASE ArHist.TranType = '7'
          REPLACE nAllow            WITH nAllow + ABS(ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2)) ,;
                  nAllow&lcGLPeriod WITH nAllow&lcGLPeriod + ABS(ROUND(ArHist.Amount &lcExRSin ArHist.nExRate &lcUntSin ArHist.nCurrUnit,2))
      ENDCASE
    ENDIF  
  ENDIF
ENDSCAN
WAIT CLEAR

IF !llUpdt
  SELECT ALLTRIM(lcTempHdr.cTempName)
  PRIVATE lnLoop , lcLoop
  DECLARE laRebMsg[3]
  SCAN
    WAIT WINDOW 'Reporting differences in Customer History file for Account : ' + Account +", Year : " + cFisFYear NOWAIT
    IF SEEK(Account+cFisFYear,'ArCusHst')
      FOR lnLoop = 1 TO 13
        lcLoop = PADL(ALLTRIM(STR(lnLoop,2)),2,'0')        
        laRebMsg[1] = " "
      
        *-- Checking Order Amount.
        IF nOrdAmt&lcLoop <> ArCusHst.nOrdAmt&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Order Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF
      
        *-- Checking Order Qty.
        IF nOrdQty&lcLoop <> ArCusHst.nOrdQty&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Order Quantity for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has quantity different than Customer History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Sales Amount.
        IF nSlsAmt&lcLoop <> ArCusHst.nSlsAmt&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Sales Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Sales Qty.
        IF nSlsQty&lcLoop <> ArCusHst.nSlsQty&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Sales Quantity for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has quantity different than Customer History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Discount Amount.
        IF nDisAmt&lcLoop <> ArCusHst.nDisAmt&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Discount Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Cost of goods sold Amount.
        IF nCogsAmt&lcLoop <> ArCusHst.nCogsAmt&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Cost of goods sold Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Return Amount.
        IF nRetAmt&lcLoop <> ArCusHst.nRetAmt&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Returned Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Return Amount.
        IF nRetQty&lcLoop <> ArCusHst.nRetQty&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Returned Quantity for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has quantity different than Customer History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Allowance.
        IF nAllow&lcLoop <> ArCusHst.nAllow&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Allowance Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Payment.
        IF nPayment&lcLoop <> ArCusHst.nPayment&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Payment Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Debit Adjustments.
        IF nDrAdj&lcLoop <> ArCusHst.nDrAdj&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Debit Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Credit Adjustments.
        IF nCrAdj&lcLoop <> ArCusHst.nCrAdj&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Credit Amount for Account : " + Account + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Customer History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF
      ENDFOR
    ELSE
      laRebMsg[2] = "Company" + lcComp_ID + ": has no record for Account code " + Account + ", Fiscal year " + cFisFYear + " in Customer History file."
      laRebMsg[3] = " "
      =lfVryRport()        
    ENDIF
  ENDSCAN
ENDIF

SELECT ALLTRIM(lcTempHdr.cTempName)
USE
*-- End of lfVryCsHst.

*!***************************************************************************
*! Name      : lfUpdCsHst
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/31/2002
*! Purpose   : Update function for ArCusHst.
*!***************************************************************************
*! Example   : = lfUpdCsHst()
*!***************************************************************************
*B605667,1
FUNCTION lfUpdCsHst
PRIVATE lcFile

IF FILE(lcFilePath + ALLTRIM(lcTempHdr.cTempName) + '.DBF')
  IF !USED(ALLTRIM(lcTempHdr.cTempName))
    USE (lcFilePath + ALLTRIM(lcTempHdr.cTempName)) IN 0 
  ENDIF
  SELECT ALLTRIM(lcTempHdr.cTempName)
  lcFile = ALLTRIM(lcTempHdr.cTempName)
  SCAN
    SCATTER MEMVAR MEMO
    SELECT ArCusHst
    IF !SEEK(&lcFile..Account+&lcFile..cFisFYear,'ArCusHst')
      APPEND BLANK
    ENDIF
    m.cAdd_User = ArCusHst.cAdd_User
    m.dAdd_Date = ArCusHst.dAdd_Date
    m.cAdd_Time = ArCusHst.cAdd_Time
    m.cEdit_User = gcUser_ID
    m.dEdit_Date = DATE()
    m.cEdit_Time = gfGetTime()
    =RLOCK()
    GATHER MEMVAR MEMO
    UNLOCK      
  ENDSCAN
  SELECT ALLTRIM(lcTempHdr.cTempName)
  USE
  ERASE(lcFilePath + ALLTRIM(lcTempHdr.cTempName)+'.DBF')
  ERASE(lcFilePath + ALLTRIM(lcTempHdr.cTempName)+'.CDX')  
ELSE
  llUpdt = .T.
  =lfStpCsHst()
  =lfVryCsHst()
  =lfUpdCsHst()
  llUpdt = .F.
ENDIF
*-- End of lfUpdCsHst.

*!***************************************************************************
*! Name      : lfvStyle
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/31/2002
*! Purpose   : Validation for Style code.
*!***************************************************************************
*! Example   : = lfvStyle()
*!***************************************************************************
*
FUNCTION lfvStyle


*B606833,1 Add variable to detect if we opened style file. [Begin]
*PRIVATE lcObjName , lcObjVal , lcStyleTag , llObjRet
PRIVATE lcObjName , lcObjVal , lcStyleTag , llObjRet , llOpnStyle
llOpnStyle = .F.
*B606833,1 Add variable to detect if we opened style file. [End]

lcObjName  = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal   = EVALUATE(SYS(18))   && Varible to hold  the value of the current GET field


IF !USED('Style')
*B606833,1 open style file in a variable. [Begin]
  *=gfOpenFile(lcFilePath+'Style', 'Style' ,'SH')
  llOpnStyle = gfOpenFile(lcFilePath+'Style', 'Style' ,'SH')
  *B606833,1 open style file in a variable. [End]
ENDIF

lcStyleTag = ORDER('Style')
SET ORDER TO TAG Style IN Style

*-- if the user want to browse or if the account he entered is not in the file
IF '?' $ lcObjVal OR (!EMPTY(lcObjVal) AND !SEEK(lcObjVal , 'Style'))
  lcObjVal   = gfStyBrw('I',@lcObjVal,'',.F.)
  &lcObjName = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
ENDIF    && end of if

*-- if the Style file did not have an active index
IF EMPTY(lcStyleTag)
  SET ORDER TO 0 IN Style
ELSE
  SET ORDER TO TAG (lcStyleTag) IN Style
ENDIF

*B606833,1 Close Style file if opened. [Begin]
IF llOpnStyle
  USE IN Style
ENDIF
*B606833,1 Close Style file if opened. [End]

*-- End of lfvStyle.

*!***************************************************************************
*! Name      : lfStpStHst
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/31/2002
*! Purpose   : Setup function for IcStyHst.
*!***************************************************************************
*! Example   : = lfStpStHst()
*!***************************************************************************
*B605667,1
FUNCTION lfStpStHst
PRIVATE lcCurrArea , laFields

IF lcTempHdr.cUpdVryIgn='U' AND !llUpdt
  RETURN
ENDIF
lcCurrArea = SELECT(0)
IF FILE(lcFilePath + ALLTRIM(lcTempHdr.cTempName)+'.DBF') 
  USE (lcFilePath + ALLTRIM(lcTempHdr.cTempName)) IN 0 EXCL
  SELECT ALLTRIM(lcTempHdr.cTempName)
  SET ORDER TO TAG (lcTempHdr.cTempName)
ELSE
  lcTag = lcTempHdr.cTempName
  SELECT IcStyHst
  =AFIELDS(laFields)
  CREATE TABLE (lcFilePath + ALLTRIM(lcTempHdr.cTempName)) FROM ARRAY laFields  
ENDIF

IF FILE(lcFilePath + ALLTRIM(lcTempHdr.cTempName)+'.CDX')               
  SET ORDER TO TAG ALLTRIM(lcTempHdr.cTempName)
ELSE
  INDEX ON Style+cFisFYear TAG &lcTag OF (lcFilePath +ALLTRIM(lcTempHdr.cTempName)+'.CDX')
ENDIF
SELECT (lcCurrArea)
*-- End of lfStpStHst.

*!***************************************************************************
*! Name      : lfVryStHst
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/31/2002
*! Purpose   : Verify function for IcStyHst.
*!***************************************************************************
*! Example   : = lfVryStHst()
*!***************************************************************************
*B605667,1
FUNCTION lfVryStHst

PRIVATE lcUntSin,lcExRSin,lcGlYear,lcGlPeriod,lnCOGSAmt,lnShipAmnt,lnDiscAmnt,;
        lcFilter,lcDateExpr,lcExpr,lcStyTitle
STORE '' TO lcGlYear,lcGlPeriod,lcUntSin
STORE 0 TO lnCOGSAmt,lnShipAmnt,lnDiscAmnt
lcStyTitle = gfItemMask('HI')
DECLARE laRebMsg[3]

*-- Zap Temp IcStyHst file first.
SELECT ALLTRIM(lcTempHdr.cTempName)

DO CASE 
  CASE EMPTY(SUBSTR(laFXFLT[7,6],1,19)) AND EMPTY(SUBSTR(laFXFLT[7,6],21,19))
    lcFilter = '.T.'
  CASE EMPTY(SUBSTR(laFXFLT[7,6],1,19)) AND !EMPTY(SUBSTR(laFXFLT[7,6],21,19))
    lcFilter = 'Style <= SUBSTR(laFXFLT[7,6],21,19)'
  CASE !EMPTY(SUBSTR(laFXFLT[7,6],1,19)) AND EMPTY(SUBSTR(laFXFLT[7,6],21,19))
    lcFilter = 'Style >= SUBSTR(laFXFLT[7,6],1,19)'
  CASE !EMPTY(SUBSTR(laFXFLT[7,6],1,19)) AND !EMPTY(SUBSTR(laFXFLT[7,6],21,19))
    lcFilter = 'Style >= SUBSTR(laFXFLT[7,6],1,19) AND Style <= SUBSTR(laFXFLT[7,6],21,19)'
ENDCASE

*-- 1) check InvHdr and InvLine files.
lcLow = SUBSTR(laFXFLT[6,6],1,4)
lcHigh = SUBSTR(laFXFLT[6,6],6,4)

DO CASE 
  CASE EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = '.T.'
  CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(InvHdr.dPostDate),4) <= lcHigh'
  CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(InvHdr.dPostDate),4) >= lcLow'
  CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(InvHdr.dPostDate),4) >= lcLow AND STR(YEAR(InvHdr.dPostDate),4) <= lcHigh'
ENDCASE

*-- To make rushmore expression.
lcExpr = STRTRAN(lcFilter,'Style','Style + Invoice + STR(LineNo,6)')

SELECT InvLine
SET RELATION TO Invoice INTO InvHdr ADDITIVE
SCAN FOR &lcExpr
  IF &lcDateExpr AND InvHdr.Status <> 'V'
    WAIT WINDOW 'Fix Style History File from Invoice # ' + Invoice NOWAIT
    STORE '' TO lcGlYear,lcGlPeriod,lcUntSin
    STORE 0 TO lnCOGSAmt,lnShipAmnt,lnDiscAmnt
    
    =CHECKPRD(InvHdr.dPostDate,'lcGlYear','lcGlPeriod','IN',.T.)
    IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcComp_ID + ": Invoice # " + Invoice + ", " + lcStyTitle + " : " + Style + " has Transaction date " + DTOC(InvHdr.dPostDate) + " which does not fall within any period."
      laRebMsg[3] = " "
      =lfVryRport()
    ELSE
      lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
      lcUntSin = ''  
      lcExRSin = gfGetExSin(@lcUntSin,InvHdr.cCurrCode)

      lnShipAmnt = TotQty * Price &lcExRSin InvHdr.nExRate &lcUntSin InvHdr.nCurrUnit
      lnDiscAmnt = TotQty * Price * InvHdr.DiscPcnt/100 &lcExRSin InvHdr.nExRate &lcUntSin InvHdr.nCurrUnit
            
      SELECT ALLTRIM(lcTempHdr.cTempName)
      IF !SEEK(InvLine.Style+lcGlYear)
        APPEND BLANK
        REPLACE Style WITH InvLine.Style , cFisFYear WITH lcGlYear
      ENDIF        
        REPLACE nSlsQty&lcGlPeriod  WITH nSlsQty&lcGlPeriod  + InvLine.TotQty   ,;
                nSlsQty             WITH nSlsQty             + InvLine.TotQty   ,;
                nSlsAmt&lcGlPeriod  WITH nSlsAmt&lcGlPeriod  + lnShipAmnt ,;
                nSlsAmt             WITH nSlsAmt             + lnShipAmnt ,;
                nDisAmt&lcGlPeriod  WITH nDisAmt&lcGlPeriod  + lnDiscAmnt ,;
                nDisAmt             WITH nDisAmt             + lnDiscAmnt ,;
                nCOGSAmt&lcGlPeriod WITH nCOGSAmt&lcGlPeriod + InvLine.TotQty*InvLine.Cost  ,;
                nCOGSAmt            WITH nCOGSAmt            + InvLine.TotQty*InvLine.Cost  
    ENDIF          
  ENDIF
ENDSCAN

*-- 2) check OrdHdr file.
DO CASE 
  CASE EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = '.T.'
  CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(OrdHdr.Entered),4) <= lcHigh'
  CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(OrdHdr.Entered),4) >= lcLow'
  CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(OrdHdr.Entered),4) >= lcLow AND STR(YEAR(OrdHdr.Entered),4) <= lcHigh'
ENDCASE

*-- To make rushmore expression.
lcExpr = STRTRAN(lcFilter,'Style','Style + DTOS(Complete) + cOrdType + Order + Store + STR(Lineno,6)')

SELECT OrdLine
SET RELATION TO cOrdType + Order INTO OrdHdr ADDITIVE
SCAN FOR &lcExpr
  IF &lcDateExpr AND !(OrdHdr.Status $ 'BX') AND !OrdHdr.Direct_Inv
    WAIT WINDOW 'Fix Style History File from Order # ' + Order NOWAIT

    STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
    =CHECKPRD(OrdHdr.Entered,'lcGlYear','lcGlPeriod','',.T.)
    IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcComp_ID + ": Order # " + Order + ", " + lcStyTitle + " : " + Style + " has Transaction date " + DTOC(OrdHdr.Entered) + " which does not fall within any period."
      laRebMsg[3] = " "
      =lfVryRport()
    ELSE
      lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
      lcUntSin = ''  
      lcExRSin = gfGetExSin(@lcUntSin,OrdHdr.cCurrCode)

      SELECT ALLTRIM(lcTempHdr.cTempName)
      IF !SEEK(OrdLine.Style+lcGlYear)
        APPEND BLANK
        REPLACE Style WITH OrdLine.Style , cFisFYear WITH lcGlYear
      ENDIF
      REPLACE  nOrdQty&lcGlPeriod WITH nOrdQty&lcGlPeriod + OrdLine.TotBook , ;
               nOrdQty WITH nOrdQty + OrdLine.TotBook , ;
               nOrdAmt&lcGlPeriod WITH nOrdAmt&lcGlPeriod + OrdLine.TotBook*OrdLine.Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit, ;
               nOrdAmt WITH nOrdAmt + OrdLine.TotBook*OrdLine.Price &lcExRSin OrdHdr.nExRate &lcUntSin OrdHdr.nCurrUnit
    ENDIF           
  ENDIF
ENDSCAN

*-- 3) check RetHdr file.
DO CASE 
  CASE EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = '.T.'
  CASE EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(RetHdr.CrDate),4) <= lcHigh'
  CASE !EMPTY(lcLow) AND EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(RetHdr.CrDate),4) >= lcLow'
  CASE !EMPTY(lcLow) AND !EMPTY(lcHigh)
    lcDateExpr = 'STR(YEAR(RetHdr.CrDate),4) >= lcLow AND STR(YEAR(RetHdr.CrDate),4) <= lcHigh'
ENDCASE

*-- To make rushmore expression.
lcExpr = STRTRAN(lcFilter,'Style','Style + CrMemo')

SELECT RetLine
SET RELATION TO CrMemo INTO RetHdr ADDITIVE
SCAN FOR &lcExpr
  IF &lcDateExpr
    WAIT WINDOW 'Fix Style History File from Return # ' + CrMemo NOWAIT

    STORE '' TO lcGlYear,lcGlPeriod,lcUntSin    
    =CHECKPRD(RetHdr.CrDate,'lcGlYear','lcGlPeriod','RM',.T.)
    IF EMPTY(lcGLYear) OR EMPTY(lcGLPeriod)
      laRebMsg[1] = " "
      laRebMsg[2] = "Company" + lcComp_ID + ": Return # " + CrMemo + ", " + lcStyTitle + " : " + Style + " has Transaction date " + DTOC(RetHdr.CrDate) + " which does not fall within any period."
      laRebMsg[3] = " "
      =lfVryRport()
    ELSE
      lcGlPeriod = PADL(ALLTRIM(lcGlPeriod),2,"0")
      lcUntSin = ''  
      lcExRSin = gfGetExSin(@lcUntSin,RetHdr.cCurrCode)

      SELECT ALLTRIM(lcTempHdr.cTempName)
      IF !SEEK(RetLine.Style+lcGlYear)
        APPEND BLANK
        REPLACE Style WITH RetLine.Style , cFisFYear WITH lcGlYear
      ENDIF
      REPLACE nRetAmt            WITH nRetAmt + ROUND(RetLine.Amount &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2) ,;
              nRetQty            WITH nRetQty + RetLine.TotQty ,;
              nDisAmt            WITH nDisAmt - ROUND(RetLine.Disc_Amt &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2) ,;
              nRetAmt&lcGlPeriod WITH nRetAmt&lcGlPeriod + ROUND(RetLine.Amount &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2) ,;
              nRetQty&lcGlPeriod WITH nRetQty&lcGlPeriod + RetLine.TotQty ,;
              nDisAmt&lcGlPeriod WITH nDisAmt&lcGlPeriod - ROUND(RetLine.Disc_Amt &lcExRSin RetHdr.nExRate &lcUntSin RetHdr.nCurrUnit,2)
    ENDIF          
  ENDIF
ENDSCAN

IF !llUpdt
  SELECT ALLTRIM(lcTempHdr.cTempName)
  PRIVATE lnLoop , lcLoop
  DECLARE laRebMsg[3]
  SCAN
    WAIT WINDOW 'Reporting differences in Style History file for ' + lcStyTitle + " : " + Style +", Year : " + cFisFYear NOWAIT
    IF SEEK(Style+cFisFYear,'IcStyHst')
      FOR lnLoop = 1 TO 13
        lcLoop = PADL(ALLTRIM(STR(lnLoop,2)),2,'0')        
        laRebMsg[1] = " "
      
        *-- Checking Order Amount.
        IF nOrdAmt&lcLoop <> IcStyHst.nOrdAmt&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Order Amount for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Style History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF
      
        *-- Checking Order Qty.
        IF nOrdQty&lcLoop <> IcStyHst.nOrdQty&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Order Quantity for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has quantity different than Style History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Sales Amount.
        IF nSlsAmt&lcLoop <> IcStyHst.nSlsAmt&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Sales Amount for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Style History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Sales Qty.
        IF nSlsQty&lcLoop <> IcStyHst.nSlsQty&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Sales Quantity for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has quantity different than Style History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Discount Amount.
        IF nDisAmt&lcLoop <> IcStyHst.nDisAmt&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Discount Amount for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Style History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Cost of goods sold Amount.
        IF nCogsAmt&lcLoop <> IcStyHst.nCogsAmt&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Cost of goods sold Amount for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Style History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Return Amount.
        IF nRetAmt&lcLoop <> IcStyHst.nRetAmt&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Returned Amount for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has amount different than Style History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF

        *-- Checking Return Amount.
        IF nRetQty&lcLoop <> IcStyHst.nRetQty&lcLoop
          laRebMsg[2] = "Company" + lcComp_ID + ": Returned Quantity for " + lcStyTitle + " : " + Style + ", Year : " + cFisFYear + " and Period # " + lcLoop + " has quantity different than Style History file."
          laRebMsg[3] = " "
          =lfVryRport()
        ENDIF
      ENDFOR
    ELSE
      laRebMsg[2] = "Company" + lcComp_ID + ": has no record for " + lcStyTitle + " : " + Style + ", Fiscal year " + cFisFYear + " in Customer History file."
      laRebMsg[3] = " "
      =lfVryRport()        
    ENDIF
  ENDSCAN
ENDIF

SELECT ALLTRIM(lcTempHdr.cTempName)
USE
*-- End of lfVryStHst.

*!***************************************************************************
*! Name      : lfUpdStHst
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 03/31/2002
*! Purpose   : Update function for IcStyHst.
*!***************************************************************************
*! Example   : = lfUpdStHst()
*!***************************************************************************
*B605667,1
FUNCTION lfUpdStHst
PRIVATE lcFile

IF FILE(lcFilePath + ALLTRIM(lcTempHdr.cTempName) + '.DBF')
  IF !USED(ALLTRIM(lcTempHdr.cTempName))
    USE (lcFilePath + ALLTRIM(lcTempHdr.cTempName)) IN 0 
  ENDIF
  SELECT ALLTRIM(lcTempHdr.cTempName)
  lcFile = ALLTRIM(lcTempHdr.cTempName)
  SCAN
    SCATTER MEMVAR MEMO
    SELECT IcStyHst
    IF !SEEK(&lcFile..Style+&lcFile..cFisFYear,'IcStyHst')
      APPEND BLANK
    ENDIF
    m.cAdd_User = IcStyHst.cAdd_User
    m.dAdd_Date = IcStyHst.dAdd_Date
    m.cAdd_Time = IcStyHst.cAdd_Time
    m.cEdit_User = gcUser_ID
    m.dEdit_Date = DATE()
    m.cEdit_Time = gfGetTime()
    =RLOCK()
    GATHER MEMVAR MEMO
    UNLOCK      
  ENDSCAN
  SELECT ALLTRIM(lcTempHdr.cTempName)
  USE
  ERASE(lcFilePath + ALLTRIM(lcTempHdr.cTempName)+'.DBF')
  ERASE(lcFilePath + ALLTRIM(lcTempHdr.cTempName)+'.CDX')  
ELSE
  llUpdt = .T.
  =lfStpStHst()
  =lfVryStHst()
  =lfUpdStHst()
  llUpdt = .F.
ENDIF
*-- End of lfUpdStHst.


*!*************************************************************
*! Name      : lfAddRec
*! Developer : Ashraf Sherif (ASH)
*! Date      : 07/11/2002
*! Purpose   : Add missed records to the StyDye file.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters      : lcWare (Missed WareHouse Code that will be added to the stydye file)
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfAddRec()
*!*************************************************************
*!B603992,1 
FUNCTION lfAddRec
PARAMETER lcSty,lcWare,lnCst

PRIVATE lcCurAlias
lcCurAlias = SELECT(0)

SELECT (lcSDyAls)
APPEND BLANK
REPLACE STYLE     WITH lcSty     ,;
        cWareCode WITH lcWare    ,;
        AVE_COST  WITH lnCst


SELECT (lcCurAlias)


