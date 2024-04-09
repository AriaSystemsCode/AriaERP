*:****************************************************************
*: Program file  : SMPURPW
*: Program desc. : Purge PW Modulw Database .
*: System        : Aria Apparel System - Version 2.7.
*: Module        : System Manager - [SM]
*: Developer     : Nader Nabil -  [NNA]
*: Date          : 06/01/2004
*: Tracking Job Number : B122793,1
*:****************************************************************
*: Calls         : FUNCTIONS  : lfvCompany , lfGetComps , lfvPrDate
*:                            : lfvproceed
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:B122793,1
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- Inatial value for variables of company .
STORE {  /  /    } TO ldDefPrDt 
DECLARE laCompany[1,1],laRpCmpDsc[1,1] , lapurFiles[1,1],laCompid[1,1]
STORE ' ' TO laCompany , lcFilePath , lcCmpStat , lcComp_ID,laCompid
STORE .F. TO llHist

*-- Collect all companies
=lfGetComps()

*-- initialize all the global variables and opens the files needed by the program. 
IF !gfSetup()
  RETURN
ENDIF

ldPrDate = gdSysDate
DO (gcScrDir+gcWinAppl+"\SMPURPW.SPX")
*-- End of the program
*!*************************************************************
*! Name      : lfvCompany
*! Developer : Nader Nabil (NNA)
*! Date      : 06/01/2004
*! Purpose   : Valid Function for the Company Selected
*!*************************************************************
*! Called from : SMPURPW.Scx
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfvCompany()
*!*************************************************************
FUNCTION lfvCompany

*--Get the company ID from the selected bar of the popup
lcComp_ID  = IIF(SUBSTR(laCompany[puCompany,1],1,3)='N/A','N/A',SUBSTR(laCompany[puCompany,1],1,2))

*-- check if N/A Company.
lcComp_ID  = IIF(lcComp_ID = 'N/A' , SPACE(0) , lcComp_ID )
lnAryElem  = puCompany

IF EMPTY(lcComp_ID)
  STORE SPACE(0) TO lcComp_ID , lcOldVal
  IF EMPTY(lcComp_ID)
    =gfModalGen("TRM00000B00000","DIALOG","Purge",.F.,"No company selected, Cannot purge.")
  SHOW GET ldPrDate  DISABLE 
  SHOW GET pbProceed DISABLE
  puCompany       = 1
  SHOW GETS
  _CUROBJ = OBJNUM(puCompany)  
  ENDIF
ELSE
  llHist     = GfGetMemVar('LLHIST',lcComp_ID)
  lcHisCmpID = GfGetMemVar('M_COMP_ID',lcComp_ID)
  lcFilePath = laCompany[puCompany,2]
  IF !llHist .AND. EMPTY(lcHisCmpID)
    *-- Message 
    *** The Selected Company doesn't have a History Company. Select Another One.***
    *** <  Ok  > ***
    =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,;
               "The selected company does not have a history company.")
    SHOW GET ldPrDate  DISABLE 
    SHOW GET pbProceed DISABLE
    puCompany       = 1
    SHOW GETS
    _CUROBJ = OBJNUM(puCompany)  
  ELSE
    IF !(FILE(ALLTRIM(lcFilePath)+'PWCTKBOM.DBF') AND FILE(ALLTRIM(lcFilePath)+'PWBUNDL.DBF'))
      *-- Message 
      *** The Selected Company doesn't have a History Company. Select Another One.***
      *** <  Ok  > ***
      =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,;
                 "The selected company doesn't have files for PW module.")
      SHOW GET ldPrDate  DISABLE 
      SHOW GET pbProceed DISABLE
      puCompany       = 1
      SHOW GETS
      _CUROBJ = OBJNUM(puCompany)  
    ELSE
      SHOW GET ldPrDate  ENABLE 
      lcCmpStat = IIF(llHist,'H','A') && H=History, A= Active
    ENDIF
  ENDIF
ENDIF
*-- End of function lfvCompany
*!*************************************************************
*! Name      : lfGetComps
*! Developer : Nader Nabil (NNA)
*! Date      : 06/01/2004
*! Purpose   : Get company array
*!*************************************************************
*! Called from : SMPURPW.PRG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfGetComps()
*!*************************************************************
FUNCTION lfGetComps

PRIVATE lnComNo

*-- [x,1]company name  [x,2]directory of data  [x,3]Modules for this company.  
SELECT ccomp_id+" - "+cCom_Name,PADR(gfGetDataDir(ALLT(cCom_dDir)),LEN(cCom_dDir)),ALLT(syccomp.mcomp_mdl);
  FROM (gcSyshome+"syccomp") ;
  INTO ARRAY laRpCmpDsc ;
  ORDER BY 1
  puCompany       = 1

lnComNo = _TALLY

DECLARE laCompany[lnComNo+1,2] ,lacompID[lnComNo,1]
laCompany[1,1] ='N/A'
IF lnComNo > 0
  lnI = 1
  FOR lnI = 1 TO lnComNo
    laCompany[lnI+1,1] = laRpCmpDsc[lnI,1]
    laCompany[lnI+1,2] = laRpCmpDsc[lnI,2]
    laCompID[lnI,1] = SUBSTR(laRpCmpDsc[lnI,1],1,2)
  ENDFOR
ENDIF

*--- End of function lfGetComps.
*!*************************************************************
*! Name      : lfvPrDate
*! Developer : Nader Nabil (NNA)
*! Date      : 06/01/2004
*! Purpose   : valid function of purge date
*!*************************************************************
*! Called from : SMPURPW.SCX
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvPrDate()
*!*************************************************************
FUNCTION lfvPrDate

IF MDOWN() 
  RETURN
ENDIF

*-- Chech if Purge Date is Empty.
IF EMPTY(ldPrDate)
  *-- Message 
  *** Date Can't be Empty ***
  *** <  Ok  > ***
  =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"Date cannot be empty.")
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

*-- Check Purge date belonging in the Previous Year.
lnAlias = SELECT(0)
=gfOpenFile(ALLTRIM(lcFilePath)+'FISHD','Compfyear','SH')

SELECT FISHD
LOCATE FOR CFISYSTAT ='P'
IF ldPrDate > DFISBGDAT-1
  *-- Message 
  *** Date Can't be greater than or equal the Start Day of Previous Year ***
  *** <  Ok  > ***
  =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,;
              "Date cannot be greater than or equal the Start Date of the Previous Year.")
  _CUROBJ = _CUROBJ
  SHOW GET pbProceed DISABLE  
  RETURN
ENDIF
SELECT (lnAlias)
SHOW GET pbProceed ENABLE

IF USED('FISHD')
  USE IN FISHD
ENDIF
*--- End of Function lfvPrDate.

*!*************************************************************
*! Name      : lfvproceed
*! Developer : Nader Nabil (NNA)
*! Date      : 06/01/2004
*! Purpose   : Validate Function To Purge Database files for PW Module
*!*************************************************************
*! Called from : SMPURPW.Scx
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfvproceed()
*!*************************************************************
FUNCTION lfvproceed

PRIVATE lnSubsc, lcACmpDir , lcHCmpCod, lcHCmpDir
IF lcCmpStat= 'A'
  lcACmpDir = ALLTRIM(laCompany[puCompany,2])
  lcHisCmpID = GfGetMemVar('M_COMP_ID',lcComp_ID)
  lcHCmpDir = ALLTRIM(laCompany[ASCAN(laCompID,lcHisCmpID)+1,2])

  IF !(gfOpenFile(lcACmpDir+'PWBUNDL',lcACmpDir+'BARCODE','SH')   .AND. ;
       gfOpenFile(lcACmpDir+'PWCTKBOM',lcACmpDir+'PWCTKBOM','SH')) .AND. ;
       gfOpenFile(lcACmpDir+'PWCBGEN',lcACmpDir+'BUNDLE','SH'))        
    =gfModalGen("TRM00000B00000","DIALOG","Purge",.F.,"Copying master files failed.")
    RETURN
  ELSE
    IF USED('PWBUNDL')
      USE IN PWBUNDL
    ENDIF
    IF USED('PWCTKBOM')
      USE IN PWCTKBOM
    ENDIF

    IF FILE(lcACmpDir+'PWBUNDL.dbf') .AND. FILE(lcHCmpDir+'PWBUNDL.dbf')
      = gfOpenFile(lcACmpDir+'PWBUNDL',lcACmpDir+'BARCODE','EX','PWBUNDL')      
      = gfOpenFile(lcHCmpDir+'PWBUNDL',lcHCmpDir+'BARCODE','EX','PWBUN_HS')
      = gfOpenFile(lcACmpDir+'PWCTKBOM',lcACmpDir+'PWCTKBOM','EX','PWCTKBOM')      
      = gfOpenFile(lcHCmpDir+'PWCTKBOM',lcHCmpDir+'PWCTKBOM','EX','PWCTKB_HS')            
      = gfOpenFile(lcACmpDir+'PWCBGEN',lcACmpDir+'BUNDLE','EX','PWCBGEN')      
      = gfOpenFile(lcHCmpDir+'PWCBGEN',lcHCmpDir+'BUNDLE','EX','CBGEN_HS')            
      SELECT PWBUNDL
      COUNT TO lnTotRec FOR REC_DATE <= ldPrDate
      LOCATE
      lnRecno = 1
      SCAN FOR REC_DATE <= ldPrDate
        SCATTER MEMVAR MEMO
        *--NNA 
        *DELETE
        =gfThermo(lnTotRec,lnRecno,"Purge PWbundl and PWcbgen")          
        SELECT PWBUN_HS 
        APPEND BLANK
        GATHER MEMVAR MEMO
        *--in PWCBGen File CmBundle Field = PWBUNDLE.CbarCode
        IF SEEK(PWBUNDL.CBARCODE,'PWCBGEN')
          SELECT PWCBGEN
          SCAN REST WHILE Cmbundle=PWBUNDL.CBARCODE
            SCATTER MEMVAR MEMO
            DELETE
            INSERT INTO CBGEN_HS FROM MEMVAR
          ENDSCAN
        ENDIF
        SELECT PWBUNDL
        DELETE
        lnRecno = lnRecno + 1
      ENDSCAN
      SELECT PWBUNDL
      WAIT WINDOW "Please wait" NOWAIT        
      PACK
      USE IN PWBUNDL
      SELECT PWCBGEN
      PACK
      USE IN PWCBGEN
      SELECT DISTINCT CUTTKT FROM PWBUN_HS INTO CURSOR CUTTMP ORDER BY CUTTKT
      USE IN PWBUN_HS
      SELECT CUTTMP
      SET RELATION TO cuttkt INTO Pwctkbom ADDITIVE
      SET SKIP TO PWCTKBOM
      SELECT CUTTMP
      SCAN
        SELECT PWCTKBOM
        IF !EMPTY(CUTTKT)
          SCATTER MEMVAR MEMO
          =gfThermo(RECCOUNT('CUTTMP'),RECNO('CUTTMP'),"Purge PWctkbom")
          DELETE
          INSERT INTO PWCTKB_HS FROM MEMVAR
        ELSE
          SKIP IN CUTTMP
        ENDIF
      ENDSCAN
      SELECT PWCTKBOM
      PACK
      USE IN PWCTKBOM
      USE IN CUTTMP     
      USE IN PWCTKB_HS           
    ENDIF
    =gfModalGen("TRM00000B00000","DIALOG","Purge",.F.,"Purge is complete.")
  ENDIF 
  CLEAR READ
ENDIF

*--- End of Purge Program.
