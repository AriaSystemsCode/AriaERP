*!************************************************************************
*! Program file  : icnmdrp
*! For Screen    : icnmdrp
*! System        : Aria Apparel System
*! Module        : Inventory Contol
*: Developer     : ABDOU ELGENDI
*! DATE          : 05/24/1999
*!************************************************************************
*! Calls         : 
*!           Procedures : lpSavScr,
*!                      : ..........
*!                      : ..........
*!           Functions  : lpShow    ,KeyValidation,lfvData_1,lfvData_2,
*!                      : lfvData_3 ,lfChkNDTxt   ,lfvDrpSiz,lfvDrpMxLn
*!                      : lfvDrpPric,lfvDrpCost   ,
*!************************************************************************
*! Passed Parameters  : lcNDAct   : NAME DROP ACCOUNT
*!                      lcNDStore : NAME DROP STORE
*!                      lcNDrpId  : NAME DROP STORE
*!************************************************************************
*! Example : DO icnamdrp
*!************************************************************************
*: Modifications      :
*E301311,1 WAB 08/25/1999 add  "Object link" button in the control pannel.
*!************************************************************************
*!********-- Variable declarations
* THREE PARAMETERS ("lcNDAct   = NAME DROP ACCOUNT") 
*                  ("lcNDStore = NAME DROP STORE  ")
*                  ("lcNDrpId  = NAME DROP ID     ")
PARAMETERS lcNDAct,lcNDStore , 'lcNDrpId'


DECLARE laKeyField[1,4]
STORE '' TO lcScFields, lcCustName

*-- Other variables

laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'Icnamdrpid'
laKeyField[1,4] = 1

*-- Add variable to be used in the read clauses of the screen.
*-- This variable hold the word "Modal" to change the screen type
*-- to be modal if called from the sales order screen.
lcModal   = IIF(TYPE('lcNDAct')='C' .AND. !EMPTY(lcNDAct) , "MODAL" , " ")
llBrowse  = .F.

*-- File openings, variable initialization, saving/restoring variables from file.
*-- screen    : lcBaseFile, lWindTitl, gcBaseWind
*-- base file : lcBrFields, lcFile_Ttl

IF !gfSetup()    
  RETURN
ENDIF  
 
*WAIT 'Browse name drops for an account + confirm at cancel message' window
llNoShow  = .F.
llAlowNew = .T.   

*-- If You Come From The Calling Program Which Came in Add Mode only.
IF TYPE('lcNDAct') = 'C' .AND. !EMPTY (lcNDAct)
  STORE .F. TO laScrMode
  STORE .T. TO laScrMode[4]
ENDIF  

*-- fields list to be scattered to Ladata array
laDefProc[9]    = .F.       && Use a local save procedure.

*lcScFields = 'cndrpid, account,store,cndrptt,cndrptb,cspecins,cndrpclr,' + ;
	         'cndrpfnt,nndrpsize,nndmaxln,cndrpspr,cndrpco'

lcScFields = 'cndrpid, account,store,cndrptt,cndrptb,cspecins,CNDRPCLR,' + ;
	         'cndrpfnt,NNDRPPSIZE,NNDRPMAXLN,NNDRPPRIC,NNDRPCOST'


*E301311,1 WAB  - To add the "Object link" button in the control pannel.
*E301311,1 WAB - START
DECLARE laPanelObj[1,3]
STORE '' TO laPanelObj
*--Object link button.
laPanelObj[1,1] = 'pbObjlnk'
laPanelObj[1,2] = gcBmpHome+'RELATE.BMP'
laPanelObj[1,3] = [VALID lfvNamOLnk() MESSAGE 'Object link' DISABLE ]
*E301311,1 WAB - END


*-- The first time we run the screen, 
IF !WEXIST(gcBaseWind)
  *-- Initialize some variables
  *-- Create temporary files

 SELECT (lcBaseFile)
 SCATTER FIELDS &lcScFields MEMO TO laData BLANK
ENDIF

lcAcctStat = IIF(laScrMode[1] .OR. laScrMode[4] .AND. EMPTY(lcModal) .AND. EMPTY(laData[2]),;
			     "ENABLE" ,"DISABLE")

lcStorStat = IIF((laScrMode[1] .OR. laScrMode[4]) .AND. !EMPTY(laData[2]),;
			     "ENABLE" ,"DISABLE")

lcAddSTat = IIF(laScrMode[3] .OR. laScrMode[4] .AND.;
								  (!EMPTY(laData[2]) .OR. EMPTY(lcModal)),;
			    "ENABLE" ,"DISABLE")

SELECT (lcBaseFile)
SET ORDER TO TAG ICNAMDRPID

DO (gcScrDir + gcWinAppl + '\icnmdrp.SPX')
IF glQuitting
  *-- Erase temp files
  *-- Reset relations
ENDIF  
*!************************************************************************
*-- Functions and Procedures :
*!************************************************************************
*! Name      : lpShow
*! Developer : ABDOU ELGENDI
*! Date      : 05/24/99
*! Purpose   : Show objecrts function.
*!************************************************************************
*! Parameters: None
*!************************************************************************
*! Returns   :  None.
*!************************************************************************
*! Example   :  =lpShow()
*!************************************************************************
*
FUNCTION lpShow

DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]
    IF TYPE('lcNDAct') = 'C' .AND. !EMPTY (lcNDAct)
     glQuitting  = .T. 
     CLEAR READ
     RETURN
    ENDIF

    SHOW GET ibAccount   ENABLE
    SHOW GET laData[2]   ENABLE
    SHOW GET ibStore     DISABLE
    SHOW GET laData[3]   DISABLE
    SHOW GET laData[4]   DISABLE
    SHOW GET laData[5]   DISABLE
    SHOW GET laData[6]   DISABLE
    SHOW GET laData[7]   DISABLE
    SHOW GET laData[8]   DISABLE
    SHOW GET laData[9]   DISABLE
    SHOW GET laData[10]  DISABLE
    SHOW GET laData[11]  DISABLE
    SHOW GET laData[12]  DISABLE   

    *E301311,1 WAB  - DISABLE button "Object link"  (Select mode)
    *E301311,1 WAB - START
    SHOW GET pbObjlnk DISABLE
    *E301311,1 WAB - END

  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]

    SHOW GET ibAccount   DISABLE
    SHOW GET laData[2]   DISABLE
    SHOW GET ibStore     DISABLE
    SHOW GET laData[3]   DISABLE
    SHOW GET laData[4]   DISABLE
    SHOW GET laData[5]   DISABLE
    SHOW GET laData[6]   DISABLE
    SHOW GET laData[7]   DISABLE
    SHOW GET laData[8]   DISABLE
    SHOW GET laData[9]   DISABLE
    SHOW GET laData[10]  DISABLE
    SHOW GET laData[11]  DISABLE
    SHOW GET laData[12]  DISABLE   

    *E301311,1 WAB  - ENABLE button "Object link"  (View mode)
    *E301311,1 WAB - START
    SHOW GET pbObjlnk ENABLE
    *E301311,1 WAB - END

  ***--- E D I T   M O D E ---***
  CASE laScrMode[3]
  
    SHOW GET ibAccount   DISABLE
    SHOW GET laData[2]   DISABLE
    SHOW GET ibStore     DISABLE
    SHOW GET laData[3]   DISABLE
    SHOW GET laData[4]   ENABLE
    SHOW GET laData[5]   ENABLE
    SHOW GET laData[6]   ENABLE
    SHOW GET laData[7]   ENABLE
    SHOW GET laData[8]   ENABLE
    SHOW GET laData[9]   ENABLE
    SHOW GET laData[10]  ENABLE
    SHOW GET laData[11]  ENABLE
    SHOW GET laData[12]  ENABLE

    *E301311,1 WAB  - ENABLE button "Object link"  (Edit mode)
    *E301311,1 WAB - START
    SHOW GET pbObjlnk ENABLE
    *E301311,1 WAB - END

  ***--- A D D   M O D E ---***
  CASE laScrMode[4]
  
    IF TYPE('lcNDAct') = 'C' .AND. !EMPTY (lcNDAct)
      laData[2] =lcNDAct
      laData[3] =lcNDStore
      SHOW GET ibAccount  DISABLE
      SHOW GET laData[2]  DISABL
      SHOW GET laData[4]   ENABLE
      SHOW GET laData[5]   ENABLE
      SHOW GET laData[6]   ENABLE
      SHOW GET laData[7]   ENABLE
      SHOW GET laData[8]   ENABLE
      SHOW GET laData[9]   ENABLE
      SHOW GET laData[10]  ENABLE
      SHOW GET laData[11]  ENABLE
      SHOW GET laData[12]  ENABLE
    ENDIF  
      SHOW GET ibAccount  ENABLE
      SHOW GET laData[2]  ENABLE
    IF EMPTY(laData[2])
      SHOW GET ibStore     DISABLE  
      SHOW GET laData[3]   DISABLE
    ELSE
      SHOW GET ibStore     ENABLE
      SHOW GET laData[3]   ENABLE
    ENDIF  
    *E301311,1 WAB  - DISABLE button "Object link"  (ADD mode)
    *E301311,1 WAB - START
    SHOW GET pbObjlnk DISABLE
    *E301311,1 WAB - END
      
ENDCASE
SELECT (lcBaseFile)
*-- END OF lpShow
*!************************************************************************
*! Name      : lfvData_1
*! Developer : ABDOU ELGENDI
*! Date      : 05/23/99
*! Purpose   : seek for Account & Store suitable to customer
*!************************************************************************
*! Parameters: None
*!************************************************************************
*! Returns   :  None.
*!************************************************************************
*! Example   :  =lfvData_1()
*!************************************************************************
*
FUNCTION lfvData_1

SELECT ICNAMDRP
IF llBrowse .OR. !EMPTY(laData[1])
  IF EMPTY(laData[2])
    =gfSeekRec()
  ELSE
    lnCurTag = VAL(SYS(21))
    SET ORDER TO TAG ICNAMDRP
    IF SEEK(laData(2) + laData(3)+ laData[1]) 
      *-- Switch to view mode
      SET ORDER TO (lnCurTag)
      laScrMode    = .F.
      laScrMode[2] = .T.
      SHOW GETS
    ELSE
      =gfBrows([ALLTRIM(laData(2) + laData(3))])  
      SET ORDER TO (lnCurTag)
    ENDIF
  ENDIF
  llBrowse = .F.
ENDIF

*-- END OF lfvData_1
*!************************************************************************
*! Name      : lfvData_2
*! Developer : ABDOU ELGENDI
*! Date      : 05/30/99
*! Purpose   : seek for Account suitable to customer 
*!				from a customer file
*!************************************************************************
*! Parameters: None
*!************************************************************************
*! Returns   :  None.
*!************************************************************************
*! Example   :  =lfvData_2()
*!************************************************************************
*
FUNCTION lfvData_2
PRIVATE lcAccount

IF MDOWN()
  RETURN
ENDIF

IF llBrowse .OR. !EMPTY(laData[2]) 
  IF llBrowse .OR. !SEEK('M'+laData[2],'CUSTOMER')
    lcAccount = laData[2]
    SELECT CUSTOMER
    GOTO TOP
    DO CUSBROWM WITH lcAccount
    laData[2] = lcAccount
  ENDIF  
  DO CASE
    *-- IF You Come From Select Mode
    CASE laScrMode[1]
      IF EMPTY (laData[2])
        laData[3] = SPACE(8)
        SHOW GET ibStore    DISABLE
        SHOW GET laData[3]  DISABLE
        _CUROBJ = _CUROBJ
      ELSE
        SHOW GET ibStore    ENABLE
        SHOW GET laData[3]  ENABLE
      ENDIF 
  
    *-- IF You Come From Add Mode
    CASE laScrMode[4]
      IF !EMPTY (laData[2])
        SHOW GET laData[2]   DISABLE
        SHOW GET ibAccount   DISABLE
        SHOW GET ibStore     ENABLE
        SHOW GET laData[3]   ENABLE
  
        SHOW GET laData[4]   ENABLE
        SHOW GET laData[5]   ENABLE
        SHOW GET laData[6]   ENABLE
        SHOW GET laData[7]   ENABLE
        SHOW GET laData[8]   ENABLE
        SHOW GET laData[9]   ENABLE
        SHOW GET laData[10]  ENABLE
        SHOW GET laData[11]  ENABLE
        SHOW GET laData[12]  ENABLE
      ELSE
        _CUROBJ = _CUROBJ
      ENDIF 
  ENDCASE
  =lfRefresh()
  llBrowse = .F.
ENDIF
*-- END OF lfvData_2
*!************************************************************************
*! Name      : lfvData_3
*! Developer : ABDOU ELGENDI
*! Date      : 05/30/99
*! Purpose   : seek for Account suitable to customer 
*!				from a customer file
*!************************************************************************
*! Parameters: None
*!************************************************************************
*! Returns   :  None.
*!************************************************************************
*! Example   :  =lfvData_3()
*!************************************************************************
*
FUNCTION lfvData_3
PRIVATE xStore
IF llBrowse .OR. (!EMPTY(laData[3]) .AND. ;
                  !SEEK('S'+laData[2]+laData[3],'CUSTOMER'))
  xStore = laData[3]
  SELECT CUSTOMER
  IF CUSBROWS(laData[2],.T.)
    laData[3] = xStore
  ELSE
    STORE SPACE(8) TO laData[3]
  ENDIF  
  =lfRefresh() 
ENDIF
llBrowse = .F.
*-- END OF lfvData_3   
*!************************************************************************
*! Name      : lfChkNDTxt
*! Developer : ABDOU ELGENDI
*! Date      : 05/31/1999
*! Purpose   : Validate Name Drop Text Top defaults 
*!					OR Name Drop Text Bottom
*!************************************************************************
*! Passed Parameters  :  None
*!************************************************************************
*! Returns            :  None
*!************************************************************************
*! Example            :  =lfChkNDTxt()
*!************************************************************************
*
FUNCTION lfChkNDTxt
IF EMPTY(laData[4]) .AND. EMPTY(laData[5])
  =gfModalGen('TRM00250B00000','ALERT',"name drop text top or text bottom")
  _CUROBJ = OBJNUM(laData[4])
  llCSave = .F.		
 RETURN .F.
ENDIF
*-- END OF lfChkNDTxt
*!************************************************************************
*! Name      : lpSavScr
*! Developer : ABDOU ELGENDI
*! Date      : 05/31/1999
*! Purpose   : Save The Data From The Array Into The File
*!************************************************************************
*! Passed Parameters  :  None
*!************************************************************************
*! Returns            :  None
*!************************************************************************
*! Example            : lpSavScr()
*!************************************************************************
*
PROCEDURE lpSavScr
 *-- If there is any reason you should not be able to save
  IF lfChkNDTxt()
   *-- Create a header
   IF laScrMode[4]
    APPEND BLANK
    laData[1]= gfsequence('cndrpid')		
    lcNDrpId = laData[1]
   ENDIF
   GATHER FROM laData FIELDS &lcScFields
  ENDIF   
SELECT (lcBaseFile)
*-- END OF lpSavScr
*!************************************************************************
*! Name      : lfvDrpSiz
*! Developer : ABDOU ELGENDI
*! Date      : 05/31/1999
*! Purpose   : To valid The Point Size If Less than 0 .
*!************************************************************************
*! Passed Parameters  :  None
*!************************************************************************
*! Returns            :  None
*!************************************************************************
*! Example            : =lfvDrpSiz()
*!************************************************************************
*
FUNCTION lfvDrpSiz
  IF laData[9] < 0
  laData[9] =0 
  SHOW GET laData[9]
  ENDIF
*-- END OF lfvDrpSiz  
*!************************************************************************
*! Name      : lfvDrpMxLn
*! Developer : ABDOU ELGENDI
*! Date      : 05/31/1999
*! Purpose   : To valid The Max Length  If Less than 0 .
*!************************************************************************
*! Passed Parameters  :  None
*!************************************************************************
*! Returns            :  None
*!************************************************************************
*! Example            : =lfvDrpMxLn()
*!************************************************************************
*
FUNCTION lfvDrpMxLn
  IF laData[10] < 0
  laData[10] =0 
  SHOW GET laData[10]
  ENDIF
*-- END OF lfvDrpMxLn
*!************************************************************************
*! Name      : lfvDrpPric
*! Developer : ABDOU ELGENDI
*! Date      : 05/31/1999
*! Purpose   : To valid The Price If Less than 0 .
*!************************************************************************
*! Passed Parameters  :  None
*!************************************************************************
*! Returns            :  None
*!************************************************************************
*! Example            : =lfvDrpPric()
*!************************************************************************
*
FUNCTION lfvDrpPric
  IF laData[11] < 0
  laData[11] =0 
  SHOW GET laData[11]
  ENDIF
*-- END OF lfvDrpPric
*!************************************************************************
*! Name      : lfvDrpCost
*! Developer : ABDOU ELGENDI
*! Date      : 05/31/1999
*! Purpose   : To valid The Point Size If Less than 0 .
*!************************************************************************
*! Passed Parameters  :  None
*!************************************************************************
*! Returns            :  None
*!************************************************************************
*! Example            : =lfvDrpCost()
*!************************************************************************
*
FUNCTION lfvDrpCost
  IF laData[12] < 0
  laData[12] =0 
  SHOW GET laData[12]
  ENDIF
*-- END OF lfvDrpCost


*!**************************************************************************
*! Func. Name: lfvNamOLnk()
*! Developer : WAB - Walid A. Wahab
*! Date      : 08/25/1999
*! Purpose   : Call Get Object link Prog.with (type 'N' and ladata[1])
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Parameters: None
*!**************************************************************************
*! Returns   :  None.
*!**************************************************************************
*! Example   :  =lfvNamOLnk()
*E301311,1 WAB 
*!**************************************************************************
FUNCTION lfvNamOLnk
DO GETOBJ WITH 'N',laData[1]

