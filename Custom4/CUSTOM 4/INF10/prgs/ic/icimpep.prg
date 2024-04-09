*:****************************************************************
*: Program file  : ICIMPEP.PRG
*: Program desc. : Importing Form Excel File [FOR INF10]
*: System        : Aria Apparel System - Version 4XP
*: Module        : Inventory Control (IC)
*: Developer     : Mariam Mazhar- [MMT]
*: Date          : 08/14/2006.
*: Tracking Job Number : C#121033,1
*:****************************************************************
*: Calls         : 
*:               : FUNCTIONS  : lfvgetFile , lfvProceed , lfCrtTemp
*:               :            : lfOpenFls  , lfClosFls  , lfStartCon 
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#121033,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- lcMainTemp :- Temp file will hold all the transaction lines.
*-- lcFileName :- Variable hold the name and Path for the Excel file.
*-- llOpenFils :- Variable To know If we open any file ort not.
*-- laOpenFile :- Array Hold the Opened Files.
*-- lctmpAdj   :- File hold the phisical transaction.
*-- lcType     :- Variable hold 'P' for Physical invantory.
*-- lcDefWareh :- Variable hold the default warehouse.
*-- lcAReason  :- variable hold the defauld reason code.
*-- lcFromWare : Variable hold the default warehouse.

DO FORM (oAriaApplication.ScreenHome+ oAriaApplication.ActiveModuleID+ '\ICIMPEP.SCX')
*:*************************************************************
*: Name      : lfvgetFile
*: Developer : Mariam Mazhar- (MMT)
*: Date      : 08/14/2006
*: Purpose   : Function to get the Excel file data Dir & File.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : lcVarNam = ladata [13]
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvApData ()
*:*************************************************************
* 
FUNCTION lfvgetFile
PARAMETERS lcVarNam 
PRIVATE lcOld_Path

lcOld_Path = FULLPATH('')

lcFileName = GETFILE('XLS', 'Excel sheet Path : ','Select') 

SET DEFAULT TO &lcOld_Path

IF EMPTY(lcFileName)
  RETURN ''  	
ELSE
  RETURN lcFileName	
ENDIF
*-- End Of lfvgetFile

*:*************************************************************
*: Name      : lfvProceed
*: Developer : Mariam Mazhar - (MMT)
*: Date      : 08/14/2006
*: Purpose   : To import the Excel file. 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : lcVarNam = ladata [13]
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example            :  lfvProceed()
*:*************************************************************
*:
FUNCTION lfvProceed
PARAMETERS lcFile



DIMENSION laOpenFile[07,03] , laOpFile[03]
STORE ''  TO lcFileName , lcMainTemp , lctmpAdj , lcDefWareh , lcAReason,;
             lcGlFyear  , lcGlPeriod , lcFromWare
STORE .F. TO llOpenFils , llGlLink , llOpenGLD , laOpenFile , laOpFile,llReBalance,;
             llUpdatPh
STORE 0   TO lnNewCost,lnOldCost
lcType = 'P'
*-- End Declaration Variabls
lcMainTemp = gfTempName()
lcGLTemp   = gfTempName()
lctmpAdj   = gfTempName()

loDBFStyle      = CreateObject("RemoteTable",'Style','Style','Style',SET("DATASESSION"))
loDBFSTYINVJL   = CreateObject("RemoteTable","STYINVJL","STYINVJL",'STYINVJL',SET("DATASESSION"))
loDBFSTYDYE 	= CreateObject("RemoteTable","STYDYE","STYDYE",'STYDYE',SET("DATASESSION"))
loDBFINVTADJ 	= CreateObject("RemoteTable","INVTADJ","INVTADJ",'INVTADJ',SET("DATASESSION"))
loDBFSEQUENCE   = CreateObject("RemoteTable","SEQUENCE","Cseq_type",'SEQUENCE',SET("DATASESSION"))
loDBFWAREHOUS	= CreateObject("RemoteTable","WAREHOUS","WAREHOUS",'WAREHOUS',SET("DATASESSION"))
loDBFCODES		= CreateObject("RemoteTable","CODES","Ccode_no",'CODES',SET("DATASESSION"))

llGlLink   = ALLTRIM(gfGetMemVar("M_LINK_GL"))  = "Y"
llDyelot   = ALLTRIM(gfGetMemVar('M_Dyelot'))   ='Y'
llWareLoc  = ALLTRIM(gfGetMemVar('M_WareLoc'))  ='Y'
llMultiWH  = ALLTRIM(gfGetMemVar('M_WareHouse'))='Y'
lcCostMth  = gfGetMemVar('M_Cost_Meth')
ldPstDate  = oAriaApplication.SystemDate


IF llGlLink
  IF TYPE("loDBFGLDIST") <> 'O'
    llOpenGLD = .T.
    loDBFGLDIST = CreateObject("RemoteTable","GLDIST","GLDISTAC",'GLDIST',SET("DATASESSION"))
  ELSE
    llOpenGLD = .F.
    SELECT GLDIST
    loDBFGLDIST.Setorder('GLDISTAC')
  ENDIF
  
  IF !FILE(oAriaApplication.WorkDir+lcGLTemp)
    *-- Create temp. file hold the gl entries.
    =AFIELDS(laFileStru)
    CREATE TABLE (oAriaApplication.WorkDir+lcGLTemp) FROM ARRAY laFileStru
  ENDIF
ENDIF


=CHECKPRD(ldPstDate,'lcGLFYear','lcGLPeriod','IP',.T.)
IF EMPTY(lcFile)
  *--- Path name can not be empty.
  =gfModalGen('TRM04074B00000','DIALOG','Path name')
  RETURN
ENDIF

*-- Check if File Is .Csv file.
IF !(".XLS" $ lcFile)
  *-- "This file can not be selected you must select a file of .XLS type."
  lcMessage = "This file can not be selected you must select a file of .XLS type."
  = gfModalGen("INM00000B00000","F","ALERT"," ",lcMessage)
  RETURN
ENDIF

*-- Check if the file is exist or not.
IF !FILE(lcFile)
  * -- "File does not exist. Cannot proceed."
  =gfModalGen('TRM00273B00000','DIALOG')
  RETURN
ENDIF

*-- Function to create the file that will hold the qty.
lcMainTemp = lfCrtTemp(lcFile)

SELECT (lcMainTemp)
LOCATE
IF EOF()
  *-- "This file can not be selected you must select a file of .XLS type."
  lcMessage = "There is no record into Excel file to import, can not proceed."
  = gfModalGen("INM00000B00000","F","ALERT"," ",lcMessage)
  RETURN
ENDIF
*-- Open Needed files.
= lfOpenFls ()
*-- Function to Convert the Excel file.
= lfStartCon (lcMainTemp)
*-- close opened files.
= lfClosFls ()
RETURN
*-- End OF lfvProceed.
*:*************************************************************
*: Name      : lfCrtTemp
*: Developer : Mariam Mazhar - (MMT)
*: Date      : 02/17/2004
*: Purpose   : To import the Excel file. 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  :None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example            :  lfCrtTemp()
*:*************************************************************
*:
FUNCTION lfCrtTemp
PARAMETERS lcFileName
PRIVATE lnPrvAlias 
lnPrvAlias = SELECT(0)
lcMainTemp = gfTempName()
DIMENSION laFileStru[14,4]

laFileStru[1,1] = 'STYLE'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 19
laFileStru[1,4] = 0

laFileStru[2,1] = 'CSTYMAJOR'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 19
laFileStru[2,4] = 0

laFileStru[3,1] = 'CSTYGROUP'
laFileStru[3,2] = 'C'
laFileStru[3,3] = 6
laFileStru[3,4] = 0

laFileStru[4,1] = 'Scale'
laFileStru[4,2] = 'C'
laFileStru[4,3] = 3
laFileStru[4,4] = 0

laFileStru[5,1] = 'DESC1'
laFileStru[5,2] = 'C'
laFileStru[5,3] = 60
laFileStru[5,4] = 0

laFileStru[6,1] = 'STK1'
laFileStru[6,2] = 'N'
laFileStru[6,3] = 7
laFileStru[6,4] = 0

laFileStru[7,1] = 'STK2'
laFileStru[7,2] = 'N'
laFileStru[7,3] = 7
laFileStru[7,4] = 0

laFileStru[8,1] = 'STK3'
laFileStru[8,2] = 'N'
laFileStru[8,3] = 7
laFileStru[8,4] = 0

laFileStru[9,1] = 'STK4'
laFileStru[9,2] = 'N'
laFileStru[9,3] = 7
laFileStru[9,4] = 0

laFileStru[10,1] = 'STK5'
laFileStru[10,2] = 'N'
laFileStru[10,3] = 7
laFileStru[10,4] = 0

laFileStru[11,1] = 'STK6'
laFileStru[11,2] = 'N'
laFileStru[11,3] = 7
laFileStru[11,4] = 0

laFileStru[12,1] = 'STK7'
laFileStru[12,2] = 'N'
laFileStru[12,3] = 7
laFileStru[12,4] = 0

laFileStru[13,1] = 'STK8'
laFileStru[13,2] = 'N'
laFileStru[13,3] = 7
laFileStru[13,4] = 0

laFileStru[14,1] = 'TOTSTK'
laFileStru[14,2] = 'N'
laFileStru[14,3] = 10
laFileStru[14,4] = 0


=gfCrtTmp(lcMainTemp,@laFileStru,'Style',lcMainTemp)
SELECT (lcMainTemp)
SET ORDER TO 
TRY 
  *-- Copying the excel file to the cursor.
  APPEND FROM (lcFileName) TYPE XLS
  LOCATE
  DELETE 
CATCH 
  lcMessage = 'No style(s) updated.'
  = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
ENDTRY 
*-- Delete the header record for the file header.
SELECT(lnPrvAlias)
RETURN (lcMainTemp)
*-- End OF lfCrtTemp.
*:*************************************************************
*: Name      : lfOpenFls
*: Developer : Mariam Mazhar-  (MMT)
*: Date      : 08/14/2006
*: Purpose   : Function to open needed files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfOpenFls ()
*:*************************************************************
*:
FUNCTION lfOpenFls
PRIVATE lnPrvAlias 

lnPrvAlias = SELECT(0)

*--- Array laOpenFile column 1 :- Hold the name of the file.
*--- Array laOpenFile column 2 :- Hold the name of the index file.
*--- Array laOpenFile column 3 :- Hold true in case open the file .
*-- Aria 27 Tables.

*!*	llOpenFils = .T.
*!*	laOpenFile[1,1] = 'STYLE'
*!*	laOpenFile[1,2] = 'STYLE'

*!*	laOpenFile[2,1] = 'STYINVJL'
*!*	laOpenFile[2,2] = 'STYINVJL'

*!*	laOpenFile[3,1] = 'STYDYE'
*!*	laOpenFile[3,2] = 'STYDYE'

*!*	laOpenFile[4,1] = 'INVTADJ'
*!*	laOpenFile[4,2] = 'INVTADJ'

*!*	laOpenFile[5,1] = 'SEQUENCE'
*!*	laOpenFile[5,2] = 'Cseq_type'

*!*	laOpenFile[6,1] = 'WAREHOUS'
*!*	laOpenFile[6,2] = 'WAREHOUS'

*!*	laOpenFile[7,1] = 'CODES'
*!*	laOpenFile[7,2] = 'Ccode_no'



*!*	FOR I = 1 To ALEN(laOpenFile,1)
*!*	  IF !USED(laOpenFile[I,1])
*!*	    laOpenFile[I,3] =gfOpenFile(gcDataDir+laOpenFile[I,1],laOpenFile[I,2], "SH")
*!*	  ENDIF
*!*	ENDFOR

*!*	IF llGlLink
*!*	  IF !USED("GLDIST")
*!*	    llOpenGLD = .T.
*!*	    =gfOpenFile(gcDataDir+"GLDIST",gcDataDir+"GLDISTAC","SH")
*!*	  ELSE
*!*	    llOpenGLD = .F.
*!*	    SELECT GLDIST
*!*	    SET ORDER TO TAG GLDISTAC
*!*	  ENDIF
*!*	  
*!*	  IF !FILE(gcWorkDir+lcGLTemp)
*!*	    *-- Create temp. file hold the gl entries.
*!*	    =AFIELDS(laFileStru)
*!*	    CREATE TABLE (gcWorkDir+lcGLTemp) FROM ARRAY laFileStru
*!*	  ENDIF
*!*	ENDIF


SELECT INVTADJ
*-- Create tmp adjustment file.
lcTmpAdj = gfTempName()
=AFIELDS(laFStru)
lnNo1=ASCAN(laFStru,'UNT_COST')
lnNo2=ASCAN(laFStru,'OLD_COST')
*--Make the lenth of this two fields as ave_cost field.
STORE 15 TO laFStru(lnNo1+2),laFStru(lnNo2+2)
STORE  7 TO laFStru(lnNo1+3),laFStru(lnNo2+3)
lnFStru = ALEN(laFStru,1)
DIMENSION laFStru[lnFStru+2,18]

laFStru[lnFStru+1,1] = 'cAdjReason'
laFStru[lnFStru+1,2] = 'C'
laFStru[lnFStru+1,3] = 6
laFStru[lnFStru+1,4] = 0

STORE ' ' TO  laFStru[lnFStru +1,7] ,laFStru[lnFStru +1,8],;
              laFStru[lnFStru +1,9] ,laFStru[lnFStru +1,10],;
              laFStru[lnFStru +1,11],laFStru[lnFStru +1,12],;
              laFStru[lnFStru +1,13],laFStru[lnFStru +1,14],;
              laFStru[lnFStru +1,15],laFStru[lnFStru +1,16]
STORE 0   TO  laFStru[lnFStru +1,17],laFStru[lnFStru +1,18]

laFStru[lnFStru+2,1] = 'cRefer'
laFStru[lnFStru+2,2] = 'C'
laFStru[lnFStru+2,3] = 6
laFStru[lnFStru+2,4] = 0

STORE ' ' TO  laFStru[lnFStru +2,7] ,laFStru[lnFStru +2,8],;
              laFStru[lnFStru +2,9] ,laFStru[lnFStru +2,10],;
              laFStru[lnFStru +2,11],laFStru[lnFStru +2,12],;
              laFStru[lnFStru +2,13],laFStru[lnFStru +2,14],;
              laFStru[lnFStru +2,15],laFStru[lnFStru +2,16]
STORE 0   TO  laFStru[lnFStru +2,17],laFStru[lnFStru +2,18]



=gfCrtTmp(lcTmpAdj,@laFStru,IIF(llDyelot,'Style+Dyelot+STR(RECNO(),6)','Style+STR(RECNO(),6)'),lcTmpAdj)

 *--Read the default warehouse.
GO TOP IN WAREHOUS
STORE WAREHOUS.cWareCode TO lcFromWare , lcDefWareh

IF llGlLink
  loDBFCODES.SEEK ('D'+'CADJREASON')
  lcAReason = Codes.cCODE_No
ENDIF

SELECT(lnPrvAlias)

*-- End OF lfOpenFls
*:*************************************************************
*: Name      : lfClosFls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to close opened files.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfClosFls ()
*:*************************************************************
*:
FUNCTION lfClosFls
IF TYPE('loDBFGLDIST') <> 'O'
  DIMENSION laTableUpdate[5]
ELSE
  DIMENSION laTableUpdate[6]
  laTableUpdate[6] = loDBFGLDIST
ENDIF 


laTableUpdate[1] = loDBFStyle
laTableUpdate[2] = loDBFSTYINVJL
laTableUpdate[3] = loDBFSTYDYE
laTableUpdate[4] = loDBFINVTADJ	
laTableUpdate[5] = loDBFSEQUENCE

=lfTableUpdate()


*!*	IF llOpenFils
*!*	  FOR I = 1 To ALEN(laOpenFile,1)
*!*	    IF USED(laOpenFile[I,1]) .AND.  laOpenFile[I,3]
*!*	      = gfCloseFile(laOpenFile[I,1])
*!*	    ENDIF
*!*	  ENDFOR
*!*	ENDIF


*!*	IF llOpenGLD .AND. USED("GLDIST")
*!*	  USE IN GLDIST 
*!*	ENDIF


*-- End OF lfClosFls.
*:*************************************************************
*: Name      : lfStartCon
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
*: Purpose   : Function to Convert the Excel file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : lfSave into ICInvSav.PRG
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Example   : = lfStartCon()
*:*************************************************************
*:
FUNCTION lfStartCon
PARAMETERS lcMainTemp
PRIVATE lnPrvAlias  , ldDate

lnPrvAlias = SELECT(0)
ldDate     = oAriaApplication.SystemDate

SELECT (lcMainTemp)
LOCATE
SCAN
  llUpdatPh = .T.
  WAIT WINDOW "Update Style # : " + Style  + "  ... Please Wait." NOWAIT
  SCATTER MEMVAR MEMO
  
  *-- Get the Old Cost from the style file.
  IF lodbfStyle.SEEK(M.Style)
    lnOldCost = IIF(lcCostMth<>'S',STYLE.Ave_Cost,STYLE.TotCost)
    *-- Save the Old & new cost as thge same cost.
 
    *-- Get the Old Stk,
    lnOldQty = Style.TotStk
  
    SELECT (lctmpAdj)
    APPEN BLANK
    REPLACE Style      WITH m.Style    ,;
            cFromWare  WITH lcFromWare ,;
            cAdjReason WITH lcAReason  ,;
            Date       WITH ldDate     ,;
            Type       WITH lcType     ,; 
            Unt_Cost   WITH lnNewCost  ,;
            Old_Cost   WITH lnOldCost  ,;
            Adj1       WITH M.STK1     ,;
            Adj2       WITH M.STK2     ,;
            Adj3       WITH M.STK3     ,;
            Adj4       WITH M.STK4     ,;
            Adj5       WITH M.STK5     ,;
            Adj6       WITH M.STK6     ,;
            Adj7       WITH M.STK7     ,;
            Adj8       WITH M.STK8     ,;                                                            
            TotAdj     WITH Adj1+Adj2+Adj3+Adj4+Adj5+Adj6+Adj7+Adj8,;
            TotOld     WITH lnOldQty   ,;
            GlFYear    WITH lcGlFyear  ,;
            GlPeriod   WITH lcGlPeriod ,;
            DpostDate  WITH ldPstDate

           =gfAdd_Info(lctmpAdj)  
  
    SELECT (lcMainTemp)
   ENDIF 
ENDSCAN

*-- Call the global save to save the Physical invantory for all styles.

DO lfSave IN (oAriaApplication.ApplicationHome+oAriaApplication.ActiveModuleID+'\ICInvSav.PRG') WITH .F.

IF llUpdatPh
  *- Message Text   :- Process completed successful.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  lcMessage = "Process completed successful."
  = gfModalGen("INM00000B00000","F","ALERT"," ",lcMessage)
ELSE
  *- Message Text   :- Global Update completed successful.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  lcMessage = 'No style(s) updated.'
  = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
ENDIF

SELECT(lnPrvAlias)
RETURN

*-- End Of lfStartCon
*!*************************************************************
*! Name      : lfTableUpdate
*! Developer : Mariam Mazhar Tawfik [MMT]
*! Date      : 08/15/2006
*! Purpose   : function to Update Sql Tables.
*!*************************************************************
FUNCTION lfTableUpdate

*--Open Dictionary files.
LOCAL lnAlias,lnConnectionHandlar,lcTranCode,lnI,llUpdate
lnAlias = SELECT(0)

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  SELECT (lnAlias)
  RETURN .F.
ENDIF

FOR lnI = 1 TO ALEN(laTableUpdate,1)
  llUpdate = laTableUpdate[lnI].TableUpdate(lcTranCode)
  IF !llUpdate
    =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
    SELECT (lnAlias)
    RETURN .F.
  ENDIF
ENDFOR

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  SELECT(lnAlias)
  RETURN .F.
ENDIF

SELECT(lnAlias)
*--end of lfTableUpdate.
*!*************************************************************
