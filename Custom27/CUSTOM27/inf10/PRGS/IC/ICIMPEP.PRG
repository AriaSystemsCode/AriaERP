*:****************************************************************
*: Program file  : ICIMPEP.PRG
*: Program desc. : Importing Form Excel File [FOR INF10]
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Sales Order (SO)
*: Developer     : Abdou Elgendy - [ABD]
*: Date          : 02/09/2004.
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

*-- Start Declaration Variabls
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


llGlLink   = ALLTRIM(gfGetMemVar("M_LINK_GL"))  = "Y"
llDyelot   = ALLTRIM(gfGetMemVar('M_Dyelot'))   ='Y'
llWareLoc  = ALLTRIM(gfGetMemVar('M_WareLoc'))  ='Y'
llMultiWH  = ALLTRIM(gfGetMemVar('M_WareHouse'))='Y'
lcCostMth  = gfGetMemVar('M_Cost_Meth')
ldPstDate  = gdSysdate
=CHECKPRD(ldPstDate,'lcGLFYear','lcGLPeriod','IP',.T.)

IF !gfSetup()
  RETURN
ENDIF

PUSH KEY
DO (gcScrDir + gcWinAppl + '\ICIMPEP.SPR')
POP KEY


RETURN
*-- End OF Code.
*:*************************************************************
*: Name      : lfvgetFile
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 02/17/2004
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
IF EMPTY(lcFileName)
  SHOW GET PbProceed DISABLE
ELSE
  SHOW GET PbProceed ENABLE
ENDIF
SHOW GET lcFileName

SET DEFA TO &lcOld_Path

*-- End Of lfvgetFile
*:*************************************************************
*: Name      : lfvProceed
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 02/17/2004
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

IF EMPTY(lcFileName)
  *--- Path name can not be empty.
  =gfModalGen('TRM04074B00000','DIALOG','Path name')
  _CUROBJ = OBJNUM(lcPathName)
  RETURN
ENDIF

*-- Check if File Is .Csv file.
IF !(".XLS" $ lcFileName)
  *-- "This file can not be selected you must select a file of .XLS type."
  lcMessage = "This file can not be selected you must select a file of .XLS type."
  = gfModalGen("INM00000B00000","F","ALERT"," ",lcMessage)
  RETURN
ENDIF

*-- Check if the file is exist or not.
IF !FILE(lcFileName)
  * -- "File does not exist. Cannot proceed."
  =gfModalGen('TRM00273B00000','DIALOG')
  _CUROBJ = OBJNUM(lcFileName)
  RETURN
ENDIF

*-- Function to create the file that will hold the qty.
= lfCrtTemp()

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
= lfStartCon ()


*-- close opened files.
= lfClosFls ()

RETURN
*-- End OF lfvProceed.
*:*************************************************************
*: Name      : lfCrtTemp
*: Developer : ABDOU ELGENDI - (ABD)
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
PRIVATE lnPrvAlias 

lnPrvAlias = SELECT(0)
CREATE CURSOR (lcMainTemp);
   (STYLE  C(19), CSTYMAJOR C(19), CSTYGROUP C(06), Scale C(03), DESC1 C(60),;
    STK1   N(07), STK2      N(07), STK3      N(07), STK4  N(07),;
    STK5   N(07), STK6      N(07), STK7      N(07), STK8  N(07),;
    TOTSTK N(10))
  
*-- Copying the excel file to the cursor.
APPEND FROM (lcFileName) TYPE XLS

*-- Delete the header record for the file header.
LOCATE
DELETE 

SELECT(lnPrvAlias)
*-- End OF lfCrtTemp.
*:*************************************************************
*: Name      : lfOpenFls
*: Developer : ABDOU ELGENDI -  (ABD)
*: Date      : 08/20/2003
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
llOpenFils = .T.
laOpenFile[1,1] = 'STYLE'
laOpenFile[1,2] = 'STYLE'

laOpenFile[2,1] = 'STYINVJL'
laOpenFile[2,2] = 'STYINVJL'

laOpenFile[3,1] = 'STYDYE'
laOpenFile[3,2] = 'STYDYE'

laOpenFile[4,1] = 'INVTADJ'
laOpenFile[4,2] = 'INVTADJ'

laOpenFile[5,1] = 'SEQUENCE'
laOpenFile[5,2] = 'Cseq_type'

laOpenFile[6,1] = 'WAREHOUS'
laOpenFile[6,2] = 'WAREHOUS'

laOpenFile[7,1] = 'CODES'
laOpenFile[7,2] = 'Ccode_no'



FOR I = 1 To ALEN(laOpenFile,1)
  IF !USED(laOpenFile[I,1])
    laOpenFile[I,3] =gfOpenFile(gcDataDir+laOpenFile[I,1],laOpenFile[I,2], "SH")
  ENDIF
ENDFOR

IF llGlLink
  IF !USED("GLDIST")
    llOpenGLD = .T.
    =gfOpenFile(gcDataDir+"GLDIST",gcDataDir+"GLDISTAC","SH")
  ELSE
    llOpenGLD = .F.
    SELECT GLDIST
    SET ORDER TO TAG GLDISTAC
  ENDIF
  
  IF !FILE(gcWorkDir+lcGLTemp)
    *-- Create temp. file hold the gl entries.
    =AFIELDS(laFileStru)
    CREATE TABLE (gcWorkDir+lcGLTemp) FROM ARRAY laFileStru
  ENDIF
ENDIF


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
DIMENSION laFStru[lnFStru+2,4]

laFStru[lnFStru+1,1] = 'cAdjReason'
laFStru[lnFStru+1,2] = 'C'
laFStru[lnFStru+1,3] = 6
laFStru[lnFStru+1,4] = 0

laFStru[lnFStru+2,1] = 'cRefer'
laFStru[lnFStru+2,2] = 'C'
laFStru[lnFStru+2,3] = 6
laFStru[lnFStru+2,4] = 0
CREATE DBF (gcWorkDir+lcTmpAdj) FROM ARRAY laFStru
lcExpr=IIF(llDyelot,'Style+Dyelot+STR(RECNO(),6)','Style+STR(RECNO(),6)')
INDEX ON &lcExpr TAG &lcTmpAdj


*IF !llMultiWH
  *--Read the default warehouse.
  GO TOP IN WAREHOUS
  STORE WAREHOUS.cWareCode TO lcFromWare , lcDefWareh
*ENDIF

IF llGlLink
  =SEEK ('D'+'CADJREASON','CODES')
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

IF llOpenFils
  FOR I = 1 To ALEN(laOpenFile,1)
    IF USED(laOpenFile[I,1]) .AND.  laOpenFile[I,3]
      = gfCloseFile(laOpenFile[I,1])
    ENDIF
  ENDFOR
ENDIF


IF llOpenGLD .AND. USED("GLDIST")
  USE IN GLDIST 
ENDIF


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
PRIVATE lnPrvAlias  , ldDate

lnPrvAlias = SELECT(0)
ldDate     = gdSysdate

SELECT (lcMainTemp)
LOCATE
SCAN
  llUpdatPh = .T.
  WAIT WINDOW "Update Style # : " + Style  + "  ... Please Wait." NOWAIT
  SCATTER MEMVAR MEMO
  
  *-- Get the Old Cost from the style file.
  IF SEEK(M.Style,'STYLE')
    lnOldCost = IIF(lcCostMth<>'S',STYLE.Ave_Cost,STYLE.TotCost)
    *-- Save the Old & new cost as thge same cost.
    lnNewCost = lnOldCost
  
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
            GlPeriod   WITH lcGlPeriod

           =gfAdd_Info(lctmpAdj)  
  
    SELECT (lcMainTemp)
   ENDIF 
ENDSCAN

*-- Call the global save to save the Physical invantory for all styles.

DO lfSave IN (gcapphome+gcwinappl+'\ICInvSav.PRG') WITH .F.

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
*:*************************************************************