*:****************************************************************
*: Program file  : HLCVSET.PRG
*: Program desc. : C.V.S. Setup Template.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 12/10/2001
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : 
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
*-- lcTemp    :- Variable to hold the temp file name.
*-- lcIndExp :- Varaible to hold the index expression.
EXTERNAL ARRAY laData,laKeyField

*-- Begin Declaration variables.
DECLARE laKeyField [1,4],laBlanks[1,1]
STORE " " TO lcTemp , lcIndExp
STORE 1   To  lnRecNo 
STORE 0   To lnOrdNo , lnFcount
STORE .F. TO llBranch


IF !gfSetup()
  RETURN
ENDIF  

        
IF !WEXIST(gcBaseWind)
  lcTemp = gfTempName()
  SELECT HLCVSETU
  lcScFields = 'CCVSPTORD   , CCVSCTDIS , CCVSIMPMT   , CCVSIMPCO  ,'+;
               'CCVSDEFTY'
               
  SCATTER FIELDS &lcScFields TO laData BLANK

  SET ORDER TO TAG Ccvsptord
  lcIndExp = SYS(14,VAL(SYS(21)))
  
  lcTagExp = ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')

  SELECT *,RECNO() AS 'nRecNo','S' AS 'cStatus';
    FROM &gcDataDir.HLCVSETU;
    INTO DBF &gcWorkDir.&lcTemp;
   ORDER BY CCVSPTORD
  SELECT (lcTemp)
  INDEX ON &lcIndExp.&lcTagExp

  COUNT FOR cCvsPtOrd <> CHR(255) .AND. !DELETED() TO lnOrdNo
ELSE
  SELECT (lcTemp)
  COUNT FOR cCvsPtOrd <> CHR(255) .AND. !DELETED() TO lnOrdNo

ENDIF


PUSH KEY
DO (gcScrDir+gcAct_Appl+"\Hlcvset.SPX")
POP KEY


IF glQuitting 

  IF USED('&lcTemp')
    USE IN &lcTemp
    ERASE &gcWorkDir.&lcTemp+'.DBF'
    ERASE &gcWorkDir.&lcTemp+'.CDX'
    ERASE &gcWorkDir.&lcTemp+'.FPT'
  ENDIF  

ENDIF  
RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwOrder
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : When Function for Order No.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfwOrder()
*:*************************************************************
*:
FUNCTION lfwOrder

SCATTER FIELDS &lcScFields MEMO TO laData

*-- To high light the list.
lsOrder = ccvsptord+' '+ccvsctdis+' '+ccvsimpmt

SHOW GETS
*-- End Of lfWOrder.
*:*************************************************************
*: Name      : lpShow
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Show function.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : Do lpShow
*:*************************************************************
*:
PROCEDURE lpShow

IF llBranch
  lsOrder = ccvsptord+' '+ccvsctdis+' '+ccvsimpmt
  SHOW GET lsOrder
  SHOW GET pbOk  ENABLE
  llBranch = .F.
ENDIF 

IF lnOrdNo > 0 
  SHOW GET pbRemove ENABLE
ELSE
  SHOW GET pbRemove DISABLE
ENDIF    

rbImpMet = IIF(EMPTY(laData[3]),1,AT(cCvsImpMt,'MAD'))
rbDefTyp = IIF(EMPTY(laData[5]),1,AT(cCvsDefTy,'7D8'))
SHOW GET rbImpMet 
SHOW GET rbDefTyp

*-- End Of lpShow.
*:*************************************************************
*: Name      : lfvData_1
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid Function For ladata[1]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_1 ()
*:*************************************************************
*:
FUNCTION lfvData_1

IF EMPTY(laData[1]) .AND. LASTKEY()=13
  _CUROBJ=OBJNUM(laData[1])
ENDIF 

IF !EMPTY(laData[1])
  ** Validation of laData[1] **

  IF !LEFT(ALLTRIM(laData[1]),1) $ '0123456789';
    .OR. !RIGHT(ALLTRIM(laData[1]),1) $ '0123456789'
    RETURN .F.
  ENDIF    

  IF LEN(ALLTRIM(laData[1])) = 1
    laData[1]= "0"+ALLTRIM(laData[1])
  ENDIF  

  IF SEEK(laData[1])
    SCATTER FIELDS &lcScFields MEMO TO laData
  ELSE
    APPEND BLANK  
    REPLACE cStatus WITH "A",;
            cCvsPtOrd WITH laData[1],;
            cCvsImpMt WITH "M",;
            cCvsDefTy WITH "7"
    SHOW GET laData[1] DISABLE
  ENDIF  
ENDIF  

lsOrder = ccvsptord+' '+ccvsctdis+' '+ccvsimpmt

SHOW GET lsOrder
SHOW GET pbRemove  ENABLE
llBranch = .T.

*-- End Of lfvData_1.
*:*************************************************************
*: Name      : lfvData_2
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid Function For ladata[2]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_2 ()
*:*************************************************************
*:
FUNCTION lfvData_2

IF UPPER(ALLTRIM(cCvsCtDis)) <> UPPER(ALLTRIM(laData[2]))
  ** If the field is updated **
  REPLACE cStatus WITH IIF(cStatus="S","M",cStatus),;
          cCvsCtDis WITH laData[2]
ENDIF  

lsOrder = ccvsptord+' '+ccvsctdis+' '+ccvsimpmt
SHOW GET lsOrder

*-- End Of lfvData_1.
*:*************************************************************
*: Name      : lfvData_3
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid Function For ladata[3]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_3 ()
*:*************************************************************
*:
FUNCTION lfvData_3

DO CASE

  CASE rbImpMet = 1
    laData[3] = "M"

  CASE rbImpMet = 2
    laData[3] = "A"

  CASE rbImpMet = 3
    laData[3] = "D"

ENDCASE 

IF UPPER(ALLTRIM(cCvsImpMt)) <> UPPER(ALLTRIM(laData[3]))
  ** If the field is updated **
  REPLACE cStatus WITH IIF(cStatus="S","M",cStatus),;
          cCvsImpMt WITH laData[3]
ENDIF  

lsOrder = ccvsptord+' '+ccvsctdis+' '+ccvsimpmt

SHOW GET rbImpMet
SHOW GET lsOrder

*-- End Of lfvData_3.
*:*************************************************************
*: Name      : lfvData_4
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid Function For ladata[4]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_4 ()
*:*************************************************************
*:
FUNCTION lfvData_4

IF UPPER(ALLTRIM(cCvsImpCo)) <> UPPER(ALLTRIM(laData[4]))
  ** If the field is updated **
  REPLACE cStatus WITH IIF(cStatus="S","M",cStatus),;
          cCvsImpCo WITH laData[4]
ENDIF  

*-- End OF lfvData_4
*:*************************************************************
*: Name      : lfvData_5
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid Function For ladata[5]
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvData_5 ()
*:*************************************************************
*:
FUNCTION lfvData_5

DO CASE 

  CASE rbDefTyp = 1
    laData[5] = '7'

  CASE rbDefTyp = 2
    laData[5] = 'D'

  CASE rbDefTyp = 3
    laData[5] = '8'

ENDCASE  

SHOW GET rbDefTyp

IF UPPER(ALLTRIM(cCvsDefTy)) <> UPPER(ALLTRIM(laData[5])) 
  ** If the field is updated **
  REPLACE cStatus WITH IIF(cStatus="S","M",cStatus),;
          cCvsDefTy WITH laData[5]
ENDIF  

*-- End of lfvData_5.
*:*************************************************************
*: Name      : lfvNew
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid Function ,
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvNew ()
*:*************************************************************
*:
FUNCTION lfvNew

SCATTER FIELDS &lcScFields MEMO TO laData BLANK
rbDefTyp = 1
rbImpMet = 1
SHOW GET laData[1] ENABLE
SHOW GET laData[2]
SHOW GET rbImpMet 
SHOW GET laData[4]
SHOW GET rbDefTyp
SHOW GET lsOrder 
SHOW GET pbOk ENABLE
_CUROBJ=OBJNUM(laData[1])

*-- End OF lfvNew.
*:*************************************************************
*: Name      : lfvRemove
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid Function ,
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvRemove ()
*:*************************************************************
*:
FUNCTION lfvRemove

*--  If the record is deleted.
REPLACE cStatus WITH "D"
DELETE
_CUROBJ=OBJNUM(lsOrder)
SHOW GET lsOrder

*-- End Of lfvRemove.
*:*************************************************************
*: Name      : lfvOk
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid Function ,
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvOk()
*:*************************************************************
*:
FUNCTION lfvOk

*-- Coping the data from the temp file to the master file **
=lFTMP2MAST('HLCVSETU',lcTemp)
glQuitting = .T.

*-- End Of lfvOk 
*:*************************************************************
*: Name      : lfvCancel
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid Function.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvCancel()
*:*************************************************************
*:
FUNCTION lfvCancel

glQuitting = .T.

*-- End Of lfvCancel.
*:*************************************************************
*: Name      : lfTmp2Mast
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/10/2001
*: Purpose   : Valid Function.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfTmp2Mast()
*:*************************************************************
*:
* Program to update records of master file from temp file 
* Temp file has to have 2 additional fields nRecNo and cStatus to hold
* the phisical records no and the action status done to this record
*
FUNCTION lfTmp2Mast

PARAMETERS lcMastFile,lcTempFile

PRIVATE    lcMastFile,lcTempFile,lcSavAlias

lcSavAlias = SELECT(0)
lcSourcDir = IIF (UPPER(LEFT(lcMastFile,2))="SY",gcSyshome,gcDatadir)

lcSaveDel = SET ('DELETE')
SET DELETE OFF

SELECT (lcTempFile)
*** Scan through all the Added,Modified or Deleted records
SCAN FOR cStatus <> 'S'
  DO CASE 

    *** New added record   
    CASE cStatus = 'A'                    
      SCATTER MEMVAR MEMO
      SELECT  (lcMastFile)  

      IF SEEK(' ')                        && Chek if there is empty 
        RECALL                            && Deleted records to recall
        GATHER MEMVAR MEMO
      ELSE  
        INSERT INTO &lcSourcDir.&lcMastFile FROM MEMVAR
      ENDIF  

    *** Record was modified
    CASE cStatus = 'M'                    
      SCATTER MEMVAR MEMO                 && Collect data from temp
      SELECT  (lcMastFile)  
      GO &lcTempFile..nRecNo
      GATHER  MEMVAR MEMO                 && Replace master data 

    *** Record was deleted
    CASE DELETED()
      SELECT  (lcMastFile)  
      GO &lcTempFile..nRecNo

      SCATTER MEMVAR MEMO BLANK           && Empty the record befor
      GATHER  MEMVAR MEMO                 && delete it

      DELETE                              && Delete recored not in temp
  ENDCASE

  SELECT  (lcTempFile)
  REPLACE cStatus WITH "S"
ENDSCAN  

SET DELETE &lcSaveDel
SELECT (lcSavAlias)

*-- End Of lfTmp2Mast.
*:*************************************************************