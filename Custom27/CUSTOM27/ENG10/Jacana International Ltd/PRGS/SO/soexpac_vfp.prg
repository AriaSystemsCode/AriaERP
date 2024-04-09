**********************************************************************************************
*     PROGRAM : SOEXPAC.PRG
* APPLICATION : ARIA27
*      MODULE : SO2
*   DEVELOPER : TMI - TAREK MOHAMED IBRAHIM
*     PORPOUS : CREATE A NEW MDB ACCESS FILE AND EXPORT DATA TO THIS FILE
*      ENTRY# : C#200414 
*        DATA : 11/10/2002
*        Note : This program is included in a project to create an exe called soexpac.exe which
*               will be called from soexpac.prg in aria27
**********************************************************************************************
*Modifications
*B606763,1 TMI 12/16/2002 Fix some problems sent by Tony
**********************************************************************************************

*-- Create a new mdb file for this customer 
*-- Note that DAO36 must be installed at the user PC
PARAMETERS lcMemFile

IF VARTYPE(lcMemFile) # 'C'
  MESSAGEBOX('Error in passed parameter to the VFP program!',0,'ARIA Advantage')
  RETURN
ENDIF
  
*-- Restore variables comming from fox2.6
RESTORE FROM (lcMemFile+'.MEM') ADDITIVE

*-- Open needed files 
=lfOpnFls()

*B606763,1 TMI [START] Create mdb file with Access 2002 format 
*-* *--Create the work space object
*-* oWorkSpace = CreateObject("DAO.DBEngine.36")
*-* *--Create the data base object
*-* oDatabase = oWorkSpace.CreateDatabase(lcSysDir+ALLT(lcFileName)+'.MDB' , ";LANGID=0x0409;CP=1252;COUNTRY=0")
*- Create an instace of Access 2002

*// [START] TMI 
*- As per tony this code is changed , since it does not work at his maching
*- The way we work with will be to have a tamplete that we copy from it a file with the new needed name
*- Open this file and finally complete rest of code as befor
*//appAccess = CreateObject("Access.Application.10")
*//*- Create the file on the disk
*//=appAccess.NewCurrentDatabase(lcSysDir+ALLT(lcFileName)+'.MDB')
*//*B606763,1 TMI [END  ] Create mdb file with Access 2002 format
*--Check existence of tamplete file
IF !FILE(gcDEF_PATH+"Tamplete.mdb")
  MESSAGEBOX('The tamplete mdb Access file does not exit.',0,'Aria Advantage Series')
  RETURN
ENDIF  
COPY FILE (gcDEF_PATH+"Tamplete.mdb") TO (lcSysDir+ALLT(lcFileName)+'.MDB')
IF !FILE(lcSysDir+ALLT(lcFileName)+'.MDB')
  MESSAGEBOX('Can not create the New Access(mdb) file.',0,'Aria Advantage Series')
  RETURN
ENDIF
appAccess = CreateObject("Access.Application.10")
appAccess.OpenCurrentDatabase(lcSysDir+ALLT(lcFileName)+'.MDB')
*// [END  ] TMI

*- set the current access data base to the object oDatabase
oDatabase = appAccess.CurrentDb

*--Create the tableDef object
oTbldef = oDatabase.CreateTableDef("ordertab")

*-- Add fields to the new defined TabelDef Object
=lfAddfields()

*--Add the table to the collection 
oDatabase.TableDefs.Append( oTbldef )

*--Release the TableDef object
RELEASE oTbldef

*--Open the new created table
oTbl = oDatabase.OpenRecordset("ordertab")

*-- Fill it with the needed data from fox files
=lfCollect()

*-- Cleanup
=lfCleanup()

*:**************************************************************************
*:* Name        : lfOpnFls
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/13/2002
*:* Purpose     : Open needed files 
*:***************************************************************************
FUNCTION lfOpnFls

USE (gcDataDir+'ORDHDR') IN 0 ORDER TAG ORDHDR SHARED
USE (gcDataDir+'ORDLINE') IN 0 ORDER TAG ORDLINE SHARED
USE (gcDataDir+'STYLE') IN 0 ORDER TAG STYLE SHARED
USE (gcDataDir+'SEQUENCE') IN 0 ORDER TAG CSEQ_TYPE SHARED
USE (gcDataDir+'SETUPS') IN 0 ORDER TAG MODVAR SHARED

SELECT ORDLINE
SET RELATION TO STYLE INTO STYLE
*-- end of lfOpnFls.

*:**************************************************************************
*:* Name        : fAddfields
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/13/2002
*:* Purpose     : Add fields to the new defined TabelDef Object
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = fAddfields()
*:***************************************************************************
FUNCTION lfAddfields
*--Define needed constants
#DEFINE dbText     10
#DEFINE dbInteger   3
#DEFINE dbDate      8
#DEFINE dbCurrency  5
#DEFINE dbDouble    7


*--Add the needed fields
WITH oTbldef.Fields
  .Append (oTbldef.Createfield('ordernr',dbText,6) )        
  *B606763,1 TMI [START] remove the firmanum field
  *.Append (oTbldef.Createfield('firmanum',dbInteger) )       
  *B606763,1 TMI [END  ]
  .Append (oTbldef.Createfield('firmaum',dbInteger) )       
  .Append (oTbldef.Createfield('saison',dbText,4) )
  .Append (oTbldef.Createfield('hvnr',dbText,2) )
  .Append (oTbldef.Createfield('style',dbText,4) )
  .Append (oTbldef.Createfield('qual',dbText,4) )
  .Append (oTbldef.Createfield('color',dbText,6) )
  .Append (oTbldef.Createfield('kundennr',dbText,6) )
  .Append (oTbldef.Createfield('multi',dbInteger) )

  .Append (oTbldef.Createfield('menge1',dbInteger) )
  .Append (oTbldef.Createfield('menge2',dbInteger) )
  .Append (oTbldef.Createfield('menge3',dbInteger) )
  .Append (oTbldef.Createfield('menge4',dbInteger) )
  .Append (oTbldef.Createfield('menge5',dbInteger) )
  .Append (oTbldef.Createfield('menge6',dbInteger) )
  .Append (oTbldef.Createfield('menge7',dbInteger) )
  .Append (oTbldef.Createfield('menge8',dbInteger) )

  .Append (oTbldef.Createfield('menge9',dbInteger) )
  .Append (oTbldef.Createfield('menge10',dbInteger) )
  .Append (oTbldef.Createfield('menge11',dbInteger) )
  .Append (oTbldef.Createfield('menge12',dbInteger) )
  .Append (oTbldef.Createfield('menge13',dbInteger) )
  .Append (oTbldef.Createfield('menge14',dbInteger) )
  .Append (oTbldef.Createfield('menge15',dbInteger) )
  .Append (oTbldef.Createfield('menge16',dbInteger) )

  .Append (oTbldef.Createfield('loginname',dbText,10) )
  .Append (oTbldef.Createfield('newdate',dbDate) )
  .Append (oTbldef.Createfield('changeddate',dbDate) )
  *B606763,1 TMI [START] Add the WAHRUNGBEZ field between CHANGEDATE and WAHRUNGPREIS and set it to value "EUR"
  .Append (oTbldef.Createfield('wahrungbez',dbText,3) )
  *B606763,1 TMI [END  ]
  .Append (oTbldef.Createfield('Wunrungpreiss',dbText) )  
  *B606763,1 TMI [START] Chenge type to double , so the pound sign is removed
  *.Append (oTbldef.Createfield('arrtotalpreiss',dbCurrency) )
  .Append (oTbldef.Createfield('arrtotalpreiss',dbDouble) )
  *B606763,1 TMI [START] 
  .Append (oTbldef.Createfield('kdmoral',dbText,5) )
  .Append (oTbldef.Createfield('lieferantnr',dbText,6) )
  .Append (oTbldef.Createfield('zusatz1',dbText,30) )
  .Append (oTbldef.Createfield('zusatz2',dbText,30) ) 
  
ENDWITH

*-- end of lfAddfields.

*:**************************************************************************
*:* Name        : lfCollect
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/13/2002
*:* Purpose     : Add data to the new created Access MDB file
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCollect()
*:***************************************************************************
FUNCTION lfCollect

*-- Position the pointer in the sequnce file
=SEEK(PADR('HVNR',10),'SEQUENCE')

lnFirmVar = 0
IF SEEK(Chr(255)+Chr(255)+'M_FIRMAUM' , 'SETUPS' )
  lnFirmVar = VAL(SETUPS.MDATA_DEF)
ENDIF
*B606763,1 TMI [START]
lieferantnr = 0
IF SEEK(Chr(255)+Chr(255)+'M_LIEFERAN' , 'SETUPS' )
  lieferantnr = ALLT(SETUPS.MDATA_DEF)
ENDIF
*B606763,1 TMI [END  ]

DO FORM MSGBOX NAME oMsgBox
oMsgBox.CAPTION = "Collecting data"

*--Collect data
SELECT ORDHDR
GO TOP
=SEEK('O')
SCAN REST WHILE CORDTYPE+ORDER = 'O' FOR EVAL(lcFltOrd) .AND. ;
                IIF(VARTYPE(laSlctdOrd[1])='L' , .T. , ASCAN(laSlctdOrd,ORDHDR.ORDER)>0 )

    oMsgBox.CAPTION = "Collecting data for Order# " +OrdHdr.Order 
    
    SELECT SEQUENCE
    REPLACE NSEQ_NO WITH NSEQ_NO+1
    lcHvnrSeq = PADL(SEQUENCE.NSEQ_NO,4,'0')

    SELECT ORDHDR
    REPLACE ORDHDR.SENDORD WITH "Y",;
            ORDHDR.HVNR    WITH lcHvnrSeq

    IF SEEK(OrdHdr.cordtype+OrdHdr.order,'OrdLine')
      SELECT ORDLINE
      SCAN REST WHILE cordtype+order+store+style+STR(lineno,6) = OrdHdr.cordtype+OrdHdr.order
        *WAIT WINDOW "Collecting data for Order# "+OrdHdr.Order+" style :" +OrdLine.Style NOWAIT
        oMsgBox.lblMsg.CAPTION = " style :" +OrdLine.Style
        
        WITH oTbl        

          .AddNew 
        
          .Fields('ordernr').value = lcHvnrSeq
          .Fields('firmaum').value = lnFirmVar
          .Fields('saison').value =  SUBSTR(ORDLINE.STYLE,9,4)
          .Fields('qual').value = SUBSTR(ORDLINE.STYLE,5,4)
          .Fields('style').value = SUBSTR(ORDLINE.STYLE,1,4)
          .Fields('color').value = SUBSTR(ORDLINE.STYLE,14,6)
          .Fields('kundennr').value = ORDLINE.ACCOUNT
          .Fields('Menge1').value = ORDLINE.QTY1
          .Fields('Menge2').value = ORDLINE.QTY2
          .Fields('Menge3').value = ORDLINE.QTY3
          .Fields('Menge4').value = ORDLINE.QTY4
          .Fields('Menge5').value = ORDLINE.QTY5
          .Fields('Menge6').value = ORDLINE.QTY6
          .Fields('Menge7').value = ORDLINE.QTY7
          .Fields('Menge8').value = ORDLINE.QTY8
          *B606763,1 TMI [START] make all numeric fields default to 0 
          .Fields('Menge9').value = 0
          .Fields('Menge10').value = 0
          .Fields('Menge11').value = 0
          .Fields('Menge12').value = 0
          .Fields('Menge13').value = 0
          .Fields('Menge14').value = 0
          .Fields('Menge15').value = 0
          .Fields('Menge16').value = 0
          .Fields('multi').value = 1              && 
          .Fields('loginname').value = "AMAZING"  && Deafult the LOGINANME field to AMAZING
          .Fields('lieferantnr').value = lieferantnr
          *B606763,1 TMI [END  ]
          
          .Fields('newDate').value = ORDHDR.ENTERED
          *B606763,1 TMI [START] Add the WAHRUNGBEZ field between CHANGEDATE and WAHRUNGPREIS and set it to value "EUR"
          .Fields('wahrungbez').value = 'EUR'
          *B606763,1 TMI [END  ]
          .Fields('Wunrungpreiss').value = STYLE.GROS_PRICE
          .Fields('arrtotalpreiss').value = (QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8)*STYLE.GROS_PRICE 
          .Fields('Hvnr').value = SUBSTR(lcHvnrSeq,1,2)
          .Fields('zusatz1').value = 'X'
          .Fields('zusatz2').value = 'N'
        
          .Update
          
        ENDWITH
        
      ENDSCAN
    ENDIF
  
ENDSCAN

WITH oMsgBox
  .CAPTION = "ARIA Advantage Series"
  .lblMsg.CAPTION = "The Access file " + ALLT(lcFileName)+'.MDB' + " was created successfully."
  .Height = 70
  .cmdOk.Enabled = .T.
ENDWITH
READ EVENTS
*-- end of lfCollect.


*:**************************************************************************
*:* Name        : lfCleanup
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/13/2002
*:* Purpose     : Close files and release variables
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCleanup()
*:***************************************************************************
FUNCTION lfCleanup

oTbl.close
oDatabase.close
RELEASE oTbl,oDatabase,oWorkSpace

ERASE (lcMemFile+'.MEM')
CLOSE DATA ALL 
*-- end of lfCleanup.
