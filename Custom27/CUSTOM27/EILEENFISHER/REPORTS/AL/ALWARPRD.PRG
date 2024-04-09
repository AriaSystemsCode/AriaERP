*:****************************************************************************************
*: Program file  : Alwarprd.Prg
*: Program desc. : Warehouse Productivity Report (For Eileen Fisher)
*: System        : Aria Apparel System (A27).
*: Module        : Sales Order Allocation (AL).
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#102307,1
*: Date          : 05/14/2001
*:****************************************************************************************
*: Calls 
*:               : FUNCTIONS  : lfCollect , lfUserId , lfCrTmpFls , lfwRepWhen
*:               ----------------------------------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************************************
*: Passed Parameters  : None
*:****************************************************************************************
*:C#102307,1.
*:****************************************************************************************
*:Modifications :
*:****************************************************************************************
*:
*-- Declaration variables.
*-- ldFromDate :- Hold the first from date 
*-- ldToDate   :- Hold the scond date
lcTime = TIME()
*-- lcTmpPick : Temp file hold the transaction.

*-- End Declaration variables.

*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
  STORE {} To ldFromDate , ldToDate
  *-- Get the Postion of customer recored in the Option Grid.
  lnDateGng = lfItmPos('PICKSCAN.DPIK_DATE')
  *-- First date
  ldFromDate  = CTOD(LEFT(laOGFxFlt[lnDateGng,6],ATC('|',laOGFxFlt[lnDateGng,6])-1))
  ldToDate    = CTOD(SUBSTR(laOGFxFlt[lnDateGng,6],ATC('|',laOGFxFlt[lnDateGng,6])+1,;
                LEN(laOGFxFlt[lnDateGng,6])))
                
  *-- call function to create temp file.
  = lfCrTmpFls ()

  *-- New function to collect the data upon the new selection critria
  *-- In the optiongrid.
  = lfCollect ()
ENDIF

SELECT (lcTmpPick)
GOTO TOP
IF EOF()
 *---Text : 'No Record Selected for the report..!'
 =gfModalGen('TRM00052B00000','DIALOG')
 RETURN
ENDIF

*-- We should save the last recored to prin the total if 
*-- the report arrive to last recored.
SELECT (lcTmpPick)
GOTO BOTTOM 
lnLastRec = RECNO()
*-- Go to top in the file before printing.
LOCATE


DO gfDispRe WITH EVAL('lcRpForm')

RETURN
*-- End Of Form code.
*-- Function Section.
*:****************************************************************
*: Name        : lfCollect
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 05/16/2001
*: Purpose     : Function to collect data on the selected criteria
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfNewColet()
*:****************************************************************
*
FUNCTION lfCollect
PRIVATE ldOldDate 
STORE {} TO ldOldDate

*-- If user select any user from the browse.
llWorkOrdr = !EMPTY(lcTempSlUs) .AND. USED(lcTempSlUs) .AND. RECCOUNT(lcTempSlUs) > 0

*--- Select all users if didn't select any user or the 
*-- file hold the selected user only.
lcWorkFile = IIF(llWorkOrdr , lcTempSlUs ,lcTempUsrs)

*-- Case for date's to collect data on it
DO CASE
  CASE EMPTY(ldFromDate) .AND. EMPTY(ldToDate)
    lDScanDate = .T.
  CASE !EMPTY(ldFromDate) .AND. EMPTY(ldToDate)
    lDScanDate = dAdd_date >= ldFromDate
  CASE EMPTY(ldFromDate) .AND. !EMPTY(ldToDate)
    lDScanDate = dAdd_date <= ldToDate
  CASE !EMPTY(ldFromDate) .AND. !EMPTY(ldToDate)
    lDScanDate = BETWEEN(dAdd_date,ldFromDate,ldToDate)
ENDCASE


SELECT (lcWorkFile)
SCAN
  =SEEK(Eval(lcWorkFile+'.CUSER_ID'),'PickScan')
  SELECT PickScan
  SCAN REST WHILE cUser_id + DTOC(dPik_date) = &lcWorkFile..CUSER_ID;
       FOR &lcRpExp
    *-- Put data in the transaction file.
    IF SEEK(cUser_id+DTOS(DPik_Date),lcTmpPick)
      SELECT (lcTmpPick)
      REPLACE nPick WITH nPick     + PickScan.nscn_qty,;
              nTotal   WITH nTotal + PickScan.nscn_qty
    ELSE
      INSERT INTO (lcTmpPick) (cUser_id,nPick,nTotal,DPik_Date);
                   Value(PickScan.cUser_id,PickScan.nScn_qty,;
                         PickScan.nScn_qty,PickScan.DPik_Date)
    ENDIF
    SELECT PickScan
  ENDSCAN


  *--  ldFromDate ldToDate
  *-- Get the packed qty from file Pack_lin File in case 
  *-- the date is change to new date.
  *-- Save the new date from pickscan file.
  SELECT Pack_Lin
  SCAN FOR  cAdd_user = Eval(lcWorkFile+'.CUSER_ID') .AND.;
            lDScanDate
    IF SEEK(Eval(lcWorkFile+'.CUSER_ID')+DTOS(Pack_Lin.dAdd_date),lcTmpPick)
      SELECT (lcTmpPick)
      REPLACE nPack    WITH nPack  + Pack_Lin.TotQty,;
              nTotal   WITH nTotal + Pack_Lin.TotQty
    ELSE
      INSERT INTO (lcTmpPick) (cUser_id,nPack,nTotal,DPik_Date);
                   Value(Eval(lcWorkFile+'.CUSER_ID'),Pack_Lin.TotQty,;
                         Pack_Lin.TotQty,Pack_Lin.dAdd_date)
    ENDIF
    SELECT Pack_Lin
  ENDSCAN

  *-- Get the recived  qty from file CTKTRCVL File.
  *-- WE will scan for the user id , specific date & type 2
  SELECT Ctktrcvl
  SCAN FOR cAdd_user = Eval(lcWorkFile+'.CUSER_ID') .AND. ;
           lDScanDate   .AND. ;
           Ctktrcvl.trancd = '2'
    IF SEEK(Eval(lcWorkFile+'.CUSER_ID')+DTOS(Ctktrcvl.dAdd_date),lcTmpPick)
      SELECT (lcTmpPick)
      REPLACE nReceived WITH nReceived + Ctktrcvl.TotQty,;
              nTotal    WITH nTotal    + Ctktrcvl.TotQty
    ELSE
      INSERT INTO (lcTmpPick) (cUser_id,nReceived,nTotal,DPik_Date);
                   Value(Eval(lcWorkFile+'.CUSER_ID'),Ctktrcvl.TotQty,;
                         Ctktrcvl.TotQty,Ctktrcvl.dAdd_date)
    ENDIF
    SELECT Ctktrcvl
  ENDSCAN
  SELECT (lcWorkFile)  
ENDSCAN

*-- End Of lfCollect.
*:****************************************************************
*: Name        : lfUserId
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 05/16/2001
*: Purpose     : Function to Show the orders in the browse.
*:****************************************************************
*: Called from : Option Gird.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfUserId()
*:****************************************************************
*
FUNCTION lfUserId

*-- This file will hold the selected users.
SELECT (lcTempSlUs)
INDEX ON CUSER_ID TAG (lcTempSlUs)

lcBrowFlds = [Cuser_ID     :H= 'User #':10]

SELECT (lcTempUsrs)
GOTO TOP
=gfrange(lcBrowFlds,lcTempSlUs,"CUSER_ID","","","","@! XXXXXXXXXXXXXXXXXXX")
*-- Ass.  .T. To llOGFltCh to collect the data again upon the new critiria.
llOGFltCh = .T.


*-- End Of lfUserId.
*:****************************************************************
*: Name        : lfCrTmpFls
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 05/16/2001
*: Purpose     : Function to Create temp files.
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfCrTmpFls ()
*:****************************************************************
*
FUNCTION lfCrTmpFls
PRIVATE lnOldAls
lnOldAls = 0

lnOldAls = SELECT(0)

*-- Create the Temp file that we will collect the data on it.
DIMENSION laFileStru[6, 4]

laFileStru[1,1] = 'cUser_Id'
laFileStru[1,2] = 'C'
laFileStru[1,3] = 10
laFileStru[1,4] = 0

laFileStru[2,1] = 'DPik_Date'
laFileStru[2,2] = 'D'
laFileStru[2,3] = 8
laFileStru[2,4] = 0

laFileStru[3,1] = 'nPick'
laFileStru[3,2] = 'N'
laFileStru[3,3] = 10
laFileStru[3,4] = 0

laFileStru[4,1] = 'nPack'
laFileStru[4,2] = 'N'
laFileStru[4,3] = 10
laFileStru[4,4] = 0

laFileStru[5,1] = 'nReceived'
laFileStru[5,2] = 'N'
laFileStru[5,3] = 10
laFileStru[5,4] = 0

laFileStru[6,1] = 'nTotal'
laFileStru[6,2] = 'N'
laFileStru[6,3] = 10
laFileStru[6,4] = 0

CREATE TABLE (gcWorkDir+lcTmpPick) FROM ARRAY laFileStru
INDEX ON cUser_ID+DTOS(DPik_Date) TAG (lcTmpPick) OF (gcWorkDir+lcTmpPick+'.CDX')

SELECT (lnOldAls)

*-- End OF lfCrTmpFls.
*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 02/11/2001
*: Purpose   : Option Grid When function
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None.
*:*************************************************************
*: Return      : None.
*:*************************************************************
*: Example     : = lfwRepWhen()
*:*************************************************************
*
FUNCTION lfwRepWhen

CREATE TABLE (gcWorkDir + lcTempSlUs) (CUSER_ID C(10))
ZAP

*-- Get the Postion of customer recored in the Option Grid.
*-- Create curser.
CREATE TABLE (gcWorkDir + lcTempUsrs) (CUSER_ID C(10))
ZAP

SELECT DISTINCT CUSER_ID FROM PICKSCAN INTO TABLE (gcWorkDir + lcTempUsrs)
INDEX ON CUSER_ID TAG (lcTempUsrs)

*-- End OF lfwRepWhen.
*:*************************************************************
*: Name      : lfClearRep
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 02/11/2001
*: Purpose   : Close any opened files if user press OG <Close> Button
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Return      : ....
*:**************************************************************************
*: Example     : = lfClearRep()
*:**************************************************************************
*
FUNCTION lfClearRep
*-- Rise llOGFltCh flag to recollect data next time preview or run because this fn called 
*-- if user press <Reset> or Clear Read.
llOGFltCh = .F.  

*-- Close Temporary Cursors [Begin]
IF USED (lcTempUsrs)
  SELECT (lcTempUsrs)
  USE IN (lcTempUsrs)
  CLOSE INDEX
  ERASE (gcWorkDir+lcTempUsrs+".CDX")
  ERASE (gcWorkDir+lcTempUsrs+".DBF")
  ERASE (gcWorkDir+lcTempUsrs+".FPT")
ENDIF

IF USED(lcTmpPick)
  SELECT (lcTmpPick)
  USE IN (lcTmpPick)
  CLOSE INDEX
  ERASE (gcWorkDir+lcTmpPick+".CDX")
  ERASE (gcWorkDir+lcTmpPick+".DBF")
  ERASE (gcWorkDir+lcTmpPick+".FPT")
ENDIF

IF USED(lcTempSlUs)
  SELECT (lcTempSlUs)
  USE IN (lcTempSlUs)
  CLOSE INDEX
ENDIF

*-- Close Temporary Cursors [End]
*-- End Of lfClearRep.
*:****************************************************************
*! Name      : lfItmPos
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 02/11/2001
*: Purpose   : Evaluate fixed filter position within array.
*:****************************************************************
*: Calls     : None.
*:****************************************************************
*: Called from : Report code
*:****************************************************************
*: Passed Parameters  : ...
*:****************************************************************
*: Returns            : Position
*:****************************************************************
*: Example   : = lfItmPos()
*:****************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF

RETURN lnItmPos

*-- End of lfItmPos.
*:*************************************************************


