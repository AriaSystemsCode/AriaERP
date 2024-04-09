*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMFishd.Prg
*:  Module      : System Manager
*:  Desc.       : Fiscal Year Screen
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 08/26/2013
*:  Reference   : *B610484,1 TMI 08/22/2013 allow to be called from the smClose.prg [T20130802.0008] 
**************************************************************************
*: Modifications:
*B612033,1 MMT 02/20/2020 Fix issue of duplicating next year record in FISHD while closing year[T20200116.0011]
*B612205,1 Es 08/20/2020 When user closes Year/Period programs to close 2 Years at the same session, the Current year is not updated correctly. [T20200609.0003]
*B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001]
*:************************************************************************
*E303339,1 TMI 01/09/2013 [Start] this is just a version to allow the smcminf.prg to work, the smfsyer screen should be developed separately
*E303339,1 TMI 01/09/2013 [End  ] 
PARAMETERS pcComp_ID
*B610484,1 TMI 08/26/2013 [Start] comment this out
*lcMsg = 'this is just a version to allow the smcminf.prg to work, the SMFISHD screen should be developed separately, It is called only from SMCLOSE, '+;
        'it has the same functionality as the SMFSYER, the latter should be adjusted so that the same screen could be used for the two cases'
*MESSAGEBOX(lcmsg,0,'I M P O R T A N T')
*B610484,1 TMI 08/26/2013 [End  ] 

DIMENSION laMonth[12]
laMonth[1]  = 'January   '
laMonth[2]  = 'February  '
laMonth[3]  = 'March     '
laMonth[4]  = 'April     '
laMonth[5]  = 'May       '
laMonth[6]  = 'June      '
laMonth[7]  = 'July      '
laMonth[8]  = 'August    '
laMonth[9]  = 'September '
laMonth[10] = 'October   '
laMonth[11] = 'November  '
laMonth[12] = 'December  '

DIMENSION laYear[4]
laYear[1] = STR(VAL(oAriaApplication.currentyear)-1,4)
laYear[2] = oAriaApplication.currentyear
laYear[3] = STR(VAL(oAriaApplication.currentyear)+1,4)
laYear[4] = STR(VAL(oAriaApplication.currentyear)+2,4)

*- Set the buffering
SELECT FISHD

*CURSORSETPROP("Buffering",5)
SELECT FSPRD
*CURSORSETPROP("Buffering",5)
SELECT syccomp
*CURSORSETPROP("Buffering",5)

*- update the years sequence
SELECT fishd
=SEEK(laYear[1])
REPLACE CFISYSTAT WITH 'H'
=SEEK(laYear[2])
REPLACE CFISYSTAT WITH 'P'
=SEEK(laYear[3])
REPLACE CFISYSTAT WITH 'C'
*B612205,1 Es 08/20/2020 When user closes Year/Period programs to close 2 Years at the same session, the Current year is not updated correctly. [T20200609.0003][Start]
oAriaApplication.Ref4.currentyear = laYear[3]
oAriaApplication.Ref5.currentyear = laYear[3]
*B612205,1 Es 08/20/2020 When user closes Year/Period programs to close 2 Years at the same session, the Current year is not updated correctly. [T20200609.0003][End]
SCATTER MEMVAR MEMO
*B612033,1 MMT 02/20/2020 Fix issue of duplicating next year record in FISHD while closing year[T20200116.0011][Start]
IF !SEEK(laYear[4],'FISHD')
*B612033,1 MMT 02/20/2020 Fix issue of duplicating next year record in FISHD while closing year[T20200116.0011][End]
  APPEND BLANK
*B612033,1 MMT 02/20/2020 Fix issue of duplicating next year record in FISHD while closing year[T20200116.0011][Start]
ENDIF 
*B612033,1 MMT 02/20/2020 Fix issue of duplicating next year record in FISHD while closing year[T20200116.0011][End]

m.CFISFYEAR = laYear[4]
m.CFISYSTAT = 'N'
m.CFISLHEAD = 'Fis. year '+laYear[4]
m.CFISSHEAD = m.CFISLHEAD 
m.DFISBGDAT = GOMONTH(m.DFISBGDAT ,12)
m.DFISENDAT = GOMONTH(m.DFISENDAT ,12)
m.MFISCOMNT = ''
GATHER MEMVAR MEMO
SELECT fishd
*gfTableUpdate()

ldFrom = m.DFISBGDAT
ldTo = GOMONTH(m.DFISBGDAT,1)-1

*- update the periods
SELECT FSPRD
FOR j=1 TO 12
  *B612033,1 MMT 02/20/2020 Fix issue of duplicating next year record in FISHD while closing year[T20200116.0011][Start]
  IF !SEEK(laYear[4]+padl(j,2,'0'),'FSPRD','COMFYRPRDI')
  *B612033,1 MMT 02/20/2020 Fix issue of duplicating next year record in FISHD while closing year[T20200116.0011][End]
    APPEND BLANK
  *B612033,1 MMT 02/20/2020 Fix issue of duplicating next year record in FISHD while closing year[T20200116.0011][Start]
  ENDIF
  *B612033,1 MMT 02/20/2020 Fix issue of duplicating next year record in FISHD while closing year[T20200116.0011][End]  
  REPLACE CFISFYEAR WITH laYear[4]  ,;
          CFSPPRDID WITH padl(j,2,'0') ,;
          CFSPPDESC WITH laMonth[MONTH(ldFrom)] ,;
          DFSPPBGDT WITH ldFrom ;
          DFSPPENDT WITH ldTo ,;
          NFSPPARTN WITH CEILING(j/3)
  ldFrom = GOMONTH(ldFrom,1)
  ldTo = GOMONTH(ldFrom,1)-1
ENDFOR 

SELECT fsprd
*gfTableUpdate()

SELECT syccomp
*LOCATE FOR ccomp_id = LEFT(loFormSet.Ariaform1.laCompany.Value,2)
LOCATE FOR ccomp_id = oAriaApplication.ActiveCompanyID
*!*	Replace CCURR_YER WITH laYear[3] ,;
*!*	        CCURR_PRD WITH '01'

ldDatChange = loFormSet.laCompany[loFormSet.puCompany,7]
llPrdChange = .T.

SELECT syccomp
*gfTableUpdate()

IF TYPE('loFormSet.lcSJ_DEF')='U'
  loFormSet.AddProperty('lcSJ_DEF')
ENDIF 
loFormSet.lcSJ_DEF = 'GL'

LOCAL lcSetProcc
lcSetProc = SET("Procedure")
SET PROCEDURE TO
*- this variable llCloseYer is used to indicate that closing year is being processed
PRIVATE llCloseYer
llCloseYer = .T.
DO (oariaapplication.applicationhome+'SM\SMFSYER.FXP') WITH oAriaApplication.ActiveCompanyID
SET PROCEDURE TO &lcSetProc


*B610484,1 TMI 08/27/2013 [Start] update the arcushst,icstyhst
=lfUpdateHist()

************************************************************
*! Name      : lfUpdateHist
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/27/2013
*! Purpose   : update the arcushst,icstyhst
************************************************************
FUNCTION lfUpdateHist
LOCAL lnSlct
lnSlct = SELECT(0)
*- Open
gfOpenTable('STYLE','STYLE','SH')
gfOpenTable('icstyhst','STYHST','SH') && STYLE+CFISFYEAR
gfOpenTable('CUSTOMER','CUSTOMER','SH')
gfOpenTable('arcushst','ACTHST','SH')   && ACCOUNT+CFISFYEAR

 
*- Fill
SELECT style

*B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001][Start]
oPross = CREATEOBJECT('ariaprogressbar')
oPross.lblFirstLabel.CAPTION = 'Updating Style History....'
SELECT DISTINCT STYLE FROM STYLE WHERE !DELETED() INTO CURSOR 'STYLES'
oPross.TotalProgress = RECCOUNT('STYLES')
oPross.AUTOCENTER = .T.
oPross.SHOW()
lnRecCntSty = 0
SELECT STYLES
*B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001][End]

LOCATE 
SCAN 
  *B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001][Start]
  *WAIT WINDOW NOWAIT STYLE.STYLE
  lnRecCntSty = lnRecCntSty + 1 
  =gfSeek(STYLES.STYLE,'STYLE','STYLE')
  oPross.CurrentProgress(lnRecCntSty)
  oPross.lblSecondLabel.CAPTION = STYLES.STYLE  
  *B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001][End]
  IF !gfSEEK(STYLE.STYLE+laYear[4],'icstyhst')
    SELECT icstyhst
    gfAppend()    
    gfReplace("STYLE     WITH STYLE.STYLE ,CFISFYEAR WITH laYear[4]")     
  ENDIF 
ENDSCAN

*B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001][Start]
oPross.HIDE()
oPross.lblFirstLabel.CAPTION = 'Updating Customer History....'
SELECT Customer
=gfSeek("M")
COUNT REST WHILE TYPE+ACCOUNT ='M' FOR !DELETED() TO lnCustCnt
oPross.TotalProgress = lnCustCnt
oPross.AUTOCENTER = .T.
oPross.SHOW()
lnRecCntCust = 0
*B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001][End]

SELECT customer
*B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001][Start]
*!*	LOCATE 
*!*	SCAN 

*!*	  WAIT WINDOW NOWAIT customer.account
=gfSeek('M')  
SCAN REST WHILE TYPE+ACCOUNT ='M' FOR !DELETED() 
  lnRecCntCust = lnRecCntCust + 1 
  oPross.CurrentProgress(lnRecCntCust)
  oPross.lblSecondLabel.CAPTION = customer.account
*B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001][End]  
  IF !gfSEEK(customer.account+laYear[4],'arcushst')
    SELECT arcushst
    *APPEND BLANK
    gfAppend()
    gfReplace("account   WITH customer.account,CFISFYEAR WITH laYear[4]")
  ENDIF 
ENDSCAN
WAIT CLEAR 

*B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001][Start]
oPross.HIDE()
oPross= Null 
*B612515,1 MMT 01/24/2022 Show progress bar while closing and avoid ICSTYHST duplication[T20220105.0001][End]

*- Update
SELECT ICSTYHST
gfTableUpdate(.T.)
SELECT ARCUSHST
gfTableUpdate(.T.)

*- Close 
gfCloseTable('STYLE')
gfCloseTable('icstyhst')
gfCloseTable('CUSTOMER')
gfCloseTable('arcushst')

SELECT (lnSlct)
*- End of lfUpdateHist.
