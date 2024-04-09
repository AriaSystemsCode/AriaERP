*!**************************************************************************
*! Program file        : ARDCTRN.PRG
*! Program description : Menu Option
*! For screen          : ARDCTRN.scx
*! For System          : aria4xp
*! For Module          : Account Receivable - (AR)
*! Developer Name      : AYMAN MAHMOUD AHMED (AYM)
*! Ticket no           : T20061227.0012
*! Track no            : N000595
*!**************************************************************************
*! Calls               : ARDCTRN.SPR
*!**************************************************************************
*! Passed Parameters   : None.
*!**************************************************************************
*! Example             : DO ARDCTRN.SPR 
*!**************************************************************************
*! Modification        : 
*! C200967,1 MMT 03/18/2008 Add Triggers to customize screen for GMA [T20080107.0013]
*!**************************************************************************
*! Note that the transaction types used to update the , 
*!      "DEBIT" and "CREDIT" files are..
*!      Type  Meaning
*!      0     Returns (Credit Memo).
*!      1     Invoice, Direct Invoice.
*!   *  2     Debit Adjustment.
*!   *  3     Charge Back (Debit On Account).
*!      4     Payment.
*!   *  5     Credit Adjustment.
*!   *  6     Credit On Account.
*!      7     Allowance (Credit Adjustment in this program).
*!      8     Charge Back (Only in the ARHIST).
*!      9     Credit On Account (Only in the ARHIST).
*!**************************************************************************

DO FORM (oAriaApplication.ScreenHome+"\ARDCTRN") 


*!*************************************************************
*! Name               : lFShow
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : lfRefresh() , lfBrowse() AND lfwBrow()
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lFShow
*!************************************************************
*!
PROCEDURE lFShow
PARAMETERS loFormSet,lcModeToChange,llFrmRadio

DO CASE lcModeToChange 
  *-- View mode..
  CASE lcModeToChange = 'V'
    loFormSet.ariaform1.ariaoptiongroup2.Enabled= .T.
  loFormSet.ariaform1.keyAccount.Enabled = .t.

  *-- Select mode ..
  CASE lcModeToChange = 'S'
       *loFormSet.ariaform1.keyAccount.Enabled = .t.

  *-- Edit mode .. 
  CASE lcModeToChange = 'E'
  loFormSet.ariaform1.keyAccount.Enabled = .f.
    DO CASE
	  CASE !loFormSet.llHasDebit AND !loFormSet.llHasCredit
	   loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton1.Value =1
	   loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton2.Value =0

	   loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton1.enabled =.f.
	   loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton2.enabled =.f.             

	  CASE loFormSet.llHasDebit  AND !loFormSet.llHasCredit
	   loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton1.Value =1
	   loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton2.Value =0
	   loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton1.enabled =.t.
	   loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton2.enabled =.f.

	  CASE !loFormSet.llHasDebit AND loFormSet.llHasCredit
	    loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton2.Value =1
        loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton1.Value =0
	    loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton1.enabled =.f.
	    loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton2.enabled =.t.

	  CASE loFormSet.llHasDebit  AND loFormSet.llHasCredit
	     IF loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton1.Value =1
	        loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton1.Value =1
	        loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton2.Value =0
	      ELSE
	        loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton1.Value =0
	        loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton2.Value =1
	      ENDIF   
	        loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton2.enabled =.t.
  	      loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton1.enabled =.t.
  	     
	ENDCASE  
    
    
    
    
    

  OTHERWISE 
    
ENDCASE

IF loFormSet.ariaform1.ariaoptiongroup2.Ariaoptionbutton1.Value =1
   loFormSet.ASSigncontrols ('D')
 ELSE
   loFormSet.ASSigncontrols ('C')
ENDIF 

*!*************************************************************
*! Name               : lpSav
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpSav
*!*************************************************************
*!
PROCEDURE lpSav
PARAMETERS loformset
PRIVATE lnSelAls,lcDepit,lcCredit

lnSelAls = SELECT(0)

IF loformset.llHasDebit
	lcDepit=loformset.lcTmpFileD
	SELECT (lcDepit)
	SCAN FOR Status = "M" AND  gfseek(ACCOUNT+TRAN+CINSTALNO+DTOS(TRANDATE),"debit")
	     REPLACE    Debit.Trancode      WITH &lcDepit..Trancode ,;
	                Debit.Reference  WITH &lcDepit..Reference,;
	                Debit.Desc       WITH &lcDepit..Desc     ,;
	                &lcDepit..Status WITH SPACE(1)


		*! C200967,1 MMT 03/18/2008 Add Triggers to customize screen for GMA [Start]
		IF ASCAN(loformset.laEvntTrig,PADR('SAVNOTES',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
		  =loformset.mDoTrigger(PADR('SAVNOTES',10)) 
		ENDIF    
		*! C200967,1 MMT 03/18/2008 Add Triggers to customize screen for GMA [End]
	ENDSCAN 
ENDIF 
  
IF loformset.llHasCredit
SELECT (loformset.lcTmpFileC)
lcCredit=loformset.lcTmpFileC
	SELECT (lcCredit)
	SCAN FOR Status = "M" AND  gfseek(ACCOUNT+TRAN+DTOS(TRANDATE),"credit")
	     REPLACE    credit.Ccreditcod  WITH &lcCredit..Ccreditcod ,;
	                credit.Reference  WITH &lcCredit..Reference,;
	                credit.Desc       WITH &lcCredit..Desc     ,;
	                &lcCredit..Status WITH SPACE(1)
	                
	  
	    *! C200967,1 MMT 03/18/2008 Add Triggers to customize screen for GMA [Start]
		IF ASCAN(loformset.laEvntTrig,PADR('SAVNOTES',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
		  =loformset.mDoTrigger(PADR('SAVNOTES',10)) 
		ENDIF    
		*! C200967,1 MMT 03/18/2008 Add Triggers to customize screen for GMA [End]
              
	ENDSCAN 
ENDIF

SELECT (lnSelAls)


*!*************************************************************
*! Name               : lpCrtTmp
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : DO lpCrtTmp
*!************************************************************
*!
PROCEDURE lpCrtTmp
PARAMETERS LOFORMSET

PRIVATE lnCurAlias 
lnCurAlias = SELECT(0)

DIMENSION laFileStru[10,18]
STORE '' TO laFileStru
laFileStru[1,1]   = 'Account'              &&-- COMMON
laFileStru[1,2]   = 'C'
laFileStru[1,3]   = 5
laFileStru[1,4]   = 0
      
laFileStru[2,1]   = 'Trancode'             &&-- DEBIT
laFileStru[2,2]   = 'C'
laFileStru[2,3]   = 6
laFileStru[2,4]   = 0

laFileStru[3,1]   = 'Desc'                &&-- COMMON
laFileStru[3,2]   = 'C'
laFileStru[3,3]   = 20
laFileStru[3,4]   = 0

laFileStru[4,1]   = 'Reference'            &&COMM
laFileStru[4,2]   = 'C'
laFileStru[4,3]   = 30
laFileStru[4,4]   = 0

laFileStru[5,1]   = 'Ccreditcod'       && CREDIT
laFileStru[5,2]   = 'C'
laFileStru[5,3]   = 6
laFileStru[5,4]   = 0

laFileStru[6,1]   = 'Tran'        &&COMMON
laFileStru[6,2]   = 'C'
laFileStru[6,3]   = 6
laFileStru[6,4]   = 0

laFileStru[7,1]   = 'Trandate'      &&COM
laFileStru[7,2]   = 'D'
laFileStru[7,3]   = 8
laFileStru[7,4]   = 0

laFileStru[8,1]   = 'Status'        && NEW
laFileStru[8,2]   = 'C'
laFileStru[8,3]   = 1
laFileStru[8,4]   = 0

laFileStru[9,1]   = 'Trantype'       && COMMON
laFileStru[9,2]   = 'C'
laFileStru[9,3]   = 1
laFileStru[9,4]   = 0

laFileStru[10,1]   = 'Cinstalno'   &&-- Debit
laFileStru[10,2]   = 'C'
laFileStru[10,3]   = 3
laFileStru[10,4]   = 0



=gfCrtTmp(LOFORMSET.lcTmpFiled ,@laFileStru,"account+tran+DTOS(trandate)",LOFORMSET.lcTmpFiled,.f.)
=gfCrtTmp(LOFORMSET.lcTmpFilec ,@laFileStru,"account+tran+DTOS(trandate)",LOFORMSET.lcTmpFilec,.f.)


*!*************************************************************
*! Name               : lfGetInfo
*! Developer          : Walid Abou El-Magd
*! Date               : 05/13/1999
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfGetInfo()
*!*************************************************************
FUNCTION   lfGetInfo
PARAMETER loformset

PRIVATE lnCurAlias 
lnCurAlias = SELECT(0)

SELECT (loformset.lcTmpFiled)
ZAP
SELECT DEBIT
gfSETORDER('Drtran')
IF gfSEEK('2','DEBIT') OR gfSEEK('3','DEBIT')  
  SCAN REST WHILE trantype+tran+cinstalno = '2' OR trantype+tran+cinstalno = '3' 
    IF DEBIT.ACCOUNT=loFormSet.lcAccount 
        loformset.llHasDebit=.t.
        SCATTER FIELDS ACCOUNT,Cinstalno,Trancode,Trantype,DESC,REFERENCE,TRAN,TRANDATE MEMVAR        
        INSERT INTO (loformset.lcTmpFiled) FROM MEMVAR        
        
        *! C200967,1 MMT 03/18/2008 Add Triggers to customize screen for GMA [Start]
		IF ASCAN(loformset.laEvntTrig,PADR('GETNOTES',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
		  =loformset.mDoTrigger(PADR('GETNOTES',10)) 
		ENDIF    
		*! C200967,1 MMT 03/18/2008 Add Triggers to customize screen for GMA [End]
    ENDIF
  ENDSCAN
  gfSETORDER('Debit')

ENDIF  


SELECT (loformset.lcTmpFilec)
ZAP
SELECT CREDIT
gfSETORDER('Crtran')
IF gfSEEK('5','CREDIT') OR gfSEEK('6','CREDIT')
  SCAN REST WHILE trantype+tran = '5' OR trantype+tran = '6'     
    IF CREDIT.ACCOUNT=loFormSet.lcAccount

        loformset.llHasCredit=.t.
        SCATTER FIELDS ACCOUNT,Ccreditcod,Trantype,DESC,REFERENCE,TRAN,TRANDATE MEMVAR
        INSERT INTO (loformset.lcTmpFilec) FROM MEMVAR
        *! C200967,1 MMT 03/18/2008 Add Triggers to customize screen for GMA [Start]
		IF ASCAN(loformset.laEvntTrig,PADR('GETNOTES',10),1,ALEN(loformset.laEvntTrig,1),1) > 0
		  =loformset.mDoTrigger(PADR('GETNOTES',10)) 
		ENDIF    
		*! C200967,1 MMT 03/18/2008 Add Triggers to customize screen for GMA [End]
    ENDIF
  ENDSCAN
   gfSETORDER('Credit')

ENDIF  

SELECT(lnCurAlias)