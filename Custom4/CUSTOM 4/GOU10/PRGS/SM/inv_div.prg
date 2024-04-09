*E303413,1 TMI 09/04/2013 [Start] Mariam reviewed this as follows , ticket T20130827.0018 - Utility to fix the division code on invoices
*Please note that the mentioned custom/fix program does not follow the standardization of Aria as it is using the files natively and using Hard coded paths for the files.
*Also the program is not updated in VSS, does not have documentation, and the source code is located on customer folder on SAAS.
*
*Fix the issues above and to check it in VSS (refer to T20120727.0031 - SALES JOURNAL MISSING DIVISION FOR INV 120473 FOR MAY)
*E303413,1 TMI 09/04/2013 [End  ] 



*B610498,1 TMI 09/04/2013 [Start] if this is not Co.02 , do not run
IF oAriaApplication.ActiveCompanyID <> '02'
  RETURN 
ENDIF 
*B610498,1 TMI 09/04/2013 [End  ] 

Wait window "Program will run now --> Press ENTER key"

*B610498,2 TMI 09/10/2013 [Start] create a new session to open the tables in it
LOCAL lnCurrSession,oSession
lnCurrSession = SET("Datasession")
oSession = NEWOBJECT('session')
SET DATASESSION TO oSession.DataSessionID
*B610498,2 TMI 09/10/2013 [End  ] 

*B610498,1 TMI 09/04/2013 [Start] open the table using gfopenfile
*USE x:\aria27\dbfs\02\ordhdr.dbf IN 1 SHARED
=gfOpenFile(oAriaApplication.DataDir+'ordhdr','ORDHDR','SH')
*B610498,1 TMI 09/04/2013 [End  ] 
Select Ordhdr

*B610498,2 TMI 09/10/2013 [Start] set order to ORDHDR
*set order to 2
SET ORDER TO ORDHDR
*B610498,2 TMI 09/10/2013 [End  ] 


*B610498,1 TMI 09/04/2013 [Start] use gfopenfile
*USE x:\aria27\dbfs\02\invhdr.dbf IN 2 SHARED
=gfOpenFile(oAriaApplication.DataDir+'invhdr','invhdr','SH')
*B610498,1 TMI 09/04/2013 [End  ] 
SELECT invhdr
SET RELATION TO 'O'+ Invhdr.order INTO Ordhdr ADDITIVE

Replace all invhdr.cdivision with ordhdr.cdivision For cdivision <> ordhdr.cdivision

*B610498,1 TMI 09/04/2013 [Start] 
*Wait window "Program Finished Division updates...Press any key"
=gfModalGen('INM00000B00000',.F.,.F.,.F.,"Program Finished Division updates")
*B610498,1 TMI 09/04/2013 [End  ] 
Select Ordhdr
Use
Select Invhdr
Use

*B610498,2 TMI 09/10/2013 [Start] 
RELEASE oSession
SET DATASESSION TO lnCurrSession
*B610498,2 TMI 09/10/2013 [End  ] 