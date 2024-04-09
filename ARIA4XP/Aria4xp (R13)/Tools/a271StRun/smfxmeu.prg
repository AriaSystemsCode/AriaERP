*B609580,1 TMI 05/07/2011 [Start] a fix program to do the following
*B609580,1 TMI                    1. remove extra items from the menu from the customers' intallations
*B609580,1 TMI                    2. remove the ALPLIST from the SYDREPRT as it conflects with the ALPLIST in the SYDOBJCT 

 
PARAMETER lcSysPath
*- This program is to remove unneeded lines from the menu over all the SaaS customers
WAIT WINDOW nowait "This program will remove from the SYCMENU the PROSS_ID of values 'SMBNRBL   |SMDLRBL   |SMCMCNV   |SMISACN   '"

SELECT 0
USE (lcSysPath+'SYCMENU') 
WAIT WINDOW NOWAIT DBF()
DELETE FOR CPROSS_ID $ 'SMBNRBL   |SMDLRBL   |SMCMCNV   |SMISACN   '
USE

WAIT WINDOW NOWAIT [  DELETE FOR CREP_ID = 'ALPLIST'  ]
USE (lcSysPath+'SYDREPRT') 
DELETE FOR CREP_ID = 'ALPLIST'

wait clear