*!*	*T20140730.0010 - Cannot get the Financial reports to run. Is something missing on the setup?
*!*	This program is created to fix the data of the GLTYPES file
*!*	I found in the demo Co. 99 that the GLTYPES file has 01,02 for each type
*!*	I found in the code in the GLINCSTA it requires that there MUST be a 01 and 02 types
*!*	I found in the Sharon Co. that there is only 00
*!*	To resolve the current problem they have to do the following
*!*	1. run the attached fix that will empty the CTYPECODE file in both glsegval AND glaccchar files
*!*	2. the customer should then go into the TYPES AND RANGES screen in the GL menu and delete all the 00 types
*!*	and add instead a 01, 02 types
*!*	3. the attahed program should then be run again to refill the ctypecode fields in the glsegval and glacchar files
*********************************************************************************************
* Modifications
*T20140730.0010, add the setting SET CPDIA
* 
*********************************************************************************************

*- Run this program twice
*one for emptying the field  ctypecode
*2nd to fill it with the correct values
CLOSE ALL
SET DELETED on
*T20140730.0010,1 TMI 09/02/2014 14:51 [Start] suppress the CP dialog
SET CPDIALOG OFF 
*T20140730.0010,1 TMI 09/02/2014 14:51 [End  ] 

set step on
cd x:\aria4xp\sysfiles\
use syccomp
scan
  lcDir = allt(ccom_ddir)
  cd (lcDir)
  do lfDoChange
endscan

****************************************************************
*
*   function lfDoChange
*
****************************************************************
function lfDoChange
if USED('glsegval')
  use in glsegval
endif   
if USED('gltypes')
  use in gltypes
endif   
if USED('glacchar')
  use in glacchar
endif   

USE glsegval IN 0
USE gltypes IN 0
USE glacchar IN 0

select glsegval
replace ctypecode with left(ctypecode,1) + '01' for right(ctypecode,2) = '00'

select gltypes 
replace ctypecode with left(ctypecode,1) + '01' for right(ctypecode,2) = '00'

select glacchar 
replace ctypecode with left(ctypecode,1) + '01' for right(ctypecode,2) = '00'


*- ignore the rest of the code, simple replace for right(ctypecode,2)='00'
return

llContinue = .T.
SELECT glsegval
LOCATE FOR !EMPTY(ctypecode )
IF found()
  repla ctypecode with ''all
  llContinue = .F.
ENDIF   

SELECT glacchar
LOCATE FOR !EMPTY(ctypecode )
IF found()
  repla ctypecode with ''all
  llContinue = .F.
ENDIF   

IF !llContinue
  MESSAGEBOX('Emptied the field ctypecode ')
  RETURN
ENDIF 

sele gltypes
scan for right(ctypecode,2) = '00'
  repla ctypecode with left(ctypecode,2)+'1'
endscan
 

lfUpd('glacchar')
lfUpd('glsegval')
MESSAGEBOX('filled the field ctypecode with the new correct values ')

*---------------------------
*   lfUpd
*---------------------------
FUNCTION lfUpd
PARAMETERS lcAlias
SELECT &lcAlias
SCAN 
  IF ALIAS() = 'GLSEGVAL'
    lcSegVl = ALLTRIM(&lcAlias..CSEGVALUE)
  ELSE
    lcSegVl = SUBSTR(&lcAlias..CACCTCODE,1,AT('-',&lcAlias..CACCTCODE)-1)
  ENDIF 
  
  SELECT gltypes
  LOCATE FOR BETWEEN(val(lcSegVl),val(CTYPLACNO),val(CTYPUACNO))
  IF FOUND()
    SELECT &lcAlias
    REPLACE ctypecode WITH gltypes.ctypecode
  ENDIF 
ENDSCAN 
