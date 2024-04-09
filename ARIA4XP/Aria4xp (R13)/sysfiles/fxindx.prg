*B610442,1 fix problems in INFORMATION screen TMI 07/24/2013 [Start] 
*- while testing the SMMTDIC I found that there is a case where the value in the field SYDFILES.CFILE_TAG 
*- is incorrect, this program intends to fix this case
*- this program is intended to loop over all the SaaS clients 
*B610442,1 fix problems in INFORMATION screen TMI 07/24/2013 [End  ] 
lcMsg = ' This program is to fix the SYDFILES.CFILE_TAG value.'+chr(13)+;
        'For this program to run copy it to the SYSFILES folder.'
messagebox(lcMsg)

use sydindex in 0 order CFILE_NAM  && CFILE_NAM+CFILE_TAG
use sydfiles in 0 order CFILE_NAM && CFILE_NAM

select sydfiles 
scan
  *- correct the SYDFILES.CFILE_TAG value
  IF !SEEK(SYDFILES.CFILE_NAM + SYDFILES.CFILE_TAG ,'SYDINDEX' )
    =SEEK(SYDFILES.CFILE_NAM ,'SYDINDEX' )
    REPLA CFILE_TAG WITH SYDINDEX.CFILE_TAG 
  ENDIF 
endscan

use IN sydindex 
use IN sydfiles 
