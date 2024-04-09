CLOSE ALL
SET CPDIALOG OFF 
cd x:\aria4xp\sysfiles\
use syccomp
scan
  lcDir = allt(ccom_ddir)
  use &lcDir.gltypes in 0 alias ('gltypes_'+syccomp.ccomp_id)
endscan

