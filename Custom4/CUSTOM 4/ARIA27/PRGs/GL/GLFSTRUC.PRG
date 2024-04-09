SELECT &lcRpFields;
  FROM &lcRpFiles;
 WHERE &lcRpExp;
 &lcRpOrder;
 INTO  DBF &gcWorkDir.&lcRpTargt


*** Report printing
                  
DO gfDispRe WITH lcRpForm
  


IF USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
  ERASE &gcWorkDir.&lcRpTargt..DBF
ENDIF



