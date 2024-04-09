*- this program will loop ver the 
SCAN FOR dtrdate >= {^2012-06-15}
  lx=SQLEXEC(ln,"Update itemjrnl set ncost="+STR(ncost,10,3)+",nstkval="+STR(nstkval,10,3)+" where rec_no='"+rec_no+"'")
  IF lx<0
  SET STEP ON on
  ENDIF 
ENDSCAN 


