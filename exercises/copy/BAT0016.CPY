       01  BAT0016I.                                                            
           02  FILLER PIC X(12).                                                
           02  EAUSWL    COMP  PIC  S9(4).                                      
           02  EAUSWF    PICTURE X.                                             
           02  FILLER REDEFINES EAUSWF.                                         
             03 EAUSWA    PICTURE X.                                            
           02  EAUSWI  PIC X(1).                                                
       01  BAT0016O REDEFINES BAT0016I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  EAUSWO  PIC X(1).                                                
