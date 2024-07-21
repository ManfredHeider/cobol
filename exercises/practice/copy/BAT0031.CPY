       01  SIGNMAQI.                                                            
           02  FILLER PIC X(12).                                                
           02  MESSHL    COMP  PIC  S9(4).                                      
           02  MESSHF    PICTURE X.                                             
           02  FILLER REDEFINES MESSHF.                                         
             03 MESSHA    PICTURE X.                                            
           02  MESSHI  PIC X(79).                                               
       01  SIGNMAQO REDEFINES SIGNMAQI.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  MESSHO  PIC X(79).                                               
