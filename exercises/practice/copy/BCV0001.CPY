       01  BCV0001I.                                                            
           02  FILLER PIC X(12).                                                
           02  CPNRL    COMP  PIC  S9(4).                                       
           02  CPNRF    PICTURE X.                                              
           02  FILLER REDEFINES CPNRF.                                          
             03 CPNRA    PICTURE X.                                             
           02  CPNRI  PIC X(7).                                                 
           02  CDATL    COMP  PIC  S9(4).                                       
           02  CDATF    PICTURE X.                                              
           02  FILLER REDEFINES CDATF.                                          
             03 CDATA    PICTURE X.                                             
           02  CDATI  PIC X(8).                                                 
           02  CUHRL    COMP  PIC  S9(4).                                       
           02  CUHRF    PICTURE X.                                              
           02  FILLER REDEFINES CUHRF.                                          
             03 CUHRA    PICTURE X.                                             
           02  CUHRI  PIC X(5).                                                 
           02  CFKTL    COMP  PIC  S9(4).                                       
           02  CFKTF    PICTURE X.                                              
           02  FILLER REDEFINES CFKTF.                                          
             03 CFKTA    PICTURE X.                                             
           02  CFKTI  PIC X(1).                                                 
           02  CBRFL    COMP  PIC  S9(4).                                       
           02  CBRFF    PICTURE X.                                              
           02  FILLER REDEFINES CBRFF.                                          
             03 CBRFA    PICTURE X.                                             
           02  CBRFI  PIC X(16).                                                
           02  CVBNRL    COMP  PIC  S9(4).                                      
           02  CVBNRF    PICTURE X.                                             
           02  FILLER REDEFINES CVBNRF.                                         
             03 CVBNRA    PICTURE X.                                            
           02  CVBNRI  PIC X(7).                                                
           02  CFEHLL    COMP  PIC  S9(4).                                      
           02  CFEHLF    PICTURE X.                                             
           02  FILLER REDEFINES CFEHLF.                                         
             03 CFEHLA    PICTURE X.                                            
           02  CFEHLI  PIC X(79).                                               
       01  BCV0001O REDEFINES BCV0001I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  CPNRO  PIC X(7).                                                 
           02  FILLER PICTURE X(3).                                             
           02  CDATO  PIC X(8).                                                 
           02  FILLER PICTURE X(3).                                             
           02  CUHRO  PIC X(5).                                                 
           02  FILLER PICTURE X(3).                                             
           02  CFKTO  PIC X(1).                                                 
           02  FILLER PICTURE X(3).                                             
           02  CBRFO  PIC X(16).                                                
           02  FILLER PICTURE X(3).                                             
           02  CVBNRO  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  CFEHLO  PIC X(79).                                               
