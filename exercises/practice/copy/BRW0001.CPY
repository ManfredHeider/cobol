       01  BRW0001I.                                                            
           02  FILLER PIC X(12).                                                
           02  BRW101L    COMP  PIC  S9(4).                                     
           02  BRW101F    PICTURE X.                                            
           02  FILLER REDEFINES BRW101F.                                        
             03 BRW101A    PICTURE X.                                           
           02  BRW101I  PIC X(4).                                               
           02  BRW1MSGL    COMP  PIC  S9(4).                                    
           02  BRW1MSGF    PICTURE X.                                           
           02  FILLER REDEFINES BRW1MSGF.                                       
             03 BRW1MSGA    PICTURE X.                                          
           02  BRW1MSGI  PIC X(78).                                             
       01  BRW0001O REDEFINES BRW0001I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  BRW101O  PIC X(4).                                               
           02  FILLER PICTURE X(3).                                             
           02  BRW1MSGO  PIC X(78).                                             
