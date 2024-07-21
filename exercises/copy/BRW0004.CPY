       01  BRW0004I.                                                            
           02  FILLER PIC X(12).                                                
           02  BRW401L    COMP  PIC  S9(4).                                     
           02  BRW401F    PICTURE X.                                            
           02  FILLER REDEFINES BRW401F.                                        
             03 BRW401A    PICTURE X.                                           
           02  BRW401I  PIC X(8).                                               
           02  BRW402L    COMP  PIC  S9(4).                                     
           02  BRW402F    PICTURE X.                                            
           02  FILLER REDEFINES BRW402F.                                        
             03 BRW402A    PICTURE X.                                           
           02  BRW402I  PIC X(2).                                               
           02  BRW403L    COMP  PIC  S9(4).                                     
           02  BRW403F    PICTURE X.                                            
           02  FILLER REDEFINES BRW403F.                                        
             03 BRW403A    PICTURE X.                                           
           02  BRW403I  PIC X(1).                                               
           02  BRW4MSGL    COMP  PIC  S9(4).                                    
           02  BRW4MSGF    PICTURE X.                                           
           02  FILLER REDEFINES BRW4MSGF.                                       
             03 BRW4MSGA    PICTURE X.                                          
           02  BRW4MSGI  PIC X(78).                                             
       01  BRW0004O REDEFINES BRW0004I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  BRW401O PIC 99.99.99.                                            
           02  FILLER PICTURE X(3).                                             
           02  BRW402O PIC XX.                                                  
           02  FILLER PICTURE X(3).                                             
           02  BRW403O  PIC X(1).                                               
           02  FILLER PICTURE X(3).                                             
           02  BRW4MSGO  PIC X(78).                                             
