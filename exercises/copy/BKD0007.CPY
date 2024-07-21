       01  BKD0007I.                                                            
           02  FILLER PIC X(12).                                                
           02  DATUML    COMP  PIC  S9(4).                                      
           02  DATUMF    PICTURE X.                                             
           02  FILLER REDEFINES DATUMF.                                         
             03 DATUMA    PICTURE X.                                            
           02  DATUMI  PIC X(8).                                                
           02  UHRL    COMP  PIC  S9(4).                                        
           02  UHRF    PICTURE X.                                               
           02  FILLER REDEFINES UHRF.                                           
             03 UHRA    PICTURE X.                                              
           02  UHRI  PIC X(5).                                                  
           02  PNRL    COMP  PIC  S9(4).                                        
           02  PNRF    PICTURE X.                                               
           02  FILLER REDEFINES PNRF.                                           
             03 PNRA    PICTURE X.                                              
           02  PNRI  PIC X(7).                                                  
           02  V0819L    COMP  PIC  S9(4).                                      
           02  V0819F    PICTURE X.                                             
           02  FILLER REDEFINES V0819F.                                         
             03 V0819A    PICTURE X.                                            
           02  V0819I  PIC X(1).                                                
           02  V2201L    COMP  PIC  S9(4).                                      
           02  V2201F    PICTURE X.                                             
           02  FILLER REDEFINES V2201F.                                         
             03 V2201A    PICTURE X.                                            
           02  V2201I  PIC X(79).                                               
           02  V2315L    COMP  PIC  S9(4).                                      
           02  V2315F    PICTURE X.                                             
           02  FILLER REDEFINES V2315F.                                         
             03 V2315A    PICTURE X.                                            
           02  V2315I  PIC X(4).                                                
           02  V2401L    COMP  PIC  S9(4).                                      
           02  V2401F    PICTURE X.                                             
           02  FILLER REDEFINES V2401F.                                         
             03 V2401A    PICTURE X.                                            
           02  V2401I  PIC X(25).                                               
       01  BKD0007O REDEFINES BKD0007I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  DATUMO  PIC X(8).                                                
           02  FILLER PICTURE X(3).                                             
           02  UHRO  PIC X(5).                                                  
           02  FILLER PICTURE X(3).                                             
           02  PNRO  PIC X(7).                                                  
           02  FILLER PICTURE X(3).                                             
           02  V0819O  PIC X(1).                                                
           02  FILLER PICTURE X(3).                                             
           02  V2201O  PIC X(79).                                               
           02  FILLER PICTURE X(3).                                             
           02  V2315O  PIC X(4).                                                
           02  FILLER PICTURE X(3).                                             
           02  V2401O  PIC X(25).                                               
