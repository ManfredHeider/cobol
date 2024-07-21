       01  BRW0002I.                                                            
           02  FILLER PIC X(12).                                                
           02  BRW201L    COMP  PIC  S9(4).                                     
           02  BRW201F    PICTURE X.                                            
           02  FILLER REDEFINES BRW201F.                                        
             03 BRW201A    PICTURE X.                                           
           02  BRW201I  PIC 9(6).                                               
           02  BRW202L    COMP  PIC  S9(4).                                     
           02  BRW202F    PICTURE X.                                            
           02  FILLER REDEFINES BRW202F.                                        
             03 BRW202A    PICTURE X.                                           
           02  BRW202I  PIC 9(15)V99.                                           
           02  BRW203L    COMP  PIC  S9(4).                                     
           02  BRW203F    PICTURE X.                                            
           02  FILLER REDEFINES BRW203F.                                        
             03 BRW203A    PICTURE X.                                           
           02  BRW203I  PIC 9(6).                                               
           02  BRW204L    COMP  PIC  S9(4).                                     
           02  BRW204F    PICTURE X.                                            
           02  FILLER REDEFINES BRW204F.                                        
             03 BRW204A    PICTURE X.                                           
           02  BRW204I  PIC 9(15)V99.                                           
           02  BRW205L    COMP  PIC  S9(4).                                     
           02  BRW205F    PICTURE X.                                            
           02  FILLER REDEFINES BRW205F.                                        
             03 BRW205A    PICTURE X.                                           
           02  BRW205I  PIC 9(6).                                               
           02  BRW2MSGL    COMP  PIC  S9(4).                                    
           02  BRW2MSGF    PICTURE X.                                           
           02  FILLER REDEFINES BRW2MSGF.                                       
             03 BRW2MSGA    PICTURE X.                                          
           02  BRW2MSGI  PIC X(70).                                             
           02  BRW206L    COMP  PIC  S9(4).                                     
           02  BRW206F    PICTURE X.                                            
           02  FILLER REDEFINES BRW206F.                                        
             03 BRW206A    PICTURE X.                                           
           02  BRW206I  PIC X(1).                                               
       01  BRW0002O REDEFINES BRW0002I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  BRW201O PIC ZZ.ZZ9.                                              
           02  FILLER PICTURE X(3).                                             
           02  BRW202O PIC ZZ.ZZZ.ZZZ.ZZ9,99.                                   
           02  FILLER PICTURE X(3).                                             
           02  BRW203O PIC ZZ.ZZ9.                                              
           02  FILLER PICTURE X(3).                                             
           02  BRW204O PIC ZZ.ZZZ.ZZZ.ZZ9,99.                                   
           02  FILLER PICTURE X(3).                                             
           02  BRW205O PIC ZZ.ZZ9.                                              
           02  FILLER PICTURE X(3).                                             
           02  BRW2MSGO  PIC X(70).                                             
           02  FILLER PICTURE X(3).                                             
           02  BRW206O  PIC X(1).                                               
