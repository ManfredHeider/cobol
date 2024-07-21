       01  BSE0002I.                                                            
           02  FILLER PIC X(12).                                                
           02  GESAMTL    COMP  PIC  S9(4).                                     
           02  GESAMTF    PICTURE X.                                            
           02  FILLER REDEFINES GESAMTF.                                        
             03 GESAMTA    PICTURE X.                                           
           02  GESAMTI  PIC X(1).                                               
           02  VONL    COMP  PIC  S9(4).                                        
           02  VONF    PICTURE X.                                               
           02  FILLER REDEFINES VONF.                                           
             03 VONA    PICTURE X.                                              
           02  VONI  PIC X(5).                                                  
           02  BISL    COMP  PIC  S9(4).                                        
           02  BISF    PICTURE X.                                               
           02  FILLER REDEFINES BISF.                                           
             03 BISA    PICTURE X.                                              
           02  BISI  PIC X(5).                                                  
           02  TEXTL    COMP  PIC  S9(4).                                       
           02  TEXTF    PICTURE X.                                              
           02  FILLER REDEFINES TEXTF.                                          
             03 TEXTA    PICTURE X.                                             
           02  TEXTI  PIC X(79).                                                
       01  BSE0002O REDEFINES BSE0002I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  GESAMTO  PIC X(1).                                               
           02  FILLER PICTURE X(3).                                             
           02  VONO  PIC X(5).                                                  
           02  FILLER PICTURE X(3).                                             
           02  BISO  PIC X(5).                                                  
           02  FILLER PICTURE X(3).                                             
           02  TEXTO  PIC X(79).                                                
