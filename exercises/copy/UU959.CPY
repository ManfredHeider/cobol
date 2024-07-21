       01  UU959I.                                                              
           02  FILLER PIC X(12).                                                
           02  PNRHIL    COMP  PIC  S9(4).                                      
           02  PNRHIF    PICTURE X.                                             
           02  FILLER REDEFINES PNRHIF.                                         
             03 PNRHIA    PICTURE X.                                            
           02  PNRHII  PIC X(7).                                                
           02  DATHIL    COMP  PIC  S9(4).                                      
           02  DATHIF    PICTURE X.                                             
           02  FILLER REDEFINES DATHIF.                                         
             03 DATHIA    PICTURE X.                                            
           02  DATHII  PIC X(8).                                                
           02  UHRHIL    COMP  PIC  S9(4).                                      
           02  UHRHIF    PICTURE X.                                             
           02  FILLER REDEFINES UHRHIF.                                         
             03 UHRHIA    PICTURE X.                                            
           02  UHRHII  PIC X(5).                                                
       01  UU959O REDEFINES UU959I.                                             
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  PNRHIO  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  DATHIO  PIC X(8).                                                
           02  FILLER PICTURE X(3).                                             
           02  UHRHIO  PIC X(5).                                                
