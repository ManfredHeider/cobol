       01  UU905I.                                                              
           02  FILLER PIC X(12).                                                
           02  PNR5L    COMP  PIC  S9(4).                                       
           02  PNR5F    PICTURE X.                                              
           02  FILLER REDEFINES PNR5F.                                          
             03 PNR5A    PICTURE X.                                             
           02  PNR5I  PIC X(7).                                                 
           02  DAT5L    COMP  PIC  S9(4).                                       
           02  DAT5F    PICTURE X.                                              
           02  FILLER REDEFINES DAT5F.                                          
             03 DAT5A    PICTURE X.                                             
           02  DAT5I  PIC X(8).                                                 
           02  UHR5L    COMP  PIC  S9(4).                                       
           02  UHR5F    PICTURE X.                                              
           02  FILLER REDEFINES UHR5F.                                          
             03 UHR5A    PICTURE X.                                             
           02  UHR5I  PIC X(5).                                                 
           02  TEXT51L    COMP  PIC  S9(4).                                     
           02  TEXT51F    PICTURE X.                                            
           02  FILLER REDEFINES TEXT51F.                                        
             03 TEXT51A    PICTURE X.                                           
           02  TEXT51I  PIC X(41).                                              
           02  TEXT52L    COMP  PIC  S9(4).                                     
           02  TEXT52F    PICTURE X.                                            
           02  FILLER REDEFINES TEXT52F.                                        
             03 TEXT52A    PICTURE X.                                           
           02  TEXT52I  PIC X(40).                                              
           02  VBD5L    COMP  PIC  S9(4).                                       
           02  VBD5F    PICTURE X.                                              
           02  FILLER REDEFINES VBD5F.                                          
             03 VBD5A    PICTURE X.                                             
           02  VBD5I  PIC X(7).                                                 
           02  MMJJ5L    COMP  PIC  S9(4).                                      
           02  MMJJ5F    PICTURE X.                                             
           02  FILLER REDEFINES MMJJ5F.                                         
             03 MMJJ5A    PICTURE X.                                            
           02  MMJJ5I  PIC X(4).                                                
           02  TRANS5L    COMP  PIC  S9(4).                                     
           02  TRANS5F    PICTURE X.                                            
           02  FILLER REDEFINES TRANS5F.                                        
             03 TRANS5A    PICTURE X.                                           
           02  TRANS5I  PIC X(4).                                               
           02  FEHL5L    COMP  PIC  S9(4).                                      
           02  FEHL5F    PICTURE X.                                             
           02  FILLER REDEFINES FEHL5F.                                         
             03 FEHL5A    PICTURE X.                                            
           02  FEHL5I  PIC X(79).                                               
       01  UU905O REDEFINES UU905I.                                             
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  PNR5O  PIC X(7).                                                 
           02  FILLER PICTURE X(3).                                             
           02  DAT5O  PIC X(8).                                                 
           02  FILLER PICTURE X(3).                                             
           02  UHR5O  PIC X(5).                                                 
           02  FILLER PICTURE X(3).                                             
           02  TEXT51O  PIC X(41).                                              
           02  FILLER PICTURE X(3).                                             
           02  TEXT52O  PIC X(40).                                              
           02  FILLER PICTURE X(3).                                             
           02  VBD5O  PIC X(7).                                                 
           02  FILLER PICTURE X(3).                                             
           02  MMJJ5O  PIC X(4).                                                
           02  FILLER PICTURE X(3).                                             
           02  TRANS5O  PIC X(4).                                               
           02  FILLER PICTURE X(3).                                             
           02  FEHL5O  PIC X(79).                                               
