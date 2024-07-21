       01  BVB0007I.                                                            
           02  FILLER PIC X(12).                                                
           02  AKPFZ1L    COMP  PIC  S9(4).                                     
           02  AKPFZ1F    PICTURE X.                                            
           02  FILLER REDEFINES AKPFZ1F.                                        
             03 AKPFZ1A    PICTURE X.                                           
           02  AKPFZ1I  PIC X(79).                                              
           02  AKPFZ2L    COMP  PIC  S9(4).                                     
           02  AKPFZ2F    PICTURE X.                                            
           02  FILLER REDEFINES AKPFZ2F.                                        
             03 AKPFZ2A    PICTURE X.                                           
           02  AKPFZ2I  PIC X(79).                                              
           02  AKPFZ3L    COMP  PIC  S9(4).                                     
           02  AKPFZ3F    PICTURE X.                                            
           02  FILLER REDEFINES AKPFZ3F.                                        
             03 AKPFZ3A    PICTURE X.                                           
           02  AKPFZ3I  PIC X(79).                                              
           02  AAUSZD OCCURS 13 TIMES.                                          
             03  AAUSZL    COMP  PIC  S9(4).                                    
             03  AAUSZF    PICTURE X.                                           
             03  AAUSZI  PIC X(79).                                             
           02  ASUMZL    COMP  PIC  S9(4).                                      
           02  ASUMZF    PICTURE X.                                             
           02  FILLER REDEFINES ASUMZF.                                         
             03 ASUMZA    PICTURE X.                                            
           02  ASUMZI  PIC X(79).                                               
       01  BVB0007O REDEFINES BVB0007I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  AKPFZ1O  PIC X(79).                                              
           02  FILLER PICTURE X(3).                                             
           02  AKPFZ2O  PIC X(79).                                              
           02  FILLER PICTURE X(3).                                             
           02  AKPFZ3O  PIC X(79).                                              
           02  DFHMS1 OCCURS 13 TIMES.                                          
             03  FILLER PICTURE X(2).                                           
             03  AAUSZA    PICTURE X.                                           
             03  AAUSZO  PIC X(79).                                             
           02  FILLER PICTURE X(3).                                             
           02  ASUMZO  PIC X(79).                                               
