       01  BVB0014I.                                                            
           02  FILLER PIC X(12).                                                
           02  ASTNRL    COMP  PIC  S9(4).                                      
           02  ASTNRF    PICTURE X.                                             
           02  FILLER REDEFINES ASTNRF.                                         
             03 ASTNRA    PICTURE X.                                            
           02  ASTNRI  PIC X(7).                                                
           02  ASTPRZL    COMP  PIC  S9(4).                                     
           02  ASTPRZF    PICTURE X.                                            
           02  FILLER REDEFINES ASTPRZF.                                        
             03 ASTPRZA    PICTURE X.                                           
           02  ASTPRZI  PIC X(1).                                               
           02  AINFOZLL    COMP  PIC  S9(4).                                    
           02  AINFOZLF    PICTURE X.                                           
           02  FILLER REDEFINES AINFOZLF.                                       
             03 AINFOZLA    PICTURE X.                                          
           02  AINFOZLI  PIC X(65).                                             
           02  AAUSZD OCCURS 7 TIMES.                                           
             03  AAUSZL    COMP  PIC  S9(4).                                    
             03  AAUSZF    PICTURE X.                                           
             03  AAUSZI  PIC X(79).                                             
       01  BVB0014O REDEFINES BVB0014I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  ASTNRO  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  ASTPRZO  PIC X(1).                                               
           02  FILLER PICTURE X(3).                                             
           02  AINFOZLO  PIC X(65).                                             
           02  DFHMS1 OCCURS 7 TIMES.                                           
             03  FILLER PICTURE X(2).                                           
             03  AAUSZA    PICTURE X.                                           
             03  AAUSZO  PIC X(79).                                             
