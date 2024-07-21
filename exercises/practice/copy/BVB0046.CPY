       01  BVB0046I.                                                            
           02  FILLER PIC X(12).                                                
           02  AEDZWL    COMP  PIC  S9(4).                                      
           02  AEDZWF    PICTURE X.                                             
           02  FILLER REDEFINES AEDZWF.                                         
             03 AEDZWA    PICTURE X.                                            
           02  AEDZWI  PIC X(7).                                                
           02  AEDZWVL    COMP  PIC  S9(4).                                     
           02  AEDZWVF    PICTURE X.                                            
           02  FILLER REDEFINES AEDZWVF.                                        
             03 AEDZWVA    PICTURE X.                                           
           02  AEDZWVI  PIC X(10).                                              
           02  ANAMEL    COMP  PIC  S9(4).                                      
           02  ANAMEF    PICTURE X.                                             
           02  FILLER REDEFINES ANAMEF.                                         
             03 ANAMEA    PICTURE X.                                            
           02  ANAMEI  PIC X(30).                                               
           02  ATBKOPFL    COMP  PIC  S9(4).                                    
           02  ATBKOPFF    PICTURE X.                                           
           02  FILLER REDEFINES ATBKOPFF.                                       
             03 ATBKOPFA    PICTURE X.                                          
           02  ATBKOPFI  PIC X(79).                                             
           02  ATABZD OCCURS 14 TIMES.                                          
             03  ATABZL    COMP  PIC  S9(4).                                    
             03  ATABZF    PICTURE X.                                           
             03  ATABZI  PIC X(79).                                             
       01  BVB0046O REDEFINES BVB0046I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  AEDZWO  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  AEDZWVO  PIC X(10).                                              
           02  FILLER PICTURE X(3).                                             
           02  ANAMEO  PIC X(30).                                               
           02  FILLER PICTURE X(3).                                             
           02  ATBKOPFO  PIC X(79).                                             
           02  DFHMS1 OCCURS 14 TIMES.                                          
             03  FILLER PICTURE X(2).                                           
             03  ATABZA    PICTURE X.                                           
             03  ATABZO  PIC X(79).                                             
