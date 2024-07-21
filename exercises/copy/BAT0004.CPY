       01  BAT0004I.                                                            
           02  FILLER PIC X(12).                                                
           02  AHILFEGL    COMP  PIC  S9(4).                                    
           02  AHILFEGF    PICTURE X.                                           
           02  FILLER REDEFINES AHILFEGF.                                       
             03 AHILFEGA    PICTURE X.                                          
           02  AHILFEGI  PIC X(66).                                             
           02  AHBLDIDL    COMP  PIC  S9(4).                                    
           02  AHBLDIDF    PICTURE X.                                           
           02  FILLER REDEFINES AHBLDIDF.                                       
             03 AHBLDIDA    PICTURE X.                                          
           02  AHBLDIDI  PIC X(6).                                              
           02  AHILFETD OCCURS 15 TIMES.                                        
             03  AHILFETL    COMP  PIC  S9(4).                                  
             03  AHILFETF    PICTURE X.                                         
             03  AHILFETI  PIC X(79).                                           
       01  BAT0004O REDEFINES BAT0004I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  AHILFEGO  PIC X(66).                                             
           02  FILLER PICTURE X(3).                                             
           02  AHBLDIDO  PIC X(6).                                              
           02  DFHMS1 OCCURS 15 TIMES.                                          
             03  FILLER PICTURE X(2).                                           
             03  AHILFETA    PICTURE X.                                         
             03  AHILFETO  PIC X(79).                                           
