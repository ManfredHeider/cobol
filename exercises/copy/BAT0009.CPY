       01  BAT0009I.                                                            
           02  FILLER PIC X(12).                                                
           02  ARTRACEL    COMP  PIC  S9(4).                                    
           02  ARTRACEF    PICTURE X.                                           
           02  FILLER REDEFINES ARTRACEF.                                       
             03 ARTRACEA    PICTURE X.                                          
           02  ARTRACEI  PIC X(239).                                            
           02  ARVARL    COMP  PIC  S9(4).                                      
           02  ARVARF    PICTURE X.                                             
           02  FILLER REDEFINES ARVARF.                                         
             03 ARVARA    PICTURE X.                                            
           02  ARVARI  PIC X(239).                                              
       01  BAT0009O REDEFINES BAT0009I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  ARTRACEO  PIC X(239).                                            
           02  FILLER PICTURE X(3).                                             
           02  ARVARO  PIC X(239).                                              
