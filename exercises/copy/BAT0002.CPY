       01  BAT0002I.                                                            
           02  FILLER PIC X(12).                                                
           02  ARINFOL    COMP  PIC  S9(4).                                     
           02  ARINFOF    PICTURE X.                                            
           02  FILLER REDEFINES ARINFOF.                                        
             03 ARINFOA    PICTURE X.                                           
           02  ARINFOI  PIC X(79).                                              
           02  ARHINWEL    COMP  PIC  S9(4).                                    
           02  ARHINWEF    PICTURE X.                                           
           02  FILLER REDEFINES ARHINWEF.                                       
             03 ARHINWEA    PICTURE X.                                          
           02  ARHINWEI  PIC X(79).                                             
           02  ERTRANSL    COMP  PIC  S9(4).                                    
           02  ERTRANSF    PICTURE X.                                           
           02  FILLER REDEFINES ERTRANSF.                                       
             03 ERTRANSA    PICTURE X.                                          
           02  ERTRANSI  PIC X(4).                                              
           02  ERKEYL    COMP  PIC  S9(4).                                      
           02  ERKEYF    PICTURE X.                                             
           02  FILLER REDEFINES ERKEYF.                                         
             03 ERKEYA    PICTURE X.                                            
           02  ERKEYI  PIC X(32).                                               
           02  ARCTVTXL    COMP  PIC  S9(4).                                    
           02  ARCTVTXF    PICTURE X.                                           
           02  FILLER REDEFINES ARCTVTXF.                                       
             03 ARCTVTXA    PICTURE X.                                          
           02  ARCTVTXI  PIC X(4).                                              
           02  ERCTVL    COMP  PIC  S9(4).                                      
           02  ERCTVF    PICTURE X.                                             
           02  FILLER REDEFINES ERCTVF.                                         
             03 ERCTVA    PICTURE X.                                            
           02  ERCTVI  PIC X(7).                                                
           02  AROKTXTL    COMP  PIC  S9(4).                                    
           02  AROKTXTF    PICTURE X.                                           
           02  FILLER REDEFINES AROKTXTF.                                       
             03 AROKTXTA    PICTURE X.                                          
           02  AROKTXTI  PIC X(3).                                              
           02  EROKL    COMP  PIC  S9(4).                                       
           02  EROKF    PICTURE X.                                              
           02  FILLER REDEFINES EROKF.                                          
             03 EROKA    PICTURE X.                                             
           02  EROKI  PIC X(1).                                                 
           02  ARFTASTL    COMP  PIC  S9(4).                                    
           02  ARFTASTF    PICTURE X.                                           
           02  FILLER REDEFINES ARFTASTF.                                       
             03 ARFTASTA    PICTURE X.                                          
           02  ARFTASTI  PIC X(79).                                             
       01  BAT0002O REDEFINES BAT0002I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  ARINFOO  PIC X(79).                                              
           02  FILLER PICTURE X(3).                                             
           02  ARHINWEO  PIC X(79).                                             
           02  FILLER PICTURE X(3).                                             
           02  ERTRANSO  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  ERKEYO  PIC X(32).                                               
           02  FILLER PICTURE X(3).                                             
           02  ARCTVTXO  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  ERCTVO  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  AROKTXTO  PIC X(3).                                              
           02  FILLER PICTURE X(3).                                             
           02  EROKO  PIC X(1).                                                 
           02  FILLER PICTURE X(3).                                             
           02  ARFTASTO  PIC X(79).                                             
