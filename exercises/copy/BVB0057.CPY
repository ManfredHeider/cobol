       01  BVB0057I.                                                            
           02  FILLER PIC X(12).                                                
           02  A2STNRL    COMP  PIC  S9(4).                                     
           02  A2STNRF    PICTURE X.                                            
           02  FILLER REDEFINES A2STNRF.                                        
             03 A2STNRA    PICTURE X.                                           
           02  A2STNRI  PIC X(7).                                               
           02  A2STPRZL    COMP  PIC  S9(4).                                    
           02  A2STPRZF    PICTURE X.                                           
           02  FILLER REDEFINES A2STPRZF.                                       
             03 A2STPRZA    PICTURE X.                                          
           02  A2STPRZI  PIC X(1).                                              
           02  EBETRNRL    COMP  PIC  S9(4).                                    
           02  EBETRNRF    PICTURE X.                                           
           02  FILLER REDEFINES EBETRNRF.                                       
             03 EBETRNRA    PICTURE X.                                          
           02  EBETRNRI  PIC X(7).                                              
           02  EBETPRZL    COMP  PIC  S9(4).                                    
           02  EBETPRZF    PICTURE X.                                           
           02  FILLER REDEFINES EBETPRZF.                                       
             03 EBETPRZA    PICTURE X.                                          
           02  EBETPRZI  PIC X(1).                                              
           02  EDIRNRL    COMP  PIC  S9(4).                                     
           02  EDIRNRF    PICTURE X.                                            
           02  FILLER REDEFINES EDIRNRF.                                        
             03 EDIRNRA    PICTURE X.                                           
           02  EDIRNRI  PIC X(3).                                               
           02  EDIRBERL    COMP  PIC  S9(4).                                    
           02  EDIRBERF    PICTURE X.                                           
           02  FILLER REDEFINES EDIRBERF.                                       
             03 EDIRBERA    PICTURE X.                                          
           02  EDIRBERI  PIC X(4).                                              
           02  EDIRNAML    COMP  PIC  S9(4).                                    
           02  EDIRNAMF    PICTURE X.                                           
           02  FILLER REDEFINES EDIRNAMF.                                       
             03 EDIRNAMA    PICTURE X.                                          
           02  EDIRNAMI  PIC X(40).                                             
           02  EPRVSTZL    COMP  PIC  S9(4).                                    
           02  EPRVSTZF    PICTURE X.                                           
           02  FILLER REDEFINES EPRVSTZF.                                       
             03 EPRVSTZA    PICTURE X.                                          
           02  EPRVSTZI  PIC X(6).                                              
           02  EPRZSTZL    COMP  PIC  S9(4).                                    
           02  EPRZSTZF    PICTURE X.                                           
           02  FILLER REDEFINES EPRZSTZF.                                       
             03 EPRZSTZA    PICTURE X.                                          
           02  EPRZSTZI  PIC X(6).                                              
           02  ERCKSTZL    COMP  PIC  S9(4).                                    
           02  ERCKSTZF    PICTURE X.                                           
           02  FILLER REDEFINES ERCKSTZF.                                       
             03 ERCKSTZA    PICTURE X.                                          
           02  ERCKSTZI  PIC X(6).                                              
           02  EIBANL    COMP  PIC  S9(4).                                      
           02  EIBANF    PICTURE X.                                             
           02  FILLER REDEFINES EIBANF.                                         
             03 EIBANA    PICTURE X.                                            
           02  EIBANI  PIC X(22).                                               
           02  ABICL    COMP  PIC  S9(4).                                       
           02  ABICF    PICTURE X.                                              
           02  FILLER REDEFINES ABICF.                                          
             03 ABICA    PICTURE X.                                             
           02  ABICI  PIC X(11).                                                
           02  ABLZL    COMP  PIC  S9(4).                                       
           02  ABLZF    PICTURE X.                                              
           02  FILLER REDEFINES ABLZF.                                          
             03 ABLZA    PICTURE X.                                             
           02  ABLZI  PIC X(8).                                                 
           02  ABANKL    COMP  PIC  S9(4).                                      
           02  ABANKF    PICTURE X.                                             
           02  FILLER REDEFINES ABANKF.                                         
             03 ABANKA    PICTURE X.                                            
           02  ABANKI  PIC X(40).                                               
           02  AKTONRL    COMP  PIC  S9(4).                                     
           02  AKTONRF    PICTURE X.                                            
           02  FILLER REDEFINES AKTONRF.                                        
             03 AKTONRA    PICTURE X.                                           
           02  AKTONRI  PIC X(20).                                              
           02  EBVINHL    COMP  PIC  S9(4).                                     
           02  EBVINHF    PICTURE X.                                            
           02  FILLER REDEFINES EBVINHF.                                        
             03 EBVINHA    PICTURE X.                                           
           02  EBVINHI  PIC X(40).                                              
           02  ESPERREL    COMP  PIC  S9(4).                                    
           02  ESPERREF    PICTURE X.                                           
           02  FILLER REDEFINES ESPERREF.                                       
             03 ESPERREA    PICTURE X.                                          
           02  ESPERREI  PIC X(1).                                              
       01  BVB0057O REDEFINES BVB0057I.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  A2STNRO  PIC X(7).                                               
           02  FILLER PICTURE X(3).                                             
           02  A2STPRZO  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  EBETRNRO  PIC X(7).                                              
           02  FILLER PICTURE X(3).                                             
           02  EBETPRZO  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  EDIRNRO  PIC X(3).                                               
           02  FILLER PICTURE X(3).                                             
           02  EDIRBERO  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  EDIRNAMO  PIC X(40).                                             
           02  FILLER PICTURE X(3).                                             
           02  EPRVSTZO  PIC X(6).                                              
           02  FILLER PICTURE X(3).                                             
           02  EPRZSTZO  PIC X(6).                                              
           02  FILLER PICTURE X(3).                                             
           02  ERCKSTZO  PIC X(6).                                              
           02  FILLER PICTURE X(3).                                             
           02  EIBANO  PIC X(22).                                               
           02  FILLER PICTURE X(3).                                             
           02  ABICO  PIC X(11).                                                
           02  FILLER PICTURE X(3).                                             
           02  ABLZO  PIC X(8).                                                 
           02  FILLER PICTURE X(3).                                             
           02  ABANKO  PIC X(40).                                               
           02  FILLER PICTURE X(3).                                             
           02  AKTONRO  PIC X(20).                                              
           02  FILLER PICTURE X(3).                                             
           02  EBVINHO  PIC X(40).                                              
           02  FILLER PICTURE X(3).                                             
           02  ESPERREO  PIC X(1).                                              
