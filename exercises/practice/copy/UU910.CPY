       01  UU910I.                                                              
           02  FILLER PIC X(12).                                                
           02  PNR10L    COMP  PIC  S9(4).                                      
           02  PNR10F    PICTURE X.                                             
           02  FILLER REDEFINES PNR10F.                                         
             03 PNR10A    PICTURE X.                                            
           02  PNR10I  PIC X(7).                                                
           02  DAT10L    COMP  PIC  S9(4).                                      
           02  DAT10F    PICTURE X.                                             
           02  FILLER REDEFINES DAT10F.                                         
             03 DAT10A    PICTURE X.                                            
           02  DAT10I  PIC X(8).                                                
           02  UHR10L    COMP  PIC  S9(4).                                      
           02  UHR10F    PICTURE X.                                             
           02  FILLER REDEFINES UHR10F.                                         
             03 UHR10A    PICTURE X.                                            
           02  UHR10I  PIC X(5).                                                
           02  TEXT10L    COMP  PIC  S9(4).                                     
           02  TEXT10F    PICTURE X.                                            
           02  FILLER REDEFINES TEXT10F.                                        
             03 TEXT10A    PICTURE X.                                           
           02  TEXT10I  PIC X(51).                                              
           02  TVBNR10L    COMP  PIC  S9(4).                                    
           02  TVBNR10F    PICTURE X.                                           
           02  FILLER REDEFINES TVBNR10F.                                       
             03 TVBNR10A    PICTURE X.                                          
           02  TVBNR10I  PIC X(6).                                              
           02  VBD10L    COMP  PIC  S9(4).                                      
           02  VBD10F    PICTURE X.                                             
           02  FILLER REDEFINES VBD10F.                                         
             03 VBD10A    PICTURE X.                                            
           02  VBD10I  PIC X(7).                                                
           02  TLETZ10L    COMP  PIC  S9(4).                                    
           02  TLETZ10F    PICTURE X.                                           
           02  FILLER REDEFINES TLETZ10F.                                       
             03 TLETZ10A    PICTURE X.                                          
           02  TLETZ10I  PIC X(6).                                              
           02  MM10L    COMP  PIC  S9(4).                                       
           02  MM10F    PICTURE X.                                              
           02  FILLER REDEFINES MM10F.                                          
             03 MM10A    PICTURE X.                                             
           02  MM10I  PIC X(2).                                                 
           02  TMON10L    COMP  PIC  S9(4).                                     
           02  TMON10F    PICTURE X.                                            
           02  FILLER REDEFINES TMON10F.                                        
             03 TMON10A    PICTURE X.                                           
           02  TMON10I  PIC X(7).                                               
           02  TVON10L    COMP  PIC  S9(4).                                     
           02  TVON10F    PICTURE X.                                            
           02  FILLER REDEFINES TVON10F.                                        
             03 TVON10A    PICTURE X.                                           
           02  TVON10I  PIC X(3).                                               
           02  VON10L    COMP  PIC  S9(4).                                      
           02  VON10F    PICTURE X.                                             
           02  FILLER REDEFINES VON10F.                                         
             03 VON10A    PICTURE X.                                            
           02  VON10I  PIC X(4).                                                
           02  TBIS10L    COMP  PIC  S9(4).                                     
           02  TBIS10F    PICTURE X.                                            
           02  FILLER REDEFINES TBIS10F.                                        
             03 TBIS10A    PICTURE X.                                           
           02  TBIS10I  PIC X(3).                                               
           02  BIS10L    COMP  PIC  S9(4).                                      
           02  BIS10F    PICTURE X.                                             
           02  FILLER REDEFINES BIS10F.                                         
             03 BIS10A    PICTURE X.                                            
           02  BIS10I  PIC X(4).                                                
           02  TMMJJ10L    COMP  PIC  S9(4).                                    
           02  TMMJJ10F    PICTURE X.                                           
           02  FILLER REDEFINES TMMJJ10F.                                       
             03 TMMJJ10A    PICTURE X.                                          
           02  TMMJJ10I  PIC X(6).                                              
           02  TRANS10L    COMP  PIC  S9(4).                                    
           02  TRANS10F    PICTURE X.                                           
           02  FILLER REDEFINES TRANS10F.                                       
             03 TRANS10A    PICTURE X.                                          
           02  TRANS10I  PIC X(4).                                              
           02  FEHL10L    COMP  PIC  S9(4).                                     
           02  FEHL10F    PICTURE X.                                            
           02  FILLER REDEFINES FEHL10F.                                        
             03 FEHL10A    PICTURE X.                                           
           02  FEHL10I  PIC X(79).                                              
       01  UU910O REDEFINES UU910I.                                             
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  PNR10O  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  DAT10O  PIC X(8).                                                
           02  FILLER PICTURE X(3).                                             
           02  UHR10O  PIC X(5).                                                
           02  FILLER PICTURE X(3).                                             
           02  TEXT10O  PIC X(51).                                              
           02  FILLER PICTURE X(3).                                             
           02  TVBNR10O  PIC X(6).                                              
           02  FILLER PICTURE X(3).                                             
           02  VBD10O  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  TLETZ10O  PIC X(6).                                              
           02  FILLER PICTURE X(3).                                             
           02  MM10O  PIC X(2).                                                 
           02  FILLER PICTURE X(3).                                             
           02  TMON10O  PIC X(7).                                               
           02  FILLER PICTURE X(3).                                             
           02  TVON10O  PIC X(3).                                               
           02  FILLER PICTURE X(3).                                             
           02  VON10O  PIC X(4).                                                
           02  FILLER PICTURE X(3).                                             
           02  TBIS10O  PIC X(3).                                               
           02  FILLER PICTURE X(3).                                             
           02  BIS10O  PIC X(4).                                                
           02  FILLER PICTURE X(3).                                             
           02  TMMJJ10O  PIC X(6).                                              
           02  FILLER PICTURE X(3).                                             
           02  TRANS10O  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  FEHL10O  PIC X(79).                                              
