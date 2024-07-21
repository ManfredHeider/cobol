       01  UU972I.                                                              
           02  FILLER PIC X(12).                                                
           02  PNR72L    COMP  PIC  S9(4).                                      
           02  PNR72F    PICTURE X.                                             
           02  FILLER REDEFINES PNR72F.                                         
             03 PNR72A    PICTURE X.                                            
           02  PNR72I  PIC X(7).                                                
           02  DATUM72L    COMP  PIC  S9(4).                                    
           02  DATUM72F    PICTURE X.                                           
           02  FILLER REDEFINES DATUM72F.                                       
             03 DATUM72A    PICTURE X.                                          
           02  DATUM72I  PIC X(8).                                              
           02  UHR72L    COMP  PIC  S9(4).                                      
           02  UHR72F    PICTURE X.                                             
           02  FILLER REDEFINES UHR72F.                                         
             03 UHR72A    PICTURE X.                                            
           02  UHR72I  PIC X(5).                                                
           02  VBNR72L    COMP  PIC  S9(4).                                     
           02  VBNR72F    PICTURE X.                                            
           02  FILLER REDEFINES VBNR72F.                                        
             03 VBNR72A    PICTURE X.                                           
           02  VBNR72I  PIC X(7).                                               
           02  GES72L    COMP  PIC  S9(4).                                      
           02  GES72F    PICTURE X.                                             
           02  FILLER REDEFINES GES72F.                                         
             03 GES72A    PICTURE X.                                            
           02  GES72I  PIC X(2).                                                
           02  VNR72L    COMP  PIC  S9(4).                                      
           02  VNR72F    PICTURE X.                                             
           02  FILLER REDEFINES VNR72F.                                         
             03 VNR72A    PICTURE X.                                            
           02  VNR72I  PIC X(15).                                               
           02  ZEILE72D OCCURS 18 TIMES.                                        
             03  ZEILE72L    COMP  PIC  S9(4).                                  
             03  ZEILE72F    PICTURE X.                                         
             03  ZEILE72I  PIC X(79).                                           
           02  VBNRA72L    COMP  PIC  S9(4).                                    
           02  VBNRA72F    PICTURE X.                                           
           02  FILLER REDEFINES VBNRA72F.                                       
             03 VBNRA72A    PICTURE X.                                          
           02  VBNRA72I  PIC X(7).                                              
           02  GESA72L    COMP  PIC  S9(4).                                     
           02  GESA72F    PICTURE X.                                            
           02  FILLER REDEFINES GESA72F.                                        
             03 GESA72A    PICTURE X.                                           
           02  GESA72I  PIC X(2).                                               
           02  VNRA72L    COMP  PIC  S9(4).                                     
           02  VNRA72F    PICTURE X.                                            
           02  FILLER REDEFINES VNRA72F.                                        
             03 VNRA72A    PICTURE X.                                           
           02  VNRA72I  PIC X(15).                                              
           02  NAMEA72L    COMP  PIC  S9(4).                                    
           02  NAMEA72F    PICTURE X.                                           
           02  FILLER REDEFINES NAMEA72F.                                       
             03 NAMEA72A    PICTURE X.                                          
           02  NAMEA72I  PIC X(8).                                              
           02  TRANS72L    COMP  PIC  S9(4).                                    
           02  TRANS72F    PICTURE X.                                           
           02  FILLER REDEFINES TRANS72F.                                       
             03 TRANS72A    PICTURE X.                                          
           02  TRANS72I  PIC X(4).                                              
           02  FEHL72L    COMP  PIC  S9(4).                                     
           02  FEHL72F    PICTURE X.                                            
           02  FILLER REDEFINES FEHL72F.                                        
             03 FEHL72A    PICTURE X.                                           
           02  FEHL72I  PIC X(78).                                              
       01  UU972O REDEFINES UU972I.                                             
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  PNR72O  PIC X(7).                                                
           02  FILLER PICTURE X(3).                                             
           02  DATUM72O  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  UHR72O  PIC X(5).                                                
           02  FILLER PICTURE X(3).                                             
           02  VBNR72O  PIC X(7).                                               
           02  FILLER PICTURE X(3).                                             
           02  GES72O  PIC X(2).                                                
           02  FILLER PICTURE X(3).                                             
           02  VNR72O  PIC X(15).                                               
           02  DFHMS1 OCCURS 18 TIMES.                                          
             03  FILLER PICTURE X(2).                                           
             03  ZEILE72A    PICTURE X.                                         
             03  ZEILE72O  PIC X(79).                                           
           02  FILLER PICTURE X(3).                                             
           02  VBNRA72O  PIC X(7).                                              
           02  FILLER PICTURE X(3).                                             
           02  GESA72O  PIC X(2).                                               
           02  FILLER PICTURE X(3).                                             
           02  VNRA72O  PIC X(15).                                              
           02  FILLER PICTURE X(3).                                             
           02  NAMEA72O  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  TRANS72O  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  FEHL72O  PIC X(78).                                              
