      *****************************************************************         
      *    COPY-MEMBER: DRKI0440                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0440                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         25.05.2022 / 13:31:23 / T13AC20            
      *****************************************************************         
      *    VERSION                                                              
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * -------------------------------------------------------------*          
      *         !        !    !                           !          *          
      *         !        !    !                           !          *          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *                                                                         
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER                    
      *    FÜR DIE FUNKTION DRKI0440                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0440.                                                         
           05  PI-DRKI0440-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0440-KFZVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI0440.                                                         
           05  PO-DRKI0440-RC                     PIC  X(003).                  
               88 PO-DRKI0440-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0440-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI0440-NI.                                                  
               10 NI-DRKI0440-TAB.                                              
                  15 DRKI0440-ELE                                               
                     PIC S9(04) COMP OCCURS 11 TIMES.                           
               10 NI-DRKI0440 REDEFINES NI-DRKI0440-TAB.                        
                                                                                
ANF   *        %M_PO_DATEN_NI%                                                  
             15 PO-DRKI0440-N-KFZFHRZGTYPD   PIC S9(4) COMP.                    
             15 PO-DRKI0440-NI-KFZGEBDAT-VN   PIC S9(4) COMP.                   
             15 PO-DRKI0440-NI-KFZGESGEW   PIC S9(4) COMP.                      
             15 PO-DRKI0440-NI-KFZKW   PIC S9(4) COMP.                          
             15 PO-DRKI0440-N-KFZLSTNGSRTD   PIC S9(4) COMP.                    
             15 PO-DRKI0440-NI-KFZNUTZ   PIC S9(4) COMP.                        
             15 PO-DRKI0440-NI-KFZPAKET-KNR   PIC S9(4) COMP.                   
             15 PO-DRKI0440-NI-KFZPAKET-KTY   PIC S9(4) COMP.                   
             15 PO-DRKI0440-NI-KFZPLATZ   PIC S9(4) COMP.                       
             15 PO-DRKI0440-NI-KFZSCOREWERT   PIC S9(4) COMP.                   
             15 PO-DRKI0440-NI-KFZTELEMATIKID   PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PO-DRKI0440-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI0440-KFZ-E-FZG-KZ   PIC  X(0001).                        
             10 PO-DRKI0440-KFZABWFAHRLSTJH   PIC S9(7)V COMP-3.                
             10 PO-DRKI0440-KFZAGBRANCHE   PIC  X(0060).                        
             10 PO-DRKI0440-KFZAKZART-KNR   PIC S9(15)V COMP-3.                 
             10 PO-DRKI0440-KFZAKZART-KTY   PIC S9(4) COMP.                     
             10 PO-DRKI0440-KFZALTERSGRP-KNR   PIC S9(15)V COMP-3.              
             10 PO-DRKI0440-KFZALTERSGRP-KTY   PIC S9(4) COMP.                  
             10 PO-DRKI0440-KFZAMTLICHESKENNZ   PIC  X(0012).                   
             10 PO-DRKI0440-KFZAUFBAUART-KNR   PIC S9(15)V COMP-3.              
             10 PO-DRKI0440-KFZAUFBAUART-KTY   PIC S9(4) COMP.                  
             10 PO-DRKI0440-KFZBEGFAHRERSCHL   PIC S9(4) COMP.                  
             10 PO-DRKI0440-KFZBEGFAHRERTEXT   PIC  X(0010).                    
             10 PO-DRKI0440-KFZBEITRANPASSAM   PIC  X(010).                     
             10 PO-DRKI0440-KFZBEITRGESAMALT   PIC S9(5)V9(2) COMP-3.           
             10 PO-DRKI0440-KFZBEITRGESAMNEU   PIC S9(5)V9(2) COMP-3.           
             10 PO-DRKI0440-KFZBERECHNETAM   PIC  X(010).                       
             10 PO-DRKI0440-KFZDATKMSTANDAKT   PIC  X(010).                     
             10 PO-DRKI0440-KFZERSTZULDATUMVN   PIC  X(010).                    
             10 PO-DRKI0440-KFZFAHRER   PIC  X(0010).                           
             10 PO-DRKI0440-KFZFAHRGEBIET   PIC  X(0010).                       
             10 PO-DRKI0440-KFZFAHRGESTELLNR   PIC  X(0020).                    
             10 PO-DRKI0440-KFZFAHRZEUGTYPID   PIC S9(4) COMP.                  
             10 PO-DRKI0440-KFZFALTERMAX-KNR   PIC S9(15)V COMP-3.              
             10 PO-DRKI0440-KFZFALTERMAX-KTY   PIC S9(4) COMP.                  
             10 PO-DRKI0440-KFZFIRMENNR   PIC  X(0006).                         
             10 PO-DRKI0440-KFZFSERWERB   PIC  X(0010).                         
             10 PO-DRKI0440-KFZFZGABSTELLPLATZ   PIC  X(0010).                  
             10 PO-DRKI0440-KFZFZGART-KNR   PIC S9(15)V COMP-3.                 
             10 PO-DRKI0440-KFZFZGART-KTY   PIC S9(4) COMP.                     
             10 PO-DRKI0440-KFZFZGNUTZUNG   PIC  X(0010).                       
             10 PO-DRKI0440-KFZGEBDAT-VN   PIC  X(010).                         
             10 PO-DRKI0440-KFZGEBDATFAHRER   PIC  X(010).                      
             10 PO-DRKI0440-KFZGEBDATFAHRERIN   PIC  X(010).                    
             10 PO-DRKI0440-KFZGESAMTNEUWERTFZ   PIC S9(9)V9(2) COMP-3.         
             10 PO-DRKI0440-KFZGESGEW   PIC S9(7)V COMP-3.                      
             10 PO-DRKI0440-KFZGESNEUWERTWKZ   PIC  X(0001).                    
             10 PO-DRKI0440-KFZHAFTPFLTYPKL   PIC S9(4) COMP.                   
             10 PO-DRKI0440-KFZHALTERNNAME   PIC  X(0040).                      
             10 PO-DRKI0440-KFZHALTERSCHL   PIC S9(4) COMP.                     
             10 PO-DRKI0440-KFZHALTERTEXT   PIC  X(0010).                       
             10 PO-DRKI0440-KFZHALTERVNAME   PIC  X(0040).                      
             10 PO-DRKI0440-KFZHERSTELLERSCHL   PIC  X(0004).                   
             10 PO-DRKI0440-KFZHERSTELLTEXT   PIC  X(0025).                     
             10 PO-DRKI0440-KFZHERSTMODTEXT   PIC  X(0020).                     
             10 PO-DRKI0440-KFZJAEHRLFAHRL   PIC S9(9) COMP.                    
             10 PO-DRKI0440-KFZKMSTAND   PIC S9(9) COMP.                        
             10 PO-DRKI0440-KFZKMSTANDAKT   PIC S9(7)V COMP-3.                  
             10 PO-DRKI0440-KFZKW   PIC S9(4)V COMP-3.                          
             10 PO-DRKI0440-KFZLEASING   PIC S9(4) COMP.                        
             10 PO-DRKI0440-KFZLEISTUNG   PIC S9(5)V9(2) COMP-3.                
             10 PO-DRKI0440-KFZLEISTUNGSARTID   PIC S9(4) COMP.                 
             10 PO-DRKI0440-KFZLEISTUNGTEXT   PIC  X(0012).                     
             10 PO-DRKI0440-KFZLSVSCOR   PIC  X(0001).                          
             10 PO-DRKI0440-KFZNAEHSPEZ-KNR   PIC S9(15)V COMP-3.               
             10 PO-DRKI0440-KFZNAEHSPEZ-KTY   PIC S9(4) COMP.                   
             10 PO-DRKI0440-KFZNUTZ   PIC S9(7)V COMP-3.                        
             10 PO-DRKI0440-KFZPAKET-KNR   PIC S9(15)V COMP-3.                  
             10 PO-DRKI0440-KFZPAKET-KTY   PIC S9(4) COMP.                      
             10 PO-DRKI0440-KFZPLATZ   PIC S9(3)V COMP-3.                       
             10 PO-DRKI0440-KFZPSFANZKH   PIC S9(4) COMP.                       
             10 PO-DRKI0440-KFZPSFANZVK   PIC S9(4) COMP.                       
             10 PO-DRKI0440-KFZSAISONBIS   PIC S9(4) COMP.                      
             10 PO-DRKI0440-KFZSAISONKZ   PIC  X(0001).                         
             10 PO-DRKI0440-KFZSAISONVON   PIC S9(4) COMP.                      
             10 PO-DRKI0440-KFZSCHLUESSELTYP   PIC  X(0003).                    
             10 PO-DRKI0440-KFZSCHUTZBRIEF   PIC  X(0010).                      
             10 PO-DRKI0440-KFZSCOREDATUM   PIC  X(010).                        
             10 PO-DRKI0440-KFZSCOREWERT   PIC S9(3)V COMP-3.                   
             10 PO-DRKI0440-KFZSFANZKH   PIC S9(4) COMP.                        
             10 PO-DRKI0440-KFZSFANZVK   PIC S9(4) COMP.                        
             10 PO-DRKI0440-KFZSFRFREIKH   PIC  X(0001).                        
             10 PO-DRKI0440-KFZSFRFREIVK   PIC  X(0001).                        
             10 PO-DRKI0440-KFZTARIFAUSWAHL   PIC  X(0001).                     
             10 PO-DRKI0440-KFZTELEMATIKID   PIC S9(14)V COMP-3.                
             10 PO-DRKI0440-KFZVERSTARLFDNR   PIC S9(4) COMP.                   
             10 PO-DRKI0440-KFZVERWEND-KNR   PIC S9(15)V COMP-3.                
             10 PO-DRKI0440-KFZVERWEND-KTY   PIC S9(4) COMP.                    
             10 PO-DRKI0440-KFZVOLLKASKOTYPKL   PIC S9(4) COMP.                 
             10 PO-DRKI0440-KFZWERKSTBINDKZ   PIC  X(0001).                     
             10 PO-DRKI0440-KFZWOHNEIGT   PIC  X(0010).                         
             10 PO-DRKI0440-KFZZULASSUNGDATUM   PIC  X(010).                    
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
