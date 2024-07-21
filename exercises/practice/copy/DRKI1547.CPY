      *****************************************************************         
      *    COPY-MEMBER: DRKI1547                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1547                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         17.01.2023 / 10:13:04 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI1547                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI1547.                                                         
           05  PI-DRKI1547-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI1547-LTAVERSTARLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI1547-LTAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI1547.                                                         
           05  PO-DRKI1547-RC                     PIC  X(003).                  
               88 PO-DRKI1547-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI1547-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI1547-NI.                                                  
               10 NI-DRKI1547-TAB.                                              
                  15 DRKI1547-ELE                                               
                     PIC S9(04) COMP OCCURS 31 TIMES.                           
               10 NI-DRKI1547 REDEFINES NI-DRKI1547-TAB.                        
                                                                                
ANF   *        %M_PO_DATEN_NI%                                                  
             15 PO-DRKI1547-NI-LTAABTRETUNGKZ   PIC S9(4) COMP.                 
             15 PO-DRKI1547-N-LTBSSZNS-BTR   PIC S9(4) COMP.                    
             15 PO-DRKI1547-NI-LTABONUSKZ   PIC S9(4) COMP.                     
             15 PO-DRKI1547-NI-LTABULEISTUNG   PIC S9(4) COMP.                  
             15 PO-DRKI1547-NI-LTADAUZULAGENKZ   PIC S9(4) COMP.                
             15 PO-DRKI1547-NI-LTADEPOTBETRAG   PIC S9(4) COMP.                 
             15 PO-DRKI1547-NI-LTADEPOTZINSEN   PIC S9(4) COMP.                 
             15 PO-DRKI1547-N-LTDPTZNSSTZ   PIC S9(4) COMP.                     
             15 PO-DRKI1547-N-LTDPWRTTRMN   PIC S9(4) COMP.                     
             15 PO-DRKI1547-N-LTDRKTVR-KNR   PIC S9(4) COMP.                    
             15 PO-DRKI1547-N-LTDRKTVR-KTY   PIC S9(4) COMP.                    
             15 PO-DRKI1547-NI-LTAFOMGNTKWAB   PIC S9(4) COMP.                  
             15 PO-DRKI1547-NI-LTAFOMGNTKWBIS   PIC S9(4) COMP.                 
             15 PO-DRKI1547-NI-LTAFONDSAKTKZN   PIC S9(4) COMP.                 
             15 PO-DRKI1547-NI-LTAFONDSPALETTE   PIC S9(4) COMP.                
             15 PO-DRKI1547-NI-LTAKONVANLPROZ   PIC S9(4) COMP.                 
             15 PO-DRKI1547-NI-LTALVART   PIC S9(4) COMP.                       
             15 PO-DRKI1547-NI-LTALVURSPRBEG   PIC S9(4) COMP.                  
             15 PO-DRKI1547-NI-LTANETTOBEITRAG   PIC S9(4) COMP.                
             15 PO-DRKI1547-NI-LTAPRODBEZ-KNR   PIC S9(4) COMP.                 
             15 PO-DRKI1547-NI-LTAPRODBEZ-KTY   PIC S9(4) COMP.                 
             15 PO-DRKI1547-NI-LTARENTENBEZUG   PIC S9(4) COMP.                 
             15 PO-DRKI1547-N-LTRNTNGRZT   PIC S9(4) COMP.                      
             15 PO-DRKI1547-N-LTRNTNZHLWD   PIC S9(4) COMP.                     
             15 PO-DRKI1547-NI-LTARUMSGRUPPE   PIC S9(4) COMP.                  
             15 PO-DRKI1547-NI-LTASTVD0067-KNR   PIC S9(4) COMP.                
             15 PO-DRKI1547-NI-LTASTVD0067-KTY   PIC S9(4) COMP.                
             15 PO-DRKI1547-NI-LTAURSPRPOLDAT   PIC S9(4) COMP.                 
             15 PO-DRKI1547-N-LTZNSBST-BTR   PIC S9(4) COMP.                    
             15 PO-DRKI1547-N-LTZNSBST-STZ   PIC S9(4) COMP.                    
             15 PO-DRKI1547-NI-LTAZULAGENART   PIC S9(4) COMP.                  
END   *                                                                         
                                                                                
           05  PO-DRKI1547-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI1547-LTAABTRETUNGKZ   PIC  X(0001).                      
             10 PO-DRKI1547-LTABASISZINS-BETR   PIC S9(13)V9(2) COMP-3.         
             10 PO-DRKI1547-LTABONUSKZ   PIC  X(0001).                          
             10 PO-DRKI1547-LTABTG-AG   PIC S9(11)V9(2) COMP-3.                 
             10 PO-DRKI1547-LTABTG-AN   PIC S9(11)V9(2) COMP-3.                 
             10 PO-DRKI1547-LTABUKTKZN   PIC  X(0001).                          
             10 PO-DRKI1547-LTABULEISTUNG   PIC  X(0001).                       
             10 PO-DRKI1547-LTADAUZULAGENKZ   PIC  X(0001).                     
             10 PO-DRKI1547-LTADEPOTBETRAG   PIC S9(11)V9(2) COMP-3.            
             10 PO-DRKI1547-LTADEPOTZINSEN   PIC S9(11)V9(2) COMP-3.            
             10 PO-DRKI1547-LTADEPOTZINSSATZ   PIC S9(3)V9(2) COMP-3.           
             10 PO-DRKI1547-LTADEPWERTTERMIN   PIC  X(010).                     
             10 PO-DRKI1547-LTADIREKTVER-KNR   PIC S9(4) COMP.                  
             10 PO-DRKI1547-LTADIREKTVER-KTY   PIC S9(4) COMP.                  
             10 PO-DRKI1547-LTAEUKLAUSELKZN   PIC  X(0001).                     
             10 PO-DRKI1547-LTAFOMGNTKWAB   PIC  X(010).                        
             10 PO-DRKI1547-LTAFOMGNTKWBIS   PIC  X(010).                       
             10 PO-DRKI1547-LTAFONDSAKTKZN   PIC  X(0001).                      
             10 PO-DRKI1547-LTAFONDSMANKZ   PIC  X(0001).                       
             10 PO-DRKI1547-LTAFONDSPALETTE   PIC  X(0008).                     
             10 PO-DRKI1547-LTAKLBERHAUSSCHL   PIC  X(0001).                    
             10 PO-DRKI1547-LTAKONVANLPROZ   PIC S9(3)V COMP-3.                 
             10 PO-DRKI1547-LTAKZKAPWRECHT   PIC  X(0001).                      
             10 PO-DRKI1547-LTALVART   PIC S9(4) COMP.                          
             10 PO-DRKI1547-LTALVURSPRBEG   PIC  X(010).                        
             10 PO-DRKI1547-LTANETTOBEITRAG   PIC S9(9)V9(2) COMP-3.            
             10 PO-DRKI1547-LTANEUABSCHLKZN   PIC  X(0001).                     
             10 PO-DRKI1547-LTAPRODBEZ-KNR   PIC S9(15)V COMP-3.                
             10 PO-DRKI1547-LTAPRODBEZ-KTY   PIC S9(4) COMP.                    
             10 PO-DRKI1547-LTARENTENBEZUG   PIC S9(4) COMP.                    
             10 PO-DRKI1547-LTARENTENGARZEIT   PIC  X(010).                     
             10 PO-DRKI1547-LTARENTENZAHLWID   PIC S9(4) COMP.                  
             10 PO-DRKI1547-LTARUMSGRUPPE   PIC  X(0006).                       
             10 PO-DRKI1547-LTASCHUELER-BU   PIC  X(0001).                      
             10 PO-DRKI1547-LTASTVD0067-KNR   PIC S9(15)V COMP-3.               
             10 PO-DRKI1547-LTASTVD0067-KTY   PIC S9(4) COMP.                   
             10 PO-DRKI1547-LTAURSPRPOLDAT   PIC  X(010).                       
             10 PO-DRKI1547-LTAVITALITYKZN-KNR   PIC S9(15)V COMP-3.            
             10 PO-DRKI1547-LTAVITALITYKZN-KTY   PIC S9(4) COMP.                
             10 PO-DRKI1547-LTAWERTSICHOPTKZN   PIC  X(0001).                   
             10 PO-DRKI1547-LTAZINSBOOST-BETR   PIC S9(13)V9(2) COMP-3.         
             10 PO-DRKI1547-LTAZINSBOOST-SATZ   PIC S9(6)V9(3) COMP-3.          
             10 PO-DRKI1547-LTAZULAGENART   PIC S9(4) COMP.                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
