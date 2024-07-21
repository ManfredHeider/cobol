      *****************************************************************         
      *    COPY-MEMBER: DRKI0370                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0370                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         17.01.2023 / 10:12:59 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI0370                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0370.                                                         
           05  PI-DRKI0370-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0370-LTAVERSTARLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI0370-LTAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI0370-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI0370-NI-LTAABTRETUNGKZ   PIC S9(4) COMP.                 
             15 PI-DRKI0370-N-LTBSSZNS-BTR      PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTABONUSKZ       PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTABTG-AG        PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTABTG-AN        PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTABUKTKZN       PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTABULEISTUNG    PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTADAUZULAGENKZ  PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTADEPOTBETRAG   PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTADEPOTZINSEN   PIC S9(4) COMP.                 
             15 PI-DRKI0370-N-LTDPTZNSSTZ       PIC S9(4) COMP.                 
             15 PI-DRKI0370-N-LTDPWRTTRMN       PIC S9(4) COMP.                 
             15 PI-DRKI0370-N-LTDRKTVR-KNR      PIC S9(4) COMP.                 
             15 PI-DRKI0370-N-LTDRKTVR-KTY      PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTAEUKLAUSELKZN  PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTAFOMGNTKWAB    PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTAFOMGNTKWBIS   PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTAFONDSAKTKZN   PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTAFONDSMANKZ    PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTAFONDSPALETTE  PIC S9(4) COMP.                 
             15 PI-DRKI0370-N-LTKLBRHSSCHL      PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTAKONVANLPROZ   PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTAKZKAPWRECHT   PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTALVART         PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTALVURSPRBEG    PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTANETTOBEITRAG  PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTANEUABSCHLKZN  PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTAPRODBEZ-KNR   PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTAPRODBEZ-KTY   PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTARENTENBEZUG   PIC S9(4) COMP.                 
             15 PI-DRKI0370-N-LTRNTNGRZT        PIC S9(4) COMP.                 
             15 PI-DRKI0370-N-LTRNTNZHLWD       PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTARUMSGRUPPE    PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTASCHUELER-BU   PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTASTVD0067-KNR  PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTASTVD0067-KTY  PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTAURSPRPOLDAT   PIC S9(4) COMP.                 
             15 PI-DRKI0370-N-LTVTLTYKZN-KNR    PIC S9(4) COMP.                 
             15 PI-DRKI0370-N-LTVTLTYKZN-KTY    PIC S9(4) COMP.                 
             15 PI-DRKI0370-N-LTWRTSCHPTKZN     PIC S9(4) COMP.                 
             15 PI-DRKI0370-N-LTZNSBST-BTR      PIC S9(4) COMP.                 
             15 PI-DRKI0370-N-LTZNSBST-STZ      PIC S9(4) COMP.                 
             15 PI-DRKI0370-NI-LTAZULAGENART    PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI0370-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI0370-LTAABTRETUNGKZ   PIC  X(0001).                      
             10 PI-DRKI0370-LTABASISZINS-BETR   PIC S9(13)V9(2) COMP-3.         
             10 PI-DRKI0370-LTABONUSKZ   PIC  X(0001).                          
             10 PI-DRKI0370-LTABTG-AG   PIC S9(11)V9(2) COMP-3.                 
             10 PI-DRKI0370-LTABTG-AN   PIC S9(11)V9(2) COMP-3.                 
             10 PI-DRKI0370-LTABUKTKZN   PIC  X(0001).                          
             10 PI-DRKI0370-LTABULEISTUNG   PIC  X(0001).                       
             10 PI-DRKI0370-LTADAUZULAGENKZ   PIC  X(0001).                     
             10 PI-DRKI0370-LTADEPOTBETRAG   PIC S9(11)V9(2) COMP-3.            
             10 PI-DRKI0370-LTADEPOTZINSEN   PIC S9(11)V9(2) COMP-3.            
             10 PI-DRKI0370-LTADEPOTZINSSATZ   PIC S9(3)V9(2) COMP-3.           
             10 PI-DRKI0370-LTADEPWERTTERMIN   PIC  X(010).                     
             10 PI-DRKI0370-LTADIREKTVER-KNR   PIC S9(4) COMP.                  
             10 PI-DRKI0370-LTADIREKTVER-KTY   PIC S9(4) COMP.                  
             10 PI-DRKI0370-LTAEUKLAUSELKZN   PIC  X(0001).                     
             10 PI-DRKI0370-LTAFOMGNTKWAB   PIC  X(010).                        
             10 PI-DRKI0370-LTAFOMGNTKWBIS   PIC  X(010).                       
             10 PI-DRKI0370-LTAFONDSAKTKZN   PIC  X(0001).                      
             10 PI-DRKI0370-LTAFONDSMANKZ   PIC  X(0001).                       
             10 PI-DRKI0370-LTAFONDSPALETTE   PIC  X(0008).                     
             10 PI-DRKI0370-LTAKLBERHAUSSCHL   PIC  X(0001).                    
             10 PI-DRKI0370-LTAKONVANLPROZ   PIC S9(3)V COMP-3.                 
             10 PI-DRKI0370-LTAKZKAPWRECHT   PIC  X(0001).                      
             10 PI-DRKI0370-LTALVART   PIC S9(4) COMP.                          
             10 PI-DRKI0370-LTALVURSPRBEG   PIC  X(010).                        
             10 PI-DRKI0370-LTANETTOBEITRAG   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI0370-LTANEUABSCHLKZN   PIC  X(0001).                     
             10 PI-DRKI0370-LTAPRODBEZ-KNR   PIC S9(15)V COMP-3.                
             10 PI-DRKI0370-LTAPRODBEZ-KTY   PIC S9(4) COMP.                    
             10 PI-DRKI0370-LTARENTENBEZUG   PIC S9(4) COMP.                    
             10 PI-DRKI0370-LTARENTENGARZEIT   PIC  X(010).                     
             10 PI-DRKI0370-LTARENTENZAHLWID   PIC S9(4) COMP.                  
             10 PI-DRKI0370-LTARUMSGRUPPE   PIC  X(0006).                       
             10 PI-DRKI0370-LTASCHUELER-BU   PIC  X(0001).                      
             10 PI-DRKI0370-LTASTVD0067-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI0370-LTASTVD0067-KTY   PIC S9(4) COMP.                   
             10 PI-DRKI0370-LTAURSPRPOLDAT   PIC  X(010).                       
             10 PI-DRKI0370-LTAVITALITYKZN-KNR   PIC S9(15)V COMP-3.            
             10 PI-DRKI0370-LTAVITALITYKZN-KTY   PIC S9(4) COMP.                
             10 PI-DRKI0370-LTAWERTSICHOPTKZN   PIC  X(0001).                   
             10 PI-DRKI0370-LTAZINSBOOST-BETR   PIC S9(13)V9(2) COMP-3.         
             10 PI-DRKI0370-LTAZINSBOOST-SATZ   PIC S9(6)V9(3) COMP-3.          
             10 PI-DRKI0370-LTAZULAGENART   PIC S9(4) COMP.                     
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI0370-NI1.                                                     
           05 PI-DRKI0370-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 43.                                    
       01  PI-DRKI0370-NI7.                                                     
           05 PI-DRKI0370-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 43.                                    
      *                                                                         
       01  PO-DRKI0370.                                                         
           05  PO-DRKI0370-RC                     PIC  X(003).                  
               88 PO-DRKI0370-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0370-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
