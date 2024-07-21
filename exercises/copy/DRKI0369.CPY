      *****************************************************************         
      *    COPY-MEMBER: DRKI0369                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0369                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         17.01.2023 / 10:12:57 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI0369                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0369.                                                         
           05  PI-DRKI0369-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0369-LTAVERSTARLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI0369-LTAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI0369-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI0369-N-LTBSSZNS-BTR      PIC S9(4) COMP.                 
             15 PI-DRKI0369-N-LTDRKTVR-KNR      PIC S9(4) COMP.                 
             15 PI-DRKI0369-N-LTDRKTVR-KTY      PIC S9(4) COMP.                 
             15 PI-DRKI0369-NI-LTAKONVANLPROZ   PIC S9(4) COMP.                 
             15 PI-DRKI0369-NI-LTALVART         PIC S9(4) COMP.                 
             15 PI-DRKI0369-NI-LTANETTOBEITRAG  PIC S9(4) COMP.                 
             15 PI-DRKI0369-NI-LTAPRODBEZ-KNR   PIC S9(4) COMP.                 
             15 PI-DRKI0369-NI-LTAPRODBEZ-KTY   PIC S9(4) COMP.                 
             15 PI-DRKI0369-NI-LTARENTENBEZUG   PIC S9(4) COMP.                 
             15 PI-DRKI0369-NI-LTASTVD0067-KNR  PIC S9(4) COMP.                 
             15 PI-DRKI0369-NI-LTASTVD0067-KTY  PIC S9(4) COMP.                 
             15 PI-DRKI0369-N-LTVTLTYKZN-KNR    PIC S9(4) COMP.                 
             15 PI-DRKI0369-N-LTVTLTYKZN-KTY    PIC S9(4) COMP.                 
             15 PI-DRKI0369-N-LTZNSBST-BTR      PIC S9(4) COMP.                 
             15 PI-DRKI0369-N-LTZNSBST-STZ      PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI0369-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI0369-LTAABTRETUNGKZ   PIC  X(0001).                      
             10 PI-DRKI0369-LTABASISZINS-BETR   PIC S9(13)V9(2) COMP-3.         
             10 PI-DRKI0369-LTABONUSKZ   PIC  X(0001).                          
             10 PI-DRKI0369-LTABTG-AG   PIC S9(11)V9(2) COMP-3.                 
             10 PI-DRKI0369-LTABTG-AN   PIC S9(11)V9(2) COMP-3.                 
             10 PI-DRKI0369-LTABUKTKZN   PIC  X(0001).                          
             10 PI-DRKI0369-LTABULEISTUNG   PIC  X(0001).                       
             10 PI-DRKI0369-LTADAUZULAGENKZ   PIC  X(0001).                     
             10 PI-DRKI0369-LTADEPOTBETRAG   PIC S9(11)V9(2) COMP-3.            
             10 PI-DRKI0369-LTADEPOTZINSEN   PIC S9(11)V9(2) COMP-3.            
             10 PI-DRKI0369-LTADEPOTZINSSATZ   PIC S9(3)V9(2) COMP-3.           
             10 PI-DRKI0369-LTADEPWERTTERMIN   PIC  X(010).                     
             10 PI-DRKI0369-LTADIREKTVER-KNR   PIC S9(4) COMP.                  
             10 PI-DRKI0369-LTADIREKTVER-KTY   PIC S9(4) COMP.                  
             10 PI-DRKI0369-LTAEUKLAUSELKZN   PIC  X(0001).                     
             10 PI-DRKI0369-LTAFOMGNTKWAB   PIC  X(010).                        
             10 PI-DRKI0369-LTAFOMGNTKWBIS   PIC  X(010).                       
             10 PI-DRKI0369-LTAFONDSAKTKZN   PIC  X(0001).                      
             10 PI-DRKI0369-LTAFONDSMANKZ   PIC  X(0001).                       
             10 PI-DRKI0369-LTAFONDSPALETTE   PIC  X(0008).                     
             10 PI-DRKI0369-LTAKLBERHAUSSCHL   PIC  X(0001).                    
             10 PI-DRKI0369-LTAKONVANLPROZ   PIC S9(3)V COMP-3.                 
             10 PI-DRKI0369-LTAKZKAPWRECHT   PIC  X(0001).                      
             10 PI-DRKI0369-LTALVART   PIC S9(4) COMP.                          
             10 PI-DRKI0369-LTALVURSPRBEG   PIC  X(010).                        
             10 PI-DRKI0369-LTANETTOBEITRAG   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI0369-LTANEUABSCHLKZN   PIC  X(0001).                     
             10 PI-DRKI0369-LTAPRODBEZ-KNR   PIC S9(15)V COMP-3.                
             10 PI-DRKI0369-LTAPRODBEZ-KTY   PIC S9(4) COMP.                    
             10 PI-DRKI0369-LTARENTENBEZUG   PIC S9(4) COMP.                    
             10 PI-DRKI0369-LTARENTENGARZEIT   PIC  X(010).                     
             10 PI-DRKI0369-LTARENTENZAHLWID   PIC S9(4) COMP.                  
             10 PI-DRKI0369-LTARUMSGRUPPE   PIC  X(0006).                       
             10 PI-DRKI0369-LTASCHUELER-BU   PIC  X(0001).                      
             10 PI-DRKI0369-LTASTVD0067-KNR   PIC S9(15)V COMP-3.               
             10 PI-DRKI0369-LTASTVD0067-KTY   PIC S9(4) COMP.                   
             10 PI-DRKI0369-LTAURSPRPOLDAT   PIC  X(010).                       
             10 PI-DRKI0369-LTAVITALITYKZN-KNR   PIC S9(15)V COMP-3.            
             10 PI-DRKI0369-LTAVITALITYKZN-KTY   PIC S9(4) COMP.                
             10 PI-DRKI0369-LTAWERTSICHOPTKZN   PIC  X(0001).                   
             10 PI-DRKI0369-LTAZINSBOOST-BETR   PIC S9(13)V9(2) COMP-3.         
             10 PI-DRKI0369-LTAZINSBOOST-SATZ   PIC S9(6)V9(3) COMP-3.          
             10 PI-DRKI0369-LTAZULAGENART   PIC S9(4) COMP.                     
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI0369-NI1.                                                     
           05 PI-DRKI0369-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 15.                                
       01  PI-DRKI0369-NI7.                                                     
           05 PI-DRKI0369-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 15.                                
                                                                                
      *                                                                         
       01  PO-DRKI0369.                                                         
           05  PO-DRKI0369-RC                     PIC  X(003).                  
               88 PO-DRKI0369-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI0369-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
