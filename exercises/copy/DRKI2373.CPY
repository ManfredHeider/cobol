      *****************************************************************         
      *    COPY-MEMBER: DRKI2373                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2373                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         19.05.2022 / 14:23:05 / T13EX78            
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
      *    FÜR DIE FUNKTION DRKI2373                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI2373.                                                         
           05  PI-DRKI2373-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI2373-LTAVERSTARLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI2373-LTAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI2373.                                                         
           05  PO-DRKI2373-RC                     PIC  X(003).                  
               88 PO-DRKI2373-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI2373-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI2373-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI2373-LTABULEISTUNG   PIC  X(0001).                       
             10 PO-DRKI2373-LTAKLBERHAUSSCHL   PIC  X(0001).                    
             10 PO-DRKI2373-LTALVART   PIC S9(4) COMP.                          
             10 PO-DRKI2373-LTANETTOBEITRAG   PIC S9(9)V9(2) COMP-3.            
             10 PO-DRKI2373-LTARENTENBEZUG   PIC S9(4) COMP.                    
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
