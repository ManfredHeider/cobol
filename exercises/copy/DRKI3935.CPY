      *****************************************************************         
      *    COPY-MEMBER: DRKI3935                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3935                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         08.12.2021 / 12:11:20 / U131382            
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
      *    FÜR DIE FUNKTION DRKI3935                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3935.                                                         
           05  PI-DRKI3935-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3935-KPEPERSONLFDNR   PIC S9(4) COMP.                    
             10 PI-DRKI3935-KPEVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI3935-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3935-N-KPLTRCKSTLLG      PIC S9(4) COMP.                 
             15 PI-DRKI3935-N-KPBTRGSVRTL       PIC S9(4) COMP.                 
             15 PI-DRKI3935-NI-KPESTAND         PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3935-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3935-KPEALTRUECKSTELLG   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3935-KPEBEITRAGSVORTEIL   PIC S9(7)V9(2) COMP-3.         
             10 PI-DRKI3935-KPESTAND   PIC  X(010).                             
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3935-NI1.                                                     
           05 PI-DRKI3935-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 3.                                     
       01  PI-DRKI3935-NI7.                                                     
           05 PI-DRKI3935-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 3.                                     
      *                                                                         
       01  PO-DRKI3935.                                                         
           05  PO-DRKI3935-RC                     PIC  X(003).                  
               88 PO-DRKI3935-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3935-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
