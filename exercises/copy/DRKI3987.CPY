      *****************************************************************         
      *    COPY-MEMBER: DRKI3987                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3987                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         02.02.2023 / 13:50:21 / U130685            
      *****************************************************************         
      *    VERSION                                                              
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  ! AENDERUNG                     !NAME       *     
      * -------------------------------------------------------------*          
      *DBO-10842!02.02.23! NEUERSTELLUNG                 ! ATAR      *     
      *         !        ! Besonderheit Sparrateid       !           *
      *         !        ! auskommentiert                !           *
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *                                                                         
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER                    
      *    FÜR DIE FUNKTION DRKI3987                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                        
       01  PI-DRKI3987.                                        
           05  PI-DRKI3987-KEY.                                
                                                               
                                                               
ANF   *        %M_PI_KEY%                                      
      *      10 PI-DRKI3987-G79GRSPARRATEID   PIC S9(9) COMP.  
             10 PI-DRKI3987-G79GRVNR   PIC  X(0015).           
END   *                                                        
                                                               
           05  PI-DRKI3987-NI.                                 
                                                               
ANF   *        %M_PI_DATEN_NI%                                 
             15 FILLER                          PIC S9(4) COMP.
END   *                                                        
                                                               
           05  PI-DRKI3987-DATEN.                              
                                                               
ANF   *        %M_PI_DATEN%                                    
             10 PI-DRKI3987-G79GRMANDANT   PIC S9(9) COMP.     
             10 PI-DRKI3987-LETZTERAENDERER   PIC  X(0010).    
END   *                                                        
                                                               
      *  INITIALISIERUNG NULL-INDIKATOR                        
       01  PI-DRKI3987-NI1.                                    
           05 PI-DRKI3987-INIT-TAB1 PIC S9(04) COMP VALUE -1   
                                  OCCURS 1.                    
       01  PI-DRKI3987-NI7.                                    
           05 PI-DRKI3987-INIT-TAB7 PIC S9(04) COMP VALUE -7   
                                  OCCURS 1.                    
      *                                                        
       01  PO-DRKI3987.                                        
           05  PO-DRKI3987-RC                     PIC  X(003). 
               88 PO-DRKI3987-RC-OK               VALUE 'OK '. 
               88 PO-DRKI3987-RC-NOT-FOUND        VALUE 'NFD'. 
      *                                                          
      ******* ENDE COPY-MEMBER ***************************************          
