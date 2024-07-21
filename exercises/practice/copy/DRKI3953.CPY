      *****************************************************************         
      *    COPY-MEMBER: DRKI3953                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3953                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         13.09.2022 / 14:40:43 / U131774            
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
      *    FÜR DIE FUNKTION DRKI3953                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3953.                                                         
           05  PI-DRKI3953-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3953-GVZVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI3953-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3953-NI-GVZFLATRATE      PIC S9(4) COMP.                 
             15 PI-DRKI3953-NI-GVZKNDRABATT     PIC S9(4) COMP.                 
             15 PI-DRKI3953-NI-GVZKUMAUSLIEFGB  PIC S9(4) COMP.                 
             15 PI-DRKI3953-NI-GVZKUMAUSLIEFGM  PIC S9(4) COMP.                 
             15 PI-DRKI3953-N-GVZKMSLFSBB       PIC S9(4) COMP.                 
             15 PI-DRKI3953-N-GVZKMSLFSBZ       PIC S9(4) COMP.                 
             15 PI-DRKI3953-NI-GVZKUMAUSLIEFSM  PIC S9(4) COMP.                 
             15 PI-DRKI3953-NI-GVZKUNDENIDEXT   PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3953-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3953-GVZFLATRATE   PIC  X(0001).                         
             10 PI-DRKI3953-GVZKNDRABATT   PIC S9(3)V9(2) COMP-3.               
             10 PI-DRKI3953-GVZKUMAUSLIEFGB   PIC S9(9) COMP.                   
             10 PI-DRKI3953-GVZKUMAUSLIEFGM   PIC S9(9) COMP.                   
             10 PI-DRKI3953-GVZKUMAUSLIEFSBB   PIC S9(9) COMP.                  
             10 PI-DRKI3953-GVZKUMAUSLIEFSBZ   PIC S9(9) COMP.                  
             10 PI-DRKI3953-GVZKUMAUSLIEFSM   PIC S9(9) COMP.                   
             10 PI-DRKI3953-GVZKUNDENIDEXT   PIC S9(9) COMP.                    
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3953-NI1.                                                     
           05 PI-DRKI3953-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 8.                                     
       01  PI-DRKI3953-NI7.                                                     
           05 PI-DRKI3953-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 8.                                     
      *                                                                         
       01  PO-DRKI3953.                                                         
           05  PO-DRKI3953-RC                     PIC  X(003).                  
               88 PO-DRKI3953-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3953-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
