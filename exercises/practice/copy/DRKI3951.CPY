      *****************************************************************         
      *    COPY-MEMBER: DRKI3951                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3951                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         13.09.2022 / 14:40:40 / U131774            
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
      *    FÜR DIE FUNKTION DRKI3951                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3951.                                                         
           05  PI-DRKI3951-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3951-GVZVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3951.                                                         
           05  PO-DRKI3951-RC                     PIC  X(003).                  
               88 PO-DRKI3951-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3951-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI3951-NI.                                                  
               10 NI-DRKI3951-TAB.                                              
                  15 DRKI3951-ELE                                               
                     PIC S9(04) COMP OCCURS 8 TIMES.                            
               10 NI-DRKI3951 REDEFINES NI-DRKI3951-TAB.                        
                                                                                
ANF   *        %M_PO_DATEN_NI%                                                  
             15 PO-DRKI3951-NI-GVZFLATRATE   PIC S9(4) COMP.                    
             15 PO-DRKI3951-NI-GVZKNDRABATT   PIC S9(4) COMP.                   
             15 PO-DRKI3951-NI-GVZKUMAUSLIEFGB   PIC S9(4) COMP.                
             15 PO-DRKI3951-NI-GVZKUMAUSLIEFGM   PIC S9(4) COMP.                
             15 PO-DRKI3951-N-GVZKMSLFSBB   PIC S9(4) COMP.                     
             15 PO-DRKI3951-N-GVZKMSLFSBZ   PIC S9(4) COMP.                     
             15 PO-DRKI3951-NI-GVZKUMAUSLIEFSM   PIC S9(4) COMP.                
             15 PO-DRKI3951-NI-GVZKUNDENIDEXT   PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PO-DRKI3951-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3951-GVZFLATRATE   PIC  X(0001).                         
             10 PO-DRKI3951-GVZKNDRABATT   PIC S9(3)V9(2) COMP-3.               
             10 PO-DRKI3951-GVZKUMAUSLIEFGB   PIC S9(9) COMP.                   
             10 PO-DRKI3951-GVZKUMAUSLIEFGM   PIC S9(9) COMP.                   
             10 PO-DRKI3951-GVZKUMAUSLIEFSBB   PIC S9(9) COMP.                  
             10 PO-DRKI3951-GVZKUMAUSLIEFSBZ   PIC S9(9) COMP.                  
             10 PO-DRKI3951-GVZKUMAUSLIEFSM   PIC S9(9) COMP.                   
             10 PO-DRKI3951-GVZKUNDENIDEXT   PIC S9(9) COMP.                    
             10 PO-DRKI3951-LETZTEAENDERUNG   PIC  X(0026).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
