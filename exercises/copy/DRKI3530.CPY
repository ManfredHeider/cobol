      *****************************************************************         
      *    COPY-MEMBER: DRKI3530                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3530                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         24.08.2022 / 10:44:25 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3530                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3530.                                                         
           05  PI-DRKI3530-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3530-G21VVVNR   PIC  X(0015).                            
END   *                                                                         
                                                                                
           05  PI-DRKI3530-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3530-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3530-G21VVAENDKZN   PIC  X(0001).                        
             10 PI-DRKI3530-G21VVBERECH-DAT   PIC  X(0010).                     
             10 PI-DRKI3530-G21VVERLFALL-SUM   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI3530-G21VVFONDSGUTH   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI3530-G21VVKAPABFIND   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI3530-G21VVLEISTUNG-KZN   PIC  X(0001).                   
             10 PI-DRKI3530-G21VVMANDANT   PIC S9(9) COMP.                      
             10 PI-DRKI3530-G21VVNJBEITRAG   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI3530-G21VVPRODUKT   PIC  X(0050).                        
             10 PI-DRKI3530-G21VVRENTEGARANT   PIC S9(7)V COMP-3.               
             10 PI-DRKI3530-G21VVRENTENBEZUG   PIC  X(0001).                    
             10 PI-DRKI3530-G21VVRENTEVORAUS   PIC S9(7)V COMP-3.               
             10 PI-DRKI3530-G21VVRNTFAKTOR   PIC S9(5)V9(2) COMP-3.             
             10 PI-DRKI3530-G21VVRNTZAHLW   PIC S9(4) COMP.                     
             10 PI-DRKI3530-G21VVSPARTE   PIC  X(0003).                         
             10 PI-DRKI3530-G21VVTARIF   PIC  X(0050).                          
             10 PI-DRKI3530-G21VVTODFALL-SUM   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI3530-G21VVVBNR   PIC S9(9) COMP.                         
             10 PI-DRKI3530-G21VVVERARB-KZN   PIC  X(0001).                     
             10 PI-DRKI3530-G21VVVERTRABLAUF   PIC  X(0010).                    
             10 PI-DRKI3530-G21VVVERTRBEGINN   PIC  X(0010).                    
             10 PI-DRKI3530-G21VVVERTRZUSTAND   PIC S9(4) COMP.                 
             10 PI-DRKI3530-G21VVZAHLBEITRAG   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI3530-G21VVZAHLWEISE   PIC S9(4) COMP.                    
             10 PI-DRKI3530-LETZTERAENDERER   PIC  X(0010).                     
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3530-NI1.                                                     
           05 PI-DRKI3530-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 1.                                     
       01  PI-DRKI3530-NI7.                                                     
           05 PI-DRKI3530-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 1.                                     
      *                                                                         
       01  PO-DRKI3530.                                                         
           05  PO-DRKI3530-RC                     PIC  X(003).                  
               88 PO-DRKI3530-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3530-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
