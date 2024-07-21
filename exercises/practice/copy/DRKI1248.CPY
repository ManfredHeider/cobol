      *****************************************************************         
      *    COPY-MEMBER: DRKI1248                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1248                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         19.09.2022 / 17:03:30 / U131774            
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
      *    FÜR DIE FUNKTION DRKI1248                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI1248.                                                         
           05  PI-DRKI1248-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI1248-GHBBUCHUNGLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI1248-GHBFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI1248-GHBGHALFDNR   PIC S9(4) COMP.                       
             10 PI-DRKI1248-GHBGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI1248-GHBVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI1248-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI1248-N-GHBBTRGLTZTBCH    PIC S9(4) COMP.                 
             15 PI-DRKI1248-NI-GHBGUTHABENANT   PIC S9(4) COMP.                 
             15 PI-DRKI1248-N-GHBKZFNHLTZTBD    PIC S9(4) COMP.                 
             15 PI-DRKI1248-N-GHBSTBTRLTZTB     PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI1248-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI1248-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3.         
             10 PI-DRKI1248-GHBBETRLBUWKZ   PIC  X(0001).                       
             10 PI-DRKI1248-GHBBUCHUNGSDATUM   PIC  X(010).                     
             10 PI-DRKI1248-GHBGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PI-DRKI1248-GHBKZFINHLETZTBUID   PIC S9(4) COMP.                
             10 PI-DRKI1248-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3.         
             10 PI-DRKI1248-GHBSTLETZTBUWKZ   PIC  X(0001).                     
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI1248-NI1.                                                     
           05 PI-DRKI1248-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 4.                                     
       01  PI-DRKI1248-NI7.                                                     
           05 PI-DRKI1248-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 4.                                     
      *                                                                         
       01  PO-DRKI1248.                                                         
           05  PO-DRKI1248-RC                     PIC  X(003).                  
               88 PO-DRKI1248-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI1248-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
