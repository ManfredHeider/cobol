      *****************************************************************         
      *    COPY-MEMBER: DRKI1273                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1273                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         19.09.2022 / 17:03:32 / U131774            
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
      *    FÜR DIE FUNKTION DRKI1273                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI1273.                                                         
           05  PI-DRKI1273-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI1273-GHBBUCHUNGLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI1273-GHBFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI1273-GHBGHALFDNR   PIC S9(4) COMP.                       
             10 PI-DRKI1273-GHBGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI1273-GHBVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI1273.                                                         
           05  PO-DRKI1273-RC                     PIC  X(003).                  
               88 PO-DRKI1273-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI1273-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI1273-NI.                                                  
               10 NI-DRKI1273-TAB.                                              
                  15 DRKI1273-ELE                                               
                     PIC S9(04) COMP OCCURS 4 TIMES.                            
               10 NI-DRKI1273 REDEFINES NI-DRKI1273-TAB.                        
                                                                                
ANF   *        %M_PO_DATEN_NI%                                                  
             15 PO-DRKI1273-N-GHBBTRGLTZTBCH   PIC S9(4) COMP.                  
             15 PO-DRKI1273-NI-GHBGUTHABENANT   PIC S9(4) COMP.                 
             15 PO-DRKI1273-N-GHBKZFNHLTZTBD   PIC S9(4) COMP.                  
             15 PO-DRKI1273-N-GHBSTBTRLTZTB   PIC S9(4) COMP.                   
END   *                                                                         
                                                                                
           05  PO-DRKI1273-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI1273-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3.         
             10 PO-DRKI1273-GHBBETRLBUWKZ   PIC  X(0001).                       
             10 PO-DRKI1273-GHBBUCHUNGSDATUM   PIC  X(010).                     
             10 PO-DRKI1273-GHBGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PO-DRKI1273-GHBKZFINHLETZTBUID   PIC S9(4) COMP.                
             10 PO-DRKI1273-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3.         
             10 PO-DRKI1273-GHBSTLETZTBUWKZ   PIC  X(0001).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
