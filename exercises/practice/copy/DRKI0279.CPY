      *****************************************************************         
      *    COPY-MEMBER: DRKI0279                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0279                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         19.09.2022 / 17:03:26 / U131774            
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
      *    FÜR DIE FUNKTION DRKI0279                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0279.                                                         
           05  PI-DRKI0279-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0279-GHBBUCHUNGLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI0279-GHBFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI0279-GHBGHALFDNR   PIC S9(4) COMP.                       
             10 PI-DRKI0279-GHBGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI0279-GHBVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI0279-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI0279-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI0279-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3.         
             10 PI-DRKI0279-GHBBETRLBUWKZ   PIC  X(0001).                       
             10 PI-DRKI0279-GHBBUCHUNGSDATUM   PIC  X(010).                     
             10 PI-DRKI0279-GHBGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PI-DRKI0279-GHBKZFINHLETZTBUID   PIC S9(4) COMP.                
             10 PI-DRKI0279-GHBNETTOBETRAG   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI0279-GHBNETTOBETWKZ   PIC  X(0001).                      
             10 PI-DRKI0279-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3.         
             10 PI-DRKI0279-GHBSTLETZTBUWKZ   PIC  X(0001).                     
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI0279-NI1.                                                     
           05 PI-DRKI0279-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 1.                                     
       01  PI-DRKI0279-NI7.                                                     
           05 PI-DRKI0279-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 1.                                     
      *                                                                         
       01  PO-DRKI0279.                                                         
           05  PO-DRKI0279-RC                     PIC  X(003).                  
               88 PO-DRKI0279-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0279-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
