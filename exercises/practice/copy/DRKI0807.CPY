      *****************************************************************         
      *    COPY-MEMBER: DRKI0807                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0807                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         25.07.2022 / 14:13:01 / U131848            
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
      *    FÜR DIE FUNKTION DRKI0807                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0807.                                                         
           05  PI-DRKI0807-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0807-LFOLFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI0807-LFOVERSTARLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI0807-LFOVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI0807-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI0807-N-LFNZHLNTL         PIC S9(4) COMP.                 
             15 PI-DRKI0807-N-LFDCKNGSKPTL      PIC S9(4) COMP.                 
             15 PI-DRKI0807-NI-LFOPROZSPARBETR  PIC S9(4) COMP.                 
             15 PI-DRKI0807-N-LFRCKNHMBTR       PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI0807-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI0807-LFOANZAHLANTBKZ   PIC  X(0001).                     
             10 PI-DRKI0807-LFOANZAHLANTEILE   PIC S9(8)V9(6) COMP-3.           
             10 PI-DRKI0807-LFODECKKAPITBKZ   PIC  X(0001).                     
             10 PI-DRKI0807-LFODECKKAPITWKZ   PIC  X(0001).                     
             10 PI-DRKI0807-LFODECKUNGSKAPITAL   PIC S9(11)V9(2) COMP-3.        
             10 PI-DRKI0807-LFOFONDSKUEBKZ   PIC  X(0001).                      
             10 PI-DRKI0807-LFOFONDSKUERZEL   PIC  X(0002).                     
             10 PI-DRKI0807-LFOPROZSPARBETR   PIC S9(3)V9(2) COMP-3.            
             10 PI-DRKI0807-LFOPROZSPARBKZ   PIC  X(0001).                      
             10 PI-DRKI0807-LFORUECKNAHMEBETR   PIC S9(11)V9(2) COMP-3.         
             10 PI-DRKI0807-LFORUECKNAHMEBKZ   PIC  X(0001).                    
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI0807-NI1.                                                     
           05 PI-DRKI0807-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 4.                                     
       01  PI-DRKI0807-NI7.                                                     
           05 PI-DRKI0807-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 4.                                     
      *                                                                         
       01  PO-DRKI0807.                                                         
           05  PO-DRKI0807-RC                     PIC  X(003).                  
               88 PO-DRKI0807-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0807-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
