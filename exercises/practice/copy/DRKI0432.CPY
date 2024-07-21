      *****************************************************************         
      *    COPY-MEMBER: DRKI0432                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0432                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         25.07.2022 / 14:12:57 / U131848            
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
      *    FÜR DIE FUNKTION DRKI0432                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0432.                                                         
           05  PI-DRKI0432-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0432-LFOLFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI0432-LFOVERSTARLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI0432-LFOVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI0432-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI0432-N-LFNZHLNTL         PIC S9(4) COMP.                 
             15 PI-DRKI0432-N-LFDCKNGSKPTL      PIC S9(4) COMP.                 
             15 PI-DRKI0432-NI-LFOPROZSPARBETR  PIC S9(4) COMP.                 
             15 PI-DRKI0432-N-LFRCKNHMBTR       PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI0432-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI0432-LFOANZAHLANTBKZ   PIC  X(0001).                     
             10 PI-DRKI0432-LFOANZAHLANTEILE   PIC S9(8)V9(6) COMP-3.           
             10 PI-DRKI0432-LFODECKKAPITBKZ   PIC  X(0001).                     
             10 PI-DRKI0432-LFODECKKAPITWKZ   PIC  X(0001).                     
             10 PI-DRKI0432-LFODECKUNGSKAPITAL   PIC S9(11)V9(2) COMP-3.        
             10 PI-DRKI0432-LFOFONDSKUEBKZ   PIC  X(0001).                      
             10 PI-DRKI0432-LFOFONDSKUERZEL   PIC  X(0002).                     
             10 PI-DRKI0432-LFOPROZSPARBETR   PIC S9(3)V9(2) COMP-3.            
             10 PI-DRKI0432-LFOPROZSPARBKZ   PIC  X(0001).                      
             10 PI-DRKI0432-LFORUECKNAHMEBETR   PIC S9(11)V9(2) COMP-3.         
             10 PI-DRKI0432-LFORUECKNAHMEBKZ   PIC  X(0001).                    
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI0432-NI1.                                                     
           05 PI-DRKI0432-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 4.                                 
       01  PI-DRKI0432-NI7.                                                     
           05 PI-DRKI0432-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 4.                                 
                                                                                
      *                                                                         
       01  PO-DRKI0432.                                                         
           05  PO-DRKI0432-RC                     PIC  X(003).                  
               88 PO-DRKI0432-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI0432-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
