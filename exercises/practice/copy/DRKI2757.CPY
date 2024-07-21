      *****************************************************************         
      *    COPY-MEMBER: DRKI2757                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2757                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         15.11.2021 / 15:47:29 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI2757                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  NEXT VALUE<<<                                           
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI2757.                                                         
           05  PI-DRKI2757-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI2757-VZAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI2757-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
                                                                                
           05  PI-DRKI2757-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI2757-VZAANZMONVERARB   PIC S9(4) COMP.                   
             10 PI-DRKI2757-VZABEGINN   PIC  X(010).                            
             10 PI-DRKI2757-VZADAUER   PIC S9(4) COMP.                          
             10 PI-DRKI2757-VZAERSTELLTDAT   PIC  X(010).                       
             10 PI-DRKI2757-VZAKLBERHAUSSCHL   PIC  X(0001).                    
             10 PI-DRKI2757-VZALFDMONRECHPGM   PIC S9(4) COMP.                  
             10 PI-DRKI2757-VZATSCHICHTNR   PIC S9(4) COMP.                     
             10 PI-DRKI2757-VZAZAHLBETRAG   PIC S9(11)V9(2) COMP-3.             
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI2757-NI1.                                                     
           05 PI-DRKI2757-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI2757-NI7.                                                     
           05 PI-DRKI2757-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI2757.                                                         
           05  PO-DRKI2757-RC                     PIC  X(003).                  
               88 PO-DRKI2757-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI2757-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
           05  PO-DRKI2757-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI2757-VZALFDNR   PIC S9(4) COMP.                          
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
