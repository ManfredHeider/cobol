      *****************************************************************         
      *    COPY-MEMBER: DRKI3262                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3262                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         06.09.2022 / 14:45:46 / U131774            
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
      *    FÜR DIE FUNKTION DRKI3262                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3262.                                                         
           05  PI-DRKI3262-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3262-G79SPARRATEID   PIC S9(9) COMP.                     
             10 PI-DRKI3262-G79VERTRAGSNR   PIC  X(0015).                       
END   *                                                                         
                                                                                
           05  PI-DRKI3262-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3262-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3262-G79ABBUCHTERMIN   PIC  X(0001).                     
             10 PI-DRKI3262-G79ABSCHLVBNR   PIC S9(9) COMP.                     
             10 PI-DRKI3262-G79ABWNAME   PIC  X(0040).                          
             10 PI-DRKI3262-G79BESTANDEURO   PIC S9(7)V9(2) COMP-3.             
             10 PI-DRKI3262-G79BESTANDGRAMM   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI3262-G79BIC   PIC  X(0011).                              
             10 PI-DRKI3262-G79BUCHUNGSDAT   PIC  X(0008).                      
             10 PI-DRKI3262-G79DYNAMIK   PIC S9(3)V9(2) COMP-3.                 
             10 PI-DRKI3262-G79ENDEAUSDAT   PIC  X(0008).                       
             10 PI-DRKI3262-G79FLATRATE   PIC  X(0001).                         
             10 PI-DRKI3262-G79IBAN   PIC  X(0034).                             
             10 PI-DRKI3262-G79ISIN   PIC  X(0012).                             
             10 PI-DRKI3262-G79KNDRABATT   PIC  X(0001).                        
             10 PI-DRKI3262-G79KUMAUSLIEF1   PIC S9(9) COMP.                    
             10 PI-DRKI3262-G79KUMAUSLIEF2   PIC S9(9) COMP.                    
             10 PI-DRKI3262-G79KUMAUSLIEF3   PIC S9(9) COMP.                    
             10 PI-DRKI3262-G79KUNDENDATEN.                                     
                49 PI-DRKI3262-G79KUNDENDATEN-LEN PIC S9(4) COMP.               
                49 PI-DRKI3262-G79KNDNDTN-TEXT PIC X(382).                      
             10 PI-DRKI3262-G79KUNDENIDEXT   PIC S9(9) COMP.                    
             10 PI-DRKI3262-G79MANDANT   PIC S9(9) COMP.                        
             10 PI-DRKI3262-G79RABATT   PIC  X(0001).                           
             10 PI-DRKI3262-G79RATEOEFFDAT   PIC  X(0008).                      
             10 PI-DRKI3262-G79RATESCHLDAT   PIC  X(0008).                      
             10 PI-DRKI3262-G79RUECKGRAMM   PIC S9(4)V9(2) COMP-3.              
             10 PI-DRKI3262-G79SPARRATE   PIC S9(7)V9(2) COMP-3.                
             10 PI-DRKI3262-G79SPARRATELFDNR   PIC S9(4) COMP.                  
             10 PI-DRKI3262-G79STATUS   PIC  X(0001).                           
             10 PI-DRKI3262-G79SUMMEAUS   PIC S9(7)V9(2) COMP-3.                
             10 PI-DRKI3262-G79SUMMEEIN   PIC S9(7)V9(2) COMP-3.                
             10 PI-DRKI3262-G79VRTRGBEGINN   PIC  X(0008).                      
             10 PI-DRKI3262-G79VRTRGENDE   PIC  X(0008).                        
             10 PI-DRKI3262-G79ZAHLUNGSART   PIC  X(0001).                      
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3262-NI1.                                                     
           05 PI-DRKI3262-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI3262-NI7.                                                     
           05 PI-DRKI3262-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI3262.                                                         
           05  PO-DRKI3262-RC                     PIC  X(003).                  
               88 PO-DRKI3262-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI3262-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
