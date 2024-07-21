      *****************************************************************         
      *    COPY-MEMBER: DRKI3873                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3873                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.10.2022 / 10:42:32 / U130684            
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
      *    FÜR DIE FUNKTION DRKI3873                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3873.                                                         
           05  PI-DRKI3873-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3873-BDKKARTENNR   PIC  X(0013).                         
             10 PI-DRKI3873-BDKLFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI3873-BDKVERTRAGSID   PIC  X(0013).                       
             10 PI-DRKI3873-BDKWAEHRUNG   PIC S9(4) COMP.                       
END   *                                                                         
                                                                                
           05  PI-DRKI3873-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3873-NI-BDKGUELTIGBIS    PIC S9(4) COMP.                 
             15 PI-DRKI3873-NI-BDKINHABERNAME1  PIC S9(4) COMP.                 
             15 PI-DRKI3873-NI-BDKINHABERNAME2  PIC S9(4) COMP.                 
             15 PI-DRKI3873-NI-BDKKARTENART     PIC S9(4) COMP.                 
             15 PI-DRKI3873-NI-BDKNFCKZN        PIC S9(4) COMP.                 
             15 PI-DRKI3873-NI-BDKNFCSTATUS     PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3873-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3873-BDKGUELTIGBIS   PIC  X(0004).                       
             10 PI-DRKI3873-BDKINHABERNAME1   PIC  X(0030).                     
             10 PI-DRKI3873-BDKINHABERNAME2   PIC  X(0030).                     
             10 PI-DRKI3873-BDKKARTENART   PIC  X(0003).                        
             10 PI-DRKI3873-BDKNFCKZN   PIC  X(0001).                           
             10 PI-DRKI3873-BDKNFCSTATUS   PIC  X(0001).                        
             10 PI-DRKI3873-LETZTERAENDERER   PIC  X(0010).                     
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3873-NI1.                                                     
           05 PI-DRKI3873-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 6.                                 
       01  PI-DRKI3873-NI7.                                                     
           05 PI-DRKI3873-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 6.                                 
                                                                                
      *                                                                         
       01  PO-DRKI3873.                                                         
           05  PO-DRKI3873-RC                     PIC  X(003).                  
               88 PO-DRKI3873-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI3873-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
