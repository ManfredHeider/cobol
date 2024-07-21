      *****************************************************************         
      *    COPY-MEMBER: DRKI3832                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3832                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         03.05.2022 / 09:36:08 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3832                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3832.                                                         
           05  PI-DRKI3832-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3832-AKMANGEBOTSNAME   PIC  X(0010).                     
             10 PI-DRKI3832-AKMANGEBOTSNR   PIC S9(4) COMP.                     
             10 PI-DRKI3832-AKMPERSONLFDNR   PIC S9(4) COMP.                    
             10 PI-DRKI3832-AKMTARIFNAME   PIC  X(0040).                        
             10 PI-DRKI3832-AKMTARIFTEIL-ID   PIC  X(0010).                     
             10 PI-DRKI3832-AKMVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI3832-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3832-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3832-AKMGESAMT-BEITRAG   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3832-AKMGESETZL-ZUSCHL   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3832-AKMPAUSCH-LEIST-1   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3832-AKMPAUSCH-LEIST-2   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3832-AKMPAUSCH-LEIST-3   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3832-AKMPAUSCH-LEIST-4   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3832-AKMSELBSTBEHALT   PIC S9(7)V9(2) COMP-3.            
             10 PI-DRKI3832-AKMSTICHTAG   PIC  X(010).                          
             10 PI-DRKI3832-AKMSUM-BEITRZUSCHL   PIC S9(7)V9(2) COMP-3.         
             10 PI-DRKI3832-AKMTARIFSTATUS   PIC  X(0020).                      
             10 PI-DRKI3832-AKMVERS-LEISTUNG   PIC S9(7)V9(2) COMP-3.           
             10 PI-DRKI3832-AKMZEITL-BEFR-LIM   PIC S9(7)V9(2) COMP-3.          
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3832-NI1.                                                     
           05 PI-DRKI3832-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI3832-NI7.                                                     
           05 PI-DRKI3832-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI3832.                                                         
           05  PO-DRKI3832-RC                     PIC  X(003).                  
               88 PO-DRKI3832-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI3832-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
