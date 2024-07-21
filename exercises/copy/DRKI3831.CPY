      *****************************************************************         
      *    COPY-MEMBER: DRKI3831                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3831                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         03.05.2022 / 09:36:05 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3831                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3831.                                                         
           05  PI-DRKI3831-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3831-AKMANGEBOTSNAME   PIC  X(0010).                     
             10 PI-DRKI3831-AKMANGEBOTSNR   PIC S9(4) COMP.                     
             10 PI-DRKI3831-AKMPERSONLFDNR   PIC S9(4) COMP.                    
             10 PI-DRKI3831-AKMTARIFNAME   PIC  X(0040).                        
             10 PI-DRKI3831-AKMTARIFTEIL-ID   PIC  X(0010).                     
             10 PI-DRKI3831-AKMVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3831.                                                         
           05  PO-DRKI3831-RC                     PIC  X(003).                  
               88 PO-DRKI3831-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3831-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI3831-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3831-AKMGESAMT-BEITRAG   PIC S9(7)V9(2) COMP-3.          
             10 PO-DRKI3831-AKMGESETZL-ZUSCHL   PIC S9(7)V9(2) COMP-3.          
             10 PO-DRKI3831-AKMPAUSCH-LEIST-1   PIC S9(7)V9(2) COMP-3.          
             10 PO-DRKI3831-AKMPAUSCH-LEIST-2   PIC S9(7)V9(2) COMP-3.          
             10 PO-DRKI3831-AKMPAUSCH-LEIST-3   PIC S9(7)V9(2) COMP-3.          
             10 PO-DRKI3831-AKMPAUSCH-LEIST-4   PIC S9(7)V9(2) COMP-3.          
             10 PO-DRKI3831-AKMSELBSTBEHALT   PIC S9(7)V9(2) COMP-3.            
             10 PO-DRKI3831-AKMSTICHTAG   PIC  X(010).                          
             10 PO-DRKI3831-AKMSUM-BEITRZUSCHL   PIC S9(7)V9(2) COMP-3.         
             10 PO-DRKI3831-AKMTARIFSTATUS   PIC  X(0020).                      
             10 PO-DRKI3831-AKMVERS-LEISTUNG   PIC S9(7)V9(2) COMP-3.           
             10 PO-DRKI3831-AKMZEITL-BEFR-LIM   PIC S9(7)V9(2) COMP-3.          
             10 PO-DRKI3831-LETZTEAENDERUNG   PIC  X(0026).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
