      *****************************************************************         
      *    COPY-MEMBER: DRKI3833                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3833                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         03.05.2022 / 09:36:11 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3833                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3833.                                                         
           05  PI-DRKI3833-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3833-AKMANGEBOTSNAME   PIC  X(0010).                     
             10 PI-DRKI3833-AKMANGEBOTSNR   PIC S9(4) COMP.                     
             10 PI-DRKI3833-AKMPERSONLFDNR   PIC S9(4) COMP.                    
             10 PI-DRKI3833-AKMTARIFNAME   PIC  X(0040).                        
             10 PI-DRKI3833-AKMTARIFTEIL-ID   PIC  X(0010).                     
             10 PI-DRKI3833-AKMVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI3833-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3833-N-KMGSMT-BTRG       PIC S9(4) COMP.                 
             15 PI-DRKI3833-N-KMGSTZL-ZSCHL     PIC S9(4) COMP.                 
             15 PI-DRKI3833-N-KMPSCH-LST-1      PIC S9(4) COMP.                 
             15 PI-DRKI3833-N-KMPSCH-LST-2      PIC S9(4) COMP.                 
             15 PI-DRKI3833-N-KMPSCH-LST-3      PIC S9(4) COMP.                 
             15 PI-DRKI3833-N-KMPSCH-LST-4      PIC S9(4) COMP.                 
             15 PI-DRKI3833-NI-AKMSELBSTBEHALT  PIC S9(4) COMP.                 
             15 PI-DRKI3833-NI-AKMSTICHTAG      PIC S9(4) COMP.                 
             15 PI-DRKI3833-N-KMSM-BTRZSCHL     PIC S9(4) COMP.                 
             15 PI-DRKI3833-NI-AKMTARIFSTATUS   PIC S9(4) COMP.                 
             15 PI-DRKI3833-N-KMVRS-LSTNG       PIC S9(4) COMP.                 
             15 PI-DRKI3833-N-KMZTL-BFR-LM      PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3833-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3833-AKMGESAMT-BEITRAG   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3833-AKMGESETZL-ZUSCHL   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3833-AKMPAUSCH-LEIST-1   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3833-AKMPAUSCH-LEIST-2   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3833-AKMPAUSCH-LEIST-3   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3833-AKMPAUSCH-LEIST-4   PIC S9(7)V9(2) COMP-3.          
             10 PI-DRKI3833-AKMSELBSTBEHALT   PIC S9(7)V9(2) COMP-3.            
             10 PI-DRKI3833-AKMSTICHTAG   PIC  X(010).                          
             10 PI-DRKI3833-AKMSUM-BEITRZUSCHL   PIC S9(7)V9(2) COMP-3.         
             10 PI-DRKI3833-AKMTARIFSTATUS   PIC  X(0020).                      
             10 PI-DRKI3833-AKMVERS-LEISTUNG   PIC S9(7)V9(2) COMP-3.           
             10 PI-DRKI3833-AKMZEITL-BEFR-LIM   PIC S9(7)V9(2) COMP-3.          
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3833-NI1.                                                     
           05 PI-DRKI3833-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 12.                                    
       01  PI-DRKI3833-NI7.                                                     
           05 PI-DRKI3833-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 12.                                    
      *                                                                         
       01  PO-DRKI3833.                                                         
           05  PO-DRKI3833-RC                     PIC  X(003).                  
               88 PO-DRKI3833-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3833-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
