      *****************************************************************         
      *    COPY-MEMBER: DRKI3834                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3834                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         03.05.2022 / 09:36:15 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3834                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  DELETE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3834.                                                         
           05  PI-DRKI3834-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3834-AKMANGEBOTSNAME   PIC  X(0010).                     
             10 PI-DRKI3834-AKMANGEBOTSNR   PIC S9(4) COMP.                     
             10 PI-DRKI3834-AKMPERSONLFDNR   PIC S9(4) COMP.                    
             10 PI-DRKI3834-AKMTARIFNAME   PIC  X(0040).                        
             10 PI-DRKI3834-AKMTARIFTEIL-ID   PIC  X(0010).                     
             10 PI-DRKI3834-AKMVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3834.                                                         
           05  PO-DRKI3834-RC                     PIC  X(003).                  
               88 PO-DRKI3834-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3834-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
