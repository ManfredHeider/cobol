      *****************************************************************         
      *    COPY-MEMBER: DRKI3698                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3698                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         06.02.2023 / 13:47:17 / U131774            
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
      *    FÜR DIE FUNKTION DRKI3698                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3698.                                                         
           05  PI-DRKI3698-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3698-G79DVERTRAGSNR   PIC  X(0015).                      
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3698.                                                         
           05  PO-DRKI3698-RC                     PIC  X(003).                  
               88 PO-DRKI3698-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3698-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI3698-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3698-G79DABSCHLVBNR   PIC S9(9) COMP.                    
             10 PO-DRKI3698-G79DANREDE   PIC  X(0001).                          
             10 PO-DRKI3698-G79DEMAIL   PIC  X(0040).                           
             10 PO-DRKI3698-G79DFIRMENNAME   PIC  X(0070).                      
             10 PO-DRKI3698-G79DGEBDAT   PIC  X(0008).                          
             10 PO-DRKI3698-G79DGESID   PIC S9(4) COMP.                         
             10 PO-DRKI3698-G79DKUNDENID   PIC S9(9) COMP.                      
             10 PO-DRKI3698-G79DLAND   PIC  X(0003).                            
             10 PO-DRKI3698-G79DMANDANT   PIC S9(9) COMP.                       
             10 PO-DRKI3698-G79DNACHNAME   PIC  X(0030).                        
             10 PO-DRKI3698-G79DORT   PIC  X(0030).                             
             10 PO-DRKI3698-G79DPLZ   PIC  X(0009).                             
             10 PO-DRKI3698-G79DSTRHNR   PIC  X(0040).                          
             10 PO-DRKI3698-G79DVORNAME   PIC  X(0030).                         
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
