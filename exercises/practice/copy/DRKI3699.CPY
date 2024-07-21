      *****************************************************************         
      *    COPY-MEMBER: DRKI3699                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3699                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         06.02.2023 / 13:47:19 / U131774            
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
      *    FÜR DIE FUNKTION DRKI3699                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3699.                                                         
           05  PI-DRKI3699-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3699-G79DVERTRAGSNR   PIC  X(0015).                      
END   *                                                                         
                                                                                
           05  PI-DRKI3699-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3699-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3699-G79DABSCHLVBNR   PIC S9(9) COMP.                    
             10 PI-DRKI3699-G79DANREDE   PIC  X(0001).                          
             10 PI-DRKI3699-G79DEMAIL   PIC  X(0040).                           
             10 PI-DRKI3699-G79DFIRMENNAME   PIC  X(0070).                      
             10 PI-DRKI3699-G79DGEBDAT   PIC  X(0008).                          
             10 PI-DRKI3699-G79DGESID   PIC S9(4) COMP.                         
             10 PI-DRKI3699-G79DKUNDENID   PIC S9(9) COMP.                      
             10 PI-DRKI3699-G79DLAND   PIC  X(0003).                            
             10 PI-DRKI3699-G79DMANDANT   PIC S9(9) COMP.                       
             10 PI-DRKI3699-G79DNACHNAME   PIC  X(0030).                        
             10 PI-DRKI3699-G79DORT   PIC  X(0030).                             
             10 PI-DRKI3699-G79DPLZ   PIC  X(0009).                             
             10 PI-DRKI3699-G79DSTRHNR   PIC  X(0040).                          
             10 PI-DRKI3699-G79DVORNAME   PIC  X(0030).                         
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3699-NI1.                                                     
           05 PI-DRKI3699-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI3699-NI7.                                                     
           05 PI-DRKI3699-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI3699.                                                         
           05  PO-DRKI3699-RC                     PIC  X(003).                  
               88 PO-DRKI3699-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI3699-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
