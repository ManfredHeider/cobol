      *****************************************************************         
      *    COPY-MEMBER: DRKI3958                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3958                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         12.09.2022 / 13:42:54 / U130685            
      *****************************************************************         
      *    VERSION                                                              
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DBO-10295  !12.09.22! NEUERSTELLUNG                ! ATAR     *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                                         
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER                    
      *    FÜR DIE FUNKTION DRKI3958                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3958.                                                         
           05  PI-DRKI3958-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3958-G79GRSPARRATEID   PIC S9(9) COMP.                   
             10 PI-DRKI3958-G79GRVNR   PIC  X(0015).                            
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3958.                                                         
           05  PO-DRKI3958-RC                     PIC  X(003).                  
               88 PO-DRKI3958-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3958-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI3958-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3958-G79GRABBUCHTAG   PIC  X(0001).                      
             10 PO-DRKI3958-G79GRABSCHLVBNR   PIC S9(9) COMP.                   
             10 PO-DRKI3958-G79GRABWNAME   PIC  X(0040).                        
             10 PO-DRKI3958-G79GRBEGINNDAT   PIC  X(0008).                      
             10 PO-DRKI3958-G79GRBESTEURO   PIC S9(7)V9(2) COMP-3.              
             10 PO-DRKI3958-G79GRBESTGRAMM   PIC S9(7)V9(4) COMP-3.             
             10 PO-DRKI3958-G79GRBIC   PIC  X(0011).                            
             10 PO-DRKI3958-G79GRBUCHUNGSDAT   PIC  X(0008).                    
             10 PO-DRKI3958-G79GRDYNAMIK   PIC S9(3)V9(2) COMP-3.               
             10 PO-DRKI3958-G79GRENDEAUSDAT   PIC  X(0008).                     
             10 PO-DRKI3958-G79GRENDEDAT   PIC  X(0008).                        
             10 PO-DRKI3958-G79GRFLATRATE   PIC  X(0001).                       
             10 PO-DRKI3958-G79GRIBAN   PIC  X(0034).                           
             10 PO-DRKI3958-G79GRISIN   PIC  X(0012).                           
             10 PO-DRKI3958-G79GRKNDRABATT   PIC  X(0001).                      
             10 PO-DRKI3958-G79GRKUNDENDATEN.                                   
                49 PO-DRKI3958-G79GRKNDNDTN-LEN PIC S9(4) COMP.                 
                49 PO-DRKI3958-G79GRKNDNDTN-TEXT PIC X(382).                    
             10 PO-DRKI3958-G79GRKUNDENIDEXT   PIC S9(9) COMP.                  
             10 PO-DRKI3958-G79GRMANDANT   PIC S9(9) COMP.                      
             10 PO-DRKI3958-G79GRRABATTKZN   PIC  X(0001).                      
             10 PO-DRKI3958-G79GRRATEOEFFDAT   PIC  X(0008).                    
             10 PO-DRKI3958-G79GRRATESCHLDAT   PIC  X(0008).                    
             10 PO-DRKI3958-G79GRRUECKGRAMM   PIC S9(4)V9(2) COMP-3.            
             10 PO-DRKI3958-G79GRSPARRATE   PIC S9(7)V9(2) COMP-3.              
             10 PO-DRKI3958-G79GRSPARRATELFDNR   PIC S9(4) COMP.                
             10 PO-DRKI3958-G79GRSTATUS   PIC  X(0001).                         
             10 PO-DRKI3958-G79GRSUMMEAUS   PIC S9(7)V9(2) COMP-3.              
             10 PO-DRKI3958-G79GRSUMMEEIN   PIC S9(7)V9(2) COMP-3.              
             10 PO-DRKI3958-G79GRZAHLUNGSART   PIC  X(0001).                    
             10 PO-DRKI3958-LETZTERAENDERER   PIC  X(0010).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************