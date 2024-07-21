      *****************************************************************         
      *    COPY-MEMBER: DRKI3961                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3961                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         12.09.2022 / 13:42:59 / U130685            
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
      *    FÜR DIE FUNKTION DRKI3961                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  DELETE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3961.                                                         
           05  PI-DRKI3961-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3961-G79GRSPARRATEID   PIC S9(9) COMP.                   
             10 PI-DRKI3961-G79GRVNR   PIC  X(0015).                            
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3961.                                                         
           05  PO-DRKI3961-RC                     PIC  X(003).                  
               88 PO-DRKI3961-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3961-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************