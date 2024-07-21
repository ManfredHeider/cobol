      *****************************************************************         
      *    COPY-MEMBER: DRKI3974                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3974                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         12.09.2022 / 13:43:00 / U130685            
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
      *    FÜR DIE FUNKTION DRKI3974                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  COUNT  <<<                                              
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3974.                                                         
           05  PI-DRKI3974-KEY.                                                 
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3974-G79GRVNR   PIC  X(0015).                            
END   *                                                                         
      *                                                                         
       01  PO-DRKI3974.                                                         
           05  PO-DRKI3974-RC                     PIC  X(003).                  
               88 PO-DRKI3974-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3974-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI3974-DATEN.                                               
             10   PO-DRKI3974-COUNTER           PIC S9(4) COMP.                 
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************