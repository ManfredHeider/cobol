      *****************************************************************
      *    COPY-MEMBER: DRPA1953                                     
      *                                                              
      *   Funktion UWPA0049 Sonst.Ges.AT Haftpfl. fprmal prüfen         
      *   und normieren                                                 
      *                                                             
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER        
      *    FÜR DIE FUNKTION DRPA1953                                
      *    AUSSER: DEM FUNKTIONSNAMEN,                              
      *            DEM ERR-ERROR-BEREICH UND                        
      *            DEM STANDARD-STATUS-BEREICH                      
      *            (JEWEILS SEPARATE COPYS)                         
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DAISY-3552 !08.06.21! Neu                          ! Kranz    *     
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
       01 PI-DRPA1953.                                              
             10 PI-DRPA1953-PZVP-VERSION    PIC S9(004) BINARY.     
             10 PI-DRPA1953-PEG-DATEN-SOGATHP.                          
                COPY DRPA1954.                                      
             10 PI-DRPA1953-KNR-MANDANT     PIC S9(004) BINARY.     
      *                                                             
       01 PO-DRPA1953.                                              
          05 PO-DRPA1953-RC                 PIC  X(003).            
             88 PO-DRPA1953-RC-OK                VALUE 'OK '.       
             88 PO-DRPA1953-RC-VSTAT-GEFUELLT    VALUE 'VSG'.       

      *****************************************************************
      *---------------------- ENDE DRPA1953 --------------------------* 
      *****************************************************************
