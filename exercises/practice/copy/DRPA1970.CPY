      *****************************************************************
      *    COPY-MEMBER: DRPA1970                                     
      *                                                              
      *   Funktionen-Copy für UWPA0021                             
      *                                                              
      *   Prüfen, ob zur MA-ST-NR ein RFT-Konto vorliegt               
      *                                                              
      *   Ja  : Ersetzen MA-ST-NR durch RFT-Konto             
      *   Nein: Keine Ersetzung                      
      *                                                              
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DAISY-4437 !27.04.22! NEUERSTELLUNG                ! U131764  *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                                 
       01  PI-DRPA1970.                                                    
           05 PI-DRPA1970-DATEN.                                          
              10 PI-DRPA1970-TAGESDATUM      PIC  X(010).             
      *                                                                 
       01  PO-DRPA1970.                                                      
           05 PO-DRPA1970-RC                 PIC  X(003).                 
              88 PO-DRPA1970-RC-OK           VALUE 'OK '.               
              88 PO-DRPA1970-RC-VSG          VALUE 'VSG'.               
              88 PO-DRPA1970-RC-NOT-OK       VALUE 'NOK'.               
           05 PO-DRPA1970-DATEN.                                          
              10 PO-DRPA1970-STRK-AEND-KZ    PIC  X(001).
      *
      *****************************************************************
      *---------------------- ENDE DRPA1970 --------------------------* 
      *****************************************************************
