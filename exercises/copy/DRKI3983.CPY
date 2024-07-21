      *****************************************************************
      *    COPY-MEMBER: DRKI3983
      *
      *   G21 - Viridium
      *   Copy-Book für den Rück-DTA Betreuerwechsel und Direktions-
      *   änderungen.
      *   Weicht darin vom "Standard" ab, dass am Ende noch die Sparte 
      *   aus GENVERTRAGDETAIL angehängt ist. 
      *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * DBO-10663  !13.12.22! Neuerstellung                ! t13ex78
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
       01 'XX'-BETRW-MIT-SPARTE.
      * Satzart 
      * 01 - Betreuerwechsel
      * 05 - Direktionsänderung
          05 'XX'-SATZART                     PIC  9(002).
      * Partnergesellschaftsnummer   
          05 'XX'-GESELLSCHAFT                PIC  9(002).
      * Vertragsnummer   
          05 'XX'-VNR                         PIC  X(015).
      * Direktionsnummer (neu)   
          05 'XX'-DIRNR                       PIC  9(003).
      * VB-Nummer des neuen betreuenden VB   
          05 'XX'-VBNR                        PIC  9(007).
      * Sparte aus GENVERTRAGDETAIL   
          05 'XX'-SPARTE                      PIC  X(003).
      *****************************************************************
      *---------------------- ENDE DRKI3983 --------------------------*
      *****************************************************************
