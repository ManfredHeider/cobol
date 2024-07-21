      *****************************************************************
      *    COPY-MEMBER: DRKI3984
      *
      *   G21 - Viridium
      *   Copy-Book für den Rück-DTA Betreuerwechsel und Direktions-
      *   änderungen
      *   Weicht darin vom "Standard" ab, dass am Ende noch die Sparte
      *   aus GENVERTRAGDETAIL angehängt ist.
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
       01 'XX'-ADRA-MIT-SPARTE.
           05 'XX'-SATZART                     PIC  9(002).
           05 'XX'-GESELLSCHAFT                PIC  9(002).
           05 'XX'-VNR                         PIC  X(015).
           05 'XX'-DIRNR                       PIC  9(003).
           05 'XX'-GESCHAEFTSSTELLE            PIC  X(010).
           05 'XX'-VBNR                        PIC  9(007).
           05 'XX'-AENDERUNGSDATUM             PIC  X(010).
           05 'XX'-AENDERUNGSGRUND             PIC  9(002).
           05 'XX'-HERKUNFTSKZ                 PIC  X(001).
           05 'XX'-KUNDENROLLE                 PIC  9(002).
           05 'XX'-KUNDENNAMEALT               PIC  X(030).
           05 'XX'-KUNDENVORNAMEALT            PIC  X(015).
           05 'XX'-STRASSEALT                  PIC  X(045).
           05 'XX'-PLZALT                      PIC  X(015).
           05 'XX'-ORTALT                      PIC  X(030).
           05 'XX'-AUSLANDSKZALT               PIC  X(003).
           05 'XX'-KUNDENNAMENEU               PIC  X(030).
           05 'XX'-KUNDENVORNAMENEU            PIC  X(015).
           05 'XX'-STRASSENEU                  PIC  X(045).
           05 'XX'-PLZNEU                      PIC  X(015).
           05 'XX'-ORTNEU                      PIC  X(030).
           05 'XX'-AUSLANDSKZNEU               PIC  X(003).
           05 'XX'-ANREDEKZNEU                 PIC  9(002).
           05 'XX'-PERSONARTKZNEU              PIC  X(001).
           05 'XX'-TELEFONNRNEU                PIC  X(128).
           05 'XX'-GEBGRUENDDAT                PIC  X(010).
           05 'XX'-SPARTE                      PIC  X(003).
           05 'XX'-RESERVE                     PIC  X(026).
      *****************************************************************
      *---------------------- ENDE DRKI3984 --------------------------*
      *****************************************************************
