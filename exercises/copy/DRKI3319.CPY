      *****************************************************************
      *  Rück-DTA für Generali Altverträge (Rumpfdaten)               *
      *  Adressänderungen inkl. der Gesellschaftsnummer               *
      *  Hier zusammengefasst sind:                                   *
      *                                                               *
      *  Gesellschaft 11 - Generali Sach                              *
      *  Gesellschaft 21 - Generali Leben                             *
      *  Gesellschaft 42 - Generali Invest                            *
      *                                                               *
      *****************************************************************
      *  Satzlänge 459 Bytes                                          *
      *                                                               *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01573!05.03.18!2.0 ! Neuerstellung             ! tfl       *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * DBO-10663  !13.12.22! Fillerbereich "ansprechbar"  ! t13ex78     
      *            !        ! gemacht um mit SPACES zu be- !
      *            !        ! legen                        !  
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
       01  'XX'-RDTA-ADRA-SATZ.
           05 'XX'-SATZART                    PIC  9(002).
           05 'XX'-GESELLSCHAFT               PIC  9(002).
           05 'XX'-VNR                        PIC  X(015).
           05 'XX'-DIRNR                      PIC  9(003).
           05 'XX'-GESCHAEFTSSTELLE           PIC  X(010).
           05 'XX'-VBNR                       PIC  9(007).
           05 'XX'-AENDERUNGSDATUM            PIC  X(010).
           05 'XX'-AENDERUNGSGRUND            PIC  9(002).
           05 'XX'-HERKUNFTSKZ                PIC  X(001).
           05 'XX'-KUNDENROLLE                PIC  9(002).
           05 'XX'-KUNDENNAMEALT              PIC  X(030).
           05 'XX'-KUNDENVORNAMEALT           PIC  X(015).
           05 'XX'-STRASSEALT                 PIC  X(045).
           05 'XX'-PLZALT                     PIC  X(015).
           05 'XX'-ORTALT                     PIC  X(030).
           05 'XX'-AUSLANDSKZALT              PIC  X(003).
           05 'XX'-KUNDENNAMENEU              PIC  X(030).
           05 'XX'-KUNDENVORNAMENEU           PIC  X(015).
           05 'XX'-STRASSENEU                 PIC  X(045).
           05 'XX'-PLZNEU                     PIC  X(015).
           05 'XX'-ORTNEU                     PIC  X(030).
           05 'XX'-AUSLANDSKZNEU              PIC  X(003).
           05 'XX'-ANREDEKZNEU                PIC  9(002).
           05 'XX'-PERSONARTKZNEU             PIC  X(001).
           05 'XX'-TELEFONNRNEU               PIC  X(128).
           05 'XX'-GEBGRUENDDAT               PIC  X(010).
           05 'XX'-RESERVE                    PIC  X(029).
      *
      ****** Ende Copy-Member ****************************************


