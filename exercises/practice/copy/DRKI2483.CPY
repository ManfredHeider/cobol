      *****************************************************************
      *    COPY-MEMBER: DRKI2483
      *
      *    G21/VIRIDIUM DTA-KUNDE
      *
      *    DTA KUNDE ZUM PG G21/PROXALTO ÜBER DIE LIEFERUNG DER
      *    VIRIDIUM.
      *
      *    SATZLÄNGE VARIABEL
      *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------
      * DBO-10293  !15.08.22! NEU                          ! T13AC20
      * -----------!--------!------------------------------!----------
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
      *    HAUPTSATZ SATZART 'C'
           05 HAUPTSATZ.
              10 SATZART                       PIC  X(001).
              10 VNR                           PIC  X(015).
              10 NACHNAME                      PIC  X(080).
              10 VORNAME                       PIC  X(030).
              10 GEBDAT                        PIC  X(010).
              10 ANREDE                        PIC S9(002).
              10 GESCHLECHT                    PIC  X(001).
              10 STR                           PIC  X(045).
              10 PLZ                           PIC  X(015).
              10 ORT                           PIC  X(030).
              10 LKZN                          PIC  X(003).
      *
      *    VORSATZ   SATZART 'A'
           05 VORSATZ  REDEFINES HAUPTSATZ.
              10 V-SATZART                     PIC  X(001).
      *          ABSENDER FIX 'VIR'
              10 V-ABSENDER                    PIC  X(003).
              10 V-ERSTELLDAT                  PIC  X(010).
      *          EMPFAENGER FIX 'ALAG'
              10 V-EMPFAENGER                  PIC  X(004).
      *          INHALT FIX 'KUNDE               '
              10 V-INHALT                      PIC  X(020).
      *
      *    NACHSATZ  SATZART 'E'
           05 NACHSATZ REDEFINES HAUPTSATZ.
              10 N-SATZART                     PIC  X(001).
              10 N-ANZSATZ                     PIC  9(009).
      *****************************************************************
      *---------------------- ENDE DRKI2483 --------------------------*
      *****************************************************************