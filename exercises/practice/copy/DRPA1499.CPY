      *****************************************************************
      *    COPY-MEMBER: DRPA1499  PEG-DATENSATZ GES. 81 - ADVOCARD    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    03.07.01                                         *
      *****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00133!03.07.01!2.1 ! NEU                       ! KLEEMEIER*
      * APA00133!17.07.01!2.2 ! KOMPLETT NEUER SATZAUFBAU,! KLEEMEIER*
      *         !        !    ! ANALOG OPAL DROP0030      !          *
      * APA00133!13.08.01!2.3 ! PROV-SATZ UND VERTR-ABLAUF! KLEEMEIER*
      *         !        !    ! AN DIE RICHTIGE STELLE    !          *
      * APA00133!16.08.01!2.4 ! PRAEMIE MIT VORZEICHEN    ! KLEEMEIER*
      * APA00175!24.01.02!2.5 ! PPFL-BEITRAG, PROV-BETRAG ! WANG     *
      *         !        !    ! AUF 2 NK ERW. KUNDENNAME  !          *
      *         !        !    ! VON 30 AUF 25 GEKÜRZT     !          *
      * APA00194!08.05.02!2.6 ! ZWEI NEUE DATENFELDER     ! WANG     *
      * APA00197!22.05.02!2.7 ! NEUER SATZAUFBAU          ! KLEEMEIER*
      * APA00510!15.09.06!2.8 ! PROD-BEZ-KZ  NEU          ! WANG     *
      * APA00622!29.10.07!2.9 ! TARIF-ERGAENZUNG NEU      ! STORCH   *
      *         !        !    ! VERSCHIEBUNG PROD-BEZ-KZ  !          *
      * APA00622!18.12.07!2.10! DEAKTIVIERUNG VERSION 2.9 ! STORCH   *
      *         !        !    ! DA DIESE VERSION NICHT    !          *
      *         !        !    ! NACH PRODUKTION GING.     !          *
      * FPA00980!07.01.08!2.11!                           !          *
      * APA00883!15.03.09!2.12! Anpassung Filler wegen    ! Jäger    *
      *         !        !    ! Änderung durch Advocard   !          *
      * APA02910!29.01.20!2.13! NEUE ANPASSUNG            ! WANG     *
      *         !03.04.20!2.14! AUF VER.2.12 ZURÜCKSETZEN !          *
      *         !03.04.20!2.15! WIEDER AUF VER.2.13 SETZEN!          *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    ADVOCARD-DTAS.                                            *
      *                                                              *
      ****************************************************************
      *
             30 ADVOCARD-DATENSATZ.
                35 PGEB-GES                 PIC  9(002).
                35 FILLER                   PIC  X(007).
                35 AVERM                    PIC  9(007).
                35 FILLER                   PIC  X(001).
                35 VNR                      PIC  9(011)       COMP-3.
                35 PIN                      PIC  9(007).
                35 FILLER                   PIC  X(009).
                35 LAUFZEIT                 PIC  9(002).
                35 KUNDE-NAME               PIC  X(030).
                35 BG                       PIC  9(002).
                35 WERTST                   PIC  9(002).
                35 PROD-BEZ                 PIC  X(004).
                35 FILLER                   PIC  X(001).
                35 FAELLIG-VON              PIC  9(008).
                35 FILLER                   PIC  X(005).
                35 PPFL-AP-BEITRAG          PIC S9(007)V9(02) COMP-3.
                35 PPFL-BEITRAG             PIC S9(007)V9(02) COMP-3.
                35 PROV-AP-BETRAG           PIC S9(007)V9(02) COMP-3.
                35 PROV-BETRAG              PIC S9(007)V9(02) COMP-3.
                35 FILLER                   PIC  X(010).
                35 ABR-DAT-PG               PIC  9(008).
                35 ZAHLWEISE                PIC  9(001).
                35 VERTR-BEGINN             PIC  9(008).
                35 FILLER                   PIC  X(009).
                35 PRAEMIE                  PIC S9(007)V9(02) COMP-3.
                35 FILLER                   PIC  X(047).
                35 VERTRAG-WAEHRUNG         PIC  X(003).
                35 BEITRAG-BIS              PIC  X(008).
                35 VERTR-ABLAUF             PIC  9(008).
                35 PROV-SATZ                PIC  9(003)V9(02) COMP-3.
                35 TARIF                    PIC  X(003).
                35 PROD-BEZ-KZ              PIC  X(001).
                35 HALBER-PROV-SATZ         PIC  X(001).
                35 VERTR-NEUORDNUNG         PIC  X(001).
                35 GESAMT-JAHRES-NT-BEITRAG PIC S9(007)V9(02) COMP-3.
                35 VORSCHUESSIGE-PROV       PIC  X(001).
                35 VERTRAGSTEIL             PIC  9(003).
      *
      ******* ENDE COPY-MEMBER **************************************



