      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM PMKI1375              *
      *     - AUSGABEDATEI Tabelle GENVERTRAGDETAIL                    *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *07.03.18! NEU                                  ! BST     !      *
      *18.11.20!Änderung zu Viridium neues Feld       ! HHU     !      *
      *AKI01812! MIG-KZ ersetzt AKZ und Wagnis        !         !      *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------
      * DBO-10293  !08.09.22! NEUE FELDER RNTZAHLW,        ! T13AC20
      *            !        !             LEISTUNG-KZN,    !
      *            !        !             ERLFALL-SUM,     !
      *            !        !             TODFALL-SUM,     !
      *            !        !             RNTFAKTOR,       !
      *            !        !             KAPABFIND,       !
      *            !        !             FONDSGUTH        !
      * -----------!--------!------------------------------!----------
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      **************************************************************** *
      *                                                                *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *     LAENGE: 400 BYTES                   STAND: 08.09.2022      *
      **************************************************************** *

       01  DRKI3307.

           05  'XX'-VERARB-KZ               PIC  X(001).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete
           05  'XX'-VERTRAGSID              PIC  X(013).

           05  'XX'-LFDNR                   PIC  9(004).
           05  'XX'-PRODUKTID               PIC  9(004).
           05  'XX'-BEGINN                  PIC  X(010).
           05  'XX'-BEGINN-AENDKZ           PIC  X(001).
     **       'J' wenn Beginn geändert

           05  'XX'-ABLAUF                  PIC  X(010).
           05  'XX'-ABLAUF-AENDKZ           PIC  X(001).
     **       'J' wenn Ablauf geändert

           05  'XX'-BEITRAG                 PIC  9(008)V99.
           05  'XX'-BEITRAG-AENDKZ          PIC  X(001).
     **       'J' wenn Beitrag geändert

           05  'XX'-NJBEITRAG               PIC  9(008)V99.
           05  'XX'-NJBEITRAG-AENDKZ        PIC  X(001).
     **       'J' wenn Nettojahresbeitrag geändert

           05  'XX'-SPARTE                  PIC  X(003).
           05  'XX'-SPARTE-AENDKZ           PIC  X(001).
      *        'J' wenn Zahlweise geändert

           05  'XX'-PRODUKTBEZ              PIC  X(050).
           05  'XX'-PRODUKTBEZ-AENDKZ       PIC  X(001).
     **       'J' wenn Produktbezeichnung geändert

           05  'XX'-TARIF                   PIC  X(050).
           05  'XX'-TARIF-AENDKZ            PIC  X(001).
      *        'J' wenn Tarif geändert

           05  'XX'-TARIF-PRODUKT           PIC  9(009).
           05  'XX'-TARIF-PRODUKT-AENDKZ    PIC  X(001).
      *        'J' wenn Tarif-Produkt geändert

           05  'XX'-LV-ART                  PIC  9(002).
           05  'XX'-LV-ART-AENDKZ           PIC  X(001).
      *        'J' wenn LV-Art geändert

           05  'XX'-SUMME                   PIC  9(008)V99.
           05  'XX'-SUMME-AENDKZ            PIC  X(001).
      *        'J' wenn Summe geändert
           05  FILLER                       PIC  X(015).

      *    Migrationskz P = Viridium sonst SPACE
           05  'XX'-MIG-KZ                  PIC X(001).
           05  'XX'-MIG-KZ-AENDKZ           PIC X(001).
      *        'J' wenn MIG-KZ geändert

           05  'XX'-GUTHABEN                PIC  9(008)V99.
           05  'XX'-GUTHABEN-AENDKZ         PIC  X(001).
      *        'J' wenn Guthaben geändert

           05  'XX'-AKTVWERT                PIC  9(008)V99.
           05  'XX'-AKTVWERT-AENDKZ         PIC  X(001).
      *        'J' wenn aktueller Vertragswert geändert

           05  'XX'-RENTENBEZUG             PIC  X(001).
           05  'XX'-RENTENBEZUG-AENDKZ      PIC  X(001).
      *        'J' wenn Rentenbezugkennz. geändert

           05  'XX'-RENTEVORAUS             PIC  9(008)V99.
           05  'XX'-RENTEVORAUS-AENDKZ      PIC  X(001).
      *        'J' wenn voraussichtliche Rente geändert

           05  'XX'-RENTEGARANT             PIC  9(008)V99.
           05  'XX'-RENTEGARANT-AENDKZ      PIC  X(001).
      *        'J' wenn garantierte Rente geändert

           05  'XX'-BERECHDAT               PIC  X(010).
           05  'XX'-BERECHDAT-AENDKZ        PIC  X(001).
      *        'J' wenn Berechnungsdatum geändert

           05  'XX'-GESCHAEFTSZW            PIC  X(002).
           05  'XX'-GESCHAEFTSZW-AENDKZ     PIC  X(001).
      *        'J' wenn Geschäftszweig geändert

           05  'XX'-FLAECHE                 PIC  9(005).
           05  'XX'-FLAECHE-AENDKZ          PIC  X(001).
      *        'J' wenn Fläche geändert

           05  'XX'-PRODKOMBI               PIC  9(005).
           05  'XX'-PRODKOMBI-AENDKZ        PIC  X(001).
      *        'J' wenn Produkt-Kombi-Nr. geändert

           05  'XX'-RNTZAHLW                PIC S9(002).
           05  'XX'-RNTZAHLW-AENDKZ         PIC  X(001).
      *        'J' wenn Rentenzahlweise geändert

           05  'XX'-LEISTUNG-KZN            PIC  X(001).
           05  'XX'-LEISTUNG-KZN-AENDKZ     PIC  X(001).
      *        'J' wenn Leistungskennzeichen ändert

           05  'XX'-ERLFALL-SUM             PIC  9(008)V99.
           05  'XX'-ERLFALL-SUM-AENDKZ      PIC  X(001).
      *        'J' wenn Erlebensfallsumme ändert

           05  'XX'-TODFALL-SUM             PIC  9(008)V99.
           05  'XX'-TODFALL-SUM-AENDKZ      PIC  X(001).
      *        'J' wenn Todesfallsumme ändert

           05  'XX'-RNTFAKTOR               PIC  9(005)V99.
           05  'XX'-RNTFAKTOR-AENDKZ        PIC  X(001).
      *        'J' wenn monatlich garantierter Rentenfaktor ändert

           05  'XX'-KAPABFIND               PIC  9(008)V99.
           05  'XX'-KAPABFIND-AENDKZ        PIC  X(001).
      *        'J' wenn Kapitalabfindung ändert

           05  'XX'-FONDSGUTH               PIC  9(008)V99.
           05  'XX'-FONDSGUTH-AENDKZ        PIC  X(001).
      *        'J' wenn Fondsguthaben ändert

           05  FILLER                       PIC  X(058).
      *---------------------------------------------------------------*