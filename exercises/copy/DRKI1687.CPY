      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1687.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI1687.CPv  $
      *
      *   Rev 2.7   May 23 2017 15:40:44   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.6   May 23 2017 15:35:12   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.5   May 23 2017 15:07:42   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.4   Jul 15 2011 08:54:44   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g FSA
      *
      *   Rev 2.3   Dec 14 2009 18:27:02   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.2   Mar 16 2005 11:46:20   t13ex39
      *FKI00922: DeuBa Hist. KSDS Tagesdatum
      *
      *   Rev 2.1   Jul 05 2004 10:34:28   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.0   Jun 17 2004 10:38:26   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR KSDS FSA DEUTSCHE BANK HISTORIE             *
      *    ----------------------------------------------------------  *
      *                                                                *
240511*    Länge = 252 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00082!11.06.04!01  ! NEU                       !W.Hornung  *
      *        !        !    !                           !           *
      *--------!-----------------------------------------!-----------*
      *FKI00992!10.03.05!02  !Tagesdatum der Löschung    !WHO        *
      *---------!-------!----!---------------------------!-----------*
261009*AKI00866!26.10.09!    ! neue Felder für           ! HRO       *
      *        !        !    ! Abgeltungssteuer          !           *
      *---------!-------!----!---------------------------!-----------*
240511*AKI01040!24.05.11!    ! neues Feld FSA-Nr.        ! HHU      *
      *--------------------------------------------------------------*
240417* AKI01512!24.04.17! Feld DB-EINBEH-KEST um 2       !WHO       *
      *         !        ! Stellen erweitert PIC 9(07)    !          *
      *         !        !                                !          *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1687.
      *
               05  KSDS-SA                  PIC  X(001).
      *            SATZART        = '4'
               05  KSDS-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '48'
               05  KSDS-KEY.
                   15  KSDS-VNR.
                       25  KSDS-FILIAL-NR   PIC 9(03).
                       25  KSDS-KDNR        PIC 9(07).
      *            VERTRAGSNUMMER
                   15  KSDS-LFDNR           PIC  9(002).
      *                LFD. NR. FÜR PRIMARY-KEY
               05  KSDS-VBNR                PIC  9(007).
      *            ABSCHLUSSVERMITTLER
               05  KSDS-GUELTIG-AB-DAT      PIC  9(008).
      *            FSA GÜLTIG AB
               05  KSDS-GUELTIG-BIS-DAT     PIC  9(008).
      *            FSA GÜLTIG BIS
               05  KSDS-FSA-GEMELDET        PIC  9(005)V99.
      *            FSA GEMELDET
               05  KSDS-FSA-VERBRAUCHT      PIC  9(005)V99.
      *            FSA VERBRAUCHT
               05  KSDS-STUECKZINSEN-LFD-J  PIC  S9(010)V99.
      *            STÜCKZINSEN lfd. JAHR

               05  KSDS-DEPOT-AKTIV         PIC  9(001).
...   *              Kennz. = 1, Kunde hat aktives Depot
...   *              Kennz. = 0, Kunde hat kein Depot
...   *              Kennz. = 9, Kennz. ist noch nicht geliefert worden
...
100305         05  KSDS-DEL-KZ              PIC  X(001).
100305         05  KSDS-AEND-DAT            PIC  X(010).
100305*            ÄNDERUNGSDATUM

261009         05  KSDS-STEUERAUSL          PIC  X(001).
....  *              Steuerausländerkennzeichen der Kundenstammnummer
      *              J/N
               05  KSDS-NICHTVERANL-BIS-DAT PIC  9(008).
      *              Nichtveranlagungsbescheinigung - gültig bis
      *              JHJJMMTT
               05  KSDS-AUSG-FSA-IM-VJ      PIC  9(005)V99.
      *              ausgenutzter Freistellungsbetrag im Vorjahr in EUR
               05  KSDS-AUFGEL-ERTRAG       PIC  9(007)V99.
      *              aufgelaufener Ertrag gesamt aktuelles Jahr
      *              (kumuliert)
240417*        05  KSDS-EINBEH-KEST         PIC  9(005)V99.
240417         05  KSDS-EINBEH-KEST         PIC  9(007)V99.
      *              einbehaltene KEST aktuelles Jahr (kumuliert)
               05  KSDS-EINBEH-SOLI         PIC  9(005)V99.
      *              einbehaltener Soli aktuelles Jahr (kumuliert)
               05  KSDS-EINBEH-KIST         PIC  9(005)V99.
      *              einbehaltene Kirchensteuer aktuelles Jahr
      *              (kumuliert)
               05  KSDS-VERLUST-AKTIEN      PIC  9(007)V99.
      *              Verlusttopf Aktien aktueller balance-Wert
               05  KSDS-VERLUST-SONSTIGE    PIC  9(007)V99.
      *              Verlusttopf Sonstige aktueller balance-Wert
               05  KSDS-ANRECHEN-QUELLENST  PIC  9(007)V99.
      *              Topf anrechenbare Quellen-steuer aktueller
      *              balance-Wert
               05  KSDS-VERL-BESCH-AKTIEN   PIC  X(001).
      *              Verlustbescheinigung zum Aktientopf für
      *              aktuelles Jahr angefordert J/N
               05  KSDS-VERL-BESCH-SONSTIGE PIC  X(001).
      *              Verlustbescheinigung zum Sonstige für
      *              aktuelles Jahr angefordert J/N
               05  KSDS-ZUG-KONFESSION-1    PIC  X(002).
      *              Zugehörigkeit Konfession 1
               05  KSDS-ZUG-KONFESSION-2    PIC  X(002).
      *              Zugehörigkeit Konfession 2
               05  KSDS-KIST-SATZ1          PIC  9(002)V99.
      *              Kirchensteuer-Satz 1
               05  KSDS-KIST-SATZ2          PIC  9(002)V99.
      *              Kirchensteuer-Satz 2
               05  KSDS-EINB-KIST-AB        PIC  9(008).
      *              Einbehalt der Kirchensteuer ab JHJJ
               05  KSDS-AUFT-MASSST-KONF1   PIC  9(003).
      *              Aufteilungsmaßstab
      *              Konfessionszugehörigkeit 1 in %
               05  KSDS-AUFT-MASSST-KONF2   PIC  9(003).
...   *              Aufteilungsmaßstab
261009*              Konfessionszugehörigkeit 2 in %
240511         05  KSDS-FSA-NUMMER             PIC X(30).
240417*        05  KSDS-RESERVE             PIC  X(045).
240417         05  KSDS-RESERVE             PIC  X(043).



