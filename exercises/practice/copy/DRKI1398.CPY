      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1398.CPY  $
      *    $Revision:   2.15  $
      *
      *    $Log:   L:/mf/CPY/DRKI1398.CPv  $
      *
      *   Rev 2.15   May 23 2017 15:40:38   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.14   May 23 2017 15:35:06   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.13   May 23 2017 15:06:50   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.12   Jun 28 2011 09:55:18   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g FSA
      *
      *   Rev 2.11   Nov 16 2009 18:41:40   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.10   Nov 13 2009 10:48:40   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.9   Nov 11 2009 16:19:30   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.8   Nov 11 2009 16:17:16   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.7   Nov 10 2009 15:50:38   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.6   Nov 05 2009 10:37:32   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.5   Nov 04 2009 14:53:32   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.4   Nov 04 2009 10:34:58   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.3   Nov 03 2009 10:47:28   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.2   Jan 27 2003 10:58:22   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 2.1   Jul 30 2002 16:35:32   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Apr 09 2002 08:33:16   p13sp16
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      *                                                              *
      *    COPY-MEMBER FÜR FSA AKTUALISIERUNG DEUTSCHE BANK          *
      *    --------------------------------------------------------  *
      *                                                              *
      *    DATEITYP  = QSAM                                          *
      *    Länge    100 bytes                                        *
      *                                                              *
      *--------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00274!        !01  ! NEU                       !W.Hornung *
      * -------------------------------------------------------------*
      * AKI00162!14.11.02!01.2! Neue Felder               !WHO       *
      *         !        !    ! ZÜCKZINSEN                !          *
      * -------------------------------------------------------------*
      * AKI00866!26.10.09!05  ! neue Felder für           ! HRO      *
      *         !        !    ! Abgeltungssteuer          !          *
      * -------------------------------------------------------------*
      * AKI01040!24.05.11!06  ! neues Feld FSA-Nr.        ! HHU      *
      *         !        !    !                           !          *
      *--------------------------------------------------------------*
      * AKI01512!24.04.17! Feld DB-EINBEH-KEST um 2       !WHO       *
      *         !        ! Stellen erweitert PIC 9(07)    !          *
      *         !        !                                !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1398.
      *
               20  FA-GES                        PIC  9(002).
      *            GESELLSCHAFT
               20  FA-VNR                        PIC  9(015).
      *            VERTRAGSNUMMER
               20  FA-GUELTIG-AB-DAT             PIC  X(010).
               20  FA-GUELTIG-AB-DAT-KZ          PIC  X(001).
      *            FSA GÜLTIG AB
               20  FA-GUELTIG-BIS-DAT            PIC  X(010).
               20  FA-GUELTIG-BIS-DAT-KZ         PIC  X(001).
      *            FSA GÜLTIG BIS
               20  FA-FSA-GEMELDET               PIC  9(005)V99.
               20  FA-FSA-GEMELDET-KZ            PIC  X(001).
      *            FSA GEMELDET
               20  FA-FSA-VERBRAUCHT             PIC  9(005)V99.
               20  FA-FSA-VERBRAUCHT-KZ          PIC  X(001).
      *            FSA VERBRAUCHT
               20  FA-STUECKZINSEN-LFD-J         PIC  9(010)V99.
               20  FA-STUECKZINSEN-LFD-J-KZ      PIC  X(001).
      *            STÜCKZINSEN lfd. JAHR
               20  FA-STEUERAUSL                 PIC  X(001).
               20  FA-STEUERAUSL-KZ              PIC  X(001).
...   *            Steuerausländerkennzeichen der Kundenstammnummer
               20  FA-NICHTVERANL-BIS-DAT         PIC  X(010).
               20  FA-NICHTVERANL-BIS-DAT-KZ      PIC  X(001).
      *            Nichtveranlagungsbescheinigung - gültig bis
               20  FA-AUSG-FSA-IM-VJ             PIC  9(005)V99.
               20  FA-AUSG-FSA-IM-VJ-KZ          PIC  X(001).
      *            ausgenutzter Freistellungsbetrag im Vorjahr in EUR
               20  FA-AUFGEL-ERTRAG              PIC  9(007)V99.
               20  FA-AUFGEL-ERTRAG-KZ           PIC  X(001).
      *            aufgelaufener Ertrag gesamt aktuelles Jahr
      *            (kumuliert)
      *        20  FA-EINBEH-KEST                PIC  9(005)V99.
               20  FA-EINBEH-KEST                PIC  9(007)V99.
               20  FA-EINBEH-KEST-KZ             PIC  X(001).
      *            einbehaltene KEST aktuelles Jahr (kumuliert)
               20  FA-EINBEH-SOLI                PIC  9(005)V99.
               20  FA-EINBEH-SOLI-KZ             PIC  X(001).
      *            einbehaltener Soli aktuelles Jahr (kumuliert)
               20  FA-EINBEH-KIST                PIC  9(005)V99.
               20  FA-EINBEH-KIST-KZ             PIC  X(001).
      *            einbehaltene Kirchensteuer aktuelles Jahr
      *            (kumuliert)
               20  FA-VERLUST-AKTIEN             PIC  9(007)V99.
               20  FA-VERLUST-AKTIEN-KZ          PIC  X(001).
      *            Verlusttopf Aktien aktueller balance-Wert
               20  FA-VERLUST-SONSTIGE           PIC  9(007)V99.
               20  FA-VERLUST-SONSTIGE-KZ        PIC  X(001).
      *            Verlusttopf Sonstige aktueller balance-Wert
               20  FA-ANRECHEN-QUELLENST         PIC  9(007)V99.
               20  FA-ANRECHEN-QUELLENST-KZ      PIC  X(001).
      *            Topf anrechenbare Quellen-steuer aktueller
      *            balance-Wert
               20  FA-VERL-BESCH-AKTIEN          PIC  X(001).
               20  FA-VERL-BESCH-AKTIEN-KZ       PIC  X(001).
      *            Verlustbescheinigung zum Aktientopf für
      *            aktuelles Jahr angefordert J/N
               20  FA-VERL-BESCH-SONSTIGE         PIC  X(001).
               20  FA-VERL-BESCH-SONSTIGE-KZ      PIC  X(001).
      *            Verlustbescheinigung zum Sonstige für
      *            aktuelles Jahr
               20  FA-ZUG-KONFESSION-1           PIC  X(002).
               20  FA-ZUG-KONFESSION-1-KZ        PIC  X(001).
      *            Zugehörigkeit Konfession 1
               20  FA-ZUG-KONFESSION-2           PIC  X(002).
               20  FA-ZUG-KONFESSION-2-KZ        PIC  X(001).
      *            Zugehörigkeit Konfession 2
               20  FA-KIST-SATZ1                 PIC  9(002)V99.
               20  FA-KIST-SATZ1-KZ              PIC  X(001).
      *            Kirchensteuer-Satz 1
               20  FA-KIST-SATZ2                 PIC  9(002)V99.
               20  FA-KIST-SATZ2-KZ              PIC  X(001).
      *            Kirchensteuer-Satz 2
               20  FA-EINB-KIST-AB               PIC  X(010).
               20  FA-EINB-KIST-AB-KZ            PIC  X(001).
      *            Einbehalt der Kirchensteuer ab JHJJMMTT
               20  FA-AUFT-MASSST-KONF1          PIC  9(003).
               20  FA-AUFT-MASSST-KONF1-KZ       PIC  X(001).
      *            Aufteilungsmaßstab
      *            Konfessionszugehörigkeit 1 in %
               20  FA-AUFT-MASSST-KONF2          PIC  9(003).
               20  FA-AUFT-MASSST-KONF2-KZ       PIC  X(001).
...   *            Aufteilungsmaßstab
      *            Konfessionszugehörigkeit 2 in %
               20  FA-FSA-NUMMER                 PIC X(30).
               20  FA-FSA-NUMMER-KZ              PIC X(001).
      **           INTERNE FREISTELLUNGSNUMMER
               20  FA-AKTUAL-KZ                  PIC X(001).
      *            I = INSERT, NUR FUER NEUE VERTRAEGE
      *            U = UPDATE
      *            D = DELETE
      ***      20  FILLER                        PIC  X(037).
      *        20  FILLER                        PIC  X(006).
               20  FILLER                        PIC  X(004).
      *            RESERVE
      ******************************************************************



