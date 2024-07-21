      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1339.CPY  $
      *    $Revision:   2.17  $
      *
      *    $Log:   L:/mf/CPY/DRKI1339.CPv  $
      *
      *   Rev 2.17   Apr 08 2020 14:40:32   u130685
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.16   Mar 17 2020 09:51:46   u130685
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.15   Feb 19 2020 10:50:16   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.14   Feb 19 2020 10:12:04   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.13   Feb 19 2020 07:57:02   i13az84
      *AKI01726: G48 Umstellung KSDS -> Temp. Table FSA
      *
      *   Rev 2.12   May 23 2017 15:40:26   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.11   May 23 2017 15:34:56   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.10   May 23 2017 15:06:42   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.9   Jun 28 2011 09:55:08   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g FSA
      *
      *   Rev 2.8   Nov 16 2009 18:41:26   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.7   Nov 13 2009 10:48:24   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.6   Nov 10 2009 15:50:28   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.5   Nov 04 2009 14:53:24   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.4   Nov 04 2009 14:45:06   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.3   Nov 03 2009 10:47:18   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.2   Jan 27 2003 10:57:36   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 2.1   Jul 30 2002 16:33:48   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Dec 14 2001 09:21:16   p13sp20
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR FSA ÄNDERUNGSBESTAND DWS                    *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Länge = 107 bytes                                           *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00274!        !01  ! NEU                       !W.Hornung *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
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
      * -------------------------------------------------------------*
      * AKI01726!19.02.20! KEINE AENDERUNG                !M.ATAR    *
      *                    AUSGABE COPY FUER TT                      *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1339.
      *
               05  BNK-SA                       PIC  X(001).
      *            SATZART        = '4'
               05  BNK-GES                      PIC  9(002).
      *            GESELLSCHAFT
               05  BNK-VERARB-KZ                PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = Insert
      *                U = Update
               05  BNK-VNR.
                   25  BNK-FILIAL-NR            PIC 9(03).
                   25  BNK-KDNR                 PIC 9(07).
      *            VERTRAGSNUMMER
               05  BNK-VBNR                     PIC  9(007).
      *            ABSCHLUSSVERMITTLER
               05  BNK-VBNR-KZ                  PIC  X(001).
      *            ÄNDERUNGSKENNZEICHEN ABSCHLUSSVERMITTLER
               05  BNK-GUELTIG-AB-DAT           PIC  9(008).
      *            FSA GÜLTIG AB
               05  BNK-GUELTIG-AB-DAT-KZ        PIC  X(001).
      *            ÄNDERUNGSKENNZEICHEN FSA GÜLTIG AB
               05  BNK-GUELTIG-BIS-DAT          PIC  9(008).
      *            FSA GÜLTIG BIS
               05  BNK-GUELTIG-BIS-DAT-KZ       PIC  X(001).
      *            ÄNDERUNGSKENNZEICHEN FSA GÜLTIG BIS
               05  BNK-FSA-GEMELDET             PIC  9(005)V99.
      *            FSA GEMELDET
               05  BNK-FSA-GEMELDET-KZ          PIC  X(001).
      *            ÄNDERUNGSKENNZEICHEN FSA GEMELDET
               05  BNK-FSA-VERBRAUCHT           PIC  9(005)V99.
      *            FSA VERBRAUCHT
               05  BNK-FSA-VERBRAUCHT-KZ        PIC  X(001).
      *            ÄNDERUNGSKENNZEICHEN FSA VERBRAUCHT
               05  BNK-STUECKZINSEN-LFD-J       PIC  S9(010)V99.
      *            STÜCKZINSEN lfd. JAHR
               05  BNK-STUECKZINSEN-LFD-J-KZ    PIC  X(001).
      *            ÄNDERUNGSKENNZEICHEN STÜCKZINSEN lfd. JAHR
               05  BNK-STEUERAUSL               PIC  X(001).
      *            Steuerausländerkennzeichen der Kundenstammnummer
               05  BNK-STEUERAUSL-KZ            PIC  X(001).
      *            ÄNDERUNGSKZ. SteuerausländerKZ Kundenstammnummer
               05  BNK-NICHTVERANL-BIS-DAT      PIC  9(008).
      *            Nichtveranlagungsbescheinigung - gültig bis
               05  BNK-NICHTVERANL-BIS-DAT-KZ   PIC  X(001).
      *            ÄNDERUNGSKZ. Nichtveranlagungsbe. - gültig bis
               05  BNK-AUSG-FSA-IM-VJ           PIC  9(005)V99.
      *            ausgenutzter Freistellungsbetrag im Vorjahr in EUR
               05  BNK-AUSG-FSA-IM-VJ-KZ        PIC  X(001).
      *            ÄNDERUNGSKZ. Freistellungsbetrag im Vorjahr in EUR
               05  BNK-AUFGEL-ERTRAG            PIC  9(007)V99.
      *            aufgelaufener Ertrag gesamt aktuelles Jahr
      *            (kumuliert)
               05  BNK-AUFGEL-ERTRAG-KZ         PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            aufgelaufener Ertrag gesamt aktuelles Jahr
      *            (kumuliert)
      *        05  BNK-EINBEH-KEST              PIC  9(005)V99.
               05  BNK-EINBEH-KEST              PIC  9(007)V99.
      *            einbehaltene KEST aktuelles Jahr (kumuliert)
               05  BNK-EINBEH-KEST-KZ           PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            einbehaltene KEST aktuelles Jahr (kumuliert)
               05  BNK-EINBEH-SOLI              PIC  9(005)V99.
      *            einbehaltener Soli aktuelles Jahr (kumuliert)
               05  BNK-EINBEH-SOLI-KZ           PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            einbehaltener Soli aktuelles Jahr (kumuliert)
               05  BNK-EINBEH-KIST              PIC  9(005)V99.
      *            einbehaltene Kirchensteuer aktuelles Jahr
      *            (kumuliert)
               05  BNK-EINBEH-KIST-KZ           PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            einbehaltene Kirchensteuer aktuelles Jahr
      *            (kumuliert)
               05  BNK-VERLUST-AKTIEN           PIC  9(007)V99.
      *            Verlusttopf Aktien aktueller balance-Wert
               05  BNK-VERLUST-AKTIEN-KZ        PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            Verlusttopf Aktien aktueller balance-Wert
               05  BNK-VERLUST-SONSTIGE         PIC  9(007)V99.
      *            Verlusttopf Sonstige aktueller balance-Wert
               05  BNK-VERLUST-SONSTIGE-KZ      PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            Verlusttopf Sonstige aktueller balance-Wert
               05  BNK-ANRECHEN-QUELLENST       PIC  9(007)V99.
      *            Topf anrechenbare Quellen-steuer aktueller
      *            balance-Wert
               05  BNK-ANRECHEN-QUELLENST-KZ    PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            Topf anrechenbare Quellen-steuer aktueller
      *            balance-Wert
               05  BNK-VERL-BESCH-AKTIEN        PIC  X(001).
      *            Verlustbescheinigung zum Aktientopf für
      *            aktuelles Jahr angefordert J/N
               05  BNK-VERL-BESCH-AKTIEN-KZ     PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            Verlustbescheinigung zum Aktientopf für
      *            aktuelles Jahr
               05  BNK-VERL-BESCH-SONSTIGE      PIC  X(001).
      *            Verlustbescheinigung zum Sonstige für
      *            aktuelles Jahr
               05  BNK-VERL-BESCH-SONSTIGE-KZ   PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            Verlustbescheinigung zum Sonstige für
      *            aktuelles Jahr
               05  BNK-ZUG-KONFESSION-1         PIC  X(002).
      *            Zugehörigkeit Konfession 1
               05  BNK-ZUG-KONFESSION-1-KZ      PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            Zugehörigkeit Konfession 1
               05  BNK-ZUG-KONFESSION-2         PIC  X(002).
      *            Zugehörigkeit Konfession 2
               05  BNK-ZUG-KONFESSION-2-KZ      PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            Zugehörigkeit Konfession 2
               05  BNK-KIST-SATZ1               PIC  9(002)V99.
      *            Kirchensteuer-Satz 1
               05  BNK-KIST-SATZ1-KZ            PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            Kirchensteuer-Satz 1
               05  BNK-KIST-SATZ2               PIC  9(002)V99.
      *            Kirchensteuer-Satz 2
               05  BNK-KIST-SATZ2-KZ            PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            Kirchensteuer-Satz 2
               05  BNK-EINB-KIST-AB             PIC  9(008).
      *            Einbehalt der Kirchensteuer ab JHJJMMTT
               05  BNK-EINB-KIST-AB-KZ          PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            Einbehalt der Kirchensteuer ab JHJJMMTT
               05  BNK-AUFT-MASSST-KONF1        PIC  9(003).
      *            Aufteilungsmaßstab
      *            Konfessionszugehörigkeit 1 in %
               05  BNK-AUFT-MASSST-KONF1-KZ     PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            Aufteilungsmaßstab
      *            Konfessionszugehörigkeit 1 in %
               05  BNK-AUFT-MASSST-KONF2        PIC  9(003).
      *            Aufteilungsmaßstab
      *            Konfessionszugehörigkeit 2 in %
               05  BNK-AUFT-MASSST-KONF2-KZ     PIC  X(001).
      *            ÄNDERUNGSKZ.
      *            Aufteilungsmaßstab
      *            Konfessionszugehörigkeit 2 in %
      ****     05  FILLER                       PIC X(041).
               05  BNK-FSA-NUMMER               PIC X(30).
               05  BNK-FSA-NUMMER-KZ            PIC X.
      *        05  FILLER                       PIC X(10).
               05  FILLER                       PIC X(08).
      *            RESERVE









