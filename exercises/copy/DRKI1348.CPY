      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1348.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/DRKI1348.CPv  $
      *
      *   Rev 2.12   May 23 2017 15:40:32   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.11   May 23 2017 15:35:02   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.10   May 23 2017 15:06:56   t13ex39
      *AKI01512: DeuBa Mai-Release
      *
      *   Rev 2.9   Jul 15 2011 08:54:38   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g FSA
      *
      *   Rev 2.8   Jun 28 2011 09:55:02   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g FSA
      *
      *   Rev 2.7   Nov 16 2009 18:41:32   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.6   Nov 13 2009 10:48:32   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.5   Nov 12 2009 13:32:54   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Anpassen Ermittlung
      *
      *   Rev 2.4   Mar 16 2005 11:45:40   t13ex39
      *FKI00922: DeuBa Hist. KSDS Tagesdatum
      *
      *   Rev 2.3   Feb 25 2003 15:45:32   t13ex12
      *AKI00328 KSDS-Kunden ohne Anlagedepot
      *
      *   Rev 2.2   Jan 27 2003 10:58:08   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 2.1   Jul 30 2002 16:35:02   t13ex39
      *AKI00274 DB/24
      *
      *   Rev 2.0   Dec 14 2001 09:22:24   p13sp20
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR KSDS FSA          VON DB24                  *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Länge = 250 bytes                                           *
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
      * AKI00162!14.11.02!02  ! Stückzinsen mit Vorzeichen!W.Hornung *
      * -------------------------------------------------------------*
      * AKI00328!17.02.03!03  ! Neues Kennz. für Kunden   !v.d.Goltz *
      *         !        !    ! Depot.                    !          *
      *---------!-----------------------------------------!------------*
      *FKI00992 !10.03.05!04  ! wegen Tagesdatum der      !Hornung     *
      *         !        !    ! Löschung in hist. KSDS    !            *
      *         !        !    ! DEL-KZ mit aufgenommen    !            *
      *         !        !    ! wegen MOVE CORR           !            *
      * -------------------------------------------------------------*
      * AKI00866!26.10.09!05  ! neue Felder für           ! HRO      *
      *         !        !    ! Abgeltungssteuer          !          *
      * -------------------------------------------------------------*
      * AKI01040!24.05.11!06  ! neues Feld FSA-Nr.        ! HHU      *
      *         !        !    !                           !            *
      *--------------------------------------------------------------*
      * AKI01512!24.04.17! Feld DB-EINBEH-KEST um 2       !WHO       *
      *         !        ! Stellen erweitert PIC 9(07)    !          *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1348.
      *
               05  KSDS-SA                  PIC  X(001).
      *            SATZART        = '4'
               05  KSDS-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '48'
               05  KSDS-KEY.
                   15  KSDS-VNR.
                       25  KSDS-FILIAL-NR        PIC 9(03).
                       25  KSDS-KDNR             PIC 9(07).
      *            VERTRAGSNUMMER
               05  KSDS-VNR-1 REDEFINES KSDS-KEY PIC 9(10).
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
               05  KSDS-DEL-KZ              PIC  X(001).

               05  KSDS-STEUERAUSL          PIC  X(001).
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
      *        05  KSDS-EINBEH-KEST         PIC  9(005)V99.
               05  KSDS-EINBEH-KEST         PIC  9(007)V99.
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
      *              Aufteilungsmaßstab
...   *              Konfessionszugehörigkeit 2 in %
      ****     05  FILLER                 PIC  X(35).
               05  KSDS-FSA-NUMMER             PIC X(30).
      *        05  FILLER                 PIC X(55).
               05  FILLER                 PIC X(53).




