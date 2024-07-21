      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0144.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRAN0144.CPv  $
      *
      *   Rev 2.13   May 18 2018 17:23:28   t13ae34
      *AAN01368: AM-Leben: Neue ungezillmerte bAV-Tarife
      *
      *   Rev 2.12   May 18 2018 14:54:26   T13AE34
      *AAN01368: AM-Leben: Neue ungezillmerte bAV-Tarife
      *
      *   Rev 2.11   May 17 2018 11:04:32   t13ae34
      *AAN01368: AM-Leben: Neue ungezillmerte bAV-Tarife
      *
      *   Rev 2.10   May 17 2018 10:39:52   t13ae23
      *AAN01356: FW: Provisionsverteilung- und Abrechnung - Teil An
      *trag Teil 2-
      *
      *   Rev 2.9   Mar 09 2007 14:27:16   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.8   Mar 07 2007 19:50:16   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.7   Mar 07 2007 19:02:10   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.6   Feb 13 2007 19:32:40   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.5   Feb 13 2007 18:32:08   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.4   Feb 13 2007 17:24:58   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   03 Jan 2007 18:54:34   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   22 Dec 2006 20:47:30   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Oct 06 2006 11:08:34   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Sep 30 2006 11:57:00   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0144                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    ZWISCHEN ANTRAGSDIALOGEN                                   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!29.09.06!2.1 ! NEU                       ! HEIDER   *
      * AAN00655!12.10.06!2.1 ! WEITERE FELDER            ! HEIDER   *
      * AAN00655!02.03.07!2.7 ! BEARBEITUNGSGEBÜHR        ! HEIDER   *
      * AAN00655!09.03.07!2.9 ! EXTERNE VERTRAGSNUMMR     ! HEIDER   *
      * AAN01356!17.05.18!2.10! ALLFINANZ-AG              ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DEN VARIABLEN KOMMUNIKATIONSBEREICH
      *    ZWISCHEN DIALOGEN
      *    - INPUT (VON AUFRUFENDEM DIALOG)
      *      ACHTUNG: AUF CADD-I-KOMM-VAR DARF NUR BEI
      *      !!!      DIALOG-INIT ZUGEGRIFFEN WERDEN; FALLS
      *      !!!      DIE INFOS SPAETER NOCH WICHTIG SIND,
      *      !!!      MUESSEN SIE IN CADI-SICHERUNG GESICHERT
      *      !!!      WERDEN.
      *      !!!      GRUND: DAS FELD WIRD EVTL. IN AUFGERUFENEN
      *      !!!      UNTERANWENDUNGEN UEBERSCHRIEBEN
      *
      ****************************************************************

       01  PI-DRAN0144.
           05  PI-DRAN0144-CADD-I-KOMM-VAR            PIC  X(128).
           05  FILLER REDEFINES   PI-DRAN0144-CADD-I-KOMM-VAR.

               10  PI-DRAN0144-FUNKTION               PIC  X(008).
                   88 PI-DRAN0144-CADD                VALUE 'DRAN0144'.

               10  PI-DRAN0144-VERARBEITUNG           PIC  X(001).
                   88 PI-DRAN0144-NEUANLAGE           VALUE 'N'.
                   88 PI-DRAN0144-AENDERUNG           VALUE 'U'.
                   88 PI-DRAN0144-ANZEIGE             VALUE 'B'.

      ***      STEUERINFOS
               10  PI-DRAN0144-CADD-STEUER-INFO       PIC  X(008).
                   88 PI-DRAN0144-CADD-ERSTAUFRUF     VALUE 'FIRST'.
                   88 PI-DRAN0144-CADD-FOLGEAUFRUF    VALUE 'NEXT'.

      ***      TRANSAKTION
               10  PI-DRAN0144-TRANS                  PIC  X(004).

      ***      AUFZURUFENDE KEY-MASKE
               10  PI-DRAN0144-KEY-MAP                PIC  X(007).

      *        VERTRIEBSGESELLSCHAFT
               10  PI-DRAN0144-VERTRIEBSGES           PIC  X(001).
170518             88  PI-DRAN0144-VG-ALLFINANZ       VALUE 'A'.
170518             88  PI-DRAN0144-VG-ALLFINANZ-AG    VALUE 'G'.
                   88  PI-DRAN0144-VG-DVAG            VALUE 'D'.
                   88  PI-DRAN0144-VG-DVAG-WIEN       VALUE 'W'.

      *        PIN ODER VNR VERARBEITUNG
               10  PI-DRAN0144-PIN-VNR                PIC  X(001).
                   88  PI-DRAN0144-PIN                VALUE 'P'.
                   88  PI-DRAN0144-VNR                VALUE 'V'.

      *        PRODUKTGEBER: EIN ODER MEHRERE PRODUKTE JE ANTRAG
               10  PI-DRAN0144-ANTRAG-PROD            PIC  X(001).
                   88  PI-DRAN0144-1-PROD             VALUE '1'.
                   88  PI-DRAN0144-N-PROD             VALUE 'N'.

      *        PRODUKTGEBER: MIT SUMME ODER BEITRAG
               10  PI-DRAN0144-SUMME-ODER-BEITRAG     PIC  X(001).
                   88  PI-DRAN0144-SUMME              VALUE 'S'.
                   88  PI-DRAN0144-BEITRAG            VALUE 'B'.

      *        PRODUKTGEBER: MIT/OHNE LAUFZEITABHÄNGIGE PRODUKTE
               10  PI-DRAN0144-LAUFZEITABHAENGIG      PIC  X(001).
                   88  PI-DRAN0144-LZ-JA              VALUE 'J'.
                   88  PI-DRAN0144-LZ-NEIN            VALUE 'N'.


      ***      AUFZURUFENDE DATEN-MASKE
               10  PI-DRAN0144-DAT-MAP                PIC  X(007).

      ***      ZUSÄTZLICHE DATEN, DIE NICHT GESPEICHERT WERDEN

      *        VB-ZEICHNER FÜR KONZESSION BEI NEUANLAGE
               10  PI-DRAN0144-VB-ZEICHNER            PIC  X(007).
      *        ERSTERFASSER BEI ÄNDERUNG
               10  PI-DRAN0144-ERST-ERFASSER REDEFINES
                   PI-DRAN0144-VB-ZEICHNER            PIC  X(007).

      *        ANTRAG EINMALZAHLWEISE
               10  PI-DRAN0144-ZAHLWEISE              PIC  X(001).
                   88  PI-DRAN0144-EINMALZAHLWEISE    VALUE 'E'.
                   88  PI-DRAN0144-ANDERE-ZAHLWEISE   VALUE 'A'.

      ***      PRODUKTSCHLÜSSEL AUS KEY-MASKE
               10  PI-DRAN0144-PROD-PROD-SCHL         PIC  9(004).

      ***      TARIF AUS KEY-MASKE
               10  PI-DRAN0144-TARIF                  PIC  X(015).

      ***      SPARTE BEI AML-TARIF
               10  PI-DRAN0144-SPARTE-AML             PIC  X(001).

      ***      TS-QUEUE NAME FÜR DATENÜBERGABE WORKFLOW ANTRAG
               10  PI-DRAN0144-TSQ-NAME-WORKFLOW      PIC  X(008).

      ***      TS-QUEUE NAME FÜR DUPLIZIERDATEN
      ***      NICHT LEER, WENN DATEN VORHANDEN
               10  PI-DRAN0144-TSQ-NAME-DUP           PIC  X(008).

      ***      TS-QUEUE NAME FÜR MATCHCODEDATEN
      ***      NICHT LEER, WENN DATEN VORHANDEN
               10  PI-DRAN0144-TSQ-NAME-MC            PIC  X(008).

      ***      TS-QUEUE ANZAHL MATCHCODEDATEN
               10  PI-DRAN0144-TSQ-ANZA-MC            PIC  9(004).

      ***      TS-QUEUE NAME FÜR HISTORIE-DATEN
      ***      NICHT LEER, WENN DATEN VORHANDEN
               10  PI-DRAN0144-TSQ-NAME-HISTORIE      PIC  X(008).

      ***      TS-QUEUE ANZAHL HISTORIE-DATEN
               10  PI-DRAN0144-TSQ-ANZA-HISTORIE      PIC  9(004).

      *        PRODUKTGEBER: MIT/OHNE BEARBEITUNGSGEBÜHR
               10  PI-DRAN0144-BEARBEITUNGSGEB        PIC  X(001).
                   88  PI-DRAN0144-BGEB-JA            VALUE 'J'.
                   88  PI-DRAN0144-BGEB-NEIN          VALUE 'N'.

      ***      EXTERNE VERTRAGSNUMMR
               10  PI-DRAN0144-VSNR                   PIC  9(015)
                                                           COMP-3.

      ***
               10  FILLER                             PIC  X(011).

      *
       01  PO-DRAN0144.
           05  PO-DRAN0144-CADD-O-KOMM-VAR            PIC  X(128).
           05  FILLER REDEFINES   PO-DRAN0144-CADD-O-KOMM-VAR.

      ***      HIER WIRD DAS ERGEBNIS DER LETZTEN AKTION ZURÜCKGEGEBEN
               10  PO-DRAN0144-CADD-ERGEBNIS          PIC  X(001).
                   88 PO-DRAN0144-CADD-KEIN-ERGEBNIS    VALUE '0'.
                   88 PO-DRAN0144-CADD-NEUANLAGE        VALUE 'N'.
                   88 PO-DRAN0144-CADD-AENDERUNG        VALUE 'A'.
                   88 PO-DRAN0144-CADD-LOESCHUNG        VALUE 'L'.
                   88 PO-DRAN0144-CADD-ABBRUCH          VALUE 'X'.

      ******* ENDE COPY-MEMBER DRAN0144  ****************************




