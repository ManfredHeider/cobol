      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  03.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3798.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3798.CPv  $
      *
      *   Rev 2.2   Jul 31 2020 14:19:26   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source
      *
      *   Rev 2.1   Jul 01 2020 10:23:18   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source
      *
      *   Rev 2.0   Jun 03 2020 11:02:18   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    G50/GEDKRANKEN DTA Aktion(s-Tool)
      *    (Membername bei der GDIS: DRDLK582)
      *
      *    Satzlänge: 400 Byte
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01784!03.06.20!2.1 !NEU                        !T13AC20
      * AKI01784!29.07.20!2.2 !NEUES FELD SA20-DGBT       !T13AC20
      *         !        !    !                           !
      *****************************************************************
      *
      **  Definition als Anker für die folgenden REDEFINES
           05 SATZ000.
              10                                 PIC  X(400).
      *
      **  Vertrag mit Angebotsinformationen 'C010'
      **  Kann nur 1 x je VertragsNr. vorhanden sein.
           05 SATZ010  REDEFINES SATZ000.
              10 SA10-KEY.
      ** 'C' Konstant
                 15 SA10-SATZART                 PIC  X(001).
      ** '010' Konstant
                 15 SA10-DATENSATZART            PIC  X(003).
      ** Angebotsname
                 15 SA10-ANGEBOTSNAME            PIC  X(010).
      ** Vertragsnummer
                 15 SA10-VTRGNR                  PIC  9(010).
      ***
              10 SA10-VERTRAGSSATZ.
      ** Aktionsstichtag / Umstellungstermin (TT.MM.JJJJ)
                 15 SA10-DATUM                   PIC  X(010).
      ** Nachname  VN
                 15 SA10-NNAME-VN                PIC  X(040).
      ** Vorname   VN
                 15 SA10-VNAME-VN                PIC  X(025).
      ** Anzahl Personen VPE
                 15 SA10-APER                    PIC  9(002).
      ** Annahmefrist       bis (TT.MM.JJJJ)    [DANGEND]
                 15 SA10-ANNAHME-BIS             PIC  X(010).
      ** Aufbewahrungsdauer bis (TT.MM.JJJJ)    [DENDANG]
                 15 SA10-AUFBEWAHRUNG-BIS        PIC  X(010).
      ** Leerstellen
                 15 FILLER                       PIC  X(279).
      *
      ** versicherte Person 'D020'
      ** Kann pro Vertragssatz 1:n vorhanden sein.
      ** Es gibt je versicherte Person jedoch nur einen Satz.
           05 SATZ020  REDEFINES SATZ000.
              10 SA20-KEY.
      ** 'C' Konstant
                 15 SA20-SATZART                 PIC  X(001).
      ** '020' Konstant
                 15 SA20-DATENSATZART            PIC  X(003).
      ** Angebotsname
                 15 SA20-ANGEBOTSNAME            PIC  X(010).
      ** Vertragsnummer
                 15 SA20-VTRGNR                  PIC  9(010).
      ** Personennummer der versicherten PERSON (VP)
                 15 SA20-VPNR                    PIC  9(004).
      ***
              10 SA20-VPE-SATZ.
      ** Aktionsstichtag / Umstellungstermin (TT.MM.JJJJ)
                 15 SA20-DATUM                   PIC  X(010).
      ** Nachname   VP
                 15 SA20-NNAME-VP                PIC  X(040).
      ** Vorname    VP
                 15 SA20-VNAME-VP                PIC  X(025).
      ** Datum_Geburtstag der versicherten PERSON (TT.MM.JJJJ)
                 15 SA20-DGBT                    PIC  X(010).
      ** Anzahl der Umstellungsangebote für die versicherte Person
                 15 SA20-ANZ-ANGEBOTE            PIC  9(002).
      ** Leerstellen
                 15 FILLER                       PIC  X(285).
      *
      ** Tarife ohne Umstellung 'D030'
           05 SATZ030  REDEFINES SATZ000.
              10 SA30-KEY.
      ** 'C' Konstant
                 15 SA30-SATZART                 PIC  X(001).
      ** '030' Konstant
                 15 SA30-DATENSATZART            PIC  X(003).
      ** Angebots
                 15 SA30-ANGEBOTSNAME            PIC  X(010).
      ** Vertragsnummer
                 15 SA30-VTRGNR                  PIC  9(010).
      ** Personennummer der versicherten PERSON (VP)
                 15 SA30-VPNR                    PIC  9(004).
      ** Tarifteil eindeutige Identifikation des versicherten Tarifes
      **           im Zusammenhang mit Vertrags- und Personennummer
                 15 SA30-TARIF-TEIL-ID           PIC  9(010).
      ***
              10 SA30-TARIF-OHNE-SATZ.
      ** Stand zum   (TT.MM.JJJJ)
                 15 SA30-DATUM                   PIC  X(010).
      ** fachlicher Tarifname
                 15 SA30-TARIF                   PIC  X(040).
      ** Gesamtbeitrag (Zahlbeitrag) des versicherten Tarifs
      **         inkl. Gutschriften und Zuschlägen
                 15 SA30-GESAMT-BEITRAG          PIC S9(007)V99.
      ** Beitrag der Selbstbeteiligung
                 15 SA30-SELBSTBEHALT            PIC S9(007)V99.
      ** Hoehe der versicherten Leistung
                 15 SA30-VERS-LEISTUNG           PIC S9(007)V99.
      ** Beitrag gesetzlicher Zuschlag
                 15 SA30-GESETZL-ZUSCHLAG        PIC S9(007)V99.
      ** Gutschrift aus befr. Limitierung
                 15 SA30-ZEITL-BEFRIST-LIMIT     PIC S9(007)V99.
      ** Summe Beitragszuschlaege
                 15 SA30-SUM-BEITRAGS-ZUSCHL     PIC S9(007)V99.
      ** Pauschalleistung bei 1 leistungsfreien Jahr
                 15 SA30-PAUSCH-LEISTUNG-1       PIC S9(007)V99.
      ** Pauschalleistung bei 2 leistungsfreien Jahren
                 15 SA30-PAUSCH-LEISTUNG-2       PIC S9(007)V99.
      ** Pauschalleistung bei 3 leistungsfreien Jahren
                 15 SA30-PAUSCH-LEISTUNG-3       PIC S9(007)V99.
      ** Pauschalleistung bei mehr als 3 leistungsfreien Jahren
                 15 SA30-PAUSCH-LEISTUNG-4       PIC S9(007)V99.
      ** Leerstellen
                 15 FILLER                       PIC  X(222).
      *
      ** Tarife mit Umstellung 'D040'
           05 SATZ040  REDEFINES SATZ000.
              10 SA40-KEY.
      ** 'C' Konstant
                 15 SA40-SATZART                 PIC  X(001).
      ** '040' Konstant
                 15 SA40-DATENSATZART            PIC  X(003).
      ** Angebotsname
                 15 SA40-ANGEBOTSNAME            PIC  X(010).
      ** Vertragsnummer
                 15 SA40-VTRGNR                  PIC  9(010).
      ** Personennummer der versicherten PERSON (VP)
                 15 SA40-VPNR                    PIC  9(004).
      ** Tarifteil eindeutige Identifikation des versicherten Tarifes
      **           im Zusammenhang mit Vertrags- und Personennummer
                 15 SA40-TARIF-TEIL-ID           PIC  9(010).
      ** Angebotsnummer
                 15 SA40-ANGNR                   PIC  9(002).
      ***
              10 SA40-TARIF-MIT-SATZ.
      ** Stand zum   (TT.MM.JJJJ)
                 15 SA40-DATUM                   PIC  X(010).
      ** fachlicher Tarifname
                 15 SA40-TARIF                   PIC  X(040).
      ** Tarifstatus 3 moegliche Zustaende
      **               - unveraendert
      **               - entfallend
      **               - zugehend
                 15 SA40-TARIFSTATUS             PIC  X(020).
      ** Gesamtbeitrag (Zahlbeitrag) des versicherten Tarifs
      **         inkl. Gutschriften und Zuschlägen
                 15 SA40-GESAMT-BEITRAG          PIC S9(007)V99.
      ** Beitrag der Selbstbeteiligung
                 15 SA40-SELBSTBEHALT            PIC S9(007)V99.
      ** Hoehe der versicherten Leistung
                 15 SA40-VERS-LEISTUNG           PIC S9(007)V99.
      ** Beitrag gesetzlicher Zuschlag
                 15 SA40-GESETZL-ZUSCHLAG        PIC S9(007)V99.
      ** Gutschrift aus befr. Limitierung
                 15 SA40-ZEITL-BEFRIST-LIMIT     PIC S9(007)V99.
      ** Summe Beitragszuschlaege
                 15 SA40-SUM-BEITRAGS-ZUSCHL     PIC S9(007)V99.
      ** Pauschalleistung bei 1 leistungsfreien Jahr
                 15 SA40-PAUSCH-LEISTUNG-1       PIC S9(007)V99.
      ** Pauschalleistung bei 2 leistungsfreien Jahren
                 15 SA40-PAUSCH-LEISTUNG-2       PIC S9(007)V99.
      ** Pauschalleistung bei 3 leistungsfreien Jahren
                 15 SA40-PAUSCH-LEISTUNG-3       PIC S9(007)V99.
      ** Pauschalleistung bei mehr als 3 leistungsfreien Jahren
                 15 SA40-PAUSCH-LEISTUNG-4       PIC S9(007)V99.
      ** Leerstellen
                 15 FILLER                       PIC  X(200).
      *
      ** DTA VORSATZ / A-Satz zur Identifikation des DTA-Inhaltes
      ** Darf nur 1 x je DTA vorhanden sein.
           05 VORSATZ  REDEFINES SATZ000.
      ** 'A' Konstant
              10 V-SATZART                       PIC  X(001).
      ** 'GENERALI DEU KRANKEN'
              10 V-ABSENDER                      PIC  X(027).
      ** ErstellDatum in der Form TTMMJJJJ
              10 V-ERSTELLDAT                    PIC  9(008).
      ** Stichtag in der Form TTMMJJJJ
              10 V-DTASTAND                      PIC  9(008).
      ** 'DVAG'
              10 V-EMPFAENGER1                   PIC  X(004).
      ** 'FFM-RECHENZENTRUM-WL24'
              10 V-EMPFAENGER2                   PIC  X(022).
      ** 'AKTION'
              10 V-LIEFERUNG                     PIC  X(008).
      ** Leerstellen
              10 FILLER                          PIC  X(322).
      *
      *    NACHSATZ  SA 'E'
           05 NACHSATZ REDEFINES SATZ000.
      ** 'E' Konstant
              10 N-SATZART                       PIC  X(001).
      ** Anzahl gelieferte Datensätze ohne Anfang- und Endesatz.
              10 N-ANZGESAMT                     PIC  9(009).
      ** Anzahl gelieferte Datensätze Datensatzart '010'
              10 N-ANZ010                        PIC  9(009).
      ** Anzahl gelieferte Datensätze Datensatzart '020'
              10 N-ANZ020                        PIC  9(009).
      ** Anzahl gelieferte Datensätze Datensatzart '030'
              10 N-ANZ030                        PIC  9(009).
      ** Anzahl gelieferte Datensätze Datensatzart '040'
              10 N-ANZ040                        PIC  9(009).
      ** Leerstellen
              10 FILLER                          PIC  X(354).

