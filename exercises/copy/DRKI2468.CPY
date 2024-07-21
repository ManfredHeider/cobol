      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.05.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2468.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2468.CPv  $
      *
      *   Rev 2.5   Mar 12 2015 13:32:30   t13ex15
      *AKI01337: Änderung KLU Sonderleistungen 2015 Anpassung VBI S
      *ST und AS-Club
      *
      *   Rev 2.4   Jul 02 2014 08:12:30   t13ex15
      *AKI01303: "VEP"-Wertung für AS-Club und KLZ Programme und Co
      *pys
      *
      *   Rev 2.3   Dec 08 2011 09:36:26   t13ex15
      *AKI01109: AS-Club Erweitung TBKD0001 um CKV-Betrag PMKI0310
      *
      *   Rev 2.2   Jun 28 2011 07:45:50   t13ex15
      *AKI01049: AS-Club Historisierung PGM und Copy
      *
      *   Rev 2.1   Jun 20 2011 10:03:22   t13ex15
      *AKI01049: AS-Club Historisierung PGM und Copy
      *
      *   Rev 2.0   May 16 2011 10:09:20   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI2468                                     *
      *                                                              *
      *    FUNKTION: COPY FUER AUSGABESATZ SIEHE UNTEN               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: S. WARZ                                         *
      *    DATUM:    16.05.2011                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01049!16.05.11! 01 !NEU                        ! SWZ      *
      * -------------------------------------------------------------*
      * AKI01109!07.12.11! 2.3!Neues Feld CKV-Betrag      ! SWZ      *
      * -------------------------------------------------------------*
      * AKI01303!12.06.14! 2.4!Anpassen Struktur an       ! SWZ      *
      *         !        !    !Ladeformat wegen neuem     !          *
      *         !        !    !Feld für AML VEP           !          *
      * -------------------------------------------------------------*
170215* AKI01337!17.02.15! 2.5!Neues Feld Typ             ! SWZ      *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      * COBOLCOPY FÜR DIE AUSGABESATZART DER AS-CLUB-VERARBEITUNG
      *   (MONATSLAUF) -> STANDARDCOPY
      *     SATZLAENGE: 65 STELLEN
      *     FORMAT:     F
      *---------------------------------------------------------------*
      *    FELDER AUS SATZART
      *         - VBNR / 7-STELLIGE BERATERNUMMER
      *         - ANZAHL KUNDEN
      *         - ANZAHL VERTRAEGE
      *         - ANZAHL VERTRAEGE mit 2. VN
      *         - PRAEMIENBETRAG FÜR DEN OBIGEN (DIESEN) SCHLÜSSEL
      *         - PRAEMIENBETRAG FÜR INVEST GUTHABENANTEIL
      *         - PRAEMIENBETRAG FÜR VERRECHNUNGSKONTO TOPZINS
      *         - ERSTELLUNGDATUM
      *---------------------------------------------------------------*



       01  'XX'-FILE-REC.
      *    VBNR
           05 'XX'-VBNR             PIC  S9(9) COMP-4.
      *    ANZAHL KUNDEN
           05 'XX'-ANZKD            PIC  S9(9) COMP-4.
      *    ANZAHL VERTRÄGE
           05 'XX'-ANZVT            PIC  S9(9) COMP-4.
      *    ANZAHL KUNDEN MIT MEHR ALS EINEM VERTRAG
           05 'XX'-ANZKD2           PIC  S9(9) COMP-4.
      *    BEITRAGSVOLUMEN
           05 'XX'-PRAEMIE          PIC  S9(9)V9(2) COMP-3.
      *    Guthabenanteil (Investment)
           05 'XX'-PRAEMIE-GHA      PIC  S9(9)V9(2) COMP-3.
      *    Topzinskonto
           05 'XX'-PRAEMIE-VKOTO-TOPZINS
                                    PIC  S9(9)V9(2) COMP-3.
      *    Erstellungsdatum JHJJ-MM-TT mit immer dem 15.
           05 'XX'-ERSTELL-DATUM    PIC  X(10).
      *    CKV-Betrag komplett
           05 'XX'-PRAEMIE-CKV      PIC  S9(9)V9(2) COMP-3.
      *    AML-VEP komplett
           05 'XX'-PRAEMIE-AML-VEP  PIC  S9(9)V9(2) COMP-3.
170215*    Typ
170215     05 'XX'-TYP              PIC  S9(4) COMP-5.


