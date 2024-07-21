      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.05.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0005.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKW0005.CPv  $
      *
      *   Rev 2.3   Jun 18 2013 16:17:24   t13ex14
      *AKI01204: SEPA - DB-Online - DVAG Wien
      *
      *   Rev 2.2   Jun 13 2013 08:53:26   t13ex14
      *AKI01204: SEPA - DB-Online - DVAG Wien
      *
      *   Rev 2.1   Nov 11 2010 11:14:08   t13ex14
      *AKI00966: DVAG Österreich: Kunden- und Vertrags-DTAs Aktuali
      *sierung
      *
      *   Rev 2.0   May 07 2010 09:06:06   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Aufbereitungs-Datei DVAG Wien Kundendaten               *
      *                                                               *
      *    SATZLÄNGE   450                                            *
      *    SATZLÄNGE   600                                            *
      *                                                               *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00966!07.05.10! 01 ! Neu                       ! Misgeld  *
140513* AKI01204!14.05.13!    ! Erweiterungen für SEPA    ! WMI      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKW0005.

           05  AUFB-KUNDEN.

               10 AUFB-KUN-VERARB-KZ              PIC X(01).
      *           Verarbeitungskennzeichen
      *                I = Insert U = Update
               10 AUFB-KUN-GESELLSCHAFT           PIC 9(002).
      *           Produktgeber
               10 AUFB-KUN-VERTRAGSNUMMER         PIC 9(015).

               10 AUFB-KUN-NACHNAME               PIC X(030).
               10 AUFB-KUN-NACHNAME-AENDKZ        PIC X(001).

               10 AUFB-KUN-VORNAME                PIC X(030).
               10 AUFB-KUN-VORNAME-AENDKZ         PIC X(001).
      *           bei Firmen ist der Vorname immer leer

               10 AUFB-KUN-NACHNAME-ALT           PIC X(040).
               10 AUFB-KUN-NACHNAME-ALT-AENDKZ    PIC X(001).

               10 AUFB-KUN-VORNAME-ALT            PIC X(030).
               10 AUFB-KUN-VORNAME-ALT-AENDKZ     PIC X(001).
      *           bei Firmen ist der Vorname immer leer

               10 AUFB-KUN-GEBURTSDATUM           PIC 9(008).
               10 AUFB-KUN-GEBURTSDATUM-AENDKZ    PIC X(001).
      *           JJJJMMTT bei Firmen ist das Geburtsdatum leer

               10 AUFB-KUN-ANREDEKZ               PIC X(001).
      *           1 = Herr, 2 = Frau, 5 = Firma, leer für unbekannt
               10 AUFB-KUN-ANREDEKZ-AENDKZ        PIC X(001).

               10 AUFB-KUN-GESCHLECHT             PIC X(001).
      *           M, W, leer für unbekannt und bei Firmen
               10 AUFB-KUN-GESCHLECHT-AENDKZ      PIC X(001).

               10 AUFB-KUN-STRASSE                PIC X(045).
      *           Strasse und Hausnummer
               10 AUFB-KUN-STRASSE-AENDKZ         PIC X(001).

               10 AUFB-KUN-POSTLEITZAHL           PIC X(009).
      *           rechtsbündig
               10 AUFB-KUN-POSTLEITZAHL-AENDKZ    PIC X(001).

               10 AUFB-KUN-WOHNORT                PIC X(040).
               10 AUFB-KUN-WOHNORT-AENDKZ         PIC X(001).

               10 AUFB-KUN-LANDKZ                 PIC X(003).
      *           ISO-Format
               10 AUFB-KUN-LANDKZ-AENDKZ          PIC X(001).

               10 AUFB-KUN-KONTONUMMER            PIC X(015).
               10 AUFB-KUN-KONTONUMMER-AENDKZ     PIC X(001).

               10 AUFB-KUN-BANKLEITZAHL           PIC 9(008).
               10 AUFB-KUN-BANKLEITZAHL-AENDKZ    PIC X(001).

               10 AUFB-KUN-KUNDENIDDIM7           PIC X(032).
      *           KundenID aus Dimension7 zu Kontrollzwecken
               10 AUFB-KUN-KUNDENIDDIM7-AENDKZ    PIC X(001).

               10 AUFB-KUN-DVAGKDNR               PIC X(009).
      *           DVAG-Kundennummer zu Kontrollzwecken
               10 AUFB-KUN-DVAGKDNR-AENDKZ        PIC X(001).

140513         10 AUFB-KUN-IBAN                  PIC X(034).
>>             10 AUFB-KUN-IBAN-AENDKZ           PIC X(001).
               10 AUFB-KUN-BIC                   PIC X(011).
               10 AUFB-KUN-BIC-AENDKZ            PIC X(001).
               10 AUFB-KUN-NACHNAME-BZ           PIC X(030).
               10 AUFB-KUN-NACHNAME-BZ-AENDKZ    PIC X(001).
               10 AUFB-KUN-VORNAME-BZ            PIC X(030).
               10 AUFB-KUN-VORNAME-BZ-AENDKZ     PIC X(001).
               10 AUFB-KUN-GEBURTSDAT-BZ         PIC 9(008).
      *           JJJJMMTT bei Firmen ist das Geburtsdatum leer
               10 AUFB-KUN-GEBURTSDAT-BZ-AENDKZ  PIC X(001).
               10 AUFB-KUN-ANREDEKZ-BZ           PIC X(001).
      *           1 = Herr, 2 = Frau, 5 = Firma, leer für unbekannt
               10 AUFB-KUN-ANREDEKZ-BZ-AENDKZ    PIC X(001).
               10 AUFB-KUN-GESCHLECHT-BZ         PIC X(001).
      *           M, W, leer für unbekannt und bei Firmen
               10 AUFB-KUN-GESCHLECHT-BZ-AENDKZ  PIC X(001).
               10 AUFB-KUN-STRASSE-BZ            PIC X(045).
      *           Strasse und Hausnummer
               10 AUFB-KUN-STRASSE-BZ-AENDKZ     PIC X(001).
               10 AUFB-KUN-POSTLEITZ-BZ          PIC X(009).
      *           rechtsbündig
               10 AUFB-KUN-POSTLEITZ-BZ-AENDKZ   PIC X(001).
               10 AUFB-KUN-WOHNORT-BZ            PIC X(040).
               10 AUFB-KUN-WOHNORT-BZ-AENDKZ     PIC X(001).
               10 AUFB-KUN-LANDKZ-BZ             PIC X(003).
               10 AUFB-KUN-LANDKZ-BZ-AENDKZ      PIC X(001).
      *           ISO-Format
    <<*        10 FILLER                          PIC X(116).
140513         10 FILLER                          PIC X(043).

      ******************************************************************
      *          E n d e    Copy-Member DRKW0005                       *
      ******************************************************************
