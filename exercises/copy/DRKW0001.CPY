      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.05.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0001.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKW0001.CPv  $
      *
      *   Rev 2.2   Jun 13 2013 08:50:54   t13ex14
      *AKI01204: SEPA - DB-Online - DVAG Wien
      *
      *   Rev 2.1   Nov 11 2010 11:13:42   t13ex14
      *AKI00966: DVAG Österreich: Kunden- und Vertrags-DTAs Aktuali
      *sierung
      *
      *   Rev 2.0   May 06 2010 12:12:40   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Eingabedatei DVAG Wien Kundendaten mit Vorsatz          *
      *                                                               *
      *    SATZLÄNGE   450                                            *
140513*    SATZLÄNGE   600                                            *
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
       01  DRKW0001.

           05  DVW-KUN-VORSATZ.

               10 DVW-KUN-V-SATZART           PIC X(001).
      *           'A'
               10 DVW-KUN-ABSENDER            PIC X(005).
      *           'DWIEN'
               10 DVW-KUN-DTADATUM            PIC 9(008).
      *           JJJJMMTT  (alle DTAs von einem Tag müssen in
      *           diesem Feld den gleichen Wert haben)
               10 DVW-KUN-EMPFAENGER          PIC X(004).
      *           'DVAG'
               10 DVW-KUN-DATENTEXT           PIC X(025).
      *           'SACHDATEN'
               10 DVW-KUN-VERABKZ             PIC X(004).
      *           'INIT'
140513*        10 FILLER                     PIC X(403).
140513         10 FILLER                     PIC X(553).

           05  DVW-KUNDEN-DATEN REDEFINES DVW-KUN-VORSATZ.

             07 DVW-AENDERUNG-SATZ.

               10 DVW-KUN-SATZART             PIC X(001).
      *           'D'
               10 DVW-KUN-KEY.
                  15 DVW-KUN-GESELLSCHAFT     PIC 9(002).
      *              Produktgeber
                  15 DVW-KUN-VERTRAGSNUMMER   PIC 9(015).
               10 DVW-KUN-NACHNAME            PIC X(030).
               10 FILLER                      PIC X(010).
               10 DVW-KUN-VORNAME             PIC X(030).
      *           bei Firmen ist der Vorname immer leer
               10 DVW-KUN-GEBURTSDATUM        PIC 9(008).
      *           JJJJMMTT bei Firmen ist das Geburtsdatum leer
               10 DVW-KUN-ANREDEKZ            PIC X(001).
      *           1 = Herr, 2 = Frau, 5 = Firma, leer für unbekannt
               10 DVW-KUN-GESCHLECHT          PIC X(001).
      *           M, W, leer für unbekannt und bei Firmen
               10 DVW-KUN-STRASSE             PIC X(045).
      *           Strasse und Hausnummer
               10 DVW-KUN-POSTLEITZAHL        PIC X(009).
      *           rechtsbündig
               10 DVW-KUN-WOHNORT             PIC X(040).
               10 DVW-KUN-LANDKZ              PIC X(003).
      *           ISO-Format
               10 DVW-KUN-KONTONUMMER         PIC X(015).
               10 DVW-KUN-BANKLEITZAHL        PIC 9(008).
               10 DVW-KUN-NACHNAME-2          PIC X(030).
               10 FILLER                      PIC X(010).
               10 DVW-KUN-VORNAME-2           PIC X(030).
               10 DVW-KUN-GEBURTSDATUM-2      PIC 9(008).
      *           JJJJMMTT bei Firmen ist das Geburtsdatum leer
               10 DVW-KUN-ANREDEKZ-2          PIC X(001).
      *           1 = Herr, 2 = Frau, 5 = Firma, leer für unbekannt
               10 DVW-KUN-GESCHLECHT-2        PIC X(001).
      *           M, W, leer für unbekannt und bei Firmen
               10 DVW-KUN-KUNDENIDDIM7        PIC X(032).
      *           SACHID aus Dimension7 zu Kontrollzwecken
               10 DVW-KUN-DVAGKDNR            PIC X(009).
      *           DVAG-KDNR zu Kontrollzwecken
140513         10 DVW-KUN-IBAN                PIC X(034).
>>             10 DVW-KUN-BIC                 PIC X(011).
               10 DVW-KUN-NACHNAME-BZ         PIC X(030).
               10 DVW-KUN-VORNAME-BZ          PIC X(030).
               10 DVW-KUN-GEBURTSDATUM-BZ     PIC 9(008).
      *           JJJJMMTT bei Firmen ist das Geburtsdatum leer
               10 DVW-KUN-ANREDEKZ-BZ         PIC X(001).
      *           1 = Herr, 2 = Frau, 5 = Firma, leer für unbekannt
               10 DVW-KUN-GESCHLECHT-BZ       PIC X(001).
      *           M, W, leer für unbekannt und bei Firmen
               10 DVW-KUN-STRASSE-BZ          PIC X(045).
      *           Strasse und Hausnummer
               10 DVW-KUN-POSTLEITZAHL-BZ     PIC X(009).
      *           rechtsbündig
               10 DVW-KUN-WOHNORT-BZ          PIC X(040).
               10 DVW-KUN-LANDKZ-BZ           PIC X(003).
      *           ISO-Format
    <<*        10 FILLER                     PIC X(111).
140513         10 FILLER                     PIC X(049).

           05  DVW-KUN-NACHSATZ REDEFINES DVW-KUN-VORSATZ.

               10 DVW-KUN-N-SATZART           PIC X(001).
      *           'E'
               10 DVW-KUN-ANZ                 PIC 9(009).
      *           Satzanzahl incl. Vor- und Nachsatz
140513*        10 FILLER                      PIC X(440).
140513         10 FILLER                      PIC X(590).

      ******************************************************************
      *          E n d e    Copy-Member DRKW0001                       *
      ******************************************************************
