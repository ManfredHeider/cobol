      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.05.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0003.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKW0003.CPv  $
      *
      *   Rev 2.2   Jun 13 2013 08:47:58   t13ex14
      *AKI01204: SEPA - DB-Online - DVAG Wien
      *
      *   Rev 2.1   Nov 11 2010 11:13:52   t13ex14
      *AKI00966: DVAG Österreich: Kunden- und Vertrags-DTAs Aktuali
      *sierung
      *
      *   Rev 2.0   May 07 2010 08:37:02   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - KSDS-Datei DVAG Wien Kundendaten                        *
      *                                                               *
      *    SATZLÄNGE   490                                            *
140513*    SATZLÄNGE   640                                            *
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
       01  DRKW0003.

           05  KOMPL-KUN-AEND-KZ                PIC 9(002).

           05  KOMPL-KUNDEN-SATZ.
               10 KOMPL-KUN-SATZART             PIC X(001).

               10 KOMPL-KUN-KEY.
                  15 KOMPL-KUN-GESELLSCHAFT     PIC 9(002).
      *           Produktgeber
                  15 KOMPL-KUN-VERTRAGSNUMMER   PIC 9(015).

               10 KOMPL-KUN-NACHNAME            PIC X(030).
               10 FILLER                        PIC X(010).
               10 KOMPL-KUN-VORNAME             PIC X(030).
      *           bei Firmen ist der Vorname immer leer
               10 KOMPL-KUN-GEBURTSDATUM        PIC 9(008).
      *           JJJJMMTT bei Firmen ist das Geburtsdatum leer
               10 KOMPL-KUN-ANREDEKZ            PIC X(001).
      *           1 = Herr, 2 = Frau, 5 = Firma, leer für unbekannt
               10 KOMPL-KUN-GESCHLECHT          PIC X(001).
      *           M, W, leer für unbekannt und bei Firmen
               10 KOMPL-KUN-STRASSE             PIC X(045).
      *           Strasse und Hausnummer
               10 KOMPL-KUN-POSTLEITZAHL        PIC X(009).
      *           rechtsbündig
               10 KOMPL-KUN-WOHNORT             PIC X(040).
               10 KOMPL-KUN-LANDKZ              PIC X(003).
      *           ISO-Format
               10 KOMPL-KUN-KONTONUMMER         PIC X(015).
               10 KOMPL-KUN-BANKLEITZAHL        PIC 9(008).
               10 KOMPL-KUN-NACHNAME-2          PIC X(030).
               10 FILLER                        PIC X(010).
               10 KOMPL-KUN-VORNAME-2           PIC X(030).
               10 KOMPL-KUN-GEBURTSDATUM-2      PIC 9(008).
      *           JJJJMMTT bei Firmen ist das Geburtsdatum leer
               10 KOMPL-KUN-ANREDEKZ-2          PIC X(001).
      *           1 = Herr, 2 = Frau, 5 = Firma, leer für unbekannt
               10 KOMPL-KUN-GESCHLECHT-2        PIC X(001).
      *           M, W, leer für unbekannt und bei Firmen
               10 KOMPL-KUN-KUNDENIDDIM7        PIC X(032).
      *           KundenID aus Dimension7 zu Kontrollzwecken
               10 KOMPL-KUN-DVAGKDNR            PIC X(009).
      *           DVAG-Kundennummer zu Kontrollzwecken
140513         10 KOMPL-KUN-IBAN                PIC X(034).
>>             10 KOMPL-KUN-BIC                 PIC X(011).
               10 KOMPL-KUN-NACHNAME-BZ         PIC X(030).
               10 KOMPL-KUN-VORNAME-BZ          PIC X(030).
               10 KOMPL-KUN-GEBURTSDATUM-BZ     PIC 9(008).
      *           JJJJMMTT bei Firmen ist das Geburtsdatum leer
               10 KOMPL-KUN-ANREDEKZ-BZ         PIC X(001).
      *           1 = Herr, 2 = Frau, 5 = Firma, leer für unbekannt
               10 KOMPL-KUN-GESCHLECHT-BZ       PIC X(001).
      *           M, W, leer für unbekannt und bei Firmen
               10 KOMPL-KUN-STRASSE-BZ          PIC X(045).
      *           Strasse und Hausnummer
               10 KOMPL-KUN-POSTLEITZAHL-BZ     PIC X(009).
      *           rechtsbündig
               10 KOMPL-KUN-WOHNORT-BZ          PIC X(040).
               10 KOMPL-KUN-LANDKZ-BZ           PIC X(003).
      *           ISO-Format
    <<*        10 FILLER                        PIC X(111).
140513         10 FILLER                        PIC X(049).

           05  KOMPL-KUN-TIMESTAMP              PIC X(026).
      *        JJJJ-MM-TT-hh.mm.ss.abcdef
      *        abcdef = Millisekunden (wird mit 0 geliefert)
           05  FILLER                           PIC X(012).

      ******************************************************************
      *          E n d e    Copy-Member DRKW0003                       *
      ******************************************************************
