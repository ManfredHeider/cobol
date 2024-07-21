      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  06.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3432.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3432.CPv  $
      *
      *   Rev 2.1   Oct 30 2018 11:37:02   t13ae52
      *AKI01628: Depottransaktionen - Ermittlung
      *
      *   Rev 2.0   Sep 06 2018 13:56:36   t13ae52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER ZUM G42 ERMITTLUNGS- UND AUFBEREITUNGSPROGRAMM  *
      *    DEPOTTRANSAKTIONEN                                          *
      *        !                                                       *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !DATUM   !TEXT                              !NAME       *
      *--------!-------------------------------------------!-----------*
      *AKI01628!06.09.18!2.0 ! NEUERSTELLUNG               !SCHUMACHER *
      *--------!-------------------------------------------!-----------*
      *        !        !    !                             !           *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *--------------------------------------------------------------- *
      *                                                                *
      *                                                                *
      **************************************************************** *
      *                                                                *
      *                    G42 GENERALI INVEST                         *
      *                 AUSGABE - D A T E N S A T Z                    *
      *                                                                *
      * AUFBEREITETER DEPOTTRANSAKTIONS-DTA ZUR FOLGEVERARBEITUNG      *
      * (AKTUALISIERUNG IN TABELLE GUTHABENBUCHUNG)                    *
      *                                                                *
      *     LAENGE:   200 BYTES                 STAND: 06.09.2018      *
      *                                                                *
      **************************************************************** *
      *
       01 'XX'-SATZ.
 
      *    I = INSERT
      *    U = UPDATE
      *    D = DELETE
           05  'XX'-AKTUAL-KNZ                PIC  X(001).

 
      *    GESELLSCHAFTSNUMMER
           05  'XX'-GES                       PIC  9(002).
 
      *    VERTRAGSNUMMER
           05  'XX'-VNR                       PIC  9(015).
 
      *    ISIN
           05  'XX'-ISIN                      PIC  X(012).
 
      *    STÜCKANZAHL ODER NOMINALWERT IN ORIGINALWÄHRUNG
           05  'XX'-GHBGUTHABENANT            PIC  9(005)V9(004).
 
      *    BUCHUNGSBETRAG
           05  'XX'-GHBBETRAGLETZTBUCH        PIC  9(007)V9(002).
 
      *    BUCHUNGSDATUM
           05  'XX'-GHBBUCHUNGSDATUM          PIC  X(010).
 
      *    BUCHUNGSSCHLÜSSEL
           05  'XX'-GHBKZFINHLETZTBUID        PIC  S9(004).
 
      *    STEUERBETRAG DER BUCHUNG (ZINSABSCHLAGSSTEUER)
           05  'XX'-GHBSTEUBETRLETZTBU        PIC  9(009)V99.
 
      *    BEI G42 IST GHBGUTHABPRODLFDNR DIE LETZTEN BEIDEN STELLEN
      *    DER VERTRAGSNUMMER
           05  'XX'-GHBGUTHABPRODLFDNR        PIC  9(003).
 
      *    BEI G42 IST GHBGHALFDNR IMMER 0
           05  'XX'-GHBGHALFDNR               PIC  9(001).
 
      *    WÄHRUNGSKENNZEICHEN, I.D.R. E (EURO)
           05  'XX'-GHBBETRLBUWKZ             PIC  X(001).

 
      *    RESERVE
           05  'XX'-RESERVE                   PIC  X(122).
 
 
 
      *
      ******************************************************************
      *                E N D E    COPY-MEMBER DRKI3432                 *
      ******************************************************************
 

