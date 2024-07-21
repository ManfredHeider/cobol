      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM G79 ERMITTLUNGS- UND AUFBEREITUNGSPROGRAMM  *
      *    BUCHUNGEN                                                   *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !DATUM   !TEXT                              !NAME       *
      *--------!-------------------------------------------!-----------*
      *AKI01565!22.02.18!      NEUERSTELLUNG               !SCHUMACHER *
      *--------!-------------------------------------------!-----------*
      *DBO10227!11.08.22!Längenänderung                    !u131774    *
      *        !        !GHBGUTHABENANT von 9  auf 11 B    !           *
      *        !        !                                  !           *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *--------------------------------------------------------------- *
      *                                                                *
      *                                                                *
      **************************************************************** *
      *                                                                *
      *                    G79 GEIGER EDELMETALLE                      *
      *                 Ausgabe - D A T E N S A T Z                    *
      *                                                                *
      * AUFBEREITETER BUCHUNGS-DTA ZUR FOLGEVERARBEITUNG               *
      * (AKTUALISIERUNG IN TABELLE GUTHABENBUCHUNG)                    *
      *                                                                *
      *     LAENGE:   200 BYTES                 STAND: 22.02.2018      *
      *                                                                *
      **************************************************************** *
      *
       01 'XX'-SATZ.
      * VERTRAGS-ID DES GOLDSPARPLANS
           05  'XX'-GHBVERTRAGSID             PIC  X(013).
      * VERTRAGSNUMMER
           05  'XX'-VNR                       PIC  X(015).
      * GESELLSCHAFTSNUMMER
           05  'XX'-GES                       PIC  9(002).
      * BEI G79 IST GHBGUTHABPRODLFDNR IMMER 0
           05  'XX'-GHBGUTHABPRODLFDNR        PIC  9(003).
           05  'XX'-ISIN                      PIC  X(012).
      * BEI G79 IST GHBGHALFDNR IMMER 0
           05  'XX'-GHBGHALFDNR               PIC  9.
      * BUCHUNGSBETRAG
           05  'XX'-GHBBETRAGLETZTBUCH        PIC  9(007)V9(002).
      * WÄHRUNGSKENNZEICHEN, I.D.R. E (EURO)
           05  'XX'-GHBBETRLBUWKZ             PIC  X.
      * BUCHUNG IN GRAMM
           05  'XX'-GHBGUTHABENANT            PIC  9(007)V9(004).      
           05  'XX'-GHBBUCHUNGSDATUM          PIC  X(010).
      * BUCHUNGSSCHLÜSSEL
           05  'XX'-GHBKZFINHLETZTBUID        PIC  S9(004).
      * WIEVIEL RESERVE ?
           05  'XX'-RESERVE                   PIC  X(118).
           05  'XX'-AKTUAL-KNZ                PIC  X.
      *          I = INSERT
      *          U = UPDATE
      *          D = DELETE
      *
      ******************************************************************
      *                E N D E    COPY-MEMBER DRKI3295                 *
      ******************************************************************
 

