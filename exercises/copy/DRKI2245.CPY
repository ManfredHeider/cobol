      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 2 4 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER GES. DeuBa,       *
      *    AUFBEREITUNG DER KUNDENDATEN UND ALLG. VERTRAGSDATEN        *
      *        - AUSGABE: ALLE EIN- UND AUSZAHLUNGEN                   *
      *                                                                *
      *    SATZLÄNGE   367 B                                           *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM    !      ÄNDERUNG                           !NAME        *
      *AUFTRAG  !                                         !            *
      *---------!-----------------------------------------!------------*
140408* AKI00708!14.02.08! zus. Attribute                 !WHO         *
      *---------!--------!--------------------------------!------------*
110612* AKI01149!11.06.12! PIN verarbeiten                !WHO         *
      *---------!--------!--------------------------------!------------*
      * FKI01544!22.03.17! III entfernt                   !HHU         *
      *---------!--------!--------------------------------!------------*
      * AKI01821!22.12.20!NEU FUER DVAG WIEN SPARPLAN     !U130685     *
      *         !        !->  GZ-FOZABRKTO-KTY            !            *
      *         !        !    GZ-FOZABRKTO-KNR            !            *
      *---------!--------!--------------------------------!------------*
      *DBO10080 !09.09.22!Formatänderung GZ-DYNAM von     !u131774     *
      *         !        !9(002)V99 nach 9(003)V99        !            *
      ******************************************************************
       01  DRKI2245.
           05  GZ-GES                PIC  9(002).
           05  GZ-VNR                PIC  9(015).
      *    05  GZ-ANTEIL-LFDNR       PIC  9(002).
 
           05  GZ-ZAHLUNGS-KZ        PIC  X(001).
      *        'E' = Einzahlung, 'A' = Auszahlung
 
251102     05  GZ-DEPOT-UNR          PIC  9(002).
251102*        Depotunternr. wird Teil des Keys in Guthabenprodukt
 
           05  GZ-ZAHLUNG-LFDNR      PIC  9(004).
      *        LFDNR von DIT /Sparplannr. von DB
 
251102     05  GZ-ISIN               PIC  X(012).
251102*        Internationale WKN
 
           05  GZ-WP-KENN-NR         PIC  X(006).
      *        WERTPAPIERKENNNUMMER
 
           05  GZ-BETRAG             PIC  9(007)V99.
           05  GZ-BETRAG-AEND        PIC  X(001).
      *        Sparleistung oder Auszahlung
           05  GZ-DYNAM              PIC  9(003)V99.
           05  GZ-DYNAM-AEND         PIC  X(001).
 
           05  GZ-ZAHLWEISE          PIC S9(002).
           05  GZ-ZAHLWEISE-AEND     PIC  X(001).
 
           05  GZ-TAG                PIC  9(002).
           05  GZ-TAG-AEND           PIC  X(001).
      *        Tag der Ein- oder Auszahlung
 
           05  GZ-BUCHSCHL           PIC S9(002).
           05  GZ-BUCHSCHL-AEND      PIC  X(001).
 
251102     05  GZ-DAT-EROEFFNUNG     PIC  X(010).
251102*        Datum Sparplan-/Auszahlplaneröffnung
 
251102     05  GZ-DAT-SCHLIESSUNG    PIC  X(010).
251102*        Datum Sparplan-/ Auszahlplanschliessung
 
251102     05  GZ-DAT-ENDE-EINZ      PIC  X(010).
251102*        Datum Ende der Einzahlungsfrist bei VL-Verträgen
 
251102     05  GZ-KONTOINHABER       PIC  X(027).
251102*        Kontoinhaber von DB
 
251102     05  GZ-BANKNAME           PIC  X(020).
251102*        Bankverbindung von DB, Namen des Instituts
 
251102     05  GZ-BLZ                PIC  9(011).
251102*        Bankverbindung von DB, BLZ des Instituts
 
251102     05  GZ-KONTONR            PIC  9(018).
251102*        Bankverbindung von DB, Kontonr.
 
           05  GZ-AKTUAL-KZ          PIC  X(001).
      *        I = INSERT
      *        D = DELETE
      *        U = UPDATE
 
           05  GZ-EIN-LOESCHEN-KZ    PIC  X(001).
      *       'J'  = alle Einzahlungen in DB löschen
           05  GZ-AUS-LOESCHEN-KZ    PIC  X(001).
      *       'J'  = alle Auszahlungen in DB löschen
 
251102     05  GZ-VL-KZ              PIC X(001).
251102*       'J'  = VL-Vertrag
251102*       'N'  = nicht-VL-Vertrag
 
181004     05  GZ-BEGINN-RATENAUSSETZUNG PIC  X(010).
181004*            Beginndatum der Ratenaussetzung
181004     05  GZ-ENDE-RATENAUSSETZUNG   PIC  X(010).
181004*            Endedatum der Ratenaussetzung
270705     05  GZ-KOSTEN-PROZ                       PIC 9(01)V99.
>>    *               Kosten in %
           05  GZ-URSPR-GES-SPAR-LEIST              PIC 9(06)V99.
      *               Ursprüngliche Gesamtsparleistung in EUR
           05  GZ-URSPR-SPAR-RATE                   PIC 9(04)V99.
      *               Ursprüngliche Sparrate
           05  GZ-AUSG-AUFSCHL-URSPR                PIC 9(02)V99.
      *               Ausgabeaufschlag des ursprünglichen Fonds
           05  GZ-AUSG-AUFSCHL-AKTUELL              PIC 9(02)V99.
      *               Ausgabeaufschlag des aktuellen Fonds
           05  GZ-VERTR-LAUFZEIT                    PIC 9(02).
      *               Vertragslaufzeit
           05  GZ-BEG-DYNAM                         PIC X(10).
      *               Beginn Dynamisierung
           05  GZ-ZAEHLER-LASTSCHR-RUECK            PIC 9(02).
      *               Zähler Lastschriftrückgaben
           05  GZ-KND-MERKMAL                       PIC X(01).
      *               Kündigungsmerkmal = 'K' bzw. ' '
           05  GZ-DAT-VERTR-AUFLOESUNG              PIC X(10).
      *               Datum Vertragsauflösung  bzw. vorzeitige
<<    *               Vertragskündigung
270705*
280706*    05  GZ-DEL-KZ                            PIC X(001).
280706*       'J'  = Sparplan DIT in DB löschen
280706*
280706     05  GZ-VERTRAGSTYP                       PIC X(002).
 
140408     05  GZ-BONIFIKATION                      PIC  9(003)V9(04).
140408     05  GZ-URSPR-VERT-NR                     PIC  X(012).
 
110612     05  GZ-PIN                               PIC  9(09).

      *               ABRECHNUNGSKONTO
           05  GZ-FOZABRKTO-KTY                     PIC  S9(004) COMP.
 
           05  GZ-FOZABRKTO-KNR                     PIC  S9(004) COMP.
 
           05  FILLER                               PIC  X(086).
 



