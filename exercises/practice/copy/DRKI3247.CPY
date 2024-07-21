      *****************************************************************
      *                                                               *
      *    G79 - Geiger Edelmetalle                                   *
      *    Buchungen - DTA                                            *
      *                                                               *
      *    Satzlänge 100 Bytes                                        *
      *                                                               *
      *    LOGBUCH                                                    *
      *--------!-------------------------------------------!----------*
      *AUFTRAG !Datum   !Text                              !Name      *
      *--------!-------------------------------------------!----------*
      *AKI01565!24.01.18!      Neu                         !tfl       *
      *--------!-------------------------------------------!----------*
      *DBO10080!13.09.22!neue/geänderte Datfelder          !u131774   *
      *        !        !                                  !          *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
       01  'XX'-DTA-SATZ.
      *
      *****************************************************************
      * Vorsatz
      *****************************************************************
      *
           05  'XX'-VORSATZ.
      *
      * Satzart - immer 'A'
               10  'XX'-V-SA                       PIC  X(001).
      *
      * Absender - 'GEIGER  '
               10  'XX'-V-ABSENDER                 PIC  X(008).
      *
      * Datum der Erstellung des DTA im Format  JHJJMMTT
               10  'XX'-V-DATUM                    PIC  9(008).
      *
      * Empfänger bzw. Mandant - 'DVAG' oder 'ALLF'
               10  'XX'-V-EMPFAENGER               PIC  X(004).
      *
      * Welche Art Lieferung? 'D' - Delta oder 'K' - Komplett
      * bei Buchungen vorerst immer 'D'
               10  'XX'-V-LIEFERUNG                PIC  X(001).
      *
      * Reserve
               10  FILLER                          PIC  X(078).
      *
      *****************************************************************
      * Hauptsatz
      *****************************************************************
      *
           05  'XX'-HAUPTSATZ REDEFINES 'XX'-VORSATZ.
      *
      * Satzart - immer 'C'
               10  'XX'-SA                         PIC  X(001).
      *
      * Gesellschaft - immer '79'
               10  'XX'-GESELLSCHAFT               PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
               10  'XX'-VNR                        PIC  X(015).
               
      * Bezeichnung des verkauften Produktes in Form einer Fake ISIN
      * bspw. 'GEIGERG00050' für den 50g Goldbarren
               10  'XX'-PRODUKT                    PIC  X(012).
      *
      * Buchungsart
      *    01 - Kauf
      *    02 - Verkauf
      *    03 - Gebühr für Administration
      *    04 - Auslieferung
      *    05 - Übertrag Zugang
      *    06 - Übertrag Abgang
      *    07 - Stornierung Kauf
      *    08 - Stornierung Verkauf
      *    09 - Stornierung Gebühr für Administration
      *    10 - Stornierung Auslieferung
      *    11 - Stornierung Übertrag Zugang
      *    12 - Stornierung Übertrag Abgang
               10  'XX'-ART                        PIC  9(002).
      *
      * Buchungsbetrag
               10  'XX'-BETRAG                     PIC  9(007)V9(002).
      *
      * Buchung in Gramm
               10  'XX'-GRAMM                      PIC  9(007)V9(004).
      *
      * Datum der Goldtransaktion im Format JHJJMMTT
               10  'XX'-DATUM                      PIC  9(008).
      *
      * Reserve
               10  FILLER                          PIC  X(040).
 
      *****************************************************************
      * Endesatz
      *****************************************************************
      *
           05  'XX'-ENDESATZ REDEFINES 'XX'-VORSATZ.
      *
      * Satzart - immer 'E'
               10  'XX'-N-SA                       PIC  X(001).
      *
      * Satzanzahl - gelieferte C-Sätze
               10  'XX'-N-ANZAHL                   PIC  9(008).
      *
      * Reserve
               10  FILLER                          PIC  X(091).
 
 
 

