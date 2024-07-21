      *****************************************************************
      *    COPY-MEMBER: DRKI3980                                      *
      *                                                               *
      *   G79 BUCHUNGSDATEN GOLDRENTE (EINGABEDATEI ERM/AUFB)         *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *                                                               *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DBO-10295  !10.10.22! Neuerstellung                ! ATAR     *
      * --------------------------------------------------------------*
      * DBO-10735  !06.01.23! Kommentar zur Buchungsart    ! u130684  *
      * und        !und     ! angepasst                    !          *
      * DBO-10778  !16.01.23!                              !          *
      * --------------------------------------------------------------*
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
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
      * Empfänger bzw. Mandant - 'DVAG', 'ALLF' oder 'ALAG'
               10  'XX'-V-EMPFAENGER               PIC  X(004).
      *
      * 'D' - Delta Lieferung
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
      *
      * Pseudo-ISIN
      * Bei Goldrente immer 'GEIGERG05000'
               10  'XX'-PRODUKT                    PIC  X(012).
      *
      * Buchungsart
      *  1 = Kauf
      *  2 = Verkauf
      *  3 = Entgeldbelastung
      *  5 = Übertrag Zugang 
      *  6 = Übertrag Abgang
      *  7 = Stornierung Kauf
      *  8 = Stornierung Verkauf
      *  9 = Stornierung Entgeldbelastung
      * 11 = Stornierung Übertrag Zugang
      * 12 = Stornierung Übertrag Abgang 
      * 13 = Gebühr für Lagerung und Versicherung
      * 14 = Storno Gebühr für Lagerung und Vers.
               10  'XX'-BUCHUNGSART                PIC  9(002).
      *
      * Buchungsbetrag
               10  'XX'-BUCHUNGSBETRAG             PIC  9(007)V9(002).
      *
      * Menge in Gramm
               10  'XX'-MENGEGRAMM                 PIC  9(007)V9(004).
      *
      * Buchungsdatum
               10  'XX'-BUCHUNGSDATUM              PIC  9(008).
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
      *****************************************************************
      *---------------------- ENDE DRKI3980 --------------------------*
      *****************************************************************
