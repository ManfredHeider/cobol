      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  21.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3382.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI3382.CPv  $
      *
      *   Rev 2.5   Nov 01 2018 11:42:28   t13ae52
      *AKI01615: Erm./Aufb./Akt. FSA (DSC)
      *
      *   Rev 2.4   Nov 01 2018 09:30:58   t13ae52
      *AKI01615: Erm./Aufb./Akt. FSA (DSC)
      *
      *   Rev 2.3   Oct 31 2018 16:50:48   t13ae52
      *AKI01615: Erm./Aufb./Akt. FSA (DSC)
      *
      *   Rev 2.2   Oct 31 2018 16:13:30   t13ae52
      *AKI01615: Erm./Aufb./Akt. FSA (DSC)
      *
      *   Rev 2.1   Oct 30 2018 10:58:10   t13ae52
      *AKI01615: Erm./Aufb./Akt. FSA (DSC)
      *
      *   Rev 2.0   Aug 21 2018 16:23:04   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER FÜR FSA / ABGELTUNSSTEUER VON GENERALI INVEST   *
      *                                                                *
      *    SATZLÄNGE   200                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !DATUM   !TEXT                              !NAME       *
      *--------!-------------------------------------------!-----------*
      *AKI01615!22.08.18! NEU                              ! WHO       *
      *        !        !                                  !           *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
CCC009*    SKIP1
       05  DRKI3382.
 
      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************
 
           10  'XX'-VORSATZ.
      *
               15  'XX'-V-SA            PIC  X(001).
      *            SATZART = 'A'
               15  'XX'-V-ABSENDER      PIC  X(027).
      *            ABSENDER = 'GENERALI INVESTMENTS'
               15  'XX'-V-DATUM         PIC  9(008).
      *            DATUM = 'JJJJMMTT' ERSTELLUNG
               15  'XX'-V-DATUM-STAND   PIC  9(008).
      *            DATUM = 'JJJJMMTT' STAND
               15  'XX'-V-EMPFAENGER    PIC  X(027).
      *            EMPFAENGER = 'ALAG'
               15  'XX'-V-TEXT          PIC  X(020).
      *            'FSA-DATEN'
               15  'XX'-V-ART           PIC  X(001).
      *            K = KOMPLETTBESTAND, D FÜR DELTA
               15  FILLER               PIC  X(108).
      *            RESERVE
      *
 
           10  'XX'-SATZ     REDEFINES      'XX'-VORSATZ.
      *
               20  'XX'-SA                  PIC  X(001).
      *            SATZART        = 'C'
               20  'XX'-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '42'
               20  'XX'-VNR                 PIC  9(015).
      *            VERTRAGSNUMMER
               20  'XX'-GUELTIG-AB-DAT      PIC  9(008).
      *            FSA GÜLTIG AB
               20  'XX'-GUELTIG-BIS-DAT     PIC  9(008).
      *            FSA GÜLTIG BIS
               20  'XX'-FSA-GEMELDET        PIC  9(005)V99.
               20  'XX'-FSA-GEMELDET-R REDEFINES 'XX'-FSA-GEMELDET
                                            PIC  X(007).
      *            FSA GEMELDET
               20  'XX'-FSA-VERBRAUCHT      PIC  9(005)V99.
               20  'XX'-FSA-VERBRAUCHT-R REDEFINES 'XX'-FSA-VERBRAUCHT
                                            PIC  X(007).
      *            FSA VERBRAUCHT
               20  'XX'-STUECKZINSEN-LFD-J  PIC  S9(010)V99.
      *            STÜCKZINSEN LFD. JAHR
               20  'XX'-STUECKZINSEN-LFD-J-R REDEFINES
                                            'XX'-STUECKZINSEN-LFD-J
                                            PIC X(12).
 
               20  'XX'-STEUERAUSL          PIC  X(001).
      *             STEUERAUSLÄNDERKENNZEICHEN DER KUNDENSTAMMNUMMER
      *             J/N
               20  'XX'-NICHTVERANL-BIS-DAT PIC  9(008).
      *             NICHTVERANLAGUNGSBESCHEINIGUNG - GÜLTIG BIS
      *             JHJJMMTT
               20  'XX'-AUSG-FSA-IM-VJ      PIC  9(005)V99.
      *             AUSGENUTZTER FREISTELLUNGSBETRAG IM VORJAHR IN EUR
               20  'XX'-AUSG-FSA-IM-VJ-R REDEFINES
                   'XX'-AUSG-FSA-IM-VJ      PIC  X(007).
 
               20  'XX'-AUFGEL-ERTRAG       PIC  9(007)V99.
      *             AUFGELAUFENER ERTRAG GESAMT AKTUELLES JAHR
      *             (KUMULIERT)
               20  'XX'-AUFGEL-ERTRAG-R REDEFINES
                   'XX'-AUFGEL-ERTRAG       PIC  X(007).
 
               20  'XX'-EINBEH-KEST         PIC  9(005)V99.
      *             EINBEHALTENE KEST AKTUELLES JAHR (KUMULIERT)
               20  'XX'-EINBEH-KEST-R REDEFINES
                   'XX'-EINBEH-KEST         PIC  X(007).
 
               20  'XX'-EINBEH-SOLI         PIC  9(005)V99.
      *             EINBEHALTENER SOLI AKTUELLES JAHR (KUMULIERT)
               20  'XX'-EINBEH-SOLI-R REDEFINES
                   'XX'-EINBEH-SOLI         PIC  X(007).
 
               20  'XX'-EINBEH-KIST         PIC  9(005)V99.
      *             EINBEHALTENE KIRCHENSTEUER AKTUELLES JAHR
      *             (KUMULIERT)
               20  'XX'-EINBEH-KIST-R REDEFINES
                   'XX'-EINBEH-KIST         PIC  X(007).
 
               20  'XX'-VERLUST-AKTIEN      PIC  9(007)V99.
      *             VERLUSTTOPF AKTIEN AKTUELLER BALANCE-WERT
               20  'XX'-VERLUST-AKTIEN-R REDEFINES
                   'XX'-VERLUST-AKTIEN      PIC  X(007).
 
               20  'XX'-VERLUST-SONSTIGE    PIC  9(007)V99.
      *             VERLUSTTOPF SONSTIGE AKTUELLER BALANCE-WERT
               20  'XX'-VERLUST-SONSTIGE-R REDEFINES
                   'XX'-VERLUST-SONSTIGE    PIC  X(007).
 
               20  'XX'-ANRECHEN-QUELLENST  PIC  9(007)V99.
      *             TOPF ANRECHENBARE QUELLEN-STEUER AKTUELLER
      *             BALANCE-WERT
               20  'XX'-ANRECHEN-QUELLENST-R REDEFINES
                   'XX'-ANRECHEN-QUELLENST  PIC  X(007).
 
               20  'XX'-VERL-BESCH-AKTIEN   PIC  X(001).
      *             VERLUSTBESCHEINIGUNG ZUM AKTIENTOPF FÜR
      *             AKTUELLES JAHR ANGEFORDERT J/N
               20  'XX'-VERL-BESCH-SONSTIGE PIC  X(001).
      *             VERLUSTBESCHEINIGUNG ZUM SONSTIGE FÜR
      *             AKTUELLES JAHR ANGEFORDERT J/N
               20  'XX'-ZUG-KONFESSION-1    PIC  X(002).
      *             ZUGEHÖRIGKEIT KONFESSION 1
               20  'XX'-ZUG-KONFESSION-2    PIC  X(002).
      *             ZUGEHÖRIGKEIT KONFESSION 2
      *
               20  'XX'-KIST-SATZ1          PIC  9(002)V99.
      *             KIRCHENSTEUER-SATZ 1
               20  'XX'-KIST-SATZ1-R REDEFINES
                   'XX'-KIST-SATZ1          PIC  X(004).
 
               20  'XX'-KIST-SATZ2          PIC  9(002)V99.
      *             KIRCHENSTEUER-SATZ 2
               20  'XX'-KIST-SATZ2-R REDEFINES
                   'XX'-KIST-SATZ2          PIC  X(004).

               20  'XX'-EINB-KIST-AB        PIC  9(008).
      *             EINBEHALT DER KIRCHENSTEUER AB JHJJMMTT
               20  'XX'-AUFT-MASSST-KONF1   PIC  9(003).
      *             AUFTEILUNGSMASSSTAB
      *             KONFESSIONSZUGEHÖRIGKEIT 1 IN %
               20  'XX'-AUFT-MASSST-KONF2   PIC  9(003).
      *             AUFTEILUNGSMASSSTAB
      *             KONFESSIONSZUGEHÖRIGKEIT 2 IN %
               20  'XX'-FSA-ID              PIC X(008).
               20  FILLER                   PIC X(031).
 
           10  'XX'-NACHSATZ REDEFINES 'XX'-VORSATZ.
               20  'XX'-N-SA            PIC X(001).
      *            SATZART             'E'
               20  'XX'-N-ANZ           PIC 9(007) COMP-3.
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER               PIC X(192).
      *            RESERVE




