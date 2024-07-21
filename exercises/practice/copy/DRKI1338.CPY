      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  JANUAR 2001
      *            KOPF AUS:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 2001 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $WORKFILE:   DRKI1338.CPY  $
      *    $REVISION:   2.11  $
      *
      *    $LOG:   L:/MF/CPY/DRKI1338.CPV  $
      *
      *   REV 2.11   MAY 23 2017 15:40:20   T13EX39
      *AKI01512: DEUBA MAI-RELEASE
      *
      *   REV 2.10   MAY 23 2017 15:34:50   T13EX39
      *AKI01512: DEUBA MAI-RELEASE
      *
      *   REV 2.9   MAY 23 2017 15:07:02   T13EX39
      *AKI01512: DEUBA MAI-RELEASE
      *
      *   REV 2.8   JUN 28 2011 09:55:12   T13AE31
      *AKI01040: ERW. DEUBA- INKL. DWS-DATEN / AUSGABE IN KI-VERTRA
      *G FSA
      *
      *   REV 2.7   NOV 16 2009 18:41:18   T13EX51
      *AKI00866: ABGELTUNGSSTEUERDATEN: ANPASSEN ERMITTLUNG
      *
      *   REV 2.6   NOV 13 2009 10:48:16   T13EX51
      *AKI00866: ABGELTUNGSSTEUERDATEN: ANPASSEN ERMITTLUNG
      *
      *   REV 2.5   NOV 12 2009 13:32:40   T13EX51
      *AKI00866: ABGELTUNGSSTEUERDATEN: ANPASSEN ERMITTLUNG
      *
      *   REV 2.4   MAY 27 2009 10:40:10   T13EX39
      *AKI00836: ABGELTUNGSSTEUER DEUBA SATZERWEITERUNG
      *
      *   REV 2.3   FEB 25 2003 15:44:50   T13EX12
      *AKI00328 'XX'-KUNDEN OHNE ANLAGEDEPOT
      *
      *   REV 2.2   JAN 27 2003 10:57:34   T13EX39
      *AKI00162 DEUTSCHE BANK NEUE FELDER
      *
      *   REV 2.1   JUL 30 2002 16:33:44   T13EX39
      *AKI00274 DB/24
      *
      *   REV 2.0   DEC 14 2001 09:21:16   P13SP20
      *ARCHIVERSTELLUNG
      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR FSA KOMPLETTBESTAND DEUTSCHE BANK           *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    LÄNGE = 100 BYTES                                           *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00274!        !01  ! NEU                       !W.HORNUNG *
      * -------------------------------------------------------------*
      * AKI00162!14.11.02!02  ! STÜCKZINSEN MIT VORZEICHEN!W.HORNUNG *
      * -------------------------------------------------------------*
      * AKI00328!17.02.03!03  ! NEUES KENNZ. FÜR KUNDEN   !V.D.GOLTZ *
      *         !        !    ! DEPOT.                    !          *
      * -------------------------------------------------------------*
      * AKI00863!27.05.09!04  ! NEUE SATZLÄNGE            ! WHO      *
      *         !        !    ! JETZT 200                 !          *
      * -------------------------------------------------------------*
      * AKI00866!26.10.09!05  ! NEUE FELDER FÜR           ! HRO      *
      *         !        !    ! ABGELTUNGSSTEUER          !          *
      * -------------------------------------------------------------*
      * AKI01040!24.05.11!06  ! NEUES FELD FSA-NR.        ! HHU      *
      *--------------------------------------------------------------*
      * FKI15124!24.04.17! FELD 'XX'-EINBEH-KEST UM 2     ! WHO      *
      *         !        ! STELLEN ERWEITERT PIC 9(07)    !          *
      * ------------------------------------------------------------ *
      * AKI01726!19.02.20! 'XX' STATT  DB FUER TT         !M.ATAR    *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1338.
           10  'XX'-VORSATZ.
      *
               20  'XX'-V-SA            PIC X(001).
      *            SATZART = '1'
               20  'XX'-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'DB AG'
               20  'XX'-V-DATUM         PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  'XX'-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  'XX'-V-TEXT          PIC X(008).
      *            INHALTSANGABE:
      *            DEPOTBESTAND     TÄGLICH   ‘FSAT'
      *            DEPOTBESTAND     KOMPLETT  ‘FSAK'
               20  'XX'-V-WKZ           PIC X(003).
      *            WÄHRUNGS-KENNZ. =  'EUR'
               20  FILLER             PIC X(168).
      *            RESERVE


           10  'XX'-SATZ     REDEFINES      'XX'-VORSATZ.
      *
               20  'XX'-SA                  PIC  X(001).
      *            SATZART        = '4'
               20  'XX'-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '48'
               20  'XX'-VNR.
                   25  'XX'-FILIAL-NR       PIC 9(03).
                   25  'XX'-KDNR            PIC 9(07).
               20  'XX'-VNR-1 REDEFINES 'XX'-VNR PIC 9(10).
      *            VERTRAGSNUMMER
               20  'XX'-VBNR                PIC  9(007).
      *            ABSCHLUSSVERMITTLER
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

               20  'XX'-DEPOT-AKTIV         PIC  9(001).
...   *              KENNZ. = 1, KUNDE HAT AKTIVES DEPOT
...   *              KENNZ. = 0, KUNDE HAT KEIN DEPOT
...
               20  'XX'-DEL-KZ              PIC  X(001).
...   *              KZ = D, SATZ IST ZU LÖSCHEN
      *              KZ = S, DATENSCHUTZSPERRE AKTIVIERT
               20  'XX'-STEUERAUSL          PIC  X(001).
      *              STEUERAUSLÄNDERKENNZEICHEN DER KUNDENSTAMMNUMMER
      *              J/N
               20  'XX'-NICHTVERANL-BIS-DAT PIC  9(008).
      *              NICHTVERANLAGUNGSBESCHEINIGUNG - GÜLTIG BIS
      *              JHJJMMTT
               20  'XX'-AUSG-FSA-IM-VJ      PIC  9(005)V99.
      *              AUSGENUTZTER FREISTELLUNGSBETRAG IM VORJAHR IN EUR
               20  'XX'-AUFGEL-ERTRAG       PIC  9(007)V99.
      *              AUFGELAUFENER ERTRAG GESAMT AKTUELLES JAHR
      *              (KUMULIERT)
      *        20  'XX'-EINBEH-KEST         PIC  9(005)V99.
               20  'XX'-EINBEH-KEST         PIC  9(007)V99.
      *              EINBEHALTENE KEST AKTUELLES JAHR (KUMULIERT)
               20  'XX'-EINBEH-SOLI         PIC  9(005)V99.
      *              EINBEHALTENER SOLI AKTUELLES JAHR (KUMULIERT)
               20  'XX'-EINBEH-KIST         PIC  9(005)V99.
      *              EINBEHALTENE KIRCHENSTEUER AKTUELLES JAHR
      *              (KUMULIERT)
               20  'XX'-VERLUST-AKTIEN      PIC  9(007)V99.
      *              VERLUSTTOPF AKTIEN AKTUELLER BALANCE-WERT
               20  'XX'-VERLUST-SONSTIGE    PIC  9(007)V99.
      *              VERLUSTTOPF SONSTIGE AKTUELLER BALANCE-WERT
               20  'XX'-ANRECHEN-QUELLENST  PIC  9(007)V99.
      *              TOPF ANRECHENBARE QUELLEN-STEUER AKTUELLER
      *              BALANCE-WERT
               20  'XX'-VERL-BESCH-AKTIEN   PIC  X(001).
      *              VERLUSTBESCHEINIGUNG ZUM AKTIENTOPF FÜR
      *              AKTUELLES JAHR ANGEFORDERT J/N
               20  'XX'-VERL-BESCH-SONSTIGE PIC  X(001).
      *              VERLUSTBESCHEINIGUNG ZUM SONSTIGE FÜR
      *              AKTUELLES JAHR ANGEFORDERT J/N
               20  'XX'-ZUG-KONFESSION-1    PIC  X(002).
      *              ZUGEHÖRIGKEIT KONFESSION 1
               20  'XX'-ZUG-KONFESSION-2    PIC  X(002).
      *              ZUGEHÖRIGKEIT KONFESSION 2
               20  'XX'-KIST-SATZ1          PIC  9(002)V99.
      *              KIRCHENSTEUER-SATZ 1
               20  'XX'-KIST-SATZ2          PIC  9(002)V99.
      *              KIRCHENSTEUER-SATZ 2
               20  'XX'-EINB-KIST-AB        PIC  9(008).
      *              EINBEHALT DER KIRCHENSTEUER AB JHJJMMTT
               20  'XX'-AUFT-MASSST-KONF1   PIC  9(003).
      *              AUFTEILUNGSMASSSTAB
      *              KONFESSIONSZUGEHÖRIGKEIT 1 IN %
               20  'XX'-AUFT-MASSST-KONF2   PIC  9(003).
...   *              AUFTEILUNGSMASSSTAB
      *              KONFESSIONSZUGEHÖRIGKEIT 2 IN %
               20  'XX'-FSA-NUMMER          PIC X(30).
      *        20  FILLER                 PIC X(5).
               20  FILLER                 PIC X(3).

           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *
               20  'XX'-N-SA            PIC X(001).
      *            SATZART             '9'
               20  'XX'-N-ANZ           PIC 9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
               20  FILLER              PIC X(192).
      *            RESERVE






