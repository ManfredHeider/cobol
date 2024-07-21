       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PMAN0076.
       AUTHOR.        RAHNER.
      *
      ****************************************************************
      *                                                              *
      *   BESCHREIBUNG: ERSTELLEN DER AUSWERTUNG ERFASSTE ANTRÄGE    *
      *                 EINES TAGES FÜR DVAG-WIEN                    *
      *                 AUS TÄGLICH EINGEREICHETN GESCHÄFT           *
      *                 (TDS-SÄTZE, DATEI P130006.UU0301IA.D1.CL)    *
      *                                                              *
      *   ABLAUF: SEQ. LESEN DER TDS-SÄTZE.                          *
      *           BERÜCKSICHTIGT WERDEN SATZART I,UN,D,DU,U2,I2      *
      *           AUFBEREITEN DER DATEN FÜR DIE AUSWERTUNG           *
      *           SCHREIBEN IN SEQ.AUSGABEDATEI.                     *
      *                                                              *
      ****************************************************************
      *                                                              *
      *   --------->  B A T C H - N O R M R A H M E N  <-----------  *
      *   --------->        N GRUPPENWECHSEL           <-----------  *
      *   --------->        1 EINGABEDATEI             <-----------  *
      *                                                              *
      *   DIESES PROGRAMM WURDE MIT EINEM BATCH-NORMRAHMEN ERSTELLT. *
      *   DER RAHMEN IST FÜR DIE BEARBEITUNG MEHRERER GRUPPENWECHSEL *
      *   UND EINER EINGABEDATEI / EINES EINGABE-CURSORS KONZIPIERT. *
      *                                                              *
      *                       *************                          *
      *                       *  ACHTUNG  *                          *
      *                       *************                          *
      *       +---------------------------------------------+        *
      *       !  VORAUSSETZUNGEN FÜR DEN RAHMEN:            !        *
      *       !  - S-DATEI-STATUS MUSS DEFINIERT SEIN       !        *
      *       !  - K-PGM-NAME     MUSS ANGELEGT SEIN        !        *
      *       !  - DIV. COPYS MÜSSEN VORH. SEIN (JEWEILS    !        *
      *       !       IM KOMMENTAR ZU DEN COPYS ANGEGEBEN)  !        *
      *       +---------------------------------------------+        *
      *                                                              *
      *                                                              *
      *   ALLGEMEINE VERARBEITUNGSHINWEISE                           *
      *   --------------------------------                           *
      *                                                              *
      *   - 'PM-#----' IST DURCH DEN PROGRAMMNAMEN ZU ERSETZEN       *
      *                                                              *
      *   - ALLE RELEVANTEN ZEILEN FÜR GRUPPENWECHSEL SIND IN DEN    *
      *     SPALTEN 5 BIS 7 MARKIERT UND MÜSSEN BEI BEDARF AKTIVIERT *
      *     BZW. GELÖSCHT WERDEN:                                    *
      *       - MARKIERUNG FÜR EINSTUFIGEN  GRUPPENWECHSEL: 'G1*'    *
      *       - MARKIERUNG FÜR ZWEISTUFIGEN GRUPPENWECHSEL: 'G2*'    *
      *                                                              *
      *   - AM ENDE DER DATEI/CURSOR VERSORGEN VON S-NZ MIT          *
      *     K-ZUSTAND-EOF. DIE RAHMENSTEUERUNG VERZWEIGT DANN        *
      *     DIREKT ZU SÄMTLICHEN NACHLAUF-ROUTINEN (N2 N1 N0).       *
      *                                                              *
      *                                                              *
      *   REGELN FÜR FEHLERHANDLING:                                 *
      *   -------------------------                                  *
      *   - VERWENDUNG DER STANDARD-FEHLER-ROUTINEN ZUR FEHLERAUF-   *
      *     BEREITUNG UND AUSGABE (BR91, BR93, BR94, BR95, BR99)     *
      *                                                              *
      *   - NACH DEM AUFRUF VON BR91, BR93, BR94, BR95 ODER BR99 MUß *
      *     DIE ROUTINE "U01-ABBRUCH" (=PROGRAMMABBRUCH) AUFGERUFEN  *
      *     WERDEN. HIER KANN EIN ROLLBACK DURCHGEFÜHRT WERDEN.      *
      *                                                              *
      *     *******                                                  *
      *     ACHTUNG: U01-ABBRUCH BEENDET DAS PROGRAMM !!!            *
      *     *******                                                  *
      *                                                              *
      ****************************************************************
      * LOGBUCH                                                      *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00887!22.05.09!2.1 ! NEU                       ! RAHNER   *
      * FAN00888!06.07.09!2.2 ! GES.26: PRÜFUNG STATUS=01 ! HEIDER   *
      * FAN00909!17.11.09!2.3 ! STATUS "D " KORREKT SETZEN! RAHNER   *
      * FAN00909!27.11.09!2.4 ! TIMESTAMP HINZU           ! RAHNER   *
      * AAN01032!23.01.12!2.5 ! KOMP. ÜBERARBEITUNG       ! RAHNER   *
      * AAN01051!15.05.12!2.6 !INTERNE ID-NUMMERN GENERALI! RAHNER   *
      * FAN01005!08.06.12!2.7 !KORREKTUR FÜLLEN FELD GEWO ! RAHNER   *
      * FAN01018!30.07.13!2.8 !SEPA ÖSTERREICH            ! RAHNER   *
      * FAN01034!25.08.14!2.9 !KORREKTUR DATUMSBEARBEITUNG! RAHNER   *
      * FAN01035!04.09.14!2.10!VERFEINERUNG DATUMSERMITTL.! RAHNER   *
      * AAN01240!24.02.16!2.14! C+L WEGEN DRAN0140        ! RAHNER   *
      * AAN01334!01.09.17!2.17!NEU GES. 33 CAPITAL-BANK   ! RAHNER   *
      * AAN01354!20.03.18!2.19!WEITERE INHABER ÖSTERREICH ! RAHNER   *
      * FAN01128!17.04.18!2.22! C+L WEGEN DRAN0140        ! RAHNER   *
      * FAN01150!18.12.18!2.23!JAHRH.BERECHNUNG ANGEPASST ! RAHNER   *
      * AAN01410!15.01.19!2.24!C+L WEGEN DRAN0207         ! RAHNER   *
      * FAN01198!15.10.20!2.25!JAHRH.BERECHN. BEGINN/ENDE ! RAHNER   *
      * AN-1022 !27.05.21!    !NEUE GES.55                ! RAHNER   *
      *                                                              *
      * AENDERUNGEN:                                                 *
      * AUFTRAG     ! DATUM  ! AENDERUNG                 ! NAME      *
      * -------------------------------------------------------------*
      * ANTRAG-1090 !14.09.21! ELEKTR. ANTRAG GES. 33    ! RAHNER    *
      * ANTRAG-1924 !18.01.23! VNR GES. 85 IN E-GES-LISTE! RAHNER    *
      * ANTRAG-1928 !21.03.23! NEU FELD DEVISENINLÄNDER  ! RAHNER    *
      * ANTRAG-2013 !22.03.23! C+L WG. DRAN0140          ! RAHNER    *
      * ANTRAG-2028 !30.03.23! FEHLER FÜLLEN DIL-FELDER  ! RAHNER    *
      * ANTRAG-2088 !11.05.23! FEHLER FÜLLEN DIL-FELDER 2! RAHNER    *
      *             !        !                           !           *
      *             !        !                           !           *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-4381.
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           SELECT  TDS-EINGABE ASSIGN    TO  AS-EINGABE
                   FILE STATUS           IS  S-TDSFILE-EIN-STATUS.
      *
           SELECT  WIEN-AUSGABE ASSIGN   TO  AUSGABE
                   ACCESS MODE           IS  SEQUENTIAL
                   ORGANIZATION          IS  SEQUENTIAL
                   FILE STATUS           IS  S-WIEN-AUS-STATUS.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *
       FD  TDS-EINGABE
           LABEL RECORD STANDARD.
CCC021 01 FILLER PIC X.
      *
       FD  WIEN-AUSGABE
           LABEL RECORD STANDARD.
CCC021 01 FILLER PIC X.
      *
      ****************************************************************
       WORKING-STORAGE SECTION.
      ****************************************************************
CCC004     COPY DRAN0163.
CCC004     COPY DRAN0227 REPLACING DRAN0227 BY WIEN-SATZ-AUS.

       01  FILLER.
           05  FILLER               PIC X(024)
                                    VALUE '*** WORKING-STORAGE ***'.
      *
      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
      *  DATENFELDER FÜR BATCH-RAHMENSTEUERUNG                     *
      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
      *------------------------------------------------------------*
      * ERR-ERROR-BEREICH  -  STANDARD-FEHLER-BEREICH              *
      * BENÖTIGT: IMMER                                            *
      *------------------------------------------------------------*
       COPY DRAT0004.
      *------------------------------------------------------------*
      * D-ERROR-BEREICH  -  DRUCKAUFBEREITUNG ERROR-BEREICH        *
      * BENÖTIGT: IMMER                                            *
      *------------------------------------------------------------*
       COPY DRAT0006.
      *------------------------------------------------------------*
      * D-DB2S  -  DRUCKAUFBEREITUNG SCHNITTST. DB2-SCHREIBMODUL   *
      * BENÖTIGT: IMMER                                            *
      *------------------------------------------------------------*
       COPY DRAT0021.
      *------------------------------------------------------------*
      * PARAMETER-STEUER/-RETURN  -  SCHNITTST. DB2-SCHREIBMODUL   *
      * BENÖTIGT: IMMER                                            *
      *------------------------------------------------------------*
       COPY DRAT0007.
      *------------------------------------------------------------*
      * STANDARD-STATUS-BEREICH                                    *
      * BENÖTIGT: IMMER                                            *
      *------------------------------------------------------------*
       COPY DRAT0109.
      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
      *  ENDE DATENFELDER FÜR BATCH-RAHMENSTEUERUNG                *
      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>*
      *01  FILLER.
           05  FILLER               PIC  X(032)
                                    VALUE '*** DB2-BEREICHE ***'.
      *    INCLUDE FÜR DB2 SPEZIFISCHE DATENFELDER
CCC009*     EXEC SQL
                 COPY SQLCA.
CCC005*     END-EXEC.
      *
      **************************************************************
       01  FILLER.
           05  FILLER               PIC X(032)
                                    VALUE '*** DRUCKBEREICH ***'.
      **************************************************************
       01  FILLER.
           05  FILLER               PIC X(032)
                                    VALUE '*** KONSTANTEN ***'.
      *
      *------------------------------------------------------------*
      * K-PGM-NAME                                                 *
      * BENÖTIGT: IMMER                                            *
      *------------------------------------------------------------*
      *
           05  K-PGM-NAME                PIC X(008) VALUE 'PMAN0076'.
           05  K-UWAN0001                PIC X(008) VALUE 'UWAN0001'.
           05  K-ZWAT0008                PIC X(008) VALUE 'ZWAT0008'.
           05  K-UPAN0019                PIC X(008) VALUE 'UPAN0019'.
           05  K-UPAN0020                PIC X(008) VALUE 'UPAN0020'.
           05  K-UPAN0023                PIC X(008) VALUE 'UPAN0023'.
           05  K-UPPA0016                PIC X(008) VALUE 'UPPA0016'.
      *
      *    * KONSTANTEN FÜR STANDARD-RAHMEN (ZUSTAND)
           05  K-ZUSTAND-OK              PIC X(001) VALUE '1'.
           05  K-ZUSTAND-EOF             PIC X(001) VALUE '2'.
      *
      *    * STANDARD-KONSTANTEN
CCC009*    EXEC SQL
              COPY DRAT0009.
CCC005*    END-EXEC.
 
      *    KTY-KONSTANTEN
           COPY DRAT0044.
 
      *    * KONSTANTEN-GESELLSCHAFTEN
CCC009*    EXEC SQL
              COPY DRAT0084.
CCC005*    END-EXEC.
      *
      ****************************************************************
       01  FILLER.
           05  FILLER               PIC  X(032)
                                VALUE '*** SCHALTER /STEUERFELDER ***'.
      *
      *------------------------------------------------------------*
      * STEUERFELDER GRUPPENWECHSELSTEUERUNG                       *
      * BENÖTIGT: IMMER                                            *
      *------------------------------------------------------------*
           05 S-NEU.
    G2*       10 S-NG2.
    G1*          15 S-NG1.
                    20 S-NZ         PIC  X(001)        VALUE SPACE.
    G1*             20 S-N1         PIC S9(004) COMP   VALUE ZERO.
    G2*          15 S-N2            PIC S9(004) COMP   VALUE ZERO.
 
           05 S-ALT.
    G2*       10 S-AG2.
    G1*          15 S-AG1.
                    20 S-AZ         PIC  X(001)        VALUE SPACE.
    G1*             20 S-A1         PIC S9(004) COMP   VALUE ZERO.
    G2*          15 S-A2            PIC S9(004) COMP   VALUE ZERO.
 
      *----------------------------------------------------------------*
      * S-DATEI-STATUS                                                 *
      * BENÖTIGT: IMMER                                                *
      *----------------------------------------------------------------*
           05  S-DATEI-STATUS       PIC  X(002) VALUE SPACE.
               88 S-DATEI-OK                    VALUE '00'.
      *
           05  S-TDSFILE-EIN-STATUS PIC  X(002) VALUE SPACE.
               88 S-TDSFILE-EIN-OK              VALUE '00'.
               88 S-TDSFILE-EIN-EOF             VALUE '10'.
      *
           05  S-WIEN-AUS-STATUS    PIC  X(002) VALUE SPACE.
               88 S-WIEN-AUS-OK                 VALUE '00'.
      *
           05  S-VERARBEITUNG       PIC  9(001) VALUE ZERO.
               88 S-VERARBEITUNG-OK             VALUE 1.
               88 S-VERARBEITUNG-NOK            VALUE ZERO.
 
           05  S-TAGESDATUM          PIC  9(001) VALUE ZERO.
               88 S-TAGESDATUM-OK                VALUE 1.
               88 S-TAGESDATUM-NOK               VALUE ZERO.
 
      ******************************************************************
       01  FILLER.
           05  FILLER               PIC X(032)
                                    VALUE '*** TABELLEN-BEREICH   ***'.
      ******************************************************************
       01  FILLER.
           05  FILLER                        PIC X(032)
                                    VALUE '*** ZWISCHENFELDER ETC ***'.
      *
           05 Z-TAGESDATUM-EUR        PIC X(010).
           05 Z-TAGESDATUM-TDS-FORMAT PIC 9(007).
           05 Z-UHRZEIT-TDS-FORMAT    PIC 9(009).
           05 Z-TAGESDATUM-JJ         PIC 9(002).
           05 Z-TAGESDATUM-TDS-LANG.
              10 Z-TAGESDATUM-TDS-LANG-TT PIC 9(002).
              10 Z-TAGESDATUM-TDS-LANG-P1 PIC X(001) VALUE '.'.
              10 Z-TAGESDATUM-TDS-LANG-MM PIC 9(002).
              10 Z-TAGESDATUM-TDS-LANG-P2 PIC X(001) VALUE '.'.
              10 Z-TAGESDATUM-TDS-LANG-JHJJ  PIC 9(004).
              10 Z-TAGESDATUM-TDS-LANG-JHJJ-R REDEFINES
                 Z-TAGESDATUM-TDS-LANG-JHJJ.
                 20 Z-TAGESDATUM-TDS-LANG-JH PIC X(002).
                 20 Z-TAGESDATUM-TDS-LANG-JJ PIC X(002).
              10 Z-TAGESDATUM-TDS-LANG-JHJJ-9 REDEFINES
                 Z-TAGESDATUM-TDS-LANG-JHJJ.
                 20 Z-TAGESDATUM-TDS-LANG-JH-9 PIC 9(002).
                 20 Z-TAGESDATUM-TDS-LANG-JJ-9 PIC 9(002).
      
           05 Z-ZAEHLER               PIC 9(003).
           05 Z-ZAEHLER2              PIC 9(003).
      *
           05 Z-ZAEHLER-EINGABE       PIC 9(8) VALUE ZERO.
           05 Z-ZAEHLER-AUSGABE       PIC 9(8) VALUE ZERO.
      *
           05 Z-BEITRAG-NUM           PIC 9(7)V99.
           05 Z-BEITRAG-NUM-2         PIC 9(7)V99.
           05 Z-BEITRAG-NUM-3         PIC 9(7)V99.
           05 Z-BEITRAG-FORMATIERT    PIC ZZZZZZZ,ZZ.
           05 Z-PROD-SCHL-FORMATIERT  PIC ZZZZ.
           05 Z-SOZ-NR-FORMATIERT     PIC X(10).
           05 Z-SOZ-NR-FORMATIERT-R REDEFINES Z-SOZ-NR-FORMATIERT.
              10 Z-SOZ-NR-1BIS4       PIC X(4).
              10 Z-SOZ-NR-5-10        PIC X(6).
      
           05 Z-UMLAUTE-GROSS         PIC X(40).
      *
           05 Z-WIEN-SATZ-KOPF-UEBERSCHR.
            10 Z-WIEN-SATZ-KOPF-SATZART   PIC X(2)  VALUE 'SA'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-1   PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-VBNR      PIC X(7)  VALUE 'VBNR   '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-2   PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-GWO-VBNR  PIC X(7)  VALUE 'GEWO-VB'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-3   PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-VMNR      PIC X(7)  VALUE 'VMNR   '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-4   PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PIN
               PIC X(15) VALUE 'PIN            '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-5   PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-VNR-ANTR
               PIC X(15) VALUE 'VNR-ANTR       '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-6   PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-VORSCHL-NR
               PIC X(9) VALUE 'VORLSCHL '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-7     PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-VORNAME
               PIC X(26) VALUE 'VORNAME-KUNDE             '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-8     PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-ZUNAME
               PIC X(26)  VALUE 'ZUNAME-KUNDE'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-9     PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-NUMMER
               PIC X(18)  VALUE 'KUNDE-BESTAND-ID  '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-10    PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-GEB-DAT
               PIC X(10)  VALUE 'KD-GEBDAT '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-11    PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-SOZVNR
               PIC X(10)  VALUE 'SOZVNR    '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-12    PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AUSL-KZ     PIC X(3)  VALUE 'AKZ'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-13    PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-PLZ      PIC X(6)  VALUE 'PLZ   '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-14    PIC X(1)  VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-ORT
               PIC X(21) VALUE 'ORT'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-15    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-STRASSE
               PIC X(26) VALUE 'STRASSE'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-16    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PGEB-GES    PIC X(2) VALUE 'GE'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-17    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PROD-SCHL-1 PIC X(4) VALUE 'P1--'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-18    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PROD-SCHL-2 PIC X(4) VALUE 'P2--'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-19    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PROD-SCHL-3 PIC X(4) VALUE 'P3--'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-20    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PROD-SCHL-4 PIC X(4) VALUE 'P4--'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-21    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PROD-SCHL-5 PIC X(4) VALUE 'P5--'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-22    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PROD-SCHL-6 PIC X(4) VALUE 'P6--'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-23    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PROD-SCHL-7 PIC X(4) VALUE 'P7--'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-24    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PROD-SCHL-8 PIC X(4) VALUE 'P8--'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-25    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PROD-SCHL-9 PIC X(4) VALUE 'P9--'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-26    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-TARIF      PIC X(10) VALUE 'TARIF     '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-27    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AN-BEITRAG PIC X(10) VALUE 'BEITRAG   '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-28    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AN-BEITR-SUM
               PIC X(10) VALUE 'BEITRG-SUM'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-29    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AN-SUMMME
               PIC X(10) VALUE 'SUMME     '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-30    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AN-BARB-GEB
               PIC X(5) VALUE 'B-GEB'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-31    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AN-ZAHLW
               PIC X(3) VALUE 'ZW '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-32    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-EINMERLARGE
               PIC X(10) VALUE 'EINMAL-ERL'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-33    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AN-BEGINN
               PIC X(10) VALUE 'BEGINN    '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-34    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AN-ABLAUF
               PIC X(10) VALUE 'ABLAUF    '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-35    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PDZ
               PIC X(3) VALUE 'PZD'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-36    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AN-JNP
               PIC X(10) VALUE 'JNP       '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-37    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PRAEM-SUM
               PIC X(10) VALUE 'PRAEM-SUM '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-38    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-EINHEITEN
               PIC X(10) VALUE 'EINHEITEN '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-39    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-ANTRAGS-ART
               PIC X(3) VALUE 'ART'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-40    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-ANTRAGS-DAT
               PIC X(10) VALUE 'ANTR-DAT  '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-41    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-STRUK-DAT
               PIC X(10) VALUE 'STRUK-DAT '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-42    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-ERF-DAT
               PIC X(10) VALUE 'ERF-DAT   '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-43    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-STAT-DAT
               PIC X(10) VALUE 'STAT-DAT  '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-44    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-ERF-ID      PIC X(7) VALUE 'ERF-ID '.
            10 Z-WIEN-SATZ-KOPF-SEMIK45     PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-TIMESTAMP
               PIC X(19) VALUE 'ERFASSUNGS-ZEIT-P  '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-46    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-STAATS-ANG
               PIC X(5) VALUE 'STAAN'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-47    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PEP-STATUS
               PIC X(3) VALUE 'PEP'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-48    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-BRANCHE
               PIC X(40) VALUE 'BRANCHE'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-49    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-BRUFSGRP
               PIC X(40) VALUE 'BERUFSGRUPPE'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-50    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-GEN-ANT-NR
               PIC X(16) VALUE 'GEN ANTRAG-NR   '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-51    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-GEN-B-ANTR-NR
               PIC X(16) VALUE 'GEN B-ANTRAG-NR '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-52    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-2-VORNAME
               PIC X(26) VALUE 'VORNAME-KUNDE2'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-53    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-2-ZUNAME
               PIC X(26) VALUE 'ZUNAME-KUNDE2'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-54    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD2-GEB-DAT
               PIC X(10)  VALUE 'KD-GEBDAT2'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-55    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AUSL-KZ2
               PIC X(4) VALUE 'AKZ2'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-56    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-PLZ2
               PIC X(6)  VALUE 'PLZ2  '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-57    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-ORT2
               PIC X(21) VALUE 'ORT2'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-58    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-STAAN2
               PIC X(6) VALUE 'STAAN2'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-59    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PEP-STATUS2
               PIC X(4) VALUE 'PEP2'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-60    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-BRANCHE2
               PIC X(40) VALUE 'BRANCHE2'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-61    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-BRUFSGRP2
               PIC X(40) VALUE 'BERUFSGRUPPE2'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-62    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-3-VORNAME
               PIC X(26) VALUE 'VORNAME-KUNDE3'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-63    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-3-ZUNAME
               PIC X(26) VALUE 'ZUNAME-KUNDE3'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-64    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD3-GEB-DAT
               PIC X(10)  VALUE 'KD-GEBDAT3'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-65    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AUSL-KZ3
               PIC X(4) VALUE 'AKZ3'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-66    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-PLZ3
               PIC X(6)  VALUE 'PLZ3  '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-67    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-ORT3
               PIC X(21) VALUE 'ORT3'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-68    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-STAAN3
               PIC X(6) VALUE 'STAAN3'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-69    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PEP-STATUS3
               PIC X(4) VALUE 'PEP3'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-70    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-BRANCHE3
               PIC X(40) VALUE 'BRANCHE3'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-71    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-BRUFSGRP3
               PIC X(40) VALUE 'BERUFSGRUPPE3'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-72    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-4-VORNAME
               PIC X(26) VALUE 'VORNAME-KUNDE4'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-73    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-4-ZUNAME
               PIC X(26) VALUE 'ZUNAME-KUNDE4'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-74    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD4-GEB-DAT
               PIC X(10)  VALUE 'KD-GEBDAT4'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-75    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AUSL-KZ4
               PIC X(4) VALUE 'AKZ4'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-76    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-PLZ4
               PIC X(6)  VALUE 'PLZ4  '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-77    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-ORT4
               PIC X(21) VALUE 'ORT4'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-78    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-STAAN4
               PIC X(6) VALUE 'STAAN4'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-79    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PEP-STATUS4
               PIC X(4) VALUE 'PEP4'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-80    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-BRANCHE4
               PIC X(40) VALUE 'BRANCHE4'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-81    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-BRUFSGRP4
               PIC X(40) VALUE 'BERUFSGRUPPE4'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-82    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-5-VORNAME
               PIC X(26) VALUE 'VORNAME-KUNDE5'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-83    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-5-ZUNAME
               PIC X(26) VALUE 'ZUNAME-KUNDE5'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-84    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD5-GEB-DAT
               PIC X(10)  VALUE 'KD-GEBDAT5'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-85    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AUSL-KZ5
               PIC X(4) VALUE 'AKZ5'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-86    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-PLZ5
               PIC X(6)  VALUE 'PLZ5  '.
            10 Z-WIEN-SATZ-KOPF-SEMIK-87    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-ORT5
               PIC X(21) VALUE 'ORT5'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-88    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-STAAN5
               PIC X(6) VALUE 'STAAN5'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-89    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-PEP-STATUS5
               PIC X(4) VALUE 'PEP5'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-90    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-BRANCHE5
               PIC X(40) VALUE 'BRANCHE5'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-91    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-BRUFSGRP5
               PIC X(40) VALUE 'BERUFSGRUPPE5'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-92    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-AKT-ANTR-STA
               PIC X(18) VALUE 'AKT.-STATUS-ANTRAG'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-93    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-KOPF-KD-NUMMER-2
               PIC X(18) VALUE 'KUNDE-BESTAND-ID-2'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-94    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-DATEN-DIL-STA-INH1 
               PIC X(004) VALUE 'DIL1'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-95    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-DATEN-DIL-STA-INH2 
               PIC X(004) VALUE 'DIL2'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-96    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-DATEN-DIL-STA-INH3 
               PIC X(004) VALUE 'DIL3'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-97    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-DATEN-DIL-STA-INH4 
               PIC X(004) VALUE 'DIL4'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-98    PIC X(1) VALUE ';'.
            10 Z-WIEN-SATZ-DATEN-DIL-STA-INH5 
               PIC X(004) VALUE 'DIL5'.
            10 Z-WIEN-SATZ-KOPF-SEMIK-99    PIC X(1) VALUE ';'.
            
      ***  'ANTRAG-DATEN' WORKFLOW ALT UND NEU
      ***  - DER WF-ALT BLEIBT UNVERÄNDERT, ER DIENT DEM ABGLEICH
      ***    DER EINGEGEBENEN DATEN GEGEN DEN URSPRUNGSZUSTAND
      ***  - DER WF-NEU NIMMT ALLE NEUEN BZW. GEÄNDERTEN DATEN AUF,
      ***    ER WIRD ZUM PRÜFEN, BERECHNEN UND SPEICHERN VERWENDET
      *    WF-ALT
           COPY DRAN0132.
      *    WF-NEU
           COPY DRAN0145.
      *
      ***  GENERALI SACH  (GES. 16)
            10  Z-ELEKTR-DATEN-GENS REDEFINES WF-ANTRAG-VORSCHLAG-NEU.
                COPY DRAN0221.
      *
      ***  GENERALI LEBEN (GES. 26)
            10  Z-ELEKTR-DATEN-GENL REDEFINES WF-ANTRAG-VORSCHLAG-NEU.
                COPY DRAN0204.
      *
      **************************************************************
       01  FILLER.
           05  FILLER               PIC X(32)
                                    VALUE '*** UEBERGABEPARAMETER ***'.
      *------------------------------------------------------------*
      *     FUNKTIONEN                                             *
      *------------------------------------------------------------*
      *************
      ***  UWAN0001 - FUNKTIONEN
           COPY DRAN0139.
      *    FUNKTION - ERMITTELN ANTRAG
           COPY DRAN0140.
      *
      *************
      ***  ZWAT0008 - FUNKTIONEN
           COPY DRAT0138.
      ***  FUNKTION - ERMITTELN ATTRIBUTE ZU KTY-NR, KNR-NR
           COPY DRAT0151.
      *************
      *
      *************
      ***  UPAN0019 - FUNKTIONEN
           COPY DRAN0167.
      ***  FUNKTION - ERMITTELN DATEN ZU GES + VORSCHLAGSNUMMER (26)
           COPY DRAN0176.
      *************
      *
      *************
      ***  UPAN0020 - FUNKTIONEN
           COPY DRAN0206.
      ***  FUNKTION - ERMITTELN DATEN ZU GES + VORSCHLAGNUMMER (16)
           COPY DRAN0207.
      *************
      *
      *************
      ***  UPAN0023 - FUNKTIONEN
           COPY DRAN0383.
      ***  FUNKTION - ERMITTELN DATEN ZU GES + VORSCHLAGNUMMER (33)
           COPY DRAN0385.
      *************
       
      *************
      ***  UPPA0016 - FUNKTIONEN
           COPY DRPA0130.
      ***  FUNKTION - DATUMSPRÜFUNG
           COPY DRPA0131.
      ***  FUNKTION - ZEITRAUM ERMITTELN
           COPY DRPA0146.
      *************
      *

      *----------------------------------------------------------------
       PROCEDURE DIVISION.
      *----------------------------------------------------------------
      *    STEUERUNG:
      *
      *    UEBERGEORDNETE ROUTINEN :  -
      *
      *    UNTERGEORDNETE ROUTINEN : VORLAUF
      *                              VERARBEITUNG
      *                              NACHLAUF
      *----------------------------------------------------------------
       S-HAUPTSTEUERUNG SECTION.
      *
           PERFORM V0-VORLAUF
           PERFORM E0-LESEN
      *
           PERFORM WITH TEST BEFORE    UNTIL S-NZ  NOT = K-ZUSTAND-OK
      *
                 PERFORM SB-BEARBEITUNG
      *
           END-PERFORM
      *
           PERFORM N0-NACHLAUF
      *
           STOP RUN
           .
      *
       S-EXIT.
           EXIT.
      *
      *-----------------------------------------------------------------
       SB-BEARBEITUNG SECTION.
      *-----------------------------------------------------------------
           PERFORM B1-BEARBEITUNG
           PERFORM E1-LESEN
 
           .
       SB-EXIT.
           EXIT.
      *
      *-----------------------------------------------------------------
       B1-BEARBEITUNG SECTION.
      *----------------------------------------------------------------*
      *  INPUT : TDS-SATZ                                              *
      *  OUTPUT: SST-SATZ                                              *
      *  VERARBEITUNG: MODUS + GESELLSCHAFT                            *
      *                PRÜFEN UND GGF. VERARBEITEN                     *
      *                NUR ÖSTRR. PRODUKTGEBER WERDEN VERARBEITET
      *----------------------------------------------------------------*
      *
           INITIALIZE WIEN-SATZ-DATEN
 
           EVALUATE TDS-PGEB-GES
               WHEN K-KNR-PGES-ALLIANZ-SACH
               WHEN K-KNR-PGES-GENERALI-SACH
               WHEN K-KNR-PGES-ALLIANZ-LEBEN
               WHEN K-KNR-PGES-GENERALI-LEBEN
               WHEN K-KNR-PGES-CAPITAL-BANK
               WHEN K-KNR-PGES-DWS-OESTERREICH
               WHEN K-KNR-PGES-GENERALI-INVEST
               WHEN K-KNR-PGES-DIT-OEST
               WHEN K-KNR-PGES-BANK-AUSTRIA-INVEST
               WHEN K-KNR-PGES-SONST-GES-AT-HAFTPF
               WHEN K-KNR-PGES-BANK-AUSTRIA
               WHEN K-KNR-PGES-GENERALI-BANK
               WHEN K-KNR-PGES-INFINA
               WHEN K-KNR-PGES-S-BAUSPARKASSE
               WHEN K-KNR-PGES-BAWAG
               WHEN K-KNR-PGES-SONST-PROD-AUSTRIA
      *
                    EVALUATE TDS-MODUS
                        WHEN 'I '
                        WHEN 'I2'
                        WHEN 'UN'
                        WHEN 'U2'
                        WHEN 'D '
                        WHEN 'DU'
      *
                             PERFORM U02-AUSWERTUNG-BILDEN
      *
                             IF S-VERARBEITUNG-OK
                                PERFORM U05-SCHREIBEN-WIEN
                             ELSE
                                CONTINUE
                             END-IF
      *
                       WHEN OTHER
                             GO TO B1-EXIT
                    END-EVALUATE
      *
               WHEN OTHER
                    GO TO B1-EXIT
           END-EVALUATE
      *
           .
      *
       B1-EXIT.
           EXIT.
      *
      *-----------------------------------------------------------------
       E0-LESEN SECTION.
      *-----------------------------------------------------------------
      * INPUT : -                                                      -
      * OUTPUT: (NÄCHSTER) SATZ EINGABEDATEI / EINGABE-CURSOR,         -
      *         S-NZ (ZUSTAND)                                         -
      *-----------------------------------------------------------------
      *
           READ TDS-EINGABE
      *
           EVALUATE TRUE
           WHEN S-TDSFILE-EIN-OK
              MOVE K-ZUSTAND-OK                 TO S-NZ
              SET S-VERARBEITUNG-OK             TO TRUE
              ADD K-1                           TO Z-ZAEHLER-EINGABE
      *
           WHEN S-TDSFILE-EIN-EOF
              MOVE K-ZUSTAND-EOF                TO S-NZ
              SET S-VERARBEITUNG-NOK            TO TRUE
      *
           WHEN OTHER
              MOVE 'E0  '                       TO ERR-ORT-SEC
              MOVE 'DATEI-STATUS UNGUELTIG'     TO ERR-VAR-ZEILE01
              MOVE S-TDSFILE-EIN-STATUS         TO S-DATEI-STATUS
              MOVE S-DATEI-STATUS               TO ERR-VAR-ZEILE02
              MOVE K-1                          TO ERR-ORT-LFD
              PERFORM BR94-DATEI-FEHLER
              PERFORM U01-ABBRUCH
      *
           END-EVALUATE
      *
           .
      *
       E0-EXIT.
           EXIT.
      *
      *-----------------------------------------------------------------
       E1-LESEN SECTION.
      *-----------------------------------------------------------------
      * INPUT : -                                                      -
      * OUTPUT: (NÄCHSTER) SATZ EINGABEDATEI / EINGABE-CURSOR,         -
      *         S-NZ (ZUSTAND)                                         -
      *-----------------------------------------------------------------
      *
           READ TDS-EINGABE
      *
           EVALUATE TRUE
           WHEN S-TDSFILE-EIN-OK
              MOVE K-ZUSTAND-OK                 TO S-NZ
              SET S-VERARBEITUNG-OK             TO TRUE
              ADD K-1                           TO Z-ZAEHLER-EINGABE
      *
           WHEN S-TDSFILE-EIN-EOF
              MOVE K-ZUSTAND-EOF                TO S-NZ
              SET S-VERARBEITUNG-NOK            TO TRUE
      *
           WHEN OTHER
              MOVE 'E1  '                       TO ERR-ORT-SEC
              MOVE 'DATEI-STATUS UNGUELTIG'     TO ERR-VAR-ZEILE01
              MOVE S-TDSFILE-EIN-STATUS         TO S-DATEI-STATUS
              MOVE S-DATEI-STATUS               TO ERR-VAR-ZEILE02
              MOVE K-2                          TO ERR-ORT-LFD
              PERFORM BR94-DATEI-FEHLER
              PERFORM U01-ABBRUCH
      *
           END-EVALUATE
      *
           .
      *
       E1-EXIT.
           EXIT.
      *----------------------------------------------------------------
       V0-VORLAUF SECTION.
      *----------------------------------------------------------------
      *    - ÖFFENEN DER EIN- UND AUSGABEDATEIEN
      *    - ERMITTELN-TAGESDATUM
      *    - AUSGABE KOPFTEIL AUSWERTUNG
      *
      *    UEBERGEORDNETE ROUTINEN : STEUERUNG
      *
      *    UNTERGEORDNETE ROUTINEN :  -
      *----------------------------------------------------------------
      *    * VORBELEGUNGEN GRUPPENSTEUERFELDER
           MOVE K-ZUSTAND-OK             TO S-NZ
           SET S-VERARBEITUNG-OK         TO TRUE
 
      * VORBELEGUNG STANDARD-STATUS-BEREICH
      *    * (WIRD AUCH FÜR DAS RESTART-MODUL BENÖTIGT)
           INITIALIZE STANDARD-STATUS-BEREICH
      *
           MOVE K-PGM-NAME              TO STAT-KENNUNG
           SET STAT-BATCH               TO TRUE
           SET S-TAGESDATUM-NOK         TO TRUE
 
           PERFORM U08-TAGESDATUM
      *
           OPEN INPUT TDS-EINGABE
      *
           IF NOT S-TDSFILE-EIN-OK
              MOVE 'V01  '               TO ERR-ORT-SEC
              MOVE K-3                   TO ERR-ORT-LFD
              MOVE 'FEHLER BEIM ÖFFNEN TDS-EINGABE-DATEI'
                                         TO ERR-VAR-ZEILE01
              MOVE S-TDSFILE-EIN-STATUS  TO S-DATEI-STATUS
              PERFORM BR94-DATEI-FEHLER
              PERFORM U01-ABBRUCH
           END-IF

           OPEN OUTPUT WIEN-AUSGABE
      *
           IF NOT S-WIEN-AUS-OK
              MOVE 'V01  '               TO ERR-ORT-SEC
              MOVE K-4                   TO ERR-ORT-LFD
              MOVE 'FEHLER BEIM ÖFFNEN WIEN-AUSGABE-DATEI'
                                         TO ERR-VAR-ZEILE01
              MOVE S-WIEN-AUS-STATUS     TO S-DATEI-STATUS
              PERFORM BR94-DATEI-FEHLER
              PERFORM U01-ABBRUCH
           END-IF
      *    * ERMITTELN TAGESDATUM
           PERFORM U07-TAGESDATUM-TDS
 
           OPEN INPUT TDS-EINGABE
      *
           IF NOT S-TDSFILE-EIN-OK
              MOVE 'V01  '               TO ERR-ORT-SEC
              MOVE K-5                   TO ERR-ORT-LFD
              MOVE 'FEHLER BEIM ÖFFNEN TDS-EINGABE-DATEI'
                                         TO ERR-VAR-ZEILE01
              MOVE S-TDSFILE-EIN-STATUS  TO S-DATEI-STATUS
              PERFORM BR94-DATEI-FEHLER
              PERFORM U01-ABBRUCH
           END-IF
      *
           .
       V0-EXIT.
           EXIT.
      *
      *-----------------------------------------------------------------
       N0-NACHLAUF SECTION.
      *-----------------------------------------------------------------
           PERFORM N12-PROTOKOLL
           PERFORM N11-DATEIEN-SCHLIESSEN
      *
           .
       N0-EXIT.
           EXIT.
      *-----------------------------------------------------------------
       N11-DATEIEN-SCHLIESSEN SECTION.
      *-----------------------------------------------------------------
      * INPUT:                                                         -
      * OUT  :                                                         -
      * VERAR: SCHLIESSEN DATEIEN                                      -
      *-----------------------------------------------------------------
           CLOSE TDS-EINGABE.
 
           IF NOT S-TDSFILE-EIN-OK
              MOVE 'N11  '               TO ERR-ORT-SEC
              MOVE K-6                   TO ERR-ORT-LFD
              MOVE 'FEHLER BEIM SCHLIEßEN TDS-EINGABE-DATEI'
                                         TO ERR-VAR-ZEILE01
              MOVE S-TDSFILE-EIN-STATUS  TO S-DATEI-STATUS
              PERFORM BR94-DATEI-FEHLER
              PERFORM U01-ABBRUCH
           END-IF
      *
           CLOSE WIEN-AUSGABE
 
           IF NOT S-WIEN-AUS-OK
              MOVE 'N11  '               TO ERR-ORT-SEC
              MOVE K-7                   TO ERR-ORT-LFD
              MOVE 'FEHLER BEIM SCHLIEßEN WIEN-AUSGABE-DATEI'
                                         TO ERR-VAR-ZEILE01
              MOVE S-WIEN-AUS-STATUS     TO S-DATEI-STATUS
              PERFORM BR94-DATEI-FEHLER
              PERFORM U01-ABBRUCH
           END-IF
      *
           .
       N11-EXIT.
           EXIT.
      *
      *================================================================*
       N12-PROTOKOLL SECTION.
      *----------------------------------------------------------------*
      *    SCHREIBEN DES VERARBEITUNGPROTOKOLLS                        *
      *----------------------------------------------------------------*
      *                                                                *
      *    DIE ROUTINE 'PROTOKOLL' FASST DEN VERARBEITUNGSABLAUF IN    *
      *    SCHRIFTLICHER FORM ZUSAMMEN. DABEI WERDEN FOLGENDE ANGABEN  *
      *    PROTOKOLLIERT :                                             *
      *                                                                *
      *         - ANZAHL ALLER EINGELESENEN UND AUSGEGEBENEN SAETZE    *
      *         - VERARBEITUNGSENDE (DATUM UND UHRZEIT)                *
      *                                                                *
      *----------------------------------------------------------------*
      *
      **** ÜBERSCHRIFTZEILEN ABZIEHEN (3 STÜCK)
           COMPUTE Z-ZAEHLER-AUSGABE = Z-ZAEHLER-AUSGABE - K-3
 
           DISPLAY ' '
           DISPLAY ' '
           DISPLAY ' '
           DISPLAY ' PROTOKOLL: ERSTELLEN AUSWERTUNG ERFASSTE ANTRAEGE'
                   ' DVAG-WIEN (NUR OESTERR. GES.) AM ' Z-TAGESDATUM-EUR
 
           DISPLAY ' ===================================================
      -            '========================'
           DISPLAY ' '
           DISPLAY ' '
           DISPLAY 'ANZAHL EINGELESENE SAETZE :     '
                   Z-ZAEHLER-EINGABE
           DISPLAY ' '
           DISPLAY 'ANZAHL AUSGEGEBENE SAETZE :     '
                   Z-ZAEHLER-AUSGABE
           DISPLAY ' '
           DISPLAY ' '
           DISPLAY ' '
           DISPLAY ' ===================================================
      -            '========================'
           DISPLAY ' VERARBEITUNGSPROTOKOLL - ENDE VERARBEITUNG '
           DISPLAY ' '
           DISPLAY ' '
           DISPLAY ' '
      *
           .
       N12-EXIT.
           EXIT.
 
      *-----------------------------------------------------------------
       U01-ABBRUCH SECTION.
      *-----------------------------------------------------------------
      * AUFRUF DER STANDARD-ABBRUCHROUTINE 'ILBOABN0' FÜR DEN
      * PROGRAMMIERTEN VERARBEITUNGSABBRUCH
      * - ALLE NOCH OFFENEN DATEIEN WERDEN GESCHLOSSEN
      * - STANDARDMÄßIG WIRD KEIN DUMP ERZEUGT
      * - RC K-COMP-CODE (3501) WIRD AN DAS JOB-PROTOKOLL ÜBERGEBEN
      * *******
      * ACHTUNG: IN DER ROUTINE 'ILBOABN0' WIRD DAS PGM. BEENDET!
      * *******
      *-----------------------------------------------------------------
           PERFORM U09-ROLLBACK
           CALL 'ILBOABN0'  USING K-COMP-CODE
      *
           .
       U01-EXIT.
           EXIT.
 
      *-----------------------------------------------------------------
       U02-AUSWERTUNG-BILDEN SECTION.
      *-----------------------------------------------------------------
      * INPUT : TDS-SATZ,ANTRAGS- UND VORSCHLAGSDATEN
      * OUTPUT: AUSWERTUNG-SATZ
      * VERARB: AUSWERTUNG BILDEN
      *-----------------------------------------------------------------
      *
           EVALUATE TDS-PGEB-GES
               WHEN K-KNR-PGES-GENERALI-LEBEN
      *
                    EVALUATE TDS-ANTR-STATUS-1
                        WHEN 01
      *
      ****   GELÖSCHTE ANTRÄGE GES. 26 AUF STATUS 01 WERDEN NICHT MIT
      ****   'VO' GEKENNZEICHNET
      *
                             EVALUATE TDS-MODUS
                             WHEN 'D '
                             WHEN 'DU'
                                  MOVE TDS-MODUS
                                    TO WIEN-SATZ-DATEN-SATZART
      *
                             WHEN OTHER
      *
      ****     ANTRÄGE GES. 26 MIT STATUS 01 ERHALTEN SATZART 'VO'
      ****     (VORLÄUFIG ERFASST) AUßER BEI LÖSCHUNG
                                  MOVE 'VO' TO WIEN-SATZ-DATEN-SATZART
      *
                             END-EVALUATE
      *
                        WHEN 10
      *
                             EVALUATE TDS-MODUS
                                 WHEN 'I '
      *
                                      MOVE TDS-MODUS
                                        TO WIEN-SATZ-DATEN-SATZART
      *
      ****    EIN AUF 10 (ERFASST) GEÄNDERTER ANTRAG ERHÄLT SATZART 'I'
      *
                                 WHEN 'UN'
      *      
                                      EVALUATE TDS-ANTR-STATUS-2
                                      WHEN 01
      *                                    'VORLÄUFIG ERFASST->'ERFASST'
                                           MOVE 'I '
                                             TO WIEN-SATZ-DATEN-SATZART
      *
                                      WHEN OTHER
      *
                                           MOVE TDS-MODUS
                                             TO WIEN-SATZ-DATEN-SATZART
      *
                                      END-EVALUATE
      *
                                 WHEN OTHER
      *
                                       MOVE TDS-MODUS
                                         TO WIEN-SATZ-DATEN-SATZART
      *
                             END-EVALUATE
      *
                        WHEN OTHER
      *
                             MOVE TDS-MODUS TO WIEN-SATZ-DATEN-SATZART
      *
                    END-EVALUATE
      *
               WHEN OTHER
      *
                    MOVE TDS-MODUS TO WIEN-SATZ-DATEN-SATZART
      *
           END-EVALUATE
      *
      ***** AVERM
      *
           MOVE TDS-AVERM    TO WIEN-SATZ-DATEN-VBNR
 
      ***** VM WENN VORHANDEN
           MOVE TDS-VM       TO WIEN-SATZ-DATEN-VMNR
      *
      ***** GESELLSCHAFT
      *
           MOVE TDS-PGEB-GES TO WIEN-SATZ-DATEN-PGEB-GES
      *
      ***** TARIF (NUR GES. 26 U. 95)
           EVALUATE TDS-PGEB-GES
           WHEN K-KNR-PGES-GENERALI-LEBEN
           WHEN K-KNR-PGES-SONST-PROD-AUSTRIA
      *
                    MOVE TDS-TARIF    TO WIEN-SATZ-DATEN-TARIF
           WHEN OTHER
      *
                    MOVE SPACE        TO WIEN-SATZ-DATEN-TARIF
           END-EVALUATE
      *
      ***** PRODUKTE
           MOVE TDS-PROD-PROD-SCHL-1   TO Z-PROD-SCHL-FORMATIERT
           MOVE Z-PROD-SCHL-FORMATIERT TO WIEN-SATZ-DATEN-PROD-SCHL-1
      *
           MOVE TDS-PROD-PROD-SCHL-2   TO Z-PROD-SCHL-FORMATIERT
           MOVE Z-PROD-SCHL-FORMATIERT TO WIEN-SATZ-DATEN-PROD-SCHL-2
      *
           MOVE TDS-PROD-PROD-SCHL-3   TO Z-PROD-SCHL-FORMATIERT
           MOVE Z-PROD-SCHL-FORMATIERT TO WIEN-SATZ-DATEN-PROD-SCHL-3
      *
           MOVE TDS-PROD-PROD-SCHL-4   TO Z-PROD-SCHL-FORMATIERT
           MOVE Z-PROD-SCHL-FORMATIERT TO WIEN-SATZ-DATEN-PROD-SCHL-4
      *
           MOVE TDS-PROD-PROD-SCHL-5   TO Z-PROD-SCHL-FORMATIERT
           MOVE Z-PROD-SCHL-FORMATIERT TO WIEN-SATZ-DATEN-PROD-SCHL-5
      *
           MOVE TDS-PROD-PROD-SCHL-6   TO Z-PROD-SCHL-FORMATIERT
           MOVE Z-PROD-SCHL-FORMATIERT TO WIEN-SATZ-DATEN-PROD-SCHL-6
      *
           MOVE TDS-PROD-PROD-SCHL-7   TO Z-PROD-SCHL-FORMATIERT
           MOVE Z-PROD-SCHL-FORMATIERT TO WIEN-SATZ-DATEN-PROD-SCHL-7
      *
           MOVE TDS-PROD-PROD-SCHL-8   TO Z-PROD-SCHL-FORMATIERT
           MOVE Z-PROD-SCHL-FORMATIERT TO WIEN-SATZ-DATEN-PROD-SCHL-8
      *
           MOVE TDS-PROD-PROD-SCHL-9   TO Z-PROD-SCHL-FORMATIERT
           MOVE Z-PROD-SCHL-FORMATIERT TO WIEN-SATZ-DATEN-PROD-SCHL-9
      *
      **** KUNDE VOR- U. ZUNAME
      *
      **** KEINE LEERES FELD VORNAME ZULASSEN
           IF TDS-KUNDE-VNAME = SPACE
              MOVE 'X'                TO WIEN-SATZ-DATEN-KD-VORNAME
           ELSE
              MOVE FUNCTION UPPER-CASE(TDS-KUNDE-VNAME)
                TO WIEN-SATZ-DATEN-KD-VORNAME
              MOVE WIEN-SATZ-DATEN-KD-VORNAME
                TO Z-UMLAUTE-GROSS
              PERFORM U12-UMLAUTE-GROSS
              MOVE Z-UMLAUTE-GROSS
                TO WIEN-SATZ-DATEN-KD-VORNAME
           END-IF
      
           MOVE FUNCTION UPPER-CASE(TDS-KUNDE-NAME)
             TO WIEN-SATZ-DATEN-KD-ZUNAME
           MOVE WIEN-SATZ-DATEN-KD-ZUNAME
             TO Z-UMLAUTE-GROSS
           PERFORM U12-UMLAUTE-GROSS
           MOVE Z-UMLAUTE-GROSS
             TO WIEN-SATZ-DATEN-KD-ZUNAME
      
      **** KUNDE ADRESSE
      
           MOVE TDS-AUSL              TO WIEN-SATZ-DATEN-KD-AUSL-KZ
      *
      **** AUSLANDKENNZEICHEN ÖSTERREICH (A) KANN BEI ALTANTRÄGEN
      *    AUCH IN DER PLZ AUFTAUCHEN.
      *
           IF TDS-PLZ(1:1) = K-A
              MOVE TDS-PLZ(2:8)       TO WIEN-SATZ-DATEN-KD-PLZ
           ELSE
              MOVE TDS-PLZ            TO WIEN-SATZ-DATEN-KD-PLZ
           END-IF
      *
      **** ORT
      *
           MOVE FUNCTION UPPER-CASE(TDS-ORT)
             TO WIEN-SATZ-DATEN-KD-ORT

           MOVE WIEN-SATZ-DATEN-KD-ORT
             TO Z-UMLAUTE-GROSS
           PERFORM U12-UMLAUTE-GROSS
           MOVE Z-UMLAUTE-GROSS
             TO WIEN-SATZ-DATEN-KD-ORT

      *
      **** BEITRAG
      **** FÜR GES. 16,39,95 SPEZIELLE BEHANDLUNG
      *
           EVALUATE TDS-PGEB-GES
               WHEN K-KNR-PGES-GENERALI-SACH
      *
      ***  FÜR GES. 16 WERDEN DIE BEITRÄGE ALLER PRODUKTE ADDIERT
      ***  DIE BEITRÄGE SIND UM DEN FAKTOR 100 ZU HOCH DA SIE IN
      ***  FELDERN OHNE NACHKOMMASTELLEN (TDS-SUMME-TAB) GESPEICHERT
      ***  SIND
                   MOVE ZERO TO Z-BEITRAG-NUM
      *
                   PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1
                     UNTIL Z-ZAEHLER > K-9
      *
                       COMPUTE Z-BEITRAG-NUM =
                               TDS-SUMME-TAB(Z-ZAEHLER)+ Z-BEITRAG-NUM
      *
                   END-PERFORM
      *
      *** ERGEBNIS IN NACHKOMMAZAHL UMWANDELN
      *
                   COMPUTE Z-BEITRAG-NUM = Z-BEITRAG-NUM / K-100
      *
                   MOVE Z-BEITRAG-NUM
                     TO Z-BEITRAG-FORMATIERT
      *
                   MOVE Z-BEITRAG-FORMATIERT
                     TO WIEN-SATZ-DATEN-BEITRAG-SUM
      *
                   MOVE SPACE TO WIEN-SATZ-DATEN-AN-BEITRAG
      *
               WHEN K-KNR-PGES-GENERALI-INVEST
      *
                   MOVE ZERO TO Z-BEITRAG-NUM
                   MOVE ZERO TO Z-BEITRAG-NUM-2
      *
                   PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1
                     UNTIL Z-ZAEHLER > K-9
      *
      **** SUMMIEREN EINMALANLAGEN DES ANTRAGES
                       IF TDS-ZAHLWEISE-TAB-X(Z-ZAEHLER) = K-8
      *
                          COMPUTE Z-BEITRAG-NUM = Z-BEITRAG-NUM +
                                  TDS-SUMME-TAB(Z-ZAEHLER)
      *
                       ELSE
      *
      **** SUMMIEREN SPARPLÄNE DES ANTRAGES 
                          EVALUATE TDS-ZAHLWEISE-TAB-X(Z-ZAEHLER)
                          WHEN K-6
      *
                               COMPUTE Z-BEITRAG-NUM-2 =
                                       Z-BEITRAG-NUM-2 +
                                       TDS-SUMME-TAB(Z-ZAEHLER)
      *
                          WHEN K-1
      
                               COMPUTE Z-BEITRAG-NUM-2 =
                                       Z-BEITRAG-NUM-2 +
                                       (K-12 * TDS-SUMME-TAB(Z-ZAEHLER))
      *
                          WHEN K-2
      
                               COMPUTE Z-BEITRAG-NUM-2 =
                                       Z-BEITRAG-NUM-2 +
                                       (K-6 * TDS-SUMME-TAB(Z-ZAEHLER))
      *
                           WHEN K-4
      
                               COMPUTE Z-BEITRAG-NUM-2 =
                                       Z-BEITRAG-NUM-2 +
                                       (K-4 * TDS-SUMME-TAB(Z-ZAEHLER))
      *
                          END-EVALUATE
      *
                       END-IF
      *
                   END-PERFORM
      *
      **** EINMALANLAGEN DES ANTRAGES
      *
                   IF Z-BEITRAG-NUM = ZERO
      *
                      MOVE SPACE TO WIEN-SATZ-DATEN-EINMERLAEGE
      
                   ELSE
      *
                      MOVE Z-BEITRAG-NUM
                        TO Z-BEITRAG-FORMATIERT
                      MOVE Z-BEITRAG-FORMATIERT
                        TO WIEN-SATZ-DATEN-EINMERLAEGE
      *
                   END-IF
      *
      **** SPRAPLÄNE DES ANTRAGES
                   IF Z-BEITRAG-NUM-2 = ZERO
      *
                      MOVE SPACE
                        TO WIEN-SATZ-DATEN-AN-BEITRAG
                      MOVE SPACE
                        TO WIEN-SATZ-DATEN-BEITRAG-SUM
      *
                   ELSE
      *
                      MOVE Z-BEITRAG-NUM-2
                        TO Z-BEITRAG-FORMATIERT
                      MOVE Z-BEITRAG-FORMATIERT
                        TO WIEN-SATZ-DATEN-BEITRAG-SUM
      *
                   END-IF
      *
               WHEN K-KNR-PGES-SONST-PROD-AUSTRIA
      *
      *** BEI GES. 95 ENTHALTEN NUR DIE PRODUKTE >= 98 EINEN BEITRAG
                    IF TDS-PROD-PROD-SCHL-1 >= K-98
      *
                       MOVE TDS-BEITRAG          TO Z-BEITRAG-NUM
                       MOVE Z-BEITRAG-NUM        TO Z-BEITRAG-FORMATIERT
                       MOVE Z-BEITRAG-FORMATIERT
                         TO WIEN-SATZ-DATEN-AN-BEITRAG
      *
                    ELSE
      *
                       MOVE SPACE TO WIEN-SATZ-DATEN-AN-BEITRAG
      *
                    END-IF
      *
               WHEN OTHER
      
                   IF TDS-BEITRAG = ZERO
                      MOVE SPACE TO WIEN-SATZ-DATEN-AN-BEITRAG
      *
                   ELSE
                      MOVE TDS-BEITRAG           TO Z-BEITRAG-NUM
                      MOVE Z-BEITRAG-NUM         TO Z-BEITRAG-FORMATIERT
                      MOVE Z-BEITRAG-FORMATIERT
                        TO WIEN-SATZ-DATEN-AN-BEITRAG
      *
                   END-IF
      *
           END-EVALUATE
      *
      ***   SUMME (NUR GES. 65,69,72,75,85,95)
      *** FÜR GES. 95 STEHT DIE SUMME IM FELD TDS-BEITRAG
           EVALUATE TDS-PGEB-GES
      *** BEI GES. 95 ERHALTEN NUR DIE PRODUKTE < 98 EINE SUMME
               WHEN K-KNR-PGES-SONST-PROD-AUSTRIA
      *
                    IF TDS-PROD-PROD-SCHL-1 < K-98
      *
                       MOVE TDS-BEITRAG
                         TO Z-BEITRAG-NUM
                       MOVE Z-BEITRAG-NUM
                         TO Z-BEITRAG-FORMATIERT
                       MOVE Z-BEITRAG-FORMATIERT
                         TO WIEN-SATZ-DATEN-SUMME
      *
                    ELSE
      *
                       MOVE SPACE TO WIEN-SATZ-DATEN-SUMME
      *
                    END-IF
      *
               WHEN K-KNR-PGES-BANK-AUSTRIA
               WHEN K-KNR-PGES-GENERALI-BANK
               WHEN K-KNR-PGES-INFINA
               WHEN K-KNR-PGES-S-BAUSPARKASSE
               WHEN K-KNR-PGES-BAWAG
      *
                    MOVE TDS-SUMME-1           TO Z-BEITRAG-NUM
                    MOVE Z-BEITRAG-NUM         TO Z-BEITRAG-FORMATIERT
                    MOVE Z-BEITRAG-FORMATIERT
                      TO WIEN-SATZ-DATEN-SUMME
      *
               WHEN OTHER
      *
                    MOVE SPACE TO WIEN-SATZ-DATEN-SUMME
      *
           END-EVALUATE
      *
      ***    ZAHLWEISE (GES. 39 IMMER MONATLICH = 6 EINSTELLEN)
           EVALUATE TDS-PGEB-GES
              WHEN K-KNR-PGES-GENERALI-INVEST
      
      ***    SIND NUR EINMALANLAGEN VORHANDEN FELD ZAHLWEISE LEER
                    IF WIEN-SATZ-DATEN-BEITRAG-SUM = SPACE
                       MOVE SPACE TO WIEN-SATZ-DATEN-AN-ZAHLW
                    ELSE
                       MOVE K-6 TO WIEN-SATZ-DATEN-AN-ZAHLW
                    END-IF
      *
              WHEN OTHER
      *
                   IF TDS-ZAHLWEISE = ZERO
                      MOVE SPACE TO WIEN-SATZ-DATEN-AN-ZAHLW
                   ELSE
                      MOVE TDS-ZAHLWEISE TO WIEN-SATZ-DATEN-AN-ZAHLW
                   END-IF
      *
           END-EVALUATE
      *
      ***   VERTRAGSBEGINN UND VERTRAGSABLAUF  (NUR GES. 16, 26, 95)
      *
           EVALUATE TDS-PGEB-GES
              WHEN K-KNR-PGES-GENERALI-SACH
              WHEN K-KNR-PGES-GENERALI-LEBEN
      *
                   MOVE TDS-VERS-BEGINN
                     TO Z-TAGESDATUM-TDS-FORMAT
      
                   MOVE Z-TAGESDATUM-TDS-FORMAT(6:2)
                     TO WIEN-SATZ-DATEN-B-TAG
                        PI-DRPA0131-DATUM-6(1:2)
      
                   MOVE Z-TAGESDATUM-TDS-FORMAT(4:2)
                     TO WIEN-SATZ-DATEN-B-MONAT
                        PI-DRPA0131-DATUM-6(3:2)
      
                   MOVE Z-TAGESDATUM-TDS-FORMAT(2:2)
                     TO WIEN-SATZ-DATEN-B-JAHR
                        PI-DRPA0131-DATUM-6(5:2)
      
                   PERFORM U10-DATUM-PRUEF
      
                   MOVE Z-TAGESDATUM-TDS-LANG-JH
                     TO WIEN-SATZ-DATEN-B-JAHRH
                   
                   MOVE TDS-VERS-ENDE
                     TO Z-TAGESDATUM-TDS-FORMAT
                   
                   MOVE Z-TAGESDATUM-TDS-FORMAT(6:2)
                     TO WIEN-SATZ-DATEN-A-TAG
                        PI-DRPA0131-DATUM-6(1:2)
                   
                   MOVE Z-TAGESDATUM-TDS-FORMAT(4:2)
                     TO WIEN-SATZ-DATEN-A-MONAT
                        PI-DRPA0131-DATUM-6(3:2)
                   
                   MOVE Z-TAGESDATUM-TDS-FORMAT(2:2)
                     TO WIEN-SATZ-DATEN-A-JAHR
                        PI-DRPA0131-DATUM-6(5:2)
      *
                   PERFORM U10-DATUM-PRUEF
      *
                   MOVE Z-TAGESDATUM-TDS-LANG-JH
                     TO WIEN-SATZ-DATEN-A-JAHRH
      *
              WHEN K-KNR-PGES-SONST-PROD-AUSTRIA
      *
      *** BEI GES. 95 BEGINN UND ABLAUF NICHT IMMER VORHANDEN
                   IF TDS-VERS-BEGINN > ZERO
      *
                      MOVE TDS-VERS-BEGINN
                        TO Z-TAGESDATUM-TDS-FORMAT
                      MOVE Z-TAGESDATUM-TDS-FORMAT(6:2)
                        TO WIEN-SATZ-DATEN-B-TAG
                           PI-DRPA0131-DATUM-6(1:2)
                      MOVE Z-TAGESDATUM-TDS-FORMAT(4:2)
                        TO WIEN-SATZ-DATEN-B-MONAT
                           PI-DRPA0131-DATUM-6(3:2)
                      MOVE Z-TAGESDATUM-TDS-FORMAT(2:2)
                        TO WIEN-SATZ-DATEN-B-JAHR
                           PI-DRPA0131-DATUM-6(5:2)
      
                      PERFORM U10-DATUM-PRUEF
      
                      MOVE Z-TAGESDATUM-TDS-LANG-JH
                        TO WIEN-SATZ-DATEN-B-JAHRH
      *
                      MOVE TDS-VERS-ENDE
                        TO Z-TAGESDATUM-TDS-FORMAT
                      MOVE Z-TAGESDATUM-TDS-FORMAT(6:2)
                        TO WIEN-SATZ-DATEN-A-TAG
                           PI-DRPA0131-DATUM-6(1:2)
                      MOVE Z-TAGESDATUM-TDS-FORMAT(4:2)
                        TO WIEN-SATZ-DATEN-A-MONAT
                           PI-DRPA0131-DATUM-6(3:2)
                      MOVE Z-TAGESDATUM-TDS-FORMAT(2:2)
                        TO WIEN-SATZ-DATEN-A-JAHR
                           PI-DRPA0131-DATUM-6(5:2)
      *
                      PERFORM U10-DATUM-PRUEF
      
                      MOVE Z-TAGESDATUM-TDS-LANG-JH
                        TO WIEN-SATZ-DATEN-A-JAHRH
      *
                   ELSE
      *
                      MOVE SPACE
                        TO WIEN-SATZ-DATEN-AN-BEGINN
                           WIEN-SATZ-DATEN-ABLAUF
      *
                   END-IF
      *
              WHEN OTHER
      *
                   MOVE SPACE
                     TO WIEN-SATZ-DATEN-AN-BEGINN
                        WIEN-SATZ-DATEN-ABLAUF
      *
           END-EVALUATE
      *
      **** EINHEITEN
      *
           MOVE ZERO TO Z-BEITRAG-NUM
      *
           PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1 UNTIL
                           Z-ZAEHLER > K-9
      
                    COMPUTE Z-BEITRAG-NUM =
                            Z-BEITRAG-NUM +
                            TDS-EINH-AVERM-TAB(Z-ZAEHLER)
      *
           END-PERFORM
      *
           MOVE Z-BEITRAG-NUM         TO Z-BEITRAG-FORMATIERT
           MOVE Z-BEITRAG-FORMATIERT
             TO WIEN-SATZ-DATEN-EINHEITEN
      *
      *
      **** ANTRAGSART
      *
           MOVE TDS-ANTRAGS-ART      TO WIEN-SATZ-DATEN-ANTRAGS-ART
      *
      *
      **** ANTRAGSDATUM (ANTRAGS-AUFNAHME-DATUM)
      *
           MOVE TDS-AN-AUFN-DAT              TO Z-TAGESDATUM-TDS-FORMAT
           MOVE Z-TAGESDATUM-TDS-FORMAT(6:2) TO WIEN-SATZ-DATEN-AD-TAG
                                                PI-DRPA0131-DATUM-6(1:2)
           MOVE Z-TAGESDATUM-TDS-FORMAT(4:2) TO WIEN-SATZ-DATEN-AD-MONAT
                                                PI-DRPA0131-DATUM-6(3:2)
           MOVE Z-TAGESDATUM-TDS-FORMAT(2:2) TO WIEN-SATZ-DATEN-AD-JAHR
                                                PI-DRPA0131-DATUM-6(5:2)
      *
           PERFORM U10-DATUM-PRUEF
      *
           MOVE Z-TAGESDATUM-TDS-LANG-JH
             TO WIEN-SATZ-DATEN-AD-JAHRH
      *
      **** STRUKTURDATUM
      *
           MOVE TDS-STRUK-DAT                TO Z-TAGESDATUM-TDS-FORMAT
           MOVE Z-TAGESDATUM-TDS-FORMAT(6:2) TO WIEN-SATZ-DATEN-SD-TAG
                                                PI-DRPA0131-DATUM-6(1:2)
           MOVE Z-TAGESDATUM-TDS-FORMAT(4:2) TO WIEN-SATZ-DATEN-SD-MONAT
                                                PI-DRPA0131-DATUM-6(3:2)
           MOVE Z-TAGESDATUM-TDS-FORMAT(2:2) TO WIEN-SATZ-DATEN-SD-JAHR
                                                PI-DRPA0131-DATUM-6(5:2)
      *
           PERFORM U10-DATUM-PRUEF
      *
           MOVE Z-TAGESDATUM-TDS-LANG-JH
             TO WIEN-SATZ-DATEN-SD-JAHRH
      *
      **** ERFASSUNGSDATUM
      *
           MOVE TDS-DATUM                    TO Z-TAGESDATUM-TDS-FORMAT
           MOVE Z-TAGESDATUM-TDS-FORMAT(6:2) TO WIEN-SATZ-DATEN-ED-TAG
                                                PI-DRPA0131-DATUM-6(1:2)
           MOVE Z-TAGESDATUM-TDS-FORMAT(4:2) TO WIEN-SATZ-DATEN-ED-MONAT
                                                PI-DRPA0131-DATUM-6(3:2)
           MOVE Z-TAGESDATUM-TDS-FORMAT(2:2) TO WIEN-SATZ-DATEN-ED-JAHR
                                                PI-DRPA0131-DATUM-6(5:2)
      *
           PERFORM U10-DATUM-PRUEF
      *
           MOVE Z-TAGESDATUM-TDS-LANG-JH
             TO WIEN-SATZ-DATEN-ED-JAHRH
      *
      **** STATISTIKDATUM
      *
           MOVE TDS-STATISTIK-DAT            TO Z-TAGESDATUM-TDS-FORMAT
           MOVE Z-TAGESDATUM-TDS-FORMAT(6:2) TO WIEN-SATZ-DATEN-ST-TAG
                                                PI-DRPA0131-DATUM-6(1:2)
           MOVE Z-TAGESDATUM-TDS-FORMAT(4:2) TO WIEN-SATZ-DATEN-ST-MONAT
                                                PI-DRPA0131-DATUM-6(3:2)
           MOVE Z-TAGESDATUM-TDS-FORMAT(2:2) TO WIEN-SATZ-DATEN-ST-JAHR
                                                PI-DRPA0131-DATUM-6(5:2)
      *
           PERFORM U10-DATUM-PRUEF
      *
           MOVE Z-TAGESDATUM-TDS-LANG-JH
             TO WIEN-SATZ-DATEN-ST-JAHRH
      *
      **** BEARBEITER-ID
      *
           MOVE TDS-BEARBEITER-ID        TO WIEN-SATZ-DATEN-ERFASSER-ID
      *
      **** ERFASSUNGSZEITSTEMPEL
      *
      **** DATUM
      *
           MOVE TDS-DATUM                    TO Z-TAGESDATUM-TDS-FORMAT
           MOVE Z-TAGESDATUM-TDS-FORMAT(6:2) TO WIEN-SATZ-DATEN-TAG
                                                PI-DRPA0131-DATUM-6(1:2)
           MOVE Z-TAGESDATUM-TDS-FORMAT(4:2) TO WIEN-SATZ-DATEN-MONAT
                                                PI-DRPA0131-DATUM-6(3:2)
           MOVE Z-TAGESDATUM-TDS-FORMAT(2:2) TO WIEN-SATZ-DATEN-JAHR
                                                PI-DRPA0131-DATUM-6(5:2)
      *
           PERFORM U10-DATUM-PRUEF
      *
           MOVE Z-TAGESDATUM-TDS-LANG-JH
             TO WIEN-SATZ-DATEN-JAHRH
      *
      **** UHRZEIT
      *
           MOVE TDS-UHR                      TO Z-UHRZEIT-TDS-FORMAT
           MOVE Z-UHRZEIT-TDS-FORMAT(8:2)    TO WIEN-SATZ-DATEN-SEKUNDE
           MOVE Z-UHRZEIT-TDS-FORMAT(6:2)    TO WIEN-SATZ-DATEN-MINUTE
           MOVE Z-UHRZEIT-TDS-FORMAT(4:2)    TO WIEN-SATZ-DATEN-STUNDE
      
      **** PRÜFEN UHRZEIT.
      
           IF WIEN-SATZ-DATEN-STUNDE > K-23
              MOVE K-23 TO WIEN-SATZ-DATEN-STUNDE
           END-IF
      
           IF WIEN-SATZ-DATEN-MINUTE > K-59
              MOVE K-59 TO WIEN-SATZ-DATEN-MINUTE
           END-IF
      
           IF WIEN-SATZ-DATEN-SEKUNDE > K-59
              MOVE K-59 TO WIEN-SATZ-DATEN-SEKUNDE
           END-IF
      *
      **** WEITERE INFORMATIONEN AUS ANTRAGSTABELLEN HOLEN.
      **** BEI DU UND D (GELÖSCHTEN SÄTZEN) NICHT MÖGLICH ENTSPR. FELDER
      **** WERDEN LEER EINGESTELLT
      *
           EVALUATE TDS-MODUS
           WHEN 'D '
           WHEN 'DU'
      
                MOVE TDS-NR TO WIEN-SATZ-DATEN-PIN
                               WIEN-SATZ-DATEN-VNR-ANTR
      
                MOVE ZERO   TO WIEN-SATZ-DATEN-GEWERBE-VBNR
                               WIEN-SATZ-DATEN-GEN-ANT-NR
                               WIEN-SATZ-DATEN-GEN-B-ANT-NR
                               WIEN-SATZ-DATEN-AKT-ANTR-STA
      
                MOVE SPACE  TO WIEN-SATZ-DATEN-VORSCHL-NR
                               WIEN-SATZ-DATEN-KD-NUMMER
                               WIEN-SATZ-DATEN-KD-NUMMER-2
                               WIEN-SATZ-DATEN-KD-GEB-DAT
                               WIEN-SATZ-DATEN-KD-SOZVNR
                               WIEN-SATZ-DATEN-KD-STRASSE
                               WIEN-SATZ-DATEN-AN-BEARB-GEB
                               WIEN-SATZ-DATEN-AN-JNP
                               WIEN-SATZ-DATEN-PRAEM-SUM
                               WIEN-SATZ-DATEN-STAATS-ANG
                               WIEN-SATZ-DATEN-PEP-STATUS
                               WIEN-SATZ-DATEN-BRANCHE
                               WIEN-SATZ-DATEN-BERUFSGRP
                               WIEN-SATZ-DATEN-FILLER
      *
                PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1
                  UNTIL Z-ZAEHLER > K-4
      
                        MOVE SPACE
                          TO WIEN-S-DATEN-W-INH-KD-VORNAME(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-ZUNAME (Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-GEB-DAT(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-AUSL-KZ(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-PLZ    (Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-ORT    (Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-STAATS-ANG(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-PEP-STATUS(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-BRANCHE   (Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-BERUFSGRP (Z-ZAEHLER)
      
                END-PERFORM
                
                PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1
                  UNTIL Z-ZAEHLER > K-5
      
                        MOVE SPACE
                          TO WIEN-S-DATEN-W-INH-DIL(Z-ZAEHLER)
                END-PERFORM

           WHEN OTHER
      *
                PERFORM U03-DATEN-AUS-ANTRAGTABELLEN
      *
           END-EVALUATE
      *
      **** FÜR GES. 16,26,33 WEITERE INFORMATIONEN AUS VORSCHLAGSDATEN
      **** HOLEN WENN VORSCHLAG VORHANDEN
      *
           EVALUATE TDS-PGEB-GES
              WHEN K-KNR-PGES-GENERALI-SACH
      *
                   IF WIEN-SATZ-DATEN-VORSCHL-NR NOT = SPACE
      *
                      PERFORM U041-DATEN-AUS-VORSCHLAG-16
      *
                   ELSE
      *
                      CONTINUE
      *
                   END-IF
      *
              WHEN K-KNR-PGES-GENERALI-LEBEN
      *
                   IF WIEN-SATZ-DATEN-VORSCHL-NR NOT = SPACE
      *
                      PERFORM U042-DATEN-AUS-VORSCHLAG-26
      *
                   ELSE
      *
                      CONTINUE
      *
                   END-IF
      *
              WHEN K-KNR-PGES-CAPITAL-BANK
      *
                   IF WIEN-SATZ-DATEN-VORSCHL-NR NOT = SPACE
      *
                      PERFORM U043-DATEN-AUS-VORSCHLAG-33
      *
                   ELSE
      *
                      CONTINUE
      *
                   END-IF
                   
              WHEN OTHER
      *
                   CONTINUE
      *
           END-EVALUATE
      *
      *
      **** SEMIKOLON
      *
           MOVE ';'                   TO WIEN-SATZ-DATEN-SEMIK-1
                                         WIEN-SATZ-DATEN-SEMIK-2
                                         WIEN-SATZ-DATEN-SEMIK-3
                                         WIEN-SATZ-DATEN-SEMIK-4
                                         WIEN-SATZ-DATEN-SEMIK-5
                                         WIEN-SATZ-DATEN-SEMIK-6
                                         WIEN-SATZ-DATEN-SEMIK-7
                                         WIEN-SATZ-DATEN-SEMIK-8
                                         WIEN-SATZ-DATEN-SEMIK-9
                                         WIEN-SATZ-DATEN-SEMIK-10
                                         WIEN-SATZ-DATEN-SEMIK-11
                                         WIEN-SATZ-DATEN-SEMIK-12
                                         WIEN-SATZ-DATEN-SEMIK-13
                                         WIEN-SATZ-DATEN-SEMIK-14
                                         WIEN-SATZ-DATEN-SEMIK-15
                                         WIEN-SATZ-DATEN-SEMIK-16
                                         WIEN-SATZ-DATEN-SEMIK-17
                                         WIEN-SATZ-DATEN-SEMIK-18
                                         WIEN-SATZ-DATEN-SEMIK-19
                                         WIEN-SATZ-DATEN-SEMIK-20
                                         WIEN-SATZ-DATEN-SEMIK-21
                                         WIEN-SATZ-DATEN-SEMIK-22
                                         WIEN-SATZ-DATEN-SEMIK-23
                                         WIEN-SATZ-DATEN-SEMIK-24
                                         WIEN-SATZ-DATEN-SEMIK-25
                                         WIEN-SATZ-DATEN-SEMIK-26
                                         WIEN-SATZ-DATEN-SEMIK-27
                                         WIEN-SATZ-DATEN-SEMIK-28
                                         WIEN-SATZ-DATEN-SEMIK-29
                                         WIEN-SATZ-DATEN-SEMIK-30
                                         WIEN-SATZ-DATEN-SEMIK-31
                                         WIEN-SATZ-DATEN-SEMIK-32
                                         WIEN-SATZ-DATEN-SEMIK-33
                                         WIEN-SATZ-DATEN-SEMIK-34
                                         WIEN-SATZ-DATEN-SEMIK-35
                                         WIEN-SATZ-DATEN-SEMIK-36
                                         WIEN-SATZ-DATEN-SEMIK-37
                                         WIEN-SATZ-DATEN-SEMIK-38
                                         WIEN-SATZ-DATEN-SEMIK-39
                                         WIEN-SATZ-DATEN-SEMIK-40
                                         WIEN-SATZ-DATEN-SEMIK-41
                                         WIEN-SATZ-DATEN-SEMIK-42
                                         WIEN-SATZ-DATEN-SEMIK-43
                                         WIEN-SATZ-DATEN-SEMIK-44
                                         WIEN-SATZ-DATEN-SEMIK-45
                                         WIEN-SATZ-DATEN-SEMIK-46
                                         WIEN-SATZ-DATEN-SEMIK-47
                                         WIEN-SATZ-DATEN-SEMIK-48
                                         WIEN-SATZ-DATEN-SEMIK-49
                                         WIEN-SATZ-DATEN-SEMIK-50
                                         WIEN-SATZ-DATEN-SEMIK-51
                                         WIEN-SATZ-DATEN-SEMIK-52
                                         WIEN-SATZ-DATEN-SEMIK-93
                                         WIEN-SATZ-DATEN-SEMIK-94

           PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1
             UNTIL Z-ZAEHLER > K-4
      
                    MOVE ';'
                     TO WIEN-SATZ-DATEN-SEMIK1-INH(Z-ZAEHLER)
                        WIEN-SATZ-DATEN-SEMIK2-INH(Z-ZAEHLER)
                        WIEN-SATZ-DATEN-SEMIK3-INH(Z-ZAEHLER)
                        WIEN-SATZ-DATEN-SEMIK4-INH(Z-ZAEHLER)
                        WIEN-SATZ-DATEN-SEMIK5-INH(Z-ZAEHLER)
                        WIEN-SATZ-DATEN-SEMIK6-INH(Z-ZAEHLER)
                        WIEN-SATZ-DATEN-SEMIK7-INH(Z-ZAEHLER)
                        WIEN-SATZ-DATEN-SEMIK8-INH(Z-ZAEHLER)
                        WIEN-SATZ-DATEN-SEMIK9-INH(Z-ZAEHLER)
                        WIEN-SATZ-DATEN-SEMIK10-INH(Z-ZAEHLER)
      
           END-PERFORM

           PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1
             UNTIL Z-ZAEHLER > K-5
                   MOVE ';'
                     TO WIEN-SATZ-DATEN-SEMIK1-INH-DIL(Z-ZAEHLER)
           END-PERFORM
      *
      **** PUNKT
      *
           MOVE '.'                  TO  WIEN-SATZ-DATEN-P1
                                         WIEN-SATZ-DATEN-P2
                                         WIEN-SATZ-DATEN-AD-P1
                                         WIEN-SATZ-DATEN-AD-P2
                                         WIEN-SATZ-DATEN-ED-P1
                                         WIEN-SATZ-DATEN-ED-P2
                                         WIEN-SATZ-DATEN-ST-P1
                                         WIEN-SATZ-DATEN-ST-P2
                                         WIEN-SATZ-DATEN-SD-P1
                                         WIEN-SATZ-DATEN-SD-P2
                                         WIEN-SATZ-DATEN-A-P1
                                         WIEN-SATZ-DATEN-A-P2
                                         WIEN-SATZ-DATEN-B-P1
                                         WIEN-SATZ-DATEN-B-P2
      *
      *
      **** FELDER IM FORMAT "  . .   " (LEERES DATUMSFELD) UNERWÜNSCHT
           IF WIEN-SATZ-DATEN-A-TAG = SPACE
              MOVE SPACE TO WIEN-SATZ-DATEN-ABLAUF
                            WIEN-SATZ-DATEN-AN-BEGINN
           ELSE
              CONTINUE
           END-IF
      *
      *
      **** DOPPELPUNKT
      *
           MOVE ':'                  TO  WIEN-SATZ-DATEN-DOPPELP1
                                         WIEN-SATZ-DATEN-DOPPELP2
      *
      *
      **** SPACE
      *
           MOVE SPACE                TO  WIEN-SATZ-DATEN-SPACE
                                         WIEN-SATZ-DATEN-FILLER
      *
      *
           .
       U02-EXIT.
           EXIT.
      *-----------------------------------------------------------------
       U03-DATEN-AUS-ANTRAGTABELLEN SECTION.
      *-----------------------------------------------------------------
      * IN   : GES , PIN/VNR
      * OUT  : ERGÄNZTE AUSWERTUNGDATEN
      *-----------------------------------------------------------------
      *
           MOVE TDS-NR       TO ANTA-NR           IN PI-DRAN0140
           MOVE TDS-PGEB-GES TO PGEB-GES          IN PI-DRAN0140
           MOVE SPACE        TO ANTA-VORSCHLAG-NR IN PI-DRAN0140
      *
           SET PI-UWAN0001-DRAN0140          TO TRUE
      *
           CALL K-UWAN0001 USING BY REFERENCE PI-UWAN0001
                                              PI-DRAN0140
                                              PO-DRAN0140
                                              WF-ANTRAG-ALT
                                              WF-ANTRAG-NEU
                                              ERR-ERROR-BEREICH
                                              STANDARD-STATUS-BEREICH
           END-CALL
      *
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
           END-IF
      *
           MOVE SPACE TO WIEN-SATZ-DATEN-BERUFSGRP
      *
           EVALUATE TRUE
           WHEN PO-DRAN0140-RC-1-OK
      *
                CONTINUE
      *
           WHEN PO-DRAN0140-RC-1-NOT-FOUND
           WHEN PO-DRAN0140-RC-1-OK-OHNE-PROD
           WHEN PO-DRAN0140-RC-2-PE-S-OK
           WHEN PO-DRAN0140-RC-2-PE-S-NOK
           WHEN PO-DRAN0140-RC-3-VD-OK
           WHEN PO-DRAN0140-RC-3-VD-NOK
      *
                MOVE TDS-NR TO WIEN-SATZ-DATEN-PIN
                               WIEN-SATZ-DATEN-VNR-ANTR
      *
                MOVE ZERO   TO WIEN-SATZ-DATEN-GEWERBE-VBNR
                               WIEN-SATZ-DATEN-GEN-ANT-NR
                               WIEN-SATZ-DATEN-GEN-B-ANT-NR
                               WIEN-SATZ-DATEN-AKT-ANTR-STA
      *
                MOVE SPACE  TO WIEN-SATZ-DATEN-VORSCHL-NR
                               WIEN-SATZ-DATEN-KD-NUMMER
                               WIEN-SATZ-DATEN-KD-NUMMER-2
                               WIEN-SATZ-DATEN-KD-GEB-DAT
                               WIEN-SATZ-DATEN-KD-SOZVNR
                               WIEN-SATZ-DATEN-KD-STRASSE
                               WIEN-SATZ-DATEN-AN-BEARB-GEB
                               WIEN-SATZ-DATEN-AN-JNP
                               WIEN-SATZ-DATEN-PRAEM-SUM
                               WIEN-SATZ-DATEN-STAATS-ANG
                               WIEN-SATZ-DATEN-PEP-STATUS
                               WIEN-SATZ-DATEN-BRANCHE
      *
                PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1
                  UNTIL Z-ZAEHLER > K-4
      
                        MOVE SPACE
                          TO WIEN-S-DATEN-W-INH-KD-VORNAME(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-ZUNAME (Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-GEB-DAT(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-AUSL-KZ(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-PLZ    (Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-ORT    (Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-STAATS-ANG(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-PEP-STATUS(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-BRANCHE   (Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-BERUFSGRP (Z-ZAEHLER)
      
                END-PERFORM

                PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1
                UNTIL Z-ZAEHLER > K-5
                     MOVE SPACE
                       TO WIEN-S-DATEN-W-INH-DIL(Z-ZAEHLER)
                END-PERFORM
                
      **** HINWEIS DAS ANTRAG NICHT GEFUNDEN WURDE
                MOVE 'ANTRAG ZWISCHENZEITLICH WIEDER GELOESCHT'
                  TO          WIEN-SATZ-DATEN-BERUFSGRP
      *
                GO TO U03-EXIT
      *
           WHEN OTHER
                MOVE 'U03  '                     TO ERR-ORT-SEC
                MOVE 'UWAN0001/DRAN0140: RETURNCOCE UNGUELTIG'
                                                 TO ERR-VAR-ZEILE01
                MOVE PO-DRAN0140(01:03)          TO ERR-VAR-ZEILE02
                MOVE K-8                         TO ERR-ORT-LFD
                PERFORM BR93-PROG-FEHLER
                PERFORM U01-ABBRUCH
           END-EVALUATE
      **
      **    WEITERE FELDER FÜLLEN
      
      ****   VB-KONZESSION
           MOVE ANTA-VB-KONZESSION IN WF-ANTRAG-NEU
             TO WIEN-SATZ-DATEN-GEWERBE-VBNR
      *
      ****   IST VB-KONZESSION LEER (ZERO) HAT DER ABSCHLUSSVERMITTLER
      *      DIE NOTWENDIGE KONZESSION UND WIRD IN DAS FELD EINGESTELLT.
      *
           IF WIEN-SATZ-DATEN-GEWERBE-VBNR = ZERO
              MOVE TDS-AVERM TO WIEN-SATZ-DATEN-GEWERBE-VBNR
      *
           ELSE
      *
              CONTINUE
      
           END-IF
      *
      ****   EXTERNE VNR. FÜR GES. 33 WIRD DIE DEPOT-NR EINGESTELLT
           
           EVALUATE TDS-PGEB-GES
           WHEN K-KNR-PGES-CAPITAL-BANK
      
               MOVE ANTP-TARIF IN WF-ANTRAG-PRODUKTE-NEU(K-1) 
                 TO WIEN-SATZ-DATEN-VNR-ANTR
         
           WHEN OTHER
               
               MOVE ANTA-NR  IN WF-ANTRAG-NEU
                 TO WIEN-SATZ-DATEN-VNR-ANTR
               
           END-EVALUATE
           
      ****   PIN
           MOVE ANTA-PIN IN WF-ANTRAG-NEU
             TO WIEN-SATZ-DATEN-PIN
      *
      *
      ***    VORSCHLAGSNUMMER
           MOVE ANTA-VORSCHLAG-NR IN WF-ANTRAG-NEU
             TO WIEN-SATZ-DATEN-VORSCHL-NR
      *
      *
      ***    KUNDEN-BESTANDS-ID
           IF ANTA-KD-BEST-ID   IN WF-ANTRAG-NEU = ZERO
              MOVE SPACE TO WIEN-SATZ-DATEN-KD-NUMMER
           ELSE
              MOVE ANTA-KD-BEST-ID   IN WF-ANTRAG-NEU
                TO WIEN-SATZ-DATEN-KD-NUMMER
           END-IF
      *
      *
      ***    KUNDE GEBURTSTAG
           IF ANTA-GEB-DAT      IN WF-ANTRAG-NEU = K-DAT-MIN-EURO
              MOVE SPACE TO WIEN-SATZ-DATEN-KD-GEB-DAT
      *
           ELSE
              MOVE ANTA-GEB-DAT      IN WF-ANTRAG-NEU
                TO WIEN-SATZ-DATEN-KD-GEB-DAT
           END-IF
      *
      *
      ***    BEARBEITUNGSGEBÜR (GES. 65,69,85,95)
           EVALUATE TDS-PGEB-GES
               WHEN K-KNR-PGES-GENERALI-BANK
               WHEN K-KNR-PGES-BANK-AUSTRIA
               WHEN K-KNR-PGES-BAWAG
               WHEN K-KNR-PGES-SONST-PROD-AUSTRIA
      *
                    MOVE ANTP-ZUSATZ-1 IN WF-ANTRAG-NEU(K-1)
                      TO Z-BEITRAG-FORMATIERT
                    MOVE Z-BEITRAG-FORMATIERT
                      TO WIEN-SATZ-DATEN-AN-BEARB-GEB
      *
               WHEN OTHER
      *
                    MOVE SPACE TO WIEN-SATZ-DATEN-AN-BEARB-GEB
      *
           END-EVALUATE
      *
      *
      ***    EINMALERLAEGE (GES. 39 HAT 9 PRODUKTE)
      ***    EINMALERLAEGE (GES. 33 HAT 15 PRODUKTE)
      *
           EVALUATE TDS-PGEB-GES
              WHEN K-KNR-PGES-GENERALI-INVEST
      *
                   MOVE ZERO TO Z-BEITRAG-NUM
      *
                   PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1
                     UNTIL Z-ZAEHLER > K-9
      
      ***    NUR SUMMEN AUS EINMALZAHLUNGEN AUSWEISEN (ZAHLWEISE = 8)
                       IF TDS-ZAHLWEISE-TAB(Z-ZAEHLER) = K-8
                          COMPUTE Z-BEITRAG-NUM =  Z-BEITRAG-NUM +
                                  TDS-SUMME-TAB(Z-ZAEHLER)
                       ELSE
                          CONTINUE
                       END-IF
      *
                   END-PERFORM
      *
                       MOVE Z-BEITRAG-NUM
                         TO Z-BEITRAG-FORMATIERT
                       MOVE Z-BEITRAG-FORMATIERT
                         TO WIEN-SATZ-DATEN-EINMERLAEGE
      
      ***    CAPITAL-BANK KANN BIS ZU 15 PRODUKTE HABEN
              WHEN K-KNR-PGES-CAPITAL-BANK
                  
                   MOVE ZERO TO Z-BEITRAG-NUM
                   MOVE ZERO TO Z-BEITRAG-NUM-2
                   MOVE ZERO TO Z-BEITRAG-NUM-3
                  
                   PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1
                     UNTIL Z-ZAEHLER > K-15
      
      ***    NUR SUMMEN AUS EINMALZAHLUNGEN AUSWEISEN (ZAHLWEISE = 8)
                       IF ANTP-ZAHLWEISE 
                          IN WF-ANTRAG-PRODUKTE-NEU(Z-ZAEHLER) = K-8
                          COMPUTE Z-BEITRAG-NUM 
                                = Z-BEITRAG-NUM +
                                + ANTP-SUMME 
                                  IN WF-ANTRAG-PRODUKTE-NEU(Z-ZAEHLER)
                       ELSE
      
                          EVALUATE ANTP-ZAHLWEISE 
                                   IN WF-ANTRAG-PRODUKTE-NEU(Z-ZAEHLER)
                          WHEN K-6
      *
                               COMPUTE Z-BEITRAG-NUM-2 =
                                       Z-BEITRAG-NUM-2 +
                                       ANTP-SUMME
                                   IN WF-ANTRAG-PRODUKTE-NEU(Z-ZAEHLER)
      *
                          WHEN K-1
      
                               COMPUTE Z-BEITRAG-NUM-2 =
                                       Z-BEITRAG-NUM-2 +
                                       (K-12 * ANTP-SUMME
                                  IN WF-ANTRAG-PRODUKTE-NEU(Z-ZAEHLER))
      *
                          WHEN K-2
      
                               COMPUTE Z-BEITRAG-NUM-2 =
                                       Z-BEITRAG-NUM-2 +
                                       (K-6 * ANTP-SUMME
                                  IN WF-ANTRAG-PRODUKTE-NEU(Z-ZAEHLER))
      *
                           WHEN K-4
      
                               COMPUTE Z-BEITRAG-NUM-2 =
                                       Z-BEITRAG-NUM-2 +
                                       (K-4 * ANTP-SUMME
                                  IN WF-ANTRAG-PRODUKTE-NEU(Z-ZAEHLER))
      *
                          END-EVALUATE
      
                       END-IF
      ***    EINHEITEN  
                       COMPUTE Z-BEITRAG-NUM-3 =
                               Z-BEITRAG-NUM-3 +
                               ANTP-EINH-AVERM
                               IN WF-ANTRAG-PRODUKTE-NEU(Z-ZAEHLER)
      *
                   END-PERFORM
      ***    EINMALERLAEGE
                   IF Z-BEITRAG-NUM = ZERO
                      MOVE SPACE TO WIEN-SATZ-DATEN-EINMERLAEGE
                   
                   ELSE    
                      MOVE Z-BEITRAG-NUM
                        TO Z-BEITRAG-FORMATIERT
                      MOVE Z-BEITRAG-FORMATIERT
                        TO WIEN-SATZ-DATEN-EINMERLAEGE
                   END-IF
      ***    SUMME
                   IF Z-BEITRAG-NUM-2 = ZERO
                      MOVE SPACE
                        TO WIEN-SATZ-DATEN-AN-BEITRAG
                      MOVE SPACE
                        TO WIEN-SATZ-DATEN-BEITRAG-SUM
      *
                   ELSE
                      MOVE Z-BEITRAG-NUM-2
                        TO Z-BEITRAG-FORMATIERT
                      MOVE Z-BEITRAG-FORMATIERT
                        TO WIEN-SATZ-DATEN-BEITRAG-SUM
      *
                   END-IF
      ***    EINHEITEN            
                   IF Z-BEITRAG-NUM-3 = ZERO
                      MOVE SPACE
                        TO WIEN-SATZ-DATEN-EINHEITEN
      *
                   ELSE
                      MOVE Z-BEITRAG-NUM-3
                        TO Z-BEITRAG-FORMATIERT
                      MOVE Z-BEITRAG-FORMATIERT
                        TO WIEN-SATZ-DATEN-EINHEITEN
      *
                   END-IF
      
      ****    SIND NUR EINMALANLAGEN VORHANDEN FELD ZAHLWEISE LEER
                    IF WIEN-SATZ-DATEN-BEITRAG-SUM = SPACE
                       MOVE SPACE TO WIEN-SATZ-DATEN-AN-ZAHLW
                    ELSE
                       MOVE K-6 TO WIEN-SATZ-DATEN-AN-ZAHLW
                    END-IF
                   
      *
              WHEN OTHER
                   MOVE SPACE TO WIEN-SATZ-DATEN-EINMERLAEGE
      *
           END-EVALUATE
      *
      *
      ***   PRÄMIENZAHLUNGSDAUER IN JAHREN  (NUR GES. 26, 39 PROD
      ***   123,124,125)
      *
           MOVE ZERO TO Z-BEITRAG-NUM
      *
           EVALUATE TDS-PGEB-GES
              WHEN K-KNR-PGES-GENERALI-LEBEN
      *
                   COMPUTE Z-BEITRAG-NUM =
                   ANTP-LAUFZEIT IN WF-ANTRAG-NEU(K-1) / K-12
      *
                   MOVE Z-BEITRAG-NUM
                     TO Z-PROD-SCHL-FORMATIERT
                   MOVE Z-PROD-SCHL-FORMATIERT
                     TO WIEN-SATZ-DATEN-PZD
      *
              WHEN K-KNR-PGES-GENERALI-INVEST
      *
                   PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1 UNTIL
                           Z-ZAEHLER > K-9
      *
                            EVALUATE PROD-PROD-SCHL
                                  IN WF-ANTRAG-NEU(Z-ZAEHLER)
      *
                            WHEN K-123
                            WHEN K-124
                            WHEN K-125
      *
                               COMPUTE Z-BEITRAG-NUM =
                               ANTP-LAUFZEIT IN WF-ANTRAG-NEU(Z-ZAEHLER)
                               / K-12
      *
                               MOVE Z-BEITRAG-NUM
                                 TO Z-PROD-SCHL-FORMATIERT
                               MOVE Z-PROD-SCHL-FORMATIERT
                                 TO WIEN-SATZ-DATEN-PZD
      *
                               ADD K-10 TO Z-ZAEHLER
      *
                            WHEN OTHER
      *
                                 CONTINUE
      *
                            END-EVALUATE
      *
                   END-PERFORM
      *
              WHEN OTHER
      *
                   MOVE SPACE TO WIEN-SATZ-DATEN-PZD
      *
           END-EVALUATE
      *
      *
      ***  JAHRESNETTOPRÄMIE (NUR GES. 26)
      *
           EVALUATE TDS-PGEB-GES
              WHEN K-KNR-PGES-GENERALI-LEBEN
      *
                   MOVE ANTP-ZUSATZ-1 IN WF-ANTRAG-NEU(K-1)
                     TO Z-BEITRAG-NUM
                   MOVE Z-BEITRAG-NUM
                     TO Z-BEITRAG-FORMATIERT
                   MOVE Z-BEITRAG-FORMATIERT
                     TO WIEN-SATZ-DATEN-AN-JNP
      *
              WHEN OTHER
      *
                   MOVE SPACE TO WIEN-SATZ-DATEN-AN-JNP
      *
           END-EVALUATE
      *
      *
      ***  PRÄMIENSUMME (NUR GES. 26, 39 PROD. 123,124,125)
      *
           EVALUATE TDS-PGEB-GES
              WHEN K-KNR-PGES-GENERALI-LEBEN
      *
                   MOVE ANTP-ZUSATZ-2 IN WF-ANTRAG-NEU(K-1)
                     TO Z-BEITRAG-NUM
                   MOVE Z-BEITRAG-NUM
                     TO Z-BEITRAG-FORMATIERT
                   MOVE Z-BEITRAG-FORMATIERT
                     TO WIEN-SATZ-DATEN-PRAEM-SUM
      *
              WHEN K-KNR-PGES-GENERALI-INVEST
      *
                   PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1 UNTIL
                           Z-ZAEHLER > K-9
      *
                           EVALUATE TDS-PROD-PROD-SCHL-TAB(Z-ZAEHLER)
                           WHEN K-123
                           WHEN K-124
                           WHEN K-125
      *
                                COMPUTE Z-BEITRAG-NUM =
                                        ANTP-SUMME
                                        IN WF-ANTRAG-NEU(Z-ZAEHLER)
                                        * ANTP-LAUFZEIT
                                        IN WF-ANTRAG-NEU(Z-ZAEHLER)
      *
                           WHEN OTHER
      *
                                CONTINUE
      *
                           END-EVALUATE
      *
                   END-PERFORM
      *
                   MOVE Z-BEITRAG-NUM
                     TO Z-BEITRAG-FORMATIERT
                   MOVE Z-BEITRAG-FORMATIERT
                     TO WIEN-SATZ-DATEN-PRAEM-SUM
      *
              WHEN OTHER
      *
                   MOVE SPACE TO WIEN-SATZ-DATEN-PRAEM-SUM
      *
           END-EVALUATE
      *
      *
      **** STAATSANGEHÖRIGKEIT KUNDE
      *
           SET  PI-ZWAT0008-DRAT0151 TO TRUE
           MOVE ANTA-KUNDE-STAA IN WF-ANTRAG-NEU
             TO PI-DRAT0151-KNR-NR
           MOVE K-KTY-AUSL
             TO PI-DRAT0151-KTY-NR

           PERFORM U11-ZWAT0008
      *
           EVALUATE TRUE
           WHEN PO-DRAT0151-RC-OK
      *         OK
                MOVE PO-DRAT0151-KNR-BEZ-KURZ
                  TO WIEN-SATZ-DATEN-STAATS-ANG
      *
           WHEN OTHER
      *
                MOVE SPACE TO WIEN-SATZ-DATEN-STAATS-ANG
      *
           END-EVALUATE
      *
      *
      **** PEP-STATUS KUNDE
      *
           MOVE ANTA-KUNDE-PEP IN WF-ANTRAG-NEU
             TO WIEN-SATZ-DATEN-PEP-STATUS
      *
      **** DIL-STATUS KUNDE
      *    WIRD VORERST NUR FÜR GES. 72 FÜR DEN ERSTEN INHABER ERFASST.
      *    FÜR WEITERE GESELLSCHAFTEN AB DEM ZWEITEN INHABER ÜBER
      *    DIALOG "WEITERE INHABER". 
      *    DIESE INFOMATIONEN WERDEN IM WEITEREN PROGRAMMABLAUF 
      *    HINZUGEFÜGT
           EVALUATE TDS-PGEB-GES
           WHEN K-KNR-PGES-INFINA
      *
                MOVE ANTA-ZUSATZ-1 IN WF-ANTRAG-NEU(1:1)
                  TO WIEN-S-DATEN-W-INH-DIL(K-1)
                
           WHEN OTHER
                MOVE SPACE TO WIEN-S-DATEN-W-INH-DIL(K-1)
           
           END-EVALUATE
                   
      *
      **** BRANCHE KUNDE
      *
           MOVE FUNCTION UPPER-CASE(ANTA-KUNDE-BRAN IN WF-ANTRAG-NEU)
             TO WIEN-SATZ-DATEN-BRANCHE
           MOVE WIEN-SATZ-DATEN-BRANCHE
             TO Z-UMLAUTE-GROSS
           PERFORM U12-UMLAUTE-GROSS
           MOVE Z-UMLAUTE-GROSS
             TO WIEN-SATZ-DATEN-BRANCHE
      *
      **** BERUFSGRUPPE KUNDE
      *
           MOVE FUNCTION UPPER-CASE(ANTA-KUNDE-BGRP IN WF-ANTRAG-NEU)
             TO WIEN-SATZ-DATEN-BERUFSGRP
           MOVE WIEN-SATZ-DATEN-BERUFSGRP
             TO Z-UMLAUTE-GROSS
           PERFORM U12-UMLAUTE-GROSS
           MOVE Z-UMLAUTE-GROSS
             TO WIEN-SATZ-DATEN-BERUFSGRP
      *
      **** WENN KEIN WERT FÜR BRANCHE DEFAULTWERTE FÜR PEP-STATUS UND
      **** STAATSANGEHÖRIGKEIT ENTFERNEN
           EVALUATE TRUE
           WHEN WIEN-SATZ-DATEN-BRANCHE = SPACE
                MOVE SPACE TO WIEN-SATZ-DATEN-PEP-STATUS
                MOVE SPACE TO WIEN-SATZ-DATEN-STAATS-ANG
      *
           WHEN OTHER
      *
                CONTINUE
      *
           END-EVALUATE
      *
      **** WEITERE INHABER ANTRAG. NUR BEI BETROFFENEN GESELLSCHAFTEN
      *
           EVALUATE TRUE
           WHEN TDS-PGEB-GES = K-KNR-PGES-CAPITAL-BANK
           WHEN TDS-PGEB-GES = K-KNR-PGES-DWS-OESTERREICH
           WHEN TDS-PGEB-GES = K-KNR-PGES-GENERALI-INVEST
           WHEN TDS-PGEB-GES = K-KNR-PGES-DIT-OEST
           WHEN TDS-PGEB-GES = K-KNR-PGES-BANK-AUSTRIA-INVEST
           WHEN TDS-PGEB-GES = K-KNR-PGES-BANK-AUSTRIA
           WHEN TDS-PGEB-GES = K-KNR-PGES-INFINA         
           WHEN TDS-PGEB-GES = K-KNR-PGES-S-BAUSPARKASSE
           WHEN TDS-PGEB-GES = K-KNR-PGES-BAWAG
           WHEN TDS-PGEB-GES = K-KNR-PGES-SONST-PROD-AUSTRIA
      *
      *    INITIALIZE AUSGABE
                PERFORM VARYING Z-ZAEHLER FROM K-1 BY K-1
                  UNTIL Z-ZAEHLER > K-4
      
                        MOVE SPACE
                          TO WIEN-S-DATEN-W-INH-KD-VORNAME(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-ZUNAME (Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-GEB-DAT(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-AUSL-KZ(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-PLZ    (Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-KD-ORT    (Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-STAATS-ANG(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-PEP-STATUS(Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-BRANCHE   (Z-ZAEHLER)
                             WIEN-S-DATEN-W-INH-BERUFSGRP (Z-ZAEHLER)
      
                END-PERFORM
                
                PERFORM VARYING Z-ZAEHLER FROM K-2 BY K-1
                  UNTIL Z-ZAEHLER > K-5
                        MOVE SPACE
                          TO WIEN-S-DATEN-W-INH-DIL(Z-ZAEHLER)
                END-PERFORM
      *
      **** ERSTER INHABER IST BEREITS BEHANDELT.
      **** HIER WEITERE INHABER.
      
                PERFORM VARYING Z-ZAEHLER FROM K-2 BY K-1
                  UNTIL Z-ZAEHLER > K-5
                     OR PO-INHABER-INFO-AUSLKZ(Z-ZAEHLER) NOT > K-ZERO
      
                        COMPUTE Z-ZAEHLER2 = Z-ZAEHLER - 1
      
      *** VORNAME
                        MOVE PO-INHABER-INFO-VORNAME
                          IN PO-DRAN0140(Z-ZAEHLER)
                          TO WIEN-S-DATEN-W-INH-KD-VORNAME(Z-ZAEHLER2)
      
      *** ZUNAME
                        MOVE PO-INHABER-INFO-ZUNAME
                          IN PO-DRAN0140(Z-ZAEHLER)
                          TO WIEN-S-DATEN-W-INH-KD-ZUNAME(Z-ZAEHLER2)
      
      *** GEBDAT ( DATUM AUF VON TTMMJJ AUF TT.MM.JJJH ÄNDERN )
                        MOVE PO-INHABER-INFO-GEBDAT
                          IN PO-DRAN0140(Z-ZAEHLER)
                          TO PI-DRPA0131-DATUM-6
      
                        PERFORM U10-DATUM-PRUEF
      
                        EVALUATE TRUE
                        WHEN PO-DRPA0131-RC-OK
      
      *  PRÜFUNG, OB DATUM > TAGESDATUM
      
                             SET PI-DRPA0146-FORMAT-MON-MIT TO TRUE
                             MOVE PO-DRPA0131-DATUM
                               TO PI-DRPA0146-DATUM-ANF
                             MOVE Z-TAGESDATUM-EUR
                               TO PI-DRPA0146-DATUM-END
      *
                             PERFORM U10A-DATUM-PRUEF-2
      *
                             EVALUATE TRUE
                             WHEN PO-DRPA0146-RC-NOK
                             WHEN PO-DRPA0146-RC-EDAT-NOK
      *  DATUM IT GRÖSSER TAGESDATUM,
      *  ALSO - 1 JAHRHUNDERT
                                  MOVE PO-DRPA0131-DATUM
                                    TO Z-TAGESDATUM-TDS-LANG
      *
      *  JAHRHUNDERT SUBTRAHIEREN
                                  SUBTRACT K-1
                                  FROM Z-TAGESDATUM-TDS-LANG-JH-9
      *
                                  MOVE Z-TAGESDATUM-TDS-LANG
                                    TO PO-DRPA0131-DATUM
      *
                             WHEN OTHER
      *  OK DATUM IT NICHT GRÖSSER TAGESDATUM
                                  MOVE PO-DRPA0131-DATUM
                                    TO Z-TAGESDATUM-TDS-LANG
      *
                             END-EVALUATE
      
                        WHEN OTHER
      
      *  DATUM IST FEHLERHAFT SPACE EINSTELEN
                             MOVE SPACE TO Z-TAGESDATUM-TDS-LANG-JH
      
                        END-EVALUATE
      
                        EVALUATE TRUE
                        WHEN PO-DRPA0131-RC-OK
                             MOVE PO-DRPA0131-DATUM TO
                             WIEN-S-DATEN-W-INH-KD-GEB-DAT(Z-ZAEHLER2)
      
                        WHEN OTHER
                             MOVE SPACE TO
                             WIEN-S-DATEN-W-INH-KD-GEB-DAT(Z-ZAEHLER2)
      
                        END-EVALUATE
      
      *** AUSLANDSKENNZEICHEN (ORT)
                        MOVE PO-INHABER-INFO-AUSLKZ
                          IN PO-DRAN0140(Z-ZAEHLER)
                          TO PI-DRAT0151-KNR-NR
                        MOVE K-KTY-AUSL
                          TO PI-DRAT0151-KTY-NR
      
                        PERFORM U11-ZWAT0008
      
                        EVALUATE TRUE
                        WHEN PO-DRAT0151-RC-OK
      *                      OK
                             MOVE PO-DRAT0151-KNR-BEZ-KURZ TO
                             WIEN-S-DATEN-W-INH-KD-AUSL-KZ(Z-ZAEHLER2)
      
                        WHEN OTHER
      
                             MOVE SPACE TO
                             WIEN-S-DATEN-W-INH-KD-AUSL-KZ(Z-ZAEHLER2)
      
                        END-EVALUATE
      
      *** PLZ
      
      **** AUSLANDKENNZEICHEN ÖSTERREICH (A) KANN BEI ALTANTRÄGEN
      *    AUCH IN DER PLZ AUFTAUCHEN.
      *
                        IF PO-INHABER-INFO-PLZ
                        IN PO-DRAN0140(Z-ZAEHLER)(1:1) = K-A
      
                           MOVE PO-INHABER-INFO-PLZ
                             IN PO-DRAN0140(Z-ZAEHLER)(2:8)
                             TO WIEN-S-DATEN-W-INH-KD-PLZ(Z-ZAEHLER2)
      
                        ELSE
      
                           MOVE PO-INHABER-INFO-PLZ
                             IN PO-DRAN0140(Z-ZAEHLER)
                             TO WIEN-S-DATEN-W-INH-KD-PLZ(Z-ZAEHLER2)
      
                        END-IF
      
      *** ORT
                        MOVE PO-INHABER-INFO-ORT
                          IN PO-DRAN0140(Z-ZAEHLER)
                          TO WIEN-S-DATEN-W-INH-KD-ORT(Z-ZAEHLER2)
      
      *** STAATSANGEHÖRIGKEIT
                        MOVE PO-INHABER-INFO-STAANG
                          IN PO-DRAN0140(Z-ZAEHLER)
                          TO PI-DRAT0151-KNR-NR
                        MOVE K-KTY-AUSL
                          TO PI-DRAT0151-KTY-NR
      
                        PERFORM U11-ZWAT0008
      
                        EVALUATE TRUE
                        WHEN PO-DRAT0151-RC-OK
      *                      OK
                             MOVE PO-DRAT0151-KNR-BEZ-KURZ TO
                             WIEN-S-DATEN-W-INH-STAATS-ANG(Z-ZAEHLER2)
      
                        WHEN OTHER
      
                             MOVE SPACE TO
                             WIEN-S-DATEN-W-INH-STAATS-ANG(Z-ZAEHLER2)
      
                        END-EVALUATE
      
      *** PEP STATUS
                        MOVE PO-INHABER-INFO-PEP-STAT
                          IN PO-DRAN0140(Z-ZAEHLER)
                          TO WIEN-S-DATEN-W-INH-PEP-STATUS(Z-ZAEHLER2)
      
      *** BRANCHE
                        MOVE PO-INHABER-INFO-BRAN
                          IN PO-DRAN0140(Z-ZAEHLER)
                          TO PI-DRAT0151-KNR-NR
                        MOVE K-KTY-OE-KUNDE-BRANCHE
                          TO PI-DRAT0151-KTY-NR
      
                        PERFORM U11-ZWAT0008
      
                        EVALUATE TRUE
                        WHEN PO-DRAT0151-RC-OK
      *                      OK
                             MOVE PO-DRAT0151-KNR-BEZ(1:40) TO
                             WIEN-S-DATEN-W-INH-BRANCHE(Z-ZAEHLER2)
      
                        WHEN OTHER
      
                             MOVE SPACE TO
                             WIEN-S-DATEN-W-INH-BRANCHE(Z-ZAEHLER2)
      
                        END-EVALUATE
      
      *** BERUFSGRUPPE
                        MOVE PO-INHABER-INFO-BGRP
                          IN PO-DRAN0140(Z-ZAEHLER)
                          TO PI-DRAT0151-KNR-NR
                        MOVE K-KTY-OE-KUNDE-BERUFSGRPUPPE
                          TO PI-DRAT0151-KTY-NR
      
                        PERFORM U11-ZWAT0008
      
                        EVALUATE TRUE
                        WHEN PO-DRAT0151-RC-OK
      *                      OK
                             MOVE PO-DRAT0151-KNR-BEZ(1:40) TO
                             WIEN-S-DATEN-W-INH-BERUFSGRP(Z-ZAEHLER2)
      
                        WHEN OTHER
      
                             MOVE SPACE TO
                             WIEN-S-DATEN-W-INH-BERUFSGRP(Z-ZAEHLER2)
      
                        END-EVALUATE
                        
                END-PERFORM

                PERFORM VARYING Z-ZAEHLER FROM K-2 BY K-1
                  UNTIL Z-ZAEHLER > K-5
                     OR PO-INHABER-INFO-AUSLKZ(Z-ZAEHLER) NOT > K-ZERO
      
      *** STATUS DEVISENINLÄNDER (DIL)
                        EVALUATE TRUE
                        WHEN PO-INHABER-INFO-DIL-STAT
                          IN PO-DRAN0140(Z-ZAEHLER) = K-JA
                        WHEN PO-INHABER-INFO-DIL-STAT
                          IN PO-DRAN0140(Z-ZAEHLER) = K-NEIN
                             MOVE PO-INHABER-INFO-DIL-STAT
                               IN PO-DRAN0140(Z-ZAEHLER)
                               TO WIEN-S-DATEN-W-INH-DIL(Z-ZAEHLER)
                             
                        WHEN OTHER
                             MOVE SPACE
                               TO WIEN-S-DATEN-W-INH-DIL(Z-ZAEHLER)
                             
                        END-EVALUATE
                        
                END-PERFORM
      
           WHEN OTHER
      
                CONTINUE
      
           END-EVALUATE
      *
      **** AKTUELLER ANTRAGSSTATUS (ERSTER TEIL)
      
           SET  PI-ZWAT0008-DRAT0151 TO TRUE
           MOVE ANTA-ANTR-STATUS-1 IN WF-ANTRAG-NEU
             TO PI-DRAT0151-KNR-NR
           MOVE K-KTY-ANTRAG-STATUS
             TO PI-DRAT0151-KTY-NR
      
           PERFORM U11-ZWAT0008
      
           EVALUATE TRUE
           WHEN PO-DRAT0151-RC-OK
      *         OK
                MOVE PO-DRAT0151-KNR-BEZ-KURZ(1:1)
                  TO WIEN-SATZ-DATEN-AKT-ANTR-STA(1:1)
      *
           WHEN OTHER
      *
                MOVE '00' TO WIEN-SATZ-DATEN-AKT-ANTR-STA
      *
           END-EVALUATE
      
      **** AKTUELLER ANTRAGSSTATUS (ZWEITER TEIL) STATISTIKKENNZEICHEN
           EVALUATE TRUE
           WHEN ANTA-STATISTIK IN WF-ANTRAG-NEU = K-NEIN
                MOVE '1' TO WIEN-SATZ-DATEN-AKT-ANTR-STA(2:1)
      
           WHEN OTHER
                MOVE '0' TO WIEN-SATZ-DATEN-AKT-ANTR-STA(2:1)
      
           END-EVALUATE
      *
           .
       U03-EXIT.
           EXIT.
      *-----------------------------------------------------------------
       U041-DATEN-AUS-VORSCHLAG-16 SECTION.
      *-----------------------------------------------------------------
      * IN   : GES , VORSCHLAGSNUMMER
      * OUT  : ERGÄNZTE AUSWERTUNGDATEN
      *-----------------------------------------------------------------
      *
           INITIALIZE PI-DRAN0207, PO-DRAN0207
           SET  PI-UPAN0020-DRAN0207  TO TRUE
      *
           MOVE K-16
           TO PI-DRAN0207-PGEB-GES
      *
           MOVE WIEN-SATZ-DATEN-VORSCHL-NR
           TO PI-DRAN0207-GENS-HVANTNUM
      *
           MOVE WIEN-SATZ-DATEN-VBNR
           TO PI-DRAN0207-GENS-AVERM
      *
           CALL K-UPAN0020 USING BY REFERENCE PI-UPAN0020
                                              PI-DRAN0207
                                              PO-DRAN0207
                                              ERR-ERROR-BEREICH
                                              STANDARD-STATUS-BEREICH
           END-CALL
      *
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
           END-IF
      *
           EVALUATE TRUE
           WHEN PO-DRAN0207-RC-OK
      *
                MOVE GENS-PC-KD-BEST-ID   TO WIEN-SATZ-DATEN-KD-NUMMER
      *
      *
      **** SOZIALVERSICHERUNGSNUMMER
                MOVE GENS-PC-VN-SVNR-E    TO Z-SOZ-NR-FORMATIERT
                MOVE Z-SOZ-NR-1BIS4       TO WIEN-SATZ-DATEN-KD-SOZVNR
      *
      *
      **** STRASSE
                MOVE FUNCTION UPPER-CASE(GENS-PC-VN-STR-E)
                  TO WIEN-SATZ-DATEN-KD-STRASSE
      *
                INSPECT WIEN-SATZ-DATEN-KD-STRASSE REPLACING
                ALL '_' BY SPACE
                MOVE WIEN-SATZ-DATEN-KD-STRASSE
                  TO Z-UMLAUTE-GROSS
                PERFORM U12-UMLAUTE-GROSS
                MOVE Z-UMLAUTE-GROSS
                  TO WIEN-SATZ-DATEN-KD-STRASSE
      *
      **** GWO-VB
                IF WIEN-SATZ-DATEN-GEWERBE-VBNR = ZERO
                   MOVE GENS-PC-VB-GW-E
                     TO WIEN-SATZ-DATEN-GEWERBE-VBNR
                ELSE
                   CONTINUE
                END-IF
      *
      *** IST GWO-VB NOCH IMMER LEER IST DER AVERM = GWO-VB
                IF WIEN-SATZ-DATEN-GEWERBE-VBNR = ZERO
                   MOVE WIEN-SATZ-DATEN-VBNR
                     TO WIEN-SATZ-DATEN-GEWERBE-VBNR
                ELSE
                   CONTINUE
                END-IF
      *
      *** INTERNE IDENTIFIKATIONSNUMMERN DER GENERALI
      *** ANTRAGSNUMMER
                MOVE GENS-PC-GEN-ANT-NR
                  TO WIEN-SATZ-DATEN-GEN-ANT-NR
      *
      *** BÜNDELANTRAGSNUMMER
                MOVE GENS-PC-GEN-B-ANT-NR
                  TO WIEN-SATZ-DATEN-GEN-B-ANT-NR
      *
           WHEN OTHER
      *
                MOVE SPACE TO WIEN-SATZ-DATEN-KD-NUMMER
                              WIEN-SATZ-DATEN-KD-SOZVNR
                              WIEN-SATZ-DATEN-KD-STRASSE
      *
                MOVE ZERO  TO WIEN-SATZ-DATEN-GEN-ANT-NR
                              WIEN-SATZ-DATEN-GEN-B-ANT-NR
      *
           END-EVALUATE
      *
           .
       U041-EXIT.
           EXIT.
      *
      *-----------------------------------------------------------------
       U042-DATEN-AUS-VORSCHLAG-26 SECTION.
      *-----------------------------------------------------------------
      * IN   : GES , VORSCHLAGSNUMMER
      * OUT  : ERGÄNZTE AUSWERTUNGDATEN
      *-----------------------------------------------------------------
      *
           INITIALIZE PI-DRAN0176, PO-DRAN0176
           SET  PI-UPAN0019-DRAN0176  TO TRUE
      *
           MOVE K-26
           TO PI-DRAN0176-PGEB-GES
      *
           MOVE WIEN-SATZ-DATEN-VORSCHL-NR
           TO PI-DRAN0176-GENL-HVANTNUM
      *
           MOVE WIEN-SATZ-DATEN-VBNR
           TO PI-DRAN0176-GENL-AVERM
      *
           CALL K-UPAN0019 USING BY REFERENCE PI-UPAN0019
                                              PI-DRAN0176
                                              PO-DRAN0176
                                              ERR-ERROR-BEREICH
                                              STANDARD-STATUS-BEREICH
           END-CALL
      *
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
           END-IF
      *
           EVALUATE TRUE
           WHEN PO-DRAN0176-RC-OK
      *
                MOVE GENL-PC-KD-BEST-ID TO WIEN-SATZ-DATEN-KD-NUMMER
      *
      *
      **** SOZIALVERSICHERUNGSNUMMER
                MOVE GENL-PC-VN-SVNR-E  TO Z-SOZ-NR-FORMATIERT
                MOVE Z-SOZ-NR-1BIS4     TO WIEN-SATZ-DATEN-KD-SOZVNR
      *
      *
      **** STRASSE
                MOVE FUNCTION UPPER-CASE(GENL-PC-VN-STR-E)
                  TO WIEN-SATZ-DATEN-KD-STRASSE
      *
                INSPECT WIEN-SATZ-DATEN-KD-STRASSE REPLACING
                ALL '_' BY SPACE
                MOVE WIEN-SATZ-DATEN-KD-STRASSE
                  TO Z-UMLAUTE-GROSS
                PERFORM U12-UMLAUTE-GROSS
                MOVE Z-UMLAUTE-GROSS
                  TO WIEN-SATZ-DATEN-KD-STRASSE
      *
      **** GWO-VB
      *
                IF WIEN-SATZ-DATEN-GEWERBE-VBNR = ZERO
                   MOVE GENL-PC-VB-GW-E
                     TO WIEN-SATZ-DATEN-GEWERBE-VBNR
                ELSE
                   CONTINUE
                END-IF
      *
      *** IST GWO-VB NOCH IMMER LEER IST DER AVERM = GWO-VB
                IF WIEN-SATZ-DATEN-GEWERBE-VBNR = ZERO
                   MOVE WIEN-SATZ-DATEN-VBNR
                     TO WIEN-SATZ-DATEN-GEWERBE-VBNR
                ELSE
                   CONTINUE
                END-IF
      *
      *** INTERNE IDENTIFIKATIONSNUMMERN DER GENERALI
      *** ANTRAGSNUMMER
                MOVE GENL-PC-GEN-ANT-NR
                  TO WIEN-SATZ-DATEN-GEN-ANT-NR
      *** BÜNDELANTRAGSNUMMER
                MOVE GENL-PC-GEN-B-ANT-NR
                  TO WIEN-SATZ-DATEN-GEN-B-ANT-NR
      *
           WHEN OTHER
      *
                MOVE SPACE TO WIEN-SATZ-DATEN-KD-NUMMER
                              WIEN-SATZ-DATEN-KD-SOZVNR
                              WIEN-SATZ-DATEN-KD-STRASSE
      *
                MOVE ZERO  TO WIEN-SATZ-DATEN-GEN-ANT-NR
                              WIEN-SATZ-DATEN-GEN-B-ANT-NR
      *
           END-EVALUATE
      *
           .
       U042-EXIT.
           EXIT.
      
      *-----------------------------------------------------------------
       U043-DATEN-AUS-VORSCHLAG-33 SECTION.
      *-----------------------------------------------------------------
      * IN   : GES , VORSCHLAGSNUMMER
      * OUT  : ERGÄNZTE AUSWERTUNGDATEN
      *-----------------------------------------------------------------
      *
           INITIALIZE PI-DRAN0385, PO-DRAN0385
           SET  PI-UPAN0023-DRAN0385  TO TRUE
      *
           MOVE K-33
           TO PI-DRAN0176-PGEB-GES
      *
           MOVE WIEN-SATZ-DATEN-VORSCHL-NR
           TO PI-DRAN0385-CABA-HVANTNUM
      *
           CALL K-UPAN0023 USING BY REFERENCE PI-UPAN0023
                                              PI-DRAN0385
                                              PO-DRAN0385
                                              ERR-ERROR-BEREICH
                                              STANDARD-STATUS-BEREICH
           END-CALL
      *
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
           END-IF
      *
           EVALUATE TRUE
           WHEN PO-DRAN0385-RC-OK
      *
                MOVE PO-DRAN0385-CABA-AS-KD-BID(K-2) 
                  TO WIEN-SATZ-DATEN-KD-NUMMER-2
      *
      **** GWO-VB
      *
                IF WIEN-SATZ-DATEN-GEWERBE-VBNR = ZERO
                   MOVE PO-DRAN0385-CABA-GEWERBE-VB
                     TO WIEN-SATZ-DATEN-GEWERBE-VBNR
                ELSE
                   CONTINUE
                END-IF
      *
      *** IST GWO-VB NOCH IMMER LEER IST DER AVERM = GWO-VB
                IF WIEN-SATZ-DATEN-GEWERBE-VBNR = ZERO
                   MOVE WIEN-SATZ-DATEN-VBNR
                     TO WIEN-SATZ-DATEN-GEWERBE-VBNR
                ELSE
                   CONTINUE
                END-IF
      *
           .
       U043-EXIT.
           EXIT.
      
      *-----------------------------------------------------------------
       U05-SCHREIBEN-WIEN SECTION.
      *-----------------------------------------------------------------
      * IN   : DATENSATZ
      * OUT  : -
      *-----------------------------------------------------------------
      *
           WRITE WIEN-SATZ-AUS
      *
           IF NOT S-WIEN-AUS-OK
              MOVE 'U05  '           TO ERR-ORT-SEC
              MOVE K-9               TO ERR-ORT-LFD
              MOVE 'FEHLER BEIM SCHREIBEN WIEN-AUS-DATEI'
                                     TO ERR-VAR-ZEILE01
              MOVE S-WIEN-AUS-STATUS  TO S-DATEI-STATUS
              PERFORM BR94-DATEI-FEHLER
              PERFORM U01-ABBRUCH
           END-IF
      *
           ADD  K-1               TO Z-ZAEHLER-AUSGABE
      *
           .
       U05-EXIT.
           EXIT.
      *
      *----------------------------------------------------------
       U07-TAGESDATUM-TDS SECTION.
       AAA-1000.
      *----------------------------------------------------------
      *
           IF S-TAGESDATUM-OK
              CONTINUE
      *
           ELSE
      *
              READ TDS-EINGABE
      *
              EVALUATE TRUE
              WHEN S-TDSFILE-EIN-OK
      *
                   EVALUATE TDS-PGEB-GES
                   WHEN K-KNR-PGES-ALLIANZ-SACH
                   WHEN K-KNR-PGES-GENERALI-SACH
                   WHEN K-KNR-PGES-ALLIANZ-LEBEN
                   WHEN K-KNR-PGES-GENERALI-LEBEN
                   WHEN K-KNR-PGES-CAPITAL-BANK
                   WHEN K-KNR-PGES-DWS-OESTERREICH
                   WHEN K-KNR-PGES-GENERALI-INVEST
                   WHEN K-KNR-PGES-DIT-OEST
                   WHEN K-KNR-PGES-BANK-AUSTRIA-INVEST
                   WHEN K-KNR-PGES-SONST-GES-AT-HAFTPF
                   WHEN K-KNR-PGES-BANK-AUSTRIA
                   WHEN K-KNR-PGES-GENERALI-BANK
                   WHEN K-KNR-PGES-INFINA
                   WHEN K-KNR-PGES-S-BAUSPARKASSE
                   WHEN K-KNR-PGES-BAWAG
                   WHEN K-KNR-PGES-SONST-PROD-AUSTRIA
      *
                        EVALUATE TDS-MODUS
                        WHEN 'I '
      *
      * SCHREIBEN ÜBERSCHRIFT + DATUM IN AUSGABE-DATEI
                             MOVE SPACE    TO WIEN-SATZ-AUS
      *
                             MOVE 'TAEGL  ERFASSTE ANTRAEGE VOM:'
                               TO WIEN-SATZ-AUS (31:29)
      *** DATUM
                             MOVE TDS-DATUM
                               TO Z-TAGESDATUM-TDS-FORMAT
                             MOVE Z-TAGESDATUM-TDS-FORMAT(6:2)
                               TO Z-TAGESDATUM-TDS-LANG-TT
                                  PI-DRPA0131-DATUM-6(1:2)
                             MOVE Z-TAGESDATUM-TDS-FORMAT(4:2)
                               TO Z-TAGESDATUM-TDS-LANG-MM
                                  PI-DRPA0131-DATUM-6(3:2)
                             MOVE Z-TAGESDATUM-TDS-FORMAT(2:2)
                               TO PI-DRPA0131-DATUM-6(5:2)
      *
                             PERFORM U10-DATUM-PRUEF
      *
                             MOVE Z-TAGESDATUM-TDS-LANG
                               TO WIEN-SATZ-AUS (71:10)
      *
                             PERFORM U05-SCHREIBEN-WIEN
      *
      *** LEERZEILE
                             MOVE SPACE TO WIEN-SATZ-AUS
      *
                             PERFORM U05-SCHREIBEN-WIEN
      *
      ***    SPALTENÜBERSCHRIFTEN
                             MOVE Z-WIEN-SATZ-KOPF-UEBERSCHR
                               TO WIEN-SATZ-AUS
      *
                             PERFORM U05-SCHREIBEN-WIEN
      *
                             MOVE SPACE TO WIEN-SATZ-AUS
      *
                             SET S-TAGESDATUM-OK TO TRUE
      *
                             CLOSE TDS-EINGABE
      *
                             IF NOT S-TDSFILE-EIN-OK
                                MOVE 'U07  '       TO ERR-ORT-SEC
                                MOVE K-10          TO ERR-ORT-LFD
                                MOVE 'FEHLER BEIM SCHLIEßEN TDS-EINGA
      -                               'BE-DATEI'  TO ERR-VAR-ZEILE01
                                MOVE S-TDSFILE-EIN-STATUS
                                  TO ERR-VAR-ZEILE02
                                PERFORM BR94-DATEI-FEHLER
                                PERFORM U01-ABBRUCH
                             END-IF
      *
                             GO TO U07-EXIT
      *
                        WHEN OTHER
                             GO TO AAA-1000
      *
                        END-EVALUATE
      *
                   WHEN OTHER
                        GO TO AAA-1000
      *
                   END-EVALUATE
      *
              WHEN S-TDSFILE-EIN-EOF
      *
      *** SIND KEINE TDS-SÄTZE VORHANDEN WIRD PER SQL DAS TAGESDATUM
      *** ERMITTELT UND IN DIE AUSGABEDATEI GESCHREIBEN
                   PERFORM U08-TAGESDATUM
      
      * SCHREIBEN ÜBERSCHRIFT + DATUM IN AUSGABE-DATEI
                   MOVE SPACE    TO WIEN-SATZ-AUS
      *
                   MOVE 'TAEGL  ERFASSTE ANTRAEGE VOM:'
                     TO WIEN-SATZ-AUS (31:29)
      *** DATUM
                    MOVE Z-TAGESDATUM-EUR
                      TO WIEN-SATZ-AUS (71:10)
      *
                    PERFORM U05-SCHREIBEN-WIEN
      *
      *** LEERZEILE
                    MOVE SPACE TO WIEN-SATZ-AUS
      *
                    PERFORM U05-SCHREIBEN-WIEN
      *
      *** SPALTENÜBERSCHRIFTEN
                    MOVE Z-WIEN-SATZ-KOPF-UEBERSCHR
                      TO WIEN-SATZ-AUS
      *
                    PERFORM U05-SCHREIBEN-WIEN
      *
                    MOVE SPACE TO WIEN-SATZ-AUS
      *
                    SET S-TAGESDATUM-OK TO TRUE
      *
                    CLOSE TDS-EINGABE
      *
                    IF NOT S-TDSFILE-EIN-OK
                       MOVE 'U07  '       TO ERR-ORT-SEC
                       MOVE K-11          TO ERR-ORT-LFD
                       MOVE 'FEHLER BEIM SCHLIEßEN TDS-EINGABE-DATEI'
                         TO ERR-VAR-ZEILE01
                       MOVE S-TDSFILE-EIN-STATUS
                         TO ERR-VAR-ZEILE02
                       PERFORM BR94-DATEI-FEHLER
                       PERFORM U01-ABBRUCH
                    END-IF
      *
                    GO TO U07-EXIT
      *
              WHEN OTHER
                   MOVE 'U07 '                       TO ERR-ORT-SEC
                   MOVE 'DATEI-STATUS UNGUELTIG'     TO ERR-VAR-ZEILE01
                   MOVE S-TDSFILE-EIN-STATUS         TO S-DATEI-STATUS
                   MOVE S-DATEI-STATUS               TO ERR-VAR-ZEILE02
                   MOVE K-12                         TO ERR-ORT-LFD
                   PERFORM BR94-DATEI-FEHLER
                   PERFORM U01-ABBRUCH
      *
              END-EVALUATE
      *
           END-IF
      *
           .
       U07-EXIT.
           EXIT.
      *
      *----------------------------------------------------------
       U08-TAGESDATUM SECTION.
      *----------------------------------------------------------
           PERFORM SQL-SEL-TAGESDATUM
           IF SQLCODE NOT = KS-OK
              MOVE 'U08  '               TO ERR-ORT-SEC
              MOVE K-13                  TO ERR-ORT-LFD
              PERFORM BR91-SQL-FEHLER
           END-IF
      *
           .
       U08-EXIT.
           EXIT.
      *
      *----------------------------------------------------------
       U09-ROLLBACK SECTION.
      *----------------------------------------------------------
           PERFORM SQL-ROLLBACK
           IF SQLCODE NOT = KS-OK
              MOVE 'U09  '               TO ERR-ORT-SEC
              MOVE K-14                  TO ERR-ORT-LFD
              PERFORM BR91-SQL-FEHLER
           ELSE
              DISPLAY '       ===>  ROLLBACK AUSGEFÜHRT <==='
           END-IF
      *
           .
       U09-EXIT.
           EXIT.

      *------------------------------------------------------------*
       U10-DATUM-PRUEF SECTION.
       U10-ANF.
      *------------------------------------------------------------*
      * IN   : DATUM 6 STELLIG
      * OUT  : DATUM 10 STELLIG
      * VERAR: DATUM PRÜFEN
      *------------------------------------------------------------*
           MOVE 'U10  ' TO ERR-ORT-SEC
      *
           SET PI-UPPA0016-DRPA0131      TO TRUE
           SET PI-DRPA0131-FORMAT-TTMMJJ TO TRUE
      *
           CALL K-UPPA0016 USING BY REFERENCE  PI-UPPA0016-FUNKTION
                                            PI-DRPA0131
                                            PO-DRPA0131
                                            ERR-ERROR-BEREICH
                                            STANDARD-STATUS-BEREICH
           END-CALL
      
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
           END-IF
      
           EVALUATE TRUE
           WHEN PO-DRPA0131-RC-OK
      
                MOVE PO-DRPA0131-DATUM TO Z-TAGESDATUM-TDS-LANG
      
           WHEN OTHER
      *         DATUM IST FEHLERHAFT SPECE EINSTELEN
                MOVE SPACE TO Z-TAGESDATUM-TDS-LANG-JH
      
           END-EVALUATE
      
           .
       AAA-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       U10A-DATUM-PRUEF-2 SECTION.
       U10A-ANF.
      *------------------------------------------------------------*
      * IN   : Z-DATUM-JJ, TAGESDATUM
      * OUT  : RC
      * VERAR: ZEITRAUM ERMITTELN
      *-----------------------------------------------------------------
           MOVE 'U10A ' TO ERR-ORT-SEC
      
           SET PI-UPPA0016-DRPA0146 TO TRUE
      
           CALL K-UPPA0016 USING BY REFERENCE  PI-UPPA0016-FUNKTION
                                            PI-DRPA0146
                                            PO-DRPA0146
                                            ERR-ERROR-BEREICH
                                            STANDARD-STATUS-BEREICH
           END-CALL
      
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
           END-IF
      
           EVALUATE TRUE
           WHEN PO-DRPA0146-RC-OK
           WHEN PO-DRPA0146-RC-NOK
           WHEN PO-DRPA0146-RC-EDAT-NOK
                CONTINUE
           WHEN OTHER
      
                MOVE K-16            TO ERR-ORT-LFD
                PERFORM BR93-PROG-FEHLER
                PERFORM U01-ABBRUCH
      
           END-EVALUATE
           .
       AAA-EXIT.
           EXIT.
      
      *
      *------------------------------------------------------------*
       U11-ZWAT0008 SECTION.
       U11-ANF.
      *------------------------------------------------------------*
      * IN   :
      * OUT  :
      * VERAR: ERMITTELN KLARTEXT ZU KTY + KNR
      *-----------------------------------------------------------------
           MOVE 'U11  ' TO ERR-ORT-SEC
      *
           CALL K-ZWAT0008 USING BY REFERENCE PI-ZWAT0008-FUNKTION
                                              PI-DRAT0151
                                              PO-DRAT0151
                                              ERR-ERROR-BEREICH
                                              STANDARD-STATUS-BEREICH
           END-CALL
      
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
           END-IF
      
           EVALUATE TRUE
           WHEN PO-DRAT0151-RC-OK
           WHEN PO-DRAT0151-RC-NOT-FOUND
                CONTINUE
      
           WHEN OTHER
                MOVE K-17           TO ERR-ORT-LFD
                PERFORM BR93-PROG-FEHLER
                PERFORM U01-ABBRUCH
      
           END-EVALUATE
      
           .
       AAA-EXIT.
           EXIT.
      
      *------------------------------------------------------------*
       U12-UMLAUTE-GROSS SECTION.
       U12-ANF.
      *------------------------------------------------------------*
      * IN   : NAME, VORNAME, ORT ... GGF MIT Ä,Ö,Ü IN KLEINSCHRIFT
      * OUT  : NAME, VORNAME, ORT ... MIT Ä,Ö,Ü IN GROßSCHRIFT
      * VERAR: ERSETZEN ä,ö,ü mit Ä,Ö,Ü
      *        Achtung Keysensitiv !!!
      *------------------------------------------------------------*
           MOVE 'U12  ' TO ERR-ORT-SEC
      
           INSPECT Z-UMLAUTE-GROSS REPLACING ALL
                   'ä' BY 'Ä'
                   'ö' BY 'Ö'
                   'ü' BY 'Ü'
      
           .
       AAA-EXIT.
           EXIT.
      ***********************************************************
      *    ALLE SQL-STATEMENTS (AUSSER DECLARE CURSOR)          *
      ***********************************************************
      *----------------------------------------------------------
       SQL-ROLLBACK SECTION.
      *----------------------------------------------------------
CCC009*    EXEC SQL
CCC007*         ROLLBACK
CCC005*    END-EXEC
      *
           .
       SQL-ROLLBACK-EXIT.
           EXIT.
 
      *----------------------------------------------------------
       SQL-SEL-TAGESDATUM SECTION.
      *----------------------------------------------------------
CCC009*    EXEC SQL
CCC007*         SET :Z-TAGESDATUM-EUR  = CURRENT DATE
CCC005*    END-EXEC
      *
           .
       SQL-SEL-TAGESDATUM-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
      * BR91-SQL-FEHLER   --  SQL-FEHLER-BEHANDLUNG                *
      * INPUT : MUSS: K-PGM-NAME, SQLCODE, SQLERRMC, SQLERRML      *
      *         KANN: ERR-ORT-SEC, ERR-ORT-LFD, ERR-VAR-ZEILE01 -04*
      * BENÖTIGT: BEI DB2-ZUGRIFFEN                                *
      *------------------------------------------------------------*
       COPY DPAT0034.
      *------------------------------------------------------------*
      * BR93-PROG-FEHLER  --  PROGRAMM-/LOGIK-FEHLER BEHANDLUNG    *
      * INPUT : MUSS: K-PGM-NAME                                   *
      *         KANN: ERR-ORT-SEC, ERR-ORT-LFD, ERR-VAR-ZEILE01 -04*
      * BENÖTIGT: IMMER                                            *
      *------------------------------------------------------------*
       COPY DPAT0035.
      *
      *------------------------------------------------------------*
      * BR94-DATEI-FEHLER --  DATEI-FEHLER-BEHANDLUNG              *
      * INPUT : MUSS: S-DATEI-STATUS, K-PGM-NAME                   *
      *         KANN: ERR-ORT-SEC, ERR-ORT-LFD, ERR-VAR-ZEILE01 -04*
      * BENÖTIGT: BEI DATEI-ZUGRIFFEN                              *
      *------------------------------------------------------------*
       COPY DPAT0036.
 
      *------------------------------------------------------------*
      * BR99-SCHWERER-FEHLER-DISPLAY  --  FEHLER-AUFBEREITUNG      *
      * UR82-D-ERR-DISPLAY                UND -AUSGABE             *
      * INPUT   : ERR-ERROR-BEREICH                                *
      * BENÖTIGT: IMMER                                            *
      *------------------------------------------------------------*
       COPY DPAT0038.
 
      *------------------------------------------------------------*
      * UR81-ERR-AUFBER   --  FEHLER-AUFBEREITUNG                  *
      * INPUT   : ERR-ERROR-BEREICH                                *
      * BENÖTIGT: IMMER                                            *
      *------------------------------------------------------------*
       COPY DPAT0039.

