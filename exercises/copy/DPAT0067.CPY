      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0067.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DPAT0067.CPv  $
      *
      *   Rev 2.5   Jun 23 2020 15:20:20   t13ac20
      *AAT00055: neue Fehlervariablen Z-FEHLER-INHALT zur Initialis
      *ierung
      *
      *   Rev 2.4   May 15 2017 11:07:18   t13ac20
      *AAT00055: Anpassungen Fehlerverfahren
      *
      *   Rev 2.3   May 11 2017 10:38:18   t13ac20
      *AAT00055: Fehlerbehebung DPAT0067
      *
      *   Rev 2.2   Mar 01 2017 17:26:22   t13ex27
      *AAT00055: Überarbeitung Fehlerverfahren-Wien
      *
      *   Rev 2.1   Jan 25 2017 10:11:36   t13ex27
      *AAT00055: Überarbeitung Fehlerverfahren
      *
      *   Rev 2.0   Feb 27 1999 11:57:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   11 Mar 1998 17:03:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER: DPAT0067
      *    --------------------------------------------------------
      *    Procedure-Copy für Fehlerverfahren
      *    --------------------------------------------------------
      ******************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * AAT00055!03.01.17!2.1 ! neu                       ! IFR
      *         !24.02.17!2.2 ! Ergänzung Reaktionen      ! IFR
110517* AAT00055!11.05.17!2.3 ! Abfrage bei Schreiben     ! SWO
      *         !        !    ! Fehlernummer 9999 in      !
      *         !        !    ! FSTATIS geändert          !
120517* AAT00055!12.05.17!2.4 ! Abbrüche setzen nach      ! SWO
      *         !        !    ! PO-DRAT0340-RC-001 und    !
      *         !        !    ! PO-DRAT0340-RC-003;       !
      *         !        !    ! PO-DRAT0340-RC-004 und    !
      *         !        !    ! PO-DRAT0340-RC-005        !
      *         !        !    ! entfernt;                 !
      *         !        !    ! Reaktion                  !
      *         !        !    ! K-KEIN-ABBRUCHFEHLER      !
      *         !        !    ! entfernt;                 !
      *         !        !    ! Reaktion                  !
      *         !        !    ! K-PROGRAMM-ABBRECHEN-SOFORT
      *         !        !    ! hinzugefügt               !
      * AAT00055!23.06.20!2.5 ! NEUE FELDER               ! T13AC20
      *         !        !    ! Z-FEHLER-INHALT-xxx       !
      *         !        !    ! INITIALISIERT             !
      *         !        !    !                           !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      ******************************************************************
      *
      *    Dieses Copy enthält die Anweisungen zur Durchführung
      *    der Fehlerbearbeitung und zur Ausgabe der Fehlerstatistik.
      *
      *    Voraussetzung: Copy DRAT0612
      *                        DRAT0340
      *                        DRAT0538
      *
      *    INPUT N00-... : Z-FS-FEHLER-ANZAHL
      *
      *    REPLACING : KX-GES
      *                KX-SYSTEMCODE
      *                UX-SCHREIBEN-FEHLERDATEI
      *
      ******************************************************************
 
      *-----------------------------------------------------------------
       N00-WRITE-F-STAT SECTION.
      *-----------------------------------------------------------------
      *
           MOVE 'N00'                             TO ERR-ORT-SEC
 
           DISPLAY ' '
           DISPLAY 'BEGINN FEHLERSTATISTIK        '
           DISPLAY 'F-NR FELD            REAKTION RELEVANZ ANZAHL '
 
           PERFORM VARYING Z-INDEX FROM 1 BY 1
                     UNTIL Z-INDEX > TB-FEHLER-MAX
 
             IF TB-FEHLER-NUMMER (Z-INDEX) > ZEROS
 
                MOVE TB-FEHLER-NUMMER   (Z-INDEX) TO FS-FEHLER-NUMMER
                MOVE TB-FEHLER-FELDNAME (Z-INDEX) TO FS-FEHLER-FELDNAME
                MOVE TB-FEHLER-ANZAHL   (Z-INDEX) TO FS-FEHLER-ANZAHL
                MOVE TB-JOB-NAME        (Z-INDEX) TO FS-FEHLER-JOB-NAME
                MOVE TB-FEHLER-REAKTION (Z-INDEX) TO FS-FEHLER-REAKTION
                MOVE TB-FEHLER-RELEVANZ (Z-INDEX) TO FS-FEHLER-RELEVANZ
                MOVE K-PGM-NAME                   TO FS-FEHLER-PGM
                MOVE 'KX-GES'                     TO FS-FEHLER-GES
                MOVE 'KX-SYSTEMCODE'              TO FS-FEHLER-SYSCODE
 
                DISPLAY FS-FEHLER-NUMMER   ' '
                        FS-FEHLER-FELDNAME ' '
                        FS-FEHLER-REAKTION '        '
                        FS-FEHLER-RELEVANZ '         '
                        FS-FEHLER-ANZAHL
 
CCCCPY*         WRITE FEHL-STATISTIK-SATZ
 
                EVALUATE FSTATIS-FILESTATUS
                   WHEN KF-OK
                      ADD 1 TO D-ANZ-FEHL-STAT
 
                   WHEN OTHER
                      MOVE 'FEHLER SCHREIBEN FEHLERSTATISTIK SÄTZE '
                                                    TO ERR-VAR-ZEILE01
                      MOVE 'FEHLERNUMMER/FELDNAME ' TO ERR-VAR-ZEILE02
                      MOVE TB-FEHLER-NUMMER (Z-INDEX)
                                                    TO ERR-VAR-ZEILE03
                      MOVE TB-FEHLER-FELDNAME (Z-INDEX)
                                                    TO ERR-VAR-ZEILE04
                      MOVE 1                        TO ERR-ORT-LFD
                      MOVE FSTATIS-FILESTATUS       TO S-DATEI-STATUS
                      PERFORM BR94-DATEI-FEHLER
                      PERFORM U01-ABBRUCH
 
                END-EVALUATE
 
             END-IF
 
           END-PERFORM
 
110517*    IF TB-FEHLER-MAX > ZERO
110517     IF D-ANZ-FEHL-STAT > ZERO
 
              MOVE 9999                         TO FS-FEHLER-NUMMER
              MOVE SPACES                       TO FS-FEHLER-FELDNAME
              MOVE Z-FS-FEHLER-ANZAHL           TO FS-FEHLER-ANZAHL
              MOVE TB-JOB-NAME (1)              TO FS-FEHLER-JOB-NAME
              MOVE SPACE                        TO FS-FEHLER-REAKTION
              MOVE SPACE                        TO FS-FEHLER-RELEVANZ
              MOVE K-PGM-NAME                   TO FS-FEHLER-PGM
              MOVE 'KX-GES'                     TO FS-FEHLER-GES
              MOVE 'KX-SYSTEMCODE'              TO FS-FEHLER-SYSCODE
 
CCCCPY*       WRITE FEHL-STATISTIK-SATZ
 
              EVALUATE FSTATIS-FILESTATUS
                 WHEN KF-OK
                    CONTINUE
 
                 WHEN OTHER
                    MOVE 'FEHLER SCHREIBEN FEHLERSTATISTIK SÄTZE '
                                                  TO ERR-VAR-ZEILE01
                    MOVE 'FEHLERNUMMER/FELDNAME ' TO ERR-VAR-ZEILE02
                    MOVE TB-FEHLER-NUMMER (Z-INDEX)
                                                  TO ERR-VAR-ZEILE03
                    MOVE TB-FEHLER-FELDNAME (Z-INDEX)
                                                  TO ERR-VAR-ZEILE04
                    MOVE 2                        TO ERR-ORT-LFD
                    MOVE FSTATIS-FILESTATUS       TO S-DATEI-STATUS
                    PERFORM BR94-DATEI-FEHLER
                    PERFORM U01-ABBRUCH
 
              END-EVALUATE
 
           END-IF
 
           DISPLAY 'ENDE  FEHLERSTATISTIK'
           DISPLAY ' '
           .
       N00-EXIT.
           EXIT.
 
      *----------------------------------------------------------------
       U21-FEHLER-HANDLING SECTION.
      *----------------------------------------------------------------
      *
      * INPUT:
      *   Z-Fehler-Nummer     - Fehlernummer mit der ein Satz in der
      *                         Tabelle xxxxxx vorhanden sein muss
      *   Z-Fehler-Feldname   - Feldname desjenigen Feldes, dessen
      *                         Inhalt fehlerhaft ist
      *   Z-Fehler-Vertrag    - Vertragsnummer, bei der der Fehler
      *                         erkannt wurde
      *   Z-Fehler-Var-1      - Variablen, die in dem entsprechenden
      *   Z-Fehler-Var-2        Satz der Tabelle xxxxx (siehe
      *   Z-Fehler-Var-3        Z-Fehler-Nummer) enthalten sind.
      *   Z-Fehler-Var-4        Die Anzahl der in der Tab. definierten
      *   Z-Fehler-Var-5        und die Anz. der übergebenen Variablen
      *                         muss übereinstimmen
      *   Z-Fehlerfeld-Inhalt - Inhalt des als fehlerhaft
      *                         erkannten Feldes
      *
      *
      * OUTPUT:
      *   Z-Fehlerfeld-Inhalt - Rückgabe des als fehlerhaft
      *                         erkannten Feldes
      *                         Dieses Feld wird je nach ermittelter
      *                         Fehlerreaktion gegebenenfalls
      *                         verändert und muss wieder ins
      *                         Originalfeld zurückübertragen werden.
      *
      *
      * Konstanten, die in diesem UPRO verwendet werden:
      *   K-Systemcode        - Entwicklungsbereich (z.B. KI)
      *   K-PGM-Name          - Programmname (PMKI0475)
      *   K-GES               - Gesellschaftsnummer (ADV = 81)
      *
      *-----------------------------------------------------------------
      *
           IF Z-FEHLER-NUMMER   = ZEROS    OR
              Z-FEHLER-FELDNAME = SPACES
 
              DISPLAY
                  'FEHLER-NUMMER ODER FEHLER-FELDNAME NICHT GEFÜLLT '
              DISPLAY 'Z-FEHLER-NUMMER   : ' Z-FEHLER-NUMMER
              DISPLAY 'Z-FEHLER-FELDNAME : ' Z-FEHLER-FELDNAME
              SET S-VOR-PROGRAMMENDE-ABBR-J    TO TRUE
 
           END-IF
 
           MOVE K-PGM-NAME         TO PI-DRAT0340-RUF-PROGRAMM
           MOVE 'KX-GES'           TO PI-DRAT0340-GES
           MOVE 'KX-SYSTEMCODE'    TO PI-DRAT0340-SYSTEMCODE
 
           MOVE Z-FEHLER-NUMMER    TO PI-DRAT0340-FEHLERNUMMER
           MOVE Z-FEHLER-FELDNAME  TO PI-DRAT0340-FELDNAME
           MOVE Z-FEHLER-VERTRAG   TO PI-DRAT0340-VERTRAG
           MOVE Z-FEHLER-VAR-1     TO PI-DRAT0340-VAR-1
           MOVE Z-FEHLER-VAR-2     TO PI-DRAT0340-VAR-2
           MOVE Z-FEHLER-VAR-3     TO PI-DRAT0340-VAR-3
           MOVE Z-FEHLER-VAR-4     TO PI-DRAT0340-VAR-4
           MOVE Z-FEHLER-VAR-5     TO PI-DRAT0340-VAR-5
 
           SET PI-UPAT0030-DRAT0340 TO TRUE
 
CCCPY1*    CALL K-UPAT0030 USING BY REFERENCE PI-UPAT0030
CCC004*                                       PI-DRAT0340
CCC004*                                       PO-DRAT0340
CCC004*                                       ERR-ERROR-BEREICH
CCC004*                                       STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
 
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
              PERFORM U01-ABBRUCH
           END-IF
 
           EVALUATE TRUE
               WHEN PO-DRAT0340-RC-OK
                   CONTINUE
               WHEN PO-DRAT0340-RC-NFD
                   DISPLAY 'FEHLERMELDUNG EXISTIERT NICHT'
                   PERFORM U212-FEHLERVARIABLEN-DISPLAY
 
                   SET S-VOR-PROGRAMMENDE-ABBR-J    TO TRUE
 
               WHEN PO-DRAT0340-RC-PAR
                   DISPLAY 'PARAMETER FALSCH'
                   PERFORM U212-FEHLERVARIABLEN-DISPLAY
 
                   SET S-VOR-PROGRAMMENDE-ABBR-J    TO TRUE
 
               WHEN PO-DRAT0340-RC-LEN
                   DISPLAY 'FEHLERMELDUNG ZU LANG'
                   PERFORM U212-FEHLERVARIABLEN-DISPLAY
 
               WHEN PO-DRAT0340-RC-001
                   DISPLAY 'FEHLERKOMBINATION '
                   DISPLAY 'RC = OK   - OHNE GEFUNDENER FEHLERREAKTION'
                   PERFORM U212-FEHLERVARIABLEN-DISPLAY
120517
120517             SET S-VOR-PROGRAMMENDE-ABBR-J    TO TRUE
 
               WHEN PO-DRAT0340-RC-002
                   DISPLAY 'FEHLERKOMBINATION '
                   DISPLAY 'RC = PAR  - MIT  GEFUNDENER FEHLERREAKTION'
                   PERFORM U212-FEHLERVARIABLEN-DISPLAY
 
               WHEN PO-DRAT0340-RC-003
                   DISPLAY 'FEHLERKOMBINATION '
                   DISPLAY 'RC = PAR  - OHNE GEFUNDENER FEHLERREAKTION'
                   PERFORM U212-FEHLERVARIABLEN-DISPLAY
120517
120517             SET S-VOR-PROGRAMMENDE-ABBR-J    TO TRUE
 
120517*        WHEN PO-DRAT0340-RC-004
>>    *            DISPLAY 'FEHLERKOMBINATION '
      *            DISPLAY 'RC = LEN  - MIT  GEFUNDENER FEHLERREAKTION'
      *            PERFORM U212-FEHLERVARIABLEN-DISPLAY
      *
      *        WHEN PO-DRAT0340-RC-005
      *            DISPLAY 'FEHLERKOMBINATION '
      *            DISPLAY 'RC = LEN  - OHNE GEFUNDENER FEHLERREAKTION'
    <<*            PERFORM U212-FEHLERVARIABLEN-DISPLAY
120517*
               WHEN PO-DRAT0340-RC-006
                   DISPLAY 'FEHLERKOMBINATION '
                   DISPLAY 'RC = PAR + LEN                            '
                   DISPLAY '     MIT  GEFUNDENER FEHLERREAKTION       '
                   PERFORM U212-FEHLERVARIABLEN-DISPLAY
 
               WHEN PO-DRAT0340-RC-007
                   DISPLAY 'FEHLERKOMBINATION '
                   DISPLAY 'RC = PAR + LEN                            '
                   DISPLAY '     OHNE GEFUNDENER FEHLERREAKTION       '
                   PERFORM U212-FEHLERVARIABLEN-DISPLAY
 
               WHEN OTHER
                   DISPLAY 'UNBEKANNTER RC  '
                   DISPLAY 'PO-DRKI0307-RC : ' PO-DRAT0340-RC
                   PERFORM U212-FEHLERVARIABLEN-DISPLAY
                   PERFORM U01-ABBRUCH
 
           END-EVALUATE
 
      * FEHLERHAFTES FELD JE NACH FEHLERREAKTION BEARBEITEN
 
           EVALUATE PO-DRAT0340-FEHLERREAKTION
               WHEN K-KEINE-REAKTION
                   CONTINUE
 
               WHEN K-INITIALISIEREN
                   INITIALIZE           Z-FEHLER-INHALT-X
                                        Z-FEHLER-INHALT-9
                                        Z-FEHLER-INHALT-P
                                        Z-FEHLER-INHALT-9V9
                                        Z-FEHLER-INHALT-9V99
                                        Z-FEHLER-INHALT-9V999
                                        Z-FEHLER-INHALT-S9
                                        Z-FEHLER-INHALT-S9V9
                                        Z-FEHLER-INHALT-S9V99
                                        Z-FEHLER-INHALT-S9V999
                                        Z-FEHLER-INHALT-SP
                                        Z-FEHLER-INHALT-SPV9
                                        Z-FEHLER-INHALT-SPV99
                                        Z-FEHLER-INHALT-SPV999
                   MOVE '01.01.0001' TO Z-FEHLER-INHALT-D-SQL
                   MOVE '0001-01-01' TO Z-FEHLER-INHALT-D-ISO
 
               WHEN K-SATZ-WEITERVERARBEITEN-N
                   SET S-SATZ-WEITERVERARBEITEN-N      TO TRUE
 
               WHEN K-VERTRAG-WEITERVERARBEITEN-N
                   SET S-SATZ-WEITERVERARBEITEN-N      TO TRUE
                   SET S-VERTRAG-WEITERVERARBEITEN-N   TO TRUE
 
               WHEN K-PROGRAMM-ABBRECHEN
                   DISPLAY 'FEHLERREAKTION --> ABBRUCH'
                   PERFORM U212-FEHLERVARIABLEN-DISPLAY
 
                   SET S-VOR-PROGRAMMENDE-ABBR-J    TO TRUE
120517
>>             WHEN K-PROGRAMM-ABBRECHEN-SOFORT
                   DISPLAY 'FEHLERREAKTION --> SOFORTIGER ABBRUCH'
                   PERFORM U212-FEHLERVARIABLEN-DISPLAY
    <<
120517             PERFORM U01-ABBRUCH
 
240217*        WHEN K-KEIN-ABBRUCHFEHLER
240217*            CONTINUE
 
               WHEN OTHER
120517*            CONTINUE
>>                 DISPLAY 'FEHLERREAKTION --> UNBEKANNT >'
                            PO-DRAT0340-FEHLERREAKTION
                           '<, SOFORTIGER ABBRUCH'
                   PERFORM U212-FEHLERVARIABLEN-DISPLAY
    <<
120517             PERFORM U01-ABBRUCH
 
           END-EVALUATE
 
      * ACHTUNG ! ! ! Z-FEHLER-VERTRAG DARF HIER NICHT GELÖSCHT WERDEN
      * DA DIESES FELD NOCH BENÖTIGT WIRD, WENN EIN VERTRAG
      * NICHT WEITERBEARBEITET WERDEN SOLL ! ! !
 
           MOVE SPACES             TO Z-FEHLER-VAR-1
           MOVE SPACES             TO Z-FEHLER-VAR-2
           MOVE SPACES             TO Z-FEHLER-VAR-3
           MOVE SPACES             TO Z-FEHLER-VAR-4
           MOVE SPACES             TO Z-FEHLER-VAR-5
 
      *----------------------------------------------------------------
      * TABELLE FÜR FEHLERSTATISTIK BEARBEITEN
      *----------------------------------------------------------------
 
           SET S-ELEMENT-GEFUNDEN-N       TO TRUE
 
           PERFORM WITH TEST AFTER
                   VARYING Z-INDEX FROM 1 BY 1
                     UNTIL Z-INDEX = TB-FEHLER-MAX
                        OR S-ELEMENT-GEFUNDEN-J
                IF TB-FEHLER-IDX       (Z-INDEX) = Z-FEHLER-SCHL OR
                   (TB-FEHLER-NUMMER   (Z-INDEX) = ZEROS AND
                    TB-FEHLER-FELDNAME (Z-INDEX) = SPACE)
                   SET S-ELEMENT-GEFUNDEN-J       TO TRUE
                END-IF
           END-PERFORM
 
           IF S-ELEMENT-GEFUNDEN-N
              DISPLAY 'FEHLER BEI TABELLE FÜR FEHLERSTATISTIK'
              PERFORM U211-FEHLER-IN-FTAB
           END-IF
 
 
           IF TB-FEHLER-NUMMER   (Z-INDEX) = ZEROS AND
              TB-FEHLER-FELDNAME (Z-INDEX) = SPACE
               MOVE Z-FEHLER-NUMMER   TO TB-FEHLER-NUMMER   (Z-INDEX)
               MOVE Z-FEHLER-FELDNAME TO TB-FEHLER-FELDNAME (Z-INDEX)
               MOVE PO-DRAT0340-JOBNAME
                                      TO TB-JOB-NAME        (Z-INDEX)
               MOVE PO-DRAT0340-FEHLERREAKTION
                                      TO TB-FEHLER-REAKTION (Z-INDEX)
               MOVE PO-DRAT0340-FEHLERRELEVANZ
                                      TO TB-FEHLER-RELEVANZ (Z-INDEX)
               ADD K-1                TO TB-FEHLER-ANZAHL   (Z-INDEX)
           ELSE
               ADD K-1                TO TB-FEHLER-ANZAHL   (Z-INDEX)
           END-IF
 
           PERFORM 'UX-SCHREIBEN-FEHLERDATEI'
           .
       U21-EXIT.
           EXIT.
 
      *----------------------------------------------------------------
       U211-FEHLER-IN-FTAB SECTION.
      *----------------------------------------------------------------
 
           DISPLAY 'AUFGETRETEN BEI FEHLERNUMMER ' Z-FEHLER-NUMMER
           DISPLAY '                FELDNAME     ' Z-FEHLER-FELDNAME
           DISPLAY 'INHALT FEHLERTABELLE         '
 
 
           PERFORM
                   VARYING Z-INDEX FROM 1 BY 1
                     UNTIL Z-INDEX > TB-FEHLER-MAX
                DISPLAY 'INDEX        ' Z-INDEX
                DISPLAY 'FEHLERNUMMER ' TB-FEHLER-NUMMER   (Z-INDEX)
                DISPLAY 'FELDNAME     ' TB-FEHLER-FELDNAME (Z-INDEX)
                DISPLAY 'RELEVANZ     ' TB-FEHLER-RELEVANZ (Z-INDEX)
                DISPLAY 'REAKTION     ' TB-FEHLER-REAKTION (Z-INDEX)
                DISPLAY 'ANZAHL       ' TB-FEHLER-ANZAHL   (Z-INDEX)
           END-PERFORM
 
           MOVE 'TABELLE FEHLERSTATISTIK ZU KLEIN' TO ERR-VAR-ZEILE01
           MOVE K-20                               TO ERR-ORT-LFD
           MOVE 'U211'                             TO ERR-ORT-SEC
           PERFORM U01-ABBRUCH
           .
       U211-EXIT.
           EXIT.
 
      *----------------------------------------------------------------
       U212-FEHLERVARIABLEN-DISPLAY SECTION.
      *----------------------------------------------------------------
 
           DISPLAY 'FEHLERNUMMER   : ' Z-FEHLER-NUMMER
           DISPLAY 'FELDNAME       : ' Z-FEHLER-FELDNAME
           DISPLAY 'VERTRAG        : ' Z-FEHLER-VERTRAG
           DISPLAY 'VARIABLE 1     : ' Z-FEHLER-VAR-1
           DISPLAY 'VARIABLE 2     : ' Z-FEHLER-VAR-2
           DISPLAY 'VARIABLE 3     : ' Z-FEHLER-VAR-3
           DISPLAY 'VARIABLE 4     : ' Z-FEHLER-VAR-4
           DISPLAY 'VARIABLE 5     : ' Z-FEHLER-VAR-5
           DISPLAY '---------------------------------------'
           .
       U212-EXIT.
           EXIT.
 
 
 
 


