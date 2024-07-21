      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0052.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0052.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   14 Jan 1999 08:23:52   T13AE34
      *AKI00005 RESTART
      *
      *   Rev 1.0   31 Jul 1997 09:34:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPAT0052                                      *
      *    --------------------------------------------------------   *
      *    BATCHRAHMEN: RESTART                                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                        *
      *    DATUM:    11.03.1998                                       *
      *****************************************************************
      *    VERSION   1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00005!11.03.98!    ! NEU                       ! BRENNECKE*
      *         !        !    !                           !          *
      *         !08.09.98!    ! AUFRUF A0000015           ! WOLFF    *
      *         !        !    !                           !          *
      *         !10.09.98!    ! SPEICHERUNG DES RC AUS    ! WOLFF    *
      *         !        !    ! BR81-RESTARTPGM-ANMELDEN  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *------------------------------------------------------------*
       BR81-RESTARTPGM-ANMELDEN SECTION.
       BR81-ANF.
      *------------------------------------------------------
      *BR81-RESTART-ANMELDEN SECTION.
      *------------------------------------------------------
      *  INPUT       : K-PGM-NAME (IMPLIZIT BR-Z-JOBNAME)
      *  OUTPUT      : BR-Z-RC, BR-Z-COMMITHAEUFIGKEIT,
      *                BR-Z-COMMITPOINT-NR
      *  VERARBEITUNG: DAS PROGRAMM MELDET SICH FüER EINEN
      *                RESTART AN.
      *------------------------------------------------------
      *
           PERFORM BR811-VORLAUF
           SET PI-UPAT0022-DRAT0248-PGM-ANM TO TRUE
CCCPY1*    CALL K-UPAT0022 USING
CCC004*                  BY REFERENCE PI-UPAT0022
CCC004*                               PI-DRAT0248
CCC004*                               PO-DRAT0248
CCC004*                               ERR-ERROR-BEREICH
CCC004*                  BY CONTENT   STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
              PERFORM U01-ABBRUCH
           END-IF
           EVALUATE TRUE
           WHEN PO-DRAT0248-RC-OK
              CONTINUE
           WHEN PO-DRAT0248-RC-UNK-PGMABBRUCH
              CONTINUE
           WHEN PO-DRAT0248-RC-K-PGMABBRUCH
              CONTINUE
           WHEN OTHER
              MOVE 'UPAT0022/PO-DRAT0248 OUT-RETURNCODE UNGUELTIG'
                                        TO ERR-VAR-ZEILE01
              MOVE 'PO-AUSGABE  = '     TO ERR-VAR-ZEILE02
              MOVE  PO-DRAT0248-RC      TO ERR-VAR-ZEILE02(20:08)
              MOVE K-6                  TO ERR-ORT-LFD
              PERFORM BR93-PROG-FEHLER
              PERFORM U01-ABBRUCH
           END-EVALUATE
      *
           PERFORM BR812-NACHLAUF
           .
       BR81-EXIT.
           EXIT.
      *------------------------------------------------------
      *BR811-VORLAUF
      *------------------------------------------------------
      *  INPUT       : K-PGM-NAME, BR-Z-JOBNAME             -
      *  OUTPUT      :                                      -
      *  VERARBEITUNG: FELDUEBERGABE AN IN-PARAMTER         -
      *------------------------------------------------------
       BR811-VORLAUF SECTION.
080998*
080998     MOVE K-1 TO PI-DRAT0279-FUNKTION
080998*
CCCPY1*    CALL 'A0000015' USING BY REFERENCE PI-DRAT0279
CCC004*                                       PO-DRAT0279
CCCPY2*    END-CALL
080998*
080998     IF PO-DRAT0279-JOB-HV = HIGH-VALUE
080998        MOVE 'JOBNAME LESEN NICHT ERFOLGREICH'
080998                                  TO ERR-VAR-ZEILE01
080998        MOVE 'BR811'              TO ERR-ORT-SEC
080998        MOVE K-1                  TO ERR-ORT-LFD
080998        PERFORM BR93-PROG-FEHLER
080998        PERFORM U01-ABBRUCH
080998     END-IF
080998*
           MOVE K-PGM-NAME TO PI-DRAT0248-PROGRAMMNAME
080998     MOVE PO-DRAT0279-JOBNAME TO BR-Z-JOBNAME
080998                                 PI-DRAT0248-JOBNAME
           .
        BR811-EXIT.
           EXIT.
      *
      *------------------------------------------------------
      *BR812-NACHLAUF
      *------------------------------------------------------
      *  INPUT       : PO-DRAT0248
      *  OUTPUT      : BR-Z-FELDER
      *  VERARBEITUNG: OUT-PARAMETERUEBERGABE AN BR-Z-FELDER
      *------------------------------------------------------
       BR812-NACHLAUF SECTION.
           MOVE PO-DRAT0248-RC TO BR-Z-RC
100998                            BR81-Z-RC
           MOVE PO-DRAT0248-COMMITHAEUFIGKEIT
                               TO BR-Z-COMMITHAEUFIGKEIT

           IF BR-Z-RC-OK
              CONTINUE
           ELSE
              MOVE PO-DRAT0248-COMMITPOINT-NR
                               TO BR-Z-COMMITPOINT-NR
           END-IF
           .
       BR812-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       BR82-RESTARTPROGRAMM-ABMELDEN SECTION.
       BR82-ANF.
      *------------------------------------------------------
      *BR82-RESTARTPGM-ABMELDEN
      *------------------------------------------------------
      *  INPUT       : BR-Z-JOBNAME, K-PGM-NAME
      *  OUTPUT      : BR-Z-RC
      *  VERARBEITUNG: DIE RESTARTVERWALTUNG WIRD ABGEMELDET.
      *                SIE IST NICHT MEHR AKTIV.
      *------------------------------------------------------
      *
           PERFORM BR821-VORLAUF
      *
           SET PI-UPAT0022-DRAT0249-PGM-ABM TO TRUE
CCCPY1*    CALL K-UPAT0022         USING BY REFERENCE
CCC004*                               PI-UPAT0022
CCC004*                               PI-DRAT0249
CCC004*                               PO-DRAT0249
CCC004*                               ERR-ERROR-BEREICH
CCC004*                               STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
              PERFORM U01-ABBRUCH
           END-IF
           EVALUATE TRUE
           WHEN PO-DRAT0249-RC-OK
              CONTINUE
           WHEN OTHER
              MOVE 'UPAT0022/PO-DRAT0249 OUT-RETURNCODE UNGUELTIG'
                                        TO ERR-VAR-ZEILE01
              MOVE 'PO-AUSGABE  = '     TO ERR-VAR-ZEILE02
              MOVE  PO-DRAT0249-RC      TO ERR-VAR-ZEILE02(20:08)
              MOVE K-6                  TO ERR-ORT-LFD
              PERFORM BR93-PROG-FEHLER
              PERFORM U01-ABBRUCH
           END-EVALUATE
           PERFORM BR822-NACHLAUF
           .
       BR82-EXIT.
           EXIT.
      *------------------------------------------------------
      *BR821-VORLAUF
      *------------------------------------------------------
      *  INPUT       : K-PGM-NAME, Z-BR-JOBNAME             -
      *  OUTPUT      : PI-DRAT0249                          -
      *  VERARBEITUNG: FELDUEBERGABE AN IN-PARAMTER         -
      *------------------------------------------------------
       BR821-VORLAUF SECTION.
           MOVE K-PGM-NAME       TO PI-DRAT0249-PROGRAMMNAME
           MOVE BR-Z-JOBNAME     TO PI-DRAT0249-JOBNAME
           .
       BR821-EXIT.
           EXIT.
      *
      *------------------------------------------------------
      *BR822-NACHLAUF
      *------------------------------------------------------
      *  INPUT       : PO-DRAT0249
      *  OUTPUT      : BR-Z-FELDER
      *  VERARBEITUNG: OUT-PARAMETERUEBERGABE AN BR-Z-FELDER
      *------------------------------------------------------
       BR822-NACHLAUF SECTION.
           MOVE PO-DRAT0249-RC          TO BR-Z-RC
           .
       BR822-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       BR83-VERARBEITUNGSBLOCK-ABMELD SECTION.
       BR83-ANF.
      *------------------------------------------------------
      *BR83-VERARBEITUNGSBLOCK ABMELDEN
      *------------------------------------------------------
      *  INPUT       : BR-Z-JOBNAME, K-PGM-NAME
      *  OUTPUT      : BR-Z-RC,
      *                BR-Z-COMMITPOINT-NR,
      *                BR-Z-PROGRAMM-BEENDEN,
      *                BR-Z-AKT-COMMITHAEUFIGKEIT
      *                BR-Z-RC
      *  VERARBEITUNG: DER RESTARTVERARBEITUNGSBLOCK WIRD 'COMMITTED'.
      *                DIE COMMITPOINT-NR WIRD HOCHGEZAEHLT.
      *                DIE RESTARTINFORMATIONEN WERDEN
      *                GELOESCHT.
      *                DIE RESTARTAKTION: 'RESTARTINFORMATIONEN
      *                SCHREIBEN' SOLLTE UNMITTELBAR FOLGEN.
      *------------------------------------------------------
      *
           PERFORM BR831-VORLAUF
      *
           SET PI-UPAT0022-DRAT0250-VER-ABM TO TRUE
CCCPY1*    CALL K-UPAT0022         USING BY REFERENCE
CCC004*                               PI-UPAT0022
CCC004*                               PI-DRAT0250
CCC004*                               PO-DRAT0250
CCC004*                               ERR-ERROR-BEREICH
CCC004*                               STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
              PERFORM U01-ABBRUCH
           END-IF
           EVALUATE TRUE
           WHEN PO-DRAT0250-RC-OK
              CONTINUE
           WHEN OTHER
              MOVE 'UPAT0022/PO-DRAT0250 OUT-RETURNCODE UNGUELTIG'
                                        TO ERR-VAR-ZEILE01
              MOVE 'PO-AUSGABE  = '     TO ERR-VAR-ZEILE02
              MOVE  PO-DRAT0250-RC      TO ERR-VAR-ZEILE02(20:08)
              MOVE K-6                  TO ERR-ORT-LFD
              PERFORM BR93-PROG-FEHLER
              PERFORM U01-ABBRUCH
           END-EVALUATE
           PERFORM BR832-NACHLAUF
           .
       BR83-EXIT.
           EXIT.
      *------------------------------------------------------
      *BR831-VORLAUF
      *------------------------------------------------------
      *  INPUT       : K-PGM-NAME, BR-Z-JOBNAME             -
      *  OUTPUT      : PI-DRAT0250                          -
      *  VERARBEITUNG: FELDUEBERGABE AN IN-PARAMTER         -
      *------------------------------------------------------
       BR831-VORLAUF SECTION.
           MOVE K-PGM-NAME       TO PI-DRAT0250-PROGRAMMNAME
           MOVE BR-Z-JOBNAME     TO PI-DRAT0250-JOBNAME
           .
       BR831-EXIT.
           EXIT.
      *
      *------------------------------------------------------
      *BR832-NACHLAUF
      *------------------------------------------------------
      *  INPUT       : PO-DRAT0250
      *  OUTPUT      : BR-Z-FELDER
      *  VERARBEITUNG: OUT-PARAMETERUEBERGABE AN BR-Z-FELDER
      *------------------------------------------------------
       BR832-NACHLAUF SECTION.
           MOVE PO-DRAT0250-RC        TO BR-Z-RC
           MOVE PO-DRAT0250-COMMITPOINT-NR
                                      TO BR-Z-COMMITPOINT-NR
           MOVE PO-DRAT0250-PROGRAMM-BEENDEN
                                      TO BR-Z-PROGRAMM-BEENDEN
      *
      ***  AKTUALISIERUNG DER AKTUELLEN COMMITHAEUFIGKEIT
           IF PO-DRAT0250-COMMITHAEUFIGKEIT > 0
              MOVE PO-DRAT0250-COMMITHAEUFIGKEIT
                                      TO BR-Z-COMMITHAEUFIGKEIT
           END-IF
           .
       BR832-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       BR84-RESTARTINFO-LESEN SECTION.
       BR84-ANF.
      *------------------------------------------------------
      *BR84-RESTARTINFORMATIONEN LESEN
      *------------------------------------------------------
      *  INPUT       : BR-Z-JOBNAME, K-PGM-NAME, BR-Z-LAUFENDE-NR
      *  OUTPUT      : BR-Z-RC,
      *                BR-Z-RESTARTINFO,
      *                BR-Z-LAUFENDE-NR
      *  VERARBEITUNG: RESTARTINFORMATIONEN WERDEN GELESEN
      *------------------------------------------------------
      *
           PERFORM BR841-VORLAUF
      *
           SET PI-UPAT0022-DRAT0251-RSTI-L TO TRUE
CCCPY1*    CALL K-UPAT0022         USING BY REFERENCE
CCC004*                               PI-UPAT0022
CCC004*                               PI-DRAT0251
CCC004*                               PO-DRAT0251
CCC004*                               ERR-ERROR-BEREICH
CCC004*                               STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
              PERFORM U01-ABBRUCH
           END-IF
           EVALUATE TRUE
           WHEN PO-DRAT0251-RC-OK
              CONTINUE
           WHEN OTHER
              MOVE 'UPAT0022/PO-DRAT0251 OUT-RETURNCODE UNGUELTIG'
                                        TO ERR-VAR-ZEILE01
              MOVE 'PO-AUSGABE  = '     TO ERR-VAR-ZEILE02
              MOVE  PO-DRAT0251-RC      TO ERR-VAR-ZEILE02(20:08)
              MOVE K-5                  TO ERR-ORT-LFD
              PERFORM BR93-PROG-FEHLER
              PERFORM U01-ABBRUCH
           END-EVALUATE
           PERFORM BR842-NACHLAUF
           .
       BR84-EXIT.
           EXIT.
      *------------------------------------------------------
      *BR841-VORLAUF
      *------------------------------------------------------
      *  INPUT       : K-PGM-NAME, Z-BR-JOBNAME             -
      *  OUTPUT      : PI-DRAT0251                          -
      *  VERARBEITUNG: FELDUEBERGABE AN IN-PARAMTER
      *------------------------------------------------------
       BR841-VORLAUF SECTION.
           MOVE BR-Z-LAUFENDE-NR TO PI-DRAT0251-LAUFENDE-NR
           MOVE K-PGM-NAME       TO PI-DRAT0251-PROGRAMMNAME
           MOVE BR-Z-JOBNAME     TO PI-DRAT0251-JOBNAME
           .
       BR841-EXIT.
           EXIT.
      *
      *------------------------------------------------------
      *BR842-NACHLAUF
      *------------------------------------------------------
      *  INPUT       : PO-DRAT0251
      *  OUTPUT      : BR-Z-FELDER
      *  VERARBEITUNG: OUT-PARAMETERUEBERGABE AN BR-Z-FELDER
      *------------------------------------------------------
       BR842-NACHLAUF SECTION.
           MOVE PO-DRAT0251-LAUFENDE-NR TO BR-Z-LAUFENDE-NR
           MOVE PO-DRAT0251-RC          TO BR-Z-RC
           MOVE PO-DRAT0251-RESTARTINFO TO BR-Z-RESTARTINFO
           .
       BR842-EXIT.
           EXIT.
      *
      *------------------------------------------------------------*
       BR85-RESTARTINFO-SCHREIBEN SECTION.
       BR85-ANF.
      *------------------------------------------------------
      *BR85-RESTARTINFORMATIONEN SCHREIBEN
      *------------------------------------------------------
      *  INPUT       : BR-Z-JOBNAME, K-PGM-NAME, BR-Z-LAUFENDE-NR
      *  OUTPUT      : BR-Z-RC,
      *                BR-Z-LAUFENDE-NR,
      *                BR-Z-RESTARTINFO
      *  VERARBEITUNG: RESTARTINFORMATIONEN WERDEN GE-
      *                SCHRIEBEN
      *------------------------------------------------------

      *
           PERFORM BR851-VORLAUF
      *
           SET PI-UPAT0022-DRAT0252-RSTI-S TO TRUE
CCCPY1*    CALL K-UPAT0022         USING BY REFERENCE
CCC004*                               PI-UPAT0022
CCC004*                               PI-DRAT0252
CCC004*                               PO-DRAT0252
CCC004*                               ERR-ERROR-BEREICH
CCC004*                               STANDARD-STATUS-BEREICH
CCCPY2*    END-CALL
           IF ERR-RC-SCHWERER-FEHLER
              PERFORM BR99-SCHWERER-FEHLER-DISPLAY
              PERFORM U01-ABBRUCH
           END-IF
           EVALUATE TRUE
           WHEN PO-DRAT0252-RC-OK
              CONTINUE
           WHEN OTHER
              MOVE 'UPAT0022/PO-DRAT0252 OUT-RETURNCODE UNGUELTIG'
                                        TO ERR-VAR-ZEILE01
              MOVE 'PO-AUSGABE  = '     TO ERR-VAR-ZEILE02
              MOVE  PO-DRAT0252-RC      TO ERR-VAR-ZEILE02(20:08)
              MOVE K-7                  TO ERR-ORT-LFD
              PERFORM BR93-PROG-FEHLER
              PERFORM U01-ABBRUCH
           END-EVALUATE
           PERFORM BR852-NACHLAUF
           .
       BR85-EXIT.
           EXIT.
      *------------------------------------------------------
      *BR851-VORLAUF
      *------------------------------------------------------
      *  INPUT       : K-PGM-NAME, Z-BR-JOBNAME             -
      *  OUTPUT      : PI-DRAT0252                          -
      *  VERARBEITUNG: FELDUEBERGABE AN IN-PARAMTER         -
      *------------------------------------------------------
       BR851-VORLAUF SECTION.
           MOVE K-PGM-NAME       TO PI-DRAT0252-PROGRAMMNAME
           MOVE BR-Z-JOBNAME     TO PI-DRAT0252-JOBNAME
           MOVE BR-Z-LAUFENDE-NR TO PI-DRAT0252-LAUFENDE-NR
           MOVE BR-Z-RESTARTINFO TO PI-DRAT0252-RESTARTINFO
           .
       BR851-EXIT.
           EXIT.
      *
      *------------------------------------------------------
      *BR852-NACHLAUF
      *------------------------------------------------------
      *  INPUT       : PO-DRAT0252
      *  OUTPUT      : BR-Z-FELDER
      *  VERARBEITUNG: OUT-PARAMETERUEBERGABE AN BR-Z-FELDER
      *------------------------------------------------------
       BR852-NACHLAUF SECTION.
           MOVE PO-DRAT0252-RC          TO BR-Z-RC
           .
       BR852-EXIT.
           EXIT.
      *
      ******** ENDE COPY-MEMBER Restart-verwaltung (DpAT0052) ********

