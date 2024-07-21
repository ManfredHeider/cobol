      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0040.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0040.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 20 1997 16:56:20   T13EX16
      *DRIVER-ANPASSUNG DAISY
      *
      *   Rev 1.0   Nov 04 1995 08:18:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00001000
      *    COPY-MEMBER: DPAT0040                                      * 00002000
      *    --------------------------------------------------------   * 00003000
      *    FEHLERBEHANDLUNG: SCHWERER-FEHLER-BEHANDLUNG               * 00004100
      *                      FÜR DIALOGE OHNE RAHMENSTEUERUNG         * 00004200
      *    --------------------------------------------------------   * 00005000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00006000
      *    DATUM:    14.09.1994                                       * 00007000
      ***************************************************************** 00008000
      *    VERSION   1    12.01.95 17:18:37
      ****************************************************************  00009100
      * AENDERUNGEN:                                                 *  00009200
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00009300
      * -------------------------------------------------------------*  00009400
      * AFVB0142!14.09.94!    ! NEU                       ! SCHÖLLH. *  00009500
      *         !12.01.95!    ! PROD.-ÜBERGABE            ! NAUMANN  *  00009601
      * AFPA0002!03.02.97! 02 ! DYNAMISCHER CALL AUF UPRO ! AUERBACH *  00009700
      *         !        !    !                           !          *  00009800
      *         !        !    !                           !          *  00009900
      *         !        !    !                           !          *  00010000
      *                                                              *  00010100
      * ENDE-LOGBUCH                                                 *  00010200
      ****************************************************************  00010300
      *                                                                 00010400
      *    SCHWERER-FEHLER-ROUTINE FUER DIALOGE OHNE                    00110000
      *    STANDARD-RAHMEN-STEUERUNG                                    00111000
      *    - AUFRUF DURCH RAHMEN AUS ANDEREN FEHLER-ROUTINEN            00120000
      *    - AUFRUF DURCH FACHFUNKTION BEI FEHLER AUS UPRO-AUFRUF       00121000
      *                                                                 00122000
      *    ABWEICHUNGEN ZUR SCHWERER-FEHLER-ROUTINE IN                  00122100
      *    DIALOGEN MIT STANDARD-RAHMEN-STEUERUNG:                      00122200
      *    - KEIN ABFANGEN VON 'DEADLOCK'                               00122300
      *                                                                 00122400
      *                                                                 00122600
      *------------------------------------------------------------*    00123000
      * UR99-SCHWERER-FEHLER                                       *    00124000
      *------------------------------------------------------------*    00125000
      * INPUT : EINGABE-WERTE - ABH. VON ERR-FEHLER-TYP,           *    00126000
      *                                  ERR-AFBEREITUNG:          *    00126100
      *         -------------------------------------------------- *    00127000
      *         ERR-FEHLER-TYP! 1(SQL) ! 2(CICS)! 3(PROG)!         *    00128000
      *         ERR-AUFBEREIT.!        !        !        !  DB2S   *    00128100
      *         --------------+--------+--------+--------+-------- *    00129000
      *         ERR-ORT       !   X    !   X    !   X    !   X     *    00130000
      *         ERR-PROGNAME  !   X    !   X    !   X    !   X     *    00150000
      *         ERR-SQLCODE   !   X    !        !        !         *    00160000
      *         ERR-CICS-RC   !        !   X    !        !         *    00170000
      *         ERR-VAR-RAHMEN!        !        !        !   X     *    00180000
      *         ERR-VAR (OPT) !  (X)   !  (X)   !  (X)   !  (X)    *    00200000
      *         -------------------------------------------------- *    00210000
      *                                                            *    00220000
      * OUTPUT: FEHLERINFORMATIONEN                                *    00230000
      *                                                            *    00240000
      * VERARBEITUNG:                                              *    00250000
      *   - ROLLBACK                                               *    00260000
      *   - ABHAENGIG VOM ERR-FEHLER-TYP WERDEN INFORMATIONEN ZUM  *    00270000
      *     FEHLER AUFBEREITET UND AUSGEGEBEN                      *    00280000
      *   - BEI BESTIMMTEN FEHLERN WIRD VERSUCHT, INFORMATIONEN    *    00290000
      *     IN EINE DB2-TABLE ZU SCHREIBEN; BEI MISSERFOLG         *    00291000
      *     WIRD EINE OPERATOR-MESSAGE AUSGEGEBEN                  *    00292000
      *                                                            *    00296000
      *------------------------------------------------------------*    00300000
       UR99-SCHWERER-FEHLER SECTION.                                    00310000
       UR99-ANF.                                                        00320000
                                                                        00330000
      *****  ROLLBACK                                                   00340000
                                                                        00350000
CCC005*    EXEC CICS SYNCPOINT                                          00360000
CCC004*              ROLLBACK                                           00370000
CCC003*    END-EXEC                                                     00380000
                                                                        00390000
      *****  EINTRAG GEMEINSAMER FEHLER-INFORMATIONEN FUER ALLE TYPEN   00400000
                                                                        00410000
           MOVE K-PGM-NAME                 TO ERR-PROGNAME              00440000
                                                                        00450000
      *****  SQL: SCHREIBEN VON OPERATOR-NOTIZ BEI BEST. SQL-CODES      00460000
                                                                        00470000
           IF ERR-FTYP-SQL       OR                                     00480000
              ERR-FTYP-ROLLBACK                                         00490000
                                                                        00500000
              PERFORM UR991-SQL-OPERATOR-NOTIZ                          00510000
           END-IF                                                       00520000
                                                                        00530000
      *****  AUFBEREITEN FEHLER-INFORMATIONEN                           00540000
                                                                        00550000
030297*     CALL 'UPAT0001' USING BY CONTENT   ERR-ERROR-BEREICH        00560000
CCCPY1*    CALL K-UPAT0001 USING BY CONTENT   ERR-ERROR-BEREICH         00560000
CCC004*                          BY REFERENCE PO-UPAT0001-AUSGABE       00570000
CCC004*                                                                 00580000
      *****  AUSGABE FEHLER-INFORMATIONEN                               00601000
CCC004*                                                                 00602000
CCC005*    EXEC CICS SEND                                               00610000
CCC004*                   FROM   (PO-UPAT0001-D-ERROR-BER)              00620000
CCC004*                   LENGTH (K-D-ERROR-LEN)                        00630000
CCC004*                   ERASE                                         00640000
CCC004*                   RESP   (C-CICS-RC)                            00650000
CCC003*    END-EXEC                                                     00660000
           IF C-CICS-RC NOT = DFHRESP(NORMAL)                           00670000
              CONTINUE                                                  00680000
           END-IF                                                       00690000
                                                                        00700000
CCC005*    EXEC CICS RETURN                                             00710000
CCC004*              RESP   (C-CICS-RC)                                 00720000
CCC003*    END-EXEC                                                     00730000
           IF C-CICS-RC NOT = DFHRESP(NORMAL)                           00740000
              CONTINUE                                                  00750000
           END-IF                                                       00760000
           .                                                            00770000
       UR99-EXIT.                                                       00780000
           EXIT.                                                        00790000
      *                                                                 00800000
      *------------------------------------------------------------*    00810000
      * UR991-SQL-OPERATOR-NOTIZ                                   *    00820000
      *------------------------------------------------------------*    00830000
      * INPUT : ERR-ERROR-BEREICH                                  *    00840000
      *                                                            *    00850000
      * OUTPUT: (OPERATOR-NOTIZ)                                   *    00860000
      *                                                            *    00870000
      * VERARBEITUNG:                                              *    00880000
      *   - BEI BESTIMMTEN SQL-CODES SCHREIBEN VON OPERATOR-NOTIZ  *    00890000
      *     IN DB2-TABELLE                                         *    00900000
      *   - FALLS SCHREIBEN ERFOLGLOS, MELDUNG AUF OPERATOR-CONSOLE*    00910000
      *                                                            *    00920000
      *------------------------------------------------------------*    00930000
       UR991-SQL-OPERATOR-NOTIZ SECTION.                                00940000
       UR991-ANF.                                                       00950000
                                                                        00960000
      ***** SCHREIBEN SQL-INFOS IN DB2-TAB                              00970000
                                                                        00980000
           EVALUATE ERR-SQLCODE                                         00990000
           WHEN -677                                                    01000000
           WHEN -802                                                    01010000
           WHEN -999 THRU -900                                          01020000
              MOVE 'DB2RES:'               TO PI-UPAT0005-FILLER-1      01030000
              MOVE ':'                     TO PI-UPAT0005-FILLER-2,     01040000
                                              PI-UPAT0005-FILLER-3,     01050000
                                              PI-UPAT0005-FILLER-4      01060000
                                                                        01070000
              MOVE ERR-PROGNAME            TO PI-UPAT0005-HPROG-ID      01080000
              MOVE ERR-UPRONAME            TO PI-UPAT0005-UPROG-ID      01090000
              MOVE ERR-SQLCODE             TO PI-UPAT0005-SQLCODE       01100000
              MOVE ERR-SQLERRMC            TO PI-UPAT0005-SQLERRMC      01110000
                                                                        01120000
030297*        CALL 'UPAT0005' USING BY CONTENT   PI-UPAT0005-EINGABE   01140000
CCCPY1*       CALL K-UPAT0005 USING BY CONTENT   PI-UPAT0005-EINGABE    01140000
CCC004*                             BY REFERENCE ERR-ERROR-BEREICH      01150000
CCC004*                                                                 01170000
CCC004*       IF ERR-RC-SCHWERER-FEHLER                                 01180000
      *          * MELDUNG AUF OPERATOR-CONSOLE                         01190000
CCC004*                                                                 01200000
CCC005*          EXEC CICS                                              01210000
CCC004*               WRITE OPERATOR TEXT      (PI-UPAT0005-EINGABE)    01220000
CCC004*                              TEXTLENGTH(K-PI-UPAT0005-LEN)      01230000
CCC004*                              RESP      (C-CICS-RC)              01240000
CCC003*          END-EXEC                                               01250000
                 IF C-CICS-RC NOT = DFHRESP(NORMAL)                     01260000
                    CONTINUE                                            01270000
                 END-IF                                                 01280000
              END-IF                                                    01290000
                                                                        01300000
           WHEN OTHER                                                   01310000
              CONTINUE                                                  01320000
           END-EVALUATE                                                 01330000
           .                                                            01340000
       UR991-EXIT.                                                      01350000
           EXIT.                                                        01360000
      *                                                                 01362000
      ******* ENDE COPY-MEMBER **************************************   01380000
                                                                        01390000
