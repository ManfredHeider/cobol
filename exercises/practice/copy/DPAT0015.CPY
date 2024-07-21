      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0015.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0015.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   13 Nov 1998 16:53:08   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   Mar 20 1997 16:56:04   T13EX16
      *DRIVER-ANPASSUNG DAISY
      *
      *   Rev 1.0   Nov 04 1995 08:13:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00001000
      *    COPY-MEMBER: DPAT0015      SCHWERER-FEHLER-BEHANDLUNG      * 00002100
      *    --------------------------------------------------------   * 00003000
      *    ERSTELLT: W. SCHOELLHAMMER (INTEGRATA AG)                  * 00004000
      *    DATUM:    24.03.1993                                       * 00005000
      ***************************************************************** 00006000
      *    VERSION   4    10.03.95 17:05:36
      ****************************************************************  00008000
      * AENDERUNGEN:                                                 *  00009000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00009100
      * -------------------------------------------------------------*  00009200
      *         !23.04.93! 01 ! NEU                       ! SCHOELLH.*  00009300
      * FEVB0156!27.09.94!    ! RAUSGENOMMEN: VERSORGEN V.! SCHOELLH.*  00009401
      *         !        !    !  PO-UPAT0001-D-AKT-DATUM  !          *  00009601
      *         !28.09.94! 02 ! PROD.ÜBERGABE             ! SCHÖLLH. *  00009702
      * FEVB0218!03.03.95!    ! ERR-DIALOG/-BILDER FÜLLEN ! SCHÖLLH. *  00009800
      *         !10.03.95! 03 ! PROD.UEBERGABE            ! SCHÖLLH. *  00070500
      *         !10.03.95!    ! FEHLER BEI COMPILE PGAT0001 SCHÖLLH. *  00009800
      *         !10.03.95! 04 ! PROD.UEBERGABE            ! SCHÖLLH. *  00070500
      *         !11.03.95!    ! FEHLER BEI COMPILE DIV.   ! SCHÖLLH. *  00009800
      *         !        !    !  PG-PROGRAMME             ! SCHÖLLH. *  00009800
      *         !10.03.95! 05 ! PROD.UEBERGABE            ! SCHÖLLH. *  00070500
      * AFPA0002!03.02.97! 06 ! DYNAMISCHER CALL DER UPRO ! AUERBACH *  00070500
      *         !24.02.97!    ! KEIN AUFRUF UPAT005 BEI   ! AUERBACH *  00009900
      *         !        !    ! SQL-CODE -923 U. -924     !          *  00009900
      * APA00012!12.08.98! 07 ! ERR-BEREICH füllen mit    ! AUERBACH *  00070500
      *         !        !    ! CICS-NAME UND USER-ID     !          *  00010000
      *         !        !    !                           !          *  00009900
      *                                                              *  00010000
      * ENDE-LOGBUCH                                                 *  00011000
      ****************************************************************  00020000
      *                                                                 00100000
      *    SCHWERER-FEHLER-ROUTINE FUER HPROS.                          00110000
      *    - AUFRUF DURCH RAHMEN AUS ANDEREN FEHLER-ROUTINEN            00120000
      *    - AUFRUF DURCH FACHFUNKTION BEI FEHLER AUS UPRO-AUFRUF       00121000
      *                                                                 00122000
      *------------------------------------------------------------*    00123000
      * UR99-SCHWERER-FEHLER                                       *    00124000
      *------------------------------------------------------------*    00125000
      * INPUT : EINGABE-WERTE - ABH. VON ERR-FEHLER-TYP:           *    00126000
      *         -------------------------------------------------- *    00127000
      *         ERR-FEHLER-TYP! 1(SQL) ! 2(CICS)! 3(PROG)! 4(DB2S) *    00128000
      *         --------------+--------+--------+--------+-------- *    00129000
      *         ERR-ORT       !   X    !   X    !   X    !   X     *    00130000
      *         ERR-MELD-KEY  !        !        !   X    !         *    00140000
      *         ERR-PROGNAME  !   X    !   X    !   X    !   X     *    00150000
      *         ERR-SQLCODE   !   X    !        !        !         *    00160000
      *         ERR-CICS-RC   !        !   X    !        !         *    00170000
      *         PR-RETURN(ETC)!        !        !        !   X     *    00180000
      *         PS-PROGR (ETC)!        !        !        !   X     *    00190000
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
      *     IN EINER DB2-TABLE ZU SCHREIBEN; BEI MISSERFOLG        *    00291000
      *     WIRD EINE OPERATOR-MESSAGE AUSGEGEBEN                  *    00292000
      *   - SEITENAUSSTIEG:                                        *    00292100
      *     BEI DEADLOCK WIRD DER DRIVER MIT CADR-E-OP 'DDL'       *    00292200
      *     AUFGERUFEN (FUERT ZU ERNEUTEM EINSTIEG IN DEN          *    00292300
      *     AKTUELLEN DIALOG PER 'DIALOG-ERSTAUFRUF')              *    00292400
      *                                                            *    00292500
      *------------------------------------------------------------*    00292600
       UR99-SCHWERER-FEHLER SECTION.                                    00292700
       UR99-ANF.                                                        00292800
                                                                        00292900
      *****  ROLLBACK                                                   00293000
                                                                        00294000
CCC005*    EXEC CICS SYNCPOINT                                          00295000
CCC004*              ROLLBACK                                           00296000
CCC003*    END-EXEC                                                     00297000
                                                                        00298000
      *****  EINTRAG GEMEINSAMER FEHLER-INFORMATIONEN FUER ALLE TYPEN   00299000
                                                                        00300000
           MOVE K-SF-PGM-NAME              TO ERR-MELD-PGM              00310000
           MOVE K-E                        TO ERR-MELD-TYP              00320000
           MOVE K-PGM-NAME                 TO ERR-PROGNAME              00330000
100395* 030395     MOVE CARR-DIALOG-AKT    TO ERR-DIALOG                00330000
100395* 030395     MOVE CARD-DIALOG-SCHRITT-AKT TO ERR-DIALOG-SCHRITT   00330000
100395* 030395     MOVE T-BILDER-TAB       TO ERR-BILDER-TAB            00330000
100395     MOVE CARR-DIALOG-AKT IN C-COMMAREA                           00330000
100395                                     TO ERR-DIALOG                00330000
100395     MOVE CARD-DIALOG-SCHRITT-AKT IN C-COMMAREA                   00330000
100395                                     TO ERR-DIALOG-SCHRITT        00330000
110395* 100395 MOVE T-BILDER-TAB IN C-COMMAREA TO ERR-BILDER-TAB        00330000
110395     MOVE T-BILDER-TAB               TO ERR-BILDER-TAB            00330000

120898     MOVE CARU-CICS-NAME IN C-COMMAREA TO ERR-CICS-NAME           00330000
120898     MOVE TS-USERID                    TO ERR-USERID              00330000
                                                                        00340000
      *****  SQL: SCHREIBEN VON OPERATOR-NOTIZ BEI BEST. SQL-CODES      00350000
                                                                        00360000
           IF ERR-FTYP-SQL       OR                                     00370000
              ERR-FTYP-ROLLBACK                                         00380000
                                                                        00390000
              PERFORM UR991-SQL-OPERATOR-NOTIZ                          00400000
           END-IF                                                       00410000
                                                                        00420000
      *****  SEITENAUSSTIEG !!!  BEI DEADLOCK                           00430000
                                                                        00440000
           IF ERR-FTYP-SQL       OR                                     00450000
              ERR-FTYP-ROLLBACK                                         00460000
                                                                        00470000
              PERFORM UR992-SQL-DEADLOCK-PRUEF                          00480000
           END-IF                                                       00490000
                                                                        00500000
      *****  AUFBEREITEN FEHLER-INFORMATIONEN                           00510000
                                                                        00520000
030297*     CALL 'UPAT0001' USING BY CONTENT   ERR-ERROR-BEREICH        00530000
CCCPY1*    CALL K-UPAT0001 USING BY CONTENT   ERR-ERROR-BEREICH         00530000
CCC004*                          BY REFERENCE PO-UPAT0001-AUSGABE       00540000
CCC004*                                                                 00550000
270994*     MOVE D-AKT-DATUM-KURZ           TO PO-UPAT0001-D-AKT-DATUM  00560000
CCC004*                                                                 00570000
      *****  AUSGABE FEHLER-INFORMATIONEN                               00580000
CCC004*                                                                 00590000
CCC005*    EXEC CICS SEND                                               00600000
CCC004*                   FROM   (PO-UPAT0001-D-ERROR-BER)              00610000
CCC004*                   LENGTH (K-D-ERROR-LEN)                        00620000
CCC004*                   ERASE                                         00630000
CCC004*                   RESP   (C-CICS-RC)                            00640000
CCC003*    END-EXEC                                                     00650000
           IF C-CICS-RC NOT = DFHRESP(NORMAL)                           00660000
              CONTINUE                                                  00670000
           END-IF                                                       00680000
                                                                        00690000
CCC005*    EXEC CICS RETURN                                             00700000
CCC004*              RESP   (C-CICS-RC)                                 00710000
CCC003*    END-EXEC                                                     00720000
           IF C-CICS-RC NOT = DFHRESP(NORMAL)                           00730000
              CONTINUE                                                  00740000
           END-IF                                                       00750000
           .                                                            00760000
       UR99-EXIT.                                                       00770000
           EXIT.                                                        00780000
      *                                                                 00790000
      *------------------------------------------------------------*    00800000
      * UR991-SQL-OPERATOR-NOTIZ                                   *    00810000
      *------------------------------------------------------------*    00820000
      * INPUT : ERR-ERROR-BEREICH                                  *    00830000
      *                                                            *    00840000
      * OUTPUT: (OPERATOR-NOTIZ)                                   *    00850000
      *                                                            *    00860000
      * VERARBEITUNG:                                              *    00870000
      *   - BEI BESTIMMTEN SQL-CODES SCHREIBEN VON OPERATOR-NOTIZ  *    00880000
      *     IN DB2-TABELLE                                         *    00890000
240297*   - IM FALLE VON SQL-CODE -923 UND -924 KANN KEIN INSERT   *    00890000
240297*     IM UPAT0005 ERFOLGEN, DER CICS BRICHT DIE TRAN MIT     *    00890000
240297*     'AEY9' AB. BEIDE SQL-CODES WERDEN JETZT üBERGANGEN.    *    00890000
      *   - FALLS SCHREIBEN ERFOLGLOS, MELDUNG AUF OPERATOR-CONSOLE*    00900000
      *                                                            *    00910000
      *------------------------------------------------------------*    00920000
       UR991-SQL-OPERATOR-NOTIZ SECTION.                                00930000
       UR991-ANF.                                                       00940000
                                                                        00950000
      ***** SCHREIBEN SQL-INFOS IN DB2-TAB                              00960000
                                                                        00970000
           EVALUATE ERR-SQLCODE                                         00980000
           WHEN -677                                                    00990000
           WHEN -802                                                    01000000
240297     WHEN -999 THRU -925                                          01010000
240297     WHEN -922 THRU -900                                          01010000
              MOVE 'DB2RES:'               TO PI-UPAT0005-FILLER-1      01020000
              MOVE K-DOPPELPUNKT           TO PI-UPAT0005-FILLER-2,     01030000
                                              PI-UPAT0005-FILLER-3,     01040000
                                              PI-UPAT0005-FILLER-4      01050000
                                                                        01060000
              MOVE ERR-PROGNAME            TO PI-UPAT0005-HPROG-ID      01070000
              MOVE ERR-UPRONAME            TO PI-UPAT0005-UPROG-ID      01080000
              MOVE ERR-SQLCODE             TO PI-UPAT0005-SQLCODE       01090000
              MOVE ERR-SQLERRMC            TO PI-UPAT0005-SQLERRMC      01100000
                                                                        01110000
030297*        CALL 'UPAT0005' USING BY CONTENT   PI-UPAT0005-EINGABE   01120000
CCCPY1*       CALL K-UPAT0005 USING BY CONTENT   PI-UPAT0005-EINGABE    01120000
CCC004*                             BY REFERENCE ERR-ERROR-BEREICH      01130000
CCC004*                                                                 01140000
CCC004*       IF ERR-RC-SCHWERER-FEHLER                                 01150000
      *          * MELDUNG AUF OPERATOR-CONSOLE                         01160000
CCC004*                                                                 01170000
CCC005*          EXEC CICS                                              01180000
CCC004*               WRITE OPERATOR TEXT      (PI-UPAT0005-EINGABE)    01190000
CCC004*                              TEXTLENGTH(K-PI-UPAT0005-LEN)      01200000
CCC004*                              RESP      (C-CICS-RC)              01210000
CCC003*          END-EXEC                                               01220000
                 IF C-CICS-RC NOT = DFHRESP(NORMAL)                     01230000
                    CONTINUE                                            01240000
                 END-IF                                                 01250000
              END-IF                                                    01260000
                                                                        01270000
           WHEN OTHER                                                   01280000
              CONTINUE                                                  01290000
           END-EVALUATE                                                 01300000
           .                                                            01310000
       UR991-EXIT.                                                      01320000
           EXIT.                                                        01330000
      *                                                                 01340000
      *------------------------------------------------------------*    01350000
      * UR992-SQL-DEADLOCK-PRUEF                                   *    01360000
      *------------------------------------------------------------*    01361000
      * INPUT : ERR-ERROR-BEREICH                                  *    01362000
      *                                                            *    01363000
      * OUTPUT: CADR-BEREICH                                       *    01364000
      *                                                            *    01365000
      * VERARBEITUNG:                                              *    01366000
      *   FALLS SQL-DEADLOCK-RC                                    *    01367000
      *   - VERSORGEN CADR-E-OP                                    *    01368000
      *   - WECHSEL ZU DRIVER                                      *    01369000
      *                                                            *    01369100
      *------------------------------------------------------------*    01369200
       UR992-SQL-DEADLOCK-PRUEF SECTION.                                01369300
       UR992-ANF.                                                       01369400
      *                                                                 01369500
           EVALUATE ERR-SQLCODE                                         01369600
           WHEN -911                                                    01369700
           WHEN -913                                                    01369800
                                                                        01369900
      *       * VORBEREITUNG DRIVER-AUFRUF                              01370000
      *       * DEADLOCK AN DRIVER                                      01370100
              MOVE K-OP-DDL          TO CADR-E-OP         IN C-COMMAREA 01370200
              MOVE SPACE             TO CADR-E-TRANS-NEXT IN C-COMMAREA 01370300
              INITIALIZE                ERR-ERROR-BEREICH               01370400
              MOVE ERR-TRACE         TO CARU-TRACE        IN C-COMMAREA 01370500
                                                                        01370600
      *       * XCTL ZUM DRIVER                                         01370700
CCC005*       EXEC CICS XCTL   PROGRAM  (CARR-DRIVER-PGM IN C-COMMAREA) 01370800
CCC004*                        COMMAREA (C-COMMAREA)                    01370900
CCC004*                        LENGTH   (K-CA-LEN)                      01371000
CCC004*                        RESP     (C-CICS-RC)                     01372000
CCC003*       END-EXEC                                                  01373000
              IF C-CICS-RC NOT = DFHRESP(NORMAL)                        01373100
                 MOVE 82                      TO ERR-ORT-LFD            01373200
                 PERFORM ZR92-CICS-FEHLER                               01373300
              END-IF                                                    01373400
                                                                        01373500
           WHEN OTHER                                                   01373600
                CONTINUE                                                01373700
           END-EVALUATE                                                 01373800
           .                                                            01373900
       UR992-EXIT.                                                      01374000
           EXIT.                                                        01375000
      *                                                                 01376000
      ******* ENDE COPY-MEMBER DPAT0015 (SCHW-FEHLER-BEHANDLUNG) ****   01377000
CCC009*    EJECT                                                        01378000
