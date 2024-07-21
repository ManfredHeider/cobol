      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0019.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0019.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:14:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0019       SERVICE FUER PERSDAT-ZUGRIFFE  * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   2    30.08.94 17:01:02
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    01.04.1993                                       * 00070000
      *    STAND:    15.04.1993                                       * 00080000
      ***************************************************************** 00090000
      *    AUFTRAG  GRUND                  VERSION  DATUM    NAME     * 00090100
      *---------------------------------------------------------------* 00090200
      *    AFRZ0013 UMSTELLUNG KUS / RACF    01     26.08.94 WIECHERS * 00091000
      *    AFRZ0013 KORREKTUR  KUS / RACF    02     30.08.94 WIECHERS * 00091000
      ***************************************************************** 00092000
      ***************************************************************** 00093000
      *                                                                 00100000
      *    ZUGRIFFE AUF PERSDAT (ANWENDER-DATEI)                        00110000
      *    (DETAILS VGL. DWAT0019)                                      00120000
      *                                                                 00130000
      *------------------------------------------------------------*    00140000
      * UR6-PERSDAT-SERVICE                                        *    00150000
      *------------------------------------------------------------*    00160000
      * INPUT : ZI-DWAT0019-INPUT                                  *    00170000
      *         ZIO-DWAT0019-INOUT                                 *    00180000
      *                                                            *    00190000
      * OUTPUT: ZO-DWAT0019-OUTPUT                                 *    00200000
      *         ZIO-DWAT0019-INOUT                                 *    00210000
      *                                                            *    00220000
      * VERARBEITUNG:                                              *    00230000
      *   ABHAENGIG VON ZI-DWAT0019-PD-SERVICE                     *    00240000
      *   ZUGRIFF AUF PERSDAT UND VERSORGEN OUTPUT-BEREICH         *    00250000
      *                                                            *    00260000
      * (DETAILS VGL. DWAT0019)                                    *    00270000
      *                                                            *    00280000
      *------------------------------------------------------------*    00290000
       UR6-PERSDAT-SERVICE SECTION.                                     00300000
       UR6-ANF.                                                         00310000
           MOVE 'UR6 '                     TO ERR-ORT-SEC               00320000
           PERFORM ZR1-TRACE-KURZ                                       00330000
      *                                                                 00340000
           SET PD-RC-OK                    TO TRUE                      00350000
                                                                        00360000
           EVALUATE TRUE                                                00370000
      *     WHEN PD-SERVICE-STARTBR-EQUAL                               00380000
      *        PERFORM UR61-PD-STARTBR-EQUAL                            00390000
                                                                        00400000
           WHEN PD-SERVICE-STARTBR-GTEQ                                 00410000
              PERFORM UR62-PD-STARTBR-GTEQ                              00420000
           WHEN PD-SERVICE-ENDBR                                        00430000
              PERFORM UR63-PD-ENDBR                                     00440000
           WHEN PD-SERVICE-READNEXT                                     00450000
              PERFORM UR64-PD-READNEXT                                  00460000
                                                                        00470000
      *     WHEN PD-SERVICE-READPREV                                    00480000
      *        PERFORM UR64-PD-READPREV                                 00490000
                                                                        00500000
           WHEN PD-SERVICE-READ                                         00510000
              PERFORM UR66-PD-READ                                      00520000
                                                                        00530000
      *     WHEN PD-SERVICE-READUPDATE                                  00540000
      *        PERFORM UR66-PD-READUPDATE                               00550000
      *     WHEN PD-SERVICE-REWRITE                                     00560000
      *        PERFORM UR66-PD-REWRITE                                  00570000
      *     WHEN PD-SERVICE-WRITE                                       00580000
      *        PERFORM UR66-PD-WRITE                                    00590000
                                                                        00600000
           WHEN OTHER                                                   00610000
      *       * FALSCHER PD-SERVICE                                     00620000
              MOVE 1                       TO ERR-ORT-LFD               00630000
      *       MOVE ???                     TO ERR-MELD-NR               00640000
              PERFORM UR93-PROG-FEHLER                                  00650000
           END-EVALUATE                                                 00660000
           .                                                            00670000
       UR6-EXIT.                                                        00680000
           EXIT.                                                        00690000
      *                                                                 00700000
      *------------------------------------------------------------*    00710000
      * UR62-PD-STARTBR-GTEQ                                       *    00720000
      *------------------------------------------------------------*    00730000
      * INPUT : ZI-DWAT0019-INPUT                                  *    00740000
      *         ZIO-DWAT0019-INOUT                                 *    00750000
      *                                                            *    00760000
      * OUTPUT: ZO-DWAT0019-OUTPUT                                 *    00770000
      *                                                            *    00780000
      * VERARBEITUNG:                                              *    00790000
      *   - STARTBR PERSDAT                                        *    00800000
      *                                                            *    00810000
      *------------------------------------------------------------*    00820000
       UR62-PD-STARTBR-GTEQ SECTION.                                    00830000
       UR62-ANF.                                                        00840000
           MOVE 'UR62 '                    TO ERR-ORT-SEC               00850000
           PERFORM ZR1-TRACE-KURZ                                       00860000
      *                                                                 00870000
300894*    EXEC CICS STARTBR  DATASET ('DD0002')                        00880000
CCC005*    EXEC CICS STARTBR  DATASET ('AT0104')                        00880000
CCC004*                       RIDFLD  (ZIO-DWAT0019-PD-KEY)             00890000
CCC004*                       GTEQ                                      00900000
CCC004*                       RESP    (C-CICS-RC)                       00910000
CCC003*    END-EXEC                                                     00920000
           IF C-CICS-RC NOT = DFHRESP(NORMAL)                           00930000
              IF C-CICS-RC = DFHRESP(NOTFND)                            00940000
                 SET PD-RC-NOTFND          TO TRUE                      00950000
              ELSE                                                      00960000
                 MOVE K-1                  TO ERR-ORT-LFD               00970000
                 PERFORM ZR92-CICS-FEHLER                               00980000
              END-IF                                                    00990000
           END-IF                                                       01000000
      *                                                                 01010000
           .                                                            01020000
       UR62-EXIT.                                                       01030000
           EXIT.                                                        01040000
                                                                        01050000
      *------------------------------------------------------------*    01060000
      * UR63-PD-ENDBR                                              *    01070000
      *------------------------------------------------------------*    01080000
      * INPUT : ZI-DWAT0019-INPUT                                  *    01090000
      *                                                            *    01100000
      * OUTPUT: ZO-DWAT0019-OUTPUT                                 *    01110000
      *                                                            *    01120000
      * VERARBEITUNG:                                              *    01130000
      *   - ENDBR PERSDAT                                          *    01140000
      *                                                            *    01150000
      *------------------------------------------------------------*    01160000
       UR63-PD-ENDBR SECTION.                                           01170000
       UR63-ANF.                                                        01180000
           MOVE 'UR63 '                    TO ERR-ORT-SEC               01190000
           PERFORM ZR1-TRACE-KURZ                                       01200000
      *                                                                 01210000
300894*    EXEC CICS ENDBR    DATASET ('DD0002')                        01220000
CCC005*    EXEC CICS ENDBR    DATASET ('AT0104')                        01220000
CCC004*                       RESP    (C-CICS-RC)                       01230000
CCC003*    END-EXEC                                                     01240000
           IF C-CICS-RC NOT = DFHRESP(NORMAL)                           01250000
              MOVE K-1                     TO ERR-ORT-LFD               01260000
              PERFORM ZR92-CICS-FEHLER                                  01270000
           END-IF                                                       01280000
           .                                                            01290000
       UR63-EXIT.                                                       01300000
           EXIT.                                                        01310000
                                                                        01320000
      *------------------------------------------------------------*    01330000
      * UR64-PD-READNEXT                                           *    01340000
      *------------------------------------------------------------*    01350000
      * INPUT : ZI-DWAT0019-INPUT                                  *    01360000
      *         ZIO-DWAT0019-INOUT                                 *    01370000
      *                                                            *    01380000
      * OUTPUT: ZO-DWAT0019-OUTPUT                                 *    01390000
      *         ZIO-DWAT0019-INOUT                                 *    01400000
      *                                                            *    01410000
      * VERARBEITUNG:                                              *    01420000
      *   - READNEXT PERSDAT                                       *    01430000
      *                                                            *    01440000
      *------------------------------------------------------------*    01450000
       UR64-PD-READNEXT SECTION.                                        01460000
       UR64-ANF.                                                        01470000
           MOVE 'UR64 '                    TO ERR-ORT-SEC               01480000
           PERFORM ZR1-TRACE-KURZ                                       01490000
      *                                                                 01500000
300894*    EXEC CICS READNEXT INTO    (ZIO-DWAT0019-PD-DATA)            01510000
300894*                       DATASET ('DD0002')                        01520000
CCC005*    EXEC CICS READNEXT INTO    (ZIO-DWAT0019-PD-DATA)            01510000
CCC004*                       DATASET ('AT0104')                        01520000
CCC004*                       RIDFLD  (ZIO-DWAT0019-PD-KEY)             01530000
CCC004*                       RESP    (C-CICS-RC)                       01540000
CCC003*    END-EXEC                                                     01550000
           EVALUATE C-CICS-RC                                           01560000
           WHEN DFHRESP(NORMAL)                                         01570000
              CONTINUE                                                  01580000
           WHEN DFHRESP(ENDFILE)                                        01590000
              SET PD-RC-ENDFILE            TO TRUE                      01600000
           WHEN DFHRESP(NOTFND)                                         01610000
              SET PD-RC-NOTFND             TO TRUE                      01620000
           WHEN DFHRESP(DUPKEY)                                         01630000
              SET PD-RC-DUPKEY             TO TRUE                      01640000
           WHEN OTHER                                                   01650000
              MOVE K-1                     TO ERR-ORT-LFD               01660000
              PERFORM ZR92-CICS-FEHLER                                  01670000
           END-EVALUATE                                                 01680000
           .                                                            01690000
       UR64-EXIT.                                                       01700000
           EXIT.                                                        01710000
                                                                        01720000
      *------------------------------------------------------------*    01730000
      * UR66-PD-READ                                               *    01740000
      *------------------------------------------------------------*    01750000
      * INPUT : ZI-DWAT0019-INPUT                                  *    01760000
      *         ZIO-DWAT0019-INOUT                                 *    01770000
      *                                                            *    01780000
      * OUTPUT: ZO-DWAT0019-OUTPUT                                 *    01790000
      *         ZIO-DWAT0019-INOUT                                 *    01800000
      *                                                            *    01810000
      * VERARBEITUNG:                                              *    01820000
      *   - READ SATZ AUS PERSDAT                                  *    01830000
      *                                                            *    01840000
      *------------------------------------------------------------*    01850000
       UR66-PD-READ SECTION.                                            01860000
       UR66-ANF.                                                        01870000
           MOVE 'UR66 '                    TO ERR-ORT-SEC               01880000
           PERFORM ZR1-TRACE-KURZ                                       01890000
      *                                                                 01900000
300894*    EXEC CICS READ     INTO    (ZIO-DWAT0019-PD-DATA)            01910000
300894*                       DATASET ('DD0002')                        01920000
CCC005*    EXEC CICS READ     INTO    (ZIO-DWAT0019-PD-DATA)            01910000
CCC004*                       DATASET ('AT0104')                        01920000
CCC004*                       RIDFLD  (ZIO-DWAT0019-PD-KEY)             01930000
CCC004*                       RESP    (C-CICS-RC)                       01940000
CCC003*    END-EXEC                                                     01950000
           IF C-CICS-RC NOT = DFHRESP(NORMAL)                           01960000
              IF C-CICS-RC = DFHRESP(NOTFND)                            01970000
                 SET PD-RC-NOTFND          TO TRUE                      01980000
              ELSE                                                      01990000
                 MOVE K-1                  TO ERR-ORT-LFD               02000000
                 PERFORM ZR92-CICS-FEHLER                               02010000
              END-IF                                                    02020000
           END-IF                                                       02030000
           .                                                            02040000
       UR66-EXIT.                                                       02050000
           EXIT.                                                        02060000
      *                                                                 02070000
      ******* ENDE COPY-MEMBER DPAT0019 (SERVICE FÜR PERS-DAT)  ******* 02080000
CCC009*    EJECT                                                        02090000
