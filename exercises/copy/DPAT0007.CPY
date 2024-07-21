      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0007.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0007.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:11:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010002
      *    COPY-MEMBER: DPAT0007       SERVICE-ROUTINE FÜR TS-QUEUES  * 00020002
      *    --------------------------------------------------------   * 00030002
      *    VERSION   001                                              * 00040002
      *    --------------------------------------------------------   * 00050002
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060002
      *    DATUM:    19.03.1993                                       * 00070002
      *    STAND:    03.04.1993                                       * 00080013
      ***************************************************************** 00090002
      *                                                                 00100002
      *    ZUGRIFFE AUF TS-QUEUES                                       00110002
      *    DIESE ROUTINE IST FUER ALLE TS-QUEUES GEEIGNET, BEI DENEN    00120002
      *    DIE DATEN UEBER DEN VARIABLEN BEREICH 'TSQ-DATA' UEBERGEBEN  00130002
      *    WERDEN KÖNNEN.                                               00140002
      *    (VGL. AUCH DWAT0007)                                         00150002
      *                                                                 00160002
      *------------------------------------------------------------*    00170002
      * UR3-TSQ-SERVICE                                            *    00180011
      *------------------------------------------------------------*    00190002
      * INPUT : ZI-DWAT0007-INPUT                                  *    00200002
      *                                                            *    00210002
      * OUTPUT: ZO-DWAT0007-OUTPUT                                 *    00220002
      *                                                            *    00230002
      * VERARBEITUNG:                                              *    00240002
      *   ABHAENGIG VON ZI-DWAT0007-TSQ-SERVICE                    *    00250002
      *   - LOESCHEN TS-QUEUE                                      *    00260002
      *   - SCHREIBEN IN/ LESEN AUS TS-QUEUE                       *    00270002
      *                                                            *    00280002
      * (DETAILS VGL. DWAT0007)                                    *    00290002
      *                                                            *    00300002
      *------------------------------------------------------------*    00310002
       UR3-TSQ-SERVICE SECTION.                                         00320011
       UR3-ANF.                                                         00330011
           MOVE 'UR3 '                     TO ERR-ORT-SEC               00340011
           PERFORM ZR1-TRACE-KURZ                                       00350009
      *                                                                 00360002
           SET TSQ-RC-OK                   TO TRUE                      00370002
           MOVE ZI-DWAT0007-TSQ-NAME       TO Z-TSQ-NAME                00380002
                                                                        00390002
           EVALUATE TRUE                                                00400002
      *    --------------------------------------------------------     00410002
      *            DELETE QUEUE                                         00420002
      *    --------------------------------------------------------     00430002
           WHEN TSQ-SERVICE-DELETE                                      00440002
CCC005*       EXEC CICS DELETEQ TS                                      00450002
CCC004*                 QUEUE (Z-TSQ-NAME)                              00460002
CCC004*                 RESP  (C-CICS-RC)                               00470002
CCC003*       END-EXEC                                                  00480002
              IF C-CICS-RC NOT = DFHRESP(NORMAL)                        00490002
                 IF C-CICS-RC = DFHRESP(QIDERR)                         00500002
      *             * KEINE TS-QUEUE VORHANDEN                          00510002
                    CONTINUE                                            00520002
                 ELSE                                                   00530002
                    MOVE K-1               TO ERR-ORT-LFD               00540002
                    PERFORM ZR92-CICS-FEHLER                            00550010
                 END-IF                                                 00560002
              END-IF                                                    00570002
                                                                        00580008
      *    --------------------------------------------------------     00590008
      *            WRITE  QUEUE                                         00600008
      *    --------------------------------------------------------     00610008
           WHEN TSQ-SERVICE-WRITE                                       00620008
              EVALUATE TRUE                                             00630008
              WHEN TSQ-PLACE-MAIN                                       00640008
      *          * WRITE NAECHSTES ITEM                                 00650008
CCC005*          EXEC CICS WRITEQ TS                                    00660008
CCC004*                    QUEUE  (Z-TSQ-NAME)                          00670008
CCC004*                    FROM   (ZI-DWAT0007-TSQ-DATA)                00680008
CCC004*                    MAIN                                         00690008
CCC004*                    RESP   (C-CICS-RC)                           00700008
CCC003*          END-EXEC                                               00710008
                 IF C-CICS-RC NOT = DFHRESP(NORMAL)                     00720008
                    MOVE K-2               TO ERR-ORT-LFD               00730008
                    PERFORM ZR92-CICS-FEHLER                            00740010
                 END-IF                                                 00750008
              WHEN OTHER                                                00760008
                 MOVE K-3                  TO ERR-ORT-LFD               00770008
                 MOVE 26                   TO ERR-MELD-NR               00780008
                 PERFORM UR31-ERROR-VORBER                              00790011
                 PERFORM UR93-PROG-FEHLER                               00800008
              END-EVALUATE                                              00810008
                                                                        00820002
      *    --------------------------------------------------------     00830002
      *           REWRITE QUEUE                                         00840002
      *    --------------------------------------------------------     00850002
           WHEN TSQ-SERVICE-REWRITE                                     00860002
              MOVE ZI-DWAT0007-TSQ-ITEM    TO Z-TSQ-ITEM                00870002
              EVALUATE TRUE                                             00880002
              WHEN TSQ-PLACE-MAIN                                       00890002
CCC005*          EXEC CICS WRITEQ TS                                    00900002
CCC004*                    QUEUE  (Z-TSQ-NAME)                          00910002
CCC004*                    FROM   (ZI-DWAT0007-TSQ-DATA)                00920002
CCC004*                    ITEM   (Z-TSQ-ITEM) REWRITE                  00930002
CCC004*                    MAIN                                         00940002
CCC004*                    RESP   (C-CICS-RC)                           00950002
CCC003*          END-EXEC                                               00960002
                 IF C-CICS-RC NOT = DFHRESP(NORMAL)                     00970002
                    IF C-CICS-RC = DFHRESP(ITEMERR)                     00980002
      *                * ANGEGEBENES ITEM NICHT VORHANDEN               00990003
                       PERFORM UR31-ERROR-VORBER                        01000011
                       SET TSQ-RC-ITEMERR  TO TRUE                      01010002
                    ELSE                                                01020002
                       MOVE K-4            TO ERR-ORT-LFD               01030002
                       PERFORM ZR92-CICS-FEHLER                         01040010
                    END-IF                                              01050002
                 END-IF                                                 01060002
              WHEN OTHER                                                01070002
                 MOVE K-5                  TO ERR-ORT-LFD               01080002
                 MOVE 26                   TO ERR-MELD-NR               01090006
                 PERFORM UR31-ERROR-VORBER                              01100011
                 PERFORM UR93-PROG-FEHLER                               01110002
              END-EVALUATE                                              01120002
                                                                        01130002
      *    --------------------------------------------------------     01140002
      *            READ   QUEUE                                         01150002
      *    --------------------------------------------------------     01160002
           WHEN TSQ-SERVICE-READ                                        01170002
              IF ZI-DWAT0007-TSQ-ITEM > ZERO                            01180002
      *          * READ ANGEGEBENES ITEM                                01190002
                 MOVE ZI-DWAT0007-TSQ-ITEM TO Z-TSQ-ITEM                01200002
CCC005*          EXEC CICS READQ TS                                     01210002
CCC004*                    QUEUE (Z-TSQ-NAME)                           01220002
CCC004*                    INTO  (ZO-DWAT0007-TSQ-DATA)                 01230002
CCC004*                    ITEM  (Z-TSQ-ITEM)                           01240002
CCC004*                    RESP  (C-CICS-RC)                            01250002
CCC003*          END-EXEC                                               01260002
                 IF C-CICS-RC NOT = DFHRESP(NORMAL)                     01270002
                    IF C-CICS-RC = DFHRESP(ITEMERR)                     01280002
      *                * ANGEGEBENES ITEM NICHT VORHANDEN               01290003
                       PERFORM UR31-ERROR-VORBER                        01300011
                       SET TSQ-RC-ITEMERR  TO TRUE                      01310002
                    ELSE                                                01320002
                       MOVE K-6            TO ERR-ORT-LFD               01330002
                       PERFORM ZR92-CICS-FEHLER                         01340010
                    END-IF                                              01350002
                 END-IF                                                 01360002
              ELSE                                                      01370002
      *          * READ NAECHSTES ITEM                                  01380002
CCC005*          EXEC CICS READQ TS                                     01390002
CCC004*                    QUEUE (Z-TSQ-NAME)                           01400002
CCC004*                    INTO  (ZO-DWAT0007-TSQ-DATA)                 01410002
CCC004*                    RESP  (C-CICS-RC)                            01420002
CCC003*          END-EXEC                                               01430002
                 IF C-CICS-RC NOT = DFHRESP(NORMAL)                     01440002
                    IF C-CICS-RC = DFHRESP(ITEMERR)                     01450002
      *                * KEIN ITEM MEHR VORHANDEN                       01460003
                       PERFORM UR31-ERROR-VORBER                        01470011
                       SET TSQ-RC-ITEMERR  TO TRUE                      01480002
                    ELSE                                                01490002
                       MOVE K-7            TO ERR-ORT-LFD               01500002
                       PERFORM ZR92-CICS-FEHLER                         01510010
                    END-IF                                              01520002
                 END-IF                                                 01530002
              END-IF                                                    01540002
                                                                        01550002
      *    --------------------------------------------------------     01560006
      *            FALSCHER TSQ-SERVICE                                 01570006
      *    --------------------------------------------------------     01580006
           WHEN OTHER                                                   01590002
              MOVE K-8                     TO ERR-ORT-LFD               01600002
              MOVE 27                      TO ERR-MELD-NR               01610006
              PERFORM UR31-ERROR-VORBER                                 01620011
              PERFORM UR93-PROG-FEHLER                                  01630002
           END-EVALUATE                                                 01640002
           .                                                            01650002
       UR3-EXIT.                                                        01660011
           EXIT.                                                        01670002
      *                                                                 01680006
      *------------------------------------------------------------*    01690006
      * UR31-ERROR-VORBER                                          *    01700011
      *------------------------------------------------------------*    01710006
      * INPUT : ZI-DWAT0007-INPUT, ZO-DWAT0007-OUTPUT              *    01720006
      *                                                            *    01730006
      * OUTPUT: ERR-AUFBEREITUNG, ERR-VAR-RAHMEN                   *    01740006
      *                                                            *    01750006
      * VERARBEITUNG:                                              *    01760006
      *   VORBEREITUNG FEHLER-AUFBEREITUNG ZU TSQ                  *    01770006
      *                                                            *    01780006
      *------------------------------------------------------------*    01790006
       UR31-ERROR-VORBER SECTION.                                       01800011
       UR31-ANF.                                                        01810011
      *                                                                 01820006
           SET ERR-AUFBER-TSQ              TO TRUE                      01830006
           MOVE ZI-DWAT0007-INPUT          TO ERR-DWAT0007-INPUT        01840006
           MOVE ZO-DWAT0007-OUTPUT         TO ERR-DWAT0007-OUTPUT       01850006
           .                                                            01860006
       UR31-EXIT.                                                       01870011
           EXIT.                                                        01880006
      *                                                                 01890002
      ******* ENDE COPY-MEMBER DPAT0007 (SERVICE FÜR TS-QUEUES) ******* 01900002
CCC009*    EJECT                                                        01910002
