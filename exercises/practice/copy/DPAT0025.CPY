      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0025.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0025.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:15:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00017000
      *    COPY-MEMBER: DPAT0025   PRUEFEN TRANS IN NEUANWENDUNGEN    * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    14.04.1993                                       * 00070000
      ***************************************************************** 00080500
      *    VERSION   2    26.08.94 16:30:37
      ****************************************************************  00080701
      * AENDERUNGEN:                                                 *  00080801
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00080901
      * -------------------------------------------------------------*  00081001
      *         !21.04.93! 01 ! NEU                       ! SCHÖLLH. *  00081101
      * AFRZ0013!26.08.94! 02 ! UMST. KUS-SYSTEM AUF RACF ! WIECHERS *  00081202
      *         !        !    !                           !          *  00081301
      *         !        !    !                           !          *  00081401
      *         !        !    !                           !          *  00081501
      *                                                              *  00081601
      * ENDE-LOGBUCH                                                 *  00081701
      ****************************************************************  00081801
      *                                                                 00081900
      *------------------------------------------------------------*    00082000
      * URT-TRANS-PRUEF-NEUANW                                     *    00082100
      *------------------------------------------------------------*    00082200
      * INPUT : ZI-DWAT0025-EINGABE                                *    00082300
      *         ??? (EINGABE FUER UR7, UR8)                        *    00082400
      *                                                            *    00083000
      * OUTPUT: ZO-DWAT0025-AUSGABE,                               *    00084000
      *         TRANS-SATZ                                         *    00085000
      *                                                            *    00086000
      * VERARBEITUNG:                                              *    00087000
      *   PRUEFEN TRANSAKTION IN NEUANWENDUNGEN:                   *    00088000
      *   - 'DVAG' (= HAUPTMENUE)                                  *    00089000
      *   - VORHANDEN/KOMPETENZ ETC (UEBER PGAT0012)               *    00090000
      *   - ALTANWENDUNG                                           *    00100000
      *   - UNTERBRECHUNGS-MODUS                                   *    00110000
      *                                                            *    00120000
      *------------------------------------------------------------*    00130000
       URT-TRANS-PRUEF-NEUANW SECTION.                                  00140000
       URT-ANF.                                                         00150000
           MOVE 'URT  '                   TO ERR-ORT-SEC                00160000
           PERFORM UR2-TRACE                                            00170000
      *                                                                 00180000
           INITIALIZE                        TRANS-SATZ                 00190000
           INITIALIZE                        PERS-SATZ                  00200000
           SET TR-OK                      TO TRUE                       00210000
                                                                        00220000
           PERFORM URT1-HAUPTMENUE-PRUEF                                00230000
           IF TR-OK                                                     00240000
              PERFORM URT2-KOMPETENZ-PRUEF                              00250000
           END-IF                                                       00260000
           IF TR-OK                                                     00270000
              PERFORM URT3-ALTANW-PRUEF                                 00280000
           END-IF                                                       00290000
           IF TR-OK-ALTANW    OR                                        00300000
              TR-OK-ALTANW-KS                                           00310000
                                                                        00320000
              PERFORM URT4-UMODUS-PRUEF                                 00330000
           END-IF                                                       00340000
           .                                                            00350000
       URT-EXIT.                                                        00360000
           EXIT.                                                        00370000
                                                                        00380000
      *------------------------------------------------------------*    00390000
      * URT1-HAUPTMENUE-PRUEF                                      *    00400000
      *------------------------------------------------------------*    00410000
      * INPUT : ZI-DWAT0025-TRANS-AKT, K-TRANS-HAUPTMENUE          *    00420000
      *                                                            *    00430000
      * OUTPUT: TR-STATUS                                          *    00440000
      *                                                            *    00450000
      * VERARBEITUNG:                                              *    00460000
      *     PRUEFEN, OB ZI-DWAT0025-TRANS-AKT = HAUPTMENUE         *    00470000
      *                                                            *    00480000
      *------------------------------------------------------------*    00490000
       URT1-HAUPTMENUE-PRUEF SECTION.                                   00500000
       URT1-ANF.                                                        00510000
           MOVE 'URT1 '                     TO ERR-ORT-SEC              00520000
           PERFORM UR2-TRACE                                            00530000
      *                                                                 00540000
           IF ZI-DWAT0025-TRANS-AKT = K-TRANS-HAUPTMENUE                00550000
              SET TR-OK-ALTANW-KS         TO TRUE                       00560000
           END-IF                                                       00570000
           .                                                            00580000
       URT1-EXIT.                                                       00590000
           EXIT.                                                        00600000
                                                                        00610000
      *------------------------------------------------------------*    00620000
      * URT2-KOMPETENZ-PRUEF                                       *    00630000
      *------------------------------------------------------------*    00640000
      * INPUT : ZI-DWAT0025-TRANS-AKT                              *    00650000
      *                                                            *    00660000
      * OUTPUT: TR-STATUS                                          *    00670000
      *                                                            *    00680000
      * VERARBEITUNG:                                              *    00690000
      *     AUFRUF KOMPETENZ-PRUEFUNG                              *    00700000
      *     AUSWERTEN RC                                           *    00710000
      *                                                            *    00720000
      *------------------------------------------------------------*    00730000
       URT2-KOMPETENZ-PRUEF SECTION.                                    00740000
       URT2-ANF.                                                        00750000
           MOVE 'URT2 '                     TO ERR-ORT-SEC              00760000
           PERFORM UR2-TRACE                                            00770000
      *                                                                 00780000
      *    * AUFRUF KOMPETENZPRUEFUNG                                   00790000
           MOVE ZI-DWAT0025-TRANS-AKT       TO CA-LINK-TRANSID          00800000
260894*    MOVE TS-PERSNR                   TO CA-LINK-PERSNR           00810002
260894     MOVE TS-USERID                   TO CA-LINK-USERID           00820002
                                                                        00830000
           PERFORM UR7-LINK-VORBEREITUNG                                00840000
CCC005*    EXEC CICS  LINK  PROGRAM  ('PGAT0012')                       00850000
CCC004*                     COMMAREA (CA-LINK)                          00860000
CCC004*                     LENGTH   (K-CA-LINK-LEN)                    00870000
CCC003*    END-EXEC                                                     00880000
           PERFORM UR8-LINK-NACHBEREITUNG                               00890000
                                                                        00900000
           IF ERR-RC-SCHWERER-FEHLER                                    00910000
              PERFORM UR99-SCHWERER-FEHLER                              00920000
           END-IF                                                       00930000
                                                                        00940000
      *    * AUSWERTUNG KOMPETENZPRUEFUNG                               00950000
           MOVE CA-LINK-FEHLER-NR             TO Z-KP-RC                00960000
           EVALUATE TRUE                                                00970000
           WHEN KP-OK                                                   00980000
                CONTINUE                                                00990000
                                                                        01000000
           WHEN KP-TRANS-UNGUELT                                        01010000
           WHEN KP-TRANS-NICHT-VORH                                     01020000
                SET TR-UNGUELTIG              TO TRUE                   01030000
                MOVE K-AT-PGM-NAME            TO ZO-DWAT0025-TR-MELD-PGM01040000
                MOVE K-I                      TO ZO-DWAT0025-TR-MELD-TYP01050000
                MOVE K-7                      TO ZO-DWAT0025-TR-MELD-NR 01060000
                                                                        01070000
           WHEN KP-TRANS-GESPERRT                                       01080000
                SET TR-GESPERRT               TO TRUE                   01090000
                MOVE K-AT-PGM-NAME            TO ZO-DWAT0025-TR-MELD-PGM01100000
                MOVE K-I                      TO ZO-DWAT0025-TR-MELD-TYP01110000
                MOVE K-3                      TO ZO-DWAT0025-TR-MELD-NR 01120000
                                                                        01130000
           WHEN KP-KEINE-KOMPETENZ                                      01140000
                SET TR-KEINE-KOMPETENZ        TO TRUE                   01150000
                MOVE K-AT-PGM-NAME            TO ZO-DWAT0025-TR-MELD-PGM01160000
                MOVE K-I                      TO ZO-DWAT0025-TR-MELD-TYP01170000
                MOVE K-4                      TO ZO-DWAT0025-TR-MELD-NR 01180000
                                                                        01190000
           WHEN KP-UNGUELT-BILDSCHIRM                                   01200000
                SET TR-UNGUELT-BILDSCHIRM     TO TRUE                   01210000
                MOVE K-AT-PGM-NAME            TO ZO-DWAT0025-TR-MELD-PGM01220000
                MOVE K-I                      TO ZO-DWAT0025-TR-MELD-TYP01230000
                MOVE K-8                      TO ZO-DWAT0025-TR-MELD-NR 01240000
                                                                        01250000
           WHEN OTHER                                                   01260000
                MOVE 1                        TO ERR-ORT-LFD            01270000
                PERFORM UR93-PROG-FEHLER                                01271000
           END-EVALUATE                                                 01272000
                                                                        01273000
           MOVE CA-LINK-TRANS-SATZ            TO TRANS-SATZ             01274000
           MOVE CA-LINK-PERS-SATZ             TO PERS-SATZ              01275000
           .                                                            01276000
       URT2-EXIT.                                                       01277000
           EXIT.                                                        01278000
                                                                        01279000
      *------------------------------------------------------------*    01280000
      * URT3-ALTANW-PRUEF                                          *    01290000
      *------------------------------------------------------------*    01300000
      * INPUT : ZI-DWAT0025-TRANS-AKT, TRANS-SATZ                  *    01310000
      *                                                            *    01320000
      * OUTPUT: TR-STATUS                                          *    01330000
      *                                                            *    01340000
      * VERARBEITUNG:                                              *    01350000
      *     PRUEFEN, OB TRANS EINE ALTANWENDUNG IST                *    01360000
      *                                                            *    01370000
      *------------------------------------------------------------*    01380000
       URT3-ALTANW-PRUEF SECTION.                                       01390000
       URT3-ANF.                                                        01400000
           MOVE 'URT3 '                     TO ERR-ORT-SEC              01410000
           PERFORM UR2-TRACE                                            01420000
      *                                                                 01430000
           IF K1-TRANS > ZERO                                           01440000
              IF K1-NAW-KZ NOT = K-J                                    01450000
                 SET TR-OK-ALTANW           TO TRUE                     01460000
              END-IF                                                    01470000
           ELSE                                                         01480000
      *       * LOGIK-FEHLER, DA TRANS-SATZ NICHT GEFUELLT              01490000
              MOVE 1                        TO ERR-ORT-LFD              01500000
              PERFORM UR93-PROG-FEHLER                                  01510000
           END-IF                                                       01520000
           .                                                            01530000
       URT3-EXIT.                                                       01540000
           EXIT.                                                        01550000
                                                                        01560000
      *------------------------------------------------------------*    01570000
      * URT4-UMODUS-PRUEF                                          *    01580000
      *------------------------------------------------------------*    01590000
      * INPUT : ZI-DWAT0025-USITZG-ANZ                             *    01600000
      *                                                            *    01610000
      * OUTPUT: TR-STATUS                                          *    01620000
      *                                                            *    01630000
      * VERARBEITUNG:                                              *    01640000
      *     PRUEFEN, OB WECHSEL ZU ANWVERZ ODER ZU ALTANW          *    01650000
      *     IM UNTERBRECHUNGSMODUS VERSUCHT WIRD                   *    01660000
      *                                                            *    01670000
      *------------------------------------------------------------*    01680000
       URT4-UMODUS-PRUEF SECTION.                                       01690000
       URT4-ANF.                                                        01700000
           MOVE 'URT4 '                     TO ERR-ORT-SEC              01710000
           PERFORM UR2-TRACE                                            01720000
      *                                                                 01730000
           IF ZI-DWAT0025-USITZG-ANZ > ZERO                             01740000
              MOVE K-AT-PGM-NAME            TO ZO-DWAT0025-TR-MELD-PGM  01750000
              MOVE K-I                      TO ZO-DWAT0025-TR-MELD-TYP  01760000
              IF TR-OK-ALTANW-KS                                        01770000
                 MOVE K-9                   TO ZO-DWAT0025-TR-MELD-NR   01780000
              ELSE                                                      01790000
                 MOVE K-6                   TO ZO-DWAT0025-TR-MELD-NR   01800000
              END-IF                                                    01810000
              SET TR-UNGUELT-IM-UMODUS      TO TRUE                     01820000
           END-IF                                                       01830000
           .                                                            01840000
       URT4-EXIT.                                                       01850000
           EXIT.                                                        01860000
      *                                                                 01870000
      ******* ENDE COPY-MEMBER DPAT0025 (PRUEF TRANS IN NEUANW)   ***** 01880000
