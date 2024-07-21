      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0018.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0018.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:14:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010001
      *    COPY-MEMBER: DPAT0018       SERVICE FUER TRANSDAT-ZUGRIFFE * 00020001
      *    --------------------------------------------------------   * 00030001
      *    VERSION   001                                              * 00040001
      *    --------------------------------------------------------   * 00050001
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060001
      *    DATUM:    01.04.1993                                       * 00070001
      *    STAND:    15.04.1993                                       * 00080005
      ***************************************************************** 00090001
      *                                                                 00100001
      *    ZUGRIFFE AUF TRANSDAT (ANWENDUNGS-DATEI)                     00110001
      *    (DETAILS VGL. DWAT0018)                                      00120001
      *                                                                 00130001
      *------------------------------------------------------------*    00140001
      * UR5-TRANSDAT-SERVICE                                       *    00150003
      *------------------------------------------------------------*    00160001
      * INPUT : ZI-DWAT0018-INPUT                                  *    00170001
      *         ZIO-DWAT0018-INOUT                                 *    00171006
      *                                                            *    00180001
      * OUTPUT: ZO-DWAT0018-OUTPUT                                 *    00190001
      *         ZIO-DWAT0018-INOUT                                 *    00191006
      *                                                            *    00200001
      * VERARBEITUNG:                                              *    00210001
      *   ABHAENGIG VON ZI-DWAT0018-TD-SERVICE                     *    00220001
      *   ZUGRIFF AUF PERSDAT UND VERSORGEN OUTPUT-BEREICH         *    00230001
      *                                                            *    00240001
      * (DETAILS VGL. DWAT0018)                                    *    00250001
      *                                                            *    00260001
      *------------------------------------------------------------*    00270001
       UR5-TRANSDAT-SERVICE SECTION.                                    00280003
       UR5-ANF.                                                         00290003
           MOVE 'UR5 '                     TO ERR-ORT-SEC               00300003
           PERFORM ZR1-TRACE-KURZ                                       00310001
      *                                                                 00320001
           SET TD-RC-OK                    TO TRUE                      00330007
                                                                        00340001
           EVALUATE TRUE                                                00350001
      *     WHEN TD-SERVICE-STARTBR-EQUAL                               00360001
      *        PERFORM UR51-TD-STARTBR-EQUAL                            00370003
      *     WHEN TD-SERVICE-STARTBR-GTEQ                                00390001
      *        PERFORM UR52-TD-STARTBR-GTEQ                             00400003
      *     WHEN TD-SERVICE-ENDBR                                       00410001
      *        PERFORM UR53-TD-ENDBR                                    00420003
      *     WHEN TD-SERVICE-READNEXT                                    00430001
      *        PERFORM UR54-TD-READNEXT                                 00440003
      *     WHEN TD-SERVICE-READPREV                                    00460001
      *        PERFORM UR54-TD-READPREV                                 00470003
                                                                        00480001
           WHEN TD-SERVICE-READ                                         00490001
              PERFORM UR56-TD-READ                                      00500003
                                                                        00510001
      *     WHEN TD-SERVICE-READUPDATE                                  00520001
      *        PERFORM UR57-TD-READUPDATE                               00530008
      *     WHEN TD-SERVICE-REWRITE                                     00540001
      *        PERFORM UR58-TD-REWRITE                                  00550008
      *     WHEN TD-SERVICE-WRITE                                       00560001
      *        PERFORM UR59-TD-WRITE                                    00570008
                                                                        00580001
           WHEN OTHER                                                   00590001
      *       * FALSCHER TD-SERVICE                                     00600001
              MOVE 5                       TO ERR-ORT-LFD               00610009
      *       MOVE ???                     TO ERR-MELD-NR               00620002
              PERFORM UR93-PROG-FEHLER                                  00630001
           END-EVALUATE                                                 00640001
           .                                                            00650001
       UR5-EXIT.                                                        00660003
           EXIT.                                                        00670001
                                                                        01650001
      *------------------------------------------------------------*    01660001
      * UR56-TD-READ                                               *    01670003
      *------------------------------------------------------------*    01680001
      * INPUT : ZI-DWAT0018-INPUT                                  *    01690001
      *         ZIO-DWAT0018-INOUT                                 *    01691006
      *                                                            *    01700001
      * OUTPUT: ZO-DWAT0018-OUTPUT                                 *    01710001
      *         ZIO-DWAT0018-INOUT                                 *    01711006
      *                                                            *    01720001
      * VERARBEITUNG:                                              *    01730001
      *   - READ SATZ AUS PERSDAT                                  *    01740001
      *                                                            *    01750001
      *------------------------------------------------------------*    01760001
       UR56-TD-READ SECTION.                                            01770003
       UR56-ANF.                                                        01780003
           MOVE 'UR56 '                    TO ERR-ORT-SEC               01790003
           PERFORM ZR1-TRACE-KURZ                                       01800001
      *                                                                 01810001
CCC005*    EXEC CICS READ     INTO    (ZIO-DWAT0018-TD-DATA)            01820006
CCC004*                       DATASET ('DD0001')                        01830004
CCC004*                       RIDFLD  (ZIO-DWAT0018-TD-KEY)             01840006
CCC004*                       RESP    (C-CICS-RC)                       01850001
CCC003*    END-EXEC                                                     01860001
           IF C-CICS-RC NOT = DFHRESP(NORMAL)                           01870001
              IF C-CICS-RC = DFHRESP(NOTFND)                            01880001
                 SET TD-RC-NOTFND          TO TRUE                      01890001
              ELSE                                                      01900001
                 MOVE 56                      TO ERR-ORT-LFD            01910009
                 MOVE ZI-DWAT0018-TD-SERVICE  TO ERR-VAR                01911008
                 MOVE ZIO-DWAT0018-TD-KEY     TO ERR-VAR(4:6)           01912008
                 PERFORM ZR92-CICS-FEHLER                               01920001
              END-IF                                                    01930001
           END-IF                                                       01940001
           .                                                            01950001
       UR56-EXIT.                                                       01960003
           EXIT.                                                        01970001
      *                                                                 01980001
      ******* ENDE COPY-MEMBER DPAT0018 (SERVICE FÜR TRANS-DAT) ******* 01990001
CCC009*    EJECT                                                        02000001
