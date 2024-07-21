      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0020.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0020.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:14:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0020  SERVICE FUER LINK-VOR/NACHBEREITUNG * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    03.04.1993                                       * 00070000
      *    STAND:    14.04.1993                                       * 00080005
      ***************************************************************** 00090000
      *                                                                 00100000
      *    VORBEREITUNG UND NACHBEREITUNG VON LINK-AUFRUFEN             00110000
      *                                                                 00120000
      *    VORAUSSETZUNG: VERWENDUNG VON CA-LINK (DRAT0037)             00130000
      *    -------------                                                00140000
      *                                                                 00152004
      *                                                                 00153004
      *------------------------------------------------------------*    00160000
      * UR7-LINK-VORBEREITUNG                                      *    00170000
      *------------------------------------------------------------*    00180000
      * INPUT : ERR-ERROR-BEREICH, CARU-TSQLKE-NAME                *    00190005
      *                                                            *    00200000
      * OUTPUT: EINTRAG IN TSQ                                     *    00210004
      *                                                            *    00220000
      * VERARBEITUNG:                                              *    00230000
      *   - VERSORGEN CA-LINK-RAHMEN                               *    00240000
      *   - SCHREIBEN ERR-ERROR-BEREICH IN TSQ FUER LINK-ERR       *    00250004
      *                                                            *    00260000
      *------------------------------------------------------------*    00270000
       UR7-LINK-VORBEREITUNG SECTION.                                   00280000
       UR7-ANF.                                                         00290000
           MOVE 'UR7 '                     TO ERR-ORT-SEC               00300000
           PERFORM UR2-TRACE                                            00310007
      *                                                                 00320000
      *    * VERSORGEN FELDER IN CA-LINK-RAHMEN                         00330000
                                                                        00340000
           MOVE CARU-TSQLKE-NAME IN C-COMMAREA TO Z-TSQ-NAME            00341006
           SET  CA-LKR-KZN-NEUANW              TO TRUE                  00350006
           MOVE Z-TSQ-NAME                     TO CA-LKR-TSQLKE-NAME    00360106
           MOVE CARU-S-TRACELEN IN C-COMMAREA  TO CA-LKR-S-TRACELEN     00361006
                                                                        00370000
      *    * LOESCHEN TSQ LINK-ERR                                      00380000
                                                                        00390000
CCC005*    EXEC CICS DELETEQ TS                                         00420000
CCC004*              QUEUE (Z-TSQ-NAME)                                 00430000
CCC004*              RESP  (C-CICS-RC)                                  00440000
CCC003*    END-EXEC                                                     00450000
           IF C-CICS-RC NOT = DFHRESP(NORMAL)                           00460000
              IF C-CICS-RC = DFHRESP(QIDERR)                            00470000
      *          * KEINE TS-QUEUE VORHANDEN --> OK                      00480000
                 CONTINUE                                               00490000
              ELSE                                                      00500000
                 MOVE K-1                  TO ERR-ORT-LFD               00510000
                 PERFORM ZR92-CICS-FEHLER                               00520000
              END-IF                                                    00530000
           END-IF                                                       00540000
                                                                        00550000
      *    * SCHREIBEN IN TSQ LINK-ERR                                  00560000
                                                                        00570000
CCC005*    EXEC CICS WRITEQ TS                                          00590000
CCC004*              QUEUE  (Z-TSQ-NAME)                                00600000
CCC004*              FROM   (ERR-ERROR-BEREICH)                         00610000
CCC004*              MAIN                                               00620000
CCC004*              RESP   (C-CICS-RC)                                 00630000
CCC003*    END-EXEC                                                     00640000
           IF C-CICS-RC NOT = DFHRESP(NORMAL)                           00650000
              MOVE K-2                     TO ERR-ORT-LFD               00660000
              PERFORM ZR92-CICS-FEHLER                                  00670000
           END-IF                                                       00680000
           .                                                            00690000
       UR7-EXIT.                                                        00700000
           EXIT.                                                        00710000
      *                                                                 00720000
      *------------------------------------------------------------*    00730000
      * UR8-LINK-NACHBEREITUNG                                     *    00740000
      *------------------------------------------------------------*    00750000
      * INPUT : EINTRAG IN TSQ, CARU-TSQLKE-NAME                   *    00760005
      *                                                            *    00770000
      * OUTPUT: ERR-ERROR-BEREICH                                  *    00780000
      *                                                            *    00790000
      * VERARBEITUNG:                                              *    00800000
      *   LESEN ERR-ERROR-BEREICH AUS TSQ FUER LINK-ERR            *    00810004
      *                                                            *    00820000
      *------------------------------------------------------------*    00830000
       UR8-LINK-NACHBEREITUNG SECTION.                                  00840000
       UR8-ANF.                                                         00850000
      *    --> NICHT SINNVOLL VOR RUECKUEBERTRAGUNG ERR-ERROR-BEREICH   00860000
      *    MOVE 'UR8 '                     TO ERR-ORT-SEC               00870000
      *    PERFORM UR2-TRACE                                            00880007
      *                                                                 00890000
      *    * LESEN TSQ LINK-ERR                                         00900000
                                                                        00910000
           MOVE CARU-TSQLKE-NAME IN C-COMMAREA TO Z-TSQ-NAME            00920006
           MOVE 1                              TO Z-TSQ-ITEM            00930006
CCC005*    EXEC CICS READQ TS                                           00940000
CCC004*              QUEUE (Z-TSQ-NAME)                                 00950000
CCC004*              INTO  (ERR-ERROR-BEREICH)                          00960000
CCC004*              ITEM  (Z-TSQ-ITEM)                                 00970000
CCC004*              RESP  (C-CICS-RC)                                  00980000
CCC003*    END-EXEC                                                     00990000
           IF C-CICS-RC NOT = DFHRESP(NORMAL)                           01000000
              MOVE 'UR8 '                  TO ERR-ORT-SEC               01010000
              PERFORM UR2-TRACE                                         01020007
              MOVE K-1                     TO ERR-ORT-LFD               01030000
              PERFORM ZR92-CICS-FEHLER                                  01040000
           END-IF                                                       01050000
           .                                                            01060000
       UR8-EXIT.                                                        01070000
           EXIT.                                                        01080000
      *                                                                 01090000
      ******* ENDE COPY-MEMBER DPAT0020 (LINK-VOR/NACHBEREITUNG) ****** 01100000
CCC009*    EJECT                                                        01110000
