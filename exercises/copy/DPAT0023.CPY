      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0023.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0023.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:15:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0023      TRACE ALTANWENDUGEN             * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    04.05.1993                                       * 00070000
      *    STAND:    04.05.1993                                       * 00080000
      ***************************************************************** 00090000
      *                                                                 00100000
      *    PARAGRAPHEN:                                                 00110000
      *                                                                 00120000
      *    1. TRACE-ALTANW-LABEL                                        00130000
      *       --> INPUT: Z-DWAT0023-TRC-LABEL (MAX. 10 BYTES)           00140000
      *       --> TRACE-LABEL IN Z-DWAT0023-TRCV EINGETRAGEN            00150000
      *                                                                 00160000
      *    2. TRACE-ALTANW-VAR                                          00170000
      *       --> INPUT: Z-DWAT0023-TRC-V80   (MAX. 80 BYTES)           00180000
      *       --> TRACE-VAR   IN Z-DWAT0023-TRCV EINGETRAGEN            00190000
      *                                                                 00200000
      *    3. TRACE-AW-SEND-RECEIVE                                     00210000
      *       --> TRACE SENDEN (GESAMTER BILDSCHIRM)                    00220000
      *       --> RECEIVE (UM NACH AUSGABE ZU STOPPEN)                  00230000
      *                                                                 00240000
      *    BEMERKUNG:                                                   00250000
      *                                                                 00260000
      *    DIESE PARAGRAPHEN SIND IN EINE SECTION EINGEHÜLLT, DAMIT     00270000
      *    ES IN DEN ALTANWENDUNGEN NICHT ZU FEHLERN KOMMEN KANN.       00280000
      *                                                                 00290000
      *    -----------------                                            00300000
      *    BEISPIEL-AUFRUFE:                                            00310000
      *    -----------------                                            00320000
      *                                                                 00330000
      *    MOVE '(LABEL)   '              TO Z-DWAT0023-TRC-LABEL       00340000
      *    PERFORM TRACE-ALTANW-LABEL                                   00350000
      *                                                                 00360000
      *    MOVE (DRUCKBARE FELDINHALTE)   TO Z-DWAT0023-TRC-V80         00370000
      *    PERFORM TRACE-ALTANW-VAR                                     00380000
      *                                                                 00390000
     T*    PERFORM TRACE-AW-SEND-RECEIVE                                00400000
      *                                                                 00410000
      *                                                                 00420000
       TRACE-DUMMY SECTION.                                             00430000
       TRACE-ALTANW-LABEL.                                              00440000
      *                                                                 00450000
           MOVE Z-DWAT0023-TRCL-HINTEN  TO Z-DWAT0023-TRCL-ZWI          00460000
           MOVE Z-DWAT0023-TRCL-ZWI     TO Z-DWAT0023-TRCL-VORN         00470000
           MOVE Z-DWAT0023-TRC-LABEL    TO Z-DWAT0023-TRCL-NEU          00480000
           .                                                            00490000
      *                                                                 00500000
       TRACE-ALTANW-VAR.                                                00510000
      *                                                                 00520000
           MOVE Z-DWAT0023-TRCV-HINTEN  TO Z-DWAT0023-TRCV-ZWI          00530000
           MOVE Z-DWAT0023-TRCV-ZWI     TO Z-DWAT0023-TRCV-VORN         00540000
           MOVE Z-DWAT0023-TRC-V80      TO Z-DWAT0023-TRCV-NEU          00550000
           .                                                            00560000
      *                                                                 00570000
       TRACE-AW-SEND-RECEIVE.                                           00580000
      *                                                                 00590000
CCC005*    EXEC CICS SEND                                               00600000
CCC004*              FROM   (Z-DWAT0023-TRC-GES)                        00610000
CCC004*              LENGTH (K-DWAT0023-TRC-LEN)                        00620000
CCC004*              ERASE                                              00630000
CCC004*              RESP   (Z-DWAT0023-RESP)                           00640000
CCC003*    END-EXEC                                                     00650000
           IF Z-DWAT0023-RESP NOT = DFHRESP(NORMAL)                     00660000
              CONTINUE                                                  00670000
           END-IF                                                       00680000
CCC005*    EXEC CICS RECEIVE                                            00690000
CCC004*              RESP   (Z-DWAT0023-RESP)                           00700000
CCC003*    END-EXEC                                                     00710000
           IF Z-DWAT0023-RESP NOT = DFHRESP(NORMAL)                     00720000
              CONTINUE                                                  00730000
           END-IF                                                       00740000
           .                                                            00750000
       TRACE-DUMMY-EXIT.                                                00760000
           EXIT.                                                        00770000
      *                                                                 00780000
      ******* ENDE COPY-MEMBER DPAT0023 (TRACE ALTANWENDUGEN)     ***** 00790000
