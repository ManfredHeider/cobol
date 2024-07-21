      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0004.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0004.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:08:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0004      TITEL VERSORGEN - HILFEDIALOG   * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    23.04.1993                                       * 00070002
      *    STAND:    23.04.1993                                       * 00080002
      ***************************************************************** 00090000
      *                                                                 00100000
      *    IM HILFEDIALOG WERDEN DIE 2 TITELFELDER IM MASKENKOPF WIE    00110000
      *    FOLGT VERSORGT:                                              00120000
      *    - TITEL 1: KONSTANT                                          00130000
      *    - TITEL 2: ABHÄNGIG VOM CA-AUFRUF-OP                         00140000
      *               DER TITEL 2 IST ZUSAMMEN MIT DEM AUFRUF-OP        00150000
      *               IN EINER TABELLE ABGELEGT                         00160000
      *    (ZUGEHÖRIGES COPY-ELEMENT DER WORKING-STORAGE: DWAT0004)     00170000
      *                                                                 00180000
      *------------------------------------------------------------*    00190000
      * R711-TITEL-VERSORG                                         *    00200000
      *------------------------------------------------------------*    00210000
      * INPUT : TITEL-ZEILEN-TABELLE; CA-AUFRUF-OP;                *    00220000
      *         K-DIALOG-TITEL1                                    *    00230000
      *                                                            *    00240000
      * OUTPUT: GEFUELLTE TITEL-ZEILEN IM BILDSCHIRMKOPF           *    00250000
      *                                                            *    00260000
      * VERARBEITUNG:                                              *    00270000
      *     VERSORGEN DER KOPFFELDER Z.T. ABHAENGIG VOM AUFRUF-OP  *    00280000
      *                                                            *    00290000
      *------------------------------------------------------------*    00300000
       R711-TITEL-VERSORG SECTION.                                      00310000
       R711-ANF.                                                        00320000
           MOVE 'R711 ' TO ERR-ORT-SEC                                  00330000
           PERFORM ZR1-TRACE-KURZ                                       00340001
      *                                                                 00350000
           MOVE K-DIALOG-TITEL1              TO ARTITL1O                00360000
            MOVE DFHBMASK                    TO ARTITL1A                00370000
                                                                        00380000
           SET I-TITEL2                      TO 1                       00390000
           SEARCH T-T2-ZEILE                                            00400000
                  VARYING I-TITEL2                                      00410000
           AT END                                                       00420000
              MOVE SPACE                     TO ARTITL2O                00430000
           WHEN T-TITEL2-OP(I-TITEL2) = CA-AUFRUF-OP                    00440000
              MOVE T-TITEL2-TEXT(I-TITEL2)   TO ARTITL2O                00450000
           END-SEARCH                                                   00460000
                                                                        00470000
           MOVE DFHBMASK                     TO ARTITL2A                00480000
           .                                                            00490000
       R711-EXIT.                                                       00500000
           EXIT.                                                        00510000
      *                                                                 00520000
      ******* ENDE COPY-MEMBER DPAT0004 (TITEL - HILFEDIALOG) ********* 00530000
CCC009*    EJECT                                                        00540000
