      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0003.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0003.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:07:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0003      TITEL VERSORGEN - STANDARD      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    15.02.1993                                       * 00070000
      *    STAND:    24.03.1993                                       * 00080001
      ***************************************************************** 00090000
      *                                                                 00100000
      *    IM STANDARD-DIALOG (FESTE DIALOG-TITEL) WERDEN DIE           00110000
      *    TITEL-FELDER AUS PROGRAMMABHÄNGIGEN KONSTANTEN VERSORGT.     00120000
      *    DIE KONSTANTEN SIND IN JEDEM PROGRAMM IN DER                 00130000
      *    WORKING-STAROAGE ZU FÜLLEN.                                  00140000
      *                                                                 00150000
      *------------------------------------------------------------*    00160000
      * R711-TITEL-VERSORG                                         *    00170000
      *------------------------------------------------------------*    00180000
      * INPUT : K-DIALOG-TITEL1, K-DIALOG-TITEL2                   *    00190000
      *                                                            *    00200000
      * OUTPUT: GEFUELLTE TITEL-ZEILEN IM BILDSCHIRMKOPF           *    00210000
      *                                                            *    00220000
      * VERARBEITUNG:                                              *    00230000
      *     VERSORGEN DER KOPFFELDER AUS PROGRAMMKONSTANTEN        *    00240000
      *                                                            *    00250000
      *------------------------------------------------------------*    00260000
       R711-TITEL-VERSORG SECTION.                                      00270000
       R711-ANF.                                                        00280000
           MOVE 'R711 ' TO ERR-ORT-SEC                                  00290000
           PERFORM ZR1-TRACE-KURZ                                       00300001
      *                                                                 00310000
           MOVE K-DIALOG-TITEL1              TO ARTITL1O                00320000
            MOVE DFHBMASK                    TO ARTITL1A                00330000
           MOVE K-DIALOG-TITEL2              TO ARTITL2O                00340000
            MOVE DFHBMASK                    TO ARTITL2A                00350000
           .                                                            00360000
       R711-EXIT.                                                       00370000
           EXIT.                                                        00380000
      *                                                                 00390000
      ******* ENDE COPY-MEMBER DPAT0003 (TITEL - STANDARD) *********    00400000
CCC009*    EJECT                                                        00410000
