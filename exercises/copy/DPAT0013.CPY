      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0013.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0013.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:12:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0013           SQL-FEHLER-BEHANDLUNG      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    24.03.1993                                       * 00070000
      *    STAND:    26.03.1993                                       * 00080001
      ***************************************************************** 00090000
      *                                                                 00100000
      *    SQL-FEHLER-ROUTINE, DIE BEI SQL-FEHLER IM HAUPTPROGRAMM      00110000
      *    BENOETIGT WIRD                                               00120000
      *                                                                 00130000
      *------------------------------------------------------------*    00140000
      * UR91-SQL-FEHLER                                            *    00150000
      *------------------------------------------------------------*    00160000
      * INPUT : SQLCODE, ERR-ORT,                                  *    00170000
      *         EVTL. ERR-VAR                                      *    00180000
      *                                                            *    00190000
      * OUTPUT: - (KEINE RUECKKEHR ZUM AUFRUFER)                   *    00200000
      *                                                            *    00210000
      * VERARBEITUNG:                                              *    00220000
      *   - FUELLEN DER ERROR-SCHNITTSTELLE                        *    00230000
      *   - AUFRUF VON UR99-SCHWERER-FEHLER                        *    00240000
      *                                                            *    00250000
      *------------------------------------------------------------*    00260000
       UR91-SQL-FEHLER SECTION.                                         00270000
       UR91-ANF.                                                        00280000
      *                                                                 00290000
      *    * VERSORGEN SCHNITTSTELLE                                    00300000
           SET ERR-FTYP-SQL                TO TRUE                      00310000
           MOVE SQLCODE                    TO ERR-SQLCODE               00320000
           IF SQLERRML > ZERO                                           00321001
              MOVE SQLERRMC                TO ERR-SQLERRMC              00330001
           END-IF                                                       00331001
           PERFORM UR99-SCHWERER-FEHLER                                 00340000
           .                                                            00350000
       UR91-EXIT.                                                       00360000
           EXIT.                                                        00370000
      *                                                                 02350000
      ******* ENDE COPY-MEMBER DPAT0013 (SQL-FEHLER-BEHANDLUNG) ****    02360000
CCC009*    EJECT                                                        02370000
