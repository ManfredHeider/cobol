      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0014.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0014.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:13:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0014          PROG-FEHLER-BEHANDLUNG      * 00020001
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    24.03.1993                                       * 00070000
      *    STAND:    26.03.1993                                       * 00080001
      ***************************************************************** 00090000
      *                                                                 00100000
      *    LOG. PROGRAMM-FEHLER-ROUTINE, DIE BEI AUFTRETEN EINES        00110000
      *    LOGISCHEN FEHLERS IN HAUPTPROGRAMMEN BENOETIGT WIRD          00120000
      *                                                                 00130000
      *------------------------------------------------------------*    00630000
      * UR93-PROG-FEHLER                                           *    00640000
      *------------------------------------------------------------*    00650000
      * INPUT : ERR-MELD-NR, ERR-ORT,                              *    00660000
      *         EVTL. ERR-VAR                                      *    00670000
      *                                                            *    00680000
      * OUTPUT: - (KEINE RUECKKEHR ZUM AUFRUFER)                   *    00690000
      *                                                            *    00700000
      * VERARBEITUNG:                                              *    00710000
      *   - FUELLEN DER ERROR-SCHNITTSTELLE                        *    00720000
      *   - AUFRUF VON UR99-SCHWERER-FEHLER                        *    00730000
      *                                                            *    00740000
      *------------------------------------------------------------*    00750000
       UR93-PROG-FEHLER SECTION.                                        00760000
       UR93-ANF.                                                        00770000
      *                                                                 00780000
      *    * VERSORGEN SCHNITTSTELLE                                    00790000
           SET ERR-FTYP-PROG               TO TRUE                      00800000
           PERFORM UR99-SCHWERER-FEHLER                                 00810000
           .                                                            00820000
       UR93-EXIT.                                                       00830000
           EXIT.                                                        00840000
      *                                                                 02350000
      ******* ENDE COPY-MEMBER DPAT0014 (PROG-FEHLER-BEHANDLUNG) ****   02360000
CCC009*    EJECT                                                        02370000
