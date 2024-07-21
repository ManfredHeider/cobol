      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0005.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0005.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:11:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0005      ERR-TRACE FUELLEN               * 00020001
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    23.02.1993                                       * 00070000
      *    STAND:    02.03.1993                                       * 00080004
      ***************************************************************** 00090000
      *                                                                 00100000
      *    HIER WIRD DIE AKTUELLE SECTION IN ERR-TRACE EINGETRAGEN.     00110000
      *    ZUGEHÖRIGE FELDER DER WS: DWAT0005                           00120001
      *                                                                 00130000
      *------------------------------------------------------------*    00140000
      * UR2-TRACE                                                  *    00150000
      *------------------------------------------------------------*    00160000
      * INPUT : ERR-ORT-SEC, ERR-TRACE                             *    00170000
      *                                                            *    00180000
      * OUTPUT: ERR-TRACE                                          *    00190000
      *                                                            *    00200000
      * VERARBEITUNG:                                              *    00210000
      *   - DER UEBERGEBENE ERR-ORT-SEC WIRD HINTEN IN ERR-TRACE   *    00220000
      *     EINGEFUEGT                                             *    00230000
      *                                                            *    00240000
      *------------------------------------------------------------*    00250000
       UR2-TRACE SECTION.                                               00260000
       UR2-ANF.                                                         00270000
      *                                                                 00280000
           MOVE ERR-TRACE-HINTEN        TO Z-DWAT0005-ERR-TRACE         00290002
           MOVE Z-DWAT0005-ERR-TRACE    TO ERR-TRACE-VORN               00300002
           MOVE ERR-ORT-SEC             TO ERR-TRACE-NEU                00310000
           .                                                            00320000
       UR2-EXIT.                                                        00330000
           EXIT.                                                        00340000
      *                                                                 00350000
      ******* ENDE COPY-MEMBER DPAT0005 (ERR-TRACE FUELLEN) ********    00360001
CCC009*    EJECT                                                        00370000
