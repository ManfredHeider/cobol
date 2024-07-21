      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWAT0005.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DWAT0005.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:30:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DWAT0005      ERR-TRACE FUELLEN               * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    23.02.1993                                       * 00070000
      *    STAND:    23.02.1993                                       * 00080000
      ***************************************************************** 00090000
      *                                                                 00100000
      *    ZWISCHENFELDER FUER FUELLEN ERR-TRACE (NACHSCHIEBEN DES      00110000
      *    AKTUELLEN SECTION-KÜRZELS)                                   00120000
      *    (ZUGEHÖRIGES COPY-ELEMENT DER PROCEDURE DIV.: DPAT0005)      00170000
      *                                                                 00180000
       01  Z-DWAT0005-FELDER.                                           00300000
                                                                        00350000
      *   ZWISCHENFELD FUER TRACE                                       00360000
           05  Z-DWAT0005-ERR-TRACE           PIC  X(225).              00370000
      *                                                                 00497400
      ******* ENDE COPY-MEMBER DWAT0005 (ERR-TRACE )          ********* 00497500
CCC009*    EJECT                                                        00498000
