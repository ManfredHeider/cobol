      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0002.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0002.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:07:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0002       FEHLERBEARB DB2-SCHREIBMODULN  * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   001                                              * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    15.02.1993                                       * 00070000
      *    STAND:    16.03.1993                                       * 00080003
      ***************************************************************** 00090000
      *                                                                 00100000
      *------------------------------------------------------------*    00110000
      * UR95-DB2S-FEHLER                                           *    00120000
      *------------------------------------------------------------*    00130000
      * INPUT : PARAMETER-RETURN, ERR-ORT,                         *    00140000
      *         EVTL. ERR-VAR                                      *    00150000
      *                                                            *    00160000
      * OUTPUT: - (KEINE RUECKKEHR ZUM AUFRUFER)                   *    00170000
      *                                                            *    00180000
      * VERARBEITUNG:                                              *    00190000
      *   - FUELLEN DER ERROR-SCHNITTSTELLE                        *    00200000
      *   - AUFRUF VON UR99-SCHWERER-FEHLER                        *    00210000
      *                                                            *    00220000
      *------------------------------------------------------------*    00230000
       UR95-DB2S-FEHLER SECTION.                                        00240000
       UR95-ANF.                                                        00250000
      *                                                                 00260000
      *    * VERSORGEN SCHNITTSTELLE                                    00270000
      *                                                                 00280002
           EVALUATE PR-FEHLER-TYP                                       00290002
           WHEN 'R'                                                     00300002
              SET ERR-FTYP-ROLLBACK        TO TRUE                      00310002
              MOVE PR-FEHLER-NR            TO ERR-SQLCODE               00320002
              MOVE PR-FEHLER-TEXT          TO ERR-SQLERRMC              00330002
           WHEN 'D'                                                     00340002
              SET ERR-FTYP-SQL             TO TRUE                      00350002
              MOVE PR-FEHLER-NR            TO ERR-SQLCODE               00360002
              MOVE PR-FEHLER-TEXT          TO ERR-SQLERRMC              00370002
      *AUCH: WHEN 'L'                                                   00380003
           WHEN OTHER                                                   00390002
              SET ERR-FTYP-PROG            TO TRUE                      00400002
           END-EVALUATE                                                 00410002
                                                                        00420002
      *    * VORBEREITUNG AUSGABE DER DB2-SCHREIBMODUL-SCHNITTSTELLE    00430002
           SET ERR-AUFBER-DB2S             TO TRUE                      00440002
           MOVE PARAMETER-STEUER           TO ERR-PARAMETER-STEUER      00450002
           MOVE PARAMETER-RETURN           TO ERR-PARAMETER-RETURN      00460002
           MOVE K-22                       TO ERR-MELD-NR               00470002
                                                                        00480000
           PERFORM UR99-SCHWERER-FEHLER                                 00490000
           .                                                            00500000
       UR95-EXIT.                                                       00510000
           EXIT.                                                        00520000
      *                                                                 00530000
      ******* ENDE COPY-MEMBER DPAT0002 (FEHLERBEARB DB2-SCHREIBM.) *** 00540000
CCC009*    EJECT                                                        00550000
