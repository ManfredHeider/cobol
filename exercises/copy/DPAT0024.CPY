      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0024.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0024.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:15:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0024      UPRO: DB2S-FEHLER-BEHANDLUNG    * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: W. SCHÖLLHAMMER (INTEGRATA AG)                   * 00060000
      *    DATUM:    23.04.1993                                       * 00070000
      ***************************************************************** 00090000
      *    VERSION   02   16.09.93                                   *  00090101
      ****************************************************************  00091000
      * AENDERUNGEN:                                                 *  00092000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00093000
      * -------------------------------------------------------------*  00094000
      *         !24.04.93! 01 ! NEU                       ! SCHOELLH.*  00095000
      *         !16.09.93! 02 ! VERSORGT: ERR-UPRONAME    ! SCHOELLH.*  00096001
      *         !        !    !                           !          *  00097000
      *         !        !    !                           !          *  00098000
      *         !        !    !                           !          *  00099000
      *         !        !    !                           !          *  00099100
      *                                                              *  00099200
      * ENDE-LOGBUCH                                                 *  00099300
      ****************************************************************  00099400
      *                                                                 00100000
      *    FEHLERBEHANDLUNGS-ROUTINE, DIE IN EINEM UPRO BEI AUFTRETEN   00110000
      *    EINES DB2S-FEHLERS (DB2-SCHREIBMODUL-AUFRUF) BENOETIGT WIRD  00120000
      *                                                                 00130000
      *------------------------------------------------------------*    00140000
      * XR95-UPRO-DB2S-FEHLER                                      *    00150000
      *------------------------------------------------------------*    00160000
      * INPUT : PARAMETER-STEUER, PARAMETER-RETURN, ERR-ORT,       *    00231000
      *                                                            *    00233000
      * OUTPUT: GEFUELLTER ERR-ERROR-BEREICH                       *    00234100
      *                                                            *    00235000
      * VERARBEITUNG:                                              *    00236000
      *   - FUELLEN DER ERROR-SCHNITTSTELLE FUER UPRO-CICS-FEHLER  *    00236100
      *                                                            *    00239000
      *------------------------------------------------------------*    00240000
       XR95-UPRO-DB2S-FEHLER SECTION.                                   00250000
       XR95-ANF.                                                        00260000
      *                                                                 00270000
      *    * VERSORGEN SCHNITTSTELLE                                    00280000
      *                                                                 00280100
           SET ERR-RC-SCHWERER-FEHLER      TO TRUE                      00280200
           MOVE K-PGM-NAME                 TO ERR-UPRONAME              00280300
                                                                        00280400
           EVALUATE PR-FEHLER-TYP                                       00280500
           WHEN 'R'                                                     00280600
              SET ERR-FTYP-ROLLBACK        TO TRUE                      00280700
              MOVE PR-FEHLER-NR            TO ERR-SQLCODE               00280800
              MOVE PR-FEHLER-TEXT          TO ERR-SQLERRMC              00280900
           WHEN 'D'                                                     00281000
              SET ERR-FTYP-SQL             TO TRUE                      00281100
              MOVE PR-FEHLER-NR            TO ERR-SQLCODE               00281200
              MOVE PR-FEHLER-TEXT          TO ERR-SQLERRMC              00281300
      *AUCH: WHEN 'L'                                                   00281400
           WHEN OTHER                                                   00281500
              SET ERR-FTYP-PROG            TO TRUE                      00281600
           END-EVALUATE                                                 00281700
                                                                        00281800
      *    * VORBEREITUNG AUSGABE DER DB2-SCHREIBMODUL-SCHNITTSTELLE    00281900
           SET ERR-AUFBER-DB2S             TO TRUE                      00282000
           MOVE PARAMETER-STEUER           TO ERR-PARAMETER-STEUER      00282100
           MOVE PARAMETER-RETURN           TO ERR-PARAMETER-RETURN      00282200
           MOVE K-22                       TO ERR-MELD-NR               00282300
                                                                        00282400
           .                                                            00282500
       XR95-EXIT.                                                       00282600
           EXIT.                                                        00282700
      *                                                                 00282800
      ******* ENDE COPY-MEMBER DPAT0024 (UPRO: DB2S-FEHLER)  ********   00282900
CCC009*    EJECT                                                        00283000
