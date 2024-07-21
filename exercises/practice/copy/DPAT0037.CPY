      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0037.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0037.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:17:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0037                                      * 00020004
      *    --------------------------------------------------------   * 00030000
      *    BATCHRAHMEN: FEHLERBEHANDLUNG FÜR DB2-SCHREIBMODULN        * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00060000
      *    DATUM:    19.08.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    14.09.94 16:35:03
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0144!19.08.94!    ! NEU                       ! SCHÖLLH. *  00140006
      *         !06.09.94! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  00140106
      *         !        !    !                           !          *  00141000
      *         !        !    !                           !          *  00142000
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00144000
      *                                                              *  00145000
      * ENDE-LOGBUCH                                                 *  00146000
      ****************************************************************  00147000
      *                                                                 00148000
      *------------------------------------------------------------*    00148100
       BR95-DB2S-FEHLER SECTION.                                        00148205
       BR95-ANF.                                                        00148305
      *------------------------------------------------------------*    00148400
      * INPUT : MUSS: K-PGM-NAME, PARAMETER-STEUER,PARAMETER-RETURN*    00148501
      *         KANN: ERR-ORT-SEC, ERR-ORT-LFD, ERR-VAR-ZEILE01 /04*    00148701
      * OUTPUT: FEHLERBILD (ERRIPRO) MIT INFOS ZU DB2S-FEHLER      *    00148800
      * VERARBEITUNG:                                              *    00149000
      *   - FUELLEN DER ERROR-SCHNITTSTELLE                        *    00149100
      *   - AUFRUF VON BR99-SCHWERER-FEHLER-DISPLAY                *    00149205
      *------------------------------------------------------------*    00149400
      *                                                                 00149500
      *    * VERSORGEN SCHNITTSTELLE                                    00149600
      *                                                                 00149700
           MOVE K-PGM-NAME                 TO ERR-PROGNAME              00149803
           EVALUATE PR-FEHLER-TYP                                       00149900
           WHEN 'R'                                                     00150000
              SET ERR-FTYP-ROLLBACK        TO TRUE                      00151000
              MOVE PR-FEHLER-NR            TO ERR-SQLCODE               00160000
              MOVE PR-FEHLER-TEXT          TO ERR-SQLERRMC              00170000
           WHEN 'D'                                                     00180000
              SET ERR-FTYP-SQL             TO TRUE                      00190000
              MOVE PR-FEHLER-NR            TO ERR-SQLCODE               00191000
              MOVE PR-FEHLER-TEXT          TO ERR-SQLERRMC              00192000
           WHEN OTHER                                                   00194000
      *       * AUCH: WHEN 'L'                                          00194103
              SET ERR-FTYP-PROG            TO TRUE                      00195000
           END-EVALUATE                                                 00196000
                                                                        00197000
      *    * VORBEREITUNG AUSGABE DER DB2-SCHREIBMODUL-SCHNITTSTELLE    00198000
           SET ERR-AUFBER-DB2S             TO TRUE                      00199000
           MOVE PARAMETER-STEUER           TO ERR-PARAMETER-STEUER      00199100
           MOVE PARAMETER-RETURN           TO ERR-PARAMETER-RETURN      00199200
                                                                        00199400
           PERFORM BR99-SCHWERER-FEHLER-DISPLAY                         00199505
           .                                                            00199600
       BR95-EXIT.                                                       00199705
           EXIT.                                                        00199800
      *                                                                 00230000
      ******** ENDE COPY-MEMBER (DPAT0037) *******************          00240004
                                                                        00250000
