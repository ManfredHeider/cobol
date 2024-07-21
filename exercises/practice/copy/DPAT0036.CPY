      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0036.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0036.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Aug 26 1996 09:46:30   T13EX16
      *AFPA0002 Anpassungen wegen der Workbench
      *
      *   Rev 1.0   Nov 04 1995 08:17:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0036                                      * 00020005
      *    --------------------------------------------------------   * 00030000
      *    BATCHRAHMEN: DATEI-FEHLER-BEHANDLUNG                       * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00060000
      *    DATUM:    19.08.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    14.09.94 16:34:46
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0144!19.08.94!    ! NEU                       ! SCHÖLLH. *  00140007
      *         !06.09.94! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  00140107
      * AFPA0002!23.08.96! 02 ! ERR-AUFBER-NORMAL GESETZT ! AUERBACH *  00140008
      *         !        !    ! WEGEN MF WB               !          *  00142000
      *         !        !    !                           !          *  00143000
      *         !        !    !                           !          *  00144000
      *                                                              *  00145000
      * ENDE-LOGBUCH                                                 *  00146000
      ****************************************************************  00147000
      *                                                                 00148000
      *------------------------------------------------------------*    00149000
       BR94-DATEI-FEHLER SECTION.                                       00151006
       BR94-ANF.                                                        00152006
      *------------------------------------------------------------*    00160000
      * INPUT : MUSS: S-DATEI-STATUS, K-PGM-NAME                   *    00170002
      *         KANN: ERR-ORT-SEC, ERR-ORT-LFD, ERR-VAR-ZEILE01 /04*    00181002
      * OUTPUT: FEHLERBILD (ERRIPRO) MIT INFOS ZU PROG-FEHLER      *    00190000
      * VERARBEITUNG:                                              *    00200000
      *   - FUELLEN DER ERROR-SCHNITTSTELLE                        *    00210000
      *   - AUFRUF VON BR99-SCHWERER-FEHLER-DISPLAY                *    00220006
      *------------------------------------------------------------*    00230000
      *                                                                 00260000
      *    * VERSORGEN SCHNITTSTELLE                                    00270000
           SET ERR-FTYP-DATEI              TO TRUE                      00280000
960823     SET ERR-AUFBER-NORMAL           TO TRUE                      00280000
           MOVE K-PGM-NAME                 TO ERR-PROGNAME              00280104
           MOVE S-DATEI-STATUS             TO ERR-FILE-STATUS           00281000
           PERFORM BR99-SCHWERER-FEHLER-DISPLAY                         00290006
           .                                                            00300000
       BR94-EXIT.                                                       00310006
           EXIT.                                                        00320000
      *                                                                 00330000
      ******** ENDE COPY-MEMBER (DPAT0036) *******************          00340005
                                                                        00350000
