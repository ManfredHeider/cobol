      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0035.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0035.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Aug 26 1996 09:45:56   T13EX16
      *AFPA0002 Anpassungen wegen der Workbench
      *
      *   Rev 1.0   Nov 04 1995 08:16:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0035                                      * 00020005
      *    --------------------------------------------------------   * 00030000
      *    BATCHRAHMEN: PROGRAMM- /LOGIKFEHLER-BEHANDLUNG             * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00060000
      *    DATUM:    19.08.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    14.09.94 16:34:30
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0144!19.08.94!    ! NEU                       ! SCHÖLLH. *  00140008
      *         !06.09.94! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  00140108
      * AFPA0002!23.08.96! 02 ! ERR-AUFBER-NORMAL GESETZT ! AUERBACH *  00140008
      *         !        !    ! WEGEN MF WB               !          *  00142000
      *         !        !    !                           !          *  00141000
      *         !        !    !                           !          *  00144000
      *                                                              *  00145000
      * ENDE-LOGBUCH                                                 *  00146000
      ****************************************************************  00147000
      *                                                                 00148000
      *------------------------------------------------------------*    00149000
       BR93-PROG-FEHLER SECTION.                                        00151006
       BR93-ANF.                                                        00152006
      *------------------------------------------------------------*    00160000
      * INPUT : MUSS: K-PGM-NAME                                   *    00170002
      *         KANN: ERR-ORT-SEC, ERR-ORT-LFD, ERR-VAR-ZEILE01 /04*    00171002
      * OUTPUT: FEHLERBILD (ERRIPRO) MIT INFOS ZU PROG-FEHLER      *    00200000
      * VERARBEITUNG:                                              *    00220000
      *   - FUELLEN DER ERROR-SCHNITTSTELLE                        *    00230000
      *   - AUFRUF VON BR99-SCHWERER-FEHLER-DISPLAY                *    00240007
      *------------------------------------------------------------*    00260000
      *                                                                 00290000
      *    * VERSORGEN SCHNITTSTELLE                                    00300000
           SET ERR-FTYP-PROG               TO TRUE                      00310000
960823     SET ERR-AUFBER-NORMAL           TO TRUE                      00310000
           MOVE K-PGM-NAME                 TO ERR-PROGNAME              00311004
           PERFORM BR99-SCHWERER-FEHLER-DISPLAY                         00320007
           .                                                            00330000
       BR93-EXIT.                                                       00340006
           EXIT.                                                        00350000
      *                                                                 00370000
      ******** ENDE COPY-MEMBER (DPAT0035) *******************          00380005
                                                                        00390000
