      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0034.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0034.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Aug 26 1996 09:45:24   T13EX16
      *AFPA0002 Anpassungen wegen der Workbench
      *
      *   Rev 1.0   Nov 04 1995 08:16:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPAT0034                                      * 00020007
      *    --------------------------------------------------------   * 00030000
      *    BATCHRAHMEN: SQL-FEHLER-BEHANDLUNG                         * 00040000
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00060000
      *    DATUM:    19.08.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    14.09.94 16:34:13
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0144!19.08.94!    ! NEU                       ! SCHÖLLH. *  00140008
      *         !06.09.94! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *  00141008
      * AFPA0002!23.08.96! 02 ! ERR-AUFBER-NORMAL GESETZT ! AUERBACH *  00140008
      *         !        !    ! WEGEN MF WB               !          *  00142000
      *         !        !    !                           !          *  00150000
      *         !        !    !                           !          *  00180000
      *                                                              *  00190000
      * ENDE-LOGBUCH                                                 *  00200000
      ****************************************************************  00210000
      *                                                                 00220000
      *------------------------------------------------------------*    00230000
       BR91-SQL-FEHLER SECTION.                                         00240002
       BR91-ANF.                                                        00250002
      *------------------------------------------------------------*    00260000
      * INPUT : MUSS: K-PGM-NAME, SQLCODE, SQLERRMC, SQLERRML      *    00270003
      *         KANN: ERR-ORT-SEC, ERR-ORT-LFD, ERR-VAR-ZEILE01 /04*    00280004
      * OUTPUT: FEHLERBILD (ERRIPRO) MIT INFOS ZU SQL-FEHLER       *    00290000
      * VERARBEITUNG:                                              *    00300000
      *   - FUELLEN DER ERROR-SCHNITTSTELLE                        *    00310000
      *   - AUFRUF VON BR99-SCHWERER-FEHLER-DISPLAY                *    00320000
      *------------------------------------------------------------*    00330000
      *                                                                 00340000
      *    * VERSORGEN SCHNITTSTELLE                                    00350000
           SET ERR-FTYP-SQL                TO TRUE                      00360000
960823     SET ERR-AUFBER-NORMAL           TO TRUE                      00360000
           MOVE K-PGM-NAME                 TO ERR-PROGNAME              00361006
           MOVE SQLCODE                    TO ERR-SQLCODE               00370000
           IF SQLERRML > ZERO                                           00380000
              MOVE SQLERRMC                TO ERR-SQLERRMC              00390000
           END-IF                                                       00400000
           PERFORM BR99-SCHWERER-FEHLER-DISPLAY                         00410000
           .                                                            00420000
       BR91-EXIT.                                                       00430000
           EXIT.                                                        00440000
      *                                                                 00450000
      ******** ENDE COPY-MEMBER (DPAT0034) *******************          00460007
                                                                        00470000
