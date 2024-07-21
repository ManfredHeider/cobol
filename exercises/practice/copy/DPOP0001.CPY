      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPOP0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPOP0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:14:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:19:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPOP0001                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    VERSION   1    22.06.95 13:24:56
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: B. BOHLINGER                                     * 00060000
      *    DATUM:    01.09.1994                                       * 00070000
      *    STAND:    21.06.1995                                       * 00080023
      ***************************************************************** 00090000
      *                                                                 00100000
      *                                                                 00130000
      *------------------------------------------------------------*    00160000
      * INPUT : ZI-DWOP0001-INPUT                                  *    00170003
      *                                                            *    00172000
      * OUTPUT: ZO-DWOP0001-OUTPUT                                 *    00173003
      *                                                            *    00175000
      * VERARBEITUNG:                                              *    00176000
      *   DIE ÜBERGEBENE GESELLSCHAFTSNUMMER WIRD ANHAND DER       *    00177022
      *   GESELLSCHAFTSDATEI OP003000.I1.CL AUF RICHTIGKEIT        *    00177123
      *   ÜBERPRÜFT                                                *    00177223
      *   OUTPUT: - RETURNCODE                                     *    00178022
      *           - GESELLSCHAFTSSATZ                              *    00179022
      *                                                            *    00190000
      *------------------------------------------------------------*    00200000
      ****************************************************************  00210023
      * AENDERUNGEN:                                                 *  00220023
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00230023
      * -------------------------------------------------------------*  00240023
      * AFOP0030!21.06.95! 01 ! NEU                       ! BOHLINGER*  00250023
      *         !        !    !                           !          *  00260023
      *         !        !    !                           !          *  00270023
      *         !        !    !                           !          *  00280023
      *         !        !    !                           !          *  00290023
      *         !        !    !                           !          *  00300023
      *                                                              *  00310023
      * ENDE-LOGBUCH                                                 *  00320023
      ****************************************************************  00330023
      *                                                                 00960000
       UV1-OP01-GESNR-PRUEFEN  SECTION.                                 00960118
       UV1-OP01-A.                                                      00960216
           MOVE 'UV1 '                     TO ERR-ORT-SEC               00960316
           PERFORM UR2-TRACE                                            00960406
                                                                        00960506
                                                                        00960606
           SET Z-DWOP0001-RC-OK TO TRUE                                 00960703
           MOVE 'OP0030'                   TO Z-DWOP0001-DAT-NAME       00960817
           MOVE 80                         TO Z-DWOP0001-DAT-LAENGE     00960917
                                                                        00961003
CCC005*    EXEC CICS READ                                               00961113
CCC004*              DATASET  (Z-DWOP0001-DAT-NAME)                     00961216
CCC004*              RIDFLD   (Z-DWOP0001-KEY)                          00961309
CCC004*              INTO     (Z-DWOP0001-GES-SATZ)                     00961405
CCC004*              LENGTH   (Z-DWOP0001-DAT-LAENGE)                   00961516
CCC004*              RESP     (C-CICS-RC)                               00961605
CCC003*    END-EXEC                                                     00961706
      *                                                                 00961805
           EVALUATE C-CICS-RC                                           00961906
           WHEN DFHRESP(NORMAL)                                         00962006
                CONTINUE                                                00962112
           WHEN DFHRESP(NOTFND)                                         00962206
           WHEN DFHRESP(ENDFILE)                                        00962306
                SET   Z-DWOP0001-RC-NOK TO  TRUE                        00963007
           WHEN OTHER                                                   00963706
                MOVE  K-1                    TO  ERR-ORT-LFD            00963812
                MOVE  Z-DWOP0001-KEY         TO  ERR-VAR-ZEILE01        00963910
                MOVE  Z-DWOP0001-DAT-NAME    TO  ERR-VAR-ZEILE02        00964019
                PERFORM ZR92-CICS-FEHLER                                00964106
           END-EVALUATE                                                 00964206
                                                                        00964320
                                                                        00964420
           .                                                            00964808
                                                                        00965003
       UV1-OP01-EXIT.                                                   00966016
           EXIT.                                                        00967008
      ******* ENDE COPY-MEMBER DPOP0001 (GESNR-PRÜFEN)******************00970016
CCC009*    EJECT                                                        00980000
