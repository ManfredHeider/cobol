      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0024.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPVB0024.CPv  $
      *
      *   Rev 2.1   Jul 18 2003 15:04:32   t13ex22
      *AVB00666.UE029: VBAS und ALP in QMF un stat. Aufrufe entfernt
      *
      *   Rev 2.0   Feb 27 1999 15:56:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:25:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0024                                      * 00020000
      *    DIESE SECTION BEWERKSTELLIGT DEN CALL AUF DAS SM           * 00021000
      *    UPVB0022 UND ERMITTELT EINE GÜLTIGE MA-STUFE               * 00021100
      *    --------------------------------------------------------   * 00021300
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021600
      *    DATUM:    02.04.1993                                       * 00021700
      ***************************************************************** 00021900
      *    VERSION   01   16.09.93                                   *  00022003
      ****************************************************************  00022103
      * AENDERUNGEN:                                                 *  00022203
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022303
      * -------------------------------------------------------------*  00022403
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00022503
      * AVB00666!17.07.03!2.1 ! MUND                      ! MUND     *  00022603
      *         !        !    !                           !          *  00022703
      *         !        !    !                           !          *  00022803
      *         !        !    !                           !          *  00022903
      *         !        !    !                           !          *  00023003
      *         !        !    !                           !          *  00023103
      *         !        !    !                           !          *  00023203
      *                                                              *  00023303
      * ENDE-LOGBUCH                                                 *  00023403
      ****************************************************************  00023503
       UPVB0022-BEARB SECTION.                                          00023600
      *------------------------------------------------------------*    00023700
      * UPVB0022-BEARB                                             *    00023800
      *------------------------------------------------------------*    00023900
      * INPUT : Z-ST-NR, Z-PROV-STZ-GP                             *    00024001
      *                                                            *    00024100
      * OUTPUT: Z-STUFE                                            *    00024200
      *         WLR-RUECK-CODE                                     *    00024300
      *         WLR-FEHLER-TEXT, FALLS KEINE KONVERTIERUNG VON     *    00024400
      *         KL BZW. JUR NACH MA MÖGLICH IST                    *    00024500
      *                                                            *    00025000
      * VERARBEITUNG:                                              *    00026000
      *   -                                                        *    00027000
      *   -                                                        *    00028000
      *   -                                                        *    00029000
      *                                                            *    00030000
      *------------------------------------------------------------*    00040000
       UPVB0022-BEARB-ANF.                                              00060000
      *-------------------------------------------------------------    00070000
           MOVE Z-PROV-STZ-GP TO PI-UPVB0022-PROV-STZ-GP                00080000
170703*    CALL 'UPVB0022' USING BY CONTENT   PI-UPVB0022,              00090000
CCCPY1*    CALL K-UPVB0022 USING BY CONTENT   PI-UPVB0022,
CCC004*                          BY REFERENCE PO-UPVB0022,              00100000
CCC004*                          BY REFERENCE ERR-ERROR-BEREICH         00110000
CCC004*                                                                 00120000
CCC004*    IF ERR-RC = 0                                                00130000
CCC004*       IF PO-UPVB0022-MS-NR = ZERO                               00140000
      *       Z-PROV-STZ-GP ENTSPRICHT KEINER GÜLTIGEN MA-STUFE         00150002
CCC004*          SET VERARB-LOG-FEHLER     TO TRUE                      00170000
CCC004*          MOVE  Z-ST-NR             TO WK-ST-NR7                 00180001
CCC004*          MOVE  WK-FEHLER-TEXT7     TO WLR-FEHLER-TEXT           00190000
CCC004*       ELSE                                                      00200000
      *       Z-PROV-STZ-GP ENTSPRICHT EINER GÜLTIGEN MA-STUFE          00210002
CCC004*          MOVE  PO-UPVB0022-MS-NR   TO Z-STUFE                   00230000
CCC004*       END-IF                                                    00240000
CCC004*    ELSE                                                         00250000
CCC004*       EVALUATE TRUE                                             00260000
CCC004*       WHEN ERR-FTYP-SQL                                         00270000
CCC004*          SET VERARB-SQL-CODE-EX    TO TRUE                      00280000
CCC004*          MOVE     ERR-SQLCODE      TO WLR-SQL-CODE              00290000
CCC004*          MOVE     ERR-SQLERRMC     TO WLR-FEHLER-TEXT           00300000
CCC004*       WHEN OTHER                                                00310000
      *       LOG. FEHLER                                               00311000
CCC004*          SET VERARB-LOG-FEHLER     TO TRUE                      00312000
CCC004*          MOVE 'UPVB0022-BEARB'     TO WK-PARAGRAPH              00313000
CCC004*          MOVE  WK-FEHLER-TEXT6     TO WLR-FEHLER-TEXT           00314000
CCC004*       END-EVALUATE                                              00315000
CCC004*    END-IF                                                       00316000
CCC004*    .                                                            00320000
CCC004*UPVB0022-BEARB-EXIT.                                             00330000
CCC004*    EXIT.                                                        00340000
