      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0025.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPVB0025.CPv  $
      *
      *   Rev 2.1   Jul 18 2003 15:04:38   t13ex22
      *AVB00666.UE029: VBAS und ALP in QMF un stat. Aufrufe entfernt
      *
      *   Rev 2.0   Feb 27 1999 15:56:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:25:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0025                                      * 00020000
      *    DIESE SECTION BEWERKSTELLIGT DEN CALL AUF DAS SM           * 00021000
      *    UPVB0023 UND ERMITTELT EINE GÜLTIGE VB-PRAXIS-STUFE        * 00021100
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021500
      *    DATUM:    02.04.1993                                       * 00021600
      ***************************************************************** 00021800
      *    VERSION   01   16.09.93                                   *  00021903
      ****************************************************************  00022003
      * AENDERUNGEN:                                                 *  00022103
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022203
      * -------------------------------------------------------------*  00022303
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00022403
      * AVB00666!17.07.03!2.1 ! STAT AUFRUFE ENTFERNT     ! MUND     *  00022503
      *         !        !    !                           !          *  00022603
      *         !        !    !                           !          *  00022703
      *         !        !    !                           !          *  00022803
      *         !        !    !                           !          *  00022903
      *         !        !    !                           !          *  00023003
      *         !        !    !                           !          *  00023103
      *                                                              *  00023203
      * ENDE-LOGBUCH                                                 *  00023303
      ****************************************************************  00023403
       UPVB0023-BEARB SECTION.                                          00023500
      *------------------------------------------------------------*    00023600
      * UPVB0023-BEARB                                             *    00023700
      *------------------------------------------------------------*    00024000
      * INPUT : Z-ST-NR, Z-PROV-STZ-AP                             *    00025002
      *                                                            *    00026000
      * OUTPUT: Z-EPROVKZ                                          *    00027000
      *         WLR-RUECK-CODE                                     *    00028000
      *         WLR-FEHLER-TEXT, FALLS KEINE KONVERTIERUNG VON     *    00029000
      *         KL BZW. JUR NACH MA MÖGLICH IST                    *    00030000
      *                                                            *    00040000
      * VERARBEITUNG:                                              *    00050000
      *   -                                                        *    00060000
      *   -                                                        *    00070000
      *   -                                                        *    00080000
      *                                                            *    00090000
      *------------------------------------------------------------*    00100000
       UPVB0023-BEARB-ANF.                                              00120000
      *----------------------------------------------------------       00130000
           MOVE Z-PROV-STZ-AP TO PI-UPVB0023-PROV-STZ-AP                00140000
170703*    CALL 'UPVB0023' USING BY CONTENT   PI-UPVB0023,              00150000
CCCPY1*    CALL K-UPVB0023 USING BY CONTENT   PI-UPVB0023,              00150000
CCC004*                          BY REFERENCE PO-UPVB0023,              00160000
CCC004*                          BY REFERENCE ERR-ERROR-BEREICH         00170000
CCC004*                                                                 00180000
CCC004*    IF ERR-RC = 0                                                00190000
CCC004*       IF PO-UPVB0023-VBP-NR = ZERO                              00200000
      *       Z-PROV-STZ-AP ENTSPRICHT KEINER GÜLTIGEN VB-PRAXIS-STUFE  00210000
CCC004*          SET VERARB-LOG-FEHLER     TO TRUE                      00220000
CCC004*          MOVE  Z-ST-NR             TO WK-ST-NR7                 00230002
CCC004*          MOVE  WK-FEHLER-TEXT7     TO WLR-FEHLER-TEXT           00240001
CCC004*       ELSE                                                      00250000
      *       Z-PROV-STZ-AP ENTSPRICHT EINER GÜLTIGEN VB-PRAXIS-STUFE   00260000
CCC004*          EVALUATE PO-UPVB0023-VBP-NR                            00270000
CCC004*          WHEN     2 THRU 10                                     00270100
CCC004*             COMPUTE Z-VBP-NR-1 = PO-UPVB0023-VBP-NR - 1         00270200
CCC004*             MOVE    Z-VBP-NR-1   TO Z-EPROVKZ                   00270300
CCC004*          WHEN     OTHER                                         00270400
      *          LOG. FEHLER                                            00270500
CCC004*             SET VERARB-LOG-FEHLER     TO TRUE                   00270600
CCC004*             MOVE 'UPVB0023-BEARB'     TO WK-PARAGRAPH           00270700
CCC004*             MOVE  WK-FEHLER-TEXT6     TO WLR-FEHLER-TEXT        00270800
CCC004*          END-EVALUATE                                           00270900
CCC004*       END-IF                                                    00280000
CCC004*    ELSE                                                         00290000
CCC004*       EVALUATE TRUE                                             00300000
CCC004*       WHEN ERR-FTYP-SQL                                         00310000
CCC004*          SET VERARB-SQL-CODE-EX    TO TRUE                      00311000
CCC004*          MOVE     ERR-SQLCODE      TO WLR-SQL-CODE              00312000
CCC004*          MOVE     ERR-SQLERRMC     TO WLR-FEHLER-TEXT           00313000
CCC004*       WHEN OTHER                                                00313100
      *       LOG. FEHLER                                               00313200
CCC004*          SET VERARB-LOG-FEHLER     TO TRUE                      00313300
CCC004*          MOVE 'UPVB0023-BEARB'     TO WK-PARAGRAPH              00313400
CCC004*          MOVE  WK-FEHLER-TEXT6     TO WLR-FEHLER-TEXT           00313500
CCC004*       END-EVALUATE                                              00313600
CCC004*    END-IF                                                       00313700
CCC004*    .                                                            00313800
CCC004*UPVB0023-BEARB-EXIT.                                             00314000
CCC004*    EXIT.                                                        00315000
