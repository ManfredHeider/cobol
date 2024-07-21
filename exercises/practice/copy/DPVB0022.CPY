      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0022.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0022.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:25:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0022                                      * 00020000
      *    LESEN DER TABLE SDNR                                       * 00021000
      *    UND FÜLLEN DER RELEVANTEN DATENFELDER AUS WLR-VB1-STRUK    * 00021100
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021500
      *    DATUM:    01.04.1993                                       * 00021600
      ***************************************************************** 00021800
      *    VERSION   1    31.03.94 17:57:15
      ****************************************************************  00022000
      * AENDERUNGEN:                                                 *  00022100
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022200
      * -------------------------------------------------------------*  00022300
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00022400
      * AFVB0044!28.03.94!    ! ZUSÄTZLICHES ATTRIBUTE    !          *  00022501
      *         !        !    ! SDNR-TYP, SDNR-ANFANG UND !          *  00022600
      *         !        !    ! SDNR-ENDE IN SELECT AUFGEN! SCHMIDT  *  00022700
      *         !30.03.94! 02 ! PROD.UEBERGABE            ! SCHÖLLH. *  00022802
      *         !        !    !                           !          *  00022900
      *         !        !    !                           !          *  00023000
      *         !        !    !                           !          *  00023100
      *         !        !    !                           !          *  00023200
      *                                                              *  00023300
      * ENDE-LOGBUCH                                                 *  00023400
      ****************************************************************  00023500
      *----------------------------------------------------------       00023600
       SDNR-ERMITT SECTION.                                             00023700
      *----------------------------------------------------------       00023800
       SDNR-ERMITT-ANF.                                                 00023900
           PERFORM  WITH TEST AFTER                                     00024000
                    VARYING  IND-I  FROM   1  BY  1                     00024100
                                    UNTIL  IND-I   >  WK-LESEN-MAX      00024200
                                     OR    (SQLCODE  NOT =  -904)       00024300
CCC005*    EXEC SQL                                                     00024400
CCC004*       SELECT                                                    00024500
CCC004*             SDNR_ST_NR,                                         00024600
CCC004*             SDNR_LAST_UPDATE,                                   00025000
CCC004*             SDNR_BEZ,                                           00026000
CCC004*             KTY_SDNR_TYP,                                       00026100
CCC004*             KNR_SDNR_TYP,                                       00026200
CCC004*             SDNR_ANFANG,                                        00026300
CCC004*             SDNR_ENDE                                           00026400
CCC004*       INTO                                                      00027000
CCC004*             :DCL-VIVB0050:NI-VIVB0050-ELE                       00028000
CCC004*       FROM                                                      00070000
CCC004*             VIVB0050                                            00080000
CCC004*       WHERE                                                     00090000
CCC004*             SDNR_ST_NR      =   :SDNR-ST-NR                     00100000
CCC003*    END-EXEC                                                     00110000
                                                                        00120000
           END-PERFORM                                                  00130000
                                                                        00140000
           EVALUATE   SQLCODE                                           00150000
           WHEN       0                                                 00160000
                 MOVE SDNR-BEZ              TO MZUNAM                   00170000
      *                                                                 00180000
TTMMJJ*          MOVE SQLCODE         TO WLR-SQL-CODE                   00190000
           WHEN       100                                               00200000
                 SET VERARB-LOG-FEHLER     TO TRUE                      00210000
                 MOVE 'SDNR-ST-NR'         TO WK-TABLE-KEY              00220000
                 MOVE  SDNR-ST-NR          TO WK-ST-NR1                 00230000
                 MOVE  WK-FEHLER-TEXT1     TO WLR-FEHLER-TEXT           00240000
           WHEN       OTHER                                             00250000
                 SET VERARB-SQL-CODE-EX    TO TRUE                      00260000
                 MOVE SQLCODE              TO WLR-SQL-CODE              00270000
                 IF    SQLERRML       >   ZERO                          00280000
                     MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT         00290000
                  ELSE                                                  00300000
                      MOVE  SPACE          TO   WLR-FEHLER-TEXT         00310000
                 END-IF                                                 00320000
           END-EVALUATE                                                 00330000
           .                                                            00340000
       SDNR-ERMITT-EXIT.                                                00500000
           EXIT.                                                        00510000
