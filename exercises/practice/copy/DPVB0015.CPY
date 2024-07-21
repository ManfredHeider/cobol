      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0015.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0015.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:23:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0015                                      * 00020000
      *    LESEN DER TABLE ST NUR FÜR ST-NR'N MIT ST-FREI = 'N'       * 00021000
      *    UND FÜLLEN DER RELEVANTEN DATENFELDER AUS WLR-VB1-STRUK    * 00021100
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021500
      *    DATUM:    01.04.1993                                       * 00021600
      ***************************************************************** 00021800
      *    VERSION   1    31.03.94 17:56:59
      ****************************************************************  00022000
      * AENDERUNGEN:                                                 *  00022100
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022200
      * -------------------------------------------------------------*  00022300
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00022400
      * AFVB0044!28.03.94!    ! ZUSÄTZLICH ST-GESELLSCHAFT!          *  00022501
      *         !        !    ! -TYP LESEN                ! SCHMIDT  *  00022600
      *         !30.03.94! 02 ! PROD.UEBERGABE            ! SCHÖLLH. *  00022702
      *         !        !    !                           !          *  00022800
      *         !        !    !                           !          *  00022900
      *         !        !    !                           !          *  00023000
      *         !        !    !                           !          *  00023100
      *         !        !    !                           !          *  00023200
      *                                                              *  00023300
      * ENDE-LOGBUCH                                                 *  00023400
      ****************************************************************  00023500
      *----------------------------------------------------------       00023600
       ST-ERMITT SECTION.                                               00023700
      *----------------------------------------------------------       00023800
       ST-ERMITT-ANF.                                                   00023900
                                                                        00024000
           PERFORM  WITH TEST AFTER                                     00024100
                    VARYING  IND-I  FROM   1  BY  1                     00024200
                                    UNTIL  IND-I   >  WK-LESEN-MAX      00024300
                                     OR    (SQLCODE  NOT =  -904)       00025000
CCC005*    EXEC SQL                                                     00026000
CCC004*       SELECT  ST_NR,                                            00027000
CCC004*               ST_LAST_UPDATE,                                   00028000
CCC004*               ST_PRUEFZIFFER,                                   00029000
CCC004*               ST_FREI,                                          00030000
CCC004*               ST_BEREINIGUNG,                                   00040000
CCC004*               KTY_ST_TYP,                                       00050000
CCC004*               KNR_ST_TYP,                                       00060000
CCC004*               KTY_GESELL_TYP,                                   00061000
CCC004*               KNR_GESELL_TYP                                    00062000
CCC004*        INTO                                                     00070000
CCC004*               :DCL-VIVBA001:NI-VIVBA001-ELE                     00080000
CCC004*                                                                 00220000
CCC004*        FROM                                                     00230000
CCC004*               VIVBA001                                          00240000
CCC004*        WHERE                                                    00250000
CCC004*               ST_NR   =   :DCL-VIVBA001.ST-NR                   00260000
CCC004*          AND  ST_FREI =   :K-ST-BELEGT                          00270000
CCC003*    END-EXEC                                                     00280000
                                                                        00290000
           END-PERFORM                                                  00300000
                                                                        00310000
           EVALUATE   SQLCODE                                           00320000
           WHEN       0                                                 00330000
                 MOVE ST-NR IN DCL-VIVBA001 TO MMANR                    00340000
                                               MWVBNR                   00350000
                 MOVE ST-PRUEFZIFFER        TO MPZ                      00360000
                 EVALUATE KNR-ST-TYP                                    00370000
                 WHEN     1                                             00380000
                 WHEN     2                                             00390000
                 WHEN     4                                             00400000
                 WHEN     5                                             00410000
                 WHEN     6                                             00420000
                 WHEN     7                                             00430000
                     MOVE 10                TO MSA                      00440000
                 WHEN     3                                             00450000
                     MOVE 11                TO MSA                      00460000
                 END-EVALUATE                                           00470000
                                                                        00480000
TTMMJJ*          MOVE SQLCODE              TO WLR-SQL-CODE              00490000
           WHEN       100                                               00500000
                 SET VERARB-SQL-CODE-EX    TO TRUE                      00510000
                 MOVE SQLCODE              TO WLR-SQL-CODE              00520000
           WHEN       OTHER                                             00530000
                 SET VERARB-SQL-CODE-EX    TO TRUE                      00540000
                 MOVE SQLCODE              TO WLR-SQL-CODE              00550000
                 IF    SQLERRML       >   ZERO                          00560000
                     MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT         00570000
                  ELSE                                                  00580000
                      MOVE  SPACE          TO   WLR-FEHLER-TEXT         00590000
                 END-IF                                                 00600000
           END-EVALUATE                                                 00610000
           .                                                            00620000
       ST-ERMITT-EXIT.                                                  00630000
           EXIT.                                                        00640000
