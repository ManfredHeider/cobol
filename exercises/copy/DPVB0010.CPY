      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0010.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0010.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:22:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0010                                      * 00020000
      *    ERMITTELN GRUPPENPROVISIONSSATZ EINES UNTERSTELLTEN MA'S   * 00021000
      *    AUFGRUND DER MS-NR UND GGF. DER BS-NR                      * 00021100
      *    --------------------------------------------------------   * 00021300
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021600
      *    DATUM:    01.04.1993                                       * 00021700
      ***************************************************************** 00021900
      *    VERSION   01   16.09.93                                   *  00022001
      ****************************************************************  00022101
      * AENDERUNGEN:                                                 *  00022201
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022301
      * -------------------------------------------------------------*  00022401
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00022501
      *         !        !    !                           !          *  00022601
      *         !        !    !                           !          *  00022701
      *         !        !    !                           !          *  00022801
      *         !        !    !                           !          *  00022901
      *         !        !    !                           !          *  00023001
      *         !        !    !                           !          *  00023101
      *         !        !    !                           !          *  00023201
      *                                                              *  00023301
      * ENDE-LOGBUCH                                                 *  00023401
      ****************************************************************  00023501
      *---------------------------------------------------------------  00023600
       PROVSATZ-STUFE SECTION.                                          00023700
      *---------------------------------------------------------------  00024000
      *-INPUT:  WK-BS-NR-UNTST, WK-MS-NR-UNTST                       -  00025000
      *-OUTPUT: WK-PROV-GP-UNTST                                     -  00026000
      *---------------------------------------------------------------  00027000
       PROVSATZ-STUFE-ANF.                                              00028000
                                                                        00029000
                                                                        00030000
           MOVE  ZERO TO WK-PROV-GP-UNTST                               00040000
                                                                        00050000
           PERFORM MS-LESEN                                             00060000
                                                                        00070000
           EVALUATE   SQLCODE                                           00080000
           WHEN       0                                                 00090000
              IF WK-BS-NR-UNTST > 0                                     00100000
                 PERFORM    MSBS-LESEN                                  00110000
                                                                        00120000
                 EVALUATE   SQLCODE                                     00130000
                 WHEN       0                                           00140000
      *          ERMITTELN DES GRUPPENPROVISIONSSATZES DES UNTERSTELLTEN00150000
      *          UNTER BERÜCKSICHTIGUNG SEINER BONUSSTUFE               00160000
                       COMPUTE WK-PROV-GP-UNTST =                       00170000
                                          MSBS-PROV-STF + MS-PROV-STF   00180000
TTMMJJ*                MOVE SQLCODE         TO WLR-SQL-CODE             00190000
                 WHEN       100                                         00200000
                       SET VERARB-LOG-FEHLER        TO TRUE             00210000
                       MOVE 'MSBS'                  TO WK-TABLE-NAME5   00220000
                       MOVE  MS-NR IN DCL-VIVB0008  TO WK-KEY-ID5       00230000
                       MOVE  WK-FEHLER-TEXT5        TO WLR-FEHLER-TEXT  00240000
                 WHEN       OTHER                                       00250000
                       SET VERARB-SQL-CODE-EX    TO TRUE                00260000
                       MOVE SQLCODE              TO WLR-SQL-CODE        00270000
                       IF    SQLERRML       >   ZERO                    00280000
                           MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT   00290000
                        ELSE                                            00300000
                            MOVE  SPACE          TO   WLR-FEHLER-TEXT   00310000
                       END-IF                                           00320000
                 END-EVALUATE                                           00330000
              ELSE                                                      00340000
      *          ERMITTELN DES GRUPPENPROVISIONSSATZES DES UNTERSTELLTEN00350000
      *          OHNE BERÜCKSICHTIGUNG SEINER BONUSSTUFE                00360000
                 MOVE MS-PROV-STF             TO WK-PROV-GP-UNTST       00370000
              END-IF                                                    00380000
           WHEN       100                                               00390000
                 SET VERARB-LOG-FEHLER        TO TRUE                   00400000
                 MOVE 'MS  '                  TO WK-TABLE-NAME5         00410000
                 MOVE  MS-NR IN DCL-VIVB0009  TO WK-KEY-ID5             00420000
                 MOVE  WK-FEHLER-TEXT5        TO WLR-FEHLER-TEXT        00430000
           WHEN       OTHER                                             00440000
                 SET VERARB-SQL-CODE-EX    TO TRUE                      00450000
                 MOVE SQLCODE              TO WLR-SQL-CODE              00460000
                 IF    SQLERRML       >   ZERO                          00470000
                     MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT         00480000
                  ELSE                                                  00490000
                      MOVE  SPACE          TO   WLR-FEHLER-TEXT         00500000
                 END-IF                                                 00510000
           END-EVALUATE                                                 00520000
           .                                                            00530000
       PROVSATZ-STUFE-EXIT.                                             00540000
           EXIT.                                                        00550000
