      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0013.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0013.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:22:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0013                                      * 00020000
      *    ERMITTELN DES GRUPPENPROVISIONSATZES DES MA-BETREUERS, DER * 00021000
      *    EIN RF IST.                                                * 00021100
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021500
      *    DATUM:    01.04.1993                                       * 00021600
      ***************************************************************** 00021800
      *    VERSION   01   16.09.93                                   *  00021901
      ****************************************************************  00022001
      * AENDERUNGEN:                                                 *  00022101
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022201
      * -------------------------------------------------------------*  00022301
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00022401
      *         !        !    !                           !          *  00022501
      *         !        !    !                           !          *  00022601
      *         !        !    !                           !          *  00022701
      *         !        !    !                           !          *  00022801
      *         !        !    !                           !          *  00022901
      *         !        !    !                           !          *  00023001
      *         !        !    !                           !          *  00023101
      *                                                              *  00023201
      * ENDE-LOGBUCH                                                 *  00023301
      ****************************************************************  00023401
      *---------------------------------------------------------------  00023500
       PROVSATZ-RF-GP SECTION.                                          00023600
      *---------------------------------------------------------------  00023700
      *-INPUT:  WK-MA-ST-NR-BETR,                                    -  00024000
      *-        WK-RF-DAT-VON-BETR, WK-RF-DAT-BIS-BETR               -  00025000
      *-OUTPUT: WK-PROV-GP-BETR                                      -  00026000
      *---------------------------------------------------------------  00027000
       PROVSATZ-RF-GP-ANF.                                              00028000
                                                                        00029000
           MOVE ZERO TO WK-PROV-GP-BETR                                 00030000
           INITIALIZE   WB-RF-PROV-GP-BETR                              00040000
                                                                        00050000
           PERFORM  WITH TEST AFTER                                     00060000
                    VARYING  IND-I  FROM   1  BY  1                     00070000
                                    UNTIL  IND-I   >  WK-LESEN-MAX      00080000
                                     OR    (SQLCODE  NOT =  -904)       00090000
CCC005*    EXEC SQL                                                     00100000
CCC004*       SELECT                                                    00110000
CCC004*             MS_PROV_STF                                         00120000
CCC004*       INTO                                                      00130000
CCC004*             :WB-RF-PROV-GP-BETR                                 00140000
CCC004*       FROM                                                      00150000
CCC004*             VIVB0009 MS, VIVB0012 MSBZ                          00160000
CCC004*       WHERE                                                     00170000
CCC004*             MA_ST_NR        =   :WK-MA-ST-NR-BETR               00180000
CCC004*        AND  MSBZ.MS_NR      =    MS.MS_NR                       00190000
CCC004*        AND  MSBZ_DAT_VON   <=   :WK-RF-DAT-VON-BETR             00200000
CCC004*        AND  MSBZ_DAT_BIS   >=   :WK-RF-DAT-BIS-BETR             00210000
CCC003*    END-EXEC                                                     00220000
                                                                        00230000
           END-PERFORM                                                  00240000
                                                                        00250000
           EVALUATE   SQLCODE                                           00260000
           WHEN       0                                                 00270000
                 MOVE WB-MS-PROV-STF     IN WB-RF-PROV-GP-BETR          00280000
                                         TO WK-PROV-GP-BETR             00290000
      *                                                                 00300000
TTMMJJ*          MOVE SQLCODE         TO WLR-SQL-CODE                   00310000
           WHEN       100                                               00320000
                 SET VERARB-LOG-FEHLER        TO TRUE                   00330000
                 MOVE 'PROVSATZ-RF-GP'        TO WK-PARAGRAPH           00340000
                 MOVE  WK-FEHLER-TEXT6        TO WLR-FEHLER-TEXT        00350000
           WHEN       OTHER                                             00360000
                 SET VERARB-SQL-CODE-EX    TO TRUE                      00370000
                 MOVE SQLCODE              TO WLR-SQL-CODE              00380000
                 IF    SQLERRML       >   ZERO                          00390000
                     MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT         00400000
                  ELSE                                                  00410000
                      MOVE  SPACE          TO   WLR-FEHLER-TEXT         00420000
                 END-IF                                                 00430000
           END-EVALUATE                                                 00440000
           .                                                            00450000
       PROVSATZ-RF-GP-EXIT.                                             00460000
           EXIT.                                                        00470000
