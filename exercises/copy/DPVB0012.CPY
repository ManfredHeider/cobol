      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0012.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0012.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:22:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0012                                      * 00020000
      *    LESEN DER TABLE MSBS AUFGRUND VON VORGEGEBENEN MS-NR UND   * 00021000
      *    MSBS-NR                                                    * 00021100
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
       MSBS-LESEN SECTION.                                              00023600
      *---------------------------------------------------------------  00023700
      *-INPUT:  WK-BS-NR-UNTST, WK-MS-NR-UNTST                       -  00024000
      *---------------------------------------------------------------  00025000
       MSBS-LESEN-ANF.                                                  00026000
                                                                        00027000
           INITIALIZE VIVB0008-GESAMT                                   00028000
           PERFORM  WITH TEST AFTER                                     00029000
                    VARYING  IND-I  FROM   1  BY  1                     00030000
                                    UNTIL  IND-I   >  WK-LESEN-MAX      00040000
                                     OR    (SQLCODE  NOT =  -904)       00050000
CCC005*    EXEC SQL                                                     00060000
CCC004*       SELECT                                                    00070000
CCC004*             MS_NR,                                              00080000
CCC004*             BS_NR,                                              00090000
CCC004*             MSBS_EH_EIG_GES,                                    00100000
CCC004*             MSBS_EH_EIG_PZTR,                                   00110000
CCC004*             MSBS_ZEITRAUM,                                      00120000
CCC004*             MSBS_EQF_EIG,                                       00130000
CCC004*             MSBS_PROV_STF,                                      00140000
CCC004*             MSBS_LAUFZEIT,                                      00150000
CCC004*             MSBS_GUELTIG_BIS,                                   00160000
CCC004*             MSBS_BEZ                                            00170000
CCC004*       INTO                                                      00180000
CCC004*             :DCL-VIVB0008.MS-NR                                 00190000
CCC004*              :NI-VIVB0008.NI-MS-NR                              00200000
CCC004*            ,:DCL-VIVB0008.BS-NR                                 00210000
CCC004*              :NI-VIVB0008.NI-BS-NR                              00220000
CCC004*            ,:DCL-VIVB0008.MSBS-EH-EIG-GES                       00230000
CCC004*              :NI-VIVB0008.NI-MSBS-EH-EIG-GES                    00240000
CCC004*            ,:DCL-VIVB0008.MSBS-EH-EIG-PZTR                      00250000
CCC004*              :NI-VIVB0008.NI-MSBS-EH-EIG-PZTR                   00260000
CCC004*            ,:DCL-VIVB0008.MSBS-ZEITRAUM                         00270000
CCC004*              :NI-VIVB0008.NI-MSBS-ZEITRAUM                      00280000
CCC004*            ,:DCL-VIVB0008.MSBS-EQF-EIG                          00290000
CCC004*              :NI-VIVB0008.NI-MSBS-EQF-EIG                       00300000
CCC004*            ,:DCL-VIVB0008.MSBS-PROV-STF                         00310000
CCC004*              :NI-VIVB0008.NI-MSBS-PROV-STF                      00320000
CCC004*            ,:DCL-VIVB0008.MSBS-LAUFZEIT                         00330000
CCC004*              :NI-VIVB0008.NI-MSBS-LAUFZEIT                      00340000
CCC004*            ,:DCL-VIVB0008.MSBS-GUELTIG-BIS                      00350000
CCC004*              :NI-VIVB0008.NI-MSBS-GUELTIG-BIS                   00360000
CCC004*            ,:DCL-VIVB0008.MSBS-BEZ                              00370000
CCC004*              :NI-VIVB0008.NI-MSBS-BEZ                           00380000
CCC004*       FROM                                                      00390000
CCC004*             VIVB0008                                            00400000
CCC004*       WHERE                                                     00410000
CCC004*             MS_NR           =   :WK-MS-NR-UNTST                 00420000
CCC004*        AND  BS_NR           =   :WK-BS-NR-UNTST                 00430000
CCC003*    END-EXEC                                                     00440000
                                                                        00450000
           END-PERFORM                                                  00460000
           .                                                            00470000
       MSBS-LESEN-EXIT.                                                 00480000
           EXIT.                                                        00490000
