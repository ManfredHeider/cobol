      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0011.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DPVB0011.CPv  $
      *
      *   Rev 2.1   05 Dec 2001 08:58:48   t13ae13
      *AVB00544: Umstellung 40%-Klausel auf variabel
      *
      *   Rev 2.0   Feb 27 1999 15:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:22:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0011                                      * 00020000
      *    LESEN DER TABLE MS AUFGRUND EINER VORGEGEBENEN MS-NR       * 00021000
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
      * AVB00544!06.11.01! 2.1! MS_40PROZ_REGEL entfällt, ! Walb     *  00022501
      *         !        !    ! dafür MS_ABZPROZ_REGEL neu!          *  00022601
      *         !        !    !                           !          *  00022701
      *         !        !    !                           !          *  00022801
      *         !        !    !                           !          *  00022901
      *         !        !    !                           !          *  00023001
      *         !        !    !                           !          *  00023101
      *                                                              *  00023201
      * ENDE-LOGBUCH                                                 *  00023301
      ****************************************************************  00023401
      *---------------------------------------------------------------  00023500
       MS-LESEN SECTION.                                                00023600
      *---------------------------------------------------------------  00023700
      *-INPUT:  WK-MS-NR-UNTST                                       -  00024000
      *---------------------------------------------------------------  00025000
       MS-LESEN-ANF.                                                    00026000
                                                                        00027000
           INITIALIZE VIVB0009-GESAMT                                   00028000
           PERFORM  WITH TEST AFTER                                     00029000
                    VARYING  IND-I  FROM   1  BY  1                     00030000
                                    UNTIL  IND-I   >  WK-LESEN-MAX      00040000
                                     OR    (SQLCODE  NOT =  -904)       00050000
CCC005*    EXEC SQL                                                     00060000
CCC004*       SELECT                                                    00070000
CCC004*             MS_NR,                                              00080000
CCC004*             MS_EINSTIEG,                                        00090000
CCC004*             MS_HPT,                                             00100000
CCC004*             MS_34C,                                             00110000
061101*             MS_40PROZ_REGEL,                                    00120000
CCC004*             MS_ABZPROZ_REGEL,                                   00120000
CCC004*             MS_EING_EH_EIG,                                     00130000
CCC004*             MS_EH_EIG_GES,                                      00140000
CCC004*             MS_EH_EIG_PZTR,                                     00150000
CCC004*             MS_EH_GRU_PZTR,                                     00160000
CCC004*             MS_EH_GES_PZTR,                                     00170000
CCC004*             MS_WEIT_EH_EIG,                                     00180000
CCC004*             MS_ZEITRAUM,                                        00190000
CCC004*             MS_EQF_EIG,                                         00200000
CCC004*             MS_EQF_GRU,                                         00210000
CCC004*             MS_EQF_GES,                                         00220000
CCC004*             MS_PROV_STF,                                        00230000
CCC004*             MS_GUELTIG_BIS,                                     00240000
CCC004*             MS_BEZ_KURZ,                                        00250000
CCC004*             MS_BEZ                                              00260000
CCC004*       INTO                                                      00270000
CCC004*             :DCL-VIVB0009.MS-NR                                 00280000
CCC004*              :NI-VIVB0009.NI-MS-NR                              00290000
CCC004*            ,:DCL-VIVB0009.MS-EINSTIEG                           00300000
CCC004*              :NI-VIVB0009.NI-MS-EINSTIEG                        00310000
CCC004*            ,:DCL-VIVB0009.MS-HPT                                00320000
CCC004*              :NI-VIVB0009.NI-MS-HPT                             00330000
CCC004*            ,:DCL-VIVB0009.MS-34C                                00340000
CCC004*              :NI-VIVB0009.NI-MS-34C                             00350000
061101*            ,:DCL-VIVB0009.MS-40PROZ-REGEL                       00360000
061101*              :NI-VIVB0009.NI-MS-40PROZ-REGEL                    00370000
CCC004*            ,:DCL-VIVB0009.MS-ABZPROZ-REGEL                      00360000
CCC004*              :NI-VIVB0009.NI-MS-ABZPROZ-REGEL                   00370000
CCC004*            ,:DCL-VIVB0009.MS-EING-EH-EIG                        00380000
CCC004*              :NI-VIVB0009.NI-MS-EING-EH-EIG                     00390000
CCC004*            ,:DCL-VIVB0009.MS-EH-EIG-GES                         00400000
CCC004*              :NI-VIVB0009.NI-MS-EH-EIG-GES                      00410000
CCC004*            ,:DCL-VIVB0009.MS-EH-EIG-PZTR                        00420000
CCC004*              :NI-VIVB0009.NI-MS-EH-EIG-PZTR                     00430000
CCC004*            ,:DCL-VIVB0009.MS-EH-GRU-PZTR                        00440000
CCC004*              :NI-VIVB0009.NI-MS-EH-GRU-PZTR                     00450000
CCC004*            ,:DCL-VIVB0009.MS-EH-GES-PZTR                        00460000
CCC004*              :NI-VIVB0009.NI-MS-EH-GES-PZTR                     00470000
CCC004*            ,:DCL-VIVB0009.MS-WEIT-EH-EIG                        00480000
CCC004*              :NI-VIVB0009.NI-MS-WEIT-EH-EIG                     00490000
CCC004*            ,:DCL-VIVB0009.MS-ZEITRAUM                           00500000
CCC004*              :NI-VIVB0009.NI-MS-ZEITRAUM                        00510000
CCC004*            ,:DCL-VIVB0009.MS-EQF-EIG                            00520000
CCC004*              :NI-VIVB0009.NI-MS-EQF-EIG                         00530000
CCC004*            ,:DCL-VIVB0009.MS-EQF-GRU                            00540000
CCC004*              :NI-VIVB0009.NI-MS-EQF-GRU                         00550000
CCC004*            ,:DCL-VIVB0009.MS-EQF-GES                            00560000
CCC004*              :NI-VIVB0009.NI-MS-EQF-GES                         00570000
CCC004*            ,:DCL-VIVB0009.MS-PROV-STF                           00580000
CCC004*              :NI-VIVB0009.NI-MS-PROV-STF                        00590000
CCC004*            ,:DCL-VIVB0009.MS-GUELTIG-BIS                        00600000
CCC004*              :NI-VIVB0009.NI-MS-GUELTIG-BIS                     00610000
CCC004*            ,:DCL-VIVB0009.MS-BEZ-KURZ                           00620000
CCC004*              :NI-VIVB0009.NI-MS-BEZ-KURZ                        00630000
CCC004*            ,:DCL-VIVB0009.MS-BEZ                                00640000
CCC004*              :NI-VIVB0009.NI-MS-BEZ                             00650000
CCC004*       FROM                                                      00660000
CCC004*             VIVB0009                                            00670000
CCC004*       WHERE                                                     00680000
CCC004*             MS_NR           =   :WK-MS-NR-UNTST                 00690000
CCC003*    END-EXEC                                                     00700000
                                                                        00710000
           END-PERFORM                                                  00720000
           .                                                            00730000
       MS-LESEN-EXIT.                                                   00740000
           EXIT.                                                        00750000
