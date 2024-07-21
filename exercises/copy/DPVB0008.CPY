      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0008.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0008.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:21:34   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0008                                      * 00020000
      *    ERMITTELN EIGENPROVISIONSSATZ AUFGRUND DER VBP-NR          * 00021001
      *    --------------------------------------------------------   * 00022000
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00025000
      *    DATUM:    01.04.1993                                       * 00026000
      ***************************************************************** 00028000
      *    VERSION   01   16.09.93                                   *  00028102
      ****************************************************************  00028202
      * AENDERUNGEN:                                                 *  00028302
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00028402
      * -------------------------------------------------------------*  00028502
      *         !16.09.93! 01 ! NEU                       ! Naumann  *  00028602
      *         !        !    !                           !          *  00028702
      *         !        !    !                           !          *  00028802
      *         !        !    !                           !          *  00028902
      *         !        !    !                           !          *  00029002
      *         !        !    !                           !          *  00029102
      *         !        !    !                           !          *  00029202
      *         !        !    !                           !          *  00029302
      *                                                              *  00029402
      * ENDE-LOGBUCH                                                 *  00029502
      ****************************************************************  00029602
      *---------------------------------------------------------------  00029700
       PROVSATZ-VBPRAXIS SECTION.                                       00030000
      *---------------------------------------------------------------  00040000
      *-INPUT:  WK-VBP-NR                                            -  00050000
      *-OUTPUT: WK-PROV-AP-UNTST                                     -  00060000
      *---------------------------------------------------------------  00070000
       PROVSATZ-VBPRAXIS-ANF.                                           00080000
                                                                        00090000
           INITIALIZE VIVB0010-GESAMT                                   00100000
           MOVE ZERO  TO WK-PROV-AP-UNTST                               00110000
                                                                        00120000
           PERFORM  WITH TEST AFTER                                     00130000
                    VARYING  Ind-I  FROM   1  BY  1                     00140000
                                    UNTIL  IND-I   >  WK-LESEN-MAX      00150000
                                     OR    (SQLCODE  NOT =  -904)       00160000
CCC005*    EXEC SQL                                                     00170000
CCC004*       SELECT                                                    00180000
CCC004*             VBP_NR,                                             00190000
CCC004*             VBP_EH_EIG_PZTR,                                    00200000
CCC004*             VBP_EH_EIG_GES,                                     00210000
CCC004*             VBP_WEIT_EH_EIG,                                    00220000
CCC004*             VBP_ZEITRAUM,                                       00230000
CCC004*             VBP_EQF_EIG,                                        00240000
CCC004*             VBP_PROV_STF,                                       00250000
CCC004*             VBP_AUFSTIEG,                                       00260000
CCC004*             VBP_GUELTIG_BIS,                                    00270000
CCC004*             VBP_BEZ_KURZ,                                       00280000
CCC004*             VBP_BEZ                                             00290000
CCC004*       INTO                                                      00300000
CCC004*             :DCL-VIVB0010.VBP-NR                                00310000
CCC004*              :NI-VIVB0010.NI-VBP-NR                             00320000
CCC004*            ,:DCL-VIVB0010.VBP-EH-EIG-PZTR                       00330000
CCC004*              :NI-VIVB0010.NI-VBP-EH-EIG-PZTR                    00340000
CCC004*            ,:DCL-VIVB0010.VBP-EH-EIG-GES                        00350000
CCC004*              :NI-VIVB0010.NI-VBP-EH-EIG-GES                     00360000
CCC004*            ,:DCL-VIVB0010.VBP-WEIT-EH-EIG                       00370000
CCC004*              :NI-VIVB0010.NI-VBP-WEIT-EH-EIG                    00380000
CCC004*            ,:DCL-VIVB0010.VBP-ZEITRAUM                          00390000
CCC004*              :NI-VIVB0010.NI-VBP-ZEITRAUM                       00400000
CCC004*            ,:DCL-VIVB0010.VBP-EQF-EIG                           00410000
CCC004*              :NI-VIVB0010.NI-VBP-EQF-EIG                        00420000
CCC004*            ,:DCL-VIVB0010.VBP-PROV-STF                          00430000
CCC004*              :NI-VIVB0010.NI-VBP-PROV-STF                       00440000
CCC004*            ,:DCL-VIVB0010.VBP-AUFSTIEG                          00450000
CCC004*              :NI-VIVB0010.NI-VBP-AUFSTIEG                       00460000
CCC004*            ,:DCL-VIVB0010.VBP-GUELTIG-BIS                       00470000
CCC004*              :NI-VIVB0010.NI-VBP-GUELTIG-BIS                    00480000
CCC004*            ,:DCL-VIVB0010.VBP-BEZ-KURZ                          00490000
CCC004*              :NI-VIVB0010.NI-VBP-BEZ-KURZ                       00500000
CCC004*            ,:DCL-VIVB0010.VBP-BEZ                               00510000
CCC004*              :NI-VIVB0010.NI-VBP-BEZ                            00520000
CCC004*       FROM                                                      00530000
CCC004*             VIVB0010                                            00540000
CCC004*       WHERE                                                     00550000
CCC004*             VBP_NR          =   :WK-VBP-NR                      00560000
CCC003*    END-EXEC                                                     00570000
                                                                        00580000
           END-PERFORM                                                  00590000
                                                                        00600000
           EVALUATE   SQLCODE                                           00610000
           WHEN       0                                                 00620000
                 MOVE VBP-PROV-STF         TO WK-PROV-AP-UNTST          00630000
      *                                                                 00640000
TTMMJJ*          MOVE SQLCODE         TO WLR-SQL-CODE                   00650000
           WHEN       100                                               00660000
                 SET VERARB-LOG-FEHLER        TO TRUE                   00670000
                 MOVE 'VBP '                  TO WK-TABLE-NAME5         00680000
                 MOVE  VBP-NR IN DCL-VIVB0010 TO WK-KEY-ID5             00690000
                 MOVE  WK-FEHLER-TEXT5        TO WLR-FEHLER-TEXT        00700000
           WHEN       OTHER                                             00710000
                 SET VERARB-SQL-CODE-EX    TO TRUE                      00720000
                 MOVE SQLCODE              TO WLR-SQL-CODE              00730000
                 IF    SQLERRML       >   ZERO                          00740000
                     MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT         00750000
                  ELSE                                                  00760000
                      MOVE  SPACE          TO   WLR-FEHLER-TEXT         00770000
                 END-IF                                                 00780000
           END-EVALUATE                                                 00790000
           .                                                            00800000
       PROVSATZ-VBPRAXIS-EXIT.                                          00810000
           EXIT.                                                        00820000
