      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0006.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0006.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:21:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0006                                      * 00020000
      *    LESEN DES MA-BETREUERS, DER EIN KL IST                     * 00021001
      *    --------------------------------------------------------   * 00022000
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00025000
      *    DATUM:    01.04.1993                                       * 00026000
      ***************************************************************** 00028000
      *    VERSION   01   16.09.93                                   *  00028103
      ****************************************************************  00028203
      * AENDERUNGEN:                                                 *  00028303
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00028403
      * -------------------------------------------------------------*  00028503
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *  00028603
      *         !        !    !                           !          *  00028703
      *         !        !    !                           !          *  00028803
      *         !        !    !                           !          *  00028903
      *         !        !    !                           !          *  00029003
      *         !        !    !                           !          *  00029103
      *         !        !    !                           !          *  00029203
      *         !        !    !                           !          *  00029303
      *                                                              *  00029403
      * ENDE-LOGBUCH                                                 *  00029503
      ****************************************************************  00029603
      *---------------------------------------------------------------  00029701
       KL-BETR-LESEN SECTION.                                           00030001
      *---------------------------------------------------------------  00040001
      *-INPUT:  BVH-BETR-NR                                          -  00050001
      *---------------------------------------------------------------  00060001
      *-OUTPUT: WB-KL-BETR                                          -   00070001
      *---------------------------------------------------------------  00080001
       KL-BETR-LESEN-ANF.                                               00090001
           INITIALIZE WB-KL-BETR                                        00100001
                                                                        00110001
           MOVE     BVH-BETR-NR           TO WB-KL-ST-NR IN WB-KL-BETR  00120001
                                                                        00130001
           PERFORM  WITH TEST AFTER                                     00140001
                    VARYING  IND-I  FROM   1  BY  1                     00150001
                                    UNTIL  IND-I   >  WK-LESEN-MAX      00160001
                                     OR    (SQLCODE  NOT =  -904)       00170001
CCC005*    EXEC SQL                                                     00180001
CCC004*       SELECT                                                    00190001
CCC004*             KL_ST_NR,                                           00200001
CCC004*             KL_LAST_UPDATE,                                     00210001
CCC004*             KL_DAT_VON,                                         00220001
CCC004*             KL_DAT_BIS,                                         00230001
CCC004*             KL_PROV_STZ_AP,                                     00240001
CCC004*             KL_PROV_STZ_GP,                                     00250001
CCC004*             KL_PROZ_STZ,                                        00260001
CCC004*             KL_RCK_STZ_DAT,                                     00270001
CCC004*             KL_RCK_STZ                                          00280001
CCC004*       INTO                                                      00290001
CCC004*             :WB-VIVB0040.WB-KL-ST-NR                            00300002
CCC004*              :WB-NI-VIVB0040.WB-NI-KL-ST-NR                     00310002
CCC004*            ,:WB-VIVB0040.WB-KL-LAST-UPDATE                      00320002
CCC004*              :WB-NI-VIVB0040.WB-NI-KL-LAST-UPDATE               00330002
CCC004*            ,:WB-VIVB0040.WB-KL-DAT-VON                          00340002
CCC004*              :WB-NI-VIVB0040.WB-NI-KL-DAT-VON                   00350002
CCC004*            ,:WB-VIVB0040.WB-KL-DAT-BIS                          00360002
CCC004*              :WB-NI-VIVB0040.WB-NI-KL-DAT-BIS                   00370002
CCC004*            ,:WB-VIVB0040.WB-KL-PROV-STZ-AP                      00380002
CCC004*              :WB-NI-VIVB0040.WB-NI-KL-PROV-STZ-AP               00390002
CCC004*            ,:WB-VIVB0040.WB-KL-PROV-STZ-GP                      00400002
CCC004*              :WB-NI-VIVB0040.WB-NI-KL-PROV-STZ-GP               00410002
CCC004*            ,:WB-VIVB0040.WB-KL-PROZ-STZ                         00420002
CCC004*              :WB-NI-VIVB0040.WB-NI-KL-PROZ-STZ                  00430002
CCC004*            ,:WB-VIVB0040.WB-KL-RCK-STZ-DAT                      00440002
CCC004*              :WB-NI-VIVB0040.WB-NI-KL-RCK-STZ-DAT               00450002
CCC004*            ,:WB-VIVB0040.WB-KL-RCK-STZ                          00460002
CCC004*              :WB-NI-VIVB0040.WB-NI-KL-RCK-STZ                   00470002
CCC004*       FROM                                                      00480001
CCC004*             VIVB0040                                            00490001
CCC004*       WHERE                                                     00500001
CCC004*             KL_ST_NR        =   :WB-VIVB0040.WB-KL-ST-NR        00510001
CCC003*    END-EXEC                                                     00520001
                                                                        00530001
           END-PERFORM                                                  00540001
                                                                        00550001
           EVALUATE   SQLCODE                                           00560001
           WHEN       0                                                 00570001
                 CONTINUE                                               00580001
           WHEN       100                                               00590001
                 SET VERARB-LOG-FEHLER     TO TRUE                      00600001
                 MOVE 'KL  '                       TO WK-TABLE-NAME5    00610001
                 MOVE  WB-KL-ST-NR IN WB-KL-BETR   TO WK-KEY-ID5        00620001
                 MOVE  WK-FEHLER-TEXT5             TO WLR-FEHLER-TEXT   00630001
           WHEN       OTHER                                             00640001
                 SET VERARB-SQL-CODE-EX    TO TRUE                      00650001
                 MOVE SQLCODE              TO WLR-SQL-CODE              00660001
                 IF    SQLERRML       >   ZERO                          00670001
                     MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT         00680001
                  ELSE                                                  00690001
                      MOVE  SPACE          TO   WLR-FEHLER-TEXT         00700001
                 END-IF                                                 00710001
           END-EVALUATE                                                 00720001
           .                                                            00730001
       KL-BETR-LESEN-EXIT.                                              00740001
           EXIT.                                                        00750001
