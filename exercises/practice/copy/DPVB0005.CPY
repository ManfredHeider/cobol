      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0005.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0005.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:20:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0005                                      * 00020000
      *    LESEN DES MA-BETREUERS, DER EIN RFT IST                    * 00021001
      *    --------------------------------------------------------   * 00022000
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00025000
      *    DATUM:    01.04.1993                                       * 00026000
      ***************************************************************** 00028000
      *    VERSION   01   16.09.93                                   *  00028103
      ****************************************************************  00028203
      * AENDERUNGEN:                                                 *  00028303
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00028403
      * -------------------------------------------------------------*  00028503
      *         !16.09.93! 01 ! NEU                       ! Naumann  *  00028603
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
       RFT-BETR-LESEN SECTION.                                          00030001
      *---------------------------------------------------------------  00040001
      *-INPUT:  BVH-BETR-NR                                          -  00050001
      *---------------------------------------------------------------  00060001
      *-OUTPUT: WB-RFT-BETR                                          -  00070001
      *---------------------------------------------------------------  00080001
       RFT-BETR-LESEN-ANF.                                              00090001
                                                                        00100001
           INITIALIZE WB-RFT-BETR                                       00110001
                                                                        00120001
           MOVE     BVH-BETR-NR           TO WB-RFT-ST-NR IN WB-RFT-BETR00130001
                                                                        00140001
           PERFORM  WITH TEST AFTER                                     00150001
                    VARYING  Ind-I  FROM   1  BY  1                     00160001
                                    UNTIL  IND-I   >  WK-LESEN-MAX      00170001
                                     OR    (SQLCODE  NOT =  -904)       00180001
CCC005*    EXEC SQL                                                     00190001
CCC004*       SELECT                                                    00200001
CCC004*             RFT_ST_NR,                                          00210001
CCC004*             RFT_LAST_UPDATE,                                    00220001
CCC004*             MA_ST_NR,                                           00230001
CCC004*             RFT_PROV_STZ_AP,                                    00240001
CCC004*             RFT_PROV_STZ_GP,                                    00250001
CCC004*             RFT_PROZ_STZ,                                       00260001
CCC004*             RFT_RCK_STZ,                                        00270001
CCC004*             RFT_RCK_STZ_DAT,                                    00280001
CCC004*             RFT_DAT_VON,                                        00290001
CCC004*             RFT_DAT_BIS,                                        00300001
CCC004*             RFT_ABLAUF_DAT                                      00310001
CCC004*       INTO                                                      00320001
CCC004*             :WB-VIVB0005.WB-RFT-ST-NR                           00330002
CCC004*              :WB-NI-VIVB0005.WB-NI-RFT-ST-NR                    00340002
CCC004*            ,:WB-VIVB0005.WB-RFT-LAST-UPDATE                     00350002
CCC004*              :WB-NI-VIVB0005.WB-NI-RFT-LAST-UPDATE              00360002
CCC004*            ,:WB-VIVB0005.WB-MA-ST-NR                            00370002
CCC004*              :WB-NI-VIVB0005.WB-NI-MA-ST-NR                     00380002
CCC004*            ,:WB-VIVB0005.WB-RFT-PROV-STZ-AP                     00390002
CCC004*              :WB-NI-VIVB0005.WB-NI-RFT-PROV-STZ-AP              00400002
CCC004*            ,:WB-VIVB0005.WB-RFT-PROV-STZ-GP                     00410002
CCC004*              :WB-NI-VIVB0005.WB-NI-RFT-PROV-STZ-GP              00420002
CCC004*            ,:WB-VIVB0005.WB-RFT-PROZ-STZ                        00430002
CCC004*              :WB-NI-VIVB0005.WB-NI-RFT-PROZ-STZ                 00440002
CCC004*            ,:WB-VIVB0005.WB-RFT-RCK-STZ                         00450002
CCC004*              :WB-NI-VIVB0005.WB-NI-RFT-RCK-STZ                  00460002
CCC004*            ,:WB-VIVB0005.WB-RFT-RCK-STZ-DAT                     00470002
CCC004*              :WB-NI-VIVB0005.WB-NI-RFT-RCK-STZ-DAT              00480002
CCC004*            ,:WB-VIVB0005.WB-RFT-DAT-VON                         00490002
CCC004*              :WB-NI-VIVB0005.WB-NI-RFT-DAT-VON                  00500002
CCC004*            ,:WB-VIVB0005.WB-RFT-DAT-BIS                         00510002
CCC004*              :WB-NI-VIVB0005.WB-NI-RFT-DAT-BIS                  00520002
CCC004*            ,:WB-VIVB0005.WB-RFT-ABLAUF-DAT                      00530002
CCC004*              :WB-NI-VIVB0005.WB-NI-RFT-ABLAUF-DAT               00540002
CCC004*       FROM                                                      00550001
CCC004*             VIVB0005                                            00560001
CCC004*       WHERE                                                     00570001
CCC004*             RFT_ST_NR        =   :WB-VIVB0005.WB-RFT-ST-NR      00580001
CCC003*    END-EXEC                                                     00590001
                                                                        00600001
           END-PERFORM                                                  00610001
                                                                        00620001
           EVALUATE   SQLCODE                                           00630001
           WHEN       0                                                 00640001
                 CONTINUE                                               00650001
           WHEN       100                                               00660001
                 SET VERARB-LOG-FEHLER             TO TRUE              00670001
                 MOVE 'RFT '                       TO WK-TABLE-NAME5    00680001
                 MOVE  WB-RFT-ST-NR IN WB-RFT-BETR TO WK-KEY-ID5        00690001
                 MOVE  WK-FEHLER-TEXT5             TO WLR-FEHLER-TEXT   00700001
           WHEN       OTHER                                             00710001
                 SET VERARB-SQL-CODE-EX    TO TRUE                      00720001
                 MOVE SQLCODE              TO WLR-SQL-CODE              00730001
                 IF    SQLERRML       >   ZERO                          00740001
                     MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT         00750001
                  ELSE                                                  00760001
                      MOVE  SPACE          TO   WLR-FEHLER-TEXT         00770001
                 END-IF                                                 00780001
           END-EVALUATE                                                 00790001
           .                                                            00800001
       RFT-BETR-LESEN-EXIT.                                             00810001
           EXIT.                                                        00820001
