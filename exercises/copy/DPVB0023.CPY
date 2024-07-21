      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0023.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0023.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:25:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0023                                      * 00020000
      *    KONVERTIEREN KL BZW. JUR NACH MA (IM ALTSYSTEM)            * 00021000
      *    MIT EINER GÜLTIGEN MSTUFE UND EVTL. EINER GÜLTIGEN         * 00021100
      *    VB-PRAXIS-STUFE                                            * 00021200
      *    --------------------------------------------------------   * 00021300
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021600
      *    DATUM:    02.04.1993                                       * 00021700
      ***************************************************************** 00021900
      *    VERSION   01   16.09.93                                   *  00022004
      ****************************************************************  00022102
      * AENDERUNGEN:                                                 *  00022202
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022302
      * -------------------------------------------------------------*  00022402
      *         !12.08.93!    !Z-VARPROV MUß IMMER DANN   ! NAUMANN  *  00022504
      *         !        !    !ERMITTELT WERDEN, WENN AUS !          *  00022602
      *         !        !    !KL EIN RF-R RÜCKKONVERTIERT!          *  00022702
      *         !        !    !WIRD.                      !          *  00022802
      *         !16.09.93! 01 !Prod.uebergabe             ! NAUMANN  *  00022904
      *         !        !    !                           !          *  00023002
      *         !        !    !                           !          *  00023102
      *                                                              *  00023202
      * ENDE-LOGBUCH                                                 *  00023302
      ****************************************************************  00023402
      *----------------------------------------------------------       00023500
       KL-JUR-KONVERT SECTION.                                          00023600
      *----------------------------------------------------------       00023700
      * KL-JUR-KONVERT                                             *    00023800
      *------------------------------------------------------------*    00023900
      * INPUT : Z-ST-NR                                            *    00024001
      *         Z-PROV-STZ-AP, Z-PROV-STZ-GP, Z-PROZ-STZ           *    00024101
      *                                                            *    00024200
      * OUTPUT: Z-STUFE, Z-EPROVKZ, Z-VARPROV                      *    00024302
      *         WLR-RUECK-CODE                                     *    00024400
      *         WLR-FEHLER-TEXT, FALLS KEINE KONVERTIERUNG VON     *    00024500
      *         KL BZW. JUR NACH MA MÖGLICH IST                    *    00025000
      *                                                            *    00026000
      * VERARBEITUNG:                                              *    00027000
      *   -                                                        *    00028000
      *   -                                                        *    00029000
      *   -                                                        *    00030000
      *                                                            *    00040000
      *------------------------------------------------------------*    00050000
       KL-JUR-KONVERT-ANF.                                              00070000
      *----------------------------------------------------------       00080000
           MOVE     ZERO   TO Z-STUFE                                   00090000
120893                        Z-VARPROV                                 00091002
           MOVE     SPACE  TO Z-EPROVKZ                                 00100000
                                                                        00110000
           EVALUATE Z-PROV-STZ-AP > ZERO                                00120000
               ALSO Z-PROV-STZ-GP > ZERO                                00130000
               ALSO Z-PROZ-STZ    > ZERO                                00140000
           WHEN     TRUE  ALSO TRUE  ALSO TRUE                          00150000
      *    SOWOHL EIGEN- ALS AUCH GRUPPENPROVISION                      00160000
              PERFORM UPVB0022-BEARB                                    00170000
              PERFORM UPVB0023-BEARB                                    00180000
           WHEN     FALSE ALSO TRUE  ALSO TRUE                          00190000
      *    ES EX. NUR GRUPPENPROVISION                                  00200000
              PERFORM UPVB0022-BEARB                                    00210000
           WHEN     TRUE  ALSO TRUE  ALSO FALSE                         00220000
           WHEN     TRUE  ALSO FALSE ALSO FALSE                         00230000
           WHEN     FALSE ALSO TRUE  ALSO FALSE                         00240000
      *    ES WIRD RF-R SIMULIERT                                       00250000
120893*    ZUSÄTZLICH WIRD DER SOG. VAR. PROV.-SATZ ERMITTELT           00251002
              MOVE  K-RF-R TO  Z-STUFE                                  00260000
120893        IF  Z-PROV-STZ-GP     >=   Z-PROV-STZ-AP                  00261002
120893            MOVE Z-PROV-STZ-GP     TO Z-VARPROV                   00262002
120893        ELSE                                                      00263002
120893            MOVE Z-PROV-STZ-AP     TO Z-VARPROV                   00264002
120893        END-IF                                                    00265002
           WHEN     FALSE ALSO FALSE ALSO TRUE                          00270000
           WHEN     FALSE ALSO FALSE ALSO FALSE                         00280000
           WHEN     TRUE  ALSO FALSE ALSO TRUE                          00290000
      *    DIESE KONSTELLATION DARF NICHT AUFTRETEN                     00300000
              SET VERARB-LOG-FEHLER     TO TRUE                         00310000
              MOVE 'KL-JUR-KONVERT'     TO WK-PARAGRAPH                 00311000
              MOVE  WK-FEHLER-TEXT6     TO WLR-FEHLER-TEXT              00312000
           END-EVALUATE                                                 00313000
           .                                                            00320000
       KL-JUR-KONVERT-EXIT.                                             00321000
           EXIT.                                                        00340000
