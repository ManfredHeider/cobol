      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0048.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0048.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:30:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0048                                      * 00020000
      *    ERMITTELN DER DATENFELDER MAN-VORNAME + MAN-ANREDE         * 00021000
      *    FUER POSTANSCHRIFT ED-KONTO.                               * 00021100
      *    --------------------------------------------------------   * 00021300
      *    ERSTELLT: P. SCHMIDT       (DVAG)                          * 00021600
      *    DATUM:    25.08.1993                                       * 00021700
      ***************************************************************** 00021900
      *    VERSION   01   16.09.93                                   *  00022006
      ***************************************************************** 00022106
      * AENDERUNGEN:                                                 *  00022205
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022305
      * -------------------------------------------------------------*  00022405
      *         !25.08.93! 01 ! neu                       ! schmidt  *  00022506
      *         !31.08.93!    ! SELECT-ANPASSUNG AN DEN BESCHLUß DER *  00022606
      *         !        !    ! PROJEKTSITZUNG VOM 07.05.1993        *  00022705
      *         !        !    !                           ! NAUMANN  *  00022805
      *         !        !    !                           !          *  00022905
      *         !        !    !                           !          *  00023005
      *         !        !    !                           !          *  00023105
      *         !        !    !                           !          *  00023205
      *                                                              *  00023305
      * ENDE-LOGBUCH                                                 *  00023405
      ****************************************************************  00023505
      *----------------------------------------------------------       00023600
       ED-ANREDE-ERMITT SECTION.                                        00023700
      *----------------------------------------------------------       00023800
       ED-ANREDE-ERMITT-ANF.                                            00023900
           PERFORM  WITH TEST AFTER                                     00024000
                    VARYING  IND-I  FROM   1  BY  1                     00024100
                                    UNTIL  IND-I   >  WK-LESEN-MAX      00024200
                                     OR    (SQLCODE  NOT =  -904)       00025000
CCC005*    EXEC SQL                                                     00026000
CCC004*       SELECT                                                    00027000
CCC004*             KNR_ANREDE,                                         00090000
CCC004*             MAN_NAME,                                           00130003
CCC004*             MAN_VORNAME                                         00131003
CCC004*       INTO                                                      00140000
CCC004*             :DCL-VIVB0014.KNR-ANREDE                            00310000
CCC004*              :NI-VIVB0014.NI-KNR-ANREDE                         00320000
CCC004*            ,:DCL-VIVB0014.MAN-NAME                              00390003
CCC004*              :NI-VIVB0014.NI-MAN-NAME                           00400003
CCC004*            ,:DCL-VIVB0014.MAN-VORNAME                           00401003
CCC004*              :NI-VIVB0014.NI-MAN-VORNAME                        00402003
CCC004*       FROM                                                      00410000
CCC004*             VIVB0014 MAN1                                       00420000
CCC004*       WHERE                                                     00430000
CCC004*             MAN_BEZUGS_NR   =   :Z-ST-NR                        00440002
CCC004*         AND MAN_LAST_UPDATE =                                   00441005
CCC004*              (SELECT MAX(MAN2.MAN_LAST_UPDATE)                  00442005
CCC004*               FROM                                              00443005
CCC004*                  VIVB0014 MAN2                                  00444005
CCC004*               WHERE MAN1.MAN_BEZUGS_NR  =                       00445005
CCC004*                     MAN2.MAN_BEZUGS_NR                          00446005
CCC004*              )                                                  00447005
310893*         AND MAN_DAT_VON     =                                   00450005
310893*              (SELECT MAX(MAN2.MAN_DAT_VON)                      00460005
310893*               FROM                                              00470005
310893*                  VIVB0014 MAN2                                  00480005
310893*               WHERE MAN1.MAN_BEZUGS_NR  =                       00490005
310893*                     MAN2.MAN_BEZUGS_NR                          00500005
310893*                 AND MAN2.MAN_DAT_VON   <= CURRENT TIMESTAMP     00510005
310893*              )                                                  00520005
CCC003*    END-EXEC                                                     00530000
                                                                        00540000
           END-PERFORM                                                  00550000
                                                                        00560000
           EVALUATE   SQLCODE                                           00570000
           WHEN       0                                                 00580000
      *                                                                 00682000
              STRING MAN-VORNAME(1:1) DELIMITED BY SIZE                 00682104
                     '.'              DELIMITED BY SIZE                 00682204
                     MAN-NAME(1:18)   DELIMITED BY SIZE                 00682304
              INTO   MVNAME                                             00682403
      *                                                                 00683003
              MOVE KNR-ANREDE           TO MANREDE                      00710000
      *                                                                 00720000
           WHEN       100                                               00740000
              CONTINUE                                                  00750000
           WHEN       OTHER                                             00760000
              SET VERARB-SQL-CODE-EX    TO TRUE                         00770000
              MOVE SQLCODE              TO WLR-SQL-CODE                 00780000
              IF    SQLERRML       >   ZERO                             00790000
                  MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT            00800000
              ELSE                                                      00810000
                  MOVE  SPACE          TO   WLR-FEHLER-TEXT             00820000
              END-IF                                                    00830000
           END-EVALUATE                                                 00840000
           .                                                            00841000
       ED-ANREDE-ERMITT-EXIT.                                           00850000
           EXIT.                                                        00860000
