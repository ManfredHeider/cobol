      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0004.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0004.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:20:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0004                                      * 00020000
      *    LESEN DER ED-PROVISION DES ED-BETREUERS, DER EIN MA IST,   * 00022003
      *    AUS DER MAT, DIE AKTUELL                                   * 00023009
      *    A) UNMITTELBAR VOR DEM ZEITPUNKT BZW.                      * 00023109
      *    B) ZU DEM ZEITPUNKT                                        * 00023209
      *    GALT/GILT.                                                 * 00023309
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00060000
      *    DATUM:    01.04.1993                                       * 00070000
      ***************************************************************** 00090000
      *    VERSION   01   16.09.93                                   *  00090111
      ***************************************************************** 00090211
      * AENDERUNGEN:                                                 *  00091010
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00092010
      * -------------------------------------------------------------*  00093010
      *         !01.04.93!    ! NEU                       ! NAUMANN  *  00094011
      *         !31.08.93!    ! SELECT-ANPASSUNG AN DEN BESCHLUß DER *  00095010
      *         !        !    ! PROJEKTSITZUNG VOM 07.05.1993        *  00096010
      *         !16.09.93! 01 ! Prod.uebergabe            ! NAUMANN  *  00096111
      *         !        !    !                           !          *  00097010
      *         !        !    !                           !          *  00098010
      *         !        !    !                           !          *  00099010
      *         !        !    !                           !          *  00099110
      *                                                              *  00099210
      * ENDE-LOGBUCH                                                 *  00099310
      ****************************************************************  00099410
      *---------------------------------------------------------------  00100000
       MA-BETR-ED-PROV-LESEN SECTION.                                   00110003
      *---------------------------------------------------------------  00120000
      *-INPUT:  WK-MA-ST-NR-BETR                                     -  00121008
310893*-        Z-ZTPKT-TSTMP NICHT MEHR!                            -  00122010
310893*-        Z-ZTPKT-DATE                                         -  00123010
      *---------------------------------------------------------------  00140000
      *-OUTPUT: WB-MAT-BETR                                          -  00150003
      *---------------------------------------------------------------  00160000
       MA-BETR-ED-PROV-LESEN-ANF.                                       00170003
           INITIALIZE WB-MAT-BETR                                       00190003
                                                                        00220000
           PERFORM  WITH TEST AFTER                                     00230000
                    VARYING  IND-I  FROM   1  BY  1                     00240000
                                    UNTIL  IND-I   >  WK-LESEN-MAX      00250000
                                     OR    (SQLCODE  NOT =  -904)       00260000
CCC005*    EXEC SQL                                                     00280003
CCC004*       SELECT                                                    00290003
CCC004*             MA_ST_NR,                                           00300003
CCC004*             MAT_DAT_VON,                                        00310003
CCC004*             MAT_LAST_UPDATE,                                    00320003
CCC004*             MAT_DAT_BIS,                                        00330003
CCC004*             MAT_KUE_DAT_VOM,                                    00340003
CCC004*             MAT_EING_KUE_DAT,                                   00350003
CCC004*             MAT_VORZFREI_DAT,                                   00360003
CCC004*             KTY_MAT_AUS_GRND,                                   00370003
CCC004*             KNR_MAT_AUS_GRND,                                   00380003
CCC004*             MAT_L_RCK_STZ,                                      00390003
CCC004*             MAT_ED_PROV1,                                       00400003
CCC004*             MAT_ED_PROV2,                                       00410003
CCC004*             MAT_ED_PROV_DAT1,                                   00420003
CCC004*             MAT_ED_PROV_DAT2                                    00430003
CCC004*       INTO                                                      00440006
CCC004*             :DCL-VIVB0049.MA-ST-NR                              00739906
CCC004*              :NI-VIVB0049.NI-MA-ST-NR                           00740006
CCC004*            ,:DCL-VIVB0049.MAT-DAT-VON                           00740106
CCC004*              :NI-VIVB0049.NI-MAT-DAT-VON                        00740206
CCC004*            ,:DCL-VIVB0049.MAT-LAST-UPDATE                       00740306
CCC004*              :NI-VIVB0049.NI-MAT-LAST-UPDATE                    00740406
CCC004*            ,:DCL-VIVB0049.MAT-DAT-BIS                           00740506
CCC004*              :NI-VIVB0049.NI-MAT-DAT-BIS                        00740606
CCC004*            ,:DCL-VIVB0049.MAT-KUE-DAT-VOM                       00740706
CCC004*              :NI-VIVB0049.NI-MAT-KUE-DAT-VOM                    00740806
CCC004*            ,:DCL-VIVB0049.MAT-EING-KUE-DAT                      00740906
CCC004*              :NI-VIVB0049.NI-MAT-EING-KUE-DAT                   00741006
CCC004*            ,:DCL-VIVB0049.MAT-VORZFREI-DAT                      00741106
CCC004*              :NI-VIVB0049.NI-MAT-VORZFREI-DAT                   00741206
CCC004*            ,:DCL-VIVB0049.KTY-MAT-AUS-GRND                      00741306
CCC004*              :NI-VIVB0049.NI-KTY-MAT-AUS-GRND                   00741406
CCC004*            ,:DCL-VIVB0049.KNR-MAT-AUS-GRND                      00741506
CCC004*              :NI-VIVB0049.NI-KNR-MAT-AUS-GRND                   00741606
CCC004*            ,:DCL-VIVB0049.MAT-L-RCK-STZ                         00741706
CCC004*              :NI-VIVB0049.NI-MAT-L-RCK-STZ                      00741806
CCC004*            ,:DCL-VIVB0049.MAT-ED-PROV1                          00741906
CCC004*              :NI-VIVB0049.NI-MAT-ED-PROV1                       00742006
CCC004*            ,:DCL-VIVB0049.MAT-ED-PROV2                          00742106
CCC004*              :NI-VIVB0049.NI-MAT-ED-PROV2                       00742206
CCC004*            ,:DCL-VIVB0049.MAT-ED-PROV-DAT1                      00742306
CCC004*              :NI-VIVB0049.NI-MAT-ED-PROV-DAT1                   00742406
CCC004*            ,:DCL-VIVB0049.MAT-ED-PROV-DAT2                      00742506
CCC004*              :NI-VIVB0049.NI-MAT-ED-PROV-DAT2                   00742606
CCC004*       FROM                                                      00750003
CCC004*             VIVB0049 MAT1                                       00760008
CCC004*       WHERE                                                     01040000
CCC004*             MA_ST_NR        =   :WK-MA-ST-NR-BETR               01050006
CCC004*       AND   MAT_LAST_UPDATE =                                   01050110
CCC004*             (SELECT MAX(MAT2.MAT_LAST_UPDATE)                   01050210
CCC004*              FROM                                               01050310
CCC004*                 VIVB0049 MAT2                                   01050410
CCC004*              WHERE  MAT1.MA_ST_NR = MAT2.MA_ST_NR               01050510
CCC004*                AND  DATE(MAT2.MAT_DAT_VON) <= :Z-ZTPKT-DATE     01050610
CCC004*             )                                                   01050710
310893*        AND  MAT1.MAT_DAT_VON     =                              01050910
310893*             (SELECT MAX(MAT2.MAT_DAT_VON)                       01051010
310893*              FROM                                               01051110
310893*                 VIVB0049 MAT2                                   01051210
310893*              WHERE MAT1.MA_ST_NR       = MAT2.MA_ST_NR          01051310
310893*                AND MAT2.MAT_DAT_VON   <= :Z-ZTPKT-TSTMP         01051410
310893*             )                                                   01051510
CCC003*    END-EXEC                                                     01060000
                                                                        01070000
           END-PERFORM                                                  01080000
                                                                        01090000
           EVALUATE   SQLCODE                                           01100000
           WHEN       0                                                 01110000
                 MOVE VIVB0049-GESAMT              TO WB-MAT-BETR       01120007
           WHEN       100                                               01130000
                 SET VERARB-LOG-FEHLER             TO TRUE              01131001
                 MOVE 'MAT '                       TO WK-TABLE-NAME5    01132003
                 MOVE  WB-MA-ST-NR IN WB-MAT-BETR  TO WK-KEY-ID5        01133003
                 MOVE  WK-FEHLER-TEXT5             TO WLR-FEHLER-TEXT   01134001
           WHEN       OTHER                                             01170000
                 SET VERARB-SQL-CODE-EX    TO TRUE                      01180000
                 MOVE SQLCODE              TO WLR-SQL-CODE              01190000
                 IF    SQLERRML       >   ZERO                          01200000
                     MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT         01210000
                  ELSE                                                  01220000
                      MOVE  SPACE          TO   WLR-FEHLER-TEXT         01230000
                 END-IF                                                 01240000
           END-EVALUATE                                                 01250000
           .                                                            01260000
       MA-BETR-ED-PROV-LESEN-EXIT.                                      01270003
           EXIT.                                                        01280000
