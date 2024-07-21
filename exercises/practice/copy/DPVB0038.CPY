      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0038.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0038.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:28:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0038                                      * 00020000
      *    DIESE SECTION LIEST DIE TABLE MAAN ZU EINEM KONKRETEN      * 00021000
      *    ZUSTELLUNGSTYPEN                                           * 00021100
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT: O. NAUMANN       (DVAG)                          * 00021500
      *    DATUM:    12.05.1993                                       * 00021600
      ***************************************************************** 00021800
      *    VERSION   02   01.10.93                                   *  00021906
      ***************************************************************** 00022002
      * AENDERUNGEN:                                                 *  00022101
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022201
      * -------------------------------------------------------------*  00022301
      *         !31.08.93!    ! SELECT-ANPASSUNG AN DEN BESCHLUß DER *  00022401
      *         !        !    ! PROJEKTSITZUNG VOM 07.05.1993        *  00022501
      *         !16.09.93! 01 ! PROD.UEBERGABE            ! NAUMANN  *  00022604
      *         !30.09.93!    ! ES DARF NICHT AUF DIE LETZTE AUSPRÄ- *  00022704
      *         !        !    ! GUNG DER MAAN BZGL. DES ZUSTELL.-TYP *  00022804
      *         !        !    ! GESUCHT WERDEN. ES MUß AUF DIE AKTU- *  00022904
      *         !        !    ! ELLE AUSPRÄGUNG GEGANGEN WERDEN!     *  00023004
      *         !        !    ! OHNE EINSCHRÄNKUNG AUF ZUSTELL-TYP   *  00023105
      *         !        !    !                           ! NAUMANN  *  00023204
      *         !01.10.93! 02 ! PROD.-UEBERGABE           ! NAUMANN  *  00023306
      *         !        !    !                           !          *  00023401
      *         !        !    !                           !          *  00023501
      *         !        !    !                           !          *  00023601
      *         !        !    !                           !          *  00023701
      *                                                              *  00023801
      * ENDE-LOGBUCH                                                 *  00023901
      ****************************************************************  00024001
      *---------------------------------------------------------------  00024100
       MAAN-LESEN SECTION.                                              00024200
      *---------------------------------------------------------------  00024300
      *-INPUT:  KNR-MAAN-TYP IN DCL-VIVB0015                            00024405
      *-OUTPUT: VIVB0015-GESAMT                                      -  00025000
      *---------------------------------------------------------------  00026000
       MAAN-LESEN-ANF.                                                  00026100
      *----------------------------------------------------------       00026200
CCC005*    EXEC SQL                                                     00026900
CCC004*       SELECT                                                    00027000
CCC004*             MAAN_BEZUGS_NR,                                     00028000
CCC004*             KTY_MAAN_TYP,                                       00029000
CCC004*             KNR_MAAN_TYP,                                       00030000
CCC004*             MAAN_DAT_VON,                                       00030100
CCC004*             MAAN_LAST_UPDATE,                                   00030200
CCC004*             MAAN_STRASSE,                                       00030300
CCC004*             MAAN_PLZ_AKTUELL,                                   00030400
CCC004*             MAAN_PLZ,                                           00030500
CCC004*             MAAN_ORT,                                           00030600
CCC004*             MAAN_PLZ_PF,                                        00030700
CCC004*             MAAN_ORT_PF,                                        00030800
CCC004*             MAAN_POSTFACH,                                      00030900
CCC004*             MAAN_WOHNT_BEI,                                     00031000
CCC004*             MAAN_DAT_BIS,                                       00031100
CCC004*             MAAN_TELEFON,                                       00031200
CCC004*             MAAN_TELEFAX,                                       00031300
CCC004*             KTY_AUSL,                                           00031400
CCC004*             KNR_AUSL,                                           00031500
CCC004*             KTY_GRND,                                           00031600
CCC004*             KNR_GRND,                                           00031700
CCC004*             KTY_ZUSTELLUNG,                                     00031800
CCC004*             KNR_ZUSTELLUNG,                                     00031900
CCC004*             KTY_BEZUGS_TYP,                                     00032000
CCC004*             KNR_BEZUGS_TYP                                      00032100
CCC004*       INTO                                                      00032200
CCC004*             :DCL-VIVB0015.MAAN-BEZUGS-NR                        00032300
CCC004*              :NI-VIVB0015.NI-MAAN-BEZUGS-NR                     00032400
CCC004*            ,:DCL-VIVB0015.KTY-MAAN-TYP                          00032500
CCC004*              :NI-VIVB0015.NI-KTY-MAAN-TYP                       00032600
CCC004*            ,:DCL-VIVB0015.KNR-MAAN-TYP                          00032700
CCC004*              :NI-VIVB0015.NI-KNR-MAAN-TYP                       00032800
CCC004*            ,:DCL-VIVB0015.MAAN-DAT-VON                          00032900
CCC004*              :NI-VIVB0015.NI-MAAN-DAT-VON                       00033000
CCC004*            ,:DCL-VIVB0015.MAAN-LAST-UPDATE                      00033100
CCC004*              :NI-VIVB0015.NI-MAAN-LAST-UPDATE                   00033200
CCC004*            ,:DCL-VIVB0015.MAAN-STRASSE                          00033300
CCC004*              :NI-VIVB0015.NI-MAAN-STRASSE                       00033400
CCC004*            ,:DCL-VIVB0015.MAAN-PLZ-AKTUELL                      00033500
CCC004*              :NI-VIVB0015.NI-MAAN-PLZ-AKTUELL                   00033600
CCC004*            ,:DCL-VIVB0015.MAAN-PLZ                              00033700
CCC004*              :NI-VIVB0015.NI-MAAN-PLZ                           00033800
CCC004*            ,:DCL-VIVB0015.MAAN-ORT                              00033900
CCC004*              :NI-VIVB0015.NI-MAAN-ORT                           00034000
CCC004*            ,:DCL-VIVB0015.MAAN-PLZ-PF                           00034100
CCC004*              :NI-VIVB0015.NI-MAAN-PLZ-PF                        00034200
CCC004*            ,:DCL-VIVB0015.MAAN-ORT-PF                           00034300
CCC004*              :NI-VIVB0015.NI-MAAN-ORT-PF                        00034400
CCC004*            ,:DCL-VIVB0015.MAAN-POSTFACH                         00034500
CCC004*              :NI-VIVB0015.NI-MAAN-POSTFACH                      00034600
CCC004*            ,:DCL-VIVB0015.MAAN-WOHNT-BEI                        00034700
CCC004*              :NI-VIVB0015.NI-MAAN-WOHNT-BEI                     00034800
CCC004*            ,:DCL-VIVB0015.MAAN-DAT-BIS                          00034900
CCC004*              :NI-VIVB0015.NI-MAAN-DAT-BIS                       00035000
CCC004*            ,:DCL-VIVB0015.MAAN-TELEFON                          00035100
CCC004*              :NI-VIVB0015.NI-MAAN-TELEFON                       00035200
CCC004*            ,:DCL-VIVB0015.MAAN-TELEFAX                          00035300
CCC004*              :NI-VIVB0015.NI-MAAN-TELEFAX                       00035400
CCC004*            ,:DCL-VIVB0015.KTY-AUSL                              00035500
CCC004*              :NI-VIVB0015.NI-KTY-AUSL                           00035600
CCC004*            ,:DCL-VIVB0015.KNR-AUSL                              00035700
CCC004*              :NI-VIVB0015.NI-KNR-AUSL                           00035800
CCC004*            ,:DCL-VIVB0015.KTY-GRND                              00035900
CCC004*              :NI-VIVB0015.NI-KTY-GRND                           00036000
CCC004*            ,:DCL-VIVB0015.KNR-GRND                              00036100
CCC004*              :NI-VIVB0015.NI-KNR-GRND                           00036200
CCC004*            ,:DCL-VIVB0015.KTY-ZUSTELLUNG                        00036300
CCC004*              :NI-VIVB0015.NI-KTY-ZUSTELLUNG                     00036400
CCC004*            ,:DCL-VIVB0015.KNR-ZUSTELLUNG                        00036500
CCC004*              :NI-VIVB0015.NI-KNR-ZUSTELLUNG                     00036600
CCC004*            ,:DCL-VIVB0015.KTY-BEZUGS-TYP                        00036700
CCC004*              :NI-VIVB0015.NI-KTY-BEZUGS-TYP                     00036800
CCC004*            ,:DCL-VIVB0015.KNR-BEZUGS-TYP                        00036900
CCC004*              :NI-VIVB0015.NI-KNR-BEZUGS-TYP                     00037000
CCC004*       FROM                                                      00037100
CCC004*             VIVB0015 MAAN1                                      00037200
CCC004*       WHERE                                                     00037300
CCC004*             MAAN_BEZUGS_NR   =  :MAAN-BEZUGS-NR                 00037400
CCC004*         AND MAAN_DAT_BIS     =  :K-DAT-BIS-MAX-ISO              00037604
CCC004*         AND KNR_MAAN_TYP     =  :KNR-MAAN-TYP                   00037705
300993*         AND KNR_ZUSTELLUNG   =  :KNR-ZUSTELLUNG                 00037805
300993*         AND MAAN_LAST_UPDATE =                                  00037904
300993*              (SELECT MAX(MAAN2.MAAN_LAST_UPDATE)                00038004
300993*               FROM                                              00038104
300993*                  VIVB0015 MAAN2                                 00038204
300993*               WHERE MAAN1.MAAN_BEZUGS_NR =                      00038304
300993*                     MAAN2.MAAN_BEZUGS_NR                        00038404
300993*                AND MAAN2.KNR_ZUSTELLUNG   = :KNR-ZUSTELLUNG     00038504
300993*              )                                                  00038604
CCC003*    END-EXEC                                                     00039300
           .                                                            00039400
       MAAN-LESEN-EXIT.                                                 00040000
           EXIT.                                                        00050000
