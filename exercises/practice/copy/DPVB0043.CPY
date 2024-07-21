      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0043.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPVB0043.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:57:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:29:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: DPVB0043                                      * 00020000
      *    DIESE SECTION LIEST DIE BV                                 * 00021000
      *    --------------------------------------------------------   * 00021200
      *    ERSTELLT:  H LEISTE        (DVAG)                          * 00021500
      *    DATUM:    14.05.1993                                       * 00021600
      ***************************************************************** 00021800
      *    VERSION   01   16.09.93                                   *  00021900
      ***************************************************************** 00022000
      * AENDERUNGEN:                                                 *  00022100
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00022200
      * -------------------------------------------------------------*  00022300
      *         !16.09.93! 01 ! PROD.UEBERGABE            ! LEISTE   *  00022700
      *         !        !    !                           !          *  00022800
      *         !        !    !                           !          *  00022900
      *         !        !    !                           !          *  00023000
      *         !        !    !                           !          *  00023100
      *                                                              *  00023200
      * ENDE-LOGBUCH                                                 *  00023300
      ****************************************************************  00023400
      *----------------------------------------------------------
       BV-LESEN SECTION.
      *----------------------------------------------------------
       BV-LESEN-ANF.
CCC005*    EXEC SQL
CCC004*       SELECT
CCC004*             ST_NR,
CCC004*             KTY_BV_TYP,
CCC004*             KNR_BV_TYP,
CCC004*             BV_DAT_VON,
CCC004*             BV_LAST_UPDATE,
CCC004*             BV_DAT_BIS,
CCC004*             KTY_SPERRE,
CCC004*             KNR_SPERRE,
CCC004*             BV_KTO_NR,
CCC004*             BV_BLZ,
CCC004*             BV_BANKNAME,
CCC004*             BV_KTO_INHABER
CCC004*       INTO
CCC004*             :DCL-VIVB0021:NI-VIVB0021-ELE
CCC004*       FROM
CCC004*             VIVB0021
CCC004*       WHERE
CCC004*             ST_NR           =   :WB-VIVB0021.WB-ST-NR
CCC004*        AND  KNR_BV_TYP      =   :WB-VIVB0021.WB-KNR-BV-TYP
CCC004*        AND  BV_DAT_BIS      =   :K-DAT-BIS-MAX-ISO
CCC003*    END-EXEC
           .
       BV-LESEN-EXIT.
           EXIT.

