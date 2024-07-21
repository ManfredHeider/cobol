      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0020.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DPVB0020.CPv  $
      *
      *   Rev 2.2   Sep 26 2003 15:00:22   t13ex22
      *AVB00568: Erf. "Anwerber" VB11/13;Anz. im UU11; Änderung im VB21
      *
      *   Rev 2.1   Jul 18 2003 15:03:56   t13ex22
      *AVB00666.UE029: VBAS und ALP in QMF un stat. Aufrufe entfernt
      *
      *   Rev 2.0   Feb 27 1999 15:56:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 28 1996 15:20:46   T13AE26
      *AFRZ0023: Anpassung an MF-Cobol Workbench
      *          (Nested Programs entf.)
      *
      *   Rev 1.0   Nov 04 1995 08:24:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPVB0020                                      *
      *    LESEN DER TABLE MA                                         *
      *    UND FÜLLEN DER RELEVANTEN DATENFELDER AUS WLR-VB1-STRUK    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: O. NAUMANN       (DVAG)                          *
      *    DATUM:    01.04.1993                                       *
      *****************************************************************
      *    VERSION   01   16.09.93                                   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *
      * AFRZ0023!03.11.95!    ! Anpassung an MF-Cobol-    !          *
      *         !        ! 02 !  WORKBENCH                ! GERLACH  *
      *         !        !    !                           !          *
      * AVB00666!17.07.03! 2.1! STAT. AUFRUF ENTFERNT     ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *----------------------------------------------------------
       MA-ERMITT SECTION.
      *----------------------------------------------------------
       MA-ERMITT-ANF.
           PERFORM  WITH TEST AFTER
                    VARYING  IND-I  FROM   1  BY  1
                                    UNTIL  IND-I   >  WK-LESEN-MAX
                                     OR    (SQLCODE  NOT =  -904)
CCC005*    EXEC SQL
CCC004*       SELECT
CCC004*             MA_ST_NR,
CCC004*             MA_LAST_UPDATE,
CCC004*             KTY_MA_TYP,
CCC004*             KNR_MA_TYP,
CCC004*             MA_BDV_MITGLIED,
CCC004*             MA_STRAFE,
CCC004*             MA_VERSANDSTOP,
CCC004*             MA_ZV_ZVII_ABL,
CCC004*             MA_RCK_STZ,
CCC004*             MA_RCK_STZ_DAT,
CCC004*             MA_FUEH_ZEUG_DAT,
CCC004*             MA_SCHUFA_DAT,
CCC004*             MA_34C_DAT,
CCC004*             MA_AFO,
CCC004*             MA_DARLEHEN,
CCC004*             MA_AVAG_DVAG,
CCC004*             MA_KOMPASS_AVAG,
CCC004*             MA_AUTOTELEFON,
CCC004*             MA_TELETEXT,
CCC004*             MA_ZV_NR,
CCC004*             MA_ZV_DAT_BIS_R,
CCC004*             MA_ZV_DAT_BIS,
CCC004*             MA_ZV_DAT_VON,
CCC004*             MA_ZV_KUE_DAT,
CCC004*             MA_ED_ZUSATZ,
CCC004*             MA_ZV_VERL,
CCC004*             MA_FG_DAT,
CCC004*             MA_127_DAT,
CCC004*             MA_VA_DAT,
CCC004*             MA_ANWERBER,
CCC004*             MA_BEFBED_NEU
CCC004*       INTO
040803*             :DCL-VIVB0004 UND NI-VIVB0004 JEWEILS AUF VIVBD004
040803*             GEÄNDERT
CCC004*             :DCL-VIVBD004.MA-ST-NR
CCC004*              :NI-VIVBD004.NI-MA-ST-NR
CCC004*            ,:DCL-VIVBD004.MA-LAST-UPDATE
CCC004*              :NI-VIVBD004.NI-MA-LAST-UPDATE
CCC004*            ,:DCL-VIVBD004.KTY-MA-TYP
CCC004*              :NI-VIVBD004.NI-KTY-MA-TYP
CCC004*            ,:DCL-VIVBD004.KNR-MA-TYP
CCC004*              :NI-VIVBD004.NI-KNR-MA-TYP
CCC004*            ,:DCL-VIVBD004.MA-BDV-MITGLIED
CCC004*              :NI-VIVBD004.NI-MA-BDV-MITGLIED
CCC004*            ,:DCL-VIVBD004.MA-STRAFE
CCC004*              :NI-VIVBD004.NI-MA-STRAFE
CCC004*            ,:DCL-VIVBD004.MA-VERSANDSTOP
CCC004*              :NI-VIVBD004.NI-MA-VERSANDSTOP
CCC004*            ,:DCL-VIVBD004.MA-ZV-ZVII-ABL
CCC004*              :NI-VIVBD004.NI-MA-ZV-ZVII-ABL
CCC004*            ,:DCL-VIVBD004.MA-RCK-STZ
CCC004*              :NI-VIVBD004.NI-MA-RCK-STZ
CCC004*            ,:DCL-VIVBD004.MA-RCK-STZ-DAT
CCC004*              :NI-VIVBD004.NI-MA-RCK-STZ-DAT
CCC004*            ,:DCL-VIVBD004.MA-FUEH-ZEUG-DAT
CCC004*              :NI-VIVBD004.NI-MA-FUEH-ZEUG-DAT
CCC004*            ,:DCL-VIVBD004.MA-SCHUFA-DAT
CCC004*              :NI-VIVBD004.NI-MA-SCHUFA-DAT
CCC004*            ,:DCL-VIVBD004.MA-34C-DAT
CCC004*              :NI-VIVBD004.NI-MA-34C-DAT
CCC004*            ,:DCL-VIVBD004.MA-AFO
CCC004*              :NI-VIVBD004.NI-MA-AFO
CCC004*            ,:DCL-VIVBD004.MA-DARLEHEN
CCC004*              :NI-VIVBD004.NI-MA-DARLEHEN
CCC004*            ,:DCL-VIVBD004.MA-AVAG-DVAG
CCC004*              :NI-VIVBD004.NI-MA-AVAG-DVAG
CCC004*            ,:DCL-VIVBD004.MA-KOMPASS-AVAG
CCC004*              :NI-VIVBD004.NI-MA-KOMPASS-AVAG
CCC004*            ,:DCL-VIVBD004.MA-AUTOTELEFON
CCC004*              :NI-VIVBD004.NI-MA-AUTOTELEFON
CCC004*            ,:DCL-VIVBD004.MA-TELETEXT
CCC004*              :NI-VIVBD004.NI-MA-TELETEXT
CCC004*            ,:DCL-VIVBD004.MA-ZV-NR
CCC004*              :NI-VIVBD004.NI-MA-ZV-NR
CCC004*            ,:DCL-VIVBD004.MA-ZV-DAT-BIS-R
CCC004*              :NI-VIVBD004.NI-MA-ZV-DAT-BIS-R
CCC004*            ,:DCL-VIVBD004.MA-ZV-DAT-BIS
CCC004*              :NI-VIVBD004.NI-MA-ZV-DAT-BIS
CCC004*            ,:DCL-VIVBD004.MA-ZV-DAT-VON
CCC004*              :NI-VIVBD004.NI-MA-ZV-DAT-VON
CCC004*            ,:DCL-VIVBD004.MA-ZV-KUE-DAT
CCC004*              :NI-VIVBD004.NI-MA-ZV-KUE-DAT
CCC004*            ,:DCL-VIVBD004.MA-ED-ZUSATZ
CCC004*              :NI-VIVBD004.NI-MA-ED-ZUSATZ
CCC004*            ,:DCL-VIVBD004.MA-ZV-VERL
CCC004*              :NI-VIVBD004.NI-MA-ZV-VERL
CCC004*            ,:DCL-VIVBD004.MA-FG-DAT
CCC004*              :NI-VIVBD004.NI-MA-FG-DAT
CCC004*            ,:DCL-VIVBD004.MA-127-DAT
CCC004*              :NI-VIVBD004.NI-MA-127-DAT
CCC004*            ,:DCL-VIVBD004.MA-VA-DAT
CCC004*              :NI-VIVBD004.NI-MA-VA-DAT
CCC004*            ,:DCL-VIVBD004.MA-ANWERBER
CCC004*              :NI-VIVBD004.NI-MA-ANWERBER
CCC004*            ,:DCL-VIVBD004.MA-BEFBED-NEU
CCC004*              :NI-VIVBD004.NI-MA-BEFBED-NEU

CCC004*       FROM
040803*             VIVB0004
CCC004*             VIVBD004
CCC004*       WHERE
040803*             MA_ST_NR        =   :DCL-VIVB0004.MA-ST-NR
CCC004*             MA_ST_NR        =   :DCL-VIVBD004.MA-ST-NR
CCC003*    END-EXEC

           END-PERFORM

           EVALUATE   SQLCODE
           WHEN       0
                 MOVE MA-RCK-STZ           TO MPROVRUECK
170703*          CALL 'UPVB0064' USING
CCCPY1*          CALL K-UPVB0064 USING
CCC004*          BY    CONTENT   MA-RCK-STZ-DAT
CCC004*          BY    REFERENCE MPROVDAT
      *
TTMMJJ*          MOVE SQLCODE              TO WLR-SQL-CODE
CCC004*    WHEN       100
CCC004*          SET VERARB-LOG-FEHLER     TO TRUE
CCC004*          MOVE 'MA-ST-NR'           TO WK-TABLE-KEY
040803*          MOVE  MA-ST-NR IN DCL-VIVB0004
CCC004*          MOVE  MA-ST-NR IN DCL-VIVBD004
CCC004*                                    TO WK-ST-NR1
CCC004*          MOVE  WK-FEHLER-TEXT1     TO WLR-FEHLER-TEXT
CCC004*    WHEN       OTHER
CCC004*          SET VERARB-SQL-CODE-EX    TO TRUE
CCC004*          MOVE SQLCODE              TO WLR-SQL-CODE
CCC004*          IF    SQLERRML       >   ZERO
CCC004*              MOVE  SQLERRMC        TO WLR-FEHLER-TEXT
CCC004*           ELSE
CCC004*               MOVE  SPACE          TO WLR-FEHLER-TEXT
CCC004*          END-IF
CCC004*    END-EVALUATE
CCC004*    .
CCC004*MA-ERMITT-EXIT.
CCC004*    EXIT.
