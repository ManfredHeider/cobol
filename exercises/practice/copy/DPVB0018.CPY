      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0018.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPVB0018.CPv  $
      *
      *   Rev 2.1   Jul 18 2003 15:03:46   t13ex22
      *AVB00666.UE029: VBAS und ALP in QMF un stat. Aufrufe entfernt
      *
      *   Rev 2.0   Feb 27 1999 15:56:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 28 1996 15:19:46   T13AE26
      *AFRZ0023: Anpassung an MF-Cobol Workbench
      *          (Nested Programs entf.)
      *
      *   Rev 1.0   Nov 04 1995 08:24:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPVB0018                                      *
      *    LESEN DER TABLE KL                                         *
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
      *         !12.08.93!    !MVARPROV-BERECHNUNG NICHT  ! NAUMANN  *
      *         !        !    !KORREKT.                   !          *
      *         !16.09.93! 01 !PROD.UEBERGABE             ! NAUMANN  *
      * AFRZ0023!03.11.95!    ! Anpassung an MF-Cobol-    !          *
      *         !        ! 02 !  WORKBENCH                ! GERLACH  *
      * AVB00666!17.07.03! 2.1! STAT. AUFRUF ENTFERNT     ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *----------------------------------------------------------
       KL-ERMITT SECTION.
      *----------------------------------------------------------
       KL-ERMITT-ANF.

           PERFORM  WITH TEST AFTER
                    VARYING  IND-I  FROM   1  BY  1
                                    UNTIL  IND-I   >  WK-LESEN-MAX
                                     OR    (SQLCODE  NOT =  -904)
CCC005*    EXEC SQL
CCC004*       SELECT
CCC004*             KL_ST_NR,
CCC004*             KL_LAST_UPDATE,
CCC004*             KL_DAT_VON,
CCC004*             KL_DAT_BIS,
CCC004*             KL_PROV_STZ_AP,
CCC004*             KL_PROV_STZ_GP,
CCC004*             KL_PROZ_STZ,
CCC004*             KL_RCK_STZ_DAT,
CCC004*             KL_RCK_STZ
CCC004*       INTO
CCC004*             :DCL-VIVB0040.KL-ST-NR
CCC004*              :NI-VIVB0040.NI-KL-ST-NR
CCC004*            ,:DCL-VIVB0040.KL-LAST-UPDATE
CCC004*              :NI-VIVB0040.NI-KL-LAST-UPDATE
CCC004*            ,:DCL-VIVB0040.KL-DAT-VON
CCC004*              :NI-VIVB0040.NI-KL-DAT-VON
CCC004*            ,:DCL-VIVB0040.KL-DAT-BIS
CCC004*              :NI-VIVB0040.NI-KL-DAT-BIS
CCC004*            ,:DCL-VIVB0040.KL-PROV-STZ-AP
CCC004*              :NI-VIVB0040.NI-KL-PROV-STZ-AP
CCC004*            ,:DCL-VIVB0040.KL-PROV-STZ-GP
CCC004*              :NI-VIVB0040.NI-KL-PROV-STZ-GP
CCC004*            ,:DCL-VIVB0040.KL-PROZ-STZ
CCC004*              :NI-VIVB0040.NI-KL-PROZ-STZ
CCC004*            ,:DCL-VIVB0040.KL-RCK-STZ-DAT
CCC004*              :NI-VIVB0040.NI-KL-RCK-STZ-DAT
CCC004*            ,:DCL-VIVB0040.KL-RCK-STZ
CCC004*              :NI-VIVB0040.NI-KL-RCK-STZ
CCC004*       FROM
CCC004*             VIVB0040
CCC004*       WHERE
CCC004*             KL_ST_NR        =   :KL-ST-NR
CCC003*    END-EXEC

           END-PERFORM

           EVALUATE   SQLCODE
           WHEN       0
120893*          IF  KL-PROV-STZ-GP    >=   KL-PROV-STZ-AP
120893*              MOVE KL-PROV-STZ-GP    TO MVARPROV (1)
120893*          ELSE
120893*              MOVE KL-PROV-STZ-AP    TO MVARPROV (1)
120893*          END-IF
      *
                 MOVE KL-RCK-STZ           TO MPROVRUECK
170703*          CALL 'UPVB0064' USING
CCCPY1*          CALL K-UPVB0064 USING
CCC004*          BY    CONTENT   KL-RCK-STZ-DAT
CCC004*          BY    REFERENCE MPROVDAT
      *
170703*          CALL 'UPVB0064' USING
CCCPY1*          CALL K-UPVB0064 USING
CCC004*          BY    CONTENT   KL-DAT-VON
CCC004*          BY    REFERENCE MEINDAT
      *
CCC004*          IF   NI-KL-DAT-BIS     = - 1
CCC004*               MOVE   000000             TO MAUSDAT
CCC004*          ELSE
170703*               CALL 'UPVB0064' USING
CCCPY1*               CALL K-UPVB0064 USING
CCC004*               BY    CONTENT   KL-DAT-BIS
CCC004*               BY    REFERENCE MAUSDAT
CCC004*          END-IF

CCC004*          MOVE KL-ST-NR        TO Z-ST-NR
CCC004*          MOVE KL-PROV-STZ-AP  TO Z-PROV-STZ-AP
CCC004*          MOVE KL-PROV-STZ-GP  TO Z-PROV-STZ-GP
CCC004*          MOVE KL-PROZ-STZ     TO Z-PROZ-STZ
CCC004*          PERFORM KL-JUR-KONVERT
CCC004*          MOVE  Z-STUFE        TO MSTUFE (1)
CCC004*          MOVE  Z-EPROVKZ      TO MEPROVKZ (1)
CCC004*          MOVE  Z-VARPROV      TO MVARPROV (1)
CCC004*    WHEN       100
CCC004*          SET VERARB-LOG-FEHLER     TO TRUE
CCC004*          MOVE 'KL-ST-NR'           TO WK-TABLE-KEY
CCC004*          MOVE  KL-ST-NR            TO WK-ST-NR1
CCC004*          MOVE  WK-FEHLER-TEXT1     TO WLR-FEHLER-TEXT
CCC004*    WHEN       OTHER
CCC004*          SET VERARB-SQL-CODE-EX    TO TRUE
CCC004*          MOVE SQLCODE              TO WLR-SQL-CODE
CCC004*          IF    SQLERRML       >   ZERO
CCC004*              MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT
CCC004*           ELSE
CCC004*               MOVE  SPACE          TO   WLR-FEHLER-TEXT
CCC004*          END-IF
CCC004*    END-EVALUATE
CCC004*    .
CCC004*KL-ERMITT-EXIT.
CCC004*    EXIT.
