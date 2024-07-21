      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0019.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPVB0019.CPv  $
      *
      *   Rev 2.1   Jul 18 2003 15:03:50   t13ex22
      *AVB00666.UE029: VBAS und ALP in QMF un stat. Aufrufe entfernt
      *
      *   Rev 2.0   Feb 27 1999 15:56:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 28 1996 15:20:18   T13AE26
      *AFRZ0023: Anpassung an MF-Cobol Workbench
      *          (Nested Programs entf.)
      *
      *   Rev 1.0   Nov 04 1995 08:24:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPVB0019                                      *
      *    LESEN DER TABLE RFT                                        *
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
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *----------------------------------------------------------
       RFT-ERMITT SECTION.
      *----------------------------------------------------------
       RFT-ERMITT-ANF.

           PERFORM  WITH TEST AFTER
                    VARYING  IND-I  FROM   1  BY  1
                                    UNTIL  IND-I   >  WK-LESEN-MAX
                                     OR    (SQLCODE  NOT =  -904)
CCC005*    EXEC SQL
CCC004*       SELECT
CCC004*             RFT_ST_NR,
CCC004*             RFT_LAST_UPDATE,
CCC004*             MA_ST_NR,
CCC004*             RFT_PROV_STZ_AP,
CCC004*             RFT_PROV_STZ_GP,
CCC004*             RFT_PROZ_STZ,
CCC004*             RFT_RCK_STZ,
CCC004*             RFT_RCK_STZ_DAT,
CCC004*             RFT_DAT_VON,
CCC004*             RFT_DAT_BIS,
CCC004*             RFT_ABLAUF_DAT
CCC004*       INTO
CCC004*             :DCL-VIVB0005.RFT-ST-NR
CCC004*              :NI-VIVB0005.NI-RFT-ST-NR
CCC004*            ,:DCL-VIVB0005.RFT-LAST-UPDATE
CCC004*              :NI-VIVB0005.NI-RFT-LAST-UPDATE
CCC004*            ,:DCL-VIVB0005.MA-ST-NR
CCC004*              :NI-VIVB0005.NI-MA-ST-NR
CCC004*            ,:DCL-VIVB0005.RFT-PROV-STZ-AP
CCC004*              :NI-VIVB0005.NI-RFT-PROV-STZ-AP
CCC004*            ,:DCL-VIVB0005.RFT-PROV-STZ-GP
CCC004*              :NI-VIVB0005.NI-RFT-PROV-STZ-GP
CCC004*            ,:DCL-VIVB0005.RFT-PROZ-STZ
CCC004*              :NI-VIVB0005.NI-RFT-PROZ-STZ
CCC004*            ,:DCL-VIVB0005.RFT-RCK-STZ
CCC004*              :NI-VIVB0005.NI-RFT-RCK-STZ
CCC004*            ,:DCL-VIVB0005.RFT-RCK-STZ-DAT
CCC004*              :NI-VIVB0005.NI-RFT-RCK-STZ-DAT
CCC004*            ,:DCL-VIVB0005.RFT-DAT-VON
CCC004*              :NI-VIVB0005.NI-RFT-DAT-VON
CCC004*            ,:DCL-VIVB0005.RFT-DAT-BIS
CCC004*              :NI-VIVB0005.NI-RFT-DAT-BIS
CCC004*            ,:DCL-VIVB0005.RFT-ABLAUF-DAT
CCC004*              :NI-VIVB0005.NI-RFT-ABLAUF-DAT
CCC004*       FROM
CCC004*             VIVB0005
CCC004*       WHERE
CCC004*             RFT_ST_NR        =   :RFT-ST-NR
CCC003*    END-EXEC

           END-PERFORM

           EVALUATE   SQLCODE
           WHEN       0
                 IF  RFT-PROV-STZ-GP    >=   RFT-PROV-STZ-AP
                     MOVE RFT-PROV-STZ-GP   TO MVARPROV (1)
                 ELSE
                     MOVE RFT-PROV-STZ-AP   TO MVARPROV (1)
                 END-IF
      *
                 MOVE RFT-RCK-STZ           TO MPROVRUECK
170703*          CALL 'UPVB0064' USING
CCCPY1*          CALL K-UPVB0064 USING
CCC004*          BY    CONTENT   RFT-RCK-STZ-DAT
CCC004*          BY    REFERENCE MPROVDAT
      *
170703*          CALL 'UPVB0064' USING
CCCPY1*          CALL K-UPVB0064 USING
CCC004*          BY    CONTENT   RFT-DAT-VON
CCC004*          BY    REFERENCE MEINDAT
      *
CCC004*          IF   NI-RFT-DAT-BIS     = - 1
CCC004*               MOVE   000000             TO MAUSDAT
CCC004*          ELSE
170703*               CALL 'UPVB0064' USING
CCCPY1*               CALL K-UPVB0064 USING
CCC004*               BY    CONTENT   RFT-DAT-BIS
CCC004*               BY    REFERENCE MAUSDAT
CCC004*          END-IF
      *
CCC004*          MOVE  888            TO MSTUFE (1)
      *
CCC004*    WHEN       100
CCC004*          SET VERARB-LOG-FEHLER     TO TRUE
CCC004*          MOVE 'RFT-ST-NR'          TO WK-TABLE-KEY
CCC004*          MOVE  RFT-ST-NR           TO WK-ST-NR1
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
CCC004*RFT-ERMITT-EXIT.
CCC004*    EXIT.
