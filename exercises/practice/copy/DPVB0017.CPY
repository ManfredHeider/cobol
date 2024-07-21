      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0017.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPVB0017.CPv  $
      *
      *   Rev 2.1   Jul 18 2003 15:03:40   t13ex22
      *AVB00666.UE029: VBAS und ALP in QMF un stat. Aufrufe entfernt
      *
      *   Rev 2.0   Feb 27 1999 15:56:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 28 1996 15:15:38   T13AE26
      *AFRZ0023: Anpassung an MF-Cobol Workbench
      *          (Nested Programs entf.)
      *
      *   Rev 1.0   Nov 04 1995 08:23:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPVB0017                                      *
      *    LESEN DER TABLE JUR                                        *
      *    UND FÜLLEN DER RELEVANTEN DATENFELDER AUS WLR-VB1-STRUK    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: O. NAUMANN       (DVAG)                          *
      *    DATUM:    01.04.1993                                       *
      *****************************************************************
      *    VERSION   01   16.09.93                                   *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !27.08.93!    ! S. WARTUNGSAUFTRAG SCHNITTSTELLEN-   *
      *         !        !    ! MODULE V. 26.08.93, PUNKT 2          *
      *         !        !    ! ANREDE-KZ = 5 FÜR JUR     ! NAUMANN  *
      *         !16.09.93! 01 ! PROD.ÜBERGABE             ! NAUMANN  *
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
       JUR-ERMITT SECTION.
      *----------------------------------------------------------
       JUR-ERMITT-ANF.

           PERFORM  WITH TEST AFTER
                    VARYING  IND-I  FROM   1  BY  1
                                    UNTIL  IND-I   >  WK-LESEN-MAX
                                     OR    (SQLCODE  NOT =  -904)
CCC005*    EXEC SQL
CCC004*       SELECT
CCC004*             JUR_ST_NR,
CCC004*             JUR_LAST_UPDATE,
CCC004*             JUR_DAT_VON,
CCC004*             JUR_DAT_BIS,
CCC004*             JUR_PROV_STZ_AP,
CCC004*             JUR_PROV_STZ_GP,
CCC004*             JUR_PROZ_STZ,
CCC004*             JUR_RCK_STZ_DAT,
CCC004*             JUR_RCK_STZ,
CCC004*             KTY_VERSAND,
CCC004*             KNR_VERSAND,
CCC004*             JUR_BEZ
CCC004*       INTO
CCC004*             :DCL-VIVB0051.JUR-ST-NR
CCC004*              :NI-VIVB0051.NI-JUR-ST-NR
CCC004*            ,:DCL-VIVB0051.JUR-LAST-UPDATE
CCC004*              :NI-VIVB0051.NI-JUR-LAST-UPDATE
CCC004*            ,:DCL-VIVB0051.JUR-DAT-VON
CCC004*              :NI-VIVB0051.NI-JUR-DAT-VON
CCC004*            ,:DCL-VIVB0051.JUR-DAT-BIS
CCC004*              :NI-VIVB0051.NI-JUR-DAT-BIS
CCC004*            ,:DCL-VIVB0051.JUR-PROV-STZ-AP
CCC004*              :NI-VIVB0051.NI-JUR-PROV-STZ-AP
CCC004*            ,:DCL-VIVB0051.JUR-PROV-STZ-GP
CCC004*              :NI-VIVB0051.NI-JUR-PROV-STZ-GP
CCC004*            ,:DCL-VIVB0051.JUR-PROZ-STZ
CCC004*              :NI-VIVB0051.NI-JUR-PROZ-STZ
CCC004*            ,:DCL-VIVB0051.JUR-RCK-STZ-DAT
CCC004*              :NI-VIVB0051.NI-JUR-RCK-STZ-DAT
CCC004*            ,:DCL-VIVB0051.JUR-RCK-STZ
CCC004*              :NI-VIVB0051.NI-JUR-RCK-STZ
CCC004*            ,:DCL-VIVB0051.KTY-VERSAND
CCC004*              :NI-VIVB0051.NI-KTY-VERSAND
CCC004*            ,:DCL-VIVB0051.KNR-VERSAND
CCC004*              :NI-VIVB0051.NI-KNR-VERSAND
CCC004*            ,:DCL-VIVB0051.JUR-BEZ
CCC004*              :NI-VIVB0051.NI-JUR-BEZ
CCC004*       FROM
CCC004*             VIVB0051
CCC004*       WHERE
CCC004*             JUR_ST_NR       =   :JUR-ST-NR
CCC003*    END-EXEC

           END-PERFORM

           EVALUATE   SQLCODE
           WHEN       KS-OK
270893           MOVE K-KNR-FIRMA          TO MANREDE
                 MOVE JUR-BEZ (1:20)       TO MZUNAM
      *
                 MOVE JUR-RCK-STZ          TO MPROVRUECK
170703*          CALL 'UPVB0064' USING
CCCPY1*          CALL K-UPVB0064 USING
CCC004*          BY    CONTENT   JUR-RCK-STZ-DAT
CCC004*          BY    REFERENCE MPROVDAT
      *
170703*          CALL 'UPVB0064' USING
CCCPY1*          CALL K-UPVB0064 USING
CCC004*          BY    CONTENT   JUR-DAT-VON
CCC004*          BY    REFERENCE MEINDAT
      *
CCC004*          IF   NI-JUR-DAT-BIS     = - 1
CCC004*               MOVE   000000      TO MAUSDAT
CCC004*          ELSE
170703*             CALL 'UPVB0064' USING
CCCPY1*             CALL K-UPVB0064 USING
CCC004*             BY    CONTENT   JUR-DAT-BIS
CCC004*             BY    REFERENCE MAUSDAT
CCC004*          END-IF

CCC004*          MOVE JUR-ST-NR       TO Z-ST-NR
CCC004*          MOVE JUR-PROV-STZ-AP TO Z-PROV-STZ-AP
CCC004*          MOVE JUR-PROV-STZ-GP TO Z-PROV-STZ-GP
CCC004*          MOVE JUR-PROZ-STZ    TO Z-PROZ-STZ
CCC004*          PERFORM KL-JUR-KONVERT
CCC004*          MOVE Z-STUFE         TO MSTUFE (1)
CCC004*          MOVE Z-EPROVKZ       TO MEPROVKZ (1)
CCC004*    WHEN       KS-NOT-FOUND
CCC004*          SET VERARB-LOG-FEHLER     TO TRUE
CCC004*          MOVE 'JUR-ST-NR'          TO WK-TABLE-KEY
CCC004*          MOVE  JUR-ST-NR           TO WK-ST-NR1
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
CCC004*JUR-ERMITT-EXIT.
CCC004*    EXIT.
