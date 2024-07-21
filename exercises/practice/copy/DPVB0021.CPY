      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0021.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DPVB0021.CPv  $
      *
      *   Rev 2.2   Jun 23 2009 11:18:42   t13ae13
      *AVB01029: Wahlmöglichkeit bei ED-Regelung - Rest
      *
      *   Rev 2.1   Jul 18 2003 15:04:02   t13ex22
      *AVB00666.UE029: VBAS und ALP in QMF un stat. Aufrufe entfernt
      *
      *   Rev 2.0   Feb 27 1999 15:56:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 28 1996 15:22:30   T13AE26
      *AFRZ0023: Anpassung an MF-Cobol Workbench
      *          (Nested Programs entf.)
      *
      *   Rev 1.0   Nov 04 1995 08:24:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPVB0021                                      *
      *    LESEN DER TABLE ED                                         *
      *    UND FÜLLEN DER RELEVANTEN DATENFELDER AUS WLR-VB1-STRUK    *
      *    ÄNDERUNGEN AN DIESEM COPY MÜSSEN EVENTUELL AUCH IM COPY    *
      *    DPVB0050 NACHGEZOGEN WERDEN. PRÜFEN!                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: O. NAUMANN       (DVAG)                          *
      *    DATUM:    04.04.1993                                       *
      *****************************************************************
      *    VERSION   1    25.04.95 16:53:10
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !16.09.93! 01 ! NEU                       ! NAUMANN  *
      * FEVB0238!24.04.94! 02 ! QUERVERWEIS DPVB0050      ! GIESE    *
      *         !        !    ! COMPILE                   !          *
      * AFRZ0023!03.11.95!    ! Anpassung an MF-Cobol-    !          *
      *         !        ! 03 !  WORKBENCH                ! GERLACH  *
      * AVB00666!17.07.03! 2.1! STAT. AUFRUF ENTFERNT     ! MUND     *
      * AVB01029!14.05.09!2.2 ! neue View VIVBA003        ! Walb     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *----------------------------------------------------------
       ED-ERMITT SECTION.
      *----------------------------------------------------------
       ED-ERMITT-ANF.

           PERFORM  WITH TEST AFTER
                    VARYING  IND-I  FROM   1  BY  1
                                    UNTIL  IND-I   >  WK-LESEN-MAX
                                     OR    (SQLCODE  NOT =  -904)
CCC005*    EXEC SQL
CCC004*       SELECT
CCC004*             ED_ST_NR,
CCC004*             ED_LAST_UPDATE,
CCC004*             MA_ST_NR,
CCC004*             ED_RCK_STZ,
CCC004*             ED_RCK_STZ_DAT,
CCC004*             ED_BEZ
CCC004*            ,ED_GENERATION
CCC004*            ,ED_ZWEIG
CCC004*            ,ED_ZWEIG_VOM
CCC004*            ,ED_PROV1
CCC004*            ,ED_PROV_DAT1
CCC004*            ,ED_PROV2
CCC004*            ,ED_PROV_DAT2
CCC004*       INTO
140509*             :DCL-VIVB0003.ED-ST-NR
140509*              :NI-VIVB0003.NI-ED-ST-NR
140509*            ,:DCL-VIVB0003.ED-LAST-UPDATE
140509*              :NI-VIVB0003.NI-ED-LAST-UPDATE
140509*            ,:DCL-VIVB0003.MA-ST-NR
140509*              :NI-VIVB0003.NI-MA-ST-NR
140509*            ,:DCL-VIVB0003.ED-RCK-STZ
140509*              :NI-VIVB0003.NI-ED-RCK-STZ
140509*            ,:DCL-VIVB0003.ED-RCK-STZ-DAT
140509*              :NI-VIVB0003.NI-ED-RCK-STZ-DAT
140509*            ,:DCL-VIVB0003.ED-BEZ
140509*              :NI-VIVB0003.NI-ED-BEZ
CCC004*             :DCL-VIVBA003.ED-ST-NR
CCC004*              :NI-VIVBA003.NI-ED-ST-NR
CCC004*            ,:DCL-VIVBA003.ED-LAST-UPDATE
CCC004*              :NI-VIVBA003.NI-ED-LAST-UPDATE
CCC004*            ,:DCL-VIVBA003.MA-ST-NR
CCC004*              :NI-VIVBA003.NI-MA-ST-NR
CCC004*            ,:DCL-VIVBA003.ED-RCK-STZ
CCC004*              :NI-VIVBA003.NI-ED-RCK-STZ
CCC004*            ,:DCL-VIVBA003.ED-RCK-STZ-DAT
CCC004*              :NI-VIVBA003.NI-ED-RCK-STZ-DAT
CCC004*            ,:DCL-VIVBA003.ED-BEZ
CCC004*              :NI-VIVBA003.NI-ED-BEZ
CCC004*            ,:DCL-VIVBA003.ED-GENERATION
CCC004*              :NI-VIVBA003.NI-ED-GENERATION
CCC004*            ,:DCL-VIVBA003.ED-ZWEIG
CCC004*              :NI-VIVBA003.NI-ED-ZWEIG
CCC004*            ,:DCL-VIVBA003.ED-ZWEIG-VOM
CCC004*              :NI-VIVBA003.NI-ED-ZWEIG-VOM
CCC004*            ,:DCL-VIVBA003.ED-PROV1
CCC004*              :NI-VIVBA003.NI-ED-PROV1
CCC004*            ,:DCL-VIVBA003.ED-PROV-DAT1
CCC004*              :NI-VIVBA003.NI-ED-PROV-DAT1
CCC004*            ,:DCL-VIVBA003.ED-PROV2
CCC004*              :NI-VIVBA003.NI-ED-PROV2
CCC004*            ,:DCL-VIVBA003.ED-PROV-DAT2
CCC004*              :NI-VIVBA003.NI-ED-PROV-DAT2
CCC004*       FROM
140509*             VIVB0003
CCC004*             VIVBA003
CCC004*       WHERE
140509*             ED_ST_NR        =   :DCL-VIVB0003.ED-ST-NR
CCC004*             ED_ST_NR        =   :DCL-VIVBA003.ED-ST-NR
CCC003*    END-EXEC

           END-PERFORM

           EVALUATE   SQLCODE
           WHEN       0
                 MOVE ED-BEZ (1:20)   TO MZUNAM
                 MOVE ED-RCK-STZ      TO MPROVRUECK
      *
170703*          CALL 'UPVB0064' USING
CCCPY1*          CALL K-UPVB0064 USING
CCC004*          BY    CONTENT   ED-RCK-STZ-DAT
CCC004*          BY    REFERENCE MPROVDAT
      *
CCC004*          MOVE  768            TO MSTUFE (1)
      *
TTMMJJ*          MOVE SQLCODE         TO WLR-SQL-CODE
CCC004*    WHEN       100
CCC004*          SET VERARB-LOG-FEHLER     TO TRUE
CCC004*          MOVE 'ED-ST-NR'           TO WK-TABLE-KEY
140509*          MOVE  ED-ST-NR IN DCL-VIVB0003
CCC004*          MOVE  ED-ST-NR IN DCL-VIVBA003
CCC004*                                    TO WK-ST-NR1
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
CCC004*ED-ERMITT-EXIT.
CCC004*    EXIT.
