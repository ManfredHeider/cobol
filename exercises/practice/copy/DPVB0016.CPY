      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0016.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPVB0016.CPv  $
      *
      *   Rev 2.1   Jul 18 2003 15:03:32   t13ex22
      *AVB00666.UE029: VBAS und ALP in QMF un stat. Aufrufe entfernt
      *
      *   Rev 2.0   Feb 27 1999 15:56:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 28 1996 15:14:08   T13AE26
      *AFRZ0023: Anpassung an MF-Cobol Workbench
      *          (Nested Programs entf.)
      *
      *   Rev 1.0   Nov 04 1995 08:23:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPVB0016                                      *
      *    LESEN DER TABLE GK                                         *
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
      * AVB00666!17.07.03! 2.1! STAT. AUFRUF ENTFERNT     ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *----------------------------------------------------------
       GK-ERMITT SECTION.
      *----------------------------------------------------------
       GK-ERMITT-ANF.

           PERFORM  WITH TEST AFTER
                    VARYING  IND-I  FROM   1  BY  1
                                    UNTIL  IND-I   >  WK-LESEN-MAX
                                     OR    (SQLCODE  NOT =  -904)
CCC005*    EXEC SQL
CCC004*       SELECT
CCC004*             GK_ST_NR,
CCC004*             GK_LAST_UPDATE,
CCC004*             GK_FUNKTION,
CCC004*             GK_DAT_VON,
CCC004*             GK_DAT_BIS,
CCC004*             GK_BEZ
CCC004*       INTO
CCC004*             :DCL-VIVB0002.GK-ST-NR
CCC004*              :NI-VIVB0002.NI-GK-ST-NR
CCC004*            ,:DCL-VIVB0002.GK-LAST-UPDATE
CCC004*              :NI-VIVB0002.NI-GK-LAST-UPDATE
CCC004*            ,:DCL-VIVB0002.GK-FUNKTION
CCC004*              :NI-VIVB0002.NI-GK-FUNKTION
CCC004*            ,:DCL-VIVB0002.GK-DAT-VON
CCC004*              :NI-VIVB0002.NI-GK-DAT-VON
CCC004*            ,:DCL-VIVB0002.GK-DAT-BIS
CCC004*              :NI-VIVB0002.NI-GK-DAT-BIS
CCC004*            ,:DCL-VIVB0002.GK-BEZ
CCC004*              :NI-VIVB0002.NI-GK-BEZ
CCC004*       FROM
CCC004*             VIVB0002
CCC004*       WHERE
CCC004*             GK_ST_NR        =   :GK-ST-NR
CCC003*    END-EXEC

           END-PERFORM

           EVALUATE   SQLCODE
           WHEN       0
                 MOVE GK-BEZ (1:20)        TO MZUNAM
                 MOVE GK-FUNKTION          TO MGEBDAT
      *
170703*          CALL 'UPVB0064' USING
CCCPY1*          CALL K-UPVB0064 USING
CCC004*          BY    CONTENT   GK-DAT-VON
CCC004*          BY    REFERENCE MEINDAT
      *
CCC004*          IF GK-DAT-BIS (1:2)     = '31' AND
CCC004*             GK-DAT-BIS (4:2)     = '12' AND
CCC004*             GK-DAT-BIS (7:4)     = '9999'
      *          FALLS GK-DAT-BIS   GLEICH 31.12.9999
CCC004*             MOVE ZERO                 TO MAUSDAT
CCC004*          ELSE
170703*             CALL 'UPVB0064' USING
CCCPY1*             CALL K-UPVB0064 USING
CCC004*             BY    CONTENT   GK-DAT-BIS
CCC004*             BY    REFERENCE MAUSDAT
CCC004*          END-IF
      *
CCC004*          MOVE  999            TO MSTUFE (1)
      *
TTMMJJ*          MOVE SQLCODE         TO WLR-SQL-CODE
CCC004*    WHEN       100
CCC004*          SET VERARB-LOG-FEHLER     TO TRUE
CCC004*          MOVE 'GK-ST-NR'           TO WK-TABLE-KEY
CCC004*          MOVE  GK-ST-NR            TO WK-ST-NR1
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
CCC004*GK-ERMITT-EXIT.
CCC004*    EXIT.
