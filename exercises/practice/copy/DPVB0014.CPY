      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPVB0014.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPVB0014.CPv  $
      *
      *   Rev 2.1   Jul 18 2003 15:02:26   t13ex22
      *
      *   Rev 2.0   Feb 27 1999 15:56:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 28 1996 15:11:06   T13AE26
      *AFRZ0023: Anpassung an MF-Cobol Workbench
      *          (Nested Programs entf.)
      *
      *   Rev 1.0   Nov 04 1995 08:23:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPVB0014                                      *
      *    ERMITTELN DER DATENFELDER AUS WLR-VB1-STRUK, DIE DEN DATEN-*
      *    FELDERN DER MAN ENTSPRECHEN.                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: O. NAUMANN       (DVAG)                          *
      *    DATUM:    01.04.1993                                       *
      *****************************************************************
      *    VERSION   01   16.09.93                                   *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !01.04.93!    ! NEU                       ! NAUMANN  *
      *         !31.08.93!    ! SELECT-ANPASSUNG AN DEN BESCHLUß DER *
      *         !        !    ! PROJEKTSITZUNG VOM 07.05.1993        *
      *         !16.09.93! 01 ! Prod.uebergabe            ! NAUMANN  *
      * AFRZ0023!03.11.95!    ! Anpassung an MF-Cobol-    !          *
      *         !        ! 02 !  Workbench                ! Gerlach  *
      * AVB00666!17.07.03! 2.1! STAT. AUFRUF ENTFERNT     ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *----------------------------------------------------------
       MAN-ERMITT SECTION.
      *----------------------------------------------------------
       MAN-ERMITT-ANF.
           PERFORM  WITH TEST AFTER
                    VARYING  IND-I  FROM   1  BY  1
                                    UNTIL  IND-I   >  WK-LESEN-MAX
                                     OR    (SQLCODE  NOT =  -904)
CCC005*    EXEC SQL
CCC004*       SELECT
CCC004*             MAN_BEZUGS_NR,
CCC004*             MAN_DAT_VON,
CCC004*             MAN_LAST_UPDATE,
CCC004*             MAN_DAT_BIS,
CCC004*             KTY_BEZUGS_TYP,
CCC004*             KNR_BEZUGS_TYP,
CCC004*             MAN_GEBURTSDAT,
CCC004*             KTY_ANREDE,
CCC004*             KNR_ANREDE,
CCC004*             MAN_TITEL_ADEL,
CCC004*             MAN_TITEL_AKAD,
CCC004*             MAN_NAME,
CCC004*             MAN_VORNAME
CCC004*       INTO
CCC004*             :DCL-VIVB0014.MAN-BEZUGS-NR
CCC004*              :NI-VIVB0014.NI-MAN-BEZUGS-NR
CCC004*            ,:DCL-VIVB0014.MAN-DAT-VON
CCC004*              :NI-VIVB0014.NI-MAN-DAT-VON
CCC004*            ,:DCL-VIVB0014.MAN-LAST-UPDATE
CCC004*              :NI-VIVB0014.NI-MAN-LAST-UPDATE
CCC004*            ,:DCL-VIVB0014.MAN-DAT-BIS
CCC004*              :NI-VIVB0014.NI-MAN-DAT-BIS
CCC004*            ,:DCL-VIVB0014.KTY-BEZUGS-TYP
CCC004*              :NI-VIVB0014.NI-KTY-BEZUGS-TYP
CCC004*            ,:DCL-VIVB0014.KNR-BEZUGS-TYP
CCC004*              :NI-VIVB0014.NI-KNR-BEZUGS-TYP
CCC004*            ,:DCL-VIVB0014.MAN-GEBURTSDAT
CCC004*              :NI-VIVB0014.NI-MAN-GEBURTSDAT
CCC004*            ,:DCL-VIVB0014.KTY-ANREDE
CCC004*              :NI-VIVB0014.NI-KTY-ANREDE
CCC004*            ,:DCL-VIVB0014.KNR-ANREDE
CCC004*              :NI-VIVB0014.NI-KNR-ANREDE
CCC004*            ,:DCL-VIVB0014.MAN-TITEL-ADEL
CCC004*              :NI-VIVB0014.NI-MAN-TITEL-ADEL
CCC004*            ,:DCL-VIVB0014.MAN-TITEL-AKAD
CCC004*              :NI-VIVB0014.NI-MAN-TITEL-AKAD
CCC004*            ,:DCL-VIVB0014.MAN-NAME
CCC004*              :NI-VIVB0014.NI-MAN-NAME
CCC004*            ,:DCL-VIVB0014.MAN-VORNAME
CCC004*              :NI-VIVB0014.NI-MAN-VORNAME
CCC004*       FROM
CCC004*             VIVB0014 MAN1
CCC004*       WHERE
CCC004*             MAN_BEZUGS_NR   =   :MAN-BEZUGS-NR
CCC004*         AND MAN_LAST_UPDATE =
CCC004*              (SELECT MAX(MAN2.MAN_LAST_UPDATE)
CCC004*               FROM
CCC004*                  VIVB0014 MAN2
CCC004*               WHERE MAN1.MAN_BEZUGS_NR  =
CCC004*                     MAN2.MAN_BEZUGS_NR
CCC004*              )
310893*         AND MAN_DAT_VON     =
310893*              (SELECT MAX(MAN2.MAN_DAT_VON)
310893*               FROM
310893*                  VIVB0014 MAN2
310893*               WHERE MAN1.MAN_BEZUGS_NR  =
310893*                     MAN2.MAN_BEZUGS_NR
310893*                 AND MAN2.MAN_DAT_VON   <= CURRENT TIMESTAMP
310893*              )
CCC003*    END-EXEC

           END-PERFORM

           EVALUATE   SQLCODE
           WHEN       0
              IF   NI-MAN-GEBURTSDAT     = - 1
                   MOVE   000000             TO MGEBDAT
              ELSE
170703*            CALL 'UPVB0064' USING
CCCPY1*            CALL K-UPVB0064 USING
CCC004*            BY    CONTENT   MAN-GEBURTSDAT
CCC004*            BY    REFERENCE MGEBDAT
CCC004*       END-IF
      *
CCC004*       MOVE MAN-NAME          TO MZUNAM
      *
      *    BERÜCKSICHTIGUNG VON ADELS- UND AKAD. TITEL
      *    MVNAME = MAN-VORNAME + ',' + MAN-TITEL-AKAD +
      *                         + ' ' + MAN-TITEL-ADEL
      *
      *    ERMITTELN ANZAHL ZEICHEN VORNAME
CCC004*       MOVE ZERO TO Z-VORNAME-LAENG
CCC004*       PERFORM VARYING I-IND1 FROM 20 BY -1 UNTIL I-IND1 <= ZERO
CCC004*         IF MAN-VORNAME (I-IND1:1) = SPACE
CCC004*            CONTINUE
CCC004*         ELSE
CCC004*            MOVE   I-IND1 TO Z-VORNAME-LAENG
CCC004*            MOVE   ZERO   TO I-IND1
CCC004*         END-IF
CCC004*       END-PERFORM

      *    ERMITTELN ANZAHL ZEICHEN ADELSTITEL
CCC004*       MOVE ZERO TO Z-TITEL-ADEL-LAENG
CCC004*       PERFORM VARYING I-IND1 FROM 10 BY -1 UNTIL I-IND1 <= ZERO
CCC004*         IF MAN-TITEL-ADEL (I-IND1:1) = SPACE
CCC004*            CONTINUE
CCC004*         ELSE
CCC004*            MOVE   I-IND1 TO Z-TITEL-ADEL-LAENG
CCC004*            MOVE   ZERO   TO I-IND1
CCC004*         END-IF
CCC004*       END-PERFORM

      *    ERMITTELN ANZAHL ZEICHEN AKAD. TITEL
CCC004*       MOVE ZERO TO Z-TITEL-AKAD-LAENG
CCC004*       PERFORM VARYING I-IND1 FROM 10 BY -1 UNTIL I-IND1 <= ZERO
CCC004*         IF MAN-TITEL-AKAD (I-IND1:1) = SPACE
CCC004*            CONTINUE
CCC004*         ELSE
CCC004*            MOVE   I-IND1 TO Z-TITEL-AKAD-LAENG
CCC004*            MOVE   ZERO   TO I-IND1
CCC004*         END-IF
CCC004*       END-PERFORM

      * ERZEUGEN VON MVNAME
CCC004*       EVALUATE Z-TITEL-AKAD-LAENG = ZERO
CCC004*           ALSO Z-TITEL-ADEL-LAENG = ZERO
CCC004*           WHEN TRUE  ALSO TRUE
      *           KEIN AKAD. TITEL UND KEIN ADELSTITEL
CCC004*              MOVE MAN-VORNAME  TO MVNAME
CCC004*           WHEN TRUE  ALSO FALSE
      *           KEIN AKAD. TITEL UND EIN ADELSTITEL
CCC004*              STRING   MAN-VORNAME (1:Z-VORNAME-LAENG)
CCC004*                                              DELIMITED BY SIZE
CCC004*              ', ' DELIMITED BY SIZE
CCC004*              MAN-TITEL-ADEL(1:Z-TITEL-ADEL-LAENG)
CCC004*                                              DELIMITED BY SIZE
CCC004*              INTO   MVNAME
CCC004*           WHEN FALSE ALSO TRUE
      *           EIN AKAD. TITEL UND KEIN ADELSTITEL
CCC004*              STRING   MAN-VORNAME (1:Z-VORNAME-LAENG)
CCC004*                                              DELIMITED BY SIZE
CCC004*              ', ' DELIMITED BY SIZE
CCC004*              MAN-TITEL-AKAD(1:Z-TITEL-AKAD-LAENG)
CCC004*                                              DELIMITED BY SIZE
CCC004*              INTO   MVNAME
CCC004*           WHEN FALSE ALSO FALSE
      *           EIN AKAD. TITEL UND EIN ADELSTITEL
CCC004*              STRING   MAN-VORNAME (1:Z-VORNAME-LAENG)
CCC004*                                              DELIMITED BY SIZE
CCC004*              ', ' DELIMITED BY SIZE
CCC004*              MAN-TITEL-AKAD(1:Z-TITEL-AKAD-LAENG)
CCC004*                                              DELIMITED BY SIZE
CCC004*              ', ' DELIMITED BY SIZE
CCC004*              MAN-TITEL-ADEL(1:Z-TITEL-ADEL-LAENG)
CCC004*                                              DELIMITED BY SIZE
CCC004*              INTO   MVNAME
CCC004*       END-EVALUATE
      *
CCC004*       MOVE KNR-ANREDE        TO MANREDE
      *
TTMMJJ*       MOVE SQLCODE         TO WLR-SQL-CODE
CCC004*    WHEN       100
CCC004*       CONTINUE
CCC004*    WHEN       OTHER
CCC004*       SET VERARB-SQL-CODE-EX    TO TRUE
CCC004*       MOVE SQLCODE              TO WLR-SQL-CODE
CCC004*       IF    SQLERRML       >   ZERO
CCC004*           MOVE  SQLERRMC        TO   WLR-FEHLER-TEXT
CCC004*       ELSE
CCC004*           MOVE  SPACE          TO   WLR-FEHLER-TEXT
CCC004*       END-IF
CCC004*    END-EVALUATE
CCC004*    .
CCC004*MAN-ERMITT-EXIT.
CCC004*    EXIT.
