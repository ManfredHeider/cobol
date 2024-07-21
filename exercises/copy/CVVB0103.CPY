      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0103.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0103.CPv  $
      *
      *   Rev 2.1   08 Nov 2006 16:51:44   t13ae35
      *AIS00060: Summenblatt: Füllen SONSTBUCH
      *
      *   Rev 2.0   24 Oct 2006 11:04:44   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CVVB0103                                     *
      *    --------------------------------------------------------  *
      *    WORKBEREICH FÜR VIVB0103 (BUKA)                           *
      *    --------------------------------------------------------  *
      *    ERSTELLT: J.Hering                                        *
      *    DATUM:    26.10.2006                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AIS00060!26.10.06! 2.1! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0103                          *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0103 TABLE
CCC004*    (
CCC004*      GEGEN_KONTO            CHAR(12)    NOT NULL
CCC004*     ,BELEG_ART              CHAR(03)    NOT NULL
CCC004*     ,KONTO_TYP              CHAR(01)    NOT NULL
CCC004*     ,SHKZ                   CHAR(01)    NOT NULL
CCC004*     ,BUKA_DAT_VON           DATE        NOT NULL
CCC004*     ,BUKA_DAT_BIS           DATE        NOT NULL
CCC004*     ,KTY_GESELL_TYP         SMALLINT    NOT NULL
CCC004*     ,KNR_GESELL_TYP         SMALLINT    NOT NULL
CCC004*     ,KTY_BUCH_TYP           SMALLINT    NOT NULL
CCC004*     ,KNR_BUCH_TYP           SMALLINT    NOT NULL
CCC004*     ,BUKA_LAST_UPDATE       TIMESTAMP   NOT NULL
CCC004*    )
CCC003*    END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0103
      ******************************************************************
       01  VIVB0103-GESAMT.
        05  NI-VIVB0103-TAB.
           10 NI-VIVB0103-ELE      PIC S9(04) BINARY OCCURS  11 TIMES.
        05  NI-VIVB0103            REDEFINES   NI-VIVB0103-TAB.
           10 NI-GEGEN-KONTO            PIC S9(04)       BINARY.
           10 NI-BELEG-ART              PIC S9(04)       BINARY.
           10 NI-KONTO-TYP              PIC S9(04)       BINARY.
           10 NI-SHKZ                   PIC S9(04)       BINARY.
           10 NI-BUKA-DAT-VON           PIC S9(04)       BINARY.
           10 NI-BUKA-DAT-BIS           PIC S9(04)       BINARY.
           10 NI-KTY-GESELL-TYP         PIC S9(04)       BINARY.
           10 NI-KNR-GESELL-TYP         PIC S9(04)       BINARY.
           10 NI-KTY-BUCH-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-BUCH-TYP           PIC S9(04)       BINARY.
           10 NI-BUKA-LAST-UPDATE       PIC S9(04)       BINARY.


        05  DCL-VIVB0103.
           10 GEGEN-KONTO               PIC X(12).
           10 BELEG-ART                 PIC X(03).
           10 KONTO-TYP                 PIC X(01).
           10 SHKZ                      PIC X(01).
           10 BUKA-DAT-VON              PIC X(10).
           10 BUKA-DAT-BIS              PIC X(10).
           10 KTY-GESELL-TYP            PIC S9(04)       BINARY.
           10 KNR-GESELL-TYP            PIC S9(04)       BINARY.
           10 KTY-BUCH-TYP              PIC S9(04)       BINARY.
           10 KNR-BUCH-TYP              PIC S9(04)       BINARY.
           10 BUKA-LAST-UPDATE          PIC X(26).


      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************

