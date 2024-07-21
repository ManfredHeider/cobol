      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0033.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0033.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 11 1997 17:18:58   T13AE26
      *AFVB0333: Rückbelastung Erfolgsprov.
      *          Neue Spalte EV_RUECKBELASTG
      *
      *   Rev 1.0   Nov 04 1995 11:58:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTVB0033                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBVB0033 (EV)                                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KUHN                                             *
      *    DATUM:    15.09.1994                                       *
      *****************************************************************
      *    VERSION   2    14.10.95 10:12:53
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0142!15.09.94! 01 ! NEU                       ! KUHN     *
      * AFVB0237!12.09.95!    ! EV-AUSZAHLUNG ENTFERNT    ! SCHÖLLH. *
      *         !14.10.95! 02 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0333!20.01.97!    ! EV-RUECKBELASTG NEU       ! T. Moll  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(TBVB0033)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0033 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      EV_MONAT                       DATE NOT NULL,
CCC004*      KNR_VERGUET_TYP                SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KTY_VERGUET_TYP                SMALLINT NOT NULL,
CCC004*      KTY_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      EV_EQF                         DECIMAL(5, 2) NOT NULL,
CCC004*      EV_STORMIND_PROZ               DECIMAL(5, 2) NOT NULL,
CCC004*      EV_AE_MONATS_DS                DECIMAL(10, 1) NOT NULL,
120995*       EV_AUSZAHLUNG                  DECIMAL(11, 2) NOT NULL,
CCC004*      EV_BRUTTO_BETRAG               DECIMAL(11, 2) NOT NULL,
CCC004*      EV_RUECKBELASTG                DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0033                           *
      ******************************************************************
       01  TBVB0033-GESAMT.
        05  NI-TBVB0033-TAB.
200197*    10 NI-TBVB0033-ELE      PIC S9(04) COMP  OCCURS 10 TIMES.
200197     10 NI-TBVB0033-ELE      PIC S9(04) COMP  OCCURS 11 TIMES.
        05  NI-TBVB0033            REDEFINES  NI-TBVB0033-TAB.
           10 NI-ST-NR             PIC S9(4) USAGE COMP.
           10 NI-EV-MONAT          PIC S9(4) USAGE COMP.
           10 NI-KNR-VERGUET-TYP   PIC S9(4) USAGE COMP.
           10 NI-KNR-WAEHRUNG      PIC S9(4) USAGE COMP.
           10 NI-KTY-VERGUET-TYP   PIC S9(4) USAGE COMP.
           10 NI-KTY-WAEHRUNG      PIC S9(4) USAGE COMP.
           10 NI-EV-EQF            PIC S9(4) USAGE COMP.
           10 NI-EV-STORMIND-PROZ  PIC S9(4) USAGE COMP.
           10 NI-EV-AE-MONATS-DS   PIC S9(4) USAGE COMP.
120995*     10 NI-EV-AUSZAHLUNG     PIC S9(4) USAGE COMP.
           10 NI-EV-BRUTTO-BETRAG  PIC S9(4) USAGE COMP.
200197     10 NI-EV-RUECKBELASTG   PIC S9(4) USAGE COMP.
        05  DCL-TBVB0033.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 EV-MONAT             PIC X(10).
           10 KNR-VERGUET-TYP      PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTY-VERGUET-TYP      PIC S9(4) USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 EV-EQF               PIC S999V99 USAGE COMP-3.
           10 EV-STORMIND-PROZ     PIC S999V99 USAGE COMP-3.
           10 EV-AE-MONATS-DS      PIC S999999999V9 USAGE COMP-3.
120995*     10 EV-AUSZAHLUNG        PIC S999999999V99 USAGE COMP-3.
           10 EV-BRUTTO-BETRAG     PIC S999999999V99 USAGE COMP-3.
200197     10 EV-RUECKBELASTG      PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
