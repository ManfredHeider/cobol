      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA033.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVBA033.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 11 1997 17:21:32   T13AE26
      *AFVB0333: Rückbelastung Erfolgsprovision
      *          Neue spalte EV_RUECKBELASTG
      *
      *   Rev 1.0   Nov 04 1995 10:16:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBA033                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÖLLHAMMER                                     *
      *    DATUM:    12.09.1995                                       *
      *****************************************************************
      *    VERSION   1    14.10.95 10:15:12
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0237!12.09.95!    ! NEU                       ! SCHÖLLH. *
      *         !14.10.95! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *
      * AFVB0333!20.01.97!    ! EV-RUECKBELASTG NEU       ! T. Moll  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * COBOL DECLARATION FOR VIEW  VIVBA033                           *
      ******************************************************************
        05  WB-NI-VIVBA033-TAB.
200197*    10 WB-NI-VIVBA033-ELE      PIC S9(04) COMP  OCCURS 10 TIMES.
200197     10 WB-NI-VIVBA033-ELE      PIC S9(04) COMP  OCCURS 11 TIMES.
        05  WB-NI-VIVBA033         REDEFINES  WB-NI-VIVBA033-TAB.
           10 WB-NI-ST-NR             PIC S9(4) USAGE COMP.
           10 WB-NI-EV-MONAT          PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-VERGUET-TYP   PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-WAEHRUNG      PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-VERGUET-TYP   PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-WAEHRUNG      PIC S9(4) USAGE COMP.
           10 WB-NI-EV-EQF            PIC S9(4) USAGE COMP.
           10 WB-NI-EV-STORMIND-PROZ  PIC S9(4) USAGE COMP.
           10 WB-NI-EV-AE-MONATS-DS   PIC S9(4) USAGE COMP.
           10 WB-NI-EV-BRUTTO-BETRAG  PIC S9(4) USAGE COMP.
200197     10 WB-NI-EV-RUECKBELASTG   PIC S9(4) USAGE COMP.
        05  WB-VIVBA033.
           10 WB-ST-NR                PIC S9(9) USAGE COMP.
           10 WB-EV-MONAT             PIC X(10).
           10 WB-KNR-VERGUET-TYP      PIC S9(4) USAGE COMP.
           10 WB-KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 WB-KTY-VERGUET-TYP      PIC S9(4) USAGE COMP.
           10 WB-KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 WB-EV-EQF               PIC S999V99 USAGE COMP-3.
           10 WB-EV-STORMIND-PROZ     PIC S999V99 USAGE COMP-3.
           10 WB-EV-AE-MONATS-DS      PIC S999999999V9 USAGE COMP-3.
           10 WB-EV-BRUTTO-BETRAG     PIC S999999999V99 USAGE COMP-3.
200197     10 WB-EV-RUECKBELASTG      PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
