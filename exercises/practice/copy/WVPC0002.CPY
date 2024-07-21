      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVPC0002.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVPC0002.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:48:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 01 1997 16:39:18   T13AE26
      *AFVB0303: Neue Tabelle VIPC0002-BE 'Bereinigte Einheiten'
      *
      *   Rev 1.0   Apr 01 1997 13:46:12   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVPC0002                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T. Moll                                          *
      *    DATUM:    09.10.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0303!09.10.96!    ! NEU                       ! T. Moll  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * COBOL DECLARATION FOR VIEW  VIPC0002                           *
      ******************************************************************
        05  WB-NI-VIPC0002-TAB.
           10 WB-NI-VIPC0002-ELE      PIC S9(04) COMP  OCCURS 6 TIMES.
        05  WB-NI-VIPC0002            REDEFINES  WB-NI-VIPC0002-TAB.
           10 WB-NI-MA-ST-NR          PIC S9(4) USAGE COMP.
           10 WB-NI-BE-MONAT          PIC S9(4) USAGE COMP.
           10 WB-NI-BE-NEU-STF        PIC S9(4) USAGE COMP.
           10 WB-NI-BE-ZEITRAUM       PIC S9(4) USAGE COMP.
           10 WB-NI-BE-EH-GS-NT-GR    PIC S9(4) USAGE COMP.
           10 WB-NI-BE-EH-GS-NT-GES   PIC S9(4) USAGE COMP.
        05  WB-VIPC0002.
           10 WB-MA-ST-NR             PIC  S9(09)    COMP.
           10 WB-BE-MONAT             PIC   X(10).
           10 WB-BE-NEU-STF           PIC  S9(04)    COMP.
           10 WB-BE-ZEITRAUM          PIC  S9(04)    COMP.
           10 WB-BE-EH-GS-NT-GR       PIC  S9(09)V9  COMP-3.
           10 WB-BE-EH-GS-NT-GES      PIC  S9(09)V9  COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
