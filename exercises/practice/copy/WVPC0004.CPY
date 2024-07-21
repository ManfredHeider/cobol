      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVPC0004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WVPC0004.CPv  $
      *
      *   Rev 2.1   23 Jan 2001 12:03:28   T13AE28
      *AVB00480: Ber.Eig.E für RD1-D
      *
      *   Rev 2.0   28 Sep 2000 12:16:48   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVPC0004                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIPC0004 (BEEG)                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    23.11.2000                                       *
      *****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00480!23.11.00! 01 ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * COBOL DECLARATION FOR VIEW  VIPC0004                           *
      ******************************************************************
        05  WB-NI-VIPC0004-TAB.
           10 WB-NI-VIPC0004-ELE        PIC S9(04) COMP  OCCURS 8 TIMES.
        05  WB-NI-VIPC0004              REDEFINES  WB-NI-VIPC0004-TAB.
           10 WB-NI-MA-ST-NR            PIC S9(04) USAGE COMP.
           10 WB-NI-BEEG-MONAT          PIC S9(04) USAGE COMP.
           10 WB-NI-BEEG-NEU-STF        PIC S9(04) USAGE COMP.
           10 WB-NI-BEEG-ZEITRAUM       PIC S9(04) USAGE COMP.
           10 WB-NI-BEEG-EH-NT-EG-GES   PIC S9(04) USAGE COMP.
           10 WB-NI-BEEG-EH-NT-EG-ABZUG PIC S9(04) USAGE COMP.
           10 WB-NI-BEEG-EH-NT-GS-GES   PIC S9(04) USAGE COMP.
           10 WB-NI-BEEG-EH-NT-GS-BER   PIC S9(04) USAGE COMP.
        05  WB-VIPC0004.
           10 WB-MA-ST-NR               PIC S9(09)    COMP.
           10 WB-BEEG-MONAT             PIC X(10).
           10 WB-BEEG-NEU-STF           PIC S9(04)    COMP.
           10 WB-BEEG-ZEITRAUM          PIC S9(04)    COMP.
           10 WB-BEEG-EH-NT-EG-GES      PIC S9(09)V9  COMP-3.
           10 WB-BEEG-EH-NT-EG-ABZUG    PIC S9(09)V9  COMP-3.
           10 WB-BEEG-EH-NT-GS-GES      PIC S9(09)V9  COMP-3.
           10 WB-BEEG-EH-NT-GS-BER      PIC S9(09)V9  COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
