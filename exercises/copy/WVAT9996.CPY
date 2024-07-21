      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   wvat9996.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\wvat9996.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Dec 29 1995 15:48:30   T13AE26
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY: WVAT9996                                             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T. Moll                                          *
      *    DATUM:    10.12.1995                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * AFVB0251!10.12.95!    ! NEU                       ! Moll      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      ******************************************************************
      * COBOL DECLARATION FOR TABLE WB-VIVB9996
      ******************************************************************
       01  WB-VIVB9996-GESAMT.
        05  WB-NI-VIVB9996-TAB.
           10 WB-NI-VIVB9996-ELE   PIC S9(04)  COMP   OCCURS  3  TIMES.
        05  WB-NI-VIVB9996         REDEFINES   WB-NI-VIVB9996-TAB.
           10 WB-NI-KNR-GR         PIC S9(04)  COMP.
           10 WB-NI-KNR-GREL       PIC S9(04)  COMP.
           10 WB-NI-KTY-GREL       PIC S9(04)  COMP.
        05  WB-VIVB9996.
           10 WB-KNR-GR            PIC S9(4) USAGE COMP.
           10 WB-KNR-GREL          PIC S9(4) USAGE COMP.
           10 WB-KTY-GREL          PIC S9(4) USAGE COMP.
