      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVAT0017.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVAT0017.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:14:12   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVAT0017                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: GIESE                                            * 00060000
      *    DATUM:    18.11.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    10.01.95 16:42:42
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * AFVB0142!18.11.94!    ! NEU                       ! GIESE    *  00140000
      *         !23.12.94! 01 ! ANPASSUNG NEUES FORMAT    ! GIESE    *  00145000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0017                           *
      ******************************************************************
        05  WB-NI-VIAT0017-TAB.
           10 WB-NI-VIAT0017-ELE   PIC S9(04) COMP  OCCURS 9 TIMES.
        05  WB-NI-VIAT0017         REDEFINES  WB-NI-VIAT0017-TAB.
           10 WB-NI-KNR-PROD-GB-GES  PIC S9(4) USAGE COMP.
           10 WB-NI-PGES-DAT-VON     PIC S9(4) USAGE COMP.
           10 WB-NI-PGES-LAST-UPDATE PIC S9(4) USAGE COMP.
           10 WB-NI-PGES-DAT-BIS     PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-AUSL         PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-BEWERT-ART   PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-PROD-GB-GES  PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-AUSL         PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-BEWERT-ART   PIC S9(4) USAGE COMP.
        05  WB-VIAT0017.
           10 WB-KNR-PROD-GB-GES     PIC S9(4) USAGE COMP.
           10 WB-PGES-DAT-VON        PIC  X(26).
           10 WB-PGES-LAST-UPDATE    PIC  X(26).
           10 WB-PGES-DAT-BIS        PIC  X(10).
           10 WB-KNR-AUSL            PIC S9(4) USAGE COMP.
           10 WB-KNR-BEWERT-ART      PIC S9(4) USAGE COMP.
           10 WB-KTY-PROD-GB-GES     PIC S9(4) USAGE COMP.
           10 WB-KTY-AUSL            PIC S9(4) USAGE COMP.
           10 WB-KTY-BEWERT-ART      PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
