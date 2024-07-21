      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVAT0016.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVAT0016.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:13:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVAT0016                                      * 00020000
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: GIESE                                            * 00060000
      *    DATUM:    18.11.1994                                       * 00070000
      ***************************************************************** 00080000
      *    VERSION   1    10.01.95 16:42:13
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
      * COBOL DECLARATION FOR TABLE VIAT0016                           *
      ******************************************************************
        05  WB-NI-VIAT0016-TAB.
           10 WB-NI-VIAT0016-ELE   PIC S9(04) COMP  OCCURS 12 TIMES.
        05  WB-NI-VIAT0016         REDEFINES  WB-NI-VIAT0016-TAB.
           10 WB-NI-KNR-PROD-GB-GES  PIC S9(4) USAGE COMP.
           10 WB-NI-PRPS-PROD-SCHL   PIC S9(4) USAGE COMP.
           10 WB-NI-PRPS-DAT-VON     PIC S9(4) USAGE COMP.
           10 WB-NI-PRPS-LAST-UPDATE PIC S9(4) USAGE COMP.
           10 WB-NI-PRPS-DAT-BIS     PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-PROD-SPARTE  PIC S9(4) USAGE COMP.
           10 WB-NI-KNR-ART-PROV-TYP PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-PROD-GB-GES  PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-PROD-SPARTE  PIC S9(4) USAGE COMP.
           10 WB-NI-KTY-ART-PROV-TYP PIC S9(4) USAGE COMP.
           10 WB-NI-PRPS-PROD-BEZ    PIC S9(4) USAGE COMP.
           10 WB-NI-PRPS-PROD-BEZ10  PIC S9(4) USAGE COMP.
        05  WB-VIAT0016.
           10 WB-KNR-PROD-GB-GES     PIC S9(4) USAGE COMP.
           10 WB-PRPS-PROD-SCHL      PIC S9(4) USAGE COMP.
           10 WB-PRPS-DAT-VON        PIC  X(26).
           10 WB-PRPS-LAST-UPDATE    PIC  X(26).
           10 WB-PRPS-DAT-BIS        PIC  X(10).
           10 WB-KNR-PROD-SPARTE     PIC S9(4) USAGE COMP.
           10 WB-KNR-ART-PROV-TYP    PIC S9(4) USAGE COMP.
           10 WB-KTY-PROD-GB-GES     PIC S9(4) USAGE COMP.
           10 WB-KTY-PROD-SPARTE     PIC S9(4) USAGE COMP.
           10 WB-KTY-ART-PROV-TYP    PIC S9(4) USAGE COMP.
           10 WB-PRPS-PROD-BEZ       PIC  X(40).
           10 WB-PRPS-PROD-BEZ10     PIC  X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
