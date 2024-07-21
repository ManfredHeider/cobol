      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB004.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVBB004.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:16:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010001
      *    COPY-MEMBER: WVVBB004                                      * 00020001
      *    --------------------------------------------------------   * 00030001
      *    ERSTELLT: H. LEISTE                                        * 00040001
      *    DATUM:    09.12.1993                                       * 00050001
      ***************************************************************** 00060001
      *    VERSION   1    22.06.94 18:06:49
      ****************************************************************  00080001
      * AENDERUNGEN:                                                 *  00090001
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00100001
      * -------------------------------------------------------------*  00110001
      *         !09.12.93! 01 ! NEU                       ! LEISTE   *  00120001
      * AFVB0061!10.01.94!    ! MA-AUN-BER FELDER GELÖSCHT! SCHÖLLH. *  00130001
      *         !20.01.94! 02 ! PROD.UEBERGABE            ! HECKER   *  00041800
      * AFVB0110!11.05.94! 03 ! MA-ZV-VERL EINGEFÜHRT     ! BERGHOFER*  00160001
      *         !        !    !                           !          *  00170001
      *                                                              *  00180001
      * ENDE-LOGBUCH                                                 *  00190001
      ****************************************************************  00200001
      *                                                                 00100000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB004                           *
      ******************************************************************
        05  WB-NI-VIVBB004-TAB.
           10  WB-NI-VIVBB004-ELE  PIC S9(04)  COMP   OCCURS  26 TIMES.
        05  WB-NI-VIVBB004           REDEFINES   WB-NI-VIVBB004-TAB.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-MA-LAST-UPDATE   PIC S9(04)  COMP.
           10 WB-NI-KTY-MA-TYP       PIC S9(04)  COMP.
           10 WB-NI-KNR-MA-TYP       PIC S9(04)  COMP.
           10 WB-NI-MA-BDV-MITGLIED  PIC S9(04)  COMP.
           10 WB-NI-MA-STRAFE        PIC S9(04)  COMP.
           10 WB-NI-MA-VERSANDSTOP   PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-ZVII-ABL   PIC S9(04)  COMP.
           10 WB-NI-MA-RCK-STZ       PIC S9(04)  COMP.
           10 WB-NI-MA-RCK-STZ-DAT   PIC S9(04)  COMP.
           10 WB-NI-MA-FUEH-ZEUG-DAT PIC S9(04)  COMP.
           10 WB-NI-MA-SCHUFA-DAT    PIC S9(04)  COMP.
           10 WB-NI-MA-34C-DAT       PIC S9(04)  COMP.
           10 WB-NI-MA-AFO           PIC S9(04)  COMP.
           10 WB-NI-MA-DARLEHEN      PIC S9(04)  COMP.
           10 WB-NI-MA-AVAG-DVAG     PIC S9(04)  COMP.
           10 WB-NI-MA-KOMPASS-AVAG  PIC S9(04)  COMP.
           10 WB-NI-MA-AUTOTELEFON   PIC S9(04)  COMP.
           10 WB-NI-MA-TELETEXT      PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-NR         PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-DAT-BIS-R  PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-DAT-BIS    PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-DAT-VON    PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-KUE-DAT    PIC S9(04)  COMP.
           10 WB-NI-MA-ED-ZUSATZ     PIC S9(04)  COMP.
           10 WB-NI-MA-ZV-VERL       PIC S9(04)  COMP.
        05  WB-VIVBB004.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-MA-LAST-UPDATE    PIC X(26)                VALUE SPACE.
           10 WB-KTY-MA-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-MA-TYP        PIC S9(4)   COMP         VALUE ZERO.
           10 WB-MA-BDV-MITGLIED   PIC X(1)                 VALUE SPACE.
           10 WB-MA-STRAFE         PIC X(1)                 VALUE SPACE.
           10 WB-MA-VERSANDSTOP    PIC X(1)                 VALUE SPACE.
           10 WB-MA-ZV-ZVII-ABL    PIC X(1)                 VALUE SPACE.
           10 WB-MA-RCK-STZ        PIC S999V99   COMP-3     VALUE ZERO.
           10 WB-MA-RCK-STZ-DAT    PIC X(10)                VALUE SPACE.
           10 WB-MA-FUEH-ZEUG-DAT  PIC X(10)                VALUE SPACE.
           10 WB-MA-SCHUFA-DAT     PIC X(10)                VALUE SPACE.
           10 WB-MA-34C-DAT        PIC X(10)                VALUE SPACE.
           10 WB-MA-AFO            PIC X(1)                 VALUE SPACE.
           10 WB-MA-DARLEHEN       PIC X(1)                 VALUE SPACE.
           10 WB-MA-AVAG-DVAG      PIC X(10)                VALUE SPACE.
           10 WB-MA-KOMPASS-AVAG   PIC X(10)                VALUE SPACE.
           10 WB-MA-AUTOTELEFON    PIC X(14)                VALUE SPACE.
           10 WB-MA-TELETEXT       PIC X(14)                VALUE SPACE.
           10 WB-MA-ZV-NR          PIC X(1)                 VALUE SPACE.
           10 WB-MA-ZV-DAT-BIS-R   PIC X(10)                VALUE SPACE.
           10 WB-MA-ZV-DAT-BIS     PIC X(10)                VALUE SPACE.
           10 WB-MA-ZV-DAT-VON     PIC X(10)                VALUE SPACE.
           10 WB-MA-ZV-KUE-DAT     PIC X(10)                VALUE SPACE.
           10 WB-MA-ED-ZUSATZ      PIC S9(04)  COMP         VALUE ZERO.
           10 WB-MA-ZV-VERL        PIC X(1)                 VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 26      *
      ******************************************************************
