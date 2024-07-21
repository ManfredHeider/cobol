      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA023.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVBA023.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:15:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVBA023                                      * 00022006
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: SCHÖLLHAMMER                                     * 00060002
      *    DATUM:    24.06.1994                                       * 00070001
      ***************************************************************** 00070000
      *    VERSION   1    18.07.94 17:25:35
      ****************************************************************  00090000
      * AENDERUNGEN:                                                 *  00100000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00101000
      * -------------------------------------------------------------*  00102000
      *         !24.06.94!    ! NEU                       ! SCHÖLLH. *  00103000
      *         !18.07.94! 01 ! PROD.UEBERGABE            ! SCHÖLLH. *
      *         !        !    !                           !          *  00107304
      *                                                              *  00108000
      * ENDE-LOGBUCH                                                 *  00109000
      ****************************************************************  00110000
      *                                                                 00100000
        05  WB-NI-VIVBA023-TAB.
           10  WB-NI-VIVBA023-ELE  PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  WB-NI-VIVBA023          REDEFINES   WB-NI-VIVBA023-TAB.
           10 WB-NI-ED-ST-NR        PIC S9(04)  COMP.
           10 WB-NI-EDD-BEZUGS-NR   PIC S9(04)  COMP.
           10 WB-NI-EDD-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-EDD-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-EDD-DAT-BIS     PIC S9(04)  COMP.
           10 WB-NI-KTY-BEZUGS-TYP  PIC S9(04)  COMP.
           10 WB-NI-KNR-BEZUGS-TYP  PIC S9(04)  COMP.
           10 WB-NI-KTY-EDD-URSACHE PIC S9(04)  COMP.
           10 WB-NI-KNR-EDD-URSACHE PIC S9(04)  COMP.
        05  WB-VIVBA023.
           10 WB-ED-ST-NR           PIC S9(09)  COMP       VALUE ZERO.
           10 WB-EDD-BEZUGS-NR      PIC S9(09)  COMP       VALUE ZERO.
           10 WB-EDD-DAT-VON        PIC  X(26)             VALUE SPACE.
           10 WB-EDD-LAST-UPDATE    PIC  X(26)             VALUE SPACE.
           10 WB-EDD-DAT-BIS        PIC  X(10)             VALUE SPACE.
           10 WB-KTY-BEZUGS-TYP     PIC S9(04)  COMP       VALUE ZERO.
           10 WB-KNR-BEZUGS-TYP     PIC S9(04)  COMP       VALUE ZERO.
           10 WB-KTY-EDD-URSACHE    PIC S9(04)  COMP       VALUE ZERO.
           10 WB-KNR-EDD-URSACHE    PIC S9(04)  COMP       VALUE ZERO.
      *
      **** ENDE COPY-MEMBER ********************************************
