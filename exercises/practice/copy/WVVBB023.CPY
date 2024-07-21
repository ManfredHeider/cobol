      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBB023.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVBB023.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Jun 1998 09:56:50   T13AE26
      *AVB00391 -  Anpassung BD-Struktur
      *
      *   Rev 1.0   18 Nov 1997 15:20:48   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVBB023                                      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GRANITZA                                         *
      *    DATUM:    20.08.1997                                       *
      *****************************************************************
      *    VERSION   1    18.07.94 17:25:35
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !                                !          *
      * AVB00391!20.08.97! 01 ! NEU                       ! GRANITZA *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
200897  05  WB-NI-VIVBB023-TAB.
200897     10  WB-NI-VIVBB023-ELE  PIC S9(04)  COMP   OCCURS 10  TIMES.
200897  05  WB-NI-VIVBB023          REDEFINES   WB-NI-VIVBB023-TAB.
           10 WB-NI-ED-ST-NR        PIC S9(04)  COMP.
           10 WB-NI-EDD-BEZUGS-NR   PIC S9(04)  COMP.
           10 WB-NI-EDD-DAT-VON     PIC S9(04)  COMP.
           10 WB-NI-EDD-LAST-UPDATE PIC S9(04)  COMP.
           10 WB-NI-EDD-DAT-BIS     PIC S9(04)  COMP.
200897     10 WB-NI-BVH-UNTST-NR    PIC S9(04)  COMP.
           10 WB-NI-KTY-BEZUGS-TYP  PIC S9(04)  COMP.
           10 WB-NI-KNR-BEZUGS-TYP  PIC S9(04)  COMP.
           10 WB-NI-KTY-EDD-URSACHE PIC S9(04)  COMP.
           10 WB-NI-KNR-EDD-URSACHE PIC S9(04)  COMP.
200897  05  WB-VIVBB023.
           10 WB-ED-ST-NR           PIC S9(09)  COMP       VALUE ZERO.
           10 WB-EDD-BEZUGS-NR      PIC S9(09)  COMP       VALUE ZERO.
           10 WB-EDD-DAT-VON        PIC  X(26)             VALUE SPACE.
           10 WB-EDD-LAST-UPDATE    PIC  X(26)             VALUE SPACE.
           10 WB-EDD-DAT-BIS        PIC  X(10)             VALUE SPACE.
200897     10 WB-BVH-UNTST-NR       PIC S9(09)  COMP       VALUE ZERO.
           10 WB-KTY-BEZUGS-TYP     PIC S9(04)  COMP       VALUE ZERO.
           10 WB-KNR-BEZUGS-TYP     PIC S9(04)  COMP       VALUE ZERO.
           10 WB-KTY-EDD-URSACHE    PIC S9(04)  COMP       VALUE ZERO.
           10 WB-KNR-EDD-URSACHE    PIC S9(04)  COMP       VALUE ZERO.
      *
      **** ENDE COPY-MEMBER ********************************************
