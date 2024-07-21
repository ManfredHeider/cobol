      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0026.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0026.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:22:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0026                                      * 00020002
      *    --------------------------------------------------------   * 00030000
      *    WORKBEREICH FÜR VIVB0026 (OLZZ)                            * 00040002
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070002
      ***************************************************************** 00080000
      *    VERSION   1    23.02.95 15:05:22
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      *         !22.04.93! 01 ! NEU                       ! SYSTEMGRP*  00140002
      * AFVB0183!02.02.95! 02 ! VERSCH. OLZ-BETRÄGE ÜBER  ! SCHÖLLH. *  00140002
      *         !        !    !  OLZ-TYP GESTEUERT        !          *  00143000
      *         !        !    !                           !          *  00143000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0026                           *
      ******************************************************************
020295  05  WB-NI-VIVB0026-TAB.
020295     10  WB-NI-VIVB0026-ELE  PIC S9(04)  COMP   OCCURS  8  TIMES.
020295  05  WB-NI-VIVB0026         REDEFINES   WB-NI-VIVB0026-TAB.
020295     10 WB-NI-MA-ST-NR           PIC S9(04)   COMP.
020295     10 WB-NI-OLZZ-MONAT         PIC S9(04)   COMP.
020295     10 WB-NI-KNR-OLZ-TYP        PIC S9(04)   COMP.
020295     10 WB-NI-KNR-WAEHRUNG       PIC S9(04)   COMP.
020295     10 WB-NI-OLZZ-LAST-UPDATE   PIC S9(04)   COMP.
020295     10 WB-NI-KTY-OLZ-TYP        PIC S9(04)   COMP.
020295     10 WB-NI-KTY-WAEHRUNG       PIC S9(04)   COMP.
020295     10 WB-NI-OLZZ-BETRAG        PIC S9(04)   COMP.
020295  05  WB-VIVB0026.
020295     10 WB-MA-ST-NR              PIC S9(09)    COMP   VALUE ZERO.
020295     10 WB-OLZZ-MONAT            PIC  X(10)           VALUE SPACE.
020295     10 WB-KNR-OLZ-TYP           PIC S9(04)    COMP   VALUE ZERO.
020295     10 WB-KNR-WAEHRUNG          PIC S9(04)    COMP   VALUE ZERO.
020295     10 WB-OLZZ-LAST-UPDATE      PIC  X(26)           VALUE SPACE.
020295     10 WB-KTY-OLZ-TYP           PIC S9(04)    COMP   VALUE ZERO.
020295     10 WB-KTY-WAEHRUNG          PIC S9(04)    COMP   VALUE ZERO.
020295     10 WB-OLZZ-BETRAG           PIC S9(09)V99 COMP-3 VALUE ZERO.

020295* 05  WB-NI-VIVB0026-TAB.
020295*    10  WB-NI-VIVB0026-ELE  PIC S9(04)  COMP   OCCURS  6  TIMES.
020295* 05  WB-NI-VIVB0026         REDEFINES   WB-NI-VIVB0026-TAB.
020295*    10 WB-NI-MA-ST-NR          PIC S9(04)  COMP.
020295*    10 WB-NI-OLZZ-QUARTAL      PIC S9(04)  COMP.
020295*    10 WB-NI-OLZZ-LAST-UPDATE  PIC S9(04)  COMP.
020295*    10 WB-NI-OLZZ-BUERO        PIC S9(04)  COMP.
020295*    10 WB-NI-OLZZ-LEISTUNG     PIC S9(04)  COMP.
020295*    10 WB-NI-OLZZ-ORG          PIC S9(04)  COMP.
020295* 05  WB-VIVB0026.
020295*    10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
020295*    10 WB-OLZZ-QUARTAL      PIC X(10)                VALUE SPACE.
020295*    10 WB-OLZZ-LAST-UPDATE  PIC X(26)                VALUE SPACE.
020295*    10 WB-OLZZ-BUERO        PIC S9(9)V99   COMP-3    VALUE ZERO.
020295*    10 WB-OLZZ-LEISTUNG     PIC S9(9)V99   COMP-3    VALUE ZERO.
020295*    10 WB-OLZZ-ORG          PIC S9(9)V99   COMP-3    VALUE ZERO.
      ******************************************************************
020295* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
