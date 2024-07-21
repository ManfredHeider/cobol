      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA030.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVBA030.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:15:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVBA030                                      * 00022007
      *    --------------------------------------------------------   * 00030000
      *    ERSTELLT: DB2 SYSTEMGRUPPE                                 * 00060002
      *    DATUM:    22.04.1993                                       * 00070001
      *    STAND:    22.04.1993                                       * 00080001
      *    --------------------------------------------------------   * 00081001
      *    VERSION:  1    22.06.94 18:06:25
      ***************************************************************** 00090000
      * AENDERUNGEN:                                                 *  00090001
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00100001
      * -------------------------------------------------------------*  00110001
      *         !22.04.93! 01 ! NEU                       ! DB2-     *  00120001
      *         !        !    !                           ! GRUPPE   *  00130001
      * AFVB0110!10.05.94! 02 ! ATTRIBUT ZVZ-VERL EINGE-  ! BERGHOFER*  00130001
      *         !        !    ! FÜHRT                     !          *  00160001
      *         !        !    !                           !          *  00170001
      *                                                              *  00180001
      * ENDE-LOGBUCH                                                 *  00190001
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA030                           *
      ******************************************************************
        05  WB-NI-VIVBA030-TAB.
           10  WB-NI-VIVBA030-ELE  PIC S9(04)  COMP   OCCURS  8  TIMES.
        05  WB-NI-VIVBA030           REDEFINES   WB-NI-VIVBA030-TAB.
           10 WB-NI-MA-ST-NR         PIC S9(04)  COMP.
           10 WB-NI-ZV-NR            PIC S9(04)  COMP.
           10 WB-NI-ZVZ-DAT-VON      PIC S9(04)  COMP.
           10 WB-NI-ZVZ-LAST-UPDATE  PIC S9(04)  COMP.
           10 WB-NI-ZVZ-DAT-BIS      PIC S9(04)  COMP.
           10 WB-NI-ZVZ-KUE-DAT      PIC S9(04)  COMP.
           10 WB-NI-ZVZ-DAT-BIS-R    PIC S9(04)  COMP.
           10 WB-NI-ZVZ-VERL         PIC S9(04)  COMP.
        05  WB-VIVBA030.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-ZV-NR             PIC X(1)                 VALUE SPACE.
           10 WB-ZVZ-DAT-VON       PIC X(10)                VALUE SPACE.
           10 WB-ZVZ-LAST-UPDATE   PIC X(26)                VALUE SPACE.
           10 WB-ZVZ-DAT-BIS       PIC X(10)                VALUE SPACE.
           10 WB-ZVZ-KUE-DAT       PIC X(10)                VALUE SPACE.
           10 WB-ZVZ-DAT-BIS-R     PIC X(10)                VALUE SPACE.
           10 WB-ZVZ-VERL          PIC X(1)                 VALUE SPACE.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
