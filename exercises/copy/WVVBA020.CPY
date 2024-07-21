      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVBA020.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVBA020.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 02 1997 09:29:26   T13EX11
      *AFVB0303  WB für neuen View auf BES-table
      *          wg. neuem Attirb. BES_BEZUGS_NR
      *
      *   Rev 1.0   Apr 02 1997 08:56:22   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: WVVBA020 - WORKBEREICH FÜR VIVBA020          *
      *    --------------------------------------------------------  *
      *    ERSTELLT: W. SCHÖLLHAMMER                                 *
      *    DATUM:    20.11.1996                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0303!20.11.96!    ! NEU                       ! SCHÖLLH. *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA020                           *
      ******************************************************************
        05  WB-NI-VIVBA020-TAB.
           10  WB-NI-VIVBA020-ELE  PIC S9(04)  COMP   OCCURS  7  TIMES.
        05  WB-NI-VIVBA020         REDEFINES   WB-NI-VIVBA020-TAB.
           10 WB-NI-MA-ST-NR       PIC S9(04)  COMP.
           10 WB-NI-BES-DAT-VON    PIC S9(04)  COMP.
           10 WB-NI-KTY-BES-GRND   PIC S9(04)  COMP.
           10 WB-NI-KNR-BES-GRND   PIC S9(04)  COMP.
           10 WB-NI-BES-LAST-UPDATE PIC S9(04) COMP.
           10 WB-NI-BES-INFORMATION PIC S9(04) COMP.
           10 WB-NI-BES-BEZUGS-NR  PIC S9(04)  COMP.
        05  WB-VIVBA020.
           10 WB-MA-ST-NR          PIC S9(9)   COMP         VALUE ZERO.
           10 WB-BES-DAT-VON       PIC X(26)                VALUE SPACE.
           10 WB-KTY-BES-GRND      PIC S9(4)   COMP         VALUE ZERO.
           10 WB-KNR-BES-GRND      PIC S9(4)   COMP         VALUE ZERO.
           10 WB-BES-LAST-UPDATE   PIC X(26)                VALUE SPACE.
           10 WB-BES-INFORMATION   PIC X(49)                VALUE SPACE.
           10 WB-BES-BEZUGS-NR     PIC S9(9)   COMP         VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
