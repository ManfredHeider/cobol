      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WTVB0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WTVB0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:09:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00001000
      *    COPY-MEMBER/ARBEITSBEREICH FÜR DIE DB2-TABLE ST            * 00002000
      *    --------------------------------------------------------   * 00002200
      *    ERSTELLT: KUHN                                             * 00002300
      *    DATUM:    31.03.1994                                       * 00002400
      ***************************************************************** 00002500
      *    VERSION   1    31.03.94 18:12:47
      ****************************************************************  00002700
      * AENDERUNGEN:                                                 *  00002800
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00002900
      * -------------------------------------------------------------*  00003000
      * AFVB0044!31.03.94!    ! NEU                       ! KUHN     *  00004001
      *         !        !    !                           !          *  00005000
      *         !        !    !                           !          *  00006000
      *         !        !    !                           !          *  00007000
      *         !        !    !                           !          *  00008000
      *         !        !    !                           !          *  00009000
      *         !        !    !                           !          *  00010000
      *         !        !    !                           !          *  00010100
      *                                                              *  00010200
      * ENDE-LOGBUCH                                                 *  00010300
      ****************************************************************  00010400
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0001                           *
      ******************************************************************
        05  WB-NI-TBVB0001-TAB.
           10 WB-NI-TBVB0001-ELE   PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  WB-NI-TBVB0001            REDEFINES   WB-NI-TBVB0001-TAB.
           10 WB-NI-ST-NR             PIC S9(04)  COMP.
           10 WB-NI-ST-LAST-UPDATE    PIC S9(04)  COMP.
           10 WB-NI-ST-PRUEFZIFFER    PIC S9(04)  COMP.
           10 WB-NI-ST-FREI           PIC S9(04)  COMP.
           10 WB-NI-ST-BEREINIGUNG    PIC S9(04)  COMP.
           10 WB-NI-KTY-ST-TYP        PIC S9(04)  COMP.
           10 WB-NI-KNR-ST-TYP        PIC S9(04)  COMP.
           10 WB-NI-KTY-GESELL-TYP    PIC S9(04)  COMP.
           10 WB-NI-KNR-GESELL-TYP    PIC S9(04)  COMP.
        05  WB-TBVB0001.
           10 WB-ST-NR                PIC S9(9)   COMP      VALUE ZERO.
           10 WB-ST-LAST-UPDATE       PIC X(26)             VALUE SPACE.
           10 WB-ST-PRUEFZIFFER       PIC X(1)              VALUE SPACE.
           10 WB-ST-FREI              PIC X(1)              VALUE SPACE.
           10 WB-ST-BEREINIGUNG       PIC X(1)              VALUE SPACE.
           10 WB-KTY-ST-TYP           PIC S9(4)   COMP      VALUE ZERO.
           10 WB-KNR-ST-TYP           PIC S9(4)   COMP      VALUE ZERO.
           10 WB-KTY-GESELL-TYP       PIC S9(4)   COMP      VALUE ZERO.
           10 WB-KNR-GESELL-TYP       PIC S9(4)   COMP      VALUE ZERO.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
