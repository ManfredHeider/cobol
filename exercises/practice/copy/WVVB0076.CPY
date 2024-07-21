      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0076.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WVVB0076.CPv  $
      *
      *   Rev 2.1   Apr 25 2000 15:45:34   T13AE35
      *ASL00049 - neu fuer Tabelle TBVB0076
      *im Rahmen DIM7
      *
      *   Rev 2.0   Feb 18 2000 10:15:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0076                                      * 00020002
      *    --------------------------------------------------------   * 00030000
      *    WORKBEREICH FÜR VIVB0076 (OBAZ)                            * 00040002
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: Walb                                             * 00060002
      *    DATUM:    12.04.2000                                       * 00070002
      ***************************************************************** 00080000
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * ASL00049!12.04.00! 01 ! NEU                       ! Walb     *  00140002
      *         !        !    !                           !          *  00143000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ****************************************************************  00148000
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0076                           *
      ******************************************************************
        05  WB-NI-VIVB0076-TAB.
           10  WB-NI-VIVB0076-ELE  PIC S9(04)  COMP   OCCURS  8  TIMES.
        05  WB-NI-VIVB0076         REDEFINES   WB-NI-VIVB0076-TAB.
           10 WB-NI-OBAZ-ST-NR         PIC S9(04)   COMP.
           10 WB-NI-OBAZ-ANSPR-VOM     PIC S9(04)   COMP.
           10 WB-NI-OBAZ-LAST-UPDATE   PIC S9(04)   COMP.
           10 WB-NI-KTY-WAEHRUNG       PIC S9(04)   COMP.
           10 WB-NI-KNR-WAEHRUNG       PIC S9(04)   COMP.
           10 WB-NI-OBAZ-ANSPR-BETRAG  PIC S9(04)   COMP.
           10 WB-NI-KTY-ANSPR-STATUS   PIC S9(04)   COMP.
           10 WB-NI-KNR-ANSPR-STATUS   PIC S9(04)   COMP.
        05  WB-VIVB0076.
           10 WB-OBAZ-ST-NR            PIC S9(09)    COMP   VALUE ZERO.
           10 WB-OBAZ-ANSPR-VOM        PIC  X(10)           VALUE SPACE.
           10 WB-OBAZ-LAST-UPDATE      PIC  X(26)           VALUE SPACE.
           10 WB-KTY-WAEHRUNG          PIC S9(04)    COMP   VALUE ZERO.
           10 WB-KNR-WAEHRUNG          PIC S9(04)    COMP   VALUE ZERO.
           10 WB-OBAZ-ANSPR-BETRAG     PIC S9(09)V99 COMP-3 VALUE ZERO.
           10 WB-KTY-ANSPR-STATUS      PIC S9(04)    COMP   VALUE ZERO.
           10 WB-KNR-ANSPR-STATUS      PIC S9(04)    COMP   VALUE ZERO.

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
