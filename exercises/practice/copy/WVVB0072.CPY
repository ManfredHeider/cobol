      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0072.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVVB0072.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Apr 30 1996 17:10:12   T13EX16
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0072                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON VIVB0072 (ZPKT)                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J. SCHINDLER                                     *
      *    DATUM:    12.12.1995                                       *
      *****************************************************************
      *    VERSION   1    02.10.95 15:15:43
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0266!12.12.95! 01 ! NEU                       !SCHINDLER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0072                           *
      ******************************************************************
        05 WB-NI-VIVB0072-TAB.
           10 WB-NI-VIVB0072-ELE   PIC S9(04) COMP  OCCURS 4 TIMES.
        05 WB-NI-VIVB0072          REDEFINES  WB-NI-VIVB0072-TAB.
           10 WB-NI-KNR-STEUER-INFO PIC S9(4)   USAGE COMP.
           10 WB-NI-ZPKT-ZEITPUNKT PIC S9(4)    USAGE COMP.
           10 WB-NI-ZPKT-LAST-UPDATE PIC S9(4)  USAGE COMP.
           10 WB-NI-KTY-STEUER-INFO PIC S9(4)   USAGE COMP.
        05 WB-VIVB0072.
           10 WB-KNR-STEUER-INFO   PIC S9(4)    USAGE COMP.
           10 WB-ZPKT-ZEITPUNKT    PIC X(26).
           10 WB-ZPKT-LAST-UPDATE  PIC X(26).
           10 WB-KTY-STEUER-INFO   PIC S9(4)    USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
