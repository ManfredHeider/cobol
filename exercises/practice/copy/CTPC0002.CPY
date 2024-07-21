      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPC0002.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTPC0002.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:47:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 01 1997 16:37:32   T13AE26
      *AFVB0303: Neue Tabelle TBPC0002-BE 'Bereinigte Einheiten'
      *
      *   Rev 1.0   Apr 01 1997 13:45:50   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPC0002                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPC0002 (BE)                                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T. Moll                                          *
      *    DATUM:    09.10.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0303!09.10.96!    ! NEU                       ! T. Moll  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      * DCLGEN TABLE(TBPC0002)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTPC0002))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPC0002 TABLE
CCC004*    (  MA_ST_NR           INTEGER              NOT NULL,
CCC004*       BE_MONAT           DATE                 NOT NULL,
CCC004*       BE_NEU_STF         SMALLINT             NOT NULL,
CCC004*       BE_ZEITRAUM        SMALLINT             NOT NULL,
CCC004*       BE_EH_GS_NT_GR     DECIMAL ( 10, 1 )    NOT NULL,
CCC004*       BE_EH_GS_NT_GES    DECIMAL ( 10, 1 )    NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPC0002                           *
      ******************************************************************
       01  TBPC0002-GESAMT.
        05  NI-TBPC0002-TAB.
           10  NI-TBPC0002-ELE     PIC S9(04)  COMP   OCCURS  6  TIMES.
        05  NI-TBPC0002            REDEFINES   NI-TBPC0002-TAB.
           10 NI-MA-ST-NR          PIC S9(4) USAGE COMP.
           10 NI-BE-MONAT          PIC S9(4) USAGE COMP.
           10 NI-BE-NEU-STF        PIC S9(4) USAGE COMP.
           10 NI-BE-ZEITRAUM       PIC S9(4) USAGE COMP.
           10 NI-BE-EH-GS-NT-GR    PIC S9(4) USAGE COMP.
           10 NI-BE-EH-GS-NT-GES   PIC S9(4) USAGE COMP.
        05  DCL-TBPC0002.
           10 MA-ST-NR             PIC  S9(09)    COMP.
           10 BE-MONAT             PIC   X(10).
           10 BE-NEU-STF           PIC  S9(04)    COMP.
           10 BE-ZEITRAUM          PIC  S9(04)    COMP.
           10 BE-EH-GS-NT-GR       PIC  S9(09)V9  COMP-3.
           10 BE-EH-GS-NT-GES      PIC  S9(09)V9  COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
