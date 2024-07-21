      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVPC0004.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\CVPC0004.CPv  $
      *
      *   Rev 2.2   30 Jul 2001 10:15:38   t13ae23
      *APC00050 - Eigenbereinigte Einheiten für UM15 -
      *
      *   Rev 2.1   23 Jan 2001 12:03:46   T13AE28
      *AVB00480: Ber.Eig.E für RD1-D
      *
      *   Rev 2.0   28 Sep 2000 12:16:46   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVPC0004                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON VIPC0004 (BEEG)                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    23.11.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00480!23.11.00!    ! NEU                       ! S. Walb  *
      * APC00050!20.07.01!2.2 ! KORREKTUR NAME BEEG..ABZUG!          *
      *         !        !    ! IN BEEG_...ABZG           ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*
      * DCLGEN TABLE(VIPC0004)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVPC0004))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIPC0004 TABLE
CCC004*    (  MA_ST_NR            INTEGER              NOT NULL,
CCC004*       BEEG_MONAT          DATE                 NOT NULL,
CCC004*       BEEG_NEU_STF        SMALLINT             NOT NULL,
CCC004*       BEEG_ZEITRAUM       SMALLINT             NOT NULL,
CCC004*       BEEG_EH_NT_EG_GES   DECIMAL ( 10, 1 )    NOT NULL,
200701*       BEEG_EH_NT_EG_ABZUG DECIMAL ( 10, 1 )    NOT NULL,
CCC004*       BEEG_EH_NT_EG_ABZG  DECIMAL ( 10, 1 )    NOT NULL,
CCC004*       BEEG_EH_NT_GS_GES   DECIMAL ( 10, 1 )    NOT NULL,
CCC004*       BEEG_EH_NT_GS_BER   DECIMAL ( 10, 1 )    NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIPC0004                           *
      ******************************************************************
       01  VIPC0004-GESAMT.
        05  NI-VIPC0004-TAB.
           10  NI-VIPC0004-ELE     PIC S9(04)  COMP   OCCURS  8  TIMES.
        05  NI-VIPC0004              REDEFINES   NI-VIPC0004-TAB.
           10 NI-MA-ST-NR            PIC S9(04) USAGE COMP.
           10 NI-BEEG-MONAT          PIC S9(04) USAGE COMP.
           10 NI-BEEG-NEU-STF        PIC S9(04) USAGE COMP.
           10 NI-BEEG-ZEITRAUM       PIC S9(04) USAGE COMP.
           10 NI-BEEG-EH-NT-EG-GES   PIC S9(04) USAGE COMP.
200701*    10 NI-BEEG-EH-NT-EG-ABZUG PIC S9(04) USAGE COMP.
200701     10 NI-BEEG-EH-NT-EG-ABZG  PIC S9(04) USAGE COMP.
           10 NI-BEEG-EH-NT-GS-GES   PIC S9(04) USAGE COMP.
           10 NI-BEEG-EH-NT-GS-BER   PIC S9(04) USAGE COMP.
        05  DCL-VIPC0004.
           10 MA-ST-NR               PIC S9(09)     COMP.
           10 BEEG-MONAT             PIC X(10).
           10 BEEG-NEU-STF           PIC S9(04)     COMP.
           10 BEEG-ZEITRAUM          PIC S9(04)     COMP.
           10 BEEG-EH-NT-EG-GES      PIC S9(09)V9   COMP-3.
200701*    10 BEEG-EH-NT-EG-ABZUG    PIC S9(09)V9   COMP-3.
200701     10 BEEG-EH-NT-EG-ABZG     PIC S9(09)V9   COMP-3.
           10 BEEG-EH-NT-GS-GES      PIC S9(09)V9   COMP-3.
           10 BEEG-EH-NT-GS-BER      PIC S9(09)V9   COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
