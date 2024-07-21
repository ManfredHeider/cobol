      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0019.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVAT0019.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Dec 29 1995 15:47:54   T13AE26
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY: CVAT0019                                             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T. Moll                                          *
      *    DATUM:    10.12.1995                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * AFVB0251!10.12.95!    ! NEU                       ! Moll      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIAT0019)                                         *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0019 TABLE
CCC004*    ( KNR_GR                         SMALLINT NOT NULL,
CCC004*      KNR_GREL                       SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0019
      ******************************************************************
       01  VIAT0019-GESAMT.
        05  NI-VIAT0019-TAB.
           10  NI-VIAT0019-ELE     PIC S9(04)  COMP   OCCURS  2  TIMES.
        05  NI-VIAT0019            REDEFINES   NI-VIAT0019-TAB.
           10 NI-KNR-GR            PIC S9(04)  COMP.
           10 NI-KNR-GR            PIC S9(04)  COMP.
        05  DCL-VIAT0019.
           10 KNR-GR               PIC S9(4) USAGE COMP.
           10 KNR-GREL             PIC S9(4) USAGE COMP.
