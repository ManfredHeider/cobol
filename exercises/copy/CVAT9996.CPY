      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   cvat9996.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\cvat9996.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Dec 29 1995 15:56:14   T13AE26
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY: CVAT9996                                             *
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
      * DCLGEN TABLE(VIAT9996)                                         *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT9996 TABLE
CCC004*    ( KNR_GR                         SMALLINT NOT NULL,
CCC004*      KNR_GREL                       SMALLINT NOT NULL,
CCC004*      KTY_GREL                       SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT9996
      ******************************************************************
       01  VIAT9996-GESAMT.
        05  NI-VIAT9996-TAB.
           10  NI-VIAT9996-ELE     PIC S9(04)  COMP   OCCURS  3  TIMES.
        05  NI-VIAT9996            REDEFINES   NI-VIAT9996-TAB.
           10 NI-KNR-GR            PIC S9(04)  COMP.
           10 NI-KNR-GREL          PIC S9(04)  COMP.
           10 NI-KTY-GREL          PIC S9(04)  COMP.
        05  DCL-VIAT9996.
           10 KNR-GR               PIC S9(4) USAGE COMP.
           10 KNR-GREL             PIC S9(4) USAGE COMP.
           10 KTY-GREL             PIC S9(4) USAGE COMP.
