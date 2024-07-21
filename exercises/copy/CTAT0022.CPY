      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0022.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTAT0022.CPv  $
      *
      *   Rev 2.1   25 Jan 2007 10:50:02   t13ex49
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB - Teil 2 
      *
      *   Rev 2.0   Feb 27 1999 11:56:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   06 Feb 1998 10:53:24   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00556!12.04.06!    ! Neu                       ! P. Pohl   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
CCC005*    EXEC SQL DECLARE SYSIBM.SYSKEYS TABLE
CCC004*    ( IXNAME                         VARCHAR(18) NOT NULL,
CCC004*      IXCREATOR                      CHAR(8) NOT NULL,
CCC004*      COLNAME                        VARCHAR(18) NOT NULL,
CCC004*      COLNO                          SMALLINT NOT NULL,
CCC004*      COLSEQ                         SMALLINT NOT NULL,
CCC004*      ORDERING                       CHAR(1) NOT NULL,
CCC004*      IBMREQD                        CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE SYSIBM.SYSKEYS                     *
      ******************************************************************
       01  DCLSYSKEYS.
           10 IXNAME.
              49 IXNAME-LEN        PIC S9(4) USAGE COMP.
              49 IXNAME-TEXT       PIC X(18).
           10 IXCREATOR            PIC X(8).
           10 COLNAME.
              49 COLNAME-LEN       PIC S9(4) USAGE COMP.
              49 COLNAME-TEXT      PIC X(18).
           10 COLNO                PIC S9(4) USAGE COMP.
           10 COLSEQ               PIC S9(4) USAGE COMP.
           10 ORDERING             PIC X(1).
           10 IBMREQD              PIC X(1).
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  ISYSKEYS.
           10 INDSTRUC           PIC S9(4) USAGE COMP OCCURS 7 TIMES.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
      *

