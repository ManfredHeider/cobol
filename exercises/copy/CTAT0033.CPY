      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0033.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\CTAT0033.CPv  $
      *
      *   Rev 2.1   02 May 2001 09:26:32   t13ex49
      *AKI00161 - erstmalige Prod-Übergabe
      *
      *   Rev 2.0   Mar 18 1999 10:44:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00161!19.03.01!    ! Neuerstellung             !A.Blaess  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *****************************************************************
CCC005*    EXEC SQL DECLARE TBAT0033 TABLE
CCC004*    (FR_SYSCODE           CHAR (2)   NOT NULL,
CCC004*     FR_JOBNAME           CHAR (8)   NOT NULL,
CCC004*     FR_PROGRAMMNAME      CHAR (8)   NOT NULL,
CCC004*     FR_FEHLERNUMMER      SMALLINT   NOT NULL,
CCC004*     FR_FELDNAME          CHAR (25)  NOT NULL,
CCC004*     FR_RELEVANZ          CHAR (1)   NOT NULL,
CCC004*     FR_REAKTION          CHAR (1)   NOT NULL,
CCC004*     FR_REAKTIONHINWEIS   CHAR (25)  NOT NULL,
CCC004*     LETZTEAENDERUNG      TIMESTAMP  NOT NULL  WITH DEFAULT,
CCC004*     FR_USERID            CHAR (7)   NOT NULL
CCC003*    ) END-EXEC.

       01  TBAT0033-GESAMT.
           05  DCL-TBAT0033.
               10 FR-SYSCODE                     PIC X(2).
               10 FR-JOBNAME                     PIC X(8).
               10 FR-PROGRAMMNAME                PIC X(8).
               10 FR-FEHLERNUMMER                PIC S9(4) COMP.
               10 FR-FELDNAME                    PIC X(25).
               10 FR-RELEVANZ                    PIC X.
               10 FR-REAKTION                    PIC X.
               10 FR-REAKTIONHINWEIS             PIC X(25).
               10 LETZTEAENDERUNG                PIC X(26).
               10 FR-USERID                      PIC X(7).
