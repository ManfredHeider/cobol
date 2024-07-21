      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   ctpa0018.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\ctpa0018.CPv  $
      *
      *   Rev 2.1   Feb 23 2000 13:57:38   T13EX21
      *APA00081 - Erweiterung um PGEB-GES
      *
      *   Rev 2.0   Feb 27 1999 14:30:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   Mar 06 1997 14:12:40   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:31:20   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:28:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0018                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0018                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * APA00081!09.02.00!2.1 ! ERWEITERUNG UM PGEB-GES   ! STORCH    *
      *         !        !    !                           !           *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0018)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0018))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
BB    *    EXEC SQL DECLARE T13AE.TBPA0018 TABLE
CCC005*    EXEC SQL DECLARE TBPA0018 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROT_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PROZ_ZEILE                     SMALLINT NOT NULL,
CCC004*      PROZ_TEXT                      CHAR(80) NOT NULL,
CCC004*      PROZ_BETRAG_JN                 CHAR(1) NOT NULL,
CCC004*      PROZ_BETRAG                    DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0018                     *
      ******************************************************************
BB    *01  DCLTBPA0018.
BB     01  TBPA0018-GESAMT.
BB         05  DCL-TBPA0018.
090200         10 PGEB-GES         PIC S9(4) USAGE COMP.
               10 PROT-ID          PIC S9(15)V USAGE COMP-3.
               10 PROZ-ZEILE       PIC S9(4) USAGE COMP.
               10 PROZ-TEXT        PIC X(80).
               10 PROZ-BETRAG-JN   PIC X(1).
               10 PROZ-BETRAG      PIC S9(9)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
