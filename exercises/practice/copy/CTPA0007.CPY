      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0007.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTPA0007.CPv  $
      *
      *   Rev 2.3   May 03 2019 14:00:06   u130076
      *APA02849: Erstellung eine Korrekturprogrammes für VB 9424470
      *
      *   Rev 2.2   May 28 2010 13:41:46   t13ae46
      *APA01049: Korrektur Rückstellungsdialog
      *
      *   Rev 2.1   Nov 09 2009 10:57:56   t13ex44
      *APA00820
      *
      *   REV 2.0   FEB 27 1999 14:30:02   P13SP20
      *NT-UMSTELLUNG
      *
      *   REV 1.2   MAR 06 1997 14:11:54   P13PVCS
      *NEUERSTELLUNG
      *
      *   REV 1.1   OCT 31 1996 16:35:56   T13AE19
      *NEUERSTELLUNG
      *
      *   REV 1.0   OCT 31 1996 14:26:10   P13SP99
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0007                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0007                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    31.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0007)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0007))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0007 TABLE
CCC004*    ( PGEB_GES                       SMALLINT   NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT   NOT NULL,
CCC004*      PRCK_SELKRIT                   CHAR(15)   NOT NULL,
CCC004*      PRCK_GUELT_AB                  DATE       NOT NULL,
CCC004*      PRCK_GUELT_BIS                 DATE       NOT NULL,
CCC004*      KTY_HAFTZEITTYP                SMALLINT   NOT NULL,
CCC004*      KNR_HAFTZEITTYP                SMALLINT   NOT NULL,
CCC004*      PRCK_HAFTZT_PG                 SMALLINT   NOT NULL,
CCC004*      PRCK_HAFTZT_VB                 SMALLINT   NOT NULL,
CCC004*      PRCK_LAST_UPDATE               TIMESTAMP  NOT NULL,
CCC004*      PRCK_KENNUNG                   CHAR(8)    NOT NULL

CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0007                     *
      ******************************************************************
       01  TBPA0007-GESAMT.
           05  DCL-TBPA0007.
               10 PGEB-GES                 PIC S9(4) USAGE COMP.
               10 PROD-PROD-SCHL           PIC S9(4) USAGE COMP.
               10 PRCK-SELKRIT             PIC  X(15).
               10 PRCK-GUELT-AB            PIC  X(10).
               10 PRCK-GUELT-BIS           PIC  X(10).
               10 KTY-HAFTZEITTYP          PIC S9(4) BINARY.
               10 KNR-HAFTZEITTYP          PIC S9(4) BINARY.
               10 PRCK-HAFTZT-PG           PIC S9(4) BINARY.
               10 PRCK-HAFTZT-VB           PIC S9(4) BINARY.
               10 PRCK-LAST-UPDATE         PIC  X(26).
               10 PRCK-KENNUNG             PIC  X(8).


      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************

