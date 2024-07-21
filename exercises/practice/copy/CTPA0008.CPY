      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0008.CPY  $
      *    $Revision:   1.2  $
      *
      *    $Log:   L:/MF/CPY/CTPA0008.CPv  $
      *
      *   Rev 1.2   Mar 06 1997 14:11:58   P13PVCS
      *Neuerstellung
      *
      *   Rev 1.1   Oct 31 1996 16:35:42   T13AE19
      *neuerstellung
      *
      *   Rev 1.0   Oct 31 1996 14:26:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0008                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0008                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SEIBEL                                           *
      *    DATUM:    30.10.1996                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0008)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0008))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
bb    *    EXEC SQL DECLARE T13AE.TBPA0008 TABLE
CCC005*    EXEC SQL DECLARE TBPA0008 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      KTY_GES                        SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KTY_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KNR_EXPORT_ART                 SMALLINT NOT NULL,
CCC004*      KTY_EXPORT_ART                 SMALLINT NOT NULL,
CCC004*      KNR_VERTRIEB_GES               SMALLINT NOT NULL,
CCC004*      KTY_VERTRIEB_GES               SMALLINT NOT NULL,
CCC004*      PGEB_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PGEB_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0008                     *
      ******************************************************************
bb    *01  DCLTBPA0008.
BB     01  TBPA0008-GESAMT.
BB         05  DCL-TBPA0008.
               10 PGEB-GES         PIC S9(4) USAGE COMP.
               10 KTY-GES          PIC S9(4) USAGE COMP.
               10 KNR-WAEHRUNG     PIC S9(4) USAGE COMP.
               10 KTY-WAEHRUNG     PIC S9(4) USAGE COMP.
               10 KNR-EXPORT-ART   PIC S9(4) USAGE COMP.
               10 KTY-EXPORT-ART   PIC S9(4) USAGE COMP.
               10 KNR-VERTRIEB-GES PIC S9(4) USAGE COMP.
               10 KTY-VERTRIEB-GES PIC S9(4) USAGE COMP.
               10 PGEB-LAST-UPDATE PIC X(26).
               10 PGEB-KENNUNG     PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
