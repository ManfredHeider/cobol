      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0086.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/CVVB0086.CPv  $
      *
      *   Rev 2.1   Jan 15 2003 09:27:14   t13ae27
      *neu
      *
      *   Rev 2.0   20 Feb 2001 13:22:24   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0086)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0086))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0086 TABLE
CCC004*    (
CCC004*       BBON_BETR_NR           INTEGER   NOT NULL,
CCC004*       BBON_UNTST_NR          INTEGER   NOT NULL,
CCC004*       BBON_DAT_ANSPRUCH      DATE      NOT NULL,
CCC004*       BBON_DAT_VON           TIMESTAMP NOT NULL,
CCC004*       BBON_DAT_BIS           DATE      NOT NULL,
CCC004*       BBON_DAT_VON_BETR      TIMESTAMP NOT NULL,
CCC004*       BBON_PROV_BETR         DECIMAL(05, 2) NOT NULL,
CCC004*       BBON_DAT_VON_UNTST     TIMESTAMP NOT NULL,
CCC004*       BBON_GRUND             CHAR(04)  NOT NULL,
CCC004*       BBON_PROV_UNTST        DECIMAL(05, 2) NOT NULL,
CCC004*       BBON_BETRAG            DECIMAL(11, 2) NOT NULL,
CCC004*       BBON_MONATE            SMALLINT  NOT NULL,
CCC004*       BBON_LAST_UPDATE       TIMESTAMP,
CCC004*       BBON_BES_GRUND         SMALLINT  NOT NULL,
CCC004*       BBON_CREATOR           CHAR(08)  NOT NULL
CCC004*    )
CCC003*    END-EXEC.

      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0085                           *
      ******************************************************************
       01  VIVB0086-GESAMT.
           05  NI-VIVB0086-TAB.
               10 NI-VIVB0086-ELE     PIC S9(04) BINARY OCCURS 15 TIMES.
           05  NI-VIVB0086            REDEFINES   NI-VIVB0086-TAB.
               10 NI-BBON-BETR-NR          PIC S9(04)  COMP.
               10 NI-BBON-UNTST-NR         PIC S9(04)  COMP.
               10 NI-BBON-DAT-ANSPRUCH     PIC S9(04)  COMP.
               10 NI-BBON-DAT-VON          PIC S9(04)  COMP.
               10 NI-BBON-DAT-BIS          PIC S9(04)  COMP.
               10 NI-BBON-DAT-VON-BETR     PIC S9(04)  COMP.
               10 NI-BBON-PROV-BETR        PIC S9(04)  COMP.
               10 NI-BBON-DAT-VON-UNTST    PIC S9(04)  COMP.
               10 NI-BBON-GRUND            PIC S9(04)  COMP.
               10 NI-BBON-PROV-UNTST       PIC S9(04)  COMP.
               10 NI-BBON-BETRAG           PIC S9(04)  COMP.
               10 NI-BBON-MONATE           PIC S9(04)  COMP.
               10 NI-BBON-LAST-UPDATE      PIC S9(04)  COMP.
               10 NI-BBON-BES-GRUND        PIC S9(04)  COMP.
               10 NI-BBON-CREATOR          PIC S9(04)  COMP.

           05 DCL-VIVB0086.
               10 BBON-BETR-NR             PIC S9(9)     USAGE COMP.
               10 BBON-UNTST-NR            PIC S9(9)     USAGE COMP.
               10 BBON-DAT-ANSPRUCH        PIC X(010).
               10 BBON-DAT-VON             PIC X(026).
               10 BBON-DAT-BIS             PIC X(010).
               10 BBON-DAT-VON-BETR        PIC X(026).
               10 BBON-PROV-BETR           PIC S9(03)V99 USAGE COMP-3.
               10 BBON-DAT-VON-UNTST       PIC X(026).
               10 BBON-GRUND               PIC X(004).
               10 BBON-PROV-UNTST          PIC S9(03)V99 USAGE COMP-3.
               10 BBON-BETRAG              PIC S9(9)V9(2) USAGE COMP-3.
               10 BBON-MONATE              PIC S9(004)   COMP.
               10 BBON-LAST-UPDATE         PIC X(026).
               10 BBON-BES-GRUND           PIC S9(004)   COMP.
               10 BBON-CREATOR             PIC X(008).

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
