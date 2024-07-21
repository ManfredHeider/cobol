      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0085.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/MF/CPY/CVVB0085.CPv  $
      *
      *   Rev 2.4   Jul 11 2002 11:00:54   t13ae30
      *AVB00587 - VB-EMAIL / VB-HOMEPAGE
      *
      *   Rev 2.3   Jul 11 2002 10:53:50   t13ae30
      *AVB00587 - VB-EMAIL / VB-HOMEPAGE
      *
      *   Rev 2.2   Jul 10 2002 14:32:48   t13ae30
      *AVB00587 - VB-EMAIL / VB-HOMEPAGE
      *
      *   Rev 2.0   20 Feb 2001 13:22:24   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0085)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0085))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0085 TABLE
CCC004*    (
CCC004*       KTY_ANW_NR             SMALLINT  NOT NULL WITH DEFAULT,
CCC004*       KNR_ANW_NR             SMALLINT  NOT NULL             ,
CCC004*       KTY_AUSN_TYP           SMALLINT  NOT NULL WITH DEFAULT,
CCC004*       KNR_AUSN_TYP           SMALLINT  NOT NULL             ,
CCC004*       AUSN_LAST_UPDATE       TIMESTAMP NOT NULL WITH DEFAULT,
CCC004*       AUSN_STRING_ALT        CHAR (70) NOT NULL,
CCC004*       AUSN_STRING_NEU        CHAR (70)
CCC004*    )
CCC003*    END-EXEC.

      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0085                           *
      ******************************************************************
       01  VIVB0085-GESAMT.
           05  NI-VIVB0085-TAB.
               10 NI-VIVB0085-ELE      PIC S9(04) BINARY OCCURS 7 TIMES.
           05  NI-VIVB0085            REDEFINES   NI-VIVB0085-TAB.
               10 NI-KTY-ANW-NR             PIC S9(04)  COMP.
               10 NI-KNR-ANW-NR             PIC S9(04)  COMP.
               10 NI-KTY-AUSN-TYP           PIC S9(04)  COMP.
               10 NI-KNR-AUSN-TYP           PIC S9(04)  COMP.
               10 NI-AUSN-LAST-UPDATE       PIC S9(04)  COMP.
               10 NI-AUSN-STRING-ALT        PIC S9(04)  COMP.
               10 NI-AUSN-STRING-NEU        PIC S9(04)  COMP.
           05 DCL-VIVB0085.
               10 KTY-ANW-NR                PIC S9(04)  COMP.
               10 KNR-ANW-NR                PIC S9(04)  COMP.
               10 KTY-AUSN-TYP              PIC S9(04)  COMP.
               10 KNR-AUSN-TYP              PIC S9(04)  COMP.
               10 AUSN-LAST-UPDATE          PIC X(26).
               10 AUSN-STRING-ALT           PIC X(70).
               10 AUSN-STRING-NEU           PIC X(70).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
