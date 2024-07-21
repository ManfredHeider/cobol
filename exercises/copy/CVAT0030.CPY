      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0030.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\CVAT0030.CPv  $
      *
      *   Rev 2.2   Mar 31 2000 18:22:14   T13AE34
      *AAN00249-SF21 Änderung
      *
      *   Rev 2.0   Mar 16 1999 09:17:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0030 TABLE
CCC004*    ( VERTRIEB_GES                   SMALLINT NOT NULL,
CCC004*      VGES_GUELTIG_AB                TIMESTAMP NOT NULL,
CCC004*      VGES_GUELTIG_BIS               DATE NOT NULL,
CCC004*      VGES_VWF                       INTEGER NOT NULL,
CCC004*      VGES_VTF                       INTEGER NOT NULL,
CCC004*      KTY_VERTRIEB_GES               SMALLINT NOT NULL,
CCC004*      KNR_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KTY_WAEHRUNG                   SMALLINT NOT NULL,
CCC004*      KNR_AUSL                       SMALLINT NOT NULL,
CCC004*      KTY_AUSL                       SMALLINT NOT NULL,
CCC004*      VGES_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      VGES_KENNUNG                   CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.VIAT0030                     *
      ******************************************************************
       01  VIAT0030-GESAMT.
           05  DCL-VIAT0030.
           10 VERTRIEB-GES         PIC S9(4) USAGE COMP.
           10 VGES-GUELTIG-AB      PIC X(26).
           10 VGES-GUELTIG-BIS     PIC X(10).
           10 VGES-VWF             PIC S9(9) USAGE COMP.
           10 VGES-VTF             PIC S9(9) USAGE COMP.
           10 KTY-VERTRIEB-GES     PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KNR-AUSL             PIC S9(4) USAGE COMP.
           10 KTY-AUSL             PIC S9(4) USAGE COMP.
           10 VGES-LAST-UPDATE     PIC X(26).
           10 VGES-KENNUNG         PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
