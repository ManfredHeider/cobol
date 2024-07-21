      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.01.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT9995.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVAT9995.CPv  $
      *
      *   Rev 2.1   Feb 02 2006 12:13:04   t13ex27
      *AKI00543: Internetadressen - Umkreissuche
      *
      *   Rev 2.0   Jan 24 2006 16:27:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE VIAT9995
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT9995 TABLE
CCC004*    ( VON_KNR_AUSL                   SMALLINT NOT NULL,
CCC004*      VON_KTY_AUSL                   SMALLINT NOT NULL,
CCC004*      VON_PLZ                        CHAR(9) NOT NULL,
CCC004*      BIS_KNR_AUSL                   SMALLINT NOT NULL,
CCC004*      BIS_KTY_AUSL                   SMALLINT NOT NULL,
CCC004*      BIS_PLZ                        CHAR(9) NOT NULL,
CCC004*      KM                             DECIMAL(8, 3)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VIAT9995
      ******************************************************************
       01  VIAT9995-GESAMT.
         05 NI-VIAT9995-TAB.
           10 NI-VIAT9995-ELE PIC S9(4) COMP OCCURS 7 TIMES.
         05 NI-VIAT9995 REDEFINES NI-VIAT9995-TAB.
           10 NI-VON-KNR-AUSL    PIC S9(4) COMP.
           10 NI-VON-KTY-AUSL    PIC S9(4) COMP.
           10 NI-VON-PLZ         PIC S9(4) COMP.
           10 NI-BIS-KNR-AUSL    PIC S9(4) COMP.
           10 NI-BIS-KTY-AUSL    PIC S9(4) COMP.
           10 NI-BIS-PLZ         PIC S9(4) COMP.
           10 NI-KM              PIC S9(4) COMP.
         05 DCL-VIAT9995.
           10 VON-KNR-AUSL         PIC S9(4) COMP.
           10 VON-KTY-AUSL         PIC S9(4) COMP.
           10 VON-PLZ              PIC X(9).
           10 BIS-KNR-AUSL         PIC S9(4) COMP.
           10 BIS-KTY-AUSL         PIC S9(4) COMP.
           10 BIS-PLZ              PIC X(9).
           10 KM                   PIC S9(5)V9(3) COMP-3.
      ******************************************************************
