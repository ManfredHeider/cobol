      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.01.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTOK0006.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTOK0006.CPv  $
      *
      *   Rev 2.2   06 Mar 2008 14:49:14   t13ex27
      *AKL00665: Allfinanz; Spartenermittlung FK, PK
      *
      *   Rev 2.1   Jan 24 2007 08:24:42   t13ex14
      *AKI00556: Datenabgleich HOST-DB <-> Online-DB
      *
      *   Rev 2.0   Jan 23 2007 08:39:14   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE SPARTE-OKI-VUE
      ******************************************************************
CCC005*    EXEC SQL DECLARE SPARTE_OKI_VUE TABLE
CCC004*    ( SOVPRODUKTID                   SMALLINT NOT NULL,
CCC004*      SOVBEREICH                     CHAR(1) NOT NULL,
CCC004*      SOVPRODUKTBEZ                  CHAR(30) NOT NULL,
CCC004*      SOVSPARTEVUE                   SMALLINT NOT NULL,
CCC004*      SOVPARTGESNAME                 CHAR(25)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  SPARTE_OKI_VUE
      ******************************************************************
       01  SPARTE-OKI-VUE-GESAMT.
         05 NI-SPARTE-OKI-VUE-TAB.
           10 NI-SPARTE-OKI-VUE-ELE PIC S9(4) COMP OCCURS 5 TIMES.
         05 NI-SPARTE-OKI-VUE REDEFINES NI-SPARTE-OKI-VUE-TAB.
           10 NI-SOVPRODUKTID      PIC S9(4) COMP.
           10 NI-SOVBEREICH        PIC S9(4) COMP.
           10 NI-SOVPRODUKTBEZ     PIC S9(4) COMP.
           10 NI-SOVSPARTEVUE      PIC S9(4) COMP.
           10 NI-SOVPARTGESNAME    PIC S9(4) COMP.
         05 DCL-SPARTE-OKI-VUE.
           10 SOVPRODUKTID         PIC S9(4) COMP.
           10 SOVBEREICH           PIC X(1).
           10 SOVPRODUKTBEZ        PIC X(30).
           10 SOVSPARTEVUE         PIC S9(4) COMP.
           10 SOVPARTGESNAME       PIC X(25).
      ******************************************************************
