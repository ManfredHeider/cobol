      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.04.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0439.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0439.CPv  $
      *
      *   Rev 2.1   28 Apr 2009 11:07:10   t13ex27
      *AKI00812: DB-Änderung für Rel. 4.52
      *
      *   Rev 2.0   07 Apr 2009 16:42:26   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE LVARTTARIF
      ******************************************************************
CCC005*    EXEC SQL DECLARE LVARTTARIF TABLE
CCC004*    ( LVTLVART                       SMALLINT NOT NULL,
CCC004*      LVTTARIF                       CHAR(15) NOT NULL,
CCC004*      LVTPRODUKTID                   SMALLINT NOT NULL,
CCC004*      LVTSTVD0067_KTY                SMALLINT,
CCC004*      LVTSTVD0067_KNR                DECIMAL(15, 0)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  LVARTTARIF
      ******************************************************************
       01  LVARTTARIF-GESAMT.
         05 NI-LVARTTARIF-TAB.
           10 NI-LVARTTARIF-ELE PIC S9(4) COMP OCCURS 5 TIMES.
         05 NI-LVARTTARIF REDEFINES NI-LVARTTARIF-TAB.
           10 NI-LVTLVART          PIC S9(4) COMP.
           10 NI-LVTTARIF          PIC S9(4) COMP.
           10 NI-LVTPRODUKTID      PIC S9(4) COMP.
           10 NI-LVTSTVD0067-KTY   PIC S9(4) COMP.
           10 NI-LVTSTVD0067-KNR   PIC S9(4) COMP.
         05 DCL-LVARTTARIF.
           10 LVTLVART             PIC S9(4) USAGE COMP.
           10 LVTTARIF             PIC X(15).
           10 LVTPRODUKTID         PIC S9(4) USAGE COMP.
           10 LVTSTVD0067-KTY      PIC S9(4) USAGE COMP.
           10 LVTSTVD0067-KNR      PIC S9(15)V USAGE COMP-3.
      ******************************************************************