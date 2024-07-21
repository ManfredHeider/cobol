      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0039.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTAT0039.CPv  $
      *
      *   Rev 2.2   16 Sep 2008 14:17:54   t13ex27
      *AKI00731: Bestandsdatenlieferung Kfz Tarifierung-DB-Änderung
      *
      *   Rev 2.1   Oct 03 2007 12:18:18   t13ex51
      *AKL00664: Allfinanz: Supporttabelle KTY_KNR_SCHLUESSEL 
      *
      *   Rev 2.0   Oct 01 2007 15:17:20   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE TBAT0039
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0039 TABLE
CCC004*    ( KTY                            SMALLINT NOT NULL,
CCC004*      KNR                            DECIMAL(15, 0) NOT NULL,
CCC004*      BEZEICHNUNG                    CHAR(100) NOT NULL,
CCC004*      BEZEICHNUNG_KURZ               CHAR(20),     
CCC004*      KNR_ALPHA                      CHAR(15)      
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  TBAT0039
      ******************************************************************
       01  TBAT0039-GESAMT.
         05 NI-TBAT0039-TAB.
           10 NI-TBAT0039-ELE PIC S9(4) COMP OCCURS 05 TIMES.
         05 NI-TBAT0039 REDEFINES NI-TBAT0039-TAB.
           10 NI-KTY                PIC S9(4) COMP.
           10 NI-KNR                PIC S9(4) COMP.
           10 NI-BEZEICHNUNG        PIC S9(4) COMP.
           10 NI-BEZEICHNUNG-KURZ   PIC S9(4) COMP.
           10 NI-KNR-ALPHA          PIC S9(4) COMP.
         05 DCL-TBAT0039.
           10 KTY                  PIC S9(4) COMP.
           10 KNR                  PIC S9(15)V USAGE COMP-3.
           10 BEZEICHNUNG          PIC X(100).
           10 BEZEICHNUNG-KURZ     PIC X(20).
           10 KNR-ALPHA            PIC X(15).    
      ******************************************************************

