      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0038.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTAT0038.CPv  $
      *
      *   Rev 2.2   Nov 13 2007 10:24:48   t13ex51
      *AKL00664: Allfinanz: Supporttabelle KTY_KNR_SCHLUESSEL 
      *
      *   Rev 2.1   Sep 03 2007 10:05:00   t13ex14
      *AKL00664: Allfinanz: Supporttabelle KTY_KNR_SCHLUESSEL 
      *
      *   Rev 2.0   Sep 03 2007 09:22:12   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE TBAT0038
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0038 TABLE
CCC004*    ( KTY                            SMALLINT NOT NULL,
CCC004*      KNR                            SMALLINT NOT NULL,
CCC004*      BEZEICHNUNG                    CHAR(100) NOT NULL,
CCC004*      BEZEICHNUNG_KURZ               CHAR(20)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  TBAT0038
      ******************************************************************
       01  TBAT0038-GESAMT.
         05 NI-TBAT0038-TAB.
           10 NI-TBAT0038-ELE PIC S9(4) COMP OCCURS 04 TIMES.
         05 NI-TBAT0038 REDEFINES NI-TBAT0038-TAB.
           10 NI-KTY                PIC S9(4) COMP.
           10 NI-KNR                PIC S9(4) COMP.
           10 NI-BEZEICHNUNG        PIC S9(4) COMP.
           10 NI-BEZEICHNUNG-KURZ   PIC S9(4) COMP.
         05 DCL-TBAT0038.
           10 KTY                  PIC S9(4) COMP.
           10 KNR                  PIC S9(4) COMP.
           10 BEZEICHNUNG          PIC X(100).
           10 BEZEICHNUNG-KURZ     PIC X(20).
      ******************************************************************
