      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0441.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0441.CPv  $
      *
      *   Rev 2.1   Sep 11 2009 07:56:32   t13ex14
      *AKI00843: DB-Änderungen für Rel. 4.60 - Badenia
      *
      *   Rev 2.0   Aug 14 2009 10:44:00   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE BAUSPJAHRWERTRST TABLE
CCC004*    ( BJRVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      BJRGUTHPRODLFDNR               SMALLINT NOT NULL,
CCC004*      BJRJAHRZULAGE                  SMALLINT,
CCC004*      BJRJHRZULAGGUT                 DECIMAL(9, 2),
CCC004*      BJRDATZULAGGUT                 DATE,
CCC004*      LETZTEAENDERUNG                TIMESTAMP
CCC004*    ) 
CCC003*    END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BAUSPJAHRWERTRST                   *
      ******************************************************************
       01  BAUSPJAHRWERTRST-GESAMT.
        05  NI-BAUSPJAHRWERTRST-TAB.
           10 NI-BAUSPJAHRWERTRST-ELE PIC S9(04) COMP OCCURS 6 TIMES.
        05  NI-BAUSPJAHRWERTRST  REDEFINES  NI-BAUSPJAHRWERTRST-TAB.
           10 NI-BJRVERTRAGSID           PIC S9(4) COMP.
           10 NI-BJRGUTHPRODLFDNR        PIC S9(4) COMP.
           10 NI-BJRJAHRZULAGE           PIC S9(4) COMP.
           10 NI-BJRJHRZULAGGUT          PIC S9(4) COMP.
           10 NI-BJRDATZULAGGUT          PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG         PIC S9(4) COMP.
        05  DCL-BAUSPJAHRWERTRST.
           10 BJRVERTRAGSID        PIC X(13).
           10 BJRGUTHPRODLFDNR     PIC S9(4) USAGE COMP.
           10 BJRJAHRZULAGE        PIC S9(4) USAGE COMP.
           10 BJRJHRZULAGGUT       PIC S9(7)V9(2) USAGE COMP-3.
           10 BJRDATZULAGGUT       PIC X(10).
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************
