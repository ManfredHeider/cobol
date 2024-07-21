      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14/04/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0540.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0540.CPv  $
      *
      *   Rev 2.2   Oct 06 2020 16:39:08   u130684
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.1   Jun 23 2020 12:04:06   t13ex27
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.0   Apr 14 2020 15:59:16   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G90CVERTRAG TABLE
CCC004*    ( G90CVVERTRAGSNR                CHAR(15) NOT NULL,
CCC004*      G90CVMANDANT                   INTEGER NOT NULL,
CCC004*      G90CVAENDKZ                    CHAR(2) NOT NULL,
CCC004*      G90CVGESID                     SMALLINT NOT NULL,
CCC004*      G90CVABSCLVBNR                 INTEGER NOT NULL,
CCC004*      G90CVVEROEFF                   CHAR(8) NOT NULL,
CCC004*      G90CVSCHLIESS                  CHAR(8) NOT NULL,
CCC004*      G90CVFINBETR                   DECIMAL(14, 2) NOT NULL,
CCC004*      G90CVZUSAGE                    CHAR(8) NOT NULL,
CCC004*      G90CVAUSZAHL                   CHAR(8) NOT NULL,
CCC004*      G90CVPRODUKTGEB                CHAR(30) NOT NULL,
CCC004*      G90CVPRODUKTART                CHAR(1) NOT NULL,
CCC004*      G90CVAKTSALDO                  DECIMAL(14, 2) NOT NULL,
CCC004*      G90CVSOLLZINS                  DECIMAL(6, 4) NOT NULL,
CCC004*      G90CVRATE                      DECIMAL(14, 2) NOT NULL,
CCC004*      G90CVFAELLIG                   CHAR(8) NOT NULL,
CCC004*      G90CVRESTLAUF                  DECIMAL(3, 0) NOT NULL,
CCC004*      G90CVDELETEKZ                  CHAR(1) NOT NULL,
CCC004*      G90CVIBAN                      CHAR(34) NOT NULL,
CCC004*      G90CVBIC                       CHAR(11) NOT NULL,
CCC004*      G90CVMANDAT                    CHAR(35) NOT NULL,
CCC004*      G90CVKTOINHNNAME               CHAR(30) NOT NULL,
CCC004*      G90CVKTOINHVNAME               CHAR(30) NOT NULL,
CCC004*      G90CVIBANKREDIT                CHAR(34) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G90CVERTRAG                 *
      ******************************************************************
       01  G90CVERTRAG-GESAMT.
        05 NI-G90CVERTRAG-TAB.
           10 NI-G90CVERTRAG-ELE PIC S9(04) COMP OCCURS 25 TIMES.
        05 NI-G90CVERTRAG REDEFINES NI-G90CVERTRAG-TAB.
           10 NI-G90CVVERTRAGSNR    PIC S9(4) COMP.
           10 NI-G90CVMANDANT       PIC S9(4) COMP.
           10 NI-G90CVAENDKZ        PIC S9(4) COMP.
           10 NI-G90CVGESID         PIC S9(4) COMP.
           10 NI-G90CVABSCLVBNR     PIC S9(4) COMP.
           10 NI-G90CVVEROEFF       PIC S9(4) COMP.
           10 NI-G90CVSCHLIESS      PIC S9(4) COMP.
           10 NI-G90CVFINBETR       PIC S9(4) COMP.
           10 NI-G90CVZUSAGE        PIC S9(4) COMP.
           10 NI-G90CVAUSZAHL       PIC S9(4) COMP.
           10 NI-G90CVPRODUKTGEB    PIC S9(4) COMP.
           10 NI-G90CVPRODUKTART    PIC S9(4) COMP.
           10 NI-G90CVAKTSALDO      PIC S9(4) COMP.
           10 NI-G90CVSOLLZINS      PIC S9(4) COMP.
           10 NI-G90CVRATE          PIC S9(4) COMP.
           10 NI-G90CVFAELLIG       PIC S9(4) COMP.
           10 NI-G90CVRESTLAUF      PIC S9(4) COMP.
           10 NI-G90CVDELETEKZ      PIC S9(4) COMP.
           10 NI-G90CVIBAN          PIC S9(4) COMP.
           10 NI-G90CVBIC           PIC S9(4) COMP.
           10 NI-G90CVMANDAT        PIC S9(4) COMP.
           10 NI-G90CVKTOINHNNAME   PIC S9(4) COMP.
           10 NI-G90CVKTOINHVNAME   PIC S9(4) COMP.
           10 NI-G90CVIBANKREDIT    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-G90CVERTRAG.
           10 G90CVVERTRAGSNR      PIC X(15).
           10 G90CVMANDANT         PIC S9(9) USAGE COMP.
           10 G90CVAENDKZ          PIC X(2).
           10 G90CVGESID           PIC S9(4) USAGE COMP.
           10 G90CVABSCLVBNR       PIC S9(9) USAGE COMP.
           10 G90CVVEROEFF         PIC X(8).
           10 G90CVSCHLIESS        PIC X(8).
           10 G90CVFINBETR         PIC S9(12)V9(2) USAGE COMP-3.
           10 G90CVZUSAGE          PIC X(8).
           10 G90CVAUSZAHL         PIC X(8).
           10 G90CVPRODUKTGEB      PIC X(30).
           10 G90CVPRODUKTART      PIC X(1).
           10 G90CVAKTSALDO        PIC S9(12)V9(2) USAGE COMP-3.
           10 G90CVSOLLZINS        PIC S9(2)V9(4) USAGE COMP-3.
           10 G90CVRATE            PIC S9(12)V9(2) USAGE COMP-3.
           10 G90CVFAELLIG         PIC X(8).
           10 G90CVRESTLAUF        PIC S9(3)V USAGE COMP-3.
           10 G90CVDELETEKZ        PIC X(1).
           10 G90CVIBAN            PIC X(34).
           10 G90CVBIC             PIC X(11).
           10 G90CVMANDAT          PIC X(35).
           10 G90CVKTOINHNNAME     PIC X(30).
           10 G90CVKTOINHVNAME     PIC X(30).
           10 G90CVIBANKREDIT      PIC X(34).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************


