      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0419.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0419.CPv  $
      *
      *   Rev 2.2   02 Dec 2008 14:28:34   t13ex27
      *AKI00772: Reisegepäckversicherung-DB-Copys und Mapping
      *
      *   Rev 2.1   17 Oct 2007 11:07:20   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-DCLGENs und DB-Änderung
      *
      *   Rev 2.0   27 Sep 2007 12:01:28   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE FKVERTRAG
      ******************************************************************
CCC005*    EXEC SQL DECLARE FKVERTRAG TABLE
CCC004*    ( FKVVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      FKVVETLFDNR                    SMALLINT NOT NULL,
CCC004*      FKVEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      FKVTARIFPRODUKT                CHAR(50) NOT NULL,
CCC004*      FKVBEDINGUNG                   CHAR(20) NOT NULL,
CCC004*      FKVZUSATZINFO                  CHAR(100) NOT NULL,
CCC004*      FKVMAILINGAKTION               CHAR(70) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      FKVGESELLSCH_KTY               SMALLINT NOT NULL,
CCC004*      FKVGESELLSCH_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      FKVVERSNUMMER                  CHAR(40),
CCC004*      FKVUMSTELLUNGDAT               DATE,
CCC004*      FKVKUENDIGUNGKZ                CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  FKVERTRAG
      ******************************************************************
       01  FKVERTRAG-GESAMT.
         05 NI-FKVERTRAG-TAB.
           10 NI-FKVERTRAG-ELE PIC S9(4) COMP OCCURS 13 TIMES.
         05 NI-FKVERTRAG REDEFINES NI-FKVERTRAG-TAB.
           10 NI-FKVVERTRAGSID        PIC S9(4) COMP.
           10 NI-FKVVETLFDNR          PIC S9(4) COMP.
           10 NI-FKVEINZVTGHOSTID     PIC S9(4) COMP.
           10 NI-FKVTARIFPRODUKT      PIC S9(4) COMP.
           10 NI-FKVBEDINGUNG         PIC S9(4) COMP.
           10 NI-FKVZUSATZINFO        PIC S9(4) COMP.
           10 NI-FKVMAILINGAKTION     PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
           10 NI-FKVGESELLSCH-KTY     PIC S9(4) COMP.
           10 NI-FKVGESELLSCH-KNR     PIC S9(4) COMP.
           10 NI-FKVVERSNUMMER        PIC S9(4) COMP.
           10 NI-FKVUMSTELLUNGDAT     PIC S9(4) COMP.
           10 NI-FKVKUENDIGUNGKZ      PIC S9(4) COMP.
         05 DCL-FKVERTRAG.
           10 FKVVERTRAGSID        PIC X(13).
           10 FKVVETLFDNR          PIC S9(4) COMP.
           10 FKVEINZVTGHOSTID     PIC X(15).
           10 FKVTARIFPRODUKT      PIC X(50).
           10 FKVBEDINGUNG         PIC X(20).
           10 FKVZUSATZINFO        PIC X(100).
           10 FKVMAILINGAKTION     PIC X(70).
           10 LETZTEAENDERUNG      PIC X(26).
           10 FKVGESELLSCH-KTY     PIC S9(4) COMP.
           10 FKVGESELLSCH-KNR     PIC S9(15)V COMP-3.
           10 FKVVERSNUMMER        PIC X(40).
           10 FKVUMSTELLUNGDAT     PIC X(10).
           10 FKVKUENDIGUNGKZ      PIC X(1).
      ******************************************************************
