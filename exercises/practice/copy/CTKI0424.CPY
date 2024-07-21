      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0424.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0424.CPv  $
      *
      *   Rev 2.3   Sep 07 2017 07:40:02   t13ae31
      *AKI01530: G10 SUH SA614 Beruf auf 48 St. OZP
      *
      *   Rev 2.2   Sep 19 2016 07:34:04   t13ae31
      *AKI01458: SUH Namensvorwort bei Versicherten Personen OZP
      *
      *   Rev 2.0   27 Sep 2007 12:02:18   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE UNFVERSPERSON
      ******************************************************************
CCC005*    EXEC SQL DECLARE UNFVERSPERSON TABLE
CCC004*    ( UVPVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      UVPVETLFDNR                    SMALLINT NOT NULL,
CCC004*      UVPPUFVPLFDNR                  SMALLINT NOT NULL,
CCC004*      UVPEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      UVPVERSPERSONLFD               SMALLINT NOT NULL,
CCC004*      UVPVPNAME                      CHAR(25) NOT NULL,
CCC004*      UVPVPBERUF                     CHAR(21) NOT NULL,
CCC004*      UVPVPGEBURTSDAT                DATE,
CCC004*      UVPVPGEFAHRENGRP               CHAR(1) NOT NULL,
CCC004*      UVPVPBEITRAG                   DECIMAL(9, 2) NOT NULL,
CCC004*      UVPTARIFART                    CHAR(30) NOT NULL,
CCC004*      UVPDYNAMIKTEXT                 CHAR(5) NOT NULL,
CCC004*      UVPDYNAMIKPROZ                 DECIMAL(4, 2) NOT NULL,
CCC004*      UVPDYNABLJAHR                  DECIMAL(8, 0) NOT NULL,
CCC004*      UVPREGULIERUNGKZ               CHAR(2) NOT NULL,
CCC004*      UVPRUECKGEWSUMKZ               CHAR(2) NOT NULL,
CCC004*      UVPPRRUECKGEWKZ                CHAR(2) NOT NULL,
CCC004*      UVPFREIZDECKKZ                 CHAR(2) NOT NULL,
CCC004*      UVPANZVERPERGRP                INTEGER NOT NULL,
CCC004*      UVPVPGESCHL_KTY                SMALLINT NOT NULL,
CCC004*      UVPVPGESCHL_KNR                DECIMAL(15, 0) NOT NULL,
CCC004*      UVPPRFREISTKIZKZ               CHAR(2) NOT NULL,
CCC004*      UVPPRAEMICHEKIZ                DECIMAL(9, 2) NOT NULL,
CCC004*      UVPVPPARTNHOSTID               CHAR(17) NOT NULL,
CCC004*      UVPDIFFDECKKZ                  CHAR(2) NOT NULL,
CCC004*      UVPZIELGRUPPE                  CHAR(30) NOT NULL,
CCC004*      UVPGESELLSCHAFT                CHAR(50) NOT NULL,
CCC004*      UVPVERSNUMMER                  CHAR(40) NOT NULL,
CCC004*      UVPUMSTELLUNGDAT               DATE,
CCC004*      UVPKUENDIGUNGKZ                CHAR(2) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      UVPNACHNAME                    CHAR(40),
CCC004*      UVPVORNAME                     CHAR(30),
CCC004*      UVPBERUF_KTY                   SMALLINT,
CCC004*      UVPBERUF_KNR                   DECIMAL(15, 0)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE UNFVERSPERSON               *
      ******************************************************************
       01  UNFVERSPERSON-GESAMT.
        05 NI-UNFVERSPERSON-TAB.
           10 NI-UNFVERSPERSON-ELE PIC S9(04) COMP OCCURS 35 TIMES.
        05 NI-UNFVERSPERSON REDEFINES NI-UNFVERSPERSON-TAB.
           10 NI-UVPVERTRAGSID      PIC S9(4) COMP.
           10 NI-UVPVETLFDNR        PIC S9(4) COMP.
           10 NI-UVPPUFVPLFDNR      PIC S9(4) COMP.
           10 NI-UVPEINZVTGHOSTID   PIC S9(4) COMP.
           10 NI-UVPVERSPERSONLFD   PIC S9(4) COMP.
           10 NI-UVPVPNAME          PIC S9(4) COMP.
           10 NI-UVPVPBERUF         PIC S9(4) COMP.
           10 NI-UVPVPGEBURTSDAT    PIC S9(4) COMP.
           10 NI-UVPVPGEFAHRENGRP   PIC S9(4) COMP.
           10 NI-UVPVPBEITRAG       PIC S9(4) COMP.
           10 NI-UVPTARIFART        PIC S9(4) COMP.
           10 NI-UVPDYNAMIKTEXT     PIC S9(4) COMP.
           10 NI-UVPDYNAMIKPROZ     PIC S9(4) COMP.
           10 NI-UVPDYNABLJAHR      PIC S9(4) COMP.
           10 NI-UVPREGULIERUNGKZ   PIC S9(4) COMP.
           10 NI-UVPRUECKGEWSUMKZ   PIC S9(4) COMP.
           10 NI-UVPPRRUECKGEWKZ    PIC S9(4) COMP.
           10 NI-UVPFREIZDECKKZ     PIC S9(4) COMP.
           10 NI-UVPANZVERPERGRP    PIC S9(4) COMP.
           10 NI-UVPVPGESCHL-KTY    PIC S9(4) COMP.
           10 NI-UVPVPGESCHL-KNR    PIC S9(4) COMP.
           10 NI-UVPPRFREISTKIZKZ   PIC S9(4) COMP.
           10 NI-UVPPRAEMICHEKIZ    PIC S9(4) COMP.
           10 NI-UVPVPPARTNHOSTID   PIC S9(4) COMP.
           10 NI-UVPDIFFDECKKZ      PIC S9(4) COMP.
           10 NI-UVPZIELGRUPPE      PIC S9(4) COMP.
           10 NI-UVPGESELLSCHAFT    PIC S9(4) COMP.
           10 NI-UVPVERSNUMMER      PIC S9(4) COMP.
           10 NI-UVPUMSTELLUNGDAT   PIC S9(4) COMP.
           10 NI-UVPKUENDIGUNGKZ    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-UVPNACHNAME        PIC S9(4) COMP.
           10 NI-UVPVORNAME         PIC S9(4) COMP.
           10 NI-UVPBERUF-KTY       PIC S9(4) COMP.
           10 NI-UVPBERUF-KNR       PIC S9(4) COMP.
        05 DCL-UNFVERSPERSON.
           10 UVPVERTRAGSID        PIC X(13).
           10 UVPVETLFDNR          PIC S9(4) USAGE COMP.
           10 UVPPUFVPLFDNR        PIC S9(4) USAGE COMP.
           10 UVPEINZVTGHOSTID     PIC X(15).
           10 UVPVERSPERSONLFD     PIC S9(4) USAGE COMP.
           10 UVPVPNAME            PIC X(25).
           10 UVPVPBERUF           PIC X(21).
           10 UVPVPGEBURTSDAT      PIC X(10).
           10 UVPVPGEFAHRENGRP     PIC X(1).
           10 UVPVPBEITRAG         PIC S9(7)V9(2) USAGE COMP-3.
           10 UVPTARIFART          PIC X(30).
           10 UVPDYNAMIKTEXT       PIC X(5).
           10 UVPDYNAMIKPROZ       PIC S9(2)V9(2) USAGE COMP-3.
           10 UVPDYNABLJAHR        PIC S9(8)V USAGE COMP-3.
           10 UVPREGULIERUNGKZ     PIC X(2).
           10 UVPRUECKGEWSUMKZ     PIC X(2).
           10 UVPPRRUECKGEWKZ      PIC X(2).
           10 UVPFREIZDECKKZ       PIC X(2).
           10 UVPANZVERPERGRP      PIC S9(9) USAGE COMP.
           10 UVPVPGESCHL-KTY      PIC S9(4) USAGE COMP.
           10 UVPVPGESCHL-KNR      PIC S9(15)V USAGE COMP-3.
           10 UVPPRFREISTKIZKZ     PIC X(2).
           10 UVPPRAEMICHEKIZ      PIC S9(7)V9(2) USAGE COMP-3.
           10 UVPVPPARTNHOSTID     PIC X(17).
           10 UVPDIFFDECKKZ        PIC X(2).
           10 UVPZIELGRUPPE        PIC X(30).
           10 UVPGESELLSCHAFT      PIC X(50).
           10 UVPVERSNUMMER        PIC X(40).
           10 UVPUMSTELLUNGDAT     PIC X(10).
           10 UVPKUENDIGUNGKZ      PIC X(2).
           10 LETZTEAENDERUNG      PIC X(26).
           10 UVPNACHNAME          PIC X(40).
           10 UVPVORNAME           PIC X(30).
           10 UVPBERUF-KTY         PIC S9(4) USAGE COMP.
           10 UVPBERUF-KNR         PIC S9(15)V USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************

