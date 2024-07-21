      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08/01/15
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0473.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0473.CPv  $
      *
      *   Rev 2.2   Mar 18 2015 14:49:20   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP 
      *für TCKI0473
      *
      *   Rev 2.1   Jan 13 2015 13:24:36   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - Standard-OZP
      *für TCKI0473
      *
      *   Rev 2.0   Jan 8 2015 17:10:20   T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE PHOTOVOLTAIK TABLE
CCC004*    ( PVVERTRAGSID                   CHAR(13) NOT NULL,
CCC004*      PVVETLFDNR                     SMALLINT NOT NULL,
CCC004*      PVEINZVTGHOSTID                CHAR(15) NOT NULL,
CCC004*      PVBEDINGUNG_KTY                SMALLINT NOT NULL,
CCC004*      PVBEDINGUNG_KNR                DECIMAL(15, 0) NOT NULL,
CCC004*      PVMAILING_KTY                  SMALLINT NOT NULL,
CCC004*      PVMAILING_KNR                  DECIMAL(15, 0) NOT NULL,
CCC004*      PVZUSATZINFO_KTY               SMALLINT NOT NULL,
CCC004*      PVZUSATZINFO_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      PVINFOQUELLE_KTY               SMALLINT NOT NULL,
CCC004*      PVINFOQUELLE_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      PVBAUJAHR                      CHAR(4),
CCC004*      PVLEISTUNG                     DECIMAL(6, 2),
CCC004*      PVBETREIBER                    CHAR(35),
CCC004*      PVBATTERIEKZ                   CHAR(2),
CCC004*      PVGEBART_KTY                   SMALLINT NOT NULL,
CCC004*      PVGEBART_KNR                   DECIMAL(15, 0) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE PHOTOVOLTAIK                *
      ******************************************************************
       01  PHOTOVOLTAIK-GESAMT.
        05 NI-PHOTOVOLTAIK-TAB.
           10 NI-PHOTOVOLTAIK-ELE PIC S9(04) COMP OCCURS 18 TIMES.
        05 NI-PHOTOVOLTAIK REDEFINES NI-PHOTOVOLTAIK-TAB.
           10 NI-PVVERTRAGSID       PIC S9(4) COMP.
           10 NI-PVVETLFDNR         PIC S9(4) COMP.
           10 NI-PVEINZVTGHOSTID    PIC S9(4) COMP.
           10 NI-PVBEDINGUNG-KTY    PIC S9(4) COMP.
           10 NI-PVBEDINGUNG-KNR    PIC S9(4) COMP.
           10 NI-PVMAILING-KTY      PIC S9(4) COMP.
           10 NI-PVMAILING-KNR      PIC S9(4) COMP.
           10 NI-PVZUSATZINFO-KTY   PIC S9(4) COMP.
           10 NI-PVZUSATZINFO-KNR   PIC S9(4) COMP.
           10 NI-PVINFOQUELLE-KTY   PIC S9(4) COMP.
           10 NI-PVINFOQUELLE-KNR   PIC S9(4) COMP.
           10 NI-PVBAUJAHR          PIC S9(4) COMP.
           10 NI-PVLEISTUNG         PIC S9(4) COMP.
           10 NI-PVBETREIBER        PIC S9(4) COMP.
           10 NI-PVBATTERIEKZ       PIC S9(4) COMP.
           10 NI-PVGEBART-KTY       PIC S9(4) COMP.
           10 NI-PVGEBART-KNR       PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-PHOTOVOLTAIK.
           10 PVVERTRAGSID         PIC X(13).
           10 PVVETLFDNR           PIC S9(4) USAGE COMP.
           10 PVEINZVTGHOSTID      PIC X(15).
           10 PVBEDINGUNG-KTY      PIC S9(4) USAGE COMP.
           10 PVBEDINGUNG-KNR      PIC S9(15)V USAGE COMP-3.
           10 PVMAILING-KTY        PIC S9(4) USAGE COMP.
           10 PVMAILING-KNR        PIC S9(15)V USAGE COMP-3.
           10 PVZUSATZINFO-KTY     PIC S9(4) USAGE COMP.
           10 PVZUSATZINFO-KNR     PIC S9(15)V USAGE COMP-3.
           10 PVINFOQUELLE-KTY     PIC S9(4) USAGE COMP.
           10 PVINFOQUELLE-KNR     PIC S9(15)V USAGE COMP-3.
           10 PVBAUJAHR            PIC X(4).
           10 PVLEISTUNG           PIC S9(4)V9(2) USAGE COMP-3.
           10 PVBETREIBER          PIC X(35).
           10 PVBATTERIEKZ         PIC X(2).
           10 PVGEBART-KTY         PIC S9(4) USAGE COMP.
           10 PVGEBART-KNR         PIC S9(15)V USAGE COMP-3.
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************

