      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.12.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0438.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0438.CPv  $
      *
      *   Rev 2.1   02 Dec 2008 14:28:44   t13ex27
      *AKI00772: Reisegepäckversicherung-DB-Copys und Mapping
      *
      *   Rev 2.0   02 Dec 2008 13:48:40   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE REISEGEPAECKVERT
      ******************************************************************
CCC005*    EXEC SQL DECLARE REISEGEPAECKVERT TABLE
CCC004*    ( RGPVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      RGPVETLFDNR                    SMALLINT NOT NULL,
CCC004*      RGPEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      RGPBEDINGUNG_KTY               SMALLINT NOT NULL,
CCC004*      RGPBEDINGUNG_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      RGPVERSSUMME                   INTEGER NOT NULL,
CCC004*      RGPMAILING_KTY                 SMALLINT NOT NULL,
CCC004*      RGPMAILING_KNR                 DECIMAL(15, 0) NOT NULL,
CCC004*      RGPZUSINFO_KTY                 SMALLINT NOT NULL,
CCC004*      RGPZUSINFO_KNR                 DECIMAL(15, 0) NOT NULL,
CCC004*      RGPDIFDECKKZ                   CHAR(1),
CCC004*      RGPGESELLSCH_KTY               SMALLINT NOT NULL,
CCC004*      RGPGESELLSCH_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      RGPVERSNUMMER                  CHAR(40),
CCC004*      RGPVUMSTELLUNGDAT              DATE,
CCC004*      RGPKUENDIGUNGKZ                CHAR(1),
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  REISEGEPAECKVERT
      ******************************************************************
       01  REISEGEPAECKVERT-GESAMT.
         05 NI-REISEGEPAECKVERT-TAB.
           10 NI-REISEGEPAECKVERT-ELE PIC S9(4) COMP OCCURS 17 TIMES.
         05 NI-REISEGEPAECKVERT REDEFINES NI-REISEGEPAECKVERT-TAB.
           10 NI-RGPVERTRAGSID        PIC S9(4) COMP.
           10 NI-RGPVETLFDNR          PIC S9(4) COMP.
           10 NI-RGPEINZVTGHOSTID     PIC S9(4) COMP.
           10 NI-RGPBEDINGUNG-KTY     PIC S9(4) COMP.
           10 NI-RGPBEDINGUNG-KNR     PIC S9(4) COMP.
           10 NI-RGPVERSSUMME         PIC S9(4) COMP.
           10 NI-RGPMAILING-KTY       PIC S9(4) COMP.
           10 NI-RGPMAILING-KNR       PIC S9(4) COMP.
           10 NI-RGPZUSINFO-KTY       PIC S9(4) COMP.
           10 NI-RGPZUSINFO-KNR       PIC S9(4) COMP.
           10 NI-RGPDIFDECKKZ         PIC S9(4) COMP.
           10 NI-RGPGESELLSCH-KTY     PIC S9(4) COMP.
           10 NI-RGPGESELLSCH-KNR     PIC S9(4) COMP.
           10 NI-RGPVERSNUMMER        PIC S9(4) COMP.
           10 NI-RGPVUMSTELLUNGDAT    PIC S9(4) COMP.
           10 NI-RGPKUENDIGUNGKZ      PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.
         05 DCL-REISEGEPAECKVERT.
           10 RGPVERTRAGSID        PIC X(13).
           10 RGPVETLFDNR          PIC S9(4) COMP.
           10 RGPEINZVTGHOSTID     PIC X(15).
           10 RGPBEDINGUNG-KTY     PIC S9(4) COMP.
           10 RGPBEDINGUNG-KNR     PIC S9(15)V COMP-3.
           10 RGPVERSSUMME         PIC S9(9) COMP.
           10 RGPMAILING-KTY       PIC S9(4) COMP.
           10 RGPMAILING-KNR       PIC S9(15)V COMP-3.
           10 RGPZUSINFO-KTY       PIC S9(4) COMP.
           10 RGPZUSINFO-KNR       PIC S9(15)V COMP-3.
           10 RGPDIFDECKKZ         PIC X(1).
           10 RGPGESELLSCH-KTY     PIC S9(4)  COMP.
           10 RGPGESELLSCH-KNR     PIC S9(15)V COMP-3.
           10 RGPVERSNUMMER        PIC X(40).
           10 RGPVUMSTELLUNGDAT    PIC X(10).
           10 RGPKUENDIGUNGKZ      PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
      ******************************************************************