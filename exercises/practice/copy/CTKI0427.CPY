      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0427.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTKI0427.CPv  $
      *
      *   Rev 2.4   04 Dec 2008 15:55:24   t13ex27
      *AKI00772: Reisegepäckversicherung-DB-Copys und Mapping
      *
      *   Rev 2.3   02 Dec 2008 14:28:38   t13ex27
      *AKI00772: Reisegepäckversicherung-DB-Copys und Mapping
      *
      *   Rev 2.2   29 Oct 2007 16:45:02   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-DCLGENs und DB-Änderung
      *
      *   Rev 2.1   17 Oct 2007 11:08:06   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-DCLGENs und DB-Änderung
      *
      *   Rev 2.0   27 Sep 2007 12:02:48   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE VERTRISIKOGRUPPE
      ******************************************************************
CCC005*    EXEC SQL DECLARE VERTRISIKOGRUPPE TABLE
CCC004*    ( VRGVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VRGVETLFDNR                    SMALLINT NOT NULL,
CCC004*      VRGRISIGRUPPEID                SMALLINT NOT NULL,
CCC004*      VRGEINZVTGHOSTID               CHAR(15) NOT NULL,
CCC004*      VRGRISIKOHOSTID                INTEGER,
CCC004*      VRGRISIGRTYP_KTY               SMALLINT NOT NULL,
CCC004*      VRGRISIGRTYP_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      VRGBEZEICHNUNG                 CHAR(50) NOT NULL,
CCC004*      VRGBEGINNRISGRUP               DATE,
CCC004*      VRGABLAUFRISGRUP               DATE,
CCC004*      VRGNETTOBEITRAG                DECIMAL(10, 2) NOT NULL,
CCC004*      VRGUBERGEOGRUPID               SMALLINT NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      VRGDIFDECKKZ                   CHAR(1),
CCC004*      VRGGESELLSCH_KTY               SMALLINT NOT NULL,
CCC004*      VRGGESELLSCH_KNR               DECIMAL(15, 0) NOT NULL,
CCC004*      VRGVERSNUMMER                  CHAR(40),
CCC004*      VRGUMSTELLUNGDAT               DATE,
CCC004*      VRGKUENDIGUNGKZ                CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  VERTRISIKOGRUPPE
      ******************************************************************
       01  VERTRISIKOGRUPPE-GESAMT.
         05 NI-VERTRISIKOGRUPPE-TAB.
           10 NI-VERTRISIKOGRUPPE-ELE PIC S9(4) COMP OCCURS 19 TIMES.
         05 NI-VERTRISIKOGRUPPE REDEFINES NI-VERTRISIKOGRUPPE-TAB.
           10 NI-VRGVERTRAGSID        PIC S9(4) COMP.
           10 NI-VRGVETLFDNR          PIC S9(4) COMP.
           10 NI-VRGRISIGRUPPEID      PIC S9(4) COMP.
           10 NI-VRGEINZVTGHOSTID     PIC S9(4) COMP.
           10 NI-VRGRISIKOHOSTID      PIC S9(4) COMP.
           10 NI-VRGRISIGRTYP-KTY     PIC S9(4) COMP.
           10 NI-VRGRISIGRTYP-KNR     PIC S9(4) COMP.
           10 NI-VRGBEZEICHNUNG       PIC S9(4) COMP.
           10 NI-VRGBEGINNRISGRUP     PIC S9(4) COMP.
           10 NI-VRGABLAUFRISGRUP     PIC S9(4) COMP.
           10 NI-VRGNETTOBEITRAG      PIC S9(4) COMP.
           10 NI-VRGUBERGEOGRUPID     PIC S9(4) COMP.
           10 NI-VRGDIFDECKKZ         PIC S9(4) COMP.
           10 NI-VRGGESELLSCH-KTY     PIC S9(4) COMP.
           10 NI-VRGGESELLSCH-KNR     PIC S9(4) COMP.
           10 NI-VRGVERSNUMMER        PIC S9(4) COMP.
           10 NI-VRGUMSTELLUNGDAT     PIC S9(4) COMP.
           10 NI-VRGKUENDIGUNGKZ      PIC S9(4) COMP.
         05 DCL-VERTRISIKOGRUPPE.
           10 VRGVERTRAGSID        PIC X(13).
           10 VRGVETLFDNR          PIC S9(4) COMP.
           10 VRGRISIGRUPPEID      PIC S9(4) COMP.
           10 VRGEINZVTGHOSTID     PIC X(15).
           10 VRGRISIKOHOSTID      PIC S9(9) COMP.
           10 VRGRISIGRTYP-KTY     PIC S9(4) COMP.
           10 VRGRISIGRTYP-KNR     PIC S9(15)V COMP-3.
           10 VRGBEZEICHNUNG       PIC X(50).
           10 VRGBEGINNRISGRUP     PIC X(10).
           10 VRGABLAUFRISGRUP     PIC X(10).
           10 VRGNETTOBEITRAG      PIC S9(8)V9(2) COMP-3.
           10 VRGUBERGEOGRUPID     PIC S9(4) COMP.
           10 LETZTEAENDERUNG      PIC X(26).
           10 VRGDIFDECKKZ         PIC X(1).
           10 VRGGESELLSCH-KTY     PIC S9(4) COMP.
           10 VRGGESELLSCH-KNR     PIC S9(15)V COMP-3.
           10 VRGVERSNUMMER        PIC X(40).
           10 VRGUMSTELLUNGDAT     PIC X(10).
           10 VRGKUENDIGUNGKZ      PIC X(1).
      ******************************************************************
