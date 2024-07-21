      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.12.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0427.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0427.CPv  $
      *
      *   Rev 2.2   04 Dec 2008 15:55:20   t13ex27
      *AKI00772: Reisegepäckversicherung-DB-Copys und Mapping
      *
      *   Rev 2.1   02 Dec 2008 14:28:26   t13ex27
      *AKI00772: Reisegepäckversicherung-DB-Copys und Mapping
      *
      *   Rev 2.0   02 Dec 2008 13:48:26   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-TCKI0427.
           10 VRGVERTRAGSID         PIC  X(13).
           10 VRGVETLFDNR           PIC  S9(4) COMP.
           10 VRGRISIGRUPPEID       PIC  S9(4) COMP.
           10 VRGEINZVTGHOSTID      PIC  X(15).
           10 VRGRISIKOHOSTID       PIC  S9(9) COMP.
           10 NIL-VRGRISIKOHOSTID   PIC  X(01).
           10 VRGRISIGRTYP_KTY      PIC  S9(4) COMP.
           10 VRGRISIGRTYP_KNR      PIC  S9(15)V COMP-3.
           10 VRGBEZEICHNUNG        PIC  X(50).
           10 VRGBEGINNRISGRUP      PIC  X(010).
           10 NIL-VRGBEGINNRISGRUP  PIC  X(01).
           10 VRGABLAUFRISGRUP      PIC  X(010).
           10 NIL-VRGABLAUFRISGRUP  PIC  X(01).
           10 VRGNETTOBEITRAG       PIC  S9(8)V9(2) COMP-3.
           10 VRGUBERGEOGRUPID      PIC  S9(4) COMP.
           10 LETZTEAENDERUNG       PIC  X(026).
           10 VRGDIFDECKKZ          PIC  X(1).
           10 NIL-VRGDIFDECKKZ      PIC  X(01).      
           10 VRGGESELLSCH_KTY      PIC  S9(4) COMP.
           10 VRGGESELLSCH_KNR      PIC  S9(15)V COMP-3.
           10 VRGVERSNUMMER         PIC  X(40).
           10 NIL-VRGVERSNUMMER     PIC  X(01).
           10 VRGUMSTELLUNGDAT      PIC  X(010).
           10 NIL-VRGUMSTELLUNGDAT  PIC  X(01).
           10 VRGKUENDIGUNGKZ       PIC  X(1).
           10 NIL-VRGKUENDIGUNGKZ   PIC  X(01).
