      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.12.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0419.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0419.CPv  $
      *
      *   Rev 2.1   02 Dec 2008 14:28:22   t13ex27
      *AKI00772: Reisegepäckversicherung-DB-Copys und Mapping
      *
      *   Rev 2.0   02 Dec 2008 13:48:18   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-TCKI0419.
           10 FKVVERTRAGSID         PIC  X(13).
           10 FKVVETLFDNR           PIC  S9(4) COMP.
           10 FKVEINZVTGHOSTID      PIC  X(15).
           10 FKVTARIFPRODUKT       PIC  X(50).
           10 FKVBEDINGUNG          PIC  X(20).
           10 FKVZUSATZINFO         PIC  X(100).
           10 FKVMAILINGAKTION      PIC  X(70).
           10 LETZTEAENDERUNG       PIC  X(026).
           10 FKVGESELLSCH_KTY      PIC  S9(4) COMP.
           10 FKVGESELLSCH_KNR      PIC  S9(15)V COMP-3.
           10 FKVVERSNUMMER         PIC  X(40).
           10 NIL-FKVVERSNUMMER     PIC  X(01).
           10 FKVUMSTELLUNGDAT      PIC  X(010).
           10 NIL-FKVUMSTELLUNGDAT  PIC  X(01).
           10 FKVKUENDIGUNGKZ       PIC  X(1).
           10 NIL-FKVKUENDIGUNGKZ   PIC  X(01).
