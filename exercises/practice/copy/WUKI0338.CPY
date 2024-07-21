      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.06.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0338.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0338.CPv  $
      *
      *   Rev 2.1   Jun 02 2010 09:48:06   t13ex14
      *AKI00970: DB-Änderungen für Rel. 5.12: Badenia
      *
      *   Rev 2.0   Jun 02 2010 07:32:38   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-UMSATZ.
           10 UMSVERTRAGSID               PIC X(13).
           10 UMSGUTHABPRODLFDNR          PIC S9(4) COMP.
           10 UMSLAUFENDENR               PIC S9(4) COMP.
           10 UMSBUCHUNGSDATUM            PIC X(10).
           10 NIL-UMSBUCHUNGSDATUM        PIC X(01).
           10 UMSUMSATZBETRAG             PIC S9(9)V9(2) COMP-3.
           10 NIL-UMSUMSATZBETRAG         PIC X(01).
           10 UMSUMSBETRWKZ               PIC X(01).
           10 UMSTEXTID                   PIC S9(4) COMP.
           10 UMSZAHLUNGSWEISEID          PIC S9(4) COMP.
           10 NIL-UMSZAHLUNGSWEISEID      PIC X(01).
           10 UMSTEXT.
              49 UMSTEXT-LEN              PIC S9(4) COMP.
              49 UMSTEXT-TEXT             PIC X(20).
           10 NIL-UMSTEXT                 PIC X(01).
           10 LETZTEAENDERUNG             PIC X(26).
           10 NIL-LETZTEAENDERUNG         PIC X(01).
           10 UMSVWLUMSATZKZ              PIC  X(1).
           10 NIL-UMSVWLUMSATZKZ          PIC X(01).
      ***********************************************************
