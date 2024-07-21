      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0441.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WUKI0441.CPv  $
      *
      *   Rev 2.1   Sep 11 2009 07:56:38   t13ex14
      *AKI00843: DB-Änderungen für Rel. 4.60 - Badenia
      *
      *   Rev 2.0   Aug 14 2009 10:44:06   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-BAUSPJAHRWERTRST.
            10 BJRVERTRAGSID               PIC  X(13).
            10 BJRGUTHPRODLFDNR            PIC  S9(4) COMP.
            10 BJRJAHRZULAGE               PIC  S9(4) COMP.
            10 BJRJHRZULAGGUT              PIC  S9(7)V9(2) COMP-3.
            10 NIL-BJRJHRZULAGGUT          PIC X(01).
            10 BJRDATZULAGGUT              PIC  X(010).
            10 NIL-BJRDATZULAGGUT          PIC X(01).
            10 LETZTEAENDERUNG             PIC  X(026).
            10 NIL-LETZTEAENDERUNG         PIC X(01).
