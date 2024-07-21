      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1733.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1733.CPv  $
      *
      *   Rev 2.1   Nov 15 2004 12:07:02   t13ex27
      *FKI00881: 9-er VB's in der BT (Gerätetausch)
      *
      *   Rev 2.0   Oct 14 2004 16:02:32   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01 PI-DRKI1733.
          05 PI-DRKI1733-PROGRAMM                 PIC  X(008).
          05 PI-DRKI1733-UFKT-CODE                PIC  9(002).
          05 PI-DRKI1733-DATEN.
            07 PI-DRKI1733-MITARBEITERNR-F        PIC  S9(009) COMP.
            07 PI-DRKI1733-MITARBEITERNR-L        PIC  S9(009) COMP.
      *
       01 PO-DRKI1733.

          05 PO-DRKI1733-RC                       PIC  X(003).
             88 PO-DRKI1733-RC-OK                    VALUE 'OK '.
             88 PO-DRKI1733-RC-LEER                  VALUE 'LER'.
             88 PO-DRKI1733-RC-EOF                   VALUE 'EOF'.

          05 PO-DRKI1733-DATEN.
            07 PO-DRKI1733-MITARBPERSONENID       PIC X(013).
