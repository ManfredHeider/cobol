      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.11.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2280.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2280.CPv  $
      *
      *   Rev 2.1   Apr 17 2009 15:24:06   t13ex51
      *AKI00750: PMKI0167 Kundenaktualisierung - Änderung bei VN-We
      *chsel
      *
      *   Rev 2.0   Nov 14 2008 10:55:14   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01 PI-DRKI2280.
          05 PI-DRKI2280-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2280-UFKT-CODE                PIC  9(002).
          05 PI-DRKI2280-DATEN.
            07 PI-DRKI2280-VNID                   PIC  X(013).
            07 PI-DRKI2280-VBID                   PIC  X(013).
      *
       01 PO-DRKI2280.

          05 PO-DRKI2280-RC                       PIC  X(003).
             88 PO-DRKI2280-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2280-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2280-RC-EOF                       VALUE 'EOF'.

          05 PO-DRKI2280-DATEN.
             07 PO-DRKI2280-MITNEU            PIC S9(009) BINARY.
             07 PO-DRKI2280-MITALT            PIC S9(009) BINARY.
             07 PO-DRKI2280-ANZ               PIC S9(09) USAGE COMP.
      ***** Ende Copy-Member DRKI2280 *****************************

