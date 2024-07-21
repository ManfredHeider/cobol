      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2749.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2749.CPv  $
      *
      *   Rev 2.1   Jun 12 2013 10:51:38   t13ex27
      *AKI01204: SEPA - DB-Online-Adressen-Programme
      *
      *   Rev 2.0   Apr 15 2013 11:06:36   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01 PI-DRKI2749.
           05  PI-DRKI2749-PROGRAMM            PIC  X(008).
           05  PI-DRKI2749-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DRKI2749-DATEN.
             10 PI-DRKI2749-BZVADRESSEID   PIC X(13).
      ***
       01 PO-DRKI2749.
           05 PO-DRKI2749-RC                    PIC  X(003).
             88 PO-DRKI2749-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2749-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2749-RC-EOF                       VALUE 'EOF'.

           05 PO-DRKI2749-DATEN.
             10 PO-DRKI2749-BZVVERTRAGSID   PIC  X(13).
             10 PO-DRKI2749-BZVLFDNR        PIC S9(004) COMP.

