      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2385.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2385.CPv  $
      *
      *   Rev 2.1   Mar 15 2010 12:09:28   t13ex51
      *AKI00935: Postal. Prüfung mit LEBENPERSONEIGEN
      *
      *   Rev 2.0   Mar 09 2010 17:30:26   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01 PI-DRKI2385.
           05  PI-DRKI2385-PROGRAMM            PIC  X(008).
           05  PI-DRKI2385-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DRKI2385-DATEN.
             10 PI-DRKI2385-LPEADRESSEID   PIC X(13).
      ***
       01 PO-DRKI2385.
           05 PO-DRKI2385-RC                    PIC  X(003).
             88 PO-DRKI2385-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2385-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2385-RC-EOF                       VALUE 'EOF'.

           05 PO-DRKI2385-DATEN.
             10 PO-DRKI2385-LPEPERSROLLEKZ   PIC X(1).
             10 PO-DRKI2385-LPEVERTRAGSID    PIC X(13).
             10 PO-DRKI2385-LPEADRESSEID     PIC X(13).
             10 PO-DRKI2385-LPEANREDE-KNR    PIC S9(4) COMP.
             10 PO-DRKI2385-LPEGEBDATUM      PIC X(10).
             10 PO-DRKI2385-LPENACHNAME      PIC X(30).
             10 PO-DRKI2385-LPEVORNAME       PIC X(30).
