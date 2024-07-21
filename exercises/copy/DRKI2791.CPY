      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2791.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2791.CPv  $
      *
      *   Rev 2.2   Apr 04 2014 11:25:36   T13Ex77
      *FKI01460: Fehlerdateien und Fehlermails DWS
      *
      *   Rev 2.1   Mar 11 2014 15:48:38   t13ex77
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.0   Nov 12 2013 13:16:02   t13ex77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
       01  PI-DRKI2791.
           05  PI-DRKI2791-KEY.
               10   PI-DRKI2791-GHPVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI2791.
           05  PO-DRKI2791-RC                   PIC  X(003).
               88 PO-DRKI2791-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2791-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2791-DATEN.
               10   PO-DRKI2791-COUNTER         PIC S9(4) COMP.
