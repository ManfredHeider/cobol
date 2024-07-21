      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.09.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0439.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0439.CPv  $
      *
      *   Rev 2.1   Sep 17 2008 09:27:54   t13ex51
      *AKI00731: Anpassen für Lesen TBAT0039 mit UPKI
      *
      *   Rev 2.0   Sep 16 2008 15:57:48   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0439
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0439.
           05  PI-DRAT0439-KEY.
             10 PI-DRAT0439-KTY               PIC S9(4) COMP.
             10 PI-DRAT0439-KNR               PIC x(15).
      *
       01  PO-DRAT0439.
           05  PO-DRAT0439-RC                 PIC  X(003).
               88 PO-DRAT0439-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0439-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRAT0439-DATEN.
             10 PO-DRAT0439-KNR               PIC S9(15)V USAGE COMP-3.
             10 PO-DRAT0439-BEZEICHNUNG       PIC X(100).
             10 PO-DRAT0439-BEZEICHNUNG-KURZ  PIC X(20).
      *
      ******* ENDE COPY-MEMBER **************************************
