      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2537.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2537.CPv  $
      *
      *   Rev 2.1   Dec 11 2012 14:52:14   t13ex27
      *AKI00899: Reaktivierung postalische Prüfung-Löschen von Adre
      *ssen
      *
      *   Rev 2.0   Nov 02 2011 16:13:24   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
       01 PI-DRKI2537.
           05  PI-DRKI2537-PROGRAMM            PIC  X(008).
           05  PI-DRKI2537-UFKT                PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
      ***
       01 PO-DRKI2537.
           05 PO-DRKI2537-RC                   PIC  X(003).
             88 PO-DRKI2537-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2537-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2537-RC-EOF                       VALUE 'EOF'.

           05 PO-DRKI2537-DATEN.
             10 PO-DRKI2537-ADRESSEID             PIC X(13).
             10 PO-DRKI2537-ADRESSEBDSLDID        PIC X(3).
             10 PO-DRKI2537-NI-ADRESSEBDSLDID     PIC X(01).
             10 PO-DRKI2537-ADRESSELANDID         PIC X(3).
             10 PO-DRKI2537-ADRESSEORT            PIC X(30).
             10 PO-DRKI2537-NI-ADRESSEORT         PIC X(01).
             10 PO-DRKI2537-ADRESSEPLZ            PIC X(15).
             10 PO-DRKI2537-NI-ADRESSEPLZ         PIC X(01).
             10 PO-DRKI2537-ADRESSESTRHAUSNUM     PIC X(45).
             10 PO-DRKI2537-NI-DRSSSTRHSNM        PIC X(01).
             10 PO-DRKI2537-ADRESSEISTVTADR       PIC X(1).
             10 PO-DRKI2537-LETZTEAENDERUNG       PIC X(026).
             10 PO-DRKI2537-NI-LETZTEAENDERUNG    PIC X(01).
             10 PO-DRKI2537-ADRESSESUCHSTRASSE    PIC X(45).
             10 PO-DRKI2537-NI-DRSSSCHSTRSS       PIC X(01).
             10 PO-DRKI2537-ADRESSEGEPRUEFT       PIC X(1).
             10 PO-DRKI2537-NI-ADRESSEGEPRUEFT    PIC X(01).
             10 PO-DRKI2537-ADRESSEORTSTEIL       PIC X(30).
             10 PO-DRKI2537-ADRESSEPOSTPRUEF      PIC X(25).
             10 PO-DRKI2537-ADRESSEPOSTPRUEFID    PIC S9(4) COMP.
      *****************************************************************
