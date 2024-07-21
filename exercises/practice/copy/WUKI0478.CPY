      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01/07/15
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUKI0478.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUKI0478.CPv  $
      *
      *   Rev 2.2   Jul 09 2015 10:16:10   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.1   Jul 09 2015 09:50:50   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.0   Jul 1 2015 10:45:58   T13EX48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        05  WU-LEBENGELRENTE.
           10 LGRVERTRAGSID                 PIC  X(0013).
           10 LGRRNTBEGINN                  PIC  X(010).
           10 LGRRNTART                     PIC S9(4) COMP.
           10 LGRRNTABLAUF                  PIC  X(010).
           10 NIL-LGRRNTABLAUF              PIC  X(01).
           10 LGRRNTZAHLW-KTY               PIC S9(4) COMP.
           10 LGRRNTZAHLW-KNR               PIC S9(4) COMP.
           10 LGRGSMTHOEHE                  PIC S9(11)V9(2) COMP-3.
           10 LETZTEAENDERUNG               PIC  X(0026).
           10 ZUGANG                        PIC  X(0032).
           10 VON                           PIC  X(0032).
           10 BIS                           PIC  X(0032).


