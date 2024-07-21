      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2572.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2572.CPv  $
      *
      *   Rev 2.1   Mar 28 2012 14:11:38   t13ex15
      *AKI01126: AS-Club Kontrollprogramm
      *
      *   Rev 2.0   Mar 13 2012 08:05:36   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    AS-Club Analyse
      *    Satzlänge 600 (FB)
      *---------------------------------------------------------------*
      *
       01  'XXXX'-SATZ.
           05 'XX'-KEY.
              07 'XX'-VERGLEICHSBEREICH.
                 10 FILLER                    PIC  X(006).
                 10 'XX'-VBNR                 PIC  9(007).
                 10 FILLER                    PIC  X(005).
                 10 'XX'-VNID                 PIC  X(013).
                 10 FILLER                    PIC  X(021).
                 10 'XX'-GESELLSCHAFT         PIC  9(002).
                 10 FILLER                    PIC  X(005).
                 10 'XX'-VERTRAGSNUMMER       PIC  9(015).
                 10 FILLER                    PIC  X(021).
                 10 'XX'-LFDNR                PIC  X(004).
                 10 FILLER REDEFINES 'XX'-LFDNR.
                    15 FILLER                 PIC  X(002).
                    15 'XX'-FOLGENUMMER       PIC  X(002).
                 10 FILLER                    PIC  X(005).
           05 'XX'-TEXT1                      PIC  X(019).
           05 FILLER                          PIC  X(005).
           05 'XX'-TEXT2                      PIC  X(032).
           05 FILLER                          PIC  X(005).
           05 'XX'-GHPPRODUKTID               PIC  X(012).
           05 FILLER                          PIC  X(005).
           05 'XX'-TOPZINS-JN                 PIC  X(013).
           05 FILLER                          PIC  X(005).
           05 FILLER                          PIC  X(011).
           05 FILLER                          PIC  X(005).
           05 'XX'-VERMANDANTNR               PIC  X(012).
           05 FILLER                          PIC  X(005).
           05 'XX'-VERVERTRZUSTANDID          PIC  X(017).
           05 FILLER                          PIC  X(005).
           05 'XX'-ZUSTANDTEXT                PIC  X(021).
           05 FILLER                          PIC  X(005).
           05 'XX'-VERVERTRAGSBEGINN          PIC  X(010).
           05 FILLER                          PIC  X(012).
           05 'XX'-VERVERTRAGSABLAUF          PIC  X(010).
           05 FILLER                          PIC  X(012).
           05 'XX'-GHPSCHLIESSDATUM           PIC  X(016).
           05 FILLER                          PIC  X(005).
           05 'XX'-VEREIGENVERMKZ             PIC  X(001).
           05 FILLER                          PIC  X(018).
           05 'XX'-GHPEIGENVERMITTELT         PIC  X(018).
           05 FILLER                          PIC  X(005).
           05 'XX'-VRBKOMMBETRKZ              PIC  X(013).
           05 FILLER                          PIC  X(005).
           05 'XX'-KIKUNDENNUMMER             PIC  X(014).
           05 FILLER                          PIC  X(005).
           05 'XX'-VRBPERSONENROLLEKZ         PIC  X(018).
           05 FILLER                          PIC  X(005).
           05 'XX'-VETPRODUKTID               PIC  X(012).
           05 FILLER                          PIC  X(005).
           05 'XX'-VERFREMDVERTRAGKZ          PIC  X(017).
           05 FILLER                          PIC  X(005).
           05 FILLER                          PIC  X(010).
           05 'XX'-VERTRAGSID-DATUM           PIC  X(010).
           05 FILLER                          PIC  X(093).

