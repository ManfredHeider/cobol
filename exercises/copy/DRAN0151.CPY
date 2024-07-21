      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0151.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRAN0151.CPv  $
      *
      *   Rev 2.7   Oct 24 2013 11:01:42   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV + 
      *ADVC (Dialog)
      *
      *   Rev 2.6   May 28 2008 08:50:20   t13ae34
      *AAN00801: POA-Antrag
      *
      *   Rev 2.5   Feb 13 2007 19:33:02   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.4   Feb 13 2007 18:32:22   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.3   Feb 13 2007 17:26:42   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   22 Dec 2006 20:50:10   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   24 Oct 2006 13:55:52   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   12 Oct 2006 10:37:12   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0151 FUNKTION                             *
      *                 ZUSATZDATEN ELEKTRONISCHER ANTRAG AMV (GES.10)*
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAN00655!12.10.06!2.1 ! NEU                       ! RAHNER    *
      * AAN00655!21.10.06!2.2 ! ANPASSUNGEN               ! HEIDER    *
      * AAN00801!27.05.08!2.3 ! KENNZ. WIDERSPRUCH AMGBSM ! HEIDER    *
      * AAN01073!24.10.13!2.7 ! SEPA                      ! RAHNER    *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      *****************************************************************

                20 ZUSATZ-SCHALTER-POA-PIN  PIC  X(001).
                   88 POA-PIN-VORHANDEN       VALUE '1'.
                   88 POA-PIN-NICHT-VORHANDEN VALUE '0'.

                20 ZUSATZ-POA-HVANTNUM-LFD-1  PIC  X(001).
                20 ZUSATZ-POA-HVANTNUM-LFD-NUM-1 REDEFINES
                   ZUSATZ-POA-HVANTNUM-LFD-1  PIC  9(001).

                20 ZUSATZ-POA-HVANTNUM-LFD-2  PIC  X(001).
                20 ZUSATZ-POA-HVANTNUM-LFD-NUM-2 REDEFINES
                   ZUSATZ-POA-HVANTNUM-LFD-2  PIC  9(001).

                20 ZUSATZ-POA-HVANTNUM-LFD-3  PIC  X(001).
                20 ZUSATZ-POA-HVANTNUM-LFD-NUM-3 REDEFINES
                   ZUSATZ-POA-HVANTNUM-LFD-3  PIC  9(001).

                20 ZUSATZ-POA-HVANTNUM-LFD-4  PIC  X(001).
                20 ZUSATZ-POA-HVANTNUM-LFD-NUM-4 REDEFINES
                   ZUSATZ-POA-HVANTNUM-LFD-4  PIC  9(001).

                20 ZUSATZ-POA-HVANTNUM-LFD-5  PIC  X(001).
                20 ZUSATZ-POA-HVANTNUM-LFD-NUM-5 REDEFINES
                   ZUSATZ-POA-HVANTNUM-LFD-5  PIC  9(001).

                20 ZUSATZ-POA-HAND          PIC  X(001).
                20 ZUSATZ-POA-VBEGINN-E     PIC  X(010).
                20 ZUSATZ-POA-VABLAUF-E     PIC  X(010).
                20 ZUSATZ-POA-KNR-ZAHLW-E   PIC S9(004) COMP.
                20 ZUSATZ-POA-KTY-ZAHLW-E   PIC S9(004) COMP.
                20 ZUSATZ-POA-PDVNBLZ-E     PIC S9(008) COMP.
                20 ZUSATZ-POA-PDVNKONTONR-E PIC S9(010) COMP.
                20 ZUSATZ-POA-PDVNBEGINKM-E PIC S9(006) COMP.
                20 ZUSATZ-POA-PDVNFGESTNR-E PIC  X(017).
                20 ZUSATZ-POA-PDVNAMTLKZ-E  PIC  X(011).
                20 ZUSATZ-POA-PDVNJAHRKM-E  PIC S9(006) COMP.
270508          20 ZUSATZ-POA-WIDERSPR-E    PIC  X(001).
241013          20 ZUSATZ-POA-IBAN-E        PIC  X(034).
241013          20 ZUSATZ-POA-BIC-E         PIC  X(011).
241013          20 FILLER                   PIC  X(875).


