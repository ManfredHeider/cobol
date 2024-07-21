      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.12.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2552.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2552.CPv  $
      *
      *   Rev 2.5   Feb 27 2012 13:24:12   t13ex15
      *AKI01121: Preisaktualisierung, Anpassung wegen neuen Attribu
      *tlängen
      *
      *   Rev 2.4   Jan 02 2012 13:57:00   t13ex39
      *AKI01094: Antei per 31.12.xxxx speichern
      *
      *   Rev 2.3   Dec 13 2011 13:12:26   t13ex18
      *AKI01094: Antei per 31.12.xxxx speichern
      *
      *   Rev 2.2   Dec 13 2011 12:53:36   t13ex18
      *AKI01094: Antei per 31.12.xxxx speichern
      *
      *   Rev 2.1   Dec 09 2011 16:52:22   t13ex18
      *AKI01094: Antei per 31.12.xxxx speichern
      *
      *   Rev 2.0   Dec 09 2011 15:40:18   t13ex18
      *Archiverstellung
      * ***************************************************************
      *
      *    Anteile per 31.12.xxxx speichern (Rücknahmepreis/RNP)
      *
      *    SATZLÄNGE 100
      *
      *    Suffix -N : Nullif-Indikator-Felder
      *
      *    LOGBUCH
      * --------!-------------------------------------------!----------
      * Auftrag ! Datum  ! Text                             ! Name
      * --------!-------------------------------------------!----------
      * AKI01094!09.12.11! Neuerstellung                    ! MDI
      * ---------------------------------------------------------------
      * AKI01121!27.02.12! Attributvergrößerung Preise      ! SWZ
      * ---------------------------------------------------------------
      * ENDE-LOGBUCH
      *****************************************************************
         01  DRKI2552.
             10 DRKI2552-FONDID            PIC S9(004) COMP.
             10 DRKI2552-FONDISIN          PIC  X(012).
             10 DRKI2552-FONDISIN-N        PIC  X(001).
             10 DRKI2552-FONDWKN           PIC  X(006).
270212*      10 DRKI2552-AUSGABEBETRAG     PIC S9(005)V9(002) COMP-3.
270212       10 DRKI2552-AUSGABEBETRAG     PIC S9(007)V9(002) COMP-3.
             10 DRKI2552-AUSGABEBETRAG-N   PIC  X(001).
             10 DRKI2552-DATUMFONDSTAND    PIC  X(010).
             10 DRKI2552-DATUMFONDSTAND-N  PIC  X(001).
270212*      10 DRKI2552-RUECKNAHMEBETR    PIC S9(005)V9(002) COMP-3.
270212       10 DRKI2552-RUECKNAHMEBETR    PIC S9(007)V9(002) COMP-3.
             10 DRKI2552-RUECKNAHMEBETR-N  PIC  X(001).
             10 DRKI2552-DATUMWERTENTW     PIC  X(010).
             10 DRKI2552-DATUMWERTENTW-N   PIC  X(001).
             10 DRKI2552-WERTENTW          PIC S9(005)V9(002) COMP-3.
             10 DRKI2552-WERTENTW-N        PIC  X(001).
             10 DRKI2552-LETZTEAENDERUNG   PIC  X(026).
             10 DRKI2552-LETZTEAENDERUNG-N PIC  X(001).

