      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2102.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2102.CPv  $
      *
      *   Rev 2.5   Sep 25 2015 12:35:06   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      * - PGMs
      *
      *   Rev 2.4   Sep 15 2015 15:07:14   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      * - PGMs
      *
      *   Rev 2.3   19 Nov 2007 09:56:06   t13ex27
      *AKL00664: Allfinanz: Anpassung Kundenaktualisierung
      *
      *   Rev 2.2   18 Nov 2007 15:26:28   t13ex27
      *No change.
      *
      *   Rev 2.1   16 Oct 2007 14:26:12   t13ex27
      *AKL00664: Allfinanz: Anpassung Kundensuche
      *
      *   Rev 2.0   25 Sep 2007 10:08:26   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2102  FUNKTION Lese-OZP                  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKL00664!24.09.07! 01 ! NEUERSTELLUNG             !I. Frenzel*
      * -------------------------------------------------------------*
010915* AKI01378!01.09.15! 02 ! Interne VNR hinzufuegen   !DDY       *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRKI2102.
           05  PI-DRKI2102-VNR                PIC X(15).
           05  PI-DRKI2102-PGEB-GES           PIC S9(4) BINARY.
 
      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRKI2102.
           05  PO-DRKI2102-RC                  PIC  X(03).
               88  PO-DRKI2102-RC-OK           VALUE 'OK '.
               88  PO-DRKI2102-RC-NFD          VALUE 'NFD'.
           05 PO-NI-DRKI2102-TAB.
             10 PO-DRKI2102-NI-ELE PIC S9(04) COMP OCCURS  3 TIMES.
           05 PO-NI-DRKI2102  REDEFINES  PO-NI-DRKI2102-TAB.
              10 PO-DRKI2102-NI-VERTRAGSBEGINN  PIC  S9(004) COMP.
              10 PO-DRKI2102-NI-DATVERTRAEND    PIC  S9(004) COMP.
              10 PO-DRKI2102-NI-VERMANDANTNR    PIC  S9(004) COMP.
010915        10 PO-DRKI2102-NI-VERINTERNEVNR   PIC  S9(004) COMP.
           05 PO-DRKI2102-DATEN.
              10 PO-DRKI2102-VERTRAGS-ID        PIC  X(13).
              10 PO-DRKI2102-VERTRAGSBEGINN     PIC  X(10).
              10 PO-DRKI2102-DATVERTRAEND       PIC  X(10).
              10 PO-DRKI2102-VERMANDANTNR       PIC S9(9) COMP.
              10 PO-DRKI2102-VERGESBEITRWKZ     PIC  X(01).
              10 PO-DRKI2102-VERABSCHLUSSVBNR   PIC S9(9) COMP.
              10 PO-DRKI2102-VERVERTRZUSTANDID  PIC S9(4) COMP.
010915        10 PO-DRKI2102-VERINTERNEVNR      PIC S9(9) COMP.
      *
      ******** ENDE COPY-MEMBER (DRKI2102) *******************
 

