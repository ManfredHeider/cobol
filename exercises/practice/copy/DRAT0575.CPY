      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0575.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0575.CPv  $
      *
      *   Rev 2.1   Feb 21 2013 15:10:16   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.0   Feb 19 2013 09:16:34   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0575                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWAT0035                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01244!19.02.13! 01 ! NEUERSTELLUNG             !B.PRETSCH *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0575                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0575.
          05 PI-DRAT0575-LZ                 PIC S9(004) COMP.
          05 PI-DRAT0575-BIC                PIC  X(011) .
          05 PI-DRAT0575-BLZ                PIC  X(011) .
      *
       01 PO-DRAT0575.
          05 PO-DRAT0575-RC                 PIC  X(003) .
             88 PO-DRAT0575-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0575-RC-NOT-FOUND                 VALUE 'NFD'.
      *
          05 PO-DRAT0575-DATEN.
            10 PO-DRAT0575-BLZ              PIC  X(011) .
            10 PO-DRAT0575-BANKNAME         PIC  X(058) .
      *
      ***** Ende Copy-Member DRAT0575 *****************************
