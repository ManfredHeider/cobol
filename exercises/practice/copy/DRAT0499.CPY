      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0499.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0499.CPv  $
      *
      *   Rev 2.2   Feb 21 2013 15:10:10   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.1   Feb 12 2013 18:54:42   t13ex71
      *AVB01244: Projekt SEPA - Realisierung VB und AN
      *
      *   Rev 2.0   Feb 11 2013 11:34:46   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0499                                      *
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
      * AVB01244!11.02.13! 01 ! NEUERSTELLUNG             !B.PRETSCH *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0499                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0499.
          05 PI-DRAT0499-LZ                 PIC S9(004) COMP.
          05 PI-DRAT0499-BIC                PIC  X(011) .
      *
       01 PO-DRAT0499.
          05 PO-DRAT0499-RC                 PIC  X(003) .
             88 PO-DRAT0499-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0499-RC-NOT-FOUND                 VALUE 'NFD'.
      *
          05 PO-DRAT0499-DATEN.
            10 PO-DRAT0499-BIC              PIC  X(011) .
      *
      ***** Ende Copy-Member DRAT0499 *****************************
