      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1816.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1816.CPv  $
      *
      *   Rev 2.2   Mar 17 2020 08:35:02   T13ae37
      *APA02884: Einführung "Bezugsjahr" bei allen USt.pfl. Produkt
      *en in DAISY - PVW
      *
      *   Rev 2.1   Sep 15 2015 10:06:32   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *PVW-An
      *passungen für LUCY PGV's
      *
      *   Rev 2.0   Jun 02 2015 06:42:56   t13ex67
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA01816  FUNKTION                          *
      *                 "UMSATZSTEUER BERECHNEN UND VERTEILEN        *
      *                                                              *
      *    DIE EINZELNEN FUNKTIONEN :                                *
      *    DRPA1817 - UMSATZSTEUER BERECHNEN UND VERTEILEN           *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    02.06.2015                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!02.06.15!02.01!NEUERSTELLUNG             ! STORCH   *
      * APA02884!19.11.19!02.2 !DRPA1941                  ! BUCH     *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UWPA0052.
           05 PI-UWPA0052-FUNKTION            PIC X(008).
              88 PI-UWPA0052-DRPA1817             VALUE 'DRPA1817'.
191119        88 PI-UWPA0052-DRPA1941             VALUE 'DRPA1941'.


