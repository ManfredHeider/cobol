      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1148.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1148.CPv  $
      *
      *   Rev 2.1   Sep 09 2016 11:36:36   t13ae13
      *AVB01400: Projekt Lucy2 - ZWVB0004, ZWVB0015 (SWL)
      *
      *   Rev 2.0   Sep 01 2016 08:16:04   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1148                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    01.09.2016                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01400!01.09.16!2.1 ! NEUERSTELLUNG             ! Walb     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1148                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB1148.
          05 PI-DRVB1148-MAAN-BEZUGS-NR     PIC S9(009) COMP.
          05 PI-DRVB1148-KNR-MAAN-TYP       PIC S9(004) COMP.
      *
       01 PO-DRVB1148.

          05 PO-DRVB1148-RC                 PIC  X(003).
             88 PO-DRVB1148-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1148-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB1148-DATEN.
             10 PO-DRVB1148-MAAN-STRASSE     PIC  X(040).
             10 PO-DRVB1148-MAAN-PLZ-AKTUELL PIC  X(009).
             10 PO-DRVB1148-MAAN-ORT         PIC  X(040).
             10 PO-DRVB1148-AUSL             PIC  S9(4) USAGE COMP.
             10 PO-DRVB1148-MAAN-TELEFAX     PIC  X(026).
             10 PO-DRVB1148-MAAN-TELEFON     PIC  X(026).
             10 PO-DRVB1148-ZUSTELLUNG       PIC  S9(4) USAGE COMP.

      *
      ***** ENDE COPY-MEMBER DRVB1148 *****************************

