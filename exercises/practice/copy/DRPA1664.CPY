      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.06.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1664.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1664.CPv  $
      *
      *   Rev 2.1   Jun 18 2007 11:33:54   t13ex21
      *FPA00915: Korrektur Lesen Gesamtprovision bei I137
      *
      *   Rev 2.0   Jun 15 2007 14:11:00   t13ex21
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1664  FUNKTION SELECT DB2-LESE-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    15.06.2007                                      *
      ****************************************************************
      *    VERSION   2.01                                            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00622!15.06.07!2.01! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1664                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<     ERMITTELN GUELTIG-BIS FÜR KONDITION   *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRPA1664.
           05  PI-DRPA1664-PGEB-GES           PIC S9(004)  BINARY.
           05  PI-DRPA1664-PROD-PROD-SCHL     PIC S9(004)  BINARY.
           05  PI-DRPA1664-KNR-KOND-ART       PIC S9(004)  BINARY.
           05  PI-DRPA1664-KTY-KOND-ART       PIC S9(004)  BINARY.
           05  PI-DRPA1664-PRKO-KOND-NR       PIC S9(004)  BINARY.
      *
       01  PO-DRPA1664.
           05  PO-DRPA1664-RC                 PIC  X(003).
               88 PO-DRPA1664-RC-OK                VALUE 'OK '.
               88 PO-DRPA1664-RC-NFD               VALUE 'NFD'.
      *
           05  PO-DRPA1664-GUELTIG-BIS        PIC  X(010).
           05  PO-DRPA1664-NACHTAG            PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
