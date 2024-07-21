      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.01.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1673.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1673.CPv  $
      *
      *   Rev 2.2   Oct 01 2008 14:17:34   t13ae46
      *FPA01056: Umnummerierung Fehler bei Ges. 91
      *
      *   Rev 2.1   01 Feb 2008 12:03:28   t13ae46
      *APA00710: Ges. 91 doppelte Vertragsnr.vergabe
      *
      *   Rev 2.0   25 Jan 2008 13:10:16   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1673                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<UPDATE WIEDERVERWENDETE VNR>>            *
      *    --------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00701!25.01.08!2.0 ! NEU                       ! WANG     *
      * FPA01056!29.08.08!2.1 ! PI-DRPA1673-PROD-PROD-SCHL! WANG     *
      *         !        !    ! HINZU                     !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1673                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
        01 PI-DRPA1673.
           05 PI-DRPA1673-DATEN.
              10 PI-DRPA1673-PGEB-GES         PIC S9(004)  BINARY.
              10 PI-DRPA1673-VNR              PIC  X(015).
290808        10 PI-DRPA1673-PROD-PROD-SCHL   PIC S9(004)  BINARY.
      *
        01 PO-DRPA1673.
           05 PO-DRPA1673-RC                  PIC  X(003).
              88 PO-DRPA1673-RC-OK            VALUES 'OKM'
                                                   , 'OKO'.
              88 PO-DRPA1673-RC-OKM           VALUE  'OKM'.
              88 PO-DRPA1673-RC-OKO           VALUE  'OKO'.
              88 PO-DRPA1673-RC-NFD           VALUE  'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
