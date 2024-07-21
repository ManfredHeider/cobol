      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.10.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1798.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1798.CPv  $
      *
      *   Rev 2.2   Feb 28 2017 09:55:48   t13ae37
      *APA02275: DAISY-Erweiterungen zu FlexN - PVW 2
      *
      *   Rev 2.1   Dec 08 2014 09:24:16   t13ex67
      *APA01809: Projekt Integration FVD    //       Migrationsprog
      *ramm
      *
      *   Rev 2.0   Oct 27 2014 13:11:02   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1798  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHAEFER                                 *
      *    DATUM:    27.10.2014                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01809!27.10.14!2.01! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1798                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PAMS TBPA0129              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1798.
          05 PI-DRPA1798-PDPT-KEY.
             10 PI-DRPA1798-PAMS-DATEN.
             COPY DRPA0989.
 
 
       01 PO-DRPA1798.
          05 PO-DRPA1798-RC                      PIC  X(003).
             88 PO-DRPA1798-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1798-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1798-RC-NOT-FOUND                  VALUE 'NFD'.
 
 
 

