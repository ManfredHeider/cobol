      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.05.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1750.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1750.CPv  $
      *
      *   Rev 2.2   Aug 09 2013 15:13:58   t13ae37
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): SST-Abrechnung
      *
      *   Rev 2.1   May 08 2013 11:47:14   t13ae37
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): SST-Abrechnung
      *
      *   Rev 2.0   May 06 2013 13:26:24   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1750  SELECT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BUCH                                            *
      *    DATUM:    05.06.2013                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.07.98! 01 ! NEUERSTELLUNG             ! JÄGER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1750:
      *    LESE KLARTEXT ZU BESTIMMTEN BEWEGUNGSSCHLÜSSEL
      ****************************************************************
       01 PI-DRPA1750.
          05 PI-DRPA1750-BEW-SCHL           PIC  X(006).
          05 PI-DRPA1750-KNR-TEXT-TYP       PIC  9(004).
          05 PI-DRPA1750-KNR-DYN-KZ-2       PIC  9(004).
      *
       01 PO-DRPA1750.
          05 PO-DRPA1750-RC                 PIC  X(003).
             88 PO-DRPA1750-RC-OK           VALUE 'OK '.
             88 PO-DRPA1750-RC-NFD          VALUE 'NFD'.
          05 PO-DRPA1750-TEXT-1             PIC  X(042).
          05 PO-DRPA1750-TEXT-2             PIC  X(025).
      *
      ***** ENDE-COPY-MEMBER *****************************************

