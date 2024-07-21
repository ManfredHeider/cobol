      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0190.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0190.CPv  $
      *
      *   Rev 2.4   Sep 17 2009 10:39:46   t13ae34
      *AAN00917:  Anpassung des Antragsmoduls
      *
      *   Rev 2.3   Apr 23 2009 10:16:28   t13ae34
      *AAN00882: Antrag
      *
      *   Rev 2.2   Jul 24 2007 12:16:42   t13ae34
      *AAN00764: Funktion Setzen Antragstatus
      *
      *   Rev 2.1   Jul 24 2007 09:23:38   t13ae34
      *AAN00764: Funktion Setzen Antragstatus
      *
      *   Rev 2.0   Jul 18 2007 14:59:24   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0190                                     *
      *    FUNKTION   : SETZEN ANTRAGSTATUS IM ANTRAGSBESTAND        *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00764!23.07.07!2.1 !NEU                        !HEIDER    *
      * AAN00882!21.04.09!2.2 !SCHREIBEN DATEI AN008900   !HEIDER    *
      * AAN00917!11.09.09!2.2 ! UNTERFUNKTIONSCODES       !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0190                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0190.
110909*>>>>>
           05 PI-DRAN0190-UFKT                   PIC 9(01).
           88 PI-DRAN0190-UFKT-OPEN              VALUE 1.
           88 PI-DRAN0190-UFKT-WRITE             VALUE 2.
           88 PI-DRAN0190-UFKT-CLOSE             VALUE 3.
110909*<<<<<
          05 PI-DRAN0190-KEY.
             10 PI-DRAN0190-PGEB-GES             PIC S9(004) COMP.
             10 PI-DRAN0190-ANTA-NR              PIC S9(15)V COMP-3.

          05 PI-DRAN0190-DATEN.
             10 PI-DRAN0190-ANTA-ANTR-STATUS-1   PIC S9(004) COMP.
      *
       01 PO-DRAN0190.
          05 PO-DRAN0190-RC                      PIC  X(003).
             88 PO-DRAN0190-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0190-RC-NOT-FOUND              VALUE 'NFD'.
             88 PO-DRAN0190-RC-NO-UPDATE              VALUE 'NUP'.
          05 PO-DRAN0190-DATEN.
             10 FILLER                           PIC  X(001).
      ******** ENDE COPY-MEMBER **************************************
