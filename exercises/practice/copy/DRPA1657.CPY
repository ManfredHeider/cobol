      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.05.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1657.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1657.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:31:26   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:56:22   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Jan 18 2008 10:22:04   t13ex21
      *APA00622: Allfinanz Stufe 1 im abgerechneten Geschäft
      *
      *   Rev 2.1   May 23 2006 07:36:52   t13ex44
      *FPA00846: SA815 erweitern (ppfl. Summe und Sonder-KZ)
      *
      *   Rev 2.0   May 23 2006 07:01:34   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1657 ERMITTELN PMBE-HAFTZEIT FÜR PMPT-ID  *
      *    -----------------------------------------------------------*
      *    ERSTELLT: SCHEIDEL                                         *
      *    DATUM:    16.05.2006                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00846!16.05.06!2.0 ! NEU                       ! SCHEIDEL *
      * APA00622!17.01.08!2.1 ! ERWEITERUNG PO-BEREICH    ! STORCH   *
      * APA01101!12.03.12!2.2 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1657                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1657.
          05 PI-DRPA1657-DATEN.
             10 PI-DRPA1657-PGEB-GES                PIC S9(004) COMP.
             10 PI-DRPA1657-PMPT-ID                 PIC S9(015) COMP-3.
             10 PI-DRPA1657-PMPT-VERSION            PIC S9(004) COMP.
      *
       01 PO-DRPA1657.
          05 PO-DRPA1657-RC                         PIC  X(003).
             88 PO-DRPA1657-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1657-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA1657-DATEN.
             10 PO-DRPA1657-HAFTZEIT-KZ             PIC  X(002).
             10 PO-DRPA1657-PMBE-KNR-MANDANT        PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
