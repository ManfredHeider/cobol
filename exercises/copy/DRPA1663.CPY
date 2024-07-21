      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.06.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1663.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRPA1663.CPv  $
      *
      *   Rev 2.9   Jun 03 2019 09:53:46   t13ae46
      *FPA01690: Ges. 42 Prod. 22 
      *
      *   Rev 2.8   May 17 2019 09:36:32   t13ae46
      *FPA01690: Ges. 42 Prod. 22
      *
      *   Rev 2.7   Mar 28 2019 09:44:06   t13ae46
      *FPA01690: Ges. 42 Prod. 22
      *
      *   Rev 2.6   Mar 27 2019 12:44:30   t13ae46
      *FPA01690: Ges. 42 Prod. 22
      *
      *   Rev 2.5   Jun 19 2012 09:31:34   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.4   May 21 2012 08:56:30   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   19 Dec 2007 11:44:02   t13ex25
      *FPA00968: Anpassung UWPA0031 (Wechsel VGES)
      *
      *   Rev 2.2   31 Oct 2007 17:01:20   t13ae30
      *AVB00905: PGM - Anpassung UWPA0012 TWI
      *
      *   Rev 2.1   Oct 31 2007 12:36:36   t13ex21
      *APA00622: Allfinanz Stufe 1 im abgerechneten Geschäft
      *
      *   Rev 2.0   Jun 11 2007 15:18:36   t13ex21
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1663  COVERKONTEN ERMITTELN              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    11.06.2007                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00622!11.06.07!2.01! NEUERSTELLUNG             ! STORCH   *
      * FPA00968!19.12.07!2.02! ERGÄNZUNG VGES DER MELDUNG! JÄGER    *
      * APA01101!29.02.12!2.03! VGES DURCH MANDANT ERSETZT! STORCH   *
      * FPA01690!08.03.19!2.04! ERGÄNZUNG RC-UNZ          ! WANG     *
      *         !        !2.05! ÄNDERUNGEN 2.04 ENTFERNEN ! WANG     *
      *         !        !2.06! 2.04 AKTIVIEREN           ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1663                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM WORKFLOW                                      *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1663.
          05 PI-DRPA1663-PGEB-GES             PIC S9(04) BINARY.
          05 PI-DRPA1663-VB-NR                PIC S9(09) COMP.
          05 PI-DRPA1663-KNR-MANDANT          PIC S9(04) COMP.

       01 PO-DRPA1663.
          05 PO-DRPA1663-RC                   PIC  X(03).
             88 PO-DRPA1663-RC-OK                  VALUE 'OK '.
             88 PO-DRPA1663-RC-NOK                 VALUE 'NOK'.
080319       88 PO-DRPA1663-RC-UNZ                 VALUE 'UNZ'.
          05 PO-DRPA1663-DATEN.
             10 PO-DRPA1663-MANDANT-VWF       PIC S9(09) BINARY.
             10 PO-DRPA1663-MANDANT-VWF-PRZ   PIC  X(01).
             10 PO-DRPA1663-MANDANT-VTF       PIC S9(09) BINARY.
             10 PO-DRPA1663-MANDANT-VTF-PRZ   PIC  X(01).
      *
      ******** ENDE COPY-MEMBER **************************************




