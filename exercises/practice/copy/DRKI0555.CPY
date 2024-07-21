      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.02.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0555.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI0555.CPv  $
      *
      *   Rev 2.2   Feb 18 2004 16:08:10   t13ex39
      *AKI00391 Fonds aktualisieren
      *
      *   Rev 2.0   Feb 12 2004 08:37:28   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0555                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0555                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  18.02.2004/11:20:08
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0555
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0555.
           05  PI-DRKI0555-KEY.


             10 PI-DRKI0555-FONDID   PIC S9(4) COMP.

      *
       01  PO-DRKI0555.
           05  PO-DRKI0555-RC                 PIC  X(003).
               88 PO-DRKI0555-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0555-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0555-DATEN.


             10 PO-DRKI0555-FONDART   PIC X(20).
             10 PO-DRKI0555-FONDBEZEICHNUNG   PIC X(40).
             10 PO-DRKI0555-FONDDVAGPRODSCHL   PIC S9(4) COMP.
             10 PO-DRKI0555-FONDGESNR   PIC S9(4) COMP.
             10 PO-DRKI0555-FONDISIN   PIC X(12).
             10 PO-DRKI0555-FONDWERTPAPKNR   PIC X(6).
             10 PO-DRKI0555-FONDWERTPAPKZ   PIC X(01).


      *
      ******* ENDE COPY-MEMBER **************************************
