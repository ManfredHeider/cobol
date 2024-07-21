      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.02.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1639.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1639.CPv  $
      *
      *   Rev 2.1   Feb 18 2004 16:09:22   t13ex39
      *AKI00391 Fonds aktualisieren
      *
      *   Rev 2.0   Feb 12 2004 08:37:28   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1639                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1639                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  18.02.2004/12:54:47
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
      *    FÜR DIE FUNKTION DRKI1639
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1639.
           05  PI-DRKI1639-KEY.


             10 PI-DRKI1639-FONDID   PIC S9(4) COMP.


           05  PI-DRKI1639-DATEN.


             10 PI-DRKI1639-FONDART   PIC X(20).
             10 PI-DRKI1639-FONDBEZEICHNUNG   PIC X(40).
             10 PI-DRKI1639-FONDDVAGPRODSCHL   PIC S9(4) COMP.
             10 PI-DRKI1639-FONDGESNR   PIC S9(4) COMP.
             10 PI-DRKI1639-FONDISIN   PIC X(12).
             10 PI-DRKI1639-FONDWERTPAPKNR   PIC X(6).
             10 PI-DRKI1639-FONDWERTPAPKZ   PIC X(01).


      *
       01  PO-DRKI1639.
           05  PO-DRKI1639-RC                 PIC  X(003).
               88 PO-DRKI1639-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1639-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
