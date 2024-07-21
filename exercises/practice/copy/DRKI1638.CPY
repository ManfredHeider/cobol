      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.02.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1638.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1638.CPv  $
      *
      *   Rev 2.1   Feb 18 2004 16:09:10   t13ex39
      *AKI00391 Fonds aktualisieren
      *
      *   Rev 2.0   Feb 12 2004 08:31:16   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1638                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1638                    *
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
      *    FÜR DIE FUNKTION DRKI1638
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1638.
           05  PI-DRKI1638-KEY.


             10 PI-DRKI1638-FONDID   PIC S9(4) COMP.


           05  PI-DRKI1638-DATEN.


             10 PI-DRKI1638-FONDART   PIC X(20).
             10 PI-DRKI1638-FONDBEZEICHNUNG   PIC X(40).
             10 PI-DRKI1638-FONDDVAGPRODSCHL   PIC S9(4) COMP.
             10 PI-DRKI1638-FONDGESNR   PIC S9(4) COMP.
             10 PI-DRKI1638-FONDISIN   PIC X(12).
             10 PI-DRKI1638-FONDWERTPAPKNR   PIC X(6).
             10 PI-DRKI1638-FONDWERTPAPKZ   PIC X(01).

      *
       01  PO-DRKI1638.
           05  PO-DRKI1638-RC                 PIC  X(003).
               88 PO-DRKI1638-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1638-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
