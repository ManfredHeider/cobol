      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2342.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2342.CPv  $
      *
      *   Rev 2.1   Oct 26 2009 07:15:08   t13ex14
      *AKI00846: Erw. der Bestands-DTA Badenia: OZP-Objekte
      *
      *   Rev 2.0   Aug 14 2009 11:29:06   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2342                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2342                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  02.09.2009/13:44:40
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
      *    FÜR DIE FUNKTION DRKI2342
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2342.
           05  PI-DRKI2342-KEY.


             10 PI-DRKI2342-BJRGUTHPRODLFDNR   PIC S9(4) USAGE COMP.
             10 PI-DRKI2342-BJRJAHRZULAGE   PIC S9(4) USAGE COMP.
             10 PI-DRKI2342-BJRVERTRAGSID   PIC X(13).
             

           05  PI-DRKI2342-DATEN.


             10 PI-DRKI2342-BJRDATZULAGGUT   PIC X(10).
             10 PI-DRKI2342-BJRJHRZULAGGUT   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2342-NI-BJRJHRZULAGGUT   PIC S9(4) COMP.
             

      *
       01  PO-DRKI2342.
           05  PO-DRKI2342-RC                 PIC  X(003).
               88 PO-DRKI2342-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2342-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
