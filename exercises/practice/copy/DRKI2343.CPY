      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2343.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2343.CPv  $
      *
      *   Rev 2.1   Oct 26 2009 07:15:14   t13ex14
      *AKI00846: Erw. der Bestands-DTA Badenia: OZP-Objekte
      *
      *   Rev 2.0   Aug 14 2009 11:29:14   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2343                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2343                    *
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
      *    FÜR DIE FUNKTION DRKI2343
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2343.
           05  PI-DRKI2343-KEY.


             10 PI-DRKI2343-BJRGUTHPRODLFDNR   PIC S9(4) USAGE COMP.
             10 PI-DRKI2343-BJRJAHRZULAGE   PIC S9(4) USAGE COMP.
             10 PI-DRKI2343-BJRVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI2343.
           05  PO-DRKI2343-RC                 PIC  X(003).
               88 PO-DRKI2343-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2343-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
