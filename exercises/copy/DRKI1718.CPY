      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1718.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1718.CPv  $
      *
      *   Rev 2.1   Nov 03 2004 15:02:00   t13ex39
      *AKI00249: Änderungen/Ergänzungen zum Datenspiegel der Deutsc
      *hen Bank
      *
      *   Rev 2.0   Sep 22 2004 15:43:16   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *  UPDATE VERDATPRODLOESCH
      *****************************************************************
      *    COPY-MEMBER: DRKI1718                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1718                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  22.09.2004/14:13:57
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
      *    FÜR DIE FUNKTION DRKI1718
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1718.
           05  PI-DRKI1718-KEY.


             10 PI-DRKI1718-VERVERTRAGSID   PIC X(13).


           05  PI-DRKI1718-DATEN.


             10 PI-DRKI1718-VERDATPRODLOESCH   PIC X(10).


      *
       01  PO-DRKI1718.
           05  PO-DRKI1718-RC                 PIC  X(003).
               88 PO-DRKI1718-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1718-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
