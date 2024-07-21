      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.05.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1575.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1575.CPv  $
      *
      *   Rev 2.1   Oct 13 2003 11:16:38   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.0   May 19 2003 10:00:22   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1575                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1575                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  21.08.2003/11:37:32
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
      *    FÜR DIE FUNKTION DRKI1575
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1575.
           05  PI-DRKI1575-KEY.


             10 PI-DRKI1575-PERSABADRESSEID   PIC X(13).
             10 PI-DRKI1575-PERSABPERSONENID   PIC X(13).

      *
       01  PO-DRKI1575.
           05  PO-DRKI1575-RC                 PIC  X(003).
               88 PO-DRKI1575-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1575-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1575-DATEN.


             10 PO-DRKI1575-PERSABVERTRADRKZ   PIC X(1).


      *
      ******* ENDE COPY-MEMBER **************************************
