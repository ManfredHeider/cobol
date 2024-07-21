      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2069.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2069.CPv  $
      *
      *   Rev 2.2   03 Dec 2007 11:28:38   t13ex39
      *AKL00664: U P K I 0 2 0 9   
      *
      *   Rev 2.1   04 Oct 2007 13:35:34   t13ex39
      *AKL00664: U P K I 0 1 6 6
      *
      *   Rev 2.0   28 Aug 2007 14:37:16   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2069                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2069                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  28.08.2007/15:16:42
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
      *    FÜR DIE FUNKTION DRKI2069
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2069.
           05  PI-DRKI2069-KEY.
               10 PI-DRKI2069-MITARBEITERNR    PIC S9(9) COMP.
      *
       01  PO-DRKI2069.
           05  PO-DRKI2069-RC                 PIC  X(003).
               88 PO-DRKI2069-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2069-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2069-DATEN.
             10 PO-DRKI2069-MITARBMANDANTNR   PIC S9(9) COMP.
             10 PO-DRKI2069-MITARBPERSONENID   PIC X(13).
             10 PO-DRKI2069-MITARBPERSROLLE    PIC X(01).

      *
      ******* ENDE COPY-MEMBER **************************************
