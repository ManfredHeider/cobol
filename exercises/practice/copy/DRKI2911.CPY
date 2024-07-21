      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.06.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2911.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2911.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:44:10   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:34:16   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Standard-OZP für TCKI0469 G10KS
      *DSSUHSA401
      *
      *   Rev 2.0   Jun 4 2014 08:14:26 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2911                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2911                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.06.2014 / 08:14:26 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2911
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2911.
           05  PI-DRKI2911-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2911-G10EZVHOSTNR   PIC  X(0015).
             10 PI-DRKI2911-G10EZVVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI2911.
           05  PO-DRKI2911-RC                     PIC  X(003).
               88 PO-DRKI2911-RC-OK               VALUE 'OK '.
               88 PO-DRKI2911-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
