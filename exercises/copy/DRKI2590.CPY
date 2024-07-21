      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.04.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2590.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2590.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 16:16:28   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-Adressen
      *
      *   Rev 2.0   Apr 11 2012 11:10:36 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2590                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2590                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.04.2012 / 11:10:36 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2590
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2590.
           05  PI-DRKI2590-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2590-LPEPERSROLLEKZ   PIC  X(0001).
             10 PI-DRKI2590-LPEVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2590-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2590-LPEADRESSEID   PIC  X(0013).
END   *


      *
       01  PO-DRKI2590.
           05  PO-DRKI2590-RC                     PIC  X(003).
               88 PO-DRKI2590-RC-OK               VALUE 'OK '.
               88 PO-DRKI2590-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
