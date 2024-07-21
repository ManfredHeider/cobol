      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.07.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2639.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2639.CPv  $
      *
      *   Rev 2.1   Aug 14 2012 14:59:20   t13ex14
      *AKI01154: automatische Vergabe neue KI-Produktnummer
      *
      *   Rev 2.0   Jul 18 2012 14:00:47 T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2639                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2639                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         18.07.2012 / 14:00:47 / T13EX14
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
      *    FÜR DIE FUNKTION DRKI2639
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  MIN / MAX <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2639.
           05  PI-DRKI2639-KEY.
ANF   *        %M_PI_KEY%
             10 PI-DRKI2639-PRODUKTID   PIC S9(4) COMP.
END   *
      *
       01  PO-DRKI2639.
           05  PO-DRKI2639-RC                     PIC  X(003).
               88 PO-DRKI2639-RC-OK               VALUE 'OK '.
               88 PO-DRKI2639-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2639-DATEN.
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2639-PRODUKTID   PIC S9(4) COMP.
END   *
      *
      ******* ENDE COPY-MEMBER **************************************
