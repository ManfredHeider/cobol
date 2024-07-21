      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.05.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2905.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2905.CPv  $
      *
      *   Rev 2.3   May 27 2014 13:06:02   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF - ZTK
      *I0467 SWZ
      *
      *   Rev 2.2   May 19 2014 15:27:34   t13ex78
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF - ZTK
      *I0467
      *
      *   Rev 2.1   May 16 2014 11:26:16   t13ex78
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF - ZTK
      *I0467
      *
      *   Rev 2.0   May 14 2014 07:25:09 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2905                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2905                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.05.2014 / 12:09:10 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2905
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2905.
           05  PI-DRKI2905-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2905-OIFDATRUECKZAHLUNG   PIC  X(010).
             10 PI-DRKI2905-OIFFONDID   PIC S9(4) COMP.
             10 PI-DRKI2905-OIFGHALFDNR   PIC S9(4) COMP.
             10 PI-DRKI2905-OIFGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2905-OIFVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2905.
           05  PO-DRKI2905-RC                     PIC  X(003).
               88 PO-DRKI2905-RC-OK               VALUE 'OK '.
               88 PO-DRKI2905-RC-NOT-FOUND        VALUE 'NFD'.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
