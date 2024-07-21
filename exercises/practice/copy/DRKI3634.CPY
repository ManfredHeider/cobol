      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3634.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3634.CPv  $
      *
      *   Rev 2.2   Jun 07 2019 08:19:00   t13ex78
      *AKI01669: OZP G67_KDETICKETS
      *
      *   Rev 2.1   Mar 19 2019 12:42:20   t13ex78
      *AKI01669: OZP G67_KDETICKETS
      *
      *   Rev 2.0   Mar 18 2019 11:12:15 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3634                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3634                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         18.03.2019 / 11:12:15 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3634
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3634.
           05  PI-DRKI3634-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3634-G67TLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3634-G67TVERTRAGSNR   PIC  X(0015).
END   *
 
      *
       01  PO-DRKI3634.
           05  PO-DRKI3634-RC                     PIC  X(003).
               88 PO-DRKI3634-RC-OK               VALUE 'OK '.
               88 PO-DRKI3634-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI3634-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3634-G67TANREDE   PIC  X(0001).
             10 PO-DRKI3634-G67TGEBDAT   PIC  X(0010).
             10 PO-DRKI3634-G67TLAND   PIC  X(0003).
             10 PO-DRKI3634-G67TNACHNAME   PIC  X(0030).
             10 PO-DRKI3634-G67TORT   PIC  X(0030).
             10 PO-DRKI3634-G67TPLZ   PIC  X(0009).
             10 PO-DRKI3634-G67TSTRHNR   PIC  X(0040).
             10 PO-DRKI3634-G67TTICKETID   PIC S9(18)V COMP-3.
             10 PO-DRKI3634-G67TVORNAME   PIC  X(0030).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************


