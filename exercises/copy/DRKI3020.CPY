      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3020.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3020.CPv  $
      *
      *   Rev 2.2   Jul 09 2015 10:15:34   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.1   Jul 09 2015 09:50:14   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: OZPs
      *
      *   Rev 2.0   Jul 1 2015 10:46:02 T13EX48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3020                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3020                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         06.07.2015 / 11:40:15 / T13EX48
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
      *    FÜR DIE FUNKTION DRKI3020
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3020.
           05  PI-DRKI3020-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3020-LGRRNTART   PIC S9(4) COMP.
             10 PI-DRKI3020-LGRRNTBEGINN   PIC  X(010).
             10 PI-DRKI3020-LGRVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI3020.
           05  PO-DRKI3020-RC                     PIC  X(003).
               88 PO-DRKI3020-RC-OK               VALUE 'OK '.
               88 PO-DRKI3020-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI3020-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI3020-LETZTEAENDERUNG   PIC  X(0026).
             10 PO-DRKI3020-LGRGSMTHOEHE   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI3020-LGRRNTABLAUF   PIC  X(010).
             10 PO-DRKI3020-LGRRNTZAHLW-KNR   PIC S9(4) COMP.
             10 PO-DRKI3020-LGRRNTZAHLW-KTY   PIC S9(4) COMP.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************


